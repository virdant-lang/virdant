"""Sphinx extension: the ``vcd`` directive.

Renders a VCD trace as a WaveDrom-style SVG inline figure.

    .. vcd::
        :caption: Hello World
        :file: path/to/trace.vcd
        :signals: top.clock,top.toggle.switch

Runs ``docs/vcd2wavedrom.py`` on the given VCD file to produce WaveJSON,
then renders that JSON as an inline SVG (embedded directly in the page so
that CSS variables can theme it for light and dark mode).  If ``:caption:``
is given, the block gets a caption styled like a code-block caption.
"""
import json
import os
import subprocess
import sys
import tempfile
from html import escape

from docutils import nodes
from docutils.parsers.rst import Directive, directives
from sphinx.util import logging

logger = logging.getLogger(__name__)

# ---------------------------------------------------------------------------
# Rendering geometry (all values in SVG user units; effectively pixels since
# the SVG has a 1:1 viewBox).  Everything the waveform renderer draws is
# derived from these constants, so changing one of them scales the whole
# diagram consistently.
# ---------------------------------------------------------------------------

# Never let a tick get smaller than this, no matter how long the trace is.
# This is the "zoomed all the way out" end of the GTKWave-style fit: a trace
# longer than (target waveform width) ticks will exceed the target width and
# fall back to horizontal scrolling (overflow-x on .vcd-body) rather than
# collapsing into an unreadable smear.
MIN_TICK_W = 1.0

# Vertical height reserved for one signal row.  Rows are stacked directly on
# top of each other with no extra spacing, so this is also the row pitch.
# Must be large enough to contain both Y_TOP and Y_BOT plus a little
# breathing room, otherwise neighbouring rows' lines will collide.
ROW_H = 26

# Within a row of height ROW_H, the vertical position of each signal level,
# measured from the top of that row:
#
#       row top
#   +--------------------------------+
#   |   (empty padding: 0..Y_TOP)    |
#   | Y_TOP ---- 1 level ------------ |
#   |                                 |
#   | Y_MID ---- z level ----------- |   z renders as a dimmed mid line
#   |                                 |
#   | Y_BOT ---- 0 level ------------ |
#   |   (empty padding: Y_BOT..ROW_H) |
#   +--------------------------------+
#
# x-values and data boxes span the full Y_TOP..Y_BOT range instead of
# sitting on a level (see the box drawing below).  Y_MID is kept near the
# midpoint but slightly below true centre so that a 'z' line does not sit
# exactly on top of the box borders it may abut.
Y_TOP, Y_BOT, Y_MID = 8, 22, 15

# Map from a WaveJSON wave character to its level (see diagram above).
# Characters not in this map ('x', '4', '.') are drawn as boxes or repeats
# rather than level lines.
LEVELS = {'0': Y_BOT, '1': Y_TOP, 'z': Y_MID}

# Approximate advance width of one character of the 10px monospace font used
# inside the SVG.  Used only for measuring, never drawing:
#   * sizing the label gutter (widest signal name must fit),
#   * deciding whether a data value (e.g. "0x2") fits inside its box, or
#   whether the box is too narrow and the label should be omitted.
CHAR_W = 6.6


def expand_wave(wave, data):
    """Expand a WaveJSON wave string into [(char, data_value), ...].

    One token per tick.  WaveDrom wave strings use:
      '0'/'1'      logic levels
      'x'          unknown (drawn as a crossed box)
      'z'          high impedance (drawn as a dimmed mid-level line)
      '4'          multi-bit value; its display text is the next entry of
                   the `data` list (hex, as emitted by vcd2wavedrom.py)
      '.'          repeat of the previous value -- no new data entry is
                   consumed, the previous value is carried forward so that
                   runs of equal values stay grouped in runs_of()
    """
    tokens = []
    prev = None        # last value char seen ('4' stays '4' through '.')
    prev_value = None  # data value carried by the last '4' token
    di = 0             # cursor into the `data` list
    for c in wave:
        if c == '.':
            if prev is None:
                continue
            tokens.append((prev, prev_value))
        else:
            value = None
            if c == '4':
                value = data[di]
                di += 1
                prev_value = value
            tokens.append((c, value))
            prev = c
    return tokens


def runs_of(tokens):
    """Group tokens into runs of (start, end, char, data).

    A run is a maximal stretch of consecutive ticks with the same value
    character AND the same data value (so consecutive multi-bit values with
    different hex text still get one box each).  start is inclusive, end is
    exclusive: x-coordinates of a run are start*tick_w .. end*tick_w.
    """
    runs = []
    i = 0
    while i < len(tokens):
        j = i
        c, v = tokens[i]
        while j < len(tokens) and tokens[j][0] == c and tokens[j][1] == v:
            j += 1
        runs.append((i, j, c, v))
        i = j
    return runs


def render_svg(wavejson, target_width):
    """Render WaveJSON (as produced by vcd2wavedrom.py) into an SVG string.

    `target_width` is the width the SVG should occupy, in CSS pixels.  The
    "zoom" is applied at the level of geometry -- pixels per tick -- NOT by
    CSS-scaling the finished SVG, so font sizes stay fixed and crisp no
    matter how the trace is stretched or compressed.
    """
    signals = wavejson['signal']

    # Longest wave string determines the total number of ticks, hence the
    # width of the waveform area.
    n_ticks = max((len(expand_wave(s['wave'], s.get('data', [])))
                   for s in signals), default=0)

    # Width of the left-hand gutter that holds signal names.  Sized to fit
    # the longest name (CHAR_W per character) plus 24px of padding so the
    # longest name does not visually touch the waveform area; floored at 40
    # so short names still get a reasonable gutter.
    label_w = max(40, int(max((len(s['name']) for s in signals), default=0) * CHAR_W) + 24)

    # Vertical space above the first signal row: 18px when there is a head
    # line ("trace.vcd  (1 tick = ...)" rendered top-right), 4px of minimal
    # padding when there is not.
    head_h = 18 if wavejson.get('head', {}).get('text') else 4

    # Right-hand padding between the last tick and the SVG border.  Also
    # anchors the right-aligned head text.
    pad_r = 12

    # "Zoom to fit": choose the pixels-per-tick so that the waveform area
    # (n_ticks ticks) plus gutter and right padding lands exactly on the
    # target width.  This is the GTKWave zoom analogue: a short trace is
    # stretched (zoomed in, each tick wide), a long trace compressed (zoomed
    # out, each tick narrow) -- while fonts stay at their fixed pixel sizes.
    # The MIN_TICK_W floor ("zoomed all the way out") is the one case where
    # the SVG may be wider than target_width; it then scrolls horizontally.
    tick_w = max(MIN_TICK_W, (target_width - label_w - pad_r) / n_ticks) if n_ticks else 0

    # Total SVG canvas size: gutter + waveform area + right padding across,
    # head line + all signal rows + a little bottom padding down.
    width = label_w + n_ticks * tick_w + pad_r
    height = head_h + len(signals) * ROW_H + 8

    parts = []
    parts.append(
        f'<svg class="vcd-svg" viewBox="0 0 {width} {height}" width="{width}" '
        f'height="{height}" xmlns="http://www.w3.org/2000/svg" role="img">')

    if wavejson.get('head', {}).get('text'):
        head = wavejson['head']['text']
        parts.append(
            f'<text class="vcd-head" x="{width - pad_r}" y="12" '
            f'text-anchor="end">{escape(head)}</text>')

    # Waveform drawing area is offset by the label gutter.
    parts.append(f'<g transform="translate({label_w},0)">')

    for idx, sig in enumerate(signals):
        # Top edge of this row on the canvas, and the three level y-positions
        # within the row (see the geometry diagram above the constants).
        row_y = head_h + idx * ROW_H
        y_top, y_bot, y_mid = row_y + Y_TOP, row_y + Y_BOT, row_y + Y_MID

        tokens = expand_wave(sig['wave'], sig.get('data', []))
        runs = runs_of(tokens)

        # Horizontal level segments and vertical connectors.
        # h_path collects the 0/1 level lines plus the vertical edges at
        # level-to-level transitions; z_path collects the dimmed 'z' lines so
        # they can be styled (faded) separately via .vcd-zline.
        h_path = []
        z_path = []
        # One horizontal segment per run of consecutive ticks sharing a value:
        # drawn at that value's level, spanning tick i through tick j.
        for (i, j, c, _v) in runs:
            x0, x1 = i * tick_w, j * tick_w
            if c in LEVELS:
                y = row_y + LEVELS[c]
                (z_path if c == 'z' else h_path).append(f'M {x0} {y} L {x1} {y}')
        # Vertical edges: at every tick boundary where two *levelled* values
        # differ, connect the old level to the new one.  Edges into/out of
        # 'x' or data boxes are skipped -- the box's own border provides the
        # vertical transition there.
        for k in range(len(tokens) - 1):
            c1, c2 = tokens[k][0], tokens[k + 1][0]
            if c1 != c2 and c1 in LEVELS and c2 in LEVELS:
                x = (k + 1) * tick_w
                h_path.append(f'M {x} {row_y + LEVELS[c1]} L {x} {row_y + LEVELS[c2]}')

        if h_path:
            parts.append(f'<path class="vcd-line" d="{" ".join(h_path)}"/>')
        if z_path:
            parts.append(f'<path class="vcd-zline" d="{" ".join(z_path)}"/>')

        # Boxes: drawn per run, spanning the full Y_TOP..Y_BOT range.
        # 'x'  -> rectangle with a diagonal cross (unknown/bus fighting)
        # '4'  -> rectangle carrying the hex data value as centred text
        for (i, j, c, v) in runs:
            x0, x1 = i * tick_w, j * tick_w
            if c == 'x':
                parts.append(
                    f'<rect class="vcd-xbox" x="{x0}" y="{y_top}" '
                    f'width="{x1 - x0}" height="{y_bot - y_top}"/>')
                parts.append(
                    f'<path class="vcd-cross" d="M {x0} {y_top} L {x1} {y_bot} '
                    f'M {x0} {y_bot} L {x1} {y_top}"/>')
            elif c == '4':
                parts.append(
                    f'<rect class="vcd-dbox" x="{x0}" y="{y_top}" '
                    f'width="{x1 - x0}" height="{y_bot - y_top}"/>')
                label = v if v is not None else ''
                # Only render the value text if it fits inside the box with
                # 2px of slack; a too-narrow box just stays empty.
                if len(label) * CHAR_W <= x1 - x0 - 2:
                    parts.append(
                        f'<text class="vcd-data" x="{(x0 + x1) / 2}" '
                        f'y="{row_y + Y_MID + 3.5}" text-anchor="middle">'
                        f'{escape(label)}</text>')

    parts.append('</g>')

    # Signal name labels sit in the gutter, outside the translated group.
    for idx, sig in enumerate(signals):
        row_y = head_h + idx * ROW_H
        parts.append(
            f'<text class="vcd-name" x="{label_w - 14}" y="{row_y + Y_MID + 4}" '
            f'text-anchor="end">{escape(sig["name"])}</text>')

    parts.append('</svg>')
    return ''.join(parts)


class VcdDirective(Directive):
    has_content = False
    required_arguments = 0
    optional_arguments = 0
    option_spec = {
        'caption': directives.unchanged,
        'file': directives.path,
        'signals': directives.unchanged,
    }

    def run(self):
        env = self.state.document.settings.env
        filename = self.options.get('file')
        if not filename:
            raise self.error('vcd directive requires a :file: option')

        # Resolve :file: relative to the document, then to the source root.
        docdir = os.path.dirname(env.doc2path(env.docname))
        for base in (docdir, env.srcdir):
            path = os.path.normpath(os.path.join(base, filename))
            if os.path.exists(path):
                break
        else:
            raise self.error(f'vcd file not found: {filename}')

        # Track the VCD as a build dependency of this document, so that
        # editing the trace alone (without touching the .rst) triggers a
        # rebuild of this document -- otherwise a changed trace leaves a
        # stale SVG and stale warnings behind.
        env.note_dependency(path)

        script = os.path.normpath(os.path.join(
            os.path.abspath(os.path.dirname(__file__)), '..', 'vcd2wavedrom.py'))

        signals = self.options.get('signals')
        fd, out_path = tempfile.mkstemp(suffix='.json')
        os.close(fd)
        try:
            cmd = [sys.executable, script, path, out_path]
            if signals:
                cmd += ['-s', signals]
            result = subprocess.run(cmd, capture_output=True, text=True)
            if result.returncode != 0:
                raise self.error(
                    f'vcd2wavedrom failed for {filename}:\n'
                    f'{result.stderr.strip() or result.stdout.strip()}')
            # vcd2wavedrom warns on stderr about signals that matched nothing
            # (exit code is still 0, so it would otherwise be invisible and
            # the rendering would silently be missing the requested signal).
            if result.stderr.strip():
                logger.warning(
                    f'vcd2wavedrom warning for {filename}: {result.stderr.strip()}',
                    location=(env.docname, self.lineno))
            with open(out_path) as f:
                wavejson = json.load(f)
            # Show the :file: value (as written in the directive) instead of the
            # absolute build-host path vcd2wavedrom.py puts in the head text.
            head = wavejson.get('head', {})
            if head.get('text') and '  (' in head['text']:
                head['text'] = f"{filename}  {head['text'].split('  ', 1)[1]}"
        finally:
            os.unlink(out_path)

        html = f'<div class="vcd-body">{render_svg(wavejson, env.config.vcd_waveform_width)}</div>'
        if 'caption' in self.options:
            caption = escape(self.options['caption'])
            html = (f'<div class="vcd-block literal-block-wrapper docutils container">'
                    f'<div class="code-block-caption">'
                    f'<span class="caption-text">{caption}</span></div>{html}</div>')
        else:
            html = f'<div class="vcd-block docutils container">{html}</div>'

        return [nodes.raw('', html, format='html')]


def setup(app):
    app.add_directive('vcd', VcdDirective)
    # Target width (px) that vcd SVGs "zoom to fit".  This is the content
    # width available *inside* .vcd-body: the content column is 800px, the
    # block measures 694px on the border box, minus 1px borders and 6px
    # horizontal padding on each side -> 680px for the SVG itself.  (If the
    # .vcd-body border/padding in custom.css changes, update this.)
    app.add_config_value('vcd_waveform_width', 680, 'env')
    return {
        'version': '0.1',
        'parallel_read_safe': True,
        'parallel_write_safe': True,
    }
