"""Sphinx extension: the ``vcd`` directive.

Renders a VCD trace as a WaveDrom-style SVG inline figure.

    .. vcd:: path/to/trace.vcd
       :caption: Hello World

       signal top.clock
       signal top.count:dec

The directive body is a line-based layout, one item per nonblank line
(empty lines have no effect):

  ``signal <name>[:<format>]``
      A signal to draw.  A signal name, optionally followed by a value
      format after a colon.  Formats are those of ``docs/vcd2wavedrom.py``
      (``bin``, ``hex``, ``dec``; add more in its FORMATTERS table); the
      default is ``hex``.  The format only affects multi-bit values, which
      are rendered as text in a data box; 1-bit signals draw as 0/1 level
      lines either way.

      A format that is not one of those keys is instead taken as the
      signal's *Virdant type*, e.g. ``signal top.load_value:Valid[Word[8]]``
      or ``signal top.cmd:shiftreg::Command[Word[2]]``.  Each distinct bit
      pattern is decoded by the ``vir-value`` tool and rendered as its
      round-trippable value text (``@Valid(179w8)``, ``@Invalid``,
      ``${Pair(lo = 1w4, hi = 2w4)}``); type names in the output (struct
      literals) are printed package-free.  A value box too narrow for its
      text stretches the timeline rather than dropping the label.  The
      stretch is uniform -- one zoom factor for the whole figure, large
      enough for the widest label -- so the shared grid stays even and the
      clock keeps its visual period.  The factor is capped by readability:
      at least VIS_CLK_PERIODS clock periods must stay visible without
      scrolling.  A label wider than its run at the capped zoom is
      abbreviated with an ellipsis rather than dropped: the full value is
      always available in the hover popup (vcd.js), which shows every
      signal's value at the cursor's time.  If that pushes the figure past its target width,
      a mild overflow is squeezed back down to fit by CSS-scaling the whole
      SVG (the text shrinks a couple of px but every label stays visible);
      only an overflow too large for the text to remain legible falls back
      to horizontal scrolling.  In a scrolling figure the gutter is a
      separate sticky SVG (vcd.css pins it at the left edge of the box,
      over an opaque background, while the waveform slides beneath --
      compositor-driven, so it never jitters), and vcd.js clamps each
      value label to the gutter's right edge while its box is at least
      partly visible, hiding the box and label once the box has scrolled
      fully off.
      The type may be fully-qualified
      (``pkg::Name[...]``) or bare for builtin types; vir-value resolves it
      against the project found by walking up from the trace file.
      An unknown type fails the build; a pattern that is not a valid
      inhabitant of the type (the sim random-initializes uninitialized
      cells, so never-written enum inputs carry undecodable garbage at
      time 0) renders as ``<invalid>`` instead, matching how the sim
      itself displays such values.
  ``space``
      A blank separator row, for visually grouping signals.
  ``label <text>``
      A labelled separator row (the text may contain spaces).

A signal that matches nothing in the trace is an error, so a typo'd name
fails the build instead of silently dropping a row.  An empty body draws
every signal in the trace.

The path is given as the directive argument, relative to the document (or
to the source root).  An ``.svg`` path is accepted as shorthand for the
``.vcd`` file sitting beside it, so documents can reference the rendered
artifact (e.g. from ``tests/poke/*/run.sh``); the waveform is still
re-rendered from the VCD so that vcd.css theming (light/dark mode) applies.

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

# Font size (px) of the waveform text: signal names, data values, and the
# head line.  The actual font-size lives in vcd.css (.vcd-svg text and
# .vcd-name) -- keep the two in sync.  13px is one step under the docs'
# .small-code size (14px), itself one step under the 17px body/code text.
# Every vertical geometry constant below is derived from this, so the
# whole diagram scales with it.
FONT_PX = 13
SCALE = FONT_PX / 10.0

# Never let a tick get smaller than this, no matter how long the trace is.
# This is the "zoomed all the way out" end of the GTKWave-style fit: a trace
# longer than (target waveform width) ticks will exceed the target width and
# be tagged .vcd-fig--overflow (scrolling on .vcd-body, sticky gutter) rather
# than collapsing into an unreadable smear.
MIN_TICK_W = 1.0

# When a figure's natural width exceeds its target width -- a stretched
# value box pushing past the target, or the MIN_TICK_W floor above -- it may
# still fit by CSS-scaling the whole SVG down to the column width, but only
# while the baked-in text stays legible.  A mild overflow squeezes down
# "just enough": the figure is tagged .vcd-svg--squeeze, which falls under
# the stylesheet's ordinary fit-to-column scaling (max-width:100% /
# width:100%), so no scrollbar appears and every label stays visible -- the
# text is merely a few px smaller.  A severe overflow is tagged
# .vcd-fig--overflow: it keeps its natural width, scrolling instead, with
# the signal-name gutter split into its own sticky SVG (see render_svg): CSS
# scaling has no natural stopping point (decoded type text can be
# arbitrarily long), so this floor is the bound.  11px: the stylesheet's
# narrow-phone guidance treats ~12px as still legible, and dipping slightly
# under that is preferable to dropping a decoded value label or adding a
# scrollbar for a figure that is only a quarter too wide.
MIN_SQUEEZE_FONT_PX = 11.0

# How many clock periods of a stretched figure must stay visible inside one
# screenful (the plain viewport width, no horizontal scrolling).  A type-
# decoded value box can demand an arbitrarily long label, and stretching a
# label's whole run uniformly still inflates the figure's zoom, so the
# uniform factor is capped: at least this many periods of the figure's
# clock signal (the first wave with 'P' posedge markers) must fit inside
# the waveform area of the viewport.  Scrolling is therefore never needed
# to see a run of cycles -- only to reach later cycles of a long trace.
# 7 rather than fewer: enough to follow a rule spanning several beats
# without losing count.  Chosen by eye, not derived; a figure with no
# clock signal is not capped, and neither is one whose whole trace spans
# fewer than this many periods -- there, the condition is unsatisfiable at
# any zoom (shrinking cannot conjure cycles the trace does not contain),
# so applying it would only suppress the stretch a label legitimately
# needs, at no readability gain.
VIS_CLK_PERIODS = 7

# Stroke width of the level lines (.vcd-line/.vcd-zline in vcd.css) in
# SVG user units.  Not scaled with SCALE: the CSS specifies a fixed
# 1.5px at any diagram size, and geometry here must match to align
# shapes with the lines' outer edges -- e.g. the true-state shading
# extends to the bottom of the '0' line's stroke (y_bot + half width)
# so the wash lies flush with the line instead of stopping at its
# centreline.  Keep in sync with vcd.css.
LINE_STROKE_W = 1.5

# Vertical height reserved for one signal row.  Rows are stacked directly on
# top of each other with no extra spacing, so this is also the row pitch.
# Must be large enough to contain both Y_TOP and Y_BOT plus a little
# breathing room, otherwise neighbouring rows' lines will collide.
ROW_H = 26 * SCALE

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
Y_TOP, Y_BOT, Y_MID = 8 * SCALE, 22 * SCALE, 15 * SCALE

# Map from a WaveJSON wave character to its level (see diagram above).
# Characters not in this map ('x', '4', '.') are drawn as boxes or repeats
# rather than level lines.
# 'P' is a posedge marker (emitted by vcd2wavedrom.py's `:clk` format): it
# renders at the high level, with an up arrow drawn at the tick boundary
# (see the arrow pass in render_svg).
LEVELS = {'0': Y_BOT, '1': Y_TOP, 'z': Y_MID, 'P': Y_TOP}

# Approximate advance width of one character of the monospace font used
# inside the SVG (10px Fira Mono is ~6.6px per character; scaled with
# FONT_PX).  Used only for measuring, never drawing:
#   * sizing the label gutter (widest signal name must fit),
#   * deciding whether a data value (e.g. "0x2") fits inside its box, or
#   whether the box is too narrow and the label should be omitted.
CHAR_W = 6.6 * SCALE

# Truncation marker for a value label that does not fit its box: drawn as
# the last character of the abbreviated label, at the same monospace
# advance width (Fira Mono has the ellipsis glyph).  The full value stays
# available in the hover popup (vcd.js), so the abbreviation is only a
# visual hint, never a loss of information.
ELLIPSIS = '…'

# Approximate advance width of one character of the font used for the
# true/false labels that annotate changes on 1-bit signals (see the
# bitlabels pass in render_svg).  The labels use the same 13px Fira Mono
# as multi-bit data labels (no font-size override in vcd.css), whose
# advance is ~0.6em, so 7.8px per character at 13px.
BITLABEL_CHAR_W = 6.0 * SCALE


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
      'P'          posedge marker (from the `:clk` format): renders at the
                   high level with an up arrow at its tick boundary.  A '.'
                   following a 'P' carries the high level as a plain '1',
                   so only the posedge tick itself is marked.
    """
    tokens = []
    prev = None        # last value char seen ('4' stays '4' through '.')
    prev_value = None  # data value carried by the last '4' token
    di = 0             # cursor into the `data` list
    for c in wave:
        if c == '.':
            if prev is None:
                continue
            # A repeat after a posedge marker is a plain high level: the
            # arrow belongs only to the edge itself.
            if prev == 'P':
                prev = '1'
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
    exclusive: tick indices, mapped to x-coordinates through the tick
    x-grid (gx in render_svg) -- uniform k*tick_w when no run stretches.
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


def clock_period_ticks(signals):
    """The figure's clock period on its tick grid, in ticks, or None.

    The first signal whose wave carries 'P' posedge markers defines the
    clock (vcd2wavedrom.py emits 'P' only for the ':clk' format).  The
    period is the median gap between consecutive 'P' markers: the median
    shrugs off a rare irregular edge (clock gating, a glitch) that a
    minimum would latch onto.  Fewer than two markers means no period is
    observable, so the caller gets None and skips the visibility cap.
    """
    for sig in signals:
        if 'P' not in sig.get('wave', ''):
            continue
        edges = [i for i, c in enumerate(expand_wave(sig['wave'],
                                                     sig.get('data', [])))
                 if c[0] == 'P']
        if len(edges) < 2:
            continue
        gaps = sorted(b - a for a, b in zip(edges, edges[1:]))
        return gaps[len(gaps) // 2]
    return None


def render_svg(wavejson, target_width, variant='desktop'):
    """Render WaveJSON (as produced by vcd2wavedrom.py) into SVG markup.

    `target_width` is the width the SVG should occupy, in CSS pixels.  The
    "zoom" is applied at the level of geometry -- pixels per tick -- NOT by
    CSS-scaling the finished SVG, so font sizes stay fixed and crisp no
    matter how the trace is stretched or compressed.  `variant` marks the
    responsive variant (vcd.css shows/hides desktop/mobile variants).  A
    figure that overflows returns a .vcd-fig flex row of two SVGs (sticky
    gutter + waveform); any other figure returns a single SVG string.
    """
    signals = wavejson['signal']

    # Longest wave string determines the total number of ticks, hence the
    # width of the waveform area.  Separator rows (space/label, no 'wave')
    # don't contribute.
    n_ticks = max((len(expand_wave(s['wave'], s.get('data', [])))
                   for s in signals if 'wave' in s), default=0)

    # Width of the left-hand gutter that holds signal names (and label
    # rows' group titles).  Sized to fit the longest text (CHAR_W per
    # character) plus scaled padding so the longest name does not visually
    # touch the waveform area; floored at a scaled minimum so short names
    # still get a reasonable gutter.
    texts = [s['name'] for s in signals if 'name' in s]
    texts += [s['label'] for s in signals if 'label' in s]
    label_w = max(40 * SCALE,
                  int(max((len(t) for t in texts), default=0) * CHAR_W)
                  + 24 * SCALE)

    # Vertical space above the first signal row: one head line when there is
    # a head text ("trace.vcd  (1 tick = ...)" rendered top-right), minimal
    # padding when there is not.
    head_h = 18 * SCALE if wavejson.get('head', {}).get('text') else 4 * SCALE

    # Right-hand padding between the last tick and the SVG border.  Also
    # anchors the right-aligned head text.
    pad_r = 12 * SCALE

    # "Zoom to fit": choose the pixels-per-tick so that the waveform area
    # (n_ticks ticks) plus gutter and right padding lands exactly on the
    # target width.  This is the GTKWave zoom analogue: a short trace is
    # stretched (zoomed in, each tick wide), a long trace compressed (zoomed
    # out, each tick narrow) -- while fonts stay at their fixed pixel sizes.
    # The MIN_TICK_W floor ("zoomed all the way out") is the one case where
    # the SVG may be wider than target_width; it then scrolls horizontally.
    zoom_floored = False
    if n_ticks:
        raw_tick_w = (target_width - label_w - pad_r) / n_ticks
        if raw_tick_w < MIN_TICK_W:
            tick_w = MIN_TICK_W
            zoom_floored = True
        else:
            tick_w = raw_tick_w
    else:
        tick_w = 0

    # WaveDrom-style stretch: a data label too wide for its run's natural
    # span stretches the timeline instead of the label being omitted.  The
    # stretch is applied uniformly -- one zoom factor for the whole trace,
    # the largest any label needs -- rather than stretching each label's
    # run locally.  Local stretching would make the shared grid non-uniform:
    # the clock's visual period would vary with the label widths around it
    # (a run of long state labels reads as a clock slowdown).  With the
    # uniform factor every tick keeps the same width, so clock edges stay
    # evenly spaced, and every label still fits its box (its run's box
    # grows to (j - i) * tick_w * factor >= w -- the run's own width is
    # distributed proportionally, so a run ending one settle-sliver tick
    # after a clock edge (the sim stamps non-clock changes at edge +
    # period/20) stays proportional to that edge instead of ballooning).
    # Only rows tagged "stretch" (Virdant type-decoded signals, whose ctor
    # text is far wider than a hex label) demand room, so ordinary figures
    # keep the uniform grid and render exactly as before.  When nothing
    # stretches, xs stays None and x(tick) is the plain k*tick_w grid.
    needs = []   # (start, end, minimum x-width) per stretched run
    for sig in signals:
        if not sig.get('stretch'):
            continue
        stoks = expand_wave(sig['wave'], sig.get('data', []))
        for (i, j, c, v) in runs_of(stoks):
            if c == '4' and v is not None:
                needs.append((i, j, len(v) * CHAR_W + 2 * 2 * SCALE))
    xs = None
    if n_ticks and needs:
        # One zoom factor for the whole trace: the largest stretch any label
        # needs.  Every tick then has the same width, so clock edges stay
        # evenly spaced and the waveform reads as constant-rate, while each
        # label still fits its box.
        factor = max(w / ((j - i) * tick_w) for i, j, w in needs)
        # The visibility cap: however far the widest label demands the
        # figure zoom, the figure's tick width must stay small enough that
        # VIS_CLK_PERIODS clock periods still fit inside the waveform area
        # of the viewport (the plain target width, no horizontal
        # scrolling).  Labels too long for their run at the capped zoom are
        # abbreviated with an ellipsis rather than dropped, so the reader
        # still sees the shape of the value (the state name, the leading
        # digits) -- the full text is never lost, because the hover popup
        # (vcd.js) shows the full value of the run under the cursor.
        # The cap only applies to traces that actually span that many
        # periods: a short trace shows every cycle it has regardless of
        # zoom, so capping there cannot buy any readability -- it would
        # only clamp away the stretch a label needs to stay whole (e.g.
        # @Valid(11) on a 6-period trace would ellipsize to @Valid(1...)).
        period_ticks = clock_period_ticks(signals)
        if (period_ticks is not None
                and n_ticks >= VIS_CLK_PERIODS * period_ticks):
            wave_area = target_width - label_w - pad_r
            factor = min(factor, wave_area / (VIS_CLK_PERIODS * period_ticks
                                               * tick_w))
        if factor > 1.0:
            xs = [t * tick_w * factor for t in range(n_ticks + 1)]

    # Hover data for the time popup (vcd.js): for every signal row, in
    # drawing order, the runs as (start tick, end tick, full value text).
    # Tick indices rather than x-coordinates: the popup resolves the
    # cursor's tick arithmetically (the grid is uniform), which is exact
    # regardless of how the figure is CSS-scaled or scrolled, and the
    # attribute payload stays small.  The text is the *full* value -- the
    # inline label may be abbreviated, but the popup must show the whole
    # thing, so no information is lost by the ellipsis.
    # Clock signals (the ':clk' format, whose wave string carries 'P'
    # posedge markers) are excluded: their edges are timing markers, not
    # values, so a clock line in the popup would just repeat '1'/'0'
    # noise.  The WaveJSON has no separate clk flag, but the 'P' marker
    # is emitted only for the ':clk' format, so the wave string is the
    # marker (vcd2wavedrom.py falls back to hex for a multi-bit :clk,
    # which emits no 'P').
    hover_rows = []
    for sig in signals:
        if 'name' not in sig or 'wave' not in sig:
            continue
        if 'P' in sig['wave']:
            continue
        tokens = expand_wave(sig['wave'], sig.get('data', []))
        bitlabels = sig.get('bitlabels')
        runs = []
        for (i, j, c, v) in runs_of(tokens):
            if c == '4':
                text = v if v is not None else 'x'
            elif c == 'x':
                text = 'x'
            elif c == 'z':
                text = 'z'
            elif bitlabels and c in ('0', '1'):
                text = 'true' if c == '1' else 'false'
            else:
                text = c
            runs.append([i, j, text])
        hover_rows.append({'n': sig['name'], 'runs': runs})

    # The x of a tick index, through the (possibly stretched) grid.
    def gx(k):
        return xs[k] if xs is not None else k * tick_w

    # Total SVG canvas size: gutter + waveform area + right padding across,
    # head line + all signal rows + a little bottom padding down.
    wave_w = xs[-1] if xs else n_ticks * tick_w
    width = label_w + wave_w + pad_r
    height = head_h + len(signals) * ROW_H + 8 * SCALE

    # Extent of the signal-row area: rows run from head_h down to
    # head_h + n_signals * ROW_H.  The waveform group's translate is
    # x-only, so these are already SVG-root coordinates.
    row_y0, row_y1 = head_h, head_h + len(signals) * ROW_H

    parts = []
    # Geometry the hover "current time cursor" in vcd.js needs: the
    # waveform's x range (excluding the signal-name gutter and right
    # padding) and the y range spanned by the signal rows.  All values are
    # SVG-root user units, so the JS needs no transform math.
    # An overflow -- detected via the floor condition itself, not a raw
    # width comparison, because a trace that fits exactly can land a few
    # ulps above target_width in floating point (e.g.
    # 310.00000000000006 > 310) and would be falsely tagged -- is split by
    # magnitude.  If CSS-scaling the whole SVG down to the target width
    # would leave the baked-in text at MIN_SQUEEZE_FONT_PX or larger, tag
    # .vcd-svg--squeeze: the stylesheet's ordinary fit-to-column scaling
    # rules then shrink the figure exactly enough to fit, trading a couple
    # of px of text size for a label-complete, scrollbar-free figure.  (The
    # SVG is vector, so the downscale stays crisp, and vcd.js reads the
    # live scale via getBoundingClientRect, so the hover time cursor stays
    # accurate.)  Otherwise the figure becomes a .vcd-fig--overflow flex
    # row: it keeps its natural width, scrolling horizontally in
    # .vcd-body with the gutter pinned sticky, instead of collapsing the
    # text (or the post-stretch timing proportions) into unreadability.
    overflows = zoom_floored or (xs is not None and width > target_width * (1 + 1e-4))
    if not overflows:
        overflow_class = ''
    elif FONT_PX * target_width / width >= MIN_SQUEEZE_FONT_PX:
        overflow_class = ' vcd-svg--squeeze'
    else:
        # Not a class: a severe overflow is emitted as a .vcd-fig flex row
        # (see below), not tagged on the SVG.
        overflow_class = ''
    split_gutter = overflows and not overflow_class
    if split_gutter:
        # The waveform SVG spans just the waveform area plus the right
        # padding -- no gutter.  Its coordinates (and the data-* hover
        # attributes below) are therefore gutter-free: the waveform starts
        # at x0 = 0, not label_w.
        wave_svg_w = wave_w + pad_r
        svg_class = 'vcd-svg'
        # :g trims floating-point noise from the geometry (e.g. 310.0 instead of
        # 310.00000000000006); %g-formatting keeps short decimals readable.
        # The hover data attributes (vcd.js): the number of ticks, to resolve
        # the cursor's tick, and the per-signal runs carrying the *full* value
        # text.
        parts.append(
            f'<svg class="{svg_class}" viewBox="0 0 {wave_svg_w:g} {height:g}" '
            f'width="{wave_svg_w:g}" height="{height:g}" '
            f'xmlns="http://www.w3.org/2000/svg" role="img" '
            f'data-waveform-x0="0" data-waveform-x1="{wave_w:g}" '
            f'data-waveform-y0="{row_y0:.2f}" data-waveform-y1="{row_y1:.2f}" '
            f'data-ticks="{n_ticks}" '
            f'data-values="{escape(json.dumps(hover_rows, separators=(",", ":")))}">')
    else:
        svg_class = f'vcd-svg vcd-svg--{variant}{overflow_class}'.strip()
        parts.append(
            f'<svg class="{svg_class}" viewBox="0 0 {width:g} {height:g}" width="{width:g}" '
            f'height="{height:g}" xmlns="http://www.w3.org/2000/svg" role="img" '
            f'data-waveform-x0="{label_w:g}" data-waveform-x1="{label_w + wave_w:g}" '
            f'data-waveform-y0="{row_y0:.2f}" data-waveform-y1="{row_y1:.2f}" '
            f'data-ticks="{n_ticks}" '
            f'data-values="{escape(json.dumps(hover_rows, separators=(",", ":")))}">')

    # Waveform drawing area.  In the split figure the SVG is already
    # gutter-free, so there is no translate; in the single-SVG figure the
    # group is offset by the label gutter.  The class name marks the group
    # as the waveform itself: vcd.js uses it to identify the figure as a
    # waveform, and it is the selector to update if the structure below
    # the svg ever changes.
    if split_gutter:
        parts.append('<g class="vcd-waveform">')
    else:
        parts.append(f'<g class="vcd-waveform" transform="translate({label_w},0)">')

    for idx, sig in enumerate(signals):
        # Top edge of this row on the canvas, and the three level y-positions
        # within the row (see the geometry diagram above the constants).
        row_y = head_h + idx * ROW_H
        y_top, y_bot, y_mid = row_y + Y_TOP, row_y + Y_BOT, row_y + Y_MID

        # Separator rows carry no waveform: 'space' is a fully blank row;
        # 'label' is a row whose title text sits in the gutter (below).
        if 'label' in sig:
            continue
        if 'space' in sig:
            continue

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
            x0, x1 = gx(i), gx(j)
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
                x = gx(k + 1)
                h_path.append(f'M {x} {row_y + LEVELS[c1]} L {x} {row_y + LEVELS[c2]}')

        # True-state shading for 1-bit non-clk signals (vcd2wavedrom.py
        # tags them with bitlabels=True): each run where the signal is
        # true gets a translucent dark-green wash spanning the full
        # height of the signal (y_top..y_bot), so the true level reads
        # as "filled in" -- the same full-height span the 'x' and '4'
        # boxes use, extended half a line-stroke past y_bot so the wash
        # lies flush with the bottom of the '0' line's stroke rather
        # than stopping at its centreline.  Runs at false, 'z', or data
        # boxes are left unshaded.  Drawn before the level lines so the
        # lines stay on top of the wash.
        shade_parts = []
        if sig.get('bitlabels'):
            for (i, j, c, _v) in runs:
                if c != '1':
                    continue
                x0, x1 = gx(i), gx(j)
                shade_parts.append(
                    f'<rect class="vcd-shade" x="{x0:g}" y="{y_top:.6g}" '
                    f'width="{x1 - x0:g}" '
                    f'height="{y_bot - y_top + LINE_STROKE_W / 2:.6g}"/>')

        if shade_parts:
            parts.extend(shade_parts)
        if h_path:
            parts.append(f'<path class="vcd-line" d="{" ".join(h_path)}"/>')
        if z_path:
            parts.append(f'<path class="vcd-zline" d="{" ".join(z_path)}"/>')

        # Posedge arrows: one arrowhead per 'P' token, drawn ON the rising
        # edge itself -- apex at the high line, base a little way down the
        # edge, so the edge reads as an upward arrow in the signal's own
        # color (see .vcd-clk in vcd.css).
        for k, (c, _v) in enumerate(tokens):
            if c != 'P':
                continue
            ax = gx(k)
            half = 3 * SCALE
            base = y_top + 5 * SCALE
            parts.append(
                f'<path class="vcd-clk" d="M {ax - half:.6g} {base:.6g} '
                f'L {ax + half:.6g} {base:.6g} L {ax:.6g} {y_top:.6g} Z"/>')

        # True/false labels for 1-bit signals (vcd2wavedrom.py tags them
        # with bitlabels=True): one label per run, drawn between the two
        # level lines, just above the '0' line -- the waveform keeps its
        # plain 0/1 look, the label only annotates what each level means.
        # Both labels share one baseline (just above the '0' line), so
        # 'true' sits under the '1' line -- on top of the true-state
        # shading -- and 'false' sits above the '0' line, vertically
        # aligned with each other.  Both glyphs fit in the row's middle:
        if sig.get('bitlabels'):
            for (i, j, c, _v) in runs:
                if c not in ('0', '1'):
                    continue
                x0, x1 = gx(i), gx(j)
                label = 'true' if c == '1' else 'false'
                pad = 2.5 * SCALE
                # Omit the label when its run is too narrow to hold it
                # with a little breathing room on each side.
                if len(label) * BITLABEL_CHAR_W > (x1 - x0) - 2 * pad:
                    continue
                # Shared baseline, 3*SCALE above the '0' line: the 13px
                # glyphs' ascent (~9.8px) clears the '1' line by about a
                # pixel, and the labels never bleed into a neighbouring
                # row.
                y = y_bot - 3 * SCALE
                parts.append(
                    f'<text class="vcd-bitlabel" x="{x0 + pad:g}" '
                    f'y="{y:.2f}" text-anchor="start" '
                    f'data-x0="{x0:g}" data-x1="{x1:g}">{label}</text>')

        # Boxes: drawn per run, spanning the full Y_TOP..Y_BOT range.
        # 'x'  -> rectangle with a diagonal cross (unknown/bus fighting)
        # '4'  -> rectangle carrying the hex data value as centred text
        for (i, j, c, v) in runs:
            x0, x1 = gx(i), gx(j)
            if c == 'x':
                parts.append(
                    f'<rect class="vcd-xbox" x="{x0}" y="{y_top}" '
                    f'width="{x1 - x0}" height="{y_bot - y_top}"/>')
                parts.append(
                    f'<path class="vcd-cross" d="M {x0} {y_top} L {x1} {y_bot} '
                    f'M {x0} {y_bot} L {x1} {y_top}"/>')
            elif c == '4':
                # The box and its value label travel in a group tagged with
                # the run's extent (vcd.js clamps the label to the left edge
                # of the visible waveform while the box is at least partly
                # on screen, and hides the group once the box has scrolled
                # fully off -- a clamped label over nothing would mislead).
                parts.append(
                    f'<g class="vcd-run" data-x0="{x0:g}" data-x1="{x1:g}">')
                parts.append(
                    f'<rect class="vcd-dbox" x="{x0}" y="{y_top}" '
                    f'width="{x1 - x0}" height="{y_bot - y_top}"/>')
                label = v if v is not None else ''
                # Values are left-justified inside the box with a small
                # padding, so runs of changing values read as aligned text
                # rather than wobbling with each box's width.  A label that
                # does not fit its box is truncated with an ellipsis rather
                # than dropped: the reader sees the shape of the value (the
                # state name, the leading digits) and the full text is
                # always available in the hover popup (vcd.js), which
                # shows the full value of the run under the cursor.
                pad = 2 * SCALE
                avail = x1 - x0 - pad
                max_chars = int(avail / CHAR_W)
                if max_chars <= 0:
                    parts.append('</g>')
                    continue
                if len(label) > max_chars:
                    label = label[:max_chars - 1] + ELLIPSIS
                parts.append(
                    f'<text class="vcd-data" x="{x0 + pad:g}" '
                    f'y="{row_y + Y_MID + 3.5 * SCALE}" text-anchor="start">'
                    f'{escape(label)}</text>')
                parts.append('</g>')

    parts.append('</g>')

    if split_gutter:
        # Split figure: the waveform SVG above carries the head line and
        # the waveform, and the gutter is a separate SVG returned beside
        # it inside a .vcd-fig flex row.  vcd.css pins the gutter SVG
        # with position:sticky -- compositor-driven, so the names stay
        # pinned with zero lag while the waveform slides beneath the
        # gutter's opaque background.
        if wavejson.get('head', {}).get('text'):
            head = wavejson['head']['text']
            parts.append(
                f'<text class="vcd-head" x="{wave_svg_w - pad_r:g}" '
                f'y="{12 * SCALE:.1f}" '
                f'text-anchor="end">{escape(head)}</text>')
        parts.append('</svg>')

        # Gutter SVG: an opaque background (so the waveform content
        # passing underneath while the figure is scrolled is masked), a
        # hairline rule just inside its right edge, and the gutter text
        # -- signal names for signal rows, group titles for label rows,
        # nothing for space rows.  All gutter text is left-justified, so
        # names and group titles share a common left edge regardless of
        # length.  Same vertical geometry as the waveform SVG (head_h,
        # one ROW_H per row), so the rows align exactly across the two
        # SVGs.  aria-hidden: the names are decorative duplicates of the
        # hover popup's signal names, not reachable content.
        gutter = [
            f'<svg class="vcd-svg vcd-gutter-svg" '
            f'viewBox="0 0 {label_w:g} {height:g}" '
            f'width="{label_w:g}" height="{height:g}" '
            f'xmlns="http://www.w3.org/2000/svg" aria-hidden="true">',
            f'<rect class="vcd-gutter-bg" x="0" y="0" width="{label_w:g}" '
            f'height="{height:g}"/>',
            f'<line class="vcd-gutter-rule" x1="{label_w - 0.5:g}" y1="0" '
            f'x2="{label_w - 0.5:g}" y2="{height:g}"/>',
        ]
        for idx, sig in enumerate(signals):
            row_y = head_h + idx * ROW_H
            if 'name' in sig:
                text, cls = sig['name'], 'vcd-name'
            elif 'label' in sig:
                text, cls = sig['label'], 'vcd-label'
            else:
                continue
            gutter.append(
                f'<text class="{cls}" x="{8 * SCALE:.1f}" '
                f'y="{row_y + Y_MID + 4 * SCALE:.1f}" '
                f'text-anchor="start">{escape(text)}</text>')
        gutter.append('</svg>')
        return (f'<div class="vcd-fig vcd-fig--{variant} vcd-fig--overflow">'
                f'{"".join(gutter)}{"".join(parts)}</div>')

    # Single-SVG figure (fit or squeeze): it never scrolls, so the gutter
    # stays an ordinary group inside the SVG: an opaque background (harmless
    # here -- nothing ever passes beneath it), a hairline rule at the
    # gutter's right edge, and the gutter text -- signal names for signal
    # rows, group titles for label rows, nothing for space rows.  All
    # gutter text is left-justified, so names and group titles share a
    # common left edge regardless of length.
    parts.append('<g class="vcd-gutter">')
    parts.append(
        f'<rect class="vcd-gutter-bg" x="0" y="0" width="{label_w:g}" '
        f'height="{height:g}"/>')
    parts.append(
        f'<line class="vcd-gutter-rule" x1="{label_w:g}" y1="0" '
        f'x2="{label_w:g}" y2="{height:g}"/>')
    for idx, sig in enumerate(signals):
        row_y = head_h + idx * ROW_H
        if 'name' in sig:
            text, cls = sig['name'], 'vcd-name'
        elif 'label' in sig:
            text, cls = sig['label'], 'vcd-label'
        else:
            continue
        parts.append(
            f'<text class="{cls}" x="{8 * SCALE:.1f}" '
            f'y="{row_y + Y_MID + 4 * SCALE:.1f}" '
            f'text-anchor="start">{escape(text)}</text>')
    parts.append('</g>')

    # The head line is drawn last, on top of the gutter group: it is
    # right-aligned at the figure's right edge, and only a very long head
    # text would intrude into the gutter's x range -- drawing it above the
    # gutter background reads better there than having it masked.
    if wavejson.get('head', {}).get('text'):
        head = wavejson['head']['text']
        parts.append(
            f'<text class="vcd-head" x="{width - pad_r}" y="{12 * SCALE:.1f}" '
            f'text-anchor="end">{escape(head)}</text>')

    parts.append('</svg>')
    return ''.join(parts)


class VcdDirective(Directive):
    has_content = True
    required_arguments = 0
    optional_arguments = 1
    final_argument_whitespace = True
    option_spec = {
        'caption': directives.unchanged,
        'file': directives.path,
        # The signal list lives in the directive body, one per line -- not
        # in an option.  'signals' is deliberately absent from this spec so
        # that docutils rejects it outright ("unknown option: signals").
        # Picoseconds per rendered tick.  Defaults to 500000 (half of the
        # virt platform's clock period), which quantizes every change onto
        # a clock edge — traces that illustrate propagation delay need a
        # finer grid to render the sub-cycle offsets at all.
        'tick': directives.positive_int,
    }

    def resolve_file(self, filename, docdir, srcdir):
        """Resolve a :file: path or argument to an existing VCD file.

        Tries each candidate against the document directory first, then the
        source root.  An ``.svg`` argument is shorthand for the ``.vcd``
        beside it (see module docstring); only the ``.vcd`` is looked up,
        even if the ``.svg`` itself exists.
        """
        candidates = [filename[:-4] + '.vcd' if filename.endswith('.svg')
                      else filename]
        for candidate in candidates:
            for base in (docdir, srcdir):
                path = os.path.normpath(os.path.join(base, candidate))
                if os.path.exists(path):
                    return path
        return None

    def run(self):
        env = self.state.document.settings.env
        filename = self.arguments[0] if self.arguments else self.options.get('file')
        if not filename:
            raise self.error('vcd directive requires a file argument')

        # Resolve the path relative to the document, then to the source root.
        docdir = os.path.dirname(env.doc2path(env.docname))
        path = self.resolve_file(filename, docdir, env.srcdir)
        if path is None:
            raise self.error(f'vcd file not found: {filename}')

        # Track the VCD as a build dependency of this document, so that
        # editing the trace alone (without touching the .rst) triggers a
        # rebuild of this document -- otherwise a changed trace leaves a
        # stale SVG and stale warnings behind.
        env.note_dependency(path)

        script = os.path.normpath(os.path.join(
            os.path.abspath(os.path.dirname(__file__)), '..', 'vcd2wavedrom.py'))

        # The directive body is a line-based layout, one item per nonblank
        # line (empty lines have no effect; they just let authors spread
        # the layout out):
        #
        #     signal <name>[:<format>]   a signal to draw
        #     space                      a blank row (visually groups signals)
        #     label <text>               a labelled separator row
        #
        # An empty body means "draw every signal in the trace".
        layout_lines = []
        for line in self.content:
            line = line.strip()
            if not line:
                continue
            kind, _, rest = line.partition(' ')
            rest = rest.strip()
            if line == 'space':
                layout_lines.append('space')
            elif kind == 'signal':
                if not rest:
                    raise self.error("'signal' needs a signal name")
                if ',' in rest:
                    raise self.error(
                        f'one signal per line, without commas: {rest!r}')
                if len(rest.split()) > 1:
                    raise self.error(
                        f'one signal per line (is this two signals on one '
                        f'line?): {rest!r}')
                layout_lines.append(f'signal {rest}')
            elif kind == 'label':
                if not rest:
                    raise self.error("'label' needs label text")
                layout_lines.append(f'label {rest}')
            else:
                raise self.error(
                    f'invalid line: {line!r}; expected '
                    f"'signal <name>[:<format>]', 'space', or 'label <text>'")

        # Hand the body to vcd2wavedrom.py verbatim as a layout file, so it
        # stays the single place that knows this syntax.
        layout_path = None
        if layout_lines:
            lfd, layout_path = tempfile.mkstemp(suffix='.layout')
            with os.fdopen(lfd, 'w') as f:
                f.write('\n'.join(layout_lines) + '\n')

        fd, out_path = tempfile.mkstemp(suffix='.json')
        os.close(fd)
        try:
            cmd = [sys.executable, script, path, out_path]
            if layout_path:
                cmd += ['-l', layout_path]
            result = subprocess.run(cmd, capture_output=True, text=True)
            if result.returncode != 0:
                raise self.error(
                    f'vcd2wavedrom failed for {filename}:\n'
                    f'{result.stderr.strip() or result.stdout.strip()}')
            # vcd2wavedrom exits nonzero (reported as a located error above)
            # when a requested signal matches nothing in the trace, so a
            # typo'd signal name fails the build instead of silently dropping
            # a row from the waveform.  Remaining stderr is advisory only
            # (e.g. ':clk' on a multi-bit signal falls back to hex).
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
            if layout_path:
                os.unlink(layout_path)

        # Two variants of the same figure, swapped by a CSS media query:
        # a desktop one zoomed to fit the ~680px content column, and a
        # mobile one zoomed to fit a ~310px phone column.  Scaling the
        # desktop SVG down on a phone would shrink the baked-in text to an
        # unreadable ~7px; re-rendering at the mobile target width keeps
        # text at a true 13px (vcd.css shows/hides the variants).
        html = (
            f'<div class="vcd-body">'
            f'{render_svg(wavejson, env.config.vcd_waveform_width, "desktop")}'
            f'{render_svg(wavejson, env.config.vcd_waveform_width_mobile, "mobile")}'
            f'</div>')
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
    # Target width (px) for the mobile variant of vcd SVGs, swapped in by
    # the <=768px media query in vcd.css.  A common phone viewport (375px)
    # yields a ~323px content column, minus .vcd-body's border/padding
    # -> ~310px for the SVG itself.  On wider phones the mobile SVG is
    # scaled up slightly by CSS (text grows a little, which reads fine);
    # on narrower ones it is scaled down (text ~12px at 320px, still
    # legible).
    app.add_config_value('vcd_waveform_width_mobile', 310, 'env')
    return {
        'version': '0.1',
        'parallel_read_safe': True,
        'parallel_write_safe': True,
    }
