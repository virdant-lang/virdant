#!/usr/bin/env python3
"""Convert a VCD file to WaveJSON for WaveDrom rendering.

usage: vcd2wavedrom.py <in.vcd> <out.json> [-s name1,name2,...] [--tick PS]

Signal names may be given as full paths ("top.clock") or leaf names ("clock",
which matches every scope containing a signal of that name). Signals are
rendered in the order given on the command line (or VCD declaration order
when no filter is given).

Each name may carry a value format after a colon ("top.count:dec"), choosing
how multi-bit values are rendered in the data boxes. The supported formats
are the keys of FORMATTERS (currently bin, hex, dec); the default is hex.
Any other format text is taken as the signal's *Virdant type*
("top.cmd:shiftreg::Command[Word[2]]"): each distinct bit pattern is decoded
by the `vir-value` tool and rendered as its round-trippable value text
(`@Valid(179w8)`, `@Invalid`, `${Pair(lo = 1w4, hi = 2w4)}`).  Type names may
be fully-qualified (`pkg::Name[...]`) or bare for builtin types; type names
appearing in the decoded output (struct literals) are printed package-free.
vir-value needs a project context, found by walking up from the VCD file to
the nearest virdant.toml (overridable with --project; builtin-only fallback
otherwise).

The signal list may also carry layout separators, via -l/--layout: a file
with one item per line --

    signal <name>[:<format>]    a signal, exactly as the -s specs
    space                       a blank row (visually groups signals)
    label <text>                a labelled separator row (text may have spaces)

Blank lines have no effect. This is what the `.. vcd::` directive's body
uses (see docs/source/vcd_ext.py); -s remains the plain comma-separated
spec list.
"""
import argparse
import json
import math
import os
import re
import shutil
import subprocess
import sys

def parse_vcd(path):
    scope = []
    idmap = {}   # id char -> full name
    order = []   # full names in declaration order
    changes = {} # full name -> [(time, value)]
    max_t = 0    # last timestamp seen, changes or not
    t = 0
    in_defs = True
    for line in open(path):
        line = line.strip()
        if not line:
            continue
        if in_defs:
            if line.startswith("$scope"):
                scope.append(line.split()[2])
            elif line.startswith("$upscope"):
                if scope: scope.pop()
            elif line.startswith("$var"):
                parts = line.split()
                ident, name = parts[3], parts[4]
                full = ".".join(scope + [name])
                idmap[ident] = full
                changes.setdefault(full, [])
                if full not in order:
                    order.append(full)
            elif line.startswith("$enddefinitions"):
                in_defs = False
            continue
        if line.startswith("#"):
            t = int(line[1:])
            max_t = max(max_t, t)
        elif line.startswith("b"):
            val, ident = line[1:].split()
            changes[idmap[ident]].append((t, val))
        elif line[0] in "01xz":
            changes[idmap[line[1]]].append((t, line[0]))
        elif line.startswith("$dumpall") or line.startswith("$end"):
            continue
    return changes, order, max_t

def split_spec(spec):
    """Split one signal spec into (name, format).

    A spec is a signal name, optionally followed by a format after a colon
    (e.g. `top.a:dec`).  The format is looked up in FORMATTERS by the caller;
    a spec without one means "use the default".
    """
    name, sep, fmt = spec.partition(":")
    return name, fmt if sep else None


def parse_layout(text):
    """Parse layout text into [('signal', spec) | ('space', None) | ('label', str), ...].

    One item per nonblank line (see the module docstring for the syntax).
    """
    items = []
    for raw in text.splitlines():
        line = raw.strip()
        if not line:
            continue
        if line == "space":
            items.append(("space", None))
            continue
        kind, _, rest = line.partition(" ")
        rest = rest.strip()
        if kind == "signal":
            if not rest:
                sys.exit("error: layout line 'signal' needs a signal name")
            if "," in rest or len(rest.split()) > 1:
                sys.exit(f"error: layout line must name exactly one signal: {rest!r}")
            items.append(("signal", rest))
        elif kind == "label":
            if not rest:
                sys.exit("error: layout line 'label' needs label text")
            items.append(("label", rest))
        else:
            sys.exit(f"error: invalid layout line: {line!r} (expected "
                     f"'signal <name>[:<format>]', 'space', or 'label <text>')")
    return items


def filter_signals(order, wanted):
    """Return (name, format) pairs matching `wanted`, in the order given.

    Each entry of `wanted` is a signal spec: a full path ("top.clock") or a
    leaf name ("clock", matching every scope containing a signal of that
    name), optionally followed by a format ("top.count:dec").  A leaf entry
    matches in VCD declaration order among its matches.

    A spec that matches nothing is an error: a typo'd signal name would
    otherwise silently drop a row from the waveform.  The error lists the
    signals the VCD actually declares, so the fix is usually obvious.
    """
    if not wanted:
        return [(name, None) for name in order]
    wanted = [w.strip() for w in wanted.split(",") if w.strip()]
    matched = []
    taken = set()
    for w in wanted:
        name, fmt = split_spec(w)
        for candidate in order:
            if (candidate == name or candidate.rsplit(".", 1)[-1] == name) \
                    and candidate not in taken:
                matched.append((candidate, fmt))
                taken.add(candidate)
    names = [split_spec(w)[0] for w in wanted]
    missing = [w for w in names
               if w not in order and w not in {n.rsplit(".", 1)[-1] for n in order}]
    if missing:
        sys.exit(f"error: no signal matches: {', '.join(missing)}\n"
                 f"       signals in the trace: {', '.join(order)}")
    return matched

# Multi-bit values are rendered as text inside a data box.  Each supported
# format turns the raw binary string from the VCD into that text; a signal
# spec picks one explicitly (`top.a:dec`) or falls back to DEFAULT_FORMAT.
# Add new formats here (e.g. 'char', 'unsigned') to extend the CLI.
FORMATTERS = {
    'hex': lambda bits: hex(int(bits, 2)),
    'dec': lambda bits: str(int(bits, 2)),
    'bin': lambda bits: '0b' + bits,
}  # plus the 'clk' shape modifier (see CLK_FORMAT)
DEFAULT_FORMAT = 'hex'

# `clk` is not a value format but a *shape* modifier for 1-bit signals:
# the wave renders as a plain clock, with an up arrow at every posedge
# (renderer-side wave character 'P').  Makes clock edges easy to spot in
# educational waveforms.  On a multi-bit signal it falls back to
# DEFAULT_FORMAT with a warning.
CLK_FORMAT = 'clk'


# ---------------------------------------------------------------------------
# Virdant type formats (`vir-value`).  A signal spec whose format is none of
# the FORMATTERS keys above (nor 'clk') is taken as the *Virdant type* of the
# signal, e.g. `top.load_value:Valid[Word[8]]` or `top.cmd:shiftreg::Command`.
# Each distinct bit pattern on the trace is decoded by the `vir-value` tool
# (see vir/src/value/) and rendered as its round-trippable value text:
# `@Valid(179w8)`, `@Invalid`, `${Pair(lo = 1w4, hi = 2w4)}`.  Type names may
# be written fully-qualified (`pkg::Name[...]`) or bare for builtin types;
# type names appearing in the output (struct literals) are always printed
# package-free.
# ---------------------------------------------------------------------------

# Where this script lives.  The repo root (its parent) carries the fallback
# builtin-only project (lib/builtin.vir) for traces that sit outside any
# project directory.
REPO_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

# A struct literal's type name is the only place a package prefix could show
# up in decoded output; print type names package-free, whatever vir-value
# does today (`${pkg::Pair(...)` -> `${Pair(...)`).
_STRIP_PKG = re.compile(r'\$\{[A-Za-z_][A-Za-z0-9_]*::')


def is_type_format(fmt):
    """True when `fmt` is a Virdant type name rather than a value format.

    A spec format that is neither a FORMATTERS key nor 'clk' is a type; a
    typo in a real format (`:decmal`) therefore surfaces as a vir-value
    "unknown type" error -- still a build failure, just with a pointer at
    the type spelling instead of the format table.
    """
    return fmt is not None and fmt not in FORMATTERS and fmt != CLK_FORMAT


def find_project(vcd_path):
    """The nearest directory at or above the VCD's holding a virdant.toml.

    vir-value always resolves types against a project, even for builtin-only
    types; guide/example traces live beside one, so walking up finds it.
    Returns None when there is no project above the trace.
    """
    d = os.path.dirname(os.path.abspath(vcd_path))
    while True:
        if os.path.isfile(os.path.join(d, 'virdant.toml')):
            return d
        parent = os.path.dirname(d)
        if parent == d:
            return None
        d = parent


def vir_value_binary():
    """Locate the `vir-value` companion binary.

    $VIR_VALUE overrides (for tests); else PATH, which is how `vir` finds
    its own companions; else the repo's build outputs, so docs builds work
    from a fresh checkout without `make install`.
    """
    candidates = []
    override = os.environ.get('VIR_VALUE')
    if override:
        if os.path.isfile(override) and os.access(override, os.X_OK):
            return override
        sys.exit(f"error: $VIR_VALUE={override} is not an executable file")
    on_path = shutil.which('vir-value')
    if on_path:
        candidates.append(on_path)
    for profile in ('release', 'debug'):
        candidates.append(os.path.join(REPO_ROOT, 'target', profile, 'vir-value'))
    for candidate in candidates:
        if os.path.isfile(candidate) and os.access(candidate, os.X_OK):
            return candidate
    sys.exit("error: cannot find the vir-value binary "
             "(tried $VIR_VALUE, PATH, and target/{release,debug})")


_DECODE_CACHE = {}


# Substrings identifying a decode failure as "not a valid inhabitant of
# the type" rather than a configuration error; see decode_value.
_INVALID_MARKERS = re.compile(
    r'unknown tag|not a valid inhabitant|non-zero padding')


def decode_value(typ, bits, project):
    """Decode one bit pattern as Virdant type `typ`, as text.

    `project` is a project directory (or None for the builtin-only
    fallback).  Results are cached per (type, pattern): a trace typically
    revisits a handful of values, and each vir-value call builds a whole
    project Db.
    """
    key = (typ, bits, project)
    if key not in _DECODE_CACHE:
        cmd = [vir_value_binary()]
        if project:
            cmd += ['-C', project]
        else:
            cmd += ['-F', os.path.join(REPO_ROOT, 'lib', 'builtin.vir')]
        cmd += ['decode', '0b' + bits, typ]
        result = subprocess.run(cmd, capture_output=True, text=True)
        if result.returncode != 0:
            message = result.stderr.strip() or result.stdout.strip()
            # The sim random-initializes uninitialized cells, so a trace
            # can carry bit patterns that are not valid inhabitants of
            # the signal's type (unknown enum/union tag, non-zero
            # padding) -- either from never-written ports at time 0 or
            # transiently mid-settlement.  The sim itself prints those
            # as `<invalid>` (see poke's display), so render them that
            # way here too instead of failing the build.  Anything else
            # (typo'd type, missing project) is still a hard error.
            if _INVALID_MARKERS.search(message):
                _DECODE_CACHE[key] = '<invalid>'
            else:
                sys.exit(f"error: vir-value decode failed for {typ} = 0b{bits}:\n"
                         f"       {message}")
        else:
            _DECODE_CACHE[key] = _STRIP_PKG.sub('${', result.stdout.strip())
    return _DECODE_CACHE[key]


def to_wavedrom(changes, items, names, end_time, tick, project=None):
    """Build the WaveJSON signal list from layout `items` and resolved `names`.

    `names` is the resolved (full_name, format) pairs for the layout's
    signal entries, in the same order.  Separator entries become marker
    dicts the renderer understands: {'space': True} draws an empty row;
    {'label': text} draws a labelled separator row.  `project` is the
    vir-value project directory for type-formatted signals (see
    is_type_format).
    """
    signals = []
    ni = 0
    for kind, val in items:
        if kind == "space":
            signals.append({"space": True})
            continue
        if kind == "label":
            signals.append({"label": val})
            continue
        name, fmt = names[ni]
        ni += 1
        clk = fmt == CLK_FORMAT
        type_fmt = is_type_format(fmt)
        # stable sort by tick only: preserves $dumpvars order for ties
        chs = sorted(((t // tick, v) for t, v in changes[name]),
                     key=lambda p: p[0])
        # A signal may be declared but never change (no $dumpvars entry);
        # it renders as an unknown 'x' box for the whole trace.
        steps = [(0, chs[0][1])] + chs if chs else []
        vals = []
        cur = None
        idx = 0
        for k in range(end_time):
            while idx < len(steps) and steps[idx][0] <= k:
                cur = steps[idx][1]
                idx += 1
            vals.append(cur)
        # `clk` only makes sense on a 1-bit signal; anything wider falls
        # back to the default value format (with a warning).
        if clk and any(v is not None and len(v) > 1 for v in vals):
            print(f"warning: {name}:clk on a multi-bit signal; rendering as {DEFAULT_FORMAT}", file=sys.stderr)
            clk = False

        # build wave string: char at tick k is value starting at tick k
        wave = ""
        for k in range(end_time):
            v = vals[k]
            if v is None:
                wave += "x"
            elif k > 0 and vals[k] == vals[k - 1]:
                wave += "."
            elif len(v) > 1 and set(v) - {"0", "1"}:
                # Partially unknown vector (e.g. '01xz'): draw an unknown box.
                wave += "x"
            elif clk and k > 0 and v == "1" and vals[k - 1] == "0":
                # Posedge: the renderer draws an up arrow here.
                wave += "P"
            else:
                # A type-formatted signal always draws data boxes, even at
                # width 1: the point is the value text (@Invalid), which a
                # bare 0/1 level line cannot carry.
                wave += "4" if (len(v) > 1 or type_fmt) else v
        sig = {"name": name, "wave": wave}
        if type_fmt:
            sig["stretch"] = True
            sig["data"] = [decode_value(fmt, v, project)
                           for v, w in zip(vals, wave) if w == "4"]
        elif any(v is not None and len(v) > 1 for v in vals):
            fmt = fmt or DEFAULT_FORMAT
            sig["data"] = [FORMATTERS[fmt](v) for v, w in zip(vals, wave) if w == "4"]
        elif not clk:
            # 1-bit non-clock signals keep their plain 0/1 level lines --
            # the waveform look stays -- but the renderer draws a small
            # "true"/"false" label at every change (each run's start).
            # clk signals get no labels: their edges are timing markers
            # rather than values.  Runs of 'z' or unknown values render
            # as their own shapes and get no label.
            sig["bitlabels"] = True
        signals.append(sig)
    return {"signal": signals, "config": {"hscale": 1}}

# Upper bound on rendered ticks when deriving the tick from the trace: a
# pathological trace (one stray off-grid timestamp collapses the GCD)
# must not explode the render.  The derived tick is raised to the
# smallest multiple of the GCD that stays within this budget, so every
# change time remains exactly on-grid -- quantization never distorts.
MAX_TICKS = 2000


def derive_tick(changes, max_t):
    """Pick the ps-per-tick for a trace: the GCD of all change
    timestamps.

    Every change time is a multiple of the GCD, so quantization is exact
    -- the wave shows changes exactly where they happened, including
    sub-cycle offsets (a 50,000 ps propagation delay renders as its own
    tick).  If the GCD is finer than the render can afford (more than
    MAX_TICKS ticks over the trace's span), it is raised to the smallest
    multiple of itself that fits -- still a divisor of every change time,
    so nothing is misrepresented, only drawn coarser.
    """
    times = {t for lst in changes.values() for t, _v in lst}
    if not times:
        return 500000  # no changes: nothing to resolve; the old default
    g = 0
    for t in sorted(times):
        g = math.gcd(g, t)
    if g == 0:
        return 500000
    span = max(max_t, 1)
    steps = -(-span // g)  # ceil(span / g)
    if steps > MAX_TICKS:
        g *= -(-steps // MAX_TICKS)
    return g


def main():
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("vcd")
    ap.add_argument("out")
    ap.add_argument("-s", "--signals", default="",
                    help="comma-separated signal specs (full path or leaf name, "
                         "optionally ':format'; e.g. top.count:dec); "
                         "default: all signals")
    ap.add_argument("-l", "--layout", default=None,
                    help="path to a layout file, one item per line: "
                         "'signal <name>[:<format>]'; 'space'; or 'label <text>' "
                         "(see the module docstring); overrides --signals")
    ap.add_argument("--tick", type=int, default=None,
                    help="picoseconds per WaveDrom tick "
                         "(default: derived from the trace -- the GCD of "
                         "all change timestamps, raised if needed to keep "
                         "the render bounded)")
    ap.add_argument("--project", default=None,
                    help="project directory (holding a virdant.toml) for "
                         "vir-value type decoding (default: the nearest one "
                         "at or above the VCD file, else builtin-only)")
    args = ap.parse_args()

    if args.layout:
        items = parse_layout(open(args.layout).read())
        wanted = [spec for kind, spec in items if kind == "signal"]
        if not wanted:
            sys.exit("error: layout has no 'signal' lines")
    else:
        wanted = [w.strip() for w in args.signals.split(",") if w.strip()]
        items = [("signal", w) for w in wanted]
    # A spec format that is none of the known formats is a Virdant type
    # name, decoded by vir-value (is_type_format).  Typos in real formats
    # surface there as "unknown type" build errors, so no up-front check
    # against the format table is needed (or possible: types are open-ended).

    changes, order, max_t = parse_vcd(args.vcd)
    names = filter_signals(order, ",".join(wanted))
    if not names:
        sys.exit("error: no signals selected")
    project = args.project
    if project is None and any(is_type_format(split_spec(w)[1]) for w in wanted):
        project = find_project(args.vcd)
    tick = args.tick if args.tick is not None else derive_tick(changes, max_t)
    # The timeline runs to the VCD's last timestamp, change or not (a
    # bare `#t` marker records pure time advancement, e.g. poke's
    # `advance`).  Only when a change lands exactly on that timestamp
    # does it need one extra tick to be visible; otherwise the trace's
    # duration is exactly max_t // tick ticks.
    changed_at_end = any(t == max_t for n, _ in names for t, _ in changes[n])
    end = max(max_t // tick + (1 if changed_at_end else 0), 1)
    json.dump(to_wavedrom(changes, items, names, end, tick, project),
              open(args.out, "w"), indent=1)
    print(f"wrote {args.out}: {len(names)} signals, {end} ticks "
          f"({tick} ps/tick)")

if __name__ == "__main__":
    main()
