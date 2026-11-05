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
"""
import argparse
import json
import math
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


def filter_signals(order, wanted):
    """Return (name, format) pairs matching `wanted`, in the order given.

    Each entry of `wanted` is a signal spec: a full path ("top.clock") or a
    leaf name ("clock", matching every scope containing a signal of that
    name), optionally followed by a format ("top.count:dec").  A leaf entry
    matches in VCD declaration order among its matches.
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
        print(f"warning: no match for: {', '.join(missing)}", file=sys.stderr)
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


def to_wavedrom(changes, names, end_time, tick):
    signals = []
    for name, fmt in names:
        clk = fmt == CLK_FORMAT
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
                wave += "4" if len(v) > 1 else v
        sig = {"name": name, "wave": wave}
        if any(v is not None and len(v) > 1 for v in vals):
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
    ap.add_argument("--tick", type=int, default=None,
                    help="picoseconds per WaveDrom tick "
                         "(default: derived from the trace -- the GCD of "
                         "all change timestamps, raised if needed to keep "
                         "the render bounded)")
    args = ap.parse_args()

    wanted = [w.strip() for w in args.signals.split(",") if w.strip()]
    bad = sorted({split_spec(w)[1] for w in wanted} - set(FORMATTERS) - {CLK_FORMAT} - {None})
    if bad:
        sys.exit("error: unknown format(s): {} (known: {})".format(
            ", ".join(bad), ", ".join(sorted(FORMATTERS))))

    changes, order, max_t = parse_vcd(args.vcd)
    names = filter_signals(order, args.signals)
    if not names:
        sys.exit("error: no signals selected")
    tick = args.tick if args.tick is not None else derive_tick(changes, max_t)
    # The timeline runs to the VCD's last timestamp, change or not (a
    # bare `#t` marker records pure time advancement, e.g. poke's
    # `advance`).  Only when a change lands exactly on that timestamp
    # does it need one extra tick to be visible; otherwise the trace's
    # duration is exactly max_t // tick ticks.
    changed_at_end = any(t == max_t for n, _ in names for t, _ in changes[n])
    end = max(max_t // tick + (1 if changed_at_end else 0), 1)
    json.dump(to_wavedrom(changes, names, end, tick),
              open(args.out, "w"), indent=1)
    print(f"wrote {args.out}: {len(names)} signals, {end} ticks "
          f"({tick} ps/tick)")

if __name__ == "__main__":
    main()
