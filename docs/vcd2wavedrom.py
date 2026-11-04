#!/usr/bin/env python3
"""Convert a VCD file to WaveJSON for WaveDrom rendering.

usage: vcd2wavedrom.py <in.vcd> <out.json> [-s name1,name2,...] [--tick PS]

Signal names may be given as full paths ("top.clock") or leaf names ("clock",
which matches every scope containing a signal of that name). Signals are
always rendered in VCD declaration order regardless of the filter order.
"""
import argparse
import json
import sys

def parse_vcd(path):
    scope = []
    idmap = {}   # id char -> full name
    order = []   # full names in declaration order
    changes = {} # full name -> [(time, value)]
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
        elif line.startswith("b"):
            val, ident = line[1:].split()
            changes[idmap[ident]].append((t, val))
        elif line[0] in "01xz":
            changes[idmap[line[1]]].append((t, line[0]))
        elif line.startswith("$dumpall") or line.startswith("$end"):
            continue
    return changes, order

def filter_signals(order, wanted):
    """Return names in declaration order matching any entry of `wanted`."""
    if not wanted:
        return order
    wanted = [w.strip() for w in wanted.split(",") if w.strip()]
    matched = []
    for name in order:
        if name in wanted or name.rsplit(".", 1)[-1] in wanted:
            matched.append(name)
    missing = [w for w in wanted
               if w not in order and w not in {n.rsplit(".", 1)[-1] for n in order}]
    if missing:
        print(f"warning: no match for: {', '.join(missing)}", file=sys.stderr)
    return matched

def to_wavedrom(changes, names, end_time, tick, title):
    signals = []
    for name in names:
        # stable sort by tick only: preserves $dumpvars order for ties
        chs = sorted(((t // tick, v) for t, v in changes[name]),
                     key=lambda p: p[0])
        steps = [(0, chs[0][1])] + chs  # force value at t=0
        vals = []
        cur = None
        idx = 0
        for k in range(end_time):
            while idx < len(steps) and steps[idx][0] <= k:
                cur = steps[idx][1]
                idx += 1
            vals.append(cur)
        # build wave string: char at tick k is value starting at tick k
        wave = ""
        for k in range(end_time):
            v = vals[k]
            if k > 0 and vals[k] == vals[k - 1]:
                wave += "."
            else:
                wave += "4" if len(v) > 1 else v
        sig = {"name": name, "wave": wave}
        if any(len(v) > 1 for v in vals):
            sig["data"] = [hex(int(v, 2)) for v, w in zip(vals, wave) if w == "4"]
        signals.append(sig)
    return {"signal": signals, "config": {"hscale": 1},
            "head": {"text": title}}

def main():
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("vcd")
    ap.add_argument("out")
    ap.add_argument("-s", "--signals", default="",
                    help="comma-separated signal names (full path or leaf); "
                         "default: all signals")
    ap.add_argument("--tick", type=int, default=500000,
                    help="picoseconds per WaveDrom tick (default 500000)")
    args = ap.parse_args()

    changes, order = parse_vcd(args.vcd)
    names = filter_signals(order, args.signals)
    if not names:
        sys.exit("error: no signals selected")
    end = max((t for n in names for t, _ in changes[n]), default=0) // args.tick + 1
    title = f"{args.vcd}  (1 tick = {args.tick // 1000} ns)"
    json.dump(to_wavedrom(changes, names, end, args.tick, title),
              open(args.out, "w"), indent=1)
    print(f"wrote {args.out}: {len(names)} signals, {end} ticks")

if __name__ == "__main__":
    main()
