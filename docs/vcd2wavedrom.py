#!/usr/bin/env python3
"""Convert a VCD file to WaveJSON for WaveDrom rendering.

usage: vcd2wavedrom.py <in.vcd> <out.json> [-s name1,name2,...] [--tick PS]

Signal names may be given as full paths ("top.clock") or leaf names ("clock",
which matches every scope containing a signal of that name). Signals are
rendered in the order given on the command line (or VCD declaration order
when no filter is given).
"""
import argparse
import json
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

def filter_signals(order, wanted):
    """Return names matching `wanted`, in the order given in `wanted`.

    Each entry of `wanted` may be a full path ("top.clock") or a leaf name
    ("clock", matching every scope containing a signal of that name).  A
    leaf entry matches in VCD declaration order among its matches.
    """
    if not wanted:
        return order
    wanted = [w.strip() for w in wanted.split(",") if w.strip()]
    matched = []
    for w in wanted:
        for name in order:
            if (name == w or name.rsplit(".", 1)[-1] == w) and name not in matched:
                matched.append(name)
    missing = [w for w in wanted
               if w not in order and w not in {n.rsplit(".", 1)[-1] for n in order}]
    if missing:
        print(f"warning: no match for: {', '.join(missing)}", file=sys.stderr)
    return matched

def to_wavedrom(changes, names, end_time, tick):
    signals = []
    for name in names:
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
        # build wave string: char at tick k is value starting at tick k
        wave = ""
        for k in range(end_time):
            v = vals[k]
            if v is None:
                wave += "x"
            elif k > 0 and vals[k] == vals[k - 1]:
                wave += "."
            else:
                wave += "4" if len(v) > 1 else v
        sig = {"name": name, "wave": wave}
        if any(v is not None and len(v) > 1 for v in vals):
            sig["data"] = [hex(int(v, 2)) for v, w in zip(vals, wave) if w == "4"]
        signals.append(sig)
    return {"signal": signals, "config": {"hscale": 1}}

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

    changes, order, max_t = parse_vcd(args.vcd)
    names = filter_signals(order, args.signals)
    if not names:
        sys.exit("error: no signals selected")
    # The timeline runs to the VCD's last timestamp, change or not (a
    # bare `#t` marker records pure time advancement, e.g. poke's
    # `advance`).  Only when a change lands exactly on that timestamp
    # does it need one extra tick to be visible; otherwise the trace's
    # duration is exactly max_t // tick ticks.
    changed_at_end = any(t == max_t for n in names for t, _ in changes[n])
    end = max(max_t // args.tick + (1 if changed_at_end else 0), 1)
    json.dump(to_wavedrom(changes, names, end, args.tick),
              open(args.out, "w"), indent=1)
    print(f"wrote {args.out}: {len(names)} signals, {end} ticks")

if __name__ == "__main__":
    main()
