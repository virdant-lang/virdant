#!/usr/bin/env python3
"""Generate a parameterizable non-exiting Virdant bench design.

Stages of Word[32] registers chained with xor + add, so per-cycle work
scales linearly with --stages.  Every stage increments itself (stage 0
by one, stage i by one plus the previous stage), so every register
changes on every cycle and no stage ever converges.  The earlier
xor-rotate chain looked linear but converged: `x ^ rotr(x, 7)` maps
period-7 patterns to zero, so downstream stages froze at the constant
7 and stopped exercising driver evaluation. The design never asserts `exit`, so
`vir run --cycles N` measures sustained throughput over exactly N cycles.
"""
import argparse
import pathlib

TEMPLATE = """\
//! Generated benchmark: {stages}-stage register chain, never exits.

pub mod Top for virt::Virt {{
    incoming clock : Clock

    outgoing tx : Bit

{regs}
    tx := r0[0]
}}
"""


def stage(i: int) -> str:
    prev = f"r{i - 1}" if i > 0 else None
    if i == 0:
        return """\
    reg r0 : Word[32] on clock {
        it <= it + 1w32
    }
"""
    return f"""\
    reg r{i} : Word[32] on clock {{
        it <= it + {prev} + 1w32
    }}
"""


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--stages", type=int, default=8)
    ap.add_argument("--out", type=pathlib.Path, required=True)
    args = ap.parse_args()
    regs = "\n".join(stage(i) for i in range(args.stages))
    src = TEMPLATE.format(stages=args.stages, regs=regs)
    src_dir = args.out / "src"
    src_dir.mkdir(parents=True, exist_ok=True)
    (src_dir / "top.vir").write_text(src)
    (args.out / "virdant.toml").write_text('[project]\nname = "bench"\n\n[prog]\ntop = "top::Top"\n')


if __name__ == "__main__":
    main()
