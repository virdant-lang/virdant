#!/bin/bash

set -e

TMPDIR_WORK="$(mktemp -d "$1"/new_bitstream.XXXXXX)"
cd "$TMPDIR_WORK"

mkdir -p blink/src
cd blink

cat > virdant.toml <<'EOF'
[project]
name = "blink"

[prog]
top = "top::Top"
EOF

cat > src/top.vir <<'EOF'
import ice40

pub mod Top for ice40::IceStick {
    incoming clock : Clock
    outgoing led1 : Bit
    outgoing led2 : Bit
    outgoing led3 : Bit
    outgoing led4 : Bit
    outgoing led5 : Bit
    incoming uart_rx : Bit
    outgoing uart_tx : Bit
    outgoing flash_cs   : Bit
    outgoing flash_clk  : Bit
    outgoing flash_mosi : Bit
    incoming flash_miso : Bit

    reg counter : Word[24] on clock {
        it <= it + 1w24
    }

    led1 := counter[23]
    led2 := false
    led3 := false
    led4 := false
    led5 := false
    uart_tx := true
    flash_cs := true
    flash_clk := false
    flash_mosi := false

    unused uart_rx
    unused flash_miso
}
EOF

vir bitstream
vir pnr --tries 2 --seed 3
test -f build/blink.bin
