TOP=top
SOURCE_DIR=src
PCF=icesugar.pcf

set -ex

rm -rf build/
vir compile "$SOURCE_DIR/$TOP.vir"
echo "read_verilog -I .. build/*.v; synth_ice40 -top Top -json build/$TOP.json" > script.ys
yosys script.ys

nextpnr-ice40 --up5k --json build/"$TOP.json" --pcf $PCF --asc "build/$TOP.asc" --package sg48
icepack -s "build/$TOP.asc" "build/$TOP.bin"
#icesprog -o 0x100000 alice.txt
icesprog "build/$TOP.bin"
