firmware=main
.PHONY: firmware

firmware:
	mkdir -p build/
	riscv64-unknown-elf-gcc -mabi=ilp32 -march=rv32i firmware/$(firmware).s -nostdlib -o build/$(firmware)
	riscv64-unknown-elf-objcopy -O binary --only-section=.text build/$(firmware) build/$(firmware).bin
	icesprog -o 0x200000 build/$(firmware).bin
