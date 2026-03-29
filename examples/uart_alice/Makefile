DEVICE="/dev/ttyACM0"
all:
	icesprog -o 0x100000 alice.txt
	cargo run -- upload
	picocom $(DEVICE) --imap lfcrlf

clean:
	rm -rf build
