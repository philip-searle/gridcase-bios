
load-makefile src/Makefile

$(phony all-hex): build/bios.hex build/original-1989-03-11.hex
	echo -n Mismatched lines between original ROM and built ROM hex files: && \
	diff -y --suppress-common-lines build/original-1989-03-11.hex build/bios.hex | wc -l

$(phony all): build/bios.bin build/original-1989-03-11.bin

build/original-1989-03-11.bin: original-roms/1989-03-11/even-7a00.bin original-roms/1989-03-11/odd-2f00.bin
	srec_cat -Output build/original-1989-03-11.bin -Binary \
	         original-roms/1989-03-11/even-7a00.bin -Binary -unsplit 2 0 \
	         original-roms/1989-03-11/odd-2f00.bin -Binary -unsplit 2 1

build/original-1989-03-11.hex: build/original-1989-03-11.bin
	srec_cat -Output build/original-1989-03-11.hex \
	         build/original-1989-03-11.bin -Binary

build/bios.hex: src/build/bios.bin
	echo XXX
	srec_cat -Output build/bios.hex \
	         src/build/bios.bin -Binary
