
all-hex: build/bios.hex build/original-1989-03-11.hex
	echo -n Mismatched lines between original ROM and built ROM hex files: && \
	diff -y --suppress-common-lines build/original-1989-03-11.hex build/bios.hex | wc -l

all: build/bios.bin build/original-1989-03-11.bin

build/original-1989-03-11.bin: original-roms/1989-03-11/even-7a00.bin original-roms/1989-03-11/odd-2f00.bin
	srec_cat -Output build/original-1989-03-11.bin -Binary \
	         original-roms/1989-03-11/even-7a00.bin -Binary -unsplit 2 0 \
	         original-roms/1989-03-11/odd-2f00.bin -Binary -unsplit 2 1

build/original-1989-03-11.hex: build/original-1989-03-11.bin
	srec_cat -Output build/original-1989-03-11.hex \
	         build/original-1989-03-11.bin -Binary

build/bios.bin: src/*.asm src/*.inc src/post/*.asm src/floppy/*.asm
	#nasm -f bin -o build/bios.tmp -l build/bios.lst src/bios.asm && \
	./euroasm.exe src/bios.asm && \
	srec_cat build/bios.tmp -Binary \
	         -crop 0x8000 0x10000 -offset -0x8000 \
	         -Output build/bios.bin -Binary

build/bios.hex: build/bios.bin
	srec_cat -Output build/bios.hex \
	         build/bios.bin -Binary

