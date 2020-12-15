
build/original-1989-03-11.bin: original-roms/1989-03-11/even-7a00.bin original-roms/1989-03-11/odd-2f00.bin
	srec_cat -Output build/original-rom-1989-03-11.bin -Binary \
	         original-roms/1989-03-11/even-7a00.bin -Binary -unsplit 2 0 \
	         original-roms/1989-03-11/odd-2f00.bin -Binary -unsplit 2 1

