
OBJECTS	:=	build/rom_header.obj \
			build/reset.obj \
			build/reset_utils.obj \
			build/post/vidtest.obj \
			build/post/kbc.obj \
			build/post/timer.obj \
			build/post/protmode.obj \
			build/int19.obj \
			build/int18.obj \
			build/int13_at.obj \
			build/int13_grid.obj \
			build/int14.obj \
			build/int16.obj \
			build/int9.obj \
			build/kbxlat.obj \
			build/keyboard.obj \
			build/kbc_286.obj \
			build/int13_fd.obj \
			build/fd_int13_handlers.obj \
			build/fd_std_dpbs.obj \
			build/fd_bda.obj \
			build/fd_commands.obj \
			build/fd_controller.obj \
			build/fd_misc.obj \
			build/int17.obj \
			build/int10.obj \
			build/int12.obj \
			build/int11.obj \
			build/int15.obj \
			build/rtc.obj \
			build/misc.obj \
			build/kb_extra.obj \
			build/int75.obj \
			build/int71.obj \
			build/sysident.obj \
			build/at_compat.obj \
			build/rom_trailer.obj

build/%.obj: %.asm *.inc
	../euroasm.exe $(input)

build/post/%.obj: post/%.asm
	pwd && \
	../euroasm.exe $(input)

build/bios.bin: $(OBJECTS) ../src-glink/glink.lnk ../src-glink/test.pl
	cd ../src-glink && \
	carton exec -- ./test.pl
