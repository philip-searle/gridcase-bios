
include euroasm_scanner.mk

&mkdir -p post

OBJECTS	:=	rom_header.obj \
			reset.obj \
			reset_utils.obj \
			post/vidtest.obj \
			post/kbc.obj \
			post/timer.obj \
			post/protmode.obj \
			int19.obj \
			int18.obj \
			int13_at.obj \
			int13_grid.obj \
			int14.obj \
			int16.obj \
			int9.obj \
			kbxlat.obj \
			keyboard.obj \
			kbc_286.obj \
			int13_fd.obj \
			fd_int13_handlers.obj \
			fd_std_dpbs.obj \
			fd_bda.obj \
			fd_commands.obj \
			fd_controller.obj \
			fd_misc.obj \
			int17.obj \
			int10.obj \
			int12.obj \
			int11.obj \
			int15.obj \
			rtc.obj \
			misc.obj \
			kb_extra.obj \
			int75.obj \
			int71.obj \
			isr.obj \
			lightpen.obj \
			cmos.obj  \
			prntscrn.obj \
			int13_xt.obj \
			grid_cpu.obj \
			grid_autodetect.obj \
			grid_fd.obj \
			grid_vid.obj \
			int15_grid.obj \
			grid_vid2.obj \
			sysident.obj \
			at_compat.obj \
			grid_pw.obj \
			grid_misc.obj \
			rom_trailer.obj

%.obj %.asm.lst: %.asm euroasm.ini
	$(EUROASM) $(input)

bios.bin bios.map bios.hex: $(OBJECTS) glink.lnk
	$(GLINK) glink.lnk
	srec_cat -Output bios.hex bios.bin -Binary
