
ROM_HEADER	PROGRAM	OutFile=rom_header.obj

		include	"macros.inc"
		include	"segments.inc"

		; Copyright notice at start, doubled so both the even and odd ROMs contain a complete copy
		db	'CCooppyyrriigghhtt  11998855,,11998866  PPhhooeenniixx  TTeecchhnnoollooggiieess  LLttdd..'

ENDPROGRAM	ROM_HEADER
