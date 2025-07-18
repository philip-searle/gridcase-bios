
KBXLAT		PROGRAM	OutFile=kbxlat.obj

		include	"macros.inc"
		include	"segments.inc"

		PUBLIC	KbXlat
		PUBLIC	KbXlatShift, KbXlatCtrl, KbXlatAlt
		PUBLIC	KbXlatExtended

; ---------------------------------------------------------------------
; KbXlat
; Scancode set 2 to ASCII translation table.
; 0FFh for no valid translation.
; ---------------------------------------------------------------------
KbXlat		db	01Bh, '1234567890-=', 08h
		db	09h, 'qwertyuiop[]', 0Dh
		db	0FFh, 'asdfghjkl;', 027h, '`'
		db	0FFh, '\zxcvbnm,./'
		db	0FFh, '*', 0FFh, ' ', 0FFh, ';<=>?@'
		db	'ABCD', 0FFh, 0FFh, 0, 0, 0, '-', 0, 0F0h, 0
		db	'+', 0, 0, 0, 0, 0, 0FFh, 0FFh, '\', 085h, 086h

; ---------------------------------------------------------------------
; KbXlatShift
; As KbXlat but with the shift state active.
; ---------------------------------------------------------------------
KbXlatShift	db	01Bh, '!@#$%%^&*()_+', 08h
		db	0, 'QWERTYUIOP{}', 0Dh
		db	0FFh, 'ASDFGHJKL:"~'
		db	0FFh, '|ZXCVBNM<>?'
		db	0FFh, 0FFh, 0FFh, ' ', 0FFh, 'TUVWXY'
		db	'Z[\]', 0FFh, 0FFh, '789-456+1230.'
		db	0FFh, 0FFh, '|', 087h, 088h

; ---------------------------------------------------------------------
; KbXlatCtrl
; As KbXlat but with the ctrl key held down.
; ---------------------------------------------------------------------
KbXlatCtrl	db	01Bh, 0FFh, 0, 0FFh, 0FFh, 0FFh, 01Eh, 0FFh, 0FFh, 0FFh, 0FFh, 01Fh, 0FFh, 07Fh
		db	94h, 11h, 17h, 05h, 12h, 14h, 19h, 15h, 09h, 0Fh, 10h, 1Bh, 1Dh, 0Ah
		db	0FFh, 01h, 13h, 04h, 06h, 07h, 08h, 0Ah, 0Bh, 0Ch, 0FFh, 0FFh
		db	0FFh, 0FFh, 1Ch, 1Ah, 18h, 03h, 16h, 02h, 0Eh, 0Dh, 0FFh, 0FFh, 0FFh
		db	0FFh, 72h, 0FFh, 20h, 0FFh, 5Eh, 5Fh, 60h, 61h, 62h, 63h
		db	64h, 65h, 66h, 67h, 0FFh, 0FFh, 77h, 8Dh, 84h, 8Eh, 73h, 8Fh, 74h, 90h, 75h, 91h, 76h, 92h, 93h
		db	0FFh, 0FFh, 7Ch, 89h, 8Ah

; ---------------------------------------------------------------------
; KbXlatAlt
; As KbXlat but with the alt key held down.
; ---------------------------------------------------------------------
KbXlatAlt	db	0F0h, 78h, 79h, 7Ah, 7Bh, 7Ch, 7Dh, 7Eh, 7Fh, 80h, 81h, 82h, 83h, 0F0h
		db	0A5h, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0F0h, 0F0h, 0F0h
		db	0FFh, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0F0h, 0F0h
		db	0F0h, 0FFh, 0F0h, 0, 0, 0, 0, 0, 0, 0, 0F0h, 0F0h
		db	0F0h, 0FFh, 0F0h, 0FFh, 20h, 0FFh, 68h, 69h, 6Ah, 6Bh, 6Ch
		db	6Dh, 6Eh, 6Fh, 70h, 71h, 0FFh, 0FFh, 07h, 08h, 09h, 0F0h, 04h
		db	05h, 06h, 0F0h, 01h, 02h, 03h, 0, 0FFh, 0FFh, 0FFh, 7Ch, 8Bh, 8Ch

; ---------------------------------------------------------------------
; KbXlatExtended
; As KbXlat but for the extended keys only.
; ---------------------------------------------------------------------
KbXlatExtended	db	097h, 098h, 099h, 0FFh, 09Bh, 0FFh, 09Dh, 0FFh
		db	09Fh, 0A0h, 0A1h, 0A2h, 0A3h

ENDPROGRAM	KBXLAT
