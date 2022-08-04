
; ---------------------------------------------------------------------------
; DMA controller ports [TechRef 4-1]

; DMA page registers
PORT_DMA_PAGE0		equ	87h
PORT_DMA_PAGE1		equ	83h
PORT_DMA_PAGE2		equ	81h
PORT_DMA_PAGE3		equ	82h
; PORT_DMA_PAGE4 used for cascade feature
PORT_DMA_PAGE5		equ	8Bh
PORT_DMA_PAGE6		equ	89h
PORT_DMA_PAGE7		equ	8Ah
PORT_DMA_RAM_REFRESH_ADDRESS	equ	8Fh

; DMA channel 0-3 control registers
PORT_DMA1_STATUS	equ	08h	; (R-)
PORT_DMA1_COMMAND	equ	08h	; (-W)
PORT_DMA1_WRITE_REQUEST	equ	09h	; (-W)
PORT_DMA1_MASK		equ	0Ah	; (RW)
PORT_DMA1_MODE		equ	0Bh	; (-W)
PORT_DMA1_CLEAR_FF	equ	0Ch	; (-W)
PORT_DMA1_MASTER_CLEAR	equ	0Dh	; (-W)

; DMA channel 4-7 control registers
PORT_DMA2_STATUS	equ	0D0h	; (R-)
PORT_DMA2_COMMAND	equ	0D0h	; (-W)
PORT_DMA2_WRITE_REQUEST	equ	0D2h	; (-W)
PORT_DMA2_MASK		equ	0D4h	; (RW)
PORT_DMA2_MODE		equ	0D6h	; (-W)
PORT_DMA2_CLEAR_FF	equ	0D8h	; (-W)
PORT_DMA2_MASTER_CLEAR	equ	0DAh	; (-W)

; DMA controller 1 channels (byte transfers)
PORT_DMA_CHAN0_CUR	equ	00h	; (R-) current address
PORT_DMA_CHAN0_BASE	equ	00h	; (-W) base address
PORT_DMA_CHAN0_WC	equ	01h	; (RW) byte count
PORT_DMA_CHAN1_CUR	equ	02h	; (R-) current address
PORT_DMA_CHAN1_BASE	equ	02h	; (-W) base address
PORT_DMA_CHAN1_WC	equ	03h	; (RW) byte count
PORT_DMA_CHAN2_CUR	equ	04h	; (R-) current address
PORT_DMA_CHAN2_BASE	equ	04h	; (-W) base address
PORT_DMA_CHAN2_WC	equ	05h	; (RW) byte count
PORT_DMA_CHAN3_CUR	equ	06h	; (R-) current address
PORT_DMA_CHAN3_BASE	equ	06h	; (-W) base address
PORT_DMA_CHAN3_WC	equ	07h	; (RW) byte count

; DMA controller 2 channels (word transfers)
PORT_DMA_CHAN4_CUR	equ	0C0h	; (R-) current address
PORT_DMA_CHAN4_BASE	equ	0C0h	; (-W) base address
PORT_DMA_CHAN4_WC	equ	0C2h	; (RW) word count
PORT_DMA_CHAN5_CUR	equ	0C4h	; (R-) current address
PORT_DMA_CHAN5_BASE	equ	0C4h	; (-W) base address
PORT_DMA_CHAN5_WC	equ	0C6h	; (RW) word count
PORT_DMA_CHAN6_CUR	equ	0C8h	; (R-) current address
PORT_DMA_CHAN6_BASE	equ	0C8h	; (-W) base address
PORT_DMA_CHAN6_WC	equ	0CAh	; (RW) word count
PORT_DMA_CHAN7_CUR	equ	0CCh	; (R-) current address
PORT_DMA_CHAN7_BASE	equ	0CCh	; (-W) base address
PORT_DMA_CHAN7_WC	equ	0CEh	; (RW) word count

