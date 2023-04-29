; vim: set ft=asm6502-2 ts=8:

.feature labels_without_colons
.feature string_escapes
.macpack longbranch

;----------------------------------------------------------------------
;			cc65 includes
;----------------------------------------------------------------------
.include "telestrat.inc"
.include "fcntl.inc"

;----------------------------------------------------------------------
;			Orix SDK includes
;----------------------------------------------------------------------
.include "SDK.mac"
.include "SDK.inc"
.include "ch376.inc"
.include "types.mac"
.include "errors.inc"

;----------------------------------------------------------------------
;				Imports
;----------------------------------------------------------------------

; From debug
.import PrintHexByte

; From sopt
.import spar1, sopt1, calposp, incr
.import loupch1
.importzp cbp
.import inbuf
spar := spar1
sopt := sopt1

; From ermes
.import ermes

; From ermtb
.import ermtb

; From main
.import __BSS_LOAD__, __BSS_SIZE__, __RAMEND__

;----------------------------------------------------------------------
;				Exports
;----------------------------------------------------------------------
.export _main

; Pour ermes
.export crlf1, out1
.export prfild, prnamd
.export seter1

.exportzp xtrk, psec
.export drive

;----------------------------------------------------------------------
;			Librairies
;----------------------------------------------------------------------

;----------------------------------------------------------------------
; Defines / Constants
;----------------------------------------------------------------------
VERSION = $202320

KERNEL_MAX_PATH_LENGTH = 49

LSB  := crc+3
NLSB := crc+2
NMSB := crc+1
MSB  := crc+0

max_path := KERNEL_MAX_PATH_LENGTH

;----------------------------------------------------------------------
;				Page Zéro
;----------------------------------------------------------------------
.zeropage
	unsigned char crclo          ; current value of CRC
	unsigned char crchi          ; not necessarily contiguous

	unsigned long crc

	unsigned char xtrk
	unsigned char psec

;----------------------------------------------------------------------
;				Variables
;----------------------------------------------------------------------
.segment "DATA"
	char str[11]
	unsigned short fp
	unsigned char fname[max_path]
	unsigned long fsize

	unsigned char xio

	unsigned char drive

	unsigned char extractbuf
	unsigned short nbpagemax

	unsigned short bufptr

;----------------------------------------------------------------------
; Variables et buffers
;----------------------------------------------------------------------
.segment "CODE"
	unsigned char crct0[256]
	unsigned char crct1[256]
	unsigned char crct2[256]
	unsigned char crct3[256]

;----------------------------------------------------------------------
;			Segments vides
;----------------------------------------------------------------------
.segment "STARTUP"
.segment "INIT"
.segment "ONCE"

;----------------------------------------------------------------------
;				Programme
;----------------------------------------------------------------------
.segment "CODE"

.proc _main
		ldy	#>( (__BSS_LOAD__ + __BSS_SIZE__) & $ff00)
		lda	#<( (__BSS_LOAD__ + __BSS_SIZE__) )
		beq	skip
		iny
	skip:
		sty	extractbuf

		; Calcul de la taille du tampon pour l'extraction
		sec
		lda	#>__RAMEND__
		sbc	extractbuf
		sta	nbpagemax+1
		lda	#$00
		sta	nbpagemax

		; Initialisation de la table
		; test cksum
		jsr	makecksumtable

		ldy	#<BUFEDT
		lda	#>BUFEDT
		sty	cbp
		sta	cbp+1

		ldy	#$ff
	loop:
		iny
		lda	(cbp),y
		clc
		beq	eol

		cmp	#' '
		bne	loop

	eol:
		; Ici si on a trouvé un ' ' => C=1
		tya
		ldy	cbp+1
		adc	cbp
		sta	cbp
		bcc	loop_end

		iny
	loop_end:
		tya
		ldy	cbp

	getopt:
		jsr	sopt
		.asciiz "HV"
		bcs	error

		cpx	#$40
		bcc	cmnd_exec

		bne	_help

		jmp	cmnd_version
	_help:
		jmp	cmnd_help

	cmnd_exec:
		ldy	#$00
		lda	(cbp),y
		cmp	#'@'
		bne	go

		; Mode batch activé
		jsr	openbatch
		bcc	getopt

		bcs	error

	go:
		jsr	getfname
		bcs	error

	cksum:
		jsr	cmnd_cksum
		bcs	error

		;
		ldy	#$00
	skipcr:
		lda	(cbp),y
		beq	end

		cmp	#$0d
		bne	_calcposp

		iny
		bne	skipcr

		; Si débordement de Y
		beq	errEOF

	_calcposp:
		jsr	calposp
		bne	getopt

	error:
		jsr	ermes
	end:
		crlf
		rts

	errEOF:
		lda	#e4
		sec
		jmp	ermes
.endproc


;----------------------------------------------------------------------
;
; Entrée:
;
; Sortie:
;
; Variables:
;	Modifiées:
;		-
;	Utilisées:
;		-
; Sous-routines:
;	-
;----------------------------------------------------------------------
.proc cmnd_help
		print helpmsg
;		print longhelp_msg
		clc
		rts
.endproc

;----------------------------------------------------------------------
;
; Entrée:
;
; Sortie:
;
; Variables:
;	Modifiées:
;		-
;	Utilisées:
;		-
; Sous-routines:
;	-
;----------------------------------------------------------------------
.proc cmnd_version
		.out .sprintf("cksum v%x.%x.%x", ::VERSION >> 8, (::VERSION & $ff)>>4 , (::VERSION & $0f))

		prints  .sprintf("cksum v%x.%x.%x", ::VERSION >> 8, (::VERSION & $ff)>>4 , (::VERSION & $0f))
		crlf

		clc
		rts
.endproc

;----------------------------------------------------------------------
;
; Entrée:
;	fname: nom du fichier
;
; Sortie:
;	A: Code erreur
;	C: 0->Ok, 1->Erreur
;
; Variables:
;	Modifiées:
;		crc
;		fp
;		fsize
;		buffer
;
;	Utilisées:
;		TR0
;
; Sous-routines:
;	fopen
;	fread
;	fclose
;	crc32
;	compl32
;	dispresult
;----------------------------------------------------------------------
.proc cmnd_cksum
		; Valeur initiale
		lda	#$00
		sta	crc
		sta	crc+1
		sta	crc+2
		sta	crc+3

		; Taille du fichier
		sta	fsize
		sta	fsize+1
		sta	fsize+2
		sta	fsize+3

		fopen fname, O_RDONLY
		sta	fp
		stx	fp+1

		eor	fp+1
		beq	errFopen

	read:
		lda	#$ff
		tay
		jsr	SetByteRead
		bcs	error

	read1:
		lda	#$27
		sta	CH376_COMMAND
		ldy	CH376_DATA
		beq	nextchunk

		; Mise à jour de la taille du fichier (MSB en premier)
		clc
		tya
		adc	fsize+3
		sta	fsize+3
		bcc	loop

		inc	fsize+2
		bne	loop

		inc	fsize+1
		bne	loop

		inc	fsize

	loop:
		lda	CH376_DATA
		jsr	crc32
		dey
		bne	loop

	nextchunk:
		jsr	ByteRdGo
		bcc	read1

		lda	#$ff
		tay
		jsr	SetByteRead
		bcc	read1

	end:
		fclose(fp)

		; Cherche le premier octet non nul de la taille du fichier
		ldx	#$ff
	loop1:
		inx
		lda	fsize,x
		bne	addfsize

		cpx	#$03
		bne	loop1

		; Ajoute crc32(taille du fichier)
		; LSB en premier
	addfsize:
		ldy	#$03
	loop3:
		lda	fsize,y
		jsr	crc32
		dey
		inx
		cpx	#$04
		bne	loop3

	dispcksum:
		jsr	compl32
		jsr	dispresult

		rts

	errFopen:
		lda	#e13
		sec
	error:
		rts

.endproc


;----------------------------------------------------------------------
;
; Entrée:
;
; Sortie:
;
; Variables:
;	Modifiées:
;		-
;	Utilisées:
;		-
; Sous-routines:
;	-
;----------------------------------------------------------------------
.proc dispresult
		; Checksum en Hexa
		;ldy crc+1
		;lda crc
		;jsr printWord
		;ldy crc+3
		;lda crc+2
		;jsr printWord

		;print #'='

		; Checksum en Décimal
		jsr	bin2bcd
		lda	#<str
		ldy	#>str
		jsr	bcd2str
		print	str

		; Taille du fichier
		print	#' '
		lda	fsize
		sta	crc
		lda	fsize+1
		sta	crc+1
		lda	fsize+2
		sta	crc+2
		lda	fsize+3
		sta	crc+3

		jsr	bin2bcd
		lda	#<str
		ldy	#>str
		jsr	bcd2str

		; Saute les 0 non significatifs
		ldx	#$ff
	skip0:
		inx
		lda	str,x
		cmp	#'0'
		bne	_print
		cpx	#$09
		bne	skip0

	_print:
		txa
		clc
		adc	#<str
		pha
		lda	#>str
		adc	#$00
		tay
		pla
		.byte $00, XWSTR0

		; Nom du fichier
		print	#' '
		print	fname

		crlf
		rts
.endproc

;----------------------------------------------------------------------
;
; Entrée:
;
; Sortie:
;
; Variables:
;	Modifiées:
;		-
;	Utilisées:
;		-
; Sous-routines:
;	-
;----------------------------------------------------------------------
.ifdef TEST
	.proc test
			crlf

			; Valeur initiale: $ffff
			lda	#$ff
			sta	crclo
			sta	crchi

			lda	#$a1
			jsr	crc16_f

			lda	#$a1
			jsr	crc16_f

			lda	#$a1
			jsr	crc16_f

			lda	#$fe
			jsr	crc16_f

			lda	#$00
			jsr	crc16_f

			lda	#$00
			jsr	crc16_f

			lda	#$01
			jsr	crc16_f

			lda	#$01
			jsr	crc16_f

			ldy	crclo
			lda	crchi
			jsr	printWord

			crlf

			; --------------------------
			; Initialisation de la table
			jsr	makecrc16table
			lda	#$ff
			sta	crclo
			sta	crchi

			lda	#$a1
			jsr	crc16

			lda	#$a1
			jsr	crc16

			lda	#$a1
			jsr	crc16

			lda	#$fe
			jsr	crc16

			lda	#$00
			jsr	crc16

			lda	#$00
			jsr	crc16

			lda	#$01
			jsr	crc16

			lda	#$01
			jsr	crc16

			ldy	crclo
			lda	crchi
			jsr	printWord

			crlf

			; --------------------------
			; Initialisation de la table
			jsr	makecrc32table

			; Valeur initiale
			lda	#$ff
			sta	crc
			sta	crc+1
			sta	crc+2
			sta	crc+3

			; Calcul du CRC32
		;	lda	#$a1
		;	jsr	crc32

		;	lda	#$a1
		;	jsr	crc32

		;	lda	#$a1
		;	jsr	crc32

		;	lda	#$fe
		;	jsr	crc32

		;	lda	#$00
		;	jsr	crc32

		;	lda	#$00
		;	jsr	crc32

		;	lda	#$01
		;	jsr	crc32

		;	lda	#$01
		;	jsr	crc32

			lda	#'1'
			jsr	crc32
			lda	#'2'
			jsr	crc32
			lda	#'3'
			jsr	crc32


		;	ldy	#$00
		;loop
		;	lda	buffer,y
		;	jsr	crc32
		;	iny
		;	bne	loop

			; Complément du CRC
			jsr	compl32

			; Affiche le crc
			ldy	crc+2
			lda	crc+3
			jsr	printWord
			ldy	crc
			lda	crc+1
			jsr	printWord

			crlf

			; --------------------------
			; Initialisation de la table
			; test cksum
			jsr	makecksumtable

			; Valeur initiale
			lda	#$00
			sta	crc
			sta	crc+1
			sta	crc+2
			sta	crc+3

			; Calcul cksum
			lda	#'1'
			jsr	crc32
			lda	#'2'
			jsr	crc32
			lda	#'3'
			jsr	crc32

			; Ajoute la longueur du message (LSB en premier)
			lda	#$03
			jsr	crc32

			jsr	compl32

			ldy	crc+1
			lda	crc
			jsr	printWord
			ldy	crc+3
			lda	crc+2
			jsr	printWord

			print	#'='

			jsr	bin2bcd
			lda	#<str
			ldy	#>str
			jsr	bcd2str
			print	str
			crlf
			rts
	.endproc
.endif

;----------------------------------------------------------------------
;
; Entrée:
;
; Sortie:
;
; Variables:
;	Modifiées:
;		-
;	Utilisées:
;		-
; Sous-routines:
;	-
;----------------------------------------------------------------------
.ifdef CRC16_F
	.proc crc16_f
			eor	crchi		; A contained the data
			sta	crchi		; XOR it into high byte
			lsr			; right shift A 4 bits
			lsr			; to make top of x^12 term
			lsr			; ($1...)
			lsr
			tax			; save it
			asl			; then make top of x^5 term
			eor	crclo		; and XOR that with low byte
			sta	crclo		; and save
			txa			; restore partial term
			eor	crchi		; and update high byte
			sta	crchi		; and save
			asl			; left shift three
			asl			; the rest of the terms
			asl			; have feedback from x^12
			tax			; save bottom of x^12
			asl			; left shift two more
			asl			; watch the carry flag
			eor	crchi		; bottom of x^5 ($..2.)
			tay			; save high byte
			txa			; fetch temp value
			rol			; bottom of x^12, middle of x^5!
			eor	crclo		; finally update low byte
			sta	crchi		; then swap high and low bytes
			sty	crclo
			rts
	.endproc
.endif

;----------------------------------------------------------------------
;
; Entrée:
;
; Sortie:
;
; Variables:
;	Modifiées:
;		-
;	Utilisées:
;		-
; Sous-routines:
;	-
;----------------------------------------------------------------------
.ifdef CRC16
	.proc crc16
			 eor	crchi		; Quick CRC computation with lookup tables
			 tax
			 lda	crclo
			 eor	crct1,X
			 sta	crchi
			 lda	crct0,X
			 sta	crclo
			 rts
	.endproc
.endif

;----------------------------------------------------------------------
;
; Entrée:
;
; Sortie:
;
; Variables:
;	Modifiées:
;		-
;	Utilisées:
;		-
; Sous-routines:
;	-
;----------------------------------------------------------------------
.proc crc32
		stx	xio

		eor	crc		; Quick CRC computation with lookup tables
		tax
		lda	crc+1
		eor	crct0,X
		sta	crc
		lda	crc+2
		eor	crct1,X
		sta	crc+1
		lda	crc+3
		eor	crct2,X
		sta	crc+2
		lda	crct3,X
		sta	crc+3

		ldx	xio
		rts
.endproc

;----------------------------------------------------------------------
;
; Entrée:
;
; Sortie:
;
; Variables:
;	Modifiées:
;		-
;	Utilisées:
;		-
; Sous-routines:
;	-
;----------------------------------------------------------------------
.proc compl32
		; Complément du CRC
		ldy	#3
	compl:
		lda	crc,Y
		eor	#$FF
		sta	crc,Y
		dey
		bpl	compl

		rts
.endproc

;----------------------------------------------------------------------
;
; Entrée:
;
; Sortie:
;
; Variables:
;	Modifiées:
;		-
;	Utilisées:
;		-
; Sous-routines:
;	-
;----------------------------------------------------------------------
.ifdef CRC16
	.proc makecrc16table
			ldx	#0		; X counts from 0 to 255
		byteloop:
			lda	#0		; A contains the low 8 bits of the CRC-16
			stx	crc		; and CRC contains the high 8 bits
			ldy	#8		; Y counts bits in a byte

		bitloop:
			asl
			rol	crc		; Shift CRC left
			bcc	noadd		; Do nothing if no overflow

			eor	#$21		; else add CRC-16 polynomial $1021
			pha			; Save low byte
			lda	crc		; Do high byte
			eor	#$10
			sta	crc
			pla			; Restore low byte

		noadd:
			dey
			bne	bitloop		; Do next bit

			sta	crct0,X		; Save CRC into table, low byte
			lda	crc		; then high byte
			sta	crct1,X
			inx
			bne	byteloop	; Do next byte

			rts
	.endproc
.endif

;----------------------------------------------------------------------
;
; Entrée:
;
; Sortie:
;
; Variables:
;	Modifiées:
;		-
;	Utilisées:
;		-
; Sous-routines:
;	-
;----------------------------------------------------------------------
.proc makecksumtable
		ldx	#0		; X counts from 0 to 255
	byteloop:
		lda	#0		; A contains the high byte of the CRC-32
		sta	crc+2		; The other three bytes are in memory
		sta	crc+1
		stx	crc

		ldy	#8		; Y counts bits in a byte
	bitloop:
		asl			; The CRC-32 algorithm is similar to CRC-16
		rol	crc+2		; except that it is reversed (originally for
		rol	crc+1		; hardware reasons). This is why we shift
		rol	crc		; right instead of left here.
		bcc	noadd		; Do nothing if no overflow

		eor	#$B7		; else add CRC-32 polynomial $EDB88320
		pha			; Save high byte while we do others
		lda	crc+2
		eor	#$1D		; Most reference books give the CRC-32 poly
		sta	crc+2		; as $04C11DB7. This is actually the same if
		lda	crc+1		; you write it in binary and read it right-
		eor	#$C1		; to-left instead of left-to-right. Doing it
		sta	crc+1		; this way means we won't have to explicitly
		lda	crc		; reverse things afterwards.
		eor	#$04
		sta	crc
		pla			; Restore high byte

	noadd:
		dey
		bne	bitloop		; Do next bit

		sta	crct3,x		; Save CRC into table, high to low bytes
		lda	crc+2
		sta	crct2,x
		lda	crc+1
		sta	crct1,x
		lda	crc
		sta	crct0,x
		inx
		bne	byteloop	; Do next byte

		rts
.endproc

;----------------------------------------------------------------------
;
; Entrée:
;
; Sortie:
;
; Variables:
;	Modifiées:
;		-
;	Utilisées:
;		-
; Sous-routines:
;	-
;----------------------------------------------------------------------
.ifdef CRC32
	.proc makecrc32table
			ldx	#0		; X counts from 0 to 255
		byteloop:
			lda	#0		; A contains the high byte of the CRC-32
			sta	crc+2		; The other three bytes are in memory
			sta	crc+1
			stx	crc

			ldy	#8		; Y counts bits in a byte
		bitloop:
			lsr			; The CRC-32 algorithm is similar to CRC-16
			ror	crc+2		; except that it is reversed (originally for
			ror	crc+1		; hardware reasons). This is why we shift
			ror	crc		; right instead of left here.
			bcc	noadd		; Do nothing if no overflow
			eor	#$ED		; else add CRC-32 polynomial $EDB88320
			pha			; Save high byte while we do others
			lda	crc+2
			eor	#$B8		; Most reference books give the CRC-32 poly
			sta	crc+2		; as $04C11DB7. This is actually the same if
			lda	crc+1		; you write it in binary and read it right-
			eor	#$83		; to-left instead of left-to-right. Doing it
			sta	crc+1		; this way means we won't have to explicitly
			lda	crc		; reverse things afterwards.
			eor	#$20
			sta	crc
			pla			; Restore high byte
		noadd:
			dey
			bne	bitloop		; Do next bit
			sta	crct3,x		; Save CRC into table, high to low bytes
			lda	crc+2
			sta	crct2,x
			lda	crc+1
			sta	crct1,x
			lda	crc
			sta	crct0,x
			inx
			bne	byteloop	; Do next byte

			rts
	.endproc
.endif

;**********************************************************************
; Fin du programme
;**********************************************************************
;----------------------------------------------------------------------
;
; Entrée:
;
; Sortie:
;
; Variables:
;	Modifiées:
;		-
;	Utilisées:
;		-
; Sous-routines:
;	-
;----------------------------------------------------------------------
.proc openbatch
		; Mode batch activé
		ldx	#cbp
		jsr	incr
		ldy	cbp
		lda	cbp+1
		jsr	getfname
		bcs	errFopen

		; rempli de buffer de $00 au cas où...
		lda	#$00
		ldx	#80
	loop:
		sta	inbuf,x
		dex
		bpl	loop

		fopen	fname, O_RDONLY
		sta	fp
		stx	fp+1

		eor	fp+1
		beq	errFopen

		; TODO: Tester le code de retour de fread
		fread	inbuf, #79, 1, fp
		fclose	(fp)

		; Remplace les $0a par des $0d
		ldx	#81
	loop1:
	 	dex
		beq	go_batch
		lda	inbuf,x
		cmp	#$0a
		bne	loop1
		lda	#$0d
		sta	inbuf,x
		bne	loop1

	go_batch:
		ldy	#<inbuf
		lda	#>inbuf
		;bne getopt			; >inbuf ne peut pas être nul

		; Met à jour cbp
		sty	cbp
		sta	cbp+1
		clc
		rts

	errFopen:
		lda	#e13
		sec

	error:
		rts
.endproc

;----------------------------------------------------------------------
;				DATAS
;----------------------------------------------------------------------
.segment "RODATA"
	helpmsg:
	    .byte $0a, $0d
	    .byte $1b,"C            cksum utility\r\n\n"
	    .byte " ",$1b,"TSyntax:",$1b,"P\r\n"
	    .byte "    cksum",$1b,"A-h",$1b,"G|",$1b,"A-v\r\n"
	    .byte "    cksum",$1b,"Afile",$1b,"B[...]\r\n"
	    .byte "    cksum",$1b,"A@filename"
	    .byte "\r\n"
	    .byte $00

.if 0
	longhelp_msg:
	    .byte "\r\n"
	    .byte " ",$1b,"TOptions:",$1b,"P\r\n"
	    .byte "   ",$1b,"A-h",$1b,"Gdisplay command syntax\r\n"
	    .byte "   ",$1b,"A-c",$1b,"Gcheck files\r\n"
	    .byte "\r\n"
	    .byte " ",$1b,"TExamples:",$1b,"P\r\n"
	    .byte "    cksum file1 file2\r\n"
	    .byte "    cksum -c file.lst\r\n"
	    .byte "    cksum @file.cmd\r\n"
	    .byte "\r\n"
	    .byte $00
.endif

;----------------------------------------------------------------------
;
; Entrée:
;
; Sortie:
;
; Variables:
;	Modifiées:
;		-
;	Utilisées:
;		-
; Sous-routines:
;	-
;----------------------------------------------------------------------
.proc printWord
		;	print #' '
		;	lda address+1
		;	jsr PrintHexByte
		;	lda address
		;	jsr PrintHexByte
		;	print #':'
		;	rts

		; Version pour Librairie (sans utilisation de 'address')
		; Entrée; AY = adresse (A=MSB, Y=LSB)
		; /!\ Verifier que PrintHexByte préserve au moins Y
		;     et que XWR0 conserve A et Y
		jsr	PrintHexByte
		tya
		jsr	PrintHexByte
		rts
.endproc

;----------------------------------------------------------------------
;
; Entrée:
;	LSB-LSB+3: Valeur binaire
;
; Sortie:
;	TR0-TR4: Valeur BCD (LSB en premier)
; Variables:
;	Modifiées:
;		-
;	Utilisées:
;		-
; Sous-routines:
;	-
;----------------------------------------------------------------------
.proc bin2bcd
		ldx	#$04		; Clear BCD accumulator
		lda	#$00

	BRM:
		sta	TR0,x		; Zeros into BCD accumulator
		dex
		bpl	BRM

		sed			; Decimal mode for add.

		ldy	#$20		; Y has number of bits to be converted

	BRN:
		asl	LSB		; Rotate binary number into carry
		rol	NLSB
		rol	NMSB
		rol	MSB

		;-------
		; Pour MSB en premier dans BCDA
		;    ldx #$05
		;
		;BRO:
		;    lda BCDA-1,X
		;    adc BCDA-1,X
		;    sta BCDA-1,x
		;    dex
		;    bne BRO

		; Pour LSB en premier dans BCDA

	BCDA = (TR0-$FB) & $ff ; = $0C

		ldx	#$fb		; X will control a five byte addition.

	BRO:
		lda	BCDA,x		; Get least-signficant byte of the BCD accumulator
		adc	BCDA,x		; Add it to itself, then store.
		sta	BCDA,x
		inx			; Repeat until five byte have been added
		bne	BRO

		dey			; et another bit rom the binary number.
		bne	BRN

		cld			; Back to binary mode.
		rts			; And back to the program.
.endproc

;----------------------------------------------------------------------
;
; Entrée:
;	YA: Adresse de la chaine
;	TR0-TR4: Valeur BCD (LSB en premier)
;
; Sortie:
;
; Variables:
;	Modifiées:
;		-
;	Utilisées:
;		-
; Sous-routines:
;	-
;----------------------------------------------------------------------
.proc bcd2str
		sta	RES
		sty	RES+1

		ldx	#$04		; Nombre d'octets à convertir
		ldy	#$00
		; clc

	loop:
		; BCDA: LSB en premier
		lda	TR0,x
		pha
		; and #$f0
		lsr
		lsr
		lsr
		lsr
		clc
		adc	#'0'
		sta	(RES),y

		pla
		and	#$0f
		adc	#'0'
		iny
		sta	(RES),y

		iny
		dex
		bpl	loop

		lda	#$00
		sta	(RES),y
		rts
.endproc

;----------------------------------------------------------------------
;
; Entrée:
;
; Sortie:
;
; Variables:
;	Modifiées:
;		-
;	Utilisées:
;		-
; Sous-routines:
;	-
;----------------------------------------------------------------------
.proc getfname
		; AY : adresse du paramètre suivant
		; cbp:   ''          ''
		;sty dskname
		;sta dskname+1

		ldy	#$ff
	loop:
		iny
		lda	(cbp),y
		sta	fname,y
		beq	endloop

		cmp	#$0d
		beq	endloop

		cmp	#' '
		bne	loop

	endloop:
		cpy	#00
		beq	error_no_filename

		; Termine la chaîne par un nul
		lda	#$00
		sta	fname,y

		; Ajuste cbp
		jsr	calposp
		rts

	error_no_filename:
		lda	#e12
		sec
		rts
.endproc

;===========================================================================
;		Gestion des erreurs
;===========================================================================
.segment "CODE"

;----------------------------------------------------------------------
;
;----------------------------------------------------------------------
.proc crlf1
		crlf
		rts
.endproc

;----------------------------------------------------------------------
;
;----------------------------------------------------------------------
.proc out1
		cputc
		rts
.endproc

;----------------------------------------------------------------------
;
;----------------------------------------------------------------------
.proc prfild
		print	fname
		rts
.endproc

;----------------------------------------------------------------------
;
;----------------------------------------------------------------------
.proc prnamd
		print	fname
		rts
.endproc


;----------------------------------------------------------------------
;
; Entrée:
;
; Sortie:
;
; Variables:
;	Modifiées:
;		-
;	Utilisées:
;		-
; Sous-routines:
;	-
;----------------------------------------------------------------------
seter:
seter1:
	rts

;===========================================================================
;		CH376 - Lecture
;===========================================================================
.segment "CODE"

;----------------------------------------------------------------------
;
; Entrée:
;	AY: Nombre d'octets à lire (A=LSB)
;
; Sortie:
;	A : 0
;	X : Modifié
;	Y : Modifié
;
; Variables:
;	Modifiées:
;		-
;	Utilisées:
;		-
; Sous-routines:
;	-
;----------------------------------------------------------------------
.proc SetByteRead
		pha

		lda	#CH376_BYTE_READ
		sta	CH376_COMMAND

		pla
		sta	CH376_DATA
		sty	CH376_DATA

		jsr	WaitResponse
		cmp	#CH376_USB_INT_DISK_READ
		bne	error

		clc
		rts

	error:
		lda	#e11
		sec
		rts
.endproc

;----------------------------------------------------------------------
;
; Entrée:
;	-
;
; Sortie:
;	A : Modifié
;	X : Modifié
;	Y : Modifié
;
; Variables:
;	Modifiées:
;		-
;	Utilisées:
;		-
; Sous-routines:
;	-
;----------------------------------------------------------------------
.proc ByteRdGo
		lda	#CH376_BYTE_RD_GO
		sta	CH376_COMMAND

		jsr	WaitResponse
		cmp	#CH376_USB_INT_DISK_READ
		bne	error

		clc
		rts

	error:
		lda	#e11
		sec
		rts
.endproc

;----------------------------------------------------------------------
;
; Entrée:
;
; Sortie:
;	A : Code d'erreur
;	X : Modifié
;	Y : Modifié
;	Z : 0 -> Ok, 1 -> Timeout
;----------------------------------------------------------------------
.proc WaitResponse
		ldy	#$ff
	loop1:
		ldx	#$ff
	loop2:
		lda	CH376_COMMAND
		bmi	loop

		lda	#$22
		sta	CH376_COMMAND
		lda	CH376_DATA
		rts

	loop:
	 	dex
	 	bne	loop2

	 	dey
	 	bne	loop1
	 	rts
.endproc

