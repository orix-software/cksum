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
;			Orix Kernel includes
;----------------------------------------------------------------------
.include "kernel/src/include/kernel.inc"
;.include "kernel/src/include/memory.inc"
;.include "kernel/src/include/process.inc"
.include "kernel/src/include/ch376.inc"
;.include "kernel/src/orix.inc"


;----------------------------------------------------------------------
;			Orix Shell includes
;----------------------------------------------------------------------
; Pour userzp: include les 2 fichiers...
;.include "shell/src/include/bash.inc"
;.include "shell/src/include/orix.inc"


;----------------------------------------------------------------------
;			Orix SDK includes
;----------------------------------------------------------------------
.include "macros/SDK.mac"
.include "include/SDK.inc"
.include "macros/types.mac"
.include "include/errors.inc"

; .reloc nécessaire parce que le dernier segment de orix.inc est .bss et qu'il
; y a des .org xxxx dans les fichiers .inc...
.reloc

;----------------------------------------------------------------------
;				Imports
;----------------------------------------------------------------------

; From debug
.import PrintHexByte
;.import PrintRegs

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
;.export _argc
;.export _argv

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
;	unsigned char extractbuf
	unsigned short fp
	char fname[max_path]
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
;				ORIXHDR
;----------------------------------------------------------------------
; MODULE __MAIN_START__, __MAIN_LAST__, _main

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
;	lda #$00
;	sta extractptr
	ldy #>( (__BSS_LOAD__ + __BSS_SIZE__) & $ff00)
	lda #<( (__BSS_LOAD__ + __BSS_SIZE__) )
	beq skip
	iny
 skip:
	sty extractbuf

	; Calcul de la taille du tampon pour l'extraction
	sec
	lda #>__RAMEND__
	sbc extractbuf
	sta nbpagemax+1
	lda #$00
	sta nbpagemax

	; Initialisation de la table
	; test cksum
	jsr makecksumtable

	ldy #<(BUFEDT+.strlen("CKSUM"))
	lda #>(BUFEDT+.strlen("CKSUM"))

 getopt:
	jsr sopt
	.asciiz "H"
	bcs error

	cpx #$80
	bne cmnd_exec
	jmp cmnd_help

 cmnd_exec:
	ldy #$00
	lda (cbp),y
	cmp #'@'
	bne go

	; Mode batch activé
	jsr openbatch
	bcc getopt
	bcs error

 go:
	jsr getfname
	bcs error
 cksum:
	jsr cmnd_cksum
	bcs error

	;
	ldy #$00
 skipcr:
	lda (cbp),y
	beq end
	cmp #$0d
	bne _calcposp
	iny
	bne skipcr
	; Si débordement de Y
	beq errEOF

  _calcposp:
	jsr calposp
;	bne go
	bne getopt

 error:
	jsr ermes
 end:
	BRK_KERNEL XCRLF
	rts

 errEOF:
	lda #e4
	sec
	jmp ermes
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
        print helpmsg, NOSAVE
;	print longhelp_msg, NOSAVE
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
	lda #$00
	sta crc
	sta crc+1
	sta crc+2
	sta crc+3
	; Taille du fichier
	sta fsize
	sta fsize+1
	sta fsize+2
	sta fsize+3

	fopen fname, O_RDONLY
	sta fp
	sty fp+1

	ora fp+1
	beq errFopen

 read:
	lda #$ff
	tay
	jsr SetByteRead
	bcs error

 read1:
	lda #$27
	sta CH376_COMMAND
	ldy CH376_DATA
	beq nextchunk

	; Mise à jour de la taille du fichier (MSB en premier)
	clc
	tya
	adc fsize+3
	sta fsize+3
	bcc loop
	inc fsize+2
	bne loop
	inc fsize+1
	bne loop
	inc fsize

 loop:
	lda CH376_DATA
	jsr crc32
	dey
	bne loop

 nextchunk:
	jsr ByteRdGo
	bcc read1

	lda #$ff
	tay
	jsr SetByteRead
	bcc read1

 end:
	fclose(fp)

	; Cherche le premier octet non nul de la taille du fichier
	ldx #$ff
 loop1:
	inx
	lda fsize,x
	bne addfsize
	cpx #$03
	bne loop1

	; Ajoute crc32(taille du fichier)
	; LSB en premier
 addfsize:
	ldy #$03
 loop3:
	lda fsize,y
	jsr crc32
	dey
	inx
	cpx #$04
	bne loop3

 dispcksum:
	jsr compl32
	jsr dispresult

	rts

 errFopen:
	lda #e13
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

	;print #'=',NOSAVE

	; Checksum en Décimal
	jsr bin2bcd
	lda #<str
	ldy #>str
	jsr bcd2str
	print str, NOSAVE

	; Taille du fichier
	print #' ', NOSAVE
	lda fsize
	sta crc
	lda fsize+1
	sta crc+1
	lda fsize+2
	sta crc+2
	lda fsize+3
	sta crc+3

	jsr bin2bcd
	lda #<str
	ldy #>str
	jsr bcd2str

	; Saute les 0 non significatifs
	ldx #$ff
 skip0:
	inx
	lda str,x
	cmp #'0'
	bne _print
	cpx #$09
	bne skip0

 _print:
	txa
	clc
	adc #<str
	pha
	lda #>str
	adc #$00
	tay
	pla
	BRK_KERNEL XWSTR0

	; Nom du fichier
	print #' ', NOSAVE
	print fname, NOSAVE

	BRK_KERNEL XCRLF
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
	BRK_KERNEL XCRLF

	; Valeur initiale: $ffff
	lda #$ff
	sta crclo
	sta crchi

	lda #$a1
	jsr crc16_f

	lda #$a1
	jsr crc16_f

	lda #$a1
	jsr crc16_f

	lda #$fe
	jsr crc16_f

	lda #$00
	jsr crc16_f

	lda #$00
	jsr crc16_f

	lda #$01
	jsr crc16_f

	lda #$01
	jsr crc16_f

	ldy crclo
	lda crchi
	jsr printWord

	BRK_KERNEL XCRLF

	; --------------------------
	; Initialisation de la table
	jsr makecrc16table
	lda #$ff
	sta crclo
	sta crchi

	lda #$a1
	jsr crc16

	lda #$a1
	jsr crc16

	lda #$a1
	jsr crc16

	lda #$fe
	jsr crc16

	lda #$00
	jsr crc16

	lda #$00
	jsr crc16

	lda #$01
	jsr crc16

	lda #$01
	jsr crc16

	ldy crclo
	lda crchi
	jsr printWord

	BRK_KERNEL XCRLF

	; --------------------------
	; Initialisation de la table
	jsr makecrc32table

	; Valeur initiale
	lda #$ff
	sta crc
	sta crc+1
	sta crc+2
	sta crc+3

	; Calcul du CRC32
;	lda #$a1
;	jsr crc32

;	lda #$a1
;	jsr crc32

;	lda #$a1
;	jsr crc32

;	lda #$fe
;	jsr crc32

;	lda #$00
;	jsr crc32

;	lda #$00
;	jsr crc32

;	lda #$01
;	jsr crc32

;	lda #$01
;	jsr crc32

	lda #'1'
	jsr crc32
	lda #'2'
	jsr crc32
	lda #'3'
	jsr crc32


;	ldy #$00
;loop
;	lda buffer,y
;	jsr crc32
;	iny
;	bne loop

	; Complément du CRC
	jsr compl32

	; Affiche le crc
	ldy crc+2
	lda crc+3
	jsr printWord
	ldy crc
	lda crc+1
	jsr printWord

	BRK_KERNEL XCRLF

	; --------------------------
	; Initialisation de la table
	; test cksum
	jsr makecksumtable

	; Valeur initiale
	lda #$00
	sta crc
	sta crc+1
	sta crc+2
	sta crc+3

	; Calcul cksum
	lda #'1'
	jsr crc32
	lda #'2'
	jsr crc32
	lda #'3'
	jsr crc32

	; Ajoute la longueur du message (LSB en premier)
	lda #$03
	jsr crc32

	jsr compl32

	ldy crc+1
	lda crc
	jsr printWord
	ldy crc+3
	lda crc+2
	jsr printWord

	print #'=',NOSAVE

	jsr bin2bcd
	lda #<str
	ldy #>str
	jsr bcd2str
	print str, NOSAVE
	BRK_KERNEL XCRLF
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
        EOR crchi       ; A contained the data
        STA crchi       ; XOR it into high byte
        LSR             ; right shift A 4 bits
        LSR             ; to make top of x^12 term
        LSR             ; ($1...)
        LSR
        TAX             ; save it
        ASL             ; then make top of x^5 term
        EOR crclo       ; and XOR that with low byte
        STA crclo       ; and save
        TXA             ; restore partial term
        EOR crchi       ; and update high byte
        STA crchi       ; and save
        ASL             ; left shift three
        ASL             ; the rest of the terms
        ASL             ; have feedback from x^12
        TAX             ; save bottom of x^12
        ASL             ; left shift two more
        ASL             ; watch the carry flag
        EOR crchi       ; bottom of x^5 ($..2.)
        TAY             ; save high byte
        TXA             ; fetch temp value
        ROL             ; bottom of x^12, middle of x^5!
        EOR crclo       ; finally update low byte
        STA crchi       ; then swap high and low bytes
        STY crclo
        RTS
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
         EOR crchi       ; Quick CRC computation with lookup tables
         TAX
         LDA crclo
         EOR crct1,X
         STA crchi
         LDA crct0,X
         STA crclo
         RTS
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
	stx xio

         EOR crc         ; Quick CRC computation with lookup tables
         TAX
         LDA crc+1
         EOR crct0,X
         STA crc
         LDA crc+2
         EOR crct1,X
         STA crc+1
         LDA crc+3
         EOR crct2,X
         STA crc+2
         LDA crct3,X
         STA crc+3

	ldx xio
         RTS

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
         LDY #3
COMPL    LDA crc,Y
         EOR #$FF
         STA crc,Y
         DEY
         BPL COMPL
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
         LDX #0          ; X counts from 0 to 255
BYTELOOP LDA #0          ; A contains the low 8 bits of the CRC-16
         STX crc         ; and CRC contains the high 8 bits
         LDY #8          ; Y counts bits in a byte
BITLOOP  ASL
         ROL crc         ; Shift CRC left
         BCC NOADD       ; Do nothing if no overflow
         EOR #$21        ; else add CRC-16 polynomial $1021
         PHA             ; Save low byte
         LDA crc         ; Do high byte
         EOR #$10
         STA crc
         PLA             ; Restore low byte
NOADD    DEY
         BNE BITLOOP     ; Do next bit
         STA crct0,X     ; Save CRC into table, low byte
         LDA crc         ; then high byte
         STA crct1,X
         INX
         BNE BYTELOOP    ; Do next byte
         RTS
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
         LDX #0          ; X counts from 0 to 255
BYTELOOP LDA #0          ; A contains the high byte of the CRC-32
         STA crc+2       ; The other three bytes are in memory
         STA crc+1
         STX crc
         LDY #8          ; Y counts bits in a byte
BITLOOP  ASL             ; The CRC-32 algorithm is similar to CRC-16
         ROL crc+2       ; except that it is reversed (originally for
         ROL crc+1       ; hardware reasons). This is why we shift
         ROL crc         ; right instead of left here.
         BCC NOADD       ; Do nothing if no overflow
         EOR #$B7        ; else add CRC-32 polynomial $EDB88320
         PHA             ; Save high byte while we do others
         LDA crc+2
         EOR #$1D        ; Most reference books give the CRC-32 poly
         STA crc+2       ; as $04C11DB7. This is actually the same if
         LDA crc+1       ; you write it in binary and read it right-
         EOR #$C1        ; to-left instead of left-to-right. Doing it
         STA crc+1       ; this way means we won't have to explicitly
         LDA crc         ; reverse things afterwards.
         EOR #$04
         STA crc
         PLA             ; Restore high byte
NOADD    DEY
         BNE BITLOOP     ; Do next bit
         STA crct3,X     ; Save CRC into table, high to low bytes
         LDA crc+2
         STA crct2,X
         LDA crc+1
         STA crct1,X
         LDA crc
         STA crct0,X
         INX
         BNE BYTELOOP    ; Do next byte
         RTS
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
         LDX #0          ; X counts from 0 to 255
BYTELOOP LDA #0          ; A contains the high byte of the CRC-32
         STA crc+2       ; The other three bytes are in memory
         STA crc+1
         STX crc
         LDY #8          ; Y counts bits in a byte
BITLOOP  LSR             ; The CRC-32 algorithm is similar to CRC-16
         ROR crc+2       ; except that it is reversed (originally for
         ROR crc+1       ; hardware reasons). This is why we shift
         ROR crc         ; right instead of left here.
         BCC NOADD       ; Do nothing if no overflow
         EOR #$ED        ; else add CRC-32 polynomial $EDB88320
         PHA             ; Save high byte while we do others
         LDA crc+2
         EOR #$B8        ; Most reference books give the CRC-32 poly
         STA crc+2       ; as $04C11DB7. This is actually the same if
         LDA crc+1       ; you write it in binary and read it right-
         EOR #$83        ; to-left instead of left-to-right. Doing it
         STA crc+1       ; this way means we won't have to explicitly
         LDA crc         ; reverse things afterwards.
         EOR #$20
         STA crc
         PLA             ; Restore high byte
NOADD    DEY
         BNE BITLOOP     ; Do next bit
         STA crct3,X     ; Save CRC into table, high to low bytes
         LDA crc+2
         STA crct2,X
         LDA crc+1
         STA crct1,X
         LDA crc
         STA crct0,X
         INX
         BNE BYTELOOP    ; Do next byte
         RTS
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
	ldx #cbp
	jsr incr
	ldy cbp
	lda cbp+1
	jsr getfname
	bcs errFopen

	; rempli de buffer de $00 au cas où...
	lda #$00
	ldx #80
 loop:
	sta inbuf,x
	dex
	bpl loop

	fopen fname, O_RDONLY
	sta fp
	sty fp+1

	ora fp+1
	beq errFopen

	; TODO: Tester le code de retour de fread
	fread inbuf, #79, fp
	fclose (fp)

	; Remplace les $0a par des $0d
	ldx #81
 loop1:
 	dex
	beq go_batch
	lda inbuf,x
	cmp #$0a
	bne loop1
	lda #$0d
	sta inbuf,x
	bne loop1

 go_batch:
	ldy #<inbuf
	lda #>inbuf
	;bne getopt			; >inbuf ne peut pas être nul

	; Met à jour cbp
	sty cbp
	sta cbp+1
	clc
	rts

  errFopen:
	lda #e13
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
	    .byte "    cksum",$1b,"A-h\r\n"
;	    .byte "    cksum",$1b,"A-c file\r\n"
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
printWord:
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
	jsr PrintHexByte
	tya
	jsr PrintHexByte
	rts

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
        ldx #$04          ; Clear BCD accumulator
        lda #$00

    BRM:
        sta TR0,x        ; Zeros into BCD accumulator
        dex
        bpl BRM

        sed               ; Decimal mode for add.

        ldy #$20          ; Y has number of bits to be converted

    BRN:
        asl LSB           ; Rotate binary number into carry
        rol NLSB
        rol NMSB
        rol MSB

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

        ldx #$fb          ; X will control a five byte addition.

    BRO:
        lda BCDA,x    ; Get least-signficant byte of the BCD accumulator
        adc BCDA,x    ; Add it to itself, then store.
        sta BCDA,x
        inx               ; Repeat until five byte have been added
        bne BRO

        dey               ; et another bit rom the binary number.
        bne BRN

        cld               ; Back to binary mode.
        rts               ; And back to the program.

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
	sta RES
	sty RES+1

	ldx #$04          ; Nombre d'octets à convertir
	ldy #$00
;	clc

loop:
	; BCDA: LSB en premier
	lda TR0,X
	pha
	; and #$f0
	lsr
	lsr
	lsr
	lsr
        clc
	adc #'0'
	sta (RES),Y

	pla
	and #$0f
	adc #'0'
	iny
	sta (RES),y

	iny
	dex
	bpl loop

	lda #$00
	sta (RES),y
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

	ldy #$ff
  loop:
	iny
	lda (cbp),y
	sta fname,y
	beq endloop
	cmp #$0d
	beq endloop
	cmp #' '
	bne loop

  endloop:
	cpy #00
	beq error_no_filename

	; Termine la chaîne par un nul
;	cmp #$00
;	beq ajuste

	lda #$00
	;sta (cbp),y
	sta fname,y
	;iny

	; Ajuste cbp
;  ajuste:
;	clc
;	tya
;	adc cbp
;	sta cbp
;	bcc skip
;	inc cbp+1
;
;  skip:
;	clc
	jsr calposp
	rts

  error_no_filename:
	lda #e12
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
crlf1:
	BRK_KERNEL XCRLF
	rts

;----------------------------------------------------------------------
;
;----------------------------------------------------------------------
out1:
	BRK_KERNEL XWR0
	rts

;----------------------------------------------------------------------
;
;----------------------------------------------------------------------
.proc prfild
	print fname, NOSAVE
	rts
.endproc

;----------------------------------------------------------------------
;
;----------------------------------------------------------------------
.proc prnamd
	print fname, NOSAVE
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

	lda #CH376_BYTE_READ
	sta CH376_COMMAND

	pla
	sta CH376_DATA
	sty CH376_DATA

	jsr WaitResponse
	cmp #CH376_USB_INT_DISK_READ
	bne error
	clc
	rts

 error:
	lda #e11
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
	lda #CH376_BYTE_RD_GO
	sta CH376_COMMAND

	jsr WaitResponse
	cmp #CH376_USB_INT_DISK_READ
	bne error
	clc
	rts

 error:
	lda #e11
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
	ldy #$ff

 loop1:
	ldx #$ff
 loop2:
	lda CH376_COMMAND
	bmi loop
	lda #$22
	sta CH376_COMMAND
	lda CH376_DATA
	rts

 loop:
 	dex
 	bne loop2
 	dey
 	bne loop1
 	rts
.endproc


