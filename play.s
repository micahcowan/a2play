.include "a2-monitor.inc"

.macpack apple2

.export InstallAmperPlay

InstallAmperPlay:
	; Save any existing & handler
        ldy #0
@cpyAmp:
        lda AS_AMP,y
        sta NextAmper,y
        iny
        cpy #3
        bne @cpyAmp
	; Install our handler
	lda #$4c
        sta AS_AMP
        lda #<AmperPlay
        sta AS_AMP+1
        lda #>AmperPlay
        sta AS_AMP+2
	rts

NextAmper:
	.byte $4C, $00, $00
PlayTag:
	.byte "PLAY,"
        .byte $00
StrRemain:
	.byte $00
AmperPlay:
	jsr CheckTag
        bcs YesItsUs
	jmp NextAmper
	rts
YesItsUs:
	jsr PrepNoteStr
        lda AS_INDEX+1
        jsr Mon_PRBYTE
        lda AS_INDEX
        jsr Mon_PRBYTE
        lda #(':' | $80)
        jsr Mon_COUT
        lda #$A0
        jsr Mon_COUT
@lo:
        jsr StrGetNext
        beq @out
        eor #$80
        jsr Mon_COUT
        iny
        bne @lo
@out:
        lda #$8D
        jsr Mon_COUT
	rts

PrepNoteStr:
	jsr AS_FRMEVL
        jsr AS_FREFAC
        sta StrRemain
        rts
StrGetNext:
	lda StrRemain
        beq @done
        dec StrRemain
        ldy #0
        lda (AS_INDEX),y
        ; convert to uppercase, ASCII
        cmp #$61
        bcc @skipCvt
        cmp #$7B
        bcs @skipCvt
        and #$DF
@skipCvt:
        inc AS_INDEX
        bne @done
        inc AS_INDEX+1
@done:
       	rts

CheckTag:
	; Save current token location,
        ;  to restore if tag doesn't match
	lda AS_TXTPTR
        pha
        lda AS_TXTPTR+1
        pha
	ldx #0
        ldy #0
	jsr AS_CHRGOT
@tagScan:
	lda PlayTag,x
        beq @match
        cmp (AS_TXTPTR),y
        bne @noMatch
        jsr AS_CHRGET
        inx
        bne @tagScan
@noMatch:
	pla
        sta AS_TXTPTR+1
        pla
        sta AS_TXTPTR
        clc
	rts
@match:
	; Not restoring token scanner, so discard
        ;  stack-saved address
	pla
        pla
	sec
        rts
        