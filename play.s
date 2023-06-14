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
MsgBad:
	scrcode $0D,"?BAD NOTE SPEC: "
        .byte 0
StrRemain:
	.byte $00
AmperPlay:
	jsr CheckTag
        bcs YesItsUs
	jmp NextAmper
	rts
YesItsUs:
	jsr PrepNoteStr
.if 0
        lda AS_INDEX+1
        jsr Mon_PRBYTE
        lda AS_INDEX
        jsr Mon_PRBYTE
        lda #(':' | $80)
        jsr Mon_COUT
        lda #$A0
        jsr Mon_COUT
        jsr StrGetCur
@lo:
        beq @out
        eor #$80
        jsr Mon_COUT
        jsr StrGetNext
        bne @lo
@out:
        lda #$8D
        jsr Mon_COUT
	rts
.endif
@loop:
	jsr DoNextNote
        ; Zero flag is set iff music string has
        ;  been consumed.
        bne @loop

DoNextNote:
	jsr StrGetCur
        beq @exit
	jsr GetNoteBasePitch
        jsr StrGetNext
        cmp #' '
        beq @skipSp
        ; XXX Handle more note spec (octave, duration...)
        jmp @exit
@skipSp:
	jsr StrGetNext
        cmp #' '
        beq @skipSp
@exit:
	rts

InitPitch:
	
	rts
GetNoteBasePitch:
	jsr InitPitch
        cmp #'H'
        bcs NoteNameErr ; > 'G', bail
        sec
        sbc #'A'
        bcc NoteNameErr ; < 'A', bail
        tax
        lda NoteSteps,x
        ; XXX Adjust base pitch
@found:
	rts
NoteNameErr:
	; handle note spec error
        bit AS_ERRFLG	; ON ERR turned on?
        bmi @noMsg ; yes; skip our message
        lda #<MsgBad
        ldy #>MsgBad
        jsr Print
        jsr PrintNoteSpec
@noMsg:
	jmp AS_SYNERR
NoteSteps:
	; How many steps away from "A" each note
        ;  is. A and B are adjusted up by an
        ;  octave, so that octaves "start" at C
        ;  and not A.
	.byte 12,14,3,5,7,8,10

Print:
	sta @lda+1
        sty @lda+2
        ldy #0
@lda:
	lda $0000,y ; OVERWRITTEN
        beq @done
        jsr Mon_COUT
        inc @lda+1
        bne @lda
        inc @lda+2
        bne @lda
@done:
	rts
PrintNoteSpec:
	jsr StrGetCur
        beq @done
@lo:
        cmp #' '
        beq @done
        ora #$80 ; Make it printable
        jsr Mon_COUT
        jsr StrGetNext
        bne @lo
@done:
	rts

PrepNoteStr:
	jsr AS_FRMEVL
        jsr AS_FREFAC
        sta StrRemain
        rts
StrGetNext:
	lda StrRemain
        beq StrRts
        dec StrRemain
        inc AS_INDEX
        bne StrGetCur
        inc AS_INDEX+1
StrGetCur:
	lda StrRemain
        beq @done
        ldy #0
        lda (AS_INDEX),y
        ; convert to uppercase, ASCII
        cmp #$61
        bcc @skipCvt
        cmp #$7B
        bcs @skipCvt
        and #$DF
@skipCvt:
@done:
StrRts:
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
        