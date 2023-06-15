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
BasePitch:
	; AppleSoft floating-point representation
        ; of the number 37190, the length of
        ; one-half of the waveform of the A note
        ; that is an octave below the lowest A note
        ; supported by this program, as measured
        ; in 1.023MHz cycles.
	.byte $90, $91, $46, $00, $00
HalfStep:
	; AppleSoft floating-point representation
        ; of 2^(1/12), or 1.0594..., the ratio of
        ; the frequency of one musical note, to
        ; the note a half-step below it.
        ; Divide a half-wavelength by this number
        ; to get the half-wavelength a half-step
        ; above it.
	.byte $81, $87, $9C, $7C, $96
MsgBad:
	scrcode $0D,"?BAD NOTE SPEC: "
        .byte 0
StrRemain:
	.byte $00
StrSpec = $6
StrSaved:
	.word 0
AmperPlay:
	jsr CheckTag
        bcs YesItsUs
	jmp NextAmper
	rts
YesItsUs:
	; Save away ZP items we may overwrite with StrSpec
	lda $6
        pha
        lda $7
        pha
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
        pla
        sta $7
        pla
        sta $6
	rts
.endif
@loop:
	jsr DoNextNote
        ; Zero flag is set iff music string has
        ;  been consumed.
        bne @loop

DoNextNote:
	; Save the current position as the start
        ;  of the note spec, so we can print
        ;  it out later if there's a problem with it.
	lda StrSpec
        sta StrSaved
        lda StrSpec+1
        sta StrSaved+1
        
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

PrintFacBytes:
	ldx #0
:
	lda AS_FAC, x
        jsr Mon_PRBYTE
        lda #$A0
        jsr Mon_COUT
        inx
        cpx #5
        bne :-
        jsr Mon_CROUT
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
        
        ; We have N, we want HalfStep^N.
        
        ; First, convert our integer exponent
        ;  into floating-point
        tay
        lda #0
        jsr AS_GIVAYF
        ; Get pointer to HalfStep into A, Y
        ; ...and do the exponentiation (^)
        lda #<HalfStep
        ldy #>HalfStep
        jsr AS_LOAD_ARG_FROM_YA
        jsr AS_FPWRT
        
        ; Now we need to divide BasePitch by our
        ;  "steps" result, to get the base pitch
        ;  for *this* note.
        lda #<BasePitch
        ldy #>BasePitch
        jsr AS_FDIV
        ldx #0
        stx $A2 ; ensure a positive result
        .if 0
        jsr AS_AYINT
        ldy $A1
        lda $A0
        jsr AS_GIVAYF
        .endif
        jsr PrintFacBytes
        jsr Mon_CROUT
        jsr AS_PRINT_FAC
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
        lda AS_INDEX
        sta StrSpec
        lda AS_INDEX+1
        sta StrSpec+1
        rts
StrGetNext:
	lda StrRemain
        beq StrRts
        dec StrRemain
        inc StrSpec
        bne StrGetCur
        inc StrSpec+1
StrGetCur:
	lda StrRemain
        beq @done
        ldy #0
        lda (StrSpec),y
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
        