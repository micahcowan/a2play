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
	.byte $90, $11, $46, $00, $00
HalfStep:
	; AppleSoft floating-point representation
        ; of 2^(1/12), or 1.0594..., the ratio of
        ; the frequency of one musical note, to
        ; the note a half-step below it.
        ; Divide a half-wavelength by this number
        ; to get the half-wavelength a half-step
        ; above it.
	.byte $81, $07, $9C, $7C, $96
NoteTypes:
	.byte "WHQESTX", $00
MsgBad:
	scrcode $0D,"?BAD NOTE SPEC: "
        .byte 0
StrRemain:
	.byte $00
StrSpec = $6
StrSaved:
	.word 0
StrRemainSaved:
	.byte 0
Octave:
	.byte $4
Pitch:
	; Reset for every note. Stored as 16-bit int
        ;  of how many cycles a half-waveform should take.
        ; That means it's not really a pitch value; it's
        ;  proportional to an INVERSE pitch value.
	.word 0
Iterations:
	; Reset for every note. Stored as 16-bit int
        .word 0
Tempo:
	; Stored in "cycles per minute" that a beat takes
        ; Default value is 340,909; equivalent to 180bpm
        .byte $93, $26, $75, $A0, $00
Duration:
	; Current note duration, in fractions of a beat.
        ; (start with quarter note = 1 beat, so 1).
        .byte $81, $00, $0, $0, $0
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
@loop:
	jsr DoNextNote
        ; Zero flag is set iff music string has
        ;  been consumed.
        bne @loop
        pla
        sta $7
        pla
        sta $7
        rts

DoNextNote:
	; Save the current position as the start
        ;  of the note spec, so we can print
        ;  it out later if there's a problem with it.
	lda StrSpec
        sta StrSaved
        lda StrSpec+1
        sta StrSaved+1
        lda StrRemain
        sta StrRemainSaved
        
	jsr StrGetCur
        beq @done; end of string; done processing!
        
	jsr GetNoteBasePitch
        jsr AdjustForOctave
        jsr RoundToNearestInt
        sty Pitch
        sta Pitch+1
        
        jsr GetDuration
        
        jsr CalcIterations
        
        ; before we play, be sure
        ; there's no trailing garbage in note spec
        jsr StrGetNext
        beq @fine ; end of string; fine.
        cmp #' '
        beq @fine ; end of spec; fine.
@notFine:
	jmp NoteNameErr

@fine:        
        jsr PrepSoundLoop
        jsr DoSoundLoop
        
        ; Skip to next note spec (or EOS)
@skipSp:
	jsr StrGetNext
        beq @done
        cmp #' '
        beq @skipSp
@done:
	rts

GetDuration:
	jsr StrGetNext
        beq @nevermind
        cmp #' '
        beq @rewind
        ; See if it's one of our recognized letters
        sta @cmp
        ldx #0
@lo:
	lda NoteTypes,x
        beq @rewind ; not a note type, rewind
@cmp = * + 1
        cmp #$00 ; OVERWRITTEN
        beq @found
        inx
        bne @lo
@rewind:
	jsr StrRewind
@nevermind:
	rts
@found:
	; Rewrite the current duration
        ; ... If we subtract X-reg from #$83, we have a lovely
        ; exponent for our new "duration" FP number.
        ; If we matched "W", x is 0, exp is $83,
        ; and an empty mantissa yields "4".  If we matched "Q",
        ; x is 2, exp becomes #$81, and we get "1". Larger
        ; values of x grant us fractional beats.
        
        ; First, negate x-reg
        txa
        eor #$FF
        clc
        ; now add #$83 (plus also the 1 that
        ; completes the twos complement negation).
        ; We've just done equivalent to #$83 - x.
        adc #$84
        sta Duration
        ldx #4
        lda #0
 @duW:
 	sta Duration,x
        dex
        bne @duW
        rts
        
CalcIterations:
	; What we need: how many sound half-waves to generate
        ; what we have: tempo, note duration, length of
        ;  half-waves in cycles
        
        ; note duration = Db beats
        ; convert Db beats to Ds seconds
        ;   with: Ds seconds
        ;            = Db beats / tempo = Db / (Tempo beats/minute)
        ;         Ds seconds
        ;            = Db beats / tempo beats/(60 seconds)
        ;            = (60 * Db / tempo) seconds
        ;	  Ds = 60 * Db / tempo
        ;   Ds seconds * (cycles/second) = Dc cycles
        ;      cycles/second = 1_022_727
        ;   Dc cycles = 1_022_727 * Ds seconds
        ;   Di half-waves [iterations]
        ;      = Dc cycles / (Hw cycles/half-wave)
        ;   Di = Dc/Hw
        ;
        ;   Tcb cycles/beat
        ;      = (cycles/second) * 60/(tempo beats/minute)
        ;   Tcb = 1_022_727 * 60/Tbpm
        ;
        ;   Dc cycles = Db beats * Tcb (cycles/beat)
        ;   Dc = Db * Tcb
        ;   Di = Dc/Hw = (Db * Tcb)/Hw
        ;     - or -
        ;   #iter = (duration in beats * a minute in cycles / tempo in beats per minute)
        ;             / (half-wave length in terms of cycles)
        
        ; Start with pitch (half-wave length in cycles)
        ldy Pitch
        lda Pitch+1
        jsr AS_GIVAYF ; convert int16 to FP
.if 0
	; Print the saved integer pitch
        jsr AS_PRINT_FAC
        jsr Mon_CROUT
        lda Pitch
        jsr Mon_PRBYTE
        lda #$A0
        jsr Mon_COUT
        lda Pitch+1
        jsr Mon_PRBYTE
        jsr Mon_CROUT
        jsr Mon_CROUT
.endif
        ldy Pitch
        lda Pitch+1
        jsr AS_GIVAYF ; convert int16 to FP
        ; Divide Tempo (stored as cycles-per-beat) by pitch
        lda #<Tempo
        ldy #>Tempo
        jsr AS_FDIV
        ; Multiply by note duration in beats (Db)
        lda #<Duration
        ldy #>Duration
        jsr AS_FMULT
        ; We now have our number of iterations!
        ; Convert to a 16-bit integer
        jsr RoundToNearestInt
        sty Iterations
        sta Iterations+1
.if 0
        tya
        jsr Mon_PRBYTE
        lda #$A0
        jsr Mon_COUT
        lda Iterations+1
        jsr Mon_PRBYTE
        jsr Mon_CROUT
        jsr Mon_CROUT
.endif
	rts

RoundToNearestInt:
        ; Round to nearest, by adding .5 and then
        ;  truncating the fraction off.
        ; RETURNS in Y (low) and A (high)
        
        ; load .5 argument
        lda #$00
        sta AS_ARG+2
        sta AS_ARG+3
        sta AS_ARG+4
        lda #$80
        sta AS_ARG
        sta AS_ARG+1
        
        ; add it (Note: ACC must not be zero!!)
        jsr AS_FADDT
        
        jsr AS_AYINT
        ldy $A1
        lda $A0
	rts

AdjustForOctave:
	jsr TryReadOctave
        ; We want to divide the pitch by 2 for
        ;  each octave above 0. The simplest way
        ;  to do that is just to subtract the octave #
        ;  from the FP number's exponent byte!
        lda AS_FAC
        sec
        sbc Octave
        sta AS_FAC
	rts

TryReadOctave:
	jsr StrGetNext
        cmp #'0'
        bcc @notOct ; c < '0', not an octave number
        cmp #'9'
        bcs @notOct ; c > '8', not an octave number
        ; Yes, we found an octave number!
        ;  set it.
        sec
        sbc #'0'
        sta Octave
        rts
@notOct:
	jsr StrRewind
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

Accidentals:
Flats:
	.byte "B-"
Sharps:
	.byte "#+"
AccidentalsEnd:
AdjustAccidental:
	pha ; push current # halfsteps from base (always >= 3)
        jsr StrGetNext
	sta @srch
	ldx #0
@lo:
	lda Accidentals,x
        @srch = * + 1
        cmp #$00 ; OVERWRITTEN
        beq @found
        inx
        cpx #(AccidentalsEnd - Accidentals)
        bne @lo
        ; Not found! Rewind character
        jsr StrRewind
        pla ; pull earlier halfsteps #
	rts
@found:
	cpx #(Sharps - Flats) ; did we find sharp or flat?
        pla
        bcc @flat
        ; sharp
        adc #0 ; (carry is set)
	rts
@flat:	; flat
	sbc #0 ; (carry unset; borrow is set)
        rts
GetNoteBasePitch:
        cmp #'H'
        bcs NoteNameErr ; > 'G', bail
        sec
        sbc #'A'
        bcc NoteNameErr ; < 'A', bail
        tax
        lda NoteSteps,x
        
        jsr AdjustAccidental
        
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
	; Overwrite overall string ptr
        ; with the one to the current note spec
        lda StrSaved
        sta $6
        lda StrSaved+1
        sta $7
        ldx StrRemainSaved
        ldy #0
        inx
@lo:
	dex
        beq @done
        lda ($6),y
        beq @done
        cmp #' '
        beq @done
        ora #$80 ; Make it printable
        jsr Mon_COUT
        iny
        jmp @lo
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
StrRewind:
	inc StrRemain
        lda StrSpec
        bne :+
        dec StrSpec+1
        :
        dec StrSpec
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

.macro varOps
	.repeat 17
        sec ; at least one of these must be preserved
        .endrepeat
.endmacro
;VariableOpsTemplate:
;	varOps
;VariableOpsTemplateEnd:

PrepSoundLoop:
	; Okay, we have the Pitch in terms of
        ;  "number of cycles" a half-waveform should
        ;  take up... but we don't infinite granularity
        ;  in our control over how long it actually takes
        ;  up.
        ; We know how to make half-waveforms that
        ;  are 74 + 27*N + C cycles long. We need to turn
        ;  Pitch into an N and C that equals the
        ;  original Pitch value.
        
        ; Check - is our Pitch lower than we can handle?
        lda Pitch+1
        bne @pitchOk
        lda Pitch
        cmp #101 ; 101 is the smallest number of cycles
        	 ;  we can manage
        bcs @pitchOk
        ; Set the [inverse] Pitch to the
        ;  minimum (highest possible actual pitch)
        lda #101
        sta Pitch
        
@pitchOk:
        ; First, subtract 74 from Pitch
        sec
        lda Pitch
        sbc #76
        sta Pitch
        sta SubIterations
        lda Pitch+1
        sbc #0 ; for borrow
        sta Pitch+1
        sta SubIterations+1
        
        ; Now divide that by 27
        ldy SubIterations
        jsr AS_GIVAYF
        lda #$80
        sta $A2
        lda #AS_FAC
        ldy #00
        jsr AS_LOAD_ARG_FROM_YA
        lda #<SubIterDivisor
        ldy #>SubIterDivisor
        jsr AS_LOAD_FAC_FROM_YA
        jsr AS_FDIVT
        
        ; That's our "N".
        ; Convert to int and store in SubIterations
        jsr AS_AYINT
        lda $A0
        sta SubIterations+1
        lda $A1
        sta SubIterations
        .if 0
        jsr Mon_PRBYTE
        lda #$A0
        jsr Mon_COUT
        lda SubIterations+1
        jsr Mon_PRBYTE
        lda #$A0
        jsr Mon_COUT
        .endif
        
        ; We interrupt this program to initialize VariableOps.
	ldx #0
@voInitLo:
	lda #$38 ; fill it with SEC ops to start
        sta VariableOps,x
        inx
        cpx #(VariableOpsEnd - VariableOps)
        bne @voInitLo
        
        ; Convert back to float and multiply by 27
        ldy SubIterations
        lda SubIterations+1
        jsr AS_GIVAYF
        lda #<SubIterDivisor
        ldy #>SubIterDivisor
        jsr AS_FMULT
        jsr AS_AYINT
        ; Now to get the "remainder", we can subtract
        ;  from the original dividend
        ;  (we can ignore the high bytes)
        lda Pitch
        sec
        sbc $A1
        ; that's our "C". Trim the VariableOps region
        ; accordingly!
        lsr
        tax
        inx ; skip first (mandatory) SEC
        bcc @tail ; C was even
        ; C was odd. Replace first SEC in VariableOps
        ; with (no-op) BCS
        dex
        lda #$B0
        sta VariableOps,x
        inx
        lda #$00
        sta VariableOps,x
        inx
@tail:
	lda #$B0
        sta VariableOps,x
        inx
        txa
        pha
        ; Negate it
        eor #$FF
        clc
        adc #1
        ; to subtract from the end
        clc
        adc #(VariableOpsEnd-VariableOps-1) ; -1 to account for diff between X, and PC when it obeys this 
        tay
        pla
        tax
        tya
        sta VariableOps,x
        
	rts

SubIterDivisor:
	.byte $85, $58, 0, 0, 0 ; 27.0

.align 256 ; helps ensure timing
SubIterations:
	.word 0
PitchCtr:
	.word 0
DoSoundLoop:
	; following four ops total 16 cycles
	lda SubIterations
        sta PitchCtr
        lda SubIterations+1
        sta PitchCtr+1
        lda SS_SPKR
        sec
VariableOps:
	varOps
VariableOpsEnd:
@halfWave:
	sec			; 2
        lda PitchCtr		; +4 = 6
        sbc #1			; +2 = 8
        sta PitchCtr		; +4 = 12
        lda PitchCtr+1		; +4 = 16
        sbc #0			; +2 = 18
        sta PitchCtr+1		; +4 = 22
        cmp #$ff		; +2 = 24
        bne @halfWave		; +3 = 27
        ; ^ Does Pitch+1 iterations
        sec			; 2
 	lda Iterations		; +4 = 6
        sbc #1			; +2 = 8
        sta Iterations		; +4 = 12
        lda Iterations+1	; +4 = 16
        sbc #0 ; for borrow	; +2 = 18
        sta Iterations+1	; +4 = 22
        cmp #$ff		; +2 = 24
        bne DoSoundLoop		; +3 = 27
        ; ^ Does iter+1 iterations
        ; ea. iter = (26 + 27 + 20 + 3 + 4 = 76) + (Pitch * 27) + C cycles
        ;    (3 for req'd varOps branch, 4 for 1 req'd varOps sec, 1 req'd sec-or-bcc (for C's odd bit)
        ; PrepSoundLoop needs to re-adjust Pitch so the total equals
        ; the desired value of *cycles*, rather than iterations
        ; in the "halfWave" loop
	rts
