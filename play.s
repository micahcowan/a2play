.include "a2-monitor.inc"
.include "division.inc"

.macpack apple2

.export InstallAmperPlay

PRODOS_FREEBUFR = $BEF8
PRODOS_GETBUFR = $BEF5
PRODOS_START = $9A00

InstallAmperPlay:
.ifdef PRODOS
        jsr PRODOS_FREEBUFR
        lda #(>PRODOS_START - >InstallAmperPlay)
        jsr PRODOS_GETBUFR
        bcc @good
@prMsg:
        ldy #0
@lo:
        lda @MemErrMsg,y
        beq @msgDone
        jsr Mon_COUT
        iny
        bne @lo
@msgDone:
        rts
@MemErrMsg:
        scrcode "COULDN'T PROTECT MEMORY! NOT INSTALLING.",$0D
        .byte $0
@good:
        cmp #>InstallAmperPlay
        bne @msgDone
.else
        ; Set HIMEM below us
        lda #<InstallAmperPlay
        sta AS_MEMSIZE
        sta AS_FRETOP
        lda #>InstallAmperPlay
        sta AS_MEMSIZE+1
        sta AS_FRETOP+1
.endif
.if 0
        ;; ONLY DO THIS IF WE'RE CHAINING
        ;; OTHERWISE WE'LL INFLOOP IF WE'RE LOADED TWICE
@saveHandler:
	; Save any existing & handler
        ldy #0
@cpyAmp:
        lda AS_AMP,y
        sta NextAmper,y
        iny
        cpy #3
        bne @cpyAmp
.endif
	; Install our handler
	lda #$4c
        sta AS_AMP
        lda #<AmperPlay
        sta AS_AMP+1
        lda #>AmperPlay
        sta AS_AMP+2
	rts

NextAmper:
	.byte $60, 0, 0
	;.byte $4C, $00, $00
PlayTag:
	.byte "PLAY,"
        .byte $00
NoteSteps:
	; How many steps away from "A" each note
        ;  is. A and B are adjusted up by an
        ;  octave, so that octaves "start" at C
        ;  and not A.
	.byte 0,2,3,5,7,8,10
BasePitches:
	;     A, A#, B
	.word 18595, 17551, 16566
        ;     C, C#, D   [drop down: 8ve starts at "C",
        ;                 but lookup starts at "A"]
        .word 31273, 29518, 27861
        ;     D#, E, F
        .word 26297, 24821, 23428
        ;     F#, G, G#
        .word 22113, 20872, 19701
OneBpmWholeNote:
	; 4 * 60, times the number of CPU cycles per second,
        ;  or 4 * 60 * 1_022_727
        ; Represents the length in CPU cycles of a
        ;  whole note at 1 bpm
        ; Stored as a big-endian 32-bit unsigned int
        ;  (like Tempo, Duration)
        .byte $0E, $A1, $56, $90
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
StackSave:
	.byte 0
IsRest:
	.byte 0
Octave:
	.byte $4
Pitch:
	; Reset for every note. Stored as BE 16-bit uint
        ;  of how many cycles a half-waveform should take.
        ; That means it's not really a pitch value; it's
        ;  proportional to an INVERSE pitch value.
	.word 0
Iterations:
	; Reset for every note. Stored as BE 16-bit uint
        .word 0
Tempo:
	; Stored in "cycles per minute" that 4 beats takes,
        ;  as a big-endian 32-bit unsigned int.
        ; Default value is 340,909; equivalent to 180bpm
        .byte $00, $14, $CE, $B4
Duration:
	; Current note duration, same format as Tempo:
        ;  "cycles per minute" that a beat takes.
        ; Default same as Tempo, since default note
        ;  length is a quarter note (one beat)
        .byte $00, $05, $33, $AD
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
        jsr MaybeTempo
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

GetIsNumPart:
	cmp #'.'
        beq @rts
        cmp #'0'
        bcc @rts
        cmp #('9'+1)
        bcs @clc
        sec
        rts
@clc:
	clc
@rts:
	rts

MaybeTempo:
	; Maybe the user specified a tempo at the
        ; start of the string?
	lda StrSpec
        sta StrSaved
        lda StrSpec+1
        sta StrSaved+1
        lda StrRemain
        sta StrRemainSaved
        
        jsr StrGetCur
        cmp #'T'
        bne @done
        
        jsr StrGetNext
        jsr GetIsNumPart
        bcs @isnum ; -->
        ; Not "T" followed by a number; back up to before T.
        jsr StrRewind
@done:
        rts
@isnum:
	; Found T followed by number.
        ; Save current position
        lda StrSpec
        sta AS_INDEX
        lda StrSpec+1
        sta AS_INDEX+1
        
        ; ...and scan for first non-number
@scan:
	jsr StrGetNext
        beq @ok
        jsr GetIsNumPart
        bcs @scan
        cmp #' '
        beq @ok
        bne @restoreSpec ; number didn't end with number char.
@ok:
        ; found end of number. Mark it.
        ldy #0
        lda (StrSpec),y
        pha ; PUSH ORIG CHAR
            lda #0
            sta (StrSpec),y
            ; co-opt the BASIC token scanner, to fetch number
            lda AS_TXTPTR
            sta AS_STRNG2
            lda AS_TXTPTR+1
            sta AS_STRNG2+1
            lda AS_INDEX
            sta AS_TXTPTR
            lda AS_INDEX+1
            sta AS_TXTPTR+1
            
            jsr AS_CHRGOT
            jsr AS_FIN
            
        pla ; restore original char where we found number-end
        sta (StrSpec),y
        ; Restore original token buffer
        lda AS_STRNG2
        sta AS_TXTPTR
        lda AS_STRNG2+1
        sta AS_TXTPTR+1
        
        ; Convert the result to an integer and arrange
        ;  as integer divisor
        jsr AS_AYINT
        lda $A0 ; high-order byte
        sta locDivisor+2
        lda $A1 ; low-order byte
        sta locDivisor+3
        lda #0
        sta locDivisor
        sta locDivisor+1
        
        ; Use a minute in cycles as the dividend
        ldx #4
@dendCpy:
        lda OneBpmWholeNote-1,x
        sta locDividend-1,x
        dex
        bne @dendCpy
        
        ; result: duration of a beat, in # cycles
        jsr div32
        
        ; copy to Tempo and Duration (reset to quarter-note)
        ldx #4
@quotCpy:
	lda locQuotient-1,x
        sta Tempo-1,x
        dex
        bne @quotCpy
        ldx #2 ; quarter-note
        jsr CalcDuration
        
        jmp SkipSpaces
@restoreSpec:
	lda StrRemainSaved
        sta StrRemain
        lda StrSaved
        sta StrSpec
        lda StrSaved+1
        sta StrSpec
        rts ; we'll fail in DoNextNote

DoNextNote:
	; Save the current position as the start
        ;  of the note spec, so we can print
        ;  it out later if there's a problem with it.
        lda #0
        sta IsRest
	lda StrSpec
        sta StrSaved
        lda StrSpec+1
        sta StrSaved+1
        lda StrRemain
        sta StrRemainSaved
        tsx
        stx StackSave
        
	jsr StrGetCur
        bne :+
        rts; end of string; done processing!
:
        
	jsr GetNoteBasePitch
        jsr AdjustForOctave
        
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
SkipSpaces:
	jsr StrGetNext
        beq @done
        cmp #' '
        beq SkipSpaces
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
        beq CalcDuration
        inx
        bne @lo
@rewind:
	jsr StrRewind
@nevermind:
	rts
CalcDuration:
	; Copy Tempo to Duration, and shift until we get our
        ;  duration.
        ldy #4
@cpyTempo:
	lda Tempo-1,y
        sta Duration-1,y
        dey
        bne @cpyTempo
        
        ; If we matched "W", x is 0, so we do no shifting,
        ;  as Tempo is already the duration of a whole note
        ; If we matched "Q", x is 2, so we shift twice
        ;  (divide by 4), giving us the duration of
        ;  a quarter note
        cpx #0
        beq @durShiftDone
@durShiftLoop:
	lsr Duration
        ror Duration+1
        ror Duration+2
        ror Duration+3
        dex
        bne @durShiftLoop
@durShiftDone:
        
        ; XXX check for triplet
        
        ; Check for "dotted"
        jsr StrGetNext
        cmp #'.'
        beq @dotted
	jmp StrRewind
@dotted:
	; XXX what about dotted triplet?
	; Halve the duration (value of just the dot)
        ;  and push result to stack
        ldx #0
        clc
@halveShift:
	lda Duration,x
        ror
        pha
        inx
        cpx #4
        bne @halveShift
        
        ; Now add it back to itself (= dotted value)
        ldx #3
        clc
@dottedAdd:
	pla ; the first of these is spurious...
        adc Duration,x
        sta Duration,x
        dex
        bpl @dottedAdd
        
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
        
        ; Divide Duration (length of note in cycles)
        ; by Pitch (length of a half-wave in cycles)
        ; to get # of iterations (number of times to
        ;   produce a half-wave until we've reached note length)

	; Copy Pitch as divisor
        lda #0
        sta locDivisor
        sta locDivisor+1
        lda Pitch
        ldy Pitch+1
        sta locDivisor+2
        sty locDivisor+3
        
        ; Now copy Duration as dividend
        ldx #3
@copyDur:
	lda Duration,x
        sta locDividend,x
        dex
        bpl @copyDur
        
        jsr div32 ; divide!
        
        ; Copy lower 16 bits of result to Iterations
        ldy locQuotient+2
        lda locQuotient+3
        sty Iterations
        sta Iterations+1
	rts

AdjustForOctave:
	jsr TryReadOctave
        ; Divide the pitch by 2 (shift right) for
        ;  each octave above 0.
        ldx Octave
        beq @doneShift
@shift:
	lsr Pitch
        ror Pitch+1
        dex
        bne @shift
@doneShift:
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
        ; did we exceed 
	rts
@flat:	; flat
	beq @wrap ; is our base note A? yes -> deal with wrap
	sbc #0 ; (carry unset; borrow is set)
        rts
@wrap:
	lda #11 ; base note was A (0), flat is Ab (11)
        rts
GetNoteBasePitch:
	cmp #'R'
        bne @notRest
        ; We're a rest. Mark that, then pretend we're "C"
        ;  for S&G.
        lda #$FF
        sta IsRest
        lda #'C'
@notRest:
        cmp #'H'
        bcs NoteNameErr ; > 'G', bail
        sec
        sbc #'A'
        bcc NoteNameErr ; < 'A', bail
        tax
        lda NoteSteps,x
        
        jsr AdjustAccidental
        
        ; At this point, we have our 12-step index
        ;  for the note.
        ; We're indexing words, not bytes, so double it
        asl
        tay
        lda BasePitches,y
        sta Pitch+1
        iny
        lda BasePitches,y
        sta Pitch
	rts
NoteNameErr:
	; handle note spec error
        ;bit AS_ERRFLG	; ON ERR turned on?
        ;bmi @noMsg ; yes; skip our message
        lda #<MsgBad
        ldy #>MsgBad
        jsr Print
        jsr PrintNoteSpec
        ; Unwind stack
        ldx StackSave
        txs
@noMsg:
	jmp SkipSpaces

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
        sta StrSpec
        lda StrSaved+1
        sta StrSpec+1
        ldx StrRemainSaved
        ldy StrRemain
        jsr StrRewind
@lo:
	jsr StrGetNext
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
        ;  take up... but we don't have infinite granularity
        ;  in our control over how long it actually takes
        ;  up.
        ; We know how to make half-waveforms that
        ;  are 74 + 27*N + C cycles long. We need to turn
        ;  Pitch into an N and C that equals the
        ;  original Pitch value.
        
        ; Check - is our Pitch lower than we can handle?
        lda Pitch
        bne @pitchOk
        lda Pitch+1
        cmp #101 ; 101 is the smallest number of cycles
        	 ;  we can manage
        bcs @pitchOk
        ; Set the [inverse] Pitch to the
        ;  minimum (highest possible actual pitch)
        lda #101
        sta Pitch+1
        
@pitchOk:
        ; First, subtract 76 from Pitch
        sec
        lda Pitch+1
        sbc #76
        sta SubIterations+1
        lda Pitch
        sbc #0 ; for borrow
        sta SubIterations
        
        ; Now divide that by 27
        lda SubIterations
        ldy SubIterations+1
        sta locDividend
        sty locDividend+1
        lda #0
        ldy #27
        sta locDivisor
        sty locDivisor+1
        
        jsr div16
        
        ; That's our "N".
        ; Convert to int and store in SubIterations
        lda locQuotient
        ldy locQuotient+1
        sta SubIterations
        sty SubIterations+1
        
        ; We interrupt this program to initialize VariableOps.
	ldx #0
@voInitLo:
	lda #$38 ; fill it with SEC ops to start
        sta VariableOps,x
        inx
        cpx #(VariableOpsEnd - VariableOps)
        bne @voInitLo
        
        ; Load remainder from division (guaranteed 1 byte) -
        ; that's our "C". Trim the VariableOps region
        ; accordingly.
        lda locRemainder+1
        ;
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
        
        ; handle rest / no rest
        bit IsRest
        bne @rest
        lda #$C0
        bne @store
@rest:
	lda #$20
@store:
        sta SpeakerLoc
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
	lda SubIterations+1
        sta PitchCtr+1
        lda SubIterations
        sta PitchCtr
SpeakerLoc = * + 2
        lda SS_SPKR
        sec
VariableOps:
	varOps
VariableOpsEnd:
@halfWave:
	sec			; 2
        lda PitchCtr+1		; +4 = 6
        sbc #1			; +2 = 8
        sta PitchCtr+1		; +4 = 12
        lda PitchCtr		; +4 = 16
        sbc #0			; +2 = 18
        sta PitchCtr		; +4 = 22
        cmp #$ff		; +2 = 24
        bne @halfWave		; +3 = 27
        ; ^ Does Pitch+1 iterations
        sec			; 2
 	lda Iterations+1	; +4 = 6
        sbc #1			; +2 = 8
        sta Iterations+1	; +4 = 12
        lda Iterations		; +4 = 16
        sbc #0 ; for borrow	; +2 = 18
        sta Iterations		; +4 = 22
        cmp #$ff		; +2 = 24
        bne DoSoundLoop		; +3 = 27
        ; ^ Does iter+1 iterations
        ; ea. iter = (26 + 27 + 20 + 3 + 4 = 76) + (Pitch * 27) + C cycles
        ;    (3 for req'd varOps branch, 4 for 1 req'd varOps sec, 1 req'd sec-or-bcc (for C's odd bit)
        ; PrepSoundLoop needs to re-adjust Pitch so the total equals
        ; the desired value of *cycles*, rather than iterations
        ; in the "halfWave" loop
	rts

div32:
	makeDivisionRoutine 4
div16:
	makeDivisionRoutine 2

ProgramEnd:
