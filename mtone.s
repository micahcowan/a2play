SPKR = $C030
COUNTR = $6
PITCHLO = $7
PITCHHI = $8
;CPYLO   = $1D
CPYHI   = $1E

        .org $4000
START:  LDY #0
REPT:
        LDA PITCHHI
        STA CPYHI
        INC CPYHI
        LDX PITCHLO
        INX
FREQ:   DEX
        BNE FREQ
        DEC CPYHI
        BNE FREQ
TWEAK:  LDA SPKR
        DEY
        BNE REPT
DONE:   RTS

        BRK
        LDA #$FF
        STA PITCHLO
        LDA #$00
        STA PITCHHI
        JSR START
        LDY #$80
        JSR REPT
        RTS
