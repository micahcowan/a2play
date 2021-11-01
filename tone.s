        PITCH  = $7
        SPKR   = $C030
        .org $300
START:  LDY #0
FREQ:   DEX
CONT:   BNE FREQ
TWEAK:  LDA SPKR
        LDX PITCH
        DEY
        BNE FREQ
DONE:   RTS

        BRK
        LDA #$FC
        STA STO+1
        LDA #$FF
        STA PITCH
        JSR START
        LDA #$FF
STO:    LDX #$F2
        JSR CONT
