        PITCH  = $7
        SPKR   = $C030
        .org $300
        LDY #0
FREQ:   DEX
        BNE FREQ
TWEAK:  LDA SPKR
        LDX PITCH
        DEY
        BNE FREQ
DONE:   RTS
