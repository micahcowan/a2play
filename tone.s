        COUNTR = $6
        PITCH  = $7
        SPKR   = $C030
        .org $300
TWEAK:  LDA SPKR
        LDX PITCH
COUNT:  DEY
        BNE FREQ
        DEC COUNTR
        BEQ DONE
FREQ:   DEX
        BNE COUNT
        BEQ TWEAK
DONE:   RTS
