.include "basic-utils.inc"

.export ASoftProg, ASoftEnd

ASoftProg:
	lineP "PLAYING MUSIC..."
        line "A$ = ",'"',"A ",'"'
        line "Q$ = ",'"',"Q ",'"'
        line "& PLAYE, A$+",'"',"C",'"',": ?",'"',"REST",'"'
FIX_LINE .set LINE_NUMBER
	line "& PLAY, A$+Q$+",'"',"C",'"',": ?",'"',"REST",'"'
        lineP "AFTER"
        scrcode "RUN", $0D
        scrcode "Q$=",'"','"',":GOTO ",.sprintf("%d",FIX_LINE),$0D
ASoftEnd: