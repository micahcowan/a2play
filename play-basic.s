.include "basic-utils.inc"

.export ASoftProg, ASoftEnd

ASoftProg:
	lineP "PLAYING MUSIC..."
        line "A$ = ",'"',"A ",'"'
        line "& PLAYE, A$+",'"',"C",'"',": ?",'"',"REST",'"'
        line "& PLAY, A$+",'"',"C",'"',": ?",'"',"REST",'"'
        lineP "AFTER"
        scrcode "RUN", $0D
ASoftEnd: