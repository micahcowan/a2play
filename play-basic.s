.include "basic-utils.inc"

.export ASoftProg, ASoftEnd

ASoftProg:
	lineP "PLAYING MUSIC..."
.if 0
        line "A$ = ",'"',"A# ",'"'
        line "Q$ = ",'"',"Q ",'"'
        line "& PLAYE, A$+",'"',"C",'"',": ?",'"',"REST",'"'
FIX_LINE .set LINE_NUMBER
	line "& PLAY, A$+Q$+",'"',"C",'"',": ?",'"',"REST",'"'
        lineP "AFTER"
        scrcode "RUN", $0D
        scrcode "Q$=",'"','"',":GOTO ",.sprintf("%d",FIX_LINE),$0D
.elseif 0
	line "& PLAY,",'"',"C D E F G A B C5",'"'
        scrcode "RUN",$0D
.elseif 1
	line "& PLAY,",'"',"T200 GE G- GQ C C GE F E G C5 B4 C5Q",'"'
        scrcode "RUN",$0D
        scrcode "CALL-151",$0D
.elseif 1
	line "& PLAY,",'"',"T400 C4E R G R C4 R E R CH. G3E R C4",'"'
        scrcode "RUN",$0D
.endif
ASoftEnd: