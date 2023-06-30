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
.elseif 0
	; Major scales test
        lineP "MAJOR SCALES IN C"
        line "& PLAY,",'"',"T180",'"',": REM SET TEMPO"
        line "FOR OCTAVE=2 TO 6"
        line "& PLAY,",'"',"C",'"'," + STR$(OCTAVE) + ",'"',"E D E F G A B",'"'
        line "NEXT OCTAVE"
        line "& PLAY,",'"',"C7",'"'
        scrcode "RUN",$0D
.elseif 1
	; Sailor's hornpipe
	line "& PLAY,",'"',"T280"
        line "& PLAY,",'"',"GE G- GQ C C GE F E G C5 B4 C5Q",'"'
        scrcode "RUN",$0D
.elseif 0
	line "& PLAY,",'"',"T240"
        line "& PLAY,",'"',"C4E R G3 R C4 R E R CH. G3E R C4",'"'
        scrcode "RUN",$0D
.elseif 0
	; sound effect
	line "& PLAY,",'"',"T1600"
        line "& PLAY,",'"',"C4E E G C5 G4 E C",'"'
        line "& PLAY,",'"',"E G C5 G4 E C",'"'
        line "& PLAY,",'"',"E G C5 G4 E C",'"'
        scrcode "RUN",$0D
.elseif 0
	; dotted
	line "& PLAY,",'"',"T240"
        line "& PLAY,",'"',"C4E R G3 R C4 R E R CH. G3E R C4",'"'
        scrcode "RUN",$0D
.elseif 1
	; triplets
	line "& PLAY,",'"',"T120"
        line "& PLAY,",'"',"G3E3 F# G C4Q B3E B- AE3 G+ A D4Q",'"'
        line "& PLAY,",'"',"D-E C B3E3 A# B E4Q E-E D CH.",'"'
        scrcode "RUN",$0D
.endif
ASoftEnd: