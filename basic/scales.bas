10 PRINT "MAJOR SCALES IN C"
20 & PLAY,"T180":REM SET TEMPO
30 FOR OCTAVE=2 TO 6
40 & PLAY,"C"+STR$(OCTAVE)+"E D E F G A B"
50 NEXT OCTAVE
60 & PLAY,"C7"
70 FOR OCTAVE=6 TO 2 STEP -1
80 & PLAY,"B"+STR$(OCTAVE)+" A G F E D C"
90 NEXT OCTAVE
200 END
