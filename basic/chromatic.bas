10 PRINT "CHROMATIC SCALES"
20 REM READ IN THE NS OF A CHROMATIC SCALE
25 DIM N$(12)
30 FOR I=1 TO 12
40 READ N$(I)
50 NEXT I
100 REM CONSTRUCT ONE OCTAVE
110 REM GOING CHROMATICALLY UPWARD,
120 REM SKIPPING C
150 U$=""
160 FOR I=2 TO 12
170 U$=U$+N$(I)+" "
180 NEXT I
200 REM NOW CONSTRUCT ONE OCTAVE
210 REM GOING CHROMATICALLY DOWNARD,
220 REM SKIPPING B
250 D$=""
260 FOR I=11 TO 1 STEP -1
270 D$=D$+N$(I)+" "
280 NEXT I
300 REM NOW PUT IT ALL TOGETHER!
320 & PLAY,"T300":REM SET TEMPO
330 FOR OCTAVE=2 TO 6
340 & PLAY,"C"+STR$(OCTAVE)+"E"
345 & PLAY,U$
350 NEXT OCTAVE
360 & PLAY,"C7"
370 FOR OCTAVE=6 TO 2 STEP -1
380 & PLAY,"B"+STR$(OCTAVE)
385 & PLAY,D$
390 NEXT OCTAVE
400 END
1000 DATA C,C#,D,D#,E,F,F#,G,G#,A,A#,B