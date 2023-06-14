;#define CFGFILE play.cfg
;#link "load-and-run-asoft-basic.s"
;#link "play-basic.s"
;#link "play.s"
;#resource "basic-utils.inc"
;#resource "play.cfg"
;#resource "apple2.rom"


.macpack apple2

.include "a2-monitor.inc"

.import LoadAndRunBasic
.import InstallAmperPlay

.segment "STARTUP"
start:
	jsr InstallAmperStub
        jsr InstallAmperPlay
	jmp LoadAndRunBasic

.segment "CODE"

InstallAmperStub:
	lda #$4c
        sta AS_AMP
        lda #<AmperStub
        sta AS_AMP+1
        lda #>AmperStub
        sta AS_AMP+2
	rts

AmperStub:
	ldy #0
@lo:
        lda AmperMsg,y
        beq @out
        jsr Mon_COUT
        iny
        bne @lo
@out:
	; Read until : or \0
        jsr AS_CHRGOT
@lo2:
        beq @end
        cmp #':'
        beq @end
        eor #$80
        jsr Mon_COUT
        jsr AS_CHRGET
        jmp @lo2
@end:
	lda #$8D
        jmp Mon_COUT
	;rts

AmperMsg:
	scrcode "AMPERSAND!! "
        .byte $00