all: tone mtone

tones: tones.c
	gcc -o tones -lm tones.c
	./tones

.s.o:
	ca65 -o $@ $<

tone: tone.o
	ld65 -t none -o $@ $<

mtone: mtone.o
	ld65 -t none -o $@ $<
