all: mtone TONE.DSK

TONE.DSK: Makefile ORIG.DSK PLAY.TOK tone
	@echo
	@echo '*** Making $@ ***'
	set -e; \
	trap 'rm TONE.DSK' EXIT; \
	cp ORIG.DSK $@; \
	\
	dos33 -y $@ SAVE A PLAY.TOK PLAY; \
	dos33 -y $@ BSAVE -a 300 tone TONE; \
	\
	trap - EXIT
	@echo

PLAY.TOK: PLAY.BAS
	tokenize_asoft < $< > $@ || { rm -f $@; exit 1; }

tones: tones.c
	gcc -o tones -lm tones.c
	./tones

.s.o:
	ca65 -o $@ $<

tone: tone.o
	ld65 -t none -o $@ $<

mtone: mtone.o
	ld65 -t none -o $@ $<
