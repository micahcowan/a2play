all: TONE.DSK

TONE.DSK: Makefile ORIG.DSK PLAY.TOK MPLAY.TOK tone mtone
	@echo
	@echo '*** Making $@ ***'
	set -e; \
	trap 'rm TONE.DSK' EXIT; \
	cp ORIG.DSK $@; \
	\
	dos33 -y $@ SAVE A PLAY.TOK PLAY; \
	dos33 -y $@ SAVE A MPLAY.TOK MPLAY; \
	dos33 -y $@ BSAVE -a '0x300' tone TONE; \
	dos33 -y $@ BSAVE -a '0x4000' mtone MTONE; \
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
