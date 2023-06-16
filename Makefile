all: TONE.DSK

TONE.DSK: Makefile ORIG.DSK PLAY.TOK MPLAY.TOK CMP.TOK tone mtone
	@echo
	@echo '*** Making $@ ***'
	set -e; \
	trap 'rm TONE.DSK' EXIT; \
	cp ORIG.DSK $@; \
	\
	dos33 -y $@ SAVE A PLAY.TOK PLAY; \
	dos33 -y $@ SAVE A MPLAY.TOK MPLAY; \
	dos33 -y $@ SAVE A CMP.TOK CMP; \
	dos33 -y $@ BSAVE -a '0x300' tone TONE; \
	dos33 -y $@ BSAVE -a '0x4000' mtone MTONE; \
	\
	trap - EXIT
	@echo

CMP.TOK: CMP.BAS
	tokenize_asoft < $< > $@ || { rm -f $@; exit 1; }

PLAY.TOK: PLAY.BAS
	tokenize_asoft < $< > $@ || { rm -f $@; exit 1; }

MPLAY.TOK: MPLAY.BAS
	tokenize_asoft < $< > $@ || { rm -f $@; exit 1; }

tones: tones.c
	gcc -o tones tones.c -lm
	./tones

.s.o:
	ca65 --listing $(subst .o,.list,$@) -o $@ $<

tone: tone.o
	ld65 -t none -o $@ $<

mtone: mtone.o
	ld65 -t none -o $@ $<
