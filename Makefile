AS=ca65
LD=ld65

BASIC_FILES:=$(wildcard basic/*.bas)
TOKENIZED_FILES:=$(subst .bas,.tok,$(BASIC_FILES))

all: PLAY.dsk
# Convenience to copy to where my emulator will find it
ifdef PLAYCOPY
	cp $< ~/dl/
endif

%.tok: %.bas
	tokenize_asoft < $< > $@ || { rm $@; exit 1; }

PLAY.dsk: PLAY.BIN empty.dsk $(TOKENIZED_FILES) Makefile
	cp empty.dsk WIP.dsk
	dos33 -a '0x8000' WIP.dsk BSAVE PLAY.BIN
	for n in $(TOKENIZED_FILES); do \
	    m=$${n##*/}; m=$${m%.tok}; \
	    dos33 -y WIP.dsk SAVE A $$n \
	    	$$(echo $$m | tr '[:lower:]' '[:upper:]'); \
	done
	mv WIP.dsk $@

PLAY.BIN: play.o play.cfg
	ld65 --config play.cfg -o $@ play.o
