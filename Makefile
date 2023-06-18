AS=ca65
LD=ld65

BASIC_FILES:=$(wildcard basic/*.bas)
TOKENIZED_FILES:=$(shell echo $(subst .bas,.tok,$(BASIC_FILES)))

all: PLAY.dsk PLAY-PRODOS.po
# Convenience to copy to where my emulator will find it
ifdef PLAYCOPY
	cp $^ ~/dl/
endif

%.tok: %.bas
	tokenize_asoft < $< > $@ || { rm $@; exit 1; }

PLAY.dsk: PLAY.BIN empty.dsk $(TOKENIZED_FILES) Makefile
	cp empty.dsk WIP.dsk
	dos33 -a '0x8000' WIP.dsk BSAVE PLAY.BIN
	set -e; \
	for n in $(TOKENIZED_FILES); do \
	    m=$${n##*/}; m=$${m%.tok}; m=$$(echo $$m | tr '[:lower:]' '[:upper:]'); \
	    dos33 -y WIP.dsk SAVE A $$n $$m; \
	done
	mv WIP.dsk $@

PLAY-PRODOS.po: PLAY.BIN\#068000 prodos_242.po $(TOKENIZED_FILES) Makefile
	cp prodos_242.po PD-WIP.po
	# Remove two-byte length header from tokenized files
	set -e -x; \
	for n in $(TOKENIZED_FILES); do \
	    m="$${n}.pd"; \
	    rm -f $$m; \
	    dd bs=1 if=$$n of=$$m skip=2; \
	done
	mv basic/hello.tok.pd basic/startup.tok.pd
	prodos PD-WIP.po SAVE -t BIN -a 0x8000 PLAY.BIN\#068000 PLAY.BIN
	#cadius ADDFILE PD-WIP.po PLAY/ PLAY.BIN\#068000
	set -e -x; \
	for n in basic/*.tok.pd; do \
	    m="$${n%.tok.pd}"; m=$${m#basic/}; m=$$(echo $$m | tr '[:lower:]' '[:upper:]'); \
	    prodos PD-WIP.po SAVE -t BAS $$n $$m; \
	done
	mv PD-WIP.po $@

PLAY.BIN: play.o play.cfg
	ld65 --config play.cfg -o $@ play.o

PLAY.BIN\#068000: pdplay.o play.cfg
	ld65 --config play.cfg -o $@ pdplay.o

pdplay.o: play.s
	ca65 -DPRODOS -o $@ $<
