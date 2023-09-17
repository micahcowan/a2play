# a2play
Music-playing utility code for use from AppleSoft on Apple ][ machines

Welcome to **a2play**! The aim of this project is to provide a *user-friendly* musical tones-generating interface, for use from AppleSoft BASIC programs running on an Apple II computer (Apple, Inc.'s first computer line, from before the Macintosh). By playing music at high tempos, it is also possible to produce sound effects for gaming.

[**Click the video link below, to see it in action!<br />
![a2play showcase video](https://img.youtube.com/vi/zDNLja04yko/0.jpg)](https://www.youtube.com/watch?v=zDNLja04yko)

You can also [try it out](https://8bitworkshop.com/redir.html?repo=micahcowan%2Fa2play&platform=apple2&file=play-start.s) and play with the code, at 8bitworkshop.com.

## Usage

### Using in your AppleSoft BASIC programs

Here is a BASIC program that produces a few bars of music using **a2play**:

```
10 PRINT CHR$(4);"BLOAD PLAY.BIN":REM Load a2play into memory
20 CALL 32768:REM Install and activate a2play's &-handler
30 & PLAY,"T240"
40 & PLAY,"C4Q. DE EQ G GQ. AE GQ E CQ. DE EQ E D C DH"
50 REM You can use string variables and expressions, too!
60 M$="C4Q. DE EQ G GQ. AE GQ E CQ. DE EQ E"
70 & PLAY,M$+" D D CW"
100 END
```

The music language used by the `& PLAY,` command is designed to be user friendly. The first command, `"T240"`, sets the song's tempo
to 240 bpm. The first note played is in the second command string: `"C4Q"`. The *C* designates the name of the note we're playing (this is always required to play a note), and the *4* denotes which octave we're playing in (*C4* is "middle C"). The *Q* stands for *quarter note*, and the dot that follows after it indicates that we want to play a *dotted quarter note* (a beat-and-a-half long). Octaves and note-lengths can be left off, in which case new notes will use the same values as the previous note did. Sharps and flats can be designated like *a#* or *ab*, or alternatively by *A+* or *A-*, and rests use the special note-name *R*.

You can either choose to set up `PLAY.BIN` at the start of each of your programs that use **a2play**, or you can do the setup just once at boot (in your `STARTUP` (ProDOS) or `HELLO` (DOS) script), and assume it's available for use in the actual music-playing programs (this is the approach taken in the release disk image files).

Numerous music-playing examples are available in the release disk - play around with it and see what you can do!

### Language

The tool's music description language is based on Hypertalk's **play** command, [described here](https://www.hypercard.center/HyperTalkReference/play).

There are some differences from the Hypertalk language. For one, uppercase note names are supported. For another, when using Apple II models that don't support lowercase, using `eb` to designate E-flat, looks like `EB` and is somewhat less intuitive. As an alternative, `-` is also accepted (in addition to `b`) for flats, and for symmetry, `+` is accepted in place of `#` for sharps.

Also, it's worth noting that HyperTalk's original **play** command language actually accepted a great deal more flexibility in note specifications than is documented: for instance, in **fq g.**, the **g** would have the value of a dotted quarter note (dotted quaver), even though the **q** for **quarter note** had been left out. In a specification like **ce8**, the **8** was accepted as an octave, even though it comes *after* the specified duration. While those patterns were accepted by original HyperTalk (despite violating the documented specification), they are *not* accepted by **a2play**. Note name first, followed (optionally) by octave, followed (optionally) by note duration, with triplet and/or dotted notation trailing at the very end.

Since musical state persists across programs (**a2play** doesn't know when one program has ended and another started), it is highly recommended to explicitly specify the tempo (defaults to 180). When the tempo is set, the note duration is reset to quarter-note.

## Details, Issues, and Quirks

Middle C is *c4*. It is succeeded by *d4*, and preceded by *b3*. Octaves begin with C.

In the passage *c4 b c*, the *b* is a major 7th above the (middle) *c*. Note that, in *c4 cb c*, the *cb* (C&flat;) is *still* a major 7th above the *c*. Likewise, in *b4 b# b*, the *b#* will sound a major 7th *below* the *b*. The rule to remember, is that C&flat; is treated *exactly* like a B, and B&sharp; exactly like a C, and they wrap across the current octave in exactly the same way. I don't know if this corresponds to how HyperTalk does it, my guess would be that it doesn't.

Usable octaves are mainly in the range of 2 to 7, with a few of the ones at the top of octave 1, and a couple at the bottom of octave 8 being legible. Octaves below that range continue to produce distinct tones, but your ear hears them only as buzzes. Meanwhile, notes above *d#8* are accepted, but are locked to the same pitch as *d#8* (the tone-generating routine is incapable of looping fast enough to produce higher tones than ~4.7 kHz).

Very high-pitched, long-duration notes at relatively low tempos will be played way too short (due to overflowing a 16-bit duration-related counter). Fixing this would reduce the supported range of pitches (across all durations), so a fix is not planned. For example, a whole-note *d#8w* (highest possible pitch) played at or below a tempo of 36 bpm will be played far shorter than it should be. A quarter-note at that pitch would have to be played at a tempo of about 9 bpm or lower (in both cases, the ear-piercingly high note is sustained for 6 or 7 seconds). A whole-note *c7w* is good until you drop the tempo to 15bpm or lower. Our recommendation is not to play ear-piercing pitches at long note-lengths.

Doubly-dotted notes are not currently supported. There is also currently no way to "tie" note-lengths, so a note lasting the duration of a half-note plus is not possible (but you can get close by simply playing an eight-note after a half-note)

## Building notes

If you want to modify or build from these sources, you will need tools from the following projects:

  * The ca65 and ld65 tools from [the cc65 project](https://github.com/cc65/cc65)
  * Vince Weaver's [dos33fsprogs](https://github.com/deater/dos33fsprogs)

NOTE: The **dos33fsprogs** project contains *many* different subprojects, most of which are *not needed* to build `fnord.dsk`. The only subdirectories you must build, are `utils/dos33fs-utils`, `utils/prodos-utils`, and `utils/asoft_basic-utils`.

a2play's Makefile assumes all of these tools are accessible from the current `PATH` environment variable.
