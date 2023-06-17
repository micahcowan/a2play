# a2play
Music-playing utility code for use from AppleSoft on Apple ][ machines

Welcome to **a2play**! The aim of this project is to provide a *user-friendly* musical tones-generating interface, for use from AppleSoft BASIC programs running on an Apple II computer (Apple, Inc.'s first computer line, from before the Macintosh).

[**Click the video link below, to see it in action!<br />
![a2play showcase video](https://img.youtube.com/vi/zDNLja04yko/0.jpg)](https://www.youtube.com/watch?v=zDNLja04yko)

## Usage

The tool's music description language is based on Hypertalk's *play* command, [described here](https://www.hypercard.center/HyperTalkReference/play).

There are some differences from the Hypertalk language. For one, uppercase note names are supported. For another, when using Apple II models that don't support lowercase, using `eb` to designate E-flat, looks like `EB` and is somewhat less intuitive. As an alternative, `-` is also accepted (in addition to `b`) for flats, and for symmetry, `+` is accepted in place of `#` for sharps.

As of the current release version, v0.9, using `3` in the note duration to designate "triplets" is unsupported. It will be added in a future release.

Since musical state persists across programs, it is highly recommended to explicitly specify the tempo (defaults to 180), and the duration of the first note.

## Issues

As mentioned, the use of `3` in the duration for triplets is not yet supported.

In general, the calculations performed for each note are a bit too time-consuming at the moment, which interferes with both rhythm and tempo - the effect on rhythm is particularly pronounced at high tempos. A future release will aim to remedy, or at least work around, this problem.

## Building notes

If you want to modify or build from these sources, you will need tools from the following projects:

  * The ca65 and ld65 tools from [the cc65 project](https://github.com/cc65/cc65)
  * Vince Weaver's [dos33fsprogs](https://github.com/deater/dos33fsprogs)

NOTE: The **dos33fsprogs** project contains *many* different subprojects, most of which are *not needed* to build `fnord.dsk`. The only subdirectories you must build, are `utils/dos33fs-utils`, `utils/prodos-utils`, and `utils/asoft_basic-utils`.

a2play's Makefile assumes all of these tools are accessible from the current `PATH` environment variable.
