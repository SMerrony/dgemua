# VS/Emua User Guide
VS/Emua is a basic user-level AOS/VS emulator designed to run Ring 7 user programs and contribute to the enhancement
of the bare-metal MV/Emua machine emulator.

## Build
`gprbuild -Pvsemua`

## Run

`./vsemua -pr LOOPS4.PR -root FILESYSTEM  -dir :SAMPLES`

You must specify...
* The root of the virtual (AOS/VS) filesystem as a local path
* The AOS/VS working directory as a complete AOS/VS path from the root
* The absolute or relative AOS/VS name of the program file to run, in AOS/VS format

Note that program names beginning with a colon are considered absolute and must contain a valid AOS/VS path from the 
virtual root.  Conversely, program names without a leading colon are looked for relative to the AOS/VS directory specified via the `-dir` option.  Thus, these two invocations are equivalent:

1. `./vsemua -pr LOOPS4.PR -root FILESYSTEM  -dir :SAMPLES`
2. `./vsemua -pr :SAMPLES:LOOPS4.PR -root FILESYSTEM  -dir :SAMPLES`

After starting the emulated program you must connect a DASHER-compatible terminal emulator to port 10000 (default).
[DasherG](https://github.com/SMerrony/DasherG) is known to work.
