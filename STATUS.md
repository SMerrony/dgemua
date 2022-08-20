# DG/Emua Status

As of 25 May 2022 this emulator equals or surpasses my previous Go version in all ways.
However, it is still a fair way from being generally useful.

There are two emulators: one attempts to emulate physical hardware, the other tries to
provide a user-level emulation of a virtual AOS/VS environment.  The emulators share
some core code - notably the CPU (processor) itself.

Due to lack of freely-available software and documentation for these historically
important machines, the virtual emulation is more advanced.

## Physical Emulator - mvemua

* Last status update: 16th April 2022
* Last significant progress: DSKBT loads from type 6239 disk - 2nd April 2022

### What Works?
* File 0 - TBOOT - Appears to be working - 10th April 2021
* File 2 - DFMTR - Runs with surface analysis to completion on 6061 disk - 18th April 2021
* File 2 - DFMTR - Runs with surface analysis to completion on 6239 disk - 31st March 2022
* File 3 - INSTL - Runs to completion on 6061 disk - 19th April 2021
* File 3 - INSTL - Runs to completion on 6239 disk - 31st March 2022
* B 24   - DSKBT - Displays Operating System Load Menu, crashes accessing mystery device - 12th April 2022
  
### What's Next?
Initial milestones are to run from image of AOS/VS 7.73 system tape image...
* ~~File 0 - TBOOT - Appears to be working - 10th April 2021~~
* ~~File 2 - DFMTR - Runs with surface analysis to completion on 6061 disk - 18th April 2021~~
* ~~File 3 - INSTL - Runs to completion - 19th April 2021~~
* File 1 - FIXUP - Produces ERROR ~71231.~ 71199. - that's further than mvemg gets! - 18th March 2022
* File 1 - PCOPY - Aborts with "Incorrect disk format revision number" - further than mvemg got! - 22nd March 2022

### Missing Information and Assumptions
As mentioned elsewhere, I have limited available documentation - some of which is of dubious accuracy.

I do not know what 'reserved' devices No. 1, 2, and 3 were.  I am currently treating access to them as no-ops.

Non-flagged PIO access to the (U)PSC is not documented.  I am currently treating them as no-ops.

DIx and DOx instructions to the BMC/DCH controller seem to be undocumented for the MV-series, but see p.5-82 of the Eclipse S/140 Programmers Reference.  In fact, this may be a red herring - the MV/2500 DC did not support BMC... it had a special HIP bus instead.

## Virtual (AOS/VS) Emulator - vsemua

* Last status update: 20 Aug 2022
* Last significant progress: MMM runs okay - 20 Aug 2022
  
All of the following 32-bit sample programs copied from a physical machine are working...
* HW.PR - Hello, World! (13 May 2021)
* HW2.PR - Hello, World! using CLI return message (13 May 2021)
* LOOPS1.PR - Basic looping constructs (13 May 2021)
* LOOPS2.PR - Further loops (13 May 2021)
* LOOPS3.PR - Loops with LWDO and -ve values (14 May 2021)
* LOOPS4.PR - As LOOPS3 with external subroutines (14 May 2021)
* SPIGOT.PR - Calculate Pi to a thousand places using the spigot method (14 May 2021)
* STRINGTESTS.PR - Various string-handling routines (14 May 2021)
* TIMEOUT.PR - Uses ?GTMES and ?WDELAY to pause for n seconds (18 May 2021)

The NADGUG library provides a good range of freely-available test targets...
  
* 32-bit NADGUG Games compiled for AOS/VS

  |    Game           |   Status                                              |   Date      |  Issue  |
  |-------------------|-------------------------------------------------------|-------------|---------|
  | 21 (BOOTER.PR)    | Unmapped write in XWSTA                                   |  5 Jun 2021 | ???              |
  | CHESS             | Prompts for options then unimplemented syscall 030 ?RUNTM |  1 Jun 2022 | ?RUNTM           |
  | DND               | Run for a while then FP overflow on WFFAD                 | 19 Aug 2022 | ?READ (extended) |
  | EMPIRE            | Prints version then unimplemented syscall 077 ?FSTAT      |  9 Aug 2022 | ?FSTAT           |
  | EMPIRE1           | Corrupted '@OUTPUT @' filename                            | 30 May 2022 | ???              |
  | EMPIRE2T          | Prompts for save file then unimplemented syscall 077 ?FSTAT | 12 Jun 2022 | ?FSTAT           |
  | FERRET            | unknown multiprocessor instruction #87A9                  | 14 Aug 2022 | ???              |
  | FISH              | EAGLE_Op instruction ENQH not yet implemented             | 18 Aug 2022 | ENQH             |
  | FOOBAR            | *Runs to completion - seems to work perfectly :-)*        | 14 Aug 2022 |                  |
  | HANGMAN (IMSUTIL) | Prints instructions then unimplemented syscall 401 ?SDLM  |  2 Jun 2022 | ?SDLM            |
  | MMM               | *Seems to be working :-)*                                 | 20 Aug 2022 |                  |
  | OTHELLO (IMSUTIL) | *Runs to completion - seems to work perfectly :-)*        |  2 Jun 2022 |                  |
  | QUEST_SERVER      | Loop after ?GTOD                                          |  5 Jun 2021 | ???              |
  | QUEST             | Welcome, prompt for initials, ?RETURN. (server was not running) | 16 Apr 2022 |      |
  | SCRABBLE          | ?WRITE non-DS NYI                                         |  6 Aug 2022 | ?WRITE issue     |
  | WUMPUS            | Won't accept input - extended D/S ?READ with scr mgmt pkt | 10 Jun 2022 | ?READ (extended) |
  | YAHTZEE           | Won't accept input - extended D/S ?READ with scr mgmt pkt | 14 Aug 2022 | ?READ (extended) |
  | ZORK              | 'Heap version not compatible'                             | 10 Jun 2022 | ??? |

* Other 32-bit NADGUG Programs

  | Program     | Problem                                                     | Date        |  Issue  |
  |-------------|-------------------------------------------------------------|-------------|---------|
  | EMACS.PR    | 'ERROR 71187'                                               | 26 May 2022 | ???     |
  | LOOK.PR     | ?REC NYI                                                    | 15 Jun 2022 | ?REC    |
  
### Reminders
* We have the FORTRAN source for FOOBAR and WUMPUS, and the C code for EMACS
* For the moment, the initial searchlist is set to the working directory

Error Codes to Look-Up...
* 11381 - F77ERMES...
* 71199 - LANG_RT...
* 71231 
* 71683 
* 71692 
* 71697 

