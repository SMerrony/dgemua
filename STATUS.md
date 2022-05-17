# DG/Emua Status

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
* ~~Implement PIT~~ - 3rd April 2022

### Missing Information and Assumptions
As mentioned elsewhere, I have limited available documentation - some of which is of dubious accuracy.

I do not know what 'reserved' devices No. 1, 2, and 3 were.  I am currently treating access to them as no-ops.

Non-flagged PIO access to the (U)PSC is not documented.  I am currently treating them as no-ops.

DIx and DOx instructions to the BMC/DCH controller seem to be undocumented for the MV-series, but see p.5-82 of the Eclipse S/140 Programmers Reference.  In fact, this may be a red herring - the MV/2500 DC did not support BMC... it had a special HIP bus instead.

## Virtual (AOS/VS) Emulator - vsemua

* Last status update: 17 Apr 2022
  
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

  |    Game           |   Problem                                             |   Date      | cf. VS/Emug |
  |-------------------|-------------------------------------------------------|-------------|-------------|
  | 21 (BOOTER.PR)    | Unmapped write in XWSTA                               |  5 Jun 2021 | Same   :-/ | 
  | CHESS             | Prompts for settings but won't accept choice          | 17 Apr 2022 | Better :-) |
  | DND               | 'Error 71697. on file CONSOLE'                        | 28 Oct 2021 | Same   :-/ |
  | EMPIRE            | 'Error 71683. on file FILE_IN'                        | 16 Apr 2022 | Worse  :-( |
  | EMPIRE2T          | 'Error 71683. on file FILE_IN'                        | 22 Jun 2021 | Same   :-/ |
  | FERRET            | WEDIT not yet implemented (!)                         | 19 Jun 2021 | Better :-) |
  | FISH              | Unimplemented sys call ?GLIST                         | 16 Apr 2022 | Same   :-/ |
  | FOOBAR            | 'Error 11381. from line 15' maybe ?GCHR problem       | 28 Oct 2021 | Worse  :-( |
  | HANGMAN (IMSUTIL) | 'Error 71231' - maybe doesn't like ?UIDSTAT response  | 17 Apr 2022 | Better :-) | 
  | MMM               | 'Error 11381. from line...'                           | 16 Apr 2022 | Better :-) |
  | QUEST_SERVER      | Loop after ?GTOD                                      |  5 Jun 2021 | Better :-) |
  | QUEST             | Welcome message, prompt for initials, ?RETURN. (server was not running) | 16 Apr 2022 | Better :-) |
  | SCRABBLE          | 'Error 71692' after ?OPENing SEED file                | 19 Jun 2021 | Same   :-/ |
  | WUMPUS            | Hang/loop after displaying start screen               | 24 May 2021 | Same   :-/ | 
  | YAHTZEE           | Unmapped read in XNLDA                                | 10 Jun 2021 | Better :-) |
  | ZORK              | 'Error 71683. on file heap_ro'                        | 28 Oct 2021 | Same   :-/ |
  
### Reminders
* ~~Check ?READ/WRITE/OPEN flag mode interpretation - it might be back-to-front~~
* Is there an off-by-one in byte handling somewhere - could explain display glitches
* We have the FORTRAN source for FOOBAR

Error Codes to Look-Up...
* 11381 - 0165?
* 71199 - 037?
* 71231 - 077?
* 71683 - 3?
* 71692 - 014?
* 71697 - 021?

