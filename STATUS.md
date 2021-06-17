# DG/Emua Status

## Physical Emulator - mvemua

* Last status update: 2nd May 2021
* Last significant progress: FIXUP produces UNKNOWN ERROR CODE 2 - that's further than mvemg gets! - 24th April 2021

### What Works?
* File 0 - TBOOT - Appears to be working - 10th April 2021
* File 2 - DFMTR - Runs with surface analysis to completion on 6061 disk - 18th April 2021
* File 3 - INSTL - Runs to completion - 19th April 2021
  
### What's Next?
Initial milestones are to run from image of AOS/VS 7.73 system tape image...
* ~~File 0 - TBOOT - Appears to be working - 10th April 2021~~
* ~~File 2 - DFMTR - Runs with surface analysis to completion on 6061 disk - 18th April 2021~~
* ~~File 3 - INSTL - Runs to completion - 19th April 2021~~
* File 1 - FIXUP - produces UNKNOWN ERROR CODE 2 - that's further than mvemg gets!- 24th April 2021
* File 1 - PCOPY - Loops with 'Fatal disk error' even though disk is not being accessed - same as mvemg - 2nd May 2021

## Virtual (AOS/VS) Emulator - vsemua

* Last status update: 10 Jun 2021
  
The following 32-bit sample programs copied from a physical machine are working...
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
  | 21 (BOOTER.PR)    | Unmapped write in XWSTA                               |  5 Jun 2021 | Same :-/ |      
  | HANGMAN (IMSUTIL) | Bad comparison in WDCMP after displaying start screen | 28 May 2021 | Same :-/ | 
  | QUEST_SERVER      | Loop after ?GTOD                                      |  5 Jun 2021 | Better :-) |
  | WUMPUS            | Hang/loop after displaying start screen               | 24 May 2021 | Same :-/ | 
  | YAHTZEE           | Unmapped read in XNLDA                                | 10 Jun 2021 | Better :-) |
  
### Reminders
* ~~Check ?READ/WRITE/OPEN flag mode interpretation - it might be back-to-front~~
* Is there an off-by-one in byte handling somewhere - could explain display glitches
* Probably need to specify both CWD and virtual root via command and handle them better. I think we need...
  * -root < location of virtual fs> 
  * -dir <AOS/VS working dir in vfs> 
  * -pr <AOS/VS path to PR> (either absolute - starting with : - or relative to working dir)

