-- MIT License

-- Copyright (c) 2021 Stephen Merrony

-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:

-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.

-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.

-- Adapted from parts of AOS/VS 7.73 PARU.32.SR definitions file

with Interfaces; use Interfaces;

with DG_Types;   use DG_Types;

package PARU_32 is

-- System Error Codes
	ERICM : constant Word_T := 8#01#;   -- ILLEGAL SYSTEM COMMAND
	ERFNO : constant Word_T := 8#02#;   -- CHANNEL NOT OPEN
	EROPR : constant Word_T := 8#03#;   -- CHANNEL ALREADY OPEN
	ERSAL : constant Word_T := 8#04#;   -- SHARED I/O REQ NOT MAP SLOT ALIGNED
	ERMEM : constant Word_T := 8#05#;   -- INSUFFICIENT MEMORY AVAILABLE
	ERADR : constant Word_T := 8#06#;   -- ILLEGAL STARTING ADDRESS
	EROVN : constant Word_T := 8#07#;   -- ILLEGAL OVERLAY NUMBER
	ERTIM : constant Word_T := 8#010#;  -- ILLEGAL TIME ARGUMENT
	ERNOT : constant Word_T := 8#011#;  -- NO TASK CONTROL BLOCK AVAILABLE
	ERXMT : constant Word_T := 8#012#;  -- SIGNAL TO ADDRESS ALREADY IN USE
	ERQTS : constant Word_T := 8#013#;  -- ERROR IN QTASK REQUEST
	ERTID : constant Word_T := 8#014#;  -- TASK I.D. ERROR
	ERDCH : constant Word_T := 8#015#;  -- DATA CHANNEL MAP FULL
	ERMPR : constant Word_T := 8#016#;  -- SYSTEM CALL PARAMETER ADDRESS ERROR
	ERABT : constant Word_T := 8#017#;  -- TASK NOT FOUND FOR ABORT
	ERIRB : constant Word_T := 8#020#;  -- INSUFFICIENT ROOM IN BUFFER
	ERSPC : constant Word_T := 8#021#;  -- FILE SPACE EXHAUSTED
	ERSFT : constant Word_T := 8#022#;  -- USER STACK FAULT
	ERDDE : constant Word_T := 8#023#;  -- DIRECTORY DOES NOT EXIST
	ERIFC : constant Word_T := 8#024#;  -- ILLEGAL FILENAME CHARACTER
	ERFDE : constant Word_T := 8#025#;  -- FILE DOES NOT EXIST
	ERNAE : constant Word_T := 8#026#;  -- FILE NAME ALREADY EXISTS
	ERNAD : constant Word_T := 8#027#;  -- NON-DIRECTORY ARGUMENT IN PATHNAME
	EREOF : constant Word_T := 8#030#;  -- END OF FILE
	ERDID : constant Word_T := 8#031#;  -- DIRECTORY DELETE ERROR
	ERWAD : constant Word_T := 8#032#;  -- WRITE ACCESS DENIED
	ERRAD : constant Word_T := 8#033#;  -- READ ACCESS DENIED
	ERAWD : constant Word_T := 8#034#;  -- APPEND AND/OR WRITE ACCESS DENIED
	ERNMC : constant Word_T := 8#035#;  -- NO CHANNELS AVAILABLE
	ERSRL : constant Word_T := 8#036#;  -- RELEASE OF NON-ACTIVE SHARED SLOT
	ERPRP : constant Word_T := 8#037#;  -- ILLEGAL PRIORITY
	ERBMX : constant Word_T := 8#040#;  -- ILLEGAL MAX SIZE ON PROCESS CREATE
	ERPTY : constant Word_T := 8#041#;  -- ILLEGAL PROCESS TYPE
	ERCON : constant Word_T := 8#042#;  -- CONSOLE DEVICE SPECIFICATION ERROR
	ERNSW : constant Word_T := 8#043#;  -- SWAP FILE SPACE EXHAUSTED
	ERIBS : constant Word_T := 8#044#;  -- DEVICE ALREADY IN SYSTEM
	ERDNM : constant Word_T := 8#045#;  -- ILLEGAL DEVICE CODE
	ERSHP : constant Word_T := 8#046#;  -- ERROR ON SHARED PARTITION SET
	ERRMP : constant Word_T := 8#047#;  -- ERROR ON REMAP CALL
	ERGSG : constant Word_T := 8#050#;  -- ILLEGAL AGENT GATE CALL
	ERPRN : constant Word_T := 8#051#;  -- NUMBER OF PROCESSES EXCEEDS MAX
	ERNEF : constant Word_T := 8#052#;  -- IPC MESSAGE EXCEEDS BUFFER LENGTH
	ERIVP : constant Word_T := 8#053#;  -- INVALID PORT NUMBER
	ERNMS : constant Word_T := 8#054#;  -- NO MATCHING SEND
	ERNOR : constant Word_T := 8#055#;  -- NO OUTSTANDING RECEIVE
	ERIOP : constant Word_T := 8#056#;  -- ILLEGAL ORIGIN PORT
	ERIDP : constant Word_T := 8#057#;  -- ILLEGAL DESTINATION PORT
	ERSEN : constant Word_T := 8#060#;  -- INVALID SHARED LIBRARY REFERENCE
	ERIRL : constant Word_T := 8#061#;  -- ILLEGAL RECORD LENGTH SPECIFIED(=0)
	ERARC : constant Word_T := 8#062#;  -- ATTEMPT TO RELEASE CONSOLE DEVICE
	ERDAI : constant Word_T := 8#063#;  -- DEVICE ALREADY IN USE
	ERARU : constant Word_T := 8#064#;  -- ATTEMPT TO RELEASE UNASSIGNED DEVICE
	ERACU : constant Word_T := 8#065#;  -- ATTEMPT TO CLOSE UNOPEN CHANNEL/DEVICE
	ERITC : constant Word_T := 8#066#;  -- I/O TERMINATED BY CLOSE
	ERLTL : constant Word_T := 8#067#;  -- LINE TOO LONG
	ERPAR : constant Word_T := 8#070#;  -- PARITY ERROR
	EREXC : constant Word_T := 8#071#;  -- RESDENT PROC TRIED TO PUSH (.EXEC)
	ERNDR : constant Word_T := 8#072#;  -- NOT A DIRECTORY
	ERNSA : constant Word_T := 8#073#;  -- SHARED I/O REQUEST NOT TO SHARED AREA
	ERSNM : constant Word_T := 8#074#;  -- ATTEMPT TO CREATE > MAX #; SONS
	ERFIL : constant Word_T := 8#075#;  -- FILE READ ERROR
	ERDTO : constant Word_T := 8#076#;  -- DEVICE TIMEOUT
	ERIOT : constant Word_T := 8#077#;  -- WRONG TYPE I/O FOR OPEN TYPE
	ERFTL : constant Word_T := 8#0100#; -- FILENAME TOO LONG
	ERBOF : constant Word_T := 8#0101#; -- POSITIONING BEFORE BEGINNING OF FILE
	ERPRV : constant Word_T := 8#0102#; -- CALLER NOT PRIVILEGED FOR THIS ACTION
	ERSIM : constant Word_T := 8#0103#; -- SIMULTANEOUS REQUESTS ON SAME CHANNEL
	ERIFT : constant Word_T := 8#0104#; -- ILLEGAL FILE TYPE
	ERNRD : constant Word_T := 8#0105#; -- INSUFFICIENT ROOM IN DIRECTORY
	ERILO : constant Word_T := 8#0106#; -- ILLEGAL OPEN
	ERPRH : constant Word_T := 8#0107#; -- ATTEMPT TO ACCESS PROC NOT IN HIERARCHY
	ERBLR : constant Word_T := 8#0110#; -- ATTEMPT TO BLOCK UNBLOCKABLE PROC
	ERPRE : constant Word_T := 8#0111#; -- INVALID SYSTEM CALL PARAMETER
	ERGES : constant Word_T := 8#0112#; -- ATTEMPT TO START MULTIPLE AGENTS
	ERCIU : constant Word_T := 8#0113#; -- CHANNEL IN USE
	ERICB : constant Word_T := 8#0114#; -- INSUFFICIENT CONTIGUOUS DISK BLOCKS
	ERSTO : constant Word_T := 8#0115#; -- STACK OVERFLOW
	ERIBM : constant Word_T := 8#0116#; -- INCONSISTENT BIT MAP DATA
	ERBSZ : constant Word_T := 8#0117#; -- ILLEGAL BLOCK SIZE FOR DEVICE
	ERXMZ : constant Word_T := 8#0120#; -- ATTEMPT TO XMT ILLEGAL MESSAGE
	ERPUF : constant Word_T := 8#0121#; -- PHYSICAL UNIT FAILURE
	ERPWL : constant Word_T := 8#0122#; -- PHYSICAL WRITE LOCK
	ERUOL : constant Word_T := 8#0123#; -- PHYSICAL UNIT OFFLINE
	ERIOO : constant Word_T := 8#0124#; -- ILLEGAL OPEN OPTION FOR FILE TYPE
	ERNDV : constant Word_T := 8#0125#; -- TOO MANY OR TOO FEW DEVICE NAMES
	ERMIS : constant Word_T := 8#0126#; -- DISK AND FILE SYS REV #'S DON'T MATCH
	ERIDD : constant Word_T := 8#0127#; -- INCONSISTENT DIB DATA
	ERILD : constant Word_T := 8#0130#; -- INCONSISTENT LD
	ERIDU : constant Word_T := 8#0131#; -- INCOMPLETE LD
	ERIDT : constant Word_T := 8#0132#; -- ILLEGAL DEVICE NAME TYPE
	ERPDF : constant Word_T := 8#0133#; -- ERROR IN PROCESS UST DEFINITION
	ERVIU : constant Word_T := 8#0134#; -- LD IN USE, CANNOT RELEASE
	ERSRE : constant Word_T := 8#0135#; -- SEARCH LIST RESOLUTION ERROR
	ERCGF : constant Word_T := 8#0136#; -- CAN'T GET IPC DATA FROM FATHER
	ERILB : constant Word_T := 8#0137#; -- ILLEGAL LIBRARY NUMBER GIVEN
	ERRFM : constant Word_T := 8#0140#; -- ILLEGAL RECORD FORMAT
	ERARG : constant Word_T := 8#0141#; -- TOO MANY OR TOO FEW ARGUMENTS TO PMGR
	ERIGM : constant Word_T := 8#0142#; -- ILLEGAL gtmes PARAMETERS
	ERICL : constant Word_T := 8#0143#; -- ILLEGAL CLI MESSAGE
	ERMRD : constant Word_T := 8#0144#; -- MESSAGE RECEIVE DISABLED
	ERNAC : constant Word_T := 8#0145#; -- NOT A CONSOLE DEVICE
	ERMIL : constant Word_T := 8#0146#; -- ATTEMPT TO EXCEED MAX INDEX LEVEL
	ERICN : constant Word_T := 8#0147#; -- ILLEGAL CHANNEL
	ERNRR : constant Word_T := 8#0150#; -- NO RECEIVER WAITING
	ERSRR : constant Word_T := 8#0151#; -- SHORT RECEIVE REQUEST
	ERTIN : constant Word_T := 8#0152#; -- TRANSMITTER INOPERATIVE
	ERUNM : constant Word_T := 8#0153#; -- ILLEGAL USER NAME
	ERILN : constant Word_T := 8#0154#; -- ILLEGAL LINK #
	ERDPE : constant Word_T := 8#0155#; -- DISK POSITIONING ERROR
	ERTXT : constant Word_T := 8#0156#; -- MSG TEXT LONGER THAN SPEC'D.
	ERSTR : constant Word_T := 8#0157#; -- SHORT TRANSMISSION
	ERHIS : constant Word_T := 8#0160#; -- ERROR ON HISTOGRAM INIT/DELETE
	ERIRV : constant Word_T := 8#0161#; -- ILLEGAL RETRY VALUE
	ERASS : constant Word_T := 8#0162#; -- ASSIGN ERROR - ALREADY YOUR DEVICE
	ERPET : constant Word_T := 8#0163#; -- MAG TAPE REQ PAST LOGICAL END OF TAPE
	ERSTS : constant Word_T := 8#0164#; -- STACK TOO SMALL (task)
	ERTMT : constant Word_T := 8#0165#; -- TOO MANY TASKS REQUESTED (task)
	ERSOC : constant Word_T := 8#0166#; -- SPOOLER OPEN RETRY COUNT EXCEEDED
	ERACL : constant Word_T := 8#0167#; -- ILLEGAL ACL
	ERWPB : constant Word_T := 8#0170#; -- stmap BUFFER INVALID OR WRITE PROTECTED
	ERINP : constant Word_T := 8#0171#; -- IPC FILE NOT OPENED BY ANOTHER PROC
	ERFPU : constant Word_T := 8#0172#; -- FPU HARDWARE NOT INSTALLED
	ERPNM : constant Word_T := 8#0173#; -- ILLEGAL PROCESS NAME
	ERPNU : constant Word_T := 8#0174#; -- PROCESS NAME ALREADY IN USE
	ERDCT : constant Word_T := 8#0175#; -- DISCONNECT ERROR (MODEM CONTROLLED)
	ERIPR : constant Word_T := 8#0176#; -- NONBLOCKING PROC REQUEST ERROR
	ERSNI : constant Word_T := 8#0177#; -- SYSTEM NOT INSTALLED
	ERLVL : constant Word_T := 8#0200#; -- MAX DIRECTORY TREE DEPTH EXCEEDED
	ERROO : constant Word_T := 8#0201#; -- RELEASING OUT-OF-USE OVERLAY
	ERRDL : constant Word_T := 8#0202#; -- RESOURCE DEADLOCK
	EREO1 : constant Word_T := 8#0203#; -- FILE IS OPEN, CAN'T EXCLUSIVE OPEN
	EREO2 : constant Word_T := 8#0204#; -- FILE IS EXCLUSIVE OPENED, CAN'T OPEN
	ERIPD : constant Word_T := 8#0205#; -- INIT PRIVILEGE DENIED
	ERMIM : constant Word_T := 8#0206#; -- MULTIPLE imsg CALLS TO SAME DCT
	ERLNK : constant Word_T := 8#0207#; -- ILLEGAL LINK
	ERIDF : constant Word_T := 8#0210#; -- ILLEGAL DUMP FORMAT
	ERXNA : constant Word_T := 8#0211#; -- EXEC NOT AVAILABLE (MOUNT, ETC.)
	ERXUF : constant Word_T := 8#0212#; -- EXEC REQUEST FUNCTION UNKNOWN
	ERESO : constant Word_T := 8#0213#; -- ONLY EXEC'S SONS CAN DO THAT
	ERRBO : constant Word_T := 8#0214#; -- REFUSED BY OPERATOR
	ERWMT : constant Word_T := 8#0215#; -- VOLUME NOT MOUNTED
	ERISV : constant Word_T := 8#0216#; -- ILLEGAL SWITCH VALUE (>65K DECIMAL)
	-- THE NEXT FOUR ERROR CODES MUST BE CONTIGUOUSLY NUMBERED
	ERIFN : constant Word_T := 8#0217#; -- INPUT FILE  DOES NOT EXIST
	EROFN : constant Word_T := 8#0220#; -- OUTPUT FILE  DOES NOT EXIST
	ERLFN : constant Word_T := 8#0221#; -- LIST FILE  DOES NOT EXIST
	ERDFN : constant Word_T := 8#0222#; -- DATA FILE DOES NOT EXIST
	ERGFE : constant Word_T := 8#0223#; -- RECURSIVE GENERIC FILE OPEN FAILURE
	ERNMW : constant Word_T := 8#0224#; -- NO MESSAGE WAITING
	ERNUD : constant Word_T := 8#0225#; -- USER DATA AREA DOES NOT EXIST
	ERDVC : constant Word_T := 8#0226#; -- ILLEGAL DEVICE TYPE FROM VSGEN
	ERRST : constant Word_T := 8#0227#; -- RESTART OF SYSTEM CALL
	ERFUR : constant Word_T := 8#0230#; -- PROBABLY FATAL HARDWARE RUNTIME ERROR
	ERCFT : constant Word_T := 8#0231#; -- USER COMMERCIAL STACK FAULT
	ERFFT : constant Word_T := 8#0232#; -- USER FLOATING POINT STACK FAULT
	ERUAE : constant Word_T := 8#0233#; -- USER DATA AREA ALREADY EXISTS
	ERISO : constant Word_T := 8#0234#; -- ILLEGAL SCREEN_EDIT REQUEST (PMGR)
	ERDDH : constant Word_T := 8#0235#; -- "send" DESTINATION DEVICE HELD BY "^S"
	EROVR : constant Word_T := 8#0236#; -- DATA OVERRUN ERROR
	ERCPD : constant Word_T := 8#0237#; -- CONTROL POINT DIRECTORY MAX SIZE EXCEEDED
	ERNSD : constant Word_T := 8#0240#; -- SYS OR BOOT DISK NOT PART OF MASTER LD
	ERUSY : constant Word_T := 8#0241#; -- UNIVERSAL SYSTEM, YOU CAN'T DO THAT
	EREAD : constant Word_T := 8#0242#; -- EXECUTE ACCESS DENIED
	ERFIX : constant Word_T := 8#0243#; -- CAN'T INIT LD, RUN FIXUP ON IT
	ERFAD : constant Word_T := 8#0244#; -- FILE ACCESS DENIED
	ERDAD : constant Word_T := 8#0245#; -- DIRECTORY ACCESS DENIED
	ERIAD : constant Word_T := 8#0246#; -- ATTEMPT TO DEFINE > 1 SPECIAL PROC
	ERIND : constant Word_T := 8#0247#; -- NO SPECIAL PROCESS IS DEFINED
	ERPRO : constant Word_T := 8#0250#; -- ATTEMPT TO ISSUE MCA REQUEST WITH
	-- DIRECT I/O IN PROGRESS
	ERDIO : constant Word_T := 8#0251#; -- ATTEMPT TO ISSUE MCA DIRECT I/O WITH
	-- OUTSTANDING REQUESTS
	ERLTK : constant Word_T := 8#0252#; -- LAST TASK WAS KILLED
	ERLRF : constant Word_T := 8#0253#; -- RESOURCE LOAD OR RELEASE FAILURE
	ERNNL : constant Word_T := 8#0254#; -- ZERO LENGTH FILENAME SPECIFIED
	-- FOLLOWING ARE AOS/VS ONLY ERROR CODES
	-- THEY ARE IN A SEPARATE GROUP FROM THE AOS ERROR CODES
	-- THEY ARE IN GROUP 77, HENCE CODES ARE 77*1000+X
	ERXXX : constant Word_T := 8#077# * 8#01000#; -- FIRST CODE FOR AOS/VS
	-- HARDWARE POINTER VALIDATION ERRORS
	ERVWP : constant Word_T := ERXXX ;    -- INVALID ADDRESS PASSED AS SYSTEM CALL ARGUMENT
	ERVBP : constant Word_T := ERVWP + 1; -- INVALID BYTE POINTER PASSED AS SYS CALL ARGUMENT
	ERDPT : constant Word_T := ERVBP + 1; -- DIFFERENT TYPE PROCESS(32/16 BIT) WITHOUT PRIVILEGE
	ERRAL : constant Word_T := ERDPT + 1; -- RING ALREADY LOADED
	ERRNI : constant Word_T := ERRAL + 1; -- RING NUMBER INVALID
	ERRTB : constant Word_T := ERRNI + 1; -- RING TOO BIG
	ERWSM : constant Word_T := ERRTB + 1; -- SET WKG SET MIN, NOT PRIVILEGED
	ERTNE : constant Word_T := ERWSM + 1; -- PMGR- TRACING NOT ENABLED
	ERTAE : constant Word_T := ERTNE + 1; -- PMGR- TRACING ALREADY ENABLED
	ERNUF : constant Word_T := ERTAE + 1; -- PMGR- TRACING FILE NOT A USER DATA FILE
	ERRNA : constant Word_T := ERNUF + 1; -- PMGR- REQUESTOR NOT TRACING AUTHORIZED
	ERPNL : constant Word_T := ERRNA + 1; -- PMGR- PATHNAME LENGTH AREA
	ERSNF : constant Word_T := ERPNL + 1; -- SYMBOL NOT FOUND IN .ST FILE
	ERSNR : constant Word_T := ERSNF + 1; -- SOURCE NOT RESIDENT ON lmap
	ERDNR : constant Word_T := ERSNR + 1; -- DESTINATION NOT RESIDENT ON lmap
	ERIBP : constant Word_T := ERDNR + 1; -- BKPT SEEN IN USER PROGRAM WHEN DEBUG NOT INIT'ED
	ERBST : constant Word_T := ERIBP + 1; -- BAD SYMBOL TABLE FORMAT SEEN (gtna CALL)
	ERPDO : constant Word_T := ERBST + 1; -- PAGE FILE DIRECTORY OVERFLOW
	ERMWT : constant Word_T := ERPDO + 1; -- MORE THAN ONE WS TRACE DEFINED ON TARGET
	ERHWT : constant Word_T := ERMWT + 1; -- BOTH TRACE AND HISTOGRAM CALLED, OR > 1 TRACE
	ERDTC : constant Word_T := ERHWT + 1; -- DIFFERENT TYPE CHAIN
	ERWST : constant Word_T := ERDTC + 1; -- NO WS TRACE DEFINED ON THIS TARGET
	ERWSS : constant Word_T := ERWST + 1; -- INVALID WKG SET MAX/MIN
	ERWSB : constant Word_T := ERWSS + 1; -- INVALID WORKING SET TRACE BUFFER
	ERWSF : constant Word_T := ERWSB + 1; -- WORKING SET NOT SWAPPABLE
	ERAWM : constant Word_T := ERWSF + 1; -- TRYING TO WIRE MORE PAGES THAN WS MAX
	ERTPW : constant Word_T := ERAWM + 1; -- TOO MANY PAGES WIRED
	ERACC : constant Word_T := ERTPW + 1; -- ACCESS DENIED ON valad
	ERRNL : constant Word_T := ERACC + 1; -- RING NOT LOADED
	ERTAL : constant Word_T := ERRNL + 1; -- TOO MANY ARGUMENTS ON LCALL
	ERXBL : constant Word_T := ERTAL + 1; -- ixit FROM BASE LEVEL
	ERPPR : constant Word_T := ERXBL + 1; -- PMGR PANIC REQUESTED BY NON-PMGR PROCESS
	ERSCI : constant Word_T := ERPPR + 1; -- SYSTEM CALL AT INTERRUPT LEVEL
	ERNIP : constant Word_T := ERSCI + 1; -- PMGR -- NOT AN IAC PMGR
	ERNID : constant Word_T := ERNIP + 1; -- PMGR -- NOT AN IAC-DRIVEN DEVICE
	ERSGO : constant Word_T := ERNID + 1; -- signl ALREADY OUTSTANDING
	ERUFR : constant Word_T := ERSGO + 1; -- UNKNOWN REQUEST FUNCTION
	ERIFS : constant Word_T := ERUFR + 1; -- ILLEGAL FED STRING
	ERA1O : constant Word_T := ERIFS + 1; -- ATTEMPT TO 1ST OPEN AN OPEN FILE
	ERIFI : constant Word_T := ERA1O + 1; -- INVALID PROTECTED FILE ID
	ERAPU : constant Word_T := ERIFI + 1; -- ATTEMPT TO PASS UNHELD ACCESS PRIVILEGES
	ERNBK : constant Word_T := ERAPU + 1; -- NO BREAKFILE ENABLED FOR THIS RING
	ERCDS : constant Word_T := ERNBK + 1; -- PMGR: MODEM DISCONNECT IN PROGRESS - CAN'T OPEN
	ERTNF : constant Word_T := ERCDS + 1; -- TASK IS NOT FAULTING
	ERNMT : constant Word_T := ERTNF + 1; -- MAP TARGET DOES NOT EXIST
	ERMTE : constant Word_T := ERNMT + 1; -- MAP TARGET (ALREADY) MAPPED ELSEWHERE
	ERMSI : constant Word_T := ERMTE + 1; -- MAP SPECIFICATION ILLEGAL FOR TARGET
	ERRAU : constant Word_T := ERMSI + 1; -- MAP REGION ALREADY IN USE
	ERJAI : constant Word_T := ERRAU + 1; -- JP ALREADY INITIALIZED
	ERJNI : constant Word_T := ERJAI + 1; -- JP NOT INITIALIZED
	ERLNE : constant Word_T := ERJNI + 1; -- LP DOES NOT EXIST
	ERLAI : constant Word_T := ERLNE + 1; -- LP ALREADY EXISTS
	ERLJP : constant Word_T := ERLAI + 1; -- ATTEMPT TO RELEASE LAST JP ATTACHED TO AN LP
	ERIJP : constant Word_T := ERLJP + 1; -- INVALID JPID
	ERILP : constant Word_T := ERIJP + 1; -- INVALID LPID
	ERJST : constant Word_T := ERILP + 1; -- JP RUNNING ONE OR MORE SYSTEM TASKS
	ERJAA : constant Word_T := ERJST + 1; -- JP ALREADY ATTACHED TO LP
	ERJNA : constant Word_T := ERJAA + 1; -- JP NOT ATTACHED TO LP
	ERMLP : constant Word_T := ERJNA + 1; -- ATTEMPT TO EXCEED MAXIMUM LP COUNT
	ERJPA : constant Word_T := ERMLP + 1; -- CANNOT DELETE LP WITH JP ATTACHED
	ERITI : constant Word_T := ERJPA + 1; -- INVALID TIME INTERVAL
	ERICI : constant Word_T := ERITI + 1; -- INVALID CLASS ID
	ERCPC : constant Word_T := ERICI + 1; -- INVALID CLASS PERCENTAGE
	ERIHL : constant Word_T := ERCPC + 1; -- INVALID HIERARCHICAL LEVEL
	ERCLU : constant Word_T := ERIHL + 1; -- CLASS IN USE
	ERIMP : constant Word_T := ERCLU + 1; -- ILLEGAL BIT MAP
	ERILV : constant Word_T := ERIMP + 1; -- ILLEGAL LOCALITY VALUE
	ERLP0 : constant Word_T := ERILV + 1; -- CANNOT DELETE LP 0
	ERNMP : constant Word_T := ERLP0 + 1; -- NOT A MULTI-PROCESSOR SYSTEM
	ERCNE : constant Word_T := ERNMP + 1; -- CLASS DOES NOT EXIST
	ERHLP : constant Word_T := ERCNE + 1; -- ILLEGAL HIERARCHY LEVEL / PERCENTAGE PAIR
	ERICD : constant Word_T := ERHLP + 1; -- ILLEGAL FUNCTION CODE
	ERJPS : constant Word_T := ERICD + 1; -- JP IS IN A BAD STATE
	ERCMM : constant Word_T := ERJPS + 1; -- MICROCODE IS INCOMPATIBLE WITH CURRENT SYSTEM
	ERMCR : constant Word_T := ERCMM + 1; -- INCORRECT MICROCODE REVISION
	ERMFF : constant Word_T := ERMCR + 1; -- MICROCODE FILE FORMAT ERROR
	ERUCP : constant Word_T := ERMFF + 1; -- INVALID CPU MODEL NUMBER
	ERCSO : constant Word_T := ERUCP + 1; -- CLASS SCHEDULING IS ENABLED
	ERHLT : constant Word_T := ERCSO + 1; -- NON-SEQUENTIAL HIERARCHY LEVELS DESIGNATED
	ERPOR : constant Word_T := ERHLT + 1; -- PID IS OUT OF RANGE FOR THIS PROCESS
	ERPNO : constant Word_T := ERPOR + 1; -- PROCESS NOT AN OPERATOR
	ERBCE : constant Word_T := ERPNO + 1; -- MAX BLOCK COUNT EXCEEDED
	ERDEB : constant Word_T := ERBCE + 1; -- DAEMON ERROR IN ERROR BUFFER
	ERDRF : constant Word_T := ERDEB + 1; -- DAEMON RESOURCE FAILURE
	ERLAS : constant Word_T := ERDRF + 1; -- LOG ALREADY STARTED
	ERCL0 : constant Word_T := ERLAS + 1; -- CANNOT DELETE CLASS 0
	ERPVM : constant Word_T := ERCL0 + 1; -- UNKNOWN PRIVILEGE MODE
	ERPVX : constant Word_T := ERPVM + 1; -- PRIVILEGE HELD EXCLUSIVELY BY OTHER PROCESS
	ERPVO : constant Word_T := ERPVX + 1; -- PRIVILEGE CANNOT BE HELD EXCLUSIVELY
	ERPVP : constant Word_T := ERPVO + 1; -- OTHER PROCESSES USING PRIVILEGE
	ERWCP : constant Word_T := ERPVP + 1; -- WORKING SET CHANGE ONLY PARTLY DONE
	ERFRD : constant Word_T := ERWCP + 1; -- FAULT RECURSION DEPTH EXCEEDED
	ERALP : constant Word_T := ERFRD + 1; -- PROCESS'S CLASS NOT SCHEDULABLE ON AN ACTIVE LP
	ERCLL : constant Word_T := ERALP + 1; -- INVALID CELL COUNT
	ERNML : constant Word_T := ERCLL + 1; -- NO MICROCODE LOADED IN THIS JP
	EREGN : constant Word_T := ERNML + 1; -- END OF GET NEXT SEQUENCE
	ERCTD : constant Word_T := EREGN + 1; -- CORRUPTED TASK CONTROL BLOCK DATA DETECTED
	ERIWR : constant Word_T := ERCTD + 1; -- INVALID WINDOW REFERENCE.
	ERWNN : constant Word_T := ERIWR + 1; -- MAXIMUM NUMBER OF WINDOWS EXCEEDED.
	ERWMD : constant Word_T := ERWNN + 1; -- WINDOW MARKED FOR DELETION.
	ERIGP : constant Word_T := ERWMD + 1; -- INVALID GRAPHICS PARAMETER.
	ERIPP : constant Word_T := ERIGP + 1; -- INVALID POINTER DEVICE PARAMETER.
	ERIVS : constant Word_T := ERIPP + 1; -- INVALID VIEW OR SCAN PORT.
	ERIWO : constant Word_T := ERIVS + 1; -- INVALID WINDOWING OPERATION.
	ERIWP : constant Word_T := ERIWO + 1; -- INVALID WINDOWING PARAMETER.
	ERADE : constant Word_T := ERIWP + 1; -- ASSOCIATION DOES NOT EXIST.
	ERUWE : constant Word_T := ERADE + 1; -- UNKNOWN WINDOWING SUBSYSTEM ERROR
	ERNSP : constant Word_T := ERUWE + 1; -- HARDWARE/MICROCODE DOES NOT SUPPORT PIXEL MAPS
	ERIFL : constant Word_T := ERNSP + 1; -- IAC FAILURE
	ERTMO : constant Word_T := ERIFL + 1; -- TOO MANY OPENS ON THIS DEVICE.

    --       USER STATUS TABLE (UST) TEMPLATE
	UST : constant Word_T := 8#0400#; -- START OF USER STATUS AREA

	USTEZ : constant Word_T := 0;         -- EXTENDED VARIABLE  WORD COUNT
	USTES : constant Word_T := USTEZ + 1; -- EXTENDED VARIABLE PAGE 0 START
	USTSS : constant Word_T := USTES + 1; -- SYMBOLS START
	USTSE : constant Word_T := USTSS + 2; -- SYMBOLS END
	USTDA : constant Word_T := USTSE + 2; -- DEB ADDR OR -1
	USTRV : constant Word_T := USTDA + 2; -- REVISION OF PROGRAM
	USTTC : constant Word_T := USTRV + 2; -- NUMBER OF TASKS (1 TO 32.)
	USTBL : constant Word_T := USTTC + 1; -- # IMPURE BLKS
	USTST : constant Word_T := USTBL + 3; -- SHARED STARTING BLK #
	-- USTST IS USTBL+3 BECAUSE THE 16. BIT USER'S
	-- USTOD IS HIDDEN UNDERNEATH
	USTIT : constant Word_T := USTST + 2;   -- INTERRUPT ADDRESS
	USTSZ : constant Word_T := USTIT + 2;   -- SHARED SIZE IN BLKS
	USTPR : constant Word_T := USTSZ + 2;   -- PROGRAM FILE TYPE (16 OR 32 BIT)
	USTSH : constant Word_T := USTPR + 5;   -- PHYSICAL STARTING PAGE OF SHARED AREA IN .PR
	USTEN : constant Word_T := USTPR + 8#021#; -- END OF USER UST
	USTPL : constant Word_T := USTEN + 6;   -- PROGRAM LOCALITY

	--  LOGICAL RECORD FORMAT TYPES
	ORDY : constant Word_T := 1; -- DYNAMIC
	ORDS : constant Word_T := 2; -- DATA SENSITIVE
	ORFX : constant Word_T := 3; -- FIXED LENGTH
	ORVR : constant Word_T := 4; -- VARIABLE LENGTH
	ORUN : constant Word_T := 5; -- UNDEFINED
	ORVB : constant Word_T := 6; -- IBM VARIABLE BLOCK - VARIABLE RECORD

	-- Record Format Field definitions
	RTDY : constant Word_T := 1; -- DYNAMIC
	RTDS : constant Word_T := 2; -- DATA SENSITIVE
	RTFX : constant Word_T := 3; -- FIXED LENGTH
	RTVR : constant Word_T := 4; -- VARIABLE LENGTH
	RTUN : constant Word_T := 5; -- UNDEFINED
	RTVB : constant Word_T := 6; -- IBM VARIABLE BLOCK - VARIABLE RECORD

	-- GENERAL USER I/O PACKET USED FOR open/read/write/close
	ICH   : constant Phys_Addr_T := 0;        -- CHANNEL NUMBER
	ISTI  : constant Phys_Addr_T := ICH + 1;  -- STATUS WORD (IN)
	ISTO  : constant Phys_Addr_T := ISTI + 1; -- RIGHT=FILE TYPE, LEFT=RESERVED
	IMRS  : constant Phys_Addr_T := ISTO + 1; -- PHYSICAL RECORD SIZE - 1 (BYTES)
	IBAD  : constant Phys_Addr_T := IMRS + 1; -- BYTE POINTER TO BUFFER
	IBAL  : constant Phys_Addr_T := IBAD + 1; -- LOW ORDER BITS OF ibad
	IRES  : constant Phys_Addr_T := IBAL + 1; -- RESERVED
	IRCL  : constant Phys_Addr_T := IRES + 1; -- RECORD LENGTH
	IRLR  : constant Phys_Addr_T := IRCL + 1; -- RECORD LENGTH (RETURNED)
	IRNW  : constant Phys_Addr_T := IRLR + 1; -- RESERVED
	IRNH  : constant Phys_Addr_T := IRNW + 1; -- RECORD NUMBER (HIGH)
	IRNL  : constant Phys_Addr_T := IRNH + 1; -- RECORD NUMBER (LOW)
	IFNP  : constant Phys_Addr_T := IRNL + 1; -- BYTE POINTER TO FILE NAME
	IFNL  : constant Phys_Addr_T := IFNP + 1; -- LOW ORDER BITS OF ifnp
	IDEL  : constant Phys_Addr_T := IFNL + 1; -- DELIMITER TABLE ADDRESS
	IDLL  : constant Phys_Addr_T := IDEL + 1; -- LOWER BITS OF idel
	IOSZ  : constant Phys_Addr_T := IDLL + 1; -- LENGTH OF STANDARD I/O PACKET

	ETSP : constant Phys_Addr_T := IDLL + 1; -- SCREEN MANAGEMENT PACKET
	ETSL : constant Phys_Addr_T := ETSP + 1; -- LOWER PORTION OF etsp
	ETFT : constant Phys_Addr_T := ETSL + 1; -- SELECTED FIELD TRANSLATION PACKET
	ETFL : constant Phys_Addr_T := ETFT + 1; -- LOWER PORTION OF etft
	ETLT : constant Phys_Addr_T := ETFL + 1; -- LABELED TAPE PACKET
	ETLL : constant Phys_Addr_T := ETLT + 1; -- LOWER PORTION OF etlt
	ENET : constant Phys_Addr_T := ETLL + 1; -- RESERVED
	ENEL : constant Phys_Addr_T := ENET + 1; -- RESERVED
	IBLT : constant Phys_Addr_T := ENEL + 1; -- LENGTH OF EXTENDED PACKET

	--  isti FLAGS: BIT DEFINITIONS
	IPLB : constant Natural := 0; -- PACKET LENGTH BIT (0 : constant Natural :=> SHORT PACKET)
	ICFB : constant Natural := 1; -- CHANGE FORMAT BIT (0 : constant Natural :=> DEFAULT)
	ICDM : constant Natural := 1; -- DUMP MODE BIT (ON close ONLY)
	IPTB : constant Natural := 2; -- POSITIONING TYPE (0 : constant Natural :=> RELATIVE)
	IBIB : constant Natural := 3; -- BINARY I/O
	IFOB : constant Natural := 4; -- FORCE OUTPUT
	IOEX : constant Natural := 5; -- EXCLUSIVE OPEN
	IIPS : constant Natural := 6; -- IPC NO WAIT BIT
	PDLM : constant Natural := 7; -- PRIORITY REQUEST
	APBT : constant Natural := 8; -- OPEN FILE FOR APPENDING
	OF1B : constant Natural := 9; -- OPEN TYPE BIT 1
	OF2B : constant Natural := 10; -- OPEN TYPE BIT 2
	OPIB : constant Natural := 11; -- OPEN FOR INPUT
	OPOB : constant Natural := 12; -- OPEN FOR OUTPUT
	RF1B : constant Natural := 13; -- RECORD FORMAT BIT 1
	RF2B : constant Natural := 14; -- RECORD FORMAT BIT 2
	RF3B : constant Natural := 15; -- RECORD FORMAT BIT 3

	--  isti FLAGS: MASK DEFINITIONS
	IPKL : constant Phys_Addr_T := Shift_Right (16#8000#, IPLB); -- EXTENDED PACKET (IF SET)
	ICRF : constant Phys_Addr_T := Shift_Right (16#8000#, ICFB); -- CHANGE RECORD FORMAT (IF SET)
	CDMP : constant Phys_Addr_T := Shift_Right (16#8000#, ICDM); -- SET DUMP BIT (ONLY ON close)
	IPST : constant Phys_Addr_T := Shift_Right (16#8000#, IPTB); -- RECORD POSITIONING TYPE (1 - ABSOLUTE)
	IBIN : constant Phys_Addr_T := Shift_Right (16#8000#, IBIB); -- BINARY I/O
	IFOP : constant Phys_Addr_T := Shift_Right (16#8000#, IFOB); -- FORCE OUTPUT
	IEXO : constant Phys_Addr_T := Shift_Right (16#8000#, IOEX); -- EXCLUSIVE OPEN
	IIPC : constant Phys_Addr_T := Shift_Right (16#8000#, IIPS); -- IPC NO WAIT BIT
	PDEL : constant Phys_Addr_T := Shift_Right (16#8000#, PDLM); -- PRIORITY OPEN-I/O
	APND : constant Phys_Addr_T := Shift_Right (16#8000#, APBT); -- OPEN FILE FOR APPENDING
	OFCR : constant Phys_Addr_T := Shift_Right (16#8000#, OF1B); -- ATTEMPT CREATE BEFORE OPEN
	OFCE : constant Phys_Addr_T := Shift_Right (16#8000#, OF2B); -- CORRECT ERROR ON CREATE OR OPEN
	OFIN : constant Phys_Addr_T := Shift_Right (16#8000#, OPIB); -- OPEN FOR INPUT
	OFOT : constant Phys_Addr_T := Shift_Right (16#8000#, OPOB); -- OPEN FOR OUTPUT
	OFIO : constant Phys_Addr_T := OFIN + OFOT   ; -- OPEN FOR INPUT AND OUTPUT
	
end PARU_32;