-- Copyright ©2021,2022,2024 Stephen Merrony
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published
-- by the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.

-- Adapted from parts of AOS-VS 7.73 PARU.32.SR definitions file

with DG_Types;   use DG_Types;

package PARU_32 is

-- System Error Codes
	ERICM : constant Word_T := 8#01#;   -- ILLEGAL SYSTEM COMMAND
	ERFNO : constant Word_T := 8#02#;   -- CHANNEL NOT OPEN
	EROPR : constant Word_T := 8#03#;   -- CHANNEL ALREADY OPEN
	ERSAL : constant Word_T := 8#04#;   -- SHARED I-O REQ NOT MAP SLOT ALIGNED
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
	ERAWD : constant Word_T := 8#034#;  -- APPEND AND-OR WRITE ACCESS DENIED
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
	ERACU : constant Word_T := 8#065#;  -- ATTEMPT TO CLOSE UNOPEN CHANNEL-DEVICE
	ERITC : constant Word_T := 8#066#;  -- I-O TERMINATED BY CLOSE
	ERLTL : constant Word_T := 8#067#;  -- LINE TOO LONG
	ERPAR : constant Word_T := 8#070#;  -- PARITY ERROR
	EREXC : constant Word_T := 8#071#;  -- RESDENT PROC TRIED TO PUSH (.EXEC)
	ERNDR : constant Word_T := 8#072#;  -- NOT A DIRECTORY
	ERNSA : constant Word_T := 8#073#;  -- SHARED I-O REQUEST NOT TO SHARED AREA
	ERSNM : constant Word_T := 8#074#;  -- ATTEMPT TO CREATE > MAX #; SONS
	ERFIL : constant Word_T := 8#075#;  -- FILE READ ERROR
	ERDTO : constant Word_T := 8#076#;  -- DEVICE TIMEOUT
	ERIOT : constant Word_T := 8#077#;  -- WRONG TYPE I-O FOR OPEN TYPE
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
	ERHIS : constant Word_T := 8#0160#; -- ERROR ON HISTOGRAM INIT-DELETE
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
	-- DIRECT I-O IN PROGRESS
	ERDIO : constant Word_T := 8#0251#; -- ATTEMPT TO ISSUE MCA DIRECT I-O WITH
	-- OUTSTANDING REQUESTS
	ERLTK : constant Word_T := 8#0252#; -- LAST TASK WAS KILLED
	ERLRF : constant Word_T := 8#0253#; -- RESOURCE LOAD OR RELEASE FAILURE
	ERNNL : constant Word_T := 8#0254#; -- ZERO LENGTH FILENAME SPECIFIED
	-- FOLLOWING ARE AOS-VS ONLY ERROR CODES
	-- THEY ARE IN A SEPARATE GROUP FROM THE AOS ERROR CODES
	-- THEY ARE IN GROUP 77, HENCE CODES ARE 77*1000+X
	ERXXX : constant Word_T := 8#077# * 8#01000#; -- FIRST CODE FOR AOS-VS
	-- HARDWARE POINTER VALIDATION ERRORS
	ERVWP : constant Word_T := ERXXX ;    -- INVALID ADDRESS PASSED AS SYSTEM CALL ARGUMENT
	ERVBP : constant Word_T := ERVWP + 1; -- INVALID BYTE POINTER PASSED AS SYS CALL ARGUMENT
	ERDPT : constant Word_T := ERVBP + 1; -- DIFFERENT TYPE PROCESS(32-16 BIT) WITHOUT PRIVILEGE
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
	ERWSS : constant Word_T := ERWST + 1; -- INVALID WKG SET MAX-MIN
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
	ERHLP : constant Word_T := ERCNE + 1; -- ILLEGAL HIERARCHY LEVEL - PERCENTAGE PAIR
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
	ERNSP : constant Word_T := ERUWE + 1; -- HARDWARE-MICROCODE DOES NOT SUPPORT PIXEL MAPS
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

	-- USTPR FLAGS
	UST16 : constant Word_T := 16#8000#; 	-- 16 BIT PROGRAM TYPE
	-- UST32 : constant Word_T := 0            ; 32 BIT PROGRAM TYPE

	-- USTPA : constant Word_T := 0B15    ; PID SIZE TYPE 'SMALLPID' (<256)
	-- USTPB : constant Word_T := 2B15    ; PID SIZE TYPE 'HYBRID' (<256)
	-- USTPC : constant Word_T := 3B15    ; PID SIZE TYPE 'ANYPID' (>256)

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

	-- GENERAL USER I-O PACKET USED FOR open-read-write-close
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
	IOSZ  : constant Phys_Addr_T := IDLL + 1; -- LENGTH OF STANDARD I-O PACKET

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
	IPLB : constant Natural := 0; -- PACKET LENGTH BIT (0 => SHORT PACKET)
	ICFB : constant Natural := 1; -- CHANGE FORMAT BIT (0 => DEFAULT)
	ICDM : constant Natural := 1; -- DUMP MODE BIT (ON close ONLY)
	IPTB : constant Natural := 2; -- POSITIONING TYPE (0 => RELATIVE)
	IBIB : constant Natural := 3; -- BINARY I-O
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
	IPKL : constant Word_T := Shift_Right (16#8000#, IPLB); -- EXTENDED PACKET (IF SET)
	ICRF : constant Word_T := Shift_Right (16#8000#, ICFB); -- CHANGE RECORD FORMAT (IF SET)
	CDMP : constant Word_T := Shift_Right (16#8000#, ICDM); -- SET DUMP BIT (ONLY ON close)
	IPST : constant Word_T := Shift_Right (16#8000#, IPTB); -- RECORD POSITIONING TYPE (1 - ABSOLUTE)
	IBIN : constant Word_T := Shift_Right (16#8000#, IBIB); -- BINARY I-O
	IFOP : constant Word_T := Shift_Right (16#8000#, IFOB); -- FORCE OUTPUT
	IEXO : constant Word_T := Shift_Right (16#8000#, IOEX); -- EXCLUSIVE OPEN
	IIPC : constant Word_T := Shift_Right (16#8000#, IIPS); -- IPC NO WAIT BIT
	PDEL : constant Word_T := Shift_Right (16#8000#, PDLM); -- PRIORITY OPEN-I-O
	APND : constant Word_T := Shift_Right (16#8000#, APBT); -- OPEN FILE FOR APPENDING
	OFCR : constant Word_T := Shift_Right (16#8000#, OF1B); -- ATTEMPT CREATE BEFORE OPEN
	OFCE : constant Word_T := Shift_Right (16#8000#, OF2B); -- CORRECT ERROR ON CREATE OR OPEN
	OFIN : constant Word_T := Shift_Right (16#8000#, OPIB); -- OPEN FOR INPUT
	OFOT : constant Word_T := Shift_Right (16#8000#, OPOB); -- OPEN FOR OUTPUT
	OFIO : constant Word_T := OFIN + OFOT   ; -- OPEN FOR INPUT AND OUTPUT

	-- PACKET TO GET INITIAL MESSAGE (?GTMES)
	GREQ : constant Phys_Addr_T := 0;        -- REQUEST TYPE (SEE BELOW)
	GNUM : constant Phys_Addr_T := GREQ + 1; -- ARGUMENT NUMBER
	GSW  : constant Phys_Addr_T := GNUM + 1; -- BYTE PTR TO POSSIBLE SWITCH
	GSW1 : constant Phys_Addr_T := GSW + 1;  -- LOWER PORTION OF gsw
	GRES : constant Phys_Addr_T := GSW1 + 1; -- BYTE PTR TO AREA TO RECEIVE
	GREL : constant Phys_Addr_T := GRES + 1; -- LOWER PORTION OF gres
	-- SWITCH
	GTLN : constant Phys_Addr_T := GREL + 1; -- PACKET LENGTH

	-- REQUEST TYPES (greq)
	GMES : constant Word_T := 0;        -- 0 -GET ENTIRE MESSAGE
	GCMD : constant Word_T := GMES + 1; -- 1 - GET CLI COMMAND
	GCNT : constant Word_T := GCMD + 1; -- 2 - GET ARGUMENT COUNT
	GARG : constant Word_T := GCNT + 1; -- 3 - GET ARGUMENT
	GTSW : constant Word_T := GARG + 1; -- 4 - TEST SWITCH
	GSWS : constant Word_T := GTSW + 1; -- 5 - TEST SWITCHES
	GDLC : constant Word_T := 16#8000#;  --1B0 DISABLE LOWER TO UPPERCASE CONVERSION

	-- FLAGS RETURNED ON gflg TYPE CALLS
	GFCF : constant Word_T := 16#8000#; -- 1B0             -- CLI FORMAT

	-- BY CONVENTION, PROGRAMS CALLABLE FROM EXEC USE BITS 1 & 2
	-- IF gfcf IS 0.
	GFEX : constant Word_T := 16#4000#; --1B1             -- FROM EXEC IF ON

	--IF gfex IS ON, gfxb GIVES JOB'S BATCH-INTERACTIVE STATUS
	GFXB : constant Word_T := 16#2000#; --1B2             -- ON=BATCH, OFF=INTERACTIVE
	-- IN ADDITION, IF CLI IS INVOKED WITH gfcf 0, BOTH gfxb & gfex
	-- EQUAL TO ZERO => EXECUTE COMMAND PASSED IN MESSAGE AND RETURN.	


--  PERIPHERAL DEVICE CHARACTERISTICS
	-- The following parameters are for the characteristic packet offsets
	ch1  : constant Word_T := 0 ; -- word 1 (offset 0)
	ch2  : constant Word_T := 1 ; -- word 2 (offset 1)
	ch3  : constant Word_T := 2 ; -- word 3 (offset 2)
	ch4  : constant Word_T := 3 ; -- word 4 (offset 3)
	ch5  : constant Word_T := 4 ; -- word 5 (offset 4)
	ch6  : constant Word_T := 5 ; -- word 6 (offset 5)
	ch7  : constant Word_T := 6 ; -- word 7 (offset 6)
	ch8  : constant Word_T := 7 ; -- word 8 (offset 7)
	ch9  : constant Word_T := 8 ; -- word 9 (offset 8)
	ch10 : constant Word_T := 9 ; -- word 10 (offset 9)
	ch11 : constant Word_T := 10; -- word 11 (offset 10)
	ch12 : constant Word_T := 11; -- word 12 (offset 11)
	ch13 : constant Word_T := 12; -- word 13 (offset 12)
	ch14 : constant Word_T := 13; -- word 14 (offset 13)
	ch15 : constant Word_T := 14; -- word 15 (offset 14)

	--        Packet length parameters
	clmin : constant Integer := 3 ; --  MIN LENGTH OF CHARACTERISTICS PACKET
	clmax : constant Integer := 15; --  MAX LENGTH OF CHARACTERISTICS PACKET
	bmlth : constant Integer := 20; --  LENGTH OF INQUIRE PACKET

	--        ch1 - offset 0
	cst  : constant Word_T := 2#10000000_00000000#; -- SIMULATE TABS
	csff : constant Word_T := 2#01000000_00000000#; -- SIMULATE FORM FEEDS
	cepi : constant Word_T := 2#00100000_00000000#; -- REQUIRE EVEN PARITY ON INPUT
	c8bt : constant Word_T := 2#00010000_00000000#; -- ALLOW 8 DATA BITS-CHARACTER
	cspo : constant Word_T := 2#00001000_00000000#; -- SET PARITY ON OUTPUT (EVEN ONLY)
	craf : constant Word_T := 2#00000100_00000000#; -- SEND RUBOUTS AFTER FORM FEEDS
	crat : constant Word_T := 2#00000010_00000000#; -- SEND RUBOUTS AFTER TABS
	crac : constant Word_T := 2#00000001_00000000#; -- SEND RUBOUTS AFTER CR AND NL
	cnas : constant Word_T := 2#00000000_10000000#; -- NON ANSI STANDARD DEVICE
	cott : constant Word_T := 2#00000000_01000000#; -- CONVERT ESC CHARACTER (FOR OLD TTY'S)
	ceol : constant Word_T := 2#00000000_00100000#; -- DO NOT AUTO CR-LF AT END OF LINE
	cuco : constant Word_T := 2#00000000_00010000#; -- OUTPUT UPPER CASE ONLY DEVICE
	cmri : constant Word_T := 2#00000000_00001000#; -- MONITOR RING INDICATOR ON MODEM CONTROL LINE
	cff  : constant Word_T := 2#00000000_00000100#; -- FORM FEED ON OPEN
	--        THE FOLLOWING TWO BITS MUST NOT BE MOVED :
	ceb0 : constant Word_T := 2#00000000_00000010#; -- ECHO MODE BIT 0
	ceb1 : constant Word_T := 2#00000000_00000001#; -- ECHO MODE BIT 1

	--        ECHO MODES :
	--        0=      NO ECHO
	--        1=      STRAIGHT ECHO
	--        2=      ECHO CONTROL CHARS AS ^B ^F (ETC.), ESC AS $
	--        3=      (RESERVED FOR FUTURE USE)

	ceos : constant Word_T := Shift_Right(16#8000#, 15);       -- STRAIGHT ECHO BIT MASK
	ceoc : constant Word_T := Shift_Right(16#8000#, 14);       -- CNTRL SPECIAL ECHO BIT MASK

	--        ch2 - offset 1
	culc : constant Word_T := 2#10000000_00000000#; -- INPUT UPPER-LOWER CASE DEVICE
	cpm  : constant Word_T := 2#01000000_00000000#; -- DEVICE IS IN PAGE MODE
	cnrm : constant Word_T := 2#00100000_00000000#; -- DISABLE MESSAGE RECEPTION
	cmod : constant Word_T := 2#00010000_00000000#; -- DEVICE ON MODEM INTERFACE

	--        THE FOLLOWING FOUR BITS MUST NOT BE MOVED :
	cdt0 : constant Word_T := 2#00001000_00000000#; -- DEVICE TYPE BIT 0 (>>4)
	cdt1 : constant Word_T := 2#00000100_00000000#; -- DEVICE TYPE BIT 1
	cdt2 : constant Word_T := 2#00000010_00000000#; -- DEVICE TYPE BIT 2
	cdt3 : constant Word_T := 2#00000001_00000000#; -- DEVICE TYPE BIT 3

	cto  : constant Word_T := 2#00000000_10000000#;  -- DEVICE TIME-OUTS ENABLED
	ctsp : constant Word_T := 2#00000000_01000000#;  -- CRA- NO TRAILING BLANK SUPPRESSION
	cpbn : constant Word_T := 2#00000000_00100000#; -- CRA- PACKED FORMATE ON BINARY READ
	cesc : constant Word_T := 2#00000000_00010000#; -- ESC CHARACTER PRODUCES INTERRUPT
	cwrp : constant Word_T := 2#00000000_00001000#; -- HARDWARE WRAPS AROUND ON LINE TOO LONG
	cfkt : constant Word_T := 2#00000000_00000100#; -- FUNCTION KEYS ARE INPUT DELIMITERS
	cnnl : constant Word_T := 2#00000000_00000010#; -- CRA- NO NEW-LINE CHARACTERS APPENDED
	--                15    -- BIT 15 USED IN PARU.16.SR FOR TRA-TPA

	--        DEFINE DEVICE TYPE MASK.
	dtype : constant Word_T := Shift_Right(16#8000#, 4) + Shift_Right(16#8000#, 5) + Shift_Right(16#8000#, 6) + Shift_Right(16#8000#, 7);
	
	tty   : constant Word_T := 0;                                          -- 4010A CONSOLE DEVICE TYPE
	crt1  : constant Word_T := Shift_Right(16#8000#, 7);                             -- 4010I CONSOLE DEVICE TYPE
	crt2  : constant Word_T := Shift_Right(16#8000#, 6);                             -- 6012  CONSOLE DEVICE TYPE
	crt3  : constant Word_T := Shift_Right(16#8000#, 6) + Shift_Right(16#8000#, 7);                -- 605X CONSOLE DEVICE TYPE
	crt4  : constant Word_T := Shift_Right(16#8000#, 5);                             -- ANOTHER CONSOLE DEVICE TYPE
	crt5  : constant Word_T := Shift_Right(16#8000#, 5) + Shift_Right(16#8000#, 7);                -- PSEUDO 6012 DEVICE
	crt6  : constant Word_T := Shift_Right(16#8000#, 5) + Shift_Right(16#8000#, 6);                -- 6130 CONSOLE DEVICE TYPE
	crt7  : constant Word_T := Shift_Right(16#8000#, 5) + Shift_Right(16#8000#, 6) + Shift_Right(16#8000#, 7); -- USER DEFINED DEVICE
	crt8  : constant Word_T := Shift_Right(16#8000#, 4);                             -- USER DEFINED DEVICE
	crt9  : constant Word_T := Shift_Right(16#8000#, 4) + Shift_Right(16#8000#, 7);                -- USER DEFINED DEVICE
	crt10 : constant Word_T := Shift_Right(16#8000#, 4) + Shift_Right(16#8000#, 6);                -- USER DEFINED DEVICE
	crt11 : constant Word_T := Shift_Right(16#8000#, 4) + Shift_Right(16#8000#, 6) + Shift_Right(16#8000#, 7); -- USER DEFINED DEVICE
	crt12 : constant Word_T := Shift_Right(16#8000#, 4) + Shift_Right(16#8000#, 5);                -- USER DEFINED DEVICE
	crt13 : constant Word_T := Shift_Right(16#8000#, 4) + Shift_Right(16#8000#, 5) + Shift_Right(16#8000#, 7); -- USER DEFINED DEVICE
	crt14 : constant Word_T := Shift_Right(16#8000#, 4) + Shift_Right(16#8000#, 5) + Shift_Right(16#8000#, 6); -- USER DEFINED DEVICE
	crt15 : constant Word_T := Shift_Right(16#8000#, 4) + Shift_Right(16#8000#, 5) + Shift_Right(16#8000#, 6) + Shift_Right(16#8000#, 7);

	--        ch3 - offset 2
	--
	--        HIGH BYTE IS LPP (LINES PER PAGE)
	--        LOW  BYTE IS CPL (CHARACTERS PER LINE)
	cpgsz : constant Word_T := ch3; -- Page size

	--        ch4 - offset 3
	cval : constant Integer := 0;   -- INDICATES THAT THE CONTENTS OF THIS
									-- OFFSET ARE VALID(USED ON RETURN
									-- FROM gechr.)  IN GENERAL, cval: constant Integer := 1
									-- FOR AN IAC SYSTEM, AND cval OTHERWISE.
	br0bit : constant Integer := 1;      -- BAUD RATE FIELD (BIT 0)
	ctck   : constant Integer := 2;      -- INTERNAL TRANSMITER CLOCK
	crck   : constant Integer := 3;      -- INTERNAL RECIEVER CLOCK
	br1bit : constant Integer := 4;      -- BAUD RATE FIELD (BIT 1)
	br2bit : constant Integer := 5;      -- BAUD RATE FIELD (BIT 2)
	br3bit : constant Integer := 6;      -- BAUD RATE FIELD (BIT 3)
	br4bit : constant Integer := 7;      -- BAUD RATE FIELD (BIT 4)
	cst0   : constant Integer := 8;      -- STOP BIT 0
	cst1   : constant Integer := 9;      -- STOP BIT 1
	cpty   : constant Integer := 10;     -- ODD-EVEN PARITY
	cpen   : constant Integer := 11;     -- PARITY DISABLED-ENABLED
	clt0   : constant Integer := 12;     -- DATA LENGTH BITS
	clt1   : constant Integer := 13;     -- DATA LENGTH BITS
	brfct  : constant Integer := 14;     -- BAUD RATE FACTOR 16X
	hrdflc : constant Integer := 15;     -- HARDWARE FLOW CONTROL (CTS)
	chofc  : constant Integer := hrdflc; -- HARDWARE OUTPUT FLOW CONTROL

	--        SPLIT BAUD RATE VALUES:
	-- csben = 16#8000#>>ctck + 16#8000#>>brfct                -- ENABLE SPLIT BAUD
	-- csbds = 16#8000#>>ctck + 16#8000#>>crck + 16#8000#>>brfct -- DISABLE SPLIT BAUD

	--        STOP BIT FIELD VALUES ARE:
	-- csmsk = 16#8000#>>cst0 + 16#8000#>>cst1 -- STOP BIT FIELD MASK

	-- cs10=  0bcst0+16#8000# >> cst1         -- 1 STOP BIT
	-- cs15=  16#8000# >> cst0+0bcst1         -- 1.5 STOP BITS
	-- cs20=  16#8000# >> cst0+16#8000# >> cst1         -- 2 STOP BITS

	--        PARITY BIT FIELD VALUES ARE:
	-- cpmsk= 16#8000# >> cpen+16#8000# >> cpty         -- PARITY FIELD MASK

	-- cpr0=  0bcpen                 -- DISABLE PARITY CHECKING
	-- cpr1=  1bcpen+0bcpty         -- ENABLE ODD  PARITY
	-- cpr2=  1bcpen+1bcpty         -- ENABLE EVEN PARITY

	-- --        BAUD RATES ARE:
	-- brmsk= 16#8000# >> br0bt)!17B(br4bit)        -- BAUD RATE MASK
	-- cr50=  0B(br0bit)+0.B(br4bit)        -- 50
	-- cr75=  0B(br0bit)+1.B(br4bit)        -- 75
	-- cr110= 0B(br0bit)+2.B(br4bit)        -- 110
	-- cr134= 0B(br0bit)+3.B(br4bit)        -- 134.5
	-- cr150= 0B(br0bit)+4.B(br4bit)        -- 150
	-- cr300= 0B(br0bit)+5.B(br4bit)        -- 300
	-- cr600= 0B(br0bit)+6.B(br4bit)        -- 600
	-- cr12h= 0B(br0bit)+7.B(br4bit)        -- 1200
	-- cr18h= 0B(br0bit)+8.B(br4bit)        -- 1800
	-- cr20h= 0B(br0bit)+9.B(br4bit)        -- 2000
	-- cr24h= 0B(br0bit)+10.B(br4bit)       -- 2400
	-- cr36h= 0B(br0bit)+11.B(br4bit)       -- 3600
	-- cr48h= 0B(br0bit)+12.B(br4bit)       -- 4800
	-- cr72h= 0B(br0bit)+13.B(br4bit)       -- 7200
	-- cr96h= 0B(br0bit)+14.B(br4bit)       -- 9600
	-- cr19k= 0B(br0bit)+15.B(br4bit)       -- 19200

	-- cr45=  16#8000# >> br0bt)+0.B(br4bit)        -- 45.5
	-- cr38k= 16#8000# >> br0bt)+1.B(br4bit)        -- 38400
	--                            2- 15           --  - RESERVED

	-- --        DATA LENGTH FIELD VALUES ARE:
	-- clmsk= 1bclt0+1bclt1         -- DATA LENGTH FIELD MASK
	-- cln5=  0bclt0+0bclt1         -- 5 BITS
	-- cln6=  0bclt0+1bclt1         -- 6 BITS
	-- cln7=  1bclt0+0bclt1         -- 7 BITS
	-- cln8=  1bclt0+1bclt1         -- 8 BITS

	--        ch5 - offset 4
	-- shco    = 0  -- SHARED CONSOLE OWNERSHIP CHARACTERISTIC
	-- xofc    = 1  -- XON XOFF OUTPUT FLOW CONTROL
	-- xifc    = 2  -- XON XOFF INPUT  FLOW CONTROL
	-- c16b    = 3  -- Enable double byte handling (16 bit characters)
	-- bmdev   = 4  -- BITMAP DEVICE
	-- trpe    = 5  -- TERMINATE READ ON POINTER EVENT
	-- cwin    = 6  -- WINDOW CHARACTERISTIC
	-- cacc    = 7  -- ENFORCE ACCESS CONTROL
	-- cctd    = 8  -- PORT IS IN A CONTENDED ENVIRONMENT (PBX, TERMSERVER)
	-- csrds   = 9  -- SUPRESS RECEIVER DISABLE
	-- cxlt    = 10 -- TRANSLATE (ANSI TERMINAL)
	-- cabd    = 11 -- [1] DO AUTOBAUD MATCH IF SET
	-- callout = 12 -- CALL OUT (PBX SUPPORT)
	-- cbk0    = 13 -- BREAK FUNCTION BIT 0
	-- cbk1    = 14 -- BREAK FUNCTION BIT 1
	-- cbk2    = 15 -- BREAK FUNCTION BIT 2

	-- -- BREAK FUNCTION FIELD DEFINITION:

	-- cbkm=  1bcbk0+1bcbk1+1bcbk2 -- MASK

	-- cbbm=  0B(cbk2)               -- BREAK BINARY MODE
	-- cbds=  16#8000# >> cbk2               -- FORCE DISCONNECT
	-- cbca=  2B(cbk2)               -- SEND ^C^A SEQUENCE
	-- cbcb=  3B(cbk2)               -- SEND ^C^B SEQUENCE
	-- cbcf=  4B(cbk2)               -- SEND ^C^F SEQUENCE
	--                5B(cbk2)               --  - RESERVED
	--                6B(cbk2)               --  - RESERVED
	--                7B(cbk2)               --  - RESERVED

	--        ch6 - offset 5
	-- --        (MODEM ENHANCEMENTS)

	-- cmdop = ch6 -- Modem options

	-- cdmc  = 0        -- RESERVED
	-- cmdua = cdmc + 1 -- DIRECT USER ACCESS TO MODEM
	-- -- (DON'T PEND FIRST WRITE)
	-- chdpx = cmdua + 1 -- HALF DUPLEX
	-- csmcd = chdpx + 1 -- SUPPRESS MONITORING CD
	-- -- (FOR MODEM CONNECTION)
	-- crtscd = csmcd + 1 -- ON HALF DUPLEX, DON'T RAISE
	-- -- RTS UNTIL CD DROPS
	-- chifc = crtscd + 1 -- HARDWARE INPUT FLOW CONTROL

	-- --        ch7 - offset 6
	-- ctcc = ch7 -- Time (in msec) to wait for CD on a modem
	-- -- connect

	-- --        ch8 - offset 7
	-- ctcd = ch8 -- Time (in msec) to wait for CD if it drops

	-- --        ch9 - offset 8
	-- ctdw = ch9 -- Time (in msec) to wait after connection
	-- -- before allowing I-O

	-- --        ch10 - offset 9
	-- cthc = ch10 -- Time (in msec) to wait after disconnect
	-- -- for modem to settle

	-- --        ch11 - offset 10
	-- ctlt = ch11 -- Time (in msec) to wait before turning
	-- -- the line around (from XMIT to REC) for
	-- -- half duplex

	-- --        ch12 - offset 11
	-- --        (Console Type)
	-- --
	-- --        HIGH BYTE IS RESERVED (=0)
	-- --        LOW  BYTE IS CONSOLE TYPE

	-- cctype = ch12 -- Console type

	-- --        Mask for accessing just console type

	-- cctypmsk = 377 -- mask for just console type

	-- --        These are the current values for console types

	-- cdcc = 0        -- Direct Connect
	-- clnc = cdcc + 1 -- Term Server
	-- ctnc = clnc + 1 -- TELNET Consoles
	-- cpdc = ctnc + 1 -- PAD Consoles
	-- cvrc = cpdc + 1 -- Virtual (SVTA-like) Consoles
	-- cpxc = cvrc + 1 -- PBX Consoles (PIM)
	-- cpcc = cpxc + 1 -- PC-TS Consoles
	-- cbmc = cpcc + 1 -- Bitmapped (Windowing) Console
	-- ctpc = cbmc + 1 -- T1 Primary Rate Console(IIC)

	-- --        ch13 - offset 12
	-- --        (Language Front-end Processor)

	-- clfp = ch13 -- LFP options

	-- ckg0 = 0 -- G1-G0 double-byte handling
	-- ckhw = 1 -- Kanji half-wide characters
	-- cnlx = 2 -- Native language translation

--        DEVICE TYPES : (FOR RUBOUT ECHO & CURSOR CONTROLS)
--
--        PIBC2   CHARACTERS TO :
--        DEVICE  MODEL   MOVE    MOVE    ERASE   RUBOUT
--        TYPE :  # :     LEFT:   RIGHT:  LINE:   ECHO:
--
--        0       4010A   (NONE)  (NONE)  (NONE)  SHIFT O
--        0       6040    (NONE)  (NONE)  (NONE)  SHIFT O
--        1       4010I   ^Z      ^Y      ^K      ^Z,SPACE,^Z
--        2       6012    ^Y      ^X      ^K      ^Y,SPACE,^Y
--        3       6052    ^Y      ^X      ^K      ^Y,SPACE,^Y
--        4       ----    ESC,D   ESC,C   ESC,K   ESC,D,SPACE,ESC,D
--        5       ----
--        6       6130    ^Y      ^X      ^K      ^Z,SPACE,^Z
--        7-15  (FOR FUTURE EXPANSION)

-- EXEC 

	-- FUNCTION CODES
	XFMLT : constant Word_T := 8#02#; -- MOUNT A LABELED TAPE
	XFMUN : constant Word_T := 8#01#; -- MOUNT A UNIT
	XFDUN : constant Word_T := 8#03#; -- DISMOUNT A UNIT OR LABELED TAPE
	XFOTH : constant Word_T := 8#04#; -- SUBMIT A BATCH JOB FOR OTHER USER
	XFSUB : constant Word_T := 8#05#; -- SUBMIT A BATCH JOB
	XFLPT : constant Word_T := 8#06#; -- SUBMIT A PRINT FILE
	XFPTP : constant Word_T := 8#07#; -- SUBMIT A PAPER TAPE PUNCH FILE
	XFXTS : constant Word_T := 8#010#; -- Status report, Large PID
	XFPLT : constant Word_T := 8#011#; -- SUBMIT A PLOT FILE
	XFHAM : constant Word_T := 8#012#; -- SUBMIT A HAMLET FILE
	XFSNA : constant Word_T := 8#013#; -- SUBMIT AN SNA/RJE FILE
	XFFTA : constant Word_T := 8#014#; -- SUBMIT A FTA REQUEST
	XFXUN : constant Word_T := 8#015#; -- EXTENDED MOUNT A UNIT
	XFXML : constant Word_T := 8#016#; -- EXTENDED MOUNT A LABELED TAPE
	XFHOL : constant Word_T := 8#017#; -- HOLD A QUEUE ENTRY
	XFUNH : constant Word_T := 8#020#; -- UNHOLD A QUEUE ENTRY
	XFCAN : constant Word_T := 8#021#; -- CANCEL A QUEUE ENTRY
	XFSTS : constant Word_T := 8#022#; -- OBTAIN RELATIONSHIP TO EXEC
	XFQST : constant Word_T := 8#023#; -- GET QUEUE TYPE FROM QUEUE NAME

	-- THE FOLLOWING FUNCTIONS ARE RESERVED FOR INTERNAL USE
	XFLO  : constant Word_T := 8#024#; -- LABELED TAPE OPEN
	XFLC  : constant Word_T := 8#025#; -- LABELED TAPE CLOSE
	XFME  : constant Word_T := 8#026#; -- MOUNT ERROR
	XFNV  : constant Word_T := 8#027#; -- MOUNT NEXT VOLUME
	XF30R : constant Word_T := 8#030#; -- Reserved
	XFSV  : constant Word_T := 8#031#; -- MOUNT SPECIFIC VOLUME
	XFMNT : constant Word_T := 8#032#; -- Submit a job to a MOUNT queue
	XFBAT : constant Word_T := 8#033#; -- Submit a job to a BATCH queue
	XFMOD : constant Word_T := 8#034#; -- Modify parameters of a queued job
	XFSQT : constant Word_T := 8#035#; -- Get queue type by sequence number
	XFNQN : constant Word_T := 8#036#; -- Get list of queue names
	XFQDS : constant Word_T := 8#037#; -- Given a queuename,
	--  Get info on all jobs in queue
	XFXDU : constant Word_T := 8#040#; -- Extended Dismount

	-- END OF INTERNAL FUNCTIONS

	-- PACKET OFFSETS FOR xfxts

	XFP1  : constant Phys_Addr_T := 2;         -- FIRST PARAMETER
	XFP2  : constant Phys_Addr_T := 3;         -- SECOND PARAMETER
	XFP2L : constant Phys_Addr_T := XFP2 + 1;  -- LOWER PORTION OF xfp2
	XFP3  : constant Phys_Addr_T := XFP2L + 1; -- 3RD PARAMETER - RESERVED
	XFP4  : constant Phys_Addr_T := XFP3 + 1;  -- 15-BIT PID

	-- PACKET TO GET SYSTEM INFORMATION (sinfo)
	SIRN : constant Phys_Addr_T := 0;        -- SYSTEM REV, LEFT BYTE=MAJOR, RIGHT BYTE=MINOR
	SIRS : constant Phys_Addr_T := SIRN + 1; -- RESERVED
	SIMM : constant Phys_Addr_T := SIRS + 1; -- LENGTH OF PHYSICAL MEMORY (HPAGE)
	SIML : constant Phys_Addr_T := SIMM + 1; -- LOWER PORTION OF simm
	SILN : constant Phys_Addr_T := SIML + 1; -- BYTE POINTER TO RECEIVE MASTER LDU NAME
	SILL : constant Phys_Addr_T := SILN + 1; -- LOWER PORTION OF siln
	SIID : constant Phys_Addr_T := SILL + 1; -- BYTE POINTER TO RECEIVE SYSTEM IDENTIFIER
	SIIL : constant Phys_Addr_T := SIID + 1; -- LOWER PORTION OF siid
	SIPL : constant Phys_Addr_T := SIIL + 1; -- UNEXTENDED PACKET LENGTH
	SIOS : constant Phys_Addr_T := SIIL + 1; -- BYTE POINTER TO EXECUTING OP SYS PATHNAME
	SIOL : constant Phys_Addr_T := SIOS + 1; -- LOWER PORTION OF sios
	SSIN : constant Phys_Addr_T := SIOL + 1; -- SYSTEM IMPLEMENTATION NUMBER (savs FOR AOSVS)

	SIEX : constant Phys_Addr_T := SSIN + 6; -- EXTENDED PACKET LENGTH (INCLUDE 3 DOUBLE
					-- WORDS FOR FUTURE EXPANSIONS)

	SAVS : constant Word_T := 2;	-- AOS/VS

	-- SYSTEM RECORD I/O PACKET FOR ALL DISK AND MAG. TAPEAND MCA REQUESTS FROM EITHER THE AGENT OR USER CONTEXTS. USED FOR rdb/wrb, prdb/PWRB, spage AND allocate
	-- Used for ?SPAGE, ?RDB, ?WDB
	PSTI : constant Phys_Addr_T := 0;        -- RECORD COUNT (RIGHT), STATUS IN (LEFT)
	PSTO : constant Phys_Addr_T := PSTI + 1; -- RESERVED (LEFT) PRIORITY (RIGHT)
	PCAD : constant Phys_Addr_T := PSTO + 1; -- WORD ADDRESS FOR DATA
	PCDL : constant Phys_Addr_T := PCAD + 1; -- LOW ORDER PORTION OF pcad
	PRNH : constant Phys_Addr_T := PCDL + 1; -- RECORD NUMBER (HIGH) LINK # (MCA)
	PRNL : constant Phys_Addr_T := PRNH + 1; -- RECORD NUMBER (LOW)  RETRY COUNT (MCA)
	PRCL : constant Phys_Addr_T := PRNL + 1; -- MAX LENGTH OF EACH RECORD (MAG TAPE)
	--                 BYTE COUNT IN LAST BLOCK (DISK WRITES)
	--                 BYTE COUNT (MCA)
	PRES : constant Phys_Addr_T := PRCL + 1; -- RESERVED WORD
	PBLT : constant Phys_Addr_T := PRES + 1; -- PACKET SIZE

	--  PACKET FOR DIRECTORY ENTRY CREATION (create)
	CFTYP : constant Phys_Addr_T := 0;        -- ENTRY TYPE (RH) AND RECORD FORMAT (LH)
	CPOR  : constant Phys_Addr_T := 1;        -- PORT NUMBER (IPC TYPES ONLY)
	CHFS  : constant Phys_Addr_T := 1;        -- HASH FRAME SIZE (DIRECTORY TYPES ONLY)
	CHID  : constant Phys_Addr_T := 1;        -- HOST ID (frem TYPE FILES ONLY )
	CCPS  : constant Phys_Addr_T := 1;        -- FILE CONTROL PARAMETER (OTHERS)
	CTIM  : constant Phys_Addr_T := 2;        -- POINTER TO TIME BLOCK
	CTIL  : constant Phys_Addr_T := CTIM + 1; -- LOWER PORTION OF ctim
	CACP  : constant Phys_Addr_T := CTIL + 1; -- POINTER TO INITIAL ACL
	CACL  : constant Phys_Addr_T := CACP + 1; -- LOWER PORTION OF cacp
	CMSH  : constant Phys_Addr_T := CACL + 1; -- MAX SPACE ALLOCATED (fcpd)
	CMSL  : constant Phys_Addr_T := CMSH + 1; -- MAX SPACE ALLOCATED (LOW)
	CDEH  : constant Phys_Addr_T := CACL + 1; -- RESERVED
	CDEL  : constant Phys_Addr_T := CDEH + 1; -- FILE ELEMENT SIZE
	CMIL  : constant Phys_Addr_T := CDEL + 1; -- MAXIMUM INDEX LEVEL DEPTH
	CMRS  : constant Phys_Addr_T := CMIL + 1; -- RESERVED
	CLTH  : constant Phys_Addr_T := CMRS + 1; -- LENGTH OF THE PARAMETER BLOCK


	-- ENTRY TYPE RANGES
	SMIN :constant Word_T := 0;        -- SYSTEM MINIMUM
	SMAX :constant Word_T := 63;       -- SYSTEM MAXIMUM
	DMIN :constant Word_T := smax + 1; -- DGC MINIMUM
	DMAX :constant Word_T := 127;      -- DGC MAXIMUM
	UMIN :constant Word_T := dmax + 1; -- USER MINIMUM
	UMAX :constant Word_T := 255;      -- USER MAXIMUM	

	-- SYSTEM ENTRY TYPES
	-- MISC
	FLNK : constant Word_T := SMIN;     -- LINK
	FSDF : constant Word_T := FLNK + 1; -- SYSTEM DATA FILE
	FMTF : constant Word_T := FSDF + 1; -- MAG TAPE FILE
	FGFN : constant Word_T := FMTF + 1; -- GENERIC FILE NAME

	-- DIRECTORIES (DO NOT CHANGE THEIR ORDER)
	FDIR : constant Word_T := 10;       -- DISK DIRECTORY
	FLDU : constant Word_T := FDIR + 1; -- LD ROOT DIRECTORY
	FCPD : constant Word_T := FLDU + 1; -- CONTROL POINT DIRECTORY
	FMTV : constant Word_T := FCPD + 1; -- MAG TAPE VOLUME
	FMDR : constant Word_T := FMTV + 1; -- RESERVED FOR RT32(MEM DIRS), NOT LEGAL FOR AOS
	FGNR : constant Word_T := FMDR + 1; -- RESERVED FOR RT32, NOT LEGAL FOR AOS
	LDIR : constant Word_T := FDIR;     -- LOW DIR TYPE
	HDIR : constant Word_T := FGNR;     -- HIGH DIR TYPE
	LCPD : constant Word_T := FLDU;     -- LOW CONTROL POINT DIR TYPE
	HCPD : constant Word_T := FCPD;     -- HIGH CONTROL POINT DIR TYPE

	-- UNITS
	FDKU : constant Word_T := 20;       -- DISK UNIT
	FMCU : constant Word_T := FDKU + 1; -- MULTIPROCESSOR COMMUNICATIONS UNIT
	FMTU : constant Word_T := FMCU + 1; -- MAG TAPE UNIT
	FLPU : constant Word_T := FMTU + 1; -- DATA CHANNEL LINE PRINTER
	FLPD : constant Word_T := FLPU + 1; -- DATA CHANNEL LP2 UNIT
	FLPE : constant Word_T := FLPD + 1; -- DATA CHANNEL LINE PRINTER (LASER)
	FPGN : constant Word_T := FLPE + 1; -- RESERVED FOR RT32(PROCESS GROUP)
	FLTU : constant Word_T := FLPU + 1; -- LABELLED MAG TAPE UNIT
	-- ***** NO LONGER USED *****
	LUNT : constant Word_T := FDKU; -- LOW UNIT TYPE
	HUNT : constant Word_T := FPGN; -- HIGH UNIT TYPE

	-- IPC ENTRY TYPES
	FIPC : constant Word_T := 30;     -- IPC PORT ENTRY

	-- DGC ENTRY TYPES
	FUDF : constant Word_T := DMIN;     -- USER DATA FILE
	FPRG : constant Word_T := FUDF + 1; -- PROGRAM FILE
	FUPF : constant Word_T := FPRG + 1; -- USER PROFILE FILE
	FSTF : constant Word_T := FUPF + 1; -- SYMBOL TABLE FILE
	FTXT : constant Word_T := FSTF + 1; -- TEXT FILE
	FLOG : constant Word_T := FTXT + 1; -- SYSTEM LOG FILE (ACCOUNTING FILE)
	FNCC : constant Word_T := FLOG + 1; -- FORTRAN CARRIAGE CONTROL FILE
	FLCC : constant Word_T := FNCC + 1; -- FORTRAN CARRIAGE CONTROL FILE
	FFCC : constant Word_T := FLCC + 1; -- FORTRAN CARRIAGE CONTROL FILE
	FOCC : constant Word_T := FFCC + 1; -- FORTRAN CARRIAGE CONTROL FILE
	FPRV : constant Word_T := FOCC + 1; -- AOS/VS PROGRAM FILE
	FWRD : constant Word_T := FPRV + 1; -- WORD PROCESSING
	FAFI : constant Word_T := FWRD + 1; -- APL FILE
	FAWS : constant Word_T := FAFI + 1; -- APL WORKSPACE FILE
	FBCI : constant Word_T := FAWS + 1; -- BASIC CORE IMAGE FILE
	FDCF : constant Word_T := FBCI + 1; -- DEVICE CONFIGURATION FILE (NETWORKING)
	FLCF : constant Word_T := FDCF + 1; -- LINK CONFIGURATION FILE (NETWORKING)
	FLUG : constant Word_T := FLCF + 1; -- LOGICAL UNIT GROUP FILE (SNA)
	FRTL : constant Word_T := FLUG + 1; -- AOS/RT32 RESERVED FILE TYPE RANGE (LO)
	FRTH : constant Word_T := FRTL + 4; -- AOS/RT32 RESERVED FILE TYPE RANGE (HI)
	FUNX : constant Word_T := FRTH + 1; -- VS/UNIX FILE
	FBBS : constant Word_T := FUNX + 1; -- BUSINESS BASIC SYMBOL FILE
	FVLF : constant Word_T := FBBS + 1; -- BUSINESS BASIC VOLUME LABEL FILE
	FDBF : constant Word_T := FVLF + 1; -- BUSINESS BASIC DATA BASE FILE

	-- CEO FILE TYPES
	FGKM : constant Word_T := FDBF + 1; -- DG GRAPHICS KERNAL METAFILE
	FVDM : constant Word_T := FGKM + 1; -- VIRTUAL DEVICE METAFILE
	FNAP : constant Word_T := FVDM + 1; -- NAPLPS STANDARD GRAPH FILE
	FTRV : constant Word_T := FNAP + 1; -- TRENDVIEW COMMAND FILE
	FSPD : constant Word_T := FTRV + 1; -- SPREADSHEET FILE
	FQRY : constant Word_T := FSPD + 1; -- PRESENT QUERY MACRO
	FDTB : constant Word_T := FQRY + 1; -- PHD DATA TABLE
	FFMT : constant Word_T := FDTB + 1; -- PHD FORMAT FILE
	FWPT : constant Word_T := FFMT + 1; -- TEXT INTERCHANGE FORMAT
	FDIF : constant Word_T := FWPT + 1; -- DATA INTERCHANGE FORMAT
	FVIF : constant Word_T := FDIF + 1; -- VOICE IMAGE FILE
	FIMG : constant Word_T := FVIF + 1; -- FACSIMILE IMAGE
	FPRF : constant Word_T := FIMG + 1; -- PRINT READY FILE

	-- MORE DGC ENTRY TYPES
	FPIP : constant Word_T := FPRF + 1; -- PIPE FILE
	FTTX : constant Word_T := FPIP + 1; -- TELETEX FILE
	FDXF : constant Word_T := FTTX + 1; -- RESERVED FOR DXA
	FDXR : constant Word_T := FDXF + 1; -- RESERVED FOR DXA
	FCWP : constant Word_T := FDXR + 1; -- CEO WORD PROCESSOR FILE
	FCWT : constant Word_T := FCWP;     -- CEOwrite WORD PROCESSOR FILE
	FRPT : constant Word_T := FCWP + 1; -- PHD REPORT FILE

	-- PACKET FOR TASK DEFINITION (?TASK)
	DLNK 	: constant Phys_Addr_T :=  0;        -- NON-ZERO = SHORT PACKET, ZERO = EXTENDED
	DLNL	: constant Phys_Addr_T := DLNK + 1;  -- 1 LOWER PORTION OF ?DLNK
	DLNKB	: constant Phys_Addr_T := DLNL + 1;  -- 2 BACKWARDS LINK (UPPER PORTION)
	DLNKBL	: constant Phys_Addr_T := DLNKB + 1; -- 3 BACKWARDS LINK (LOWER PORTION)
	DPRI	: constant Phys_Addr_T := DLNKBL + 1; --4 PRIORITY, ZERO TO USE CALLER'S
	DID		: constant Phys_Addr_T := DPRI + 1; -- 5 I.D., ZERO FOR NONE
	DPC		: constant Phys_Addr_T := DID + 1;  -- 6 STARTING ADDRESS OR RESOURCE ENTRY
	DPCL	: constant Phys_Addr_T := DPC + 1;  -- 7 LOWER PORTION OF ?DPC
	DAC2	: constant Phys_Addr_T := DPCL + 1; -- 8 INITIAL AC2 CONTENTS
	DCL2	: constant Phys_Addr_T := DAC2 + 1; -- 9 LOWER PORTION OF ?DAC2
	DSTB	: constant Phys_Addr_T := DCL2 + 1; -- 10 STACK BASE, MINUS ONE FOR NO STACK
	DSTL	: constant Phys_Addr_T := DSTB + 1; -- 11 LOWER PORTION OF ?DSTB
	DSFLT	: constant Phys_Addr_T := DSTL + 1; -- 12 STACK FAULT ROUTINE ADDR OR -1 IF SAME AS CURRENT
	DSSZ	: constant Phys_Addr_T := DSFLT + 1;-- 13 STACK SIZE, IGNORED IF NO STACK
	DSSL	: constant Phys_Addr_T := DSSZ + 1; -- 14 LOWER PORTION OF ?DSSZ
	DFLGS	: constant Phys_Addr_T := DSSL + 1; -- 15 FLAGS
	-- DFL0	: constant Phys_Addr_T :=  1B0     -- RESERVED FOR SYSTEM
	-- DFLRC	: constant Phys_Addr_T := 1B1     -- RESOURCE CALL TASK
	-- DFL15	: constant Phys_Addr_T := 1B15    -- RESERVED FOR SYSTEM
	DRES	: constant Phys_Addr_T := DFLGS + 1; -- 16 RESERVED FOR SYSTEM
	DNUM	: constant Phys_Addr_T := DRES + 1;  -- 17 NUMBER OF TASKS TO CREATE

	DSLTH	: constant Phys_Addr_T := DNUM + 1; -- LENGTH OF SHORT PACKET

	DSH		: constant Phys_Addr_T := DNUM + 1; -- STARTING HOUR, -1 IF IMMEDIATE
	DSMS	: constant Phys_Addr_T := DSH + 1;  -- STARTING SECOND IN HOUR, IGNORED IF IMMEDIATE
	DCC		: constant Phys_Addr_T := DSMS + 1; -- NUMBER OF TIMES TO CREATE TASK(S)
	DCI		: constant Phys_Addr_T := DCC + 1;  -- CREATION INCREMENT  IN SECONDS

	DXLTH	:constant Phys_Addr_T := DCI + 1;  --  LENGTH OF EXTENDED PACKET	

	-- ?RNGPR PACKET OFFSETS
	--
   RNGBP : constant Phys_Addr_T := 0;           -- BYTE POINTER TO BUFFER (2 WORDS)
   RNGNM : constant Phys_Addr_T := RNGBP + 2;   -- RING # OF RING TO CHECK
   RNGLB : constant Phys_Addr_T := RNGNM + 1;   -- BUFFER BYTE LENGTH
   RNGPL : constant Phys_Addr_T := RNGLB + 1;   -- PACKET LENGTH

	-- ?UIDSTAT
	UUID   : constant Phys_Addr_T := 0; 	      -- Unique task identifier
	UTSTAT : constant Phys_Addr_T := UUID + 1; 	 -- Task Status Word
	UTID   : constant Phys_Addr_T := UTSTAT + 1; -- Standard Task ID
	UTPRI  : constant Phys_Addr_T := UTID + 1;   -- Task Priority

    --------------------------------------------------------
	-- INTERPROCESS COMMUNICATION SYSTEM (IPC) PARAMETERS --
    --------------------------------------------------------

	-- HIGHEST LEGAL LOCAL PORT NUMBER
	IMPRT : constant Word_T := 2047;   -- MAX LEGAL USER LOCAL PORT # (Decimal)
	MXLPN : constant Word_T := 4095;   -- MAX LEGAL LOCAL PORT # (Decimal)

	-- IPC MESSAGE HEADER
	ISFL : constant Phys_Addr_T := 0;       -- SYSTEM FLAGS
	IUFL : constant Phys_Addr_T := 1;       -- USER FLAGS

	-- PORT NUMBERS FOR ?ISEND
	IDPH : constant Phys_Addr_T := 2;       -- DESTINATION PORT NUMBER (HIGH)
	IDPL : constant Phys_Addr_T := 3;       -- DESTINATION PORT NUMBER (LOW)
	IOPN : constant Phys_Addr_T := 4;       -- ORIGIN PORT NUMBER

	-- PORT NUMBERS FOR ?IREC
	IOPH : constant Phys_Addr_T := 2;       -- ORIGIN PORT NUMBER (HIGH)
	IOPL : constant Phys_Addr_T := 3;       -- ORIGIN PORT NUMBER (LOW)
	IDPN : constant Phys_Addr_T := 4;       -- DESTINATION PORT NUMBER

	ILTH : constant Phys_Addr_T := 5;       -- LENGTH OF MESSAGE OR BUFFER (IN WORDS)
	IPTR : constant Phys_Addr_T := 6;       -- POINTER TO MESSAGE/BUFFER
	IPTL : constant Phys_Addr_T := IPTR+1;  -- LOWER PORTION OF ?IPTR

	IPLTH : constant Phys_Addr_T := IPTL+1; -- LENGTH OF HEADER

	IRSV : constant Phys_Addr_T := IPTL+1;  -- RESERVED
	IRLT : constant Phys_Addr_T := IRSV+1;  -- ?IS.R RECEIVE BUFFER LENGTH
	IRPT : constant Phys_Addr_T := IRLT+1;  -- ?IS.R RECEIVE BUFFER POINTER
	IRPL : constant Phys_Addr_T := IRPT+1;  -- LOWER PORTION OF ?IRPT
	IPRLTH : constant Phys_Addr_T := IRPL+1; -- LENGTH OF ?IS.R HEADER

	-- SYSTEM FLAG BIT MASKS
	IFSTM  : constant Word_T := 2#10000000_00000000#;    -- SEND TO SELF
	IFRFM  : constant Word_T := 2#10000000_00000000#;    -- RECEIVE FROM SELF
	IFSOV  : constant Word_T := 2#01000000_00000000#;    -- =1 => SPOOL MESSAGE IF BUFFER TOO SMALL
	IFNBK  : constant Word_T := 2#00100000_00000000#;    -- =1 => RETURN ERROR IF NO WAITING MESSAGE
	IFNSP  : constant Word_T := 2#00010000_00000000#;    -- =1 => DO NOT SPOOL MESSAGE
	IFRING : constant Word_T := 2#00000000_00000111#;    -- IPC SOURCE RING IDENTIFIER
	IFDTH  : constant Word_T := 2#00000000_00010000#;    -- OBIT FROM ?ASSOC'D PROCESS
	IFPR   : constant Word_T := 2#10000000_00001000#;    -- 16. BIT CALLER

	-- ?GOPEN PACKET

	OPFL : constant Phys_Addr_T := 0;       -- FLAGS IN TO ?GOPEN
	OPCH : constant Phys_Addr_T := OPFL;   -- CHANNEL NUMBER
	OPTY : constant Phys_Addr_T := 1;       -- FORMAT (LH) AND FILE TYPE (RH)
	OPFC : constant Phys_Addr_T := 2;       -- FILE CONTROL PARAMETERS
	OPEW : constant Phys_Addr_T := 3;       -- RESERVED
	OPEH : constant Phys_Addr_T := 4;       -- FILE EOF (HIGH)
	OPEL : constant Phys_Addr_T := 5;       -- FILE EOF (LOW)
	OPPH : constant Phys_Addr_T := 2;       -- PORT NUMBER (HIGH)
	OPPL : constant Phys_Addr_T := 3;       -- PORT NUMBER (LOW)
	OPLT : constant Phys_Addr_T := OPEL+1; -- LENGTH OF PACKET
	-- --  FLAGS IN BIT POINTERS
	-- OPBE=  0.              -- EXCLUSIVE OPEN
	-- OPDF=  1.              -- INHIBIT INITIAL FORM FEED
	-- OPD0=  2.              -- DENSITY BITS.
	-- OPD1=  3.              -- 
	-- OPD2=  4.              -- 
	-- OPBX=  7.              --  EXTENDED PACKET
	-- OPBF=  8.              --  USE BUFFERED MODE ON TAPE I/O
	-- OPST=  9.              --  USE STREAMING MODE ON TAPE I/O
	-- FLAGS IN MASKS
	OPME :constant Word_T :=  2#10000000_00000000#;   -- EXCLUSIVE OPEN
	OPMD :constant Word_T :=  2#01000000_00000000#;   -- INHIBIT INITIAL FORM FEED
	-- TODO many more flags etc. for ?GOPEN

	--  ?TLOCK/?TUNLOCK PARAMETERS
	TMYRING : constant  Word_T := 2#10000000_00000000#;
	TALOCK  : constant  Word_T := 2#01000000_00000000#;

end PARU_32; 