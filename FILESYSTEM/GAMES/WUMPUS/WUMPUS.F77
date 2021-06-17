*******************************************************************
*
      	PROGRAM WUMPUS

*		by  Stephen C. Schwei     10/4/82
*
*******************************************************************
C
	CHARACTER*60	MESG(20)
	CHARACTER*1	CONT
	CHARACTER*1	ERASELINE	/'<213>'/
	CHARACTER*1	ENABLE		/'<022>'/
	CHARACTER*1	POS		/'<020>'/
	CHARACTER*1	C(80)
	CHARACTER*1	L(24)
C
	INTEGER*4	ISEED		/-1/
	INTEGER*4	CONNECTION(3,12), ARROWS, ROOMS(2,12)
	INTEGER*4	PIT(2), BAT(2), WUMPUS, CHOICES(66)
C
	CALL SETUPPOSITIONS (C,L)
	CALL SETUPROOMS (ROOMS)
	CALL SETUPMESSAGES (MESG)
	CALL SETUPCHOICES (CHOICES)
C
  100   CALL STARTSCREEN (C,L)
	CALL INITIALIZE (CONNECTION,ARROWS,PIT,BAT,WUMPUS,ISEED)
	CALL INSTRUCTIONS (C,L,MESG)
	CALL COMMAND (C,L,MESG,CONNECTION,ARROWS,PIT,BAT,WUMPUS,ISEED,
     /		      ROOMS,CHOICES)
C
	PRINT *, POS,C(1),L(24),ERASELINE
	PRINT *, POS,C(3),L(24),MESG(9)
	READ (*,1000) CONT
 1000   FORMAT (A1)
	IF (CONT.EQ."Y") GO TO 100
C
	PRINT *, ENABLE
C
      	CALL EXIT
      	END
C
C
*******************************************************************
*
      	SUBROUTINE  COMMAND (C,L,MESG,CONNECTION,ARROWS,PIT,BAT,
     /      	             WUMPUS,ISEED,ROOMS,CHOICES)
*
*******************************************************************
C
	INTEGER*4    CONNECTION(3,12), ISEED, ARROWS, ROOMS(2,12)
	INTEGER*4    PIT(2), BAT(2), WUMPUS, CURRPOS, DONE, CHOICES(66)
C
	CHARACTER*4 	COMMANDS(2)
	CHARACTER*40	COMMANDLINE
	CHARACTER*60	MESG(20)
	CHARACTER*1 	C(80)
	CHARACTER*1 	L(24)
	CHARACTER*1 	POS		/'<020>'/
	CHARACTER*1 	ERASELINE	/'<213>'/
C
	COMMANDS(1)="MOVE"
	COMMANDS(2)="FIRE"
	DONE=0
	CURRPOS=0
C
   10	PRINT *, POS,C(1),L(23),MESG(15)
   20	PRINT *, POS,C(1),L(24),ERASELINE
     	PRINT *, POS,C(4),L(24),"COMMAND: "
	READ (*,1000) COMMANDLINE
 1000   FORMAT (A40)
C
	DO 100 I=1,40
	    IF (COMMANDLINE(I:I).GT." ") GO TO 120
  100   CONTINUE
	PRINT *, POS,C(1),L(23),MESG(14)
	PRINT *, POS,C(4),L(24),ERASELINE
	GO TO 20
C
  120   DO 200 J=I+1,40
	    IF (COMMANDLINE(J:J).LE." ") GO TO 220
  200   CONTINUE
  220	LENGTH=J-I-1
C
	DO 300 K=1,2
	IF (COMMANDLINE(I:I+LENGTH).EQ.COMMANDS(K)(1:LENGTH+1))GO TO 400
  300	CONTINUE
	PRINT *, POS,C(1),L(23),MESG(14)
	PRINT *, POS,C(4),L(24),ERASELINE
	GO TO 20
C
  400	IF (K.LT.1.OR.K.GT.2) GO TO 300
        GO TO (500,600), K
C
  500   CALL MOVE (COMMANDLINE,J,CONNECTION,PIT,BAT,WUMPUS,ISEED,MESG,
     /		   C,L,ROOMS,CURRPOS,DONE,CHOICES)
	IF (DONE.EQ.1) RETURN
	GO TO 10
C
  600   CALL FIRE (COMMANDLINE,J,CONNECTION,WUMPUS,ARROWS,ISEED,MESG,
     /		   C,L,CURRPOS,DONE)
	IF (DONE.EQ.1) RETURN
	GO TO 10
C
	END
C
C
*******************************************************************
*
      	SUBROUTINE  MOVE (COMMANDLINE,J,CONNECTION,PIT,BAT,WUMPUS,
     /			  ISEED,MESG,C,L,ROOMS,CURRPOS,DONE,CHOICES)
*
*******************************************************************
C
	INTEGER*4    CONNECTION(3,12), ISEED, ARROWS, ROOMS(2,12)
	INTEGER*4    PIT(2), BAT(2), WUMPUS, CURRPOS, DONE, CHOICES(66)
C
	CHARACTER*40	COMMANDLINE
	CHARACTER*1	C(80)
	CHARACTER*1	L(24)
	CHARACTER*1	POS		/'<020>'/
	CHARACTER*60	MESG(20)
C
	CALL READMOVECOMMANDLINE (COMMANDLINE,J,NEXTPOS,MESG,C,L)
	IF (NEXTPOS.EQ.0) RETURN
	CALL DISPLAYCONNECTION (CONNECTION,CURRPOS,NEXTPOS,C,L,MESG,
     /				CHOICES)
	IF (NEXTPOS.EQ.0) RETURN
C
	IF (CURRPOS.EQ.0) GO TO 100
	CALL DISPLAYROOM (ROOMS,PIT,BAT,WUMPUS,CURRPOS,CONNECTION,ISEED,
     /			  MESG,C,L,1)
  100	CALL DISPLAYROOM (ROOMS,PIT,BAT,WUMPUS,NEXTPOS,CONNECTION,ISEED,
     /			  MESG,C,L,0)
	CURRPOS=NEXTPOS
	IF (CURRPOS.EQ.0) DONE=1
C
	RETURN
	END
C
C
*******************************************************************
*
      	SUBROUTINE  READMOVECOMMANDLINE (COMMANDLINE,J,NEXTPOS,MESG,C,L)
*
*******************************************************************
C
	CHARACTER*1	POS		/'<020>'/
	CHARACTER*1	C(80)
	CHARACTER*1	L(24)
	CHARACTER*2	CPOS
	CHARACTER*60	MESG(20)
	CHARACTER*40	COMMANDLINE
C
	NPOS=J
C
  100	IF (COMMANDLINE(NPOS:NPOS).GT." ") GO TO 200
	NPOS=NPOS+1
	IF (NPOS.EQ.41) GO TO 900
	GO TO 100
C
  200   NSTART=NPOS
	NPOS=NPOS+1
C
  300   IF (COMMANDLINE(NPOS:NPOS).LE." ") GO TO 400
	NPOS=NPOS+1
	IF (NPOS.EQ.41) GO TO 400
	GO TO 300
C
  400	NEND=NPOS-1
	NLEN=NEND-NSTART+1
	IF (NPOS.EQ.41) GO TO 500
	IF (COMMANDLINE(NPOS:40).GT." ") GO TO 950
C
  500	CPOS="  "
	IF (NLEN.EQ.1) CPOS(2:2)=COMMANDLINE(NSTART:NEND)
	IF (NLEN.EQ.2) CPOS(1:2)=COMMANDLINE(NSTART:NEND)
	IF (NLEN.GT.2) GO TO 900
C
	READ (CPOS,1000) NEXTPOS
 1000   FORMAT (I2)
C
	IF (NEXTPOS.LT.1.OR.NEXTPOS.GT.12) GO TO 900
	RETURN
C
  900   PRINT *, POS,C(1),L(23),MESG(19)
	NEXTPOS=0
	CALL PAUSE
	RETURN
C
  950	PRINT *, POS,C(1),L(23),MESG(20)
	NEXTPOS=0
	CALL PAUSE
	RETURN
C
	END
C
C
*******************************************************************
*
	SUBROUTINE  DISPLAYROOM (ROOMS,PIT,BAT,WUMPUS,ROOMPOS,
     /			         CONNECTION,ISEED,MESG,C,L,NDIM)
*
*******************************************************************
C
	INTEGER*4	CONNECTION(3,12), ISEED, ROOMS(2,12)
	INTEGER*4	PIT(2), BAT(2), WUMPUS, ROOMPOS
C
	REAL*8		RANDOM
C
	CHARACTER*1	C(80)
	CHARACTER*1	L(24)
	CHARACTER*1	DIM		/'<034>'/
	CHARACTER*1	ENDDIM		/'<035>'/
	CHARACTER*1	POS		/'<020>'/
	CHARACTER*2     CPOS
	CHARACTER*8	CDISP
	CHARACTER*6	DISP(4)
	CHARACTER*60	MESG(20)
C
	IF (NDIM.EQ.1) PRINT *, DIM
C
  100	DO 110 I=1,4
	    DISP(I)="      "
  110	CONTINUE
C
	DISP(2)(2:4)="***"
	DISP(4)(2:4)="***"
	DISP(3)(1:5)="*   *"
	LASTPOS=5
	IF (ROOMPOS.GT.9) THEN
	    DISP(2)(5:5)="*"
	    DISP(4)(5:5)="*"
	    DISP(3)(5:6)=" *"
	    LASTPOS=6
	    WRITE (CPOS,1000) ROOMPOS
 1000	    FORMAT (I2)
	ELSE
	    WRITE (CPOS,1001) ROOMPOS
 1001	    FORMAT (I1," ")
	END IF
C
	DISP(3)(3:4)=CPOS
C
	WRITE (CDISP,1002) (CONNECTION(K,ROOMPOS),K=1,3)
 1002   FORMAT (I2,1X,I2,1X,I2)
C
	IF (NDIM.EQ.1) THEN
	    IF (WUMPUS.EQ.ROOMPOS) DISP(1)(5:5)="W"
	    GO TO 120
	END IF
C
	IF (PIT(1).EQ.ROOMPOS.OR.PIT(2).EQ.ROOMPOS) THEN
	    PRINT *, POS,C(1),L(23),MESG(7)
	    CALL PAUSE
	    ROOMPOS=0
	    RETURN
	END IF
C
	IF (WUMPUS.EQ.ROOMPOS) THEN
	    NCHANCE=RANDOM(ISEED)*2
	    IF (NCHANCE.EQ.1) THEN
		PRINT *, POS,C(1),L(23),MESG(18)
		CALL PAUSE
		ROOMPOS=0
		RETURN
	    ELSE
		DISP(1)(5:5)="W"
	    END IF
	END IF
C
  120	DO 500 I=1,3
	    NC=CONNECTION(I,ROOMPOS)
	    IF (DISP(1)(1:1).EQ."P") GO TO 135
	    IF (PIT(1).EQ.NC.OR.PIT(2).EQ.NC) THEN
		IF (NDIM.EQ.1) GO TO 130
		PRINT *, POS,C(1),L(23),MESG(3)
		CALL PAUSE
  130		DISP(1)(1:1)="P"
	    END IF
  135	    IF (DISP(1)(3:3).EQ."B") GO TO 145
     	    IF (BAT(1).EQ.NC.OR.BAT(2).EQ.NC) THEN
		IF (NDIM.EQ.1) GO TO 140
		PRINT *, POS,C(1),L(23),MESG(1)
		CALL PAUSE
  140		DISP(1)(3:3)="B"
	    END IF
  145	    IF (DISP(1)(5:5).EQ."W") GO TO 500
	    IF (WUMPUS.EQ.NC) GO TO 300
	    DO 200 K=1,3
		NC2=CONNECTION(K,NC)
	        IF (WUMPUS.EQ.NC2) GO TO 300
  200	    CONTINUE
	    GO TO 500
  300	    IF (NDIM.EQ.1) GO TO 310
     	    PRINT *, POS,C(1),L(23),MESG(2)
	    CALL PAUSE
  310	    DISP(1)(5:5)="W"
  500	CONTINUE
C
	DO 800 I=0,3
	    PRINT *, POS,C(ROOMS(2,ROOMPOS)),L(ROOMS(1,ROOMPOS)+I),
     /			DISP(I+1)
  800	CONTINUE
	IF (ROOMPOS.EQ.6) THEN
	    PRINT *, POS,C(40),L(22),CDISP
	ELSE
	  PRINT *, POS,C(ROOMS(2,ROOMPOS)-1),L(ROOMS(1,ROOMPOS)+4),CDISP
	END IF
C
	IF (NDIM.EQ.1) THEN
	    PRINT *, ENDDIM
	    RETURN
	END IF
C
	IF (BAT(1).EQ.ROOMPOS.OR.BAT(2).EQ.ROOMPOS) THEN
	    PRINT *, POS,C(1),L(23),MESG(10)
	    CALL PAUSE
	    ROOMPOS=RANDOM(ISEED)*12 + 1
	    PRINT *, POS,C(1),L(24),MESG(11),ROOMPOS
	    CALL PAUSE
	    CALL PAUSE
	    GO TO 100
	END IF
C
	RETURN
	END
C
C
*******************************************************************
*
      	SUBROUTINE  FIRE (COMMANDLINE,J,CONNECTION,WUMPUS,ARROWS,
     /			  ISEED,MESG,C,L,CURRPOS,DONE)
*
*******************************************************************
C
	INTEGER*4	CONNECTION(3,12), ISEED, ARROWS, CURRPOS
	INTEGER*4	PIT(2), BAT(2), WUMPUS, PATH(12), HIT, DONE
C
	CHARACTER*40	COMMANDLINE
	CHARACTER*1	C(80)
	CHARACTER*1	L(24)
	CHARACTER*1	POS		/'<020>'/
	CHARACTER*60	MESG(20)
C
	CALL READFIRECOMMANDLINE (COMMANDLINE,J,PATH,MESG,C,L)
	IF (PATH(1).EQ.0) RETURN
C
	CALL CHECKPATH (CONNECTION,PATH,C,L,MESG,HIT,CURRPOS,ISEED,
     /			WUMPUS,DONE)
	IF (PATH(1).EQ.0) RETURN
C
	IF (HIT.EQ.0) THEN
	    PRINT *, POS,C(1),L(23),MESG(5)
	    CALL PAUSE
	    ARROWS=ARROWS-1
	    PRINT *, POS,C(13),L(21),ARROWS
	    IF (ARROWS.EQ.0) THEN
		PRINT *, POS,C(1),L(23),MESG(12),WUMPUS
		CALL  PAUSE
		DONE=1
	    END IF
	ELSE
	    IF (HIT.EQ.-1) RETURN
	    PRINT *, POS,C(1),L(23),MESG(4)
	    CALL PAUSE
	    DONE=1
	END IF
C
	RETURN
	END
C
C
*******************************************************************
*
      	SUBROUTINE  READFIRECOMMANDLINE (COMMANDLINE,J,PATH,MESG,C,L)
*
*******************************************************************
C
	INTEGER*4	PATH(12)
C
	CHARACTER*1	POS 		/'<020>'/
	CHARACTER*1	C(80)
	CHARACTER*1	L(24)
	CHARACTER*2	CPATH
	CHARACTER*60	MESG(20)
	CHARACTER*40	COMMANDLINE
C
	NCNT=1
	NPOS=J+1
C
	DO 100 I=1,12
	    PATH(I)=0
  100	CONTINUE
C
  200	IF (COMMANDLINE(NPOS:NPOS).GT." ") GO TO 300
	NPOS=NPOS+1
	IF (NPOS.EQ.41) GO TO 900
	GO TO 200
C
  300   NSTART=NPOS
	NPOS=NPOS+1
C
  400   IF (COMMANDLINE(NPOS:NPOS).LE." ") GO TO 500
	NPOS=NPOS+1
	IF (NPOS.EQ.41) GO TO 500
	GO TO 400
C
  500   NEND=NPOS-1
	NLEN=NEND-NSTART+1
	CPATH="  "
	IF (NLEN.EQ.1) CPATH(2:2)=COMMANDLINE(NSTART:NEND)
	IF (NLEN.EQ.2) CPATH(1:2)=COMMANDLINE(NSTART:NEND)
	IF (NLEN.GT.2) THEN
	    PRINT *, POS,C(1),L(23),MESG(17)
	    CALL PAUSE
	    PATH(1)=0
	    RETURN
	END IF
	READ (CPATH,1000) PATH(NCNT)
 1000   FORMAT (I2)
	IF (PATH(NCNT).GT.12) THEN
	    PRINT *, POS,C(1),L(23),MESG(17)
	    CALL PAUSE
	    PATH(1)=0
	    RETURN
	END IF
	NCNT=NCNT+1
	IF (NCNT.EQ.13) GO TO 900
	NPOS=NPOS+1
	IF (NPOS.LE.40) GO TO 200
C
  900	RETURN
C
	END
C
C
*******************************************************************
*
	SUBROUTINE  CHECKPATH (CONNECTION,PATH,C,L,MESG,HIT,CURRPOS,
     /				ISEED,WUMPUS,DONE)
*
*******************************************************************
C
	INTEGER*4	HIT, PATH(12), CONNECTION(3,12), CURRPOS
	INTEGER*4	WUMPUS, DONE, ISEED
C
	REAL*8		RANDOM
C
	CHARACTER*1	C(80)
	CHARACTER*1	L(24)
	CHARACTER*1	POS		/'<020>'/
	CHARACTER*60	MESG(20)
C
	NPOS=CURRPOS
	IF (NPOS.EQ.0) THEN
	    PRINT *, POS,C(1),L(23),MESG(13)
	    CALL PAUSE
	    HIT=-1
	    RETURN
	END IF
C
	HIT=0
C
	DO 700 I=1,12
	    IF (PATH(I).EQ.0) RETURN
	    DO 100 K=1,3
		IF (CONNECTION(K,NPOS).EQ.PATH(I)) GO TO 200
  100	    CONTINUE
	    PRINT *, POS,C(1),L(23),MESG(6)
	    CALL PAUSE
	    PATH(1)=0
	    RETURN
C
  200	    NPOS=PATH(I)
	    IF (NPOS.EQ.WUMPUS) THEN
		HIT=1
		RETURN
	    END IF
C
	    IF (NPOS.EQ.CURRPOS) THEN
		NCHANCE=RANDOM(ISEED)*2
		IF (NCHANCE.EQ.1) THEN
		    PRINT *, C(1),L(23),MESG(8)
		    CALL PAUSE
		    PATH(1)=0
		    DONE=1
		END IF
	    END IF
C
  700	CONTINUE
C
	RETURN
	END
C
C
*******************************************************************
*
      	SUBROUTINE INITIALIZE (CONNECTION,ARROWS,PIT,BAT,WUMPUS,
     /			       ISEED)
*
*******************************************************************
C
	INTEGER*4	CONNECTION(3,12), ISEED, ARROWS
	INTEGER*4	PIT(2), BAT(2), WUMPUS
C
	REAL*8		RANDOM
C
	CALL SETUPCONNECTIONS (CONNECTION,ISEED)
	CALL SETUPPITSANDBATS (PIT,BAT,ISEED)
C
C ASSIGN WUMPUS POSITION
C ----------------------
C
	WUMPUS=RANDOM(ISEED)*12 + 1
C
C ASSIGN STARTING NUMBER OF ARROWS
C --------------------------------
C
	ARROWS=5
C
      	RETURN
      	END
C
C
*******************************************************************
*
      	SUBROUTINE  SETUPCONNECTIONS (CONNECTION,ISEED)
*
*******************************************************************
C
C   This routine sets up the connections between the rooms, giving
C   each room three possible choices for rooms the player can travel
C   to.
C
	REAL*8		RANDOM
C
	INTEGER*4	ISEED		
	INTEGER*4	CONNECTION(3,12), JC(3)
C
C INITIALIZE CONNECTION ARRAY
C ---------------------------
C
  100	DO 200 I=1,12
	    DO 200 J=1,3
		CONNECTION(J,I)=0
  200	CONTINUE
C
C MAKE 18 CONNECTIONS - 2 FROM ROOMS 1 TO 6, AND 1 FROM ROOMS 7 TO 12
C   -- Only 18 are necessary, since if I is connected to J, then J is
C      automatically connected to I.
C   -- If a connection can't be made between I and J, J is incremented
C      and checked a total of 12 times.  JCYCLE counts the number of
C      times J is incremented and checked against I.  If no connection
C      is possible after cycling 12 times, then begin all over with
C      the initialization phase.
C -------------------------------------------------------------------
C
	DO 500 ICK=1,18
	    J=RANDOM(ISEED)*12 + 1
	    JCYCLE=1
	    I=ICK
	    IF (I.GT.12) I=I-12
  300	    CALL CHECKCONNECTION (CONNECTION,I,J,IPOS,JPOS)
	    IF (JPOS.EQ.0) THEN
		J=J+1
		IF (J.EQ.13) J=1
		JCYCLE=JCYCLE+1
		IF (JCYCLE.EQ.13) THEN
C		    PRINT *, "BAD SETUP DEAL."
		    GO TO 100
		END IF
		GO TO 300
	    END IF
	    CONNECTION (JPOS,J)=I
	    CONNECTION (IPOS,I)=J
  500	CONTINUE
C
C ORDER CONNECTIONS (In room 2, the order would be 1, 10, 5 for example)
C ----------------------------------------------------------------------
C
	DO 800 I=1,12
	    DO 600 J=1,3
	        JC(J)=CONNECTION(J,I)
		IF (JC(J).LT.I) JC(J)=JC(J) + 12
  600	    CONTINUE
	    IF (JC(2).GT.JC(3)) GO TO 650
	    JCSAVE=JC(2)
	    JC(2)=JC(3)
	    JC(3)=JCSAVE
C
  650	    IF (JC(1).GT.JC(2)) GO TO 700
	    JCSAVE=JC(1)
	    JC(1)=JC(2)
	    JC(2)=JCSAVE
C
  700	    IF (JC(2).GT.JC(3)) GO TO 750
	    JCSAVE=JC(2)
	    JC(2)=JC(3)
	    JC(3)=JCSAVE
C
  750	    DO 760 J=1,3
		IF (JC(J).GT.12) JC(J)=JC(J) - 12
		CONNECTION(J,I)= JC(J)
  760	    CONTINUE
  800	CONTINUE
C
      	RETURN
      	END
C
C
*******************************************************************
*
        SUBROUTINE  CHECKCONNECTION (CONNECTION,I,J,IPOS,JPOS)
*
*******************************************************************
C
C   This subroutine checks whether a connection has already been
C   established between I and J, and returns the next available
C   positions for placement of the connection if none has been made.
C   If I=J, then don't make a connection, and if 3 connections have
C   already been made from either I or J, then don't make another one.
C
C
	INTEGER*4	CONNECTION(3,12), CONNECT1, CONNECT2, CONNECT3
C
C SET UP CURRENT J CONNECTIONS
C ----------------------------
C
	CONNECT1=CONNECTION(1,J)
	CONNECT2=CONNECTION(2,J)
	CONNECT3=CONNECTION(3,J)
C
C IF RETURN TO MAIN PROGRAM IS MADE WITH JPOS=0, THEN NO CONNECTION
C IS POSSIBLE FOR THIS I AND J.
C -----------------------------------------------------------------
C
	JPOS=0
C
C DON'T MAKE A CONNECTION IF I=J
C ------------------------------
C
	IF (J.EQ.I) RETURN
C
C FIND THE FIRST OPEN CONNECTION SPOT FOR J  (CONNECTx = 0)
C   -- BE SURE NO CONNECTION HAS YET BEEN MADE TO I  (CONNECTx = I)
C -----------------------------------------------------------------
C
	IF (CONNECT1.EQ.0) THEN
	    JPOS=1
	    GO TO 200
	END IF
C
C ONE CONNECTION ALREADY MADE FROM J
C ----------------------------------
C
	IF (CONNECT1.NE.I.AND.CONNECT2.EQ.0) THEN
	    JPOS=2
	    GO TO 200
	END IF
C
C TWO CONNECTIONS ALREADY MADE FROM J OR FIRST ONE IS TO I
C --------------------------------------------------------
C
	IF (CONNECT1.NE.I.AND.CONNECT2.NE.I.AND.CONNECT3.EQ.0) THEN
	    JPOS=3
	    GO TO 200
	END IF
C
C NO CONNECTION AVAILABLE BETWEEN I AND J
C ---------------------------------------
C
	RETURN
C
C BEGIN CHECKING I FOR AVAILABLE POSITION
C ---------------------------------------
C
  200	IPOS=3
C
C SET UP CURRENT I CONNECTIONS
C ----------------------------
C
	CONNECT1=CONNECTION(1,I)
	CONNECT2=CONNECTION(2,I)
	CONNECT3=CONNECTION(3,I)
C
C CHECK WHETHER THREE CONNECTIONS HAVE BEEN MADE
C ----------------------------------------------
C
	IF (CONNECT3.NE.0) THEN
	    JPOS=0
	    RETURN
	END IF
C
C A CONNECTION IS AVAILABLE - ASSIGN FIRST ZERO POSITION
C ------------------------------------------------------
C
	IF (CONNECT2.EQ.0) IPOS=2
	IF (CONNECT1.EQ.0) IPOS=1
C
	RETURN
	END
C
C
*******************************************************************
*
        SUBROUTINE  SETUPPITSANDBATS (PIT,BAT,ISEED)
*
*******************************************************************
C
	INTEGER*4   PIT(2), BAT(2), ISEED
C
	REAL*8	    RANDOM
C
C ASSIGN BAT 1
C ------------
C
	J=RANDOM(ISEED)*12 + 1
	BAT(1)=J
C
C ASSIGN BAT 2
C ------------
C
  200	J=RANDOM(ISEED)*12 + 1
	IF (J.EQ.BAT(1)) GO TO 200
	BAT(2)=J
C
C ASSIGN PIT 1
C ------------
C
	J=RANDOM(ISEED)*12 + 1
	PIT(1)=J
C
C ASSIGN PIT 2
C ------------
C
  300	J=RANDOM(ISEED)*12 + 1
	IF (J.EQ.PIT(1)) GO TO 300
	PIT(2)=J
C
	RETURN
	END
C
C
*******************************************************************
*
        SUBROUTINE  SETUPPOSITIONS (C,L)
*
*******************************************************************
C
        CHARACTER*1    C(80)
        CHARACTER*1    L(24)
C
C SET UP SCREEN COLUMNS
C ---------------------
C
        C(1)="<000>"
        C(2)="<001>"
        C(3)="<002>"
        C(4)="<003>"
        C(5)="<324>"
        C(6)="<005>"
        C(7)="<006>"
        C(8)="<007>"
        C(9)="<010>"
        C(10)="<131>"
        C(11)="<012>"
        C(12)="<013>"
        C(13)="<014>"
        C(14)="<015>"
        C(15)="<016>"
        C(16)="<017>"
        C(17)="<020>"
        C(18)="<021>"
        C(19)="<022>"
        C(20)="<023>"
        C(21)="<024>"
        C(22)="<025>"
        C(23)="<026>"
        C(24)="<027>"
        C(25)="<030>"
        C(26)="<031>"
        C(27)="<032>"
        C(28)="<033>"
        C(29)="<034>"
        C(30)="<035>"
        C(31)="<036>"
        C(32)="<037>"
        C(33)="<040>"
        C(34)="<041>"
        C(35)="<042>"
        C(36)="<043>"
        C(37)="<044>"
        C(38)="<045>"
        C(39)="<046>"
        C(40)="<047>"
        C(41)="<050>"
        C(42)="<051>"
        C(43)="<052>"
        C(44)="<053>"
        C(45)="<054>"
        C(46)="<055>"
        C(47)="<056>"
        C(48)="<057>"
        C(49)="<060>"
        C(50)="<061>"
        C(51)="<062>"
        C(52)="<063>"
        C(53)="<064>"
        C(54)="<065>"
        C(55)="<066>"
        C(56)="<067>"
        C(57)="<070>"
        C(58)="<071>"
        C(59)="<072>"
        C(60)="<073>"
        C(61)="<074>"
        C(62)="<075>"
        C(63)="<076>"
        C(64)="<077>"
        C(65)="<100>"
        C(66)="<101>"
        C(67)="<102>"
        C(68)="<103>"
        C(69)="<104>"
        C(70)="<105>"
        C(71)="<106>"
        C(72)="<107>"
        C(73)="<110>"
        C(74)="<111>"
        C(75)="<112>"
        C(76)="<113>"
        C(77)="<114>"
        C(78)="<115>"
        C(79)="<116>"
        C(80)="<117>"
C
C SET UP SCREEN LINES
C -------------------
C
        L(1)="<000>"
        L(2)="<001>"
        L(3)="<002>"
        L(4)="<003>"
        L(5)="<004>"
        L(6)="<005>"
        L(7)="<006>"
        L(8)="<007>"
        L(9)="<010>"
        L(10)="<041>"
        L(11)="<012>"
        L(12)="<013>"
        L(13)="<014>"
        L(14)="<015>"
        L(15)="<016>"
        L(16)="<017>"
        L(17)="<020>"
        L(18)="<021>"
        L(19)="<022>"
        L(20)="<023>"
        L(21)="<024>"
        L(22)="<025>"
        L(23)="<026>"
        L(24)="<027>"
C
	RETURN
	END
C
C
*******************************************************************
*
      	SUBROUTINE  STARTSCREEN (C,L)
*
*******************************************************************
C
	CHARACTER*80	SCR(23)
	CHARACTER*1	C(80)
	CHARACTER*1	L(24)
	CHARACTER*1	DIM		/'<034>'/
	CHARACTER*1	ENDDIM		/'<035>'/
	CHARACTER*1	ROLLDIS		/'<023>'/
	CHARACTER*1	POS		/'<020>'/
	CHARACTER*1	BLANK		/'<014>'/
C
	PRINT *, BLANK
	PRINT *, DIM
	PRINT *, ROLLDIS
C
	SCR( 1)="WUMPUS"
        SCR( 2)="======                            ****"
        SCR( 3)="                                 * 12 *"
        SCR( 4)="                                  ****"
        SCR( 5)="                       ****                  ***"
        SCR( 6)="                      * 11 *                * 1 *"
        SCR( 7)="                       ****                  ***"
        SCR( 8)="            ****                                        ***"
        SCR( 9)="           * 10 *                                      * 2 *"
        SCR(10)="            ****                                        ***"
        SCR(11)=" ***                                                               ***"
        SCR(12)="* 9 *                                                             * 3 *"
        SCR(13)=" ***                                                               ***"
        SCR(14)="            ***                                         ***"
        SCR(15)="           * 8 *                                       * 4 *"
        SCR(16)="            ***                                         ***"
        SCR(17)="                       ***                   ***"
        SCR(18)="                      * 7 *                 * 5 *"
        SCR(19)="                       ***                   ***"
        SCR(20)="                                  ***"
        SCR(21)="  # Arrows:                      * 6 *"
        SCR(22)="                                  ***"
C
	DO 100 I=1,22
	    PRINT *, POS,C(1),L(I),SCR(I)
  100	CONTINUE
C
	PRINT *, ENDDIM
C
      	RETURN
      	END
C
C
*******************************************************************
*
	SUBROUTINE  SETUPROOMS (ROOMS)
*
*******************************************************************
C
	INTEGER*4    ROOMS(2,12)
C
	ROOMS(1,1) =  4
	ROOMS(2,1) = 45
	ROOMS(1,2) =  7
	ROOMS(2,2) = 56
	ROOMS(1,3) = 10
	ROOMS(2,3) = 67
	ROOMS(1,4) = 13
	ROOMS(2,4) = 56
	ROOMS(1,5) = 16
	ROOMS(2,5) = 45
	ROOMS(1,6) = 19
	ROOMS(2,6) = 34
	ROOMS(1,7) = 16
	ROOMS(2,7) = 23
	ROOMS(1,8) = 13
	ROOMS(2,8) = 12
	ROOMS(1,9) = 10
	ROOMS(2,9) =  1
	ROOMS(1,10)=  7
	ROOMS(2,10)= 12
	ROOMS(1,11)=  4
	ROOMS(2,11)= 23
	ROOMS(1,12)=  1
	ROOMS(2,12)= 34
C
	RETURN
	END
C
C
*******************************************************************
*
	SUBROUTINE  SETUPMESSAGES (MESG)
*
*******************************************************************
C
	CHARACTER*1	ERASELINE		/'<213>'/
	CHARACTER*60	MESG(20)
C
	MESG(1)="You can hear a Bat from this room."
	MESG(2)="You can smell the WUMPUS from this room."
	MESG(3)="There is a draft from a Pit in this room."
	MESG(4)="You have killed the WUMPUS!  Congratulations."
	MESG(5)="Your shot missed."
	MESG(6)="There is no such path through the rooms."
	MESG(7)="You have fallen deep into a Pit . . .  You Died."
	MESG(8)="You have shot yourself. Ridiculous! Game over."
	MESG(9)="Would you like to play again? "
	MESG(10)="A Bat is taking you for a long ride."
	MESG(11)="You have been dropped in Room #"
	MESG(12)="You are out of arrows. You lose. The WUMPUS was in room #"
	MESG(13)="You can not fire on your first turn. MOVE first."
	MESG(14)="Not a valid command. (MOVE, FIRE)  Please Re-enter."
	MESG(15)="Available commands:  MOVE (M)   FIRE (F)"
	MESG(16)="Would you like instructions? "
	MESG(17)="Bad Room Number Specified with Fire Command."
	MESG(18)="You have been eaten by the WUMPUS."
	MESG(19)="Bad argument to MOVE command."
	MESG(20)="Too many arguments to MOVE command."
C
	DO 500 I=1,20
	    DO 200 J=60,1,-1
		IF (MESG(I)(J:J).GT." ") GO TO 300
  200	    CONTINUE
  300	    MESG(I)(J+1:J+2)=" "//ERASELINE
	    IF (I.EQ.9.OR.I.EQ.11.OR.I.EQ.12.OR.I.EQ.16) THEN
		DO 400 K=J+3,60
		    MESG(I)(K:K)="<NUL>"
  400		CONTINUE
	    END IF
  500	CONTINUE
C
	RETURN
	END
C
C
*******************************************************************
*
	SUBROUTINE  INSTRUCTIONS (C,L,MESG)
*
*******************************************************************
C
	CHARACTER*60	MESG(20)
	CHARACTER*1	CONT
	CHARACTER*1	C(80)
	CHARACTER*1	L(24)
	CHARACTER*1	ROLLDIS		/'<023>'/
	CHARACTER*1	POS		/'<020>'/
	CHARACTER*1	ERASELINE	/'<213>'/
	CHARACTER*80	SCR(23)
C
	PRINT *, ROLLDIS
C
	PRINT *, POS,C(1),L(23),MESG(16)
	READ (*,1000) CONT
	IF (CONT.NE."Y") GO TO 900
C
	DO 100 I=1,23
	    SCR(I)=" "
  100	CONTINUE
C
	SCR(1)="                     WUMPUS INSTRUCTIONS"
	SCR(2)="                     ==================="
	SCR(3)="   Welcome to WUMPUS.  You are trying to shoot the WUMPUS before he eats you,"
	SCR(4)="taking care not to fall in any pits along the way.  You will start with 5 arrows"
	SCR(5)="and can fire them at any time to try to hit the WUMPUS.  He can be in any one of"
	SCR(6)="the 12 rooms on the game screen."
	SCR(7)="   You will choose the room you want to start in, by typing MOVE x, where x is"
	SCR(8)="the number of the room (1 to 12) you want to start in.  Whenever you enter a"
	SCR(9)="room, the other rooms connected to it will be shown by numbers surrounding it."
	SCR(10)="You can move to any of those 3 rooms by typing MOVE x, where x is any of the 3"
	SCR(11)="connected rooms.  If you happen to move into a room with a Pit in it, you will"
	SCR(12)="fall into the pit, die, and therefore lose the game.  If you move into a room"
	SCR(13)="with a Bat in it, the Bat will pick you up and drop you randomly in one of the"
	SCR(14)="12 rooms.  There are two Pits and two Bats.  When you are one room away from a"
	SCR(15)="Pit (via a connection), you will feel a draft.  When you are one room away from"
	SCR(16)="a Bat (via a connection), you will hear him.  When you are one or two rooms away"
	SCR(17)="from the WUMPUS (via a connection), you will smell the stinking ogre.  If you"
	SCR(18)="land in the same room as the WUMPUS, there is a 50/50 chance he will eat you."
	SCR(19)="If he does not eat you, he will not tell you he is there, but will smell."
	SCR(20)="   If you believe you have determined where the WUMPUS is, you can fire an arrow"
	SCR(21)="by typing FIRE x y z, where x y z are the room numbers of the path you want the"
	SCR(22)="arrow to take (following connections).  The path can be as long as you want,"
	SCR(23)="but be careful you don't shoot yourself. You lose if you run out of arrows."
C
	DO 200 I=1,23
	    PRINT *, POS,C(1),L(I),SCR(I)
  200	CONTINUE
C
	PRINT *, POS,C(3),L(24),"TYPE NEW-LINE TO BEGIN YOUR SEARCH FOR THE WUMPUS "
	READ (*,1000) CONT
 1000   FORMAT (A1)
C
	CALL STARTSCREEN (C,L)
C
  900	PRINT *, POS,C(13),L(21),"5"
	PRINT *, POS,C(1),L(23),ERASELINE
C
	RETURN
	END
C
C
*******************************************************************
*
      	SUBROUTINE  DISPLAYCONNECTION (CONNECTION,CURRPOS,NEXTPOS,C,L,
     /				       MESG,CHOICES)
*
*******************************************************************
C
	INTEGER*4	CONNECTION(3,12), CURRPOS, CHOICES(66)
C
	CHARACTER*1	CHAR1		/'/'/
	CHARACTER*1	CHAR2		/'\'/
	CHARACTER*1	CHAR3		/'-'/
	CHARACTER*1	CHAR4		/'_'/
	CHARACTER*1	CHAR5		/'!'/
	CHARACTER*1	CHAR6		/'.'/
	CHARACTER*1	C(80)
	CHARACTER*1	L(24)
	CHARACTER*1	POS		/'<020>'/
	CHARACTER*60	MESG(20)
C
	IF (CURRPOS.EQ.0) RETURN
C
	DO 50 I=1,3
	    NCK=CONNECTION(I,CURRPOS)
	    IF (NCK.EQ.NEXTPOS) GO TO 80
   50	CONTINUE
	PRINT *, POS,C(1),L(23),MESG(6)
	CALL PAUSE
	NEXTPOS=0
	RETURN
C
   80	NCK1=CURRPOS*100 + NEXTPOS
	NCK2=NEXTPOS*100 + CURRPOS
C
	DO 100 I=1,66
	    IF (CHOICES(I).EQ.NCK1) GO TO 200
	    IF (CHOICES(I).EQ.NCK2) GO TO 200
  100   CONTINUE
	PRINT *, "BAD POSITIONS.  NCK = ",NCK1
	RETURN
C
  200   IF (I.EQ.64) GO TO 164
        IF (I.EQ.65) GO TO 165
	IF (I.EQ.66) GO TO 166
        GO TO (101,102,103,104,105,106,107,108,109,110,111,112,113,114,
     /		115,116,117,118,119,120,121,122,123,124,
     /		125,126,127,128,129,130,131,132,133,134,
     /		135,136,137,138,139,140,141,142,143,144,
     /		145,146,147,148,149,150,151,152,153,154,
     /		155,156,157,158,159,160,161,162,163), I
C
  101	CALL LINEDISPLAY ( 2,34, 3, 5,CHAR2,C,L)
	RETURN
C
  102	CALL LINEDISPLAY (99, 3,40,58,CHAR3,C,L)
	CALL LINEDISPLAY ( 0,58, 3, 7,CHAR5,C,L)
	RETURN
C
  103	CALL LINEDISPLAY (99, 2,39,70,CHAR3,C,L)
	CALL LINEDISPLAY ( 0,70, 2,10,CHAR5,C,L)
	RETURN
C
  104	CALL LINEDISPLAY ( 2,27, 5,14,CHAR2,C,L)
	RETURN
C
  105	CALL LINEDISPLAY ( 1,31, 5,16,CHAR2,C,L)
	RETURN
C
  106	CALL LINEDISPLAY ( 0,36, 5,19,CHAR5,C,L)
	RETURN
C
  107	CALL LINEDISPLAY (-1,41, 5,16,CHAR1,C,L)
	RETURN
C
  108	CALL LINEDISPLAY (-2,47, 5,14,CHAR1,C,L)
	RETURN
C
  109	CALL LINEDISPLAY (99, 2, 3,34,CHAR3,C,L)
	CALL LINEDISPLAY ( 0, 3, 2,10,CHAR5,C,L)
	RETURN
C
  110	CALL LINEDISPLAY (99, 3,14,33,CHAR3,C,L)
	CALL LINEDISPLAY ( 0,14, 3, 7,CHAR5,C,L)
	RETURN
C
  111	CALL LINEDISPLAY (-2,39, 3, 5,CHAR1,C,L)
	RETURN
C
  112	CALL LINEDISPLAY ( 2,38, 6, 8,CHAR2,C,L)
	RETURN
C
  113	CALL LINEDISPLAY (99, 6,50,68,CHAR3,C,L)
	CALL LINEDISPLAY ( 0,68, 6,10,CHAR5,C,L)
	RETURN
C
  114	CALL LINEDISPLAY ( 1,41, 8,14,CHAR2,C,L)
	RETURN
C
  115 	CALL LINEDISPLAY ( 0,47, 8,16,CHAR5,C,L)
	RETURN
C
  116	CALL LINEDISPLAY (-1,55, 8,19,CHAR1,C,L)
	RETURN
C
  117	CALL LINEDISPLAY (-2,60, 8,16,CHAR1,C,L)
	RETURN
C
  118	CALL LINEDISPLAY (-4,72, 7,14,CHAR6,C,L)
	RETURN
C
  119	CALL LINEDISPLAY (99,12, 5,35,CHAR3,C,L)
	CALL LINEDISPLAY (-2,59, 8,12,CHAR1,C,L)
	RETURN
C
  120	CALL LINEDISPLAY (99, 9,18,44,CHAR3,C,L)
	CALL LINEDISPLAY ( 0,45, 8, 8,CHAR5,C,L)
	RETURN
C
  121	CALL LINEDISPLAY (99, 6,29,44,CHAR3,C,L)
	RETURN
C
  122	CALL LINEDISPLAY ( 2,43, 9,11,CHAR2,C,L)
	RETURN
C
  123	CALL LINEDISPLAY ( 0,58,11,13,CHAR5,C,L)
	RETURN
C
  124	CALL LINEDISPLAY (-1,68,11,18,CHAR1,C,L)
	RETURN
C
  125	CALL LINEDISPLAY (-2,76,10,19,CHAR1,C,L)
	RETURN
C
  126	CALL LINEDISPLAY (-4,98,11,17,CHAR6,C,L)
	RETURN
C
  127	CALL LINEDISPLAY (99,14,16,51,CHAR3,C,L)
	CALL LINEDISPLAY (-2,79,11,14,CHAR1,C,L)
	RETURN
C
  128	CALL LINEDISPLAY (99,12, 6,48,CHAR3,C,L)
	CALL LINEDISPLAY (-2,72, 9,11,CHAR1,C,L)
	RETURN
C
  129	CALL LINEDISPLAY (99, 9,18,55,CHAR3,C,L)
	RETURN
C
  130	CALL LINEDISPLAY (99, 8,28,56,CHAR3,C,L)
	RETURN
C
  131	CALL LINEDISPLAY (-2,91,13,15,CHAR1,C,L)
	RETURN
C
  132	CALL LINEDISPLAY ( 0,68,14,18,CHAR5,C,L)
	CALL LINEDISPLAY (99,18,50,68,CHAR3,C,L)
	RETURN
C
  133	CALL LINEDISPLAY ( 0,69,14,21,CHAR5,C,L)
	CALL LINEDISPLAY (99,21,39,69,CHAR3,C,L)
	RETURN
C
  134	CALL LINEDISPLAY (99,12,36,66,CHAR3,C,L)
	CALL LINEDISPLAY (-2,60,12,16,CHAR1,C,L)
	RETURN
C
  135	CALL LINEDISPLAY (99,12,18,66,CHAR3,C,L)
	CALL LINEDISPLAY (-1,30,12,14,CHAR1,C,L)
	RETURN
C
  136	CALL LINEDISPLAY (99,12, 6,66,CHAR3,C,L)
	RETURN
C
  137	CALL LINEDISPLAY (99,12,18,66,CHAR3,C,L)
	CALL LINEDISPLAY ( 1, 6,11,12,CHAR2,C,L)
	RETURN
C
  138	CALL LINEDISPLAY (99,12,36,66,CHAR3,C,L)
	CALL LINEDISPLAY ( 2,12, 8,12,CHAR2,C,L)
	RETURN
C
  139	CALL LINEDISPLAY (-2,85,15,17,CHAR1,C,L)
	RETURN
C
  140	CALL LINEDISPLAY ( 0,58,17,21,CHAR5,C,L)
	CALL LINEDISPLAY (99,21,39,58,CHAR3,C,L)
	RETURN
C
  141	CALL LINEDISPLAY (99,15,28,56,CHAR3,C,L)
	CALL LINEDISPLAY (-1,43,16,16,CHAR1,C,L)
	RETURN
C
  142	CALL LINEDISPLAY (99,14,16,56,CHAR3,C,L)
	RETURN
C
  143	CALL LINEDISPLAY (99,12, 6,55,CHAR3,C,L)
	CALL LINEDISPLAY ( 1,43,13,13,CHAR2,C,L)
	RETURN
C
  144	CALL LINEDISPLAY (99,14,20,55,CHAR3,C,L)
	CALL LINEDISPLAY ( 1, 6,11,14,CHAR2,C,L)
	RETURN
C
  145	CALL LINEDISPLAY ( 4, 0, 7,14,CHAR6,C,L)
	RETURN
C
  146	CALL LINEDISPLAY (-2,81,19,21,CHAR1,C,L)
	RETURN
C
  147	CALL LINEDISPLAY (99,18,28,44,CHAR3,C,L)
	RETURN
C
  148	CALL LINEDISPLAY (99,15,17,45,CHAR3,C,L)
	CALL LINEDISPLAY ( 1,30,16,16,CHAR2,C,L)
	RETURN
C
  149	CALL LINEDISPLAY (99,12, 6,34,CHAR3,C,L)
	CALL LINEDISPLAY ( 2,11,12,17,CHAR2,C,L)
	RETURN
C
  150	CALL LINEDISPLAY ( 4,-23,10,17,CHAR6,C,L)
	RETURN
C
  151	CALL LINEDISPLAY ( 2,11, 8,17,CHAR2,C,L)
	RETURN
C
  152	CALL LINEDISPLAY ( 2,-10,19,21,CHAR2,C,L)
	RETURN
C
  153	CALL LINEDISPLAY (99,21,14,33,CHAR3,C,L)
	CALL LINEDISPLAY ( 0,14,17,21,CHAR5,C,L)
	RETURN
C
  154 	CALL LINEDISPLAY (99,22, 3,34,CHAR3,C,L)
	CALL LINEDISPLAY ( 0, 3,14,22,CHAR5,C,L)
	RETURN
C
  155	CALL LINEDISPLAY ( 2,-3,10,19,CHAR2,C,L)
	RETURN
C
  156	CALL LINEDISPLAY ( 0,36,11,19,CHAR5,C,L)
	CALL LINEDISPLAY ( 2,14, 7,10,CHAR2,C,L)
	RETURN
C
  157	CALL LINEDISPLAY ( 2,-13,15,17,CHAR2,C,L)
	RETURN
C
  158	CALL LINEDISPLAY (99,18, 4,22,CHAR3,C,L)
	CALL LINEDISPLAY ( 0, 4,14,18,CHAR5,C,L)
	RETURN
C
  159	CALL LINEDISPLAY ( 2,-8,11,16,CHAR2,C,L)
	RETURN
C
  160	CALL LINEDISPLAY ( 0,25, 8,16,CHAR5,C,L)
	RETURN
C
  161	CALL LINEDISPLAY ( 2,-20,13,15,CHAR2,C,L)
	RETURN
C
  162	CALL LINEDISPLAY ( 0,14,11,13,CHAR5,C,L)
	RETURN
C
  163	CALL LINEDISPLAY (-2,40, 8,13,CHAR1,C,L)
	RETURN
C
  164	CALL LINEDISPLAY (-2,29, 9,11,CHAR1,C,L)
	RETURN
C
  165	CALL LINEDISPLAY ( 0, 3, 4,10,CHAR5,C,L)
	CALL LINEDISPLAY (99, 3, 3,33,CHAR3,C,L)
	RETURN
C
  166	CALL LINEDISPLAY (-2,34, 6, 8,CHAR1,C,L)
	RETURN
C
	END
C
C
*******************************************************************
*
      	SUBROUTINE  LINEDISPLAY (SLOPE,INTERC,X1,X2,CHAR,C,L)
*
*******************************************************************
C
	CHARACTER*1	POS		/'<020>'/
	CHARACTER*1	C(80)
	CHARACTER*1	L(24)
	CHARACTER*1	CHAR
C
	INTEGER*4	X, Y, SLOPE, X1, X2
C
	IF (SLOPE.EQ.99) GO TO 200
C
	DO 100 X=X1,X2
	    Y=SLOPE*X + INTERC
	    PRINT *, POS,C(Y),L(X),CHAR
  100   CONTINUE
	RETURN
C
  200   X=INTERC
	DO 300 Y=X1,X2
	    PRINT *, POS,C(Y),L(X),CHAR
  300   CONTINUE
C
	RETURN
	END
C
C
*******************************************************************
*
      	SUBROUTINE  SETUPCHOICES (CHOICES)
*
*******************************************************************
C
	INTEGER*4	CHOICES(66)
C
	CHOICES( 1)=1201
	CHOICES( 2)=1202
	CHOICES( 3)=1203
	CHOICES( 4)=1204
	CHOICES( 5)=1205
	CHOICES( 6)=1206
	CHOICES( 7)=1207
	CHOICES( 8)=1208
	CHOICES( 9)=1209
	CHOICES(10)=1210
	CHOICES(11)=1211
	CHOICES(12)= 102
	CHOICES(13)= 103
	CHOICES(14)= 104
	CHOICES(15)= 105
	CHOICES(16)= 106
	CHOICES(17)= 107
	CHOICES(18)= 108
	CHOICES(19)= 109
	CHOICES(20)= 110
	CHOICES(21)= 111
	CHOICES(22)= 203
	CHOICES(23)= 204
	CHOICES(24)= 205
	CHOICES(25)= 206
	CHOICES(26)= 207
	CHOICES(27)= 208
	CHOICES(28)= 209
	CHOICES(29)= 210
	CHOICES(30)= 211
	CHOICES(31)= 304
	CHOICES(32)= 305
	CHOICES(33)= 306
	CHOICES(34)= 307
	CHOICES(35)= 308
	CHOICES(36)= 309
	CHOICES(37)= 310
	CHOICES(38)= 311
	CHOICES(39)= 405
	CHOICES(40)= 406
	CHOICES(41)= 407
	CHOICES(42)= 408
	CHOICES(43)= 409
	CHOICES(44)= 410
	CHOICES(45)= 411
	CHOICES(46)= 506
	CHOICES(47)= 507
	CHOICES(48)= 508
	CHOICES(49)= 509
	CHOICES(50)= 510
	CHOICES(51)= 511
	CHOICES(52)= 607
	CHOICES(53)= 608
	CHOICES(54)= 609
	CHOICES(55)= 610
	CHOICES(56)= 611
	CHOICES(57)= 708
	CHOICES(58)= 709
	CHOICES(59)= 710
	CHOICES(60)= 711
	CHOICES(61)= 809
	CHOICES(62)= 810
	CHOICES(63)= 811
	CHOICES(64)= 910
	CHOICES(65)= 911
	CHOICES(66)=1011
C
	RETURN
	END
C
C
*********************************************************************
*
	SUBROUTINE  PAUSE
*
*********************************************************************
C
	INTEGER*4	ITIME(3)
C
	CALL TIME(ITIME)
	NTIME=3600*ITIME(1) + 60*ITIME(2) + ITIME(3)
C
  100	CALL TIME(ITIME)
	KTIME=3600*ITIME(1) + 60*ITIME(2) + ITIME(3)
	IF (KTIME-NTIME.LT.2) GO TO 100
C
	RETURN
	END
