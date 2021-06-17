      PROGRAM FOOBAR
C            By Stephen Schwei 1/15/82
C
      COMMON IMOVE(1296),LIMIT,NTESTVAL
      COMMON /IO/ MINPUT(12),MOVENUM
      COMMON /CH/ MTABLE(12,4)
      COMMON /RND/ ISEED
      COMMON /ICH/ IINPUT(12),IPOS(12),INUM(12)
      REAL*8 RANDOM
      INTEGER*4 ISEED /-1/
      CHARACTER*3 MTABLE
      CHARACTER*3 M$
C
   2  PRINT *, "Welcome to FOOBAR. Would you like instructions?"
      READ (*,8) M$
   8  FORMAT (A3)
      PRINT *, " "
      IF (M$.EQ."Y  ".OR.M$.EQ."YES") THEN
         CALL INSTRUCTIONS
      END IF
C
   5  ICHNUM=0
      CALL SETUP(MYMOVE)
      ICHOICE=668
C
  10  IF (MYMOVE.EQ.0) THEN
         GO TO 20
      END IF
C
  15  CALL YOURGUESS(MYMOVE)
C
  20  IF (ICHNUM.LT.0) THEN
         IF (MYMOVE.EQ.0) THEN
            PRINT *, "Would you like to play again?"
            READ (*,8) M$
            IF (M$.EQ."Y  ".OR.M$.EQ."YES") THEN
               GO TO 5
            ELSE
               CALL EXIT
            END IF
         ELSE
            GO TO 15
         END IF
      END IF
C
      CALL MYGUESS(ICHNUM,ICHOICE)
C
      IF (ICHNUM.LT.0) THEN
         GO TO 10
      END IF
C
      CALL CHGIMOVE(ICHOICE,IPOS(ICHNUM),INUM(ICHNUM))
      IF (ICHNUM.GE.0.AND.LIMIT.EQ.0) CALL MYGUESS(ICHNUM,ICHOICE)
      IF (ICHNUM.LT.0) GO TO 10
C
      CALL STRATEGY(ICHOICE)
C
      GO TO 10
C
      END
C
      SUBROUTINE COMPARE (I,J,NPOS,NUM)
C
      DIMENSION ITEST(4),JTEST(4),MASK(4)
C
      MASK(1)=7
      MASK(2)=56
      MASK(3)=448
      MASK(4)=3584
      NPOS=0
      NUM=0
C
      DO 10 L1=1,4
         ITEST(L1)=ISHFT(IAND(MASK(L1),I),(1-L1)*3)
         JTEST(L1)=ISHFT(IAND(MASK(L1),J),(1-L1)*3)
  10  CONTINUE
C
      DO 15 L4=1,4
         IF (ITEST(L4).EQ.JTEST(L4)) THEN
            NPOS=NPOS+1
         END IF
  15  CONTINUE
C
      DO 30 L2=1,4
         DO 20 L3=1,4
            IF (ITEST(L3).EQ.JTEST(L2)) THEN
               JTEST(L2)=0
               ITEST(L3)=7
               NUM=NUM+1
               GO TO 30
            END IF
  20     CONTINUE
  30  CONTINUE
      NUM=NUM-NPOS
C
      RETURN
      END
C
      SUBROUTINE SETUP(MYMOVE)
C
      COMMON IMOVE(1296),LIMIT,NTESTVAL
      COMMON /IO/ MINPUT(12),MOVENUM
      COMMON /CH/ MTABLE(12,4)
      COMMON /RND/ ISEED
      REAL*8 RANDOM
      INTEGER*4 ISEED
      CHARACTER*3 MTABLE
C
      NCTR=0
      DO 40 I=1,6
         DO 30 J=8,48,8
            DO 20 K=64,384,64
               DO 10 L=512,3072,512
                  NCTR=NCTR+1
                  IMOVE(NCTR)=L+K+J+I
  10           CONTINUE
  20        CONTINUE
  30     CONTINUE
  40  CONTINUE
C
      DO 60 I=1,12
         DO 50 J=1,4
            MTABLE(I,J)=" "
  50     CONTINUE
  60  CONTINUE
C
      NTESTVAL=4
      LIMIT=1296
      MYMOVE=IMOVE(INT(RANDOM(ISEED)*1296)+1)
      MOVENUM=0
C
      RETURN
      END
C
      SUBROUTINE STRATEGY(ICHOICE)
C
      COMMON IMOVE(1296),LIMIT,NTESTVAL
C
      IHIST=0
      ICHOICE=0
      ICHVAL=2000
C
      DO 100 I=1,LIMIT
         NLOW=0
	 NHIGH=0
         DO 50 J=1,LIMIT
	    CALL COMPARE(IMOVE(I),IMOVE(J),NPOS,NUM)
	    IF (NPOS+NUM.LE.NTESTVAL) THEN
               NLOW=NLOW+1
            ELSE NHIGH=NHIGH+1
            END IF
  50     CONTINUE
         ITEMP=NHIGH-NLOW
         ITEMP2=IABS(ITEMP)
         IF (ITEMP2.LT.ICHVAL) THEN
            ICHVAL=ITEMP2
            ICHOICE=IMOVE(I)
	    IHIST=ITEMP
         END IF
 100  CONTINUE
      XICHVAL=ICHVAL
      XLIMIT=LIMIT
      TEMP=XICHVAL/XLIMIT
      IF (TEMP.GE.0.5) THEN
         NTESTVAL=NTESTVAL+ISIGN(1,IHIST)
      END IF
C
      RETURN
      END
C
      SUBROUTINE CHGIMOVE(MOVE,MPOS,MNUM)
C
      COMMON IMOVE(1296),LIMIT,NTESTVAL
C
      NCTR=0
      DO 100 I=1,LIMIT
         CALL COMPARE(MOVE,IMOVE(I),NPOS,NUM)
         IF (NPOS.EQ.MPOS.AND.NUM.EQ.MNUM) THEN
            NCTR=NCTR+1
	    IMOVE(NCTR)=IMOVE(I)
         END IF
 100  CONTINUE
      LIMIT=NCTR
C
      RETURN
      END
C
      SUBROUTINE YOURGUESS(MYMOVE)
C
      COMMON /IO/ MINPUT(12),MOVENUM
      COMMON /CH/ MTABLE(12,4)
      CHARACTER*3 MTABLE
C
      IF (MOVENUM.EQ.0) THEN
         PRINT *, "What is your first guess?"
	 GO TO 150
      END IF
C
      PRINT *, "Here are the guesses you've made so far:"
      PRINT *, " "
      WRITE (*,101) (MINPUT(L1),(MTABLE(L1,L2),L2=1,4),L1=1,MOVENUM)
 101  FORMAT (1X,I4,3X,A3,3X,A3,3X,A3,3X,A3)
      PRINT *, " "
      PRINT *, "   What is your next guess?"
 150  READ (*,1001) M
1001  FORMAT (I4)
      MOVENUM=MOVENUM+1
C
      IF (MOVENUM.EQ.13) THEN
         MASK1=7
         MASK2=56
         MASK3=448
         MASK4=3584
         MASK1=IAND(MASK1,MYMOVE)
         MASK2=10*ISHFT(IAND(MASK2,MYMOVE),-3)
         MASK3=100*ISHFT(IAND(MASK3,MYMOVE),-6)
         MASK4=1000*ISHFT(IAND(MASK4,MYMOVE),-9)
         MYMOVE=MASK1+MASK2+MASK3+MASK4
         IF (MYMOVE.EQ.M) THEN
            NPOS=4
            GO TO 400
         ELSE
            PRINT *, "You get no more guesses. My number was ",MYMOVE
            PRINT *, "If you run out of guesses often, tell Steve."
            MYMOVE=0
            RETURN
         END IF
      END IF
C
      MINPUT(MOVENUM)=M
C
      M1=M/1000
      M=M-(M1*1000)
      M2=M/100
      M=M-(M2*100)
      M3=M/10
      M4=M-(M3*10)
C
      M=ISHFT(M1,9)+ISHFT(M2,6)+ISHFT(M3,3)+M4
      CALL COMPARE(M,MYMOVE,NPOS,NUM)
  400 IF (NPOS.EQ.4) THEN
         PRINT *, " "
         PRINT *, "You've guessed my number! Congratulations!"
         PRINT *, " "
         MYMOVE=0
         RETURN
      END IF
      DO 100 I=1,NPOS
         MTABLE(MOVENUM,I)="FOO"
 100  CONTINUE
      DO 200 J=NPOS+1,NPOS+NUM
         MTABLE(MOVENUM,J)="BAR"
 200  CONTINUE
C
      PRINT *, " "
      PRINT *, "Here are the results of that guess:"
      PRINT *, " "

      WRITE (*,101) MINPUT(MOVENUM),(MTABLE(MOVENUM,L1),L1=1,4)
C
      RETURN
      END
C
      SUBROUTINE INSTRUCTIONS
C
      PRINT *, "Hi. My name is PAM. This game is very much like the game of "
      PRINT *, """Mastermind"". We will each choose a 4-digit number using only"
      PRINT *, "the digits 1 through 6. You may use a digit more than once."
      PRINT *, "We will then take turns giving each other 4-digit numbers"
      PRINT *, "and use information supplied by the other person to try to"
      PRINT *, "guess the number hidden by the other person. When you give me"
      PRINT *, "a 4-digit number as a guess, I will print out the words ""FOO"""
      PRINT *, "and ""BAR"" a certain number of times. For each FOO I print,"
      PRINT *, "you know the number you guessed has one digit the same as my"
      PRINT *, "hidden number and it is in the same position as in my hidden"
      PRINT *, "number. You just don't know which of the four digits is the"
      PRINT *, "correct one. For each BAR I print, you know that one of the"
      PRINT *, "digits you guessed is also one of the digits in my hidden"
      PRINT *, "number, but it is not in the correct position. If you have"
      PRINT *, "a digit in the correct position, I will not print both a FOO"
      PRINT *, "and a BAR --  only a FOO."
C
      PRINT *, " "
      PAUSE
      PRINT *, " "
C
      PRINT *, "     When I make my guesses, you will give me the same infor-"
      PRINT *, "mation, but you will do it by typing in number responses."
      PRINT *, "For each of my guesses, you type in two 1-digit numbers,"
      PRINT *, "immediately after my request for them and not separated by a"
      PRINT *, "comma or any other character. The first number will be the"
      PRINT *, "number of digits in the number I guessed that are the same"
      PRINT *, "as digits in your number AND in the correct position. The"
      PRINT *, "second number you input will be the number of digits in my"
      PRINT *, "guess number that are in your hidden number, but not in the"
      PRINT *, "correct position. Remember, if a digit is in the correct position,"
      PRINT *, "you must include it in the first number that you type in,"
      PRINT *, "even if you have more than one of those digits in your hidden"
      PRINT *, "number."
      PRINT *, "     After one of us guesses the other's number, the other"
      PRINT *, "will continue playing until both numbers are guessed. Do"
      PRINT *, "not be alarmed if I take a long time to ponder your responses"
      PRINT *, "to my first guess number. I do a lot of my thinking early"
      PRINT *, "in the game. GOOD LUCK!  Let's go to it . . . ."
      PRINT *, " "
C
      RETURN
      END
C
      SUBROUTINE MYGUESS(ICHNUM,ICHOICE)
C
      COMMON /ICH/ IINPUT(12),IPOS(12),INUM(12)
      COMMON IMOVE(1296),LIMIT,NTESTVAL
C
      IF (LIMIT.EQ.0) THEN
         PRINT *, "You must have given me a wrong answer somewhere, since"
         PRINT *, "I have eliminated all possibilities with the answers given."
         ICHNUM=-1
         RETURN
      END IF
C
      ICHNUM=ICHNUM+1
      MASK1=7
      MASK2=56
      MASK3=448
      MASK4=3584
C
      M1=IAND(ICHOICE,MASK1)
      M2=10*ISHFT(IAND(ICHOICE,MASK2),-3)
      M3=100*ISHFT(IAND(ICHOICE,MASK3),-6)
      M4=1000*ISHFT(IAND(ICHOICE,MASK4),-9)
C
      IINPUT(ICHNUM)=M1+M2+M3+M4
C
      IF (LIMIT.EQ.1) THEN
         PRINT *, "I have guessed your number. It is ",IINPUT(ICHNUM)
         PRINT *, "You can continue if you haven't guessed mine yet."
         ICHNUM=-1
         RETURN
      END IF
C
   10 PRINT *, " "
      PRINT *, "My guess is ",IINPUT(ICHNUM)
      PRINT *, " "
      PRINT *, "Please tell me how many digits are the right number in the"
      PRINT *, "correct position, and how many digits are the right number"
      PRINT *, "but in the wrong position:"
      READ (*,1001) IPOS(ICHNUM),INUM(ICHNUM)
 1001 FORMAT (2I1)
C
      PRINT *, " "
      PRINT *, "           **********************"
      PRINT *, " "
C
      IF (IPOS(ICHNUM)+INUM(ICHNUM).GT.4) THEN
         PRINT *, "The total of your two inputs can not be greater"
         PRINT *, "than four. Please enter again."
         GO TO 10
      END IF
C
      IF (IPOS(ICHNUM).EQ.4) THEN
         PRINT *, "I have guessed your number!"
         PRINT *, "You can continue if you haven't guessed mine yet."
         PRINT *, " "
         ICHNUM=-1
      END IF
C
      RETURN
      END
