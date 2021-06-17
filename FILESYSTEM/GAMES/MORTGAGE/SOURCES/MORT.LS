Source file: MORT.BP
Compiled on 07-Jan-85 at 11:56:12 by AOS/VS BASIC Rev 02.41.00.00
Options: BAS/L=MORT.LS/O=MORT.OB/Optimize=3/Statistics

00001 rem MORTGAGE 9/16/83 -- JFQ
00005
00010 dim CLI_COMMAND$*60,QUEUE$*1,TFNAME$*32,TITLE$*30,REPORT_TYPE$*1
00011 dim ANSWER$*1
00012
00014 declare all real*8
00016 declare integer*4 AC0,AC1,AC2
00017
00020 declare integer*2 CHOICE,EXPONENT,FACTOR
00021 declare integer*2 FIND_LIFE,FIND_PAYMENT,FIND_PRINCIPAL,FIND_RATE
00022 declare integer*2 GROUP_COUNTER,LIFE_YEARS,LIFE_MONTHS
00024 declare integer*2 LOOP_COUNTER,MONTH_COUNTER,PSTATPKT(152),REPORT_YEARS
00026 declare integer*2 ROW,SEQNO,START_MONTH,YEAR_COUNTER
00027 declare integer*2 SINK_FUND,ANNU_FV,ORDI_PV,ANNU_PV
00028
00030 declare real*8 CUMMULATIVE_INTEREST,CUMMULATIVE_PRINCIPAL
00031 declare real*8 BAL_PAYMENT,TEMP3
00032 declare real*8 MONTHLY_RATE,MONTHS_INTEREST,MONTHS_PRINCIPAL,PAYMENT
00034 declare real*8 PRINCIPAL_REMAINING,TEMP,TEMP1,TEMP2,YEARLY_RATE
00036 declare real*8 YEARS_INTEREST,YEARS_PRINCIPAL
00037
00045 rem -- Set initial values for four types of annuities
00046 let SINK_FUND=1
00047 let ANNU_FV=2
00048 let ORDI_PV=3
00049 let ANNU_PV=4
00060
00061 rem -- Set initial parameter values
00062 let FIND_RATE=1
00064 let FIND_LIFE=2
00066 let FIND_PRINCIPAL=3
00068 let FIND_PAYMENT=4
00069 let FIND_BAL=5
00070 screen TITLE_SCREEN blank screen
00080   field tab(20,3) using "#############################" from TITLE$
00090 end screen
00100 screen INITIAL_SCREEN
00120   literal tab(21,5)"This program can compute:"
00130   literal tab(18,7)"(1) The rate of a mortgage"
00140   literal tab(18,8)"(2) The life of a mortgage"
00150   literal tab(18,9)"(3) The principal amount of a mortgage"
00160   literal tab(18,10)"(4) The monthly payment of a mortgage"
00170   literal tab(25,12)"Your choice:"
00180   field tab(38,12) required using "#" to CHOICE
00190 end screen
00200
00210 screen INITIAL_SCREEN_1
00212   literal tab(21,5)"This program can compute:"
00214   literal tab(18,7)"(1) The rate of a mortgage"
00216   literal tab(18,8)"(2) The life of a mortgage"
00218   literal tab(18,9)"(3) The principal amount of a mortgage"
00220   literal tab(18,10)"(4) The monthly payment of a mortgage"
00222   literal tab(18,11)"(5) The balloon payment of a mortgage"
00224   literal tab(25,12)"Your choice:"
00226   field tab(38,12) required using "#" to CHOICE
00228 end screen
00230
00232 screen INITIAL_SCREEN_2
00234   literal tab(21,5)"Which annuity do you want to choice?"
00236   literal tab(18,7)"(1) The Sinking Fund"
00238   literal tab(18,8)"(2) The Annuity Due/FV"
00240   literal tab(18,9)"(3) The Ordinary Annuity/PV"
00242   literal tab(18,10)"(4) The Annuity Due/PV  (Ordinary Mortgage)"
00244   literal tab(25,12)"Your select:"
00246   field tab(38,12) required using "#" to SELECT
00268 end screen
00269
00270 screen REPEAT
00272   literal tab(1,23)"Do you want to stop? (Y or N) [ ]"
00274   field tab(32,23) required using "#" to ANSWER$
00276 end screen
00279
00500 handler ERROR_1
00504   resume
00505 end handler
00520 handler ERROR_3
00522   print tab(1,22);"The parameters that you have entered are not realistic and the mortgage"
00524   print "that you are specifying could never be retired."
00526   close
00527   stop
00528 end handler
00530 handler ERROR_4
00536   print tab(1,22);"Error encounter at line: ";exline," Error code:";excode
00538   print excode$
00550   close
00560   stop
00570 end handler
00580
00590 handler CREFILE
00600   if excode=22 then
00610     let SEQNO=SEQNO+1
00620     resume
00630   else
00640     print "Unexpected error encountered creating the temporary filename"
00650     print "Error code: ";excode;"  ";excode$;" at ";exline
00660     characteristics "off","esc"
00670     stop
00680   end if
00690 end handler
00700
00710 handler ESCKEY
00720   characteristics "off","esc"
00730   stop
00740   characteristics "on","esc"
00750   continue
00760 end handler
00770
00780
00790 enable handler ERROR_4
00800 characteristics "on","esc"
00810 enable key handler ESCKEY
00812 let TITLE$=chr$(29)
00813 display TITLE_SCREEN
00815 display INITIAL_SCREEN_2
00816 accept INITIAL_SCREEN_2
00817 if SELECT>ANNU_PV or SELECT<SINK_FUND then goto 00816
00820 let TITLE$=chr$(29)&chr$(20)&"Mortgage Table Generator"&chr$(21)
00830 display TITLE_SCREEN
00832 if SELECT>ANNU_FV then
00834   display INITIAL_SCREEN_1
00835   accept INITIAL_SCREEN_1
00836   if CHOICE>FIND_BAL or CHOICE<FIND_RATE then
00837     goto 00835
00838   else
00839     goto 00910
00840   end if
00870 else
00880   display INITIAL_SCREEN
00890   accept INITIAL_SCREEN
00900   if CHOICE>FIND_PAYMENT or CHOICE<FIND_RATE then goto 00890
00902 end if
00910 if CHOICE<>FIND_RATE then
00912   let YEARLY_RATE=0
00915   input tab(1,14), prompt "What is the nominal annual percentage rate (e.g. xx.xxxxxx%)? ":YEARLY_RATE;
00940   let YEARLY_RATE=YEARLY_RATE/100
00950   if YEARLY_RATE>=1 then goto 00912
00960 end if
00980 if CHOICE<>FIND_LIFE then
00982   let LIFE_YEARS,LIFE_MONTHS=0
00985   if CHOICE=FIND_RATE then
00990     let ROW=14
00997   else
01000     let ROW=15
01005   end if
01008   enable handler ERROR_1, resume at 01010
01010   input tab(1,ROW), prompt "What is the life of the mortgage? Years: "&chr$(11):LIFE_YEARS;
01011   if LIFE_YEARS<0 then goto 01010
01014   enable handler ERROR_1, resume at 01015
01015   input tab(46,ROW), prompt "Months: "&chr$(11):LIFE_MONTHS;
01016   if LIFE_MONTHS<0 then goto 01015
01017   enable handler ERROR_4
01018 end if
01020 if CHOICE<>FIND_PRINCIPAL then
01021   let PRINCIPAL_REMAINING=0
01023   if CHOICE=FIND_RATE or CHOICE=FIND_LIFE then
01024     let ROW=15
01026   else
01028     let ROW=16
01030   end if
01040   input tab(1,ROW), prompt "What is the mortgage principal? ":PRINCIPAL_REMAINING;
01057   if PRINCIPAL_REMAINING<=0 then goto 01021
01058 end if
01060 if CHOICE<>FIND_PAYMENT then
01065   let PAYMENT=0
01066   if CHOICE=FIND_RATE or CHOICE=FIND_LIFE or CHOICE=FIND_PRINCIPAL then
01067     let ROW=16
01068   else
01069     let ROW=17
01070   end if
01071   input tab(1,ROW), prompt "What is the amount of one monthly payment? ":PAYMENT;
01080 end if
01082 if SELECT<ORDI_PV then
01084   let ROW=17
01086 else
01087   let ROW=18
01088   if CHOICE<>FIND_BAL then
01089     let BAL_PAYMENT=0
01090     input tab(1,17), prompt "What is the amount of balloon payment? ":BAL_PAYMENT;
01094   end if
01095 end if
01100 input tab(1,ROW), prompt "For how many calender years do you want the mortgage table printed? ":REPORT_YEARS;
01130 if REPORT_YEARS<>0 then
01132  
01135   enable handler ERROR_1, resume at 01140
01137   let ROW=ROW+1
01140   input tab(1,ROW), prompt "When does the loan start? month: "&chr$(11):START_MONTH;
01145   if START_MONTH<1 or START_MONTH>12 then goto 01140
01150  
01160   input tab(38,ROW), prompt "Year: "&chr$(11):START_YEAR;
01162   if START_YEAR<0 then goto 01160
01164   if START_YEAR>99 then let START_YEAR=mod(START_YEAR,100)
01166  
01170   enable handler ERROR_4
01173   let ROW=ROW+1
01180   input tab(1,ROW), prompt "Do you want an annual mortgage table (A) or a monthly table (M)? ":REPORT_TYPE$
01185   let REPORT_TYPE$=uprc$(REPORT_TYPE$)
01190   if REPORT_TYPE$<>"A" and REPORT_TYPE$<>"M" then goto 01180
01250   gosub 02830                               ! Prep the temp filename
01253   let ROW=ROW+1
01260   input tab(1,ROW), prompt "Do you want it printed in the scanner (S) building or the bank (B) building? ":QUEUE$;
01265  
01270   let QUEUE$=uprc$(QUEUE$)
01290   if QUEUE$<>"S" and QUEUE$<>"B" then goto 01260
01300   open #1:TFNAME$ sequential, display, output, stream
01310   margin #1:0
01320 end if
01330
01335 rem ENABLE HANDLER ERROR_3
01340 if CHOICE<>FIND_LIFE then let TEMP2=12*LIFE_YEARS+LIFE_MONTHS
01360 if CHOICE<>FIND_RATE then let MONTHLY_RATE=YEARLY_RATE/12
01362 select SELECT
01363 case 1
01364   gosub 03000
01366 case 2
01367   gosub 03300
01369 case 3
01370   gosub 03600
01372 case 4
01373   gosub 03900
01374 end select
01583 enable handler ERROR_4
01584
01585 for ROW=1 to 12                      ! Erase the top 12 rows of the screen
01590   print tab(1,ROW);chr$(11)                 !Erase to the end of the line
01595 next ROW
01597
01598 margin #0:0
01599
01640 print tab(29,3);chr$(29)&chr$(20)&"*** mortgage terms ***"&chr$(21)
01660 print tab(1,5);"Life of the mortgage:....................................";
01670 output #0 using "###":LIFE_YEARS;
01671 output #0 using "###########":" years and ";
01672 output #0 using "##":LIFE_MONTHS;
01673 output #0 using "#######":" months";
01679 print tab(1,6);"Nominal Annual Rate:.............................................";
01680 output #0 using "  ######.######":YEARLY_RATE*100;
01682 output #0 using "#":"%";
01689 print tab(1,7);"Amount Borrowed:.................................................";
01690 output #0 using "$##,###,###.##":PRINCIPAL_REMAINING;
01700 print tab(1,8);"Payment:.........................";
01705 output #0 using "$##,###,###.##/month  $##,###,###.##/year":PAYMENT,PAYMENT*12;
01706 if SELECT>ANNU_FV then
01707   print tab(1,9);"Balloon Payment:.............................................";
01708   output #0 using "$##,###,###.##":BAL_PAYMENT;
01709 end if
01710 if CHOICE=FIND_LIFE then
01730   print tab(1,10);"Note: The mortgage life has been rounded upward to the nearest month"
01735 end if
01740
01750 if REPORT_YEARS<>0 then              ! Stop here if there is no print file
01759  
01760   print tab(1,12);"Your mortgage table is being printed on the line printer in the";
01762   if QUEUE$="S" then
01763     print " computer room.";
01765   else
01767     print " bank building.";
01768   end if
01810   output #1:chr$(10)&"                             *** MORTGAGE TERMS ***"&chr$(10)
01820   output #1:"Life of the mortgage:....................................";
01822   output #1 using "###":LIFE_YEARS;
01824   output #1 using "###########":" years and ";
01826   output #1 using "##":LIFE_MONTHS;
01828   output #1 using "#######":" months"
01830   output #1:chr$(10)&"Nominal Annual Rate:.............................................";
01835   output #1 using "  ######.######":YEARLY_RATE*100;
01836   output #1 using "#":"%"
01840   output #1:"Amount Borrowed:.................................................";
01845   output #1 using "$##,###,###.##":PRINCIPAL_REMAINING
01850   output #1:"Payment:...............................";
01855   output #1 using "$##,###,###.##/month  $##,###,###.##/year":PAYMENT,PAYMENT*12
01856   if SELECT>ANNU_FV then
01857     output #1:"Balloon payment........................................................";
01858     output #1 using "$##,###,###.##":BAL_PAYMENT
01859   end if
01860   if CHOICE=FIND_LIFE then
01870     output #1:chr$(10)&"Note: The mortgage life has been rounded upward to the nearest month"
01880   end if
01885  
01890   output #1:rpt$(chr$(10),2)&rpt$(" ",51)&"*** MORTGAGE TABLE ***"&chr$(10)
01920   let CUMMULATIVE_INTEREST,CUMMULATIVE_PRINCIPAL,YEARS_INTEREST,YEARS_PRINCIPAL=0
01945   let YEAR_COUNTER=START_YEAR
01950   let MONTH_COUNTER=START_MONTH
01985  
02000   if REPORT_TYPE$="M" then
02010     let GROUP_COUNTER=1
02020     gosub 02630
02040   else
02050     output #1:"Year       Interest Paid        Cummulative Interest       Principal repaid      Cummulative Principal    Principa
l Outstanding"
02060     output #1:"                                        paid                                            repaid"
02085   end if
02087  
02088   rem ----Loop thru each month of the mortgage or the end of the report----
02089  
02090   for LOOP_COUNTER=MONTH_COUNTER to 12*REPORT_YEARS
02095     if PRINCIPAL_REMAINING<0 then goto 02600 ! Check for the end of mortgage
02100     let MONTHS_INTEREST=PRINCIPAL_REMAINING*MONTHLY_RATE
02110     if PAYMENT>=PRINCIPAL_REMAINING+MONTHS_INTEREST then
02120       let MONTHS_PRINCIPAL=PRINCIPAL_REMAINING
02130     else
02140       let MONTHS_PRINCIPAL=PAYMENT-MONTHS_INTEREST
02145     end if
02160     let PRINCIPAL_REMAINING=PRINCIPAL_REMAINING-MONTHS_PRINCIPAL
02170     let CUMMULATIVE_INTEREST=CUMMULATIVE_INTEREST+MONTHS_INTEREST
02175     let YEARS_INTEREST=YEARS_INTEREST+MONTHS_INTEREST
02180     let CUMMULATIVE_PRINCIPAL=CUMMULATIVE_PRINCIPAL+MONTHS_PRINCIPAL
02185     let YEARS_PRINCIPAL=YEARS_PRINCIPAL+MONTHS_PRINCIPAL
02195    
02196     rem ---If a monthly report is being generated, print a month item line---
02200     if REPORT_TYPE$<>"A" then
02210       if MONTH_COUNTER=1 then gosub 02630 ! If this is Jan., output a year heading
02250       output #1 using 02270:MONTH_COUNTER,MONTHS_INTEREST,CUMMULATIVE_INTEREST,MONTHS_PRINCIPAL,CUMMULATIVE_PRINCIPAL,PRINCIPAL_R
EMAINING
02270       image : <%    -###,###,###.##           -###,###,###.##          -###,###,###.##          -###,###,###.##          -###,###
,###.##
02280      
02282       rem -End of month, or end of mortgage.  Give the year's summary figures-
02284      
02290       if MONTH_COUNTER=12 or PRINCIPAL_REMAINING<=0 then
02305         output #1
02310         output #1 using 02320:YEARS_INTEREST,YEARS_PRINCIPAL
02320         image :       -###,###,###.##                                    -###,###,###.##
02325         if PRINCIPAL_REMAINING<=0 then goto 02600                   ! Done
02330       end if
02363      
02364       rem --- An annual report is being generated.  Print if we are at year end.
02365     else
02370       if MONTH_COUNTER=12 and PRINCIPAL_REMAINING>=0 then
02490         output #1 using 02270:YEAR_COUNTER,YEARS_INTEREST,CUMMULATIVE_INTEREST,YEARS_PRINCIPAL,CUMMULATIVE_PRINCIPAL,PRINCIPAL_RE
MAINING
02500       end if
02510     end if
02512    
02540    
02550     if MONTH_COUNTER=12 then      ! If year end, reset the yearly counters
02553       let YEAR_COUNTER=YEAR_COUNTER+1       ! Increment the year counter
02557       if YEAR_COUNTER>=100 then let YEAR_COUNTER=YEAR_COUNTER-100 ! Wrap it around if we past a century
02560       let YEARS_INTEREST=0
02570       let YEARS_PRINCIPAL=0
02580       let MONTH_COUNTER=0
02585     end if
02587     let MONTH_COUNTER=MONTH_COUNTER+1
02590   next LOOP_COUNTER
02595  
02596 end if
02597
02600 close
02602 print tab(1,22);
02603 if REPORT_YEARS>0 then
02604   if QUEUE$="S" then
02605     let CLI_COMMAND$="QPRINT/DEL "&TFNAME$
02606   else
02607     let CLI_COMMAND$="QPRINT/QUE=LPT1/DEL "&TFNAME$
02608   end if
02609   cli CLI_COMMAND$
02610   print tab(1,20);
02611 end if
02612 let ANSWER$="N"
02613 display REPEAT
02614 accept REPEAT
02620 if ANSWER$="N" or ANSWER$="n" then
02621   goto 00790
02622 else
02623   if ANSWER$<>"Y" and ANSWER$<>"y" then
02624     goto 02614
02625   end if
02626 end if
02627 stop
02628
02630 rem   ***  SUBROUTINE TO PRINT YEAR HEADINGS  ***
02680 if GROUP_COUNTER>=3 then
02690   output #1:chr$(12)
02710   let GROUP_COUNTER=1
02715 else
02718   output #1:chr$(10)                        ! Space down 2 lines
02720   let GROUP_COUNTER=GROUP_COUNTER+1
02725 end if
02730 output #1 using "                                                      Calendar Year '%%":YEAR_COUNTER
02740 output #1:"Month      Interest Paid        Cummulative Interest       Principal repaid      Cummulative Principal    Principal Ou
tstanding"
02750 output #1:"                                        paid                                            repaid"
02760 output #1                                   ! Skip a line
02790 return
02800
02810 rem  Subroutine to prepare and create the temporary output filename
02820
02830 let AC0=-1
02840 let AC1=0
02850 let AC2=addr(PSTATPKT)
02860 mat PSTATPKT=zer
02870 if sys("PSTAT",AC0,AC1,AC2)<>0 then
02880   print "Unexpected error encountered in temporary filname formation"
02890   print "from ?Pstat system call.  System returned code: ";AC0
02900   characteristics "off","esc"
02910   stop
02920 end if
02930 let SEQNO=0
02940 enable handler CREFILE, resume at 02950
02950 let TFNAME$="?."&str$(PSTATPKT(30))&"."&str$(SEQNO)&".mortgage.tmp"
02960 create TFNAME$
02970 enable handler ERROR_4
02980 return
03000 select CHOICE                               !find the sinking fund
03010 case 2                                      !find the mortgage life
03020   let TEMP2=log(abs(PRINCIPAL_REMAINING*MONTHLY_RATE/PAYMENT+1))/log(1+MONTHLY_RATE)
03030   let TEMP2=int(TEMP2)+1
03040   let LIFE_YEARS=int(TEMP2/12)
03050   let LIFE_MONTHS=TEMP2-12*LIFE_YEARS
03060 case 3                                      !find the mortgage principal
03070   let PRINCIPAL_REMAINING=PAYMENT*((1+MONTHLY_RATE)^TEMP2-1)/MONTHLY_RATE
03080 case 4                                      !find the monthly payment
03090   let PAYMENT=PRINCIPAL_REMAINING*MONTHLY_RATE/((1+MONTHLY_RATE)^TEMP2-1)
03100 case 1                                      !find the mortgage rate
03110   let MONTHLY_RATE=0
03120   for EXPONENT=1 to 5
03130     for FACTOR=1 to 10
03140       let TEMP=FACTOR*(1/10^EXPONENT)+MONTHLY_RATE
03150       let TEMP1=PAYMENT*((1+TEMP)^TEMP2-1)/TEMP
03160       if TEMP1>PRINCIPAL_REMAINING then goto 03180
03170     next FACTOR
03180     let MONTHLY_RATE=TEMP-1/10^EXPONENT
03190   next EXPONENT
03200   let YEARLY_RATE=MONTHLY_RATE*12
03210 end select
03290 return
03300 select CHOICE                               !find the annuity due/fv
03310 case 2                                      !find the mortgage life
03320   let TEMP2=log((PRINCIPAL_REMAINING*MONTHLY_RATE+PAYMENT*(1+MONTHLY_RATE))/PAYMENT)/log(1+MONTHLY_RATE)-1
03330   let TEMP2=int(TEMP2)+1
03340   let LIFE_YEARS=int(TEMP2/12)
03350   let LIFE_MONTHS=TEMP2-12*LIFE_YEARS
03360 case 3                                      !find the mortgage principal
03370   let PRINCIPAL_REMAINING=PAYMENT*(1+MONTHLY_RATE)*((1+MONTHLY_RATE)^TEMP2-1)/MONTHLY_RATE
03380 case 4                                      !find the monthly payment
03390   let PAYMENT=PRINCIPAL_REMAINING*MONTHLY_RATE/((1+MONTHLY_RATE)^(TEMP2+1)-(1+MONTHLY_RATE))
03400 case 1                                      !find the mortgage rate
03410   let MONTHLY_RATE=0
03420   for EXPONENT=1 to 5
03430     for FACTOR=1 to 10
03440       let TEMP=FACTOR*(1/10^EXPONENT)+MONTHLY_RATE
03450       let TEMP1=PAYMENT*(1+TEMP)*((1+TEMP)^TEMP2-1)/TEMP
03460       if TEMP1>PRINCIPAL_REMAINING then goto 03480
03470     next FACTOR
03480     let MONTHLY_RATE=TEMP-1/10^EXPONENT
03490   next EXPONENT
03500   let YEARLY_RATE=MONTHLY_RATE*12
03510 end select
03590 return
03600 select CHOICE                               !find the ordinary annuity/pv
03605  
03610 case 2                                      !find the mortgage life
03620   let TEMP2=-log(abs((PRINCIPAL_REMAINING*MONTHLY_RATE-PAYMENT)/(BAL_PAYMENT*MONTHLY_RATE-PAYMENT)))/log(1+MONTHLY_RATE)
03630   let TEMP2=int(TEMP2)+1
03640   let LIFE_YEARS=int(TEMP2/12)
03650   let LIFE_MONTHS=TEMP2-12*LIFE_YEARS
03660 case 3                                      !find the mortgage principal
03670   let PRINCIPAL_REMAINING=PAYMENT*(1-(1+MONTHLY_RATE)^(-TEMP2))/MONTHLY_RATE+BAL_PAYMENT*(1+MONTHLY_RATE)^(-TEMP2)
03680 case 4                                      !find the monthly payment
03690   let PAYMENT=(PRINCIPAL_REMAINING-BAL_PAYMENT*(1+MONTHLY_RATE)^(-TEMP2))/((1-(1+MONTHLY_RATE)^(-TEMP2))/MONTHLY_RATE)
03700 case 5                                      !find the balloon payment
03710   let TEMP3=PAYMENT*(1-(1+MONTHLY_RATE)^(-TEMP2))/MONTHLY_RATE
03712   let TEMP1=PRINCIPAL_REMAINING-TEMP3
03714   let BAL_PAYMENT=TEMP1/(1+MONTHLY_RATE)^(-TEMP2)
03720 case 1
03730   let MONTHLY_RATE=0
03740   for EXPONENT=1 to 5
03750     for FACTOR=1 to 10
03760       let TEMP=FACTOR*(1/10^EXPONENT)+MONTHLY_RATE
03770       let TEMP1=PAYMENT*(1-1/(1+TEMP)^TEMP2)/TEMP+BAL_PAYMENT*(1+TEMP)^(-TEMP2)
03780       if TEMP1<PRINCIPAL_REMAINING then goto 03800
03790     next FACTOR
03800     let MONTHLY_RATE=TEMP-1/10^EXPONENT
03810   next EXPONENT
03820   let YEARLY_RATE=MONTHLY_RATE*12
03830 end select
03890 return
03900 select CHOICE                               !find the annuity due/pv
03910 case 2                                      !find the mortgage life
03920   let TEMP2=log(abs((PAYMENT*(1+MONTHLY_RATE)+BAL_PAYMENT*MONTHLY_RATE)/(PAYMENT-1-MONTHLY_RATE-PRINCIPAL_REMAINING*MONTHLY_RATE)
))
03922   let TEMP3=log(1+MONTHLY_RATE)
03924   let TEMP2=TEMP2/TEMP3
03930   let TEMP2=int(TEMP2)+1
03940   let LIFE_YEARS=int(TEMP2/12)
03950   let LIFE_MONTHS=TEMP2-12*LIFE_YEARS
03960 case 3                                      !find the mortgage principal
03970   let PRINCIPAL_REMAINING=PAYMENT*(1+MONTHLY_RATE)*(1-(1+MONTHLY_RATE)^(-TEMP2))/MONTHLY_RATE
03972   let TEMP3=BAL_PAYMENT*(1+MONTHLY_RATE)^(-TEMP2)
03974   let PRINCIPAL_REMAINING=PRINCIPAL_REMAINING+TEMP3
03980 case 4                                      !find the monthly payment
03990   let PAYMENT=(PRINCIPAL_REMAINING*MONTHLY_RATE-BAL_PAYMENT*MONTHLY_RATE*(1+MONTHLY_RATE)^(-TEMP2))/(1+MONTHLY_RATE)
03992   let TEMP3=1-(1+MONTHLY_RATE)^(-TEMP2)
03994   let PAYMENT=PAYMENT/TEMP3
04000 case 5                                      !find the balloon payment
04010   let TEMP1=PAYMENT*(1+MONTHLY_RATE)*(1-(1+MONTHLY_RATE)^(-TEMP2))/MONTHLY_RATE
04011   let TEMP3=PRINCIPAL_REMAINING-TEMP1
04012   let BAL_PAYMENT=TEMP3/(1+MONTHLY_RATE)^(-TEMP2)
04020 case 1                                      !find the mortgage rate
04030   let MONTHLY_RATE=0
04040   for EXPONENT=1 to 5
04050     for FACTOR=1 to 10
04060       let TEMP=FACTOR*(1/10^EXPONENT)+MONTHLY_RATE
04070       let TEMP1=PAYMENT*(1+TEMP)*(1-(1+TEMP)^(-TEMP2))/TEMP+BAL_PAYMENT*(1+TEMP)^(-TEMP2)
04080       if TEMP1<PRINCIPAL_REMAINING then goto 04100
04090     next FACTOR
04100     let MONTHLY_RATE=TEMP-1/10^EXPONENT
04110   next EXPONENT
04120   let YEARLY_RATE=MONTHLY_RATE*12
04130 end select
04190 return
05000 end
Source file: MORT.BP
Compiled on 07-Jan-85 at 11:56:12 by AOS/VS BASIC Rev 02.41.00.00
Options: BAS/L=MORT.LS/O=MORT.OB/Optimize=3/Statistics

                        - Allocation Map -

Main program .MAIN
Name                            Strg/entry   Size      Loc       Type           
----                            ----------   ----      ---       ----           
AC0                             Static       2w        2495      Integer*4      
AC1                             Static       2w        2493      Integer*4      
AC2                             Static       2w        2491      Integer*4      
ANNU_FV                         Static       1w        2319      Integer*2      
ANNU_PV                         Static       1w        2317      Integer*2      
ANSWER$                         Static       2w        2497      String         
BAL_PAYMENT                     Static       4w        2305      Real*8         
CHOICE                          Static       1w        2490      Integer*2      
CLI_COMMAND$                    Static       31w       2536      String         
CREFILE                         Handler                                         
CUMMULATIVE_INTEREST            Static       4w        2313      Real*8         
CUMMULATIVE_PRINCIPAL           Static       4w        2309      Real*8         
ERROR_1                         Handler                                         
ERROR_3                         Handler                                         
ERROR_4                         Handler                                         
ESCKEY                          Handler                                         
EXPONENT                        Static       1w        2489      Integer*2      
FACTOR                          Static       1w        2488      Integer*2      
FIND_BAL                        Static       4w        2253      Real*8         
FIND_LIFE                       Static       1w        2487      Integer*2      
FIND_PAYMENT                    Static       1w        2486      Integer*2      
FIND_PRINCIPAL                  Static       1w        2485      Integer*2      
FIND_RATE                       Static       1w        2484      Integer*2      
GROUP_COUNTER                   Static       1w        2483      Integer*2      
LIFE_MONTHS                     Static       1w        2481      Integer*2      
LIFE_YEARS                      Static       1w        2482      Integer*2      
LOOP_COUNTER                    Static       1w        2480      Integer*2      
MONTHLY_RATE                    Static       4w        2297      Real*8         
MONTHS_INTEREST                 Static       4w        2293      Real*8         
MONTHS_PRINCIPAL                Static       4w        2289      Real*8         
MONTH_COUNTER                   Static       1w        2479      Integer*2      
ORDI_PV                         Static       1w        2318      Integer*2      
PAYMENT                         Static       4w        2285      Real*8         
PRINCIPAL_REMAINING             Static       4w        2281      Real*8         
PSTATPKT                        Static       153w      2326      Integer*2      
QUEUE$                          Static       2w        2534      String         
REPORT_TYPE$                    Static       2w        2499      String         
REPORT_YEARS                    Static       1w        2325      Integer*2      
ROW                             Static       1w        2324      Integer*2      
SELECT                          Static       4w        195       Real*8         
SEQNO                           Static       1w        2323      Integer*2      
SINK_FUND                       Static       1w        2320      Integer*2      
START_MONTH                     Static       1w        2322      Integer*2      
START_YEAR                      Static       4w        0         Real*8         
TEMP                            Static       4w        2277      Real*8         
TEMP1                           Static       4w        2273      Real*8         
TEMP2                           Static       4w        2269      Real*8         
TEMP3                           Static       4w        2301      Real*8         
TFNAME$                         Static       17w       2517      String         
TITLE$                          Static       16w       2501      String         
YEARLY_RATE                     Static       4w        2265      Real*8         
YEARS_INTEREST                  Static       4w        2261      Real*8         
YEARS_PRINCIPAL                 Static       4w        2257      Real*8         
YEAR_COUNTER                    Static       1w        2321      Integer*2      
Source file: MORT.BP
Compiled on 7-Jan-85 at 11:58:20 by AOS/VS BASIC Rev 02.41.00.00

			 Compilation Statistics


		Lines Compiled		           505
		Symtab Pages (Max 555)	            15
		Symtab Nodes (Max 32249)           770
		Unshared Data		          2568
		Shared Data		           658
		Shared Code		          8812

		Physical Page Faults	           294
		Logical Page Faults	            51
		Elapsed Time	               2:10.00  
		CPU Time	               0:20.09  
		Lines/CPU Minute	          1508
		Lines/Elapsed Minute	           233
