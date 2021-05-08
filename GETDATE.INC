*******************************************************************************
* 
*                         G E T D A T E
* 
*      SUBROUTINE TO GET THE DATE FROM 'ARCLOCK' OR THE OPERATOR
*      IF IT IS NOT ALREADY SET or AVAILABLE
* 
*      ON EXIT:
*         a.  GREGDATE---------> MM/DD/YY
*         b.  GMONTH-----------> MM
*         c.  GDAY-------------> DD
*         d.  GYEAR------------> YY
*         e.  JULEYEAR---------> YY
*         f.  JULEDAY----------> DDD
*         g.  JULEDATE---------> YYDDD
* 
* --DATE--  -----------------------MODIFICATION--------------------------------
* 02/16/82  CHANGED TO USE ARCCLOCK FILE IF AVAILABLE...JMM
* 10/13/82  NEW VERSION DUE TO DESTRUCTION OF THE ORIGINAL VERSION...JMM
*
*
*
*******************************************************************************
. 
GETDATE
         CMATCH    "Y" TO DATESET         ** HAS THE DATE BEEN SET
         GOTO      DATEXIT IF EQUAL       ** YES, RTN
         CLOCK     YEAR TO DATEWK2        ** WHAT DOES DATASHARE THINK YEAR IS?
         MOVE      DATEWK2 TO JULEYEAR    ** GET TO NUMERIC
         CLOCK     DAY  TO DATEWK3        **
         MOVE      DATEWK3 TO JULEDAY
         MOVE      "00" TO GMONTH         **
         MOVE      JULEYEAR TO GYEAR      **
         TRAP      GETCDATE IF IO
         OPEN      ARCCLOCK,"ARCCLOCK"    ** OPEN THE ARCCLOCK FILE
         READ      ARCCLOCK,DATEWK1;DATEWKD1,GYEAR:
                                    DATEWKD1,GMONTH:
                                    DATEWKD1,GDAY
         GOTO      GETCD IF OVER
         COMPARE   "0" TO GYEAR           ** HAS THE DATE BEEN SET?
         GOTO      GETCD IF EQUAL         ** NO, ASK FOR IT
         GOTO      DATEXIT
. 
GETCDATE
         NORETURN                         ** POP THE TRAP STACK
GETCD
         TRAPCLR   IO                     ** CLEAR IO TRAPPING
         COMPARE   "1" TO JULEDAY         ** CHECK FOR CLOCK COMMAND GIVEN
         GOTO      NODATE IF LESS         ** AND REQUEST ENTRY IF NOT
         COMPARE   "1" TO JULEYEAR        ** ANOTHER CK FOR CLOCK COMMAND
         GOTO      NODATE IF LESS         **
         MOVE      JULEYEAR TO DATEWK1    ** LEAP YEAR CHECKS
         DIVIDE    "4" INTO DATEWK1       **
         MULTIPLY  "4" INTO DATEWK1       **
         COMPARE   JULEYEAR TO DATEWK1    ** IS IT A LEAP YEAR?
         GOTO      MDLOOP IF NOT EQUAL    ** NO, LEAVE FEB = 28 DAYS
         MOVE      "29" TO NFEB           ** YES, MAKE FEB = 29 DAYS
MDLOOP
         ADD       "1" TO GMONTH
         LOAD      DATEWK1 FROM GMONTH OF N31,NFEB,N31:  JAN/FEB/MAR
                                          N30,N31,N30:   APR/MAY/JUN
                                          N31,N31,N30:   JUL/AUG/SEP
                                          N31,N30,N31    OCT/NOV/DEC
         SUBTRACT  DATEWK1 FROM JULEDAY
         GOTO      MDL1 IF EQUAL          ** SUBTRACT # DAYS OF THE MONTH
         GOTO      MDLOOP IF NOT LESS     ** UNTIL MONTH IS FOUND
MDL1
         ADD       DATEWK1 TO JULEDAY     ** UNBIAS FROM LAST SUBTRACT
         MOVE      JULEDAY TO GDAY        ** TO GET DAY OF THE MONTH
. 
. PUT THE DATE INTO MM/DD/YY FORMAT
. 
DATEXIT
         CMOVE     "/" TO DATEWKD1        ** USED TO FORMAT
         PACK      GREGDATE FROM GMONTH, DATEWKD1:
                                 GDAY, DATEWKD1:
                                 GYEAR
         REPLACE   " 0" IN GREGDATE
         CMOVE     "Y" TO DATESET         ** SHOW THAT DATE HAS NOW BEEN SET
         RETURN
. 
. 
NODATE
         KEYIN     *R,*B,*P01:24,*EL,*HON,*+,"Enter current date MM/DD/YY ":
                   *DE,*JR,*ZF,*P20:24,GMONTH:
                   *P23:24,GDAY:
                   *P26:24,GYEAR,*RD;
         COMPARE   "1" TO GDAY
         GOTO      NODATE IF LESS
         COMPARE   "32" TO GDAY
         GOTO      NODATE IF NOT LESS
         COMPARE   "1" TO GMONTH
         GOTO      NODATE IF LESS
         COMPARE   "13" TO GMONTH
         GOTO      NODATE IF NOT LESS
         COMPARE   "83" TO GYEAR
         GOTO      NODATE IF LESS
         COMPARE   "90" TO GYEAR
         GOTO      NODATE IF NOT LESS
         CMOVE     "Y" TO DB11FLAG
         GOTO      DATEXIT
. 
*******************************************************************************
