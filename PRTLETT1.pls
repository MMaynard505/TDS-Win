. ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
. บ  PRTLETT1    Special program to read CUSTMAST file & create CUSTLET1.PRT   บ
. บ  V.M=3.0     This program uses CUSTLET1.TXT as the base letter.            บ
. บ                                                                            บ
. ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
.
. ษออออออออออัอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
. บ  Date    ณ                 Modification                                    บ
. วฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
. บ  1/19/98 ณ Initial Version 1.0                                             บ
. บ  1/21/99 ณ Ver 3.0 created from V1.0 for PLBWin...MM                       บ
. บ          ณ Some tweaking for SPLfile...MM                                  บ
. บ          ณ                                                                 บ
. บ          ณ                                                                 บ
. บ          ณ                                                                 บ
. ศออออออออออฯอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ

         INCLUDE   COMMON.INC
Version  INIT      "PRTLETT1 V.M=3.0"

CustMast IFILE     VARIABLE=1024,WEOF

         INCLUDE   TJONCMDS.INC

CustNbr  DIM       3             ;; Unique Customer Number
CustNbrS DIM       8             ;; Save area for Current Customer filename

Spaces   DIM       80
Line     DIM       80
NextProg DIM       80

SF       FILE
SplFile  PFILE

LetFile  FILE

SFName   DIM       18           /*      "CUSTLETx.PRT"
SFNameC  INIT      "CUSTLET"    ;; Print file name (first 7)
PrtExt   INIT      ".PRT"
LetExt   INIT      ".TXT"
CLNameC  INIT      "CUSTLET"
CLName   DIM       8

DateLine INIT      "Date: MM/DD/CCYY"
FromLine INIT      "From: "
CustLine INIT      "Cust: "
CNLine   INIT      "Customer Name: "
ConLine  INIT      "Contact: "
AdrLine  INIT      "Mailing Address: "
CSLine   INIT      "City, State: "
ZipLine  INIT      "Zip Code: "
PhonLine INIT      "Phone: "
FaxLine  INIT      "Fax: "
EMailLn  INIT      "E-Mail: "
Com1Line INIT      "Com1Line: "


Drive    INIT      ":"
Bureaus  FORM      1

Key      DIM       3
KeySave  DIM       3
PN1      DIM       3
PN2      DIM       3
PN3      DIM       4
Seq      FORM      "-2"

FP       FORM      3
LP       FORM      3

RecsOut  FORM      3
IntSW    INIT      "N"
Reply    DIM       1
MaxLines FORM      "60"
LineCnt  FORM      2
CDate    DIM       8
CDate2   DIM       10                  ;; Date with Century
PageCnt  FORM      " 0"
BOF      FORM      "0"
LetNbr   DIM       1
MM       DIM       2
DD       DIM       2
YY       DIM       2
YearWork FORM      4
DateMin  DIM       10                 ;; CCYYMMDD Minimum date for 'current'
DateMinW FORM      10                 ;; Numeric work area for testing date
CurrDatW FORM      10
CurrDate DIM       10
Century  DIM       2                  ;; Century field=19 if YY > 75

CustDtY4 DIM       4
CustDtM2 DIM       2
CustDtD2 DIM       2
.                                     ;; Century field=20 if YY < 75
NullLine INIT      "______________________"
.
ConvertC
         RESET     Spaces TO 80
         LENSET    Spaces
         RESET     Spaces
         MOVE      MaxLines TO LineCnt
         CLOCK     DATE TO CDate
         REPLACE   "-/" IN CDate
         UNPACK    CDate TO MM, Reply, DD, Reply, YY
         MOVE      "19" TO Century
         MOVE      YY TO YearWork
         COMPARE   "75" TO YearWork
         GOTO      DateOK IF NOT LESS
         MOVE      "20" TO Century
DateOK
         PACK      CDate2 FROM MM, "/", DD, "/", Century, YY
         PACK      CurrDate FROM Century, YY, MM, DD

         MOVE      CurrDate TO CurrDatW

         TRAP      INT IF INTERRUPT
         TRAP      INT IF ESCAPE

. ======================================= Command line options ============== 

         GOTO      ParseCmd
Argument DIM       30(5)              ;; 5 parameters - 30 bytes each
ArgCount FORM      2
Range    INIT      "azAZ09..$$"
PMax     FORM      "5"                ;; Maximum number of parameters
.
ParseCmd
         CMATCH    " " TO S$CMDLIN
         GOTO      NoParms IF EOS                /* This tests for null input
         BUMP      S$CMDLIN
         GOTO      NoParms IF EOS                /* This tests for null input
Parse1
         COMPARE   ArgCount TO PMax
         GOTO      PTooMany IF EQUAL
         ADD       "1" TO ArgCount
         PARSE     S$CmdLin,Argument(ArgCount),Range,UPPERCASE
         GOTO      Parse2 IF NOT EQUAL
         GOTO      Parse3 IF LESS
         GOTO      Parse1
Parse2
         SUB       "1",ArgCount
         BUMP      S$CmdLin
         GOTO      Parse1 IF NOT EOS
Parse3
         MOVE      Argument(1) TO LetNbr      ;; Input parameter 1
         TYPE      LetNbr
         GOTO      NoParms IF NOT EQUAL
         GOTO      GotParms
PTooMany
         DISPLAY   *R,*C,*B,"Error : Only ",PMax," parameters allowed";
         GOTO      GotParms
NoParms
         MOVE      "1" TO LetNbr
GotParms
. ======================================= Command line options ============== 

         PACK      CLName FROM CLNameC, LetNbr, LetExt
         TRAP      LetMiss IF IO
         OPEN      LetFile,CLName        ;; Open Customer Letter File
         TRAPCLR   IO

         TRAP      ARMiss IF IO
         OPEN      CustMast,"CUSTMAST"
         TRAPCLR   IO

         CALL      CKSF                   /* Create the Letter spool file
.
         MOVE      Spaces TO Key
         READ      CustMast,Key;;
         MOVE      "0" TO RecsOut
         DISPLAY   *COLOR 23:
                   *R,*P1:24,"Records Printed------------->":
                   *P30:24,RecsOut;
ReadLoop
         READKS    CustMast;CustRcd
         GOTO      EOJ IF OVER

.
. Search for current records using the date of last invoice
. A record is current if it is less than 1 year old
.
         REPLACE   " 0" IN CustLInD
         UNPACK    CustLInD TO CustDtM2, CustDtD2, CustDtY4
         MOVE      CustDtY4 TO YearWork
         ADD       "1" TO YearWork
         MOVE      YearWork TO CustDtY4
         REPLACE   " 0" IN CustDtY4
         PACK      DateMin FROM CustDtY4, CustDtM2, CustDtD2
         MOVE      DateMin TO DateMinW           ;; Get Last Inv Date in Wk area

. =========================== Debug ====================
. No-Op  KEYIN     *R,*C,"Date Compare Routine":
. No-Op            *R,*C,"DateMinW=",*DV,DateMinW,",  CurrDatW=",*DV,CurrDatW:
. No-Op            Reply;
. =========================== Debug ====================

         COMPARE   DateMinW TO CurrDatW          ;; Is this a current record?
         GOTO      ReadLoop IF NOT LESS          ;; Not current, skip

         ADD       "1" TO RecsOut
         DISPLAY   *P30:24,RecsOut;
         MOVE      CustNum TO KeySave

. Now, read the letter and plug variables where needed.

         REPOSIT   LetFile,BOF
ReadLet
         CALL      FullLine
         MOVE      Spaces TO Line
         READ      LetFile,SEQ;Line
         GOTO      LetEOF IF OVER
Scan1
         SCAN      DateLine IN Line
         GOTO      Scan2 IF NOT EQUAL
         BUMP      Line BY -1
         APPEND    CDate2 TO Line
         APPEND    Spaces TO Line
         GOTO      PrtLine

Scan2
         SCAN      FromLine IN Line
         GOTO      Scan3 IF NOT EQUAL
         MOVE      "000" TO Key
         READ      CustMast,Key;CustRcd
         MOVE      CustName TO Line
         RESET     Line
         PRINT     *1,Line
         MOVE      CustAddr TO Line
         PRINT     *1,Line
         CLEAR     Line
         APPEND    CustCity TO Line
         APPEND    "  " TO Line
         APPEND    CustSte TO Line
         APPEND    "  " TO Line
         APPEND    CustZip TO Line
         CALL      FullLine
         PRINT     *1,Line
         ADD       "3" TO LineCnt
         GOTO      ReadLet

Scan3
         SCAN      CustLine IN Line
         GOTO      Scan4 IF NOT EQUAL
         MOVE      KeySave TO Key
         READ      CustMast,Key;CustRcd
         BUMP      Line BY 5                ;; Past Cust: constant
         APPEND    CustNum TO Line
         GOTO      PrtLine

Scan4
         SCAN      CNLine IN Line
         GOTO      Scan5 IF NOT EQUAL
         BUMP      Line BY 16               ;; Past constant
         APPEND    CustName TO Line
         GOTO      PrtLine

Scan5
         SCAN      ConLine IN Line
         GOTO      Scan6 IF NOT EQUAL
         BUMP      Line BY 16               ;; Past constant
         APPEND    CustAttn TO Line
         GOTO      PrtLine

Scan6
         SCAN      AdrLine IN Line
         GOTO      Scan7 IF NOT EQUAL
         BUMP      Line BY 16               ;; Past constant
         APPEND    CustAddr TO Line
         GOTO      PrtLine

Scan7
         SCAN      CSLine IN Line
         GOTO      Scan8 IF NOT EQUAL
         BUMP      Line BY 16               ;; Past constant
         APPEND    CustCity TO Line
         APPEND    " " TO Line
         APPEND    CustSte TO Line
         GOTO      PrtLine

Scan8
         SCAN      ZipLine IN Line
         GOTO      Scan9 IF NOT EQUAL
         BUMP      Line BY 16               ;; Past constant
         APPEND    CustZip  TO Line
         GOTO      PrtLine

Scan9
         SCAN      PhonLine IN Line
         GOTO      Scan10 IF NOT EQUAL
         BUMP      Line BY 16               ;; Past constant
         APPEND    CustPhon TO Line
         GOTO      PrtLine

Scan10
         SCAN      FaxLine IN Line
         GOTO      Scan11 IF NOT EQUAL
         BUMP      Line BY 16               ;; Past constant
         APPEND    CustFax TO Line
         GOTO      PrtLine

Scan11
         SCAN      EMailLn IN Line
         GOTO      Scan12 IF NOT EQUAL
         BUMP      Line BY 16               ;; Past constant
         APPEND    NullLine TO Line
         GOTO      PrtLine

Scan12
         SCAN      Com1Line IN Line
         GOTO      Scan13 IF NOT EQUAL

. Check to see if this customer is only reporting to a single bureau. If
. they are, then print a tickler message.
. If they report to 2 or more then we are maximizing our revenue.

         MOVE      "0" TO Bureaus
CkCBI
         CMATCH    "Y" TO CustCBIf
         GOTO      CkExp IF NOT EQUAL
         ADD       "1" TO Bureaus
CkExp
         CMATCH    "Y" TO CustExpf
         GOTO      CkTU IF NOT EQUAL
         ADD       "1" TO Bureaus
CkTU
         CMATCH    "Y" TO CustTUf
         GOTO      CkExit IF NOT EQUAL
         ADD       "1" TO Bureaus
CkExit

. =========================== Debug ====================
. No-Op  KEYIN     *R,*C,"Check for Number of Bureaus routine:":
. No-Op            *R,*C,"CBI=",*DV,CustCBIf,",  TRW=",*DV,CustExpf:
. No-Op            ", TU=",*DV,CustTUf,", Count=",*DV,Bureaus:
. No-Op            Reply;
. =========================== Debug ====================

         COMPARE   "1" TO Bureaus
         GOTO      Tickle IF EQUAL
         COMPARE   "0" TO Bureaus
         GOTO      WhyHere IF EQUAL
         GOTO      ReadLet

Tickle
         PRINT     *1,"   ___  Check here if you would like information ":
                   "on our special pricing",*N:
                      "        for adding additional credit bureaus to ":
                   "receive your credit",*N:
                      "        reporting information.";
         CALL      FullLine
         MOVE      Spaces TO Line
         GOTO      PrtLine

WhyHere
         KEYIN     *B,*R,*C,"PrtLett1:   Customer has no bureaus in CUSTMAST":
                   ",  Cust=",*DV,CustNum,"  ID=",*DV,CustFN," ",*-,Reply;
         GOTO      ReadLet

Scan13
         GOTO      PrtLine

FullLine
         RESET     Line TO 80
         LENSET    Line
         RESET     Line
         RETURN

PrtLine
         CALL      FullLine
         PRINT     *1,Line
         ADD       "1" TO LineCnt
         GOTO      ReadLet

LetEOF
         PRINT     *F;
         GOTO      ReadLoop

INT
         NORETURN
         CMOVE     "Y" TO INTSW
         GOTO      EOJ

ARMiss
         KEYIN     *B,*P1:24,*EL,"TDS Customer Master File is missing. ":
                   "(CUSTMAST) ":
                   REPLY;
         STOP

LetMiss
         KEYIN     *B,*P1:24,*EL,"Customer Letter File is missing. ":
                   "(",*DV,CLName,") ":
                   REPLY;
         STOP

CKSF
         TRAP      NEWSF IF IO
         PACK      SFName FROM SFNameC, LetNbr, PrtExt
         OPEN      SF,SFName
         CLOSE     SF
         TRAPCLR   IO
         KEYIN     *B,*P1:24,*EL:
                   "The print file ",*DV,SFNAME," already exists! ":
                   " Write over? (Y/N) ",*CL,*-,*UC,REPLY,*LC;
         CMATCH    "Y" TO REPLY
         GOTO      NEWSF2 IF EQUAL
         KEYIN     *P52:24,*EL,"Append to the end? (Y/N) ":
                   *CL,*-,*UC,REPLY,*LC;
         CMATCH    "Y" TO REPLY
         GOTO      ShutDown IF NOT EQUAL
         SPLOPEN   SPLFILE,SFNAME,"Q" /* APPEND TO THE CURRENT SPOOLFILE
         GOTO      CKSFEXIT           /* AND EXIT THE SPOOL FILE START RTN
NEWSF
         NORETURN
         TRAPCLR   IO
         MOVE      "C" TO Reply            /* Will default to C drive
         GOTO      NEWSF3
NEWSF2
         CMOVE     " " TO Reply
         MOVE      Spaces TO Drive
         CLEAR     Reply
         CLEAR     Drive
NEWSF3
         CLEAR     SFName
         APPEND    Reply TO SFName             /*
         APPEND    Drive TO SFName             /* Now using PC filename stds
         APPEND    SFNameC TO SFName           /* Example C:filename.ext
         APPEND    LetNbr TO SFName
         APPEND    PrtExt TO SFName
         APPEND    Spaces TO SFName
         LENSET    SFName
         RESET     SFName
         SPLOPEN   SplFile,SFName
CKSFEXIT
         RETURN                                /*  9/27/92...JMM

EOJ
         SPLCLOSE  SPLFile

         CLOSE     CustMast
         CLOSE     LetFile
         DISPLAY   *R,*P1:24,*EL,SFNAME," is the print file. ";
         CMATCH    "Y" TO INTSW
         GOTO      ShutDown IF EQUAL
         MOVE      Spaces TO NextProg
         CLEAR     NextProg
         APPEND    "PRTENV1 " TO NextProg
         APPEND    LetNbr TO NextProg
         LENSET    NextProg
         RESET     NextProg
         SHUTDOWN  NextProg
ShutDown
         KEYIN     *B,*R,*P1:24,"Program abnormally terminated.........":
                   Reply;
         STOP
