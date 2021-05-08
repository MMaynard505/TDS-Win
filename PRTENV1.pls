. PRTENV1.DBS     Print Envelopes for current customers
. V.M=3.0
.
. ****************************************************************************
.
. --DATE--  -----------------------MODIFICATION----------------------------
. 01/18/98  Program created from TDSENVPR.DBS Ver 1.1
. 01/21/99  Ver 3.0 created from PrtEnv1.     Ver 1.0
.
.
.
. ****************************************************************************
.
         INCLUDE   COMMON.INC
Version  INIT      "PRTENV1  V.M=3.0"

CustMast IFILE     VARIABLE=1024,WEOF

         INCLUDE   TJONCMDS.INC

CustNbr  DIM       3             ;; Unique Customer Number
CustNbrS DIM       8             ;; Save area for Current Customer filename

Spaces   DIM       80
Blanks   DIM       80
Line     DIM       80

SF       FILE
SplFile  PFILE

SFName   DIM       18           /*      "CUSTLETx.PRT"
SFNameC  INIT      "CUSTLET"    ;; Print file name (first 7)
EnvExt   INIT      ".Env"
CLNameC  INIT      "CUSTLET"
CLName   DIM       8


DIM60    DIM       59
CustWord INIT      "Customer   "
Customer DIM       3

Reply    INIT      "N"     FORMS ALIGNMENT REPLY
IntSw    INIT      " "     Y=Interrupt in progress
Firstime INIT      "Y"
TryD     INIT      "Y"
InitEnv  INIT      0x1B,"&l3h1o26E":
                   0x1B,"(s10h12v0s3b3T":
                   0x1B,")s10h12v0s3b3T":
                   0x1B,"&a1R"
ResetLJ  INIT      0x1B,"E"
MM       DIM       2
DD       DIM       2
YY       DIM       2
WORK2    DIM       2

KEY      DIM       3
Seq      FORM      "-2"
SaveCust DIM       3

TDSNum   INIT      "000"  /*     1-3    Customer number    (Primary Key)
TDSName  DIM       25     /*     4-28   Company name
TDSAddr  DIM       25     /*    29-53   Address
TDSCity  DIM       14     /*    54-67   City
TDSSte   DIM        2     /*    68-69   State
TDSZip   DIM        5     /*    70-74   Zip code

SavAddr  DIM       25     /*       Save area for duplicate checking
SavCity  DIM       14     /*
SavSte   DIM        2     /*
SavZip   DIM        5     /*

EOJSW    DIM       1
FIRSTSW  DIM       1
TOF      EQU       014     /* Decimal 12 = Printer top of form
CR       EQU       015     /*              Printer Carriage Return
LF       EQU       012     /*              Printer Line Feed
FF       INIT      0x0C

CDate    DIM       8
CDate2   DIM       10                  ;; Date with Century
PageCnt  FORM      " 0"
BOF      FORM      "0"
LetNbr   DIM       1
YearWork FORM      4
DateMin  DIM       10                 ;; CCYYMMDD Minimum date for 'current'
DateMinW FORM      10                 ;; Numeric work area for testing date
CurrDatW FORM      10
CurrDate DIM       10
Century  DIM       2                  ;; Century field=19 if YY > 75

CustDtY4 DIM       4
CustDtM2 DIM       2
CustDtD2 DIM       2

Month    DIM       2
Day      DIM       2
Year     DIM       2

DRIVE    INIT      ":"
LINE1    DIM       30                         12/9...JMM
DIMWRK9  DIM       9
DIMWRK10 DIM       10
FILLER   DIM       12
INDEX    FORM      2
IFSCUST  DIM       3
IFKEYSV  DIM       7
NbrEnvs  FORM      "  0"                /* Number of Envelopes printed
RecIn    DIM       127
.
. =================================================================
         RESET     Spaces TO 80
         LENSET    Spaces
         RESET     Spaces
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


. =================================================================
.
TDSEnvPr
         RESET     Blanks TO 80
         LENSET    Blanks
         RESET     Blanks
         MOVE      Blanks TO SavAddr

         TRAP      ARMISS IF IO
         OPEN      CUSTMAST,"CUSTMAST"
         TRAPCLR   IO

         READ      CUSTMAST,TDSNum;*LC,CustRcd ;; TDS is Customer Number 000
         GOTO      NoTDSrec IF OVER

         MOVE      CustName TO TDSName
         MOVE      CustAddr TO TDSAddr

         ENDSET    CustCity
         LENSET    CustCity
TDSLoop1
         CMATCH    " " TO CustCity
         GOTO      TDSLoop2 IF NOT EQUAL
         BUMP      CustCity BY -1
         GOTO      TDSLoop1
TDSLoop2
         LENSET    CustCity
         RESET     CustCity

         MOVE      CustCity TO TDSCity
         MOVE      CustSte  TO TDSSte
         MOVE      CustZip  TO TDSZIp

         MOVE      "N" TO EOJSW
         MOVE      "99" TO SAVECUST
         UNPACK    TODAY TO MM, REPLY, DD, REPLY, YY
         DISPLAY   *COLOR 23,*ES:
                   *P10:04,"TDS Conversions, Inc.":
                   " Special Envelope Print Program":
                   "  V.M=1.0 ":
                   *P10:06,"Return address....",TDSName:
                   *P28:07,TDSAddr,*P28:08,*LL,TDSCity,", ":
                   TDSSte,"  ",TDSZip;
.
GETHC
         KEYIN     *P1:13,"Ready to create the envelope spool file?":
                   " (Y/N) ":
                   *+,*UC,REPLY;
         CMATCH    "Y" TO REPLY
         GOTO      ShutDown IF NOT EQUAL      /* No, just stop
         GOTO      CKSF
.
.
. Here we will read CUSTMAST and use current customers....
.

ReadCust
         MOVE      Spaces TO Key                   ;; Position to BOF
         READ      CustMast,Key;;

ReadLoop
         READKS    CustMast;*LC,CustRcd
         GOTO      EOJ IF OVER
         TRAPCLR   RANGE
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

TDSCont1
         MOVE      CustAddr TO SavAddr
         MOVE      CustCity TO SavCity
         MOVE      CustSte  TO SavSte
         MOVE      CustZip  TO SavZip

PrintEnv
         CMATCH    "Y" TO Firstime
         CALL      InitPrtr IF EQUAL
         ENDSET    CustCity
         LENSET    CustCity
CityLoop
         CMATCH    " " TO CustCity
         GOTO      GotCity IF NOT EQUAL
         BUMP      CustCity BY -1
         GOTO      CityLoop
GotCity
         LENSET    CustCity
         RESET     CustCity
.
         CMATCH    "Y" TO Firstime
         GOTO      PrintIt  IF EQUAL
         PRINT     FF
PrintIt
         PRINT     "   ",TDSName,*N:
                   "   ",TDSAddr,*N:
                   "   ",*LL,TDSCity,", ",TDSSte,"  ",TDSZip,*N:
                   *N,*N,*N,*N:
                   *N,*N,*N:
                   *N,*47,CustAttn:
                   *N,*47,CustName:
                   *N,*47,CustAddr:
                   *N,*47,*LL,CustCity,", ",CustSte,"  ",CustZip:
                   *N;
         MOVE      Customer TO SaveCust              /* Only 1 envelope req'd
         ADD       "1" TO NbrEnvs

. Also need a self-addressed return envelope

         PRINT     FF
         PRINT     "   ",CustName,*N:    CustName
                   "   ",CustAddr,*N:    CustAddr
                   "   ",*LL,CustCity,", ",CustSte,"  ",CustZip,*N:
                   *N,*N,*N,*N:
                   *N,*N,*N:
                   *N:
                   *N,*47,TDSName:
                   *N,*47,TDSAddr:
                   *N,*47,*LL,TDSCity,", ",TDSSte,"  ",TDSZip:
                   *N;

         ADD       "1" TO NbrEnvs

         CMOVE     "N" TO Firstime
         GOTO      ReadLoop
.
InitPrtr
         PRINT     InitEnv
         RETURN
.
.    EOJ PROCESSING
.
EOJExit
         DISPLAY   *R,*P1:24:
                   "   Total number of envelopes printed--------->":
                   NbrEnvs," ";
         CMATCH    "Y" TO INTSW
         GOTO      NOREL IF NOT EQUAL
         DISPLAY   *B,*R,*P1:24:
                   "  Printing has been interrupted....";
NOREL
.
.    Reset the laser printer
.
         PRINT     ResetLJ;       /* Reset the laser printer
         SPLCLOSE  SplFile        /* 1/6/86...JMM
         CLOSE     CUSTMAST
.
ShutDown STOP                   /*        GO BACK TO MENU    12/5...JMM
INT
         NORETURN
         CMOVE     "Y" TO INTSW
         GOTO      EOJEXIT
.
CKSF
         TRAP      NEWSF IF IO
         CLEAR     SFName
         APPEND    SFNameC TO SFName
         APPEND    LetNbr  TO SFName
         APPEND    EnvExt  TO SFName
         LENSET    SFName
         RESET     SFName
         OPEN      SF,SFName
         CLOSE     SF
         TRAPCLR   IO
         KEYIN     *B,*P1:24,*EL:
                   "The print file ",*LL,*DV,SFNAME," already exists! ":
                   *P56:24," Write over? (Y/N)",*CL,*-,*UC,REPLY,*LC;
         CMATCH    "Y" TO REPLY
         GOTO      NEWSF2 IF EQUAL
         KEYIN     *P56:24,*EL,"Append to the end? (Y/N)":
                   *CL,*-,*UC,REPLY,*LC;
         CMATCH    "Y" TO REPLY
         GOTO      ShutDown IF NOT EQUAL
         SPLOPEN   SplFile,SFNAME,"Q" /* APPEND TO THE CURRENT SPOOLFILE
         GOTO      CKSFEXIT           /* AND EXIT THE SPOOL FILE START RTN
NewSF
         NORETURN
         TRAPCLR   IO
NewSF2
         MOVE     "C" to Reply
         CLEAR     SFName
         APPEND    Reply to SFName             /*
         APPEND    Drive to SFName             /* Now using PC filename stds
         APPEND    SFNameC TO SFName
         APPEND    LetNbr  TO SFName
         APPEND    EnvExt  TO SFName
         LENSET    SFNAME
         RESET     SFNAME
         SPLOPEN   SplFile,SFNAME
CKSFEXIT
         GOTO      ReadCust

NoTDSrec
         KEYIN     *B,*P1:24,*EL,"TDS Control record (Cust=000) missing. ":
                   "[CUSTMAST] ":
                   REPLY;
         STOP
.
.
ARMISS
         KEYIN     *B,*P1:24,*EL,"Accounts Receivable Master is missing. ":
                   "[CUSTMAST] ":
                   REPLY;
         STOP

EOJ      MOVE      "Y" TO EOJSW
         TRAPCLR   RANGE
         GOTO      EOJExit
