. TDSENVPR.DBS     Print Envelopes from the spooled invoices (TJONINV3.PRT)
. V.M=3.0
.
. ****************************************************************************
.
. --DATE--  -----------------------MODIFICATION----------------------------
. 11/16/92  Program created from TJONINV3.DBS
. 07/23/93  Changed for Casper...
. 12/18/95  Changed to read cust=000 for the current TDS address....MM
. 01/13/96  Changed to skip back-to-back envelopes with same address
. 03/15/97  Ver 1.1 Changed to print a bold courier font...MM
.           Also changed TJONCMDS.INC and had to change this program for
.           the zip code going from a numeric field to a DIM field
. 12/05/98  Ver 1.2 Changes to remove PCL codes from the file. The file
.           will be printed by a batch file (CED Synonym) that will
.           prepend and append the appropriate PCL codes if printing to
.           a PCL printer *or* it will use the appropriate PS codes if
.           printing to a PostScript printer.
. 01/21/99  Ver 3.0 created from V1.2 for PLBWin....MM
.           Required some minor SPLFile tweaking
.
. ****************************************************************************
.
         INCLUDE   COMMON.INC
Version  INIT      "V.M=1.2 "
FF       INIT      0x0C

DIM60    DIM       59
CustWord INIT      "Customer   "
Customer DIM       3

BLANKS   DIM       80
REPLY    INIT      "N"     FORMS ALIGNMENT REPLY
INTSW    INIT      " "     Y=Interrupt in progress
Firstime INIT      "Y"
TRYD     INIT      "Y"
. ************************************************************
. Note....  The routines that printed these codes were deactivated
.           in V1.2 on 12/05/98  (MM)
InitEnv  INIT      0x1B,"&l3h1o26E":
                   0x1B,"(s10h12v0s3b3T":
                   0x1B,")s10h12v0s3b3T":
                   0x1B,"&a1R"
ResetLJ  INIT      0x1B,"E"
. ***************************************************************
MM       DIM       2
DD       DIM       2
YY       DIM       2
WORK2    DIM       2
.
CUSTMAST IFILE     VARIABLE=1024,NODUP,WEOF
.
INVOICES FILE                   /* Spooled invoices
InvFile  DIM       80           /* Filename to open the spooled invoices
.
SF       FILE                   /* Used to check for spool file
SPLFILE  PFILE                  /* Spool File
SFNAME   DIM       80           /*      "TDSENVPR.PRT"
.
         INCLUDE   TJONCMDS.INC
.
.
KEY      DIM       3
CTRLKEY  DIM       12
SEQ      FORM      "-2"
SEQ1     FORM      "-2"
SEQ2     FORM      "-2"
SAVECUST DIM       3

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
.
MONTH    DIM       2
DAY      DIM       2
YEAR     DIM       2
DRIVE    INIT      ":"
SAVEINUM FORM      4
INVNUMS  DIM       4     SAVE AREA FOR INVOICE NBR
LINE1    DIM       30                         12/9...JMM
DIMWRK9  DIM       9
DIMWRK10 DIM       10
FILLER   DIM       12
INDEX    FORM      2
IFSCUST  DIM       3
IFSINV   DIM       4
IFKEYSV  DIM       7
NbrEnvs  FORM      "  0"                /* Number of Envelopes printed
RecIn    DIM       127
.
. INITIALIZE FILLERS TO SPACES...JMM
.
.
TDSENVPR RESET     BLANKS TO 80
         LENSET    BLANKS
         RESET     BLANKS
         MOVE      Blanks TO SavAddr
.
         TRAP      INT IF INTERRUPT
         MOVE      "C:\TDS\TJONINV3.PRT" TO InvFile
.
OpenINV
         TRAP      NOIF IF IO
         OPEN      INVOICES,InvFile
         TRAPCLR   IO
.
.
.
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
.
.
.
         MOVE      "N" TO EOJSW
         MOVE      "99" TO SAVECUST
.
.
.
         UNPACK    TODAY TO MM, REPLY, DD, REPLY, YY
.
.
.
         DISPLAY   *COLOR 23,*ES:
                   *P10:04,"TDS Conversions, Inc.":
                   " Envelope Print Program  ":
                   Version:
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
ReadInvF
         READ      INVOICES,SEQ;*LC,RecIn
         GOTO      EOJExit IF OVER
         UNPACK    RecIn TO DIM60, CustWord, Customer
*        KEYIN     *R,*P1:24,*DV,DIM60,*DV,CustWord,*DV,Customer:
*                  REPLY;
         MATCH     "CUSTOMER   " TO CustWord
         GOTO      ReadCust IF EQUAL
         MATCH     "Customer   " TO CustWord
         GOTO      ReadCust IF EQUAL
         GOTO      ReadInvF
ReadCust
         MATCH     Customer TO SaveCust
         GOTO      ReadInvF IF EQUAL         /* Only 1 envelope per cust req'd
         READ      CUSTMAST,Customer;*LC,CustRcd
         GOTO      ReadInvF IF OVER
         TRAPCLR   RANGE

. The following code is checking for duplicate addresses
         MATCH     CustAddr TO SavAddr
         GOTO      TDSCont1 IF NOT EQUAL
         MATCH     CustCity TO SavCity
         GOTO      TDSCont1 IF NOT EQUAL
         MATCH     CustSte  TO SavSte
         GOTO      TDSCont1 IF NOT EQUAL
         MATCH     CustZip  TO SavZip
         GOTO      TDSCont1 IF NOT EQUAL
         GOTO      ReadInvF                  ;; Same, skip printing

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
         CMOVE     "N" TO Firstime
         GOTO      ReadInvF
.
InitPrtr
. No-Op  PRINT     InitEnv                 ;; Removed 12/05/98 V1.2
         RETURN
.
.    EOJ PROCESSING
.
EOJEXIT
         KEYIN     *R,*P1:24:
                   "   Total number of envelopes printed--------->":
                   *DV,NbrEnvs," ",*CL,REPLY;
         CMATCH    "Y" TO INTSW
         GOTO      NOREL IF NOT EQUAL
         DISPLAY   *B,*R,*P1:24:
                   "  Printing has been interrupted....";
NOREL
.
.    Reset the laser printer
.
. No-Op  PRINT     ResetLJ;       /* Reset the laser printer   (V1.2 12/05)
         SPLCLOSE  SplFile        /* 1/6/86...JMM
         CLOSE     CUSTMAST
         CLOSE     INVOICES
.
ShutDown STOP                   /*        GO BACK TO MENU    12/5...JMM
INT
         NORETURN
         CMOVE     "Y" TO INTSW
         GOTO      EOJEXIT
.
CKSF
         TRAP      NEWSF IF IO
         CLEAR     SFNAME
         APPEND    "TDSENVPR.PRT" TO SFNAME
         RESET     SFNAME
         OPEN      SF,SFNAME
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
         SPLOPEN   SplFile,SFNAME,"Q"  /* APPEND TO THE CURRENT SPOOLFILE
         GOTO      CKSFEXIT            /* AND EXIT THE SPOOL FILE START RTN
NEWSF
         NORETURN
         TRAPCLR   IO
         CLEAR     REPLY
         CMOVE    "C" to reply
*        KEYIN     *P1:24,*EL,"Create ",*LL,*DV,SFNAME," on which drive? ":
*                  *CL,*-,"    ?:",*P47:24,*UC,REPLY,*LC;
         CMATCH   "C" to reply
         goto      newsf3 if equal
         CMATCH   "D" to reply
         goto      newsf3 if equal
         CMATCH   "A" to reply
         goto      newsf3 if equal
         goto      newsf if not eos
         move      "C" to reply            /* Will default to C drive
         GOTO      NEWSF3
NEWSF2
         CMOVE     " " TO REPLY
         MOVE      BLANKS TO DRIVE
         clear     reply
         clear     drive
NEWSF3
         CLEAR     SFNAME
         append    reply to sfname             /*
         append    drive to sfname             /* Now using PC filename stds
         APPEND    "TDSENVPR.PRT " TO SFNAME   /* Example D:filename.ext
         LENSET    SFNAME
         RESET     SFNAME
         SPLOPEN   SplFile,SFNAME
CKSFEXIT
         GOTO      ReadInvF
.
NOIF
         CMATCH    "Y" TO TRYD
         GOTO      NOIFMSG IF NOT EQUAL
         NORETURN
         CMOVE     "N" TO TRYD
         MOVE      "D:\COMPLETE\TJONINV3.PRT" TO InvFile
         GOTO      OpenINV

NOIFMSG
         KEYIN     *B,*P1:24,*EL,"Printed Invoices file is missing [":
                   *DV,*LL,InvFile,"] ",*CL:
                   REPLY;
         STOP



NoTDSrec
         KEYIN     *B,*P1:24,*EL,"TDS Control record (Cust=000) missing. ":
                   "[CUSTMAST] ":
                   REPLY;
         STOP


ARMISS
         KEYIN     *B,*P1:24,*EL,"Accounts Receivable Master is missing. ":
                   "[CUSTMAST] ":
                   REPLY;
         STOP


NOINVREC
         KEYIN     *B,*P1:24,*EL,"No Invoice Record found.....":
                   REPLY;
         STOP


EOJ      MOVE      "Y" TO EOJSW
         TRAPCLR   RANGE
         GOTO      EOJExit
