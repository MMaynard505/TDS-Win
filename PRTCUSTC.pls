. PRTCUSTC.DBS    Print CSC Customers from the CUSTMAST file
. V.M=1.0
.
. ****************************************************************************
.
. --DATE--  -----------------------MODIFICATION----------------------------
. 09/27/92  Initial version
.
.
. ****************************************************************************
.
         INCLUDE   COMMON/INC
SPACES   DIM       80
.
CUSTMAST IFILE     VARIABLE=1024,NODUP,WEOF
SF       FILE
SplFile  PFILE
SFNAME   DIM       18           /*      "PRTCUSTC.PRT"
SFNAMEC  INIT      "PRTCUSTC.PRT"
DRIVE    INIT      ":"
.
         INCLUDE   TJONCMDS/INC
.
KEY      DIM       3
PN1      DIM       3
PN2      DIM       3
PN3      DIM       4
SEQ      FORM      "-2"
RecsOut  FORM      3
IntSW    INIT      "N"
REPLY    DIM       1
MaxLines FORM      "60"
LineCnt  FORM      2
CDate    DIM       8
PageCnt  FORM      " 0"

.
.
CONVERTC RESET     SPACES TO 80
         LENSET    SPACES
         RESET     SPACES
         MOVE      MaxLines TO LineCnt
         CLOCK     DATE TO CDATE
         REPLACE   "-/" IN CDATE
.
         TRAP      INT IF INTERRUPT
         TRAP      ARMISS IF IO
         OPEN      CUSTMAST,"CUSTMAST"
         TRAPCLR   IO
.
         CALL      CKSF                   /* Create the spool file
.
         MOVE      SPACES TO Key
         READ      CUSTMAST,Key;;
         MOVE      "0" TO RecsOut
         DISPLAY   *COLOR 23:
                   *R,*P1:24,"Records Printed------------->":
                   *P30:24,RecsOut;
ReadLoop
         READKS    CUSTMAST;CustRcd
         GOTO      EOJ IF OVER
.
.
. Search for CSC records                          9/27/92
.
.
         MATCH     "CSC" TO CustName             /* 9/27/92
         GOTO      ReadLoop IF NOT EQUAL         /*
         ADD       "1" TO RecsOut
         COMPARE   MaxLines TO LineCnt
         CALL      PrintHdr IF NOT LESS
         Display   *P30:24,RecsOut;
         PRINT     *1,"Cust ##",CustNum,*20,CustAttn,*50,CustAddr,*N:
                   *1,CustCity,*20,"Phone:",CustPhon,*50,CustCity,"  ":
                               CustSte,"  ",CustZip,*N:
                               *20,"Fax:  ",CustFax,*50,CustPOB,*N
         ADD       "4" TO LineCnt
         GOTO      ReadLoop
.
PrintHdr
         ADD       "1" TO PageCnt
         COMPARE   "1" TO PageCnt
         GOTO      PrintPg1 IF EQUAL
         PRINT     *F;
PrintPg1
         PRINT     *1,CDATE,*30,"CSC Customer Contacts":
                   *68,"Page ",PageCnt,*N,*N:
                   *1,"Nbr & Location",*20,"    Key Contact":
                   *50,"    Address",*N:
                   *1,"--------------",*20,"-------------------------":
                   *50,"-------------------------",*N
         MOVE      "6" TO LineCnt
         RETURN
.
INT
         NORETURN
         CMOVE     "Y" TO INTSW
         GOTO      EOJ
.
.
ARMISS
         KEYIN     *B,*P1:24,*EL,"Accounts Receivable Master is missing. ":
                   "(CUSTMAST) ":
                   REPLY;
         STOP
.
CKSF
         TRAP      NEWSF IF IO
         MOVE      SFNAMEC TO SFNAME
         OPEN      SF,SFNAME
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
         SPLOPEN   SplFile,SFNAME,"Q"  /* APPEND TO THE CURRENT SPOOLFILE
         GOTO      CKSFEXIT            /* AND EXIT THE SPOOL FILE START RTN
NEWSF
         NORETURN
         TRAPCLR   IO
         MOVE      "C" TO REPLY            /* Will default to C drive
         GOTO      NEWSF3
NEWSF2
         CMOVE     " " TO REPLY
         MOVE      SPACES TO DRIVE
         CLEAR     REPLY
         CLEAR     DRIVE
NEWSF3
         CLEAR     SFNAME
         append    reply to sfname             /*
         append    drive to sfname             /* Now using PC filename stds
         APPEND    SFNAMEC TO SFNAME           /* Example C:filename.ext
         APPEND    SPACES TO SFNAME
         LENSET    SFNAME
         RESET     SFNAME
         SPLOPEN   SplFile,SFNAME
CKSFEXIT
         RETURN                                /*  9/27/92...JMM
.
EOJ
         CLOSE     CUSTMAST
         SPLCLOSE  SplFile
         KEYIN     *R,*P1:24,*EL,*DV,SFNAME," is the print file. ":
                   REPLY;
         CMATCH    "Y" TO INTSW
         STOP      IF NOT EQUAL
ShutDown
         KEYIN     *B,*R,*P1:24,"Program abnormally terminated.........";
         STOP
