. ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
. บ  TJONINV2                    Enter Accounting Detail Records               บ
. บ  V.M=3.0                                                                   บ
. บ                                                                            บ
. ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
.
. ษออออออออออัอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
. บ  Date    ณ                 Modification                                    บ
. วฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
. บ 06/21/95 ณ V.M=2.2 Increased prices from $25 to $29                        บ
. บ          ณ  Changes for automatic posting of shipping & handling charges   บ
. บ 01/21/99 ณ V.M=3.0 Copy of V.M=2.2 for PLBWin                              บ
. บ          ณ                                                                 บ
. บ          ณ                                                                 บ
. บ          ณ                                                                 บ
. บ          ณ                                                                 บ
. บ          ณ                                                                 บ
. บ          ณ                                                                 บ
. ศออออออออออฯอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ

         INCLUDE   COMMON.INC
VERSION  INIT      "TJONINV2 V.M=3.0"
CUSTMAST IFILE     VARIABLE=1024,NODUP,WEOF
TIMEFILE FILE
         INCLUDE   TJONCMDS.INC
         INCLUDE   TIMERCDS.INC
KEY      DIM       3
IWOSAVE  INIT      "    "
INVNUM   FORM      4
SEQ      FORM      "-1"
REPLY    INIT      "N"           ;; Leave this default 'N'.... 6/11/90... [JMM]
Bureau   INIT      " "           ;; Save area for bureau (C,T,U)
ShipChg  INIT      "3.00"        ;; Unit Price for S&H   6/21/95
ConvChg  INIT      "29.00"       ;; Unit Price for Conv  6/21/95
WORK9    FORM      7.2
IWDSAVE  FORM      6
DATE     DIM       6
DISPDATE DIM       8
DISPTR   FORM      2.2
MMW      DIM       2
DDW      DIM       2
YYW      DIM       2
SPACES   DIM       80
S28      DIM       28                           " 28 SPACES
S10      DIM       10         "                   10 SPACES
S6       DIM       6       "                       6 SPACES
.
BEGIN    RESET     SPACES TO 80
         LENSET    SPACES
         RESET     SPACES
         MOVE      SPACES TO S28
         MOVE      SPACES TO S10
         MOVE      SPACES TO S6
         TRAP      NOARMAST IF IO
         OPEN      CUSTMAST,"CUSTMAST"
         TRAPCLR   IO

         TRAP      NOINVMST IF IO
         OPEN      TIMEFILE,"TIMEFILE"
         TRAPCLR   IO
         POSITEOF  TIMEFILE                          /*  5/17/88...JMM

START    DISPLAY   *COLOR 23,*ES:
                   *P01:01,VERSION:
                   *P30:1,"    TDS Conversions":
                   *P30:3,"Time Accounting Program"
. No-Op  KEYIN     *P10:5:
. No-Op            "Is this an invoice to a Water District? (Y/N) ",REPLY:
. No-Op            *P10:5,*EL;
         CMATCH    "Y" TO REPLY
         GOTO      INV7 IF EQUAL
         CMATCH    "y" TO REPLY
         GOTO      KEYDATE IF NOT EQUAL
INV7
         CHAIN     "TJONINV7"             SPECIAL INVOICING PROGRAM FOR MUDS
.
KEYDATE  KEYIN     *P10:5,"Enter Accounting Date ":
                   *P32:5,"MM/DD/YY":
                   *DE,*JR,*ZF,*+:
                   *P32:5,MMW,"/",DDW,"/",*-,YYW;
         CMATCH    " " TO MMW
         GOTO      KEYDATE IF EOS
         CLEAR     DATE
         APPEND    MMW TO DATE
         APPEND    DDW TO DATE
         APPEND    YYW TO DATE
         RESET     DATE
         MOVE      DATE TO IWRKDATE
         MOVE      DATE TO IWDSAVE
         PACK      DISPDATE FROM MMW, "/", DDW, "/", YYW
KEYINAR  KEYIN     *P32:05,*DV,DISPDATE,*EF,*P10:6,"Enter Customer Number ":
                   *+,*P32:06,"___",*P32:06,*RV,*DE,*JR,*ZF,KEY:
                   *P32:06,*DV,KEY;
         MATCH     "999" TO KEY
         GOTO      EOJ IF EQUAL
         CMATCH    "*" TO KEY
         GOTO      KeyInAR IF EOS
         GOTO      EOJ IF EQUAL
         TRAP      RANGETRP IF RANGE
         READ      CUSTMAST,KEY;CustRcd
         GOTO      DISPCUST IF NOT OVER
         DISPLAY   *B,*P35:06," อออ Customer ",KEY:
                   " is not on file. ";
         GOTO      KEYINAR
DISPCUST
         MOVE      KEY TO ICUST
         TRAPCLR   RANGE
         DISPLAY   *P10:8,CUSTNAME:
                   *P10:9,CUSTADDR:
                   *P10:10,CUSTCITY,*P23:10,"  ",*P25:10,CUSTSTE:
                   *P27:10,"  ",*P29:10,CUSTZIP:
                   *P10:11,CUSTATTN:
                   *P50:08,*HON," C = Diskette + S&H for CBI ":
                   *P50:09,*HON," T = Diskette + S&H for TRW ":
                   *P50:10,*HON," U = Diskette + S&H for TU  ":
                   *P50:11,*HON," 3 = All 3 Bureaus price    ":
                   *HOFF;
         CALC      DISPTR=(TAXRATE*100)
. NO-OP  CMOVE     "N" TO REPLY        FORCE THEM TO SAY YES
. NO-OP  KEYIN     *+,*P10:24,*EL,"RIGHT CUSTOMER ?  Y or N":
. NO-OP            *P35:24,REPLY
. NO-OP  CMATCH    "Y" TO REPLY
. NO-OP  GOTO      KEYINAR IF NOT EQUAL
LineItem
         DISPLAY   *COLOR 19:
                   *P60:05,"Sales Tax Rate (",TAXSTATE,")":
                   *P66:06,DISPTR,"%":
                   *P01:14,*EF,"W.O.#   Quantity     Type":
                   *P30:14,"--------Description---------":
                   *P60:14,"Unit Price  Ext Cost":
                   *P1:16,"____   __________   ______":
                   *P30:16,"____________________________":
                   *P30:18,"____________________________":
                   *P61:18,"_________";
REKEY
         MOVE      IWOSAVE TO IWO
         KEYIN     *COLOR 23:
                   *+,*P01:16,*DV,IWO,*RV,*P01:16,IWO,*P01:16,*DV,IWO;
         CMATCH    "*" TO IWO
         GOTO      GETINFO IF EOS
         GOTO      ChgCust IF EQUAL
         MATCH     "9999" TO IWO
         GOTO      GETINFO IF NOT EQUAL
ChgCust
         CALL      WRITENUL     WRITE A NULL RECORD TO CHANGE CUSTOMERS
         MOVE      IWDSAVE TO IWRKDATE
         GOTO      KEYINAR
GETINFO
         MOVE      IWO TO IWOSAVE
         CMOVE     " " TO Bureau         /* 6/21/95
         MOVE      "1" TO IQUAN          /* 6/21/95
.
.  Removed the 'Type' field from the following KEYIN  6/21/95
.                  *P21:16,*RV,ITYPE,*P21:16,*DV,S6,*P21:16,*DV,ITYPE:
.
         KEYIN     *+,*P08:16,*JR,*RV,IQUAN,*P08:16,*DV,IQUAN:
                   *IT:
                   *P30:16,*RV,IDESC1,*P30:16,*DV,S28,*P30:16,*DV,IDESC1;

. Check for Special 3 Bureau Pricing...........  4/7/94
         CMATCH    "3" TO IDESC1         /* Possibly a "Macro"?
         GOTO      GetInfo IF EOS
         GOTO      CkSpcl1 IF EQUAL      /* Yes, then check further

         CMATCH    "c" TO IDESC1         /* Possibly a "Macro"?
         GOTO      DCBI IF EQUAL         /* Yes, then check further

         CMATCH    "C" TO IDESC1         /* Possibly a "Macro"?
         GOTO      DCBI IF EQUAL         /* Yes, then check further

         CMATCH    "t" TO IDESC1         /* Possibly a "Macro"?
         GOTO      DTRW IF EQUAL         /* Yes, then check further

         CMATCH    "T" TO IDESC1         /* Possibly a "Macro"?
         GOTO      DTRW IF EQUAL         /* Yes, then check further

         CMATCH    "u" TO IDESC1         /* Possibly a "Macro"?
         GOTO      DTU  IF EQUAL         /* Yes, then check further

         CMATCH    "U" TO IDESC1         /* Possibly a "Macro"?
         GOTO      DTU  IF EQUAL         /* Yes, then check further

. Check for Diskette Conversion Macro
         CMATCH    "d" TO IDESC1         /* Possibly a "Macro"?
         GOTO      DCBI IF EQUAL         /* Yes, then check further

         CMATCH    "D" TO IDESC1         /* Possibly a "Macro"?
         GOTO      DCBI IF EQUAL         /* Yes, then check further

         MATCH     "dt" TO IDESC1
         GOTO      CkDisk1 IF LESS        /* Less than 2 chars entered?
         GOTO      DTRW IF EQUAL
         MATCH     "DT" TO IDESC1
         GOTO      CkDisk1 IF LESS        /* Less than 2 chars entered?
         GOTO      DTRW IF EQUAL
         MATCH     "du" TO IDESC1
         GOTO      CkDisk1 IF LESS        /* Less than 2 chars entered?
         GOTO      DTU  IF EQUAL
         MATCH     "DU" TO IDESC1
         GOTO      CkDisk1 IF LESS        /* Less than 2 chars entered?
         GOTO      DTU  IF EQUAL
         CMATCH    "D" TO IDESC1         /* Possibly a "Macro"?
         GOTO      CkDisk1 IF EQUAL       /* Yes, then check further
         CMATCH    "d" TO IDESC1         /* Possibly a "Macro"?
         GOTO      CkDisk1 IF EQUAL       /* Yes, then check further

. Check for Modem Transmission Macro
         MATCH     "mt" TO IDESC1
         GOTO      CkXmit1 IF LESS        /* Less than 2 chars entered?
         GOTO      MTRW IF EQUAL
         MATCH     "MT" TO IDESC1
         GOTO      CkXmit1 IF LESS        /* Less than 2 chars entered?
         GOTO      MTRW IF EQUAL
         MATCH     "mu" TO IDESC1
         GOTO      CkXmit1 IF LESS        /* Less than 2 chars entered?
         GOTO      MTU  IF EQUAL
         MATCH     "MU" TO IDESC1
         GOTO      CkXmit1 IF LESS        /* Less than 2 chars entered?
         GOTO      MTU  IF EQUAL
         CMATCH    "M" TO IDESC1         /* Possibly a "Macro"?
         GOTO      CkXmit1 IF EQUAL       /* Yes, then check further
         CMATCH    "m" TO IDESC1         /* Possibly a "Macro"?
         GOTO      CkXmit1 IF EQUAL       /* Yes, then check further

. Check for Shipping Charges Macro
         MATCH     "st" TO IDESC1
         GOTO      CkShip1 IF LESS        /* Less than 2 chars entered?
         GOTO      STRW IF EQUAL
         MATCH     "ST" TO IDESC1
         GOTO      CkShip1 IF LESS        /* Less than 2 chars entered?
         GOTO      STRW IF EQUAL
         MATCH     "su" TO IDESC1
         GOTO      CkShip1 IF LESS        /* Less than 2 chars entered?
         GOTO      STU  IF EQUAL
         MATCH     "SU" TO IDESC1
         GOTO      CkShip1 IF LESS        /* Less than 2 chars entered?
         GOTO      STU  IF EQUAL
         CMATCH    "S" TO IDESC1         /* Possibly a "Macro"?
         GOTO      CkShip1 IF EQUAL       /* Yes, then check further
         CMATCH    "s" TO IDESC1         /* Possibly a "Macro"?
         GOTO      CkShip1 IF EQUAL       /* Yes, then check further

         GOTO      KeyD2                 /* No,  Continue....    8/14/90

CkDisk1
         BUMP      IDESC1
         GOTO      KeyD2   IF NOT EOS
DCBI
         CMOVE     "C" TO Bureau         /* 6/21/95
         MOVE      "Diskette to tape conversions" TO IDESC1
         MOVE      "for credit reporting to CBI." TO IDESC2
         DISPLAY   *P30:16,IDESC1,*P30:18,IDESC2;
         MOVE      ConvChg TO IUPRICE                /* Unit Price for CBI
         GOTO      Compute
DTRW
         CMOVE     "T" TO Bureau         /* 6/21/95
         MOVE      "Diskette to tape conversions" TO IDESC1
         MOVE      "for credit reporting to TRW." TO IDESC2
         DISPLAY   *P30:16,IDESC1,*P30:18,IDESC2;
         MOVE      ConvChg TO IUPRICE                /* Unit Price for TRW
         GOTO      Compute

DTU
         CMOVE     "U" TO Bureau         /* 6/21/95
         MOVE      "Diskette to tape conversions" TO IDESC1
         MOVE      "for credit reporting to TU. " TO IDESC2
         DISPLAY   *P30:16,IDESC1,*P30:18,IDESC2;
         MOVE      ConvChg TO IUPRICE                /* Unit Price for TU
         GOTO      Compute

CkXmit1
         BUMP      IDESC1
         GOTO      KeyD2   IF NOT EOS
MCBI
         MOVE      "Modem transmission of data  " TO IDESC1
         MOVE      "for credit reporting to CBI." TO IDESC2
         DISPLAY   *P30:16,IDESC1,*P30:18,IDESC2;
         GOTO      Compute
MTRW
         MOVE      "Modem transmission of data  " TO IDESC1
         MOVE      "for credit reporting to TRW." TO IDESC2
         DISPLAY   *P30:16,IDESC1,*P30:18,IDESC2;
         GOTO      Compute

MTU
         MOVE      "Modem transmission of data  " TO IDESC1
         MOVE      "for credit reporting to TU. " TO IDESC2
         DISPLAY   *P30:16,IDESC1,*P30:18,IDESC2;
         GOTO      Compute

CkShip1
         BUMP      IDESC1
         GOTO      KeyD2   IF NOT EOS
SCBI
         MOVE      "Shipping & Handling charges." TO IDESC1
         MOVE      "(Tapes sent to CBI-Atlanta) " TO IDESC2
         DISPLAY   *P30:16,IDESC1,*P30:18,IDESC2;
         MOVE      ShipChg TO IUPRICE                /* Unit Price for S&H
         GOTO      Compute
STRW
         MOVE      "Shipping & Handling charges." TO IDESC1
         MOVE      "(Tapes sent to TRW-Allen,TX)" TO IDESC2
         DISPLAY   *P30:16,IDESC1,*P30:18,IDESC2;
         MOVE      ShipChg TO IUPRICE                /* Unit Price for S&H
         GOTO      Compute

STU
         MOVE      "Shipping & Handling charges." TO IDESC1
         MOVE      "(Tapes sent to TU-Chicago)  " TO IDESC2
         DISPLAY   *P30:16,IDESC1,*P30:18,IDESC2;
         MOVE      ShipChg TO IUPRICE                /* Unit Price for S&H
         GOTO      Compute

******************************************************************************
CkSpcl1
         BUMP      IDESC1
         GOTO      KeyD2   IF NOT EOS

         MOVE      "Special pricing for reports " TO IDESC1
         MOVE      "sent to all 3 credit bureaus" TO IDESC2
         MOVE      "67.00" TO IEXTCOST
         KEYIN     *P30:16,*DV,IDESC1,*P30:18,*DV,IDESC2:
                   *P72:18,*DV,IEXTCOST:
                   *P10:24,"Screen Correct?  (Y/N) ":
                   *P35:24,*UC,REPLY,*LC;
         CMATCH    "Y" TO REPLY
         GOTO      REKEY IF NOT EQUAL

         MOVE      "0000" TO IINVNUM
         MOVE      "1" TO IQUAN
         MOVE      ConvChg TO IUPRICE
         MOVE      ConvChg TO IEXTCOST
         MOVE      "Diskette to tape conversions" TO IDESC1
         MOVE      "for credit reporting to CBI." TO IDESC2
         TRAP      BADWRITE IF IO
         WRITE     TIMEFILE,SEQ;TimeRcd

         MOVE      ConvChg TO IUPRICE
         MOVE      ConvChg TO IEXTCOST
         MOVE      "Diskette to tape conversions" TO IDESC1
         MOVE      "for credit reporting to TRW." TO IDESC2
         WRITE     TIMEFILE,SEQ;TimeRcd

         MOVE      " 0.00" TO IUPRICE
         MOVE      " 0.00" TO IEXTCOST
         MOVE      "Diskette to tape conversions" TO IDESC1
         MOVE      "for credit reporting to TU. " TO IDESC2
         WRITE     TIMEFILE,SEQ;TimeRcd

         MOVE      "Shipping & Handling charges." TO IDESC1
         MOVE      "(Tapes sent to CBI-Atlanta) " TO IDESC2
         MOVE      ShipChg TO IUPRICE                /* Unit Price for S&H
         MOVE      ShipChg TO IEXTCOST
         WRITE     TIMEFILE,SEQ;TimeRcd

         MOVE      "Shipping & Handling charges." TO IDESC1
         MOVE      "(Tapes sent to TRW-Allen,TX)" TO IDESC2
         MOVE      ShipChg TO IUPRICE                /* Unit Price for S&H
         MOVE      ShipChg TO IEXTCOST
         WRITE     TIMEFILE,SEQ;TimeRcd

         MOVE      "Shipping & Handling charges." TO IDESC1
         MOVE      "(Tapes sent to TU-Chicago)  " TO IDESC2
         MOVE      ShipChg TO IUPRICE                /* Unit Price for S&H
         MOVE      ShipChg TO IEXTCOST
         WRITE     TIMEFILE,SEQ;TimeRcd

         MOVE      "Special pricing for reports " TO IDESC1
         MOVE      "sent to all 3 credit bureaus" TO IDESC2
         MOVE      "0.00" TO IUPRICE                /*
         MOVE      "0.00" TO IEXTCOST
         WRITE     TIMEFILE,SEQ;TimeRcd
         TRAPCLR   IO

         DISPLAY   *P30:16,IDESC1,*P30:18,IDESC2;
         MOVE      "67.00" TO IEXTCOST
         DISPLAY   *P72:18,IEXTCOST;

         GOTO      LINEITEM

******************************************************************************
CkComp1
         BUMP      IDESC1
         GOTO      KeyD2   IF NOT EOS
         MOVE      "Computer processing charges " TO IDESC1
         MOVE      SPACES                         TO IDESC2
         DISPLAY   *P30:16,IDESC1,*P30:18,IDESC2;
         GOTO      Compute
CkProg1
         BUMP      IDESC1
         GOTO      KeyD2   IF NOT EOS
         MOVE      "Program modification charges" TO IDESC1
         MOVE      SPACES                         TO IDESC2
         DISPLAY   *P30:16,IDESC1,*P30:18,IDESC2;
         GOTO      KeyD2                 /* No,  Continue....    8/14/90

KeyD2
         KEYIN     *IT:
                   *P30:18,*RV,IDESC2,*P30:18,*DV,S28,*P30:18,*DV,IDESC2;
Compute
         KEYIN     *IN,*P61:18,*DV,IUPRICE:
                   *P61:18,*JR,*RV,IUPRICE,*P61:18,*DV,S10,*P61:18,*DV,IUPRICE;
         RESET     IDESC1
         MOVE      IQUAN TO WORK9
         MULT      IUPRICE BY WORK9
         MOVE      WORK9 TO IEXTCOST
         DISPLAY   *P72:18,IEXTCOST;
         MATCH     "T" TO ITAXFLAG               /*  3/31/88
         GOTO      STAX1 IF EQUAL                /*  3/31/88
         MOVE      "N" TO ITAXFLAG               /*  3/31/88
         GOTO      NOSTAX2                       /*     6/04/93
. No-Op  GOTO      STAX2                         /*  3/31/88
STAX1
         MOVE      "Y" TO ITAXFLAG               /*  3/31/88
STAX2
         KEYIN     *P10:23,"Line item Taxable? (Y/N) ":  /* 3/31/88
                   *P35:23,*UC,*RV,ITAXFLAG,*LC;          /* 3/31/88
         MATCH     "N" TO ITAXFLAG               /*  3/31/88
         GOTO      NOSTAX1 IF EQUAL              /*  3/31/88
         DISPLAY   *P35:23,*HON,"Yes",*HOFF,*EL;
         MOVE      "T" TO ITAXFLAG               /*  3/31/88
         GOTO      NOSTAX2
NOSTAX1
         DISPLAY   *P35:23,*HON,"No ",*HOFF,*EL;
         MOVE      " " TO ITAXFLAG               /*  3/31/88
NOSTAX2
         KEYIN     *P10:24,"Screen Correct?  (Y/N) ":
                   *P35:24,*UC,REPLY,*LC;
         CMATCH    "Y" TO REPLY
         GOTO      REKEY IF NOT EQUAL
         MOVE      "0000" TO IINVNUM
         TRAP      BADWRITE IF IO
         WRITE     TIMEFILE,SEQ;TimeRcd
         TRAPCLR   IO
.
. Code added 6/21/95 to create the corresponding S&H record
.
         CMATCH    " " TO Bureau
         GOTO      LineItem IF EOS
         GOTO      LineItem IF EQUAL
CkCBI
         CMATCH    "C" TO Bureau
         GOTO      CkTRW IF NOT EQUAL

         MOVE      "Shipping & Handling charges." TO IDESC1
         MOVE      "(Tapes sent to CBI-Atlanta) " TO IDESC2
         MOVE      ShipChg TO IUPRICE                /* Unit Price for S&H
         MOVE      ShipChg TO IEXTCOST
         WRITE     TIMEFILE,SEQ;TimeRcd
         GOTO      LineItem

CkTRW
         CMATCH    "T" TO Bureau
         GOTO      CkTU  IF NOT EQUAL

         MOVE      "Shipping & Handling charges." TO IDESC1
         MOVE      "(Tapes sent to TRW-Allen,TX)" TO IDESC2
         MOVE      ShipChg TO IUPRICE                /* Unit Price for S&H
         MOVE      ShipChg TO IEXTCOST
         WRITE     TIMEFILE,SEQ;TimeRcd
         GOTO      LineItem

CkTU
         CMATCH    "U" TO Bureau
         GOTO      LineItem IF NOT EQUAL

         MOVE      "Shipping & Handling charges." TO IDESC1
         MOVE      "(Tapes sent to TU-Chicago ) " TO IDESC2
         MOVE      ShipChg TO IUPRICE                /* Unit Price for S&H
         MOVE      ShipChg TO IEXTCOST
         WRITE     TIMEFILE,SEQ;TimeRcd
         GOTO      LineItem

.
RANGETRP KEYIN     *P10:24,*EL,"RANGE Error on 'CUSTMAST' ":
                   *B,*CL,REPLY;
         CMATCH    "Y" TO REPLY
         GOTO      KEYINAR IF EQUAL
         GOTO      EOJ
EOJ      WEOF      TIMEFILE,SEQ
         CLOSE     TIMEFILE
         CHAIN     "TJONMENU"         GO BACK TO MENU      12/5...JMM
.
.
.
NOARMAST KEYIN     *P10:24,*EL," 'CUSTMAST' File cannot be opened. ":
                   *B,*CL,REPLY;
         STOP
.
.
.
NOINVMST PREP      TIMEFILE,"TIMEFILE"
         TRAPCLR   IO
         NORETURN
         GOTO      START
.
.
.
BADWRITE KEYIN     *P10:24,*EL,"Error writing to 'TIMEFILE' ":
                   *B,*CL,REPLY;
         STOP
.
.
WRITENUL MOVE      SPACES TO IDESC1
         MOVE      SPACES TO IDESC2
         MOVE      "0.0" TO IQUAN
         MOVE      "0.0" TO IUPRICE
         MOVE      "0.0" TO IEXTCOST
         MOVE      SPACES TO ITAXFLAG
         MOVE      SPACES TO ITYPE
         MOVE      "00000000" TO IWRKDATE
         MOVE      "000" TO ICUST
         MOVE      "0000" TO IWO
         MOVE      "0000" TO IWOSAVE
         WRITE     TIMEFILE,SEQ;TimeRcd
         RETURN
