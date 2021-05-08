. ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
. บ  INVOICE     Special program to read CSCFILES.DAT and create TIMEFILE.TXT  บ
. บ  V.M=1.6     Invoicing for conversion of credit reporting diskettes.       บ
. บ                                                                            บ
. ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
.
. ษออออออออออัอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
. บ  Date    ณ                 Modification                                    บ
. วฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
. บ  7/21/95 ณ Created from TJONINV2 Ver 2.2                                   บ
. บ 12/18/95 ณ Instructions to clarify which processing month will be used.    บ
. บ          ณ V1.1   JMM                                                      บ
. บ  1/23/96 ณ Instructions were wrong... now they should be correct.          บ
. บ          ณ Semi edit the EOM date.                                         บ
. บ          ณ                                                                 บ
. บ  3/19/96 ณ V1.2 correct prblm when CTmmddyy.bur records are encountered in บ
. บ          ณ in the CSCFILES.DAT file.                                       บ
. บ          ณ                                                                 บ
. บ 11/13/96 ณ V1.3 Added code to pick up last months MM/YY billing record and บ
. บ          ณ use it as the default for this process.                         บ
. บ          ณ                                                                 บ
. บ 07/23/97 ณ V1.4 Special pricing for Auto Credit of Tampa & Jacksonville    บ
. บ          ณ If CBI & TU, then TU is $15 plus S&H...(MM)                     บ
. บ          ณ                                                                 บ
. บ 01/11/98 ณ V1.5 Added routine to create another file that has the TDS      บ
. บ          ณ invoice number and the number of transactions (distributions)   บ
. บ          ณ that are in TIMEFILE for each invoice.  The purpose is so that  บ
. บ          ณ TJONINV3 program can create a required field with the number    บ
. บ          ณ of distributions in the TDSSALES.CSV file. This file is used    บ
. บ          ณ by PeachTree First Accounting during the invoice/sales import.  บ
. บ          ณ                                                                 บ
. บ 02/01/98 ณ Added code to get company name from CUSTMAST Key=000            บ
. บ          ณ                                                                 บ
. บ 09/27/98 ณ V1.6 Special pricing for Sound Ford, Renton, Washington         บ
. บ          ณ If all 3 then price is for only 1      (MM)                     บ
. บ          ณ                                                                 บ
. บ 11/30/98 ณ Fix bug in V1.6 Sound Ford section....... (MM)                  บ
. บ          ณ and bug causing loss of last bureau in CSCFILES.DAT             บ
. บ          ณ                                                                 บ
. ศออออออออออฯอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ

         INCLUDE   COMMON.INC
VERSION  INIT      "INVOICE  V.M=1.6"

CustMast IFILE     VARIABLE=1024,WEOF
CustFNi  IFILE     VARIABLE=1024,WEOF
CMKey    DIM       3
         INCLUDE   TJONCMDS.INC

CustFNk  DIM       8             ;; Unique Customer Filename
CFNSaveP DIM       8             ;; Save area for Prior   Customer filename
CFNSaveC DIM       8             ;; Save area for Current Customer filename
CompName DIM       25            ;; Company Name from CUSTMAST Key=000

CSCFILE  IFILE

InvDist  IFILE     VARIABLE=256,WEOF,NODUP
InvDisFN INIT      "INVDIST"      ;; Invoice distribution file

IDKey    DIM       4              ;; TDS Invoice number
IDNbr    FORM      2              ;; Number of distributions

CSCFLKEY INIT      0x0E,"Filename Ex"         ;; filename is 12 bytes (Key)
CSCLRKEY INIT      0xFB,0x0F,"          "     ;; Last record         (Key)
CSCFN    DIM       30
CFN      DIM       8              ;;       filename 8 bytes
CExt     DIM       3              ;;       extension
CSize    DIM       8              ;;       size
CCDate   DIM       8              ;;       creation date
CCTime   DIM       5              ;;       creation time
CCAMPM   DIM       1              ;;       AM or PM (a|p)
CRecords FORM      6              ;;       Computed record count
CAsOfDt  DIM       8              ;;       As Of date from header record
CRunDate DIM       8              ;;       TJON Run date
CCustH   DIM       10             ;;       Customer name in HEADER record
CProgNm  DIM       18             ;;       ACRSnnn  program name
CTapeVol DIM       6              ;;       Tape volume number
CFill1   DIM       1              ;;   1 byte filler
CFill2   DIM       2              ;;   2 byte filler following Extension
FirstCFN INIT      "Y"            ;;   Switch for handling TOF
SndCompl INIT      "N"            ;;   Flag for Sound Ford Complete
GotEOF   INIT      "N"            ;;   Flag for EOF on CSCData file

CSCData  VARLIST   CFN,CFill1,CExt,CFill2,CSize:
                   CFill2,CCDate,CFill1,CRecords:
                   CFill1,CRunDate:
                   CFill2,CCustH,CFill2,CAsOfDt:
                   CFill1,CProgNm:
                   CTapeVol

TIMEFILE FILE
         INCLUDE   TIMERCDS.INC

CtrlFile IFILE
LastBMo  INIT      "LAST BILL MO"
NextINbr INIT      "NEXT INVOICE"            ;; 1/11/98
IDInvNbr FORM      4                         ;; 1/11/98

. LAST BILL MO MM YY xxxx yyyy $$$$$.$$

LastBMM  FORM      2
LastBYY  FORM      2
LastSIN  FORM      4
LastEIN  FORM      4
LastAmt  FORM      5.2
MMWork   DIM       2
YYWork   DIM       2
BillWork DIM       5

Program  DIM       80
IWOSAVE  INIT      "    "
INVNUM   FORM      4
Seq      FORM      "-1"
Reply    INIT      "N"           ;; Leave this default 'N'.... 6/11/90... [JMM]
FirstSW  INIT      "Y"           ;; Y=First time switch
Bureau3  INIT      " "           ;; Save area for bureau (C,T,U)
BureauC  INIT      " "           ;; Save area for bureau (C,T,U)
BureauT  INIT      " "           ;; Save area for bureau (C,T,U)
BureauU  INIT      " "           ;; Save area for bureau (C,T,U)
ShipChg  INIT      "3.00"        ;; Unit Price for S&H   6/21/95
ConvChg  INIT      "29.00"       ;; Unit Price for Conv  6/21/95
ACOFTU$  INIT      "15.00"       ;; Auto Credit of Florida spcl pricing 7/23
Sound3   INIT      "29.00"       ;; Sound Ford spcl pricing 9/27/98
WORK9    FORM      7.2
IWDSAVE  FORM      6
DATE     DIM       6
DISPDATE DIM       8
DISPTR   FORM      2.2
MMW      DIM       2
DDW      DIM       2
YYW      DIM       2
MMNumber FORM      2
DDNumber FORM      2
YYNumber FORM      2
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

         TRAP      NOCFI IF IO
         OPEN      CustFNi,"CustFNi"
         TRAPCLR   IO

         TRAP      NOCFI IF IO
         OPEN      CustMast,"CUSTMAST"
         MOVE      "000" TO CMKey
         READ      CustMast,CMKey;CustRcd
         MOVE      CustName TO CompName
         TRAPCLR   IO

         ENDSET    CompName
         LENSET    CompName
CNLoop1
         CMATCH    " " TO CompName
         GOTO      CNLoop2 IF NOT EQUAL
         BUMP      CompName BY -1
         GOTO      CNLoop1
CNLoop2
         LENSET    CompName
         RESET     CompName

         TRAP      NOCTL IF IO
         OPEN      CtrlFile,"CTRLFILE"
         TRAPCLR   IO
         READ      CtrlFile,LastBMO;LastBMO,Reply:
                                    LastBMM,Reply:
                                    LastBYY;
         GOTO      NoCtlRec IF OVER

. Now get next invoice number
         READ      CtrlFile,NextINbr;NextINbr,IDInvNbr
         GOTO      NoCtlRec IF OVER

         TRAP      NoInvDis IF IO                     ;; 1/11/98
         OPEN      InvDist,InvDisFN                   ;; 1/11/98

         TRAP      NoInvMst IF IO
         OPEN      TIMEFILE,"TIMEFILE"
         POSITEOF  TIMEFILE                          /*  5/17/88...JMM

START    DISPLAY   *COLOR 23,*ES:
                   *P01:01,Version:
                   *P30:01,"    ",*LL,CompName:
                   *P20:03,"Automatic Diskette Conversion Invoicing Program"

. Bump to next Month (and Year if last was December)

         ADD       "1" TO LastBMM
         COMPARE   "13" TO LastBMM
         GOTO      NotDec IF NOT EQUAL
         MOVE      "01" TO LastBMM
         ADD       "1" TO LastBYY
NotDec
         MOVE      LastBMM TO MMWork
         REPLACE   " 0" IN MMWork
         MOVE      LastBYY TO YYWORK
         REPLACE   " 0" IN YYWork
         PACK      BillWork FROM MMWork, "/", YYWork
         MOVE      MMWork TO MMW
         MOVE      YYWork TO YYW
         MOVE      "30"   TO DDW
KEYDATE  KEYIN   *COLOR 23,*P10:5,"Enter EOM Accounting Date ":
                 *COLOR 18:
                 *P06:07,"This date is Work Performed date for ":
                 "the reporting data on diskette.":
                 *P14:08,"Example: Work performed  in October.........":
                 *P14:09,"         Customer As-of-Date is September...":
                 *P14:10,"         EOM Accounting Date = 10/96        ":
                 *P14:11,"         Program will use D:\complete\cscfiles.M10":
                 *P14:08,*COLOR 20,"Example:",*P45:10,"10",*P62:11,"10":
                 *COLOR 23:
                 *P36:05,*DV,BillWork:
                 *COLOR 19:
                 *RV,*DE,*JR,*ZF,*+,*P36:05,MMW;
         MOVE      MMW TO MMWork
         REPLACE   " 0" IN MMWork
         MATCH     "99" TO MMWork
         GOTO      Exit IF EQUAL
         KEYIN     *P36:05,*DV,MMWork,"/",*-,*RV,*DE,*JR,*ZF,YYW;
         MOVE      YYW TO YYWork
         REPLACE   " 0" IN YYWork
         DISPLAY   *P38:05,YYWork;
         CMATCH    " " TO MMW
         GOTO      KEYDATE IF EOS
         TYPE      MMW
         GOTO      KeyDate IF NOT EQUAL
         TYPE      YYW
         GOTO      KeyDate IF NOT EQUAL
         MOVE      MMW TO MMNumber
         MOVE      DDW TO DDNumber
         MOVE      YYW TO YYNumber

         COMPARE   "13" TO MMNumber
         GOTO      KeyDate IF NOT LESS
         COMPARE   "00" TO MMNumber
         GOTO      KeyDate IF EQUAL
         COMPARE   "96" TO YYNumber
         GOTO      KeyDate IF LESS
         MOVE      "30" TO DDW                    ;; Just go ahead and force
         PACK      DATE FROM MMW, DDW, YYW
         MOVE      DATE TO IWRKDATE
         MOVE      DATE TO IWDSAVE
         PACK      DISPDATE FROM MMW, "/", DDW, "/", YYW
KEYINAR  DISPLAY   *COLOR 19,*P36:05,DISPDATE,*COLOR 23,*EF;
         CLEAR     CSCFN
         APPEND    "D:\COMPLETE\CSCFILES." TO CSCFN
         APPEND    MMW TO CSCFN
         APPEND    "I" TO CSCFN
         LENSET    CSCFN
         RESET     CSCFN
. No-Op KEYIN       *N,"Opening CSCFN=",*DV,CSCFN,Reply;
         TRAP      NOCSCFL IF IO
         OPEN      CSCFILE,CSCFN
         TRAPCLR   IO

* Get the customer name from the CSCFILES.DAT file and then use
* CUSTMAST file's secondary index to get the customer number.

ResetBur
         MOVE      Spaces TO BureauC          ;; Clear bureau flags
         MOVE      Spaces TO BureauT          ;; Clear bureau flags
         MOVE      Spaces TO BureauU          ;; Clear bureau flags
         MOVE      Spaces TO Bureau3          ;; Clear bureau flags

ReadCSC
         READKS    CSCFILE;CFN,CFill1,CExt
         GOTO      EOJ IF OVER

. No-Op KEYIN       *N,"ReadKS CSCFILES.DAT Record = ",*DV,CFN,Reply;
         MATCH     CFN TO CSCFLKey            ;; Special record in file?
         GOTO      ResetBur IF EQUAL          ;; Yes, skip
         MATCH     CFN TO CSCLRKey            ;; Special 'Last record' in file?
         GOTO      ReadCSC1 IF NOT EQUAL      ;; No, continue
. Special last record inf CSCFILES.DAT processing
        KEYIN       *N,"Found Special Last Record",*DV,CFN,Reply;
         GOTO      EOF                        ;; Signal Last customer


ReadCSC1
         MOVE      CFN TO CFNSaveC            ;; Save Current Cust ID
. No-op  KEYIN     *N,"Checking FirstCFN= ",*DV,FirstCFN,Reply;
         CMATCH    "N" TO FirstCFN            ;; Is this TOF?
         GOTO      CkSameCF IF EQUAL          ;; No, Check same cust
         MOVE      CFN TO CFNSaveP            ;; Prior & Current are same
         MOVE      CFN TO CFNSaveC            ;; Prior & Current are same
         CMOVE     "N" TO FirstCFN            ;; Reset first time switch
CkSameCF
        KEYIN       *N,"Matching CFN=",*DV,CFN," to CFNSaveP=":
                   *DV,CFNSaveP,Reply;
         MATCH     CFN TO CFNSaveP            ;; Same cust?
         GOTO      ChkAll3 IF NOT EQUAL       ;; No, then process

. Continue here after creating the TIMEFILE records for this customer
CkBurC
         MATCH     "CBI" TO CExt              ;; Is this a CBI tape?
         GOTO      CkBurT IF NOT EQUAL
         CMOVE     "C" TO BureauC
         GOTO      CkBurEx                    ;; Exit Check Bureau section
CkBurT
         MATCH     "TRW" TO CExt              ;; Is this a TRW tape?
         GOTO      CkBurU IF NOT EQUAL
         CMOVE     "T" TO BureauT
         GOTO      CkBurEx                    ;; Exit Check Bureau section
CkBurU
         MATCH     "TU " TO CExt              ;; Is this a TU  tape?
         GOTO      CkBurEx IF NOT EQUAL
         CMOVE     "U" TO BureauU
. Fall Through to CkBurEx
CkBurEx
         GOTO      ReadCSC                    ;; Go read another record


.
. When this section is entered, we have completed reading all detail
. records for a unique customer filename, queried each for specific bureaus,
. and now we will flag the customer for special pricing if all 3 bureaus
. are reported.
. There is also special pricing for Auto Credit of Florida (#262 & #289) 7/22
. There is special pricing for Sound Ford (#280) 10/31/98
.

ChkAll3

. No-Op KEYIN       *N,"Checking for All 3 ",Reply;
         MATCH     " " TO BureauC             ;; If any flag is not set
         GOTO      GetCMRec IF EQUAL          ;;  then don't give the
         MATCH     " " TO BureauT             ;;  special billing rate
         GOTO      GetCMRec IF EQUAL          ;;  for all 3 bureaus.
         MATCH     " " TO BureauU             ;;
         GOTO      GetCMRec IF EQUAL          ;;
         CMOVE     "3" TO Bureau3             ;; Set all 3 bureaus flag

.
. Now get corresponding record in CUSTMAST
.
GetCMRec
         MOVE      CFNSaveP TO CustFNk
         TRAP      RANGETRP IF RANGE
         MATCH     "Y" TO GotEOF               ;; At EOF on CSCData?  11/30
         GOTO      ReadCN1 IF NOT EQUAL        ;; No, continue        11/30
        KEYIN       *N,"GetCMRec w/EOF,BureauC=",*DV,BureauC:
                   ", BureauT=",*DV,BureauT:
                   ", BureauU=",*DV,BureauU:
                   ", Bureau3=",*DV,Bureau3:
                   Reply;
. At EOF, must use saved customer number
         MOVE      CFNSaveC TO CustFNk
ReadCN1
        KEYIN       *N,"Reading CustFNi Key=",*DV,CustFNk:
                    "; ",*DV,CFNSaveC,Reply;
         READ      CustFNi,CustFNk;CustRcd
         GOTO      DispCust IF NOT OVER
         KEYIN     *B,*COLOR 124:
                   *P35:06," อออ Customer ",*DV,CustFNk:
                   " is not on file. ",*COLOR 23,Reply;
         MOVE      CFNSaveC TO CFNSaveP        ;; Save the prior cust filename
         MOVE      CFN      TO CFNSaveC        ;; Now use the Current filename
         GOTO      ResetBur                    ;; 3/19/96 Bug fix (V1.2)

DispCust
         MOVE      CustNum TO ICust
         TRAPCLR   RANGE
         DISPLAY   *COLOR 19,*EF:
                   *P10:8,CustName:
                   *P10:9,CustAddr:
                   *P10:10,CustCity,*P23:10,"  ",*P25:10,CustSte:
                   *P27:10,"  ",*P29:10,CustZip:
                   *P10:11,CustAttn:
                   *P50:08,*HON," C = Diskette + S&H for CBI ":
                   *P50:09,*HON," T = Diskette + S&H for TRW ":
                   *P50:10,*HON," U = Diskette + S&H for TU  ":
                   *P50:11,*HON," 3 = All 3 Bureaus price    ":
                   *HOFF;
         CALC      DISPTR=(TAXRATE*100)
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
         MOVE      IWOSAVE TO IWO

ChgCust
         MATCH     "Y" TO FirstSW     ;; Is this the initial entry?
         GOTO      BPWrNull IF EQUAL  ;; Yes, Skip initial null record
         CALL      WRITENUL   ;;  Write a null record to change customers
BPWrNull
         MOVE      "N" TO FirstSW     ;; Reset switch
         MOVE      IWDSAVE TO IWRKDATE
         MOVE      IWO TO IWOSAVE
         MOVE      "1" TO IQUAN          /* 6/21/95
         MOVE      CustNum TO ICust

*
* Build the correct TIMEFILE.TXT billing records
*
         MATCH     "280" TO ICust          ;; Sound Ford?        10/31/98
         GOTO      SoundChg IF EQUAL       ;; Yes, special price
         MATCH     "3" TO Bureau3              ;; Is this a special billing?
         GOTO      Spcl3 IF EQUAL              ;; Yes, create TIMEFILE records
         MATCH     "C" TO BureauC              ;; Is this a CBI billing record?
         CALL      BillCBI IF EQUAL            ;; Yes, create TIMEFILE records
         MATCH     "T" TO BureauT              ;; Is this a TRW billing record?
         CALL      BillTRW IF EQUAL            ;; Yes, create TIMEFILE records
         MATCH     "U" TO BureauU              ;; Is this a TU  billing record?
         CALL      BillTU  IF EQUAL            ;; Yes, create TIMEFILE records
         MOVE      CFNSaveC TO CFNSaveP        ;; Save the prior cust filename
         MOVE      CFN      TO CFNSaveC        ;; Now use the Current filename
         MOVE      Spaces TO BureauC          ;; Clear bureau flags
         MOVE      Spaces TO BureauT          ;; Clear bureau flags
         MOVE      Spaces TO BureauU          ;; Clear bureau flags
         MOVE      Spaces TO Bureau3          ;; Clear bureau flags
. No-Op KEYIN         *N,"After write cycle. SaveP=",*DV,CFNSaveP:
. No-Op            ", SaveC=",*DV,CFNSaveC:
. No-Op            ", CFN=",*DV,CFN:
. No-Op            Reply;
         GOTO      CkBurC


******************************************************************************
BillCBI
         MOVE      "0000" TO IINVNUM
         MOVE      "1" TO IQUAN
         MOVE      ConvChg TO IUPRICE
         MOVE      ConvChg TO IEXTCOST
         MOVE      "Diskette to tape conversions" TO IDESC1
         MOVE      "for credit reporting to CBI." TO IDESC2
         TRAP      BADWRITE IF IO
. No-Op KEYIN       *N,"Writing TIMEFILE CBI Conv  CustNum=",*DV,CustNum,Reply;
         WRITE     TIMEFILE,SEQ;TimeRcd

         ADD       "1" TO IDNbr                     ;; Bump Nbr of Distributions
         MOVE      "0000" TO IINVNUM
         MOVE      "1" TO IQUAN
         MOVE      "Shipping & Handling charges." TO IDESC1
         MOVE      "(Tapes sent to CBI-Atlanta) " TO IDESC2
         MOVE      ShipChg TO IUPRICE                /* Unit Price for S&H
         MOVE      ShipChg TO IEXTCOST
. No-Op KEYIN       *N,"Writing TIMEFILE CBI S&H ",Reply;
         WRITE     TIMEFILE,SEQ;TimeRcd
         ADD       "1" TO IDNbr                     ;; Bump Nbr of Distributions
         TRAPCLR   IO

         MOVE      "0.00" TO IEXTCOST
         RETURN                       ;; Return to calling location

******************************************************************************
BillTRW
         MOVE      "0000" TO IINVNUM
         MOVE      "1" TO IQUAN
         MOVE      ConvChg TO IUPRICE
         MOVE      ConvChg TO IEXTCOST
         MOVE      "Diskette to tape conversions" TO IDESC1
         MOVE      "for credit reporting to TRW." TO IDESC2
         TRAP      BADWRITE IF IO
. No-Op KEYIN       *N,"Writing TIMEFILE TRW Conv  CustNum=",*DV,CustNum,Reply;
         WRITE     TIMEFILE,SEQ;TimeRcd

         ADD       "1" TO IDNbr                     ;; Bump Nbr of Distributions
         MOVE      "0000" TO IINVNUM
         MOVE      "1" TO IQUAN
         MOVE      "Shipping & Handling charges." TO IDESC1
         MOVE      "(Tapes sent to TRW-Allen,TX)" TO IDESC2
         MOVE      ShipChg TO IUPRICE                /* Unit Price for S&H
         MOVE      ShipChg TO IEXTCOST
. No-Op KEYIN       *N,"Writing TIMEFILE TRW S&H ",Reply;
         WRITE     TIMEFILE,SEQ;TimeRcd
         TRAPCLR   IO

         ADD       "1" TO IDNbr                     ;; Bump Nbr of Distributions
         MOVE      "0.00" TO IEXTCOST
         RETURN                       ;; Return to calling location

******************************************************************************
BillTU
         MOVE      "0000" TO IINVNUM
         MOVE      "1" TO IQUAN
         MOVE      ConvChg TO IUPRICE
         MOVE      ConvChg TO IEXTCOST
         MOVE      "Diskette to tape conversions" TO IDESC1
         MOVE      "for credit reporting to TU. " TO IDESC2

. Special pricing for Auto Credit of Florida/Tampa & Jacksonville (#262 & #289)
         MATCH     "262" TO ICust          ;; Jax?
         GOTO      ACOFChg IF EQUAL        ;; Yes, special price
         MATCH     "289" TO ICust          ;; Tampa?
         GOTO      ACOFChg IF EQUAL        ;; Yes, special price

         GOTO      PriceOK                 ;; No,  price is OK

ACOFChg
         MOVE      ACOFTU$ TO IUPrice      ;; Yes, price is special
         MOVE      ACOFTU$ TO IExtCost     ;; Yes, price is special
         MOVE      "AutoCredit of Florida charge" TO IDESC1
         MOVE      "for credit reporting to TU. " TO IDESC2
         GOTO      PriceOK

PriceOK
         TRAP      BADWRITE IF IO
. No-Op KEYIN       *N,"Writing TIMEFILE TU  Conv  CustNum=",*DV,CustNum,Reply;
         WRITE     TIMEFILE,SEQ;TimeRcd

         ADD       "1" TO IDNbr                     ;; Bump Nbr of Distributions
         MOVE      "0000" TO IINVNUM
         MOVE      "1" TO IQUAN
         MOVE      "Shipping & Handling charges." TO IDESC1
         MOVE      "(Tapes sent to TU-Chicago)  " TO IDESC2
         MOVE      ShipChg TO IUPRICE                /* Unit Price for S&H
         MOVE      ShipChg TO IEXTCOST
. No-Op KEYIN       *N,"Writing TIMEFILE TU  S&H ",Reply;
         WRITE     TIMEFILE,SEQ;TimeRcd
         TRAPCLR   IO

         ADD       "1" TO IDNbr                     ;; Bump Nbr of Distributions
         MOVE      "0.00" TO IEXTCOST
         RETURN                       ;; Return to calling location

******************************************************************************
. Special pricing for Sound Ford...   Only 1 charge for all 3 bureaus

SoundChg
        KEYIN       *N,"Entered SoundChg Routine",Reply;
. No-Op  MOVE      "Y" TO FirstSW      ;; Reset switch needed for Sound 11/30
         MATCH     "Y" TO SndCompl     ;; Is Sound Ford already complete?
         GOTO      SndExit IF EQUAL    ;; Yes, Exit this section

        KEYIN       *N,"Writing TIMEFILE Rcds for Spcl Sound Ford",Reply;

         MOVE      "0000" TO IINVNUM
         MOVE      "1" TO IQUAN
         MOVE      Sound3  TO IUPrice      ;; Yes, price is special
         MOVE      Sound3  TO IExtCost     ;; Yes, price is special
         MOVE      "Sound Ford conversion charge" TO IDesc1
         MOVE      "for credit reporting.       " TO IDesc2
         TRAP      BADWRITE IF IO
         WRITE     TIMEFILE,SEQ;TimeRcd

         ADD       "1" TO IDNbr                     ;; Bump Nbr of Distributions
         MOVE      " 0.00" TO IUPRICE       ;; 11/30/98 No Charge for TRW & TU
         MOVE      " 0.00" TO IEXTCOST
         MOVE      "Diskette to tape conversions" TO IDESC1
         MOVE      "for credit reporting to TRW." TO IDESC2
         WRITE     TIMEFILE,SEQ;TimeRcd

         ADD       "1" TO IDNbr                     ;; Bump Nbr of Distributions
         MOVE      " 0.00" TO IUPRICE       ;; 11/30/98 No Charge for TRW & TU
         MOVE      " 0.00" TO IEXTCOST
         MOVE      "Diskette to tape conversions" TO IDESC1
         MOVE      "for credit reporting to TU. " TO IDESC2
         WRITE     TIMEFILE,SEQ;TimeRcd

         ADD       "1" TO IDNbr                     ;; Bump Nbr of Distributions
         MOVE      "Shipping & Handling charges." TO IDESC1
         MOVE      "(Tapes sent to all 3 bureaus" TO IDESC2
         MOVE      ShipChg TO IUPRICE                /* Unit Price for S&H
         MOVE      ShipChg TO IEXTCOST
         WRITE     TIMEFILE,SEQ;TimeRcd


         ADD       "1" TO IDNbr                     ;; Bump Nbr of Distributions
         MOVE      "Special pricing for reports " TO IDESC1
         MOVE      "sent to all 3 credit bureaus" TO IDESC2
         MOVE      "0.00" TO IUPRICE                /*
         MOVE      "0.00" TO IEXTCOST
         WRITE     TIMEFILE,SEQ;TimeRcd
         TRAPCLR   IO

         ADD       "1" TO IDNbr                     ;; Bump Nbr of Distributions
         DISPLAY   *P30:16,IDESC1,*P30:18,IDESC2;
         MOVE      "32.00" TO IEXTCOST

. No-Op  MOVE      CFNSaveC TO CFNSaveP
. No-Op  MOVE      CFN      TO CFNSaveC
         MOVE      Spaces TO BureauC          ;; Clear bureau flags
         MOVE      Spaces TO BureauT          ;; Clear bureau flags
         MOVE      Spaces TO BureauU          ;; Clear bureau flags
         MOVE      Spaces TO Bureau3          ;; Clear bureau flags

SndExit
         MOVE      "Y" TO SndCompl      ;; 11/30/98 Sound is complete
         GOTO      ResetBur             ;; Go process another Customer


******************************************************************************


Spcl3
. No-Op KEYIN       *N,"Writing TIMEFILE Rcds for all 3 bureaus",Reply;
         MOVE      "Special pricing for reports " TO IDESC1
         MOVE      "sent to all 3 credit bureaus" TO IDESC2
         MOVE      "67.00" TO IEXTCOST

         MOVE      "0000" TO IINVNUM
         MOVE      "1" TO IQUAN
         MOVE      ConvChg TO IUPRICE
         MOVE      ConvChg TO IEXTCOST
         MOVE      "Diskette to tape conversions" TO IDESC1
         MOVE      "for credit reporting to CBI." TO IDESC2
         TRAP      BADWRITE IF IO
         WRITE     TIMEFILE,SEQ;TimeRcd

         ADD       "1" TO IDNbr                     ;; Bump Nbr of Distributions
         MOVE      ConvChg TO IUPRICE
         MOVE      ConvChg TO IEXTCOST
         MOVE      "Diskette to tape conversions" TO IDESC1
         MOVE      "for credit reporting to TRW." TO IDESC2
         WRITE     TIMEFILE,SEQ;TimeRcd

         ADD       "1" TO IDNbr                     ;; Bump Nbr of Distributions
         MOVE      " 0.00" TO IUPRICE
         MOVE      " 0.00" TO IEXTCOST
         MOVE      "Diskette to tape conversions" TO IDESC1
         MOVE      "for credit reporting to TU. " TO IDESC2
         WRITE     TIMEFILE,SEQ;TimeRcd

         ADD       "1" TO IDNbr                     ;; Bump Nbr of Distributions
         MOVE      "Shipping & Handling charges." TO IDESC1
         MOVE      "(Tapes sent to CBI-Atlanta) " TO IDESC2
         MOVE      ShipChg TO IUPRICE                /* Unit Price for S&H
         MOVE      ShipChg TO IEXTCOST
         WRITE     TIMEFILE,SEQ;TimeRcd

         ADD       "1" TO IDNbr                     ;; Bump Nbr of Distributions
         MOVE      "Shipping & Handling charges." TO IDESC1
         MOVE      "(Tapes sent to TRW-Allen,TX)" TO IDESC2
         MOVE      ShipChg TO IUPRICE                /* Unit Price for S&H
         MOVE      ShipChg TO IEXTCOST
         WRITE     TIMEFILE,SEQ;TimeRcd

         ADD       "1" TO IDNbr                     ;; Bump Nbr of Distributions
         MOVE      "Shipping & Handling charges." TO IDESC1
         MOVE      "(Tapes sent to TU-Chicago)  " TO IDESC2
         MOVE      ShipChg TO IUPRICE                /* Unit Price for S&H
         MOVE      ShipChg TO IEXTCOST
         WRITE     TIMEFILE,SEQ;TimeRcd

         ADD       "1" TO IDNbr                     ;; Bump Nbr of Distributions
         MOVE      "Special pricing for reports " TO IDESC1
         MOVE      "sent to all 3 credit bureaus" TO IDESC2
         MOVE      "0.00" TO IUPRICE                /*
         MOVE      "0.00" TO IEXTCOST
         WRITE     TIMEFILE,SEQ;TimeRcd
         TRAPCLR   IO

         ADD       "1" TO IDNbr                     ;; Bump Nbr of Distributions
         DISPLAY   *P30:16,IDESC1,*P30:18,IDESC2;
         MOVE      "67.00" TO IEXTCOST

         MOVE      CFNSaveC TO CFNSaveP
         MOVE      CFN      TO CFNSaveC
         MOVE      Spaces TO BureauC          ;; Clear bureau flags
         MOVE      Spaces TO BureauT          ;; Clear bureau flags
         MOVE      Spaces TO BureauU          ;; Clear bureau flags
         MOVE      Spaces TO Bureau3          ;; Clear bureau flags
         GOTO      CkBurC             ;; Go process another Customer

******************************************************************************


.
RANGETRP
         KEYIN     *B,*COLOR 124:
                   *P10:24,*EL,"RANGE Error on 'CUSTMAST' ":
                   *CL,REPLY;
         CMATCH    "Y" TO REPLY
         GOTO      KEYINAR IF EQUAL
         GOTO      EOJ

EOJ
         MATCH     "N" TO GotEOF              ;; Are we at EOJ?
         GOTO      EOF IF EQUAL               ;; No, set EOF flag & process last
         CALL      WRITENUL   ;;  Write a null record to change customers
         WEOF      TIMEFILE,SEQ
         CLOSE     TIMEFILE
         CLOSE     InvDist                    ;; 1/11/98
         CLEAR     Program
         APPEND    "TJONINV3 " TO Program
         APPEND    MMW TO Program
         APPEND    " " TO Program
         APPEND    DispDate TO Program
         APPEND    Spaces TO Program
         LENSET    Program
         RESET     Program
. No-Op Keyin  *N,"About to execute ",*DV,Program,Reply;
         EXECUTE   PROGRAM                    ;; Go spool the invoices
Exit
         STOP
.
EOF
       KEYIN       *N,"At EOF Routine, Going back to ChkAll3. ",Reply;
         MOVE      "Y" TO GotEOF              ;; Set the EOJ switch
         MOVE      CFNSaveC TO CFNSaveP       ;; Get correct Customer
         MOVE      CFNSaveC TO CFN            ;; Get correct Customer
         GOTO      ResetBur                   ;;    and process last customer
. No-Op  GOTO      ChkAll3                    ;;    and process last customer
.
.
NoCFi
         KEYIN     *B,*COLOR 124:
                   *P10:24,*EL," 'CustFNi ' File cannot be opened. ":
                   *CL,REPLY;
         STOP
.
NoCTL
         KEYIN     *B,*COLOR 124:
                   *P10:24,*EL," 'CTRLFILE' File cannot be opened. ":
                   *CL,REPLY;
         STOP
.
NoCTLRec
         KEYIN     *B,*COLOR 124:
                   *P10:24,*EL," 'CTRLFILE' Record cannot be found.":
                   *CL,REPLY;
         STOP
.
NoCSCFL
         KEYIN     *B,*COLOR 124:
                   *P10:24,*EL,*DV,CSCFN," อออ  File cannot be opened. ":
                   *CL,REPLY;
         STOP
.
.
NoInvDis
         KEYIN     *B,*COLOR 124:
                   *P10:24,*EL,*DV,InvDisFN," อออ  File cannot be opened. ":
                   *CL,REPLY;
         STOP
.
.
IDProb1
         KEYIN     *B,*COLOR 124:
                   *P10:24,*EL,*DV,InvDisFN," อออ  Some kind of file prob1":
                   *CL,REPLY;
         STOP
.
IDProb2
         KEYIN     *B,*COLOR 124:
                   *P10:24,*EL,*DV,InvDisFN," อออ  Some kind of file prob2":
                   *CL,REPLY;
         STOP
.
.
.
NoInvMst
         TRAPCLR   IO
         PREP      TIMEFILE,"TIMEFILE"
         NORETURN
         GOTO      START
.
.
.
BADWRITE
         KEYIN     *P10:24,*EL,"Error writing to 'TIMEFILE' ":
                   *B,*CL,REPLY;
         STOP
.
.
WriteNul
. No-Op KEYIN       *N,"Writing Null record to TIMEFILE ",Reply;
         MOVE      SPACES TO IDESC1
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

         TRAP      DupeInv IF IO
         MOVE      IDInvNbr TO IDKey           ;; 1/11/98  PeachTree Import
. Debug Code Follows ........................................................
         KEYIN     *P1:25,"Update InvDist Record. ":
                   "  IDKey=",*DV,IDKey:
                   "  IDNbr=",*DV,IDNbr:
                   *EL,Reply;
. ...........................................................................
         WRITE     InvDist,IDKey;IDKey,IDNbr            ;; 1/11/98
         GOTO      WrtNulEx

DupeInv
         TRAPCLR   IO
         NORETURN
         TRAP      IDProb1 IF IO
         READ      InvDist,IDKey;IDKey                  ;; 1/11/98
         TRAP      IDProb2 IF IO
         UPDATE    InvDist;IDKey,IDNbr                  ;; 1/11/98
. Debug Code Follows ........................................................
. No-Op  KEYIN     *P1:25,"Update InvDist Record. ":
.                  " (Dupe)  IDKey=",*DV,IDKey,*EL,Reply;
. ...........................................................................
WrtNulEx
         MOVE      "0" TO IDNbr                         ;; 1/11/98
         TRAPCLR   IO
         ADD       "1" TO IDInvNbr             ;; Bump to next invoice nbr
         RETURN
