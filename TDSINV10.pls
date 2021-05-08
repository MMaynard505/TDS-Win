. This program replaced TJONINV5.DBS   11/13/96
.
. TDSINV10.DBS   This program will update a record in the control file that
.                contains the last month that invoices were printed. The
.                record also contains the starting and ending invoice numbers
.                and the total billed for the month. The program also updates
.                the next invoice number and the next customer number.
.
         INCLUDE   COMMON.INC
Version  INIT      "TDSINV10 V1.1"
CTRLFILE IFILE
CUSTMAST IFILE     VARIABLE=1024,NODUP,WEOF
KEY      DIM       12
NEXTIKEY INIT      "NEXT INVOICE"
NEXTCKEY INIT      "NEXT CUSTNBR"
LastBMo  INIT      "LAST BILL MO"

. LAST BILL MO MM YY xxxx yyyy $$$$$.$$

BillDate DIM       5
INVWORK  DIM       4
NEWINV   FORM      4

CUSTWORK DIM       3
NEWCUST  FORM      3
CompName DIM       25

LastBMM  FORM      2
LastBYY  FORM      2
LastSIN  FORM      4
LastEIN  FORM      4
LastAmt  FORM      5.2
MMWork   DIM       2
YYWork   DIM       2

REPLY    DIM       1

         INCLUDE   TJONCMDS.INC

         OPEN      CTRLFILE,"CTRLFILE"
         OPEN      CUSTMAST,"CUSTMAST"
         MOVE      "000" TO Key
         READ      CustMast,Key;CustRcd
         MOVE      CustName TO CompName

         MOVE      NEXTIKEY TO KEY                   ;; Start with Inv nbr
         READ      CTRLFILE,KEY;KEY,NEWINV
         MOVE      NEWINV TO INVWORK             ;; Start with file contents
         KEYIN     *COLOR 23,*ES,*DV,Version:
                   *P25:03,*LL,*DV,CustName:
                   *P19:04,"Control File Maintenance":
                   *P01:06,"The next invoice number is......":
                   *P33:06,*COLOR 19,*DV,NEWINV,*COLOR 23:
                   *P01:07,"Do you need to change it? (Y/N)":
                   *P33:07,REPLY;
         CMATCH    "Y" TO REPLY
         GOTO      K1 IF EQUAL
         CMATCH    "y" TO REPLY
         GOTO      D1 IF NOT EQUAL
.
.  Allow for updating the invoice number
.
K1       KEYIN     *COLOR 23,*P1:7,"Enter the next invoice number:  ____":
                   *COLOR 19,*P33:7,*DE,*JR,*ZF,InvWork:
                   *P33:07,*DV,InvWork,*COLOR 23;
         TYPE      INVWORK
         GOTO      K1 IF NOT EQUAL
         MOVE      INVWORK TO NEWINV
         COMPARE   "0000" TO NEWINV
         GOTO      K1 IF LESS
         GOTO      K1 IF EQUAL
         UPDATE    CTRLFILE;KEY,NEWINV

D1       DISPLAY   *P1:06,*COLOR 23:
                   "The next invoice number to be printed will be.....":
                   *COLOR 19,InvWork,*COLOR 23,*P1:07,*EL;

         MOVE      NEXTCKEY TO KEY                 ;; Next is Cust nbr
         READ      CTRLFILE,KEY;KEY,REPLY,NEWCUST
         MOVE      NEWCUST TO CUSTWORK             ;; Start with file contents
         KEYIN     *COLOR 23,*P1:08,"The next customer number is.....":
                   *P33:08,*COLOR 19,*DV,NEWCUST:
                   *COLOR 23,*P1:09,"Do you need to change it? (Y/N)":
                   *COLOR 19,*P33:09,REPLY,*COLOR 23;
         CMATCH    "Y" TO REPLY
         GOTO      K2 IF EQUAL
         CMATCH    "y" TO REPLY
         GOTO      GetLBMO IF NOT EQUAL
.
.  Allow for updating the next customer number
.
K2       KEYIN     *P1:09,"Enter the next customer number: ___":
                   *P33:09,*COLOR 19,*DE,*ZF,*JR,CustWork,*EL:
                   *P33:09,*DV,CustWork,*COLOR 23,*EL;
         TYPE      CustWork
         GOTO      K2 IF NOT EQUAL
         MOVE      CUSTWORK TO NEWCUST
         COMPARE   "001" TO NEWCUST
         GOTO      K2 IF LESS
         COMPARE   "999" TO NEWCUST
         GOTO      EXIT IF EQUAL
         READ      CUSTMAST,CUSTWORK;REPLY
         GOTO      BADCN IF NOT OVER
         READ      CTRLFILE,KEY;KEY
         MOVE      " " TO REPLY
         UPDATE    CTRLFILE;KEY,REPLY,NEWCUST
         KEYIN     *COLOR 23,*P1:08:
                   "The next customer number to be used will be....... ":
                   *COLOR 19,*DV,NEWCUST,*COLOR 23:
                   *P1:09,*EL:
                   *P1:12,"Press any key to acknowledge...... ",*CL,*+,REPLY;
GetLBMO
         MOVE      LastBMO TO KEY                  ;; Last Billing Month
         READ      CTRLFILE,KEY;KEY,REPLY,LastBMM,Reply:
                                          LastBYY,Reply:
                                          LastSIN,Reply:
                                          LastEIN,Reply:
                                          LastAmt
         GOTO      ContLBMM IF NOT OVER
         KEYIN     *P1:23,"Oops...   KEY=",*DV,KEY:
                   *P1:24,"      LastBMM=",*DV,LastBMM:
                   *+,Reply;
ContLBMM
         MOVE      LastBMM TO MMWork
         MOVE      LastBYY TO YYWork
         REPLACE   " 0" IN MMWork
         REPLACE   " 0" IN YYWork
         PACK      BillDate FROM MMWork, "/", YYWork

         KEYIN     *COLOR 23,*P1:11,"The last billing record was: ":
                   *COLOR 19,*P33:11,*DV,BillDate," Invoices ":
                   *DV,LastSIN,"-",*DV,LastEIN," Totaling ",*DV,LastAmt:
                   *COLOR 23,*P1:12,"Do you need to change it? (Y/N)":
                   *P33:12,*UC,REPLY;
         CMATCH    "Y" TO REPLY
         GOTO      EXIT IF NOT EQUAL
.
.  Allow for updating the last billing record
.
K3       KEYIN     *P1:11,*COLOR 23,*EL:
                   "Enter the last billing month/year __/__":
                   *P35:11,*ZF,*JR,*DE,*+,LastBMM;
         MOVE      LastBMM TO MMWork
         REPLACE   " 0" IN MMWork
         KEYIN     *P35:11,*DV,MMWork,"/":
                   *ZF,*JR,*DE,LastBYY
         MOVE      LastBYY TO YYWork
         REPLACE   " 0" IN YYWork
         DISPLAY   *P38:11,YYWork;

         COMPARE   "01" TO LastBMM
         GOTO      K3 IF LESS
         COMPARE   "99" TO LastBMM
         GOTO      EXIT IF EQUAL
         COMPARE   "13" TO LastBMM
         GOTO      K3 IF NOT LESS
         COMPARE   "96" TO LastBYY
         GOTO      K3 IF LESS
         MOVE      " " TO REPLY
.
.  The reasoning here is that if the Last Billing date is changing then
.  we must be preparing to rerun a billing cycle. The other fields will
.  get updated during the printing cycle.
.
         MOVE      "0000" TO LastSIN
         MOVE      "0000" TO LastEIN
         MOVE      "0.00" TO LastAmt
         READ      CTRLFILE,KEY;KEY
         UPDATE    CTRLFILE;KEY,REPLY,LastBMM,Reply:
                                          LastBYY,Reply:
                                          LastSIN,Reply:
                                          LastEIN,Reply:
                                          LastAmt
         KEYIN     *P1:11,*EL,"The last billing month/year will be ":
                   *DV,MMWork,"/",*DV,YYWork,*EL:
                   *P1:12,"Press any key to acknowledge...... ",*CL,*+,REPLY;
         GOTO      EXIT

BADCN
         DISPLAY   *B,*P37:09,*EL:
                   *COLOR 19,"<=== Customer ",NEWCUST," already on file! ":
                   *COLOR 23;
         GOTO      K2

EXIT     CHAIN     "TJONMENU"       RETURN TO TJONMENU
