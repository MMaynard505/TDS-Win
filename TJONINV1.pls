. TJONINV1.DBS             Custmast file maintenance program
. V.M=1.2     2/09/88
. V.M=1.3     1/10/89
. V.M=1.4     1/09/92  Minor modification for new customer numbers
. V.M=1.5     9/25/92  Allow entry of customer's fax number
. V.M=1.6     7/20/95  Allow entry of customer unique filename (CustFN)
. V.M=1.7     1/17/96  Allow update by customer unique filename (CustFN)
. V.M=1.8     1/29/96  Flag to indicate if eligible for COMBINED tape output
. V.M=1.9     5/29/96  Bug in ZIP processing (4 digit zip is blank filled)
.             7/17/96  Minor bug in the change routine CUSTATTN field
. V.M=1.A     1/10/98  Added Last Invoice Date field
. V.M=1.B     1/17/98  Added Updating/Displaying of CSC/TRW/TU flags
. V.M=1.C     1/31/98  Added Updating/Displaying of Pos 44-53 in Diskette Header
.             2/01/98  Added Chain to invoice and custmast maintenance
.
         INCLUDE   COMMON.INC
.
CustMast IFILE     VARIABLE=1024,NODUP,WEOF
.
CustFNi  IFILE     VARIABLE=1024,WEOF
.
         INCLUDE   TJONCMDS.INC     /* Customer Master Record Description
.
CTRLFILE IFILE
.
Version  INIT      "TJONINV1 V.M=1.C"
CompName DIM       25             ;; Company Name
Spaces   DIM       80
Index    DIM       1
PIndex   FORM      1
Reply    DIM       1
Key      DIM       3
NameWork DIM       25
CustFNwk DIM       8              /* Work area for Customer unique filename
CustFNsv DIM       8              /* Save area for Customer unique filename
AddrWork DIM       25
CityWork DIM       14
SteWork  DIM       2
ZipWork  DIM       5
AttnWork DIM       25
EHdrWork DIM       10             /*  Expected Header Record            1/31/98
WorkMM   DIM       2              /*  Month of the Last Invoice Date 1/10/98
WorkDD   DIM       2              /*  Day   of the Last Invoice Date
WorkYYYY DIM       4              /*  Year w/Century of the Last Invoice Date
PN1      DIM       3              /*  Area code
PN2      DIM       3              /*  First 3 of phone
PN3      DIM       4              /*  Last  4 of phone
FN1      DIM       3              /*  Area code of fax number
FN2      DIM       3              /*  First 3 of fax
FN3      DIM       4              /*  Last  4 of fax
CFKey    DIM       12             /*  Key to CTRLFILE
NextCust DIM       3              /*  Next Customer number
FORM3    FORM      3
DIM1     DIM       1
.
.
.
         RESET     SPACES TO 80
         LENSET    SPACES
         RESET     SPACES
         TRAP      NOFILE IF IO
         OPEN      CUSTMAST,"CUSTMAST"
         TRAPCLR   IO
         MOVE      "000" TO Key            ;; Get Company Info
         READ      CustMast,Key;CustRcd
         MOVE      CustName TO CompName    ;; Company Name
         TRAP      NoCFFile IF IO
         OPEN      CustFNi,"CustFNi"
         TRAPCLR   IO
         TRAP      NoCTFile IF IO
         OPEN      CTRLFILE,"CTRLFILE"
         TRAPCLR   IO
         MOVE      "NEXT CUSTNBR" TO CFKEY
.
.
Start
         DISPLAY   *COLOR 23,*ES:
                   *P01:01,Version:
                   *COLOR 19,*P30:01,CompName:
                   *COLOR 23,*P22:03,"Customer File Maintenance Program  ":
                   *COLOR 23,*P25:05,"1-Add    ",*COLOR 19,*P27:05,"A":
                   *COLOR 23,*P25:06,"2-Change ",*COLOR 19,*P27:06,"C":
                   *COLOR 23,*P25:07,"3-Delete ",*COLOR 19,*P27:07,"D":
                   *COLOR 23,*P25:08,"4-Inquiry",*COLOR 19,*P27:08,"I":
                   *COLOR 23,*P25:09,"5-Maintenance",*COLOR 19,*P27:09,"M":
                   *COLOR 23,*P25:12,"9-Exit   ",*COLOR 19,*P27:12,"E":
                   *COLOR 23,*P25:16,"Please select desired function:";
GetIndex
         KEYIN     *COLOR 19,*+,*P57:16,"_",*P57:16,INDEX,*COLOR 23;
         CMATCH    "9" TO INDEX
         GOTO      CLOSEFLE IF EOS
         CMATCH    "9" TO INDEX
         GOTO      CLOSEFLE IF EQUAL
         CMATCH    "E" TO INDEX
         GOTO      CLOSEFLE IF EQUAL
         CMATCH    "e" TO INDEX
         GOTO      CLOSEFLE IF EQUAL

         CMATCH    "1" TO INDEX
         GOTO      ADDREC IF EQUAL
         CMATCH    "A" TO INDEX
         GOTO      ADDREC IF EQUAL
         CMATCH    "a" TO INDEX
         GOTO      ADDREC IF EQUAL

         CMATCH    "2" TO INDEX
         GOTO      CHGREC IF EQUAL
         CMATCH    "C" TO INDEX
         GOTO      CHGREC IF EQUAL
         CMATCH    "c" TO INDEX
         GOTO      CHGREC IF EQUAL

         CMATCH    "3" TO INDEX
         GOTO      DELREC IF EQUAL
         CMATCH    "D" TO INDEX
         GOTO      DELREC IF EQUAL
         CMATCH    "d" TO INDEX
         GOTO      DELREC IF EQUAL

         CMATCH    "4" TO INDEX
         GOTO      INQUIRY IF EQUAL
         CMATCH    "I" TO INDEX
         GOTO      INQUIRY IF EQUAL
         CMATCH    "i" TO INDEX
         GOTO      INQUIRY IF EQUAL


         CMATCH    "5" TO INDEX
         GOTO      Mainten IF EQUAL
         CMATCH    "M" TO INDEX
         GOTO      Mainten IF EQUAL
         CMATCH    "m" TO INDEX
         GOTO      Mainten IF EQUAL

         GOTO      GETINDEX
.
.         THIS ROUTINE WILL ADD A CUSTOMER RECORD TO THE DATA BASE
.
AddRec
         READ      CTRLFILE,CFKEY;CFKEY,DIM1,NEXTCUST
         MOVE      NEXTCUST TO CUSTNUM
         MOVE      "0" TO FORM3
NKLoop
         MOVE      CUSTNUM TO KEY
         READ      CUSTMAST,KEY;CustRcd
         GOTO      UNIQUE IF OVER
         MOVE      KEY TO FORM3
         ADD       "1" TO FORM3
         MOVE      FORM3 TO CUSTNUM
         GOTO      NKLoop
Unique
.
         DISPLAY   *COLOR 23,*ES:
                   *P30:01,CompName:
                   *P14:03,"This program will add a new customer ":
                   *P51:03,"To the Database":
                   *P10:06,"Customer number: ___":
                   *P44:06,"Unique Customer Filename: ________":
                   *P44:07,"Expected Header Record:   __________":
                   *P10:08,"Customer name: _________________________":
                   *P10:10,"Mailing address: _________________________":
                   *P10:12,"City: ______________":
                   *P10:14,"State: __":
                   *P10:16,"Zip code: _____":
                   *P10:18,"Send to attn of: _________________________":
                   *P10:19,"Last Invoice Date: MM/DD/YYYY":
                   *P10:20,"Telephone number: ___-___-____":
                   *P10:21,"Fax phone number: ___-___-____":
                   *P50:21,"CBI?   Experian?  Trans Union?":
                   *P10:22,"Add to COMBINED tape: (Y/N) _",*EL:
                   *P50:22," _         _            _     ";
.
.
AddCust
         KEYIN     *COLOR 19,*P27:06,*DV,CUSTNUM:
                   *+,*DE,*JR,*ZF,*P27:06,*RV,CUSTNUM:
                   *P27:06,*DV,CUSTNUM;
         MATCH     "999" TO CUSTNUM
         GOTO      START IF EQUAL
         MOVE      CUSTNUM TO KEY
. No-op  TRAP      DUPREC IF IO
         READ      CUSTMAST,KEY;CustRcd
         GOTO      DUPREC IF NOT OVER
         MOVE      Spaces TO CustFN                      /* 7/20/95...jmm
         KEYIN     *COLOR 19,*+,*JL,*P70:06,*UC,CustFN,*LC:   /*
                   *P70:06,*DV,CustFN;                        /*
         MATCH     Spaces TO CustFN
         GOTO      AddHdrID IF EQUAL

         READ      CustFNi,CustFN;;                      /*
         GOTO      Duprec2 IF NOT OVER                   /*

AddHdrID
. Code in here to get the expected diskette header information (CustEHdr)

         KEYIN     *COLOR 19,*+,*JL,*P70:07,*UC,CustEHdr,*LC:   /*
                   *P70:07,*DV,CustEHdr;                        /*
         MATCH     Spaces TO CustEHdr
         GOTO      AddHdrID IF EQUAL

         MOVE      Spaces TO PN1
         MOVE      Spaces TO PN2
         MOVE      Spaces TO PN3
         MOVE      Spaces TO FN1
         MOVE      Spaces TO FN2
         MOVE      Spaces TO FN3
         MOVE      Spaces TO CustAttn                     /* 7/17/96 Bug Fix
         MOVE      Spaces TO CustLInD                     /* 1/10/98 New Field
         KEYIN     *COLOR 19,*IT,*+,*JL,*P25:08,CustName:
                   *P25:08,*DV,CustName:
                   *JL,*P27:10,CustAddr:
                   *P27:10,*DV,CustAddr:
                   *JL,*P16:12,CustCity:
                   *P16:12,*DV,CustCity:
                   *JL,*P17:14,CustSte:
                   *P17:14,*DV,CustSte:
                   *IN,*DE,*JR,*ZF,*P20:16,CustZip:
                   *P20:16,*DV,CustZip:
                   *IT,*JL,*P27:18,*RV,CustAttn,*IN,*DE,*JR:
                   *P27:18,*DV,CustAttn,*IN,*DE,*JR:
                   *ZF,*P28:20,*RV,PN1,*P28:20,*DV,PN1:
                   *P31:20,"-",*P32:20,*RV,PN2,*P32:20,*DV,PN2:
                   *P35:20,"-",*P36:20,*RV,PN3,*P36:20,*DV,PN3;
         PACK      CustPhon FROM PN1, "-", PN2, "-", PN3     /* 2/09/88
         MOVE      PN1 TO FN1
         MOVE      PN2 TO FN2
         MOVE      "    " TO FN3
         PACK      CustFax FROM FN1, "-", FN2, "-", FN3      /* 9/25/92
         KEYIN     *P28:20,*DV,CustPhon:
                   *P28:21,*DV,CustFax:
                   *+,*DE,*JR,*ZF,*P28:21,*RV,FN1,*P28:21,*DV,FN1:
                   *P31:21,"-",*P32:21,*RV,FN2,*P32:21,*DV,FN2:
                   *P35:21,"-",*P36:21,*RV,FN3,*P36:21,*DV,FN3;
.
         PACK      CustFax  FROM FN1, "-", FN2, "-", FN3     /* 9/25/92
         DISPLAY   *P28:21,CustFax;

. Code to get the bureaus to report to
         MOVE      "?" TO CustCBIf            ;; CBI Flag
         MOVE      "?" TO CustExpf            ;; Exp Flag
         MOVE      "?" TO CustTUf             ;; TU  Flag
KICBIf
         KEYIN     *P51:22,*DV,CustCBIf,*+,*RV,*UC,*P51:22,CustCBIf:
                   *P51:22,*DV,CustCBIf;
         CMATCH    "?" TO CustCBIf            ;; Any Change?
         GOTO      KICBIf IF EOS
         GOTO      KICBIf IF EQUAL
         CMATCH    "Y" TO CustCBIf            ;; Y=Yes
         GOTO      KIExpf IF EQUAL
         CMATCH    "N" TO CustCBIf            ;; N=No
         GOTO      KICBIf IF NOT EQUAL
KIExpf
         KEYIN     *P61:22,*DV,CustExpf,*+,*RV,*UC,*P61:22,CustExpf:
                   *P61:22,*DV,CustExpf;
         CMATCH    "?" TO CustExpf            ;; Any Change?
         GOTO      KIExpf IF EOS
         GOTO      KIExpf IF EQUAL
         CMATCH    "Y" TO CustExpf            ;; Y=Yes
         GOTO      KITUf  IF EQUAL
         CMATCH    "N" TO CustExpf            ;; N=No
         GOTO      KIExpf IF NOT EQUAL
KITUf
         KEYIN     *P74:22,*DV,CustTUf,*+,*RV,*UC,*P74:22,CustTUf:
                   *P74:22,*DV,CustTUf;
         CMATCH    "?" TO CustTUf            ;; Any Change?
         GOTO      KITUf IF EOS
         GOTO      KITUf IF EQUAL
         CMATCH    "Y" TO CustTUf            ;; Y=Yes
         GOTO      AddCust3 IF EQUAL
         CMATCH    "N" TO CustTUf            ;; N=No
         GOTO      KITUf IF NOT EQUAL

AddCust3
         MOVE      "Y" TO CustComb            ;; 1/29/96 Default to COMBINED=Y
         KEYIN     *COLOR 19,*P38:22,*DV,CustComb:
                   *P38:22,*UC,*+,*RV,CustComb,*LC:
                   *P38:22,*DV,CustComb,*EL:
                   *P51:22,*DV,CustCBIf:
                   *P61:22,*DV,CustExpf:
                   *P74:22,*DV,CustTUf;
         CMATCH    "Y" TO CustComb
         GOTO      AddCust4 IF EQUAL
         CMATCH    "N" TO CustComb
         GOTO      AddCust4 IF EQUAL
         DISPLAY   *B,*P39:22,*COLOR 124:
                   " <==== Combined Flag must be Y or N ",*EL;
         GOTO      AddCust3
AddCust4
         WRITE     CUSTMAST,KEY;*PL,*-,CustRcd
         INSERT    CustFNi,CustFN
. No-op  TRAPCLR   IO
         MATCH     NEXTCUST TO CUSTNUM
         GOTO      ADDREC IF NOT EQUAL     ;; If user changed, don't update CF
         MOVE      NEXTCUST TO FORM3
GetUKey
         ADD       "1" TO FORM3
         MOVE      FORM3 TO NEXTCUST

         READ      CustMast,NextCust;Reply;
         GOTO      GETUKEY IF NOT OVER
. This is a unique customer number, update the CTRLFILE...
         REPLACE   " 0" IN NEXTCUST
         READ      CTRLFILE,CFKEY;CFKEY
         UPDATE    CTRLFILE;CFKEY,DIM1,NEXTCUST
         MOVE      NEXTCUST TO CUSTNUM
         GOTO      ADDREC

DupRec
         UNPACK    CustLInD TO WorkMM, WorkDD, WorkYYYY
         DISPLAY   *COLOR 23:
                   *P10:06,*EF,"Customer number: ",CustNum:
                   *P44:06,"Unique Customer Filename: ",CustFN:
                   *P44:07,"Expected Header Record:   ",CustEHdr:
                   *P10:08,"Customer name: ",CustName:
                   *P10:10,"Mailing address: ",CustAddr:
                   *P10:12,"City: ",CustCity:
                   *P10:14,"State: ",CustSte:
                   *P10:16,"Zip code: ",CustZip:
                   *P10:18,"Send to attn of: ",CustAttn:
                   *P10:19,"Last Invoice Date: ",WorkMM,"/",WorkDD,"/",WorkYYYY:
                   *P10:20,"Telephone number: ",CustPhon:
                   *P10:21,"Fax phone number: ",CustFax:
                   *P50:21,"CBI?   Experian?  Trans Union?":
                   *P51:22,CustCBIf,*P61:22,CustExpf,*P74:22,CustTUf:
                   *P10:22,"Add to COMBINED tape: (Y/N) ",CustComb
         KEYIN     *COLOR 124:
                   *P10:24,"Customer number ",*DV,KEY:
                   "  Is already on file.  Try again?  Y or N":
                   *+,*P71:24,"_",*P71:24,*B,REPLY;
         CMATCH    "Y" TO REPLY
         GOTO      ADDREC IF EQUAL
         CMATCH    "y" TO REPLY
         GOTO      ADDREC IF EQUAL
         GOTO      START

DupRec2
         UNPACK    CustLInD TO WorkMM, WorkDD, WorkYYYY
         DISPLAY   *COLOR 23:
                   *P10:06,*EF,"Customer number: ",CustNum:
                   *P44:06,"Unique Customer Filename: ",CustFN:
                   *P44:07,"Expected Header Record:   ",CustEHdr:
                   *P10:08,"Customer name: ",CustName:
                   *P10:10,"Mailing address: ",CustAddr:
                   *P10:12,"City: ",CustCity:
                   *P10:14,"State: ",CustSte:
                   *P10:16,"Zip code: ",CustZip:
                   *P10:18,"Send to attn of: ",CustAttn:
                   *P10:19,"Last Invoice Date: ",WorkMM,"/",WorkDD,"/",WorkYYYY:
                   *P10:20,"Telephone number: ",CustPhon:
                   *P10:21,"Fax phone number: ",CustFax:
                   *P50:21,"CBI?   Experian?  Trans Union?":
                   *P51:22,CustCBIf,*P61:22,CustExpf,*P74:22,CustTUf:
                   *P10:22,"Add to COMBINED tape: (Y/N) ",CustComb
         KEYIN     *COLOR 124:
                   *P10:22,"Customer Unique Filename ",*DV,CustFN:
                   "  Is already on file.":
                   *P10:23,"    Try again?  Y or N":
                   *+,*P38:23,"_",*P38:23,*B,REPLY
         CMATCH    "Y" TO REPLY
         GOTO      ADDREC IF EQUAL
         CMATCH    "y" TO REPLY
         GOTO      ADDREC IF EQUAL
         GOTO      START

ChgRec
         DISPLAY   *ES,*COLOR 23:
                   *P30:01,CompName:
                   *P14:03,"This program will modify the customer ":
                   *P52:03,"master record":
                   *P10:06,*EF,"Customer to be changed: "
ChgNum
         KEYIN     *COLOR 19,*+,*P35:06,"___",*P35:06,KEY
         CMATCH    " " TO KEY
         GOTO      ChgID  IF EOS
         MATCH     "999" TO KEY
         GOTO      START IF EQUAL
         MATCH     "***" TO KEY
         GOTO      START IF EQUAL
         TRAP      RANGETRP IF RANGE
         READ      CUSTMAST,KEY;CustRcd
.  12/15/92 Added code to clear all of the work areas for updating
         MOVE      Spaces TO CustFNwk
         MOVE      Spaces TO CustFNsv
         MOVE      Spaces TO NAMEWORK
         MOVE      Spaces TO ADDRWORK
         MOVE      Spaces TO CITYWORK
         MOVE      Spaces TO STEWORK
         MOVE      Spaces TO ZIPWORK
         MOVE      Spaces TO ATTNWORK
         MOVE      Spaces TO WorkMM               /*  Added 1/10/98
         MOVE      Spaces TO WorkDD               /*  Added 1/10/98
         MOVE      Spaces TO WorkYYYY             /*  Added 1/10/98
         MOVE      Spaces TO EHdrWork             /*  Added 1/31/98
         MOVE      Spaces TO PN1
         MOVE      Spaces TO PN2
         MOVE      Spaces TO PN3
         MOVE      Spaces TO FN1
         MOVE      Spaces TO FN2
         MOVE      Spaces TO FN3

         GOTO      CHGNOF IF OVER
         TRAPCLR   RANGE
         GOTO      DispRec

ChgID
         KEYIN     *COLOR 23:
                   *P44:06,*EL,"Unique Customer Filename: ":
                   *COLOR 19:
                   *UC,*+,*P70:06,CustFNwk:
                   *LC,*P70:06,*DV,CustFNwk;
         CMATCH    " " TO CustFNwk
         GOTO      ChgNum IF EOS
         RESET     CustFNwk to 8
         LENSET    CustFNwk
         RESET     CustFNwk
         MOVE      CustFNwk TO CustFN
         READ      CustFNi,CustFN;CustNum;
         GOTO      ChgNOF IF OVER
         MOVE      CustNum TO KEY
         READ      CUSTMAST,KEY;CustRcd

DispRec
         UNPACK    CustLInD TO WorkMM, WorkDD, WorkYYYY
         DISPLAY   *COLOR 23:
                   *P10:06,*EL,"Customer number: ",CustNum:
                   *P44:06,"Unique Customer Filename: ",CustFN:
                   *P44:07,"Expected Header Record:   ",CustEHdr:
                   *P10:08,"Customer name: ",CustName:
                   *P10:10,"Mailing address: ",CustAddr:
                   *P10:12,"City: ",CustCity:
                   *P10:14,"State: ",CustSte:
                   *P10:16,"Zip code: ",CustZip:
                   *P10:18,"Send to attn of: ",CustAttn:
                   *P10:19,"Last Invoice Date: ",WorkMM,"/",WorkDD,"/",WorkYYYY:
                   *P10:20,"Telephone number: ",CustPhon:
                   *P10:21,"Fax phone number: ",CustFax:
                   *P50:21,"CBI?   Experian?  Trans Union?":
                   *P51:22,CustCBIf,*P61:22,CustExpf,*P74:22,CustTUf:
                   *P10:22,"Add to COMBINED tape: (Y/N) ",CustComb
Keyin0   KEYIN     *UC,*+,*P70:06,CustFNwk:
                   *LC,*P70:06,*DV,CustFNwk;
         CMATCH    " " TO CustFNwk
         GOTO      Keyin1 IF EOS
         RESET     CustFNwk to 8
         LENSET    CustFNwk
         RESET     CustFNwk
         MATCH     CustFN TO CustFNwk
         GOTO      Keyin1 IF EQUAL
         MOVE      CustFN   TO CustFNsv            /* Save the old for deleteK
         MOVE      CustFNwk TO CustFN
         READ      CustFNi,CustFN;CustNum;
         GOTO      DupeFNi IF NOT OVER
.
Keyin1
         KEYIN     *UC,*+,*P70:07,EHdrWork:
                   *LC,*P70:07,*DV,EHdrWork
         CMATCH    " " TO EHdrWork
         GOTO      Keyin1B IF EOS
         MOVE      EHdrWork TO CustEHdr

Keyin1B
         KEYIN     *IT,*+,*P25:08,NameWork:
                   *P25:08,*DV,NameWork
         CMATCH    " " TO NameWork
         GOTO      Keyin2 IF EOS
         MOVE      NameWork TO CustName

Keyin2   KEYIN     *IT,*+,*P27:10,ADDRWORK:
                   *P27:10,*DV,ADDRWORK
         CMATCH    " " TO ADDRWORK
         GOTO      Keyin3 IF EOS
         MOVE      ADDRWORK TO CUSTADDR
Keyin3   KEYIN     *+,*P16:12,CITYWORK:
                   *P16:12,*DV,CITYWORK
         CMATCH    " " TO CITYWORK
         GOTO      Keyin4 IF EOS
         MOVE      CITYWORK TO CUSTCITY
Keyin4   KEYIN     *+,*P17:14,STEWORK:
                   *P17:14,*DV,STEWORK
         CMATCH    " " TO STEWORK
         GOTO      Keyin5 IF EOS
         MOVE      STEWORK TO CUSTSTE
Keyin5   KEYIN     *IN,*+,*DE,*JR,*ZF,*P20:16,ZIPWORK:
                   *P20:16,*DV,ZIPWORK
         CMATCH    " " TO ZIPWORK
         GOTO      Keyin6 IF EOS
         MOVE      ZIPWORK TO CUSTZIP
Keyin6   KEYIN     *IT,*+,*P27:18,ATTNWORK:
                   *P27:18,*DV,ATTNWORK
         CMATCH    " " TO ATTNWORK
         GOTO      Keyin7 IF EOS
         MOVE      ATTNWORK TO CUSTATTN

. Changed 1/10/98 for Customer Last Invoice Date
Keyin7
         KEYIN     *IN,*+,*DE,*JR,*ZF,*P29:19,WorkMM,*P29:19,*DV,WorkMM;
         CMATCH    " " TO WorkMM
         GOTO      Keyin7B IF EOS
         KEYIN     *P31:19,"/",*IN,*+,*DE,*JR,*ZF,WorkDD:
                   *P32:19,*DV,WorkDD:
                   *P34:19,"/",*IN,*+,*DE,*JR,*ZF,WorkYYYY:
                   *P35:19,*DV,WorkYYYY;

         PACK      CustLInD FROM WorkMM, WorkDD, WorkYYYY
         DISPLAY   *P29:19,WorkMM,"/",WorkDD,"/",WorkYYYY;

Keyin7B
         UNPACK    CUSTPHON TO PN1, REPLY, PN2, REPLY, PN3
         KEYIN     *IN,*+,*DE,*JR,*ZF,*P28:20,PN1:
                   *P28:20,*DV,PN1,*P31:20,"-",PN2:
                   *P32:20,*DV,PN2,*P35:20,"-",PN3,*P36:20,*DV,PN3;
         CMATCH    " " TO PN1
         GOTO      Keyin8  IF EOS
         PACK      CUSTPHON FROM PN1, "-", PN2, "-", PN3
Keyin8
         DISPLAY   *P28:20,CUSTPHON;
         UNPACK    CustFax  TO FN1, REPLY, FN2, REPLY, FN3
         KEYIN     *IN,*+,*DE,*JR,*ZF,*P28:21,FN1:
                   *P28:21,*DV,FN1,*P31:21,"-",FN2:
                   *P32:21,*DV,FN2,*P35:21,"-",FN3,*P36:21,*DV,FN3;
         CMATCH    " " TO FN1
         GOTO      Keyin9  IF EOS
         PACK      CustFax FROM FN1, "-", FN2, "-", FN3
Keyin9
         DISPLAY   *P28:21,CustFax;

. Code to get the bureaus to report to
KInCBIf
         KEYIN     *P51:22,*DV,CustCBIf,*+,*RV,*UC,*P51:22,CustCBIf:
                   *P51:22,*DV,CustCBIf;
         CMATCH    "Y" TO CustCBIf            ;; Y=Yes
         GOTO      KInExpf IF EOS
         GOTO      KInExpf IF EQUAL
         CMATCH    "N" TO CustCBIf            ;; N=No
         GOTO      KInCBIf IF NOT EQUAL
KInExpf
         KEYIN     *P61:22,*DV,CustExpf,*+,*RV,*UC,*P61:22,CustExpf:
                   *P61:22,*DV,CustExpf;
         CMATCH    "Y" TO CustExpf            ;; Y=Yes
         GOTO      KInTUf IF EOS
         GOTO      KInTUf  IF EQUAL
         CMATCH    "N" TO CustExpf            ;; N=No
         GOTO      KInExpf IF NOT EQUAL
KInTUf
         KEYIN     *P74:22,*DV,CustTUf,*+,*RV,*UC,*P74:22,CustTUf:
                   *P74:22,*DV,CustTUf;
         CMATCH    "Y" TO CustTUf            ;; Y=Yes
         GOTO      Keyin10 IF EOS
         GOTO      Keyin10 IF EQUAL
         CMATCH    "N" TO CustTUf            ;; N=No
         GOTO      KInTUf IF NOT EQUAL

Keyin10
         KEYIN     *COLOR 23,*P38:22,*DV,CustComb:
                   *P38:22,*UC,*+,*RV,CustComb,*LC:
                   *P38:22,*DV,CustComb,*EL:
                   *P51:22,*DV,CustCBIf:
                   *P61:22,*DV,CustExpf:
                   *P74:22,*DV,CustTUf;
         CMATCH    "Y" TO CustComb
         GOTO      Keyin11 IF EQUAL
         CMATCH    "N" TO CustComb
         GOTO      Keyin11 IF EQUAL
         DISPLAY   *B,*P39:22,*COLOR 124:
                   " <==== Combined Flag must be Y or N ",*EL;
         GOTO      Keyin10
Keyin11

.        Finished with data entry, update the record
ReWrite
*  KEYIN *P1:25,*EL,*HON,"Updating Record into CUSTMAST--->",*DV,CustFN,Reply;
         UPDATE    CUSTMAST;*PL,*-,CustRcd
*  KEYIN *P1:25,*EL,*HON,"Inserting Key into CustFNi-->",*DV,CustFNsv,Reply;
         INSERT    CustFNi,CustFN
         MATCH     Spaces TO CustFNsv
         GOTO      ChgMsg IF EQUAL
*  KEYIN *P1:25,*EL,*HON,"Deleting Key from CustFNi-->",*DV,CustFNsv,Reply;
         DELETEK   CustFNi,CustFNsv
ChgMsg   KEYIN     *+,*P10:23,*EL,"Any more changes?  Y or N":
                   *P39:23,"_",*P39:23,REPLY
         CMATCH    "Y" TO REPLY
         GOTO      CHGNUM IF EQUAL
         GOTO      START

DupeFNi
         KEYIN     *B,*COLOR 124:
                   *P1:23,*EL,"Unique Customer filename is already in ":
                   "the file. (",*DV,CustFN,"=":
                   *DV,CustNum:
                   ")",*P1:24,*EL,"   Try again?  Y or N":
                   *+,*P25:24,"_",*P25:24,REPLY
         CMATCH    "Y" TO REPLY
         GOTO      CHGNUM IF EQUAL
         GOTO      CHGMSG

ChgNOF   KEYIN     *P10:22,*EL,"Record not on file.  Try again?  Y or N":
                   *+,*P50:22,"_",*P50:22,REPLY
         CMATCH    "Y" TO REPLY
         GOTO      CHGNUM IF EQUAL
         GOTO      CHGMSG
.
.
.
DELREC   DISPLAY   *ES:
                   *P30:01,CompName:
                   *P14:03,"This program will Delete a customer ":
                   *P50:03,"from the Database":
                   *P10:06,"Delete customer number:"
DelNum   KEYIN     *P33:06,*+,*EF,"___",*P33:06,KEY
         CMATCH    " " TO KEY
         GOTO      DelID IF EOS
         CMATCH    "^" TO KEY
         GOTO      START IF EQUAL
         CMATCH    "*" TO KEY
         GOTO      START IF EQUAL
         MATCH     "***" TO KEY
         GOTO      START IF EQUAL
         MATCH     "999" TO KEY
         GOTO      START IF EQUAL
         READ      CUSTMAST,KEY;CustRcd
         GOTO      DelNOF IF OVER
         MOVE      " " TO REPLY
         GOTO      DelDisp

DelID
         KEYIN     *COLOR 23:
                   *P44:06,*EL,"Unique Customer Filename: ":
                   *COLOR 19:
                   *UC,*+,*P70:06,CustFNwk:
                   *LC,*P70:06,*DV,CustFNwk;
         CMATCH    " " TO CustFNwk
         GOTO      DelNum IF EOS
         RESET     CustFNwk to 8
         LENSET    CustFNwk
         RESET     CustFNwk
         MOVE      CustFNwk TO CustFN
         READ      CustFNi,CustFN;CustNum;
         GOTO      DelNOF IF OVER
         MOVE      CustNum TO KEY
         READ      CUSTMAST,KEY;CustRcd
         UNPACK    CustLInD TO WorkMM, WorkDD, WorkYYYY

DelDisp
         DISPLAY   *P44:06,"Unique Customer Filename: ",CustFN:
                   *P44:07,"Expected Header Record:   ",CustEHdr:
                   *P10:08,"Customer name: ",CustName:
                   *P10:10,"Mailing address: ",CustAddr:
                   *P10:12,"City: ",CustCity:
                   *P10:14,"State: ",CustSte:
                   *P10:16,"Zip code: ",CustZip:
                   *P10:18,"Send to attn of: ",CustAttn:
                   *P10:19,"Last Invoice Date: ":
                   WorkMM,"/",WorkDD,"/",WorkYYYY:
                   *P10:20,"Telephone number: ",CustPhon:
                   *P10:21,"Fax phone number: ",CustFax:
                   *P50:21,"CBI?   Experian?  Trans Union?":
                   *P51:22,CustCBIf,*P61:22,CustExpf,*P74:22,CustTUf:
                   *P10:22,"Add to COMBINED tape: (Y/N) ",CustComb;
         KEYIN     *P10:23,*EL,*COLOR 19,"Delete this customer?  Y or N":
                   *+,*P42:23,"_",*P42:23,Reply
         CMATCH    "Y" TO REPLY
         GOTO      DELIT IF EQUAL
         CMATCH    "y" TO REPLY
         GOTO      DELIT IF EQUAL
         GOTO      DELMSG
.
.
.
DELIT    DELETE    CUSTMAST,KEY
         DELETEK   CustFNi,CustFN             /* Delete secondary index
DELMSG   DISPLAY   *P10:23,*EL,"Any more customers to delete?  Y or N"
         KEYIN     *+,*P50:23,"_",*P50:23,REPLY
         CMATCH    "Y" TO REPLY
         GOTO      DELNUM IF EQUAL
         GOTO      START

DelNOF
         KEYIN     *P10:24,*EL,"Record not on file.  Try again?  Y or N":
                   *+,REPLY
         CMATCH    "Y" TO REPLY
         GOTO      DELNUM IF EQUAL
         GOTO      DELMSG

Mainten
         CHAIN     "TDSINV10"       ;; Maintenance Program....  2/1/98
         GOTO      Start

Inquiry  DISPLAY   *ES:
                   *P30:01,CompName:
                   *P14:03,"This program will inquire into a customer ":
                   *P56:03,"master record"
InqNum   KEYIN     *P10:06,*EF,"Please enter customer number:":
                   *+,*P39:06,"___",*P39:06,KEY
         CMATCH    "*" TO KEY
         GOTO      INQID  IF EOS
         GOTO      INQNUM IF EQUAL
         CMATCH    "^" TO KEY
         GOTO      INQNUM IF EQUAL
         MATCH     "999" TO KEY
         GOTO      START IF EQUAL
         TRAP      INQNOF IF RANGE
         READ      CUSTMAST,KEY;CustRcd
         CALL      INQNOF IF OVER
         TRAPCLR   RANGE
         GOTO      InqDisp

InqID
         KEYIN     *COLOR 23:
                   *P44:06,*EL,"Unique Customer Filename: ":
                   *COLOR 19:
                   *UC,*+,*P70:06,CustFNwk:
                   *LC,*P70:06,*DV,CustFNwk;
         CMATCH    " " TO CustFNwk
         GOTO      InqNum IF EOS
         RESET     CustFNwk to 8
         LENSET    CustFNwk
         RESET     CustFNwk
         MOVE      CustFNwk TO CustFN
         READ      CustFNi,CustFN;CustNum;
         GOTO      InqNOF IF OVER
         MOVE      CustNum TO KEY
         READ      CUSTMAST,KEY;CustRcd
         UNPACK    CustLInD TO WorkMM, WorkDD, WorkYYYY

InqDisp
         DISPLAY   *P10:06,*EL,"Customer number: ",CUSTNUM:
                   *P44:06,"Unique Customer Filename: ",CustFN:
                   *P44:07,"Expected Header Record:   ",CustEHdr:
                   *P10:08,"Customer name: ",CUSTNAME:
                   *P10:10,"Mailing address: ",CUSTADDR:
                   *P10:12,"City: ",CUSTCITY:
                   *P10:14,"State: ",CUSTSTE:
                   *P10:16,"Zip code: ",CUSTZIP:
                   *P10:18,"Send to attn of: ",CUSTATTN:
                   *P10:19,"Last Invoice Date: ",WorkMM,"/",WorkDD,"/",WorkYYYY:
                   *P10:20,"Telephone number: ",CUSTPHON:
                   *P10:21,"Fax phone number: ",CustFax:
                   *P50:21,"CBI?   Experian?  Trans Union?":
                   *P51:22,CustCBIf,*P61:22,CustExpf,*P74:22,CustTUf:
                   *P10:22,"Add to COMBINED tape: (Y/N) ",CustComb
         KEYIN     *+,*P10:24,*EL,"Another record?  Y or N":
                   *P35:24,"_",*P35:24,REPLY
         CMATCH    "Y" TO REPLY
         GOTO      INQNUM IF EQUAL
         GOTO      START
.
.
.
InqNOF   NORETURN
         KEYIN     *P10:24,*EL,"Record not on file.  Try again?  Y or N":
                   *+,REPLY
         CMATCH    "Y" TO REPLY
         GOTO      INQNUM IF EQUAL
         GOTO      START

NOCFFILE KEYIN     *B,*P1:24,*COLOR 124:
                   "Error opening CustFNi file.",*B,*EL,*CL,*-,REPLY;
         STOP
.
NoCTFile KEYIN     *B,*P1:24,*COLOR 124:
                   "Error opening the CTRLFILE file.",*B,*EL,*CL,*-,REPLY;
         STOP
NOFILE   KEYIN     *B,*P1:24,*COLOR 124:
                   "Error opening the CUSTMAST file.",*B,*EL,*CL,*-,REPLY;
         STOP
.
.
.
CLOSEFLE
         CLOSE     CUSTMAST
         CLOSE     CustFNi
         CHAIN     "TJONMENU"       GO BACK TO MENU     12/5...JMM
.
.
.
RANGETRP NORETURN
         KEYIN     *+,*P10:24,*EL,"Record not on file.  Try again?  Y or N":
                   *P47:24,REPLY
         CMATCH    "Y" TO REPLY
         GOTO      CHGNUM IF EQUAL
         GOTO      START
