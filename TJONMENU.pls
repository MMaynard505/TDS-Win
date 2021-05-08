. ******************************************************************************
. *  TJONMENU      TDS Conversions Invoicing System                            *
. *  V.M=3.0                                                                   *
. *                                                                            *
. ******************************************************************************
.
. ******************************************************************************
. *  Date    *                 Modification                                    *
. ******************************************************************************
. * 01/20/99 ³ Ver 3.0 Program moved to PLBWIn source code for testing         *
. *          *                                                                 *
. *          *                                                                 *
. *          *                                                                 *
. *          *                                                                 *
. ******************************************************************************
. TJONMENU -           TDS Conversions Invoicing System
.
. This program was copied from TJONMENU program on 1/17/98
.
.
         INCLUDE   COMMON.INC
CustMast IFILE     VARIABLE=1024,NODUP,WEOF
CMKey    DIM       3
CompName DIM       25

         INCLUDE   TJONCMDS.INC

CMD      DIM       5
FORM5    FORM      5
Key      DIM       16
ProgName INIT      "TDSMenu (TJONMENU)"
Reply    DIM       1
Program  DIM       30
Spaces   INIT      "                                             "
Time     INIT      "hh:mm:ss"          hours:minutes:seconds
+................................................................
. MAINLINE
.
*................................................................
.
.
. DISPLAY THE MENU
.
         CLOCK     DATE TO TODAY
         REPLACE   "-/" IN TODAY
         CLOCK     TIME TO TIME
         TRAP      NOFILE IF IO
         OPEN      CustMast,"CUSTMAST"
         TRAPCLR   IO
         MOVE      "000" TO CMKey          ;; Get Company Info
         READ      CustMast,CMKey;CustRcd
         CLOSE     CustMast

         MOVE      CustName TO CompName    ;; Company Name

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

Start
         MOVE      Spaces TO Program
         DISPLAY   *COLOR 23,*ES:
                   *P1:1,"TDSMenu ":
                   "                  ":
                   *COLOR 19,*LL,CompName:
                   " Invoicing System":
                   *COLOR 23,*P72:1,TODAY:
                   *P72:2,TIME:
                   *P1:3,*EL:
                  *N,*EL,*N,*EL,*N,*EL,*N,*EL:
                  *N,*EL,*N,*EL,*N,*EL,*N,*EL:
                  *N,*EL,*N,*EL,*N,*EL,*N,*EL:
                  *N,*EL,*N,*EL,*N,*EL:
                  *P1:19,*EL:
                  *P1:20,"===============":
                  "===============":
                 *P33:20,"SYSTEM  MESSAGES":
                  *P51:20,"===============":
                  "================":
                   *P10:05," 1 - ":
              "Customer file maintenance":
                   *P10:06," 2 - ":
              "Invoice Data Entry":
                   *P10:07," 3 - ":
              "Spool Invoices":
                   *P10:08," 4 - ":
              "Invoice Number (CTRLFILE) Maintenance":

                   *P10:09," 5 - ":
              "Print Diskette Return Mailing Labels       ":

                   *P10:10," 6 - ":
              "Run Batch program to print spooled invoices & envelopes":
                   *P10:11," 7 - ":
              "Run Batch program to print MBE shipping form for 3 bureaus":
                   *P10:12," 8 - ":
              "Run program to create invoice print file from CSCFILES.Mxx":
                   *P10:13,"     ":
              "  (Also creates invoice import file for Peachtree First Acct)":

                   *P10:14," 9 - ":
              "Run Batch program to print invoice envelopes":
                   *P10:15,"10 - ":
              "Old TJONMENU (MENUOLD)":
                   *P10:16,"11 - ":
              "Exit to DOS":
                  *P1:21,*EF;
*................................................................
. GET THE PROGRAM'S INDEX
.
         DISPLAY  *P1:23,*EL:
                 "Select an item from the menu ":
                 "or a command from the list ":
                 "below............. _____":
                 *P31:24,*EL,"EXIT       ":
                 "Enter key";
GETINDEX KEYIN   *P76:23,"_____":
                *P76:23,CMD,*P1:21,*EL,*L,*EL;
.
         TRAP      ERROR1 IF CFAIL
         TYPE      CMD
         GOTO      CKEXIT IF NOT EQUAL
         MOVE      CMD TO FORM5
         COMPARE   "1" TO FORM5
         GOTO      ERROR1 IF LESS
         GOTO      BRANCH1
ERROR1   DISPLAY   *P1:21,*EL,*B:
                   "Invalid command - ":
                   "please try again";
         GOTO      GETINDEX
CkExit
         MATCH    "E" TO CMD
         GOTO      NextMenu IF EOS
         GOTO      Exit IF EQUAL
         MATCH     "Q" TO CMD
         GOTO      Exit IF EQUAL
         GOTO      Error1
Exit
         STOP
NEXTMENU GOTO      GETINDEX
*................................................................
. BRANCH TO THE ROUTINE INDICATED BY THE INDEX
.
BRANCH1  TRAP      BADCHAIN IF CFAIL
         MOVE      SPACES TO PROGRAM
         CLOCK     TIME TO TIME
         BRANCH    FORM5 OF TJONINV1:  1  Customer file maintenance
                            TJONINV2:  2  Invoice Data Entry
                            TJONINV3:  3  Spool Invoices
                            TDSINV10:  4  Invoice Number (CTRLFILE)
                            TDSLBLPR:  5  Print Return Mailing Labels
                            PRTINV:    6  Batch file to print spooled invoices
                            MBESHIP3:  7  Print MBE shipping form
                            INVOICE:   8  Program to prt invoices from CSCFILES
                            TDSENVPR:  9  Batch file to print invoice envelopes
                            TJONMENU: 10  Old TJON Menu
                            SHUTDOWN  11  Exit to DOS
         GOTO      GETINDEX
*................................................................
. PROGRAM DOES NOT EXIST.
.
BADCHAIN
         DISPLAY   *B,*P1:21,*EL,"This program does not exist...(":
                   PROGRAM,")";
         RETURN
*................................................................
. CHAIN INSTRUCTIONS
*................................................................
. Customer file maintenance
.
TJONINV1
         MOVE      "TJONINV1" TO PROGRAM
         CHAIN     PROGRAM
         GOTO      GETINDEX
*................................................................
. Invoice Data Entry
.
TJONINV2
         MOVE      "TJONINV2" TO PROGRAM
         CHAIN     PROGRAM
         GOTO      GETINDEX
*................................................................
. Print Invoices
.
TJONINV3
         MOVE      "TJONINV3" TO PROGRAM
         CHAIN     PROGRAM
         GOTO      GETINDEX
*................................................................
. Old TJON Menu
.
TJONMenu
         MOVE      "MENUOLD" TO PROGRAM
         CHAIN     PROGRAM
         GOTO      GETINDEX
*................................................................
. Invoice Number (CTRLFILE) Maintenance
.
TDSINV10
         MOVE      "TDSINV10" TO PROGRAM
         CHAIN     PROGRAM
         GOTO      GETINDEX
TDSEnvPr
         MOVE      "PRTENV  " TO PROGRAM
         EXECUTE   PROGRAM
         GOTO      START
TDSLBLPR
         MOVE      "C:\cscdata\PRTLABEL" TO PROGRAM
         EXECUTE   PROGRAM
         GOTO      START

INVOICE
         EXECUTE   "D:"
         EXECUTE   "CD\COMPLETE"
         EXECUTE   "DIR >NUL "
         EXECUTE   "C:"
         EXECUTE   "CD\TDS"
         MOVE      "INVOICE"  TO PROGRAM
         CHAIN     PROGRAM
         GOTO      GETINDEX
PRTINV
         MOVE      "PRTINV  " TO PROGRAM
         EXECUTE   PROGRAM
         GOTO      START
MBESHIP3
         MOVE      "MBESHIP3" TO PROGRAM
         EXECUTE   PROGRAM
         GOTO      START

NoFile   KEYIN     *B,*P1:24,*COLOR 124:
                   "Error opening the CUSTMAST file.",*B,*EL,*CL,*-,Reply;
         STOP
SHUTDOWN
         SHUTDOWN
