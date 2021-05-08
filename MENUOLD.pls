. TJONMENU -           TDS Conversions Invoicing System
.
. THIS PROGRAM WAS GENERATED USING THE "MENUGEN" PROGRAM
.
.
         INCLUDE   COMMON/INC
CMD      DIM       5
FORM5    FORM      5
KEY      DIM       16
PROGNAME INIT      "TJONMENU "
BEEP     EQU       007
PROGRAM  DIM       30
SPACES   INIT      "                                             "
TIME     INIT      "hh:mm:ss"          hours:minutes:seconds
+................................................................
. MAINLINE
.
*................................................................
.
.
. DISPLAY THE MENU
.
START
         MOVE      SPACES TO PROGRAM
         DISPLAY   *COLOR 23,*ES:
                   *P1:1,"TJONMENU ":
                   "                  ":
                   *COLOR 19,"TDS Conversions Invoicing System":
                   *COLOR 23,*P72:1,TODAY:
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
              "Print Invoice Summary of All Invoices in History File":
                   *P10:09," 5 - ":
              "Invoice Number (CTRLFILE) Maintenance":
                   *P10:10," 6 - ":
              "Invoice History (TJONINVF) File Maintenance":
                   *P10:11," 7 - ":
              "Print Summary of Unpaid Invoices":
                   *P10:12," 8 - ":
              "Print Envelopes for Invoices              ":
                   *P10:13," 9 - ":
              "Print Diskette Return Mailing Labels       ":
                   *P10:14,"10 - ":
              "Print Invoice Summary by Customer Number":
                   *P10:15,"11 - ":
              "Program Selection by Name":
                   *P10:16,"12 - ":
              "Run Batch program to print spooled invoices & envelopes":
                   *P10:17,"13 - ":
              "Run Batch program to print MBE shipping form for 3 bureaus":
                   *P10:18,"14 - ":
              "Run program to create invoice print file from CSCFILES.Mxx":
                   *P10:19,"15 - ":
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
CKEXIT   CMATCH    "E" TO CMD
         GOTO      NEXTMENU IF EOS
         GOTO      ERROR1   IF NOT EQUAL
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
                            TJONINV4:  4  Print Invoice Journal
                            TDSINV10:  5  Invoice Number (CTRLFILE)
                            TJONINV6:  6  Invoice History File Maintenance
                            TJONINV9:  7  Print Invoice History Status
                            TDSENVPR:  8  Print Envelopes for Invoices
                            TDSLBLPR:  9  Print Return Mailing Labels
                            TJONINVX: 10  Print Invoice Summary by Cust Nbr
                            SELECT:   11  Select a program name
                            PRTINV:   12  Batch file to print spooled invoices
                            MBESHIP3: 13  Print MBE shipping form
                            INVOICE:  14  Program to prt invoices from CSCFILES
                            SHUTDOWN  15  Exit to DOS
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
. Print Invoice Journal
.
TJONINV4
         MOVE      "TJONINV4" TO PROGRAM
         CHAIN     PROGRAM
         GOTO      GETINDEX
*................................................................
. Invoice Number (CTRLFILE) Maintenance
.
TDSINV10
         MOVE      "TDSINV10" TO PROGRAM
         CHAIN     PROGRAM
         GOTO      GETINDEX
TJONINV6
         MOVE      "TJONINV6" TO PROGRAM
         CHAIN     PROGRAM
         GOTO      GETINDEX
TJONINV9
         MOVE      "TJONINV9" TO PROGRAM
         CHAIN     PROGRAM
         GOTO      GETINDEX
TDSENVPR
         MOVE      "PRTENV  " TO PROGRAM
         EXECUTE   PROGRAM
         GOTO      START
TDSLBLPR
         MOVE      "C:\cscdata\PRTLABEL" TO PROGRAM
         EXECUTE   PROGRAM
         GOTO      START
TJONINV8
         MOVE      "TJONINV8" TO PROGRAM
         CHAIN     PROGRAM
         GOTO      GETINDEX
TJONINVX
         MOVE      "TJONINVX" TO PROGRAM
         CHAIN     PROGRAM
         GOTO      GETINDEX
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
SHUTDOWN
         SHUTDOWN
.
         GOTO      START
*................................................................
. Program Selection by Name
.
SELECT
         KEYIN     *P1:23:
                   *EL,*P30:23,"Enter the Program Name..............________":
                   *P1:24,*EL:
                   *P66:23,PROGRAM,*P1:21,*EL,*N,*EL;
         CMATCH    "*" TO PROGRAM
         GOTO      PROGEXIT IF EOS
         GOTO      PROGEXIT IF EQUAL
         MATCH     "ANSWER" TO PROGRAM
         GOTO      GETINDEX IF EQUAL
         MATCH     "MASTER" TO PROGRAM
         GOTO      GETINDEX IF EQUAL
         TRAP      BADPROG IF CFAIL
         CHAIN     PROGRAM
BADPROG
         DISPLAY   *B,*P1:21,*EL,"Program does not exist (",PROGRAM,")";
         NORETURN
         GOTO      SELECT
PROGEXIT
         GOTO      START
