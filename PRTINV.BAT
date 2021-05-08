@echo off
Rem  PRTINV.BAT [A:\]
Rem              Optional for printing from a file on floppy...
Rem    Revised:     7/05/96
Rem
Rem
Rem
Rem  This batch file will be used to print invoices on demand.
Rem  This batch file will be used to "Clean-up" TJONINV3.PRT file created by
Rem  the TJON invoicing system.  This command file will remove extraneous
Rem  top of form commands, and the total summary printed at the end of the
Rem  invoicing run.  This command file can be used to print the large
Rem  group of invoices created at end of month.
Rem

cls
sw /kyelÄ
sw /r4 /l0  "               Invoice Printing                 " "    The Laser Printer should be set-up as follows:    "
sw /r8 /l0  "Primary tray must have TDS letterhead (8« x 11 sheets)"
sw /r12 /l0 "             Is the Laser Printer READY?              "
sw /r16 /ewhi /s /bred /fYEL /gbla /tbla "If yes enter ~Y, otherwise ~N will bypass letterhead" /q~yn
if errorlevel 2 goto NO1
if errorlevel 1 goto YES1
if errorlevel 0 goto ESC
:YES1
sw /kblu
stackey !
Rem stackey ~y "file" cr         (Removed the ~y that deletes total page)
Rem xced kedit %1tjoninv3.prt
Echo on
cls
xced LR
xced prthp %1tjoninv3.prt

:NO1
@echo off
cls
sw /kyelÄ
sw /r4 /l0  "               Invoice Printing                 " "    The Laser Printer should be set-up as follows:    "
sw /r8 /l0  "Primary tray must have plain white 8« x 11 sheets"
sw /r12 /l0 "             Is the Laser Printer READY?              "
sw /r16 /ewhi /s /bred /fYEL /gbla /tbla "If yes enter ~Y, otherwise ~N will bypass plain sheets" /q~yn
if errorlevel 2 goto NO2
if errorlevel 1 goto YES2
if errorlevel 0 goto ESC
:YES2
sw /kblu
Echo on
xced prthp %1tjoninv3.prt
Rem create envelope spool file (tdsenvpr.prt),
Rem print tdsenvpr.prt & delete tdsenvpr.prt file
:NO2
@echo off
cls
sw /kyelÄ
sw /r4 /l0  "               Envelope Printing                " "    The Laser Printer should be set-up as follows:    "
sw /r8 /l0  "Envelopes fed into manual tray"
sw /r12 /l0 "             Is the Laser Printer READY?              "
sw /r16 /ewhi /s /bred /fYEL /gbla /tbla "If yes enter ~Y, otherwise ~N will bypass envelopes" /q~yn
if errorlevel 2 goto NO3
if errorlevel 1 goto YES3
if errorlevel 0 goto ESC

:YES3
sw /kblu
Echo on
call prtenv
xced move %1tjoninv3.prt D:\complete

:NO3
@echo off
cls
sw /kyelÄ
sw /r4 /l0  "     Parcel Shipping Order Printing   " "    The Laser Printer should be set-up as follows:    "
sw /r8 /l0  "Parcel Shipping Form in  manual tray"
sw /r12 /l0 "             Is the Laser Printer READY?              "
sw /r16 /ewhi /s /bred /fYEL /gbla /tbla "If yes enter ~Y, otherwise ~N will bypass shipping form" /q~yn
if errorlevel 2 goto NO4
if errorlevel 1 goto YES4
if errorlevel 0 goto ESC

:YES4
sw /kblu
Echo on
call mbeship3
:NO4
GOTO EXIT

:ESC
sw /kblu  "Batch file terminated"
GOTO EXIT
:EXIT
Echo on
Echo PRTINV.BAT completed
