@echo off
Rem  PRTENV.BAT [A:\]
Rem              Optional for printing from a file on floppy...
Rem    Revised:     8/05/91
Rem    Revised     11/16/92  Changed to read the new tdsenvpr.prt file that
Rem                          is created by reading the invoices printed file.
Rem    Revised:    12/05/98  Changed to used PRTENV CED synonym. Under NT
Rem                          this synonym prints to the printer as PostScript
Rem                          and under DOS it prints to the printer as HP
Rem                          and uses separate PCL codes so the text file
Rem                          can be virgin.
Rem
Rem  This batch file will be used to print envelopes on demand.
Rem

cls
sw /kyelÄ
sw /r4 /l0  "              Envelope Printing                 " "    The Laser Printer should be set-up as follows:    "
sw /r8 /l0  "Manual feed tray must have envelopes "
sw /r12 /l0 "             Is the Laser Printer READY?              "
sw /r16 /ewhi /s /bred /fYEL /gbla /tbla "If yes enter ~Y, otherwise ~N will not print envelopes" /q~yn
if errorlevel 2 goto NO
if errorlevel 1 goto YES1
if errorlevel 0 goto ESC
:YES1
sw /kblu
Echo on
if exist %1tdsenvpr.prt del %1tdsenvpr.prt
cls
:: Run program to build envelope file for printing via "PrtEnv" CED synonym
tdsenvpr
if not exist %1tdsenvpr.prt goto NFMsg
:: Changed from prthp   12/05/98
xced prtenv %1tdsenvpr.prt
del %1tdsenvpr.prt
GOTO EXIT

:NFMsg
sw /kredÄ /r12
sw /r4  "  Input file %1tdsenvpr.prt does not Exist. Please check.  "
GOTO EXIT

:ESC
:NO
sw /kblu  "Batch file terminated"
:EXIT
