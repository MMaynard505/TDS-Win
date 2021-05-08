@echo off
Rem  PRT1ENV.BAT [A:\]
Rem              Optional for printing from a file on floppy...
Rem    Revised:     8/05/91
Rem    Revised     11/16/92  Changed to read the new tdsenvpr.prt file that
Rem                          is created by reading the invoices printed file.
Rem
Rem  This batch file will be used to print 1 envelope on demand.
Rem

cls
sw /kyelÄ
sw /r4 /l0  "     Envelope Print from file PRT1ENV.PRT       " "    The Laser Printer should be set-up as follows:    "
sw /r8 /l0  "Manual feed tray must have one envelope "
sw /r12 /l0 "             Is the Laser Printer READY?              "
sw /r16 /ewhi /s /bred /fYEL /gbla /tbla "If yes enter ~Y, otherwise ~N will not print envelopes" /q~yn
if errorlevel 2 goto NO
if errorlevel 1 goto YES1
if errorlevel 0 goto ESC
:YES1
sw /kblu
Echo on
cls
Echo OS is %OS%
IF /%OS%/==/Windows_NT/ goto NT

if not exist %1prt1env.prt goto NFMsg
xced prtenv %1prt1env.prt
Rem pdel %1prt1env.prt
GOTO EXIT
:: Special coding for printing under Win NT
:: Actually the only change is that we're now using a different synonym
:: under NT. The filename is back to the original after some testing proved
:: that we don't need 2 different files. The 'special' NT code is not
:: needed but has been left in as a model for future NT specific batch
:: files for DOS programs.
:NT
if not exist %1prt1env.prt goto NT-NFMsg
xced prtenv %1prt1env.prt
Rem pdel %1prt1env.prt
GOTO EXIT

:NT-NFMsg
sw /kredÄ /r12
sw /r4  "  Input file %1prt1enNT.prt does not Exist. Please check.  "
GOTO EXIT

:NFMsg
sw /kredÄ /r12
sw /r4  "  Input file %1prt1env.prt does not Exist. Please check.  "
GOTO EXIT

:ESC
:NO
sw /kblu  "Batch file terminated"
:EXIT
