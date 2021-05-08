@echo off
Rem  PRTCBM10.BAT [A:\]
Rem    Print 10 Maynard return label envelopes (No addressee on envelope)
Rem    Revised     11/01/97  Created
Rem
Rem  This batch file will be used to print envelopes on demand.
Rem
cls
sw /kyelÄ
sw /r4 /l0  "     Envelope Print from file CBMRETRN.PRT      " "    The Laser Printer should be set-up as follows:    "
sw /r8 /l0  "Manual feed tray must have envelopes "
sw /r12 /l0 "             Is the Laser Printer READY?              "
sw /r16 /ewhi /s /bred /fYEL /gbla /tbla "If yes enter ~Y, otherwise ~N will not print envelopes" /q~yn
if errorlevel 2 goto NO
if errorlevel 1 goto YES1
if errorlevel 0 goto ESC
:YES1
sw /kblu
Echo on
cls
if not exist %1cbmretrn.prt goto EXIT
xced prthp %1cbmretrn.prt
GOTO EXIT
:ESC
:NO
sw /kblu  "Batch file terminated"
:EXIT
