@echo off
Rem  MBESHIP3.BAT
Rem
Rem  This batch file will be used to print MBE shipping form.
Rem  This version prints for all 3 Credit Bureaus.

cls
sw /kyelÄ
sw /r4 /l0  "     MBE Shipping Form from file MBESHIP3.PRT    " "    The Laser Printer should be set-up as follows:    "
sw /r8 /l0  "MBE Shipping Form (Landscape) in Manual Tray"
sw /r12 /l0 "             Is the Laser Printer READY?              "
sw /r16 /ewhi /s /bred /fYEL /gbla /tbla "If yes enter ~Y, otherwise ~N will not print MBE Form" /q~yn
if errorlevel 2 goto NO
if errorlevel 1 goto YES1
if errorlevel 0 goto ESC
:YES1
sw /kblu
Echo on
cls
if not exist %1mbeship3.prt goto EXIT
xced prthp %1mbeship3.prt
GOTO EXIT
:ESC
:NO
sw /kblu  "Batch file terminated"
:EXIT
