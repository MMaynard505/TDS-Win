: TestFax.BAT   Batch file to test QFax program
:start
@echo off
REM
REM Batchfile to send Faxes in DOS using QFax program
REM
Echo Usage: qfax [-opt] filename 292-5008
REM               !       !           !
REM               !       !           Number to dial
REM               !       !
REM               !       File to Send
REM               !
REM               Options (See USAGE.TXT)
REM

C:\QFax\QFax -cfg Strings.CFG -r -def TVOL 654321 COVR-TRW.TXT 292-5008

IF ERRORLEVEL == 255 goto notfound
IF ERRORLEVEL == 12 goto timeout
IF ERRORLEVEL == 11 goto busy
IF ERRORLEVEL == 10 goto nodialtone
IF ERRORLEVEL == 6 goto noport
IF ERRORLEVEL == 5 goto timeout
IF ERRORLEVEL == 4 goto timeout
IF ERRORLEVEL == 3 goto nocarrier
IF ERRORLEVEL == 2 goto lostcarrier
IF ERRORLEVEL == 1 goto lostfax
:ok
Echo Transmission ok!
Echo Insert your commands in the Batchfile here
goto EXIT

:notfound
Echo File not found
Echo Insert your commands in the Batchfile here
goto EXIT

:lostcarrier
Echo Carrier lost while the fax was sending!
Echo Insert your commands in the Batchfile here
goto EXIT

:timeout
Echo Modem not responding!
Echo Insert your commands in the Batchfile here
goto EXIT

:noport
Echo Port is not Avail!
Echo Insert your commands in the Batchfile here
goto EXIT

:busy
Echo Line is busy
Echo Insert your commands in the Batchfile here
REM choice /T:J,30   (30 Seconds delay)
REM goto start
goto EXIT

:nodialtone
Echo No Dialtone detected
Echo Insert your commands in the Batchfile here
REM choice /T:J,30   (30 Seconds delay)
REM goto start
goto EXIT

:nocarrier
Echo No carrier can be detected. Possible no fax at that number?
Echo Insert your commands in the Batchfile here
REM choice /T:J,30   (30 Seconds delay)
REM goto start
goto EXIT

:EXIT
Echo ErrorLevel=%ERRORLEVEL%
Echo TestFax.bat finished.
