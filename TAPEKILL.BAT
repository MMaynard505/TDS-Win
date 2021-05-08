@echo off
:: TapeKill.BAT  Batch file to kill off any information on 9-track tape
:: This version uses NovaStor utility TPU.EXE
::  Usage:    TapeKill filename
::            where   %1 is the filename to write to tape
::                    %2 is tape volid  (Default VolID=111111)
::
if /%1==/  goto NOPARM1
set IFN=%1

if /%2==/  goto USETVN
set DFN=111111
GOTO CKCOMP

:USETVN
set DFN=%2

:CKCOMB

Echo Initializing 9-trk tape

: set TAPEUNIT=27        (M4 9-Track tape unit, NovaStore TPU Default)
@Echo ON
set TAPEUNIT=27
C:\ntape\TPU REW ;
C:\ntape\TPU INT SN%DFN% OWN"TDS.ABQ.NM" IBM ;
C:\ntape\TPU DN"%IFN%" TN"TDS.Conversions" DOS RPD030 DLT XA TFF DFV BL440 RL440 ;
C:\ntape\TPU ULD ;
GOTO END

:NOPARM1
echo ERROR:  Filename of file to write to tape is missing
@echo  Usage:    TapeKill filename [nnnnnn]
@echo   where            filename   TapeVol
goto END

:PARM3ERR
echo Entered parameter 3 (%3) not equal COMBINED and not null
beep
goto END

:PARM4ERR
echo If COMBINED tape then As-of-Date is required to follow the COMBINED flag
beep
goto END

:END
