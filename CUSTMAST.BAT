@Echo on
Rem
Rem   CUSTMAST.BAT
Rem        This Batch file will reorganize and reindex the
Rem        Customer Master file for the TDS invoicing system.
Rem
Pause  Press the Enter key to continue
Rem
copy CUSTMAST.TXT,CUSTMAST.WRK
FASTSORT CUSTMAST.WRK,CUSTMAST.TXT;1-3
INDEX CUSTMAST.TXT;NV1-3
INDEX CUSTMAST.TXT,CustFNi;V112-119
del  CUSTMAST.WRK
Rem
Rem  CUSTMAST.BAT has completed
