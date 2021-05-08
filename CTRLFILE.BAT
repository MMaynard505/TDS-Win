Rem
Rem   CTRLFILE.BAT
Rem        This Batch file will reorganize and reindex the
Rem        Control file for the Tjon invoicing system.
Rem
Pause  Press the Enter key to continue
Rem
copy CTRLFILE.TXT,CTRLFILE.WRK
sundbsys
INDEX CTRLFILE.TXT;1-12
del  CTRLFILE.WRK
Rem
Rem   CTRLFILE.BAT has completed
