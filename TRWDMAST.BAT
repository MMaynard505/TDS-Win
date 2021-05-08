Rem
Rem   TRWDMAST.BAT
Rem        This Batch file will reorganize and reindex the
Rem        TRWDMAST file for the Tjon invoicing system.
Rem
Pause  Press the Enter key to continue
Rem
copy TRWDMAST.TXT,TRWDMAST.WRK
release relsundb
sundbsys
INDEX TRWDMAST.TXT;1-6
relsundb
del relsundb.com
del  TRWDMAST.WRK
Rem
Rem   TRWDMAST.BAT has completed
