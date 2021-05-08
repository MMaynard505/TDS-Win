@Echo on
Rem
Rem   TJONINVF.BAT
Rem        This Batch file will reorganize and reindex the
Rem        Invoice History file for the TDS  invoicing system.
Rem
Pause  Press the Enter key to continue
Rem
copy TJONINVF.TXT,TJONINVF.WRK
FASTSORT TJONINVF.WRK,TJONINVF.TXT;4-7
INDEX E:\TDS-WIN\TJONINVF.TXT,E:\TDS-Win\INVOICES.ISI;NV4-7
INDEX E:\TDS-Win\TJONINVF.TXT;NV1-7
del  TJONINVF.WRK
Rem
@Echo  TJONINVF.BAT has completed
