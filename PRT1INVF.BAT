echo off
Rem  PRT1INVF.BAT [A:]
Rem              Optional for printing from a file on floppy...
Rem
Rem
Rem
Rem  This batch file will be used to print a single invoice, or a small number
Rem  of invoices on demand, with the Tjon Letterhead used as an overlay on the
Rem  first group printed.  Be sure that macro-id 101 has been loaded to the
Rem  laser printer with:
Rem           [copy /b tjon101.ovl prn ]
Rem  This batch file will be used to "Clean-up" TJONINV3.PRT file created by
Rem  the TJON invoicing system.  This command file will remove extraneous
Rem  top of form commands, and the total summary printed at the end of the
Rem  invoicing run.  This command file should not be used to print the large
Rem  group of invoices created at end of month.
Rem
stackey !
stackey ~y "file" cr
D:\$kedit\kedit %1tjoninv3.prt
Echo on
cls
Rem Insert 8x11 plain white in Laser printer to be used with Tjon overlay.
Pause
COPY /B d:\$hplaser\$macros\TJON101.OVR PRN
xced send prn \027\038f101y4X
copy/b %1tjoninv3.prt prn
xced send prn \027\038f5X
Rem Insert 8x11 plain white in Laser printer for the Tjon internal copies.
Pause
copy/b %1tjoninv3.prt prn
xced kill %1tjoninv3.prt
