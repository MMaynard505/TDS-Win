.  TJONINVF.INC    Include file for Invoice History records
.     V.M=1.2      9/07/90
.
InvKey   DIM       4                /* Key to Invoice file
IFKey    DIM       7                /* Key to invoice history file

.                Length   From-To          Description
.                -----    -------     --------------------------------------
IFCustID DIM       3       1-3      /* Customer number
IFInv    FORM      4       4-7      /* Invoice number
IFIDate  DIM       6       8-13     /* Invoice date (MMDDYY)
IFIAmt$  FORM      7.2    14-23     /* Invoice amount
IFPaidD  DIM       6      24-29     /* Invoice paid date (MMDDYY)
IFTaxAmt FORM      5.2    30-37     /* Sales Tax Amount Billed ..... 2/2/88..JMM
IFWO     DIM       4      38-41     /* Work Order number
IFFiller DIM       9      42-50     /* Filler    (  Unused at present  )
.               ______
.                 50  Byte record


IFRecord VARLIST   IFCustID,IFInv:
                   IFIDate,IFIAmt$,IFPaidD:
                   IFTaxAmt,IFWO:
                   IFFiller


