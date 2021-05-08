
  [Note... This is an obsolete record layout and is no longer used in the
           Tjon invoicing system...6/11/90...JMM]

. INVRECDS.INC     Include file for Tjon invoicing system.
.    V.M=1.3       3/17/88
.
.                Length         From-To             Description
.                ------         -------  --------------------------------------
INVNUM   FORM      4        /*    1-4    Invoice number
INVCUST  DIM       3        /*    5-7    Customer number
INVDTEBM FORM      2        /*    8-9    Invoice date (Month)
INVDTEBD FORM      2        /*   10-11                (Day)
INVDTEBY FORM      2        /*   12-13                (Year)
INVAMTB  FORM      7.2      /*   14-23   Invoice amount
INVDTEPM FORM      2        /*   24-25   Date paid    (Month)
INVDTEPD FORM      2        /*   26-27                (Day)
INVDTEPY FORM      2        /*   28-29                (Year)
INVAMTP  FORM      7.2      /*   30-39   Amount paid
INVTAX   FORM      4.2      /*   40-46   Sales Tax Amount Billed     2/02/88
INVWO    DIM       4        /*   47-50   Work Order number           3/17/88
.                -----
.                 50 bytes
.
. End of INVRECDS.INC
