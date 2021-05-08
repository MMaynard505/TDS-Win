. TIMERCDS.INC    Include file for record description of invoice detail records
.
.     V.M=1.1     Revised   3/17/88
.     V.M=1.2     Revised   1/14/91  Changed TX Sales tax to 8¬% from 8«% (JMM)
.     V.M=1.3     Revised   6/21/95  Changed WY
.     V.M=1.4     Revised   1/10/98  Changed back to NM

.                 Length       From-To               Description
.                 ------       -------   -------------------------------------
ICust    DIM       3      /*     1-3     Customer number
IInvNum  DIM       4      /*     4-7     Invoice number
IWrkDate FORM      6      /*     8-13    Date
IWO      DIM       4      /*    14-17    Changed to Work Order Nbr   3/17/88
IQuan    FORM      7.2    /*    18-27    Quantity
IType    DIM       6      /*    28-33    Type of service (i.e. tape, disk, etc)
IDesc1   DIM       28     /*    34-61    Description line 1
IDesc2   DIM       28     /*    62-89    Description line 2
IUPrice  FORM      4.4    /*    90-98    Unit price
IExtCost FORM      5.2    /*    99-106   Extended cost
ITaxFlag DIM       1      /*   107-107   T=Subject to state sales tax
.                ------
.                 107 bytes in file

TimeRcd  VARLIST   ICust,IInvNum,IWrkDate,IWO,IQuan,IType:
                   IDesc1,IDesc2,IUPrice,IExtCost,ITaxFlag

TaxRate  FORM    "0.00000"          ;; New Mexico State Sales Tax Rate 1/10/98
TaxState INIT   "NM"                ;; State Code
. TaxRate  FORM    "0.08250"          ;; Texas State Sales Tax Rate  1/14/91
. TaxState INIT   "TX"                ;; State Code
. TAXRATE  FORM    "0.00000"          ;; NM    State Sales Tax Rate
. TAXSTATE INIT   "NM"                ;; State Code
. TAXRATE  FORM    "0.00000"          ;; WY    State Sales Tax Rate
. TAXSTATE INIT   "WY"                ;; State Code

