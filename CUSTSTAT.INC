.
. ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
. บ  CUSTSTAT.INC  Include file for record description of Customer Status File บ
. บ  V.M=1.0       Created  2/02/92                                            บ
. บ                                                                            บ
. ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
.
     As Of    Program          Tape Vol
.                 Length       From-To               Description
.                 ------       -------  -------------------------------------
CSCNbr   DIM        3     /*     1-3    Customer number    (Primary Key)
CSFName  DIM        8     /*     4-11   CSCFILES Unique filename
CSYear   DIM        2     /*    12-13   Processing Year
CSMonth  DIM        2     /*    14-15   Processing Month
CSDRDate DIM        8     /*    16-23   Date Diskettes were received at TDS
CSNbr    DIM        2     /*    24-25   Number of diskettes received
CSRunDt  DIM        8     /*    26-33   Processing Date
CSCDate  DIM        8     /*    34-41   Diskette Creation Date
CSCProg  DIM       12     /*    42-53   Program name used for processing
CSTVNCBI DIM        6     /*    54-59   CSC/CBI Tape Volume Number
CSTVNTRW DIM        6     /*    60-65   TRW     Tape Volume Number
CSTVNTU  DIM        6     /*    66-71   TU      Tape Volume Number
CSTVNOth DIM        6     /*    72-77   Other   Tape Volume Number
CSHeader DIM       20     /*    78-97   Header record found on diskettes
CSTRecs  DIM        8     /*    98-105  Actual record count on output tape
CSFiller DIM       25     /*   106-130  Filler
.                ------
.                 130 bytes in file

CustStat VARLIST CSCNbr:
                 CSFName:
                 CSYear:
                 CSMonth:
                 CSDRDate:
                 CSNbr:
                 CSRunDt:
                 CSCDate:
                 CSCProg:
                 CSTVNCBI:
                 CSTVNTRW:
                 CSTVNTU:
                 CSTVNOth:
                 CSHeader:
                 CSTRecs:
                 CSFiller

.   End of CUSTSTAT.INC อออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
