.
. ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
. บ  TJONCMDS.INC  Include file for record description of Customer Master      บ
. บ  V.M=1.5       Revised  3/11/92                                            บ
. บ  V.M=1.6       Revised  1/29/96  (CustComb)                                บ
. บ  V.M=1.7       Revised  5/29/96  (CustZip changed to DIM from FORM)        บ
. บ  V.M=1.8       Revised  1/12/98  (Last Invoice Date added)                 บ
. บ  V.M=1.9       Revised  1/19/98  (Flags for CBI, Experian, Trans Union)    บ
. บ  V.M=1.A       Revised  1/31/98  Expected header (10 bytes)                บ
. บ                                                                            บ
. ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
.
.                 Length       From-To               Description
.                 ------       -------  -------------------------------------
CustNum  DIM        3     /*     1-3    Customer number    (Primary Key)
CustName DIM       25     /*     4-28   Company name
CustAddr DIM       25     /*    29-53   Address
CustCity DIM       14     /*    54-67   City
CustSte  DIM        2     /*    68-69   State
CustZip  DIM        5     /*    70-74   Zip code
CustAttn DIM       25     /*    75-99   Person to contact
CustPhon DIM       12     /*   100-111  Area code and phone number   2/9/88
CustFN   DIM        8     /*   112-119  CSCFILES unique filename (Secondary key)
CustDSN  DIM        5     /*   120-124  CSC/CBI DSN
CustExp  DIM        6     /*   125-130  Experian D-Number
CustTU   DIM       10     /*   131-140  Trans Union ID Number
CustPOB  DIM       25     /*   141-165  P.O. Box address if present
CustRunD DIM        8     /*   166-173  Our Run date of last diskette processed
CustFax  DIM       12     /*   174-185  Customer Fax number
CustCBIf DIM        1     /*   186-186  Flag if tape should be sent to CSC/CBI
CustExpf DIM        1     /*   187-187  Flag if tape should be sent to Exp
CustTUf  DIM        1     /*   188-188  Flag if tape should be sent to TU/SARMA
CustZ4   DIM        4     /*   189-192  Zip+4 if available
CustCBI$ FORM     3.2     /*   193-198  CSC/CBI  Unit price per diskette/xmit
CustExp$ FORM     3.2     /*   199-204  Experian Unit price per diskette/xmit
CustTU$  FORM     3.2     /*   205-210  TU       Unit price per diskette/xmit
CustCBIn DIM        3     /*   211-213  CSC/CBI  Customer nbr to bill
CustExpn DIM        3     /*   214-216  Experian Customer nbr to bill
CustTUn  DIM        3     /*   217-219  TU       Customer nbr to bill
CustCBIs FORM     3.2     /*   220-225  CSC/CBI  Unit price for shipping tapes
CustExps FORM     3.2     /*   226-231  Experian Unit price for shipping tapes
CustTUs  FORM     3.2     /*   232-237  TU       Unit price for shipping tapes
CustComb DIM        1     /*   238-238  Flag=Y if eligible for COMBINED tape **
CustRDt  DIM        2     /*   239-240  Date to trigger reminder if data not rcd
CustEHdr DIM       10     /*   241-250  Expected header record on diskette
CMFill1  DIM       10     /*   251-260  ----Filler----
CustERcs DIM        5     /*   261-265  Expected number of records
CustLInD DIM        8     /*   266-273  Last Invoice Date MMDDYYYY
CMFill2  DIM        7     /*   274-280  ----Filler----
.                ------
.                 280 bytes in file


CustRcd  VARLIST   CustNum:
                   CustName:
                   CustAddr:
                   CustCity:
                   CustSte:
                   CustZip:
                   CustAttn:
                   CustPhon:
                   CustFN:
                   CustDSN:
                   CustExp:
                   CustTU:
                   CustPOB:
                   CustRunD:
                   CustFax:
                   CustCBIf:
                   CustExpf:
                   CustTUf:
                   CustZ4:
                   CustCBI$:
                   CustExp$:
                   CustTU$:
                   CustCBIn:
                   CustExpn:
                   CustTUn:
                   CustCBIs:
                   CustExps:
                   CustTUs:
                   CustComb:
                   CustRDt:
                   CustEHdr:
                   CMFill1:
                   CustERcs:
                   CustLInD:
                   CMFill2

.   End of TJONCMDS.INC อออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
