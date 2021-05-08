. ==================================================== Start of MONTHTBL.INC
. MONTHTBL.INC  Include file for crude way to handle invoicing
.
. When this table is changed, you must recompile TJONINV3.DBS
.
MTYear   INIT      "98"
M1       INIT      "January 1998.  "
M2       INIT      "February 1998. "
M3       INIT      "March 1998.    "
M4       INIT      "April 1998.    "
M5       INIT      "May 1998.      "
M6       INIT      "June 1998.     "
M7       INIT      "July 1998.     "
M8       INIT      "August 1998.   "
M9       INIT      "September 1998."
M10      INIT      "October 1998.  "
M11      INIT      "November 1998. "
M12      INIT      "December 1998. "

. The following codes should stay UC. They are used in conjunction with
. other variables to build job codes for the PeachTree First Accounting
. interface to the TDS Invoicing system.
.
MMM1     INIT      "JAN"
MMM2     INIT      "FEB"
MMM3     INIT      "MAR"
MMM4     INIT      "APR"
MMM5     INIT      "MAY"
MMM6     INIT      "JUN"
MMM7     INIT      "JUL"
MMM8     INIT      "AUG"
MMM9     INIT      "SEP"
MMM10    INIT      "OCT"
MMM11    INIT      "NOV"
MMM12    INIT      "DEC"

. ====================================================== End of MONTHTBL.INC
