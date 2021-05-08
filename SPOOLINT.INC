. SPOOLINC/INC
. This file contains the data definitions for the Datashare spool subroutine

. Definitions common to all record types

. The following definitions are used internally for linked list management,
. and are not set by the user.  If space is very tight, however, these
. variables may be used for any purpose desired, but when "SPOOLIT" is called,
. they will be destroyed on return.

SPLTYPE  FORM      1           /* RECORD TYPE
SPLFREEF FORM      5           /* FREE RECORDS POINTERS
SPLFREEB FORM      5
SPLSITEF FORM      5           /* SITE RECORDS POINTERS
SPLSITEB FORM      5
SPLPRTAF FORM      5           /* PRINTER AVAILABLE RECORD POINTERS
SPLPRTAB FORM      5
SPLPRTQF FORM      5           /* PRINT QUEUE RECORD POINTERS
SPLPRTQB FORM      5
SPLPRTCF FORM      5           /* PRINT COMPLETION QUEUE RECORD POINTERS
SPLPRTCB FORM      5
.
. Definitions for specific record types
. Record Type 1 - Site Master Record
.
SPLEXTNT FORM      5           /* LRN OF FIRST UNUSED SECTOR (EOF MARK)
SPLVRSN  FORM      2           /* QUEUE FILE VERSION
.
. Record Type 2 - Free Record
. (none)
.
. Record Type 3 - Site Master Record
.
SPLSITNM DIM       8           /* SITE NAME
.SPLSITCP FORM      3              NUMBER OF JOBS COMPLETED BY THIS SITE
.SPLUVER  DIM       1              UNSPOOL VERSION
.SPLUREL  DIM       1              UNSPOOL RELEASE
.SPLUPRE  DIM       1              UNSPOOL PRE-RELEASE LETTER

. The definitions above which have been commented out are merely for
. documentation purposes, and are not used by the subroutine.
.
. Record Type 4 - Printer Configuration Record
. (not needed for this program)
.
.FORMSA   DIM       8              FORMS TYPE CONFIGURED
.ACTIVE   FORM      5              JOB NUMBER PRINTING
ACTIVE   FORM      5     /* Reused by TJON to build spool name in SETSPLNM
.PTYPEA   DIM       8              PRINTER ATTRIBUTES
.SPEED    FORM      5              SPEED OF PRINTER
.BUSADR   DIM       4              I/O BUS ADDRESS
.PORTADR  FORM      2              PORT ADDRESS
.CWIDTH   FORM      3              COLUMN WIDTH
.PSTATUS  DIM       1              - RESERVED -
.LINEPG   FORM      3              LINES PER PAGE
.SBURSTS  FORM      3              NUMBER OF STARTING BURST LINES
.EBURSTS  FORM      3              NUMBER OF ENDING BURST LINES
.
. Record Type 5 - Print Request Record
.
SPLFORMS INIT      "        "  /* FORMS TYPE REQUIRED TO PRINT ON
SPLFILNM DIM       8           /* FILE NAME TO PRINT
SPLEXTN  INIT      "PRT"       /* FILE EXTENSION
SPLVOLID DIM       8           /* VOLUME NAME CONTAINING THE FILE
SPLUNAME DIM       8           /* USER NAME (SUBDIRECTORY) OWNING THE FILE
SPLTYPER DIM       8           /* ATTRIBUTES OF PRINTER REQUIRED TO PRINT
SPLPAGNO FORM      "       0"  /* CURRENT PAGE NUMBER PRINTING. (INIT. 0)
SPLINENO FORM      "       0"  /* CURRENT LINE NUMBER PRINTING. (INIT. 0)
SPLSTAT  INIT      "WAITING "  /* INITIAL JOB STATUS
SPLNCOPS FORM      "  1"       /* NUMBER OF COPIES TO PRINT (NORMALLY 1)
SPLWIDTH FORM      "132"       /* MINIMUM COLUMN WIDTH REQUIRED
SPLFDISP INIT      "K"         /* FILE DISPOSITION UPON COMPLETION (K/D)
SPLPRIO  FORM      " 10"       /* PRIORITY OF JOB
SPLMSGFR DIM       16          /* MESSAGE FROM OPERATOR
SPLDIST  DIM       16          /* DISTRIBUTION INFORMATION
SPLMSGTO DIM       16          /* MESSAGE TO OPERATOR
SPLSITEN DIM       8           /* SITE REQUIRED
SPLTIMQD DIM       21          /* TIME QUEUED
SPLTIMPS DIM       21          /* TIME PRINTING STARTED
.
. Other subroutine constants
.
SPLPZ    FORM      "    0"     /* NULL RECORD POINTER
SPLSITRN FORM      5           /* THAT SITE'S SITE RECORD NUMBER
SPLDIM1  DIM       1           /* Used for DIM 1 Field variable
.
. File variables
.
SPLQFILE FILE                  /* QUEUE FILE
SPLCLKFL FILE                  /* CLOCK FILE
SPLSEQ   FORM      "-1"        /* FOR SEQUENTIAL ACCESS
.
. Linked list search and management variables
.
SPLROOTP FORM      5
SPLNEXTP FORM      5
SPLTHISP FORM      5
SPLPREVP FORM      5
.
. Internal "junk" variables
.
SPLFLAG  FORM      1           /* INTERNAL FLAG
SPLPTEST FORM      3           /* TO CHECK A PRIORITY
SPLGUYBK FORM      5           /* RECORD NUMBER OF GUY IN BACK OF THIS JOB
.
. Variables which are passed back to the user for return
.
SPLRETN  FORM      1           /* RETURN STATUS - SEE SPOOLSUB FOR INFO
SPLJOBNO FORM      5           /* JOB NUMBER SPOOLED
.
. Report name and file
.
SPLFL    FILE
SPLNAME  DIM       12
.
. Variables used to get distribution info.
.
USERS    IFILE
USERNAME DIM       20
.
. End of subroutine definitions
................................................... End of SPOOLINC.TXT
