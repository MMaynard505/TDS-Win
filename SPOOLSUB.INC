. SPOOLSUB.TXT  V.M=1.2            22 June 87
. Datashare Spool Subroutine
. This file is included in the mainline area of any Datashare program.
. It provides a mechanism for adding a job to the unspooler queue.
.
************************************************************************
* --Date--  ----------------------Modification------------------------ *
* 06/22/87  Routine modified to remove extraneous TOF records to keep  *
*           all of the blank sheets from the files printed on the      *
*           Laser printer...JMM                                        *
*                                                                      *
*                                                                      *
*                                                                      *
************************************************************************
. The label "SPOOLIT" is the program entry point, and is called by the
. user program.  This is called only after all variables have been
. initialized to their proper values.  Upon return, the variable
. "SPLRETN" contains the status of the spool.  These are:

. 0 - Successful Spool.  The variable "SPLJOBNO" contains the job number
.     of the job in the queue, for later inspection.
. 1 - Queue file not found.  No queuing took place.
. 2 - Site not found.  The site name specified could not be found in the
.     queue.  No queuing took place.
. 3 - Clock file error.  The file ARCCLOCK/TXT could not be found, or
.     contained an invalid date.  The job was queued anyway, and "SPLJOBNO"
.     contains the job number, except that the "time queued" was set to
.     00:00:00, 00/00/00.
. 4 - Queue file error.  The queue file contains an unrecoverable error,
.     such as format, or an invalid version.  No spooling took place.
. 5 - Invalid specifications.  One of the specification variables was
.     invalid.  For example, the file name given was blank.

. This routine may also be used to perform other functions.  For example,
. it may be used to simply look up a site, or just see if the queue file
. exists.
. the spool program performs the following steps, in sequence:

. 1. Find the queue file.
. 2. Look up the site specified.
. 3. Check user fields for correct format.
. 4. Get the time and date from the ARC clock file.
. 5. Queue the job.
. 6. Return to user.

. If the user wishes to simply locate the queue file, he may call
. the program with a site name of all blanks, call SPOOLIT, and see if
. the status returned is "Site Not Found," rather than "Queue File Not
. Found."  If the site is to be looked up, simply set the file name to
. all blanks, and call "SPOOLIT."  If the status returned is invalid
. user fields, rather than site not found, then the site desired exists.

. Note on usage of labels:  all internal labels begin with the
. letters "SPL" in order to avoid conflict with the user's own program labels.
. the exception to this is the entry point "SPOOLIT".
+
. First thing to do after opening the file is to check every pointer and
. make sure the queue is all safe.

SPOOLIT  MOVE      "0" TO SPLRETN
         TRAP      SPLBADFQ IF IO
         OPEN      SPLQFILE,"SPOOLPRT/TXT"
         COMPARE   "0" TO SPLRETN
         RETURN    IF NOT EQUAL
         TRAP      SPLBADFQ IF FORMAT
         READ      SPLQFILE,SPLSEQ;SPLTYPE,SPLFREEF,SPLFREEB,SPLSITEF,SPLSITEB:
                   SPLPRTAF,SPLPRTAB,SPLPRTQF,SPLPRTQB,SPLPRTCF,SPLPRTCB:
                   SPLEXTNT,SPLVRSN
         COMPARE   "0" TO SPLRETN
         RETURN    IF NOT EQUAL
         COMPARE   "1" TO SPLTYPE
         GOTO      SPLBADFQ IF NOT EQUAL
         COMPARE   "0" TO SPLFREEF
         GOTO      SPLBADFQ IF LESS
         COMPARE   "0" TO SPLFREEB
         GOTO      SPLBADFQ IF LESS
         COMPARE   "0" TO SPLSITEF
         GOTO      SPLBADFQ IF LESS
         COMPARE   "0" TO SPLSITEB
         GOTO      SPLBADFQ IF LESS
         COMPARE   "0" TO SPLPRTAF
         GOTO      SPLBADFQ IF LESS
         COMPARE   "0" TO SPLPRTAB
         GOTO      SPLBADFQ IF LESS
         COMPARE   "0" TO SPLPRTQF
         GOTO      SPLBADFQ IF LESS
         COMPARE   "0" TO SPLPRTQB
         GOTO      SPLBADFQ IF LESS
         COMPARE   "0" TO SPLPRTCF
         GOTO      SPLBADFQ IF LESS
         COMPARE   "0" TO SPLPRTCB
         GOTO      SPLBADFQ IF LESS
         COMPARE   "1" TO SPLEXTNT
         GOTO      SPLBADFQ IF LESS
         COMPARE   SPLEXTNT TO SPLFREEF
         GOTO      SPLBADFQ IF NOT LESS
         COMPARE   SPLEXTNT TO SPLFREEB
         GOTO      SPLBADFQ IF NOT LESS
         COMPARE   SPLEXTNT TO SPLSITEF
         GOTO      SPLBADFQ IF NOT LESS
         COMPARE   SPLEXTNT TO SPLSITEB
         GOTO      SPLBADFQ IF NOT LESS
         COMPARE   SPLEXTNT TO SPLPRTAF
         GOTO      SPLBADFQ IF NOT LESS
         COMPARE   SPLEXTNT TO SPLPRTAB
         GOTO      SPLBADFQ IF NOT LESS
         COMPARE   SPLEXTNT TO SPLPRTQF
         GOTO      SPLBADFQ IF NOT LESS
         COMPARE   SPLEXTNT TO SPLPRTQB
         GOTO      SPLBADFQ IF NOT LESS
         COMPARE   SPLEXTNT TO SPLPRTCF
         GOTO      SPLBADFQ IF NOT LESS
         COMPARE   SPLEXTNT TO SPLPRTCB
         GOTO      SPLBADFQ IF NOT LESS
         COMPARE   "1" TO SPLVRSN
         GOTO      SPLBADFQ IF NOT EQUAL
         COMPARE   "0" TO SPLRETN
         RETURN    IF NOT EQUAL

. First, look up the specified site name.  If it exists, save it in
. SPLSITRN.

         MOVE      "0" TO SPLROOTP
         MOVE      "0" TO SPLTHISP
         MOVE      "0" TO SPLPREVP
         RESET     SPLSITEN
         MATCH     "        " TO SPLSITEN
         GOTO      SPLINVST IF EQUAL
.
SPLSITLP FILEPI    20;SPLQFILE
         READ      SPLQFILE,SPLTHISP;SPLTYPE,*12,SPLNEXTP
         COMPARE   "0" TO SPLRETN
         RETURN    IF NOT EQUAL
         COMPARE   "1" TO SPLTYPE
         GOTO      SPLNOS1 IF EQUAL
         COMPARE   "3" TO SPLTYPE
         GOTO      SPLOHMY IF NOT EQUAL
         READ      SPLQFILE,SPLTHISP;*52,SPLSITNM
         MATCH     SPLSITNM TO SPLSITEN
         GOTO      SPLGOTST IF EQUAL
SPLNOS1  MOVE      SPLNEXTP TO SPLTHISP
         PI        0
         COMPARE   SPLTHISP TO SPLROOTP
         GOTO      SPLSITLP IF NOT EQUAL
SPLINVST MOVE      "2" TO SPLRETN
         RETURN

SPLGOTST MOVE      SPLTHISP TO SPLSITRN

. Now, check the fields for validity

         RESET     SPLFILNM
         RESET     SPLEXTN
         RESET     SPLVOLID
         RESET     SPLUNAME
         RESET     SPLFDISP
         RESET     SPLSTAT
         MATCH     "        " TO SPLFILNM
         GOTO      SPLBADST IF EQUAL
         MATCH     "        " TO SPLVOLID
         GOTO      SPLBADST IF EQUAL
         MATCH     "   " TO SPLEXTN
         GOTO      SPLBADST IF EQUAL
         MATCH     "        " TO SPLUNAME
         GOTO      SPLBADST IF EQUAL
         CMATCH    "D" TO SPLFDISP
         GOTO      SPLGD1 IF EQUAL
         CMATCH    "K" TO SPLFDISP
         GOTO      SPLBADST IF NOT EQUAL
SPLGD1   MATCH     "WAITING " TO SPLSTAT
         GOTO      SPLGD2 IF EQUAL
         MATCH     "IN HOLD " TO SPLSTAT
         GOTO      SPLBADST IF NOT EQUAL

. First, find the specified priority
. This is done by searching forward through the queue until a job is found
. containing a priority less than my own.  The guy with the lesser
. priority will go right behind me in the queue.

SPLGD2   MOVE      SPLSITRN TO SPLTHISP

SPLFNDP  PI        0
         FILEPI    20;SPLQFILE
         READ      SPLQFILE,SPLTHISP;SPLTYPE,SPLFREEF,SPLFREEB,SPLSITEF:
                   SPLSITEB,SPLPRTAF,SPLPRTAB,SPLPRTQF,SPLPRTQB
         COMPARE   "0" TO SPLRETN
         RETURN    IF NOT EQUAL
         COMPARE   "3" TO SPLTYPE
         GOTO      SPLITS IF EQUAL
         COMPARE   "5" TO SPLTYPE
         GOTO      SPLOHMY IF NOT EQUAL
         READ      SPLQFILE,SPLTHISP;*126,SPLPTEST
         COMPARE   "0" TO SPLRETN
         RETURN    IF NOT EQUAL
         COMPARE   SPLPRIO TO SPLPTEST
         GOTO      SPLGOTP IF LESS
SPLITS   MOVE      SPLPRTQF TO SPLTHISP
         COMPARE   SPLTHISP TO SPLSITRN
         GOTO      SPLFNDP IF NOT EQUAL

. Got the priority.  SPLTHISP contains the guy to go in back of me

SPLGOTP  PI        0
         MOVE      SPLTHISP TO SPLGUYBK

. Now, get a free record, and write my stuff into it

         FILEPI    20;SPLQFILE
         READ      SPLQFILE,SPLPZ;*2,SPLFREEF,SPLFREEB,*52,SPLEXTNT
         COMPARE   "0" TO SPLRETN
         RETURN    IF NOT EQUAL
         COMPARE   "0" TO SPLFREEF
         GOTO      SPLGOTFR IF NOT EQUAL
         MOVE      SPLEXTNT TO SPLTHISP
         ADD       "1" TO SPLEXTNT
         WEOF      SPLQFILE,SPLEXTNT
         COMPARE   "0" TO SPLRETN
         RETURN    IF NOT EQUAL
         WRITAB    SPLQFILE,SPLPZ;*52,SPLEXTNT
         COMPARE   "0" TO SPLRETN
         RETURN    IF NOT EQUAL
         PI        0
         GOTO      SPLGOTIT

. Now, I have a free record.  Get the time of day, to be put in also

SPLGOTFR MOVE      SPLFREEF TO SPLTHISP
         READ      SPLQFILE,SPLFREEF;*2,SPLFREEF
         COMPARE   "0" TO SPLRETN
         RETURN    IF NOT EQUAL
         WRITAB    SPLQFILE,SPLPZ;*2,SPLFREEF
         COMPARE   "0" TO SPLRETN
         RETURN    IF NOT EQUAL
         WRITAB    SPLQFILE,SPLFREEF;*7,SPLPZ
         COMPARE   "0" TO SPLRETN
         RETURN    IF NOT EQUAL
         PI        0

SPLGOTIT TRAP      SPLNOCLK IF IO
         TRAP      SPLNOCLK IF FORMAT
         OPEN      SPLCLKFL,"ARCCLOCK/TXT"
         COMPARE   "0" TO SPLRETN
         GOTO      SPLCONT IF NOT EQUAL
         READ      SPLCLKFL,SPLSEQ;SPLTIMQD
         COMPARE   "0" TO SPLRETN
         GOTO      SPLCONT IF NOT EQUAL
         CLOSE     SPLCLKFL

SPLCONT  TRAP      SPLBADFQ IF IO
         TRAP      SPLBADFQ IF FORMAT

         FILEPI    4;SPLQFILE
         READ      SPLQFILE,SPLGUYBK;SPLTYPE,SPLFREEF,SPLFREEB,SPLSITEF:
                   SPLSITEB,SPLPRTAF,SPLPRTAB,SPLPRTQF,SPLPRTQB
         COMPARE   "0" TO SPLRETN
         RETURN    IF NOT EQUAL
         WRITE     SPLQFILE,SPLTHISP;"5",SPLPZ,SPLPZ,SPLPZ,SPLPZ,SPLPZ,SPLPZ:
                   SPLGUYBK,SPLPRTQB,SPLPZ,SPLPZ,SPLFORMS,SPLFILNM,SPLEXTN:
                   SPLVOLID,SPLUNAME,SPLTYPER,SPLPAGNO,SPLINENO,SPLSTAT:
                   SPLNCOPS,SPLWIDTH,SPLFDISP,SPLPRIO,SPLMSGFR,SPLDIST:
                   SPLMSGTO,SPLSITEN,SPLTIMQD,SPLTIMPS
         COMPARE   "0" TO SPLRETN
         RETURN    IF NOT EQUAL

. Now, update the pointers of the guy in back, and his pointer to the
. guy in front of me.

         WRITAB    SPLQFILE,SPLGUYBK;*37,SPLTHISP
         COMPARE   "0" TO SPLRETN
         RETURN    IF NOT EQUAL
         WRITAB    SPLQFILE,SPLPRTQB;*32,SPLTHISP
         COMPARE   "0" TO SPLRETN
         RETURN    IF NOT EQUAL

.  Now tell the user that his print request is queued and give him his assigned
.  print job number (for use in later inquiries).

         CLOSE     SPLQFILE
         MOVE      SPLTHISP TO SPLJOBNO
         COMPARE   "0" TO SPLFLAG
         GOTO      SPLEXIT IF EQUAL        /*  Normal Exit with SPLRETN=0
. ****** RETURN    IF EQUAL                /*  Fall thru to Error Rtn Code=3
         MOVE      "3" TO SPLRETN
         RETURN
.  Normal exit, Return Code=0
SPLEXIT
. ***************************************************************************
.
.  This code was added 6/22/87 to read the Spooled Data file and remove
.  extraneous Top of Forms at the front and the end of the spool file.
.
. ***************************************************************************
         TRAP      SPLBADST IF IO    /* File not found is a specification Error
         OPEN      SPLFL,SPLNAME     /*
         COMPARE   "0" TO SPLRETN    /*
         RETURN    IF NOT EQUAL      /*
         MOVE      "1" TO SPLPTEST   /* Used as a tab character for reads
         READ      SPLFL,SPLPZ;*SPLPTEST,SPLDIM1;
         CMATCH    003 TO SPLDIM1    /* Is this a /PRT format file?
         GOTO      SPLCKR1 IF NOT EQUAL /* No, don't check the last record
         ADD       "1" TO SPLPTEST   /* Bump past EOS to the '*' character
         READ      SPLFL,SPLPZ;*SPLPTEST,SPLDIM1,SPLPTEST,SPLGUYBK;
         CMATCH    "*" TO SPLDIM1    /* Is this a "/PRT" format file?
         GOTO      SPLCKR1 IF NOT EQUAL /* No, don't check the last record
         MOVE      "11" TO SPLPTEST  /* Set up to read req'd "DSPORT" constant
         READ      SPLFL,SPLPZ;*SPLPTEST,SPLUNAME
         MATCH     "DSPORT" TO SPLUNAME
         GOTO      SPLCKR1 IF NOT EQUAL
         COMPARE   "2" TO SPLGUYBK
         GOTO      SPLCKR1 IF NOT LESS
. If all the above checks still leave us here, then we must have a regulation
. Spool file with the EOF sector in SPLGUYBK, so we will back up to the last
. used sector and scan for two trailing TOF records.
         SUBTRACT  "1" FROM SPLGUYBK
         MOVE      "0" TO SPLPTEST         /* Random read to find EOS
SPLLOOP
         ADD       "1" TO SPLPTEST         /* Bump to the next byte in file
         READ      SPLFL,SPLGUYBK;*SPLPTEST,SPLDIM1;
         GOTO      SPLEOF2 IF OVER
         CMATCH    003 TO SPLDIM1
         GOTO      SPLLOOP IF NOT EQUAL    /* Scan to the EOS
. We have now found the EOS character and need to back up to find any trailing
. TOF records, and clear them to 'Print with no spacing' (CC=0)

. Coding was stopped here...............JMM




SPLRETN  RETURN
+ Error Routines

.  The user gave some bad options.  For example, the file name, extension,
.  volid, or user name was blank!

SPLBADST MOVE      "5" TO SPLRETN
         RETURN

. The following routine is executed when an unexpected condition arises,
. caused by a messup in the queue file.  For example, a site master record
. has the wrong record type.

SPLOHMY  MOVE      "4" TO SPLRETN
         RETURN

. The following routine is called when the queue file can not be found.

SPLNOFQF MOVE      "1" TO SPLRETN
         RETURN

.  The following routine handles the exceptional condition when the
.  queue file exists but appears to be in bad format.

SPLBADFQ MOVE      "4" TO SPLRETN
         RETURN

. This routine is executed if an error occures while I am trying to get the
. current time and date to put in the job record.  This error is an exception,
. in that it does not abort, but merely puts in a time of all zeroes, and
. continues.  When the subroutine returns to the user, the job will be queued
. as normal, but the status will be returned as a "3".

SPLNOCLK MOVE      "  0  0  0  0  0  0  0" TO SPLTIMQD
         MOVE      "1" TO SPLFLAG
         RETURN
