.
.   GETDATEW   V.M=1.0      (10/13/82...JMM)
.
ARCCLOCK FILE                   /* GET THE DATE FROM HERE IF FILE IS PRESENT
JULEDATE FORM      5            /* JULIAN DATE YYDDD
JULEYEAR FORM      2            /* JULIAN YEAR
JULEDAY  FORM      3            /* JULIAN DAY OF THE YEAR
GREGDATE INIT      "MM/DD/YY"   /* DATE IN GREGORIAN FORMAT
GMONTH   FORM      2            /*     MONTH   NUMERIC
GDAY     FORM      2            /*     DAY     NUMERIC
GYEAR    FORM      2            /*     YEAR    NUMERIC
DATESET  INIT      "N"          /* FLAG THAT DETERMINES IF DATE HAS BEEN SET
DB11FLAG INIT      "N"          /* (Y=DB11)
NFEB     FORM      "28"         /* NUMBER OF DAYS IN FEBRUARY
N30      FORM      "30"         /* NUMERIC CONSTANT
N31      FORM      "31"         /* NUMERIC CONSTANT
DATEWKD1 INIT      " "          /* WORK AREA
DATEWK1  FORM      "-1"
DATEWK2  DIM       2
DATEWK3  DIM       3
.
...............................................................................
