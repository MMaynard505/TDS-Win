.
.   TRDSWDM.INC    Include file for Water District header records
.      V.M=1.2     Revised 11/08/88
.
.
.Field   Type      Length     From-To   Description
.-----   ----      ------     -------   ------------------------------------
WDM1     DIM       4      /*    1-4     W/D Number
WDM2     DIM       2      /*    5-6     W/D Tax Year
WDM3     DIM       28     /*    7-34    W/D Name/Address 1
WDM4     DIM       28     /*   35-62    W/D Name/Address 2
WDM5     DIM       28     /*   63-90    W/D Name/Address 3
WDM6     DIM       28     /*   91-118   W/D Name/Address 4
WDM7     DIM       10     /*  118-128   W/D Phone number
WDM8     FORM      2.2    /*  129-133   W/D Tax Rate
WDM9     FORM      10.2   /*  134-146   To-Date Payments amount
WDM10    FORM      12     /*  147-158   To-Date Land value
WDM11    FORM      12     /*  159-170   To-Date Ag. value
WDM12    FORM      12     /*  171-182   To-Date Improvement value
WDM13    FORM      12     /*  183-194   To-Date Personal value
WDM14    FORM      11.2   /*  195-208   To-Date Net adjustments (+, -)
WDM15    FORM      10     /*  209-218   To-Date Exemptions
WDM16    FORM      10.2   /*  219-231   To-Date Tax value
WDM17    FORM      5      /*  232-236   Number of Units (Updtd only by TRAP31)
WDM18    DIM       2      /*  237-238   Last supplement number
WDM19    DIM       1      /*  239-239   Additional 15% Penalty
WDM20    DIM       3      /*  240-242   CAD Jurisdiction number
WDM21    DIM       2      /*  243-244   Last correction roll number
WDM22    DIM       4      /*  245-248   Drainage District number      11/08/88
WDM23    DIM       2      /*  249-250   Filler   (  Unused at present  )
.                -----
.                250 bytes in record
.
WDMREC   VARLIST   WDM1,WDM2,WDM3,WDM4,WDM5,WDM6,WDM7,WDM8,WDM9,WDM10:
                   WDM11,WDM12,WDM13,WDM14,WDM15,WDM16,WDM17,WDM18:
                   WDM19,WDM20,WDM21,WDM22,WDM23
