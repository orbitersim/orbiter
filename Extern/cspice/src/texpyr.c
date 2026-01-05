/* texpyr.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure TEXPYR ( Time --- Expand year ) */
/* Subroutine */ int texpyr_0_(int n__, integer *year)
{
    /* Initialized data */

    static integer centry = 1900;
    static integer lbound = 1969;

/* $ Abstract */

/*     Expand an abbreviated year to a full year specification. */

/* $ Disclaimer */

/*     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE */
/*     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S. */
/*     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE */
/*     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE */
/*     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS" */
/*     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY */
/*     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A */
/*     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC */
/*     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE */
/*     SOFTWARE AND RELATED MATERIALS, HOWEVER USED. */

/*     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA */
/*     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT */
/*     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND, */
/*     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS, */
/*     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE */
/*     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY. */

/*     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF */
/*     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY */
/*     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE */
/*     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE. */

/* $ Required_Reading */

/*     TIME */

/* $ Keywords */

/*     TIME */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     YEAR      I-O  The year of some epoch abbreviated/expanded. */

/* $ Detailed_Input */

/*     YEAR     is an "abbreviated year." In other words the 98 of */
/*              1998,  05 of 2005, etc. */

/* $ Detailed_Output */

/*     YEAR     is the expansion of the abbreviated year according */
/*              to the lower bound established in the entry point */
/*              TSETYR. By default if YEAR is 69 to 99, the output */
/*              is 1900 + the input value of YEAR. If YEAR is 0 to 68 */
/*              the output value of YEAR is 2000 + the input value of */
/*              YEAR. */

/*              See the entry point TSETRY to modify this behavior. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  If on input YEAR is not in the inclusive interval from */
/*         0 to 99, YEAR is returned unchanged. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine allows all of the SPICE time subsystem to handle */
/*     uniformly the expansion of "abbreviated" years.  (i.e. the */
/*     remainder after dividing the actual year by 100). */

/*     By using this routine together with the routine TSETYR you */
/*     can recover the actual year to associate with an abbreviation. */

/*     The default behavior is as follows */

/*        YEAR Input      YEAR Output */
/*        ----------      ----------- */
/*            00              2000 */
/*            01              2001 */
/*             .                . */
/*             .                . */
/*             .                . */
/*            66              2066 */
/*            67              2067 */
/*            68              2068 */
/*            69              1969 */
/*             .                . */
/*             .                . */
/*             .                . */
/*            99              1999 */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Demonstrate the default behavior of routine TEXPYR and then */
/*        modify it in order to set the lower bound of the expansion to */
/*        1980. */

/*        Example code begins here. */


/*              PROGRAM TEXPYR_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local parameters. */
/*        C */
/*              INTEGER               NYEARS */
/*              PARAMETER           ( NYEARS = 10 ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              INTEGER               I */
/*              INTEGER               EXYEAR */
/*              INTEGER               YEARS  ( NYEARS ) */

/*        C */
/*        C     Set the input years. */
/*        C */
/*              DATA                  YEARS /  0,  1, 68, 69, 70, */
/*             .                              78, 79, 80, 81, 99  / */

/*        C */
/*        C     Display the default behavior. */
/*        C */
/*              WRITE(*,'(A)') 'Default behavior:' */
/*              WRITE(*,*) */

/*              WRITE(*,'(A)') 'In  Expansion' */
/*              WRITE(*,'(A)') '--  ---------' */

/*              DO I=1, NYEARS */

/*                 EXYEAR = YEARS(I) */
/*                 CALL TEXPYR ( EXYEAR ) */

/*                 WRITE(*,'(I2.2,5X,I4)') YEARS(I), EXYEAR */

/*              END DO */

/*        C */
/*        C     Set up the lower bound for the expansion of abbreviated */
/*        C     years to 1980. */
/*        C */
/*              CALL TSETYR ( 1980 ) */

/*        C */
/*        C     Display the new behavior. */
/*        C */
/*              WRITE(*,*) */
/*              WRITE(*,'(A)') 'Lower bound for expansion set to 1980:' */
/*              WRITE(*,*) */

/*              WRITE(*,'(A)') 'In  Expansion' */
/*              WRITE(*,'(A)') '--  ---------' */

/*              DO I=1, NYEARS */

/*                 EXYEAR = YEARS(I) */
/*                 CALL TEXPYR ( EXYEAR ) */

/*                 WRITE(*,'(I2.2,5X,I4)') YEARS(I), EXYEAR */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        Default behavior: */

/*        In  Expansion */
/*        --  --------- */
/*        00     2000 */
/*        01     2001 */
/*        68     2068 */
/*        69     1969 */
/*        70     1970 */
/*        78     1978 */
/*        79     1979 */
/*        80     1980 */
/*        81     1981 */
/*        99     1999 */

/*        Lower bound for expansion set to 1980: */

/*        In  Expansion */
/*        --  --------- */
/*        00     2000 */
/*        01     2001 */
/*        68     2068 */
/*        69     2069 */
/*        70     2070 */
/*        78     2078 */
/*        79     2079 */
/*        80     1980 */
/*        81     1981 */
/*        99     1999 */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.1, 23-SEP-2020 (JDR) */

/*        Edited the header to comply with NAIF standard. Added complete */
/*        example code. */

/*        Added TIME to the list of Required Readings. */

/* -    SPICELIB Version 2.0.0, 18-NOV-1997 (WLT) */

/*        The default century was changed from 1950-2049 to 1969-2068 */

/* -    SPICELIB Version 1.0.0, 08-APR-1996 (WLT) */

/* -& */
/* $ Index_Entries */

/*     Expand an abbreviated year to a fully specified year. */

/* -& */
    switch(n__) {
	case 1: goto L_tsetyr;
	}

    if (*year >= 100 || *year < 0) {
	return 0;
    }
    *year += centry;
    if (*year < lbound) {
	*year += 100;
    }
    return 0;
/* $Procedure TSETYR ( Time --- set year expansion boundaries ) */

L_tsetyr:
/* $ Abstract */

/*     Set the lower bound on the 100 year range. */

/* $ Disclaimer */

/*     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE */
/*     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S. */
/*     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE */
/*     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE */
/*     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS" */
/*     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY */
/*     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A */
/*     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC */
/*     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE */
/*     SOFTWARE AND RELATED MATERIALS, HOWEVER USED. */

/*     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA */
/*     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT */
/*     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND, */
/*     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS, */
/*     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE */
/*     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY. */

/*     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF */
/*     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY */
/*     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE */
/*     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE. */

/* $ Required_Reading */

/*     TIME */

/* $ Keywords */

/*     TIME */

/* $ Declarations */

/*     INTEGER               YEAR */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     YEAR       I   Lower bound on the 100 year interval of expansion */

/* $ Detailed_Input */

/*     YEAR     is the year associated with the lower bound on all year */
/*              expansions computed by the SPICELIB routine TEXPYR. For */
/*              example if YEAR is 1980, then the range of years that can */
/*              be abbreviated is from 1980 to 2079. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  If YEAR is less than 1, no action is taken. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This entry point is used to set the range to which years */
/*     abbreviated to the last two digits will be expanded, allowing all */
/*     of the SPICE time subsystem routines to handle uniformly the */
/*     expansion those "abbreviated" years (i.e. the remainder after */
/*     dividing the actual year by 100.) The input supplied to this */
/*     routine represents the lower bound of the expansion interval. The */
/*     upper bound of the expansion interval is YEAR + 99. */

/*     The default expansion interval is from 1969 to 2068. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Suppose that you need to manipulate time strings and that */
/*        you want to treat years components in the range from 0 to 99 */
/*        as being abbreviations for years in the range from */
/*        1980 to 2079 (provided that the years are not modified by */
/*        an ERA substring). The example code below shows how you */
/*        could go about this. */

/*        Use the LSK kernel below to load the leap seconds and time */
/*        constants required for the conversions. */

/*           naif0012.tls */


/*        Example code begins here. */


/*              PROGRAM TSETYR_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local parameters. */
/*        C */
/*              INTEGER               DATELN */
/*              PARAMETER           ( DATELN = 11 ) */

/*              INTEGER               NTSTRS */
/*              PARAMETER           ( NTSTRS = 7 ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              CHARACTER*(DATELN)    DATE   (NTSTRS) */
/*              CHARACTER*(DATELN)    TIMSTR */

/*              DOUBLE PRECISION      ET */

/*              INTEGER               I */

/*        C */
/*        C     Assign an array of calendar dates. */
/*        C */
/*              DATA                  DATE   / '00 JAN 21', */
/*             .                               '01 FEB 22', */
/*             .                               '48 MAR 23', */
/*             .                               '49 APR 24', */
/*             .                               '79 JUL 14', */
/*             .                               '80 FEB 02', */
/*             .                               '99 DEC 31' / */

/*        C */
/*        C     Load the required LSK. */
/*        C */
/*              CALL FURNSH ( 'naif0012.tls' ) */

/*        C */
/*        C     Set up the lower bound for the */
/*        C     expansion of abbreviated years. */
/*        C */
/*              CALL TSETYR ( 1980 ) */

/*        C */
/*        C     Expand the years in input time strings. */
/*        C */
/*              WRITE(*,*) 'Time string    Expansion' */
/*              WRITE(*,*) '-----------    -----------' */

/*              DO I = 1, NTSTRS */

/*                 CALL STR2ET ( DATE(I), ET ) */
/*                 CALL TIMOUT ( ET, 'YYYY MON DD', TIMSTR ) */

/*                 WRITE(*,*) DATE(I), '    ', TIMSTR */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Time string    Expansion */
/*         -----------    ----------- */
/*         00 JAN 21      2000 JAN 21 */
/*         01 FEB 22      2001 FEB 22 */
/*         48 MAR 23      2048 MAR 23 */
/*         49 APR 24      2049 APR 24 */
/*         79 JUL 14      2079 JUL 14 */
/*         80 FEB 02      1980 FEB 02 */
/*         99 DEC 31      1999 DEC 31 */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.1.0, 23-SEP-2020 (JDR) */

/*        Fixed bug: Added check for "YEAR" to be positive in order to */
/*        update the lower bound for the expansion. */

/*        Edited the header to comply with NAIF standard. Added complete */
/*        code example. */

/*        Added TIME to the list of Required Readings. Extended */
/*        description in $Particulars to further describe the intended */
/*        use of this routine. */

/* -    SPICELIB Version 2.0.0, 18-NOV-1997 (WLT) */

/*        The default century was change from 1950-2049 to 1969-2068. */

/* -    SPICELIB Version 1.0.0, 08-APR-1996 (WLT) */

/* -& */
/* $ Index_Entries */

/*     Set the interval of expansion for abbreviated years */

/* -& */
    if (*year > 0) {
	centry = *year / 100 * 100;
	lbound = *year;
    }
    return 0;
} /* texpyr_ */

/* Subroutine */ int texpyr_(integer *year)
{
    return texpyr_0_(0, year);
    }

/* Subroutine */ int tsetyr_(integer *year)
{
    return texpyr_0_(1, year);
    }

