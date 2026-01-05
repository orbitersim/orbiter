/* tyear.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure TYEAR ( Seconds per tropical year ) */
doublereal tyear_(void)
{
    /* System generated locals */
    doublereal ret_val;

/* $ Abstract */

/*     Return the number of seconds in a tropical year. */

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

/*     None. */

/* $ Keywords */

/*     CONSTANTS */

/* $ Declarations */

/*     None. */

/* $ Brief_I/O */

/*     The function returns the number of seconds per tropical year. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     The function returns the number of seconds per tropical */
/*     year. This value is taken from the 1992 Explanatory Supplement */
/*     to the Astronomical Almanac (see [1]). */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The tropical year is often used as a fundamental unit */
/*     of time when dealing with older ephemeris data. For this */
/*     reason its value in terms of ephemeris seconds is */
/*     recorded in this function. */

/* $ Examples */

/*     Suppose you wish to compute the number of tropical centuries */
/*     that have elapsed since the ephemeris epoch B1950 (beginning */
/*     of the Besselian year 1950) at a particular ET epoch. The */
/*     following line of code will do the trick. */


/*        CENTRY = ( ET - UNITIM ( B1950(), 'JED', 'ET' ) ) */
/*       .       / ( 100.0D0 * TYEAR() ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     [1]  P. Kenneth Seidelmann (Ed.), "Explanatory Supplement to the */
/*          Astronomical Almanac," p 80, University Science Books, 1992. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 13-APR-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.0, 13-JUL-1993 (WLT) */

/* -& */
/* $ Index_Entries */

/*     Number of seconds per tropical year */

/* -& */
    ret_val = 31556925.9747;
    return ret_val;
} /* tyear_ */

