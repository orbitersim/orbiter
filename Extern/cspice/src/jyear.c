/* jyear.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure JYEAR ( Seconds per julian year ) */
doublereal jyear_(void)
{
    /* System generated locals */
    doublereal ret_val;

/* $ Abstract */

/*     Return the number of seconds in a julian year. */

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

/*     The function returns the number of seconds per julian year. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     The function returns the number of seconds per julian year. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The julian year is often used as a fundamental unit of time when */
/*     dealing with ephemeris data. For this reason its value in terms of */
/*     ephemeris seconds is recorded in this function. */

/* $ Examples */

/*     Suppose you wish to compute the number of julian centuries */
/*     that have elapsed since the ephemeris epoch J1950 (beginning */
/*     of the julian year 1950) at a particular ET epoch. The */
/*     following line of code will do the trick. */


/*        CENTRY = ( ET - UNITIM ( J1950(), 'JED', 'ET' ) ) */
/*       .       / ( 100.0D0 * JYEAR() ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     [1]  P. Kenneth Seidelmann (Ed.), "Explanatory Supplement to the */
/*          Astronomical Almanac," Page 8, University Science Books, */
/*          1992. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 08-APR-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.0, 13-JUL-1993 (WLT) */

/* -& */
/* $ Index_Entries */

/*     Number of seconds per julian year */

/* -& */
    ret_val = 31557600.;
    return ret_val;
} /* jyear_ */

