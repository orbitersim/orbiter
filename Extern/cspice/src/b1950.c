/* b1950.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure B1950 ( Besselian Date 1950.0 ) */
doublereal b1950_(void)
{
    /* System generated locals */
    doublereal ret_val;

/* $ Abstract */

/*     Return the Julian Date corresponding to Besselian Date 1950.0. */

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

/*     The function returns the Julian Date corresponding to Besselian */
/*     date 1950.0. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     The function returns 2433282.42345905, the Julian Date */
/*     corresponding to Besselian Date 1950.0. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The function always returns the constant value shown above. */

/* $ Examples */

/*     The following code fragment illustrates the use of B1950. */

/*        C */
/*        C     Convert Julian Date to UTC seconds past the reference */
/*        C     epoch (B1950). */
/*        C */
/*              SPREF = ( JD - B1950() ) * SPD() */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     [1]  J. Lieske, "Precession Matrix Based on IAU (1976) System of */
/*          Astronomical Constants," Astron. Astrophys. 73, 282-284, */
/*          1979. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.2, 08-JUL-2020 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 2.0.1, 18-AUG-2008 (EDW) */

/*        Edited the value stated in $Detailed_Output to match the */
/*        current return value. The edit changed: */

/*           2433282.423 */

/*        to */

/*           2433282.42345905 */

/* -    SPICELIB Version 2.0.0, 30-JUL-1993 (WLT) */

/*        The value of B1950 was updated to reflect the value given */
/*        by Lieske in [1] */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU) */

/* -& */
/* $ Index_Entries */

/*     besselian date 1950.0 */

/* -& */
    ret_val = 2433282.42345905;
    return ret_val;
} /* b1950_ */

