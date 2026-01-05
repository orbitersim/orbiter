/* pi.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure PI ( Value of pi ) */
doublereal pi_(void)
{
    /* Initialized data */

    static doublereal value = 0.;

    /* System generated locals */
    doublereal ret_val;

    /* Builtin functions */
    double acos(doublereal);

/* $ Abstract */

/*     Return the value of pi (the ratio of the circumference of */
/*     a circle to its diameter). */

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

/*     The function returns the value of pi. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     The function returns the value of pi (the ratio of a circle's */
/*     circumference to its diameter), determined by the ACOS function. */
/*     That is, */

/*           PI = ACOS ( -1.D0 ) */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The first time the function is referenced, the value is computed */
/*     as shown above. The value is saved, and returned directly upon */
/*     subsequent reference. */

/* $ Examples */

/*     The code fragment below illustrates the use of PI. */

/*        C */
/*        C     Compute the polar radius, */
/*        C */
/*        C                 p */
/*        C          ---------------- */
/*        C          1 + e cos(theta) */
/*        C */
/*        C     at evenly spaced values of the polar angle, theta. */
/*        C */
/*              DELTA = PI() / N */

/*              DO I = 0, N */
/*                 R(I) = P / (1.D0 + ECC * COS(I*DELTA)) */
/*              END DO */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.2, 08-JUL-2020 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU) */

/* -& */
/* $ Index_Entries */

/*     value of pi */

/* -& */

/*     Local variables */


/*     Initial values */


/*     What is there to say? */

    if (value == 0.) {
	value = acos(-1.);
    }
    ret_val = value;
    return ret_val;
} /* pi_ */

