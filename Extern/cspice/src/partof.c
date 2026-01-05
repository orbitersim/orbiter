/* partof.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure PARTOF ( Parabolic time of flight ) */
/* Subroutine */ int partof_(doublereal *ma, doublereal *d__)
{
    /* System generated locals */
    doublereal d__1;

    /* Local variables */
    doublereal m;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern doublereal dcbrt_(doublereal *);
    doublereal deriv, deriv2, fn, change;
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Solve the time of flight equation MA = D + (D**3) / 3 */
/*     for the parabolic eccentric anomaly D, given mean anomaly. */

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

/*     CONIC */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     MA         I   Mean anomaly at epoch. */
/*     D          O   Parabolic eccentric anomaly. */

/* $ Detailed_Input */

/*     MA       is the parabolic mean anomaly of an orbiting body at */
/*              some epoch t, */

/*                                   3  1/2 */
/*                 MA = (t-T) (mu/(2q )) */

/*              where T is the time of periapsis passage, mu is */
/*              the gravitational parameter of the primary body, */
/*              and q is the perifocal distance. */

/* $ Detailed_Output */

/*     D        is the corresponding parabolic anomaly. This is the */
/*              solution to the time of flight equation */

/*                           3 */
/*                 MA = D + D / 3 */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Iterate to solve */

/*                         3 */
/*        f(D,MA,p) = D + D / 3 - MA = 0 */

/* $ Examples */

/*     ELLTOF, HYPTOF, and PARTOF are used by CONICS. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     [1]  R. Bate, D. Mueller, and J. White, "Fundamentals of */
/*          Astrodynamics," Dover Publications Inc., 1971. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.1.0, 14-APR-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Removed */
/*        unnecessary $Revisions section. */

/* -    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 2.0.0, 19-APR-1991 (WLT) */

/*        A write statement left over from debugging days was removed. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) */

/* -& */
/* $ Index_Entries */

/*     parabolic time of flight */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("PARTOF", (ftnlen)6);
    }

/*     If the mean anomaly is zero, the eccentric anomaly is also zero */
/*     (by inspection). If the mean anomaly is negative, we can pretend */
/*     that it's positive (by symmetry). */

    if (*ma == 0.) {
	*d__ = 0.;
	chkout_("PARTOF", (ftnlen)6);
	return 0;
    } else {
	m = abs(*ma);
    }

/*     We need an initial guess for the eccentric anomaly D. The function */
/*     is well behaved, so just about any guess will do. */

    d__1 = m * 3.;
    *d__ = dcbrt_(&d__1);

/*     Use the Newton second-order method, */

/*                                           2 */
/*          F    = F  - (f/f')*(1 + f*f''/2f' ) */
/*           i+1    i */

/*     where */

/*                     3 */
/*          f   = D + D / 3 - M */

/*                     2 */
/*          f'  = 1 + D */


/*          f'' = 2 D */

    change = 1.;
    while(abs(change) > 1e-13) {
/* Computing 3rd power */
	d__1 = *d__;
	fn = *d__ + d__1 * (d__1 * d__1) / 3. - m;
/* Computing 2nd power */
	d__1 = *d__;
	deriv = d__1 * d__1 + 1.;
	deriv2 = *d__ * 2.;
/* Computing 2nd power */
	d__1 = deriv;
	change = fn / deriv * (fn * deriv2 / (d__1 * d__1 * 2.) + 1.);
	*d__ -= change;
    }
    if (*ma < 0.) {
	*d__ = -(*d__);
    }
    chkout_("PARTOF", (ftnlen)6);
    return 0;
} /* partof_ */

