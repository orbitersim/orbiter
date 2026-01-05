/* edpnt.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure EDPNT ( Ellipsoid point  ) */
/* Subroutine */ int edpnt_(doublereal *p, doublereal *a, doublereal *b, 
	doublereal *c__, doublereal *ep)
{
    /* System generated locals */
    doublereal d__1, d__2, d__3, d__4;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    doublereal level;
    extern /* Subroutine */ int errdp_(char *, doublereal *, ftnlen);
    extern logical vzero_(doublereal *), failed_(void);
    doublereal sq;
    extern doublereal touchd_(doublereal *);
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen);

/* $ Abstract */

/*     Scale a point so that it lies on the surface of a specified */
/*     triaxial ellipsoid that is centered at the origin and aligned */
/*     with the Cartesian coordinate axes. */

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

/*     ELLIPSOID */
/*     GEOMETRY */
/*     MATH */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     P          I   A point in three-dimensional space. */
/*     A          I   Semi-axis length in the X direction. */
/*     B          I   Semi-axis length in the Y direction. */
/*     C          I   Semi-axis length in the Z direction. */
/*     EP         O   Point on ellipsoid. */

/* $ Detailed_Input */

/*     P        is a non-zero point in three-dimensional space. */

/*     A, */
/*     B, */
/*     C        are, respectively, the semi-axis lengths of a triaxial */
/*              ellipsoid in the X, Y, and Z directions. The axes of */
/*              the ellipsoid are aligned with the axes of the */
/*              Cartesian coordinate system. */

/* $ Detailed_Output */

/*     EP       is the result of scaling the input point P so that */
/*              it lies on the surface of the triaxial ellipsoid */
/*              defined by the input semi-axis lengths. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If any of the target ellipsoid's semi-axis lengths is */
/*         non-positive, the error SPICE(INVALIDAXES) is signaled. */

/*     2)  If P is the zero vector, the error SPICE(ZEROVECTOR) is */
/*         signaled. */

/*     3)  If the level surface parameter of the input point */
/*         underflows, the error SPICE(POINTTOOSMALL) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine efficiently computes the ellipsoid surface point */
/*     corresponding to a specified ray emanating from the origin. */
/*     Practical examples of this computation occur in the SPICELIB */
/*     routines LATSRF and SRFREC. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */


/*     1) Find the surface intercept point on an ellipsoid having radii */

/*            ( 3, 2, 1 ) */

/*        of the ray emanating from the origin and having direction */
/*        vector */

/*            ( 1, 1, 1 ) */


/*        Example code begins here. */


/*              PROGRAM EDPNT_EX1 */
/*              IMPLICIT NONE */

/*              CHARACTER*(*)         FMT1 */
/*              PARAMETER           ( FMT1 = '(A,F18.14)'  ) */

/*              CHARACTER*(*)         FMT3 */
/*              PARAMETER           ( FMT3 = '(A,3F18.14)' ) */

/*              DOUBLE PRECISION      A */
/*              DOUBLE PRECISION      B */
/*              DOUBLE PRECISION      C */
/*              DOUBLE PRECISION      V      ( 3 ) */
/*              DOUBLE PRECISION      EP     ( 3 ) */
/*              DOUBLE PRECISION      LEVEL */

/*              A = 3.D0 */
/*              B = 2.D0 */
/*              C = 1.D0 */

/*              CALL VPACK ( 1.D0, 1.D0, 1.D0, V ) */

/*              CALL EDPNT ( V, A, B, C, EP ) */

/*              WRITE (*,FMT3) 'EP    = ', EP */

/*        C */
/*        C     Verify that EP is on the ellipsoid. */
/*        C */
/*              LEVEL = (EP(1)/A)**2 + (EP(2)/B)**2 + (EP(3)/C)**2 */

/*              WRITE (*,FMT1) 'LEVEL = ', LEVEL */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        EP    =   0.85714285714286  0.85714285714286  0.85714285714286 */
/*        LEVEL =   1.00000000000000 */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.1, 09-JUL-2020 (JDR) */

/*        Minor edits to the header and code example. */

/* -    SPICELIB Version 2.0.0, 19-APR-2016 (NJB) (EDW) */

/* -& */
/* $ Index_Entries */

/*     scale point to lie on ellipsoid */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Use discovery check-in. */

    if (failed_()) {
	return 0;
    }
    if (*a <= 0. || *b <= 0. || *c__ <= 0.) {
	chkin_("EDPNT", (ftnlen)5);
	setmsg_("Ellipsoid radii must be strictly positive but are (#, #, #)."
		, (ftnlen)60);
	errdp_("#", a, (ftnlen)1);
	errdp_("#", b, (ftnlen)1);
	errdp_("#", c__, (ftnlen)1);
	sigerr_("SPICE(INVALIDRADII)", (ftnlen)19);
	chkout_("EDPNT", (ftnlen)5);
	return 0;
    }

/*     The input point must be non-zero, or we can't scale it */
/*     to the ellipsoid. */

    if (vzero_(p)) {
	chkin_("EDPNT", (ftnlen)5);
	setmsg_("Input point was the zero vector. A non-zero vector is requi"
		"red.", (ftnlen)63);
	sigerr_("SPICE(ZEROVECTOR)", (ftnlen)17);
	chkout_("EDPNT", (ftnlen)5);
	return 0;
    }

/*     Find the level surface parameter of the input point with respect */
/*     to the scaled ellipsoid. */

/* Computing 2nd power */
    d__2 = p[0] / *a;
/* Computing 2nd power */
    d__3 = p[1] / *b;
/* Computing 2nd power */
    d__4 = p[2] / *c__;
    d__1 = d__2 * d__2 + d__3 * d__3 + d__4 * d__4;
    level = touchd_(&d__1);
    if (level <= 0.) {

/*        We expect that LEVEL will be non-negative, but it could */
/*        be zero. We check for negative values as a precaution. */

	chkin_("EDPNT", (ftnlen)5);
	setmsg_("Input point's level surface parameter was non-positive. The"
		" point is too close to the origin to be scaled to the ellips"
		"oid. The point was (#, #, #).", (ftnlen)148);
	errdp_("#", p, (ftnlen)1);
	errdp_("#", &p[1], (ftnlen)1);
	errdp_("#", &p[2], (ftnlen)1);
	sigerr_("SPICE(POINTTOOSMALL)", (ftnlen)20);
	chkout_("EDPNT", (ftnlen)5);
	return 0;
    }

/*     Scale the point to one for which the level surface parameter is 1. */

    sq = sqrt(level);
    ep[0] = p[0] / sq;
    ep[1] = p[1] / sq;
    ep[2] = p[2] / sq;
    return 0;
} /* edpnt_ */

