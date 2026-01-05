/* pltnp.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure PLTNP ( Nearest point on triangular plate ) */
/* Subroutine */ int pltnp_(doublereal *point, doublereal *v1, doublereal *v2,
	 doublereal *v3, doublereal *pnear, doublereal *dist)
{
    extern /* Subroutine */ int vadd_(doublereal *, doublereal *, doublereal *
	    );
    doublereal perp[3];
    extern doublereal vdot_(doublereal *, doublereal *);
    extern /* Subroutine */ int vsub_(doublereal *, doublereal *, doublereal *
	    ), vequ_(doublereal *, doublereal *);
    logical degen;
    doublereal pdiff[3], d1, d2, d3, e1[3];
    extern doublereal vdist_(doublereal *, doublereal *);
    doublereal e2[3], e3[3], l1, l2, l3;
    extern doublereal vnorm_(doublereal *);
    extern /* Subroutine */ int vcrss_(doublereal *, doublereal *, doublereal 
	    *), vperp_(doublereal *, doublereal *, doublereal *);
    extern logical vzero_(doublereal *);
    doublereal enorm1[3], enorm2[3], enorm3[3], normal[3];
    logical in1, in2, in3;
    extern /* Subroutine */ int npsgpt_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *);
    extern logical return_(void);
    doublereal np1[3], np2[3];

/* $ Abstract */

/*     Find the nearest point on a triangular plate to a given point. */

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

/*     DSK */

/* $ Keywords */

/*     GEOMETRY */
/*     MATH */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     POINT      I   A point in 3-dimensional space. */
/*     V1, */
/*     V2, */
/*     V3         I   Vertices of a triangular plate. */
/*     PNEAR      O   Nearest point on the plate to POINT. */
/*     DIST       O   Distance between PNEAR and POINT. */

/* $ Detailed_Input */

/*     POINT    is an arbitrary point in 3-dimensional space. */

/*     V1, */
/*     V2, */
/*     V3       are 3-vectors constituting the vertices of */
/*              a triangular plate. */

/*              The plate is allowed to be degenerate: it may */
/*              consist of a line segment or of a single point. */

/* $ Detailed_Output */

/*     PNEAR    is the closest point on the plate to POINT. */
/*              PNEAR is unique, since the plate is convex. */

/*     DIST     is the distance between POINT and PNEAR. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  The input plate is allowed to be degenerate: it may be */
/*         a line segment or a single point. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as input */
/*     (if any), the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */


/*     1) Find the nearest point to the point (2,2,2) on a plate having */
/*        vertices at the unit basis vectors that lie along the positive */
/*        X, Y, and Z coordinate axes. */


/*        Example code begins here. */


/*              PROGRAM PLTNP_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local parameters */
/*        C */
/*              CHARACTER*(*)         FMT1 */
/*              PARAMETER           ( FMT1 = '(A,3E15.7)' ) */
/*        C */
/*        C     Local variables */
/*        C */
/*              DOUBLE PRECISION      DIST */
/*              DOUBLE PRECISION      POINT  ( 3 ) */
/*              DOUBLE PRECISION      PNEAR  ( 3 ) */
/*              DOUBLE PRECISION      V1     ( 3 ) */
/*              DOUBLE PRECISION      V2     ( 3 ) */
/*              DOUBLE PRECISION      V3     ( 3 ) */

/*        C */
/*        C     POINT is the input point. */
/*        C */
/*              CALL VPACK ( 2.D0, 2.D0, 2.D0, POINT ) */
/*        C */
/*        C     V1, V2, V3 are the vertices of a plate. */
/*        C */
/*              CALL VPACK ( 1.D0, 0.D0, 0.D0, V1 ) */
/*              CALL VPACK ( 0.D0, 1.D0, 0.D0, V2 ) */
/*              CALL VPACK ( 0.D0, 0.D0, 1.D0, V3 ) */
/*        C */
/*        C     Find the near point on the plate. */
/*        C */
/*              CALL PLTNP ( POINT, V1, V2, V3, PNEAR, DIST ) */

/*              WRITE (*,*) ' ' */
/*              WRITE (*,FMT1) 'Plate vertex 1 = ', V1 */
/*              WRITE (*,FMT1) 'Plate vertex 2 = ', V2 */
/*              WRITE (*,FMT1) 'Plate vertex 3 = ', V3 */
/*              WRITE (*,FMT1) 'Input point    = ', POINT */
/*              WRITE (*,*)    ' ' */
/*              WRITE (*,FMT1) 'Near point     = ', PNEAR */
/*              WRITE (*,FMT1) 'Distance       = ', DIST */
/*              WRITE (*,*) ' ' */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        Plate vertex 1 =   0.1000000E+01  0.0000000E+00  0.0000000E+00 */
/*        Plate vertex 2 =   0.0000000E+00  0.1000000E+01  0.0000000E+00 */
/*        Plate vertex 3 =   0.0000000E+00  0.0000000E+00  0.1000000E+01 */
/*        Input point    =   0.2000000E+01  0.2000000E+01  0.2000000E+01 */

/*        Near point     =   0.3333333E+00  0.3333333E+00  0.3333333E+00 */
/*        Distance       =   0.2886751E+01 */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */

/* $ Version */

/* -    SPICELIB Version 1.1.3, 04-JUL-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/*        Edited code example output format for the solution to fit */
/*        within the $Examples section without modifications. Added */
/*        DSK to $Required_Readings. */

/* -    SPICELIB Version 1.1.2, 01-FEB-2016 (NJB) */

/*        Added code example to header. */

/*     DSKLIB Version 1.1.1, 19-MAR-2015 (NJB) */

/*        Fixed spelling error in header. */

/*     DSKLIB Version 1.1.0, 31-DEC-2014 (NJB) */

/*        Bug fix: vertex indices for outside case, near */
/*        point on 3rd edge were corrected. */

/*     DSKLIB Version 1.0.0, 29-SEP-2014 (NJB) */

/* -& */
/* $ Index_Entries */

/*     nearest point on triangular plate */

/* -& */

/*     SPICELIB functions */


/*     Local variables */

    if (return_()) {
	return 0;
    }

/*     Use discovery check-in. */


/*     Compute the plate's edges. */

    vsub_(v2, v1, e1);
    vsub_(v3, v2, e2);
    vsub_(v1, v3, e3);

/*     Compute a normal vector for the plate, if possible. */
/*     If the plate is degenerate, we'll find out at this point. */

    vcrss_(e1, e2, normal);

/*     Compute the outward normals of the plate's edges in the */
/*     plate containing the plate. */

    vcrss_(e1, normal, enorm1);
    vcrss_(e2, normal, enorm2);
    vcrss_(e3, normal, enorm3);
    degen = vzero_(normal) || vzero_(enorm1) || vzero_(enorm2) || vzero_(
	    enorm3);
    if (degen) {

/*        The "plate" is a line segment or point. Determine */
/*        which case we have. */

	l1 = vnorm_(e1);
	l2 = vnorm_(e2);
	l3 = vnorm_(e3);
	if (l1 == 0. && l2 == 0.) {

/*           Up to round-off error, the vertices coincide. */
/*           The vertex V1 for practical purposes is the plate. */

	    vequ_(v1, pnear);
	    *dist = vdist_(pnear, point);
	} else {

/*           The plate is a line segment having positive length. */
/*           One of the edges will coincide with the segment. */
/*           Determine which vertices are the endpoints. */

	    if (l1 > max(l2,l3)) {

/*              The segment is bounded by V1 and V2. */

		npsgpt_(v1, v2, point, pnear, dist);
	    } else if (l2 > max(l3,l1)) {

/*              The segment is bounded by V2 and V3. */

		npsgpt_(v2, v3, point, pnear, dist);
	    } else {

/*              The segment is bounded by V3 and V1. */

		npsgpt_(v3, v1, point, pnear, dist);
	    }
	}

/*        The outputs are set for the degenerate cases. */

	return 0;
    }

/*     We have a non-degenerate plate. NORMAL has unit length. */

/*     We'll treat V1 as an origin in the plane containing */
/*     the plate. Find the offset of the POINT from V1, and */
/*     find the component of this offset orthogonal to NORMAL. */

    vsub_(point, v1, pdiff);
    vperp_(pdiff, normal, perp);

/*     Determine whether V1+PERP is inside the plate. */

/*     Note that the "line constants" for edges 1 and 3 */
/*     are zero, since these edges contain V1. The line */
/*     constant for edge 2 is that of the offset of V2 */
/*     from V1; this offset is edge 1. */

    in1 = vdot_(perp, enorm1) <= 0.;
    in2 = vdot_(perp, enorm2) <= vdot_(e1, enorm2);
    in3 = vdot_(perp, enorm3) <= 0.;
    if (in1 && in2 && in3) {

/*        V1+PERP is inside the plate. It is the closest */
/*        point on the plate to POINT. */

	vadd_(v1, perp, pnear);

/*        We have the near point; set the distance. */

	*dist = vdist_(pnear, point);
    } else {

/*        PERP is outside the plate. The nearest point */
/*        on the plate to POINT is on one of the edges. */

/*        We'll use the "in" flags to reduce the number */
/*        of point-edge distance computations. */

	if (! in1 && (in2 && in3)) {

/*           The solution must be on the first edge. */

	    npsgpt_(v1, v2, point, pnear, dist);
	} else if (! in2 && (in3 && in1)) {

/*           The solution must be on the second edge. */

	    npsgpt_(v2, v3, point, pnear, dist);
	} else if (! in3 && (in1 && in2)) {

/*           The solution must be on the third edge. */

	    npsgpt_(v3, v1, point, pnear, dist);
	} else {

/*           Compute solutions on all three edges and pick */
/*           the best one. */

	    npsgpt_(v1, v2, point, np1, &d1);
	    npsgpt_(v2, v3, point, np2, &d2);
	    npsgpt_(v3, v1, point, pnear, &d3);
	    if (d1 <= min(d2,d3)) {
		vequ_(np1, pnear);
		*dist = d1;
	    } else if (d2 <= min(d3,d1)) {
		vequ_(np2, pnear);
		*dist = d2;
	    } else {

/*              PNEAR is already set. */

		*dist = d3;
	    }
	}
    }
    return 0;
} /* pltnp_ */

