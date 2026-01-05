/* pl2psv.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure PL2PSV ( Plane to point and spanning vectors ) */
/* Subroutine */ int pl2psv_(doublereal *plane, doublereal *point, doublereal 
	*span1, doublereal *span2)
{
    extern /* Subroutine */ int frame_(doublereal *, doublereal *, doublereal 
	    *), pl2nvp_(doublereal *, doublereal *, doublereal *);
    doublereal normal[3];

/* $ Abstract */

/*     Return a point and two orthogonal spanning vectors that generate */
/*     a specified plane. */

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

/*     PLANES */

/* $ Keywords */

/*     GEOMETRY */
/*     MATH */
/*     PLANE */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     PLANE      I   A SPICE plane. */
/*     POINT, */
/*     SPAN1, */
/*     SPAN2      O   A point in the input plane and two vectors */
/*                    spanning the input plane. */

/* $ Detailed_Input */

/*     PLANE    is a SPICE plane. */

/* $ Detailed_Output */

/*     POINT, */
/*     SPAN1, */
/*     SPAN2    are, respectively, a point and two orthogonal spanning */
/*              vectors that generate the geometric plane represented by */
/*              PLANE. The geometric plane is the set of vectors */

/*                 POINT   +   s * SPAN1   +   t * SPAN2 */

/*              where `s' and `t' are real numbers. POINT is the closest */
/*              point in the plane to the origin; this point is always a */
/*              multiple of the plane's normal vector. SPAN1 and SPAN2 */
/*              are an orthonormal pair of vectors. POINT, SPAN1, and */
/*              SPAN2 are mutually orthogonal. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  The input plane MUST have been created by one of the SPICELIB */
/*         routines */

/*            NVC2PL ( Normal vector and constant to plane ) */
/*            NVP2PL ( Normal vector and point to plane    ) */
/*            PSV2PL ( Point and spanning vectors to plane ) */

/*         Otherwise, the results of this routine are unpredictable. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     SPICELIB geometry routines that deal with planes use the `plane' */
/*     data type to represent input and output planes. This data type */
/*     makes the subroutine interfaces simpler and more uniform. */

/*     The SPICELIB routines that produce SPICE planes from data that */
/*     define a plane are: */

/*        NVC2PL ( Normal vector and constant to plane ) */
/*        NVP2PL ( Normal vector and point to plane    ) */
/*        PSV2PL ( Point and spanning vectors to plane ) */

/*     The SPICELIB routines that convert SPICE planes to data that */
/*     define a plane are: */

/*        PL2NVC ( Plane to normal vector and constant ) */
/*        PL2NVP ( Plane to normal vector and point    ) */
/*        PL2PSV ( Plane to point and spanning vectors ) */

/* $ Examples */

/*     1)  Project a vector V orthogonally onto a plane defined by */
/*         POINT, SPAN1, and SPAN2. PROJ is the projection we want; it */
/*         is the closest vector in the plane to V. */

/*            CALL PSV2PL ( POINT,   SPAN1,    SPAN2,   PLANE ) */
/*            CALL VPRJP  ( V,       PLANE,    PROJ           ) */


/*     2)  Find the intersection of a plane and the unit sphere. This */
/*         is a geometry problem that arises in computing the */
/*         intersection of a plane and a triaxial ellipsoid. The */
/*         SPICELIB routine INEDPL computes this intersection, but this */
/*         example does illustrate how to use this routine. */


/*            C */
/*            C     The geometric plane of interest will be represented */
/*            C     by the SPICE plane PLANE in this example. */
/*            C */
/*            C     The intersection circle will be represented by the */
/*            C     vectors CENTER, V1, and V2; the circle is the set */
/*            C     of points */
/*            C */
/*            C        CENTER  +  cos(theta) V1  +  sin(theta) V2, */
/*            C */
/*            C     where theta is in the interval (-pi, pi]. */
/*            C */
/*            C     The logical variable FOUND indicates whether the */
/*            C     intersection is non-empty. */
/*            C */

/*            C */
/*            C     The center of the intersection circle will be the */
/*            C     closest point in the plane to the origin. This */
/*            C     point is returned by PL2PSV. The distance of the */
/*            C     center from the origin is the norm of CENTER. */
/*            C */
/*                  CALL PL2PSV  ( PLANE, CENTER, SPAN1, SPAN2 ) */

/*                  DIST = VNORM ( CENTER ) */

/*            C */
/*            C     The radius of the intersection circle will be */
/*            C */
/*            C             ____________ */
/*            C         _  /          2 */
/*            C          \/  1 - DIST */
/*            C */
/*            C     since the radius of the circle, the distance of the */
/*            C     plane from the origin, and the radius of the sphere */
/*            C     (1) are the lengths of the sides of a right triangle. */
/*            C */
/*                  RADIUS = SQRT ( 1.0D0 - DIST**2 ) */

/*                  CALL VSCL  ( RADIUS, SPAN1, V1 ) */
/*                  CALL VSCL  ( RADIUS, SPAN2, V2 ) */

/*                  FOUND = .TRUE. */


/*     3)  Apply a linear transformation represented by the matrix M to */
/*         a plane represented by the normal vector N and the constant C. */
/*         Find a normal vector and constant for the transformed plane. */

/*            C */
/*            C     Make a SPICE plane from N and C, and then find a */
/*            C     point in the plane and spanning vectors for the */
/*            C     plane. N need not be a unit vector. */
/*            C */
/*                  CALL NVC2PL ( N,      C,      PLANE         ) */
/*                  CALL PL2PSV ( PLANE,  POINT,  SPAN1,  SPAN2 ) */

/*            C */
/*            C     Apply the linear transformation to the point and */
/*            C     spanning vectors. All we need to do is multiply */
/*            C     these vectors by M, since for any linear */
/*            C     transformation T, */
/*            C */
/*            C           T ( POINT  +  t1 * SPAN1     +  t2 * SPAN2 ) */
/*            C */
/*            C        =  T (POINT)  +  t1 * T(SPAN1)  +  t2 * T(SPAN2), */
/*            C */
/*            C     which means that T(POINT), T(SPAN1), and T(SPAN2) */
/*            C     are a point and spanning vectors for the transformed */
/*            C     plane. */
/*            C */
/*                  CALL MXV ( M, POINT, TPOINT ) */
/*                  CALL MXV ( M, SPAN1, TSPAN1 ) */
/*                  CALL MXV ( M, SPAN2, TSPAN2 ) */

/*            C */
/*            C     Make a new SPICE plane TPLANE from the */
/*            C     transformed point and spanning vectors, and find a */
/*            C     unit normal and constant for this new plane. */
/*            C */
/*                  CALL PSV2PL ( TPOINT,  TSPAN1,  TSPAN2,  TPLANE ) */
/*                  CALL PL2NVC ( TPLANE,  TN,      TC              ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     [1]  G. Thomas and R. Finney, "Calculus and Analytic Geometry," */
/*          7th Edition, Addison Wesley, 1988. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 24-AUG-2021 (NJB) (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 01-NOV-1990 (NJB) */

/* -& */
/* $ Index_Entries */

/*     plane to point and spanning vectors */

/* -& */

/*     Local variables */


/*     Note for programmers: validity of the input plane is not */
/*     checked in the interest of efficiency. The input plane will be */
/*     valid if it was created by one of the SPICE plane construction */
/*     routines. */

/*     Find a unit normal vector for the plane, and find the closest */
/*     point in the plane to the origin. */

    pl2nvp_(plane, normal, point);

/*     Next, find an orthogonal pair of vectors that are also */
/*     orthogonal to the PLANE's normal vector.  The SPICELIB routine */
/*     FRAME does this for us.  NORMAL, SPAN1, and SPAN2 form a */
/*     right-handed orthonormal system upon output from FRAME. */

    frame_(normal, span1, span2);
    return 0;
} /* pl2psv_ */

