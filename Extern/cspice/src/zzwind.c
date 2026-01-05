/* zzwind.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure  ZZWIND ( Find winding number of polygon about point ) */
integer zzwind_(doublereal *plane, integer *n, doublereal *vertcs, doublereal 
	*point)
{
    /* System generated locals */
    integer ret_val, i__1;
    doublereal d__1;

    /* Builtin functions */
    integer i_dnnt(doublereal *);

    /* Local variables */
    doublereal rvec[3], cons;
    extern doublereal vdot_(doublereal *, doublereal *), vsep_(doublereal *, 
	    doublereal *);
    extern /* Subroutine */ int vsub_(doublereal *, doublereal *, doublereal *
	    ), vequ_(doublereal *, doublereal *);
    integer i__, j;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    doublereal rperp[3], vtemp[3];
    extern /* Subroutine */ int vperp_(doublereal *, doublereal *, doublereal 
	    *), ucrss_(doublereal *, doublereal *, doublereal *);
    doublereal rnext[3];
    extern doublereal twopi_(void);
    extern logical vzero_(doublereal *);
    extern /* Subroutine */ int pl2nvc_(doublereal *, doublereal *, 
	    doublereal *);
    doublereal atotal, normal[3];
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int vminus_(doublereal *, doublereal *);
    doublereal sep;

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Find the winding number of a planar polygon, embedded in */
/*     3-dimensional space, about a specified point. */

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

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     PLANE      I   A SPICELIB plane. */
/*     N          I   Number of vertices of polygon. */
/*     VERTCS     I   Vertices of polygon. */
/*     POINT      I   Point in PLANE. */
/*     UBPL       P   Upper bound of SPICELIB plane array. */

/*     The function returns the winding number of the input polygon */
/*     about the input point. */

/* $ Detailed_Input */

/*     PLANE          is a SPICELIB plane containing a polygon and */
/*                    a point. */

/*     N, */
/*     VERTCS         are, respectively, the number vertices defining */
/*                    the polygon and the vertices themselves.  Each */
/*                    pair of consecutive vectors in the array VERTCS */
/*                    defines an edge of the polygon. */

/*     POINT          is a point lying in PLANE; the winding number */
/*                    of the polygon about POINT is sought. */

/* $ Detailed_Output */

/*     The function returns the winding number of the input polygon */
/*     about the input point.  The winding number measures the "net" */
/*     number of times the polygon wraps around POINT:  this is */
/*     the number of times the polygon wraps around POINT in the */
/*     counterclockwise sense minus the number of times the polygon */
/*     wraps around POINT in the clockwise sense. */

/*     The possible values and meanings of the winding number are: */

/*        ZZWIND > 0:    The polygon winds about POINT a total */
/*                       of ZZWIND times in the counterclockwise */
/*                       direction. */

/*                       POINT is inside the polygon. */


/*        ZZWIND < 0:    The polygon winds about POINT a total */
/*                       of ZZWIND times in the clockwise */
/*                       direction. */

/*                       POINT is inside the polygon. */


/*        ZZWIND = 0:    The number of times the polygon wraps around */
/*                       POINT in the counterclockwise sense is equal */
/*                       to the number of times the polygon wraps around */
/*                       POINT in the clockwise sense. */

/*                       POINT is outside the polygon. */

/* $ Parameters */

/*     UBPL           is the array upper bound for SPICELIB planes. */

/* $ Exceptions */


/*     1)  If the number of boundary vectors N is not at least 3, */
/*         or if the number exceeds MAXFOV, the error */
/*         SPICE(INVALIDCOUNT) will be signaled. */

/*     2)  The input point and vertices are expected to lie in */
/*         the input plane.  To avoid problems introduced by */
/*         round-off errors, all of these vectors are projected */
/*         orthogonally onto the plane before the winding number */
/*         is computed.  If the input point or vertices are "far" */
/*         from the input plane, no error will be signaled. */

/*     3)  If the input plane as a zero normal vector, the error */
/*         SPICE(ZEROVECTOR) will be signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Find the winding number of a 3-D polygon about a specified */
/*     point.  Although in principle the polygon is two-dimensional, */
/*     it is embedded in 3-space. */

/*     This routine supports determination of whether an ellipsoidal */
/*     body is in the field of view of a remote-sensing instrument */
/*     with a field of view having polygonal cross section. */

/*     The winding number is actually defined for closed, piecewise */
/*     differentiable curves in the complex plane.  If z(t), t in */
/*     [0, 2*Pi], is a parameterization of such a curve, then if the */
/*     symbol I is used to represent the integration operator, z0 is the */
/*     complex point of interest, and w is the winding number, we have */

/*        w = ( argument of z(2*pi) - argument of z(0) ) / (2*pi) */

/*          = (1/i) * ( log( z(2*pi)-z0 ) - log( z(0)-z0 ) ) / (2*pi) */

/*     Note the above is true because the curve is closed, so the real */
/*     parts of the logarithms cancel.  Then */

/*                1 */
/*        w =  -------  *  I  ( d ( log(z-z0) ) ) */
/*             2*Pi*i     z(t) */


/*                1 */
/*          =  -------  *  I  ( ( 1 / (z-z0) ) dz ) */
/*             2*Pi*i     z(t) */


/*     Because of Cauchy's theorem, we can transform the problem, */
/*     without loss of generality (leaving out *many* steps here), to */
/*     one for which the curve has the simple form */

/*                        i n*(t-t0) */
/*        z(t) =  z0 + r e */

/*     for some real values r, n, and t0.  So */


/*             1 */
/*      w = -------  *  I  ( 1 / (z-z0) ) */
/*          2*Pi*i     z(t) */


/*             1      t=2*pi        i n*(t-t0)           i n*(t-t0) */
/*        = ------- *   I   ( (1/r e         ) * ( r i n e          )dt ) */
/*          2*Pi*i     t=0 */


/*             1     t=2*pi */
/*        = ------- *  I (   i n dt ) */
/*          2*Pi*i    t=0 */

/*             1 */
/*        = ------  *  ( 2 * Pi * i * n ) */
/*          2*Pi*i */


/*        =    n */


/*     Given the simplified form of z(t) we've chosen, it's now clear */
/*     that n is the winding number. */

/*     In the simple case of a polygonal curve, the integral can */
/*     be computed from the original definition of the winding number: */

/*        w = ( argument of z(2*pi) - argument of z(0) ) / (2*pi) */

/*     The difference of arguments */

/*        argument of z(2*pi) - argument of z(0) */

/*     can be expressed as the telescoping sum */

/*         N */
/*        ___ */
/*        \ */
/*        /    ( argument of vertex(i+1) - argument of vertex(i) ) */
/*        --- */
/*        i=1 */

/*     where vertex N+1 is considered identical to vertex 1. */


/* $ Examples */

/*     See usage in ZZELVUPY. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     [1] `Calculus and Analytic Geometry', Thomas and Finney. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 11-AUG-2005 (NJB) */

/* -& */
/* $ Index_Entries */

/*     find winding number of polygon about point */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Initialize the function return value. */

    ret_val = 0;
    if (return_()) {
	return ret_val;
    }
    chkin_("ZZWIND", (ftnlen)6);

/*     Check the number of sides of the polygon. */

    if (*n < 3) {
	setmsg_("Polygon must have at least 3 sides; N = #.", (ftnlen)42);
	errint_("#", n, (ftnlen)1);
	sigerr_("SPICE(DEGENERATECASE)", (ftnlen)21);
	chkout_("ZZWIND", (ftnlen)6);
	return ret_val;
    }

/*     Unpack the plane's normal and constant. */

    pl2nvc_(plane, normal, &cons);

/*     Check the normal vector. */

    if (vzero_(normal)) {
	setmsg_("Plane's normal vector is zero.", (ftnlen)30);
	sigerr_("SPICE(ZEROVECTOR)", (ftnlen)17);
	chkout_("ZZWIND", (ftnlen)6);
	return ret_val;
    }

/*     We want the normal vector to point on the same side of the */
/*     plane as the boundary vectors.  Negate the normal */
/*     if necessary to make this true.  We don't touch CONS because */
/*     it's not used later, but in principle it should be negated. */

    if (vdot_(normal, vertcs) < 0.) {
	vminus_(normal, vtemp);
	vequ_(vtemp, normal);
    }

/*     Find the angular argument of each point; find the difference */
/*     of this angle from the preceding angle; add the difference to */
/*     the total. */

    vsub_(vertcs, point, vtemp);

/*     Get the component RVEC of the difference vector orthogonal to */
/*     the plane's normal vector. */

    vperp_(vtemp, normal, rvec);

/*     The total "wrap angle" starts at zero. */

    atotal = 0.;
    i__1 = *n + 1;
    for (i__ = 2; i__ <= i__1; ++i__) {
	if (i__ <= *n) {
	    j = i__;
	} else {
	    j = 1;
	}

/*        Find the angular separation of RVEC and the next vector */
/*        RNEXT. */

	vsub_(&vertcs[j * 3 - 3], point, vtemp);
	vperp_(vtemp, normal, rnext);
	sep = vsep_(rnext, rvec);

/*        Create a normal vector to RVEC by rotating RVEC pi/2 radians */
/*        counterclockwise.  We'll use this vector RPERP to determine */
/*        whether the next point is reached by clockwise or */
/*        counterclockwise rotation from RVEC. */

	ucrss_(normal, rvec, rperp);
	if (vdot_(rnext, rperp) >= 0.) {

/*           RNEXT is reached by counterclockwise rotation from */
/*           RVEC.  Note that in the case of zero rotation, the */
/*           sign doesn't matter because the contribution is zero. */

	    atotal += sep;
	} else {
	    atotal -= sep;
	}

/*        Update RVEC. */

	vequ_(rnext, rvec);
    }

/*     The above sum is 2 * pi * <the number of times the polygon */
/*     wraps around P>.  Let ZZWIND be the wrap count. */

    d__1 = atotal / twopi_();
    ret_val = i_dnnt(&d__1);
    chkout_("ZZWIND", (ftnlen)6);
    return ret_val;
} /* zzwind_ */

