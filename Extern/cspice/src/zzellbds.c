/* zzellbds.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ZZELLBDS ( Create bounding ellipsoids ) */
/* Subroutine */ int zzellbds_(doublereal *a, doublereal *b, doublereal *hmax,
	 doublereal *hmin, doublereal *amax, doublereal *bmax, doublereal *
	amin, doublereal *bmin)
{
    /* System generated locals */
    doublereal d__1;

    /* Local variables */
    extern /* Subroutine */ int chkin_(char *, ftnlen), errdp_(char *, 
	    doublereal *, ftnlen), sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen);

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Given an oblate spheroid and upper and lower height bounds */
/*     relative to that spheroid, determine radii of inner and outer */
/*     bounding spheroids. */

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

/*     DEM */
/*     DSK */
/*     ELLIPSOID */
/*     GEOMETRY */
/*     TOPOGRAPHY */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     A          I   Input spheroid's semi-major axis length. */
/*     B          I   Input spheroid's semi-minor axis length. */
/*     HMAX       I   Maximum height relative to input spheroid. */
/*     HMIN       I   Minimum height relative to input spheroid. */
/*     AMAX       O   Outer spheroid's semi-major axis length. */
/*     BMAX       O   Outer spheroid's semi-minor axis length. */
/*     AMIN       O   Inner spheroid's semi-major axis length. */
/*     BMIN       O   Inner spheroid's semi-minor axis length. */

/* $ Detailed_Input */

/*     A          is the semi-major axis length of a reference */
/*                spheroid. A may have any units; B, HMAX, and */
/*                HMIN must have the same units. */

/*     B          is the semi-minor axis length of a reference */
/*                spheroid. B must not exceed A. */

/*     HMAX       is an upper bound for a set of heights relative */
/*                the reference spheroid defined by A and B. HMAX */
/*                is a signed quantity. */

/*     HMIN       is a lower bound for a set of heights relative */
/*                the reference spheroid defined by A and B. HMIN */
/*                is a signed quantity. HMIN must not exceed HMAX. */
/*                HMIN must be greater than -B. */

/* $ Detailed_Output */

/*     AMAX, */
/*     BMAX       are, respectively, semi-major and semi-minor axis */
/*                lengths for an outer bounding spheroid. The set of */
/*                points at height HMAX relative to the input spheroid */
/*                defined by A and B lies on or below the spheroid */
/*                defined by AMAX and BMAX. */

/*                When HMAX is non-negative */

/*                   AMAX  =  A + HMAX */
/*                   BMAX  =  B + HMAX*(A/B) */

/*                When HMAX is negative */

/*                   AMAX  =  A + HMAX*(B/A) */
/*                   BMAX  =  B + HMAX */
/*     AMIN, */
/*     BMIN       are, respectively, semi-major and semi-minor axis */
/*                lengths for an inner bounding spheroid. The set of */
/*                points at height HMIN relative to the input spheroid */
/*                defined by A and B lies on or above the spheroid */
/*                defined by AMIN and BMIN. */

/*                When HMIN is non-positive */

/*                   AMIN  =  A + HMIN */
/*                   BMIN  =  B + HMIN*(A/B) */

/*                When HMIN is positive */

/*                   AMIN  =  A + HMIN*(B/A) */
/*                   BMIN  =  B + HMIN */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the semi-minor axis length B is non-positive, the */
/*         error SPICE(NONPOSITIVERADIUS) will be signaled. */

/*     2)  If the semi-major axis length A is less than the */
/*         semi-minor axis length B, the error SPICE(RADIIOUTOFORDER) */
/*         will be signaled. */

/*     3)  If HMIN is less than or equal to -B, the error */
/*         SPICE(LOWERBOUNDTOOLOW) will be signaled. */

/*     4)  If the lower height bound HMIN is greater than the */
/*         upper height bound HMAX, the error SPICE(BOUNDSOUTOFORDER) */
/*         will be signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Ellipsoidal bounding surfaces may be used for volume elements in */
/*     planetodetic coordinates. The combination of these surfaces, */
/*     together with cones of constant latitude and planes of constant */
/*     longitude, may be used to create a simple set of bounding */
/*     surfaces for such an element. These surfaces can be used to */
/*     rapidly determine that a given ray does not intersect the volume */
/*     element. */

/*     For a surface represented by a digital elevation model (DEM), */
/*     efficient solution of the ray-surface intercept problem is */
/*     enhanced by determination of a line segment---also called a */
/*     "chord"---outside of which no solution is possible. One way of */
/*     generating a chord is to determine the intersection of the ray */
/*     with inner or outer bounding surfaces. An inner bounding surface */
/*     is one which has, for every latitude and longitude covered by the */
/*     DEM, altitude less than or equal to that of the DEM. An outer */
/*     bounding surface has, for every latitude and longitude covered by */
/*     the DEM, altitude greater than or equal to the DEM. */

/*     In order for bounding surfaces to be useful, it must be possible */
/*     to rapidly compute intersections of rays with these surfaces. */
/*     The bounding surfaces must also lie "close" to the DEM surface; */
/*     otherwise chords formed by intersections of rays with the bounding */
/*     surfaces may be longer than necessary. */

/*     Spheroids are a natural choice for bounding surfaces, since DEMs */
/*     are typically referenced to a spheroidal surface, and because */
/*     ray-ellipsoid intersections can be rapidly computed by closed-form */
/*     algebraic methods. */

/*     It might seem that, given a spheroid with semi-major axis length */
/*     A and semi-minor axis length B, and given a surface having maximum */
/*     and minimum spheroid-relative heights HMAX and HMIN respectively, */
/*     that the level surfaces having heights HMAX and HMIN relative to */
/*     the spheroid would be candidates for the inner and outer bounding */
/*     spheroids. The outer spheroid would have radii A+HMAX and */
/*     B+HMAX, while the inner spheroid would have radii A+HMIN and */
/*     B+HMIN. However, it can be shown (by numerical comparison, for */
/*     example), that these spheroids do not bound the level surfaces */
/*     at heights HMAX and HMIN, except in certain special cases such as */
/*     that of a spherical reference surface. */

/*     This routine generates semi-axis lengths of inner and outer */
/*     bounding spheroids that are valid for all eccentricities of the */
/*     reference spheroid (as long as A >= B) and, for reference */
/*     spheroids of low eccentricity, lie close to the level surfaces at */
/*     heights HMIN and HMAX. A discussion of the method follows. */


/*     Explanation */
/*     =========== */

/*     Since we're working with spheroids, we can reduce the problem */
/*     to a two-dimensional one. We'll compute inner and outer bounding */
/*     ellipses for an ellipse with positive semi-axis lengths A and B. */
/*     Revolving the bounding ellipses about the Z axis creates bounding */
/*     ellipsoids. */

/*        Consider an ellipse E with vertical semi-minor axis length */
/*        B and horizontal semi-major axis A. For a point (x,y) on E, */
/*        let N be the normal vector */

/*                  x      y */
/*           N = ( ---- , ---- ) */
/*                   2      2 */
/*                  A      B */

/*        Let LAMBDA be a constant, and Let E' be the curve */

/*           { (x,y) + LAMBDA*N } */

/*        Then E' is an ellipse (since it's produced by a linear */
/*        transformation of E) with semi-axis lengths */

/*           (A + LAMBDA/A), (B + LAMBDA/B) */


/*        For any point (x,y) on E, the height of E' above that point is */

/*           LAMBDA*||N|| */

/*        The square of this height HSQ is */


/*                       2 */
/*           HSQ = LAMBDA  * <N, N> */


/*                               2      2 */
/*                       2      x      y */
/*               = LAMBDA  * ( ---- + ---- ) */
/*                               4      4 */
/*                              A      B */

/*                               2     2       2  2 */
/*                       2      x     B ( 1 - x /A ) */
/*               = LAMBDA  * ( ---- + -------------- ) */
/*                               4           4 */
/*                              A           B */

/*                               2 */
/*                       2      x     1        1        1 */
/*               = LAMBDA  * ( ---- (----  -  ----) +  ---- ) */
/*                               2     2        2        2 */
/*                              A     A        B        B */


/*        If A = B, this expression is constant. */

/*        If A > B, then the term */

/*           1/A**2 - 1/B**2 */

/*        is negative, so HSQ is a decreasing function of x for */

/*           0 <= x <= A */

/*        This implies that for x in the above range, E' is closest */
/*        to E when x = A. We'll use this fact to generate bounding */
/*        ellipsoids with the following properties: */

/*           If HMAX >= 0    E' will have height HMAX at x = A and */
/*                           height >= HMAX if 0 <= x < A. */

/*           If HMAX <  0    E' will have height HMAX at x = 0 and */
/*                           height >= HMAX if 0 < x <= A. */

/*           If HMIN <= 0    E' will have height HMIN at x = A and */
/*                           height <= HMIN if 0 <= x < A. */

/*           If HMIN >  0    E' will have height HMIN at x = 0 and */
/*                           height <= HMIN if 0 < x <= A. */



/*     Application to prolate spheroids */
/*     ================================ */

/*     For a spheroid having semi-axes A, B, C, for which */

/*        A = B < C */

/*     This routine can be applied to the semi-axis lengths */
/*     A' and B', where */

/*        A'  =  C */
/*        B'  =  A */

/*     The outer bounding surface is a prolate spheroid with semi-axis */
/*     lengths */

/*        BMAX, BMAX, AMAX */

/*     The inner bounding surface is a prolate spheroid with semi-axis */
/*     lengths */

/*        BMIN, BMIN, AMIN */


/* $ Examples */

/*     See usage in ZZRYXPDT. */

/* $ Restrictions */

/*     1)  The bounding spheroids generated by this routine may not be */
/*         suitable for reference spheroids with high eccentricity. */

/*     2)  Callers of this routine should not rely explicitly on the */
/*         formulas shown in the Detailed Output section of this */
/*         routine's header comments. The formulas could be upgraded */
/*         in a future version of this routine. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 13-JAN-2017 (NJB) */

/*        Added error check for oblate case where lower bounding */
/*        ellipsoid has negative polar radius. */

/*        Updated $Particulars; added mention of use for volume elements */
/*        in planetodetic coordinate systems. Remarks about application */
/*        to prolate spheroids were added. Changed contents of $Files */
/*        section to "None." Made miscellaneous small changes to */
/*        comments. */

/*     Original version 27-NOV-2012 (NJB) */

/* -& */
/* $ Index_Entries */

/*     create bounding ellipsoids for a dem */
/*     bounding ellipsoids for a digital elevation model */
/*     create bounding ellipsoids for a dsk type 4 segment */

/* -& */


/*     Use discovery check-in. */

    if (*b <= 0.) {
	chkin_("ZZELLBDS", (ftnlen)8);
	setmsg_("This routine requires B > 0, but B = #.", (ftnlen)39);
	errdp_("#", b, (ftnlen)1);
	sigerr_("SPICE(NONPOSITIVERADIUS)", (ftnlen)24);
	chkout_("ZZELLBDS", (ftnlen)8);
	return 0;
    }
    if (*b > *a) {
	chkin_("ZZELLBDS", (ftnlen)8);
	setmsg_("This routine requires A >= B, but A = #; B = #.", (ftnlen)47)
		;
	errdp_("#", a, (ftnlen)1);
	errdp_("#", b, (ftnlen)1);
	sigerr_("SPICE(RADIIOUTOFORDER)", (ftnlen)22);
	chkout_("ZZELLBDS", (ftnlen)8);
	return 0;
    }
    if (*b + *hmin <= 0.) {
	chkin_("ZZELLBDS", (ftnlen)8);
	setmsg_("This routine requires B + HMIN > 0, but B = #; HMIN = #, B+"
		"HMIN = #.", (ftnlen)68);
	errdp_("#", b, (ftnlen)1);
	errdp_("#", hmin, (ftnlen)1);
	d__1 = *b + *hmin;
	errdp_("#", &d__1, (ftnlen)1);
	sigerr_("SPICE(LOWERBOUNDTOOLOW)", (ftnlen)23);
	chkout_("ZZELLBDS", (ftnlen)8);
	return 0;
    }
    if (*hmin < 0.) {
	if (*b + *a / *b * *hmin <= 0.) {
	    chkin_("ZZELLBDS", (ftnlen)8);
	    setmsg_("For oblate spheroids and HMIN < 0, This routine require"
		    "s B + (A/B)HMIN > 0, but A = #, B = #; HMIN = #, B+(A/B)"
		    "HMIN = #.", (ftnlen)120);
	    errdp_("#", a, (ftnlen)1);
	    errdp_("#", b, (ftnlen)1);
	    errdp_("#", hmin, (ftnlen)1);
	    d__1 = *b + *a / *b * *hmin;
	    errdp_("#", &d__1, (ftnlen)1);
	    sigerr_("SPICE(LOWERBOUNDTOOLOW)", (ftnlen)23);
	    chkout_("ZZELLBDS", (ftnlen)8);
	    return 0;
	}
    }
    if (*hmin > *hmax) {
	chkin_("ZZELLBDS", (ftnlen)8);
	setmsg_("This routine requires HMAX >= HMIN, but HMIN = #; HMAX = #.",
		 (ftnlen)59);
	errdp_("#", hmin, (ftnlen)1);
	errdp_("#", hmax, (ftnlen)1);
	sigerr_("SPICE(BOUNDSOUTOFORDER)", (ftnlen)23);
	chkout_("ZZELLBDS", (ftnlen)8);
	return 0;
    }

/*     In the following comments, N, E, E', and LAMBDA are */
/*     defined as in the Particulars section above. */


/*     Generate radii of the outer bounding ellipsoid. */

    if (*hmax >= 0.) {

/*        Pick radii of E' so that E' matches */

/*           E + HMAX * N / ||N|| */

/*        that is, E' has height HMAX above E, at x=A. */

/*        For smaller x, the height of E' above E will */
/*        will be greater than or equal to HMAX. */

/*        Set LAMBDA = A * HMAX. */

/*        Then the radii of E' are */

/*                             | */
/*           x + LAMBDA*||N||  | */
/*                             |x=A,y=0 */

/*        and */
/*                             | */
/*           y + LAMBDA*||N||  | */
/*                             |x=0,y=B */

/*        so the radii of E', AMAX and BMAX, are: */

/*           AMAX =  A + LAMBDA*A/A**2  =  A + HMAX */
/*           BMAX =  B + LAMBDA*B/B**2  =  B + HMAX*(A/B) */


	*amax = *a + *hmax;
	*bmax = *b + *hmax * (*a / *b);
    } else {

/*        HMAX < 0. */

/*        In this case the outer bounding ellipse should match E+HMAX */
/*        at x = 0. The ellipse will be closer to E for x > 0. */

/*        Set LAMBDA = B * HMAX. Then */

/*           AMAX =  A + LAMBDA*A/A**2  =  A + HMAX * (B/A) */
/*           BMAX =  B + LAMBDA*B/B**2  =  B + HMAX */
	*amax = *a + *hmax * (*b / *a);
	*bmax = *b + *hmax;
    }

/*     Find radii of the inner bounding ellipsoid. */

    if (*hmin <= 0.) {

/*        This case is similar to that of the outer bounding */
/*        ellipsoid for HMAX >= 0. We can create an ellipse */
/*        that has height HMIN at x = A and that is further */
/*        from E for x < A. */

/*        Set LAMBDA = A * HMAX. Then */

/*           AMAX =  A + LAMBDA*A/A**2  =  A + HMAX */
/*           BMAX =  B + LAMBDA*B/B**2  =  B + HMAX*(A/B) */
	*amin = *a + *hmin;
	*bmin = *b + *hmin * (*a / *b);
    } else {

/*        HMIN > 0. */

/*        In this case the inner bounding ellipse should match E+HMIN */
/*        at x = 0. The ellipse will be closer to E for x > 0. */

/*        Set LAMBDA = B * HMAX. Then */

/*           AMIN =  A + LAMBDA*A/A**2  =  A + HMIN * (B/A) */
/*           BMIN =  B + LAMBDA*B/B**2  =  B + HMIN */

	*amin = *a + *hmin * (*b / *a);
	*bmin = *b + *hmin;
    }
    return 0;
} /* zzellbds_ */

