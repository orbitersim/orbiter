/* zzinpdt0.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static doublereal c_b5 = 1e-12;

/* $Procedure ZZINPDT0 ( DSK, in planetodetic element, w/o margin? ) */
/* Subroutine */ int zzinpdt0_(doublereal *p, doublereal *lon, doublereal *
	bounds, doublereal *corpar, integer *exclud, logical *inside)
{
    /* Initialized data */

    static logical first = TRUE_;
    static doublereal pi2 = -1.;

    /* System generated locals */
    doublereal d__1, d__2, d__3;

    /* Local variables */
    doublereal emin, emax, pmin, pmax;
    extern /* Subroutine */ int zzellbds_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *), zzpdcmpl_(doublereal *, doublereal *,
	     doublereal *, doublereal *, integer *);
    doublereal f;
    extern /* Subroutine */ int zznrmlon_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *), chkin_(char *, ftnlen);
    doublereal level;
    extern doublereal twopi_(void);
    extern logical failed_(void);
    doublereal re, rp;
    extern doublereal halfpi_(void);
    doublereal loclon, maxlat, maxalt, maxlon, minlat, minlon, minalt;
    integer maxrel, minrel;
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     Test a point represented by a set of planetodetic coordinates for */
/*     inclusion in a specified planetodetic volume element. Ellipsoidal */
/*     surfaces are used in place of surfaces of constant altitude. The */
/*     test is performed without using the "greedy" margins for the */
/*     element. The built-in angular rounding tolerance is still used */
/*     for the longitude comparison. */

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

/*     DSK */
/*     GEOMETRY */
/*     INTERSECTION */
/*     SURFACE */
/*     TOPOGRAPHY */

/* $ Declarations */

/*     File: dsktol.inc */


/*     This file contains declarations of tolerance and margin values */
/*     used by the DSK subsystem. */

/*     It is recommended that the default values defined in this file be */
/*     changed only by expert SPICE users. */

/*     The values declared in this file are accessible at run time */
/*     through the routines */

/*        DSKGTL  {DSK, get tolerance value} */
/*        DSKSTL  {DSK, set tolerance value} */

/*     These are entry points of the routine DSKTOL. */

/*        Version 1.0.0 27-FEB-2016 (NJB) */




/*     Parameter declarations */
/*     ====================== */

/*     DSK type 2 plate expansion factor */
/*     --------------------------------- */

/*     The factor XFRACT is used to slightly expand plates read from DSK */
/*     type 2 segments in order to perform ray-plate intercept */
/*     computations. */

/*     This expansion is performed to prevent rays from passing through */
/*     a target object without any intersection being detected. Such */
/*     "false miss" conditions can occur due to round-off errors. */

/*     Plate expansion is done by computing the difference vectors */
/*     between a plate's vertices and the plate's centroid, scaling */
/*     those differences by (1 + XFRACT), then producing new vertices by */
/*     adding the scaled differences to the centroid. This process */
/*     doesn't affect the stored DSK data. */

/*     Plate expansion is also performed when surface points are mapped */
/*     to plates on which they lie, as is done for illumination angle */
/*     computations. */

/*     This parameter is user-adjustable. */


/*     The keyword for setting or retrieving this factor is */


/*     Greedy segment selection factor */
/*     ------------------------------- */

/*     The factor SGREED is used to slightly expand DSK segment */
/*     boundaries in order to select segments to consider for */
/*     ray-surface intercept computations. The effect of this factor is */
/*     to make the multi-segment intercept algorithm consider all */
/*     segments that are sufficiently close to the ray of interest, even */
/*     if the ray misses those segments. */

/*     This expansion is performed to prevent rays from passing through */
/*     a target object without any intersection being detected. Such */
/*     "false miss" conditions can occur due to round-off errors. */

/*     The exact way this parameter is used is dependent on the */
/*     coordinate system of the segment to which it applies, and the DSK */
/*     software implementation. This parameter may be changed in a */
/*     future version of SPICE. */


/*     The keyword for setting or retrieving this factor is */


/*     Segment pad margin */
/*     ------------------ */

/*     The segment pad margin is a scale factor used to determine when a */
/*     point resulting from a ray-surface intercept computation, if */
/*     outside the segment's boundaries, is close enough to the segment */
/*     to be considered a valid result. */

/*     This margin is required in order to make DSK segment padding */
/*     (surface data extending slightly beyond the segment's coordinate */
/*     boundaries) usable: if a ray intersects the pad surface outside */
/*     the segment boundaries, the pad is useless if the intercept is */
/*     automatically rejected. */

/*     However, an excessively large value for this parameter is */
/*     detrimental, since a ray-surface intercept solution found "in" a */
/*     segment can supersede solutions in segments farther from the */
/*     ray's vertex. Solutions found outside of a segment thus can mask */
/*     solutions that are closer to the ray's vertex by as much as the */
/*     value of this margin, when applied to a segment's boundary */
/*     dimensions. */

/*     The keyword for setting or retrieving this factor is */


/*     Surface-point membership margin */
/*     ------------------------------- */

/*     The surface-point membership margin limits the distance */
/*     between a point and a surface to which the point is */
/*     considered to belong. The margin is a scale factor applied */
/*     to the size of the segment containing the surface. */

/*     This margin is used to map surface points to outward */
/*     normal vectors at those points. */

/*     If this margin is set to an excessively small value, */
/*     routines that make use of the surface-point mapping won't */
/*     work properly. */


/*     The keyword for setting or retrieving this factor is */


/*     Angular rounding margin */
/*     ----------------------- */

/*     This margin specifies an amount by which angular values */
/*     may deviate from their proper ranges without a SPICE error */
/*     condition being signaled. */

/*     For example, if an input latitude exceeds pi/2 radians by a */
/*     positive amount less than this margin, the value is treated as */
/*     though it were pi/2 radians. */

/*     Units are radians. */


/*     This parameter is not user-adjustable. */

/*     The keyword for retrieving this parameter is */


/*     Longitude alias margin */
/*     ---------------------- */

/*     This margin specifies an amount by which a longitude */
/*     value can be outside a given longitude range without */
/*     being considered eligible for transformation by */
/*     addition or subtraction of 2*pi radians. */

/*     A longitude value, when compared to the endpoints of */
/*     a longitude interval, will be considered to be equal */
/*     to an endpoint if the value is outside the interval */
/*     differs from that endpoint by a magnitude less than */
/*     the alias margin. */


/*     Units are radians. */


/*     This parameter is not user-adjustable. */

/*     The keyword for retrieving this parameter is */


/*     End of include file dsktol.inc */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     P          I   Cartesian coordinates of point. */
/*     LON        I   Planetocentric longitude of point. */
/*     BOUNDS     I   Coordinate bounds of element. */
/*     CORPAR     I   Coordinate parameters. */
/*     EXCLUD     I   Index of coordinate to exclude from test. */
/*     INSIDE     O   Flag indicating whether point is in element. */
/*     ANGMRG     P   Angular rounding margin. */
/*     LONALI     P   Longitude alias margin. */
/*     NONE       P   Meaning: exclude nothing. */
/*     LONIDX     P   Longitude index. Also used for exclusion. */
/*     LATIDX     P   Latitude index. Also used for exclusion. */
/*     ALTIDX     P   Altitude index. Also used for exclusion. */

/* $ Detailed_Input */

/*     P          is a point expressed in Cartesian coordinates. The */
/*                reference frame is that of the segment of interest. */
/*                The point is to be checked to determine whether it is */
/*                inside the volume element specified by BOUNDS. */


/*     LON        is the planetocentric longitude of the point P. */


/*     BOUNDS     is an 2x3 array containing the bounds of a volume */
/*                element expressed in planetodetic coordinates. BOUNDS */
/*                defines the volume element used in the comparison. In */
/*                the element */

/*                   BOUNDS(I,J) */

/*                J is the coordinate index. J is one of */

/*                   { LONIDX, LATIDX, ALTIDX } */

/*                I is the bound index. */

/*                   I = 1   ->   lower bound */
/*                   I = 2   ->   upper bound */

/*                If necessary, the longitude bounds are normalized */
/*                for the comparison. The normalized bounds differ */
/*                by no more than 2*pi and are in increasing order. */
/*                See ZZNRMLON for details. */

/*                The aliasing margin LONALI and the angular rounding */
/*                margin ANGMRG are used for longitude comparisons. */
/*                However, the greedy margin is not used. See the */
/*                Parameters section below for further information. */


/*     CORPAR     is an array of coordinate parameters; normally these */
/*                are obtained from a DSK descriptor. The first element */
/*                is the equatorial radius of the reference spheroid. */
/*                The second element is the flattening coefficient. */
/*                Any other elements are ignored by this routine. */


/*     EXCLUD     is either a coordinate index or the parameter NONE. */

/*                If EXCLUD is set to one of */

/*                   { LONIDX, LATIDX, ALTIDX } */

/*                then the indicated coordinate is excluded from */
/*                comparison with the corresponding volume element */
/*                boundaries. */

/*                If EXCLUD is set to NONE, all coordinates are */
/*                compared. */

/*                Exclusion of coordinates is used in cases where a */
/*                point is known to be on a level surface of a given */
/*                coordinate. For example, if a point is on the surface */
/*                of altitude equal to the upper altitude bound, */
/*                altitude need not be used in the comparison and in */
/*                fact can't be meaningfully compared, due to round-off */
/*                errors. */

/* $ Detailed_Output */

/*     INSIDE     is a logical flag that is set to .TRUE. if and */
/*                only if the input coordinates represent a */
/*                point inside or on the surface of the volume */
/*                element, according to the comparisons that are */
/*                performed. */

/*                The value of INSIDE is not affected by the value */
/*                of any excluded coordinate. */

/* $ Parameters */

/*     LONALI     is a longitude shift margin. If the input longitude */
/*                coordinate LON is less than the longitude lower bound */
/*                by more than this amount, the value LON + 2*pi is used */
/*                in the longitude comparison. If the input longitude */
/*                exceeds the upper longitude bound by this amount, the */
/*                value LON - 2*pi is used in the comparison. */

/*     ANGMRG     is an angular rounding margin. If the input longitude */
/*                is outside of the longitude bounds by less than this */
/*                amount, the longitude is considered to be within the */
/*                bounds. The margin is used analogously for latitude. */

/*                This margin is distinct from the "greedy" segment */
/*                selection margin, which is used to expand a volume */
/*                element for the purpose of determining whether a given */
/*                point lies in that element. */

/*                See dsktol.inc for details. */

/*     NONE       when used as a value of the input argument EXCLUD, */
/*                indicates that no coordinates should be excluded from */
/*                comparison. */

/*     LONIDX     is the index of longitude in the second dimension of */
/*                BOUNDS. When used as a value of the input argument */
/*                EXCLUD, indicates that longitude should be excluded */
/*                from comparison. */

/*     LATIDX     is the index of latitude in the second dimension of */
/*                BOUNDS. When used as a value of the input argument */
/*                EXCLUD, indicates that latitude should be excluded */
/*                from comparison. */


/*     ALTIDX     is the index of altitude in the second dimension of */
/*                BOUNDS. When used as a value of the input argument */
/*                EXCLUD, indicates that altitude should be excluded */
/*                from comparison. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine uses ellipsoidal bounding surfaces, in place of */
/*     surfaces of constant altitude, to represent the "outer" and */
/*     "inner" segment boundaries. These surfaces must be consistent */
/*     with those used by the ray-planetodetic volume element intercept */
/*     routine ZZRYTPDT. */

/*     The radii of these ellipsoidal surfaces are those produced by */
/*     the routine ZZELLBDS. */

/*     Note that a margin is used for longitude shifting, although */
/*     none is used to expand the volume element. */

/* $ Examples */

/*     See usage in ZZINPDT. */

/* $ Restrictions */

/*     1)  Validity of the ellipsoid shape parameters must be ensured by */
/*         the caller. */

/*     2)  This is a private routine. It is meant to be used only by the */
/*         DSK subsystem. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 16-AUG-2016 (NJB) */

/*        Now uses ANGMRG for latitude comparisons. */

/*        25-MAY-2016 (NJB) */

/*           Original version. */

/* -& */
/* $ Index_Entries */

/*     test point against planetodetic element without margin */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Numeric relation codes returned by ZZPDCMPL: */


/*     Local variables */


/*     Saved variables */


/*     Initial values */

    if (return_()) {
	return 0;
    }
    chkin_("ZZINPDT0", (ftnlen)8);
    if (first) {
	pi2 = twopi_();
	first = FALSE_;
    }

/*     Unpack the shape parameters. Set the polar axis length for */
/*     later use. */

    re = corpar[0];
    f = corpar[1];
    rp = re * (1. - f);

/*     Assume the point is outside to start. This allows us */
/*     to skip setting INSIDE when we find a boundary test */
/*     failure. */

    *inside = FALSE_;

/*     Compare coordinates of the input point and the segment */
/*     bounds. Deal with altitude last, since it involves the */
/*     most work. We may be able to exit before performing the */
/*     altitude tests. */

    if (*exclud != 2) {

/*        Compare the point's latitude to the segment's latitude bounds. */

/* Computing MAX */
	d__1 = bounds[2] - 1e-12, d__2 = -halfpi_();
	minlat = max(d__1,d__2);
/* Computing MIN */
	d__1 = bounds[3] + 1e-12, d__2 = halfpi_();
	maxlat = min(d__1,d__2);
	zzpdcmpl_(&re, &f, p, &minlat, &minrel);
	zzpdcmpl_(&re, &f, p, &maxlat, &maxrel);
	if (failed_()) {
	    chkout_("ZZINPDT0", (ftnlen)8);
	    return 0;
	}
	if (minrel == -1 || maxrel == 1) {

/*           The point's latitude is outside of the segment's range. */

	    chkout_("ZZINPDT0", (ftnlen)8);
	    return 0;
	}
    }

/*     Move the longitude of the input point into the interval */

/*        [ MINLON, MAXLON ] */

/*     if necessary and if possible. */

    if (*exclud != 1) {

/*        Put the local longitude bounds in order, if necessary. */

	zznrmlon_(bounds, &bounds[1], &c_b5, &minlon, &maxlon);

/*        Compare the point's longitude to the segment's longitude */
/*        bounds. */

	loclon = *lon;
	if (*lon < minlon - 1e-12) {

/*           If the point's longitude is less than the segment's */
/*           longitude by more than a small margin, shift the longitude */
/*           right by 2*pi. */
	    loclon = *lon + pi2;
	} else if (*lon > maxlon + 1e-12) {

/*           If the point's longitude is greater than the segment's */
/*           longitude by more than a small margin, shift the longitude */
/*           left by 2*pi. */
	    loclon = *lon - pi2;
	}
	if (loclon < minlon - 1e-12 || loclon > maxlon + 1e-12) {

/*           The point's longitude, adjusted if necessary for */
/*           comparison, is outside of the segment's range. */

	    chkout_("ZZINPDT0", (ftnlen)8);
	    return 0;
	}
    }
    if (*exclud != 3) {

/*        Extract altitude bounds from the segment descriptor. */

	minalt = bounds[4];
	maxalt = bounds[5];

/*        Find the semi-axes of the bounding spheroids. */

	if (f >= 0.) {

/*           This is the oblate case. Get the semi-axis lengths */
/*           for the inner and outer bounding spheroids. */

	    zzellbds_(&re, &rp, &maxalt, &minalt, &emax, &pmax, &emin, &pmin);
	} else {

/*           This is the prolate case. RP is the longer axis. Get the */
/*           semi-axis lengths for the inner and outer bounding */
/*           spheroids. */

/*           In this call, we'll store the radii associated with */
/*           the longer axis in PMAX and PMIN. */

	    zzellbds_(&rp, &re, &maxalt, &minalt, &pmax, &emax, &pmin, &emin);
	}

/*        Compute the input point's level surface parameters */
/*        for the inner and outer bounding ellipsoids. Do these */
/*        computations one at a time, so the second one can be */
/*        skipped if the first one shows the point is outside */
/*        the outer ellipsoid. */

/* Computing 2nd power */
	d__1 = p[0] / emax;
/* Computing 2nd power */
	d__2 = p[1] / emax;
/* Computing 2nd power */
	d__3 = p[2] / pmax;
	level = d__1 * d__1 + d__2 * d__2 + d__3 * d__3;
	if (level > 1.) {

/*           The point is outside of the outer bounding ellipsoid. */

	    chkout_("ZZINPDT0", (ftnlen)8);
	    return 0;
	}
/* Computing 2nd power */
	d__1 = p[0] / emin;
/* Computing 2nd power */
	d__2 = p[1] / emin;
/* Computing 2nd power */
	d__3 = p[2] / pmin;
	level = d__1 * d__1 + d__2 * d__2 + d__3 * d__3;
	if (level < 1.) {

/*           The point is inside the inner bounding ellipsoid, which */
/*           implies it is outside of the segment's boundaries. */

	    chkout_("ZZINPDT0", (ftnlen)8);
	    return 0;
	}
    }

/*     Getting to this point means the input point is inside */
/*     the segment. Being on the boundary counts as inside. */

    *inside = TRUE_;
    chkout_("ZZINPDT0", (ftnlen)8);
    return 0;
} /* zzinpdt0_ */

