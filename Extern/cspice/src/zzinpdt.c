/* zzinpdt.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static doublereal c_b16 = 1e-12;

/* $Procedure ZZINPDT ( DSK, in planetodetic element? ) */
/* Subroutine */ int zzinpdt_(doublereal *p, doublereal *bounds, doublereal *
	corpar, doublereal *margin, integer *exclud, logical *inside)
{
    /* Initialized data */

    static logical first = TRUE_;
    static doublereal hpi = -1.;

    /* System generated locals */
    doublereal d__1, d__2;

    /* Builtin functions */
    double cos(doublereal);

    /* Local variables */
    doublereal dlon;
    extern /* Subroutine */ int zzinpdt0_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, integer *, logical *), zzpdcmpl_(
	    doublereal *, doublereal *, doublereal *, doublereal *, integer *)
	    ;
    doublereal f;
    extern /* Subroutine */ int zznrmlon_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *);
    doublereal r__;
    extern /* Subroutine */ int chkin_(char *, ftnlen), errdp_(char *, 
	    doublereal *, ftnlen);
    extern doublereal twopi_(void);
    extern logical failed_(void);
    doublereal re;
    extern doublereal pi_(void), halfpi_(void);
    static doublereal altbds[6]	/* was [2][3] */;
    doublereal amnalt, amnlat, amnlon, amxalt, amxlat, amxlon, lonmrg, maxalt,
	     maxlat, maxlon, minalt, minlat, minlon, pcnlat;
    static doublereal pi2;
    integer relmin;
    extern logical return_(void);
    integer relmax;
    logical alpass;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen), sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), reclat_(doublereal *, doublereal *, doublereal *, 
	    doublereal *);
    doublereal lon;

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     Test a point represented by a set of planetodetic coordinates for */
/*     inclusion in a specified planetodetic volume element. Ellipsoidal */
/*     surfaces are used in place of surfaces of constant altitude. The */
/*     test is performed using margins for the element. */

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
/*     P          I   Input point. */
/*     BOUNDS     I   Coordinate bounds of element. */
/*     CORPAR     I   Coordinate system parameters. */
/*     MARGIN     I   Margin used for inclusion testing. */
/*     EXCLUD     I   Index of coordinate to exclude from test. */
/*     INSIDE     O   Flag indicating whether point is in element. */
/*     LONALI     P   Longitude shift margin. */
/*     NONE       P   Meaning: exclude nothing. */
/*     LONIDX     P   Longitude index. Also used for exclusion. */
/*     LATIDX     P   Latitude index. Also used for exclusion. */
/*     ALTIDX     P   Altitude index. Also used for exclusion. */
/*     LATMRG     P   High/low latitude margin. */

/* $ Detailed_Input */

/*     P          is a point expressed in Cartesian coordinates. The */
/*                point is to be checked to determine whether it is */
/*                inside the volume element specified by BOUNDS. */

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

/*                If the longitude upper bound is not greater than the */
/*                longitude lower bound, a value greater than the upper */
/*                bound by 2*pi is used for the comparison. */


/*     CORPAR     is an array of coordinate parameters; normally these */
/*                are obtained from a DSK descriptor. The first element */
/*                is the equatorial radius of the reference spheroid. */
/*                The second element is the flattening coefficient. */
/*                Any other elements are ignored by this routine. */


/*     MARGIN     is a fraction used to expand the volume element for */
/*                inclusion testing. */

/*                   - The upper altitude bound is increased by */

/*                        MARGIN*ABS( BOUNDS(2,3) ) */

/*                     The lower altitude bound is decreased by */

/*                        MARGIN*ABS( BOUNDS(1,3) ) */

/*                     Note that these bounds are used as inputs to */
/*                     compute the radii of upper and lower bounding */
/*                     ellipsoids. The input point's position is tested */
/*                     for inclusion within or exclusion from these */
/*                     ellipsoids. */


/*                   - Latitude bounds are extended by MARGIN radians, */
/*                     within the interval -pi/2 : pi/2. */

/*                   - For any input point having latitude "not close" */
/*                     to pi/2 or -pi/2, the element's longitude */
/*                     interval is extended by */

/*                        MARGIN / cos(LAT) radians */

/*                     where LAT is the planetocentric [sic] latitude of */
/*                     the input point. */

/*                     Here "close to" means "within LATMRG radians of." */

/*                   - For any input point having planetocentric */
/*                     latitude close to (within LATMRG radians of) pi/2 */
/*                     or -pi/2, comparison against the longitude bounds */
/*                     of the volume element is omitted if MARGIN > 0. */


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
/*                coordinate. For example, if a point is on the outer */
/*                bounding ellipsoid, the level surface parameter of the */
/*                point with respect to the ellipsoid need not be used */
/*                in the comparison and in fact can't be meaningfully */
/*                compared, due to round-off errors. */

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
/*                by more than this amount, the value LON + 2*pi is */
/*                used in the longitude comparison, provided that the */
/*                comparison is not excluded. */

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


/*     LATMRG     is the margin used to determine whether the input */
/*                latitude is "close" to the poles. Units are radians. */
/*                The value of this parameter must be kept in sync */
/*                with the "greedy" margin declared in dsktol.inc. */

/* $ Exceptions */

/*     1)  If MARGIN is negative, the error SPICE(VALUEOUTOFRANGE) */
/*         is signaled. */

/*     2)  If EXCLUD is outside of the range 0:3, the error */
/*         SPICE(VALUEOUTOFRANGE) is signaled. */

/*     3)  If an error occurs while determining the planetodetic */
/*         coordinates of the input point, the error will be signaled by */
/*         a routine in the call tree of this routine. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     See usage in ZZRYTPDT. */

/* $ Restrictions */

/*     1)  This is a private routine. It is meant to be used only by the */
/*         DSK subsystem. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 13-FEB-2017 (NJB) */

/*        03-JUN-2016 (NJB) */

/*           Added standard traceback participation. Added check for */
/*           invalid values of EXCLUD. */

/*        05-MAR-2016 (NJB) */

/*           Original version 03-OCT-2014 (NJB) */

/* -& */
/* $ Index_Entries */

/*     test point against planetodetic element using margin */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Element boundary indices: */


/*     Numeric relation codes returned by ZZPDCMPL: */


/*     The code EQ can be returned by ZZPDCMPL, but we make no */
/*     references to it, so it's not declared here. For the */
/*     record, EQ is set to 0. */


/*     Local variables */


/*     Saved variables */


/*     Initial values */

    if (return_()) {
	return 0;
    }
    chkin_("ZZINPDT", (ftnlen)7);
    if (first) {
	hpi = halfpi_();
	pi2 = twopi_();

/*        Initialize the local array used for altitude checks. */

	altbds[2] = -halfpi_();
	altbds[3] = halfpi_();
	altbds[0] = -pi_();
	altbds[1] = pi_();
	altbds[4] = 0.;
	altbds[5] = 0.;
	first = FALSE_;
    }
    if (*exclud < 0 || *exclud > 3) {
	setmsg_("EXCLUD must be in the range 0:3 but was #.", (ftnlen)42);
	errint_("#", exclud, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("ZZINPDT", (ftnlen)7);
	return 0;
    }

/*     Get the planetocentric [sic] coordinates of the input point. The */
/*     latitude we obtain will be planetocentric. To emphasize this, we */
/*     use the name "PCNLAT." */

    reclat_(p, &r__, &lon, &pcnlat);

/*     RECLAT is error free, so we don't call FAILED() here. */

    if (*margin == 0.) {

/*        ZZINPDT0 contains the logic required to determine whether */
/*        the input point is contained in the element. */

	zzinpdt0_(p, &lon, bounds, corpar, exclud, inside);
	chkout_("ZZINPDT", (ftnlen)7);
	return 0;
    } else if (*margin < 0.) {
	setmsg_("Margin must be non-negative but was #.", (ftnlen)38);
	errdp_("#", margin, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("ZZINPDT", (ftnlen)7);
	return 0;
    }

/*     At this point a more detailed analysis is needed. */


/*     Assume the point is outside to start. This allows us */
/*     to skip setting INSIDE when we find a boundary test */
/*     failure. */

    *inside = FALSE_;

/*     We'll use the shape parameters for latitude and altitude */
/*     comparisons. */

    re = corpar[0];
    f = corpar[1];

/*     Get local copies of the coordinate bounds. Don't normalize the */
/*     longitude bounds until we know we need them. */

    minlat = bounds[2];
    maxlat = bounds[3];
    minalt = bounds[4];
    maxalt = bounds[5];

/*     Compare coordinates to adjusted latitude boundaries. */

    if (*exclud != 2) {

/*        Create adjusted latitude bounds. */

/* Computing MAX */
	d__1 = -hpi - 1e-12, d__2 = minlat - *margin;
	amnlat = max(d__1,d__2);
/* Computing MIN */
	d__1 = hpi + 1e-12, d__2 = maxlat + *margin;
	amxlat = min(d__1,d__2);

/*        Compare the latitude of the input point to the bounds. */

	zzpdcmpl_(&re, &f, p, &amnlat, &relmin);
	zzpdcmpl_(&re, &f, p, &amxlat, &relmax);
	if (failed_()) {
	    chkout_("ZZINPDT", (ftnlen)7);
	    return 0;
	}
	if (relmin == -1 || relmax == 1) {

/*           The latitude of P is strictly outside of the element's */
/*           latitude bounds. */

	    chkout_("ZZINPDT", (ftnlen)7);
	    return 0;
	}
    }

/*     Test the point for inclusion in the region between the bounding */
/*     ellipsoids that act as proxies for altitude boundaries. */

    if (*exclud != 3) {

/*        Extract altitude bounds from the segment descriptor. */

	minalt = bounds[4];
	maxalt = bounds[5];

/*        Adjust altitude bounds to account for the margin. */

	amnalt = minalt - *margin * abs(minalt);
	amxalt = maxalt + *margin * abs(maxalt);

/*        Set up a "boundary" array so that we can use ZZINPDT0 */
/*        to do the altitude check for us. We'll exclude longitude */
/*        tests; the latitude test is set up for an automatic pass */
/*        (the latitude range of ALTBDS is [-pi/2, pi/2]). */

	altbds[4] = amnalt;
	altbds[5] = amxalt;
	zzinpdt0_(p, &lon, altbds, corpar, &c__1, &alpass);
	if (! alpass) {
	    chkout_("ZZINPDT", (ftnlen)7);
	    return 0;
	}
    }

/*     At this point, the input altitude and latitude are within the */
/*     adjusted bounds, if their tests haven't been excluded by */
/*     the caller. */

/*     Perform longitude tests, unless they're excluded by the */
/*     caller. */

    if (*exclud != 1) {

/*        Start out by normalizing the element's longitude bounds. */

	zznrmlon_(bounds, &bounds[1], &c_b16, &minlon, &maxlon);
	if (failed_()) {
	    chkout_("ZZINPDT", (ftnlen)7);
	    return 0;
	}

/*        Set the margin to be used for longitude interval */
/*        inclusion tests. */

/* Computing MAX */
	d__1 = 1e-12, d__2 = abs(*margin);
	lonmrg = max(d__1,d__2);

/*        We have a special case for segments that include the poles. If */
/*        the latitude and altitude of the input point are within */
/*        bounds, and if the latitude of the point is close enough to a */
/*        pole we consider the point to be included in the segment, */
/*        regardless of the point's longitude. All other points get the */
/*        normal longitude test. */

/*        We use planetocentric latitude to determine whether the point */
/*        is "close" to a pole. */

	d__1 = hpi - 1e-8;
	zzpdcmpl_(&re, &f, p, &d__1, &relmax);
	d__1 = -hpi + 1e-8;
	zzpdcmpl_(&re, &f, p, &d__1, &relmin);
	if (failed_()) {
	    chkout_("ZZINPDT", (ftnlen)7);
	    return 0;
	}
	if (relmax != 1 && relmin != -1) {

/*           This is the usual case: the latitude of the input point is */
/*           bounded away from the poles. */

/*           Check the point's longitude against the segment's longitude */
/*           bounds. */

/*           We'll scale the longitude margin to compensate for the */
/*           latitude of the input point. Note that the division */
/*           below is safe; presuming a reasonable value of MARGIN, */
/*           we know that */

/*              DLON << 1 */

/*           Note that we use planetocentric latitude for scaling the */
/*           longitude margin. This substitution (for planetodetic */
/*           latitude) is adequate for this purpose. */

/* Computing MAX */
	    d__2 = (d__1 = cos(pcnlat), abs(d__1));
	    dlon = lonmrg / max(d__2,1e-8);
	    amnlon = minlon - dlon;
	    amxlon = maxlon + dlon;

/*           Now move the input point's longitude into range, if */
/*           necessary, so we can make a valid comparison against */
/*           the longitude bounds. */

	    if (lon < amnlon) {
		if (lon < amnlon - 1e-12) {

/*                 See whether an aliased version of LON is a match. */

		    lon += pi2;
		} else {

/*                 Consider LON to be a match with the lower bound. */

		    lon = amnlon;
		}
	    } else if (lon > amxlon) {
		if (lon > amxlon + 1e-12) {

/*                 See whether an aliased version of LON is a match. */

		    lon -= pi2;
		} else {

/*                 Consider LON to be a match with the upper bound. */

		    lon = amxlon;
		}
	    }
	    if (lon < amnlon || lon > amxlon) {
		chkout_("ZZINPDT", (ftnlen)7);
		return 0;
	    }
	} else {

/*           The latitude of the input point is close to one of the */
/*           poles. */

/*           This is a no-op case. */

/*           The input point has already passed whichever of the */
/*           altitude and latitude tests that were not excluded. */

/*           If the element has a non-degenerate latitude boundary */
/*           having the same sign as the latitude of the input point, */
/*           and if latitude is excluded because the input point is */
/*           already nominally on that boundary, then passing the */
/*           altitude check implies that the point is close to the */
/*           element. */

/*           If the element has a degenerate latitude boundary having */
/*           the same sign as the latitude of the input point---namely, */
/*           the element contains the pole, and latitude is excluded, */
/*           then then passing the altitude check implies that the point */
/*           is close to the portion of the Z-axis contained in the */
/*           element. */

/*           If the altitude check has been excluded because the point */
/*           is already nominally on one of the element's altitude */
/*           boundaries, the passing the latitude test implies the point */
/*           is close to the element. */

/*           In all cases, as long as EXCLUD has been set appropriately, */
/*           the point is close to the element. We consider the point to */
/*           be in the expanded element. */

	}
    }

/*     All tests that were commanded have been passed. The input */
/*     point is considered to be contained in the expanded volume */
/*     element. */

    *inside = TRUE_;
    chkout_("ZZINPDT", (ftnlen)7);
    return 0;
} /* zzinpdt_ */

