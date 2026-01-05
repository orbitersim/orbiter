/* zzinlat.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static doublereal c_b12 = 1e-12;

/* $Procedure ZZINLAT ( DSK, in latitudinal element? ) */
/* Subroutine */ int zzinlat_(doublereal *p, doublereal *bounds, doublereal *
	margin, integer *exclud, logical *inside)
{
    /* Initialized data */

    static logical first = TRUE_;
    static doublereal pi2 = -1.;
    static doublereal hpi = -1.;

    /* System generated locals */
    doublereal d__1, d__2;

    /* Builtin functions */
    double cos(doublereal);

    /* Local variables */
    extern /* Subroutine */ int zzinlat0_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, integer *, logical *);
    doublereal dlon, minr, smin, maxr, smax;
    extern /* Subroutine */ int zznrmlon_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *);
    doublereal r__;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    doublereal aminr, amaxr;
    extern /* Subroutine */ int errdp_(char *, doublereal *, ftnlen);
    extern doublereal twopi_(void), halfpi_(void);
    extern /* Subroutine */ int reclat_(doublereal *, doublereal *, 
	    doublereal *, doublereal *);
    doublereal amaxlo, amaxlt, aminlt, aminlo, maxlat, lonmrg, maxlon, minlat,
	     minlon;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), sigerr_(char *, 
	    ftnlen), chkout_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen);
    extern logical return_(void);
    doublereal lat, lon;

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     Test a point represented by a set of latitudinal coordinates for */
/*     inclusion in a specified latitudinal volume element. The test is */
/*     performed using margins for the element. */

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
/*     MARGIN     I   Margin used for inclusion testing. */
/*     EXCLUD     I   Index of coordinate to exclude from test. */
/*     INSIDE     O   Flag indicating whether point is in element. */
/*     LATMRG     P   Margin for latitude near the poles. */
/*     LONALI     P   Longitude shift margin. */
/*     ANGMRG     P   Angular rounding margin. */
/*     NONE       P   Meaning: exclude nothing. */
/*     LONIDX     P   Longitude index. Also used for exclusion. */
/*     LATIDX     P   Latitude index. Also used for exclusion. */
/*     RADIDX     P   Radius index. Also used for exclusion. */

/* $ Detailed_Input */

/*     P          is a point expressed in Cartesian coordinates. The */
/*                point is to be checked to determine whether it is */
/*                inside the volume element specified by BOUNDS. */

/*     BOUNDS     is an 2x3 array containing the bounds of a volume */
/*                element expressed in latitudinal coordinates. BOUNDS */
/*                defines the volume element used in the comparison. In */
/*                the element */

/*                   BOUNDS(I,J) */

/*                J is the coordinate index. J is one of */

/*                   { LONIDX, LATIDX, RADIDX } */

/*                I is the bound index. */

/*                   I = 1   ->   lower bound */
/*                   I = 2   ->   upper bound */

/*                If necessary, the longitude bounds are normalized */
/*                for the comparison. The normalized bounds differ */
/*                by no more than 2*pi and are in increasing order. */
/*                See ZZNRMLON for details. */

/*                The aliasing margin LONALI, the greedy margin, and the */
/*                angular rounding margin ANGMRG are used for longitude */
/*                comparisons. ANGMRG is used if its magnitude exceeds */
/*                that of the greedy margin. See the Parameters section */
/*                below for further information. */


/*     MARGIN     is a fraction used to expand the volume element for */
/*                inclusion testing. */

/*                   - The outer radius is scaled by (1+MARGIN). */
/*                     The inner radius is scaled by (1-MARGIN). */

/*                   - Latitude bounds are extended by MARGIN radians, */
/*                     within the interval -pi/2 : pi/2. */

/*                   - For any input point having latitude "not close" */
/*                     to pi/2 or -pi/2, the element's longitude */
/*                     interval is extended by */

/*                        MARGIN / cos(LAT) */

/*                     where LAT is the latitude of the input point. */

/*                     Here "close to" means "within LATMRG radians of." */

/*                   - For any input point having latitude close to */
/*                     pi/2 or -pi/2, comparison against the longitude */
/*                     bounds of the volume element is omitted if */
/*                     MARGIN > 0. */



/*     EXCLUD     is either a coordinate index or the parameter NONE. */

/*                If EXCLUD is set to one of */

/*                   { LONIDX, LATIDX, RADIDX } */

/*                then the indicated coordinate is excluded from */
/*                comparison with the corresponding volume element */
/*                boundaries. */

/*                If EXCLUD is set to NONE, all coordinates are */
/*                compared. */

/*                Exclusion of coordinates is used in cases where a */
/*                point is known to be on a level surface of a given */
/*                coordinate. For example, if a point is on the sphere */
/*                of radius equal to the upper radius bound, radius need */
/*                not be used in the comparison and in fact can't be */
/*                meaningfully compared, due to round-off errors. */

/* $ Detailed_Output */

/*     INSIDE     is a logical flag that is set to .TRUE. if and */
/*                only if the input coordinates represent a */
/*                point inside or on the surface of the volume */
/*                element, according to the comparisons that are */
/*                performed. */

/*                The value of INSIDE is not affected by the value */
/*                of any excluded coordinate. */

/* $ Parameters */


/*     LATMRG     Margin for determining whether the input latitude is */
/*                "close" to the poles. Units are radians. This value */
/*                should be kept in sync with the default "greedy" */
/*                margin. */

/*     LONALI     is a longitude shift margin. If the input longitude */
/*                coordinate LON is less than the longitude lower bound */
/*                by more than this amount, the value LON + 2*pi is */
/*                used in the longitude comparison, provided that the */
/*                comparison is not excluded. */

/*                See dsktol.inc for details. */

/*     ANGMRG     is an angular rounding margin. If the input longitude */
/*                is outside of the longitude bounds by less than this */
/*                amount, the longitude is considered to be within the */
/*                bounds. ANGMRG is used when the magnitude of the */
/*                "greedy" margin is less than ANGMRG. ANGMRG is not */
/*                used for computations involving coordinates other than */
/*                longitude. */

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


/*     RADIDX     is the index of radius in the second dimension of */
/*                BOUNDS. When used as a value of the input argument */
/*                EXCLUD, indicates that radius should be excluded */
/*                from comparison. */

/* $ Exceptions */

/*     1)  If MARGIN is negative, the error SPICE(VALUEOUTOFRANGE) */
/*         is signaled. */

/*     2)  If EXCLUD is outside of the range 0:3, the error */
/*         SPICE(VALUEOUTOFRANGE) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     See usage in ZZRYTLAT. */

/* $ Restrictions */

/*     This is a private routine. It is meant to be used only by the DSK */
/*     subsystem. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 03-JUN-2016 (NJB) */

/*        Original version 03-OCT-2014 (NJB) */

/* -& */
/* $ Index_Entries */

/*     test point against latitudinal element using margin */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Element boundary indices: */


/*     Local variables */


/*     Use discovery check-in. */

    if (return_()) {
	return 0;
    }
    if (first) {
	pi2 = twopi_();
	hpi = halfpi_();
	first = FALSE_;
    }

/*     Get the latitudinal coordinates of the input point. */

    reclat_(p, &r__, &lon, &lat);

/*     Handle the simpler zero-margin case separately. */

    if (*margin == 0.) {
	zzinlat0_(&r__, &lon, &lat, bounds, exclud, inside);
	return 0;
    } else if (*margin < 0.) {
	chkin_("ZZINLAT", (ftnlen)7);
	setmsg_("Margin must be non-negative but was #.", (ftnlen)38);
	errdp_("#", margin, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("ZZINLAT", (ftnlen)7);
	return 0;
    }
    if (*exclud < 0 || *exclud > 3) {
	chkin_("ZZINLAT", (ftnlen)7);
	setmsg_("EXCLUD must be in the range 0:3 but was #.", (ftnlen)42);
	errint_("#", exclud, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("ZZINLAT", (ftnlen)7);
	return 0;
    }

/*     Special case: if the input point is within distance MARGIN */
/*     from the origin, and the minimum radius of the volume element */
/*     is less than or equal to MARGIN, the point is inside. */

    if (r__ <= *margin) {
	if (bounds[4] <= *margin) {
	    *inside = TRUE_;
	    return 0;
	}
    }

/*     Assume the point is outside to start. This allows us */
/*     to skip setting INSIDE when we find a boundary test */
/*     failure. */

    *inside = FALSE_;

/*     Get local copies of the coordinate bounds. Don't normalize the */
/*     longitude bounds until we know we need them. */

    minr = bounds[4];
    maxr = bounds[5];
    minlat = bounds[2];
    maxlat = bounds[3];

/*     Compare coordinates to adjusted latitude and radius */
/*     boundaries. */

    if (*exclud != 3) {

/*        Create adjusted radius bounds. */

	smax = *margin + 1.;
	smin = 1. - *margin;
/* Computing MAX */
	d__1 = 0., d__2 = minr * smin;
	aminr = max(d__1,d__2);
	amaxr = maxr * smax;
	if (r__ < aminr || r__ > amaxr) {
	    return 0;
	}
    }
    if (*exclud != 2) {

/*        Create adjusted latitude bounds. */

/* Computing MAX */
	d__1 = -hpi, d__2 = minlat - *margin;
	aminlt = max(d__1,d__2);
/* Computing MIN */
	d__1 = hpi, d__2 = maxlat + *margin;
	amaxlt = min(d__1,d__2);
	if (lat < aminlt || lat > amaxlt) {
	    return 0;
	}
    }

/*     At this point, the input radius and latitude are within the */
/*     adjusted bounds, if their tests haven't been excluded by */
/*     the caller. */

/*     Perform longitude tests, unless they're excluded by the */
/*     caller. */

    if (*exclud != 1) {
	zznrmlon_(bounds, &bounds[1], &c_b12, &minlon, &maxlon);

/*        Set the margin to be used for longitude interval */
/*        inclusion tests. */

/* Computing MAX */
	d__1 = 1e-12, d__2 = abs(*margin);
	lonmrg = max(d__1,d__2);

/*        We have a special case for segments that include the poles. If */
/*        the input point is close enough to a pole contained in the */
/*        segment, we consider the point to be included in the segment, */
/*        regardless of the point's longitude. All other points get the */
/*        normal longitude test. */

	if (lat <= hpi - 1e-8 && lat >= -hpi + 1e-8) {

/*           This is the usual case: the latitude of the input point */
/*           is bounded away from the poles. */

/*           Check the point's longitude against the segment's */
/*           longitude bounds. */

/*           We'll scale the longitude margin to compensate for the */
/*           latitude of the input point. Note that the division */
/*           below is safe; presuming a reasonable value of MARGIN; */
/*           we know that */

/*              DLON << 1 */

/* Computing MAX */
	    d__2 = (d__1 = cos(lat), abs(d__1));
	    dlon = lonmrg / max(d__2,1e-8);
	    aminlo = minlon - dlon;
	    amaxlo = maxlon + dlon;

/*           Now move the input point's longitude into range, if */
/*           necessary. */

	    if (lon < aminlo) {
		if (lon < aminlo - 1e-12) {

/*                 See whether an aliased version of LON is a match. */

		    lon += pi2;
		} else {

/*                 Consider LON to be a match with the lower bound. */

		    lon = aminlo;
		}
	    } else if (lon > amaxlo) {
		if (lon > amaxlo + 1e-12) {

/*                 See whether an aliased version of LON is a match. */

		    lon -= pi2;
		} else {

/*                 Consider LON to be a match with the upper bound. */

		    lon = amaxlo;
		}
	    }

/*           Compare the adjusted longitude of the input point to the */
/*           adjusted longitude bounds. */

	    if (lon < aminlo || lon > amaxlo) {
		return 0;
	    }
	} else {

/*           The latitude of the input point is close to one of the */
/*           poles. */

/*           This is a no-op case. */

/*           The input point has already passed whichever of the radius */
/*           and latitude tests that were not excluded. */

/*           If the element has a non-degenerate latitude boundary */
/*           having the same sign as the latitude of the input point, */
/*           and if latitude is excluded because the input point is */
/*           already nominally on that boundary, then passing the radius */
/*           check implies that the point is close to the element. */

/*           If the element has a degenerate latitude boundary having */
/*           the same sign as the latitude of the input point---namely, */
/*           the element contains the pole, and latitude is excluded, */
/*           then then passing the radius check implies that the point */
/*           is close to the portion of the Z-axis contained in the */
/*           element. */

/*           If the radius check has been excluded because the point is */
/*           already nominally on one of the element's radius boundaries, */
/*           the passing the latitude test implies the point is close */
/*           to the element. */

/*           In all cases, as long as EXCLUD has been set appropriately, */
/*           the point is close to the element. We consider the point to */
/*           be in the expanded element. */

	}
    }

/*     All tests that were commanded have been passed. The input */
/*     point is considered to be contained in the expanded volume */
/*     element. */

    *inside = TRUE_;
    return 0;
} /* zzinlat_ */

