/* zzrytpdt.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__0 = 0;
static doublereal c_b13 = 1e-12;
static integer c__3 = 3;
static doublereal c_b21 = 1.;
static integer c__2 = 2;
static doublereal c_b37 = 0.;
static integer c__1 = 1;

/* $Procedure ZZRYTPDT ( DSK, ray touches planetodetic element ) */
/* Subroutine */ int zzrytpdt_(doublereal *vertex, doublereal *raydir, 
	doublereal *bounds, doublereal *corpar, doublereal *margin, integer *
	nxpts, doublereal *xpt)
{
    /* Initialized data */

    static doublereal z__[3] = { 0.,0.,1. };

    /* System generated locals */
    doublereal d__1, d__2, d__3;

    /* Builtin functions */
    double cos(doublereal), sin(doublereal);

    /* Local variables */
    doublereal emin, emax, apex[3];
    extern /* Subroutine */ int vhat_(doublereal *, doublereal *);
    doublereal dist, pmin, pmax, udir[3], maxr;
    extern doublereal vdot_(doublereal *, doublereal *), vsep_(doublereal *, 
	    doublereal *);
    extern /* Subroutine */ int vsub_(doublereal *, doublereal *, doublereal *
	    ), vequ_(doublereal *, doublereal *);
    doublereal srfx[3];
    extern /* Subroutine */ int zzellbds_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *);
    extern logical zzpdpltc_(doublereal *, doublereal *, doublereal *, 
	    doublereal *);
    logical xval1, xval2;
    extern /* Subroutine */ int zzelnaxx_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *);
    doublereal eback[3], f;
    extern /* Subroutine */ int zznrmlon_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *), zzinrypl_(doublereal *,
	     doublereal *, doublereal *, doublereal *, doublereal *, integer *
	    , doublereal *);
    doublereal s, angle, wback[3];
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    doublereal eastb[3];
    extern /* Subroutine */ int vpack_(doublereal *, doublereal *, doublereal 
	    *, doublereal *);
    extern doublereal dpmax_(void);
    logical found;
    extern /* Subroutine */ int errdp_(char *, doublereal *, ftnlen), vlcom_(
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *);
    doublereal westb[3];
    extern doublereal vdist_(doublereal *, doublereal *);
    extern /* Subroutine */ int ucrss_(doublereal *, doublereal *, doublereal 
	    *);
    extern doublereal vnorm_(doublereal *);
    extern logical vzero_(doublereal *);
    doublereal endpt2[3];
    extern logical failed_(void);
    doublereal re, rp;
    extern doublereal halfpi_(void);
    integer nx;
    doublereal negdir[3], amnalt;
    logical inside;
    extern /* Subroutine */ int incnsg_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, integer *, doublereal *,
	     doublereal *);
    doublereal minalt, amxalt, maxalt, maxlat, loncov, maxlon, minlat, minlon,
	     mndist, xincpt, vtxang, yincpt, vtxoff[3];
    extern /* Subroutine */ int setmsg_(char *, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), vminus_(doublereal *, doublereal *), surfpt_(doublereal *
	    , doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, logical *);
    doublereal vtxlvl;
    logical xin;
    extern /* Subroutine */ int zzinpdt_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, integer *, logical *);
    doublereal xpt2[3];

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     Find nearest intersection to a given ray's vertex of the ray and */
/*     a planetodetic volume element. If the vertex is inside the */
/*     element, the vertex is considered to be the solution. */

/*     In the computation performed by this routine, ellipsoidal */
/*     surfaces are used, instead of surfaces of constant altitude, to */
/*     define boundaries of planetodetic volume elements. The element */
/*     defined by the input boundaries is contained in the element */
/*     bounded by the input latitude and longitude boundaries and by the */
/*     ellipsoidal surfaces. */

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
/*     INTERCEPT */
/*     INTERSECTION */
/*     RAY */
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
/*     VERTEX     I   Ray's vertex. */
/*     RAYDIR     I   Ray's direction vector. */
/*     BOUNDS     I   Bounds of planetodetic volume element. */
/*     CORPAR     I   Coordinate parameters. */
/*     MARGIN     I   Margin used for element expansion. */
/*     NXPTS      O   Number of intercept points. */
/*     XPT        O   Intercept. */
/*     LONIDX     P   Longitude index. */
/*     LATIDX     P   Latitude index. */
/*     ALTIDX     P   Altitude index. */

/* $ Detailed_Input */

/*     VERTEX, */
/*     RAYDIR     are, respectively, the vertex and direction vector of */
/*                the ray to be used in the intercept computation. */

/*                Both the vertex and ray direction must be represented */
/*                in the reference frame to which the planetodetic */
/*                volume element boundaries correspond. The vertex is */
/*                considered to be an offset from the center of the */
/*                reference frame associated with the element. */

/*     BOUNDS     is a 2x3 array containing the bounds of a planetodetic */
/*                volume element. Normally this is the coverage boundary */
/*                of a DSK segment. In the element */

/*                   BOUNDS(I,J) */

/*                J is the coordinate index. J is one of */

/*                   { LONIDX, LATIDX, ALTIDX } */

/*                I is the bound index. */

/*                   I = 1   ->   lower bound */
/*                   I = 2   ->   upper bound */

/*                If the longitude upper bound is not greater than the */
/*                longitude lower bound, a value greater than the upper */
/*                bound by 2*pi is used for the comparison. */

/*     RE, */
/*     F          are, respectively, the equatorial radius and */
/*                flattening coefficient associated with the */
/*                planetodetic coordinate system in which the input */
/*                volume element is described. */


/*     MARGIN     is a scale factor used to effectively expand the */
/*                segment boundaries so as to include intersections */
/*                that lie slightly outside the volume element. */


/* $ Detailed_Output */

/*     XPT        is the intercept of the ray on the surface described */
/*                by the segment, if such an intercept exists. If the */
/*                ray intersects the surface at multiple points, the */
/*                one closest to the ray's vertex is selected. XPT is */
/*                valid if and only if FOUND is .TRUE. */

/*                XPT is expressed in the reference frame associated */
/*                with the inputs VERTEX and RAYDIR. XPT represents */
/*                an offset from the origin of the coordinate system. */

/*                XPT is valid only if NXPTS is set to 1. */


/*     NXPTS      is the number of intercept points of the ray and */
/*                the volume element. */

/*                Currently there are only two possible values for */
/*                NXPTS: */

/*                   1 for an intersection */
/*                   0 for no intersection */

/*                If the vertex is inside the element, NXPTS is */
/*                set to 1. */

/* $ Parameters */

/*     LONIDX     is the index of longitude in the second dimension of */
/*                BOUNDS. */

/*     LATIDX     is the index of latitude in the second dimension of */
/*                BOUNDS. */

/*     ALTIDX     is the index of altitude in the second dimension of */
/*                BOUNDS. */
/* $ Exceptions */

/*     1)  If MARGIN is negative, the error SPICE(VALUEOUTOFRANGE) */
/*         is signaled. */

/*     2)  If the input ray direction vector is zero, the error */
/*         SPICE(ZEROVECTOR) will be signaled. */

/*     3)  Any errors that occur while calculating the ray-surface */
/*         intercept will be signaled by routines in the call tree */
/*         of this routine. */

/* $ Files */

/*     None. However, the input segment boundaries normally have */
/*     been obtained from a loaded DSK file. */

/* $ Particulars */

/*     This routine sits on top of data DSK type-specific ray-segment */
/*     intercept routines such as DSKX02. */

/* $ Examples */

/*     See usage in ZZDSKBUX. */

/* $ Restrictions */

/*     This is a private routine. It is meant to be used only by the DSK */
/*     subsystem. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 19-JAN-2017 (NJB) */

/* -& */
/* $ Index_Entries */

/*     find intercept of ray on planetodetic volume element */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Altitude expansion factor: */


/*     Element boundary indices: */


/*     Local variables */


/*     Saved variables */


/*     Initial values */

    if (return_()) {
	return 0;
    }
    chkin_("ZZRYTPDT", (ftnlen)8);
    if (*margin < 0.) {
	setmsg_("Margin must be non-negative but was #.", (ftnlen)38);
	errdp_("#", margin, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("ZZRYTPDT", (ftnlen)8);
	return 0;
    }
    if (vzero_(raydir)) {
	setmsg_("The ray's direction was the zero vector.", (ftnlen)40);
	sigerr_("SPICE(ZEROVECTOR)", (ftnlen)17);
	chkout_("ZZRYTPDT", (ftnlen)8);
	return 0;
    }

/*     Determine whether the vertex is inside the element. */

    zzinpdt_(vertex, bounds, corpar, margin, &c__0, &inside);
    if (failed_()) {
	chkout_("ZZRYTPDT", (ftnlen)8);
	return 0;
    }
    if (inside) {

/*        We know the answer. */

	*nxpts = 1;
	vequ_(vertex, xpt);
	chkout_("ZZRYTPDT", (ftnlen)8);
	return 0;
    }

/*     Get semi-axis lengths of the reference spheroid. */

    re = corpar[0];
    f = corpar[1];
    rp = re * (1. - f);

/*     Extract the segment's coordinate bounds into easily */
/*     readable variables. */

    minalt = bounds[4];
    maxalt = bounds[5];

/*     Normalize the longitude bounds. After this step, the bounds will */
/*     be in order and differ by no more than 2*pi. */

    zznrmlon_(bounds, &bounds[1], &c_b13, &minlon, &maxlon);
    if (failed_()) {
	chkout_("ZZRYTPDT", (ftnlen)8);
	return 0;
    }
    minlat = bounds[2];
    maxlat = bounds[3];

/*     Compute adjusted altitude bounds, taking margin into */
/*     account. */

    amnalt = minalt - *margin * abs(minalt);
    amxalt = maxalt + *margin * abs(maxalt);

/*     Generate semi-axis lengths of inner and outer bounding */
/*     ellipsoids. */

    if (re >= rp) {

/*        The reference spheroid is oblate. */

	zzellbds_(&re, &rp, &amxalt, &amnalt, &emax, &pmax, &emin, &pmin);
    } else {

/*        The reference spheroid is prolate. */

	zzellbds_(&rp, &re, &amxalt, &amnalt, &pmax, &emax, &pmin, &emin);
    }
    if (failed_()) {
	chkout_("ZZRYTPDT", (ftnlen)8);
	return 0;
    }

/*     The vertex is outside the element. */

/*     Indicate no intersection to start. */

    *nxpts = 0;

/*     We'll use a unit length copy of the ray's direction vector. */

    vhat_(raydir, udir);

/*     Initialize the distance to the closest solution. We'll keep track */
/*     of this quantity in order to compare competing solutions. */

    mndist = dpmax_();

/*     Find the intersection of the ray and outer bounding ellipsoid, if */
/*     possible. Often this intersection is the closest to the vertex. */
/*     If the intersection exists and is on the boundary of the element, */
/*     it's a winner. */

    surfpt_(vertex, udir, &emax, &emax, &pmax, srfx, &found);
    if (failed_()) {
	chkout_("ZZRYTPDT", (ftnlen)8);
	return 0;
    }
    if (! found) {

/*        There are no intersections. The ray cannot hit the volume */
/*        element. */

	chkout_("ZZRYTPDT", (ftnlen)8);
	return 0;
    }

/*     The ray hits the outer bounding ellipsoid. See whether */
/*     the longitude and latitude are within bounds, taking */
/*     the margin into account. Exclude the altitude coordinate */
/*     from testing. */

    zzinpdt_(srfx, bounds, corpar, margin, &c__3, &xin);
    if (failed_()) {
	chkout_("ZZRYTPDT", (ftnlen)8);
	return 0;
    }
    if (xin) {

/*        This solution is a candidate. */

	vequ_(srfx, xpt);
	*nxpts = 1;

/*        Find the level surface parameter of the vertex relative */
/*        to the adjusted outer bounding ellipsoid. */

/* Computing 2nd power */
	d__1 = vertex[0] / emax;
/* Computing 2nd power */
	d__2 = vertex[1] / emax;
/* Computing 2nd power */
	d__3 = vertex[2] / pmax;
	vtxlvl = d__1 * d__1 + d__2 * d__2 + d__3 * d__3;
	if (vtxlvl > 1.) {

/*           The vertex is outside this ellipsoid, and the DSK segment */
/*           lies within the ellipsoid. */

/*           No other intersection can be closer to the vertex; */
/*           we don't need to check the other surfaces. */

	    chkout_("ZZRYTPDT", (ftnlen)8);
	    return 0;
	} else {

/*           We have a possible solution. */

	    mndist = vdist_(vertex, xpt);
	}
    }

/*     So far there may be a candidate solution. We'll try the latitude */
/*     boundaries next. */

/*     For testing intersections with the latitude boundaries, we'll */
/*     need a far endpoint for the line segment on which to perform the */
/*     test. */

    maxr = max(emax,pmax);
    s = vnorm_(vertex) + maxr * 1.1;
    vlcom_(&c_b21, vertex, &s, udir, endpt2);

/*     Now try the upper latitude bound. We can skip this test */
/*     if the upper bound is pi/2 radians. */

    if (maxlat < halfpi_()) {

/*        Let ANGLE be the angular separation of the surface of latitude */
/*        MAXLAT and the +Z axis. Note that the surface might be the */
/*        lower nappe of the cone. */

/* Computing MAX */
	d__1 = 0., d__2 = halfpi_() - maxlat;
	angle = max(d__1,d__2);

/*        Compute the Z coordinate of the apex of the latitude cone. */

	zzelnaxx_(&re, &rp, &maxlat, &xincpt, &yincpt);
	if (failed_()) {
	    chkout_("ZZRYTPDT", (ftnlen)8);
	    return 0;
	}
	apex[0] = 0.;
	apex[1] = 0.;
	apex[2] = yincpt;

/*        Find the offset of the ray's vertex from the cone's apex, */
/*        and find the angular separation of the offset from the +Z */
/*        axis. This separation enables us to compare the latitude of */
/*        the vertex to the latitude boundary without making a RECGEO */
/*        call to compute the planetodetic coordinates of the vertex. */

/*        (The comparison will be done later.) */

	vsub_(vertex, apex, vtxoff);
	vtxang = vsep_(vtxoff, z__);

/*        Check for intersection of the ray with the latitude cone. */

	incnsg_(apex, z__, &angle, vertex, endpt2, &nx, srfx, xpt2);
	if (failed_()) {
	    chkout_("ZZRYTPDT", (ftnlen)8);
	    return 0;
	}

/*        Unlike the case of latitudinal coordinates, for planetodetic */
/*        coordinates, the surface of the latitude cone does not */
/*        coincide with the set of points having that latitude (which is */
/*        equal to pi/2 - the cone's angular separation from the +Z */
/*        axis). The subset of the cone having the specified latitude is */
/*        truncated by the X-Y plane. If we ignore round-off errors, we */
/*        can assert that the Z-coordinate of a point having the given */
/*        planetodetic latitude must match the direction of the nappe of */
/*        the cone: positive if ANGLE < pi/2, negative if ANGLE > pi/2, */
/*        and 0 if ANGLE = pi/2. */

/*        However, we cannot ignore round-off errors. For a cone having */
/*        angle from its central axis of nearly pi/2, it's possible for */
/*        a valid ray-cone intercept to be on the "wrong" side of the */
/*        X-Y plane due to round-off errors. So we use a more robust */
/*        check to determine whether an intercept should be considered */
/*        to have the same latitude as the cone. */

/*        Check all intercepts. */

	if (nx > 0) {

/*           Check the first intercept. */

	    xval1 = zzpdpltc_(&re, &f, srfx, &maxlat);
	    xval2 = FALSE_;
	    if (nx == 2) {

/*              Check the second intercept. */

		xval2 = zzpdpltc_(&re, &f, xpt2, &maxlat);
	    }
	    if (xval1 && ! xval2) {
		nx = 1;
	    } else if (xval2 && ! xval1) {

/*              Only the second solution is valid. Overwrite */
/*              the first. */

		nx = 1;
		vequ_(xpt2, srfx);
	    } else if (! xval1 && ! xval2) {

/*              Neither solution is valid. */

		nx = 0;
	    }
	}
	if (nx >= 1) {

/*           The ray intercept SRFX lies on the upper latitude boundary. */

/*           See whether SRFX meets the longitude and proxy altitude */
/*           constraints. */

	    zzinpdt_(srfx, bounds, corpar, margin, &c__2, &xin);
	    if (failed_()) {
		chkout_("ZZRYTPDT", (ftnlen)8);
		return 0;
	    }
	    if (xin) {

/*              SRFX is a candidate solution. */

		dist = vdist_(vertex, srfx);
		if (dist < mndist) {
		    vequ_(srfx, xpt);
		    *nxpts = 1;
		    if (vtxang < angle) {
			if (maxlat < 0. || vertex[2] > 0.) {

/*                       If MAXLAT is negative, the vertex offset */
/*                       being outside the cone is enough to */
/*                       guarantee the planetodetic latitude of the */
/*                       vertex is greater than that of the cone. */

/*                       If MAXLAT is non-negative, the angle of the */
/*                       vertex offset relative to the +Z axis is not */
/*                       enough; we need the vertex to lie above the */
/*                       X-Y plane as well. */

/*                       Getting here means one of these conditions */
/*                       was met. */

/*                       Since the latitude of the vertex is greater */
/*                       than MAXLAT, this is the best solution, since */
/*                       the volume element is on the other side of the */
/*                       maximum latitude boundary. */

			    chkout_("ZZRYTPDT", (ftnlen)8);
			    return 0;
			}
		    }

/*                 This is the best solution seen so far, but we */
/*                 need to check the remaining boundaries. */

		    mndist = dist;
		}
	    }
	    if (nx == 2) {

/*              Check the second solution as well. */

		zzinpdt_(xpt2, bounds, corpar, margin, &c__2, &xin);
		if (failed_()) {
		    chkout_("ZZRYTPDT", (ftnlen)8);
		    return 0;
		}
		if (xin) {

/*                 XPT2 is a candidate solution. */

		    dist = vdist_(vertex, xpt2);
		    if (dist < mndist) {
			vequ_(xpt2, xpt);
			*nxpts = 1;
			mndist = dist;

/*                    This is the best solution seen so far. */
/*                    However, it's not necessarily the best */
/*                    solution. So we continue. */

		    }
		}
	    }

/*           We've handled the second root, if any. */

	}

/*        We're done with the upper latitude boundary. */

    }

/*     Try the lower latitude bound. We can skip this test if the lower */
/*     bound is -pi/2 radians. */

    if (minlat > -halfpi_()) {

/*        Let ANGLE be the angular separation of the surface */
/*        of latitude MINLAT and the +Z axis. Note that the */
/*        surface might be the lower nappe of the cone. */

	angle = halfpi_() - minlat;
/*        Compute the Z coordinate of the apex of the latitude cone. */

	zzelnaxx_(&re, &rp, &minlat, &xincpt, &yincpt);
	if (failed_()) {
	    chkout_("ZZRYTPDT", (ftnlen)8);
	    return 0;
	}
	apex[0] = 0.;
	apex[1] = 0.;
	apex[2] = yincpt;
	incnsg_(apex, z__, &angle, vertex, endpt2, &nx, srfx, xpt2);
	if (failed_()) {
	    chkout_("ZZRYTPDT", (ftnlen)8);
	    return 0;
	}

/*        Find the offset of the ray's vertex from the cone's apex, */
/*        and find the angular separation of the offset from the +Z */
/*        axis. This separation enables us to compare the latitude of */
/*        the vertex to the latitude boundary without making a RECGEO */
/*        call to compute the planetodetic coordinates of the vertex. */

/*        (The comparison will be done later.) */

	vsub_(vertex, apex, vtxoff);
	vtxang = vsep_(vtxoff, z__);

/*        Check whether the latitude of the intercept can be */
/*        considered to match that of the cone. */

	if (nx > 0) {

/*           Check the first intercept. */

	    xval1 = zzpdpltc_(&re, &f, srfx, &minlat);
	    xval2 = FALSE_;
	    if (nx == 2) {

/*              Check the second intercept. */

		xval2 = zzpdpltc_(&re, &f, xpt2, &minlat);
	    }
	    if (xval1 && ! xval2) {
		nx = 1;
	    } else if (xval2 && ! xval1) {

/*              Only the second solution is valid. Overwrite */
/*              the first. */

		nx = 1;
		vequ_(xpt2, srfx);
	    } else if (! xval1 && ! xval2) {

/*              Neither solution is valid. */

		nx = 0;
	    }
	}
	if (nx >= 1) {

/*           The ray intercept SRFX lies on the lower latitude boundary. */

/*           See whether SRFX meets the longitude and proxy altitude */
/*           constraints. */

	    zzinpdt_(srfx, bounds, corpar, margin, &c__2, &xin);
	    if (failed_()) {
		chkout_("ZZRYTPDT", (ftnlen)8);
		return 0;
	    }
	    if (xin) {

/*              SRFX is a candidate solution. */

		dist = vdist_(vertex, srfx);
		if (dist < mndist) {
		    vequ_(srfx, xpt);
		    *nxpts = 1;
		    if (vtxang > angle) {
			if (minlat > 0. || vertex[2] < 0.) {

/*                       If MINLAT is positive, the vertex offset */
/*                       being outside the cone is enough to */
/*                       guarantee the planetodetic latitude of the */
/*                       vertex is less than that of the cone. */

/*                       If MINLAT is non-positive, the angle of the */
/*                       vertex offset relative to the +Z axis is not */
/*                       enough; we need the vertex to lie below the */
/*                       X-Y plane as well. */

/*                       Getting here means one of these conditions */
/*                       was met. */

/*                       Since the latitude of the vertex is less than */
/*                       than MINLAT, this is the best solution, since */
/*                       the volume element is on the other side of the */
/*                       minimum latitude boundary. */

			    chkout_("ZZRYTPDT", (ftnlen)8);
			    return 0;
			}
		    }

/*                 This is the best solution seen so far, but we */
/*                 need to check the remaining boundaries. */
		    mndist = dist;
		}
	    }
	    if (nx == 2) {

/*              Check the second solution as well. */

		zzinpdt_(xpt2, bounds, corpar, margin, &c__2, &xin);
		if (failed_()) {
		    chkout_("ZZRYTPDT", (ftnlen)8);
		    return 0;
		}
		if (xin) {

/*                 XPT2 is a candidate solution. */

		    dist = vdist_(vertex, xpt2);
		    if (dist < mndist) {
			vequ_(xpt2, xpt);
			*nxpts = 1;
			mndist = dist;

/*                    This is the best solution seen so far. */
/*                    However, it's not necessarily the best */
/*                    solution. So we continue. */

			chkout_("ZZRYTPDT", (ftnlen)8);
			return 0;
		    }
		}
	    }
	}

/*        We're done with the lower latitude boundary. */

    }

/*     Perform longitude boundary checks if the coverage is not */
/*     2*pi radians. Note that MAXLON > MINLON at this point. */

    loncov = maxlon - minlon;
    if (cos(loncov) < 1.) {

/*        We have distinct longitude boundaries. Go to work. */


/*        Check the longitude boundaries. Try the plane of western */
/*        longitude first. */

	d__1 = sin(minlon);
	d__2 = -cos(minlon);
	vpack_(&d__1, &d__2, &c_b37, westb);
	s = (vnorm_(vertex) + maxr) * 1.1;
	zzinrypl_(vertex, udir, westb, &c_b37, &s, &nx, srfx);
	if (nx == 1) {

/*           We have one point of intersection. Determine whether it's a */
/*           candidate solution.  Don't use longitude in the following */
/*           inclusion test. Note that we'll perform a separate check */
/*           later in place of the longitude check. */

	    zzinpdt_(srfx, bounds, corpar, margin, &c__1, &xin);
	    if (failed_()) {
		chkout_("ZZRYTPDT", (ftnlen)8);
		return 0;
	    }
	    if (xin) {

/*              Make sure the intercept is not too far on the */
/*              "wrong" side of the Z axis. */

		ucrss_(westb, z__, wback);
		if (vdot_(srfx, wback) < *margin * maxr) {

/*                 The intercept is either on the same side of the Z */
/*                 axis as the west face of the segment, or is very */
/*                 close to the Z axis. */

		    dist = vdist_(vertex, srfx);
		    if (dist < mndist) {

/*                    Record the intercept, distance, and surface index. */

			vequ_(srfx, xpt);
			*nxpts = 1;
			mndist = dist;
		    }
		}
	    }
	}

/*        We're done with the western boundary. */


/*        Try the plane of eastern longitude next. */

	d__1 = -sin(maxlon);
	d__2 = cos(maxlon);
	vpack_(&d__1, &d__2, &c_b37, eastb);
	zzinrypl_(vertex, udir, eastb, &c_b37, &s, &nx, srfx);
	if (nx == 1) {

/*           We have one point of intersection. Determine whether it's a */
/*           candidate solution. */

	    zzinpdt_(srfx, bounds, corpar, margin, &c__1, &xin);
	    if (failed_()) {
		chkout_("ZZRYTPDT", (ftnlen)8);
		return 0;
	    }
	    if (xin) {

/*              Make sure the intercept is not too far on the "wrong" */
/*              side of the Z axis. */

		ucrss_(z__, eastb, eback);
		if (vdot_(srfx, eback) < *margin * maxr) {

/*                 The intercept is either on the same side of the Z */
/*                 axis as the east face of the segment, or is very */
/*                 close to the Z axis. */

		    dist = vdist_(vertex, srfx);
		    if (dist < mndist) {

/*                    Record the intercept, distance, and surface index. */

			vequ_(srfx, xpt);
			*nxpts = 1;
			mndist = dist;
		    }
		}
	    }
	}
    }

/*     End of longitude boundary checks. */


/*     Find the intersection of the ray and lower bounding */
/*     ellipsoid, if possible. */

    surfpt_(vertex, udir, &emin, &emin, &pmin, srfx, &found);
    if (failed_()) {
	chkout_("ZZRYTPDT", (ftnlen)8);
	return 0;
    }
    if (found) {

/*        See whether this solution is in the element. */

	zzinpdt_(srfx, bounds, corpar, margin, &c__3, &xin);
	if (failed_()) {
	    chkout_("ZZRYTPDT", (ftnlen)8);
	    return 0;
	}
	if (xin) {
	    dist = vdist_(vertex, srfx);
	    if (dist < mndist) {

/*              Record the intercept, distance, and surface index. */

		vequ_(srfx, xpt);
		*nxpts = 1;
		mndist = dist;
	    }
	}
    }

/*     Unlike the outer ellipsoid, either intersection of the ray with */
/*     the inner ellipsoid might be a valid solution. We'll test for the */
/*     case where the intersection farther from the ray's vertex is the */
/*     correct one. */

    vminus_(udir, negdir);
    surfpt_(endpt2, negdir, &emin, &emin, &pmin, srfx, &found);
    if (failed_()) {
	chkout_("ZZRYTPDT", (ftnlen)8);
	return 0;
    }
    if (found) {
	zzinpdt_(srfx, bounds, corpar, margin, &c__3, &xin);
	if (failed_()) {
	    chkout_("ZZRYTPDT", (ftnlen)8);
	    return 0;
	}
	if (xin) {
	    dist = vdist_(vertex, srfx);
	    if (dist < mndist) {

/*              Record the intercept, distance, and surface index. */

		vequ_(srfx, xpt);
		*nxpts = 1;

/*              There's no need to update MNDIST at this point. */

	    }
	}
    }

/*     NXPTS and XPT are set. */

    chkout_("ZZRYTPDT", (ftnlen)8);
    return 0;
} /* zzrytpdt_ */

