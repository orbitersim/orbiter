/* zzrytlat.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__0 = 0;
static doublereal c_b3 = 1e-12;
static integer c__3 = 3;
static doublereal c_b5 = 1.;
static integer c__2 = 2;
static doublereal c_b10 = 0.;
static integer c__1 = 1;

/* $Procedure ZZRYTLAT ( DSK, ray touches latitudinal element ) */
/* Subroutine */ int zzrytlat_(doublereal *vertex, doublereal *raydir, 
	doublereal *bounds, doublereal *margin, integer *nxpts, doublereal *
	xpt)
{
    /* Initialized data */

    static doublereal origin[3] = { 0.,0.,0. };
    static doublereal z__[3] = { 0.,0.,1. };

    /* System generated locals */
    doublereal d__1, d__2;

    /* Builtin functions */
    double cos(doublereal), sin(doublereal);

    /* Local variables */
    extern /* Subroutine */ int vhat_(doublereal *, doublereal *);
    doublereal dist, udir[3], minr, vlat, maxr;
    extern doublereal vdot_(doublereal *, doublereal *);
    doublereal vlon;
    extern /* Subroutine */ int vequ_(doublereal *, doublereal *);
    doublereal srfx[3], eback[3];
    extern /* Subroutine */ int zznrmlon_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *), zzinrypl_(doublereal *,
	     doublereal *, doublereal *, doublereal *, doublereal *, integer *
	    , doublereal *);
    doublereal s, angle, wback[3], eastb[3];
    extern /* Subroutine */ int zzryxsph_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, logical *), vpack_(doublereal *, 
	    doublereal *, doublereal *, doublereal *);
    extern doublereal dpmax_(void);
    logical found;
    extern /* Subroutine */ int vlcom_(doublereal *, doublereal *, doublereal 
	    *, doublereal *, doublereal *);
    doublereal westb[3];
    extern doublereal vdist_(doublereal *, doublereal *);
    extern /* Subroutine */ int ucrss_(doublereal *, doublereal *, doublereal 
	    *);
    extern doublereal vnorm_(doublereal *);
    doublereal endpt2[3];
    extern logical failed_(void);
    extern doublereal halfpi_(void);
    integer nx;
    doublereal negdir[3], vr;
    logical inside;
    extern /* Subroutine */ int reclat_(doublereal *, doublereal *, 
	    doublereal *, doublereal *);
    doublereal minlat, maxlat;
    extern /* Subroutine */ int incnsg_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, integer *, doublereal *,
	     doublereal *);
    doublereal minlon, maxlon, loncov, mndist;
    extern /* Subroutine */ int vminus_(doublereal *, doublereal *);
    logical xin;
    extern /* Subroutine */ int zzinlat_(doublereal *, doublereal *, 
	    doublereal *, integer *, logical *);
    doublereal xpt2[3];

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     Find nearest intersection to a given ray's vertex of that ray and */
/*     at latitudinal volume element. If the vertex is inside the */
/*     element, the vertex is considered to be the solution. */

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
/*     BOUNDS     I   Bounds of latitudinal volume element. */
/*     MARGIN     I   Margin used for element expansion. */
/*     NXPTS      O   Number of intercept points. */
/*     XPT        O   Intercept. */
/*     LONIDX     P   Longitude index. */
/*     LATIDX     P   Latitude index. */
/*     RADIDX     P   Radius index. */

/* $ Detailed_Input */

/*     VERTEX, */
/*     RAYDIR     are, respectively, the vertex and direction vector of */
/*                the ray to be used in the intercept computation. */

/*                Both the vertex and ray direction must be represented */
/*                in the reference frame to which the volume element */
/*                boundaries correspond. The vertex is considered to be */
/*                an offset from the center of the reference frame */
/*                associated with the volume element. */

/*     BOUNDS     is a 2x3 array containing the bounds of a latitudinal */
/*                volume element. Normally this is the coverage boundary */
/*                of a DSK segment. In the element */

/*                   BOUNDS(I,J) */

/*                J is the coordinate index. J is one of */

/*                   { LONIDX, LATIDX, RADIDX } */

/*                I is the bound index. */

/*                   I = 1   ->   lower bound */
/*                   I = 2   ->   upper bound */

/*                If the longitude upper bound is not greater than the */
/*                longitude lower bound, a value greater than the upper */
/*                bound by 2*pi is used for the comparison. */


/*     MARGIN     is a scale factor used to effectively expand the */
/*                segment boundaries so as to include intersections */
/*                that lie slightly outside the volume element. */


/* $ Detailed_Output */

/*     XPT        is the intercept of the ray on the surface described */
/*                by the input coordinate bounds, if such an intercept */
/*                exists. If the ray intersects the surface at multiple */
/*                points, the one closest to the ray's vertex is */
/*                selected. XPT is valid if and only if FOUND is .TRUE. */

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

/*     RADIDX     is the index of radius in the second dimension of */
/*                BOUNDS. */
/* $ Exceptions */

/*     For the sake of run-time efficiency, this routine does not */
/*     participate in call tracing. However, it's possible for */
/*     routines called by this routine to signal errors. */

/* $ Files */

/*     None. However, the input volume element boundaries normally have */
/*     been obtained from a segment in a loaded DSK file. */

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

/* -    SPICELIB Version 1.0.0, 18-JAN-2017 (NJB) */

/*        Based on ZZRYXLAT 20-MAR-2015 (NJB) */

/*        Original 01-OCT-2014 (NJB) */

/* -& */
/* $ Index_Entries */

/*     find intercept of ray on latitudinal volume element */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Radius expansion factor: */


/*     Element boundary indices: */


/*     Local variables */


/*     Saved variables */


/*     Initial values */


/*     CAUTION: this routine doesn't check in, so it won't */
/*     appear in the traceback if a lower-level routine */
/*     signals an error. */


/*     Determine whether the vertex is inside the element. */
/*     Use double the margin for this test, since we don't */
/*     want to have false negative tests for rays having */
/*     vertices lying on the expanded element boundary. */

    d__1 = *margin * 2;
    zzinlat_(vertex, bounds, &d__1, &c__0, &inside);
    if (failed_()) {
	return 0;
    }
    if (inside) {

/*        We know the answer. */

	*nxpts = 1;
	vequ_(vertex, xpt);
	return 0;
    }

/*     Extract the segment's coordinate bounds into easily */
/*     readable variables. */

    minr = bounds[4];
    maxr = bounds[5];

/*     Normalize the longitude bounds. After this step, the bounds will */
/*     be in order and differ by no more than 2*pi. */

    zznrmlon_(bounds, &bounds[1], &c_b3, &minlon, &maxlon);
    if (failed_()) {
	return 0;
    }
    minlat = bounds[2];
    maxlat = bounds[3];

/*     The vertex is outside the element. */

/*     Indicate no intersection to start. */

    *nxpts = 0;

/*     We'll use a unit length copy of the ray's direction vector. */

    vhat_(raydir, udir);

/*     Initialize the distance to the closest solution. We'll keep track */
/*     of this quantity in order to compare competing solutions. */

    mndist = dpmax_();

/*     Find the intersection of the ray and outer bounding sphere, if */
/*     possible. Often this intersection is the closest to the vertex. */
/*     If the intersection exists and is on the boundary of the element, */
/*     it's a winner. */

    zzryxsph_(vertex, udir, &maxr, srfx, &found);
    if (! found) {

/*        There are no intersections. The ray cannot hit the */
/*        volume element. */

	return 0;
    }

/*     Get the latitudinal coordinates of the ray's vertex. */

    reclat_(vertex, &vr, &vlon, &vlat);

/*     The ray hits the outer bounding sphere. See whether */
/*     the longitude and latitude are within bounds, taking */
/*     the margin into account. Exclude the radius coordinate */
/*     from testing. */

    zzinlat_(srfx, bounds, margin, &c__3, &xin);
    if (failed_()) {
	return 0;
    }
    if (xin) {

/*        This solution is a candidate. */

	vequ_(srfx, xpt);
	*nxpts = 1;
	if (vr > maxr) {

/*           The vertex is outside this sphere, and the segment */
/*           lies within the sphere. */

/*           No other intersection can be closer to the vertex; */
/*           we don't need to check the other surfaces. */

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

    s = vnorm_(vertex) + maxr * 1.1;
    vlcom_(&c_b5, vertex, &s, udir, endpt2);

/*     Now try the upper latitude bound. We can skip this test */
/*     if the upper bound is pi/2 radians. */

    if (maxlat < halfpi_()) {

/*        Let ANGLE be the angular separation of the surface of latitude */
/*        MAXLAT and the +Z axis. Note that the surface might be the */
/*        lower nappe of the cone. */

/* Computing MAX */
	d__1 = 0., d__2 = halfpi_() - maxlat;
	angle = max(d__1,d__2);
	incnsg_(origin, z__, &angle, vertex, endpt2, &nx, srfx, xpt2);
	if (failed_()) {
	    return 0;
	}
	if (nx >= 1) {

/*           See whether SRFX is in the element. */

	    zzinlat_(srfx, bounds, margin, &c__2, &xin);
	    if (failed_()) {
		return 0;
	    }
	    if (xin) {

/*              SRFX is a candidate solution. */

		dist = vdist_(vertex, srfx);
		if (dist < mndist) {
		    vequ_(srfx, xpt);
		    *nxpts = 1;
		    if (vlat > maxlat) {

/*                    Since the latitude of the vertex is greater than */
/*                    MAXLAT, this is the best solution, since the */
/*                    volume element is on the other side of the maximum */
/*                    latitude boundary. */

			return 0;
		    }

/*                 This is the best solution seen so far. */

		    mndist = dist;
		}
	    }
	    if (nx == 2) {

/*              Check the second solution as well. */

		zzinlat_(xpt2, bounds, margin, &c__2, &xin);
		if (failed_()) {
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
	incnsg_(origin, z__, &angle, vertex, endpt2, &nx, srfx, xpt2);
	if (failed_()) {
	    return 0;
	}
	if (nx >= 1) {

/*           See whether SRFX is in the element. */

	    zzinlat_(srfx, bounds, margin, &c__2, &xin);
	    if (failed_()) {
		return 0;
	    }
	    if (xin) {

/*              SRFX is a candidate solution. */

		dist = vdist_(vertex, srfx);
		if (dist < mndist) {
		    vequ_(srfx, xpt);
		    *nxpts = 1;
		    if (vlat < minlat) {

/*                    Since the latitude of the vertex is less than */
/*                    MINLAT, this is the best solution, since the */
/*                    volume element is on the other side of the minimum */
/*                    latitude boundary. */

			return 0;
		    }

/*                 This is the best solution seen so far. */

		    mndist = dist;
		}
	    }
	    if (nx == 2) {

/*              Check the second solution as well. */

		zzinlat_(xpt2, bounds, margin, &c__2, &xin);
		if (failed_()) {
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

/*        The vector WESTB is an outward-facing vector normal to */
/*        the west boundary. */

	d__1 = sin(minlon);
	d__2 = -cos(minlon);
	vpack_(&d__1, &d__2, &c_b10, westb);
	s = (vnorm_(vertex) + maxr) * 1.1;
	zzinrypl_(vertex, udir, westb, &c_b10, &s, &nx, srfx);
	if (nx == 1) {

/*           We have one point of intersection. Determine whether it's a */
/*           candidate solution.  Don't use longitude in the following */
/*           inclusion test. Note that we'll perform a separate check */
/*           later in place of the longitude check. */

	    zzinlat_(srfx, bounds, margin, &c__1, &xin);
	    if (failed_()) {
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

/*        The vector EASTB is an outward-facing vector normal to */
/*        the east boundary. */
	d__1 = -sin(maxlon);
	d__2 = cos(maxlon);
	vpack_(&d__1, &d__2, &c_b10, eastb);
	zzinrypl_(vertex, udir, eastb, &c_b10, &s, &nx, srfx);
	if (nx == 1) {

/*           We have one point of intersection. Determine whether it's a */
/*           candidate solution. */

	    zzinlat_(srfx, bounds, margin, &c__1, &xin);
	    if (failed_()) {
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


/*     Find the intersection of the ray and inner bounding */
/*     sphere, if possible. */

    if (minr > 0.) {
	zzryxsph_(vertex, udir, &minr, srfx, &found);
	if (found) {

/*           See whether this solution is in the element. */

	    zzinlat_(srfx, bounds, margin, &c__3, &xin);
	    if (failed_()) {
		return 0;
	    }
	    if (xin) {
		dist = vdist_(vertex, srfx);
		if (dist < mndist) {

/*                 Record the intercept, distance, and surface index. */

		    vequ_(srfx, xpt);
		    *nxpts = 1;
		    mndist = dist;
		}
	    }
	}

/*        Unlike the outer sphere, either intersection of the ray with */
/*        the inner sphere can be an optimal solution. We'll test for */
/*        the case where the intercept further from the ray's vertex is */
/*        the correct solution. */

	vminus_(udir, negdir);
	zzryxsph_(endpt2, negdir, &minr, srfx, &found);
	if (found) {
	    zzinlat_(srfx, bounds, margin, &c__3, &xin);
	    if (failed_()) {
		return 0;
	    }
	    if (xin) {
		dist = vdist_(vertex, srfx);
		if (dist < mndist) {

/*                 Record the intercept, distance, and surface index. */

		    vequ_(srfx, xpt);
		    *nxpts = 1;
		    mndist = dist;
		}
	    }
	}
    }

/*     NXPTS and XPT are set. */

    return 0;
} /* zzrytlat_ */

