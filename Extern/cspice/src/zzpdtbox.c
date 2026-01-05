/* zzpdtbox.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static doublereal c_b34 = 0.;

/* $Procedure ZZPDTBOX (Bounding box for planetodetic volume element) */
/* Subroutine */ int zzpdtbox_(doublereal *bounds, doublereal *corpar, 
	doublereal *center, doublereal *lr, doublereal *lt, doublereal *lz, 
	doublereal *radius)
{
    /* System generated locals */
    doublereal d__1, d__2, d__3;

    /* Builtin functions */
    double cos(doublereal), sin(doublereal);

    /* Local variables */
    doublereal diag[3], midr, minv[3], botv[3], maxv[3], minz, maxz, topv[3], 
	    f;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    doublereal inrad, hdlon;
    extern /* Subroutine */ int vpack_(doublereal *, doublereal *, doublereal 
	    *, doublereal *), errdp_(char *, doublereal *, ftnlen);
    extern doublereal vnorm_(doublereal *), twopi_(void);
    doublereal re;
    extern doublereal halfpi_(void);
    extern /* Subroutine */ int georec_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *);
    doublereal maxabs, midlon, minalt, minlat, maxalt, maxlat, maxlon, minlon,
	     outrad;
    extern logical return_(void);
    extern /* Subroutine */ int setmsg_(char *, ftnlen), sigerr_(char *, 
	    ftnlen), chkout_(char *, ftnlen), cylrec_(doublereal *, 
	    doublereal *, doublereal *, doublereal *);

/* $ Abstract */

/*     Create a bounding box for a planetodetic volume element. */

/*     The outputs are the box's center, dimensions in the radial, */
/*     tangential, and vertical directions, and the box's radius. */

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

/*     GEOMETRY */
/*     MATH */

/* $ Declarations */

/*     Include file dskdsc.inc */

/*     This include file declares parameters for DSK segment descriptors. */

/* -       SPICELIB Version 1.0.0 08-FEB-2017 (NJB) */

/*           Updated version info. */

/*           22-JAN-2016 (NJB) */

/*              Added parameter for data class 2. Changed name of data */
/*              class 1 parameter. Corrected data class descriptions. */

/*           13-MAY-2010 (NJB) */

/*              Descriptor now contains two ID codes, one for the */
/*              surface, one for the associated ephemeris object. This */
/*              supports association of multiple surfaces with one */
/*              ephemeris object without creating file management */
/*              issues. */

/*              Room was added for coordinate system definition */
/*              parameters. */

/*               Flag arrays and model ID/component entries were deleted. */

/*            11-SEP-2008 (NJB) */


/*     DSK segment descriptors are implemented as an array of d.p. */
/*     numbers.  Note that each integer descriptor datum occupies one */
/*     d.p. value. */




/*     Segment descriptor parameters */

/*     Each segment descriptor occupies a contiguous */
/*     range of DAS d.p. addresses. */

/*        The DSK segment descriptor layout is: */

/*           +---------------------+ */
/*           | Surface ID code     | */
/*           +---------------------+ */
/*           | Center ID code      | */
/*           +---------------------+ */
/*           | Data class code     | */
/*           +---------------------+ */
/*           | Data type           | */
/*           +---------------------+ */
/*           | Ref frame code      | */
/*           +---------------------+ */
/*           | Coord sys code      | */
/*           +---------------------+ */
/*           | Coord sys parameters|  {10 elements} */
/*           +---------------------+ */
/*           | Min coord 1         | */
/*           +---------------------+ */
/*           | Max coord 1         | */
/*           +---------------------+ */
/*           | Min coord 2         | */
/*           +---------------------+ */
/*           | Max coord 2         | */
/*           +---------------------+ */
/*           | Min coord 3         | */
/*           +---------------------+ */
/*           | Max coord 3         | */
/*           +---------------------+ */
/*           | Start time          | */
/*           +---------------------+ */
/*           | Stop time           | */
/*           +---------------------+ */

/*     Parameters defining offsets for segment descriptor elements */
/*     follow. */


/*     Surface ID code: */


/*     Central ephemeris object NAIF ID: */


/*     Data class: */

/*     The "data class" is a code indicating the category of */
/*     data contained in the segment. */


/*     Data type: */


/*     Frame ID: */


/*     Coordinate system code: */


/*     Coordinate system parameter start index: */


/*     Number of coordinate system parameters: */


/*     Ranges for coordinate bounds: */


/*     Coverage time bounds: */


/*     Descriptor size (24): */


/*     Data class values: */

/*        Class 1 indicates a surface that can be represented as a */
/*                single-valued function of its domain coordinates. */

/*                An example is a surface defined by a function that */
/*                maps each planetodetic longitude and latitude pair to */
/*                a unique altitude. */


/*        Class 2 indicates a general surface. Surfaces that */
/*                have multiple points for a given pair of domain */
/*                coordinates---for example, multiple radii for a given */
/*                latitude and longitude---belong to class 2. */



/*     Coordinate system values: */

/*        The coordinate system code indicates the system to which the */
/*        tangential coordinate bounds belong. */

/*        Code 1 refers to the planetocentric latitudinal system. */

/*        In this system, the first tangential coordinate is longitude */
/*        and the second tangential coordinate is latitude. The third */
/*        coordinate is radius. */



/*        Code 2 refers to the cylindrical system. */

/*        In this system, the first tangential coordinate is radius and */
/*        the second tangential coordinate is longitude. The third, */
/*        orthogonal coordinate is Z. */



/*        Code 3 refers to the rectangular system. */

/*        In this system, the first tangential coordinate is X and */
/*        the second tangential coordinate is Y. The third, */
/*        orthogonal coordinate is Z. */



/*        Code 4 refers to the planetodetic/geodetic system. */

/*        In this system, the first tangential coordinate is longitude */
/*        and the second tangential coordinate is planetodetic */
/*        latitude. The third, orthogonal coordinate is altitude. */



/*     End of include file dskdsc.inc */


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

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     BOUNDS     I   Array of coordinate bounds for segment. */
/*     CORPAR     I   Array containing coordinate parameters. */
/*     CENTER     O   Center of bounding box. */
/*     LR         O   Extent of box in the cylindrical radial direction. */
/*     LT         O   Extent of box in the tangential direction. */
/*     LZ         O   Extent of box in the Z direction. */
/*     RADIUS     O   Radius of box. */
/*     ANGMRG     P   Angular margin. */

/* $ Detailed_Input */

/*     BOUNDS     is a 2 x 3 double precision array containing bounds */
/*                on the planetodetic coordinates of the spatial region */
/*                (volume element) covered by a DSK segment. The */
/*                contents of BOUNDS are: */

/*                   BOUNDS(*,1)              Longitude bounds */
/*                   BOUNDS(*,2)              Latitude bounds */
/*                   BOUNDS(*,3)              Altitude bounds */

/*                Elements (1,*) are lower bounds; elements (2,*) are */
/*                upper bounds. Angular units are radians. */


/*     CORPAR     is an array of coordinate system parameters. The first */
/*                and second elements of the array are, respectively, */
/*                the equatorial radius and flattening coefficient of */
/*                the coordinate system. Any additional elements are */
/*                ignored by this routine. */


/* $ Detailed_Output */

/*     CENTER     is a double precision 3-vector representing the center */
/*                of a box tangent to and containing the volume */
/*                specified by BOUNDS. The sides of the box are parallel */
/*                to the radial, tangential, and Z directions, where the */
/*                radial direction is a vector orthogonal to the Z axis, */
/*                having longitude equal to the midpoint of the */
/*                segment's longitude range. The tangential direction is */
/*                orthogonal to the radial and Z directions. */

/*     LR, */
/*     LT, */
/*     LZ         are, respectively, the extents (edge lengths) of the */
/*                bounding box in the radial, tangential, and Z */
/*                directions. */

/*     RADIUS     is the radius of the sphere that circumscribes the */
/*                box. RADIUS is equal to the length of a line segment */
/*                connecting the center of the box to any corner. */

/* $ Parameters */

/*     ANGMRG     is used for latitude range validity tests. Latitude */
/*                values can be out of range by as much as ANGMRG. */

/*                See dsktol.inc for details. */

/* $ Exceptions */

/*     1) If the minimum longitude exceeds the maximum by more than */
/*        2*pi, the error SPICE(BADLONGITUDERANGE) is signaled. */

/*     2) If the latitude bounds are out of order, the error */
/*        SPICE(BADLATITUDEBOUNDS) is signaled. */

/*     3) If the minimum latitude is less than -pi/2 - ANGMRG, or */
/*        if the maximum latitude is greater than pi/2 + ANGMRG, */
/*        the error SPICE(BADLATITUDERANGE) is signaled. */

/*     4) If the input equatorial radius in the array CORPAR is */
/*        non-positive, the error SPICE(VALUEOUTOFRANGE) is signaled. */

/*     5) If the input flattening coefficient in the array CORPAR is */
/*        greater than or equal to 1, the error */
/*        SPICE(VALUEOUTOFRANGE) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine provides a simple way to compute the center and */
/*     radius for an outer bounding sphere for the volume covered */
/*     by a DSK segment using planetodetic coordinates. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */
/*     H.A. Neilan    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 02-MAR-2015 (NJB) */

/* -& */
/* $ Index_Entries */

/*     bounding box for planetodetic volume element */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     This routine uses discovery check-in. We check RETURN in order to */
/*     avoid performing math operations using invalid operands. */

    if (return_()) {
	return 0;
    }
    re = corpar[0];
    f = corpar[1];

/*     The equatorial radius must be greater than zero. */

    if (re <= 0.) {
	chkin_("ZZPDTBOX", (ftnlen)8);
	setmsg_("Equatorial radius from CORPAR array was #.", (ftnlen)42);
	errdp_("#", &re, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("ZZPDTBOX", (ftnlen)8);
	return 0;
    }

/*     If the flattening coefficient is greater than one, the polar */
/*     radius computed below is negative. If it's equal to one, the */
/*     polar radius is zero. Either case is a problem, so signal an */
/*     error and check out. */

    if (f >= 1.) {
	chkin_("ZZPDTBOX", (ftnlen)8);
	setmsg_("Flattening coefficient from CORPAR array was #.", (ftnlen)47)
		;
	errdp_("*", &f, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("ZZPDTBOX", (ftnlen)8);
	return 0;
    }

/*     Get local copies of the bounds of the volume element. */

    minlon = bounds[0];
    maxlon = bounds[1];
    if (maxlon <= minlon) {
	maxlon += twopi_();
    }
    if (maxlon <= minlon) {
	chkin_("ZZPDTBOX", (ftnlen)8);
	setmsg_("Longitude bounds are #:#. Minimum longitude exceeds maximum"
		" by more than 2 pi.", (ftnlen)78);
	errdp_("#", &minlon, (ftnlen)1);
	errdp_("#", &bounds[1], (ftnlen)1);
	sigerr_("SPICE(BADLONGITUDERANGE)", (ftnlen)24);
	chkout_("ZZPDTBOX", (ftnlen)8);
	return 0;
    }
    minlat = bounds[2];
    maxlat = bounds[3];
    minalt = bounds[4];
    maxalt = bounds[5];
    if (minlat > maxlat) {
	chkin_("ZZPDTBOX", (ftnlen)8);
	setmsg_("Latitude bounds #:# are out of order.", (ftnlen)37);
	errdp_("#", &minlat, (ftnlen)1);
	errdp_("#", &maxlat, (ftnlen)1);
	sigerr_("SPICE(BADLATITUDEBOUNDS)", (ftnlen)24);
	chkout_("ZZPDTBOX", (ftnlen)8);
	return 0;
    }
    if (minlat < -halfpi_() - 1e-12) {
	chkin_("ZZPDTBOX", (ftnlen)8);
	setmsg_("Minimum latitude # is less than -pi/2.", (ftnlen)38);
	errdp_("#", &minlat, (ftnlen)1);
	sigerr_("SPICE(BADLATITUDERANGE)", (ftnlen)23);
	chkout_("ZZPDTBOX", (ftnlen)8);
	return 0;
    }
    if (maxlat > halfpi_() + 1e-12) {
	chkin_("ZZPDTBOX", (ftnlen)8);
	setmsg_("Maximum latitude # is more than -pi/2.", (ftnlen)38);
	errdp_("#", &maxlat, (ftnlen)1);
	sigerr_("SPICE(BADLATITUDERANGE)", (ftnlen)23);
	chkout_("ZZPDTBOX", (ftnlen)8);
	return 0;
    }
/* Computing MAX */
    d__1 = minlat, d__2 = -halfpi_();
    minlat = max(d__1,d__2);
/* Computing MIN */
    d__1 = maxlat, d__2 = halfpi_();
    maxlat = min(d__1,d__2);

/*     Let INRAD and OUTRAD be, respectively, the radii of the */
/*     orthogonal projections onto the X-Y plane of the element's arcs */
/*     of minimum and maximum distance from the Z axis. */

/*     If the element lies on or above the X-Y plane, the outer radius */
/*     is that of the lower latitude bound on the surface of maximum */
/*     altitude and the inner radius is that of the upper latitude bound */
/*     on the surface of minimum altitude. */

/*     These relationships are reversed for elements that lie on or */
/*     below the X-Y plane. */

/*     For elements that span the X-Y plane, the outer radius is that of */
/*     the coordinate system's equatorial radius plus the maximum */
/*     altitude. The inner radius is that of the latitude circle on the */
/*     surface of minimum altitude for which the absolute value of the */
/*     latitude is maximum. */

    if (minlat >= 0.) {
	georec_(&c_b34, &minlat, &maxalt, &re, &f, maxv);
	georec_(&c_b34, &maxlat, &minalt, &re, &f, minv);
	maxv[2] = 0.;
	minv[2] = 0.;
	outrad = vnorm_(maxv);
	inrad = vnorm_(minv);
    } else if (maxlat <= 0.) {
	georec_(&c_b34, &maxlat, &maxalt, &re, &f, maxv);
	georec_(&c_b34, &minlat, &minalt, &re, &f, minv);
	maxv[2] = 0.;
	minv[2] = 0.;
	outrad = vnorm_(maxv);
	inrad = vnorm_(minv);
    } else {
	outrad = re + maxalt;
/* Computing MAX */
	d__1 = abs(maxlat), d__2 = abs(minlat);
	maxabs = max(d__1,d__2);
	georec_(&c_b34, &maxabs, &minalt, &re, &f, minv);
	minv[2] = 0.;
	inrad = vnorm_(minv);
    }

/*     Let MIDLON be the longitude of the midpoint of the element's */
/*     longitude coverage. Let HDLON be one half of the extent of the */
/*     longitude coverage. */

    hdlon = (maxlon - minlon) / 2;
    midlon = minlon + hdlon;

/*     LR is the length of the bounding box in the radial direction, */
/*     where "radius" is defined as the distance from the Z axis. */

    if (hdlon <= halfpi_()) {
	*lr = outrad - inrad * cos(hdlon);
    } else {
	*lr = (1. - cos(hdlon)) * outrad;
    }

/*     The tangential length of bounding box depends on the longitude */
/*     extent. For any extent larger than Pi radians, the width */
/*     is just that of the outer radius. */

    if (hdlon > halfpi_()) {
	*lt = outrad * 2.;
    } else {
	*lt = outrad * 2. * sin(hdlon);
    }

/*     The height bounds are derived from the lowest and highest points */
/*     on the element. */

    if (minlat >= 0.) {
	georec_(&c_b34, &maxlat, &maxalt, &re, &f, topv);
	georec_(&c_b34, &minlat, &minalt, &re, &f, botv);
    } else if (maxlat < 0.) {
	georec_(&c_b34, &maxlat, &minalt, &re, &f, topv);
	georec_(&c_b34, &minlat, &maxalt, &re, &f, botv);
    } else {
	georec_(&c_b34, &maxlat, &maxalt, &re, &f, topv);
	georec_(&c_b34, &minlat, &maxalt, &re, &f, botv);
    }
    maxz = topv[2];
    minz = botv[2];
    *lz = maxz - minz;

/*     Make sure all dimensions are non-negative. */

    *lr = max(0.,*lr);
    *lt = max(0.,*lt);
    *lz = max(0.,*lz);

/*     Compute the coordinates of the center of the box. */

/*     The box center lies on the meridian of central */
/*     longitude. The outer tangential edge is at radius */
/*     OUTRAD. Let MIDR be the radius of the center. */

    midr = outrad - *lr / 2;
    d__1 = minz + *lz / 2;
    cylrec_(&midr, &midlon, &d__1, center);

/*     The radius is the distance from the center of the box */
/*     to any corner. */

    d__1 = *lr / 2;
    d__2 = *lt / 2;
    d__3 = *lz / 2;
    vpack_(&d__1, &d__2, &d__3, diag);
    *radius = vnorm_(diag);
    return 0;
} /* zzpdtbox_ */

