/* zzlatbox.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ZZLATBOX (Bounding box for latitudinal volume element) */
/* Subroutine */ int zzlatbox_(doublereal *bounds, doublereal *center, 
	doublereal *lr, doublereal *lt, doublereal *lz, doublereal *radius)
{
    /* System generated locals */
    doublereal d__1, d__2, d__3;

    /* Builtin functions */
    double cos(doublereal), sin(doublereal);

    /* Local variables */
    doublereal diag[3], midr, minr, maxr, minz, maxz;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    doublereal inrad, hdlon;
    extern /* Subroutine */ int vpack_(doublereal *, doublereal *, doublereal 
	    *, doublereal *), errdp_(char *, doublereal *, ftnlen);
    extern doublereal vnorm_(doublereal *), twopi_(void), halfpi_(void);
    extern /* Subroutine */ int cylrec_(doublereal *, doublereal *, 
	    doublereal *, doublereal *);
    doublereal midlon, minlat, maxlat;
    extern /* Subroutine */ int sigerr_(char *, ftnlen);
    doublereal minlon;
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    doublereal maxlon, outrad;
    extern /* Subroutine */ int setmsg_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Create a bounding box for a latitudinal volume element. */

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
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     BOUNDS     I   Array of coordinate bounds for segment. */
/*     CENTER     O   Center of bounding box. */
/*     LR         O   Extent of box in the cylindrical radial direction. */
/*     LT         O   Extent of box in the tangential direction. */
/*     LZ         O   Extent of box in the Z direction. */
/*     RADIUS     O   Radius of box. */
/*     MARGIN     P   Angular margin. */

/* $ Detailed_Input */

/*     BOUNDS     is a 2 x 3 double precision array containing bounds */
/*                on the latitudinal coordinates of the spatial region */
/*                (volume element) covered by a DSK segment. The */
/*                contents of BOUNDS are: */

/*                   BOUNDS(*,1)              Longitude bounds */
/*                   BOUNDS(*,2)              Latitude bounds */
/*                   BOUNDS(*,3)              Radius bounds */

/*                Elements (1,*) are lower bounds; elements (2,*) are */
/*                upper bounds. Angular units are radians. */

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

/*     MARGIN     is used for latitude range validity tests. Latitude */
/*                values can be out of range by as much as MARGIN. */

/* $ Exceptions */

/*     1) If the minimum longitude exceeds the maximum by more than */
/*        2*pi, the error SPICE(BADLONGITUDERANGE) is signaled. */

/*     2) If the latitude bounds are out of order, the error */
/*        SPICE(BADLATITUDEBOUNDS) is signaled. */

/*     3) If the minimum latitude is less than -pi/2 - MARGIN, or */
/*        if the maximum latitude is greater than pi/2 + MARGIN, */
/*        the error SPICE(BADLATITUDERANGE) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine provides a simple way to compute the center and */
/*     radius for an outer bounding sphere for the volume covered */
/*     by a DSK segment using latitudinal coordinates. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 03-FEB-2015 (NJB) */

/* -& */
/* $ Index_Entries */

/*     bounding box for latitudinal volume element */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     This routine uses discovery check-in. We check RETURN in order to */
/*     avoid performing math operations using invalid operands. */

    if (return_()) {
	return 0;
    }

/*     Get local copies of the bounds of the volume element. */

    minlon = bounds[0];
    maxlon = bounds[1];
    if (maxlon <= minlon) {
	maxlon += twopi_();
    }
    if (maxlon <= minlon) {
	chkin_("ZZLATBOX", (ftnlen)8);
	setmsg_("Longitude bounds are #:#. Minimum longitude exceeds maximum"
		" by more than 2 pi.", (ftnlen)78);
	errdp_("#", &minlon, (ftnlen)1);
	errdp_("#", &bounds[1], (ftnlen)1);
	sigerr_("SPICE(BADLONGITUDERANGE)", (ftnlen)24);
	chkout_("ZZLATBOX", (ftnlen)8);
	return 0;
    }
    minlat = bounds[2];
    maxlat = bounds[3];
    minr = bounds[4];
    maxr = bounds[5];
    if (minlat > maxlat) {
	chkin_("ZZLATBOX", (ftnlen)8);
	setmsg_("Latitude bounds #:# are out of order.", (ftnlen)37);
	errdp_("#", &minlat, (ftnlen)1);
	errdp_("#", &maxlat, (ftnlen)1);
	sigerr_("SPICE(BADLATITUDEBOUNDS)", (ftnlen)24);
	chkout_("ZZLATBOX", (ftnlen)8);
	return 0;
    }
    if (minlat < -halfpi_() - 1e-12) {
	chkin_("ZZLATBOX", (ftnlen)8);
	setmsg_("Minimum latitude # is less than -pi/2.", (ftnlen)38);
	errdp_("#", &minlat, (ftnlen)1);
	sigerr_("SPICE(BADLATITUDERANGE)", (ftnlen)23);
	chkout_("ZZLATBOX", (ftnlen)8);
	return 0;
    }
    if (maxlat > halfpi_() + 1e-12) {
	chkin_("ZZLATBOX", (ftnlen)8);
	setmsg_("Maximum latitude # is more than -pi/2.", (ftnlen)38);
	errdp_("#", &maxlat, (ftnlen)1);
	sigerr_("SPICE(BADLATITUDERANGE)", (ftnlen)23);
	chkout_("ZZLATBOX", (ftnlen)8);
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
/*     is that of the lower latitude bound on the outer bounding sphere, */
/*     and the inner radius is that of the upper latitude bound on the */
/*     inner bounding sphere. */

/*     These relationships are reversed for elements that lie on or */
/*     below the X-Y plane. */

/*     For elements that span the X-Y plane, the outer radius is that of */
/*     the outer bounding sphere. The inner radius is that of the */
/*     latitude circle on the inner bounding sphere for which the */
/*     absolute value of the latitude is maximum. */

    if (minlat >= 0.) {
	outrad = cos(minlat) * maxr;
	inrad = cos(maxlat) * minr;
    } else if (maxlat <= 0.) {
	outrad = cos(maxlat) * maxr;
	inrad = cos(minlat) * minr;
    } else {
	outrad = maxr;
/* Computing MAX */
	d__1 = abs(maxlat), d__2 = abs(minlat);
	inrad = cos((max(d__1,d__2))) * minr;
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
	maxz = maxr * sin(maxlat);
	minz = minr * sin(minlat);
    } else if (maxlat <= 0.) {
	maxz = minr * sin(maxlat);
	minz = maxr * sin(minlat);
    } else {
	maxz = maxr * sin(maxlat);
	minz = maxr * sin(minlat);
    }
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
} /* zzlatbox_ */

