/* zztangnt.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__0 = 0;
static doublereal c_b9 = 1e-12;

/* $Procedure ZZTANGNT ( DSK, find target tangent rays in half-plane ) */
/* Subroutine */ int zztangnt_(integer *curve, doublereal *srcrad, integer *
	shape, integer *trgcde, integer *nsurf, integer *srflst, integer *
	fixfid, doublereal *et, doublereal *plnvec, doublereal *axis, 
	doublereal *schstp, doublereal *soltol, doublereal *result, 
	doublereal *points)
{
    /* System generated locals */
    integer i__1;
    doublereal d__1;

    /* Local variables */
    extern /* Subroutine */ int vequ_(doublereal *, doublereal *), zzmaxrad_(
	    doublereal *), zztanini_(integer *, doublereal *, integer *, 
	    integer *, integer *, integer *, integer *, doublereal *, 
	    doublereal *, doublereal *);
    extern /* Subroutine */ int zztansta_();
    integer i__;
    extern /* Subroutine */ int zztanslv_(U_fp, U_fp, U_fp, logical *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, logical *);
    extern integer cardd_(doublereal *);
    integer n;
    doublereal r__, alpha;
    extern /* Subroutine */ int chkin_(char *, ftnlen), errdp_(char *, 
	    doublereal *, ftnlen);
    logical cstep;
    doublereal start;
    extern doublereal vnorm_(doublereal *);
    extern /* Subroutine */ int vrotv_(doublereal *, doublereal *, doublereal 
	    *, doublereal *);
    extern logical failed_(void);
    extern doublereal pi_(void);
    logical endflg[2];
    extern /* Subroutine */ int scardd_(integer *, doublereal *);
    extern doublereal dasine_(doublereal *, doublereal *);
    extern /* Subroutine */ int gfrefn_();
    doublereal refvec[3], maxrad, finish;
    extern /* Subroutine */ int gfstep_();
    doublereal mindst;
    extern /* Subroutine */ int chkout_(char *, ftnlen), setmsg_(char *, 
	    ftnlen), sigerr_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     Solve for tangent points on a target surface of rays contained */
/*     within a specified half-plane. The rays may emanate from a */
/*     specified vertex or may be tangent to a "source" sphere; the */
/*     first case supports limb finding and the second terminator */
/*     finding. */

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
/*     LIMB */
/*     RAY */
/*     TANGENT */
/*     TERMINATOR */
/*     TOPOGRAPHY */
/*     UTILITY */

/* $ Declarations */

/*     File: zzdsk.inc */


/*     Version 4.0.0 13-NOV-2015 (NJB) */

/*        Changed parameter LBTLEN to CVTLEN. */
/*        Added parameter LMBCRV. */

/*     Version 3.0.0 05-NOV-2015 (NJB) */

/*        Added parameters */

/*           CTRCOR */
/*           ELLCOR */
/*           GUIDED */
/*           LBTLEN */
/*           PNMBRL */
/*           TANGNT */
/*           TMTLEN */
/*           UMBRAL */

/*     Version 2.0.0 04-MAR-2015 (NJB) */

/*        Removed declaration of parameter SHPLEN. */
/*        This name is already in use in the include */
/*        file gf.inc. */

/*     Version 1.0.0 26-JAN-2015 (NJB) */


/*     Parameters supporting METHOD string parsing: */


/*     Local method length. */


/*     Length of sub-point type string. */


/*     Length of curve type string. */


/*     Limb type parameter codes. */


/*     Length of terminator type string. */


/*     Terminator type and limb parameter codes. */


/*     Length of aberration correction locus string. */


/*     Aberration correction locus codes. */


/*     End of include file zzdsk.inc */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     CURVE      I   Type of curve: limb or terminator type. */
/*     SRCRAD     I   Radius of illumination source. */
/*     SHAPE      I   Target shape. */
/*     TRGCDE     I   Target body ID code. */
/*     NSURF      I   Number of surfaces in list. */
/*     SRFLST     I   Surface ID list. */
/*     FIXFID     I   Frame ID of target body-fixed frame. */
/*     ET         I   Epoch, expressed as seconds past J2000 TDB. */
/*     PLNVEC     I   Reference vector contained in cutting half-plane. */
/*     AXIS       I   Axis vector: edge of cutting half-plane. */
/*     SCHSTP     I   Angular step size for searching. */
/*     SOLTOL     I   Solution convergence tolerance. */
/*     RESULT     O   Cell (not window) containing transition angles. */
/*     POINTS     O   Array of tangent points. */
/*     ELLSHP     P   Ellipsoid shape code. */
/*     DSKSHP     P   DSK shape code. */
/*     LMBCRV     P   Limb code. */
/*     UMBRAL     P   Umbral terminator code. */
/*     PNMBRL     P   Penumbral terminator code. */

/* $ Detailed_Input */

/*     CURVE      is an integer code indicating the type of set on the */
/*                target body on which tangent points are to be found. */
/*                When the target is an ellipsoid, the set is literally */
/*                a curve. When the target is a DSK shape, the set is */
/*                often not connected; so it is not actually a curve; */
/*                still, we retain the familiar terminology for the */
/*                ellipsoid case. */

/*                Possible values and meanings are: */

/*                   LMBCRV            limb */
/*                   UMBRAL            umbral terminator */
/*                   PNMBRL            penumbral terminator */

/*                Terminator computations are performed assuming */
/*                the light source is spherical. */


/*     SRCRAD     is the radius of the illumination source. This */
/*                value is used for terminator computations only. */
/*                When used, SRCRAD must be strictly positive. */
/*                Units are km. */


/*     SHAPE      is an integer code indicating the target body shape. */

/*                Possible values and meanings are: */

/*                   ELLSHP            shape is modeled as an ellipsoid */
/*                   DSKSHP            shape is provided by DSK data */


/*     TRGCDE     is the body ID code of the target body. */


/*     NSURF, */
/*     SRFLST     are, respectively, the count of surface IDs and the */
/*                surface ID list. */


/*     FIXFID     is the frame ID code of a body-fixed frame centered */
/*                on the target body. The output tangent points will */
/*                be expressed in this frame. */


/*     ET         is the computation epoch, expressed as seconds past */
/*                J2000 TDB. ET is used for DSK segment selection. */
/*                If the target shape is modeled as an ellipsoid, ET */
/*                is ignored. */


/*     PLNVEC     is a vector used to define a half-plane in which */
/*                to search for tangent rays. The half-plane contains */
/*                PLNVEC, the target body center, and AXIS. */

/*                For limb and umbral terminator computations, */
/*                tangent rays lie in the half-plane containing PLNVEC. */

/*                For penumbral terminator computations, tangent rays */
/*                touch the target in the half-plane containing PLNVEC, */
/*                but touch the light source in the complementary */
/*                half-plane bounded by the line containing AXIS. */


/*     AXIS       is a second vector used to define a half-plane in */
/*                which to search for tangent vectors. AXIS lies in */
/*                the line forming the boundary of the half-plane. */

/*                For limb computations, AXIS points from the target */
/*                toward the observation point. */

/*                For terminator computations, AXIS points from the */
/*                target toward the center of the illumination source. */


/*     SCHSTP, */
/*     SOLTOL     are, respectively, the search angular step size and */
/*                solution convergence tolerance used to find tangent */
/*                rays and associated limb points within each cutting */
/*                half plane. */

/* $ Detailed_Output */

/*     RESULT     is a cell (not a window) containing ray-axis angular */
/*                separation values at which ray-surface tangencies */
/*                occur. */

/*                Here "angular separation" refers to the angle between */
/*                a ray and the input vector AXIS. AXIS points away from */
/*                the target. */

/*                The vertex of the vector depends on the curve type, */
/*                which is indicated by the input code CURVE. */

/*                For a limb computation, the vertex is the observer's */
/*                location. */

/*                For an umbral terminator computation, the vertex is on */
/*                the surface of the light source, in the half- plane */
/*                defined by PLNDEF and AXIS. The line containing the */
/*                ray is tangent to the light source at the vertex. */

/*                For a penumbral terminator computation, the vertex is */
/*                on the surface of the light source, in the half-plane */
/*                complementary to that defined by PLNDEF and AXIS. The */
/*                line containing the ray is tangent to the light source */
/*                at the vertex. */

/*                Units are radians. */


/*     POINTS     is an array of tangent points on the target body's */
/*                surface. Elements */

/*                   POINTS(J,I), J = 1, 3 */

/*                constitute the tangent point for the ray having */
/*                angular separation */

/*                   RESULT(I) */

/*                from AXIS. Here the ray's vertex is as described */
/*                above in the discussion of the RESULT output cell. */


/* $ Parameters */

/*     See the INCLUDE file zzdsk.inc for declarations of these */
/*     parameters. */


/*     ELLSHP     is a code specifying an ellipsoidal shape model. */

/*     DSKSHP     is a code specifying a DSK-based shape model. */

/*     LMBCRV     is a code specifying a limb computation. */

/*     UMBRAL     is a code specifying an umbral terminator computation. */

/*                The umbral terminator is the boundary of the portion */
/*                of the target surface that receives no light from the */
/*                illumination source. */

/*     PNMBRL     is a code specifying an penumbral terminator */
/*                computation. */

/*                The penumbral terminator is the boundary of the */
/*                portion of the target surface that is not subject to */
/*                self-occultation of light from the illumination */
/*                source. Given that the light source is modeled as */
/*                a sphere, from any target surface point nearer to */
/*                the source than the penumbral terminator, the source */
/*                appears to be a lit disc. */


/* $ Exceptions */

/*     1)  If AXIS is the zero vector, the error SPICE(ZEROVECTOR) is */
/*         signaled. */

/*     2)  If PLNDEF is the zero vector, the error SPICE(ZEROVECTOR) is */
/*         signaled. */

/*     3)  If the curve type code is unrecognized, the error */
/*         SPICE(BADCURVETYPE) is signaled. */

/*     4)  If a terminator curve is specified by the source radius */
/*         is non-positive, the error SPICE(BADSOURCERADIUS) is */
/*         signaled. */

/*     5)  If AXIS and PLNDEF are linearly dependent, the error */
/*         SPICE(DEGENERATECASE) is signaled. */

/*     6)  If the target shape code is unrecognized, the error */
/*         SPICE(BADSHAPE) is signaled. */

/* $ Files */

/*     This routine makes use of DSK files loaded by the ZZDSKBSR */
/*     subsystem. */

/*     If any loaded DSK segment has a reference frame that is not */
/*     centered at the segment's central (target) body, SPK data are */
/*     required to compute the offset between the frame's center and */
/*     the segment's center. */

/*     Frame kernels may be required in order to look up a segment's */
/*     frame center offset. In some cases, additional kernels such */
/*     as CK kernels and SCLK kernels could be required to support */
/*     the offset vector lookup. */

/*     This routine uses PCK data for target body reference */
/*     ellipsoids. */

/* $ Particulars */

/*     This routine is meant to be used only by the DSK subsystem. */

/*     This routine computes target tangent points for limb or */
/*     terminator computations. */

/* $ Examples */

/*     See usage in LIMBPT and TERMPT. */

/* $ Restrictions */

/*     1) This is a private routine. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -     SPICELIB Version 1.1.0, 17-OCT-2021 (NJB) */

/*         Deleted unnecessary declarations of external routines */
/*         GFBAIL and GFREPU. */

/* -     SPICELIB Version 1.0.0, 24-AUG-2016 (NJB) */

/*           10-FEB-2016 (NJB) */

/*           Removed VERTEX from argument list. */

/*           Original version 26-OCT-2015 (NJB) */
/* -& */
/* $ Index_Entries */

/*     find target tangent rays in specified half-plane */

/* -& */

/*     SPICELIB functions */


/*     EXTERNAL routines */


/*     Local parameters */


/*     Local variables */

    if (return_()) {
	return 0;
    }
    chkin_("ZZTANGNT", (ftnlen)8);

/*     Empty the result window. */

    scardd_(&c__0, result);

/*     Rotate the plane definition vector by pi about AXIS if */
/*     we're generating penumbral terminator points. */

    if (*curve == 2) {
	d__1 = pi_();
	vrotv_(plnvec, axis, &d__1, refvec);
    } else {
	vequ_(plnvec, refvec);
    }

/*     Prepare the tangent finding utilities. */

    zztanini_(curve, srcrad, shape, trgcde, nsurf, srflst, fixfid, et, refvec,
	     axis);

/*     Fetch a maximum bounding radius for the target. */

/*     Caution: we assume ZZTANINI has initialized the */
/*     SINCPT utility subsystem by calling one of */

/*        ZZSUELIN */
/*        ZZSUDSKI */

    zzmaxrad_(&maxrad);

/*     Scale up MAXRAD slightly to ensure bracketing. */

    maxrad *= 1.001;
    if (failed_()) {
	chkout_("ZZTANGNT", (ftnlen)8);
	return 0;
    }
    if (maxrad <= 0.) {
	setmsg_("Target maximum radius # is non-positive.", (ftnlen)40);
	errdp_("#", &maxrad, (ftnlen)1);
	sigerr_("SPICE(INVALIDRADIUS)", (ftnlen)20);
	chkout_("ZZTANGNT", (ftnlen)8);
	return 0;
    }
    if (*curve == 0) {

/*        We're looking for limb points. */

/*        Set the initial ray-axis separation. */

	start = 0.;

/*        If the vertex is outside of the bounding sphere, */
/*        set the initial angle to the supplement of */
/*        the angular radius of the target, based on */
/*        its maximum radius. */

	r__ = vnorm_(axis);
	mindst = maxrad * 1.0000000000010001;
	if (r__ > mindst) {
	    d__1 = maxrad / r__;
	    start = pi_() - dasine_(&d__1, &c_b9);
	}
	if (failed_()) {
	    chkout_("ZZTANGNT", (ftnlen)8);
	    return 0;
	}

/*        Set the final ray-axis separation. */

	finish = pi_();
    } else {

/*        For the terminator cases, check for an invalid source radius. */

	if (*srcrad <= 0.) {
	    setmsg_("Source radius # is non-positive.", (ftnlen)32);
	    errdp_("#", srcrad, (ftnlen)1);
	    sigerr_("SPICE(INVALIDRADIUS)", (ftnlen)20);
	    chkout_("ZZTANGNT", (ftnlen)8);
	    return 0;
	}

/*        Make sure the source and outer bounding sphere of the */
/*        target don't intersect. */

	r__ = vnorm_(axis);
	if (*srcrad + maxrad > r__) {
	    setmsg_("Source radius # and target maximum radius # sum to #; d"
		    "istance between source and target centers is #. Source a"
		    "nd target are too close together.", (ftnlen)144);
	    errdp_("#", srcrad, (ftnlen)1);
	    errdp_("#", &maxrad, (ftnlen)1);
	    errdp_("#", &r__, (ftnlen)1);
	    sigerr_("SPICE(OBJECTSTOOCLOSE)", (ftnlen)22);
	    chkout_("ZZTANGNT", (ftnlen)8);
	    return 0;
	}
	if (*curve == 1) {

/*           We'll search for a point on the umbral terminator. */

/*           For this search, the angle we measure is that between a ray */
/*           tangent to the source and a ray emanating from the tangent */
/*           point and parallel to the axis, pointing in the */
/*           target-source direction. The ray lies in a plane containing */
/*           the source-target axis. */

/*           The minimum angle is achieved when the ray is tangent to */
/*           the target sphere; the maximum angle is achieved when the */
/*           ray intersects the target center. */


/*           The following equation is valid regardless of whether */
/*           or not SRCRAD > MAXRAD. */

	    d__1 = (*srcrad - maxrad) / r__;
	    start = pi_() + dasine_(&d__1, &c_b9);
	    if (failed_()) {
		chkout_("ZZTANGNT", (ftnlen)8);
		return 0;
	    }

/*           Set the final ray-axis separation. */

	    d__1 = *srcrad / r__;
	    finish = pi_() + dasine_(&d__1, &c_b9);
	    if (failed_()) {
		chkout_("ZZTANGNT", (ftnlen)8);
		return 0;
	    }
	} else if (*curve == 2) {

/*           We'll search for a point on the umbral terminator. */

/*           We measure the ray's angle from the axis, but in this case, */
/*           the angle increases in the clockwise (negative sense about */
/*           the normal to the cutting half-plane defined by the cross */
/*           product of the axis and the reference vector. Each ray */
/*           emanating from, and tangent to, the source's surface passes */
/*           through the axis at a point between the source and target. */

/*           In order to use the root-finding utilities, we treat the */
/*           angle as a positive quantity. */

/*           The initial ray contains a line segment connecting */
/*           tangency points on each sphere. The segment, axis, */
/*           and radii of the spheres form two similar triangles. */
/*           Below, ALPHA is the fraction of the source-target */
/*           distance belonging to the triangle having a vertex */
/*           at the center of the source. */

	    alpha = *srcrad / (*srcrad + maxrad);
	    d__1 = *srcrad / (alpha * r__);
	    start = pi_() - dasine_(&d__1, &c_b9);
	    if (failed_()) {
		chkout_("ZZTANGNT", (ftnlen)8);
		return 0;
	    }

/*           We stop looking when the ray intersects the target center. */

	    d__1 = *srcrad / r__;
	    finish = pi_() - dasine_(&d__1, &c_b9);
	    if (failed_()) {
		chkout_("ZZTANGNT", (ftnlen)8);
		return 0;
	    }
	} else {
	    setmsg_("Input curve code # was not recognized.", (ftnlen)38);
	    errint_("#", curve, (ftnlen)1);
	    sigerr_("SPICE(BUG)", (ftnlen)10);
	    chkout_("ZZTANGNT", (ftnlen)8);
	    return 0;
	}
    }

/*     Search for ray occultations. The endpoints of the occultation */
/*     intervals are angles at which tangency occurs. */

/*     We consider the angle to be measured from the AXIS vector. */
/*     The initial and final values START and FINISH have been set */
/*     above. */

    cstep = TRUE_;
    zztanslv_((U_fp)zztansta_, (U_fp)gfstep_, (U_fp)gfrefn_, &cstep, schstp, &
	    start, &finish, soltol, result, points, endflg);
    if (failed_()) {
	chkout_("ZZTANGNT", (ftnlen)8);
	return 0;
    }

/*     If the first endpoint of RESULT is the interval start but is not */
/*     a point of transition, delete it from RESULT. Note that RESULT */
/*     becomes a cell rather than a window. We must delete the */
/*     corresponding point from the POINTS array as well. */

    if (cardd_(result) > 0) {
	if (result[6] == start && ! endflg[0]) {
	    n = cardd_(result);
	    i__1 = n;
	    for (i__ = 2; i__ <= i__1; ++i__) {
		result[i__ + 4] = result[i__ + 5];
		vequ_(&points[i__ * 3 - 3], &points[(i__ - 1) * 3 - 3]);
	    }
	    i__1 = n - 1;
	    scardd_(&i__1, result);
	}
    }

/*     If the final endpoint of RESULT is not a transition, delete */
/*     it as well. In this case decrementing the cardinality of */
/*     RESULT suffices. */

    n = cardd_(result);
    if (n > 0) {
	if (result[n + 5] == finish && ! endflg[1]) {
	    i__1 = n - 1;
	    scardd_(&i__1, result);
	}
    }
    chkout_("ZZTANGNT", (ftnlen)8);
    return 0;
} /* zztangnt_ */

