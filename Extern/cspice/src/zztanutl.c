/* zztanutl.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ZZTANUTL ( DSK, tangent ray utilities ) */
/* Subroutine */ int zztanutl_0_(int n__, integer *curve, doublereal *srcrad, 
	integer *shape, integer *trgcde, integer *nsurf, integer *srflst, 
	integer *fixfid, doublereal *et, doublereal *plnvec, doublereal *axis,
	 doublereal *angle, logical *ocultd, doublereal *point)
{
    /* System generated locals */
    doublereal d__1;

    /* Local variables */
    extern /* Subroutine */ int vadd_(doublereal *, doublereal *, doublereal *
	    );
    doublereal apex[3];
    extern /* Subroutine */ int vhat_(doublereal *, doublereal *), vequ_(
	    doublereal *, doublereal *);
    static doublereal svet;
    extern /* Subroutine */ int zzsuelin_(integer *), zzsudski_(integer *, 
	    integer *, integer *, integer *), zzraysfx_(doublereal *, 
	    doublereal *, doublereal *, doublereal *, logical *), chkin_(char 
	    *, ftnlen), errdp_(char *, doublereal *, ftnlen), vcrss_(
	    doublereal *, doublereal *, doublereal *);
    extern logical vzero_(doublereal *);
    extern /* Subroutine */ int vrotv_(doublereal *, doublereal *, doublereal 
	    *, doublereal *);
    extern doublereal pi_(void);
    doublereal raydir[3];
    static doublereal svirad;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    doublereal vrtoff[3];
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen), vhatip_(doublereal *), vsclip_(doublereal *, 
	    doublereal *);
    static doublereal svaxis[3];
    extern logical return_(void);
    static doublereal svnrml[3];
    static integer svcurv;
    static doublereal svvrtx[3];

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     This is the umbrella routine for utilities supporting the */
/*     tangent ray finding capability used by LIMBPT and TERMPT. */

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

/*     DLA */
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

/*     Variable  I/O  Entry points */
/*     --------  ---  -------------------------------------------------- */
/*     CURVE      I   ZZTANINI */
/*     SRCRAD     I   ZZTANINI */
/*     SHAPE      I   ZZTANINI */
/*     TRGCDE     I   ZZTANINI */
/*     NSURF      I   ZZTANINI */
/*     SRFLST     I   ZZTANINI */
/*     FIXFID     I   ZZTANINI */
/*     ET         I   ZZTANINI */
/*     PLNVEC     I   ZZTANINI */
/*     AXIS       I   ZZTANINI */
/*     ANGLE      I   ZZTANSTA */
/*     OCULTD     O   ZZTANSTA */
/*     POINT      O   ZZTANSTA */

/* $ Detailed_Input */

/*     See the entry points. */

/* $ Detailed_Output */

/*     See the entry points. */

/* $ Parameters */

/*     See the entry points. */

/* $ Exceptions */

/*     1)  If this routine is called directly, it signals the error */
/*         SPICE(BOGUSENTRY). */

/*     See the entry points for descriptions of errors specific to */
/*     those routines. */

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

/*     This routine contains the following entry points that support */
/*     the generalized ray-surface intercept algorithm used by SINCPT: */

/*        ZZTANINI:   Initialize tangent ray utilities for limb or */
/*                    terminator computation. Terminators may be */
/*                    umbral or penumbral. */

/*        ZZTANSTA:   Compute state of ray for a given input angle. */
/*                    State is "occulted" or "not occulted." When */
/*                    the state is occulted, return the ray-surface */
/*                    intercept point. */

/* $ Examples */

/*     See usage in ZZTANGNT. */

/* $ Restrictions */

/*     1) This is a private routine. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -     SPICELIB Version 1.0.0 16-FEB-2016 (NJB) */

/*        Added error handling and headers. */

/*        Original version 26-OCT-2015 (NJB) */

/* -& */
/* $ Index_Entries */

/*     umbrella for tangent ray utilities */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Saved variables */

    /* Parameter adjustments */
    if (srflst) {
	}
    if (plnvec) {
	}
    if (axis) {
	}
    if (point) {
	}

    /* Function Body */
    switch(n__) {
	case 1: goto L_zztanini;
	case 2: goto L_zztansta;
	}

    chkin_("ZZTANUTL", (ftnlen)8);
    sigerr_("SPICE(BOGUSENTRY)", (ftnlen)17);
    chkout_("ZZTANUTL", (ftnlen)8);
    return 0;
/* $Procedure ZZTANINI ( DSK, tangent utility initialization ) */

L_zztanini:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     Initialize the data used by the tangent ray state routine */
/*     ZZTANSTA, which is used directly by ZZTANGNT and indirectly by */
/*     LIMBPT and TERMPT. */

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

/*     DLA */
/*     DSK */
/*     LIMB */
/*     RAY */
/*     TANGENT */
/*     TERMINATOR */
/*     TOPOGRAPHY */
/*     UTILITY */

/* $ Declarations */

/*     INTEGER               CURVE */
/*     DOUBLE PRECISION      SRCRAD */
/*     INTEGER               SHAPE */
/*     INTEGER               TRGCDE */
/*     INTEGER               NSURF */
/*     INTEGER               SRFLST ( * ) */
/*     INTEGER               FIXFID */
/*     DOUBLE PRECISION      ET */
/*     DOUBLE PRECISION      PLNVEC ( 3 ) */
/*     DOUBLE PRECISION      AXIS   ( 3 ) */

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


/* $ Detailed_Output */

/*     None. This routine operates by side effects. */

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

/*     This routine initializes the tangent ray utilities for limb or */
/*     terminator computation. */

/* $ Examples */

/*     See usage in ZZTANGNT. */

/* $ Restrictions */

/*     1) This is a private routine. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -     SPICELIB Version 1.0.0 11-FEB-2016 (NJB) */

/*        Original version 26-OCT-2015 (NJB) */

/* -& */
/* $ Index_Entries */

/*     initialize tangent ray finding state function */

/* -& */
    if (return_()) {
	return 0;
    }
    chkin_("ZZTANINI", (ftnlen)8);

/*     Check for zero vectors on input. */

    if (vzero_(axis)) {
	setmsg_("Input axis vector is the zero vector.", (ftnlen)37);
	sigerr_("SPICE(ZEROVECTOR)", (ftnlen)17);
	chkout_("ZZTANINI", (ftnlen)8);
	return 0;
    }
    if (vzero_(plnvec)) {
	setmsg_("Input reference vector is the zero vector.", (ftnlen)42);
	sigerr_("SPICE(ZEROVECTOR)", (ftnlen)17);
	chkout_("ZZTANINI", (ftnlen)8);
	return 0;
    }

/*     Save the curve type. */

    if (*curve != 0 && *curve != 1 && *curve != 2) {
	setmsg_("Curve type code # was not recognized.", (ftnlen)37);
	errint_("#", curve, (ftnlen)1);
	sigerr_("SPICE(BADCURVETYPE)", (ftnlen)19);
	chkout_("ZZTANINI", (ftnlen)8);
	return 0;
    }
    svcurv = *curve;

/*     Save the illumination source radius. */

    if (*curve == 1 || *curve == 2) {
	if (*srcrad <= 0.) {
	    setmsg_("The source radius was #. The radius must be positive fo"
		    "r a terminator computation.", (ftnlen)82);
	    errdp_("#", srcrad, (ftnlen)1);
	    sigerr_("SPICE(BADSOURCERADIUS)", (ftnlen)22);
	    chkout_("ZZTANINI", (ftnlen)8);
	    return 0;
	}
    }
    svirad = *srcrad;

/*     Compute a normal vector to the plane defined by */
/*     AXIS and PLNVEC. The direction of positive rotation */
/*     about the normal is from AXIS toward PLNVEC. */

    vcrss_(axis, plnvec, svnrml);
    if (vzero_(svnrml)) {
	setmsg_("Input reference vector and axis vector are linearly depende"
		"nt.", (ftnlen)62);
	sigerr_("SPICE(DEGENERATECASE)", (ftnlen)21);
	chkout_("ZZTANINI", (ftnlen)8);
	return 0;
    }

/*     Scale the normal vector to unit length. */

    vhatip_(svnrml);

/*     Save a unit-length copy of the input axis. */
/*     Save the original axis as the ray's vertex; this */
/*     will be used directly in the limb computation and */
/*     will be used, after addition of an offset, in the */
/*     terminator computations. Save the evaluation epoch. */

    vequ_(axis, svvrtx);
    vhat_(axis, svaxis);
    svet = *et;

/*     Prepare the DSK SINCPT utilities for a computation with */
/*     the input surface set. */

    if (*shape == 1) {

/*        This is the ellipsoid case. */

	zzsuelin_(trgcde);
    } else if (*shape == 2) {

/*        This is the DSK case. */

	zzsudski_(trgcde, nsurf, srflst, fixfid);
    } else {
	setmsg_("Target shape code # was not recognized.", (ftnlen)39);
	errint_("#", shape, (ftnlen)1);
	sigerr_("SPICE(BADSHAPE)", (ftnlen)15);
	chkout_("ZZTANINI", (ftnlen)8);
	return 0;
    }
    chkout_("ZZTANINI", (ftnlen)8);
    return 0;
/* $Procedure ZZTANSTA ( DSK, tangent ray state ) */

L_zztansta:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     This is the state callback routine used by ZZTANSLV to */
/*     find tangent rays on the target body's surface. */

/*     Indicate whether a vector emanating from a given vertex, and */
/*     rotated about the normal by the specified angle, measured from */
/*     the AXIS direction, intersects the target. If an intersection */
/*     exists, return the intercept closest to the ray's vertex. */

/*     The ray's vertex depends on the curve type set via a call */
/*     to ZZTANINI. */

/*     This routine is called directly by ZZTANGNT and is used */
/*     indirectly by LIMBPT and TERMPT. */

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

/*     DLA */
/*     DSK */
/*     LIMB */
/*     RAY */
/*     TANGENT */
/*     TERMINATOR */
/*     TOPOGRAPHY */
/*     UTILITY */

/* $ Declarations */

/*     DOUBLE PRECISION      ANGLE */
/*     LOGICAL               OCULTD */
/*     DOUBLE PRECISION      POINT  ( 3 ) */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     ANGLE      I   Angle of ray. */
/*     OCULTD     O   Occultation state flag. True if ray is occulted. */
/*     POINT      O   Tangent point on target. */

/* $ Detailed_Input */

/*     ANGLE      is the angle between a ray and the AXIS vector */
/*                stored by ZZTANINI. AXIS points away from the */
/*                target. */

/*                The vertex of the vector depends on the curve type, */
/*                which also is stored by ZZTANINI. */

/*                For a limb computation, the vertex is the observer's */
/*                location. */

/*                For an umbral terminator computation, the vertex */
/*                is on the surface of the light source, in the half- */
/*                plane defined by PLNDEF and AXIS. The line containing */
/*                the ray is tangent to the light source at the vertex. */


/*                For a penumbral terminator computation, the vertex is */
/*                on the surface of the light source, in the half-plane */
/*                complementary to that defined by PLNDEF and AXIS. The */
/*                line containing the ray is tangent to the light source */
/*                at the vertex. */

/*                Units are radians. */

/* $ Detailed_Output */

/*     OCULTD     is a logical flag that is .TRUE. if and only if the */
/*                ray defined by ANGLE and the values set by ZZTANINI */
/*                intersects the target. */

/*     POINT      is the ray-surface intercept closest to the ray's */
/*                vertex, if an intercept exists. */

/* $ Parameters */

/*     See zzdsk.inc for declarations of parameters used internally. */

/* $ Exceptions */

/*     1)  Any errors that occur while looking up DSK data will be */
/*         signaled by a routine in the call tree of this routine. */

/*     2)  Any errors that occur while computing ray-surface intercepts */
/*         will be signaled by a routine in the call tree of this */
/*         routine. */

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

/*     This routine computes the state of ray for a given input angle. */
/*     The state is "occulted" or "not occulted." When the state is */
/*     occulted, this routine returns the surface intercept point */
/*     nearest to the ray's vertex. */

/*     This is the state callback routine used by ZZTANSLV to */
/*     find tangent rays on the target body's surface. */

/* $ Examples */

/*     See usage in ZZTANGNT and ZZTANSLV. */

/* $ Restrictions */

/*     1) This is a private routine. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -     SPICELIB Version 1.0.0 11-FEB-2016 (NJB) */

/*        Original version 26-OCT-2015 (NJB) */

/* -& */
/* $ Index_Entries */

/*     tangent ray finding occultation state and intercept */

/* -& */
    if (return_()) {
	return 0;
    }
    chkin_("ZZTANSTA", (ftnlen)8);
    if (svcurv == 0) {

/*        This is the limb case. */

/*        We'll rotate SVAXIS by ANGLE to achieve the desired result. */

	vrotv_(svaxis, svnrml, angle, raydir);
	zzraysfx_(svvrtx, raydir, &svet, point, ocultd);
    } else if (svcurv == 1) {

/*        This is the umbral terminator case. */

/*        Produce the ray's direction vector by rotating */
/*        the axis about the cutting half-plane normal by */
/*        the input angle. */

	vrotv_(svaxis, svnrml, angle, raydir);

/*        Produce the offset of the ray's vertex from the */
/*        center of the source by rotating the axis */
/*        vector by ANGLE-pi/2 radians. The length */
/*        of the vector must be SVIRAD. The saved axis */
/*        has unit length. */

	d__1 = *angle - pi_() / 2;
	vrotv_(svaxis, svnrml, &d__1, vrtoff);
	vsclip_(&svirad, vrtoff);
	vadd_(svvrtx, vrtoff, apex);
	zzraysfx_(apex, raydir, &svet, point, ocultd);
    } else if (svcurv == 2) {

/*        This is the penumbral terminator case. */

/*        Produce the ray's direction vector by rotating */
/*        the axis about the cutting half-plane normal by */
/*        the *negative* of the input angle. */

	d__1 = -(*angle);
	vrotv_(svaxis, svnrml, &d__1, raydir);

/*        Produce the ray's vertex by rotating the axis */
/*        vector about the normal, *not its negative,* */
/*        by 3*pi/2 - ANGLE radians. The length of the vector */
/*        must be SRCRAD. The saved axis has unit length. */

	d__1 = pi_() * 1.5 - *angle;
	vrotv_(svaxis, svnrml, &d__1, vrtoff);
	vsclip_(&svirad, vrtoff);
	vadd_(svvrtx, vrtoff, apex);
	zzraysfx_(apex, raydir, &svet, point, ocultd);
    } else {

/*        This case should have been ruled out by a check in */
/*        ZZTANINI. Check again anyway. */

	setmsg_("Bad curve type code #.", (ftnlen)22);
	errint_("#", &svcurv, (ftnlen)1);
	sigerr_("SPICE(BUG)", (ftnlen)10);
	chkout_("ZZTANSTA", (ftnlen)8);
	return 0;
    }
    chkout_("ZZTANSTA", (ftnlen)8);
    return 0;
} /* zztanutl_ */

/* Subroutine */ int zztanutl_(integer *curve, doublereal *srcrad, integer *
	shape, integer *trgcde, integer *nsurf, integer *srflst, integer *
	fixfid, doublereal *et, doublereal *plnvec, doublereal *axis, 
	doublereal *angle, logical *ocultd, doublereal *point)
{
    return zztanutl_0_(0, curve, srcrad, shape, trgcde, nsurf, srflst, fixfid,
	     et, plnvec, axis, angle, ocultd, point);
    }

/* Subroutine */ int zztanini_(integer *curve, doublereal *srcrad, integer *
	shape, integer *trgcde, integer *nsurf, integer *srflst, integer *
	fixfid, doublereal *et, doublereal *plnvec, doublereal *axis)
{
    return zztanutl_0_(1, curve, srcrad, shape, trgcde, nsurf, srflst, fixfid,
	     et, plnvec, axis, (doublereal *)0, (logical *)0, (doublereal *)0)
	    ;
    }

/* Subroutine */ int zztansta_(doublereal *angle, logical *ocultd, doublereal 
	*point)
{
    return zztanutl_0_(2, (integer *)0, (doublereal *)0, (integer *)0, (
	    integer *)0, (integer *)0, (integer *)0, (integer *)0, (
	    doublereal *)0, (doublereal *)0, (doublereal *)0, angle, ocultd, 
	    point);
    }

