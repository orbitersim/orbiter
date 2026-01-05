/* bodeul.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__3 = 3;
static integer c__800 = 800;
static integer c__200 = 200;

/* $Procedure BODEUL ( Return Euler angles for a body ) */
/* Subroutine */ int bodeul_(integer *body, doublereal *et, doublereal *ra, 
	doublereal *dec, doublereal *w, doublereal *lambda)
{
    /* Initialized data */

    static logical first = TRUE_;
    static logical found = FALSE_;

    /* System generated locals */
    integer i__1, i__2, i__3, i__4;
    doublereal d__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    double d_mod(doublereal *, doublereal *);
    integer i_dnnt(doublereal *), s_rnge(char *, integer, char *, integer);
    double pow_di(doublereal *, integer *), sin(doublereal), cos(doublereal);

    /* Local variables */
    static char bref[32], item[32];
    static doublereal j2ref[9]	/* was [3][3] */, j2bfx[9]	/* was [3][3] 
	    */;
    static char item2[32];
    extern integer zzbodbry_(integer *);
    extern /* Subroutine */ int eul2m_(doublereal *, doublereal *, doublereal 
	    *, integer *, integer *, integer *, doublereal *), m2eul_(
	    doublereal *, integer *, integer *, integer *, doublereal *, 
	    doublereal *, doublereal *);
    static doublereal d__;
    static integer i__, j, k, m;
    static doublereal dcoef[3], t, delta;
    static integer refid;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    static doublereal epoch, rcoef[3], tcoef[800], wcoef[3];
    extern /* Subroutine */ int errch_(char *, char *, ftnlen, ftnlen);
    static doublereal theta, dpval;
    extern /* Subroutine */ int repmi_(char *, char *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen), errdp_(char *, doublereal *, ftnlen);
    static doublereal costh[200];
    extern doublereal vdotg_(doublereal *, doublereal *, integer *);
    static char dtype[1];
    static doublereal sinth[200];
    extern doublereal twopi_(void);
    static integer j2code;
    static doublereal rf2bfx[9]	/* was [3][3] */;
    static logical found2;
    static doublereal ac[200], dc[200];
    static integer na, nd;
    extern logical failed_(void);
    static doublereal wc[200];
    static integer nl;
    extern logical bodfnd_(integer *, char *, ftnlen);
    extern /* Subroutine */ int cleard_(integer *, doublereal *), bodvcd_(
	    integer *, char *, integer *, integer *, doublereal *, ftnlen);
    extern doublereal halfpi_(void);
    static integer nw;
    static doublereal conepc, eulang[6];
    static integer nphase, ntheta;
    extern /* Subroutine */ int pckeul_(integer *, doublereal *, logical *, 
	    char *, doublereal *, ftnlen), gdpool_(char *, integer *, integer 
	    *, integer *, doublereal *, logical *, ftnlen), gipool_(char *, 
	    integer *, integer *, integer *, integer *, logical *, ftnlen);
    static integer nphsco;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), irfnum_(char *, integer *, ftnlen), dtpool_(char *, 
	    logical *, integer *, char *, ftnlen, ftnlen), setmsg_(char *, 
	    ftnlen), errint_(char *, integer *, ftnlen), irfrot_(integer *, 
	    integer *, doublereal *);
    extern logical return_(void);
    extern doublereal j2000_(void);
    static integer deg, dim, ref;
    static doublereal phi;
    extern doublereal rpd_(void), spd_(void);
    extern /* Subroutine */ int mxm_(doublereal *, doublereal *, doublereal *)
	    ;

/* $ Abstract */

/*     Return the Euler angles needed to compute the transformation from */
/*     inertial to body-fixed coordinates for any body in the kernel */
/*     pool. */

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

/*     PCK */
/*     NAIF_IDS */
/*     TIME */

/* $ Keywords */

/*     CONSTANTS */
/*     ROTATION */
/*     TRANSFORMATION */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     BODY       I   ID code of body. */
/*     ET         I   Epoch of transformation. */
/*     RA         O   Right ascension of the (IAU) north pole. */
/*     DEC        O   Declination of the (IAU) north pole of the body. */
/*     W          O   Prime meridian rotation angle. */
/*     LAMBDA     O   Angle between the prime meridian and longitude of */
/*                    longest axis. */

/* $ Detailed_Input */

/*     BODY     is the integer ID code of the body for which the */
/*              transformation is requested. Bodies are numbered */
/*              according to the standard NAIF numbering scheme. */

/*     ET       is the epoch at which the transformation is */
/*              requested. */

/* $ Detailed_Output */

/*     RA, */
/*     DEC      are the right ascension and declination of the */
/*              (IAU) north pole of the body at the epoch of */
/*              transformation. RA and DEC are given in radians. */

/*     W        is the angle between the ascending node of the */
/*              body-fixed equatorial plane on the inertial */
/*              equatorial plane and the prime meridian of the body. */
/*              The node is the cross product of the inertial */
/*              frame's Z-axis with the Z-axis of the body-fixed */
/*              frame. The angle is measured in the positive */
/*              (counterclockwise) sense about the body-fixed */
/*              Z-axis, from the node to the prime meridian. W is */
/*              given in radians. */

/*     LAMBDA   is the angle between the prime meridian and the */
/*              longest axis of the tri-axial ellipsoid which */
/*              models the body. LAMBDA is given in radians. */
/*              See the $Particulars section below for further */
/*              discussion. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If any of the PCK keywords required to compute the angles are */
/*         not available in the kernel pool, an error is signaled by a */
/*         routine in the call tree of this routine. */

/*     2)  If the number of phase terms is insufficient, the error */
/*         SPICE(INSUFFICIENTANGLES) is signaled. */

/*     3)  If, for a given body, both forms of the kernel variable names */

/*            BODY<body ID>_CONSTANTS_JED_EPOCH */
/*            BODY<body ID>_CONSTS_JED_EPOCH */

/*         are found in the kernel pool, the error */
/*         SPICE(COMPETINGEPOCHSPEC) is signaled. This is done */
/*         regardless of whether the values assigned to the kernel */
/*         variable names match. */

/*     4)  If, for a given body, both forms of the kernel variable names */

/*            BODY<body ID>_CONSTANTS_REF_FRAME */
/*            BODY<body ID>_CONSTS_REF_FRAME */

/*         are found in the kernel pool, the error */
/*         SPICE(COMPETINGFRAMESPEC) is signaled. This is done */
/*         regardless of whether the values assigned to the kernel */
/*         variable names match. */

/*     5)  If the central body associated with the input BODY, whether */
/*         a system barycenter or BODY itself, has associated phase */
/*         angles (aka nutation precession angles), and the kernel */
/*         variable BODY<body ID>_MAX_PHASE_DEGREE for the central */
/*         body is present but has a value outside the range 1:3, */
/*         the error SPICE(DEGREEOUTOFRANGE) is signaled. */

/* $ Files */

/*     A text or binary PCK containing orientation data for the */
/*     body designated by BODY must be loaded at the time this */
/*     routine is called. */

/*     Normally PCK files are loaded during program initialization; */
/*     they need not be re-loaded prior to each call to this routine. */

/* $ Particulars */

/*     Applications that need to compute the transformation between */
/*     body-fixed and inertial frames usually can call the higher-level */
/*     routine PXFORM instead of this routine. */


/*     If there exists high-precision binary PCK kernel information for */
/*     the body at the requested time, the angles, W, DELTA and PHI are */
/*     computed directly from that file. These angles are then used to */
/*     compute RA, DEC and W. The most recently loaded binary PCK file */
/*     has first priority followed by previously loaded binary PCK files */
/*     in backward time order. If no binary PCK file has been loaded, */
/*     the text P_constants kernel file (PCK) is used. */

/*     If there is only text PCK kernel information, it is expressed in */
/*     terms of RA, DEC and W (same W as above), where */

/*        RA    = PHI - HALFPI() */
/*        DEC   = HALFPI() - DELTA */

/*     RA, DEC, and W are defined as follows in the text PCK file: */

/*        RA  = RA0  + RA1*T  + RA2*T*T   + a  sin theta */
/*                                           i          i */

/*        DEC = DEC0 + DEC1*T + DEC2*T*T  + d  cos theta */
/*                                           i          i */

/*        W   = W0   + W1*d   + W2*d*d    + w  sin theta */
/*                                           i          i */

/*     where: */

/*        d = days past J2000. */

/*        T = Julian centuries past J2000. */

/*        a , d , and w  arrays apply to satellites only. */
/*         i   i       i */

/*        theta  = THETA0 * THETA1*T are specific to each planet. */
/*             i */

/*       These angles -- typically nodal rates -- vary in number and */
/*       definition from one planetary system to the next. */


/*     The prime meridian offset LAMBDA */
/*     ================================ */

/*     The offset LAMBDA is the value specified by the kernel variable */

/*        BODYnnn_LONG_AXIS */

/*     if such a variable is defined. */

/*     The offset LAMBDA is a constant for a given body. LAMBDA serves */
/*     to distinguish between the planetocentric prime meridian, which */
/*     is provided in the PCK file, and the meridian that passes through */
/*     the +X axis of a reference frame aligned with the axes of the */
/*     body's reference ellipsoid. */

/*     However, SPICE Toolkit makes no use of LAMBDA. In order to */
/*     perform geometry computations using a reference ellipsoid not */
/*     aligned with a body's planetocentric reference frame, a */
/*     fixed-offset (aka "TK") reference frame aligned with the */
/*     ellipsoid's axes should be specified in a frames kernel. Note */
/*     that a fixed-offset frame may be rotated from the planetocentric */
/*     frame about an arbitrary axis, not just the polar axis. */

/*     See the Frames Required Reading frames.req for details on */
/*     constructing a fixed-offset frame specification. */

/* $ Examples */

/*     In the following code fragment, BODEUL is used to get the unit */
/*     vector (POLE) parallel to the north pole of a target body (BODY) */
/*     at a specific epoch (ET). */

/*        CALL BODEUL ( BODY, ET, RA, DEC, W, LAMBDA ) */
/*        CALL RADREC ( 1.D0, RA,  DEC, POLE ) */

/*     Note that the items necessary to compute the Euler angles */
/*     must have been loaded into the kernel pool (by one or more */
/*     previous calls to LDPOOL). */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */
/*     K.S. Zukor         (JPL) */

/* $ Version */

/* -    SPICELIB Version 5.0.0, 14-APR-2021 (NJB) (JDR) */

/*        The routine was updated to support user-defined maximum phase */
/*        angle degrees. The additional text kernel kernel variable name */
/*        BODYnnn_MAX_PHASE_DEGREE must be used when the phase angle */
/*        polynomials have degree higher than 1. The maximum allowed */
/*        degree is 3. */

/*        The kernel variable names */

/*           BODY#_CONSTS_REF_FRAME */
/*           BODY#_CONSTS_JED_EPOCH */

/*        are now recognized. */

/*        Error handling was upgraded to check FAILED() between kernel */
/*        data lookups and computations. */

/*        Now SAVEs all local variables. */

/*        Edited the header to comply with NAIF standard. Moved NAIF_IDS */
/*        required reading from $Literature_References to */
/*        $Required_Reading section. */

/* -    SPICELIB Version 4.2.0, 02-MAR-2016 (BVS) */

/*        BUG FIX: changed available room in the BODVCD call */
/*        fetching 'NUT_PREC_ANGLES' from MAXANG to MAXANG*2. */

/*        Fixed indention in some header sections. */

/*        Removed BODEUL: prefix from the text of the long */
/*        error for insufficient angles. */

/*     Last update was 24-APR-2014 (NJB) */

/*        Corrected the brief and detailed descriptions of W. */

/* -    SPICELIB Version 4.1.0, 24-OCT-2005 (NJB) */

/*        Calls to ZZBODVCD have been replaced with calls to */
/*        BODVCD. */

/* -    SPICELIB Version 4.0.0, 13-FEB-2004 (NJB) */

/*        Code has been updated to support satellite ID codes in the */
/*        range 10000 to 99999 and to allow nutation precession angles */
/*        to be associated with any object. */

/*        Implementation changes were made to improve robustness */
/*        of the code. */

/* -    SPICELIB Version 3.1.0, 21-MAR-1995 (KSZ) */

/*        REF frame is now passed correctly as a character string. */

/* -    SPICELIB Version 3.0.0, 10-MAR-1994 (KSZ) */

/*        Ability to get Euler angles from binary PCK file added. */
/*        This uses the new routine PCKEUL. */

/* -    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB) */

/*        Updated to handle P_constants referenced to different epochs */
/*        and inertial reference frames. */

/* -    SPICELIB Version 1.1.0, 02-NOV-1990 (NJB) */

/*        Allowed number of nutation precession angles increased to */
/*        100. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU) */

/* -& */
/* $ Index_Entries */

/*     euler angles for orientation of a body */
/*     fetch euler angles for a body */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 4.1.0, 24-OCT-2005 (NJB) */

/*        Calls to ZZBODVCD have been replaced with calls to */
/*        BODVCD. */

/* -    SPICELIB Version 4.0.0, 13-FEB-2004 (NJB) */

/*        Code has been updated to support satellite ID codes in the */
/*        range 10000 to 99999 and to allow nutation precession angles */
/*        to be associated with any object. */

/*        Calls to deprecated kernel pool access routine RTPOOL */
/*        were replaced by calls to GDPOOL. */

/*        Calls to BODVAR have been replaced with calls to */
/*        ZZBODVCD. */

/* -    SPICELIB Version 3.1.0, 21-MAR-1995 (KSZ) */

/*        REF frame is now passed correctly as a character string. */

/* -    SPICELIB Version 3.0.0, 10-MAR-1994 (KSZ) */

/*        BODEUL now uses new software to check for the */
/*        existence of binary PCK files, search the for */
/*        data corresponding to the requested body and time, */
/*        and return the appropriate Euler angles, using the */
/*        new routine PCKEUL. Otherwise the code calculates */
/*        the Euler angles from the P_constants kernel file. */

/* -    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB) */

/*        Updated to handle P_constants referenced to different epochs */
/*        and inertial reference frames. */

/*        Updated to handle P_constants referenced to different epochs */
/*        and inertial reference frames. */

/*        BODEUL now checks the kernel pool for presence of the */
/*        variables */

/*           BODY#_CONSTANTS_REF_FRAME */

/*        and */

/*           BODY#_CONSTANTS_JED_EPOCH */

/*        where # is the NAIF integer code of the barycenter of a */
/*        planetary system or of a body other than a planet or */
/*        satellite. If either or both of these variables are */
/*        present, the P_constants for BODY are presumed to be */
/*        referenced to the specified inertial frame or epoch. */
/*        If the epoch of the constants is not J2000, the input */
/*        time ET is converted to seconds past the reference epoch. */
/*        If the frame of the constants is not J2000, the Euler angles */
/*        defining the rotation from the P_constants' frame to */
/*        body-fixed coordinates are transformed so that they define */
/*        the rotation from J2000 coordinates to body-fixed */
/*        coordinates. */

/* -    SPICELIB Version 1.1.0, 02-NOV-1990  (NJB) */

/*        Allowed number of nutation precession angles increased to */
/*        100. */

/* -    Beta Version 2.0.0, 23-JUN-1989 (HAN) */

/*        Mod angles by two pi. Check to see that right ascension and */
/*        prime meridian angles are within the range 0 to two pi. */

/*        LAMBDA used to be returned in degrees. It has been corrected */
/*        to return LAMBDA in radians. */

/* -    Beta Version 1.1.0, 16-FEB-1989 (IMU) (NJB) */

/*        $Examples section completed. Declarations of unused variables */
/*        HALFPI and N removed. */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Maximum number of coefficients per phase angle polynomial. */


/*     Local variables */


/*     Saved variables */


/*     Initial values */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("BODEUL", (ftnlen)6);

/*     Get the code for the J2000 frame, if we don't have it yet. */

    if (first) {
	irfnum_("J2000", &j2code, (ftnlen)5);
	first = FALSE_;
    }

/*     Get Euler angles from high precision data file. */

    pckeul_(body, et, &found, bref, eulang, (ftnlen)32);
    if (failed_()) {
	chkout_("BODEUL", (ftnlen)6);
	return 0;
    }
    if (found) {
	phi = eulang[0];
	delta = eulang[1];
	*w = eulang[2];
	irfnum_(bref, &ref, (ftnlen)32);
	if (failed_()) {
	    chkout_("BODEUL", (ftnlen)6);
	    return 0;
	}

/*        The offset of the prime meridian is optional. */

	s_copy(item, "LONG_AXIS", (ftnlen)32, (ftnlen)9);
	if (bodfnd_(body, item, (ftnlen)32)) {
	    bodvcd_(body, item, &c__1, &nl, lambda, (ftnlen)32);
	    *lambda *= rpd_();
	    d__1 = twopi_();
	    *lambda = d_mod(lambda, &d__1);
	} else {
	    *lambda = 0.;
	}
    } else {

/*        Find the body code used to label the reference frame and epoch */
/*        specifiers for the orientation constants for BODY. */

/*        For planetary systems, the reference frame and epoch for the */
/*        orientation constants is associated with the system */
/*        barycenter, not with individual bodies in the system.  For any */
/*        other bodies, (the Sun or asteroids, for example) the body's */
/*        own code is used as the label. */

	refid = zzbodbry_(body);

/*        Look up the epoch of the constants. The epoch is specified */
/*        as a Julian ephemeris date. The epoch defaults to J2000. */

/*        Look for both forms of the JED epoch kernel variable. At */
/*        most one is allowed to be present. */

	s_copy(item, "BODY#_CONSTANTS_JED_EPOCH", (ftnlen)32, (ftnlen)25);
	repmi_(item, "#", &refid, item, (ftnlen)32, (ftnlen)1, (ftnlen)32);
	s_copy(item2, "BODY#_CONSTS_JED_EPOCH", (ftnlen)32, (ftnlen)22);
	repmi_(item2, "#", &refid, item2, (ftnlen)32, (ftnlen)1, (ftnlen)32);
	gdpool_(item, &c__1, &c__1, &dim, &conepc, &found, (ftnlen)32);
	if (! found) {
	    gdpool_(item2, &c__1, &c__1, &dim, &conepc, &found2, (ftnlen)32);
	    if (! found2) {
		conepc = j2000_();
	    }
	} else {

/*           Check for presence of both forms of the variable names. */

	    dtpool_(item2, &found2, &dim, dtype, (ftnlen)32, (ftnlen)1);
	    if (found2) {
		setmsg_("Both kernel variables # and # are present in the ke"
			"rnel pool. At most one form of the kernel variable n"
			"ame may be present.", (ftnlen)122);
		errch_("#", item, (ftnlen)1, (ftnlen)32);
		errch_("#", item2, (ftnlen)1, (ftnlen)32);
		sigerr_("SPICE(COMPETINGEPOCHSPEC)", (ftnlen)25);
		chkout_("BODEUL", (ftnlen)6);
		return 0;
	    }
	}
	if (failed_()) {
	    chkout_("BODEUL", (ftnlen)6);
	    return 0;
	}
	if (found || found2) {

/*           The reference epoch is returned as a JED.  Convert to */
/*           ephemeris seconds past J2000.  Then convert the input ET to */
/*           seconds past the reference epoch. */

	    conepc = spd_() * (conepc - j2000_());
	    epoch = *et - conepc;
	} else {
	    epoch = *et;
	}

/*        Look up the reference frame of the constants.  The reference */
/*        frame is specified by a code recognized by CHGIRF.  The */
/*        default frame is J2000, symbolized by the code J2CODE. */

	irfnum_("J2000", &j2code, (ftnlen)5);
	s_copy(item, "BODY#_CONSTANTS_REF_FRAME", (ftnlen)32, (ftnlen)25);
	repmi_(item, "#", &refid, item, (ftnlen)32, (ftnlen)1, (ftnlen)32);
	s_copy(item2, "BODY#_CONSTS_REF_FRAME", (ftnlen)32, (ftnlen)22);
	repmi_(item2, "#", &refid, item2, (ftnlen)32, (ftnlen)1, (ftnlen)32);
	repmi_(item, "#", &refid, item, (ftnlen)32, (ftnlen)1, (ftnlen)32);
	gipool_(item, &c__1, &c__1, &dim, &ref, &found, (ftnlen)32);
	if (! found) {
	    gipool_(item2, &c__1, &c__1, &dim, &ref, &found2, (ftnlen)32);
	    if (! found2) {
		ref = j2code;
	    }
	} else {

/*           Check for presence of both forms of the variable names. */

	    dtpool_(item2, &found2, &dim, dtype, (ftnlen)32, (ftnlen)1);
	    if (found2) {
		setmsg_("Both kernel variables # and # are present in the ke"
			"rnel pool. At most one form of the kernel variable n"
			"ame may be present.", (ftnlen)122);
		errch_("#", item, (ftnlen)1, (ftnlen)32);
		errch_("#", item2, (ftnlen)1, (ftnlen)32);
		sigerr_("SPICE(COMPETINGFRAMESPEC)", (ftnlen)25);
		chkout_("BODEUL", (ftnlen)6);
		return 0;
	    }
	}

/*        Whatever the body, it has quadratic time polynomials for */
/*        the RA and Dec of the pole, and for the rotation of the */
/*        Prime Meridian. */

	s_copy(item, "POLE_RA", (ftnlen)32, (ftnlen)7);
	cleard_(&c__3, rcoef);
	bodvcd_(body, item, &c__3, &na, rcoef, (ftnlen)32);
	s_copy(item, "POLE_DEC", (ftnlen)32, (ftnlen)8);
	cleard_(&c__3, dcoef);
	bodvcd_(body, item, &c__3, &nd, dcoef, (ftnlen)32);
	s_copy(item, "PM", (ftnlen)32, (ftnlen)2);
	cleard_(&c__3, wcoef);
	bodvcd_(body, item, &c__3, &nw, wcoef, (ftnlen)32);

/*        The offset of the prime meridian is optional. */

	s_copy(item, "LONG_AXIS", (ftnlen)32, (ftnlen)9);
	if (bodfnd_(body, item, (ftnlen)32)) {
	    bodvcd_(body, item, &c__1, &nl, lambda, (ftnlen)32);
	} else {
	    *lambda = 0.;
	}
	if (failed_()) {
	    chkout_("BODEUL", (ftnlen)6);
	    return 0;
	}

/*        There may be additional nutation and libration (THETA) terms. */

	ntheta = 0;
	na = 0;
	nd = 0;
	nw = 0;
	s_copy(item, "NUT_PREC_ANGLES", (ftnlen)32, (ftnlen)15);
	if (bodfnd_(&refid, item, (ftnlen)32)) {

/*           Find out whether the maximum phase angle degree has been */
/*           explicitly set. */

	    s_copy(item2, "MAX_PHASE_DEGREE", (ftnlen)32, (ftnlen)16);
	    if (bodfnd_(&refid, item2, (ftnlen)32)) {
		bodvcd_(&refid, item2, &c__1, &dim, &dpval, (ftnlen)32);
		deg = i_dnnt(&dpval);
		if (deg < 1 || deg > 3) {
		    setmsg_("Maximum phase angle degree for body # must be i"
			    "n the range 1:# but was #.", (ftnlen)73);
		    errint_("#", &refid, (ftnlen)1);
		    errint_("#", &c__3, (ftnlen)1);
		    errint_("#", &deg, (ftnlen)1);
		    sigerr_("SPICE(DEGREEOUTOFRANGE)", (ftnlen)23);
		    chkout_("BODEUL", (ftnlen)6);
		    return 0;
		}
		nphsco = deg + 1;
	    } else {

/*              The default degree is 1, yielding two coefficients. */

		nphsco = 2;
	    }

/*           There is something a bit obscure going on below. BODVCD */
/*           loads the array TCOEF in the following order */

/*              TCOEF(1,1), TCOEF(2,1), ... TCOEF(NPHSCO), */
/*              TCOEF(1,2), TCOEF(2,2), ... */

/*           The NTHETA that comes back is the total number of items */
/*           loaded, but we will need the actual limit on the second */
/*           dimension. That is --- NTHETA / NPHSCO. */

	    bodvcd_(&refid, item, &c__800, &ntheta, tcoef, (ftnlen)32);
	    if (failed_()) {
		chkout_("BODEUL", (ftnlen)6);
		return 0;
	    }

/*           NPHSCO is at least 1; this division is safe. */

	    nphase = ntheta / nphsco;
	}
	s_copy(item, "NUT_PREC_RA", (ftnlen)32, (ftnlen)11);
	if (bodfnd_(body, item, (ftnlen)32)) {
	    bodvcd_(body, item, &c__200, &na, ac, (ftnlen)32);
	}
	s_copy(item, "NUT_PREC_DEC", (ftnlen)32, (ftnlen)12);
	if (bodfnd_(body, item, (ftnlen)32)) {
	    bodvcd_(body, item, &c__200, &nd, dc, (ftnlen)32);
	}
	s_copy(item, "NUT_PREC_PM", (ftnlen)32, (ftnlen)11);
	if (bodfnd_(body, item, (ftnlen)32)) {
	    bodvcd_(body, item, &c__200, &nw, wc, (ftnlen)32);
	}
	if (failed_()) {
	    chkout_("BODEUL", (ftnlen)6);
	    return 0;
	}
/* Computing MAX */
	i__1 = max(na,nd);
	if (max(i__1,nw) > ntheta) {
	    setmsg_("Insufficient number of nutation/precession angles for b"
		    "ody * at time #.", (ftnlen)71);
	    errint_("*", body, (ftnlen)1);
	    errdp_("#", et, (ftnlen)1);
	    sigerr_("SPICE(INSUFFICIENTANGLES)", (ftnlen)25);
	    chkout_("BODEUL", (ftnlen)6);
	    return 0;
	}

/*        Evaluate the time polynomials at EPOCH. */

	d__ = epoch / spd_();
	t = d__ / 36525.;
	*ra = rcoef[0] + t * (rcoef[1] + t * rcoef[2]);
	*dec = dcoef[0] + t * (dcoef[1] + t * dcoef[2]);
	*w = wcoef[0] + d__ * (wcoef[1] + d__ * wcoef[2]);

/*        Add nutation and libration as appropriate. */

	i__1 = nphase;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    if (nphsco == 2) {

/*              This case is so common that we'll deal with it */
/*              separately. We'll avoid unnecessary arithmetic */
/*              operations. */

		k = (i__ - 1 << 1) + 1;
		m = k + 1;
		theta = (tcoef[(i__2 = k - 1) < 800 && 0 <= i__2 ? i__2 : 
			s_rnge("tcoef", i__2, "bodeul_", (ftnlen)868)] + t * 
			tcoef[(i__3 = m - 1) < 800 && 0 <= i__3 ? i__3 : 
			s_rnge("tcoef", i__3, "bodeul_", (ftnlen)868)]) * 
			rpd_();
	    } else {

/*              THETA and DTHETA have higher-order terms; there are */
/*              NPHSCO coefficients for each angle. */

		theta = 0.;
		i__2 = nphsco;
		for (j = 1; j <= i__2; ++j) {

/*                 K is the start index for the coefficients of the */
/*                 Ith angle. */

		    k = j + nphsco * (i__ - 1);
		    i__4 = j - 1;
		    theta += pow_di(&t, &i__4) * tcoef[(i__3 = k - 1) < 800 &&
			     0 <= i__3 ? i__3 : s_rnge("tcoef", i__3, "bodeu"
			    "l_", (ftnlen)884)];
		}
		theta *= rpd_();
	    }
	    sinth[(i__2 = i__ - 1) < 200 && 0 <= i__2 ? i__2 : s_rnge("sinth",
		     i__2, "bodeul_", (ftnlen)892)] = sin(theta);
	    costh[(i__2 = i__ - 1) < 200 && 0 <= i__2 ? i__2 : s_rnge("costh",
		     i__2, "bodeul_", (ftnlen)893)] = cos(theta);
	}

/*        Adjust RA, DEC, W by their librations and nutations. */

	*ra += vdotg_(ac, sinth, &na);
	*dec += vdotg_(dc, costh, &nd);
	*w += vdotg_(wc, sinth, &nw);

/*        Convert from degrees to radians and mod by two pi. */

	*ra *= rpd_();
	*dec *= rpd_();
	*w *= rpd_();
	*lambda *= rpd_();
	d__1 = twopi_();
	*ra = d_mod(ra, &d__1);
	d__1 = twopi_();
	*dec = d_mod(dec, &d__1);
	d__1 = twopi_();
	*w = d_mod(w, &d__1);
	d__1 = twopi_();
	*lambda = d_mod(lambda, &d__1);

/*        Convert to Euler angles. */

	phi = *ra + halfpi_();
	delta = halfpi_() - *dec;
    }

/*     Convert the angles to the J2000 frame if they are not already */
/*     referenced to J2000. */

    if (ref != j2code) {

/*        Find the transformation from the J2000 frame to the frame */
/*        designated by REF.  Form the transformation from `REF' */
/*        coordinates to body-fixed coordinates, using our Euler angles. */
/*        Compose the transformations to obtain the J2000-to-body-fixed */
/*        transformation.  Decompose this transformation into Euler */
/*        angles. */

	irfrot_(&j2code, &ref, j2ref);
	eul2m_(w, &delta, &phi, &c__3, &c__1, &c__3, rf2bfx);
	mxm_(rf2bfx, j2ref, j2bfx);
	m2eul_(j2bfx, &c__3, &c__1, &c__3, w, &delta, &phi);
	if (failed_()) {
	    chkout_("BODEUL", (ftnlen)6);
	    return 0;
	}
    }

/*     The Euler angles now give the transformation from J2000 to */
/*     body-fixed coordinates at epoch ET seconds past J2000, */
/*     regardless of the epoch and frame of the orientation constants */
/*     for the specified body. */

    *ra = phi - halfpi_();
    *dec = halfpi_() - delta;

/*     Make sure that the prime meridian and right ascension are in */
/*     the correct range. */

    if (*w < 0.) {
	*w += twopi_();
    }
    if (*ra < 0.) {
	*ra += twopi_();
    }
    chkout_("BODEUL", (ftnlen)6);
    return 0;
} /* bodeul_ */

