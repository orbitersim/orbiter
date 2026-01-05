/* zzgfcprx.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__3 = 3;
static doublereal c_b15 = 1.;
static doublereal c_b39 = 0.;

/* $Procedure ZZGFCPRX ( GF, coordinate derivative proxy ) */
/* Subroutine */ int zzgfcprx_(doublereal *state, char *corsys, doublereal *
	re, doublereal *f, integer *sense, integer *cdsign, ftnlen corsys_len)
{
    /* System generated locals */
    integer i__1, i__2;
    doublereal d__1;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen), s_rnge(char *, integer, 
	    char *, integer);
    double d_sign(doublereal *, doublereal *);
    integer i_dnnt(doublereal *);

    /* Local variables */
    extern /* Subroutine */ int vhat_(doublereal *, doublereal *);
    doublereal xmat[9]	/* was [3][3] */;
    extern doublereal vdot_(doublereal *, doublereal *);
    extern /* Subroutine */ int zzrtnmat_(doublereal *, doublereal *);
    integer i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen), vpack_(doublereal *, doublereal *, doublereal *,
	     doublereal *);
    extern logical vzero_(doublereal *), failed_(void);
    doublereal dp;
    extern /* Subroutine */ int cleari_(integer *, integer *), recgeo_(
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *), latrec_(doublereal *, doublereal *, 
	    doublereal *, doublereal *);
    integer dpsign;
    doublereal normal[3];
    extern /* Subroutine */ int sigerr_(char *, ftnlen), vhatip_(doublereal *)
	    , chkout_(char *, ftnlen), setmsg_(char *, ftnlen), errint_(char *
	    , integer *, ftnlen);
    doublereal rtnvel[3];
    integer rtnsgn[3];
    extern logical return_(void);
    doublereal alt, lat, vel[3], lon;
    extern /* Subroutine */ int mxv_(doublereal *, doublereal *, doublereal *)
	    ;

/* $ Abstract */

/*     SPICE private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     Return the signs of a Cartesian velocity vector's coordinates */
/*     when the velocity is transformed to a given coordinate system. */

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

/*     COORDINATE */
/*     GEOMETRY */
/*     UTILITY */

/* $ Declarations */
/* $ Abstract */

/*     SPICE private include file intended solely for the support of */
/*     SPICE routines. Users should not include this routine in their */
/*     source code due to the volatile nature of this file. */

/*     This file contains private, global parameter declarations */
/*     for the SPICELIB Geometry Finder (GF) subsystem. */

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

/*     GF */

/* $ Keywords */

/*     GEOMETRY */
/*     ROOT */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman      (JPL) */
/*     E.D. Wright       (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 17-FEB-2009 (NJB) (EDW) */

/* -& */

/*     The set of supported coordinate systems */

/*        System          Coordinates */
/*        ----------      ----------- */
/*        Rectangular     X, Y, Z */
/*        Latitudinal     Radius, Longitude, Latitude */
/*        Spherical       Radius, Colatitude, Longitude */
/*        RA/Dec          Range, Right Ascension, Declination */
/*        Cylindrical     Radius, Longitude, Z */
/*        Geodetic        Longitude, Latitude, Altitude */
/*        Planetographic  Longitude, Latitude, Altitude */

/*     Below we declare parameters for naming coordinate systems. */
/*     User inputs naming coordinate systems must match these */
/*     when compared using EQSTR. That is, user inputs must */
/*     match after being left justified, converted to upper case, */
/*     and having all embedded blanks removed. */


/*     Below we declare names for coordinates. Again, user */
/*     inputs naming coordinates must match these when */
/*     compared using EQSTR. */


/*     Note that the RA parameter value below matches */

/*        'RIGHT ASCENSION' */

/*     when extra blanks are compressed out of the above value. */


/*     Parameters specifying types of vector definitions */
/*     used for GF coordinate searches: */

/*     All string parameter values are left justified, upper */
/*     case, with extra blanks compressed out. */

/*     POSDEF indicates the vector is defined by the */
/*     position of a target relative to an observer. */


/*     SOBDEF indicates the vector points from the center */
/*     of a target body to the sub-observer point on */
/*     that body, for a given observer and target. */


/*     SOBDEF indicates the vector points from the center */
/*     of a target body to the surface intercept point on */
/*     that body, for a given observer, ray, and target. */


/*     Number of workspace windows used by ZZGFREL: */


/*     Number of additional workspace windows used by ZZGFLONG: */


/*     Index of "existence window" used by ZZGFCSLV: */


/*     Progress report parameters: */

/*     MXBEGM, */
/*     MXENDM    are, respectively, the maximum lengths of the progress */
/*               report message prefix and suffix. */

/*     Note: the sum of these lengths, plus the length of the */
/*     "percent complete" substring, should not be long enough */
/*     to cause wrap-around on any platform's terminal window. */


/*     Total progress report message length upper bound: */


/*     End of file zzgf.inc. */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     STATE      I   A 6-dimensional Cartesian state vector. */
/*     CORSYS     I   A coordinate system name parameter. */
/*     RE         I   Ellipsoid equatorial radius. */
/*     F          I   Ellipsoid flattening coefficient. */
/*     SENSE      I   Reference body longitude sense. */
/*     CDSIGN     O   Velocity sign vector. */

/* $ Detailed_Input */

/*     STATE          is any Cartesian state vector. The order of the */
/*                    components matches those used by the SPK system. */

/*     CORSYS         is a character string parameter identifying a */
/*                    coordinate system. The recognized values of CORSYS */
/*                    are declared in the INCLUDE file */

/*                       zzgf.inc */

/*     RE             Equatorial radius of a reference spheroid.  This */
/*                    spheroid is a volume of revolution:  its */
/*                    horizontal cross sections are circular.  The shape */
/*                    of the spheroid is defined by an equatorial radius */
/*                    RE and a polar radius RP. */

/*     F              Flattening coefficient = (RE-RP) / RE,  where RP */
/*                    is the polar radius of the spheroid. */

/*     SENSE          is an integer indicating the sense of longitude */
/*                    for planetographic coordinate systems. A value of */
/*                    +1 indicates positive East; a value of -1 indicates */
/*                    positive West. */

/* $ Detailed_Output */

/*     CDSIGN         is an array of three integers indicating signs of */
/*                    the derivatives with respect to time of each */
/*                    coordinate, where the coordinates are determined */
/*                    by the input state and coordinate system. The */
/*                    elements of CDSIGN are -1, 0, or 1: these indicate */
/*                    negative, zero, or positive derivatives, */
/*                    respectively. The relationship between elements of */
/*                    CDSIGN and coordinates is given by the coordinate */
/*                    orders used in the RECxxx coordinate conversion */
/*                    routines. Those orders are shown in the table */
/*                    below. */

/*                    System          Coordinates */
/*                    ----------      ----------- */
/*                    Rectangular     X, Y, Z */
/*                    Latitudinal     Radius, Longitude, Latitude */
/*                    Spherical       Radius, Colatitude, Longitude */
/*                    RA/Dec          Range, Right Ascension, Declination */
/*                    Cylindrical     Radius, Longitude, Z */
/*                    Geodetic        Longitude, Latitude, Altitude */
/*                    Planetographic  Longitude, Latitude, Altitude */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If the input argument SENSE has a value other than -1 or 1, */
/*        and the coordinate system is planetographic, the error */
/*        SPICE(VALUEOUTOFRANGE) is signaled. For other coordinate */
/*        systems, this argument is ignored. */

/*     2) If the input coordinate system specifier is not recognized, */
/*        the error SPICE(NOTSUPPORTED) is signaled. */

/*     3) If the coordinate system is geodetic or planetographic, */
/*        invalid ellipsoid shape parameters will be diagnosed by */
/*        routines in the call tree of this routine. For other */
/*        coordinate systems, these arguments are ignored. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     In order to conduct searches involving constraints on the */
/*     coordinates of a position vector, the GF system requires the */
/*     signs of the derivatives with respect to time of the coordinates */
/*     referenced in the constraints. The most direct way to obtain */
/*     these signs is to convert the Cartesian velocity to the */
/*     coordinate system of interest using the SPICE Jacobian matrix */
/*     routines; however, that technique has the drawback of being */
/*     unusable at or near singularities of the mapping from rectangular */
/*     coordinates to any non-rectangular coordinate system. */

/*     This routine avoids problems with singularities by determining */
/*     signs of coordinate derivatives without computing the */
/*     (problematic) derivatives themselves. Instead this routine uses */
/*     proxy functions that have the same signs, for a given set of */
/*     inputs, as the coordinate derivatives of interest, where those */
/*     derivatives are defined. In addition, this routine returns */
/*     derivative signs for any position, including those where Jacobian */
/*     matrices are undefined. This allows the GF system to handle cases */
/*     where the time derivative of one coordinate is defined but */
/*     unavailable from the Jacobian matrix routines because another */
/*     coordinate is undefined or not differentiable at the same */
/*     position. */

/*     Below, we discuss the proxy functions used by this routine. */


/*     Non-singular case */
/*     ================= */

/*     For positions off the Z-axis, all of the rectangular-to-alternate */
/*     coordinate transformation Jacobian matrices are defined in */
/*     principle. These matrices may not be computable in practice */
/*     because the derivative with respect to time of longitude can */
/*     overflow. */

/*     Our solution is to transform the input Cartesian velocity to a */
/*     "modified radial, tangential, normal" (MRTN) reference */
/*     frame: the basis vectors of this frame point "up", "East," and */
/*     "North." For geodetic and planetographic coordinate systems, the */
/*     "up" direction points along the outward normal of the reference */
/*     ellipsoid defined by the input parameters RE and F; in other */
/*     words, "up" is the direction of increasing altitude. For */
/*     cylindrical coordinates, "up" is the radial direction and "North" */
/*     is the +Z direction. */

/*     For the other latitudinal systems, the "up" direction points in */
/*     the direction of increasing radius; the up direction is parallel */
/*     to the position component of the input state. */

/*     The basis vectors of the MRTN frame lose precision for positions */
/*     very close to the Z-axis, but there are no problems with division */
/*     by zero or arithmetic overflow. */

/*     The MRTN frame velocity indicates the signs of the coordinate */
/*     derivatives as follows: */

/*        - Longitude: the sign of the rate of change of positive East */
/*          longitude is equal to the sign of the East component of */
/*          the MRTN velocity. */

/*          For planetographic coordinate systems, the sign is adjusted */
/*          as needed to account for the sense of positive longitude. */
/*          The caller passes in a "longitude sense" indicator, allowing */
/*          the GF system to determine this sense once per search at */
/*          search initialization time. */

/*        - Latitude: the sign of the rate of change of planetocentric */
/*          latitude is equal to the sign of the North component of */
/*          the MRTN velocity. */

/*        - Co-latitude:  the sign of the rate of change of */
/*          planetocentric latitude is equal to the negative of the sign */
/*          of the North component of the MRTN velocity. */

/*        - Radius or altitude: the sign of the rate of change of */
/*          these coordinates is equal to sign of the up component of */
/*          the MRTN velocity. */


/*     Singular cases */
/*     ============== */

/*     When the position lies on the Z-axis, some or all of the */
/*     derivatives of the coordinates with respect to Cartesian */
/*     coordinates may not exist. This routine assigns all such */
/*     derivatives a sign of zero. Other derivatives, such as */
/*     those of radius or altitude, may exist. */

/*     Below we summarize the treatment of the singular cases. */
/*     We assume the input velocity is non-zero, and we omit */
/*     the case of rectangular coordinates. */

/*        Coordinate Derivative                Sign */
/*        ---------------------                ---- */
/*        Longitude (all systems)              0 */
/*        Right ascension                      0 */
/*        Latitude  (all systems)              0 */
/*        Declination                          0 */
/*        Co-latitude                          0 */

/*        Non-cylindrical radius, altitude  {  0 if position is at */
/*                                               origin */

/*                                             1 if dot product of */
/*                                               velocity and position */
/*                                               is positive */

/*                                            -1 if dot product of */
/*                                               velocity and position */
/*                                               is negative            } */

/*        Cylindrical radius                   0 */

/*        Z                                 {  1 if velocity Z-component */
/*                                               is positive */

/*                                             0 if velocity Z-component */
/*                                               is zero */

/*                                            -1 if velocity Z-component */
/*                                               is negative            } */


/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     [1] ANSI Fortran 77 Standard, p. 15-23. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 15-APR-2014 (NJB) */

/*        Added FAILED() check to avoid numeric problems. */

/* -    SPICELIB Version 1.0.0, 15-APR-2009 (NJB) */

/* -& */
/* $ Index_Entries */

/*     coordinate derivative proxy */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */

/*     Internally, we're going to use the more */
/*     descriptive names EAST for the "tangential" */
/*     direction and NORTH for the "normal" direction. */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("ZZGFCPRX", (ftnlen)8);

/*     For planetographic coordinates, check the longitude sense. */

    if (s_cmp(corsys, "PLANETOGRAPHIC", corsys_len, (ftnlen)14) == 0) {
	if (*sense != 1 && *sense != -1) {
	    setmsg_("Longitude sense # should be 1 or -1.", (ftnlen)36);
	    errint_("#", sense, (ftnlen)1);
	    sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	    chkout_("ZZGFCPRX", (ftnlen)8);
	    return 0;
	}
    }

/*     If we have a zero velocity vector, just indicate that each */
/*     velocity coordinate isn't changing and return now. If the */
/*     velocity vector is non-zero, convert it to a unit vector; this */
/*     guarantees that overflow can't occur. */
    if (vzero_(&state[3])) {

/*        The velocity is zero. Indicate that the coordinates are */
/*        not changing and return. Returning now simplifies the */
/*        logic of the rest of the routine, since the case of */
/*        zero-velocity can be ignored. */

	cleari_(&c__3, cdsign);
	chkout_("ZZGFCPRX", (ftnlen)8);
	return 0;
    } else {
	vhat_(&state[3], vel);
    }

/*     The rectangular case is trivial; handle it now. */

    if (s_cmp(corsys, "RECTANGULAR", corsys_len, (ftnlen)11) == 0) {

/*        The output system is rectangular. Just indicate the */
/*        signs of the input velocity. */

	for (i__ = 1; i__ <= 3; ++i__) {
	    if (vel[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("vel", 
		    i__1, "zzgfcprx_", (ftnlen)403)] == 0.) {
		cdsign[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge(
			"cdsign", i__1, "zzgfcprx_", (ftnlen)405)] = 0;
	    } else {

/*              Use the Fortran sign transfer intrinsic function */
/*              to set CDSIGN(I) to 1 or -1, depending */
/*              on whether the corresponding velocity component */
/*              is positive or negative. See reference [1] for a */
/*              discussion of this Fortran intrinsic function. */

		d__1 = d_sign(&c_b15, &vel[(i__2 = i__ - 1) < 3 && 0 <= i__2 ?
			 i__2 : s_rnge("vel", i__2, "zzgfcprx_", (ftnlen)415)]
			);
		cdsign[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge(
			"cdsign", i__1, "zzgfcprx_", (ftnlen)415)] = i_dnnt(&
			d__1);
	    }
	}

/*        All done. */

	chkout_("ZZGFCPRX", (ftnlen)8);
	return 0;
    }

/*     There's quite a bit of common logic for the "on Z-axis" case; */
/*     take care of it here. */

    if (state[0] == 0. && state[1] == 0.) {

/*        The position lies on the Z-axis. */

/*        For all of the coordinate systems having a longitude */
/*        coordinate (this includes right ascension), the derivative of */
/*        longitude with respect to time is undefined; we set the sign */
/*        of the derivative to zero. */

/*        For all of the coordinate systems having a latitude coordinate */
/*        (this includes declination), if the position is not at the */
/*        origin, the derivative of latitude with respect to time is */
/*        undefined unless the input velocity is zero. At the origin, */
/*        the derivative of latitude with respect to time doesn't exist. */
/*        In both cases, we set the sign of the velocity components */
/*        to zero. */

/*        For the coordinate systems that have a radius or range */
/*        coordinate, where distance is measured from the origin, when */
/*        the input position is not at the origin, distance is */
/*        increasing, constant, or decreasing depending on whether the */
/*        dot product of velocity and the position's Z-coordinate is */
/*        positive, zero, or negative, respectively. This dot product */
/*        test is valid for the derivative of altitude as well (we */
/*        assert this without proof for the case of positions inside */
/*        prolate spheroids). */

/*        If the position is at the origin, then since range and */
/*        altitude are not differentiable, their signs are set to */
/*        zero. */

/*        Cylindrical coordinates are a special case which we treat */
/*        separately. */

	if (state[2] != 0.) {

/*           The position is on the Z-axis but not at the origin. */

/*           Compute the dot product used for the range/altitude */
/*           derivative. */

	    dp = vdot_(state, vel);
	    if (dp == 0.) {
		dpsign = 0;
	    } else {
		d__1 = d_sign(&c_b15, &dp);
		dpsign = i_dnnt(&d__1);
	    }
	} else {

/*           The position is at the origin. We know the velocity */
/*           is non-zero, and any movement increases radius or */
/*           altitude. However, neither radius nor altitude are */
/*           differentiable here, so we indicate no sign. */

	    dpsign = 0;
	}

/*        Set the coordinate derivative signs for all but the */
/*        rectangular system, which was handled already, and */
/*        the cylindrical system. */


/*        Recall the coordinate systems and their coordinate orders: */

/*        System          Coordinates */
/*        ----------      ----------- */
/*        Rectangular     X, Y, Z */
/*        Latitudinal     Radius, Longitude, Latitude */
/*        Spherical       Radius, Colatitude, Longitude */
/*        RA/Dec          Range, Right Ascension, Declination */
/*        Cylindrical     Radius, Longitude, Z */
/*        Geodetic        Longitude, Latitude, Altitude */
/*        Planetographic  Longitude, Latitude, Altitude */


	if (s_cmp(corsys, "LATITUDINAL", corsys_len, (ftnlen)11) == 0) {

/*           The radial derivative sign was computed; the */
/*           other derivative signs are set to zero. */

	    cdsign[0] = dpsign;
	    cdsign[1] = 0;
	    cdsign[2] = 0;
	} else if (s_cmp(corsys, "SPHERICAL", corsys_len, (ftnlen)9) == 0) {

/*           The radial derivative sign was computed; the */
/*           longitude derivative signs is set to zero. */

	    cdsign[0] = dpsign;
	    cdsign[2] = 0;

/*           Co-latitude is a special case. Co-latitude is */
/*           not differentiable with respect to Cartesian */
/*           position for positions on the Z-axis, since */
/*           co-latitude is a v-shaped function of distance */
/*           from the Z-axis. We simply set the sign */
/*           of the co-latitude derivative to zero in this */
/*           case. */

	    cdsign[1] = 0;
	} else if (s_cmp(corsys, "RA/DEC", corsys_len, (ftnlen)6) == 0) {

/*           RA/Dec derivatives are assigned in the same manner */
/*           as latitudinal ones. */

	    cdsign[0] = dpsign;
	    cdsign[1] = 0;
	    cdsign[2] = 0;
	} else if (s_cmp(corsys, "GEODETIC", corsys_len, (ftnlen)8) == 0) {

/*           Altitude plays the role of radius for this */
/*           system. */

	    cdsign[0] = 0;
	    cdsign[1] = 0;
	    cdsign[2] = dpsign;
	} else if (s_cmp(corsys, "PLANETOGRAPHIC", corsys_len, (ftnlen)14) == 
		0) {

/*           Altitude plays the role of radius for this */
/*           system. */

	    cdsign[0] = 0;
	    cdsign[1] = 0;
	    cdsign[2] = dpsign;
	} else if (s_cmp(corsys, "CYLINDRICAL", corsys_len, (ftnlen)11) == 0) 
		{
	    cdsign[0] = 0;
	    cdsign[1] = 0;

/*           For cylindrical coordinates, the derivative of Z with */
/*           respect to time is already present in VEL. */

	    if (vel[2] == 0.) {
		cdsign[2] = 0;
	    } else {
		d__1 = d_sign(&c_b15, &vel[2]);
		cdsign[2] = i_dnnt(&d__1);
	    }
	} else {

/*           If we end up here, we have an invalid coordinate system. */

	    setmsg_("Coordinate system # is not supported. Verify that the c"
		    "oordinate system specifier matches a value from zzgf.inc."
		    , (ftnlen)112);
	    errch_("#", corsys, (ftnlen)1, corsys_len);
	    sigerr_("SPICE(NOTSUPPORTED)", (ftnlen)19);
	    chkout_("ZZGFCPRX", (ftnlen)8);
	    return 0;
	}

/*        We've handled the on-Z-axis cases. Return now. */

	chkout_("ZZGFCPRX", (ftnlen)8);
	return 0;
    }

/*     This is the normal case: the position is not on the Z-axis. */

/*     The type of MRTN frame we use depends on the coordinate system. */
/*     Planetodetic and planetographic coordinate systems are a special */
/*     case. */

    if (s_cmp(corsys, "GEODETIC", corsys_len, (ftnlen)8) == 0 || s_cmp(corsys,
	     "PLANETOGRAPHIC", corsys_len, (ftnlen)14) == 0) {

/*        Instead of defining the MRTN frame using the input */
/*        position vector, we define it using an outward normal vector */
/*        on the reference ellipsoid at the geodetic latitude */
/*        and longitude of the input position. */

	recgeo_(state, re, f, &lon, &lat, &alt);
	if (failed_()) {
	    chkout_("ZZGFCPRX", (ftnlen)8);
	    return 0;
	}
	latrec_(&c_b15, &lon, &lat, normal);
    } else if (s_cmp(corsys, "CYLINDRICAL", corsys_len, (ftnlen)11) == 0) {

/*        The normal vector is aligned with the local radial */
/*        direction; this vector is parallel to the X-Y plane. */

	vpack_(state, &state[1], &c_b39, normal);
	vhatip_(normal);
    } else {

/*        The position vector provides the normal direction. */

	vhat_(state, normal);
    }
/*     Obtain the matrix required to transform the velocity to the MRTN */
/*     frame; transform the velocity. */

    zzrtnmat_(normal, xmat);
    mxv_(xmat, vel, rtnvel);

/*     We can think of the basis vectors of the MRTN frame as local "up", */
/*     "East," "North" directions. Compute the signs of the up, East, */
/*     and North velocity components. */

    for (i__ = 1; i__ <= 3; ++i__) {
	if (rtnvel[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("rtnvel",
		 i__1, "zzgfcprx_", (ftnlen)659)] == 0.) {
	    rtnsgn[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("rtnsgn",
		     i__1, "zzgfcprx_", (ftnlen)661)] = 0;
	} else {
	    d__1 = d_sign(&c_b15, &rtnvel[(i__2 = i__ - 1) < 3 && 0 <= i__2 ? 
		    i__2 : s_rnge("rtnvel", i__2, "zzgfcprx_", (ftnlen)663)]);
	    rtnsgn[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("rtnsgn",
		     i__1, "zzgfcprx_", (ftnlen)663)] = i_dnnt(&d__1);
	}
    }

/*     Set the signs of the coordinate derivatives from the MRTN */
/*     derivative signs. */


/*        Recall the coordinate systems and their coordinate orders: */

/*        System          Coordinates */
/*        ----------      ----------- */
/*        Rectangular     X, Y, Z */
/*        Latitudinal     Radius, Longitude, Latitude */
/*        Spherical       Radius, Colatitude, Longitude */
/*        RA/Dec          Range, Right Ascension, Declination */
/*        Cylindrical     Radius, Longitude, Z */
/*        Geodetic        Longitude, Latitude, Altitude */
/*        Planetographic  Longitude, Latitude, Altitude */


    if (s_cmp(corsys, "LATITUDINAL", corsys_len, (ftnlen)11) == 0) {
	cdsign[0] = rtnsgn[0];
	cdsign[1] = rtnsgn[1];
	cdsign[2] = rtnsgn[2];
    } else if (s_cmp(corsys, "SPHERICAL", corsys_len, (ftnlen)9) == 0) {

/*        For spherical coordinate systems, the sign of the */
/*        derivative of co-latitude is the negative of the */
/*        sign of the North derivative. */

	cdsign[0] = rtnsgn[0];
	cdsign[1] = -rtnsgn[2];
	cdsign[2] = rtnsgn[1];
    } else if (s_cmp(corsys, "RA/DEC", corsys_len, (ftnlen)6) == 0) {
	cdsign[0] = rtnsgn[0];
	cdsign[1] = rtnsgn[1];
	cdsign[2] = rtnsgn[2];
    } else if (s_cmp(corsys, "GEODETIC", corsys_len, (ftnlen)8) == 0) {
	cdsign[0] = rtnsgn[1];
	cdsign[1] = rtnsgn[2];
	cdsign[2] = rtnsgn[0];
    } else if (s_cmp(corsys, "PLANETOGRAPHIC", corsys_len, (ftnlen)14) == 0) {

/*        For planetographic coordinates, altitude and latitude */
/*        behave identically to their geodetic counterparts. We */
/*        need to adjust the sign of the longitude derivative */
/*        according to whether longitude is positive East or West. */

	cdsign[0] = rtnsgn[1] * *sense;
	cdsign[1] = rtnsgn[2];
	cdsign[2] = rtnsgn[0];
    } else if (s_cmp(corsys, "CYLINDRICAL", corsys_len, (ftnlen)11) == 0) {
	cdsign[0] = rtnsgn[0];
	cdsign[1] = rtnsgn[1];
	cdsign[2] = rtnsgn[2];
    } else {

/*        If we end up here, we have an invalid coordinate system. */

	setmsg_("Coordinate system # is not supported. Verify that the coord"
		"inate system specifier matches a value from zzgf.inc.", (
		ftnlen)112);
	errch_("#", corsys, (ftnlen)1, corsys_len);
	sigerr_("SPICE(NOTSUPPORTED)", (ftnlen)19);
	chkout_("ZZGFCPRX", (ftnlen)8);
	return 0;
    }
    chkout_("ZZGFCPRX", (ftnlen)8);
    return 0;
} /* zzgfcprx_ */

