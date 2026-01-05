/* pgrrec.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__0 = 0;

/* $Procedure PGRREC ( Planetographic to rectangular ) */
/* Subroutine */ int pgrrec_(char *body, doublereal *lon, doublereal *lat, 
	doublereal *alt, doublereal *re, doublereal *f, doublereal *rectan, 
	ftnlen body_len)
{
    /* Initialized data */

    static logical first = TRUE_;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int zzbods2c_(integer *, char *, integer *, 
	    logical *, char *, integer *, logical *, ftnlen, ftnlen), 
	    zzctruin_(integer *);
    integer n;
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen);
    logical found;
    extern /* Subroutine */ int errdp_(char *, doublereal *, ftnlen);
    integer sense;
    extern /* Subroutine */ int repmi_(char *, char *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen);
    static logical svfnd1;
    static integer svctr1[2];
    extern /* Subroutine */ int georec_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *);
    integer bodyid;
    static integer svbdid;
    doublereal geolon;
    extern /* Subroutine */ int gcpool_(char *, integer *, integer *, integer 
	    *, char *, logical *, ftnlen, ftnlen);
    char kvalue[80];
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    char pmkvar[32], pgrlon[4];
    extern /* Subroutine */ int setmsg_(char *, ftnlen);
    static char svbody[36];
    extern /* Subroutine */ int ljucrs_(integer *, char *, char *, ftnlen, 
	    ftnlen);
    extern integer plnsns_(integer *);
    extern logical return_(void);

/* $ Abstract */

/*     Convert planetographic coordinates to rectangular coordinates. */

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

/*     KERNEL */
/*     NAIF_IDS */
/*     PCK */

/* $ Keywords */

/*     CONVERSION */
/*     COORDINATES */
/*     GEOMETRY */
/*     MATH */

/* $ Declarations */
/* $ Abstract */

/*     This include file defines the dimension of the counter */
/*     array used by various SPICE subsystems to uniquely identify */
/*     changes in their states. */

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

/* $ Parameters */

/*     CTRSIZ      is the dimension of the counter array used by */
/*                 various SPICE subsystems to uniquely identify */
/*                 changes in their states. */

/* $ Author_and_Institution */

/*     B.V. Semenov    (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 29-JUL-2013 (BVS) */

/* -& */

/*     End of include file. */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     BODY       I   Body with which coordinate system is associated. */
/*     LON        I   Planetographic longitude of a point (radians). */
/*     LAT        I   Planetographic latitude of a point (radians). */
/*     ALT        I   Altitude of a point above reference spheroid. */
/*     RE         I   Equatorial radius of the reference spheroid. */
/*     F          I   Flattening coefficient. */
/*     RECTAN     O   Rectangular coordinates of the point. */

/* $ Detailed_Input */

/*     BODY     is the name of the body with which the planetographic */
/*              coordinate system is associated. */

/*              BODY is used by this routine to look up from the */
/*              kernel pool the prime meridian rate coefficient giving */
/*              the body's spin sense. See the $Files and $Particulars */
/*              header sections below for details. */

/*     LON      is the planetographic longitude of the input point. */
/*              This is the angle between the prime meridian and the */
/*              meridian containing the input point. For bodies */
/*              having prograde (aka direct) rotation, the direction */
/*              of increasing longitude is positive west: from the +X */
/*              axis of the rectangular coordinate system toward the */
/*              -Y axis. For bodies having retrograde rotation, the */
/*              direction of increasing longitude is positive east: */
/*              from the +X axis toward the +Y axis. */

/*              The earth, moon, and sun are exceptions: */
/*              planetographic longitude is measured positive east for */
/*              these bodies. */

/*              The default interpretation of longitude by this */
/*              and the other planetographic coordinate conversion */
/*              routines can be overridden; see the discussion in */
/*              $Particulars below for details. */

/*              LON is measured in radians. On input, the range */
/*              of longitude is unrestricted. */

/*     LAT      is the planetographic latitude of the input point. */
/*              For a point P on the reference spheroid, this is the */
/*              angle between the XY plane and the outward normal */
/*              vector at P. For a point P not on the reference */
/*              spheroid, the planetographic latitude is that of the */
/*              closest point to P on the spheroid. */

/*              LAT is measured in radians. On input, the */
/*              range of latitude is unrestricted. */

/*     ALT      is the altitude of point above the reference spheroid. */
/*              Units of ALT must match those of RE. */

/*     RE       is the equatorial radius of a reference spheroid. */
/*              This spheroid is a volume of revolution: its */
/*              horizontal cross sections are circular. The shape of */
/*              the spheroid is defined by an equatorial radius RE and */
/*              a polar radius RP. Units of RE must match those of */
/*              ALT. */

/*     F        is the flattening coefficient of the body, a */
/*              dimensionless value defined as: */

/*                 (RE - RP) / RE */

/*              where RP is the polar radius of the spheroid, and the */
/*              units of RP match those of RE. */

/* $ Detailed_Output */

/*     RECTAN   are the rectangular coordinates of the input point. */
/*              See the discussion below in the $Particulars header */
/*              section for details. */

/*              The units associated with RECTAN are those associated */
/*              with the inputs ALT and RE. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the body name BODY cannot be mapped to a NAIF ID code, */
/*         and if BODY is not a string representation of an integer, */
/*         the error SPICE(IDCODENOTFOUND) is signaled. */

/*     2)  If the kernel variable */

/*            BODY<ID code>_PGR_POSITIVE_LON */

/*         is present in the kernel pool but has a value other */
/*         than one of */

/*             'EAST' */
/*             'WEST' */

/*         the error SPICE(INVALIDOPTION) is signaled. Case */
/*         and blanks are ignored when these values are interpreted. */

/*     3)  If polynomial coefficients for the prime meridian of BODY */
/*         are not available in the kernel pool, and if the kernel */
/*         variable BODY<ID code>_PGR_POSITIVE_LON is not present in */
/*         the kernel pool, the error SPICE(MISSINGDATA) is signaled. */

/*     4)  If the equatorial radius is non-positive, the error */
/*         SPICE(VALUEOUTOFRANGE) is signaled. */

/*     5)  If the flattening coefficient is greater than or equal to one, */
/*         the error SPICE(VALUEOUTOFRANGE) is signaled. */

/* $ Files */

/*     This routine expects a kernel variable giving BODY's prime */
/*     meridian angle as a function of time to be available in the */
/*     kernel pool. Normally this item is provided by loading a PCK */
/*     file. The required kernel variable is named */

/*        BODY<body ID>_PM */

/*     where <body ID> represents a string containing the NAIF integer */
/*     ID code for BODY. For example, if BODY is 'JUPITER', then */
/*     the name of the kernel variable containing the prime meridian */
/*     angle coefficients is */

/*        BODY599_PM */

/*     See the PCK Required Reading for details concerning the prime */
/*     meridian kernel variable. */

/*     The optional kernel variable */

/*        BODY<body ID>_PGR_POSITIVE_LON */

/*     also is normally defined via loading a text kernel. When this */
/*     variable is present in the kernel pool, the prime meridian */
/*     coefficients for BODY are not required by this routine. See the */
/*     $Particulars section below for details. */

/* $ Particulars */

/*     Given the planetographic coordinates of a point, this routine */
/*     returns the body-fixed rectangular coordinates of the point. The */
/*     body-fixed rectangular frame is that having the X-axis pass */
/*     through the 0 degree latitude 0 degree longitude direction, the */
/*     Z-axis pass through the 90 degree latitude direction, and the */
/*     Y-axis equal to the cross product of the unit Z-axis and X-axis */
/*     vectors. */

/*     The planetographic definition of latitude is identical to the */
/*     planetodetic (also called "geodetic" in SPICE documentation) */
/*     definition. In the planetographic coordinate system, latitude is */
/*     defined using a reference spheroid. The spheroid is */
/*     characterized by an equatorial radius and a polar radius. For a */
/*     point P on the spheroid, latitude is defined as the angle between */
/*     the X-Y plane and the outward surface normal at P. For a point P */
/*     off the spheroid, latitude is defined as the latitude of the */
/*     nearest point to P on the spheroid. Note if P is an interior */
/*     point, for example, if P is at the center of the spheroid, there */
/*     may not be a unique nearest point to P. */

/*     In the planetographic coordinate system, longitude is defined */
/*     using the spin sense of the body. Longitude is positive to the */
/*     west if the spin is prograde and positive to the east if the spin */
/*     is retrograde. The spin sense is given by the sign of the first */
/*     degree term of the time-dependent polynomial for the body's prime */
/*     meridian Euler angle "W":  the spin is retrograde if this term is */
/*     negative and prograde otherwise. For the sun, planets, most */
/*     natural satellites, and selected asteroids, the polynomial */
/*     expression for W may be found in a SPICE PCK kernel. */

/*     The earth, moon, and sun are exceptions: planetographic longitude */
/*     is measured positive east for these bodies. */

/*     If you wish to override the default sense of positive longitude */
/*     for a particular body, you can do so by defining the kernel */
/*     variable */

/*        BODY<body ID>_PGR_POSITIVE_LON */

/*     where <body ID> represents the NAIF ID code of the body. This */
/*     variable may be assigned either of the values */

/*        'WEST' */
/*        'EAST' */

/*     For example, you can have this routine treat the longitude */
/*     of the earth as increasing to the west using the kernel */
/*     variable assignment */

/*        BODY399_PGR_POSITIVE_LON = 'WEST' */

/*     Normally such assignments are made by placing them in a text */
/*     kernel and loading that kernel via FURNSH. */

/*     The definition of this kernel variable controls the behavior of */
/*     the SPICELIB planetographic routines */

/*        PGRREC */
/*        RECPGR */
/*        DPGRDR */
/*        DRDPGR */

/*     It does not affect the other SPICELIB coordinate conversion */
/*     routines. */

/* $ Examples */

/*     The numerical results shown for these examples may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Find the rectangular coordinates of the point having Mars */
/*        planetographic coordinates: */

/*           longitude = 90 degrees west */
/*           latitude  = 45 degrees north */
/*           altitude  = 300 km */

/*        Use the PCK kernel below to load the required triaxial */
/*        ellipsoidal shape model and orientation data for Mars. */

/*           pck00008.tpc */


/*        Example code begins here. */


/*              PROGRAM PGRREC_EX1 */
/*              IMPLICIT NONE */
/*        C */
/*        C     SPICELIB functions */
/*        C */
/*              DOUBLE PRECISION      RPD */
/*        C */
/*        C     Local variables */
/*        C */
/*              DOUBLE PRECISION      ALT */
/*              DOUBLE PRECISION      F */
/*              DOUBLE PRECISION      LAT */
/*              DOUBLE PRECISION      LON */
/*              DOUBLE PRECISION      RADII  ( 3 ) */
/*              DOUBLE PRECISION      RE */
/*              DOUBLE PRECISION      RECTAN ( 3 ) */
/*              DOUBLE PRECISION      RP */

/*              INTEGER               N */
/*        C */
/*        C     Load a PCK file containing a triaxial */
/*        C     ellipsoidal shape model and orientation */
/*        C     data for Mars. */
/*        C */
/*              CALL FURNSH ( 'pck00008.tpc' ) */

/*        C */
/*        C     Look up the radii for Mars.  Although we */
/*        C     omit it here, we could first call BADKPV */
/*        C     to make sure the variable BODY499_RADII */
/*        C     has three elements and numeric data type. */
/*        C     If the variable is not present in the kernel */
/*        C     pool, BODVRD will signal an error. */
/*        C */
/*              CALL BODVRD ( 'MARS', 'RADII', 3, N, RADII ) */

/*        C */
/*        C     Compute flattening coefficient. */
/*        C */
/*              RE  =  RADII(1) */
/*              RP  =  RADII(3) */
/*              F   =  ( RE - RP ) / RE */

/*        C */
/*        C     Do the conversion.  Note that we must provide */
/*        C     longitude and latitude in radians. */
/*        C */
/*              LON =  90.D0 * RPD() */
/*              LAT =  45.D0 * RPD() */
/*              ALT =   3.D2 */

/*              CALL PGRREC ( 'MARS', LON, LAT, ALT, RE, F, RECTAN ) */

/*              WRITE (*,*) ' ' */
/*              WRITE (*,*) 'Planetographic coordinates:' */
/*              WRITE (*,*) ' ' */
/*              WRITE (*,*) '  Longitude (deg)        = ', LON / RPD() */
/*              WRITE (*,*) '  Latitude  (deg)        = ', LAT / RPD() */
/*              WRITE (*,*) '  Altitude  (km)         = ', ALT */
/*              WRITE (*,*) ' ' */
/*              WRITE (*,*) 'Ellipsoid shape parameters: ' */
/*              WRITE (*,*) ' ' */
/*              WRITE (*,*) '  Equatorial radius (km) = ', RE */
/*              WRITE (*,*) '  Polar radius      (km) = ', RP */
/*              WRITE (*,*) '  Flattening coefficient = ', F */
/*              WRITE (*,*) ' ' */
/*              WRITE (*,*) 'Rectangular coordinates:' */
/*              WRITE (*,*) ' ' */
/*              WRITE (*,*) '  X (km)                 = ', RECTAN(1) */
/*              WRITE (*,*) '  Y (km)                 = ', RECTAN(2) */
/*              WRITE (*,*) '  Z (km)                 = ', RECTAN(3) */
/*              WRITE (*,*) ' ' */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Planetographic coordinates: */

/*           Longitude (deg)        =    90.000000000000000 */
/*           Latitude  (deg)        =    45.000000000000000 */
/*           Altitude  (km)         =    300.00000000000000 */

/*         Ellipsoid shape parameters: */

/*           Equatorial radius (km) =    3396.1900000000001 */
/*           Polar radius      (km) =    3376.1999999999998 */
/*           Flattening coefficient =    5.8860075555255261E-003 */

/*         Rectangular coordinates: */

/*           X (km)                 =    1.6047030223125209E-013 */
/*           Y (km)                 =   -2620.6789148181788 */
/*           Z (km)                 =    2592.4089088569663 */


/*     2) Below is a table showing a variety of rectangular coordinates */
/*        and the corresponding Mars planetographic coordinates. The */
/*        values are computed using the reference spheroid having radii */

/*           Equatorial radius:    3396.190 */
/*           Polar radius:         3376.200 */

/*        Note:  the values shown above may not be current or suitable */
/*               for your application. */


/*        Corresponding rectangular and planetographic coordinates are */
/*        listed to three decimal places. */


/*        RECTAN(1)  RECTAN(2)  RECTAN(3)       LON       LAT        ALT */
/*        -------------------------------------------------------------- */
/*         3396.190      0.000      0.000     0.000     0.000      0.000 */
/*        -3396.190      0.000      0.000   180.000     0.000      0.000 */
/*        -3406.190      0.000      0.000   180.000     0.000     10.000 */
/*        -3386.190      0.000      0.000   180.000     0.000    -10.000 */
/*            0.000  -3396.190      0.000    90.000     0.000      0.000 */
/*            0.000   3396.190      0.000   270.000     0.000      0.000 */
/*            0.000      0.000   3376.200     0.000    90.000      0.000 */
/*            0.000      0.000  -3376.200     0.000   -90.000      0.000 */
/*            0.000      0.000      0.000     0.000    90.000  -3376.200 */


/*     3) Below we show the analogous relationships for the earth, */
/*        using the reference ellipsoid radii */

/*           Equatorial radius:    6378.140 */
/*           Polar radius:         6356.750 */

/*        Note the change in longitudes for points on the +/- Y axis */
/*        for the earth vs the Mars values. */


/*        RECTAN(1)  RECTAN(2)  RECTAN(3)       LON       LAT        ALT */
/*        -------------------------------------------------------------- */
/*         6378.140      0.000      0.000     0.000     0.000      0.000 */
/*        -6378.140      0.000      0.000   180.000     0.000      0.000 */
/*        -6388.140      0.000      0.000   180.000     0.000     10.000 */
/*        -6368.140      0.000      0.000   180.000     0.000    -10.000 */
/*            0.000  -6378.140      0.000   270.000     0.000      0.000 */
/*            0.000   6378.140      0.000    90.000     0.000      0.000 */
/*            0.000      0.000   6356.750     0.000    90.000      0.000 */
/*            0.000      0.000  -6356.750     0.000   -90.000      0.000 */
/*            0.000      0.000      0.000     0.000    90.000  -6356.750 */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     C.H. Acton         (JPL) */
/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     H.A. Neilan        (JPL) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.1, 27-MAY-2021 (JDR) */

/*        Edits to header to comply with NAIF standard. */

/* -    SPICELIB Version 1.1.0, 21-SEP-2013 (BVS) */

/*        Updated to save the input body name and ZZBODTRN state */
/*        counter and to do name-ID conversion only if the counter */
/*        has changed. */

/*        Updated to call LJUCRS instead of CMPRSS/UCASE. */

/* -    SPICELIB Version 1.0.0, 26-DEC-2004 (CHA) (NJB) (HAN) (BVS) (WLT) */

/* -& */
/* $ Index_Entries */

/*     convert planetographic to rectangular coordinates */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Saved body name length. */


/*     Local variables */


/*     Saved name/ID item declarations. */


/*     Saved name/ID items. */


/*     Initial values. */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("PGRREC", (ftnlen)6);

/*     Initialization. */

    if (first) {

/*        Initialize counter. */

	zzctruin_(svctr1);
	first = FALSE_;
    }

/*     Convert the body name to an ID code. */

    zzbods2c_(svctr1, svbody, &svbdid, &svfnd1, body, &bodyid, &found, (
	    ftnlen)36, body_len);
    if (! found) {
	setmsg_("The value of the input argument BODY is #, this is not a re"
		"cognized name of an ephemeris object. The cause of this prob"
		"lem may be that you need an updated version of the SPICE Too"
		"lkit. ", (ftnlen)185);
	errch_("#", body, (ftnlen)1, body_len);
	sigerr_("SPICE(IDCODENOTFOUND)", (ftnlen)21);
	chkout_("PGRREC", (ftnlen)6);
	return 0;
    }

/*     The equatorial radius must be positive. If not, signal an error */
/*     and check out. */

    if (*re <= 0.) {
	setmsg_("Equatorial radius was #.", (ftnlen)24);
	errdp_("#", re, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("PGRREC", (ftnlen)6);
	return 0;
    }

/*     If the flattening coefficient is greater than 1, the polar radius */
/*     is negative. If F is equal to 1, the polar radius is zero. Either */
/*     case is a problem, so signal an error and check out. */

    if (*f >= 1.) {
	setmsg_("Flattening coefficient was #.", (ftnlen)29);
	errdp_("#", f, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("PGRREC", (ftnlen)6);
	return 0;
    }

/*     Look up the longitude sense override variable from the */
/*     kernel pool. */

    repmi_("BODY#_PGR_POSITIVE_LON", "#", &bodyid, pmkvar, (ftnlen)22, (
	    ftnlen)1, (ftnlen)32);
    gcpool_(pmkvar, &c__1, &c__1, &n, kvalue, &found, (ftnlen)32, (ftnlen)80);
    if (found) {

/*        Make sure we recognize the value of PGRLON. */

	ljucrs_(&c__0, kvalue, pgrlon, (ftnlen)80, (ftnlen)4);
	if (s_cmp(pgrlon, "EAST", (ftnlen)4, (ftnlen)4) == 0) {
	    sense = 1;
	} else if (s_cmp(pgrlon, "WEST", (ftnlen)4, (ftnlen)4) == 0) {
	    sense = -1;
	} else {
	    setmsg_("Kernel variable # may have the values EAST or WEST.  Ac"
		    "tual value was #.", (ftnlen)72);
	    errch_("#", pmkvar, (ftnlen)1, (ftnlen)32);
	    errch_("#", kvalue, (ftnlen)1, (ftnlen)80);
	    sigerr_("SPICE(INVALIDOPTION)", (ftnlen)20);
	    chkout_("PGRREC", (ftnlen)6);
	    return 0;
	}
    } else {

/*        Look up the spin sense of the body's prime meridian. */

	sense = plnsns_(&bodyid);

/*        If the required prime meridian rate was not available, */
/*        PLNSNS returns the code 0.  Here we consider this situation */
/*        to be an error. */

	if (sense == 0) {
	    repmi_("BODY#_PM", "#", &bodyid, pmkvar, (ftnlen)8, (ftnlen)1, (
		    ftnlen)32);
	    setmsg_("Prime meridian rate coefficient defined by kernel varia"
		    "ble # is required but not available for body #. ", (
		    ftnlen)103);
	    errch_("#", pmkvar, (ftnlen)1, (ftnlen)32);
	    errch_("#", body, (ftnlen)1, body_len);
	    sigerr_("SPICE(MISSINGDATA)", (ftnlen)18);
	    chkout_("PGRREC", (ftnlen)6);
	    return 0;
	}

/*        Handle the special cases:  earth, moon, and sun. */

	if (bodyid == 399 || bodyid == 301 || bodyid == 10) {
	    sense = 1;
	}
    }

/*     At this point, SENSE is set to +/- 1. */

/*     Adjust the longitude according to the sense of the body's */
/*     spin, or according to the override value if one is provided. */
/*     We want positive east longitude. */

    geolon = sense * *lon;

/*     Now that we have geodetic longitude in hand, convert the geodetic */
/*     equivalent of the input coordinates to rectangular coordinates. */

    georec_(&geolon, lat, alt, re, f, rectan);
    chkout_("PGRREC", (ftnlen)6);
    return 0;
} /* pgrrec_ */

