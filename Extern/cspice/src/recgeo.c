/* recgeo.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure RECGEO ( Rectangular to geodetic ) */
/* Subroutine */ int recgeo_(doublereal *rectan, doublereal *re, doublereal *
	f, doublereal *lon, doublereal *lat, doublereal *alt)
{
    /* Builtin functions */
    double atan2(doublereal, doublereal);

    /* Local variables */
    doublereal base[3], a, b, c__;
    extern /* Subroutine */ int chkin_(char *, ftnlen), errdp_(char *, 
	    doublereal *, ftnlen), reclat_(doublereal *, doublereal *, 
	    doublereal *, doublereal *);
    doublereal radius, normal[3];
    extern /* Subroutine */ int nearpt_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *), sigerr_(
	    char *, ftnlen), chkout_(char *, ftnlen), setmsg_(char *, ftnlen),
	     surfnm_(doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *);
    extern logical return_(void);

/* $ Abstract */

/*     Convert from rectangular coordinates to geodetic coordinates. */

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

/*     CONVERSION */
/*     COORDINATES */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     RECTAN     I   Rectangular coordinates of a point. */
/*     RE         I   Equatorial radius of the reference spheroid. */
/*     F          I   Flattening coefficient. */
/*     LON        O   Geodetic longitude of the point (radians). */
/*     LAT        O   Geodetic latitude  of the point (radians). */
/*     ALT        O   Altitude of the point above reference spheroid. */

/* $ Detailed_Input */

/*     RECTAN   are the rectangular coordinates of a point. RECTAN */
/*              must be in the same units as RE. */

/*     RE       is the equatorial radius of a reference spheroid. This */
/*              spheroid is a volume of revolution: its horizontal cross */
/*              sections are circular. The shape of the spheroid is */
/*              defined by an equatorial radius RE and a polar radius RP. */
/*              RE must be in the same units as RECTAN. */

/*     F        is the flattening coefficient = (RE-RP) / RE, where RP is */
/*              the polar radius of the spheroid. */

/* $ Detailed_Output */

/*     LON      is the geodetic longitude of the input point. This is the */
/*              angle between the prime meridian and the meridian */
/*              containing RECTAN. The direction of increasing longitude */
/*              is from the +X axis towards the +Y axis. */

/*              LON is output in radians. The range of LON is [-pi, pi]. */

/*     LAT      is the geodetic latitude of the input point. For a point */
/*              P on the reference spheroid, this is the angle between */
/*              the XY plane and the outward normal vector at P. For a */
/*              point P not on the reference spheroid, the geodetic */
/*              latitude is that of the closest point to P on the */
/*              spheroid. */

/*              LAT is output in radians. The range of LAT is */
/*              [-pi/2, pi/2]. */

/*     ALT      is the altitude of point above the reference spheroid. */

/*              The units associated with ALT are those associated with */
/*              the inputs RECTAN and RE. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the equatorial radius is non-positive, the error */
/*         SPICE(VALUEOUTOFRANGE) is signaled. */

/*     2)  If the flattening coefficient is greater than or equal to */
/*         one, the error SPICE(VALUEOUTOFRANGE) is signaled. */

/*     3)  For points inside the reference ellipsoid, the nearest */
/*         point on the ellipsoid to RECTAN may not be unique, so */
/*         latitude may not be well-defined. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Given the body-fixed rectangular coordinates of a point, and the */
/*     constants describing the reference spheroid,  this routine */
/*     returns the geodetic coordinates of the point. The body-fixed */
/*     rectangular frame is that having the x-axis pass through the */
/*     0 degree latitude 0 degree longitude point. The y-axis passes */
/*     through the 0 degree latitude 90 degree longitude. The z-axis */
/*     passes through the 90 degree latitude point. For some bodies */
/*     this coordinate system may not be a right-handed coordinate */
/*     system. */

/* $ Examples */

/*     The numerical results shown for these examples may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Find the geodetic coordinates of the point having Earth */
/*        rectangular coordinates: */

/*           X (km) =  -2541.748162 */
/*           Y (km) =   4780.333036 */
/*           Z (km) =   3360.428190 */

/*        Use the PCK kernel below to load the required triaxial */
/*        ellipsoidal shape model and orientation data for the Earth. */

/*           pck00010.tpc */


/*        Example code begins here. */


/*              PROGRAM RECGEO_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     SPICELIB functions */
/*        C */
/*              DOUBLE PRECISION      DPR */

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
/*        C     data for the Earth. */
/*        C */
/*              CALL FURNSH ( 'pck00010.tpc' ) */

/*        C */
/*        C     Retrieve the triaxial radii of the Earth */
/*        C */
/*              CALL BODVRD ( 'EARTH', 'RADII', 3, N, RADII ) */

/*        C */
/*        C     Compute flattening coefficient. */
/*        C */
/*              RE  =  RADII(1) */
/*              RP  =  RADII(3) */
/*              F   =  ( RE - RP ) / RE */

/*        C */
/*        C     Set a body-fixed position. */
/*        C */
/*              RECTAN(1) =  -2541.748162D0 */
/*              RECTAN(2) =   4780.333036D0 */
/*              RECTAN(3) =   3360.428190D0 */

/*        C */
/*        C     Do the conversion. Output angles in degrees. */
/*        C */
/*              CALL RECGEO( RECTAN, RADII(1), F, LON, LAT, ALT ) */

/*              WRITE (*,*) 'Rectangular coordinates in km (x, y, z)' */
/*              WRITE (*,'(A,3F14.6)') ' ', RECTAN */
/*              WRITE (*,*) 'Geodetic coordinates in deg and ' */
/*             . //         'km (lon, lat, alt)' */
/*              WRITE (*,'(A,3F14.6)') ' ', LON * DPR(), LAT * DPR(), ALT */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Rectangular coordinates in km (x, y, z) */
/*           -2541.748162   4780.333036   3360.428190 */
/*         Geodetic coordinates in deg and km (lon, lat, alt) */
/*             118.000000     31.999957      0.001916 */


/*     2) Create a table showing a variety of rectangular coordinates */
/*        and the corresponding Earth geodetic coordinates. The */
/*        values are computed using the equatorial radius of the Clark */
/*        66 spheroid and the Clark 66 flattening factor: */

/*           radius: 6378.2064 */
/*           flattening factor: 1./294.9787 */

/*        Note: the values shown above may not be current or suitable */
/*              for your application. */


/*        Corresponding rectangular and geodetic coordinates are */
/*        listed to three decimal places. Output angles are in degrees. */

/*        Example code begins here. */


/*              PROGRAM RECGEO_EX2 */
/*              IMPLICIT NONE */

/*        C */
/*        C     SPICELIB functions */
/*        C */
/*              DOUBLE PRECISION      DPR */

/*        C */
/*        C     Local parameters. */
/*        C */
/*              INTEGER               NREC */
/*              PARAMETER           ( NREC = 11 ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              DOUBLE PRECISION      ALT */
/*              DOUBLE PRECISION      CLARKR */
/*              DOUBLE PRECISION      CLARKF */
/*              DOUBLE PRECISION      LAT */
/*              DOUBLE PRECISION      LON */
/*              DOUBLE PRECISION      RECTAN ( 3, NREC ) */

/*              INTEGER               I */
/*              INTEGER               J */

/*        C */
/*        C     Define the input rectangular coordinates. */
/*        C */
/*              DATA                 RECTAN / */
/*             .                  0.D0,         0.D0,         0.D0, */
/*             .               6378.2064D0,     0.D0,         0.D0, */
/*             .                  0.D0,      6378.2064D0,     0.D0, */
/*             .                  0.D0,         0.D0,      6378.2064D0, */
/*             .              -6378.2064D0,     0.D0,         0.D0, */
/*             .                  0.D0,     -6378.2064D0,     0.D0, */
/*             .                  0.D0,         0.D0,     -6378.2064D0, */
/*             .               6378.2064D0,  6378.2064D0,     0.D0, */
/*             .               6378.2064D0,     0.D0,      6378.2064D0, */
/*             .                  0.D0,      6378.2064D0,  6378.2064D0, */
/*             .               6378.2064D0,  6378.2064D0,  6378.2064D0 / */

/*        C */
/*        C     Using the equatorial radius of the Clark66 spheroid */
/*        C     (CLARKR = 6378.2064 km) and the Clark 66 flattening */
/*        C     factor (CLARKF = 1.0 / 294.9787 ) convert from */
/*        C     body fixed rectangular coordinates. */
/*        C */
/*              CLARKR = 6378.2064D0 */
/*              CLARKF = 1.0D0 / 294.9787D0 */

/*        C */
/*        C     Print the banner. */
/*        C */
/*              WRITE(*,*) ' RECTAN(1)  RECTAN(2)  RECTAN(3) ' */
/*             . //        '   LON      LAT       ALT' */
/*              WRITE(*,*) ' ---------  ---------  --------- ' */
/*             . //        ' -------  -------  ---------' */

/*        C */
/*        C     Do the conversion. Output angles in degrees. */
/*        C */
/*              DO I = 1, NREC */

/*                 CALL RECGEO( RECTAN(1,I), CLARKR, CLARKF, */
/*             .                LON,         LAT,    ALT    ) */

/*                 WRITE (*,'(3F11.3,2F9.3,F11.3)') */
/*             .             ( RECTAN(J,I), J=1,3 ), LON * DPR(), */
/*             .             LAT * DPR(), ALT */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*          RECTAN(1)  RECTAN(2)  RECTAN(3)    LON      LAT       ALT */
/*          ---------  ---------  ---------  -------  -------  --------- */
/*              0.000      0.000      0.000    0.000   90.000  -6356.584 */
/*           6378.206      0.000      0.000    0.000    0.000      0.000 */
/*              0.000   6378.206      0.000   90.000    0.000      0.000 */
/*              0.000      0.000   6378.206    0.000   90.000     21.623 */
/*          -6378.206      0.000      0.000  180.000    0.000      0.000 */
/*              0.000  -6378.206      0.000  -90.000    0.000      0.000 */
/*              0.000      0.000  -6378.206    0.000  -90.000     21.623 */
/*           6378.206   6378.206      0.000   45.000    0.000   2641.940 */
/*           6378.206      0.000   6378.206    0.000   45.137   2652.768 */
/*              0.000   6378.206   6378.206   90.000   45.137   2652.768 */
/*           6378.206   6378.206   6378.206   45.000   35.370   4676.389 */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     [1]  R. Bate, D. Mueller, and J. White, "Fundamentals of */
/*          Astrodynamics," Dover Publications Inc., 1971. */

/* $ Author_and_Institution */

/*     C.H. Acton         (JPL) */
/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.0, 01-OCT-2021 (JDR) (NJB) */

/*        Changed the output argument name LONG to LON for consistency */
/*        with other routines. */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Added complete */
/*        code examples. */

/* -    SPICELIB Version 1.1.0, 03-AUG-2016 (BVS) (NJB) */

/*        Re-implemented derivation of longitude to improve */
/*        accuracy. */

/*        Minor header edits. */

/* -    SPICELIB Version 1.0.3, 02-JUL-2007 (NJB) */

/*        In $Examples section of header, description of right-hand */
/*        table was updated to use correct names of columns. Term */
/*        "bodyfixed" is now hyphenated. */

/* -    SPICELIB Version 1.0.2, 30-JUL-2003 (NJB) (CHA) */

/*        Various header changes were made to improve clarity. Some */
/*        minor header corrections were made. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) */

/* -& */
/* $ Index_Entries */

/*     rectangular to geodetic */

/* -& */
/* $ Revisions */

/* -    Beta Version 3.0.1, 9-JUN-1989 (HAN) */

/*        Error handling was added to detect and equatorial radius */
/*        whose value is less than or equal to zero. */

/* -    Beta Version 2.0.0, 21-DEC-1988 (HAN) */

/*        Error handling to detect invalid flattening coefficients */
/*        was added. Because the flattening coefficient is used to */
/*        compute the length of an axis, it must be checked so that */
/*        the length is greater than zero. */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("RECGEO", (ftnlen)6);
    }

/*     The equatorial radius must be positive. If not, signal an error */
/*     and check out. */

    if (*re <= 0.) {
	setmsg_("Equatorial radius was *.", (ftnlen)24);
	errdp_("*", re, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("RECGEO", (ftnlen)6);
	return 0;
    }

/*     If the flattening coefficient is greater than one, the length */
/*     of the 'C' axis computed below is negative. If it's equal to one, */
/*     the length of the axis is zero. Either case is a problem, so */
/*     signal an error and check out. */

    if (*f >= 1.) {
	setmsg_("Flattening coefficient was *.", (ftnlen)29);
	errdp_("*", f, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("RECGEO", (ftnlen)6);
	return 0;
    }

/*     Determine the lengths of the axes of the reference ellipsoid. */

    a = *re;
    b = *re;
    c__ = *re - *f * *re;

/*     Find the point on the reference spheroid closest to the input */
/*     point. From this closest point determine the surface normal. */

    nearpt_(rectan, &a, &b, &c__, base, alt);
    surfnm_(&a, &b, &c__, base, normal);

/*     Using the surface normal, determine the latitude and longitude */
/*     of the input point. */

    reclat_(normal, &radius, lon, lat);

/*     Compute longitude directly rather than from the normal vector. */

    if (rectan[0] == 0. && rectan[1] == 0.) {
	*lon = 0.;
    } else {
	*lon = atan2(rectan[1], rectan[0]);
    }
    chkout_("RECGEO", (ftnlen)6);
    return 0;
} /* recgeo_ */

