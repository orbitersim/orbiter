/* georec.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static doublereal c_b11 = 1.;

/* $Procedure GEOREC ( Geodetic to rectangular coordinates ) */
/* Subroutine */ int georec_(doublereal *lon, doublereal *lat, doublereal *
	alt, doublereal *re, doublereal *f, doublereal *rectan)
{
    /* System generated locals */
    doublereal d__1, d__2, d__3, d__4;

    /* Builtin functions */
    double cos(doublereal), sin(doublereal), sqrt(doublereal);

    /* Local variables */
    doublereal base[3], cphi, sphi, scale, x, y;
    extern /* Subroutine */ int chkin_(char *, ftnlen), errdp_(char *, 
	    doublereal *, ftnlen), vlcom_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *);
    doublereal clmbda, rp, slmbda, height, normal[3];
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), surfnm_(doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *);
    extern logical return_(void);
    doublereal big;

/* $ Abstract */

/*     Convert geodetic coordinates to rectangular coordinates. */

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
/*     LON        I   Geodetic longitude of point (radians). */
/*     LAT        I   Geodetic latitude  of point (radians). */
/*     ALT        I   Altitude of point above the reference spheroid. */
/*     RE         I   Equatorial radius of the reference spheroid. */
/*     F          I   Flattening coefficient. */
/*     RECTAN     O   Rectangular coordinates of point. */

/* $ Detailed_Input */

/*     LON      is the geodetic longitude of the input point. This is */
/*              the angle between the prime meridian and the meridian */
/*              containing RECTAN. The direction of increasing */
/*              longitude is from the +X axis towards the +Y axis. */

/*              Longitude is measured in radians. On input, the */
/*              range of longitude is unrestricted. */

/*     LAT      is the geodetic latitude of the input point. For a */
/*              point P on the reference spheroid, this is the angle */
/*              between the XY plane and the outward normal vector at */
/*              P. For a point P not on the reference spheroid, the */
/*              geodetic latitude is that of the closest point to P on */
/*              the spheroid. */

/*              Latitude is measured in radians. On input, the */
/*              range of latitude is unrestricted. */

/*     ALT      is the altitude of point above the reference spheroid. */
/*              ALT must be in the same units as RE. */

/*     RE       is the equatorial radius of a reference spheroid. This */
/*              spheroid is a volume of revolution: its horizontal */
/*              cross sections are circular. The shape of the */
/*              spheroid is defined by an equatorial radius RE and */
/*              a polar radius RP. RE must be in the same units */
/*              as ALT. */

/*     F        is the flattening coefficient = (RE-RP) / RE,  where */
/*              RP is the polar radius of the spheroid. */

/* $ Detailed_Output */

/*     RECTAN   are the rectangular coordinates of a point. */

/*              The units associated with RECTAN are those associated */
/*              with the inputs ALT and RE. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the flattening coefficient is greater than or equal to */
/*         one, the error SPICE(VALUEOUTOFRANGE) is signaled. */

/*     2)  If the equatorial radius is less than or equal to zero, */
/*         the error SPICE(VALUEOUTOFRANGE) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Given the geodetic coordinates of a point, and the constants */
/*     describing the reference spheroid,  this routine returns the */
/*     bodyfixed rectangular coordinates of the point. The bodyfixed */
/*     rectangular frame is that having the x-axis pass through the */
/*     0 degree latitude 0 degree longitude point. The y-axis passes */
/*     through the 0 degree latitude 90 degree longitude. The z-axis */
/*     passes through the 90 degree latitude point. For some bodies */
/*     this coordinate system may not be a right-handed coordinate */
/*     system. */

/* $ Examples */

/*     This routine can be used to convert body fixed geodetic */
/*     coordinates (such as the used for United States Geological */
/*     Survey topographic maps) to bodyfixed rectangular coordinates */
/*     such as the Satellite Tracking and Data Network of 1973. */

/*     1) Find the rectangular coordinates of the point having Earth */
/*        geodetic coordinates: */

/*           LON (deg) =  118.0 */
/*           LAT (deg) =   32.0 */
/*           ALT (km)  =    0.0 */

/*        Use the PCK kernel below to load the required triaxial */
/*        ellipsoidal shape model and orientation data for the Earth. */

/*           pck00010.tpc */


/*        Example code begins here. */


/*              PROGRAM GEOREC_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     SPICELIB functions */
/*        C */
/*              DOUBLE PRECISION      DPR */
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
/*        C     Set a geodetic position. */
/*        C */
/*              LON = 118.D0 * RPD() */
/*              LAT =  30.D0 * RPD() */
/*              ALT =   0.D0 */

/*        C */
/*        C     Do the conversion. */
/*        C */
/*              CALL GEOREC( LON, LAT, ALT, RADII(1), F, RECTAN ) */

/*              WRITE (*,*) 'Geodetic coordinates in deg and ' */
/*             . //         'km (lon, lat, alt)' */
/*              WRITE (*,'(A,3F14.6)') ' ', LON * DPR(), LAT * DPR(), ALT */
/*              WRITE (*,*) 'Rectangular coordinates in km (x, y, z)' */
/*              WRITE (*,'(A,3F14.6)') ' ', RECTAN */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Geodetic coordinates in deg and km (lon, lat, alt) */
/*             118.000000     30.000000      0.000000 */
/*         Rectangular coordinates in km (x, y, z) */
/*           -2595.359123   4881.160589   3170.373523 */


/*     2) Create a table showing a variety of rectangular coordinates */
/*        and the corresponding Earth geodetic coordinates. The */
/*        values are computed using the equatorial radius of the Clark */
/*        66 spheroid and the Clark 66 flattening factor: */

/*           radius: 6378.2064 */
/*           flattening factor: 1./294.9787 */

/*        Note: the values shown above may not be current or suitable */
/*              for your application. */


/*        Corresponding rectangular and geodetic coordinates are */
/*        listed to three decimal places. Input angles are in degrees. */


/*        Example code begins here. */


/*              PROGRAM GEOREC_EX2 */
/*              IMPLICIT NONE */

/*        C */
/*        C     SPICELIB functions */
/*        C */
/*              DOUBLE PRECISION      RPD */

/*        C */
/*        C     Local parameters. */
/*        C */
/*              INTEGER               NREC */
/*              PARAMETER           ( NREC = 11 ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              DOUBLE PRECISION      ALT    ( NREC ) */
/*              DOUBLE PRECISION      CLARKR */
/*              DOUBLE PRECISION      CLARKF */
/*              DOUBLE PRECISION      LAT    ( NREC ) */
/*              DOUBLE PRECISION      LON    ( NREC ) */
/*              DOUBLE PRECISION      RECTAN ( 3    ) */
/*              DOUBLE PRECISION      RLAT */
/*              DOUBLE PRECISION      RLON */

/*              INTEGER               I */

/*        C */
/*        C     Define the input geodetic coordinates. Angles in */
/*        C     degrees. */
/*        C */
/*              DATA                 LON /  0.D0,   0.D0,  90.D0, */
/*             .                            0.D0, 180.D0, -90.D0, */
/*             .                            0.D0,  45.D0,   0.D0, */
/*             .                           90.D0,  45.D0         / */

/*              DATA                 LAT / 90.D0,     0.D0,     0.D0, */
/*             .                           90.D0,     0.D0,     0.D0, */
/*             .                          -90.D0,     0.D0,    88.707D0, */
/*             .                           88.707D0, 88.1713D0         / */

/*              DATA                 ALT /   -6356.5838D0,     0.D0, */
/*             .                   0.D0,         0.D0,         0.D0, */
/*             .                   0.D0,         0.D0,         0.D0, */
/*             .               -6355.5725D0, -6355.5725D0, -6355.5612D0 / */

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
/*              WRITE(*,*) '   LON      LAT       ALT    ' */
/*             . //        ' RECTAN(1)  RECTAN(2)  RECTAN(3)' */
/*              WRITE(*,*) ' -------  -------  --------- ' */
/*             . //        ' ---------  ---------  ---------' */

/*        C */
/*        C     Do the conversion. */
/*        C */
/*              DO I = 1, NREC */

/*                 RLON = LON(I) * RPD() */
/*                 RLAT = LAT(I) * RPD() */

/*                 CALL GEOREC( RLON,   RLAT,   ALT(I), */
/*             .                CLARKR, CLARKF, RECTAN ) */

/*                 WRITE (*,'(2F9.3,F11.3,3F11.3)') */
/*             .             LON(I), LAT(I), ALT(I), RECTAN */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*            LON      LAT       ALT     RECTAN(1)  RECTAN(2)  RECTAN(3) */
/*          -------  -------  ---------  ---------  ---------  --------- */
/*            0.000   90.000  -6356.584      0.000      0.000      0.000 */
/*            0.000    0.000      0.000   6378.206      0.000      0.000 */
/*           90.000    0.000      0.000      0.000   6378.206      0.000 */
/*            0.000   90.000      0.000      0.000      0.000   6356.584 */
/*          180.000    0.000      0.000  -6378.206      0.000      0.000 */
/*          -90.000    0.000      0.000      0.000  -6378.206      0.000 */
/*            0.000  -90.000      0.000      0.000      0.000  -6356.584 */
/*           45.000    0.000      0.000   4510.073   4510.073      0.000 */
/*            0.000   88.707  -6355.573      1.000      0.000      1.000 */
/*           90.000   88.707  -6355.573      0.000      1.000      1.000 */
/*           45.000   88.171  -6355.561      1.000      1.000      1.000 */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     [1]  R. Bate, D. Mueller, and J. White, "Fundamentals of */
/*          Astrodynamics," Dover Publications Inc., 1971. */

/* $ Author_and_Institution */

/*     C.H. Acton         (JPL) */
/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     H.A. Neilan        (JPL) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 01-OCT-2021 (JDR) (NJB) */

/*        Changed the input argument name LONG to LON for consistency */
/*        with other routines. */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Added complete */
/*        code examples. */

/* -    SPICELIB Version 1.0.3, 26-JUL-2016 (BVS) */

/*        Minor headers edits. */

/* -    SPICELIB Version 1.0.2, 29-JUL-2003 (NJB) (CHA) */

/*        Various header changes were made to improve clarity. Some */
/*        minor header corrections were made. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (HAN) */

/* -& */
/* $ Index_Entries */

/*     geodetic to rectangular coordinates */

/* -& */
/* $ Revisions */

/* -    Beta Version 3.0.0, 09-JUN-1989  (HAN) */

/*        Error handling added to detect equatorial radius out of */
/*        range. If the equatorial radius is less than or equal to */
/*        zero, an error is signaled. */

/* -    Beta Version 2.0.0, 21-DEC-1988 (HAN) */

/*        Error handling to detect invalid flattening coefficients */
/*        was added. Because the flattening coefficient is used to */
/*        compute the polar radius, it must be checked so that the */
/*        polar radius greater than zero. */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("GEOREC", (ftnlen)6);
    }

/*     The equatorial radius must be greater than zero. */

    if (*re <= 0.) {
	setmsg_("Equatorial radius was *.", (ftnlen)24);
	errdp_("*", re, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("GEOREC", (ftnlen)6);
	return 0;
    }

/*     If the flattening coefficient is greater than one, the polar */
/*     radius computed below is negative. If it's equal to one, the */
/*     polar radius is zero. Either case is a problem, so signal an */
/*     error and check out. */

    if (*f >= 1.) {
	setmsg_("Flattening coefficient was *.", (ftnlen)29);
	errdp_("*", f, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("GEOREC", (ftnlen)6);
	return 0;
    }

/*     Move the altitude to a temporary variable. */

    height = *alt;

/*     Compute the polar radius of the spheroid. */

    rp = *re - *f * *re;

/*     Compute a scale factor needed for finding the rectangular */
/*     coordinates of a point with altitude 0 but the same geodetic */
/*     latitude and longitude as the input point. */

    cphi = cos(*lat);
    sphi = sin(*lat);
    clmbda = cos(*lon);
    slmbda = sin(*lon);
/* Computing MAX */
    d__3 = (d__1 = *re * cphi, abs(d__1)), d__4 = (d__2 = rp * sphi, abs(d__2)
	    );
    big = max(d__3,d__4);
    x = *re * cphi / big;
    y = rp * sphi / big;
    scale = 1. / (big * sqrt(x * x + y * y));

/*     Compute the rectangular coordinates of the point with zero */
/*     altitude. */

    base[0] = scale * *re * *re * clmbda * cphi;
    base[1] = scale * *re * *re * slmbda * cphi;
    base[2] = scale * rp * rp * sphi;

/*     Fetch the normal to the ellipsoid at this point. */

    surfnm_(re, re, &rp, base, normal);

/*     Move along the normal to the input point. */

    vlcom_(&c_b11, base, &height, normal, rectan);
    chkout_("GEOREC", (ftnlen)6);
    return 0;
} /* georec_ */

