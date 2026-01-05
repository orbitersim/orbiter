/* drdgeo.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure DRDGEO ( Derivative of rectangular w.r.t. geodetic ) */
/* Subroutine */ int drdgeo_(doublereal *lon, doublereal *lat, doublereal *
	alt, doublereal *re, doublereal *f, doublereal *jacobi)
{
    /* Builtin functions */
    double cos(doublereal), sin(doublereal), sqrt(doublereal);

    /* Local variables */
    doublereal clat, flat, clon, slat, slon, flat2, g;
    extern /* Subroutine */ int chkin_(char *, ftnlen), errdp_(char *, 
	    doublereal *, ftnlen);
    doublereal g2, dgdlat;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Compute the Jacobian matrix of the transformation from geodetic */
/*     to rectangular coordinates. */

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

/*     COORDINATES */
/*     DERIVATIVES */
/*     MATRIX */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     LON        I   Geodetic longitude of point (radians). */
/*     LAT        I   Geodetic latitude of point (radians). */
/*     ALT        I   Altitude of point above the reference spheroid. */
/*     RE         I   Equatorial radius of the reference spheroid. */
/*     F          I   Flattening coefficient. */
/*     JACOBI     O   Matrix of partial derivatives. */

/* $ Detailed_Input */

/*     LON      is the geodetic longitude of point (radians). */

/*     LAT      is the geodetic latitude  of point (radians). */

/*     ALT      is the altitude of point above the reference spheroid. */

/*     RE       is the equatorial radius of the reference spheroid. */

/*     F        is the flattening coefficient = (RE-RP) / RE,  where */
/*              RP is the polar radius of the spheroid. (More */
/*              importantly RP = RE*(1-F).) */

/* $ Detailed_Output */

/*     JACOBI   is the matrix of partial derivatives of the conversion */
/*              between geodetic and rectangular coordinates. It */
/*              has the form */

/*                 .-                             -. */
/*                 |  DX/DLON   DX/DLAT  DX/DALT   | */
/*                 |  DY/DLON   DY/DLAT  DY/DALT   | */
/*                 |  DZ/DLON   DZ/DLAT  DZ/DALT   | */
/*                 `-                             -' */

/*              evaluated at the input values of LON, LAT and ALT. */

/*              The formulae for computing X, Y, and Z from */
/*              geodetic coordinates are given below. */

/*                 X = [ALT +          RE/G(LAT,F)]*COS(LON)*COS(LAT) */
/*                 Y = [ALT +          RE/G(LAT,F)]*SIN(LON)*COS(LAT) */
/*                 Z = [ALT + RE*(1-F)**2/G(LAT,F)]*         SIN(LAT) */

/*              where */

/*                 RE is the polar radius of the reference spheroid. */

/*                 F  is the flattening factor (the polar radius is */
/*                    obtained by multiplying the equatorial radius by */
/*                    1-F). */

/*                 G( LAT, F ) is given by */

/*                    sqrt ( cos(lat)**2 + (1-f)**2 * sin(lat)**2 ) */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the flattening coefficient is greater than or equal to */
/*         one, the error SPICE(VALUEOUTOFRANGE) is signaled. */

/*     2)  If the equatorial radius is non-positive, the error */
/*         SPICE(BADRADIUS) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     It is often convenient to describe the motion of an object in */
/*     the geodetic coordinate system. However, when performing */
/*     vector computations its hard to beat rectangular coordinates. */

/*     To transform states given with respect to geodetic coordinates */
/*     to states with respect to rectangular coordinates, one makes use */
/*     of the Jacobian of the transformation between the two systems. */

/*     Given a state in geodetic coordinates */

/*          ( lon, lat, alt, dlon, dlat, dalt ) */

/*     the velocity in rectangular coordinates is given by the matrix */
/*     equation: */

/*                    t          |                                  t */
/*        (dx, dy, dz)   = JACOBI|              * (dlon, dlat, dalt) */
/*                               |(lon,lat,alt) */


/*     This routine computes the matrix */

/*              | */
/*        JACOBI| */
/*              |(lon,lat,alt) */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Find the geodetic state of the earth as seen from */
/*        Mars in the IAU_MARS reference frame at January 1, 2005 TDB. */
/*        Map this state back to rectangular coordinates as a check. */

/*        Use the meta-kernel shown below to load the required SPICE */
/*        kernels. */


/*           KPL/MK */

/*           File name: drdgeo_ex1.tm */

/*           This meta-kernel is intended to support operation of SPICE */
/*           example programs. The kernels shown here should not be */
/*           assumed to contain adequate or correct versions of data */
/*           required by SPICE-based user applications. */

/*           In order for an application to use this meta-kernel, the */
/*           kernels referenced here must be present in the user's */
/*           current working directory. */

/*           The names and contents of the kernels referenced */
/*           by this meta-kernel are as follows: */

/*              File name                     Contents */
/*              ---------                     -------- */
/*              de421.bsp                     Planetary ephemeris */
/*              pck00010.tpc                  Planet orientation and */
/*                                            radii */
/*              naif0009.tls                  Leapseconds */


/*           \begindata */

/*              KERNELS_TO_LOAD = ( 'de421.bsp', */
/*                                  'pck00010.tpc', */
/*                                  'naif0009.tls'  ) */

/*           \begintext */

/*           End of meta-kernel */


/*        Example code begins here. */


/*              PROGRAM DRDGEO_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     SPICELIB functions */
/*        C */
/*              DOUBLE PRECISION      RPD */

/*        C */
/*        C     Local parameters */
/*        C */
/*              CHARACTER*(*)         FMT1 */
/*              PARAMETER           ( FMT1 = '(A,E18.8)' ) */

/*        C */
/*        C     Local variables */
/*        C */
/*              DOUBLE PRECISION      ALT */
/*              DOUBLE PRECISION      DRECTN ( 3 ) */
/*              DOUBLE PRECISION      ET */
/*              DOUBLE PRECISION      F */
/*              DOUBLE PRECISION      JACOBI ( 3, 3 ) */
/*              DOUBLE PRECISION      LAT */
/*              DOUBLE PRECISION      LON */
/*              DOUBLE PRECISION      LT */
/*              DOUBLE PRECISION      GEOVEL ( 3 ) */
/*              DOUBLE PRECISION      RADII  ( 3 ) */
/*              DOUBLE PRECISION      RE */
/*              DOUBLE PRECISION      RECTAN ( 3 ) */
/*              DOUBLE PRECISION      RP */
/*              DOUBLE PRECISION      STATE  ( 6 ) */

/*              INTEGER               N */

/*        C */
/*        C     Load SPK, PCK, and LSK kernels, use a meta kernel for */
/*        C     convenience. */
/*        C */
/*              CALL FURNSH ( 'drdgeo_ex1.tm' ) */

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
/*        C     Look up the apparent state of earth as seen from Mars */
/*        C     at January 1, 2005 TDB, relative to the IAU_MARS */
/*        C     reference frame. */
/*        C */
/*              CALL STR2ET ( 'January 1, 2005 TDB', ET ) */

/*              CALL SPKEZR ( 'Earth', ET,    'IAU_MARS', 'LT+S', */
/*             .              'Mars',  STATE, LT                ) */

/*        C */
/*        C     Convert position to geodetic coordinates. */
/*        C */
/*              CALL RECGEO ( STATE, RE, F, LON, LAT, ALT ) */

/*        C */
/*        C     Convert velocity to geodetic coordinates. */
/*        C */

/*              CALL DGEODR (  STATE(1), STATE(2), STATE(3), */
/*             .               RE,       F,        JACOBI   ) */

/*              CALL MXV ( JACOBI, STATE(4), GEOVEL ) */

/*        C */
/*        C     As a check, convert the geodetic state back to */
/*        C     rectangular coordinates. */
/*        C */
/*              CALL GEOREC ( LON, LAT, ALT, RE, F, RECTAN ) */

/*              CALL DRDGEO ( LON, LAT, ALT, RE, F, JACOBI ) */

/*              CALL MXV ( JACOBI, GEOVEL, DRECTN ) */


/*              WRITE(*,*) ' ' */
/*              WRITE(*,*) 'Rectangular coordinates:' */
/*              WRITE(*,*) ' ' */
/*              WRITE(*,FMT1) '  X (km)                 = ', STATE(1) */
/*              WRITE(*,FMT1) '  Y (km)                 = ', STATE(2) */
/*              WRITE(*,FMT1) '  Z (km)                 = ', STATE(3) */
/*              WRITE(*,*) ' ' */
/*              WRITE(*,*) 'Rectangular velocity:' */
/*              WRITE(*,*) ' ' */
/*              WRITE(*,FMT1) '  dX/dt (km/s)           = ', STATE(4) */
/*              WRITE(*,FMT1) '  dY/dt (km/s)           = ', STATE(5) */
/*              WRITE(*,FMT1) '  dZ/dt (km/s)           = ', STATE(6) */
/*              WRITE(*,*) ' ' */
/*              WRITE(*,*) 'Ellipsoid shape parameters: ' */
/*              WRITE(*,*) ' ' */
/*              WRITE(*,FMT1) '  Equatorial radius (km) = ', RE */
/*              WRITE(*,FMT1) '  Polar radius      (km) = ', RP */
/*              WRITE(*,FMT1) '  Flattening coefficient = ', F */
/*              WRITE(*,*) ' ' */
/*              WRITE(*,*) 'Geodetic coordinates:' */
/*              WRITE(*,*) ' ' */
/*              WRITE(*,FMT1) '  Longitude (deg)        = ', LON / RPD() */
/*              WRITE(*,FMT1) '  Latitude  (deg)        = ', LAT / RPD() */
/*              WRITE(*,FMT1) '  Altitude  (km)         = ', ALT */
/*              WRITE(*,*) ' ' */
/*              WRITE(*,*) 'Geodetic velocity:' */
/*              WRITE(*,*) ' ' */
/*              WRITE(*,FMT1) '  d Longitude/dt (deg/s) = ', */
/*             .                                         GEOVEL(1)/RPD() */
/*              WRITE(*,FMT1) '  d Latitude/dt  (deg/s) = ', */
/*             .                                         GEOVEL(2)/RPD() */
/*              WRITE(*,FMT1) '  d Altitude/dt  (km/s)  = ', GEOVEL(3) */
/*              WRITE(*,*) ' ' */
/*              WRITE(*,*) 'Rectangular coordinates from inverse ' // */
/*             .           'mapping:' */
/*              WRITE(*,*) ' ' */
/*              WRITE(*,FMT1) '  X (km)                 = ', RECTAN(1) */
/*              WRITE(*,FMT1) '  Y (km)                 = ', RECTAN(2) */
/*              WRITE(*,FMT1) '  Z (km)                 = ', RECTAN(3) */
/*              WRITE(*,*) ' ' */
/*              WRITE(*,*) 'Rectangular velocity from inverse mapping:' */
/*              WRITE(*,*) ' ' */
/*              WRITE(*,FMT1) '  dX/dt (km/s)           = ', DRECTN(1) */
/*              WRITE(*,FMT1) '  dY/dt (km/s)           = ', DRECTN(2) */
/*              WRITE(*,FMT1) '  dZ/dt (km/s)           = ', DRECTN(3) */
/*              WRITE(*,*) ' ' */
/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Rectangular coordinates: */

/*          X (km)                 =    -0.76096183E+08 */
/*          Y (km)                 =     0.32436380E+09 */
/*          Z (km)                 =     0.47470484E+08 */

/*         Rectangular velocity: */

/*          dX/dt (km/s)           =     0.22952075E+05 */
/*          dY/dt (km/s)           =     0.53760111E+04 */
/*          dZ/dt (km/s)           =    -0.20881149E+02 */

/*         Ellipsoid shape parameters: */

/*          Equatorial radius (km) =     0.33961900E+04 */
/*          Polar radius      (km) =     0.33762000E+04 */
/*          Flattening coefficient =     0.58860076E-02 */

/*         Geodetic coordinates: */

/*          Longitude (deg)        =     0.10320290E+03 */
/*          Latitude  (deg)        =     0.81089876E+01 */
/*          Altitude  (km)         =     0.33653182E+09 */

/*         Geodetic velocity: */

/*          d Longitude/dt (deg/s) =    -0.40539288E-02 */
/*          d Latitude/dt  (deg/s) =    -0.33189934E-05 */
/*          d Altitude/dt  (km/s)  =    -0.11211601E+02 */

/*         Rectangular coordinates from inverse mapping: */

/*          X (km)                 =    -0.76096183E+08 */
/*          Y (km)                 =     0.32436380E+09 */
/*          Z (km)                 =     0.47470484E+08 */

/*         Rectangular velocity from inverse mapping: */

/*          dX/dt (km/s)           =     0.22952075E+05 */
/*          dY/dt (km/s)           =     0.53760111E+04 */
/*          dZ/dt (km/s)           =    -0.20881149E+02 */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR) */

/*        Changed the input argument name LONG to LON for consistency */
/*        with other routines. */

/*        Edited the header to comply with NAIF standard. */
/*        Added complete code example. */

/* -    SPICELIB Version 1.0.0, 20-JUL-2001 (WLT) */

/* -& */
/* $ Index_Entries */

/*     Jacobian of rectangular w.r.t. geodetic coordinates */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("DRDGEO", (ftnlen)6);
    }

/*     If the flattening coefficient is greater than one, the polar */
/*     radius computed below is negative. If it's equal to one, the */
/*     polar radius is zero. Either case is a problem, so signal an */
/*     error and check out. */

    if (*f >= 1.) {
	setmsg_("Flattening coefficient was *.", (ftnlen)29);
	errdp_("*", f, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("DRDGEO", (ftnlen)6);
	return 0;
    }
    if (*re <= 0.) {
	setmsg_("Equatorial Radius <= 0.0D0. RE = *", (ftnlen)34);
	errdp_("*", re, (ftnlen)1);
	sigerr_("SPICE(BADRADIUS)", (ftnlen)16);
	chkout_("DRDGEO", (ftnlen)6);
	return 0;
    }

/*     For the record, here is a derivation of the formulae for the */
/*     values of x, y and z as a function of longitude, latitude and */
/*     altitude. */

/*     First, let's take the case where the longitude is 0. Moreover, */
/*     lets assume that the length of the equatorial axis is a and */
/*     that the polar axis is b: */

/*        a = re */
/*        b = re * (1-f) */

/*     For any point on the spheroid where y is zero we know that there */
/*     is a unique q in the range (-Pi, Pi] such that */

/*        x = a cos(q) and z = b sin(q). */

/*     The normal to the surface at such a point is given by */

/*           cos(q)     sin(q) */
/*        ( ------- ,  ------- ) */
/*             a          b */

/*     The unit vector in the same direction is */

/*                 b cos(q)                         a sin(q) */
/*        ( --------------------------  ,  -------------------------- ) */
/*             ______________________         ______________________ */
/*            / 2   2        2   2           / 2   2        2   2 */
/*          \/ b cos (q)  + a sin (q)      \/ b cos (q)  + a sin (q) */


/*     The first component of this term is by definition equal to the */
/*     cosine of the geodetic latitude, thus */

/*                                ______________________ */
/*                               / 2   2        2   2 */
/*        b cos(q) = cos(lat)  \/ b cos (q)  + a sin (q) */


/*     This can be transformed to the equation */

/*                                ______________________________ */
/*                               /   2    2     2        2 */
/*        b cos(q) = cos(lat)  \/ ( b  - a  )cos (q)  + a */


/*     Squaring both sides and rearranging terms gives: */

/*         2   2         2         2   2     2        2    2 */
/*        b cos (q) + cos (lat) ( a - b ) cos (q) =  a  cos (lat) */

/*     Thus */
/*                           2    2 */
/*           2              a  cos (lat) */
/*        cos (q)  =  -------------------------- */
/*                     2    2         2   2 */
/*                    b  sin (lat) + a cos (lat) */



/*                             cos (lat) */
/*                 =  ------------------------------ */
/*                       _____________________________ */
/*                      /      2    2           2 */
/*                    \/  (b/a)  sin (lat) + cos (lat) */



/*                             cos (lat) */
/*                 =  --------------------------------- */
/*                       _____________________________ */
/*                      /      2    2           2 */
/*                    \/  (1-f)  sin (lat) + cos (lat) */



/*     From this one can also conclude that */


/*                           (1-f) sin (lat) */
/*        sin(q)   =  ---------------------------------- */
/*                        _____________________________ */
/*                       /      2    2           2 */
/*                     \/  (1-f)  sin (lat) + cos (lat) */



/*     Thus the point on the surface of the spheroid is given by */

/*                            re * cos (lat) */
/*        x_0      =  --------------------------------- */
/*                        _____________________________ */
/*                       /      2    2           2 */
/*                     \/  (1-f)  sin (lat) + cos (lat) */



/*                                  2 */
/*                        re * (1-f) sin (lat) */
/*        z_0      =  ---------------------------------- */
/*                        _____________________________ */
/*                       /      2    2           2 */
/*                     \/  (1-f)  sin (lat) + cos (lat) */


/*     Thus given a point with the same latitude but a non-zero */
/*     longitude, one can conclude that */

/*                         re * cos (lon) *cos (lat) */
/*        x_0      =  --------------------------------- */
/*                        _____________________________ */
/*                       /      2    2           2 */
/*                     \/  (1-f)  sin (lat) + cos (lat) */



/*                         re * sin (lon) cos (lat) */
/*        y_0      =  --------------------------------- */
/*                        _____________________________ */
/*                       /      2    2           2 */
/*                     \/  (1-f)  sin (lat) + cos (lat) */


/*                                    2 */
/*                          re * (1-f) sin (lat) */
/*        z_0      =  ---------------------------------- */
/*                        _____________________________ */
/*                       /      2    2           2 */
/*                     \/  (1-f)  sin (lat) + cos (lat) */


/*     The unit normal, n, at this point is simply */

/*        ( cos(lon)cos(lat),  sin(lon)cos(lat),  sin(lat) ) */


/*     Thus for a point at altitude alt, we simply add the vector */

/*        alt*n */

/*     to the vector ( x_0, y_0, z_0 ).  Hence we have */

/*        x = [ alt +          re/g(lat,f) ] * cos(lon) * cos(lat) */
/*        y = [ alt +          re/g(lat,f) ] * sin(lon) * cos(lat) */
/*        z = [ alt + re*(1-f)**2/g(lat,f) ] *            sin(lat) */


/*     We're going to need the sine and cosine of LAT and LON many */
/*     times.  We'll just compute them once. */

    clat = cos(*lat);
    clon = cos(*lon);
    slat = sin(*lat);
    slon = sin(*lon);

/*     Referring to the G given in the header we have... */

    flat = 1. - *f;
    flat2 = flat * flat;
    g = sqrt(clat * clat + flat2 * slat * slat);
    g2 = g * g;
    dgdlat = (flat2 - 1.) * slat * clat / g;

/*     Now simply take the partial derivatives of the x,y,z w.r.t. */
/*     lon, lat, alt. */

    jacobi[0] = -(*alt + *re / g) * slon * clat;
    jacobi[1] = (*alt + *re / g) * clon * clat;
    jacobi[2] = 0.;
    jacobi[3] = -(*re) * dgdlat / g2 * clon * clat - (*alt + *re / g) * clon *
	     slat;
    jacobi[4] = -(*re) * dgdlat / g2 * slon * clat - (*alt + *re / g) * slon *
	     slat;
    jacobi[5] = -flat2 * *re * dgdlat / g2 * slat + (*alt + flat2 * *re / g) *
	     clat;
    jacobi[6] = clon * clat;
    jacobi[7] = slon * clat;
    jacobi[8] = slat;
    chkout_("DRDGEO", (ftnlen)6);
    return 0;
} /* drdgeo_ */

