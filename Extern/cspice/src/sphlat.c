/* sphlat.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure SPHLAT ( Spherical to latitudinal coordinates ) */
/* Subroutine */ int sphlat_(doublereal *r__, doublereal *colat, doublereal *
	slon, doublereal *radius, doublereal *lon, doublereal *lat)
{
    extern doublereal halfpi_(void);
    doublereal rr, lattud;

/* $ Abstract */

/*     Convert from spherical coordinates to latitudinal coordinates. */

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
/*     R          I   Distance of the point from the origin. */
/*     COLAT      I   Angle of the point from positive Z axis (radians). */
/*     SLON       I   Angle of the point from the XZ plane (radians). */
/*     RADIUS     O   Distance of a point from the origin */
/*     LON        O   Angle of the point from the XZ plane in radians */
/*     LAT        O   Angle of the point from the XY plane in radians */

/* $ Detailed_Input */

/*     R        is the distance of the point from the origin. */

/*     COLAT    is the angle between the vector from the origin to the */
/*              point and the positive Z-axis in radians. */

/*     SLON     is the angle of the point from the XZ plane (radians). */

/* $ Detailed_Output */

/*     RADIUS   is the distance of a point from the origin. */

/*     LON      is the angle of the point from the XZ plane in */
/*              radians. LON is set equal to SLON. */

/*     LAT      is the angle of the point from the XY plane in */
/*              radians. LAT is computed as pi/2 - COLAT. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine returns the latitudinal coordinates of a point */
/*     whose position is input in spherical coordinates. */

/*     Latitudinal coordinates are defined by a distance from a central */
/*     reference point, an angle from a reference meridian, and an angle */
/*     above the equator of a sphere centered at the central reference */
/*     point. */

/*     Spherical coordinates are defined by a distance from a central */
/*     reference point, an angle from a reference meridian, and an angle */
/*     from the Z-axis. */

/* $ Examples */

/*     The numerical results shown for these examples may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Latitude is obtained by subtracting co-latitude from HALFPI */
/*        Radius and longitude mean the same thing in both latitudinal */
/*        and spherical coordinates. The table below lists LAT and */
/*        corresponding COLAT in terms of degrees. */

/*             LAT     COLAT */
/*            -----    ----- */
/*               0        90 */
/*              20        70 */
/*              45        45 */
/*             -30       120 */
/*              90         0 */
/*             -45       135 */


/*     2) Compute the spherical coordinates of the position of the Moon */
/*        as seen from the Earth, and convert them to latitudinal and */
/*        rectangular coordinates. */

/*        Use the meta-kernel shown below to load the required SPICE */
/*        kernels. */


/*           KPL/MK */

/*           File name: sphlat_ex2.tm */

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
/*              naif0012.tls                  Leapseconds */


/*           \begindata */

/*              KERNELS_TO_LOAD = ( 'de421.bsp', */
/*                                  'naif0012.tls'  ) */

/*           \begintext */

/*           End of meta-kernel */


/*        Example code begins here. */


/*              PROGRAM SPHLAT_EX2 */
/*              IMPLICIT NONE */

/*        C */
/*        C     SPICELIB functions */
/*        C */
/*              DOUBLE PRECISION      DPR */

/*        C */
/*        C     Local parameters */
/*        C */
/*              CHARACTER*(*)         FMT1 */
/*              PARAMETER           ( FMT1 = '(A,F20.8)' ) */

/*        C */
/*        C     Local variables */
/*        C */
/*              DOUBLE PRECISION      COLAT */
/*              DOUBLE PRECISION      ET */
/*              DOUBLE PRECISION      LT */
/*              DOUBLE PRECISION      LAT */
/*              DOUBLE PRECISION      LON */
/*              DOUBLE PRECISION      POS    ( 3 ) */
/*              DOUBLE PRECISION      R */
/*              DOUBLE PRECISION      RADIUS */
/*              DOUBLE PRECISION      RECTAN ( 3 ) */
/*              DOUBLE PRECISION      SLON */
/*              DOUBLE PRECISION      Z */

/*        C */
/*        C     Load SPK and LSK kernels, use a meta kernel for */
/*        C     convenience. */
/*        C */
/*              CALL FURNSH ( 'sphlat_ex2.tm' ) */

/*        C */
/*        C     Look up the geometric state of the Moon as seen from */
/*        C     the Earth at 2017 Mar 20, relative to the J2000 */
/*        C     reference frame. */
/*        C */
/*              CALL STR2ET ( '2017 Mar 20', ET ) */

/*              CALL SPKPOS ( 'Moon',  ET,  'J2000', 'NONE', */
/*             .              'Earth', POS, LT               ) */

/*        C */
/*        C     Convert the position vector POS to spherical */
/*        C     coordinates. */
/*        C */
/*              CALL RECSPH ( POS, R, COLAT, SLON ) */

/*        C */
/*        C     Convert the spherical coordinates to latitudinal. */
/*        C */
/*              CALL SPHLAT ( R, COLAT, SLON, RADIUS, LON, LAT ) */

/*        C */
/*        C     Convert the latitudinal coordinates to rectangular. */
/*        C */
/*              CALL LATREC ( RADIUS, LON, LAT, RECTAN ) */


/*              WRITE(*,*) ' ' */
/*              WRITE(*,*) 'Original rectangular coordinates:' */
/*              WRITE(*,*) ' ' */
/*              WRITE(*,FMT1) '  X           (km): ', POS(1) */
/*              WRITE(*,FMT1) '  Y           (km): ', POS(2) */
/*              WRITE(*,FMT1) '  Z           (km): ', POS(3) */
/*              WRITE(*,*) ' ' */
/*              WRITE(*,*) 'Spherical coordinates:' */
/*              WRITE(*,*) ' ' */
/*              WRITE(*,FMT1) '  Radius      (km): ', R */
/*              WRITE(*,FMT1) '  Colatitude (deg): ', COLAT*DPR() */
/*              WRITE(*,FMT1) '  Longitude  (deg): ', SLON*DPR() */
/*              WRITE(*,*) ' ' */
/*              WRITE(*,*) 'Latitudinal coordinates:' */
/*              WRITE(*,*) ' ' */
/*              WRITE(*,FMT1) '  Radius      (km): ', RADIUS */
/*              WRITE(*,FMT1) '  Longitude  (deg): ', LON*DPR() */
/*              WRITE(*,FMT1) '  Latitude   (deg): ', LAT*DPR() */
/*              WRITE(*,*) ' ' */
/*              WRITE(*,*) 'Rectangular coordinates from LATREC:' */
/*              WRITE(*,*) ' ' */
/*              WRITE(*,FMT1) '  X           (km): ', RECTAN(1) */
/*              WRITE(*,FMT1) '  Y           (km): ', RECTAN(2) */
/*              WRITE(*,FMT1) '  Z           (km): ', RECTAN(3) */
/*              WRITE(*,*) ' ' */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Original rectangular coordinates: */

/*          X           (km):      -55658.44323296 */
/*          Y           (km):     -379226.32931475 */
/*          Z           (km):     -126505.93063865 */

/*         Spherical coordinates: */

/*          Radius      (km):      403626.33912495 */
/*          Colatitude (deg):         108.26566077 */
/*          Longitude  (deg):         -98.34959789 */

/*         Latitudinal coordinates: */

/*          Radius      (km):      403626.33912495 */
/*          Longitude  (deg):         -98.34959789 */
/*          Latitude   (deg):         -18.26566077 */

/*         Rectangular coordinates from LATREC: */

/*          X           (km):      -55658.44323296 */
/*          Y           (km):     -379226.32931475 */
/*          Z           (km):     -126505.93063865 */


/*     3) Create a table showing a variety of spherical coordinates */
/*        and the corresponding cylindrical coordinates. */

/*        Corresponding spherical and cylindrical coordinates are */
/*        listed to three decimal places. Input and output angles */
/*        are in degrees. */


/*        Example code begins here. */


/*              PROGRAM SPHLAT_EX3 */
/*              IMPLICIT NONE */

/*        C */
/*        C     SPICELIB functions */
/*        C */
/*              DOUBLE PRECISION      DPR */
/*              DOUBLE PRECISION      RPD */

/*        C */
/*        C     Local parameters. */
/*        C */
/*              INTEGER               NREC */
/*              PARAMETER           ( NREC = 11 ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              DOUBLE PRECISION      COLAT  ( NREC ) */
/*              DOUBLE PRECISION      LAT */
/*              DOUBLE PRECISION      LON */
/*              DOUBLE PRECISION      R      ( NREC ) */
/*              DOUBLE PRECISION      RADIUS */
/*              DOUBLE PRECISION      RCOLAT */
/*              DOUBLE PRECISION      RSLON */
/*              DOUBLE PRECISION      SLON   ( NREC ) */

/*              INTEGER               I */

/*        C */
/*        C     Define the input spherical coordinates. Angles in */
/*        C     degrees. */
/*        C */
/*              DATA                 R      / 0.D0, 1.D0,     1.D0, */
/*             .                              1.D0, 1.4142D0, 1.D0, */
/*             .                              1.D0, 1.D0,     1.4142D0, */
/*             .                              1.D0, 0.D0               / */

/*              DATA                 COLAT /  0.D0,   90.D0,  90.D0, */
/*             .                              0.D0,   45.D0,  90.D0, */
/*             .                            180.D0,   90.D0, 135.D0, */
/*             .                              0.D0,   90.D0            / */

/*              DATA                 SLON  /  0.D0,    0.D0,  90.D0, */
/*             .                              0.D0,  180.D0, -90.D0, */
/*             .                              0.D0,   45.D0, 180.D0, */
/*             .                             180.D0,  33.D0            / */

/*        C */
/*        C     Print the banner. */
/*        C */
/*              WRITE(*,*) '     R     COLAT     SLON  ' */
/*             . //        '  RADIUS    LON      LAT   ' */
/*              WRITE(*,*) ' -------  -------  ------- ' */
/*             . //        ' -------  -------  ------- ' */

/*        C */
/*        C     Do the conversion. Output angles in degrees. */
/*        C */
/*              DO I = 1, NREC */

/*                 RCOLAT = COLAT(I) * RPD() */
/*                 RSLON  = SLON(I)  * RPD() */

/*                 CALL SPHLAT( R(I), RCOLAT, RSLON, RADIUS, LON, LAT ) */

/*                 WRITE (*,'(6F9.3)') R(I), COLAT(I), SLON(I), */
/*             .                       RADIUS, LON * DPR(), LAT * DPR() */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*              R     COLAT     SLON    RADIUS    LON      LAT */
/*          -------  -------  -------  -------  -------  ------- */
/*            0.000    0.000    0.000    0.000    0.000   90.000 */
/*            1.000   90.000    0.000    1.000    0.000    0.000 */
/*            1.000   90.000   90.000    1.000   90.000    0.000 */
/*            1.000    0.000    0.000    1.000    0.000   90.000 */
/*            1.414   45.000  180.000    1.414  180.000   45.000 */
/*            1.000   90.000  -90.000    1.000  -90.000    0.000 */
/*            1.000  180.000    0.000    1.000    0.000  -90.000 */
/*            1.000   90.000   45.000    1.000   45.000    0.000 */
/*            1.414  135.000  180.000    1.414  180.000  -45.000 */
/*            1.000    0.000  180.000    1.000  180.000   90.000 */
/*            0.000   90.000   33.000    0.000   33.000    0.000 */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 05-JUL-2021 (JDR) */

/*        Changed the argument names LONGS and LONG to SLON and LON for */
/*        consistency with other routines. */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Removed */
/*        unnecessary $Revisions section. Added complete code examples. */

/* -    SPICELIB Version 1.0.2, 26-JUL-2016 (BVS) */

/*        Minor headers edits. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) */

/* -& */
/* $ Index_Entries */

/*     spherical to latitudinal coordinates */

/* -& */

/*     SPICELIB functions */


/*     Local Variables */


/*     Convert to latitudinal coordinates, storing the results in */
/*     temporary variables */

    rr = *r__;
    lattud = halfpi_() - *colat;

/*     Move the results to the output variables. */

    *lon = *slon;
    *radius = rr;
    *lat = lattud;
    return 0;
} /* sphlat_ */

