/* latrec.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure LATREC ( Latitudinal to rectangular coordinates ) */
/* Subroutine */ int latrec_(doublereal *radius, doublereal *lon, doublereal *
	lat, doublereal *rectan)
{
    /* Builtin functions */
    double cos(doublereal), sin(doublereal);

    /* Local variables */
    doublereal x, y, z__;

/* $ Abstract */

/*     Convert from latitudinal coordinates to rectangular coordinates. */

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
/*     RADIUS     I   Distance of a point from the origin. */
/*     LON        I   Longitude of point in radians. */
/*     LAT        I   Latitude of point in radians. */
/*     RECTAN     O   Rectangular coordinates of the point. */

/* $ Detailed_Input */

/*     RADIUS   is the distance of a point from the origin. */

/*     LON      is the Longitude of the input point. This is the */
/*              angle between the prime meridian and the meridian */
/*              containing the point. The direction of increasing */
/*              longitude is from the +X axis towards the +Y axis. */

/*              Longitude is measured in radians. On input, the */
/*              range of longitude is unrestricted. */

/*     LAT      is the latitude of the input point. This is the angle */
/*              from the XY plane of the ray from the origin through */
/*              the point. */

/*              Latitude is measured in radians. On input, the range */
/*              of latitude is unrestricted. */

/* $ Detailed_Output */

/*     RECTAN   are the rectangular coordinates of the input point. */
/*              RECTAN is a 3-vector. */

/*              The units associated with RECTAN are those */
/*              associated with the input RADIUS. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine returns the rectangular coordinates of a point */
/*     whose position is input in latitudinal coordinates. */

/*     Latitudinal coordinates are defined by a distance from a central */
/*     reference point, an angle from a reference meridian, and an angle */
/*     above the equator of a sphere centered at the central reference */
/*     point. */

/* $ Examples */

/*     The numerical results shown for these examples may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Compute the latitudinal coordinates of the position of the */
/*        Moon as seen from the Earth, and convert them to rectangular */
/*        coordinates. */

/*        Use the meta-kernel shown below to load the required SPICE */
/*        kernels. */


/*           KPL/MK */

/*           File name: latrec_ex1.tm */

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


/*              PROGRAM LATREC_EX1 */
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
/*              DOUBLE PRECISION      ET */
/*              DOUBLE PRECISION      LAT */
/*              DOUBLE PRECISION      LON */
/*              DOUBLE PRECISION      LT */
/*              DOUBLE PRECISION      POS    ( 3 ) */
/*              DOUBLE PRECISION      RADIUS */
/*              DOUBLE PRECISION      RECTAN ( 3 ) */

/*        C */
/*        C     Load SPK and LSK kernels, use a meta kernel for */
/*        C     convenience. */
/*        C */
/*              CALL FURNSH ( 'latrec_ex1.tm' ) */

/*        C */
/*        C     Look up the geometric state of the Moon as seen from */
/*        C     the Earth at 2017 Mar 20, relative to the J2000 */
/*        C     reference frame. */
/*        C */
/*              CALL STR2ET ( '2017 Mar 20', ET ) */

/*              CALL SPKPOS ( 'Moon',  ET,  'J2000', 'NONE', */
/*             .              'Earth', POS, LT               ) */

/*        C */
/*        C     Convert the position vector POS to latitudinal */
/*        C     coordinates. */
/*        C */
/*              CALL RECLAT ( POS, RADIUS, LON, LAT ) */

/*        C */
/*        C     Convert the latitudinal to rectangular coordinates. */
/*        C */

/*              CALL LATREC ( RADIUS, LON, LAT, RECTAN ) */

/*              WRITE(*,*) ' ' */
/*              WRITE(*,*) 'Original rectangular coordinates:' */
/*              WRITE(*,*) ' ' */
/*              WRITE(*,FMT1) '  X          (km): ', POS(1) */
/*              WRITE(*,FMT1) '  Y          (km): ', POS(2) */
/*              WRITE(*,FMT1) '  Z          (km): ', POS(3) */
/*              WRITE(*,*) ' ' */
/*              WRITE(*,*) 'Latitudinal coordinates:' */
/*              WRITE(*,*) ' ' */
/*              WRITE(*,FMT1) '  Radius     (km): ', RADIUS */
/*              WRITE(*,FMT1) '  Longitude (deg): ', LON*DPR() */
/*              WRITE(*,FMT1) '  Latitude  (deg): ', LAT*DPR() */
/*              WRITE(*,*) ' ' */
/*              WRITE(*,*) 'Rectangular coordinates from LATREC:' */
/*              WRITE(*,*) ' ' */
/*              WRITE(*,FMT1) '  X          (km): ', RECTAN(1) */
/*              WRITE(*,FMT1) '  Y          (km): ', RECTAN(2) */
/*              WRITE(*,FMT1) '  Z          (km): ', RECTAN(3) */
/*              WRITE(*,*) ' ' */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Original rectangular coordinates: */

/*          X          (km):      -55658.44323296 */
/*          Y          (km):     -379226.32931475 */
/*          Z          (km):     -126505.93063865 */

/*         Latitudinal coordinates: */

/*          Radius     (km):      403626.33912495 */
/*          Longitude (deg):         -98.34959789 */
/*          Latitude  (deg):         -18.26566077 */

/*         Rectangular coordinates from LATREC: */

/*          X          (km):      -55658.44323296 */
/*          Y          (km):     -379226.32931475 */
/*          Z          (km):     -126505.93063865 */


/*     2) Create a table showing a variety of latitudinal coordinates */
/*        and the corresponding rectangular coordinates. */

/*        Corresponding latitudinal and rectangular coordinates are */
/*        listed to three decimal places. Input angles are in degrees. */


/*        Example code begins here. */


/*              PROGRAM LATREC_EX2 */
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
/*              DOUBLE PRECISION      LAT    ( NREC ) */
/*              DOUBLE PRECISION      LON    ( NREC ) */
/*              DOUBLE PRECISION      RADIUS ( NREC ) */
/*              DOUBLE PRECISION      RECTAN ( 3    ) */
/*              DOUBLE PRECISION      RLAT */
/*              DOUBLE PRECISION      RLON */

/*              INTEGER               I */

/*        C */
/*        C     Define the input latitudinal coordinates. Angles in */
/*        C     degrees. */
/*        C */

/*              DATA                 RADIUS / 0.D0,    1.D0,      1.D0, */
/*             .                              1.D0,     1.D0,     1.D0, */
/*             .                              1.D0, 1.4142D0, 1.4142D0, */
/*             .                          1.4142D0, 1.732D0            / */

/*              DATA                 LON    / 0.D0,    0.D0,  90.D0, */
/*             .                              0.D0,  180.D0, -90.D0, */
/*             .                              0.D0,   45.D0,   0.D0, */
/*             .                             90.D0,   45.D0            / */

/*              DATA                 LAT    / 0.D0,     0.D0,   0.D0, */
/*             .                             90.D0,     0.D0,   0.D0, */
/*             .                            -90.D0,     0.D0,  45.D0, */
/*             .                             45.D0, 35.264D0           / */

/*        C */
/*        C     Print the banner. */
/*        C */
/*              WRITE(*,*) '  RADIUS    LON      LAT   ' */
/*             . //        ' RECT(1)  RECT(2)  RECT(3) ' */
/*              WRITE(*,*) ' -------  -------  ------- ' */
/*             . //        ' -------  -------  ------- ' */

/*        C */
/*        C     Do the conversion. */
/*        C */
/*              DO I = 1, NREC */

/*                 RLON = LON(I) * RPD() */
/*                 RLAT = LAT(I) * RPD() */

/*                 CALL LATREC( RADIUS(I), RLON, RLAT, RECTAN ) */

/*                 WRITE (*,'(6F9.3)') RADIUS(I), LON(I), LAT(I), */
/*             .                       RECTAN */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*           RADIUS    LON      LAT    RECT(1)  RECT(2)  RECT(3) */
/*          -------  -------  -------  -------  -------  ------- */
/*            0.000    0.000    0.000    0.000    0.000    0.000 */
/*            1.000    0.000    0.000    1.000    0.000    0.000 */
/*            1.000   90.000    0.000    0.000    1.000    0.000 */
/*            1.000    0.000   90.000    0.000    0.000    1.000 */
/*            1.000  180.000    0.000   -1.000    0.000    0.000 */
/*            1.000  -90.000    0.000    0.000   -1.000    0.000 */
/*            1.000    0.000  -90.000    0.000    0.000   -1.000 */
/*            1.414   45.000    0.000    1.000    1.000    0.000 */
/*            1.414    0.000   45.000    1.000    0.000    1.000 */
/*            1.414   90.000   45.000    0.000    1.000    1.000 */
/*            1.732   45.000   35.264    1.000    1.000    1.000 */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     C.H. Acton         (JPL) */
/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR) */

/*        Changed the input argument name LONG to LON for consistency */
/*        with other routines. */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Removed */
/*        unnecessary $Revisions section. */

/*        Added complete code examples. */

/* -    SPICELIB Version 1.0.2, 29-JUL-2003 (NJB) (CHA) */

/*        Various header changes were made to improve clarity. Some */
/*        minor header corrections were made. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) */

/* -& */
/* $ Index_Entries */

/*     latitudinal to rectangular coordinates */

/* -& */

/*     Local variables */


/*     Convert to rectangular coordinates, storing the results in */
/*     temporary variables. */

    x = *radius * cos(*lon) * cos(*lat);
    y = *radius * sin(*lon) * cos(*lat);
    z__ = *radius * sin(*lat);

/*  Move the results to the output variables. */

    rectan[0] = x;
    rectan[1] = y;
    rectan[2] = z__;
    return 0;
} /* latrec_ */

