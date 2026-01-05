/* cyllat.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure CYLLAT ( Cylindrical to latitudinal ) */
/* Subroutine */ int cyllat_(doublereal *r__, doublereal *clon, doublereal *
	z__, doublereal *radius, doublereal *lon, doublereal *lat)
{
    /* System generated locals */
    doublereal d__1, d__2;

    /* Builtin functions */
    double sqrt(doublereal), atan2(doublereal, doublereal);

    /* Local variables */
    doublereal x, y, lattud, big, rho;

/* $ Abstract */

/*     Convert from cylindrical to latitudinal coordinates. */

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
/*     R          I   Distance of point from Z axis. */
/*     CLON       I   Cylindrical angle of point from XZ plane(radians). */
/*     Z          I   Height of point above XY plane. */
/*     RADIUS     O   Distance of point from origin. */
/*     LON        O   Longitude of point (radians). */
/*     LAT        O   Latitude of point (radians). */

/* $ Detailed_Input */

/*     R        is the distance of the input point from Z axis. */

/*     CLON     is the cylindrical angle of the point from XZ plane */
/*              (radians). */

/*     Z        is the height of the point above XY plane. */

/* $ Detailed_Output */

/*     RADIUS   is the distance of the input point from origin. */

/*     LON      is the longitude (i.e. angle from the XZ plane) of */
/*              the input point (radians). LON is set equal to CLON. */

/*     LAT      is the latitude (i.e. angle above the XY plane) of the */
/*              input point (radians). The range of LAT is [-pi, pi]. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine converts coordinates given in cylindrical */
/*     coordinates to coordinates in latitudinal coordinates. */

/*     Latitudinal coordinates are defined by a distance from a central */
/*     reference point, an angle from a reference meridian, and an angle */
/*     above the equator of a sphere centered at the central reference */
/*     point. */

/* $ Examples */

/*     The numerical results shown for these examples may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Compute the cylindrical coordinates of the position of the */
/*        Moon as seen from the Earth, and convert them to latitudinal */
/*        and rectangular coordinates. */

/*        Use the meta-kernel shown below to load the required SPICE */
/*        kernels. */


/*           KPL/MK */

/*           File name: cyllat_ex1.tm */

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


/*              PROGRAM CYLLAT_EX1 */
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
/*              DOUBLE PRECISION      CLON */
/*              DOUBLE PRECISION      ET */
/*              DOUBLE PRECISION      LAT */
/*              DOUBLE PRECISION      LON */
/*              DOUBLE PRECISION      LT */
/*              DOUBLE PRECISION      POS    ( 3 ) */
/*              DOUBLE PRECISION      RADIUS */
/*              DOUBLE PRECISION      RECTAN ( 3 ) */
/*              DOUBLE PRECISION      R */
/*              DOUBLE PRECISION      Z */

/*        C */
/*        C     Load SPK and LSK kernels, use a meta kernel for */
/*        C     convenience. */
/*        C */
/*              CALL FURNSH ( 'cyllat_ex1.tm' ) */

/*        C */
/*        C     Look up the geometric state of the Moon as seen from */
/*        C     the Earth at 2017 Mar 20, relative to the J2000 */
/*        C     reference frame. */
/*        C */
/*              CALL STR2ET ( '2017 Mar 20', ET ) */

/*              CALL SPKPOS ( 'Moon',  ET,  'J2000', 'NONE', */
/*             .              'Earth', POS, LT               ) */

/*        C */
/*        C     Convert the position vector POS to cylindrical */
/*        C     coordinates. */
/*        C */
/*              CALL RECCYL ( POS, R, CLON, Z ) */

/*        C */
/*        C     Convert the cylindrical coordinates to latitudinal. */
/*        C */
/*              CALL CYLLAT ( R, CLON, Z, RADIUS, LON, LAT ) */

/*        C */
/*        C     Convert the latitudinal coordinates to rectangular. */
/*        C */
/*              CALL LATREC ( RADIUS, LON, LAT, RECTAN ) */


/*              WRITE(*,*) ' ' */
/*              WRITE(*,*) 'Original rectangular coordinates:' */
/*              WRITE(*,*) ' ' */
/*              WRITE(*,FMT1) '  X          (km): ', POS(1) */
/*              WRITE(*,FMT1) '  Y          (km): ', POS(2) */
/*              WRITE(*,FMT1) '  Z          (km): ', POS(3) */
/*              WRITE(*,*) ' ' */
/*              WRITE(*,*) 'Cylindrical coordinates:' */
/*              WRITE(*,*) ' ' */
/*              WRITE(*,FMT1) '  Radius     (km): ', R */
/*              WRITE(*,FMT1) '  Longitude (deg): ', CLON*DPR() */
/*              WRITE(*,FMT1) '  Z          (km): ', Z */
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

/*         Cylindrical coordinates: */

/*          Radius     (km):      383289.01777726 */
/*          Longitude (deg):         261.65040211 */
/*          Z          (km):     -126505.93063865 */

/*         Latitudinal coordinates: */

/*          Radius     (km):      403626.33912495 */
/*          Longitude (deg):         261.65040211 */
/*          Latitude  (deg):         -18.26566077 */

/*         Rectangular coordinates from LATREC: */

/*          X          (km):      -55658.44323296 */
/*          Y          (km):     -379226.32931475 */
/*          Z          (km):     -126505.93063865 */


/*     2) Create a table showing a variety of cylindrical coordinates */
/*        and the corresponding latitudinal coordinates. */

/*        Corresponding latitudinal and cylindrical coordinates are */
/*        listed to three decimal places. All input and output angles */
/*        are in degrees. */


/*        Example code begins here. */


/*              PROGRAM CYLLAT_EX2 */
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
/*              DOUBLE PRECISION      CLON   ( NREC ) */
/*              DOUBLE PRECISION      LAT */
/*              DOUBLE PRECISION      LON */
/*              DOUBLE PRECISION      R      ( NREC ) */
/*              DOUBLE PRECISION      RADIUS */
/*              DOUBLE PRECISION      RCLON */
/*              DOUBLE PRECISION      Z      ( NREC ) */

/*              INTEGER               I */

/*        C */
/*        C     Define the input cylindrical coordinates. Angles */
/*        C     in degrees. */
/*        C */

/*              DATA                 R    /  0.D0, 1.D0, 1.D0, */
/*             .                             0.D0, 1.D0, 1.D0, */
/*             .                             0.D0, 1.D0, 1.D0, */
/*             .                             0.D0, 0.D0           / */

/*              DATA                 CLON /  0.D0,   0.D0,  90.D0, */
/*             .                             0.D0, 180.D0, -90.D0, */
/*             .                             0.D0,  45.D0, 180.D0, */
/*             .                           180.D0,  33.D0         / */

/*              DATA                 Z    /  0.D0,   0.D0,  0.D0, */
/*             .                             1.D0,   1.D0,  0.D0, */
/*             .                            -1.D0,   0.D0, -1.D0, */
/*             .                             1.D0,   0.D0         / */

/*        C */
/*        C     Print the banner. */
/*        C */
/*              WRITE(*,*) '    R       CLON      Z    ' */
/*             . //        '  RADIUS     LON     LAT   ' */
/*              WRITE(*,*) ' -------  -------  ------- ' */
/*             . //        ' -------  -------  ------- ' */

/*        C */
/*        C     Do the conversion. Output angles in degrees. */
/*        C */
/*              DO I = 1, NREC */

/*                 RCLON = CLON(I) * RPD() */

/*                 CALL CYLLAT( R(I), RCLON, Z(I), RADIUS, LON, LAT ) */

/*                 WRITE (*,'(6F9.3)') R(I), CLON(I), Z(I), */
/*             .                       RADIUS, LON * DPR(), LAT * DPR() */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*             R       CLON      Z      RADIUS     LON     LAT */
/*          -------  -------  -------  -------  -------  ------- */
/*            0.000    0.000    0.000    0.000    0.000    0.000 */
/*            1.000    0.000    0.000    1.000    0.000    0.000 */
/*            1.000   90.000    0.000    1.000   90.000    0.000 */
/*            0.000    0.000    1.000    1.000    0.000   90.000 */
/*            1.000  180.000    1.000    1.414  180.000   45.000 */
/*            1.000  -90.000    0.000    1.000  -90.000    0.000 */
/*            0.000    0.000   -1.000    1.000    0.000  -90.000 */
/*            1.000   45.000    0.000    1.000   45.000    0.000 */
/*            1.000  180.000   -1.000    1.414  180.000  -45.000 */
/*            0.000  180.000    1.000    1.000  180.000   90.000 */
/*            0.000   33.000    0.000    0.000   33.000    0.000 */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR) */

/*        Changed the argument names LONGC and LONG to CLON and LON for */
/*        consistency with other routines. */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Removed */
/*        unnecessary $Revisions section. Added complete code examples. */

/* -    SPICELIB Version 1.0.3, 26-JUL-2016 (BVS) */

/*        Minor headers edits. */

/* -    SPICELIB Version 1.0.2, 22-AUG-2001 (EDW) */

/*        Corrected ENDIF to END IF. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) */

/* -& */
/* $ Index_Entries */

/*     cylindrical to latitudinal */

/* -& */

/*     Local Variables */


/*     Convert the input cylindrical coordinates to latitudinal */
/*     coordinates, storing in temporary variables. */

/* Computing MAX */
    d__1 = abs(*r__), d__2 = abs(*z__);
    big = max(d__1,d__2);
    if (big > 0.) {
	x = *r__ / big;
	y = *z__ / big;
	rho = big * sqrt(x * x + y * y);
    } else {
	rho = 0.;
    }
    if (rho == 0.) {
	lattud = 0.;
    } else {
	lattud = atan2(*z__, *r__);
    }

/*     Move results to output variables */

    *lon = *clon;
    *radius = rho;
    *lat = lattud;

    return 0;
} /* cyllat_ */

