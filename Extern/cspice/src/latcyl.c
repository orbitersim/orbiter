/* latcyl.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure LATCYL ( Latitudinal to cylindrical coordinates ) */
/* Subroutine */ int latcyl_(doublereal *radius, doublereal *lon, doublereal *
	lat, doublereal *r__, doublereal *clon, doublereal *z__)
{
    /* Builtin functions */
    double cos(doublereal), sin(doublereal);

    /* Local variables */
    doublereal rh, zz;

/* $ Abstract */

/*     Convert from latitudinal coordinates to cylindrical coordinates. */

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
/*     LON        I   Angle of the point from the XZ plane in radians. */
/*     LAT        I   Angle of the point from the XY plane in radians. */
/*     R          O   Distance of the point from the Z axis. */
/*     CLON       O   Angle of the point from the XZ plane in radians. */
/*     Z          O   Height of the point above the XY plane. */

/* $ Detailed_Input */

/*     RADIUS   is the distance of a point from the origin. */

/*     LON      is the angle of the point from the XZ plane in */
/*              radians. */

/*     LAT      is the angle of the point from the XY plane in */
/*              radians. */

/* $ Detailed_Output */

/*     R        is the distance of the point from the Z-axis. */

/*     CLON     is the angle of the point from the XZ plane in */
/*              radians. CLON is set equal to LON. */

/*     Z        is the height of the point above the XY plane. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine returns the cylindrical coordinates of a point */
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

/*     1) Compute the latitudinal coordinates of the position of the Moon */
/*        as seen from the Earth, and convert them to cylindrical and */
/*        rectangular coordinates. */

/*        Use the meta-kernel shown below to load the required SPICE */
/*        kernels. */


/*           KPL/MK */

/*           File name: latcyl_ex1.tm */

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


/*              PROGRAM LATCYL_EX1 */
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
/*              CALL FURNSH ( 'latcyl_ex1.tm' ) */

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
/*        C     Convert the latitudinal coordinates to cylindrical. */
/*        C */
/*              CALL LATCYL ( RADIUS, LON, LAT, R, CLON, Z ) */

/*        C */
/*        C     Convert the cylindrical coordinates to rectangular. */
/*        C */
/*              CALL CYLREC ( R, CLON, Z, RECTAN ) */


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
/*              WRITE(*,*) 'Cylindrical coordinates:' */
/*              WRITE(*,*) ' ' */
/*              WRITE(*,FMT1) '  Radius     (km): ', R */
/*              WRITE(*,FMT1) '  Longitude (deg): ', CLON*DPR() */
/*              WRITE(*,FMT1) '  Z          (km): ', Z */
/*              WRITE(*,*) ' ' */
/*              WRITE(*,*) 'Rectangular coordinates from CYLREC:' */
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

/*         Cylindrical coordinates: */

/*          Radius     (km):      383289.01777726 */
/*          Longitude (deg):         -98.34959789 */
/*          Z          (km):     -126505.93063865 */

/*         Rectangular coordinates from CYLREC: */

/*          X          (km):      -55658.44323296 */
/*          Y          (km):     -379226.32931475 */
/*          Z          (km):     -126505.93063865 */


/*     2) Create a table showing a variety of latitudinal coordinates */
/*        and the corresponding cylindrical coordinates. */

/*        Corresponding latitudinal and cylindrical coordinates are */
/*        listed to three decimal places. Input and output angles are */
/*        in degrees. */


/*        Example code begins here. */


/*              PROGRAM LATCYL_EX2 */
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
/*              DOUBLE PRECISION      CLON */
/*              DOUBLE PRECISION      LAT    ( NREC ) */
/*              DOUBLE PRECISION      LON    ( NREC ) */
/*              DOUBLE PRECISION      RADIUS ( NREC ) */
/*              DOUBLE PRECISION      R */
/*              DOUBLE PRECISION      RLAT */
/*              DOUBLE PRECISION      RLON */
/*              DOUBLE PRECISION      Z */

/*              INTEGER               I */

/*        C */
/*        C     Define the input latitudinal coordinates. Angles in */
/*        C     degrees. */
/*        C */

/*              DATA                 RADIUS / 0.D0, 1.D0,     1.D0, */
/*             .                              1.D0, 1.4142D0, 1.D0, */
/*             .                              1.D0, 1.D0,     1.4142D0, */
/*             .                              1.D0, 0.D0               / */

/*              DATA                 LON   /  0.D0,    0.D0,  90.D0, */
/*             .                              0.D0,  180.D0, -90.D0, */
/*             .                              0.D0,   45.D0, 180.D0, */
/*             .                             180.D0,  33.D0            / */

/*              DATA                 LAT   / 90.D0,    0.D0,   0.D0, */
/*             .                             90.D0,   45.D0,   0.D0, */
/*             .                            -90.D0,    0.D0, -45.D0, */
/*             .                             90.D0,    0.D0            / */

/*        C */
/*        C     Print the banner. */
/*        C */
/*              WRITE(*,*) '  RADIUS    LON      LAT   ' */
/*             . //        '    R       CLON      Z    ' */
/*              WRITE(*,*) ' -------  -------  ------- ' */
/*             . //        ' -------  -------  ------- ' */

/*        C */
/*        C     Do the conversion. Output angles in degrees. */
/*        C */
/*              DO I = 1, NREC */

/*                 RLON = LON(I) * RPD() */
/*                 RLAT = LAT(I) * RPD() */

/*                 CALL LATCYL( RADIUS(I), RLON, RLAT, R, CLON, Z ) */

/*                 WRITE (*,'(6F9.3)') RADIUS(I), LON(I), LAT(I), */
/*             .                       R, CLON * DPR(), Z */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*           RADIUS    LON      LAT       R       CLON      Z */
/*          -------  -------  -------  -------  -------  ------- */
/*            0.000    0.000   90.000    0.000    0.000    0.000 */
/*            1.000    0.000    0.000    1.000    0.000    0.000 */
/*            1.000   90.000    0.000    1.000   90.000    0.000 */
/*            1.000    0.000   90.000    0.000    0.000    1.000 */
/*            1.414  180.000   45.000    1.000  180.000    1.000 */
/*            1.000  -90.000    0.000    1.000  -90.000    0.000 */
/*            1.000    0.000  -90.000    0.000    0.000   -1.000 */
/*            1.000   45.000    0.000    1.000   45.000    0.000 */
/*            1.414  180.000  -45.000    1.000  180.000   -1.000 */
/*            1.000  180.000   90.000    0.000  180.000    1.000 */
/*            0.000   33.000    0.000    0.000   33.000    0.000 */


/*     3) Other than the obvious conversion between coordinate systems */
/*        this routine could be used to obtain the axial projection */
/*        from a sphere to a cylinder about the z-axis that contains */
/*        the equator of the sphere. */

/*        Such a projection is valuable because it preserves the */
/*        areas between regions on the sphere and their projections to */
/*        the cylinder. */


/*        Example code begins here. */


/*              PROGRAM LATCYL_EX3 */
/*              IMPLICIT NONE */

/*        C */
/*        C     SPICELIB functions */
/*        C */
/*              DOUBLE PRECISION      DPR */
/*              DOUBLE PRECISION      RPD */

/*        C */
/*        C     Local parameters */
/*        C */
/*              CHARACTER*(*)         FMT1 */
/*              PARAMETER           ( FMT1 = '(A,F23.11)' ) */

/*        C */
/*        C     Local variables */
/*        C */
/*              DOUBLE PRECISION      CLON */
/*              DOUBLE PRECISION      LAT */
/*              DOUBLE PRECISION      LON */
/*              DOUBLE PRECISION      RADIUS */
/*              DOUBLE PRECISION      R */
/*              DOUBLE PRECISION      Z */

/*        C */
/*        C     Define the point whose projection is to be */
/*        C     computed. */
/*        C */
/*              RADIUS =  100.D0 */
/*              LON    =   45.D0  * RPD() */
/*              LAT    =  -12.5D0 * RPD() */

/*        C */
/*        C     Convert the latitudinal coordinates to cylindrical. */
/*        C */
/*              CALL LATCYL ( RADIUS, LON, LAT, R, CLON, Z ) */

/*              WRITE(*,*) 'Coordinates of the projected point on ' */
/*             .        // 'cylinder:' */
/*              WRITE(*,*) ' ' */
/*              WRITE(*,FMT1) '  Radius     (km): ', R */
/*              WRITE(*,FMT1) '  Longitude (deg): ', CLON*DPR() */
/*              WRITE(*,FMT1) '  Z          (km): ', Z */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Coordinates of the projected point on cylinder: */

/*          Radius     (km):          97.62960071199 */
/*          Longitude (deg):          45.00000000000 */
/*          Z          (km):         -21.64396139381 */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR) */

/*        Changed the argument names LONG and LONGC to LON and CLON for */
/*        consistency with other routines. */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Removed */
/*        unnecessary $Revisions section. */

/*        Added complete code examples. */

/* -    SPICELIB Version 1.0.2, 26-JUL-2016 (BVS) */

/*        Minor headers edits. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) */

/* -& */
/* $ Index_Entries */

/*     latitudinal to cylindrical coordinates */

/* -& */

/*     Local variables */


/*     Convert to cylindrical, storing in temporary variables */

    rh = *radius * cos(*lat);
    zz = *radius * sin(*lat);

/*     Move the results to output variables. */

    *clon = *lon;
    *r__ = rh;
    *z__ = zz;
    return 0;
} /* latcyl_ */

