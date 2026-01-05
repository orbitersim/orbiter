/* cylsph.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure CYLSPH ( Cylindrical to spherical ) */
/* Subroutine */ int cylsph_(doublereal *r__, doublereal *clon, doublereal *
	z__, doublereal *radius, doublereal *colat, doublereal *slon)
{
    /* System generated locals */
    doublereal d__1, d__2;

    /* Builtin functions */
    double sqrt(doublereal), atan2(doublereal, doublereal);

    /* Local variables */
    doublereal x, y, rh, th, big;

/* $ Abstract */

/*     Convert from cylindrical to spherical coordinates. */

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
/*     --------  ---  ------------------------------------------------- */
/*     R          I   Distance of point from Z axis. */
/*     CLON       I   Angle (radians) of point from XZ plane. */
/*     Z          I   Height of point above XY plane. */
/*     RADIUS     O   Distance of point from origin. */
/*     COLAT      O   Polar angle (co-latitude in radians) of point. */
/*     SLON       O   Azimuthal angle (longitude) of point (radians). */

/* $ Detailed_Input */

/*     R        is the distance of the point of interest from Z axis. */

/*     CLON     is the cylindrical angle (radians) of the point from */
/*              the XZ plane. */

/*     Z        is the height of the point above XY plane. */

/* $ Detailed_Output */

/*     RADIUS   is the distance of the point from origin. */

/*     COLAT    is the polar angle (co-latitude in radians) of the */
/*              point. The range of COLAT is [-pi, pi]. */

/*     SLON     is the azimuthal angle (longitude) of the point */
/*              (radians). SLON is set equal to CLON. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This returns the spherical coordinates of a point whose position */
/*     is input through cylindrical coordinates. */

/* $ Examples */

/*     The numerical results shown for these examples may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Compute the cylindrical coordinates of the position of the */
/*        Moon as seen from the Earth, and convert them to spherical */
/*        and rectangular coordinates. */

/*        Use the meta-kernel shown below to load the required SPICE */
/*        kernels. */


/*           KPL/MK */

/*           File name: cylsph_ex1.tm */

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


/*              PROGRAM CYLSPH_EX1 */
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
/*              DOUBLE PRECISION      COLAT */
/*              DOUBLE PRECISION      ET */
/*              DOUBLE PRECISION      LT */
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
/*              CALL FURNSH ( 'cylsph_ex1.tm' ) */

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
/*        C     Convert the cylindrical coordinates to spherical. */
/*        C */
/*              CALL CYLSPH ( R, CLON, Z, RADIUS, COLAT, SLON ) */

/*        C */
/*        C     Convert the spherical coordinates to rectangular. */
/*        C */
/*              CALL SPHREC ( RADIUS, COLAT, SLON, RECTAN ) */


/*              WRITE(*,*) ' ' */
/*              WRITE(*,*) 'Original rectangular coordinates:' */
/*              WRITE(*,*) ' ' */
/*              WRITE(*,FMT1) '  X           (km): ', POS(1) */
/*              WRITE(*,FMT1) '  Y           (km): ', POS(2) */
/*              WRITE(*,FMT1) '  Z           (km): ', POS(3) */
/*              WRITE(*,*) ' ' */
/*              WRITE(*,*) 'Cylindrical coordinates:' */
/*              WRITE(*,*) ' ' */
/*              WRITE(*,FMT1) '  Radius      (km): ', R */
/*              WRITE(*,FMT1) '  Longitude  (deg): ', CLON*DPR() */
/*              WRITE(*,FMT1) '  Z           (km): ', Z */
/*              WRITE(*,*) ' ' */
/*              WRITE(*,*) 'Spherical coordinates:' */
/*              WRITE(*,*) ' ' */
/*              WRITE(*,FMT1) '  Radius      (km): ', RADIUS */
/*              WRITE(*,FMT1) '  Colatitude (deg): ', COLAT*DPR() */
/*              WRITE(*,FMT1) '  Longitude  (deg): ', SLON*DPR() */
/*              WRITE(*,*) ' ' */
/*              WRITE(*,*) 'Rectangular coordinates from SPHREC:' */
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

/*         Cylindrical coordinates: */

/*          Radius      (km):      383289.01777726 */
/*          Longitude  (deg):         261.65040211 */
/*          Z           (km):     -126505.93063865 */

/*         Spherical coordinates: */

/*          Radius      (km):      403626.33912495 */
/*          Colatitude (deg):         108.26566077 */
/*          Longitude  (deg):         261.65040211 */

/*         Rectangular coordinates from SPHREC: */

/*          X           (km):      -55658.44323296 */
/*          Y           (km):     -379226.32931475 */
/*          Z           (km):     -126505.93063865 */


/*     2) Create a table showing a variety of cylindrical coordinates */
/*        and the corresponding spherical coordinates. */

/*        Corresponding spherical and cylindrical coordinates are */
/*        listed to three decimal places. All input and output angles */
/*        are in degrees. */


/*        Example code begins here. */


/*              PROGRAM CYLSPH_EX2 */
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
/*              DOUBLE PRECISION      COLAT */
/*              DOUBLE PRECISION      R      ( NREC ) */
/*              DOUBLE PRECISION      RADIUS */
/*              DOUBLE PRECISION      RCLON */
/*              DOUBLE PRECISION      SLON */
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
/*             . //        '  RADIUS   COLAT     SLON  ' */
/*              WRITE(*,*) ' -------  -------  ------- ' */
/*             . //        ' -------  -------  ------- ' */

/*        C */
/*        C     Do the conversion. Output angles in degrees. */
/*        C */
/*              DO I = 1, NREC */

/*                 RCLON = CLON(I) * RPD() */

/*                 CALL CYLSPH( R(I), RCLON, Z(I), RADIUS, COLAT, SLON ) */

/*                 WRITE (*,'(6F9.3)') R(I), CLON(I), Z(I), RADIUS, */
/*             .                       COLAT * DPR(), SLON  * DPR() */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*             R       CLON      Z      RADIUS   COLAT     SLON */
/*          -------  -------  -------  -------  -------  ------- */
/*            0.000    0.000    0.000    0.000    0.000    0.000 */
/*            1.000    0.000    0.000    1.000   90.000    0.000 */
/*            1.000   90.000    0.000    1.000   90.000   90.000 */
/*            0.000    0.000    1.000    1.000    0.000    0.000 */
/*            1.000  180.000    1.000    1.414   45.000  180.000 */
/*            1.000  -90.000    0.000    1.000   90.000  -90.000 */
/*            0.000    0.000   -1.000    1.000  180.000    0.000 */
/*            1.000   45.000    0.000    1.000   90.000   45.000 */
/*            1.000  180.000   -1.000    1.414  135.000  180.000 */
/*            0.000  180.000    1.000    1.000    0.000  180.000 */
/*            0.000   33.000    0.000    0.000    0.000   33.000 */


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

/* -    SPICELIB Version 1.2.0, 06-JUL-2021 (JDR) */

/*        Changed the argument names LONGC and LONG to CLON and SLON for */
/*        consistency with other routines. */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Removed */
/*        unnecessary $Revisions section. Added complete code examples. */

/* -    SPICELIB Version 1.1.1, 26-JUL-2016 (BVS) */

/*        Minor headers edits. */

/* -    SPICELIB Version 1.1.0, 30-MAR-2016 (BVS) */

/*        A cosmetic change: replaced '0.0 D0's with '0.0D0's. */
/*        Re-arranged header sections. */

/* -    SPICELIB Version 1.0.2, 22-AUG-2001 (EDW) */

/*        Corrected ENDIF to END IF. Obsolete $Revisions section */
/*        deleted. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) */

/* -& */
/* $ Index_Entries */

/*     cylindrical to spherical */

/* -& */

/*     Local variables */


/*     Convert to spherical, storing in temporary variables */

/* Computing MAX */
    d__1 = abs(*r__), d__2 = abs(*z__);
    big = max(d__1,d__2);
    if (big == 0.) {
	th = 0.;
	rh = 0.;
    } else {
	x = *r__ / big;
	y = *z__ / big;
	rh = big * sqrt(x * x + y * y);
	th = atan2(*r__, *z__);
    }

/*     Move the results to output variables */

    *slon = *clon;
    *radius = rh;
    *colat = th;
    return 0;
} /* cylsph_ */

