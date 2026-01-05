/* sphrec.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure SPHREC ( Spherical to rectangular coordinates ) */
/* Subroutine */ int sphrec_(doublereal *r__, doublereal *colat, doublereal *
	slon, doublereal *rectan)
{
    /* Builtin functions */
    double cos(doublereal), sin(doublereal);

    /* Local variables */
    doublereal x, y, z__;

/* $ Abstract */

/*     Convert from spherical coordinates to rectangular coordinates. */

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
/*     R          I   Distance of a point from the origin. */
/*     COLAT      I   Angle of the point from the Z-axis in radians. */
/*     SLON       I   Angle of the point from the XZ plane in radians. */
/*     RECTAN     O   Rectangular coordinates of the point. */

/* $ Detailed_Input */

/*     R        is the distance of the point from the origin. */

/*     COLAT    is the angle between the point and the positive */
/*              Z-axis in radians. */

/*     SLON     is the angle of the projection of the point to the */
/*              XY plane from the positive X-axis in radians. The */
/*              positive Y-axis is at longitude PI/2 radians. */

/* $ Detailed_Output */

/*     RECTAN   are the rectangular coordinates of a point. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine returns the rectangular coordinates of a point */
/*     whose position is input in spherical coordinates. */

/*     Spherical coordinates are defined by a distance from a central */
/*     reference point, an angle from a reference meridian, and an angle */
/*     from the Z-axis. The co-latitude of the positive Z-axis is */
/*     zero. The longitude of the positive Y-axis is PI/2 radians. */

/* $ Examples */

/*     The numerical results shown for these examples may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Compute the spherical coordinates of the position of the Moon */
/*        as seen from the Earth, and convert them to rectangular */
/*        coordinates. */

/*        Use the meta-kernel shown below to load the required SPICE */
/*        kernels. */


/*           KPL/MK */

/*           File name: sphrec_ex1.tm */

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


/*              PROGRAM SPHREC_EX1 */
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
/*              CALL FURNSH ( 'sphrec_ex1.tm' ) */

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
/*              CALL RECSPH ( POS, RADIUS, COLAT, SLON ) */

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

/*         Spherical coordinates: */

/*          Radius      (km):      403626.33912495 */
/*          Colatitude (deg):         108.26566077 */
/*          Longitude  (deg):         -98.34959789 */

/*         Rectangular coordinates from SPHREC: */

/*          X           (km):      -55658.44323296 */
/*          Y           (km):     -379226.32931475 */
/*          Z           (km):     -126505.93063865 */


/*     2) Create a table showing a variety of spherical coordinates */
/*        and the corresponding rectangular coordinates. */

/*        Corresponding spherical and rectangular coordinates are */
/*        listed to three decimal places. Input angles are in degrees. */


/*        Example code begins here. */


/*              PROGRAM SPHREC_EX2 */
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
/*              DOUBLE PRECISION      RADIUS ( NREC ) */
/*              DOUBLE PRECISION      RCOLAT */
/*              DOUBLE PRECISION      RSLON */
/*              DOUBLE PRECISION      SLON   ( NREC ) */
/*              DOUBLE PRECISION      RECTAN ( 3    ) */

/*              INTEGER               I */

/*        C */
/*        C     Define the input spherical coordinates. Angles in */
/*        C     degrees. */
/*        C */
/*              DATA                 RADIUS / 0.D0, 1.D0,         1.D0, */
/*             .                              1.D0, 1.D0,         1.D0, */
/*             .                              1.D0, 1.4142D0, 1.4142D0, */
/*             .                          1.4142D0, 1.7320D0           / */

/*              DATA                 COLAT /  0.D0,   90.D0,  90.D0, */
/*             .                              0.D0,   90.D0,  90.D0, */
/*             .                            180.D0,   90.D0,  45.D0, */
/*             .                             45.D0,   54.7356D0        / */

/*              DATA                 SLON  /  0.D0,    0.D0,  90.D0, */
/*             .                              0.D0,  180.D0, -90.D0, */
/*             .                              0.D0,   45.D0,   0.D0, */
/*             .                              90.D0,  45.D0            / */

/*        C */
/*        C     Print the banner. */
/*        C */
/*              WRITE(*,*) '  RADIUS   COLAT     SLON  ' */
/*             . //        ' RECT(1)  RECT(2)  RECT(3) ' */
/*              WRITE(*,*) ' -------  -------  ------- ' */
/*             . //        ' -------  -------  ------- ' */

/*        C */
/*        C     Do the conversion. */
/*        C */
/*              DO I = 1, NREC */

/*                 RCOLAT = COLAT(I) * RPD() */
/*                 RSLON  = SLON(I)  * RPD() */

/*                 CALL SPHREC( RADIUS(I), RCOLAT, RSLON, RECTAN ) */

/*                 WRITE (*,'(6F9.3)') RADIUS(I), COLAT(I), SLON(I), */
/*             .                       RECTAN */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*           RADIUS   COLAT     SLON   RECT(1)  RECT(2)  RECT(3) */
/*          -------  -------  -------  -------  -------  ------- */
/*            0.000    0.000    0.000    0.000    0.000    0.000 */
/*            1.000   90.000    0.000    1.000    0.000    0.000 */
/*            1.000   90.000   90.000    0.000    1.000    0.000 */
/*            1.000    0.000    0.000    0.000    0.000    1.000 */
/*            1.000   90.000  180.000   -1.000    0.000    0.000 */
/*            1.000   90.000  -90.000    0.000   -1.000    0.000 */
/*            1.000  180.000    0.000    0.000    0.000   -1.000 */
/*            1.414   90.000   45.000    1.000    1.000    0.000 */
/*            1.414   45.000    0.000    1.000    0.000    1.000 */
/*            1.414   45.000   90.000    0.000    1.000    1.000 */
/*            1.732   54.736   45.000    1.000    1.000    1.000 */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 13-AUG-2021 (JDR) */

/*        Changed the argument name LONG to SLON for consistency with */
/*        other routines. */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Removed */
/*        unnecessary $Revisions section. Added complete code examples. */

/* -    SPICELIB Version 1.0.4, 26-JUL-2016 (BVS) */

/*        Minor headers edits. */

/* -    SPICELIB Version 1.0.3, 24-SEP-1997 (WLT) */

/*        The BRIEF I/O section was corrected so that it */
/*        correctly reflects the inputs and outputs. */

/* -    SPICELIB Version 1.0.2, 12-JUL-1995 (WLT) */

/*        The header documentation was corrected so that longitude */
/*        now is correctly described as the angle from the */
/*        XZ plane instead of XY. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) */

/* -& */
/* $ Index_Entries */

/*     spherical to rectangular coordinates */

/* -& */

/*     Local Variables */


/*     Convert to rectangular coordinates, storing in the results in */
/*     temporary variables */

    x = *r__ * cos(*slon) * sin(*colat);
    y = *r__ * sin(*slon) * sin(*colat);
    z__ = *r__ * cos(*colat);

/*     Move the results to the output variables */

    rectan[0] = x;
    rectan[1] = y;
    rectan[2] = z__;
    return 0;
} /* sphrec_ */

