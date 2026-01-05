/* drdsph.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure DRDSPH ( Derivative of rectangular w.r.t. spherical ) */
/* Subroutine */ int drdsph_(doublereal *r__, doublereal *colat, doublereal *
	slon, doublereal *jacobi)
{
    /* Builtin functions */
    double cos(doublereal), sin(doublereal);

    /* Local variables */
    doublereal clong, slong, ccolat, scolat;

/* $ Abstract */

/*     Compute the Jacobian matrix of the transformation from spherical */
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
/*     R          I   Distance of a point from the origin. */
/*     COLAT      I   Angle of the point from the positive Z-axis. */
/*     SLON       I   Angle of the point from the XY plane. */
/*     JACOBI     O   Matrix of partial derivatives. */

/* $ Detailed_Input */

/*     R        is the distance of a point from the origin. */

/*     COLAT    is the angle between the point and the positive */
/*              Z-axis, in radians. */

/*     SLON     is the angle of the point from the XZ plane in */
/*              radians. The angle increases in the counterclockwise */
/*              sense about the +Z axis. */

/* $ Detailed_Output */

/*     JACOBI   is the matrix of partial derivatives of the conversion */
/*              between spherical and rectangular coordinates, */
/*              evaluated at the input coordinates. This matrix has */
/*              the form */

/*                  .-                                   -. */
/*                  |  DX/DR     DX/DCOLAT     DX/DSLON   | */
/*                  |                                     | */
/*                  |  DY/DR     DY/DCOLAT     DY/DSLON   | */
/*                  |                                     | */
/*                  |  DZ/DR     DZ/DCOLAT     DZ/DSLON   | */
/*                  `-                                   -' */

/*               evaluated at the input values of R, SLON and LAT. */
/*               Here X, Y, and Z are given by the familiar formulae */

/*                 X = R*COS(SLON)*SIN(COLAT) */
/*                 Y = R*SIN(SLON)*SIN(COLAT) */
/*                 Z = R*COS(COLAT) */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     It is often convenient to describe the motion of an object in */
/*     the spherical coordinate system. However, when performing */
/*     vector computations its hard to beat rectangular coordinates. */

/*     To transform states given with respect to spherical coordinates */
/*     to states with respect to rectangular coordinates, one makes use */
/*     of the Jacobian of the transformation between the two systems. */

/*     Given a state in spherical coordinates */

/*          ( r, colat, slon, dr, dcolat, dslon ) */

/*     the velocity in rectangular coordinates is given by the matrix */
/*     equation: */
/*                    t          |                                   t */
/*        (dx, dy, dz)   = JACOBI|              * (dr, dcolat, dslon) */
/*                               |(r,colat,slon) */

/*     This routine computes the matrix */

/*              | */
/*        JACOBI| */
/*              |(r,colat,slon) */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Find the spherical state of the Earth as seen from */
/*        Mars in the IAU_MARS reference frame at January 1, 2005 TDB. */
/*        Map this state back to rectangular coordinates as a check. */

/*        Use the meta-kernel shown below to load the required SPICE */
/*        kernels. */


/*           KPL/MK */

/*           File name: drdsph_ex1.tm */

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


/*              PROGRAM DRDSPH_EX1 */
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
/*              DOUBLE PRECISION      COLAT */
/*              DOUBLE PRECISION      DRECTN ( 3 ) */
/*              DOUBLE PRECISION      ET */
/*              DOUBLE PRECISION      JACOBI ( 3, 3 ) */
/*              DOUBLE PRECISION      LT */
/*              DOUBLE PRECISION      SPHVEL ( 3 ) */
/*              DOUBLE PRECISION      RECTAN ( 3 ) */
/*              DOUBLE PRECISION      R */
/*              DOUBLE PRECISION      SLON */
/*              DOUBLE PRECISION      STATE  ( 6 ) */

/*        C */
/*        C     Load SPK, PCK and LSK kernels, use a meta kernel for */
/*        C     convenience. */
/*        C */
/*              CALL FURNSH ( 'drdsph_ex1.tm' ) */

/*        C */
/*        C     Look up the apparent state of earth as seen from Mars at */
/*        C     January 1, 2005 TDB, relative to the IAU_MARS reference */
/*        C     frame. */
/*        C */
/*              CALL STR2ET ( 'January 1, 2005 TDB', ET ) */

/*              CALL SPKEZR ( 'Earth', ET,    'IAU_MARS', 'LT+S', */
/*             .              'Mars',  STATE, LT                ) */

/*        C */
/*        C     Convert position to spherical coordinates. */
/*        C */
/*              CALL RECSPH ( STATE, R, COLAT, SLON ) */

/*        C */
/*        C     Convert velocity to spherical coordinates. */
/*        C */

/*              CALL DSPHDR ( STATE(1), STATE(2), STATE(3), JACOBI ) */

/*              CALL MXV ( JACOBI, STATE(4), SPHVEL ) */

/*        C */
/*        C     As a check, convert the spherical state back to */
/*        C     rectangular coordinates. */
/*        C */
/*              CALL SPHREC ( R, COLAT, SLON, RECTAN ) */

/*              CALL DRDSPH ( R, COLAT, SLON, JACOBI ) */

/*              CALL MXV ( JACOBI, SPHVEL, DRECTN ) */


/*              WRITE(*,*) ' ' */
/*              WRITE(*,*) 'Rectangular coordinates:' */
/*              WRITE(*,*) ' ' */
/*              WRITE(*,FMT1) '  X (km)                  = ', STATE(1) */
/*              WRITE(*,FMT1) '  Y (km)                  = ', STATE(2) */
/*              WRITE(*,FMT1) '  Z (km)                  = ', STATE(3) */
/*              WRITE(*,*) ' ' */
/*              WRITE(*,*) 'Rectangular velocity:' */
/*              WRITE(*,*) ' ' */
/*              WRITE(*,FMT1) '  dX/dt (km/s)            = ', STATE(4) */
/*              WRITE(*,FMT1) '  dY/dt (km/s)            = ', STATE(5) */
/*              WRITE(*,FMT1) '  dZ/dt (km/s)            = ', STATE(6) */
/*              WRITE(*,*) ' ' */
/*              WRITE(*,*) 'Spherical coordinates:' */
/*              WRITE(*,*) ' ' */
/*              WRITE(*,FMT1) '  Radius     (km)         = ', R */
/*              WRITE(*,FMT1) '  Colatitude (deg)        = ', */
/*             .                                             COLAT/RPD() */
/*              WRITE(*,FMT1) '  Longitude  (deg)        = ', SLON/RPD() */
/*              WRITE(*,*) ' ' */
/*              WRITE(*,*) 'Spherical velocity:' */
/*              WRITE(*,*) ' ' */
/*              WRITE(*,FMT1) '  d Radius/dt     (km/s)  = ', SPHVEL(1) */
/*              WRITE(*,FMT1) '  d Colatitude/dt (deg/s) = ', */
/*             .                                        SPHVEL(2)/RPD() */
/*              WRITE(*,FMT1) '  d Longitude/dt  (deg/s) = ', */
/*             .                                        SPHVEL(3)/RPD() */
/*              WRITE(*,*) ' ' */
/*              WRITE(*,*) 'Rectangular coordinates from inverse ' // */
/*             .           'mapping:' */
/*              WRITE(*,*) ' ' */
/*              WRITE(*,FMT1) '  X (km)                  = ', RECTAN(1) */
/*              WRITE(*,FMT1) '  Y (km)                  = ', RECTAN(2) */
/*              WRITE(*,FMT1) '  Z (km)                  = ', RECTAN(3) */
/*              WRITE(*,*) ' ' */
/*              WRITE(*,*) 'Rectangular velocity from inverse mapping:' */
/*              WRITE(*,*) ' ' */
/*              WRITE(*,FMT1) '  dX/dt (km/s)            = ', DRECTN(1) */
/*              WRITE(*,FMT1) '  dY/dt (km/s)            = ', DRECTN(2) */
/*              WRITE(*,FMT1) '  dZ/dt (km/s)            = ', DRECTN(3) */
/*              WRITE(*,*) ' ' */
/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Rectangular coordinates: */

/*          X (km)                  =    -0.76096183E+08 */
/*          Y (km)                  =     0.32436380E+09 */
/*          Z (km)                  =     0.47470484E+08 */

/*         Rectangular velocity: */

/*          dX/dt (km/s)            =     0.22952075E+05 */
/*          dY/dt (km/s)            =     0.53760111E+04 */
/*          dZ/dt (km/s)            =    -0.20881149E+02 */

/*         Spherical coordinates: */

/*          Radius     (km)         =     0.33653522E+09 */
/*          Colatitude (deg)        =     0.81891013E+02 */
/*          Longitude  (deg)        =     0.10320290E+03 */

/*         Spherical velocity: */

/*          d Radius/dt     (km/s)  =    -0.11211601E+02 */
/*          d Colatitude/dt (deg/s) =     0.33189930E-05 */
/*          d Longitude/dt  (deg/s) =    -0.40539288E-02 */

/*         Rectangular coordinates from inverse mapping: */

/*          X (km)                  =    -0.76096183E+08 */
/*          Y (km)                  =     0.32436380E+09 */
/*          Z (km)                  =     0.47470484E+08 */

/*         Rectangular velocity from inverse mapping: */

/*          dX/dt (km/s)            =     0.22952075E+05 */
/*          dY/dt (km/s)            =     0.53760111E+04 */
/*          dZ/dt (km/s)            =    -0.20881149E+02 */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */
/*        Added complete code example. */

/*        Changed the argument name LONG to SLON for consistency with */
/*        other routines. */

/* -    SPICELIB Version 1.0.0, 20-JUL-2001 (WLT) (IMU) */

/* -& */
/* $ Index_Entries */

/*     Jacobian of rectangular w.r.t. spherical coordinates */

/* -& */

/*     Local parameters */


/*     Local variables */

    ccolat = cos(*colat);
    scolat = sin(*colat);
    clong = cos(*slon);
    slong = sin(*slon);
    jacobi[0] = clong * scolat;
    jacobi[1] = slong * scolat;
    jacobi[2] = ccolat;
    jacobi[3] = *r__ * clong * ccolat;
    jacobi[4] = *r__ * slong * ccolat;
    jacobi[5] = -(*r__) * scolat;
    jacobi[6] = -(*r__) * slong * scolat;
    jacobi[7] = *r__ * clong * scolat;
    jacobi[8] = 0.;
    return 0;
} /* drdsph_ */

