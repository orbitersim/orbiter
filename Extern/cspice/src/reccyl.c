/* reccyl.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure RECCYL ( Rectangular to cylindrical coordinates ) */
/* Subroutine */ int reccyl_(doublereal *rectan, doublereal *r__, doublereal *
	clon, doublereal *z__)
{
    /* System generated locals */
    doublereal d__1, d__2;

    /* Builtin functions */
    double sqrt(doublereal), atan2(doublereal, doublereal);

    /* Local variables */
    doublereal x, y;
    extern doublereal twopi_(void);
    doublereal big;

/* $ Abstract */

/*     Convert from rectangular to cylindrical coordinates. */

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
/*     RECTAN     I   Rectangular coordinates of a point. */
/*     R          O   Distance of the point from Z axis. */
/*     CLON       O   Angle (radians) of the point from XZ plane */
/*     Z          O   Height of the point above XY plane. */

/* $ Detailed_Input */

/*     RECTAN   are the rectangular coordinates of the point of */
/*              interest. */

/* $ Detailed_Output */

/*     R        is the distance of the point of interest from Z-axis. */

/*     CLON     is the cylindrical angle (in radians) of the point of */
/*              interest from XZ plane. The CLON range is [0, 2pi]. */

/*     Z        is the height of the point above XY plane. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine transforms the coordinates of a point from */
/*     rectangular to cylindrical coordinates. */

/* $ Examples */

/*     The numerical results shown for these examples may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Compute the cylindrical coordinates of the position of the Moon */
/*        as seen from the Earth, and convert them to rectangular */
/*        coordinates. */

/*        Use the meta-kernel shown below to load the required SPICE */
/*        kernels. */


/*           KPL/MK */

/*           File name: reccyl_ex1.tm */

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


/*              PROGRAM RECCYL_EX1 */
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
/*              DOUBLE PRECISION      LT */
/*              DOUBLE PRECISION      POS    ( 3 ) */
/*              DOUBLE PRECISION      RECTAN ( 3 ) */
/*              DOUBLE PRECISION      R */
/*              DOUBLE PRECISION      Z */

/*        C */
/*        C     Load SPK and LSK kernels, use a meta kernel for */
/*        C     convenience. */
/*        C */
/*              CALL FURNSH ( 'reccyl_ex1.tm' ) */

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
/*        C     Convert the cylindrical to rectangular coordinates. */
/*        C */

/*              CALL CYLREC ( R, CLON, Z, RECTAN ) */

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

/*         Cylindrical coordinates: */

/*          Radius     (km):      383289.01777726 */
/*          Longitude (deg):         261.65040211 */
/*          Z          (km):     -126505.93063865 */

/*         Rectangular coordinates from CYLREC: */

/*          X          (km):      -55658.44323296 */
/*          Y          (km):     -379226.32931475 */
/*          Z          (km):     -126505.93063865 */


/*     2) Create a table showing a variety of rectangular coordinates */
/*        and the corresponding cylindrical coordinates. */

/*        Corresponding rectangular and cylindrical coordinates are */
/*        listed to three decimal places. Output angles are in degrees. */


/*        Example code begins here. */


/*              PROGRAM RECCYL_EX2 */
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
/*              DOUBLE PRECISION      CLON */
/*              DOUBLE PRECISION      R */
/*              DOUBLE PRECISION      RECTAN ( 3, NREC ) */
/*              DOUBLE PRECISION      Z */

/*              INTEGER               I */
/*              INTEGER               J */

/*        C */
/*        C     Define the input rectangular coordinates. */
/*        C */
/*              DATA                 RECTAN / */
/*             .                  0.D0,         0.D0,         0.D0, */
/*             .                  1.D0,         0.D0,         0.D0, */
/*             .                  0.D0,         1.D0,         0.D0, */
/*             .                  0.D0,         0.D0,         1.D0, */
/*             .                 -1.D0,         0.D0,         0.D0, */
/*             .                  0.D0,        -1.D0,         0.D0, */
/*             .                  0.D0,         0.D0,        -1.D0, */
/*             .                  1.D0,         1.D0,         0.D0, */
/*             .                  1.D0,         0.D0,         1.D0, */
/*             .                  0.D0,         1.D0,         1.D0, */
/*             .                  1.D0,         1.D0,         1.D0  / */

/*        C */
/*        C     Print the banner. */
/*        C */
/*              WRITE(*,*) ' RECT(1)  RECT(2)  RECT(3) ' */
/*             . //        '    R       CLON      Z' */
/*              WRITE(*,*) ' -------  -------  ------- ' */
/*             . //        ' -------  -------  ------- ' */

/*        C */
/*        C     Do the conversion. Output angles in degrees. */
/*        C */
/*              DO I = 1, NREC */

/*                 CALL RECCYL( RECTAN(1,I), R, CLON, Z ) */

/*                 WRITE (*,'(6F9.3)') ( RECTAN(J,I), J=1,3 ), */
/*             .                         R, CLON * DPR(), Z */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*          RECT(1)  RECT(2)  RECT(3)     R       CLON      Z */
/*          -------  -------  -------  -------  -------  ------- */
/*            0.000    0.000    0.000    0.000    0.000    0.000 */
/*            1.000    0.000    0.000    1.000    0.000    0.000 */
/*            0.000    1.000    0.000    1.000   90.000    0.000 */
/*            0.000    0.000    1.000    0.000    0.000    1.000 */
/*           -1.000    0.000    0.000    1.000  180.000    0.000 */
/*            0.000   -1.000    0.000    1.000  270.000    0.000 */
/*            0.000    0.000   -1.000    0.000    0.000   -1.000 */
/*            1.000    1.000    0.000    1.414   45.000    0.000 */
/*            1.000    0.000    1.000    1.000    0.000    1.000 */
/*            0.000    1.000    1.000    1.000   90.000    1.000 */
/*            1.000    1.000    1.000    1.414   45.000    1.000 */


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

/*        Changed the output argument name LONG to CLON for consistency */
/*        with other routines. */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */
/*        Added complete code examples. */

/* -    SPICELIB Version 1.0.3, 26-JUL-2016 (BVS) */

/*        Minor headers edits. */

/* -    SPICELIB Version 1.0.2, 22-AUG-2001 (EDW) */

/*        Corrected ENDIF to END IF. Obsolete $Revisions section */
/*        deleted. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) */

/* -& */
/* $ Index_Entries */

/*     rectangular to cylindrical coordinates */

/* -& */

/*     SPICELIB functions */


/*     Local Variables */


/*     Use temporary variables for computing R. */

/* Computing MAX */
    d__1 = abs(rectan[0]), d__2 = abs(rectan[1]);
    big = max(d__1,d__2);

/*     Convert to cylindrical coordinates */

    *z__ = rectan[2];
    if (big == 0.) {
	*r__ = 0.;
	*clon = 0.;
    } else {
	x = rectan[0] / big;
	y = rectan[1] / big;
	*r__ = big * sqrt(x * x + y * y);
	*clon = atan2(y, x);
    }
    if (*clon < 0.) {
	*clon += twopi_();
    }
    return 0;
} /* reccyl_ */

