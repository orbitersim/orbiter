/* ducrss.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__6 = 6;

/* $Procedure DUCRSS ( Unit Normalized Cross Product and Derivative ) */
/* Subroutine */ int ducrss_(doublereal *s1, doublereal *s2, doublereal *sout)
{
    /* System generated locals */
    doublereal d__1, d__2;

    /* Local variables */
    doublereal scls1[6], scls2[6];
    extern /* Subroutine */ int dvhat_(doublereal *, doublereal *), moved_(
	    doublereal *, integer *, doublereal *), vsclg_(doublereal *, 
	    doublereal *, integer *, doublereal *);
    doublereal f1, f2;
    extern /* Subroutine */ int dvcrss_(doublereal *, doublereal *, 
	    doublereal *);
    doublereal tmpsta[6];

/* $ Abstract */

/*     Compute the unit vector parallel to the cross product of */
/*     two 3-dimensional vectors and the derivative of this unit vector. */

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

/*     DERIVATIVE */
/*     VECTOR */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     S1         I   Left hand state for cross product and derivative. */
/*     S2         I   Right hand state for cross product and derivative. */
/*     SOUT       O   Unit vector and derivative of the cross product. */

/* $ Detailed_Input */

/*     S1       is any state vector. Typically, this might represent the */
/*              apparent state of a planet or the Sun, which defines the */
/*              orientation of axes of some coordinate system. */

/*     S2       is any state vector. */

/* $ Detailed_Output */

/*     SOUT     is the unit vector parallel to the cross product of the */
/*              position components of S1 and S2 and the derivative of */
/*              the unit vector. */

/*              If the cross product of the position components is */
/*              the zero vector, then the position component of the */
/*              output will be the zero vector. The velocity component */
/*              of the output will simply be the derivative of the */
/*              cross product of the position components of S1 and S2. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  If the position components of S1 and S2 cross together to */
/*         give a zero vector, the position component of the output */
/*         will be the zero vector. The velocity component of the */
/*         output will simply be the derivative of the cross product */
/*         of the position vectors. */

/*     2)  If S1 and S2 are large in magnitude (taken together, */
/*         their magnitude surpasses the limit allowed by the */
/*         computer) then it may be possible to generate a */
/*         floating point overflow from an intermediate */
/*         computation even though the actual cross product and */
/*         derivative may be well within the range of double */
/*         precision numbers. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     DUCRSS calculates the unit vector parallel to the cross product */
/*     of two vectors and the derivative of that unit vector. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) One can construct non-inertial coordinate frames from apparent */
/*        positions of objects or defined directions. However, if one */
/*        wants to convert states in this non-inertial frame to states */
/*        in an inertial reference frame, the derivatives of the axes of */
/*        the non-inertial frame are required. */

/*        Define a reference frame with the apparent direction of the */
/*        Sun as seen from Earth as the primary axis X. Use the Earth */
/*        pole vector to define with the primary axis the XY plane of */
/*        the frame, with the primary axis Y pointing in the direction */
/*        of the pole. */

/*        Use the meta-kernel shown below to load the required SPICE */
/*        kernels. */


/*           KPL/MK */

/*           File name: ducrss_ex1.tm */

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
/*              pck00008.tpc                  Planet orientation and */
/*                                            radii */
/*              naif0009.tls                  Leapseconds */


/*           \begindata */

/*              KERNELS_TO_LOAD = ( 'de421.bsp', */
/*                                  'pck00008.tpc', */
/*                                  'naif0009.tls'  ) */

/*           \begintext */

/*           End of meta-kernel */


/*        Example code begins here. */


/*              PROGRAM DUCRSS_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local variables */
/*        C */
/*              DOUBLE PRECISION      ET */
/*              DOUBLE PRECISION      LT */
/*              DOUBLE PRECISION      STATE  ( 6    ) */
/*              DOUBLE PRECISION      TRANS  ( 6, 6 ) */
/*              DOUBLE PRECISION      X_NEW  ( 6    ) */
/*              DOUBLE PRECISION      Y_NEW  ( 6    ) */
/*              DOUBLE PRECISION      Z      ( 6    ) */
/*              DOUBLE PRECISION      Z_NEW  ( 6    ) */
/*              DOUBLE PRECISION      ZINERT ( 6    ) */

/*              INTEGER               I */


/*        C */
/*        C     Define the earth body-fixed pole vector (Z). The pole */
/*        C     has no velocity in the Earth fixed frame IAU_EARTH. */
/*        C */
/*              DATA                  Z  / 0.D0, 0.D0, 1.D0, */
/*             .                           0.D0, 0.D0, 0.D0  / */

/*        C */
/*        C     Load SPK, PCK, and LSK kernels, use a meta kernel for */
/*        C     convenience. */
/*        C */
/*              CALL FURNSH ( 'ducrss_ex1.tm' ) */

/*        C */
/*        C     Calculate the state transformation between IAU_EARTH and */
/*        C     J2000 at an arbitrary epoch. */
/*        C */
/*              CALL STR2ET ( 'Jan 1, 2009', ET ) */
/*              CALL SXFORM ( 'IAU_EARTH', 'J2000', ET, TRANS ) */

/*        C */
/*        C     Transform the earth pole vector from the IAU_EARTH frame */
/*        C     to J2000. */
/*        C */
/*              CALL MXVG ( TRANS, Z, 6, 6, ZINERT ) */

/*        C */
/*        C     Calculate the apparent state of the Sun from Earth at */
/*        C     the epoch ET in the J2000 frame. */
/*        C */
/*              CALL SPKEZR ( 'Sun',   ET,   'J2000', 'LT+S', */
/*             .              'Earth', STATE, LT              ) */

/*        C */
/*        C     Define the z axis of the new frame as the cross product */
/*        C     between the apparent direction of the Sun and the Earth */
/*        C     pole. Z_NEW cross X_NEW defines the Y axis of the */
/*        C     derived frame. */
/*        C */
/*              CALL DVHAT  ( STATE, X_NEW         ) */
/*              CALL DUCRSS ( STATE, ZINERT, Z_NEW ) */
/*              CALL DUCRSS ( Z_NEW, STATE,  Y_NEW ) */

/*        C */
/*        C     Display the results. */
/*        C */
/*              WRITE(*,'(A)') 'New X-axis:' */
/*              WRITE(*,'(A,3F16.12)') '   position:', (X_NEW(I), I=1,3) */
/*              WRITE(*,'(A,3F16.12)') '   velocity:', (X_NEW(I), I=4,6) */
/*              WRITE(*,'(A)') 'New Y-axis:' */
/*              WRITE(*,'(A,3F16.12)') '   position:', (Y_NEW(I), I=1,3) */
/*              WRITE(*,'(A,3F16.12)') '   velocity:', (Y_NEW(I), I=4,6) */
/*              WRITE(*,'(A)') 'New Z-axis:' */
/*              WRITE(*,'(A,3F16.12)') '   position:', (Z_NEW(I), I=1,3) */
/*              WRITE(*,'(A,3F16.12)') '   velocity:', (Z_NEW(I), I=4,6) */


/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        New X-axis: */
/*           position:  0.183446637633 -0.901919663328 -0.391009273602 */
/*           velocity:  0.000000202450  0.000000034660  0.000000015033 */
/*        New Y-axis: */
/*           position:  0.078846540163 -0.382978080242  0.920386339077 */
/*           velocity:  0.000000082384  0.000000032309  0.000000006387 */
/*        New Z-axis: */
/*           position: -0.979862518033 -0.199671507623  0.000857203851 */
/*           velocity:  0.000000044531 -0.000000218531 -0.000000000036 */


/*        Note that these vectors define the transformation between the */
/*        new frame and J2000 at the given ET: */

/*               .-            -. */
/*               |       :      | */
/*               |   R   :  0   | */
/*           M = | ......:......| */
/*               |       :      | */
/*               | dRdt  :  R   | */
/*               |       :      | */
/*               `-            -' */

/*        with */

/*           DATA         R     / X_NEW(1:3), Y_NEW(1:3), Z_NEW(1:3)  / */

/*           DATA         dRdt  / X_NEW(4:6), Y_NEW(4:6), Z_NEW(4:6)  / */

/* $ Restrictions */

/*     1)  No checking of S1 or S2 is done to prevent floating point */
/*         overflow. The user is required to determine that the magnitude */
/*         of each component of the states is within an appropriate range */
/*         so as not to cause floating point overflow. In almost every */
/*         case there will be no problem and no checking actually needs */
/*         to be done. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.3.0, 06-JUL-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Removed */
/*        unnecessary $Revisions section. */

/*        Added complete code example. */

/* -    SPICELIB Version 1.2.0, 08-APR-2014 (NJB) */

/*        Now scales inputs to reduce chance of numeric */
/*        overflow. */

/* -    SPICELIB Version 1.1.1, 22-APR-2010 (NJB) */

/*        Header correction: assertions that the output */
/*        can overwrite the input have been removed. */

/* -    SPICELIB Version 1.1.0, 30-AUG-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in DVHAT call. */

/* -    SPICELIB Version 1.0.0, 15-JUN-1995 (WLT) */

/* -& */
/* $ Index_Entries */

/*     Compute a unit cross product and its derivative */

/* -& */

/*     Local variables */


/*     Scale the components of the input states so the states have the */
/*     same direction and angular rates, but their largest position */
/*     components have absolute value equal to 1. Do not modify states */
/*     that have all position components equal to zero. */

/* Computing MAX */
    d__1 = abs(s1[0]), d__2 = abs(s1[1]), d__1 = max(d__1,d__2), d__2 = abs(
	    s1[2]);
    f1 = max(d__1,d__2);
/* Computing MAX */
    d__1 = abs(s2[0]), d__2 = abs(s2[1]), d__1 = max(d__1,d__2), d__2 = abs(
	    s2[2]);
    f2 = max(d__1,d__2);
    if (f1 > 0.) {
	d__1 = 1. / f1;
	vsclg_(&d__1, s1, &c__6, scls1);
    } else {
	moved_(s1, &c__6, scls1);
    }
    if (f2 > 0.) {
	d__1 = 1. / f2;
	vsclg_(&d__1, s2, &c__6, scls2);
    } else {
	moved_(s2, &c__6, scls2);
    }

/*     Not much to this.  Just get the cross product and its derivative. */
/*     Using that, get the associated unit vector and its derivative. */

    dvcrss_(scls1, scls2, tmpsta);
    dvhat_(tmpsta, sout);
    return 0;
} /* ducrss_ */

