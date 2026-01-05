/* dvcrss.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure DVCRSS ( Derivative of Vector cross product ) */
/* Subroutine */ int dvcrss_(doublereal *s1, doublereal *s2, doublereal *sout)
{
    extern /* Subroutine */ int vadd_(doublereal *, doublereal *, doublereal *
	    ), vequ_(doublereal *, doublereal *);
    doublereal vtemp[3];
    extern /* Subroutine */ int vcrss_(doublereal *, doublereal *, doublereal 
	    *);
    doublereal dvtmp1[3], dvtmp2[3];

/* $ Abstract */

/*     Compute the cross product of two 3-dimensional vectors */
/*     and the derivative of this cross product. */

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
/*     SOUT       O   State associated with cross product of positions. */

/* $ Detailed_Input */

/*     S1       is any state vector. Typically, this might represent the */
/*              apparent state of a planet or the Sun, which defines the */
/*              orientation of axes of some coordinate system. */

/*     S2       is any state vector. */

/* $ Detailed_Output */

/*     SOUT     is the state associated with the cross product of the */
/*              position components of S1 and S2. In other words, if */
/*              S1 = (P1,V1) and S2 = (P2,V2) then SOUT is */
/*              ( P1xP2, d/dt( P1xP2 ) ). */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  If S1 and S2 are large in magnitude (taken together, */
/*         their magnitude surpasses the limit allowed by the */
/*         computer) then it may be possible to generate a */
/*         floating point overflow from an intermediate */
/*         computation even though the actual cross product and */
/*         derivative may be well within the range of double */
/*         precision numbers. */

/*         DVCRSS does NOT check the magnitude of S1 or S2 to */
/*         insure that overflow will not occur. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     DVCRSS calculates the three-dimensional cross product of two */
/*     vectors and the derivative of that cross product according to */
/*     the definition. */

/* $ Examples */

/*     The numerical results shown for these examples may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Compute the cross product of two 3-dimensional vectors */
/*        and the derivative of this cross product. */


/*        Example code begins here. */


/*              PROGRAM DVCRSS_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local variables */
/*        C */
/*              DOUBLE PRECISION      S1     ( 6, 2 ) */
/*              DOUBLE PRECISION      S2     ( 6, 2 ) */
/*              DOUBLE PRECISION      SOUT   ( 6    ) */

/*              INTEGER               I */
/*              INTEGER               J */

/*        C */
/*        C     Set S1 and S2 vectors. */
/*        C */
/*              DATA                  S1 / */
/*             .                   0.D0, 1.D0, 0.D0, 1.D0, 0.D0, 0.D0, */
/*             .                   5.D0, 5.D0, 5.D0, 1.D0, 0.D0, 0.D0  / */
/*              DATA                  S2 / */
/*             .                 1.D0,  0.D0,  0.D0, 1.D0, 0.D0, 0.D0, */
/*             .                -1.D0, -1.D0, -1.D0, 2.D0, 0.D0, 0.D0  / */

/*        C */
/*        C     For each vector S1 and S2, compute their cross product */
/*        C     and its derivative. */
/*        C */
/*              DO I = 1, 2 */

/*                 CALL DVCRSS ( S1(1,I), S2(1,I), SOUT) */

/*                 WRITE(*,'(A,6F7.1)') 'S1  :', ( S1(J,I), J=1,6 ) */
/*                 WRITE(*,'(A,6F7.1)') 'S2  :', ( S2(J,I), J=1,6 ) */
/*                 WRITE(*,'(A,6F7.1)') 'SOUT:', ( SOUT(J), J=1,6 ) */
/*                 WRITE(*,*) */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        S1  :    0.0    1.0    0.0    1.0    0.0    0.0 */
/*        S2  :    1.0    0.0    0.0    1.0    0.0    0.0 */
/*        SOUT:    0.0    0.0   -1.0    0.0    0.0   -1.0 */

/*        S1  :    5.0    5.0    5.0    1.0    0.0    0.0 */
/*        S2  :   -1.0   -1.0   -1.0    2.0    0.0    0.0 */
/*        SOUT:    0.0    0.0    0.0    0.0   11.0  -11.0 */


/*     2) One can construct non-inertial coordinate frames from apparent */
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

/*           File name: dvcrss_ex2.tm */

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


/*              PROGRAM DVCRSS_EX2 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local variables */
/*        C */
/*              DOUBLE PRECISION      ET */
/*              DOUBLE PRECISION      LT */
/*              DOUBLE PRECISION      STATE  ( 6    ) */
/*              DOUBLE PRECISION      TMPSTA ( 6    ) */
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
/*              CALL FURNSH ( 'dvcrss_ex2.tm' ) */

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
/*        C     Define the X axis of the new frame to aligned with */
/*        C     the computed state. Calculate the state's unit vector */
/*        C     and its derivative to get the X axis and its */
/*        C     derivative. */
/*        C */
/*              CALL DVHAT  ( STATE, X_NEW         ) */

/*        C */
/*        C     Define the Z axis of the new frame as the cross product */
/*        C     between the computed state and the Earth pole. */
/*        C     Calculate the Z direction in the new reference frame, */
/*        C     then calculate the this direction's unit vector and its */
/*        C     derivative to get the Z axis and its derivative. */
/*        C */
/*              CALL DVCRSS ( STATE,  ZINERT, TMPSTA ) */
/*              CALL DVHAT  ( TMPSTA, Z_NEW          ) */

/*        C */
/*        C     As for Z_NEW, calculate the Y direction in the new */
/*        C     reference frame, then calculate this direction's unit */
/*        C     vector and its derivative to get the Y axis and its */
/*        C     derivative. */
/*        C */
/*              CALL DUCRSS ( Z_NEW,  STATE, TMPSTA ) */
/*              CALL DVHAT  ( TMPSTA, Y_NEW         ) */

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

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Added complete */
/*        code examples. */

/* -    SPICELIB Version 1.0.1, 22-APR-2010 (NJB) */

/*        Header correction: assertions that the output */
/*        can overwrite the input have been removed. */

/* -    SPICELIB Version 1.0.0, 15-JUN-1995 (WLT) */

/* -& */
/* $ Index_Entries */

/*     Compute the derivative of a cross product */

/* -& */

/*     Local Variables */


/*     Calculate the cross product of S1 and S2, store it in VTEMP. */

    vcrss_(s1, s2, vtemp);

/*     Calculate the two components of the derivative of S1 x S2. */

    vcrss_(&s1[3], s2, dvtmp1);
    vcrss_(s1, &s2[3], dvtmp2);

/*     Put all of the pieces into SOUT. */

    vequ_(vtemp, sout);
    vadd_(dvtmp1, dvtmp2, &sout[3]);
    return 0;
} /* dvcrss_ */

