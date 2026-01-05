/* qxq.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static doublereal c_b2 = 1.;

/* $Procedure QXQ (Quaternion times quaternion) */
/* Subroutine */ int qxq_(doublereal *q1, doublereal *q2, doublereal *qout)
{
    extern doublereal vdot_(doublereal *, doublereal *);
    doublereal cross[3];
    extern /* Subroutine */ int vcrss_(doublereal *, doublereal *, doublereal 
	    *), vlcom3_(doublereal *, doublereal *, doublereal *, doublereal *
	    , doublereal *, doublereal *, doublereal *);

/* $ Abstract */

/*     Multiply two quaternions. */

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

/*     ROTATION */

/* $ Keywords */

/*     MATH */
/*     POINTING */
/*     ROTATION */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     Q1         I   First SPICE quaternion factor. */
/*     Q2         I   Second SPICE quaternion factor. */
/*     QOUT       O   Product of Q1 and Q2. */

/* $ Detailed_Input */

/*     Q1       is a 4-vector representing a SPICE-style */
/*              quaternion. See the discussion of quaternion */
/*              styles in $Particulars below. */

/*              Note that multiple styles of quaternions */
/*              are in use. This routine will not work properly */
/*              if the input quaternions do not conform to */
/*              the SPICE convention. See the $Particulars */
/*              section for details. */

/*     Q2       is a second SPICE-style quaternion. */

/* $ Detailed_Output */

/*     QOUT     is 4-vector representing the quaternion product */

/*                 Q1 * Q2 */

/*              Representing Q(i) as the sums of scalar (real) */
/*              part s(i) and vector (imaginary) part v(i) */
/*              respectively, */

/*                 Q1 = s1 + v1 */
/*                 Q2 = s2 + v2 */

/*              QOUT has scalar part s3 defined by */

/*                 s3 = s1 * s2 - <v1, v2> */

/*              and vector part v3 defined by */

/*                 v3 = s1 * v2  +  s2 * v1  +  v1 x v2 */

/*              where the notation < , > denotes the inner */
/*              product operator and x indicates the cross */
/*              product operator. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Quaternion Styles */
/*     ----------------- */

/*     There are different "styles" of quaternions used in */
/*     science and engineering applications. Quaternion styles */
/*     are characterized by */

/*     -  The order of quaternion elements */

/*     -  The quaternion multiplication formula */

/*     -  The convention for associating quaternions */
/*        with rotation matrices */

/*     Two of the commonly used styles are */

/*        - "SPICE" */

/*           > Invented by Sir William Rowan Hamilton */
/*           > Frequently used in mathematics and physics textbooks */

/*        - "Engineering" */

/*           > Widely used in aerospace engineering applications */


/*     SPICELIB subroutine interfaces ALWAYS use SPICE quaternions. */
/*     Quaternions of any other style must be converted to SPICE */
/*     quaternions before they are passed to SPICELIB routines. */


/*     Relationship between SPICE and Engineering Quaternions */
/*     ------------------------------------------------------ */

/*     Let M be a rotation matrix such that for any vector V, */

/*        M*V */

/*     is the result of rotating V by theta radians in the */
/*     counterclockwise direction about unit rotation axis vector A. */
/*     Then the SPICE quaternions representing M are */

/*        (+/-) (  cos(theta/2), */
/*                 sin(theta/2) A(1), */
/*                 sin(theta/2) A(2), */
/*                 sin(theta/2) A(3)  ) */

/*     while the engineering quaternions representing M are */

/*        (+/-) ( -sin(theta/2) A(1), */
/*                -sin(theta/2) A(2), */
/*                -sin(theta/2) A(3), */
/*                 cos(theta/2)       ) */

/*     For both styles of quaternions, if a quaternion q represents */
/*     a rotation matrix M, then -q represents M as well. */

/*     Given an engineering quaternion */

/*        QENG   = ( q0,  q1,  q2,  q3 ) */

/*     the equivalent SPICE quaternion is */

/*        QSPICE = ( q3, -q0, -q1, -q2 ) */


/*     Associating SPICE Quaternions with Rotation Matrices */
/*     ---------------------------------------------------- */

/*     Let FROM and TO be two right-handed reference frames, for */
/*     example, an inertial frame and a spacecraft-fixed frame. Let the */
/*     symbols */

/*        V    ,   V */
/*         FROM     TO */

/*     denote, respectively, an arbitrary vector expressed relative to */
/*     the FROM and TO frames. Let M denote the transformation matrix */
/*     that transforms vectors from frame FROM to frame TO; then */

/*        V   =  M * V */
/*         TO         FROM */

/*     where the expression on the right hand side represents left */
/*     multiplication of the vector by the matrix. */

/*     Then if the unit-length SPICE quaternion q represents M, where */

/*        q = (q0, q1, q2, q3) */

/*     the elements of M are derived from the elements of q as follows: */

/*          +-                                                         -+ */
/*          |           2    2                                          | */
/*          | 1 - 2*( q2 + q3 )   2*(q1*q2 - q0*q3)   2*(q1*q3 + q0*q2) | */
/*          |                                                           | */
/*          |                                                           | */
/*          |                               2    2                      | */
/*      M = | 2*(q1*q2 + q0*q3)   1 - 2*( q1 + q3 )   2*(q2*q3 - q0*q1) | */
/*          |                                                           | */
/*          |                                                           | */
/*          |                                                   2    2  | */
/*          | 2*(q1*q3 - q0*q2)   2*(q2*q3 + q0*q1)   1 - 2*( q1 + q2 ) | */
/*          |                                                           | */
/*          +-                                                         -+ */

/*     Note that substituting the elements of -q for those of q in the */
/*     right hand side leaves each element of M unchanged; this shows */
/*     that if a quaternion q represents a matrix M, then so does the */
/*     quaternion -q. */

/*     To map the rotation matrix M to a unit quaternion, we start by */
/*     decomposing the rotation matrix as a sum of symmetric */
/*     and skew-symmetric parts: */

/*                                        2 */
/*        M = [ I  +  (1-cos(theta)) OMEGA  ] + [ sin(theta) OMEGA ] */

/*                     symmetric                   skew-symmetric */


/*     OMEGA is a skew-symmetric matrix of the form */

/*                   +-             -+ */
/*                   |  0   -n3   n2 | */
/*                   |               | */
/*         OMEGA  =  |  n3   0   -n1 | */
/*                   |               | */
/*                   | -n2   n1   0  | */
/*                   +-             -+ */

/*     The vector N of matrix entries (n1, n2, n3) is the rotation axis */
/*     of M and theta is M's rotation angle. Note that N and theta */
/*     are not unique. */

/*     Let */

/*        C = cos(theta/2) */
/*        S = sin(theta/2) */

/*     Then the unit quaternions Q corresponding to M are */

/*        Q = +/- ( C, S*n1, S*n2, S*n3 ) */

/*     The mappings between quaternions and the corresponding rotations */
/*     are carried out by the SPICELIB routines */

/*        Q2M {quaternion to matrix} */
/*        M2Q {matrix to quaternion} */

/*     M2Q always returns a quaternion with scalar part greater than */
/*     or equal to zero. */


/*     SPICE Quaternion Multiplication Formula */
/*     --------------------------------------- */

/*     Given a SPICE quaternion */

/*        Q = ( q0, q1, q2, q3 ) */

/*     corresponding to rotation axis A and angle theta as above, we can */
/*     represent Q using "scalar + vector" notation as follows: */

/*        s =   q0           = cos(theta/2) */

/*        v = ( q1, q2, q3 ) = sin(theta/2) * A */

/*        Q = s + v */

/*     Let Q1 and Q2 be SPICE quaternions with respective scalar */
/*     and vector parts s1, s2 and v1, v2: */

/*        Q1 = s1 + v1 */
/*        Q2 = s2 + v2 */

/*     We represent the dot product of v1 and v2 by */

/*        <v1, v2> */

/*     and the cross product of v1 and v2 by */

/*        v1 x v2 */

/*     Then the SPICE quaternion product is */

/*        Q1*Q2 = s1*s2 - <v1,v2>  + s1*v2 + s2*v1 + (v1 x v2) */

/*     If Q1 and Q2 represent the rotation matrices M1 and M2 */
/*     respectively, then the quaternion product */

/*        Q1*Q2 */

/*     represents the matrix product */

/*        M1*M2 */

/* $ Examples */

/*     The numerical results shown for these examples may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Given the "basis" quaternions: */

/*           QID:  ( 1.0, 0.0, 0.0, 0.0 ) */
/*           QI :  ( 0.0, 1.0, 0.0, 0.0 ) */
/*           QJ :  ( 0.0, 0.0, 1.0, 0.0 ) */
/*           QK :  ( 0.0, 0.0, 0.0, 1.0 ) */

/*        the following quaternion products give these results: */

/*            Product       Expected result */
/*           -----------   ---------------------- */
/*            QI  * QJ     ( 0.0, 0.0, 0.0, 1.0 ) */
/*            QJ  * QK     ( 0.0, 1.0, 0.0, 0.0 ) */
/*            QK  * QI     ( 0.0, 0.0, 1.0, 0.0 ) */
/*            QI  * QI     (-1.0, 0.0, 0.0, 0.0 ) */
/*            QJ  * QJ     (-1.0, 0.0, 0.0, 0.0 ) */
/*            QK  * QK     (-1.0, 0.0, 0.0, 0.0 ) */
/*            QID * QI     ( 0.0, 1.0, 0.0, 0.0 ) */
/*            QI  * QID    ( 0.0, 1.0, 0.0, 0.0 ) */
/*            QID * QJ     ( 0.0, 0.0, 1.0, 0.0 ) */

/*        The following code example uses QXQ to produce these results. */


/*        Example code begins here. */


/*              PROGRAM QXQ_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local variables */
/*        C */
/*              DOUBLE PRECISION      QID    ( 0 : 3 ) */
/*              DOUBLE PRECISION      QI     ( 0 : 3 ) */
/*              DOUBLE PRECISION      QJ     ( 0 : 3 ) */
/*              DOUBLE PRECISION      QK     ( 0 : 3 ) */
/*              DOUBLE PRECISION      QOUT   ( 0 : 3 ) */

/*        C */
/*        C     Let QID, QI, QJ, QK be the "basis" */
/*        C     quaternions. */
/*        C */
/*              DATA                  QID  / 1.D0,  0.D0,  0.D0,  0.D0 / */
/*              DATA                  QI   / 0.D0,  1.D0,  0.D0,  0.D0 / */
/*              DATA                  QJ   / 0.D0,  0.D0,  1.D0,  0.D0 / */
/*              DATA                  QK   / 0.D0,  0.D0,  0.D0,  1.D0 / */

/*        C */
/*        C     Compute: */
/*        C */
/*        C        QI x QJ = QK */
/*        C        QJ x QK = QI */
/*        C        QK x QI = QJ */
/*        C */
/*              CALL QXQ ( QI, QJ, QOUT ) */
/*              WRITE(*,'(A,4F8.2)') 'QI x QJ  =', QOUT */
/*              WRITE(*,'(A,4F8.2)') '     QK  =', QK */
/*              WRITE(*,*) ' ' */

/*              CALL QXQ ( QJ, QK, QOUT ) */
/*              WRITE(*,'(A,4F8.2)') 'QJ x QK  =', QOUT */
/*              WRITE(*,'(A,4F8.2)') '     QI  =', QI */
/*              WRITE(*,*) ' ' */

/*              CALL QXQ ( QK, QI, QOUT ) */
/*              WRITE(*,'(A,4F8.2)') 'QK x QI  =', QOUT */
/*              WRITE(*,'(A,4F8.2)') '     QJ  =', QJ */
/*              WRITE(*,*) ' ' */

/*        C */
/*        C     Compute: */
/*        C */
/*        C        QI x QI  ==  -QID */
/*        C        QJ x QJ  ==  -QID */
/*        C        QK x QK  ==  -QID */
/*        C */
/*              CALL QXQ ( QI, QI, QOUT ) */
/*              WRITE(*,'(A,4F8.2)') 'QI x QI  =', QOUT */
/*              WRITE(*,'(A,4F8.2)') '     QID =', QID */
/*              WRITE(*,*) ' ' */

/*              CALL QXQ ( QJ, QJ, QOUT ) */
/*              WRITE(*,'(A,4F8.2)') 'QJ x QJ  =', QOUT */
/*              WRITE(*,'(A,4F8.2)') '     QID =', QID */
/*              WRITE(*,*) ' ' */

/*              CALL QXQ ( QK, QK, QOUT ) */
/*              WRITE(*,'(A,4F8.2)') 'QK x QK  =', QOUT */
/*              WRITE(*,'(A,4F8.2)') '     QID =', QID */
/*              WRITE(*,*) ' ' */

/*        C */
/*        C     Compute: */
/*        C */
/*        C        QID x QI  = QI */
/*        C        QI  x QID = QI */
/*        C        QID x QJ  = QJ */
/*        C */
/*              CALL QXQ ( QID, QI, QOUT ) */
/*              WRITE(*,'(A,4F8.2)') 'QID x QI =', QOUT */
/*              WRITE(*,'(A,4F8.2)') '      QI =', QI */
/*              WRITE(*,*) ' ' */

/*              CALL QXQ ( QI, QID, QOUT ) */
/*              WRITE(*,'(A,4F8.2)') 'QI x QID =', QOUT */
/*              WRITE(*,'(A,4F8.2)') '      QI =', QI */
/*              WRITE(*,*) ' ' */

/*              CALL QXQ ( QID, QJ, QOUT ) */
/*              WRITE(*,'(A,4F8.2)') 'QID x QJ =', QOUT */
/*              WRITE(*,'(A,4F8.2)') '      QJ =', QJ */
/*              WRITE(*,*) ' ' */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        QI x QJ  =    0.00    0.00    0.00    1.00 */
/*             QK  =    0.00    0.00    0.00    1.00 */

/*        QJ x QK  =    0.00    1.00    0.00    0.00 */
/*             QI  =    0.00    1.00    0.00    0.00 */

/*        QK x QI  =    0.00    0.00    1.00    0.00 */
/*             QJ  =    0.00    0.00    1.00    0.00 */

/*        QI x QI  =   -1.00    0.00    0.00    0.00 */
/*             QID =    1.00    0.00    0.00    0.00 */

/*        QJ x QJ  =   -1.00    0.00    0.00    0.00 */
/*             QID =    1.00    0.00    0.00    0.00 */

/*        QK x QK  =   -1.00    0.00    0.00    0.00 */
/*             QID =    1.00    0.00    0.00    0.00 */

/*        QID x QI =    0.00    1.00    0.00    0.00 */
/*              QI =    0.00    1.00    0.00    0.00 */

/*        QI x QID =    0.00    1.00    0.00    0.00 */
/*              QI =    0.00    1.00    0.00    0.00 */

/*        QID x QJ =    0.00    0.00    1.00    0.00 */
/*              QJ =    0.00    0.00    1.00    0.00 */


/*     2) Compute the composition of two rotation matrices by */
/*        converting them to quaternions and computing their */
/*        product, and by directly multiplying the matrices. */

/*        Example code begins here. */


/*              PROGRAM QXQ_EX2 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local variables */
/*        C */
/*              DOUBLE PRECISION      CMAT1  ( 3,  3 ) */
/*              DOUBLE PRECISION      CMAT2  ( 3,  3 ) */
/*              DOUBLE PRECISION      CMOUT  ( 3,  3 ) */
/*              DOUBLE PRECISION      Q1     ( 0 : 3 ) */
/*              DOUBLE PRECISION      Q2     ( 0 : 3 ) */
/*              DOUBLE PRECISION      QOUT   ( 0 : 3 ) */

/*              INTEGER               I */

/*              DATA                  CMAT1  /  1.D0,  0.D0,  0.D0, */
/*             .                                0.D0, -1.D0,  0.D0, */
/*             .                                0.D0,  0.D0, -1.D0  / */

/*              DATA                  CMAT2  /  0.D0,  1.D0,  0.D0, */
/*             .                                1.D0,  0.D0,  0.D0, */
/*             .                                0.D0,  0.D0, -1.D0  / */


/*        C */
/*        C     Convert the C-matrices to quaternions. */
/*        C */
/*              CALL M2Q ( CMAT1, Q1 ) */
/*              CALL M2Q ( CMAT2, Q2 ) */

/*        C */
/*        C     Find the product. */
/*        C */
/*              CALL QXQ ( Q1, Q2, QOUT ) */

/*        C */
/*        C     Convert the result to a C-matrix. */
/*        C */
/*              CALL Q2M ( QOUT, CMOUT ) */

/*              WRITE(*,'(A)') 'Using quaternion product:' */
/*              WRITE(*,'(3F10.4)') (CMOUT(1,I), I = 1, 3) */
/*              WRITE(*,'(3F10.4)') (CMOUT(2,I), I = 1, 3) */
/*              WRITE(*,'(3F10.4)') (CMOUT(3,I), I = 1, 3) */

/*        C */
/*        C     Multiply CMAT1 and CMAT2 directly. */
/*        C */
/*              CALL MXM ( CMAT1, CMAT2, CMOUT ) */

/*              WRITE(*,'(A)') 'Using matrix product:' */
/*              WRITE(*,'(3F10.4)') (CMOUT(1,I), I = 1, 3) */
/*              WRITE(*,'(3F10.4)') (CMOUT(2,I), I = 1, 3) */
/*              WRITE(*,'(3F10.4)') (CMOUT(3,I), I = 1, 3) */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        Using quaternion product: */
/*            0.0000    1.0000    0.0000 */
/*           -1.0000    0.0000    0.0000 */
/*            0.0000    0.0000    1.0000 */
/*        Using matrix product: */
/*            0.0000    1.0000    0.0000 */
/*           -1.0000    0.0000    0.0000 */
/*            0.0000    0.0000    1.0000 */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */

/* $ Version */

/* -    SPICELIB Version 1.0.2, 06-JUL-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */
/*        Created complete code examples from existing example and */
/*        code fragments. */

/* -    SPICELIB Version 1.0.1, 26-FEB-2008 (NJB) */

/*        Updated header; added information about SPICE */
/*        quaternion conventions. */

/* -    SPICELIB Version 1.0.0, 18-AUG-2002 (NJB) */

/* -& */
/* $ Index_Entries */

/*     quaternion times quaternion */
/*     multiply quaternion by quaternion */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Compute the scalar part of the product. */

    qout[0] = q1[0] * q2[0] - vdot_(&q1[1], &q2[1]);

/*     And now the vector part.  The SPICELIB routine VLCOM3 computes */
/*     a linear combination of three 3-vectors. */

    vcrss_(&q1[1], &q2[1], cross);
    vlcom3_(q1, &q2[1], q2, &q1[1], &c_b2, cross, &qout[1]);
    return 0;
} /* qxq_ */

