/* axisar.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure AXISAR ( Axis and angle to rotation ) */
/* Subroutine */ int axisar_(doublereal *axis, doublereal *angle, doublereal *
	r__)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    extern /* Subroutine */ int vequ_(doublereal *, doublereal *);
    integer i__;
    extern /* Subroutine */ int ident_(doublereal *);
    doublereal vtemp[3];
    extern /* Subroutine */ int vrotv_(doublereal *, doublereal *, doublereal 
	    *, doublereal *);

/* $ Abstract */

/*     Construct a rotation matrix that rotates vectors by a specified */
/*     angle about a specified axis. */

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

/*     MATRIX */
/*     ROTATION */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     AXIS       I   Rotation axis. */
/*     ANGLE      I   Rotation angle, in radians. */
/*     R          O   Rotation matrix corresponding to AXIS and ANGLE. */

/* $ Detailed_Input */

/*     AXIS, */
/*     ANGLE    are, respectively, a rotation axis and a rotation */
/*              angle. AXIS and ANGLE determine a coordinate */
/*              transformation whose effect on any vector V is to */
/*              rotate V by ANGLE radians about the vector AXIS. */

/* $ Detailed_Output */

/*     R        is a rotation matrix representing the coordinate */
/*              transformation determined by AXIS and ANGLE: for */
/*              each vector V, R*V is the vector resulting from */
/*              rotating V by ANGLE radians about AXIS. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  If AXIS is the zero vector, the rotation generated is the */
/*         identity. This is consistent with the specification of VROTV. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     AXISAR can be thought of as a partial inverse of RAXISA.  AXISAR */
/*     really is a `left inverse': the code fragment */

/*        CALL RAXISA ( R,    AXIS,  ANGLE ) */
/*        CALL AXISAR ( AXIS, ANGLE, R     ) */

/*     preserves R, except for round-off error, as long as R is a */
/*     rotation matrix. */

/*     On the other hand, the code fragment */

/*        CALL AXISAR ( AXIS, ANGLE, R     ) */
/*        CALL RAXISA ( R,    AXIS,  ANGLE ) */

/*     preserves AXIS and ANGLE, except for round-off error, only if */
/*     ANGLE is in the range (0, pi). So AXISAR is a right inverse */
/*     of RAXISA only over a limited domain. */

/* $ Examples */

/*     The numerical results shown for these examples may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Compute a matrix that rotates vectors by pi/2 radians about */
/*        the Z-axis, and compute the rotation axis and angle based on */
/*        that matrix. */


/*        Example code begins here. */


/*              PROGRAM AXISAR_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     SPICELIB functions. */
/*        C */
/*              DOUBLE PRECISION      DPR */
/*              DOUBLE PRECISION      HALFPI */

/*        C */
/*        C     Local variables */
/*        C */
/*              DOUBLE PRECISION      ANGLE */
/*              DOUBLE PRECISION      ANGOUT */
/*              DOUBLE PRECISION      AXIS   ( 3    ) */
/*              DOUBLE PRECISION      AXOUT  ( 3    ) */
/*              DOUBLE PRECISION      ROTMAT ( 3, 3 ) */

/*              INTEGER               I */
/*              INTEGER               J */

/*        C */
/*        C     Define an axis and an angle for rotation. */
/*        C */
/*              AXIS(1) = 0.D0 */
/*              AXIS(2) = 0.D0 */
/*              AXIS(3) = 1.D0 */
/*              ANGLE   = HALFPI() */

/*        C */
/*        C     Determine the rotation matrix. */
/*        C */
/*              CALL AXISAR ( AXIS, ANGLE, ROTMAT ) */

/*        C */
/*        C     Now calculate the rotation axis and angle based on */
/*        C     ROTMAT as input. */
/*        C */
/*              CALL RAXISA ( ROTMAT, AXOUT, ANGOUT ) */

/*        C */
/*        C     Display the results. */
/*        C */
/*              WRITE(*,'(A)') 'Rotation matrix:' */
/*              WRITE(*,*) */
/*              DO I = 1, 3 */
/*                 WRITE(*,'(3F10.5)') ( ROTMAT(I,J), J=1,3 ) */
/*              END DO */
/*              WRITE(*,*) */
/*              WRITE(*,'(A,3F10.5)') 'Rotation axis       :', AXOUT */
/*              WRITE(*,'(A,F10.5)')  'Rotation angle (deg):', */
/*             .                                      ANGOUT * DPR() */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        Rotation matrix: */

/*           0.00000  -1.00000   0.00000 */
/*           1.00000   0.00000   0.00000 */
/*           0.00000   0.00000   1.00000 */

/*        Rotation axis       :   0.00000   0.00000   1.00000 */
/*        Rotation angle (deg):  90.00000 */


/*     2) Linear interpolation between two rotation matrices. */

/*        Let R(t) be a time-varying rotation matrix; R could be */
/*        a C-matrix describing the orientation of a spacecraft */
/*        structure. Given two points in time t1 and t2 at which */
/*        R(t) is known, and given a third time t3, where */

/*           t1  <  t3  <  t2, */

/*        we can estimate R(t3) by linear interpolation. In other */
/*        words, we approximate the motion of R by pretending that */
/*        R rotates about a fixed axis at a uniform angular rate */
/*        during the time interval [t1, t2]. More specifically, we */
/*        assume that each column vector of R rotates in this */
/*        fashion. This procedure will not work if R rotates through */
/*        an angle of pi radians or more during the time interval */
/*        [t1, t2]; an aliasing effect would occur in that case. */


/*        Example code begins here. */


/*              PROGRAM AXISAR_EX2 */
/*              IMPLICIT NONE */

/*        C */
/*        C     SPICELIB functions. */
/*        C */
/*              DOUBLE PRECISION      DPR */
/*              DOUBLE PRECISION      HALFPI */

/*        C */
/*        C     Local variables */
/*        C */
/*              DOUBLE PRECISION      ANGLE */
/*              DOUBLE PRECISION      AXIS   ( 3    ) */
/*              DOUBLE PRECISION      DELTA  ( 3, 3 ) */
/*              DOUBLE PRECISION      FRAC */
/*              DOUBLE PRECISION      Q      ( 3, 3 ) */
/*              DOUBLE PRECISION      R1     ( 3, 3 ) */
/*              DOUBLE PRECISION      R2     ( 3, 3 ) */
/*              DOUBLE PRECISION      R3     ( 3, 3 ) */
/*              DOUBLE PRECISION      T1 */
/*              DOUBLE PRECISION      T2 */
/*              DOUBLE PRECISION      T3 */

/*              INTEGER               I */
/*              INTEGER               J */

/*        C */
/*        C     Lets assume that R(t) is the matrix that rotates */
/*        C     vectors by pi/2 radians about the Z-axis every */
/*        C     minute. */
/*        C */
/*        C     Let */
/*        C */
/*        C        R1 = R(t1), for t1 =  0", and */
/*        C        R2 = R(t2), for t1 = 60". */
/*        C */
/*        C     Define both matrices and times. */
/*        C */
/*              AXIS(1) = 0.D0 */
/*              AXIS(2) = 0.D0 */
/*              AXIS(3) = 1.D0 */

/*              T1 =  0.D0 */
/*              T2 = 60.D0 */
/*              T3 = 30.D0 */

/*              CALL IDENT  ( R1 ) */
/*              CALL AXISAR ( AXIS, HALFPI(), R2 ) */

/*        C */
/*        C     Lets compute */
/*        C */
/*        C                    -1 */
/*        C        Q  = R2 * R1  , */
/*        C */
/*        C     The rotation axis and angle of Q define the rotation */
/*        C     that each column of R(t) undergoes from time `t1' to */
/*        C     time `t2'.  Since R(t) is orthogonal, we can find Q */
/*        C     using the transpose of R1.  We find the rotation axis */
/*        C     and angle via RAXISA. */

/*              CALL MXMT   ( R2,   R1,    Q      ) */
/*              CALL RAXISA ( Q,    AXIS,  ANGLE  ) */

/*        C */
/*        C     Find the fraction of the total rotation angle that R */
/*        C     rotates through in the time interval [t1, t3]. */
/*        C */
/*              FRAC = ( T3 - T1 )  /  ( T2 - T1 ) */

/*        C */
/*        C     Finally, find the rotation DELTA that R(t) undergoes */
/*        C     during the time interval [t1, t3], and apply that */
/*        C     rotation to R1, yielding R(t3), which we'll call R3. */
/*        C */
/*              CALL AXISAR ( AXIS,   FRAC * ANGLE,  DELTA  ) */
/*              CALL MXM    ( DELTA,  R1,            R3     ) */

/*        C */
/*        C     Display the results. */
/*        C */
/*              WRITE(*,'(A,F10.5)')  'Time (s)            :', T3 */
/*              WRITE(*,'(A,3F10.5)') 'Rotation axis       :', AXIS */
/*              WRITE(*,'(A,F10.5)')  'Rotation angle (deg):', */
/*             .                               FRAC * ANGLE * DPR() */
/*              WRITE(*,'(A)')        'Rotation matrix     :' */
/*              WRITE(*,*) */
/*              DO I = 1, 3 */
/*                 WRITE(*,'(3F10.5)') ( R3(I,J), J=1,3 ) */
/*              END DO */


/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        Time (s)            :  30.00000 */
/*        Rotation axis       :   0.00000   0.00000   1.00000 */
/*        Rotation angle (deg):  45.00000 */
/*        Rotation matrix     : */

/*           0.70711  -0.70711   0.00000 */
/*           0.70711   0.70711   0.00000 */
/*           0.00000   0.00000   1.00000 */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.0, 06-JUL-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Removed */
/*        unnecessary $Revisions section. */

/*        Added complete code examples based on existing code fragments. */

/* -    SPICELIB Version 1.1.0, 25-AUG-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in VROTV call. */

/*        Identity matrix is now obtained from IDENT. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 30-AUG-1990 (NJB) */

/* -& */
/* $ Index_Entries */

/*     axis and angle to rotation */

/* -& */

/*     Local variables */


/*     First, set R equal to the identity. */

    ident_(r__);

/*     The matrix we want rotates EVERY vector by ANGLE about AXIS. */
/*     In particular, it does so to our basis vectors.  The columns */
/*     of R are the images of the basis vectors under this rotation. */

    for (i__ = 1; i__ <= 3; ++i__) {
	vrotv_(&r__[(i__1 = i__ * 3 - 3) < 9 && 0 <= i__1 ? i__1 : s_rnge(
		"r", i__1, "axisar_", (ftnlen)393)], axis, angle, vtemp);
	vequ_(vtemp, &r__[(i__1 = i__ * 3 - 3) < 9 && 0 <= i__1 ? i__1 : 
		s_rnge("r", i__1, "axisar_", (ftnlen)394)]);
    }
    return 0;
} /* axisar_ */

