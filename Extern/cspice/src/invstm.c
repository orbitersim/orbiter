/* invstm.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__6 = 6;
static integer c__3 = 3;

/* $Procedure INVSTM ( Inverse of state transformation matrix ) */
/* Subroutine */ int invstm_(doublereal *mat, doublereal *invmat)
{
    extern /* Subroutine */ int xposbl_(doublereal *, integer *, integer *, 
	    integer *, doublereal *);

/* $ Abstract */

/*     Return the inverse of a state transformation matrix. */

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
/*     MATRIX */
/*     TRANSFORMATION */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     MAT        I   A state transformation matrix. */
/*     INVMAT     O   The inverse of MAT. */

/* $ Detailed_Input */

/*     MAT      is a state transformation matrix for converting states */
/*              relative to one frame to states relative to another. */
/*              The state transformation of a state vector, S, is */
/*              performed by the matrix-vector product. */

/*                 MAT * S. */

/*              For MAT to be a "true" state transformation matrix */
/*              it must have the form */

/*                  .-            -. */
/*                  |       :      | */
/*                  |   R   :   0  | */
/*                  |.......:......| */
/*                  |       :      | */
/*                  |  W*R  :   R  | */
/*                  |       :      | */
/*                  `-            -' */

/*              where R is a 3x3 rotation matrix, 0 is the 3x3 zero */
/*              matrix and W is a 3x3 skew-symmetric matrix. */

/*              NOTE: no checks are performed on MAT to ensure that it */
/*                    does indeed have the form described above. */

/* $ Detailed_Output */

/*     INVMAT   is the inverse of MAT under the operation of matrix */
/*              multiplication. */

/*              If MAT has the form described above, then INVMAT has */
/*              the form shown below. */

/*                 .-             -. */
/*                 |     t  :      | */
/*                 |    R   :   0  | */
/*                 |........:......| */
/*                 |      t :    t | */
/*                 | (W*R)  :   R  | */
/*                 |        :      | */
/*                 `-             -' */

/*              (The superscript "t" denotes the matrix transpose */
/*              operation.) */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  No checks are performed to ensure that the input matrix is */
/*         indeed a state transformation matrix. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Given a matrix for transforming states relative frame 1 to */
/*     states relative frame 2, the routine produces the inverse */
/*     matrix. That is, it returns the matrix for transforming states */
/*     relative to frame 2 to states relative to frame 1. */

/*     This special routine exists because unlike the inverse of a */
/*     rotation matrix, the inverse of a state transformation matrix, */
/*     is NOT simply the transpose of the matrix. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Suppose you have a geometric state of a spacecraft in Earth */
/*        body-fixed reference frame and wish to express this state */
/*        relative to an Earth centered J2000 frame. The following */
/*        example code illustrates how to carry out this computation. */

/*        Use the PCK kernel below to load the required high precision */
/*        orientation of the ITRF93 Earth body-fixed reference frame. */
/*        Note that the body ID code used in this file for the Earth is */
/*        3000. */

/*           earth_720101_070426.bpc */


/*        Example code begins here. */


/*              PROGRAM INVSTM_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local variables. */
/*        C */
/*              DOUBLE PRECISION      ET */
/*              DOUBLE PRECISION      INVMAT ( 6, 6 ) */
/*              DOUBLE PRECISION      ISTAT1 ( 6    ) */
/*              DOUBLE PRECISION      ISTAT2 ( 6    ) */
/*              DOUBLE PRECISION      MAT    ( 6, 6 ) */
/*              DOUBLE PRECISION      STATE  ( 6    ) */
/*              DOUBLE PRECISION      XMAT   ( 6, 6 ) */

/*              INTEGER               EARTH */

/*        C */
/*        C     Define the state of the spacecraft, in km and */
/*        C     km/s, and the ET epoch, in seconds past J2000. */
/*        C */
/*              DATA                  ET    /  0.0D0 / */
/*              DATA                  STATE /  175625246.29100420D0, */
/*             .                               164189388.12540060D0, */
/*             .                               -62935198.26067264D0, */
/*             .                                   11946.73372264D0, */
/*             .                                  -12771.29732556D0, */
/*             .                                      13.84902914D0 / */

/*        C */
/*        C     Load the required high precision Earth PCK. */
/*        C */
/*              CALL FURNSH ( 'earth_720101_070426.bpc' ) */

/*        C */
/*        C     First get the state transformation from J2000 frame */
/*        C     to Earth body-fixed frame at the time of interest ET. */
/*        C     The body ID code used in high precision PCK files for */
/*        C     the Earth is 3000; this number indicates that the */
/*        C     terrestrial frame used is ITRF93. */
/*        C */
/*              EARTH = 3000 */
/*              CALL TISBOD ( 'J2000', EARTH, ET, MAT ) */

/*        C */
/*        C     Get the inverse of MAT. */
/*        C */
/*              CALL INVSTM ( MAT,  INVMAT          ) */

/*        C */
/*        C     Transform from bodyfixed state to inertial state. */
/*        C */
/*              CALL MXVG ( INVMAT, STATE, 6, 6, ISTAT1 ) */

/*        C */
/*        C     Print the resulting state. */
/*        C */
/*              WRITE(*,'(A)') 'Input state in Earth centered J2000 ' */
/*             .            // 'frame, using INVSTM:' */
/*              WRITE(*,'(A,3F16.3)') '   Position:', ISTAT1(1:3) */
/*              WRITE(*,'(A,3F16.3)') '   Velocity:', ISTAT1(4:6) */

/*        C */
/*        C     Compute the same state using SXFORM. */
/*        C */
/*              CALL SXFORM ( 'ITRF93', 'J2000', ET, XMAT ) */
/*              CALL MXVG   ( XMAT, STATE, 6, 6, ISTAT2 ) */

/*              WRITE(*,*) */
/*              WRITE(*,'(A)') 'Input state in Earth centered J2000 ' */
/*             .            // 'frame, using SXFORM:' */
/*              WRITE(*,'(A,3F16.3)') '   Position:', ISTAT2(1:3) */
/*              WRITE(*,'(A,3F16.3)') '   Velocity:', ISTAT2(4:6) */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        Input state in Earth centered J2000 frame, using INVSTM: */
/*           Position:   192681395.921  -143792821.383   -62934296.473 */
/*           Velocity:          30.312          32.007          13.876 */

/*        Input state in Earth centered J2000 frame, using SXFORM: */
/*           Position:   192681395.921  -143792821.383   -62934296.473 */
/*           Velocity:          30.312          32.007          13.876 */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 25-NOV-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Removed unnecessary Standard SPICE error handling calls to */
/*        register/unregister this routine in the error handling */
/*        subsystem; this routine is Error free. */

/*        Edited the header to comply with NAIF standard. */
/*        Added complete code example based on the existing fragment. */

/* -    SPICELIB Version 1.0.2, 22-APR-2010 (NJB) */

/*        Header correction: assertions that the output */
/*        can overwrite the input have been removed. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 29-OCT-1990 (WLT) */

/* -& */
/* $ Index_Entries */

/*     inverse of state transformation matrix */

/* -& */

/*     Local parameters */


/*     Not much to this. Just call the more general routine XPOSBL. */

    xposbl_(mat, &c__6, &c__6, &c__3, invmat);
    return 0;
} /* invstm_ */

