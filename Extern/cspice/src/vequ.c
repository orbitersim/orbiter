/* vequ.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure VEQU ( Vector equality, 3 dimensions ) */
/* Subroutine */ int vequ_(doublereal *vin, doublereal *vout)
{
/* $ Abstract */

/*     Make one double precision 3-dimensional vector equal to */
/*     another. */

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

/*     ASSIGNMENT */
/*     VECTOR */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     VIN        I   Double precision 3-dimensional vector. */
/*     VOUT       O   Double precision 3-dimensional vector set equal */
/*                    to VIN. */

/* $ Detailed_Input */

/*     VIN      is an arbitrary, double precision 3-dimensional vector. */

/* $ Detailed_Output */

/*     VOUT     is a double precision 3-dimensional vector set equal */
/*              to VIN. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     VEQU simply sets each component of VOUT in turn equal to VIN. No */
/*     error checking is performed because none is needed. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Lets assume we have a pointing record that contains the */
/*        start time of an interpolation interval, the components of */
/*        the quaternion that represents the C-matrix associated with */
/*        the start time of the interval, and the angular velocity vector */
/*        of the interval. The following example demonstrates how to */
/*        extract the time, the quaternion and the angular velocity */
/*        vector into separate variables for their processing. */


/*        Example code begins here. */


/*              PROGRAM VEQU_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local variables. */
/*        C */
/*              DOUBLE PRECISION      AV     ( 3 ) */
/*              DOUBLE PRECISION      QUAT   ( 4 ) */
/*              DOUBLE PRECISION      RECORD ( 8 ) */
/*              DOUBLE PRECISION      TIME */

/*              INTEGER               I */

/*        C */
/*        C     Define the pointing record. We would normally obtain it */
/*        C     from, e.g. CK readers or other non SPICE data files. */
/*        C */
/*              DATA                  RECORD  / */
/*             .      283480.753D0,   0.99999622D0,  0.0D0,  0.0D0, */
/*             .     -0.0027499965D0, 0.0D0,         0.0D0,  0.01D0 / */

/*        C */
/*        C     Get the time, quaternion and angular velocity vector */
/*        C     into separate variables. */
/*        C */
/*              TIME = RECORD(1) */

/*              CALL VEQUG  ( RECORD(2), 4, QUAT ) */
/*              CALL VEQU   ( RECORD(6),    AV   ) */

/*        C */
/*        C     Display the contents of the variables. */
/*        C */
/*              WRITE(*,'(A,F11.3)') 'Time            :', TIME */

/*              WRITE(*,'(A)')       'Quaternion      :' */
/*              WRITE(*,'(4F15.10)')  QUAT */
/*              WRITE(*,'(A)')       'Angular velocity:' */
/*              WRITE(*,'(3F15.10)')  AV */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        Time            : 283480.753 */
/*        Quaternion      : */
/*           0.9999962200   0.0000000000   0.0000000000  -0.0027499965 */
/*        Angular velocity: */
/*           0.0000000000   0.0000000000   0.0100000000 */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.M. Owen          (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Added complete */
/*        code example. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) */

/* -& */
/* $ Index_Entries */

/*     assign a 3-dimensional vector to another */

/* -& */
    vout[0] = vin[0];
    vout[1] = vin[1];
    vout[2] = vin[2];
    return 0;
} /* vequ_ */

