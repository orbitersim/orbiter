/* vrelg.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure VRELG ( Vector relative difference, general dimension ) */
doublereal vrelg_(doublereal *v1, doublereal *v2, integer *ndim)
{
    /* System generated locals */
    doublereal ret_val, d__1, d__2;

    /* Local variables */
    doublereal denorm;
    extern doublereal vdistg_(doublereal *, doublereal *, integer *), vnormg_(
	    doublereal *, integer *);
    doublereal nunorm;

/* $ Abstract */

/*     Return the relative difference between two vectors of general */
/*     dimension. */

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

/*     MATH */
/*     VECTOR */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     V1, */
/*     V2         I   Input vectors. */
/*     NDIM       I   Dimension of V1 and V2. */

/*     The function returns the relative difference between two vectors */
/*     of general dimension. */

/* $ Detailed_Input */

/*     V1, */
/*     V2       are two vectors for which the relative difference is to */
/*              be computed. */

/*     NDIM     is the dimension of V1 and V2. */

/* $ Detailed_Output */

/*     The function returns the relative difference between the two input */
/*     n-dimensional vectors V1 and V2. */

/*     It is defined as: */

/*                          || V1 - V2 || */
/*        VRELG   =   ------------------------ */
/*                     MAX ( ||V1||, ||V2|| ) */

/*     where ||X|| indicates the Euclidean norm of the vector X. */

/*     VRELG assumes values in the range [0,2]. If both V1 and V2 are */
/*     zero vectors then VRELG is defined to be zero. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  If both V1 and V2 are zero vectors, then VRELG is defined to */
/*         be zero. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This function computes the relative difference between two vectors */
/*     of general dimension as defined above. */

/*     The function VREL may be used to find the relative difference */
/*     for two 3-dimensional vectors. */

/* $ Examples */

/*     This example determines if the state of Jupiter, with respect */
/*     to Voyager 2, for a set of times is the same for two different */
/*     ephemeris files. Instead of insisting on absolute equality */
/*     between the state vectors, the program will check if the relative */
/*     difference between the vectors is greater than a fixed tolerance. */

/*     C */
/*     C     The NAIF code for Jupiter is 599 and for Voyager 2 is -32. */
/*     C     Set the tolerance to be 0.0005. */
/*     C */
/*           INTEGER               JUP */
/*           PARAMETER           ( JUP = 599 ) */

/*           INTEGER               VG2 */
/*           PARAMETER           ( VG2 = -32 ) */

/*           INTEGER               NUM */
/*           PARAMETER           ( NUM = 500 ) */

/*           DOUBLE PRECISION      TOL */
/*           PARAMETER           ( TOL = 5.D-04 ) */

/*     C */
/*     C     SPICELIB function */
/*     C */
/*           DOUBLE PRECISION      VRELG */
/*     C */
/*     C     Local variables */
/*     C */
/*           DOUBLE PRECISION      STATE1 ( 6, NUM ) */
/*           DOUBLE PRECISION      STATE2 ( 6, NUM ) */
/*           DOUBLE PRECISION      ET     (    NUM ) */
/*           DOUBLE PRECISION      LT */
/*           DOUBLE PRECISION      DIFF */

/*           INTEGER               HANDLE */
/*           INTEGER               I */

/*           . */
/*           . */
/*           . */

/*           C */
/*           C     Load  the first SPK file. */
/*           C */
/*                 CALL SPKLEF ( 'VG2_SOURCE_1.BSP', HANDLE ) */
/*           C */
/*           C     Find the states for each time in the array ET. */
/*           C     This example assumes that the SPK file can */
/*           C     provide states for all of the times in the array. */
/*           C */
/*                 DO I = 1, NUM */

/*                    CALL SPKEZ ( JUP, ET(I),      'J2000', 'LT', */
/*                .                VG2, STATE1(1,I), LT           ) */

/*                 END DO */
/*           C */
/*           C     Unload the first file and load the second one. */
/*           C */
/*                 CALL SPKUEF ( HANDLE ) */

/*                 CALL SPKLEF ( 'VG2_SOURCE_2.BSP', HANDLE ) */
/*           C */
/*           C     Find the states from the new file. */
/*           C */
/*                 DO I = 1, NUM */

/*                    CALL SPKEZ ( JUP, ET(I),      'J2000', 'LT', */
/*                .                VG2, STATE2(1,I), LT           ) */

/*                 END DO */
/*           C */
/*           C     Now compare the two state vectors for each time. */
/*           C */
/*                 DO I = 1, NUM */

/*                    DIFF = VRELG ( STATE1(1,I), STATE2(1,I), 6 ) */

/*                    IF ( DIFF .GT. TOL ) THEN */

/*                       . */
/*                       . */
/*                       . */

/*                    END IF */

/*                 END DO */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     J.M. Lynch         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.0, 15-JUN-1992 (JML) */

/* -& */
/* $ Index_Entries */

/*     relative difference of n-dimensional vectors */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     If the numerator is zero then set VRELG equal to zero. Otherwise, */
/*     perform the rest of the calculation. */

/*     This handles the case where both vectors are zero vectors since */
/*     the distance between them will be zero. */

    nunorm = vdistg_(v1, v2, ndim);
    if (nunorm == 0.) {
	ret_val = 0.;
    } else {
/* Computing MAX */
	d__1 = vnormg_(v1, ndim), d__2 = vnormg_(v2, ndim);
	denorm = max(d__1,d__2);
	ret_val = nunorm / denorm;
    }
    return ret_val;
} /* vrelg_ */

