/* vcrss.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure VCRSS ( Vector cross product, 3 dimensions ) */
/* Subroutine */ int vcrss_(doublereal *v1, doublereal *v2, doublereal *vout)
{
    doublereal vtemp[3];

/* $ Abstract */

/*     Compute the cross product of two 3-dimensional vectors. */

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

/*     VECTOR */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     V1         I   Left hand vector for cross product. */
/*     V2         I   Right hand vector for cross product. */
/*     VOUT       O   Cross product V1 x V2. */

/* $ Detailed_Input */

/*     V1, */
/*     V2       are two 3-dimensional vectors. Typically, these might */
/*              represent the (possibly unit) vector to a planet, Sun, */
/*              or a star which defines the orientation of axes of some */
/*              reference frame. */

/* $ Detailed_Output */

/*     VOUT     is the cross product of V1 and V2. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     VCRSS calculates the three dimensional cross product of two */
/*     vectors according to the definition. */

/*     If V1 and V2 are large in magnitude (taken together, their */
/*     magnitude surpasses the limit allowed by the computer) then it */
/*     may be possible to generate a floating point overflow from an */
/*     intermediate computation even though the actual cross product may */
/*     be well within the range of double precision numbers. VCRSS does */
/*     NOT check the magnitude of V1 or V2 to insure that overflow will */
/*     not occur. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Define two sets of vectors and compute the cross product of */
/*        each vector in first set and the corresponding vector in */
/*        the second set. */


/*        Example code begins here. */


/*              PROGRAM VCRSS_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local parameters. */
/*        C */
/*              INTEGER               NDIM */
/*              PARAMETER           ( NDIM   = 3 ) */

/*              INTEGER               SETSIZ */
/*              PARAMETER           ( SETSIZ = 2 ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              DOUBLE PRECISION      SETA ( NDIM, SETSIZ ) */
/*              DOUBLE PRECISION      SETB ( NDIM, SETSIZ ) */
/*              DOUBLE PRECISION      VOUT ( NDIM ) */

/*              INTEGER               I */
/*              INTEGER               J */

/*        C */
/*        C     Define the two vector sets. */
/*        C */
/*              DATA                  SETA / 0.D0,  1.D0,  0.D0, */
/*             .                             5.D0,  5.D0,  5.D0  / */

/*              DATA                  SETB / 1.D0,  0.D0,  0.D0, */
/*             .                            -1.D0, -1.D0, -1.D0  / */

/*        C */
/*        C     Calculate the cross product of each pair of vectors */
/*        C */
/*              DO I=1, SETSIZ */

/*                 CALL VCRSS ( SETA(1,I), SETB(1,I), VOUT ) */

/*                 WRITE(*,'(A,3F5.1)') 'Vector A     : ', */
/*             .                        ( SETA(J,I), J=1,3 ) */
/*                 WRITE(*,'(A,3F5.1)') 'Vector B     : ', */
/*             .                        ( SETB(J,I), J=1,3 ) */
/*                 WRITE(*,'(A,3F5.1)') 'Cross product: ', VOUT */
/*                 WRITE(*,*) ' ' */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        Vector A     :   0.0  1.0  0.0 */
/*        Vector B     :   1.0  0.0  0.0 */
/*        Cross product:   0.0  0.0 -1.0 */

/*        Vector A     :   5.0  5.0  5.0 */
/*        Vector B     :  -1.0 -1.0 -1.0 */
/*        Cross product:   0.0  0.0  0.0 */


/* $ Restrictions */

/*     1)  No checking of V1 or V2 is done to prevent floating point */
/*         overflow. The user is required to determine that the */
/*         magnitude of each component of the vectors is within an */
/*         appropriate range so as not to cause floating point overflow. */
/*         In almost every case there will be no problem and no checking */
/*         actually needs to be done. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.M. Owen          (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 05-JUL-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Added complete */
/*        code example based on existing example. */

/* -    SPICELIB Version 1.0.2, 22-APR-2010 (NJB) */

/*        Header correction: assertions that the output */
/*        can overwrite the input have been removed. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) */

/* -& */
/* $ Index_Entries */

/*     vector cross product */

/* -& */

/*     Local variables */


/*  Calculate the cross product of V1 and V2, store in VTEMP */

    vtemp[0] = v1[1] * v2[2] - v1[2] * v2[1];
    vtemp[1] = v1[2] * v2[0] - v1[0] * v2[2];
    vtemp[2] = v1[0] * v2[1] - v1[1] * v2[0];

/*  Now move the result into VOUT */

    vout[0] = vtemp[0];
    vout[1] = vtemp[1];
    vout[2] = vtemp[2];

    return 0;
} /* vcrss_ */

