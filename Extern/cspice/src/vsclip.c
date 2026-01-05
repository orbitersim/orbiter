/* vsclip.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure VSCLIP ( Vector scaling, 3 dimensions, in place ) */
/* Subroutine */ int vsclip_(doublereal *s, doublereal *v)
{
/* $ Abstract */

/*     Multiply a scalar and a 3-dimensional double precision vector, */
/*     replacing the input vector with the result. */

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
/*     S          I   Scalar by which to multiply a vector. */
/*     V         I-O  Vector to be multiplied/result of multiplication. */

/* $ Detailed_Input */

/*     S        is a double precision scalar used to multiply the vector */
/*              V. */

/*     V        is a 3-dimensional, double precision vector which is to */
/*              be scaled by S. */

/* $ Detailed_Output */

/*     V        is the 3-dimensional, double precision vector resulting */
/*              from the scalar multiplication */

/*                 S * V */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine is provided for situation where it is convenient to */
/*     scale a vector in place rather than store the result in a */
/*     separate variable. Note that the call */

/*        CALL VSCL ( S, V, V ) */

/*     is not permitted by the ANSI Fortran 77 standard; this routine */
/*     can be called instead to achieve the same result. */

/*     VSCLIP multiplies each component of V by S to form the respective */
/*     components of the output vector. No error checking is performed. */

/* $ Examples */

/*     The following table shows the output V as a function of the */
/*     the inputs V and S. */

/*        V on input         S          V on output */
/*        ------------------------------------------------------- */
/*        (1D0, -2D0, 0D0)   -1D0       (-1D0, 2D0, 0D0) */
/*        (0D0, 0D0, 0D0)     5D0       (0D0, 0D0, 0D0) */

/* $ Restrictions */

/*     1)  The user is responsible for insuring that no floating point */
/*         overflow occurs from multiplying S by any component of V. No */
/*         error recovery or reporting scheme is incorporated in this */
/*         subroutine. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.M. Owen          (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 16-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.0, 01-SEP-2005 (NJB) (WMO) */

/* -& */
/* $ Index_Entries */

/*     3-dimensional vector scaling in place */

/* -& */
    v[0] = *s * v[0];
    v[1] = *s * v[1];
    v[2] = *s * v[2];
    return 0;
} /* vsclip_ */

