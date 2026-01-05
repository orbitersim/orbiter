/* sameai.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure SAMEAI ( Are two integer arrays the same? ) */
logical sameai_(integer *a1, integer *a2, integer *ndim)
{
    /* System generated locals */
    integer i__1;
    logical ret_val;

    /* Local variables */
    integer i__;

/* $ Abstract */

/*     Indicate whether two integer arrays are equal. */

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

/*     ARRAY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     A1         I    First array to be compared. */
/*     A2         I    Second array to be compared. */
/*     NDIM       I    Dimension of A1 and A2. */

/*     The function returns the value .TRUE. if and only if A1 = A2. */

/* $ Detailed_Input */

/*     A1, */
/*     A2       are two integer arrays to be compared.  A1 and */
/*              A2 must have the same dimension. */

/*     NDIM     is the common dimension of A1 and A2. */

/* $ Detailed_Output */

/*     The function takes the value .TRUE. if and only if A1 equals A2. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This function can be thought of as a macro. It replaces the */
/*     loop */

/*        SAME  = .TRUE. */
/*        I     =  1 */

/*        DO WHILE (  ( I .LE. NDIM )  .AND.  SAME  ) */

/*           IF ( A1(I) .NE.  A2(I)  ) */
/*              SAME  = .FALSE. */
/*           ELSE */
/*              I     =  I + 1 */
/*           END IF */

/*        END DO */

/* $ Examples */

/*     1)  Test two integer arrays A1 and A2 for equality, where both */
/*         arrays have declared length 10: */

/*            SAME  =  SAMEAI ( A1, A2, 10 ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.0, 19-DEC-1995 (NJB) */

/* -& */
/* $ Index_Entries */

/*     test two integer arrays for equality */

/* -& */

/*     Local variables */


/*     Executable code */

    ret_val = TRUE_;
    i__1 = *ndim;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (a1[i__ - 1] != a2[i__ - 1]) {
	    ret_val = FALSE_;
	    return ret_val;
	}
    }
    return ret_val;
} /* sameai_ */

