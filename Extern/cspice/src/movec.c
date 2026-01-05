/* movec.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure MOVEC  ( Move a character array to another ) */
/* Subroutine */ int movec_(char *arrfrm, integer *ndim, char *arrto, ftnlen 
	arrfrm_len, ftnlen arrto_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer i__;

/* $ Abstract */

/*     Copy the elements of one character array into another */
/*     array. */

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
/*     ARRFRM     I   Character array to be moved. */
/*     NDIM       I   Number of elements to copy, i.e. the dimension */
/*                    of ARRFRM and ARRTO. */
/*     ARRTO      O   Destination array. */

/* $ Detailed_Input */

/*     ARRFRM   is an array from which to copy items. */

/*     NDIM     is the number of items to copy. */

/* $ Detailed_Output */

/*     ARRTO    is the array to which items should be copied. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine is simply shorthand for the following 3 lines of */
/*     code. */

/*             DO I = 1, NDIM */
/*                ARRTO(I) = ARRFRM(I) */
/*             END DO */

/* $ Examples */

/*     Often one needs to make a temporary copy of an array so that */
/*     it can be manipulated without altering the original array. */
/*     As pointed out in particulars, you could just do this within */
/*     the code that needs the copy. However, if you have several */
/*     arrays to copy, you can cut the number of lines of code that */
/*     are needed by a third. */

/*     For example: */

/*          DO I = 1, 19 */
/*             TEMPA(I) = A(I) */
/*          END DO */

/*          DO I = 1, 38 */
/*             TEMPB(I) = B(I) */
/*          END DO */

/*     Can be rewritten as */

/*          CALL MOVEC ( A, 19, TEMPA ) */
/*          CALL MOVEC ( B, 38, TEMPB ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.M. Owen          (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) */

/* -& */
/* $ Index_Entries */

/*     move a character array to another character array */

/* -& */
/* $ Revisions */

/* -     Beta Version 1.0.1, 4-FEB-1989 (WLT) */

/*      Header fully filled out. */

/* -& */

/*     Local variables */

    i__1 = *ndim;
    for (i__ = 1; i__ <= i__1; ++i__) {
	s_copy(arrto + (i__ - 1) * arrto_len, arrfrm + (i__ - 1) * arrfrm_len,
		 arrto_len, arrfrm_len);
    }
    return 0;
} /* movec_ */

