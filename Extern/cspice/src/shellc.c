/* shellc.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure SHELLC ( Shell sort a character array ) */
/* Subroutine */ int shellc_(integer *ndim, char *array, ftnlen array_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    logical l_le(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer i__, j;
    extern /* Subroutine */ int swapc_(char *, char *, ftnlen, ftnlen);
    integer jg, gap;

/* $ Abstract */

/*     Sort an array of character strings according to the ASCII */
/*     collating sequence using the Shell Sort algorithm. */

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
/*     SORT */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     NDIM       I   Dimension of the array. */
/*     ARRAY     I-O  The array. */

/* $ Detailed_Input */

/*     NDIM     is the number of elements in the array to be sorted. */

/*     ARRAY    on input, is the array to be sorted. */

/* $ Detailed_Output */

/*     ARRAY    on output, contains the same elements, sorted */
/*              according to the ASCII collating sequence. */
/*              The actual sorting is done in place in ARRAY. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  If NDIM < 2, this routine does not modify the array. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The Shell Sort Algorithm is well known. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Given a list of words, sort it according to the ASCII */
/*        collating sequence using the Shell Sort algorithm. */


/*        Example code begins here. */


/*              PROGRAM SHELLC_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local parameters. */
/*        C */
/*              INTEGER               NDIM */
/*              PARAMETER           ( NDIM  = 6 ) */

/*              INTEGER               WRDSZ */
/*              PARAMETER           ( WRDSZ = 8 ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              CHARACTER*(WRDSZ)     ARRAY  ( NDIM ) */
/*              INTEGER               I */

/*        C */
/*        C     Let ARRAY contain the following elements: */
/*        C */
/*              ARRAY(1) = 'FEYNMAN' */
/*              ARRAY(2) = 'NEWTON' */
/*              ARRAY(3) = 'EINSTEIN' */
/*              ARRAY(4) = 'GALILEO' */
/*              ARRAY(5) = 'EUCLID' */
/*              ARRAY(6) = 'Galileo' */

/*        C */
/*        C     Print ARRAY before calling SHELLC. */
/*        C */
/*              WRITE(*,*) 'Array before calling SHELLC:' */
/*              WRITE(*,*) */
/*              DO I = 1, NDIM */
/*                 WRITE(*,*) '   ', ARRAY(I) */
/*              END DO */
/*              WRITE(*,*) */

/*        C */
/*        C     Call SHELLC and print ARRAY again. */
/*        C */
/*              CALL SHELLC ( NDIM, ARRAY ) */

/*              WRITE(*,*) 'Array after calling SHELLC:' */
/*              WRITE(*,*) */
/*              DO I = 1, NDIM */
/*                 WRITE(*,*) '   ', ARRAY(I) */
/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Array before calling SHELLC: */

/*            FEYNMAN */
/*            NEWTON */
/*            EINSTEIN */
/*            GALILEO */
/*            EUCLID */
/*            Galileo */

/*         Array after calling SHELLC: */

/*            EINSTEIN */
/*            EUCLID */
/*            FEYNMAN */
/*            GALILEO */
/*            Galileo */
/*            NEWTON */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Added complete */
/*        code example based on existing example. Extended $Exceptions */
/*        section to explain what happens if NDIM < 2. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) */

/* -& */
/* $ Index_Entries */

/*     shell sort a character array */

/* -& */

/*     Local variables */


/*     This is a straightforward implementation of the Shell Sort */
/*     algorithm. */

    gap = *ndim / 2;
    while(gap > 0) {
	i__1 = *ndim;
	for (i__ = gap + 1; i__ <= i__1; ++i__) {
	    j = i__ - gap;
	    while(j > 0) {
		jg = j + gap;
		if (l_le(array + (j - 1) * array_len, array + (jg - 1) * 
			array_len, array_len, array_len)) {
		    j = 0;
		} else {
		    swapc_(array + (j - 1) * array_len, array + (jg - 1) * 
			    array_len, array_len, array_len);
		}
		j -= gap;
	    }
	}
	gap /= 2;
    }
    return 0;
} /* shellc_ */

