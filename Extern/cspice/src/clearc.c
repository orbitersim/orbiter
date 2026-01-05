/* clearc.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure CLEARC ( Clear an array of strings ) */
/* Subroutine */ int clearc_(integer *ndim, char *array, ftnlen array_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer i__;

/* $ Abstract */

/*     Fill an array of strings with blank strings. */

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
/*     ASSIGNMENT */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     NDIM       I   Number of elements of ARRAY to be set to blank. */
/*     ARRAY      O   Array of strings to be filled with blank strings. */

/* $ Detailed_Input */

/*     NDIM     is the number of elements in ARRAY which are to be set to */
/*              blank. */

/* $ Detailed_Output */

/*     ARRAY    is the array of strings with each of its NDIM elements */
/*              set to a blank string. If NDIM is smaller than the */
/*              declared dimension of ARRAY, only the first NDIM */
/*              elements of ARRAY are set to blank; all other elements */
/*              remain unchanged. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  If NDIM < 1, the array is not modified. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Initialize a one dimensional string array and then clear */
/*        the first two strings. */


/*        Example code begins here. */


/*              PROGRAM CLEARC_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local parameters. */
/*        C */
/*              INTEGER               STRSZ */
/*              PARAMETER           ( STRSZ = 21 ) */

/*              INTEGER               NDIM */
/*              PARAMETER           ( NDIM = 4   ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              CHARACTER*(STRSZ)     ARRAY(NDIM) */
/*              INTEGER               I */

/*        C */
/*        C     Initialize ARRAY. */
/*        C */
/*              ARRAY(1) = 'Element #1' */
/*              ARRAY(2) = 'Element #2' */
/*              ARRAY(3) = 'Element #3' */
/*              ARRAY(4) = 'Element #4' */

/*              WRITE(*,'(A)') 'Contents of ARRAY before CLEARC:' */
/*              WRITE(*,*) */
/*              DO I = 1, NDIM */
/*                 WRITE(*,'(A,I1,2A)') 'Position #', I , ': ', ARRAY(I) */
/*              END DO */

/*        C */
/*        C     Clear the first 2 elements. */
/*        C */
/*              CALL CLEARC ( 2, ARRAY ) */

/*              WRITE(*,*) */
/*              WRITE(*,'(A)') 'Contents of ARRAY after CLEARC:' */
/*              WRITE(*,*) */
/*              DO I = 1, NDIM */
/*                 WRITE(*,'(A,I1,2A)') 'Position #', I , ': ', ARRAY(I) */
/*              END DO */


/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        Contents of ARRAY before CLEARC: */

/*        Position #1: Element #1 */
/*        Position #2: Element #2 */
/*        Position #3: Element #3 */
/*        Position #4: Element #4 */

/*        Contents of ARRAY after CLEARC: */

/*        Position #1: */
/*        Position #2: */
/*        Position #3: Element #3 */
/*        Position #4: Element #4 */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.M. Owen          (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 19-MAY-2021 (JDR) (NJB) */

/*        Added IMPLICIT NONE statement. */

/*        Updated the header to comply with NAIF standard. Added */
/*        full code example. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) */

/* -& */
/* $ Index_Entries */

/*     clear a character array */

/* -& */

/*     Local variables */

    i__1 = *ndim;
    for (i__ = 1; i__ <= i__1; ++i__) {
	s_copy(array + (i__ - 1) * array_len, " ", array_len, (ftnlen)1);
    }
    return 0;
} /* clearc_ */

