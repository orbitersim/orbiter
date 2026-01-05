/* orderc.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ORDERC ( Order of a character array ) */
/* Subroutine */ int orderc_(char *array, integer *ndim, integer *iorder, 
	ftnlen array_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    logical l_le(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer i__, j;
    extern /* Subroutine */ int swapi_(integer *, integer *);
    integer jg, gap;

/* $ Abstract */

/*     Determine the order of elements in an array of character strings. */

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
/*     ARRAY      I    Input array. */
/*     NDIM       I    Dimension of ARRAY. */
/*     IORDER     O    Order vector for ARRAY. */

/* $ Detailed_Input */

/*     ARRAY    is the input array. */

/*     NDIM     is the number of elements in the input array. */

/* $ Detailed_Output */

/*     IORDER   is the order vector for the input array. */
/*              IORDER(1) is the index of the smallest element */
/*              of ARRAY; IORDER(2) is the index of the next */
/*              smallest; and so on. Strings are ordered according */
/*              to the ASCII collating sequence. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     ORDERC finds the index of the smallest element of the input */
/*     array. This becomes the first element of the order vector. */
/*     The process is repeated for the rest of the elements. */

/*     The order vector returned by ORDERC may be used by any of */
/*     the REORD routines to sort sets of related arrays, as shown */
/*     in the example below. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Sort four related arrays (containing the names, masses, */
/*        integer ID codes, and flags indicating whether they have */
/*        a ring system, for a group of planets). */


/*        Example code begins here. */


/*              PROGRAM ORDERC_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local constants. */
/*        C */
/*              INTEGER                 NDIM */
/*              PARAMETER             ( NDIM   = 8  ) */

/*              INTEGER                 STRLEN */
/*              PARAMETER             ( STRLEN = 7  ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              CHARACTER*(STRLEN)      NAMES  ( NDIM ) */

/*              DOUBLE PRECISION        MASSES ( NDIM ) */

/*              INTEGER                 CODES  ( NDIM ) */
/*              INTEGER                 I */
/*              INTEGER                 IORDER ( NDIM ) */

/*              LOGICAL                 RINGS  ( NDIM ) */

/*        C */
/*        C     Set the arrays containing the names, masses (given as */
/*        C     ratios to of Solar GM to barycenter GM), integer ID */
/*        C     codes, and flags indicating whether they have a ring */
/*        C     system. */
/*        C */
/*              DATA                    NAMES  / */
/*             .            'MERCURY', 'VENUS',  'EARTH',  'MARS', */
/*             .            'JUPITER', 'SATURN', 'URANUS', 'NEPTUNE' / */

/*              DATA                    MASSES / */
/*             .                       22032.080D0,   324858.599D0, */
/*             .                      398600.436D0,    42828.314D0, */
/*             .                   126712767.881D0, 37940626.068D0, */
/*             .                     5794559.128D0,  6836534.065D0 / */

/*              DATA                    CODES  / 199, 299, 399, 499, */
/*             .                                 599, 699, 799, 899 / */

/*              DATA                    RINGS  / */
/*             .                   .FALSE., .FALSE., .FALSE., .FALSE., */
/*             .                   .TRUE.,  .TRUE.,  .TRUE., .TRUE.   / */

/*        C */
/*        C     Sort the object arrays by name. */
/*        C */
/*              CALL ORDERC ( NAMES,  NDIM, IORDER ) */

/*              CALL REORDC ( IORDER, NDIM, NAMES  ) */
/*              CALL REORDD ( IORDER, NDIM, MASSES ) */
/*              CALL REORDI ( IORDER, NDIM, CODES  ) */
/*              CALL REORDL ( IORDER, NDIM, RINGS  ) */

/*        C */
/*        C     Output the resulting table. */
/*        C */
/*              WRITE(*,'(A)') ' Planet   Mass(GMS/GM)  ID Code  Rings?' */
/*              WRITE(*,'(A)') '-------  -------------  -------  ------' */

/*              DO I = 1, NDIM */

/*                 WRITE(*,'(A,F15.3,I9,L5)') NAMES(I), MASSES(I), */
/*             .                              CODES(I), RINGS(I) */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Planet   Mass(GMS/GM)  ID Code  Rings? */
/*        -------  -------------  -------  ------ */
/*        EARTH       398600.436      399    F */
/*        JUPITER  126712767.881      599    T */
/*        MARS         42828.314      499    F */
/*        MERCURY      22032.080      199    F */
/*        NEPTUNE    6836534.065      899    T */
/*        SATURN    37940626.068      699    T */
/*        URANUS     5794559.128      799    T */
/*        VENUS       324858.599      299    F */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 04-JUL-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */
/*        Added complete code example. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) */

/* -& */
/* $ Index_Entries */

/*     order of a character array */

/* -& */

/*     Local variables */


/*     Begin with the initial ordering. */

    i__1 = *ndim;
    for (i__ = 1; i__ <= i__1; ++i__) {
	iorder[i__ - 1] = i__;
    }

/*     Find the smallest element, then the next smallest, and so on. */
/*     This uses the Shell Sort algorithm, but swaps the elements of */
/*     the order vector instead of the array itself. */

    gap = *ndim / 2;
    while(gap > 0) {
	i__1 = *ndim;
	for (i__ = gap + 1; i__ <= i__1; ++i__) {
	    j = i__ - gap;
	    while(j > 0) {
		jg = j + gap;
		if (l_le(array + (iorder[j - 1] - 1) * array_len, array + (
			iorder[jg - 1] - 1) * array_len, array_len, array_len)
			) {
		    j = 0;
		} else {
		    swapi_(&iorder[j - 1], &iorder[jg - 1]);
		}
		j -= gap;
	    }
	}
	gap /= 2;
    }
    return 0;
} /* orderc_ */

