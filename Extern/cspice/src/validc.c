/* validc.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure VALIDC ( Validate a character set ) */
/* Subroutine */ int validc_(integer *size, integer *n, char *a, ftnlen a_len)
{
    integer card;
    extern /* Subroutine */ int chkin_(char *, ftnlen), scardc_(integer *, 
	    char *, ftnlen), rmdupc_(integer *, char *, ftnlen), sigerr_(char 
	    *, ftnlen), chkout_(char *, ftnlen), ssizec_(integer *, char *, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Create a valid set from a character set array. */

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

/*     SETS */

/* $ Keywords */

/*     CELLS */
/*     SETS */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     SIZE       I   Size (maximum cardinality) of the set. */
/*     N          I   Initial no. of (possibly non-distinct) elements. */
/*     A         I-O  Set to be validated. */

/* $ Detailed_Input */

/*     SIZE     is the maximum cardinality (number of elements) */
/*              of the set. */

/*     N        is the number of (possibly non-distinct) elements */
/*              initially contained in the array used to maintain */
/*              the set. N cannot be greater than the size of the */
/*              set. */


/*     A        is a set. */


/*              On input, A contains N elements beginning at A(1). */
/*              To create a valid set, the elements are ordered, */
/*              and duplicate elements are removed. The contents */
/*              of A(LBCELL) through A(0) are lost during validation. */

/* $ Detailed_Output */

/*     A        on output, is the set containing the ordered, */
/*              distinct values in the input array, ready for */
/*              use with other set routines. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the size of the set is too small to hold the set BEFORE */
/*         validation, the error SPICE(INVALIDSIZE) is signaled. The set */
/*         A is not modified. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine is typically used to turn an array which has been */
/*     initialized through DATA or I/O statements into a set, which */
/*     can then be used with the other set routines. */

/*     Because a set is ordered and contains distinct values, to */
/*     create a set from an array, it is necessary to sort the array */
/*     into the set and remove duplicates. Once the array has been */
/*     sorted, duplicate elements (adjacent after sorting) are removed. */
/*     The size and cardinality of the set are initialized, and the */
/*     set is ready to go. */

/*     Because validation is done in place, there is no chance of */
/*     overflow. */

/* $ Examples */

/*     Empty sets may be initialized with the cell routines SSIZEx. */
/*     Sets may also be initialized from nonempty set arrays. */
/*     This process, called validation, is done by the set routines */
/*     VALIDC and VALIDI. In the following example, */

/*           INTEGER      BODIES  ( LBCELL:100 ) */

/*           DATA       ( BODIES(I), I=1,8)       /  3, 301, */
/*          .                                        3, 399, */
/*          .                                        5, 501, */
/*          .                                        6, 601,  / */

/*           CALL VALIDI ( 100, 8, BODIES ) */

/*     the integer set BODIES is validated. The size of BODIES set to */
/*     100. The eight elements of the array (stored in elements 1-8) */
/*     are sorted, and duplicate elements (in this case, the number 3, */
/*     which appears twice) are removed, and the cardinality of the set */
/*     is set to the number of distinct elements, now seven. The set is */
/*     now ready for use with the rest of the set routines. */

/*     The previous contents of elements LBCELL through 0 are lost */
/*     during the process of validation. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     C.A. Curzon        (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 05-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU) (NJB) */

/* -& */
/* $ Index_Entries */

/*     validate a character set */

/* -& */
/* $ Revisions */

/* -    Beta Version 2.0.0, 13-MAR-1989 (NJB) */

/*        Now participates in error handling. References to RETURN, */
/*        CHKIN, and CHKOUT added. Check for adequate set size added. */

/*        The examples have been updated to illustrate set initialization */
/*        without the use of the EMPTYx routines, which have been */
/*        removed from the library. Errors in the examples have been */
/*        removed, also. */

/* -& */

/*     SPICELIB functions */


/*     Local variables */

    if (return_()) {
	return 0;
    } else {
	chkin_("VALIDC", (ftnlen)6);
    }

/*     Is the set size big enough? */

    if (*n > *size) {
	setmsg_("Size of un-validated set is too small.  Size is #, size req"
		"uired is #. ", (ftnlen)71);
	errint_("#", size, (ftnlen)1);
	errint_("#", n, (ftnlen)1);
	sigerr_("SPICE(INVALIDSIZE)", (ftnlen)18);
	chkout_("VALIDC", (ftnlen)6);
	return 0;
    }

/*     Just like it says above. Order the array, and remove duplicates. */

    card = *n;
    rmdupc_(&card, a + a_len * 6, a_len);

/*     Set the size and cardinality of the input set. */

    ssizec_(size, a, a_len);
    scardc_(&card, a, a_len);
    chkout_("VALIDC", (ftnlen)6);
    return 0;
} /* validc_ */

