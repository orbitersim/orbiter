/* remlac.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure REMLAC ( Remove elements from a character array ) */
/* Subroutine */ int remlac_(integer *ne, integer *loc, char *array, integer *
	na, ftnlen array_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen), sigerr_(char *, 
	    ftnlen), chkout_(char *, ftnlen), setmsg_(char *, ftnlen), 
	    errint_(char *, integer *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Remove one or more elements from a character array at the */
/*     indicated location. */

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
/*     NE         I   Number of elements to be removed. */
/*     LOC        I   Location of the first removed element. */
/*     ARRAY     I-O  Input/output array. */
/*     NA        I-O  Number of elements in the input/output array. */

/* $ Detailed_Input */

/*     NE       is the number of elements to be removed. */

/*     LOC      is the location in the array at which the first */
/*              element is to be removed. */

/*     ARRAY    on input, is the original array. */

/*     NA       on input, is the number of elements in ARRAY. */

/* $ Detailed_Output */

/*     ARRAY    on output, is the original array with elements */
/*              LOC through LOC+NE-1 removed. Succeeding elements */
/*              are moved forward to fill the vacated spaces. */

/*     NA       on output, is the number of elements in ARRAY. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If LOC is not in the interval [1, NA], the error */
/*         SPICE(INVALIDINDEX) is signaled. */

/*     2)  If the number of elements to be removed is greater than the */
/*         number of elements that can be removed, the error */
/*         SPICE(NONEXISTELEMENTS) is signaled. */

/*     3)  If NE is less than one, the array is not modified. */

/*     4)  If NA is less than one, any location is invalid, and, the */
/*         error SPICE(INVALIDINDEX) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The elements in positions LOC through LOC+NE-1 are overwritten */
/*     as the elements beginning at LOC+NE are moved back. */

/* $ Examples */

/*     Let */

/*           NA = 7      ARRAY(1) = 'The' */
/*                       ARRAY(2) = 'boy' */
/*                       ARRAY(3) = 'in' */
/*                       ARRAY(4) = 'the' */
/*                       ARRAY(5) = 'park' */
/*                       ARRAY(6) = 'fell' */
/*                       ARRAY(7) = 'down' */

/*     Then the call */

/*           CALL REMLAC ( 3, 3, ARRAY, NA ) */

/*     yields the following result: */

/*           NA = 4      ARRAY(1) = 'The' */
/*                       ARRAY(2) = 'boy' */
/*                       ARRAY(3) = 'fell' */
/*                       ARRAY(4) = 'down' */


/*     The following calls would signal errors: */

/*     CALL REMLAC ( 3,  1, ARRAY, -1 ) */
/*     CALL REMLAC ( 3, -1, ARRAY,  7 ) */
/*     CALL REMLAC ( 3,  6, ARRAY,  7 ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 05-JUN-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) */

/* -& */
/* $ Index_Entries */

/*     remove elements from a character array */

/* -& */
/* $ Revisions */

/* -     Beta Version 2.0.0, 1-JAN-1989 (HAN) */

/*         Code was added to handle the following exceptional */
/*         inputs. */

/*         If the dimension of the array is less than one, any */
/*         value of LOC is invalid. The old version did not check */
/*         the dimension of the array, and as a result, its output */
/*         was unpredictable. */

/*         If the location at which the elements are to be removed is */
/*         not in the interval [1, NA], an error is signaled. */
/*         Locations not within that interval refer to non-existent */
/*         array elements. The old routine did not signal an error. */
/*         It just returned the original array. */

/*         If the number of elements to be removed is greater than the */
/*         number of elements can be removed, an error is signaled. */
/*         In the old version, only those elements that could be */
/*         removed were removed, and no error was signaled. */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("REMLAC", (ftnlen)6);
    }

/*     If LOC does not point to an actual element, signal an error and */
/*     check out. If the dimension of the array is less than one, any */
/*     value of LOC is invalid, and an error is signaled. */

    if (*loc < 1 || *loc > *na) {
	setmsg_("Location was *.", (ftnlen)15);
	errint_("*", loc, (ftnlen)1);
	sigerr_("SPICE(INVALIDINDEX)", (ftnlen)19);
	chkout_("REMLAC", (ftnlen)6);
	return 0;

/*     Don't try to remove non-existent elements. */

    } else if (*ne > *na - *loc + 1) {
	setmsg_("Trying to remove non-existent elements.", (ftnlen)39);
	sigerr_("SPICE(NONEXISTELEMENTS)", (ftnlen)23);
	chkout_("REMLAC", (ftnlen)6);
	return 0;

/*     If there are elements to be removed, remove them. Otherwise, */
/*     do not modify the array. */

    } else if (*ne > 0) {

/*        Move the elements forward. */

	i__1 = *na - *ne;
	for (i__ = *loc; i__ <= i__1; ++i__) {
	    s_copy(array + (i__ - 1) * array_len, array + (i__ + *ne - 1) * 
		    array_len, array_len, array_len);
	}

/*        Update the number of elements in the array. */

	*na -= *ne;
    }
    chkout_("REMLAC", (ftnlen)6);
    return 0;
} /* remlac_ */

