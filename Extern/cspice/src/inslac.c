/* inslac.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure INSLAC ( Insert at location in a character array ) */
/* Subroutine */ int inslac_(char *elts, integer *ne, integer *loc, char *
	array, integer *na, ftnlen elts_len, ftnlen array_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer size, i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen), sigerr_(char *, 
	    ftnlen), chkout_(char *, ftnlen), setmsg_(char *, ftnlen), 
	    errint_(char *, integer *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Insert one or more elements into a character array at the */
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
/*     ELTS       I   Elements to be inserted. */
/*     NE         I   Number of elements to be inserted. */
/*     LOC        I   Location of the first inserted element. */
/*     ARRAY     I-O  Input/output array. */
/*     NA        I-O  Number of elements in the input/output array. */

/* $ Detailed_Input */

/*     ELTS     contains one or more elements which are to be */
/*              inserted into the input array. */

/*     NE       is the number of elements to be inserted. */

/*     LOC      is the location in the array at which the first */
/*              element of ELTS is to be inserted. LOC must be */
/*              within the interval [1, NA+1]. To append to */
/*              ARRAY, set LOC equal to NA+1. */

/*     ARRAY    on input, is the original array. */

/*     NA       on input, is the number of elements in ARRAY. */

/* $ Detailed_Output */

/*     ARRAY    on output, is the original array with the elements */
/*              of ELT inserted into positions LOC through LOC+NE-1. */
/*              The original elements in these positions are moved */
/*              back to make room for the inserted elements. If the */
/*              new elements are longer than the declared lengths */
/*              of the elements of ARRAY, the new elements are */
/*              truncated on the right. */

/*     NA       on output, is the number of elements in ARRAY. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  The dimension of the array is set equal to zero if its */
/*         input value is less than one. */

/*     2)  If LOC is not in the interval [1, NA+1], the error */
/*         SPICE(INVALIDINDEX) is signaled. */

/*     3)  If the number of elements to be inserted is less than one, */
/*         the array is not modified. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The elements in positions LOC through LOC+NE-1 are moved back */
/*     by NE spaces to make room for the new elements, which are then */
/*     inserted into the vacated spaces. */

/* $ Examples */

/*     Let */

/*           ELTS(1) = 'very'       NA = 4      ARRAY(1) =  'I' */
/*           ELTS(2) = 'big'                    ARRAY(2) =  'saw' */
/*           ELTS(3) = 'brown'                  ARRAY(3) =  'a' */
/*                                              ARRAY(4) =  'dog' */

/*     Then the call */

/*           CALL INSLAC ( ELTS, 3, 4, ARRAY, NA ) */

/*     yields the following result: */

/*           NA = 7      ARRAY(1) = 'I' */
/*                       ARRAY(2) = 'saw' */
/*                       ARRAY(3) = 'a' */
/*                       ARRAY(4) = 'very' */
/*                       ARRAY(5) = 'big' */
/*                       ARRAY(6) = 'brown' */
/*                       ARRAY(7) = 'dog' */


/*     The following calls to INSLAC signal errors. */

/*     CALL INSLAC ( ELTS, 3, -1, ARRAY, NA ) */
/*     CALL INSLAC ( ELTS, 3,  6, ARRAY, NA ) */
/*     CALL INSLAC ( ELTS, 3,  2, ARRAY, -1 ) */
/*     CALL INSLAC ( ELTS, 3, -1, ARRAY, -1 ) */

/* $ Restrictions */

/*     1)  The array must be large enough to contain both the original */
/*         and the inserted elements. */

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

/*     insert at location in a character array */

/* -& */
/* $ Revisions */

/* -     Beta Version 2.0.0, 30-DEC-1988 (HAN) */

/*         If the location at which the elements are to be inserted is */
/*         not in the interval [1, NA+1], an error is signaled. */
/*         Locations not within that interval refer to non-existent */
/*         array elements. (To append to the array, the location */
/*         should be equal to NA+1.) */

/*         A negative dimension bug was fixed. The results of the */
/*         old version were unpredictable if the input array dimension */
/*         was negative. To avoid this problem the maximum of zero and */
/*         the input dimension becomes the dimension used by the */
/*         the routine. In this case, the only valid location at which */
/*         to insert is 1. If it is not 1, an error is signaled */
/*         when the location is checked. */

/* -& */

/*     SPICELIB functions */


/*     Other functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("INSLAC", (ftnlen)6);
    }

/*     Check the dimension of the array. */

    size = max(0,*na);

/*     Make sure the location at which the elements are to be inserted */
/*     is not out of range. If it is, signal an error and bail out. */

    if (*loc < 1 || *loc > size + 1) {
	setmsg_("Location was *.", (ftnlen)15);
	errint_("*", loc, (ftnlen)1);
	sigerr_("SPICE(INVALIDINDEX)", (ftnlen)19);
	chkout_("INSLAC", (ftnlen)6);
	return 0;
    }

/*     If the number of elements to be inserted is greater than zero, */
/*     insert them. If not, do not modify the array. */

    if (*ne > 0) {

/*        Move the trailing elements back to make room for the new ones. */

	i__1 = *loc;
	for (i__ = size; i__ >= i__1; --i__) {
	    s_copy(array + (i__ + *ne - 1) * array_len, array + (i__ - 1) * 
		    array_len, array_len, array_len);
	}

/*        Now put the new elements in the vacated spaces. */

	i__1 = *ne;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    s_copy(array + (*loc + i__ - 2) * array_len, elts + (i__ - 1) * 
		    elts_len, array_len, elts_len);
	}

/*        Update the number of elements in the array. */

	*na = size + *ne;
    }
    chkout_("INSLAC", (ftnlen)6);
    return 0;
} /* inslac_ */

