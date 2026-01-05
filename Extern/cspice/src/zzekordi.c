/* zzekordi.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      ZZEKORDI ( Order of an integer EK column ) */
/* Subroutine */ int zzekordi_(integer *ivals, logical *nullok, logical *
	nlflgs, integer *nvals, integer *iorder)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    integer i__, j;
    extern /* Subroutine */ int swapi_(integer *, integer *);
    integer jg, gap;

/* $ Abstract */

/*     Determine the order of elements in an integer EK column, using */
/*     dictionary ordering on integer data values and array indices. */

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

/*     EK */

/* $ Keywords */

/*     EK */
/*     SORT */

/* $ Declarations */
/* $ Brief_I/O */

/*      VARIABLE  I/O  DESCRIPTION */
/*      --------  ---  -------------------------------------------------- */
/*      IVALS      I   Array of integer column values. */
/*      NULLOK     I   Logical flag indicating whether nulls are allowed. */
/*      NLFLGS     I   Flags indicating whether column entries are null. */
/*      NVALS      I   Dimension of IVALS. */
/*      IORDER     O   Order vector for IVALS. */

/* $ Detailed_Input */

/*      IVALS          is an array of integer EK column values, */
/*                     some of which may be null, if null values are */
/*                     permitted.  See the description of the input */
/*                     arguments NULLOK and NLFLGS below. */

/*      NULLOK         is a logical flag indicating whether column */
/*                     elements may be null.  If NULLOK is TRUE, then */
/*                     NLFLGS must be set to indicate the status of each */
/*                     element of IVALS. */

/*      NLFLGS         is an array of logical flags that indicate whether */
/*                     the corresponding elements of IVALS are null. */
/*                     NLFLGS is meaningful only when NULLOK is .TRUE. */
/*                     When NULLOK is .TRUE., the Ith element of IVALS is */
/*                     null if and only if the Ith element of NLFLGS */
/*                     is .TRUE. */

/*                     When NULLOK is .FALSE., all elements of IVALS are */
/*                     considered to be non-null. */

/*      NVALS          is the number of elements in the input array. */

/* $ Detailed_Output */

/*      IORDER      is the order vector for the input array. */
/*                  IORDER(1) is the index of the smallest element */
/*                  of IVALS; IORDER(2) is the index of the next */
/*                  smallest; and so on.  Null values, if allowed, are */
/*                  considered to be less than all non-null values.  The */
/*                  order relation between equal values is determined */
/*                  by the indices of the values in the input array; */
/*                  values with lower indices are considered to be */
/*                  smaller. */

/* $ Parameters */

/*     None. */

/* $ Particulars */

/*     ZZEKORDI creates an order vector for an array of integer */
/*     column values.  Null values are allowed.  The order */
/*     relation used is dictionary ordering on ordered pairs consisting */
/*     of data values and array indices:  if two input data values */
/*     are equal, the associated array indices determine the order */
/*     relation of the values, where the smaller index is considered */
/*     to precede the greater. */

/* $ Examples */

/*      1)  Sort the following list of values, some of which are */
/*          null: */

/*                  Value                         Null? */
/*             --------------             --------------------- */
/*             IVALS(1)  =  3             NLFLGS(1)  =  .FALSE. */
/*             IVALS(2)  =  1             NLFLGS(2)  =  .FALSE. */
/*             IVALS(3)  =  4             NLFLGS(3)  =  .TRUE. */
/*             IVALS(4)  =  5             NLFLGS(4)  =  .FALSE. */
/*             IVALS(5)  =  2             NLFLGS(5)  =  .TRUE. */


/*          The subroutine call */

/*              CALL ZZEKORDI ( IVALS, .TRUE., NLFLGS, 5, IORDER ) */

/*          generates the output */

/*             IORDER(1)  =  3 */
/*             IORDER(2)  =  5 */
/*             IORDER(3)  =  2 */
/*             IORDER(4)  =  1 */
/*             IORDER(5)  =  4 */

/*          Note that the order of the null values is determined by */
/*          their indices in the input array. */


/*      2)  Given the same inputs values of IVALS and NLFLGS, the */
/*          subroutine call */

/*             CALL ZZEKORDI ( IVALS, .FALSE., NLFLGS, 5, IORDER ) */

/*          generates the output */

/*             IORDER(1)  =  2 */
/*             IORDER(2)  =  5 */
/*             IORDER(3)  =  1 */
/*             IORDER(4)  =  3 */
/*             IORDER(5)  =  4 */

/* $ Restrictions */

/*      None. */

/* $ Exceptions */

/*      Error free. */

/* $ Files */

/*      None. */

/* $ Author_and_Institution */

/*      N.J. Bachman    (JPL) */
/*      I.M. Underwood  (JPL) */

/* $ Literature_References */

/*      None. */

/* $ Version */

/* -     Beta Version 3.0.0, 26-MAY-1995 (NJB) */

/*         Re-written to use dictionary ordering on values and input */
/*         array indices. */

/* -     Beta Version 2.0.0, 13-FEB-1995 (NJB) */

/*         Renamed as a private routine. */

/* -     Beta Version 1.0.0, 13-APR-1994 (NJB) (IMU) */

/* -& */

/*     Local variables */


/*     Statement functions */


/*     Begin with the initial ordering. */

    i__1 = *nvals;
    for (i__ = 1; i__ <= i__1; ++i__) {
	iorder[i__ - 1] = i__;
    }

/*     Find the smallest element, then the next smallest, and so on. */
/*     This uses the Shell Sort algorithm, but swaps the elements of */
/*     the order vector instead of the array itself. */

    gap = *nvals / 2;
    while(gap > 0) {
	i__1 = *nvals;
	for (i__ = gap + 1; i__ <= i__1; ++i__) {
	    j = i__ - gap;
	    while(j > 0) {
		jg = j + gap;
		if (! (*nullok) && (ivals[iorder[j - 1] - 1] < ivals[iorder[
			jg - 1] - 1] || ivals[iorder[j - 1] - 1] == ivals[
			iorder[jg - 1] - 1] && iorder[j - 1] < iorder[jg - 1])
			 || *nullok && (nlflgs[iorder[j - 1] - 1] && ! nlflgs[
			iorder[jg - 1] - 1] || nlflgs[iorder[j - 1] - 1] && 
			nlflgs[iorder[jg - 1] - 1] && iorder[j - 1] < iorder[
			jg - 1] || ! (nlflgs[iorder[j - 1] - 1] || nlflgs[
			iorder[jg - 1] - 1]) && (ivals[iorder[j - 1] - 1] < 
			ivals[iorder[jg - 1] - 1] || ivals[iorder[j - 1] - 1] 
			== ivals[iorder[jg - 1] - 1] && iorder[j - 1] < 
			iorder[jg - 1]))) {

/*                 Getting here means that */

/*                    IVALS(IORDER(J)) .LE. IVALS(IORDER(JG)) */

/*                 according to our order relation. */

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
} /* zzekordi_ */

