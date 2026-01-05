/* samsub.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure SAMSUB (Same substrings) */
logical samsub_(char *str1, integer *b1, integer *e1, char *str2, integer *b2,
	 integer *e2, ftnlen str1_len, ftnlen str2_len)
{
    /* System generated locals */
    logical ret_val;

    /* Builtin functions */
    integer i_len(char *, ftnlen), s_cmp(char *, char *, ftnlen, ftnlen);

/* $ Abstract */

/*     Determine whether or not two substrings are the same */

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

/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  ------------------------------------------------- */
/*     STR1       I   A string */
/*     B1         I   Beginning of a substring in STR1 */
/*     E1         I   End of s substring in STR1 */
/*     STR2       I   A second string */
/*     B2         I   The beginning of a substring in STR2 */
/*     E2         I   The end  of s substring in STR2 */

/*     The function returns .TRUE. if the substrings are identical */

/* $ Detailed_Input */

/*     STR1     is a character string */

/*     B1       are integers giving the beginning and ending of a */
/*     E1       substring in STR1 */

/*     STR2     is a character string */

/*     B2       are integers giving the beginning and ending of a */
/*     E2       substring in STR2 */

/* $ Detailed_Output */

/*     The function returns .TRUE. if the two substrings STR(B1:E1) and */
/*     STR(B2:E2) have the same length and the same characters. */

/*     If any of the indices B1, E1, B2, E2 are out of range or out */
/*     of order the function returns .FALSE. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  If any of the B1, E1, B2, E2 are out of range or if an */
/*         ending substring index is before a beginning substring */
/*         index, the function returns false. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine is a macro for comparing two substrings of */
/*     strings and handles all of the bounds checking to avoid */
/*     out of range errors with string indices. */

/* $ Examples */

/*     Suppose a string contains a number of occurrences of some */
/*     particular substring in sequence and that you need to locate */
/*     the first character that is out of this sequence or the */
/*     end of the string. */

/*     If one ignores boundary constraints this can easily be */
/*     coded as shown here: We assume the particular substring is */

/*     '/beg' */

/*        B = 1 */
/*        E = B + LEN('/beg' ) */

/*        DO WHILE (       E           .LE. LEN(STR) */
/*                   .AND. STRING(B:E) .EQ. '/beg' ) */

/*           B = B + LEN('/beg') */
/*           E = E + LEN('/beg') */

/*        END DO */

/*        IF ( B .LT. LEN(STR) ) THEN */

/*           we've found the start of a substring of interest */

/*        ELSE */

/*           there is no substring to find. */

/*        END IF */

/*     Unfortunately, you can't rely upon FORTRAN to check the boundary */
/*     condition: E .LE. LEN(STR) and skip the second test if the first */
/*     condition if false. As a result you can get an out of range */
/*     error. */

/*     Instead you could code: */

/*     B = 1 */
/*     E = B + LEN('/beg') */

/*     IF ( E .LE. LEN(STR) ) THEN */
/*        ALIKE = STRINB(B:E) .EQ. '/beg' */
/*     ELSE */
/*        ALIKE = .FALSE. */
/*     END IF */

/*     DO WHILE ( ALIKE ) */

/*           B = B + LEN('/beg') */
/*           E = E + LEN('/beg') */

/*        IF ( E .LE. LEN(STR) ) THEN */
/*           ALIKE = STRINB(B:E) .EQ. '/beg' */
/*        ELSE */
/*           ALIKE = .FALSE. */
/*        END IF */

/*     END DO */


/*     However, this is code is far more effort. Using this routine */
/*     you can make a much simpler block of code. */

/*     B = 1 */
/*     E = B + LEN('/beg' ) */

/*     DO WHILE ( SAMSUB(STR,B,E, '/beg',1,4 ) ) */

/*        B = B + LEN('/beg') */
/*        E = E + LEN('/beg') */

/*     END DO */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 12-AUG-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.0, 31-MAR-1995 (WLT) */

/* -& */
/* $ Index_Entries */

/*     Check equality of two substrings. */

/* -& */
    if (*e1 < *b1 || *e2 < *b2 || *b1 < 1 || *b2 < 1 || *e1 > i_len(str1, 
	    str1_len) || *e2 > i_len(str2, str2_len) || *e1 - *b1 != *e2 - *
	    b2) {
	ret_val = FALSE_;
	return ret_val;
    }
    ret_val = s_cmp(str1 + (*b1 - 1), str2 + (*b2 - 1), *e1 - (*b1 - 1), *e2 
	    - (*b2 - 1)) == 0;
    return ret_val;
} /* samsub_ */

