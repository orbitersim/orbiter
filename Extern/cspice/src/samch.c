/* samch.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure SAMCH ( Same character ) */
logical samch_(char *str1, integer *l1, char *str2, integer *l2, ftnlen 
	str1_len, ftnlen str2_len)
{
    /* System generated locals */
    logical ret_val;

    /* Builtin functions */
    integer i_len(char *, ftnlen);

/* $ Abstract */

/*     Determine if two characters from different strings are the */
/*     same. */

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
/*     --------  ---  -------------------------------------------------- */
/*     STR1       I   A character string */
/*     L1         I   The location (index) of a character in STR1 */
/*     STR2       I   A character string */
/*     L2         I   The location (index) of a character in STR2 */

/*     The function returns .TRUE. if the two characters are the */
/*     same. */

/* $ Detailed_Input */

/*     STR1     is a character string */

/*     L1       is the location (index) of a character in STR1 */

/*     STR2     is a character string */

/*     L2       is the location (index) of a character in STR2 */

/* $ Detailed_Output */

/*     The function returns .TRUE. if the characters STR1(L1:L1) and */
/*     STR2(L2:L2) are the same. */

/*     If the characters are different or L1 or L2 is out of range the */
/*     function returns .FALSE. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  If either L1 or L2 is out of range the function returns */
/*         .FALSE. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This is a utility function for determining whether or not */
/*     two characters in different strings are the same. This */
/*     function is intended for situation in which you need to */
/*     search two strings for a match (or mismatch). */

/* $ Examples */

/*     Often you need to scan through two string comparing character */
/*     by character until a mismatch occurs. The usual way to code */
/*     this is */

/*        DO WHILE (        L1 .LE. LEN(STR1) */
/*                    .AND. L2 .LE. LEN(STR2) */
/*                    .AND. STR1(L1:L1) .EQ. STR2(L2:L2) ) */

/*           L1 = L1 + 1 */
/*           L2 = L2 + 1 */

/*        END DO */

/*        Check L1, L2 to make sure we are still in range, etc. */

/*     The problem with this loop is that even though the check to make */
/*     sure that L1 and L2 are in range is performed, FORTRAN may */
/*     go ahead and compute the equality condition even though one of the */
/*     first two steps failed. This can lead to out of range errors */
/*     and possible halting of your program depending upon how */
/*     the routine is compiled. An alternative way to code this is */

/*        IF ( L1 .LE. LEN(STR1) .AND. L2 .LE. LEN(STR2) ) THEN */
/*           ALIKE = STR1(L1:L1) .EQ. STR2(L2:L2) */
/*        ELSE */
/*           ALIKE = .FALSE. */
/*        END IF */

/*        DO WHILE ( ALIKE ) */

/*           L1 = L1 + 1 */
/*           L2 = L2 + 1 */

/*           IF ( L1 .LE. LEN(STR1) .AND. L2 .LE. LEN(STR2) ) THEN */
/*              ALIKE = STR1(L1:L1) .EQ. STR2(L2:L2) */
/*           ELSE */
/*              ALIKE = .FALSE. */
/*           END IF */
/*        END DO */

/*     However this is a much more complicated section of code. This */
/*     routine allows you to code the above loops as: */


/*        DO WHILE ( SAMCH ( STR1,L1, STR2,L2 )  ) */
/*           L1 = L1 + 1 */
/*           L2 = L2 + 1 */
/*        END DO */

/*     The boundary checks are automatically performed and out */
/*     of range errors are avoided. */

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

/*     Check two characters substrings for case sensitive equal */

/* -& */
    if (*l1 < 1 || *l2 < 1 || *l1 > i_len(str1, str1_len) || *l2 > i_len(str2,
	     str2_len)) {
	ret_val = FALSE_;
	return ret_val;
    }
    ret_val = *(unsigned char *)&str1[*l1 - 1] == *(unsigned char *)&str2[*l2 
	    - 1];
    return ret_val;
} /* samch_ */

