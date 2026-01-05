/* rtrim.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure RTRIM ( Right trim ) */
integer rtrim_(char *string, ftnlen string_len)
{
    /* System generated locals */
    integer ret_val, i__1, i__2;

    /* Local variables */
    extern integer lastnb_(char *, ftnlen);

/* $ Abstract */

/*     Return the maximum of 1 and the location of the last non-blank */
/*     character in the string. */

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

/*     CHARACTER */
/*     STRING */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */

/*     STRING     I   String to be trimmed. */

/*     The function returns the maximum of 1 and the location of the */
/*     last non-blank character in STRING. */

/* $ Detailed_Input */

/*     STRING   is a string to be trimmed: the location of the */
/*              last non-blank character is desired. */

/* $ Detailed_Output */

/*     The function returns the maximum of 1 and the location of the */
/*     last non-blank character in STRING. */

/*     In particular, when STRING is blank, the function returns the */
/*     value 1. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     When writing a character string to a file, we usually are content */
/*     to omit the trailing blanks. We'd like to use LASTNB as an upper */
/*     substring bound, but we have to handle the case where LASTNB */
/*     returns 0, so we write: */


/*        WRITE ( UNIT, '(A)' ),  STRING ( : MAX (1, LASTNB (STRING)) ) */


/*     This can be simplified using RTRIM: */


/*        WRITE ( UNIT, '(A)' ),  STRING ( : RTRIM (STRING) )  ) */


/*     This routine has a counterpart, LTRIM, which finds the maximum of */
/*     1 and the position of the first non-blank character of a string. */

/* $ Examples */

/*     1)  Write the non-blank portion of each element of a character */
/*         cell to file SPUD.DAT: */

/*            DO I = 1,  CARDC (CELL) */

/*               CALL WRLINE ( 'SPUD.DAT', */
/*           .                  CELL(I) ( LTRIM (CELL) : RTRIM (CELL) ) ) */

/*            END DO */

/*         When CELL(I) is blank, the string ' ' will be written. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 02-MAY-1990 (NJB) */

/* -& */
/* $ Index_Entries */

/*     right trim */

/* -& */

/*     SPICELIB functions */


/*     `Just do it'. */

/* Computing MAX */
    i__1 = 1, i__2 = lastnb_(string, string_len);
    ret_val = max(i__1,i__2);
    return ret_val;
} /* rtrim_ */

