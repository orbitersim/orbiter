/* touchc.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure TOUCHC ( Touch a variable ) */
/* Character */ VOID touchc_(char *ret_val, ftnlen ret_val_len, char *string, 
	ftnlen string_len)
{
/* $ Abstract */

/*     Return the first character of a string */

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
/*     STRING     I   any character string */

/*      The function returns the first character of the string. */

/* $ Detailed_Input */

/*     STRING   is any character string */

/* $ Detailed_Output */

/*     The function returns the first character of the string */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This is a utility routine so that formal arguments to a routine */
/*     that are never used can be given the appearance of being used */
/*     to a compiler. In this way it is possible to leave "hooks" in */
/*     a calling sequence even if those variables are for the moment */
/*     unused. Similarly, variables declared for future use can be left */
/*     in place so that they don't need to be commented out */

/* $ Examples */

/*     Suppose that a routine takes as an argument a */
/*     fortran structure implemented as a set of parallel arrays. */
/*     But that one of the arrays is not needed for the purposes of */
/*     the routine. This routine allows you to touch that array */
/*     without changing it. */


/*        SUBROUTINE INCPTR ( N, PTR, VALUES ) */

/*        This routine increments the current pointer into a circular */
/*        array of character strings. */

/*        INTEGER               N */
/*        INTEGER               PTR */
/*        CHARACTER*(*)         VALUES ( * ) */

/*        Even though we don't need to do anything with the values */
/*        array, it's passed for the sake of uniformity in calling */
/*        sequences. Touch the VALUES array so that the compiler */
/*        will think it's been used. */

/*        VALUES(1)(1:1) = TOUCHC ( VALUES(1)(1:1) ) */

/*        PTR = PTR + 1 */

/*        IF ( PTR .GT. N ) THEN */
/*           PTR = 1 */
/*        END IF */
/*        RETURN */

/* $ Restrictions */

/*     1)  If you use this routine, it would be a very good idea to */
/*         write down why you are using it in the calling routine. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 16-AUG-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.0, 06-MAY-1994 (WLT) */

/* -& */
/* $ Index_Entries */

/*     touch a character string */

/* -& */
    *(unsigned char *)ret_val = *(unsigned char *)string;
    return ;
} /* touchc_ */

