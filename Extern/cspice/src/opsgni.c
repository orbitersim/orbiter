/* opsgni.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure OPSGNI  ( Opposite Sign Integers ) */
logical opsgni_(integer *x, integer *y)
{
    /* System generated locals */
    logical ret_val;

/* $ Abstract */

/*     Return .TRUE. if two given integer numbers have opposite signs. */

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

/*     NUMBERS */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     X          I   An integer. */
/*     Y          I   An integer. */

/*     The function returns .TRUE. when the integer numbers X and Y have */
/*     opposite signs. */

/* $ Detailed_Input */

/*     X        is any integer number. */

/*     Y        is any integer number. */

/* $ Detailed_Output */

/*     The function returns .TRUE. if one of the pair X,Y is positive and */
/*     the other is negative. If either of the two values is zero, OPSGNI */
/*     will be .FALSE. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine returns the value: */

/*           (      (( X .GT. 0) .AND. (Y .LT. 0)) */
/*             .OR. (( X .LT. 0) .AND. (Y .GT. 0)) ) */

/*     This is a more stable value than */

/*           ( X*Y .LT. 0 ) */

/*     Note that if either of the two values is zero, OPSGNI will be */
/*     false. */

/* $ Examples */

/*     This routine can be used whenever a decision depends upon two */
/*     integer values having opposite signs. */

/*     IF ( OPSGNI ( F(X1), F(X2) ) ) THEN */
/*           . */
/*           . */
/*        find the value of F closest to zero. */
/*           . */
/*           . */
/*     ELSE */
/*           . */
/*           . */
/*        do something */
/*           . */
/*           . */
/*     END IF */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Extended */
/*        $Detailed_Output section to indicate the output value for */
/*        the case of either input being zero. */

/* -    SPICELIB Version 1.0.2, 07-NOV-2005 (BVS) */

/*        Fixed cut-and-paste errors in the header. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) */

/* -& */
/* $ Index_Entries */

/*     opposite sign integers */

/* -& */
    ret_val = *x > 0 && *y < 0 || *x < 0 && *y > 0;
    return ret_val;
} /* opsgni_ */

