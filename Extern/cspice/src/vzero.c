/* vzero.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure VZERO    ( Is a vector the zero vector? ) */
logical vzero_(doublereal *v)
{
    /* System generated locals */
    logical ret_val;

/* $ Abstract */

/*     Indicate whether a 3-vector is the zero vector. */

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

/*     MATH */
/*     VECTOR */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     V          I   Vector to be tested. */

/*     The function returns the value .TRUE. if and only if V is the */
/*     zero vector. */

/* $ Detailed_Input */

/*     V        is a vector in 3-space. */

/* $ Detailed_Output */

/*     The function returns the value .TRUE. if and only if V is the */
/*     zero vector. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This function has the same truth value as the logical expression */

/*        VNORM ( V )  .EQ.  0.D0 */

/*     Replacing the above expression by */

/*        VZERO ( V ) */

/*     has several advantages: the latter expresses the test more */
/*     clearly, looks better, and doesn't go through the work of scaling, */
/*     squaring, taking a square root, and re-scaling (all of which */
/*     VNORM must do) just to find out that a vector is non-zero. */

/*     A related function is VZEROG, which accepts vectors of arbitrary */
/*     dimension. */

/* $ Examples */

/*     1)  When testing whether a vector is the zero vector, one */
/*         normally constructs tests like */

/*            IF (  VNORM ( V )  .EQ.  0.D0  ) THEN */
/*                        . */
/*                        . */
/*                        . */


/*         These can be replaced with the code */

/*            IF (  VZERO ( V )  ) THEN */
/*                        . */
/*                        . */
/*                        . */


/*      2)  Check that a normal vector is non-zero before creating */
/*         a plane with PNV2PL: */

/*         IF (  VZERO ( NORMAL )  )  THEN */

/*            [ handle error ] */

/*         ELSE */

/*            CALL PNV2PL ( POINT, NORMAL, PLANE ) */
/*                          . */
/*                          . */
/*                          . */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 05-JUL-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 17-JUL-1990 (NJB) (IMU) */

/* -& */
/* $ Index_Entries */

/*     test whether a 3-dimensional vector is the zero vector */

/* -& */

/*     `Just do it'. */


    ret_val = v[0] == 0. && v[1] == 0. && v[2] == 0.;
    return ret_val;
} /* vzero_ */

