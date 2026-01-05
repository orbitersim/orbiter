/* sharpr.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure SHARPR ( Sharpen a rotation ) */
/* Subroutine */ int sharpr_(doublereal *rot)
{
    extern /* Subroutine */ int ucrss_(doublereal *, doublereal *, doublereal 
	    *), vhatip_(doublereal *);

/* $ Abstract */

/*     Adjust the columns of a matrix that is "nearly" a rotation */
/*     so that they are numerically unit length and orthogonal, */
/*     going from left to right in the usual printed presentation */
/*     of a matrix. */

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

/*     MATRIX */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     ROT       I-O  The rotation matrix to be sharpened. */

/* $ Detailed_Input */

/*     ROT      is a 3x3 matrix that is nearly a rotation matrix. */

/* $ Detailed_Output */

/*     ROT      is the input after sharpening the columns. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  This routine is not meant to be used on singular or near- */
/*         singular matrices (in other words, matrices with determinant */
/*         close to zero). */

/*         If the input matrix is singular, the output matrix may not */
/*         be a rotation matrix. In any case, the results should be */
/*         considered unreliable in this case. */

/*         No error handling is done for invalid input matrices. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine "sharpens" the orthogonality of a potential */
/*     rotation matrix. It is intended for use in those situations */
/*     in which you have a rotation matrix that may be derived */
/*     from single precision inputs or that may have experienced */
/*     round off errors in its construction. */

/* $ Examples */

/*     Suppose that you have a rotation matrix that needs to be */
/*     converted to a quaternion. The SPICE matrix to quaternion */
/*     conversion routine M2Q performs error checks on the input */
/*     matrix and signals an error if it does not meet the checks */
/*     for a quaternion. By calling this routine you can ensure that */
/*     your rotation matrix (provided it's non-singular) will pass */
/*     the restrictions imposed by M2Q. */

/*         CALL SHARPR ( ROT ) */
/*         CALL M2Q    ( ROT, Q ) */

/* $ Restrictions */

/*     1)  See the $Exceptions section above. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.1, 26-OCT-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. Removed */
/*        unnecessary $Revisions section. */

/* -    SPICELIB Version 1.1.0, 13-OCT-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in VHAT call. Some header updates were made. */

/* -    SPICELIB Version 1.0.0, 16-SEP-1999 (WLT) */

/* -& */
/* $ Index_Entries */

/*     Sharpen the orthogonality of the columns of a rotation */

/* -& */

/*     Unitize the first column of the rotation. */

    vhatip_(rot);

/*     Unitize the third column of the rotation and make it */
/*     orthogonal to the first two columns. */

    ucrss_(rot, &rot[3], &rot[6]);

/*     Unitize the second column of the rotation and make it */
/*     orthogonal to the first and third columns. */

    ucrss_(&rot[6], rot, &rot[3]);
    return 0;
} /* sharpr_ */

