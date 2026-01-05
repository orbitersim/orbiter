/* vrotv.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__3 = 3;

/* $Procedure VROTV ( Vector rotation about an axis ) */
/* Subroutine */ int vrotv_(doublereal *v, doublereal *axis, doublereal *
	theta, doublereal *r__)
{
    /* Builtin functions */
    double cos(doublereal), sin(doublereal);

    /* Local variables */
    extern /* Subroutine */ int vadd_(doublereal *, doublereal *, doublereal *
	    ), vhat_(doublereal *, doublereal *), vsub_(doublereal *, 
	    doublereal *, doublereal *);
    doublereal c__, p[3], s, x[3];
    extern /* Subroutine */ int moved_(doublereal *, integer *, doublereal *),
	     vlcom_(doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *), vproj_(doublereal *, doublereal *, doublereal *);
    extern doublereal vnorm_(doublereal *);
    extern /* Subroutine */ int vcrss_(doublereal *, doublereal *, doublereal 
	    *);
    doublereal v1[3], v2[3], rplane[3];

/* $ Abstract */

/*     Rotate a vector about a specified axis vector by a specified */
/*     angle and return the rotated vector. */

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

/*     ROTATION */

/* $ Keywords */

/*     ROTATION */
/*     VECTOR */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     V          I   Vector to be rotated. */
/*     AXIS       I   Axis of the rotation. */
/*     THETA      I   Angle of rotation (radians). */
/*     R          O   Result of rotating V about AXIS by THETA. */

/* $ Detailed_Input */

/*     V        is a 3-dimensional vector to be rotated. */

/*     AXIS     is the axis about which the rotation is to be */
/*              performed. */

/*     THETA    is the angle through which V is to be rotated about */
/*              AXIS. */

/* $ Detailed_Output */

/*     R        is the result of rotating V about AXIS by THETA. */
/*              If AXIS is the zero vector, R = V. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  If the input axis is the zero vector, R will be returned */
/*         as V. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine computes the result of rotating (in a right handed */
/*     sense) the vector V about the axis represented by AXIS through */
/*     an angle of THETA radians. */

/*     If W is a unit vector parallel to AXIS, then R is given by: */

/*         R = V + ( 1 - cos(THETA) ) Wx(WxV) + sin(THETA) (WxV) */

/*     where "x" above denotes the vector cross product. */

/* $ Examples */

/*     If AXIS = ( 0, 0, 1 ) and THETA = PI/2 then the following results */
/*     for R will be obtained */

/*             V                           R */
/*        -------------             ---------------- */
/*        ( 1, 2, 3 )                ( -2, 1, 3 ) */
/*        ( 1, 0, 0 )                (  0, 1, 0 ) */
/*        ( 0, 1, 0 )                ( -1, 0, 0 ) */


/*     If AXIS = ( 0, 1, 0 ) and THETA = PI/2 then the following results */
/*     for R will be obtained */

/*             V                           R */
/*        -------------             ---------------- */
/*        ( 1, 2, 3 )                (  3, 2, -1 ) */
/*        ( 1, 0, 0 )                (  0, 0, -1 ) */
/*        ( 0, 1, 0 )                (  0, 1,  0 ) */


/*     If AXIS = ( 1, 1, 1 ) and THETA = PI/2 then the following results */
/*     for R will be obtained */

/*             V                                     R */
/*        -----------------------------   ----------------------------- */
/*        ( 1.0,     2.0,     3.0     )   ( 2.577.., 0.845.., 2.577.. ) */
/*        ( 2.577.., 0.845.., 2.577.. )   ( 3.0      2.0,     1.0     ) */
/*        ( 3.0      2.0,     1.0     )   ( 1.422.., 3.154.., 1.422.. ) */
/*        ( 1.422.., 3.154.., 1.422.. )   ( 1.0      2.0,     3.0     ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     H.A. Neilan        (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Removed */
/*        unnecessary $Revisions section. */

/* -    SPICELIB Version 1.0.2, 05-FEB-2003 (NJB) */

/*        Header examples were corrected.  $Exceptions section */
/*        filled in. Miscellaneous header corrections were made. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (NJB) (HAN) */

/* -& */
/* $ Index_Entries */

/*     vector rotation about an axis */

/* -& */

/*     SPICELIB functions */


/*     Local Variables */


/*     Just in case the user tries to rotate about the zero vector - */
/*     check, and if so return the input vector */

    if (vnorm_(axis) == 0.) {
	moved_(v, &c__3, r__);
	return 0;
    }

/*     Compute the unit vector that lies in the direction of the */
/*     AXIS.  Call it X. */

    vhat_(axis, x);

/*     Compute the projection of V onto AXIS.  Call it P. */

    vproj_(v, x, p);

/*     Compute the component of V orthogonal to the AXIS.  Call it V1. */

    vsub_(v, p, v1);

/*     Rotate V1 by 90 degrees about the AXIS and call the result V2. */

    vcrss_(x, v1, v2);

/*     Compute COS(THETA)*V1 + SIN(THETA)*V2. This is V1 rotated about */
/*     the AXIS in the plane normal to the axis, call the result RPLANE */

    c__ = cos(*theta);
    s = sin(*theta);
    vlcom_(&c__, v1, &s, v2, rplane);

/*     Add the rotated component in the normal plane to AXIS to the */
/*     projection of V onto AXIS (P) to obtain R. */

    vadd_(rplane, p, r__);

    return 0;
} /* vrotv_ */

