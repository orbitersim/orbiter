/* vhatip.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure VHATIP ( "V-Hat", 3-d unit vector along V, in place ) */
/* Subroutine */ int vhatip_(doublereal *v)
{
    doublereal vmag;
    extern doublereal vnorm_(doublereal *);

/* $ Abstract */

/*     Scale a three-dimensional vector to unit length. */

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

/*     VECTOR */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     V         I-O  Vector to be normalized/unit vector. */

/* $ Detailed_Input */

/*     V        is any double precision, 3-dimensional vector. If this */
/*              vector is the zero vector, this routine will detect it, */
/*              and will not attempt to divide by zero. */

/* $ Detailed_Output */

/*     V        is the unit vector in the direction of the input vector. */
/*              If on input V represents the zero vector, then V will be */
/*              returned as the zero vector. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  The zero vector is returned if the input value of V is the */
/*         zero vector. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine is provided for situation where it is convenient */
/*     to scale a vector to unit length in place rather than store */
/*     the result in a separate variable. Note that the call */

/*        CALL VHAT ( V, V ) */

/*     is not permitted by the ANSI Fortran 77 standard; this routine */
/*     can be called instead to achieve the same result. */

/*     VHATIP determines the magnitude of V and then, if the magnitude */
/*     is non-zero, divides each component of V by the magnitude. This */
/*     process is highly stable over the whole range of 3-dimensional */
/*     vectors. */

/* $ Examples */

/*     The following table shows how selected vectors are mapped to */
/*     unit vectors */

/*     V on input            V on output */
/*     ------------------    ------------------ */
/*     (5, 12, 0)            (5/13, 12/13, 0) */
/*     (1D-7, 2D-7, 2D-7)    (1/3, 2/3, 2/3) */

/* $ Restrictions */

/*     1)  There is no known case whereby floating point overflow may */
/*         occur. Thus, no error recovery or reporting scheme is */
/*         incorporated into this subroutine. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     H.A. Neilan        (JPL) */
/*     W.M. Owen          (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.0, 20-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.1.0, 01-SEP-2005 (NJB) (HAN) (WMO) (WLT) */

/* -& */
/* $ Index_Entries */

/*     unitize a 3-dimensional vector in place */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Obtain the magnitude of V. */

    vmag = vnorm_(v);

/*     If VMAG is nonzero, then normalize. Note that this process is */
/*     numerically stable: overflow could only happen if VMAG were */
/*     small, but this could only happen if each component of V1 were */
/*     small. In fact, the magnitude of any vector is never less than */
/*     the magnitude of any component. */

    if (vmag > 0.) {
	v[0] /= vmag;
	v[1] /= vmag;
	v[2] /= vmag;
    } else {
	v[0] = 0.;
	v[1] = 0.;
	v[2] = 0.;
    }
    return 0;
} /* vhatip_ */

