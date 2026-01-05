/* vperp.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure VPERP ( Perpendicular component of a 3-vector ) */
/* Subroutine */ int vperp_(doublereal *a, doublereal *b, doublereal *p)
{
    /* System generated locals */
    doublereal d__1, d__2;

    /* Local variables */
    doublereal biga, bigb;
    extern /* Subroutine */ int vsub_(doublereal *, doublereal *, doublereal *
	    );
    doublereal r__[3], t[3], v[3];
    extern /* Subroutine */ int vproj_(doublereal *, doublereal *, doublereal 
	    *), vsclip_(doublereal *, doublereal *);

/* $ Abstract */

/*     Find the component of a vector that is perpendicular to a second */
/*     vector. All vectors are 3-dimensional. */

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
/*     A          I   The vector whose orthogonal component is sought. */
/*     B          I   The vector used as the orthogonal reference. */
/*     P          O   The component of A orthogonal to B. */

/* $ Detailed_Input */

/*     A        is a double precision, 3-dimensional vector. It the */
/*              vector whose component orthogonal to B is sought. (There */
/*              is a unique decomposition of A into a sum V + P, where V */
/*              is parallel to B and P is orthogonal to B. We want the */
/*              component P.) */

/*     B        is a double precision, 3-dimensional vector. This */
/*              vector is the vector used as a reference for the */
/*              decomposition of A. */

/* $ Detailed_Output */

/*     P        is a double precision, 3-dimensional vector containing */
/*              the component of A that is orthogonal to B. */
/*              P may overwrite either A or B. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Given and non-zero vector B and a vector A, there is a unique */
/*     decomposition of A as a sum V + P such that P is orthogonal */
/*     to B and V is parallel to B. This routine finds the vector P. */

/*     If B is a zero vector, P will be identical to A. */

/* $ Examples */

/*     The following table gives sample inputs and results from calling */
/*     VPERP. */

/*        A                  B                 P */
/*        ------------------------------------------ */
/*        (6, 6, 6)      ( 2, 0, 0)        (0, 6, 6) */
/*        (6, 6, 6)      (-3, 0, 0)        (0, 6, 6) */
/*        (6, 6, 0)      ( 0, 7, 0)        (6, 0, 0) */
/*        (6, 0, 0)      ( 0, 0, 9)        (6, 0, 0) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     [1]  G. Thomas and R. Finney, "Calculus and Analytic Geometry," */
/*          7th Edition, Addison Wesley, 1988. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.0, 16-SEP-2020 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.1.1, 11-MAY-2010 (EDW) */

/*        Minor edit to code comments eliminating typo. */

/*        Reordered header sections to proper NAIF convention. */
/*        Removed Revision section, it listed a duplication of a */
/*        $Version section entry. */

/* -    SPICELIB Version 1.1.0, 09-SEP-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in VSCL call. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) */

/* -& */
/* $ Index_Entries */

/*     perpendicular component of a 3-vector */

/* -& */

/*     Local variables */


/*     Error free routine:  no check-in. */

/* Computing MAX */
    d__1 = abs(a[0]), d__2 = abs(a[1]), d__1 = max(d__1,d__2), d__2 = abs(a[2]
	    );
    biga = max(d__1,d__2);
/* Computing MAX */
    d__1 = abs(b[0]), d__2 = abs(b[1]), d__1 = max(d__1,d__2), d__2 = abs(b[2]
	    );
    bigb = max(d__1,d__2);

/*     If A is the zero vector, just set P to zero and return. */

    if (biga == 0.) {
	p[0] = 0.;
	p[1] = 0.;
	p[2] = 0.;
	return 0;
    }

/*     If B is the zero vector, then set P equal to A. */

    if (bigb == 0.) {
	p[0] = a[0];
	p[1] = a[1];
	p[2] = a[2];
	return 0;
    }
    t[0] = a[0] / biga;
    t[1] = a[1] / biga;
    t[2] = a[2] / biga;
    r__[0] = b[0] / bigb;
    r__[1] = b[1] / bigb;
    r__[2] = b[2] / bigb;
    vproj_(t, r__, v);
    vsub_(t, v, p);
    vsclip_(&biga, p);
    return 0;
} /* vperp_ */

