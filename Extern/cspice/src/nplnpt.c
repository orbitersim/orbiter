/* nplnpt.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure NPLNPT ( Nearest point on line to point ) */
/* Subroutine */ int nplnpt_(doublereal *linpt, doublereal *lindir, 
	doublereal *point, doublereal *pnear, doublereal *dist)
{
    extern /* Subroutine */ int vadd_(doublereal *, doublereal *, doublereal *
	    );
    doublereal proj[3];
    extern /* Subroutine */ int vsub_(doublereal *, doublereal *, doublereal *
	    ), chkin_(char *, ftnlen);
    doublereal trans[3];
    extern doublereal vdist_(doublereal *, doublereal *);
    extern /* Subroutine */ int vproj_(doublereal *, doublereal *, doublereal 
	    *);
    extern logical vzero_(doublereal *);
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Find the nearest point on a line to a specified point, and find */
/*     the distance between the two points. */

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

/*     GEOMETRY */
/*     MATH */
/*     VECTOR */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     LINPT, */
/*     LINDIR     I   Point on a line and the line's direction vector. */
/*     POINT      I   A second point. */
/*     PNEAR      O   Nearest point on the line to POINT. */
/*     DIST       O   Distance between POINT and PNEAR. */

/* $ Detailed_Input */

/*     LINPT, */
/*     LINDIR   are, respectively, a point and a direction vector */
/*              that define a line in 3-dimensional space. The */
/*              line is the set of points */

/*                 LINPT   +   t * LINDIR */

/*              where `t' is any real number. */

/*     POINT    is a point in 3-dimensional space. */

/* $ Detailed_Output */

/*     PNEAR    is the nearest point on the input line to the input */
/*              point. */

/*     DIST     is the distance between the input line and input */
/*              point. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the line direction vector LINDIR is the zero vector, the */
/*         error SPICE(ZEROVECTOR) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     For every line L and point P, there is a unique closest point */
/*     on L to P. Call this closest point C. It is always true that */
/*     P - C  is perpendicular to L, and the length of P - C is called */
/*     the `distance' between P and L. */

/* $ Examples */

/*     1)  Suppose a line passes through the point ( 1, 2, 3 ) and */
/*         has direction vector ( 0, 1, 1 ).  We wish to find the */
/*         closest point on the line to the point ( -6, 9, 10 ).  We */
/*         can use the code fragment */

/*            LINPT(1)   =  1.D0 */
/*            LINPT(2)   =  2.D0 */
/*            LINPT(3)   =  3.D0 */

/*            LINDIR(1)  =  0.D0 */
/*            LINDIR(2)  =  1.D0 */
/*            LINDIR(3)  =  1.D0 */

/*            POINT(1)   = -6.D0 */
/*            POINT(2)   =  9.D0 */
/*            POINT(3)   = 10.D0 */

/*            CALL NPLNPT ( LINPT, LINDIR, POINT, PNEAR, DIST ) */

/*         After the call, PNEAR will take the value */

/*            ( 1.D0, 9.D0, 10.D0 ); */

/*         DIST will be 7.0. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.3.0, 27-MAY-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.2.0, 17-SEP-2014 (NJB) */

/*        Now uses discovery check-in. */

/* -    SPICELIB Version 1.1.0, 09-SEP-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in VADD call. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 02-NOV-1990 (NJB) */

/* -& */
/* $ Index_Entries */

/*     distance between point and line */
/*     nearest point on line to point */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Use discovery check-in. */

    if (return_()) {
	return 0;
    }

/*     We need a real direction vector to work with. */

    if (vzero_(lindir)) {
	chkin_("NPLNPT", (ftnlen)6);
	setmsg_("Direction vector must be non-zero.", (ftnlen)34);
	sigerr_("SPICE(ZEROVECTOR)", (ftnlen)17);
	chkout_("NPLNPT", (ftnlen)6);
	return 0;
    }

/*     We translate line and input point so as to put the line through */
/*     the origin.  Then the nearest point on the translated line to the */
/*     translated point TRANS is the projection of TRANS onto the line. */

    vsub_(point, linpt, trans);
    vproj_(trans, lindir, proj);
    vadd_(proj, linpt, pnear);
    *dist = vdist_(pnear, point);
    return 0;
} /* nplnpt_ */

