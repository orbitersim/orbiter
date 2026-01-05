/* inelpl.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static doublereal c_b26 = 1.;

/* $Procedure INELPL ( Intersection of ellipse and plane ) */
/* Subroutine */ int inelpl_(doublereal *ellips, doublereal *plane, integer *
	nxpts, doublereal *xpt1, doublereal *xpt2)
{
    /* System generated locals */
    doublereal d__1, d__2;

    /* Builtin functions */
    double acos(doublereal), atan2(doublereal, doublereal), cos(doublereal), 
	    sin(doublereal);

    /* Local variables */
    doublereal beta;
    extern doublereal vdot_(doublereal *, doublereal *), vsep_(doublereal *, 
	    doublereal *);
    extern /* Subroutine */ int vsub_(doublereal *, doublereal *, doublereal *
	    ), vequ_(doublereal *, doublereal *);
    doublereal alpha, v[2];
    extern /* Subroutine */ int chkin_(char *, ftnlen), errdp_(char *, 
	    doublereal *, ftnlen);
    doublereal const__, trans[4], point[3];
    extern logical vzero_(doublereal *);
    doublereal angle1, angle2;
    extern /* Subroutine */ int el2cgv_(doublereal *, doublereal *, 
	    doublereal *, doublereal *), vlcom3_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *), pl2nvc_(doublereal *, doublereal *, doublereal *), 
	    pl2nvp_(doublereal *, doublereal *, doublereal *), nvp2pl_(
	    doublereal *, doublereal *, doublereal *);
    extern doublereal halfpi_(void);
    doublereal center[3], inpcon, normal[3], smajor[3];
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    doublereal tmpvec[3];
    extern /* Subroutine */ int setmsg_(char *, ftnlen);
    doublereal sminor[3];
    extern doublereal vnormg_(doublereal *, integer *);
    extern logical vzerog_(doublereal *, integer *), return_(void);
    doublereal sep;

/* $ Abstract */

/*     Find the intersection of an ellipse and a plane. */

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

/*     ELLIPSES */
/*     PLANES */

/* $ Keywords */

/*     ELLIPSE */
/*     GEOMETRY */
/*     MATH */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     ELLIPS     I   A SPICE ellipse. */
/*     PLANE      I   A SPICE plane. */
/*     NXPTS      O   Number of intersection points of plane and ellipse. */
/*     XPT1, */
/*     XPT2       O   Intersection points. */

/* $ Detailed_Input */

/*     ELLIPS   is a SPICE ellipse. The ellipse is allowed to */
/*              be degenerate: one or both semi-axes may have */
/*              zero length. */

/*     PLANE    is a SPICE plane. */

/* $ Detailed_Output */

/*     NXPTS    is the number of points of intersection of the */
/*              geometric plane and ellipse represented by PLANE */
/*              and ELLIPS. NXPTS may take the values 0, 1, 2 or */
/*              -1. The value -1 indicates that the ellipse */
/*              consists of more than one point and lies in the */
/*              plane, so the number of intersection points is */
/*              infinite. */

/*              When the ellipse consists of a single point and */
/*              lies in the plane, NXPTS is set to 1. */

/*     XPT1, */
/*     XPT2     are the points of intersection of the input plane */
/*              and ellipse. If there is only one intersection */
/*              point, both XPT1 and XPT2 contain that point. If */
/*              the number of intersection points is zero or */
/*              infinite, the contents of XPT1 and XPT2 are */
/*              undefined. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the input plane is invalid, the error SPICE(INVALIDPLANE) */
/*         is signaled. The input plane must be a SPICE plane: the normal */
/*         vector must be non-zero and the constant must be non-negative. */

/*     2)  If the input ellipse has non-orthogonal axes, the error */
/*         SPICE(INVALIDELLIPSE) is signaled. */

/*     3)  The input ellipse is allowed to be a line segment or a point; */
/*         these cases are not considered to be errors. If the ellipse */
/*         consists of a single point and lies in the plane, the number */
/*         of intersection points is set to 1 (rather than -1) and */
/*         the output arguments XPT1 and XPT2 are assigned the value */
/*         of the ellipse's center. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine computes the intersection set of a non-degenerate */
/*     plane with a possibly degenerate ellipse. The ellipse is allowed */
/*     to consist of a line segment or a point. */

/*     A plane may intersect an ellipse in 0, 1, 2, or infinitely many */
/*     points. For there to be an infinite set of intersection points, */
/*     the ellipse must lie in the plane and consist of more than one */
/*     point. */

/* $ Examples */

/*     1)  If we want to find the angle of some ray above the limb of an */
/*         ellipsoid, where the angle is measured in a plane containing */
/*         the ray and a `down' vector, we can follow the procedure */
/*         given below. We assume the ray does not intersect the */
/*         ellipsoid. The result we seek is called ANGLE, imaginatively */
/*         enough. */

/*         We assume that all vectors are given in body-fixed */
/*         coordinates. */

/*            C */
/*            C     Find the limb of the ellipsoid as seen from the */
/*            C     point OBSERV. Here A, B, and C are the lengths of */
/*            C     the semi-axes of the ellipsoid. */
/*            C */
/*                  CALL EDLIMB ( A, B, C, OBSERV, LIMB ) */

/*            C */
/*            C     The ray direction vector is RAYDIR, so the ray is the */
/*            C     set of points */
/*            C */
/*            C        OBSERV  +  t * RAYDIR */
/*            C */
/*            C     where t is any non-negative real number. */
/*            C */
/*            C     The `down' vector is just -OBSERV. The vectors */
/*            C     OBSERV and RAYDIR are spanning vectors for the plane */
/*            C     we're interested in. We can use PSV2PL to represent */
/*            C     this plane by a SPICE plane. */
/*            C */
/*                  CALL PSV2PL ( OBSERV, OBSERV, RAYDIR, PLANE ) */

/*            C */
/*            C     Find the intersection of the plane defined by OBSERV */
/*            C     and RAYDIR with the limb. */
/*            C */
/*                  CALL INELPL ( LIMB, PLANE, NXPTS, XPT1, XPT2 ) */

/*            C */
/*            C     We always expect two intersection points, if DOWN */
/*            C     is valid. */
/*            C */
/*                  IF ( NXPTS .LT. 2 ) THEN */

/*                     [ do something about the error ] */

/*                  ENDIF */

/*            C */
/*            C     Form the vectors from OBSERV to the intersection */
/*            C     points. Find the angular separation between the */
/*            C     boresight ray and each vector from OBSERV to the */
/*            C     intersection points. */
/*            C */
/*                  CALL VSUB   ( XPT1, OBSERV, VEC1 ) */
/*                  CALL VSUB   ( XPT2, OBSERV, VEC2 ) */

/*                  SEP1 = VSEP ( VEC1, RAYDIR ) */
/*                  SEP2 = VSEP ( VEC2, RAYDIR ) */

/*            C */
/*            C     The angular separation we're after is the minimum of */
/*            C     the two separations we've computed. */
/*            C */
/*                  ANGLE = MIN ( SEP1, SEP2 ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 3.1.0, 24-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 3.0.0, 07-OCT-2011 (NJB) */

/*        Relaxed ellipse semi-axes orthogonality test limit */
/*        SEPLIM from 1.D-12 TO 1.D-9 radians. The angular */
/*        separation of the axes of the input ellipse must not */
/*        differ from pi/2 radians by more than this limit. */

/* -    SPICELIB Version 2.0.0, 14-JAN-2008 (NJB) */

/*        Bug fix: the routine's specification and behavior have been */
/*        updated so the routine now returns a meaningful result for the */
/*        case of an ellipse consisting of a single point. */

/*        Bug fix: in the degenerate case where the input ellipse is a */
/*        line segment of positive length, and this segment intersects */
/*        the plane, the number of intersection points is set to 1 */
/*        rather than 2. */

/*        Invalid input planes and ellipses are now diagnosed. */

/* -    SPICELIB Version 1.2.0, 25-AUG-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in VSUB call. */

/* -    SPICELIB Version 1.1.0, 24-MAR-1992 (NJB) (WLT) */

/*        Output arguments XPT1, XPT2 are now correctly declared */
/*        with length 3. Comment section for permuted index source */
/*        lines was added following the header. */

/* -    SPICELIB Version 1.0.0, 02-NOV-1990 (NJB) */

/* -& */
/* $ Index_Entries */

/*     intersection of ellipse and plane */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 2.0.0, 14-JAN-2008 (NJB) */

/*        Bug fix: the routine's specification and behavior have been */
/*        updated so the routine now returns a meaningful result for the */
/*        case of an ellipse consisting of a single point. In this case, */
/*        if an intersection is found, the number of intersection points */
/*        is set to 1 and both intersection arguments are set equal to */
/*        the ellipse's center. */

/*        Bug fix: in the degenerate case where the input ellipse is a */
/*        line segment of positive length, and this segment intersects */
/*        the plane, the number of intersection points is set to 1 */
/*        rather than 2. */

/*        Invalid input planes and ellipses are now diagnosed. */
/*        Error handling code has been added to trap errors that had */
/*        been erroneously passed off to lower level routines for */
/*        diagnosis. */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("INELPL", (ftnlen)6);

/*     Check the input plane. */

    pl2nvc_(plane, normal, &inpcon);
    if (vzero_(normal)) {
	setmsg_("Input SPICE plane has zero normal vector.", (ftnlen)41);
	sigerr_("SPICE(INVALIDPLANE)", (ftnlen)19);
	chkout_("INELPL", (ftnlen)6);
	return 0;
    } else if (inpcon < 0.) {
	setmsg_("Input SPICE plane has non-positive constant #. Properly con"
		"structed SPICE planes always have non-negative constants.", (
		ftnlen)116);
	errdp_("#", &inpcon, (ftnlen)1);
	sigerr_("SPICE(INVALIDPLANE)", (ftnlen)19);
	chkout_("INELPL", (ftnlen)6);
	return 0;
    }

/*     Get the components of the input ellipse; check for */
/*     invalid semi-axes. The semi-axes may have zero length */
/*     but they must always be orthogonal. We require this */
/*     check only if both semi-axes have non-zero length. */

    el2cgv_(ellips, center, smajor, sminor);
    if (! vzero_(sminor)) {
	sep = vsep_(smajor, sminor);
	if ((d__1 = sep - halfpi_(), abs(d__1)) > 1e-9) {
	    setmsg_("Input SPICE ellipse has non-orthogonal semi-axes: (#,#,"
		    "#) and (#,#,#). Angular separation of these vectors is #"
		    " radians. Properly constructed SPICE ellipses always hav"
		    "e orthogonal semi-axes.", (ftnlen)190);
	    errdp_("#", smajor, (ftnlen)1);
	    errdp_("#", &smajor[1], (ftnlen)1);
	    errdp_("#", &smajor[2], (ftnlen)1);
	    errdp_("#", sminor, (ftnlen)1);
	    errdp_("#", &sminor[1], (ftnlen)1);
	    errdp_("#", &sminor[2], (ftnlen)1);
	    errdp_("#", &sep, (ftnlen)1);
	    sigerr_("SPICE(INVALIDELLIPSE)", (ftnlen)21);
	    chkout_("INELPL", (ftnlen)6);
	    return 0;
	}
    }

/*     If the input ellipse is a single point, decide now */
/*     whether the ellipse lies in the plane. */

    if (vzero_(smajor)) {

/*        The ellipse is a single point. If the ellipse's center */
/*        lies in the plane, the whole ellipse is the one */
/*        intersection point. Check the inner product of the */
/*        center and the plane's normal vector. */

	if (vdot_(center, normal) == inpcon) {

/*           The center does in fact lie in the plane. */

	    *nxpts = 1;
	    vequ_(center, xpt1);
	    vequ_(center, xpt2);
	} else {

/*           There's no intersection: the intersection arguments */
/*           are left undefined in this case. */

	    *nxpts = 0;
	}

/*        Return now; this simplifies the logic to follow. */

	chkout_("INELPL", (ftnlen)6);
	return 0;
    }

/*     At this point the ellipse may still be degenerate: it can be a */
/*     line segment. We'll need to compute the intersection point or */
/*     points if we have a positive, finite intersection set. */

/*     The first thing we want to do is translate the plane and the */
/*     ellipse so as to center the ellipse at the origin.  To translate */
/*     the plane, just get a point and normal vector, and translate */
/*     the point.  Find the plane constant of the translated plane. */

    pl2nvp_(plane, normal, tmpvec);
    vsub_(tmpvec, center, point);
    nvp2pl_(normal, point, trans);
    pl2nvc_(trans, normal, &const__);

/*     Ok, we can get to work.  The locus of the ellipse is */

/*        cos(theta) SMAJOR  +  sin(theta) SMINOR, */

/*     and any point X of the ellipse that intersects the input plane */
/*     satisfies */

/*        < X, NORMAL >  =  CONST. */

/*     Substituting our expression for points on the ellipse into the */
/*     second equation, we arrive at */

/*           cos(theta) < SMAJOR, NORMAL > */
/*        +  sin(theta) < SMINOR, NORMAL >   =  CONST.        (1) */

/*     This equation merits a little analysis. First, if NORMAL */
/*     is orthogonal to SMAJOR and SMINOR, the plane and ellipse must */
/*     be parallel. Also, the left side of the equation is zero in */
/*     this case. If CONST is non-zero, there are no solutions: */
/*     the ellipse and plane are parallel but do not intersect. If */
/*     CONST is zero, the ellipse lies in the plane: all values of */
/*     theta are solutions. Let's get this case out of the way */
/*     right now, shall we? */

    v[0] = vdot_(smajor, normal);
    v[1] = vdot_(sminor, normal);

/*     Test whether the plane and ellipse are parallel: */

    if (vzerog_(v, &c__2)) {

/*        The ellipse lies in the plane if and only if CONST is zero. */
/*        In any case, we don't modify XPT1 or XPT2. */

	if (const__ == 0.) {
	    *nxpts = -1;
	} else {
	    *nxpts = 0;
	}
	chkout_("INELPL", (ftnlen)6);
	return 0;
    }

/*     Now if NORMAL is not orthogonal to both SMAJOR and SMINOR, */
/*     the vector */

/*        V = (  < SMAJOR, NORMAL >,  < SMINOR, NORMAL >  ) */

/*     is non-zero.  We can re-write (1) as */

/*        < U, V >  =  CONST, */

/*     where */

/*        U = ( cos(theta), sin(theta) ). */

/*     If alpha is the angle between U and V, we have */

/*        < U, V >  =  || U ||  *  || V ||  *  cos(alpha), */

/*     so */

/*        || V ||  *  cos(alpha)  =  CONST.                   (2) */

/*     CONST is positive, since PL2NVC returns the distance */
/*     of between its input plane and the origin as the output */
/*     plane constant. */

/*     Equation (2) has solutions if and only if */

/*        || V ||  >    CONST.                                (3) */
/*                 - */

/*     Let's return right now if there are no solutions. */

    if (vnormg_(v, &c__2) < const__) {
	*nxpts = 0;
	chkout_("INELPL", (ftnlen)6);
	return 0;
    }

/*     Since (3) above is satisfied, the plane and ellipse intersect. */
/*     We can find alpha using the formula */

/*        alpha  =  +  arccos (  CONST  /  || V ||  ) */

/*     Since alpha is the angular separation between U and V, we */
/*     can find U once we have the angular position of V; let's */
/*     call that beta.  The angular position of U (which we called */
/*     theta earlier) will be */

/*        theta   =   beta  +  alpha. */
/*                          - */

/*     The values of theta are the angles we seek. */

    alpha = acos(const__ / vnormg_(v, &c__2));
    beta = atan2(v[1], v[0]);
    angle1 = beta - alpha;
    angle2 = beta + alpha;

/*     Determine the number of intersection points. We have a special */
/*     case if the semi-minor axis has length zero: in that case BETA is */
/*     zero or Pi, and although ANGLE1 and ANGLE2 may differ, the */
/*     cosines of these angles are identical. Since in this case */
/*     the solutions corresponding to ANGLE1 and ANGLE2 have the */
/*     form */

/*        CENTER + cos(ANGLE1)*SMAJOR */
/*        CENTER + cos(ANGLE2)*SMAJOR */

/*     the solutions are identical. */


    if (vzero_(sminor)) {
	*nxpts = 1;
    } else {
	if (angle1 == angle2) {

/*           This case occurs when ALPHA is zero. */

	    *nxpts = 1;
	} else {
	    *nxpts = 2;
	}
    }

/*     Compute the intersection points. */

    d__1 = cos(angle1);
    d__2 = sin(angle1);
    vlcom3_(&c_b26, center, &d__1, smajor, &d__2, sminor, xpt1);
    d__1 = cos(angle2);
    d__2 = sin(angle2);
    vlcom3_(&c_b26, center, &d__1, smajor, &d__2, sminor, xpt2);
    chkout_("INELPL", (ftnlen)6);
    return 0;
} /* inelpl_ */

