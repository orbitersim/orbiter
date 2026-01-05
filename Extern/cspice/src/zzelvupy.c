/* zzelvupy.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__10000 = 10000;
static doublereal c_b79 = 2.;
static doublereal c_b90 = .5;

/* $Procedure      ZZELVUPY ( Is ellipse in polygonal field of view? ) */
/* Subroutine */ int zzelvupy_(doublereal *ellips, doublereal *vertex, 
	doublereal *axis, integer *n, doublereal *bounds, logical *found)
{
    /* Initialized data */

    static doublereal origin[3] = { 0.,0.,0. };

    /* System generated locals */
    integer bounds_dim2, i__1, i__2, i__3;
    doublereal d__1, d__2;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    double asin(doublereal), pow_dd(doublereal *, doublereal *);

    /* Local variables */
    doublereal asep, apex[3];
    extern /* Subroutine */ int vhat_(doublereal *, doublereal *);
    extern doublereal vdot_(doublereal *, doublereal *), vsep_(doublereal *, 
	    doublereal *);
    extern /* Subroutine */ int vsub_(doublereal *, doublereal *, doublereal *
	    );
    static doublereal work[30000]	/* was [3][10000] */;
    doublereal edge1[3], edge2[3], a, b, d__;
    integer i__, j;
    doublereal vxpt1[3], vxpt2[3], scale, x, y;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    doublereal plane[4];
    extern /* Subroutine */ int errdp_(char *, doublereal *, ftnlen), vlcom_(
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *);
    extern doublereal vdist_(doublereal *, doublereal *);
    doublereal vtemp[3];
    extern /* Subroutine */ int ucrss_(doublereal *, doublereal *, doublereal 
	    *);
    extern doublereal vnorm_(doublereal *);
    extern logical vzero_(doublereal *);
    integer nxpts;
    extern /* Subroutine */ int el2cgv_(doublereal *, doublereal *, 
	    doublereal *, doublereal *), cgv2el_(doublereal *, doublereal *, 
	    doublereal *, doublereal *);
    doublereal hafedg;
    extern /* Subroutine */ int nvp2pl_(doublereal *, doublereal *, 
	    doublereal *);
    doublereal cp[3];
    extern /* Subroutine */ int psv2pl_(doublereal *, doublereal *, 
	    doublereal *, doublereal *);
    extern doublereal pi_(void);
    doublereal hafsec, eplane[4], ellscl[9], center[3], easize, ebsctr[3];
    extern /* Subroutine */ int saelgv_(doublereal *, doublereal *, 
	    doublereal *, doublereal *), inelpl_(doublereal *, doublereal *, 
	    integer *, doublereal *, doublereal *);
    doublereal ctrvec[3], consep, offset[3], pasize, smajor[3];
    char errmsg[1840];
    extern /* Subroutine */ int sigerr_(char *, ftnlen), setmsg_(char *, 
	    ftnlen);
    doublereal fovpln[4], vbsctr[3];
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    doublereal sminor[3];
    extern /* Subroutine */ int errint_(char *, integer *, ftnlen), repmot_(
	    char *, char *, integer *, char *, char *, ftnlen, ftnlen, ftnlen,
	     ftnlen);
    doublereal gv1[3];
    extern logical return_(void);
    doublereal gv2[3];
    extern /* Subroutine */ int inrypl_(doublereal *, doublereal *, 
	    doublereal *, integer *, doublereal *);
    extern integer zzwind_(doublereal *, integer *, doublereal *, doublereal *
	    );
    doublereal xpt[3], xpt1[3], xpt2[3];

/* $ Abstract */

/*     Determine whether a specified ellipse intersects the pyramid */
/*     defined by a polygonal field of view. */

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
/*     PLANE */

/* $ Declarations */
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


/*     Include File:  SPICELIB Error Handling Parameters */

/*        errhnd.inc  Version 2    18-JUN-1997 (WLT) */

/*           The size of the long error message was */
/*           reduced from 25*80 to 23*80 so that it */
/*           will be accepted by the Microsoft Power Station */
/*           FORTRAN compiler which has an upper bound */
/*           of 1900 for the length of a character string. */

/*        errhnd.inc  Version 1    29-JUL-1997 (NJB) */



/*     Maximum length of the long error message: */


/*     Maximum length of the short error message: */


/*     End Include File:  SPICELIB Error Handling Parameters */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     ELLIPS     I   A SPICELIB ellipse. */
/*     VERTEX     I   Vertex of a pyramid. */
/*     AXIS       I   Axis of a pyramid. */
/*     N          I   Number of boundary vectors of the pyramid. */
/*     BOUNDS     I   Boundary vectors of the pyramid. */
/*     FOUND      O   Flag indicating whether intersection was found. */
/*     UBEL       P   Upper bound of SPICELIB ellipse array. */
/*     UBPL       P   Upper bound of SPICELIB plane array. */
/*     MAXFOV     P   Maximum number of boundary vectors. */

/* $ Detailed_Input */

/*     ELLIPS         is a SPICELIB ellipse having non-zero semi-axes. */

/*     VERTEX         is the single point of intersection of the vectors */
/*                    defining the edges of a pyramid.  The vectors */
/*                    emanate from this point.  The pyramid represents */
/*                    the spatial region viewed by a polygonal field of */
/*                    view (FOV). */

/*     AXIS           is a vector emanating from VERTEX that lies inside */
/*                    the pyramid defined by VERTEX, N, and BOUNDS. */
/*                    AXIS represents the boresight direction of the FOV. */

/*     N, */
/*     BOUNDS         are, respectively, the number of boundary vectors */
/*                    defining the pyramid and the boundary vectors */
/*                    themselves.  Each pair of consecutive vectors in */
/*                    the array BOUNDS, together with VERTEX, defines a */
/*                    face of the pyramid. */

/*                    Each boundary vector must have angular separation */
/*                    of less than pi/2 radians from AXIS. */

/*                    For any plane that doesn't contain VERTEX and that */
/*                    intersects AXIS at right angles, the intersections */
/*                    of the boundary vectors with that plane are the */
/*                    vertices of a polygon.  The polygon need not be */
/*                    convex, but it must be non-self-intersecting. */


/* $ Detailed_Output */

/*     FOUND          is set to .TRUE. if the pyramid and ellipse */
/*                    intersect; otherwise FOUND is .FALSE. */

/* $ Parameters */

/*     UBEL           is the array upper bound for SPICELIB ellipses. */

/*     UBPL           is the array upper bound for SPICELIB planes. */

/*     MAXFOV         is the maximum number of boundary vectors that */
/*                    may be supplied in the input array argument */
/*                    BOUNDS. */

/* $ Exceptions */

/*     If an error is found, the output argument FOUND will be set to */
/*     .FALSE. */


/*     1)  If either of the semi-axes of the input ellipse is the */
/*         zero vector, the error SPICE(ZEROVECTOR) will be signaled. */

/*     2)  If the norm of the input ellipse's semi-minor axis is */
/*         zero after division by the maximum of the norms of the */
/*         semi-major axis, the ellipse's center, and the vertex of */
/*         the pyramid, the error SPICE(DEGENERATECASE) will be */
/*         signaled. */

/*     3)  If the vertex of the pyramid lies in the plane containing */
/*         the ellipse, at most the edge of the ellipse can be "seen" */
/*         from the vertex.  This case is not considered to be an */
/*         error. */

/*     4)  If the number of boundary vectors N is not at least 3, */
/*         or if the number exceeds MAXFOV, the error */
/*         SPICE(INVALIDCOUNT) will be signaled. */

/*     5)  If any boundary vector is the zero vector, the error */
/*         SPICE(ZEROVECTOR) will be signaled. */

/*     6)  If the axis is the zero vector, the error SPICE(ZEROVECTOR) */
/*         will be signaled. */

/*     7)  If any boundary vector has angular separation of at least */
/*         pi/2 radians from AXIS, the error SPICE(INVALIDFOV) */
/*         will be signaled. */

/*     8)  If any boundary vector has angular separation of zero */
/*         radians from one of its neighbors, the error SPICE(INVALIDFOV) */
/*         will be signaled. */

/*     9)  No test is done to ensure that the input boundary vectors */
/*         define a non-self-intersecting polygon via their intersection */
/*         with a plane normal to AXIS.  If the boundary vectors don't */
/*         meet this condition, the results of this routine are */
/*         unreliable. */

/*     10) The pyramidal field of view and the input ellipse must not */
/*         differ too radically in scale, or great loss of precision */
/*         will result, making the results of this routine unreliable. */
/*         For example, if the ratio of the norm of the semi-minor axis */
/*         of the ellipse to the distance from VERTEX to the center of */
/*         the ellipse is less than double precision epsilon on the host */
/*         system, a meaningful result can't be computed. */

/*         This routine does not attempt to judge the minimum */
/*         acceptable level of accuracy. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine is useful for determining whether an ellipsoidal */
/*     body is in the field of view of a remote-sensing instrument */
/*     with a field of view having polygonal cross section. */

/* $ Examples */

/*     Test an ellipse for intersection with a square field */
/*     of view. */


/*           PROGRAM EX1 */
/*           IMPLICIT NONE */

/*           INTEGER               MAXN */
/*           PARAMETER           ( MAXN   = 4 ) */

/*           INTEGER               UBEL */
/*           PARAMETER           ( UBEL   = 9 ) */

/*           DOUBLE PRECISION      AXIS   ( 3 ) */
/*           DOUBLE PRECISION      CENTER ( 3 ) */
/*           DOUBLE PRECISION      ELLIPS ( UBEL ) */
/*           DOUBLE PRECISION      FOV    ( 3, MAXN ) */
/*           DOUBLE PRECISION      SMAJOR ( 3 ) */
/*           DOUBLE PRECISION      SMINOR ( 3 ) */
/*           DOUBLE PRECISION      VERTEX ( 3 ) */

/*           INTEGER               N */

/*           LOGICAL               FOUND */

/*     C */
/*     C     The FOV (field of view) "looks" in the -x direction: */
/*     C     the axis of the FOV is parallel to the x axis. */
/*     C     The FOV intersects the plane of the ellipse in a */
/*     C     square having height and width 4 units.  The edges */
/*     C     of the square are parallel to the y and z axes. */
/*     C */
/*           N = 4 */

/*           CALL VPACK ( -1.D0,  -1.D0, -1.D0,  FOV(1,1) ) */
/*           CALL VPACK ( -1.D0,   1.D0, -1.D0,  FOV(1,2) ) */
/*           CALL VPACK ( -1.D0,   1.D0,  1.D0,  FOV(1,3) ) */
/*           CALL VPACK ( -1.D0,  -1.D0,  1.D0,  FOV(1,4) ) */

/*           CALL VPACK ( -1.D0,   0.D0,  0.D0,  AXIS     ) */
/*           CALL VPACK (  1.D0,   0.D0,  0.D0,  VERTEX   ) */

/*     C */
/*     C     The ellipse is oriented with the major axis */
/*     C     vertical and is parallel to the x-z plane.  The ellipse */
/*     C     lies in the plane defined by x = -1.  The ellipse */
/*     C     ever-so-slightly overlaps the bottom edge of the FOV. */
/*     C */
/*           CALL VPACK (  0.D0,   0.D0,   1.D0,           SMAJOR ) */
/*           CALL VPACK (  0.D0,   5.D-1,  0.D0,           SMINOR ) */
/*           CALL VPACK ( -1.D0,   0.D0,  -3.D0 + 1.D-12,  CENTER ) */

/*     C */
/*     C     Create a SPICELIB ellipse from the center and semi-axes. */
/*     C */
/*           CALL CGV2EL ( CENTER, SMAJOR, SMINOR, ELLIPS ) */

/*     C */
/*     C     Test for intersection.  We expect an intersection to be */
/*     C     found. */
/*     C */
/*           CALL ZZELVUPY ( ELLIPS, VERTEX, AXIS, N, FOV, FOUND ) */

/*           WRITE (*,*) 'Case 1: FOUND = ', FOUND */

/*     C */
/*     C     Shift the ellipse center to move the ellipse outside of */
/*     C     the FOV, then repeat the test.  We expect FOUND to be */
/*     C     .FALSE. */
/*     C */
/*           CALL VPACK ( -1.D0,   0.D0,  -3.D0 - 1.D-12,  CENTER ) */

/*           CALL CGV2EL ( CENTER, SMAJOR, SMINOR, ELLIPS ) */

/*           CALL ZZELVUPY ( ELLIPS, VERTEX, AXIS, N, FOV, FOUND ) */

/*           WRITE (*,*) 'Case 2: FOUND = ', FOUND */

/*           END */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     [1] `Calculus and Analytic Geometry', Thomas and Finney. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */
/*     B.V. Semenov    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 28-FEB-2008 (BVS) */

/*        Corrected the contents of the Required_Reading section. */

/* -    SPICELIB Version 1.0.0, 10-AUG-2005 (NJB) */

/* -& */
/* $ Index_Entries */

/*     test whether pyramid intersects ellipse */
/*     test whether ellipse is in pyramidal field of view */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Saved variables */


/*     Initial values */

    /* Parameter adjustments */
    bounds_dim2 = *n;

    /* Function Body */
    if (return_()) {
	return 0;
    }
    chkin_("ZZELVUPY", (ftnlen)8);

/*     We start out by checking the inputs. */

/*     The next step will be to look for an intersection of the ellipse */
/*     and pyramid.  There are three intersection cases: */

/*        1) The ellipse is completely contained in the pyramid. */

/*        2) The ellipse "contains" the field of view in the sense */
/*           that the intersection of the pyramid and the plane of the */
/*           ellipse is contained in the region bounded by the ellipse. */

/*        3) One or more sides of the pyramid intersect the ellipse. */

/*     There is also a non-intersection case:  this is when cones */
/*     bounding the ellipse and pyramid and having their apexes in */
/*     common with that of the pyramid intersect only in that common */
/*     apex.  Before test (1), we perform this non-intersection test, */
/*     since it can be done quickly. */

/*     No intersection has been found so far.  Set the default value */
/*     of the FOUND flag here so it won't have to be set in every error */
/*     checking block below. */

    *found = FALSE_;

/*     Validate the ellipse.  First find the center and the semi-axes */
/*     of the ellipse. */

    el2cgv_(ellips, center, gv1, gv2);
    saelgv_(gv1, gv2, smajor, sminor);

/*     Check the semi-axis lengths. */

/*     If the semi-major axis is the zero vector, we'd expect */
/*     the semi-minor axis to be the zero vector as well.  But */
/*     round-off error could conceivably violate this assumption. */

    if (vzero_(smajor) || vzero_(sminor)) {
	setmsg_("Input ellipse has semi-major axis length # and semi-minor a"
		"xis length #.  Both vectors are required to be non-zero.", (
		ftnlen)115);
	d__1 = vnorm_(smajor);
	errdp_("#", &d__1, (ftnlen)1);
	d__1 = vnorm_(sminor);
	errdp_("#", &d__1, (ftnlen)1);
	sigerr_("SPICE(ZEROVECTOR)", (ftnlen)17);
	chkout_("ZZELVUPY", (ftnlen)8);
	return 0;
    }

/*     Scale the vectors defining the ellipse and the vertex of the */
/*     pyramid so that the largest of these vectors has unit length. */

/* Computing MAX */
    d__1 = vnorm_(center), d__2 = vnorm_(smajor), d__1 = max(d__1,d__2), d__2 
	    = vnorm_(vertex);
    scale = 1. / max(d__1,d__2);
    for (i__ = 1; i__ <= 3; ++i__) {
	center[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("center", 
		i__1, "zzelvupy_", (ftnlen)452)] = scale * center[(i__2 = i__ 
		- 1) < 3 && 0 <= i__2 ? i__2 : s_rnge("center", i__2, "zzelv"
		"upy_", (ftnlen)452)];
	smajor[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("smajor", 
		i__1, "zzelvupy_", (ftnlen)453)] = scale * smajor[(i__2 = i__ 
		- 1) < 3 && 0 <= i__2 ? i__2 : s_rnge("smajor", i__2, "zzelv"
		"upy_", (ftnlen)453)];
	sminor[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("sminor", 
		i__1, "zzelvupy_", (ftnlen)454)] = scale * sminor[(i__2 = i__ 
		- 1) < 3 && 0 <= i__2 ? i__2 : s_rnge("sminor", i__2, "zzelv"
		"upy_", (ftnlen)454)];
	apex[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("apex", i__1, 
		"zzelvupy_", (ftnlen)455)] = scale * vertex[(i__2 = i__ - 1) <
		 3 && 0 <= i__2 ? i__2 : s_rnge("vertex", i__2, "zzelvupy_", (
		ftnlen)455)];
    }

/*     Create a scaled ellipse.  We'll perform the FOV side-ellipse */
/*     intersection computations using this ellipse. */

    cgv2el_(center, smajor, sminor, ellscl);

/*     After scaling, make sure the semi-axes have sufficient length to */
/*     prevent numerical problems.  Let A and B be the scaled semi-axis */
/*     lengths of the ellipse. */

    a = vnorm_(smajor);
    b = vnorm_(sminor);
    if (b == 0.) {
	setmsg_("Scaled ellipse's semi-minor axis length = 0.", (ftnlen)44);
	sigerr_("SPICE(DEGENERATECASE)", (ftnlen)21);
	chkout_("ZZELVUPY", (ftnlen)8);
	return 0;
    }

/*     Validate the input pyramid. */

/*     The axis must not be the zero vector. */

    if (vzero_(axis)) {
	setmsg_("The pyramid's axis the zero vector.", (ftnlen)35);
	sigerr_("SPICE(ZEROVECTOR)", (ftnlen)17);
	chkout_("ZZELVUPY", (ftnlen)8);
	return 0;
    }

/*     There must be at least three boundary vectors. */

    if (*n < 3) {
	setmsg_("The number of boundary vectors was #; this number must be a"
		"t least 3.", (ftnlen)69);
	errint_("#", n, (ftnlen)1);
	sigerr_("SPICE(INVALIDCOUNT)", (ftnlen)19);
	chkout_("ZZELVUPY", (ftnlen)8);
	return 0;
    }

/*     There must be no more than MAXFOV boundary vectors. */

    if (*n > 10000) {
	setmsg_("The number of boundary vectors was #; this number must not "
		"exceed #.", (ftnlen)68);
	errint_("#", n, (ftnlen)1);
	errint_("#", &c__10000, (ftnlen)1);
	sigerr_("SPICE(INVALIDCOUNT)", (ftnlen)19);
	chkout_("ZZELVUPY", (ftnlen)8);
	return 0;
    }

/*     We must initialize certain variables before continuing with */
/*     the checks. */

/*     Let CTRVEC be the vector from the apex to the center of the */
/*     ellipse.  This vector will be used in several places later; */
/*     it's convenient to compute it here. */

    vsub_(center, apex, ctrvec);

/*     Compute PASIZE:  an upper bound on the angular radius of a */
/*     circular cone whose axis is the input central axis.  While */
/*     we're at it, check the angular separation of the boundary */
/*     vectors from the central axis and from each other. */

    pasize = 0.;
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {

/*        Each boundary vector must have angular separation from the */
/*        axis of less than pi/2 radians.  Keep track of the maximum */
/*        angular separation PASIZE as we go.  We'll use this variable */
/*        later in a non-intersection test. */

	asep = vsep_(axis, &bounds[(i__2 = i__ * 3 - 3) < bounds_dim2 * 3 && 
		0 <= i__2 ? i__2 : s_rnge("bounds", i__2, "zzelvupy_", (
		ftnlen)550)]);
	if (asep >= pi_() / 2) {
	    setmsg_("The angular separation of boundary vector # from the ax"
		    "is is #. This number must less than pi/2.", (ftnlen)96);
	    errint_("#", &i__, (ftnlen)1);
	    errdp_("#", &asep, (ftnlen)1);
	    sigerr_("SPICE(INVALIDFOV)", (ftnlen)17);
	    chkout_("ZZELVUPY", (ftnlen)8);
	    return 0;
	}
	pasize = max(pasize,asep);

/*        Each boundary vector must have non-zero angular separation */
/*        from its neighbors. */

	if (i__ < *n) {
	    j = i__ + 1;
	} else {
	    j = 1;
	}
	ucrss_(&bounds[(i__2 = i__ * 3 - 3) < bounds_dim2 * 3 && 0 <= i__2 ? 
		i__2 : s_rnge("bounds", i__2, "zzelvupy_", (ftnlen)577)], &
		bounds[(i__3 = j * 3 - 3) < bounds_dim2 * 3 && 0 <= i__3 ? 
		i__3 : s_rnge("bounds", i__3, "zzelvupy_", (ftnlen)577)], cp);
	if (vzero_(cp)) {

/*           The cross product may be zero because one of the */
/*           boundary vectors is zero.  Check this first. */

	    if (vzero_(&bounds[(i__2 = j * 3 - 3) < bounds_dim2 * 3 && 0 <= 
		    i__2 ? i__2 : s_rnge("bounds", i__2, "zzelvupy_", (ftnlen)
		    584)]) || vzero_(&bounds[(i__3 = i__ * 3 - 3) < 
		    bounds_dim2 * 3 && 0 <= i__3 ? i__3 : s_rnge("bounds", 
		    i__3, "zzelvupy_", (ftnlen)584)])) {
		s_copy(errmsg, "The # boundary vector is the zero vector.", (
			ftnlen)1840, (ftnlen)41);
		if (vzero_(&bounds[(i__2 = i__ * 3 - 3) < bounds_dim2 * 3 && 
			0 <= i__2 ? i__2 : s_rnge("bounds", i__2, "zzelvupy_",
			 (ftnlen)588)])) {
		    j = i__;
		}
		repmot_(errmsg, "#", &j, "L", errmsg, (ftnlen)1840, (ftnlen)1,
			 (ftnlen)1, (ftnlen)1840);
		setmsg_(errmsg, (ftnlen)1840);
		sigerr_("SPICE(ZEROVECTOR)", (ftnlen)17);
	    } else {
		setmsg_("The angular separation of boundary vector # from ve"
			"ctor # is 0.This number must be positive.", (ftnlen)
			92);
		errint_("#", &i__, (ftnlen)1);
		errint_("#", &j, (ftnlen)1);
		sigerr_("SPICE(INVALIDFOV)", (ftnlen)17);
	    }
	    chkout_("ZZELVUPY", (ftnlen)8);
	    return 0;
	}
    }

/*     That's it for the error checks.  We'll now answer the question */
/*     this routine is meant to answer:  does the ellipse or the region */
/*     it bounds intersect the pyramid? */

/*     We'll start out with a simple check to rule out intersection */
/*     when the ellipse and pyramid are contained in disjoint right */
/*     circular cones with a common apex. */

/*     Find the angular radius (that is, one-half of the angular extent) */
/*     of a bounding cone of the ellipse as seen from the apex.  The */
/*     cone circumscribes a sphere of radius A centered at the ellipse's */
/*     center, where A is the length of the semi-major axis.  Note that */
/*     the cone does not in general circumscribe the ellipse itself. */

/*     The test can be performed only if the apex of the FOV is outside */
/*     of the sphere of radius A centered at the ellipse center. */

    d__ = vdist_(center, apex);
    if (a < d__) {
	easize = asin(a / d__);

/*        The variable PASIZE already contains the angular radius of a */
/*        bounding cone of the pyramid as seen from the pyramid's apex. */
/*        The angular radius is the maximum of the angular separations */
/*        of each pyramid edge from the pyramid's axis. Check whether */
/*        the bounding cones of ellipse and pyramid are disjoint. Recall */
/*        CTRVEC is the vector from the apex to the center of the */
/*        ellipse.  If the angular separation of CTRVEC and AXIS exceeds */
/*        the sum of the angular radii of the ellipse's and pyramid's */
/*        bounding cones, there can be no intersection. */

	consep = vsep_(ctrvec, axis) - (easize + pasize);
	if (consep > 0.) {
	    chkout_("ZZELVUPY", (ftnlen)8);
	    return 0;
	}
    }

/*     At this point, we have to take a more detailed look at the */
/*     possible intersection of ellipse and pyramid.  First check */
/*     whether the center of the ellipse is contained in the pyramid. */
/*     If the ellipse is completely contained in the pyramid, this */
/*     check will yield a positive result. */

/*     The center of the ellipse is inside the pyramid if a plane */
/*     containing this point and normal to the axis vector */
/*     chops the pyramid in a polygon that has non-zero winding */
/*     number about the center. */

/*     The center of the ellipse must lie in the correct half-space */
/*     for this test to be applicable. */

    if (vdot_(axis, ctrvec) > 0.) {

/*        Construct the plane and find the polygon. */

	nvp2pl_(axis, ctrvec, fovpln);

/*        Create the planar FOV boundary using the intersections */
/*        of the FOV boundary vectors with FOVPLN. */

	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    inrypl_(origin, &bounds[(i__2 = i__ * 3 - 3) < bounds_dim2 * 3 && 
		    0 <= i__2 ? i__2 : s_rnge("bounds", i__2, "zzelvupy_", (
		    ftnlen)686)], fovpln, &nxpts, &work[(i__3 = i__ * 3 - 3) <
		     30000 && 0 <= i__3 ? i__3 : s_rnge("work", i__3, "zzelv"
		    "upy_", (ftnlen)686)]);

/*           We expect to have a single point of intersection for each */
/*           boundary vector. */

	    if (nxpts != 1) {
		setmsg_("NXPTS = # for boundary vector #/FOV plane intersect"
			"ion.", (ftnlen)55);
		errint_("#", &nxpts, (ftnlen)1);
		errint_("#", &i__, (ftnlen)1);
		sigerr_("SPICE(BUG)", (ftnlen)10);
		chkout_("ZZELVUPY", (ftnlen)8);
		return 0;
	    }
	}

/*        Now WORK contains the polygon representing the intersection of */
/*        the pyramid with the plane FOVPLN. If the winding number of */
/*        the polygon about the ellipse center is non-zero, we conclude */
/*        the center is in the pyramid. */

	if (zzwind_(fovpln, n, work, ctrvec) != 0) {

/*           The center of the ellipse is inside the pyramid.  We're */
/*           done. */

	    *found = TRUE_;
	    chkout_("ZZELVUPY", (ftnlen)8);
	    return 0;
	}
    }

/*     Check whether the ray defined by APEX and the first boundary */
/*     vector of the pyramid (the "boundary ray") intersects the plane */
/*     region bounded by the ellipse.  If the intersection of the */
/*     pyramid and the plane of the ellipse is completely contained in */
/*     the region bounded by the ellipse, this check will yield a */
/*     positive result. */

/*     First find the intersection of the boundary ray and the plane */
/*     containing the ellipse; represent this plane using the SPICELIB */
/*     plane EPLANE. */
/*     We don't check FAILED() here because the spanning vectors */
/*     are orthogonal, and because PSV2PL (via a call to UCRSS) */
/*     does scaling to prevent underflow. */

    psv2pl_(center, smajor, sminor, eplane);
    inrypl_(apex, &bounds[(i__1 = 0) < bounds_dim2 * 3 ? i__1 : s_rnge("boun"
	    "ds", i__1, "zzelvupy_", (ftnlen)745)], eplane, &nxpts, xpt);

/*     The routine INRYPL can return the NXPTS values 1, 0, or INF---a */
/*     code indicating an infinite number of intersection points of ray */
/*     and plane.  If the value is 1, the boundary ray may intersect */
/*     the region bounded by the ellipse. */

    if (nxpts == 1) {

/*        The boundary ray intersects the plane of the ellipse in a */
/*        single point. Decide whether this point is inside the ellipse. */
/*        To test for containment, find the "coordinates" of the */
/*        center-to-point vector relative to the two-dimensional basis */
/*        formed by the semi-axes of the ellipse.  Call this */
/*        center-to-point vector OFFSET.  Recall A and B are the */
/*        semi-axis lengths of the ellipse. Let X and Y be the */
/*        coordinates of OFFSET in the two-dimensional reference frame */
/*        whose basis consists of normalized versions of SMAJOR and */
/*        SMINOR. */

/*        Note that we could have the special case in which the vertex */
/*        of the pyramid lies in the plane of the ellipse, in which case */
/*        the FOV "sees" the ellipse edge-on.  However, since NXPTS is */
/*        not INF, the boundary vector does not lie in the plane of the */
/*        ellipse.  So in this special case, APEX would be in the region */
/*        bounded by the ellipse. */

	vsub_(xpt, center, offset);
	x = vdot_(offset, smajor) / a;
	y = vdot_(offset, sminor) / b;
	d__1 = x / a;
	d__2 = y / b;
	if (pow_dd(&d__1, &c_b79) + pow_dd(&d__2, &c_b79) <= 1.) {

/*           The boundary-vector-plane intercept lies in the */
/*           topologically closed region bounded by the ellipse. */

	    *found = TRUE_;
	    chkout_("ZZELVUPY", (ftnlen)8);
	    return 0;
	}
    }

/*     Check whether one of the pyramid's sides intersects the ellipse. */
/*     For each side, we first test whether the plane containing that */
/*     side intersects the ellipse.  If it does, the intersection is */
/*     a (possibly degenerate) line segment with endpoints on the */
/*     ellipse.  The triangle (or segment) defined by the pyramid's */
/*     apex and this segment (point) is then checked for intersection */
/*     with the currently considered side of the pyramid. */

    i__ = 1;
    while(i__ <= *n && ! (*found)) {

/*        Create a SPICELIB plane containing the Ith side of the */
/*        pyramid. */

	if (i__ < *n) {
	    j = i__ + 1;
	} else {
	    j = 1;
	}

/*        Although PSV2PL can signal an error if the spanning */
/*        vectors are linearly dependent, it won't do so here */
/*        because we've already ensured the cross product of */
/*        these vectors is non-zero. */

	psv2pl_(apex, &bounds[(i__1 = i__ * 3 - 3) < bounds_dim2 * 3 && 0 <= 
		i__1 ? i__1 : s_rnge("bounds", i__1, "zzelvupy_", (ftnlen)820)
		], &bounds[(i__2 = j * 3 - 3) < bounds_dim2 * 3 && 0 <= i__2 ?
		 i__2 : s_rnge("bounds", i__2, "zzelvupy_", (ftnlen)820)], 
		plane);

/*        Find the intersection of the plane and the ellipse. */

	inelpl_(ellscl, plane, &nxpts, xpt1, xpt2);

/*        If the ellipse-plane intersection is non-empty, test it to see */
/*        whether it has non-empty intersection with the current side of */
/*        the pyramid. */

	if (nxpts > 0) {

/*           Let EDGE1 and EDGE2 be the unit length boundary vectors */
/*           forming the edges of the currently considered side of the */
/*           pyramid. */

	    vhat_(&bounds[(i__1 = i__ * 3 - 3) < bounds_dim2 * 3 && 0 <= i__1 
		    ? i__1 : s_rnge("bounds", i__1, "zzelvupy_", (ftnlen)837)]
		    , edge1);
	    vhat_(&bounds[(i__1 = j * 3 - 3) < bounds_dim2 * 3 && 0 <= i__1 ? 
		    i__1 : s_rnge("bounds", i__1, "zzelvupy_", (ftnlen)838)], 
		    edge2);

/*           Let EBSCTR ("pyramid edge bisector") be a bisector of the */
/*           sector bounded by EDGE1 and EDGE2. */

	    vlcom_(&c_b90, edge1, &c_b90, edge2, ebsctr);

/*           Let HAFEDG be half of the angular measure of this sector. */

	    hafedg = vsep_(edge1, edge2) / 2.;

/*           Let VXPT1 and VXPT2 be the unit vectors pointing from the */
/*           pyramid's apex to the points of intersection of the ellipse */
/*           and the plane containing the currently considered side of */
/*           the pyramid. */

	    vsub_(xpt1, apex, vtemp);
	    vhat_(vtemp, vxpt1);
	    vsub_(xpt2, apex, vtemp);
	    vhat_(vtemp, vxpt2);

/*           At this point we'll introduce a bit of terminology. We're */
/*           going to work with plane regions defined by pairs of */
/*           vectors with a common endpoint.  We'll abuse standard */
/*           terminology a bit and call the region bounded by such a */
/*           vector pair a "sector."  Strictly speaking, sectors refer */
/*           only to subsets of a disc. */

/*           When it's convenient, we'll also identify "sectors" with */
/*           regions of the unit circle.  This will make it possible */
/*           to talk about intersections of sectors in terms of */
/*           intersections of the associated arcs on the unit circle. */
/*           By the "endpoints" of a sector we mean the endpoints */
/*           of the arc associated with the sector on the unit circle. */

/*           Let VBSCTR ("VXPT bisector") be a bisector of the sector */
/*           bounded by VXPT1 and VXPT2. */

	    vlcom_(&c_b90, vxpt1, &c_b90, vxpt2, vbsctr);

/*           Let HAFSEC be half of the angular measure of the sector */
/*           bounded by VXPT1 and VXPT2. */

	    hafsec = vsep_(vxpt1, vxpt2) / 2.;

/*           EDGE1, EDGE2, VXPT1, and VXPT2 are four co-planar vectors */
/*           emanating from APEX.  We want to find out whether the */
/*           sector bounded by EDGE1 and EDGE2 intersects the sector */
/*           bounded by VXPT1 and VXPT2.  If there's an intersection, at */
/*           least one endpoint of one sector is contained in the other */
/*           sector. */

/*           Because of potential round-off problems when the sectors */
/*           are nearly coincident, we perform the precautionary check */
/*           (case 3) on the angle bisector of the sector defined by */
/*           VXPT1 and VXPT2. */

/*           If the sector defined by VXPT1 and VXPT2 has no endpoint */
/*           contained in the other sector, it's possible that the */
/*           former sector contains the latter.  In that case the */
/*           angular bisector of the latter sector is contained in the */
/*           former (case 4). */

/*           We test a vector's containment in a sector by comparing the */
/*           vector's angular separation from the sector's angle */
/*           bisector to one-half of the angular measure of the sector. */

/*              Case 1:  VXPT1 lies between EDGE1 and EDGE2. */
/*              Case 2:  VXPT2 lies between EDGE1 and EDGE2. */
/*              Case 3:  VBSCTR lies between EDGE1 and EDGE2. */
/*              Case 4:  EBSCTR lies between VXPT1 and VXPT2. */

	    if (vsep_(vxpt1, ebsctr) <= hafedg) {
		*found = TRUE_;
	    } else if (vsep_(vxpt2, ebsctr) <= hafedg) {
		*found = TRUE_;
	    } else if (vsep_(vbsctr, ebsctr) <= hafedg) {
		*found = TRUE_;
	    } else if (vsep_(ebsctr, vbsctr) <= hafsec) {
		*found = TRUE_;
	    }
	    if (*found) {

/*              We've found an intersection.  We're done. */

		chkout_("ZZELVUPY", (ftnlen)8);
		return 0;
	    }
	}

/*        If no intersection was found, look at the next side of the */
/*        pyramid. */

	++i__;
    }

/*     If we got this far, the ellipse is not in view.  FOUND has */
/*     already been set to .FALSE. */

    chkout_("ZZELVUPY", (ftnlen)8);
    return 0;
} /* zzelvupy_ */

