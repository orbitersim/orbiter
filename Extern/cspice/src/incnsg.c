/* incnsg.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure INCNSG ( Intersection of cone and line segment ) */
/* Subroutine */ int incnsg_(doublereal *apex, doublereal *axis, doublereal *
	angle, doublereal *endpt1, doublereal *endpt2, integer *nxpts, 
	doublereal *xpt1, doublereal *xpt2)
{
    /* Initialized data */

    static doublereal origin[3] = { 0.,0.,0. };
    static doublereal y[3] = { 0.,1.,0. };
    static doublereal z__[3] = { 0.,0.,1. };

    /* System generated locals */
    integer i__1, i__2;
    doublereal d__1;

    /* Builtin functions */
    double cos(doublereal);
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    doublereal dmag;
    extern /* Subroutine */ int vadd_(doublereal *, doublereal *, doublereal *
	    );
    doublereal minp[3], maxp[3], udir[3];
    extern /* Subroutine */ int vhat_(doublereal *, doublereal *);
    extern doublereal vdot_(doublereal *, doublereal *);
    extern /* Subroutine */ int vsub_(doublereal *, doublereal *, doublereal *
	    ), vequ_(doublereal *, doublereal *);
    doublereal plnx[3], uuax, wuax;
    extern /* Subroutine */ int mtxv_(doublereal *, doublereal *, doublereal *
	    );
    doublereal v1mag, v2mag;
    extern /* Subroutine */ int zzcnquad_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, integer *, doublereal *, doublereal *)
	    ;
    doublereal uoff1[3], uoff2[3], xoff1[3], xoff2[3], a, b, c__;
    extern /* Subroutine */ int zzsglatx_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *);
    integer i__;
    extern /* Subroutine */ int zzcxbrut_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, logical *)
	    ;
    integer n;
    doublereal x[3];
    extern /* Subroutine */ int frame_(doublereal *, doublereal *, doublereal 
	    *), chkin_(char *, ftnlen);
    doublereal axmag, colat;
    extern /* Subroutine */ int errdp_(char *, doublereal *, ftnlen);
    extern doublereal vdist_(doublereal *, doublereal *);
    doublereal uaxis[3], vtemp[3], xform[9]	/* was [3][3] */;
    integer nplnx;
    extern /* Subroutine */ int unorm_(doublereal *, doublereal *, doublereal 
	    *);
    doublereal s1, s2, v1[3], v2[3], w2, vtemp2[3];
    extern /* Subroutine */ int nvp2pl_(doublereal *, doublereal *, 
	    doublereal *);
    extern logical failed_(void);
    extern doublereal pi_(void), halfpi_(void);
    doublereal locang, cosang, wu;
    logical isbrck;
    doublereal minlat, ca2, maxlat;
    extern /* Subroutine */ int sigerr_(char *, ftnlen);
    doublereal coserr;
    extern /* Subroutine */ int chkout_(char *, ftnlen), setmsg_(char *, 
	    ftnlen);
    doublereal dp1, dp2, nrmpln[4];
    logical in1, in2;
    extern /* Subroutine */ int inrypl_(doublereal *, doublereal *, 
	    doublereal *, integer *, doublereal *);
    extern logical return_(void);
    doublereal uv1[3], uv2[3], dir[3];
    extern /* Subroutine */ int mxv_(doublereal *, doublereal *, doublereal *)
	    ;
    logical neg1;
    doublereal off1[3], off2[3];
    logical neg2;

/* $ Abstract */

/*     Compute the points of intersection of a specified nappe of a cone */
/*     and a line segment. */

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

/*     CONE */
/*     GEOMETRY */
/*     INTERSECTION */
/*     LINE */
/*     MATH */
/*     SEGMENT */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     APEX       I   Apex of cone. */
/*     AXIS       I   Axis of cone. */
/*     ANGLE      I   Angle of cone. */
/*     ENDPT1, */
/*     ENDPT2     I   Endpoints of line segment. */
/*     NXPTS      O   Number of intersection points. */
/*     XPT1       O   First intersection point, if it exists. */
/*     XPT2       O   Second intersection point, if it exists. */

/* $ Detailed_Input */

/*     APEX     is the apex (tip) of the cone. In this routine's */
/*              documentation, we'll consider the cone to be a */
/*              semi-infinite pyramid with circular cross-section. In */
/*              some contexts, this object is called one "nappe" of */
/*              the complete cone. */

/*     AXIS     is an axis vector of the cone. */

/*     ANGLE    is the angular separation from AXIS of the rays */
/*              comprising the cone. Let the notation */

/*                 < A, B > */

/*              denote the dot product of vectors A and B, and let */

/*                 ||A|| */

/*              denote the norm of vector A. Then the cone is the set */
/*              of points */

/*                           X-APEX       AXIS */
/*                 { X:  < ----------,  -------- >  =  cos(ANGLE) } */
/*                         ||X-APEX||   ||AXIS|| */


/*     ENDPT1, */
/*     ENDPT2   are endpoints of a line segment. These points */
/*              must be distinct. */

/* $ Detailed_Output */

/*     NXPTS    is the number of points of intersection of the input */
/*              line segment and cone. */

/*     XPT1     is the point of intersection of the segment and cone */
/*              that is closest to ENDPT1, if an intersection exists. */
/*              If there are no intersections, XPT1 is undefined. */

/*     XPT2     is the point of intersection of the segment and cone */
/*              that is farthest from ENDPT1, if two points of */
/*              intersection exist. If there are not two */
/*              intersections, XPT2 is undefined. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If AXIS is the zero vector, the error SPICE(ZEROVECTOR) */
/*         is signaled. */

/*     2)  If ANGLE is less than zero, the error SPICE(INVALIDANGLE) */
/*         is signaled. */

/*     3)  If ENDPT1 and ENDPT2 coincide, the error */
/*         SPICE(ENDPOINTSMATCH) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine is used by the SPICELIB DSK subsystem. In */
/*     particular, it is used to determine whether a ray contacts a */
/*     latitude boundary of a volume element in either planetocentric */
/*     latitudinal or planetodetic coordinates. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as input */
/*     (if any), the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Compute the intersection of a line segment and cone in */
/*        a simple case for which the results can easily be checked. */

/*        Let the apex of the cone be at the origin. Let the axis */
/*        of the cone lie on the +X axis. Let the angle of the cone */
/*        be 45 degrees. Let the line segment have endpoints */

/*           ENDPT1 = ( 1,   -2, sqrt(3)/2 ) */
/*           ENDPT2 = ( 1,    2, sqrt(3)/2 ) */

/*        We expect there to be two points of intersection: */

/*           XPT1   = ( 1, -1/2, sqrt(3)/2 ) */
/*           XPT2   = ( 1,  1/2, sqrt(3)/2 ) */


/*        Example code begins here. */


/*              PROGRAM INCNSG_EX1 */
/*              IMPLICIT NONE */
/*        C */
/*        C     SPICELIB functions */
/*        C */
/*              DOUBLE PRECISION      RPD */
/*        C */
/*        C     Local parameters */
/*        C */
/*              CHARACTER*(*)         FMT1 */
/*              PARAMETER           ( FMT1 = '(A,3F13.8)' ) */

/*              CHARACTER*(*)         FMT2 */
/*              PARAMETER           ( FMT2 = '(A,I2)' ) */
/*        C */
/*        C     Local variables */
/*        C */
/*              DOUBLE PRECISION      ANGLE */
/*              DOUBLE PRECISION      APEX   ( 3 ) */
/*              DOUBLE PRECISION      AXIS   ( 3 ) */
/*              DOUBLE PRECISION      ENDPT1 ( 3 ) */
/*              DOUBLE PRECISION      ENDPT2 ( 3 ) */
/*              DOUBLE PRECISION      SQ3 */
/*              DOUBLE PRECISION      XPT1   ( 3 ) */
/*              DOUBLE PRECISION      XPT2   ( 3 ) */

/*              INTEGER               NXPTS */

/*        C */
/*        C     Set up the cone's geometric attributes. */
/*        C */
/*              CALL VPACK ( 0.D0, 0.D0, 0.D0, APEX ) */
/*              CALL VPACK ( 1.D0, 0.D0, 0.D0, AXIS ) */

/*              ANGLE = 45.D0 * RPD() */
/*        C */
/*        C     Initialize the line segment's endpoints. */
/*        C */
/*              SQ3 = SQRT( 3.D0  ) */

/*              CALL VPACK ( 1.D0, -2.D0, SQ3/2, ENDPT1 ) */
/*              CALL VPACK ( 1.D0,  2.D0, SQ3/2, ENDPT2 ) */
/*        C */
/*        C     Find the points of intersection. */
/*        C */
/*              CALL INCNSG ( APEX,   AXIS,  ANGLE, ENDPT1, */
/*             .              ENDPT2, NXPTS, XPT1,  XPT2   ) */

/*              WRITE (*,*) ' ' */
/*              WRITE (*,FMT1) 'Apex:        ', APEX */
/*              WRITE (*,FMT1) 'Axis:        ', AXIS */
/*              WRITE (*,FMT1) 'Angle (deg): ', ANGLE/RPD() */
/*              WRITE (*,FMT1) 'Endpoint 1:  ', ENDPT1 */
/*              WRITE (*,FMT1) 'Endpoint 2:  ', ENDPT2 */
/*              WRITE (*,*) ' ' */
/*              WRITE (*,FMT2) 'Number of intersection points: ', */
/*             .            NXPTS */
/*              WRITE (*,*) ' ' */
/*              WRITE (*,FMT1) 'Point 1:    ', XPT1 */
/*              WRITE (*,FMT1) 'Point 2:    ', XPT2 */
/*              WRITE (*,*) ' ' */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        Apex:           0.00000000   0.00000000   0.00000000 */
/*        Axis:           1.00000000   0.00000000   0.00000000 */
/*        Angle (deg):   45.00000000 */
/*        Endpoint 1:     1.00000000  -2.00000000   0.86602540 */
/*        Endpoint 2:     1.00000000   2.00000000   0.86602540 */

/*        Number of intersection points:  2 */

/*        Point 1:       1.00000000  -0.50000000   0.86602540 */
/*        Point 2:       1.00000000   0.50000000   0.86602540 */


/* $ Restrictions */

/*     1)  This routine is designed to avoid arithmetic overflow in */
/*         normal cases, such as those in which the line segment is */
/*         nearly parallel to the cone. However, it is possible to cause */
/*         arithmetic overflow by using input vectors with extremely */
/*         large magnitudes. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 06-JUL-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.0, 26-OCT-2016 (NJB) */

/* -& */
/* $ Index_Entries */

/*     intersection of line segment and cone */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Saved values */


/*     Initial values */


/*     Use quasi-discovery check-in. We'll check in before */
/*     code sections that can generate SPICE errors, and check */
/*     out afterward. When those code sections are skipped, */
/*     we avoid traceback participation. */

    if (return_()) {
	return 0;
    }

/*     No intersection was found so far. */

    *nxpts = 0;

/*     The cone's axis vector must be non-zero. */

    unorm_(axis, uaxis, &axmag);
    if (axmag == 0.) {
	chkin_("INCNSG", (ftnlen)6);
	setmsg_("The cone's axis vector must be non-zero but sadly, it faile"
		"d to meet this criterion.", (ftnlen)84);
	sigerr_("SPICE(ZEROVECTOR)", (ftnlen)17);
	chkout_("INCNSG", (ftnlen)6);
	return 0;
    }

/*     The cone's angular radius must be non-negative. */

    if (*angle < 0.) {
	chkin_("INCNSG", (ftnlen)6);
	setmsg_("The cone's angular radius must be  non-negative but was # ("
		"radians).", (ftnlen)68);
	errdp_("#", angle, (ftnlen)1);
	sigerr_("SPICE(INVALIDANGLE)", (ftnlen)19);
	chkout_("INCNSG", (ftnlen)6);
	return 0;
    }

/*     The endpoints of the segment must be distinct. Check this after */
/*     computing a unit direction vector for the line segment. */

    vsub_(endpt2, endpt1, dir);
    unorm_(dir, udir, &dmag);
    if (dmag == 0.) {
	chkin_("INCNSG", (ftnlen)6);
	setmsg_("The distance between the segment's endpoints was zero. Firs"
		"t endpoint: (# # #).", (ftnlen)79);
	errdp_("#", endpt1, (ftnlen)1);
	errdp_("#", &endpt1[1], (ftnlen)1);
	errdp_("#", &endpt1[2], (ftnlen)1);
	sigerr_("SPICE(ENDPOINTSMATCH)", (ftnlen)21);
	chkout_("INCNSG", (ftnlen)6);
	return 0;
    }

/*     Store the cosine of the cone's angular radius. We'll treat all */
/*     cases with COSANG equal to 0 as though the cone is actually a */
/*     plane normal to the axis and containing the apex. */

    cosang = cos(*angle);
    locang = *angle;

/*     We'll work with a local axis that has angular separation of */
/*     no more than pi/2 from the nappe. */

    if (cosang < 0.) {
	cosang = -cosang;
	locang = pi_() - *angle;
	uaxis[0] = -uaxis[0];
	uaxis[1] = -uaxis[1];
	uaxis[2] = -uaxis[2];
    }

/*     Compute the offsets of the endpoints of the segment from */
/*     the cone's apex. */

    vsub_(endpt1, apex, off1);
    vsub_(endpt2, apex, off2);

/*     Deal with some of the simple cases first. */

    vhat_(off1, uoff1);
    vhat_(off2, uoff2);
    dp1 = vdot_(uoff1, uaxis);
    dp2 = vdot_(uoff2, uaxis);

/*     The given axis is inside the nappe defined by the angular radius. */

/*     There's no intersection if both endpoints are in the interior of */
/*     the nappe of the cone (since the nappe is convex). */

    in1 = dp1 >= cosang;
    in2 = dp2 >= cosang;

/*     If the line segment lies on the far side of the plane that */
/*     contains the apex and is orthogonal to the axis, there's no */
/*     intersection. */

    neg1 = dp1 < 0.;
    neg2 = dp2 < 0.;
    if (in1 && in2 || neg1 && neg2) {

/*        The segment is in the interior of the cone or */
/*        on the far side of the plane. */

	*nxpts = 0;
	return 0;
    }

/*     Here's where we handle the half-space case. */

    if (abs(cosang) < 1e-14) {

/*        See whether the ray emanating from the first endpoint and */
/*        having direction UDIR hits the plane normal to the axis and */
/*        containing the apex. We'll call this plane NRMPLN. */

/*        NVP2PL can signal an error only if the input axis is the */
/*        zero vector. We've ensured that it isn't. */

	nvp2pl_(uaxis, apex, nrmpln);
	inrypl_(endpt1, udir, nrmpln, &nplnx, plnx);

/*        If the ray doesn't hit the plane, we're done. Otherwise, */
/*        check the intercept. */

	if (nplnx == 1) {

/*           The ray does hit the plane. If the intersection is on the */
/*           line segment, we have a solution. */

	    if (vdist_(plnx, endpt1) <= dmag) {

/*              The intercept is not further along the ray than the */
/*              second endpoint. It's a valid solution. */

		*nxpts = 1;
		vequ_(plnx, xpt1);
	    }
	}

/*        This is the end of the half-space case. */

	return 0;
    }

/*     At this point we've disposed of the trivial cases. We'll */
/*     set up a quadratic equation for the intersection of the */
/*     line segment with the surface of the cone's nappe. */

/*     Due to round-off errors, the solution of the quadratic may */
/*     either be inaccurate or may not be found at all. We'll */
/*     examine the solutions we find and solve the problem by */
/*     an alternate method if necessary. However, the quadratic */
/*     method is fast, so we give it priority. */

/*     The equation of a ray starting at ENDPT1 and having unit */
/*     direction vector UDIR is */

/*        RAY  = { ENDPT1 + s*UDIR, s >= 0 }                          (1) */

/*     The equation of the nappe of the cone is */

/*        CONE = { X: < X - APEX, UAXIS > = ||X-APEX|| * cos(ANGLE) } (2) */

/*     where ANGLE is the angular radius of the cone and UAXIS is the */
/*     unit axis vector. Substituting the right hand side expression of */
/*     (1) for X in equation (2) and squaring both sides yields a */
/*     quadratic equation for S. We'll derive the coefficients of the */
/*     equation below. */

/*     Let */

/*        Q  = X - APEX */
/*        W  = ENDPT1 - APEX */
/*        U  = UDIR */
/*        CA = cos(ANGLE) */

/*     We can translate the cone and ray by -APEX, and (1) and (2) */
/*     can be re-written as */

/*        RAY  = { W + s*U, s >= 0 }                                  (3) */

/*        CONE = { Q: < Q, UAXIS > = ||Q|| * cos(ANGLE) }             (4) */


/*        Substituting the ray expression for Q, we obtain */

/*           < W + s*U, UAXIS > = ||W+s*U|| * CA                      (5) */

/*        and squaring both sides yields */

/*                      2                                      2   2 */
/*             <W,UAXIS>  + 2*<W,UAXIS>*<U,UAXIS>*s + <U,UAXIS> * s */

/*                    2                2       2 */
/*           = ( ||W||  + 2*<W,U>*s + s  ) * CA                       (6) */


/*       Collecting coefficients of powers of s, we have */

/*                         2     2     2 */
/*              ( <U,UAXIS>  - CA ) * s */

/*                                            2 */
/*            + 2 * ( <W,UAXIS>*<U,UAXIS> - CA * <W,U> ) * s */

/*                       2        2    2 */
/*            + <W,UAXIS>  - ||W|| * CA */


/*         =  0                                                       (7) */


/*      Before continuing, we observe that the only non-unit vector */
/*      in (7) is W. So the coefficients in (7) have no possibility */
/*      of overflowing unless the vertex of the ray is very far from */
/*      the apex of the cone. */

/*      W has been computed above as OFF1. */


/*         [ Consider adding check on OFF1 here. ] */


/*     Intermediate values: */

    uuax = vdot_(udir, uaxis);
    wuax = vdot_(off1, uaxis);
    wu = vdot_(off1, udir);
    w2 = vdot_(off1, off1);
    ca2 = cosang * cosang;

/*     Quadratic coefficients: */

    a = uuax * uuax - ca2;
    b = (wuax * uuax - ca2 * wu) * 2;
    c__ = wuax * wuax - w2 * ca2;

/*     We're not interested in solutions that lie outside */
/*     of the line segment. The length of the segment is */
/*     DMAG. */

/*     Solve the equation, using DMAG as an upper bound */
/*     on the magnitude of the roots. */

    zzcnquad_(&a, &b, &c__, &dmag, &n, &s1, &s2);

/*     Compute the possible intersection points and test them */
/*     to make sure they really are solutions. */

    if (n > 0) {

/*        Start with the solution closest to the ray's vertex. */
/*        Compute XPT1 and make sure it's on the correct nappe */
/*        of the cone. */

	if (s1 >= 0.) {
	    xpt1[0] = endpt1[0] + s1 * udir[0];
	    xpt1[1] = endpt1[1] + s1 * udir[1];
	    xpt1[2] = endpt1[2] + s1 * udir[2];
	    vsub_(xpt1, apex, v1);

/*           See whether V1 is on the cone. */

	    unorm_(v1, uv1, &v1mag);
	    if (v1mag > 0.) {
		coserr = (d__1 = vdot_(uv1, uaxis) - cosang, abs(d__1));
	    } else {
		coserr = 0.;
	    }
	    if (v1mag == 0. || coserr < 1e-10) {

/*              The root is on the cone (on the apex if V1MAG is zero). */

/*              We accept this root. Update NXPTS. Note that this is */
/*              not necessarily the final value of NXPTS; that */
/*              depends on the validity of the second root. */
		*nxpts = 1;
	    }
	}
	if (n == 2) {

/*           Check the second root. */

	    if (s2 >= 0.) {
		xpt2[0] = endpt1[0] + s2 * udir[0];
		xpt2[1] = endpt1[1] + s2 * udir[1];
		xpt2[2] = endpt1[2] + s2 * udir[2];
		vsub_(xpt2, apex, v2);

/*              See whether V2 is on the cone. */

		unorm_(v2, uv2, &v2mag);
		if (v2mag > 0.) {
		    coserr = (d__1 = vdot_(uv2, uaxis) - cosang, abs(d__1));
		} else {
		    coserr = 0.;
		}
		if (v2mag == 0. || coserr < 1e-10) {

/*                 The root is on the cone (on the apex if V2MAG is */
/*                 zero). */

/*                 We accept this root. */

		    ++(*nxpts);
		    if (*nxpts == 1) {

/*                    This is the only valid root; overwrite XPT1. */

			vequ_(xpt2, xpt1);
		    }
		}
	    }
	}
    }

/*     We're not done yet. If we have fewer roots than we should, we'll */
/*     need to solve the problem by an alternate method. */

/*     If we have two roots, we're in good shape. Otherwise we must */
/*     determine how many roots should be found. */

    if (*nxpts < 2) {

/*        We must determine the expected number of roots, and if */
/*        we didn't come up with them, we must find the roots */
/*        by an alternate method. */

/*        We'll examine the containment of the endpoints within the */
/*        cone. */

/*        The case where both endpoints are inside the cone was handled */
/*        earlier. */

/*        If one endpoint is inside the cone and one is outside, */
/*        we expect to have one root. */

	if (in1 && ! in2 || in2 && ! in1) {

/*           There's supposed to be one root. If we found none, find one */
/*           now. */

	    if (*nxpts == 0) {

/*              ZZCXBRUT signals an error if the axis is the zero */
/*              vector, but not otherwise. We've already ruled out this */
/*              situation. Therefore, we don't check in before the */
/*              following call. */

		zzcxbrut_(apex, uaxis, &locang, endpt1, endpt2, xpt1, &isbrck)
			;
		if (isbrck) {

/*                 As long as the root was bracketed, XPT1 is a */
/*                 solution. */

		    *nxpts = 1;
		}
	    }
	} else {
	    chkin_("INCNSG", (ftnlen)6);

/*           Both endpoints are outside the cone. We could have zero to */
/*           two roots. If the minimum angular separation of the segment */
/*           from the axis is less than ANGLE, we expect to find two */
/*           roots; if it's equal to ANGLE, we expect to find one, and */
/*           if it's greater than ANGLE, none. */

/*           We'll transform OFF1 and OFF2 into a reference frame in */
/*           which angular separation from the axis is equivalent to */
/*           colatitude. Then we'll find the maximum latitude attained */
/*           on the segment. */

/*           We'll count the roots we find, so we'll start at zero. */

	    *nxpts = 0;
	    frame_(uaxis, x, y);
	    for (i__ = 1; i__ <= 3; ++i__) {
		xform[(i__1 = i__ * 3 - 3) < 9 && 0 <= i__1 ? i__1 : s_rnge(
			"xform", i__1, "incnsg_", (ftnlen)837)] = x[(i__2 = 
			i__ - 1) < 3 && 0 <= i__2 ? i__2 : s_rnge("x", i__2, 
			"incnsg_", (ftnlen)837)];
		xform[(i__1 = i__ * 3 - 2) < 9 && 0 <= i__1 ? i__1 : s_rnge(
			"xform", i__1, "incnsg_", (ftnlen)838)] = y[(i__2 = 
			i__ - 1) < 3 && 0 <= i__2 ? i__2 : s_rnge("y", i__2, 
			"incnsg_", (ftnlen)838)];
		xform[(i__1 = i__ * 3 - 1) < 9 && 0 <= i__1 ? i__1 : s_rnge(
			"xform", i__1, "incnsg_", (ftnlen)839)] = uaxis[(i__2 
			= i__ - 1) < 3 && 0 <= i__2 ? i__2 : s_rnge("uaxis", 
			i__2, "incnsg_", (ftnlen)839)];
	    }
	    mxv_(xform, off1, xoff1);
	    mxv_(xform, off2, xoff2);
	    zzsglatx_(xoff1, xoff2, &minlat, minp, &maxlat, maxp);
	    if (failed_()) {
		chkout_("INCNSG", (ftnlen)6);
		return 0;
	    }

/*           COLAT is the colatitude of the point of maximum latitude. */

	    colat = halfpi_() - maxlat;
	    if (colat < locang) {

/*              MAXP is inside the cone. There should be an intersection */
/*              on the segment between XOFF1 and MAXP and another */
/*              between MAXP and XOFF2. */

		zzcxbrut_(origin, z__, &locang, xoff1, maxp, vtemp, &isbrck);
		if (isbrck) {

/*                 Convert VTEMP to the original frame, then translate */
/*                 it so that it's represented as an offset from the */
/*                 origin. */

		    mtxv_(xform, vtemp, vtemp2);
		    vadd_(vtemp2, apex, xpt1);
		    *nxpts = 1;
		}
		zzcxbrut_(origin, z__, &locang, maxp, xoff2, vtemp, &isbrck);
		if (isbrck) {

/*                 Convert VTEMP to the original frame, then translate */
/*                 it so that it's represented as an offset from the */
/*                 origin. */

		    mtxv_(xform, vtemp, vtemp2);
		    vadd_(vtemp2, apex, xpt2);
		    if (*nxpts == 1) {

/*                    Both roots are valid. */

			*nxpts = 2;
		    } else {

/*                    The second root is the only valid root. Move it */
/*                    into XPT1. */

			vequ_(xpt2, xpt1);
			*nxpts = 1;
		    }
		}
	    } else if (colat == locang) {

/*              The root corresponds to a point of tangency of */
/*              the segment and cone. This occurs at the point */
/*              having maximum latitude: MAXP. */

		vequ_(maxp, xpt1);
		*nxpts = 1;

/*           Note that if COLAT > LOCANG, there are no roots. */

	    }
	    chkout_("INCNSG", (ftnlen)6);
	}

/*        This is the end of portion of the "brute force" branch in */
/*        which both endpoints are outside the cone. */

    }

/*     NXPTS  has been set. */

/*     If NXPTS is 1, then XPT1 is set. */

/*     If NXPTS is 2, then both XPT1 and XPT2 are set. */

    return 0;
} /* incnsg_ */

