/* zzedterm.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static doublereal c_b30 = 0.;
static doublereal c_b35 = 1.;

/* $Procedure ZZEDTERM ( Ellipsoid terminator ) */
/* Subroutine */ int zzedterm_(char *type__, doublereal *a, doublereal *b, 
	doublereal *c__, doublereal *srcrad, doublereal *srcpos, integer *
	npts, doublereal *trmpts, ftnlen type_len)
{
    /* System generated locals */
    integer trmpts_dim2, i__1, i__2;
    doublereal d__1, d__2, d__3;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    double asin(doublereal);
    integer s_rnge(char *, integer, char *, integer);
    double d_sign(doublereal *, doublereal *);

    /* Local variables */
    extern /* Subroutine */ int vadd_(doublereal *, doublereal *, doublereal *
	    );
    doublereal rmin, rmax;
    extern /* Subroutine */ int vscl_(doublereal *, doublereal *, doublereal *
	    );
    extern doublereal vdot_(doublereal *, doublereal *), vsep_(doublereal *, 
	    doublereal *);
    integer nitr;
    extern /* Subroutine */ int vsub_(doublereal *, doublereal *, doublereal *
	    ), vequ_(doublereal *, doublereal *);
    doublereal d__, e[3];
    integer i__;
    doublereal s, angle, v[3], x[3], delta, y[3], z__[3], inang;
    extern /* Subroutine */ int chkin_(char *, ftnlen), frame_(doublereal *, 
	    doublereal *, doublereal *);
    doublereal plane[4];
    extern /* Subroutine */ int ucase_(char *, char *, ftnlen, ftnlen), 
	    errch_(char *, char *, ftnlen, ftnlen), vpack_(doublereal *, 
	    doublereal *, doublereal *, doublereal *);
    doublereal theta;
    extern /* Subroutine */ int errdp_(char *, doublereal *, ftnlen);
    doublereal trans[9]	/* was [3][3] */, srcpt[3], vtemp[3];
    extern doublereal vnorm_(doublereal *), twopi_(void);
    extern /* Subroutine */ int ljust_(char *, char *, ftnlen, ftnlen), 
	    pl2nvc_(doublereal *, doublereal *, doublereal *);
    doublereal lambda;
    extern /* Subroutine */ int nvp2pl_(doublereal *, doublereal *, 
	    doublereal *);
    extern doublereal halfpi_(void);
    doublereal minrad;
    extern /* Subroutine */ int latrec_(doublereal *, doublereal *, 
	    doublereal *, doublereal *);
    doublereal maxrad, angerr;
    logical umbral;
    extern doublereal touchd_(doublereal *);
    doublereal offset[3], prvdif;
    extern /* Subroutine */ int sigerr_(char *, ftnlen);
    doublereal outang, plcons, prvang;
    extern /* Subroutine */ int chkout_(char *, ftnlen), setmsg_(char *, 
	    ftnlen), errint_(char *, integer *, ftnlen);
    char loctyp[50];
    extern logical return_(void);
    extern /* Subroutine */ int vminus_(doublereal *, doublereal *);
    doublereal dir[3];
    extern /* Subroutine */ int mxv_(doublereal *, doublereal *, doublereal *)
	    ;
    doublereal vtx[3];

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Compute a set of points on the umbral or penumbral terminator of */
/*     a specified ellipsoid, given a spherical light source. */

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

/* $ Keywords */

/*     BODY */
/*     GEOMETRY */
/*     MATH */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     TYPE       I   Terminator type. */
/*     A          I   Length of ellipsoid semi-axis lying on the x-axis. */
/*     B          I   Length of ellipsoid semi-axis lying on the y-axis. */
/*     C          I   Length of ellipsoid semi-axis lying on the z-axis. */
/*     SRCRAD     I   Radius of light source. */
/*     SRCPOS     I   Position of center of light source. */
/*     NPTS       I   Number of points in terminator point set. */
/*     TRMPTS     O   Terminator point set. */

/* $ Detailed_Input */

/*     TYPE           is a string indicating the type of terminator to */
/*                    compute:  umbral or penumbral.  The umbral */
/*                    terminator is the boundary of the portion of the */
/*                    ellipsoid surface in total shadow.  The penumbral */
/*                    terminator is the boundary of the portion of the */
/*                    surface that is completely illuminated.  Possible */
/*                    values of TYPE are */

/*                       'UMBRAL' */
/*                       'PENUMBRAL' */

/*                    Case and leading or trailing blanks in TYPE are */
/*                    not significant. */

/*     A, */
/*     B, */
/*     C              are the lengths of the semi-axes of a triaxial */
/*                    ellipsoid.  The ellipsoid is centered at the */
/*                    origin and oriented so that its axes lie on the */
/*                    x, y and z axes.  A, B, and C are the lengths of */
/*                    the semi-axes that point in the x, y, and z */
/*                    directions respectively. */

/*                    Length units associated with A, B, and C must */
/*                    match those associated with SRCRAD, SRCPOS, */
/*                    and the output TRMPTS. */

/*     SRCRAD         is the radius of the spherical light source. */

/*     SRCPOS         is the position of the center of the light source */
/*                    relative to the center of the ellipsoid. */

/*     NPTS           is the number of terminator points to compute. */


/* $ Detailed_Output */

/*     TRMPTS         is an array of points on the umbral or penumbral */
/*                    terminator of the ellipsoid, as specified by the */
/*                    input argument TYPE.  The Ith point is contained */
/*                    in the array elements */

/*                        TRMPTS(J,I),  J = 1, 2, 3 */

/*                    The terminator points are expressed in the */
/*                    body-fixed reference frame associated with the */
/*                    ellipsoid.  Units are those associated with */
/*                    the input axis lengths. */

/*                    Each terminator point is the point of tangency of */
/*                    a plane that is also tangent to the light source. */
/*                    These associated points of tangency on the light */
/*                    source have uniform distribution in longitude when */
/*                    expressed in a cylindrical coordinate system whose */
/*                    Z-axis is SRCPOS.  The magnitude of the separation */
/*                    in longitude between these tangency points on the */
/*                    light source is */

/*                       2*Pi / NPTS */

/*                    If the target is spherical, the terminator points */
/*                    also are uniformly distributed in longitude in the */
/*                    cylindrical system described above.  If the target */
/*                    is non-spherical, the longitude distribution of */
/*                    the points generally is not uniform. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the terminator type is not recognized, the error */
/*         SPICE(NOTSUPPORTED) is signaled. */

/*     2)  If the set size NPTS is not at least 1, the error */
/*         SPICE(INVALIDSIZE) is signaled. */

/*     3)  If any of the ellipsoid's semi-axis lengths is non-positive, */
/*         the error SPICE(INVALIDAXISLENGTH) is signaled. */

/*     4)  If the light source has non-positive radius, the error */
/*         SPICE(INVALIDRADIUS) is signaled. */

/*     5)  If the light source intersects the smallest sphere */
/*         centered at the origin and containing the ellipsoid, the */
/*         error SPICE(OBJECTSTOOCLOSE) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine models the boundaries of shadow regions on an */
/*     ellipsoid "illuminated" by a spherical light source.  Light rays */
/*     are assumed to travel along straight lines; refraction is not */
/*     modeled. */

/*     Points on the ellipsoid at which the entire cap of the light */
/*     source is visible are considered to be completely illuminated. */
/*     Points on the ellipsoid at which some portion (or all) of the cap */
/*     of the light source are blocked are considered to be in partial */
/*     (or total) shadow. */

/*     In this routine, we use the term "umbral terminator" to denote */
/*     the curve usually called the "terminator":  this curve is the */
/*     boundary of the portion of the surface that lies in total shadow. */
/*     We use the term "penumbral terminator" to denote the boundary of */
/*     the completely illuminated portion of the surface. */

/*     In general, the terminator on an ellipsoid is a more complicated */
/*     curve than the limb (which is always an ellipse).  Aside from */
/*     various special cases, the terminator does not lie in a plane. */

/*     However, the condition for a point X on the ellipsoid to lie on */
/*     the terminator is simple:  a plane tangent to the ellipsoid at X */
/*     must also be tangent to the light source.  If this tangent plane */
/*     does not intersect the vector from the center of the ellipsoid to */
/*     the center of the light source, then X lies on the umbral */
/*     terminator; otherwise X lies on the penumbral terminator. */

/* $ Examples */

/*     See the SPICELIB routine EDTERM. */

/* $ Restrictions */

/*     This is a private SPICELIB routine.  User applications should not */
/*     call this routine. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.1, 02-OCT-2021 (NJB) */

/*        Corrected typo in comments. */

/* -    SPICELIB Version 1.1.0, 24-APR-2012 (NJB) */

/*        Deleted computations of unused quantities */
/*        MAXANG and MINANG. */

/* -    SPICELIB Version 1.0.0, 03-FEB-2007 (NJB) */

/* -& */
/* $ Index_Entries */

/*     find terminator on ellipsoid */
/*     find umbral terminator on ellipsoid */
/*     find penumbral terminator on ellipsoid */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Standard SPICELIB error handling. */

    /* Parameter adjustments */
    trmpts_dim2 = *npts;

    /* Function Body */
    if (return_()) {
	return 0;
    }
    chkin_("ZZEDTERM", (ftnlen)8);

/*     Check the terminator type. */

    ljust_(type__, loctyp, type_len, (ftnlen)50);
    ucase_(loctyp, loctyp, (ftnlen)50, (ftnlen)50);
    if (s_cmp(loctyp, "UMBRAL", (ftnlen)50, (ftnlen)6) == 0) {
	umbral = TRUE_;
    } else if (s_cmp(loctyp, "PENUMBRAL", (ftnlen)50, (ftnlen)9) == 0) {
	umbral = FALSE_;
    } else {
	setmsg_("Terminator type must be UMBRAL or PENUMBRAL but was actuall"
		"y #.", (ftnlen)63);
	errch_("#", type__, (ftnlen)1, type_len);
	sigerr_("SPICE(NOTSUPPORTED)", (ftnlen)19);
	chkout_("ZZEDTERM", (ftnlen)8);
	return 0;
    }

/*     Check the terminator set dimension. */

    if (*npts < 1) {
	setmsg_("Set must contain at least one point; NPTS  = #.", (ftnlen)47)
		;
	errint_("#", npts, (ftnlen)1);
	sigerr_("SPICE(INVALIDSIZE)", (ftnlen)18);
	chkout_("ZZEDTERM", (ftnlen)8);
	return 0;
    }

/*     The ellipsoid semi-axes must have positive length. */

    if (*a <= 0. || *b <= 0. || *c__ <= 0.) {
	setmsg_("Semi-axis lengths:  A = #, B = #, C = #. ", (ftnlen)41);
	errdp_("#", a, (ftnlen)1);
	errdp_("#", b, (ftnlen)1);
	errdp_("#", c__, (ftnlen)1);
	sigerr_("SPICE(INVALIDAXISLENGTH)", (ftnlen)24);
	chkout_("ZZEDTERM", (ftnlen)8);
	return 0;
    }

/*     Check the input light source radius. */

    if (*srcrad <= 0.) {
	setmsg_("Light source must have positive radius; actual radius was #."
		, (ftnlen)60);
	errdp_("#", srcrad, (ftnlen)1);
	sigerr_("SPICE(INVALIDRADIUS)", (ftnlen)20);
	chkout_("ZZEDTERM", (ftnlen)8);
	return 0;
    }

/*     The light source must not intersect the outer bounding */
/*     sphere of the ellipsoid. */

    d__ = vnorm_(srcpos);
/* Computing MAX */
    d__1 = max(*a,*b);
    rmax = max(d__1,*c__);
/* Computing MIN */
    d__1 = min(*a,*b);
    rmin = min(d__1,*c__);
    if (*srcrad + rmax >= d__) {

/*        The light source is too close. */

	setmsg_("Light source intersects outer bounding sphere of the ellips"
		"oid.  Light source radius = #; ellipsoid's longest axis = #;"
		" sum = #; distance between centers = #.", (ftnlen)158);
	errdp_("#", srcrad, (ftnlen)1);
	errdp_("#", &rmax, (ftnlen)1);
	d__1 = *srcrad + rmax;
	errdp_("#", &d__1, (ftnlen)1);
	errdp_("#", &d__, (ftnlen)1);
	sigerr_("SPICE(OBJECTSTOOCLOSE)", (ftnlen)22);
	chkout_("ZZEDTERM", (ftnlen)8);
	return 0;
    }

/*     Let the negative of the ellipsoid-light source vector be the */
/*     Z-axis of a frame we'll use to generate the terminator set. */

    vminus_(srcpos, z__);
    frame_(z__, x, y);

/*     Create the rotation matrix required to convert vectors */
/*     from the source-centered frame back to the target body-fixed */
/*     frame. */

    vequ_(x, trans);
    vequ_(y, &trans[3]);
    vequ_(z__, &trans[6]);

/*     Find the maximum and minimum target radii. */

/* Computing MAX */
    d__1 = max(*a,*b);
    maxrad = max(d__1,*c__);
/* Computing MIN */
    d__1 = min(*a,*b);
    minrad = min(d__1,*c__);
    if (umbral) {

/*        Compute the angular offsets from the axis of rays tangent to */
/*        both the source and the bounding spheres of the target, where */
/*        the tangency points lie in a half-plane bounded by the line */
/*        containing the origin and SRCPOS.  (We'll call this line */
/*        the "axis.") */

/*        OUTANG corresponds to the target's outer bounding sphere; */
/*        INANG to the inner bounding sphere. */

	outang = asin((*srcrad - maxrad) / d__);
	inang = asin((*srcrad - minrad) / d__);
    } else {

/*        Compute the angular offsets from the axis of rays tangent to */
/*        both the source and the bounding spheres of the target, where */
/*        the tangency points lie in opposite half-planes bounded by the */
/*        axis (compare the case above). */

/*        OUTANG corresponds to the target's outer bounding sphere; */
/*        INANG to the inner bounding sphere. */

	outang = asin((*srcrad + maxrad) / d__);
	inang = asin((*srcrad + minrad) / d__);
    }

/*     Compute the angular delta we'll use for generating */
/*     terminator points. */

    delta = twopi_() / *npts;

/*     Generate the terminator points. */

    i__1 = *npts;
    for (i__ = 1; i__ <= i__1; ++i__) {
	theta = (i__ - 1) * delta;

/*        Let SRCPT be the surface point on the source lying in */
/*        the X-Y plane of the frame produced by FRAME */
/*        and corresponding to the angle THETA. */

	latrec_(srcrad, &theta, &c_b30, srcpt);

/*        Now solve for the angle by which SRCPT must be rotated (toward */
/*        +Z in the umbral case, away from +Z in the penumbral case) */
/*        so that a plane tangent to the source at SRCPT is also tangent */
/*        to the target. The rotation is bracketed by OUTANG on the low */
/*        side and INANG on the high side in the umbral case; the */
/*        bracketing values are reversed in the penumbral case. */

	if (umbral) {
	    angle = outang;
	} else {
	    angle = inang;
	}
	prvdif = twopi_();
	prvang = angle + halfpi_();
	nitr = 0;
	for(;;) { /* while(complicated condition) */
	    d__2 = (d__1 = angle - prvang, abs(d__1));
	    if (!(nitr <= 10 && touchd_(&d__2) < prvdif))
	    	break;
	    ++nitr;
	    d__2 = (d__1 = angle - prvang, abs(d__1));
	    prvdif = touchd_(&d__2);
	    prvang = angle;

/*           Find the closest point on the ellipsoid to the plane */
/*           corresponding to "ANGLE". */

/*           The tangent point on the source is obtained by rotating */
/*           SRCPT by ANGLE towards +Z.  The plane's normal vector is */
/*           parallel to VTX in the source-centered frame. */

	    latrec_(srcrad, &theta, &angle, vtx);
	    vequ_(vtx, dir);

/*           VTX and DIR are expressed in the source-centered frame.  We */
/*           must translate VTX to the target frame and rotate both */
/*           vectors into that frame. */

	    mxv_(trans, vtx, vtemp);
	    vadd_(srcpos, vtemp, vtx);
	    mxv_(trans, dir, vtemp);
	    vequ_(vtemp, dir);

/*           Create the plane defined by VTX and DIR. */

	    nvp2pl_(dir, vtx, plane);

/*           Find the closest point on the ellipsoid to the plane. At */
/*           the point we seek, the outward normal on the ellipsoid is */
/*           parallel to the choice of plane normal that points away */
/*           from the origin.  We can always obtain this choice from */
/*           PL2NVC. */

	    pl2nvc_(plane, dir, &plcons);

/*           At the point */

/*               E = (x, y, z) */

/*           on the ellipsoid's surface, an outward normal */
/*           is */

/*               N = ( x/A**2, y/B**2, z/C**2 ) */

/*           which is also */

/*               lambda * ( DIR(1), DIR(2), DIR(3) ) */

/*           Equating components in the normal vectors yields */

/*               E = lambda * ( DIR(1)*A**2, DIR(2)*B**2, DIR(3)*C**2 ) */

/*           Taking the inner product with the point E itself and */
/*           applying the ellipsoid equation, we find */

/*               lambda * <DIR, E>  =  < N, E >  =  1 */

/*           The first term above is */

/*               lambda**2 * || ( A*DIR(1), B*DIR(2), C*DIR(3) ) ||**2 */

/*           So the positive root lambda is */

/*               1 / || ( A*DIR(1), B*DIR(2), C*DIR(3) ) || */

/*           Having lambda we can compute E. */

	    d__1 = *a * dir[0];
	    d__2 = *b * dir[1];
	    d__3 = *c__ * dir[2];
	    vpack_(&d__1, &d__2, &d__3, v);
	    lambda = 1. / vnorm_(v);
	    d__1 = *a * v[0];
	    d__2 = *b * v[1];
	    d__3 = *c__ * v[2];
	    vpack_(&d__1, &d__2, &d__3, e);
	    vscl_(&lambda, e, &trmpts[(i__2 = i__ * 3 - 3) < trmpts_dim2 * 3 
		    && 0 <= i__2 ? i__2 : s_rnge("trmpts", i__2, "zzedterm_", 
		    (ftnlen)586)]);

/*           Make a new estimate of the plane rotation required to touch */
/*           the target. */

	    vsub_(&trmpts[(i__2 = i__ * 3 - 3) < trmpts_dim2 * 3 && 0 <= i__2 
		    ? i__2 : s_rnge("trmpts", i__2, "zzedterm_", (ftnlen)592)]
		    , vtx, offset);

/*           Let ANGERR be an estimate of the magnitude of angular error */
/*           between the plane and the terminator. */

	    angerr = vsep_(dir, offset) - halfpi_();

/*           Let S indicate the sign of the altitude error:  where */
/*           S is positive, the plane is above E. */

	    d__1 = vdot_(e, dir);
	    s = d_sign(&c_b35, &d__1);
	    if (umbral) {

/*              If the plane is above the target, increase the */
/*              rotation angle; otherwise decrease the angle. */

		angle += s * angerr;
	    } else {

/*              This is the penumbral case; decreasing the angle */
/*              "lowers" the plane toward the target. */

		angle -= s * angerr;
	    }
	}
    }
    chkout_("ZZEDTERM", (ftnlen)8);
    return 0;
} /* zzedterm_ */

