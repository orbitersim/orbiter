/* zzedtmpt.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__9 = 9;
static doublereal c_b31 = 1e-14;

/* $Procedure ZZEDTMPT ( Ellipsoid terminator point in half-plane ) */
/* Subroutine */ int zzedtmpt_(logical *umbral, doublereal *a, doublereal *b, 
	doublereal *c__, doublereal *r__, doublereal *axis, doublereal *
	plnvec, doublereal *point)
{
    /* System generated locals */
    doublereal d__1;

    /* Local variables */
    extern /* Subroutine */ int vadd_(doublereal *, doublereal *, doublereal *
	    ), vhat_(doublereal *, doublereal *);
    doublereal maxr;
    extern /* Subroutine */ int vscl_(doublereal *, doublereal *, doublereal *
	    );
    doublereal proj[3];
    extern doublereal vdot_(doublereal *, doublereal *);
    integer nitr;
    extern /* Subroutine */ int vequ_(doublereal *, doublereal *);
    doublereal d__, h__, s, angle;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    doublereal theta;
    extern /* Subroutine */ int errdp_(char *, doublereal *, ftnlen);
    doublereal const__, trans[9]	/* was [3][3] */, taxis[3];
    extern doublereal vdist_(doublereal *, doublereal *);
    extern /* Subroutine */ int vperp_(doublereal *, doublereal *, doublereal 
	    *), vcrss_(doublereal *, doublereal *, doublereal *);
    extern doublereal vnorm_(doublereal *);
    extern logical vzero_(doublereal *);
    extern /* Subroutine */ int vrotv_(doublereal *, doublereal *, doublereal 
	    *, doublereal *);
    extern logical failed_(void);
    doublereal ta, tb, tc, xa, xb, xc;
    extern /* Subroutine */ int cleard_(integer *, doublereal *);
    extern doublereal dasine_(doublereal *, doublereal *), halfpi_(void);
    doublereal angerr;
    extern doublereal touchd_(doublereal *);
    doublereal normal[3], hplnml[3];
    extern /* Subroutine */ int sigerr_(char *, ftnlen), ednmpt_(doublereal *,
	     doublereal *, doublereal *, doublereal *, doublereal *), chkout_(
	    char *, ftnlen);
    doublereal sgnnml[3], tmpvec[3], targpt[3];
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen);
    doublereal tplnvc[3], srcpnt[3], utaxis[3];
    extern logical return_(void);
    extern /* Subroutine */ int mxv_(doublereal *, doublereal *, doublereal *)
	    ;

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     Compute an umbral or penumbral terminator point on an ellipsoidal */
/*     target. The point is confined to a specified half-plane. The */
/*     illumination source is spherical. */

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
/*     ILLUMINATION */
/*     TERMINATOR */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     UMBRAL     I   Flag indicating whether terminator is umbral. */
/*     A          I   Semi-axis length in the X direction. */
/*     B          I   Semi-axis length in the Y direction. */
/*     C          I   Semi-axis length in the Z direction. */
/*     R          I   Radius of illumination source. */
/*     AXIS       I   Axis vector from target to source. */
/*     PLNVEC     I   Vector in cutting half-plane. */
/*     POINT      O   Terminator point. */

/* $ Detailed_Input */

/*     UMBRAL     is a logical flag that must be set by the caller */
/*                to .TRUE. if an umbral terminator point is to be */
/*                computed, and set to .FALSE. if a penumbral */
/*                terminator point is to be computed. */

/*     A, */
/*     B, */
/*     C          are, respectively, the semi-axis lengths of a */
/*                triaxial ellipsoid representing a target object. */
/*                The axes of the ellipsoid are aligned with the */
/*                axes of the Cartesian coordinate system. */


/*     R          is the radius of the spherical model of the */
/*                illumination source. */


/*     AXIS       is the position of the center of the illumination */
/*                source relative to the center of the target. AXIS is */
/*                contained in the line bounding cutting half-planes in */
/*                which terminator points are found. */


/*     PLNVEC     is a vector that, together with AXIS, defines */
/*                a cutting half-plane. The half-plane contains */
/*                PLNVEC and has the line containing AXIS as a */
/*                boundary. */

/*                The terminator point that is sought lies within */
/*                the half-plane. */

/* $ Detailed_Output */

/*     POINT      is a terminator point lying within the half-plane */
/*                defined by AXIS and PLNVEC. */

/*                When UMBRAL is set to .TRUE., POINT lies on the */
/*                umbral terminator: the plane tangent to the target */
/*                ellipsoid at POINT is also tangent to the illumination */
/*                source. This plane does not intersect the vector */
/*                AXIS. */

/*                When UMBRAL is set to .FALSE., POINT lies on the */
/*                penumbral terminator: the plane tangent to the target */
/*                ellipsoid at POINT is also tangent to the illumination */
/*                source. This plane intersects the vector AXIS. */

/*                Unless the target is spherical, the plane tangent */
/*                to the target at POINT is usually tangent to the */
/*                illumination source at a point outside the plane */
/*                containing the cutting half-plane. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If any of the target ellipsoid's semi-axis lengths is */
/*         non-positive, the error SPICE(INVALIDAXISLENGTH) is signaled. */

/*     2)  If the radius of the illumination source is non-positive, the */
/*         error SPICE(INVALIDRADIUS) is signaled. */

/*     3)  If AXIS is the zero vector, the error SPICE(ZEROVECTOR) is */
/*         signaled. */

/*     4)  If the target and illumination source are separated by */
/*         less than the sum of the radius of the source sphere */
/*         and the maximum radius of the target ellipsoid, the */
/*         error SPICE(OBJECTSTOOCLOSE) is signaled. */

/*     5)  If PLNVEC is the zero vector, the error SPICE(ZEROVECTOR) is */
/*         signaled. */

/*     6)  If AXIS and PLNVEC are linearly dependent, the error */
/*         SPICE(DEGENERATECASE) is signaled. */

/*     7)  If the terminator solution doesn't converge, the error */
/*         SPICE(NOCONVERGENCE) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The "umbral" terminator on an ellipsoid is the boundary of the */
/*     region that is in total shadow. At any point P on this boundary, */
/*     the illumination source is below and tangent to the local */
/*     horizontal plane at P. */

/*     The "penumbral" terminator on an ellipsoid is the boundary of the */
/*     region that totally illuminated. At any point P on this boundary, */
/*     the illumination source is above and tangent to the local */
/*     horizontal plane at P. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     This is a SPICELIB private routine. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.0, 30-JUN-2016 (NJB) */

/* -& */
/* $ Index_Entries */

/*     compute ellipsoid terminator point in half-plane */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Tolerance used for arcsine arguments: */


/*     Angular error used to determine convergence: */


/*     Maximum number of iterations allowed for root finding: */


/*     Local variables */

    if (return_()) {
	return 0;
    }
    chkin_("ZZEDTMPT", (ftnlen)8);

/*     Check A, B, C, and R. */

    if (*a <= 0. || *b <= 0. || *c__ <= 0.) {
	setmsg_("Target radii must be strictly positive but were #, #, #.", (
		ftnlen)56);
	errdp_("#", a, (ftnlen)1);
	errdp_("#", b, (ftnlen)1);
	errdp_("#", c__, (ftnlen)1);
	sigerr_("SPICE(INVALIDAXISLENGTH)", (ftnlen)24);
	chkout_("ZZEDTMPT", (ftnlen)8);
	return 0;
    }
    if (*r__ <= 0.) {
	setmsg_("Source radius must be strictly positive but was #.", (ftnlen)
		50);
	errdp_("#", r__, (ftnlen)1);
	sigerr_("SPICE(INVALIDRADIUS)", (ftnlen)20);
	chkout_("ZZEDTMPT", (ftnlen)8);
	return 0;
    }

/*     Check AXIS and PLNVEC. */

    if (vzero_(axis)) {
	setmsg_("AXIS must be a non-zero vector but is in fact zero.", (
		ftnlen)51);
	sigerr_("SPICE(ZEROVECTOR)", (ftnlen)17);
	chkout_("ZZEDTMPT", (ftnlen)8);
	return 0;
    }
/* Computing MAX */
    d__1 = max(*a,*b);
    if (*r__ + max(d__1,*c__) >= vnorm_(axis)) {
	setmsg_("Centers of source and target are too close together; distan"
		"ce is #. Radius of source is #; semi-axis lengths are #, #, "
		"#.", (ftnlen)121);
	d__1 = vnorm_(axis);
	errdp_("#", &d__1, (ftnlen)1);
	errdp_("#", r__, (ftnlen)1);
	errdp_("#", a, (ftnlen)1);
	errdp_("#", b, (ftnlen)1);
	errdp_("#", c__, (ftnlen)1);
	sigerr_("SPICE(OBJECTSTOOCLOSE)", (ftnlen)22);
	chkout_("ZZEDTMPT", (ftnlen)8);
	return 0;
    }
    if (vzero_(plnvec)) {
	setmsg_("PLNVEC must be a non-zero vector but is in fact zero.", (
		ftnlen)53);
	sigerr_("SPICE(ZEROVECTOR)", (ftnlen)17);
	chkout_("ZZEDTMPT", (ftnlen)8);
	return 0;
    }

/*     Transform the source, target, axis, and plane vector */
/*     so that the target becomes a unit sphere. */

    cleard_(&c__9, trans);
    ta = 1. / *a;
    tb = 1. / *b;
    tc = 1. / *c__;
    xa = ta * *r__;
    xb = tb * *r__;
    xc = tc * *r__;
    trans[0] = ta;
    trans[4] = tb;
    trans[8] = tc;

/*     TNEGAX is the negative of the transformed axis. */
/*     UTAXIS is the unit vector in the direction of TNEGAX. */

    mxv_(trans, plnvec, tplnvc);
    mxv_(trans, axis, taxis);
    vhat_(taxis, utaxis);

/*     Let HPLNML be a normal vector to the plane containing */
/*     the transformed axis and plane vectors. */

    vcrss_(tplnvc, taxis, hplnml);
    if (vzero_(hplnml)) {
	setmsg_("Plane reference vector and axis are linearly dependent.", (
		ftnlen)55);
	sigerr_("SPICE(DEGENERATECASE)", (ftnlen)21);
	chkout_("ZZEDTMPT", (ftnlen)8);
	return 0;
    }

/*     Let MAXR be an outer bounding radius for the transformed */
/*     source sphere. */

/* Computing MAX */
    d__1 = max(xa,xb);
    maxr = max(d__1,xc);
    d__ = vnorm_(taxis);
    if (*umbral) {

/*        Find the angle between the negative axis and a ray tangent to */
/*        both the transformed target and the outer bounding sphere of */
/*        the transformed source. Here a tangent point on the */
/*        transformed target is the vertex, and the tangent ray is */
/*        confined to the half-plane normal to HPLNML, containing */
/*        TPLNVC, and bounded by the line containing TNEGAX. The */
/*        tangent ray does not cross the line containing TNEGAX. */

	d__1 = (maxr - 1.) / d__;
	angle = dasine_(&d__1, &c_b31);
	if (failed_()) {
	    chkout_("ZZEDTMPT", (ftnlen)8);
	    return 0;
	}

/*        Create the tangent point on the transformed target. */

	theta = -(halfpi_() + angle);
	vrotv_(utaxis, hplnml, &theta, targpt);

/*        S is the sign applied to pi/2 - ANGLE. */

	s = 1.;
    } else {

/*        This is the penumbral case. The tangent ray crosses */
/*        the line containing TNEGAX. */


/*        The tangent line always slopes downward (toward AXIS) */
/*        toward the light source. */

	d__1 = (maxr + 1.) / d__;
	angle = dasine_(&d__1, &c_b31);
	if (failed_()) {
	    chkout_("ZZEDTMPT", (ftnlen)8);
	    return 0;
	}

/*        Create the tangent point on the transformed target. */

	theta = angle - halfpi_();
	vrotv_(utaxis, hplnml, &theta, targpt);
	s = -1.;
    }

/*     The tangent point is also a normal direction for the plane */
/*     tangent to both objects. Get the corresponding unit normal and */
/*     the plane constant. */

    vhat_(targpt, normal);
    const__ = vdot_(normal, targpt);

/*     Find the height of the plane relative to the transformed source. */
/*     We'll find the unique point on the transformed source where the */
/*     outward normal is parallel to NORMAL and find the height of this */
/*     point relative to the plane. */

/*     Let SGNNML be the "signed" normal which is parallel to NORMAL */
/*     in the umbral case and anti-parallel otherwise. */

    vscl_(&s, normal, sgnnml);
    ednmpt_(&xa, &xb, &xc, sgnnml, srcpnt);

/*     Express the source point as an offset from the transformed */
/*     target center. */

    vadd_(srcpnt, taxis, tmpvec);
    vequ_(tmpvec, srcpnt);

/*     H is the height of the surface point on the source, relative */
/*     to the plane tangent to the target at TARGPT. ANGERR is the */
/*     corresponding angular error estimate: an estimate of the */
/*     amount by which TARGPT needs to be rotated in the positive */
/*     sense about HPLNML to make the plane contain SRCPNT. */

    h__ = vdot_(srcpnt, normal) - const__;
    d__1 = -h__ / d__;
    angerr = touchd_(&d__1);
    nitr = 0;

/*     The loop terminates when the angular error magnitude */
/*     stops decreasing. If the iteration count exceeds the */
/*     limit, an error will be signaled. */

    while(abs(angerr) > 1e-15 && nitr <= 20) {

/*        Rotate the target point about HPLNML in the positive sense */
/*        by the angular error. This should make the tangent plane */
/*        closer to the source point. */

	vrotv_(targpt, hplnml, &angerr, tmpvec);
	vequ_(tmpvec, targpt);
	vhat_(targpt, normal);

/*        Re-compute the normal and constant of the tangent plane. */

	const__ = vdot_(normal, targpt);
	vscl_(&s, normal, sgnnml);

/*        Find the near point on the source to the tangent plane. */

	ednmpt_(&xa, &xb, &xc, sgnnml, srcpnt);
	vadd_(srcpnt, taxis, tmpvec);
	vequ_(tmpvec, srcpnt);

/*        Re-compute the height error and angular error. */

	h__ = vdot_(srcpnt, normal) - const__;
	vperp_(srcpnt, hplnml, proj);
	d__ = vdist_(proj, targpt);
	d__1 = -h__ / d__;
	angerr = touchd_(&d__1);
	++nitr;
	if (nitr > 20) {
	    setmsg_("Tangent finding loop failed to converge. Iteration coun"
		    "t = #.", (ftnlen)61);
	    errint_("#", &nitr, (ftnlen)1);
	    sigerr_("SPICE(NOCONVERGENCE)", (ftnlen)20);
	    chkout_("ZZEDTMPT", (ftnlen)8);
	    return 0;
	}
    }

/*     Apply the inverse distortion transformation to TARGPT in order to */
/*     obtain the tangent point on the original, ellipsoidal target. */

    point[0] = *a * targpt[0];
    point[1] = *b * targpt[1];
    point[2] = *c__ * targpt[2];
    chkout_("ZZEDTMPT", (ftnlen)8);
    return 0;
} /* zzedtmpt_ */

