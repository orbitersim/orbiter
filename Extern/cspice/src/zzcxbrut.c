/* zzcxbrut.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static doublereal c_b6 = 1.;

/* $Procedure  ZZCXBRUT ( Cone-segment intersection by brute force ) */
/* Subroutine */ int zzcxbrut_(doublereal *apex, doublereal *axis, doublereal 
	*angle, doublereal *endpt1, doublereal *endpt2, doublereal *xpt, 
	logical *isbrck)
{
    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */
    double cos(doublereal);

    /* Local variables */
    extern /* Subroutine */ int vadd_(doublereal *, doublereal *, doublereal *
	    );
    doublereal high;
    extern /* Subroutine */ int vhat_(doublereal *, doublereal *);
    extern doublereal vdot_(doublereal *, doublereal *);
    integer nitr;
    extern /* Subroutine */ int vsub_(doublereal *, doublereal *, doublereal *
	    ), vequ_(doublereal *, doublereal *);
    doublereal uoff1[3], uoff2[3], x[3], delta;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    doublereal midpt;
    logical state;
    extern /* Subroutine */ int vlcom_(doublereal *, doublereal *, doublereal 
	    *, doublereal *, doublereal *);
    extern logical vzero_(doublereal *);
    logical state1, state2;
    doublereal dp;
    extern doublereal pi_(void), halfpi_(void);
    doublereal locang, cosang, ux[3], locaxi[3];
    extern /* Subroutine */ int sigerr_(char *, ftnlen), vhatip_(doublereal *)
	    , chkout_(char *, ftnlen), setmsg_(char *, ftnlen);
    doublereal dp1, dp2, prvdlt;
    extern logical return_(void);
    extern /* Subroutine */ int vminus_(doublereal *, doublereal *);
    doublereal seg[3], low, off1[3], off2[3];

/* $ Abstract */


/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Compute a bracketed point of intersection of a specified nappe of */
/*     a cone and a line segment by "brute force"---specifically by */
/*     bisection. */

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

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     APEX       I   Apex of cone. */
/*     AXIS       I   Axis of cone. */
/*     ANGLE      I   Angle of cone. */
/*     ENDPT1, */
/*     ENDPT2     I   Endpoints of line segment. */
/*     XPT        O   Intersection point, if it exists. */
/*     ISBRCK     O   Logical flag indicating whether root is bracketed. */

/* $ Detailed_Input */

/*     APEX       is the apex (tip) of the cone. In this routine's */
/*                documentation, we'll consider the cone to be a */
/*                semi-infinite pyramid with circular cross-section. In */
/*                some contexts, this object is called one "nappe" of */
/*                the complete cone. */

/*     AXIS       is an axis vector of the cone. */

/*     ANGLE      is the angular separation from AXIS of the rays */
/*                comprising the cone. Let the notation */

/*                   < A, B > */

/*                denote the dot product of vectors A and B, and let */

/*                   ||A|| */

/*                denote the norm of vector A. Then the cone is the set */
/*                of points */

/*                             X-APEX       AXIS */
/*                   { X:  < ----------,  -------- >  =  cos(ANGLE) } */
/*                           ||X-APEX||   ||AXIS|| */


/*     ENDPT1, */
/*     ENDPT2     are endpoints of a line segment. These points */
/*                must be distinct. */

/*                Exactly one of ENDPT1 and ENDPT2 must be inside */
/*                the cone. */

/* $ Detailed_Output */

/*     NXPTS      is the number of points of intersection of the input */
/*                line segment and cone. */

/*     XPT        is the unique point of intersection of the segment and */
/*                cone that lies on the line segment connecting ENDPT1 */
/*                and ENDPT2. */

/*     ISBRCK     is a logical flag that is set to .TRUE. if and only if */
/*                ENDPT1 and ENDPT2 bracket a root. Equivalently, the */
/*                endpoints bracket a root when exactly one of ENDPT1 */
/*                and ENDPT2 is inside the cone. */


/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If AXIS is the zero vector, the error SPICE(ZEROVECTOR) */
/*         will be signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine is used by the SPICELIB routine INCNSG to handle */
/*     cases where solution of a quadratic equation is subject to */
/*     excessive loss of precision. */

/* $ Examples */

/*     See usage in INCNSG. */

/* $ Restrictions */

/*     This is a private SPICELIB routine. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0 30-SEP-2016 (NJB) */

/* -& */
/* $ Index_Entries */

/*     brute force intersection of line segment and cone */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Use discovery check-in. */

    if (return_()) {
	return 0;
    }

/*     Check the axis. */

    if (vzero_(axis)) {
	chkin_("ZZCXBRUT", (ftnlen)8);
	setmsg_("Cone axis is the zero vector", (ftnlen)28);
	sigerr_("SPICE(ZEROVECTOR)", (ftnlen)17);
	chkout_("ZZCXBRUT", (ftnlen)8);
	return 0;
    }

/*     Make a local version of the cone's axis and angle. The */
/*     angle will be less than or equal to pi/2 radians. */

    if (*angle > halfpi_()) {
	locang = pi_() - *angle;
	vminus_(axis, locaxi);
    } else {
	locang = *angle;
	vequ_(axis, locaxi);
    }
    vhatip_(locaxi);
    cosang = cos(locang);

/*     Calculate the offsets of the endpoints from the apex, */
/*     and get unit-length versions of these. */

    vsub_(endpt1, apex, off1);
    vsub_(endpt2, apex, off2);
    vhat_(off1, uoff1);
    vhat_(off2, uoff2);

/*     Get the dot products of the unit offsets with the axis. */
/*     These will serve as proxies for latitude. */

    dp1 = vdot_(uoff1, locaxi);
    dp2 = vdot_(uoff2, locaxi);

/*     The "state" variables at the endpoints are .TRUE. if */
/*     the endpoints are on or inside the cone. */

    state1 = dp1 >= cosang;
    state2 = dp2 >= cosang;

/*     The intersection is supposed to be bracketed. Return */
/*     if not, indicating the situation via ISBRCK. */

    *isbrck = state1 != state2;
    if (! (*isbrck)) {
	return 0;
    }

/*     Prepare for a solution by bisection. */

    vsub_(off2, off1, seg);
    low = 0.;
    high = 1.;
    delta = (d__1 = high - low, abs(d__1));
    prvdlt = 2.;
    nitr = 0;
    while(delta > 1e-15 && delta < prvdlt && nitr < 1000) {
	midpt = (low + high) / 2;
	vlcom_(&c_b6, off1, &midpt, seg, x);
	vhat_(x, ux);
	dp = vdot_(ux, locaxi);
	state = dp >= cosang;
	if (state == state1) {

/*           There has been no state change between OFF1 */
/*           and XPT. */

	    low = midpt;
	} else {
	    high = midpt;
	}
	prvdlt = delta;
	delta = (d__1 = high - low, abs(d__1));
	++nitr;
    }

/*     X is an offset from APEX. The solution is an offset from the */
/*     origin. */

    vadd_(apex, x, xpt);
    return 0;
} /* zzcxbrut_ */

