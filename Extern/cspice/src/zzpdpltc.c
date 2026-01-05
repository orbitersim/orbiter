/* zzpdpltc.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ZZPDPLTC (Planetodetic coordinates, point latitude check) */
logical zzpdpltc_(doublereal *re, doublereal *f, doublereal *p, doublereal *
	lat)
{
    /* System generated locals */
    logical ret_val;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    doublereal xxpt, yxpt, a, b;
    extern /* Subroutine */ int zzelnaxx_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *);
    doublereal r__;
    extern /* Subroutine */ int chkin_(char *, ftnlen), errdp_(char *, 
	    doublereal *, ftnlen);
    doublereal r2;
    extern logical failed_(void);
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     Indicate whether a given point on a planetodetic latitude cone */
/*     has the correct latitude sign. */

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

/*     COORDINATES */
/*     GEOMETRY */
/*     PLANETODETIC */
/*     LATITUDE */
/*     MATH */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     RE         I   Equatorial radius. */
/*     F          I   Flattening coefficient. */
/*     P          I   Three-dimensional point. */
/*     LAT        I   Planetodetic latitude. */

/*     The function returns .TRUE. if the sign of the planetodetic */
/*     latitude of the input point matches that of the input */
/*     planetodetic latitude. */

/* $ Detailed_Input */

/*     RE, */
/*     F          are, respectively, the equatorial radius */
/*                and flattening coefficient of a biaxial */
/*                spheroid. */

/*                The polar radius RP of the spheroid is */

/*                   RP = RE * ( 1 - F ) */

/*                RP may be less than, equal to, or greater than RE. */


/*     P          is a point (equivalently, a vector) in */
/*                three-dimensional space. P is expressed in Cartesian */
/*                coordinates. P must lie on the planetodetic */
/*                latitude cone corresponding to LAT. */

/*                The units of P must be consistent with those of RE. */


/*     LAT        is a planetodetic latitude value defining a cone. */
/*                Units are radius. */


/* $ Detailed_Output */

/*     The function returns .TRUE. if any of the following */
/*     are true: */

/*        - The input spheroid is prolate or spherical. */

/*        - The input latitude is zero. */

/*        - The input point is determined to have planetodetic */
/*          latitude of the same sign as the input latitude. */

/*    Otherwise the function returns .FALSE. */

/*    See Particulars below for details. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If either the equatorial radius or flattening coefficient is */
/*         invalid, the error SPICE(VALUEOUTOFRANGE) will be signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine supports computation of the intersection between */
/*     a ray and a planetodetic volume element. The routine is used */
/*     to determine whether the intersection of the ray and latitude */
/*     cone is actually on the element's boundary. */

/*     This function serves as a "macro" that executes a logical test */
/*     that would be awkward to perform in-line. */

/*     For a given reference spheroid, all points having a given */
/*     planetodetic latitude lie on a cone. The axis of the cone */
/*     coincides with the Z axis of the coordinate system. */

/*     The possibility that a point on a given latitude can have a Z */
/*     coordinate with the wrong sign exists for oblate reference */
/*     spheroids. For these shapes, the vertex of a positive latitude */
/*     cone has a negative Z component, and the vertex of a negative */
/*     latitude cone has a positive Z component. */

/*     The purpose of the function is to determine, for a point that */
/*     lies on a given planetodetic latitude cone, whether that point is */
/*     on the correct side of the X-Y plane: that is, the side */
/*     corresponding to the sign of the input latitude value. */

/*     This check is not as simple as checking the sign of the Z */
/*     component of the input point. For values of LAT that are non-zero */
/*     but have very small magnitude, points that are nominally on the */
/*     corresponding latitude cone may have Z components of the wrong */
/*     sign due to round-off errors that occurred in the process of */
/*     computing those points. For such cases, the input point is */
/*     checked by comparing its distance from the Z axis to the X */
/*     intercept of a line normal to the reference spheroid at a point */
/*     having planetodetic latitude LAT and longitude 0. This check can */
/*     be performed accurately even when the Z component of the input */
/*     point consists of noise. */

/*     This routine does not test the input point to determine whether */
/*     it lies on the latitude cone defined by the input argument LAT. */
/*     The caller must ensure that this is the case. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     This routine does not test the input point to determine whether */
/*     it lies on the latitude cone defined by the input argument LAT. */
/*     The caller must ensure that this is the case. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 13-JAN-2017 (NJB) */

/* -& */
/* $ Index_Entries */

/*     is point on planetodetic latitude boundary */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Give the function a default value. */

    ret_val = FALSE_;
    if (return_()) {
	return ret_val;
    }
    chkin_("ZZPDPLTC", (ftnlen)8);

/*     The equatorial radius must be greater than zero. */

    if (*re <= 0.) {
	setmsg_("Equatorial radius was *.", (ftnlen)24);
	errdp_("*", re, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("ZZPDPLTC", (ftnlen)8);
	return ret_val;
    }

/*     If the flattening coefficient is greater than one, the polar */
/*     radius computed below is negative. If it's equal to one, the */
/*     polar radius is zero. Either case is a problem, so signal an */
/*     error and check out. */

    if (*f >= 1.) {
	setmsg_("Flattening coefficient was *.", (ftnlen)29);
	errdp_("*", f, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("ZZPDPLTC", (ftnlen)8);
	return ret_val;
    }

/*     The input point is assumed to be on the cone */
/*     corresponding to the input latitude. */

/*     If the reference spheroid is prolate or spherical, there's */
/*     nothing to do: the point is automatically on the correct side of */
/*     the X-Y plane. */

    if (*f <= 0.) {
	ret_val = TRUE_;
    } else {

/*        This is the oblate case. */


/*        If the point is on the "correct" side of the X-Y plane--- */
/*        that is, its Z component as the same sign as LAT, the */
/*        point is considered to have the correct latitude. */

/*        If the point is on the X-Y plane, or if LAT is zero, the point */
/*        is considered to have the indicated latitude. We condense */
/*        these cases by requiring that */

/*              LAT * P(3) >= 0 */

/*           rather than */

/*              LAT * P(3) > 0 */


	if (p[2] * *lat >= 0.) {
	    ret_val = TRUE_;
	} else if (abs(*lat) >= .01) {

/*           Ideally, the input point is considered to have the given */
/*           latitude if the point is on the side of the X-Y plane */
/*           corresponding to the sign of the input latitude. The */
/*           problem with this criterion is that it can't be applied */
/*           correctly when LAT has very small magnitude. */

/*           If the magnitude of LAT is above the limit, it's ok to */
/*           use the sign of P(3) to determine whether the point */
/*           has the given latitude. */

/*           The point has the indicated latitude if both LAT and P(3) */
/*           have the same sign. In this case, we know they have the */
/*           opposite sign. */

	    ret_val = FALSE_;
	} else {

/*           At this point we know LAT is non-zero, so the cone */
/*           corresponding to LAT has its vertex on the opposite side of */
/*           the X-Y plane from any point having latitude LAT. So it's */
/*           possible for a point to be on the cone but not have the */
/*           correct latitude. */

/*           We're in the special case where the point's Z component */
/*           has the opposite sign as LAT, and the magnitude of LAT */
/*           is below the limit. We don't automatically reject the */
/*           point in this case: we'll accept it if it is far enough */
/*           from the Z axis to be outside the portion of the latitude */
/*           cone on the wrong side of the X-Y plane. */

	    a = *re;
	    b = a * (1. - *f);

/*           Compute the intercepts of a normal vector of a point */
/*           at latitude LAT, longitude 0, with the X and Y axes. */

	    zzelnaxx_(&a, &b, lat, &xxpt, &yxpt);
	    if (failed_()) {
		chkout_("ZZPDPLTC", (ftnlen)8);
		return ret_val;
	    }

/*           We check the point's distance from the Z axis. This can be */
/*           done accurately even when the Z component of P consists of */
/*           noise. */

/*           The point is considered to have the correct latitude when */
/*           it is farther from the Z axis than the intercept on the X */
/*           axis of a normal line passing through a point having */
/*           latitude LAT and longitude 0. Ideally, a point that is on */
/*           the latitude cone and that satisfies this criterion must be */
/*           on the correct side of the X-Y plane. */

	    r2 = p[0] * p[0] + p[1] * p[1];
	    r__ = sqrt((max(r2,0.)));
	    ret_val = r__ >= xxpt;
	}
    }
    chkout_("ZZPDPLTC", (ftnlen)8);
    return ret_val;
} /* zzpdpltc_ */

