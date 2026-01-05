/* zzstelab.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__3 = 3;
static doublereal c_b7 = 1.;

/* $Procedure ZZSTELAB ( Private --- stellar aberration correction ) */
/* Subroutine */ int zzstelab_(logical *xmit, doublereal *accobs, doublereal *
	vobs, doublereal *starg, doublereal *scorr, doublereal *dscorr)
{
    /* System generated locals */
    integer i__1;
    doublereal d__1, d__2;

    /* Builtin functions */
    double sqrt(doublereal);
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    extern /* Subroutine */ int vadd_(doublereal *, doublereal *, doublereal *
	    );
    doublereal dphi, rhat[3];
    extern /* Subroutine */ int vhat_(doublereal *, doublereal *);
    extern doublereal vdot_(doublereal *, doublereal *);
    extern /* Subroutine */ int vequ_(doublereal *, doublereal *);
    doublereal term1[3], term2[3], term3[3], c__, lcacc[3];
    integer i__;
    doublereal s;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    doublereal saoff[6]	/* was [3][2] */, drhat[3];
    extern /* Subroutine */ int dvhat_(doublereal *, doublereal *);
    doublereal ptarg[3], evobs[3], srhat[6], vphat[3], vtarg[3];
    extern /* Subroutine */ int vlcom_(doublereal *, doublereal *, doublereal 
	    *, doublereal *, doublereal *), vperp_(doublereal *, doublereal *,
	     doublereal *);
    extern doublereal vnorm_(doublereal *);
    extern logical vzero_(doublereal *);
    extern /* Subroutine */ int vlcom3_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *), cleard_(integer *, doublereal *);
    doublereal vp[3];
    extern doublereal clight_(void);
    doublereal dptmag, ptgmag, eptarg[3], dvphat[3], lcvobs[3];
    extern /* Subroutine */ int qderiv_(integer *, doublereal *, doublereal *,
	     doublereal *, doublereal *), sigerr_(char *, ftnlen), chkout_(
	    char *, ftnlen), setmsg_(char *, ftnlen);
    doublereal svphat[6];
    extern logical return_(void);
    extern /* Subroutine */ int vminus_(doublereal *, doublereal *);
    doublereal sgn, dvp[3], svp[6];

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Return the state (position and velocity) of a target body */
/*     relative to an observing body, optionally corrected for light */
/*     time (planetary aberration) and stellar aberration. */

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

/*     SPK */

/* $ Keywords */

/*     EPHEMERIS */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     XMIT       I   Reception/transmission flag. */
/*     ACCOBS     I   Observer acceleration relative to SSB. */
/*     VOBS       I   Observer velocity relative to to SSB. */
/*     STARG      I   State of target relative to observer. */
/*     SCORR      O   Stellar aberration correction for position. */
/*     DSCORR     O   Stellar aberration correction for velocity. */

/* $ Detailed_Input */

/*     XMIT        is a logical flag which is set to .TRUE. for the */
/*                 "transmission" case in which photons *depart* from */
/*                 the observer's location at an observation epoch ET */
/*                 and arrive at the target's location at the light-time */
/*                 corrected epoch ET+LT, where LT is the one-way light */
/*                 time between observer and target; XMIT is set to */
/*                 .FALSE. for "reception" case in which photons depart */
/*                 from the target's location at the light-time */
/*                 corrected epoch ET-LT and *arrive* at the observer's */
/*                 location at ET. */

/*                 Note that the observation epoch is not used in this */
/*                 routine. */

/*                 XMIT must be consistent with any light time */
/*                 corrections used for the input state STARG: if that */
/*                 state has been corrected for "reception" light time; */
/*                 XMIT must be .FALSE.; otherwise XMIT must be .TRUE. */

/*     ACCOBS      is the geometric acceleration of the observer */
/*                 relative to the solar system barycenter. Units are */
/*                 km/sec**2. ACCOBS must be expressed relative to */
/*                 an inertial reference frame. */

/*     VOBS        is the geometric velocity of the observer relative to */
/*                 the solar system barycenter. VOBS must be expressed */
/*                 relative to the same inertial reference frame as */
/*                 ACCOBS. Units are km/sec. */

/*     STARG       is the Cartesian state of the target relative to the */
/*                 observer. Normally STARG has been corrected for */
/*                 one-way light time, but this is not required. STARG */
/*                 must be expressed relative to the same inertial */
/*                 reference frame as ACCOBS. Components are */
/*                 (x, y, z, dx, dy, dz). Units are km and km/sec. */

/* $ Detailed_Output */

/*     SCORR       is the stellar aberration correction for the position */
/*                 component of STARG. Adding SCORR to this position */
/*                 vector produces the input observer-target position, */
/*                 corrected for stellar aberration. */

/*                 The reference frame of SCORR is the common frame */
/*                 relative to which the inputs ACCOBS, VOBS, and STARG */
/*                 are expressed. Units are km. */

/*     DSCORR      is the stellar aberration correction for the velocity */
/*                 component of STARG. Adding DSCORR to this velocity */
/*                 vector produces the input observer-target velocity, */
/*                 corrected for stellar aberration. */

/*                 The reference frame of DSCORR is the common frame */
/*                 relative to which the inputs ACCOBS, VOBS, and STARG */
/*                 are expressed. Units are km/s. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If attempt to divide by zero occurs, the error */
/*        SPICE(DIVIDEBYZERO) will be signaled. This case may occur */
/*        due to uninitialized inputs. */

/*     2) Loss of precision will occur for geometric cases in which */
/*        VOBS is nearly parallel to the position component of STARG. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine computes a Newtonian estimate of the stellar */
/*     aberration correction of an input state. Normally the input state */
/*     has already been corrected for one-way light time. */

/*     Since stellar aberration corrections are typically "small" */
/*     relative to the magnitude of the input observer-target position */
/*     and velocity, this routine avoids loss of precision by returning */
/*     the corrections themselves rather than the corrected state */
/*     vector. This allows the caller to manipulate (for example, */
/*     interpolate) the corrections with greater accuracy. */

/* $ Examples */

/*     See SPICELIB routine SPKACS. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     SPK Required Reading. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.0, 15-APR-2014 (NJB) */

/*        Added RETURN test and discovery check-in. */
/*        Check for division by zero was added. This */
/*        case might occur due to uninitialized inputs. */

/* -    SPICELIB Version 1.0.1, 12-FEB-2009 (NJB) */

/*        Minor updates were made to the inline documentation. */

/* -    SPICELIB Version 1.0.0, 17-JAN-2008 (NJB) */

/* -& */

/*     Note for the maintenance programmer */
/*     =================================== */

/*     The source code of the test utility T_ZZSTLABN must be */
/*     kept in sync with the source code of this routine. That */
/*     routine uses a value of SEPLIM that forces the numeric */
/*     branch of the velocity computation to be taken in all */
/*     cases. See the documentation of that routine for details. */


/*     SPICELIB functions */


/*     Local parameters */


/*     Let PHI be the (non-negative) rotation angle of the stellar */
/*     aberration correction; then SEPLIM is a limit on how close PHI */
/*     may be to zero radians while stellar aberration velocity is */
/*     computed analytically. When sin(PHI) is less than SEPLIM, the */
/*     velocity must be computed numerically. */


/*     Let TDELTA be the time interval, measured in seconds, */
/*     used for numerical differentiation of the stellar */
/*     aberration correction, when this is necessary. */


/*     Local variables */


/*     Use discovery check-in. */

    if (return_()) {
	return 0;
    }

/*     In the discussion below, the dot product of vectors X and Y */
/*     is denoted by */

/*        <X,Y> */

/*     The speed of light is denoted by the lower case letter "c." BTW, */
/*     variable names used here are case-sensitive: upper case "C" */
/*     represents a different quantity which is unrelated to the speed */
/*     of light. */

/*     Variable names ending in "HAT" denote unit vectors. Variable */
/*     names starting with "D" denote derivatives with respect to time. */

/*     We'll compute the correction SCORR and its derivative with */
/*     respect to time DSCORR for the reception case. In the */
/*     transmission case, we perform the same computation with the */
/*     negatives of the observer velocity and acceleration. */

/*     In the code below, we'll store the position and velocity portions */
/*     of the input observer-target state STARG in the variables PTARG */
/*     and VTARG, respectively. */

/*     Let VP be the component of VOBS orthogonal to PTARG. VP */
/*     is defined as */

/*         VOBS - < VOBS, RHAT > RHAT                                 (1) */

/*     where RHAT is the unit vector */

/*         PTARG/||PTARG|| */

/*     Then */

/*        ||VP||/c                                                    (2) */

/*     is the magnitude of */

/*        s = sin( phi )                                              (3) */

/*     where phi is the stellar aberration correction angle. We'll */
/*     need the derivative with respect to time of (2). */

/*     Differentiating (1) with respect to time yields the */
/*     velocity DVP, where, letting */

/*        DRHAT  =  d(RHAT) / dt */
/*        VPHAT  =  VP      / ||VP|| */
/*        DVPMAG =  d( ||VP|| ) / dt */

/*     we have */

/*        DVP = d(VP)/dt */

/*            = ACCOBS - (  ( <VOBS,DRHAT> + <ACCOBS, RHAT> )*RHAT */
/*                        +   <VOBS,RHAT>  * DRHAT                 )  (4) */

/*     and */

/*        DVPMAG = < DVP, VPHAT >                                     (5) */

/*     Now we can find the derivative with respect to time of */
/*     the stellar aberration angle phi: */

/*        ds/dt = d(sin(phi))/dt = d(phi)/dt * cos(phi)               (6) */

/*     Using (2) and (5), we have for positive phi, */

/*        ds/dt = (1/c)*DVPMAG = (1/c)*<DVP, VPHAT>                   (7) */

/*     Then for positive phi */

/*        d(phi)/dt = (1/cos(phi)) * (1/c) * <DVP, VPHAT>             (8) */

/*     Equation (8) is well-defined as along as VP is non-zero: */
/*     if VP is the zero vector, VPHAT is undefined. We'll treat */
/*     the singular and near-singular cases separately. */

/*     The aberration correction itself is a rotation by angle phi */
/*     from RHAT towards VP, so the corrected vector is */

/*        ( sin(phi)*VPHAT + cos(phi)*RHAT ) * ||PTARG|| */

/*     and  we can express the offset of the corrected vector from */
/*     PTARG, which is the output SCORR, as */

/*        SCORR = */

/*        ( sin(phi)*VPHAT + (cos(phi)-1)*RHAT ) * ||PTARG||          (9) */

/*     Let DPTMAG be defined as */

/*        DPTMAG  =  d ( ||PTARG|| ) / dt                            (10) */

/*     Then the derivative with respect to time of SCORR is */

/*        DSCORR = */

/*             (      sin(phi)*DVPHAT */

/*                +   cos(phi)*d(phi)/dt * VPHAT */

/*                +  (cos(phi) - 1) * DRHAT */

/*                +  ( -sin(phi)*d(phi)/dt ) * RHAT   ) * ||PTARG|| */

/*           + ( sin(phi)*VPHAT + (cos(phi)-1)*RHAT ) * DPTMAG       (11) */


/*     Computations begin here: */

/*     Split STARG into position and velocity components. Compute */

/*        RHAT */
/*        DRHAT */
/*        VP */
/*        DPTMAG */

    if (*xmit) {
	vminus_(vobs, lcvobs);
	vminus_(accobs, lcacc);
    } else {
	vequ_(vobs, lcvobs);
	vequ_(accobs, lcacc);
    }
    vequ_(starg, ptarg);
    vequ_(&starg[3], vtarg);
    dvhat_(starg, srhat);
    vequ_(srhat, rhat);
    vequ_(&srhat[3], drhat);
    vperp_(lcvobs, rhat, vp);
    dptmag = vdot_(vtarg, rhat);

/*     Compute sin(phi) and cos(phi), which we'll call S and C */
/*     respectively. Note that phi is always close to zero for */
/*     realistic inputs (for which ||VOBS|| << CLIGHT), so the */
/*     cosine term is positive. */

    s = vnorm_(vp) / clight_();
/* Computing MAX */
    d__1 = 0., d__2 = 1 - s * s;
    c__ = sqrt((max(d__1,d__2)));
    if (c__ == 0.) {

/*        C will be used as a divisor later (in the computation */
/*        of DPHI), so we'll put a stop to the problem here. */

	chkin_("ZZSTELAB", (ftnlen)8);
	setmsg_("Cosine of the aberration angle is 0; this cannot occur for "
		"realistic observer velocities. This case can arise due to un"
		"initialized inputs. This cosine value is used as a divisor i"
		"n a later computation, so it must not be equal to zero.", (
		ftnlen)234);
	sigerr_("SPICE(DIVIDEBYZERO)", (ftnlen)19);
	chkout_("ZZSTELAB", (ftnlen)8);
	return 0;
    }

/*     Compute the unit vector VPHAT and the stellar */
/*     aberration correction. We avoid relying on */
/*     VHAT's exception handling for the zero vector. */

    if (vzero_(vp)) {
	cleard_(&c__3, vphat);
    } else {
	vhat_(vp, vphat);
    }

/*     Now we can use equation (9) to obtain the stellar */
/*     aberration correction SCORR: */

/*        SCORR = */

/*           ( sin(phi)*VPHAT + (cos(phi)-1)*RHAT ) * ||PTARG|| */


    ptgmag = vnorm_(ptarg);
    d__1 = ptgmag * s;
    d__2 = ptgmag * (c__ - 1.);
    vlcom_(&d__1, vphat, &d__2, rhat, scorr);

/*     Now we use S as an estimate of PHI to decide if we're */
/*     going to differentiate the stellar aberration correction */
/*     analytically or numerically. */

/*     Note that S is non-negative by construction, so we don't */
/*     need to use the absolute value of S here. */

    if (s >= 1e-6) {

/*        This is the analytic case. */

/*        Compute DVP---the derivative of VP with respect to time. */
/*        Recall equation (4): */

/*        DVP = d(VP)/dt */

/*            = ACCOBS - (  ( <VOBS,DRHAT> + <ACCOBS, RHAT> )*RHAT */
/*                        +   <VOBS,RHAT>  * DRHAT                 ) */

	d__1 = -vdot_(lcvobs, drhat) - vdot_(lcacc, rhat);
	d__2 = -vdot_(lcvobs, rhat);
	vlcom3_(&c_b7, lcacc, &d__1, rhat, &d__2, drhat, dvp);
	vhat_(vp, vphat);

/*        Now we can compute DVPHAT, the derivative of VPHAT: */

	vequ_(vp, svp);
	vequ_(dvp, &svp[3]);
	dvhat_(svp, svphat);
	vequ_(&svphat[3], dvphat);

/*        Compute the DPHI, the time derivative of PHI, using equation 8: */

/*           d(phi)/dt = (1/cos(phi)) * (1/c) * <DVP, VPHAT> */


	dphi = 1. / (c__ * clight_()) * vdot_(dvp, vphat);

/*        At long last we've assembled all of the "ingredients" required */
/*        to compute DSCORR: */

/*           DSCORR = */

/*             (     sin(phi)*DVPHAT */

/*                +  cos(phi)*d(phi)/dt * VPHAT */

/*                +  (cos(phi) - 1) * DRHAT */

/*                +  ( -sin(phi)*d(phi)/dt ) * RHAT   ) * ||PTARG|| */

/*                +  ( sin(phi)*VPHAT + (cos(phi)-1)*RHAT ) * DPTMAG */


	d__1 = c__ * dphi;
	vlcom_(&s, dvphat, &d__1, vphat, term1);
	d__1 = c__ - 1.;
	d__2 = -s * dphi;
	vlcom_(&d__1, drhat, &d__2, rhat, term2);
	vadd_(term1, term2, term3);
	d__1 = dptmag * s;
	d__2 = dptmag * (c__ - 1.);
	vlcom3_(&ptgmag, term3, &d__1, vphat, &d__2, rhat, dscorr);
    } else {

/*        This is the numeric case. We're going to differentiate */
/*        the stellar aberration correction offset vector using */
/*        a quadratic estimate. */

	for (i__ = 1; i__ <= 2; ++i__) {

/*           Set the sign of the time offset. */

	    if (i__ == 1) {
		sgn = -1.;
	    } else {
		sgn = 1.;
	    }

/*           Estimate the observer's velocity relative to the */
/*           solar system barycenter at the current epoch. We use */
/*           the local copies of the input velocity and acceleration */
/*           to make a linear estimate. */

	    d__1 = sgn * 1.;
	    vlcom_(&c_b7, lcvobs, &d__1, lcacc, evobs);

/*           Estimate the observer-target vector. We use the */
/*           observer-target state velocity to make a linear estimate. */

	    d__1 = sgn * 1.;
	    vlcom_(&c_b7, starg, &d__1, &starg[3], eptarg);

/*           Let RHAT be the unit observer-target position. */
/*           Compute the component of the observer's velocity */
/*           that is perpendicular to the target position; call */
/*           this vector VP. Also compute the unit vector in */
/*           the direction of VP. */

	    vhat_(eptarg, rhat);
	    vperp_(evobs, rhat, vp);
	    if (vzero_(vp)) {
		cleard_(&c__3, vphat);
	    } else {
		vhat_(vp, vphat);
	    }

/*           Compute the sine and cosine of the correction */
/*           angle. */

	    s = vnorm_(vp) / clight_();
/* Computing MAX */
	    d__1 = 0., d__2 = 1 - s * s;
	    c__ = sqrt((max(d__1,d__2)));

/*           Compute the vector offset of the correction. */

	    ptgmag = vnorm_(eptarg);
	    d__1 = ptgmag * s;
	    d__2 = ptgmag * (c__ - 1.);
	    vlcom_(&d__1, vphat, &d__2, rhat, &saoff[(i__1 = i__ * 3 - 3) < 6 
		    && 0 <= i__1 ? i__1 : s_rnge("saoff", i__1, "zzstelab_", (
		    ftnlen)597)]);
	}

/*        Now compute the derivative. */

	qderiv_(&c__3, saoff, &saoff[3], &c_b7, dscorr);
    }

/*     At this point the correction offset SCORR and its derivative */
/*     with respect to time DSCORR are both set. */

    return 0;
} /* zzstelab_ */

