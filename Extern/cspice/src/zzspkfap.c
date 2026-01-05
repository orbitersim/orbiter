/* zzspkfap.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ZZSPKFAP ( SPK function, apparent inertial state ) */
/* Subroutine */ int zzspkfap_(U_fp trgsub, doublereal *et, char *ref, char *
	abcorr, doublereal *stobs, doublereal *accobs, doublereal *starg, 
	doublereal *lt, doublereal *dlt, ftnlen ref_len, ftnlen abcorr_len)
{
    /* Initialized data */

    static logical pass1 = TRUE_;
    static char prvcor[5] = "     ";

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int vadd_(doublereal *, doublereal *, doublereal *
	    ), vequ_(doublereal *, doublereal *);
    static logical xmit;
    extern /* Subroutine */ int zzstelab_(logical *, doublereal *, doublereal 
	    *, doublereal *, doublereal *, doublereal *), zzvalcor_(char *, 
	    logical *, ftnlen), zzspkflt_(U_fp, doublereal *, char *, char *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, ftnlen, 
	    ftnlen);
    integer refid;
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen);
    doublereal pcorr[3];
    static logical uselt;
    extern logical failed_(void);
    logical attblk[15];
    doublereal dpcorr[3], corvel[3];
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), irfnum_(char *, integer *, ftnlen), setmsg_(char *, 
	    ftnlen);
    doublereal corpos[3];
    extern logical return_(void);
    static logical usestl;

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Given the state and acceleration of an observer relative to the */
/*     solar system barycenter, return the state (position and velocity) */
/*     of a target body relative to the observer, optionally corrected */
/*     for light time and stellar aberration. All input and output */
/*     vectors are expressed relative to an inertial reference frame. An */
/*     input subroutine provides the state of the target relative to its */
/*     center of motion. */

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
/* $ Abstract */

/*     Include file zzabcorr.inc */

/*     SPICE private file intended solely for the support of SPICE */
/*     routines.  Users should not include this file directly due */
/*     to the volatile nature of this file */

/*     The parameters below define the structure of an aberration */
/*     correction attribute block. */

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

/* $ Parameters */

/*     An aberration correction attribute block is an array of logical */
/*     flags indicating the attributes of the aberration correction */
/*     specified by an aberration correction string.  The attributes */
/*     are: */

/*        - Is the correction "geometric"? */

/*        - Is light time correction indicated? */

/*        - Is stellar aberration correction indicated? */

/*        - Is the light time correction of the "converged */
/*          Newtonian" variety? */

/*        - Is the correction for the transmission case? */

/*        - Is the correction relativistic? */

/*    The parameters defining the structure of the block are as */
/*    follows: */

/*       NABCOR    Number of aberration correction choices. */

/*       ABATSZ    Number of elements in the aberration correction */
/*                 block. */

/*       GEOIDX    Index in block of geometric correction flag. */

/*       LTIDX     Index of light time flag. */

/*       STLIDX    Index of stellar aberration flag. */

/*       CNVIDX    Index of converged Newtonian flag. */

/*       XMTIDX    Index of transmission flag. */

/*       RELIDX    Index of relativistic flag. */

/*    The following parameter is not required to define the block */
/*    structure, but it is convenient to include it here: */

/*       CORLEN    The maximum string length required by any aberration */
/*                 correction string */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 18-DEC-2004 (NJB) */

/* -& */
/*     Number of aberration correction choices: */


/*     Aberration correction attribute block size */
/*     (number of aberration correction attributes): */


/*     Indices of attributes within an aberration correction */
/*     attribute block: */


/*     Maximum length of an aberration correction string: */


/*     End of include file zzabcorr.inc */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     TRGSUB     I   Target body state subroutine. */
/*     ET         I   Observer epoch. */
/*     REF        I   Inertial reference frame of output state. */
/*     ABCORR     I   Aberration correction flag. */
/*     STOBS      I   State of the observer relative to the SSB. */
/*     ACCOBS     I   Acceleration of the observer relative to the SSB. */
/*     STARG      O   State of target. */
/*     LT         O   One way light time between observer and target. */
/*     DLT        O   Derivative of light time with respect to time. */

/* $ Detailed_Input */

/*     TRGSUB      is the name of an external subroutine that returns */
/*                 the geometric state of the target body relative to a */
/*                 center of motion, expressed in the inertial reference */
/*                 frame REF, at the epoch ET. */

/*                 The calling sequence of TRGSUB is */

/*                    SUBROUTINE TRGSUB ( ET, REF, TRGCTR, STATE ) */

/*                    DOUBLE PRECISION      ET */
/*                    CHARACTER*(*)         REF */
/*                    INTEGER               TRGCTR */
/*                    DOUBLE PRECISION      STATE ( 6 ) */


/*                    The inputs of TRGSUB are ET and REF; the outputs */
/*                    are TRGCTR and STATE. STATE is the geometric state */
/*                    of the target relative to the returned center of */
/*                    motion at ET, expressed in the frame REF. */

/*                 The target and observer define a state vector whose */
/*                 position component points from the observer to the */
/*                 target. */

/*     ET          is the ephemeris time, expressed as seconds past */
/*                 J2000 TDB, at which the state of the target body */
/*                 relative to the observer is to be computed.  ET */
/*                 refers to time at the observer's location. */

/*     REF         is the inertial reference frame with respect to which */
/*                 the input state STOBS, the input acceleration ACCOBS, */
/*                 and the output state STARG are expressed. REF must be */
/*                 recognized by the SPICE Toolkit.  The acceptable */
/*                 frames are listed in the Frames Required Reading, as */
/*                 well as in the SPICELIB routine CHGIRF. */

/*                 Case and blanks are not significant in the string */
/*                 REF. */

/*     ABCORR      indicates the aberration corrections to be applied */
/*                 to the state of the target body to account for one-way */
/*                 light time and stellar aberration. See the discussion */
/*                 in the header of SPKEZR for recommendations on */
/*                 how to choose aberration corrections. */

/*                 ABCORR may be any of the following: */

/*                    'NONE'     Apply no correction. Return the */
/*                               geometric state of the target body */
/*                               relative to the observer. */

/*                 The following values of ABCORR apply to the */
/*                 "reception" case in which photons depart from the */
/*                 target's location at the light-time corrected epoch */
/*                 ET-LT and *arrive* at the observer's location at ET: */

/*                    'LT'       Correct for one-way light time (also */
/*                               called "planetary aberration") using a */
/*                               Newtonian formulation. This correction */
/*                               yields the state of the target at the */
/*                               moment it emitted photons arriving at */
/*                               the observer at ET. */

/*                               The light time correction uses an */
/*                               iterative solution of the light time */
/*                               equation (see Particulars for details). */
/*                               The solution invoked by the 'LT' option */
/*                               uses one iteration. */

/*                    'LT+S'     Correct for one-way light time and */
/*                               stellar aberration using a Newtonian */
/*                               formulation. This option modifies the */
/*                               state obtained with the 'LT' option to */
/*                               account for the observer's velocity */
/*                               relative to the solar system */
/*                               barycenter. The result is the apparent */
/*                               state of the target---the position and */
/*                               velocity of the target as seen by the */
/*                               observer. */

/*                    'CN'       Converged Newtonian light time */
/*                               correction. In solving the light time */
/*                               equation, the 'CN' correction iterates */
/*                               until the solution converges (three */
/*                               iterations on all supported platforms). */
/*                               Whether the 'CN+S' solution is */
/*                               substantially more accurate than the */
/*                               'LT' solution depends on the geometry */
/*                               of the participating objects and on the */
/*                               accuracy of the input data. In all */
/*                               cases this routine will execute more */
/*                               slowly when a converged solution is */
/*                               computed. See the Particulars section of */
/*                               SPKEZR for a discussion of precision of */
/*                               light time corrections. */

/*                    'CN+S'     Converged Newtonian light time */
/*                               correction and stellar aberration */
/*                               correction. */


/*                 The following values of ABCORR apply to the */
/*                 "transmission" case in which photons *depart* from */
/*                 the observer's location at ET and arrive at the */
/*                 target's location at the light-time corrected epoch */
/*                 ET+LT: */

/*                    'XLT'      "Transmission" case:  correct for */
/*                               one-way light time using a Newtonian */
/*                               formulation. This correction yields the */
/*                               state of the target at the moment it */
/*                               receives photons emitted from the */
/*                               observer's location at ET. */

/*                    'XLT+S'    "Transmission" case:  correct for */
/*                               one-way light time and stellar */
/*                               aberration using a Newtonian */
/*                               formulation  This option modifies the */
/*                               state obtained with the 'XLT' option to */
/*                               account for the observer's velocity */
/*                               relative to the solar system */
/*                               barycenter. The position component of */
/*                               the computed target state indicates the */
/*                               direction that photons emitted from the */
/*                               observer's location must be "aimed" to */
/*                               hit the target. */

/*                    'XCN'      "Transmission" case:  converged */
/*                               Newtonian light time correction. */

/*                    'XCN+S'    "Transmission" case:  converged */
/*                               Newtonian light time correction and */
/*                               stellar aberration correction. */


/*                 Neither special nor general relativistic effects are */
/*                 accounted for in the aberration corrections applied */
/*                 by this routine. */

/*                 Case and blanks are not significant in the string */
/*                 ABCORR. */


/*     STOBS       is the geometric state of the observer relative to */
/*                 the solar system barycenter at ET. STOBS is expressed */
/*                 relative to the reference frame designated by REF. */
/*                 The target and observer define a state vector whose */
/*                 position component points from the observer to the */
/*                 target. */

/*     ACCOBS      is the geometric acceleration of the observer */
/*                 relative to the solar system barycenter at ET. This */
/*                 is the derivative with respect to time of the */
/*                 velocity portion of STOBS. ACCOBS is expressed */
/*                 relative to the reference frame designated by REF. */

/*                 ACCOBS is used for computing stellar aberration */
/*                 corrected velocity. If stellar aberration corrections */
/*                 are not specified by ABCORR, ACCOBS is ignored; the */
/*                 caller need not provide a valid input value in this */
/*                 case. */

/* $ Detailed_Output */

/*     STARG       is a Cartesian state vector representing the position */
/*                 and velocity of the target body relative to the */
/*                 specified observer. STARG is corrected for the */
/*                 specified aberrations, and is expressed with respect */
/*                 to the inertial reference frame designated by REF. */
/*                 The first three components of STARG represent the x-, */
/*                 y- and z-components of the target's position; last */
/*                 three components form the corresponding velocity */
/*                 vector. */

/*                 The position component of STARG points from the */
/*                 observer's location at ET to the aberration-corrected */
/*                 location of the target. Note that the sense of the */
/*                 position vector is independent of the direction of */
/*                 radiation travel implied by the aberration */
/*                 correction. */

/*                 Units are always km and km/sec. */

/*     LT          is the one-way light time between the observer and */
/*                 target in seconds.  If the target state is corrected */
/*                 for light time, then LT is the one-way light time */
/*                 between the observer and the light time-corrected */
/*                 target location. */

/*     DLT         is the derivative with respect to barycentric */
/*                 dynamical time of the one way light time between */
/*                 target and observer: */

/*                    DLT = d(LT)/d(ET) */

/*                 DLT can also be described as the rate of change of */
/*                 one way light time. DLT is unitless, since LT and */
/*                 ET both have units of TDB seconds. */

/*                 If the observer and target are at the same position, */
/*                 then DLT is set to zero. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If the value of ABCORR is not recognized, the error */
/*        is diagnosed by a routine in the call tree of this */
/*        routine. */

/*     2) If the reference frame requested is not a recognized */
/*        inertial reference frame, the error SPICE(BADFRAME) */
/*        is signaled. */

/*     3) If the state of the target relative to the solar system */
/*        barycenter cannot be computed, the error will be diagnosed */
/*        by routines in the call tree of this routine. */

/*     4) If the observer and target are at the same position, */
/*        then DLT is set to zero. This situation could arise, */
/*        for example, when the observer is Mars and the target */
/*        is the Mars barycenter. */

/* $ Files */

/*     This routine computes states using SPK files that have been */
/*     loaded into the SPICE system, normally via the kernel loading */
/*     interface routine FURNSH.  Application programs typically load */
/*     kernels once before this routine is called, for example during */
/*     program initialization; kernels need not be loaded repeatedly. */
/*     See the routine FURNSH and the SPK and KERNEL Required Reading */
/*     for further information on loading (and unloading) kernels. */

/*     If any of the ephemeris data used to compute STARG are expressed */
/*     relative to a non-inertial frame in the SPK files providing those */
/*     data, additional kernels may be needed to enable the reference */
/*     frame transformations required to compute the state. Normally */
/*     these additional kernels are PCK files or frame kernels. Any such */
/*     kernels must already be loaded at the time this routine is */
/*     called. */

/* $ Particulars */

/*     This routine supports higher-level routines that can */
/*     perform both light time and stellar aberration corrections */
/*     and that use target states provided by subroutines rather */
/*     than by the conventional, public SPK APIs. For example, this */
/*     routine can be used for objects having fixed positions */
/*     on the surfaces of planets. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     1) This routine must not be called by routines of the SPICE */
/*        frame subsystem. It must not be called by any portion of */
/*        the SPK subsystem other than the private SPK function-based */
/*        component. */

/*     2) The input subroutine TRGSUB must not call this routine */
/*        or any of the supporting, private SPK routines */

/*           ZZSPKFAT */
/*           ZZSPKFAO */
/*           ZZSPKFZO */
/*           ZZSPKFZT */

/*     3)  When possible, the routine SPKGEO should be used instead of */
/*         this routine to compute geometric states. SPKGEO introduces */
/*         less round-off error when the observer and target have common */
/*         center that is closer to both objects than is the solar */
/*         system barycenter. */

/*     4)  Unlike most other SPK state computation routines, this */
/*         routine requires that the output state be relative to an */
/*         inertial reference frame. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 04-JUL-2014 (NJB) */

/*        Discussion of light time corrections was updated. Assertions */
/*        that converged light time corrections are unlikely to be */
/*        useful were removed. */

/* -    SPICELIB Version 1.0.0, 09-JAN-2011 (NJB) */

/* -& */
/* $ Index_Entries */

/*     low-level aberration-corrected state computation */
/*     low-level light time and stellar aberration correction */

/* -& */
/* $ Revisions */

/*     None. */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Saved variables */


/*     Initial values */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("ZZSPKFAP", (ftnlen)8);
    if (pass1 || s_cmp(abcorr, prvcor, abcorr_len, (ftnlen)5) != 0) {

/*        The aberration correction flag differs from the value it */
/*        had on the previous call, if any.  Analyze the new flag. */

	zzvalcor_(abcorr, attblk, abcorr_len);
	if (failed_()) {
	    chkout_("ZZSPKFAP", (ftnlen)8);
	    return 0;
	}

/*        The aberration correction flag is recognized; save it. */

	s_copy(prvcor, abcorr, (ftnlen)5, abcorr_len);

/*        Set logical flags indicating the attributes of the requested */
/*        correction: */

/*           XMIT is .TRUE. when the correction is for transmitted */
/*           radiation. */

/*           USELT is .TRUE. when any type of light time correction */
/*           (normal or converged Newtonian) is specified. */

/*           USECN indicates converged Newtonian light time correction. */

/*        The above definitions are consistent with those used by */
/*        ZZVALCOR. */

	xmit = attblk[4];
	uselt = attblk[1];
	usestl = attblk[2];
	pass1 = FALSE_;
    }

/*     See if the reference frame is a recognized inertial frame. */

    irfnum_(ref, &refid, ref_len);
    if (refid == 0) {
	setmsg_("The requested frame '#' is not a recognized inertial frame. "
		, (ftnlen)60);
	errch_("#", ref, (ftnlen)1, ref_len);
	sigerr_("SPICE(BADFRAME)", (ftnlen)15);
	chkout_("ZZSPKFAP", (ftnlen)8);
	return 0;
    }

/*     Get the state of the target relative to the observer, */
/*     optionally corrected for light time. */

    zzspkflt_((U_fp)trgsub, et, ref, abcorr, stobs, starg, lt, dlt, ref_len, 
	    abcorr_len);
    if (failed_()) {
	chkout_("ZZSPKFAP", (ftnlen)8);
	return 0;
    }

/*     If stellar aberration corrections are not needed, we're */
/*     already done. */

    if (! usestl) {
	chkout_("ZZSPKFAP", (ftnlen)8);
	return 0;
    }

/*     Get the stellar aberration correction and its time derivative. */

    zzstelab_(&xmit, accobs, &stobs[3], starg, pcorr, dpcorr);

/*     Adding the stellar aberration correction to the light */
/*     time-corrected target position yields the position corrected for */
/*     both light time and stellar aberration. */

    vadd_(pcorr, starg, corpos);
    vequ_(corpos, starg);

/*     Velocity is treated in an analogous manner. */

    vadd_(dpcorr, &starg[3], corvel);
    vequ_(corvel, &starg[3]);
    chkout_("ZZSPKFAP", (ftnlen)8);
    return 0;
} /* zzspkfap_ */

