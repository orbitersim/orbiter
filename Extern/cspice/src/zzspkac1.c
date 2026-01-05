/* zzspkac1.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__0 = 0;
static integer c__3 = 3;
static doublereal c_b13 = 1.;

/* $Procedure ZZSPKAC1 ( S/P Kernel, aberration corrected state ) */
/* Subroutine */ int zzspkac1_(integer *targ, doublereal *et, char *ref, char 
	*abcorr, integer *obs, doublereal *starg, doublereal *lt, doublereal *
	dlt, ftnlen ref_len, ftnlen abcorr_len)
{
    /* Initialized data */

    static logical first = TRUE_;
    static char prvcor[5] = "     ";

    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    extern /* Subroutine */ int zzspkas1_(integer *, doublereal *, char *, 
	    char *, doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, ftnlen, ftnlen), zzspkgo1_(integer *, doublereal *, 
	    char *, integer *, doublereal *, doublereal *, ftnlen), zzvalcor_(
	    char *, logical *, ftnlen);
    integer i__;
    doublereal t;
    integer refid;
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen);
    doublereal ltssb, ssblt, stobs[12]	/* was [6][2] */;
    extern logical failed_(void);
    extern /* Subroutine */ int cleard_(integer *, doublereal *);
    logical attblk[15];
    extern /* Subroutine */ int qderiv_(integer *, doublereal *, doublereal *,
	     doublereal *, doublereal *);
    doublereal ssbobs[6];
    extern /* Subroutine */ int chkout_(char *, ftnlen), sigerr_(char *, 
	    ftnlen), irfnum_(char *, integer *, ftnlen), setmsg_(char *, 
	    ftnlen);
    extern logical return_(void);
    static logical usestl;
    doublereal acc[3];

/* $ Abstract */

/*     Return the state (position and velocity) of a target body */
/*     relative to an observer, optionally corrected for light time */
/*     and stellar aberration, expressed relative to an inertial */
/*     reference frame. */

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
/*     TARG       I   Target body. */
/*     ET         I   Observer epoch. */
/*     REF        I   Inertial reference frame of output state. */
/*     ABCORR     I   Aberration correction flag. */
/*     OBS        I   Observer. */
/*     STARG      O   State of target. */
/*     LT         O   One way light time between observer and target. */
/*     DLT        O   Derivative of light time with respect to time. */

/* $ Detailed_Input */

/*     TARG        is the NAIF ID code for a target body.  The target */
/*                 and observer define a state vector whose position */
/*                 component points from the observer to the target. */

/*     ET          is the ephemeris time, expressed as seconds past */
/*                 J2000 TDB, at which the state of the target body */
/*                 relative to the observer is to be computed.  ET */
/*                 refers to time at the observer's location. */

/*     REF         is the inertial reference frame with respect to which */
/*                 the output state STARG is expressed. REF must be */
/*                 recognized by the SPICE Toolkit.  The acceptable */
/*                 frames are listed in the Frames Required Reading, as */
/*                 well as in the SPICELIB routine CHGIRF. */

/*                 Case and blanks are not significant in the string */
/*                 REF. */

/*     ABCORR      indicates the aberration corrections to be applied */
/*                 to the state of the target body to account for one-way */
/*                 light time and stellar aberration.  See the discussion */
/*                 in the Particulars section for recommendations on */
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


/*     OBS         is the NAIF ID code for the observer body.  The */
/*                 target and observer define a state vector whose */
/*                 position component points from the observer to the */
/*                 target. */

/* $ Detailed_Output */

/*     STARG       is a Cartesian state vector representing the position */
/*                 and velocity of the target body relative to the */
/*                 specified observer. STARG is corrected for the */
/*                 specified aberrations, and is expressed with respect */
/*                 to the specified inertial reference frame.  The first */
/*                 three components of STARG represent the x-, y- and */
/*                 z-components of the target's position; last three */
/*                 components form the corresponding velocity vector. */

/*                 The position component of STARG points from the */
/*                 observer's location at ET to the aberration-corrected */
/*                 location of the target. Note that the sense of the */
/*                 position vector is independent of the direction of */
/*                 radiation travel implied by the aberration */
/*                 correction. */

/*                 Units are always km and km/sec. */

/*     LT          is the one-way light time between the observer and */
/*                 target in seconds.  If the target state is corrected */
/*                 for aberrations, then LT is the one-way light time */
/*                 between the observer and the light time corrected */
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
/*     these additional kernels are PCK files or frame kernels. Any */
/*     such kernels must already be loaded at the time this routine is */
/*     called. */

/* $ Particulars */

/*     This routine supports higher-level SPK API routines that can */
/*     perform both light time and stellar aberration corrections. */
/*     User applications normally will not need to call this routine */
/*     directly. */

/*     See the header of the routine SPKEZR for a detailed discussion */
/*     of aberration corrections. */

/* $ Examples */

/*    1) Look up a sequence of states of the Moon as seen from the */
/*       Earth. Use light time and stellar aberration corrections. */
/*       Compute the first state for the epoch 2000 JAN 1 12:00:00 TDB; */
/*       compute subsequent states at intervals of 1 hour. For each */
/*       epoch, display the states, the one way light time between */
/*       target and observer, and the rate of change of the one way */
/*       light time. */

/*       Use the following meta-kernel to specify the kernels to */
/*       load: */

/*          KPL/MK */

/*          This meta-kernel is intended to support operation of SPICE */
/*          example programs. The kernels shown here should not be */
/*          assumed to contain adequate or correct versions of data */
/*          required by SPICE-based user applications. */

/*          In order for an application to use this meta-kernel, the */
/*          kernels referenced here must be present in the user's */
/*          current working directory. */


/*          \begindata */

/*             KERNELS_TO_LOAD = ( 'de418.bsp', */
/*                                 'pck00008.tpc', */
/*                                 'naif0008.tls'  ) */

/*          \begintext */


/*       The code example follows: */

/*           PROGRAM EX1 */
/*           IMPLICIT NONE */
/*     C */
/*     C     Local constants */
/*     C */
/*     C     The meta-kernel name shown here refers to a file whose */
/*     C     contents are those shown above. This file and the kernels */
/*     C     it references must exist in your current working directory. */
/*     C */
/*     C */
/*           CHARACTER*(*)         META */
/*           PARAMETER           ( META   = 'example.mk' ) */
/*     C */
/*     C     Use a time step of 1 hour; look up 5 states. */
/*     C */
/*           DOUBLE PRECISION      STEP */
/*           PARAMETER           ( STEP   = 3600.0D0 ) */

/*           INTEGER               MAXITR */
/*           PARAMETER           ( MAXITR = 5 ) */
/*     C */
/*     C     Local variables */
/*     C */
/*           DOUBLE PRECISION      DLT */
/*           DOUBLE PRECISION      ET */
/*           DOUBLE PRECISION      ET0 */
/*           DOUBLE PRECISION      LT */
/*           DOUBLE PRECISION      STATE ( 6 ) */
/*           INTEGER               I */

/*     C */
/*     C     Load the SPK and LSK kernels via the meta-kernel. */
/*     C */
/*           CALL FURNSH ( META ) */
/*     C */
/*     C     Convert the start time to seconds past J2000 TDB. */
/*     C */
/*           CALL STR2ET ( '2000 JAN 1 12:00:00 TDB', ET0 ) */
/*     C */
/*     C     Step through a series of epochs, looking up a */
/*     C     state vector at each one. */
/*     C */
/*           DO I = 1, MAXITR */

/*              ET = ET0 + (I-1)*STEP */
/*     C */
/*     C        Look up a state vector at epoch ET using the */
/*     C        following inputs: */
/*     C */
/*     C           Target:                 Moon (NAIF ID code 301) */
/*     C           Reference frame:        J2000 */
/*     C           Aberration correction:  Light time and stellar */
/*     C                                   aberration ('LT+S') */
/*     C           Observer:               Earth (NAIF ID code 399) */
/*     C */

/*              CALL SPKACS ( 301, ET,    'J2000', 'LT+S', */
/*          .                 399, STATE, LT,      DLT     ) */

/*              WRITE (*,*) 'ET = ', ET */
/*              WRITE (*,*) 'J2000 x-position (km):   ', STATE(1) */
/*              WRITE (*,*) 'J2000 y-position (km):   ', STATE(2) */
/*              WRITE (*,*) 'J2000 z-position (km):   ', STATE(3) */
/*              WRITE (*,*) 'J2000 x-velocity (km/s): ', STATE(4) */
/*              WRITE (*,*) 'J2000 y-velocity (km/s): ', STATE(5) */
/*              WRITE (*,*) 'J2000 z-velocity (km/s): ', STATE(6) */
/*              WRITE (*,*) 'One-way light time (s):  ', LT */
/*              WRITE (*,*) 'Light time rate:         ', DLT */
/*              WRITE (*,*) ' ' */

/*           END DO */

/*           END */


/*     The output produced by this program will vary somewhat as */
/*     a function of the platform on which the program is built and */
/*     executed. On a PC/Linux/g77 platform, the following output */
/*     was produced: */

/*        ET =   0. */
/*        J2000 x-position (km):    -291584.614 */
/*        J2000 y-position (km):    -266693.406 */
/*        J2000 z-position (km):    -76095.6532 */
/*        J2000 x-velocity (km/s):   0.643439157 */
/*        J2000 y-velocity (km/s):  -0.666065874 */
/*        J2000 z-velocity (km/s):  -0.301310063 */
/*        One-way light time (s):    1.34231061 */
/*        Light time rate:           1.07316909E-07 */

/*        ET =   3600. */
/*        J2000 x-position (km):    -289256.459 */
/*        J2000 y-position (km):    -269080.605 */
/*        J2000 z-position (km):    -77177.3528 */
/*        J2000 x-velocity (km/s):   0.64997032 */
/*        J2000 y-velocity (km/s):  -0.660148253 */
/*        J2000 z-velocity (km/s):  -0.299630418 */
/*        One-way light time (s):    1.34269395 */
/*        Light time rate:           1.05652599E-07 */

/*        ET =   7200. */
/*        J2000 x-position (km):    -286904.897 */
/*        J2000 y-position (km):    -271446.417 */
/*        J2000 z-position (km):    -78252.9655 */
/*        J2000 x-velocity (km/s):   0.656443883 */
/*        J2000 y-velocity (km/s):  -0.654183552 */
/*        J2000 z-velocity (km/s):  -0.297928533 */
/*        One-way light time (s):    1.34307131 */
/*        Light time rate:           1.03990457E-07 */

/*        ET =   10800. */
/*        J2000 x-position (km):    -284530.133 */
/*        J2000 y-position (km):    -273790.671 */
/*        J2000 z-position (km):    -79322.4117 */
/*        J2000 x-velocity (km/s):   0.662859505 */
/*        J2000 y-velocity (km/s):  -0.648172247 */
/*        J2000 z-velocity (km/s):  -0.296204558 */
/*        One-way light time (s):    1.34344269 */
/*        Light time rate:           1.02330665E-07 */

/*        ET =   14400. */
/*        J2000 x-position (km):    -282132.378 */
/*        J2000 y-position (km):    -276113.202 */
/*        J2000 z-position (km):    -80385.612 */
/*        J2000 x-velocity (km/s):   0.669216846 */
/*        J2000 y-velocity (km/s):  -0.642114815 */
/*        J2000 z-velocity (km/s):  -0.294458645 */
/*        One-way light time (s):    1.3438081 */
/*        Light time rate:           1.00673404E-07 */


/* $ Restrictions */

/*     1) The kernel files to be used by SPKACS must be loaded */
/*        (normally by the SPICELIB kernel loader FURNSH) before */
/*        this routine is called. */

/*     2) Unlike most other SPK state computation routines, this */
/*        routine requires that the output state be relative to an */
/*        inertial reference frame. */

/* $ Literature_References */

/*     SPK Required Reading. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 23-AUG-2021 (NJB) */

/*        Bug fix: aberration correction strings are now parsed using */
/*        ZZVALCOR. This improves error checking. */

/* -    SPICELIB Version 1.0.1, 04-JUL-2014 (NJB) */

/*        Discussion of light time corrections was updated. Assertions */
/*        that converged light time corrections are unlikely to be */
/*        useful were removed. */

/* -    SPICELIB Version 1.0.0, 11-JAN-2008 (NJB) */

/* -& */
/* $ Index_Entries */

/*     low-level aberration correction */
/*     aberration-corrected state from spk file */
/*     get light time and stellar aberration-corrected state */

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
    } else {
	chkin_("ZZSPKAC1", (ftnlen)8);
    }
    if (first || s_cmp(abcorr, prvcor, abcorr_len, (ftnlen)5) != 0) {

/*        The aberration correction flag differs from the value it */
/*        had on the previous call, if any.  Analyze the new flag. */

	zzvalcor_(abcorr, attblk, abcorr_len);
	if (failed_()) {
	    chkout_("ZZSPKAC1", (ftnlen)8);
	    return 0;
	}

/*        The aberration correction flag is recognized; save it. */

	s_copy(prvcor, abcorr, (ftnlen)5, abcorr_len);

/*        Set logical flags indicating the attributes of the requested */
/*        correction: */

/*           USESTL is .TRUE. when stellar aberration correction is */
/*           specified. */

/*        The above definitions are consistent with those used by */
/*        ZZVALCOR. */

	usestl = attblk[2];
	first = FALSE_;
    }

/*     See if the reference frame is a recognized inertial frame. */

    irfnum_(ref, &refid, ref_len);
    if (refid == 0) {
	setmsg_("The requested frame '#' is not a recognized inertial frame. "
		, (ftnlen)60);
	errch_("#", ref, (ftnlen)1, ref_len);
	sigerr_("SPICE(BADFRAME)", (ftnlen)15);
	chkout_("ZZSPKAC1", (ftnlen)8);
	return 0;
    }

/*     Prepare to look up the apparent state of the target */
/*     as seen by the observer. We'll need the geometric */
/*     state of the observer relative to the solar system */
/*     barycenter. If we're using stellar aberration */
/*     corrections, we'll need the observer's acceleration */
/*     as well. */

/*     Get the geometric state of the observer relative to the SSB, */
/*     which we'll call SSBOBS. */

    zzspkgo1_(obs, et, ref, &c__0, ssbobs, &ssblt, ref_len);
    if (usestl) {

/*        Numerically differentiate the observer velocity relative to */
/*        the SSB to obtain acceleration. We first evaluate the */
/*        geometric state of the observer relative to the solar system */
/*        barycenter at ET +/- DELTA. */
	for (i__ = 1; i__ <= 2; ++i__) {
	    t = *et + ((i__ << 1) - 3) * 1.;
	    zzspkgo1_(obs, &t, ref, &c__0, &stobs[(i__1 = i__ * 6 - 6) < 12 &&
		     0 <= i__1 ? i__1 : s_rnge("stobs", i__1, "zzspkac1_", (
		    ftnlen)637)], &ltssb, ref_len);
	}
	qderiv_(&c__3, &stobs[3], &stobs[9], &c_b13, acc);
    } else {
	cleard_(&c__3, acc);
    }

/*     Look up the apparent state. The light time and light */
/*     rate are returned as well. */

    zzspkas1_(targ, et, ref, abcorr, ssbobs, acc, starg, lt, dlt, ref_len, 
	    abcorr_len);
    chkout_("ZZSPKAC1", (ftnlen)8);
    return 0;
} /* zzspkac1_ */

