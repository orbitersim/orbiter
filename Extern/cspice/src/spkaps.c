/* spkaps.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure SPKAPS ( SPK, apparent state ) */
/* Subroutine */ int spkaps_(integer *targ, doublereal *et, char *ref, char *
	abcorr, doublereal *stobs, doublereal *accobs, doublereal *starg, 
	doublereal *lt, doublereal *dlt, ftnlen ref_len, ftnlen abcorr_len)
{
    /* Initialized data */

    static logical first = TRUE_;
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
	    logical *, ftnlen);
    integer refid;
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen);
    doublereal pcorr[3];
    extern logical failed_(void);
    logical attblk[15];
    doublereal dpcorr[3], corvel[3];
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), irfnum_(char *, integer *, ftnlen), setmsg_(char *, 
	    ftnlen), spkltc_(integer *, doublereal *, char *, char *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, ftnlen, 
	    ftnlen);
    doublereal corpos[3];
    extern logical return_(void);
    static logical usestl;

/* $ Abstract */

/*     Return the state (position and velocity) of a target body */
/*     relative to an observer specified by its state and */
/*     acceleration relative to the solar system barycenter. The */
/*     returned state may be optionally corrected for light time */
/*     and stellar aberration. All input and output vectors are */
/*     expressed relative to an inertial reference frame. */

/*     This routine supersedes SPKAPP. */

/*     SPICE users normally should call the high-level API routines */
/*     SPKEZR or SPKEZ rather than this routine. */

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

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     TARG       I   Target body. */
/*     ET         I   Observer epoch. */
/*     REF        I   Inertial reference frame of output state. */
/*     ABCORR     I   Aberration correction flag. */
/*     STOBS      I   State of the observer relative to the SSB. */
/*     ACCOBS     I   Acceleration of the observer relative to the SSB. */
/*     STARG      O   State of target. */
/*     LT         O   One way light time between observer and target. */
/*     DLT        O   Derivative of light time with respect to time. */

/* $ Detailed_Input */

/*     TARG     is the NAIF ID code for a target body. The target */
/*              and observer define a state vector whose position */
/*              component points from the observer to the target. */

/*     ET       is the ephemeris time, expressed as seconds past */
/*              J2000 TDB, at which the state of the target body */
/*              relative to the observer is to be computed.  ET */
/*              refers to time at the observer's location. */

/*     REF      is the inertial reference frame with respect to which */
/*              the input state STOBS, the input acceleration ACCOBS, */
/*              and the output state STARG are expressed. REF must be */
/*              recognized by the SPICE Toolkit. The acceptable */
/*              frames are listed in the Frames Required Reading, as */
/*              well as in the SPICELIB routine CHGIRF. */

/*              Case and blanks are not significant in the string */
/*              REF. */

/*     ABCORR   indicates the aberration corrections to be applied */
/*              to the state of the target body to account for one-way */
/*              light time and stellar aberration. See the discussion */
/*              in the header of SPKEZR for recommendations on */
/*              how to choose aberration corrections. */

/*              ABCORR may be any of the following: */

/*                 'NONE'     Apply no correction. Return the */
/*                            geometric state of the target body */
/*                            relative to the observer. */

/*              The following values of ABCORR apply to the */
/*              "reception" case in which photons depart from the */
/*              target's location at the light-time corrected epoch */
/*              ET-LT and *arrive* at the observer's location at ET: */

/*                 'LT'       Correct for one-way light time (also */
/*                            called "planetary aberration") using a */
/*                            Newtonian formulation. This correction */
/*                            yields the state of the target at the */
/*                            moment it emitted photons arriving at */
/*                            the observer at ET. */

/*                            The light time correction uses an */
/*                            iterative solution of the light time */
/*                            equation (see $Particulars for details). */
/*                            The solution invoked by the 'LT' option */
/*                            uses one iteration. */

/*                 'LT+S'     Correct for one-way light time and */
/*                            stellar aberration using a Newtonian */
/*                            formulation. This option modifies the */
/*                            state obtained with the 'LT' option to */
/*                            account for the observer's velocity */
/*                            relative to the solar system */
/*                            barycenter. The result is the apparent */
/*                            state of the target---the position and */
/*                            velocity of the target as seen by the */
/*                            observer. */

/*                 'CN'       Converged Newtonian light time */
/*                            correction. In solving the light time */
/*                            equation, the 'CN' correction iterates */
/*                            until the solution converges (three */
/*                            iterations on all supported platforms). */
/*                            Whether the 'CN+S' solution is */
/*                            substantially more accurate than the */
/*                            'LT' solution depends on the geometry */
/*                            of the participating objects and on the */
/*                            accuracy of the input data. In all */
/*                            cases this routine will execute more */
/*                            slowly when a converged solution is */
/*                            computed. See the $Particulars section of */
/*                            SPKEZR for a discussion of precision of */
/*                            light time corrections. */

/*                 'CN+S'     Converged Newtonian light time */
/*                            correction and stellar aberration */
/*                            correction. */

/*              The following values of ABCORR apply to the */
/*              "transmission" case in which photons *depart* from */
/*              the observer's location at ET and arrive at the */
/*              target's location at the light-time corrected epoch */
/*              ET+LT: */

/*                 'XLT'      "Transmission" case: correct for */
/*                            one-way light time using a Newtonian */
/*                            formulation. This correction yields the */
/*                            state of the target at the moment it */
/*                            receives photons emitted from the */
/*                            observer's location at ET. */

/*                 'XLT+S'    "Transmission" case: correct for */
/*                            one-way light time and stellar */
/*                            aberration using a Newtonian */
/*                            formulation  This option modifies the */
/*                            state obtained with the 'XLT' option to */
/*                            account for the observer's velocity */
/*                            relative to the solar system */
/*                            barycenter. The position component of */
/*                            the computed target state indicates the */
/*                            direction that photons emitted from the */
/*                            observer's location must be "aimed" to */
/*                            hit the target. */

/*                 'XCN'      "Transmission" case: converged */
/*                            Newtonian light time correction. */

/*                 'XCN+S'    "Transmission" case: converged */
/*                            Newtonian light time correction and */
/*                            stellar aberration correction. */


/*              Neither special nor general relativistic effects are */
/*              accounted for in the aberration corrections applied */
/*              by this routine. */

/*              Case and blanks are not significant in the string */
/*              ABCORR. */


/*     STOBS    is the geometric state of the observer relative to */
/*              the solar system barycenter at ET. STOBS is expressed */
/*              relative to the reference frame designated by REF. */
/*              The target and observer define a state vector whose */
/*              position component points from the observer to the */
/*              target. */

/*     ACCOBS   is the geometric acceleration of the observer */
/*              relative to the solar system barycenter at ET. This */
/*              is the derivative with respect to time of the */
/*              velocity portion of STOBS. ACCOBS is expressed */
/*              relative to the reference frame designated by REF. */

/*              ACCOBS is used for computing stellar aberration */
/*              corrected velocity. If stellar aberration corrections */
/*              are not specified by ABCORR, ACCOBS is ignored; the */
/*              caller need not provide a valid input value in this */
/*              case. */

/* $ Detailed_Output */

/*     STARG    is a Cartesian state vector representing the position */
/*              and velocity of the target body relative to the */
/*              specified observer. STARG is corrected for the */
/*              specified aberrations, and is expressed with respect */
/*              to the inertial reference frame designated by REF. */
/*              The first three components of STARG represent the x-, */
/*              y- and z-components of the target's position; last */
/*              three components form the corresponding velocity */
/*              vector. */

/*              The position component of STARG points from the */
/*              observer's location at ET to the aberration-corrected */
/*              location of the target. Note that the sense of the */
/*              position vector is independent of the direction of */
/*              radiation travel implied by the aberration */
/*              correction. */

/*              Units are always km and km/sec. */

/*     LT       is the one-way light time between the observer and */
/*              target in seconds. If the target state is corrected */
/*              for light time, then LT is the one-way light time */
/*              between the observer and the light time-corrected */
/*              target location. */

/*     DLT      is the derivative with respect to barycentric */
/*              dynamical time of the one way light time between */
/*              target and observer: */

/*                 DLT = d(LT)/d(ET) */

/*              DLT can also be described as the rate of change of */
/*              one way light time. DLT is unitless, since LT and */
/*              ET both have units of TDB seconds. */

/*              If the observer and target are at the same position, */
/*              then DLT is set to zero. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the value of ABCORR is not recognized, an error is signaled */
/*         by a routine in the call tree of this routine. */

/*     2)  If ABCORR calls for stellar aberration but not light */
/*         time corrections, the error SPICE(NOTSUPPORTED) is */
/*         signaled. */

/*     3)  If ABCORR calls for relativistic light time corrections, the */
/*         error SPICE(NOTSUPPORTED) is signaled. */

/*     4)  If the reference frame requested is not a recognized */
/*         inertial reference frame, the error SPICE(BADFRAME) */
/*         is signaled. */

/*     5)  If the state of the target relative to the solar system */
/*         barycenter cannot be computed, an error is signaled by a */
/*         routine in the call tree of this routine. */

/*     6)  If the observer and target are at the same position, */
/*         then DLT is set to zero. This situation could arise, */
/*         for example, when the observer is Mars and the target */
/*         is the Mars barycenter. */

/* $ Files */

/*     This routine computes states using SPK files that have been */
/*     loaded into the SPICE system, normally via the kernel loading */
/*     interface routine FURNSH. Application programs typically load */
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

/*     This routine supports higher-level SPK API routines that can */
/*     perform both light time and stellar aberration corrections. */

/*     User applications normally will not need to call this routine */
/*     directly. However, this routine can improve run-time efficiency */
/*     in situations where many targets are observed from the same */
/*     location at the same time. In such cases, the state and */
/*     acceleration of the observer relative to the solar system */
/*     barycenter need be computed only once per look-up epoch. */

/*     When apparent positions, rather than apparent states, are */
/*     required, consider using the high-level position-only API */
/*     routines */

/*        SPKPOS */
/*        SPKEZP */

/*     or the low-level, position-only analog of this routine */

/*        SPKAPO */

/*     In general, the position-only routines are more efficient. */

/*     See the header of the routine SPKEZR for a detailed discussion */
/*     of aberration corrections. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Look up a sequence of states of the Moon as seen from the */
/*        Earth. Use light time and stellar aberration corrections. */
/*        Compute the first state for the epoch 2000 JAN 1 12:00:00 TDB; */
/*        compute subsequent states at intervals of 1 hour. For each */
/*        epoch, display the states, the one way light time between */
/*        target and observer, and the rate of change of the one way */
/*        light time. */

/*        Use the meta-kernel shown below to load the required SPICE */
/*        kernels. */


/*           KPL/MK */

/*           File name: spkaps_ex1.tm */

/*           This meta-kernel is intended to support operation of SPICE */
/*           example programs. The kernels shown here should not be */
/*           assumed to contain adequate or correct versions of data */
/*           required by SPICE-based user applications. */

/*           In order for an application to use this meta-kernel, the */
/*           kernels referenced here must be present in the user's */
/*           current working directory. */

/*           The names and contents of the kernels referenced */
/*           by this meta-kernel are as follows: */

/*              File name                     Contents */
/*              ---------                     -------- */
/*              de418.bsp                     Planetary ephemeris */
/*              pck00008.tpc                  Planet orientation and */
/*                                            radii */
/*              naif0008.tls                  Leapseconds */


/*           \begindata */

/*              KERNELS_TO_LOAD = ( 'de418.bsp', */
/*                                  'pck00008.tpc', */
/*                                  'naif0008.tls'  ) */

/*           \begintext */

/*           End of meta-kernel */


/*        Example code begins here. */


/*              PROGRAM SPKAPS_EX1 */
/*              IMPLICIT NONE */
/*        C */
/*        C     Local constants */
/*        C */
/*        C     The meta-kernel name shown here refers to a file whose */
/*        C     contents are those shown above. This file and the kernels */
/*        C     it references must exist in your current working */
/*        C     directory. */
/*        C */
/*              CHARACTER*(*)         META */
/*              PARAMETER           ( META   = 'spkaps_ex1.tm' ) */
/*        C */
/*        C     Use a time step of 1 hour; look up 5 states. */
/*        C */
/*              DOUBLE PRECISION      STEP */
/*              PARAMETER           ( STEP   = 3600.0D0 ) */

/*              INTEGER               MAXITR */
/*              PARAMETER           ( MAXITR = 5 ) */
/*        C */
/*        C     Local variables */
/*        C */
/*              DOUBLE PRECISION      ACC    ( 3 ) */
/*              DOUBLE PRECISION      DLT */
/*              DOUBLE PRECISION      ET */
/*              DOUBLE PRECISION      ET0 */
/*              DOUBLE PRECISION      LT */
/*              DOUBLE PRECISION      STATE  ( 6 ) */
/*              DOUBLE PRECISION      STATE0 ( 6 ) */
/*              DOUBLE PRECISION      STATE2 ( 6 ) */
/*              DOUBLE PRECISION      STOBS  ( 6 ) */
/*              DOUBLE PRECISION      TDELTA */
/*              INTEGER               I */

/*        C */
/*        C     Load the SPK and LSK kernels via the meta-kernel. */
/*        C */
/*              CALL FURNSH ( META ) */
/*        C */
/*        C     Convert the start time to seconds past J2000 TDB. */
/*        C */
/*              CALL STR2ET ( '2000 JAN 1 12:00:00 TDB', ET0 ) */
/*        C */
/*        C     Step through a series of epochs, looking up a */
/*        C     state vector at each one. */
/*        C */
/*              DO I = 1, MAXITR */

/*                 ET = ET0 + (I-1)*STEP */

/*        C */
/*        C        Look up a state vector at epoch ET using the */
/*        C        following inputs: */
/*        C */
/*        C           Target:                 Moon (NAIF ID code 301) */
/*        C           Reference frame:        J2000 */
/*        C           Aberration correction:  Light time and stellar */
/*        C                                   aberration ('LT+S') */
/*        C           Observer:               Earth (NAIF ID code 399) */
/*        C */
/*        C        Before we can execute this computation, we'll need the */
/*        C        geometric state and acceleration of the observer */
/*        C        relative to the solar system barycenter at ET, */
/*        C        expressed relative to the J2000 reference frame. First */
/*        C        find the state: */
/*        C */
/*                 CALL SPKSSB ( 399, ET, 'J2000', STOBS ) */
/*        C */
/*        C        Next compute the acceleration. We numerically */
/*        C        differentiate the velocity using a quadratic */
/*        C        approximation: */
/*        C */
/*                 TDELTA = 1.D0 */

/*                 CALL SPKSSB ( 399, ET-TDELTA, 'J2000', STATE0 ) */
/*                 CALL SPKSSB ( 399, ET+TDELTA, 'J2000', STATE2 ) */

/*                 CALL QDERIV ( 3, STATE0(4), STATE2(4), TDELTA, ACC ) */
/*        C */
/*        C        Now compute the desired state vector: */
/*        C */
/*                 CALL SPKAPS ( 301,   ET,  'J2000', 'LT+S', */
/*             .                 STOBS, ACC, STATE,    LT,   DLT ) */

/*                 WRITE (*,*) 'ET = ', ET */
/*                 WRITE (*,*) 'J2000 x-position (km):   ', STATE(1) */
/*                 WRITE (*,*) 'J2000 y-position (km):   ', STATE(2) */
/*                 WRITE (*,*) 'J2000 z-position (km):   ', STATE(3) */
/*                 WRITE (*,*) 'J2000 x-velocity (km/s): ', STATE(4) */
/*                 WRITE (*,*) 'J2000 y-velocity (km/s): ', STATE(5) */
/*                 WRITE (*,*) 'J2000 z-velocity (km/s): ', STATE(6) */
/*                 WRITE (*,*) 'One-way light time (s):  ', LT */
/*                 WRITE (*,*) 'Light time rate:         ', DLT */
/*                 WRITE (*,*) ' ' */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         ET =    0.0000000000000000 */
/*         J2000 x-position (km):     -291584.61369497533 */
/*         J2000 y-position (km):     -266693.40583162551 */
/*         J2000 z-position (km):     -76095.653209237149 */
/*         J2000 x-velocity (km/s):   0.64343915743508395 */
/*         J2000 y-velocity (km/s):  -0.66606587365741410 */
/*         J2000 z-velocity (km/s):  -0.30131006342946742 */
/*         One-way light time (s):     1.3423106103251679 */
/*         Light time rate:            1.0731690869897750E-007 */

/*         ET =    3600.0000000000000 */
/*         J2000 x-position (km):     -289256.45942322229 */
/*         J2000 y-position (km):     -269080.60545907740 */
/*         J2000 z-position (km):     -77177.352771302132 */
/*         J2000 x-velocity (km/s):   0.64997032016926526 */
/*         J2000 y-velocity (km/s):  -0.66014825329341664 */
/*         J2000 z-velocity (km/s):  -0.29963041790724715 */
/*         One-way light time (s):     1.3426939548635302 */
/*         Light time rate:            1.0565259895222426E-007 */

/*         ET =    7200.0000000000000 */
/*         J2000 x-position (km):     -286904.89654239739 */
/*         J2000 y-position (km):     -271446.41676468350 */
/*         J2000 z-position (km):     -78252.965533623050 */
/*         J2000 x-velocity (km/s):   0.65644388315539315 */
/*         J2000 y-velocity (km/s):  -0.65418355204586442 */
/*         J2000 z-velocity (km/s):  -0.29792853294482308 */
/*         One-way light time (s):     1.3430713117337547 */
/*         Light time rate:            1.0399045689875861E-007 */

/*         ET =    10800.000000000000 */
/*         J2000 x-position (km):     -284530.13302756584 */
/*         J2000 y-position (km):     -273790.67111559171 */
/*         J2000 z-position (km):     -79322.411703917489 */
/*         J2000 x-velocity (km/s):   0.66285950473048116 */
/*         J2000 y-velocity (km/s):  -0.64817224685146524 */
/*         J2000 z-velocity (km/s):  -0.29620455846903732 */
/*         One-way light time (s):     1.3434426890693671 */
/*         Light time rate:            1.0233066524342374E-007 */

/*         ET =    14400.000000000000 */
/*         J2000 x-position (km):     -282132.37807791750 */
/*         J2000 y-position (km):     -276113.20159697317 */
/*         J2000 z-position (km):     -80385.612030562901 */
/*         J2000 x-velocity (km/s):   0.66921684649247459 */
/*         J2000 y-velocity (km/s):  -0.64211481528028158 */
/*         J2000 z-velocity (km/s):  -0.29445864490384888 */
/*         One-way light time (s):     1.3438080956559786 */
/*         Light time rate:            1.0067340363005083E-007 */


/* $ Restrictions */

/*     1)  This routine should not be used to compute geometric states. */
/*         Instead, use SPKEZR, SPKEZ, or SPKGEO. SPKGEO, which is called */
/*         by SPKEZR and SPKEZ, introduces less round-off error when the */
/*         observer and target have a common center that is closer to */
/*         both objects than is the solar system barycenter. */

/*     2)  The kernel files to be used by SPKAPS must be loaded */
/*         (normally by the SPICELIB kernel loader FURNSH) before */
/*         this routine is called. */

/*     3)  Unlike most other SPK state computation routines, this */
/*         routine requires that the output state be relative to an */
/*         inertial reference frame. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */

/* $ Version */

/* -    SPICELIB Version 1.2.0, 26-OCT-2021 (JDR) (NJB) */

/*        Edited the header to comply with NAIF standard. Updated */
/*        example's meta-kernel. */

/*        Bug fix: ABCORR now is parsed using ZZVALCOR. This improves */
/*        error checking. Removed unnecessary in-line error checking */
/*        now provided by ZZVALCOR. */

/* -    SPICELIB Version 1.1.0, 04-JUL-2014 (NJB) */

/*        Discussion of light time corrections was updated. Assertions */
/*        that converged light time corrections are unlikely to be */
/*        useful were removed. */

/*     Last update was 15-APR-2014 (NJB) */

/*        Added a FAILED() call to prevent numeric problems */
/*        resulting from uninitialized values. */

/* -    SPICELIB Version 1.0.0, 11-JAN-2008 (NJB) */

/* -& */
/* $ Index_Entries */

/*     low-level aberration-corrected state computation */
/*     low-level light time and stellar aberration correction */

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
    chkin_("SPKAPS", (ftnlen)6);
    if (first || s_cmp(abcorr, prvcor, abcorr_len, (ftnlen)5) != 0) {

/*        The aberration correction flag differs from the value it */
/*        had on the previous call, if any.  Analyze the new flag. */

	zzvalcor_(abcorr, attblk, abcorr_len);
	if (failed_()) {
	    chkout_("SPKAPS", (ftnlen)6);
	    return 0;
	}

/*        The aberration correction flag is recognized; save it. */

	s_copy(prvcor, abcorr, (ftnlen)5, abcorr_len);

/*        Set logical flags indicating the attributes of the requested */
/*        correction: */

/*           XMIT is .TRUE. when the correction is for transmitted */
/*           radiation. */

/*           USECN indicates converged Newtonian light time correction. */

/*        The above definitions are consistent with those used by */
/*        ZZPRSCOR. */

	xmit = attblk[4];
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
	chkout_("SPKAPS", (ftnlen)6);
	return 0;
    }

/*     Get the state of the target relative to the observer, */
/*     optionally corrected for light time. */

    spkltc_(targ, et, ref, abcorr, stobs, starg, lt, dlt, ref_len, abcorr_len)
	    ;
    if (failed_()) {
	chkout_("SPKAPS", (ftnlen)6);
	return 0;
    }

/*     If stellar aberration corrections are not needed, we're */
/*     already done. */

    if (! usestl) {
	chkout_("SPKAPS", (ftnlen)6);
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
    chkout_("SPKAPS", (ftnlen)6);
    return 0;
} /* spkaps_ */

