/* zzspkfzt.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__6 = 6;

/* $Procedure ZZSPKFZT ( SPK function, easy reader, target ) */
/* Subroutine */ int zzspkfzt_(S_fp trgsub, doublereal *et, char *ref, char *
	abcorr, integer *obs, doublereal *starg, doublereal *lt, ftnlen 
	ref_len, ftnlen abcorr_len)
{
    /* Initialized data */

    static logical first = TRUE_;
    static char prvcor[5] = "     ";

    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static integer fj2000, type__;
    static logical xmit;
    extern /* Subroutine */ int mxvg_(doublereal *, doublereal *, integer *, 
	    integer *, doublereal *), zzvalcor_(char *, logical *, ftnlen), 
	    zzspkfat_(S_fp, doublereal *, char *, char *, integer *, 
	    doublereal *, doublereal *, doublereal *, ftnlen, ftnlen), 
	    zzcorsxf_(logical *, doublereal *, doublereal *, doublereal *), 
	    vaddg_(doublereal *, doublereal *, integer *, doublereal *), 
	    chkin_(char *, ftnlen), errch_(char *, char *, ftnlen, ftnlen);
    static logical found;
    static doublereal state[6], stemp[6], stobs[6], xform[36]	/* was [6][6] 
	    */;
    extern doublereal vnorm_(doublereal *);
    extern logical failed_(void);
    extern /* Subroutine */ int frmchg_(integer *, integer *, doublereal *, 
	    doublereal *);
    extern doublereal clight_(void);
    static integer center;
    static logical attblk[15];
    extern /* Subroutine */ int namfrm_(char *, integer *, ftnlen), frinfo_(
	    integer *, integer *, integer *, integer *, logical *);
    static logical usegeo;
    static doublereal ltcent;
    extern /* Subroutine */ int spkgeo_(integer *, doublereal *, char *, 
	    integer *, doublereal *, doublereal *, ftnlen);
    static doublereal dltctr, corxfm[36]	/* was [6][6] */;
    static integer reqfrm, ltsign, typeid;
    extern /* Subroutine */ int chkout_(char *, ftnlen), setmsg_(char *, 
	    ftnlen), sigerr_(char *, ftnlen);
    static integer trgctr;
    extern /* Subroutine */ int spkssb_(integer *, doublereal *, char *, 
	    doublereal *, ftnlen), spkltc_(integer *, doublereal *, char *, 
	    char *, doublereal *, doublereal *, doublereal *, doublereal *, 
	    ftnlen, ftnlen);
    extern logical return_(void);
    static doublereal sttctr[6], dlt;

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Return the state (position and velocity) of a target body */
/*     relative to an observing body, optionally corrected for light */
/*     time (planetary aberration) and stellar aberration. An input */
/*     subroutine provides the state of the target relative to its */
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
/*     NAIF_IDS */
/*     FRAMES */
/*     TIME */

/* $ Keywords */

/*     EPHEMERIS */

/* $ Declarations */
/* $ Abstract */

/*     The parameters below form an enumerated list of the recognized */
/*     frame types. They are: INERTL, PCK, CK, TK, DYN, SWTCH, and ALL. */
/*     The meanings are outlined below. */

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

/*     INERTL      an inertial frame that is listed in the routine */
/*                 CHGIRF and that requires no external file to */
/*                 compute the transformation from or to any other */
/*                 inertial frame. */

/*     PCK         is a frame that is specified relative to some */
/*                 INERTL frame and that has an IAU model that */
/*                 may be retrieved from the PCK system via a call */
/*                 to the routine TISBOD. */

/*     CK          is a frame defined by a C-kernel. */

/*     TK          is a "text kernel" frame.  These frames are offset */
/*                 from their associated "relative" frames by a */
/*                 constant rotation. */

/*     DYN         is a "dynamic" frame.  These currently are */
/*                 parameterized, built-in frames where the full frame */
/*                 definition depends on parameters supplied via a */
/*                 frame kernel. */

/*     SWTCH       is a "switch" frame. These frames have orientation */
/*                 defined by their alignment with base frames selected */
/*                 from a prioritized list. The base frames optionally */
/*                 have associated time intervals of applicability. */

/*     ALL         indicates any of the above classes. This parameter */
/*                 is used in APIs that fetch information about frames */
/*                 of a specified class. */


/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */
/*     B.V. Semenov    (JPL) */
/*     W.L. Taber      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 5.0.0, 08-OCT-2020 (NJB) (BVS) */

/*       The parameter SWTCH was added to support the switch */
/*       frame class. */

/* -    SPICELIB Version 4.0.0, 08-MAY-2012 (NJB) */

/*       The parameter ALL was added to support frame fetch APIs. */

/* -    SPICELIB Version 3.0.0, 28-MAY-2004 (NJB) */

/*       The parameter DYN was added to support the dynamic frame class. */

/* -    SPICELIB Version 2.0.0, 12-DEC-1996 (WLT) */

/*        Various unused frames types were removed and the */
/*        frame time TK was added. */

/* -    SPICELIB Version 1.0.0, 10-DEC-1995 (WLT) */

/* -& */

/*     End of INCLUDE file frmtyp.inc */

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
/*     REF        I   Reference frame of output state vector. */
/*     ABCORR     I   Aberration correction flag. */
/*     OBS        I   Observing body. */
/*     STARG      O   State of target. */
/*     LT         O   One way light time between observer and target. */

/* $ Detailed_Input */

/*     TRGSUB      is the name of an external subroutine that returns */
/*                 the geometric state of the target body relative to a */
/*                 center of motion, expressed in the inertial reference */
/*                 frame REF, at the epoch ET. */

/*                 The center of motion can be any ephemeris object; it */
/*                 need not be a gravity source. */

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

/*     ET          is the ephemeris time, expressed as seconds past J2000 */
/*                 TDB, at which the state of the target body relative to */
/*                 the observer is to be computed. ET refers to time at */
/*                 the observer's location. */

/*     REF         is the name of the reference frame relative to which */
/*                 the output state vector should be expressed. This may */
/*                 be any frame supported by the SPICE system, including */
/*                 built-in frames (documented in the Frames Required */
/*                 Reading) and frames defined by a loaded frame kernel */
/*                 (FK). */

/*                 When REF designates a non-inertial frame, the */
/*                 orientation of the frame is evaluated at an epoch */
/*                 dependent on the selected aberration correction. */
/*                 See the description of the output state vector STARG */
/*                 for details. */

/*     ABCORR      indicates the aberration corrections to be applied */
/*                 to the state of the target body to account for one-way */
/*                 light time and stellar aberration. See the discussion */
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
/*                               computed. See the Particulars section */
/*                               below for a discussion of precision of */
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

/*     OBS         is the NAIF ID code for an observing body. */


/* $ Detailed_Output */

/*     STARG       is a Cartesian state vector representing the position */
/*                 and velocity of the target body relative to the */
/*                 specified observer. STARG is corrected for the */
/*                 specified aberrations, and is expressed with respect */
/*                 to the reference frame specified by REF. The first */
/*                 three components of STARG represent the x-, y- and */
/*                 z-components of the target's position; the last three */
/*                 components form the corresponding velocity vector. */

/*                 The position component of STARG points from the */
/*                 observer's location at ET to the aberration-corrected */
/*                 location of the target. Note that the sense of the */
/*                 position vector is independent of the direction of */
/*                 radiation travel implied by the aberration */
/*                 correction. */

/*                 The velocity component of STARG is the derivative */
/*                 with respect to time of the position component of */
/*                 STARG. */

/*                 Units are always km and km/sec. */

/*                 Non-inertial frames are treated as follows: letting */
/*                 LTCENT be the one-way light time between the observer */
/*                 and the central body associated with the frame, the */
/*                 orientation of the frame is evaluated at ET-LTCENT, */
/*                 ET+LTCENT, or ET depending on whether the requested */
/*                 aberration correction is, respectively, for received */
/*                 radiation, transmitted radiation, or is omitted. */
/*                 LTCENT is computed using the method indicated by */
/*                 ABCORR. */

/*     LT          is the one-way light time between the observer and */
/*                 target in seconds. If the target state is corrected */
/*                 for aberrations, then LT is the one-way light time */
/*                 between the observer and the light time corrected */
/*                 target location. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If the reference frame REF is not a recognized reference */
/*        frame the error 'SPICE(UNKNOWNFRAME)' is signaled. */

/*     2) If an invalid aberration correction is specified, the */
/*        error will be diagnosed by a routine in the call tree */
/*        of this routine. */

/*     3) If the loaded kernels provide insufficient data to */
/*        compute the requested state vector, the deficiency will */
/*        be diagnosed by a routine in the call tree of this routine. */

/*     4) If an error occurs while reading an SPK or other kernel file, */
/*        the error will be diagnosed by a routine in the call tree */
/*        of this routine. */

/* $ Files */

/*     This routine computes states using SPK files that have been */
/*     loaded into the SPICE system, normally via the kernel loading */
/*     interface routine FURNSH. See the routine FURNSH and the SPK */
/*     and KERNEL Required Reading for further information on loading */
/*     (and unloading) kernels. */

/*     If the output state STARG is to be expressed relative to a */
/*     non-inertial frame, or if any of the ephemeris data used to */
/*     compute STARG are expressed relative to a non-inertial frame in */
/*     the SPK files providing those data, additional kernels may be */
/*     needed to enable the reference frame transformations required to */
/*     compute the state. Normally these additional kernels are PCK */
/*     files or frame kernels. Any such kernels must already be loaded */
/*     at the time this routine is called. */

/* $ Particulars */

/*     This routine is part of the user interface to the SPICE ephemeris */
/*     system. It allows you to retrieve state information for any */
/*     ephemeris object relative to any other in a reference frame that */
/*     is convenient for further computations. */


/*     Aberration corrections */
/*     ====================== */

/*     In space science or engineering applications one frequently */
/*     wishes to know where to point a remote sensing instrument, such */
/*     as an optical camera or radio antenna, in order to observe or */
/*     otherwise receive radiation from a target. This pointing problem */
/*     is complicated by the finite speed of light:  one needs to point */
/*     to where the target appears to be as opposed to where it actually */
/*     is at the epoch of observation. We use the adjectives */
/*     "geometric," "uncorrected," or "true" to refer to an actual */
/*     position or state of a target at a specified epoch. When a */
/*     geometric position or state vector is modified to reflect how it */
/*     appears to an observer, we describe that vector by any of the */
/*     terms "apparent," "corrected," "aberration corrected," or "light */
/*     time and stellar aberration corrected." The SPICE Toolkit can */
/*     correct for two phenomena affecting the apparent location of an */
/*     object:  one-way light time (also called "planetary aberration") */
/*     and stellar aberration. */

/*     One-way light time */
/*     ------------------ */

/*     Correcting for one-way light time is done by computing, given an */
/*     observer and observation epoch, where a target was when the */
/*     observed photons departed the target's location. The vector from */
/*     the observer to this computed target location is called a "light */
/*     time corrected" vector. The light time correction depends on the */
/*     motion of the target relative to the solar system barycenter, but */
/*     it is independent of the velocity of the observer relative to the */
/*     solar system barycenter. Relativistic effects such as light */
/*     bending and gravitational delay are not accounted for in the */
/*     light time correction performed by this routine. */

/*     Stellar aberration */
/*     ------------------ */

/*     The velocity of the observer also affects the apparent location */
/*     of a target:  photons arriving at the observer are subject to a */
/*     "raindrop effect" whereby their velocity relative to the observer */
/*     is, using a Newtonian approximation, the photons' velocity */
/*     relative to the solar system barycenter minus the velocity of the */
/*     observer relative to the solar system barycenter. This effect is */
/*     called "stellar aberration."  Stellar aberration is independent */
/*     of the velocity of the target. The stellar aberration formula */
/*     used by this routine does not include (the much smaller) */
/*     relativistic effects. */

/*     Stellar aberration corrections are applied after light time */
/*     corrections:  the light time corrected target position vector is */
/*     used as an input to the stellar aberration correction. */

/*     When light time and stellar aberration corrections are both */
/*     applied to a geometric position vector, the resulting position */
/*     vector indicates where the target "appears to be" from the */
/*     observer's location. */

/*     As opposed to computing the apparent position of a target, one */
/*     may wish to compute the pointing direction required for */
/*     transmission of photons to the target. This also requires */
/*     correction of the geometric target position for the effects of */
/*     light time and stellar aberration, but in this case the */
/*     corrections are computed for radiation traveling *from* the */
/*     observer to the target. */

/*     The "transmission" light time correction yields the target's */
/*     location as it will be when photons emitted from the observer's */
/*     location at ET arrive at the target. The transmission stellar */
/*     aberration correction is the inverse of the traditional stellar */
/*     aberration correction:  it indicates the direction in which */
/*     radiation should be emitted so that, using a Newtonian */
/*     approximation, the sum of the velocity of the radiation relative */
/*     to the observer and of the observer's velocity, relative to the */
/*     solar system barycenter, yields a velocity vector that points in */
/*     the direction of the light time corrected position of the target. */

/*     One may object to using the term "observer" in the transmission */
/*     case, in which radiation is emitted from the observer's location. */
/*     The terminology was retained for consistency with earlier */
/*     documentation. */

/*     Below, we indicate the aberration corrections to use for some */
/*     common applications: */

/*        1) Find the apparent direction of a target for a remote-sensing */
/*           observation. */

/*              Use 'LT+S' or 'CN+S: apply both light time and stellar */
/*              aberration corrections. */

/*           Note that using light time corrections alone ('LT' or 'CN') */
/*           is generally not a good way to obtain an approximation to */
/*           an apparent target vector: since light time and stellar */
/*           aberration corrections often partially cancel each other, */
/*           it may be more accurate to use no correction at all than to */
/*           use light time alone. */


/*        2) Find the corrected pointing direction to radiate a signal */
/*           to a target. This computation is often applicable for */
/*           implementing communications sessions. */

/*              Use 'XLT+S' or 'XCN+S: apply both light time and stellar */
/*              aberration corrections for transmission. */


/*        3) Compute the apparent position of a target body relative */
/*           to a star or other distant object. */

/*              Use 'LT', 'CN', 'LT+S', or 'CN+S' as needed to match the */
/*              correction applied to the position of the distant */
/*              object. For example, if a star position is obtained from */
/*              a catalog, the position vector may not be corrected for */
/*              stellar aberration. In this case, to find the angular */
/*              separation of the star and the limb of a planet, the */
/*              vector from the observer to the planet should be */
/*              corrected for light time but not stellar aberration. */


/*        4) Obtain an uncorrected state vector derived directly from */
/*           data in an SPK file. */

/*              Use 'NONE'. */


/*        5) Use a geometric state vector as a low-accuracy estimate */
/*           of the apparent state for an application where execution */
/*           speed is critical. */

/*              Use 'NONE'. */


/*        6) While this routine cannot perform the relativistic */
/*           aberration corrections required to compute states */
/*           with the highest possible accuracy, it can supply the */
/*           geometric states required as inputs to these computations. */

/*              Use 'NONE', then apply relativistic aberration */
/*              corrections (not available in the SPICE Toolkit). */


/*     Below, we discuss in more detail how the aberration corrections */
/*     applied by this routine are computed. */

/*        Geometric case */
/*        ============== */

/*        ZZSPKFZT begins by computing the geometric position T(ET) of */
/*        the target body relative to the solar system barycenter (SSB). */
/*        Subtracting the geometric position of the observer O(ET) gives */
/*        the geometric position of the target body relative to the */
/*        observer. The one-way light time, LT, is given by */

/*                  | T(ET) - O(ET) | */
/*           LT = ------------------- */
/*                          c */

/*        The geometric relationship between the observer, target, and */
/*        solar system barycenter is as shown: */


/*           SSB ---> O(ET) */
/*            |      / */
/*            |     / */
/*            |    / */
/*            |   /  T(ET) - O(ET) */
/*            V  V */
/*           T(ET) */


/*        The returned state consists of the position vector */

/*           T(ET) - O(ET) */

/*        and a velocity obtained by taking the difference of the */
/*        corresponding velocities. In the geometric case, the */
/*        returned velocity is actually the time derivative of the */
/*        position. */


/*        Reception case */
/*        ============== */

/*        When any of the options 'LT', 'CN', 'LT+S', 'CN+S' is selected */
/*        for ABCORR, ZZSPKFZT computes the position of the target body */
/*        at epoch ET-LT, where LT is the one-way light time. Let T(t) */
/*        and O(t) represent the positions of the target and observer */
/*        relative to the solar system barycenter at time t; then LT is */
/*        the solution of the light-time equation */

/*                  | T(ET-LT) - O(ET) | */
/*           LT = ------------------------                            (1) */
/*                           c */

/*        The ratio */

/*            | T(ET) - O(ET) | */
/*          ---------------------                                     (2) */
/*                    c */

/*        is used as a first approximation to LT; inserting (2) into the */
/*        right hand side of the light-time equation (1) yields the */
/*        "one-iteration" estimate of the one-way light time ("LT"). */
/*        Repeating the process until the estimates of LT converge */
/*        yields the "converged Newtonian" light time estimate ("CN"). */

/*        Subtracting the geometric position of the observer O(ET) gives */
/*        the position of the target body relative to the observer: */
/*        T(ET-LT) - O(ET). */

/*           SSB ---> O(ET) */
/*            | \     | */
/*            |  \    | */
/*            |   \   | T(ET-LT) - O(ET) */
/*            |    \  | */
/*            V     V V */
/*           T(ET)  T(ET-LT) */

/*        The position component of the light time corrected state */
/*        is the vector */

/*           T(ET-LT) - O(ET) */

/*        The velocity component of the light time corrected state */
/*        is the difference */

/*           T_vel(ET-LT)*(1-dLT/dET) - O_vel(ET) */

/*        where T_vel and O_vel are, respectively, the velocities of the */
/*        target and observer relative to the solar system barycenter at */
/*        the epochs ET-LT and ET. */

/*        If correction for stellar aberration is requested, the target */
/*        position is rotated toward the solar system barycenter- */
/*        relative velocity vector of the observer. The rotation is */
/*        computed as follows: */

/*           Let r be the light time corrected vector from the observer */
/*           to the object, and v be the velocity of the observer with */
/*           respect to the solar system barycenter. Let w be the angle */
/*           between them. The aberration angle phi is given by */

/*              sin(phi) = v sin(w) / c */

/*           Let h be the vector given by the cross product */

/*              h = r X v */

/*           Rotate r by phi radians about h to obtain the apparent */
/*           position of the object. */

/*        When stellar aberration corrections are used, the rate of */
/*        change of the stellar aberration correction is accounted for */
/*        in the computation of the output velocity. */


/*        Transmission case */
/*        ================== */

/*        When any of the options 'XLT', 'XCN', 'XLT+S', 'XCN+S' is */
/*        selected, ZZSPKFZT computes the position of the target body T */
/*        at epoch ET+LT, where LT is the one-way light time. LT is the */
/*        solution of the light-time equation */

/*                  | T(ET+LT) - O(ET) | */
/*           LT = ------------------------                            (3) */
/*                            c */

/*        Subtracting the geometric position of the observer, O(ET), */
/*        gives the position of the target body relative to the */
/*        observer: T(ET-LT) - O(ET). */

/*                   SSB --> O(ET) */
/*                  / |    * */
/*                 /  |  *  T(ET+LT) - O(ET) */
/*                /   |* */
/*               /   *| */
/*              V  V  V */
/*          T(ET+LT)  T(ET) */

/*        The position component of the light-time corrected state */
/*        is the vector */

/*           T(ET+LT) - O(ET) */

/*        The velocity component of the light-time corrected state */
/*        consists of the difference */

/*           T_vel(ET+LT)*(1+dLT/dET) - O_vel(ET) */

/*        where T_vel and O_vel are, respectively, the velocities of the */
/*        target and observer relative to the solar system barycenter at */
/*        the epochs ET+LT and ET. */

/*        If correction for stellar aberration is requested, the target */
/*        position is rotated away from the solar system barycenter- */
/*        relative velocity vector of the observer. The rotation is */
/*        computed as in the reception case, but the sign of the */
/*        rotation angle is negated. Velocities are adjusted to account */
/*        for the rate of change of the stellar aberration correction. */


/*     Precision of light time corrections */
/*     =================================== */

/*        Corrections using one iteration of the light time solution */
/*        ---------------------------------------------------------- */

/*        When the requested aberration correction is 'LT', 'LT+S', */
/*        'XLT', or 'XLT+S', only one iteration is performed in the */
/*        algorithm used to compute LT. */

/*        The relative error in this computation */

/*           | LT_ACTUAL - LT_COMPUTED |  /  LT_ACTUAL */

/*        is at most */

/*            (V/C)**2 */
/*           ---------- */
/*            1 - (V/C) */

/*        which is well approximated by (V/C)**2, where V is the */
/*        velocity of the target relative to an inertial frame and C is */
/*        the speed of light. */

/*        For nearly all objects in the solar system V is less than 60 */
/*        km/sec. The value of C is ~300000 km/sec. Thus the */
/*        one-iteration solution for LT has a potential relative error */
/*        of not more than 4e-8. This is a potential light time error of */
/*        approximately 2e-5 seconds per astronomical unit of distance */
/*        separating the observer and target. Given the bound on V cited */
/*        above: */

/*           As long as the observer and target are separated by less */
/*           than 50 astronomical units, the error in the light time */
/*           returned using the one-iteration light time corrections is */
/*           less than 1 millisecond. */

/*           The magnitude of the corresponding position error, given */
/*           the above assumptions, may be as large as (V/C)**2 * the */
/*           distance between the observer and the uncorrected target */
/*           position: 300 km or equivalently 6 km/AU. */

/*        In practice, the difference between positions obtained using */
/*        one-iteration and converged light time is usually much smaller */
/*        than the value computed above and can be insignificant. For */
/*        example, for the spacecraft Mars Reconnaissance Orbiter and */
/*        Mars Express, the position error for the one-iteration light */
/*        time correction, applied to the spacecraft-to-Mars center */
/*        vector, is at the 1 cm level. */

/*        Comparison of results obtained using the one-iteration and */
/*        converged light time solutions is recommended when adequacy of */
/*        the one-iteration solution is in doubt. */


/*        Converged corrections */
/*        --------------------- */

/*        When the requested aberration correction is 'CN', 'CN+S', */
/*        'XCN', or 'XCN+S', as many iterations as are required for */
/*        convergence are performed in the computation of LT. Usually */
/*        the solution is found after three iterations. The relative */
/*        error present in this case is at most */

/*            (V/C)**4 */
/*           ---------- */
/*            1 - (V/C) */

/*        which is well approximated by (V/C)**4. */

/*           The precision of this computation (ignoring round-off */
/*           error) is better than 4e-11 seconds for any pair of objects */
/*           less than 50 AU apart, and having speed relative to the */
/*           solar system barycenter less than 60 km/s. */

/*           The magnitude of the corresponding position error, given */
/*           the above assumptions, may be as large as (V/C)**4 * the */
/*           distance between the observer and the uncorrected target */
/*           position: 1.2 cm at 50 AU or equivalently 0.24 mm/AU. */

/*        However, to very accurately model the light time between */
/*        target and observer one must take into account effects due to */
/*        general relativity. These may be as high as a few hundredths */
/*        of a millisecond for some objects. */

/*     Relativistic Corrections */
/*     ========================= */

/*     This routine does not attempt to perform either general or */
/*     special relativistic corrections in computing the various */
/*     aberration corrections. For many applications relativistic */
/*     corrections are not worth the expense of added computation */
/*     cycles. If however, your application requires these additional */
/*     corrections we suggest you consult the astronomical almanac (page */
/*     B36) for a discussion of how to carry out these corrections. */


/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     1) This routine must not be called by routines of the SPICE */
/*        frame or SPK subsystems. */

/*     2) The input subroutine TRGSUB must not call this routine */
/*        or any of the supporting, private SPK routines */

/*           ZZSPKFAO */
/*           ZZSPKFAP */
/*           ZZSPKFAT */
/*           ZZSPKFLT */
/* C */
/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     C.H. Acton      (JPL) */
/*     W.L. Taber      (JPL) */
/*     N.J. Bachman    (JPL) */
/*     I.M. Underwood  (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 04-JUL-2014 (CHA) (NJB) (WLT) (IMU) */

/*        Discussion of light time corrections was updated. Assertions */
/*        that converged light time corrections are unlikely to be */
/*        useful were removed. */

/*     Last update was 22-FEB-2012 (CHA) (NJB) (WLT) (IMU) */

/* -& */
/* $ Index_Entries */

/*     target function easy reader for spk file */

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
	chkin_("ZZSPKFZT", (ftnlen)8);
    }
    if (first || s_cmp(abcorr, prvcor, abcorr_len, (ftnlen)5) != 0) {

/*        The aberration correction flag differs from the value it */
/*        had on the previous call, if any. Analyze the new flag. */

	zzvalcor_(abcorr, attblk, abcorr_len);
	if (failed_()) {
	    chkout_("ZZSPKFZT", (ftnlen)8);
	    return 0;
	}

/*        The aberration correction flag is recognized; save it. */

	s_copy(prvcor, abcorr, (ftnlen)5, abcorr_len);

/*        Set logical flags indicating the attributes of the requested */
/*        correction: */

/*           XMIT is .TRUE. when the correction is for transmitted */
/*           radiation. */

/*           USEGEO indicates geometric state computation. */

/*        The above definitions are consistent with those used by */
/*        ZZVALCOR. */

	xmit = attblk[4];
	usegeo = attblk[0];

/*        Get the frame ID for J2000 on the first call to this routine. */

	if (first) {
	    namfrm_("J2000", &fj2000, (ftnlen)5);
	    first = FALSE_;
	}
    }

/*     If we only want a geometric state, then use SPKGEO to compute */
/*     just that. */

/*     Otherwise, if REF is inertial, compute the state of the target */
/*     relative to the observer via SPKACS. If REF is non-inertial, */
/*     compute the requested state in the J2000 frame, then transform it */
/*     to the frame designated by REF. */

    if (usegeo) {

/*        Get the state of the target relative to its center at ET. */
/*        Add the state of the target's center to obtain the state */
/*        of the target with respect to the observer. */

	(*trgsub)(et, ref, &trgctr, sttctr, ref_len);
	spkgeo_(&trgctr, et, ref, obs, stemp, &ltcent, ref_len);
	if (failed_()) {
	    chkout_("ZZSPKFZT", (ftnlen)8);
	    return 0;
	}
	vaddg_(sttctr, stemp, &c__6, starg);
	*lt = vnorm_(starg) / clight_();
    } else {

/*        Get the auxiliary information about the requested output */
/*        frame. */

	namfrm_(ref, &reqfrm, ref_len);
	if (reqfrm == 0) {
	    setmsg_("The requested output frame '#' is not recognized by the"
		    " reference frame subsystem. Please check that the approp"
		    "riate kernels have been loaded and that you have correct"
		    "ly entered the name of the output frame. ", (ftnlen)208);
	    errch_("#", ref, (ftnlen)1, ref_len);
	    sigerr_("SPICE(UNKNOWNFRAME)", (ftnlen)19);
	    chkout_("ZZSPKFZT", (ftnlen)8);
	    return 0;
	}
	frinfo_(&reqfrm, &center, &type__, &typeid, &found);

/*        If we are dealing with an inertial frame, we can simply */
/*        call ZZSPKFAT and return. */

	if (type__ == 1) {
	    zzspkfat_((S_fp)trgsub, et, ref, abcorr, obs, starg, lt, &dlt, 
		    ref_len, abcorr_len);
	    chkout_("ZZSPKFZT", (ftnlen)8);
	    return 0;
	}

/*        Still here? */

/*        We are dealing with a non-inertial frame. But we need to do */
/*        light time and stellar aberration corrections in an inertial */
/*        frame. Get the "apparent" state of TARG in the intermediary */
/*        inertial reference frame J2000. */

/*        We also need the light time to the center of the frame. */
/*        We compute that first so that we can re-use the temporary */
/*        variable STATE when we compute the inertial apparent state */
/*        of the target relative to the observer. */

	zzspkfat_((S_fp)trgsub, et, "J2000", abcorr, obs, state, lt, &dlt, (
		ftnlen)5, abcorr_len);
	if (failed_()) {
	    chkout_("ZZSPKFZT", (ftnlen)8);
	    return 0;
	}

/*        If the frame is centered at the observer, the light time and */
/*        light time rate are both zero. If the frame is centered */
/*        elsewhere, we'll need to obtain the light time between the */
/*        observer and the frame center. Unlike SPKEZ, we have no case */
/*        here for the frame centered at the target, since the target */
/*        isn't an ephemeris object, as far as this routine can */
/*        determine. */

	if (center == *obs) {
	    ltcent = 0.;
	    dltctr = 0.;
	} else {
	    spkssb_(obs, et, "J2000", stobs, (ftnlen)5);
	    spkltc_(&center, et, "J2000", abcorr, stobs, stemp, &ltcent, &
		    dltctr, (ftnlen)5, abcorr_len);
	}

/*        If something went wrong (like we couldn't get the state of */
/*        the center relative to the observer) now it is time to quit. */

	if (failed_()) {
	    chkout_("ZZSPKFZT", (ftnlen)8);
	    return 0;
	}

/*        If the aberration corrections are for transmission, make the */
/*        sign of the light time positive, since we wish to compute the */
/*        orientation of the non-inertial frame at an epoch later than */
/*        ET by the one-way light time. */

	if (xmit) {
	    ltsign = 1;
	} else {
	    ltsign = -1;
	}

/*        Get the state transformation from J2000 to the requested frame */
/*        and convert the state. */

	d__1 = *et + ltsign * ltcent;
	frmchg_(&fj2000, &reqfrm, &d__1, xform);
	if (failed_()) {
	    chkout_("ZZSPKFZT", (ftnlen)8);
	    return 0;
	}

/*        There's a tricky bit here:  since XFORM is evaluated */
/*        at time */

/*           ET + LTSIGN*LTCENT */

/*        XFORM is actually dependent on LTCENT. We need to account for */
/*        this dependency in our velocity transformation. */

	zzcorsxf_(&xmit, &dltctr, xform, corxfm);

/*        Now apply the frame transformation CORXFM to produce the */
/*        state expressed relative to the request frame REQFRM. */

	mxvg_(corxfm, state, &c__6, &c__6, starg);
    }
    chkout_("ZZSPKFZT", (ftnlen)8);
    return 0;
} /* zzspkfzt_ */

