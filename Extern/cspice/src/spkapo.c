/* spkapo.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__0 = 0;
static integer c__9 = 9;

/* $Procedure SPKAPO ( S/P Kernel, apparent position only ) */
/* Subroutine */ int spkapo_(integer *targ, doublereal *et, char *ref, 
	doublereal *sobs, char *abcorr, doublereal *ptarg, doublereal *lt, 
	ftnlen ref_len, ftnlen abcorr_len)
{
    /* Initialized data */

    static logical first = TRUE_;
    static char flags[5*9] = "NONE " "LT   " "LT+S " "CN   " "CN+S " "XLT  " 
	    "XLT+S" "XCN  " "XCN+S";
    static char prvcor[5] = "     ";

    /* System generated locals */
    integer i__1;
    doublereal d__1;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    char corr[5];
    extern /* Subroutine */ int vsub_(doublereal *, doublereal *, doublereal *
	    );
    static logical xmit;
    extern /* Subroutine */ int vequ_(doublereal *, doublereal *);
    doublereal tpos[3];
    integer i__, refid;
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen);
    static logical usecn, uselt;
    extern doublereal vnorm_(doublereal *);
    extern logical failed_(void);
    extern doublereal clight_(void);
    extern integer isrchc_(char *, integer *, char *, ftnlen, ftnlen);
    extern /* Subroutine */ int stelab_(doublereal *, doublereal *, 
	    doublereal *), sigerr_(char *, ftnlen), chkout_(char *, ftnlen), 
	    stlabx_(doublereal *, doublereal *, doublereal *);
    integer ltsign;
    extern /* Subroutine */ int ljucrs_(integer *, char *, char *, ftnlen, 
	    ftnlen), setmsg_(char *, ftnlen);
    integer maxitr;
    extern /* Subroutine */ int irfnum_(char *, integer *, ftnlen), spkgps_(
	    integer *, doublereal *, char *, integer *, doublereal *, 
	    doublereal *, ftnlen);
    extern logical return_(void);
    static logical usestl;
    extern logical odd_(integer *);

/* $ Abstract */

/*     Return the position of a target body relative to an observer, */
/*     optionally corrected for light time and stellar aberration. */

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

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     TARG       I   Target body. */
/*     ET         I   Observer epoch. */
/*     REF        I   Inertial reference frame of observer's state. */
/*     SOBS       I   State of observer wrt. solar system barycenter. */
/*     ABCORR     I   Aberration correction flag. */
/*     PTARG      O   Position of target. */
/*     LT         O   One way light time between observer and target. */

/* $ Detailed_Input */

/*     TARG     is the NAIF ID code for a target body. The target */
/*              and observer define a position vector which points */
/*              from the observer to the target. */

/*     ET       is the ephemeris time, expressed as seconds past */
/*              J2000 TDB, at which the position of the target body */
/*              relative to the observer is to be computed. ET */
/*              refers to time at the observer's location. */

/*     REF      is the inertial reference frame with respect to which */
/*              the observer's state SOBS is expressed. REF must be */
/*              recognized by the SPICE Toolkit. The acceptable */
/*              frames are listed in the Frames Required Reading, as */
/*              well as in the SPICELIB routine CHGIRF. */

/*              Case and blanks are not significant in the string REF. */

/*     SOBS     is the geometric (uncorrected) state of the observer */
/*              relative to the solar system barycenter at epoch ET. */
/*              SOBS is a 6-vector:  the first three components of */
/*              SOBS represent a Cartesian position vector; the last */
/*              three components represent the corresponding velocity */
/*              vector. SOBS is expressed relative to the inertial */
/*              reference frame designated by REF. */

/*              Units are always km and km/sec. */


/*     ABCORR   indicates the aberration corrections to be applied to */
/*              the position of the target body to account for */
/*              one-way light time and stellar aberration. See the */
/*              discussion in the $Particulars section for */
/*              recommendations on how to choose aberration */
/*              corrections. */

/*              ABCORR may be any of the following: */

/*                 'NONE'     Apply no correction. Return the */
/*                            geometric position of the target body */
/*                            relative to the observer. */

/*              The following values of ABCORR apply to the */
/*              "reception" case in which photons depart from the */
/*              target's location at the light-time corrected epoch */
/*              ET-LT and *arrive* at the observer's location at ET: */

/*                 'LT'       Correct for one-way light time (also */
/*                            called "planetary aberration") using a */
/*                            Newtonian formulation. This correction */
/*                            yields the position of the target at the */
/*                            moment it emitted photons arriving at */
/*                            the observer at ET. */

/*                            The light time correction involves */
/*                            iterative solution of the light time */
/*                            equation (see $Particulars for details). */
/*                            The solution invoked by the 'LT' option */
/*                            uses one iteration. */

/*                 'LT+S'     Correct for one-way light time and */
/*                            stellar aberration using a Newtonian */
/*                            formulation. This option modifies the */
/*                            position obtained with the 'LT' option */
/*                            to account for the observer's velocity */
/*                            relative to the solar system */
/*                            barycenter. The result is the apparent */
/*                            position of the target---the position */
/*                            of the target as seen by the observer. */

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
/*                            computed. See the $Particulars section */
/*                            of SPKEZR for a discussion of precision */
/*                            of light time corrections. */

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
/*                            position of the target at the moment it */
/*                            receives photons emitted from the */
/*                            observer's location at ET. */

/*                 'XLT+S'    "Transmission" case: correct for */
/*                            one-way light time and stellar */
/*                            aberration using a Newtonian */
/*                            formulation  This option modifies the */
/*                            position obtained with the 'XLT' option */
/*                            to account for the observer's velocity */
/*                            relative to the solar system */
/*                            barycenter. The target position */
/*                            indicates the direction that photons */
/*                            emitted from the observer's location */
/*                            must be "aimed" to hit the target. */

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

/* $ Detailed_Output */

/*     PTARG    is a Cartesian 3-vector representing the position of */
/*              the target body relative to the specified observer. */
/*              PTARG is corrected for the specified aberrations, and */
/*              is expressed with respect to the specified inertial */
/*              reference frame. The components of PTARG represent */
/*              the x-, y- and z-components of the target's position. */

/*              The vector PTARG points from the observer's position */
/*              at ET to the aberration-corrected location of the */
/*              target. Note that the sense of the position vector is */
/*              independent of the direction of radiation travel */
/*              implied by the aberration correction. */

/*              Units are always km. */

/*     LT       is the one-way light time between the observer and */
/*              target in seconds. If the target position is */
/*              corrected for aberrations, then LT is the one-way */
/*              light time between the observer and the light time */
/*              corrected target location. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the value of ABCORR is not recognized, the error */
/*         SPICE(SPKINVALIDOPTION) is signaled. */

/*     2)  If the reference frame requested is not a recognized */
/*         inertial reference frame, the error SPICE(BADFRAME) is */
/*         signaled. */

/*     3)  If the position of the target relative to the solar system */
/*         barycenter cannot be computed, an error is signaled by a */
/*         routine in the call tree of this routine. */

/* $ Files */

/*     This routine computes positions using SPK files that have been */
/*     loaded into the SPICE system, normally via the kernel loading */
/*     interface routine FURNSH. Application programs typically load */
/*     kernels once before this routine is called, for example during */
/*     program initialization; kernels need not be loaded repeatedly. */
/*     See the routine FURNSH and the SPK and KERNEL Required Reading */
/*     for further information on loading (and unloading) kernels. */

/*     If any of the ephemeris data used to compute PTARG are expressed */
/*     relative to a non-inertial frame in the SPK files providing those */
/*     data, additional kernels may be needed to enable the reference */
/*     frame transformations required to compute PTARG. Normally */
/*     these additional kernels are PCK files or frame kernels. Any */
/*     such kernels must already be loaded at the time this routine is */
/*     called. */

/* $ Particulars */

/*     In space science or engineering applications one frequently */
/*     wishes to know where to point a remote sensing instrument, such */
/*     as an optical camera or radio antenna, in order to observe or */
/*     otherwise receive radiation from a target. This pointing problem */
/*     is complicated by the finite speed of light: one needs to point */
/*     to where the target appears to be as opposed to where it actually */
/*     is at the epoch of observation. We use the adjectives */
/*     "geometric," "uncorrected," or "true" to refer to an actual */
/*     position or state of a target at a specified epoch. When a */
/*     geometric position or state vector is modified to reflect how it */
/*     appears to an observer, we describe that vector by any of the */
/*     terms "apparent," "corrected," "aberration corrected," or "light */
/*     time and stellar aberration corrected." */

/*     The SPICE Toolkit can correct for two phenomena affecting the */
/*     apparent location of an object: one-way light time (also called */
/*     "planetary aberration") and stellar aberration. Correcting for */
/*     one-way light time is done by computing, given an observer and */
/*     observation epoch, where a target was when the observed photons */
/*     departed the target's location. The vector from the observer to */
/*     this computed target location is called a "light time corrected" */
/*     vector. The light time correction depends on the motion of the */
/*     target, but it is independent of the velocity of the observer */
/*     relative to the solar system barycenter. Relativistic effects */
/*     such as light bending and gravitational delay are not accounted */
/*     for in the light time correction performed by this routine. */

/*     The velocity of the observer also affects the apparent location */
/*     of a target: photons arriving at the observer are subject to a */
/*     "raindrop effect" whereby their velocity relative to the observer */
/*     is, using a Newtonian approximation, the photons' velocity */
/*     relative to the solar system barycenter minus the velocity of the */
/*     observer relative to the solar system barycenter. This effect is */
/*     called "stellar aberration." Stellar aberration is independent */
/*     of the motion of the target. The stellar aberration formula used */
/*     by this routine is non- relativistic. */

/*     Stellar aberration corrections are applied after light time */
/*     corrections: the light time corrected target position vector is */
/*     used as an input to the stellar aberration correction. */

/*     When light time and stellar aberration corrections are both */
/*     applied to a geometric position vector, the resulting position */
/*     vector indicates where the target "appears to be" from the */
/*     observer's location. */

/*     As opposed to computing the apparent position of a target, one */
/*     may wish to compute the pointing direction required for */
/*     transmission of photons to the target. This requires correction */
/*     of the geometric target position for the effects of light time and */
/*     stellar aberration, but in this case the corrections are computed */
/*     for radiation traveling from the observer to the target. */

/*     The "transmission" light time correction yields the target's */
/*     location as it will be when photons emitted from the observer's */
/*     location at ET arrive at the target. The transmission stellar */
/*     aberration correction is the inverse of the traditional stellar */
/*     aberration correction: it indicates the direction in which */
/*     radiation should be emitted so that, using a Newtonian */
/*     approximation, the sum of the velocity of the radiation relative */
/*     to the observer and of the observer's velocity, relative to the */
/*     solar system barycenter, yields a velocity vector that points in */
/*     the direction of the light time corrected position of the target. */

/*     The traditional aberration corrections applicable to observation */
/*     and those applicable to transmission are related in a simple way: */
/*     one may picture the geometry of the "transmission" case by */
/*     imagining the "observation" case running in reverse time order, */
/*     and vice versa. */

/*     One may reasonably object to using the term "observer" in the */
/*     transmission case, in which radiation is emitted from the */
/*     observer's location. The terminology was retained for */
/*     consistency with earlier documentation. */

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


/*        5) Use a geometric position vector as a low-accuracy estimate */
/*           of the apparent position for an application where execution */
/*           speed is critical: */

/*              Use 'NONE'. */


/*        6) While this routine cannot perform the relativistic */
/*           aberration corrections required to compute positions */
/*           with the highest possible accuracy, it can supply the */
/*           geometric positions required as inputs to these */
/*           computations: */

/*              Use 'NONE', then apply high-accuracy aberration */
/*              corrections (not available in the SPICE Toolkit). */


/*     Below, we discuss in more detail how the aberration corrections */
/*     applied by this routine are computed. */


/*     Geometric case */
/*     ============== */

/*        SPKAPO begins by computing the geometric position T(ET) of the */
/*        target body relative to the solar system barycenter (SSB). */
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


/*        The returned position vector is */

/*           T(ET) - O(ET) */


/*     Reception case */
/*     ============== */

/*        When any of the options 'LT', 'CN', 'LT+S', 'CN+S' are */
/*        selected, SPKAPO computes the position of the target body at */
/*        epoch ET-LT, where LT is the one-way light time. Let T(t) */
/*        and O(t) represent the positions of the target and observer */
/*        relative to the solar system barycenter at time t; then LT */
/*        is the solution of the */
/*        light-time equation */

/*                  | T(ET-LT) - O(ET) | */
/*           LT = ------------------------                            (1) */
/*                           c */

/*        The ratio */

/*            | T(ET) - O(ET) | */
/*          ---------------------                                     (2) */
/*                    c */

/*        is used as a first approximation to LT; inserting (2) into the */
/*        RHS of the light-time equation (1) yields the "one-iteration" */
/*        estimate of the one-way light time. Repeating the process */
/*        until the estimates of LT converge yields the "converged */
/*        Newtonian" light time estimate. */

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


/*        The light-time corrected position is the vector */

/*           T(ET-LT) - O(ET) */

/*        If correction for stellar aberration is requested, the target */
/*        position is rotated toward the solar system barycenter-relative */
/*        velocity vector of the observer. The magnitude of the rotation */
/*        depends on the magnitude of the observer's velocity relative */
/*        to the solar system barycenter and the angle between */
/*        this velocity and the observer-target vector. The rotation */
/*        is computed as follows: */

/*           Let r be the light time corrected vector from the observer */
/*           to the object, and v be the velocity of the observer with */
/*           respect to the solar system barycenter. Let w be the angle */
/*           between them. The aberration angle phi is given by */

/*              sin(phi) = v sin(w) / c */

/*           Let h be the vector given by the cross product */

/*              h = r X v */

/*           Rotate r by phi radians about h to obtain the apparent */
/*           position of the object. */



/*     Transmission case */
/*     ================== */

/*        When any of the options 'XLT', 'XCN', 'XLT+S', 'XCN+S' are */
/*        selected, SPKAPO computes the position of the target body T at */
/*        epoch ET+LT, where LT is the one-way light time. LT is the */
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


/*        The light-time corrected position is */

/*           T(ET+LT) - O(ET) */

/*        If correction for stellar aberration is requested, the target */
/*        position is rotated away from the solar system barycenter- */
/*        relative velocity vector of the observer. The magnitude of the */
/*        rotation depends on the magnitude of the velocity and the */
/*        angle between the velocity and the observer-target vector. */
/*        The rotation is computed as in the reception case, but the */
/*        sign of the rotation angle is negated. */

/*     Neither special nor general relativistic effects are accounted */
/*     for in the aberration corrections performed by this routine. */

/* $ Examples */

/*     In the following code fragment, SPKSSB and SPKAPO are used */
/*     to display the position of Io (body 501) as seen from the */
/*     Voyager 2 spacecraft (Body -32) at a series of epochs. */

/*     Normally, one would call the high-level reader SPKPOS to obtain */
/*     position vectors. The example below illustrates the interface */
/*     of this routine, but is not intended as a recommendation on */
/*     how to use the SPICE SPK subsystem. */

/*     The use of integer ID codes is necessitated by the low-level */
/*     interface of this routine. */

/*        IO    = 501 */
/*        VGR2  = -32 */

/*        DO WHILE ( EPOCH .LE. END ) */

/*           CALL SPKSSB (  VGR2,  EPOCH, 'J2000', STVGR2  ) */
/*           CALL SPKAPO (  IO,    EPOCH, 'J2000', STVGR2, */
/*       .                 'LT+S', STIO,   LT              ) */

/*           CALL RECRAD (  STIO,  RANGE,  RA,     DEC     ) */
/*           WRITE (*,*)  RA * DPR(),  DEC * DPR() */

/*           EPOCH = EPOCH + DELTA */

/*        END DO */

/* $ Restrictions */

/*     1)  The ephemeris files to be used by SPKAPO must be loaded */
/*         (normally by the SPICELIB kernel loader FURNSH) before */
/*         this routine is called. */

/*     2)  Unlike most other SPK position computation routines, this */
/*         routine requires that the input state be relative to an */
/*         inertial reference frame. Non-inertial frames are not */
/*         supported by this routine. */

/*     3)  In a future version of this routine, the implementation */
/*         of the aberration corrections may be enhanced to improve */
/*         accuracy. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.3.1, 28-APR-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. Removed */
/*        unnecessary $Revisions section. */

/* -    SPICELIB Version 2.3.0, 03-JUL-2014 (NJB) (BVS) */

/*        Discussion of light time corrections was updated. Assertions */
/*        that converged light time corrections are unlikely to be */
/*        useful were removed. */

/*        Last update was 21-SEP-2013 (BVS) */

/*           Updated to call LJUCRS instead of CMPRSS/UCASE. */

/* -    SPICELIB Version 2.2.0, 17-MAY-2010 (NJB) */

/*        Bug fix: routine now returns immediately after */
/*        state lookup failure. */

/* -    SPICELIB Version 2.1.0, 31-AUG-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in VSUB call. */

/* -    SPICELIB Version 2.0.1, 20-OCT-2003 (EDW) */

/*        Added mention that LT returns in seconds. */
/*        Corrected spelling errors. */

/* -    SPICELIB Version 2.0.0, 18-DEC-2001 (NJB) */

/*        Updated to handle aberration corrections for transmission */
/*        of radiation. Formerly, only the reception case was */
/*        supported. The header was revised and expanded to explain */
/*        the functionality of this routine in more detail. */

/* -    SPICELIB Version 1.0.0, 03-MAR-1999 (WLT) */

/* -& */
/* $ Index_Entries */

/*     apparent position from SPK file */
/*     get apparent position */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Indices of flags in the FLAGS array: */


/*     NAIF ID code for the solar system barycenter: */


/*     Local variables */


/*     Saved variables */


/*     Initial values */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("SPKAPO", (ftnlen)6);
    }
    if (first || s_cmp(abcorr, prvcor, abcorr_len, (ftnlen)5) != 0) {

/*        The aberration correction flag differs from the value it */
/*        had on the previous call, if any.  Analyze the new flag. */

/*        Remove leading and embedded white space from the aberration */
/*        correction flag, then convert to upper case. */

	ljucrs_(&c__0, abcorr, corr, abcorr_len, (ftnlen)5);

/*        Locate the flag in our list of flags. */

	i__ = isrchc_(corr, &c__9, flags, (ftnlen)5, (ftnlen)5);
	if (i__ == 0) {
	    setmsg_("Requested aberration correction was #.", (ftnlen)38);
	    errch_("#", abcorr, (ftnlen)1, abcorr_len);
	    sigerr_("SPICE(SPKINVALIDOPTION)", (ftnlen)23);
	    chkout_("SPKAPO", (ftnlen)6);
	    return 0;
	}

/*        The aberration correction flag is recognized; save it. */

	s_copy(prvcor, abcorr, (ftnlen)5, abcorr_len);

/*        Set logical flags indicating the attributes of the requested */
/*        correction. */

	xmit = i__ > 5;
	uselt = i__ == 2 || i__ == 3 || i__ == 6 || i__ == 7;
	usestl = i__ > 1 && odd_(&i__);
	usecn = i__ == 4 || i__ == 5 || i__ == 8 || i__ == 9;
	first = FALSE_;
    }

/*     See if the reference frame is a recognized inertial frame. */

    irfnum_(ref, &refid, ref_len);
    if (refid == 0) {
	setmsg_("The requested frame '#' is not a recognized inertial frame. "
		, (ftnlen)60);
	errch_("#", ref, (ftnlen)1, ref_len);
	sigerr_("SPICE(BADFRAME)", (ftnlen)15);
	chkout_("SPKAPO", (ftnlen)6);
	return 0;
    }

/*     Determine the sign of the light time offset. */

    if (xmit) {
	ltsign = 1;
    } else {
	ltsign = -1;
    }

/*     Find the geometric position of the target body with respect to the */
/*     solar system barycenter. Subtract the position of the observer */
/*     to get the relative position. Use this to compute the one-way */
/*     light time. */

    spkgps_(targ, et, ref, &c__0, ptarg, lt, ref_len);
    if (failed_()) {
	chkout_("SPKAPO", (ftnlen)6);
	return 0;
    }
    vsub_(ptarg, sobs, tpos);
    vequ_(tpos, ptarg);
    *lt = vnorm_(ptarg) / clight_();

/*     To correct for light time, find the position of the target body */
/*     at the current epoch minus the one-way light time. Note that */
/*     the observer remains where he is. */

    if (uselt) {
	maxitr = 1;
    } else if (usecn) {
	maxitr = 3;
    } else {
	maxitr = 0;
    }
    i__1 = maxitr;
    for (i__ = 1; i__ <= i__1; ++i__) {
	d__1 = *et + ltsign * *lt;
	spkgps_(targ, &d__1, ref, &c__0, ptarg, lt, ref_len);
	if (failed_()) {
	    chkout_("SPKAPO", (ftnlen)6);
	    return 0;
	}
	vsub_(ptarg, sobs, tpos);
	vequ_(tpos, ptarg);
	*lt = vnorm_(ptarg) / clight_();
    }

/*     At this point, PTARG contains the geometric or light-time */
/*     corrected position of the target relative to the observer, */
/*     depending on the specified correction. */

/*     If stellar aberration correction is requested, perform it now. */

    if (usestl) {
	if (xmit) {

/*           This is the transmission case. */

/*           Compute the position vector obtained by applying */
/*           "reception" stellar aberration to PTARG. */

	    stlabx_(ptarg, &sobs[3], tpos);
	    vequ_(tpos, ptarg);
	} else {

/*           This is the reception case. */

/*           Compute the position vector obtained by applying */
/*           "reception" stellar aberration to PTARG. */

	    stelab_(ptarg, &sobs[3], tpos);
	    vequ_(tpos, ptarg);
	}
    }
    chkout_("SPKAPO", (ftnlen)6);
    return 0;
} /* spkapo_ */

