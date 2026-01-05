/* spkpos.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure SPKPOS ( S/P Kernel, position ) */
/* Subroutine */ int spkpos_(char *targ, doublereal *et, char *ref, char *
	abcorr, char *obs, doublereal *ptarg, doublereal *lt, ftnlen targ_len,
	 ftnlen ref_len, ftnlen abcorr_len, ftnlen obs_len)
{
    /* Initialized data */

    static logical first = TRUE_;

    extern /* Subroutine */ int zzbods2c_(integer *, char *, integer *, 
	    logical *, char *, integer *, logical *, ftnlen, ftnlen), 
	    zzctruin_(integer *), chkin_(char *, ftnlen);
    integer obsid;
    extern /* Subroutine */ int errch_(char *, char *, ftnlen, ftnlen);
    logical found;
    static logical svfnd1, svfnd2;
    static integer svctr1[2], svctr2[2];
    integer targid;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    static integer svtgid;
    extern /* Subroutine */ int setmsg_(char *, ftnlen);
    static integer svobsi;
    static char svtarg[36], svobsn[36];
    extern /* Subroutine */ int spkezp_(integer *, doublereal *, char *, char 
	    *, integer *, doublereal *, doublereal *, ftnlen, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Return the position of a target body relative to an observing */
/*     body, optionally corrected for light time (planetary aberration) */
/*     and stellar aberration. */

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

/*     ABCORR */
/*     SPK */
/*     NAIF_IDS */
/*     FRAMES */
/*     TIME */

/* $ Keywords */

/*     EPHEMERIS */

/* $ Declarations */
/* $ Abstract */

/*     This include file defines the dimension of the counter */
/*     array used by various SPICE subsystems to uniquely identify */
/*     changes in their states. */

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

/*     CTRSIZ      is the dimension of the counter array used by */
/*                 various SPICE subsystems to uniquely identify */
/*                 changes in their states. */

/* $ Author_and_Institution */

/*     B.V. Semenov    (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 29-JUL-2013 (BVS) */

/* -& */

/*     End of include file. */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     TARG       I   Target body name. */
/*     ET         I   Observer epoch. */
/*     REF        I   Reference frame of output position vector. */
/*     ABCORR     I   Aberration correction flag. */
/*     OBS        I   Observing body name. */
/*     PTARG      O   Position of target. */
/*     LT         O   One way light time between observer and target. */

/* $ Detailed_Input */

/*     TARG     is the name of a target body. Optionally, you may */
/*              supply the integer ID code for the object as an */
/*              integer string. For example both 'MOON' and '301' */
/*              are legitimate strings that indicate the moon is the */
/*              target body. */

/*              The target and observer define a position vector */
/*              which points from the observer to the target. */

/*     ET       is the ephemeris time, expressed as seconds past J2000 */
/*              TDB, at which the position of the target body relative to */
/*              the observer is to be computed. ET refers to time at the */
/*              observer's location. */

/*     REF      is the name of the reference frame relative to which */
/*              the output position vector should be expressed. This */
/*              may be any frame supported by the SPICE system, */
/*              including built-in frames (documented in the Frames */
/*              Required Reading) and frames defined by a loaded */
/*              frame kernel (FK). */

/*              When REF designates a non-inertial frame, the */
/*              orientation of the frame is evaluated at an epoch */
/*              dependent on the selected aberration correction. See */
/*              the description of the output position vector PTARG */
/*              for details. */

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
/*                            yields the position of the target at */
/*                            the moment it emitted photons arriving */
/*                            at the observer at ET. */

/*                            The light time correction uses an */
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
/*                            as seen by the observer. */

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
/*                            below for a discussion of precision of */
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
/*                            position of the target at the moment it */
/*                            receives photons emitted from the */
/*                            observer's location at ET. */

/*                 'XLT+S'    "Transmission" case: correct for */
/*                            one-way light time and stellar */
/*                            aberration using a Newtonian */
/*                            formulation. This option modifies the */
/*                            position obtained with the 'XLT' option */
/*                            to account for the observer's velocity */
/*                            relative to the solar system */
/*                            barycenter. The computed target */
/*                            position indicates the direction that */
/*                            photons emitted from the observer's */
/*                            location must be "aimed" to hit the */
/*                            target. */

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

/*     OBS      is the name of an observing body. Optionally, you */
/*              may supply the ID code of the object as an integer */
/*              string. For example, both 'EARTH' and '399' are */
/*              legitimate strings to supply to indicate the */
/*              observer is Earth. */

/* $ Detailed_Output */

/*     PTARG    is a Cartesian 3-vector representing the position of */
/*              the target body relative to the specified observer. */
/*              PTARG is corrected for the specified aberrations, and */
/*              is expressed with respect to the reference frame */
/*              specified by REF. The three components of PTARG */
/*              represent the x-, y- and z-components of the target's */
/*              position. */

/*              PTARG points from the observer's location at ET to */
/*              the aberration-corrected location of the target. */
/*              Note that the sense of this position vector is */
/*              independent of the direction of radiation travel */
/*              implied by the aberration correction. */

/*              Units are always km. */

/*              Non-inertial frames are treated as follows: letting */
/*              LTCENT be the one-way light time between the observer */
/*              and the central body associated with the frame, the */
/*              orientation of the frame is evaluated at ET-LTCENT, */
/*              ET+LTCENT, or ET depending on whether the requested */
/*              aberration correction is, respectively, for received */
/*              radiation, transmitted radiation, or is omitted. */
/*              LTCENT is computed using the method indicated by */
/*              ABCORR. */

/*     LT       is the one-way light time between the observer and */
/*              target in seconds. If the target position is */
/*              corrected for aberrations, then LT is the one-way */
/*              light time between the observer and the light time */
/*              corrected target location. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If name of target or observer cannot be translated to its */
/*         NAIF ID code, the error SPICE(IDCODENOTFOUND) is signaled. */

/*     2)  If the reference frame REF is not a recognized reference */
/*         frame, the error SPICE(UNKNOWNFRAME) is signaled. */

/*     3)  If the loaded kernels provide insufficient data to compute the */
/*         requested position vector, an error is signaled by a routine */
/*         in the call tree of this routine. */

/*     4)  If an error occurs while reading an SPK or other kernel file, */
/*         the error  is signaled by a routine in the call tree */
/*         of this routine. */

/* $ Files */

/*     This routine computes positions using SPK files that have been */
/*     loaded into the SPICE system, normally via the kernel loading */
/*     interface routine FURNSH. See the routine FURNSH and the SPK */
/*     and KERNEL Required Reading for further information on loading */
/*     (and unloading) kernels. */

/*     If the output position PTARG is to be expressed relative to a */
/*     non-inertial frame, or if any of the ephemeris data used to */
/*     compute PTARG are expressed relative to a non-inertial frame in */
/*     the SPK files providing those data, additional kernels may be */
/*     needed to enable the reference frame transformations required to */
/*     compute the position. These additional kernels may be C-kernels, */
/*     PCK files or frame kernels. Any such kernels must already be */
/*     loaded at the time this routine is called. */

/* $ Particulars */

/*     This routine is part of the user interface to the SPICE ephemeris */
/*     system. It allows you to retrieve position information for any */
/*     ephemeris object relative to any other in a reference frame that */
/*     is convenient for further computations. */

/*     This routine is identical in function to the routine SPKEZP */
/*     except that it allows you to refer to ephemeris objects by name */
/*     (via a character string). */

/*     Please refer to the Aberration Corrections Required Reading */
/*     abcorr.req for detailed information describing the nature and */
/*     calculation of the applied corrections. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Load a planetary ephemeris SPK, then look up a series of */
/*        geometric positions of the moon relative to the earth, */
/*        referenced to the J2000 frame. */

/*        Use the SPK kernel below to load the required Earth and */
/*        Moon ephemeris data. */

/*           de421.bsp */


/*        Example code begins here. */


/*              IMPLICIT NONE */
/*        C */
/*        C     Local constants */
/*        C */
/*              CHARACTER*(*)         FRAME */
/*              PARAMETER           ( FRAME  = 'J2000' ) */

/*              CHARACTER*(*)         ABCORR */
/*              PARAMETER           ( ABCORR = 'NONE' ) */

/*              CHARACTER*(*)         SPK */
/*              PARAMETER           ( SPK    = 'de421.bsp' ) */

/*        C */
/*        C     ET0 represents the date 2000 Jan 1 12:00:00 TDB. */
/*        C */
/*              DOUBLE PRECISION      ET0 */
/*              PARAMETER           ( ET0    = 0.0D0 ) */

/*        C */
/*        C     Use a time step of 1 hour; look up 4 positions. */
/*        C */
/*              DOUBLE PRECISION      STEP */
/*              PARAMETER           ( STEP   = 3600.0D0 ) */

/*              INTEGER               MAXITR */
/*              PARAMETER           ( MAXITR = 4 ) */

/*              CHARACTER*(*)         OBSRVR */
/*              PARAMETER           ( OBSRVR = 'Earth' ) */

/*              CHARACTER*(*)         TARGET */
/*              PARAMETER           ( TARGET = 'Moon' ) */

/*        C */
/*        C     Local variables */
/*        C */
/*              DOUBLE PRECISION      ET */
/*              DOUBLE PRECISION      LT */
/*              DOUBLE PRECISION      POS ( 3 ) */

/*              INTEGER               I */

/*        C */
/*        C     Load the SPK file. */
/*        C */
/*              CALL FURNSH ( SPK ) */

/*        C */
/*        C     Step through a series of epochs, looking up a */
/*        C     position vector at each one. */
/*        C */
/*              DO I = 1, MAXITR */

/*                 ET = ET0 + (I-1)*STEP */

/*                 CALL SPKPOS ( TARGET, ET, FRAME, ABCORR, OBSRVR, */
/*             .                 POS,    LT                        ) */

/*                 WRITE (*,*) 'ET = ', ET */
/*                 WRITE (*,*) ' ' */
/*                 WRITE (*,*) 'J2000 x-position (km):   ', POS(1) */
/*                 WRITE (*,*) 'J2000 y-position (km):   ', POS(2) */
/*                 WRITE (*,*) 'J2000 z-position (km):   ', POS(3) */
/*                 WRITE (*,*) ' ' */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         ET =    0.0000000000000000 */

/*         J2000 x-position (km):     -291608.38530964090 */
/*         J2000 y-position (km):     -266716.83294678747 */
/*         J2000 z-position (km):     -76102.487146783606 */

/*         ET =    3600.0000000000000 */

/*         J2000 x-position (km):     -289279.89831331203 */
/*         J2000 y-position (km):     -269104.10842893779 */
/*         J2000 z-position (km):     -77184.242072912006 */

/*         ET =    7200.0000000000000 */

/*         J2000 x-position (km):     -286928.00140550011 */
/*         J2000 y-position (km):     -271469.99024601618 */
/*         J2000 z-position (km):     -78259.908307700243 */

/*         ET =    10800.000000000000 */

/*         J2000 x-position (km):     -284552.90265547187 */
/*         J2000 y-position (km):     -273814.30975274299 */
/*         J2000 z-position (km):     -79329.406046598189 */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     C.H. Acton         (JPL) */
/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 3.2.0, 01-OCT-2021 (JDR) (NJB) */

/*        Deleted include statement for frmtyp.inc. */

/*        Edited the header to comply with NAIF standard. Added complete */
/*        code example from existing fragment. */

/*        Updated $Particulars to refer to Aberration Corrections */
/*        Required Reading document, which was added to */
/*        $Required_Reading list. */

/* -    SPICELIB Version 3.1.0, 03-JUL-2014 (NJB) (BVS) */

/*        Discussion of light time corrections was updated. Assertions */
/*        that converged light time corrections are unlikely to be */
/*        useful were removed. */

/*        Updated to save the input body names and ZZBODTRN state */
/*        counters and to do name-ID conversions only if the counters */
/*        have changed. */

/* -    SPICELIB Version 3.0.3, 04-APR-2008 (NJB) */

/*        Corrected minor error in description of XLT+S aberration */
/*        correction. */

/* -    SPICELIB Version 3.0.2, 20-OCT-2003 (EDW) */

/*        Added mention that LT returns in seconds. */

/* -    SPICELIB Version 3.0.1, 29-JUL-2003 (NJB) (CHA) */

/*        Various minor header changes were made to improve clarity. */

/* -    SPICELIB Version 3.0.0, 31-DEC-2001 (NJB) */

/*        Updated to handle aberration corrections for transmission */
/*        of radiation. Formerly, only the reception case was */
/*        supported. The header was revised and expanded to explain */
/*        the functionality of this routine in more detail. */

/* -    SPICELIB Version 1.0.0, 03-MAR-1999 (WLT) */

/* -& */
/* $ Index_Entries */

/*     using body names get position relative to an observer */
/*     get position relative observer corrected for aberrations */
/*     read ephemeris data */
/*     read trajectory data */

/* -& */

/*     SPICELIB functions */


/*     Saved body name length. */


/*     Local variables */


/*     Saved name/ID item declarations. */


/*     Saved name/ID items. */


/*     Initial values. */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("SPKPOS", (ftnlen)6);
    }

/*     Initialization. */

    if (first) {

/*        Initialize counters. */

	zzctruin_(svctr1);
	zzctruin_(svctr2);
	first = FALSE_;
    }

/*     Starting from translation of target name to its code */

    zzbods2c_(svctr1, svtarg, &svtgid, &svfnd1, targ, &targid, &found, (
	    ftnlen)36, targ_len);
    if (! found) {
	setmsg_("The target, '#', is not a recognized name for an ephemeris "
		"object. The cause of this problem may be that you need an up"
		"dated version of the SPICE toolkit. Alternatively you may ca"
		"ll SPKEZP directly if you know the SPICE id-codes for both '"
		"#' and '#' ", (ftnlen)250);
	errch_("#", targ, (ftnlen)1, targ_len);
	errch_("#", targ, (ftnlen)1, targ_len);
	errch_("#", obs, (ftnlen)1, obs_len);
	sigerr_("SPICE(IDCODENOTFOUND)", (ftnlen)21);
	chkout_("SPKPOS", (ftnlen)6);
	return 0;
    }

/*     Now do the same for observer. */

    zzbods2c_(svctr2, svobsn, &svobsi, &svfnd2, obs, &obsid, &found, (ftnlen)
	    36, obs_len);
    if (! found) {
	setmsg_("The observer, '#', is not a recognized name for an ephemeri"
		"s object. The cause of this problem may be that you need an "
		"updated version of the SPICE toolkit. Alternatively you may "
		"call SPKEZP directly if you know the SPICE id-codes for both"
		" '#' and '#' ", (ftnlen)252);
	errch_("#", obs, (ftnlen)1, obs_len);
	errch_("#", targ, (ftnlen)1, targ_len);
	errch_("#", obs, (ftnlen)1, obs_len);
	sigerr_("SPICE(IDCODENOTFOUND)", (ftnlen)21);
	chkout_("SPKPOS", (ftnlen)6);
	return 0;
    }

/*     After all translations are done we can call SPKEZP. */

    spkezp_(&targid, et, ref, abcorr, &obsid, ptarg, lt, ref_len, abcorr_len);
    chkout_("SPKPOS", (ftnlen)6);
    return 0;
} /* spkpos_ */

