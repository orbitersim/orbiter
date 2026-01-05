/* spkezr.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure SPKEZR ( S/P Kernel, easier reader ) */
/* Subroutine */ int spkezr_(char *targ, doublereal *et, char *ref, char *
	abcorr, char *obs, doublereal *starg, doublereal *lt, ftnlen targ_len,
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
    extern /* Subroutine */ int spkez_(integer *, doublereal *, char *, char *
	    , integer *, doublereal *, doublereal *, ftnlen, ftnlen);
    static logical svfnd1, svfnd2;
    static integer svctr1[2], svctr2[2];
    integer targid;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    static integer svtgid;
    extern /* Subroutine */ int setmsg_(char *, ftnlen);
    static integer svobsi;
    static char svtarg[36], svobsn[36];
    extern logical return_(void);

/* $ Abstract */

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

/*     ABCORR */
/*     FRAMES */
/*     NAIF_IDS */
/*     SPK */
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
/*     REF        I   Reference frame of output state vector. */
/*     ABCORR     I   Aberration correction flag. */
/*     OBS        I   Observing body name. */
/*     STARG      O   State of target. */
/*     LT         O   One way light time between observer and target. */

/* $ Detailed_Input */

/*     TARG     is the name of a target body. Optionally, you may */
/*              supply the integer ID code for the object as */
/*              an integer string. For example both 'MOON' and */
/*              '301' are legitimate strings that indicate the */
/*              moon is the target body. */

/*              The target and observer define a state vector whose */
/*              position component points from the observer to the */
/*              target. */

/*     ET       is the ephemeris time, expressed as seconds past J2000 */
/*              TDB, at which the state of the target body relative to */
/*              the observer is to be computed. ET refers to time at */
/*              the observer's location. */

/*     REF      is the name of the reference frame relative to which */
/*              the output state vector should be expressed. This may */
/*              be any frame supported by the SPICE system, including */
/*              built-in frames (documented in the Frames Required */
/*              Reading) and frames defined by a loaded frame kernel */
/*              (FK). */

/*              When REF designates a non-inertial frame, the */
/*              orientation of the frame is evaluated at an epoch */
/*              dependent on the selected aberration correction. */
/*              See the description of the output state vector STARG */
/*              for details. */

/*     ABCORR   indicates the aberration corrections to be applied */
/*              to the state of the target body to account for one-way */
/*              light time and stellar aberration. See the discussion */
/*              in the $Particulars section for recommendations on */
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

/*     OBS      is the name of an observing body. Optionally, you */
/*              may supply the ID code of the object as an integer */
/*              string. For example, both 'EARTH' and '399' are */
/*              legitimate strings to supply to indicate the */
/*              observer is Earth. */

/* $ Detailed_Output */

/*     STARG    is a Cartesian state vector representing the position */
/*              and velocity of the target body relative to the */
/*              specified observer. STARG is corrected for the */
/*              specified aberrations, and is expressed with respect */
/*              to the reference frame specified by REF. The first */
/*              three components of STARG represent the x-, y- and */
/*              z-components of the target's position; the last three */
/*              components form the corresponding velocity vector. */

/*              The position component of STARG points from the */
/*              observer's location at ET to the aberration-corrected */
/*              location of the target. Note that the sense of the */
/*              position vector is independent of the direction of */
/*              radiation travel implied by the aberration */
/*              correction. */

/*              The velocity component of STARG is the derivative */
/*              with respect to time of the position component of */
/*              STARG. */

/*              Units are always km and km/sec. */

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
/*              target in seconds. If the target state is corrected */
/*              for aberrations, then LT is the one-way light time */
/*              between the observer and the light time corrected */
/*              target location. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If name of target or observer cannot be translated to its */
/*         NAIF ID code, the error SPICE(IDCODENOTFOUND) is signaled. */

/*     2)  If the reference frame REF is not a recognized reference */
/*         frame, the error SPICE(UNKNOWNFRAME) is signaled. */

/*     3)  If the loaded kernels provide insufficient data to compute the */
/*         requested state vector, an error is signaled by a routine in */
/*         the call tree of this routine. */

/*     4)  If an error occurs while reading an SPK or other kernel file, */
/*         the error is signaled by a routine in the call tree of this */
/*         routine. */

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
/*     compute the state. These additional kernels may be C-kernels, PCK */
/*     files or frame kernels. Any such kernels must already be loaded */
/*     at the time this routine is called. */

/* $ Particulars */

/*     This routine is part of the user interface to the SPICE ephemeris */
/*     system. It allows you to retrieve state information for any */
/*     ephemeris object relative to any other in a reference frame that */
/*     is convenient for further computations. */

/*     This routine is identical in function to the routine SPKEZ except */
/*     that it allows you to refer to ephemeris objects by name (via a */
/*     character string). */

/*     Please refer to the Aberration Corrections Required Reading */
/*     abcorr.req for detailed information describing the nature and */
/*     calculation of the applied corrections. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Load a planetary ephemeris SPK, then look up a */
/*        state of the MARS BARYCENTER relative to EARTH */
/*        in the J2000 frame with aberration correction LT+S. */

/*        Use the meta-kernel shown below to load the required SPICE */
/*        kernels. */


/*           KPL/MK */

/*           File: spkezr_ex1.tm */

/*           This meta-kernel is intended to support operation of SPICE */
/*           example programs. The kernels shown here should not be */
/*           assumed to contain adequate or correct versions of data */
/*           required by SPICE-based user applications. */

/*           In order for an application to use this meta-kernel, the */
/*           kernels referenced here must be present in the user's */
/*           current working directory. */

/*           The names and contents of the kernels referenced */
/*           by this meta-kernel are as follows: */

/*              File name                        Contents */
/*              ---------                        -------- */
/*              de430.bsp                        Planetary ephemeris */
/*              pck00010.tpc                     Planet orientation and */
/*                                               radii */
/*              naif0011.tls                     Leapseconds */


/*           \begindata */

/*              KERNELS_TO_LOAD = ( 'de430.bsp', */
/*                                  'pck00010.tpc', */
/*                                  'naif0011.tls' ) */
/*           \begintext */

/*           End of meta-kernel */


/*        Example code begins here. */


/*              PROGRAM SPKEZR_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local variables */
/*        C */
/*              CHARACTER*(32)        FRAME */
/*              CHARACTER*(32)        ABCORR */
/*              CHARACTER*(36)        OBS */
/*              CHARACTER*(36)        TARGET */
/*              CHARACTER*(36)        EPOCH */

/*              DOUBLE PRECISION      ET */
/*              DOUBLE PRECISION      LT */
/*              DOUBLE PRECISION      STATE ( 6 ) */

/*              INTEGER               I */

/*        C */
/*        C     Load a set of kernels: an SPK file, a PCK */
/*        C     file and a leapseconds file. Use a meta-kernel. */
/*        C */
/*              CALL FURNSH( 'spkezr_ex1.tm' ) */

/*        C */
/*        C     Define parameters for a state lookup: */
/*        C */
/*        C     Return the state vector of Mars Barycenter (4) as seen */
/*        C     from Earth (399) in the J2000 frame  using aberration */
/*        C     correction LT+S (light time plus stellar aberration) */
/*        C     at the epoch JAN 1 2015 12:00:00. */
/*        C */
/*              TARGET   = 'MARS BARYCENTER' */
/*              EPOCH    = 'JAN 1 2015 12:00:00' */
/*              FRAME    = 'J2000' */
/*              ABCORR   = 'LT+S' */
/*              OBS      = 'EARTH' */

/*        C */
/*        C     Convert the epoch to ephemeris time. */
/*        C */
/*              CALL STR2ET( EPOCH, ET ) */

/*        C */
/*        C     Look-up the state for the defined parameters. */
/*        C */
/*              CALL SPKEZR( TARGET, ET, FRAME, ABCORR, OBS, STATE, LT) */

/*        C */
/*        C     Output... */
/*        C */
/*              WRITE(*,*) 'The position of    : ', TARGET */
/*              WRITE(*,*) 'As observed from   : ', OBS */
/*              WRITE(*,*) 'In reference frame : ', FRAME */
/*              WRITE(*,*) 'At epoch           : ', EPOCH */
/*              WRITE(*,*) ' ' */

/*        C */
/*        C     The first three entries of state contain the */
/*        C     X, Y, Z position components. The final three contain */
/*        C     the Vx, Vy, Vz velocity components. */
/*        C */
/*              WRITE(*,*) 'R (kilometers)     : ' */
/*              WRITE(*,'(3F20.6)') (STATE(I), I=1,3 ) */

/*              WRITE(*,*) 'V (kilometers/sec) : ' */
/*              WRITE(*,'(3F20.6)') (STATE(I), I=4,6 ) */

/*              WRITE(*,*) 'Light time (secs)  : ', LT */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         The position of    : MARS BARYCENTER */
/*         As observed from   : EARTH */
/*         In reference frame : J2000 */
/*         At epoch           : JAN 1 2015 12:00:00 */

/*         R (kilometers)     : */
/*            229953013.746498   -167125346.211588    -78800343.963573 */
/*         V (kilometers/sec) : */
/*                   35.380861           28.653402           12.861524 */
/*         Light time (secs)  :    983.97882466162321 */


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

/* -    SPICELIB Version 4.2.0, 01-OCT-2021 (JDR) (NJB) */

/*        Deleted include statement for frmtyp.inc. */

/*        Edited the header to comply with NAIF standard. Changed output */
/*        format in code example to comply with maximum line length for */
/*        header comments. Added ABCORR to $Required_Reading. */

/* -    SPICELIB Version 4.1.1, 19-JAN-2016 (EDW) */

/*        Example code replaced with a complete program and */
/*        the corresponding output. */

/*        $Particulars updated to refer to Aberration Corrections */
/*        Required Reading document. */

/* -    SPICELIB Version 4.1.0, 03-JUL-2014 (NJB) (BVS) */

/*        Discussion of light time corrections was updated. Assertions */
/*        that converged light time corrections are unlikely to be */
/*        useful were removed. */

/*        Updated to save the input body names and ZZBODTRN state */
/*        counters and to do name-ID conversions only if the counters */
/*        have changed. */

/* -    SPICELIB Version 4.0.0, 27-DEC-2007 (NJB) */

/*        This routine was upgraded to more accurately compute */
/*        aberration-corrected velocity, and in particular, make it */
/*        more consistent with observer-target positions. */

/*        When light time corrections are used, the derivative of light */
/*        time with respect to time is now accounted for in the */
/*        computation of observer-target velocities. When the reference */
/*        frame associated with the output state is time-dependent, the */
/*        derivative of light time with respect to time is now accounted */
/*        for in the computation of the rate of change of orientation of */
/*        the reference frame. */

/*        When stellar aberration corrections are used, velocities */
/*        now reflect the rate of range of the stellar aberration */
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

/* -    SPICELIB Version 2.0.0, 21-FEB-1997 (WLT) */

/*        Extended the functionality of the routine. Users may */
/*        now entered the id code of an object as an ascii string */
/*        and the string will be converted to the corresponding */
/*        integer representation. */

/* -    SPICELIB Version 1.1.0, 09-JUL-1996 (WLT) */

/*        Corrected the description of LT in the Detailed Output */
/*        section of the header. */

/* -    SPICELIB Version 1.0.0, 25-SEP-1995 (BVS) */

/* -& */
/* $ Index_Entries */

/*     using body names get target state relative to an observer */
/*     get state relative to observer corrected for aberrations */
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
	chkin_("SPKEZR", (ftnlen)6);
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
		"dated version of the SPICE Toolkit. Alternatively you may ca"
		"ll SPKEZ directly if you know the SPICE ID codes for both '#"
		"' and '#' ", (ftnlen)249);
	errch_("#", targ, (ftnlen)1, targ_len);
	errch_("#", targ, (ftnlen)1, targ_len);
	errch_("#", obs, (ftnlen)1, obs_len);
	sigerr_("SPICE(IDCODENOTFOUND)", (ftnlen)21);
	chkout_("SPKEZR", (ftnlen)6);
	return 0;
    }

/*     Now do the same for observer */

    zzbods2c_(svctr2, svobsn, &svobsi, &svfnd2, obs, &obsid, &found, (ftnlen)
	    36, obs_len);
    if (! found) {
	setmsg_("The observer, '#', is not a recognized name for an ephemeri"
		"s object. The cause of this problem may be that you need an "
		"updated version of the SPICE toolkit. Alternatively you may "
		"call SPKEZ directly if you know the SPICE ID codes for both "
		"'#' and '#' ", (ftnlen)251);
	errch_("#", obs, (ftnlen)1, obs_len);
	errch_("#", targ, (ftnlen)1, targ_len);
	errch_("#", obs, (ftnlen)1, obs_len);
	sigerr_("SPICE(IDCODENOTFOUND)", (ftnlen)21);
	chkout_("SPKEZR", (ftnlen)6);
	return 0;
    }

/*     After all translations are done we can call SPKEZ. */

    spkez_(&targid, et, ref, abcorr, &obsid, starg, lt, ref_len, abcorr_len);
    chkout_("SPKEZR", (ftnlen)6);
    return 0;
} /* spkezr_ */

