/* spkcpo.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__3 = 3;

/* $Procedure SPKCPO ( SPK, constant position observer state ) */
/* Subroutine */ int spkcpo_(char *target, doublereal *et, char *outref, char 
	*refloc, char *abcorr, doublereal *obspos, char *obsctr, char *obsref,
	 doublereal *state, doublereal *lt, ftnlen target_len, ftnlen 
	outref_len, ftnlen refloc_len, ftnlen abcorr_len, ftnlen obsctr_len, 
	ftnlen obsref_len)
{
    extern /* Subroutine */ int vequ_(doublereal *, doublereal *), chkin_(
	    char *, ftnlen), cleard_(integer *, doublereal *);
    doublereal obsepc, obssta[6];
    extern /* Subroutine */ int chkout_(char *, ftnlen), spkcvo_(char *, 
	    doublereal *, char *, char *, char *, doublereal *, doublereal *, 
	    char *, char *, doublereal *, doublereal *, ftnlen, ftnlen, 
	    ftnlen, ftnlen, ftnlen, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Return the state of a specified target relative to an "observer," */
/*     where the observer has constant position in a specified reference */
/*     frame. The observer's position is provided by the calling program */
/*     rather than by loaded SPK files. */

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

/*     FRAMES */
/*     PCK */
/*     SPK */
/*     TIME */

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
/*     TARGET     I   Name of target ephemeris object. */
/*     ET         I   Observation epoch. */
/*     OUTREF     I   Reference frame of output state. */
/*     REFLOC     I   Output reference frame evaluation locus. */
/*     ABCORR     I   Aberration correction. */
/*     OBSPOS     I   Observer position relative to center of motion. */
/*     OBSCTR     I   Center of motion of observer. */
/*     OBSREF     I   Frame of observer position. */
/*     STATE      O   State of target with respect to observer. */
/*     LT         O   One way light time between target and */
/*                    observer. */

/* $ Detailed_Input */

/*     TARGET   is the name of a target body. Optionally, you may */
/*              supply the ID code of the object as an integer */
/*              string. For example, both 'EARTH' and '399' are */
/*              legitimate strings to supply to indicate the target */
/*              is Earth. */

/*              Case and leading and trailing blanks are not */
/*              significant in the string TARGET. */


/*     ET       is the ephemeris time at which the state of the */
/*              target relative to the observer is to be computed. ET */
/*              is expressed as seconds past J2000 TDB. ET refers to */
/*              time at the observer's location. */


/*     OUTREF   is the name of the reference frame with respect to */
/*              which the output state is expressed. */

/*              When OUTREF is time-dependent (non-inertial), its */
/*              orientation relative to the J2000 frame is evaluated */
/*              in the manner commanded by the input argument REFLOC */
/*              (see description below). */

/*              Case and leading and trailing blanks are not */
/*              significant in the string OUTREF. */


/*     REFLOC   is a string indicating the output reference frame */
/*              evaluation locus: this is the location associated */
/*              with the epoch at which this routine is to evaluate */
/*              the orientation, relative to the J2000 frame, of the */
/*              output frame OUTREF. The values and meanings of */
/*              REFLOC are: */

/*                 'OBSERVER'  Evaluate OUTREF at the observer's */
/*                             epoch ET. */

/*                             Normally the locus 'OBSERVER' should */
/*                             be selected when OUTREF is centered */
/*                             at the observer. */


/*                 'TARGET'    Evaluate OUTREF at the target epoch; */
/*                             letting LT be the one-way light time */
/*                             between the target and observer, the */
/*                             target epoch is */

/*                                ET-LT  if reception aberration */
/*                                       corrections are used */

/*                                ET+LT  if transmission aberration */
/*                                       corrections are used */

/*                                ET     if no aberration corrections */
/*                                       are used */

/*                             Normally the locus 'TARGET' should */
/*                             be selected when OUTREF is centered */
/*                             at the target object. */


/*                 'CENTER'    Evaluate the frame OUTREF at the epoch */
/*                             associated its center. This epoch, */
/*                             which we'll call ETCTR, is determined */
/*                             as follows: */

/*                                Let LTCTR be the one-way light time */
/*                                between the observer and the center */
/*                                of OUTREF. Then ETCTR is */

/*                                   ET-LTCTR  if reception */
/*                                             aberration corrections */
/*                                             are used */

/*                                   ET+LTCTR  if transmission */
/*                                             aberration corrections */
/*                                             are used */

/*                                   ET        if no aberration */
/*                                             corrections are used */


/*                             The locus 'CENTER' should be selected */
/*                             when the user intends to obtain */
/*                             results compatible with those produced */
/*                             by SPKEZR. */

/*              When OUTREF is inertial, all choices of REFLOC */
/*              yield the same results. */

/*              Case and leading and trailing blanks are not */
/*              significant in the string REFLOC. */


/*     ABCORR   indicates the aberration corrections to be applied to */
/*              the observer-target state to account for one-way */
/*              light time and stellar aberration. */

/*              ABCORR may be any of the following: */

/*                 'NONE'     Apply no correction. Return the */
/*                            geometric state of the target */
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
/*                            equation. The solution invoked by the */
/*                            'LT' option uses one iteration. */

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
/*                            until the solution converges. */

/*                 'CN+S'     Converged Newtonian light time */
/*                            and stellar aberration corrections. */


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
/*                            formulation. This option modifies the */
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
/*                            Newtonian light time and stellar */
/*                            aberration corrections. */


/*              Neither special nor general relativistic effects are */
/*              accounted for in the aberration corrections applied */
/*              by this routine. */

/*              Case and leading and trailing blanks are not */
/*              significant in the string ABCORR. */


/*     OBSPOS   is the fixed (constant) geometric position of an */
/*              observer relative to its center of motion OBSCTR, */
/*              expressed in the reference frame OBSREF. */

/*              Units are always km. */


/*     OBSCTR   is the name of the center of motion of OBSPOS. The */
/*              ephemeris of OBSCTR is provided by loaded SPK files. */

/*              Optionally, you may supply the integer ID code for */
/*              the object as an integer string. For example both */
/*              'MOON' and '301' are legitimate strings that indicate */
/*              the moon is the center of motion. */

/*              Case and leading and trailing blanks are not */
/*              significant in the string OBSCTR. */


/*     OBSREF   is the name of the reference frame relative to which */
/*              the input position OBSPOS is expressed. The observer */
/*              has constant position relative to its center of */
/*              motion in this reference frame. */

/*              Case and leading and trailing blanks are not */
/*              significant in the string OBSREF. */

/* $ Detailed_Output */

/*     STATE    is a Cartesian state vector representing the position */
/*              and velocity of the target relative to the specified */
/*              observer. STATE is corrected for the specified */
/*              aberrations and is expressed with respect to the */
/*              reference frame specified by OUTREF. The first three */
/*              components of STATE represent the x-, y- and */
/*              z-components of the target's position; the last three */
/*              components form the corresponding velocity vector. */

/*              The position component of STATE points from the */
/*              observer's location at ET to the aberration-corrected */
/*              location of the target. Note that the sense of the */
/*              position vector is independent of the direction of */
/*              radiation travel implied by the aberration */
/*              correction. */

/*              The velocity component of STATE is the derivative */
/*              with respect to time of the position component of */
/*              STATE. */

/*              Units are always km and km/sec. */

/*              When STATE is expressed in a time-dependent */
/*              (non-inertial) output frame, the orientation of that */
/*              frame relative to the J2000 frame is evaluated in the */
/*              manner indicated by the input argument REFLOC (see */
/*              description above). */


/*     LT       is the one-way light time between the observer and */
/*              target in seconds. If the target state is corrected */
/*              for aberrations, then LT is the one-way light time */
/*              between the observer and the light time corrected */
/*              target location. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If either the name of the center of motion or the target */
/*         cannot be translated to its NAIF ID code, an error is signaled */
/*         by a routine in the call tree of this routine. */

/*     2)  If the reference frame OUTREF is unrecognized, an error is */
/*         signaled by a routine in the call tree of this routine. */

/*     3)  If the reference frame OBSREF is unrecognized, an error is */
/*         signaled by a routine in the call tree of this routine. */

/*     4)  If the frame evaluation locus REFLOC is not recognized, an */
/*         error is signaled by a routine in the call tree of this */
/*         routine. */

/*     5)  If the loaded kernels provide insufficient data to compute */
/*         the requested state vector, an error is signaled */
/*         by a routine in the call tree of this routine. */

/*     6)  If an error occurs while reading an SPK or other kernel file, */
/*         the error  is signaled by a routine in the call tree of */
/*         this routine. */

/*     7)  If the aberration correction ABCORR is not recognized, an */
/*         error is signaled by a routine in the call tree of this */
/*         routine. */

/* $ Files */

/*     Appropriate kernels must be loaded by the calling program before */
/*     this routine is called. */

/*     The following data are required: */

/*     -  SPK data: ephemeris data for the observer center and target */
/*        must be loaded. If aberration corrections are used, the */
/*        states of the observer center and target relative to the */
/*        solar system barycenter must be calculable from the */
/*        available ephemeris data. Typically ephemeris data are made */
/*        available by loading one or more SPK files using FURNSH. */

/*     The following data may be required: */

/*     -  PCK data: if the target frame is a PCK frame, rotation data */
/*        for the target frame must be loaded. These may be provided */
/*        in a text or binary PCK file. */

/*     -  Frame data: if a frame definition not built into SPICE is */
/*        required, for example to convert the observer-target state */
/*        to the output frame, that definition must be available in */
/*        the kernel pool. Typically frame definitions are supplied */
/*        by loading a frame kernel using FURNSH. */

/*     -  Additional kernels: if any frame used in this routine's */
/*        state computation is a CK frame, then at least one CK and */
/*        corresponding SCLK kernel is required. If dynamic frames */
/*        are used, additional SPK, PCK, CK, or SCLK kernels may be */
/*        required. */

/*     In all cases, kernel data are normally loaded once per program */
/*     run, NOT every time this routine is called. */

/* $ Particulars */

/*     This routine computes observer-target states for observers whose */
/*     trajectories are not provided by SPK files. */

/*     Observers supported by this routine must have constant position */
/*     with respect to a specified center of motion, expressed in a */
/*     caller-specified reference frame. The state of the center of */
/*     motion relative to the target must be computable using */
/*     loaded SPK data. */

/*     For applications in which the observer has constant, non-zero */
/*     velocity relative to its center of motion, the SPICELIB routine */

/*        SPKCVO     { SPK, constant velocity observer state } */

/*     can be used. */

/*     This routine is suitable for computing states of target ephemeris */
/*     objects, as seen from landmarks on the surface of an extended */
/*     object, in cases where no SPK data are available for those */
/*     landmarks. */

/*     This routine's treatment of the output reference frame differs */
/*     from that of the principal SPK API routines */

/*        SPKEZR */
/*        SPKEZ */
/*        SPKPOS */
/*        SPKEZP */

/*     which require both observer and target ephemerides to be provided */
/*     by loaded SPK files: */

/*        The SPK API routines listed above evaluate the orientation of */
/*        the output reference frame (with respect to the J2000 frame) */
/*        at an epoch corrected for one-way light time between the */
/*        observer and the center of the output frame. When the center */
/*        of the output frame is not the target (for example, when the */
/*        target is on the surface of Mars and the output frame is */
/*        centered at Mars' center), the epoch of evaluation may not */
/*        closely match the light-time corrected epoch associated with */
/*        the target itself. A similar problem may occur when the */
/*        observer is a surface point on an extended body and the */
/*        output frame is centered at the body center: the listed */
/*        routines will correct the orientation of the output frame for */
/*        one-way light time between the frame center and the observer. */

/*        This routine allows the caller to dictate how the orientation */
/*        of the output reference frame is to be evaluated. The caller */
/*        passes to this routine an input string called the output */
/*        frame's evaluation "locus." This string specifies the location */
/*        associated with the output frame's evaluation epoch. The three */
/*        possible values of the locus are */

/*           'TARGET' */
/*           'OBSERVER' */
/*           'CENTER' */

/*        The choice of locus has an effect when aberration corrections */
/*        are used and the output frame is non-inertial. */

/*        When the locus is 'TARGET' and light time corrections are */
/*        used, the orientation of the output frame is evaluated at the */
/*        epoch obtained by correcting the observation epoch ET for */
/*        one-way light time LT. The evaluation epoch will be either */
/*        ET-LT or ET+LT for reception or transmission corrections */
/*        respectively. */

/*        For remote sensing applications where the target is a surface */
/*        point on an extended object, and the orientation of that */
/*        object should be evaluated at the emission time, the locus */
/*        'TARGET' should be used. */

/*        When the output frame's orientation should be evaluated at */
/*        the observation epoch ET, which is the case when the */
/*        output frame is centered at the observer, the locus */
/*        'OBSERVER' should be used. */

/*        The locus option 'CENTER' is provided for compatibility */
/*        with existing SPK state computation APIs such as SPKEZR. */

/*        Note that the output frame evaluation locus does not affect */
/*        the computation of light time between the target and */
/*        observer. */


/*     The SPK routines that compute observer-target states for */
/*     combinations of objects having ephemerides provided by the SPK */
/*     system and objects having constant position or constant velocity */
/*     are */

/*        SPKCPO {SPK, Constant position observer} */
/*        SPKCPT {SPK, Constant position target} */
/*        SPKCVO {SPK, Constant velocity observer} */
/*        SPKCVT {SPK, Constant velocity target} */

/* $ Examples */

/*     The numerical results shown for these examples may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */


/*     1) Compute apparent solar azimuth and elevation as seen from a */
/*        specified surface point on the earth. */

/*        Task Description */
/*        ================ */

/*        In this example we'll use the location of the DSN station */
/*        DSS-14 as our surface point. */

/*        We'll perform the solar azimuth and elevation computation two */
/*        ways: */

/*           - Using a station frame kernel to provide the */
/*             specification of a topocentric reference frame */
/*             centered at DSS-14. */

/*           - Computing inline the transformation from the earth-fixed, */
/*             earth-centered frame ITRF93 to a topocentric frame */
/*             centered at DSS-14. */

/*        Note that results of the two computations will differ */
/*        slightly. There are three sources of the differences: */

/*           1) The station position is time-dependent due to tectonic */
/*              plate motion, and epochs of the station positions used */
/*              to specify the axes of the topocentric frame are */
/*              different in the two cases. This gives rise to different */
/*              orientations of the frame's axes relative to the frame */
/*              ITRF93. */

/*           2) The two computations use different earth radii; this */
/*              results in computation of different geodetic latitudes */
/*              of the station. This difference also affects the */
/*              topocentric frame orientation relative to ITRF93. */

/*           3) The station movement between ET and the epoch at which */
/*              the DSS-14_TOPO frame is specified contributes a very */
/*              small offset---on the order of 10 cm---to the station-sun */
/*              position vector, expressed in the ITRF93 frame. */


/*        Kernels */
/*        ======= */

/*        Use the meta-kernel shown below to load the required SPICE */
/*        kernels. */


/*        KPL/MK */

/*           File name: spkcpo_ex1.tm */

/*           This is the meta-kernel file for the header code example for */
/*           the subroutine SPKCPO. These kernel files can be found on */
/*           the NAIF website. */

/*           In order for an application to use this meta-kernel, the */
/*           kernels referenced here must be present in the user's */
/*           current working directory. */

/*           The names and contents of the kernels referenced */
/*           by this meta-kernel are as follows: */

/*              File name                        Contents */
/*              ---------                        -------- */
/*              de421.bsp                        Planetary ephemeris */
/*              pck00010.tpc                     Planet orientation and */
/*                                               radii */
/*              naif0010.tls                     Leapseconds */
/*              earth_720101_070426.bpc          Earth historical */
/*                                               binary PCK */
/*              earthstns_itrf93_050714.bsp      DSN station SPK */
/*              earth_topo_050714.tf             DSN station FK */
/*              mgs_moc_v20.ti                   MGS MOC instrument */
/*                                               parameters */
/*              mgs_sclkscet_00061.tsc           MGS SCLK coefficients */
/*              mgs_sc_ext12.bc                  MGS s/c bus attitude */
/*              mgs_ext12_ipng_mgs95j.bsp        MGS ephemeris */

/*           \begindata */

/*           KERNELS_TO_LOAD = ( 'de421.bsp', */
/*                               'pck00010.tpc', */
/*                               'naif0010.tls', */
/*                               'earth_720101_070426.bpc', */
/*                               'earthstns_itrf93_050714.bsp', */
/*                               'earth_topo_050714.tf', */
/*                               'mgs_moc_v20.ti', */
/*                               'mgs_sclkscet_00061.tsc', */
/*                               'mgs_sc_ext12.bc', */
/*                               'mgs_ext12_ipng_mgs95j.bsp'  ) */

/*           \begintext */

/*           End of meta-kernel. */


/*        Example code begins here. */


/*        C */
/*        C     Program: SPKCPO_EX1 */
/*        C */
/*        C     This program uses SPKCPO to compute solar azimuth */
/*        C     and elevation at a given surface point on the earth. */
/*        C */

/*              PROGRAM SPKCPO_EX1 */
/*              IMPLICIT NONE */
/*        C */
/*        C     SPICELIB functions */
/*        C */
/*              DOUBLE PRECISION      DPR */
/*              DOUBLE PRECISION      VDIST */

/*        C */
/*        C     Local parameters */
/*        C */
/*              CHARACTER*(*)         FMT0 */
/*              PARAMETER           ( FMT0   = '(A,3F20.8)' ) */

/*              CHARACTER*(*)         FMT1 */
/*              PARAMETER           ( FMT1   = '(1X,A, F20.8)' ) */

/*              CHARACTER*(*)         META */
/*              PARAMETER           ( META   = 'spkcpo_ex1.tm' ) */

/*              CHARACTER*(*)         TIMFMT */
/*              PARAMETER           ( TIMFMT = */
/*             .                    'YYYY MON DD HR:MN:SC.###### UTC' ) */

/*              CHARACTER*(*)         TIMFM2 */
/*              PARAMETER           ( TIMFM2 = */
/*             .              'YYYY MON DD HR:MN:SC.###### TDB ::TDB' ) */

/*              INTEGER               BDNMLN */
/*              PARAMETER           ( BDNMLN = 36 ) */

/*              INTEGER               CORLEN */
/*              PARAMETER           ( CORLEN = 10 ) */

/*              INTEGER               EVLLEN */
/*              PARAMETER           ( EVLLEN = 25 ) */

/*              INTEGER               FRNMLN */
/*              PARAMETER           ( FRNMLN = 32 ) */

/*              INTEGER               TIMLEN */
/*              PARAMETER           ( TIMLEN = 40 ) */

/*        C */
/*        C     Local variables */
/*        C */
/*              CHARACTER*(CORLEN)    ABCORR */
/*              CHARACTER*(TIMLEN)    EMITIM */
/*              CHARACTER*(EVLLEN)    REFLOC */
/*              CHARACTER*(BDNMLN)    OBSCTR */
/*              CHARACTER*(FRNMLN)    OBSREF */
/*              CHARACTER*(TIMLEN)    OBSTIM */
/*              CHARACTER*(FRNMLN)    OUTREF */
/*              CHARACTER*(BDNMLN)    TARGET */

/*              DOUBLE PRECISION      AZ */
/*              DOUBLE PRECISION      EL */
/*              DOUBLE PRECISION      ET */
/*              DOUBLE PRECISION      F */
/*              DOUBLE PRECISION      LAT */
/*              DOUBLE PRECISION      LON */
/*              DOUBLE PRECISION      LT0 */
/*              DOUBLE PRECISION      LT1 */
/*              DOUBLE PRECISION      NORMAL ( 3 ) */
/*              DOUBLE PRECISION      OBSALT */
/*              DOUBLE PRECISION      OBSLAT */
/*              DOUBLE PRECISION      OBSLON */
/*              DOUBLE PRECISION      OBSPOS ( 3 ) */
/*              DOUBLE PRECISION      R */
/*              DOUBLE PRECISION      RADII  ( 3 ) */
/*              DOUBLE PRECISION      RE */
/*              DOUBLE PRECISION      RP */
/*              DOUBLE PRECISION      STATE0 ( 6 ) */
/*              DOUBLE PRECISION      STATE1 ( 6 ) */
/*              DOUBLE PRECISION      TOPVEC ( 3 ) */
/*              DOUBLE PRECISION      XFORM  ( 3, 3 ) */
/*              DOUBLE PRECISION      Z      ( 3 ) */

/*              INTEGER               I */
/*              INTEGER               N */

/*        C */
/*        C     Initial values */
/*        C */
/*              DATA                  Z / 0.D0, 0.D0, 1.D0 / */


/*        C */
/*        C     Load SPICE kernels. */
/*        C */
/*              CALL FURNSH ( META ) */

/*        C */
/*        C     Convert the observation time to seconds past J2000 TDB. */
/*        C */
/*              OBSTIM = '2003 OCT 13 06:00:00.000000 UTC' */

/*              CALL STR2ET ( OBSTIM, ET ) */

/*        C */
/*        C     Set the target, observer center, observer frame, and */
/*        C     observer position relative to its center. */
/*        C */
/*              TARGET = 'SUN' */
/*              OBSCTR = 'EARTH' */
/*              OBSREF = 'ITRF93' */

/*        C */
/*        C     Set the position of DSS-14 relative to the earth's */
/*        C     center at the J2000 epoch, expressed in the */
/*        C     ITRF93 reference frame. Values come from the */
/*        C     earth station SPK specified in the meta-kernel. */
/*        C */
/*        C     The actual station velocity is non-zero due */
/*        C     to tectonic plate motion; we ignore the motion */
/*        C     in this example. See the routine SPKCVO for an */
/*        C     example in which the plate motion is accounted for. */
/*        C */
/*              OBSPOS(1) =  -2353.6213656676991D0 */
/*              OBSPOS(2) =  -4641.3414911499403D0 */
/*              OBSPOS(3) =   3677.0523293197439D0 */

/*        C */
/*        C     Find the apparent state of the sun relative */
/*        C     to the station in the DSS-14_TOPO reference frame. */
/*        C     Evaluate the output frame's orientation, that is the */
/*        C     orientation of the DSS-14_TOPO frame relative to the */
/*        C     J2000 frame, at the observation epoch. This */
/*        C     correction is obtained by setting REFLOC to */
/*        C     'OBSERVER'. */
/*        C */
/*              OUTREF = 'DSS-14_TOPO' */
/*              ABCORR = 'CN+S' */

/*              REFLOC = 'OBSERVER' */

/*        C */
/*        C     Compute the observer-target state. */
/*        C */
/*              CALL SPKCPO ( TARGET, ET,     OUTREF, REFLOC, */
/*             .              ABCORR, OBSPOS, OBSCTR, */
/*             .              OBSREF, STATE0, LT0            ) */

/*        C */
/*        C     Compute planetocentric coordinates of the */
/*        C     observer-target position in the local */
/*        C     topocentric reference frame DSS-14_TOPO. */
/*        C */
/*              CALL RECLAT ( STATE0, R, LON, LAT ) */

/*        C */
/*        C     Compute solar azimuth. The latitude we've */
/*        C     already computed is the elevation. Express */
/*        C     both angles in degrees. */
/*        C */
/*              EL =   LAT * DPR() */
/*              AZ = - LON * DPR() */

/*              IF ( AZ .LT. 0.D0 ) THEN */
/*                 AZ = AZ + 360.D0 */
/*              END IF */

/*        C */
/*        C     Display the computed state, light time, and */
/*        C     angles. */
/*        C */
/*              CALL TIMOUT ( ET-LT0, TIMFMT, EMITIM ) */

/*              WRITE (*,*) ' ' */
/*              WRITE (*,*) 'Frame evaluation locus:     ', REFLOC */
/*              WRITE (*,*) ' ' */
/*              WRITE (*,*) 'Target:                     ', TARGET */
/*              WRITE (*,*) 'Observation time:           ', OBSTIM */
/*              WRITE (*,*) 'Observer center:            ', OBSCTR */
/*              WRITE (*,*) 'Observer frame:             ', OBSREF */
/*              WRITE (*,*) 'Emission time:              ', EMITIM */
/*              WRITE (*,*) 'Output reference frame:     ', OUTREF */
/*              WRITE (*,*) 'Aberration correction:      ', ABCORR */
/*              WRITE (*,*) ' ' */
/*              WRITE (*,*) 'Observer-target position (km):   ' */
/*              WRITE (*,FMT0) '   ', ( STATE0(I), I = 1, 3 ) */
/*              WRITE (*,*) 'Observer-target velocity (km/s): ' */
/*              WRITE (*,FMT0) '   ', ( STATE0(I), I = 4, 6 ) */
/*              WRITE (*,FMT1) 'Light time (s):        ', LT0 */
/*              WRITE (*,*) ' ' */
/*              WRITE (*,FMT1) 'Solar azimuth (deg):   ', AZ */
/*              WRITE (*,FMT1) 'Solar elevation (deg): ', EL */
/*              WRITE (*,*) ' ' */

/*        C */
/*        C     For an arbitrary surface point, we might not */
/*        C     have a frame kernel available. In this case */
/*        C     we can look up the state in the observer frame */
/*        C     using SPKCPO and then convert the state to */
/*        C     the local topocentric frame. We'll first */
/*        C     create the transformation matrix for converting */
/*        C     vectors in the observer frame to the topocentric */
/*        C     frame. */
/*        C */
/*        C     First step: find the geodetic (planetodetic) */
/*        C     coordinates of the observer. We need the */
/*        C     equatorial radius and flattening coefficient */
/*        C     of the reference ellipsoid. */
/*        C */
/*              CALL BODVRD ( 'EARTH', 'RADII', 3, N, RADII ) */

/*              RE = RADII(1) */
/*              RP = RADII(3) */

/*              F  = ( RE - RP ) / RE */

/*              CALL RECGEO ( OBSPOS, RE, F, OBSLON, OBSLAT, OBSALT ) */

/*        C */
/*        C     Find the outward surface normal on the reference */
/*        C     ellipsoid at the observer's longitude and latitude. */
/*        C */
/*              CALL LATREC ( 1.D0, OBSLON, OBSLAT, NORMAL ) */

/*        C */
/*        C     The topocentric frame has its +Z axis aligned */
/*        C     with NORMAL and its +X axis pointed north. */
/*        C     The north direction is aligned with the component */
/*        C     of the ITRF93 +Z axis orthogonal to the topocentric */
/*        C     +Z axis. */
/*        C */
/*              CALL TWOVEC ( NORMAL, 3, Z, 1, XFORM ) */

/*              OUTREF = 'ITRF93' */
/*              ABCORR = 'CN+S' */

/*              REFLOC = 'OBSERVER' */

/*        C */
/*        C     Compute the observer-target state. */
/*        C */
/*              CALL SPKCPO ( TARGET, ET,     OUTREF, REFLOC, */
/*             .              ABCORR, OBSPOS, OBSCTR, */
/*             .              OBSREF, STATE1, LT1            ) */

/*        C */
/*        C     Convert the position to the topocentric frame. */
/*        C */
/*              CALL MXV ( XFORM, STATE1, TOPVEC ) */

/*        C */
/*        C     Compute azimuth and elevation. */
/*        C */
/*              CALL RECLAT ( TOPVEC, R, LON, LAT ) */

/*              EL =   LAT * DPR() */
/*              AZ = - LON * DPR() */

/*              IF ( AZ .LT. 0.D0 ) THEN */
/*                 AZ = AZ + 360.D0 */
/*              END IF */

/*              WRITE (*,*) ' ' */
/*              WRITE (*,*) ' ' */
/*              WRITE (*,*) 'AZ/EL computed without frame kernel: ' */
/*              WRITE (*,*) ' ' */
/*              WRITE (*,FMT1) 'Distance between last two ' */
/*             .//             'positions (km): ', */
/*             .               VDIST ( STATE0, TOPVEC ) */
/*              WRITE (*,*) ' ' */
/*              WRITE (*,FMT1) 'Solar azimuth (deg):   ', AZ */
/*              WRITE (*,FMT1) 'Solar elevation (deg): ', EL */
/*              WRITE (*,*) ' ' */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Frame evaluation locus:     OBSERVER */

/*         Target:                     SUN */
/*         Observation time:           2003 OCT 13 06:00:00.000000 UTC */
/*         Observer center:            EARTH */
/*         Observer frame:             ITRF93 */
/*         Emission time:              2003 OCT 13 05:51:42.068322 UTC */
/*         Output reference frame:     DSS-14_TOPO */
/*         Aberration correction:      CN+S */

/*         Observer-target position (km): */
/*              62512272.82074845   58967494.42513601 -122059095.46751881 */
/*         Observer-target velocity (km/s): */
/*                  2475.97326517      -9870.26706232      -3499.90809969 */
/*         Light time (s):                497.93167797 */

/*         Solar azimuth (deg):           316.67141599 */
/*         Solar elevation (deg):         -54.85253168 */



/*         AZ/EL computed without frame kernel: */

/*         Distance between last two positions (km):           3.07056970 */

/*         Solar azimuth (deg):           316.67141786 */
/*         Solar elevation (deg):         -54.85253216 */


/*     2) Demonstrate applications of the output frame evaluation locus. */

/*        The following program is not necessarily realistic: for */
/*        brevity, it combines several unrelated computations. */

/*        Task Description */
/*        ================ */

/*        Find the state of the Mars Global Surveyor spacecraft, as seen */
/*        from a given surface point on earth, corrected for light time */
/*        and stellar aberration, expressed in the earth fixed reference */
/*        frame ITRF93. The surface point is the position of the DSN */
/*        station DSS-14. */

/*        Contrast the states computed by setting the output frame */
/*        evaluation locus to 'OBSERVER' and to 'CENTER'. Show that the */
/*        latter choice produces results very close to those that */
/*        can be obtained using SPKEZR. */

/*        Also compute the central meridian longitude on Mars of DSS-14. */
/*        This computation performs aberration corrections for the center */
/*        of Mars. */

/*        Note that in general, the routine SUBPNT should be used for */
/*        sub-observer point computations when high-accuracy aberration */
/*        corrections are desired. */

/*        The observation epoch is 2003 OCT 13 06:00:00 UTC. */


/*        Kernels */
/*        ======= */

/*        Use the meta-kernel of example 1 above. */


/*        Example code begins here. */


/*        C     Program: SPKCPO_EX2 */
/*        C */
/*        C     This program demonstrates the use of SPKCPO. */
/*        C     Computations are performed using all three possible */
/*        C     values of the output frame evaluation locus REFLOC: */
/*        C */
/*        C        'OBSERVER' */
/*        C        'CENTER' */
/*        C        'TARGET' */
/*        C */
/*        C     Several unrelated computations are performed in */
/*        C     this program. In particular, computation of the */
/*        C     central meridian longitude on Mars is included */
/*        C     simply to demonstrate use of the 'TARGET' option. */
/*        C */

/*              PROGRAM SPKCPO_EX2 */
/*              IMPLICIT NONE */
/*        C */
/*        C     SPICELIB functions */
/*        C */
/*              DOUBLE PRECISION      DPR */
/*              DOUBLE PRECISION      VDIST */

/*        C */
/*        C     Local parameters */
/*        C */
/*              CHARACTER*(*)         FMT0 */
/*              PARAMETER           ( FMT0   = '(A,3F20.8)' ) */

/*              CHARACTER*(*)         FMT1 */
/*              PARAMETER           ( FMT1   = '(1X,A, F20.8)' ) */

/*              CHARACTER*(*)         META */
/*              PARAMETER           ( META   = 'spkcpo_ex1.tm' ) */

/*              CHARACTER*(*)         TIMFMT */
/*              PARAMETER           ( TIMFMT = */
/*             .                    'YYYY MON DD HR:MN:SC.###### UTC' ) */

/*              INTEGER               BDNMLN */
/*              PARAMETER           ( BDNMLN = 36 ) */

/*              INTEGER               CORLEN */
/*              PARAMETER           ( CORLEN = 10 ) */

/*              INTEGER               EVLLEN */
/*              PARAMETER           ( EVLLEN = 25 ) */

/*              INTEGER               FRNMLN */
/*              PARAMETER           ( FRNMLN = 32 ) */

/*              INTEGER               TIMLEN */
/*              PARAMETER           ( TIMLEN = 40 ) */

/*        C */
/*        C     Local variables */
/*        C */
/*              CHARACTER*(CORLEN)    ABCORR */
/*              CHARACTER*(TIMLEN)    EMITIM */
/*              CHARACTER*(EVLLEN)    REFLOC */
/*              CHARACTER*(BDNMLN)    OBSRVR */
/*              CHARACTER*(TIMLEN)    OBSTIM */
/*              CHARACTER*(FRNMLN)    OUTREF */
/*              CHARACTER*(BDNMLN)    TARGET */
/*              CHARACTER*(BDNMLN)    OBSCTR */
/*              CHARACTER*(FRNMLN)    OBSREF */

/*              DOUBLE PRECISION      ET */
/*              DOUBLE PRECISION      LAT */
/*              DOUBLE PRECISION      LON */
/*              DOUBLE PRECISION      LT0 */
/*              DOUBLE PRECISION      LT1 */
/*              DOUBLE PRECISION      LT2 */
/*              DOUBLE PRECISION      LT3 */
/*              DOUBLE PRECISION      R */
/*              DOUBLE PRECISION      STATE0 ( 6 ) */
/*              DOUBLE PRECISION      STATE1 ( 6 ) */
/*              DOUBLE PRECISION      STATE2 ( 6 ) */
/*              DOUBLE PRECISION      STATE3 ( 6 ) */
/*              DOUBLE PRECISION      OBSPOS ( 3 ) */
/*              DOUBLE PRECISION      OBSVEC ( 3 ) */

/*              INTEGER               I */

/*        C */
/*        C     Load SPICE kernels. */
/*        C */
/*              CALL FURNSH ( META ) */

/*        C */
/*        C     Convert the observation time to seconds past J2000 TDB. */
/*        C */
/*              OBSTIM = '2003 OCT 13 06:00:00.000000 UTC' */

/*              CALL STR2ET ( OBSTIM, ET ) */

/*        C */
/*        C     Set the target, observer center, observer frame, and */
/*        C     observer position relative to its center. */
/*        C */
/*              TARGET = 'MGS' */
/*              OBSCTR = 'EARTH' */
/*              OBSREF = 'ITRF93' */

/*        C */
/*        C     Set the position of DSS-14 relative to the earth's */
/*        C     center at the J2000 epoch, expressed in the */
/*        C     ITRF93 reference frame. Values come from the */
/*        C     earth station SPK specified in the meta-kernel. */
/*        C */
/*        C     The actual station velocity is non-zero due */
/*        C     to tectonic plate motion; we ignore the motion */
/*        C     in this example. See the routine SPKCVT for an */
/*        C     example in which the plate motion is accounted for. */
/*        C */
/*              OBSPOS(1) =  -2353.6213656676991D0 */
/*              OBSPOS(2) =  -4641.3414911499403D0 */
/*              OBSPOS(3) =   3677.0523293197439D0 */

/*        C */
/*        C     Find the apparent state of the spacecraft relative */
/*        C     to the station in the ITRF93 reference frame. */
/*        C     Evaluate the earth's orientation, that is the */
/*        C     orientation of the ITRF93 frame relative to the */
/*        C     J2000 frame, at the observation epoch. This */
/*        C     correction is obtained by setting REFLOC to */
/*        C     'OBSERVER'. */
/*        C */
/*              OUTREF = 'ITRF93' */
/*              ABCORR = 'CN+S' */

/*              REFLOC = 'OBSERVER' */

/*        C */
/*        C     Compute the observer-target state. */
/*        C */
/*              CALL SPKCPO ( TARGET, ET,     OUTREF, REFLOC, */
/*             .              ABCORR, OBSPOS, OBSCTR, */
/*             .              OBSREF, STATE0, LT0            ) */

/*        C */
/*        C     Display the computed state and light time. */
/*        C */
/*              CALL TIMOUT ( ET-LT0, TIMFMT, EMITIM ) */

/*              WRITE (*,*) ' ' */
/*              WRITE (*,*) 'Frame evaluation locus:     ', REFLOC */
/*              WRITE (*,*) ' ' */
/*              WRITE (*,*) 'Target:                     ', TARGET */
/*              WRITE (*,*) 'Observation time:           ', OBSTIM */
/*              WRITE (*,*) 'Observer center:            ', OBSCTR */
/*              WRITE (*,*) 'Observer frame:             ', OBSREF */
/*              WRITE (*,*) 'Emission time:              ', EMITIM */
/*              WRITE (*,*) 'Output reference frame:     ', OUTREF */
/*              WRITE (*,*) 'Aberration correction:      ', ABCORR */
/*              WRITE (*,*) ' ' */
/*              WRITE (*,*) 'Observer-target position (km):   ' */
/*              WRITE (*,FMT0) '   ', ( STATE0(I), I = 1, 3 ) */
/*              WRITE (*,*) 'Observer-target velocity (km/s): ' */
/*              WRITE (*,FMT0) '   ', ( STATE0(I), I = 4, 6 ) */
/*              WRITE (*,FMT1) 'Light time (s):   ', LT0 */
/*              WRITE (*,*) ' ' */

/*        C */
/*        C     Repeat the computation, this time evaluating the */
/*        C     earth's orientation at the epoch obtained by */
/*        C     subtracting from the observation time the one way */
/*        C     light time from the earth's center. */
/*        C */
/*        C     This is equivalent to looking up the observer-target */
/*        C     state using SPKEZR. */
/*        C */
/*              REFLOC = 'CENTER' */

/*              CALL SPKCPO ( TARGET, ET,     OUTREF, REFLOC, */
/*             .              ABCORR, OBSPOS, OBSCTR, */
/*             .              OBSREF, STATE1, LT1            ) */

/*        C */
/*        C     Display the computed state and light time. */
/*        C */
/*              CALL TIMOUT ( ET-LT1, TIMFMT, EMITIM ) */

/*              WRITE (*,*) ' ' */
/*              WRITE (*,*) 'Frame evaluation locus:     ', REFLOC */
/*              WRITE (*,*) ' ' */
/*              WRITE (*,*) 'Target:                     ', TARGET */
/*              WRITE (*,*) 'Observation time:           ', OBSTIM */
/*              WRITE (*,*) 'Observer center:            ', OBSCTR */
/*              WRITE (*,*) 'Observer frame:             ', OBSREF */
/*              WRITE (*,*) 'Emission time:              ', EMITIM */
/*              WRITE (*,*) 'Output reference frame:     ', OUTREF */
/*              WRITE (*,*) 'Aberration correction:      ', ABCORR */
/*              WRITE (*,*) ' ' */
/*              WRITE (*,*) 'Observer-target position (km):   ' */
/*              WRITE (*,FMT0) '   ', ( STATE1(I), I = 1, 3 ) */
/*              WRITE (*,*) 'Observer-target velocity (km/s): ' */
/*              WRITE (*,FMT0) '   ', ( STATE1(I), I = 4, 6 ) */
/*              WRITE (*,FMT1) 'Light time (s):   ', LT1 */
/*              WRITE (*,*) ' ' */

/*              WRITE (*,FMT1) 'Distance between above positions ' */
/*             .//             '(km):    ',   VDIST( STATE0, STATE1 ) */
/*              WRITE (*,FMT1) 'Velocity difference magnitude ' */
/*             .//             ' (km/s):    ', */
/*             .               VDIST( STATE0(4), STATE1(4) ) */

/*        C */
/*        C     Check: compare the state computed directly above */
/*        C     to one produced by SPKEZR. */
/*        C */
/*              OBSRVR = 'DSS-14' */

/*              CALL SPKEZR ( TARGET, ET,     OUTREF, ABCORR, */
/*             .              OBSRVR, STATE2, LT2            ) */

/*              WRITE (*,*) ' ' */
/*              WRITE (*,*) ' ' */
/*              WRITE (*,*) 'State computed using SPKEZR: ' */
/*              WRITE (*,*) ' ' */
/*              WRITE (*,*) 'Target:                 ', TARGET */
/*              WRITE (*,*) 'Observation time:       ', OBSTIM */
/*              WRITE (*,*) 'Output reference frame: ', OUTREF */
/*              WRITE (*,*) 'Aberration correction:  ', ABCORR */
/*              WRITE (*,*) 'Observer:               ', OBSRVR */
/*              WRITE (*,*) ' ' */
/*              WRITE (*,*) 'Observer-target position (km):   ' */
/*              WRITE (*,FMT0) '   ', ( STATE2(I), I = 1, 3 ) */
/*              WRITE (*,*) 'Observer-target velocity (km/s): ' */
/*              WRITE (*,FMT0) '   ', ( STATE2(I), I = 4, 6 ) */
/*              WRITE (*,FMT1) 'Light time (s): ', LT2 */
/*              WRITE (*,*) ' ' */

/*              WRITE (*,FMT1) 'Distance between last two ' */
/*             .//             'positions (km): ', */
/*             .               VDIST ( STATE1, STATE2 ) */
/*              WRITE (*,FMT1) 'Velocity difference magnitude ' */
/*             .//             '    (km/s): ', */
/*             .               VDIST( STATE1(4), STATE2(4) ) */

/*        C */
/*        C     Finally, compute an observer-target state in */
/*        C     a frame centered at the target. This state */
/*        C     can be used to compute the sub-observer longitude. */
/*        C     The reference frame is the Mars-fixed frame IAU_MARS. */
/*        C */
/*              TARGET = 'MARS' */
/*              OUTREF = 'IAU_MARS' */

/*              REFLOC = 'TARGET' */

/*              CALL SPKCPO ( TARGET, ET,     OUTREF, REFLOC, */
/*             .              ABCORR, OBSPOS, OBSCTR, */
/*             .              OBSREF, STATE3, LT3            ) */

/*        C */
/*        C     Central meridian longitude is the longitude of the */
/*        C     observer relative to the target center, so we must */
/*        C     negate the position portion of the state we just */
/*        C     computed. */
/*        C */
/*              CALL VMINUS ( STATE3, OBSVEC ) */

/*              CALL RECLAT ( OBSVEC, R, LON, LAT ) */

/*              WRITE (*,*) ' ' */
/*              WRITE (*,*) ' ' */
/*              WRITE (*,*) 'Frame evaluation locus:     ', REFLOC */
/*              WRITE (*,*) ' ' */
/*              WRITE (*,*) 'Target:                     ', TARGET */
/*              WRITE (*,*) 'Observation time:           ', OBSTIM */
/*              WRITE (*,*) 'Observer center:            ', OBSCTR */
/*              WRITE (*,*) 'Observer frame:             ', OBSREF */
/*              WRITE (*,*) 'Emission time:              ', EMITIM */
/*              WRITE (*,*) 'Output reference frame:     ', OUTREF */
/*              WRITE (*,*) 'Aberration correction:      ', ABCORR */
/*              WRITE (*,*) ' ' */
/*              WRITE (*,*) 'Observer-target position (km):   ' */
/*              WRITE (*,FMT0) '   ', ( STATE3(I), I = 1, 3 ) */
/*              WRITE (*,*) 'Observer-target velocity (km/s): ' */
/*              WRITE (*,FMT0) '   ', ( STATE3(I), I = 4, 6 ) */
/*              WRITE (*,FMT1) 'Light time (s):   ', LT3 */
/*              WRITE (*,*) ' ' */
/*              WRITE (*,*) 'Central meridian ' */
/*              WRITE (*,FMT1) 'longitude (deg):  ', LON*DPR() */
/*              WRITE (*,*) ' ' */
/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Frame evaluation locus:     OBSERVER */

/*         Target:                     MGS */
/*         Observation time:           2003 OCT 13 06:00:00.000000 UTC */
/*         Observer center:            EARTH */
/*         Observer frame:             ITRF93 */
/*         Emission time:              2003 OCT 13 05:55:44.201144 UTC */
/*         Output reference frame:     ITRF93 */
/*         Aberration correction:      CN+S */

/*         Observer-target position (km): */
/*             -53720675.37947631  -51381249.05335969  -18838416.34718024 */
/*         Observer-target velocity (km/s): */
/*                 -3751.69274754       3911.73417167         -2.17503628 */
/*         Light time (s):           255.79885530 */


/*         Frame evaluation locus:     CENTER */

/*         Target:                     MGS */
/*         Observation time:           2003 OCT 13 06:00:00.000000 UTC */
/*         Observer center:            EARTH */
/*         Observer frame:             ITRF93 */
/*         Emission time:              2003 OCT 13 05:55:44.201144 UTC */
/*         Output reference frame:     ITRF93 */
/*         Aberration correction:      CN+S */

/*         Observer-target position (km): */
/*             -53720595.74385086  -51381332.31464963  -18838416.34738571 */
/*         Observer-target velocity (km/s): */
/*                 -3751.69880992       3911.72835653         -2.17503628 */
/*         Light time (s):           255.79885530 */

/*         Distance between above positions (km):            115.21404099 */
/*         Velocity difference magnitude  (km/s):              0.00840050 */


/*         State computed using SPKEZR: */

/*         Target:                 MGS */
/*         Observation time:       2003 OCT 13 06:00:00.000000 UTC */
/*         Output reference frame: ITRF93 */
/*         Aberration correction:  CN+S */
/*         Observer:               DSS-14 */

/*         Observer-target position (km): */
/*             -53720595.74378239  -51381332.31467460  -18838416.34737090 */
/*         Observer-target velocity (km/s): */
/*                 -3751.69880992       3911.72835653         -2.17503628 */
/*         Light time (s):         255.79885530 */

/*         Distance between last two positions (km):           0.00007437 */
/*         Velocity difference magnitude     (km/s):           0.00000000 */


/*         Frame evaluation locus:     TARGET */

/*         Target:                     MARS */
/*         Observation time:           2003 OCT 13 06:00:00.000000 UTC */
/*         Observer center:            EARTH */
/*         Observer frame:             ITRF93 */
/*         Emission time:              2003 OCT 13 05:55:44.201144 UTC */
/*         Output reference frame:     IAU_MARS */
/*         Aberration correction:      CN+S */

/*         Observer-target position (km): */
/*             -71445232.12770166    2312773.74175354   27766441.52048387 */
/*         Observer-target velocity (km/s): */
/*                   155.65895286       5061.78618477          5.09447030 */
/*         Light time (s):           255.79702283 */

/*         Central meridian */
/*         longitude (deg):           -1.85409037 */


/* $ Restrictions */

/*     1)  This routine may not be suitable for work with stars or other */
/*         objects having large distances from the observer, due to loss */
/*         of precision in position vectors. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     S.C. Krening       (JPL) */
/*     B.V. Semenov       (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 05-JUL-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/*        Corrected meta-kernel name in code examples. Modified code */
/*        examples output format for the solutions to fit within the */
/*        $Examples section without modifications. */

/* -    SPICELIB Version 1.0.0, 27-MAR-2012 (NJB) (SCK) (BVS) */

/* -& */
/* $ Index_Entries */

/*     state relative to constant_position_observer */
/*     state relative to constant_position surface_point */
/*     state relative to surface_point on extended_object */
/*     state relative to landmark on extended_object */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("SPKCPO", (ftnlen)6);

/*     Create a state vector for the observer. The velocity */
/*     portion of the state is zero. */

    vequ_(obspos, obssta);
    cleard_(&c__3, &obssta[3]);

/*     Set the observation epoch; the value is arbitrary, since */
/*     the observer's velocity is zero. */

    obsepc = 0.;

/*     Compute the observer-target state vector. */

    spkcvo_(target, et, outref, refloc, abcorr, obssta, &obsepc, obsctr, 
	    obsref, state, lt, target_len, outref_len, refloc_len, abcorr_len,
	     obsctr_len, obsref_len);
    chkout_("SPKCPO", (ftnlen)6);
    return 0;
} /* spkcpo_ */

