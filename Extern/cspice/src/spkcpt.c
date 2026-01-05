/* spkcpt.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__3 = 3;

/* $Procedure SPKCPT ( SPK, constant position target state ) */
/* Subroutine */ int spkcpt_(doublereal *trgpos, char *trgctr, char *trgref, 
	doublereal *et, char *outref, char *refloc, char *abcorr, char *
	obsrvr, doublereal *state, doublereal *lt, ftnlen trgctr_len, ftnlen 
	trgref_len, ftnlen outref_len, ftnlen refloc_len, ftnlen abcorr_len, 
	ftnlen obsrvr_len)
{
    extern /* Subroutine */ int vequ_(doublereal *, doublereal *), chkin_(
	    char *, ftnlen), cleard_(integer *, doublereal *);
    doublereal trgepc;
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    doublereal trgsta[6];
    extern /* Subroutine */ int spkcvt_(doublereal *, doublereal *, char *, 
	    char *, doublereal *, char *, char *, char *, char *, doublereal *
	    , doublereal *, ftnlen, ftnlen, ftnlen, ftnlen, ftnlen, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Return the state, relative to a specified observer, of a target */
/*     having constant position in a specified reference frame. The */
/*     target's position is provided by the calling program rather than */
/*     by loaded SPK files. */

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
/*     TRGPOS     I   Target position relative to center of motion. */
/*     TRGCTR     I   Center of motion of target. */
/*     TRGREF     I   Frame of target position. */
/*     ET         I   Observation epoch. */
/*     OUTREF     I   Reference frame of output state. */
/*     REFLOC     I   Output reference frame evaluation locus. */
/*     ABCORR     I   Aberration correction. */
/*     OBSRVR     I   Name of observing ephemeris object. */
/*     STATE      O   State of target with respect to observer. */
/*     LT         O   One way light time between target and */
/*                    observer. */

/* $ Detailed_Input */

/*     TRGPOS   is the fixed (constant) geometric position of a */
/*              target relative to its "center of motion" TRGCTR, */
/*              expressed in the reference frame TRGREF. */

/*              Units are always km. */


/*     TRGCTR   is the name of the center of motion of TRGPOS. The */
/*              ephemeris of TRGCTR is provided by loaded SPK files. */

/*              Optionally, you may supply the integer ID code for */
/*              the object as an integer string. For example both */
/*              'MOON' and '301' are legitimate strings that indicate */
/*              the moon is the center of motion. */

/*              Case and leading and trailing blanks are not */
/*              significant in the string TRGCTR. */


/*     TRGREF   is the name of the reference frame relative to which */
/*              the input position TRGPOS is expressed. The target has */
/*              constant position relative to its center of motion */
/*              in this reference frame. */

/*              Case and leading and trailing blanks are not */
/*              significant in the string TRGREF. */


/*     ET       is the ephemeris time at which the state of the */
/*              target relative to the observer is to be */
/*              computed. ET is expressed as seconds past J2000 TDB. */
/*              ET refers to time at the observer's location. */


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
/*                             be selected when OUTREF is TRGREF, */
/*                             the frame in which the target position */
/*                             is specified. */


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
/*                            Newtonian light time and stellar */
/*                            aberration corrections. */


/*              Neither special nor general relativistic effects are */
/*              accounted for in the aberration corrections applied */
/*              by this routine. */

/*              Case and leading and trailing blanks are not */
/*              significant in the string ABCORR. */


/*     OBSRVR   is the name of an observing body. Optionally, you */
/*              may supply the ID code of the object as an integer */
/*              string. For example, both 'EARTH' and '399' are */
/*              legitimate strings to supply to indicate the */
/*              observer is Earth. */

/*              Case and leading and trailing blanks are not */
/*              significant in the string OBSRVR. */

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

/*     1)  If either the name of the center of motion or the observer */
/*         cannot be translated to its NAIF ID code, an error is signaled */
/*         by a routine in the call tree of this routine. */

/*     2)  If the reference frame OUTREF is unrecognized, an error */
/*         is signaled by a routine in the call tree of this */
/*         routine. */

/*     3)  If the reference frame TRGREF is unrecognized, an error is */
/*         signaled by a routine in the call tree of this routine. */

/*     4)  If the frame evaluation locus REFLOC is not recognized, an */
/*         error is signaled by a routine in the call tree of this */
/*         routine. */

/*     5)  If the loaded kernels provide insufficient data to compute */
/*         the requested state vector, an error is signaled */
/*         by a routine in the call tree of this routine. */

/*     6)  If an error occurs while reading an SPK or other kernel file, */
/*         the error is signaled by a routine in the call tree of */
/*         this routine. */

/*     7)  If the aberration correction ABCORR is not recognized, an */
/*         error is signaled by a routine in the call tree of this */
/*         routine. */

/* $ Files */

/*     Appropriate kernels must be loaded by the calling program before */
/*     this routine is called. */

/*     The following data are required: */

/*     -  SPK data: ephemeris data for target center and observer */
/*        must be loaded. If aberration corrections are used, the */
/*        states of target center and observer relative to the solar */
/*        system barycenter must be calculable from the available */
/*        ephemeris data. Typically ephemeris data are made available */
/*        by loading one or more SPK files using FURNSH. */

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

/*     This routine computes observer-target states for targets whose */
/*     trajectories are not provided by SPK files. */

/*     Targets supported by this routine must have constant position */
/*     with respect to a specified center of motion, expressed in a */
/*     caller-specified reference frame. The state of the center of */
/*     motion relative to the observer must be computable using */
/*     loaded SPK data. */

/*     For applications in which the target has non-zero, constant */
/*     velocity relative to its center of motion, the SPICELIB routine */

/*        SPKCVT     { SPK, constant velocity target } */

/*     can be used. */

/*     This routine is suitable for computing states of landmarks on the */
/*     surface of an extended object, as seen by a specified observer, */
/*     in cases where no SPK data are available for those landmarks. */

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
/*        the target itself. */

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

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Demonstrate use of this routine; in particular demonstrate */
/*        applications of the output frame evaluation locus. */

/*        The following program is not necessarily realistic: for */
/*        brevity, it combines several unrelated computations. */

/*        Task Description */
/*        ================ */

/*        Find the state of a given surface point on earth, corrected */
/*        for light time and stellar aberration, relative to the Mars */
/*        Global Surveyor spacecraft, expressed in the earth fixed */
/*        reference frame ITRF93. The selected point is the position */
/*        of the DSN station DSS-14. */

/*        Contrast the states computed by setting the output frame */
/*        evaluation locus to 'TARGET' and to 'CENTER'. Show that the */
/*        latter choice produces results very close to those that */
/*        can be obtained using SPKEZR. */

/*        Also compute the state of a selected Mars surface point as */
/*        seen from MGS. The point we'll use is the narrow angle MOC */
/*        boresight surface intercept corresponding to the chosen */
/*        observation time. Express the state in a spacecraft-centered */
/*        reference frame. Use the output frame evaluation locus */
/*        'OBSERVER' for this computation. */

/*        The observation epoch is 2003 OCT 13 06:00:00 UTC. */


/*        Kernels */
/*        ======= */

/*        Use the meta-kernel shown below to load the required SPICE */
/*        kernels. */


/*           KPL/MK */

/*           File name: spkcpt_ex1.tm */

/*           This is the meta-kernel file for the header code example for */
/*           the subroutine SPKCPT. The kernel files referenced by this */
/*           meta-kernel can be found on the NAIF website. */

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
/*                               'mgs_moc_v20.ti', */
/*                               'mgs_sclkscet_00061.tsc', */
/*                               'mgs_sc_ext12.bc', */
/*                               'mgs_ext12_ipng_mgs95j.bsp'  ) */

/*           \begintext */

/*           End of meta-kernel. */


/*        Example code begins here. */


/*        C */
/*        C     Program: SPKCPT_EX1 */
/*        C */
/*        C     This program demonstrates the use of SPKCPT. */
/*        C     Computations are performed using all three possible */
/*        C     values of the output frame evaluation locus REFLOC: */
/*        C */
/*        C        'TARGET' */
/*        C        'OBSERVER' */
/*        C        'CENTER' */
/*        C */
/*        C     Several unrelated computations are performed in */
/*        C     this program. In particular, computations */
/*        C     involving a surface point on Mars are included */
/*        C     simply to demonstrate use of the 'OBSERVER' */
/*        C     option. */
/*        C */


/*              PROGRAM SPKCPT_EX1 */
/*              IMPLICIT NONE */
/*        C */
/*        C     SPICELIB functions */
/*        C */
/*              DOUBLE PRECISION      VDIST */
/*              DOUBLE PRECISION      VNORM */

/*        C */
/*        C     Local parameters */
/*        C */
/*              CHARACTER*(*)         CAMERA */
/*              PARAMETER           ( CAMERA = 'MGS_MOC_NA' ) */

/*              CHARACTER*(*)         FMT0 */
/*              PARAMETER           ( FMT0   = '(A,3F20.8)' ) */

/*              CHARACTER*(*)         FMT1 */
/*              PARAMETER           ( FMT1   = '(1X,A, F20.8)' ) */

/*              CHARACTER*(*)         META */
/*              PARAMETER           ( META   = 'spkcpt_ex1.tm' ) */

/*              CHARACTER*(*)         TIMFMT */
/*              PARAMETER           ( TIMFMT = */
/*             .                    'YYYY MON DD HR:MN:SC.###### UTC' ) */


/*              INTEGER               BDNMLN */
/*              PARAMETER           ( BDNMLN = 36 ) */

/*              INTEGER               CORLEN */
/*              PARAMETER           ( CORLEN = 10 ) */

/*              INTEGER               LOCLEN */
/*              PARAMETER           ( LOCLEN = 25 ) */

/*              INTEGER               FRNMLN */
/*              PARAMETER           ( FRNMLN = 32 ) */

/*              INTEGER               MAXBND */
/*              PARAMETER           ( MAXBND = 10 ) */

/*              INTEGER               SHPLEN */
/*              PARAMETER           ( SHPLEN = 80 ) */

/*              INTEGER               TIMLEN */
/*              PARAMETER           ( TIMLEN = 40 ) */

/*        C */
/*        C     Local variables */
/*        C */
/*              CHARACTER*(CORLEN)    ABCORR */
/*              CHARACTER*(FRNMLN)    CAMREF */
/*              CHARACTER*(TIMLEN)    EMITIM */
/*              CHARACTER*(LOCLEN)    REFLOC */
/*              CHARACTER*(BDNMLN)    OBSRVR */
/*              CHARACTER*(TIMLEN)    OBSTIM */
/*              CHARACTER*(FRNMLN)    OUTREF */
/*              CHARACTER*(SHPLEN)    SHAPE */
/*              CHARACTER*(BDNMLN)    TARGET */
/*              CHARACTER*(BDNMLN)    TRGCTR */
/*              CHARACTER*(FRNMLN)    TRGREF */

/*              DOUBLE PRECISION      BOUNDS ( 3, MAXBND ) */
/*              DOUBLE PRECISION      BSIGHT ( 3 ) */
/*              DOUBLE PRECISION      ET */
/*              DOUBLE PRECISION      LT0 */
/*              DOUBLE PRECISION      LT1 */
/*              DOUBLE PRECISION      LT2 */
/*              DOUBLE PRECISION      LT3 */
/*              DOUBLE PRECISION      SPOINT ( 3 ) */
/*              DOUBLE PRECISION      SRFVEC ( 3 ) */
/*              DOUBLE PRECISION      STATE0 ( 6 ) */
/*              DOUBLE PRECISION      STATE1 ( 6 ) */
/*              DOUBLE PRECISION      STATE2 ( 6 ) */
/*              DOUBLE PRECISION      STATE3 ( 6 ) */
/*              DOUBLE PRECISION      TRGEPC */
/*              DOUBLE PRECISION      TRGPOS ( 3 ) */

/*              INTEGER               CAMID */
/*              INTEGER               I */
/*              INTEGER               N */

/*              LOGICAL               FOUND */

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
/*        C     Set the observer, target center, target frame, and */
/*        C     target state relative to its center. */
/*        C */
/*              OBSRVR = 'MGS' */
/*              TRGCTR = 'EARTH' */
/*              TRGREF = 'ITRF93' */

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
/*              TRGPOS(1) =  -2353.6213656676991D0 */
/*              TRGPOS(2) =  -4641.3414911499403D0 */
/*              TRGPOS(3) =   3677.0523293197439D0 */

/*        C */
/*        C     Find the apparent state of the station relative */
/*        C     to the spacecraft in the ITRF93 reference frame. */
/*        C     Evaluate the earth's orientation, that is the */
/*        C     orientation of the ITRF93 frame relative to the */
/*        C     J2000 frame, at the epoch obtained by correcting */
/*        C     the observation time for one-way light time. This */
/*        C     correction is obtained by setting REFLOC to 'TARGET'. */
/*        C */
/*              OUTREF = 'ITRF93' */
/*              ABCORR = 'CN+S' */

/*              REFLOC = 'TARGET' */

/*        C */
/*        C     Compute the observer-target state. */
/*        C */
/*              CALL SPKCPT ( TRGPOS, TRGCTR, TRGREF, */
/*             .              ET,     OUTREF, REFLOC, ABCORR, */
/*             .              OBSRVR, STATE0, LT0            ) */

/*        C */
/*        C     Display the computed state and light time. */
/*        C */
/*              CALL TIMOUT ( ET-LT0, TIMFMT, EMITIM ) */

/*              WRITE (*,*) ' ' */
/*              WRITE (*,*) 'Frame evaluation locus:   ', REFLOC */
/*              WRITE (*,*) ' ' */
/*              WRITE (*,*) 'Observer:                 ', OBSRVR */
/*              WRITE (*,*) 'Observation time:         ', OBSTIM */
/*              WRITE (*,*) 'Target center:            ', TRGCTR */
/*              WRITE (*,*) 'Target frame:             ', TRGREF */
/*              WRITE (*,*) 'Emission time:            ', EMITIM */
/*              WRITE (*,*) 'Output reference frame:   ', OUTREF */
/*              WRITE (*,*) 'Aberration correction:    ', ABCORR */
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

/*              CALL SPKCPT ( TRGPOS, TRGCTR, TRGREF, */
/*             .              ET,     OUTREF, REFLOC, ABCORR, */
/*             .              OBSRVR, STATE1, LT1            ) */

/*        C */
/*        C     Display the computed state and light time. */
/*        C */
/*              CALL TIMOUT ( ET-LT1, TIMFMT, EMITIM ) */

/*              WRITE (*,*) ' ' */
/*              WRITE (*,*) 'Frame evaluation locus:   ', REFLOC */
/*              WRITE (*,*) ' ' */
/*              WRITE (*,*) 'Observer:                 ', OBSRVR */
/*              WRITE (*,*) 'Observation time:         ', OBSTIM */
/*              WRITE (*,*) 'Target center:            ', TRGCTR */
/*              WRITE (*,*) 'Target frame:             ', TRGREF */
/*              WRITE (*,*) 'Emission time:            ', EMITIM */
/*              WRITE (*,*) 'Output reference frame:   ', OUTREF */
/*              WRITE (*,*) 'Aberration correction:    ', ABCORR */
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
/*              TARGET = 'DSS-14' */

/*              CALL SPKEZR ( TARGET, ET,     OUTREF, ABCORR, */
/*             .              OBSRVR, STATE2, LT2            ) */

/*              WRITE (*,*) ' ' */
/*              WRITE (*,*) ' ' */
/*              WRITE (*,*) 'State computed using SPKEZR: ' */
/*              WRITE (*,*) ' ' */
/*              WRITE (*,*) 'Observer:               ', OBSRVR */
/*              WRITE (*,*) 'Observation time:       ', OBSTIM */
/*              WRITE (*,*) 'Target:                 ', TARGET */
/*              WRITE (*,*) 'Output reference frame: ', OUTREF */
/*              WRITE (*,*) 'Aberration correction:  ', ABCORR */
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
/*        C     a frame centered at the observer. */
/*        C     The reference frame will be that of the */
/*        C     MGS MOC NA camera. */
/*        C */
/*        C     In this case we'll use as the target the surface */
/*        C     intercept on Mars of the camera boresight. This */
/*        C     allows us to easily verify the correctness of */
/*        C     the results returned by SPKCPT. */
/*        C */
/*        C     Get camera frame and FOV parameters. We'll need */
/*        C     the camera ID code first. */
/*        C */
/*              CALL BODN2C ( CAMERA, CAMID, FOUND ) */

/*              IF ( .NOT. FOUND ) THEN */

/*                 WRITE (*,*) 'Camera name could not be mapped ' */
/*             .   //          'to an ID code.' */
/*                 STOP */

/*              END IF */

/*        C */
/*        C     GETFOV will return the name of the camera-fixed frame */
/*        C     in the string CAMREF, the camera boresight vector in */
/*        C     the array BSIGHT, and the FOV corner vectors in the */
/*        C     array BOUNDS. All we're going to use are the camera */
/*        C     frame name and camera boresight. */
/*        C */
/*              CALL GETFOV ( CAMID,  MAXBND, SHAPE,  CAMREF, */
/*             .              BSIGHT, N,      BOUNDS         ) */

/*        C */
/*        C     Find the camera boresight surface intercept. */
/*        C */
/*              TRGCTR = 'MARS' */
/*              TRGREF = 'IAU_MARS' */

/*              CALL SINCPT ( 'ELLIPSOID', TRGCTR, ET,     TRGREF, */
/*             .              ABCORR,      OBSRVR, CAMREF, BSIGHT, */
/*             .              SPOINT,      TRGEPC, SRFVEC, FOUND  ) */


/*              OUTREF = CAMREF */

/*              REFLOC = 'OBSERVER' */

/*              CALL SPKCPT ( SPOINT, TRGCTR, TRGREF, */
/*             .              ET,     OUTREF, REFLOC, ABCORR, */
/*             .              OBSRVR, STATE3, LT3             ) */

/*        C */
/*        C     Convert the emission time and the target state */
/*        C     evaluation epoch to strings for output. */
/*        C */
/*              CALL TIMOUT ( ET - LT3, TIMFMT, EMITIM ) */

/*              WRITE (*,*) ' ' */
/*              WRITE (*,*) ' ' */
/*              WRITE (*,*) 'Frame evaluation locus:   ', REFLOC */
/*              WRITE (*,*) ' ' */
/*              WRITE (*,*) 'Observer:                 ', OBSRVR */
/*              WRITE (*,*) 'Observation time:         ', OBSTIM */
/*              WRITE (*,*) 'Target center:            ', TRGCTR */
/*              WRITE (*,*) 'Target frame:             ', TRGREF */
/*              WRITE (*,*) 'Emission time:            ', EMITIM */
/*              WRITE (*,*) 'Output reference frame:   ', OUTREF */
/*              WRITE (*,*) 'Aberration correction:    ', ABCORR */
/*              WRITE (*,*) ' ' */
/*              WRITE (*,*) 'Observer-target position (km):   ' */
/*              WRITE (*,FMT0) '   ', ( STATE3(I), I = 1, 3 ) */
/*              WRITE (*,*) 'Observer-target velocity (km/s): ' */
/*              WRITE (*,FMT0) '   ', ( STATE3(I), I = 4, 6 ) */
/*              WRITE (*,FMT1) 'Light time (s): ', LT3 */
/*              WRITE (*,FMT1) 'Target range from SINCPT (km): ' */
/*             .//             '           ',    VNORM( SRFVEC ) */
/*              WRITE (*,*) ' ' */
/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Frame evaluation locus:   TARGET */

/*         Observer:                 MGS */
/*         Observation time:         2003 OCT 13 06:00:00.000000 UTC */
/*         Target center:            EARTH */
/*         Target frame:             ITRF93 */
/*         Emission time:            2003 OCT 13 05:55:44.232914 UTC */
/*         Output reference frame:   ITRF93 */
/*         Aberration correction:    CN+S */

/*         Observer-target position (km): */
/*              52746468.84243592   52367725.79653772   18836142.68957234 */
/*         Observer-target velocity (km/s): */
/*                  3823.39593314      -3840.60002121          2.21337692 */
/*         Light time (s):           255.76708533 */


/*         Frame evaluation locus:   CENTER */

/*         Observer:                 MGS */
/*         Observation time:         2003 OCT 13 06:00:00.000000 UTC */
/*         Target center:            EARTH */
/*         Target frame:             ITRF93 */
/*         Emission time:            2003 OCT 13 05:55:44.232914 UTC */
/*         Output reference frame:   ITRF93 */
/*         Aberration correction:    CN+S */

/*         Observer-target position (km): */
/*              52746419.34648802   52367775.65036674   18836142.68969753 */
/*         Observer-target velocity (km/s): */
/*                  3823.40103499      -3840.59789000          2.21337692 */
/*         Light time (s):           255.76708533 */

/*         Distance between above positions (km):             70.25135676 */
/*         Velocity difference magnitude  (km/s):              0.00552910 */


/*         State computed using SPKEZR: */

/*         Observer:               MGS */
/*         Observation time:       2003 OCT 13 06:00:00.000000 UTC */
/*         Target:                 DSS-14 */
/*         Output reference frame: ITRF93 */
/*         Aberration correction:  CN+S */

/*         Observer-target position (km): */
/*              52746419.34641990   52367775.65039122   18836142.68968301 */
/*         Observer-target velocity (km/s): */
/*                  3823.40103499      -3840.59789000          2.21337692 */
/*         Light time (s):         255.76708533 */

/*         Distance between last two positions (km):           0.00007383 */
/*         Velocity difference magnitude     (km/s):           0.00000000 */


/*         Frame evaluation locus:   OBSERVER */

/*         Observer:                 MGS */
/*         Observation time:         2003 OCT 13 06:00:00.000000 UTC */
/*         Target center:            MARS */
/*         Target frame:             IAU_MARS */
/*         Emission time:            2003 OCT 13 05:59:59.998702 UTC */
/*         Output reference frame:   MGS_MOC_NA */
/*         Aberration correction:    CN+S */

/*         Observer-target position (km): */
/*                     0.00000001         -0.00000001        388.97573572 */
/*         Observer-target velocity (km/s): */
/*                     2.91968665          0.15140014          0.92363513 */
/*         Light time (s):           0.00129748 */
/*         Target range from SINCPT (km):                    388.97573572 */


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

/* -    SPICELIB Version 1.0.1, 25-MAY-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/*        Modified code example output format for the solution to fit */
/*        within the $Examples section without modifications. */

/* -    SPICELIB Version 1.0.0, 27-MAR-2012 (NJB) (SCK) (BVS) */

/* -& */
/* $ Index_Entries */

/*     state of constant_position_target */
/*     state of fixed_position_target */
/*     state of surface_point on extended_object */
/*     state of landmark on extended_object */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("SPKCPT", (ftnlen)6);

/*     Create a state vector for the target. The velocity */
/*     portion of the state is zero. */

    vequ_(trgpos, trgsta);
    cleard_(&c__3, &trgsta[3]);

/*     Set the target epoch; the value is arbitrary, since */
/*     the target's velocity is zero. */

    trgepc = 0.;
    spkcvt_(trgsta, &trgepc, trgctr, trgref, et, outref, refloc, abcorr, 
	    obsrvr, state, lt, trgctr_len, trgref_len, outref_len, refloc_len,
	     abcorr_len, obsrvr_len);
    chkout_("SPKCPT", (ftnlen)6);
    return 0;
} /* spkcpt_ */

