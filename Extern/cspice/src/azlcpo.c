/* azlcpo.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__9 = 9;
static integer c__3 = 3;

/* $Procedure AZLCPO ( AZ/EL, constant position observer state ) */
/* Subroutine */ int azlcpo_(char *method, char *target, doublereal *et, char 
	*abcorr, logical *azccw, logical *elplsz, doublereal *obspos, char *
	obsctr, char *obsref, doublereal *azlsta, doublereal *lt, ftnlen 
	method_len, ftnlen target_len, ftnlen abcorr_len, ftnlen obsctr_len, 
	ftnlen obsref_len)
{
    /* Initialized data */

    static doublereal z__[3] = { 0.,0.,1. };

    /* System generated locals */
    doublereal d__1;

    /* Local variables */
    extern /* Subroutine */ int zzgftreb_(integer *, doublereal *);
    doublereal radii[3];
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen), ident_(doublereal *), moved_(doublereal *, 
	    integer *, doublereal *);
    doublereal lhsta[6];
    logical found;
    doublereal state[6];
    extern logical eqstr_(char *, char *, ftnlen, ftnlen);
    extern /* Subroutine */ int bods2c_(char *, integer *, logical *, ftnlen);
    extern logical failed_(void);
    doublereal jacobi[9]	/* was [3][3] */;
    extern doublereal pi_(void);
    integer fxfcde, obscde, center, clssid;
    extern /* Subroutine */ int namfrm_(char *, integer *, ftnlen), frinfo_(
	    integer *, integer *, integer *, integer *, logical *), recazl_(
	    doublereal *, logical *, logical *, doublereal *, doublereal *, 
	    doublereal *);
    doublereal normal[3];
    extern /* Subroutine */ int nearpt_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *), sigerr_(
	    char *, ftnlen);
    integer frclss;
    extern /* Subroutine */ int chkout_(char *, ftnlen), spkcpo_(char *, 
	    doublereal *, char *, char *, char *, doublereal *, char *, char *
	    , doublereal *, doublereal *, ftnlen, ftnlen, ftnlen, ftnlen, 
	    ftnlen, ftnlen), dazldr_(doublereal *, doublereal *, doublereal *,
	     logical *, logical *, doublereal *);
    doublereal tmpmat[9]	/* was [3][3] */;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen), rotmat_(doublereal *, doublereal *, integer *,
	     doublereal *), twovec_(doublereal *, integer *, doublereal *, 
	    integer *, doublereal *);
    doublereal obsspt[3];
    extern /* Subroutine */ int surfnm_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *);
    extern logical return_(void);
    doublereal xftopo[9]	/* was [3][3] */, alt;
    extern /* Subroutine */ int mxv_(doublereal *, doublereal *, doublereal *)
	    ;

/* $ Abstract */

/*     Return the azimuth/elevation coordinates of a specified target */
/*     relative to an "observer," where the observer has constant */
/*     position in a specified reference frame. The observer's position */
/*     is provided by the calling program rather than by loaded SPK */
/*     files. */

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

/*     COORDINATES */
/*     EPHEMERIS */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     METHOD     I   Method to obtain the surface normal vector. */
/*     TARGET     I   Name of target ephemeris object. */
/*     ET         I   Observation epoch. */
/*     ABCORR     I   Aberration correction. */
/*     AZCCW      I   Flag indicating how azimuth is measured. */
/*     ELPLSZ     I   Flag indicating how elevation is measured. */
/*     OBSPOS     I   Observer position relative to center of motion. */
/*     OBSCTR     I   Center of motion of observer. */
/*     OBSREF     I   Body-fixed, body-centered frame of observer's */
/*                    center. */
/*     AZLSTA     O   State of target with respect to observer, */
/*                    in azimuth/elevation coordinates. */
/*     LT         O   One way light time between target and */
/*                    observer. */

/* $ Detailed_Input */

/*     METHOD   is a short string providing parameters defining the */
/*              computation method to be used to obtain the surface */
/*              normal vector that defines the local zenith. Parameters */
/*              include, but are not limited to, the shape model used to */
/*              represent the body's surface of observer's center of */
/*              motion. */

/*              The only choice currently supported is */

/*                 'ELLIPSOID'        The intercept computation uses */
/*                                    a triaxial ellipsoid to model */
/*                                    the body's surface of the */
/*                                    observer's center of motion. */
/*                                    The ellipsoid's radii must be */
/*                                    available in the kernel pool. */

/*              Neither case nor white space are significant in */
/*              METHOD. For example, the string ' eLLipsoid ' is */
/*              valid. */

/*              In a later Toolkit release, this argument will be */
/*              used to invoke a wider range of surface */
/*              representations. For example, it will be possible to */
/*              represent the target body's surface using a digital */
/*              shape model. */

/*     TARGET   is the name of a target body. Optionally, you may supply */
/*              the ID code of the object as an integer string. For */
/*              example, both 'EARTH' and '399' are legitimate strings */
/*              to supply to indicate the target is Earth. */

/*              Case and leading and trailing blanks are not significant */
/*              in the string TARGET. */

/*     ET       is the ephemeris time at which the state of the */
/*              target relative to the observer is to be computed. ET */
/*              is expressed as seconds past J2000 TDB. ET refers to */
/*              time at the observer's location. */

/*     ABCORR   is a short string that indicates the aberration */
/*              corrections to be applied to the observer-target state */
/*              to account for one-way light time and stellar */
/*              aberration. */

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

/*     AZCCW    is a flag indicating how the azimuth is measured. */

/*              If AZCCW is .TRUE., the azimuth increases in the */
/*              counterclockwise direction; otherwise it increases */
/*              in the clockwise direction. */

/*     ELPLSZ   is a flag indicating how the elevation is measured. */

/*              If ELPLSZ is .TRUE., the elevation increases from */
/*              the XY plane toward +Z; otherwise toward -Z. */

/*     OBSPOS   is the fixed (constant) geometric position of an */
/*              observer relative to its center of motion OBSCTR, */
/*              expressed in the reference frame OBSREF. */

/*              OBSPOS does not need to be located on the surface of */
/*              the object centered at OBSCTR. */

/*              Units are always km. */

/*     OBSCTR   is the name of the center of motion of OBSPOS. The */
/*              ephemeris of OBSCTR is provided by loaded SPK files. */

/*              Optionally, you may supply the integer ID code for the */
/*              object as an integer string. For example both 'MOON' and */
/*              '301' are legitimate strings that indicate the moon is */
/*              the center of motion. */

/*              Case and leading and trailing blanks are not significant */
/*              in the string OBSCTR. */

/*     OBSREF   is the name of the body-fixed, body-centered reference */
/*              frame associated with the observer's center of motion, */
/*              relative to which the input position OBSPOS is */
/*              expressed. The observer has constant position relative */
/*              to its center of motion in this reference frame. */

/*              Case and leading and trailing blanks are not significant */
/*              in the string OBSREF. */

/* $ Detailed_Output */

/*     AZLSTA   is a state vector representing the position and */
/*              velocity of the target relative to the specified */
/*              observer, corrected for the specified aberrations */
/*              and expressed in azimuth/elevation coordinates. The */
/*              first three components of AZLSTA represent the range, */
/*              azimuth and elevation of the target's position; the */
/*              last three components form the corresponding velocity */
/*              vector: */

/*                 AZLSTA = ( R, AZ, EL, dR/dt, dAZ/dt, dEL/dt ) */

/*              The position component of AZLSTA points from the */
/*              observer's location at ET to the aberration-corrected */
/*              location of the target. Note that the sense of the */
/*              position vector is independent of the direction of */
/*              radiation travel implied by the aberration correction. */

/*              The velocity component of AZLSTA is the derivative with */
/*              respect to time of the position component of AZLSTA. */

/*              Azimuth, elevation and its derivatives are measured with */
/*              respect to the axes of the local topocentric reference */
/*              frame. See the $Particulars section for the definition */
/*              of this reference frame. */

/*              The azimuth is the angle between the projection onto the */
/*              local topocentric principal (X-Y) plane of the vector */
/*              from the observer's position to the target and the */
/*              principal axis of the reference frame. The azimuth is */
/*              zero on the +X axis. */

/*              The elevation is the angle between the vector from the */
/*              observer's position to the target and the local */
/*              topocentric principal plane. The elevation is zero on */
/*              the plane. */

/*              Units are km for R, radians for AZ and EL, km/sec for */
/*              dR/dt, and radians/sec for dAZ/dt and dEL/dt. The range */
/*              of AZ is [0, 2*pi] and the range of EL is [-pi/2, pi/2]. */

/*              The way azimuth and elevation are measured depend */
/*              respectively on the value of the logical flags AZCCW and */
/*              ELPLSZ. See the description of these input arguments for */
/*              details. */

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

/*     2)  If the reference frame OBSREF is not recognized, the error */
/*         SPICE(UNKNOWNFRAME) is signaled. A frame name may fail to be */
/*         recognized because a required frame specification kernel has */
/*         not been loaded; another cause is a misspelling of the frame */
/*         name. */

/*     3)  If the reference frame OBSREF is not centered at the */
/*         observer's center of motion OBSCTR, the error */
/*         SPICE(INVALIDFRAME) is signaled. */

/*     4)  If the radii of the center of motion body are not available */
/*         from the kernel pool, an error is signaled by a routine in */
/*         the call tree of this routine. */

/*     5)  If the size of the OBSCTR body radii kernel variable is not */
/*         three, an error is signaled by a routine in the call tree of */
/*         this routine. */

/*     6)  If any of the three OBSCTR body radii is less-than or equal to */
/*         zero, an error is signaled by a routine in the call tree of */
/*         this routine. */

/*     7)  If the ratio of the longest to the shortest */
/*         radii is large enough so that arithmetic expressions */
/*         involving its squared value may overflow, an error is */
/*         signaled by a routine in the call tree of this routine. */

/*     8)  If the radii of the center of motion body and the axes of */
/*         OBSPOS have radically different magnitudes so that arithmetic */
/*         overflow may occur during the computation of the nearest */
/*         point of the observer on the center of motion's reference */
/*         ellipsoid, an error is signaled by a routine in the call tree */
/*         of this routine. Note that even if there is no overflow, if */
/*         the ratios of the radii lengths, or the ratio of the */
/*         magnitude of OBSPOS and the shortest radius vary by many */
/*         orders of magnitude, the results may have poor precision. */

/*     9)  If the computation METHOD is not recognized, the error */
/*         SPICE(INVALIDMETHOD) is signaled. */

/*     10) If the loaded kernels provide insufficient data to compute */
/*         the requested state vector, an error is signaled by a routine */
/*         in the call tree of this routine. */

/*     11) If an error occurs while reading an SPK or other kernel file, */
/*         the error  is signaled by a routine in the call tree of this */
/*         routine. */

/*     12) If the aberration correction ABCORR is not recognized, an */
/*         error is signaled by a routine in the call tree of this */
/*         routine. */

/*     13) If TARGET is on the Z-axis ( X = 0 and Y = 0 ) of the local */
/*         topocentric frame centered at OBSPOS, an error is signaled by */
/*         a routine in the call tree of this routine. See item 2 in the */
/*         $Restrictions section for further details. */

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

/*     -  Shape and orientation data: if the computation method is */
/*        specified as "Ellipsoid," triaxial radii for the center body */
/*        must be loaded into the kernel pool. Typically this is done by */
/*        loading a text PCK file via FURNSH. Additionally, rotation */
/*        data for the body-fixed, body-centered frame associated with */
/*        the observer's center of motion must be loaded. These may be */
/*        provided in a text or binary PCK file. In some cases these */
/*        data may be provided by a CK file. */

/*     The following data may be required: */

/*     -  Frame data: if a frame definition not built into SPICE is */
/*        required, for example to convert the observer-target state */
/*        to the body-fixed body-centered frame, that definition */
/*        must be available in the kernel pool. Typically frame */
/*        definitions are supplied by loading a frame kernel using */
/*        FURNSH. */

/*     -  Additional kernels: if a CK frame is used in this routine's */
/*        state computation, then at least one CK and corresponding SCLK */
/*        kernel is required. If dynamic frames are used, additional */
/*        SPK, PCK, CK, or SCLK kernels may be required. */

/*     In all cases, kernel data are normally loaded once per program */
/*     run, NOT every time this routine is called. */

/* $ Particulars */

/*     This routine computes azimuth/elevation coordinates of a target */
/*     as seen from an observer whose trajectory is not provided by SPK */
/*     files. */

/*     Observers supported by this routine must have constant position */
/*     with respect to a specified center of motion, expressed in a */
/*     caller-specified reference frame. The state of the center of */
/*     motion relative to the target must be computable using */
/*     loaded SPK data. */

/*     This routine is suitable for computing the azimuth/elevation */
/*     coordinates and its derivatives of target ephemeris */
/*     objects, as seen from landmarks on the surface of an extended */
/*     object, in cases where no SPK data are available for those */
/*     landmarks. */

/*     The azimuth/elevation coordinates are defined with respect to */
/*     the observer's local topocentric reference frame. This frame is */
/*     generally defined as follows: */

/*     -  the +Z axis is aligned with the surface normal outward */
/*        vector at the observer's location; */

/*     -  the +X axis is aligned with the component of the +Z axis */
/*        of the body-fixed reference frame orthogonal to the */
/*        outward normal vector, i.e. the +X axis points towards */
/*        the body's North pole; */

/*     -  the +Y axis completes the right-handed system. */

/*     For observers located on the +Z axis of the body-fixed frame */
/*     designated by OBSREF, the following definition of the local */
/*     topocentric reference frame is used by this routine: */

/*     -  the +Z axis is aligned with the surface normal outward */
/*        vector at the observer's location; */

/*     -  the +X axis aligned with the +X axis of the body-fixed */
/*        reference frame; */

/*     -  the +Y axis completes the right-handed system. */

/*     In both cases, the origin of the local topocentric frame is */
/*     the observer's location. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Find the azimuth/elevation state of Venus as seen from the */
/*        DSS-14 station at a given epoch first using the position of */
/*        the station given as a vector in the ITRF93 frame and then */
/*        using the data provided in the kernel pool for the DSS-14 */
/*        station. */


/*        Task description */
/*        ================ */

/*        In this example, we will obtain the apparent state of Venus as */
/*        seen from DSS-14 station in the DSS-14 topocentric reference */
/*        frame. For this computation, we'll use the DSS-14 station's */
/*        location given as a vector in the ITRF93 frame. */

/*        Then we will compute same apparent state using SPKPOS to */
/*        obtain a Cartesian state vector, after which we will transform */
/*        the vector coordinates to azimuth, elevation and range and */
/*        their derivatives using RECAZL and DAZLDR. */

/*        In order to introduce the usage of the logical flags AZCCW */
/*        and ELPLSZ, we will request the azimuth to be measured */
/*        clockwise and the elevation positive towards the +Z */
/*        axis of the DSS-14_TOPO reference frame. */

/*        Results from the two computations will not agree exactly */
/*        because of time-dependent differences in the orientation, */
/*        relative to the ITRF93 frame, of the topocentric frame centered */
/*        at DSS-14. This orientation varies with time due to movement of */
/*        the station, which is affected by tectonic plate motion. The */
/*        computation using AZLCPO evaluates the orientation of this */
/*        frame using the station location at the observation epoch, */
/*        while the SPKPOS computation uses the orientation provided by */
/*        the station frame kernel. The latter is fixed and is derived */
/*        from the station location at an epoch specified in the */
/*        documentation of that kernel. */


/*        Kernels */
/*        ======= */

/*        Use the meta-kernel shown below to load the required SPICE */
/*        kernels. */


/*           KPL/MK */

/*           File name: azlcpo_ex1.tm */

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
/*              naif0011.tls                     Leapseconds */
/*              pck00010.tpc                     Planetary constants */
/*              earth_720101_070426.bpc          Earth historical */
/*                                                  binary PCK */
/*              earthstns_itrf93_050714.bsp      DSN station SPK */
/*              earth_topo_050714.tf             DSN station FK */

/*           \begindata */

/*           KERNELS_TO_LOAD = ( 'de430.bsp', */
/*                               'naif0011.tls', */
/*                               'pck00010.tpc', */
/*                               'earth_720101_070426.bpc', */
/*                               'earthstns_itrf93_050714.bsp', */
/*                               'earth_topo_050714.tf'         ) */

/*           \begintext */

/*           End of meta-kernel. */


/*        Example code begins here. */


/*              PROGRAM AZLCPO_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     SPICELIB functions */
/*        C */
/*              DOUBLE PRECISION      DPR */

/*        C */
/*        C     Local parameters */
/*        C */
/*              CHARACTER*(*)         FMT1 */
/*              PARAMETER           ( FMT1   = '(A,F20.8)'     ) */

/*              CHARACTER*(*)         META */
/*              PARAMETER           ( META   = 'azlcpo_ex1.tm' ) */

/*              INTEGER               BDNMLN */
/*              PARAMETER           ( BDNMLN = 36 ) */

/*              INTEGER               CORLEN */
/*              PARAMETER           ( CORLEN = 10 ) */

/*              INTEGER               FRNMLN */
/*              PARAMETER           ( FRNMLN = 32 ) */

/*              INTEGER               STRLEN */
/*              PARAMETER           ( STRLEN = 40 ) */

/*              INTEGER               TIMLEN */
/*              PARAMETER           ( TIMLEN = 40 ) */

/*        C */
/*        C     Local variables */
/*        C */
/*              CHARACTER*(CORLEN)    ABCORR */
/*              CHARACTER*(BDNMLN)    OBS */
/*              CHARACTER*(BDNMLN)    OBSCTR */
/*              CHARACTER*(FRNMLN)    OBSREF */
/*              CHARACTER*(TIMLEN)    OBSTIM */
/*              CHARACTER*(STRLEN)    METHOD */
/*              CHARACTER*(FRNMLN)    REF */
/*              CHARACTER*(BDNMLN)    TARGET */

/*              DOUBLE PRECISION      AZ */
/*              DOUBLE PRECISION      AZLSTA ( 6    ) */
/*              DOUBLE PRECISION      AZLVEL ( 3    ) */
/*              DOUBLE PRECISION      EL */
/*              DOUBLE PRECISION      ET */
/*              DOUBLE PRECISION      JACOBI ( 3, 3 ) */
/*              DOUBLE PRECISION      LT */
/*              DOUBLE PRECISION      STATE  ( 6    ) */
/*              DOUBLE PRECISION      OBSPOS ( 3    ) */
/*              DOUBLE PRECISION      R */

/*              INTEGER               I */

/*              LOGICAL               AZCCW */
/*              LOGICAL               ELPLSZ */

/*        C */
/*        C     Load SPICE kernels. */
/*        C */
/*              CALL FURNSH ( META ) */

/*        C */
/*        C     Convert the observation time to seconds past J2000 TDB. */
/*        C */
/*              OBSTIM = '2003 Jan 01 00:00:00 TDB' */

/*              CALL STR2ET ( OBSTIM, ET ) */

/*        C */
/*        C     Set the method, target, center of motion of the observer, */
/*        C     frame of observer position, and aberration corrections. */
/*        C */
/*              METHOD = 'ELLIPSOID' */
/*              TARGET = 'VENUS' */
/*              OBSCTR = 'EARTH' */
/*              OBSREF = 'ITRF93' */
/*              ABCORR = 'CN+S' */

/*        C */
/*        C     Set the position of DSS-14 relative to the earth's */
/*        C     center at the observation epoch, expressed in the */
/*        C     ITRF93 reference frame. Values come from the */
/*        C     earth station SPK specified in the meta-kernel. */
/*        C */
/*        C     The actual station velocity is non-zero due */
/*        C     to tectonic plate motion; we ignore the motion */
/*        C     in this example. */
/*        C */
/*              OBSPOS(1) =  -2353.621419700D0 */
/*              OBSPOS(2) =  -4641.341471700D0 */
/*              OBSPOS(3) =   3677.052317800D0 */

/*        C */
/*        C     We want the azimuth/elevation coordinates to be measured */
/*        C     with the azimuth increasing clockwise and the */
/*        C     elevation positive towards +Z axis of the local */
/*        C     topocentric reference frame */
/*        C */
/*              AZCCW  = .FALSE. */
/*              ELPLSZ = .TRUE. */

/*              CALL AZLCPO ( METHOD, TARGET, ET,     ABCORR, */
/*             .              AZCCW,  ELPLSZ, OBSPOS, OBSCTR, */
/*             .              OBSREF, AZLSTA, LT              ) */

/*        C */
/*        C     In order to check the results obtained using AZLCPO */
/*        C     we are going to compute the same azimuth/elevation state */
/*        C     using the position of DSS-14 and its local topocentric */
/*        C     reference frame 'DSS-14_TOPO' from the kernel pool. */
/*        C */
/*              OBS    = 'DSS-14' */
/*              REF    = 'DSS-14_TOPO' */

/*        C */
/*        C     Compute the observer-target state. */
/*        C */
/*              CALL SPKEZR ( TARGET, ET, REF, ABCORR, OBS, */
/*             .              STATE,  LT                   ) */

/*        C */
/*        C     Convert the position to azimuth/elevation coordinates. */
/*        C */
/*              CALL RECAZL ( STATE, AZCCW, ELPLSZ, R, AZ, EL ) */

/*        C */
/*        C     Convert velocity to azimuth/elevation coordinates. */
/*        C */
/*              CALL DAZLDR ( STATE(1), STATE(2), STATE(3), */
/*             .              AZCCW,    ELPLSZ,   JACOBI   ) */

/*              CALL MXV ( JACOBI, STATE(4), AZLVEL ) */

/*              WRITE(*,*) */
/*              WRITE(*,'(A)') 'AZ/EL coordinates (from AZLCPO):' */
/*              WRITE(*,*) */
/*              WRITE(*,FMT1) '   Range     (km)         = ', AZLSTA(1) */
/*              WRITE(*,FMT1) '   Azimuth   (deg)        = ', AZLSTA(2) */
/*             .                                            * DPR() */
/*              WRITE(*,FMT1) '   Elevation (deg)        = ', AZLSTA(3) */
/*             .                                            * DPR() */
/*              WRITE(*,*) */
/*              WRITE(*,'(A)') 'AZ/EL coordinates (using kernels):' */
/*              WRITE(*,*) */
/*              WRITE(*,FMT1) '   Range     (km)         = ', R */
/*              WRITE(*,FMT1) '   Azimuth   (deg)        = ', AZ * DPR() */
/*              WRITE(*,FMT1) '   Elevation (deg)        = ', EL * DPR() */
/*              WRITE(*,*) */
/*              WRITE(*,'(A)') 'AZ/EL velocity (from AZLCPO):' */
/*              WRITE(*,*) */
/*              WRITE(*,FMT1) '   d Range/dt    (km/s)   = ', AZLSTA(4) */
/*              WRITE(*,FMT1) '   d Azimuth/dt  (deg/s)  = ', AZLSTA(5) */
/*             .                                            * DPR() */
/*              WRITE(*,FMT1) '   d Elevation/dt (deg/s) = ', AZLSTA(6) */
/*             .                                            * DPR() */
/*              WRITE(*,*) */
/*              WRITE(*,'(A)') 'AZ/EL velocity (using kernels):' */
/*              WRITE(*,*) */
/*              WRITE(*,FMT1) '   d Range/dt     (km/s)  = ', AZLVEL(1) */
/*              WRITE(*,FMT1) '   d Azimuth/dt   (deg/s) = ', AZLVEL(2) */
/*             .                                            * DPR() */
/*              WRITE(*,FMT1) '   d Elevation/dt (deg/s) = ', AZLVEL(3) */
/*             .                                            * DPR() */
/*              WRITE(*,*) */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        AZ/EL coordinates (from AZLCPO): */

/*           Range     (km)         =    89344802.82679011 */
/*           Azimuth   (deg)        =         269.04481881 */
/*           Elevation (deg)        =         -25.63088321 */

/*        AZ/EL coordinates (using kernels): */

/*           Range     (km)         =    89344802.82679011 */
/*           Azimuth   (deg)        =         269.04481846 */
/*           Elevation (deg)        =         -25.63088278 */

/*        AZ/EL velocity (from AZLCPO): */

/*           d Range/dt    (km/s)   =          13.41734176 */
/*           d Azimuth/dt  (deg/s)  =           0.00238599 */
/*           d Elevation/dt (deg/s) =          -0.00339644 */

/*        AZ/EL velocity (using kernels): */

/*           d Range/dt     (km/s)  =          13.41734176 */
/*           d Azimuth/dt   (deg/s) =           0.00238599 */
/*           d Elevation/dt (deg/s) =          -0.00339644 */


/*        Note the discrepancy in the AZ/EL coordinates found by the two */
/*        computation methods. Please refer to the task description for */
/*        an explanation. */

/* $ Restrictions */

/*     1)  This routine may not be suitable for work with stars or other */
/*         objects having large distances from the observer, due to loss */
/*         of precision in position vectors. */

/*     2)  The Jacobian matrix of the transformation from rectangular to */
/*         azimuth/elevation coordinates has a singularity for points */
/*         located on the Z-axis ( X = 0 and Y = 0 ) of the local */
/*         topocentric frame centered at OBSPOS; therefore the */
/*         derivative of the azimuth/elevation coordinates cannot be */
/*         computed for those points. */

/*         A user who wishes to compute the azimuth/elevation */
/*         coordinates, without their derivatives, of TARGET as seen */
/*         from OBSPOS at the input time ET, for those cases when TARGET */
/*         is located along the local topocentric Z-axis, could do so by */
/*         executing the following calls: */

/*            CALL SPKCPO ( TARGET, ET,     OBSREF, 'OBSERVER', ABCORR, */
/*           .              OBSPOS, OBSCTR, OBSREF,  STATE,     LT     ) */

/*            RANGE = VNORM( STATE ) */

/*         By definition, the azimuth is zero and the elevation is */
/*         either pi/2 if ELPLSZ is .TRUE., or -pi/2 otherwise. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 01-NOV-2021 (JDR) (NJB) (EDW) */

/* -& */
/* $ Index_Entries */

/*     AZ/EL_coordinates relative to constant_position_observer */
/*     AZ/EL_coordinates w.r.t. constant_position surface_point */
/*     AZ/EL_coordinates relative to surface_point extended_object */
/*     AZ/EL_coordinates relative to landmark on extended_object */

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
    chkin_("AZLCPO", (ftnlen)6);

/*     Get the center of motion ID code here, since it will be */
/*     need later on several calls. */

    bods2c_(obsctr, &obscde, &found, obsctr_len);
    if (! found) {
	setmsg_("The observer's center of motion, '#', is not a recognized n"
		"ame for an ephemeris object. The cause of this problem may b"
		"e that you did not load a text kernel containing body-name m"
		"apping assignments for this name, or that you need an update"
		"d version of the SPICE Toolkit.", (ftnlen)270);
	errch_("#", obsctr, (ftnlen)1, obsctr_len);
	sigerr_("SPICE(IDCODENOTFOUND)", (ftnlen)21);
	chkout_("AZLCPO", (ftnlen)6);
	return 0;
    }

/*     Determine the attributes of the frame designated by OBSREF. */

    namfrm_(obsref, &fxfcde, obsref_len);
    frinfo_(&fxfcde, &center, &frclss, &clssid, &found);
    if (failed_()) {
	chkout_("AZLCPO", (ftnlen)6);
	return 0;
    }
    if (! found) {
	setmsg_("Reference frame # is not recognized by the SPICE frame subs"
		"ystem. Possibly a required frame definition kernel has not b"
		"een loaded.", (ftnlen)130);
	errch_("#", obsref, (ftnlen)1, obsref_len);
	sigerr_("SPICE(UNKNOWNFRAME)", (ftnlen)19);
	chkout_("AZLCPO", (ftnlen)6);
	return 0;
    }

/*     Make sure that OBSREF is centered at the observer's center of */
/*     motion. */

    if (center != obscde) {
	setmsg_("Reference frame # is not centered at the observer's center "
		"of motion #. The ID code of the frame center is #.", (ftnlen)
		109);
	errch_("#", obsref, (ftnlen)1, obsref_len);
	errch_("#", obsctr, (ftnlen)1, obsctr_len);
	errint_("#", &center, (ftnlen)1);
	sigerr_("SPICE(INVALIDFRAME)", (ftnlen)19);
	chkout_("AZLCPO", (ftnlen)6);
	return 0;
    }

/*     Construct the local topocentric reference frame. Check */
/*     first the method to be used. */

    if (eqstr_(method, "ELLIPSOID", method_len, (ftnlen)9)) {

/*        If the input observer position is on the Z-axis of the */
/*        body-fixed, the local topocentric frame will be defined */
/*        as follows: */

/*           - the +Z axis aligned with the outward normal vector; */

/*           - the +X axis aligned with the +X axis of the body-fixed */
/*             reference frame; */

/*           - the +Y axis completes the right-handed system. */

/*        otherwise, the local topocentric frame will be defined as */
/*        follows: */

/*           - the +Z axis aligned with the outward normal vector; */

/*           - the +X axis aligned with the component of the +Z axis */
/*             of the body-fixed reference frame orthogonal to the */
/*             outward normal vector; */

/*           - the +Y axis completes the right-handed frame. */

	if (obspos[0] == 0. && obspos[1] == 0.) {
	    ident_(xftopo);
	    if (obspos[2] < 0.) {
		d__1 = pi_();
		rotmat_(xftopo, &d__1, &c__1, tmpmat);
		moved_(tmpmat, &c__9, xftopo);
	    }
	} else {

/*           Get the radii of the observer center of motion from the */
/*           kernel pool. */

	    zzgftreb_(&obscde, radii);
	    if (failed_()) {
		chkout_("AZLCPO", (ftnlen)6);
		return 0;
	    }

/*           The observer's position does not need to be located on */
/*           the surface of the reference ellipsoid. Find the nearest */
/*           point on the ellipsoid to the observer. */

	    nearpt_(obspos, radii, &radii[1], &radii[2], obsspt, &alt);

/*           Get the outward-pointing, unit normal vector from the point */
/*           on the surface of the reference ellipsoid. */

	    surfnm_(radii, &radii[1], &radii[2], obsspt, normal);

/*           Construct the transformation matrix from the body-fixed */
/*           reference frame associated with the observer's center of */
/*           motion and the local topocentric frame at the observer's */
/*           location. */

	    twovec_(normal, &c__3, z__, &c__1, xftopo);
	}
    } else {
	setmsg_("The computation method # was not recognized. ", (ftnlen)45);
	errch_("#", method, (ftnlen)1, method_len);
	sigerr_("SPICE(INVALIDMETHOD)", (ftnlen)20);
	chkout_("AZLCPO", (ftnlen)6);
	return 0;
    }

/*     Compute the observer-target position vector. Use as OUTREF the */
/*     same reference frame used for expressing the OBSPOS vector */
/*     (OBSREF). */

    spkcpo_(target, et, obsref, "OBSERVER", abcorr, obspos, obsctr, obsref, 
	    state, lt, target_len, obsref_len, (ftnlen)8, abcorr_len, 
	    obsctr_len, obsref_len);
    if (failed_()) {
	chkout_("AZLCPO", (ftnlen)6);
	return 0;
    }

/*     STATE is expressed with respect to the reference frame */
/*     specified by OBSREF. Convert this vector from OBSREF frame */
/*     to local-horizon frame. */

    mxv_(xftopo, state, lhsta);
    mxv_(xftopo, &state[3], &lhsta[3]);

/*     Convert LHSTA from rectangular to azimuth/elevation coordinates */

    recazl_(lhsta, azccw, elplsz, azlsta, &azlsta[1], &azlsta[2]);
    dazldr_(lhsta, &lhsta[1], &lhsta[2], azccw, elplsz, jacobi);
    if (failed_()) {
	chkout_("AZLCPO", (ftnlen)6);
	return 0;
    }
    mxv_(jacobi, &lhsta[3], &azlsta[3]);
    chkout_("AZLCPO", (ftnlen)6);
    return 0;
} /* azlcpo_ */

