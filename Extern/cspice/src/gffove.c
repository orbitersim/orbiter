/* gffove.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__0 = 0;
static logical c_false = FALSE_;
static doublereal c_b16 = 1.;

/* $Procedure GFFOVE ( GF, is target in FOV? ) */
/* Subroutine */ int gffove_(char *inst, char *tshape, doublereal *raydir, 
	char *target, char *tframe, char *abcorr, char *obsrvr, doublereal *
	tol, U_fp udstep, U_fp udrefn, logical *rpt, S_fp udrepi, U_fp udrepu,
	 S_fp udrepf, logical *bail, L_fp udbail, doublereal *cnfine, 
	doublereal *result, ftnlen inst_len, ftnlen tshape_len, ftnlen 
	target_len, ftnlen tframe_len, ftnlen abcorr_len, ftnlen obsrvr_len)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    extern /* Subroutine */ int zzgffvin_(char *, char *, doublereal *, char *
	    , char *, char *, char *, ftnlen, ftnlen, ftnlen, ftnlen, ftnlen, 
	    ftnlen);
    extern /* Subroutine */ int zzgffvst_();
    extern /* Subroutine */ int zzgfsolv_(U_fp, U_fp, U_fp, logical *, L_fp, 
	    logical *, doublereal *, doublereal *, doublereal *, doublereal *,
	     logical *, U_fp, doublereal *);
    integer i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen), errdp_(char *, 
	    doublereal *, ftnlen);
    extern integer sized_(doublereal *);
    integer count;
    doublereal start;
    extern logical failed_(void);
    extern /* Subroutine */ int scardd_(integer *, doublereal *);
    extern integer wncard_(doublereal *);
    doublereal finish;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), wnfetd_(doublereal *, integer *, doublereal *, 
	    doublereal *);

/* $ Abstract */

/*     Determine time intervals when a specified target body or ray */
/*     intersects the space bounded by the field-of-view (FOV) of a */
/*     specified instrument. Report progress and handle interrupts if so */
/*     commanded. */

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

/*     CK */
/*     FRAMES */
/*     GF */
/*     KERNEL */
/*     NAIF_IDS */
/*     PCK */
/*     SPK */
/*     TIME */
/*     WINDOWS */

/* $ Keywords */

/*     EVENT */
/*     FOV */
/*     GEOMETRY */
/*     INSTRUMENT */
/*     SEARCH */
/*     WINDOW */

/* $ Declarations */
/* $ Abstract */

/*     This file contains public, global parameter declarations */
/*     for the SPICELIB Geometry Finder (GF) subsystem. */

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

/*     GF */

/* $ Keywords */

/*     GEOMETRY */
/*     ROOT */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman      (JPL) */
/*     L.E. Elson        (JPL) */
/*     E.D. Wright       (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 2.0.0  29-NOV-2016 (NJB) */

/*        Upgraded to support surfaces represented by DSKs. */

/*        Bug fix: removed declaration of NVRMAX parameter. */

/* -    SPICELIB Version 1.3.0, 01-OCT-2011 (NJB) */

/*       Added NWILUM parameter. */

/* -    SPICELIB Version 1.2.0, 14-SEP-2010 (EDW) */

/*       Added NWPA parameter. */

/* -    SPICELIB Version 1.1.0, 08-SEP-2009 (EDW) */

/*       Added NWRR parameter. */
/*       Added NWUDS parameter. */

/* -    SPICELIB Version 1.0.0, 21-FEB-2009 (NJB) (LSE) (EDW) */

/* -& */

/*     Root finding parameters: */

/*     CNVTOL is the default convergence tolerance used by the */
/*     high-level GF search API routines. This tolerance is */
/*     used to terminate searches for binary state transitions: */
/*     when the time at which a transition occurs is bracketed */
/*     by two times that differ by no more than CNVTOL, the */
/*     transition time is considered to have been found. */

/*     Units are TDB seconds. */


/*     NWMAX is the maximum number of windows allowed for user-defined */
/*     workspace array. */

/*        DOUBLE PRECISION      WORK   ( LBCELL : MW, NWMAX ) */

/*     Currently no more than twelve windows are required; the three */
/*     extra windows are spares. */

/*     Callers of GFEVNT can include this file and use the parameter */
/*     NWMAX to declare the second dimension of the workspace array */
/*     if necessary. */


/*     Callers of GFIDST should declare their workspace window */
/*     count using NWDIST. */


/*     Callers of GFSEP should declare their workspace window */
/*     count using NWSEP. */


/*     Callers of GFRR should declare their workspace window */
/*     count using NWRR. */


/*     Callers of GFUDS should declare their workspace window */
/*     count using NWUDS. */


/*     Callers of GFPA should declare their workspace window */
/*     count using NWPA. */


/*     Callers of GFILUM should declare their workspace window */
/*     count using NWILUM. */


/*     ADDWIN is a parameter used to expand each interval of the search */
/*     (confinement) window by a small amount at both ends in order to */
/*     accommodate searches using equality constraints. The loaded */
/*     kernel files must accommodate these expanded time intervals. */


/*     FRMNLN is a string length for frame names. */


/*     FOVTLN -- maximum length for FOV string. */


/*     Specify the character strings that are allowed in the */
/*     specification of field of view shapes. */


/*     Character strings that are allowed in the */
/*     specification of occultation types: */


/*     Occultation target shape specifications: */


/*     Specify the number of supported occultation types and occultation */
/*     type string length: */


/*     Instrument field-of-view (FOV) parameters */

/*     Maximum number of FOV boundary vectors: */


/*     FOV shape parameters: */

/*        circle */
/*        ellipse */
/*        polygon */
/*        rectangle */


/*     End of file gf.inc. */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     LBCELL     P   SPICE Cell lower bound. */
/*     MAXVRT     P   Maximum number of FOV boundary vertices. */
/*     INST       I   Name of the instrument. */
/*     TSHAPE     I   Type of shape model used for target body. */
/*     RAYDIR     I   Ray's direction vector. */
/*     TARGET     I   Name of the target body. */
/*     TFRAME     I   Body-fixed, body-centered frame for target body. */
/*     ABCORR     I   Aberration correction flag. */
/*     OBSRVR     I   Name of the observing body. */
/*     TOL        I   Convergence tolerance in seconds. */
/*     UDSTEP     I   Name of routine that returns a time step. */
/*     UDREFN     I   Name of the routine that computes a refined time. */
/*     RPT        I   Progress report flag. */
/*     UDREPI     I   Function that initializes progress reporting. */
/*     UDREPU     I   Function that updates the progress report. */
/*     UDREPF     I   Function that finalizes progress reporting. */
/*     BAIL       I   Logical indicating program interrupt monitoring. */
/*     UDBAIL     I   Name of a routine that signals a program interrupt. */
/*     CNFINE     I   SPICE window to which the search is restricted. */
/*     RESULT    I-O  SPICE window containing results. */

/* $ Detailed_Input */

/*     INST     is a string indicates the name of an instrument, such as */
/*              a spacecraft-mounted framing camera, the field of view */
/*              (FOV) of which is to be used for a target intersection */
/*              search: times when the specified target intersects the */
/*              region of space corresponding to the FOV are sought. */

/*              INST must have a corresponding NAIF ID and a frame */
/*              defined, as is normally done in a frame kernel. It must */
/*              also have an associated reference frame and a FOV shape, */
/*              boresight and boundary vertices (or reference vector and */
/*              reference angles) defined, as is usually done in an */
/*              instrument kernel. */

/*              See the header of the SPICELIB routine GETFOV for a */
/*              description of the required parameters associated with an */
/*              instrument. */

/*     TSHAPE   is a string indicating the geometric model used to */
/*              represent the location and shape of the target body. The */
/*              target body may be represented by either an ephemeris */
/*              object or a ray emanating from the observer. */

/*              The supported values of TSHAPE are: */

/*                 'ELLIPSOID'     The target is an ephemeris object. */

/*                                 The target's shape is represented */
/*                                 using triaxial ellipsoid model, */
/*                                 with radius values provided via the */
/*                                 kernel pool. A kernel variable */
/*                                 having a name of the form */

/*                                    'BODYnnn_RADII' */

/*                                 where nnn represents the NAIF */
/*                                 integer code associated with the */
/*                                 body, must be present in the kernel */
/*                                 pool. This variable must be */
/*                                 associated with three numeric */
/*                                 values giving the lengths of the */
/*                                 ellipsoid's X, Y, and Z semi-axes. */

/*                 'POINT'         The target is an ephemeris object. */
/*                                 The body is treated as a single */
/*                                 point. */

/*                 'RAY'           The target is NOT an ephemeris */
/*                                 object. Instead, the target is */
/*                                 represented by the ray emanating */
/*                                 from the observer's location and */
/*                                 having direction vector RAYDIR. The */
/*                                 target is considered to be visible */
/*                                 if and only if the ray is contained */
/*                                 within the space bounded by the */
/*                                 instrument FOV. */

/*              Case and leading or trailing blanks are not */
/*              significant in the string TSHAPE. */

/*     RAYDIR   is the direction vector associated with a ray */
/*              representing the target. RAYDIR is used if and only if */
/*              TSHAPE (see description above) indicates the target is */
/*              modeled as a ray. */

/*     TARGET   is the name of the target body, the appearances of which */
/*              in the specified instrument's field of view are sought. */
/*              The body must be an ephemeris object. */

/*              Optionally, you may supply the integer NAIF ID code for */
/*              the body as a string. For example both 'MOON' and '301' */
/*              are legitimate strings that designate the Moon. */

/*              Case and leading or trailing blanks are not significant */
/*              in the string TARGET. */

/*              The input argument TARGET is used if and only if the */
/*              target is NOT modeled as ray, as indicated by the input */
/*              argument TSHAPE. */

/*              TARGET may be set to a blank string if the target is */
/*              modeled as a ray. */

/*     TFRAME   is the name of the reference frame associated with the */
/*              target. Examples of such names are 'IAU_SATURN' (for */
/*              Saturn) and 'ITRF93' (for the Earth). */

/*              If the target is an ephemeris object modeled as an */
/*              ellipsoid, TFRAME must designate a body-fixed reference */
/*              frame centered on the target body. */

/*              If the target is an ephemeris object modeled as a point, */
/*              TFRAME is ignored; TFRAME should be left blank. */

/*              If the target is modeled as a ray, TFRAME may designate */
/*              any reference frame. Since light time corrections are not */
/*              supported for rays, the orientation of the frame is */
/*              always evaluated at the epoch associated with the */
/*              observer, as opposed to the epoch associated with the */
/*              light-time corrected position of the frame center. */

/*              Case and leading or trailing blanks bracketing a */
/*              non-blank frame name are not significant in the string */
/*              TFRAME. */

/*     ABCORR   is a string indicating the aberration corrections to be */
/*              applied when computing the target's position and */
/*              orientation. The supported values of ABCORR depend on the */
/*              target representation. */

/*              If the target is represented by a ray, the aberration */
/*              correction options are */

/*                 'NONE'       No correction. */
/*                 'S'          Stellar aberration correction, reception */
/*                              case. */
/*                 'XS'         Stellar aberration correction, */
/*                              transmission case. */

/*              If the target is an ephemeris object, the aberration */
/*              correction options are those supported by the SPICE SPK */
/*              system. For remote sensing applications, where the */
/*              apparent position and orientation of the target seen by */
/*              the observer are desired, normally either of the */
/*              corrections */

/*                 'LT+S' */
/*                 'CN+S' */

/*              should be used. These and the other supported options are */
/*              described below. */

/*              Supported aberration correction options for observation */
/*              (the case where radiation is received by observer at ET) */
/*              are: */

/*                 'NONE'       No correction. */
/*                 'LT'         Light time only */
/*                 'LT+S'       Light time and stellar aberration. */
/*                 'CN'         Converged Newtonian (CN) light time. */
/*                 'CN+S'       CN light time and stellar aberration. */

/*              Supported aberration correction options for transmission */
/*              (the case where radiation is emitted from observer at ET) */
/*              are: */

/*                 'XLT'        Light time only. */
/*                 'XLT+S'      Light time and stellar aberration. */
/*                 'XCN'        Converged Newtonian (CN) light time. */
/*                 'XCN+S'      CN light time and stellar aberration. */

/*              For detailed information, see the geometry finder */
/*              required reading, gf.req. */

/*              Case, leading and trailing blanks are not significant */
/*              in the string ABCORR. */

/*     OBSRVR   is the name of the body from which the target is */
/*              observed. The instrument designated by INST is treated as */
/*              if it were co-located with the observer. */

/*              Optionally, you may supply the integer NAIF ID code */
/*              for the body as a string. */

/*              Case and leading or trailing blanks are not */
/*              significant in the string OBSRVR. */

/*     TOL      is a tolerance value used to determine convergence of */
/*              root-finding operations. TOL is measured in TDB seconds */
/*              and must be greater than zero. */

/*     UDSTEP   is an externally specified routine that computes a time */
/*              step used to find transitions of the state being */
/*              considered. A state transition occurs where the state */
/*              changes from being "visible" to being "not visible" or */
/*              vice versa. */

/*              This routine relies on UDSTEP returning step sizes small */
/*              enough so that state transitions within the confinement */
/*              window are not overlooked. */

/*              The calling sequence for UDSTEP is: */

/*                 CALL UDSTEP ( ET, STEP ) */

/*              where: */

/*                 ET      is the input start time from which the */
/*                         algorithm is to search forward for a state */
/*                         transition. ET is expressed as seconds past */
/*                         J2000 TDB. ET is a DOUBLE PRECISION number. */

/*                 STEP    is the output step size. STEP indicates */
/*                         how far to advance ET so that ET and */
/*                         ET+STEP may bracket a state transition and */
/*                         definitely do not bracket more than one */
/*                         state transition. STEP is a DOUBLE */
/*                         PRECISION number. Units are TDB seconds. */

/*              If a constant step size is desired, the SPICELIB routine */

/*                 GFSTEP */

/*              may be used as the step size function. If GFSTEP is used, */
/*              the step size must be set by calling GFSSTP prior to */
/*              calling this routine. */

/*     UDREFN   is the name of the externally specified routine that */
/*              refines the times that bracket a transition point. In */
/*              other words, once a pair of times, T1 and T2, that */
/*              bracket a state transition have been found, UDREFN */
/*              computes an intermediate time T such that either [T1, T] */
/*              or [T, T2] contains the time of the state transition. The */
/*              calling sequence for UDREFN is: */

/*                 CALL UDREFN ( T1, T2, S1, S2, T ) */

/*              where the inputs are: */

/*                 T1    is a time when the visibility state is S1. T1 */
/*                       is expressed as seconds past J2000 TDB. */

/*                 T2    is a time when the visibility state is S2. T2 is */
/*                       expressed as seconds past J2000 TDB and is */
/*                       assumed to be larger than T1. */

/*                 S1    is the visibility state at time T1. S1 is a */
/*                       LOGICAL value. */

/*                 S2    is the visibility state at time T2. S2 is a */
/*                       LOGICAL value. */

/*              The output is: */

/*                 T     is the next time to check for a state */
/*                       transition. T is expressed as seconds past */
/*                       J2000 TDB and is between T1 and T2. */

/*              If a simple bisection method is desired, the SPICELIB */
/*              routine GFREFN may be used as the refinement function. */

/*     RPT      is a logical variable that controls whether progress */
/*              reporting is enabled. When RPT is .TRUE., progress */
/*              reporting is enabled and the routines UDREPI, UDREPU, and */
/*              UDREPF (see descriptions below) are used to report */
/*              progress. */

/*     UDREPI   is a user-defined subroutine that initializes a progress */
/*              report. When progress reporting is enabled, UDREPI is */
/*              called at the start of a search. The calling sequence of */
/*              UDREPI is */

/*                 UDREPI ( CNFINE, SRCPRE, SRCSUF ) */

/*                 DOUBLE PRECISION    CNFINE ( LBCELL : * ) */
/*                 CHARACTER*(*)       SRCPRE */
/*                 CHARACTER*(*)       SRCSUF */

/*              where */

/*                 CNFINE */

/*              is the confinement window specifying the time period */
/*              over which a search is conducted, and */

/*                 SRCPRE */
/*                 SRCSUF */

/*              are prefix and suffix strings used in the progress */
/*              report: these strings are intended to bracket a */
/*              representation of the fraction of work done. For example, */
/*              when the SPICELIB progress reporting functions are used, */
/*              if SRCPRE and SRCSUF are, respectively, */

/*                 'Target visibility search' */
/*                 'done.' */

/*              the progress report display at the end of the search will */
/*              be: */

/*                 Target visibility search 100.00% done. */

/*              The SPICELIB routine GFREPI may be used as the actual */
/*              argument corresponding to UDREPI. If so, the SPICELIB */
/*              routines GFREPU and GFREPF must be the actual arguments */
/*              corresponding to UDREPU and UDREPF. */

/*     UDREPU   is a user-defined subroutine that updates the progress */
/*              report for a search. The calling sequence of UDREPU is */

/*                 UDREPU ( IVBEG, IVEND, ET ) */

/*                 DOUBLE PRECISION      IVBEG */
/*                 DOUBLE PRECISION      IVEND */
/*                 DOUBLE PRECISION      ET */

/*              Here IVBEG, IVEND are the bounds of an interval that is */
/*              contained in some interval belonging to the confinement */
/*              window. The confinement window is associated with some */
/*              root finding activity. It is used to determine how much */
/*              total time is being searched in order to find the events */
/*              of interest. */

/*              ET is an epoch belonging to the interval [IVBEG, IVEND]. */

/*              In order for a meaningful progress report to be */
/*              displayed, IVBEG and IVEND must satisfy the following */
/*              constraints: */

/*              -  IVBEG must be less than or equal to IVEND. */

/*              -  The interval [ IVBEG, IVEND ] must be contained in */
/*                 some interval of the confinement window. It can be */
/*                 a proper subset of the containing interval; that */
/*                 is, it can be smaller than the interval of the */
/*                 confinement window that contains it. */

/*              -  Over a search, the sum of the differences */

/*                    IVEND - IVBEG */

/*                 for all calls to this routine made during the search */
/*                 must equal the measure of the confinement window. */

/*              The SPICELIB routine GFREPU may be used as the actual */
/*              argument corresponding to UDREPU. If so, the SPICELIB */
/*              routines GFREPI and GFREPF must be the actual arguments */
/*              corresponding to UDREPI and UDREPF. */

/*     UDREPF   is a user-defined subroutine that finalizes a progress */
/*              report. UDREPF has no arguments. */

/*              The SPICELIB routine GFREPF may be used as the actual */
/*              argument corresponding to UDREPF. If so, the SPICELIB */
/*              routines GFREPI and GFREPU must be the actual arguments */
/*              corresponding to UDREPI and UDREPU. */

/*     BAIL     is a logical variable indicating whether or not interrupt */
/*              handling is enabled. When BAIL is set to .TRUE., the */
/*              input function UDBAIL (see description below) is used to */
/*              determine whether an interrupt has been issued. */

/*     UDBAIL   is the name of a user defined logical function that */
/*              indicates whether an interrupt signal has been issued */
/*              (for example, from the keyboard).  UDBAIL has no */
/*              arguments and returns a LOGICAL value. The return value */
/*              is .TRUE. if an interrupt has been issued; otherwise the */
/*              value is .FALSE. */

/*              GFFOVE uses UDBAIL only when BAIL (see above) is set to */
/*              .TRUE., indicating that interrupt handling is enabled. */
/*              When interrupt handling is enabled, GFFOVE and routines */
/*              in its call tree will call UDBAIL to determine whether to */
/*              terminate processing and return immediately. */

/*              If interrupt handing is not enabled, a logical function */
/*              must still be passed to GFFOVE as an input argument. The */
/*              SPICELIB function */

/*                 GFBAIL */

/*              may be used for this purpose. */

/*     CNFINE   is a SPICE window that confines the time period over */
/*              which the specified search is conducted. CNFINE may */
/*              consist of a single interval or a collection of */
/*              intervals. */

/*              The endpoints of the time intervals comprising CNFINE */
/*              are interpreted as seconds past J2000 TDB. */

/*              See the $Examples section below for a code example */
/*              that shows how to create a confinement window. */

/*              CNFINE must be initialized by the caller via the */
/*              SPICELIB routine SSIZED. */

/*     RESULT   is a double precision SPICE window which will contain */
/*              the search results. RESULT must be declared and */
/*              initialized with sufficient size to capture the full */
/*              set of time intervals within the search region on which */
/*              the specified condition is satisfied. */

/*              RESULT must be initialized by the caller via the */
/*              SPICELIB routine SSIZED. */

/*              If RESULT is non-empty on input, its contents will be */
/*              discarded before GFFOVE conducts its search. */

/* $ Detailed_Output */

/*     RESULT   is a SPICE window representing the set of time */
/*              intervals, within the confinement period, when image */
/*              of the target body is partially or completely within */
/*              the specified instrument field of view. */

/*              The endpoints of the time intervals comprising RESULT */
/*              are interpreted as seconds past J2000 TDB. */

/*              If no times within the confinement window satisfy the */
/*              search criteria, RESULT will be returned with a */
/*              cardinality of zero. */

/* $ Parameters */

/*     LBCELL   is the lower bound for SPICE cell arrays. */

/*     MAXVRT   is the maximum number of vertices that may be used */
/*              to define the boundary of the specified instrument's */
/*              field of view. */

/*     See INCLUDE file gf.inc for declarations and descriptions of */
/*     parameters used throughout the GF system. */

/* $ Exceptions */

/*     1)  In order for this routine to produce correct results, */
/*         the step size must be appropriate for the problem at hand. */
/*         Step sizes that are too large may cause this routine to miss */
/*         roots; step sizes that are too small may cause this routine */
/*         to run unacceptably slowly and in some cases, find spurious */
/*         roots. */

/*         This routine does not diagnose invalid step sizes, except */
/*         that if the step size is non-positive, an error is signaled by */
/*         a routine in the call tree of this routine. */

/*     2)  Due to numerical errors, in particular, */

/*            - Truncation error in time values */
/*            - Finite tolerance value */
/*            - Errors in computed geometric quantities */

/*         it is *normal* for the condition of interest to not always be */
/*         satisfied near the endpoints of the intervals comprising the */
/*         result window. */

/*         The result window may need to be contracted slightly by the */
/*         caller to achieve desired results. The SPICE window routine */
/*         WNCOND can be used to contract the result window. */

/*     3)  If the name of either the target or observer cannot be */
/*         translated to a NAIF ID code, an error is signaled by */
/*         a routine in the call tree of this routine. */

/*     4)  If the specified aberration correction is not a supported */
/*         value for the target type (ephemeris object or ray), an error */
/*         is signaled by a routine in the call tree of this routine. */

/*     5)  If the radii of a target body modeled as an ellipsoid cannot */
/*         be determined by searching the kernel pool for a kernel */
/*         variable having a name of the form */

/*            'BODYnnn_RADII' */

/*         where nnn represents the NAIF integer code associated with */
/*         the body, an error is signaled by a routine in the */
/*         call tree of this routine. */

/*     6)  If the target body coincides with the observer body OBSRVR, an */
/*         error is signaled by a routine in the call tree of this */
/*         routine. */

/*     7)  If the body model specifier TSHAPE is not recognized, an error */
/*         is signaled by a routine in the call tree of this routine. */

/*     8)  If a target body-fixed reference frame associated with a */
/*         non-point target is not recognized, an error is signaled by a */
/*         routine in the call tree of this routine. */

/*     9)  If a target body-fixed reference frame is not centered at the */
/*         corresponding target body, an error is signaled by a routine */
/*         in the call tree of this routine. */

/*     10) If the instrument name INST does not have corresponding NAIF */
/*         ID code, an error is signaled by a routine in the call */
/*         tree of this routine. */

/*     11) If the FOV parameters of the instrument are not present in */
/*         the kernel pool, an error is signaled by a routine */
/*         in the call tree of this routine. */

/*     12) If the FOV boundary has more than MAXVRT vertices, an error */
/*         is signaled by a routine in the call tree of this */
/*         routine. */

/*     13) If the instrument FOV is polygonal, and this routine cannot */
/*         find a ray R emanating from the FOV vertex such that maximum */
/*         angular separation of R and any FOV boundary vector is within */
/*         the limit (pi/2)-SPICE_GF_MARGIN radians, an error is signaled */
/*         by a routine in the call tree of this routine. If the FOV is */
/*         any other shape, the same error check will be applied with the */
/*         instrument boresight vector serving the role of R. */

/*     14) If the loaded kernels provide insufficient data to compute a */
/*         requested state vector, an error is signaled by a */
/*         routine in the call tree of this routine. */

/*     15) If an error occurs while reading an SPK or other kernel file, */
/*         the error is signaled by a routine in the call tree */
/*         of this routine. */

/*     16) If the output SPICE window RESULT has insufficient capacity */
/*         to contain the number of intervals on which the specified */
/*         visibility condition is met, an error is signaled */
/*         by a routine in the call tree of this routine. */

/*     17) If the result window has size less than 2, the error */
/*         SPICE(WINDOWTOOSMALL) is signaled. */

/*     18) If the convergence tolerance size is non-positive, the error */
/*         SPICE(INVALIDTOLERANCE) is signaled. */

/*     19) If the step size is non-positive, an error is signaled by a */
/*         routine in the call tree of this routine. */

/*     20) If the ray's direction vector is zero, an error is signaled by */
/*         a routine in the call tree of this routine. */

/*     21) If operation of this routine is interrupted, the output result */
/*         window will be invalid. */

/* $ Files */

/*     Appropriate SPICE ernels must be loaded by the calling program */
/*     before this routine is called. */

/*     The following data are required: */

/*     -  SPK data: ephemeris data for target and observer that */
/*        describes the ephemeris of these objects for the period */
/*        defined by the confinement window, 'CNFINE' must be */
/*        loaded. If aberration corrections are used, the states of */
/*        target and observer relative to the solar system barycenter */
/*        must be calculable from the available ephemeris data. */
/*        Typically ephemeris data are made available by loading one */
/*        or more SPK files via FURNSH. */

/*     -  Frame data: if a frame definition is required to convert */
/*        the observer and target states to the body-fixed frame of */
/*        the target, that definition must be available in the kernel */
/*        pool. Typically the definitions of frames not already */
/*        built-in to SPICE are supplied by loading a frame kernel. */

/*        Data defining the reference frame associated with the */
/*        instrument designated by INST must be available in the kernel */
/*        pool. Additionally the name INST must be associated with an */
/*        ID code. Normally these data are  made available by loading */
/*        a frame kernel via FURNSH. */

/*     -  IK data: the kernel pool must contain data such that */
/*        the SPICELIB routine GETFOV may be called to obtain */
/*        parameters for INST. Normally such data are provided by */
/*        an IK via FURNSH. */

/*     The following data may be required: */

/*     -  PCK data: bodies modeled as triaxial ellipsoids must have */
/*        orientation data provided by variables in the kernel pool. */
/*        Typically these data are made available by loading a text */
/*        PCK file via FURNSH. */

/*        Bodies modeled as triaxial ellipsoids must have semi-axis */
/*        lengths provided by variables in the kernel pool. Typically */
/*        these data are made available by loading a text PCK file via */
/*        FURNSH. */

/*     -  CK data: if the instrument frame is fixed to a spacecraft, */
/*        at least one CK file will be needed to permit transformation */
/*        of vectors between that frame and both J2000 and the target */
/*        body-fixed frame. */

/*     -  SCLK data: if a CK file is needed, an associated SCLK */
/*        kernel is required to enable conversion between encoded SCLK */
/*        (used to time-tag CK data) and barycentric dynamical time */
/*        (TDB). */

/*     -  Since the input ray direction may be expressed in any */
/*        frame, FKs, CKs, SCLK kernels, PCKs, and SPKs may be */
/*        required to map the direction to the J2000 frame. */

/*     Kernel data are normally loaded once per program run, NOT every */
/*     time this routine is called. */

/* $ Particulars */

/*     This routine determines a set of one or more time intervals */
/*     within the confinement window when a specified ray or any portion */
/*     of a specified target body appears within the field of view of a */
/*     specified instrument. We'll use the term "visibility event" to */
/*     designate such an appearance. The set of time intervals resulting */
/*     from the search is returned as a SPICE window. */

/*     This routine provides the SPICE GF system's most flexible */
/*     interface for searching for FOV intersection events. */

/*     Applications that require do not require support for progress */
/*     reporting, interrupt handling, non-default step or refinement */
/*     functions, or non-default convergence tolerance normally should */
/*     call either GFTFOV or GFRFOV rather than this routine. */

/*     Below we discuss in greater detail aspects of this routine's */
/*     solution process that are relevant to correct and efficient use */
/*     of this routine in user applications. */


/*     The Search Process */
/*     ================== */

/*     The search for visibility events is treated as a search for state */
/*     transitions: times are sought when the state of the target ray or */
/*     body changes from "not visible" to "visible" or vice versa. */

/*     Step Size */
/*     ========= */

/*     Each interval of the confinement window is searched as follows: */
/*     first, the input step size is used to determine the time */
/*     separation at which the visibility state will be sampled. */
/*     Starting at the left endpoint of an interval, samples will be */
/*     taken at each step. If a state change is detected, a root has */
/*     been bracketed; at that point, the "root"--the time at which the */
/*     state change occurs---is found by a refinement process, for */
/*     example, via binary search. */

/*     Note that the optimal choice of step size depends on the lengths */
/*     of the intervals over which the visibility state is constant: */
/*     the step size should be shorter than the shortest visibility event */
/*     duration and the shortest period between visibility events, within */
/*     the confinement window. */

/*     Having some knowledge of the relative geometry of the target and */
/*     observer can be a valuable aid in picking a reasonable step size. */
/*     In general, the user can compensate for lack of such knowledge by */
/*     picking a very short step size; the cost is increased computation */
/*     time. */

/*     Note that the step size is not related to the precision with which */
/*     the endpoints of the intervals of the result window are computed. */
/*     That precision level is controlled by the convergence tolerance. */


/*     Convergence Tolerance */
/*     ===================== */

/*     The times of state transitions are called ``roots.'' */

/*     Once a root has been bracketed, a refinement process is used to */
/*     narrow down the time interval within which the root must lie. */
/*     This refinement process terminates when the location of the root */
/*     has been determined to within an error margin called the */
/*     "convergence tolerance." */

/*     The convergence tolerance used by high-level GF routines that */
/*     call this routine is set via the parameter CNVTOL, which is */
/*     declared in the INCLUDE file gf.inc. The value of CNVTOL is set */
/*     to a "tight" value so that the tolerance doesn't become the */
/*     limiting factor in the accuracy of solutions found by this */
/*     routine. In general the accuracy of input data will be the */
/*     limiting factor. */

/*     Setting the input tolerance TOL tighter than CNVTOL is unlikely */
/*     to be useful, since the results are unlikely to be more accurate. */
/*     Making the tolerance looser will speed up searches somewhat, */
/*     since a few convergence steps will be omitted. However, in most */
/*     cases, the step size is likely to have a much greater effect on */
/*     processing time than would the convergence tolerance. */


/*     The Confinement Window */
/*     ====================== */

/*     The simplest use of the confinement window is to specify a time */
/*     interval within which a solution is sought. However, the */
/*     confinement window can, in some cases, be used to make searches */
/*     more efficient. Sometimes it's possible to do an efficient search */
/*     to reduce the size of the time period over which a relatively */
/*     slow search of interest must be performed. For an example, see */
/*     the program CASCADE in the GF Example Programs chapter of the GF */
/*     Required Reading, gf.req. */

/* $ Examples */

/*     The numerical results shown for these examples may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */


/*     1) Search for times when Saturn's satellite Phoebe is within */
/*        the FOV of the Cassini narrow angle camera (CASSINI_ISS_NAC). */
/*        To simplify the problem, restrict the search to a short time */
/*        period where continuous Cassini bus attitude data are */
/*        available. */

/*        Use default SPICELIB progress reporting. */

/*        Use a step size of 1 second to reduce chances of missing */
/*        short visibility events and to make the search slow enough */
/*        so the progress report's updates are visible. */

/*        Use the meta-kernel shown below to load the required SPICE */
/*        kernels. */


/*           KPL/MK */

/*           File name: gffove_ex1.tm */

/*           This meta-kernel is intended to support operation of SPICE */
/*           example programs. The kernels shown here should not be */
/*           assumed to contain adequate or correct versions of data */
/*           required by SPICE-based user applications. */

/*           In order for an application to use this meta-kernel, the */
/*           kernels referenced here must be present in the user's */
/*           current working directory. */

/*           The names and contents of the kernels referenced */
/*           by this meta-kernel are as follows: */

/*              File name                       Contents */
/*              -----------------------------   ---------------------- */
/*              naif0012.tls                    Leapseconds */
/*              pck00010.tpc                    Satellite orientation */
/*                                              and radii */
/*              041014R_SCPSE_01066_04199.bsp   CASSINI, planetary and */
/*                                              Saturn satellite */
/*                                              ephemeris */
/*              cas_v40.tf                      Cassini FK */
/*              04161_04164ra.bc                Cassini bus CK */
/*              cas00071.tsc                    Cassini SCLK kernel */
/*              cas_iss_v10.ti                  Cassini IK */


/*           \begindata */

/*              KERNELS_TO_LOAD = ( 'naif0012.tls', */
/*                                  'pck00010.tpc', */
/*                                  '041014R_SCPSE_01066_04199.bsp', */
/*                                  'cas_v40.tf', */
/*                                  '04161_04164ra.bc', */
/*                                  'cas00071.tsc', */
/*                                  'cas_iss_v10.ti'            ) */
/*           \begintext */

/*           End of meta-kernel */


/*        Example code begins here. */


/*              PROGRAM GFFOVE_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     SPICELIB functions */
/*        C */
/*              INTEGER               WNCARD */

/*        C */
/*        C     SPICELIB default functions for */
/*        C */
/*        C        - Interrupt handling (no-op function):   GFBAIL */
/*        C        - Search refinement:                     GFREFN */
/*        C        - Progress report termination:           GFREPF */
/*        C        - Progress report initialization:        GFREPI */
/*        C        - Progress report update:                GFREPU */
/*        C        - Search step size "get" function:       GFSTEP */
/*        C */
/*              EXTERNAL              GFBAIL */
/*              EXTERNAL              GFREFN */
/*              EXTERNAL              GFREPF */
/*              EXTERNAL              GFREPI */
/*              EXTERNAL              GFREPU */
/*              EXTERNAL              GFSTEP */

/*        C */
/*        C     Local parameters */
/*        C */
/*              CHARACTER*(*)         META */
/*              PARAMETER           ( META   = 'gffove_ex1.tm' ) */

/*              CHARACTER*(*)         TIMFMT */
/*              PARAMETER           ( TIMFMT = */
/*             .      'YYYY-MON-DD HR:MN:SC.######::TDB' ) */

/*              INTEGER               LBCELL */
/*              PARAMETER           ( LBCELL = -5 ) */

/*              INTEGER               MAXWIN */
/*              PARAMETER           ( MAXWIN = 10000 ) */

/*              INTEGER               CORLEN */
/*              PARAMETER           ( CORLEN = 10 ) */

/*              INTEGER               BDNMLN */
/*              PARAMETER           ( BDNMLN = 36 ) */

/*              INTEGER               FRNMLN */
/*              PARAMETER           ( FRNMLN = 32 ) */

/*              INTEGER               SHPLEN */
/*              PARAMETER           ( SHPLEN = 25 ) */

/*              INTEGER               TIMLEN */
/*              PARAMETER           ( TIMLEN = 35 ) */

/*              INTEGER               LNSIZE */
/*              PARAMETER           ( LNSIZE = 80 ) */

/*        C */
/*        C     Local variables */
/*        C */
/*              CHARACTER*(CORLEN)    ABCORR */
/*              CHARACTER*(BDNMLN)    INST */
/*              CHARACTER*(LNSIZE)    LINE */
/*              CHARACTER*(BDNMLN)    OBSRVR */
/*              CHARACTER*(BDNMLN)    TARGET */
/*              CHARACTER*(FRNMLN)    TFRAME */
/*              CHARACTER*(TIMLEN)    TIMSTR ( 2 ) */
/*              CHARACTER*(SHPLEN)    TSHAPE */

/*              DOUBLE PRECISION      CNFINE ( LBCELL : 2 ) */
/*              DOUBLE PRECISION      ENDPT  ( 2 ) */
/*              DOUBLE PRECISION      ET0 */
/*              DOUBLE PRECISION      ET1 */
/*              DOUBLE PRECISION      RAYDIR ( 3 ) */
/*              DOUBLE PRECISION      RESULT ( LBCELL : MAXWIN ) */
/*              DOUBLE PRECISION      TOL */

/*              INTEGER               I */
/*              INTEGER               J */
/*              INTEGER               N */

/*              LOGICAL               BAIL */
/*              LOGICAL               RPT */

/*        C */
/*        C     Saved variables */
/*        C */
/*        C     The confinement and result windows CNFINE and RESULT are */
/*        C     saved because this practice helps to prevent stack */
/*        C     overflow. */
/*        C */
/*              SAVE                  CNFINE */
/*              SAVE                  RESULT */

/*        C */
/*        C     Since we're treating the target as an ephemeris object, */
/*        C     the ray direction is unused. We simply initialize the */
/*        C     direction vector to avoid portability problems. */
/*        C */
/*              DATA                  RAYDIR / 3*0.D0 / */

/*        C */
/*        C     Load kernels. */
/*        C */
/*              CALL FURNSH ( META ) */

/*        C */
/*        C     Initialize windows. */
/*        C */
/*              CALL SSIZED ( 2,      CNFINE ) */
/*              CALL SSIZED ( MAXWIN, RESULT ) */

/*        C */
/*        C     Insert search time interval bounds into the */
/*        C     confinement window. */
/*        C */
/*              CALL STR2ET ( '2004 JUN 11 06:30:00 TDB', ET0 ) */
/*              CALL STR2ET ( '2004 JUN 11 12:00:00 TDB', ET1 ) */

/*              CALL WNINSD ( ET0, ET1, CNFINE ) */

/*        C */
/*        C     Initialize inputs for the search. */
/*        C */
/*              INST   = 'CASSINI_ISS_NAC' */
/*              TARGET = 'PHOEBE' */
/*              TSHAPE = 'ELLIPSOID' */
/*              TFRAME = 'IAU_PHOEBE' */
/*              ABCORR = 'LT+S' */
/*              OBSRVR = 'CASSINI' */

/*        C */
/*        C     Use a particularly short step size to make the progress */
/*        C     report's updates visible. */
/*        C */
/*        C     Pass the step size (1 second) to the GF default step */
/*        C     size put/get system. */
/*        C */
/*              CALL GFSSTP ( 1.D0 ) */

/*        C */
/*        C     Set the convergence tolerance to 1 microsecond. */
/*        C */
/*              TOL    = 1.D-6 */

/*        C */
/*        C     Use progress reporting; turn off interrupt handling. */
/*        C */
/*              RPT  = .TRUE. */
/*              BAIL = .FALSE. */

/*              WRITE (*,*) ' ' */
/*              WRITE (*, '(A)' ) 'Instrument: '//INST */
/*              WRITE (*, '(A)' ) 'Target:     '//TARGET */

/*        C */
/*        C     Perform the search. */
/*        C */
/*              CALL GFFOVE ( INST,    TSHAPE,  RAYDIR, */
/*             .              TARGET,  TFRAME,  ABCORR,  OBSRVR, */
/*             .              TOL,     GFSTEP,  GFREFN,  RPT, */
/*             .              GFREPI,  GFREPU,  GFREPF,  BAIL, */
/*             .              GFBAIL,  CNFINE,  RESULT          ) */

/*              N = WNCARD( RESULT ) */

/*              IF ( N .EQ. 0 ) THEN */

/*                 WRITE (*, '(A)' ) 'No FOV intersection found.' */

/*              ELSE */

/*                 WRITE (*, '(A)' ) '  Visibility start time (TDB)' */
/*             .    //               '           Stop time (TDB)' */
/*                 WRITE (*, '(A)' ) '  ---------------------------' */
/*             .    //               '     ---------------------------' */

/*                 DO I = 1, N */

/*                    CALL WNFETD ( RESULT, I, ENDPT(1), ENDPT(2) ) */

/*                    DO J = 1, 2 */
/*                       CALL TIMOUT ( ENDPT(J), TIMFMT, TIMSTR(J) ) */
/*                    END DO */

/*                    LINE( :3) = ' ' */
/*                    LINE(2: ) = TIMSTR(1) */
/*                    LINE(34:) = TIMSTR(2) */

/*                    WRITE (*,*) LINE */

/*                 END DO */

/*              END IF */

/*              WRITE (*,*) ' ' */
/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        Instrument: CASSINI_ISS_NAC */
/*        Target:     PHOEBE */

/*        Target visibility search 100.00% done. */

/*          Visibility start time (TDB)           Stop time (TDB) */
/*          ---------------------------     --------------------------- */
/*          2004-JUN-11 07:35:27.066980     2004-JUN-11 08:48:03.954696 */
/*          2004-JUN-11 09:02:56.580045     2004-JUN-11 09:35:04.038509 */
/*          2004-JUN-11 09:49:56.476397     2004-JUN-11 10:22:04.242879 */
/*          2004-JUN-11 10:36:56.283771     2004-JUN-11 11:09:04.397165 */
/*          2004-JUN-11 11:23:56.020645     2004-JUN-11 11:56:04.733536 */


/*        Note that the progress report has the format shown below: */

/*           Target visibility search   6.02% done. */

/*        The completion percentage was updated approximately once per */
/*        second. */

/*        When the program was interrupted at an arbitrary time, */
/*        the output was: */

/*           Target visibility search  13.63% done. */
/*           Search was interrupted. */

/*        This message was written after an interrupt signal */
/*        was trapped. By default, the program would have terminated */
/*        before this message could be written. */

/*     2) A variation of example (1): search the same confinement */
/*        window for times when a selected background star is visible. */
/*        We use the FOV of the Cassini ISS wide angle camera */
/*        (CASSINI_ISS_WAC) to enhance the probability of viewing the */
/*        star. */

/*        The star we'll use has catalog number 6000 in the Hipparcos */
/*        Catalog. The star's J2000 right ascension and declination, */
/*        proper motion, and parallax are taken from that catalog. */

/*        Use the meta-kernel from the first example. */

/*        Example code begins here. */


/*              PROGRAM GFFOVE_EX2 */
/*              IMPLICIT NONE */

/*        C */
/*        C     SPICELIB functions */
/*        C */
/*              DOUBLE PRECISION      J1950 */
/*              DOUBLE PRECISION      J2000 */
/*              DOUBLE PRECISION      JYEAR */
/*              DOUBLE PRECISION      RPD */

/*              INTEGER               WNCARD */

/*        C */
/*        C     SPICELIB default functions for */
/*        C */
/*        C        - Interrupt handling (no-op function):   GFBAIL */
/*        C        - Search refinement:                     GFREFN */
/*        C        - Progress report termination:           GFREPF */
/*        C        - Progress report initialization:        GFREPI */
/*        C        - Progress report update:                GFREPU */
/*        C        - Search step size "get" function:       GFSTEP */
/*        C */
/*              EXTERNAL              GFBAIL */
/*              EXTERNAL              GFREFN */
/*              EXTERNAL              GFREPF */
/*              EXTERNAL              GFREPI */
/*              EXTERNAL              GFREPU */
/*              EXTERNAL              GFSTEP */

/*        C */
/*        C     Local parameters */
/*        C */
/*              CHARACTER*(*)         META */
/*              PARAMETER           ( META   = 'gffove_ex1.tm' ) */

/*              CHARACTER*(*)         TIMFMT */
/*              PARAMETER           ( TIMFMT = */
/*             .      'YYYY-MON-DD HR:MN:SC.######::TDB' ) */


/*              DOUBLE PRECISION      AU */
/*              PARAMETER           ( AU     = 149597870.693D0 ) */

/*              INTEGER               LBCELL */
/*              PARAMETER           ( LBCELL = -5 ) */

/*              INTEGER               MAXWIN */
/*              PARAMETER           ( MAXWIN = 10000 ) */

/*              INTEGER               CORLEN */
/*              PARAMETER           ( CORLEN = 10 ) */

/*              INTEGER               BDNMLN */
/*              PARAMETER           ( BDNMLN = 36 ) */

/*              INTEGER               FRNMLN */
/*              PARAMETER           ( FRNMLN = 32 ) */

/*              INTEGER               SHPLEN */
/*              PARAMETER           ( SHPLEN = 25 ) */

/*              INTEGER               TIMLEN */
/*              PARAMETER           ( TIMLEN = 35 ) */

/*              INTEGER               LNSIZE */
/*              PARAMETER           ( LNSIZE = 80 ) */

/*        C */
/*        C     Local variables */
/*        C */
/*              CHARACTER*(CORLEN)    ABCORR */
/*              CHARACTER*(BDNMLN)    INST */
/*              CHARACTER*(LNSIZE)    LINE */
/*              CHARACTER*(BDNMLN)    OBSRVR */
/*              CHARACTER*(FRNMLN)    RFRAME */
/*              CHARACTER*(BDNMLN)    TARGET */
/*              CHARACTER*(TIMLEN)    TIMSTR ( 2 ) */
/*              CHARACTER*(SHPLEN)    TSHAPE */

/*              DOUBLE PRECISION      CNFINE ( LBCELL : 2 ) */
/*              DOUBLE PRECISION      DEC */
/*              DOUBLE PRECISION      DECEPC */
/*              DOUBLE PRECISION      DECPM */
/*              DOUBLE PRECISION      DECDEG */
/*              DOUBLE PRECISION      DECDG0 */
/*              DOUBLE PRECISION      DTDEC */
/*              DOUBLE PRECISION      DTRA */
/*              DOUBLE PRECISION      ENDPT  ( 2 ) */
/*              DOUBLE PRECISION      ET0 */
/*              DOUBLE PRECISION      ET1 */
/*              DOUBLE PRECISION      LT */
/*              DOUBLE PRECISION      PARLAX */
/*              DOUBLE PRECISION      PLXDEG */
/*              DOUBLE PRECISION      POS    ( 3 ) */
/*              DOUBLE PRECISION      PSTAR  ( 3 ) */
/*              DOUBLE PRECISION      RA */
/*              DOUBLE PRECISION      RADEG */
/*              DOUBLE PRECISION      RADEG0 */
/*              DOUBLE PRECISION      RAEPC */
/*              DOUBLE PRECISION      RAPM */
/*              DOUBLE PRECISION      RAYDIR ( 3 ) */
/*              DOUBLE PRECISION      RESULT ( LBCELL : MAXWIN ) */
/*              DOUBLE PRECISION      RSTAR */
/*              DOUBLE PRECISION      T */
/*              DOUBLE PRECISION      TOL */

/*              INTEGER               CATNO */
/*              INTEGER               I */
/*              INTEGER               J */
/*              INTEGER               N */

/*              LOGICAL               BAIL */
/*              LOGICAL               RPT */

/*        C */
/*        C     Saved variables */
/*        C */
/*        C     The confinement and result windows CNFINE and RESULT are */
/*        C     saved because this practice helps to prevent stack */
/*        C     overflow. */
/*        C */
/*              SAVE                  CNFINE */
/*              SAVE                  RESULT */

/*        C */
/*        C     Load kernels. */
/*        C */
/*              CALL FURNSH ( META ) */

/*        C */
/*        C     Initialize windows. */
/*        C */
/*              CALL SSIZED ( 2,      CNFINE ) */
/*              CALL SSIZED ( MAXWIN, RESULT ) */

/*        C */
/*        C     Insert search time interval bounds into the */
/*        C     confinement window. */
/*        C */
/*              CALL STR2ET ( '2004 JUN 11 06:30:00 TDB', ET0 ) */
/*              CALL STR2ET ( '2004 JUN 11 12:00:00 TDB', ET1 ) */

/*              CALL WNINSD ( ET0, ET1, CNFINE ) */

/*        C */
/*        C     Initialize inputs for the search. */
/*        C */
/*              INST   = 'CASSINI_ISS_WAC' */
/*              TARGET = ' ' */
/*              TSHAPE = 'RAY' */

/*        C */
/*        C     Create a unit direction vector pointing from */
/*        C     observer to star. We'll assume the direction */
/*        C     is constant during the confinement window, and */
/*        C     we'll use et0 as the epoch at which to compute the */
/*        C     direction from the spacecraft to the star. */
/*        C */
/*        C     The data below are for the star with catalog */
/*        C     number 6000 in the Hipparcos catalog. Angular */
/*        C     units are degrees; epochs have units of Julian */
/*        C     years and have a reference epoch of J1950. */
/*        C     The reference frame is J2000. */
/*        C */
/*              CATNO  = 6000 */

/*              PLXDEG = 0.000001056D0 */

/*              RADEG0 = 19.290789927D0 */
/*              RAPM   = -0.000000720D0 */
/*              RAEPC  = 41.2000D0 */

/*              DECDG0 =  2.015271007D0 */
/*              DECPM  =  0.000001814D0 */
/*              DECEPC = 41.1300D0 */

/*              RFRAME = 'J2000' */

/*        C */
/*        C     Correct the star's direction for proper motion. */
/*        C */
/*        C     The argument t represents et0 as Julian years */
/*        C     past J1950. */
/*        C */
/*              T      =      ET0/JYEAR() */
/*             .         +  ( J2000()- J1950() ) / 365.25D0 */

/*              DTRA   = T - RAEPC */
/*              DTDEC  = T - DECEPC */

/*              RADEG  = RADEG0  +  DTRA  * RAPM */
/*              DECDEG = DECDG0  +  DTDEC * DECPM */

/*              RA     = RADEG  * RPD() */
/*              DEC    = DECDEG * RPD() */

/*              CALL RADREC ( 1.D0, RA, DEC, PSTAR ) */

/*        C */
/*        C     Correct star position for parallax applicable at */
/*        C     the Cassini orbiter's position. (The parallax effect */
/*        C     is negligible in this case; we're simply demonstrating */
/*        C     the computation.) */
/*        C */
/*              PARLAX = PLXDEG * RPD() */
/*              RSTAR  = AU / TAN(PARLAX) */

/*        C */
/*        C     Scale the star's direction vector by its distance from */
/*        C     the solar system barycenter. Subtract off the position */
/*        C     of the spacecraft relative to the solar system */
/*        C     barycenter; the result is the ray's direction vector. */
/*        C */
/*              CALL VSCLIP ( RSTAR, PSTAR ) */

/*              CALL SPKPOS ( 'CASSINI', ET0, 'J2000',  'NONE', */
/*             .              'SOLAR SYSTEM BARYCENTER', POS,  LT ) */

/*              CALL VSUB   ( PSTAR, POS, RAYDIR ) */

/*        C */
/*        C     Correct the star direction for stellar aberration when */
/*        C     we conduct the search. */
/*        C */
/*              ABCORR = 'S' */
/*              OBSRVR = 'CASSINI' */

/*        C */
/*        C     Use a particularly short step size to make the progress */
/*        C     report's updates visible. */
/*        C */
/*        C     Pass the step size (1 second) to the GF default step size */
/*        C     put/get system. */
/*        C */
/*              CALL GFSSTP ( 1.D0 ) */

/*        C */
/*        C     Set the convergence tolerance to 1 microsecond. */
/*        C */
/*              TOL = 1.D-6 */

/*        C */
/*        C     Use progress reporting; turn off interrupt handling. */
/*        C */
/*              RPT  = .TRUE. */
/*              BAIL = .FALSE. */


/*              WRITE (*,*) ' ' */
/*              WRITE (*,*) 'Instrument:              '//INST */
/*              WRITE (*,*) 'Star''s catalog number:  ', CATNO */

/*        C */
/*        C     Perform the search. */
/*        C */
/*              CALL GFFOVE ( INST,    TSHAPE,  RAYDIR, */
/*             .              TARGET,  RFRAME,  ABCORR,  OBSRVR, */
/*             .              TOL,     GFSTEP,  GFREFN,  RPT, */
/*             .              GFREPI,  GFREPU,  GFREPF,  BAIL, */
/*             .              GFBAIL,  CNFINE,  RESULT          ) */

/*              N = WNCARD( RESULT ) */

/*              IF ( N .EQ. 0 ) THEN */

/*                 WRITE (*,*) 'No FOV intersection found.' */

/*              ELSE */

/*                 WRITE (*, '(A)' ) '  Visibility start time (TDB)' */
/*             .    //               '           Stop time (TDB)' */
/*                 WRITE (*, '(A)' ) '  ---------------------------' */
/*             .    //               '     ---------------------------' */

/*                 DO I = 1, N */

/*                    CALL WNFETD ( RESULT, I, ENDPT(1), ENDPT(2) ) */

/*                    DO J = 1, 2 */
/*                       CALL TIMOUT ( ENDPT(J), TIMFMT, TIMSTR(J) ) */
/*                    END DO */

/*                    LINE( :3) = ' ' */
/*                    LINE(2: ) = TIMSTR(1) */
/*                    LINE(34:) = TIMSTR(2) */

/*                    WRITE (*,*) LINE */

/*                 END DO */

/*              END IF */

/*              WRITE (*,*) ' ' */
/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Instrument:              CASSINI_ISS_WAC */
/*         Star's catalog number:          6000 */

/*        Target visibility search 100.00% done. */

/*          Visibility start time (TDB)           Stop time (TDB) */
/*          ---------------------------     --------------------------- */
/*          2004-JUN-11 06:30:00.000000     2004-JUN-11 12:00:00.000000 */


/* $ Restrictions */

/*     1)  The kernel files to be used by GFFOVE must be loaded (normally */
/*         via the SPICELIB routine FURNSH) before GFFOVE is called. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     L.S. Elson         (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.2, 06-AUG-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/*        Modified code examples' output to comply with maximum line */
/*        length of header comments. Updated Examples' kernels set to use */
/*        PDS archived data. Added SAVE statements for CNFINE and RESULT */
/*        variables in code examples. */

/*        Updated description of RESULT argument in $Brief_I/O, */
/*        $Detailed_Input and $Detailed_Output. */

/*        Added entries #17 and #22 in $Exceptions section. */

/*        Corrected reporting message in UDREPI description. */

/* -    SPICELIB Version 1.0.1, 17-JAN-2017 (NJB) (JDR) */

/*        Fixed typo in second example program: initial letter */
/*        "C" indicating a comment line was in lower case. */

/* -    SPICELIB Version 1.0.0, 15-APR-2009 (NJB) (LSE) (EDW) */

/* -& */
/* $ Index_Entries */

/*     GF mid-level target in instrument FOV search */

/* -& */

/*     SPICELIB functions */


/*     External routines */


/*     Local parameters */


/*     STEP is a step size initializer for the unused, dummy step size */
/*     argument to ZZGFSOLV. The routine UDSTEP, which is passed to */
/*     ZZGFSOLV, will be used by that routine to obtain the step size. */


/*     CSTEP indicates whether a constant step size, provided */
/*     via the input argument STEP, is to be used by ZZGFSOLV. */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("GFFOVE", (ftnlen)6);

/*     Check the result window's size. */

    if (sized_(result) < 2) {
	setmsg_("Result window size must be at least 2 but was #.", (ftnlen)
		48);
	i__1 = sized_(result);
	errint_("#", &i__1, (ftnlen)1);
	sigerr_("SPICE(WINDOWTOOSMALL)", (ftnlen)21);
	chkout_("GFFOVE", (ftnlen)6);
	return 0;
    }

/*     Empty the RESULT window. */

    scardd_(&c__0, result);

/*     Check the convergence tolerance. */

    if (*tol <= 0.) {
	setmsg_("Tolerance must be positive but was #.", (ftnlen)37);
	errdp_("#", tol, (ftnlen)1);
	sigerr_("SPICE(INVALIDTOLERANCE)", (ftnlen)23);
	chkout_("GFFOVE", (ftnlen)6);
	return 0;
    }

/*     Note to maintenance programmer: most input exception checks are */
/*     delegated to ZZGFFVIN. If the implementation of that routine */
/*     changes, or if this routine is modified to call a different */
/*     routine in place of ZZGFFVIN, then the error handling performed */
/*     by ZZGFFVIN will have to be performed here or in a routine called */
/*     by this routine. */


/*     Initialize the visibility calculation. */

    zzgffvin_(inst, tshape, raydir, target, tframe, abcorr, obsrvr, inst_len, 
	    tshape_len, target_len, tframe_len, abcorr_len, obsrvr_len);
    if (failed_()) {
	chkout_("GFFOVE", (ftnlen)6);
	return 0;
    }

/*     Prepare the progress reporter if appropriate. */

    if (*rpt) {
	(*udrepi)(cnfine, "Target visibility search ", "done.", (ftnlen)25, (
		ftnlen)5);
    }

/*     Cycle over the intervals in the confinement window. */

    count = wncard_(cnfine);
    i__1 = count;
    for (i__ = 1; i__ <= i__1; ++i__) {

/*        Retrieve the bounds for the Ith interval of the confinement */
/*        window. Search this interval for visibility events. Union the */
/*        result with the contents of the RESULT window. */

	wnfetd_(cnfine, &i__, &start, &finish);
	zzgfsolv_((U_fp)zzgffvst_, (U_fp)udstep, (U_fp)udrefn, bail, (L_fp)
		udbail, &c_false, &c_b16, &start, &finish, tol, rpt, (U_fp)
		udrepu, result);
	if (failed_()) {
	    chkout_("GFFOVE", (ftnlen)6);
	    return 0;
	}
	if (*bail) {

/*           Interrupt handling is enabled. */

	    if ((*udbail)()) {

/*              An interrupt has been issued. Return now regardless of */
/*              whether the search has been completed. */

		chkout_("GFFOVE", (ftnlen)6);
		return 0;
	    }
	}
    }

/*     End the progress report. */

    if (*rpt) {
	(*udrepf)();
    }
    chkout_("GFFOVE", (ftnlen)6);
    return 0;
} /* gffove_ */

