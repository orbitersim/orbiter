/* gfoclt.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c_n1 = -1;
static integer c__3 = 3;
static logical c_false = FALSE_;

/* $Procedure GFOCLT ( GF, find occultation ) */
/* Subroutine */ int gfoclt_(char *occtyp, char *front, char *fshape, char *
	fframe, char *back, char *bshape, char *bframe, char *abcorr, char *
	obsrvr, doublereal *step, doublereal *cnfine, doublereal *result, 
	ftnlen occtyp_len, ftnlen front_len, ftnlen fshape_len, ftnlen 
	fframe_len, ftnlen back_len, ftnlen bshape_len, ftnlen bframe_len, 
	ftnlen abcorr_len, ftnlen obsrvr_len)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern integer sized_(doublereal *);
    extern logical gfbail_();
    extern /* Subroutine */ int gfocce_(char *, char *, char *, char *, char *
	    , char *, char *, char *, char *, doublereal *, U_fp, U_fp, 
	    logical *, U_fp, U_fp, U_fp, logical *, L_fp, doublereal *, 
	    doublereal *, ftnlen, ftnlen, ftnlen, ftnlen, ftnlen, ftnlen, 
	    ftnlen, ftnlen, ftnlen);
    logical ok;
    extern /* Subroutine */ int gfrefn_(), gfrepf_(), gfrepi_(), gfrepu_(), 
	    gfstep_();
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen), sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int gfsstp_(doublereal *);
    doublereal tol;
    extern /* Subroutine */ int zzholdd_(integer *, integer *, logical *, 
	    doublereal *);

/* $ Abstract */

/*     Determine time intervals when an observer sees one target body */
/*     occulted by, or in transit across, another. */

/*     The surfaces of the target bodies may be represented by triaxial */
/*     ellipsoids or by topographic data provided by DSK files. */

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
/*     SPK */
/*     TIME */
/*     WINDOWS */

/* $ Keywords */

/*     EVENT */
/*     GEOMETRY */
/*     OCCULTATION */
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

/* $ Abstract */

/*     SPICE private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     This file contains parameter declarations for the ZZHOLDD */
/*     routine. */

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

/*     None. */

/* $ Keywords */

/*     None. */

/* $ Declarations */

/*     None. */

/* $ Brief_I/O */

/*     None. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     GEN       general value, primarily for testing. */

/*     GF_REF    user defined GF reference value. */

/*     GF_TOL    user defined GF convergence tolerance. */

/*     GF_DT     user defined GF step for numeric differentiation. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     E.D. Wright    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0  03-DEC-2013 (EDW) */

/* -& */

/*     OP codes. The values exist in the integer domain */
/*     [ -ZZNOP, -1], */


/*     Current number of OP codes. */


/*     ID codes. The values exist in the integer domain */
/*     [ 1, NID], */


/*     General use, primarily testing. */


/*     The user defined GF reference value. */


/*     The user defined GF convergence tolerance. */


/*     The user defined GF step for numeric differentiation. */


/*     Current number of ID codes, dimension of array */
/*     in ZZHOLDD. Bad things can happen if this parameter */
/*     does not have the proper value. */


/*     End of file zzholdd.inc. */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     LBCELL     P   SPICE Cell lower bound. */
/*     CNVTOL     P   Convergence tolerance. */
/*     ZZGET      P   ZZHOLDD retrieves a stored DP value. */
/*     GF_TOL     P   ZZHOLDD acts on the GF subsystem tolerance. */
/*     OCCTYP     I   Type of occultation. */
/*     FRONT      I   Name of body occulting the other. */
/*     FSHAPE     I   Type of shape model used for front body. */
/*     FFRAME     I   Body-fixed, body-centered frame for front body. */
/*     BACK       I   Name of body occulted by the other. */
/*     BSHAPE     I   Type of shape model used for back body. */
/*     BFRAME     I   Body-fixed, body-centered frame for back body. */
/*     ABCORR     I   Aberration correction flag. */
/*     OBSRVR     I   Name of the observing body. */
/*     STEP       I   Step size in seconds for finding occultation */
/*                    events. */
/*     CNFINE     I   SPICE window to which the search is restricted. */
/*     RESULT    I-O  SPICE window containing results. */

/* $ Detailed_Input */

/*     OCCTYP   indicates the type of occultation that is to be found. */
/*              Note that transits are considered to be a type of */
/*              occultation. */

/*              Supported values and corresponding definitions are: */

/*                 'FULL'      denotes the full occultation of the */
/*                             body designated by BACK by the body */
/*                             designated by FRONT, as seen from the */
/*                             location of the observer. In other */
/*                             words, the occulted body is completely */
/*                             invisible as seen from the observer's */
/*                             location. */

/*                 'ANNULAR'   denotes an annular occultation: the */
/*                             body designated by FRONT blocks part */
/*                             of, but not the limb of, the body */
/*                             designated by BACK, as seen from the */
/*                             location of the observer. */

/*                 'PARTIAL'   denotes a partial, non-annular */
/*                             occultation: the body designated by */
/*                             FRONT blocks part, but not all, of the */
/*                             limb of the body designated by BACK, as */
/*                             seen from the location of the observer. */

/*                 'ANY'       denotes any of the above three types of */
/*                             occultations: 'PARTIAL', 'ANNULAR', or */
/*                             'FULL'. */

/*                             'ANY' should be used to search for */
/*                             times when the body designated by FRONT */
/*                             blocks any part of the body designated */
/*                             by BACK. */

/*                             The option 'ANY' must be used if either */
/*                             the front or back target body is */
/*                             modeled as a point. */

/*              Case and leading or trailing blanks are not */
/*              significant in the string OCCTYP. */

/*     FRONT    is the name of the target body that occults---that is, */
/*              passes in front of---the other. Optionally, you may */
/*              supply the integer NAIF ID code for the body as a */
/*              string. For example both 'MOON' and '301' are */
/*              legitimate strings that designate the Moon. */

/*              Case and leading or trailing blanks are not */
/*              significant in the string FRONT. */

/*     FSHAPE   is a string indicating the geometric model used to */
/*              represent the shape of the front target body. The */
/*              supported options are: */

/*                 'ELLIPSOID' */

/*                     Use a triaxial ellipsoid model with radius */
/*                     values provided via the kernel pool. A kernel */
/*                     variable having a name of the form */

/*                        BODYnnn_RADII */

/*                     where nnn represents the NAIF integer code */
/*                     associated with the body, must be present in */
/*                     the kernel pool. This variable must be */
/*                     associated with three numeric values giving the */
/*                     lengths of the ellipsoid's X, Y, and Z */
/*                     semi-axes. */

/*                 'POINT' */

/*                     Treat the body as a single point. When a point */
/*                     target is specified, the occultation type must */
/*                     be set to 'ANY'. */

/*                 'DSK/UNPRIORITIZED[/SURFACES = <surface list>]' */

/*                     Use topographic data provided by DSK files to */
/*                     model the body's shape. These data must be */
/*                     provided by loaded DSK files. */

/*                     The surface list specification is optional. The */
/*                     syntax of the list is */

/*                        <surface 1> [, <surface 2>...] */

/*                     If present, it indicates that data only for the */
/*                     listed surfaces are to be used; however, data */
/*                     need not be available for all surfaces in the */
/*                     list. If absent, loaded DSK data for any surface */
/*                     associated with the target body are used. */

/*                     The surface list may contain surface names or */
/*                     surface ID codes. Names containing blanks must */
/*                     be delimited by double quotes, for example */

/*                        SURFACES = "Mars MEGDR 128 PIXEL/DEG" */

/*                     If multiple surfaces are specified, their names */
/*                     or IDs must be separated by commas. */

/*                     See the $Particulars section below for details */
/*                     concerning use of DSK data. */

/*              The combinations of the shapes of the target bodies */
/*              FRONT and BACK must be one of: */

/*                 One ELLIPSOID, one POINT */
/*                 Two ELLIPSOIDs */
/*                 One DSK, one POINT */

/*              Case and leading or trailing blanks are not */
/*              significant in the string FSHAPE. */

/*     FFRAME   is the name of the body-fixed, body-centered reference */
/*              frame associated with the front target body. Examples of */
/*              such names are 'IAU_SATURN' (for Saturn) and 'ITRF93' */
/*              (for the Earth). */

/*              If the front target body is modeled as a point, FFRAME */
/*              should be left blank. */

/*              Case and leading or trailing blanks bracketing a */
/*              non-blank frame name are not significant in the string */
/*              FFRAME. */

/*     BACK     is the name of the target body that is occulted */
/*              by---that is, passes in back of---the other. */
/*              Optionally, you may supply the integer NAIF ID code */
/*              for the body as a string. For example both 'MOON' and */
/*              '301' are legitimate strings that designate the Moon. */

/*              Case and leading or trailing blanks are not */
/*              significant in the string BACK. */

/*     BSHAPE   is the shape specification for the body designated */
/*              by BACK. The supported options are those for */
/*              FSHAPE. See the description of FSHAPE above for */
/*              details. */

/*     BFRAME   is the name of the body-fixed, body-centered reference */
/*              frame associated with the ``back'' target body. Examples */
/*              of such names are 'IAU_SATURN' (for Saturn) and 'ITRF93' */
/*              (for the Earth). */

/*              If the back target body is modeled as a point, BFRAME */
/*              should be left blank. */

/*              Case and leading or trailing blanks bracketing a */
/*              non-blank frame name are not significant in the string */
/*              BFRAME. */

/*     ABCORR   indicates the aberration corrections to be applied to */
/*              the state of each target body to account for one-way */
/*              light time. Stellar aberration corrections are */
/*              ignored if specified, since these corrections don't */
/*              improve the accuracy of the occultation determination. */

/*              See the header of the SPICE routine SPKEZR for a */
/*              detailed description of the aberration correction */
/*              options. For convenience, the options supported by */
/*              this routine are listed below: */

/*                 'NONE'     Apply no correction. */

/*                 'LT'       "Reception" case: correct for */
/*                            one-way light time using a Newtonian */
/*                            formulation. */

/*                 'CN'       "Reception" case: converged */
/*                            Newtonian light time correction. */

/*                 'XLT'      "Transmission" case: correct for */
/*                            one-way light time using a Newtonian */
/*                            formulation. */

/*                 'XCN'      "Transmission" case: converged */
/*                            Newtonian light time correction. */

/*              Case and blanks are not significant in the string */
/*              ABCORR. */

/*     OBSRVR   is the name of the body from which the occultation is */
/*              observed. Optionally, you may supply the integer NAIF */
/*              ID code for the body as a string. */

/*              Case and leading or trailing blanks are not */
/*              significant in the string OBSRVR. */

/*     STEP     is the step size to be used in the search. STEP must */
/*              be shorter than any interval, within the confinement */
/*              window, over which the specified occultation condition */
/*              is met. In other words, STEP must be shorter than the */
/*              shortest occultation event that the user wishes to */
/*              detect; STEP must also be shorter than the shortest */
/*              time interval between two occultation events that */
/*              occur within the confinement window (see below). */
/*              However, STEP must not be *too* short, or the search */
/*              will take an unreasonable amount of time. */

/*              The choice of STEP affects the completeness but not */
/*              the precision of solutions found by this routine; the */
/*              precision is controlled by the convergence tolerance. */
/*              See the discussion of the parameter CNVTOL for */
/*              details. */

/*              STEP has units of TDB seconds. */

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
/*              discarded before GFOCLT conducts its search. */

/* $ Detailed_Output */

/*     RESULT   is a SPICE window representing the set of time intervals, */
/*              within the confinement window, when the specified */
/*              occultation occurs. */

/*              The endpoints of the time intervals comprising RESULT are */
/*              interpreted as seconds past J2000 TDB. */

/*              If no times within the confinement window satisfy the */
/*              search criteria, RESULT will be returned with a */
/*              cardinality of zero. */

/* $ Parameters */

/*     LBCELL   is the lower bound for SPICE cell arrays. */

/*     CNVTOL   is the convergence tolerance used for finding */
/*              endpoints of the intervals comprising the result */
/*              window. CNVTOL is used to determine when binary */
/*              searches for roots should terminate: when a root is */
/*              bracketed within an interval of length CNVTOL, the */
/*              root is considered to have been found. */

/*              The accuracy, as opposed to precision, of roots found */
/*              by this routine depends on the accuracy of the input */
/*              data. In most cases, the accuracy of solutions will be */
/*              inferior to their precision. */

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

/*     3)  If name of either target or the observer cannot be translated */
/*         to a NAIF ID code, an error is signaled by a routine */
/*         in the call tree of this routine. */

/*     4)  If the radii of a target body modeled as an ellipsoid cannot */
/*         be determined by searching the kernel pool for a kernel */
/*         variable having a name of the form */

/*            'BODYnnn_RADII' */

/*         where nnn represents the NAIF integer code associated with */
/*         the body, an error is signaled by a routine in the */
/*         call tree of this routine. */

/*     5)  If either of the target bodies FRONT or BACK coincides with */
/*         the observer body OBSRVR, an error is signaled by a */
/*         routine in the call tree of this routine. */

/*     6)  If the body designated by FRONT coincides with that */
/*         designated by BACK, an error is signaled by a routine */
/*         in the call tree of this routine. */

/*     7)  If either of the body model specifiers FSHAPE or BSHAPE */
/*         is not recognized, an error is signaled by a routine */
/*         in the call tree of this routine. */

/*     8)  If both of the body model specifiers FSHAPE and BSHAPE */
/*         specify point targets, an error is signaled by a */
/*         routine in the call tree of this routine. */

/*     9)  If one of the body model specifiers FSHAPE and BSHAPE */
/*         specifies a DSK model, and the other argument does not */
/*         specify a point target, an error is signaled by a routine in */
/*         the call tree of this routine. */

/*     10) If a target body-fixed reference frame associated with a */
/*         non-point target is not recognized, an error is signaled by a */
/*         routine in the call tree of this routine. */

/*     11) If a target body-fixed reference frame is not centered at the */
/*         corresponding target body, an error is signaled by a routine */
/*         in the call tree of this routine. */

/*     12) If the loaded kernels provide insufficient data to compute any */
/*         required state vector, an error is signaled by a routine in */
/*         the call tree of this routine. */

/*     13) If an error occurs while reading an SPK or other kernel file, */
/*         the error is signaled by a routine in the call tree */
/*         of this routine. */

/*     14) If a point target is specified and the occultation type is set */
/*         to a valid value other than 'ANY', an error is signaled by a */
/*         routine in the call tree of this routine. */

/*     15) If the output SPICE window RESULT has size less than 2, the */
/*         error SPICE(WINDOWTOOSMALL) is signaled. */

/*     16) If the output SPICE window RESULT has insufficient capacity */
/*         to contain the number of intervals on which the specified */
/*         occultation condition is met, an error is signaled */
/*         by a routine in the call tree of this routine. */

/*     17) If the occultation type OCCTYP is invalid, an error is */
/*         signaled by a routine in the call tree of this routine. */

/*     18) If the aberration correction specification ABCORR is invalid, */
/*         an error is signaled by a routine in the call tree of this */
/*         routine. */

/*     19) If either FSHAPE or BSHAPE specifies that the target surface */
/*         is represented by DSK data, and no DSK files are loaded for */
/*         the specified target, an error is signaled by a routine in */
/*         the call tree of this routine. */

/*     20) If either FSHAPE or BSHAPE specifies that the target surface */
/*         is represented by DSK data, but the shape specification is */
/*         invalid, an error is signaled by a routine in the call tree */
/*         of this routine. */

/* $ Files */

/*     Appropriate SPICE kernels must be loaded by the calling program */
/*     before this routine is called. */

/*     The following data are required: */

/*     -  SPK data: the calling application must load ephemeris data */
/*        for the targets, source and observer that cover the time */
/*        period specified by the window CNFINE. If aberration */
/*        corrections are used, the states of the target bodies and of */
/*        the observer relative to the solar system barycenter must be */
/*        calculable from the available ephemeris data. Typically */
/*        ephemeris data are made available by loading one or more SPK */
/*        files via FURNSH. */

/*     -  PCK data: bodies modeled as triaxial ellipsoids must have */
/*        semi-axis lengths provided by variables in the kernel pool. */
/*        Typically these data are made available by loading a text */
/*        PCK file via FURNSH. */

/*     -  FK data: if either of the reference frames designated by */
/*        BFRAME or FFRAME are not built in to the SPICE system, */
/*        one or more FKs specifying these frames must be loaded. */

/*     The following data may be required: */

/*     -  DSK data: if either FSHAPE or BSHAPE indicates that DSK */
/*        data are to be used, DSK files containing topographic data */
/*        for the target body must be loaded. If a surface list is */
/*        specified, data for at least one of the listed surfaces must */
/*        be loaded. */

/*     -  Surface name-ID associations: if surface names are specified */
/*        in FSHAPE or BSHAPE, the association of these names with */
/*        their corresponding surface ID codes must be established by */
/*        assignments of the kernel variables */

/*           NAIF_SURFACE_NAME */
/*           NAIF_SURFACE_CODE */
/*           NAIF_SURFACE_BODY */

/*        Normally these associations are made by loading a text */
/*        kernel containing the necessary assignments. An example */
/*        of such a set of assignments is */

/*           NAIF_SURFACE_NAME += 'Mars MEGDR 128 PIXEL/DEG' */
/*           NAIF_SURFACE_CODE += 1 */
/*           NAIF_SURFACE_BODY += 499 */

/*     -  CK data: either of the body-fixed frames to which FFRAME or */
/*        BFRAME refer might be a CK frame. If so, at least one CK */
/*        file will be needed to permit transformation of vectors */
/*        between that frame and the J2000 frame. */

/*     -  SCLK data: if a CK file is needed, an associated SCLK */
/*        kernel is required to enable conversion between encoded SCLK */
/*        (used to time-tag CK data) and barycentric dynamical time */
/*        (TDB). */

/*     Kernel data are normally loaded once per program run, NOT every */
/*     time this routine is called. */

/* $ Particulars */

/*     This routine provides a simpler, but less flexible, interface */
/*     than does the SPICELIB routine GFOCCE for conducting searches for */
/*     occultation events. Applications that require support for */
/*     progress reporting, interrupt handling, non-default step or */
/*     refinement functions, or non-default convergence tolerance should */
/*     call GFOCCE rather than this routine. */

/*     This routine determines a set of one or more time intervals */
/*     within the confinement window when a specified type of */
/*     occultation occurs. The resulting set of intervals is returned as */
/*     a SPICE window. */

/*     Below we discuss in greater detail aspects of this routine's */
/*     solution process that are relevant to correct and efficient */
/*     use of this routine in user applications. */


/*     The Search Process */
/*     ================== */

/*     The search for occultations is treated as a search for state */
/*     transitions: times are sought when the state of the BACK body */
/*     changes from "not occulted" to "occulted" or vice versa. */

/*     Step Size */
/*     ========= */

/*     Each interval of the confinement window is searched as follows: */
/*     first, the input step size is used to determine the time */
/*     separation at which the occultation state will be sampled. */
/*     Starting at the left endpoint of the interval, samples of the */
/*     occultation state will be taken at each step. If a state change */
/*     is detected, a root has been bracketed; at that point, the */
/*     "root"--the time at which the state change occurs---is found by a */
/*     refinement process, for example, via binary search. */

/*     Note that the optimal choice of step size depends on the lengths */
/*     of the intervals over which the occultation state is constant: */
/*     the step size should be shorter than the shortest occultation */
/*     duration and the shortest period between occultations, within */
/*     the confinement window. */

/*     Having some knowledge of the relative geometry of the targets and */
/*     observer can be a valuable aid in picking a reasonable step size. */
/*     In general, the user can compensate for lack of such knowledge by */
/*     picking a very short step size; the cost is increased computation */
/*     time. */

/*     Note that the step size is not related to the precision with which */
/*     the endpoints of the intervals of the result window are computed. */
/*     That precision level is controlled by the convergence tolerance. */


/*     Convergence Tolerance */
/*     ===================== */

/*     Once a root has been bracketed, a refinement process is used to */
/*     narrow down the time interval within which the root must lie. */
/*     This refinement process terminates when the location of the root */
/*     has been determined to within an error margin called the */
/*     "convergence tolerance." The default convergence tolerance */
/*     used by this routine is set by the parameter CNVTOL (defined */
/*     in gf.inc). */

/*     The value of CNVTOL is set to a "tight" value so that the */
/*     tolerance doesn't become the limiting factor in the accuracy of */
/*     solutions found by this routine. In general the accuracy of input */
/*     data will be the limiting factor. */

/*     The user may change the convergence tolerance from the default */
/*     CNVTOL value by calling the routine GFSTOL, e.g. */

/*        CALL GFSTOL( tolerance value ) */

/*     Call GFSTOL prior to calling this routine. All subsequent */
/*     searches will use the updated tolerance value. */

/*     Setting the tolerance tighter than CNVTOL is unlikely to be */
/*     useful, since the results are unlikely to be more accurate. */
/*     Making the tolerance looser will speed up searches somewhat, */
/*     since a few convergence steps will be omitted. However, in most */
/*     cases, the step size is likely to have a much greater effect */
/*     on processing time than would the convergence tolerance. */


/*     The Confinement Window */
/*     ====================== */

/*     The simplest use of the confinement window is to specify a time */
/*     interval within which a solution is sought. */

/*     The confinement window also can be used to restrict a search to */
/*     a time window over which required data (typically ephemeris */
/*     data, in the case of occultation searches) are known to be */
/*     available. */

/*     In some cases, the confinement window be used to make searches */
/*     more efficient. Sometimes it's possible to do an efficient search */
/*     to reduce the size of the time period over which a relatively */
/*     slow search of interest must be performed. See the "CASCADE" */
/*     example program in gf.req for a demonstration. */


/*     Using DSK data */
/*     ============== */

/*        DSK loading and unloading */
/*        ------------------------- */

/*        DSK files providing data used by this routine are loaded by */
/*        calling FURNSH and can be unloaded by calling UNLOAD or */
/*        KCLEAR. See the documentation of FURNSH for limits on numbers */
/*        of loaded DSK files. */

/*        For run-time efficiency, it's desirable to avoid frequent */
/*        loading and unloading of DSK files. When there is a reason to */
/*        use multiple versions of data for a given target body---for */
/*        example, if topographic data at varying resolutions are to be */
/*        used---the surface list can be used to select DSK data to be */
/*        used for a given computation. It is not necessary to unload */
/*        the data that are not to be used. This recommendation presumes */
/*        that DSKs containing different versions of surface data for a */
/*        given body have different surface ID codes. */


/*        DSK data priority */
/*        ----------------- */

/*        A DSK coverage overlap occurs when two segments in loaded DSK */
/*        files cover part or all of the same domain---for example, a */
/*        given longitude-latitude rectangle---and when the time */
/*        intervals of the segments overlap as well. */

/*        When DSK data selection is prioritized, in case of a coverage */
/*        overlap, if the two competing segments are in different DSK */
/*        files, the segment in the DSK file loaded last takes */
/*        precedence. If the two segments are in the same file, the */
/*        segment located closer to the end of the file takes */
/*        precedence. */

/*        When DSK data selection is unprioritized, data from competing */
/*        segments are combined. For example, if two competing segments */
/*        both represent a surface as sets of triangular plates, the */
/*        union of those sets of plates is considered to represent the */
/*        surface. */

/*        Currently only unprioritized data selection is supported. */
/*        Because prioritized data selection may be the default behavior */
/*        in a later version of the routine, the UNPRIORITIZED keyword is */
/*        required in the FSHAPE and BSHAPE arguments. */


/*        Syntax of the shape input arguments for the DSK case */
/*        ---------------------------------------------------- */

/*        The keywords and surface list in the target shape arguments */
/*        FSHAPE and BSHAPE, when DSK shape models are specified, are */
/*        called "clauses." The clauses may appear in any order, for */
/*        example */

/*           DSK/<surface list>/UNPRIORITIZED */
/*           DSK/UNPRIORITIZED/<surface list> */
/*           UNPRIORITIZED/<surface list>/DSK */

/*        The simplest form of a target argument specifying use of */
/*        DSK data is one that lacks a surface list, for example: */

/*           'DSK/UNPRIORITIZED' */

/*        For applications in which all loaded DSK data for the target */
/*        body are for a single surface, and there are no competing */
/*        segments, the above string suffices. This is expected to be */
/*        the usual case. */

/*        When, for the specified target body, there are loaded DSK */
/*        files providing data for multiple surfaces for that body, the */
/*        surfaces to be used by this routine for a given call must be */
/*        specified in a surface list, unless data from all of the */
/*        surfaces are to be used together. */

/*        The surface list consists of the string */

/*           SURFACES = */

/*        followed by a comma-separated list of one or more surface */
/*        identifiers. The identifiers may be names or integer codes in */
/*        string format. For example, suppose we have the surface */
/*        names and corresponding ID codes shown below: */

/*           Surface Name                              ID code */
/*           ------------                              ------- */
/*           'Mars MEGDR 128 PIXEL/DEG'                1 */
/*           'Mars MEGDR 64 PIXEL/DEG'                 2 */
/*           'Mars_MRO_HIRISE'                         3 */

/*        If data for all of the above surfaces are loaded, then */
/*        data for surface 1 can be specified by either */

/*           'SURFACES = 1' */

/*        or */

/*           'SURFACES = "Mars MEGDR 128 PIXEL/DEG"' */

/*        Double quotes are used to delimit the surface name because */
/*        it contains blank characters. */

/*        To use data for surfaces 2 and 3 together, any */
/*        of the following surface lists could be used: */

/*           'SURFACES = 2, 3' */

/*           'SURFACES = "Mars MEGDR  64 PIXEL/DEG", 3' */

/*           'SURFACES = 2, Mars_MRO_HIRISE' */

/*           'SURFACES = "Mars MEGDR 64 PIXEL/DEG", Mars_MRO_HIRISE' */

/*        An example of a shape argument that could be constructed */
/*        using one of the surface lists above is */

/*          'DSK/UNPRIORITIZED/SURFACES = "Mars MEGDR 64 PIXEL/DEG", 3' */

/* $ Examples */

/*     The numerical results shown for these examples may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Find occultations of the Sun by the Moon (that is, solar */
/*        eclipses) as seen from the center of the Earth over the month */
/*        December, 2001. */

/*        Use light time corrections to model apparent positions of Sun */
/*        and Moon. Stellar aberration corrections are not specified */
/*        because they don't affect occultation computations. */

/*        We select a step size of 3 minutes, which means we */
/*        ignore occultation events lasting less than 3 minutes, */
/*        if any exist. */

/*        Use the meta-kernel shown below to load the required SPICE */
/*        kernels. */


/*           KPL/MK */

/*           File name: gfoclt_ex1.tm */

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
/*              de421.bsp                     Planetary ephemeris */
/*              pck00008.tpc                  Planet orientation and */
/*                                            radii */
/*              naif0009.tls                  Leapseconds */


/*           \begindata */

/*              KERNELS_TO_LOAD = ( 'de421.bsp', */
/*                                  'pck00008.tpc', */
/*                                  'naif0009.tls'  ) */

/*           \begintext */

/*           End of meta-kernel */


/*        Example code begins here. */


/*              PROGRAM GFOCLT_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     SPICELIB functions */
/*        C */
/*              INTEGER               WNCARD */

/*        C */
/*        C     Local parameters */
/*        C */
/*              CHARACTER*(*)         TIMFMT */
/*              PARAMETER           ( TIMFMT = */
/*             .   'YYYY MON DD HR:MN:SC.###### (TDB)::TDB' ) */

/*              INTEGER               MAXWIN */
/*              PARAMETER           ( MAXWIN = 2 * 100 ) */

/*              INTEGER               TIMLEN */
/*              PARAMETER           ( TIMLEN = 40 ) */

/*              INTEGER               LBCELL */
/*              PARAMETER           ( LBCELL = -5 ) */

/*        C */
/*        C     Local variables */
/*        C */
/*              CHARACTER*(TIMLEN)    WIN0 */
/*              CHARACTER*(TIMLEN)    WIN1 */
/*              CHARACTER*(TIMLEN)    BEGSTR */
/*              CHARACTER*(TIMLEN)    ENDSTR */

/*              DOUBLE PRECISION      CNFINE ( LBCELL : 2 ) */
/*              DOUBLE PRECISION      ET0 */
/*              DOUBLE PRECISION      ET1 */
/*              DOUBLE PRECISION      LEFT */
/*              DOUBLE PRECISION      RESULT ( LBCELL : MAXWIN ) */
/*              DOUBLE PRECISION      RIGHT */
/*              DOUBLE PRECISION      STEP */

/*              INTEGER               I */

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
/*              CALL FURNSH ( 'gfoclt_ex1.tm' ) */

/*        C */
/*        C     Initialize the confinement and result windows. */
/*        C */
/*              CALL SSIZED ( 2,      CNFINE ) */
/*              CALL SSIZED ( MAXWIN, RESULT ) */

/*        C */
/*        C     Obtain the TDB time bounds of the confinement */
/*        C     window, which is a single interval in this case. */
/*        C */
/*              WIN0 = '2001 DEC 01 00:00:00 TDB' */
/*              WIN1 = '2002 JAN 01 00:00:00 TDB' */

/*              CALL STR2ET ( WIN0, ET0 ) */
/*              CALL STR2ET ( WIN1, ET1 ) */

/*        C */
/*        C     Insert the time bounds into the confinement */
/*        C     window. */
/*        C */
/*              CALL WNINSD ( ET0, ET1, CNFINE ) */

/*        C */
/*        C     Select a 3-minute step. We'll ignore any occultations */
/*        C     lasting less than 3 minutes. Units are TDB seconds. */
/*        C */
/*              STEP = 180.D0 */

/*        C */
/*        C     Perform the search. */
/*        C */
/*              CALL GFOCLT ( 'ANY', */
/*             .              'MOON',  'ellipsoid', 'IAU_MOON', */
/*             .              'SUN',   'ellipsoid', 'IAU_SUN', */
/*             .              'LT',    'EARTH',     STEP, */
/*             .              CNFINE,  RESULT                  ) */


/*              IF ( WNCARD(RESULT) .EQ. 0 ) THEN */

/*                 WRITE (*,*) 'No occultation was found.' */

/*              ELSE */

/*                 DO I = 1, WNCARD(RESULT) */

/*        C */
/*        C           Fetch and display each occultation interval. */
/*        C */
/*                    CALL WNFETD ( RESULT, I, LEFT, RIGHT ) */

/*                    CALL TIMOUT ( LEFT,  TIMFMT, BEGSTR ) */
/*                    CALL TIMOUT ( RIGHT, TIMFMT, ENDSTR ) */

/*                    WRITE (*,*) 'Interval ', I */
/*                    WRITE (*,*) '   Start time: '//BEGSTR */
/*                    WRITE (*,*) '   Stop time:  '//ENDSTR */

/*                 END DO */

/*              END IF */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Interval            1 */
/*            Start time: 2001 DEC 14 20:10:14.195952 (TDB) */
/*            Stop time:  2001 DEC 14 21:35:50.317994 (TDB) */


/*     2) Find occultations of Titan by Saturn or of Saturn by */
/*        Titan as seen from the center of the Earth over the */
/*        last four months of 2008. Model both target bodies as */
/*        ellipsoids. Search for every type of occultation. */

/*        Use light time corrections to model apparent positions of */
/*        Saturn and Titan. Stellar aberration corrections are not */
/*        specified because they don't affect occultation computations. */

/*        We select a step size of 15 minutes, which means we */
/*        ignore occultation events lasting less than 15 minutes, */
/*        if any exist. */

/*        Use the meta-kernel shown below to load the required SPICE */
/*        kernels. */


/*           KPL/MK */

/*           File name: gfoclt_ex2.tm */

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
/*              de421.bsp                     Planetary ephemeris */
/*              sat427.bsp                    Satellite ephemeris for */
/*                                            Saturn */
/*              pck00008.tpc                  Planet orientation and */
/*                                            radii */
/*              naif0009.tls                  Leapseconds */

/*           \begindata */

/*              KERNELS_TO_LOAD = ( 'de421.bsp', */
/*                                  'sat427.bsp', */
/*                                  'pck00008.tpc', */
/*                                  'naif0009.tls'  ) */

/*           \begintext */

/*           End of meta-kernel */


/*        Example code begins here. */


/*              PROGRAM GFOCLT_EX2 */
/*              IMPLICIT NONE */

/*        C */
/*        C     SPICELIB functions */
/*        C */
/*              INTEGER               WNCARD */

/*        C */
/*        C     Local parameters */
/*        C */
/*              CHARACTER*(*)         TIMFMT */
/*              PARAMETER           ( TIMFMT = */
/*             .   'YYYY MON DD HR:MN:SC.######::TDB' ) */

/*              INTEGER               MAXWIN */
/*              PARAMETER           ( MAXWIN = 2 * 100 ) */

/*              INTEGER               TIMLEN */
/*              PARAMETER           ( TIMLEN = 40 ) */

/*              INTEGER               BDNMLN */
/*              PARAMETER           ( BDNMLN = 36 ) */

/*              INTEGER               FRNMLN */
/*              PARAMETER           ( FRNMLN = 32 ) */

/*        C */
/*        C     Number of occultation types */
/*        C */
/*              INTEGER               NTYPES */
/*              PARAMETER           ( NTYPES = 4 ) */

/*        C */
/*        C     Occultation type name length */
/*        C */
/*              INTEGER               OCNMLN */
/*              PARAMETER           ( OCNMLN = 10 ) */

/*        C */
/*        C     Output line length */
/*        C */
/*              INTEGER               LNSIZE */
/*              PARAMETER           ( LNSIZE = 80 ) */

/*              INTEGER               LBCELL */
/*              PARAMETER           ( LBCELL = -5 ) */

/*        C */
/*        C     Local variables */
/*        C */
/*              CHARACTER*(BDNMLN)    BACK */
/*              CHARACTER*(FRNMLN)    BFRAME */
/*              CHARACTER*(FRNMLN)    FFRAME */
/*              CHARACTER*(BDNMLN)    FRONT */
/*              CHARACTER*(LNSIZE)    LINE */
/*              CHARACTER*(BDNMLN)    OBSRVR */
/*              CHARACTER*(OCNMLN)    OCCTYP ( NTYPES ) */
/*              CHARACTER*(LNSIZE)    TEMPLT ( NTYPES ) */
/*              CHARACTER*(TIMLEN)    TIMSTR */
/*              CHARACTER*(LNSIZE)    TITLE */
/*              CHARACTER*(TIMLEN)    WIN0 */
/*              CHARACTER*(TIMLEN)    WIN1 */

/*              DOUBLE PRECISION      CNFINE ( LBCELL : 2 ) */
/*              DOUBLE PRECISION      ET0 */
/*              DOUBLE PRECISION      ET1 */
/*              DOUBLE PRECISION      FINISH */
/*              DOUBLE PRECISION      RESULT ( LBCELL : MAXWIN ) */
/*              DOUBLE PRECISION      START */
/*              DOUBLE PRECISION      STEP */

/*              INTEGER               I */
/*              INTEGER               J */
/*              INTEGER               K */

/*        C */
/*        C     Saved variables */
/*        C */
/*        C     The confinement and result windows CNFINE */
/*        C     and RESULT are saved because this practice */
/*        C     helps to prevent stack overflow. */
/*        C */
/*        C     The variables OCCTYP and TEMPLT are */
/*        C     saved to facilitate turning this main program into */
/*        C     a subroutine. In a main program, it's not */
/*        C     necessary to save these variables. */
/*        C */
/*              SAVE                  CNFINE */
/*              SAVE                  OCCTYP */
/*              SAVE                  RESULT */
/*              SAVE                  TEMPLT */

/*        C */
/*        C     Initial values */
/*        C */
/*              DATA                  OCCTYP / 'FULL', */
/*             .                               'ANNULAR', */
/*             .                               'PARTIAL', */
/*             .                               'ANY'     / */

/*              DATA                  TEMPLT / */
/*             .      'Condition: # occultation of # by #', */
/*             .      'Condition: # occultation of # by #', */
/*             .      'Condition: # occultation of # by #', */
/*             .      'Condition: # occultation of # by #'      / */

/*        C */
/*        C     Load kernels. */
/*        C */
/*              CALL FURNSH ( 'gfoclt_ex2.tm' ) */

/*        C */
/*        C     Initialize the confinement and result windows. */
/*        C */
/*              CALL SSIZED ( 2,      CNFINE ) */
/*              CALL SSIZED ( MAXWIN, RESULT ) */

/*        C */
/*        C     Obtain the TDB time bounds of the confinement */
/*        C     window, which is a single interval in this case. */
/*        C */
/*              WIN0 = '2008 SEP 01 00:00:00 TDB' */
/*              WIN1 = '2009 JAN 01 00:00:00 TDB' */

/*              CALL STR2ET ( WIN0, ET0 ) */
/*              CALL STR2ET ( WIN1, ET1 ) */

/*        C */
/*        C     Insert the time bounds into the confinement */
/*        C     window. */
/*        C */
/*              CALL WNINSD ( ET0, ET1, CNFINE ) */

/*        C */
/*        C     Select a 15-minute step. We'll ignore any occultations */
/*        C     lasting less than 15 minutes. Units are TDB seconds. */
/*        C */
/*              STEP = 900.D0 */

/*        C */
/*        C     The observation location is the Earth. */
/*        C */
/*              OBSRVR = 'EARTH' */

/*        C */
/*        C     Loop over the occultation types. */
/*        C */
/*              DO I = 1, NTYPES */

/*        C */
/*        C        For each type, do a search for both transits of */
/*        C        Titan across Saturn and occultations of Titan by */
/*        C        Saturn. */
/*        C */
/*                 DO J = 1, 2 */

/*                    IF ( J .EQ. 1 ) THEN */

/*                       FRONT  = 'TITAN' */
/*                       FFRAME = 'IAU_TITAN' */
/*                       BACK   = 'SATURN' */
/*                       BFRAME = 'IAU_SATURN' */

/*                    ELSE */

/*                       FRONT  = 'SATURN' */
/*                       FFRAME = 'IAU_SATURN' */
/*                       BACK   = 'TITAN' */
/*                       BFRAME = 'IAU_TITAN' */

/*                    END IF */

/*        C */
/*        C           Perform the search. The target body shapes */
/*        C           are modeled as ellipsoids. */
/*        C */
/*                    CALL GFOCLT ( OCCTYP(I), */
/*             .                    FRONT,  'ELLIPSOID', FFRAME, */
/*             .                    BACK,   'ELLIPSOID', BFRAME, */
/*             .                    'LT',   OBSRVR,      STEP, */
/*             .                    CNFINE, RESULT              ) */

/*        C */
/*        C           Display the results. */
/*        C */
/*                    WRITE (*,*) ' ' */

/*        C */
/*        C           Substitute the occultation type and target */
/*        C           body names into the title string: */
/*        C */
/*                    CALL REPMC ( TEMPLT(I), '#', OCCTYP(I), TITLE ) */
/*                    CALL REPMC ( TITLE,     '#', BACK,      TITLE ) */
/*                    CALL REPMC ( TITLE,     '#', FRONT,     TITLE ) */

/*                    WRITE (*, '(A)' ) TITLE */

/*                    IF ( WNCARD(RESULT) .EQ. 0 ) THEN */

/*                       WRITE (*, '(A)' ) ' Result window is empty: ' */
/*             .         //                'no occultation was found.' */

/*                    ELSE */

/*                       WRITE (*, '(A)' ) ' Result window start, ' */
/*             .         //                'stop times (TDB):' */

/*                       DO K = 1, WNCARD(RESULT) */

/*        C */
/*        C                 Fetch the endpoints of the Kth interval */
/*        C                 of the result window. */
/*        C */
/*                          CALL WNFETD ( RESULT, K, START, FINISH ) */

/*                          LINE = '  #    #' */

/*                          CALL TIMOUT ( START, TIMFMT, TIMSTR ) */

/*                          CALL REPMC  ( LINE, '#', TIMSTR, LINE ) */

/*                          CALL TIMOUT ( FINISH, TIMFMT, TIMSTR ) */

/*                          CALL REPMC  ( LINE, '#', TIMSTR, LINE ) */

/*                          WRITE ( *, '(A)' ) LINE */

/*                       END DO */

/*                    END IF */

/*        C */
/*        C           We've finished displaying the results of the */
/*        C           current search. */
/*        C */
/*                 END DO */

/*        C */
/*        C        We've finished displaying the results of the */
/*        C        searches using the current occultation type. */
/*        C */
/*              END DO */

/*              WRITE (*,*) ' ' */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        Condition: FULL occultation of SATURN by TITAN */
/*         Result window is empty: no occultation was found. */

/*        Condition: FULL occultation of TITAN by SATURN */
/*         Result window start, stop times (TDB): */
/*          2008 OCT 27 22:08:01.672540    2008 OCT 28 01:05:03.332576 */
/*          2008 NOV 12 21:21:59.270691    2008 NOV 13 02:06:05.034713 */
/*          2008 NOV 28 20:49:02.415745    2008 NOV 29 02:13:58.978004 */
/*          2008 DEC 14 20:05:09.258916    2008 DEC 15 01:44:53.517960 */
/*          2008 DEC 30 19:00:56.586894    2008 DEC 31 00:42:43.219311 */

/*        Condition: ANNULAR occultation of SATURN by TITAN */
/*         Result window start, stop times (TDB): */
/*          2008 OCT 19 21:29:20.694709    2008 OCT 19 22:53:34.442728 */
/*          2008 NOV 04 20:15:38.652650    2008 NOV 05 00:18:59.130645 */
/*          2008 NOV 20 19:38:59.674043    2008 NOV 21 00:35:26.726756 */
/*          2008 DEC 06 18:58:34.093679    2008 DEC 07 00:16:17.653066 */
/*          2008 DEC 22 18:02:46.308375    2008 DEC 22 23:26:52.721881 */

/*        Condition: ANNULAR occultation of TITAN by SATURN */
/*         Result window is empty: no occultation was found. */

/*        Condition: PARTIAL occultation of SATURN by TITAN */
/*         Result window start, stop times (TDB): */
/*          2008 OCT 19 20:44:30.377189    2008 OCT 19 21:29:20.694709 */
/*          2008 OCT 19 22:53:34.442728    2008 OCT 19 23:38:26.219865 */
/*          2008 NOV 04 19:54:40.368045    2008 NOV 04 20:15:38.652650 */
/*          2008 NOV 05 00:18:59.130645    2008 NOV 05 00:39:58.607159 */
/*          2008 NOV 20 19:21:46.714396    2008 NOV 20 19:38:59.674043 */
/*          2008 NOV 21 00:35:26.726756    2008 NOV 21 00:52:40.606954 */
/*          2008 DEC 06 18:42:36.120122    2008 DEC 06 18:58:34.093679 */
/*          2008 DEC 07 00:16:17.653066    2008 DEC 07 00:32:16.331199 */
/*          2008 DEC 22 17:47:10.796147    2008 DEC 22 18:02:46.308375 */
/*          2008 DEC 22 23:26:52.721881    2008 DEC 22 23:42:28.860689 */

/*        Condition: PARTIAL occultation of TITAN by SATURN */
/*         Result window start, stop times (TDB): */
/*          2008 OCT 27 21:37:17.003993    2008 OCT 27 22:08:01.672540 */
/*          2008 OCT 28 01:05:03.332576    2008 OCT 28 01:35:49.235670 */
/*          2008 NOV 12 21:01:47.121213    2008 NOV 12 21:21:59.270691 */
/*          2008 NOV 13 02:06:05.034713    2008 NOV 13 02:26:18.211753 */
/*          2008 NOV 28 20:31:28.534248    2008 NOV 28 20:49:02.415745 */
/*          2008 NOV 29 02:13:58.978004    2008 NOV 29 02:31:33.684575 */
/*          2008 DEC 14 19:48:27.106157    2008 DEC 14 20:05:09.258916 */
/*          2008 DEC 15 01:44:53.517960    2008 DEC 15 02:01:36.356012 */
/*          2008 DEC 30 18:44:23.495003    2008 DEC 30 19:00:56.586894 */
/*          2008 DEC 31 00:42:43.219311    2008 DEC 31 00:59:17.027816 */

/*        Condition: ANY occultation of SATURN by TITAN */
/*         Result window start, stop times (TDB): */
/*          2008 OCT 19 20:44:30.377189    2008 OCT 19 23:38:26.219865 */
/*          2008 NOV 04 19:54:40.368045    2008 NOV 05 00:39:58.607159 */
/*          2008 NOV 20 19:21:46.714396    2008 NOV 21 00:52:40.606954 */
/*          2008 DEC 06 18:42:36.120122    2008 DEC 07 00:32:16.331199 */
/*          2008 DEC 22 17:47:10.796147    2008 DEC 22 23:42:28.860689 */

/*        Condition: ANY occultation of TITAN by SATURN */
/*         Result window start, stop times (TDB): */
/*          2008 OCT 27 21:37:17.003993    2008 OCT 28 01:35:49.235670 */
/*          2008 NOV 12 21:01:47.121213    2008 NOV 13 02:26:18.211753 */
/*          2008 NOV 28 20:31:28.534248    2008 NOV 29 02:31:33.684575 */
/*          2008 DEC 14 19:48:27.106157    2008 DEC 15 02:01:36.356012 */
/*          2008 DEC 30 18:44:23.495003    2008 DEC 31 00:59:17.027816 */


/*     3) Find occultations of the Mars Reconnaissance Orbiter (MRO) */
/*        by Mars or transits of the MRO spacecraft across Mars */
/*        as seen from the DSN station DSS-14 over a period of a */
/*        few hours on FEB 28 2015. */

/*        Use both ellipsoid and DSK shape models for Mars. */

/*        Use light time corrections to model apparent positions of */
/*        Mars and MRO. Stellar aberration corrections are not */
/*        specified because they don't affect occultation computations. */

/*        We select a step size of 3 minutes, which means we */
/*        ignore occultation events lasting less than 3 minutes, */
/*        if any exist. */

/*        Use the meta-kernel shown below to load the required SPICE */
/*        kernels. */


/*           KPL/MK */

/*           File: gfoclt_ex3.tm */

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
/*              de410.bsp                        Planetary ephemeris */
/*              mar063.bsp                       Mars satellite ephemeris */
/*              pck00010.tpc                     Planet orientation and */
/*                                               radii */
/*              naif0011.tls                     Leapseconds */
/*              earthstns_itrf93_050714.bsp      DSN station ephemeris */
/*              earth_latest_high_prec.bpc       Earth orientation */
/*              mro_psp34.bsp                    MRO ephemeris */
/*              megr90n000cb_plate.bds           Plate model based on */
/*                                               MEGDR DEM, resolution */
/*                                               4 pixels/degree. */

/*           \begindata */

/*              KERNELS_TO_LOAD = ( 'de410.bsp', */
/*                                  'mar063.bsp', */
/*                                  'mro_psp34.bsp', */
/*                                  'earthstns_itrf93_050714.bsp', */
/*                                  'earth_latest_high_prec.bpc', */
/*                                  'pck00010.tpc', */
/*                                  'naif0011.tls', */
/*                                  'megr90n000cb_plate.bds' */
/*                                ) */
/*           \begintext */

/*           End of meta-kernel */


/*        Example code begins here. */


/*              PROGRAM GFOCLT_EX3 */
/*              IMPLICIT NONE */

/*        C */
/*        C     SPICELIB functions */
/*        C */
/*              INTEGER               WNCARD */

/*        C */
/*        C     Local parameters */
/*        C */
/*              CHARACTER*(*)         META */
/*              PARAMETER           ( META   = 'gfoclt_ex3.tm' ) */

/*              CHARACTER*(*)         TIMFMT */
/*              PARAMETER           ( TIMFMT = */
/*             .   'YYYY MON DD HR:MN:SC.###### (TDB)::TDB' ) */

/*              INTEGER               MAXWIN */
/*              PARAMETER           ( MAXWIN = 2 * 100 ) */

/*              INTEGER               CORLEN */
/*              PARAMETER           ( CORLEN = 10 ) */

/*              INTEGER               TIMLEN */
/*              PARAMETER           ( TIMLEN = 40 ) */

/*              INTEGER               BDNMLN */
/*              PARAMETER           ( BDNMLN = 36 ) */

/*              INTEGER               FRNMLN */
/*              PARAMETER           ( FRNMLN = 32 ) */

/*              INTEGER               SHPLEN */
/*              PARAMETER           ( SHPLEN = 100 ) */

/*              INTEGER               OTYPLN */
/*              PARAMETER           ( OTYPLN = 20 ) */

/*              INTEGER               LBCELL */
/*              PARAMETER           ( LBCELL = -5 ) */

/*        C */
/*        C     Local variables */
/*        C */
/*              CHARACTER*(CORLEN)    ABCORR */
/*              CHARACTER*(BDNMLN)    BACK */
/*              CHARACTER*(FRNMLN)    BFRAME */
/*              CHARACTER*(SHPLEN)    BSHAPE */
/*              CHARACTER*(BDNMLN)    FRONT */
/*              CHARACTER*(SHPLEN)    FSHAPE */
/*              CHARACTER*(FRNMLN)    FFRAME */
/*              CHARACTER*(OTYPLN)    OCCTYP */
/*              CHARACTER*(BDNMLN)    OBSRVR */
/*              CHARACTER*(TIMLEN)    WIN0 */
/*              CHARACTER*(TIMLEN)    WIN1 */
/*              CHARACTER*(TIMLEN)    BEGSTR */
/*              CHARACTER*(TIMLEN)    ENDSTR */

/*              DOUBLE PRECISION      CNFINE ( LBCELL : 2 ) */
/*              DOUBLE PRECISION      ET0 */
/*              DOUBLE PRECISION      ET1 */
/*              DOUBLE PRECISION      LEFT */
/*              DOUBLE PRECISION      RESULT ( LBCELL : MAXWIN ) */
/*              DOUBLE PRECISION      RIGHT */
/*              DOUBLE PRECISION      STEP */

/*              INTEGER               I */
/*              INTEGER               J */
/*              INTEGER               K */

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
/*        C     Initialize the confinement and result windows. */
/*        C */
/*              CALL SSIZED ( MAXWIN, CNFINE ) */
/*              CALL SSIZED ( MAXWIN, RESULT ) */

/*        C */
/*        C     Set the observer and aberration correction. */
/*        C */
/*              OBSRVR = 'DSS-14' */
/*              ABCORR = 'CN' */

/*        C */
/*        C     Set the occultation type. */
/*        C */
/*              OCCTYP = 'ANY' */

/*        C */
/*        C     Set the TDB time bounds of the confinement */
/*        C     window, which is a single interval in this case. */
/*        C */
/*              WIN0 = '2015 FEB 28 07:00:00 TDB' */
/*              WIN1 = '2015 FEB 28 12:00:00 TDB' */

/*              CALL STR2ET ( WIN0, ET0 ) */
/*              CALL STR2ET ( WIN1, ET1 ) */

/*        C */
/*        C     Insert the time bounds into the confinement */
/*        C     window. */
/*        C */
/*              CALL WNINSD ( ET0, ET1, CNFINE ) */

/*        C */
/*        C     Select a 3-minute step. We'll ignore any occultations */
/*        C     lasting less than 3 minutes. Units are TDB seconds. */
/*        C */
/*              STEP = 180.D0 */

/*        C */
/*        C     Perform both spacecraft occultation and spacecraft */
/*        C     transit searches. */
/*        C */
/*              WRITE (*,*) ' ' */

/*              DO I = 1, 2 */

/*                 IF ( I .EQ. 1 ) THEN */

/*        C */
/*        C           Perform a spacecraft occultation search. */
/*        C */
/*                    FRONT  = 'MARS' */
/*                    FFRAME = 'IAU_MARS' */

/*                    BACK   = 'MRO' */
/*                    BSHAPE = 'POINT' */
/*                    BFRAME = ' ' */

/*                 ELSE */

/*        C */
/*        C           Perform a spacecraft transit search. */
/*        C */
/*                    FRONT  = 'MRO' */
/*                    FSHAPE = 'POINT' */
/*                    FFRAME = ' ' */

/*                    BACK   = 'MARS' */
/*                    BFRAME = 'IAU_MARS' */

/*                 END IF */


/*                 DO J = 1, 2 */

/*                    IF ( J .EQ. 1 ) THEN */

/*        C */
/*        C              Model the planet shape as an ellipsoid. */
/*        C */
/*                       IF ( I .EQ. 1 ) THEN */
/*                          FSHAPE = 'ELLIPSOID' */
/*                       ELSE */
/*                          BSHAPE = 'ELLIPSOID' */
/*                       END IF */

/*                    ELSE */

/*        C */
/*        C              Model the planet shape using DSK data. */
/*        C */
/*                       IF ( I .EQ. 1 ) THEN */
/*                          FSHAPE = 'DSK/UNPRIORITIZED' */
/*                       ELSE */
/*                          BSHAPE = 'DSK/UNPRIORITIZED' */
/*                       END IF */

/*                    END IF */

/*        C */
/*        C           Perform the spacecraft occultation or */
/*        C           transit search. */

/*                    IF ( I .EQ. 1 ) THEN */
/*                       CALL TOSTDO ( 'Using shape model '//FSHAPE     ) */
/*                       CALL TOSTDO ( 'Starting occultation search...' ) */
/*                    ELSE */
/*                       CALL TOSTDO ( 'Using shape model '//BSHAPE ) */
/*                       CALL TOSTDO ( 'Starting transit search...' ) */
/*                    END IF */

/*                    CALL GFOCLT ( OCCTYP, */
/*             .                    FRONT,  FSHAPE, FFRAME, */
/*             .                    BACK,   BSHAPE, BFRAME, */
/*             .                    ABCORR, OBSRVR, STEP, */
/*             .                    CNFINE, RESULT         ) */

/*                    IF ( WNCARD(RESULT) .EQ. 0 ) THEN */

/*                       WRITE (*,*) 'No event was found.' */

/*                    ELSE */

/*                       DO K = 1, WNCARD(RESULT) */

/*        C */
/*        C                 Fetch and display each event interval. */
/*        C */
/*                          CALL WNFETD ( RESULT, K, LEFT, RIGHT ) */

/*                          CALL TIMOUT ( LEFT,  TIMFMT, BEGSTR ) */
/*                          CALL TIMOUT ( RIGHT, TIMFMT, ENDSTR ) */

/*                          WRITE (*,*) '   Interval ', K */
/*                          WRITE (*,*) '      Start time: '//BEGSTR */
/*                          WRITE (*,*) '      Stop time:  '//ENDSTR */

/*                       END DO */

/*                    END IF */

/*                    WRITE (*,*) ' ' */

/*                 END DO */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        Using shape model ELLIPSOID */
/*        Starting occultation search... */
/*            Interval            1 */
/*               Start time: 2015 FEB 28 07:17:35.379879 (TDB) */
/*               Stop time:  2015 FEB 28 07:50:37.710284 (TDB) */
/*            Interval            2 */
/*               Start time: 2015 FEB 28 09:09:46.920140 (TDB) */
/*               Stop time:  2015 FEB 28 09:42:50.497193 (TDB) */
/*            Interval            3 */
/*               Start time: 2015 FEB 28 11:01:57.845730 (TDB) */
/*               Stop time:  2015 FEB 28 11:35:01.489716 (TDB) */

/*        Using shape model DSK/UNPRIORITIZED */
/*        Starting occultation search... */
/*            Interval            1 */
/*               Start time: 2015 FEB 28 07:17:38.130608 (TDB) */
/*               Stop time:  2015 FEB 28 07:50:38.310802 (TDB) */
/*            Interval            2 */
/*               Start time: 2015 FEB 28 09:09:50.314903 (TDB) */
/*               Stop time:  2015 FEB 28 09:42:55.369626 (TDB) */
/*            Interval            3 */
/*               Start time: 2015 FEB 28 11:02:01.756296 (TDB) */
/*               Stop time:  2015 FEB 28 11:35:08.368384 (TDB) */

/*        Using shape model ELLIPSOID */
/*        Starting transit search... */
/*            Interval            1 */
/*               Start time: 2015 FEB 28 08:12:21.112018 (TDB) */
/*               Stop time:  2015 FEB 28 08:45:48.401746 (TDB) */
/*            Interval            2 */
/*               Start time: 2015 FEB 28 10:04:32.682324 (TDB) */
/*               Stop time:  2015 FEB 28 10:37:59.920302 (TDB) */
/*            Interval            3 */
/*               Start time: 2015 FEB 28 11:56:39.757564 (TDB) */
/*               Stop time:  2015 FEB 28 12:00:00.000000 (TDB) */

/*        Using shape model DSK/UNPRIORITIZED */
/*        Starting transit search... */
/*            Interval            1 */
/*               Start time: 2015 FEB 28 08:12:15.750020 (TDB) */
/*               Stop time:  2015 FEB 28 08:45:43.406870 (TDB) */
/*            Interval            2 */
/*               Start time: 2015 FEB 28 10:04:29.031706 (TDB) */
/*               Stop time:  2015 FEB 28 10:37:55.565509 (TDB) */
/*            Interval            3 */
/*               Start time: 2015 FEB 28 11:56:34.634642 (TDB) */
/*               Stop time:  2015 FEB 28 12:00:00.000000 (TDB) */


/* $ Restrictions */

/*     1)  The kernel files to be used by GFOCLT must be loaded (normally */
/*         via the SPICELIB routine FURNSH) before GFOCLT is called. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     L.S. Elson         (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.1, 25-NOV-2021 (JDR) (NJB) */

/*        Edited the header to comply with NAIF standard. */

/*        Modified code example #2 output to comply with maximum line */
/*        length of header comments. Added SAVE statements for CNFINE and */
/*        RESULT variables in code examples. */

/*        The $Exceptions section now lists the case of the combination */
/*        of DSK and non-point target shapes. */

/*        Updated description of RESULT argument in $Brief_I/O, */
/*        $Detailed_Input and $Detailed_Output. */

/* -    SPICELIB Version 2.0.0, 29-FEB-2016 (NJB) */

/*        Header was updated. An example program demonstrating */
/*        DSK usage was added. */

/*        04-MAR-2015 (NJB) */

/*        Upgraded to support surfaces represented by DSKs. */

/* -    SPICELIB Version 1.1.0, 31-AUG-2010 (EDW) */

/*        Implemented use of ZZHOLDD to allow user to alter convergence */
/*        tolerance. */

/*        Removed the STEP > 0 error check. The GFSSTP call includes */
/*        the check. */

/* -    SPICELIB Version 1.0.0, 07-APR-2009 (NJB) (LSE) (EDW) */

/* -& */
/* $ Index_Entries */

/*     GF occultation search */

/* -& */

/*     SPICELIB functions */


/*     Local variables. */


/*     External routines */


/*     Interrupt handler: */


/*     Routines to set step size, refine transition times */
/*     and report work: */


/*     Local parameters */


/*     Geometric quantity  bail switch: */


/*     Progress report switch: */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("GFOCLT", (ftnlen)6);

/*     Note to maintenance programmer: input exception checks */
/*     are delegated to GFOCCE. If the implementation of that */
/*     routine changes, or if this routine is modified to call */
/*     a different routine in place of GFOCCE, then the error */
/*     handling performed by GFOCCE will have to be performed */
/*     here or in a routine called by this routine. */

/*     Check the result window's size. */

    if (sized_(result) < 2) {
	setmsg_("Result window size must be at least 2 but was #.", (ftnlen)
		48);
	i__1 = sized_(result);
	errint_("#", &i__1, (ftnlen)1);
	sigerr_("SPICE(WINDOWTOOSMALL)", (ftnlen)21);
	chkout_("GFOCLT", (ftnlen)6);
	return 0;
    }

/*     Check and set the step size. */

    gfsstp_(step);

/*     Retrieve the convergence tolerance, if set. */

    zzholdd_(&c_n1, &c__3, &ok, &tol);

/*     Use the default value CNVTOL if no stored tolerance value. */

    if (! ok) {
	tol = 1e-6;
    }

/*     Look for solutions. */

    gfocce_(occtyp, front, fshape, fframe, back, bshape, bframe, abcorr, 
	    obsrvr, &tol, (U_fp)gfstep_, (U_fp)gfrefn_, &c_false, (U_fp)
	    gfrepi_, (U_fp)gfrepu_, (U_fp)gfrepf_, &c_false, (L_fp)gfbail_, 
	    cnfine, result, occtyp_len, front_len, fshape_len, fframe_len, 
	    back_len, bshape_len, bframe_len, abcorr_len, obsrvr_len);
    chkout_("GFOCLT", (ftnlen)6);
    return 0;
} /* gfoclt_ */

