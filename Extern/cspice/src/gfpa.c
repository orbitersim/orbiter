/* gfpa.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__5 = 5;
static integer c_n1 = -1;
static integer c__3 = 3;
static integer c__0 = 0;
static integer c__4 = 4;
static logical c_false = FALSE_;

/* $Procedure GFPA ( GF, phase angle search ) */
/* Subroutine */ int gfpa_(char *target, char *illmn, char *abcorr, char *
	obsrvr, char *relate, doublereal *refval, doublereal *adjust, 
	doublereal *step, doublereal *cnfine, integer *mw, integer *nw, 
	doublereal *work, doublereal *result, ftnlen target_len, ftnlen 
	illmn_len, ftnlen abcorr_len, ftnlen obsrvr_len, ftnlen relate_len)
{
    /* System generated locals */
    integer work_dim1, work_offset, i__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern integer sized_(doublereal *);
    extern logical gfbail_();
    logical ok;
    extern /* Subroutine */ int scardd_(integer *, doublereal *);
    extern /* Subroutine */ int gfrefn_(), gfrepi_(), gfrepf_(), gfrepu_(), 
	    gfstep_();
    char qcpars[80*4], qpnams[80*4];
    extern logical return_(void);
    doublereal qdpars[4];
    integer qipars[4];
    logical qlpars[4];
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen), sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), gfsstp_(doublereal *), gfevnt_(U_fp, U_fp, char *, 
	    integer *, char *, char *, doublereal *, integer *, logical *, 
	    char *, doublereal *, doublereal *, doublereal *, doublereal *, 
	    logical *, U_fp, U_fp, U_fp, integer *, integer *, doublereal *, 
	    logical *, L_fp, doublereal *, ftnlen, ftnlen, ftnlen, ftnlen);
    extern logical odd_(integer *);
    doublereal tol;
    extern /* Subroutine */ int zzholdd_(integer *, integer *, logical *, 
	    doublereal *);

/* $ Abstract */

/*     Determine time intervals for which a specified constraint */
/*     on the phase angle between an illumination source, a target, */
/*     and observer body centers is met. */

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
/*     NAIF_IDS */
/*     SPK */
/*     TIME */
/*     WINDOWS */

/* $ Keywords */

/*     EPHEMERIS */
/*     EVENT */
/*     GEOMETRY */
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
/*     CNVTOL     P   Default convergence tolerance. */
/*     TARGET     I   Name of the target body. */
/*     ILLMN      I   Name of the illuminating body. */
/*     ABCORR     I   Aberration correction flag. */
/*     OBSRVR     I   Name of the observing body. */
/*     RELATE     I   Relational operator. */
/*     REFVAL     I   Reference value. */
/*     ADJUST     I   Adjustment value for absolute extrema searches. */
/*     STEP       I   Step size used for locating extrema and roots. */
/*     CNFINE     I   SPICE window to which the search is confined. */
/*     MW         I   Workspace window size. */
/*     NW         I   The number of workspace windows needed for */
/*                    the search. */
/*     WORK       O   Array of workspace windows. */
/*     RESULT    I-O  SPICE window containing results. */

/* $ Detailed_Input */

/*     TARGET   is the name of a target body. Optionally, you may supply */
/*              a string containing the integer ID code for the object. */
/*              For example both 'MOON' and '301' are legitimate strings */
/*              that indicate the Moon is the target body. */

/*              Case and leading or trailing blanks are not significant */
/*              in the string TARGET. */

/*     ILLMN    is the name of the illuminating body. This will normally */
/*              be 'SUN' but the algorithm can use any ephemeris object. */

/*              Case and leading or trailing blanks are not significant */
/*              in the string ILLMN. */

/*     ABCORR   is the description of the aberration corrections to apply */
/*              to the state evaluations to account for one-way light */
/*              time and stellar aberration. */

/*              This routine accepts only reception mode aberration */
/*              corrections. See the header of SPKEZR for a detailed */
/*              description of the aberration correction options. For */
/*              convenience, the allowed aberration options are listed */
/*              below: */

/*                 'NONE'     Apply no correction. Returns the "true" */
/*                            geometric state. */

/*                 'LT'       "Reception" case: correct for */
/*                            one-way light time using a Newtonian */
/*                            formulation. */

/*                 'LT+S'     "Reception" case: correct for */
/*                            one-way light time and stellar */
/*                            aberration using a Newtonian */
/*                            formulation. */

/*                 'CN'       "Reception" case: converged */
/*                            Newtonian light time correction. */

/*                'CN+S'      "Reception" case: converged */
/*                            Newtonian light time and stellar */
/*                            aberration corrections. */

/*              Case and leading or trailing blanks are not significant */
/*              in the string ABCORR. */

/*     OBSRVR   is the name of an observing body. Optionally, you may */
/*              supply a string containing the integer ID code for the */
/*              object. For example both "MOON" and "301" are legitimate */
/*              strings that indicate the Moon is the observer. */

/*              Case and leading or trailing blanks are not significant */
/*              in the string OBSRVR. */

/*     RELATE   is a relational operator used to define a constraint on a */
/*              specified phase angle. The result window found by this */
/*              routine indicates the time intervals where the constraint */
/*              is satisfied. Supported values of RELATE and */
/*              corresponding meanings are shown below: */

/*                 '>'        The phase angle value is greater than the */
/*                            reference value REFVAL. */

/*                 '='        The phase angle value is equal to the */
/*                            reference value REFVAL. */

/*                 '<'        The phase angle value is less than the */
/*                            reference value REFVAL. */

/*                 'ABSMAX'   The phase angle value is at an absolute */
/*                            maximum. */

/*                 'ABSMIN'   The phase angle value is at an absolute */
/*                            minimum. */

/*                 'LOCMAX'   The phase angle value is at a local */
/*                            maximum. */

/*                 'LOCMIN'   The phase angle value is at a local */
/*                            minimum. */

/*              RELATE may be used to specify an "adjusted" absolute */
/*              extremum constraint: this requires the phase angle to be */
/*              within a specified offset relative to an absolute */
/*              extremum. The argument ADJUST (described below) is used */
/*              to specify this offset. */

/*              Local extrema are considered to exist only in the */
/*              interiors of the intervals comprising the confinement */
/*              window:  a local extremum cannot exist at a boundary */
/*              point of the confinement window. */

/*              Case and leading or trailing blanks are not */
/*              significant in the string RELATE. */

/*     REFVAL   is the double precision reference value used together */
/*              with the argument RELATE to define an equality or */
/*              inequality to be satisfied by the phase angle. See the */
/*              discussion of RELATE above for further information. */

/*              The units of REFVAL are radians. */

/*     ADJUST   is a double precision value used to modify searches for */
/*              absolute extrema: when RELATE is set to 'ABSMAX' or */
/*              'ABSMIN' and ADJUST is set to a positive value, GFPA */
/*              finds times when the phase angle is within ADJUST radians */
/*              of the specified extreme value. */

/*              For RELATE set to 'ABSMAX', the RESULT window contains */
/*              time intervals when the phase angle has */
/*              values between ABSMAX - ADJUST and ABSMAX. */

/*              For RELATE set to 'ABSMIN', the RESULT window contains */
/*              time intervals when the phase angle has */
/*              values between ABSMIN and ABSMIN + ADJUST. */

/*              ADJUST is not used for searches for local extrema, */
/*              equality or inequality conditions. */

/*     STEP     is the double precision time step size to use in the */
/*              search. */

/*              STEP must be short enough for a search using this step */
/*              size to locate the time intervals where the phase angle */
/*              function is monotone increasing or decreasing. However, */
/*              STEP must not be *too* short, or the search will take an */
/*              unreasonable amount of time. */

/*              The choice of STEP affects the completeness but not */
/*              the precision of solutions found by this routine; the */
/*              precision is controlled by the convergence tolerance. */
/*              See the discussion of the parameter CNVTOL for */
/*              details. */

/*              STEP has units of TDB seconds. */

/*     CNFINE   is a double precision SPICE window that confines the time */
/*              period over which the specified search is conducted. */
/*              CNFINE may consist of a single interval or a collection */
/*              of intervals. */

/*              In some cases the confinement window can be used to */
/*              greatly reduce the time period that must be searched */
/*              for the desired solution. See the $Particulars section */
/*              below for further discussion. */

/*              The endpoints of the time intervals comprising CNFINE are */
/*              interpreted as seconds past J2000 TDB. */

/*              See the $Examples section below for a code example */
/*              that shows how to create a confinement window. */

/*              CNFINE must be initialized by the caller using the */
/*              SPICELIB routine SSIZED. */

/*              In some cases the observer's state may be computed at */
/*              times outside of CNFINE by as much as 2 seconds. See */
/*              $Particulars for details. */

/*     MW       is a parameter specifying the length of the SPICE */
/*              windows in the workspace array WORK (see description */
/*              below) used by this routine. */

/*              MW should be set to a number at least twice as large */
/*              as the maximum number of intervals required by any */
/*              workspace window. In many cases, it's not necessary to */
/*              compute an accurate estimate of how many intervals are */
/*              needed; rather, the user can pick a size considerably */
/*              larger than what's really required. */

/*              However, since excessively large arrays can prevent */
/*              applications from compiling, linking, or running */
/*              properly, sometimes MW must be set according to */
/*              the actual workspace requirement. A rule of thumb */
/*              for the number of intervals NINTVLS needed is */

/*                  NINTVLS  =  2*N  +  ( M / STEP ) */

/*              where */

/*                  N     is the number of intervals in the confinement */
/*                        window */

/*                  M     is the measure of the confinement window, in */
/*                        units of seconds */

/*                  STEP  is the search step size in seconds */

/*              MW should then be set to */

/*                  2 * NINTVLS */

/*     NW       is a parameter specifying the number of SPICE windows */
/*              in the workspace array WORK (see description below) */
/*              used by this routine. NW should be set to the */
/*              parameter NWPA; this parameter is declared in the */
/*              include file gf.inc. (The reason this dimension is */
/*              an input argument is that this allows run-time */
/*              error checking to be performed.) */

/*     RESULT   is a double precision SPICE window which will contain */
/*              the search results. RESULT must be declared and */
/*              initialized with sufficient size to capture the full */
/*              set of time intervals within the search region on which */
/*              the specified condition is satisfied. */

/*              RESULT must be initialized by the caller via the */
/*              SPICELIB routine SSIZED. */

/*              If RESULT is non-empty on input, its contents will be */
/*              discarded before GFPA conducts its search. */

/* $ Detailed_Output */

/*     WORK     is an array used to store workspace windows. */

/*              This array should be declared by the caller as shown: */

/*                 INCLUDE 'gf.inc' */
/*                    ... */

/*                 DOUBLE PRECISION    WORK ( LBCELL : MW, NWPA ) */

/*              where MW is a constant declared by the caller and */
/*              NWPA is a constant defined in the SPICELIB INCLUDE */
/*              file gf.inc. See the discussion of MW above. */

/*              WORK need not be initialized by the caller. */

/*              WORK is modified by this routine. The caller should */
/*              re-initialize this array before attempting to use it for */
/*              any other purpose. */

/*     RESULT   is the SPICE window of intervals, contained within the */
/*              confinement window CNFINE, on which the specified phase */
/*              angle constraint is satisfied. */

/*              The endpoints of the time intervals comprising RESULT are */
/*              interpreted as seconds past J2000 TDB. */

/*              If the search is for local extrema, or for absolute */
/*              extrema with ADJUST set to zero, then normally each */
/*              interval of RESULT will be a singleton: the left and */
/*              right endpoints of each interval will be identical. */

/*              If no times within the confinement window satisfy the */
/*              search criteria, RESULT will be returned with a */
/*              cardinality of zero. */

/* $ Parameters */

/*     LBCELL   is the integer value defining the lower bound for */
/*              SPICE Cell arrays (a SPICE window is a kind of cell). */

/*     CNVTOL   is the default convergence tolerance used for finding */
/*              endpoints of the intervals comprising the result */
/*              window. CNVTOL is also used for finding intermediate */
/*              results; in particular, CNVTOL is used for finding the */
/*              windows on which the phase angle is increasing */
/*              or decreasing. CNVTOL is used to determine when binary */
/*              searches for roots should terminate: when a root is */
/*              bracketed within an interval of length CNVTOL; the */
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

/*         This routine does not diagnose invalid step sizes, except that */
/*         if the step size is non-positive, an error is signaled by a */
/*         routine in the call tree of this routine. */

/*     2)  Due to numerical errors, in particular, */

/*            - truncation error in time values */
/*            - finite tolerance value */
/*            - errors in computed geometric quantities */

/*         it is *normal* for the condition of interest to not always be */
/*         satisfied near the endpoints of the intervals comprising the */
/*         RESULT window. One technique to handle such a situation, */
/*         slightly contract RESULT using the window routine WNCOND. */

/*     3)  If workspace window size, MW, is not at least 2 and an even */
/*         value, the error SPICE(INVALIDDIMENSION) is signaled is */
/*         signaled. */

/*     4)  If workspace window count, NW, is not at least NWPA, the error */
/*         SPICE(INVALIDDIMENSION) is signaled is signaled. */

/*     5)  If result window, RESULT, is not at least 2 and an even value, */
/*         the error SPICE(INVALIDDIMENSION) is signaled is signaled. */

/*     6)  If RESULT has insufficient capacity to contain the */
/*         number of intervals on which the specified angle condition */
/*         is met, an error is signaled by a routine in the call */
/*         tree of this routine. */

/*     7)  If an error (typically cell overflow) occurs during */
/*         window arithmetic, the error is signaled by a routine */
/*         in the call tree of this routine. */

/*     8)  If the relational operator RELATE is not recognized, an */
/*         error is signaled by a routine in the call tree of this */
/*         routine. */

/*     9)  If ADJUST is negative, an error is signaled by a routine in */
/*         the call tree of this routine. */

/*     10) If ADJUST has a non-zero value when RELATE has any value other */
/*         than 'ABSMIN' or 'ABSMAX', an error is signaled by a routine */
/*         in the call tree of this routine. */

/*     11) If any of the input body names, TARGET, ILLMN, OBSRVR, do */
/*         not map to NAIF ID codes, an error is signaled by a routine */
/*         in the call tree of this routine. */

/*     12) If the input body names, TARGET, ILLMN, OBSRVR, are not */
/*         distinct, an error is signaled by a routine in the call */
/*         tree of this routine. */

/*     13) If required ephemerides or other kernel data are not */
/*         available, an error is signaled by a routine in the call tree */
/*         of this routine. */

/*     14) If the aberration correction specifier contains an */
/*         unrecognized value, an error is signaled by a routine in the */
/*         call tree of this routine. */

/*     15) If a transmit mode aberration correction is requested, an */
/*         error is signaled by a routine in the call tree of this */
/*         routine. */

/* $ Files */

/*     Appropriate SPK and PCK kernels must be loaded by the calling */
/*     program before this routine is called. */

/*     The following data are required: */

/*     -  SPK data: the calling application must load ephemeris data */
/*        for the targets, observer, and any intermediate objects in */
/*        a chain connecting the targets and observer that cover the */
/*        time period specified by the window CNFINE. If aberration */
/*        corrections are used, the states of target and observer */
/*        relative to the solar system barycenter must be calculable */
/*        from the available ephemeris data. Typically ephemeris data */
/*        are made available by loading one or more SPK files using */
/*        FURNSH. */

/*     -  In some cases the observer's state may be computed at times */
/*        outside of CNFINE by as much as 2 seconds; data required to */
/*        compute this state must be provided by loaded kernels. See */
/*        $Particulars for details. */

/*     Kernel data are normally loaded once per program run, NOT every */
/*     time this routine is called. */

/* $ Particulars */

/*                        ILLMN      OBS */
/*        ILLMN as seen      *       / */
/*        from TARG at       |      / */
/*        ET - LT.           |     / */
/*                          >|..../< phase angle */
/*                           |   / */
/*                         . |  / */
/*                       .   | / */
/*                      .     *     TARG as seen from OBS */
/*                SEP   .   TARG    at ET */
/*                       .  / */
/*                         / */
/*                        * */

/*     This routine determines if the caller-specified constraint */
/*     condition on the geometric event (phase angle) is satisfied for */
/*     any time intervals within the confinement window CNFINE. If one */
/*     or more such time intervals exist, those intervals are added */
/*     to the RESULT window. */

/*     This routine provides a simpler, but less flexible interface */
/*     than does the routine GFEVNT for conducting searches for */
/*     illuminator-target-observer phase angle value events. */
/*     Applications that require support for progress reporting, */
/*     interrupt handling, non-default step or refinement functions */
/*     should call GFEVNT rather than this routine. */

/*     Below we discuss in greater detail aspects of this routine's */
/*     solution process that are relevant to correct and efficient */
/*     use of this routine in user applications. */


/*     The Search Process */
/*     ================== */

/*     Regardless of the type of constraint selected by the caller, this */
/*     routine starts the search for solutions by determining the time */
/*     periods, within the confinement window, over which the */
/*     phase angle function is monotone increasing and monotone */
/*     decreasing. Each of these time periods is represented by a SPICE */
/*     window. Having found these windows, all of the phase angle */
/*     function's local extrema within the confinement window are known. */
/*     Absolute extrema then can be found very easily. */

/*     Within any interval of these "monotone" windows, there will be at */
/*     most one solution of any equality constraint. Since the boundary */
/*     of the solution set for any inequality constraint is contained in */
/*     the union of */

/*     -  the set of points where an equality constraint is met */

/*     -  the boundary points of the confinement window */

/*     the solutions of both equality and inequality constraints can be */
/*     found easily once the monotone windows have been found. */


/*     Step Size */
/*     ========= */

/*     The monotone windows (described above) are found using a two-step */
/*     search process. Each interval of the confinement window is */
/*     searched as follows: first, the input step size is used to */
/*     determine the time separation at which the sign of the rate of */
/*     change of phase angle will be sampled. Starting at */
/*     the left endpoint of an interval, samples will be taken at each */
/*     step. If a change of sign is found, a root has been bracketed; at */
/*     that point, the time at which the time derivative of the */
/*     phase angle is zero can be found by a refinement process, for */
/*     example, using a binary search. */

/*     Note that the optimal choice of step size depends on the lengths */
/*     of the intervals over which the phase angle function is monotone: */
/*     the step size should be shorter than the shortest of these */
/*     intervals (within the confinement window). */

/*     The optimal step size is *not* necessarily related to the lengths */
/*     of the intervals comprising the result window. For example, if */
/*     the shortest monotone interval has length 10 days, and if the */
/*     shortest result window interval has length 5 minutes, a step size */
/*     of 9.9 days is still adequate to find all of the intervals in the */
/*     result window. In situations like this, the technique of using */
/*     monotone windows yields a dramatic efficiency improvement over a */
/*     state-based search that simply tests at each step whether the */
/*     specified constraint is satisfied. The latter type of search can */
/*     miss solution intervals if the step size is longer than the */
/*     shortest solution interval. */

/*     Having some knowledge of the relative geometry of the target, */
/*     illumination source, and observer can be a valuable aid in */
/*     picking a reasonable step size. In general, the user can */
/*     compensate for lack of such knowledge by picking a very short */
/*     step size; the cost is increased computation time. */

/*     Note that the step size is not related to the precision with which */
/*     the endpoints of the intervals of the result window are computed. */
/*     That precision level is controlled by the convergence tolerance. */


/*     Convergence Tolerance */
/*     ===================== */

/*     As described above, the root-finding process used by this routine */
/*     involves first bracketing roots and then using a search process */
/*     to locate them. "Roots" are both times when local extrema are */
/*     attained and times when the geometric quantity function is equal */
/*     to a reference value. All endpoints of the intervals comprising */
/*     the result window are either endpoints of intervals of the */
/*     confinement window or roots. */

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
/*     interval within which a solution is sought. However, the */
/*     confinement window can, in some cases, be used to make searches */
/*     more efficient. Sometimes it's possible to do an efficient search */
/*     to reduce the size of the time period over which a relatively */
/*     slow search of interest must be performed. */

/*     Certain types of searches require the state of the observer, */
/*     relative to the solar system barycenter, to be computed at times */
/*     slightly outside the confinement window CNFINE. The time window */
/*     that is actually used is the result of "expanding" CNFINE by a */
/*     specified amount "T": each time interval of CNFINE is expanded by */
/*     shifting the interval's left endpoint to the left and the right */
/*     endpoint to the right by T seconds. Any overlapping intervals are */
/*     merged. (The input argument CNFINE is not modified.) */

/*     The window expansions listed below are additive: if both */
/*     conditions apply, the window expansion amount is the sum of the */
/*     individual amounts. */

/*     -  If a search uses an equality constraint, the time window */
/*        over which the state of the observer is computed is expanded */
/*        by 1 second at both ends of all of the time intervals */
/*        comprising the window over which the search is conducted. */

/*     -  If a search uses stellar aberration corrections, the time */
/*        window over which the state of the observer is computed is */
/*        expanded as described above. */

/*     When light time corrections are used, expansion of the search */
/*     window also affects the set of times at which the light time- */
/*     corrected state of the target is computed. */

/*     In addition to the possible 2 second expansion of the search */
/*     window that occurs when both an equality constraint and stellar */
/*     aberration corrections are used, round-off error should be taken */
/*     into account when the need for data availability is analyzed. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Determine the time windows from December 1, 2006 UTC to */
/*        January 31, 2007 UTC for which the sun-moon-earth configuration */
/*        phase angle satisfies the relation conditions with respect to a */
/*        reference value of .57598845 radians (the phase angle at */
/*        January 1, 2007 00:00:00.000 UTC, 33.001707 degrees). Also */
/*        determine the time windows corresponding to the local maximum */
/*        and minimum phase angles, and the absolute maximum and minimum */
/*        phase angles during the search interval. The configuration */
/*        defines the Sun as the illuminator, the Moon as the target, */
/*        and the Earth as the observer. */

/*        Use the meta-kernel shown below to load the required SPICE */
/*        kernels. */


/*           KPL/MK */

/*           File name: gfpa_ex1.tm */

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
/*              pck00009.tpc                  Planet orientation and */
/*                                            radii */
/*              naif0009.tls                  Leapseconds */

/*           \begindata */

/*              KERNELS_TO_LOAD = ( 'de421.bsp', */
/*                                  'pck00009.tpc', */
/*                                  'naif0009.tls'  ) */

/*           \begintext */

/*           End of meta-kernel */


/*        Example code begins here. */


/*              PROGRAM GFPA_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Include GF parameter declarations: */
/*        C */
/*              INCLUDE 'gf.inc' */

/*        C */
/*        C     SPICELIB functions */
/*        C */
/*              DOUBLE PRECISION      SPD */
/*              DOUBLE PRECISION      PHASEQ */

/*              INTEGER               WNCARD */

/*        C */
/*        C     Local parameters */
/*        C */
/*              INTEGER               LBCELL */
/*              PARAMETER           ( LBCELL = -5 ) */

/*        C */
/*        C     Use the parameter MAXWIN for both the result window size */
/*        C     and the workspace size. */
/*        C */
/*              INTEGER               MAXWIN */
/*              PARAMETER           ( MAXWIN = 1000 ) */

/*        C */
/*        C     Length of strings: */
/*        C */
/*              INTEGER               TIMLEN */
/*              PARAMETER           ( TIMLEN = 26 ) */

/*              INTEGER               NLOOPS */
/*              PARAMETER           ( NLOOPS = 7 ) */

/*        C */
/*        C     Local variables */
/*        C */
/*              CHARACTER*(TIMLEN)    RELATE (NLOOPS) */
/*              CHARACTER*(6)         ABCORR */
/*              CHARACTER*(6)         ILLMN */
/*              CHARACTER*(6)         OBSRVR */
/*              CHARACTER*(6)         TARGET */
/*              CHARACTER*(TIMLEN)    TIMSTR */

/*              DOUBLE PRECISION      CNFINE ( LBCELL : 2 ) */
/*              DOUBLE PRECISION      RESULT ( LBCELL : MAXWIN ) */
/*              DOUBLE PRECISION      WORK   ( LBCELL : MAXWIN, NWPA ) */
/*              DOUBLE PRECISION      ADJUST */
/*              DOUBLE PRECISION      ET0 */
/*              DOUBLE PRECISION      ET1 */
/*              DOUBLE PRECISION      FINISH */
/*              DOUBLE PRECISION      PHASE */
/*              DOUBLE PRECISION      REFVAL */
/*              DOUBLE PRECISION      START */
/*              DOUBLE PRECISION      STEP */

/*              INTEGER               I */
/*              INTEGER               J */

/*        C */
/*        C     Saved variables */
/*        C */
/*        C     The confinement, workspace and result windows CNFINE, */
/*        C     WORK and RESULT are saved because this practice helps to */
/*        C     prevent stack overflow. */
/*        C */
/*              SAVE                  CNFINE */
/*              SAVE                  RESULT */
/*              SAVE                  WORK */

/*        C */
/*        C     The relation values for the search. */
/*        C */
/*              DATA                  RELATE / '=', */
/*             .                               '<', */
/*             .                               '>', */
/*             .                               'LOCMIN', */
/*             .                               'ABSMIN', */
/*             .                               'LOCMAX', */
/*             .                               'ABSMAX'  / */


/*        C */
/*        C     Load kernels. */
/*        C */
/*              CALL FURNSH ( 'gfpa_ex1.tm' ) */

/*        C */
/*        C     Initialize windows. */
/*        C */
/*              CALL SSIZED ( MAXWIN, RESULT ) */
/*              CALL SSIZED ( 2,      CNFINE ) */

/*        C */
/*        C     Store the time bounds of our search interval in */
/*        C     the confinement window. */
/*        C */
/*              CALL STR2ET ( '2006 DEC 01', ET0 ) */
/*              CALL STR2ET ( '2007 JAN 31', ET1 ) */

/*              CALL WNINSD ( ET0, ET1, CNFINE ) */

/*        C */
/*        C     Search using a step size of 1 day (in units of seconds). */
/*        C     The reference value is 0.57598845 radians. We're not */
/*        C     using the adjustment feature, so we set ADJUST to zero. */
/*        C */
/*              STEP   = SPD() */
/*              REFVAL = 0.57598845D0 */
/*              ADJUST = 0.D0 */

/*        C */
/*        C     Define the values for target, observer, illuminator, and */
/*        C     aberration correction. */
/*        C */
/*              TARGET = 'MOON' */
/*              ILLMN  = 'SUN' */
/*              ABCORR = 'LT+S' */
/*              OBSRVR = 'EARTH' */

/*              DO J=1, NLOOPS */

/*                 WRITE(*,*) 'Relation condition: ', RELATE(J) */

/*        C */
/*        C        Perform the search. The SPICE window RESULT contains */
/*        C        the set of times when the condition is met. */
/*        C */
/*                 CALL GFPA (  TARGET,    ILLMN,  ABCORR, OBSRVR, */
/*             .                RELATE(J), REFVAL, ADJUST, STEP, */
/*             .                CNFINE,    MAXWIN, NWPA,   WORK, */
/*             .                RESULT ) */

/*        C */
/*        C        Display the results. */
/*        C */
/*                 IF ( WNCARD(RESULT) .EQ. 0 ) THEN */

/*                    WRITE (*, '(A)') 'Result window is empty.' */

/*                 ELSE */

/*                    DO I = 1, WNCARD(RESULT) */
/*        C */
/*        C              Fetch the endpoints of the Ith interval */
/*        C              of the result window. */
/*        C */
/*                       CALL WNFETD ( RESULT, I, START, FINISH ) */

/*                       PHASE = PHASEQ( START, TARGET, ILLMN, OBSRVR, */
/*             .                         ABCORR ) */
/*                       CALL TIMOUT ( START, */
/*             .                       'YYYY-MON-DD HR:MN:SC.###', */
/*             .                       TIMSTR                          ) */

/*                       WRITE (*, '(A,F16.9)') 'Start time = '//TIMSTR, */
/*             .                                                  PHASE */


/*                       PHASE = PHASEQ( FINISH, TARGET, ILLMN, OBSRVR, */
/*             .                         ABCORR ) */
/*                       CALL TIMOUT ( FINISH, */
/*             .                       'YYYY-MON-DD HR:MN:SC.###', */
/*             .                       TIMSTR                          ) */

/*                       WRITE (*, '(A,F16.9)') 'Stop time  = '//TIMSTR, */
/*             .                                                  PHASE */

/*                    END DO */

/*                 END IF */

/*                 WRITE(*,*) ' ' */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Relation condition: = */
/*        Start time = 2006-DEC-02 13:31:34.414       0.575988450 */
/*        Stop time  = 2006-DEC-02 13:31:34.414       0.575988450 */
/*        Start time = 2006-DEC-07 14:07:55.470       0.575988450 */
/*        Stop time  = 2006-DEC-07 14:07:55.470       0.575988450 */
/*        Start time = 2006-DEC-31 23:59:59.997       0.575988450 */
/*        Stop time  = 2006-DEC-31 23:59:59.997       0.575988450 */
/*        Start time = 2007-JAN-06 08:16:25.512       0.575988450 */
/*        Stop time  = 2007-JAN-06 08:16:25.512       0.575988450 */
/*        Start time = 2007-JAN-30 11:41:32.557       0.575988450 */
/*        Stop time  = 2007-JAN-30 11:41:32.557       0.575988450 */

/*         Relation condition: < */
/*        Start time = 2006-DEC-02 13:31:34.414       0.575988450 */
/*        Stop time  = 2006-DEC-07 14:07:55.470       0.575988450 */
/*        Start time = 2006-DEC-31 23:59:59.997       0.575988450 */
/*        Stop time  = 2007-JAN-06 08:16:25.512       0.575988450 */
/*        Start time = 2007-JAN-30 11:41:32.557       0.575988450 */
/*        Stop time  = 2007-JAN-31 00:00:00.000       0.468279091 */

/*         Relation condition: > */
/*        Start time = 2006-DEC-01 00:00:00.000       0.940714974 */
/*        Stop time  = 2006-DEC-02 13:31:34.414       0.575988450 */
/*        Start time = 2006-DEC-07 14:07:55.470       0.575988450 */
/*        Stop time  = 2006-DEC-31 23:59:59.997       0.575988450 */
/*        Start time = 2007-JAN-06 08:16:25.512       0.575988450 */
/*        Stop time  = 2007-JAN-30 11:41:32.557       0.575988450 */

/*         Relation condition: LOCMIN */
/*        Start time = 2006-DEC-05 00:16:50.317       0.086121423 */
/*        Stop time  = 2006-DEC-05 00:16:50.317       0.086121423 */
/*        Start time = 2007-JAN-03 14:18:31.977       0.079899769 */
/*        Stop time  = 2007-JAN-03 14:18:31.977       0.079899769 */

/*         Relation condition: ABSMIN */
/*        Start time = 2007-JAN-03 14:18:31.977       0.079899769 */
/*        Stop time  = 2007-JAN-03 14:18:31.977       0.079899769 */

/*         Relation condition: LOCMAX */
/*        Start time = 2006-DEC-20 14:09:10.392       3.055062862 */
/*        Stop time  = 2006-DEC-20 14:09:10.392       3.055062862 */
/*        Start time = 2007-JAN-19 04:27:54.600       3.074603891 */
/*        Stop time  = 2007-JAN-19 04:27:54.600       3.074603891 */

/*         Relation condition: ABSMAX */
/*        Start time = 2007-JAN-19 04:27:54.600       3.074603891 */
/*        Stop time  = 2007-JAN-19 04:27:54.600       3.074603891 */


/* $ Restrictions */

/*     1)  The kernel files to be used by this routine must be loaded */
/*         (normally using the SPICELIB routine FURNSH) before this */
/*         routine is called. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 27-OCT-2021 (JDR) (NJB) */

/*        Edited the header to comply with NAIF standard. */

/*        Updated description of RELATE, REFVAL, WORK and RESULT */
/*        arguments in $Brief_I/O, $Detailed_Input and $Detailed_Output. */

/*        Added SAVE statements for CNFINE, WORK and RESULT variables in */
/*        code example. */

/*        Replaced entry #9 by new entries #9 and #10, and added entry */
/*        #14 in $Exceptions. */

/*        Updated header to describe use of expanded confinement window. */

/* -    SPICELIB Version 1.0.0, 15-JUL-2014 (EDW) (NJB) */

/* -& */
/* $ Index_Entries */

/*     GF phase angle search */

/* -& */

/*     SPICELIB functions */


/*     Routines to set step size, refine transition times */
/*     and report work. */


/*     Local parameters */


/*     Local variables */


/*     Quantity definition parameter arrays: */


/*     Standard SPICE error handling. */

    /* Parameter adjustments */
    work_dim1 = *mw + 6;
    work_offset = work_dim1 - 5;

    /* Function Body */
    if (return_()) {
	return 0;
    }

/*     Check into the error subsystem. */

    chkin_("GFPA", (ftnlen)4);

/*     Confirm minimum window sizes. */

    if (*mw < 2 || odd_(mw)) {
	setmsg_("Workspace window size was #; size must be at least 2 and an"
		" even value.", (ftnlen)71);
	errint_("#", mw, (ftnlen)1);
	sigerr_("SPICE(INVALIDDIMENSION)", (ftnlen)23);
	chkout_("GFPA", (ftnlen)4);
	return 0;
    }
    if (*nw < 5) {
	setmsg_("Workspace window count was #; count must be at least #.", (
		ftnlen)55);
	errint_("#", nw, (ftnlen)1);
	errint_("#", &c__5, (ftnlen)1);
	sigerr_("SPICE(INVALIDDIMENSION)", (ftnlen)23);
	chkout_("GFPA", (ftnlen)4);
	return 0;
    }

/*     Check the result window size. */

    i__1 = sized_(result);
    if (sized_(result) < 2 || odd_(&i__1)) {
	setmsg_("Result window size was #; size must be at least 2 and an ev"
		"en value.", (ftnlen)68);
	i__1 = sized_(result);
	errint_("#", &i__1, (ftnlen)1);
	sigerr_("SPICE(INVALIDDIMENSION)", (ftnlen)23);
	chkout_("GFPA", (ftnlen)4);
	return 0;
    }

/*     Set up a call to GFEVNT specific to the phase angle search. */

    s_copy(qpnams, "TARGET", (ftnlen)80, (ftnlen)6);
    s_copy(qcpars, target, (ftnlen)80, target_len);
    s_copy(qpnams + 80, "OBSERVER", (ftnlen)80, (ftnlen)8);
    s_copy(qcpars + 80, obsrvr, (ftnlen)80, obsrvr_len);
    s_copy(qpnams + 160, "ABCORR", (ftnlen)80, (ftnlen)6);
    s_copy(qcpars + 160, abcorr, (ftnlen)80, abcorr_len);
    s_copy(qpnams + 240, "ILLUM", (ftnlen)80, (ftnlen)5);
    s_copy(qcpars + 240, illmn, (ftnlen)80, illmn_len);

/*     Set the step size. */

    gfsstp_(step);

/*     Retrieve the convergence tolerance, if set. */

    zzholdd_(&c_n1, &c__3, &ok, &tol);

/*     Use the default value CNVTOL if no stored tolerance value. */

    if (! ok) {
	tol = 1e-6;
    }

/*     Initialize the RESULT window to empty. */

    scardd_(&c__0, result);

/*     Look for solutions. */

/*     Progress report and interrupt options are set to .FALSE. */

    gfevnt_((U_fp)gfstep_, (U_fp)gfrefn_, "PHASE ANGLE", &c__4, qpnams, 
	    qcpars, qdpars, qipars, qlpars, relate, refval, &tol, adjust, 
	    cnfine, &c_false, (U_fp)gfrepi_, (U_fp)gfrepu_, (U_fp)gfrepf_, mw,
	     &c__5, work, &c_false, (L_fp)gfbail_, result, (ftnlen)11, (
	    ftnlen)80, (ftnlen)80, relate_len);
    chkout_("GFPA", (ftnlen)4);
    return 0;
} /* gfpa_ */

