/* zzgfudb.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static logical c_false = FALSE_;
static doublereal c_b10 = 1.;

/* $Procedure ZZGFUDB ( Private --- GF, general use boolean search ) */
/* Subroutine */ int zzgfudb_(U_fp udfuns, U_fp udfunb, doublereal *tol, U_fp 
	udstep, U_fp udrefn, logical *rpt, S_fp udrepi, U_fp udrepu, S_fp 
	udrepf, logical *bail, L_fp udbail, doublereal *cnfine, doublereal *
	result)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    integer i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen), errdp_(char *, 
	    doublereal *, ftnlen);
    integer count;
    doublereal start;
    extern logical failed_(void);
    extern /* Subroutine */ int zzgfsolvx_(U_fp, U_fp, U_fp, U_fp, logical *, 
	    L_fp, logical *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, logical *, U_fp, doublereal *);
    extern integer wncard_(doublereal *);
    doublereal finish;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), wnfetd_(doublereal *, integer *,
	     doublereal *, doublereal *);
    extern logical return_(void);

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Routine to determine time intervals when a user defined boolean */
/*     function returns true. Report progress and handle interrupts */
/*     if so commanded. */

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

/*     GF */
/*     TIME */
/*     WINDOWS */

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

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     UDFUNS     I   Name of the routine that computes a scalar */
/*                    quantity of interest. */
/*     UDFUNB     I   Name of the routine that computes the boolean */
/*                    value of interest. */
/*     TOL        I   Convergence tolerance in seconds. */
/*     UDSTEP     I   Name of the routine that returns a time step. */
/*     UDREFN     I   Name of the routine that computes a refined time. */
/*     RPT        I   Progress report flag. */
/*     UDREPI     I   Function that initializes progress reporting. */
/*     UDREPU     I   Function that updates the progress report. */
/*     UDREPF     I   Function that finalizes progress reporting. */
/*     BAIL       I   Logical indicating program interrupt monitoring. */
/*     UDBAIL     I   Name of a routine that signals a program interrupt. */
/*     CNFINE     I   SPICE window to which the search is restricted. */
/*     RESULT     O   SPICE window containing results. */

/* $ Detailed_Input */

/*     UDFUNS     the routine that returns the value of the scalar */
/*                quantity of interest at time ET. The calling sequence */
/*                for UDFUNC is: */

/*                   CALL UDFUNS ( ET, VALUE ) */

/*                where: */

/*                   ET      a double precision value representing */
/*                           ephemeris time, expressed as seconds past */
/*                           J2000 TDB at which to determine the scalar */
/*                           value. */

/*                   VALUE   is the value of the scalar quantity */
/*                           at ET. */


/*     UDFUNB     the user defined routine returning a boolean value */
/*                for an epoch ET. */

/*                The calling sequence: */

/*                   CALL UDFUNB ( UDFUNS, ET, BOOL ) */

/*                where: */

/*                   UDFUNS   a scalar function as previously defined. */

/*                   ET       a double precision value representing */
/*                            ephemeris time, expressed as seconds past */
/*                            J2000 TDB, at which to evaluate UDFUNB. */

/*                   BOOL     the boolean value at ET. */


/*     TOL        is a tolerance value used to determine convergence of */
/*                root-finding operations. TOL is measured in TDB seconds */
/*                and must be greater than zero. */


/*     UDSTEP     is an externally specified routine that computes a */
/*                time step used to find transitions of the state being */
/*                considered. A state transition occurs where the state */
/*                changes from being "in state" to being "not in */
/*                state" or vice versa. */

/*                This routine relies on UDSTEP returning step sizes */
/*                small enough so that state transitions within the */
/*                confinement window are not overlooked. */

/*                The calling sequence for UDSTEP is: */

/*                   CALL UDSTEP ( ET, STEP ) */

/*                where: */

/*                   ET      is the input start time from which the */
/*                           algorithm is to search forward for a state */
/*                           transition. ET is expressed as seconds past */
/*                           J2000 TDB. ET is a DOUBLE PRECISION number. */

/*                   STEP    is the output step size.  STEP indicates */
/*                           how far to advance ET so that ET and */
/*                           ET+STEP may bracket a state transition and */
/*                           definitely do not bracket more than one */
/*                           state transition. STEP is a DOUBLE */
/*                           PRECISION number. Units are TDB seconds. */

/*                If a constant step size is desired, the routine GFSTEP */
/*                may be used. If GFSTEP is used, the step size must be */
/*                set by calling GFSSTP prior to calling this routine. */


/*     UDREFN     is the name of the externally specified routine that */
/*                refines the times that bracket a transition point. In */
/*                other words, once a pair of times, T1 and T2, that */
/*                bracket a state transition have been found, UDREFN */
/*                computes an intermediate time T such that either */
/*                [T1, T] or [T, T2] contains the time of the state */
/*                transition. The calling sequence for UDREFN is: */

/*                   CALL UDREFN ( T1, T2, S1, S2, T ) */

/*                where the inputs are: */

/*                   T1    is a time when the visibility state is S1. T1 */
/*                         is expressed as seconds past J2000 TDB. */

/*                   T2    is a time when the visibility state is S2. T2 */
/*                         is expressed as seconds past J2000 TDB. and */
/*                         is assumed to be larger than T1. */

/*                   S1    is the visibility state at time T1. S1 is a */
/*                         LOGICAL value. */

/*                   S2    is the visibility state at time T2. S2 is a */
/*                         LOGICAL value. */

/*                The output is: */

/*                   T     is the next time to check for a state */
/*                         transition. T is expressed as seconds past */
/*                         J2000 TDB and is between T1 and T2. */

/*                If a simple bisection method is desired, the routine */
/*                GFREFN may be used. */


/*     RPT        is a logical variable which controls whether */
/*                progress reporting is enabled. When RPT is .TRUE., */
/*                progress reporting is enabled and the routines */
/*                UDREPI, UDREPU, and UDPREF (see descriptions below) */
/*                are used to report progress. */


/*     UDREPI     is a user-defined subroutine that initializes a */
/*                progress report. When progress reporting is */
/*                enabled, UDREPI is called at the start */
/*                of a search. The calling sequence of UDREPI is */

/*                   UDREPI ( CNFINE, SRCPRE, SRCSUF ) */

/*                   DOUBLE PRECISION    CNFINE ( LBCELL : * ) */
/*                   CHARACTER*(*)       SRCPRE */
/*                   CHARACTER*(*)       SRCSUF */

/*                where */

/*                   CNFINE */

/*                is the confinement window and */

/*                   SRCPRE */
/*                   SRCSUF */

/*                are prefix and suffix strings used in the progress */
/*                report: these strings are intended to bracket a */
/*                representation of the fraction of work done.  For */
/*                example, when the CSPICE progress reporting functions */
/*                are used, if SRCPRE and SRCSUF are, respectively, */

/*                   "User defined boolean event search" */
/*                   "done." */

/*                the progress report display at the end of the */
/*                search will be: */

/*                   User defined boolean event search 100.00% done. */

/*                The SPICELIB routine GFREPI may be used as the */
/*                actual argument corresponding to UDREPI. If so, */
/*                the SPICELIB routines GFREPU and GFREPF must be */
/*                the actual arguments corresponding to UDREPU and */
/*                UDREPF. */


/*     UDREPU     is a user-defined subroutine that updates the */
/*                progress report for a search.  The calling sequence */
/*                of UDREPU is */

/*                   UDREPU ( IVBEG, IVEND, ET ) */

/*                   DOUBLE PRECISION      IVBEG */
/*                   DOUBLE PRECISION      IVEND */
/*                   DOUBLE PRECISION      ET */

/*                Here IVBEG, IVEND are the bounds of an interval that */
/*                is contained in some interval belonging to the */
/*                confinement window. The confinement window is */
/*                associated with some root finding activity. It is used */
/*                to determine how much total time is being searched in */
/*                order to find the events of interest. */

/*                ET is an epoch belonging to the interval */
/*                [IVBEG, IVEND]. */

/*                In order for a meaningful progress report to be */
/*                displayed, IVBEG and IVEND must satisfy the following */
/*                constraints: */

/*                 - IVBEG must be less than or equal to IVEND. */

/*                 - The interval [ IVBEG, IVEND ] must be contained in */
/*                   some interval of the confinement window. It can be */
/*                   a proper subset of the containing interval; that */
/*                   is, it can be smaller than the interval of the */
/*                   confinement window that contains it. */

/*                 - Over a search, the sum of the differences */

/*                      IVEND - IVBEG */

/*                   for all calls to this routine made during the search */
/*                   must equal the measure of the confinement window. */

/*                The SPICELIB routine GFREPU may be used as the */
/*                actual argument corresponding to UDREPU. If so, */
/*                the SPICELIB routines GFREPI and GFREPF must be */
/*                the actual arguments corresponding to UDREPI and */
/*                UDREPF. */


/*     UDREPF     is a user-defined subroutine that finalizes a */
/*                progress report. UDREPF has no arguments. */

/*                The SPICELIB routine GFREPF may be used as the */
/*                actual argument corresponding to UDREPF. If so, */
/*                the SPICELIB routines GFREPI and GFREPU must be */
/*                the actual arguments corresponding to UDREPI and */
/*                UDREPU. */


/*     BAIL       is a logical variable indicating whether or not */
/*                interrupt handling is enabled. When BAIL is */
/*                set to .TRUE., the input function UDBAIL (see */
/*                description below) is used to determine whether */
/*                an interrupt has been issued. */


/*     UDBAIL     is the name of a user defined logical function that */
/*                indicates whether an interrupt signal has been */
/*                issued (for example, from the keyboard).  UDBAIL */
/*                has no arguments and returns a LOGICAL value. */
/*                The return value is .TRUE. if an interrupt has */
/*                been issued; otherwise the value is .FALSE. */

/*                GFUDB uses UDBAIL only when BAIL (see above) is set */
/*                to .TRUE., indicating that interrupt handling is */
/*                enabled. When interrupt handling is enabled, GFUDB */
/*                and routines in its call tree will call UDBAIL to */
/*                determine whether to terminate processing and return */
/*                immediately. */

/*                If interrupt handing is not enabled, a logical */
/*                function must still be passed to GFUDB as */
/*                an input argument. The SPICE function */

/*                   GFBAIL */

/*                may be used for this purpose. */


/*     CNFINE     is a SPICE window that confines the time period over */
/*                which the specified search is conducted. CNFINE may */
/*                consist of a single interval or a collection of */
/*                intervals. */

/*                The endpoints of the time intervals comprising CNFINE */
/*                are interpreted as seconds past J2000 TDB. */

/*                See the Examples section below for a code example */
/*                that shows how to create a confinement window. */

/*                CNFINE must be initialized by the caller via the */
/*                SPICELIB routine SSIZED. */


/* $ Detailed_Output */

/*     RESULT     is a SPICE window representing the set of time */
/*                intervals, within the confinement period, when the */
/*                specified boolean function has a value true. */

/*                The endpoints of the time intervals comprising RESULT */
/*                are interpreted as seconds past J2000 TDB. */

/*                If RESULT is non-empty on input, its contents */
/*                will be discarded before GFUDB conducts its */
/*                search. */

/* $ Parameters */

/*     LBCELL     is the SPICELIB cell lower bound. */

/* $ Exceptions */

/*     1)  SPICE(INVALIDTOLERANCE) will signal if the convergence */
/*         tolerance value is non-positive. */

/* $ Files */

/*     Appropriate kernels must be loaded by the calling program before */
/*     this routine is called. */

/*     If the boolean function requires access to ephemeris data: */

/*        - SPK data: ephemeris data for any body over the */
/*          time period defined by the confinement window must be */
/*          loaded. If aberration corrections are used, the states of */
/*          target and observer relative to the solar system barycenter */
/*          must be calculable from the available ephemeris data. */
/*          Typically ephemeris data are made available by loading one */
/*          or more SPK files via FURNSH. */

/*        - If non-inertial reference frames are used, then PCK */
/*          files, frame kernels, C-kernels, and SCLK kernels may be */
/*          needed. */

/*     In all cases, kernel data are normally loaded once per program */
/*     run, NOT every time this routine is called. */

/* $ Particulars */

/*     This routine provides the SPICE GF system's most flexible */
/*     interface for searching for boolean events based on a user */
/*     defined boolean function. */

/*     Applications that do not require support for progress */
/*     reporting, interrupt handling, non-default step refinement */
/*     functions, or non-default convergence tolerance normally should */
/*     call GFUDB rather than this routine. */

/*     This routine determines a set of one or more time intervals */
/*     within the confinement window when the boolean function */
/*     satisfies a caller-specified constraint. The resulting set of */
/*     intervals is returned as a SPICE window. */

/*     Below we discuss in greater detail aspects of this routine's */
/*     solution process that are relevant to correct and efficient */
/*     use of this routine in user applications. */

/*     The Search Process */
/*     ================== */

/*     The search for boolean events is treated as a search for state */
/*     transitions: times are sought when the boolean function value */
/*     changes from true to false or vice versa. */

/*     Step Size */
/*     ========= */

/*     Each interval of the confinement window is searched as follows: */
/*     first, the input step size is used to determine the time */
/*     separation at which the boolean function will be sampled. */
/*     Starting at the left endpoint of the interval, samples of the */
/*     boolean function will be taken at each step. If a state change */
/*     is detected, a root has been bracketed; at that point, the */
/*     "root"--the time at which the state change occurs---is found by a */
/*     refinement process, for example, via binary search. */

/*     Note that the optimal choice of step size depends on the lengths */
/*     of the intervals over which the boolean function is constant: */
/*     the step size should be shorter than the shortest such interval */
/*     and the shortest separation between the intervals, within */
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
/*     a time window over which required data are known to be */
/*     available. */

/*     In some cases, the confinement window be used to make searches */
/*     more efficient. Sometimes it's possible to do an efficient search */
/*     to reduce the size of the time period over which a relatively */
/*     slow search of interest must be performed. See the "CASCADE" */
/*     example program in gf.req for a demonstration. */

/* $ Examples */

/*    Refer to GFUDB. */

/* $ Restrictions */

/*     1) Any kernel files required by this routine must be loaded */
/*        (normally via the SPICELIB routine FURNSH) before this routine */
/*        is called. */

/* $ Literature_References */

/*    None. */

/* $ Author_and_Institution */

/*    N.J. Bachman   (JPL) */
/*    E.D. Wright    (JPL) */

/* $ Version */

/* -   SPICELIB Version 1.0.0  17-OCT-2013 (EDW) */

/*       Logic and implementation based on GFOCCE by Nat Bachman. */

/* -& */
/* $ Index_Entries */

/*   GF user defined boolean function search */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     STEP is a step size initializer for the unused, dummy step size */
/*     argument to ZZGFSOLVX. The routine UDSTEP, which is passed to */
/*     ZZGFSOLVX, will be used by that routine to obtain the step size. */


/*     CSTEP indicates whether a constant step size, provided */
/*     via the input argument STEP, is to be used by ZZGFSOLVX. */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("ZZGFUDB", (ftnlen)7);

/*     Check the convergence tolerance. */

    if (*tol <= 0.) {
	setmsg_("Tolerance must be positive but was #.", (ftnlen)37);
	errdp_("#", tol, (ftnlen)1);
	sigerr_("SPICE(INVALIDTOLERANCE)", (ftnlen)23);
	chkout_("ZZGFUDB", (ftnlen)7);
	return 0;
    }

/*     Prepare the progress reporter if appropriate. */

    if (*rpt) {
	(*udrepi)(cnfine, "User defined boolean event search ", "done.", (
		ftnlen)34, (ftnlen)5);
    }

/*     Cycle over the intervals in the confining window. */

    count = wncard_(cnfine);
    i__1 = count;
    for (i__ = 1; i__ <= i__1; ++i__) {

/*        Retrieve the bounds for the Ith interval of the confinement */
/*        window. Search this interval for boolean events. Union the */
/*        result with the contents of the RESULT window. */

	wnfetd_(cnfine, &i__, &start, &finish);

/*        Call ZZGFSOLVX to do the event detection work. The boolean */
/*        function passes as UDFUNB, the scalar as UDFUNS. */

	zzgfsolvx_((U_fp)udfuns, (U_fp)udfunb, (U_fp)udstep, (U_fp)udrefn, 
		bail, (L_fp)udbail, &c_false, &c_b10, &start, &finish, tol, 
		rpt, (U_fp)udrepu, result);
	if (failed_()) {
	    chkout_("ZZGFUDB", (ftnlen)7);
	    return 0;
	}
	if (*bail) {

/*           Interrupt handling is enabled. */

	    if ((*udbail)()) {

/*              An interrupt has been issued. Return now regardless of */
/*              whether the search completed. */

		chkout_("ZZGFUDB", (ftnlen)7);
		return 0;
	    }
	}
    }

/*     End the progress report. */

    if (*rpt) {
	(*udrepf)();
    }
    chkout_("ZZGFUDB", (ftnlen)7);
    return 0;
} /* zzgfudb_ */

