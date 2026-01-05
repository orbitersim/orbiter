/* zzgfsolvx.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1000 = 1000;

/* $Procedure ZZGFSOLVX ( Private --- GF, event finding routine ) */
/* Subroutine */ int zzgfsolvx_(U_fp udfuns, S_fp udfunb, S_fp udstep, S_fp 
	udrefn, logical *bail, L_fp udbail, logical *cstep, doublereal *step, 
	doublereal *start, doublereal *finish, doublereal *tol, logical *rpt, 
	S_fp udrepu, doublereal *result)
{
    /* System generated locals */
    doublereal d__1, d__2;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    doublereal diff;
    extern /* Subroutine */ int zzwninsd_(doublereal *, doublereal *, char *, 
	    doublereal *, ftnlen);
    logical s;
    doublereal begin, t;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern doublereal dpmax_(void);
    extern /* Subroutine */ int errdp_(char *, doublereal *, ftnlen);
    integer nloop;
    logical l1, l2, savst;
    doublereal t1, t2;
    logical state1;
    extern logical failed_(void);
    extern doublereal brcktd_(doublereal *, doublereal *, doublereal *), 
	    touchd_(doublereal *);
    doublereal prvdif;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    logical instat;
    doublereal curtim, svdtim, timest;
    logical curste;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen);
    extern logical return_(void);
    char contxt[256];
    doublereal trnstn;

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     This routine is a root finding general purpose event location */
/*     routine. Most of the HARD work has been delegated to other */
/*     routines (In particular, how the dynamic step size is chosen). */

/*     Sister routine to ZZGFSOLV. Copy any edits to ZZGFSOLV or */
/*     ZZGFSOLVX to the sister routine. */

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

/*     ROOT */
/*     SEARCH */
/*     WINDOWS */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     UDFUNS     I   The routine that computes the scalar quantity of */
/*                    interest. */
/*     UDFUNB     I   Name of the routine that compares the current state */
/*                    condition with-respect-to a constraint. */
/*     UDSTEP     I   Name of the routine that computes a time step */
/*     UDREFN     I   Name of the routine that computes a refined time. */
/*     BAIL       I   Logical indicating program interrupt monitoring. */
/*     UDBAIL     I   Name of a routine that signals a program interrupt. */
/*     CSTEP      I   Logical indicating constant step size. */
/*     STEP       I   Constant step size in seconds for finding geometric */
/*                    events. */
/*     START      I   Beginning of the search interval. */
/*     FINISH     I   End of the search interval. */
/*     TOL        I   Maximum error in detection of state transitions. */
/*     RPT        I   Progress reporter on ( .TRUE.) or off ( .FALSE. ) */
/*     UDREPU     I   Function that updates the progress report. */
/*     RESULT    I-O  SPICE window containing results. */

/* $ Detailed_Input */

/*     UDFUNS     the routine that returns the value of the scalar */
/*                quantity of interest at time ET. The calling sequence */
/*                for UDFUNS is: */

/*                   CALL UDFUNS ( ET, VALUE ) */

/*                where: */

/*                   ET      a double precision value representing */
/*                           ephemeris time, expressed as seconds past */
/*                           J2000 TDB at which to determine the scalar */
/*                           value. */

/*                   VALUE   is the value of the scalar quantity */
/*                           at ET. */

/*     UDFUNB     the routine that determines if UDFUNS */
/*                satisfies some constraint condition at epoch ET. */

/*                The calling sequence: */

/*                   CALL UDFUNB ( UDFUNS, ET, BOOL ) */

/*                where: */

/*                   ET       a double precision value representing */
/*                            ephemeris time, expressed as seconds past */
/*                            J2000 TDB, at which to evaluate UDFUNS. */

/*                   BOOL     a logical value indicating whether */
/*                            or not UDFUNS satisfies the constraint */
/*                            at ET (TRUE) or not (FALSE). */

/*     UDSTEP     the routine that computes a time step in an attempt to */
/*                find a transition of the state of the specified */
/*                coordinate. In the context of this routine's algorithm, */
/*                a "state transition" occurs where the geometric state */
/*                changes from being in the desired geometric condition */
/*                event to not, or vice versa. */

/*                This routine relies on UDSTEP returning step sizes */
/*                small enough so that state transitions within the */
/*                confinement window are not overlooked.  There must */
/*                never be two roots A and B separated by less than */
/*                STEP, where STEP is the minimum step size returned by */
/*                UDSTEP for any value of ET in the interval [A, B]. */

/*                The calling sequence for UDSTEP is: */

/*                   CALL UDSTEP ( ET, STEP ) */

/*                where: */

/*                   ET      a double precision value representing */
/*                           ephemeris time, expressed as seconds past */
/*                           J2000 TDB, from which the algorithm is to */
/*                           search forward for a state transition. */

/*                   STEP    is the output step size. STEP indicates */
/*                           how far to advance ET so that ET and */
/*                           ET+STEP may bracket a state transition and */
/*                           definitely do not bracket more than one */
/*                           state transition. Units are TDB seconds. */

/*                If a constant step size is desired, the routine */

/*                   GFSTEP */

/*                may be used. This is the default option. If using */
/*                GFSTEP, the step size must be set by calling */

/*                   GFSSTP(STEP) */

/*                prior to calling this routine. */

/*     UDREFN     the routine that computes a refinement in the times */
/*                that bracket a transition point. In other words, once */
/*                a pair of times have been detected such that the system */
/*                is in different states at each of the two times, UDREFN */
/*                selects an intermediate time which should be closer to */
/*                the transition state than one of the two known times. */
/*                The calling sequence for UDREFN is: */

/*                   CALL UDREFN ( T1, T2, S1, S2, T ) */

/*                where the inputs are: */

/*                   T1    a time when the system is in state S1. */

/*                   T2    a time when the system is in state S2. T2 */
/*                         is assumed to be larger than T1. */

/*                   S1    a logical indicating the state of the system */
/*                         at time T1. */

/*                   S2    a logical indicating the state of the system */
/*                         at time T2. */

/*                UDREFN may use or ignore the S1 and S2 values. */

/*                The output is: */

/*                   T     a time to check for a state transition */
/*                         between T1 and T2. */

/*                If a simple bisection method is desired, the routine */
/*                GFREFN may be used. This is the default option. */

/*     BAIL       is a logical indicating whether or not interrupt */
/*                signaling is enabled. When `bail' is set to TRUE, */
/*                the input function UDBAIL (see description below) */
/*                is used to determine whether an interrupt has been */
/*                issued. */

/*     UDBAIL     the routine that indicates whether an interrupt signal */
/*                has been issued (for example, from the keyboard). */
/*                UDBAIL has no arguments and returns a logical. */
/*                The return value is .TRUE. if an interrupt has */
/*                been issued; otherwise the value is .FALSE. */

/*                ZZGFSOLVX uses UDBAIL only when BAIL (see above) is set */
/*                to .TRUE., indicating that interrupt handling is */
/*                enabled. When interrupt handling is enabled, ZZGFSOLVX */
/*                and will call UDBAIL to determine whether to terminate */
/*                processing and return immediately. */

/*                If interrupt handing is not enabled, a logical */
/*                function must still be passed as an input argument. */
/*                The function */

/*                   GFBAIL */

/*                may be used for this purpose. */

/*     CSTEP      is a logical indicating whether or not the step size */
/*                used in searching is constant.  If it is, the value */
/*                STEP is used. Note that even if UDSTEP has the value */
/*                GFSTEP, i.e. the public, constant step routine, CSTEP */
/*                should still be .FALSE., in which case STEP is ignored. */

/*     STEP       is the step size to be used in the search. STEP must */
/*                be short enough for a search using this step size */
/*                to locate the time intervals where the geometric */
/*                event function is monotone increasing or decreasing. */
/*                However, STEP must not be *too* short, or the */
/*                search will take an unreasonable amount of time. */

/*                The choice of STEP affects the completeness but not */
/*                the precision of solutions found by this routine; */
/*                precision is controlled by the convergence */
/*                the tolerance, TOL. */

/*                STEP has units of TDB seconds. */

/*     START      is the beginning of the interval over which the state */
/*                is to be detected. */

/*     FINISH     is the end of the interval over which the state is */
/*                to be detected. */

/*     TOL        is a tolerance value used to determine convergence of */
/*                root-finding operations. TOL is measured in seconds */
/*                and is greater than zero. */

/*     RPT        is a logical variable which controls whether the */
/*                progress reporter is enabled. When RPT is TRUE, */
/*                progress reporting is enabled and the routine */
/*                UDREPU (see description  below) reports progress. */

/*     UDREPU     the routine that updates the progress report for a */
/*                search. The calling sequence of UDREPU is */

/*                   UDREPU (IVBEG, IVEND, ET ) */

/*                   DOUBLE PRECISION      ET */
/*                   DOUBLE PRECISION      IVBEG */
/*                   DOUBLE PRECISION      IVEND */

/*                where ET is an epoch belonging to the confinement */
/*                window, IVBEG and IVEND are the start and stop times, */
/*                respectively of the current confinement window */
/*                interval.  The ratio of the measure of the portion */
/*                of CNFINE that precedes ET to the measure of CNFINE */
/*                would be a logical candidate for the searches */
/*                completion percentage; however the method of */
/*                measurement is up to the user. */

/*                If the user doesn't wish to provide a custom set of */
/*                progress reporting functions, the routine */

/*                   GFREPU */

/*                may be used. */

/*     RESULT     is an initialized SPICE window. RESULT may not be empty */
/*                on entry and must be large enough to hold all of the */
/*                intervals found by the search. */

/* $ Detailed_Output */

/*     RESULT     is a SPICE window containing the intersection of the */
/*                results of the search and the contents of RESULT */
/*                on entry. */

/*                When RESULT is empty on input, the intervals of the */
/*                output window stored in RESULT represent times when */
/*                the state function UDFUNB returns the value .TRUE. */

/* $ Parameters */

/*     LBCELL     is the SPICELIB cell lower bound. */

/* $ Exceptions */

/*     1)  If TOL is non-positive, the error SPICE(VALUEOUTOFRANGE) */
/*         will signal. */

/*     2)  If START is greater than FINISH or SVDTIM is greater than */
/*         CURTIM, SPICE(BADTIMECASE) will signal. */

/*     3)  If the inner convergence loop fails to converge to TOL */
/*         within MXLOOP iterations, the error SPICE(NOCONVERG) */
/*         will signal. */

/* $ Files */

/*     This routine computes states using SPK files that have been */
/*     loaded into the SPICE system, normally via the kernel loading */
/*     interface routine FURNSH. See the routine FURNSH and the SPK */
/*     and KERNEL Required Reading for further information on loading */
/*     (and unloading) kernels. */

/* $ Particulars */

/*     This routine implements a strategy for searching for geometric */
/*     state events important for planning solar system observations. */
/*     The actual details of selecting time steps while searching for */
/*     a state change as well as the scheme used for zeroing in on the */
/*     actual time of transition are handled by lower level routines. */

/*     By delegating the work of selecting search time steps and the */
/*     process of refining a transition time estimate to lower level */
/*     routines, the common work of the search can be isolated here. */
/*     The routines that do the decision making, can be modified */
/*     and made smarter as time permits. */

/* $ Examples */

/*      See GFUDS and ZZGFRELX. */

/* $ Restrictions */

/*      It is important that the user understand how the routines */
/*      UDFUNB, UDSTEP and UDREFN are to be used and that the */
/*      calling sequences match precisely with the descriptions given */
/*      here. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */
/*     W.L. Taber     (JPL) */
/*     I.M. Underwood (JPL) */
/*     L.S. Elson     (JPL) */
/*     E.D. Wright    (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.0,  31-JAN-2017 (NJB) (EDW) */

/*        Restrictions on the input tolerance have been loosened: */
/*        it is no longer required that the tolerance must yield */
/*        a new value when it is added to, or subtracted from, */
/*        either of the input interval bounds. The tolerance */
/*        still must be strictly positive. */

/* -    SPICELIB Version 1.2.0,  24-OCT-2010 (EDW) */

/*       TOL error check now returns SPICE(INVALIDTOLERANCE) instead of */
/*       previous return SPICE(VALUEOUTOFRANGE). */

/* -    SPICELIB Version 1.1.0,  16-FEB-2010 (EDW) */

/*        Modified version of ZZGFSOLV. */

/* -    SPICELIB Version 1.0.0, 17-MAR-2009 (EDW)(LSE)(NJB) */

/* -& */
/* $ Index_Entries */

/*     find times of an event */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     The maximum number of search loop iterations to execute. */
/*     The default refinement method is bisection, a very slow */
/*     method to convergence. Since 2**1000 ~ 10**301, */
/*     1000 loop iterations represents enough effort to assume */
/*     either the search will not converge or that the refinement */
/*     function operates slower than would bisection, in which */
/*     case the user should use the default GFREFN function. */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("ZZGFSOLVX", (ftnlen)9);

/*     Check the convergence tolerance. */

    if (*tol <= 0.) {
	setmsg_("Tolerance must be positive but was #.", (ftnlen)37);
	errdp_("#", tol, (ftnlen)1);
	sigerr_("SPICE(INVALIDTOLERANCE)", (ftnlen)23);
	chkout_("ZZGFSOLVX", (ftnlen)9);
	return 0;
    }

/*     Make sure that START is not greater than FINISH. Signal an */
/*     error for START > FINISH. */

    if (*start > *finish) {
	setmsg_("Bad time interval result, START > FINISH.", (ftnlen)41);
	sigerr_("SPICE(BADTIMECASE)", (ftnlen)18);
	chkout_("ZZGFSOLVX", (ftnlen)9);
	return 0;
    }

/*     If active, update the progress reporter. */

    if (*rpt) {
	(*udrepu)(start, finish, start);
    }

/*     This algorithm determines those intervals when a given state */
/*     is observed to occur within a specified search interval. */

/*     Pairs of times are recorded.  The first member of each pair */
/*     denotes the time when the system changes to the state of */
/*     interest.  The second denotes a transition out of that state. */

/*     If the system is in the state of interest at the beginning of */
/*     the interval, the beginning of the time interval will be */
/*     recorded.  This may or may not be a transition point. */

/*     Similarly if the system is in the state of interest at the end */
/*     of the interval, the end of the interval will be recorded. */
/*     Again, this may or may not be a transition point. */


/*     Initially the current time is the beginning of the search */
/*     interval. */

    curtim = *start;

/*     Determine if the state at the current time satisfies some */
/*     constraint. This constraint may indicate only existence of */
/*     a state. */

    (*udfunb)((U_fp)udfuns, &curtim, &curste);
    if (failed_()) {
	chkout_("ZZGFSOLVX", (ftnlen)9);
	return 0;
    }

/*     If the system is in the state of interest, record the initial */
/*     time of the search interval. */

    if (curste) {
	instat = TRUE_;
	begin = curtim;
    } else {
	instat = FALSE_;
    }

/*     If the step size is constant, use the value supplied. */

    if (*cstep) {
	timest = *step;
    }

/*     Save the current time and state somewhere. */

    svdtim = curtim;
    savst = curste;

/*     Once initializations have been performed keep working */
/*     until the search interval has been exhausted. */

/*     While time remains in the search interval. */

    while(svdtim < *finish) {

/*        Using the current window and internally stored */
/*        information about the current state, select a new current */
/*        time. */

	if (! (*cstep)) {
	    (*udstep)(&curtim, &timest);
	    if (failed_()) {
		chkout_("ZZGFSOLVX", (ftnlen)9);
		return 0;
	    }
	}

/*        Add the time step to the current time.  Make sure that the */
/*        time does not move beyond the end of the search interval. */

/* Computing MIN */
	d__1 = curtim + timest;
	curtim = min(d__1,*finish);

/*        Compute the state at time CURTIM. */

	(*udfunb)((U_fp)udfuns, &curtim, &curste);
	if (failed_()) {
	    chkout_("ZZGFSOLVX", (ftnlen)9);
	    return 0;
	}

/*        While the state remains unchanged and the interval is not */
/*        completely searched ... */

	while(savst == curste && svdtim < *finish) {

/*           First check for an interrupt signal if checking is enabled. */

	    if (*bail) {
		if ((*udbail)()) {
		    chkout_("ZZGFSOLVX", (ftnlen)9);
		    return 0;
		}
	    }

/*           Report the current time to the monitoring utility, if */
/*           appropriate. */

	    if (*rpt) {
		(*udrepu)(start, finish, &svdtim);
	    }

/*           Save the current time and state somewhere. */

	    svdtim = curtim;
	    savst = curste;

/*           Compute a new current time so that we will not step */
/*           past the end of the interval.  This time will be */
/*           based on: */

/*                 1. The kind of event we are looking for. */
/*                 2. The objects and observer class. */
/*                 3. Transition times already found. */
/*                 4. A minimum time step allowed. */

	    if (! (*cstep)) {
		(*udstep)(&curtim, &timest);
		if (failed_()) {
		    chkout_("ZZGFSOLVX", (ftnlen)9);
		    return 0;
		}
	    }
/* Computing MIN */
	    d__1 = curtim + timest;
	    curtim = min(d__1,*finish);

/*           Compute the current state */

	    (*udfunb)((U_fp)udfuns, &curtim, &curste);
	    if (failed_()) {
		chkout_("ZZGFSOLVX", (ftnlen)9);
		return 0;
	    }

/*           Loop back to see if the state has changed. */

	}

/*        If we have detected a state change and not merely run out */
/*        of the search interval... */

	if (savst != curste) {

/*           Call the previous state STATE1 */
/*           Call the current  state STATE2 */

/*           Call the time at state STATE1, T1 */
/*           Call the time at state STATE2, T2 */

/*           Save the current time. */

	    state1 = savst;
	    t1 = svdtim;
	    t2 = curtim;

/*           Set the states at T1 and T2 for use by the refinement */
/*           function, in case the caller has passed in a function */
/*           that uses them. */

	    l1 = savst;
	    l2 = curste;

/*           Make sure that T1 is not greater than T2. Signal an */
/*           error for T1 > T2. */

	    if (t1 > t2) {
		setmsg_("Bad time interval result, T1 > T2.", (ftnlen)34);
		sigerr_("SPICE(BADTIMECASE)", (ftnlen)18);
		chkout_("ZZGFSOLVX", (ftnlen)9);
		return 0;
	    }
	    svdtim = curtim;
	    savst = curste;

/*           T1 and T2 bracket the time of transition.  Squeeze this */
/*           interval down until it is less than some tolerance in */
/*           length.  Do it as described below... */

/*           Loop while the difference between the times T1 and T2 */
/*           exceeds a specified tolerance, and while the magnitude */
/*           of the difference is decreasing from one loop iteration */
/*           to the next. */

	    prvdif = dpmax_();
	    d__2 = (d__1 = t2 - t1, abs(d__1));
	    diff = touchd_(&d__2);
	    nloop = 0;
	    while(diff > *tol && diff < prvdif) {
		++nloop;

/*              This loop count error exists to catch pathologies */
/*              in the refinement function. The default bisection */
/*              refinement will converge before 1000 iterations if */
/*              a convergence is numerically possible. Any other */
/*              refinement function should require fewer iterations */
/*              compared to bisection. If not, the user should */
/*              probably use bisection. */

		if (nloop >= 1000) {
		    setmsg_("Loop run exceeds maximum loop count. Unable to "
			    "converge to TOL value #1 within MXLOOP value #2 "
			    "iterations.", (ftnlen)106);
		    errdp_("#1", tol, (ftnlen)2);
		    errint_("#2", &c__1000, (ftnlen)2);
		    sigerr_("SPICE(NOCONVERG)", (ftnlen)16);
		    chkout_("ZZGFSOLVX", (ftnlen)9);
		    return 0;
		}
		if (*bail) {
		    if ((*udbail)()) {
			chkout_("ZZGFSOLVX", (ftnlen)9);
			return 0;
		    }
		}

/*              Select a time T, between T1 and T2 (possibly based on the */
/*              values of L1 and L2). */

		(*udrefn)(&t1, &t2, &l1, &l2, &t);

/*              Check for an error signal. The default refinement */
/*              routine, GFREFN, does not include error checks. */

		if (failed_()) {
		    chkout_("ZZGFSOLVX", (ftnlen)9);
		    return 0;
		}

/*              Check whether T is between T1 and T2.  If */
/*              not then assume that we have gone as far as */
/*              we can in refining our estimate of the transition */
/*              point. Set T1 and T2 equal to T. */

		t = brcktd_(&t, &t1, &t2);
		if (t == t1) {
		    t2 = t;
		} else if (t == t2) {
		    t1 = t;
		} else {

/*                 Compute the state time T. If this state, S, */
/*                 equals STATE1, set T1 to T, otherwise set */
/*                 T2 to T. */

		    (*udfunb)((U_fp)udfuns, &t, &s);
		    if (s == state1) {
			t1 = t;
			l1 = s;
		    } else {
			t2 = t;
			l2 = s;
		    }
		}

/*              Update PRVDIF and DIFF for the next loop termination */
/*              test. */

		prvdif = diff;
		d__2 = (d__1 = t2 - t1, abs(d__1));
		diff = touchd_(&d__2);
	    }

/*           Let TRNSTN be the midpoint of [T1, T2].  Record this */
/*           time as marking the transition from STATE1 to STATE2. */

	    d__1 = (t1 + t2) * .5;
	    trnstn = brcktd_(&d__1, &t1, &t2);

/*           In state-of-interest or not? */

	    if (instat) {

/*              We were in the state of interest, TRNSTN marks the */
/*              point in time when the state changed to "not of */
/*              interest" We need to record the interval from BEGIN to */
/*              FINISH and note that we are no longer in the state of */
/*              interest. */


/*              Add an interval starting at BEGIN and ending at TRNSTN */
/*              to the result window. */

		s_copy(contxt, "Adding interval [BEGIN,TRNSTN] to RESULT. TR"
			"NSTN represents time of passage out of the state-of-"
			"interest.", (ftnlen)256, (ftnlen)105);
		zzwninsd_(&begin, &trnstn, contxt, result, (ftnlen)256);
	    } else {

/*              We were not in the state of interest.  As a result */
/*              TRNSTN marks the point where we are changing to */
/*              the state of interest.  Note that we have transitioned */
/*              to the state of interest and record the time at */
/*              which the transition occurred. */

		begin = trnstn;
	    }

/*           A transition occurred either from from in-state to */
/*           out-of-state or the inverse. Reverse the value of the */
/*           INSTAT flag to signify the transition event. */

	    instat = ! instat;

/*        That's it for this detection of state change. */

	}

/*        Continue if there is more time in the search interval. */

    }

/*     Check if in-state at this time (FINISH). If so record the */
/*     interval. */

    if (instat) {

/*        Add an interval starting at BEGIN and ending at FINISH to the */
/*        window. */

	s_copy(contxt, "Adding interval [BEGIN,FINISH] to RESULT. FINISH rep"
		"resents end of the search interval.", (ftnlen)256, (ftnlen)87)
		;
	zzwninsd_(&begin, finish, contxt, result, (ftnlen)256);
    }

/*     If active, update the progress reporter before exiting this */
/*     routine. */

    if (*rpt) {
	(*udrepu)(start, finish, finish);
    }

/*     Check-out then return. */

    chkout_("ZZGFSOLVX", (ftnlen)9);
    return 0;
} /* zzgfsolvx_ */

