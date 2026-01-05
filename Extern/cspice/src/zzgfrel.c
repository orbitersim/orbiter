/* zzgfrel.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__5 = 5;
static integer c__7 = 7;
static integer c__0 = 0;
static logical c_true = TRUE_;

/* $Procedure ZZGFREL ( Private --- GF, geometric relation finder ) */
/* Subroutine */ int zzgfrel_(U_fp udstep, U_fp udrefn, U_fp udqdec, U_fp 
	udcond, S_fp udfunc, S_fp udqref, char *relate, doublereal *refval, 
	doublereal *tol, doublereal *adjust, doublereal *cnfine, integer *mw, 
	integer *nw, doublereal *work, logical *rpt, S_fp udrepi, U_fp udrepu,
	 S_fp udrepf, char *rptpre, char *rptsuf, logical *bail, L_fp udbail, 
	doublereal *result, ftnlen relate_len, ftnlen rptpre_len, ftnlen 
	rptsuf_len)
{
    /* Initialized data */

    static char cnames[80*7] = "<                                           "
	    "                                    " "=                        "
	    "                                                       " ">     "
	    "                                                                "
	    "          " "LOCMIN                                             "
	    "                             " "ABSMIN                          "
	    "                                                " "LOCMAX       "
	    "                                                                "
	    "   " "ABSMAX                                                    "
	    "                      ";
    static logical cstep = FALSE_;

    /* System generated locals */
    integer work_dim1, work_dim2, work_offset, i__1, i__2, i__3;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen), s_rnge(char *, integer, 
	    char *, integer);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    doublereal addl, addr__;
    integer case__;
    logical need;
    integer name__[2], pass, want;
    doublereal step;
    extern /* Subroutine */ int zzgfsolv_(U_fp, U_fp, U_fp, logical *, L_fp, 
	    logical *, doublereal *, doublereal *, doublereal *, doublereal *,
	     logical *, U_fp, doublereal *), zzwninsd_(doublereal *, 
	    doublereal *, char *, doublereal *, ftnlen);
    integer i__;
    extern integer cardd_(doublereal *);
    extern /* Subroutine */ int zzgfwsts_(doublereal *, doublereal *, char *, 
	    doublereal *, ftnlen), chkin_(char *, ftnlen), ucase_(char *, 
	    char *, ftnlen, ftnlen), errch_(char *, char *, ftnlen, ftnlen);
    integer minat;
    doublereal endpt[2];
    integer maxat;
    doublereal value;
    extern integer sized_(doublereal *);
    extern /* Subroutine */ int errdp_(char *, doublereal *, ftnlen), copyd_(
	    doublereal *, doublereal *);
    integer qcnum;
    extern /* Subroutine */ int swapi_(integer *, integer *);
    integer count;
    doublereal start;
    extern /* Subroutine */ int ljust_(char *, char *, ftnlen, ftnlen);
    doublereal refer2;
    extern logical failed_(void);
    extern /* Subroutine */ int scardd_(integer *, doublereal *);
    extern integer isrchc_(char *, integer *, char *, ftnlen, ftnlen), 
	    wncard_(doublereal *);
    extern logical return_(void);
    char contxt[500], locrel[80];
    doublereal extrem, finish;
    integer winsiz;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen), sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), ssized_(integer *, doublereal *), wnexpd_(doublereal *, 
	    doublereal *, doublereal *), wnfetd_(doublereal *, integer *, 
	    doublereal *, doublereal *), wnextd_(char *, doublereal *, ftnlen)
	    , wnintd_(doublereal *, doublereal *, doublereal *), wndifd_(
	    doublereal *, doublereal *, doublereal *);

/* $ Abstract */

/*     SPICE private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     This routine determines time intervals when the value of some */
/*     geometric quantity related to one or more objects and an observer */
/*     satisfies a user specified constraint within time intervals */
/*     specified by the window CNFINE. */

/*     Sister routine to ZZGFRELX. Copy any edits to ZZGFREL or ZZGFRELX */
/*     to the sister routine. */

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

/*     SPK */
/*     TIME */
/*     NAIF_IDS */
/*     FRAMES */

/* $ Keywords */

/*     EPHEMERIS */
/*     GEOMETRY */
/*     SEARCH */

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
/*     LBCELL     P   SPICELIB cell lower bound. */
/*     NWREQ      P   Minimum number of workspace windows. */
/*     UDSTEP     I   Name of the routine that computes and returns a */
/*                    time step. */
/*     UDREFN     I   Name of the routine that computes a refined time. */
/*     UDQDEC     I   Name of the routine that computes whether the */
/*                    geometric quantity is decreasing. */
/*     UDCOND     I   Name of the routine that computes the geometric */
/*                    condition with-respect-to the constraint. */
/*     UDFUNC     I   The routine that computes the geometric quantity of */
/*                    interest. */
/*     UDQREF     I   Name of the routine that resets the current value */
/*                    of REFVAL. */
/*     RELATE     I   Operator that either looks for an extreme value */
/*                    (max, min, local, absolute) or compares the */
/*                    geometric quantity value and a number. */
/*     REFVAL     I   Value used as reference for geometric quantity */
/*                    condition. */
/*     TOL        I   Convergence tolerance in seconds. */
/*     ADJUST     I   Allowed variation for absolute extremal */
/*                    geometric conditions. */
/*     CNFINE     I   Confinement schedule */
/*     MW         I   Size of workspace windows. */
/*     NW         I   Number of workspace windows. */
/*     WORK       I   Array containing workspace windows */
/*     RPT        I   Progress reporter on ( .TRUE.) or off ( .FALSE. ) */
/*     UDREPI     I   Function that initializes progress reporting. */
/*     UDREPU     I   Function that updates the progress report. */
/*     UDREPF     I   Function that finalizes progress reporting. */
/*     RPTPRE     I   Progress reporter beginning message. */
/*     RPTSUF     I   Progress reporter ending message. */
/*     BAIL       I   Logical indicating program interrupt monitoring. */
/*     UDBAIL     I   Name of a routine that signals a program interrupt. */
/*     RESULT    I-O  SPICE window containing results. */


/* $ Detailed_Input */

/*     UDSTEP     the routine that computes a time step in an attempt to */
/*                find a transition of the state of the specified */
/*                coordinate. In the context of this routine's algorithm, */
/*                a "state transition" occurs where the coordinate value */
/*                changes from "decreasing" to "not decreasing" or vice */
/*                versa. */

/*                This routine relies on UDSTEP returning step sizes */
/*                small enough so that state transitions within the */
/*                confinement window are not overlooked.  There must */
/*                never be two roots A and B separated by less than */
/*                STEP, where STEP is the minimum step size returned by */
/*                UDSTEP for any value of ET in the interval [A, B]. */

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
/*                           state transition.  STEP is a DOUBLE */
/*                           PRECISION number.  Units are TDB seconds. */

/*                If a constant step size is desired, the routine */
/*                GFSTEP may be used. This is the default option. */

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

/*     UDQDEC     the routine that determines if the geometric quantity */
/*                is decreasing. */

/*                The calling sequence: */

/*                   CALL UDQDEC ( ET, ISDECR ) */

/*                where: */

/*                   ET       a double precision value representing */
/*                            ephemeris time, expressed as seconds past */
/*                            J2000 TDB, at which to determine the time */
/*                            derivative of the geometric quantity. */

/*                   ISDECR   a logical return indicating whether */
/*                            or not the geometric quantity */
/*                            is decreasing. ISDECR returns true if the */
/*                            time derivative of the geometric quantity */
/*                            at ET is negative. */

/*     UDCOND     the routine that determines if the geometric quantity */
/*                satisfies some constraint condition at epoch ET. */

/*                The calling sequence: */

/*                   CALL UDCOND ( ET, IN_CON ) */

/*                where: */

/*                   ET       a double precision value representing */
/*                            ephemeris time, expressed as seconds past */
/*                            J2000 TDB, at which to evaluate the */
/*                            geometric quantity. */

/*                   IN_CON   a logical value indicating whether or */
/*                            not the geometric quantity satisfies the */
/*                            constraint at ET (TRUE) or not (FALSE). */

/*     UDFUNC     the routine that returns the value of the geometric */
/*                quantity at the time of interest. The calling sequence */
/*                for UDFUNC is: */

/*                   CALL UDFUNC ( TIME, VALUE ) */

/*                where: */

/*                   TIME    a double precision value representing */
/*                           ephemeris time, expressed as seconds past */
/*                           J2000 TDB, at which  to determine */
/*                           the value of the geometric quantity. */

/*                   VALUE   is the value of the geometric quantity at */
/*                           time TIME. */

/*     UDQREF     the routine that resets the current value of REFVAL. */
/*                The calling sequence for UDQREF is: */

/*                   CALL UDQREF ( REFER2 ) */

/*                where REFER2 is a new value of REFVAL. */

/*     RELATE     is a comparison operator, indicating the numeric */
/*                constraint of interest. Values are: */

/*                '>'   value of geometric quantity greater than some */
/*                      reference (REFVAL). */

/*                '='   value of geometric quantity equal to some */
/*                      reference (REFVAL). */

/*                '<'   value of geometric quantity less than some */
/*                      reference (REFVAL). */

/*                ABSMAX-the geometric quantity is at an absolute */
/*                maximum. */

/*                ABSMIN-the geometric quantity is at an absolute */
/*                minimum. */

/*                LOCMAX-the geometric quantity is at an local maximum. */

/*                LOCMIN-the geometric quantity is at an local minimum. */

/*     REFVAL     Reference value for geometric quantity (in */
/*                radians, radians/sec, km, or km/sec as appropriate). */

/*     TOL        is a tolerance value used to determine convergence of */
/*                root-finding operations.  TOL is measured in seconds */
/*                and is greater than zero. */

/*     ADJUST     The amount by which the numerical quantity is */
/*                allowed to vary from an absolute extremum. If ADJUST */
/*                is non-zero, the resulting schedule contains */
/*                intervals when the geometric quantity has */
/*                values either between ABSMIN and ABSMIN + ADJUST */
/*                or between ABSMAX and ABSMAX - ADJUST. ADJUST must */
/*                not be negative. */

/*     CNFINE     is a SPICE window that confines the bounds of the */
/*                search. Note that like all windows (see windows.req) */
/*                CNFINE can contain multiple time intervals. See the */
/*                Examples section for information on how to create this */
/*                window. */

/*     MW         is the cell size of the windows in the workspace array */
/*                WORK. */

/*     NW         is the number of windows in the workspace array WORK. */
/*                NW must be at least as large as the parameter NWREQ. */

/*     WORK       is an array used to store workspace windows. This */
/*                array has dimensions WORK (-5 : MW, NW). */

/*     RPT        is a logical variable which controls whether the */
/*                progress reporter is on or off. The progress reporter */
/*                writes to the user's terminal. */

/*     UDREPI     the routine that initializes a progress report. */
/*                When progress reporting is enabled, UDREPI */
/*                is called at the start of a search.  The calling */
/*                sequence of UDREPI is: */

/*                   UDREPI ( CNFINE, RPTPRE, RPTSUF ) */

/*                   DOUBLE PRECISION    CNFINE ( LBCELL : * ) */
/*                   CHARACTER*(*)       RPTPRE */
/*                   CHARACTER*(*)       RPTSUF */

/*                where */

/*                   CNFINE */

/*                is the confinement window passed into ZZGFRELX, and */

/*                   RPTPRE */
/*                   RPTSUF */

/*                are prefix and suffix strings used in the progress */
/*                report:  these strings are intended to bracket a */
/*                representation of the fraction of work done. */

/*                If the user has no progress reporting initialization */
/*                routine, the SPICELIB routine GFRPIN may be used. This */
/*                is the default option. */

/*     UDREPU     the routine that updates the progress report for a */
/*                search.  The calling sequence of UDREPU is: */

/*                   UDREPU (IVBEG, IVEND, ET ) */

/*                   DOUBLE PRECISION      ET */
/*                   DOUBLE PRECISION      IVBEG */
/*                   DOUBLE PRECISION      IVEND */

/*                where ET is an epoch belonging to the confinement */
/*                window, IVBEG and IVEND are the start and stop times, */
/*                respectively of the current confinement window */
/*                interval.  The ratio of the measure of the portion */
/*                of CNFINE that precedes ET to the measure of CNFINE */
/*                would be a logical candidate for the search's */
/*                completion percentage; however the method of */
/*                measurement is up to the user. */

/*                If the user has no progress reporting update routine, */
/*                the SPICELIB routine GFRPUD may be used. This is the */
/*                default option. */

/*     UDREPF     the routine that finalizes a progress report. UDREPF */
/*                has no arguments. */

/*                If the user has no progress reporting finalizing */
/*                routine, the SPICELIB routine GFRPEN may be used. This */
/*                is the default option. */

/*     RPTPRE     is an array of strings containing the prefixes of */
/*                the output messages reported by the progress reporter. */
/*                The Ith element of RPTPRE is the prefix for the */
/*                message corresponding to the Ith traversal of the */
/*                confinement window executed by this routine; such */
/*                traversals are called "passes." The number of passes */
/*                executed depends on the relational operator RELATE. */
/*                Searches for local extrema and unadjusted absolute */
/*                extrema require one pass; searches for adjusted */
/*                absolute extrema, equalities, and inequalities require */
/*                two passes. */

/*                An example of the contents of RPTPRE for a distance */
/*                equality search: */

/*                   RPTPRE(1) = 'Distance pass 1 of 2' */
/*                   RPTPRE(2) = 'Distance pass 2 of 2' */

/*     RPTSUF     is an array of strings containing the suffixes of */
/*                the output messages reported by the progress reporter. */
/*                The Ith element of RPTSUF is the suffix for the */
/*                message corresponding to the Ith pass. */

/*                An example of the contents of RPTSUF for a distance */
/*                equality search: */

/*                   RPTSUF(1) = 'done.' */
/*                   RPTSUF(2) = 'done.' */

/*                For this search, the complete progress report message */
/*                for the Ith pass has the form */

/*                   'Distance pass I of 2 xxx.xx% done.' */

/*     BAIL       is a logical indicating whether or not interrupt */
/*                signaling is enabled. */

/*     UDBAIL     the routine that checks to see whether an interrupt */
/*                signal has been issued from, e.g. the keyboard. If */
/*                this capability is not to be used, a dummy function, */
/*                ZZGFBAIL must be supplied. */

/*     RESULT     is an initialized SPICE window. RESULT is large */
/*                enough to hold all of the intervals, within the */
/*                confinement window, on which the specified condition */
/*                is met. */

/* $ Detailed_Output */

/*     RESULT     is a SPICE window containing the time intervals within */
/*                the confinement window, over which the specified */
/*                condition is met. */

/*                RESULT is emptied before new values are assigned to */
/*                it. */

/* $ Parameters */

/*     LBCELL     is the SPICELIB cell lower bound. */

/*     NWREQ      is the required number of workspace windows; the */
/*                input argument NW must not be less than NWREQ. */

/* $ Exceptions */

/*     1)  A negative value for ADJUST causes the routine to signal */
/*         the error SPICE(VALUEOUTOFRANGE). A non-zero value for ADJUST */
/*         when RELATE has any value other than "ABSMIN" or "ABSMAX", */
/*         causes the routine to signal the error SPICE(INVALIDVALUE). */

/*     2)  If an improper comparison operator is specified, the error */
/*         SPICE(NOTRECOGNIZED) is signaled. */

/*     3)  If TOL is not greater than zero, the error */
/*         SPICE(VALUEOUTOFRANGE) will be signaled by routines called */
/*         from this routine. */

/*     4)  If the number of workspace windows is less than NWREQ, the */
/*         error SPICE(TOOFEWWINDOWS) is signaled. */

/*     5)  If the window size MW is less than 2, the error */
/*         SPICE(INVALIDDIMENSION) will be signaled. */

/*     6)  If the output SPICE window RESULT has insufficient capacity */
/*         to contain the number of intervals on which the specified */
/*         visibility condition is met, the error will be diagnosed */
/*         by a routine in the call tree of this routine. If the result */
/*         window has size less than 2, the error SPICE(WINDOWTOOSMALL) */
/*         will be signaled by this routine. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine determines time intervals when the value of some */
/*     geometric quantity related to one or more objects and an observer */
/*     satisfies a user specified constraint. It puts these times in a */
/*     result window called RESULT. It does this by first finding */
/*     schedules (windows) when the quantity of interest is either */
/*     monotonically increasing or decreasing. These schedules are then */
/*     manipulated to give the final result. Note that the determination */
/*     of "=" involves finding intervals where the quantity is "less */
/*     than" to a tolerance of TOL. This means that the end points of */
/*     these intervals are within TOL of being equal to the value. */

/* $ Examples */

/*     See GFEVNT. */

/* $ Restrictions */

/*     The kernel files to be used by ZZGFREL must be loaded (normally */
/*     via the SPICELIB routine FURNSH) before ZZGFREL is called. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */
/*     L.S. Elson     (JPL) */
/*     W.L. Taber     (JPL) */
/*     I.M. Underwood (JPL) */
/*     E.D. Wright    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1  21-DEC-2009 (EDW) */

/*        Edit to Abstract to document sister routine ZZGFRELX. */

/* -    SPICELIB Version 1.0.0  21-FEB-2009 (NJB) (LSE) (WLT) (IMU) (EDW) */

/* -& */
/* $ Index_Entries */

/*     determine when a scalar quantity satisfies a condition */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Workspace window indices: */


/*     Number of supported comparison operators. */

/*     One-letter alias for LBCELL to make references to the workspace */
/*     array tolerable: */


/*     Context string length: */


/*     Local variables */


/*     Saved variables */


/*     Below we initialize the list of comparison operator names. */

    /* Parameter adjustments */
    work_dim1 = *mw + 6;
    work_dim2 = *nw;
    work_offset = work_dim1 - 5;

    /* Function Body */

/*     Set constant step parameter to .FALSE.. */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("ZZGFREL", (ftnlen)7);

/*     Make sure we have enough workspace windows. */

    if (*nw < 5) {
	setmsg_("The number of workspace windows (#) is less than the minimu"
		"m #.", (ftnlen)63);
	errint_("#", nw, (ftnlen)1);
	errint_("#", &c__5, (ftnlen)1);
	sigerr_("SPICE(TOOFEWWINDOWS)", (ftnlen)20);
	chkout_("ZZGFREL", (ftnlen)7);
	return 0;
    }

/*     Make sure the workspace windows can contain at least one interval. */

    if (*mw < 2) {
	setmsg_("Workspace window size was #; size must be at least 2.", (
		ftnlen)53);
	errint_("#", mw, (ftnlen)1);
	sigerr_("SPICE(INVALIDDIMENSION)", (ftnlen)23);
	chkout_("ZZGFREL", (ftnlen)7);
	return 0;
    }

/*     Check the result window size. */

    if (sized_(result) < 2) {
	setmsg_("Result window size was #; size must be at least 2.", (ftnlen)
		50);
	i__1 = sized_(result);
	errint_("#", &i__1, (ftnlen)1);
	sigerr_("SPICE(INVALIDDIMENSION)", (ftnlen)23);
	chkout_("ZZGFREL", (ftnlen)7);
	return 0;
    }

/*     Make sure the requested comparison is one we recognize. */

    ljust_(relate, locrel, relate_len, (ftnlen)80);
    ucase_(locrel, locrel, (ftnlen)80, (ftnlen)80);
    qcnum = isrchc_(locrel, &c__7, cnames, (ftnlen)80, (ftnlen)80);
    if (qcnum == 0) {
	setmsg_("The comparison operator, # is not recognized.  Supported qu"
		"antities are: <, =, >, LOCMIN, ABSMIN, LOCMAX, ABSMAX.", (
		ftnlen)113);
	errch_("#", relate, (ftnlen)1, relate_len);
	sigerr_("SPICE(NOTRECOGNIZED)", (ftnlen)20);
	chkout_("ZZGFREL", (ftnlen)7);
	return 0;
    }

/*     Confirm ADJUST is non-negative. */

    if (*adjust < 0.) {
	setmsg_("ADJUST was #; must be non-negative.", (ftnlen)35);
	errdp_("#", adjust, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("ZZGFREL", (ftnlen)7);
	return 0;
    }

/*    Confirm ADJUST equals zero unless LOCREL (RELATE) has value */
/*    "ABSMAX" or "ABSMIN." */

    if (s_cmp(locrel, "ABSMIN", (ftnlen)80, (ftnlen)6) != 0 && s_cmp(locrel, 
	    "ABSMAX", (ftnlen)80, (ftnlen)6) != 0) {
	if (*adjust != 0.) {
	    setmsg_("ADJUST should have value zero for all comparison operat"
		    "ors except ABSMAX and ABSMIN", (ftnlen)83);
	    sigerr_("SPICE(INVALIDVALUE)", (ftnlen)19);
	    chkout_("ZZGFREL", (ftnlen)7);
	    return 0;
	}
    }

/*     If the confinement window is empty, the result window must */
/*     be empty as well.  In this case, there's not much to do. */

    if (cardd_(cnfine) == 0) {
	scardd_(&c__0, result);
	chkout_("ZZGFREL", (ftnlen)7);
	return 0;
    }

/*     We need to set up several working windows, one each for */
/*     increasing and decreasing schedules, one for the confining */
/*     schedule and one for copying. */

    ssized_(mw, &work[(i__1 = (work_dim1 << 1) - 5 - work_offset) < work_dim1 
	    * work_dim2 && 0 <= i__1 ? i__1 : s_rnge("work", i__1, "zzgfrel_",
	     (ftnlen)769)]);
    ssized_(mw, &work[(i__1 = work_dim1 - 5 - work_offset) < work_dim1 * 
	    work_dim2 && 0 <= i__1 ? i__1 : s_rnge("work", i__1, "zzgfrel_", (
	    ftnlen)770)]);
    ssized_(mw, &work[(i__1 = work_dim1 * 3 - 5 - work_offset) < work_dim1 * 
	    work_dim2 && 0 <= i__1 ? i__1 : s_rnge("work", i__1, "zzgfrel_", (
	    ftnlen)771)]);
    ssized_(mw, &work[(i__1 = (work_dim1 << 2) - 5 - work_offset) < work_dim1 
	    * work_dim2 && 0 <= i__1 ? i__1 : s_rnge("work", i__1, "zzgfrel_",
	     (ftnlen)772)]);
    ssized_(mw, &work[(i__1 = work_dim1 * 5 - 5 - work_offset) < work_dim1 * 
	    work_dim2 && 0 <= i__1 ? i__1 : s_rnge("work", i__1, "zzgfrel_", (
	    ftnlen)773)]);
    name__[0] = 2;
    name__[1] = 1;
    if (failed_()) {
	chkout_("ZZGFREL", (ftnlen)7);
	return 0;
    }

/*     For equality constraints, we work with a somewhat expanded */
/*     version of the confinement window so we can find equality */
/*     solutions that lie on the boundary of the original confinement */
/*     window. The expansion amount is ADDWIN. For other cases the */
/*     expansion amount is set to zero. */

    if (s_cmp(relate, "=", relate_len, (ftnlen)1) == 0) {
	addl = .5;
	addr__ = .5;
    } else {
	addl = 0.;
	addr__ = 0.;
    }
    copyd_(cnfine, &work[(i__1 = work_dim1 * 3 - 5 - work_offset) < work_dim1 
	    * work_dim2 && 0 <= i__1 ? i__1 : s_rnge("work", i__1, "zzgfrel_",
	     (ftnlen)799)]);
    wnexpd_(&addl, &addr__, &work[(i__1 = work_dim1 * 3 - 5 - work_offset) < 
	    work_dim1 * work_dim2 && 0 <= i__1 ? i__1 : s_rnge("work", i__1, 
	    "zzgfrel_", (ftnlen)800)]);
    if (failed_()) {
	chkout_("ZZGFREL", (ftnlen)7);
	return 0;
    }

/*     Make a local copy of the reference value. */

    refer2 = *refval;

/*     Set the pass number for progress reporting. */

    pass = 1;

/*     Initialize the work in progress reporter. */

    if (*rpt) {
	(*udrepi)(&work[(i__1 = work_dim1 * 3 - 5 - work_offset) < work_dim1 *
		 work_dim2 && 0 <= i__1 ? i__1 : s_rnge("work", i__1, "zzgfr"
		"el_", (ftnlen)821)], rptpre + (pass - 1) * rptpre_len, rptsuf 
		+ (pass - 1) * rptsuf_len, rptpre_len, rptsuf_len);
    }

/*     Look up the size of the confinement schedule... */

    count = wncard_(&work[(i__1 = work_dim1 * 3 - 5 - work_offset) < 
	    work_dim1 * work_dim2 && 0 <= i__1 ? i__1 : s_rnge("work", i__1, 
	    "zzgfrel_", (ftnlen)827)]);

/*     Start the window that contains intervals when the quantity of */
/*     interest is decreasing. The result will contain all intervals in */
/*     (expanded) CNFINE when the selected geometric quantity function */
/*     is decreasing, since this is how ZZGFSOLV is configured. */

    i__1 = count;
    for (i__ = 1; i__ <= i__1; ++i__) {

/*        Locate the bounds for the I'th interval of the confinement */
/*        schedule. Results are accumulated in the WORK array. */

	wnfetd_(&work[(i__2 = work_dim1 * 3 - 5 - work_offset) < work_dim1 * 
		work_dim2 && 0 <= i__2 ? i__2 : s_rnge("work", i__2, "zzgfre"
		"l_", (ftnlen)840)], &i__, &start, &finish);
	zzgfsolv_((U_fp)udqdec, (U_fp)udstep, (U_fp)udrefn, bail, (L_fp)
		udbail, &cstep, &step, &start, &finish, tol, rpt, (U_fp)
		udrepu, &work[(i__2 = (work_dim1 << 1) - 5 - work_offset) < 
		work_dim1 * work_dim2 && 0 <= i__2 ? i__2 : s_rnge("work", 
		i__2, "zzgfrel_", (ftnlen)842)]);
	if (failed_()) {
	    chkout_("ZZGFREL", (ftnlen)7);
	    return 0;
	}
	if (*bail) {
	    if ((*udbail)()) {
		if (*rpt) {
		    (*udrepf)();
		}
		chkout_("ZZGFREL", (ftnlen)7);
		return 0;
	    }
	}
    }
    if (*rpt) {
	(*udrepf)();
    }

/*     Let's think about what we have now. We have the intervals in the */
/*     confinement window when a value of some kind is decreasing. */

/*     The left endpoints are points at which the quantity begins */
/*     decreasing, thus they are times when the quantity is at a local */
/*     maximum (at least in the interior of the confinement window). */

/*     The right endpoints are where the quantity stops decreasing. Thus */
/*     those endpoints in the interior of the confinement window are */
/*     local minima of the quantity. */

/*     The complement relative to the confinement window is the set of */
/*     intervals within the confinement window for which the quantity is */
/*     increasing. At the left endpoints of the complement the */
/*     function is increasing. Thus the interior left endpoints are */
/*     local minima within the confinement window. The interior right */
/*     endpoints are local maxima within the confinement window. */

/*     Moreover, to within our ability to detect local extrema, there */
/*     are no local extrema within any of the intervals. Thus, the */
/*     function may be regarded as monotone within each of */
/*     the intervals of these windows. Thus for any desired value of the */
/*     quantity, there is at most one time within each of the intervals */
/*     that the desired value is achieved. */

    if (s_cmp(locrel, "LOCMIN", (ftnlen)80, (ftnlen)6) == 0) {

/*        We are interested in only interior minima of the quantity. */
/*        These occur at right endpoints of the intervals in TEMPW */
/*        that are interior points of CNFINE. First extract the right */
/*        endpoints. Then find those that are contained in the initial */
/*        confinement schedule, excluding endpoints. */

	wnextd_("R", &work[(i__1 = (work_dim1 << 1) - 5 - work_offset) < 
		work_dim1 * work_dim2 && 0 <= i__1 ? i__1 : s_rnge("work", 
		i__1, "zzgfrel_", (ftnlen)908)], (ftnlen)1);
	zzgfwsts_(&work[(i__1 = (work_dim1 << 1) - 5 - work_offset) < 
		work_dim1 * work_dim2 && 0 <= i__1 ? i__1 : s_rnge("work", 
		i__1, "zzgfrel_", (ftnlen)910)], cnfine, "()", result, (
		ftnlen)2);
	chkout_("ZZGFREL", (ftnlen)7);
	return 0;
    } else if (s_cmp(locrel, "LOCMAX", (ftnlen)80, (ftnlen)6) == 0) {

/*        We are interested in only interior maxima of the quantity. */
/*        These occur at right endpoints of the intervals in TEMPW */
/*        that are interior points of CNFINE. */

	wnextd_("L", &work[(i__1 = (work_dim1 << 1) - 5 - work_offset) < 
		work_dim1 * work_dim2 && 0 <= i__1 ? i__1 : s_rnge("work", 
		i__1, "zzgfrel_", (ftnlen)922)], (ftnlen)1);
	zzgfwsts_(&work[(i__1 = (work_dim1 << 1) - 5 - work_offset) < 
		work_dim1 * work_dim2 && 0 <= i__1 ? i__1 : s_rnge("work", 
		i__1, "zzgfrel_", (ftnlen)924)], cnfine, "()", result, (
		ftnlen)2);
	chkout_("ZZGFREL", (ftnlen)7);
	return 0;
    }

/*     We will need the intervals when the quantity of interest is */
/*     increasing in value. */

    if (s_cmp(locrel, "ABSMIN", (ftnlen)80, (ftnlen)6) == 0 || s_cmp(locrel, 
	    "ABSMAX", (ftnlen)80, (ftnlen)6) == 0) {

/*        We need an absolute max or min over the schedule CNFINE. */
/*        But we have decreasing values in WORK(B,DECRES). */
/*        Make a copy of WORK(B,DECRES) then compute the schedules */
/*        of decreasing or increasing quantity over the schedule CNFINE. */

	copyd_(&work[(i__1 = (work_dim1 << 1) - 5 - work_offset) < work_dim1 *
		 work_dim2 && 0 <= i__1 ? i__1 : s_rnge("work", i__1, "zzgfr"
		"el_", (ftnlen)942)], &work[(i__2 = (work_dim1 << 2) - 5 - 
		work_offset) < work_dim1 * work_dim2 && 0 <= i__2 ? i__2 : 
		s_rnge("work", i__2, "zzgfrel_", (ftnlen)942)]);
	wnintd_(cnfine, &work[(i__1 = (work_dim1 << 1) - 5 - work_offset) < 
		work_dim1 * work_dim2 && 0 <= i__1 ? i__1 : s_rnge("work", 
		i__1, "zzgfrel_", (ftnlen)944)], &work[(i__2 = work_dim1 * 5 
		- 5 - work_offset) < work_dim1 * work_dim2 && 0 <= i__2 ? 
		i__2 : s_rnge("work", i__2, "zzgfrel_", (ftnlen)944)]);
	copyd_(&work[(i__1 = work_dim1 * 5 - 5 - work_offset) < work_dim1 * 
		work_dim2 && 0 <= i__1 ? i__1 : s_rnge("work", i__1, "zzgfre"
		"l_", (ftnlen)945)], &work[(i__2 = (work_dim1 << 1) - 5 - 
		work_offset) < work_dim1 * work_dim2 && 0 <= i__2 ? i__2 : 
		s_rnge("work", i__2, "zzgfrel_", (ftnlen)945)]);
	wndifd_(cnfine, &work[(i__1 = (work_dim1 << 1) - 5 - work_offset) < 
		work_dim1 * work_dim2 && 0 <= i__1 ? i__1 : s_rnge("work", 
		i__1, "zzgfrel_", (ftnlen)947)], &work[(i__2 = work_dim1 * 5 
		- 5 - work_offset) < work_dim1 * work_dim2 && 0 <= i__2 ? 
		i__2 : s_rnge("work", i__2, "zzgfrel_", (ftnlen)947)]);
	copyd_(&work[(i__1 = work_dim1 * 5 - 5 - work_offset) < work_dim1 * 
		work_dim2 && 0 <= i__1 ? i__1 : s_rnge("work", i__1, "zzgfre"
		"l_", (ftnlen)948)], &work[(i__2 = work_dim1 - 5 - work_offset)
		 < work_dim1 * work_dim2 && 0 <= i__2 ? i__2 : s_rnge("work", 
		i__2, "zzgfrel_", (ftnlen)948)]);

/*        Here's what we plan to do, we want to look over two schedules */
/*        DECREASING and INCREASING to search for the absolute max or */
/*        min.  We start with DECREASING.  In this schedule the max is */
/*        always at the left endpoint,  The min is at the right */
/*        endpoint.  In the INCREASING schedule the min is at the LEFT */
/*        endpoint of an interval, the max is at the RIGHT endpoint of */
/*        an interval */

	minat = 2;
	maxat = 1;

/*        As yet we still need to compute our first extremum. */

	need = TRUE_;

/*        The extrema search is logically the same for both */
/*        maximum and minimum. We just need to keep track of */
/*        our extremum and when we find a more extreme value */
/*        replace it. DECREASING is first. */

	for (case__ = 1; case__ <= 2; ++case__) {
	    if (s_cmp(locrel, "ABSMIN", (ftnlen)80, (ftnlen)6) == 0) {
		want = minat;
	    } else if (s_cmp(locrel, "ABSMAX", (ftnlen)80, (ftnlen)6) == 0) {
		want = maxat;
	    }
	    winsiz = wncard_(&work[(i__2 = name__[(i__1 = case__ - 1) < 2 && 
		    0 <= i__1 ? i__1 : s_rnge("name", i__1, "zzgfrel_", (
		    ftnlen)986)] * work_dim1 - 5 - work_offset) < work_dim1 * 
		    work_dim2 && 0 <= i__2 ? i__2 : s_rnge("work", i__2, 
		    "zzgfrel_", (ftnlen)986)]);
	    i__1 = winsiz;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		wnfetd_(&work[(i__3 = name__[(i__2 = case__ - 1) < 2 && 0 <= 
			i__2 ? i__2 : s_rnge("name", i__2, "zzgfrel_", (
			ftnlen)990)] * work_dim1 - 5 - work_offset) < 
			work_dim1 * work_dim2 && 0 <= i__3 ? i__3 : s_rnge(
			"work", i__3, "zzgfrel_", (ftnlen)990)], &i__, endpt, 
			&endpt[1]);
		(*udfunc)(&endpt[(i__2 = want - 1) < 2 && 0 <= i__2 ? i__2 : 
			s_rnge("endpt", i__2, "zzgfrel_", (ftnlen)993)], &
			value);
		if (failed_()) {
		    chkout_("ZZGFREL", (ftnlen)7);
		    return 0;
		}

/*              Initialize the extreme value. This step will */
/*              be executed on the first pass through the */
/*              DECREASING interval. */

		if (need) {
		    need = FALSE_;
		    extrem = value;
		}

/*              Check to see if current VALUE is more extreme than */
/*              EXTREM. */

		if (s_cmp(locrel, "ABSMIN", (ftnlen)80, (ftnlen)6) == 0) {
		    if (*adjust == 0. && value <= extrem) {

/*                    Let's save the epoch in case it's that of the */
/*                    absolute min. Add this endpoint as a singleton */
/*                    interval to the RESULT window. */

			scardd_(&c__0, result);
			s_copy(contxt, "Saving current candidate epoch at wh"
				"ich an absolute minimum may occur.", (ftnlen)
				500, (ftnlen)70);
			zzwninsd_(&endpt[(i__2 = want - 1) < 2 && 0 <= i__2 ? 
				i__2 : s_rnge("endpt", i__2, "zzgfrel_", (
				ftnlen)1030)], &endpt[(i__3 = want - 1) < 2 &&
				 0 <= i__3 ? i__3 : s_rnge("endpt", i__3, 
				"zzgfrel_", (ftnlen)1030)], contxt, result, (
				ftnlen)500);
		    }
		    extrem = min(extrem,value);
		} else {
		    if (*adjust == 0. && value >= extrem) {

/*                    Let's save the epoch in case it's that of the */
/*                    absolute max. Add this endpoint as a singleton */
/*                    interval to the RESULT window. */

			scardd_(&c__0, result);
			s_copy(contxt, "Saving current candidate epoch at wh"
				"ich an absolute maximum may occur.", (ftnlen)
				500, (ftnlen)70);
			zzwninsd_(&endpt[(i__2 = want - 1) < 2 && 0 <= i__2 ? 
				i__2 : s_rnge("endpt", i__2, "zzgfrel_", (
				ftnlen)1052)], &endpt[(i__3 = want - 1) < 2 &&
				 0 <= i__3 ? i__3 : s_rnge("endpt", i__3, 
				"zzgfrel_", (ftnlen)1052)], contxt, result, (
				ftnlen)500);
		    }
		    extrem = max(extrem,value);
		}
	    }
	    if (failed_()) {
		chkout_("ZZGFREL", (ftnlen)7);
		return 0;
	    }

/*           When we go to the next schedule, the min and max are at */
/*           opposite ends of the intervals. */

	    swapi_(&minat, &maxat);
	}

/*        If the adjustment is zero, we're done. */

	if (*adjust == 0.) {
	    chkout_("ZZGFREL", (ftnlen)7);
	    return 0;
	}

/*        We have a non-zero adjustment. we have the extreme value. Now */
/*        we need to find the epochs when the extreme value is achieved, */
/*        allowing for adjustment. */

	if (s_cmp(locrel, "ABSMIN", (ftnlen)80, (ftnlen)6) == 0) {
	    refer2 = extrem + *adjust;
	} else {

/*           The only other possible value of LOCREL within this block */
/*           is 'ABSMAX'. */

	    refer2 = extrem - *adjust;
	}

/*        If we reach this point, we need to re-establish the */
/*        original expanded coverage of 'DECREASING' and 'INCREASING'. */

	copyd_(&work[(i__1 = (work_dim1 << 2) - 5 - work_offset) < work_dim1 *
		 work_dim2 && 0 <= i__1 ? i__1 : s_rnge("work", i__1, "zzgfr"
		"el_", (ftnlen)1107)], &work[(i__2 = (work_dim1 << 1) - 5 - 
		work_offset) < work_dim1 * work_dim2 && 0 <= i__2 ? i__2 : 
		s_rnge("work", i__2, "zzgfrel_", (ftnlen)1107)]);
    }
    wndifd_(&work[(i__1 = work_dim1 * 3 - 5 - work_offset) < work_dim1 * 
	    work_dim2 && 0 <= i__1 ? i__1 : s_rnge("work", i__1, "zzgfrel_", (
	    ftnlen)1111)], &work[(i__2 = (work_dim1 << 1) - 5 - work_offset) <
	     work_dim1 * work_dim2 && 0 <= i__2 ? i__2 : s_rnge("work", i__2, 
	    "zzgfrel_", (ftnlen)1111)], &work[(i__3 = work_dim1 - 5 - 
	    work_offset) < work_dim1 * work_dim2 && 0 <= i__3 ? i__3 : s_rnge(
	    "work", i__3, "zzgfrel_", (ftnlen)1111)]);
    if (failed_()) {
	chkout_("ZZGFREL", (ftnlen)7);
	return 0;
    }

/*     We have some kind of greater than, less than, or equal to */
/*     relation to solve for. Note that ABSMAX and ABSMIN are for case */
/*     where there is a non-zero adjustment. Reset the reference value, */
/*     which may have been changed in the ABSOLUTE MAX or MIN blocks */
/*     above. */

    (*udqref)(&refer2);

/*     If progress reporting is enabled, initialize the progress */
/*     reporter for a second pass over the confinement window. */

    if (*rpt) {

/*        Note that the window passed to UDREPI need not contain the */
/*        same intervals as those passed to UDREPU; the window passed to */
/*        UPREPI need only have the correct measure. From UDREPI's */
/*        perspective, the sole purpose of this window is to convey to */
/*        the progress reporting system the sum of the measures of the */
/*        increasing and decreasing windows. */

	pass = 2;
	(*udrepi)(&work[(i__1 = work_dim1 * 3 - 5 - work_offset) < work_dim1 *
		 work_dim2 && 0 <= i__1 ? i__1 : s_rnge("work", i__1, "zzgfr"
		"el_", (ftnlen)1143)], rptpre + (pass - 1) * rptpre_len, 
		rptsuf + (pass - 1) * rptsuf_len, rptpre_len, rptsuf_len);
    }

/*     Find those intervals when the geometric quantity is less than */
/*     REFER2. */

    scardd_(&c__0, result);
    for (case__ = 1; case__ <= 2; ++case__) {
	winsiz = wncard_(&work[(i__2 = name__[(i__1 = case__ - 1) < 2 && 0 <= 
		i__1 ? i__1 : s_rnge("name", i__1, "zzgfrel_", (ftnlen)1155)] 
		* work_dim1 - 5 - work_offset) < work_dim1 * work_dim2 && 0 <=
		 i__2 ? i__2 : s_rnge("work", i__2, "zzgfrel_", (ftnlen)1155)]
		);

/*        Search each interval of the window identified by NAME(CASE) for */
/*        times when the quantity is less than the reference value. */

	i__1 = winsiz;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    wnfetd_(&work[(i__3 = name__[(i__2 = case__ - 1) < 2 && 0 <= i__2 
		    ? i__2 : s_rnge("name", i__2, "zzgfrel_", (ftnlen)1163)] *
		     work_dim1 - 5 - work_offset) < work_dim1 * work_dim2 && 
		    0 <= i__3 ? i__3 : s_rnge("work", i__3, "zzgfrel_", (
		    ftnlen)1163)], &i__, &start, &finish);

/*           For each interval, accumulate the result in RESULT. */

/*           Note we know that the behavior of the quantity is monotonic */
/*           within each window, so the step size can be large. In fact, */
/*           we use the interval length as the step size. */

	    step = finish - start;
	    zzgfsolv_((U_fp)udcond, (U_fp)udstep, (U_fp)udrefn, bail, (L_fp)
		    udbail, &c_true, &step, &start, &finish, tol, rpt, (U_fp)
		    udrepu, result);
	    if (failed_()) {
		chkout_("ZZGFREL", (ftnlen)7);
		return 0;
	    }
	    if (*bail) {
		if ((*udbail)()) {
		    chkout_("ZZGFREL", (ftnlen)7);
		    return 0;
		}
	    }
	}
    }
    if (*rpt) {

/*        Finish the progress report for the second pass. */

	(*udrepf)();
    }

/*     RESULT is the window, within the expanded confinement window, */
/*     over which the function of interest is less than the reference */
/*     value. We can use this window to get whatever was requested. */

    if (s_cmp(locrel, "<", (ftnlen)80, (ftnlen)1) == 0 || s_cmp(locrel, "ABS"
	    "MIN", (ftnlen)80, (ftnlen)6) == 0) {

/*        We simply need to restrict our result to the original */
/*        confinement schedule. Note that the ABSMIN search with */
/*        non-zero adjustment is now a search for values less than the */
/*        adjusted absolute minimum. Same for ABSMAX below. */

	wnintd_(cnfine, result, &work[(i__1 = work_dim1 * 5 - 5 - work_offset)
		 < work_dim1 * work_dim2 && 0 <= i__1 ? i__1 : s_rnge("work", 
		i__1, "zzgfrel_", (ftnlen)1215)]);
	copyd_(&work[(i__1 = work_dim1 * 5 - 5 - work_offset) < work_dim1 * 
		work_dim2 && 0 <= i__1 ? i__1 : s_rnge("work", i__1, "zzgfre"
		"l_", (ftnlen)1216)], result);
    } else if (s_cmp(locrel, ">", (ftnlen)80, (ftnlen)1) == 0 || s_cmp(locrel,
	     "ABSMAX", (ftnlen)80, (ftnlen)6) == 0) {

/*        Subtract from the confinement window the window where the */
/*        quantity is less than the reference value: the remainder is */
/*        the portion of the confinement window on which the quantity is */
/*        greater than or equal to the reference value. */

	wndifd_(cnfine, result, &work[(i__1 = work_dim1 * 5 - 5 - work_offset)
		 < work_dim1 * work_dim2 && 0 <= i__1 ? i__1 : s_rnge("work", 
		i__1, "zzgfrel_", (ftnlen)1226)]);
	copyd_(&work[(i__1 = work_dim1 * 5 - 5 - work_offset) < work_dim1 * 
		work_dim2 && 0 <= i__1 ? i__1 : s_rnge("work", i__1, "zzgfre"
		"l_", (ftnlen)1227)], result);
    } else {

/*        This is the branch for the relational operator '='. */

/*        Create a window of singleton intervals from the endpoints */
/*        of RESULT. */

	scardd_(&c__0, &work[(i__1 = work_dim1 * 5 - 5 - work_offset) < 
		work_dim1 * work_dim2 && 0 <= i__1 ? i__1 : s_rnge("work", 
		i__1, "zzgfrel_", (ftnlen)1236)]);
	i__1 = cardd_(result);
	for (i__ = 1; i__ <= i__1; ++i__) {
	    s_copy(contxt, "Inserting endpoints of result window into worksp"
		    "ace window WORK(B,TEMPW). These points are candidate epo"
		    "chs that may satisfy an equality constraint.", (ftnlen)
		    500, (ftnlen)148);
	    zzwninsd_(&result[i__ + 5], &result[i__ + 5], contxt, &work[(i__2 
		    = work_dim1 * 5 - 5 - work_offset) < work_dim1 * 
		    work_dim2 && 0 <= i__2 ? i__2 : s_rnge("work", i__2, 
		    "zzgfrel_", (ftnlen)1245)], (ftnlen)500);
	    if (failed_()) {
		chkout_("ZZGFREL", (ftnlen)7);
		return 0;
	    }
	}

/*        The window WORK(B,TEMPW) contains singleton intervals where */
/*        either the equality constraint is met, or where a boundary */
/*        point of the expanded confinement window is located. We're not */
/*        interested in the boundary points; these are likely not */
/*        solution points and in any case are outside the original */
/*        confinement window. */

/*        Keep only the endpoints of RESULT that are contained in the */
/*        original confinement window CNFINE; these are by construction */
/*        interior points of the expanded confinement window. */

	wnintd_(cnfine, &work[(i__1 = work_dim1 * 5 - 5 - work_offset) < 
		work_dim1 * work_dim2 && 0 <= i__1 ? i__1 : s_rnge("work", 
		i__1, "zzgfrel_", (ftnlen)1267)], result);
    }
    chkout_("ZZGFREL", (ftnlen)7);
    return 0;
} /* zzgfrel_ */

