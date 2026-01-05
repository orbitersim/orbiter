/* zzgfrpwk.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__5 = 5;
static integer c__1 = 1;
static integer c__0 = 0;
static doublereal c_b19 = 0.;
static doublereal c_b20 = 100.;

/* $Procedure    ZZGFRPWK ( Geometry finder report work done on a task ) */
/* Subroutine */ int zzgfrpwk_0_(int n__, integer *unit, doublereal *total, 
	doublereal *wait, integer *tcheck, char *begin, char *end, doublereal 
	*incr, ftnlen begin_len, ftnlen end_len)
{
    /* Initialized data */

    static integer calls = 0;
    static integer stdout = 6;
    static doublereal step = 0.;
    static doublereal svincr = 0.;
    static integer svunit = 6;
    static integer check = 1;
    static doublereal done = 0.;
    static doublereal entire = 0.;
    static char finish[13] = "             ";
    static logical first = TRUE_;
    static integer ls = 1;
    static doublereal lstsec = 0.;
    static char start[55] = "                                               "
	    "        ";

    /* System generated locals */
    address a__1[5];
    integer i__1[5];
    doublereal d__1, d__2;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen), s_cat(char *,
	     char **, integer *, integer *, ftnlen);

    /* Local variables */
    doublereal tvec[6];
    extern /* Subroutine */ int zzgfdsps_(integer *, char *, char *, integer *
	    , ftnlen, ftnlen), zzcputim_(doublereal *), chkin_(char *, ftnlen)
	    , dpfmt_(doublereal *, char *, char *, ftnlen, ftnlen), stdio_(
	    char *, integer *, ftnlen);
    extern integer rtrim_(char *, ftnlen);
    extern doublereal brcktd_(doublereal *, doublereal *, doublereal *);
    doublereal fractn;
    char messge[78];
    doublereal cursec;
    char prcent[10];
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int writln_(char *, integer *, ftnlen);

/* $ Abstract */

/*     The entry points under this routine allows one to easily monitor */
/*     the status of job in progress. */

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

/*     UTILITY */
/*     REPORT */
/*     WORK */

/* $ Declarations */
/* $ Abstract */

/*     SPICE private include file intended solely for the support of */
/*     SPICE routines. Users should not include this routine in their */
/*     source code due to the volatile nature of this file. */

/*     This file contains private, global parameter declarations */
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
/*     E.D. Wright       (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 17-FEB-2009 (NJB) (EDW) */

/* -& */

/*     The set of supported coordinate systems */

/*        System          Coordinates */
/*        ----------      ----------- */
/*        Rectangular     X, Y, Z */
/*        Latitudinal     Radius, Longitude, Latitude */
/*        Spherical       Radius, Colatitude, Longitude */
/*        RA/Dec          Range, Right Ascension, Declination */
/*        Cylindrical     Radius, Longitude, Z */
/*        Geodetic        Longitude, Latitude, Altitude */
/*        Planetographic  Longitude, Latitude, Altitude */

/*     Below we declare parameters for naming coordinate systems. */
/*     User inputs naming coordinate systems must match these */
/*     when compared using EQSTR. That is, user inputs must */
/*     match after being left justified, converted to upper case, */
/*     and having all embedded blanks removed. */


/*     Below we declare names for coordinates. Again, user */
/*     inputs naming coordinates must match these when */
/*     compared using EQSTR. */


/*     Note that the RA parameter value below matches */

/*        'RIGHT ASCENSION' */

/*     when extra blanks are compressed out of the above value. */


/*     Parameters specifying types of vector definitions */
/*     used for GF coordinate searches: */

/*     All string parameter values are left justified, upper */
/*     case, with extra blanks compressed out. */

/*     POSDEF indicates the vector is defined by the */
/*     position of a target relative to an observer. */


/*     SOBDEF indicates the vector points from the center */
/*     of a target body to the sub-observer point on */
/*     that body, for a given observer and target. */


/*     SOBDEF indicates the vector points from the center */
/*     of a target body to the surface intercept point on */
/*     that body, for a given observer, ray, and target. */


/*     Number of workspace windows used by ZZGFREL: */


/*     Number of additional workspace windows used by ZZGFLONG: */


/*     Index of "existence window" used by ZZGFCSLV: */


/*     Progress report parameters: */

/*     MXBEGM, */
/*     MXENDM    are, respectively, the maximum lengths of the progress */
/*               report message prefix and suffix. */

/*     Note: the sum of these lengths, plus the length of the */
/*     "percent complete" substring, should not be long enough */
/*     to cause wrap-around on any platform's terminal window. */


/*     Total progress report message length upper bound: */


/*     End of file zzgf.inc. */

/* $ Brief_I/O */

/*     VARIABLE  I/O  Entry points */
/*     --------  ---  -------------------------------------------------- */
/*     UNIT      I-O  ZZGFWKUN, ZZGFWKMO */
/*     TOTAL     I-O  ZZGFTSWK, ZZGFWKAD, ZZGFWKMO */
/*     WAIT      I-O  ZZGFTSWK, ZZGFWKAD, ZZGFWKMO */
/*     TCHECK    I-O  ZZGFTSWK, ZZGFWKAD, ZZGFWKMO */
/*     BEGIN     I-O  ZZGFTSWK, ZZGFWKAD, ZZGFWKMO */
/*     END       I-O  ZZGFTSWK, ZZGFWKAD, ZZGFWKMO */
/*     INCR      I-O  ZZGFWKIN, ZZGFWKMO */

/* $ Detailed_Input */

/*     See the headers of the entry points. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     MXBEGM, */
/*     MXENDM, */
/*     MXMSG     are, respectively, the maximum lengths of the progress */
/*               message prefix, progress message suffix, and the */
/*               complete message. */

/* $ Exceptions */

/*     If this routine is called directly, the error SPICE(BOGUSENTRY) */
/*     is signaled. */

/*     See the entry points for descriptions of exceptions they detect. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The entry points under this routine are designed to allow one to */
/*     easily build into his/her application a monitoring facility */
/*     that reports how work on a particular task is proceeding. */

/*     There are five entry points: ZZGFTSWK, ZZGFWKIN, ZZGFWKAD, */
/*     ZZGFWKUN, and ZZGFWKMO. */

/*     The first entry point ZZGFTSWK is used to initialize the reporter. */
/*     It is used to tell the reporter "I have some work to do.  This is */
/*     how much, and this is how often I want you to report on the */
/*     progress of the task." */

/*     The second entry point ZZGFWKIN is used to tell the reporter "I've */
/*     just finished some of the task I told you about with ZZGFTSWK. */
/*     This is how much I've just done."  (As in real life, the amount */
/*     of work you've just done can be negative.)  The reporter uses */
/*     this information together with the information input in ZZGFTSWK */
/*     to decide whether and how much work to report as finished.  The */
/*     reports will be sent to the current output device. */

/*     The third entry point, ZZGFWKAD, adjusts the frequency with which */
/*     work progress is reported. */

/*     The fourth entry point ZZGFWKUN also is used for testing. It is */
/*     used to send the output to the file connected to a specified */
/*     logical unit. */

/*     The fifth entry point ZZGFWKMO is used for testing. It returns */
/*     the saved search parameters. */

/*     A more detailed description of each entry point is provided in its */
/*     associated header. */

/* $ Examples */

/*     A typical use of ZZGFRPWK might be as follows. */


/*     C */
/*     C     Compute how much work is to be done and put it in TOTAL */
/*     C */

/*           code */
/*           computing */
/*           how */
/*           much */
/*           work */
/*           to */
/*           do */
/*            . */
/*            . */
/*            . */
/*           TOTAL     = <the amount of work to do> */

/*     C */
/*     C     Tell the work reporter to report work completed every */
/*     C     3 seconds. (The third argument in ZZGFTSWK is explained */
/*     C     in the header for ZZGFTSWK.) */
/*     C */
/*           WAIT   = 3.0D0 */
/*           BEGIN  = 'Current work status: ' */
/*           END    = 'completed. ' */

/*           CALL ZZGFTSWK ( TOTAL, WAIT, 1, BEGIN, END ) */

/*           DO WHILE ( THERE_IS_MORE_WORK_TO_DO ) */

/*              code that */
/*              performs */
/*              the work to */
/*              be done */

/*              AMOUNT = amount of work done in this loop pass */

/*              CALL ZZGFWKIN ( AMOUNT ) */

/*           END DO */


/* $ Restrictions */

/*      You can use this routine to report progress on only one task at */
/*      a time.  The work reporter must be initialized using ZZGFTSWK */
/*      before calling ZZGFWKIN.  Failure to do this may lead to */
/*      unexpected results. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */
/*     L.S. Elson     (JPL) */
/*     W.L. Taber     (JPL) */
/*     I.M. Underwood (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0 05-NOV-2021 (NJB) */

/*        Changed name of argument FREQ to WAIT. Updated entry points */
/*        affected by this change. Deleted description of argument */
/*        UNIT from header of entry point ZZGFTSWK. */

/* -    SPICELIB Version 1.0.0 17-FEB-2009 (NJB) (LSE) (WLT) (IMU) */

/* -& */
/* $ Index_Entries */

/*     GF low-level progress report umbrella */

/* -& */

/*     SPICELIB Functions */


/*     Local variables */


/*     Saved variables */


/*     Initial values */

    switch(n__) {
	case 1: goto L_zzgftswk;
	case 2: goto L_zzgfwkin;
	case 3: goto L_zzgfwkad;
	case 4: goto L_zzgfwkun;
	case 5: goto L_zzgfwkmo;
	}

    chkin_("ZZGFRPWK", (ftnlen)8);
    sigerr_("SPICE(BOGUSENTRY)", (ftnlen)17);
    chkout_("ZZGFRPWK", (ftnlen)8);
    return 0;
/* $Procedure ZZGFTSWK ( Geometry finder total sum of work to be done. ) */

L_zzgftswk:
/* $ Abstract */

/*     Initialize the work progress utility. This is required prior to */
/*     use of the routine that performs the actual reporting. */

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

/*     UTILITY */
/*     REPORT */
/*     WORK */

/* $ Declarations */

/*     DOUBLE PRECISION      TOTAL */
/*     DOUBLE PRECISION      WAIT */
/*     INTEGER               TCHECK */
/*     CHARACTER*(*)         BEGIN */
/*     CHARACTER*(*)         END */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     TOTAL      I   A measure of the total amount of work to be done. */
/*     WAIT       I   Minimum wait time between reports. */
/*     TCHECK     I   How often to sample the system clock. */
/*     BEGIN      I   First part of the output message. */
/*     END        I   Last part of the output message. */

/* $ Detailed_Input */

/*     TOTAL      is a measure of the total amount of work to be done */
/*                by the routine(s) that will be using this facility. */
/*                It is expected (but not required) that TOTAL is a */
/*                positive number. */

/*     WAIT       is the minimum duration between reports in seconds. If */
/*                WAIT = 5 then a work progress report will be sent to */
/*                the output device no more frequently than every 5 */
/*                seconds.  Since writing to the output device takes */
/*                time, the smaller WAIT is, the greater the overhead */
/*                taken up by the work reporter will be. (A value of 2 */
/*                or greater should not burden your application */
/*                appreciably.) */

/*                Regardless of the value of WAIT, the number of calls */
/*                to ZZGFWKIN between reports will not be less than that */
/*                specified by the argument TCHECK. See the description */
/*                below. */

/*     TCHECK     is an integer used to the tell the reporter how often */
/*                to sample the system clock; TCHECK specifies the */
/*                number of calls to ZZGFWKIN between system clock */
/*                samples. If TCHECK = 7, then on every seventh call to */
/*                ZZGFWKIN, the system clock will be sampled to */
/*                determine if WAIT seconds have elapsed since the last */
/*                report time.  Sampling the system clock takes time. */
/*                Not a lot of time, but it does take time. If ZZGFWKIN */
/*                is being called from a loop that does not take a lot */
/*                of time for each pass, the sampling of the system */
/*                clock can become a significant overhead cost in */
/*                itself.  On the VAX the sampling of the system clock */
/*                used here takes about 37 double precision multiplies. */
/*                If thousands of multiplies take place between calls to */
/*                ZZGFWKIN, the sampling time is insignificant.  On the */
/*                other hand, if only a hundred or so multiplies occur */
/*                between calls to ZZGFWKIN, the sampling of the system */
/*                clock can become a significant fraction of your */
/*                overhead.  TCHECK allows you to tailor the work */
/*                reporter to your application. */

/*                If a non-positive value for TCHECK is entered, a value */
/*                of 1 will be used instead of the input value. */

/*      BEGIN     Is the first part of the output message that will be */
/*                constructed for shipment to the output device. This */
/*                message will have the form: */

/*                BEGIN // xx.x% // END */

/*                where xx.x  is the percentage of the job completed when */
/*                the output message is sent to the output device. */

/*      END       is the second part of the output message that will be */
/*                constructed and sent to the output device (see above). */


/* $ Detailed_Output */

/*      None. */

/* $ Parameters */

/*      None. */

/* $ Exceptions */

/*     Standard SPICE error handling. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This entry point is used to initialize parameters that will */
/*     be used by ZZGFWKIN. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     See the header for this module */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */
/*     L.S. Elson     (JPL) */
/*     W.L. Taber     (JPL) */
/*     I.M. Underwood (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0 05-NOV-2021 (NJB) */

/*        Changed name of argument FREQ to WAIT. Updated header to */
/*        clarify roles of WAIT and TCHECK. Deleted description of */
/*        UNIT from $Detailed_Input header section. */

/* -    SPICELIB Version 1.0.0 17-FEB-2009 (NJB) (LSE) (WLT) (IMU) */

/* -& */
/* $ Index_Entries */

/*     GF low-level initialize progress report */

/* -& */
    if (return_()) {
	return 0;
    }
    chkin_("ZZGFTSWK", (ftnlen)8);

/*     On the first pass, obtain the logical unit for */
/*     standard output. */

    if (first) {
	stdio_("STDOUT", &stdout, (ftnlen)6);

/*        The output unit is STDOUT unless the caller */
/*        sets it to something else. */

	svunit = stdout;
	first = FALSE_;
    }

/*     Save the inputs and set the amount of work done to 0 */

    entire = *total;
/* Computing MIN */
    d__1 = 3600., d__2 = max(0.,*wait);
    step = min(d__1,d__2);
    check = max(1,*tcheck);
    s_copy(start, begin, (ftnlen)55, begin_len);
    s_copy(finish, end, (ftnlen)13, end_len);
    done = 0.;

/*     Set the timer. */

    zzcputim_(tvec);
    lstsec = tvec[3] * 3600. + tvec[4] * 60. + tvec[5];

/*     Set the increment counter */

    calls = 0;

/*     Compose the output message. */

    ls = rtrim_(start, (ftnlen)55);
/* Writing concatenation */
    i__1[0] = ls, a__1[0] = start;
    i__1[1] = 1, a__1[1] = " ";
    i__1[2] = 7, a__1[2] = "  0.00%";
    i__1[3] = 1, a__1[3] = " ";
    i__1[4] = 13, a__1[4] = finish;
    s_cat(messge, a__1, i__1, &c__5, (ftnlen)78);

/*     Display a blank line, make sure we don't overwrite anything */
/*     at the bottom of the screen. The display the message. */

    if (svunit == stdout) {
	zzgfdsps_(&c__1, messge, "A", &c__0, (ftnlen)78, (ftnlen)1);
    } else {

/*        Write the message without special carriage control. */

	writln_(" ", &svunit, (ftnlen)1);
	writln_(" ", &svunit, (ftnlen)1);
	writln_(messge, &svunit, (ftnlen)78);
    }
    chkout_("ZZGFTSWK", (ftnlen)8);
    return 0;
/* $Procedure ZZGFWKIN ( Geometry finder work finished increment ) */

L_zzgfwkin:
/* $ Abstract */

/*     Let the work reporter know that an increment of work has just */
/*     been completed. */

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

/*     UTILITY */
/*     REPORT */
/*     WORK */

/* $ Declarations */

/*     DOUBLE PRECISION      INCR */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     INCR       I   An amount of work just completed. */

/* $ Detailed_Input */

/*     INCR       is some amount of work that has been completed since */
/*                the last call to ZZGFWKIN. */

/* $ Detailed_Output */

/*      None. */

/* $ Parameters */

/*      None. */

/* $ Exceptions */

/*     Standard SPICE error handling. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This entry point is used to report work that has been done since */
/*     initialization was performed using ZZGFTSWK or since the last */
/*     call to ZZGFWKIN.  The work reporter uses this information */
/*     together with samples of the system clock to report how much of */
/*     the total job has been completed. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     See the header for this module */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */
/*     L.S. Elson     (JPL) */
/*     W.L. Taber     (JPL) */
/*     I.M. Underwood (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0 17-FEB-2009 (NJB) (LSE) (WLT) (IMU) */

/* -& */
/* $ Index_Entries */

/*     ZZGF low-level progress report increment */

/* -& */
    if (return_()) {
	return 0;
    }
    chkin_("ZZGFWKIN", (ftnlen)8);
    svincr = *incr;
    done += *incr;
    ++calls;
    if (entire == 0.) {
	chkout_("ZZGFWKIN", (ftnlen)8);
	return 0;
    }
    if (calls >= check) {
	calls = 0;
	zzcputim_(tvec);
	cursec = tvec[3] * 3600. + tvec[4] * 60. + tvec[5];
	if ((d__1 = cursec - lstsec, abs(d__1)) >= step) {
	    lstsec = cursec;

/*           Report how much work has been done. */

	    d__1 = done / entire * 100.;
	    fractn = brcktd_(&d__1, &c_b19, &c_b20);
	    dpfmt_(&fractn, "xxx.xx", prcent, (ftnlen)6, (ftnlen)10);
	    *(unsigned char *)&prcent[6] = '%';
/* Writing concatenation */
	    i__1[0] = ls, a__1[0] = start;
	    i__1[1] = 1, a__1[1] = " ";
	    i__1[2] = 7, a__1[2] = prcent;
	    i__1[3] = 1, a__1[3] = " ";
	    i__1[4] = rtrim_(finish, (ftnlen)13), a__1[4] = finish;
	    s_cat(messge, a__1, i__1, &c__5, (ftnlen)78);
	    if (svunit == stdout) {
		zzgfdsps_(&c__0, messge, "A", &c__0, (ftnlen)78, (ftnlen)1);
	    } else {

/*              Write the message without special carriage control. */

		writln_(messge, &svunit, (ftnlen)78);
	    }
	}
    }
    chkout_("ZZGFWKIN", (ftnlen)8);
    return 0;
/* $Procedure ZZGFWKAD ( Geometry finder work reporting adjustment ) */

L_zzgfwkad:
/* $ Abstract */

/*     Adjust the frequency with which work progress is reported. */

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

/*     UTILITY */
/*     REPORT */
/*     WORK */

/* $ Declarations */

/*     DOUBLE PRECISION      WAIT */
/*     INTEGER               TCHECK */
/*     CHARACTER*(*)         BEGIN */
/*     CHARACTER*(*)         END */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     TOTAL      I   A measure of the total amount of work to be done. */
/*     WAIT       I   Minimum wait time between reports. */
/*     BEGIN      I   First part of the output message. */
/*     END        I   Last part of the output message. */

/* $ Detailed_Input */

/*     WAIT       is the minimum duration between reports in seconds. If */
/*                WAIT = 5 then a work progress report will be sent to */
/*                the output device no more frequently than every 5 */
/*                seconds.  Since writing to the output device takes */
/*                time, the smaller WAIT is, the greater the overhead */
/*                taken up by the work reporter will be. (A value of 2 */
/*                or greater should not burden your application */
/*                appreciably.) */

/*                Regardless of the value of WAIT, the number of calls */
/*                to ZZGFWKIN between reports will not be less than that */
/*                specified by the argument TCHECK. See the description */
/*                below. */

/*     TCHECK     is an integer used to the tell the reporter how often */
/*                to sample the system clock; TCHECK specifies the */
/*                number of calls to ZZGFWKIN between system clock */
/*                samples. If TCHECK = 7, then on every seventh call to */
/*                ZZGFWKIN, the system clock will be sampled to */
/*                determine if WAIT seconds have elapsed since the last */
/*                report time.  Sampling the system clock takes time. */
/*                Not a lot of time, but it does take time. If ZZGFWKIN */
/*                is being called from a loop that does not take a lot */
/*                of time for each pass, the sampling of the system */
/*                clock can become a significant overhead cost in */
/*                itself.  On the VAX the sampling of the system clock */
/*                used here takes about 37 double precision multiplies. */
/*                If thousands of multiplies take place between calls to */
/*                ZZGFWKIN, the sampling time is insignificant.  On the */
/*                other hand, if only a hundred or so multiplies occur */
/*                between calls to ZZGFWKIN, the sampling of the system */
/*                clock can become a significant fraction of your */
/*                overhead.  TCHECK allows you to tailor the work */
/*                reporter to your application. */

/*                If a non-positive value for TCHECK is entered, a value */
/*                of 1 will be used instead of the input value. */


/*     BEGIN      Is the first part of the output message that will be */
/*                constructed for shipment to the output device. This */
/*                message will have the form: */

/*                BEGIN // xx.x% // END */

/*                where xx.x  is the percentage of the job completed when */
/*                the output message is sent to the output device. */

/*     END        is the second part of the output message that will be */
/*                constructed and sent to the output device (see above). */


/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1) If TCHECK is less than 1, the value 1 is stored. */

/*     2) If WAIT is less than 0.D0, the value 0.D0 is stored. */
/*        If WAIT is greater than 3600, the value 3600 is stored. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This entry point exists to modify the reporting frequency set */
/*     up by an initial call to ZZGFTSWK.  In this way one can override */
/*     how often reporting of work increments is performed, without */
/*     causing the screen to be modified (which happens if a new */
/*     call to  ZZGFTSWK is made.) */

/*     It exists primarily as a back door to existing code */
/*     that calls ZZGFTSWK in a rigid way. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     See the header for this module. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */
/*     W.L. Taber     (JPL) */
/*     I.M. Underwood (JPL) */
/*     L.S. Elson     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0 21-FEB-2021 (NJB) */

/*        Changed name of argument FREQ to WAIT. Updated header to */
/*        clarify roles of WAIT and TCHECK. Corrected Exceptions */
/*        section's description of handling of WAIT values less than */
/*        zero. */

/* -    SPICELIB Version 1.0.0 17-FEB-2009 (NJB) (LSE) (WLT) (IMU) */

/* -& */
/* $ Index_Entries */

/*     GF low-level progress report adjust frequency */

/* -& */
/* Computing MIN */
    d__1 = 3600., d__2 = max(0.,*wait);
    step = min(d__1,d__2);
    check = max(1,*tcheck);
    s_copy(start, begin, (ftnlen)55, begin_len);
    s_copy(finish, end, (ftnlen)13, end_len);
    return 0;
/* $Procedure ZZGFWUN ( Geometry finder set work report output unit ) */

L_zzgfwkun:
/* $ Abstract */

/*     Set the output unit for the progress report. */

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

/*     UTILITY */
/*     REPORT */
/*     WORK */

/* $ Declarations */

/*     INTEGER               UNIT */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     UNIT       I   Output logical unit. */

/* $ Detailed_Input */

/*     UNIT           Logical unit of a text file open for write access. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     The file designated by UNIT should be a text file opened by the */
/*     calling application. */

/* $ Particulars */

/*     This routine can be called before ZZGFTSWK to set the output */
/*     logical unit to that of a text file. */

/*     This entry point exists to support testing of the higher-level */
/*     GF progress reporting routines */

/*        GFREPI */
/*        GFREPU */
/*        GFREPF */

/*     This routine enables TSPICE to send the output report to */
/*     a specified file. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0 17-FEB-2009 (NJB) */

/* -& */
/* $ Index_Entries */

/*     GF low-level progress report output select unit */

/* -& */

/*     On the first pass, obtain the logical unit for */
/*     standard output. */

    if (first) {
	stdio_("STDOUT", &stdout, (ftnlen)6);
	first = FALSE_;
    }
    svunit = *unit;
    return 0;
/* $Procedure ZZGFWKMO ( Geometry finder work reporting monitor ) */

L_zzgfwkmo:
/* $ Abstract */

/*     Return saved progress report parameters. */

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

/*     UTILITY */
/*     REPORT */
/*     WORK */

/* $ Declarations */

/*     INTEGER               UNIT */
/*     DOUBLE PRECISION      TOTAL */
/*     DOUBLE PRECISION      WAIT */
/*     INTEGER               TCHECK */
/*     CHARACTER*(*)         BEGIN */
/*     CHARACTER*(*)         END */
/*     DOUBLE PRECISION      INCR */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     UNIT       O   Output logical unit. */
/*     TOTAL      O   A measure of the total amount of work to be done. */
/*     WAIT       O   Minimum wait time between reports. */
/*     TCHECK     O   Number of calls between system time check. */
/*     BEGIN      O   First part of the output message. */
/*     END        O   Last part of the output message. */
/*     INCR       O   Last progress increment. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     UNIT, */
/*     TOTAL, */
/*     WAIT, */
/*     TCHECK, */
/*     BEGIN, */
/*     END, */
/*     INCR           are the most recent values of these */
/*                    variables passed in via calls to ZZGFTSWK, */
/*                    ZZGFWKIN, or ZZGFWKAD. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This entry point exists to support testing of the higher-level */
/*     GF progress reporting routines */

/*        GFREPI */
/*        GFREPU */
/*        GFREPF */

/*     This routine enables TSPICE to determine the values passed */
/*     in to entry points of this package by those routines. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0 21-FEB-2021 (NJB) */

/*        Changed name of argument FREQ to WAIT. Updated Brief_I/O */
/*        description of this argument. */

/* -    SPICELIB Version 1.0.0 17-FEB-2009 (NJB) */

/* -& */
/* $ Index_Entries */

/*     GF low-level progress report monitor */

/* -& */
    *unit = svunit;
    *total = entire;
    *wait = step;
    *tcheck = check;
    s_copy(begin, start, begin_len, (ftnlen)55);
    s_copy(end, finish, end_len, (ftnlen)13);
    *incr = svincr;
    return 0;
} /* zzgfrpwk_ */

/* Subroutine */ int zzgfrpwk_(integer *unit, doublereal *total, doublereal *
	wait, integer *tcheck, char *begin, char *end, doublereal *incr, 
	ftnlen begin_len, ftnlen end_len)
{
    return zzgfrpwk_0_(0, unit, total, wait, tcheck, begin, end, incr, 
	    begin_len, end_len);
    }

/* Subroutine */ int zzgftswk_(doublereal *total, doublereal *wait, integer *
	tcheck, char *begin, char *end, ftnlen begin_len, ftnlen end_len)
{
    return zzgfrpwk_0_(1, (integer *)0, total, wait, tcheck, begin, end, (
	    doublereal *)0, begin_len, end_len);
    }

/* Subroutine */ int zzgfwkin_(doublereal *incr)
{
    return zzgfrpwk_0_(2, (integer *)0, (doublereal *)0, (doublereal *)0, (
	    integer *)0, (char *)0, (char *)0, incr, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int zzgfwkad_(doublereal *wait, integer *tcheck, char *begin,
	 char *end, ftnlen begin_len, ftnlen end_len)
{
    return zzgfrpwk_0_(3, (integer *)0, (doublereal *)0, wait, tcheck, begin, 
	    end, (doublereal *)0, begin_len, end_len);
    }

/* Subroutine */ int zzgfwkun_(integer *unit)
{
    return zzgfrpwk_0_(4, unit, (doublereal *)0, (doublereal *)0, (integer *)
	    0, (char *)0, (char *)0, (doublereal *)0, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int zzgfwkmo_(integer *unit, doublereal *total, doublereal *
	wait, integer *tcheck, char *begin, char *end, doublereal *incr, 
	ftnlen begin_len, ftnlen end_len)
{
    return zzgfrpwk_0_(5, unit, total, wait, tcheck, begin, end, incr, 
	    begin_len, end_len);
    }

