/* erract.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__5 = 5;
static integer c__2 = 2;

/* $Procedure ERRACT  ( Get/Set Default Error Action ) */
/* Subroutine */ int erract_(char *op, char *action, ftnlen op_len, ftnlen 
	action_len)
{
    /* Initialized data */

    static char actns[7*5] = "ABORT  " "REPORT " "RETURN " "IGNORE " "DEFAULT"
	    ;

    /* System generated locals */
    address a__1[2];
    integer i__1, i__2[2];
    char ch__1[73], ch__2[65];

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen), s_rnge(char *, integer, 
	    char *, integer);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen), s_cat(char *,
	     char **, integer *, integer *, ftnlen);

    /* Local variables */
    integer iact;
    extern /* Subroutine */ int chkin_(char *, ftnlen), ucase_(char *, char *,
	     ftnlen, ftnlen);
    char locop[3];
    extern /* Subroutine */ int ljust_(char *, char *, ftnlen, ftnlen);
    char locact[7];
    extern /* Subroutine */ int getact_(integer *);
    extern integer isrchc_(char *, integer *, char *, ftnlen, ftnlen);
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), putact_(integer *), setmsg_(char *, ftnlen);

/* $ Abstract */

/*     Retrieve or set the default error action. */

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

/*     ERROR */

/* $ Keywords */

/*     ERROR */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     OP         I   Operation -- 'GET' or 'SET' */
/*     ACTION    I-O  Error response action */

/* $ Detailed_Input */

/*     OP       indicates the operation -- 'GET' or 'SET'.  'GET' means, */
/*              "Set ACTION to the current value of the error response */
/*              action." */
/*              'SET' means, "update the error response action to the */
/*              value indicated by ACTION." */

/*              OP may be in mixed case; for example, */

/*                     CALL ERRACT ( 'gEt', ACTION ) */

/*              will work. */

/*     ACTION   when OP is 'SET', ACTION is an input argument.  It */
/*              takes the values,  'ABORT',  'IGNORE', */
/*              'REPORT', 'RETURN', and 'DEFAULT'. */

/*              Please read the "required reading" file if you */
/*              haven't already done so! */

/*              Briefly, the meanings of the error response */
/*              choices are as follows: */

/*              1.  'ABORT'  --  When an error is detected by a */
/*                               SPICELIB routine, or when */
/*                               ANY routine signals detection */
/*                  of an error via a call to SIGERR, the */
/*                  toolkit will output any error messages that */
/*                  it has been enabled to output (see ERRPRT */
/*                  and ERRDEV also ), and then execute a */
/*                  FORTRAN STOP statement. */

/*              2.  'REPORT' --  In this mode, the toolkit does */
/*                               NOT abort when errors are detected. */
/*                               When SIGERR is called to report */
/*                  an error, all error messages that the toolkit */
/*                  is enabled to output will be sent to the */
/*                  designated error output device. Similarly, */
/*                  a call to SETMSG will result in the long */
/*                  error message being output, if the toolkit */
/*                  is enabled to output it. */

/*              3.  'RETURN' --  In this mode, the toolkit also */
/*                               does NOT abort when errors are */
/*                               detected. Instead, error messages */
/*                  are output if the toolkit is enabled to do */
/*                  so, and subsequently, ALL TOOLKIT ROUTINES */
/*                  RETURN IMMEDIATELY UPON ENTRY until the */
/*                  error status is reset via a call to RESET. */
/*                  (No, RESET itself doesn't return on entry). */
/*                  Resetting the error status will cause the */
/*                  toolkit routines to resume their normal */
/*                  execution threads. */

/*              4.  'IGNORE' --  The toolkit will not take any */
/*                               action in response to errors; */
/*                               calls to SIGERR will have no */
/*                               effect. */

/*              5.  'DEFAULT' -- This mode is the same as 'ABORT', */
/*                               except that an additional error */
/*                               message is output.  The additional */
/*                               message informs the user that the */
/*                               error response action can be */
/*                               modified, and refers to documentation */
/*                               of the error handling feature. */

/*              ACTION may be in mixed case; for example, */

/*                         CALL ERRACT ( 'SET', 'igNORe' ) */

/*              will work. */

/* $ Detailed_Output */

/*     ACTION   when OP is 'GET', ACTION is the current error response */
/*              action. Possible values are:  'ABORT', 'REPORT', */
/*              'RETURN', and 'IGNORE'. See "Detailed Input" */
/*              for descriptions of the meanings of these values. */

/*              ACTION is not an output unless OP is 'GET'. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If an invalid value of the operation OP is supplied, the error */
/*         SPICE(INVALIDOPERATION) is signaled. */

/*     2)  If OP is 'SET' and the input argument ACTION does not indicate */
/*         a valid error handling action, the error SPICE(INVALIDACTION) */
/*         is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine is part of the SPICELIB error handling mechanism. */

/*     First of all, please read the ``required reading'' file. */
/*     The information below will make a lot more sense if you do. */

/*     Here is a brief discussion of how to use this routine. */

/*     If you are a user, you will probably be interested */
/*     in only the 'SET' operation (as far as this routine is */
/*     concerned, ok?).  As indicated in the "detailed */
/*     input" section above, the choices for ACTION are */
/*     'ABORT', 'REPORT', 'RETURN', 'IGNORE', and 'DEFAULT'.  These */
/*     choices control the way the toolkit behaves when an */
/*     error is detected.  The toolkit thinks an error has */
/*     been detected when SIGERR is called. */

/*     1.  'ABORT'   In this mode, the toolkit sends error messages */
/*         to the error output device and then stops. */
/*         This is the default mode. It is probably */
/*         the one to choose for running non-interactive programs. */
/*         You may also wish to use this for programs which */
/*         have many bugs, or in other cases where continued */
/*         operation following detection of an error isn't useful. */

/*     2.  'REPORT'  In this mode, the toolkit sends error messages */
/*         to the error output device and keeps going. This mode */
/*         may be useful if you are debugging a large program, */
/*         since you can get more information from a single test run. */
/*         You will probably want to use ERRDEV to indicate a file */
/*         where your error messages should be sent. */

/*     3.  'RETURN'  In this mode, the toolkit also sends error messages */
/*          to the error output device and "keeps going".  But */
/*          instead of following their normal execution threads, */
/*          the toolkit routines will simply return immediately upon */
/*          entry, once an error has been detected. */
/*          The availability of this feature makes it safe to call */
/*          multiple toolkit routines without checking the error */
/*          status after each one returns; if one routine detects */
/*          an error, subsequent calls to toolkit routines will have */
/*          no effect; therefore, no crash will occur. The error */
/*          messages set by the routine which detected the error */
/*          will remain available for retrieval by GETMSG. */

/*     4.   'IGNORE'  This mode can be dangerous!  It is best */
/*          used when running a program whose behavior you */
/*          understand well, in cases where you wish to suppress */
/*          annoying messages.  BUT, if an unexpected error */
/*          occurs, you won't hear about it from anyone, except */
/*          possibly your run-time system. */

/*     5.  'DEFAULT'  As the name suggests, this is the default */
/*          error handling mode. The error handling mechanism */
/*          starts out in this mode when a program using the */
/*          toolkit is run, and the mode remains 'DEFAULT' until */
/*          it is changed via a call to this routine. */
/*          This mode is the same as 'ABORT', */
/*          except that an additional error message is output. */
/*          The additional message informs the user that the */
/*          error response action can be modified, and refers */
/*          to documentation of the error handling feature. */


/*     NOTE: */

/*         By default, error messages are printed to the screen */
/*         when errors are detected.  You may want to send them */
/*         to a different output device, or choose a subset to */
/*         output.  Use the routines ERRDEV and ERRPRT to choose */
/*         the output device and select the messages to output, */
/*         respectively. */

/*         You can also suppress the automatic output of messages */
/*         and retrieve them directly in your own program.  GETMSG */
/*         can be used for this.  To make sure that the messages */
/*         retrieved correspond to the FIRST error that occurred, */
/*         use 'RETURN' mode. In 'REPORT' mode, new messages */
/*         overwrite old ones in the SPICELIB message storage */
/*         area, so GETMSG will get the messages from the LATEST */
/*         error that occurred. */

/* $ Examples */

/*     1. Setting up 'ABORT' mode: */


/*         C */
/*         C      We wish to have our program abort if an error */
/*         C      is detected. But instead of having the error */
/*         C      messages printed on the screen, we want them */
/*         C      to be written to the file, ERROR_LOG.DAT */
/*         C      (This is valid VAX/VMS file name; syntax */
/*         C      on your system may be different ). */
/*         C */
/*         C      We want to see all of the messages, so we */
/*         C      call ERRPRT, using the 'ALL' option. */
/*         C */
/*         C      Finally, we call ERRACT to set the action to 'ABORT': */
/*         C */

/*                CALL ERRDEV ( 'SET', 'ERROR_LOG.DAT' ) */

/*                CALL ERRPRT ( 'SET',  'ALL'  ) */

/*                CALL ERRACT ( 'SET', 'ABORT' ) */



/*     2. Setting up 'REPORT' mode: */

/*         C */
/*         C      This is the same thing as before, except */
/*         C      that the argument supplied to ERRACT */
/*         C      is different. */
/*         C */

/*                CALL ERRDEV ( 'SET', 'ERROR_LOG.DAT' ) */

/*                CALL ERRPRT ( 'SET',   'ALL'  ) */

/*                CALL ERRACT ( 'SET', 'REPORT' ) */


/*     3. Setting up 'RETURN' mode: This is the same */
/*         as example #2, except that the ERRACT call becomes: */

/*                CALL ERRACT ( 'SET', 'RETURN' ) */



/*     4. Setting up 'IGNORE' mode: */

/*         C      In this case, we aren't going to have */
/*         C      ANY error messages (unless the call */
/*         C      to ERRACT itself fails), so we don't */
/*         C      really need to call ERRPRT and ERRDEV. */
/*         C      (If the call to ERRACT DOES fail, which */
/*         C      it can do only if we misspell "IGNORE," */
/*         C      the resulting error messages will go to */
/*         C      the screen). */


/*                CALL ERRACT ( 'SET', 'IGNORE' ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.1.0, 12-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. $Exceptions */
/*        section has been completely updated to provide only the list */
/*        of exceptions. Additional information provided there has been */
/*        moved to $Particulars. */

/* -    SPICELIB Version 2.0.0, 22-APR-1996 (KRG) */

/*        This subroutine has been modified in an attempt to improve */
/*        the general performance of the SPICELIB error handling */
/*        mechanism. The specific modification has been to change the */
/*        type of the error action passed to PUTACT from a short */
/*        character string to an integer. This change is backwardly */
/*        incompatible because the type of the input argument has */
/*        changed. This should pose no difficulties because PUTACT is a */
/*        private subroutine used by the error handling system, and */
/*        hence isolated from direct use. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (NJB) */

/* -& */
/* $ Index_Entries */

/*     get/set default error action */

/* -& */
/* $ Revisions */

/* -     SPICELIB Version 2.0.0, 22-APR-1996 (KRG) */

/*         This subroutine has been modified in an attempt to improve */
/*         the general performance of the SPICELIB error handling */
/*         mechanism. The specific modification has been to change the */
/*         type of the error action passed to PUTACT from a short */
/*         character string to an integer. This change is backwardly */
/*         incompatible because the type of the input argument has */
/*         changed. This should pose no difficulties because PUTACT is a */
/*         private subroutine used by the error handling system, and */
/*         hence isolated from direct use. */

/* -     SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*         Comment section for permuted index source lines was added */
/*         following the header. */

/* -     Beta Version 1.1.0, 28-FEB-1989 (NJB) */

/*         Trace participation added. This routine now checks in */
/*         and checks out. However, it does not test RETURN, */
/*         because it should be able to execute in RETURN mode when */
/*         an error condition exists. */

/* -& */

/*     SPICELIB Functions */


/*     Local Parameters */

/*     Define the length of an option. */


/*     Define the maximum length of an action. */


/*     Define the number of actions */


/*     Local Variables */


/*     Saved Variables */


/*     Initial Values: */


/*     Executable Code: */

    chkin_("ERRACT", (ftnlen)6);

/*     We convert the input values to upper case, as needed. Note: we */
/*     only check the first character of the input variable OP, as that */
/*     is sufficient to distinguish 'GET' from 'SET' */

    ljust_(op, locop, op_len, (ftnlen)3);
    ucase_(locop, locop, (ftnlen)3, (ftnlen)3);
    if (s_cmp(locop, "GET", (ftnlen)3, (ftnlen)3) == 0) {
	getact_(&iact);
	s_copy(action, actns + ((i__1 = iact - 1) < 5 && 0 <= i__1 ? i__1 : 
		s_rnge("actns", i__1, "erract_", (ftnlen)447)) * 7, 
		action_len, (ftnlen)7);
    } else if (s_cmp(locop, "SET", (ftnlen)3, (ftnlen)3) == 0) {
	ljust_(action, locact, action_len, (ftnlen)7);
	ucase_(locact, locact, (ftnlen)7, (ftnlen)7);
	iact = isrchc_(locact, &c__5, actns, (ftnlen)7, (ftnlen)7);
	if (iact > 0) {
	    putact_(&iact);
	} else {

/*           We have an invalid value of ACTION */

	    s_copy(locact, action, (ftnlen)7, action_len);
/* Writing concatenation */
	    i__2[0] = 66, a__1[0] = "ERRACT: An invalid value of ACTION was "
		    "supplied.  The value was:  ";
	    i__2[1] = 7, a__1[1] = locact;
	    s_cat(ch__1, a__1, i__2, &c__2, (ftnlen)73);
	    setmsg_(ch__1, (ftnlen)73);
	    sigerr_("SPICE(INVALIDACTION)", (ftnlen)20);
	}

/*        We've set the error action, or signaled an error. */

    } else {

/*        We have an invalid value of OP */

	s_copy(locop, op, (ftnlen)3, op_len);
/* Writing concatenation */
	i__2[0] = 62, a__1[0] = "ERRACT: An invalid value of OP was supplied"
		".  The value was:  ";
	i__2[1] = 3, a__1[1] = locop;
	s_cat(ch__2, a__1, i__2, &c__2, (ftnlen)65);
	setmsg_(ch__2, (ftnlen)65);
	sigerr_("SPICE(INVALIDOPERATION)", (ftnlen)23);
    }

/*     We've performed the requested operation, or signaled an */
/*     error. */

    chkout_("ERRACT", (ftnlen)6);
    return 0;
} /* erract_ */

