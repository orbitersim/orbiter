/* sigerr.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static logical c_true = TRUE_;
static logical c_false = FALSE_;

/* $Procedure SIGERR ( Signal Error Condition ) */
/* Subroutine */ int sigerr_(char *msg, ftnlen msg_len)
{
    /* Initialized data */

    static char defmsg[40] = "SHORT, EXPLAIN, LONG, TRACEBACK, DEFAULT";
    static char errmsg[40] = "SHORT, EXPLAIN, LONG, TRACEBACK         ";

    static logical stat;
    extern logical failed_(void), accept_(logical *);
    extern /* Subroutine */ int getact_(integer *);
    static integer action;
    extern /* Subroutine */ int byebye_(char *, ftnlen), freeze_(void);
    extern logical seterr_(logical *);
    extern /* Subroutine */ int outmsg_(char *, ftnlen), putsms_(char *, 
	    ftnlen);

/* $ Abstract */

/*     Inform the SPICELIB error processing mechanism that an error has */
/*     occurred, and specify the type of error. */

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
/*     MSG        I   A short error message. */

/* $ Detailed_Input */

/*     MSG      is a ``short'' error message. */

/*              MSG indicates the type of error that has occurred. */

/*              The exact format that MSG must follow is */
/*              described in the required reading file, error.req. */
/*              Only the first 25 characters of MSG will be stored; */
/*              additional characters will be truncated. */

/*              Generally, MSG will be stored internally by the SPICELIB */
/*              error handling mechanism. The only exception */
/*              is the case in which the user has commanded the error */
/*              handling mechanism to ``ignore'' the error indicated by */
/*              MSG. */

/*              As a default, MSG will be output to the screen. */
/*              See the required reading file for a discussion of how */
/*              to customize SPICELIB error handling behavior, and */
/*              in particular, the disposition of MSG. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  This routine does not detect any errors. */

/*         However, this routine is part of the interface to the */
/*         SPICELIB error handling mechanism. For this reason, */
/*         this routine does not participate in the trace scheme, */
/*         even though it has external references. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     First of all, please read the ``required reading'' file. */
/*     The information below will make a lot more sense if you do. */

/*     This is the routine used by SPICELIB to signal the detection */
/*     of errors. */

/*     Making a call to SIGERR is the way to inform the error */
/*     handling mechanism that an error has occurred. */

/*     Specifically, the effects of this routine are: */

/*     1. If responding to the error indicated by MSG has */
/*         not been disabled: */

/*         a. MSG will be stored internally. As a result, */
/*            The SPICELIB routine, GETMSG, will be able to */
/*            retrieve MSG, until MSG has been ``erased'' */
/*            by a call to RESET, or overwritten by another */
/*            call to SIGERR. */

/*         b. An indication of an ``error condition'' will */
/*            be set internally. The SPICELIB logical */
/*            function, FAILED, will take the value, .TRUE., */
/*            as a result, until the error condition is */
/*            negated by a call to RESET. */

/*         c. All of the error messages that have been selected */
/*            for automatic output via ERRPRT will be output. */
/*            The set of messages is some subset of { short message, */
/*            long message, explanation of short message, */
/*            traceback, and default message }. */

/*         d. If the error response mode is not 'RETURN', */
/*            Setting of the long error message is enabled. */
/*            You can't re-set the long error message, once */
/*            it has been set, without first signaling an error. */

/*         e. In 'RETURN' mode, further signaling of error */
/*            messages, and setting of the long message, are disabled. */
/*            (These capabilities can be re-enabled by calling RESET). */


/*     2. If the error handling mechanism has been commanded to */
/*         ``ignore'' the error indicated by MSG, the call to SIGERR */
/*         has no effect. */

/*     If you wish to set the long error message, call */
/*     SETMSG BEFORE calling SIGERR. */

/* $ Examples */

/*     The numerical results shown for these examples may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Create a user-defined error message, including both the */
/*        short and long messages, providing the value of an integer */
/*        and a double precision variables within the long message, */
/*        and signal the error. */


/*        Example code begins here. */


/*              PROGRAM SIGERR_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Set long error message, with two different MARKER */
/*        C     strings where the value of the variables will go. */
/*        C     Our markers are '#' and 'XX'. */
/*        C */
/*              CALL SETMSG ( 'LONG MESSAGE. Invalid operation value. ' */
/*             .         //   '  The value was #.  Left endpoint ' */
/*             .         //   'exceeded right endpoint.  The left ' */
/*             .         //   'endpoint was:  XX.'                     ) */

/*        C */
/*        C     Insert the integer number where the # is now. */
/*        C */
/*              CALL ERRINT ( '#',  5  ) */

/*        C */
/*        C     Insert a double precision number where the XX is now. */
/*        C */
/*              CALL ERRDP  ( 'XX', 910.26111991D0 ) */

/*        C */
/*        C     Signal the error. */
/*        C */
/*              CALL SIGERR ( 'SPICE(USERDEFINED)' ) */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        ============================================================*** */

/*        Toolkit version: N0066 */

/*        SPICE(USERDEFINED) -- */

/*        LONG MESSAGE. Invalid operation value. The value was 5. Left*** */
/*        exceeded right endpoint. The left endpoint was: 9.1026111991*** */

/*        Oh, by the way:  The SPICELIB error handling actions are USER- */
/*        TAILORABLE.  You can choose whether the Toolkit aborts or co*** */
/*        when errors occur, which error messages to output, and where*** */
/*        the output.  Please read the ERROR "Required Reading" file, *** */
/*        the routines ERRACT, ERRDEV, and ERRPRT. */

/*        ============================================================*** */


/*        Warning: incomplete output. 7 lines extended past the right */
/*        margin of the header and have been truncated. These lines are */
/*        marked by "***" at the end of each line. */


/*        Note that the execution of this program produces the error */
/*        SPICE(USERDEFINED), which follows the NAIF standard as */
/*        described in the ERROR required reading. */


/*     2) Create a user-defined error message, including only the */
/*        short messages, and signal the error. */


/*        Example code begins here. */


/*              PROGRAM SIGERR_EX2 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Signal the error; the short message is given by */
/*        C     SIGERR input argument. */
/*        C */
/*              CALL SIGERR ( 'SPICE(USERDEFINED)' ) */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        ============================================================*** */

/*        Toolkit version: N0066 */

/*        SPICE(USERDEFINED) -- */

/*          Oh, by the way:  The SPICELIB error handling actions are U*** */
/*        TAILORABLE.  You can choose whether the Toolkit aborts or co*** */
/*        when errors occur, which error messages to output, and where*** */
/*        the output.  Please read the ERROR "Required Reading" file, *** */
/*        the routines ERRACT, ERRDEV, and ERRPRT. */

/*        ============================================================*** */


/*        Warning: incomplete output. 6 lines extended past the right */
/*        margin of the header and have been truncated. These lines are */
/*        marked by "***" at the end of each line. */


/*        Note that the execution of this program produces the same */
/*        SPICE(USERDEFINED) error as in Example #1, but in this case, */
/*        only the short message is presented. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.2.0, 13-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Removed */
/*        unnecessary $Revisions section. */

/*        Added complete code examples. */

/* -    SPICELIB Version 2.1.1, 18-APR-2014 (BVS) */

/*        Minor header edits. */

/* -    SPICELIB Version 2.1.0, 26-JUL-1996 (KRG) */

/*        The STOP statement in this subroutine has been replaced */
/*        with a call to the subroutine BYEBYE which passes a failure */
/*        status to the operating system or command shell/environment */
/*        on all platforms which support this capability. */

/* -    SPICELIB Version 2.0.0, 22-APR-1996 (KRG) */

/*        This subroutine has been modified in an attempt to improve */
/*        the general performance of the SPICELIB error handling */
/*        mechanism. The specific modification has been to change the */
/*        type of the error action from a short character string to an */
/*        integer. This change is backwardly incompatible because the */
/*        type has changed. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (NJB) */

/* -& */
/* $ Index_Entries */

/*     signal error condition */

/* -& */

/*     SPICELIB functions: */


/*     Local Parameters */

/*     Define mnemonics for the integer action codes used by the error */
/*     handling. See ERRACT for the character string equivalents used. */


/*     Length for output messages default settings. */


/*     Local Variables: */


/*     Initial Values */

/*     Define the default error message strings for OUTMSG. */


/*     We must first check whether the error indicated by */
/*     MSG is one we're supposed to ignore... */

/*     There are two cases in which we do not want to respond */
/*     to the signaled error. */

/*     1.  When the error action is 'IGNORE'.  The user has */
/*         commanded that all messages be ignored. */

/*     2.  When the error action is 'RETURN', and an error */
/*         condition already exists.  We wish to preserve the */
/*         error data from the FIRST error until the user/ */
/*         user's program has reset the error status via */
/*         a call to RESET. */

    getact_(&action);
    if (action != 4) {
	if (action != 3 || ! failed_()) {

/*           This one's for real.  Indicate an error condition, and */
/*           store the short error message. */

/*           Note:  the following strange -- looking function */
/*           reference sets the toolkit error status.  STAT */
/*           doesn't have any meaning. */

	    stat = seterr_(&c_true);
	    putsms_(msg, msg_len);

/*           Create a frozen copy of the traceback: */

	    freeze_();

/*           Now we output the error data that are available at this */
/*           time, and whose output has been enabled.  The choice of */
/*           data is any combination of the following: */

/*              1. The short error message */
/*              2. The explanation of the short error message */
/*              3. The traceback */
/*              4. The long error message */
/*              5. The default message */

/*           Note that OUTMSG outputs only those messages which have */
/*           been SELECTED for output, via a call to ERRPRT, except */
/*           if the error action is DEFAULT.  In that case, the */
/*           default message selection applies. */

	    if (action != 5) {
		outmsg_(errmsg, (ftnlen)40);
	    } else {
		outmsg_(defmsg, (ftnlen)40);
	    }
	    if (action == 3) {

/*              Don't accept new long error messages or updates */
/*              to current long error message: */
/*              (STAT has no meaning). */

		stat = accept_(&c_false);
	    } else {
		stat = accept_(&c_true);
	    }
	} else {
	    stat = accept_(&c_false);
	}
    }

/*     We could be in ABORT or DEFAULT mode. */

    if (action == 5 || action == 1) {
	byebye_("FAILURE", (ftnlen)7);
    }

/*     That's all, folks! */

    return 0;
} /* sigerr_ */

