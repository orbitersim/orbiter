/* reset.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static logical c_false = FALSE_;
static logical c_true = TRUE_;

/* $Procedure RESET ( Reset Error Status ) */
/* Subroutine */ int reset_(void)
{
    logical stat;
    extern logical accept_(logical *), seterr_(logical *);
    extern /* Subroutine */ int putlms_(char *, ftnlen), putsms_(char *, 
	    ftnlen);

/* $ Abstract */

/*     Reset the SPICELIB error status to a value of "no error." */
/*     As a result, the status routine, FAILED, will return a value */
/*     of .FALSE. */

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

/*     None. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  This routine does not detect any errors. */

/*         However, this routine is part of the SPICELIB error */
/*         handling mechanism. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Please read the "required reading" first! */

/*     The effects of this routine are: */

/*     1. The SPICELIB status is set to a value of "no error." */

/*     2. The long and short error messages are set to blank. */

/*     3. Setting of the long error message is re-enabled. */


/*     Subsequent to a call to RESET, references to the status */
/*     indicator function, FAILED, will return a value of .FALSE., */
/*     until an error is detected. */

/*     This routine should be called in cases where one wishes */
/*     to attempt to continue processing after detection of an */
/*     error, and the 'RETURN' error action is being used. When */
/*     the error response action is set to 'RETURN', routines */
/*     that have external references, or that can */
/*     detect errors, return immediately upon entry when an */
/*     error condition exists. This prevents a program from */
/*     crashing, but does not allow for a recovery attempt. */

/*     If one does wish to attempt to recover, */
/*     in general the procedure is to test for an error */
/*     condition, and if one exists, respond to the error */
/*     (by outputting diagnostic messages, for example).  Next, */
/*     a call to RESET can be made. After resetting the */
/*     error status, the normal execution thread can be resumed. */

/*     It is also appropriate to call this routine when the error */
/*     response action is 'REPORT', if one wishes to recover */
/*     from errors. */

/* $ Examples */

/*     1. In this example, we try to read a line from the file, */
/*         SPUD.DAT, using the toolkit routine, RDTEXT. */
/*         When FAILED indicates an error, we grab the short */
/*         error message and its explanation, using GETMSG (see), */
/*         log the messages using our user-defined routine, */
/*         USER_LOG (NOT a SPICELIB routine), reset the */
/*         status, and keep going. */

/*     C */
/*     C      We read a line from SPUD.DAT: */
/*     C */

/*            CALL RDTEXT ( 'SPUD.DAT', LINE, EOF ) */

/*            IF ( FAILED() ) THEN */
/*     C */
/*     C         Oops! an error occurred during the read. */
/*     C         Recover the short error message and its */
/*     C         explanation, reset the error status, */
/*     C         log the messages, and continue... */
/*     C */

/*               CALL GETMSG   ( 'SHORT'    ,    SMSG ) */
/*               CALL GETMSG   ( 'EXPLAIN'  ,    EXPL ) */

/*               CALL USER_LOG (  SMSG ) */
/*               CALL USER_LOG (  EXPL ) */

/*               CALL RESET */

/*            END IF */

/* $ Restrictions */

/*     1)  It can be dangerous to call this routine without */
/*         RESPONDING to the error condition first; by calling */
/*         RESET, you are wiping out the SPICELIB's knowledge of */
/*         the error. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.0, 26-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.1.0, 14-MAR-1996 (KRG) */

/*        Removed the call to FREEZE at the end of this subroutine. */
/*        The call had no effect other than to copy the current */
/*        stack in the trace package from the active stack into the */
/*        frozen stack. The frozen stack could NEVER be accessed */
/*        after this copying action; the only time the frozen stack */
/*        could be accessed is when a program is executing in 'RETURN' */
/*        mode and FAILED is .TRUE., i.e. after an error has been */
/*        signaled, causing the active stack at the time of the */
/*        error to be copied to the frozen stack. So this copying */
/*        of the active stack on a RESET of the error handling */
/*        accomplishes nothing. */

/*        References to the setting of the frozen traceback were */
/*        removed from the header as well. */

/*        A missing Fortran RETURN statement was also added before the */
/*        END statement */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (NJB) */

/* -& */
/* $ Index_Entries */

/*     reset error status */

/* -& */

/*     SPICELIB functions */


/*     Local Variables: */


/*     Executable Code: */

/*     This odd-looking function reference resets the error */
/*     status to indicate "no error": */

    stat = seterr_(&c_false);

/*     Wipe out the short and long error messages: */

    putsms_(" ", (ftnlen)1);
    putlms_(" ", (ftnlen)1);

/*     Allow long error message to be updated: */

    stat = accept_(&c_true);
    return 0;
} /* reset_ */

