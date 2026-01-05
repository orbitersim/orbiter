/* seterr.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure SETERR ( Set Error Status ) */
logical seterr_0_(int n__, logical *status)
{
    /* Initialized data */

    static logical svstat = FALSE_;

    /* System generated locals */
    logical ret_val;

/* $ Abstract */

/*     Set the SPICELIB error status.  DO NOT CALL THIS ROUTINE. */

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
/*     STATUS     I   Status indicator. */

/*     The function takes an UNSPECIFIED (and meaningless) value */
/*     on exit. */

/* $ Detailed_Input */

/*     STATUS   is a flag that provides the new status. When .TRUE., it */
/*              means that an error condition exists. */

/* $ Detailed_Output */

/*     None. */

/*     This purpose of this routine is to set status; the */
/*     function takes an UNSPECIFIED value on exit. The */
/*     assigned value does not have any meaning. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     DO NOT CALL THIS ROUTINE. */

/*     This is a data structure access routine for the */
/*     SPICELIB status. This routine should be used for no */
/*     other purpose; in particular, it should not be used */
/*     to signal errors. Use SIGERR or FAILED for that. */

/*     This routine assigns a value to SETERR on exit. */
/*     However, the value is not meaningful. */

/* $ Examples */

/*     None.  DON'T CALL THIS ROUTINE. */

/*     No examples. If you don't know EXACTLY what a */
/*     ``data structure access routine'' is, don't call */
/*     this routine. If you do know, you don't need an */
/*     example. */

/* $ Restrictions */

/*     1)  DON'T CALL THIS ROUTINE. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR) (NJB) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (NJB) */

/* -& */
/* $ Index_Entries */

/*     None. */

/* -& */
/* $ Revisions */

/* -     Beta Version 1.0.1, 08-FEB-1989 (NJB) */

/*         Warnings added to discourage use of this routine in */
/*         non-error-handling code. */

/* -& */

/*     Local Variables: */


/*     The SPICELIB status: */


/*     Declaration of the entry point, FAILED: */


/*     Initial values: */

    switch(n__) {
	case 1: goto L_failed;
	}


/*     Executable Code: */

    svstat = *status;

/*     Give SETERR a value; the value does not have any */
/*     meaning, but it appears standard FORTRAN requires this. */

    ret_val = TRUE_;
    return ret_val;
/* $Procedure FAILED ( Error Status Indicator ) */

L_failed:
/* $ Abstract */

/*     Return .TRUE. if an error condition has been signaled via SIGERR. */
/*     FAILED is the SPICELIB status indicator. */

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

/*     IMPLICIT NONE */

/* $ Brief_I/O */

/*     The function takes the value .TRUE. if an error condition */
/*     was detected; it is .FALSE. otherwise. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     Please read the required reading file before reading this! */

/*     The value taken by FAILED indicates status. */

/*     The status value applies to the SPICELIB routines, */
/*     and to any other routines which call the status-setting */
/*     routine, SIGERR. */

/*     When FAILED has the value, .TRUE., an error condition */
/*     exists.   .FALSE. means "no error." */

/*     More specifically, when FAILED has the value .TRUE., */
/*     some routine has indicated an error by calling the */
/*     SPICELIB routine, SIGERR. All SPICELIB routines */
/*     which can detect errors do this. Non-SPICELIB */
/*     routines may also reference SIGERR if desired. */

/*     When FAILED has the value .FALSE., either no routine */
/*     has yet signaled an error via SIGERR, or the status */
/*     has been reset using, what else, RESET. */

/*     FAILED is initialized to have the value, .FALSE. */
/*     This indicates a  "no error" status. */

/*     See "particulars" below for (slightly) more information. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  However, this routine is part of the SPICELIB error */
/*         handling mechanism. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     See the required reading file for details of error */
/*     processing. However, here are some notes: */

/*     When any SPICELIB routine detects an error, the */
/*     status is set to indicate an error condition via */
/*     a call to SIGERR. After SIGERR */
/*     returns, further calls to FAILED will return the */
/*     value, .TRUE., indicating an error condition. */

/*     Non-SPICELIB routines may also call SIGERR to indicate */
/*     an error condition; FAILED will reflect such calls */
/*     as well. */

/*     It is possible to re-set the error status to indicate */
/*     "no error" using the SPICELIB routine, RESET (see). */

/*     The effect on FAILED of resetting the status is */
/*     that FAILED will again return the value .FALSE., */
/*     indicating "no error." */

/*     One of the main virtues of the SPICELIB error */
/*     handling mechanism is that you don't HAVE to test the */
/*     error status after every call to a SPICELIB routine. */
/*     If you set the error handling mode to 'RETURN', using */
/*     the routine, ERRACT, SPICELIB routines won't crash */
/*     when an error occurs; following the detection of the */
/*     error, each routine will return immediately upon entry. */
/*     Therefore, you call several SPICELIB routines in a */
/*     row, and just test status at the end of the sequence */
/*     of calls, if you wish. See "examples" below. */

/* $ Examples */

/*     1. Here's an example of a simple call to RDTEXT, followed */
/*         by a test of the status. */


/*     C */
/*     C     We read a line of text from file SPUD.DAT: */
/*     C */

/*           CALL RDTEXT ( 'SPUD.DAT', LINE, EOF ) */

/*           IF ( FAILED() ) THEN */

/*     C        An error occurred during the read. */

/*              [respond to error here] */

/*           END IF */


/*     2.    Here's an example in which we don't want to */
/*           put the error test inside our loop.  We just */
/*           test the error status after the loop terminates. */
/*           We can do this because we (that is, you, the user) */
/*           have made the call, */

/*                  CALL ERRACT ( 'RETURN' ) */

/*           prior to execution of the following code. If an */
/*           error does occur, the remaining calls to RDTEXT */
/*           will have no effect. Here's the example: */

/*     C */
/*     C     We read the first 5000 lines of a file, or until */
/*     C     EOF is reached, whichever comes first: */
/*     C */
/*     C     Note:  the "DO WHILE" construct is available in */
/*     C     VAX FORTRAN. */
/*     C */

/*           LCOUNT = 0 */
/*           DO WHILE (  ( .NOT. EOF ) .AND. ( LCOUNT .LE. 5000 )  ) */

/*              CALL RDTEXT ( 'SPUD.DAT', LINE(LCOUNT), EOF ) */

/*              LCOUNT = LCOUNT + 1 */

/*           END DO */

/*           IF ( FAILED() ) THEN */
/*     C */
/*     C        An error occurred during the read */
/*     C */
/*              [respond to error here] */

/*           END IF */

/* $ Restrictions */

/*     1)  This routine automatically detects errors occurring in */
/*         the SPICELIB code. To make this routine work */
/*         for your own routines, your routines must call SIGERR */
/*         to report errors. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR) (NJB) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (NJB) */

/* -& */
/* $ Index_Entries */

/*     error status indicator */

/* -& */
/* $ Revisions */

/* -     Beta Version 1.1.0, 18-DEC-1989 (HAN) */

/*         Empty parentheses added to the ENTRY statement in order to */
/*         comply with the ANSI Fortran 77 Standard. */

/* -& */

/*     Executable Code: */


/*     Grab saved status value: */

    ret_val = svstat;
    return ret_val;
} /* seterr_ */

logical seterr_(logical *status)
{
    return seterr_0_(0, status);
    }

logical failed_(void)
{
    return seterr_0_(1, (logical *)0);
    }

