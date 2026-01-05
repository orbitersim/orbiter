/* return.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure RETURN ( Immediate Return Indicator ) */
logical return_(void)
{
    /* System generated locals */
    logical ret_val;

    /* Local variables */
    extern logical failed_(void);
    extern /* Subroutine */ int getact_(integer *);
    integer action;

/* $ Abstract */

/*     Return .TRUE. if SPICELIB routines should return immediately upon */
/*     entry. */

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

/*      None. */

/* $ Brief_I/O */

/*     The function returns the value .TRUE. if and only if SPICELIB */
/*     routines should return immediately upon entry. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     The function returns the value .TRUE. if and only if SPICELIB */
/*     routines should return immediately upon entry. The criterion */
/*     for this is that the error response action is set to */
/*     'RETURN', and an error condition exists. */

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

/*     This routine can be referenced in non-toolkit code; in */
/*     fact, its use is encouraged. Its purpose is to signal */
/*     to the routine calling it that the caller should */
/*     return immediately. The reference to RETURN should */
/*     be the first executable line of the calling program. */

/*     In 'RETURN' mode, SPICELIB routines */
/*     that have external references, or that can */
/*     detect errors, return immediately upon entry when an */
/*     error condition exists. They use RETURN to determine */
/*     when these conditions are met. Non--toolkit routines */
/*     can do the same. */

/*     Additionally, when an error is signaled in 'RETURN' mode, */
/*     no further errors can be signaled until the error condition */
/*     is reset by a call to RESET. Calls to SIGERR simply have */
/*     no effect. Therefore, the error messages set in response */
/*     to the FIRST error that was detected will be saved until */
/*     RESET is called. These messages can be retrieved by */
/*     calls to GETMSG. */

/*     There are a number of advantages to using this mechanism. */
/*     First, the likelihood of an error resulting in crash */
/*     in a different routine is greatly reduced. Second, */
/*     a program does not have to test the error status */
/*     (using a reference to FAILED) after each call to a toolkit */
/*     routine, but rather can make one test of status at the end */
/*     of a series of calls. See "Examples" below. */

/*     See the subroutine ERRACT for definitions of the error action */
/*     codes. */

/* $ Examples */

/*     1. In this example, we show how to place a reference */
/*         to RETURN in your code: */

/*         C */
/*         C     No executable lines precede this one. */
/*         C */
/*         C     Test whether to return before doing */
/*         C     anything else. */
/*         C */

/*               IF ( RETURN() )  RETURN */


/*               [ rest of code goes here] */

/*                         . */
/*                         . */
/*                         . */


/*     2. Here's how one might code a sequence of calls */
/*         to routines with code that follows the pattern */
/*         given in example #1 above: */

/*                        . */
/*                        . */
/*                        . */

/*               [ code may go here ] */

/*         C */
/*         C     We call routines A, B, and C;  then we */
/*         C     test for errors, using the SPICELIB error */
/*         C     status indicator, FAILED: */
/*         C */

/*               CALL  A */
/*               CALL  B */
/*               CALL  C */

/*               IF ( FAILED() ) THEN */

/*         C */
/*         C        If we're here, an error occurred. The */
/*         C        error might have been detected by A, B, C, */
/*         C        or by a routine called by one of them. */
/*         C        Get the explanation of the short error message */
/*         C        and output it using the routine, USER_OUT */
/*         C        [USER_OUT is a fictitious routine]: */
/*         C */

/*                  CALL GETMSG ( 'EXPLAIN', MSG ) */

/*                  CALL USER_OUT ( MSG ) */

/*               END IF */

/*               [ rest of code goes here ] */

/*                          . */
/*                          . */
/*                          . */

/* $ Restrictions */

/*     1)  This routine has no effect unless the error action is */
/*         'RETURN'! */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.1.1, 26-OCT-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 2.1.0, 04-APR-2014 (NJB) */

/*        Re-organized code to improve efficiency in the non-error */
/*        case. */

/* -    SPICELIB Version 2.0.0, 22-APR-1996 (KRG) */

/*        This subroutine has been modified in an attempt to improve */
/*        the general performance of the SPICELIB error handling */
/*        mechanism. The specific modification has been to change the */
/*        type of error action from a short character string to an */
/*        integer. This change is backwardly incompatible because the */
/*        type has changed. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (NJB) */

/* -& */
/* $ Index_Entries */

/*     immediate return indicator */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 2.0.0, 22-APR-1996 (KRG) */

/*        This subroutine has been modified in an attempt to improve */
/*        the general performance of the SPICELIB error handling */
/*        mechanism. The specific modification has been to change the */
/*        type of error action from a short character string to an */
/*        integer. This change is backwardly incompatible because the */
/*        type has changed. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    Beta Version 1.1.0, 17-FEB-1989 (NJB) */

/*        Added parentheses to the declaration of RETURN. */

/* -& */

/*     SPICELIB functions */


/*     Local Parameters */

/*     Define the mnemonic for the return action. */


/*     Local Variables */


/*     Immediate return is indicated only in 'RETURN' mode, */
/*     when an error condition is in effect: */

    if (! failed_()) {
	ret_val = FALSE_;
	return ret_val;
    }

/*     At this point, we know a SPICE error condition exists. */

    getact_(&action);
    ret_val = action == 3;
    return ret_val;
} /* return_ */

