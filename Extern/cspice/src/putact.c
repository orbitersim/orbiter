/* putact.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure PUTACT ( Store Error Response Action ) */
/* Subroutine */ int putact_0_(int n__, integer *action)
{
    /* Initialized data */

    static integer savact = 5;

/* $ Abstract */

/*     Store the error response action. */

/*     PUTACT is a low-level data structure access routine. */
/*     DO NOT CALL THIS ROUTINE. USE ERRACT, NOT PUTACT, TO SET THE */
/*     CURRENT ERROR RESPONSE ACTION. */

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
/*     ACTION     I   The integer code for the error response action. */

/* $ Detailed_Input */

/*     ACTION   is the new integer code for the error response action. */
/*              This code is saved for use by the error handling */
/*              system. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     DO NOT CALL THIS ROUTINE. */

/*     This is a data structure access routine for the SPICELIB */
/*     error response action. This routine should be used for */
/*     no other purpose. In particular, it should not be used */
/*     by non-SPICELIB routines to set up an error response; */
/*     use ERRACT for that. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     1)  DO NOT CALL THIS ROUTINE. */

/*     2)  Calls to this routine by routines other than the SPICELIB */
/*         error handling routines may interfere with error processing. */

/*     3)  See the subroutine ERRACT for the definitions of the error */
/*         action codes. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.1.0, 27-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Added */
/*        $Index_Entries entry. */

/* -    SPICELIB Version 2.0.0, 22-APR-1996 (KRG) */

/*        This subroutine has been modified in an attempt to improve */
/*        the general performance of the SPICELIB error handling */
/*        mechanism. The specific modification has been to change the */
/*        type of the saved error action from a short character string */
/*        to an integer. This change is backwardly incompatible */
/*        because the type of the input argument has changed. This */
/*        should pose no difficulties because it is a private subroutine */
/*        used by the error handling system, and hence isolated from */
/*        direct use. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (NJB) */

/* -& */
/* $ Index_Entries */

/*     store the error response action */

/* -& */
/* $ Revisions */

/* -     SPICELIB Version 2.0.0, 22-APR-1996 (KRG) */

/*         This subroutine has been modified in an attempt to improve */
/*         the general performance of the SPICELIB error handling */
/*         mechanism. The specific modification has been to change the */
/*         type of the saved error action from a short character string */
/*         to an integer. This change is backwardly incompatible */
/*         because the type of the input argument has changed. This */
/*         should pose no difficulties because it is a private subroutine */
/*         used by the error handling system, and hence isolated from */
/*         direct use. */

/* -     SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*         Comment section for permuted index source lines was added */
/*         following the header. */

/* -     Beta Version 1.0.1, 08-FEB-1989 (NJB) */

/*         Warnings added to discourage use of this routine in */
/*         non-error-handling code. */

/* -& */

/*     Local Parameters: */

/*     Define the mnemonic for the default error action. */


/*     Local Variables: */

/*     The current error response action: */


/*     Initial values: */

    switch(n__) {
	case 1: goto L_getact;
	}


/*     Executable Code: */

    savact = *action;
    return 0;
/* $Procedure GETACT ( Get Error Response Action ) */

L_getact:
/* $ Abstract */

/*     Return the value of the current error response action. */

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

/*     INTEGER               ACTION */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     ACTION     O   The integer code for the error response action. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     ACTION   is the integer code for the current error response */
/*              action. See the ERRACT subroutine and the "required */
/*              reading" file for a detailed discussion of error */
/*              response actions. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  However, this routine is part of the SPICELIB error */
/*         handling mechanism. */

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

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.1.0, 03-JUN-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Added */
/*        $Index_Entries entry. */

/* -    SPICELIB Version 2.0.0, 22-APR-1996 (KRG) */

/*        This subroutine has been modified in an attempt to improve */
/*        the general performance of the SPICELIB error handling */
/*        mechanism. The specific modification has been to change the */
/*        type of the saved error action from a short character string */
/*        to an integer. This change is backwardly incompatible */
/*        because the type of the input argument has changed. This */
/*        should pose no difficulties because it is a private subroutine */
/*        used by the error handling system, and hence isolated from */
/*        direct use. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (NJB) */

/* -& */
/* $ Index_Entries */

/*     get current error response action */

/* -& */
/* $ Revisions */

/* -     SPICELIB Version 2.0.0, 22-APR-1996 (KRG) */

/*         This subroutine has been modified in an attempt to improve */
/*         the general performance of the SPICELIB error handling */
/*         mechanism. The specific modification has been to change the */
/*         type of the saved error action from a short character string */
/*         to an integer. This change is backwardly incompatible */
/*         because the type of the input argument has changed. This */
/*         should pose no difficulties because it is a private subroutine */
/*         used by the error handling system, and hence isolated from */
/*         direct use. */

/* -     SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*         Comment section for permuted index source lines was added */
/*         following the header. */

/* -& */

/*     Executable Code: */


/*     Grab saved error response action: */

    *action = savact;
    return 0;
} /* putact_ */

/* Subroutine */ int putact_(integer *action)
{
    return putact_0_(0, action);
    }

/* Subroutine */ int getact_(integer *action)
{
    return putact_0_(1, action);
    }

