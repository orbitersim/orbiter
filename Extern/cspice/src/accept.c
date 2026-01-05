/* accept.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ACCEPT ( Accept New Long Error Message ) */
logical accept_0_(int n__, logical *ok)
{
    /* Initialized data */

    static logical savok = TRUE_;

    /* System generated locals */
    logical ret_val;

/* $ Abstract */

/*     Indicate to the SPICELIB error handling mechanism whether or not */
/*     a replacement or modification of the long error message can be */
/*     accepted. */

/*     DO NOT CALL THIS ROUTINE. */

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
/*     OK         I   Indicates whether long error msg changes are ok. */

/*     The function takes an UNSPECIFIED value on exit. */

/* $ Detailed_Input */

/*     OK       indicates to the error handling mechanism whether */
/*              replacement of or changes to the long error message */
/*              are to be allowed; for them to be allowed, */
/*              both of the following must be true: */

/*              1. No error condition exists, or the error response */
/*                 action is not 'RETURN'. */

/*              2. The current error response mode is not 'IGNORE'. */

/* $ Detailed_Output */

/*     The function is assigned a value on output, but the */
/*     value is not meaningful. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  This routine does not detect any errors. */

/*         However, this routine is part of the SPICELIB error handling */
/*         mechanism. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     DO NOT CALL THIS ROUTINE. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     1)  DO NOT CALL THIS ROUTINE. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR) */

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

/* -     Beta Version 1.1.0, 13-DEC-1989 (NJB) */

/*         ACCEPT must return a value, in order to comply with the */
/*         Fortran standard. So, now it does. The value has no */
/*         meaning, as far as the specification of ACCEPT is */
/*         concerned. */

/* -     Beta Version 1.0.1, 08-FEB-1989 (NJB) */

/*         Warnings added to discourage use of this routine in */
/*         non-error-handling code. */

/* -& */

/*     SPICELIB functions: */


/*     Local Variables: */


/*     Initial Values: */

    switch(n__) {
	case 1: goto L_allowd;
	}


/*     Executable Code: */

    savok = *ok;
    ret_val = FALSE_;
    return ret_val;
/* $Procedure ALLOWD    (Are Changes of Long Error Message Allowed?) */

L_allowd:
/* $ Abstract */

/*     Return .TRUE. if replacement or modification of the long error */
/*     message is allowed. */

/*     DO NOT CALL THIS ROUTINE. */

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

/*     The function takes the value, .TRUE., if replacement or */
/*     modification of the long error message is currently allowed. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     The function takes the value, .TRUE., if replacement of or */
/*     changes to the long error message are to be allowed; for them */
/*     to be allowed, both of the following must be true: */

/*     1. No error condition exists, or the error response */
/*        action is not 'RETURN'. */

/*     2. The current error response mode is not 'IGNORE'. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  This routine does not detect any errors. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     DO NOT CALL THIS ROUTINE. */

/*     Non-error handling routines should not call this routine. Such */
/*     routines can set the long error message using SETMSG, which */
/*     itself calls this routine to test whether an update is allowed. */

/*     The initial value returned by ALLOWD is .FALSE. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     1)  DO NOT CALL THIS ROUTINE. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     H.A. Neilan        (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (NJB) (HAN) */

/* -& */
/* $ Index_Entries */

/*     allow changes of long error message */

/* -& */
/* $ Revisions */

/* -     Beta Version 1.1.0, 18-DEC-1989 (HAN) */

/*         Empty parentheses added to the ENTRY statement in order to */
/*         comply with the ANSI Fortran 77 Standard. */

/* -     Beta Version 1.0.1, 08-FEB-1989 (NJB) */

/*         Warnings added to discourage use of this routine in */
/*         non-error-handling code. */

/* -& */

/*     Executable Code: */

    ret_val = savok;
    return ret_val;
} /* accept_ */

logical accept_(logical *ok)
{
    return accept_0_(0, ok);
    }

logical allowd_(void)
{
    return accept_0_(1, (logical *)0);
    }

