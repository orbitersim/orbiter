/* putlms.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure PUTLMS ( Store Long Error Message ) */
/* Subroutine */ int putlms_0_(int n__, char *msg, ftnlen msg_len)
{
    /* Initialized data */

    static char savmsg[1840] = "                                            "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "    ";

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

/* $ Abstract */

/*     Store a long error message. */

/*     PUTLMS is a low-level data structure access routine. */
/*     DO NOT CALL THIS ROUTINE. USE SETMSG, NOT PUTLMS, TO SET THE */
/*     CURRENT LONG ERROR MESSAGE. */

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


/*     Include File:  SPICELIB Error Handling Parameters */

/*        errhnd.inc  Version 2    18-JUN-1997 (WLT) */

/*           The size of the long error message was */
/*           reduced from 25*80 to 23*80 so that it */
/*           will be accepted by the Microsoft Power Station */
/*           FORTRAN compiler which has an upper bound */
/*           of 1900 for the length of a character string. */

/*        errhnd.inc  Version 1    29-JUL-1997 (NJB) */



/*     Maximum length of the long error message: */


/*     Maximum length of the short error message: */


/*     End Include File:  SPICELIB Error Handling Parameters */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     MSG        I   A long error message. */

/* $ Detailed_Input */

/*     MSG      is the current long error message. This value will be */
/*              saved. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     LMSGLN   is the maximum length of the long error message. See */
/*              the include file errhnd.inc for the value of LMSGLN. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     DO NOT CALL THIS ROUTINE. */

/*     This routine should be used only by routines within the SPICELIB */
/*     error handling system. Other routines should use SETMSG to set */
/*     the long error message. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     1)  Calls to this routine by routines outside of the SPICELIB */
/*         error handling system may interfere with error processing. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.1, 27-AUG-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. Added */
/*        $Index_Entries entry. */

/* -    SPICELIB Version 1.1.0, 29-JUL-1997 (NJB) */

/*        Maximum length of the long error message is now represented */
/*        by the parameter LMSGLN. Miscellaneous header fixes were */
/*        made. Some indentation and vertical white space abnormalities */
/*        in the code were fixed. Some dubious comments were deleted */
/*        from the code. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (NJB) */

/* -& */
/* $ Index_Entries */

/*     store long error message */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.1.0, 29-JUL-1997 (NJB) */

/*        Maximum length of the long error message is now represented */
/*        by the parameter LMSGLN. Miscellaneous header fixes were */
/*        made. Some indentation and vertical white space abnormalities */
/*        in the code were fixed. Some dubious comments were deleted */
/*        from the code. */

/* -    Beta Version 1.0.1, 08-FEB-1989 (NJB) */

/*        Warnings added to discourage use of this routine in */
/*        non-error-handling code. */

/* -& */

/*     Local Variables: */


/*     The current long error message: */


/*     Initial values: */

    switch(n__) {
	case 1: goto L_getlms;
	}


/*     Executable Code: */

    s_copy(savmsg, msg, (ftnlen)1840, msg_len);
    return 0;
/* $Procedure GETLMS ( Get Long Error Message ) */

L_getlms:
/* $ Abstract */

/*     Return the value of the current long error message. */

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

/*    CHARACTER*(*)          MSG */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     MSG        O   The current long error message. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     MSG      is the current long error message. See the */
/*              "required reading" file for a detailed discussion */
/*              of error messages. */

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
/*     processing. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.3, 03-JUN-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. Added */
/*        $Index_Entries entry. Removed unnecessary $Revisions section. */

/* -    SPICELIB Version 1.0.2, 29-JUL-1997 (NJB) */

/*        Maximum length of the long error message is now represented */
/*        by the parameter LMSGLN. Miscellaneous header fixes were */
/*        made. Some indentation and vertical white space abnormalities */
/*        in the code were fixed. Some dubious comments were deleted */
/*        from the code. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (NJB) */

/* -& */
/* $ Index_Entries */

/*     get current long error message */

/* -& */

/*     Grab the saved long message: */

    s_copy(msg, savmsg, msg_len, (ftnlen)1840);
    return 0;
} /* putlms_ */

/* Subroutine */ int putlms_(char *msg, ftnlen msg_len)
{
    return putlms_0_(0, msg, msg_len);
    }

/* Subroutine */ int getlms_(char *msg, ftnlen msg_len)
{
    return putlms_0_(1, msg, msg_len);
    }

