/* getmsg.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;

/* $Procedure GETMSG ( Get Error Message ) */
/* Subroutine */ int getmsg_(char *option, char *msg, ftnlen option_len, 
	ftnlen msg_len)
{
    /* System generated locals */
    address a__1[2];
    integer i__1[2];
    char ch__1[144];

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen), s_cat(char *,
	     char **, integer *, integer *, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int ucase_(char *, char *, ftnlen, ftnlen), 
	    expln_(char *, char *, ftnlen, ftnlen), ljust_(char *, char *, 
	    ftnlen, ftnlen);
    char upopt[10];
    extern /* Subroutine */ int getlms_(char *, ftnlen), sigerr_(char *, 
	    ftnlen);
    char locopt[10];
    extern /* Subroutine */ int getsms_(char *, ftnlen), setmsg_(char *, 
	    ftnlen);
    char shrtms[25];

/* $ Abstract */

/*     Retrieve the current short error message, the explanation of the */
/*     short error message, or the long error message. */

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
/*     OPTION     I   Indicates type of error message. */
/*     MSG        O   The error message to be retrieved. */

/* $ Detailed_Input */

/*     OPTION   is a string that indicates the type of error message to */
/*              be retrieved. */

/*              Possible values of OPTION are: */

/*                 'SHORT'     indicates that the short message is to be */
/*                             retrieved. */

/*                 'EXPLAIN'   indicates that the explanation of the */
/*                             short message is to be retrieved. */

/*                 'LONG'      indicates that the long message is to be */
/*                             retrieved. */

/*              The input strings indicating the choice of option may be */
/*              in mixed case. Leading and trailing blanks in OPTION are */
/*              not significant. */

/* $ Detailed_Output */

/*     MSG      is the error message to be retrieved. Its value depends */
/*              on OPTION, and on whether an error condition exists. */

/*              When there is no error condition, MSG is blank. */

/*              If an error condition does exist, and OPTION is */

/*                'SHORT'        MSG is the current short error message. */
/*                               This is a very condensed, 25-character */
/*                               description of the error. */

/*                'EXPLAIN'      MSG is the explanation of the current */
/*                               short error message. This is a one-line */
/*                               expansion of the text of the short */
/*                               message. */

/*                               All SPICELIB short error messages */
/*                               do have corresponding explanation text. */
/*                               For other short error messages, if */
/*                               there is no explanation text, MSG */
/*                               will be blank. */

/*                'LONG'         MSG is the current long error message. */
/*                               The long error message is a detailed */
/*                               explanation of the error, possibly */
/*                               containing data specific to the */
/*                               particular occurrence of the error. */
/*                               Not all errors have long error messages. */
/*                               If there is none, MSG will be blank. */
/*                               Long error messages are no longer than */
/*                               320 characters. */

/*              If OPTION is invalid, MSG will remain unchanged from its */
/*              value on input. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the input OPTION is invalid, the error */
/*         SPICE(INVALIDMSGTYPE) is signaled. In that case no messages */
/*         are returned; MSG retains the value it had on input. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Please read the "required reading" first! */

/*     A good time to call this routine would be when an error */
/*     condition exists, as indicated by the SPICELIB function, */
/*     FAILED. */

/*     See the example below for a serving suggestion. */

/*     GETMSG isn't too useful if an error condition doesn't */
/*     exist, since it will return a blank string in that case. */

/* $ Examples */

/*     Here's an example of a real-life call to GETMSG to get the */
/*     explanation of the current short error message. */

/*     In this example, a SPICELIB routine, RDTEXT, is called. */
/*     Following the return from RDTEXT, the logical function, */
/*     FAILED, is tested to see whether an error occurred. */
/*     If it did, the message is retrieved and output via */
/*     a user-defined output routine: */


/*     C */
/*     C     We call RDTEXT; then test for errors... */
/*     C */
/*           CALL RDTEXT ( FILE, LINE, EOF ) */

/*           IF ( FAILED ) THEN */

/*     C */
/*     C        Get explanation text for the current short message */
/*     C        and print it: */
/*     C */

/*              CALL GETMSG ( 'EXPLAIN', TEXT ) */

/*              CALL USER_DEFINED_OUTPUT ( TEXT ) */

/*                    . */
/*                    .   [Do more stuff here] */
/*                    . */

/*           END IF */

/* $ Restrictions */

/*     1)  This routine is part of the interface to the SPICELIB error */
/*         handling mechanism. For this reason, this routine does not */
/*         participate in the trace scheme, even though it has external */
/*         references. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 20-APR-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Moved */
/*        disclaimer on participation of this routine in the SPICELIB */
/*        trace scheme from $Exceptions to $Restrictions section. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (NJB) */

/* -& */
/* $ Index_Entries */

/*     get error message */

/* -& */

/*     Local Variables: */


/*     Length of short error message: */


/*     Upper case version of the option: */


/*     Heeeeeeeeeeeeeeeeeeeeer's the code! */


/*     We only speak upper case in this routine, */
/*     so convert any lower case letters in OPTION */
/*     to upper case.  We save the original OPTION */
/*     string just in case we need to echo it in */
/*     an error message. */

    ljust_(option, upopt, option_len, (ftnlen)10);
    ucase_(upopt, upopt, (ftnlen)10, (ftnlen)10);
    if (s_cmp(upopt, "SHORT", (ftnlen)10, (ftnlen)5) == 0) {

/*        Retrieve short message: */

	getsms_(msg, msg_len);
    } else if (s_cmp(upopt, "EXPLAIN", (ftnlen)10, (ftnlen)7) == 0) {

/*        Get current short message; then get explanation */
/*        corresponding to current short error message: */

	getsms_(shrtms, (ftnlen)25);
	expln_(shrtms, msg, (ftnlen)25, msg_len);
    } else if (s_cmp(upopt, "LONG", (ftnlen)10, (ftnlen)4) == 0) {

/*        Grab long error message: */

	getlms_(msg, msg_len);
    } else {

/*        Invalid value of OPTION!!  Signal error, and set long */
/*        error message as well: */

	s_copy(locopt, option, (ftnlen)10, option_len);
/* Writing concatenation */
	i__1[0] = 134, a__1[0] = "GETMSG: An invalid value of OPTION was inp"
		"ut.  Valid choices are 'SHORT',       'EXPLAIN', or 'LONG'. "
		" The value that was input was:  ";
	i__1[1] = 10, a__1[1] = locopt;
	s_cat(ch__1, a__1, i__1, &c__2, (ftnlen)144);
	setmsg_(ch__1, (ftnlen)144);
	sigerr_("SPICE(INVALIDMSGTYPE)", (ftnlen)21);
    }
    return 0;
} /* getmsg_ */

