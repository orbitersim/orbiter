/* errprt.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__0 = 0;
static integer c__10 = 10;
static integer c__2 = 2;

/* $Procedure ERRPRT ( Get/Set Error Output Items ) */
/* Subroutine */ int errprt_(char *op, char *list, ftnlen op_len, ftnlen 
	list_len)
{
    /* System generated locals */
    address a__1[2];
    integer i__1, i__2, i__3[2];
    char ch__1[89], ch__2[65];

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_rnge(char *, integer, char *, integer);
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen);

    /* Local variables */
    logical long__, expl;
    char upop[3];
    integer i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    logical trace;
    extern /* Subroutine */ int ucase_(char *, char *, ftnlen, ftnlen);
    char locop[3], words[9*10];
    logical short__;
    extern /* Subroutine */ int ljust_(char *, char *, ftnlen, ftnlen);
    logical dfault;
    extern /* Subroutine */ int lparse_(char *, char *, integer *, integer *, 
	    char *, ftnlen, ftnlen, ftnlen);
    extern logical msgsel_(char *, ftnlen);
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), suffix_(char *, integer *, char 
	    *, ftnlen, ftnlen);
    integer numwrd;
    char upword[9];
    extern logical setprt_(logical *, logical *, logical *, logical *, 
	    logical *);
    logical status;

/* $ Abstract */

/*     Retrieve or set the list of error message items */
/*     to be output when an error is detected. */

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
/*     OP         I   The operation:  'GET' or 'SET'. */
/*     LIST      I-O  Specification of error messages to be output. */

/* $ Detailed_Input */

/*     OP       indicates the operation to be performed. Possible */
/*              values are 'GET' and 'SET'. */

/*              'SET' means, "the following list specifies the default */
/*              selection of error messages to be output." These are */
/*              the messages that will be output to the default error */
/*              output device (selected by ERRDEV) when an error is */
/*              detected. */

/*              'GET' means, "return the current list of error output */
/*              items." This is the exact list that was set by the */
/*              last call to this routine with the 'SET' option. */

/*              The option can be specified in mixed case. For example, */
/*              the following call will work: */

/*              CALL ERRPRT ( 'SeT' , 'ALL' ) */


/*     LIST     is a list of error message items. The items */
/*              are delimited by commas. The items that can be */
/*              in the list are the words: */

/*              1.  SHORT        ...indicates the short error message */
/*              2.  EXPLAIN      ...the explanation of the short message */
/*              3.  LONG         ...the long error message */
/*              4.  TRACEBACK    ...the traceback */
/*              5.  ALL          ...indicates "output all messages" */
/*              6.  NONE         ...indicates "don't output any messages" */
/*              7.  DEFAULT      ...same as ALL, but includes default */
/*                                  message */

/*              A "list" is a character string containing some or */
/*              all of the above words, delimited by commas. Examples */
/*              are: */

/*              1.  'SHORT, EXPLAIN' */
/*              2.  'SHORT, LONG' */
/*              3.  'ALL' */
/*              4.  'NONE' */
/*              5.  'ALL, NONE, ALL, SHORT, NONE' */

/*              Each word in the list can be thought of as */
/*              "flipping a switch" to enable or disable the output */
/*              of the message(s) indicated by the word. The */
/*              words are acted on in the order they occur in the */
/*              list, starting with the leftmost word. As examples, */
/*              consider the sample lists above. */

/*              The effect of the first list above, 'SHORT, EXPLAIN', */
/*              is to enable the output of the short error message */
/*              and the explanatory text corresponding to it. */

/*              The effect of the second list is to enable the output */
/*              of the short and long messages. */

/*              The effect of the third list is to enable the output of */
/*              all of the error messages (short, long, explanation */
/*              of the short message, and traceback). */

/*              The effect of the fourth list is to disable output of */
/*              all of the messages. */

/*              The effect of the fifth list is to disable output of */
/*              all of the messages. The reason for this is that */
/*              the words in the list are responded to in order, */
/*              from left to right, and "NONE" is the last word. */

/*              If any words other than SHORT, LONG, EXPLAIN, ALL, */
/*              DEFAULT, TRACEBACK or NONE appear in LIST, those words */
/*              that are recognized are responded to. The words */
/*              that are not recognized are diagnosed as */
/*              erroneous, and error messages are generated */
/*              for each such unrecognized word. */

/*              The length of LIST is caller-defined, but only */
/*              the first 100 characters of LIST will be saved */
/*              for later retrieval. */

/*              Only the first 10 items in the list are used; */
/*              the rest are ignored. */

/* $ Detailed_Output */

/*     LIST     is a list of error message items. The value of */
/*              LIST is that set by the last call to this routine */
/*              using the 'SET' option. See "Detailed Input" */
/*              for a description of the possible values and */
/*              meanings of LIST. */

/*              The initial value returned is 'DEFAULT'. */

/*              Only the first 100 characters of LIST are saved */
/*              when the list is set; any additional characters */
/*              are truncated. Therefore, the first 100 */
/*              characters, at most, of the saved value of LIST */
/*              will be returned. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If an invalid value of the argument OP is supplied, the error */
/*         SPICE(INVALIDOPERATION) is signaled. */

/*     2)  If OP is 'SET' and an invalid word is detected within the list */
/*         of error message items LIST, the error SPICE(INVALIDLISTITEM) */
/*         is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine is part of the SPICELIB error handling mechanism. */

/*     Please read the "required reading"! */

/*     This routine is intended to be used in conjunction with */
/*     ERRDEV, which selects the default output device to which */
/*     the error messages selected by this routine will be */
/*     output. */

/*     Additionally, the error response action must be */
/*     something other than 'IGNORE' if the error messages */
/*     are to be output. Possible choices of the error */
/*     response action are 'RETURN', 'REPORT', 'ABORT', 'DEFAULT', and */
/*     'IGNORE'.  Use ERRACT to set the error response action. */


/*     Only the first 100 characters of LIST are saved. */

/*     The default set of error messages that are output is the */
/*     set specified by 'DEFAULT'; i.e., all of them, including */
/*     the 'default' message. */

/* $ Examples */

/*     1. In this example, we select as the output device */
/*        the file, SPUD.DAT, and then select the error */
/*        messages to be output. We choose the short */
/*        error message and the traceback. Since a */
/*        different set of messages may have been selected */
/*        previously, we clear the old setting by putting */
/*        the word, 'NONE', at the beginning of the list. */

/*        C */
/*        C      Set the error output device to SPUD.DAT: */
/*        C */

/*               CALL ERRDEV (  'SET',  'SPUD.DAT'  ) */

/*        C */
/*        C      Choose error messages: */
/*        C */

/*               CALL ERRPRT (  'SET',  'NONE, SHORT, TRACEBACK'  ) */

/* $ Restrictions */

/*     1)  The device to which the selected error messages will be */
/*         written must be selected via ERRDEV; otherwise, messages will */
/*         be written to the initial default device. */

/*     2)  Only the first 100 characters of LIST are saved. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.0, 19-APR-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. $Exceptions */
/*        section has been completely updated to provide only the list */
/*        of exceptions. Additional information provided there has been */
/*        moved to $Particulars. */

/* -    SPICELIB Version 1.1.0, 28-AUG-1999 (NJB) */

/*        Output string is now built on the fly. The routine previously */
/*        returned a saved string which could fail to represent correctly */
/*        the set of selected message types. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (NJB) */

/* -& */
/* $ Index_Entries */

/*     get/set error output items */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.1.0, 28-AUG-1999 (NJB) */

/*        Output string is now built on the fly. The routine previously */
/*        returned a saved string which could fail to represent correctly */
/*        the set of selected message types. */

/* -    Beta Version 1.2.0, 16-FEB-1988 (NJB) */

/*        Declaration of the unused variable TMPLST removed. */
/*        Trace participation added. This routine now checks in */
/*        and checks out. However, it does not test RETURN, */
/*        because it should be able to execute in RETURN mode when */
/*        an error condition exists. */

/* -    Beta Version 1.1.0, 06-OCT-1988 (NJB) */

/*        Superfluous references to LASTNB removed. These references */
/*        were so many tonsils; they really had no function. */

/* -& */

/*     SPICELIB functions */


/*     Local Variables: */


/*     Executable Code: */

    chkin_("ERRPRT", (ftnlen)6);

/*     We first initialize the message selection flags to */
/*     correspond to the current selection of error messages: */

    short__ = msgsel_("SHORT", (ftnlen)5);
    long__ = msgsel_("LONG", (ftnlen)4);
    expl = msgsel_("EXPLAIN", (ftnlen)7);
    trace = msgsel_("TRACEBACK", (ftnlen)9);
    dfault = msgsel_("DEFAULT", (ftnlen)7);

/*     We save the operation string as input, and get */
/*     an upper case version for our own use: */

    ljust_(op, upop, op_len, (ftnlen)3);
    ucase_(upop, upop, (ftnlen)3, (ftnlen)3);
    if (s_cmp(upop, "GET", (ftnlen)3, (ftnlen)3) == 0) {

/*        Construct a string indicating which messages are enabled. */

	s_copy(list, " ", list_len, (ftnlen)1);
	if (short__) {
	    s_copy(list, "SHORT", list_len, (ftnlen)5);
	}
	if (long__) {
	    if (s_cmp(list, " ", list_len, (ftnlen)1) == 0) {
		s_copy(list, "LONG", list_len, (ftnlen)4);
	    } else {
		suffix_(", LONG", &c__0, list, (ftnlen)6, list_len);
	    }
	}
	if (expl) {
	    if (s_cmp(list, " ", list_len, (ftnlen)1) == 0) {
		s_copy(list, "EXPLAIN", list_len, (ftnlen)7);
	    } else {
		suffix_(", EXPLAIN", &c__0, list, (ftnlen)9, list_len);
	    }
	}
	if (trace) {
	    if (s_cmp(list, " ", list_len, (ftnlen)1) == 0) {
		s_copy(list, "TRACEBACK", list_len, (ftnlen)9);
	    } else {
		suffix_(", TRACEBACK", &c__0, list, (ftnlen)11, list_len);
	    }
	}
	if (dfault) {
	    if (s_cmp(list, " ", list_len, (ftnlen)1) == 0) {
		s_copy(list, "DEFAULT", list_len, (ftnlen)7);
	    } else {
		suffix_(", DEFAULT", &c__0, list, (ftnlen)9, list_len);
	    }
	}
    } else if (s_cmp(upop, "SET", (ftnlen)3, (ftnlen)3) == 0) {

/*        We parse the list of words, converting each word */
/*        to upper case, testing each word for validity, */
/*        and "flipping the switches" to enable or disable */
/*        the output of the various error messages as */
/*        directed by each word, starting with the leftmost. */
/*        We update local flags according to the words we */
/*        recognize, and update the global flags when we're */
/*        done parsing the list. */

/*        If an invalid word is encountered, we signal an */
/*        error, and continue parsing the list. */


	lparse_(list, ",", &c__10, &numwrd, words, list_len, (ftnlen)1, (
		ftnlen)9);
	i__1 = numwrd;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    ucase_(words + ((i__2 = i__ - 1) < 10 && 0 <= i__2 ? i__2 : 
		    s_rnge("words", i__2, "errprt_", (ftnlen)440)) * 9, 
		    upword, (ftnlen)9, (ftnlen)9);
	    if (s_cmp(upword, "SHORT", (ftnlen)9, (ftnlen)5) == 0) {
		short__ = TRUE_;
	    } else if (s_cmp(upword, "LONG", (ftnlen)9, (ftnlen)4) == 0) {
		long__ = TRUE_;
	    } else if (s_cmp(upword, "EXPLAIN", (ftnlen)9, (ftnlen)7) == 0) {
		expl = TRUE_;
	    } else if (s_cmp(upword, "TRACEBACK", (ftnlen)9, (ftnlen)9) == 0) 
		    {
		trace = TRUE_;
	    } else if (s_cmp(upword, "ALL", (ftnlen)9, (ftnlen)3) == 0) {
		short__ = TRUE_;
		long__ = TRUE_;
		expl = TRUE_;
		trace = TRUE_;
	    } else if (s_cmp(upword, "DEFAULT", (ftnlen)9, (ftnlen)7) == 0) {
		short__ = TRUE_;
		long__ = TRUE_;
		expl = TRUE_;
		trace = TRUE_;
		dfault = TRUE_;
	    } else if (s_cmp(upword, "NONE", (ftnlen)9, (ftnlen)4) == 0) {
		short__ = FALSE_;
		long__ = FALSE_;
		expl = FALSE_;
		trace = FALSE_;
		dfault = FALSE_;
	    } else if (s_cmp(upword, " ", (ftnlen)9, (ftnlen)1) != 0) {

/*              Oops! Invalid word... */

/* Writing concatenation */
		i__3[0] = 80, a__1[0] = "ERRPRT: An invalid list item was fo"
			"und in the error message list.  The word was:";
		i__3[1] = 9, a__1[1] = words + ((i__2 = i__ - 1) < 10 && 0 <= 
			i__2 ? i__2 : s_rnge("words", i__2, "errprt_", (
			ftnlen)486)) * 9;
		s_cat(ch__1, a__1, i__3, &c__2, (ftnlen)89);
		setmsg_(ch__1, (ftnlen)89);
		sigerr_("SPICE(INVALIDLISTITEM)", (ftnlen)22);
	    }

/*           At this point, we have either set some set of */
/*           flags in response to WORD, or determined that */
/*           WORD was invalid. */

	}

/*        We've now responded to all words in LIST. */


/*        Now we store the flag values we've set, for global */
/*        consumption (SETPRT doesn't actually detect errors). */

	status = setprt_(&short__, &expl, &long__, &trace, &dfault);
    } else {

/*        An invalid value of OP was supplied. */

	s_copy(locop, op, (ftnlen)3, op_len);
/* Writing concatenation */
	i__3[0] = 62, a__1[0] = "ERRPRT:  An invalid value of OP was supplie"
		"d.  The value was: ";
	i__3[1] = 3, a__1[1] = locop;
	s_cat(ch__2, a__1, i__3, &c__2, (ftnlen)65);
	setmsg_(ch__2, (ftnlen)65);
	sigerr_("SPICE(INVALIDOPERATION)", (ftnlen)23);
    }
    chkout_("ERRPRT", (ftnlen)6);
    return 0;
} /* errprt_ */

