/* outmsg.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__5 = 5;
static integer c__2 = 2;
static integer c__3 = 3;
static integer c__0 = 0;
static integer c__1 = 1;

/* $Procedure OUTMSG ( Output Error Messages ) */
/* Subroutine */ int outmsg_(char *list, ftnlen list_len)
{
    /* Initialized data */

    static char defmsg[80*4] = "Oh, by the way:  The SPICELIB error handling"
	    " actions are USER-TAILORABLE.  You  " "can choose whether the To"
	    "olkit aborts or continues when errors occur, which     " "error "
	    "messages to output, and where to send the output.  Please read t"
	    "he ERROR  " "\"Required Reading\" file, or see the routines ERRA"
	    "CT, ERRDEV, and ERRPRT.        ";
    static logical first = TRUE_;

    /* System generated locals */
    address a__1[2], a__2[3];
    integer i__1, i__2, i__3[2], i__4[3];
    char ch__1[38];

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer), s_cmp(char *, char *, 
	    ftnlen, ftnlen);
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen),
	     s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    char name__[32], line[80];
    logical long__;
    char lmsg[1840];
    logical expl;
    char smsg[25], xmsg[80];
    integer i__;
    logical trace;
    extern /* Subroutine */ int ucase_(char *, char *, ftnlen, ftnlen);
    integer depth, index;
    extern integer wdcnt_(char *, ftnlen);
    extern /* Subroutine */ int expln_(char *, char *, ftnlen, ftnlen);
    extern integer rtrim_(char *, ftnlen);
    char versn[80], words[9*5];
    integer start;
    logical short__;
    extern /* Subroutine */ int ljust_(char *, char *, ftnlen, ftnlen);
    char device[255];
    integer remain;
    static char border[80];
    extern /* Subroutine */ int getdev_(char *, ftnlen);
    logical dfault;
    integer length;
    extern /* Subroutine */ int trcdep_(integer *);
    extern integer lastnb_(char *, ftnlen);
    extern /* Subroutine */ int trcnam_(integer *, char *, ftnlen), lparse_(
	    char *, char *, integer *, integer *, char *, ftnlen, ftnlen, 
	    ftnlen);
    extern logical msgsel_(char *, ftnlen);
    integer wrdlen;
    extern /* Subroutine */ int getlms_(char *, ftnlen), wrline_(char *, char 
	    *, ftnlen, ftnlen), getsms_(char *, ftnlen), suffix_(char *, 
	    integer *, char *, ftnlen, ftnlen);
    char tmpmsg[105];
    extern /* Subroutine */ int nextwd_(char *, char *, char *, ftnlen, 
	    ftnlen, ftnlen);
    integer numwrd;
    char upword[9], outwrd[1840];
    extern /* Subroutine */ int tkvrsn_(char *, char *, ftnlen, ftnlen);
    logical output;

/* $ Abstract */

/*     Output error messages. */

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
/*     LIST       I   A list of error message types. */
/*     FILEN      P   Maximum length of file name. */
/*     NAMLEN     P   Maximum length of module name. See TRCPKG. */
/*     LL         P   Output line length. */

/* $ Detailed_Input */

/*     LIST     is a list of error message types. A list is a */
/*              character string containing one or more words */
/*              from the following list, separated by commas. */

/*                 SHORT */
/*                 EXPLAIN */
/*                 LONG */
/*                 TRACEBACK */
/*                 DEFAULT */

/*              Each type of error message specified in LIST will */
/*              be output when an error is detected, if it is */
/*              enabled for output. Note that DEFAULT does */
/*              NOT refer to the "default message selection," */
/*              but rather to a special message that is output */
/*              when the error action is 'DEFAULT'.  This message */
/*              is a statement referring the user to the error */
/*              handling documentation. */

/*              Messages are never duplicated in the output; for */
/*              instance, supplying a value of LIST such as */

/*                 'SHORT, SHORT' */

/*              does NOT result in the output of two short */
/*              messages. */

/*              The words in LIST may appear in mixed case; */
/*              for example, the call */

/*                 CALL OUTMSG ( 'ShOrT' ) */

/*              will work. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     FILEN    is the maximum device name length that can be */
/*              accommodated by this routine. */

/*     NAMELN   is the maximum length of an individual module name. */

/*     LL       is the maximum line length for the output message. */
/*              If the output message string is very long, it is */
/*              displayed over several lines, each of which has a */
/*              maximum length of LL characters. */

/* $ Exceptions */

/*     1)  If an invalid message type is provided in LIST, the error */
/*         SPICE(INVALIDLISTITEM) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine is part of the SPICELIB error handling */
/*     mechanism. */

/*     This routine outputs the error messages specified in LIST that */
/*     have been enabled for output (use the SPICELIB routine ERRPRT */
/*     to enable or disable output of specified types of error */
/*     messages).  A border is written out preceding and following the */
/*     messages. Output is directed to the current error output device. */

/* $ Examples */

/*     1)  Output the short and long error messages: */

/*        C */
/*        C     Output short and long messages: */
/*        C */
/*              CALL OUTMSG ( 'SHORT, LONG' ) */

/* $ Restrictions */

/*     1)  This routine is intended for use by the SPICELIB error */
/*         handling mechanism. SPICELIB users are not expected to */
/*         need to call this routine. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     H.A. Neilan        (JPL) */
/*     B.V. Semenov       (JPL) */
/*     M.J. Spencer       (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 5.29.0, 28-NOV-2021 (BVS) */

/*        Updated for MAC-OSX-M1-64BIT-CLANG_C. */

/* -    SPICELIB Version 5.28.0, 13-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 5.27.0, 10-MAR-2014 (BVS) */

/*        Updated for SUN-SOLARIS-64BIT-INTEL. */

/* -    SPICELIB Version 5.26.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-LINUX-64BIT-IFORT. */

/* -    SPICELIB Version 5.25.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-CYGWIN-GFORTRAN. */

/* -    SPICELIB Version 5.24.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-CYGWIN-64BIT-GFORTRAN. */

/* -    SPICELIB Version 5.23.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-CYGWIN-64BIT-GCC_C. */

/* -    SPICELIB Version 5.22.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-INTEL. */

/* -    SPICELIB Version 5.21.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-INTEL-CC_C. */

/* -    SPICELIB Version 5.20.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C. */

/* -    SPICELIB Version 5.19.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-64BIT-NATIVE_C. */

/* -    SPICELIB Version 5.18.0, 13-MAY-2010 (BVS) */

/*        Updated for PC-WINDOWS-64BIT-IFORT. */

/* -    SPICELIB Version 5.17.0, 13-MAY-2010 (BVS) */

/*        Updated for PC-LINUX-64BIT-GFORTRAN. */

/* -    SPICELIB Version 5.16.0, 13-MAY-2010 (BVS) */

/*        Updated for PC-64BIT-MS_C. */

/* -    SPICELIB Version 5.15.0, 13-MAY-2010 (BVS) */

/*        Updated for MAC-OSX-64BIT-INTEL_C. */

/* -    SPICELIB Version 5.14.0, 13-MAY-2010 (BVS) */

/*        Updated for MAC-OSX-64BIT-IFORT. */

/* -    SPICELIB Version 5.13.0, 13-MAY-2010 (BVS) */

/*        Updated for MAC-OSX-64BIT-GFORTRAN. */

/* -    SPICELIB Version 5.12.0, 18-MAR-2009 (BVS) */

/*        Updated for PC-LINUX-GFORTRAN. */

/* -    SPICELIB Version 5.11.0, 18-MAR-2009 (BVS) */

/*        Updated for MAC-OSX-GFORTRAN. */

/* -    SPICELIB Version 5.10.0, 01-MAR-2009 (NJB) */

/*        Bug fix: truncation of long words in */
/*        output has been corrected. Local parameter */
/*        TMPLEN was added and is used in declaration */
/*        of TMPMSG. */

/* -    SPICELIB Version 5.9.0, 19-FEB-2008 (BVS) */

/*        Updated for PC-LINUX-IFORT. */

/* -    SPICELIB Version 5.8.0, 14-NOV-2006 (BVS) */

/*        Updated for PC-LINUX-64BIT-GCC_C. */

/* -    SPICELIB Version 5.7.0, 14-NOV-2006 (BVS) */

/*        Updated for MAC-OSX-INTEL_C. */

/* -    SPICELIB Version 5.6.0, 14-NOV-2006 (BVS) */

/*        Updated for MAC-OSX-IFORT. */

/* -    SPICELIB Version 5.5.0, 14-NOV-2006 (BVS) */

/*        Updated for PC-WINDOWS-IFORT. */

/* -    SPICELIB Version 5.4.0, 26-OCT-2005 (BVS) */

/*        Updated for SUN-SOLARIS-64BIT-GCC_C. */

/* -    SPICELIB Version 5.3.0, 03-JAN-2005 (BVS) */

/*        Updated for PC-CYGWIN_C. */

/* -    SPICELIB Version 5.2.0, 03-JAN-2005 (BVS) */

/*        Updated for PC-CYGWIN. */

/* -    SPICELIB Version 5.1.5, 17-JUL-2002 (BVS) */

/*        Added MAC-OSX environments. */

/* -    SPICELIB Version 5.1.4, 08-OCT-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given. New */
/*        environments are WIN-NT */

/* -    SPICELIB Version 5.1.3, 24-SEP-1999 (NJB) */

/*        CSPICE environments were added. Some typos were corrected. */

/* -    SPICELIB Version 5.1.2, 28-JUL-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given. New */
/*        environments are PC-DIGITAL, SGI-O32 and SGI-N32. */

/* -    SPICELIB Version 5.1.1, 18-MAR-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given. Previously, */
/*        environments such as SUN-SUNOS and SUN-SOLARIS were implied */
/*        by the environment label SUN. */

/* -    SPICELIB Version 5.1.0, 13-JAN-1999 (BVS) */

/*        ``errhnd.inc'' file was included. Long and short error */
/*        message lengths parameter declarations were deleted. Long */
/*        and short error message string sizes were changed to those */
/*        declared in ``errhnd.inc''. */

/* -    SPICELIB Version 5.0.0, 08-APR-1998 (NJB) */

/*        Module was updated for the PC-LINUX platform. */

/* -    SPICELIB Version 4.0.0, 09-MAY-1996 (KRG) */

/*        Added the toolkit version to the output error message. */

/*        Updated this routine to be consistent with the trace package */
/*        revisions. This primarily affects the creation of the */
/*        traceback string. */

/*        Long error messages are now wrapped on word boundaries when */
/*        they are longer than the output line length. Note that this */
/*        only happens for long error messages obtained from GETLMS, */
/*        and not for the error messages displayed by this subroutine */
/*        and other error handling subroutines that write their own */
/*        error messages. */

/* -    SPICELIB Version 3.0.0, 09-NOV-1993 (HAN) */

/*        Module was updated to include the value for FILEN */
/*        for the Silicon Graphics, DEC Alpha-OSF/1, and */
/*        NeXT platforms. Also, the previous value of 256 for */
/*        Unix platforms was changed to 255. */

/* -    SPICELIB Version 2.2.0, 12-OCT-1992 (HAN) */

/*        Updated module for multiple environments. Moved the parameter */
/*        LL to the $Declarations section of the header since it's */
/*        environment dependent. */

/*        The code was also reformatted so that a utility program can */
/*        create the source file for a specific environment given a */
/*        master source file. */

/* -    SPICELIB Version 2.1.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 2.1.0, 15-MAY-1991 (MJS) */

/*        Module was updated to include the value of LL for the */
/*        Macintosh. */

/* -    SPICELIB Version 2.0.0, 28-MAR-1991 (NJB) */

/*        Work-around for MS Fortran compiler error under DOS 3.10 */
/*        was made. Some substring bounds were simplified using RTRIM. */
/*        Updates were made to the header to clarify the text and */
/*        improve the header's appearance. The default error message */
/*        was slightly de-uglified. */

/*        The IBM PC version of this routine now uses an output line */
/*        length of 78 characters rather than 80. This prevents */
/*        wrapping of the message borders and default error message. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (NJB) */

/* -& */
/* $ Index_Entries */

/*     None. */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 5.1.0, 13-JAN-1999 (BVS) */

/*        ``errhnd.inc'' file was included. Long and short error */
/*        message lengths parameter declarations were deleted. Long */
/*        and short error message string size were changed to those */
/*        declared in ``errhnd.inc''. */

/* -    SPICELIB Version 5.0.0, 08-APR-1998 (NJB) */

/*        Module was updated for the PC-LINUX platform. */

/* -    SPICELIB Version 4.0.0, 09-MAY-1996 (KRG) */

/*        Added the toolkit version to the output error message. */

/*        Updated this routine to be consistent with the trace package */
/*        revisions. This primarily affects the creation of the */
/*        traceback string. */

/*        Long error messages are now wrapped on word boundaries when */
/*        they are longer than the output line length. Note that this */
/*        only happens for long error messages obtained from GETLMS, */
/*        and not for the error messages displayed by this subroutine */
/*        and other error handling subroutines that write their own */
/*        error messages. */

/* -    SPICELIB Version 3.0.0, 9-NOV-1993 (HAN) */

/*        Module was updated to include the value for FILEN */
/*        for the Silicon Graphics, DEC Alpha-OSF/1, and */
/*        NeXT platforms. Also, the previous value of 256 for */
/*        Unix platforms was changed to 255. */

/* -    SPICELIB Version 2.2.0, 12-OCT-1992 (HAN) */

/*        Updated module for multiple environments. Moved the */
/*        parameter LL to the $Declarations section of the header since */
/*        it's environment dependent. */

/*        The code was also reformatted so that a utility program can */
/*        create the source file for a specific environment given a */
/*        master source file. */

/* -    SPICELIB Version 2.1.0, 15-MAY-1991 (MJS) */

/*        Module was updated to include the value of LL for the */
/*        Macintosh. */

/* -    SPICELIB Version 2.0.0, 28-MAR-1991 (NJB) */

/*        1)  Work-around for MS Fortran compiler error under DOS 3.10 */
/*            was made. The compiler did not correctly handle code that */
/*            concatenated strings whose bounds involved the intrinsic */
/*            MAX function. */

/*        2)  Some substring bounds were simplified using RTRIM. */

/*        3)  Updates were made to the header to clarify the text and */
/*            improve the header's appearance. */

/*        4)  $Declarations were re-organized. */

/*        5)  The default error message was slightly de-uglified. */

/*        6)  The IBM PC version of this routine now uses an output line */
/*            length of 78 characters rather than 80. This prevents */
/*            wrapping of the message borders and default error message. */

/* -    Beta Version 1.3.0, 19-JUL-1989 (NJB) */

/*        Calls to REMSUB removed; blanking and left-justifying used */
/*        instead. This was done because REMSUB handles substring */
/*        bounds differently than in previous versions, and no longer */
/*        handles all possible inputs as required by this routine. */
/*        LJUST, which is used now, is error free. */

/*        Also, an instance of .LT. was changed to .LE. The old code */
/*        caused a line break one character too soon. A minor bug, but */
/*        a bug nonetheless. */

/*        Also, two substring bounds were changed to ensure that they */
/*        remain greater than zero. */

/* -    Beta Version 1.2.0, 16-FEB-1989 (NJB) */

/*        Warnings added to discourage use of this routine in */
/*        non-error-handling code. $Parameters section updated to */
/*        describe FILEN and NAMLEN. */

/*        Declaration of unused function FAILED removed. */

/* -    Beta Version 1.1.0, 06-OCT-1988 (NJB) */

/*        Test added to ensure substring upper bound is greater than 0. */
/*        REMAIN must be greater than 0 when used as the upper bound */
/*        for a substring of NAME. Also, substring upper bound in */
/*        WRLINE call is now forced to be greater than 0. */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     These parameters are system-independent. */


/*     Local variables */


/*     Saved variables */


/*     Initial Values: */


/*     Executable Code: */


/*     The first time through, set up the output borders. */

    if (first) {
	first = FALSE_;
	for (i__ = 1; i__ <= 80; ++i__) {
	    *(unsigned char *)&border[i__ - 1] = '=';
	}
    }

/*     No messages are to be output which are not specified */
/*     in LIST: */

    short__ = FALSE_;
    expl = FALSE_;
    long__ = FALSE_;
    trace = FALSE_;
    dfault = FALSE_;
/*     We parse the list of message types, and set local flags */
/*     indicating which ones are to be output.  If we find */
/*     a word we don't recognize in the list, we signal an error */
/*     and continue parsing the list. */

    lparse_(list, ",", &c__5, &numwrd, words, list_len, (ftnlen)1, (ftnlen)9);
    i__1 = numwrd;
    for (i__ = 1; i__ <= i__1; ++i__) {
	ucase_(words + ((i__2 = i__ - 1) < 5 && 0 <= i__2 ? i__2 : s_rnge(
		"words", i__2, "outmsg_", (ftnlen)629)) * 9, upword, (ftnlen)
		9, (ftnlen)9);
	if (s_cmp(upword, "SHORT", (ftnlen)9, (ftnlen)5) == 0) {
	    short__ = TRUE_;
	} else if (s_cmp(upword, "EXPLAIN", (ftnlen)9, (ftnlen)7) == 0) {
	    expl = TRUE_;
	} else if (s_cmp(upword, "LONG", (ftnlen)9, (ftnlen)4) == 0) {
	    long__ = TRUE_;
	} else if (s_cmp(upword, "TRACEBACK", (ftnlen)9, (ftnlen)9) == 0) {
	    trace = TRUE_;
	} else if (s_cmp(upword, "DEFAULT", (ftnlen)9, (ftnlen)7) == 0) {
	    dfault = TRUE_;
	} else {

/*           Unrecognized word!  This is an error... */

/*           We have a special case on our hands; this routine */
/*           is itself called by SIGERR, so a recursion error will */
/*           result if this routine calls SIGERR.  So we output */
/*           the error message directly: */

	    getdev_(device, (ftnlen)255);
	    wrline_(device, "SPICE(INVALIDLISTITEM)", (ftnlen)255, (ftnlen)22)
		    ;
	    wrline_(device, " ", (ftnlen)255, (ftnlen)1);
	    wrline_(device, "OUTMSG:  An invalid message type was specified "
		    "in the type list. ", (ftnlen)255, (ftnlen)65);
/* Writing concatenation */
	    i__3[0] = 29, a__1[0] = "The invalid message type was ";
	    i__3[1] = 9, a__1[1] = words + ((i__2 = i__ - 1) < 5 && 0 <= i__2 
		    ? i__2 : s_rnge("words", i__2, "outmsg_", (ftnlen)666)) * 
		    9;
	    s_cat(ch__1, a__1, i__3, &c__2, (ftnlen)38);
	    wrline_(device, ch__1, (ftnlen)255, (ftnlen)38);
	}
    }

/*     LIST has been parsed. */

/*     Now, we output those error messages that were specified by LIST */
/*     and which belong to the set of messages selected for output. */


/*     We get the default error output device: */

    getdev_(device, (ftnlen)255);
    output = short__ && msgsel_("SHORT", (ftnlen)5) || expl && msgsel_("EXPL"
	    "AIN", (ftnlen)7) || long__ && msgsel_("LONG", (ftnlen)4) || trace 
	    && msgsel_("TRACEBACK", (ftnlen)9) || dfault && msgsel_("DEFAULT",
	     (ftnlen)7) && s_cmp(device, "NULL", (ftnlen)255, (ftnlen)4) != 0;

/*     We go ahead and output those messages that have been specified */
/*     in the list and also are enabled for output. The order of the */
/*     cases below IS significant; the order in which the messages */
/*     appear in the output depends on it. */


/*     If there's nothing to output, we can leave now. */

    if (! output) {
	return 0;
    }

/*     Write the starting border: skip a line, write the border, */
/*     skip a line. */

    wrline_(device, " ", (ftnlen)255, (ftnlen)1);
    wrline_(device, border, (ftnlen)255, (ftnlen)80);
    wrline_(device, " ", (ftnlen)255, (ftnlen)1);

/*     Output the toolkit version and skip a line. */

    tkvrsn_("TOOLKIT", versn, (ftnlen)7, (ftnlen)80);
/* Writing concatenation */
    i__3[0] = 17, a__1[0] = "Toolkit version: ";
    i__3[1] = 80, a__1[1] = versn;
    s_cat(line, a__1, i__3, &c__2, (ftnlen)80);
    wrline_(device, line, (ftnlen)255, (ftnlen)80);
    wrline_(device, " ", (ftnlen)255, (ftnlen)1);

/*     Next, we output the messages specified in the list */
/*     that have been enabled. */

/*     We start with the short message and its accompanying */
/*     explanation.  If both are to be output, they are */
/*     concatenated into a single message. */

    if (short__ && msgsel_("SHORT", (ftnlen)5) && (expl && msgsel_("EXPLAIN", 
	    (ftnlen)7))) {

/*        Extract the short message from global storage; then get */
/*        the corresponding explanation. */

	getsms_(smsg, (ftnlen)25);
	expln_(smsg, xmsg, (ftnlen)25, (ftnlen)80);
/* Writing concatenation */
	i__4[0] = rtrim_(smsg, (ftnlen)25), a__2[0] = smsg;
	i__4[1] = 4, a__2[1] = " -- ";
	i__4[2] = 80, a__2[2] = xmsg;
	s_cat(tmpmsg, a__2, i__4, &c__3, (ftnlen)105);
	wrline_(device, tmpmsg, (ftnlen)255, (ftnlen)105);
	wrline_(device, " ", (ftnlen)255, (ftnlen)1);
    } else if (short__ && msgsel_("SHORT", (ftnlen)5)) {

/*        Output the short error message without the explanation. */

	getsms_(smsg, (ftnlen)25);
	wrline_(device, smsg, (ftnlen)255, (ftnlen)25);
	wrline_(device, " ", (ftnlen)255, (ftnlen)1);
    } else if (expl && msgsel_("EXPLAIN", (ftnlen)7)) {

/*        Obtain the explanatory text for the short error */
/*        message and output it: */

	getsms_(smsg, (ftnlen)25);
	expln_(smsg, xmsg, (ftnlen)25, (ftnlen)80);
	wrline_(device, xmsg, (ftnlen)255, (ftnlen)80);
	wrline_(device, " ", (ftnlen)255, (ftnlen)1);
    }
    if (long__ && msgsel_("LONG", (ftnlen)4)) {

/*        Extract the long message from global storage and */
/*        output it: */

	getlms_(lmsg, (ftnlen)1840);

/*        Get the number of words in the error message. */

	numwrd = wdcnt_(lmsg, (ftnlen)1840);
	s_copy(line, " ", (ftnlen)80, (ftnlen)1);
	start = 1;

/*        Format the words into output lines and display them as */
/*        needed. */

	i__1 = numwrd;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    nextwd_(lmsg, outwrd, lmsg, (ftnlen)1840, (ftnlen)1840, (ftnlen)
		    1840);
	    wrdlen = rtrim_(outwrd, (ftnlen)1840);
	    if (start + wrdlen <= 80) {
		s_copy(line + (start - 1), outwrd, 80 - (start - 1), (ftnlen)
			1840);
		start = start + wrdlen + 1;
	    } else {
		if (wrdlen <= 80) {

/*                 We had a short word, so just write the line and */
/*                 continue. */

		    wrline_(device, line, (ftnlen)255, (ftnlen)80);
		    start = wrdlen + 2;
		    s_copy(line, outwrd, (ftnlen)80, (ftnlen)1840);
		} else {

/*                 We got a very long word here, so we break it up and */
/*                 write it out. We fit as much of it as we an into line */
/*                 as possible before writing it. */

/*                 Get the remaining space. If START is > 1 we have at */
/*                 least one word already in the line, including it's */
/*                 trailing space, otherwise the line is blank. If line */
/*                 is empty, we have all of the space available. */

		    if (start > 1) {
			remain = 80 - start;
		    } else {
			remain = 80;
		    }

/*                 Now we stuff bits of the word into the output line */
/*                 until we're done, i.e., until we have a word part */
/*                 that is less than the output length. First, we */
/*                 check to see if there is a "significant" amount of */
/*                 room left in the current output line. If not, we */
/*                 write it and then begin stuffing the long word into */
/*                 output lines. */

		    if (remain < 10) {
			wrline_(device, line, (ftnlen)255, (ftnlen)80);
			s_copy(line, " ", (ftnlen)80, (ftnlen)1);
			remain = 80;
			start = 1;
		    }

/*                 Stuff the word a chunk at a time into output lines */
/*                 and write them. After writing a line, we clear the */
/*                 part of the long word that we just wrote, left */
/*                 justifying the remaining part before proceeding. */

		    while(wrdlen > 80) {
			s_copy(line + (start - 1), outwrd, 80 - (start - 1), 
				remain);
			wrline_(device, line, (ftnlen)255, (ftnlen)80);
			s_copy(outwrd, " ", remain, (ftnlen)1);
			ljust_(outwrd, outwrd, (ftnlen)1840, (ftnlen)1840);
			s_copy(line, " ", (ftnlen)80, (ftnlen)1);
			wrdlen -= remain;
			remain = 80;
			start = 1;
		    }

/*                 If we had a part of the long word left, get set up to */
/*                 append more words from the error message to the output */
/*                 line. If we finished the word, WRDLEN .EQ. 0, then */
/*                 START and LINE have already been initialized. */

		    if (wrdlen > 0) {
			start = wrdlen + 2;
			s_copy(line, outwrd, (ftnlen)80, (ftnlen)1840);
		    }
		}
	    }
	}

/*        We may need to write the remaining part of a line. */

	if (s_cmp(line, " ", (ftnlen)80, (ftnlen)1) != 0) {
	    wrline_(device, line, (ftnlen)255, (ftnlen)80);
	}
	wrline_(device, " ", (ftnlen)255, (ftnlen)1);
    }
    if (trace && msgsel_("TRACEBACK", (ftnlen)9)) {

/*        Extract the traceback from global storage and */
/*        output it: */

	trcdep_(&depth);
	if (depth > 0) {

/*           We know we'll be outputting some trace information. */
/*           So, write a line telling the reader what's coming. */

	    wrline_(device, "A traceback follows.  The name of the highest l"
		    "evel module is first.", (ftnlen)255, (ftnlen)68);

/*           While there are more names in the traceback */
/*           representation, we stuff them into output lines and */
/*           write the lines out when they are full. */

	    s_copy(line, " ", (ftnlen)80, (ftnlen)1);
	    remain = 80;
	    i__1 = depth;
	    for (index = 1; index <= i__1; ++index) {

/*              For each module name in the traceback representation, */
/*              retrieve module name and stuff it into one or more */
/*              lines for output. */

/*              Get a name and add the call order sign.  We */
/*              indicate calling order by a ' --> ' delimiter; e.g. */
/*              "A calls B" is indicated by 'A --> B'. */

		trcnam_(&index, name__, (ftnlen)32);
		length = lastnb_(name__, (ftnlen)32);

/*              If it's the first name, just put it into the output */
/*              line, otherwise, add the call order sign and put the */
/*              name into the output line. */

		if (index == 1) {
		    suffix_(name__, &c__0, line, (ftnlen)32, (ftnlen)80);
		    remain -= length;
		} else {

/*                 Add the calling order indicator, if it will fit. */
/*                 If not, write the line and put the indicator as */
/*                 the first thing on the next line. */

		    if (remain >= 4) {
			suffix_("-->", &c__1, line, (ftnlen)3, (ftnlen)80);
			remain += -4;
		    } else {
			wrline_(device, line, (ftnlen)255, (ftnlen)80);
			s_copy(line, "-->", (ftnlen)80, (ftnlen)3);
			remain = 77;
		    }

/*                 The name fits or it doesn't. If it does, just add */
/*                 it, if it doesn't, write it, then make the name */
/*                 the first thing on the next line. */

		    if (remain >= length) {
			suffix_(name__, &c__1, line, (ftnlen)32, (ftnlen)80);
			remain = remain - length - 1;
		    } else {
			wrline_(device, line, (ftnlen)255, (ftnlen)80);
			s_copy(line, name__, (ftnlen)80, (ftnlen)32);
			remain = 80 - length;
		    }
		}
	    }

/*           At this point, no more names are left in the */
/*           trace representation.  LINE may still contain */
/*           names, or part of a long name.  If it does, */
/*           we now write it out. */

	    if (s_cmp(line, " ", (ftnlen)80, (ftnlen)1) != 0) {
		wrline_(device, line, (ftnlen)255, (ftnlen)80);
	    }
	    wrline_(device, " ", (ftnlen)255, (ftnlen)1);
	}

/*        At this point, either we have output the trace */
/*        representation, or the trace representation was */
/*        empty. */

    }
    if (dfault && msgsel_("DEFAULT", (ftnlen)7)) {

/*        Output the default message: */

	for (i__ = 1; i__ <= 4; ++i__) {
	    wrline_(device, defmsg + ((i__1 = i__ - 1) < 4 && 0 <= i__1 ? 
		    i__1 : s_rnge("defmsg", i__1, "outmsg_", (ftnlen)987)) * 
		    80, (ftnlen)255, (ftnlen)80);
	}
	wrline_(device, " ", (ftnlen)255, (ftnlen)1);
    }

/*     At this point, we've output all of the enabled messages */
/*     that were specified in LIST.  At least one message that */
/*     was specified was enabled. */

/*     Write the ending border out: */

    wrline_(device, border, (ftnlen)255, (ftnlen)80);
    return 0;
} /* outmsg_ */

