/* excess.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__0 = 0;

/* $Procedure EXCESS ( Report an excess of elements in a cell ) */
/* Subroutine */ int excess_(integer *number, char *struct__, ftnlen 
	struct_len)
{
    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    char error[320];
    extern /* Subroutine */ int prefix_(char *, integer *, char *, ftnlen, 
	    ftnlen), chkout_(char *, ftnlen), setmsg_(char *, ftnlen), 
	    suffix_(char *, integer *, char *, ftnlen, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int intstr_(integer *, char *, ftnlen);

/* $ Abstract */

/*     Set the long error message so as to indicate the number of excess */
/*     elements encountered by a routine operating on cells or on data */
/*     structures based on cells. */

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

/*     CELLS */

/* $ Keywords */

/*     CELLS */
/*     ERROR */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     NUMBER     I   Number of excess elements. */
/*     STRUCT     I   Name of the data structure. */

/* $ Detailed_Input */

/*     NUMBER   is the number of excess elements encountered. */
/*              This may be zero or negative, which indicates */
/*              no excess. */

/*     STRUCT   is the name of the data structure being manipulated. */
/*              Typically, this is one of the strings: 'cell', 'set', */
/*              or 'symbol table'. However, it may be any character */
/*              string. STRUCT should NOT end in a period. */
/*              The period at the end of the message is supplied */
/*              automatically. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  This routine does not detect any errors. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine is part of the SPICELIB error handling mechanism. */

/*     EXCESS sets the long error message. The message has the form: */

/*                 An excess of <NUMBER> element(s) could */
/*                 not be accommodated in the output <STRUCT>. */

/*     Leading and trailing blanks in STRUCT are removed. If there is */
/*     no excess (NUMBER is zero or negative), then is blank. */

/* $ Examples */

/*     The response of EXCESS to a variety of inputs is illustrated */
/*     below. */

/*           NUMBER = 1 */
/*           STRUCT = 'set' */
/*           ERROR  = 'An excess of 1 element could not */
/*                     be accommodated in the output set.' */

/*           NUMBER = 5 */
/*           STRUCT = 'stack' */
/*           ERROR  =  An excess of 5 elements could not */
/*                     be accommodated in the output stack.' */

/*           NUMBER = 0 */
/*           STRUCT = */
/*           ERROR  = ' ' */

/*           NUMBER = -6 */
/*           STRUCT = */
/*           ERROR  = ' ' */

/*     In particular, note that EXCESS does not set the long error */
/*     message when the number of excess elements is not positive. Also, */
/*     the singular 'element' is used for an excess of one, while */
/*     the plural 'elements' is used for all other positive excesses. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     C.A. Curzon        (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 13-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU) (NJB) */

/* -& */
/* $ Index_Entries */

/*     report an excess of elements in a cell */

/* -& */
/* $ Revisions */

/* -    Beta Version 2.0.0, 11-JAN-1989 (NJB) */

/*        Sets the long error message directly. No longer returns */
/*        an error message. Message no longer contains name of */
/*        routine which detected the error. */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Set up the error processing. */

    if (return_()) {
	return 0;
    }
    chkin_("EXCESS", (ftnlen)6);

/*     If there is no excess, don't report one. */

    if (*number > 0) {

/*        Begin with the number. We will build the rest of the */
/*        message around it. */

	intstr_(number, error, (ftnlen)320);

/*        A short blurb goes in front of the number. */

	prefix_("An excess of", &c__1, error, (ftnlen)12, (ftnlen)320);

/*        Singular or plural? */

	if (*number == 1) {
	    suffix_("element", &c__1, error, (ftnlen)7, (ftnlen)320);
	} else {
	    suffix_("elements", &c__1, error, (ftnlen)8, (ftnlen)320);
	}

/*        Another short blurb. */

	suffix_("could not be accommodated in the output", &c__1, error, (
		ftnlen)39, (ftnlen)320);

/*        And the name of the structure. */

	suffix_(struct__, &c__1, error, struct_len, (ftnlen)320);

/*        And a period at the end, to complete the sentence. */

	suffix_(".", &c__0, error, (ftnlen)1, (ftnlen)320);

/*        Set the long error message: */

	setmsg_(error, (ftnlen)320);
    } else {
	s_copy(error, " ", (ftnlen)320, (ftnlen)1);
    }
    chkout_("EXCESS", (ftnlen)6);
    return 0;
} /* excess_ */

