/* lxname.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__255 = 255;
static integer c__0 = 0;

/* $Procedure LXNAME ( Lex names ) */
/* Subroutine */ int lxname_0_(int n__, char *hdchrs, char *tlchrs, char *
	string, integer *first, integer *last, integer *idspec, integer *
	nchar, ftnlen hdchrs_len, ftnlen tlchrs_len, ftnlen string_len)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer c__, headc[261], i__, l, nhead;
    extern integer cardi_(integer *);
    integer tailc[261];
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer ntail, tcpos;
    extern integer rtrim_(char *, ftnlen);
    integer hl, tl;
    extern /* Subroutine */ int scardi_(integer *, integer *), validi_(
	    integer *, integer *, integer *);
    extern integer bsrchi_(integer *, integer *, integer *);
    extern /* Subroutine */ int appndi_(integer *, integer *), sigerr_(char *,
	     ftnlen), chkout_(char *, ftnlen), setmsg_(char *, ftnlen), 
	    errint_(char *, integer *, ftnlen), ssizei_(integer *, integer *),
	     insrti_(integer *, integer *);
    extern logical return_(void);

/* $ Abstract */

/*     Umbrella routine for name scanning entry points. */

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

/*     None. */

/* $ Keywords */

/*     CHARACTER */
/*     PARSING */
/*     SCANNING */
/*     STRING */
/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  ENTRY POINTS */
/*     --------  ---  -------------------------------------------------- */
/*     HDCHRS     I   LXCSID */
/*     TLCHRS     I   LXCSID */
/*     STRING     I   LXIDNT */
/*     FIRST      I   LXIDNT */
/*     IDSPEC    I-O  LXDFID, LXCSID, LXIDNT */
/*     LAST       O   LXIDNT */
/*     NCHAR      O   LXIDNT */
/*     MXSPEC     P   LXDFID, LXCSID */
/*     LBCELL     P   LXIDNT, LXDFID, LXCSID */

/* $ Detailed_Input */

/*     See the entry points for descriptions of their inputs. */

/* $ Detailed_Output */

/*     See the entry points for descriptions of their outputs. */

/* $ Parameters */

/*     See the entry points for descriptions of their parameters. */

/* $ Exceptions */

/*     See the entry points for descriptions of the exceptions specific */
/*     to those entry points. */

/*     1)  If this routine is called directly, the error */
/*         SPICE(BOGUSENTRY) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Many computer languages include tokens that represent names. */
/*     Examples of names include procedure names and variable names. */
/*     The term `identifier' is generally used to indicate this type */
/*     of token. Rules for constructing identifiers vary from */
/*     language to language, but identifiers conforming to the */
/*     following rules are widely recognized: */

/*        1)  The first character of the identifier is a letter. */

/*        2)  The remaining characters are letters or numbers. */

/*        3)  The length of the identifier is less than some specified */
/*            limit. */

/*     This suite of routines has its own set of default rules for */
/*     forming identifiers. These rules are somewhat more liberal */
/*     than those listed above. Rule (1) above still holds, but */
/*     trailing characters may include letters, numbers, and the */
/*     special characters */

/*        $ */
/*        _  (underscore) */

/*     No mechanism for enforcing rule (3) is provided; this task is */
/*     left to the caller, since this routine would be unnecessarily */
/*     complicated by the need to construct diagnostic messages. */

/*     The entry point LXIDNT (Lex identifier) recognizes valid */
/*     identifier tokens, using either the default character sets */
/*     for the head and tail of the identifier, or character sets */
/*     specified in the last call to LXCSID. */

/*     In order to use this suite of routines to scan identifiers that */
/*     conform to the default rules, a program normally calls the entry */
/*     point LXDFID (Lex, default identifier specification) once to */
/*     obtain the default `identifier specification'. This specification */
/*     is an integer array in which the allowed head and tail character */
/*     sets are specified. This specification is then saved and supplied */
/*     to the entry point LXIDNT (Lex identifier) whenever LXIDNT is */
/*     called to scan an identifier. The entry point LXIDNT  recognizes */
/*     valid identifier tokens, using an input identifier specification */
/*     to decide which head and tail characters are allowed in an */
/*     identifier. */

/*     The scanning code using these routines might have the following */
/*     structure: */


/*              INTEGER               IDSPEC ( LBCELL : MXSPEC ) */
/*                 . */
/*                 . */
/*                 . */
/*        C */
/*        C     Initialize the identifier specification, using the */
/*        C     default: */
/*        C */
/*              CALL SSIZEI ( MXSPEC, IDSPEC ) */
/*              CALL LXDFID ( IDSPEC ) */
/*                 . */
/*                 . */
/*                 . */
/*        C */
/*        C     Scan string: */
/*        C */
/*              DO WHILE ( <more tokens> ) */
/*                       . */
/*                       . */
/*                       . */
/*                 IF ( <test for identifier> ) THEN */

/*                    CALL LXIDNT ( IDSPEC, STRING, FIRST, LAST, NCHARS ) */

/*                    IF ( NCHARS .GT. 0 ) THEN */

/*                       [Identifier was found--process result] */

/*                    ELSE */

/*                       [Token at starting at location FIRST was not */
/*                        an identifier--handle alternatives] */

/*                    END IF */

/*                 ELSE */

/*                    [ perform tests for other tokens ] */

/*                 END IF */

/*              END DO */


/*     It is possible to override the default rules by calling the */
/*     entry point LXCSID (Lex, custom identifier characters).  This */
/*     routine allows the caller to specify the precise set of */
/*     characters allowed as the first character (`head') of the */
/*     identifier, as well as those allowed in the remainder (`tail') */
/*     of the identifier. */

/*     If a custom identifier specification is desired, the call to */
/*     LXDFID in the pseudo code above would be replaced by a call to */
/*     LXCSID. After setting the strings HDCHRS and TLCHRS to contain, */
/*     respectively, the allowed head and tail identifier characters, the */
/*     following call would produce an identifier specification structure */
/*     IDSPEC representing these set of allowed characters. */

/*        CALL LXCSID ( HDCHRS, TLCHRS, IDSPEC ) */

/*     The array IDSPEC obtained from LXCSID would be used as input to */
/*     LXIDNT, instead of using the array obtained by calling LXDFID. */

/* $ Examples */

/*     1)  The following table illustrates the behavior of the scanning */
/*         entry point LXIDNT when the default identifier syntax is in */
/*         effect: */

/*         STRING CONTENTS             FIRST   LAST   NCHAR */
/*         ========================================================== */
/*         WHERE A LT B                1       5      5 */
/*         WHERE A LT B                7       7      1 */
/*         WHERE A.LT.B                7       7      1 */
/*         WHERE (A0)LT(B8)            8       9      2 */
/*         WHERE A0$LT_B7              7       14     8 */
/*         WHERE A LT B                12      12     1 */
/*         WHERE A .LT. B              9       8      0 */


/*     2)  The following table illustrates the behavior of the scanning */
/*         entry point LXIDNT when a custom identifier syntax is used. */
/*         The call */

/*            CALL LXCSID ( HDCHRS, TLCHRS, IDSPEC ) */

/*         where */

/*            HDCHRS = 'abcdefghijklmnopqrstuvwxyz' */

/*         and */

/*            TLCHRS = 'abcdefghijklmnopqrstuvwxyz012345.' */

/*        will produce an identifier specification IDSPEC that, */
/*        when supplied as an input to LXIDNT, will cause LXIDNT */
/*        to perform in accordance with the table shown below: */


/*         STRING CONTENTS             FIRST   LAST   NCHAR */
/*         ========================================================== */
/*         WHERE A LT B                1       0      0 */
/*         where a lt b                1       5      5 */
/*         WHERE a LT b                7       7      1 */
/*         WHERE a.LT.b                7       8      2 */
/*         WHERE (a0)LT(b8)            14      14     1 */
/*         WHERE (a0)LT(b5)            14      15     2 */
/*         WHERE a0.lt.b8              7       13     7 */
/*         WHERE a0$lt_b7              7       8      2 */
/*         where a .lt. b              9       12     4 */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.1, 10-FEB-2014 (BVS) */

/*        Added LBCELL to the $Brief_I/O section. */

/* -    SPICELIB Version 1.0.0, 25-OCT-1995 (NJB) */

/* -& */
/* $ Index_Entries */

/*     scan name tokens --- umbrella */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     IDSPEC parameters: */


/*     Local variables */


/*     Standard SPICE error handling. */

    switch(n__) {
	case 1: goto L_lxidnt;
	case 2: goto L_lxdfid;
	case 3: goto L_lxcsid;
	}

    if (return_()) {
	return 0;
    } else {
	chkin_("LXNAME", (ftnlen)6);
    }
    sigerr_("SPICE(BOGUSENTRY)", (ftnlen)17);
    chkout_("LXNAME", (ftnlen)6);
    return 0;
/* $Procedure LXIDNT ( Lex identifier ) */

L_lxidnt:
/* $ Abstract */

/*     Scan an identifier, starting from a specified character */
/*     position. */

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

/*     None. */

/* $ Keywords */

/*     CHARACTER */
/*     PARSING */
/*     SCANNING */
/*     STRING */
/*     UTILITY */

/* $ Declarations */

/*     IMPLICIT NONE */

/*     INTEGER               LBCELL */
/*     PARAMETER           ( LBCELL = -5 ) */

/*     INTEGER               IDSPEC ( LBCELL : * ) */
/*     CHARACTER*(*)         STRING */
/*     INTEGER               FIRST */
/*     INTEGER               LAST */
/*     INTEGER               NCHAR */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     IDSPEC     I   Identifier character specification. */
/*     STRING     I   String to be scanned. */
/*     FIRST      I   Character position at which to start scanning. */
/*     LAST       O   Character position of end of token. */
/*     NCHAR      O   Number of characters in token. */
/*     LBCELL     P   The SPICE cell lower bound. */

/* $ Detailed_Input */

/*     IDSPEC   is an integer cell containing a specification of */
/*              the head and tail identifier character sets to be */
/*              used in scanning the input argument STRING. IDSPEC */
/*              should be obtained by calling LXDFID or LXCSID. */
/*              The structure of IDSPEC is not part of the */
/*              specification of this routine suite and should not */
/*              be relied upon by calling code. */

/*     STRING   is a character string that may contain an */
/*              `identifier' starting at the character position */
/*              indicated by the input argument FIRST (see */
/*              below). Identifier tokens are sequences of */
/*              characters that represent names. Syntactically, an */
/*              identifier is a sequence of characters that begins */
/*              with a character belonging to a set of valid `head' */
/*              characters and is followed by zero or more */
/*              characters belonging to a set of valid `tail' */
/*              characters. */

/*     FIRST    is the character position at which the routine */
/*              is to start scanning an identifier. Note */
/*              that the character STRING(FIRST:FIRST) must be a */
/*              valid head character if an identifier is to */
/*              be found; this routine does *not* attempt to locate */
/*              the first identifier following the position */
/*              FIRST. */

/* $ Detailed_Output */

/*     LAST     is the last character position such that the */
/*              substring STRING(FIRST:LAST) is an identifier, if */
/*              such a substring exists. Otherwise, the */
/*              returned value of LAST is FIRST-1. */

/*     NCHAR    is the length of the identifier found by this */
/*              routine, if such a token exists. If an identifier */
/*              is not found, the returned value of NCHAR is */
/*              zero. */

/* $ Parameters */

/*     LBCELL   is the SPICE cell lower bound. */

/* $ Exceptions */

/*     Error free. */

/*     1)  If the input argument FIRST is less than 1 or greater than */
/*         LEN(STRING)-1, the returned value of LAST is FIRST-1, and the */
/*         returned value of NCHAR is zero. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The default syntax rules for valid identifiers are specified in */
/*     the $Particulars section of the umbrella routine LXNAME. These */
/*     rules may be overridden by calling LXCSID. */

/* $ Examples */

/*     See the $Examples section of the umbrella routine LXNAME. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.1, 10-FEB-2014 (BVS) */

/*        Added LBCELL to the $Declarations, $Brief_I/O, and $Parameters */
/*        sections. */

/* -    SPICELIB Version 1.0.0, 25-OCT-1995 (NJB) */

/* -& */
/* $ Index_Entries */

/*     scan identifiers */

/* -& */

/*     No check-in required; this entry point is error-free. */


/*     Save the length of the non-blank prefix of the input string. */

    l = rtrim_(string, string_len);

/*     Handle the cases in which we can tell right away that */
/*     no token can be found. */

    if (*first < 1 || *first > l) {
	*last = *first - 1;
	*nchar = 0;
	return 0;
    }

/*     In order for there to be a match, the character at position */
/*     FIRST must be in the head character set. */

    nhead = idspec[6];
    c__ = *(unsigned char *)&string[*first - 1];
    i__ = bsrchi_(&c__, &nhead, &idspec[8]);
    if (i__ == 0) {
	*last = *first - 1;
	*nchar = 0;
	return 0;
    }

/*     We have an identifier.  The remaining question is how long it is. */
/*     Each subsequent character that is in the tail character set is */
/*     considered to be part of the identifier. */

    *nchar = 1;
    *last = *first;
    ntail = idspec[7];
    tcpos = nhead + 3;
    while(*last < l) {
	i__1 = *last;
	c__ = *(unsigned char *)&string[i__1];
	i__ = bsrchi_(&c__, &ntail, &idspec[tcpos + 5]);
	if (i__ == 0) {
	    return 0;
	} else {
	    ++(*nchar);
	    ++(*last);
	}
    }
    return 0;
/* $Procedure LXDFID ( Lex, default identifier characters ) */

L_lxdfid:
/* $ Abstract */

/*     Return the default specification for the characters that may */
/*     appear in an identifier. */

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

/*     None. */

/* $ Keywords */

/*     CHARACTER */
/*     PARSING */
/*     SCANNING */
/*     STRING */
/*     UTILITY */

/* $ Declarations */

/*     IMPLICIT NONE */

/*     INTEGER               MXSPEC */
/*     PARAMETER           ( MXSPEC = 512 ) */

/*     INTEGER               LBCELL */
/*     PARAMETER           ( LBCELL = -5 ) */

/*     INTEGER               IDSPEC ( LBCELL : * ) */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     IDSPEC    I-O  Identifier character specification. */
/*     MXSPEC     P   Recommended size for declaration of IDSPEC. */
/*     LBCELL     P   The SPICE cell lower bound. */

/* $ Detailed_Input */

/*     IDSPEC   is an integer cell. The caller must initialize */
/*              IDSPEC as a cell, and should use MXSPEC as the size */
/*              of IDSPEC. */

/* $ Detailed_Output */

/*     IDSPEC   is an integer cell containing a specification of */
/*              the head and tail identifier character sets to be */
/*              used the entry point LXIDNT in scanning strings. */

/* $ Parameters */

/*     MXSPEC   is the recommended size for the declaration of */
/*              IDSPEC; the caller should declare IDSPEC as shown: */

/*                 INTEGER       IDSPEC ( LBCELL : MXSPEC ) */

/*              The caller should also initialize IDSPEC as shown: */

/*                 CALL SSIZEI ( MXSPEC, IDSPEC ) */

/*     LBCELL   is the SPICE cell lower bound. */

/* $ Exceptions */

/*     1)  If IDSPEC is not properly initialized on input, or if its size */
/*         is too small, an error is signaled by a routine in the call */
/*         tree of this routine. IDSPEC is undefined on output in this */
/*         case. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine allows a calling program to obtain the default set of */
/*     allowed patterns for identifiers recognized by LXIDNT. */

/*     Normally, this routine should be called once during the calling */
/*     program's initialization. */

/* $ Examples */

/*     See the $Examples section of the umbrella routine LXNAME. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 27-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.1, 10-FEB-2014 (BVS) */

/*        Added LBCELL to the $Declarations, $Brief_I/O, and $Parameters */
/*        sections. */

/* -    SPICELIB Version 1.0.0, 25-OCT-1995 (NJB) */

/* -& */
/* $ Index_Entries */

/*     return default allowed identifier characters */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("LXDFID", (ftnlen)6);
    }

/*     Initialize our head and tail character sets. */

    ssizei_(&c__255, headc);
    ssizei_(&c__255, tailc);

/*     Fill in the head and tail character arrays with their default */
/*     values.  User integer codes for the characters. */

    for (i__ = 1; i__ <= 26; ++i__) {
	headc[(i__1 = i__ + 5) < 261 && 0 <= i__1 ? i__1 : s_rnge("headc", 
		i__1, "lxname_", (ftnlen)793)] = 'A' + i__ - 1;
	headc[(i__1 = i__ + 31) < 261 && 0 <= i__1 ? i__1 : s_rnge("headc", 
		i__1, "lxname_", (ftnlen)794)] = 'a' + i__ - 1;
	tailc[(i__1 = i__ + 5) < 261 && 0 <= i__1 ? i__1 : s_rnge("tailc", 
		i__1, "lxname_", (ftnlen)795)] = headc[(i__2 = i__ + 5) < 261 
		&& 0 <= i__2 ? i__2 : s_rnge("headc", i__2, "lxname_", (
		ftnlen)795)];
	tailc[(i__1 = i__ + 31) < 261 && 0 <= i__1 ? i__1 : s_rnge("tailc", 
		i__1, "lxname_", (ftnlen)796)] = headc[(i__2 = i__ + 31) < 
		261 && 0 <= i__2 ? i__2 : s_rnge("headc", i__2, "lxname_", (
		ftnlen)796)];
    }
    for (i__ = 1; i__ <= 10; ++i__) {
	tailc[(i__1 = i__ + 57) < 261 && 0 <= i__1 ? i__1 : s_rnge("tailc", 
		i__1, "lxname_", (ftnlen)801)] = '0' + i__ - 1;
    }
    tailc[68] = '$';
    tailc[69] = '_';
    nhead = 52;
    ntail = 64;

/*     Turn the arrays into integer sets. */

    validi_(&c__255, &nhead, headc);
    validi_(&c__255, &ntail, tailc);

/*     Create the output specification IDSPEC.  This is a cell */
/*     containing, in order, */

/*        - the number of head characters */
/*        - the number of tail characters */
/*        - integer codes for the head characters */
/*        - integer codes for the tail characters */

/*     IDSPEC is assumed to be initialized. */


    scardi_(&c__0, idspec);
    appndi_(&nhead, idspec);
    appndi_(&ntail, idspec);
    i__1 = nhead;
    for (i__ = 1; i__ <= i__1; ++i__) {
	appndi_(&headc[(i__2 = i__ + 5) < 261 && 0 <= i__2 ? i__2 : s_rnge(
		"headc", i__2, "lxname_", (ftnlen)834)], idspec);
    }
    i__1 = ntail;
    for (i__ = 1; i__ <= i__1; ++i__) {
	appndi_(&tailc[(i__2 = i__ + 5) < 261 && 0 <= i__2 ? i__2 : s_rnge(
		"tailc", i__2, "lxname_", (ftnlen)838)], idspec);
    }
    chkout_("LXDFID", (ftnlen)6);
    return 0;
/* $Procedure LXCSID ( Lex, custom identifier characters ) */

L_lxcsid:
/* $ Abstract */

/*     Set the acceptable characters that may appear in an identifier */
/*     token. */

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

/*     None. */

/* $ Keywords */

/*     CHARACTER */
/*     PARSING */
/*     SCANNING */
/*     STRING */
/*     UTILITY */

/* $ Declarations */

/*     IMPLICIT NONE */

/*     INTEGER               MXSPEC */
/*     PARAMETER           ( MXSPEC = 512 ) */

/*     INTEGER               LBCELL */
/*     PARAMETER           ( LBCELL = -5 ) */

/*     CHARACTER*(*)         HDCHRS */
/*     CHARACTER*(*)         TLCHRS */
/*     INTEGER               IDSPEC ( LBCELL : * ) */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HDCHRS     I   Allowed head characters for identifiers. */
/*     TLCHRS     I   Allowed tail characters for identifiers. */
/*     IDSPEC    I-O  Identifier character specification. */
/*     MXSPEC     P   Recommended size for declaration of IDSPEC. */
/*     LBCELL     P   The SPICE cell lower bound. */

/* $ Detailed_Input */

/*     HDCHRS   is a string containing the set of characters */
/*              allowed as the first (`head') character of an */
/*              identifier token. Case is significant; if both */
/*              upper and lower case instances of a letter are */
/*              allowed, they must both be listed. White space is */
/*              ignored. Non-printing characters are not allowed. */

/*     TLCHRS   is a string containing the set of characters */
/*              allowed as tail characters (characters following */
/*              the head character) of an identifier token. Case */
/*              is significant; white space is ignored. */
/*              Non-printing characters are not allowed. */

/*     IDSPEC   is an integer cell. The caller must initialize */
/*              IDSPEC as a cell, and should use MXSPEC as the size */
/*              of IDSPEC. */

/* $ Detailed_Output */

/*     IDSPEC   is an integer cell containing a specification of */
/*              the head and tail identifier character sets to be */
/*              used the entry point LXIDNT in scanning strings. */
/*              The caller must initialize IDSPEC as a cell, and */
/*              should use MXSPEC as the size of IDSPEC. */

/* $ Parameters */

/*     MXSPEC   is the recommended size for the declaration of */
/*              IDSPEC; the caller should declare IDSPEC as shown: */

/*                 INTEGER       IDSPEC ( LBCELL : MXSPEC ) */

/*              The caller should also initialize IDSPEC as shown: */

/*                 CALL SSIZEI ( MXSPEC, IDSPEC ) */

/*     LBCELL   is the SPICE cell lower bound. */

/* $ Exceptions */

/*     1)  If non-printing characters are found in either of the input */
/*         arguments HDCHRS or TLCHRS, the error SPICE(NONPRINTINGCHARS) */
/*         is signaled. The set of allowed identifier characters is not */
/*         modified. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine allows a calling program to customize the set of */
/*     allowed patterns for identifiers recognized by LXIDNT. */

/*     Normally, this routine should be called once during the calling */
/*     program's initialization, if this routine is called at all. */

/* $ Examples */

/*     See the $Examples section of the umbrella routine LXNAME. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 27-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.1, 10-FEB-2014 (BVS) */

/*        Added LBCELL to the $Declarations, $Brief_I/O, and $Parameters */
/*        sections. */

/* -    SPICELIB Version 1.0.0, 25-OCT-1995 (NJB) */

/* -& */
/* $ Index_Entries */

/*     customize allowed identifier characters for lexing */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("LXCSID", (ftnlen)6);
    }

/*     Initialize our head and tail character sets, every time. */

    ssizei_(&c__255, headc);
    ssizei_(&c__255, tailc);

/*     Check the inputs before proceeding. */

    hl = rtrim_(hdchrs, hdchrs_len);
    tl = rtrim_(tlchrs, tlchrs_len);
    i__1 = hl;
    for (i__ = 1; i__ <= i__1; ++i__) {
	c__ = *(unsigned char *)&hdchrs[i__ - 1];
	if (c__ < 32 || c__ > 126) {
	    setmsg_("The character having integer code # in position # of th"
		    "e head character string HDCHRS is a non-printing charact"
		    "er.", (ftnlen)114);
	    errint_("#", &c__, (ftnlen)1);
	    errint_("#", &i__, (ftnlen)1);
	    sigerr_("SPICE(NONPRINTINGCHARS)", (ftnlen)23);
	    chkout_("LXCSID", (ftnlen)6);
	    return 0;
	}
    }
    i__1 = tl;
    for (i__ = 1; i__ <= i__1; ++i__) {
	c__ = *(unsigned char *)&tlchrs[i__ - 1];
	if (c__ < 32 || c__ > 126) {
	    setmsg_("The character having integer code # in position # of th"
		    "e tail character string TLCHRS is a non-printing charact"
		    "er.", (ftnlen)114);
	    errint_("#", &c__, (ftnlen)1);
	    errint_("#", &i__, (ftnlen)1);
	    sigerr_("SPICE(NONPRINTINGCHARS)", (ftnlen)23);
	    chkout_("LXCSID", (ftnlen)6);
	    return 0;
	}
    }

/*     The characters of HDCHRS become the set of acceptable */
/*     characters for the head identifier character---all except */
/*     the blanks.  Same deal goes for the tail characters. */

    i__1 = hl;
    for (i__ = 1; i__ <= i__1; ++i__) {
	c__ = *(unsigned char *)&hdchrs[i__ - 1];
	if (c__ != 32) {
	    insrti_(&c__, headc);
	}
    }
    nhead = cardi_(headc);
    i__1 = tl;
    for (i__ = 1; i__ <= i__1; ++i__) {
	c__ = *(unsigned char *)&tlchrs[i__ - 1];
	if (c__ != 32) {
	    insrti_(&c__, tailc);
	}
    }
    ntail = cardi_(tailc);

/*     Create the output specification IDSPEC.  This is a cell */
/*     containing, in order, */

/*        - the number of head characters */
/*        - the number of tail characters */
/*        - integer codes for the head characters */
/*        - integer codes for the tail characters */

/*     IDSPEC is assumed to be initialized. */


    scardi_(&c__0, idspec);
    appndi_(&nhead, idspec);
    appndi_(&ntail, idspec);
    i__1 = nhead;
    for (i__ = 1; i__ <= i__1; ++i__) {
	appndi_(&headc[(i__2 = i__ + 5) < 261 && 0 <= i__2 ? i__2 : s_rnge(
		"headc", i__2, "lxname_", (ftnlen)1132)], idspec);
    }
    i__1 = ntail;
    for (i__ = 1; i__ <= i__1; ++i__) {
	appndi_(&tailc[(i__2 = i__ + 5) < 261 && 0 <= i__2 ? i__2 : s_rnge(
		"tailc", i__2, "lxname_", (ftnlen)1136)], idspec);
    }
    chkout_("LXCSID", (ftnlen)6);
    return 0;
} /* lxname_ */

/* Subroutine */ int lxname_(char *hdchrs, char *tlchrs, char *string, 
	integer *first, integer *last, integer *idspec, integer *nchar, 
	ftnlen hdchrs_len, ftnlen tlchrs_len, ftnlen string_len)
{
    return lxname_0_(0, hdchrs, tlchrs, string, first, last, idspec, nchar, 
	    hdchrs_len, tlchrs_len, string_len);
    }

/* Subroutine */ int lxidnt_(integer *idspec, char *string, integer *first, 
	integer *last, integer *nchar, ftnlen string_len)
{
    return lxname_0_(1, (char *)0, (char *)0, string, first, last, idspec, 
	    nchar, (ftnint)0, (ftnint)0, string_len);
    }

/* Subroutine */ int lxdfid_(integer *idspec)
{
    return lxname_0_(2, (char *)0, (char *)0, (char *)0, (integer *)0, (
	    integer *)0, idspec, (integer *)0, (ftnint)0, (ftnint)0, (ftnint)
	    0);
    }

/* Subroutine */ int lxcsid_(char *hdchrs, char *tlchrs, integer *idspec, 
	ftnlen hdchrs_len, ftnlen tlchrs_len)
{
    return lxname_0_(3, hdchrs, tlchrs, (char *)0, (integer *)0, (integer *)0,
	     idspec, (integer *)0, hdchrs_len, tlchrs_len, (ftnint)0);
    }

