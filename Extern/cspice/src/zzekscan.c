/* zzekscan.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__512 = 512;
static integer c__2 = 2;
static integer c__29 = 29;
static integer c__13 = 13;

/* $Procedure      ZZEKSCAN ( EK, scan query ) */
/* Subroutine */ int zzekscan_(char *query, integer *maxntk, integer *maxnum, 
	integer *ntoken, integer *tokens, integer *lxbegs, integer *lxends, 
	integer *values, doublereal *numvls, char *chrbuf, integer *chbegs, 
	integer *chends, logical *scnerr, char *errmsg, ftnlen query_len, 
	ftnlen chrbuf_len, ftnlen errmsg_len)
{
    /* Initialized data */

    static char keywds[32*29] = "ALL                             " "AND     "
	    "                        " "ASC                             " 
	    "AVG                             " "BETWEEN                     "
	    "    " "BY                              " "COUNT                 "
	    "          " "DESC                            " "DISTINCT        "
	    "                " "EQ                              " "FROM      "
	    "                      " "GE                              " "GROU"
	    "P                           " "GT                              " 
	    "HAVING                          " "IS                          "
	    "    " "LE                              " "LIKE                  "
	    "          " "LT                              " "MAX             "
	    "                " "MIN                             " "NE        "
	    "                      " "NOT                             " "NULL"
	    "                            " "OR                              " 
	    "ORDER                           " "SELECT                      "
	    "    " "SUM                             " "WHERE                 "
	    "          ";
    static integer kwvals[29] = { 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,
	    18,19,20,21,22,23,24,25,26,27,28,29 };
    static char spcstr[2*13] = "!=" "^=" "<>" "<=" ">=" "< " "> " "= " "( " 
	    ") " ", " ". " "* ";
    static integer spctok[13] = { 1,1,1,1,1,1,1,1,6,7,8,9,10 };
    static integer spcval[13] = { 22,22,22,17,12,19,14,10,0,0,0,0,0 };
    static logical pass1 = TRUE_;

    /* System generated locals */
    integer i__1, i__2;
    char ch__1[1], ch__2[1], ch__3[1];

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer i_len(char *, ftnlen), s_cmp(char *, char *, ftnlen, ftnlen), 
	    s_rnge(char *, integer, char *, integer);

    /* Local variables */
    static integer last, cptr, room, i__, j, l;
    extern /* Subroutine */ int chkin_(char *, ftnlen), ucase_(char *, char *,
	     ftnlen, ftnlen);
    extern logical beint_(char *, ftnlen);
    extern /* Subroutine */ int repmc_(char *, char *, char *, char *, ftnlen,
	     ftnlen, ftnlen, ftnlen), repmi_(char *, char *, integer *, char *
	    , ftnlen, ftnlen, ftnlen);
    static integer state;
    extern integer rtrim_(char *, ftnlen);
    static integer nnums, nstrs, chcard;
    extern /* Subroutine */ int lx4num_(char *, integer *, integer *, integer 
	    *, ftnlen);
    extern integer bsrchc_(char *, integer *, char *, ftnlen, ftnlen);
    static integer idspec[518];
    extern integer isrchc_(char *, integer *, char *, ftnlen, ftnlen);
    static char hdchrs[80];
    static integer nchars, length;
    extern integer frstpc_(char *, ftnlen);
    extern logical return_(void);
    static char tlchrs[80], tquery[2000];
    extern /* Subroutine */ int ssizei_(integer *, integer *), lxcsid_(char *,
	     char *, integer *, ftnlen, ftnlen), chkout_(char *, ftnlen), 
	    lxqstr_(char *, char *, integer *, integer *, integer *, ftnlen, 
	    ftnlen), parsqs_(char *, char *, char *, integer *, logical *, 
	    char *, integer *, ftnlen, ftnlen, ftnlen, ftnlen), prefix_(char *
	    , integer *, char *, ftnlen, ftnlen), nparsd_(char *, doublereal *
	    , char *, integer *, ftnlen, ftnlen), lxidnt_(integer *, char *, 
	    integer *, integer *, integer *, ftnlen), suffix_(char *, integer 
	    *, char *, ftnlen, ftnlen);
    static char chr[1];
    static integer ptr;

/* $ Abstract */

/*     Scan tokens in an EK query. */

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

/*     EK */

/* $ Keywords */

/*     EK */
/*     PARSE */

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


/*     Include Section:  EK Keyword Code Parameters */

/*        ekkeyw.inc  Version 4    24-JAN-1995 (NJB) */



/*     The EK query language keywords and codes are: */

/*        ALL */
/*        AND */
/*        ASC */
/*        AVG */
/*        BETWEEN */
/*        BY */
/*        COUNT */
/*        DESC */
/*        DISTINCT */
/*        EQ */
/*        FROM */
/*        GE */
/*        GROUP */
/*        GT */
/*        HAVING */
/*        IS */
/*        LE */
/*        LT */
/*        LIKE */
/*        MAX */
/*        MIN */
/*        NE */
/*        NOT */
/*        NULL */
/*        OR */
/*        ORDER */
/*        SELECT */
/*        SUM */
/*        WHERE */


/*     End Include Section:  EK Keyword Code Parameters */

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


/*     Include Section:  EK Token Code Parameters */

/*        ektokn.inc  Version 2    25-JAN-1995 (NJB) */

/*           Updated to distinguish between special characters. */


/*        ektokn.inc  Version 1    05-DEC-1994 (NJB) */


/*     The EK query language tokens and codes are: */

/*        <keyword> */
/*        <identifier> */
/*        <integer> */
/*        <d.p. number> */
/*        <quoted string> */
/*        <left parenthesis> */
/*        <right parenthesis> */
/*        <comma> */
/*        <period> */
/*        <end of query> */



/*     End Include Section:  EK Token Code Parameters */

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


/*     Include Section:  EK Query Limit Parameters */

/*        ekqlimit.inc  Version 3    16-NOV-1995 (NJB) */

/*           Parameter MAXCON increased to 1000. */

/*        ekqlimit.inc  Version 2    01-AUG-1995 (NJB) */

/*           Updated to support SELECT clause. */


/*        ekqlimit.inc  Version 1    07-FEB-1995 (NJB) */


/*     These limits apply to character string queries input to the */
/*     EK scanner.  This limits are part of the EK system's user */
/*     interface:  the values should be advertised in the EK required */
/*     reading document. */


/*     Maximum length of an input query:  MAXQRY.  This value is */
/*     currently set to twenty-five 80-character lines. */


/*     Maximum number of columns that may be listed in the */
/*     `order-by clause' of a query:  MAXSEL.  MAXSEL = 50. */


/*     Maximum number of tables that may be listed in the `FROM */
/*     clause' of a query: MAXTAB. */


/*     Maximum number of relational expressions that may be listed */
/*     in the `constraint clause' of a query: MAXCON. */

/*     This limit applies to a query when it is represented in */
/*     `normalized form': that is, the constraints have been */
/*     expressed as a disjunction of conjunctions of relational */
/*     expressions. The number of relational expressions in a query */
/*     that has been expanded in this fashion may be greater than */
/*     the number of relations in the query as orginally written. */
/*     For example, the expression */

/*             ( ( A LT 1 ) OR ( B GT 2 ) ) */
/*        AND */
/*             ( ( C NE 3 ) OR ( D EQ 4 ) ) */

/*     which contains 4 relational expressions, expands to the */
/*     equivalent normalized constraint */

/*             (  ( A LT 1 ) AND ( C NE 3 )  ) */
/*        OR */
/*             (  ( A LT 1 ) AND ( D EQ 4 )  ) */
/*        OR */
/*             (  ( B GT 2 ) AND ( C NE 3 )  ) */
/*        OR */
/*             (  ( B GT 2 ) AND ( D EQ 4 )  ) */

/*     which contains eight relational expressions. */



/*     MXJOIN is the maximum number of tables that can be joined. */


/*     MXJCON is the maximum number of join constraints allowed. */


/*     Maximum number of order-by columns that may be used in the */
/*     `order-by clause' of a query: MAXORD. MAXORD = 10. */


/*     Maximum number of tokens in a query: 500. Tokens are reserved */
/*     words, column names, parentheses, and values. Literal strings */
/*     and time values count as single tokens. */


/*     Maximum number of numeric tokens in a query: */


/*     Maximum total length of character tokens in a query: */


/*     Maximum length of literal string values allowed in queries: */
/*     MAXSTR. */


/*     End Include Section:  EK Query Limit Parameters */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     QUERY      I   Query specifying events to be found. */
/*     MAXNTK     I   Maximum number of tokens to return. */
/*     MAXNUM     I   Maximum number of numeric tokens allowed. */
/*     NTOKEN     O   Number of tokens returned. */
/*     TOKENS     O   Code numbers of identified tokens. */
/*     LXBEGS, */
/*     LXENDS     O   Start and end locations of lexemes in query. */
/*     VALUES     O   Token values or value pointers, as needed. */
/*     NUMVLS     O   Buffer containing values of numeric tokens. */
/*     CHRBUF     O   Buffer containing string tokens and identifiers. */
/*     CHBEGS     O   Begin locations of string tokens in CHRBUF. */
/*     CHENDS     O   End locations of string tokens in CHRBUF. */
/*     SCNERR     O   Flag indicating whether query parsed correctly. */
/*     ERRMSG     O   Scan error description. */

/* $ Detailed_Input */

/*     QUERY          is character string containing an EK query.  See */
/*                    the header of the subroutine EKFIND for a */
/*                    detailed description of the EK query language. */

/*     MAXNTK         is the maximum number of tokens that may occur */
/*                    in QUERY. */

/*     MAXNUM         is the maximum number of tokens representing */
/*                    numeric values that may occur in QUERY. */

/* $ Detailed_Output */

/*     NTOKEN         is the number of tokens found in the input QUERY. */
/*                    This number will be less than or equal to MAXNTK. */

/*     TOKENS         is an array of codes for the tokens found in QUERY. */
/*                    The parameter values for these codes are not part */
/*                    of the EKSCAN specification; however, these values */
/*                    must be kept consistent with those used by EKPARS. */
/*                    The caller of EKSCAN should declare TOKENS with */
/*                    dimension MAXNTK. */

/*     LXBEGS, */
/*     LXENDS         are, respectively, arrays of begin and end pointers */
/*                    for the lexemes occurring in QUERY. Lexemes are */
/*                    the strings in QUERY that correspond to tokens. For */
/*                    example, '4.9D0' and '3' are both lexemes that map */
/*                    to the token <number>. */

/*     VALUES         is an array of token values.  The Ith element of */
/*                    VALUES refers to the Ith token. */

/*                    If the Ith token is a number, the Ith element of */
/*                    VALUES is a pointer into the NUMVLS array where */
/*                    the value of the number is stored.  The Ith token */
/*                    code indicates whether the number was a signed */
/*                    integer or d.p. number. */

/*                    If the Ith token is a keyword, the Ith element of */
/*                    VALUES is the code for that keyword. */

/*                    If the Ith token is a quoted string, the Ith */
/*                    element of VALUES is the common index in the arrays */
/*                    CHBEGS and CHENDS where the begin and end positions */
/*                    in CHRBUF of the parsed identifier are stored. */
/*                    Identifiers are converted to upper case when they */
/*                    are scanned. */

/*                    If the Ith token is an identifier, the Ith element */
/*                    of VALUES has the same role as in the case of a */
/*                    quoted string. */

/*                    If the Ith token is a special character, the Ith */
/*                    element of values is undefined; the value of */
/*                    TOKENS is the value of ICHAR() applied to the */
/*                    character. */

/*                    The caller of EKSCAN should declare VALUES with */
/*                    dimension MAXNTK. */


/*     NUMVLS         is an array of numeric values of parsed numeric */
/*                    tokens.  The caller of EKSCAN should declare */
/*                    NUMVLS with dimension at least MAXNUM. */

/*     CHRBUF         is a character string used to contain the values */
/*                    of literal string tokens and identifiers.  The */
/*                    value MAXQRY is guaranteed to be a safe length for */
/*                    CHRBUF, though the caller of EKSCAN can probably */
/*                    get away with less. */

/*                    The reason for the existence of CHRBUF is that */
/*                    the lexemes representing quoted strings may contain */
/*                    doubled quote characters representing embedded */
/*                    quotes; these characters are undoubled when the */
/*                    lexemes are parsed.  Hence the parsed quoted */
/*                    strings are not necessarily substrings of the */
/*                    original lexemes from which they are derived. */

/*     CHBEGS, */
/*     CHENDS         are, respectively, arrays of begin and end pointers */
/*                    for parsed quoted strings and identifiers stored in */
/*                    CHRBUF. */

/*     SCNERR         is a logical flag which is set to .TRUE. if a */
/*                    scanning error is detected, and is set to .FALSE. */
/*                    otherwise.  If SCNERR is returned .TRUE., all */
/*                    outputs save ERRMSG are undefined. */

/*     ERRMSG         is an error message that describes the cause of */
/*                    a scanning error, if such an error is detected. */
/*                    When SCNERR is returned .FALSE., ERRMSG is set to */
/*                    blank. */

/* $ Parameters */

/*     See the include files. */

/* $ Exceptions */

/*     Error free. */

/*     This routine set the error flag ERROR to .TRUE. and returns an */
/*     error message in the event that a syntax error precludes scanning */
/*     the input string.  Note that incorrect queries may scan */
/*     successfully; it is the responsibility of the caller to ensure */
/*     syntactic and semantic correctness of queries. */

/*     The following error messages are returned by this routine: */

/*        'No table list preceded first keyword.' */
/*        'Column clause and WHERE keyword are missing.' */
/*        'WHERE keyword is missing.' */
/*        'Too many tokens in query; max allowed is #.' */
/*        'Column list was empty.' */
/*        'Quoted string in positions #:# is empty.' */
/*        'Unexpected token found in query: #' */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine breaks up a valid EK query into an array of */
/*     individual tokens in order to facilitate parsing. */

/*     Time values and quoted strings are treated as single tokens. */

/* $ Examples */

/*     1)  Examples of strings containing lexically valid queries */
/*         are: */

/*            FROM TIME * WHERE TIME LT 'MO SCLK 15328997.121' */

/*            from time, event_type where event_type eq "MOC_EVENT" */

/*            FROM * WHERE TIME GE "1994 MAR 1" AND  IDCODE EQ -94030 */

/*            FROM  *   WHERE */
/*                  TIME        GE   "1994 MAR 1" */
/*            AND   TIME        LE   '1-MAR-1994 18:4:1' */
/*            AND   EVENT_TYPE  LIKE '*PMIRR*' */

/*            FROM * WHERE TIME LT "MO SCLK 15328997.121" ORDER BY TIME */

/*            from col_1 col_2 col_3 where time lt '2010' */

/*            from col_1 col_2 col_3 */

/*            from * */

/*            from * order by event_type */

/*         For a query to be semantically valid, all of the column names */
/*         referenced in the query must be present in at least one */
/*         loaded E-kernel. */


/*     2)  Examples of lexically invalid queries are: */

/*            from time where time lt */
/*            1991 jan 1                         {time string is not */
/*                                                quoted} */

/*            from time * where time */
/*             .lt. 1991 jan 1                   {operator should be lt} */


/*            from event_type * where */
/*            event_type eq ""                   {quoted string is empty} */

/*            from event_type ^ where */
/*            event_type eq "cmd"                {unexpected token} */

/*            from column1 where */
/*            column1 eq  3c                     {invalid numeric token} */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 3.0.2, 02-OCT-2021 (NJB) */

/*        Corrected typo in comments. */

/* -    SPICELIB Version 3.0.1, 22-OCT-1996 (NJB) */

/*        Corrected miscellaneous errors in the header. */

/* -    SPICELIB Version 3.0.0, 14-NOV-1995 (NJB) */

/*        Complete re-write for architecture 3. */

/* -& */
/* $ Index_Entries */

/*     scan EK query */
/*     find tokens in EK query */

/* -& */
/* $ Revisions */

/* -    Beta Version 3.0.0, 14-NOV-1995 (NJB) */

/*        Complete re-write for architecture 3. */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Number of tokens made up of special characters: */


/*     Max length of any such token: */


/*     Local variables */


/*     Statement Functions */


/*     Saved variables */


/*     Initial values */


/*     These keyword declarations must be made in alphabetical order! */


/*     The following tokens are sequences of special characters.  Some */
/*     of these are synonyms for keywords; some have other meanings.  In */
/*     this data statement, the longer sequences must precede the shorter */
/*     ones, in order for the matching algorithm to work properly. */


/*     Statement Function Definitions */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("ZZEKSCAN", (ftnlen)8);
    }

/*     The first time through, set up our identifier character set. */

    if (pass1) {

/*        Each identifier must start with a letter (of either case). */
/*        The subsequent characters must be letters, numbers, dollar */
/*        signs or underscores. */

	s_copy(hdchrs, "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz",
		 (ftnlen)80, (ftnlen)52);
	s_copy(tlchrs, "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
		"0123456789$_", (ftnlen)80, (ftnlen)64);
	ssizei_(&c__512, idspec);
	lxcsid_(hdchrs, tlchrs, idspec, (ftnlen)80, (ftnlen)80);
	pass1 = FALSE_;
    }

/*     We'll work with a local copy of the query. */

    l = rtrim_(query, query_len);
    s_copy(tquery, query, (ftnlen)2000, l);

/*     Initialize pointers and counts. */

    cptr = 1;
    nnums = 0;
    nstrs = 0;
    chcard = 0;
    *ntoken = 0;

/*     Start out in the token search state. */

    state = 1;
    while(state != 3) {
	if (state == 1) {

/*           In our initial state, we're looking for a new token. */
/*           We stop when we have enough characters to determine */
/*           which kind of token we have, or if we run out of */
/*           characters. */

/*           Set our character pointer to the beginning of the next */
/*           token. */

	    if (*ntoken > 0) {
		cptr = lxends[*ntoken - 1] + 1;
	    }
	    if (cptr > l) {
		state = 3;
	    } else {
		while(*(unsigned char *)&tquery[cptr - 1] == ' ' && cptr < l) 
			{
		    ++cptr;
		}
		if (*(unsigned char *)&tquery[cptr - 1] == ' ') {

/*                 We're out of non-blank characters to look at. */

		    state = 3;
		} else {
		    *(unsigned char *)chr = *(unsigned char *)&tquery[cptr - 
			    1];
		    state = 2;
		}
	    }

/*           STATE is in the set {NEWTOK, TERM}. */

	} else if (state == 2) {

/*           If we got this far, we have the initial character of */
/*           something that could be a valid token.  We test for */

/*              - quoted strings */
/*              - numbers */
/*              - identifiers */
/*              - special symbols */

/*           in that order.  Of course, we must have room in our output */
/*           arrays for the token. */

	    if (*ntoken == *maxntk) {
		s_copy(errmsg, "Maximum allowed number of tokens is #; at le"
			"ast # tokens are present in QUERY.", errmsg_len, (
			ftnlen)78);
		repmi_(errmsg, "#", maxntk, errmsg, errmsg_len, (ftnlen)1, 
			errmsg_len);
		i__1 = *maxntk + 1;
		repmi_(errmsg, "#", &i__1, errmsg, errmsg_len, (ftnlen)1, 
			errmsg_len);
		*scnerr = TRUE_;
		chkout_("ZZEKSCAN", (ftnlen)8);
		return 0;
	    }
	    *(unsigned char *)&ch__1[0] = *(unsigned char *)chr;
	    if (*(unsigned char *)&ch__1[0] == '\'' || *(unsigned char *)&
		    ch__1[0] == '"') {
		state = 4;
	    } else /* if(complicated condition) */ {
		*(unsigned char *)&ch__1[0] = *(unsigned char *)chr;
		if (*(unsigned char *)&ch__1[0] == '.') {
		    state = 5;
		} else /* if(complicated condition) */ {
		    *(unsigned char *)&ch__1[0] = *(unsigned char *)chr;
		    *(unsigned char *)&ch__2[0] = *(unsigned char *)&ch__1[0];
		    *(unsigned char *)&ch__3[0] = *(unsigned char *)&ch__1[0];
		    if (*(unsigned char *)&ch__2[0] >= '0' && *(unsigned char 
			    *)&ch__2[0] <= '9' || (*(unsigned char *)&ch__3[0]
			     == '+' || *(unsigned char *)&ch__3[0] == '-') || 
			    *(unsigned char *)&ch__1[0] == '.') {
			state = 6;
		    } else /* if(complicated condition) */ {
			*(unsigned char *)&ch__1[0] = *(unsigned char *)chr;
			if (*(unsigned char *)&ch__1[0] >= 'A' && *(unsigned 
				char *)&ch__1[0] <= 'Z' || *(unsigned char *)&
				ch__1[0] >= 'a' && *(unsigned char *)&ch__1[0]
				 <= 'z') {
			    state = 7;
			} else {
			    state = 8;
			}
		    }
		}
	    }

/*           At this point, the next value of STATE has been determined. */
/*           STATE is in the set */

/*              {QSTR, NUMBER, IDENT, SPCIAL} */

	} else if (state == 4) {

/*           Look for a quoted string starting at location CPTR. */
/*           Use the current character as the quote character. */

	    lxqstr_(tquery, chr, &cptr, &last, &nchars, (ftnlen)2000, (ftnlen)
		    1);
	    if (nchars == 0) {
		s_copy(errmsg, "Invalid quoted string at location #.", 
			errmsg_len, (ftnlen)36);
		repmi_(errmsg, "#", &cptr, errmsg, errmsg_len, (ftnlen)1, 
			errmsg_len);
		*scnerr = TRUE_;
		chkout_("ZZEKSCAN", (ftnlen)8);
		return 0;
	    }

/*           We've located a quoted string lexeme.  Parse the lexeme */
/*           and obtain the corresponding string value.  First make */
/*           sure we have enough room for the parsed string. */

	    room = i_len(chrbuf, chrbuf_len) - chcard;
	    if (nchars > room) {
		s_copy(errmsg, "Insufficient space to store quoted string at"
			" location #; # chars needed; only # are available.", 
			errmsg_len, (ftnlen)94);
		repmi_(errmsg, "#", &cptr, errmsg, errmsg_len, (ftnlen)1, 
			errmsg_len);
		repmi_(errmsg, "#", &nchars, errmsg, errmsg_len, (ftnlen)1, 
			errmsg_len);
		repmi_(errmsg, "#", &room, errmsg, errmsg_len, (ftnlen)1, 
			errmsg_len);
		*scnerr = TRUE_;
		chkout_("ZZEKSCAN", (ftnlen)8);
		return 0;
	    }
	    i__1 = chcard;
	    parsqs_(tquery + (cptr - 1), chr, chrbuf + i__1, &length, scnerr, 
		    errmsg, &ptr, cptr + nchars - 1 - (cptr - 1), (ftnlen)1, 
		    chrbuf_len - i__1, errmsg_len);
	    if (*scnerr) {
		prefix_("#", &c__2, errmsg, (ftnlen)1, errmsg_len);
		repmc_(errmsg, "#", "Error occurred while parsing quoted str"
			"ing token at location #:", errmsg, errmsg_len, (
			ftnlen)1, (ftnlen)63, errmsg_len);
		repmi_(errmsg, "#", &cptr, errmsg, errmsg_len, (ftnlen)1, 
			errmsg_len);
		*scnerr = TRUE_;
		chkout_("ZZEKSCAN", (ftnlen)8);
		return 0;
	    }

/*           We've found a valid quoted string.  Set our outputs. */

	    ++(*ntoken);
	    tokens[*ntoken - 1] = 5;
	    ++nstrs;
	    values[*ntoken - 1] = nstrs;
	    chbegs[nstrs - 1] = chcard + 1;
	    chends[nstrs - 1] = chcard + length;
	    chcard = chends[nstrs - 1];
	    lxbegs[*ntoken - 1] = cptr;
	    lxends[*ntoken - 1] = last;
	    state = 1;

/*           STATE is now NXTTOK. */

	} else if (state == 5) {

/*           The token begins with a period.  We could be looking at */
/*           a floating point number, or we could be looking at a */
/*           period in a compound identifier. */

/*           Look for a number starting at location CPTR. */

	    lx4num_(tquery, &cptr, &last, &nchars, (ftnlen)2000);
	    if (nchars > 0) {
		state = 6;
	    } else {
		state = 8;
	    }

/*           STATE has been set to NUMBER or SPCIAL.  CPTR and NTOKEN */
/*           remain unchanged. */

	} else if (state == 6) {

/*           Look for a number starting at location CPTR. */

	    lx4num_(tquery, &cptr, &last, &nchars, (ftnlen)2000);
	    if (nchars == 0) {
		s_copy(errmsg, "Invalid numeric token at location #.", 
			errmsg_len, (ftnlen)36);
		repmi_(errmsg, "#", &cptr, errmsg, errmsg_len, (ftnlen)1, 
			errmsg_len);
		*scnerr = TRUE_;
		chkout_("ZZEKSCAN", (ftnlen)8);
		return 0;
	    }

/*           Parse the token, but only do so if there's enough */
/*           room to store the result. */

	    room = *maxnum - nnums;
	    if (room < 1) {
		s_copy(errmsg, "Insufficient space to store value of number "
			"at location #; # elements are available in the NUMVL"
			"S array; # are required.", errmsg_len, (ftnlen)120);
		repmi_(errmsg, "#", &cptr, errmsg, errmsg_len, (ftnlen)1, 
			errmsg_len);
		repmi_(errmsg, "#", maxnum, errmsg, errmsg_len, (ftnlen)1, 
			errmsg_len);
		i__1 = *maxnum + 1;
		repmi_(errmsg, "#", &i__1, errmsg, errmsg_len, (ftnlen)1, 
			errmsg_len);
		*scnerr = TRUE_;
		chkout_("ZZEKSCAN", (ftnlen)8);
		return 0;
	    }
	    nparsd_(tquery + (cptr - 1), &numvls[nnums], errmsg, &ptr, last - 
		    (cptr - 1), errmsg_len);
	    if (s_cmp(errmsg, " ", errmsg_len, (ftnlen)1) != 0) {

/*              This check is done for safety; by construction, we */
/*              should always have a valid number if LX4NUM */
/*              thinks we have a valid number, so in fact ERRMSG */
/*              should always be blank. */

		prefix_("#", &c__2, errmsg, (ftnlen)1, errmsg_len);
		repmc_(errmsg, "#", "Error found in numeric token at locatio"
			"n #:", errmsg, errmsg_len, (ftnlen)1, (ftnlen)43, 
			errmsg_len);
		i__1 = cptr + ptr - 1;
		repmi_(errmsg, "#", &i__1, errmsg, errmsg_len, (ftnlen)1, 
			errmsg_len);
		*scnerr = TRUE_;
		chkout_("ZZEKSCAN", (ftnlen)8);
		return 0;
	    }

/*           We found a valid numeric token.  We distinguish */
/*           between integers and d.p. numbers; set the token */
/*           to the most restrictive category possible. */

	    ++(*ntoken);
	    if (beint_(tquery + (cptr - 1), last - (cptr - 1))) {
		tokens[*ntoken - 1] = 3;
	    } else {
		tokens[*ntoken - 1] = 4;
	    }

/*           Set the rest of our outputs. */

	    ++nnums;
	    values[*ntoken - 1] = nnums;
	    lxbegs[*ntoken - 1] = cptr;
	    lxends[*ntoken - 1] = last;
	    state = 1;

/*           STATE is now NXTTOK. */

	} else if (state == 7) {

/*           Look for an identifier starting at location CPTR. */

	    lxidnt_(idspec, tquery, &cptr, &last, &nchars, (ftnlen)2000);
	    if (nchars == 0) {

/*              This check is done for safety; by construction, we */
/*              should always have a valid identifier of at least one */
/*              character if we get to the IDENT state, so in fact */
/*              NCHARS should never equal zero. */

		s_copy(errmsg, "Invalid identifier at location #.", 
			errmsg_len, (ftnlen)33);
		repmi_(errmsg, "#", &cptr, errmsg, errmsg_len, (ftnlen)1, 
			errmsg_len);
		*scnerr = TRUE_;
		chkout_("ZZEKSCAN", (ftnlen)8);
		return 0;
	    }

/*           We've located an identifier lexeme.  Make sure we have */
/*           enough room for the string. */

	    room = i_len(chrbuf, chrbuf_len) - chcard;
	    if (nchars > room) {
		s_copy(errmsg, "Insufficient space to store identifier strin"
			"g at location #; # chars needed; only # are availabl"
			"e.", errmsg_len, (ftnlen)98);
		repmi_(errmsg, "#", &cptr, errmsg, errmsg_len, (ftnlen)1, 
			errmsg_len);
		repmi_(errmsg, "#", &nchars, errmsg, errmsg_len, (ftnlen)1, 
			errmsg_len);
		repmi_(errmsg, "#", &room, errmsg, errmsg_len, (ftnlen)1, 
			errmsg_len);
		*scnerr = TRUE_;
		chkout_("ZZEKSCAN", (ftnlen)8);
		return 0;
	    }

/*           We've found a valid identifier or keyword.  Set our */
/*           outputs.  Convert the string to upper case. */

	    ++(*ntoken);
	    i__1 = chcard;
	    ucase_(tquery + (cptr - 1), chrbuf + i__1, last - (cptr - 1), 
		    chcard + nchars - i__1);
	    i__1 = chcard;
	    i__ = bsrchc_(chrbuf + i__1, &c__29, keywds, chcard + nchars - 
		    i__1, (ftnlen)32);
	    if (i__ > 0) {

/*              It's a keyword. */

		tokens[*ntoken - 1] = 1;
		values[*ntoken - 1] = kwvals[(i__1 = i__ - 1) < 29 && 0 <= 
			i__1 ? i__1 : s_rnge("kwvals", i__1, "zzekscan_", (
			ftnlen)956)];
		lxbegs[*ntoken - 1] = cptr;
		lxends[*ntoken - 1] = last;
		state = 1;
	    } else {

/*              It's an identifier. */

		++nstrs;
		chbegs[nstrs - 1] = chcard + 1;
		chends[nstrs - 1] = chcard + nchars;
		chcard = chends[nstrs - 1];
		tokens[*ntoken - 1] = 2;
		values[*ntoken - 1] = nstrs;
		lxbegs[*ntoken - 1] = cptr;
		lxends[*ntoken - 1] = last;
		state = 1;

/*              We finished scanning an identifier. */

/*              STATE is set to NXTTOK. */

	    }

/*           We scanned a keyword or an identifier. */

/*           STATE is set to NXTTOK. */

	} else if (state == 8) {

/*           Look for a valid token starting with a special character at */
/*           location CPTR. We attempt to match the longest possible */
/*           special token. */

/* Computing MIN */
	    i__1 = 2, i__2 = l - cptr + 1;
	    i__ = min(i__1,i__2);
	    j = 0;
	    while(i__ >= 1 && j == 0) {
		last = cptr + i__ - 1;
		j = isrchc_(tquery + (cptr - 1), &c__13, spcstr, last - (cptr 
			- 1), (ftnlen)2);
		if (j == 0) {
		    --i__;
		}
	    }
	    if (j > 0) {

/*              We've identified a valid token. */

		++(*ntoken);
		tokens[*ntoken - 1] = spctok[(i__1 = j - 1) < 13 && 0 <= i__1 
			? i__1 : s_rnge("spctok", i__1, "zzekscan_", (ftnlen)
			1017)];
		values[*ntoken - 1] = spcval[(i__1 = j - 1) < 13 && 0 <= i__1 
			? i__1 : s_rnge("spcval", i__1, "zzekscan_", (ftnlen)
			1018)];
		lxbegs[*ntoken - 1] = cptr;
		lxends[*ntoken - 1] = cptr - 1 + rtrim_(spcstr + (((i__1 = j 
			- 1) < 13 && 0 <= i__1 ? i__1 : s_rnge("spcstr", i__1,
			 "zzekscan_", (ftnlen)1020)) << 1), (ftnlen)2);
		state = 1;
	    } else {
		s_copy(errmsg, "Invalid character found at location #. ", 
			errmsg_len, (ftnlen)39);
		repmi_(errmsg, "#", &cptr, errmsg, errmsg_len, (ftnlen)1, 
			errmsg_len);

/*              If the offending character is printable, include it */
/*              in the error message.  Otherwise, include the integer */
/*              code for the character. */

		if (frstpc_(chr, (ftnlen)1) > 0) {
		    suffix_("<character> = '#'", &c__2, errmsg, (ftnlen)17, 
			    errmsg_len);
		    repmc_(errmsg, "#", chr, errmsg, errmsg_len, (ftnlen)1, (
			    ftnlen)1, errmsg_len);
		} else {
		    suffix_("ICHAR(<character>) = #", &c__2, errmsg, (ftnlen)
			    22, errmsg_len);
		    i__1 = *(unsigned char *)chr;
		    repmi_(errmsg, "#", &i__1, errmsg, errmsg_len, (ftnlen)1, 
			    errmsg_len);
		}
		*scnerr = TRUE_;
		chkout_("ZZEKSCAN", (ftnlen)8);
		return 0;
	    }

/*           STATE is now NXTTOK. */

	}
    }

/*     If we got this far, we've found the tokens in the query. */

    *scnerr = FALSE_;
    s_copy(errmsg, " ", errmsg_len, (ftnlen)1);
    chkout_("ZZEKSCAN", (ftnlen)8);
    return 0;
} /* zzekscan_ */

