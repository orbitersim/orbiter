/* zzrvbf.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__32 = 32;
static integer c__132 = 132;

/* $Procedure ZZRVBF ( Private --- Pool, read the next buffer variable ) */
/* Subroutine */ int zzrvbf_(char *buffer, integer *bsize, integer *linnum, 
	integer *namlst, integer *nmpool, char *names, integer *datlst, 
	integer *dppool, doublereal *dpvals, integer *chpool, char *chvals, 
	char *varnam, logical *eof, ftnlen buffer_len, ftnlen names_len, 
	ftnlen chvals_len, ftnlen varnam_len)
{
    /* Initialized data */

    static logical first = TRUE_;

    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen), i_len(char *, ftnlen), 
	    s_rnge(char *, integer, char *, integer);

    /* Local variables */
    static integer head, code, itab;
    static char name__[132];
    static integer begs[132], free, node;
    static char line[132];
    static integer ends[132], tail;
    static logical even, full;
    static integer type__[132], b, e, i__, j, badat;
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen), lnkan_(integer *, integer *);
    static logical found;
    static integer ncomp, count;
    static char error[256];
    static integer iplus;
    extern integer rtrim_(char *, ftnlen);
    extern /* Subroutine */ int zzcln_(integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *);
    static integer r1, r2;
    extern logical failed_(void);
    static integer at, datahd, iblank, chnode, icomma, nameat, dpnode;
    extern /* Subroutine */ int lnkila_(integer *, integer *, integer *);
    static integer iequal;
    static doublereal dvalue;
    extern integer lastpc_(char *, ftnlen), lnknfn_(integer *);
    static integer ilparn, irparn, itmark, dirctv, lookat, iquote;
    extern integer zzhash_(char *, ftnlen);
    static integer varlen;
    static logical intokn, insepf;
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    extern logical return_(void);
    static logical inquot;
    static integer status, vartyp, nxttok;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen), sigerr_(char *, ftnlen), lnkfsl_(integer *, 
	    integer *, integer *), tparse_(char *, doublereal *, char *, 
	    ftnlen, ftnlen), nparsd_(char *, doublereal *, char *, integer *, 
	    ftnlen, ftnlen);

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Read the next variable from a text buffer into the kernel pool. */

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

/*     PRIVATE KERNEL */

/* $ Keywords */

/*     POOL */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     BUFFER     I   Array of text to be read and parsed. */
/*     BSIZE      I   Number of lines in text buffer. */
/*     LINNUM    I/O  line number to begin reading buffer */
/*     NAMLST    I/O  array of collision resolution list heads */
/*     NMPOOL    I/O  linked list pool of collision resolution lists */
/*     NAMES     I/O  array of names of kernel pool variables */
/*     DATLST    I/O  array of heads of lists of variable values */
/*     DPPOOL    I/O  linked list pool of pointer lists to d.p. values */
/*     DPVALS    I/O  array of d.p. kernel pool values */
/*     CHPOOL    I/O  linked list pool of pointer lists to string values */
/*     CHVALS    I/O  array of string kernel pool values */
/*     VARNAM     O   name of variable parsed */
/*     EOF        O   if TRUE end of input file has been reached. */

/* $ Detailed_Input */

/*     BUFFER    is a string array that contains the text that should */
/*               be parsed and placed into the kernel pool data */
/*               structure. */

/*     BSIZE     is the number of lines of text in BUFFER. */

/*     LINNUM    the line number (in BUFFER) at which to begin parsing */
/*               text. */

/*     NAMLST    this collection of arrays together with the hash */
/*     NMPOOL    function ZZHASH provide the mechanism for storing */
/*     NAMES     and retrieving kernel pool variables. */
/*     DATLST */
/*     DPPOOL    Given a potential variable name NAME the function */
/*     DPVALS    ZZHASH(NAME) gives the location in the array in */
/*     CHPOOL    NAMLST where one should begin looking for the */
/*     CHVALS    kernel pool variable NAME. */

/*               If NAMLST( ZZHASH(NAME) ) is zero, there is no kernel */
/*               pool variable corresponding to NAME.  If it is non-zero */
/*               then NAMLST is the head node of a linked list of names */
/*               that evaluate to the same integer under the function */
/*               ZZHASH.  Letting NODE = NAMLST( ZZHASH(NAME) ) check */
/*               NAMES(NODE) for equality with NAME.  If there is */
/*               no match find the next node ( NMPOOL(NEXT,NODE) ) until */
/*               a match occurs or all nodes of the list have been */
/*               examined.  To insert a new NAME allocate a node NEW from */
/*               the free list of NMPOOL and append it to the tail of the */
/*               list pointed to by NAMLST ( ZZHASH(NAME) ). */

/*               Once a node for NAME is located (call it NAMEAT) */
/*               the values for NAME can be found by examining */
/*               DATLST(NAMEAT).  If zero, no values have yet been */
/*               given to NAME.  If less than zero, -DATLST(NAMEAT) */
/*               is the head node of a list in CHPOOL that gives the */
/*               indexes of the values of NAME in CHVALS.  If greater */
/*               than zero, DATLST(NAMEAT) is the head node of a list */
/*               in DPPOOL that gives the indexes of the values of NAME */
/*               in DPVALS. */




/* $ Detailed_Output */

/*     LINNUM     is the line number at which the "next" read should */
/*                begin. */

/*     NAMLST     is the same structure as input but updated to */
/*     NMPOOL     include the next variable read from the text buffer. */
/*     NAMES */
/*     DATLST */
/*     DPPOOL */
/*     DPVALS */
/*     CHPOOL */
/*     CHVALS */

/*     VARNAM     is the name of the variable. VARNAM is blank if */
/*                no variable is read. */

/*     EOF        is true when the end of the internal buffer has been */
/*                reached, and is false otherwise. */

/* $ Parameters */

/*      LINLEN      is the maximum length of a line in the buffer. */

/*      MAXLEN      is the maximum length of the variable names that */
/*                  can be stored in the kernel pool (defined in pool.f). */

/* $ Exceptions */


/*     1) The error 'SPICE(BADTIMESPEC)' is signaled if a value */
/*        beginning with '@' cannot be parsed as a time. */

/*     2) The error 'SPICE(BADVARASSIGN)' is signaled if variable */
/*        assignment does not have the form NAME = [(] value [ value ) ]. */

/*     3) The error 'SPICE(KERNELPOOLFULL)' is signaled if there is */
/*        no room left in the kernel pool to store another variable */
/*        or value. */

/*     4) The error 'SPICE(NONPRINTINGCHAR)' is signaled if the name */
/*        in a variable assignment contains a non-printing character. */

/*     5) The error 'SPICE(NUMBEREXPECTED)' is signaled if a value */
/*        that is unquoted cannot be parsed as time or number. */

/*     6) The error 'SPICE(TYPEMISMATCH)' is signaled if a variable */
/*        has a first value of one type (numeric or character) and */
/*        a subsequent component has the other type. */

/*     7) The error 'SPICE(BADVARNAME)' signals if a kernel pool */
/*        variable name length exceeds MAXLEN. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     See POOL (entry point LMPOOL). */

/* $ Restrictions */

/*     The input buffer should be no more than 132 characters in width. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.1, 03-OCT-2021 (NJB) */

/*        Corrected typo in comments. */

/* -    SPICELIB Version 1.1.0, 09-FEB-2010 (EDW) */

/*        Added an error check on the length of the kernel pool variable */
/*        names read from BUFFER. */

/* -    SPICELIB Version 1.0.0, 29-MAR-1999 (WLT) */

/* -& */


/*     SPICELIB functions */


/*     Local parameters. */

/*     Below are a collection of enumerated lists that are used */
/*     to discern what part of the processing we are in and what */
/*     kind of entity we are dealing with.  First the overall */
/*     processing flow of a variable assignment. */


/*     Next we have the various types of tokens that can be found */
/*     in the parsing of an input line */

/*     Q   --- quoted (or protected tokens) */
/*     NQ  --- unquoted tokens */
/*     BV  --- beginning of a vector */
/*     EV  --- ending of a vector */
/*     EQ  --- equal sign */
/*     EQP --- equal sign plus */


/*     A variable can have one of three types as we process */
/*     it.  It can have an unknown type UNKNWN, STRTYP or NUMTYP. */



/*     The next two parameters indicate which component of a linked */
/*     list node point to the previous node and the next node. */


/*     The next collection of variables are set up in first pass */
/*     through this routine.  They would be parameters if FORTRAN */
/*     allowed us to do this in a standard way. */


/*     The logicals below are used to take apart the tokens in an */
/*     input line. */


/*     The following logicals are in-line functions that are used */
/*     when processing the input strings. */


/*     Save everything. */


/*     Below are a collection of In-line function definitions that are */
/*     intended to make the code a bit easier to write and read. */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("ZZRVBF", (ftnlen)6);
    }

/*     Initializations. */

    if (first) {
	first = FALSE_;
	icomma = ',';
	iblank = ' ';
	iquote = '\'';
	ilparn = '(';
	irparn = ')';
	iequal = '=';
	iplus = '+';
	itmark = '@';
	itab = 9;
    }

/*     No variable yet and no parsing errors so far. */

    s_copy(name__, " ", (ftnlen)132, (ftnlen)1);
    s_copy(error, " ", (ftnlen)256, (ftnlen)1);
    ncomp = 0;

/*     Get the next data line. Unless something is terribly wrong, */
/*     this will begin a new variable definition. We have to read */
/*     the whole variable, unless we get an error, in which case */
/*     we can quit. */

    status = 1;
    while(status != 2 && ! failed_()) {
	s_copy(line, " ", (ftnlen)132, (ftnlen)1);

/*        We need to skip blank lines... */

	while(s_cmp(line, " ", (ftnlen)132, (ftnlen)1) == 0) {
	    *eof = *linnum > *bsize;
	    if (*eof) {
		chkout_("ZZRVBF", (ftnlen)6);
		return 0;
	    }
	    s_copy(line, buffer + (*linnum - 1) * buffer_len, (ftnlen)132, 
		    buffer_len);
	    ++(*linnum);
	}

/*        Find the "tokens" in the input line. As you scan from left */
/*        to right along the line, exactly one of the following */
/*        conditions is true. */

/*        1) You are in a separator field */
/*        4) You are in a quoted substring */
/*        5) You are in a non-quoted substring that isn't a separator */
/*           field. */

/*        Stuff between separator fields are regarded as tokens.  Note */
/*        this includes quoted strings. */

/*        In addition we keep track of 3 separators: '=', '(', ')' */
/*        Finally, whenever we encounters the separator '=', we back */
/*        up and see if it is preceded by a '+', if so we attach */
/*        it to the '=' and treat the pair of characters as a single */
/*        separator. */

	even = TRUE_;
	intokn = FALSE_;
	inquot = FALSE_;
	insepf = TRUE_;
	count = 0;
	i__ = 0;
	while(i__ < i_len(line, (ftnlen)132)) {

/*           The current character is either a separator, quote or */
/*           some other character. */

	    ++i__;
	    code = *(unsigned char *)&line[i__ - 1];
	    if (code == iblank || code == icomma || code == ilparn || code == 
		    irparn || code == iequal || code == itab) {

/*              There are 3 possible states we could be in */
/*                 Separation Field */
/*                 A quoted substring with the last quote an odd one. */
/*                 A quoted substring with the last quote an even one. */
/*                 A non-quoted token. */
/*              In the first two cases nothing changes, but in the */
/*              next two cases we transition to a separation field. */

		if (intokn || inquot && even) {
		    inquot = FALSE_;
		    intokn = FALSE_;
		    insepf = TRUE_;
		}
		if (insepf) {

/*                 We need to see if this is one of the special */
/*                 separators */

		    if (code == iequal) {
			++count;
			begs[(i__1 = count - 1) < 132 && 0 <= i__1 ? i__1 : 
				s_rnge("begs", i__1, "zzrvbf_", (ftnlen)548)] 
				= i__;
			type__[(i__1 = count - 1) < 132 && 0 <= i__1 ? i__1 : 
				s_rnge("type", i__1, "zzrvbf_", (ftnlen)549)] 
				= 5;
			ends[(i__1 = count - 1) < 132 && 0 <= i__1 ? i__1 : 
				s_rnge("ends", i__1, "zzrvbf_", (ftnlen)550)] 
				= i__;
			if (i__ > 1) {

/*                       Look back at the previous character. */
/*                       See if it is a plus character. */

			    i__1 = i__ - 2;
			    code = *(unsigned char *)&line[i__1];
			    if (code == iplus) {

/*                          This is the directive '+=' we need */
/*                          to set the beginning of this token */
/*                          to the one before this and adjust */
/*                          the end of the last token. */

				type__[(i__1 = count - 1) < 132 && 0 <= i__1 ?
					 i__1 : s_rnge("type", i__1, "zzrvbf_"
					, (ftnlen)566)] = 6;
				begs[(i__1 = count - 1) < 132 && 0 <= i__1 ? 
					i__1 : s_rnge("begs", i__1, "zzrvbf_",
					 (ftnlen)567)] = i__ - 1;
				if (begs[(i__1 = count - 2) < 132 && 0 <= 
					i__1 ? i__1 : s_rnge("begs", i__1, 
					"zzrvbf_", (ftnlen)569)] == ends[(
					i__2 = count - 2) < 132 && 0 <= i__2 ?
					 i__2 : s_rnge("ends", i__2, "zzrvbf_"
					, (ftnlen)569)]) {
				    --count;
				    begs[(i__1 = count - 1) < 132 && 0 <= 
					    i__1 ? i__1 : s_rnge("begs", i__1,
					     "zzrvbf_", (ftnlen)573)] = i__ - 
					    1;
				    ends[(i__1 = count - 1) < 132 && 0 <= 
					    i__1 ? i__1 : s_rnge("ends", i__1,
					     "zzrvbf_", (ftnlen)574)] = i__;
				    type__[(i__1 = count - 1) < 132 && 0 <= 
					    i__1 ? i__1 : s_rnge("type", i__1,
					     "zzrvbf_", (ftnlen)575)] = 6;
				} else {
				    ends[(i__1 = count - 2) < 132 && 0 <= 
					    i__1 ? i__1 : s_rnge("ends", i__1,
					     "zzrvbf_", (ftnlen)579)] = ends[(
					    i__2 = count - 2) < 132 && 0 <= 
					    i__2 ? i__2 : s_rnge("ends", i__2,
					     "zzrvbf_", (ftnlen)579)] - 1;
				}
			    }
			}
		    } else if (code == irparn) {
			++count;
			begs[(i__1 = count - 1) < 132 && 0 <= i__1 ? i__1 : 
				s_rnge("begs", i__1, "zzrvbf_", (ftnlen)590)] 
				= i__;
			ends[(i__1 = count - 1) < 132 && 0 <= i__1 ? i__1 : 
				s_rnge("ends", i__1, "zzrvbf_", (ftnlen)591)] 
				= i__;
			type__[(i__1 = count - 1) < 132 && 0 <= i__1 ? i__1 : 
				s_rnge("type", i__1, "zzrvbf_", (ftnlen)592)] 
				= 4;
		    } else if (code == ilparn) {
			++count;
			begs[(i__1 = count - 1) < 132 && 0 <= i__1 ? i__1 : 
				s_rnge("begs", i__1, "zzrvbf_", (ftnlen)597)] 
				= i__;
			ends[(i__1 = count - 1) < 132 && 0 <= i__1 ? i__1 : 
				s_rnge("ends", i__1, "zzrvbf_", (ftnlen)598)] 
				= i__;
			type__[(i__1 = count - 1) < 132 && 0 <= i__1 ? i__1 : 
				s_rnge("type", i__1, "zzrvbf_", (ftnlen)599)] 
				= 3;
		    }
		}
	    } else if (code == iquote) {

/*              There are 3 cases of interest. */
/*                 We are in a quoted substring already */
/*                 We are in a separator field */
/*                 We are in a non-quoted token. */
/*              In the first case nothing changes.  In the second */
/*              two cases we change to being in a quoted substring. */

		even = ! even;
		if (! inquot) {
		    insepf = FALSE_;
		    intokn = FALSE_;
		    inquot = TRUE_;
		    ++count;
		    begs[(i__1 = count - 1) < 132 && 0 <= i__1 ? i__1 : 
			    s_rnge("begs", i__1, "zzrvbf_", (ftnlen)622)] = 
			    i__;
		    type__[(i__1 = count - 1) < 132 && 0 <= i__1 ? i__1 : 
			    s_rnge("type", i__1, "zzrvbf_", (ftnlen)623)] = 1;
		}
		ends[(i__1 = count - 1) < 132 && 0 <= i__1 ? i__1 : s_rnge(
			"ends", i__1, "zzrvbf_", (ftnlen)627)] = i__;
	    } else {

/*              This is some character other than a quote, or */
/*              separator character. */

/*              We are in one of four situations. */

/*                 1) We are in a quoted substring with an odd number of */
/*                    quotes. */
/*                 2) We are in a quoted substring with an even number of */
/*                    quotes. */
/*                 2) We are in a separator field */
/*                 3) We are in a non-quoted token. */

/*              In cases 1 and 3 nothing changes. So we won't check */
/*              those cases. */

		if (insepf || inquot && even) {
		    inquot = FALSE_;
		    insepf = FALSE_;
		    intokn = TRUE_;
		    ++count;
		    begs[(i__1 = count - 1) < 132 && 0 <= i__1 ? i__1 : 
			    s_rnge("begs", i__1, "zzrvbf_", (ftnlen)652)] = 
			    i__;
		    type__[(i__1 = count - 1) < 132 && 0 <= i__1 ? i__1 : 
			    s_rnge("type", i__1, "zzrvbf_", (ftnlen)653)] = 2;
		}
		ends[(i__1 = count - 1) < 132 && 0 <= i__1 ? i__1 : s_rnge(
			"ends", i__1, "zzrvbf_", (ftnlen)656)] = i__;
	    }
	}

/*        The first word on the first line should be the name of a */
/*        variable. The second word should be a directive: = or +=. */

	if (status == 1) {

/*           There must be at least 3 contributing tokens on this line. */

	    if (count < 3) {
		setmsg_("A kernel variable was not properly formed on line #"
			" text buffer.Such an assignment should have the form"
			": '<variable name> [+]= <values>'. This line was '#'"
			". ", (ftnlen)157);
		r2 = rtrim_(line, (ftnlen)132);
		errint_("#", linnum, (ftnlen)1);
		errch_("#", line, (ftnlen)1, r2);
		sigerr_("SPICE(BADVARASSIGN)", (ftnlen)19);
		chkout_("ZZRVBF", (ftnlen)6);
		return 0;
	    }

/*           See if the variable name is legitimate: */

	    i__1 = begs[0] - 1;
	    badat = lastpc_(line + i__1, ends[0] - i__1);
	    if (badat <= ends[0] - begs[0]) {

/*              There is a non-printing character in the variable */
/*              name.  This isn't allowed. */

		at = begs[0] + badat;
		setmsg_("There is a non-printing character embedded in line "
			"# of the text buffer.  Non-printing characters are n"
			"ot allowed in kernel variable assignments.  The non-"
			"printing character has ASCII code #. ", (ftnlen)192);
		errint_("#", linnum, (ftnlen)1);
		i__1 = *(unsigned char *)&line[at - 1];
		errint_("#", &i__1, (ftnlen)1);
		sigerr_("SPICE(NONPRINTINGCHAR)", (ftnlen)22);
		chkout_("ZZRVBF", (ftnlen)6);
		return 0;
	    }

/*           Check the variable name length; signal an error */
/*           if longer than MAXLEN. */

	    i__1 = begs[0] - 1;
	    varlen = i_len(line + i__1, ends[0] - i__1);
	    if (varlen > 32) {
		setmsg_("A kernel pool variable name in the input buffer exc"
			"eeds the maximum allowed length #1. The actual lengt"
			"h of the variable name is #2, the offending variable"
			" name to #3 characters: '#4'.", (ftnlen)184);
		errint_("#1", &c__32, (ftnlen)2);
		errint_("#2", &varlen, (ftnlen)2);
		errint_("#3", &c__132, (ftnlen)2);
		i__1 = begs[0] - 1;
		errch_("#4", line + i__1, (ftnlen)2, ends[0] - i__1);
		sigerr_("SPICE(BADVARNAME)", (ftnlen)17);
	    }

/*           The variable name is ok. How about the directive. */

	    i__1 = begs[0] - 1;
	    s_copy(varnam, line + i__1, varnam_len, ends[0] - i__1);
	    dirctv = type__[1];

/*           If this is replacement (=) and not an addition (+=), */
/*           delete the values currently associated with the variable. */
/*           They will be replaced later. */

	    if (dirctv != 5 && dirctv != 6) {
		setmsg_("A kernel variable was not properly formed on line #"
			" of the text buffer. Such an assignment should have "
			"the form: '<variable name> [+]= <values>'.  More spe"
			"cifically, the assignment operator did not have one "
			"of the expected forms: '=' or '+='. The line was '#'"
			". ", (ftnlen)261);
		r2 = rtrim_(line, (ftnlen)132);
		errint_("#", linnum, (ftnlen)1);
		errch_("#", line, (ftnlen)1, r2);
		sigerr_("SPICE(BADVARASSIGN)", (ftnlen)19);
		chkout_("ZZRVBF", (ftnlen)6);
		return 0;
	    }

/*           Locate this variable name in the name pool or insert it */
/*           if it isn't there.  The location will be NAMEAT and */
/*           we will use the variable FOUND to indicate whether or */
/*           not it was already present. */

	    lookat = zzhash_(varnam, varnam_len);
	    node = namlst[lookat - 1];
	    full = lnknfn_(nmpool) <= 0;
	    found = FALSE_;

/*           See if this name (or one colliding with it in the */
/*           hash scheme) has already been stored in the name list. */

	    if (node > 0) {
		head = node;
		tail = -nmpool[(head << 1) + 11];
		while(node > 0 && ! found) {
		    found = s_cmp(names + (node - 1) * names_len, varnam, 
			    names_len, varnam_len) == 0;
		    nameat = node;
		    node = nmpool[(node << 1) + 10];
		}
		if (! found && ! full) {

/*                 We didn't find this name on the conflict resolution */
/*                 list. Allocate a new slot for it. */

		    lnkan_(nmpool, &node);
		    lnkila_(&tail, &node, nmpool);
		    s_copy(names + (node - 1) * names_len, varnam, names_len, 
			    varnam_len);
		    nameat = node;
		}
	    } else if (! full) {

/*              Nothing like this variable name (in the hashing sense) */
/*              has been loaded so far.  We need to allocate */
/*              a name slot for this variable. */

		lnkan_(nmpool, &node);
		namlst[lookat - 1] = node;
		s_copy(names + (node - 1) * names_len, varnam, names_len, 
			varnam_len);
		nameat = node;
	    }

/*           If the name pool was full and we didn't find this name */
/*           we've got an error. Diagnose it and return. */

	    if (full && ! found) {
		setmsg_("The kernel pool does not have room for any more var"
			"iables.  It filled up at line # of the text buffer. ",
			 (ftnlen)103);
		errint_("#", linnum, (ftnlen)1);
		sigerr_("SPICE(KERNELPOOLFULL)", (ftnlen)21);
		chkout_("ZZRVBF", (ftnlen)6);
		return 0;
	    }

/*           Now depending upon the kind of directive, we will need */
/*           to remove data and allocate a new list or simply append */
/*           data to the existing list. */

	    if (dirctv == 5) {

/*              We are going to dump whatever is associated with */
/*              this name and then we will need to allocate a new */
/*              linked list for the data. */

		vartyp = 3;
		if (found) {

/*                 We need to free the data associated with this */
/*                 variable. */

		    datahd = datlst[nameat - 1];
		    datlst[nameat - 1] = 0;
		    if (datahd < 0) {

/*                    This variable was character type we need to */
/*                    free a linked list from the character data */
/*                    pool. */

			head = -datahd;
			tail = -chpool[(head << 1) + 11];
			lnkfsl_(&head, &tail, chpool);
		    } else {

/*                    This variable was numeric type. We need to */
/*                    free a linked list from the numeric pool. */

			head = datahd;
			tail = -dppool[(head << 1) + 11];
			lnkfsl_(&head, &tail, dppool);
		    }
		}
	    } else if (dirctv == 6) {

/*              We need to append to the current variable. */

		if (found) {
		    if (datlst[nameat - 1] > 0) {
			vartyp = 2;
		    } else if (datlst[nameat - 1] < 0) {
			vartyp = 1;
		    } else {
			vartyp = 3;
		    }
		} else {
		    vartyp = 3;
		}
	    }

/*           If this is a vector, the next thing on the line will be a */
/*           left parenthesis. Otherwise, assume that this is a scalar. */
/*           If it's a vector, get the first value. If it's a scalar, */
/*           plant a bogus right parenthesis, to make the following loop */
/*           terminate after one iteration. */

	    if (type__[2] == 3) {
		nxttok = 4;
	    } else {
		nxttok = 3;
		++count;
		type__[(i__1 = count - 1) < 132 && 0 <= i__1 ? i__1 : s_rnge(
			"type", i__1, "zzrvbf_", (ftnlen)925)] = 4;
	    }

/*        For subsequent lines, treat everything as a new value. */

	} else {
	    nxttok = 1;
	}

/*        We have a value anyway. Store it in the table. */

/*        Keep going until the other shoe (the right parenthesis) */
/*        drops, or until the end of the line is reached. */

/*        Dates begin with @; anything else is presumed to be a number. */

	while(type__[(i__1 = nxttok - 1) < 132 && 0 <= i__1 ? i__1 : s_rnge(
		"type", i__1, "zzrvbf_", (ftnlen)945)] != 4 && nxttok <= 
		count) {

/*           Get the begin and end of this token. */

	    b = begs[(i__1 = nxttok - 1) < 132 && 0 <= i__1 ? i__1 : s_rnge(
		    "begs", i__1, "zzrvbf_", (ftnlen)949)];
	    e = ends[(i__1 = nxttok - 1) < 132 && 0 <= i__1 ? i__1 : s_rnge(
		    "ends", i__1, "zzrvbf_", (ftnlen)950)];
	    if (vartyp == 3) {

/*              We need to determine which category of variable we */
/*              have by looking at this token and deducing the */
/*              type. */

		if (type__[(i__1 = nxttok - 1) < 132 && 0 <= i__1 ? i__1 : 
			s_rnge("type", i__1, "zzrvbf_", (ftnlen)958)] == 1) {
		    vartyp = 1;
		} else if (type__[(i__1 = nxttok - 1) < 132 && 0 <= i__1 ? 
			i__1 : s_rnge("type", i__1, "zzrvbf_", (ftnlen)962)] 
			== 2) {
		    vartyp = 2;
		} else {

/*                 This is an error. We should have had one of the */
/*                 two previous types. */

/*                 First perform the clean up function. */

		    zzcln_(&lookat, &nameat, namlst, datlst, nmpool, chpool, 
			    dppool);
		    setmsg_("The first item following the assignment operato"
			    "r should be the value of a variable or a left pa"
			    "renthesis '(' followed by a value for a variable"
			    ". This is not true on line # of the text buffer. "
			    , (ftnlen)192);
		    errint_("#", linnum, (ftnlen)1);
		    sigerr_("SPICE(BADVARASSIGN)", (ftnlen)19);
		    chkout_("ZZRVBF", (ftnlen)6);
		    return 0;
		}
	    }
	    if (vartyp == 1) {

/*              First make sure that this token represents a string. */

		if (type__[(i__1 = nxttok - 1) < 132 && 0 <= i__1 ? i__1 : 
			s_rnge("type", i__1, "zzrvbf_", (ftnlen)1000)] != 1) {

/*                 First perform the clean up function. */

		    zzcln_(&lookat, &nameat, namlst, datlst, nmpool, chpool, 
			    dppool);
		    r1 = rtrim_(varnam, varnam_len);
		    setmsg_("The kernel variable # has been set up as a stri"
			    "ng variable.  However, the value that you are at"
			    "tempting to assign to this variable on line # of"
			    " the text buffer is not a string value. ", (
			    ftnlen)183);
		    errch_("#", varnam, (ftnlen)1, r1);
		    errint_("#", linnum, (ftnlen)1);
		    sigerr_("SPICE(TYPEMISMATCH)", (ftnlen)19);
		    chkout_("ZZRVBF", (ftnlen)6);
		    return 0;
		}

/*              Still going? Make sure there is something between */
/*              the quotes. */

		if (b + 1 >= e) {

/*                 First perform the clean up function. */

		    zzcln_(&lookat, &nameat, namlst, datlst, nmpool, chpool, 
			    dppool);
		    setmsg_("There is a quoted string with no characters on "
			    "line # of the text buffer. ", (ftnlen)74);
		    errint_("#", linnum, (ftnlen)1);
		    sigerr_("SPICE(TYPEMISMATCH)", (ftnlen)19);
		    chkout_("ZZRVBF", (ftnlen)6);
		    return 0;
		}

/*              We are ready to go.  Allocate a node for this data */
/*              item. First make sure there is room to do so. */

		free = lnknfn_(chpool);
		if (free <= 0) {
		    setmsg_("There is no room available for adding another c"
			    "haracter value to the kernel pool.  The characte"
			    "r values buffer became full at line # of the tex"
			    "t buffer. ", (ftnlen)153);
		    errint_("#", linnum, (ftnlen)1);
		    sigerr_("SPICE(KERNELPOOLFULL)", (ftnlen)21);
		    chkout_("ZZRVBF", (ftnlen)6);
		    return 0;
		}

/*              Allocate a node for storing this string value: */

		lnkan_(chpool, &chnode);
		if (datlst[nameat - 1] == 0) {

/*                 There was no data for this name yet.  We make */
/*                 CHNODE be the head of the data list for this name. */

		    datlst[nameat - 1] = -chnode;
		} else {

/*                 Put this node after the tail of the current list. */

		    head = -datlst[nameat - 1];
		    tail = -chpool[(head << 1) + 11];
		    lnkila_(&tail, &chnode, chpool);
		}

/*              Finally insert this data item in the data buffer */
/*              at CHNODE.  Note any quotes will be doubled so we */
/*              have to undo this affect when we store the data. */

		s_copy(chvals + (chnode - 1) * chvals_len, " ", chvals_len, (
			ftnlen)1);
		++ncomp;
		i__ = 1;
		j = b + 1;
		while(j < e) {
		    code = *(unsigned char *)&line[j - 1];
		    if (code == iquote) {
			++j;
		    }
		    *(unsigned char *)&chvals[(chnode - 1) * chvals_len + (
			    i__ - 1)] = *(unsigned char *)&line[j - 1];
		    ++i__;
		    ++j;
		}

/*              That's all for this value. It's now time to loop */
/*              back through and get the next value. */

	    } else {
		if (type__[(i__1 = nxttok - 1) < 132 && 0 <= i__1 ? i__1 : 
			s_rnge("type", i__1, "zzrvbf_", (ftnlen)1121)] != 2) {

/*                 First perform the clean up function. */

		    zzcln_(&lookat, &nameat, namlst, datlst, nmpool, chpool, 
			    dppool);
		    r1 = rtrim_(varnam, varnam_len);
		    setmsg_("The kernel variable # has been set up as a nume"
			    "ric or time variable.  However, the value that y"
			    "ou are attempting to assign to this variable on "
			    "line # of the kernel buffer is not a numeric or "
			    "time value. ", (ftnlen)203);
		    errch_("#", varnam, (ftnlen)1, r1);
		    errint_("#", linnum, (ftnlen)1);
		    sigerr_("SPICE(TYPEMISMATCH)", (ftnlen)19);
		    chkout_("ZZRVBF", (ftnlen)6);
		    return 0;
		}

/*              Look at the first character to see if we have a time */
/*              or a number. */

		code = *(unsigned char *)&line[b - 1];
		if (code == itmark) {

/*                 We need to have more than a single character. */

		    if (e == b) {

/*                    First perform the clean up function. */

			zzcln_(&lookat, &nameat, namlst, datlst, nmpool, 
				chpool, dppool);
			r1 = rtrim_(varnam, varnam_len);
			setmsg_("At character # of  line # in the text buffe"
				"r the character '@' appears.  This character"
				" is reserved for identifying time values in "
				"assignments to kernel pool variables.  Howev"
				"er it is not being used in this fashion for "
				"the variable '#'. ", (ftnlen)237);
			errint_("#", &b, (ftnlen)1);
			errint_("#", linnum, (ftnlen)1);
			errch_("#", varnam, (ftnlen)1, r1);
			sigerr_("SPICE(BADTIMESPEC)", (ftnlen)18);
			chkout_("ZZRVBF", (ftnlen)6);
			return 0;
		    }
		    i__1 = b;
		    tparse_(line + i__1, &dvalue, error, e - i__1, (ftnlen)
			    256);
		    if (s_cmp(error, " ", (ftnlen)256, (ftnlen)1) != 0) {

/*                    First perform the clean up function. */

			zzcln_(&lookat, &nameat, namlst, datlst, nmpool, 
				chpool, dppool);
			setmsg_("Encountered '#' while attempting to parse a"
				" time on line # of the text buffer. ", (
				ftnlen)79);
			i__1 = b;
			errch_("#", line + i__1, (ftnlen)1, e - i__1);
			errint_("#", linnum, (ftnlen)1);
			sigerr_("SPICE(BADTIMESPEC)", (ftnlen)18);
			chkout_("ZZRVBF", (ftnlen)6);
			return 0;
		    }
		} else {
		    nparsd_(line + (b - 1), &dvalue, error, &i__, e - (b - 1),
			     (ftnlen)256);
		    if (s_cmp(error, " ", (ftnlen)256, (ftnlen)1) != 0) {
			zzcln_(&lookat, &nameat, namlst, datlst, nmpool, 
				chpool, dppool);
			setmsg_("Encountered '#' while attempting to parse a"
				" number on line # of the text buffer", (
				ftnlen)79);
			errch_("#", line + (b - 1), (ftnlen)1, e - (b - 1));
			errint_("#", linnum, (ftnlen)1);
			sigerr_("SPICE(NUMBEREXPECTED)", (ftnlen)21);
			chkout_("ZZRVBF", (ftnlen)6);
			return 0;
		    }
		}

/*              OK. We have a parsed value.  See if there is room in */
/*              the numeric portion of the pool to store this value. */

		free = lnknfn_(dppool);
		if (free <= 0) {
		    setmsg_("There is no room available for adding another n"
			    "umeric value to the kernel pool.  The numeric va"
			    "lues buffer became full at line # of the text bu"
			    "ffer.", (ftnlen)148);
		    errint_("#", linnum, (ftnlen)1);
		    sigerr_("SPICE(KERNELPOOLFULL)", (ftnlen)21);
		    chkout_("ZZRVBF", (ftnlen)6);
		    return 0;
		}

/*              Allocate a node for storing this numeric value: */

		lnkan_(dppool, &dpnode);
		if (datlst[nameat - 1] == 0) {

/*                 There was no data for this name yet.  We make */
/*                 DPNODE be the head of the data list for this name. */

		    datlst[nameat - 1] = dpnode;
		} else {

/*                 Put this node after the tail of the current list. */

		    head = datlst[nameat - 1];
		    tail = -dppool[(head << 1) + 11];
		    lnkila_(&tail, &dpnode, dppool);
		}

/*              Finally insert this data item into the numeric buffer. */

		dpvals[dpnode - 1] = dvalue;
		++ncomp;
	    }

/*           Now process the next token in the list of tokens. */

	    ++nxttok;
	}

/*        We could have ended the above loop in one of two ways. */

/*        1) NXTTOK now exceeds count.  This means we did not reach */
/*           an end of vector marker. */
/*        2) We hit an end of vector marker. */

	if (nxttok > count) {
	    status = 3;
	} else {
	    status = 2;
	}
    }

/*     It is possible that we reached this point without actually */
/*     assigning a value to the kernel pool variable.  This can */
/*     happen if there is a vector input of the form NAME = ( ) */

    if (ncomp < 1) {
	zzcln_(&lookat, &nameat, namlst, datlst, nmpool, chpool, dppool);
	setmsg_("The first item following the assignment operator should be "
		"the value of a variable or a left parenthesis '(' followed b"
		"y a value for a variable. This is not true on line # of the "
		"text buffer. ", (ftnlen)192);
	i__1 = *linnum - 1;
	errint_("#", &i__1, (ftnlen)1);
	sigerr_("SPICE(BADVARASSIGN)", (ftnlen)19);
	chkout_("ZZRVBF", (ftnlen)6);
	return 0;
    }

/*     Return the name of the variable. */

    s_copy(name__, varnam, (ftnlen)132, varnam_len);
    chkout_("ZZRVBF", (ftnlen)6);
    return 0;
} /* zzrvbf_ */

