/* nparsd.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* $Procedure NPARSD ( Double Precision parsing of a string ) */
/* Subroutine */ int nparsd_(char *string, doublereal *x, char *error, 
	integer *ptr, ftnlen string_len, ftnlen error_len)
{
    /* Initialized data */

    static doublereal lookup[11] = { 1.,10.,100.,1e3,1e4,1e5,1e6,1e7,1e8,1e9,
	    1e10 };
    static logical first = TRUE_;
    static doublereal values[128] = { 0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0. };
    static integer class__[129] = { 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0 };

    /* System generated locals */
    integer i__1, i__2, i__3;
    doublereal d__1, d__2;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_rnge(char *, integer, char *, integer);
    double d_lg10(doublereal *), d_int(doublereal *);
    integer s_cmp(char *, char *, ftnlen, ftnlen), i_len(char *, ftnlen), 
	    i_dnnt(doublereal *);

    /* Local variables */
    static doublereal next;
    static integer b;
    extern /* Subroutine */ int zzinssub_(char *, char *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen);
    static integer i__, l, m;
    static logical dodec;
    static integer blank;
    static logical bpiok, epiok;
    extern doublereal dpmax_(void);
    static doublereal value;
    static logical doint, doexp;
    static integer thisi;
    static logical expok;
    static integer nexti;
    static logical zeroi, pntok;
    static integer id;
    extern doublereal pi_(void);
    static integer nl;
    static doublereal decval, factor, intbnd, smlbnd;
    static logical sigchr;
    static char toobig[160];
    static doublereal dpsign[2];
    static logical mantsa, signok, roundd;
    static integer signdx;
    static char blnkst[160];
    static doublereal ecount, divisr, expval, intval, maxexp;
    static char unxpch[160];
    static doublereal minexp;
    static logical roundi;
    static char unrcst[160];
    extern /* Subroutine */ int prefix_(char *, integer *, char *, ftnlen, 
	    ftnlen);
    static char unxpsn[160], unxppt[160];
    static integer exp__;

/* $ Abstract */

/*     Parse a character string that represents a number and return */
/*     a double precision value. */

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

/*     ALPHANUMERIC */
/*     CONVERSION */
/*     PARSING */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  --------------------------------------------------- */
/*     STRING     I   Character string representing a numeric value. */
/*     X          O   Double precision value parsed from STRING. */
/*     ERROR      O   Message indicating whether errors have occurred. */
/*     PTR        O   Position in string where an error occurred. */

/* $ Detailed_Input */

/*     STRING   is a character string that represents a numeric value. */
/*              Commas and spaces may be used in this string for */
/*              ease of reading and writing the number. They */
/*              are treated as insignificant but non-error-producing */
/*              characters. */

/*              For exponential representation the characters */
/*              'E','D','e','d' may be used. */

/*              The following are legitimate numeric expressions */

/*               +12.2 e-1 */
/*               -3. 1415 9276 */
/*               1e12 */
/*               E10 */

/*              The program also recognizes the following  mnemonics */
/*              'PI', 'pi', 'Pi', 'pI' */
/*              '+PI', '+pi', '+Pi', '+pI' */
/*              '-PI', '-pi', '-Pi', '-pI' */
/*              and returns the value */
/*              ( + OR - ) 3.1415 9265 3589 7932 3846 2600 D0 as */
/*              appropriate. */

/* $ Detailed_Output */

/*     X        double precision parsed value of input string. If an */
/*              error is encountered, X is not changed. */

/*     ERROR    is a message indicating that the string could */
/*              not be parsed due to use of an unexpected or misplaced */
/*              character or due to a string representing a number */
/*              too large for double precision. If the number was */
/*              successfully parsed, ERROR will be returned as a blank. */

/*              In particular, blank strings, or strings that do not */
/*              contain either a digit or exponent character will */
/*              be regarded as errors. */

/*     PTR      this indicates which character was being used when */
/*              the error occurred. If no error occurs, PTR is */
/*              returned as 0. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  If the string is non-numeric, PTR indicates the location in */
/*         the string where the error occurred, and ERROR contains a */
/*         descriptive error message. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine parses an input character string that represents a */
/*     number, checks for overflow, unexpected or misplaced */
/*     characters. It returns the double precision number or an error */
/*     message. */

/* $ Examples */

/*     Let   LINE = 'DELTA_T_A       =   32.184' */

/*     The following code fragment parses the line and obtains the */
/*     double precision value. */


/*        CALL NEXTWD ( LINE,  FIRST,  REST ) */
/*        CALL NEXTWD ( REST, SECOND,  REST ) */
/*        CALL NEXTWD ( REST,  THIRD,  REST ) */

/*        CALL NPARSD (  THIRD,  VALUE, ERROR, PTR    ) */

/* $ Restrictions */

/*     1)  Due to rounding errors this routine may not be able to parse */
/*         the decimal character string representation of the largest */
/*         and smallest double precision numbers. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     H.A. Neilan        (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 3.6.0, 12-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Removed */
/*        unnecessary $Revisions section. */

/* -    SPICELIB Version 3.5.0, 15-AUG-2002 (WLT) */

/*        Replaced the call to INSSUB with a call to ZZINSSUB so */
/*        that this routine can legitimately call itself Error Free */

/* -    SPICELIB Version 3.4.0, 03-DEC-2001 (NJB) */

/*        Added an extra check to make sure that ICHAR of any character */
/*        of the input string is positive. */

/* -    SPICELIB Version 3.3.0, 29-FEB-1996 (KRG) */

/*        The declaration for the SPICELIB function PI is now */
/*        preceded by an EXTERNAL statement declaring PI to be an */
/*        external function. This removes a conflict with any */
/*        compilers that have a PI intrinsic function. */

/*        Removed the error message and storage for the unexpected */
/*        comma error message. This variable was set but never used, */
/*        and according to the spec for this routine a comma is a valid */
/*        delimiter, treated like a space, within numbers. */

/* -    SPICELIB Version 3.2.0, 10-JAN-1995 (WLT) */

/*        Changed error strings from parameters to assignments to */
/*        compensate for shortcomings of the Absoft FORTRAN compiler */
/*        on the NeXT. */

/* -    SPICELIB Version 3.1.0, 12-JUL-1994 (WLT) */

/*        The previous version of the routine assumed that the range */
/*        of values of ICHAR was 0 to 128. That turns out not to be */
/*        true on some machines. If a character whose ICHAR value is */
/*        outside this range is detected, it is now handled properly */
/*        as an unexpected character. */

/* -    SPICELIB Version 3.0.0, 24-FEB-1993 (WLT) */

/*        The previous version of the algorithm interpreted P or p as 1. */
/*        This was not the intent of the routine and was corrected. */

/* -    SPICELIB Version 2.0.0, 28-AUG-1992 (WLT) (KRG) */

/*        The basic algorithm was completely re-written. As a result */
/*        the routine now runs an order of magnitude faster than */
/*        it did before. In addition, strings that do not contain */
/*        enough information to assign a value to the string are now */
/*        regarded as errors. These include blank strings or strings */
/*        that contain only a sign characters, blanks and commas. */

/*        In addition the error diagnosis and checking for overflow */
/*        was greatly enhanced. */

/*        Note: strings may now parse with slightly different values */
/*        from the previous version of NPARSD. The current */
/*        implementation is more accurate in converting strings to */
/*        double precision numbers. */

/* -    SPICELIB Version 1.1.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.1.0, 17-APR-1990 (WLT) */

/*        Bug fix. The subscript used to reference individual characters */
/*        of the input string could sometimes step out of bounds. This */
/*        went unnoticed until NAIF began compiling with the CHECK=BOUNDS */
/*        option of the DEC Fortran compiler. */


/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (HAN) (NJB) */

/* -& */
/* $ Index_Entries */

/*     parse a character_string to a d.p. number */

/* -& */

/*     SPICELIB functions. */


/*     Local Parameters. */


/*     Save everything.  It's easier than tracking down every */
/*     little variable that might need to be saved. */

    if (first) {
	first = FALSE_;

/*        Set up the error messages */

	s_copy(toobig, "The number represented by the input string is too la"
		"rge to be stored as a double precision number. ", (ftnlen)160,
		 (ftnlen)99);
	s_copy(unxpch, "An unexpected character was found while attempting t"
		"o parse the input string. ", (ftnlen)160, (ftnlen)78);
	s_copy(unxppt, "An unexpected decimal point was found in the input s"
		"tring. ", (ftnlen)160, (ftnlen)59);
	s_copy(unxpsn, "An unexpected sign character was found in the input "
		"string. ", (ftnlen)160, (ftnlen)60);
	s_copy(blnkst, "The input string is blank. Blank strings are not con"
		"sidered to be numbers. ", (ftnlen)160, (ftnlen)75);
	s_copy(unrcst, "The input string could not be recognized as a number"
		". ", (ftnlen)160, (ftnlen)54);
	blank = ' ';
	values[(i__1 = '0' - 1) < 128 && 0 <= i__1 ? i__1 : s_rnge("values", 
		i__1, "nparsd_", (ftnlen)412)] = 0.;
	values[(i__1 = '1' - 1) < 128 && 0 <= i__1 ? i__1 : s_rnge("values", 
		i__1, "nparsd_", (ftnlen)413)] = 1.;
	values[(i__1 = '2' - 1) < 128 && 0 <= i__1 ? i__1 : s_rnge("values", 
		i__1, "nparsd_", (ftnlen)414)] = 2.;
	values[(i__1 = '3' - 1) < 128 && 0 <= i__1 ? i__1 : s_rnge("values", 
		i__1, "nparsd_", (ftnlen)415)] = 3.;
	values[(i__1 = '4' - 1) < 128 && 0 <= i__1 ? i__1 : s_rnge("values", 
		i__1, "nparsd_", (ftnlen)416)] = 4.;
	values[(i__1 = '5' - 1) < 128 && 0 <= i__1 ? i__1 : s_rnge("values", 
		i__1, "nparsd_", (ftnlen)417)] = 5.;
	values[(i__1 = '6' - 1) < 128 && 0 <= i__1 ? i__1 : s_rnge("values", 
		i__1, "nparsd_", (ftnlen)418)] = 6.;
	values[(i__1 = '7' - 1) < 128 && 0 <= i__1 ? i__1 : s_rnge("values", 
		i__1, "nparsd_", (ftnlen)419)] = 7.;
	values[(i__1 = '8' - 1) < 128 && 0 <= i__1 ? i__1 : s_rnge("values", 
		i__1, "nparsd_", (ftnlen)420)] = 8.;
	values[(i__1 = '9' - 1) < 128 && 0 <= i__1 ? i__1 : s_rnge("values", 
		i__1, "nparsd_", (ftnlen)421)] = 9.;
	values[(i__1 = '-' - 1) < 128 && 0 <= i__1 ? i__1 : s_rnge("values", 
		i__1, "nparsd_", (ftnlen)422)] = -1.;
	values[(i__1 = '+' - 1) < 128 && 0 <= i__1 ? i__1 : s_rnge("values", 
		i__1, "nparsd_", (ftnlen)423)] = 1.;
	class__[(i__1 = ' ') < 129 && 0 <= i__1 ? i__1 : s_rnge("class", i__1,
		 "nparsd_", (ftnlen)425)] = 4;
	class__[(i__1 = ',') < 129 && 0 <= i__1 ? i__1 : s_rnge("class", i__1,
		 "nparsd_", (ftnlen)426)] = 4;
	class__[(i__1 = '.') < 129 && 0 <= i__1 ? i__1 : s_rnge("class", i__1,
		 "nparsd_", (ftnlen)428)] = 2;
	class__[(i__1 = 'E') < 129 && 0 <= i__1 ? i__1 : s_rnge("class", i__1,
		 "nparsd_", (ftnlen)430)] = 3;
	class__[(i__1 = 'D') < 129 && 0 <= i__1 ? i__1 : s_rnge("class", i__1,
		 "nparsd_", (ftnlen)431)] = 3;
	class__[(i__1 = 'e') < 129 && 0 <= i__1 ? i__1 : s_rnge("class", i__1,
		 "nparsd_", (ftnlen)432)] = 3;
	class__[(i__1 = 'd') < 129 && 0 <= i__1 ? i__1 : s_rnge("class", i__1,
		 "nparsd_", (ftnlen)433)] = 3;
	class__[(i__1 = '+') < 129 && 0 <= i__1 ? i__1 : s_rnge("class", i__1,
		 "nparsd_", (ftnlen)435)] = 7;
	class__[(i__1 = '-') < 129 && 0 <= i__1 ? i__1 : s_rnge("class", i__1,
		 "nparsd_", (ftnlen)436)] = 7;
	class__[(i__1 = '1') < 129 && 0 <= i__1 ? i__1 : s_rnge("class", i__1,
		 "nparsd_", (ftnlen)438)] = 1;
	class__[(i__1 = '2') < 129 && 0 <= i__1 ? i__1 : s_rnge("class", i__1,
		 "nparsd_", (ftnlen)439)] = 1;
	class__[(i__1 = '3') < 129 && 0 <= i__1 ? i__1 : s_rnge("class", i__1,
		 "nparsd_", (ftnlen)440)] = 1;
	class__[(i__1 = '4') < 129 && 0 <= i__1 ? i__1 : s_rnge("class", i__1,
		 "nparsd_", (ftnlen)441)] = 1;
	class__[(i__1 = '5') < 129 && 0 <= i__1 ? i__1 : s_rnge("class", i__1,
		 "nparsd_", (ftnlen)442)] = 1;
	class__[(i__1 = '6') < 129 && 0 <= i__1 ? i__1 : s_rnge("class", i__1,
		 "nparsd_", (ftnlen)443)] = 1;
	class__[(i__1 = '7') < 129 && 0 <= i__1 ? i__1 : s_rnge("class", i__1,
		 "nparsd_", (ftnlen)444)] = 1;
	class__[(i__1 = '8') < 129 && 0 <= i__1 ? i__1 : s_rnge("class", i__1,
		 "nparsd_", (ftnlen)445)] = 1;
	class__[(i__1 = '9') < 129 && 0 <= i__1 ? i__1 : s_rnge("class", i__1,
		 "nparsd_", (ftnlen)446)] = 1;
	class__[(i__1 = '0') < 129 && 0 <= i__1 ? i__1 : s_rnge("class", i__1,
		 "nparsd_", (ftnlen)447)] = 1;
	class__[(i__1 = 'p') < 129 && 0 <= i__1 ? i__1 : s_rnge("class", i__1,
		 "nparsd_", (ftnlen)449)] = 5;
	class__[(i__1 = 'P') < 129 && 0 <= i__1 ? i__1 : s_rnge("class", i__1,
		 "nparsd_", (ftnlen)450)] = 5;
	class__[(i__1 = 'i') < 129 && 0 <= i__1 ? i__1 : s_rnge("class", i__1,
		 "nparsd_", (ftnlen)451)] = 6;
	class__[(i__1 = 'I') < 129 && 0 <= i__1 ? i__1 : s_rnge("class", i__1,
		 "nparsd_", (ftnlen)452)] = 6;

/*        Finally create the numbers that will be used for checking */
/*        for floating point overflow. */

/*        NOTE: The value for MINEXP may be too small by one, but it */
/*              really doesn't make any difference, as you're going to */
/*              underflow anyway, and dividing zero by a number (BASE) */
/*              still gives you zero. */

	d__2 = dpmax_();
	d__1 = d_lg10(&d__2);
	maxexp = d_int(&d__1);
	minexp = -(maxexp + 1);
	smlbnd = dpmax_() / lookup[10];
	intbnd = 10.;
	next = intbnd + 1.;
	while(intbnd != next) {
	    intbnd *= 10.;
	    next = intbnd + 1.;
	}
	intbnd /= 10.;

/*        That takes care of the first pass initializations. */

    }

/*     Here's what's true right now. */

/*     There are no errors. */
/*     The error pointer doesn't need to point anywhere. */
/*     It's ok for the next token to be a decimal point. */
/*     It's ok for the next token to be a sign character. */
/*     It's ok for the next token to be an exponent marker. */
/*     It's ok for the next character to be the start of pi. */

/*     We expect to be constructing the integer part of the */
/*     numeric string. */

    s_copy(error, " ", error_len, (ftnlen)1);
    *ptr = 0;
    pntok = TRUE_;
    signok = TRUE_;
    expok = TRUE_;
    bpiok = TRUE_;
    doint = TRUE_;
    roundd = TRUE_;
    roundi = TRUE_;

/*     Here's some other facts. */

/*     We are not parsing the decimal part of the string. */
/*     We are not parsing the exponent part of the string. */
/*     We have not encountered any digits in the mantissa. */
/*     We have not encountered any significant characters. */
/*     It's not ok for the next character to be the end of pi (i). */

    dodec = FALSE_;
    doexp = FALSE_;
    mantsa = FALSE_;
    sigchr = FALSE_;
    epiok = FALSE_;

/*     So far there is no integer, decimal or exponent part to this */
/*     string. */

    intval = 0.;
    decval = 0.;
    expval = 0.;
    divisr = 1.;
    factor = 1.;
    ecount = 0.;

/*     Right now if we encounter a sign, it's part of the mantissa. */
/*     And until we know better the sign of both the mantissa and */
/*     exponent are +1 (as opposed to -1). */

    signdx = 1;
    dpsign[0] = 1.;
    dpsign[1] = 1.;

/*     Before doing anything else we determine whether or not */
/*     the input string is empty. */

    if (s_cmp(string, " ", string_len, (ftnlen)1) == 0) {
	s_copy(error, blnkst, error_len, (ftnlen)160);
	*ptr = 1;
	return 0;
    }

/*     We need to find the last non-blank character of the input */
/*     string.  We shall use the idea of binary searching to locate */
/*     this character.  At first this may appear to be a bit convoluted */
/*     when compared to the obvious thing to do (start at the end of */
/*     the string and step backward until a non-blank character is */
/*     located).  However, on every machine we've looked at this method */
/*     locates the last non-blank character much more quickly on average */
/*     than the obvious method. */

/*     L and B denote the last and beginning characters */
/*     of the substring we are searching.  NL is the next to last */
/*     character that we are concerned with and M is the middle of */
/*     the current search interval ( from B to NL ). */

    l = i_len(string, string_len);
    b = 1;
    nl = l - 1;

/*     We want M to be ( B + NL ) / 2   but right now that's L/2 */

    m = l / 2;
    while(l - b > 16) {

/*        What is true right now?  The string from L+1 on out */
/*        is blank.  L > B; L-1 = NL >= B;  M = (B + NL) / 2; */
/*        and M >= B,  B is at least one and if greater than 1 */
/*        there must be a non-blank character between B and the */
/*        end of the string. */

	if (*(unsigned char *)&string[l - 1] != blank) {
	    b = l;
	} else if (s_cmp(string + (m - 1), " ", nl - (m - 1), (ftnlen)1) == 0)
		 {

/*           If you got here, the STRING(L:L) is a blank. */
/*           The string from L+1 on out is blank. */
/*           The string from M to NL (=L-1) is blank.  Thus the */
/*           string from M out is blank. */

/*           M is greater than or equal to B. */
/*           If M  is less than B + 2, then L will become */
/*           B or less and there will not be a */
/*           next pass through the loop.  That means that */
/*           we will never get to this point again and don't */
/*           have to worry about the reference STRING(M:NL) */
/*           giving us an access violation. */

	    l = m - 1;

/*           With the new value of L, we now know that STRING(L+1:) */
/*           is blank. */

	} else {

/*           If you get to this point all of the string from */
/*           L out is blank and L is greater than M. */
/*           There is a non-blank character between M and NL. */
/*           If L should get within 16 of B, then the loop */
/*           will not be executed again.  That means again that */
/*           we don't have to worry about STRING(M:NL) being */
/*           an ill formed string. */

	    l = nl;
	    b = m;

/*           With the new value of L, we now know that STRING(L+1:) */
/*           is blank. */

	}

/*        Finally compute NL,the index of the character that precedes */
/*        L and the new midpoint of the stuff from B to NL. */

	nl = l - 1;
	m = (b + nl) / 2;

/*        What's true now?  The string from L+1 on out is blank. */

    }

/*     L is now within 16 characters of the last non-blank character */
/*     of the input string.  We simply search backward from L to */
/*     locate this last non-blank. */

    while(*(unsigned char *)&string[l - 1] == blank) {
	--l;
    }

/*     Begin to collect the number in its various parts: an integer */
/*     portion, a fractional portion, and an exponent. */

    i__1 = l;
    for (i__ = 1; i__ <= i__1; ++i__) {
	id = *(unsigned char *)&string[i__ - 1];
	if (id > 128 || id < 0) {

/*           This is definitely not expected.  Set the error message */
/*           and return. */

	    nexti = i__ + 1;
	    thisi = i__;
	    zzinssub_(string, "]", &nexti, error, string_len, (ftnlen)1, 
		    error_len);
	    zzinssub_(error, "[", &thisi, error, error_len, (ftnlen)1, 
		    error_len);
	    prefix_(unxpch, &c__1, error, (ftnlen)160, error_len);
	    *ptr = i__;
	    return 0;

/*        The action taken depends upon the class of the token. */

	} else if (class__[(i__2 = id) < 129 && 0 <= i__2 ? i__2 : s_rnge(
		"class", i__2, "nparsd_", (ftnlen)675)] == 1) {

/*           Once a digit has been encountered, we can no longer */
/*           allow the string 'PI' or a sign until an exponent */
/*           character is hit and resets the SIGNOK flag. */

	    bpiok = FALSE_;
	    epiok = FALSE_;
	    signok = FALSE_;
	    sigchr = TRUE_;

/*           If we are constructing the integer part ... */

	    if (doint) {
		mantsa = TRUE_;

/*              Check the current value of the integer part to */
/*              make sure we don't overflow. */

		if (intval < intbnd) {
		    intval = intval * 10. + values[(i__2 = id - 1) < 128 && 0 
			    <= i__2 ? i__2 : s_rnge("values", i__2, "nparsd_",
			     (ftnlen)697)];
		} else {

/*                 Once the integer exceeds a given bound, */
/*                 we add the rest on as fractional part and */
/*                 keep track of the factor we will need to */
/*                 multiply the decimal part by to scale things */
/*                 appropriately.  We also keep track of the number */
/*                 we will need to add to the exponent part. */

		    ecount += 1;
		    factor /= 10.;
		    if (roundi) {
			roundi = FALSE_;
			if (values[(i__2 = id - 1) < 128 && 0 <= i__2 ? i__2 :
				 s_rnge("values", i__2, "nparsd_", (ftnlen)
				715)] > 5.) {
			    intval += 1.;
			}
		    }
		}

/*           ... or the decimal part ... */

	    } else if (dodec) {
		mantsa = TRUE_;

/*              There are two cases to consider.  The case in which */
/*              the integer portion of the string has value 0... */

		if (zeroi) {

/*                 We can just keep accumulating the decimal part */
/*                 as an integer.  But we keep track of how many */
/*                 places past the decimal point the first non-zero */
/*                 digit occurs.  Note that once the decimal part */
/*                 exceeds the integer bound, we don't need to do */
/*                 anything.  The remaining digits cannot contribute */
/*                 to the value of the decimal part. */

		    if (decval < intbnd) {
			decval = decval * 10. + values[(i__2 = id - 1) < 128 
				&& 0 <= i__2 ? i__2 : s_rnge("values", i__2, 
				"nparsd_", (ftnlen)744)];
			ecount += -1;
		    } else if (roundd) {
			roundd = FALSE_;
			if (values[(i__2 = id - 1) < 128 && 0 <= i__2 ? i__2 :
				 s_rnge("values", i__2, "nparsd_", (ftnlen)
				751)] >= 5.) {
			    decval += 1.;
			}
		    }

/*              ...and the case in which the integer portion is not */
/*              zero. */

		} else {

/*                 In this case, we know there is at least _something_ */
/*                 to the integer part of this string.  We can */
/*                 stop accumulating the decimal part when the divisor */
/*                 portion exceeds the integer barrier.  After that */
/*                 the extra digits can't make any contribution to */
/*                 the double precision value given to the string. */

		    if (divisr < intbnd) {
			decval = decval * 10. + values[(i__2 = id - 1) < 128 
				&& 0 <= i__2 ? i__2 : s_rnge("values", i__2, 
				"nparsd_", (ftnlen)771)];
			divisr *= 10.;
		    }
		}

/*           ...or the exponent part of the string. */

	    } else if (doexp) {
		if (expval + ecount > maxexp) {

/*                 This number is too big to put into a double */
/*                 precision number. The marginal case where */
/*                 EXPVAL + ECOUNT .EQ. MAXEXP will be dealt */
/*                 with when the integer and fractional parts */
/*                 of the double precision number are built */
/*                 at the end of this routine. */

		    s_copy(error, toobig, error_len, (ftnlen)160);
		    *ptr = i__;
		    return 0;
		} else if (expval + ecount < minexp) {

/*                 This number is going to underflow, we can */
/*                 just stop accumulating exponent. But we don't */
/*                 stop parsing the string yet. There might be */
/*                 a bad character lurking somewhere later in the */
/*                 string. */

/*                 NOTE: It is also possible to underflow when the */
/*                       value of EXPVAL + ECOUNT is equal to MINEXP, */
/*                       since an entire 'BASE' scale is not supported */
/*                       for this particular exponent. */

		} else {

/*                 This is the case we expect.  Just add on the */
/*                 next part of the exponent. */

		    expval = expval * 10. + dpsign[1] * values[(i__2 = id - 1)
			     < 128 && 0 <= i__2 ? i__2 : s_rnge("values", 
			    i__2, "nparsd_", (ftnlen)813)];
		}

/*           Even though this character is a digit, its not expected */
/*           for some reason.  Set the error flag and return. */

	    } else {
		nexti = i__ + 1;
		thisi = i__;
		zzinssub_(string, "]", &nexti, error, string_len, (ftnlen)1, 
			error_len);
		zzinssub_(error, "[", &thisi, error, error_len, (ftnlen)1, 
			error_len);
		prefix_(unxpch, &c__1, error, (ftnlen)160, error_len);
		*ptr = i__;
		return 0;
	    }
	} else if (class__[(i__2 = id) < 129 && 0 <= i__2 ? i__2 : s_rnge(
		"class", i__2, "nparsd_", (ftnlen)834)] == 2) {
	    if (pntok) {
		bpiok = FALSE_;
		epiok = FALSE_;
		pntok = FALSE_;
		signok = FALSE_;
		dodec = TRUE_;
		doint = FALSE_;
		doexp = FALSE_;
		zeroi = intval == 0.;
	    } else {
		nexti = i__ + 1;
		thisi = i__;
		zzinssub_(string, "]", &nexti, error, string_len, (ftnlen)1, 
			error_len);
		zzinssub_(error, "[", &thisi, error, error_len, (ftnlen)1, 
			error_len);
		prefix_(unxppt, &c__1, error, (ftnlen)160, error_len);
		*ptr = i__;
		return 0;
	    }
	} else if (class__[(i__2 = id) < 129 && 0 <= i__2 ? i__2 : s_rnge(
		"class", i__2, "nparsd_", (ftnlen)861)] == 3) {
	    sigchr = TRUE_;
	    if (expok) {
		bpiok = FALSE_;
		epiok = FALSE_;
		expok = FALSE_;
		pntok = FALSE_;
		dodec = FALSE_;
		doint = FALSE_;
		doexp = TRUE_;
		signok = TRUE_;
		signdx = 2;
	    } else {
		nexti = i__ + 1;
		thisi = i__;
		zzinssub_(string, "]", &nexti, error, string_len, (ftnlen)1, 
			error_len);
		zzinssub_(error, "[", &thisi, error, error_len, (ftnlen)1, 
			error_len);
		prefix_(unxpch, &c__1, error, (ftnlen)160, error_len);
		*ptr = i__;
		return 0;
	    }
	} else if (class__[(i__2 = id) < 129 && 0 <= i__2 ? i__2 : s_rnge(
		"class", i__2, "nparsd_", (ftnlen)891)] == 7) {
	    if (signok) {
		dpsign[(i__2 = signdx - 1) < 2 && 0 <= i__2 ? i__2 : s_rnge(
			"dpsign", i__2, "nparsd_", (ftnlen)895)] = values[(
			i__3 = id - 1) < 128 && 0 <= i__3 ? i__3 : s_rnge(
			"values", i__3, "nparsd_", (ftnlen)895)];
		signok = FALSE_;
	    } else {
		nexti = i__ + 1;
		thisi = i__;
		zzinssub_(string, "]", &nexti, error, string_len, (ftnlen)1, 
			error_len);
		zzinssub_(error, "[", &thisi, error, error_len, (ftnlen)1, 
			error_len);
		prefix_(unxpsn, &c__1, error, (ftnlen)160, error_len);
		*ptr = i__;
		return 0;
	    }
	} else if (class__[(i__2 = id) < 129 && 0 <= i__2 ? i__2 : s_rnge(
		"class", i__2, "nparsd_", (ftnlen)912)] == 5) {
	    sigchr = TRUE_;
	    if (bpiok) {
		doint = FALSE_;
		dodec = FALSE_;
		doexp = FALSE_;
		expok = FALSE_;
		pntok = FALSE_;
		bpiok = FALSE_;
		signok = FALSE_;
		epiok = TRUE_;
	    } else {
		nexti = i__ + 1;
		thisi = i__;
		zzinssub_(string, "]", &nexti, error, string_len, (ftnlen)1, 
			error_len);
		zzinssub_(error, "[", &thisi, error, error_len, (ftnlen)1, 
			error_len);
		prefix_(unxpch, &c__1, error, (ftnlen)160, error_len);
		*ptr = i__;
		return 0;
	    }
	} else if (class__[(i__2 = id) < 129 && 0 <= i__2 ? i__2 : s_rnge(
		"class", i__2, "nparsd_", (ftnlen)941)] == 6) {
	    if (epiok) {
		doint = FALSE_;
		dodec = FALSE_;
		doexp = FALSE_;
		expok = FALSE_;
		pntok = FALSE_;
		bpiok = FALSE_;
		signok = FALSE_;
		epiok = FALSE_;
		mantsa = TRUE_;
		intval = pi_();
	    } else {
		nexti = i__ + 1;
		thisi = i__;
		zzinssub_(string, "]", &nexti, error, string_len, (ftnlen)1, 
			error_len);
		zzinssub_(error, "[", &thisi, error, error_len, (ftnlen)1, 
			error_len);
		prefix_(unxpch, &c__1, error, (ftnlen)160, error_len);
		*ptr = i__;
		return 0;
	    }
	} else if (class__[(i__2 = id) < 129 && 0 <= i__2 ? i__2 : s_rnge(
		"class", i__2, "nparsd_", (ftnlen)971)] == 4) {

/*           We don't do anything. */

	} else {

/*           This is definitely not expected.  Set the error message */
/*           and return. */

	    nexti = i__ + 1;
	    thisi = i__;
	    zzinssub_(string, "]", &nexti, error, string_len, (ftnlen)1, 
		    error_len);
	    zzinssub_(error, "[", &thisi, error, error_len, (ftnlen)1, 
		    error_len);
	    prefix_(unxpch, &c__1, error, (ftnlen)160, error_len);
	    *ptr = i__;
	    return 0;
	}
    }

/*     If we got through the loop and it's OK to end PI, then we started */
/*     it but never finished.  This is an error. */

    if (epiok) {
	s_copy(error, unrcst, error_len, (ftnlen)160);
	*ptr = l;
	return 0;
    }

/*     Put together the portion that does not involve an exponent. */

/*     If */
/*        (1) MANTSA = .TRUE., then we had some explicit part of a */
/*            number, an  integer part, a fractional part, or both. */

/*        (2) SIGCHR = .TRUE, then we had either: */

/*            (a) MANTSA = .TRUE. */

/*         or */

/*            (b) there was an implicit value associated with the input */
/*                string. For example, an exponent character followed */
/*                by an optional exponent would produce a valid number: */
/*                E+10 --> 1.0d+10. This is due to the fact that this */
/*                routine emulates an RPN calculator of popular repute, */
/*                not because it is inherently a good idea. */

    if (mantsa) {

/*        We had an integer part of the number, a fractional part, or */
/*        both, so we need to put them together in an appropriate */
/*        fashion. */

	value = intval + decval / divisr * factor;
    } else if (sigchr) {

/*        We do not have a mantissa, so we had an  implicit mantissa, */
/*        see above, so we need to set the value to one. */

	value = 1.;
    } else {

/*        We have an error. There were no significant characters in the */
/*        input character string, and hence we could not parse it into */
/*        a number. An example of such a string would be: '+  ,,.,,'. */
/*        So, we will set an appropriate error message and return. */

	s_copy(error, unrcst, error_len, (ftnlen)160);
	*ptr = i_len(string, string_len) + 1;
	return 0;
    }

/*     Adjust the entered part of the exponent by the amount */
/*     we "shifted" the decimal point when we were computing */
/*     the integer and decimal values. */

    expval += ecount;

/*     Now take care of the exponent contribution to the answer. */

/*     If the exponent is negative ... */

    if (expval < 0.) {
	while(expval < -10.) {
	    value /= lookup[10];
	    expval += 10.;
	}
	value /= lookup[(i__1 = -((integer) expval)) < 11 && 0 <= i__1 ? i__1 
		: s_rnge("lookup", i__1, "nparsd_", (ftnlen)1075)];

/*     If the exponent is positive ... */

    } else if (expval > 0.) {
	while(expval > 10.) {

/*           Make sure that a multiply isn't going to create */
/*           a number that overflows. */

	    if (value >= smlbnd) {
		s_copy(error, toobig, error_len, (ftnlen)160);
		*ptr = i_len(string, string_len) + 1;
		return 0;
	    } else {
		value *= lookup[10];
		expval += -10.;
	    }
	}
	exp__ = i_dnnt(&expval);

/*        Again, make sure that a floating point overflow isn't */
/*        going to happen. */

	if (value < dpmax_() / lookup[(i__1 = exp__) < 11 && 0 <= i__1 ? i__1 
		: s_rnge("lookup", i__1, "nparsd_", (ftnlen)1108)]) {
	    value *= lookup[(i__1 = exp__) < 11 && 0 <= i__1 ? i__1 : s_rnge(
		    "lookup", i__1, "nparsd_", (ftnlen)1110)];
	} else {
	    s_copy(error, toobig, error_len, (ftnlen)160);
	    *ptr = i_len(string, string_len) + 1;
	    return 0;
	}
    }
    *x = dpsign[0] * value;
    return 0;
} /* nparsd_ */

