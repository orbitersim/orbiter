/* zzphsh.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ZZPHSH ( Private---kernel POOL hash function umbrella ) */
integer zzphsh_0_(int n__, char *word, integer *m, integer *m2, ftnlen 
	word_len)
{
    /* Initialized data */

    static logical first = TRUE_;
    static integer divisr = -1;

    /* System generated locals */
    integer ret_val, i__1, i__2, i__3, i__4;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer), i_len(char *, ftnlen);

    /* Local variables */
    static integer base, f, i__, blank;
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen);
    static integer length, maxdiv;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    extern integer intmax_(void);
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen);
    static integer val[129];

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     This is an umbrella routine for the kernel POOL hash function. */
/*     It should never be called directly. */

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

/*     PRIVATE UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  Entry point */
/*     --------  ---  -------------------------------------------------- */
/*     WORD       I   ZZHASH, ZZHASH2 */
/*     M          I   ZZSHSH */
/*     M2         I   ZZHASH2 */

/*     The function returns zero. */

/* $ Detailed_Input */

/*     See individual entry points. */

/* $ Detailed_Output */

/*     The function ZZPHSH should never be called. However, it returns */
/*     the value zero. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine is an umbrella for the kernel POOL hash function */
/*     ZZHASH and its set up routine ZZSHSH, and for an arbitrary */
/*     divisor hash function ZZHASH2 that uses the same algorithm as */
/*     ZZHASH. */

/*     ZZSHSH and ZZHASH are intended to be used ONLY by the POOL */
/*     subsystem. ZZSHSH must be called once to save the POOL-specific */
/*     hash divisor and to initialize character-code map prior to the */
/*     first call to ZZHASH. */

/*     ZZHASH2 can be used with with an arbitrary divisor (recommended */
/*     to be a prime number) that is passed in as an input argument. */
/*     If ZZHASH2 is called prior to ZZSHSH, it does the same */
/*     character-code map initialization. */

/*     The algorithm implemented in ZZHASH and ZZHASH2 is */
/*     case-insensitive and uses only the first word of the input */
/*     string. In order to make effective use of this hash algorithm, */
/*     input strings should be left-justified and space-less. */

/* $ Examples */

/*     To make use of the ZZHASH hash function the POOL subsystem first */
/*     calls ZZSHSH. The value returned by ZZSHSH has no meaning and can */
/*     bed assigned to any temporary variable. Then ZZHASH can be used */
/*     as needed by the POOL routine to compute hash value for the POOL */
/*     keywords. */

/*        I = ZZSHSH ( M ) */

/*           ...any other set up code... */

/*        LOOKAT = ZZHASH ( WORD ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     1)  Knuth, Donald E. "The Art of Computer Programming, Volume */
/*         3/Sorting and Searching 2nd Edition" 1997, pp 513-521. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */
/*     E.D. Wright     (JPL) */
/*     B.V. Semenov    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.2, 31-JUL-2013 (BVS) */

/*        Added more details to the header. Added initialization of POOL */
/*        divisor to -1 to allow for initialization error checking on */
/*        the first call to ZZHASH. */

/* -    SPICELIB Version 1.1.1, 21-NOV-2006 (EDW)(BVS) */

/*        Replaced ICHAR('\\') expression with parameter */
/*        BSLASH, the parameter set to the ASCII value */
/*        of the backslash character, 92. */

/* -    SPICELIB Version 1.1.0, 14-SEP-2005 (EDW) */

/*        Added function ZZHASH2. Operation matches */
/*        that of ZZHASH with the exception that ZZHASH2 */
/*        accepts the divisor value, M, as an input. */

/* -    SPICELIB Version 1.0.0, 20-SEP-1995 (WLT) */

/* -& */

/*     Entry Points */


/*     SPICELIB functions */


/*     Local Variables. */

    switch(n__) {
	case 1: goto L_zzshsh;
	case 2: goto L_zzhash;
	case 3: goto L_zzhash2;
	}


/*     We do not diagnose a bogus call since this is a private routine. */

    ret_val = 0;
    return ret_val;
/* $Procedure ZZSHSH ( Private---Set up POOL hash function ) */

L_zzshsh:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     This routine sets up the kernel POOL hash function. Call it once */
/*     per program execution prior to the first call to ZZHASH. */

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

/*     PRIVATE UTILITY */

/* $ Declarations */

/*     INTEGER               M */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     M          I   Divisor used for the POOL hash function */

/*     The function returns 0. */

/* $ Detailed_Input */

/*     M           is the divisor of the hashing function. It is */
/*                 recommended that this be a prime number nominally */
/*                 equal to the maximum number of POOL variables. */
/*                 The value of M must be in the range from 1 to */
/*                 INTMAX/68 - 1. */

/* $ Detailed_Output */

/*     The function returns the value zero (0). */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If the input divisor value is not in the allowed range, the */
/*        error 'SPICE(INVALIDDIVISOR)' will be signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This entry point saves the divisor used for hashing input */
/*     strings. It must be called once by an initialization branch of */
/*     the kernel POOL. */

/*     The character-code map initialized by this function is */
/*     case-insensitive. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     This entry must NOT be called by any subsystem except the */
/*     kernel POOL. */

/* $ Literature_References */

/*     1)  Knuth, Donald E. "The Art of Computer Programming, Volume */
/*         3/Sorting and Searching 2nd Edition" 1997, pp 513-521. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */
/*     E.D. Wright     (JPL) */
/*     B.V. Semenov    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.2, 31-JUL-2013 (BVS) */

/*        Added more details to the header. Added computation of */
/*        the maximum allowed divisor value to the initialization */
/*        block. Added a check for the input divisor to be in the */
/*        allowed range. */

/* -    SPICELIB Version 1.1.1, 21-NOV-2006 (EDW)(BVS) */

/*        Replaced ICHAR('\\') expression with parameter */
/*        BSLASH, the parameter set to the ASCII value */
/*        of the backslash character, 92. */

/* -    SPICELIB Version 1.1.0, 06-JUL-2005 (EDW) */

/*        Added punctuation marks to array of allowed */
/*        characters. The function can process any */
/*        character with ASCII decimal value 33 to 122. */

/* -    SPICELIB Version 1.0.0, 20-SEP-1995 (WLT) */

/* -& */

/*     Return zero. */

    ret_val = 0;

/*     The initialization block below is identical to the initialization */
/*     block in the entry ZZHASH2. If this block is changed in any way, */
/*     the block in ZZHASH2 must be changed in the same way. */

    if (first) {
	first = FALSE_;
	base = 68;
	blank = ' ';
	maxdiv = intmax_() / base - 1;
	for (i__ = 0; i__ <= 128; ++i__) {
	    val[(i__1 = i__) < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		    "zzphsh_", (ftnlen)360)] = 0;
	}
	val[(i__1 = '0') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)363)] = 1;
	val[(i__1 = '1') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)364)] = 2;
	val[(i__1 = '2') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)365)] = 3;
	val[(i__1 = '3') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)366)] = 4;
	val[(i__1 = '4') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)367)] = 5;
	val[(i__1 = '5') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)368)] = 6;
	val[(i__1 = '6') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)369)] = 7;
	val[(i__1 = '7') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)370)] = 8;
	val[(i__1 = '8') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)371)] = 9;
	val[(i__1 = '9') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)372)] = 10;
	val[(i__1 = 'A') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)373)] = 11;
	val[(i__1 = 'B') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)374)] = 12;
	val[(i__1 = 'C') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)375)] = 13;
	val[(i__1 = 'D') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)376)] = 14;
	val[(i__1 = 'E') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)377)] = 15;
	val[(i__1 = 'F') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)378)] = 16;
	val[(i__1 = 'G') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)379)] = 17;
	val[(i__1 = 'H') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)380)] = 18;
	val[(i__1 = 'I') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)381)] = 19;
	val[(i__1 = 'J') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)382)] = 20;
	val[(i__1 = 'K') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)383)] = 21;
	val[(i__1 = 'L') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)384)] = 22;
	val[(i__1 = 'M') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)385)] = 23;
	val[(i__1 = 'N') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)386)] = 24;
	val[(i__1 = 'O') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)387)] = 25;
	val[(i__1 = 'P') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)388)] = 26;
	val[(i__1 = 'Q') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)389)] = 27;
	val[(i__1 = 'R') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)390)] = 28;
	val[(i__1 = 'S') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)391)] = 29;
	val[(i__1 = 'T') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)392)] = 30;
	val[(i__1 = 'U') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)393)] = 31;
	val[(i__1 = 'V') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)394)] = 32;
	val[(i__1 = 'W') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)395)] = 33;
	val[(i__1 = 'X') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)396)] = 34;
	val[(i__1 = 'Y') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)397)] = 35;
	val[(i__1 = 'Z') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)398)] = 36;
	val[(i__1 = '-') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)399)] = 37;
	val[(i__1 = '_') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)400)] = 38;
	val[(i__1 = '.') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)401)] = 39;
	val[(i__1 = '/') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)402)] = 40;
	val[(i__1 = '!') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)403)] = 41;
	val[(i__1 = '@') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)404)] = 42;
	val[(i__1 = '#') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)405)] = 43;
	val[(i__1 = '$') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)406)] = 44;
	val[(i__1 = '%') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)407)] = 45;
	val[(i__1 = '^') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)408)] = 46;
	val[(i__1 = '&') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)409)] = 47;
	val[(i__1 = '*') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)410)] = 48;
	val[(i__1 = '(') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)411)] = 49;
	val[(i__1 = ')') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)412)] = 50;
	val[(i__1 = '+') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)413)] = 51;
	val[(i__1 = '=') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)414)] = 52;
	val[(i__1 = '[') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)415)] = 53;
	val[(i__1 = '{') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)416)] = 54;
	val[(i__1 = ']') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)417)] = 55;
	val[(i__1 = '}') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)418)] = 56;
	val[(i__1 = '|') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)419)] = 57;
	val[92] = 58;
	val[(i__1 = ':') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)421)] = 59;
	val[(i__1 = ';') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)422)] = 60;
	val[(i__1 = '<') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)423)] = 61;
	val[(i__1 = ',') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)424)] = 62;
	val[(i__1 = '>') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)425)] = 63;
	val[(i__1 = '?') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)426)] = 64;

/*        Note, ICHAR('''') returns the ASCII value for the single */
/*        quote -> '. */

	val[(i__1 = '\'') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)432)] = 65;
	val[(i__1 = '"') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)433)] = 66;
	val[(i__1 = '`') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)434)] = 67;
	val[(i__1 = '~') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)435)] = 68;
	val[(i__1 = 'a') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)437)] = val[(i__2 = 'A') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)437)];
	val[(i__1 = 'b') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)438)] = val[(i__2 = 'B') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)438)];
	val[(i__1 = 'c') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)439)] = val[(i__2 = 'C') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)439)];
	val[(i__1 = 'd') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)440)] = val[(i__2 = 'D') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)440)];
	val[(i__1 = 'e') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)441)] = val[(i__2 = 'E') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)441)];
	val[(i__1 = 'f') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)442)] = val[(i__2 = 'F') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)442)];
	val[(i__1 = 'g') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)443)] = val[(i__2 = 'G') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)443)];
	val[(i__1 = 'h') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)444)] = val[(i__2 = 'H') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)444)];
	val[(i__1 = 'i') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)445)] = val[(i__2 = 'I') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)445)];
	val[(i__1 = 'j') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)446)] = val[(i__2 = 'J') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)446)];
	val[(i__1 = 'k') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)447)] = val[(i__2 = 'K') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)447)];
	val[(i__1 = 'l') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)448)] = val[(i__2 = 'L') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)448)];
	val[(i__1 = 'm') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)449)] = val[(i__2 = 'M') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)449)];
	val[(i__1 = 'n') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)450)] = val[(i__2 = 'N') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)450)];
	val[(i__1 = 'o') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)451)] = val[(i__2 = 'O') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)451)];
	val[(i__1 = 'p') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)452)] = val[(i__2 = 'P') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)452)];
	val[(i__1 = 'q') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)453)] = val[(i__2 = 'Q') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)453)];
	val[(i__1 = 'r') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)454)] = val[(i__2 = 'R') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)454)];
	val[(i__1 = 's') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)455)] = val[(i__2 = 'S') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)455)];
	val[(i__1 = 't') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)456)] = val[(i__2 = 'T') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)456)];
	val[(i__1 = 'u') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)457)] = val[(i__2 = 'U') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)457)];
	val[(i__1 = 'v') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)458)] = val[(i__2 = 'V') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)458)];
	val[(i__1 = 'w') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)459)] = val[(i__2 = 'W') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)459)];
	val[(i__1 = 'x') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)460)] = val[(i__2 = 'X') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)460)];
	val[(i__1 = 'y') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)461)] = val[(i__2 = 'Y') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)461)];
	val[(i__1 = 'z') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)462)] = val[(i__2 = 'Z') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)462)];
    }

/*     Check and save divisor. */

    if (*m <= 0 || *m > maxdiv) {
	chkin_("ZZSHSH", (ftnlen)6);
	setmsg_("The input hash function divisor was not in the allowed rang"
		"e from 1 to #. It was #.", (ftnlen)83);
	errint_("#", &maxdiv, (ftnlen)1);
	errint_("#", m, (ftnlen)1);
	sigerr_("SPICE(INVALIDDIVISOR)", (ftnlen)21);
	chkout_("ZZSHSH", (ftnlen)6);
	return ret_val;
    }
    divisr = *m;
    return ret_val;
/* $Procedure ZZHASH ( Private---POOL Hash function ) */

L_zzhash:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     This routine computes the hash value associated with a kernel */
/*     POOL variable name. */

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

/*     PRIVATE UTILITY */

/* $ Declarations */

/*     CHARACTER*(*)         WORD */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     WORD       I   A left justified string of characters. */

/*     The function returns the hash value associated with WORD. */

/* $ Detailed_Input */

/*     WORD        is a left justified string of characters. Nominally */
/*                 this is the name of some kernel POOL variable. */

/* $ Detailed_Output */

/*     The function returns the hash value of WORD. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If this routine is called prior to a call to ZZSPSH, the */
/*        error 'SPICE(CALLEDOUTOFORDER)' will be signaled. This should */
/*        never occur. In this case the function returns 0. */

/*     2) If this routine calculates a negative value, the error */
/*        SPICE(NEGATIVEHASHVALUE1) or SPICE(NEGATIVEHASHVALUE2) will be */
/*        signaled. This should never occur. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine computes the hash value of a string of characters. */
/*     The algorithm implemented by this function is case-insensitive and */
/*     uses only the first word of the input string. In order to make */
/*     effective use of this hash algorithm, input strings should be */
/*     left-justified and space-less. All non-left justified strings map */
/*     to the same value 0. */

/* $ Examples */

/*     See POOL. */

/* $ Restrictions */

/*     ZZSPSH must be called prior to calling this routine. */

/* $ Literature_References */

/*     1)  Knuth, Donald E. "The Art of Computer Programming, Volume */
/*         3/Sorting and Searching 2nd Edition" 1997, pp 513-521. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */
/*     E.D. Wright     (JPL) */
/*     B.V. Semenov    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.1, 31-JUL-2013 (BVS) */

/*        Added more details to the header. Add an exception for */
/*        un-initialized divisor. Added one more exception for */
/*        a negative output value. */

/* -    SPICELIB Version 1.1.0, 06-JUL-2005 (EDW) */

/*        Added error test to catch non-positive hash values. */

/* -    SPICELIB Version 1.0.0, 20-SEP-1995 (WLT) */

/* -& */

/*     Check if divisor was initialized by a prior call to ZZSHSH. */

    if (divisr == -1) {
	ret_val = 0;
	chkin_("ZZHASH", (ftnlen)6);
	setmsg_("The ZZHASH function was called before the POOL hash paramet"
		"ers were initialized by a call to ZZSHSH.", (ftnlen)100);
	sigerr_("SPICE(CALLEDOUTOFORDER)", (ftnlen)23);
	chkout_("ZZHASH", (ftnlen)6);
	return ret_val;
    }

/*     Compute hash value for the input string. */

    f = 0;
    length = i_len(word, word_len);
    i__1 = length;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (*(unsigned char *)&word[i__ - 1] == blank) {
	    ret_val = f * base % divisr + 1;

/*           A negative value for ZZHASH indicates a serious problem. */

	    if (ret_val < 0) {
		chkin_("ZZHASH", (ftnlen)6);
		setmsg_("The ZZHASH function calculated a negative value for"
			" string $1. Contact NAIF.", (ftnlen)76);
		errch_("$1", word, (ftnlen)2, word_len);
		sigerr_("SPICE(NEGATIVEHASHVALUE1)", (ftnlen)25);
		chkout_("ZZHASH", (ftnlen)6);
	    }
	    return ret_val;
	}
/* Computing MIN */
	i__3 = 128, i__4 = *(unsigned char *)&word[i__ - 1];
	f = val[(i__2 = min(i__3,i__4)) < 129 && 0 <= i__2 ? i__2 : s_rnge(
		"val", i__2, "zzphsh_", (ftnlen)664)] + f * base;
	f %= divisr;
    }
    ret_val = f * base % divisr + 1;

/*     A negative value for ZZHASH indicates a serious problem. */

    if (ret_val < 0) {
	chkin_("ZZHASH", (ftnlen)6);
	setmsg_("The ZZHASH function calculated a negative value for string "
		"$1. Contact NAIF.", (ftnlen)76);
	errch_("$1", word, (ftnlen)2, word_len);
	sigerr_("SPICE(NEGATIVEHASHVALUE2)", (ftnlen)25);
	chkout_("ZZHASH", (ftnlen)6);
    }
    return ret_val;
/* $Procedure ZZHASH2 ( Private---Arbitrary divisor hash function ) */

L_zzhash2:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     This routine computes the hash value corresponding to an string */
/*     given a particular divisor value (M2). */

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

/*     PRIVATE UTILITY */

/* $ Declarations */

/*     CHARACTER*(*)         WORD */
/*     INTEGER               M2 */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     WORD       I   A left justified string of characters. */
/*     M2         I   Divisor used for the hash function */

/*     The function returns the hash value associated with WORD. */

/* $ Detailed_Input */

/*     WORD        is a left justified string of characters. */

/*     M2          the divisor of the hashing function. This value */
/*                 defines the spread of the hash values, that spread */
/*                 covering the interval [1, M2]. The larger the M2 */
/*                 value, the less the chance of a hash key collision. */
/*                 The user should always chose a prime for M2. The */
/*                 value of M2 must be in the range from 1 to INTMAX/68 */
/*                 - 1. If it is not, the function signals an error and */
/*                 returns 0. */

/* $ Detailed_Output */

/*     The function returns the hash value of WORD as computed using M2 */
/*     as the hash function divisor. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If the input divisor value is not in the allowed range, the */
/*        error 'SPICE(INVALIDDIVISOR)' will be signaled. */

/*     2) If this routine calculates a negative value, the error */
/*        SPICE(NEGATIVEHASHVALUE1) or SPICE(NEGATIVEHASHVALUE2) will be */
/*        signaled. This should never occur. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine computes the hash value of a string of characters */
/*     using the user specified hash divisor value. The algorithm */
/*     implemented by this function is case-insensitive and uses only */
/*     the first word of the input string. In order to make effective */
/*     use of this hash algorithm, input strings should be */
/*     left-justified and space-less. All non-left justified strings map */
/*     to the same value 0. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     1)  Knuth, Donald E. "The Art of Computer Programming, Volume */
/*         3/Sorting and Searching 2nd Edition" 1997, pp 513-521. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */
/*     E.D. Wright     (JPL) */
/*     B.V. Semenov    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 31-JUL-2013 (BVS) */

/*        Added more details to the header. Added computation of the */
/*        maximum allowed divisor value to the initialization block. */
/*        Added a check for the input divisor to be in the allowed */
/*        range. Added one more exception for a negative output value. */

/* -    SPICELIB Version 1.0.0, 14-SEP-2005 (EDW) */

/* -& */

/*     The initialization block below is identical to the initialization */
/*     block in the entry ZZSHSH. If this block is changed in any way, */
/*     the block in ZZSHSH must be changed in the same way. */

    if (first) {
	first = FALSE_;
	base = 68;
	blank = ' ';
	maxdiv = intmax_() / base - 1;
	for (i__ = 0; i__ <= 128; ++i__) {
	    val[(i__1 = i__) < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		    "zzphsh_", (ftnlen)841)] = 0;
	}
	val[(i__1 = '0') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)844)] = 1;
	val[(i__1 = '1') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)845)] = 2;
	val[(i__1 = '2') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)846)] = 3;
	val[(i__1 = '3') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)847)] = 4;
	val[(i__1 = '4') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)848)] = 5;
	val[(i__1 = '5') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)849)] = 6;
	val[(i__1 = '6') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)850)] = 7;
	val[(i__1 = '7') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)851)] = 8;
	val[(i__1 = '8') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)852)] = 9;
	val[(i__1 = '9') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)853)] = 10;
	val[(i__1 = 'A') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)854)] = 11;
	val[(i__1 = 'B') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)855)] = 12;
	val[(i__1 = 'C') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)856)] = 13;
	val[(i__1 = 'D') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)857)] = 14;
	val[(i__1 = 'E') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)858)] = 15;
	val[(i__1 = 'F') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)859)] = 16;
	val[(i__1 = 'G') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)860)] = 17;
	val[(i__1 = 'H') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)861)] = 18;
	val[(i__1 = 'I') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)862)] = 19;
	val[(i__1 = 'J') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)863)] = 20;
	val[(i__1 = 'K') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)864)] = 21;
	val[(i__1 = 'L') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)865)] = 22;
	val[(i__1 = 'M') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)866)] = 23;
	val[(i__1 = 'N') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)867)] = 24;
	val[(i__1 = 'O') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)868)] = 25;
	val[(i__1 = 'P') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)869)] = 26;
	val[(i__1 = 'Q') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)870)] = 27;
	val[(i__1 = 'R') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)871)] = 28;
	val[(i__1 = 'S') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)872)] = 29;
	val[(i__1 = 'T') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)873)] = 30;
	val[(i__1 = 'U') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)874)] = 31;
	val[(i__1 = 'V') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)875)] = 32;
	val[(i__1 = 'W') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)876)] = 33;
	val[(i__1 = 'X') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)877)] = 34;
	val[(i__1 = 'Y') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)878)] = 35;
	val[(i__1 = 'Z') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)879)] = 36;
	val[(i__1 = '-') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)880)] = 37;
	val[(i__1 = '_') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)881)] = 38;
	val[(i__1 = '.') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)882)] = 39;
	val[(i__1 = '/') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)883)] = 40;
	val[(i__1 = '!') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)884)] = 41;
	val[(i__1 = '@') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)885)] = 42;
	val[(i__1 = '#') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)886)] = 43;
	val[(i__1 = '$') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)887)] = 44;
	val[(i__1 = '%') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)888)] = 45;
	val[(i__1 = '^') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)889)] = 46;
	val[(i__1 = '&') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)890)] = 47;
	val[(i__1 = '*') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)891)] = 48;
	val[(i__1 = '(') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)892)] = 49;
	val[(i__1 = ')') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)893)] = 50;
	val[(i__1 = '+') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)894)] = 51;
	val[(i__1 = '=') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)895)] = 52;
	val[(i__1 = '[') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)896)] = 53;
	val[(i__1 = '{') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)897)] = 54;
	val[(i__1 = ']') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)898)] = 55;
	val[(i__1 = '}') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)899)] = 56;
	val[(i__1 = '|') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)900)] = 57;
	val[92] = 58;
	val[(i__1 = ':') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)902)] = 59;
	val[(i__1 = ';') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)903)] = 60;
	val[(i__1 = '<') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)904)] = 61;
	val[(i__1 = ',') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)905)] = 62;
	val[(i__1 = '>') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)906)] = 63;
	val[(i__1 = '?') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)907)] = 64;

/*        Note, ICHAR('''') returns the ASCII value for the single */
/*        quote -> '. */

	val[(i__1 = '\'') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)913)] = 65;
	val[(i__1 = '"') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)914)] = 66;
	val[(i__1 = '`') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)915)] = 67;
	val[(i__1 = '~') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)916)] = 68;
	val[(i__1 = 'a') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)918)] = val[(i__2 = 'A') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)918)];
	val[(i__1 = 'b') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)919)] = val[(i__2 = 'B') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)919)];
	val[(i__1 = 'c') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)920)] = val[(i__2 = 'C') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)920)];
	val[(i__1 = 'd') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)921)] = val[(i__2 = 'D') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)921)];
	val[(i__1 = 'e') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)922)] = val[(i__2 = 'E') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)922)];
	val[(i__1 = 'f') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)923)] = val[(i__2 = 'F') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)923)];
	val[(i__1 = 'g') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)924)] = val[(i__2 = 'G') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)924)];
	val[(i__1 = 'h') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)925)] = val[(i__2 = 'H') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)925)];
	val[(i__1 = 'i') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)926)] = val[(i__2 = 'I') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)926)];
	val[(i__1 = 'j') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)927)] = val[(i__2 = 'J') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)927)];
	val[(i__1 = 'k') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)928)] = val[(i__2 = 'K') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)928)];
	val[(i__1 = 'l') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)929)] = val[(i__2 = 'L') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)929)];
	val[(i__1 = 'm') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)930)] = val[(i__2 = 'M') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)930)];
	val[(i__1 = 'n') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)931)] = val[(i__2 = 'N') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)931)];
	val[(i__1 = 'o') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)932)] = val[(i__2 = 'O') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)932)];
	val[(i__1 = 'p') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)933)] = val[(i__2 = 'P') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)933)];
	val[(i__1 = 'q') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)934)] = val[(i__2 = 'Q') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)934)];
	val[(i__1 = 'r') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)935)] = val[(i__2 = 'R') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)935)];
	val[(i__1 = 's') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)936)] = val[(i__2 = 'S') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)936)];
	val[(i__1 = 't') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)937)] = val[(i__2 = 'T') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)937)];
	val[(i__1 = 'u') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)938)] = val[(i__2 = 'U') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)938)];
	val[(i__1 = 'v') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)939)] = val[(i__2 = 'V') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)939)];
	val[(i__1 = 'w') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)940)] = val[(i__2 = 'W') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)940)];
	val[(i__1 = 'x') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)941)] = val[(i__2 = 'X') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)941)];
	val[(i__1 = 'y') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)942)] = val[(i__2 = 'Y') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)942)];
	val[(i__1 = 'z') < 129 && 0 <= i__1 ? i__1 : s_rnge("val", i__1, 
		"zzphsh_", (ftnlen)943)] = val[(i__2 = 'Z') < 129 && 0 <= 
		i__2 ? i__2 : s_rnge("val", i__2, "zzphsh_", (ftnlen)943)];
    }

/*     Check divisor. */

    if (*m2 <= 0 || *m2 > maxdiv) {
	ret_val = 0;
	chkin_("ZZHASH2", (ftnlen)7);
	setmsg_("The input hash function divisor was not in the allowed rang"
		"e from 1 to #. It was #.", (ftnlen)83);
	errint_("#", &maxdiv, (ftnlen)1);
	errint_("#", m2, (ftnlen)1);
	sigerr_("SPICE(INVALIDDIVISOR)", (ftnlen)21);
	chkout_("ZZHASH2", (ftnlen)7);
	return ret_val;
    }

/*     Compute hash value for the input string. */

    f = 0;
    length = i_len(word, word_len);
    i__1 = length;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (*(unsigned char *)&word[i__ - 1] == blank) {
	    ret_val = f * base % *m2 + 1;

/*           A negative value for ZZHASH2 indicates a serious problem. */

	    if (ret_val < 0) {
		chkin_("ZZHASH2", (ftnlen)7);
		setmsg_("The ZZHASH2 function calculated a negative value fo"
			"r string $1. Contact NAIF.", (ftnlen)77);
		errch_("$1", word, (ftnlen)2, word_len);
		sigerr_("SPICE(NEGATIVEHASHVALUE1)", (ftnlen)25);
		chkout_("ZZHASH2", (ftnlen)7);
	    }
	    return ret_val;
	}
/* Computing MIN */
	i__3 = 128, i__4 = *(unsigned char *)&word[i__ - 1];
	f = val[(i__2 = min(i__3,i__4)) < 129 && 0 <= i__2 ? i__2 : s_rnge(
		"val", i__2, "zzphsh_", (ftnlen)996)] + f * base;
	f %= *m2;
    }
    ret_val = f * base % *m2 + 1;

/*     A negative value for ZZHASH2 indicates a serious problem. */

    if (ret_val < 0) {
	chkin_("ZZHASH2", (ftnlen)7);
	setmsg_("The ZZHASH2 function calculated a negative value for string"
		" $1. Contact NAIF.", (ftnlen)77);
	errch_("$1", word, (ftnlen)2, word_len);
	sigerr_("SPICE(NEGATIVEHASHVALUE2)", (ftnlen)25);
	chkout_("ZZHASH2", (ftnlen)7);
    }
    return ret_val;
} /* zzphsh_ */

integer zzphsh_(char *word, integer *m, integer *m2, ftnlen word_len)
{
    return zzphsh_0_(0, word, m, m2, word_len);
    }

integer zzshsh_(integer *m)
{
    return zzphsh_0_(1, (char *)0, m, (integer *)0, (ftnint)0);
    }

integer zzhash_(char *word, ftnlen word_len)
{
    return zzphsh_0_(2, word, (integer *)0, (integer *)0, word_len);
    }

integer zzhash2_(char *word, integer *m2, ftnlen word_len)
{
    return zzphsh_0_(3, word, (integer *)0, m2, word_len);
    }

