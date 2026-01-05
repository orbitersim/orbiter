/* ana.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__33 = 33;
static integer c__22 = 22;

/* $Procedure ANA ( AN or A ? ) */
/* Character */ VOID ana_(char *ret_val, ftnlen ret_val_len, char *word, char 
	*case__, ftnlen word_len, ftnlen case_len)
{
    /* Initialized data */

    static char a[2*3] = "A " "A " "a ";
    static char an[2*3] = "AN" "An" "an";
    static char anword[8*22] = "HEIR    " "HONEST  " "HONOR   " "H       " 
	    "HOUR    " "HORS    " "HOMBRE  " "F       " "L       " "M       " 
	    "N       " "R       " "S       " "X       " "UNIN    " "UNIM    " 
	    "ONEI    " "ONER    " "SPK     " "EK      " "IK      " "SCLK    ";
    static char aword[8*33] = "HORSE   " "ONE     " "ONE-    " "ONCE    " 
	    "ONENESS " "UIG     " "UIN     " "UKA     " "UKE     " "UKO     " 
	    "UKI     " "UKU     " "ULOT    " "UNANI   " "UNI     " "UNINU   " 
	    "UPA     " "URA     " "URE     " "URO     " "USA     " "USE     " 
	    "USU     " "UTE     " "UTI     " "UTO     " "UVA     " "UVE     " 
	    "UVU     " "EU      " "EWE     " "UTRI    " "U       ";

    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer i_indx(char *, char *, ftnlen, ftnlen), s_rnge(char *, integer, 
	    char *, integer);

    /* Local variables */
    static integer caps, i__;
    static char begin[1];
    extern /* Subroutine */ int ucase_(char *, char *, ftnlen, ftnlen);
    static char start[32*7];
    extern /* Subroutine */ int ljust_(char *, char *, ftnlen, ftnlen);
    extern integer isrchc_(char *, integer *, char *, ftnlen, ftnlen);
    extern /* Subroutine */ int replch_(char *, char *, char *, char *, 
	    ftnlen, ftnlen, ftnlen, ftnlen);
    static char mycase[1], myword[32];

/* $ Abstract */

/*     Return the correct article "a" or "an" used to modify a word */
/*     and return it capitalized, lower case, or upper case. */

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

/*     UTILITY */
/*     WORD */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     WORD       I   is a word that should be modified by "a" or "an". */
/*     CASE       I   'U', 'L', or 'C' to specify capitalization of ANA. */

/*     The function returns the correct article, 'A' or 'AN', needed to */
/*     modify a word WORD, appropriately capitalized. */

/* $ Detailed_Input */

/*     WORD     is any English word for which you want to write the */
/*              correct phrase "a(an) response(answer)".  The case of the */
/*              letters of word do not matter. */

/*              Leading white space in word is ignored. The characters */
/*              " and ' are ignored.  Thus ''' apple '' ' and */
/*              '"apple"' and ' apple' and 'apple' are all treated as */
/*              the same word. */

/*     CASE     is a character that describes how the value returned in */
/*              ANA should be capitalized. The rules are: */

/*                 'U'  ---  ANA is returned in all caps ( A, AN ) */
/*                 'C'  ---  ANA is returned capitalized ( A, An ) */
/*                 'L'  ---  ANA is returned lower case  ( a, an ) */

/*              The case of CASE does not matter. Any value other than */
/*              those specified result in ANA being returned in all lower */
/*              case. */

/* $ Detailed_Output */

/*     The function returns the correct indefinite article needed to */
/*     modify the word contained in WORD. */

/*     ANA should be declared to be */

/*        CHARACTER*(2) */

/*     (or CHARACTER*(N) where N > 1) in the calling program. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  If the uppercase value of CASE is not 'U', 'C' or 'L', it */
/*         shall be treated as 'L'. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine allows you to construct grammatically correct phrases */
/*     when you need to modify a word by an indefinite article. Using */
/*     the pronunciations contained in the Webster's Ninth Collegiate */
/*     Dictionary, the phrase */

/*        ANA(WORD, CASE) // ' ' // WORD */

/*     will be grammatically correct. */

/* $ Examples */

/*     Suppose you wished to construct one of the messages */

/*        'a new file' */
/*        'an existing file' */

/*     and that the NEW/EXISTING word was in the variable WORD. Then */
/*     you could write */

/*        MESSAGE = ANA( WORD, 'L' ) // ' ' // WORD // ' file ' */
/*        CALL CMPRSS ( ' ', 1, MESSAGE, MESSAGE ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     [1]  Merriam-Webster (Ed.), "Webster's Ninth New Collegiate */
/*          Dictionary," 10th edition, 1990. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.3, 24-NOV-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.1.2, 28-FEB-2008 (BVS) */

/*        Corrected the contents of the $Required_Reading section. */

/* -    SPICELIB Version 1.1.1, 22-SEP-2004 (EDW) */

/*        Added Copyright section. */

/* -    SPICELIB Version 1.1.0, 18-JAN-2001 (WLT) */

/*        Made SCLK an "an" word. */

/* -    SPICELIB Version 1.0.0, 29-NOV-1995 (WLT) */

/* -& */
/* $ Index_Entries */

/*     get the correct indefinite article */

/* -& */
    ucase_(word, myword, word_len, (ftnlen)32);
    replch_(myword, "'", " ", myword, (ftnlen)32, (ftnlen)1, (ftnlen)1, (
	    ftnlen)32);
    replch_(myword, "\"", " ", myword, (ftnlen)32, (ftnlen)1, (ftnlen)1, (
	    ftnlen)32);
    ljust_(myword, myword, (ftnlen)32, (ftnlen)32);
    ucase_(case__, mycase, case_len, (ftnlen)1);
    s_copy(ret_val, " ", ret_val_len, (ftnlen)1);
    if (*(unsigned char *)mycase == 'U') {
	caps = 1;
    } else if (*(unsigned char *)mycase == 'C') {
	caps = 2;
    } else {
	caps = 3;
    }

/*     Handle the obvious things first. */

    *(unsigned char *)begin = *(unsigned char *)myword;
    if (i_indx("AI", begin, (ftnlen)2, (ftnlen)1) > 0) {
	s_copy(ret_val, an + (((i__1 = caps - 1) < 3 && 0 <= i__1 ? i__1 : 
		s_rnge("an", i__1, "ana_", (ftnlen)251)) << 1), ret_val_len, (
		ftnlen)2);
	return ;
    } else if (i_indx("BCDGJKPQTVWYZ", begin, (ftnlen)13, (ftnlen)1) > 0) {
	s_copy(ret_val, a + (((i__1 = caps - 1) < 3 && 0 <= i__1 ? i__1 : 
		s_rnge("a", i__1, "ana_", (ftnlen)256)) << 1), ret_val_len, (
		ftnlen)2);
	return ;
    }

/*     If we are still here, we need to be a bit more careful */
/*     in our determination of ANA. */

/*     Get the beginnings of the input word. */

    for (i__ = 1; i__ <= 7; ++i__) {
	s_copy(start + (((i__1 = i__ - 1) < 7 && 0 <= i__1 ? i__1 : s_rnge(
		"start", i__1, "ana_", (ftnlen)268)) << 5), myword, (ftnlen)
		32, i__);
    }

/*     Now see if the start of the input word belongs to */
/*     one of the special collections. */

    for (i__ = 7; i__ >= 2; --i__) {
	if (isrchc_(start + (((i__1 = i__ - 1) < 7 && 0 <= i__1 ? i__1 : 
		s_rnge("start", i__1, "ana_", (ftnlen)277)) << 5), &c__33, 
		aword, (ftnlen)32, (ftnlen)8) != 0) {
	    s_copy(ret_val, a + (((i__1 = caps - 1) < 3 && 0 <= i__1 ? i__1 : 
		    s_rnge("a", i__1, "ana_", (ftnlen)279)) << 1), 
		    ret_val_len, (ftnlen)2);
	    return ;
	}
	if (isrchc_(start + (((i__1 = i__ - 1) < 7 && 0 <= i__1 ? i__1 : 
		s_rnge("start", i__1, "ana_", (ftnlen)284)) << 5), &c__22, 
		anword, (ftnlen)32, (ftnlen)8) != 0) {
	    s_copy(ret_val, an + (((i__1 = caps - 1) < 3 && 0 <= i__1 ? i__1 :
		     s_rnge("an", i__1, "ana_", (ftnlen)286)) << 1), 
		    ret_val_len, (ftnlen)2);
	    return ;
	}
    }

/*     If we got this far we can determine the ANA by */
/*     just looking at the beginning of the string. */

    if (i_indx("AEIOU", myword, (ftnlen)5, (ftnlen)1) > 0) {
	s_copy(ret_val, an + (((i__1 = caps - 1) < 3 && 0 <= i__1 ? i__1 : 
		s_rnge("an", i__1, "ana_", (ftnlen)299)) << 1), ret_val_len, (
		ftnlen)2);
    } else {
	s_copy(ret_val, a + (((i__1 = caps - 1) < 3 && 0 <= i__1 ? i__1 : 
		s_rnge("a", i__1, "ana_", (ftnlen)303)) << 1), ret_val_len, (
		ftnlen)2);
    }
    return ;
} /* ana_ */

