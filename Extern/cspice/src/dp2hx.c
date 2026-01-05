/* dp2hx.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;

/* $Procedure DP2HX ( D.p. number to hexadecimal string ) */
/* Subroutine */ int dp2hx_(doublereal *number, char *hxstr, integer *hxssiz, 
	ftnlen hxstr_len)
{
    /* Initialized data */

    static char digits[1*16] = "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" 
	    "B" "C" "D" "E" "F";

    /* System generated locals */
    address a__1[2];
    integer i__1, i__2[2];

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen),
	     s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int int2hx_(integer *, char *, integer *, ftnlen);
    doublereal remndr;
    integer explen;
    logical negtiv;
    integer intexp, positn, result;
    doublereal tmpnum;
    logical postiv;
    char expstr[255], tmpstr[255];

/* $ Abstract */

/*     Convert a double precision number to an equivalent character */
/*     string using a base 16 "scientific notation." */

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

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     STRLEN     P   Max number of characters allowed in output string. */
/*     NUMBER     I   D.p. number to be converted. */
/*     HXSTR      O   Equivalent character string, left justified. */
/*     HXSSIZ     O   Length of the character string produced. */

/* $ Detailed_Input */

/*     NUMBER   is the double precision number to be converted to a */
/*              character string representation. */

/* $ Detailed_Output */

/*     HXSTR    is the character string produced by this routine that */
/*              represents NUMBER in a base 16 "scientific notation," */
/*              e.g.: */

/*                 672.0 = '2A^3' = ( 2/16 + 10/( 16**2 ) ) * 16**3 */

/*              and */

/*                 -11.0 = '-B^1' = - ( 11/16 ) * 16**1. */

/*              The following table describes the character set used to */
/*              represent the hexadecimal digits and their corresponding */
/*              values. */

/*                 Character    Value         Character    Value */
/*                 ---------    ------        ---------    ------ */
/*                   '0'         0.0D0          '8'         8.0D0 */
/*                   '1'         1.0D0          '9'         9.0D0 */
/*                   '2'         2.0D0          'A'        10.0D0 */
/*                   '3'         3.0D0          'B'        11.0D0 */
/*                   '4'         4.0D0          'C'        12.0D0 */
/*                   '5'         5.0D0          'D'        13.0D0 */
/*                   '6'         6.0D0          'E'        14.0D0 */
/*                   '7'         7.0D0          'F'        15.0D0 */

/*              The caret, or hat, character, '^', is used to */
/*              distinguish the exponent. */

/*              The plus sign, '+', and the minus sign, '-', are used, */
/*              and they have their usual meanings. */

/*              In order to obtain the entire character string produced */
/*              by this routine, the output character string should be */
/*              at least N characters long, where */


/*                          # of bits per double precision mantissa + 3 */
/*                 N = 3 + --------------------------------------------- */
/*                                               4 */

/*                          # of bits per double precision exponent + 3 */
/*                       + --------------------------------------------- */
/*                                               4 */

/*              There should be one character position for the sign of */
/*              the mantissa, one for the sign of the exponent, one for */
/*              the exponentiation character, and one for each */
/*              hexadecimal digit that could be produced from a mantissa */
/*              and an exponent. */

/*              The following table contains minimum output string */
/*              lengths necessary to obtain the complete character */
/*              string produced by this routine for some typical */
/*              implementations of double precision numbers. */

/*                 Double precision number */
/*                 Size Mantissa Exponent   Minimum output string */
/*                 bits   bits     bits     length */
/*                 ---- -------- --------   ---------------------- */
/*                 64   48       15         3 + 12 + 4 = 19 */
/*                 64   55+1     8          3 + 14 + 2 = 19 (VAX) */
/*                 64   52       11         3 + 13 + 3 = 19 (IEEE) */

/*              The base 16 "scientific notation" character string */
/*              produced by this routine will be left justified and */
/*              consist of a contiguous sequence of characters with one */
/*              of the following formats: */

/*                 (1)   h h h h  ... h ^H H  ... H */
/*                        1 2 3 4      n  1 2      m */

/*                 (2)   -h h h h  ... h ^H H  ... H */
/*                         1 2 3 4      n  1 2      m */

/*                 (3)   h h h h  ... h ^-H H  ... H */
/*                        1 2 3 4      n   1 2      m */

/*                 (4)   -h h h h  ... h ^-H H  ... H */
/*                         1 2 3 4      n   1 2      m */

/*              where */

/*                 h   and  H   denote hexadecimal digits */
/*                  i        j */

/*                 '^'          denotes exponentiation ( base 16 ) */

/*              and */

/*                 '+' and '-'  have their usual interpretations. */

/*              The character string produced will be blank padded on */
/*              the right if HXSSIZ < LEN( HXSTR ). */

/*     HXSSIZ   is the length of the base 16 "scientific notation" */
/*              character string produced by this routine. */

/* $ Parameters */

/*     STRLEN   is the maximum number of characters permitted in the */
/*              output string. The value of STRLEN is 255. */

/* $ Exceptions */

/*     Error free. */

/*     1)  If the output character string is not long enough to */
/*         contain the entire character string that was produced, */
/*         the string will be truncated on the right. */

/*     2)  If LEN( HXSTR ) > HXSSIZ, the output character string will */
/*         be blank padded on the right. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine converts a double precision number into an equivalent */
/*     character string using a base 16 "scientific notation." This */
/*     representation allows the full precision of a number to be placed */
/*     in a format that is suitable for porting or archival storage. */

/*     This routine is one of a pair of routines which are used to */
/*     perform conversions between double precision numbers and */
/*     an equivalent base 16 "scientific notation" character string */
/*     representation: */

/*        DP2HX  -- Convert a double precision number into a base 16 */
/*                  "scientific notation" character string. */

/*        HX2DP  -- Convert a base 16 "scientific notation" */
/*                  character string into a double precision number. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Convert a set of double precision numbers to their equivalent */
/*        character string using a base 16 "scientific notation." */


/*        Example code begins here. */


/*              PROGRAM DP2HX_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local constants. */
/*        C */
/*              INTEGER               HXSLEN */
/*              PARAMETER           ( HXSLEN = 40 ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              CHARACTER*(HXSLEN)    STRVAL */

/*              DOUBLE PRECISION      NUMBER (8) */

/*              INTEGER               I */
/*              INTEGER               SVALLN */

/*        C */
/*        C     Assign an array of double precision numbers. */
/*        C */
/*              DATA                  NUMBER /    2.0D-9,       1.0D0, */
/*             .                                  -1.0D0,    1024.0D0, */
/*             .                               -1024.0D0,  521707.0D0, */
/*             .                                  27.0D0,       0.0D0 / */

/*        C */
/*        C     Loop over the NUMBER array, call DP2HX for each */
/*        C     element of NUMBER. */
/*        C */
/*              WRITE(*,*) 'number       string             length' */
/*              WRITE(*,*) '-----------  -----------------  ------' */

/*              DO I= 1, 8 */

/*                 CALL DP2HX ( NUMBER(I), STRVAL, SVALLN ) */
/*                 WRITE(*,'(E12.4,2X,A17,2X,I5)') NUMBER(I), STRVAL, */
/*             .                                              SVALLN */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         number       string             length */
/*         -----------  -----------------  ------ */
/*          0.2000E-08  89705F4136B4A8^-7     17 */
/*          0.1000E+01  1^1                    3 */
/*         -0.1000E+01  -1^1                   4 */
/*          0.1024E+04  4^3                    3 */
/*         -0.1024E+04  -4^3                   4 */
/*          0.5217E+06  7F5EB^5                7 */
/*          0.2700E+02  1B^2                   4 */
/*          0.0000E+00  0^0                    3 */


/*        Note: the hat or caret, '^', signals an exponent. */

/* $ Restrictions */

/*     1)  The maximum number of characters permitted in the output */
/*         string is specified by the parameter STRLEN. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR) */

/*        Changed the argument names "STRING" and "LENGTH" to "HXSTR" */
/*        and "HXSSIZ" for consistency with other routines. */

/*        Added IMPLICIT NONE statement. */

/*        The declaration of STRLEN has been promoted to the */
/*        $Declarations section. */

/*        Edited the header to comply with NAIF standard. Removed */
/*        unnecessary $Revisions section. */

/*        Added complete example code. */

/*        Updated $Brief_I/O, $Parameters and $Restrictions sections to */
/*        properly describe STRLEN. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1994 (KRG) */

/*        Fixed a typo in the description of the input argument STRING. */
/*        The example showing the expansion of 160 into hexadecimal */
/*        was incorrect. 160 was replaced with 672 which makes the */
/*        example correct. */

/* -    SPICELIB Version 1.0.0, 26-OCT-1992 (KRG) */

/* -& */
/* $ Index_Entries */

/*     convert d.p. to signed normalized hexadecimal string */
/*     convert d.p. number to encoded d.p. number */
/*     convert d.p. to base 16 scientific notation */

/* -& */

/*     Local Parameters */


/*     Local variables */


/*     Saved variables */


/*     Initial values */


/*     Make a copy of the input so that it will not be changed by this */
/*     routine. Also, assume that we do not know the sign of the number. */

    tmpnum = *number;
    negtiv = FALSE_;
    postiv = FALSE_;

/*     Check to see what the sign of the number is, because we treat */
/*     negative numbers, positive numbers and zero separately. This */
/*     simplifies the testing in the loop boundaries a bit, and removes */
/*     calls to DABS() that would otherwise have been necessary. */

/*     Set the appropriate logical flag for the sign of the input number. */

    if (tmpnum < 0.) {
	negtiv = TRUE_;
    } else if (tmpnum > 0.) {
	postiv = TRUE_;
    }

/*     If nonzero, a double precision number is first normalized, */
/*     so that it has a value between 1.0D0/BASE and 1.0D0 or -1.0D0 */
/*     and -1/BASE. The hexadecimal digits in the mantissa  are found */
/*     by repeated applications of multiplication and truncation */
/*     operations. The hexadecimal digits will be in the correct order */
/*     when finished. The string will be left justified, and its length */
/*     will be set before returning. */

/*     Calculate the exponent of the number using multiple scaling */
/*     levels. The different scale factors, 16**8, 16**4, and 16, */
/*     provide a significant speed improvement for the normalization */
/*     process. */

    intexp = 0;
    if (negtiv) {
	if (tmpnum > -1.) {

/*           ABS(TMPNUM) .LT. 1.0 */

	    while(tmpnum * 4294967296. > -1.) {

/*              Scale the number and decrement the exponent. */

		tmpnum *= 4294967296.;
		intexp += -8;
	    }
	    while(tmpnum * 65536. > -1.) {

/*              Scale the number and decrement the exponent. */

		tmpnum *= 65536.;
		intexp += -4;
	    }
	    while(tmpnum * 16. > -1.) {

/*              Scale the number and decrement the exponent. */

		tmpnum *= 16.;
		--intexp;
	    }

/*           At this point, -1 < TMPNUM <= -1/BASE. */

	} else {

/*           ABS(TMPNUM) .GE. 1.0 */

	    while(tmpnum * 2.3283064365386963e-10 <= -1.) {

/*              Scale the number and increment the exponent. */

		tmpnum *= 2.3283064365386963e-10;
		intexp += 8;
	    }
	    while(tmpnum * 1.52587890625e-5 <= -1.) {

/*              Scale the number and increment the exponent. */

		tmpnum *= 1.52587890625e-5;
		intexp += 4;
	    }
	    while(tmpnum <= -1.) {

/*              Scale the number and increment the exponent. */

		tmpnum *= .0625;
		++intexp;
	    }

/*           At this point, -1 < TMPNUM <= -1/BASE. */

	}
    } else if (postiv) {
	if (tmpnum < 1.) {

/*           ABS(TMPNUM) .LT. 1.0 */

	    while(tmpnum * 4294967296. < 1.) {

/*              Scale the number and decrement the exponent. */

		tmpnum *= 4294967296.;
		intexp += -8;
	    }
	    while(tmpnum * 65536. < 1.) {

/*              Scale the number and decrement the exponent. */

		tmpnum *= 65536.;
		intexp += -4;
	    }
	    while(tmpnum * 16. < 1.) {

/*              Scale the number and decrement the exponent. */

		tmpnum *= 16.;
		--intexp;
	    }

/*           At this point, 1/BASE <= TMPNUM < 1 */

	} else {

/*           ABS(TMPNUM) .GE. 1.0 */

	    while(tmpnum * 2.3283064365386963e-10 >= 1.) {

/*              Scale the number and increment the exponent. */

		tmpnum *= 2.3283064365386963e-10;
		intexp += 8;
	    }
	    while(tmpnum * 1.52587890625e-5 >= 1.) {

/*              Scale the number and increment the exponent. */

		tmpnum *= 1.52587890625e-5;
		intexp += 4;
	    }
	    while(tmpnum >= 1.) {

/*              Scale the number and increment the exponent. */

		tmpnum *= .0625;
		++intexp;
	    }

/*           At this point, 1/BASE <= TMPNUM < 1 */

	}
    }

/*     We do different things for the cases where the number to be */
/*     converted is positive, negative, or zero. */

    if (negtiv) {

/*        Set the beginning position. */

	positn = 1;

/*        Put the minus sign in place. */

	*(unsigned char *)&tmpstr[positn - 1] = '-';

/*        Start with the remainder equal to the normalized value of the */
/*        original number. */

	remndr = tmpnum;

/*        Collect all of the digits in the string. */

/*        This stopping test works because the base is a power of */
/*        2 and the mantissa is composed of a sum of powers of 2. */

	while(remndr != 0.) {

/*           -1 < REMNDR <= -1/BASE */

	    ++positn;
	    tmpnum = remndr * 16.;
	    result = (integer) tmpnum;
	    remndr = tmpnum - (doublereal) result;
	    *(unsigned char *)&tmpstr[positn - 1] = *(unsigned char *)&digits[
		    (i__1 = -result) < 16 && 0 <= i__1 ? i__1 : s_rnge("digi"
		    "ts", i__1, "dp2hx_", (ftnlen)619)];
	}

/*        Put the exponent on the end of the number and update the */
/*        position. */

	int2hx_(&intexp, expstr, &explen, (ftnlen)255);
	i__1 = positn;
/* Writing concatenation */
	i__2[0] = 1, a__1[0] = "^";
	i__2[1] = explen, a__1[1] = expstr;
	s_cat(tmpstr + i__1, a__1, i__2, &c__2, 255 - i__1);
	positn = positn + explen + 1;
    } else if (postiv) {

/*        Set the beginning position. */

	positn = 0;

/*        Start with the remainder equal to the normalized value of the */
/*        original number. */

	remndr = tmpnum;

/*        Collect all of the digits in the string. */

/*        This stopping test works because the base is a power of */
/*        2 and the mantissa is composed of a sum of powers of 2. */

	while(remndr != 0.) {

/*           1/BASE <= REMNDR < 1 */

	    ++positn;
	    tmpnum = remndr * 16.;
	    result = (integer) tmpnum;
	    remndr = tmpnum - (doublereal) result;
	    *(unsigned char *)&tmpstr[positn - 1] = *(unsigned char *)&digits[
		    (i__1 = result) < 16 && 0 <= i__1 ? i__1 : s_rnge("digits"
		    , i__1, "dp2hx_", (ftnlen)654)];
	}

/*        Put the exponent on the end of the number and update the */
/*        position. */

	int2hx_(&intexp, expstr, &explen, (ftnlen)255);
	i__1 = positn;
/* Writing concatenation */
	i__2[0] = 1, a__1[0] = "^";
	i__2[1] = explen, a__1[1] = expstr;
	s_cat(tmpstr + i__1, a__1, i__2, &c__2, 255 - i__1);
	positn = positn + explen + 1;
    } else {

/*        Treat zero as a special case, because it's easier. */

	positn = 3;
	s_copy(tmpstr, "0^0", (ftnlen)3, (ftnlen)3);
    }

/*     Set the value for the length of the character string produced */
/*     before returning. */

    *hxssiz = positn;

/*     Set the value of the output string before returning. Let the */
/*     Fortran string assignment deal with the left justification, and */
/*     the truncation on the right if HXSTR is not long enough to */
/*     contain all of the characters produced. */

    s_copy(hxstr, tmpstr, hxstr_len, (*hxssiz));
    return 0;
} /* dp2hx_ */

