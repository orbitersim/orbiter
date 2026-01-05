/* hx2dp.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__31 = 31;

/* $Procedure HX2DP ( Hexadecimal string to d.p. number ) */
/* Subroutine */ int hx2dp_(char *string, doublereal *number, logical *error, 
	char *errmsg, ftnlen string_len, ftnlen errmsg_len)
{
    /* Initialized data */

    static doublereal dpval[16] = { 0.,1.,2.,3.,4.,5.,6.,7.,8.,9.,10.,11.,12.,
	    13.,14.,15. };
    static logical first = TRUE_;

    /* System generated locals */
    integer i__1, i__2, i__3;
    char ch__1[1];

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer), s_cmp(char *, char *, 
	    ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer i_len(char *, ftnlen);

    /* Local variables */
    integer ival[32];
    logical more;
    integer i__;
    extern doublereal dpmin_(void);
    static doublereal mindp;
    extern doublereal dpmax_(void);
    static doublereal maxdp;
    extern /* Subroutine */ int repmc_(char *, char *, char *, char *, ftnlen,
	     ftnlen, ftnlen, ftnlen), repmi_(char *, char *, integer *, char *
	    , ftnlen, ftnlen, ftnlen);
    static integer iplus;
    extern /* Subroutine */ int hx2int_(char *, integer *, logical *, char *, 
	    ftnlen, ftnlen);
    static integer lccbeg, digbeg, lccend, uccbeg, digend, uccend, ispace;
    static doublereal scales[31];
    integer ndigit;
    static integer iexpch;
    logical fndexp;
    integer strbeg;
    logical negtiv;
    integer letter, strend, iexpon;
    static integer iminus;
    integer positn;
    doublereal tmpnum;

/* $ Abstract */

/*     Convert a string representing a double precision number in a */
/*     base 16 "scientific notation" into its equivalent double */
/*     precision number. */

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
/*     MAXMAN     P   Maximum number of digits in a hex mantissa. */
/*     STRING     I   Hex form string to convert to double precision. */
/*     NUMBER     O   Double precision value to be returned. */
/*     ERROR      O   A logical flag which is .TRUE. on error. */
/*     ERRMSG     O   A descriptive error message. */

/* $ Detailed_Input */

/*     STRING   is a character string containing a base 16 "scientific */
/*              notation" representation of a double precision number */
/*              which is to be converted to a double precision number. */
/*              Examples of such a string are: */

/*                 '2A^3' = ( 2/16 + 10/( 16**2 ) ) * 16**3 = 672.0 */

/*              and */

/*                 '-B^1' = - ( 11/16 ) * 16**1             = -11.0 */

/*              The following table describes the character set used to */
/*              represent the hexadecimal digits and their corresponding */
/*              values. */

/*                 Character     Value         Character     Value */
/*                 ---------    -------        ---------    ------- */
/*                    '0'         0.0D0           '8'         8.0D0 */
/*                    '1'         1.0D0           '9'         9.0D0 */
/*                    '2'         2.0D0         'A','a'      10.0D0 */
/*                    '3'         3.0D0         'B','b'      11.0D0 */
/*                    '4'         4.0D0         'C','c'      12.0D0 */
/*                    '5'         5.0D0         'D','d'      13.0D0 */
/*                    '6'         6.0D0         'E','e'      14.0D0 */
/*                    '7'         7.0D0         'F','f'      15.0D0 */

/*              The caret, or hat, character, '^', is used to */
/*              distinguish the exponent. */

/*              The plus sign, '+', and the minus sign, '-', are used, */
/*              and they have their usual meanings. */

/*              A base 16 "scientific notation" character string which */
/*              is to be parsed by this routine should consist of a sign, */
/*              '+' or '-' (the plus sign is optional for nonnegative */
/*              numbers), followed immediately by a contiguous sequence */
/*              of hexadecimal digits, the exponent character, and a */
/*              signed hexadecimal exponent. The exponent is required, */
/*              but the sign is optional for a nonnegative exponent. */

/*              A number in base 16 "scientific notation" consists of */
/*              a contiguous sequence of characters with one of the */
/*              following formats: */

/*                 (1)   h h h h  ... h ^H H  ... H */
/*                        1 2 3 4      n  1 2      m */

/*                 (2)   +h h h h  ... h ^H H  ... H */
/*                         1 2 3 4      n  1 2      m */

/*                 (3)   -h h h h  ... h ^H H  ... H */
/*                         1 2 3 4      n  1 2      m */

/*                 (4)    h h h h  ... h ^+H H  ... H */
/*                         1 2 3 4      n   1 2      m */

/*                 (5)   +h h h h  ... h ^+H H  ... H */
/*                         1 2 3 4      n   1 2      m */

/*                 (6)   -h h h h  ... h ^+H H  ... H */
/*                         1 2 3 4      n   1 2      m */

/*                 (7)   h h h h  ... h ^-H H  ... H */
/*                        1 2 3 4      n   1 2      m */

/*                 (8)   +h h h h  ... h ^-H H  ... H */
/*                         1 2 3 4      n   1 2      m */

/*                 (9)   -h h h h  ... h ^-H H  ... H */
/*                         1 2 3 4      n   1 2      m */

/*              where */

/*                 h  and H  denote hexadecimal digits; */
/*                  i      j */

/*                 ^         denotes exponentiation; */

/*              and */

/*                 + and - have their usual interpretations. */

/*              STRING may have leading and trailing blanks, but blanks */
/*              embedded within the significant portion of the input */
/*              string are not allowed. */

/* $ Detailed_Output */

/*     NUMBER   is the double precision value to be returned. The value */
/*              of this argument is not changed if an error occurs while */
/*              parsing the input string. */

/*     ERROR    is a logical flag which indicates whether an error */
/*              occurred while attempting to parse NUMBER from the input */
/*              character string STRING. ERROR will have the value */
/*              .TRUE. if an error occurs. It will have the value */
/*              .FALSE. otherwise. */

/*     ERRMSG   is a descriptive error message if an error occurs while */
/*              attempting to parse the number NUMBER from the */
/*              hexadecimal character string STRING, blank otherwise. */

/* $ Parameters */

/*     MAXMAN   is the maximum number of digits in a hexadecimal */
/*              mantissa. The value of MAXMAN is 31. */

/*              The current value of MAXMAN is more than sufficient for */
/*              most double precision implementations, providing almost */
/*              twice as many digits as can actually be produced. This */
/*              value may be changed when a greater precision is known */
/*              to exist among all of the supported platforms. */

/* $ Exceptions */

/*     Error free. */

/*     1)  If an unexpected character is encountered, an appropriate */
/*         error message will be set, and the routine will exit. The */
/*         value of NUMBER will be unchanged. */

/*     2)  If the input string represents a number that is larger in */
/*         absolute magnitude than the maximum representable */
/*         double precision number an appropriate error message */
/*         will be set, and the routine will exit. The value of */
/*         NUMBER will be unchanged. */

/*     3)  If the input string is blank, an appropriate error message */
/*         will be set, and the routine will exit. The value of */
/*         NUMBER will be unchanged. */

/*     4)  If the string has too many digits in the mantissa, then an */
/*         appropriate error message will be set, and the routine will */
/*         exit. The value of NUMBER will be unchanged. */

/*     5)  If the output error message string is not long enough to */
/*         contain the entire error message, the error message will be */
/*         truncated on the right. */

/*     6)  This routine does NOT check for underflow errors when */
/*         constructing a double precision number. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine will convert a character string containing a number */
/*     in base 16 "scientific notation" into its equivalent double */
/*     precision number. */

/*     This routine is one of a pair of routines which are used to */
/*     perform conversions between double precision numbers and */
/*     an equivalent base 16 "scientific notation" character string */
/*     representation: */

/*           DP2HX  -- Convert a double precision number into a base 16 */
/*                     "scientific notation" character string. */

/*           HX2DP  -- Convert a base 16 "scientific notation" */
/*                     character string into a double precision number. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Convert a set of character strings containing a base 16 */
/*        "scientific notation" representation of a double precision */
/*        number, to their double precision values. */


/*        Example code begins here. */


/*              PROGRAM HX2DP_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local constants. */
/*        C */
/*              INTEGER               ERRLEN */
/*              PARAMETER           ( ERRLEN = 80 ) */

/*              INTEGER               STRLEN */
/*              PARAMETER           ( STRLEN = 17 ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              CHARACTER*(ERRLEN)    ERRMSG */
/*              CHARACTER*(STRLEN)    NUMBER ( 16 ) */

/*              DOUBLE PRECISION      VALUE */

/*              INTEGER               I */

/*              LOGICAL               ERROR */

/*        C */
/*        C     Assign an array of strings representing, in base 16 */
/*        C     "scientific notation", double precision numbers. */
/*        C     Not all of them are valid representations. */
/*        C */
/*              DATA                  NUMBER / */
/*             .                  '89705F4136B4A6^-7', '12357898765X34', */
/*             .                  '1^1',               '-1^1', */
/*             .                  '4^3',               '-4^3', */
/*             .                  '7F5EB^5',           '7F5eb^5', */
/*             .                  '1B^2',              '+1B^2', */
/*             .                  '+1B^+2',            '0^0', */
/*             .                  ' ',                 '-AB238Z^2', */
/*             .                  '234ABC',            '234ABC^'    / */

/*        C */
/*        C     Loop over the NUMBER array, call HX2DP for each */
/*        C     element of NUMBER. */
/*        C */
/*              WRITE(*,'(A)') 'string             number' */
/*              WRITE(*,'(A)') '-----------------  ----------------' */

/*              DO I= 1, 16 */

/*                 CALL HX2DP ( NUMBER(I), VALUE, ERROR, ERRMSG ) */

/*                 IF ( ERROR ) THEN */

/*                    WRITE(*,'(A17,2X,A)') NUMBER(I), ERRMSG */

/*                 ELSE */

/*                    WRITE(*,'(A17,X,E17.9)') NUMBER(I), VALUE */

/*                 END IF */

/*              END DO */

/*        C */
/*        C     Finally, try with a number that has too many digits in */
/*        C     the mantissa. */
/*        C */
/*              CALL HX2DP ( '4ABC123AB346523BDC568798C2473678^1', */
/*             .             VALUE, ERROR, ERRMSG ) */

/*              WRITE(*,*) */
/*              WRITE(*,*) 'String 4ABC123AB346523BDC568798C2473678^1 ' */
/*             .        // 'produces:' */
/*              WRITE(*,*) '   ', ERRMSG */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        string             number */
/*        -----------------  ---------------- */
/*        89705F4136B4A6^-7   0.200000000E-08 */
/*        12357898765X34     ERROR: Illegal character 'X' encountered. */
/*        1^1                 0.100000000E+01 */
/*        -1^1               -0.100000000E+01 */
/*        4^3                 0.102400000E+04 */
/*        -4^3               -0.102400000E+04 */
/*        7F5EB^5             0.521707000E+06 */
/*        7F5eb^5             0.521707000E+06 */
/*        1B^2                0.270000000E+02 */
/*        +1B^2               0.270000000E+02 */
/*        +1B^+2              0.270000000E+02 */
/*        0^0                 0.000000000E+00 */
/*                           ERROR: A blank input string is not allowed. */
/*        -AB238Z^2          ERROR: Illegal character 'Z' encountered. */
/*        234ABC             ERROR: Missing exponent. */
/*        234ABC^            ERROR: Missing exponent. */

/*         String 4ABC123AB346523BDC568798C2473678^1 produces: */
/*            ERROR: Too many digits in the mantissa (> 31). */


/*        Note: The hat or caret, '^', signals an exponent. */

/*        Note that some errors are machine dependent. For example, */
/*        for a VAX using D_floating arithmetic we get: */

/*           STRING = '23BCE^30' */
/*           NUMBER = ( Not defined ) */
/*           ERROR  = .TRUE. */
/*           ERRMSG = 'ERROR: Number is too large to be represented.' */

/*           STRING = '-2abc3^22' */
/*           NUMBER = ( Not defined ) */
/*           ERROR  = .TRUE. */
/*           ERRMSG = 'ERROR: Number is too small to be represented.' */

/* $ Restrictions */

/*     1)  The current value of MAXMAN is more than sufficient for most */
/*         double precision implementations, providing almost twice as */
/*         many digits as can actually be produced. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     B.V. Semenov       (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR) (BVS) */

/*        Added IMPLICIT NONE statement. */

/*        The declaration of MAXMAN has been promoted to the */
/*        $Declarations section and the error produced when the maximum */
/*        number of digits for the mantissa is exceeded has been updated */
/*        to inform about MAXMAN value. */

/*        Edited the header to comply with NAIF standard. Added complete */
/*        code example based on existing example. */

/*        Updated $Brief_I/O, $Parameters, $Exceptions and $Restrictions */
/*        sections to properly describe MAXMAN. */

/*        Corrected $Revisions entries. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1994 (KRG) */

/*        Fixed a typo in the description of the input argument STRING. */
/*        The example showing the expansion of 160 into hexadecimal */
/*        was incorrect. 160 was replaced with 672 which makes the */
/*        example correct. */

/* -    SPICELIB Version 1.0.0, 26-OCT-1992 (KRG) */

/* -& */
/* $ Index_Entries */

/*     convert signed normalized hexadecimal string to d.p. */
/*     convert encoded d.p. number to d.p. number */
/*     convert base 16 scientific notation d.p. number */

/* -& */

/*     SPICELIB functions */


/*     Local Parameters */


/*     Local variables */


/*     Saved variables */


/*     Initial values */


/*     Here is a brief outline of the algorithm used to convert the */
/*     character string into its equivalent double precision number. */

/*        The input hexadecimal string is scanned from left to right. */

/*        0) Any leading white space is skipped. */

/*        1) The length of the significant portion of the string */
/*           is determined. */

/*        2) The sign of the mantissa is determined. */

/*        3) The digits of the hexadecimal mantissa are parsed. */

/*        4) The exponent of the number is parsed. */

/*        5) The mantissa of the double precision number is generated */
/*           by summing appropriately scaled values of the hexadecimal */
/*           mantissa digits which were collected in step 2. The */
/*           summation is performed so that the summands are added */
/*           in order of increasing magnitude to eliminate a potential */
/*           loss of significance which might occur otherwise. This */
/*           yields a number in the range of 1/BASE and 1.0 or zero. */

/*        6) The double precision number is then scaled by the exponent */
/*           obtained in step 3. */

    if (first) {

/*        If this is the first call, set up the array that is used to */
/*        properly scale each of the hexadecimal digits when summing */
/*        them to build a double precision number. Right now, the value */
/*        of MAXMAN, the maximum number of digits in a hexadecimal */
/*        mantissa, is 31. MAXMAN = 31 is more than sufficient for most */
/*        current double precision implementations, providing almost */
/*        twice as many digits as can actually be produced. This value */
/*        may be changed when a greater precision is known to exist on */
/*        any of the supported platforms. */

	first = FALSE_;
	scales[0] = .0625;
	for (i__ = 2; i__ <= 31; ++i__) {
	    scales[(i__1 = i__ - 1) < 31 && 0 <= i__1 ? i__1 : s_rnge("scales"
		    , i__1, "hx2dp_", (ftnlen)555)] = scales[(i__2 = i__ - 2) 
		    < 31 && 0 <= i__2 ? i__2 : s_rnge("scales", i__2, "hx2dp_"
		    , (ftnlen)555)] * .0625;
	}

/*        Initialize the upper and lower bounds for the decimal digits, */
/*        the upper and lower bounds for the uppercase hexadecimal */
/*        digits, the upper and lower bounds for the lowercase */
/*        hexadecimal digits, the space, the plus sign, and the minus */
/*        sign in the character sequence. */

	digbeg = '0';
	digend = '9';
	uccbeg = 'A';
	uccend = 'F';
	lccbeg = 'a';
	lccend = 'f';
	iminus = '-';
	iplus = '+';
	ispace = ' ';

/*        Also get the integer value for the exponent character. */

	iexpch = '^';

/*        Initialize some boundary values for error checking while */
/*        constructing the desired double precision number. These */
/*        are used to help determine whether an overflow condition */
/*        is imminent due to the overly large magnitude of a positive */
/*        or negative number. */

	mindp = dpmin_() * .0625;
	maxdp = dpmax_() * .0625;
    }

/*     There are no errors initially, so set the error flag to */
/*     .FALSE. */

    *error = FALSE_;

/*     If the string is blank, set the error flag and return immediately. */

    if (s_cmp(string, " ", string_len, (ftnlen)1) == 0) {
	*error = TRUE_;
	s_copy(errmsg, "ERROR: A blank input string is not allowed.", 
		errmsg_len, (ftnlen)43);
	return 0;
    }

/*     Initialize a few other things. */

    s_copy(errmsg, " ", errmsg_len, (ftnlen)1);
    tmpnum = 0.;

/*     Assume that the number is nonnegative. */

    negtiv = FALSE_;

/*     Skip any leading white space. We know that there is at least */
/*     one nonblank character at this point, so we will not loop */
/*     off the end of the string. */

    strbeg = 1;
    while(*(unsigned char *)&string[strbeg - 1] == ispace) {
	++strbeg;
    }

/*     Now, we want to find the end of the significant portion of */
/*     the input string and the position of the exponent character. */

    strend = strbeg + 1;
    more = TRUE_;
    while(more) {
	if (strend <= i_len(string, string_len)) {
	    if (s_cmp(string + (strend - 1), " ", string_len - (strend - 1), (
		    ftnlen)1) != 0) {
		++strend;
	    } else {
		more = FALSE_;
	    }
	} else {
	    more = FALSE_;
	}
    }

/*     At this point, STREND is one larger than the length of the */
/*     significant portion of the string because we incremented */
/*     its value after the test. We will subtract one from the */
/*     value of STREND so that it exactly represents the position */
/*     of the last significant character in the string. */

    --strend;

/*     Set the position pointer to the beginning of the significant */
/*     part, i.e., the nonblank part, of the string, because we are */
/*     now ready to try and parse the number. */

    positn = strbeg;

/*     The first character should be either a plus sign, '+', a */
/*     minus sign, '-', or a digit, '0' - '9', 'A' - 'F', or */
/*     'a' - 'f'. Anything else is bogus and we will catch it in */
/*     the main loop below. */

/*     If the character is a minus sign, we want to set the value of */
/*     NEGTIV to .TRUE. and increment the position. */

/*     If the character is a plus sign, we want to increment the */
/*     position. */

    if (*(unsigned char *)&string[positn - 1] == iminus) {
	negtiv = TRUE_;
	++positn;
    } else if (*(unsigned char *)&string[positn - 1] == iplus) {
	++positn;
    }

/*     Collect all of the digits in the mantissa, storing them */
/*     for later conversion. We do this because we want to add */
/*     the digits of the mantissa in increasing order so that we */
/*     do not lose any significance. */

/*     A normalized hexadecimal number must have an exponent, */
/*     which is represented by the hat character, EXPCHR, which */
/*     s why that test is part of the loop termination. */

/*     We currently have no digits, and we have not found the */
/*     exponent character yet. */

    ndigit = 0;
    fndexp = FALSE_;
    while(positn <= strend && ! fndexp) {
	letter = *(unsigned char *)&string[positn - 1];
	if (letter >= digbeg && letter <= digend) {
	    ++positn;
	    ++ndigit;
	    ival[(i__1 = ndigit - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge("ival",
		     i__1, "hx2dp_", (ftnlen)713)] = letter - digbeg;
	} else if (letter >= uccbeg && letter <= uccend) {
	    ++positn;
	    ++ndigit;
	    ival[(i__1 = ndigit - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge("ival",
		     i__1, "hx2dp_", (ftnlen)720)] = letter + 10 - uccbeg;
	} else if (letter >= lccbeg && letter <= lccend) {
	    ++positn;
	    ++ndigit;
	    ival[(i__1 = ndigit - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge("ival",
		     i__1, "hx2dp_", (ftnlen)727)] = letter + 10 - lccbeg;
	} else if (letter == iexpch) {

/*           We have found the exponent character, so set the */
/*           indicator and increment the position. */

	    fndexp = TRUE_;
	    ++positn;
	} else {
	    *error = TRUE_;
	    s_copy(errmsg, "ERROR: Illegal character '#' encountered.", 
		    errmsg_len, (ftnlen)41);
	    *(unsigned char *)&ch__1[0] = letter;
	    repmc_(errmsg, "#", ch__1, errmsg, errmsg_len, (ftnlen)1, (ftnlen)
		    1, errmsg_len);
	    return 0;
	}

/*        We need to make sure that the number of mantissa digits */
/*        remains less than or equal to the number of mantissa */
/*        digits that we declared, see the MAXMAN parameter. */

	if (ndigit > 31) {
	    *error = TRUE_;
	    s_copy(errmsg, "ERROR: Too many digits in the mantissa (> #).", 
		    errmsg_len, (ftnlen)45);
	    repmi_(errmsg, "#", &c__31, errmsg, errmsg_len, (ftnlen)1, 
		    errmsg_len);
	    return 0;
	}
    }

/*     At this point, we have found an exponent character, and: */

/*        1) We are beyond the end of the significant portion of the */
/*           string, which is an error: no exponent digits were found. */

/*        2) We are positioned on the first digit of the exponent, */
/*           and are ready to try and parse it. */

    if (positn <= strend) {

/*        If there is at least one significant character left in the */
/*        string, we need to try and parse it as an exponent. */

	hx2int_(string + (positn - 1), &iexpon, error, errmsg, string_len - (
		positn - 1), errmsg_len);
	if (*error) {

/*           If an error occurred while attempting to parse the */
/*           exponent, we simply want to exit. The error message */
/*           is already set. */

	    return 0;
	}
    } else {
	*error = TRUE_;
	s_copy(errmsg, "ERROR: Missing exponent.", errmsg_len, (ftnlen)24);
	return 0;
    }

/*     We now have everything that we need to build the double */
/*     precision number, a mantissa and an exponent. So, let's */
/*     start building the number. We need to be careful that we */
/*     do not overflow when we scale the number using the exponent. */

/*     First, we build up the mantissa ... */

    if (negtiv) {
	while(ndigit > 0) {
	    tmpnum -= dpval[(i__2 = ival[(i__1 = ndigit - 1) < 32 && 0 <= 
		    i__1 ? i__1 : s_rnge("ival", i__1, "hx2dp_", (ftnlen)805)]
		    ) < 16 && 0 <= i__2 ? i__2 : s_rnge("dpval", i__2, "hx2d"
		    "p_", (ftnlen)805)] * scales[(i__3 = ndigit - 1) < 31 && 0 
		    <= i__3 ? i__3 : s_rnge("scales", i__3, "hx2dp_", (ftnlen)
		    805)];
	    --ndigit;
	}
    } else {
	while(ndigit > 0) {
	    tmpnum += dpval[(i__2 = ival[(i__1 = ndigit - 1) < 32 && 0 <= 
		    i__1 ? i__1 : s_rnge("ival", i__1, "hx2dp_", (ftnlen)814)]
		    ) < 16 && 0 <= i__2 ? i__2 : s_rnge("dpval", i__2, "hx2d"
		    "p_", (ftnlen)814)] * scales[(i__3 = ndigit - 1) < 31 && 0 
		    <= i__3 ? i__3 : s_rnge("scales", i__3, "hx2dp_", (ftnlen)
		    814)];
	    --ndigit;
	}
    }

/*     At this point, one of the following is true: */

/*        1)  -1     <  TMPNUM <= -1/BASE */

/*        2)  1/BASE <= TMPNUM <  1 */

/*     or */

/*        3) TMPNUM = 0.0D0 */

/*     Now we to scale the normalized number using the exponent. If */
/*     the exponent is zero, we will simply fall through the loop */
/*     structures below at no greater cost than a few comparisons. */

    if (iexpon < 0) {

/*        We do not check for any sort of underflow conditions. */

	i__1 = -iexpon;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    tmpnum *= .0625;
	}
    } else {
	if (negtiv) {
	    i__1 = iexpon;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		if (tmpnum >= mindp) {
		    tmpnum *= 16.;
		} else {
		    *error = TRUE_;
		    s_copy(errmsg, "ERROR: Number is too small to be represe"
			    "nted.", errmsg_len, (ftnlen)45);
		    return 0;
		}
	    }
	} else {
	    i__1 = iexpon;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		if (tmpnum <= maxdp) {
		    tmpnum *= 16.;
		} else {
		    *error = TRUE_;
		    s_copy(errmsg, "ERROR: Number is too large to be represe"
			    "nted.", errmsg_len, (ftnlen)45);
		    return 0;
		}
	    }
	}
    }

/*     If we got to here, we have successfully parsed the hexadecimal */
/*     string into a double precision number. So, set the value and */
/*     return. */

    *number = tmpnum;
    return 0;
} /* hx2dp_ */

