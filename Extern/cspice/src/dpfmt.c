/* dpfmt.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static logical c_true = TRUE_;

/* $Procedure DPFMT ( Format a double precision number ) */
/* Subroutine */ int dpfmt_(doublereal *x, char *pictur, char *str, ftnlen 
	pictur_len, ftnlen str_len)
{
    /* System generated locals */
    integer i__1, i__2, i__3, i__4;

    /* Builtin functions */
    integer i_len(char *, ftnlen), s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    char fill[1];
    integer dpat;
    char sign[1];
    integer i__;
    extern /* Subroutine */ int zzvsbstr_(integer *, integer *, logical *, 
	    char *, logical *, ftnlen);
    doublereal y;
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen), zzvststr_(doublereal *, char *, integer *, 
	    ftnlen);
    logical shift;
    extern integer ncpos_(char *, char *, integer *, ftnlen, ftnlen);
    extern /* Subroutine */ int dpstr_(doublereal *, integer *, char *, 
	    ftnlen);
    integer start;
    extern /* Subroutine */ int ljust_(char *, char *, ftnlen, ftnlen), 
	    rjust_(char *, char *, ftnlen, ftnlen);
    char mystr[32];
    integer declen, sigdig;
    logical needsn;
    integer lastch, sgnlen, frstch, intlen, firstb;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen);
    logical ovflow;
    integer expsiz, sprsiz, exp__;
    extern integer pos_(char *, char *, integer *, ftnlen, ftnlen);

/* $ Abstract */

/*     Create a formatted string that represents a double precision */
/*     number, using a format picture. */

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
/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     X          I   a double precision number. */
/*     PICTUR     I   a string describing the appearance of the output */
/*     STR        O   a string representing X as prescribed by PICTUR */

/* $ Detailed_Input */

/*     X        is any double precision number. */

/*     PICTUR   is a string used to describe the format of the */
/*              output string. There are four special characters */
/*              recognized by DPFMT --- a leading + or -, a leading */
/*              zero ( '0' ) or a zero that follows a leading + or -, */
/*              and the first decimal point of the string. */

/*              All other non-blank characters are regarded as */
/*              equivalent. The picture ends at the first blank */
/*              character. The effects associated with the various */
/*              characters in a picture are spelled out in the */
/*              description of the output STRING. */

/*              The following pictures are treated as errors. */

/*              ' ', '+', '-', '.', '+.', '-.' */

/* $ Detailed_Output */

/*     STRING   is a string representing X that matches the input */
/*              picture. The format of STRING is governed by PICTUR. */
/*              It will represent X rounded to the level of precision */
/*              specified by PICTUR. */

/*              If the first character of the picture is a minus sign, */
/*              the first character in the output string will be */
/*              a blank if the number is non-negative, a minus sign */
/*              if the number is negative. */

/*              If the first character of the picture is a plus sign, */
/*              the first character of the output string will be a */
/*              plus if the number is positive, a blank if the number */
/*              is zero, and a minus sign if the number is negative. */

/*              If the first character of the string is NOT a sign */
/*              (plus or minus) the first character of the output */
/*              string will be a minus sign if the number is negative */
/*              and will be the first character of the integer part */
/*              of the number otherwise. */

/*              The integer portion of STRING will contain the same */
/*              number of characters as appear before the decimal */
/*              point (or last character if there is no decimal */
/*              point) but after a leading + or -. */

/*              If the picture begins with any of the following */

/*                 '+0', '-0', or '0' */

/*              it is said to have a leading zero. If a picture has */
/*              a leading zero and the integer portion is not large */
/*              enough to fill up the integer space specified by */
/*              PICTUR, STRING will be zero padded from the sign (if */
/*              one is required) up to the first character of the */
/*              integer part of the number. */

/*              If picture does NOT have a leading zero and the */
/*              integer portion is not large enough to fill up the */
/*              space specified by PICTUR, STRING will be blank */
/*              padded on the left between the sign (if one is */
/*              required) and the first character of the integer part */
/*              of the number. */

/*              If a decimal point ( '.' ) is present in PICTUR it */
/*              will be present following the integer portion of */
/*              STRING. Moreover, the decimal portion of STRING will */
/*              contain the same number of digits as there are */
/*              non-blank characters following the decimal point in */
/*              PICTUR. However, only the first 14 digits starting */
/*              with the first non-zero digit are meaningful. */

/*              If the format specified by PICTUR does not provide */
/*              enough room for the integer portion of X, the routine */
/*              determines whether or not the number of characters */
/*              present in the picture is sufficient to create a */
/*              representation for X using scientific notation. If */
/*              so, the output is displayed using scientific notation */
/*              (leading signs, if they are present in PICTUR, will */
/*              also appear in STRING).   If the format specified by */
/*              PICTUR is too short to accommodate scientific */
/*              notation, the output string is filled with '*' to the */
/*              same length as the length of PICTUR. Leading signs */
/*              are not preserved in this overflow case. */

/*              STRING may overwrite PICTUR. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If PICTUR begins with a blank, the error SPICE(NOPICTURE) is */
/*         signaled. */

/*     2)  If PICTUR consists only of '+', '-', '.', '+.' or '-.' are */
/*         regarded as invalid (there's no significant component to the */
/*         picture.) therefore, the error SPICE(BADPICTURE) is signaled. */

/*     3)  If the length of STR is less than the length of the first */
/*         non-blank portion of PICTUR, the error SPICE(OUTPUTTOOSHORT) */
/*         is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine provides a mechanism for producing numeric strings */
/*     formatted according to a user supplied picture. We expect that */
/*     the string produced by this routine will be used to assist in */
/*     the construction of a string that can be read by people. */

/*     Note that the process of converting a double precision number */
/*     to a string, in not precisely invertible even if the string */
/*     contains all of the significant figures allowed by this */
/*     routine. You should not anticipate that the string produced */
/*     by this routine can be "read" into a double precision number */
/*     to reproduce the double precision number X. To the level of */
/*     accuracy implied by the string representation, they will be */
/*     the same. But, they are unlikely to have the same internal */
/*     binary representation. */

/* $ Examples */

/*     Suppose that X has the binary representation of PI. Then the */
/*     table below illustrates the strings that would be produced */
/*     by a variety of different pictures. */

/*     PICTUR         |    STRING */
/*     ------------------------------- */
/*     '0x.xxx'       |  '03.142' */
/*     'xx.xxx'       |  ' 3.142' */
/*     '+xxx.yyyy'    |  '+  3.1416' */
/*     '-.yyyy'       |  '******' */
/*     'xxxxxxxx'     |  '       3' */
/*     '00xx'         |  '0003' */
/*     '-00.0000000'  |  ' 03.1415927' */
/*     '00'           |  '03' */
/*     'x.'           |  '3.' */
/*     '.mynumber'    |  '3.142E+00' */
/*     'my dog spot'  |  ' 3' */
/*     'my.dog spot'  |  ' 3.142' */
/*     '+my.dog,spot' |  '+ 3.14159265' */



/*     Suppose that X has the binary representation of 2/3. Then the */
/*     table below illustrates the strings that would be produced */
/*     by a variety of different pictures. */

/*     PICTUR         |    STRING */
/*     ------------------------------- */
/*     '+x.xxx'       |  '+0.667' */
/*     '+xx.xxx'      |  '+ 0.667' */
/*     'xxx.yyyy'     |  '  0.6667' */
/*     '.yyyy'        |  '.6667' */
/*     'xxxxxxxx'     |  '       1' */
/*     '00xx'         |  '0001' */
/*     '-0.0000000'   |  ' 0.6666667' */
/*     '00'           |  '01' */
/*     'x.'           |  '1.' */
/*     'mynumber'     |  '       1' */
/*     'my dog spot'  |  ' 1' */
/*     'my.dog spot'  |  ' 0.667' */
/*     'my.dog,spot'  |  ' 0.66666667' */

/*     Suppose that X has the binary representation of -8/9. Then the */
/*     table below illustrates the strings that would be produced */
/*     by a variety of different pictures. */


/*     PICTUR         |    STRING */
/*     ------------------------------- */
/*     '+x.xxx'       |  '-0.889' */
/*     '-00.xxxx'     |  '-00.8889' */
/*     'xxx.xxx'      |  ' -0.889' */
/*     '000.000'      |  '-00.889' */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.3, 27-OCT-2021 (JDR) (EDW) (NJB) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.2, 31-JAN-2008 (BVS) */

/*        Removed non-standard end-of-declarations marker */
/*        'C%&END_DECLARATIONS' from comments. */

/* -    SPICELIB Version 1.0.1, 22-JUN-1998 (WLT) */

/*        A number of typographical and grammatical errors */
/*        were corrected in the header. */

/* -    SPICELIB Version 1.0.0, 17-SEP-1996 (WLT) */

/* -& */
/* $ Index_Entries */

/*     format a string representing a d.p. number */
/*     string from a d.p. number and format picture */

/* -& */

/*     SPICELIB functions */


/*     Initial values */


/*     Determine where the picture ends. */

    firstb = pos_(pictur, " ", &c__1, pictur_len, (ftnlen)1);
    if (firstb == 0) {
	lastch = i_len(pictur, pictur_len);
    } else {
	lastch = firstb - 1;
    }

/*     Make sure there is a picture to worry about. */

    if (lastch == 0) {
	chkin_("DPFMT", (ftnlen)5);
	setmsg_("The format picture must begin with a non-blank character. T"
		"he picture supplied was begun with a blank.", (ftnlen)102);
	sigerr_("SPICE(NOPICTURE)", (ftnlen)16);
	chkout_("DPFMT", (ftnlen)5);
	return 0;
    } else if (lastch == 1) {
	if (s_cmp(pictur, "+", pictur_len, (ftnlen)1) == 0 || s_cmp(pictur, 
		"-", pictur_len, (ftnlen)1) == 0 || s_cmp(pictur, ".", 
		pictur_len, (ftnlen)1) == 0) {
	    chkin_("DPFMT", (ftnlen)5);
	    setmsg_("Format pictures must have at least one significant char"
		    "acter. The picture provided '#' does not. ", (ftnlen)97);
	    errch_("#", pictur, (ftnlen)1, (ftnlen)1);
	    sigerr_("SPICE(BADPICTURE)", (ftnlen)17);
	    chkout_("DPFMT", (ftnlen)5);
	    return 0;
	}
    } else if (lastch == 2) {
	if (s_cmp(pictur, "+.", pictur_len, (ftnlen)2) == 0 || s_cmp(pictur, 
		"-.", pictur_len, (ftnlen)2) == 0) {
	    chkin_("DPFMT", (ftnlen)5);
	    setmsg_("Format pictures must have at least one significant char"
		    "acter. The picture provided '#' does not. ", (ftnlen)97);
	    errch_("#", pictur, (ftnlen)1, (ftnlen)2);
	    sigerr_("SPICE(BADPICTURE)", (ftnlen)17);
	    chkout_("DPFMT", (ftnlen)5);
	    return 0;
	}
    } else if (lastch > i_len(str, str_len)) {
	chkin_("DPFMT", (ftnlen)5);
	setmsg_("The output string is not long enough to accommodate a numbe"
		"r formatted according to the supplied format picture. The ou"
		"tput string has length #. The output picture '#' requires # "
		"characters. ", (ftnlen)191);
	i__1 = i_len(str, str_len);
	errint_("#", &i__1, (ftnlen)1);
	errch_("#", pictur, (ftnlen)1, lastch);
	errint_("#", &lastch, (ftnlen)1);
	sigerr_("SPICE(OUTPUTTOOSHORT)", (ftnlen)21);
	chkout_("DPFMT", (ftnlen)5);
	return 0;
    }

/*     If we get this far, the routine can go ahead and do its business. */
/*     Determine the sign of X.  Also, determine how many characters */
/*     are needed to represent the sign if leading sign is suppressed for */
/*     positive numbers. */

    if (*x > 0.) {
	*(unsigned char *)sign = '+';
	sprsiz = 0;
    } else if (*x < 0.) {
	*(unsigned char *)sign = '-';
	sprsiz = 1;
    } else {
	*(unsigned char *)sign = ' ';
	sprsiz = 0;
    }

/*     Look at the picture and see if a leading sign is required and */
/*     if so whether the sign just determined should use a different */
/*     character and how many characters are needed for the sign. */

    if (*(unsigned char *)pictur == '+') {
	needsn = TRUE_;
	sgnlen = 1;
    } else if (*(unsigned char *)pictur == '-') {
	needsn = TRUE_;
	sgnlen = 1;
	if (*x > 0.) {
	    *(unsigned char *)sign = ' ';
	}
    } else {
	if (*x > 0.) {
	    *(unsigned char *)sign = ' ';
	}
	needsn = FALSE_;
	sgnlen = sprsiz;
    }

/*     If we need a leading sign. The numeric part of the string */
/*     will start at character 2.  Otherwise it starts at character 1. */

    if (needsn) {
	start = 2;
    } else {
	start = 1;
    }

/*     We can set the sign portion of the string now. */

    s_copy(str, sign, str_len, (ftnlen)1);

/*     Determine what character should be use for leading characters */
/*     before the first significant character of the output string. */

    if (*(unsigned char *)&pictur[start - 1] == '0') {
	*(unsigned char *)fill = '0';
    } else {
	*(unsigned char *)fill = ' ';
    }

/*     See if there is a decimal point. */

    dpat = pos_(pictur, ".", &c__1, pictur_len, (ftnlen)1);

/*     The integer part is the stuff to the left of the first */
/*     decimal point and that follows the sign (if there is one */
/*     that is explicitly required.  The length of the decimal */
/*     portion is the stuff to the right of the decimal point. */

    if (dpat > 0) {
	intlen = dpat - start;
	declen = lastch - dpat;
    } else {
	intlen = lastch - start + 1;
	declen = -1;
    }

/*     If a sign was not explicitly requested by placing it in */
/*     the first digit of the picture START will be 1.  If in */
/*     addition X is less than zero ( SGNLEN will be 1 in this */
/*     case) we have one fewer digits available for the integer */
/*     portion of the string than is currently set in INTLEN. */
/*     Adjust INTLEN to reflect the actual number of digits */
/*     available. */

/*     Also set the SHIFT flag to .TRUE. so that we know to swap */
/*     the sign and any blanks that might lie between the sign */
/*     and the first significant character of the output string. */

    if (start == 1 && sgnlen == 1) {
	--intlen;
	shift = TRUE_;

/*        If INTLEN has become negative (i.e. -1) the picture */
/*        must be of the form .xxxxx and the input number must */
/*        be negative. Add 1 back onto the INTLEN but take one */
/*        away from the decimal length DECLEN. */

	if (intlen == -1) {
	    intlen = 0;
	    --declen;
	    if (declen == 0 && intlen == 0) {

/*              There is no room for anything other than a */
/*              decimal point.  We simply fill the output */
/*              string with the '*' character. */

		i__1 = lastch;
		for (i__ = 1; i__ <= i__1; ++i__) {
		    *(unsigned char *)&str[i__ - 1] = '*';
		}
		return 0;
	    }
	}
    } else {
	shift = FALSE_;
    }

/*     Create the "virtual decimal string" associated with the */
/*     unsigned part of X. */

    y = abs(*x);
    zzvststr_(&y, fill, &exp__, (ftnlen)1);

/*     The actual number of digits required to print the unsigned integer */
/*     portion X is EXP + 1 (provided EXP is at least 0.) We have */
/*     INTLEN slots available.  So if EXP + 1 is more than INTLEN */
/*     ( which is equivalent to EXP being at least INTLEN) we don't */
/*     have enough room to print the unsigned integer portion of the */
/*     number. */

    if (exp__ >= intlen && y != 0.) {

/*        See if we have room to print an exponential form. */
/*        First we need the number of characters for the */
/*        exponent which is always of the form 'E+dd...' */

/* Computing MIN */
	i__1 = 1, i__2 = exp__ / 1000;
/* Computing MIN */
	i__3 = 1, i__4 = exp__ / 100;
	expsiz = min(i__1,i__2) + 4 + min(i__3,i__4);

/*        The number of significant digits that can be printed is the */
/*        size of the picture minus:   the size of the sign */
/*                                     the size of the exponent */
/*                                     the size of the decimal point. */

	sigdig = lastch - sgnlen - expsiz - 1;

/*        If we don't have room for at least one significant digit, */
/*        there's not much we can do.  Fill the string with '*'. */

	if (sigdig < 1) {
	    i__1 = lastch;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		*(unsigned char *)&str[i__ - 1] = '*';
	    }
	} else {
	    dpstr_(x, &sigdig, mystr, (ftnlen)32);
	    *(unsigned char *)mystr = *(unsigned char *)sign;
	    ljust_(mystr, str, (ftnlen)32, str_len);
	    rjust_(str, str, lastch, lastch);
	}
	return 0;
    }

/*     One more check.  If -INTLEN is greater than DECLEN, or if */
/*     both are zero, we don't have room to create an output string. */

    if (intlen == 0 && declen == 0 || -intlen > declen) {
	i__1 = lastch;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    *(unsigned char *)&str[i__ - 1] = '*';
	}
	return 0;
    }

/*     We have a reasonable chance of successfully constructing */
/*     the string without overflow. */

    start = sgnlen + 1;
    i__1 = -intlen;
    zzvsbstr_(&i__1, &declen, &c_true, str + (start - 1), &ovflow, str_len - (
	    start - 1));

/*     We might be done at this point.  The IF-THEN block below */
/*     handles the one snag that could arise. */

/*     If the first digit is a zero as a result of rounding it up */
/*     OVFLOW will be true.  This means we don't have enough room */
/*     in the picture for the integer portion of the string.  We try */
/*     to make an exponential picture. */

    if (ovflow) {

/*        See if we have room to print an exponential form. */

/* Computing MIN */
	i__1 = 1, i__2 = exp__ / 1000;
/* Computing MIN */
	i__3 = 1, i__4 = exp__ / 100;
	expsiz = min(i__1,i__2) + 4 + min(i__3,i__4);

/*        The number of significant digits that can be printed is the */
/*        size of the picture minus:   the size of the sign */
/*                                     the size of the exponent */
/*                                     the size of the decimal point. */

	sigdig = lastch - sgnlen - expsiz - 1;
	if (sigdig < 1) {
	    i__1 = lastch;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		*(unsigned char *)&str[i__ - 1] = '*';
	    }
	} else {
	    dpstr_(x, &sigdig, mystr, (ftnlen)32);
	    *(unsigned char *)mystr = *(unsigned char *)sign;
	    ljust_(mystr, str, (ftnlen)32, str_len);
	    rjust_(str, str, lastch, lastch);
	    return 0;
	}
    } else if (shift) {

/*        We need to move the sign right until, there are no */
/*        blanks between it and the next character. */

	frstch = ncpos_(str, " -", &c__1, str_len, (ftnlen)2);
	if (frstch > 2) {
	    i__1 = frstch - 2;
	    s_copy(str + i__1, str, frstch - 1 - i__1, (ftnlen)1);
	    *(unsigned char *)str = ' ';
	}
    }
    return 0;
} /* dpfmt_ */

