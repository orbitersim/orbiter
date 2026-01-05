/* dpstrf.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c_n1 = -1;
static logical c_false = FALSE_;
static logical c_true = TRUE_;

/* $Procedure DPSTRF ( Double Precision Number to Character ) */
/* Subroutine */ int dpstrf_(doublereal *x, integer *sigdig, char *format, 
	char *string, ftnlen format_len, ftnlen string_len)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer i_len(char *, ftnlen);

    /* Local variables */
    integer last, i__, j;
    extern /* Subroutine */ int zzvsbstr_(integer *, integer *, logical *, 
	    char *, logical *, ftnlen);
    doublereal y;
    extern /* Subroutine */ int zzvststr_(doublereal *, char *, integer *, 
	    ftnlen);
    integer first;
    extern /* Subroutine */ int dpstr_(doublereal *, integer *, char *, 
	    ftnlen);
    integer maxdig, lastch;
    logical ovflow;
    integer exp__;

/* $ Abstract */

/*     Take a double precision number and convert it to an */
/*     equivalent formatted character string representation (base 10). */

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
/*     CONVERSION */
/*     PARSING */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     X          I   A double precision number */
/*     SIGDIG     I   The number of significant digits saved for output */
/*     FORMAT     I   'E' for scientific, 'F' for floating point. */
/*     STRING     O   A character string representation of X */

/* $ Detailed_Input */

/*     X        is a double precision number. */

/*     SIGDIG   is the number of significant digits that are desired */
/*              for the output string. */

/*     FORMAT   is a character flag that indicates how the double */
/*              precision number should be represented. The two */
/*              acceptable inputs are 'E' and 'F'.  If the input */
/*              is 'E' then the number will be displayed with an */
/*              exponent in scientific notation. It will have the */
/*              form 'sx.xxx - - - xxxxxEsyy' where there are */
/*              SIGDIG x's and s is ' ' or '-' at its first occurrence */
/*              and '-' or '+' in the second. */

/*              If the input is 'F' then the number will be */
/*              displayed without an exponent --- the representation */
/*              will be strictly decimal. The first symbol will be */
/*              a sign ('-' or ' '). */

/* $ Detailed_Output */

/*     STRING   is a character representation of X to the number of */
/*              significant digits specified by SIGDIG. The number of */
/*              spaces required to return the requested character */
/*              string is SIGDIG + 6. If STRING is not declared to */
/*              have adequate length, the number returned will be */
/*              truncated on the right. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  If SIGDIG is less than one, this routine returns one */
/*         significant digit in the output string. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine computes an approximate character representation */
/*     of the input string X. The maximum number of significant */
/*     digits returned is 14 (in F format there may be many extra */
/*     zeros returned but only a maximum of 14 digits will be */
/*     significant. */

/*     The output string is left justified. */

/*     This routine has the advantage that it does not use an internal */
/*     file and is about twice as fast as an internal write. It can */
/*     be used as part of character function without fear of introducing */
/*     recursive I/O conflicts. It is intended to be an approximate */
/*     inverse to the subroutine NPARSD. */

/*     IF you want the character string representation of a double */
/*     precision number to be the same as that produced by a formatted */
/*     write statement use a FORTRAN write statement. */

/*     For example the number represented by the string */

/*           1.245454545454545454545E+01 */

/*     when read (via a FORTRAN READ statement) into the DP variable X */
/*     and converted back to a character string having 14 significant */
/*     digits by this routine yields */

/*           1.2454545454545E+01  in E format */
/*           12.454545454545      in F format */

/*     The FORTRAN write statement */

/*           WRITE ( 6, FMT='(P1E)' ) X */

/*     yields */

/*           1.2454545454545454E+01 */

/*     If this is too much error for your application DO NOT use this */
/*     routine. You should be aware however, that a character string */
/*     read into a double precision number may not WRITE out with an */
/*     equivalent character representation as was input. */

/*     For example on a VAX 11/780 if you */

/*           READ  (5,*)         X */
/*           WRITE (6,FMT='(E)') X */

/*     and enter a value of 7.00000001 for the read statement */
/*     the output written will be 0.7000000010000001E+01 */

/* $ Examples */

/*     Suppose that you wished to insert the character representation */
/*     of some DOUBLE PRECISION number into a line of text. */

/*     For example suppose X contains the double precision number */
/*     4.268176872928187 and you would like to insert the character */
/*     representation of this number to 2 places between the strings */

/*     'There are', 'meters between lamp posts' */

/*     You could perform the following sequence of steps */


/*           DOUBLE PRECISION  X */
/*           CHARACTER*5       DISTANCE */
/*           CHARACTER*80      MESSAGE */

/*           CALL DPSTRF ( X, 2, 'F', DISTANCE ) */

/*           MESSAGE = 'There are '                // */
/*          .           DISTANCE                   // */
/*          .          'meters between lamp posts' */
/*          . */

/*     C */
/*     C     Squeeze any extra spaces out of the message string. */
/*     C */
/*           CALL CMPRSS ( ' ', 1, MESSAGE, MESSAGE ) */



/*     The string MESSAGE would contain: */

/*          'There are 4.2 meters between lamp posts' */

/* $ Restrictions */

/*     1)  The maximum number of significant digits returned is 14. */

/*     2)  If the output string is not declared to be adequately large */
/*         the numeric string will be truncated to the side opposite its */
/*         justification (At least SIGDIG + 6 characters are needed in E */
/*         format, in F format the size required is dependent upon the */
/*         input X and the number of significant digits requested. In */
/*         extreme cases up to 56 characters may be required.) */

/*     3)  This routine makes explicit use of the format of the string */
/*         returned by DPSTR, should that routine change, substantial */
/*         work may be required to bring this routine back up to snuff. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.1, 12-AUG-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.2.0, 17-SEP-1996 (WLT) */

/*        Upgraded routine to handle arbitrary magnitude d.p. numbers. */

/* -    SPICELIB Version 1.1.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.1.0, 30-JUL-1990 (WLT) */

/*        The routine was repaired so that references to zero-length */
/*        strings ( for example STRING(4:3) ) are not made. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) */

/* -& */
/* $ Index_Entries */

/*     d.p. number to character with formatting */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.1.0, 30-JUL-1990 (WLT) */

/*        As previously implemented, one would occasionally reference */
/*        a zero length substring of the variable NUMSTR. This was */
/*        O.K. under VAX Fortran, because it allows such references. */
/*        However, most implementations of Fortran are not as forgiving. */

/* -& */

/*     Local variables */

/* Computing MIN */
    i__1 = 14, i__2 = max(1,*sigdig);
    maxdig = min(i__1,i__2);

/*     If the format is 'E' we just let DPSTR handle the problem. */

    if (*(unsigned char *)format == 'E') {
	dpstr_(x, &maxdig, string, string_len);
	return 0;
    }

/*     If we're still here, we have a decimal format requested.  Set */
/*     the sign for the number. */

    if (*x < 0.) {
	s_copy(string, "-", string_len, (ftnlen)1);
    } else {
	s_copy(string, " ", string_len, (ftnlen)1);
    }

/*     If X is zero, we can handle this without any regard to the */
/*     exponent. */

    if (*x == 0.) {
	zzvststr_(x, " ", &exp__, (ftnlen)1);
	zzvsbstr_(&c_n1, &maxdig, &c_false, string + 1, &ovflow, string_len - 
		1);
	return 0;
    }

/*     We've already set the sign, now we deal with the unsigned */
/*     portion of X. */

    y = abs(*x);

/*     Create a virtual decimal string for Y. */

    zzvststr_(&y, " ", &exp__, (ftnlen)1);

/*     Now we can just fill in the string by reading the appropriate */
/*     substring from the virtual decimal string.  We need to compute */
/*     the first and last virtual digits to retrieve.  To do this */
/*     we look at EXP. */

    if (exp__ >= 0) {
	first = -exp__ - 1;
    } else {
	first = -exp__;
    }
    last = first + maxdig - 1;
    if (first < 0 && last >= 0) {
	++last;
    }
    first = min(-1,first);
    zzvsbstr_(&first, &last, &c_true, string + 1, &ovflow, string_len - 1);
    if (ovflow) {
	--first;
	zzvsbstr_(&first, &last, &c_true, string + 1, &ovflow, string_len - 1)
		;

/*        We need to blank out the last digit of string. */

	lastch = last - first + 2;
	if (last > 0 && lastch <= i_len(string, string_len)) {
	    s_copy(string + (lastch - 1), " ", string_len - (lastch - 1), (
		    ftnlen)1);
	}
    }
    if (last < 0) {
	j = last - first + 3;
	for (i__ = last + 1; i__ <= -1; ++i__) {
	    if (j <= i_len(string, string_len)) {
		*(unsigned char *)&string[j - 1] = '0';
	    }
	    ++j;
	}
	if (j <= i_len(string, string_len)) {
	    *(unsigned char *)&string[j - 1] = '.';
	}
    }
    return 0;
} /* dpstrf_ */

