/* dpstr.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;

/* $Procedure DPSTR ( Double Precision Number to Character ) */
/* Subroutine */ int dpstr_(doublereal *x, integer *sigdig, char *string, 
	ftnlen string_len)
{
    /* Initialized data */

    static doublereal power[18] = { 1.,10.,100.,1e3,1e4,1e5,1e6,1e7,1e8,1e9,
	    1e10,1e11,1e12,1e13,1e14,1e15,1e16,1e17 };
    static doublereal ipower[18] = { 1.,.1,.01,.001,1e-4,1e-5,1e-6,1e-7,1e-8,
	    1e-9,1e-10,1e-11,1e-12,1e-13,1e-14,1e-15,1e-16,1e-17 };
    static char digits[1*10] = "0" "1" "2" "3" "4" "5" "6" "7" "8" "9";
    static doublereal values[10] = { 0.,1.,2.,3.,4.,5.,6.,7.,8.,9. };
    static char vaxexp[2*41] = "00" "01" "02" "03" "04" "05" "06" "07" "08" 
	    "09" "10" "11" "12" "13" "14" "15" "16" "17" "18" "19" "20" "21" 
	    "22" "23" "24" "25" "26" "27" "28" "29" "30" "31" "32" "33" "34" 
	    "35" "36" "37" "38" "39" "40";

    /* System generated locals */
    address a__1[2];
    integer i__1, i__2, i__3[2];
    doublereal d__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen), s_cat(char *,
	     char **, integer *, integer *, ftnlen);
    double d_lg10(doublereal *);
    integer s_rnge(char *, integer, char *, integer);
    double d_nint(doublereal *);

    /* Local variables */
    doublereal exp10;
    char expc[20];
    integer last;
    doublereal copy;
    char zero[28];
    integer i__, k, postn, maxsig, expont;
    extern /* Subroutine */ int intstr_(integer *, char *, ftnlen);
    char numstr[32];

/* $ Abstract */

/*     Take a double precision number and convert it to */
/*     an equivalent character string representation (base 10). */

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
/*     SIGDIG     I   The number of significant digits placed in output */
/*     STRING     O   A character string representation of X */

/* $ Detailed_Input */

/*     X        is a double precision number. */

/*     SIGDIG   is the number of significant digits that are desired */
/*              for the output string. */

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
/*     digits returned is 14. The representation returned will be */
/*     the same as that given by the FORTRAN write statement */

/*         WRITE ( STRING, FMT=(P1E23.xx) */

/*     where xx is a two digit number that represents MIN(14,SIGDIG). */
/*     The last decimal place is rounded. The output string is left */
/*     justified. */

/*     This routine has the advantage that it does not use an internal */
/*     file and is about 2.3 times as fast as an internal write. It can */
/*     be used as part of character function without fear of introducing */
/*     recursive I/O conflicts. It is intended to be an approximate */
/*     inverse to the subroutine NPARSD. */

/*     There is of course no formatting of the output string. All */
/*     outputs are written in scientific notation. */

/*     IF you want the character string representation of a double */
/*     precision number to be the same as that produced by a formatted */
/*     write statement use a FORTRAN write statement. */

/*     For example the number represented by the string */

/*           1.245454545454545454545E+01 */

/*     when read (via a FORTRAN READ statement) into the DP variable X */
/*     and converted back to a character string having 14 significant */
/*     digits by this routine yields */

/*           1.2454545454545E+01 */

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

/*     This routine is intended for use by routines that manipulate */
/*     character strings. For example, it may be desirable for a */
/*     routine to be able to take a character string input such as */

/*           12 miles */

/*     and convert it to the string */

/*           1.932E+02 km */

/*     or to simply */

/*           1.932E+02 */

/*     The arithmetic is of course most easily handled using numeric */
/*     variables. However, it may be that a string is required for */
/*     subsequent processing of the input.  A SPICELIB routine NPARSD */
/*     exists that will take a character representation of a number */
/*     and convert it to a DOUBLE PRECISION number. The 12 above */
/*     can be converted to double precision using NPARSD,  the d.p. */
/*     number can then be multiplied by the 1.61... and the result */
/*     converted back to a string using this routine. */

/*     Suppose the following declarations are made */

/*           CHARACTER*(80)     TEXT */
/*           CHARACTER*(80)     NUMBER */
/*           CHARACTER*(80)     SCRATCH */

/*           DOUBLE PRECISION   X */
/*           INTEGER            I */

/*     and that TEXT contains the string '12 mi'.  Then the following */
/*     code would produce a character string  '1.932E+01 KM' */

/*           CALL NEXTWD (  TEXT,   NUMBER, SCRATCH   ) */
/*           CALL NPARSD (  NUMBER, X,      ERROR,  I ) */

/*           IF ( ERROR .EQ. ' ' ) THEN */

/*              X    = X * 1.61D0 */
/*              CALL DPSTR ( X, 5, NUMBER ) */
/*              TEXT = NUMBER(1:10) // 'KM' */

/*           ELSE */
/*              . */
/*              . */
/*              create an error message, try again, etc. */
/*              . */
/*              . */
/*           END IF */

/* $ Restrictions */

/*     1)  The format of the string returned by this routine is used in */
/*         DPSTRF which is in the call tree to DPFMT. Changes to the */
/*         format of the output string may have unexpected consequences */
/*         for these SPICE routines. Please check those routines before */
/*         modifying this routine. */

/*     2)  The maximum number of significant digits returned is 14. */

/*     3)  If the output string is not declared to be adequately large */
/*         (at least SIGDIG + 6), the numeric string will be truncated to */
/*         the side opposite its justification. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     H.A. Neilan        (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.0, 20-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.1.1, 09-SEP-1996 (WLT) */

/*        Added a reference to the header concerning the dependency */
/*        of the SPICE routines DPSTRF and DPFMT on the format of */
/*        the string produced by this routine. */

/* -    SPICELIB Version 1.1.0, 11-JUN-1992 (WLT) */

/*        A bug that caused this routine to have a floating point */
/*        overflow for values of X close to zero was corrected. In */
/*        addition the restriction on range of exponents supported */
/*        has been removed. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (HAN) (NJB) */

/* -& */
/* $ Index_Entries */

/*     d.p. number to character */

/* -& */
/* $ Revisions */

/* -     SPICELIB Version 1.1.0, 14-OCT-1992 (WLT) */

/*         A bug that caused this routine to have a floating point */
/*         overflow for values of X close to zero was corrected. In */
/*         addition the restriction on range of exponents supported */
/*         has been removed. */

/* -     Beta Version 1.1.0, 16-FEB-1989 (HAN) (NJB) */

/*         Header was changed to reflect the "error free" status */
/*         of the module, and a comment was added stating what the */
/*         routine does if SIGIDG is less than one. */

/*         Declaration of the unused variable FRAC removed. */

/* -& */

/*     Maximum number of allowed significant digits. */


/*     Local variables */


/*     Transfer X to the local variable COPY and leave X alone for the */
/*     rest of the routine. */

    copy = *x;

/*     Wipe out anything sitting in NUMSTR */

    s_copy(numstr, " ", (ftnlen)32, (ftnlen)1);

/*     At least 1 significant digit is required.  The most allowed is 14. */
/*     MAXSIG is the integer in this range that is closest to SIGDIG. */

/* Computing MIN */
    i__1 = 14, i__2 = max(1,*sigdig);
    maxsig = min(i__1,i__2);

/*     Examine COPY to see if its positive, zero, or negative. */
/*     This determines whether we need a minus sign and where the */
/*     decimal point needs to go in the output string. */

    if (copy < 0.) {
	*(unsigned char *)numstr = '-';
	copy = -copy;
	postn = 2;
	*(unsigned char *)&numstr[2] = '.';
    } else if (copy > 0.) {
	*(unsigned char *)numstr = ' ';
	postn = 2;
	*(unsigned char *)&numstr[2] = '.';
    } else {
	s_copy(zero, " 0.0000000000000000000000000", (ftnlen)28, (ftnlen)28);
/* Writing concatenation */
	i__3[0] = maxsig + 2, a__1[0] = zero;
	i__3[1] = 4, a__1[1] = "E+00";
	s_cat(numstr, a__1, i__3, &c__2, (ftnlen)32);
	s_copy(string, numstr, string_len, (ftnlen)32);
	return 0;
    }

/*     We need a first guess at the exponent string.  Compute the LOG */
/*     base 10 of COPY */

    exp10 = d_lg10(&copy);

/*     Scale our copy of the input into the range 1 to 10. */

    if (exp10 < 0.) {

/*        In this case the exponent will be negative.  We want the */
/*        largest integer exponent less than EXP10,  but the FORTRAN */
/*        INT function gives the INTEGER closest to EXP10 between EXP10 */
/*        and zero.  As a result we have to subtract 1 from INT(EXP10). */

	expont = (integer) exp10 - 1;
	k = -expont;
	while(k > 16) {
	    copy *= 1e16;
	    k += -16;
	}
	if (k != 0) {
	    copy *= power[(i__1 = k) < 18 && 0 <= i__1 ? i__1 : s_rnge("power"
		    , i__1, "dpstr_", (ftnlen)438)];
	}
    } else {
	expont = (integer) exp10;
	k = expont;
	while(k > 16) {
	    copy *= 1e-16;
	    k += -16;
	}
	if (k != 0) {
	    copy *= ipower[(i__1 = k) < 18 && 0 <= i__1 ? i__1 : s_rnge("ipo"
		    "wer", i__1, "dpstr_", (ftnlen)453)];
	}
    }

/*     Round off the last significant digit. */

    d__1 = copy * power[(i__1 = maxsig - 1) < 18 && 0 <= i__1 ? i__1 : s_rnge(
	    "power", i__1, "dpstr_", (ftnlen)464)];
    copy = (d_nint(&d__1) + .125) * ipower[(i__2 = maxsig - 1) < 18 && 0 <= 
	    i__2 ? i__2 : s_rnge("ipower", i__2, "dpstr_", (ftnlen)464)];

/*     We might have accidentally made copy as big as 10 by the */
/*     round off process.  If we did we need to divide by 10 and add 1 */
/*     to the exponent value.  (COPY must always remain between 0 and 10) */

    if (copy >= 10.) {
	copy *= .1;
	++expont;
    }

/*     Get the first digit of the decimal expansion of X. */

    i__ = (integer) copy;
    *(unsigned char *)&numstr[postn - 1] = *(unsigned char *)&digits[(i__1 = 
	    i__) < 10 && 0 <= i__1 ? i__1 : s_rnge("digits", i__1, "dpstr_", (
	    ftnlen)480)];
    copy = (copy - values[(i__1 = i__) < 10 && 0 <= i__1 ? i__1 : s_rnge(
	    "values", i__1, "dpstr_", (ftnlen)482)]) * 10.;

/*     Set the string pointer to the next position and compute the */
/*     position of the last significant digit */

    postn += 2;
    last = postn + maxsig - 1;

/*     Fetch digits until we fill in the last available slot for */
/*     significant digits. */

    while(postn < last) {
	i__ = (integer) copy;
	*(unsigned char *)&numstr[postn - 1] = *(unsigned char *)&digits[(
		i__1 = i__) < 10 && 0 <= i__1 ? i__1 : s_rnge("digits", i__1, 
		"dpstr_", (ftnlen)498)];
	copy = (copy - values[(i__1 = i__) < 10 && 0 <= i__1 ? i__1 : s_rnge(
		"values", i__1, "dpstr_", (ftnlen)499)]) * 10.;
	++postn;
    }

/*     Tack on the exponent to the output. Note that the rather odd */
/*     if, else if, else construction below is done to maintain backward */
/*     compatibility of the "look" of the output. */

/*     First get the exponent symbol and sign of the exponent. */

    if (expont >= 0) {
	s_copy(numstr + (postn - 1), "E+", 32 - (postn - 1), (ftnlen)2);
    } else {
	expont = -expont;
	s_copy(numstr + (postn - 1), "E-", 32 - (postn - 1), (ftnlen)2);
    }
    postn += 2;

/*     Now get the numeric representation. */

    if (expont <= 40) {
	s_copy(expc, vaxexp + (((i__1 = expont) < 41 && 0 <= i__1 ? i__1 : 
		s_rnge("vaxexp", i__1, "dpstr_", (ftnlen)528)) << 1), (ftnlen)
		20, (ftnlen)2);
    } else {
	intstr_(&expont, expc, (ftnlen)20);
    }
    s_copy(numstr + (postn - 1), expc, 32 - (postn - 1), (ftnlen)20);
    s_copy(string, numstr, string_len, (ftnlen)32);

/*     That's all folks. */

    return 0;
} /* dpstr_ */

