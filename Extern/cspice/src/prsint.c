/* prsint.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure PRSINT   ( Parse integer with error checking ) */
/* Subroutine */ int prsint_(char *string, integer *intval, ftnlen string_len)
{
    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int chkin_(char *, ftnlen), sigerr_(char *, 
	    ftnlen), nparsi_(char *, integer *, char *, integer *, ftnlen, 
	    ftnlen), chkout_(char *, ftnlen);
    char errmsg[320];
    extern /* Subroutine */ int setmsg_(char *, ftnlen);
    integer ptr;

/* $ Abstract */

/*     Parse a string as an integer, encapsulating error handling. */

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

/*     INTEGER */
/*     PARSING */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     STRING     I   String representing a numeric value. */
/*     INTVAL     O   Integer value obtained by parsing STRING. */

/* $ Detailed_Input */

/*     STRING   is a string representing a numeric value. Commas and */
/*              spaces may be used in this string for ease of reading */
/*              and writing the number. They are treated as */
/*              insignificant but non-error-producing characters. */

/*              For exponential representation any of the characters */
/*              'E','D','e','d' may be used. */

/*              The following are legitimate numeric expressions */

/*                 +12.2 e-1 */
/*                 -3. 1415 9276 */
/*                 1e6 */
/*                 E8 */

/*              The program also recognizes the following  mnemonics */

/*                 'PI',  'pi',  'Pi',  'pI' */
/*                 '+PI', '+pi', '+Pi', '+pI' */
/*                 '-PI', '-pi', '-Pi', '-pI' */

/*              and returns the value ( + OR - ) 3 as appropriate. */

/* $ Detailed_Output */

/*     INTVAL   is the integer obtained by parsing STRING. If an error is */
/*              encountered, INTVAL is not changed from whatever the */
/*              input value was. If the input string has a fractional */
/*              part, the fractional part will be truncated. Thus */
/*              3.18 is interpreted as 3. -4.98 is interpreted as -4. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the input string cannot be parsed or if the string */
/*         represents a number that is outside the range of */
/*         representable integers, as defined by INTMIN and INTMAX, the */
/*         error SPICE(NOTANINTEGER) is signaled. The value of INTVAL is */
/*         not changed from whatever the input value was. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The purpose of this routine is to enable safe parsing of numeric */
/*     values into an INTEGER variable without the necessity of in-line */
/*     error checking. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Parse into an INTEGER variable a set of strings representing */
/*        numeric values. */


/*        Example code begins here. */


/*              PROGRAM PRSINT_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local parameters. */
/*        C */
/*              INTEGER               SETSIZ */
/*              PARAMETER           ( SETSIZ = 10 ) */

/*              INTEGER               STRLEN */
/*              PARAMETER           ( STRLEN = 11 ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              CHARACTER*(STRLEN)    STRVAL ( SETSIZ ) */

/*              INTEGER               I */
/*              INTEGER               INTVAL */

/*        C */
/*        C     Initialize the array of strings. */
/*        C */
/*              DATA                  STRVAL / '100,000,000', */
/*             .                               ' -2 690 192', */
/*             .                               '  +12.2 e-1', */
/*             .                               '-3. 141 592', */
/*             .                               '      1.2e8', */
/*             .                               '         E6', */
/*             .                               '         Pi', */
/*             .                               '        -PI', */
/*             .                               '-2147483648', */
/*             .                               ' 2147483647' / */

/*        C */
/*        C     Parse each string into an INTEGER variable. */
/*        C */
/*              WRITE(*,'(A)') '   STRVAL       INTVAL' */
/*              WRITE(*,'(A)') '-----------  ------------' */
/*              DO I = 1, SETSIZ */

/*                 CALL PRSINT ( STRVAL(I), INTVAL ) */

/*                 WRITE(*,'(A11,2X,I12)') STRVAL(I), INTVAL */

/*              END DO */


/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*          STRVAL        INTVAL */
/*        -----------  ------------ */
/*        100,000,000     100000000 */
/*         -2 690 192      -2690192 */
/*          +12.2 e-1             1 */
/*        -3. 141 592            -3 */
/*              1.2e8     120000000 */
/*                 E6       1000000 */
/*                 Pi             3 */
/*                -PI            -3 */
/*        -2147483648   -2147483648 */
/*         2147483647    2147483647 */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 04-JUL-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. Added complete */
/*        code example. */

/*        Updated the header to properly describe its input, output, */
/*        exceptions and particulars. */

/* -    SPICELIB Version 1.0.0, 22-JUL-1997 (NJB) */

/* -& */
/* $ Index_Entries */

/*     parse integer with encapsulated error handling */

/* -& */

/*     Local parameters */


/*     Local variables */


/*     Use discovery check-in. */

    nparsi_(string, intval, errmsg, &ptr, string_len, (ftnlen)320);
    if (s_cmp(errmsg, " ", (ftnlen)320, (ftnlen)1) != 0) {
	chkin_("PRSINT", (ftnlen)6);
	setmsg_(errmsg, (ftnlen)320);
	sigerr_("SPICE(NOTANINTEGER)", (ftnlen)19);
	chkout_("PRSINT", (ftnlen)6);
	return 0;
    }
    return 0;
} /* prsint_ */

