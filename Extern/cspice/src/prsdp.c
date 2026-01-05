/* prsdp.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure PRSDP   ( Parse d.p. number with error checking ) */
/* Subroutine */ int prsdp_(char *string, doublereal *dpval, ftnlen 
	string_len)
{
    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int chkin_(char *, ftnlen), nparsd_(char *, 
	    doublereal *, char *, integer *, ftnlen, ftnlen), sigerr_(char *, 
	    ftnlen), chkout_(char *, ftnlen);
    char errmsg[320];
    extern /* Subroutine */ int setmsg_(char *, ftnlen);
    integer ptr;

/* $ Abstract */

/*     Parse a string as a double precision number, encapsulating error */
/*     handling. */

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

/*     NUMBER */
/*     PARSING */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     STRING     I   String representing a numeric value. */
/*     DPVAL      O   D.p. value obtained by parsing STRING. */

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
/*                 1e12 */
/*                 E10 */

/*              The program also recognizes the following  mnemonics */

/*                 'PI',  'pi',  'Pi',  'pI' */
/*                 '+PI', '+pi', '+Pi', '+pI' */
/*                 '-PI', '-pi', '-Pi', '-pI' */

/*              and returns the value */

/*                 ( + OR - ) 3.1415 9265 3589 7932 3846 26 ... */

/*              as appropriate. */

/* $ Detailed_Output */

/*     DPVAL    is the double precision number obtained by parsing */
/*              STRING. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the input string cannot be parsed  due to use of an */
/*         unexpected or misplaced character or due to a string */
/*         representing a number too large for double precision, the */
/*         error SPICE(NOTADPNUMBER) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The purpose of this routine is to enable safe parsing of double */
/*     precision numbers without the necessity of in-line error checking. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Parse into a DOUBLE PRECISION variable a set of strings */
/*        representing numeric values. */


/*        Example code begins here. */


/*              PROGRAM PRSDP_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local parameters. */
/*        C */
/*              INTEGER               SETSIZ */
/*              PARAMETER           ( SETSIZ = 8  ) */

/*              INTEGER               STRLEN */
/*              PARAMETER           ( STRLEN = 11 ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              CHARACTER*(STRLEN)    STRVAL ( SETSIZ ) */

/*              DOUBLE PRECISION      DPVAL */

/*              INTEGER               I */

/*        C */
/*        C     Initialize the array of strings. */
/*        C */
/*              DATA                  STRVAL / '100,000,000', */
/*             .                               ' -2 690 192', */
/*             .                               '  +12.2 e-1', */
/*             .                               '-3. 141 592', */
/*             .                               '     1.2e12', */
/*             .                               '        E10', */
/*             .                               '         Pi', */
/*             .                               '        -PI' / */

/*        C */
/*        C     Parse each string into a DOUBLE PRECISION variable. */
/*        C */
/*              WRITE(*,'(A)') '   STRVAL               DPVAL' */
/*              WRITE(*,'(A)') '-----------  --------------------------' */
/*              DO I = 1, SETSIZ */

/*                 CALL PRSDP ( STRVAL(I), DPVAL ) */

/*                 WRITE(*,'(A11,F28.12)') STRVAL(I), DPVAL */

/*              END DO */


/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*           STRVAL               DPVAL */
/*        -----------  -------------------------- */
/*        100,000,000      100000000.000000000000 */
/*         -2 690 192       -2690192.000000000000 */
/*          +12.2 e-1              1.220000000000 */
/*        -3. 141 592             -3.141592000000 */
/*             1.2e12  1200000000000.000000000000 */
/*                E10    10000000000.000000000000 */
/*                 Pi              3.141592653590 */
/*                -PI             -3.141592653590 */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */

/* $ Version */

/* -    SPICELIB Version 1.1.1, 28-MAY-2020 (JDR) */

/*        Edited the header to comply with NAIF standard. Added complete */
/*        code example. */

/*        Updated the header to properly describe its input, output, */
/*        exceptions and particulars. */

/* -    SPICELIB Version 1.1.0, 15-SEP-1997 (NJB) */

/*        Bug fix: output argument declaration changed from INTEGER */
/*        to DOUBLE PRECISION. */

/* -    SPICELIB Version 1.0.0, 22-JUL-1997 (NJB) */

/* -& */
/* $ Index_Entries */

/*     parse d.p. number with encapsulated error handling */

/* -& */

/*     Local parameters */


/*     Local variables */


/*     Use discovery check-in. */

    nparsd_(string, dpval, errmsg, &ptr, string_len, (ftnlen)320);
    if (s_cmp(errmsg, " ", (ftnlen)320, (ftnlen)1) != 0) {
	chkin_("PRSDP", (ftnlen)5);
	setmsg_(errmsg, (ftnlen)320);
	sigerr_("SPICE(NOTADPNUMBER)", (ftnlen)19);
	chkout_("PRSDP", (ftnlen)5);
	return 0;
    }
    return 0;
} /* prsdp_ */

