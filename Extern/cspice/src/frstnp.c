/* frstnp.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure FRSTNP ( First non-printable character ) */
integer frstnp_(char *string, ftnlen string_len)
{
    /* System generated locals */
    integer ret_val, i__1;

    /* Builtin functions */
    integer i_len(char *, ftnlen);

    /* Local variables */
    integer i__;

/* $ Abstract */

/*     Return the index of the first non-printable character in a */
/*     character string. ASCII characters 32-126 are considered */
/*     printable by this routine. (Blanks are considered printable.) */

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

/*     ASCII */
/*     CHARACTER */
/*     SEARCH */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     STRING     I   Input character string. */

/*     The function returns the index of first non-printable character in */
/*     STRING. */

/* $ Detailed_Input */

/*     STRING   is the input character string. */

/* $ Detailed_Output */

/*     The function returns the index of the first non-printable */
/*     character in the input string. Characters having integer codes */
/*     outside the range 32-126 are considered to be non-printable */
/*     characters. Blanks are considered to be printable characters. If */
/*     the input string contains no non-printable characters, FRSTNP is */
/*     zero. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine may be used to assist in validating strings that */
/*     are intended to be free of non-printable characters. */

/*     This routine treats blanks as printable characters. This choice */
/*     prevents embedded blanks from causing false positive results in */
/*     tests of strings for invalid characters. Note that the routine */
/*     FRSTPC and LASTPC treat blanks as non-printable. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Find the first non-printable character in a given string, */
/*        considering blanks as printable characters. */


/*        Example code begins here. */


/*              PROGRAM FRSTNP_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     SPICELIB functions */
/*        C */
/*              INTEGER               FRSTNP */

/*        C */
/*        C     Local variables */
/*        C */
/*              CHARACTER*10    S */

/*        C */
/*        C     Add some random characters to the string, some of them */
/*        C     non-printable. */
/*        C */
/*              S( 1: 1) = 'A' */
/*              S( 2: 2) = ' ' */
/*              S( 3: 3) = CHAR ( 3 ) */
/*              S( 4: 4) = 'A' */
/*              S( 5: 5) = 'B' */
/*              S( 6: 6) = 'C' */
/*              S( 7: 7) = CHAR ( 7 ) */
/*              S( 8: 8) = CHAR ( 8 ) */
/*              S( 9: 9) = CHAR ( 9 ) */
/*              S(10:10) = ' ' */

/*              WRITE (*,'(A,I2)') 'First non-printable character ' */
/*             .//                 'found at index', FRSTNP(S) */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        First non-printable character found at index 3 */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 07-APR-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Created */
/*        complete code example from existing fragment. */

/* -    SPICELIB Version 1.0.0, 16-JUN-1995 (NJB) (IMU) */

/* -& */
/* $ Index_Entries */

/*     first non-printable character */

/* -& */

/*     Local variables */


/*     Look for the first character outside the range [32,126], and */
/*     return its index. */

    i__1 = i_len(string, string_len);
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (*(unsigned char *)&string[i__ - 1] < 32 || *(unsigned char *)&
		string[i__ - 1] > 126) {
	    ret_val = i__;
	    return ret_val;
	}
    }

/*     Still here? All characters are printable. Return zero. */

    ret_val = 0;
    return ret_val;
} /* frstnp_ */

