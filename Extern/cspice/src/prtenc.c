/* prtenc.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure PRTENC ( Encode a character string, portably ) */
/* Subroutine */ int prtenc_0_(int n__, integer *number, char *string, ftnlen 
	string_len)
{
    /* Builtin functions */
    integer i_len(char *, ftnlen);

    /* Local variables */
    integer base, i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer remain;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    integer num;

/* $ Abstract */

/*     Encode a nonnegative integer number into a character string, */
/*     portably, using 128 as the base for encoding. */

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

/*     CELLS */
/*     CHARACTER */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     NUMBER     I   Number to be encoded. */
/*     STRING     O   Encoded string. */
/*     MINLEN     P   Minimum length of string. */

/* $ Detailed_Input */

/*     NUMBER   is an arbitrary nonnegative integer. */

/* $ Detailed_Output */

/*     STRING   is the character string implied by the ASCII */
/*              interpretation of NUMBER when converted to its */
/*              base 128 representation. */

/*              Let L be the declared length of STRING, and let */
/*              NUMBER be given by */

/*                                  0           1                 L-1 */
/*                 NUMBER = a    128  + a    128  + ... + a    128 */
/*                           1           2                 L */

/*              Then */

/*                 STRING(i:i) = CHAR(a )   for i = 1, L */
/*                                     i */

/*              Note that, just as for any other "numbers", */
/*              the "digits" in STRING are arranged from right */
/*              to left in order of increasing significance. */
/*              The string is, in effect, "padded with nulls" */
/*              on the left. */

/* $ Parameters */

/*     MINLEN   is the minimum length of a string into which a */
/*              number may be encoded. In order to avoid padding */
/*              long strings with hundreds, possibly thousands */
/*              of null characters, only the first MINLEN characters */
/*              of the string are actually used. Note that this */
/*              also allows the encoded number to be preserved */
/*              during assignments, */

/*                 STR1 = STR2 */

/*              so long as both strings are of length MINLEN or */
/*              greater. */

/* $ Exceptions */

/*     1)  If the length of the output string is less than MINLEN, */
/*         the error SPICE(INSUFFLEN) is signaled. */

/*     2)  If the number to be encoded is negative, the error */
/*         SPICE(OUTOFRANGE) is signaled. */

/*                                                       MINLEN */
/*     3)  If the number to be encoded is larger than 128       - 1, */
/*         the error SPICE(OUTOFRANGE) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine is identical to ENCHAR, except that this routine */
/*     does not use the machine-dependent encoding base returned by */
/*     the SPICELIB routine CHBASE. Instead, the base 128 is used. */
/*     This base is expected to work on all systems supporting ASCII */
/*     encoding of characters. */

/* $ Examples */

/*     See: SCARDC, SSIZEC. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.0, 19-DEC-1995 (NJB) (WLT) */

/* -& */
/* $ Index_Entries */

/*     encode a character string, portably */

/* -& */

/*     Local parameters */


/*     Local variables */


/*     Standard SPICE error handling. */

    switch(n__) {
	case 1: goto L_prtdec;
	}

    if (i_len(string, string_len) < 5) {
	chkin_("PRTENC", (ftnlen)6);
	sigerr_("SPICE(INSUFFLEN)", (ftnlen)16);
	chkout_("PRTENC", (ftnlen)6);
	return 0;
    } else if (*number < 0) {
	chkin_("PRTENC", (ftnlen)6);
	sigerr_("SPICE(OUTOFRANGE)", (ftnlen)17);
	chkout_("PRTENC", (ftnlen)6);
	return 0;
    }

/*     Generate the digits from right to left. */

    base = 128;
    num = *number;
    for (i__ = 5; i__ >= 1; --i__) {
	remain = num % base;
	*(unsigned char *)&string[i__ - 1] = (char) remain;
	num /= base;
    }

/*     More error handling. */

    if (num > 0) {
	chkin_("PRTENC", (ftnlen)6);
	sigerr_("SPICE(OUTOFRANGE)", (ftnlen)17);
	chkout_("PRTENC", (ftnlen)6);
    }
    return 0;
/* $Procedure PRTDEC ( Decode a character string ) */

L_prtdec:
/* $ Abstract */

/*     Decode a character string encoded by PRTENC. */

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

/* $ Declarations */

/*     CHARACTER*(*)      STRING */
/*     INTEGER            NUMBER */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     STRING     I   Encoded character string. */
/*     NUMBER     O   Decoded number. */

/* $ Detailed_Input */

/*     STRING   is a character string previously encoded by PRTENC. */
/*              This contains an integer in base 128 notation, */
/*              where 128 is a function of the size of the */
/*              available character set. See PRTENC for details */
/*              about the format of STRING. */

/* $ Detailed_Output */

/*     NUMBER   is the integer encoded in the input string. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the length of the input string is less than MINLEN, */
/*         the error SPICE(INSUFFLEN) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     PRTDEC is the inverse of PRTENC. In the example below, */

/*           CALL PRTENC (      I, STRING ) */
/*           CALL PRTDEC ( STRING,      J ) */

/*           IF ( I .EQ. J ) THEN */
/*            . */
/*            . */
/*           END IF */

/*     the logical test (I .EQ. J) is always true. */

/*     This routine is identical to DECHAR, except that this routine */
/*     does not use the machine-dependent encoding base returned by */
/*     the SPICELIB routine CHBASE. Instead, the base 128 is used. */
/*     This base is expected to work on all systems supporting ASCII */
/*     encoding of characters. */

/* $ Examples */

/*     See: CARDC, SIZEC. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.0, 19-DEC-1995 (NJB) (WLT) */

/* -& */
/* $ Index_Entries */

/*     decode a portably encoded character string */

/* -& */
    if (i_len(string, string_len) < 5) {
	chkin_("PRTDEC", (ftnlen)6);
	sigerr_("SPICE(INSUFFLEN)", (ftnlen)16);
	chkout_("PRTDEC", (ftnlen)6);
	return 0;
    }

/*     Sum the products of the 'digits' and the corresponding powers */
/*     of NDCHAR, just like any other base conversion. */

    base = 128;
    *number = 0;
    for (i__ = 1; i__ <= 5; ++i__) {
	*number = base * *number + *(unsigned char *)&string[i__ - 1];
    }
    return 0;
} /* prtenc_ */

/* Subroutine */ int prtenc_(integer *number, char *string, ftnlen string_len)
{
    return prtenc_0_(0, number, string, string_len);
    }

/* Subroutine */ int prtdec_(char *string, integer *number, ftnlen string_len)
{
    return prtenc_0_(1, number, string, string_len);
    }

