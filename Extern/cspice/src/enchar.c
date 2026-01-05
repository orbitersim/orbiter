/* enchar.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ENCHAR ( Encode a character string ) */
/* Subroutine */ int enchar_0_(int n__, integer *number, char *string, ftnlen 
	string_len)
{
    /* Builtin functions */
    integer i_len(char *, ftnlen);

    /* Local variables */
    integer base, i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern integer chbase_(void);
    integer remain;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    extern logical return_(void);
    integer num;

/* $ Abstract */

/*     Encode a nonnegative integer number into a character string */
/*     as the expansion of the number in base CHBASE (a function of */
/*     the size of the available character set). */

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
/*              base CHBASE representation. */

/*              Let L be the declared length of STRING, and let */
/*              NUMBER be given by */

/*                                  0           1                 L-1 */
/*                 NUMBER = a CHBASE  + a CHBASE  + ... + a CHBASE */
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

/*                                                          MINLEN */
/*     3)  If the number to be encoded is larger than CHBASE       - 1, */
/*         the error SPICE(OUTOFRANGE) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The value of CHBASE, which varies from machine to machine, is */
/*     returned by a constant function of the same name. */

/* $ Examples */

/*     See: SCARDC, SSIZEC. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.2, 31-JAN-2008 (BVS) */

/*        Changed header section title '$C Revision' to '$C Revisions'. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU) */

/* -& */
/* $ Index_Entries */

/*     encode a character_string */

/* -& */
/* $ Revisions */

/* -    Beta Version 2.0.0, 13-JAN-1989 (IMU) */

/*        Only the first MINLEN characters of the string are now */
/*        used to encode the value. Also, negative values are now */
/*        treated as errors. */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    switch(n__) {
	case 1: goto L_dechar;
	}

    if (return_()) {
	return 0;
    } else if (i_len(string, string_len) < 5) {
	chkin_("ENCHAR", (ftnlen)6);
	sigerr_("SPICE(INSUFFLEN)", (ftnlen)16);
	chkout_("ENCHAR", (ftnlen)6);
	return 0;
    } else if (*number < 0) {
	chkin_("ENCHAR", (ftnlen)6);
	sigerr_("SPICE(OUTOFRANGE)", (ftnlen)17);
	chkout_("ENCHAR", (ftnlen)6);
	return 0;
    }

/*     Generate the digits from right to left. */

    base = chbase_();
    num = *number;
    for (i__ = 5; i__ >= 1; --i__) {
	remain = num % base;
	*(unsigned char *)&string[i__ - 1] = (char) remain;
	num /= base;
    }

/*     More error handling. */

    if (num > 0) {
	chkin_("ENCHAR", (ftnlen)6);
	sigerr_("SPICE(OUTOFRANGE)", (ftnlen)17);
	chkout_("ENCHAR", (ftnlen)6);
    }
    return 0;
/* $Procedure DECHAR ( Decode a character string ) */

L_dechar:
/* $ Abstract */

/*     Decode a character string encoded by ENCHAR. */

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

/*     IMPLICIT NONE */

/*     CHARACTER*(*)      STRING */
/*     INTEGER            NUMBER */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     STRING     I   Encoded character string. */
/*     NUMBER     O   Decoded number. */

/* $ Detailed_Input */

/*     STRING   is a character string previously encoded by ENCHAR. */
/*              This contains an integer in base CHBASE notation, */
/*              where CHBASE is a function of the size of the */
/*              available character set. See ENCHAR for details */
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

/*     DECHAR is the inverse of ENCHAR. In the example below, */

/*           CALL ENCHAR (      I, STRING ) */
/*           CALL DECHAR ( STRING,      J ) */

/*           IF ( I .EQ. J ) THEN */
/*            . */
/*            . */
/*           END IF */

/*     the logical test (I .EQ. J) is always true. */

/* $ Examples */

/*     See: CARDC, SIZEC. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.2, 31-JAN-2008 (BVS) */

/*        Changed header section title '$C Revision' to '$C Revisions'. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU) */

/* -& */
/* $ Index_Entries */

/*     decode a character_string */

/* -& */
/* $ Revisions */

/* -    Beta Version 2.0.0, 13-JAN-1989 (IMU) */

/*        Changed to reflect changes in ENCHAR. In particular, */
/*        it now checks the length of the input string. It is */
/*        also an entry point of ENCHAR, to make sure they always */
/*        have the same value of MINLEN. (Also, if CHBASE is */
/*        changed, ENCHAR and DECHAR will always be recompiled */
/*        simultaneously.) */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else if (i_len(string, string_len) < 5) {
	chkin_("DECHAR", (ftnlen)6);
	sigerr_("SPICE(INSUFFLEN)", (ftnlen)16);
	chkout_("DECHAR", (ftnlen)6);
	return 0;
    }

/*     Sum the products of the 'digits' and the corresponding powers */
/*     of NDCHAR, just like any other base conversion. */

    base = chbase_();
    *number = 0;
    for (i__ = 1; i__ <= 5; ++i__) {
	*number = base * *number + *(unsigned char *)&string[i__ - 1];
    }
    return 0;
} /* enchar_ */

/* Subroutine */ int enchar_(integer *number, char *string, ftnlen string_len)
{
    return enchar_0_(0, number, string, string_len);
    }

/* Subroutine */ int dechar_(char *string, integer *number, ftnlen string_len)
{
    return enchar_0_(1, number, string, string_len);
    }

