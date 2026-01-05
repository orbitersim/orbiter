/* repmc.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure REPMC  ( Replace marker with character string ) */
/* Subroutine */ int repmc_(char *in, char *marker, char *value, char *out, 
	ftnlen in_len, ftnlen marker_len, ftnlen value_len, ftnlen out_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer i_indx(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int zzrepsub_(char *, integer *, integer *, char *
	    , char *, ftnlen, ftnlen, ftnlen);
    integer mrknbf;
    extern integer lastnb_(char *, ftnlen);
    integer mrknbl;
    extern integer frstnb_(char *, ftnlen);
    integer mrkpsb, mrkpse;

/* $ Abstract */

/*     Replace a marker with a character string. */

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
/*     STRING */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     IN         I   Input string. */
/*     MARKER     I   Marker to be replaced. */
/*     VALUE      I   Replacement string. */
/*     OUT        O   Output string. */

/* $ Detailed_Input */

/*     IN       is an arbitrary character string. */

/*     MARKER   is an arbitrary character string. The first occurrence of */
/*              MARKER in the input string is to be replaced by VALUE. */

/*              Leading and trailing blanks in MARKER are NOT */
/*              significant. In particular, no substitution is performed */
/*              if MARKER is blank. */

/*     VALUE    is the replacement character string. */

/*              Leading and trailing blanks in VALUE are NOT significant: */
/*              the portion of VALUE that is substituted for MARKER */
/*              extends from its first non-blank character to its last */
/*              non-blank character. */

/*              However, if VALUE is blank, a single blank is substituted */
/*              for the first occurrence of MARKER. */

/* $ Detailed_Output */

/*     OUT      is the string obtained by substituting VALUE (leading and */
/*              trailing blanks excepted) for the first occurrence of */
/*              MARKER in the input string. */

/*              OUT and IN must be identical or disjoint. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  If OUT does not have sufficient length to accommodate the */
/*         result of the substitution, the result will be truncated on */
/*         the right. */

/*     2)  If MARKER is blank, or if MARKER is not a substring of IN, */
/*         no substitution is performed. (OUT and IN are identical.) */

/*     3)  If VALUE is blank, a single blank is substituted for the */
/*         first occurrence of MARKER. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This is one of a family of related routines for inserting values */
/*     into strings. They are typically to construct messages that */
/*     are partly fixed, and partly determined at run time. For example, */
/*     a message like */

/*        'Fifty-one pictures were found in directory [USER.DATA].' */

/*     might be constructed from the fixed string */

/*        '#1 pictures were found in directory #2.' */

/*     by the calls */

/*        CALL REPMCT ( STRING, '#1', N_PICS,  'C', STRING ) */
/*        CALL REPMC  ( STRING, '#2', DIR_NAME,     STRING ) */

/*     which substitute the cardinal text 'Fifty-one' and the character */
/*     string '[USER.DATA]' for the markers '#1' and '#2' respectively. */

/*     The complete list of routines is shown below. */

/*        REPMC    ( Replace marker with character string value ) */
/*        REPMD    ( Replace marker with double precision value ) */
/*        REPMF    ( Replace marker with formatted d.p. value   ) */
/*        REPMI    ( Replace marker with integer value          ) */
/*        REPML    ( Replace marker with logical value          ) */
/*        REPMCT   ( Replace marker with cardinal text          ) */
/*        REPMOT   ( Replace marker with ordinal text           ) */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) The following example illustrate the use of REPMC to */
/*        replace a marker within a string with a character string */
/*        value. */


/*        Example code begins here. */


/*              PROGRAM REPMC_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local parameters. */
/*        C */
/*              INTEGER                 STRLEN */
/*              PARAMETER             ( STRLEN = 80 ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              CHARACTER*(STRLEN)      INSTR */
/*              CHARACTER*(STRLEN)      MARKER */
/*              CHARACTER*(STRLEN)      OUTSTR */

/*        C */
/*        C     1. Single marker */
/*        C */
/*              MARKER = '#' */
/*              INSTR  = 'Invalid value. The value was:  #' */

/*              CALL REPMC ( INSTR, MARKER, 'append', OUTSTR ) */

/*              WRITE(*,*) 'Case 1: Single marker.' */
/*              WRITE(*,*) '   Input : ', INSTR */
/*              WRITE(*,*) '   Output: ', OUTSTR */
/*              WRITE(*,*) */

/*        C */
/*        C     2. Multiple markers */
/*        C */
/*              MARKER = ' XX ' */
/*              INSTR  = 'The token XX was not recognized. Was it XX?' */

/*              CALL REPMC ( INSTR, MARKER, '  FND  ', OUTSTR ) */

/*              WRITE(*,*) 'Case 2: Multiple markers.' */
/*              WRITE(*,*) '   Input : ', INSTR */
/*              WRITE(*,*) '   Output: ', OUTSTR */
/*              WRITE(*,*) */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Case 1: Single marker. */
/*            Input : Invalid value. The value was:  # */
/*            Output: Invalid value. The value was:  append */

/*         Case 2: Multiple markers. */
/*            Input : The token XX was not recognized. Was it XX? */
/*            Output: The token FND was not recognized. Was it XX? */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.3.0, 21-AUG-2020 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Added complete */
/*        code example, based on existing fragments. */

/*        Added REPML to the list of available replace marker routines. */

/* -    SPICELIB Version 1.2.0, 21-SEP-2013 (BVS) */

/*        Minor efficiency update: the routine now looks up the first */
/*        and last non-blank characters only once. */

/* -    SPICELIB Version 1.1.0, 15-AUG-2002 (WLT) */

/*        The routine is now error free. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 30-AUG-1990 (NJB) (IMU) */

/* -& */
/* $ Index_Entries */

/*     replace marker with character_string */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     If MARKER is blank, no substitution is possible. */

    if (s_cmp(marker, " ", marker_len, (ftnlen)1) == 0) {
	s_copy(out, in, out_len, in_len);
	return 0;
    }

/*     Locate the leftmost occurrence of MARKER, if there is one */
/*     (ignoring leading and trailing blanks). If MARKER is not */
/*     a substring of IN, no substitution can be performed. */

    mrknbf = frstnb_(marker, marker_len);
    mrknbl = lastnb_(marker, marker_len);
    mrkpsb = i_indx(in, marker + (mrknbf - 1), in_len, mrknbl - (mrknbf - 1));
    if (mrkpsb == 0) {
	s_copy(out, in, out_len, in_len);
	return 0;
    }
    mrkpse = mrkpsb + mrknbl - mrknbf;

/*     Okay, MARKER is non-blank and has been found. If VALUE is */
/*     blank, substitute a single blank. (This removes the marker.) */
/*     Otherwise substitute the non-blank portion. */

    if (s_cmp(value, " ", value_len, (ftnlen)1) == 0) {
	zzrepsub_(in, &mrkpsb, &mrkpse, " ", out, in_len, (ftnlen)1, out_len);
    } else {
	i__1 = frstnb_(value, value_len) - 1;
	zzrepsub_(in, &mrkpsb, &mrkpse, value + i__1, out, in_len, lastnb_(
		value, value_len) - i__1, out_len);
    }
    return 0;
} /* repmc_ */

