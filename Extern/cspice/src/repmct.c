/* repmct.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure REPMCT  ( Replace marker with cardinal text ) */
/* Subroutine */ int repmct_(char *in, char *marker, integer *value, char *
	rtcase, char *out, ftnlen in_len, ftnlen marker_len, ftnlen 
	rtcase_len, ftnlen out_len)
{
    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer i_indx(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    char card[145];
    extern /* Subroutine */ int lcase_(char *, char *, ftnlen, ftnlen), 
	    chkin_(char *, ftnlen), ucase_(char *, char *, ftnlen, ftnlen), 
	    errch_(char *, char *, ftnlen, ftnlen), ljust_(char *, char *, 
	    ftnlen, ftnlen);
    integer mrknbf;
    extern integer lastnb_(char *, ftnlen);
    integer mrknbl;
    char tmpcas[1];
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    extern integer frstnb_(char *, ftnlen);
    integer mrkpsb;
    extern /* Subroutine */ int repsub_(char *, integer *, integer *, char *, 
	    char *, ftnlen, ftnlen, ftnlen);
    integer mrkpse;
    extern /* Subroutine */ int setmsg_(char *, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int inttxt_(integer *, char *, ftnlen);

/* $ Abstract */

/*     Replace a marker with the text representation of a */
/*     cardinal number. */

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
/*     VALUE      I   Replacement value. */
/*     RTCASE     I   Case of replacement text. */
/*     OUT        O   Output string. */
/*     MAXLCN     P   Maximum length of a cardinal number. */

/* $ Detailed_Input */

/*     IN       is an arbitrary character string. */

/*     MARKER   is an arbitrary character string. The first occurrence of */
/*              MARKER in the input string is to be replaced by the text */
/*              representation of the cardinal number VALUE. */

/*              Leading and trailing blanks in MARKER are NOT */
/*              significant. In particular, no substitution is performed */
/*              if MARKER is blank. */

/*     VALUE    is an arbitrary integer. */

/*     RTCASE   indicates the case of the replacement text. RTCASE may be */
/*              any of the following: */

/*                 RTCASE   Meaning        Example */
/*                 ------   -----------    ----------------------- */
/*                 U, u     Uppercase      ONE HUNDRED FIFTY-THREE */

/*                 L, l     Lowercase      one hundred fifty-three */

/*                 C, c     Capitalized    One hundred fifty-three */

/* $ Detailed_Output */

/*     OUT      is the string obtained by substituting the text */
/*              representation of the cardinal number VALUE for the first */
/*              occurrence of MARKER in the input string. */

/*              OUT and IN must be identical or disjoint. */

/* $ Parameters */

/*     MAXLCN   is the maximum expected length of any cardinal text. 145 */
/*              characters are sufficient to hold the text representing */
/*              any value in the range */

/*                ( -10**12, 10**12 ) */

/*              An example of a number whose text representation is of */
/*              maximum length is */

/*                 - 777 777 777 777 */

/* $ Exceptions */

/*     1)  If OUT does not have sufficient length to accommodate the */
/*         result of the substitution, the result will be truncated on */
/*         the right. */

/*     2)  If MARKER is blank, or if MARKER is not a substring of IN, */
/*         no substitution is performed. (OUT and IN are identical.) */

/*     3)  If the value of RTCASE is not recognized, the error */
/*         SPICE(INVALIDCASE) is signaled. OUT is not changed. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This is one of a family of related routines for inserting values */
/*     into strings. They are typically used to construct messages that */
/*     are partly fixed, and partly determined at run time. For example, */
/*     a message like */

/*        'Fifty-one pictures were found in directory [USER.DATA].' */

/*     might be constructed from the fixed string */

/*        '#1 pictures were found in directory #2.' */

/*     by the calls */

/*        CALL REPMCT ( STRING, '#1',  51,           'C', STRING ) */
/*        CALL REPMC  ( STRING, '#2', '[USER.DATA]',      STRING ) */

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

/*     1) The following example illustrate the use of REPMCT to */
/*        replace a marker within a string with the cardinal text */
/*        corresponding to an integer. */


/*        Example code begins here. */


/*              PROGRAM REPMCT_EX1 */
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
/*        C     1. Uppercase */
/*        C */
/*              MARKER = '#' */
/*              INSTR  = 'INVALID COMMAND. WORD # NOT RECOGNIZED.' */

/*              CALL REPMCT ( INSTR, MARKER, 5, 'U', OUTSTR ) */

/*              WRITE(*,*) 'Case 1: Replacement text in uppercase.' */
/*              WRITE(*,*) '   Input : ', INSTR */
/*              WRITE(*,*) '   Output: ', OUTSTR */
/*              WRITE(*,*) ' ' */

/*        C */
/*        C     2. Lowercase */
/*        C */
/*              MARKER = ' XX ' */
/*              INSTR  = 'Word XX of the XX sentence was ...' */

/*              CALL REPMCT ( INSTR, MARKER, 5, 'L', OUTSTR ) */

/*              WRITE(*,*) 'Case 2: Replacement text in lowercase.' */
/*              WRITE(*,*) '   Input : ', INSTR */
/*              WRITE(*,*) '   Output: ', OUTSTR */
/*              WRITE(*,*) ' ' */

/*        C */
/*        C     2. Capitalized */
/*        C */
/*              MARKER = ' XX ' */
/*              INSTR  = 'Name:  YY.  Rank:  XX.' */

/*              CALL REPMC  ( INSTR,  'YY',  'Moriarty', OUTSTR ) */
/*              CALL REPMCT ( OUTSTR, MARKER, 5,    'C', OUTSTR ) */

/*              WRITE(*,*) 'Case 3: Replacement text capitalized.' */
/*              WRITE(*,*) '   Input : ', INSTR */
/*              WRITE(*,*) '   Output: ', OUTSTR */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Case 1: Replacement text in uppercase. */
/*            Input : INVALID COMMAND. WORD # NOT RECOGNIZED. */
/*            Output: INVALID COMMAND. WORD FIVE NOT RECOGNIZED. */

/*         Case 2: Replacement text in lowercase. */
/*            Input : Word XX of the XX sentence was ... */
/*            Output: Word five of the XX sentence was ... */

/*         Case 3: Replacement text capitalized. */
/*            Input : Name:  YY.  Rank:  XX. */
/*            Output: Name:  Moriarty.  Rank:  Five. */


/* $ Restrictions */

/*     1)  VALUE must be in the range accepted by the SPICELIB routine */
/*         INTTXT. This range is currently */

/*            ( -10**12, 10**12 ) */

/*         Note that the endpoints of the interval are excluded. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.0, 06-JUL-2021 (JDR) */

/*        Changed input argument name CASE to RTCASE for consistency */
/*        with other routines. */

/*        Added IMPLICIT NONE statement. */

/*        Updated code to remove unnecessary lines of code in the */
/*        Standard SPICE error handling CHKIN statements. */

/*        Edited the header to comply with NAIF standard. Added complete */
/*        code example from existing fragments. */

/*        Added REPML to the list of available replace marker routines. */

/* -    SPICELIB Version 1.1.0, 21-SEP-2013 (BVS) */

/*        Minor efficiency update: the routine now looks up the first */
/*        and last non-blank characters only once. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 30-AUG-1990 (NJB) (IMU) */

/* -& */
/* $ Index_Entries */

/*     replace marker with cardinal text */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("REPMCT", (ftnlen)6);

/*     Bail out if RTCASE is not recognized. */

    ljust_(rtcase, tmpcas, (ftnlen)1, (ftnlen)1);
    ucase_(tmpcas, tmpcas, (ftnlen)1, (ftnlen)1);
    if (*(unsigned char *)tmpcas != 'U' && *(unsigned char *)tmpcas != 'L' && 
	    *(unsigned char *)tmpcas != 'C') {
	setmsg_("Case (#) must be U, L, or C.", (ftnlen)28);
	errch_("#", rtcase, (ftnlen)1, (ftnlen)1);
	sigerr_("SPICE(INVALIDCASE)", (ftnlen)18);
	chkout_("REPMCT", (ftnlen)6);
	return 0;
    }

/*     If MARKER is blank, no substitution is possible. */

    if (s_cmp(marker, " ", marker_len, (ftnlen)1) == 0) {
	s_copy(out, in, out_len, in_len);
	chkout_("REPMCT", (ftnlen)6);
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
	chkout_("REPMCT", (ftnlen)6);
	return 0;
    }
    mrkpse = mrkpsb + mrknbl - mrknbf;

/*     Okay, RTCASE is recognized and MARKER has been found. */
/*     Generate the cardinal text corresponding to VALUE. */

    inttxt_(value, card, (ftnlen)145);

/*     CARD is always returned in upper case; change to the specified */
/*     case, if required. */

    if (*(unsigned char *)tmpcas == 'L') {
	lcase_(card, card, (ftnlen)145, (ftnlen)145);
    } else if (*(unsigned char *)tmpcas == 'C') {
	lcase_(card + 1, card + 1, (ftnlen)144, (ftnlen)144);
    }

/*     Replace MARKER with CARD. */

    repsub_(in, &mrkpsb, &mrkpse, card, out, in_len, lastnb_(card, (ftnlen)
	    145), out_len);
    chkout_("REPMCT", (ftnlen)6);
    return 0;
} /* repmct_ */

