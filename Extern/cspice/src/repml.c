/* repml.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__3 = 3;

/* $Procedure REPML ( Replace marker with logical value text ) */
/* Subroutine */ int repml_(char *in, char *marker, logical *value, char *
	rtcase, char *out, ftnlen in_len, ftnlen marker_len, ftnlen 
	rtcase_len, ftnlen out_len)
{
    /* Initialized data */

    static char casstr[1*3] = "U" "L" "C";
    static char valstr[5*3*2] = "TRUE " "true " "True " "FALSE" "false" "Fal"
	    "se";

    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer i_indx(char *, char *, ftnlen, ftnlen), s_rnge(char *, integer, 
	    char *, integer);

    /* Local variables */
    extern /* Subroutine */ int chkin_(char *, ftnlen), ucase_(char *, char *,
	     ftnlen, ftnlen), errch_(char *, char *, ftnlen, ftnlen);
    extern integer isrchc_(char *, integer *, char *, ftnlen, ftnlen);
    integer casidx, mrknbf;
    extern integer lastnb_(char *, ftnlen);
    integer mrknbl;
    char tmpcas[1], lvalue[5];
    integer validx;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    extern integer frstnb_(char *, ftnlen);
    integer mrkpsb, mrkpse;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), repsub_(char *, 
	    integer *, integer *, char *, char *, ftnlen, ftnlen, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Replace a marker with the text representation of a logical value. */

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
/*     VALUE      I   Replacement logical value. */
/*     RTCASE     I   Case of replacement text. */
/*     OUT        O   Output string. */

/* $ Detailed_Input */

/*     IN       is an arbitrary character string. */

/*     MARKER   is an arbitrary character string. The first occurrence */
/*              of MARKER in the input string is to be replaced by */
/*              VALUE. */

/*              MARKER is case-sensitive. */

/*              Leading and trailing blanks in MARKER are NOT */
/*              significant. In particular, no substitution is */
/*              performed if MARKER is blank. */

/*     VALUE    is an arbitrary logical value, either .TRUE. or */
/*              .FALSE. */

/*     RTCASE   indicates the case of the replacement text. RTCASE may */
/*              be any of the following: */

/*                 RTCASE    Meaning        Output values */
/*                 ------    -----------    --------------- */
/*                 U, u      Uppercase      'TRUE', 'FALSE' */

/*                 L, l      Lowercase      'true', 'false' */

/*                 C, c      Capitalized    'True', 'False' */

/* $ Detailed_Output */

/*     OUT      is the string obtained by substituting the text */
/*              representation of VALUE for the first occurrence of */
/*              MARKER in the input string. */

/*              OUT and IN must be disjoint. */

/* $ Parameters */

/*     None. */

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

/*        CALL REPMCT ( STRING, '#1',  51,           'C', TMPSTR ) */
/*        CALL REPMC  ( TMPSTR, '#2', '[USER.DATA]',      STRING ) */

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

/*     1) The following example illustrates the use of REPML to replace */
/*        a marker within a string with the text representation of a */
/*        logical value. */


/*        Example code begins here. */


/*              PROGRAM REPML_EX1 */
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
/*              INSTR  = 'Invalid value. The value was:  #.' */

/*              CALL REPML ( INSTR, MARKER, .FALSE. , 'U' , OUTSTR ) */

/*              WRITE(*,*) 'Case 1: Replacement text in uppercase.' */
/*              WRITE(*,*) '   Input : ', INSTR */
/*              WRITE(*,*) '   Output: ', OUTSTR */
/*              WRITE(*,*) ' ' */

/*        C */
/*        C     2. Lowercase */
/*        C */
/*              MARKER = ' XX ' */
/*              INSTR  = 'Invalid value. The value was:  XX.' */

/*              CALL REPML ( INSTR, MARKER, .TRUE. , 'l' , OUTSTR ) */

/*              WRITE(*,*) 'Case 2: Replacement text in lowercase.' */
/*              WRITE(*,*) '   Input : ', INSTR */
/*              WRITE(*,*) '   Output: ', OUTSTR */
/*              WRITE(*,*) ' ' */

/*        C */
/*        C     2. Capitalized */
/*        C */
/*              MARKER = '#' */
/*              INSTR  = 'Invalid value. The value was:  #.' */

/*              CALL REPML ( INSTR, MARKER, .FALSE. , 'c' , OUTSTR ) */

/*              WRITE(*,*) 'Case 3: Replacement text capitalized.' */
/*              WRITE(*,*) '   Input : ', INSTR */
/*              WRITE(*,*) '   Output: ', OUTSTR */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Case 1: Replacement text in uppercase. */
/*            Input : Invalid value. The value was:  #. */
/*            Output: Invalid value. The value was:  FALSE. */

/*         Case 2: Replacement text in lowercase. */
/*            Input : Invalid value. The value was:  XX. */
/*            Output: Invalid value. The value was:  true. */

/*         Case 3: Replacement text capitalized. */
/*            Input : Invalid value. The value was:  #. */
/*            Output: Invalid value. The value was:  False. */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 08-JAN-2021 (JDR) (NJB) */

/* -& */
/* $ Index_Entries */

/*     replace marker with logical value */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Saved variables */


/*     Initial values */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("REPML", (ftnlen)5);

/*     Identify the case string and find its index in the */
/*     array of uppercase case strings. Bail out if RTCASE is not */
/*     recognized. */

/*     RTCASE has length 1, so we need not be concerned with leading */
/*     blanks. */

    ucase_(rtcase, tmpcas, (ftnlen)1, (ftnlen)1);
    casidx = isrchc_(tmpcas, &c__3, casstr, (ftnlen)1, (ftnlen)1);
    if (casidx == 0) {
	setmsg_("Case (#) must be U, L, or C.", (ftnlen)28);
	errch_("#", rtcase, (ftnlen)1, (ftnlen)1);
	sigerr_("SPICE(INVALIDCASE)", (ftnlen)18);
	chkout_("REPML", (ftnlen)5);
	return 0;
    }

/*     If MARKER is blank, no substitution is possible. */

    if (s_cmp(marker, " ", marker_len, (ftnlen)1) == 0) {
	s_copy(out, in, out_len, in_len);
	chkout_("REPML", (ftnlen)5);
	return 0;
    }

/*     Locate the leftmost occurrence of MARKER, if there is one */
/*     (ignoring leading and trailing blanks). If MARKER is not */
/*     a substring of IN, no substitution can be performed. */

    mrknbf = frstnb_(marker, marker_len);
    mrknbl = lastnb_(marker, marker_len);

/*     MARKER is non-blank, so the index range below is valid. */

    mrkpsb = i_indx(in, marker + (mrknbf - 1), in_len, mrknbl - (mrknbf - 1));
    if (mrkpsb == 0) {
	s_copy(out, in, out_len, in_len);
	chkout_("REPML", (ftnlen)5);
	return 0;
    }
    mrkpse = mrkpsb + mrknbl - mrknbf;

/*     Okay, MARKER is non-blank and has been found. */

    if (*value) {
	validx = 1;
    } else {
	validx = 2;
    }

/*     Set the value string based on the case specification and */
/*     the input logical value. */

    s_copy(lvalue, valstr + ((i__1 = casidx + validx * 3 - 4) < 6 && 0 <= 
	    i__1 ? i__1 : s_rnge("valstr", i__1, "repml_", (ftnlen)396)) * 5, 
	    (ftnlen)5, (ftnlen)5);

/*     Replace MARKER with LVALUE. */

    repsub_(in, &mrkpsb, &mrkpse, lvalue, out, in_len, lastnb_(lvalue, (
	    ftnlen)5), out_len);
    chkout_("REPML", (ftnlen)5);
    return 0;
} /* repml_ */

