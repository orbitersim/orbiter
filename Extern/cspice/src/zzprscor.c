/* zzprscor.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__15 = 15;
static integer c__0 = 0;

/* $Procedure ZZPRSCOR ( Parse aberration correction ) */
/* Subroutine */ int zzprscor_(char *abcorr, logical *attblk, ftnlen 
	abcorr_len)
{
    /* Initialized data */

    static char corlst[5*15] = "CN   " "CN+S " "LT   " "LT+S " "NONE " "RL   "
	     "RL+S " "S    " "XCN  " "XCN+S" "XLT  " "XLT+S" "XRL  " "XRL+S" 
	    "XS   ";
    static logical geo[15] = { FALSE_,FALSE_,FALSE_,FALSE_,TRUE_,FALSE_,
	    FALSE_,FALSE_,FALSE_,FALSE_,FALSE_,FALSE_,FALSE_,FALSE_,FALSE_ };
    static logical lt[15] = { TRUE_,TRUE_,TRUE_,TRUE_,FALSE_,TRUE_,TRUE_,
	    FALSE_,TRUE_,TRUE_,TRUE_,TRUE_,TRUE_,TRUE_,FALSE_ };
    static logical stl[15] = { FALSE_,TRUE_,FALSE_,TRUE_,FALSE_,FALSE_,TRUE_,
	    TRUE_,FALSE_,TRUE_,FALSE_,TRUE_,FALSE_,TRUE_,TRUE_ };
    static logical conv[15] = { TRUE_,TRUE_,FALSE_,FALSE_,FALSE_,FALSE_,
	    FALSE_,FALSE_,TRUE_,TRUE_,FALSE_,FALSE_,FALSE_,FALSE_,FALSE_ };
    static logical xmit[15] = { FALSE_,FALSE_,FALSE_,FALSE_,FALSE_,FALSE_,
	    FALSE_,FALSE_,TRUE_,TRUE_,TRUE_,TRUE_,TRUE_,TRUE_,TRUE_ };
    static logical rel[15] = { FALSE_,FALSE_,FALSE_,FALSE_,FALSE_,TRUE_,TRUE_,
	    FALSE_,FALSE_,FALSE_,FALSE_,FALSE_,TRUE_,TRUE_,FALSE_ };
    static logical first = TRUE_;

    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen);
    extern integer bsrchc_(char *, integer *, char *, ftnlen, ftnlen);
    extern /* Subroutine */ int orderc_(char *, integer *, integer *, ftnlen),
	     reordc_(integer *, integer *, char *, ftnlen);
    integer ordvec[15];
    extern /* Subroutine */ int reordl_(integer *, integer *, logical *), 
	    sigerr_(char *, ftnlen), chkout_(char *, ftnlen), ljucrs_(integer 
	    *, char *, char *, ftnlen, ftnlen), setmsg_(char *, ftnlen);
    char tmpcor[5];
    extern logical return_(void);
    integer loc;

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Parse an aberration correction string; return attributes. */

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

/*     ABERRATION */
/*     PARSING */
/*     PRIVATE */
/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  ------------------------------------------------- */
/*     ABCORR     I   Aberration correction string. */
/*     ATTBLK     O   Aberration correction attribute block. */

/* $ Detailed_Input */

/*     ABCORR         is a string representing a aberration */
/*                    correction.  The supported values are: */

/*                       'CN' */
/*                       'CN+S' */
/*                       'LT' */
/*                       'LT+S' */
/*                       'NONE' */
/*                       'RL' */
/*                       'RL+S' */
/*                       'S' */
/*                       'XCN' */
/*                       'XCN+S' */
/*                       'XLT' */
/*                       'XLT+S' */
/*                       'XRL' */
/*                       'XRL+S' */
/*                       'XS' */

/*                    Note that some values not supported by the */
/*                    SPICELIB SPK subsystem are supported by */
/*                    this routine: */

/*                       - The letter 'R' indicates relativistic */
/*                         corrections. */

/*                       - Stellar aberration-only corrections are */
/*                         indicated by the strings */

/*                            'S' */
/*                            'XS' */

/*                    Case and embedded blanks are not significant in */
/*                    ABCORR. */

/* $ Detailed_Output */

/*     ATTBLK         is a block of logical flags indicating the */
/*                    attributes of the aberration correction */
/*                    specified by ABCORR.  The attributes are: */

/*                       - Is the correction "geometric"? */

/*                       - Is light time correction indicated? */

/*                       - Is stellar aberration correction indicated? */

/*                       - Is the light time correction of the */
/*                         "converged Newtonian" variety? */

/*                       - Is the correction for the transmission */
/*                         case? */

/*                       - Is the correction relativistic? */

/*                    The structure of ATTBLK is defined in the */
/*                    include file */

/*                       zzabcorr.inc */

/*                    The size of ATTBLK and the offsets of the */
/*                    component flags are defined there. */

/* $ Parameters */

/*     See INCLUDE file zzabcorr.inc. */

/* $ Exceptions */

/*     1) If the input aberration correction choice is not recognized, */
/*        the error SPICE(INVALIDOPTION) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Many SPICELIB routines have logic branches based on the */
/*     attributes of aberration corrections.  Much duplicated */
/*     parsing code can be avoided by using this routine. */

/*     In particular, the routine ZZCOREPC uses this routine */
/*     to combine an epoch and light time value to compute */
/*     a light-time-adjusted epoch. */

/* $ Examples */

/*     See ZZCOREPC. */

/* $ Restrictions */

/*     1) This is a SPICE private routine; the routine is subject */
/*        to change without notice.  User applications should not */
/*        call this routine. */

/*     2) This routine recognizes some aberration corrections not */
/*        handled by most SPICELIB routines.  Callers should do */
/*        their own checking to ensure the parsed correction is */
/*        acceptable. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */
/*     B.V. Semenov    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 21-SEP-2013 (BVS) */

/*        Efficiency updates: moved CHKIN inside the exception block; */
/*        replaced CMPRSS/UCASE with LJUCRS. */

/* -    SPICELIB Version 1.0.0, 13-DEC-2004 (NJB) */

/* -& */

/*     SPICELIB functions */

/* $ Abstract */

/*     Include file zzabcorr.inc */

/*     SPICE private file intended solely for the support of SPICE */
/*     routines.  Users should not include this file directly due */
/*     to the volatile nature of this file */

/*     The parameters below define the structure of an aberration */
/*     correction attribute block. */

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

/* $ Parameters */

/*     An aberration correction attribute block is an array of logical */
/*     flags indicating the attributes of the aberration correction */
/*     specified by an aberration correction string.  The attributes */
/*     are: */

/*        - Is the correction "geometric"? */

/*        - Is light time correction indicated? */

/*        - Is stellar aberration correction indicated? */

/*        - Is the light time correction of the "converged */
/*          Newtonian" variety? */

/*        - Is the correction for the transmission case? */

/*        - Is the correction relativistic? */

/*    The parameters defining the structure of the block are as */
/*    follows: */

/*       NABCOR    Number of aberration correction choices. */

/*       ABATSZ    Number of elements in the aberration correction */
/*                 block. */

/*       GEOIDX    Index in block of geometric correction flag. */

/*       LTIDX     Index of light time flag. */

/*       STLIDX    Index of stellar aberration flag. */

/*       CNVIDX    Index of converged Newtonian flag. */

/*       XMTIDX    Index of transmission flag. */

/*       RELIDX    Index of relativistic flag. */

/*    The following parameter is not required to define the block */
/*    structure, but it is convenient to include it here: */

/*       CORLEN    The maximum string length required by any aberration */
/*                 correction string */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 18-DEC-2004 (NJB) */

/* -& */
/*     Number of aberration correction choices: */


/*     Aberration correction attribute block size */
/*     (number of aberration correction attributes): */


/*     Indices of attributes within an aberration correction */
/*     attribute block: */


/*     Maximum length of an aberration correction string: */


/*     End of include file zzabcorr.inc */


/*     Local variables */


/*     Saved variables */


/*     Initial values */


/*     It is recommended that, for maintainability, the correction */
/*     strings be kept in increasing order in this list.  However, */
/*     this routine does not rely on the strings being ordered */
/*     in this data statement:  the strings and associated values */
/*     are ordered at run time. */

    if (return_()) {
	return 0;
    }
    if (first) {

/*        The first time this routine is called, we sort the */
/*        aberration correction strings and the associated flag */
/*        lists.  This ensures we have an ordered list suitable */
/*        for a binary search. */

/*        Find the sorted order of the aberration correction strings. */

	orderc_(corlst, &c__15, ordvec, (ftnlen)5);

/*        Put the aberration correction strings and the associated */
/*        arrays into increasing order. */

	reordc_(ordvec, &c__15, corlst, (ftnlen)5);
	reordl_(ordvec, &c__15, geo);
	reordl_(ordvec, &c__15, lt);
	reordl_(ordvec, &c__15, stl);
	reordl_(ordvec, &c__15, conv);
	reordl_(ordvec, &c__15, xmit);
	reordl_(ordvec, &c__15, rel);
	first = FALSE_;
    }

/*     Obtain a blank-free, upper-case copy of the aberration */
/*     correction string. */

    ljucrs_(&c__0, abcorr, tmpcor, abcorr_len, (ftnlen)5);

/*     Search the list for the aberration correction string. */

    loc = bsrchc_(tmpcor, &c__15, corlst, (ftnlen)5, (ftnlen)5);
    if (loc == 0) {
	chkin_("ZZPRSCOR", (ftnlen)8);
	setmsg_("Aberration correction specification # is not recognized.", (
		ftnlen)56);
	errch_("#", abcorr, (ftnlen)1, abcorr_len);
	sigerr_("SPICE(INVALIDOPTION)", (ftnlen)20);
	chkout_("ZZPRSCOR", (ftnlen)8);
	return 0;
    }

/*     Set the output flags. */

    attblk[0] = geo[(i__1 = loc - 1) < 15 && 0 <= i__1 ? i__1 : s_rnge("geo", 
	    i__1, "zzprscor_", (ftnlen)318)];
    attblk[1] = lt[(i__1 = loc - 1) < 15 && 0 <= i__1 ? i__1 : s_rnge("lt", 
	    i__1, "zzprscor_", (ftnlen)319)];
    attblk[2] = stl[(i__1 = loc - 1) < 15 && 0 <= i__1 ? i__1 : s_rnge("stl", 
	    i__1, "zzprscor_", (ftnlen)320)];
    attblk[3] = conv[(i__1 = loc - 1) < 15 && 0 <= i__1 ? i__1 : s_rnge("conv"
	    , i__1, "zzprscor_", (ftnlen)321)];
    attblk[4] = xmit[(i__1 = loc - 1) < 15 && 0 <= i__1 ? i__1 : s_rnge("xmit"
	    , i__1, "zzprscor_", (ftnlen)322)];
    attblk[5] = rel[(i__1 = loc - 1) < 15 && 0 <= i__1 ? i__1 : s_rnge("rel", 
	    i__1, "zzprscor_", (ftnlen)323)];
    return 0;
} /* zzprscor_ */

