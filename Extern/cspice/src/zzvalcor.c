/* zzvalcor.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ZZVALCOR ( Validate aberration correction ) */
/* Subroutine */ int zzvalcor_(char *abcorr, logical *attblk, ftnlen 
	abcorr_len)
{
    extern /* Subroutine */ int zzprscor_(char *, logical *, ftnlen), chkin_(
	    char *, ftnlen), errch_(char *, char *, ftnlen, ftnlen);
    extern logical failed_(void);
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Validate an aberration correction string suitable for use by */
/*     the SPK system; return attributes. */

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
/*                       'XCN' */
/*                       'XCN+S' */
/*                       'XLT' */
/*                       'XLT+S' */

/*                    Note that some values not supported by the */
/*                    SPICELIB SPK subsystem are supported by */
/*                    the underlying routine ZZPRSCOR: */

/*                       - The letter 'R' indicates relativistic */
/*                         corrections. */

/*                       - Stellar aberration-only corrections are */
/*                         indicated by the strings */

/*                            'S' */
/*                            'XS' */

/*                    This routine *does not* permit values that */
/*                    the SPK system doesn't handle. */

/*                    Case and embedded blanks are not significant in */
/*                    ABCORR. */

/*                    If ABCORR contains an unsupported value, this */
/*                    routine will signal an error. */

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

/*                       - Is the correction relativistic? (This */
/*                         value is always .FALSE. for aberration */
/*                         correction specifications allowed by */
/*                         this routine.) */

/*                    The structure of ATTBLK is defined in the */
/*                    include file */

/*                       zzabcorr.inc */

/*                    The size of ATTBLK and the offsets of the */
/*                    component flags are defined there. */

/* $ Parameters */

/*     See INCLUDE file zzabcorr.inc. */

/* $ Exceptions */

/*     1) If the input aberration correction choice is not allowed, */
/*        the error SPICE(INVALIDOPTION) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine is similar to ZZPRSCOR, but stellar aberration-only */
/*     and relativistic corrections specifications are not allowed */
/*     by this routine. The allowed values are precisely those allowed */
/*     by SPKEZR. */

/* $ Examples */

/*     See ZZGFOCIN. */

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

/* $ Version */

/* -    SPICELIB Version 1.0.1, 01-OCT-2021 (NJB) */

/*        Corrected typo in comments. */

/* -    SPICELIB Version 1.0.0, 11-APR-2008 (NJB) */

/* -& */

/*     SPICELIB functions */

    if (return_()) {
	return 0;
    }
    chkin_("ZZVALCOR", (ftnlen)8);

/*     Parse the aberration correction string and obtain */
/*     an attribute block. */

    zzprscor_(abcorr, attblk, abcorr_len);
    if (failed_()) {
	chkout_("ZZVALCOR", (ftnlen)8);
	return 0;
    }

/*     Check the attribute block. We don't allow relativistic */
/*     corrections. */

    if (attblk[5]) {
	setmsg_("Aberration correction specification # calls for relativisti"
		"c corrections, which are not supported.", (ftnlen)98);
	errch_("#", abcorr, (ftnlen)1, abcorr_len);
	sigerr_("SPICE(INVALIDOPTION)", (ftnlen)20);
	chkout_("ZZVALCOR", (ftnlen)8);
	return 0;
    }

/*     Stellar aberration corrections are allowed only if light */
/*     time corrections are specified as well. */

    if (attblk[2] && ! attblk[1]) {
	setmsg_("Aberration correction specification # calls for stellar abe"
		"rration correction without light time correction; this combi"
		"nation is not supported.", (ftnlen)143);
	errch_("#", abcorr, (ftnlen)1, abcorr_len);
	sigerr_("SPICE(INVALIDOPTION)", (ftnlen)20);
	chkout_("ZZVALCOR", (ftnlen)8);
	return 0;
    }
    chkout_("ZZVALCOR", (ftnlen)8);
    return 0;
} /* zzvalcor_ */

