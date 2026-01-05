/* zzcorepc.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ZZCOREPC ( Correct epoch for aberration ) */
/* Subroutine */ int zzcorepc_(char *abcorr, doublereal *et, doublereal *lt, 
	doublereal *etcorr, ftnlen abcorr_len)
{
    extern /* Subroutine */ int zzprscor_(char *, logical *, ftnlen), chkin_(
	    char *, ftnlen);
    logical corblk[6];
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Compute an aberration corrected epoch, given an aberration */
/*     correction specification, an epoch, and a light time. */

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
/*     ET         I   Ephemeris time, seconds past J2000. */
/*     LT         I   Light time. */
/*     ETCORR     O   Light time-corrected epoch. */

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

/*                    Case and leading and trailing blanks are not */
/*                    significant in ABCORR. */


/*     ET             is an epoch, expressed as seconds past J2000 TDB. */

/*     LT             is a light time value, expressed as TDB seconds. */


/* $ Detailed_Output */


/*     ETCORR         is the input epoch ET, corrected for light time: */

/*                       If the specified aberration correction calls */
/*                       for some type of light time correction (normal, */
/*                       converged Newtonian, relativistic), LT will be */
/*                       added to or subtracted from ET.  If the */
/*                       correction is of the transmission type, then */

/*                          ETCORR = ET + LT */

/*                       If the correction is of the reception type, */
/*                       then */

/*                          ETCORR = ET - LT */

/*                       If no light time correction is specified, then */

/*                          ETCORR = ET */

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

/* $ Examples */

/*     See ZZDYNFRM. */

/* $ Restrictions */

/*     1) This is a SPICE private routine; the routine is subject */
/*        to change without notice.  User applications should not */
/*        call this routine. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 24-NOV-2004 (NJB) */

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


/*     Local parameters */


/*     Local variables */

    if (return_()) {
	return 0;
    }
    chkin_("ZZCOREPC", (ftnlen)8);

/*     Parse the aberration correction string.  Obtain a correction */
/*     attribute block. */

    zzprscor_(abcorr, corblk, abcorr_len);
    if (corblk[1]) {

/*        Light time corrections are used.  The output epoch */
/*        must be adjusted according to whether the correction */
/*        is for received or transmitted radiation. */

	if (corblk[4]) {

/*           This is the transmission case. */

	    *etcorr = *et + *lt;
	} else {

/*           This is the reception case. */

	    *etcorr = *et - *lt;
	}
    } else {

/*        Light time corrections are not used. */

	*etcorr = *et;
    }
    chkout_("ZZCOREPC", (ftnlen)8);
    return 0;
} /* zzcorepc_ */

