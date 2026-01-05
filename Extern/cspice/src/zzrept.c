/* zzrept.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      ZZREPT ( Private --- replace tokens ) */
logical zzrept_(char *sub, char *replac, logical *l2r, ftnlen sub_len, ftnlen 
	replac_len)
{
    /* System generated locals */
    logical ret_val;

    /* Local variables */
    logical ok;
    extern logical zzremt_(char *, ftnlen), zzsubt_(char *, char *, logical *,
	     ftnlen, ftnlen);

/* $ Abstract */

/*    SPICE Private routine intended solely for the support of SPICE */
/*    routines.  Users should not call this routine directly due */
/*    to the volatile nature of this routine. */

/*    Replace matching tokens and remove the character "*" */

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

/*     TIME --- PRIVATE */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     SUB        I   is the substring to perform replacements on */
/*     REPLAC     I   is the replacement string */
/*     L2R        I   use left to right scanning if L2R is TRUE. */

/*     The function returns TRUE if a replacement is performed */

/* $ Detailed_Input */

/*     SUB        is a substring of characters to located in the */
/*                current internal tokenized representation of a */
/*                time string that is maintained by ZZTIME. */

/*     REPLAC     is a string of characters that will replace 1 for 1 */
/*                the characters in SUB.  Note that character * is */
/*                a special character in this substitution as it */
/*                will be removed (via ZZREMT) after substitution. */

/*     L2R        is a logical flag.  If L2R is TRUE, the search */
/*                for a substring matching SUB will be performed */
/*                in left to right order. If L2R is FALSE the */
/*                search for substring matching SUB will be performed */
/*                from right to left. */

/* $ Detailed_Output */

/*     The function returns TRUE if a replacement is performed. Otherwise */
/*     it returns FALSE. */

/* $ Parameters */

/*     None. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Particulars */

/*     This routine is simply a macro for the combination of the */
/*     ZZTIME entry points ZZSUBT and ZZREMT */

/* $ Examples */

/*     See TPARTV. */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 8-APR-1996 (WLT) */


/* -& */
    ret_val = zzsubt_(sub, replac, l2r, sub_len, replac_len);
    ok = zzremt_("*", (ftnlen)1);
    return ret_val;
} /* zzrept_ */

