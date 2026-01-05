/* zztknerr.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure  ZZTKNERR ( Create ZZTOKNS overflow error message ) */
/* Subroutine */ int zztknerr_(char *templt, char *string, char *token, char *
	error, logical *status, ftnlen templt_len, ftnlen string_len, ftnlen 
	token_len, ftnlen error_len)
{
    extern /* Subroutine */ int repmc_(char *, char *, char *, char *, ftnlen,
	     ftnlen, ftnlen, ftnlen);

/* $ Abstract */

/*     SPICE private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Create an error message for ZZTOKNS token buffer and picture */
/*     overflow exceptions. */

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

/*     TIME */

/* $ Keywords */

/*     TIME */
/*     PRIVATE */
/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     TEMPLT     I   Error message template */
/*     STRING     I   First string to be substituted */
/*     TOKEN      I   Second string to be substituted */
/*     ERROR      O   Complete error message */
/*     STATUS     O   Status flag */

/* $ Detailed_Input */

/*     TEMPLT         is the input error message template. It should */
/*                    have two '#' characters in it. */

/*     STRING         is the string that should replace the first '#' in */
/*                    the template. */

/*     TOKEN          is the string that should replace the second '#' */
/*                    in the template. */

/* $ Detailed_Output */

/*     ERROR          is the complete output error message. */

/*     STATUS         is the output status flag always set .FALSE. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The sole purpose of this routine is to encapsulate in a single */
/*     routine the two REPMC calls and one ZZTOKNS = .FALSE. assignments */
/*     needed to make the error messages and returned status used in */
/*     numerous overflow check IF blocks in ZZTOKNS. */

/* $ Examples */

/*     For these inputs */

/*        TEMPLT = 'This time string ''#'' has a bad token ''#''.' */

/*        STRING = '2001-01-01 boo 00:00:00' */

/*        TOKEN  = 'boo' */

/*     this routine returns */

/*        ERROR  =  'This time string ''2001-01-01 boo 00:00:00'' has ' */
/*       .          'a bad token ''boo''.' */

/*        STATUS = .FALSE. */

/* $ Restrictions */

/*     1) This is a private routine. SPICE user applications should not */
/*        call this routine. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     B.V. Semenov   (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 05-FEB-2014 (BVS) */

/* -& */
/* $ Index_Entries */

/*     create error message for ZZTOKNS overflow failure */

/* -& */
    repmc_(templt, "#", string, error, templt_len, (ftnlen)1, string_len, 
	    error_len);
    repmc_(error, "#", token, error, error_len, (ftnlen)1, token_len, 
	    error_len);
    *status = FALSE_;
    return 0;
} /* zztknerr_ */

