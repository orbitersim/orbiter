/* zzbodbry.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      ZZBODBRY ( Return barycenter code for a body ) */
integer zzbodbry_(integer *body)
{
    /* System generated locals */
    integer ret_val;

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Return the barycenter code associated with a body belonging to */
/*     a planetary system.  For other bodies, simply return the */
/*     input ID code. */

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

/*     NAIF_IDS */

/* $ Keywords */

/*     PRIVATE */
/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     BODY       I   ID code of body. */

/*     The function returns the NAIF integer ID code of the barycenter, */
/*     if any, associated with BODY. */

/* $ Detailed_Input */

/*      BODY        is the integer ID code of the body for which the */
/*                  barycenter ID code is requested. */

/* $ Detailed_Output */

/*     The function returns the NAIF integer ID code of the barycenter, */
/*     if any, associated with BODY.  If BODY is not the NAIF integer */
/*     ID code of a planet or satellite, the value BODY is returned. */

/*     Planetary barycenter codes are the integers 1, ..., 9. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1) If BODY is not the NAIF integer ID code of a planet or */
/*        satellite, the value BODY is returned.  This case is */
/*        not considered to be an error. */

/*     2) Codes of the form */

/*           PXNNN, where */

/*                  P   is    1, ...,  9, */
/*                  X   is    1, 2, 3, 4, 6, 7, 8, 9 */
/*              and NNN is  001, ... 999 */

/*        are mapped to the integer P.  These codes are not */
/*        considered to be erroneous, though they were not */
/*        part of the planned satellite numbering scheme at */
/*        the date this routine was released. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine enables the caller to determine to which */
/*     planetary system, if any, a planet or natural satellite belongs. */
/*     This capability is used by the SPICELIB PCK subsystem. */

/*     Planets have ID codes of the form P99, where P is 1, ..., 9. */

/*     Natural satellites have ID codes of the form */

/*        PNN, where */

/*               P  is  1, ..., 9 */
/*           and NN is 01, ... 98 */

/*     or */

/*        PXNNN, where */

/*               P   is    1, ...,  9, */
/*               X   is    0  or    5, */
/*           and NNN is  001, ... 999 */

/*     Codes with X = 5 are provisional. */

/* $ Examples */

/*     1) Find the planetary system (indicated by a barycenter ID */
/*        code) associated with the ID code 65001 (a provisional code */
/*        for a Saturnian satellite): */

/*            BODY = 65001 */
/*            BARY = ZZBODBRY ( BODY ) */

/*        BARY is assigned the value 6. */

/*     2) Find the planetary system associated with the ID code */
/*        60001 (an "extended" code for a Saturnian satellite): */

/*            BODY = 60001 */
/*            BARY = ZZBODBRY ( BODY ) */

/*        BARY is assigned the value 6. */

/*     3) Find the planetary system associated with the ID code */
/*        606 (Titan): */

/*            BODY = 606 */
/*            BARY = ZZBODBRY ( BODY ) */

/*        BARY is assigned the value 6. */

/*     4) Find the planetary system associated with the ID code */
/*        699 (Saturn): */

/*            BODY = 699 */
/*            BARY = ZZBODBRY ( BODY ) */

/*        BARY is assigned the value 6. */

/*     5) Find the planetary system associated with the ID code 6 */
/*        (Saturn system barycenter): */

/*            BODY = 6 */
/*            BARY = ZZBODBRY ( BODY ) */

/*        BARY is assigned the value 6. */

/*     6) Find the planetary system associated with the ID code */
/*        9511010 (asteroid Gaspra): */

/*            BODY = 9511010 */
/*            BARY = ZZBODBRY ( BODY ) */

/*        BARY is assigned the value 9511010. */

/* $ Restrictions */

/*     1) This routine should not be called from routines outside */
/*        of SPICELIB.  The interface and functionality may change */
/*        without notice. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 01-FEB-2004 (NJB) */

/* -& */
    if (*body >= 100 && *body <= 999) {

/*        BODY is a "traditional" NAIF planet or natural satellite */
/*        ID code. */

	ret_val = *body / 100;
    } else if (*body >= 10000 && *body <= 99999) {

/*        BODY is an "extended" NAIF natural satellite ID code. */

	ret_val = *body / 10000;
    } else {

/*        BODY is a barycenter code or is not associated with a */
/*        planetary system.  In either case, we simply return */
/*        the input value BODY. */

	ret_val = *body;
    }
    return ret_val;
} /* zzbodbry_ */

