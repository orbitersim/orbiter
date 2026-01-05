/* bodfnd.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__0 = 0;

/* $Procedure BODFND ( Find values from the kernel pool ) */
logical bodfnd_(integer *body, char *item, ftnlen item_len)
{
    /* System generated locals */
    logical ret_val;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    char code[16];
    integer n;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    logical found;
    char dtype[1], varnam[32];
    extern /* Subroutine */ int chkout_(char *, ftnlen), dtpool_(char *, 
	    logical *, integer *, char *, ftnlen, ftnlen), suffix_(char *, 
	    integer *, char *, ftnlen, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int intstr_(integer *, char *, ftnlen);

/* $ Abstract */

/*     Determine whether values exist for some item for any body */
/*     in the kernel pool. */

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

/*     KERNEL */
/*     PCK */
/*     SPK */

/* $ Keywords */

/*     CONSTANTS */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     BODY       I   ID code of body. */
/*     ITEM       I   Item to find ('RADII', 'NUT_AMP_RA', etc.). */

/*     The function returns .TRUE. if ITEM is in the kernel pool, .FALSE. */
/*     otherwise. */

/* $ Detailed_Input */

/*     BODY     is the ID code of the body for which the item is */
/*              requested. */

/*     ITEM     is the item to be returned. Together, the body and item */
/*              name combine to form a variable name, e.g., */

/*                    'BODY599_RADII' */
/*                    'BODY4_POLE_RA' */

/* $ Detailed_Output */

/*     The function returns .TRUE. if ITEM is in the kernel pool, and */
/*     .FALSE. if it is not. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     BODVCD, which returns values from the kernel pool, causes an */
/*     error to be signaled whenever the specified item is not found. */
/*     In many cases, this is appropriate. However, sometimes the */
/*     program may attempt to recover, by providing default values, */
/*     prompting for replacements, and so on. */

/* $ Examples */

/*     In the following example, default values are substituted for */
/*     bodies for which axes are not found. */

/*        IF ( BODFND ( TARGET, 'RADII' ) ) THEN */
/*           CALL BODVCD ( TARGET, 'RADII', 3, N, RADII ) */
/*        ELSE */
/*           CALL VPACK ( 100.D0, 100.D0, 100.D0, RADII ) */
/*        END IF */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     H.A. Neilan        (JPL) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.3.0, 12-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Updated */
/*        input argument BODY detailed description. Added SPK and PCK to */
/*        the list of required readings. */

/* -    SPICELIB Version 1.2.1, 24-OCT-2005 (NJB) */

/*        Header update: calls to BODVAR in example code were replaced */
/*        with calls to BODVCD. The string 'AXES' and variable AXES */
/*        were replaced with the string 'RADII' and variable 'RADII' */
/*        throughout the header. */

/* -    SPICELIB Version 1.2.0, 15-MAR-2002 (NJB) */

/*        Bug fix: routine was updated to work with string-valued */
/*        kernel variables. */

/* -    SPICELIB Version 1.1.0, 17-MAY-1994 (HAN) */

/*        If the value of the function RETURN is .TRUE. upon execution of */
/*        this module, this function is assigned a default value of */
/*        either 0, 0.0D0, .FALSE., or blank depending on the type of */
/*        the function. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU) */

/* -& */
/* $ Index_Entries */

/*     find constants for a body in the kernel pool */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	ret_val = FALSE_;
	return ret_val;
    } else {
	chkin_("BODFND", (ftnlen)6);
    }

/*     Construct the variable name from BODY and ITEM. */

    s_copy(varnam, "BODY", (ftnlen)32, (ftnlen)4);
    intstr_(body, code, (ftnlen)16);
    suffix_(code, &c__0, varnam, (ftnlen)16, (ftnlen)32);
    suffix_("_", &c__0, varnam, (ftnlen)1, (ftnlen)32);
    suffix_(item, &c__0, varnam, item_len, (ftnlen)32);

/*     Search the kernel pool for the item. */

    dtpool_(varnam, &found, &n, dtype, (ftnlen)32, (ftnlen)1);

/*     Was anything there? */

    ret_val = found;
    chkout_("BODFND", (ftnlen)6);
    return ret_val;
} /* bodfnd_ */

