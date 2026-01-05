/* sizec.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure SIZEC ( Size of a character cell ) */
integer sizec_(char *cell, ftnlen cell_len)
{
    /* System generated locals */
    integer ret_val;

    /* Local variables */
    integer card, size;
    extern /* Subroutine */ int chkin_(char *, ftnlen), dechar_(char *, 
	    integer *, ftnlen), sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Return the size (maximum cardinality) of a character cell. */

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

/*     CELLS */

/* $ Keywords */

/*     CELLS */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     CELL       I   Input cell. */

/*     The function returns the size of the input cell. */

/* $ Detailed_Input */

/*     CELL     is a cell. */

/* $ Detailed_Output */

/*     The function returns the size of (maximum number of elements in) */
/*     the input cell. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the input array has invalid cardinality, the error */
/*         SPICE(INVALIDCARDINALITY) is signaled. SIZEC returns */
/*         an unspecified value in this case. */

/*     2)  If the input array has invalid size, the error */
/*         SPICE(INVALIDSIZE) is signaled. SIZEC returns */
/*         an unspecified value in this case. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     The size (SIZE) functions are typically used in conjunction */
/*     with the cardinality functions to predict (and subsequently */
/*     avoid) overflows when manipulating cells. In the following */
/*     example, SIZEI is used to determine whether the integer cell */
/*     ORIGINAL can be safely copied into the integer cell SAVE before */
/*     actually attempting the operation. (If ORIGINAL contains more */
/*     elements than SAVE is capable of holding, then the operation */
/*     will fail.) */

/*           IF ( CARDI ( ORIGINAL ) .LE. SIZEI ( SAVE ) ) THEN */
/*              CALL COPYI ( ORIGINAL, SAVE, ERROR ) */

/*           ELSE */
/*            . */
/*            . */
/*           END DO */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     C.A. Curzon        (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     H.A. Neilan        (JPL) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.0, 12-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.1.0, 17-MAY-1994 (HAN) */

/*        If the value of the function RETURN is .TRUE. upon execution of */
/*        this module, this function is assigned a default value of */
/*        either 0, 0.0D0, .FALSE., or blank depending on the type of the */
/*        function. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*         Comment section for permuted index source lines was added */
/*         following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU) (NJB) */

/* -& */
/* $ Index_Entries */

/*     size of a character cell */

/* -& */
/* $ Revisions */

/* -    Beta Version 2.0.0, 13-MAR-1989 (NJB) */

/*        Check for valid input cell added. The input cell must */
/*        have valid size and cardinality values. */

/* -& */

/*     SPICELIB functions */


/*     Local variables */

    if (return_()) {
	ret_val = 0;
	return ret_val;
    } else {
	chkin_("SIZEC", (ftnlen)5);
    }

/*     Set return value, regardless of validity. */

    dechar_(cell + (cell_len << 2), &size, cell_len);
    ret_val = size;

/*     Squeal if something is awry. */

    dechar_(cell + cell_len * 5, &card, cell_len);
    if (size < 0) {
	setmsg_("Invalid cell size.  The size was #.", (ftnlen)35);
	errint_("#", &size, (ftnlen)1);
	sigerr_("SPICE(INVALIDSIZE)", (ftnlen)18);
	chkout_("SIZEC", (ftnlen)5);
	return ret_val;
    } else if (card < 0) {
	setmsg_("Invalid cell cardinality.  The cardinality was #.", (ftnlen)
		49);
	errint_("#", &card, (ftnlen)1);
	sigerr_("SPICE(INVALIDCARDINALITY)", (ftnlen)25);
	chkout_("SIZEC", (ftnlen)5);
	return ret_val;
    } else if (card > size) {
	setmsg_("Invalid cell cardinality; cardinality exceeds cell size.  T"
		"he cardinality was #.  The size was #.", (ftnlen)97);
	errint_("#", &card, (ftnlen)1);
	errint_("#", &size, (ftnlen)1);
	sigerr_("SPICE(INVALIDCARDINALITY)", (ftnlen)25);
	chkout_("SIZEC", (ftnlen)5);
	return ret_val;
    }
    chkout_("SIZEC", (ftnlen)5);
    return ret_val;
} /* sizec_ */

