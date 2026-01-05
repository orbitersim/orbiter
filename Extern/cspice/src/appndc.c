/* appndc.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure APPNDC ( Append an item to a character cell ) */
/* Subroutine */ int appndc_(char *item, char *cell, ftnlen item_len, ftnlen 
	cell_len)
{
    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern integer cardc_(char *, ftnlen);
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen);
    extern integer sizec_(char *, ftnlen);
    extern /* Subroutine */ int scardc_(integer *, char *, ftnlen);
    integer nwcard;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Append an item to a character cell. */

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
/*     ITEM       I   The item to append. */
/*     CELL      I-O  The cell to which ITEM will be appended. */

/* $ Detailed_Input */

/*     ITEM     is a character string which is to be appended to CELL. */

/*     CELL     is a character SPICE cell to which ITEM will be */
/*              appended. */

/* $ Detailed_Output */

/*     CELL     is the input cell with ITEM appended. ITEM is the last */
/*              member of CELL. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the input cell has invalid cardinality, an error is */
/*         signaled by a routine in the call tree of this routine. */

/*     2)  If the input cell has invalid size, an error is signaled by a */
/*         routine in the call tree of this routine. */

/*     3)  If the cell is not large enough to accommodate the addition */
/*         of a new element, the error SPICE(CELLTOOSMALL) is signaled. */

/*     4)  If the length of the item is longer than the length of the */
/*         cell, ITEM is truncated on the right. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     In the following example, the item 'PLUTO' is appended to */
/*     the character cell PLANETS. */

/*     Before appending 'PLUTO', the cell contains: */

/*     PLANETS (1) = 'MERCURY' */
/*     PLANETS (2) = 'VENUS' */
/*     PLANETS (3) = 'EARTH' */
/*     PLANTES (4) = 'MARS' */
/*     PLANETS (5) = 'JUPITER' */
/*     PLANETS (6) = 'SATURN' */
/*     PLANETS (7) = 'URANUS' */
/*     PLANETS (8) = 'NEPTUNE' */

/*     The call */

/*        CALL APPNDC ( 'PLUTO', PLANETS ) */

/*     appends the element 'PLUTO' at the location PLANETS (9), and the */
/*     cardinality is updated. */

/*     If the cell is not big enough to accommodate the addition of */
/*     the item, an error is signaled. In this case, the cell is not */
/*     altered. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     H.A. Neilan        (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 27-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/*        Improved the documentation of CELL in $Detailed_Input and */
/*        $Detailed_Output. Added entries #1 and #2 to $Exceptions. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (HAN) */

/* -& */
/* $ Index_Entries */

/*     append an item to a character cell */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("APPNDC", (ftnlen)6);
    }

/*     Check to see if the cell can accommodate the addition of a */
/*     new item. If there is room, append the item to the cell and */
/*     reset the cardinality. If the cell cannot accommodate the */
/*     addition of a new item, signal an error. */

    nwcard = cardc_(cell, cell_len) + 1;
    if (nwcard <= sizec_(cell, cell_len)) {
	s_copy(cell + (nwcard + 5) * cell_len, item, cell_len, item_len);
	scardc_(&nwcard, cell, cell_len);
    } else {
	setmsg_("The cell cannot accommodate the addition of the item *.", (
		ftnlen)55);
	errch_("*", item, (ftnlen)1, item_len);
	sigerr_("SPICE(CELLTOOSMALL)", (ftnlen)19);
    }
    chkout_("APPNDC", (ftnlen)6);
    return 0;
} /* appndc_ */

