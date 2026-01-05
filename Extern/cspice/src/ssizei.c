/* ssizei.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure SSIZEI ( Set the size of an integer cell ) */
/* Subroutine */ int ssizei_(integer *size, integer *cell)
{
    integer i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen), sigerr_(char *, 
	    ftnlen), chkout_(char *, ftnlen), setmsg_(char *, ftnlen), 
	    errint_(char *, integer *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Set the size (maximum cardinality) of an integer cell. */

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
/*     SIZE       I   Size (maximum cardinality) of the cell. */
/*     CELL       O   The cell. */

/* $ Detailed_Input */

/*     SIZE     is the size (maximum number of elements) of the cell. */

/* $ Detailed_Output */

/*     CELL     is a cell. */


/*                On output, the size of the cell is SIZE. The */
/*                cardinality of the cell is 0. The rest of the */
/*                control area is zeroed out. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The set cardinality (SCARDC, SCARDD, and SCARDI) and set size */
/*     (SSIZEC, SSIZED, and SSIZEI) routines are typically used to */
/*     initialize cells for subsequent use. Since all cell routines */
/*     expect to find the size and cardinality of a cell in place, */
/*     no cell can be used until both have been set. */

/* $ Examples */

/*     In the example below, the size and cardinality of the character */
/*     cell FRED are set in the main module of the program FLNSTN. */
/*     Both are subsequently retrieved, and the cardinality changed, */
/*     in one of its subroutines, WILMA. */

/*           PROGRAM FLNSTN */

/*           CHARACTER*30     FRED ( LBCELL:100 ) */
/*            . */
/*            . */
/*           CALL SSIZEC ( 100, FRED ) */
/*            . */
/*            . */
/*           CALL WILMA ( FRED ) */
/*            . */
/*            . */
/*           STOP */
/*           END */


/*           SUBROUTINE WILMA ( FRED ) */

/*           CHARACTER*(*)      FRED  ( LBCELL:* ) */
/*           INTEGER            SIZE */
/*           INTEGER            CARD */

/*           INTEGER            CARDC */
/*           INTEGER            SIZEC */
/*            . */
/*            . */
/*           SIZE = SIZEC ( FRED ) */
/*           CARD = CARDC ( FRED ) */
/*            . */
/*            . */
/*           CALL SCARDC ( MIN ( SIZE, CARD ), FRED ) */
/*           CALL EXCESS ( CARD-SIZE, 'cell' ) */
/*            . */
/*            . */
/*           RETURN */
/*           END */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     C.A. Curzon        (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 20-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU) (NJB) */

/* -& */
/* $ Index_Entries */

/*     set the size of an integer cell */

/* -& */
/* $ Revisions */

/* -    Beta Version 2.0.0, 13-MAR-1989 (NJB) */

/*        Check for invalid size value added. An error */
/*        is signaled if the value is out of range. The cardinality */
/*        is now automatically reset to 0. The rest of the control */
/*        area is now zeroed out. */

/*        The examples have been updated to illustrate set initialization */
/*        without the use of the EMPTYx routines, which have been */
/*        removed from the library. Errors in the examples have been */
/*        removed, also. */

/* -& */

/*     SPICELIB functions */


/*     Local variables */

    if (return_()) {
	return 0;
    } else {
	chkin_("SSIZEI", (ftnlen)6);
    }

/*     The size must be non-negative.  Other values will be snubbed. */

    if (*size < 0) {
	setmsg_("Attempt to set size of cell to invalid value.  The value wa"
		"s #.", (ftnlen)63);
	errint_("#", size, (ftnlen)1);
	sigerr_("SPICE(INVALIDSIZE)", (ftnlen)18);
	chkout_("SSIZEI", (ftnlen)6);
	return 0;
    }

/*     Not much to this. */

    cell[4] = *size;
    cell[5] = 0;
    for (i__ = -5; i__ <= -2; ++i__) {
	cell[i__ + 5] = 0;
    }
    chkout_("SSIZEI", (ftnlen)6);
    return 0;
} /* ssizei_ */

