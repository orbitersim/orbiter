/* appndi.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure APPNDI ( Append an item to an integer cell ) */
/* Subroutine */ int appndi_(integer *item, integer *cell)
{
    extern integer cardi_(integer *);
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern integer sizei_(integer *);
    extern /* Subroutine */ int scardi_(integer *, integer *);
    integer nwcard;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Append an item to an integer cell. */

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

/*     ITEM     is an integer value which is to be appended to CELL. */

/*     CELL     is an integer SPICE cell to which ITEM will be */
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

/*     3)  If the cell is not big enough to accommodate the addition */
/*         of a new element, the error SPICE(CELLTOOSMALL) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Create a cell for fifteen elements, add a first element to */
/*        it and then append several more integer numbers. Validate */
/*        the cell into a set and print the result. */


/*        Example code begins here. */


/*              PROGRAM APPNDI_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     SPICELIB functions. */
/*        C */
/*              INTEGER                 CARDI */

/*        C */
/*        C     Local constants. */
/*        C */
/*              INTEGER                 LBCELL */
/*              PARAMETER             ( LBCELL = -5 ) */

/*              INTEGER                 LISTSZ */
/*              PARAMETER             ( LISTSZ = 9  ) */

/*              INTEGER                 SIZE */
/*              PARAMETER             ( SIZE   = 15 ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              INTEGER                 A      ( LBCELL:SIZE ) */

/*              INTEGER                 I */
/*              INTEGER                 ITEMS  ( LISTSZ      ) */

/*        C */
/*        C     Set the list of integers to be appended to the cell. */
/*        C */
/*              DATA                    ITEMS /  3,  1,  1 , 2, 5, */
/*             .                                 8, 21, 13, 34    / */

/*        C */
/*        C     Initialize the cell. */
/*        C */
/*              CALL SSIZEI ( SIZE, A ) */

/*        C */
/*        C     Add a single item to the new cell. */
/*        C */
/*              CALL APPNDI ( 0, A ) */

/*        C */
/*        C     Now insert a list of items. */
/*        C */
/*              DO I= 1, LISTSZ */

/*                 CALL APPNDI ( ITEMS(I), A ) */

/*              END DO */

/*        C */
/*        C     Output the original contents of cell A. */
/*        C */
/*              WRITE(*,*) 'Items in original cell A:' */
/*              WRITE(*,'(15I6)') ( A(I), I = 1, CARDI ( A ) ) */

/*        C */
/*        C     Validate the set: remove duplicates and sort the */
/*        C     elements. */
/*        C */
/*              CALL VALIDI ( SIZE, CARDI( A ), A ) */

/*        C */
/*        C     Output the contents of the set A. */
/*        C */
/*              WRITE(*,*) 'Items in cell A after VALIDI (now a set):' */
/*              WRITE(*,'(15I6)') ( A(I), I = 1, CARDI ( A ) ) */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Items in original cell A: */
/*             0     3     1     1     2     5     8    21    13    34 */
/*         Items in cell A after VALIDI (now a set): */
/*             0     1     2     3     5     8    13    21    34 */


/*        Note that if the cell is not big enough to accommodate the */
/*        addition of an item, an error is signaled. In this case, the */
/*        cell is not altered. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     H.A. Neilan        (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 27-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Added complete */
/*        code example. */

/*        Improved the documentation of CELL in $Detailed_Input and */
/*        $Detailed_Output. Added entries #1 and #2 to $Exceptions. */

/* -    SPICELIB Version 1.0.2, 31-JUL-2002 (NJB) */

/*        Corrected miscellaneous typos in header and in the long */
/*        error message text. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (HAN) */

/* -& */
/* $ Index_Entries */

/*     append an item to an integer cell */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("APPNDI", (ftnlen)6);
    }

/*     Check to see if the cell can accommodate the addition of a */
/*     new item. If there is room, append the item to the cell and */
/*     reset the cardinality. If the cell cannot accommodate the */
/*     addition of a new item, signal an error. */

    nwcard = cardi_(cell) + 1;
    if (nwcard <= sizei_(cell)) {
	cell[nwcard + 5] = *item;
	scardi_(&nwcard, cell);
    } else {
	setmsg_("The cell cannot accommodate the addition of the element *. ",
		 (ftnlen)59);
	errint_("*", item, (ftnlen)1);
	sigerr_("SPICE(CELLTOOSMALL)", (ftnlen)19);
    }
    chkout_("APPNDI", (ftnlen)6);
    return 0;
} /* appndi_ */

