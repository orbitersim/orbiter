/* insrtd.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure INSRTD ( Insert an item into a double precision set ) */
/* Subroutine */ int insrtd_(doublereal *item, doublereal *a)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    integer card, last, size, i__;
    extern integer cardd_(doublereal *);
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern integer sized_(doublereal *);
    logical in;
    extern /* Subroutine */ int scardd_(integer *, doublereal *);
    extern integer lstled_(doublereal *, integer *, doublereal *);
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Insert an item into a double precision set. */

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

/*     SETS */

/* $ Keywords */

/*     CELLS */
/*     SETS */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     ITEM       I   Item to be inserted. */
/*     A         I-O  Insertion set. */

/* $ Detailed_Input */

/*     ITEM     is an item which is to be inserted into the specified */
/*              set. ITEM may or may not already be an element of the */
/*              set. */

/*     A        is a SPICE set. */

/*              On input, A may or may not contain the input item as an */
/*              element. */

/* $ Detailed_Output */

/*     A        on output, contains the union of the input set and the */
/*              singleton set containing the input item, unless there was */
/*              not sufficient room in the set for the item to be */
/*              included, in which case the set is not changed and an */
/*              error is signaled. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the insertion of the element into the set causes an excess */
/*         of elements, the error SPICE(SETEXCESS) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Create an double precision set for ten elements, insert items */
/*        to it and then remove the even values. */


/*        Example code begins here. */


/*              PROGRAM INSRTD_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     SPICELIB functions. */
/*        C */
/*              INTEGER                 CARDD */

/*        C */
/*        C     Local constants. */
/*        C */
/*              INTEGER                 LBCELL */
/*              PARAMETER             ( LBCELL = -5 ) */

/*              INTEGER                 SETDIM */
/*              PARAMETER             ( SETDIM   = 10  ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              DOUBLE PRECISION        A      ( LBCELL:SETDIM ) */
/*              DOUBLE PRECISION        EVEN   ( SETDIM        ) */
/*              DOUBLE PRECISION        ITEMS  ( SETDIM        ) */

/*              INTEGER                 I */

/*        C */
/*        C     Create a list of items and even numbers. */
/*        C */
/*              DATA                    EVEN  / */
/*             .                      0.D0,  2.D0,  4.D0,  6.D0,  8.D0, */
/*             .                     10.D0, 12.D0, 14.D0, 16.D0, 18.D0  / */

/*              DATA                    ITEMS / */
/*             .                      0.D0,  1.D0,  1.D0,  2.D0,  3.D0, */
/*             .                      5.D0,  8.D0, 10.D0, 13.D0, 21.D0  / */

/*        C */
/*        C     Initialize the empty set. */
/*        C */
/*              CALL VALIDD ( SETDIM, 0, A ) */

/*        C */
/*        C     Insert the list of double precision numbers into the */
/*        C     set. If the item is an element of the set, the set is */
/*        C     not changed. */
/*        C */
/*              DO I = 1, SETDIM */

/*                 CALL INSRTD ( ITEMS(I), A ) */

/*              END DO */

/*        C */
/*        C     Output the original contents of set A. */
/*        C */
/*              WRITE(*,*) 'Items in original set A:' */
/*              WRITE(*,'(10F6.1)') ( A(I), I = 1, CARDD ( A ) ) */
/*              WRITE(*,*) ' ' */

/*        C */
/*        C     Remove the even values. If the item is not an element of */
/*        C     the set, the set is not changed. */
/*        C */
/*              DO I = 1, SETDIM */

/*                 CALL REMOVD ( EVEN(I), A ) */

/*              END DO */

/*        C */
/*        C     Output the contents of A. */
/*        C */
/*              WRITE(*,*) 'Odd numbers in set A:' */
/*              WRITE(*,'(10F6.1)') ( A(I), I = 1, CARDD ( A ) ) */
/*              WRITE(*,*) ' ' */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Items in original set A: */
/*           0.0   1.0   2.0   3.0   5.0   8.0  10.0  13.0  21.0 */

/*         Odd numbers in set A: */
/*           1.0   3.0   5.0  13.0  21.0 */


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

/* -    SPICELIB Version 2.1.0, 24-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */
/*        Added complete code example. */

/*        Removed unnecessary $Revisions section. */

/* -    SPICELIB Version 2.0.0, 01-NOV-2005 (NJB) */

/*        Code was modified slightly to keep logical structure parallel */
/*        to that of INSRTC. */

/*        Long error message was updated to include size of */
/*        set into which insertion was attempted. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU) (NJB) */

/* -& */
/* $ Index_Entries */

/*     insert an item into a d.p. set */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Set up the error processing. */

    if (return_()) {
	return 0;
    }
    chkin_("INSRTD", (ftnlen)6);

/*     What are the size and cardinality of the set? */

    size = sized_(a);
    card = cardd_(a);

/*     Find the last element of the set which would come before the */
/*     input item. This will be the item itself, if it is already an */
/*     element of the set. */

    last = lstled_(item, &card, &a[6]);

/*     Is the item already in the set? If not, it needs to be inserted. */

    if (last > 0) {
	in = a[last + 5] == *item;
    } else {
	in = FALSE_;
    }
    if (! in) {

/*        If there is room in the set for the new element, then move */
/*        the succeeding elements back to make room. And update the */
/*        cardinality for future reference. */

	if (card < size) {
	    i__1 = last + 1;
	    for (i__ = card; i__ >= i__1; --i__) {
		a[i__ + 6] = a[i__ + 5];
	    }
	    a[last + 6] = *item;
	    i__1 = card + 1;
	    scardd_(&i__1, a);
	} else {
	    setmsg_("An element could not be inserted into the set due to la"
		    "ck of space; set size is #.", (ftnlen)82);
	    errint_("#", &size, (ftnlen)1);
	    sigerr_("SPICE(SETEXCESS)", (ftnlen)16);
	}
    }
    chkout_("INSRTD", (ftnlen)6);
    return 0;
} /* insrtd_ */

