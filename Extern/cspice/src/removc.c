/* removc.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure REMOVC ( Remove an item from a character set ) */
/* Subroutine */ int removc_(char *item, char *a, ftnlen item_len, ftnlen 
	a_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer card, i__;
    extern integer cardc_(char *, ftnlen);
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    logical in;
    extern /* Subroutine */ int scardc_(integer *, char *, ftnlen);
    extern integer bsrchc_(char *, integer *, char *, ftnlen, ftnlen);
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    extern logical return_(void);
    integer loc;

/* $ Abstract */

/*     Remove an item from a character set. */

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
/*     ITEM       I   Item to be removed. */
/*     A         I-O  Removal set. */

/* $ Detailed_Input */

/*     ITEM     is an item which is to be removed from the specified set. */
/*              ITEM may or may not already be an element of the set. */
/*              Trailing blanks in ITEM are not significant. */

/*     A        is a SPICE set. */

/*              On input, A may or may not contain the input item as an */
/*              element. */

/* $ Detailed_Output */

/*     A        on output, contains the difference of the input set and */
/*              the input item. If the item is not an element of the set, */
/*              the set is not changed. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the input set A has invalid cardinality, an error is */
/*         signaled by a routine in the call tree of this routine. */

/*     2)  If the input set A has invalid size, an error is signaled by a */
/*         routine in the call tree of this routine. */

/*     3)  The data values in set A must be monotone strictly increasing. */
/*         This is not checked. If this condition is not met, the results */
/*         are unpredictable. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Create a set with all the original planets of the Solar */
/*        System and then remove Pluto from that set. */


/*        Example code begins here. */


/*              PROGRAM REMOVC_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     SPICELIB functions. */
/*        C */
/*              INTEGER                 CARDC */

/*        C */
/*        C     Local constants. */
/*        C */
/*              INTEGER                 LBCELL */
/*              PARAMETER             ( LBCELL = -5  ) */

/*              INTEGER                 PNAMSZ */
/*              PARAMETER             ( PNAMSZ   = 7 ) */

/*              INTEGER                 SETDIM */
/*              PARAMETER             ( SETDIM   = 9 ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              CHARACTER*(PNAMSZ)      LIST   ( SETDIM        ) */
/*              CHARACTER*(PNAMSZ)      PLNETS ( LBCELL:SETDIM ) */

/*              INTEGER                 I */

/*        C */
/*        C     Create the original planets list. */
/*        C */
/*              DATA                    LIST  / */
/*             .                'MERCURY', 'VENUS',   'EARTH', */
/*             .                'MARS',    'JUPITER', 'SATURN', */
/*             .                'URANUS',  'NEPTUNE', 'PLUTO'   / */

/*        C */
/*        C     Initialize the empty set. */
/*        C */
/*              CALL VALIDC ( SETDIM, 0, PLNETS ) */

/*        C */
/*        C     Insert the list of planets into the set. If the item is */
/*        C     an element of the set, the set is not changed. */
/*        C */
/*              DO I = 1, SETDIM */

/*                 CALL INSRTC ( LIST(I), PLNETS ) */

/*              END DO */

/*        C */
/*        C     Remove the Pluto from the set. If the Pluto is not an */
/*        C     element of the set, the set is not changed. */
/*        C */
/*              CALL REMOVC ( 'PLUTO', PLNETS ) */

/*        C */
/*        C     Output the contents of PLNETS. */
/*        C */
/*              WRITE(*,*) 'Planets of the Solar System:' */

/*              DO I = 1, CARDC ( PLNETS ) */

/*                 WRITE(*,*) '   ', PLNETS(I) */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Planets of the Solar System: */
/*            EARTH */
/*            JUPITER */
/*            MARS */
/*            MERCURY */
/*            NEPTUNE */
/*            SATURN */
/*            URANUS */
/*            VENUS */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     C.A. Curzon        (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 24-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Added complete */
/*        code example. Extended the $Exceptions section. */

/*        Updated description of argument ITEM to indicate that trailing */
/*        blanks are not significant */

/*        Removed unnecessary $Revisions section. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU) */

/* -& */
/* $ Index_Entries */

/*     remove an item from a character set */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard error handling: */

    if (return_()) {
	return 0;
    } else {
	chkin_("REMOVC", (ftnlen)6);
    }

/*     What is the cardinality of the set? */

    card = cardc_(a, a_len);

/*     Determine the location (if any) of the item within the set. */

    loc = bsrchc_(item, &card, a + a_len * 6, item_len, a_len);

/*     Is the item in the set? If so, it needs to be removed. */

    in = loc > 0;
    if (in) {

/*        Move succeeding elements forward to take up the slack left */
/*        by the departing element. And update the cardinality for */
/*        future reference. */

	i__1 = card - 1;
	for (i__ = loc; i__ <= i__1; ++i__) {
	    s_copy(a + (i__ + 5) * a_len, a + (i__ + 6) * a_len, a_len, a_len)
		    ;
	}
	i__1 = card - 1;
	scardc_(&i__1, a, a_len);
    }
    chkout_("REMOVC", (ftnlen)6);
    return 0;
} /* removc_ */

