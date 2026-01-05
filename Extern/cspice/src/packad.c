/* packad.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure PACKAD ( Pack a double precision array ) */
/* Subroutine */ int packad_(doublereal *in, integer *pack, integer *npack, 
	integer *maxout, integer *nout, doublereal *out)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    integer i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen), sigerr_(char *, 
	    ftnlen), chkout_(char *, ftnlen), setmsg_(char *, ftnlen), 
	    errint_(char *, integer *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Pack the contents of a double precision array. That is, */
/*     take a set of arbitrarily spaced elements from an input */
/*     array, and make them adjacent elements in an output array. */

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

/*     ARRAY */
/*     ASSIGNMENT */
/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     IN         I   Input array. */
/*     PACK       I   Indices of elements to be packed. */
/*     NPACK      I   Number of indices. */
/*     MAXOUT     I   Maximum number of elements in the output array. */
/*     NOUT       O   Number of elements in the output array. */
/*     OUT        O   Output array. */

/* $ Detailed_Input */

/*     IN       is the input array. */

/*     PACK     is the set of elements to be packed into the output */
/*              array. PACK(i) is the index of the element in the */
/*              input array that is to become the i'th element of the */
/*              output array. */

/*     NPACK    is the number of elements to be packed into the */
/*              output array. */

/*     MAXOUT   is the maximum number of elements to be packed into */
/*              the output array. If NPACK is larger than MAXOUT, the */
/*              extra items are ignored. */

/* $ Detailed_Output */

/*     NOUT     is the number of elements in the output array. */

/*     OUT      is the output array. This array contains up to */
/*              MAXOUT elements from the input array, located */
/*              in the first NOUT elements of the array. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If an element in the PACK array is less than 1, the error */
/*         SPICE(INVALIDINDEX) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The indicated elements are moved from their current locations */
/*     in the input array to consecutive positions in the output array. */

/*        OUT(   1) = IN(PACK(   1)) */
/*        OUT(   2) = IN(PACK(   2)) */
/*             . */
/*             . */
/*       OUT(NOUT) = IN(PACK(NOUT)) */

/*     NOUT is either NPACK or MAXOUT, whichever is smaller. */

/* $ Examples */

/*     The most common use for this routine is to remove unwanted items */
/*     from an array or set of arrays. For example, suppose that the */
/*     arrays NAME, CODE, RADIUS and MASS contain the names, NAIF */
/*     integer ID codes, radii, and masses of a set of NSAT satellites. */
/*     Suppose further that the user selects a subset of the original */
/*     set of satellites from a menu of some sort. Let the indices of */
/*     these satellites be the NSEL elements of the array SEL. The */
/*     following sequence would remove the names, codes, etc., of the */
/*     unselected satellites from the arrays. */

/*        CALL PACKAC ( NAME,   SEL, NSEL, NSAT, NOUT, NAME2   ) */
/*        CALL PACKAI ( CODE,   SEL, NSEL, NSAT, NOUT, CODE2   ) */
/*        CALL PACKAD ( RADIUS, SEL, NSEL, NSAT, NOUT, RADIUS2 ) */
/*        CALL PACKAD ( MASS,   SEL, NSEL, NSAT, NOUT, MASS2   ) */

/*     In the example above, suppose that NAME and PACK contain */
/*     the following: */

/*        NAME = 'MIMAS'          PACK = 2, 4, 6, 7 */
/*               'ENCELADUS' */
/*               'TETHYS' */
/*               'DIONE' */
/*               'RHEA' */
/*               'TITAN' */
/*               'HYPERION' */
/*               'IAPETUS' */
/*               'PHOEBE' */

/*     Then, following the call to PACKAC, NOUT and NAME2 contain */
/*     the following: */

/*        NOUT = 4                 NAME2 = 'ENCELADUS' */
/*                                         'DIONE' */
/*                                         'TITAN' */
/*                                         'HYPERION' */

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

/* -    SPICELIB Version 1.1.0, 20-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.2, 23-APR-2010 (NJB) */

/*        Header correction: assertions that the output */
/*        can overwrite the input have been removed. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU) (HAN) */

/* -& */
/* $ Index_Entries */

/*     pack a d.p. array */

/* -& */
/* $ Revisions */

/* -     Beta Version 2.0.0, 4-JAN-1989 (HAN) */

/*         Error handling was added to detect array indices that are */
/*         out of bound. If any element contained in the PACK array is */
/*         less than one, an error is signaled, and the output array is */
/*         not packed. */

/* -& */

/*     Spicelib functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("PACKAD", (ftnlen)6);
    }

/*     First, determine how many items to transfer. */

    *nout = min(*npack,*maxout);

/*     Check to see if PACK contains valid array indices. */

    i__1 = *nout;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (pack[i__ - 1] < 1) {
	    setmsg_("Element number * contains index *.", (ftnlen)34);
	    errint_("*", &i__, (ftnlen)1);
	    errint_("*", &pack[i__ - 1], (ftnlen)1);
	    sigerr_("SPICE(INVALIDINDEX)", (ftnlen)19);
	    chkout_("PACKAD", (ftnlen)6);
	    return 0;
	}
    }

/*     Transfer them. Just like it says in the header. */

    i__1 = *nout;
    for (i__ = 1; i__ <= i__1; ++i__) {
	out[i__ - 1] = in[pack[i__ - 1] - 1];
    }
    chkout_("PACKAD", (ftnlen)6);
    return 0;
} /* packad_ */

