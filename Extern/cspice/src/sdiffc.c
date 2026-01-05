/* sdiffc.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure SDIFFC ( Symmetric difference of two character sets ) */
/* Subroutine */ int sdiffc_(char *a, char *b, char *c__, ftnlen a_len, 
	ftnlen b_len, ftnlen c_len)
{
    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Builtin functions */
    integer i_len(char *, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    logical l_lt(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer over, acard, bcard;
    extern integer cardc_(char *, ftnlen);
    integer ccard;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern integer sizec_(char *, ftnlen);
    integer csize;
    extern /* Subroutine */ int scardc_(integer *, char *, ftnlen);
    integer apoint, bpoint;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), excess_(integer *, char *, ftnlen), setmsg_(char *, 
	    ftnlen), errint_(char *, integer *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Take the symmetric difference of two character sets to form */
/*     a third set. */

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
/*     A          I   First input set. */
/*     B          I   Second input set. */
/*     C          O   Symmetric difference of A and B. */

/* $ Detailed_Input */

/*     A        is a set. */


/*     B        is a set, distinct from A. */

/* $ Detailed_Output */

/*     C        is a set, distinct from sets A and B, which */
/*              contains the symmetric difference of A and B */
/*              (that is, all of the elements which are in A */
/*              OR in B, but NOT in both). */

/*              If the size (maximum cardinality) of C is smaller */
/*              than the cardinality of the symmetric difference of */
/*              A and B, then only as many items as will fit in C */
/*              are included, and an error is signaled. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the symmetric difference of the two sets causes an excess */
/*         of elements, the error SPICE(SETEXCESS) is signaled. */

/*     2)  If length of the elements of the output set is < the */
/*         maximum of the lengths of the elements of the input */
/*         sets, the error SPICE(ELEMENTSTOOSHORT) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     The SYMMETRIC DIFFERENCE of two sets contains every */
/*     element which is in the first set OR in the second set, */
/*     but NOT in both sets. */

/*           {a,b}      sym. difference {c,d}     =  {a,b,c,d} */
/*           {a,b,c}                    {b,c,d}      {a,d} */
/*           {a,b,c,d}                  {}           {a,b,c,d} */
/*           {}                         {a,b,c,d}    {a,b,c,d} */
/*           {}                         {}           {} */

/*     The following call */

/*           CALL SDIFFC  ( PLANETS, ASTEROIDS, RESULT ) */

/*     places the symmetric difference of the character sets PLANETS and */
/*     ASTEROIDS into the character set RESULT. */

/*     The output set must be distinct from both of the input sets. */
/*     For example, the following calls are invalid. */

/*           CALL SDIFFI ( CURRENT,     NEW, CURRENT ) */
/*           CALL SDIFFI (     NEW, CURRENT, CURRENT ) */

/*     In each of the examples above, whether or not the subroutine */
/*     signals an error, the results will almost certainly be wrong. */
/*     Nearly the same effect can be achieved, however, by placing the */
/*     result into a temporary set, which is immediately copied back */
/*     into one of the input sets, as shown below. */

/*           CALL SDIFFI ( CURRENT, NEW,  TEMP ) */
/*           CALL COPYI  ( TEMP,    NEW        ) */

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

/* -    SPICELIB Version 1.2.0, 20-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.1.0, 18-JUN-1999 (WLT) */

/*        Modified call to CHKOUT to be consistent with CHKIN. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU) (NJB) */

/* -& */
/* $ Index_Entries */

/*     symmetric difference of two character sets */

/* -& */
/* $ Revisions */

/* -    Beta Version 2.0.0, 05-JAN-1989 (NJB) */

/*        Error signaled if output set elements are not long enough. */
/*        Length must be at least max of lengths of input elements. */
/*        Also, calling protocol for EXCESS has been changed. */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Set up the error processing. */

    if (return_()) {
	return 0;
    }
    chkin_("SDIFFC", (ftnlen)6);

/*     Make sure output set elements are long enough. */

/* Computing MAX */
    i__1 = i_len(a, a_len), i__2 = i_len(b, b_len);
    if (i_len(c__, c_len) < max(i__1,i__2)) {
	setmsg_("Length of output cell is #.  Length required to contain res"
		"ult is #.", (ftnlen)68);
	i__1 = i_len(c__, c_len);
	errint_("#", &i__1, (ftnlen)1);
/* Computing MAX */
	i__2 = i_len(a, a_len), i__3 = i_len(b, b_len);
	i__1 = max(i__2,i__3);
	errint_("#", &i__1, (ftnlen)1);
	sigerr_("SPICE(ELEMENTSTOOSHORT)", (ftnlen)23);
	chkout_("SDIFFC", (ftnlen)6);
	return 0;
    }

/*     Find the cardinality of the input sets, and the allowed size */
/*     of the output set. */

    acard = cardc_(a, a_len);
    bcard = cardc_(b, b_len);
    csize = sizec_(c__, c_len);

/*     Begin with the input pointers at the first elements of the */
/*     input sets. The cardinality of the output set is zero. */
/*     And there is no overflow so far. */

    apoint = 1;
    bpoint = 1;
    ccard = 0;
    over = 0;

/*     When the end of both input sets are reached, we're done. */

    while(apoint <= acard || bpoint <= bcard) {

/*        If there is still space in the output set, fill it */
/*        as necessary. */

	if (ccard < csize) {
	    if (apoint > acard) {
		++ccard;
		s_copy(c__ + (ccard + 5) * c_len, b + (bpoint + 5) * b_len, 
			c_len, b_len);
		++bpoint;
	    } else if (bpoint > bcard) {
		++ccard;
		s_copy(c__ + (ccard + 5) * c_len, a + (apoint + 5) * a_len, 
			c_len, a_len);
		++apoint;
	    } else if (s_cmp(a + (apoint + 5) * a_len, b + (bpoint + 5) * 
		    b_len, a_len, b_len) == 0) {
		++apoint;
		++bpoint;
	    } else if (l_lt(a + (apoint + 5) * a_len, b + (bpoint + 5) * 
		    b_len, a_len, b_len)) {
		++ccard;
		s_copy(c__ + (ccard + 5) * c_len, a + (apoint + 5) * a_len, 
			c_len, a_len);
		++apoint;
	    } else if (l_lt(b + (bpoint + 5) * b_len, a + (apoint + 5) * 
		    a_len, b_len, a_len)) {
		++ccard;
		s_copy(c__ + (ccard + 5) * c_len, b + (bpoint + 5) * b_len, 
			c_len, b_len);
		++bpoint;
	    }

/*        Otherwise, stop following the array, but continue to count the */
/*        number of elements in excess of the size of the output set. */

	} else {
	    if (apoint > acard) {
		++over;
		++bpoint;
	    } else if (bpoint > bcard) {
		++over;
		++apoint;
	    } else if (s_cmp(a + (apoint + 5) * a_len, b + (bpoint + 5) * 
		    b_len, a_len, b_len) == 0) {
		++apoint;
		++bpoint;
	    } else if (l_lt(a + (apoint + 5) * a_len, b + (bpoint + 5) * 
		    b_len, a_len, b_len)) {
		++over;
		++apoint;
	    } else if (l_lt(b + (bpoint + 5) * b_len, a + (apoint + 5) * 
		    a_len, b_len, a_len)) {
		++over;
		++bpoint;
	    }
	}
    }

/*     Set the cardinality of the output set. */

    scardc_(&ccard, c__, c_len);

/*     Report any excess. */

    if (over > 0) {
	excess_(&over, "set", (ftnlen)3);
	sigerr_("SPICE(SETEXCESS)", (ftnlen)16);
    }
    chkout_("SDIFFC", (ftnlen)6);
    return 0;
} /* sdiffc_ */

