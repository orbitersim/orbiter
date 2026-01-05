/* diffc.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure DIFFC ( Difference of two character sets ) */
/* Subroutine */ int diffc_(char *a, char *b, char *c__, ftnlen a_len, ftnlen 
	b_len, ftnlen c_len)
{
    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Builtin functions */
    integer i_len(char *, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    logical l_lt(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer over, acard;
    extern integer cardc_(char *, ftnlen);
    integer bcard, ccard;
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

/*     Take the difference of two character sets to form a third set. */

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
/*     C          O   Difference of A and B. */

/* $ Detailed_Input */

/*     A        is a set. */


/*     B        is a set, distinct from A. */

/* $ Detailed_Output */

/*     C        is a set, distinct from sets A and B, which */
/*              contains the difference of A and B (that is, */
/*              all of the elements which are in A, but NOT */
/*              in B). */

/*              If the size (maximum cardinality) of C is smaller */
/*              than the cardinality of the difference of A and B, */
/*              then only as many items as will fit in C are */
/*              included, and an error is returned. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the difference of the two sets causes an excess of */
/*         elements, the error SPICE(SETEXCESS) is signaled. */

/*     2)  If length of the elements of the output set is less than */
/*         the length of the elements of the FIRST input set, the */
/*         error SPICE(ELEMENTSTOOSHORT) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     The DIFFERENCE of two sets contains every element which is */
/*     in the first set, but NOT in the second. */

/*           {a,b}      difference  {c,d}     =  {a,b} */
/*           {a,b,c}                {b,c,d}      {a} */
/*           {a,b,c,d}              {}           {a,b,c,d} */
/*           {}                     {a,b,c,d}    {} */
/*           {}                     {}           {} */

/*     The following call */

/*           CALL DIFFC  ( PLANETS, ASTEROIDS, RESULT ) */

/*     places the difference of the character sets PLANETS and */
/*     ASTEROIDS into the character set RESULT. */

/*     The output set must be distinct from both of the input sets. */
/*     For example, the following calls are invalid. */

/*           CALL DIFFI  ( CURRENT,     NEW, CURRENT ) */
/*           CALL DIFFI  (     NEW, CURRENT, CURRENT ) */

/*     In each of the examples above, whether or not the subroutine */
/*     signals an error, the results will almost certainly be wrong. */
/*     Nearly the same effect can be achieved, however, by placing the */
/*     result into a temporary set, which is immediately copied back */
/*     into one of the input sets, as shown below. */

/*           CALL DIFFI  ( CURRENT, NEW,  TEMP ) */
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

/* -    SPICELIB Version 1.1.0, 20-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU) (NJB) */

/* -& */
/* $ Index_Entries */

/*     difference of two character sets */

/* -& */
/* $ Revisions */

/* -    Beta Version 2.0.0, 21-DEC-1988 (NJB) */

/*        Error signaled if output set elements are not long enough. */
/*        Length must be at least max of lengths of input elements. */
/*        Also, calling protocol for EXCESS has been changed. Call to */
/*        SETMSG removed. */

/*        Also, in the overflow case, the number of excess elements was */
/*        computed incorrectly; this has been fixed. The problem was */
/*        that OVER was incremented in all cases of the overflow IF */
/*        block, rather than only in the cases where the cardinality of */
/*        the output cell would have been incremented if there were */
/*        enough room. */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Set up the error processing. */

    if (return_()) {
	return 0;
    }
    chkin_("DIFFC", (ftnlen)5);

/*     Make sure output set elements are long enough. */

    if (i_len(c__, c_len) < i_len(a, a_len)) {
	setmsg_("Length of output cell is #.  Length required to contain res"
		"ult is #.", (ftnlen)68);
	i__1 = i_len(c__, c_len);
	errint_("#", &i__1, (ftnlen)1);
/* Computing MAX */
	i__2 = i_len(a, a_len), i__3 = i_len(b, b_len);
	i__1 = max(i__2,i__3);
	errint_("#", &i__1, (ftnlen)1);
	sigerr_("SPICE(ELEMENTSTOOSHORT)", (ftnlen)23);
	chkout_("DIFFC", (ftnlen)5);
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

/*     When the end of the first input set is reached, we're done. */

    while(apoint <= acard) {

/*        If there is still space in the output set, fill it */
/*        as necessary. */

	if (ccard < csize) {
	    if (bpoint > bcard) {
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
		++bpoint;
	    }

/*        Otherwise, stop filling the array, but continue to count the */
/*        number of elements in excess of the size of the output set. */

	} else {
	    if (bpoint > bcard) {
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
    chkout_("DIFFC", (ftnlen)5);
    return 0;
} /* diffc_ */

