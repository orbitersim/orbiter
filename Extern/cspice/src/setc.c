/* setc.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure SETC ( Compare character sets ) */
logical setc_(char *a, char *op, char *b, ftnlen a_len, ftnlen op_len, ftnlen 
	b_len)
{
    /* System generated locals */
    logical ret_val;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer cond, carda, cardb;
    extern integer cardc_(char *, ftnlen);
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen);
    integer condab, condoa, condob, indexa, condeq, indexb, condgt, condlt;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Compare two character sets, as indicated by a relational operator. */

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
/*     SETS */

/* $ Keywords */

/*     CELLS */
/*     SETS */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     A          I   First set. */
/*     OP         I   Comparison operator. */
/*     B          I   Second set. */

/*     The function returns the result of the comparison: A (OP) B. */

/* $ Detailed_Input */

/*     A        is a set. */


/*     OP       is a comparison operator, indicating the way in */
/*              which the input sets are to be compared. OP may */
/*              be any of the following: */

/*                  Operator             Meaning */
/*                  --------  ------------------------------------- */
/*                    '='     A = B is .TRUE. if A and B are equal */
/*                            (contain the same elements). */

/*                    '<>'    A <> B is .TRUE. if A and B are not */
/*                            equal. */

/*                    '<='    A <= B is .TRUE. if A is a subset of B. */

/*                    '<'     A < B is .TRUE. if A is a proper subset */
/*                            of B. */

/*                    '>='    A >= B is .TRUE. if B is a subset of A. */

/*                    '>'     A > B is .TRUE. if B is a proper subset */
/*                            of A. */

/*                    '&'     A & B is .TRUE. if A and B have one or */
/*                            more elements in common. (The */
/*                            intersection of the two sets in */
/*                            non-empty.) */

/*                    '~'     A ~ B is .TRUE. if A and B are disjoint */
/*                            sets. */

/*     B        is a set. */

/* $ Detailed_Output */

/*     The function returns the result of the comparison: A (OP) B. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the set relational operator is not recognized, the error */
/*         SPICE(INVALIDOPERATION) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     1) In the following example, SETx is used to repeat an operation */
/*        for as long as the integer set FINISHED remains a proper */
/*        subset of the integer set PLANNED. */

/*           DO WHILE ( SETx ( FINISHED, '<', PLANNED ) ) */
/*              . */
/*              . */
/*           END DO */


/*     2) In the following example, let the integer sets A, B, and C */
/*        contain the elements listed below. Let E be an empty integer */
/*        set. */

/*             A        B        C */
/*           ---      ---      --- */
/*             1        1        1 */
/*             2        3        3 */
/*             3 */
/*             4 */

/*     Then all of the following expressions are true. */

/*           SETI ( B, '=',  C )      "B is equal to C" */
/*           SETI ( A, '<>', C )      "A is not equal to C" */
/*           SETI ( A, '>',  B )      "A is a proper superset of B" */
/*           SETI ( B, '<=', C )      "B is a subset of C" */
/*           SETI ( C, '<=', B )      "C is a subset of B" */
/*           SETI ( A, '<=', A )      "A is a subset of A" */
/*           SETI ( E, '<=', B )      "E is a subset of B" */
/*           SETI ( E, '<',  B )      "E is a proper subset of B" */
/*           SETI ( E, '<=', E )      "E is a subset of E" */
/*           SETI ( A, '&',  B )      "A has elements in common with B." */
/*           SETI ( B, '&',  C )      "B has elements in common with C." */

/*     And all of the following are false. */

/*           SETI ( B, '<>',  C )      "B is not equal to C" */
/*           SETI ( A, '=',   C )      "A is equal to C" */
/*           SETI ( A, '<',   B )      "A is a proper subset of B" */
/*           SETI ( B, '<',   C )      "B is a proper subset of C" */
/*           SETI ( B, '>=',  A )      "B is a superset of A" */
/*           SETI ( A, '>',   A )      "A is a proper superset of A" */
/*           SETI ( E, '>=',  A )      "E is a superset of A" */
/*           SETI ( E, '<',   E )      "E is a proper subset of E" */
/*           SETI ( A, '~',   B )      "A and B are disjoint sets." */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     H.A. Neilan        (JPL) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.0, 20-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.1.0, 17-MAY-1994 (HAN) */

/*        Set the default function value to either 0, 0.0D0, .FALSE., */
/*        or blank depending on the type of the function. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*         Comment section for permuted index source lines was added */
/*         following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU) (HAN) */

/* -& */
/* $ Index_Entries */

/*     compare character sets */

/* -& */
/* $ Revisions */

/* -    Beta Version 2.0.0, 11-JAN-1989 (WLT) (HAN) */

/*       The old version was not compatible with the error handling */
/*       mechanism. Taking the difference of sets A and B caused an */
/*       overflow of the set DIFF, whose dimension was one. The method of */
/*       determining the function value has been redesigned, and the */
/*       difference of the sets is no longer computed. */

/*       The new routine recognizes two new operators, '~' and '&'. */
/*       If the operator is not recognized, an error is now signaled. */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	ret_val = FALSE_;
	return ret_val;
    } else {
	chkin_("SETC", (ftnlen)4);
	ret_val = FALSE_;
    }

/*     Obtain the cardinality of the sets. */

    carda = cardc_(a, a_len);
    cardb = cardc_(b, b_len);

/*     The easiest way to compare two sets is to list them side by side */
/*     as shown below: */

/*     Set A  Set B */
/*     -----  ----- */
/*       1      1 */
/*              2 */
/*       3      3 */
/*       4      4 */
/*       5 */
/*              6 */
/*       7      7 */

/*     When listed this way, one can easily determine intersections, */
/*     differences, and unions.  Moreover, to determine if one set */
/*     is a subset of another, if they are equal, etc, one can just */
/*     inspect the two lists. */

/*     We can mimic this in an algorithm.  The main trick is to figure */
/*     out how to list the sets in this way.  Once we know how to */
/*     list them, we can simply adapt the listing algorithm to get */
/*     a comparison algorithm. */

/*     By the time we get this far, we know that our sets have distinct */
/*     elements and they are ordered. To write out the list above, */
/*     we start at the beginning of both sets (they're ordered, */
/*     remember?).  Look at the next element of A and the next element */
/*     of B ( to start out ``next'' means ``first'' ).  If the item */
/*     from A is smaller it should be written and space should be left */
/*     in the B column. If they are the same write them both.  Otherwise, */
/*     the item from B is smaller, so  leave space in the A column and */
/*     write the item from B.  Continue until you run out of items in */
/*     one of the sets.  Then just write down all those remaining in the */
/*     other set in the appropriate column.  This is what the loop */
/*     below does. */


/*           NEXTA = 1 */
/*           NEXTB = 1 */

/*           DO WHILE (       ( NEXTA .LT. CARD(A) ) */
/*          .           .AND. ( NEXTB .LT. CARD(B) ) ) */

/*              IF ( A(NEXTA) .LT. B(NEXTB) ) THEN */

/*                 WRITE (UNIT,*) A(NEXTA),   SPACES */
/*                 NEXTA = NEXTA + 1 */

/*              ELSE IF ( A(NEXTA) .EQ. B(NEXTB) ) THEN */

/*                 WRITE (UNIT,*) A(NEXTA),   B(NEXTB) */
/*                 NEXTA = NEXTA + 1 */
/*                 NEXTB = NEXTB + 1 */

/*              ELSE */

/*                 WRITE (UNIT,*) SPACES, B(NEXTB) */
/*                 NEXTB = NEXTB + 1 */

/*              END IF */
/*           END DO */

/*           DO NEXTA = 1, CARD(A) */
/*              WRITE (UNIT,*) A(NEXTA),SPACES */
/*           END DO */

/*           DO NEXTB = 1, CARD(B) */
/*              WRITE (UNIT,*) B(NEXTB),SPACES */
/*           END DO */


/*     This also gives us a way to compare the elements of the two */
/*     sets one item at a time.  Instead of writing the items, we */
/*     can make a decision as to whether or not the sets have the */
/*     relationship we are interested in. */

/*     At the beginning of the loop we assume that the two sets are */
/*     related in the way we want.  Once the comparison has been made */
/*     we can decide if they are still related in that way.  If not, */
/*     we can RETURN .FALSE.  Using psuedo-code the loop is modified */
/*     as shown below. */

/*           NEXTA = 1 */
/*           NEXTB = 1 */

/*           DO WHILE (       ( NEXTA .LT. CARD(A) ) */
/*          .           .AND. ( NEXTB .LT. CARD(B) ) ) */

/*              IF ( A(NEXTA) .LT. B(NEXTB) ) THEN */

/*                 RELATED = RELATIONSHIP_OF_INTEREST(A<B) */
/*                 NEXTA   = NEXTA + 1 */

/*              ELSE IF ( A(NEXTA) .EQ. B(NEXTB) ) THEN */

/*                 RELATED = RELATIONSHIP_OF_INTEREST(A=B) */
/*                 NEXTA   = NEXTA + 1 */
/*                 NEXTB   = NEXTB + 1 */

/*              ELSE */

/*                 RELATED = RELATIONSHIP_OF_INTEREST(A>B) */
/*                 NEXTB   = NEXTB + 1 */

/*              END IF */

/*              IF ( SURE_NOW(RELATED) ) THEN */
/*                 RETURN with the correct value. */
/*              ELSE */
/*                 Keep going. */
/*              END IF */

/*           END DO */


/*     Using the cardinality of the two sets, some function */
/*     values can be determined right away. If the cardinality */
/*     is not enough, we need to set up some conditions for the */
/*     loop which compares the individual elements of the sets. */


/*     A cannot be a proper subset of B if the cardinality of A is */
/*     greater than or equal to the cardinality of B. */

    if (s_cmp(op, "<", op_len, (ftnlen)1) == 0) {
	if (carda >= cardb) {
	    ret_val = FALSE_;
	    chkout_("SETC", (ftnlen)4);
	    return ret_val;
	} else {
	    condlt = 0;
	    condeq = 1;
	    condgt = 1;
	    condoa = 0;
	    condob = 1;
	    condab = 1;
	}

/*     A cannot be a subset of B if A contains more elements than B. */

    } else if (s_cmp(op, "<=", op_len, (ftnlen)2) == 0) {
	if (carda > cardb) {
	    ret_val = FALSE_;
	    chkout_("SETC", (ftnlen)4);
	    return ret_val;
	} else {
	    condlt = 0;
	    condeq = 1;
	    condgt = 1;
	    condoa = 0;
	    condob = 1;
	    condab = 1;
	}

/*     If the cardinality of the two sets is not equal, there's no way */
/*     that the two sets could be equal. */

    } else if (s_cmp(op, "=", op_len, (ftnlen)1) == 0) {
	if (carda != cardb) {
	    ret_val = FALSE_;
	    chkout_("SETC", (ftnlen)4);
	    return ret_val;
	} else {
	    condlt = 0;
	    condeq = 1;
	    condgt = 0;
	    condoa = 0;
	    condob = 0;
	    condab = 1;
	}

/*     If the cardinality of the two sets is not equal, the sets */
/*     are not equal. */

    } else if (s_cmp(op, "<>", op_len, (ftnlen)2) == 0) {
	if (carda != cardb) {
	    ret_val = TRUE_;
	    chkout_("SETC", (ftnlen)4);
	    return ret_val;
	} else {
	    condlt = 2;
	    condeq = 1;
	    condgt = 2;
	    condoa = 0;
	    condob = 0;
	    condab = 0;
	}

/*     B cannot be a proper subset of A if the cardinality of A is less */
/*     than or equal to the cardinality of B. */

    } else if (s_cmp(op, ">", op_len, (ftnlen)1) == 0) {
	if (carda <= cardb) {
	    ret_val = FALSE_;
	    chkout_("SETC", (ftnlen)4);
	    return ret_val;
	} else {
	    condlt = 1;
	    condeq = 1;
	    condgt = 0;
	    condoa = 1;
	    condob = 0;
	    condab = 1;
	}

/*     B cannot be a subset of A if B contains more elements than A. */

    } else if (s_cmp(op, ">=", op_len, (ftnlen)2) == 0) {
	if (carda < cardb) {
	    ret_val = FALSE_;
	    chkout_("SETC", (ftnlen)4);
	    return ret_val;
	} else {
	    condlt = 1;
	    condeq = 1;
	    condgt = 0;
	    condoa = 1;
	    condob = 0;
	    condab = 1;
	}

/*     If the cardinality of one of the sets is zero, they can't */
/*     possibly have any elements in common. */

    } else if (s_cmp(op, "&", op_len, (ftnlen)1) == 0) {
	if (carda == 0 || cardb == 0) {
	    ret_val = FALSE_;
	    chkout_("SETC", (ftnlen)4);
	    return ret_val;
	} else {
	    condlt = 1;
	    condeq = 2;
	    condgt = 1;
	    condoa = 0;
	    condob = 0;
	}

/*     If either A or B is the null set, the two sets are disjoint. */

    } else if (s_cmp(op, "~", op_len, (ftnlen)1) == 0) {
	if (carda == 0 || cardb == 0) {
	    ret_val = TRUE_;
	    chkout_("SETC", (ftnlen)4);
	    return ret_val;
	} else {
	    condlt = 1;
	    condeq = 0;
	    condgt = 1;
	    condoa = 1;
	    condob = 1;
	}

/*     If the relational operator is not recognized, signal an */
/*     error. */

    } else {
	setmsg_("Relational operator, *, is not recognized.", (ftnlen)42);
	errch_("*", op, (ftnlen)1, op_len);
	sigerr_("SPICE(INVALIDOPERATION)", (ftnlen)23);
	chkout_("SETC", (ftnlen)4);
	return ret_val;
    }

/*     Initialize counters used for checking the elements of the sets. */

    indexa = 1;
    indexb = 1;
    cond = 0;

/*     If we've come this far we need to check the elements of the */
/*     sets to determine the function value. */

    while(indexa <= carda && indexb <= cardb) {
	if (s_cmp(a + (indexa + 5) * a_len, b + (indexb + 5) * b_len, a_len, 
		b_len) < 0) {
	    cond = condlt;
	    ++indexa;
	} else if (s_cmp(a + (indexa + 5) * a_len, b + (indexb + 5) * b_len, 
		a_len, b_len) == 0) {
	    cond = condeq;
	    ++indexa;
	    ++indexb;
	} else {
	    cond = condgt;
	    ++indexb;
	}

/*        At this point, there are several cases which allow us to */
/*        determine the function value without continuing to compare */
/*        the elements of the sets: */

/*        1. If the operator is '~' and a common element was found, */
/*           the sets are not disjoint ( COND = 0 ). */

/*        2. If the operator is '&' and a common element was found, */
/*           the sets have at least one common element ( COND = 2 ). */

/*        3. If the sets are being compared for containment, and the */
/*           first element of the "contained" set is less than the first */
/*           element of the "containing" set, the "contained" set */
/*           cannot be a subset of the "containing" set ( COND = 0 ). */

/*        4. If the operator is '=' and the elements being compared are */
/*           not equal, the sets are not equal ( COND = 0 ). */

/*        5. If the operator is '<>' and the elements being compared are */
/*           not equal, the sets are not equal ( COND = 2 ). */


	if (cond == 0) {
	    ret_val = FALSE_;
	    chkout_("SETC", (ftnlen)4);
	    return ret_val;
	} else if (cond == 2) {
	    ret_val = TRUE_;
	    chkout_("SETC", (ftnlen)4);
	    return ret_val;
	}
    }

/*     We've exited the loop, so now we need to make a decision based on */
/*     what's left over. */


/*     We've gone through all of set B and there are elements left in */
/*     A. */

    if (indexa <= carda) {
	cond = condoa;

/*     We've gone through all of set A and there are elements left in */
/*     B. */

    } else if (indexb <= cardb) {
	cond = condob;

/*     We've gone through both the sets. */

    } else {
	cond = condab;
    }

/*     Determine the value of SETC from the results. */

    ret_val = cond == 1;
    chkout_("SETC", (ftnlen)4);
    return ret_val;
} /* setc_ */

