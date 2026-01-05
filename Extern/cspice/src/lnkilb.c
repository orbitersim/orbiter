/* lnkilb.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__0 = 0;

/* $Procedure LNKILB ( LNK, insert list before node ) */
/* Subroutine */ int lnkilb_(integer *list, integer *next, integer *pool)
{
    integer head, tail, prev;
    extern /* Subroutine */ int chkin_(char *, ftnlen), sigerr_(char *, 
	    ftnlen), chkout_(char *, ftnlen), setmsg_(char *, ftnlen), 
	    errint_(char *, integer *, ftnlen);

/* $ Abstract */

/*     Insert the list containing a specified node into a another list, */
/*     preceding a specified node. */

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

/*     LIST */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     LIST       I   Node in the list to be inserted. */
/*     NEXT       I   Node before which a new list is to be inserted. */
/*     POOL      I-O  A doubly linked list pool. */
/*     LBPOOL     P   Lower bound of pool column indices. */

/* $ Detailed_Input */

/*     LIST     is a node in the list to be inserted. The entire */
/*              list containing LIST is to be inserted into the */
/*              list containing NEXT. The inserted list will be */
/*              located between NEXT and its predecessor, if any. */

/*     NEXT     is a node in a list. NEXT is permitted to be */
/*              nil, in which case POOL is not modified. */

/*     POOL     is a doubly linked list pool. */

/* $ Detailed_Output */

/*     POOL     is the input pool, with the following */
/*              modifications: */

/*                 Let HEAD and TAIL be the head and tail nodes of */
/*                 the list containing LIST. Then on output */

/*                    -- The successor of TAIL is NEXT. */

/*                    -- The predecessor of NEXT is TAIL. */


/*                 Let PREV be the node that on input was the */
/*                 predecessor of NEXT; if PREV exists, then on */
/*                 output */

/*                    -- The successor of PREV is HEAD. */

/*                    -- The predecessor of HEAD is PREV. */

/*                 If PREV is nil, the backward pointer of the */
/*                 inserted sublist is set to the negative of */
/*                 the tail of the list containing NEXT. */

/* $ Parameters */

/*     LBPOOL   is the lower bound of the column indices of the POOL */
/*              array. The columns indexed LBPOOL to 0 are reserved */
/*              as a control area for the pool. */

/* $ Exceptions */

/*     For efficiency, discovery check-in is used in this routine. */

/*     1)  If LIST is not a valid node number, the error */
/*         SPICE(INVALIDNODE) is signaled. POOL will not be */
/*         modified. */

/*     2)  If NEXT is positive but is not a valid node number, the error */
/*         SPICE(INVALIDNODE) is signaled. POOL will not be */
/*         modified. */

/*     3)  It is not an error for NEXT to be non-positive; if it is, */
/*         the call to this routine does not affect the pool. */

/*     4)  If either of LIST or NEXT are valid node numbers but are not */
/*         allocated, the error SPICE(UNALLOCATEDNODE) is signaled. POOL */
/*         will not be modified. */

/*     5)  If LIST belongs to the same list as does NEXT, this routine */
/*         may fail in mysterious ways. For efficiency, this error */
/*         condition is not checked. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine is used for augmenting lists by inserting other */
/*     lists into them. The case of insertion of a single allocated */
/*     node is not special: this is insertion of a singleton list. */

/*     To insert a list into a list AFTER a specified element, use the */
/*     routine LNKILA. */

/* $ Examples */

/*     1)  Let POOL be a doubly linked list pool that contains the lists */

/*             3 <--> 7 <--> 1    and    500 <--> 2 <--> 80 */

/*         To insert the second list into the first before node 7, use */
/*         the call */

/*             CALL LNKILB ( 500, 7, POOL ) */

/*         The resulting list will be: */

/*             3 <--> 500 <--> 2 <--> 80 <--> 7 <--> 1 */


/*     2)  Let POOL be a doubly linked list pool that contains 5 nodes. */
/*         The sequence of calls */

/*            HEAD = 0 */

/*            DO I = 1, 5 */
/*               CALL LNKAN  ( POOL, NODE       ) */
/*               CALL LNKILB ( NODE, HEAD, POOL ) */
/*               HEAD = NODE */
/*            END DO */

/*         builds the list */

/*             5 <--> 4 <--> 3 <--> 2 <--> 1 */

/*         Note that the first call to LNKILB does not cause an error */
/*         to be signaled, even though HEAD is 0 at that point. */

/* $ Restrictions */

/*     1)  Linked list pools must be initialized via the routine */
/*         LNKINI. Failure to initialize a linked list pool */
/*         will almost certainly lead to confusing results. */

/*     2)  For efficiency, discovery check-in is used in this routine. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 24-NOV-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/*        Added note about efficiency in $Restrictions section. */

/* -    SPICELIB Version 1.0.0, 19-DEC-1995 (NJB) (WLT) */

/* -& */
/* $ Index_Entries */

/*     insert sublist into linked list before a node */

/* -& */

/*     Local parameters */


/*     The control area contains 3 elements.  They are: */

/*        The "size" of the pool, that is, the number */
/*        of nodes in the pool. */

/*        The number of free nodes in the pool. */

/*        The "free pointer," which is the column index of the first free */
/*        node. */

/*     Parameters defining the row and column indices of these control */
/*     elements are given below. */


/*     Each assigned node consists of a backward pointer and a forward */
/*     pointer. */

/*        +-------------+       +-------------+       +-------------+ */
/*        |  forward--> |       |  forward--> |       |  forward--> | */
/*        +-------------+  ...  +-------------+  ...  +-------------+ */
/*        | <--backward |       | <--backward |       | <--backward | */
/*        +-------------+       +-------------+       +-------------+ */

/*            node 1                 node I              node SIZE */




/*     Free nodes say that that's what they are.  The way they say it */
/*     is by containing the value FREE in their backward pointers. */
/*     Needless to say, FREE is a value that cannot be a valid pointer. */


/*     Local variables */


/*     Use discovery check-in. */


/*     If NEXT is non-positive, return now. */

    if (*next <= 0) {
	return 0;
    }

/*     If we arrived here, NEXT and LIST must be valid node numbers. */
/*     These nodes must be allocated as well. */

    if (*next > pool[10] || *list < 1 || *list > pool[10]) {
	chkin_("LNKILB", (ftnlen)6);
	setmsg_("NEXT was #.  LIST was #. Valid range is 1 to #.", (ftnlen)47)
		;
	errint_("#", next, (ftnlen)1);
	errint_("#", list, (ftnlen)1);
	errint_("#", &pool[10], (ftnlen)1);
	sigerr_("SPICE(INVALIDNODE)", (ftnlen)18);
	chkout_("LNKILB", (ftnlen)6);
	return 0;
    } else if (pool[(*next << 1) + 11] == 0 || pool[(*list << 1) + 11] == 0) {
	chkin_("LNKILB", (ftnlen)6);
	setmsg_("Node NEXT: node number = #; backward pointer = #;  forward "
		"pointer = #. Node LIST: node number = #; backward pointer = "
		"#;  forward pointer = #. (\"FREE\" is #)", (ftnlen)157);
	errint_("#", next, (ftnlen)1);
	errint_("#", &pool[(*next << 1) + 11], (ftnlen)1);
	errint_("#", &pool[(*next << 1) + 10], (ftnlen)1);
	errint_("#", list, (ftnlen)1);
	errint_("#", &pool[(*list << 1) + 11], (ftnlen)1);
	errint_("#", &pool[(*list << 1) + 10], (ftnlen)1);
	errint_("#", &c__0, (ftnlen)1);
	sigerr_("SPICE(UNALLOCATEDNODE)", (ftnlen)22);
	chkout_("LNKILB", (ftnlen)6);
	return 0;
    }

/*     Find the head and tail of the list containing LIST. */

    head = *list;
    while(pool[(head << 1) + 11] > 0) {
	head = pool[(head << 1) + 11];
    }
    tail = -pool[(head << 1) + 11];

/*     Let PREV be the backward pointer of NEXT. */

/*     Insert TAIL before NEXT. */

/*     If NEXT has a predecessor, HEAD follows it. */

/*     If NEXT has no predecessor, HEAD is the new head of the list. */
/*     The forward pointer of the tail of the merged list should */
/*     be set to -HEAD. */

/*     In either case, the backward pointer of HEAD should be set */
/*     to the backward pointer of NEXT. */


    prev = pool[(*next << 1) + 11];
    pool[(tail << 1) + 10] = *next;
    pool[(*next << 1) + 11] = tail;
    if (prev > 0) {
	pool[(prev << 1) + 10] = head;
    } else {
	pool[(-prev << 1) + 10] = -head;
    }
    pool[(head << 1) + 11] = prev;
    return 0;
} /* lnkilb_ */

