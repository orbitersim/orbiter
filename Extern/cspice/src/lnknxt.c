/* lnknxt.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__0 = 0;

/* $Procedure LNKNXT ( LNK, next node ) */
integer lnknxt_(integer *node, integer *pool)
{
    /* System generated locals */
    integer ret_val;

    /* Local variables */
    extern /* Subroutine */ int chkin_(char *, ftnlen), sigerr_(char *, 
	    ftnlen), chkout_(char *, ftnlen), setmsg_(char *, ftnlen), 
	    errint_(char *, integer *, ftnlen);

/* $ Abstract */

/*     Find the node following a specified node in a doubly linked list */
/*     pool. */

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
/*     NODE       I   Number of an allocated node. */
/*     POOL       I   A doubly linked list pool. */
/*     LBPOOL     P   Lower bound of pool column indices. */

/*     The function returns the number of the successor of the node */
/*     indicated by NODE. */

/* $ Detailed_Input */

/*     NODE     is the number of an allocated node in POOL. */

/*     POOL     is a doubly linked list pool. */

/* $ Detailed_Output */

/*     The function returns the number of the successor of the node */
/*     indicated by NODE. If NODE is the tail node of a list, the */
/*     function returns the negative of the node number of the head */
/*     of the list. */

/* $ Parameters */

/*     LBPOOL   is the lower bound of the column indices of the POOL */
/*              array. The columns indexed LBPOOL to 0 are reserved */
/*              as a control area for the pool. */

/* $ Exceptions */

/*     1)  If NODE is the tail node of a list, the function returns the */
/*         negative of the node number of the head of the list. */

/*     2)  If NODE is not a valid node number, the error */
/*         SPICE(INVALIDNODE) is signaled. The value 0 is returned. */

/*     3)  If NODE is not the number of an allocated node, the error */
/*         SPICE(UNALLOCATEDNODE) is signaled. The value 0 is returned. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The raison d'etre of this routine is to allow forward traversal */
/*     of lists in a doubly linked list pool. */

/*     Traversing a list is often performed in cases where the list is */
/*     used to index elements of a data structure, and the elements */
/*     indexed by the list must be searched. */

/*     To traverse a list in backward order, use LNKPRV. */

/* $ Examples */

/*     1)  Let POOL be doubly linked list pool, and let */

/*           3 <--> 7 <--> 1 */

/*         be a list in the pool. The table below shows the effects */
/*         of function references to LNKNXT, where nodes in this list */
/*         are used as inputs: */

/*            Function reference               Value Returned */
/*            ------------------               -------------- */

/*            LNKNXT ( 3, POOL )                     7 */
/*            LNKNXT ( 7, POOL )                     1 */
/*            LNKNXT ( 1, POOL )                    -3 */


/*     2)  Forward traversal of a list: Let POOL be a doubly linked */
/*         list pool, and let NODE be an allocated node in the pool. */
/*         To traverse the list containing NODE in forward order */
/*         and print out the nodes of the list, we can use the */
/*         following code fragment: */

/*            C */
/*            C     Find the head of the list containing NODE. */
/*            C */
/*                  NEXT = LNKHL ( NODE, POOL ) */

/*            C */
/*            C     Traverse the list, printing out node numbers */
/*            C     as we go. */
/*            C */
/*                  WRITE (*,*) 'The list, in forward order, is: ' */

/*                  DO WHILE ( NEXT .GT. 0 ) */

/*                     WRITE (*,*) NEXT */
/*                     NEXT = LNKNXT ( NEXT, POOL ) */

/*                  END DO */

/* $ Restrictions */

/*     1)  Linked list pools must be initialized via the routine */
/*         LNKINI. Failure to initialize a linked list pool */
/*         will almost certainly lead to confusing results. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 24-NOV-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.0, 19-DEC-1995 (NJB) (WLT) */

/* -& */
/* $ Index_Entries */

/*     return next node in linked list */

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


/*     If the node is out of range, something's very wrong. */

    if (*node < 1 || *node > pool[10]) {
	ret_val = 0;
	chkin_("LNKNXT", (ftnlen)6);
	setmsg_("NODE was #; valid range is 1 to #.", (ftnlen)34);
	errint_("#", node, (ftnlen)1);
	errint_("#", &pool[10], (ftnlen)1);
	sigerr_("SPICE(INVALIDNODE)", (ftnlen)18);
	chkout_("LNKNXT", (ftnlen)6);
	return ret_val;

/*     We don't do free nodes. */

    } else if (pool[(*node << 1) + 11] == 0) {
	ret_val = 0;
	chkin_("LNKNXT", (ftnlen)6);
	setmsg_("NODE was #; backward pointer = #; forward pointer = #. \"FR"
		"EE\" is #)", (ftnlen)67);
	errint_("#", node, (ftnlen)1);
	errint_("#", &pool[(*node << 1) + 11], (ftnlen)1);
	errint_("#", &pool[(*node << 1) + 10], (ftnlen)1);
	errint_("#", &c__0, (ftnlen)1);
	sigerr_("SPICE(UNALLOCATEDNODE)", (ftnlen)22);
	chkout_("LNKNXT", (ftnlen)6);
	return ret_val;
    }

/*     Just return the forward pointer of NODE. */

    ret_val = pool[(*node << 1) + 10];
    return ret_val;
} /* lnknxt_ */

