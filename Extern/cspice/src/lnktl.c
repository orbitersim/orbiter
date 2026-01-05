/* lnktl.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__0 = 0;

/* $Procedure LNKTL ( LNK, tail of list ) */
integer lnktl_(integer *node, integer *pool)
{
    /* System generated locals */
    integer ret_val;

    /* Local variables */
    integer next;
    extern /* Subroutine */ int chkin_(char *, ftnlen), sigerr_(char *, 
	    ftnlen), chkout_(char *, ftnlen), setmsg_(char *, ftnlen), 
	    errint_(char *, integer *, ftnlen);

/* $ Abstract */

/*     Return the tail node of the list containing a specified node. */

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
/*     NODE       I   Number of a node. */
/*     POOL       I   A doubly linked list pool. */
/*     LBPOOL     P   Lower bound of pool column indices. */

/*     The function returns the number of the tail node of the list */
/*     containing NODE. */

/* $ Detailed_Input */

/*     NODE     is the number of a node in POOL. Normally, */
/*              NODE will designate an allocated node, but NODE */
/*              is permitted to be less than or equal to zero. */

/*     POOL     is a doubly linked list pool. */

/* $ Detailed_Output */

/*     The function returns the number of the tail node of the list */
/*     containing NODE. If NODE is non-positive, the function returns */
/*     zero. */

/* $ Parameters */

/*     LBPOOL   is the lower bound of the column indices of the POOL */
/*              array. The columns indexed LBPOOL to 0 are reserved */
/*              as a control area for the pool. */

/* $ Exceptions */

/*     1)  If the NODE is less than or equal to zero, NODE is not */
/*         considered to be erroneous. The value 0 is returned. */

/*     2)  If NODE is greater than the size of the pool, the error */
/*         SPICE(INVALIDNODE) is signaled. The value 0 is returned. */

/*     3)  If NODE is not the number of an allocated node, the error */
/*         SPICE(UNALLOCATEDNODE) is signaled. The value 0 is returned. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine provides a convenient way to find the tail of a list */
/*     in a doubly linked list pool. The need to find the tail of a */
/*     list arises in applications such as buffer management. For */
/*     example, in a system using a "least recently used" buffer */
/*     replacement policy, the tail of a list may point to the least */
/*     recently accessed buffer element. */

/* $ Examples */

/*     1)  If POOL is a doubly linked list pool that contains the list */

/*            3 <--> 7 <--> 1 <--> 44 */

/*         any of function references */

/*            TAIL = LNKTL ( 3,  POOL ) */
/*            TAIL = LNKTL ( 7,  POOL ) */
/*            TAIL = LNKTL ( 44, POOL ) */

/*         will assign the value 44 to TAIL. */


/*     2)  If POOL is a doubly linked list pool that contains the */
/*         singleton list consisting of the allocated node */

/*            44 */

/*         the function reference */

/*            TAIL = LNKTL ( 44, POOL ) */

/*         will assign the value 44 to TAIL. */

/* $ Restrictions */

/*     1)  Linked list pools must be initialized via the routine */
/*         LNKINI. Failure to initialize a linked list pool, */
/*         will almost certainly lead to confusing results. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.1, 24-NOV-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.1.0, 09-JAN-1997 (NJB) */

/*        Corrected module name in one pair of CHKIN/CHKOUT calls. */

/* -    SPICELIB Version 1.0.0, 19-DEC-1995 (NJB) (WLT) */

/* -& */
/* $ Index_Entries */

/*     return tail of linked list */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.1.0, 09-JAN-1997 (NJB) */

/*        Corrected module name in one pair of CHKIN/CHKOUT calls. The */
/*        affected error case was the check for a node number being out */
/*        of range. */

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


/*     If the node is non-positive, we regard it as the nil node. */

    if (*node < 1) {
	ret_val = 0;
	return ret_val;

/*     If the node is out of range, something's very wrong. */

    } else if (*node > pool[10]) {
	ret_val = 0;
	chkin_("LNKTL", (ftnlen)5);
	setmsg_("NODE was #; valid range is 1 to #.", (ftnlen)34);
	errint_("#", node, (ftnlen)1);
	errint_("#", &pool[10], (ftnlen)1);
	sigerr_("SPICE(INVALIDNODE)", (ftnlen)18);
	chkout_("LNKTL", (ftnlen)5);
	return ret_val;

/*     We don't do free nodes. */

    } else if (pool[(*node << 1) + 11] == 0) {
	ret_val = 0;
	chkin_("LNKTL", (ftnlen)5);
	setmsg_("NODE was #; backward pointer = #; forward pointer = #. \"FR"
		"EE\" is #)", (ftnlen)67);
	errint_("#", node, (ftnlen)1);
	errint_("#", &pool[(*node << 1) + 11], (ftnlen)1);
	errint_("#", &pool[(*node << 1) + 10], (ftnlen)1);
	errint_("#", &c__0, (ftnlen)1);
	sigerr_("SPICE(UNALLOCATEDNODE)", (ftnlen)22);
	chkout_("LNKTL", (ftnlen)5);
	return ret_val;
    }

/*     Find the tail of the list. */

    ret_val = *node;
    next = pool[(*node << 1) + 10];
    while(next > 0) {
	ret_val = next;
	next = pool[(ret_val << 1) + 10];
    }

/*     LNKTL is now the tail of the list. */

    return ret_val;
} /* lnktl_ */

