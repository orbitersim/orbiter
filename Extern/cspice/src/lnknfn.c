/* lnknfn.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure LNKNFN ( LNK, number of free nodes ) */
integer lnknfn_(integer *pool)
{
    /* System generated locals */
    integer ret_val;

/* $ Abstract */

/*     Return the number of free nodes in a doubly linked list pool. */

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
/*     POOL       I   A doubly linked list pool. */
/*     LBPOOL     P   Lower bound of pool column indices. */

/*     The function returns the number of free nodes in the pool. */

/* $ Detailed_Input */

/*     SIZE     is the number of nodes in the pool. */

/*     POOL     is a doubly linked list pool. */

/* $ Detailed_Output */

/*     The function returns the number of free nodes in the pool. */

/* $ Parameters */

/*     LBPOOL   is the lower bound of the column indices of the POOL */
/*              array. The columns indexed LBPOOL to 0 are reserved */
/*              as a control area for the pool. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine allows the caller to find the number of free nodes */
/*     available in a doubly linked list pool, without having to make */
/*     use of knowledge of the internal structure of the pool. */

/*     Routines that allocate nodes can use this routine to determine */
/*     how many nodes can be allocated safely---an attempt to allocate */
/*     a node when no free nodes are available causes a SPICELIB error */
/*     to be signaled. */

/* $ Examples */

/*     1)  Let POOL be a doubly linked list pool containing 5 nodes. */
/*         If POOL contains the list */

/*            4 <--> 5 <--> 1 <--> 2 */


/*         and the node 3 is unallocated, then the function reference */

/*            NFREE  =  LNKNFN ( POOL ) */


/*         will assign the value 1 to NFREE. */

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

/*     return number of nodes in linked list pool */

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


/*     Grab the number of free nodes from the control area. */

    ret_val = pool[11];
    return ret_val;
} /* lnknfn_ */

