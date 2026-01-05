/* lnkini.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure LNKINI ( LNK, initialize ) */
/* Subroutine */ int lnkini_(integer *size, integer *pool)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    integer i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen), sigerr_(char *, 
	    ftnlen), chkout_(char *, ftnlen), setmsg_(char *, ftnlen), 
	    errint_(char *, integer *, ftnlen);

/* $ Abstract */

/*     Initialize a doubly linked list pool. */

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
/*     SIZE       I   Number of nodes in the pool. */
/*     POOL      I-O  An array that is a linked pool on output. */
/*     LBPOOL     P   Lower bound of pool column indices. */

/* $ Detailed_Input */

/*     SIZE     is the number of nodes in the pool. */

/*     POOL     is an integer array that will contain the linked */
/*              pool on output. */

/* $ Detailed_Output */

/*     POOL     is an initialized doubly linked list pool. */
/*              The status of the pool is as follows: */

/*                --  All nodes in the pool are on the free list. */

/*                --  The free pointer indicates the first node. */

/*                --  The total node count is set to the input */
/*                    value, SIZE. */

/*                --  The free node count is the input value, SIZE. */

/* $ Parameters */

/*     LBPOOL   is the lower bound of the column indices of the POOL */
/*              array. The columns indexed LBPOOL to 0 are reserved */
/*              as a control area for the pool. */

/* $ Exceptions */

/*     1)  If the requested number of nodes is nonpositive, the error */
/*         SPICE(INVALIDCOUNT) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     LNKINI must be called once to initialize a doubly linked list */
/*     pool before the pool is used. LNKINI can be called at any time */
/*     to re-initialize a doubly linked list pool. */

/*     The functions */

/*        LNKNFN ( LNK, number of free nodes ) and */
/*        LNKSIZ ( LNK, size of pool ) */

/*     will both return the value PLSIZE if called immediately after a */
/*     call to LNKINI. */

/* $ Examples */

/*     1)  Let POOL be a doubly linked list pool with a maximum of */
/*         100 nodes. POOL should be declared as follows: */

/*            INTEGER               LBPOOL */
/*            PARAMETER           ( LBPOOL = -5 ) */

/*            INTEGER               PLSIZE */
/*            PARAMETER           ( PLSIZE = 100 ) */

/*            INTEGER               POOL ( 2, LBPOOL : PLSIZE ) */


/*         To initialize POOL, use the call */

/*            CALL LNKINI ( PLSIZE, POOL ) */

/* $ Restrictions */

/*     None. */

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

/*     initialize linked list pool */

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


/*     The requested number of nodes must be valid. */

    if (*size < 1) {
	chkin_("LNKINI", (ftnlen)6);
	setmsg_("A linked list cannot have # nodes.", (ftnlen)34);
	errint_("#", size, (ftnlen)1);
	sigerr_("SPICE(INVALIDCOUNT)", (ftnlen)19);
	chkout_("LNKINI", (ftnlen)6);
	return 0;
    }

/*     Initialize the pool.  The free list occupies the whole pool at */
/*     this point. */


/*     POOL( SIZROW, SIZCOL ) is the pool size. */

    pool[10] = *size;

/*     POOL( NFRROW, NFRCOL ) is the number of free nodes. */

    pool[11] = *size;

/*     POOL( FREROW, FRECOL) is the "free" pointer.  It points to the */
/*     first free node, which is node 1. */

    pool[8] = 1;

/*     Initialize the backward and forward pointers.  The last forward */
/*     pointer is zero.  All of the backward pointers contain the value */
/*     FREE. */

    i__1 = *size - 1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	pool[(i__ << 1) + 10] = i__ + 1;
	pool[(i__ << 1) + 11] = 0;
    }
    pool[(*size << 1) + 10] = 0;
    pool[(*size << 1) + 11] = 0;
    return 0;
} /* lnkini_ */

