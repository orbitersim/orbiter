/* zzektrin.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* $Procedure      ZZEKTRIN ( EK tree, insert value ) */
/* Subroutine */ int zzektrin_(integer *handle, integer *tree, integer *key, 
	integer *value)
{
    integer node, left, lval, lkey, pkey, root;
    extern /* Subroutine */ int zzektrbn_(integer *, integer *, integer *, 
	    integer *, integer *, integer *), zzektrki_(integer *, integer *, 
	    integer *, integer *, integer *), zzektrlk_(integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, integer *);
    extern integer zzektrnk_(integer *, integer *, integer *);
    extern /* Subroutine */ int zzektrpi_(integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *), zzektrui_(integer *, integer *, 
	    integer *, integer *, logical *);
    integer lnode, level, rnode, right, pkidx, lpidx, state, lpkey, nsize, 
	    nkeys, rpidx, rpkey, trust;
    extern logical failed_(void);
    integer parent;
    logical overfl;
    integer noffst, poffst, idx;
    extern /* Subroutine */ int zzektr13_(integer *, integer *), zzektr23_(
	    integer *, integer *, integer *, integer *, integer *, integer *, 
	    logical *);

/* $ Abstract */

/*     Insert a value into an EK tree at a specified location. */

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

/*     EK */

/* $ Keywords */

/*     EK */
/*     PRIVATE */

/* $ Declarations */
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


/*     Include Section:  EK Das Paging Parameters */

/*        ekpage.inc  Version 4    25-AUG-1995 (NJB) */



/*     The EK DAS paging system makes use of the integer portion */
/*     of an EK file's DAS address space to store the few numbers */
/*     required to describe the system's state.  The allocation */
/*     of DAS integer addresses is shown below. */


/*                       DAS integer array */

/*        +--------------------------------------------+ */
/*        |            EK architecture code            |  Address = 1 */
/*        +--------------------------------------------+ */
/*        |      Character page size (in DAS words)    | */
/*        +--------------------------------------------+ */
/*        |        Character page base address         | */
/*        +--------------------------------------------+ */
/*        |      Number of character pages in file     | */
/*        +--------------------------------------------+ */
/*        |   Number of character pages on free list   | */
/*        +--------------------------------------------+ */
/*        |      Character free list head pointer      |  Address = 6 */
/*        +--------------------------------------------+ */
/*        |                                            |  Addresses = */
/*        |           Metadata for d.p. pages          |    7--11 */
/*        |                                            | */
/*        +--------------------------------------------+ */
/*        |                                            |  Addresses = */
/*        |         Metadata for integer pages         |    12--16 */
/*        |                                            | */
/*        +--------------------------------------------+ */
/*                              . */
/*                              . */
/*                              . */
/*        +--------------------------------------------+ */
/*        |                                            |  End Address = */
/*        |                Unused space                |  integer page */
/*        |                                            |  end */
/*        +--------------------------------------------+ */
/*        |                                            |  Start Address = */
/*        |             First integer page             |  integer page */
/*        |                                            |  base */
/*        +--------------------------------------------+ */
/*                              . */
/*                              . */
/*                              . */
/*        +--------------------------------------------+ */
/*        |                                            | */
/*        |              Last integer page             | */
/*        |                                            | */
/*        +--------------------------------------------+ */

/*     The following parameters indicate positions of elements in the */
/*     paging system metadata array: */



/*     Number of metadata items per data type: */


/*     Character metadata indices: */


/*     Double precision metadata indices: */


/*     Integer metadata indices: */


/*     Size of metadata area: */


/*     Page sizes, in units of DAS words of the appropriate type: */


/*     Default page base addresses: */


/*     End Include Section:  EK Das Paging Parameters */

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


/*     Include Section:  EK Tree Parameters */

/*        ektree.inc  Version 3    22-OCT-1995 (NJB) */


/*     The parameters in this file define the tree structure */
/*     used by the EK system.  This structure is a variant of the */
/*     B*-tree structure described in Knuth's book, that is */

/*         Knuth, Donald E.  "The Art of Computer Programming, */
/*         Volume 3/Sorting and Searching" 1973, pp 471-479. */

/*     The trees used in the EK system differ from generic B*-trees */
/*     primarily in the way keys are treated.  Rather than storing */
/*     unique primary key values in each node, EK trees store integer */
/*     counts that represent the ordinal position of each data value, */
/*     counting from the lowest indexed element in the subtree whose */
/*     root is the node in question.  Thus the keys are unique within */
/*     a node but not across multiple nodes:  in fact the Nth key in */
/*     every leaf node is N.  The absolute ordinal position of a data */
/*     item is defined recursively as the sum of the key of the data item */
/*     and the absolute ordinal position of the data item in the parent */
/*     node that immediately precedes all elements of the node in */
/*     question.  This data structure allows EK trees to support lookup */
/*     of data items based on their ordinal position in a data set.  The */
/*     two prime applications of this capability in the EK system are: */

/*        1) Using trees to index the records in a table, allowing */
/*           the Nth record to be located efficiently. */

/*        2) Using trees to implement order vectors that can be */
/*           maintained when insertions and deletions are done. */



/*                           Root node */

/*        +--------------------------------------------+ */
/*        |              Tree version code             | */
/*        +--------------------------------------------+ */
/*        |           Number of nodes in tree          | */
/*        +--------------------------------------------+ */
/*        |            Number of keys in tree          | */
/*        +--------------------------------------------+ */
/*        |                Depth of tree               | */
/*        +--------------------------------------------+ */
/*        |            Number of keys in root          | */
/*        +--------------------------------------------+ */
/*        |              Space for n keys,             | */
/*        |                                            | */
/*        |        n = 2 * INT( ( 2*m - 2 )/3 )        | */
/*        |                                            | */
/*        |  where m is the max number of children per | */
/*        |          node in the child nodes           | */
/*        +--------------------------------------------+ */
/*        |        Space for n+1 child pointers,       | */
/*        |         where n is as defined above.       | */
/*        +--------------------------------------------+ */
/*        |          Space for n data pointers,        | */
/*        |         where n is as defined above.       | */
/*        +--------------------------------------------+ */


/*                           Child node */

/*        +--------------------------------------------+ */
/*        |        Number of keys present in node      | */
/*        +--------------------------------------------+ */
/*        |              Space for m-1 keys            | */
/*        +--------------------------------------------+ */
/*        |         Space for m child pointers         | */
/*        +--------------------------------------------+ */
/*        |         Space for m-1 data pointers        | */
/*        +--------------------------------------------+ */




/*     The following parameters give the maximum number of children */
/*     allowed in the root and child nodes.  During insertions, the */
/*     number of children may overflow by 1. */


/*     Maximum number of children allowed in a child node: */


/*     Maximum number of keys allowed in a child node: */


/*     Minimum number of children allowed in a child node: */


/*     Minimum number of keys allowed in a child node: */


/*     Maximum number of children allowed in the root node: */


/*     Maximum number of keys allowed in the root node: */


/*     Minimum number of children allowed in the root node: */



/*     The following parameters indicate positions of elements in the */
/*     tree node structures shown above. */


/*     The following parameters are for the root node only: */


/*     Location of version code: */


/*     Version code: */


/*     Location of node count: */


/*     Location of total key count for the tree: */


/*     Location of tree depth: */


/*     Location of count of keys in root node: */


/*     Base address of keys in the root node: */


/*     Base address of child pointers in root node: */


/*     Base address of data pointers in the root node (allow room for */
/*     overflow): */


/*     Size of root node: */


/*     The following parameters are for child nodes only: */


/*     Location of number of keys in node: */


/*     Base address of keys in child nodes: */


/*     Base address of child pointers in child nodes: */


/*     Base address of data pointers in child nodes (allow room */
/*     for overflow): */


/*     Size of child node: */


/*     A number of EK tree routines must declare stacks of fixed */
/*     depth; this depth limit imposes a limit on the maximum depth */
/*     that an EK tree can have.  Because of the large branching */
/*     factor of EK trees, the depth limit is of no practical */
/*     importance:  The number of keys that can be held in an EK */
/*     tree of depth N is */

/*                           N-1 */
/*                     MXKIDC   -  1 */
/*         MXKIDR  *   ------------- */
/*                      MXKIDC  - 1 */


/*     This formula yields a capacity of over 1 billion keys for a */
/*     tree of depth 6. */


/*     End Include Section:  EK Tree Parameters */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   File handle. */
/*     TREE       I   Root of tree. */
/*     KEY        I   Key at which to insert value. */
/*     VALUE      I   Value to insert. */

/* $ Detailed_Input */

/*     HANDLE         is a file handle of an EK open for write access. */

/*     TREE           is the root node number of the tree of interest. */

/*     KEY            is an absolute key indicating the insertion */
/*                    location.  In EK trees, absolute keys are just */
/*                    ordinal positions relative to the leftmost element */
/*                    of the tree, with the leftmost element having */
/*                    position 1.  So setting KEY to 10, for example, */
/*                    indicates that the input VALUE is the 10th item in */
/*                    the tree. */

/*                    KEY must be in the range 1 : (NKEYS+1), where */
/*                    NKEYS is the number of keys in the tree prior to */
/*                    the insertion. */

/*     VALUE          is an integer value to be inserted into the */
/*                    specified tree at the ordinal position KEY. */

/* $ Detailed_Output */

/*     None.  See $Particulars for a description of the effect of this */
/*     routine. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If HANDLE is invalid, the error will be diagnosed by routines */
/*         called by this routine.  The file will not be modified. */

/*     2)  If an I/O error occurs while reading or writing the indicated */
/*         file, the error will be diagnosed by routines called by this */
/*         routine. */

/*     3)  If the input key is out of range, the error is diagnosed by */
/*         routines called by this routine. */

/* $ Files */

/*     See the EK Required Reading for a discussion of the EK file */
/*     format. */

/* $ Particulars */

/*     This routine inserts a value into an EK tree at a specified */
/*     location.  If the location is occupied, the value previously at */
/*     that location and all higher-indexed values have their indexes */
/*     incremented.  Since keys are stored in subtree-relative form, */
/*     the only keys actually modified by the insertion itself are */
/*     higher-indexed keys in the node into which the insertion is done, */
/*     and higher-indexed keys in the chain of ancestors of this node. */

/*     The insertion is not the end of the story, however:  it's possible */
/*     that the node at which the insertion is done (the `target node') */
/*     will overflow.  If overflow occurs, this routine will restore the */
/*     tree to its normal form as follows: */

/*        1)  If a neighbor of the target node has room, data will be */
/*           `rotated' from the target node, through the target's parent, */
/*            and into the neighbor.  The insertion is complete at this */
/*            point. */

/*        2)  If no neighbor has room, then the target node and a */
/*            neighbor are split and recombined into three nodes:  this */
/*            is called a `2-3 split'.  The parent node is modified */
/*            appropriately so that all values are in the proper order */
/*            and all subtree-relative keys are correct.  This 2-3 split */
/*            increases the number of values in the parent by one.  If */
/*            the increase does not cause an overflow in the parent, the */
/*            insertion is complete. */

/*        3)  If the parent overflows as a result of a 2-3 split, the */
/*            solution process is repeated at the parent's level.  The */
/*            process iterates until the overflow is resolved or the */
/*            root overflows. */

/*        4)  If the root overflows, the root is split into two children */
/*            and a new root node; the new root contains a single value. */
/*            The children of the old root become children of the two */
/*            new child nodes of the new root.  This is the only */
/*            case in which the tree grows taller. */

/*            The process of splitting the root is called a `1-3 split'. */
/*            After a 1-3 split is performed, the number of values in */
/*            each node is within bounds. */


/*     An EK tree is always balanced after an insertion:  all leaf nodes */
/*     are at the same level. */

/* $ Examples */

/*     See EKINSR. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     1)  Knuth, Donald E.  "The Art of Computer Programming, Volume */
/*         3/Sorting and Searching" 1973, pp 471-479. */

/*         EK trees are closely related to the B* trees described by */
/*         Knuth. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    Beta Version 1.0.0, 01-NOV-1995 (NJB) */

/* -& */

/*     SPICELIB functions */


/*     Other functions */


/*     Local parameters */


/*     Local variables */


/*     Use discovery check-in for speed. */

/*     Set the variable ROOT, so we'll have something mnemonic to go */
/*     by when referring to the root node. */

    root = *tree;

/*     Work with local copies of the input key and value. */

    lkey = *key;
    lval = *value;

/*     The first step is to insert the key into the tree without */
/*     balancing.  This step may cause a node to overflow.  We'll */
/*     handle the overflow later.  In general, the probability of */
/*     overflow is low:  each overflow creates at least one new node, */
/*     but the ratio of nodes to keys is very small. */

    zzektrui_(handle, tree, &lkey, &lval, &overfl);
    if (failed_()) {
	return 0;
    }

/*     If the insertion didn't result in an overflow, we're done. */

    if (! overfl) {
	return 0;
    }

/*     Handle node overflows, as required. */

    state = 2;
    while(state != 1) {
	if (state == 2) {

/*           Look up the node containing LKEY. */

	    zzektrlk_(handle, tree, &lkey, &idx, &node, &noffst, &level, &
		    lval);
	    if (node == root) {
		state = 6;
	    } else {

/*              See if there's room in the left sibling.  Of course, */
/*              there must be a left sibling in order for there to be */
/*              room. */

		zzektrpi_(handle, tree, &lkey, &parent, &pkey, &poffst, &
			lpidx, &lpkey, &left, &rpidx, &rpkey, &right);
		if (left > 0) {
		    nkeys = zzektrnk_(handle, tree, &left);
		    if (nkeys < 62) {
			lnode = left;
			rnode = node;
			pkidx = lpidx;
			state = 4;
		    } else {
			state = 3;
		    }
		} else {
		    state = 3;
		}
	    }
	} else if (state == 3) {

/*           See whether there's room in the right sibling, if there */
/*           is a right sibling.  The left sibling has already been */
/*           checked and found wanting. */

	    if (right > 0) {
		nkeys = zzektrnk_(handle, tree, &right);
		if (nkeys < 62) {
		    lnode = node;
		    rnode = right;
		    pkidx = rpidx;
		    state = 4;
		} else {
		    lnode = node;
		    rnode = right;
		    pkidx = rpidx;
		    state = 5;
		}
	    } else {

/*              The left sibling is full, but at least it's there. */

		lnode = left;
		rnode = node;
		pkidx = lpidx;
		state = 5;
	    }
	} else if (state == 4) {

/*           LNODE has a right sibling, and between the two nodes, */
/*           there's enough room to accommodate the overflow.  After */
/*           balancing these nodes, we're done. */

	    zzektrbn_(handle, tree, &lnode, &rnode, &parent, &pkidx);
	    state = 1;
	} else if (state == 5) {

/*           LNODE has a right sibling, and between the two nodes, */
/*           there's an overflow of one key.  Split these two nodes */
/*           into three.  This splitting process adds a key to the */
/*           parent; the parent may overflow as a result. */

/*           After executing the 2-3 split, to ensure that we reference */
/*           the parent correctly, we'll obtain a fresh key from the */
/*           parent.  The old key PKEY may not be in the parent any more; */
/*           this key may have been rotated into the middle node created */
/*           by the 2-3 split. */

/*           To start with, we'll get a trusted key from the */
/*           original node NODE.  If NODE got mapped to LNODE, */
/*           then the first key in NODE will be unchanged by */
/*           the 2-3 split.  If NODE got mapped to RNODE, then */
/*           the last key in NODE will be unchanged. */

	    if (node == lnode) {

/*              Save the first key from NODE. */

		zzektrki_(handle, tree, &lkey, &c__1, &trust);
	    } else {

/*              Save the last key from NODE. */

		nsize = zzektrnk_(handle, tree, &node);
		zzektrki_(handle, tree, &lkey, &nsize, &trust);
	    }
	    zzektr23_(handle, tree, &lnode, &rnode, &parent, &pkidx, &overfl);
	    if (overfl) {
		if (parent == root) {
		    state = 6;
		} else {

/*                 We'll need to handle overflow in the parent. */
/*                 The parent should be correctly identified by the */
/*                 parent of TRUST. */

		    zzektrpi_(handle, tree, &trust, &parent, &pkey, &poffst, &
			    lpidx, &lpkey, &left, &rpidx, &rpkey, &right);
		    lkey = pkey;
		    state = 2;
		}
	    } else {
		state = 1;
	    }
	} else if (state == 6) {

/*           We've got an overflow in the root.  Split the root, */
/*           creating two new children.  The root contains a single */
/*           key after this split. */

	    zzektr13_(handle, tree);
	    state = 1;
	}
    }
    return 0;
} /* zzektrin_ */

