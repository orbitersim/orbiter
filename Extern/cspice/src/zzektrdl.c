/* zzektrdl.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c_n1 = -1;

/* $Procedure      ZZEKTRDL ( EK tree, delete value ) */
/* Subroutine */ int zzektrdl_(integer *handle, integer *tree, integer *key)
{
    integer node, lsib, left, rsib, lkey, pkey, rkey, root;
    extern /* Subroutine */ int zzektrbn_(integer *, integer *, integer *, 
	    integer *, integer *, integer *), zzektrki_(integer *, integer *, 
	    integer *, integer *, integer *), zzektrsb_(integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *), zzektrlk_(
	    integer *, integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *);
    extern integer zzektrnk_(integer *, integer *, integer *);
    extern /* Subroutine */ int zzektrud_(integer *, integer *, integer *, 
	    integer *, logical *), zzektrpi_(integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *), zzektrrk_(integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *);
    integer lnode, mnode, level, llsib, rnode, lrsib, right, rlsib, llkey, 
	    lnkey, lpidx, lpkey, rrsib, lrkey, rlkey, rpidx, nkeys, rpkey, 
	    state, rrkey, trust;
    extern logical failed_(void);
    integer parent;
    logical undrfl;
    integer noffst, poffst, trgkey, idx, ptr;
    extern /* Subroutine */ int zzektr31_(integer *, integer *), zzektr32_(
	    integer *, integer *, integer *, integer *, integer *, integer *, 
	    integer *, logical *);

/* $ Abstract */

/*     Delete a value from an EK tree at a specified location. */

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
/*     KEY        I   Key at which to delete value. */

/* $ Detailed_Input */

/*     HANDLE         is a file handle of an EK open for write access. */

/*     TREE           is the root node number of the tree of interest. */

/*     KEY            is an absolute key indicating the deletion */
/*                    location.  In EK trees, absolute keys are just */
/*                    ordinal positions relative to the leftmost element */
/*                    of the tree, with the leftmost element having */
/*                    position 1.  So setting KEY to 10, for example, */
/*                    indicates that the input VALUE is the 10th item in */
/*                    the tree. */

/*                    KEY must be in the range 1 : NKEYS, where */
/*                    NKEYS is the number of keys in the tree prior to */
/*                    the deletion. */

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

/*     This routine deletes a value from an EK tree at a specified */
/*     location.  The successor of the value and all higher-indexed */
/*     values have their indexes decremented.  Since keys are stored in */
/*     subtree-relative form, the only keys actually modified by the */
/*     deletion itself are higher-indexed keys in the node from which */
/*     the deletion is done, and higher-indexed keys in the chain of */
/*     ancestors of this node. */

/*     The deletion is always done from a leaf node.  If KEY is not in a */
/*     leaf node, the value corresponding to KEY is swapped with that of */
/*     an immediate neighbor, and the neighbor is deleted.  This is */
/*     possible because every key is either in a leaf or has the property */
/*     that its predecessor and successor are both located in leaf nodes. */

/*     The deletion is not the end of the story, however:  it's possible */
/*     that the node from which the deletion is done (the `target node') */
/*     will underflow.  If underflow occurs, this routine will restore */
/*     the tree to its normal form as follows: */

/*        1)  If a neighbor of the target node contains at least one more */
/*            key than the minimum allowed number, data will be `rotated' */
/*            from the neighbor node, through the target's parent, */
/*            and into the target.  The deletion is complete at this */
/*            point. */

/*        2)  If the target node has only one neighbor, but that neighbor */
/*            is neighbor to a sibling that can contribute a key, data */
/*            will be rotated from the second sibling, through the */
/*            siblings' parent, into the first sibling, and then from */
/*            the first sibling through the target's parent, and into */
/*            the target.  The deletion is complete at this point. */

/*        3)  If the target is not a child of the root, and if */
/*            the target has two neighbors, but neither neighbor has a */
/*            key to spare, then the target node and its neighbors will */
/*            be merged into two nodes:   this is called a `3-2 merge'. */
/*            The parent node is modified appropriately so that all */
/*            values are in the proper order and all subtree-relative */
/*            keys are correct.  This `3-2 merge' decreases the number */
/*            of values in the parent by one.  If the decrease does not */
/*            cause an underflow in the parent, the deletion is complete. */

/*            If the target has only one neighbor, and both the neighbor */
/*            and the neighbor's other neighbor (which always exists) */
/*            contain the minimum number of keys, these three nodes are */
/*            combined into two via a 3-2 merge. */

/*        4)  If the parent underflows as a result of a 3-2 merge, the */
/*            solution process is repeated at the parent's level.  The */
/*            process iterates until the underflow is resolved or a */
/*            child of the root underflows. */

/*        5)  If a child of the root underflows, the problem is solved */
/*            by balancing keys with a neighbor if possible.  Balancing */
/*            cannot be done only if the root has only two children, and */
/*            these contain the minimum number of keys.  In this case, */
/*            the contents of the two children of the root are moved */
/*            into the root and the children are eliminated.  The */
/*            children of the child nodes become children of the root. */
/*            This is the only case in which the tree grows shorter. */

/*            The process of collapsing two child nodes into the root is */
/*            called a `3-1 merge'.  After a 3-1 merge is performed, the */
/*            number of values in each node is within bounds. */


/*     An EK tree is always balanced after a deletion:  all leaf nodes */
/*     are at the same level. */

/* $ Examples */

/*     See EKDELR. */

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

/* -    Beta Version 1.0.0, 23-OCT-1995 (NJB) */

/* -& */

/*     SPICELIB functions */


/*     Other functions */


/*     Local parameters */


/*     Local variables */


/*     Use discovery check-in for speed. */

/*     Set the variable ROOT, so we'll have something mnemonic to go */
/*     by when referring to the root node. */

    root = *tree;

/*     Work with a local copy of the input key. */

    lkey = *key;

/*     The first step is to delete the key from the tree without */
/*     balancing.  This step may cause a node to underflow.  We'll */
/*     handle the underflow later. */

    zzektrud_(handle, tree, &lkey, &trgkey, &undrfl);
    if (failed_()) {
	return 0;
    }

/*     If the deletion didn't result in an underflow, we're done. */

    if (! undrfl) {
	return 0;
    }

/*     Handle node underflows, as required.  We describe our approach */
/*     below.  If any step fails, we try the next step.  We proceed */
/*     until we succeed in resolving the underflow. */

/*        1) If an immediate sibling can contribute a key, balance NODE */
/*           with that sibling. */

/*        2) If both left and right siblings exist, but neither can */
/*           contribute a key, execute a 3-2 merge. */

/*        3) If the left sibling has its own left sibling, and if that */
/*           second left sibling can contribute a key, rotate a key */
/*           from that sibling into NODE's left sibling.  Then execute */
/*           (1). */

/*        4) If the left sibling has its own left sibling, and if that */
/*           second left sibling cannot contribute a key, execute a 3-2 */
/*           merge using NODE as the rightmost child. */

/*        5) Same as (3), except on the right side. */

/*        6) Same as (4), except on the right side. */

/*        7) Arrival at this step implies that NODE is a child of the */
/*           root and has one sibling.  Execute a 3-1 merge. */

    state = 2;
    while(state != 1) {
	if (state == 2) {

/*           Look up the node containing the target key TRGKEY.  This */
/*           is where the underflow occurred; note that this node may */
/*           be different from the one that contained LKEY. */

	    zzektrlk_(handle, tree, &trgkey, &idx, &node, &noffst, &level, &
		    ptr);

/*           Look up the siblings of NODE.  If either sibling exists */
/*           and has a surplus of keys, we can remove the underflow */
/*           by balancing. */

	    zzektrsb_(handle, tree, &trgkey, &lsib, &lkey, &rsib, &rkey);
	    if (lsib > 0) {
		nkeys = zzektrnk_(handle, tree, &lsib);
		if (nkeys > 41) {

/*                 The left sibling can contribute a key. */

		    lnkey = lkey;
		    lnode = lsib;
		    rnode = node;
		    state = 4;
		} else if (rsib > 0) {

/*                 The left sibling cannot help with balancing, but */
/*                 the right sibling may be able to. */

		    state = 3;
		} else {

/*                 The right sibling does not exist; the only chance */
/*                 of balancing will come from the left sibling of */
/*                 LSIB, if such a sibling exists. */

		    state = 7;
		}
	    } else {

/*              There is no left sibling, so there must be a right */
/*              sibling.  Examine it. */

		state = 3;
	    }
	} else if (state == 3) {

/*           See whether there's a node surplus in the right sibling */
/*           The left sibling has already been checked and found wanting, */
/*           or wasn't found at all. */

	    nkeys = zzektrnk_(handle, tree, &rsib);
	    if (nkeys > 41) {

/*              The right sibling can contribute a key. */

		lnkey = trgkey;
		lnode = node;
		rnode = rsib;
		state = 4;
	    } else if (lsib > 0) {

/*              NODE has siblings on both sides, and each one contains */
/*              the minimum number of keys.  Execute a 3-2 merge. */

		lnkey = lkey;
		lnode = lsib;
		mnode = node;
		rnode = rsib;
		state = 5;
	    } else {

/*              Look for the right sibling of the right sibling. */

		state = 8;
	    }
	} else if (state == 7) {

/*           See whether the left sibling has its own left sibling. */

	    zzektrsb_(handle, tree, &lkey, &llsib, &llkey, &lrsib, &lrkey);
	    if (llsib > 0) {
		nkeys = zzektrnk_(handle, tree, &llsib);
		if (nkeys > 41) {

/*                 The left**2 sibling can contribute a key.  Rotate */
/*                 this key into the left sibling.  We'll need the */
/*                 parent and index of left parent key of LSIB in order */
/*                 to do this rotation. */

		    zzektrpi_(handle, tree, &lkey, &parent, &pkey, &poffst, &
			    lpidx, &lpkey, &llsib, &rpidx, &rpkey, &lrsib);
		    zzektrrk_(handle, tree, &llsib, &lsib, &parent, &lpidx, &
			    c__1);

/*                 Now LSIB has a one-key surplus, so we can balance */
/*                 LSIB and NODE. */

		    lnkey = lkey;
		    lnode = lsib;
		    rnode = node;
		    state = 4;
		} else {

/*                 The left**2 sibling contains the minimum allowed */
/*                 number of keys.  Execute a 3-2 merge, with NODE */
/*                 as the right node. */

		    lnkey = llkey;
		    lnode = llsib;
		    mnode = lsib;
		    rnode = node;
		    state = 5;
		}
	    } else {

/*              LSIB and NODE are the only children of their parent. */
/*              The parent must be the root.  Also, LSIB and NODE */
/*              together contain the one less than twice the minimum */
/*              allowed number of keys.  Execute a 3-1 merge. */

		lnode = lsib;
		rnode = node;
		state = 6;
	    }
	} else if (state == 8) {

/*           See whether the right sibling has its own right sibling. */

	    zzektrsb_(handle, tree, &rkey, &rlsib, &rlkey, &rrsib, &rrkey);
	    if (rrsib > 0) {
		nkeys = zzektrnk_(handle, tree, &rrsib);
		if (nkeys > 41) {

/*                 The right**2 sibling can contribute a key.  Rotate */
/*                 this key into the right sibling.  We'll need the */
/*                 parent and index of the right parent key of RSIB in */
/*                 order to do this rotation. */

		    zzektrpi_(handle, tree, &rkey, &parent, &pkey, &poffst, &
			    lpidx, &lpkey, &rlsib, &rpidx, &rpkey, &rrsib);
		    zzektrrk_(handle, tree, &rsib, &rrsib, &parent, &rpidx, &
			    c_n1);

/*                 Now RSIB has a one-key surplus, so we can balance */
/*                 RSIB and NODE. */

		    lnkey = trgkey;
		    lnode = node;
		    rnode = rsib;
		    state = 4;
		} else {

/*                 The right**2 sibling contains the minimum allowed */
/*                 number of keys.  Execute a 3-2 merge, with NODE */
/*                 as the left node. */

		    lnkey = trgkey;
		    lnode = node;
		    mnode = rsib;
		    rnode = rrsib;
		    state = 5;
		}
	    } else {

/*              RSIB and NODE are the only children of their parent. */
/*              The parent must be the root.  Also, RSIB and NODE */
/*              together contain one less than twice the minimum allowed */
/*              number of keys.  Execute a 3-1 merge. */

		lnode = node;
		rnode = rsib;
		state = 6;
	    }
	} else if (state == 4) {

/*           LNODE has a right sibling, and between the two nodes, */
/*           there are enough keys to accommodate the underflow.  After */
/*           balancing these nodes, we're done. */

	    zzektrpi_(handle, tree, &lnkey, &parent, &pkey, &poffst, &lpidx, &
		    lpkey, &rlsib, &rpidx, &rpkey, &rrsib);

/*           The common parent of the nodes is PARENT.  The right parent */
/*           key of the left node is at location RPIDX.  We're ready to */
/*           balance the nodes. */

	    zzektrbn_(handle, tree, &lnode, &rnode, &parent, &rpidx);
	    state = 1;
	} else if (state == 5) {

/*           LNODE, MNODE, and RNODE are siblings, and between the three */
/*           nodes, there's an underflow of one key.  Merge these three */
/*           nodes into two.  This merging process removes a key from the */
/*           parent; the parent may underflow as a result. */

/*           After executing the 3-2 merge, to ensure that we reference */
/*           the parent correctly, we'll obtain a fresh key from the */
/*           parent. */

/*           To start with, we'll get a trusted key from the */
/*           leftmost node LNODE.  The first key of LNODE won't be */
/*           touched by the merge. */

	    zzektrki_(handle, tree, &lnkey, &c__1, &trust);
	    zzektrpi_(handle, tree, &lnkey, &parent, &pkey, &poffst, &lpidx, &
		    lpkey, &rlsib, &rpidx, &rpkey, &rrsib);

/*           The right parent key of the left node is the left parent */
/*           key of the middle node.  The index of this key is required */
/*           by ZZEKTR32. */

	    zzektr32_(handle, tree, &lnode, &mnode, &rnode, &parent, &rpidx, &
		    undrfl);
	    if (undrfl) {

/*              We'll need to handle underflow in the parent. */
/*              The parent should be correctly identified by the */
/*              parent of TRUST. */

/*              Note that a 3-2 merge can't create an underflow in */
/*              the parent if the parent is the root:  the parent */
/*              contains at least one key after this merge. */

		zzektrpi_(handle, tree, &trust, &parent, &pkey, &poffst, &
			lpidx, &lpkey, &left, &rpidx, &rpkey, &right);
		trgkey = pkey;
		state = 2;
	    } else {
		state = 1;
	    }
	} else if (state == 6) {

/*           We've got an underflow in the two children of the root. */
/*           Move all of the keys from these children into the root. */
/*           The root contains the maximum allowed number of keys */
/*           after this merge. */

	    zzektr31_(handle, tree);
	    state = 1;
	}
    }
    return 0;
} /* zzektrdl_ */

