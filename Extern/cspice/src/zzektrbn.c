/* zzektrbn.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__82 = 82;
static integer c__124 = 124;

/* $Procedure      ZZEKTRBN ( EK tree, balance nodes ) */
/* Subroutine */ int zzektrbn_(integer *handle, integer *tree, integer *left, 
	integer *right, integer *parent, integer *pkidx)
{
    integer root;
    extern integer zzektrnk_(integer *, integer *, integer *);
    extern /* Subroutine */ int zzektrrk_(integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *), chkin_(char *, 
	    ftnlen);
    integer schlep;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen);
    integer lnkeys;
    extern /* Subroutine */ int errint_(char *, integer *, ftnlen);
    integer rnkeys, sum;

/* $ Abstract */

/*     Solve overflow in a node by balancing the node */
/*     with one of its sibling nodes. */

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
/*     LEFT       I   Left node of pair to be balanced. */
/*     RIGHT      I   Right node of pair to be balanced. */
/*     PARENT     I   Parent node of pair to be balanced. */
/*     PKIDX      I   Parent key index. */

/* $ Detailed_Input */

/*     HANDLE         is a file handle of an EK open for write access. */

/*     TREE           is the root node number of the tree of interest. */

/*     LEFT, */
/*     RIGHT          are the node numbers of a pair of nodes to */
/*                    be balanced.  LEFT and RIGHT must be neighboring */
/*                    subnodes of a common parent. */

/*     PARENT         is the node number of the common parent node of */
/*                    nodes LEFT, RIGHT. */

/*     PKIDX          is the `parent key index', that is, the */
/*                    node-relative index of the key in the parent that */
/*                    sits between PARENT's child node pointers to */
/*                    nodes LEFT and RIGHT.  The key at location PKIDX */
/*                    is the immediate successor of the greatest key in */
/*                    the subnode headed by LEFT.  It is the immediate */
/*                    predecessor of the least key in the subnode headed */
/*                    by RIGHT. */

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

/*     3)  If either LEFT or RIGHT are actually the root, the error */
/*         SPICE(BUG) is signaled. */

/*     4)  If LEFT and RIGHT are not neighboring sibling nodes, the */
/*         error will be diagnosed by routines called by this routine. */


/*     5)  The sum of the key counts in LEFT and RIGHT must be between */
/*         2*MNKEYC and 2*MXKEYC; otherwise the key count invariants */
/*         cannot be satisfied by balancing.  If the sum fails to meet */
/*         this condition, the error SPICE(BUG) is signaled. */

/* $ Files */

/*     See the EK Required Reading for a discussion of the EK file */
/*     format. */

/* $ Particulars */

/*     Insertions into and deletions from EK trees can result in */
/*     overflows or underflows of keys in nodes affected by these */
/*     operations.  Many times key count invariants can be restored by */
/*     moving keys from one node into an adjacent sibling node.  This */
/*     maneuver is called `balancing' the nodes.  After balancing, the */
/*     key counts of the affected nodes differ by at most 1. */

/*     The balancing process also affects the parent node of the */
/*     neighboring children because one key of the parent sits between */
/*     the children.  This `parent key' gets moved into one of the */
/*     children as keys are shifted.  If the shift is to the right, the */
/*     parent key is the largest key of the shifted set; if the shift */
/*     is to the left, the parent key is the least of the shifted set. */

/*     When keys are shifted, their data values move along with them. */
/*     In general, child pointers move along with keys, but there are */
/*     some tricky points: */

/*        - The left and right child pointers of the parent key don't */
/*          get updated; they continue to point to the two children */
/*          LEFT and RIGHT. */

/*        - On a right shift, the right child pointer of the key that */
/*          gets moved into the parent key's original position becomes */
/*          the first left child pointer of the right sibling.  The left */
/*          child pointer of this key doesn't get moved at all. */

/*        - On a left shift, the left child pointer of the key that */
/*          gets moved into the parent key's original position becomes */
/*          the last right child pointer of the left sibling.  The right */
/*          child pointer of this key becomes the left child pointer of */
/*          the first key of RIGHT. */

/* $ Examples */

/*     See ZZEKTRIN. */

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

/* -    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB) */

/*        Corrected typos in comments. */

/* -    Beta Version 1.0.0, 27-OCT-1995 (NJB) */

/* -& */

/*     Non-SPICELIB functions */


/*     Local variables */


/*     Use discovery check-in for speed. */

    root = *tree;
    if (*left == root || *right == root) {
	chkin_("ZZEKTRBN", (ftnlen)8);
	setmsg_("Input node is root; only children can be balanced.", (ftnlen)
		50);
	sigerr_("SPICE(BUG)", (ftnlen)10);
	chkout_("ZZEKTRBN", (ftnlen)8);
    }

/*     Get the key counts for the left and right nodes. */

    lnkeys = zzektrnk_(handle, tree, left);
    rnkeys = zzektrnk_(handle, tree, right);

/*     Balancing the nodes should give each of them a key count in */
/*     the range of */

/*        MNKEYC : MXKEYC */

/*     If that's not possible, we have a serious problem. */

    sum = lnkeys + rnkeys;
    if (sum > 124 || sum < 82) {
	chkin_("ZZEKTRBN", (ftnlen)8);
	setmsg_("Node # and right sibling # contain # and # keys respectivel"
		"y; count sum should be in range #:#.", (ftnlen)95);
	errint_("#", left, (ftnlen)1);
	errint_("#", right, (ftnlen)1);
	errint_("#", &lnkeys, (ftnlen)1);
	errint_("#", &rnkeys, (ftnlen)1);
	errint_("#", &c__82, (ftnlen)1);
	errint_("#", &c__124, (ftnlen)1);
	sigerr_("SPICE(BUG)", (ftnlen)10);
	chkout_("ZZEKTRBN", (ftnlen)8);
	return 0;
    }

/*     Now, the actions we take depend on whether we must schlep keys */
/*     to the right or left. */

    if (lnkeys > rnkeys) {
	schlep = lnkeys - (sum + 1) / 2;
    } else if (lnkeys < rnkeys) {
	schlep = -(rnkeys - (sum + 1) / 2);
    } else {
	schlep = 0;
    }

/*     Rotate the requested number of keys. */

    zzektrrk_(handle, tree, left, right, parent, pkidx, &schlep);
    return 0;
} /* zzektrbn_ */

