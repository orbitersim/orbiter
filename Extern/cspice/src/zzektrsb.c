/* zzektrsb.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      ZZEKTRSB ( EK tree, identify siblings ) */
/* Subroutine */ int zzektrsb_(integer *handle, integer *tree, integer *key, 
	integer *lsib, integer *lkey, integer *rsib, integer *rkey)
{
    integer base, pkey;
    extern integer zzektrbs_(integer *);
    extern /* Subroutine */ int zzektrpi_(integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *);
    integer nkbas, lpidx, lpkey, rpidx, rpkey;
    extern logical failed_(void);
    integer kidbas;
    extern /* Subroutine */ int dasrdi_(integer *, integer *, integer *, 
	    integer *);
    integer keybas, addrss, parent, llpidx, loffst, poffst, roffst;

/* $ Abstract */

/*     Identify the immediate siblings of a node:  return a key in each */
/*     sibling and the siblings' node numbers. */

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
/*     KEY        I   Key of interest. */
/*     LSIB       O   Left sibling node. */
/*     LKEY       O   Key in left sibling. */
/*     RSIB       O   Right sibling node. */
/*     RKEY       O   Key in right sibling. */

/* $ Detailed_Input */

/*     HANDLE         is a file handle of an EK open for read or write */
/*                    access. */

/*     TREE           is the root node number of the tree of interest. */

/*     KEY            is a key belonging to a node whose sibling nodes */
/*                    are sought.  KEY is expected to be an absolute, */
/*                    not node-relative, key. */

/* $ Detailed_Output */

/*     LSIB           is the number of the left sibling node of the node */
/*                    containing KEY.  If the node containing KEY has no */
/*                    left sibling, LSIB is set to zero. */

/*     LKEY           is an absolute key in node LSIB. */

/*     RSIB           is the number of the right sibling node of the node */
/*                    containing KEY.  If the node containing KEY has no */
/*                    right sibling, RSIB is set to zero. */

/*     RKEY           is an absolute key in node RSIB. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If HANDLE is invalid, the error will be diagnosed by routines */
/*         called by this routine. */

/*     2)  If an error occurs while looking up the parent of the node */
/*         containing KEY, the error will be diagnosed by routines */
/*         called by this routine.  It is not an error for the node */
/*         containing KEY to have no parent, as long as KEY belongs to */
/*         the root. */

/*     3)  If an I/O error occurs while reading the indicated file, the */
/*         error will be diagnosed by routines called by this routine. */

/* $ Files */

/*     See the EK Required Reading for a discussion of the EK file */
/*     format. */

/* $ Particulars */

/*     This routine is a utility intended for use by other routines in */
/*     the EKTRxx set. */

/*     The output keys LKEY and RKEY may be used to find the siblings */
/*     of the sibling nodes LSIB and RSIB. */

/* $ Examples */

/*     See ZZEKTRDL. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    Beta Version 1.0.0, 20-OCT-1995 (NJB) */

/* -& */

/*     SPICELIB functions */


/*     Other functions */


/*     Local variables */


/*     Start out by looking up the parent node.  We get LSIB */
/*     and RSIB for free. */

    zzektrpi_(handle, tree, key, &parent, &pkey, &poffst, &lpidx, &lpkey, 
	    lsib, &rpidx, &rpkey, rsib);
    if (failed_()) {
	return 0;
    }

/*     Set the base addresses for the child pointers and keys, */
/*     based on whether the parent is the root. */

    if (parent == *tree) {
	keybas = 5;
	kidbas = 88;
	nkbas = 5;
    } else {
	keybas = 1;
	kidbas = 64;
	nkbas = 1;
    }

/*     We need to find absolute keys in each sibling that exists. */
/*     To do this, we need the node offset of each sibling node. */
/*     That offset is the value of the parent key preceding each node, */
/*     plus the parent's offset. */

    if (lpidx > 1) {

/*        The left parent key has a predecessor.  This predecessor is */
/*        the immediate predecessor of the left sibling node. */

	llpidx = lpidx - 1;
	base = zzektrbs_(&parent);
	addrss = base + keybas + llpidx;
	dasrdi_(handle, &addrss, &addrss, &loffst);
	loffst += poffst;

/*        Get the first key from the left sibling.  Convert the key */
/*        to an absolute key. */

	base = zzektrbs_(lsib);
	addrss = base + 2;
	dasrdi_(handle, &addrss, &addrss, lkey);
	*lkey += loffst;
    } else if (lpidx == 1) {

/*        The left parent key is the first key.  The left sibling has */
/*        no predecessor. */

/*        Get the first key from the left sibling.  Convert the key */
/*        to an absolute key. */

	base = zzektrbs_(lsib);
	addrss = base + 2;
	dasrdi_(handle, &addrss, &addrss, lkey);
	*lkey += poffst;
    } else {

/*        There's no left sibling.  Set the left sibling's key to a */
/*        value that won't be mistaken for a valid one. */

	*lkey = 0;
    }

/*     LKEY is set.  It's time to produce an absolute key for the */
/*     right sibling. */

    if (rpidx > 0) {

/*        The right parent key exists.  This key is the */
/*        immediate predecessor of the right sibling node. */

	roffst = rpkey + poffst;

/*        Get the first key from the right sibling.  Convert the key */
/*        to an absolute key. */

	base = zzektrbs_(rsib);
	addrss = base + 2;
	dasrdi_(handle, &addrss, &addrss, rkey);
	*rkey += roffst;
    } else {

/*        There's no right sibling.  Set the right sibling's key to a */
/*        value that won't be mistaken for a valid one. */

	*rkey = 0;
    }

/*     All outputs are set. */

    return 0;
} /* zzektrsb_ */

