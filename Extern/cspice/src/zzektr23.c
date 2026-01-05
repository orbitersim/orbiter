/* zzektr23.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__125 = 125;
static integer c__3 = 3;
static integer c__256 = 256;

/* $Procedure      ZZEKTR23 ( EK tree, 2-3 split ) */
/* Subroutine */ int zzektr23_(integer *handle, integer *tree, integer *left, 
	integer *right, integer *parent, integer *pkidx, logical *overfl)
{
    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer base, lsib, rsib, root;
    extern /* Subroutine */ int zzekpgal_(integer *, integer *, integer *, 
	    integer *), zzekpgri_(integer *, integer *, integer *), zzekpgwi_(
	    integer *, integer *, integer *);
    extern integer zzektrbs_(integer *);
    integer i__, ppage[256], rbase;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer nnode;
    extern /* Subroutine */ int movei_(integer *, integer *, integer *);
    integer lsize, msize, rsize, c1page[256], c2page[256], c3page[256], 
	    datbas, kidbas, ldelta;
    extern /* Subroutine */ int cleari_(integer *, integer *), dasrdi_(
	    integer *, integer *, integer *, integer *), dasudi_(integer *, 
	    integer *, integer *, integer *);
    integer rdelta, keybas, lshift;
    extern /* Subroutine */ int sigerr_(char *, ftnlen);
    integer lnmove, nlkeys, npkeys, nrkeys, ltrsiz, rnmove, rshift;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen), chkout_(char *, ftnlen);
    integer new__, sum;

/* $ Abstract */

/*     Execute a 2-3 split:  split two sibling nodes into three nodes, */
/*     each one approximately 2/3 full. */

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


/*     Include Section:  EK Data Types */

/*        ektype.inc Version 1  27-DEC-1994 (NJB) */


/*     Within the EK system, data types of EK column contents are */
/*     represented by integer codes.  The codes and their meanings */
/*     are listed below. */

/*     Integer codes are also used within the DAS system to indicate */
/*     data types; the EK system makes no assumptions about compatibility */
/*     between the codes used here and those used in the DAS system. */


/*     Character type: */


/*     Double precision type: */


/*     Integer type: */


/*     `Time' type: */

/*     Within the EK system, time values are represented as ephemeris */
/*     seconds past J2000 (TDB), and double precision numbers are used */
/*     to store these values.  However, since time values require special */
/*     treatment both on input and output, and since the `TIME' column */
/*     has a special role in the EK specification and code, time values */
/*     are identified as a type distinct from double precision numbers. */


/*     End Include Section:  EK Data Types */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   File handle. */
/*     TREE       I   Root of tree. */
/*     LEFT       I   Left sibling node. */
/*     RIGHT      I   Right sibling node. */
/*     PARENT     I   Common parent node. */
/*     PKIDX      I   Node-relative index of parent key. */
/*     OVERFL     O   Overflow flag. */

/* $ Detailed_Input */

/*     HANDLE         is a file handle of an EK open for write access. */

/*     TREE           is the root node number of the tree of interest. */

/*     LEFT           is the node number of the left node of a pair of */
/*                    siblings.  LEFT is either full or overflowing by */
/*                    one key. */

/*     RIGHT          is the node number of the right node of a pair of */
/*                    siblings.  The total number of keys in nodes */
/*                    LEFT and RIGHT amounts to an overflow of 1 key. */

/*     PARENT         is the node number of the common parent of LEFT */
/*                    and RIGHT. */

/*     PKIDX          is the node-relative index in PARENT of the key */
/*                    that sits between nodes LEFT and RIGHT. */

/* $ Detailed_Output */

/*     OVERFL         is a logical flag indicating whether the parent */
/*                    node overflowed as a result of the 2-3 split. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If HANDLE is invalid, the error will be diagnosed by routines */
/*         called by this routine.  The file will not be modified. */

/*     2)  If an I/O error occurs while reading the indicated file, the */
/*         error will be diagnosed by routines called by this routine. */

/*     3)  If LEFT and RIGHT are not neighboring siblings, the error */
/*         SPICE(BUG) is signalled. */

/*     4)  If either LEFT or RIGHT are not children of PARENT, the error */
/*         SPICE(BUG) is signalled. */

/*     5)  If the sum of the number of keys in LEFT and RIGHT does not */
/*         correspond to an overflow of exactly 1 key, the error */
/*         SPICE(BUG) is signalled. */

/* $ Files */

/*     See the EK Required Reading for a discussion of the EK file */
/*     format. */

/* $ Particulars */

/*     Insertions into an EK tree start at a leaf node.  If the node */
/*     overflows, the EK system attempts to shuffle keys at the leaf */
/*     level to resolve the overflow.  That attempt failing, the system */
/*     delegates the problem upward to the next higher level. */

/*     There are two ways to resolve overflow in a non-root node: */
/*     balance the overflowing node with one of its closest siblings */
/*     (see ZZEKTRBN), or if the closest siblings are full, execute a 2-3 */
/*     split. */

/*     A 2-3 split involves creation of a new sibling node between two */
/*     siblings, one of which is full and one of which contains one */
/*     excess key, and redistributing the keys between the three nodes */
/*     and their common parent so that each of the three siblings is */
/*     approximately two-thirds full.  The parent gains a key in the */
/*     process. */

/*     After the 2-3 split, the tree is balanced and the siblings */
/*     satisfy the key count invariants.  However, the parent of the */
/*     siblings may overflow by one key. */

/*     Below are the gory details concerning the actions of this routine. */
/*     All of the parameters referred to here (in capital letters) are */
/*     defined in the include file ektree.inc. */

/*     In a 2-3 split: */

/*        - The leftmost (2*MXKEYC)/3 keys of the left child remain in */
/*          that child. */

/*        - The rest of the keys in the left child node are rotated */
/*          through the parent into the middle child.  The last of these */
/*          rotated keys remains in the parent.  The others become the */
/*          leftmost keys of the middle child. */

/*        - The data values associated with the rotated keys of the */
/*          left child are moved along with the keys. */

/*        - All but the leftmost of the left child pointers associated */
/*          with the rotated keys of the left child are moved along with */
/*          the keys.  The leftmost of these pointers remains in the left */
/*          child node. */

/*        - The right child pointers associated with the rotated keys */
/*          of the left child node move along with the keys, except for */
/*          the right child pointer of the leftmost key of the rotated */
/*          set.  This leftmost key ends up in the parent, but its right */
/*          child pointer becomes the leftmost left child pointer of the */
/*          center sibling. */

/*        - The key from the left child node that is rotated into the */
/*          parent loses both of its original child pointers; these */
/*          are replaced by pointers to the left and center siblings. */

/*        - The parent key that originally sat between the left and */
/*          right siblings is moved down into the center sibling, along */
/*          with its data value.  It becomes the immediate successor of */
/*          the set of nodes rotated into the center from the left child. */

/*        - The actions taken to rotate keys from the right child are */
/*          basically symmetric with those that apply to the left child, */
/*          except that the number of keys left in the right node is */
/*          (2*MXKEYC+2)/3, and these keys are shifted to the left side */
/*          of the right node.  The rightmost key of the rotated set */
/*          contributed by the right child is placed in the parent as */
/*          the successor of the key moved into the parent from the left */
/*          child.  The rest of the rotated set become successors of */
/*          the key moved into the middle child from the parent. */

/*        - The middle child ends up with (2*MXKEYC+1)/3 keys.  This */
/*          may be deduced from the facts that the original two children */
/*          had between them an overflow of one key, the parent gained */
/*          a key, and the expression */

/*             2*MXKEYC     2*MXKEYC+1     2*MXKEYC+2 */
/*             --------  +  ----------  +  ---------- */
/*                 3             3             3 */

/*          where integer division is performed, yields one less than */
/*          the same expression when real division is performed (since */
/*          exactly one of the numerators is a multiple of 3).  So the */
/*          above expression evaluates to */

/*             2*MXKEYC */

/*          which is exactly one less than the number of keys in the */
/*          original two siblings. */

/*          Since */

/*             MNKEYC  =  MNKIDC - 1 */
/*                     =  (  ( 2*MXKIDC + 1 ) / 3   )  -  1 */
/*                     =  ( 2*MXKIDC - 2 ) / 3 */
/*                     =  ( 2*MXKEYC ) / 3 */

/*          we see that the smallest of the new child nodes has at */
/*          least the minimum allowed number of keys.  The constraint */
/*          on the maximum is met as well, since the maximum is */
/*          approximately 3/2 times the minimum, and the minimum is */
/*          approximately 40. */


/*     As the above description shows, the parent gains a key as a */
/*     result of a 2-3 split.  This may cause the parent to overflow; */
/*     if it does, the overflow at the parent's level must be resolved. */

/* $ Examples */

/*     See ZZEKTRIN. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    Beta Version 1.0.0, 16-NOV-1995 (NJB) */

/* -& */

/*     Non-SPICELIB functions */


/*     Local variables */


/*     Use discovery check-in for speed. */

/*     The plan is to take two sibling nodes, one of which is full and */
/*     one of which is overflowing by 1 key, and to split off about */
/*     one third of the keys from each one into a new node.  The new */
/*     node will be a child of the common parent of the input nodes and */
/*     will be inserted between them. */

/*     After the split, the sum of the numbers of keys in the three */
/*     children will be exactly 2*MXKEYC.  The numbers of keys in the */
/*     left, middle, and right nodes will be, respectively: */

    lsize = 41;
    msize = 41;
    rsize = 42;

/*     Note that exactly one of the numerators above is a multiple of 3, */
/*     so the sum of the above numbers is 1 less than if real division */
/*     were performed.  Therefore, the sum of the numbers of keys in the */
/*     child nodes is 2*MXKEYC.  The parent will contain one more node */
/*     than it did before the split:  the key originally between LEFT and */
/*     RIGHT will be moved down into the middle child, and the */
/*     smallest key moved from LEFT and the largest key moved from RIGHT */
/*     will go into PARENT. */

    zzekpgri_(handle, left, c1page);
    zzekpgri_(handle, right, c2page);
    zzekpgri_(handle, parent, ppage);

/*     The actual addresses in the parent node depend on whether the */
/*     parent is the root.  Compute the necessary bases to avoid a lot */
/*     of cases. */

    root = *tree;
    if (*parent == root) {
	keybas = 5;
	datbas = 172;
	kidbas = 88;
    } else {
	keybas = 1;
	datbas = 128;
	kidbas = 64;
    }

/*     Verify that LEFT and RIGHT are siblings, and that PARENT is */
/*     their common parent. */

    lsib = ppage[(i__1 = kidbas + *pkidx - 1) < 256 && 0 <= i__1 ? i__1 : 
	    s_rnge("ppage", i__1, "zzektr23_", (ftnlen)344)];
    rsib = ppage[(i__1 = kidbas + *pkidx) < 256 && 0 <= i__1 ? i__1 : s_rnge(
	    "ppage", i__1, "zzektr23_", (ftnlen)345)];
    if (lsib != *left || rsib != *right) {
	chkin_("ZZEKTR23", (ftnlen)8);
	setmsg_("LEFT, RIGHT, PARENT, and PKIDX are inconsistent. LEFT = #; "
		"RIGHT = #; PARENT = #; PKIDX = #; LSIB derived from PARENT ="
		" #; RSIB = #.", (ftnlen)132);
	errint_("#", left, (ftnlen)1);
	errint_("#", right, (ftnlen)1);
	errint_("#", parent, (ftnlen)1);
	errint_("#", pkidx, (ftnlen)1);
	errint_("#", &lsib, (ftnlen)1);
	errint_("#", &rsib, (ftnlen)1);
	sigerr_("SPICE(BUG)", (ftnlen)10);
	chkout_("ZZEKTR23", (ftnlen)8);
	return 0;
    }
    nlkeys = c1page[0];
    nrkeys = c2page[0];
    sum = nlkeys + nrkeys;

/*     The sum of the number of keys in the two input nodes must */
/*     sum exactly to the value representing an overflow level of 1 key. */

    if (sum != 125) {
	chkin_("ZZEKTR23", (ftnlen)8);
	setmsg_("Number of keys in LEFT = #; number of keys in right = #; bu"
		"t sum should be #.", (ftnlen)77);
	errint_("#", left, (ftnlen)1);
	errint_("#", right, (ftnlen)1);
	errint_("#", &c__125, (ftnlen)1);
	sigerr_("SPICE(BUG)", (ftnlen)10);
	chkout_("ZZEKTR23", (ftnlen)8);
	return 0;
    }

/*     Allocate a new page.  This page will become the right sibling */
/*     of LEFT and the left sibling of RIGHT. */

    zzekpgal_(handle, &c__3, &new__, &base);
    cleari_(&c__256, c3page);

/*     It's time to set up the keys in the middle child.  First, we'll */
/*     take the last LSHIFT keys from the left node, where */

    lshift = nlkeys - (lsize + 1);

/*     When these keys are moved, they lose LDELTA predecessors, where */
/*     LDELTA is the size of the key set preceding and including the key */
/*     at location LSIZE + 1.  The size of this subtree is just the */
/*     key value at location LSIZE+1. */

    ldelta = c1page[(i__1 = lsize + 1) < 256 && 0 <= i__1 ? i__1 : s_rnge(
	    "c1page", i__1, "zzektr23_", (ftnlen)407)];
    i__1 = lshift;
    for (i__ = 1; i__ <= i__1; ++i__) {
	c3page[(i__2 = i__) < 256 && 0 <= i__2 ? i__2 : s_rnge("c3page", i__2,
		 "zzektr23_", (ftnlen)410)] = c1page[(i__3 = lsize + 2 + i__ 
		- 1) < 256 && 0 <= i__3 ? i__3 : s_rnge("c1page", i__3, "zze"
		"ktr23_", (ftnlen)410)] - ldelta;
    }
    movei_(&c1page[(i__1 = lsize + 129) < 256 && 0 <= i__1 ? i__1 : s_rnge(
	    "c1page", i__1, "zzektr23_", (ftnlen)413)], &lshift, &c3page[128])
	    ;
    i__2 = lshift + 1;
    movei_(&c1page[(i__1 = lsize + 65) < 256 && 0 <= i__1 ? i__1 : s_rnge(
	    "c1page", i__1, "zzektr23_", (ftnlen)414)], &i__2, &c3page[64]);

/*     Compute the size of the tree headed by the left subnode.  We'll */
/*     need this shortly.  The size of this tree is one less than the */
/*     difference of the parent key and its predecessor, if any. */

    if (*pkidx == 1) {
	ltrsiz = ppage[(i__1 = keybas) < 256 && 0 <= i__1 ? i__1 : s_rnge(
		"ppage", i__1, "zzektr23_", (ftnlen)424)] - 1;
    } else {
	ltrsiz = ppage[(i__1 = keybas + *pkidx - 1) < 256 && 0 <= i__1 ? i__1 
		: s_rnge("ppage", i__1, "zzektr23_", (ftnlen)426)] - ppage[(
		i__2 = keybas + *pkidx - 2) < 256 && 0 <= i__2 ? i__2 : 
		s_rnge("ppage", i__2, "zzektr23_", (ftnlen)426)] - 1;
    }

/*     The next item to add to the middle child is the middle key */
/*     from the parent.  The data pointer is copied; the key value is */
/*     simply set.  The value of the key is one more than the size of */
/*     the entire key set (including descendants) we moved into the */
/*     middle from the left.  LNMOVE is the size of this key set. */

/*     No child pointer is copied. */

    lnmove = ltrsiz - ldelta;
    c3page[(i__1 = lshift + 1) < 256 && 0 <= i__1 ? i__1 : s_rnge("c3page", 
	    i__1, "zzektr23_", (ftnlen)439)] = lnmove + 1;
    c3page[(i__1 = lshift + 128) < 256 && 0 <= i__1 ? i__1 : s_rnge("c3page", 
	    i__1, "zzektr23_", (ftnlen)442)] = ppage[(i__2 = datbas + *pkidx 
	    - 1) < 256 && 0 <= i__2 ? i__2 : s_rnge("ppage", i__2, "zzektr23_"
	    , (ftnlen)442)];

/*     Now we copy keys from the right child into the middle.  We'll */
/*     take the first RSHIFT keys from the right node, where */

    rshift = nrkeys - (rsize + 1);

/*     When these keys are moved, they gain RDELTA predecessors, where */
/*     RDELTA is the size of the key set already in the middle node. */

    rdelta = lnmove + 1;
    i__1 = rshift;
    for (i__ = 1; i__ <= i__1; ++i__) {
	c3page[(i__2 = lshift + 2 + i__ - 1) < 256 && 0 <= i__2 ? i__2 : 
		s_rnge("c3page", i__2, "zzektr23_", (ftnlen)457)] = c2page[(
		i__3 = i__) < 256 && 0 <= i__3 ? i__3 : s_rnge("c2page", i__3,
		 "zzektr23_", (ftnlen)457)] + rdelta;
    }
    movei_(&c2page[128], &rshift, &c3page[(i__1 = lshift + 129) < 256 && 0 <= 
	    i__1 ? i__1 : s_rnge("c3page", i__1, "zzektr23_", (ftnlen)460)]);
    i__2 = rshift + 1;
    movei_(&c2page[64], &i__2, &c3page[(i__1 = lshift + 65) < 256 && 0 <= 
	    i__1 ? i__1 : s_rnge("c3page", i__1, "zzektr23_", (ftnlen)461)]);

/*     Save the size of the entire key set moved into the middle from */
/*     the right. */

    rnmove = c2page[(i__1 = rshift + 1) < 256 && 0 <= i__1 ? i__1 : s_rnge(
	    "c2page", i__1, "zzektr23_", (ftnlen)467)] - 1;

/*     Set the key count in the new child. */

    c3page[0] = msize;

/*     The middle child is complete. */

/*     The next step is to set up the parent node.  The original parent */
/*     key at index PKIDX is replaced by the key from the left child */
/*     at location LSIZE + 1.  The following parent keys are shifted */
/*     right by one location, making room for a second key following */
/*     the one at PKIDX.  This newly freed slot is filled in with the */
/*     key at location RSHIFT+1 in the right child. */

/*     The keys in the parent to the right of position PKIDX+1 gain no */
/*     predecessors as the result of these re-arrangements. */

/*     Get the number of keys in the parent. */

    if (*parent == root) {
	npkeys = ppage[4];
    } else {
	npkeys = ppage[0];
    }

/*     Make room for the new key.  Shift elements starting from the */
/*     right. */

    i__1 = *pkidx + 1;
    for (i__ = npkeys; i__ >= i__1; --i__) {
	ppage[(i__2 = keybas + i__) < 256 && 0 <= i__2 ? i__2 : s_rnge("ppage"
		, i__2, "zzektr23_", (ftnlen)500)] = ppage[(i__3 = keybas + 
		i__ - 1) < 256 && 0 <= i__3 ? i__3 : s_rnge("ppage", i__3, 
		"zzektr23_", (ftnlen)500)];
    }
    i__1 = *pkidx + 1;
    for (i__ = npkeys; i__ >= i__1; --i__) {
	ppage[(i__2 = datbas + i__) < 256 && 0 <= i__2 ? i__2 : s_rnge("ppage"
		, i__2, "zzektr23_", (ftnlen)504)] = ppage[(i__3 = datbas + 
		i__ - 1) < 256 && 0 <= i__3 ? i__3 : s_rnge("ppage", i__3, 
		"zzektr23_", (ftnlen)504)];
    }
    i__1 = *pkidx + 1;
    for (i__ = npkeys + 1; i__ >= i__1; --i__) {
	ppage[(i__2 = kidbas + i__) < 256 && 0 <= i__2 ? i__2 : s_rnge("ppage"
		, i__2, "zzektr23_", (ftnlen)508)] = ppage[(i__3 = kidbas + 
		i__ - 1) < 256 && 0 <= i__3 ? i__3 : s_rnge("ppage", i__3, 
		"zzektr23_", (ftnlen)508)];
    }

/*     Copy in the data pointer from the left child.  Note that */
/*     no child pointer comes along. */

    ppage[(i__1 = datbas + *pkidx - 1) < 256 && 0 <= i__1 ? i__1 : s_rnge(
	    "ppage", i__1, "zzektr23_", (ftnlen)515)] = c1page[(i__2 = lsize 
	    + 128) < 256 && 0 <= i__2 ? i__2 : s_rnge("c1page", i__2, "zzekt"
	    "r23_", (ftnlen)515)];

/*     Set the key value at PKIDX.  The value exceeds that of the */
/*     preceding key, if any, by one more than the size of the subtree */
/*     headed by the left child.  That size is one less than */
/*     LDELTA, since LDELTA includes the key at location LSIZE+1. */

    if (*pkidx == 1) {
	ppage[(i__1 = keybas + *pkidx - 1) < 256 && 0 <= i__1 ? i__1 : s_rnge(
		"ppage", i__1, "zzektr23_", (ftnlen)524)] = ldelta;
    } else {
	ppage[(i__1 = keybas + *pkidx - 1) < 256 && 0 <= i__1 ? i__1 : s_rnge(
		"ppage", i__1, "zzektr23_", (ftnlen)526)] = ppage[(i__2 = 
		keybas + *pkidx - 2) < 256 && 0 <= i__2 ? i__2 : s_rnge("ppa"
		"ge", i__2, "zzektr23_", (ftnlen)526)] + ldelta;
    }

/*     Copy in the data pointer from the right child.  Again, note that */
/*     no child pointer comes along. */

    ppage[(i__1 = datbas + *pkidx) < 256 && 0 <= i__1 ? i__1 : s_rnge("ppage",
	     i__1, "zzektr23_", (ftnlen)533)] = c2page[(i__2 = rshift + 128) <
	     256 && 0 <= i__2 ? i__2 : s_rnge("c2page", i__2, "zzektr23_", (
	    ftnlen)533)];

/*     Set the key value at PKIDX+1.  The value exceeds that of the */
/*     preceding key by one more than the size of the subtree headed by */
/*     the middle child. */

    ppage[(i__1 = keybas + *pkidx) < 256 && 0 <= i__1 ? i__1 : s_rnge("ppage",
	     i__1, "zzektr23_", (ftnlen)540)] = ppage[(i__2 = keybas + *pkidx 
	    - 1) < 256 && 0 <= i__2 ? i__2 : s_rnge("ppage", i__2, "zzektr23_"
	    , (ftnlen)540)] + lnmove + rnmove + 2;

/*     The child pointer at PKIDX+1 does get set:  it points to the */
/*     middle child. */

    ppage[(i__1 = kidbas + *pkidx) < 256 && 0 <= i__1 ? i__1 : s_rnge("ppage",
	     i__1, "zzektr23_", (ftnlen)549)] = new__;

/*     Remarkably, the only required change to the parent's metadata is */
/*     updating the key count.  At this point, we can set the overflow */
/*     flag, depending on the status of the parent. */

    if (*parent == root) {
	++ppage[4];
	*overfl = ppage[4] == 83;
    } else {
	++ppage[0];
	*overfl = ppage[0] == 63;
    }

/*     Update the metadata in the first child.  This node has lost */
/*     just enough keys to give it size LSIZE. */

    c1page[0] = lsize;

/*     For safety, clean out the vacated key and pointer locations. */
/*     Clear the overflow addresses as well. */

    i__2 = 63 - lsize;
    cleari_(&i__2, &c1page[(i__1 = lsize + 1) < 256 && 0 <= i__1 ? i__1 : 
	    s_rnge("c1page", i__1, "zzektr23_", (ftnlen)578)]);
    i__2 = 63 - lsize;
    cleari_(&i__2, &c1page[(i__1 = lsize + 128) < 256 && 0 <= i__1 ? i__1 : 
	    s_rnge("c1page", i__1, "zzektr23_", (ftnlen)579)]);
    i__2 = 64 - (lsize + 1);
    cleari_(&i__2, &c1page[(i__1 = lsize + 65) < 256 && 0 <= i__1 ? i__1 : 
	    s_rnge("c1page", i__1, "zzektr23_", (ftnlen)580)]);

/*     The first child is set. */

/*     To adjust the second child, we must shift the keys and pointers */
/*     left to fill in the vacated space.  The keys in this second child */
/*     must be adjusted to account for the loss of the predecessors */
/*     moved to the middle child and the parent. */

/*     Shift elements starting from the left. */

    i__1 = rsize;
    for (i__ = 1; i__ <= i__1; ++i__) {
	c2page[(i__2 = i__) < 256 && 0 <= i__2 ? i__2 : s_rnge("c2page", i__2,
		 "zzektr23_", (ftnlen)593)] = c2page[(i__3 = rshift + 2 + i__ 
		- 1) < 256 && 0 <= i__3 ? i__3 : s_rnge("c2page", i__3, "zze"
		"ktr23_", (ftnlen)593)] - (rnmove + 1);
    }
    i__1 = rsize;
    for (i__ = 1; i__ <= i__1; ++i__) {
	c2page[(i__2 = i__ + 127) < 256 && 0 <= i__2 ? i__2 : s_rnge("c2page",
		 i__2, "zzektr23_", (ftnlen)597)] = c2page[(i__3 = rshift + 
		129 + i__ - 1) < 256 && 0 <= i__3 ? i__3 : s_rnge("c2page", 
		i__3, "zzektr23_", (ftnlen)597)];
    }
    i__1 = rsize + 1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	c2page[(i__2 = i__ + 63) < 256 && 0 <= i__2 ? i__2 : s_rnge("c2page", 
		i__2, "zzektr23_", (ftnlen)601)] = c2page[(i__3 = rshift + 65 
		+ i__ - 1) < 256 && 0 <= i__3 ? i__3 : s_rnge("c2page", i__3, 
		"zzektr23_", (ftnlen)601)];
    }

/*     Update the key count in the second child.  This node has lost */
/*     just enough keys to give it size RSIZE. */

    c2page[0] = rsize;

/*     For safety, clean out the vacated key and pointer locations. */
/*     Clear the overflow addresses as well. */

    i__2 = 63 - rsize;
    cleari_(&i__2, &c2page[(i__1 = rsize + 1) < 256 && 0 <= i__1 ? i__1 : 
	    s_rnge("c2page", i__1, "zzektr23_", (ftnlen)614)]);
    i__2 = 63 - rsize;
    cleari_(&i__2, &c2page[(i__1 = rsize + 128) < 256 && 0 <= i__1 ? i__1 : 
	    s_rnge("c2page", i__1, "zzektr23_", (ftnlen)615)]);
    i__2 = 64 - (rsize + 1);
    cleari_(&i__2, &c2page[(i__1 = rsize + 65) < 256 && 0 <= i__1 ? i__1 : 
	    s_rnge("c2page", i__1, "zzektr23_", (ftnlen)616)]);

/*     The second child is set. */

/*     The last change we must make is to update the node count in */
/*     the root. */

    if (*parent == root) {
	++ppage[1];
    } else {

/*        We won't read in the whole root page; we'll just get the */
/*        base address of the root and update the affected location. */

	rbase = zzektrbs_(&root);
	i__1 = rbase + 2;
	i__2 = rbase + 2;
	dasrdi_(handle, &i__1, &i__2, &nnode);
	i__1 = rbase + 2;
	i__2 = rbase + 2;
	i__3 = nnode + 1;
	dasudi_(handle, &i__1, &i__2, &i__3);
    }

/*     Write out our updates. */

    zzekpgwi_(handle, parent, ppage);
    zzekpgwi_(handle, left, c1page);
    zzekpgwi_(handle, right, c2page);
    zzekpgwi_(handle, &new__, c3page);
    return 0;
} /* zzektr23_ */

