/* zzektrrk.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__40 = 40;
static integer c__63 = 63;

/* $Procedure      ZZEKTRRK ( EK tree, rotate keys ) */
/* Subroutine */ int zzektrrk_(integer *handle, integer *tree, integer *left, 
	integer *right, integer *parent, integer *pkidx, integer *nrot)
{
    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer dpar, lsib, rsib, root;
    extern /* Subroutine */ int zzekpgri_(integer *, integer *, integer *), 
	    zzekpgwi_(integer *, integer *, integer *);
    integer i__, lpage[256], ppage[256], rpage[256];
    extern /* Subroutine */ int chkin_(char *, ftnlen), movei_(integer *, 
	    integer *, integer *);
    extern logical failed_(void);
    integer datbas, kidbas, remain, keybas, dshift, schlep;
    extern /* Subroutine */ int sigerr_(char *, ftnlen);
    integer drotat, futrpk, lnkeys, lnsize, nvopar, rnkeys, subsiz;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), chkout_(char *, 
	    ftnlen), errint_(char *, integer *, ftnlen);

/* $ Abstract */

/*     Rotate a specified number of keys from one node, through */
/*     the parent, into a neighboring sibling node. */

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

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   File handle. */
/*     TREE       I   Root of tree. */
/*     LEFT       I   Left node of pair to participate in rotation. */
/*     RIGHT      I   Right node of pair to participate in rotation. */
/*     PARENT     I   Parent node of pair to participate in rotation. */
/*     PKIDX      I   Parent key index. */
/*     NROT       I   Number of keys to rotate. */

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

/*     NROT           is the number of keys to rotate.  Positive counts */
/*                    indicate that keys are to be rotated from node */
/*                    LEFT to node RIGHT; negative counts indicate */
/*                    rotation in the reverse direction. */

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

/*     5)  The rotation is not allowed to create an overflow of more */
/*         than one key in the destination node, not an underflow of */
/*         more than one key in the source node.  If either restriction */
/*         is violated, the error SPICE(BUG) is signaled. */

/* $ Files */

/*     See the EK Required Reading for a discussion of the EK file */
/*     format. */

/* $ Particulars */

/*     Insertions into and deletions from EK trees can result in */
/*     overflows or underflows of keys in nodes affected by these */
/*     operations.  Many times key count invariants can be restored by */
/*     moving keys from one node into an adjacent sibling node.  This */
/*     maneuver is called `balancing' the nodes.  The process of moving */
/*     keys from one node, through the parent, into a neighboring */
/*     sibling node is called `rotating' the keys. */

/*     Key rotation affects the parent node of the neighboring children */
/*     because one key of the parent sits between the children.  This */
/*     `parent key' gets moved into one of the children as keys are */
/*     rotated.  If the rotation is to the right, the parent key is the */
/*     largest key of the rotated set; if the rotation is to the left, */
/*     the parent key is the least of the rotated set. */

/*     When keys are rotated, their data values move along with them. */
/*     In general, child pointers move along with keys, but there are */
/*     some tricky points: */

/*        - The left and right child pointers of the parent key don't */
/*          get updated; they continue to point to the two children */
/*          LEFT and RIGHT. */

/*        - On a right rotation, the right child pointer of the key that */
/*          gets moved into the parent key's original position becomes */
/*          the first left child pointer of the right sibling.  The left */
/*          child pointer of this key doesn't get moved at all. */

/*        - On a left rotation, the left child pointer of the key that */
/*          gets moved into the parent key's original position becomes */
/*          the last right child pointer of the left sibling.  The right */
/*          child pointer of this key becomes the left child pointer of */
/*          the first key of RIGHT. */

/* $ Examples */

/*     See ZZEKTRBN. */

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

/* -    Beta Version 1.0.0, 16-NOV-1995 (NJB) */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Use discovery check-in for speed. */

    if (*nrot == 0) {
	return 0;
    }
    root = *tree;
    if (*left == root || *right == root) {
	chkin_("ZZEKTRRK", (ftnlen)8);
	setmsg_("Input node is root; only children are eligible for key rota"
		"tion.", (ftnlen)64);
	sigerr_("SPICE(BUG)", (ftnlen)10);
	chkout_("ZZEKTRRK", (ftnlen)8);
    }

/*     Read in the input nodes. */

    zzekpgri_(handle, left, lpage);
    zzekpgri_(handle, right, rpage);
    zzekpgri_(handle, parent, ppage);
    if (failed_()) {
	return 0;
    }

/*     Set the base index of the parent keys.  This value depends on */
/*     whether the parent is the root.  Do the same for the pointer */
/*     bases. */

    if (*parent == *tree) {
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
	    s_rnge("ppage", i__1, "zzektrrk_", (ftnlen)280)];
    rsib = ppage[(i__1 = kidbas + *pkidx) < 256 && 0 <= i__1 ? i__1 : s_rnge(
	    "ppage", i__1, "zzektrrk_", (ftnlen)281)];
    if (lsib != *left || rsib != *right) {
	chkin_("ZZEKTRRK", (ftnlen)8);
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
	chkout_("ZZEKTRRK", (ftnlen)8);
	return 0;
    }

/*     Get the key counts for the left and right nodes. */

    lnkeys = lpage[0];
    rnkeys = rpage[0];

/*     The requested rotation will not be permitted to cause an */
/*     underflow of more than one key in the source node, nor an */
/*     overflow of more than one key in the destination node. */

    if (*nrot > 0) {
	if (lnkeys - *nrot < 40 || rnkeys + *nrot > 63) {
	    chkin_("ZZEKTRRK", (ftnlen)8);
	    setmsg_("Node # and right sibling # contain # and # keys respect"
		    "ively; rotation of # keys to the right will violate the "
		    "key count bounds of #:#.", (ftnlen)135);
	    errint_("#", left, (ftnlen)1);
	    errint_("#", right, (ftnlen)1);
	    errint_("#", &lnkeys, (ftnlen)1);
	    errint_("#", &rnkeys, (ftnlen)1);
	    errint_("#", nrot, (ftnlen)1);
	    errint_("#", &c__40, (ftnlen)1);
	    errint_("#", &c__63, (ftnlen)1);
	    sigerr_("SPICE(BUG)", (ftnlen)10);
	    chkout_("ZZEKTRRK", (ftnlen)8);
	    return 0;
	}
    } else if (*nrot < 0) {
	if (lnkeys - *nrot > 63 || rnkeys + *nrot < 40) {
	    chkin_("ZZEKTRRK", (ftnlen)8);
	    setmsg_("Node # and right sibling # contain # and # keys respect"
		    "ively; rotation of # keys to the left will violate the k"
		    "ey count bounds of #:#.", (ftnlen)134);
	    errint_("#", left, (ftnlen)1);
	    errint_("#", right, (ftnlen)1);
	    errint_("#", &lnkeys, (ftnlen)1);
	    errint_("#", &rnkeys, (ftnlen)1);
	    i__1 = -(*nrot);
	    errint_("#", &i__1, (ftnlen)1);
	    errint_("#", &c__40, (ftnlen)1);
	    errint_("#", &c__63, (ftnlen)1);
	    sigerr_("SPICE(BUG)", (ftnlen)10);
	    chkout_("ZZEKTRRK", (ftnlen)8);
	    return 0;
	}
    }

/*     Compute the size of the tree headed by the left subnode.  We'll */
/*     need this later.  The size of this tree is one less than the */
/*     difference of the parent key and its predecessor, if any. */

    if (*pkidx == 1) {
	lnsize = ppage[(i__1 = keybas) < 256 && 0 <= i__1 ? i__1 : s_rnge(
		"ppage", i__1, "zzektrrk_", (ftnlen)368)] - 1;
    } else {
	lnsize = ppage[(i__1 = keybas + *pkidx - 1) < 256 && 0 <= i__1 ? i__1 
		: s_rnge("ppage", i__1, "zzektrrk_", (ftnlen)370)] - ppage[(
		i__2 = keybas + *pkidx - 2) < 256 && 0 <= i__2 ? i__2 : 
		s_rnge("ppage", i__2, "zzektrrk_", (ftnlen)370)] - 1;
    }

/*     Now, the actions we take depend on whether we must schlep keys */
/*     to the right or left. */

    if (*nrot > 0) {

/*        We'll rotate keys to the right.  There are a bunch of numbers */
/*        to compute first: */

/*           -- The number of keys remaining in the input node:  REMAIN */

/*           -- The size of the subtree headed by the */
/*              rotated keys:  SUBSIZ */

/*           -- The offset delta to be applied to the rotated */
/*              keys:  DROTAT */

/*           -- The offset delta to be applied to the keys shifted */
/*              right in the sibling:  DSHIFT */

/*           -- The new value of the old right parent key, */
/*              which gets rotated into the sibling:  NVOPAR */

/*           -- The offset delta to apply to the new right parent key, */
/*              DPAR.  Note that the successors of this key in the */
/*              parent node remain unchanged. */


	schlep = *nrot;
	remain = lnkeys - schlep;

/*        The size of the rotated subtree is the original size of the */
/*        subtree headed by LEFT, minus the value of the key preceding */
/*        the rotated subtree.  That key, which resides at location */
/*        REMAIN + 1, is the future right parent key; this key is also */
/*        the successor of the subtree left behind. */

	futrpk = lpage[(i__1 = remain + 1) < 256 && 0 <= i__1 ? i__1 : s_rnge(
		"lpage", i__1, "zzektrrk_", (ftnlen)411)];
	subsiz = lnsize - futrpk;

/*        The rotated set of keys will no longer be preceded by the */
/*        set of keys of size NEWRPK that they originally followed. */

	drotat = -futrpk;

/*        The shifted keys in the right sibling get SUBSIZ + 1 new */
/*        predecessors. */

	dshift = subsiz + 1;

/*        The old right parent key will become the successor of the */
/*        shifted subtree.  Its value is just one greater than the */
/*        size of this subtree. */

	nvopar = dshift;

/*        The new parent key has DSHIFT fewer predecessors after */
/*        the rotation. */

	dpar = -dshift;

/*        It's time for some action.  First of all, shift the keys */
/*        in the sibling to the right.  Their data pointers and child */
/*        pointers move along with them.  Update all the keys by */
/*        applying the shift delta to them. */

/*        Move the rightmost elements of each data component first. */
/*        Adjust the keys at the same time.  Note that the regions */
/*        allocated to keys, data pointers, and child pointers occupy */
/*        non-overlapping addresses, so the order in which we shift */
/*        these data sets is not important.  Within each data set, we */
/*        must be careful not to trash occupied addresses. */

	for (i__ = rnkeys; i__ >= 1; --i__) {
	    rpage[(i__1 = i__ + 1 + schlep - 1) < 256 && 0 <= i__1 ? i__1 : 
		    s_rnge("rpage", i__1, "zzektrrk_", (ftnlen)453)] = rpage[(
		    i__2 = i__) < 256 && 0 <= i__2 ? i__2 : s_rnge("rpage", 
		    i__2, "zzektrrk_", (ftnlen)453)] + dshift;
	}
	for (i__ = rnkeys; i__ >= 1; --i__) {
	    rpage[(i__1 = i__ + 128 + schlep - 1) < 256 && 0 <= i__1 ? i__1 : 
		    s_rnge("rpage", i__1, "zzektrrk_", (ftnlen)457)] = rpage[(
		    i__2 = i__ + 127) < 256 && 0 <= i__2 ? i__2 : s_rnge(
		    "rpage", i__2, "zzektrrk_", (ftnlen)457)];
	}
	for (i__ = rnkeys + 1; i__ >= 1; --i__) {
	    rpage[(i__1 = i__ + 64 + schlep - 1) < 256 && 0 <= i__1 ? i__1 : 
		    s_rnge("rpage", i__1, "zzektrrk_", (ftnlen)461)] = rpage[(
		    i__2 = i__ + 63) < 256 && 0 <= i__2 ? i__2 : s_rnge("rpa"
		    "ge", i__2, "zzektrrk_", (ftnlen)461)];
	}

/*        `Move' the old parent key to its target destination in the */
/*        sibling.  Actually, only the data pointer is copied; the key */
/*        is simply set to its new value. */

	rpage[(i__1 = schlep) < 256 && 0 <= i__1 ? i__1 : s_rnge("rpage", 
		i__1, "zzektrrk_", (ftnlen)469)] = nvopar;
	rpage[(i__1 = schlep + 127) < 256 && 0 <= i__1 ? i__1 : s_rnge("rpage"
		, i__1, "zzektrrk_", (ftnlen)470)] = ppage[(i__2 = datbas + *
		pkidx - 1) < 256 && 0 <= i__2 ? i__2 : s_rnge("ppage", i__2, 
		"zzektrrk_", (ftnlen)470)];

/*        `Move' the future parent key to its target destination in the */
/*        parent.  The data pointer is copied; the key is adjusted by */
/*        the offset delta we've computed. */

	ppage[(i__1 = datbas + *pkidx - 1) < 256 && 0 <= i__1 ? i__1 : s_rnge(
		"ppage", i__1, "zzektrrk_", (ftnlen)477)] = lpage[(i__2 = 
		remain + 128) < 256 && 0 <= i__2 ? i__2 : s_rnge("lpage", 
		i__2, "zzektrrk_", (ftnlen)477)];
	ppage[(i__1 = keybas + *pkidx - 1) < 256 && 0 <= i__1 ? i__1 : s_rnge(
		"ppage", i__1, "zzektrrk_", (ftnlen)478)] = ppage[(i__2 = 
		keybas + *pkidx - 1) < 256 && 0 <= i__2 ? i__2 : s_rnge("ppa"
		"ge", i__2, "zzektrrk_", (ftnlen)478)] + dpar;

/*        Rotate the subtree following the future parent key to its */
/*        destination in the sibling.  Update the keys to account for */
/*        their new offset. */

	i__1 = schlep - 1;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    rpage[(i__2 = i__) < 256 && 0 <= i__2 ? i__2 : s_rnge("rpage", 
		    i__2, "zzektrrk_", (ftnlen)486)] = lpage[(i__3 = remain + 
		    2 + i__ - 1) < 256 && 0 <= i__3 ? i__3 : s_rnge("lpage", 
		    i__3, "zzektrrk_", (ftnlen)486)] + drotat;
	}
	i__2 = schlep - 1;
	movei_(&lpage[(i__1 = remain + 129) < 256 && 0 <= i__1 ? i__1 : 
		s_rnge("lpage", i__1, "zzektrrk_", (ftnlen)489)], &i__2, &
		rpage[128]);
	movei_(&lpage[(i__1 = remain + 65) < 256 && 0 <= i__1 ? i__1 : s_rnge(
		"lpage", i__1, "zzektrrk_", (ftnlen)490)], &schlep, &rpage[64]
		);

/*        Update the key counts in both the input node and sibling. */

	lpage[0] -= schlep;
	rpage[0] += schlep;

/*        Update the pages in the kernel. */

	zzekpgwi_(handle, parent, ppage);
	zzekpgwi_(handle, left, lpage);
	zzekpgwi_(handle, right, rpage);
    } else {

/*        Rotation to the left is almost, but not quite, a mirror image */
/*        of rotation to the right. */

	schlep = -(*nrot);
	remain = rnkeys - schlep;

/*        The size of the rotated subtree is one less than the value of */
/*        the future parent key.  This key resides at location */
/*        SCHLEP and is also the predecessor of the subtree */
/*        left behind. */

	futrpk = rpage[(i__1 = schlep) < 256 && 0 <= i__1 ? i__1 : s_rnge(
		"rpage", i__1, "zzektrrk_", (ftnlen)521)];
	subsiz = futrpk - 1;

/*        The rotated set of keys will be preceded by the keys already */
/*        present in LEFT, as well as the key moved in from the parent */
/*        node. */

	drotat = lnsize + 1;

/*        The shifted keys in the right sibling lose SUBSIZ + 1 */
/*        predecessors. */

	dshift = -(subsiz + 1);

/*        The old parent key will become the successor of the */
/*        keys already in LEFT; it will be the predecessor of the */
/*        rotated subtree. */

	nvopar = drotat;

/*        The new parent key has (-DSHIFT) more predecessors after */
/*        the rotation. */

	dpar = -dshift;

/*        It's time for some action. */

/*        `Move' the old parent key to its target destination in the */
/*        input node.  Actually, only the data pointer is copied; the key */
/*        is simply set to its new value. */

	lpage[(i__1 = lnkeys + 1) < 256 && 0 <= i__1 ? i__1 : s_rnge("lpage", 
		i__1, "zzektrrk_", (ftnlen)557)] = nvopar;
	lpage[(i__1 = lnkeys + 128) < 256 && 0 <= i__1 ? i__1 : s_rnge("lpage"
		, i__1, "zzektrrk_", (ftnlen)558)] = ppage[(i__2 = datbas + *
		pkidx - 1) < 256 && 0 <= i__2 ? i__2 : s_rnge("ppage", i__2, 
		"zzektrrk_", (ftnlen)558)];

/*        `Move' the future parent key to its target destination in the */
/*        parent.  The data pointer is copied; the key is adjusted by */
/*        the offset delta we've computed. */

	ppage[(i__1 = datbas + *pkidx - 1) < 256 && 0 <= i__1 ? i__1 : s_rnge(
		"ppage", i__1, "zzektrrk_", (ftnlen)565)] = rpage[(i__2 = 
		schlep + 127) < 256 && 0 <= i__2 ? i__2 : s_rnge("rpage", 
		i__2, "zzektrrk_", (ftnlen)565)];
	ppage[(i__1 = keybas + *pkidx - 1) < 256 && 0 <= i__1 ? i__1 : s_rnge(
		"ppage", i__1, "zzektrrk_", (ftnlen)566)] = ppage[(i__2 = 
		keybas + *pkidx - 1) < 256 && 0 <= i__2 ? i__2 : s_rnge("ppa"
		"ge", i__2, "zzektrrk_", (ftnlen)566)] + dpar;

/*        Rotate the subtree following the future parent key to its */
/*        destination in the sibling.  Update the keys to account for */
/*        their new offset. */

	i__2 = schlep - 1;
	movei_(&rpage[1], &i__2, &lpage[(i__1 = lnkeys + 2) < 256 && 0 <= 
		i__1 ? i__1 : s_rnge("lpage", i__1, "zzektrrk_", (ftnlen)573)]
		);
	i__2 = schlep - 1;
	movei_(&rpage[128], &i__2, &lpage[(i__1 = lnkeys + 129) < 256 && 0 <= 
		i__1 ? i__1 : s_rnge("lpage", i__1, "zzektrrk_", (ftnlen)574)]
		);
	movei_(&rpage[64], &schlep, &lpage[(i__1 = lnkeys + 65) < 256 && 0 <= 
		i__1 ? i__1 : s_rnge("lpage", i__1, "zzektrrk_", (ftnlen)575)]
		);
	i__1 = schlep - 1;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    lpage[(i__2 = lnkeys + 2 + i__ - 1) < 256 && 0 <= i__2 ? i__2 : 
		    s_rnge("lpage", i__2, "zzektrrk_", (ftnlen)578)] = lpage[(
		    i__3 = lnkeys + 2 + i__ - 1) < 256 && 0 <= i__3 ? i__3 : 
		    s_rnge("lpage", i__3, "zzektrrk_", (ftnlen)578)] + drotat;
	}

/*        Shift the remaining elements of the sibling to the left. */
/*        Their data pointers and child pointers move along with them. */
/*        Update all the keys by applying the shift delta to them. */

/*        Move the leftmost elements of each data component first. */
/*        Adjust the keys at the same time. */

	i__1 = remain;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    rpage[(i__2 = i__) < 256 && 0 <= i__2 ? i__2 : s_rnge("rpage", 
		    i__2, "zzektrrk_", (ftnlen)590)] = rpage[(i__3 = i__ + 1 
		    + schlep - 1) < 256 && 0 <= i__3 ? i__3 : s_rnge("rpage", 
		    i__3, "zzektrrk_", (ftnlen)590)] + dshift;
	}
	i__1 = remain;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    rpage[(i__2 = i__ + 127) < 256 && 0 <= i__2 ? i__2 : s_rnge("rpa"
		    "ge", i__2, "zzektrrk_", (ftnlen)594)] = rpage[(i__3 = i__ 
		    + 128 + schlep - 1) < 256 && 0 <= i__3 ? i__3 : s_rnge(
		    "rpage", i__3, "zzektrrk_", (ftnlen)594)];
	}
	i__1 = remain + 1;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    rpage[(i__2 = i__ + 63) < 256 && 0 <= i__2 ? i__2 : s_rnge("rpage"
		    , i__2, "zzektrrk_", (ftnlen)598)] = rpage[(i__3 = i__ + 
		    64 + schlep - 1) < 256 && 0 <= i__3 ? i__3 : s_rnge("rpa"
		    "ge", i__3, "zzektrrk_", (ftnlen)598)];
	}

/*        Update the key counts in both the input node and sibling. */

	lpage[0] += schlep;
	rpage[0] -= schlep;

/*        Update the pages in the kernel. */

	zzekpgwi_(handle, parent, ppage);
	zzekpgwi_(handle, left, lpage);
	zzekpgwi_(handle, right, rpage);
    }
    return 0;
} /* zzektrrk_ */

