/* zzektrud.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__63 = 63;

/* $Procedure      ZZEKTRUD ( EK tree, unbalanced deletion ) */
/* Subroutine */ int zzektrud_(integer *handle, integer *tree, integer *key, 
	integer *trgkey, logical *undrfl)
{
    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer leaf, lsib, rsib, pkey, prev, root, lsib2, rsib2;
    extern /* Subroutine */ int zzekpgri_(integer *, integer *, integer *), 
	    zzekpgwi_(integer *, integer *, integer *);
    integer pkey2;
    extern /* Subroutine */ int zzektrlk_(integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *), zzektrpi_(
	    integer *, integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, integer *);
    integer i__, lpage[256];
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer rpage[256], tpage[256], depth, level, nnode, lpidx, lpkey, rpidx, 
	    nkeys, rpkey, paren2, poffs2, lpidx2, lpkey2, rpidx2, rpkey2;
    extern logical failed_(void);
    extern /* Subroutine */ int errhan_(char *, integer *, ftnlen);
    integer target, parent;
    extern /* Subroutine */ int sigerr_(char *, ftnlen);
    integer keyidx, datptr, loffst, nlkeys, poffst, tnkeys, toffst, totkey;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen), chkout_(char *, ftnlen);

/* $ Abstract */

/*     Delete a value from a tree at a specified location without */
/*     balancing the tree. */

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
/*     KEY        I   Key to delete. */
/*     TRGKEY     O   Key identifying node from which deletion occurred. */
/*     UNDRFL     O   Underflow flag. */

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

/*     TRGKEY         is an absolute key identifying the node from which */
/*                    the deletion occurred.  This node may be different */
/*                    from the node that contained KEY before the */
/*                    deletion; see $Particulars for details. */

/*     UNDRFL         is a logical flag indicating whether the node */
/*                    at which VALUE was inserted underflowed as a */
/*                    result.  Child nodes must contain at least */
/*                    MNKEYC keys; this bound is declared in ektree.inc. */
/*                    The root node is permitted to become empty. */

/*                    When an underflow condition exists, the tree */
/*                    violates an invariant.  The underflow must be */
/*                    resolved before any other insertions or deletions */
/*                    are performed. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If HANDLE is invalid, the error will be diagnosed by routines */
/*         called by this routine.  The file will not be modified. */

/*     2)  If an I/O error occurs while reading or writing the indicated */
/*         file, the error will be diagnosed by routines called by this */
/*         routine. */

/*     3)  If the input key is out of range, the error */
/*         SPICE(INVALIDINDEX) is signaled. */

/* $ Files */

/*     See the EK Required Reading for a discussion of the EK file */
/*     format. */

/* $ Particulars */

/*     This routine deletes a value from an EK tree at the ordinal */
/*     position indicated by KEY.  The deletion is always done from a */
/*     leaf node.  If KEY is not in a leaf node, the value corresponding */
/*     to KEY is swapped with that of an immediate neighbor, and the */
/*     neighbor is deleted. This is possible because every key is either */
/*     in a leaf or has the property that its predecessor and successor */
/*     are both located in leaf nodes. */

/*     After the deletion, the successor of location from which the */
/*     deletion actually was done is shifted to the next-lower-indexed */
/*     position.  The routine updates all affected key counts and key */
/*     values, both in the target node and all ancestors of the target. */
/*     Here the target node is the leaf from which the deletion was */
/*     actually done. */

/*     The caller must balance the tree when underflow occurs. */

/*     Deletion is not quite the opposite of insertion.  Note that the */
/*     output TRGKEY has no analog in the unbalanced insertion routine */
/*     ZZEKTRUI. */

/* $ Examples */

/*     See ZZEKTRDL. */

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

/* -    SPICELIB Version 1.2.0, 09-FEB-2015 (NJB) */

/*        Now uses ERRHAN to insert DAS file name into */
/*        long error messages. */

/* -    Beta Version 1.1.0, 18-JUN-1999 (WLT) */

/*        Fixed calls to CHKIN and CHKOUT so that the same name */
/*        is used throughout the routine. */

/* -    Beta Version 1.0.0, 20-OCT-1995 (NJB) */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Use discovery check-in. */


/*     Set the variable ROOT, so we'll have something mnemonic to go */
/*     by when referring to the root node. */

    root = *tree;

/*     We always need to update the root page, so read it now. */

    zzekpgri_(handle, &root, rpage);

/*     The allowed range of keys is 1 to TOTKEY, where TOTKEY is the */
/*     total number of keys already present. */

    totkey = rpage[2];
    if (*key < 1 || *key > totkey) {
	chkin_("ZZEKTRUD", (ftnlen)8);
	setmsg_("Key = #. Valid range is 1:#.  File = #.", (ftnlen)39);
	errint_("#", key, (ftnlen)1);
	errint_("#", &totkey, (ftnlen)1);
	errhan_("#", handle, (ftnlen)1);
	chkout_("ZZEKTRUD", (ftnlen)8);
	return 0;
    }

/*     Get the number of nodes in the tree.  Also save the tree's depth. */

    nnode = rpage[1];
    depth = rpage[3];

/*     Find the point at which the deletion is to occur.  When the */
/*     tree contains only one node, no search is necessary. */

    if (nnode == 1) {

/*        This is the simplest case; all we need do is delete the */
/*        key from the root node. */

/*        Set: */

/*           - The number of keys in the tree */
/*           - The number of keys in the root */
/*           - The last key */
/*           - The data pointer for the last key */
/*           - The child pointer following the last key */

/*        In the root node, relative keys coincide with absolute keys, */
/*        so the key value need not be adjusted. */

	nkeys = totkey;
	rpage[2] = nkeys - 1;
	rpage[4] = nkeys - 1;

/*        Shift the keys, data pointer, and child pointers to the left */
/*        of the deleted key.  Update the shifted keys. */

	i__1 = nkeys - 1;
	for (i__ = *key; i__ <= i__1; ++i__) {
	    rpage[(i__2 = i__ + 4) < 256 && 0 <= i__2 ? i__2 : s_rnge("rpage",
		     i__2, "zzektrud_", (ftnlen)296)] = rpage[(i__3 = i__ + 5)
		     < 256 && 0 <= i__3 ? i__3 : s_rnge("rpage", i__3, "zzek"
		    "trud_", (ftnlen)296)] - 1;
	    rpage[(i__2 = i__ + 171) < 256 && 0 <= i__2 ? i__2 : s_rnge("rpa"
		    "ge", i__2, "zzektrud_", (ftnlen)297)] = rpage[(i__3 = i__ 
		    + 172) < 256 && 0 <= i__3 ? i__3 : s_rnge("rpage", i__3, 
		    "zzektrud_", (ftnlen)297)];
	}
	i__1 = nkeys;
	for (i__ = *key; i__ <= i__1; ++i__) {
	    rpage[(i__2 = i__ + 87) < 256 && 0 <= i__2 ? i__2 : s_rnge("rpage"
		    , i__2, "zzektrud_", (ftnlen)301)] = rpage[(i__3 = i__ + 
		    88) < 256 && 0 <= i__3 ? i__3 : s_rnge("rpage", i__3, 
		    "zzektrud_", (ftnlen)301)];
	}

/*        Zero out the freed entries. */

	rpage[(i__1 = nkeys + 4) < 256 && 0 <= i__1 ? i__1 : s_rnge("rpage", 
		i__1, "zzektrud_", (ftnlen)307)] = 0;
	rpage[(i__1 = nkeys + 171) < 256 && 0 <= i__1 ? i__1 : s_rnge("rpage",
		 i__1, "zzektrud_", (ftnlen)308)] = 0;
	rpage[(i__1 = nkeys + 88) < 256 && 0 <= i__1 ? i__1 : s_rnge("rpage", 
		i__1, "zzektrud_", (ftnlen)309)] = 0;

/*        Update the key count. */

	--nkeys;

/*        Underflow never occurs in the root; the tree simply becomes */
/*        empty if no keys are left. */

	*undrfl = FALSE_;

/*        The first key in the root will serve as the target key, */
/*        as long as the root isn't empty. */

	if (nkeys > 0) {
	    *trgkey = rpage[5];
	} else {
	    *trgkey = 0;
	}

/*        Write the page back out, and we're all set. */

	zzekpgwi_(handle, &root, rpage);
    } else if (*key == totkey) {

/*        The deleted key is the last key in the tree.  This case */
/*        is simple, because no remaining keys change as a result of */
/*        this deletion. */

	zzektrlk_(handle, tree, key, &keyidx, &target, &toffst, &level, &
		datptr);
	if (failed_()) {
	    return 0;
	}
	zzekpgri_(handle, &target, tpage);
	nkeys = tpage[0];

/*        Zero out the freed entries. */

	tpage[(i__1 = nkeys) < 256 && 0 <= i__1 ? i__1 : s_rnge("tpage", i__1,
		 "zzektrud_", (ftnlen)360)] = 0;
	tpage[(i__1 = nkeys + 127) < 256 && 0 <= i__1 ? i__1 : s_rnge("tpage",
		 i__1, "zzektrud_", (ftnlen)361)] = 0;
	tpage[(i__1 = nkeys + 64) < 256 && 0 <= i__1 ? i__1 : s_rnge("tpage", 
		i__1, "zzektrud_", (ftnlen)362)] = 0;

/*        Update the key count for this node: */

	--tpage[0];

/*        Since the key we deleted has no successors, there's no need */
/*        to adjust any other keys.  We must decrement the total */
/*        node count in the root, however. */

	rpage[2] = totkey - 1;

/*        Underflow occurs when the node started out at the minimum */
/*        key count. */

	*undrfl = nkeys == 41;

/*        The first key in the target page is the target key.  Return */
/*        an absolute key. */

	*trgkey = tpage[1] + toffst;

/*        Write the affected pages back out. */

	zzekpgwi_(handle, &root, rpage);
	zzekpgwi_(handle, &target, tpage);
    } else {

/*        Locate the item we wish to delete. */

	zzektrlk_(handle, tree, key, &keyidx, &target, &toffst, &level, &
		datptr);
	if (level == depth) {

/*           The node containing KEY is a leaf node, which is what we */
/*           want.  Deletions always take place at leaf nodes. */

/*           Since we'll have to update the ancestors of TARGET, */
/*           look up a key in the parent node now.  The order of */
/*           operations here is delicate; since the deletion */
/*           we're going to do will temporarily screw up our */
/*           addressing method, we want to do this look-up while */
/*           we're sure it will work. */

	    zzektrpi_(handle, tree, key, &parent, &pkey, &poffst, &lpidx, &
		    lpkey, &lsib, &rpidx, &rpkey, &rsib);
	    if (failed_()) {
		return 0;
	    }

/*           Read the target page.  Get the key count for this node. */

	    zzekpgri_(handle, &target, tpage);
	    tnkeys = tpage[0];

/*           Each node is allowed to underflow by 1 element.  If there */
/*           is already a deficit, OK, that's it. */

	    if (tnkeys < 41) {
		chkin_("ZZEKTRUD", (ftnlen)8);
		setmsg_("Node = #. Tree = #. File = #. Key count = #; max al"
			"lowed, including overflow, is #.", (ftnlen)83);
		errint_("#", &target, (ftnlen)1);
		errint_("#", tree, (ftnlen)1);
		errhan_("#", handle, (ftnlen)1);
		errint_("#", &tnkeys, (ftnlen)1);
		errint_("#", &c__63, (ftnlen)1);
		sigerr_("SPICE(BUG)", (ftnlen)10);
		chkout_("ZZEKTRUD", (ftnlen)8);
		return 0;
	    }

/*           Shift the keys, data pointers, and child pointers starting */
/*           at KEY to the left by 1 position.  Careful, move the */
/*           leftmost elements first.  Update the shifted key values */
/*           while we're at it. */

	    i__1 = tnkeys - 1;
	    for (i__ = keyidx; i__ <= i__1; ++i__) {
		tpage[(i__2 = i__) < 256 && 0 <= i__2 ? i__2 : s_rnge("tpage",
			 i__2, "zzektrud_", (ftnlen)457)] = tpage[(i__3 = i__ 
			+ 1) < 256 && 0 <= i__3 ? i__3 : s_rnge("tpage", i__3,
			 "zzektrud_", (ftnlen)457)] - 1;
	    }
	    i__1 = tnkeys - 1;
	    for (i__ = keyidx; i__ <= i__1; ++i__) {
		tpage[(i__2 = i__ + 127) < 256 && 0 <= i__2 ? i__2 : s_rnge(
			"tpage", i__2, "zzektrud_", (ftnlen)461)] = tpage[(
			i__3 = i__ + 128) < 256 && 0 <= i__3 ? i__3 : s_rnge(
			"tpage", i__3, "zzektrud_", (ftnlen)461)];
	    }
	    i__1 = tnkeys;
	    for (i__ = keyidx; i__ <= i__1; ++i__) {
		tpage[(i__2 = i__ + 63) < 256 && 0 <= i__2 ? i__2 : s_rnge(
			"tpage", i__2, "zzektrud_", (ftnlen)465)] = tpage[(
			i__3 = i__ + 64) < 256 && 0 <= i__3 ? i__3 : s_rnge(
			"tpage", i__3, "zzektrud_", (ftnlen)465)];
	    }

/*           Update the key count for the target node. */

	    tpage[0] = tnkeys - 1;

/*           Underflow occurs when the node started out at the minimum */
/*           count. */

	    *undrfl = tnkeys == 41;

/*           The first key in the target page is the target key. */

	    *trgkey = tpage[1] + toffst;

/*           Write the target page back out. */

	    zzekpgwi_(handle, &target, tpage);
	} else {

/*           The node containing KEY is not a leaf node.  Therefore, */
/*           KEY > 1 and KEY has a predecessor.  This predecessor */
/*           is guaranteed to reside in a leaf node.  This is simply */
/*           a property of B*-trees, of which EK trees are a subclass. */
/*           Find this predecessor. */

	    i__1 = *key - 1;
	    zzektrlk_(handle, tree, &i__1, &prev, &leaf, &loffst, &level, &
		    datptr);
	    if (failed_()) {
		return 0;
	    }

/*           Since we'll have to update the ancestors of LEAF, */
/*           look up a key in the parent node now.  The order of */
/*           operations here is delicate; since the deletion */
/*           we're going to do will temporarily screw up our */
/*           addressing method, we want to do this look-up while */
/*           we're sure it will work. */

	    i__1 = *key - 1;
	    zzektrpi_(handle, tree, &i__1, &parent, &pkey, &poffst, &lpidx, &
		    lpkey, &lsib, &rpidx, &rpkey, &rsib);
	    if (failed_()) {
		return 0;
	    }

/*           Since deletions are allowed only in leaf nodes, we'll */
/*           perform a little sleight-of-code:  We'll move the key's */
/*           predecessor into the key's location, then remove the */
/*           predecessor from its leaf node.  The order of the keys */
/*           is not disturbed by this re-arrangement. */

/*           Moving the key's predecessor into the key's location is */
/*           accomplished simply by transferring the data pointer. */

	    zzekpgri_(handle, &leaf, lpage);
	    if (target == root) {

/*              The root page has already been read into RPAGE. */

		rpage[(i__1 = keyidx + 171) < 256 && 0 <= i__1 ? i__1 : 
			s_rnge("rpage", i__1, "zzektrud_", (ftnlen)537)] = 
			lpage[(i__2 = prev + 127) < 256 && 0 <= i__2 ? i__2 : 
			s_rnge("lpage", i__2, "zzektrud_", (ftnlen)537)];
	    } else {
		zzekpgri_(handle, &target, tpage);
		tpage[(i__1 = keyidx + 127) < 256 && 0 <= i__1 ? i__1 : 
			s_rnge("tpage", i__1, "zzektrud_", (ftnlen)543)] = 
			lpage[(i__2 = prev + 127) < 256 && 0 <= i__2 ? i__2 : 
			s_rnge("lpage", i__2, "zzektrud_", (ftnlen)543)];
	    }

/*           The keys and data pointers in the leaf must be shifted */
/*           left to account for the deletion.  We'll zero out the */
/*           freed elements.  All child pointers are NIL and hence need */
/*           not be shifted. */

	    nlkeys = lpage[0];
	    i__1 = nlkeys - 1;
	    for (i__ = prev; i__ <= i__1; ++i__) {
		lpage[(i__2 = i__) < 256 && 0 <= i__2 ? i__2 : s_rnge("lpage",
			 i__2, "zzektrud_", (ftnlen)556)] = lpage[(i__3 = i__ 
			+ 1) < 256 && 0 <= i__3 ? i__3 : s_rnge("lpage", i__3,
			 "zzektrud_", (ftnlen)556)] - 1;
		lpage[(i__2 = i__ + 127) < 256 && 0 <= i__2 ? i__2 : s_rnge(
			"lpage", i__2, "zzektrud_", (ftnlen)557)] = lpage[(
			i__3 = i__ + 128) < 256 && 0 <= i__3 ? i__3 : s_rnge(
			"lpage", i__3, "zzektrud_", (ftnlen)557)];
	    }

/*           Update the key count for the leaf node. */

	    lpage[0] = nlkeys - 1;

/*           Underflow occurs when the leaf node started out at the */
/*           minimum count. */

	    *undrfl = nlkeys == 41;

/*           The first key in the leaf page is the target key. */

	    *trgkey = lpage[1] + loffst;

/*           Write the leaf, and if necessary, the target page back out. */

	    zzekpgwi_(handle, &leaf, lpage);
	    if (target != root) {
		zzekpgwi_(handle, &target, tpage);
	    }

/*           The next step will be to update the ancestors of LEAF. */
/*           For the purposes of this operation, LEAF is the target */
/*           node. */

	    target = leaf;
	}

/*        We must update the affected keys in every ancestor of TARGET. */
/*        We've already looked up information for the parent of */
/*        TARGET.  See the note at the prior call to ZZEKTRPI. */

	while(parent != root) {

/*           Before going to work on the parent, get *its* parent's info. */
/*           This is the last chance to do so. */

	    zzektrpi_(handle, tree, &pkey, &paren2, &pkey2, &poffs2, &lpidx2, 
		    &lpkey2, &lsib2, &rpidx2, &rpkey2, &rsib2);

/*           Read the parent node.  All keys from the right parent key */
/*           onward get decremented.  Remember that there may be no */
/*           right parent key. */

	    zzekpgri_(handle, &parent, tpage);
	    tnkeys = tpage[0];
	    if (rpidx > 0) {
		i__1 = tnkeys;
		for (i__ = rpidx; i__ <= i__1; ++i__) {
		    tpage[(i__2 = i__) < 256 && 0 <= i__2 ? i__2 : s_rnge(
			    "tpage", i__2, "zzektrud_", (ftnlen)621)] = tpage[
			    (i__3 = i__) < 256 && 0 <= i__3 ? i__3 : s_rnge(
			    "tpage", i__3, "zzektrud_", (ftnlen)621)] - 1;
		}

/*              Write the updated page back out. */

		zzekpgwi_(handle, &parent, tpage);
	    }
	    parent = paren2;
	    pkey = pkey2;
	    rpidx = rpidx2;
	}

/*        Update the keys in the root.  Recall that the root page has */
/*        already been read into RPAGE. */

	tnkeys = rpage[4];
	if (rpidx > 0) {
	    i__1 = tnkeys;
	    for (i__ = rpidx; i__ <= i__1; ++i__) {
		rpage[(i__2 = i__ + 4) < 256 && 0 <= i__2 ? i__2 : s_rnge(
			"rpage", i__2, "zzektrud_", (ftnlen)647)] = rpage[(
			i__3 = i__ + 4) < 256 && 0 <= i__3 ? i__3 : s_rnge(
			"rpage", i__3, "zzektrud_", (ftnlen)647)] - 1;
	    }
	}

/*        Update the total key count for the tree. */

	rpage[2] = totkey - 1;

/*        Write the updated root page back out. */

	zzekpgwi_(handle, &root, rpage);
    }
    return 0;
} /* zzektrud_ */

