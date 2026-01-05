/* zzektrpi.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      ZZEKTRPI ( EK tree, parent information ) */
/* Subroutine */ int zzektrpi_(integer *handle, integer *tree, integer *key, 
	integer *parent, integer *pkey, integer *poffst, integer *lpidx, 
	integer *lpkey, integer *lsib, integer *rpidx, integer *rpkey, 
	integer *rsib)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer page[256], lkey, prev;
    extern /* Subroutine */ int zzekpgri_(integer *, integer *, integer *);
    integer child;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    logical found;
    extern /* Subroutine */ int errhan_(char *, integer *, ftnlen);
    integer offset;
    extern /* Subroutine */ int sigerr_(char *, ftnlen);
    extern integer lstlei_(integer *, integer *, integer *);
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    integer maxkey, newkey, prvkey, totkey;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen);

/* $ Abstract */

/*     Given a key, return general information pertaining to the key's */
/*     parent node. */

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
/*     KEY        I   Key belonging to node of interest. */
/*     PARENT     O   Parent node of the node containing KEY. */
/*     PKEY       O   A key in the parent node. */
/*     POFFST     O   Key offset of the parent node. */
/*     LPIDX      O   Node-relative index of the left parent key. */
/*     LPKEY      O   Left parent key. */
/*     LSIB       O   Node number of left sibling. */
/*     RPIDX      O   Node-relative index of the right parent key. */
/*     RPKEY      O   Right parent key. */
/*     RSIB       O   Node number of right sibling. */

/* $ Detailed_Input */

/*     HANDLE         is a file handle of an EK open for read or write */
/*                    access. */

/*     TREE           is the root node number of the tree of interest. */

/*     NODE           is the node number of interest. */

/* $ Detailed_Output */

/*     PARENT         is the number of the parent node of the node */
/*                    containing KEY.  If KEY is in the root, PARENT is */
/*                    set to zero. */

/*     PKEY           is a key in PARENT.  If PARENT is set to zero, */
/*                    PKEY is set to zero as well.  PKEY is used to */
/*                    traverse a chain of ancestors towards the to root. */

/*     POFFST         is the key offset of PARENT; this is the offset */
/*                    that must be added to the node-relative key */
/*                    values in PARENT to convert them to absolute keys. */

/*     LPIDX          is the index in PARENT of the key `to the left' */
/*                    of the node containing KEY.  This key is the */
/*                    immediate predecessor of the first key in the */
/*                    subtree headed by the node containing KEY. */

/*                    The key indices in PARENT start at 1.  If PARENT */
/*                    contains no keys that precede the node containing */
/*                    KEY, LPIDX is set to zero. */

/*     LPKEY          is the absolute key located in PARENT at index */
/*                    LPIDX.  If PARENT contains no keys that precede the */
/*                    node containing KEY, LPKEY is set to zero. */

/*     LSIB           is the number of the left sibling node of the node */
/*                    containing KEY.  If PARENT contains no keys that */
/*                    precede the node containing KEY, then the node */
/*                    containing KEY has no left sibling, and LSIB is */
/*                    set to zero. */

/*     RPIDX          is the index in PARENT of the key `to the right' */
/*                    of the node containing KEY.  This key is the */
/*                    immediate successor of the last key in the */
/*                    subtree headed by the node containing KEY. */

/*                    If PARENT contains no keys that succeed the node */
/*                    containing KEY, RPIDX is set to zero. */

/*     RPKEY          is the absolute key located in PARENT at index */
/*                    RPIDX.  If PARENT contains no keys that succeed the */
/*                    node containing KEY, RPKEY is set to zero. */

/*     RSIB           is the number of the right sibling node of the node */
/*                    containing KEY.  If PARENT contains no keys that */
/*                    succeed the node containing KEY, then the node */
/*                    containing KEY has no right sibling, and RSIB is */
/*                    set to zero. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If HANDLE is invalid, the error will be diagnosed by routines */
/*         called by this routine.  The file will not be modified. */

/*     2)  If an I/O error occurs while reading the indicated file, the */
/*         error will be diagnosed by routines called by this routine. */

/*     3)  If the input key is out of range, the error */
/*         SPICE(INDEXOUTOFRANGE) is signaled. */

/*     4)  If the input key is not found in the tree, the error */
/*         SPICE(ITEMNOTFOUND) is signaled.  This error most likely */
/*         indicates the presence of a serious bug in the EK software, */
/*         or that the input EK file has been corrupted. */

/* $ Files */

/*     See the EK Required Reading for a discussion of the EK file */
/*     format. */

/* $ Particulars */

/*     This routine supports tree operations that involve identifying */
/*     the parent node of a specified node.  In particular, this */
/*     routine supports updating ancestors of a node when an insertion */
/*     or deletion occurs. */

/* $ Examples */

/*     See ZZEKTRUD, ZZEKTRUI. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 09-FEB-2015 (NJB) */

/*        Now uses ERRHAN to insert DAS file name into */
/*        long error messages. */


/* -    Beta Version 1.0.0, 23-OCT-1995 (NJB) */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Use discovery check-in in this puppy. */

/*     Nothing found to begin with. */

    found = FALSE_;

/*     Get a local copy of the input key.  We may overwrite the input */
/*     key when we set PKEY. */

    lkey = *key;

/*     Start out by reading in the root page.  The node level starts */
/*     out at 1. */

    zzekpgri_(handle, tree, page);
    *parent = 0;
    *pkey = 0;
    *poffst = 0;
    *lpidx = 0;
    *lpkey = 0;
    *lsib = 0;
    *rpidx = 0;
    *rpkey = 0;
    *rsib = 0;

/*     Find out how many keys are in the tree.  If LKEY is outside */
/*     this range, we won't find it. */

    totkey = page[2];
    if (lkey < 1 || lkey > totkey) {
	chkin_("ZZEKTRPI", (ftnlen)8);
	setmsg_("Key = #; valid range = 1:#. Tree = #, file = #", (ftnlen)46);
	errint_("#", &lkey, (ftnlen)1);
	errint_("#", &totkey, (ftnlen)1);
	errint_("#", tree, (ftnlen)1);
	errhan_("#", handle, (ftnlen)1);
	sigerr_("SPICE(INDEXOUTOFRANGE)", (ftnlen)22);
	chkout_("ZZEKTRPI", (ftnlen)8);
	return 0;
    }

/*     Find the last key at this level that is less than or equal to */
/*     the requested key. */

    prev = lstlei_(&lkey, &page[4], &page[5]);
    if (prev > 0) {
	prvkey = page[(i__1 = prev + 4) < 256 && 0 <= i__1 ? i__1 : s_rnge(
		"page", i__1, "zzektrpi_", (ftnlen)279)];
    } else {
	prvkey = 0;
    }

/*     If we were lucky enough to get an exact match, we can quit now. */
/*     The root has no parent so the output values remain set to zero. */

    if (prvkey == lkey) {
	return 0;
    }

/*     Still here?  Traverse the pointer path until we find the key */
/*     or run out of progeny. */

    offset = prvkey;
    *parent = *tree;
    *pkey = page[5];
    maxkey = page[4];
    if (prev > 0) {
	*lpidx = prev;
	*lpkey = page[(i__1 = *lpidx + 4) < 256 && 0 <= i__1 ? i__1 : s_rnge(
		"page", i__1, "zzektrpi_", (ftnlen)303)];
	*lsib = page[(i__1 = *lpidx + 87) < 256 && 0 <= i__1 ? i__1 : s_rnge(
		"page", i__1, "zzektrpi_", (ftnlen)304)];
    } else {
	*lpidx = 0;
	*lpkey = 0;
	*lsib = 0;
    }
    if (prev < maxkey) {
	*rpidx = prev + 1;
	*rpkey = page[(i__1 = *rpidx + 4) < 256 && 0 <= i__1 ? i__1 : s_rnge(
		"page", i__1, "zzektrpi_", (ftnlen)313)];
	*rsib = page[(i__1 = *rpidx + 88) < 256 && 0 <= i__1 ? i__1 : s_rnge(
		"page", i__1, "zzektrpi_", (ftnlen)314)];
    } else {
	*rpidx = 0;
	*rpkey = 0;
	*rsib = 0;
    }
    child = page[(i__1 = prev + 88) < 256 && 0 <= i__1 ? i__1 : s_rnge("page",
	     i__1, "zzektrpi_", (ftnlen)322)];
    found = FALSE_;
    while(child > 0 && ! found) {

/*        Read in the child page. */

	zzekpgri_(handle, &child, page);

/*        Find the last key at this level that is less than or equal to */
/*        the requested key.  Since the keys we're looking at now are */
/*        ordinal positions relative to the subtree whose root is the */
/*        current node, we must subtract from LKEY the position of the */
/*        node preceding the first key of this subtree. */

	newkey = lkey - offset;
	prev = lstlei_(&newkey, page, &page[1]);
	if (prev > 0) {
	    prvkey = page[(i__1 = prev) < 256 && 0 <= i__1 ? i__1 : s_rnge(
		    "page", i__1, "zzektrpi_", (ftnlen)342)];
	} else {
	    prvkey = 0;
	}

/*        If we were lucky enough to get an exact match, we can quit. */
/*        The outputs are set. */

	if (prvkey == newkey) {
	    found = TRUE_;
	} else {

/*           Record information from the current node before we read the */
/*           next child page. */

	    *parent = child;
	    *poffst = offset;
	    *pkey = page[1] + offset;
	    maxkey = page[0];
	    if (prev > 0) {
		*lpidx = prev;
		*lpkey = page[(i__1 = *lpidx) < 256 && 0 <= i__1 ? i__1 : 
			s_rnge("page", i__1, "zzektrpi_", (ftnlen)367)];
		*lsib = page[(i__1 = *lpidx + 63) < 256 && 0 <= i__1 ? i__1 : 
			s_rnge("page", i__1, "zzektrpi_", (ftnlen)368)];
	    } else {
		*lpidx = 0;
		*lpkey = 0;
		*lsib = 0;
	    }
	    if (prev < maxkey) {
		*rpidx = prev + 1;
		*rpkey = page[(i__1 = *rpidx) < 256 && 0 <= i__1 ? i__1 : 
			s_rnge("page", i__1, "zzektrpi_", (ftnlen)377)];
		*rsib = page[(i__1 = *rpidx + 64) < 256 && 0 <= i__1 ? i__1 : 
			s_rnge("page", i__1, "zzektrpi_", (ftnlen)378)];
	    } else {
		*rpidx = 0;
		*rpkey = 0;
		*rsib = 0;
	    }

/*           Update the offset of the tree headed by CHILD, and set */
/*           the new child node. */

	    offset = prvkey + offset;
	    child = page[(i__1 = prev + 64) < 256 && 0 <= i__1 ? i__1 : 
		    s_rnge("page", i__1, "zzektrpi_", (ftnlen)390)];
	}
    }

/*     If we found the key, our outputs are already set.  If not, we've */
/*     got trouble. */

    if (! found) {
	chkin_("ZZEKTRPI", (ftnlen)8);
	setmsg_("Key #; valid range = 1:#. Tree = #, file = #.  Key was not "
		"found.  This probably indicates a corrupted file or a bug in"
		" the EK code.", (ftnlen)132);
	errint_("#", &lkey, (ftnlen)1);
	errint_("#", &totkey, (ftnlen)1);
	errint_("#", tree, (ftnlen)1);
	errhan_("#", handle, (ftnlen)1);
	sigerr_("SPICE(ITEMNOTFOUND)", (ftnlen)19);
	chkout_("ZZEKTRPI", (ftnlen)8);
	return 0;
    }
    return 0;
} /* zzektrpi_ */

