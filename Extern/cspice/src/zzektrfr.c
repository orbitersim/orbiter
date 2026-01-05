/* zzektrfr.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__10 = 10;
static integer c__3 = 3;

/* $Procedure      ZZEKTRFR ( EK tree, free ) */
/* Subroutine */ int zzektrfr_(integer *handle, integer *tree)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer page[256], node;
    extern /* Subroutine */ int zzekpgfr_(integer *, integer *, integer *), 
	    zzekpgri_(integer *, integer *, integer *), chkin_(char *, ftnlen)
	    ;
    integer depth, level, nkids, stack[30]	/* was [3][10] */, first, 
	    nkeys, kidbas, remain;
    extern /* Subroutine */ int errhan_(char *, integer *, ftnlen), sigerr_(
	    char *, ftnlen), chkout_(char *, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen);

/* $ Abstract */

/*     Free a tree:  deallocate all pages belonging to the tree. */

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

/* $ Detailed_Input */

/*     HANDLE         is a file handle of an EK open for write access. */

/*     TREE           is the root node number of the tree of interest. */

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

/*     3)  If the input tree is deeper than the maximum allowed depth */
/*         TRMXDP, the error SPICE(INVALIDFORMAT) is signaled. */

/* $ Files */

/*     See the EK Required Reading for a discussion of the EK file */
/*     format. */

/* $ Particulars */

/*     This routine cleans up the pages occupied by an EK tree; the pages */
/*     are deallocated by the EK paging system.  Freeing a tree allows */
/*     the pages previous occupied by the tree to be used for other */
/*     purposes. */

/* $ Examples */

/*     1)  Return the pages occupied by the tree whose root node number */
/*         is TREE.  Assume HANDLE is a file handle of the EK to which */
/*         the tree belongs: */

/*            CALL EKTRFR ( HANDLE, TREE ) */

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

/* -    SPICELIB Version 1.3.0, 09-FEB-2015 (NJB) */

/*        Now uses ERRHAN to insert DAS file name into */
/*        long error messages. */

/* -    SPICELIB Version 1.2.0, 18-JUN-1999 (WLT) */

/*        Removed a redundant call to CHKIN. */

/* -    SPICELIB Version 1.1.0, 14-OCT-1996 (NJB) */

/*        Bug fix:  the original version was untested and had numerous */
/*        problems. */

/* -    SPICELIB Version 1.0.0, 22-OCT-1995 (NJB) */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.1.0, 14-OCT-1996 (NJB) */

/*        Bug fix:  the original version was untested and had numerous */
/*        problems. */


/* -& */

/*     SPICELIB functions */


/*     Non-SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("ZZEKTRFR", (ftnlen)8);

/*     Read in the root node. */

    zzekpgri_(handle, tree, page);

/*     Check the depth of the tree.  If the tree is deeper than */
/*     we expected, we've a problem. */

    depth = page[3];
    if (depth > 10) {
	setmsg_("Tree has depth #; max supported depth is #.EK = #; TREE = #."
		, (ftnlen)60);
	errint_("#", &depth, (ftnlen)1);
	errint_("#", &c__10, (ftnlen)1);
	errhan_("#", handle, (ftnlen)1);
	errint_("#", tree, (ftnlen)1);
	sigerr_("SPICE(INVALIDFORMAT)", (ftnlen)20);
	chkout_("ZZEKTRFR", (ftnlen)8);
	return 0;
    }

/*     We traverse the tree in post-order fashion:  at each node, */
/*     we first delete all of the node's children in left-to-right */
/*     order, then we delete the node itself.  We use a stack to */
/*     keep track of the ancestors of the node we're currently */
/*     considering. */

    level = 1;
    remain = page[1];
    node = *tree;

/*     Initialize the child count and the location of the first */
/*     child in the current node.  The child count of the root is */
/*     one more than the number of keys in the root if the root has */
/*     children; otherwise, the child count is zero. */

    nkeys = page[4];
    if (depth == 1) {
	nkids = 0;
    } else {
	nkids = nkeys + 1;
    }
    first = 1;
    while(remain > 0) {

/*        At this point, */

/*           NODE is the current node to consider. */
/*           NKIDS is the number of children of NODE. */
/*           FIRST is the index of the first child in NODE. */

	if (nkids > 0) {

/*           This node has children, so push the current node, the */
/*           number of children, and the location of the first child on */
/*           the stack.  Before incrementing the stack level, determine */
/*           the base address of the child pointers. */

	    if (level == 1) {
		kidbas = 88;
	    } else {
		kidbas = 64;
	    }
	    stack[(i__1 = level * 3 - 3) < 30 && 0 <= i__1 ? i__1 : s_rnge(
		    "stack", i__1, "zzektrfr_", (ftnlen)273)] = node;
	    stack[(i__1 = level * 3 - 2) < 30 && 0 <= i__1 ? i__1 : s_rnge(
		    "stack", i__1, "zzektrfr_", (ftnlen)274)] = nkids;
	    stack[(i__1 = level * 3 - 1) < 30 && 0 <= i__1 ? i__1 : s_rnge(
		    "stack", i__1, "zzektrfr_", (ftnlen)275)] = first;
	    ++level;

/*           Read in the first child node. */

	    node = page[(i__1 = kidbas + first - 1) < 256 && 0 <= i__1 ? i__1 
		    : s_rnge("page", i__1, "zzektrfr_", (ftnlen)281)];
	    zzekpgri_(handle, &node, page);

/*           We've never visited this node before, so the node's */
/*           metadata is valid, and the first child pointer, if any, */
/*           is at location 1. */

	    nkeys = page[0];
	    if (level < depth) {
		nkids = nkeys + 1;
	    } else {
		nkids = 0;
	    }
	    first = 1;
	} else {

/*           This node has no children.  We can free this page. */

	    zzekpgfr_(handle, &c__3, &node);
	    --remain;

/*           Obtain the parent node by popping the stack. */

	    --level;
	    if (level > 0) {
		node = stack[(i__1 = level * 3 - 3) < 30 && 0 <= i__1 ? i__1 :
			 s_rnge("stack", i__1, "zzektrfr_", (ftnlen)316)];
		first = stack[(i__1 = level * 3 - 1) < 30 && 0 <= i__1 ? i__1 
			: s_rnge("stack", i__1, "zzektrfr_", (ftnlen)317)];
		nkids = stack[(i__1 = level * 3 - 2) < 30 && 0 <= i__1 ? i__1 
			: s_rnge("stack", i__1, "zzektrfr_", (ftnlen)318)];

/*              The parent has one less child, and the location of the */
/*              first child is the successor of the stored location. */

		--nkids;
		++first;

/*              The parent page has been overwritten; read it back in. */

		zzekpgri_(handle, &node, page);
	    }
	}

/*        On this pass through the loop, we either visited a node */
/*        for the first time, or we deleted a node.  Therefore, we */
/*        made progress toward loop termination. */

    }
    chkout_("ZZEKTRFR", (ftnlen)8);
    return 0;
} /* zzektrfr_ */

