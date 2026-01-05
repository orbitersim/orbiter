/* zzektr1s.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__10 = 10;
static integer c__3 = 3;
static integer c__256 = 256;

/* $Procedure      ZZEKTR1S ( EK tree, one-shot load ) */
/* Subroutine */ int zzektr1s_(integer *handle, integer *tree, integer *size, 
	integer *values)
{
    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer base, page[256], nbig, node, subd, next;
    extern /* Subroutine */ int zzekpgal_(integer *, integer *, integer *, 
	    integer *), zzekpgri_(integer *, integer *, integer *), zzekpgwi_(
	    integer *, integer *, integer *);
    extern integer zzektrbs_(integer *);
    integer d__, i__, n, q, child, s;
    extern integer zzektrsz_(integer *, integer *);
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer level, nkids, npred, nkeys, tsize, kidbas;
    extern /* Subroutine */ int cleari_(integer *, integer *), dasudi_(
	    integer *, integer *, integer *, integer *);
    integer basidx;
    extern /* Subroutine */ int errhan_(char *, integer *, ftnlen);
    integer bigsiz, nnodes, nsmall, stnbig[10], stnbas[10], stnode[10];
    extern /* Subroutine */ int sigerr_(char *, ftnlen);
    extern logical return_(void);
    integer maxsiz, reqsiz, stlsiz[10], stnext[10], stnkey[10], subsiz, 
	    totnod;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen), chkout_(char *, ftnlen);
    integer div, key;

/* $ Abstract */

/*     One-shot tree load:  insert an entire array into an empty */
/*     tree. */

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
/*     SIZE       I   Size of tree. */
/*     VALUES     I   Values to insert. */

/* $ Detailed_Input */

/*     HANDLE         is a file handle of an EK open for write access. */

/*     TREE           is the root node number of the tree of interest. */
/*                    The tree must be empty. */

/*     SIZE           is the size of the tree to create:  SIZE is the */
/*                    number of values that will be inserted into the */
/*                    tree. */

/*     VALUES         is an array of integer values to be inserted into */
/*                    the tree. */

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

/*     3)  If the input tree is not empty, the error SPICE(NONEMPTYTREE) */
/*         is signaled. */

/*     4)  If the depth of the tree needed to hold the number of values */
/*         indicated by SIZE exceeds the maximum depth limit, the error */
/*         SPICE(COUNTTOOLARGE) is signaled. */

/* $ Files */

/*     See the EK Required Reading for a discussion of the EK file */
/*     format. */

/* $ Particulars */

/*     This routine creates an EK tree and loads the tree with the */
/*     integer values supplied in the array VALUES.  The ordinal */
/*     positions of the values in the tree correspond to the positions */
/*     of the values in the input array:  for example, the 10th element */
/*     of the array is pointed to by the key 10. */

/*     This routine loads a tree much faster than can be done by */
/*     sequentially loading the set of values by successive calls to */
/*     ZZEKTRIN.  On the other hand, the caller must declare an array */
/*     large enough to hold all of the values to be loaded.  Note that */
/*     a partially full tree cannot be extended using this routine. */

/* $ Examples */

/*     See EKFFLD. */

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

/* -    SPICELIB Version 1.2.0, 05-FEB-2015 (NJB) */

/*        Now uses ERRHAN */

/*        Cleaned up use of unnecessary variables and unneeded */
/*        declarations. */

/* -    Beta Version 1.1.0, 18-JUN-1999 (WLT) */

/*        Removed redundant calls to CHKIN */

/* -    Beta Version 1.0.0, 22-OCT-1995 (NJB) */

/* -& */

/*     SPICELIB functions */


/*     Non-SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("ZZEKTR1S", (ftnlen)8);

/*     Make sure the input tree is empty. */

    tsize = zzektrsz_(handle, tree);
    if (tsize > 0) {
	setmsg_("Tree has size #; should be empty.EK = #; TREE = #.", (ftnlen)
		50);
	errint_("#", &tsize, (ftnlen)1);
	errhan_("#", handle, (ftnlen)1);
	errint_("#", tree, (ftnlen)1);
	sigerr_("SPICE(NONEMPTYTREE)", (ftnlen)19);
	chkout_("ZZEKTR1S", (ftnlen)8);
	return 0;
    }

/*     Compute the tree depth required.  The largest tree of a given */
/*     depth D contains the root node plus S(D) child nodes, where */


/*        S(1)  =  1 */


/*     and if D is at least 2, */
/*                                    D - 2 */
/*                                    ____ */
/*                                    \              i */
/*        S(D)  =  MAX_SIZE      *    /      MAX_SIZE */
/*                         Root       ----           Child */
/*                                    i = 0 */


/*                                    D - 2 */
/*                                    ____ */
/*                                    \            i */
/*              =  MXKIDR        *    /      MXKIDC */
/*                                    ---- */
/*                                    i = 0 */


/*                                         D-1 */
/*                                   MXKIDC   -  1 */
/*              =  MXKIDR        *   ------------- */
/*                                    MXKIDC  - 1 */


/*     If all of these nodes are full, the number of keys that */
/*     can be held in this tree is */

/*        MXKEYR  +  S(D) * MXKEYC */

/*     We want the minimum value of D such that this expression */
/*     is greater than or equal to SIZE. */

    tsize = 82;
    d__ = 1;
    s = 1;
    while(tsize < *size) {
	++d__;
	if (d__ == 2) {
	    s = 82;
	} else {

/*           For computational purposes, the relationship */

/*              S(D+1)  =   MXKIDR  +  MXKIDC * S(D) */

/*           is handy. */


	    s = s * 63 + 83;
	}
	tsize = s * 62 + 82;
    }

/*     If the tree must be deeper than we expected, we've a problem. */

    if (d__ > 10) {
	setmsg_("Tree has depth #; max supported depth is #.EK = #; TREE = #."
		, (ftnlen)60);
	errint_("#", &d__, (ftnlen)1);
	errint_("#", &c__10, (ftnlen)1);
	errhan_("#", handle, (ftnlen)1);
	errint_("#", tree, (ftnlen)1);
	sigerr_("SPICE(COUNTTOOLARGE)", (ftnlen)20);
	chkout_("ZZEKTR1S", (ftnlen)8);
	return 0;
    }

/*     The basic error checks are done.  At this point, we can build the */
/*     tree. */

/*     The approach is to fill in the tree in a top-down fashion. */
/*     We decide how big each subtree of the root will be; this */
/*     information allows us to decide which keys actually belong */
/*     in the root.  Having filled in the root, we repeat the process */
/*     for each subtree of the root in left-to-right order. */

/*     We use a stack to keep track of the ancestors of the */
/*     node we're currently considering.  The table below shows the */
/*     items we save on the stack and the stack variables associated */
/*     with those items: */


/*        Item                                 Stack Variable */
/*        ----                                 --------------- */
/*        Node number                          STNODE */

/*        Number of keys in node               STNKEY */

/*        Larger subtree size                  STLSIZ */

/*        Number of large subtrees             STNBIG */

/*        Index of next subtree to visit       STNEXT */

/*        Base index of node                   STNBAS */


    node = *tree;
    subsiz = *size;
    next = 1;
    level = 1;
    basidx = 0;
    while(level > 0) {

/*        At this point, LEVEL, NEXT, NODE, SUBSIZ and BASIDX are set. */

	if (next == 1) {

/*           This node has not been visited yet.  We'll fill in this */
/*           node before proceeding to fill in its descendants.  The */
/*           first step is to compute the number and sizes of the */
/*           subtrees of this node. */

/*           Decide the large subtree size and the number of subtrees of */
/*           this node.  The depth SUBD of the subtrees of this node is */
/*           D - LEVEL.  Each subtree has size bounded by the sizes of */
/*           the subtree of depth SUBD in which all nodes contain MNKEYC */
/*           keys and the by the subtree of depth SUBD in which all nodes */
/*           contain MXKEYC keys.  If this node is not the root and is */
/*           not a leaf node, the number of subtrees must be between */
/*           MNKIDC and MXKIDC. */

	    if (level == 1) {

/*              We're working on the root.  The number of subtrees is */
/*              anywhere between 0 and MXKIDR, inclusive.  We'll create */
/*              a tree with the minimum number of subtrees of the root. */

		if (d__ > 1) {

/*                 We'll find the number of subtrees of maximum size */
/*                 that we would need to hold the non-root keys of the */
/*                 tree.  We'll then determine the actual required sizes */
/*                 of these subtrees. */

		    subd = d__ - 1;
		    nnodes = 0;
		    i__1 = subd;
		    for (i__ = 1; i__ <= i__1; ++i__) {
			nnodes = nnodes * 63 + 1;
		    }
		    maxsiz = nnodes * 62;

/*                 If we had NKIDS subtrees of size MAXSIZ, NKIDS */
/*                 would be the smallest integer such that */

/*                    ( NKIDS - 1 )  +  NKIDS * MAXSIZ  >  SUBSIZ */
/*                                                      - */

/*                 or equivalently, */

/*                     NKIDS * ( MAXSIZ + 1 )  >  SUBSIZ + 1 */
/*                                             - */

/*                 We'll compute this value of NKIDS. */


		    q = subsiz + 1;
		    div = maxsiz + 1;
		    nkids = (q + div - 1) / div;

/*                 The minimum number of keys we must store in child */
/*                 nodes is the number of keys in the tree, minus those */
/*                 that can be accommodated in the root: */

		    n = subsiz - (nkids - 1);

/*                 Now we can figure out how large the subtrees would */
/*                 have to be in order to hold N keys, if all subtrees */
/*                 had the same size. */

		    bigsiz = (n + nkids - 1) / nkids;

/*                 We may have more capacity than we need if all subtrees */
/*                 have size BIGSIZ.  So, we'll allow some subtrees to */
/*                 have size BIGSIZ-1.  Not all subtrees can have the */
/*                 smaller size (otherwise BIGSIZ would have been */
/*                 smaller).  The first NBIG subtrees will have the */
/*                 larger size. */

		    nsmall = nkids * bigsiz - n;
		    nbig = nkids - nsmall;
		    nkeys = nkids - 1;
		} else {

/*                 All keys are in the root. */

		    nkeys = *size;
		    nkids = 0;
		}

/*              Read in the root page. */

		zzekpgri_(handle, tree, page);

/*              We have enough information to fill in the root node. */
/*              We'll allocate nodes for the immediate children. */
/*              There is one key `between' each child pointer. */

		i__1 = nkeys;
		for (i__ = 1; i__ <= i__1; ++i__) {

/*                 The Ith key may be found by considering the number */
/*                 of keys in the subtree between the Ith key and its */
/*                 predecessor in the root. */

		    if (i__ == 1) {
			npred = 0;
		    } else {
			npred = page[(i__2 = i__ + 3) < 256 && 0 <= i__2 ? 
				i__2 : s_rnge("page", i__2, "zzektr1s_", (
				ftnlen)480)];
		    }
		    if (d__ > 1) {

/*                    The tree contains subtrees. */

			if (i__ <= nbig) {
			    key = npred + bigsiz + 1;
			} else {
			    key = npred + bigsiz;
			}
		    } else {
			key = i__;
		    }
		    page[(i__2 = i__ + 4) < 256 && 0 <= i__2 ? i__2 : s_rnge(
			    "page", i__2, "zzektr1s_", (ftnlen)499)] = key;
		    page[(i__2 = i__ + 171) < 256 && 0 <= i__2 ? i__2 : 
			    s_rnge("page", i__2, "zzektr1s_", (ftnlen)500)] = 
			    values[key - 1];
		}
		totnod = 1;
		i__1 = nkids;
		for (i__ = 1; i__ <= i__1; ++i__) {

/*                 Allocate a node for the Ith child.  Store pointers */
/*                 to these nodes. */

		    zzekpgal_(handle, &c__3, &child, &base);
		    page[(i__2 = i__ + 87) < 256 && 0 <= i__2 ? i__2 : s_rnge(
			    "page", i__2, "zzektr1s_", (ftnlen)513)] = child;
		    ++totnod;
		}

/*              Fill in the root's metadata.  There is one item that */
/*              we'll have to fill in when we're done:  the number of */
/*              nodes in the tree.  We know the rest of the information */
/*              now. */

		page[2] = *size;
		page[3] = d__;
		page[4] = nkeys;
		page[1] = 0;

/*              Write out the root. */

		zzekpgwi_(handle, tree, page);
	    } else if (level < d__) {

/*              The current node is a non-leaf child node. */

		cleari_(&c__256, page);

/*              The tree headed by this node has depth D-LEVEL+1 and */
/*              must hold SUBSIZ keys.  We must figure out the size */
/*              and number of subtrees of the current node.  Unlike in */
/*              the case of the root, we must have between MNKIDC */
/*              and MXKIDC subtrees of this node.  We start out by */
/*              computing the required subtree size if there were */
/*              exactly MNKIDC subtrees.  In this case, the total */
/*              number of keys in the subtrees would be */

/*                 SUBSIZ  -  MNKEYC */


		n = subsiz - 41;
		reqsiz = (n + 40) / 41;

/*              Compute the maximum allowable number of keys in */
/*              a subtree. */

		subd = d__ - level;
		nnodes = 0;
		i__1 = subd;
		for (i__ = 1; i__ <= i__1; ++i__) {
		    nnodes = nnodes * 63 + 1;
		}
		maxsiz = nnodes * 62;

/*              If the number REQSIZ we came up with is a valid size, */
/*              we'll be able to get the correct number of children */
/*              by using subtrees of size REQSIZ and REQSIZ-1.  Note */
/*              that it's impossible for REQSIZ to be too small, */
/*              since the smallest possible number of subtrees is */
/*              MNKIDC. */

		if (reqsiz <= maxsiz) {

/*                 Decide how many large and small subtrees we need. */

		    nkids = 42;
		    bigsiz = reqsiz;
		    nsmall = bigsiz * nkids - n;
		    nbig = nkids - nsmall;
		} else {


/*                 See how many subtrees of size MAXSIZ it would take */
/*                 to hold the requisite number of keys.  We know the */
/*                 number is more than MNKIDC.  If we have NKIDS */
/*                 subtrees of size MAXSIZ, the total number of */
/*                 keys in the subtree headed by NODE is */

/*                   ( NKIDS - 1 )  +  ( NKIDS * MAXSIZ ) */

/*                 or */

/*                     NKIDS * ( MAXSIZ + 1 )   -   1 */

/*                 We must find the smallest value of NKIDS such */
/*                 that the above quantity is greater than or equal */
/*                 to SUBSIZ. */

		    q = subsiz + 1;
		    div = maxsiz + 1;
		    nkids = (q + div - 1) / div;

/*                 We know that NKIDS subtrees of size MAXSIZ, plus */
/*                 NKIDS-1 keys in NODE, can hold at least SUBSIZ */
/*                 keys.  We now want to find the smallest subtree */
/*                 size such that NKIDS subtrees of that size, */
/*                 together with the NKIDS-1 keys in NODE, contain */
/*                 at least SUBSIZ keys.  The size we seek will */
/*                 become BIGSIZ, the larger of the two subtree */
/*                 sizes we'll use.  So BIGSIZ is the smallest */
/*                 integer such that */

/*                    ( NKIDS - 1 ) + ( NKIDS * BIGSIZ )  >  SUBSIZ */
/*                                                        - */

/*                 or equivalently */

/*                    BIGSIZ * NKIDS  >  SUBSIZ - NKIDS + 1 */
/*                                    - */

		    q = subsiz - nkids + 1;
		    div = nkids;
		    bigsiz = (q + div - 1) / div;
		    nsmall = bigsiz * nkids - q;
		    nbig = nkids - nsmall;
		}

/*              Fill in the keys for the current node. */

		nkeys = nkids - 1;
		i__1 = nkeys;
		for (i__ = 1; i__ <= i__1; ++i__) {

/*                 The Ith key may be found by considering the number */
/*                 of keys in the subtree between the Ith key and its */
/*                 predecessor in the current node. */

		    if (i__ == 1) {
			npred = basidx;
		    } else {
			npred = basidx + page[(i__2 = i__ - 1) < 256 && 0 <= 
				i__2 ? i__2 : s_rnge("page", i__2, "zzektr1s_"
				, (ftnlen)652)];
		    }
		    if (i__ <= nbig) {
			key = npred + bigsiz + 1;
		    } else {
			key = npred + bigsiz;
		    }
		    page[(i__2 = i__) < 256 && 0 <= i__2 ? i__2 : s_rnge(
			    "page", i__2, "zzektr1s_", (ftnlen)661)] = key - 
			    basidx;
		    page[(i__2 = i__ + 127) < 256 && 0 <= i__2 ? i__2 : 
			    s_rnge("page", i__2, "zzektr1s_", (ftnlen)662)] = 
			    values[key - 1];
		}
		i__1 = nkids;
		for (i__ = 1; i__ <= i__1; ++i__) {

/*                 Allocate a node for the Ith child.  Store pointers */
/*                 to these nodes. */

		    zzekpgal_(handle, &c__3, &child, &base);
		    page[(i__2 = i__ + 63) < 256 && 0 <= i__2 ? i__2 : s_rnge(
			    "page", i__2, "zzektr1s_", (ftnlen)674)] = child;
		    ++totnod;
		}

/*              We can now fill in the metadata for the current node. */

		page[0] = nkeys;
		zzekpgwi_(handle, &node, page);
	    }

/*           Unless the current node is a leaf node, prepare to visit */
/*           the first child of the current node. */

	    if (level < d__) {

/*              Push our current state. */

		stnode[(i__1 = level - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge(
			"stnode", i__1, "zzektr1s_", (ftnlen)696)] = node;
		stnkey[(i__1 = level - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge(
			"stnkey", i__1, "zzektr1s_", (ftnlen)697)] = nkeys;
		stlsiz[(i__1 = level - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge(
			"stlsiz", i__1, "zzektr1s_", (ftnlen)698)] = bigsiz;
		stnbig[(i__1 = level - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge(
			"stnbig", i__1, "zzektr1s_", (ftnlen)699)] = nbig;
		stnext[(i__1 = level - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge(
			"stnext", i__1, "zzektr1s_", (ftnlen)700)] = 2;
		stnbas[(i__1 = level - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge(
			"stnbas", i__1, "zzektr1s_", (ftnlen)701)] = basidx;

/*              NEXT is already set to 1.  BASIDX is set, since the */
/*              base index of the first child is that of the parent. */

		if (level == 1) {
		    kidbas = 88;
		} else {
		    kidbas = 64;
		}
		++level;
		node = page[(i__1 = kidbas) < 256 && 0 <= i__1 ? i__1 : 
			s_rnge("page", i__1, "zzektr1s_", (ftnlen)714)];
		subsiz = bigsiz;
	    } else if (level > 1) {

/*              The current node is a child leaf node.  There are no */
/*              calculations to do; we simply assign keys and pointers, */
/*              write out metadata, and pop our state. */

		nkeys = subsiz;
		i__1 = nkeys;
		for (i__ = 1; i__ <= i__1; ++i__) {
		    key = basidx + i__;
		    page[(i__2 = i__) < 256 && 0 <= i__2 ? i__2 : s_rnge(
			    "page", i__2, "zzektr1s_", (ftnlen)729)] = i__;
		    page[(i__2 = i__ + 127) < 256 && 0 <= i__2 ? i__2 : 
			    s_rnge("page", i__2, "zzektr1s_", (ftnlen)730)] = 
			    values[key - 1];
		}

/*              We can now fill in the metadata for the current node. */

		page[0] = nkeys;
		zzekpgwi_(handle, &node, page);

/*              A leaf node is a subtree unto itself, and we're */
/*              done with this subtree.  Pop our state. */

		--level;
		if (level >= 1) {
		    node = stnode[(i__1 = level - 1) < 10 && 0 <= i__1 ? i__1 
			    : s_rnge("stnode", i__1, "zzektr1s_", (ftnlen)749)
			    ];
		    nkeys = stnkey[(i__1 = level - 1) < 10 && 0 <= i__1 ? 
			    i__1 : s_rnge("stnkey", i__1, "zzektr1s_", (
			    ftnlen)750)];
		    bigsiz = stlsiz[(i__1 = level - 1) < 10 && 0 <= i__1 ? 
			    i__1 : s_rnge("stlsiz", i__1, "zzektr1s_", (
			    ftnlen)751)];
		    nbig = stnbig[(i__1 = level - 1) < 10 && 0 <= i__1 ? i__1 
			    : s_rnge("stnbig", i__1, "zzektr1s_", (ftnlen)752)
			    ];
		    next = stnext[(i__1 = level - 1) < 10 && 0 <= i__1 ? i__1 
			    : s_rnge("stnext", i__1, "zzektr1s_", (ftnlen)753)
			    ];
		    basidx = stnbas[(i__1 = level - 1) < 10 && 0 <= i__1 ? 
			    i__1 : s_rnge("stnbas", i__1, "zzektr1s_", (
			    ftnlen)754)];
		    nkids = nkeys + 1;

/*                 Read in the current node. */

		    zzekpgri_(handle, &node, page);
		}
	    } else {

/*              The only node is the root.  Pop out. */

		level = 0;
	    }

/*           We've decided which node to go to next at this point. */
/*           At this point, LEVEL, NEXT, NODE, SUBSIZ and BASIDX are set. */

	} else {

/*           The current node has been visited already.  Visit the */
/*           next child, if there is one. */

	    if (next <= nkids) {

/*              Prepare to visit the next child of the current node. */

		stnext[(i__1 = level - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge(
			"stnext", i__1, "zzektr1s_", (ftnlen)786)] = next + 1;
		if (level == 1) {
		    kidbas = 88;
		} else {
		    kidbas = 64;
		}
		node = page[(i__1 = kidbas + next - 1) < 256 && 0 <= i__1 ? 
			i__1 : s_rnge("page", i__1, "zzektr1s_", (ftnlen)796)]
			;
		if (next <= nbig) {
		    subsiz = stlsiz[(i__1 = level - 1) < 10 && 0 <= i__1 ? 
			    i__1 : s_rnge("stlsiz", i__1, "zzektr1s_", (
			    ftnlen)800)];
		} else {
		    subsiz = stlsiz[(i__1 = level - 1) < 10 && 0 <= i__1 ? 
			    i__1 : s_rnge("stlsiz", i__1, "zzektr1s_", (
			    ftnlen)802)] - 1;
		}
		if (next <= nbig + 1) {
		    basidx = stnbas[(i__1 = level - 1) < 10 && 0 <= i__1 ? 
			    i__1 : s_rnge("stnbas", i__1, "zzektr1s_", (
			    ftnlen)808)] + (next - 1) * stlsiz[(i__2 = level 
			    - 1) < 10 && 0 <= i__2 ? i__2 : s_rnge("stlsiz", 
			    i__2, "zzektr1s_", (ftnlen)808)] + (next - 1);
		} else {
		    basidx = stnbas[(i__1 = level - 1) < 10 && 0 <= i__1 ? 
			    i__1 : s_rnge("stnbas", i__1, "zzektr1s_", (
			    ftnlen)814)] + nbig * stlsiz[(i__2 = level - 1) < 
			    10 && 0 <= i__2 ? i__2 : s_rnge("stlsiz", i__2, 
			    "zzektr1s_", (ftnlen)814)] + (next - nbig - 1) * (
			    stlsiz[(i__3 = level - 1) < 10 && 0 <= i__3 ? 
			    i__3 : s_rnge("stlsiz", i__3, "zzektr1s_", (
			    ftnlen)814)] - 1) + (next - 1);
		}
		++level;
		next = 1;

/*              LEVEL, NEXT, NODE, SUBSIZ, and BASIDX are set. */

	    } else {

/*              We're done with the current subtree.  Pop the stack. */

		--level;
		if (level >= 1) {
		    node = stnode[(i__1 = level - 1) < 10 && 0 <= i__1 ? i__1 
			    : s_rnge("stnode", i__1, "zzektr1s_", (ftnlen)835)
			    ];
		    nkeys = stnkey[(i__1 = level - 1) < 10 && 0 <= i__1 ? 
			    i__1 : s_rnge("stnkey", i__1, "zzektr1s_", (
			    ftnlen)836)];
		    bigsiz = stlsiz[(i__1 = level - 1) < 10 && 0 <= i__1 ? 
			    i__1 : s_rnge("stlsiz", i__1, "zzektr1s_", (
			    ftnlen)837)];
		    nbig = stnbig[(i__1 = level - 1) < 10 && 0 <= i__1 ? i__1 
			    : s_rnge("stnbig", i__1, "zzektr1s_", (ftnlen)838)
			    ];
		    next = stnext[(i__1 = level - 1) < 10 && 0 <= i__1 ? i__1 
			    : s_rnge("stnext", i__1, "zzektr1s_", (ftnlen)839)
			    ];
		    basidx = stnbas[(i__1 = level - 1) < 10 && 0 <= i__1 ? 
			    i__1 : s_rnge("stnbas", i__1, "zzektr1s_", (
			    ftnlen)840)];
		    nkids = nkeys + 1;

/*                 Read in the current node. */

		    zzekpgri_(handle, &node, page);
		}
	    }
	}

/*        On this pass through the loop, we either--- */

/*           - Visited a node for the first time and filled in the */
/*             node. */

/*           - Advanced to a new node that has not yet been visited. */

/*           - Exited from a completed subtree. */

/*        Each of these actions can be performed a finite number of */
/*        times.  Therefore, we made progress toward loop termination. */

    }

/*     The last chore is setting the total number of nodes in the root. */

    base = zzektrbs_(tree);
    i__1 = base + 2;
    i__2 = base + 2;
    dasudi_(handle, &i__1, &i__2, &totnod);
    chkout_("ZZEKTR1S", (ftnlen)8);
    return 0;
} /* zzektr1s_ */

