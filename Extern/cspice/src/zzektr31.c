/* zzektr31.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__81 = 81;
static integer c__3 = 3;

/* $Procedure      ZZEKTR31 ( EK tree, 3-1 merge ) */
/* Subroutine */ int zzektr31_(integer *handle, integer *tree)
{
    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer root;
    extern /* Subroutine */ int zzekpgfr_(integer *, integer *, integer *), 
	    zzekpgri_(integer *, integer *, integer *), zzekpgwi_(integer *, 
	    integer *, integer *);
    integer i__, child[2], delta;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer rpage[256];
    extern /* Subroutine */ int movei_(integer *, integer *, integer *);
    integer c1page[256], c2page[256], middle;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    integer nlkeys, nrkeys;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen);
    integer sum;

/* $ Abstract */

/*     Execute a 3-1 merge:  move the contents of two children into */
/*     the root node and delete the children. */

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

/*     2)  If an I/O error occurs while reading the indicated file, the */
/*         error will be diagnosed by routines called by this routine. */

/*     3)  If there is not exactly 1 key in the root at the time this */
/*         routine is called, the error SPICE(BUG) is signaled. */

/* $ Files */

/*     See the EK Required Reading for a discussion of the EK file */
/*     format. */

/* $ Particulars */

/*     Deletions from an EK tree start at a leaf node.  If the node */
/*     underflows, the EK system attempts to shuffle keys at the leaf */
/*     level to resolve the underflow.  That attempt failing, the system */
/*     delegates the problem upward to the next higher level.  Underflow */
/*     may occur there as well; if it does, the problem gets passed */
/*     upward again.  If the root has only two children and one of these */
/*     underflows, the system reduces the height of the tree by */
/*     executing what's called a `3-1' merge:  the root loses its two */
/*     children, and all of the keys in the children are moved into */
/*     the root.  The former grandchildren of the root become */
/*     children of the root. */

/*     A tree is eligible for a 3-1 merge only if the root has exactly */
/*     two children, and the sum of the key counts of the children */
/*     constitutes an underflow of 1 key:  that is, the sum is */

/*        2*MNKEYC - 1 */

/*     After the 3-1 merge, the tree is balanced and all invariants */
/*     relating to key counts are restored. */

/*     The tree grows shorter by one level as a result of a 3-1 merge; */
/*     this is the only circumstance under which the tree grows shorter. */

/*     Below are the gory details concerning the actions of this routine. */
/*     All of the parameters referred to here (in capital letters) are */
/*     defined in the include file ektree.inc. */

/*     In a 3-1 merge: */


/*        - The keys of the left child are moved into the root.  These */
/*          become the leftmost MNKEYC or MNKEYC-1 keys of the root, */
/*          depending on whether the underflow occurred in the left */
/*          child. */

/*        - The data values associated with the keys of the left child */
/*          of the root are moved into the root along with the keys. */

/*        - The left child pointers associated with the keys of the left */
/*          child of the root are moved into the root along with the */
/*          keys. */

/*        - The last right child pointer in the left child of the root */
/*          the root is moved to location NLEFT+1 in the child pointer */
/*          array of the root, where NLEFT is the number of keys in */
/*          the former left child.  This pointer overwrites the root's */
/*          pointer to the left child. */

/*        - The keys of the right child are moved into the root.  These */
/*          become the rightmost MNKEYC or MNKEYC-1 keys of the root, */
/*          depending on whether the underflow occurred in the right */
/*          child. */

/*        - The data values associated with the keys of the right child */
/*          of the root are moved into the root along with the keys. */

/*        - The left child pointers associated with the keys of the right */
/*          child of the root are moved into the root along with the */
/*          keys.  The first of these pointers overwrites the root's */
/*          pointer to the right child. */

/*        - The last right child pointer in the right child of the root */
/*          the root is moved to location 2*MNKEYC+1 in the child pointer */
/*          array of the root. */

/*        - The former children of the root are deleted. */

/*     As the above list shows, the root contains the maximum allowed */
/*     number of keys after a 3-1 merge.  This is because */

/*        MXKEYR  =  MXKIDR - 1 */

/*                =  2 * ( (2*MXKIDC - 2)/3 ) */

/*                =  2 * ( (2*MXKIDC + 1)/3  - 1 ) */

/*                =  2 * ( MNKIDC - 1) */

/*                =  2 * MNKEYC */

/*     Our assumptions were that there was one key in the root and */
/*     that the sum of the key counts of the two children of the root */
/*     was */

/*        ( 2 * MNKEYC ) - 1 */

/*     Thus the size constraints on the root node are met. */

/* $ Examples */

/*     See ZZEKTRDL. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB) */

/*        Corrected typos in comments. */

/* -    Beta Version 1.0.0, 27-OCT-1995 (NJB) */

/* -& */

/*     Local variables */


/*     Use discovery check-in for speed. */

    root = *tree;
    zzekpgri_(handle, &root, rpage);
    nrkeys = rpage[4];

/*     There must be exactly 1 key in the root. */

    if (nrkeys != 1) {
	chkin_("ZZEKTR31", (ftnlen)8);
	setmsg_("Number of keys in root = #; should be 1.", (ftnlen)40);
	errint_("#", &nrkeys, (ftnlen)1);
	sigerr_("SPICE(BUG)", (ftnlen)10);
	chkout_("ZZEKTR31", (ftnlen)8);
	return 0;
    }

/*     Read in the child pages.  Get the key counts for these pages. */

    child[0] = rpage[88];
    child[1] = rpage[89];
    zzekpgri_(handle, child, c1page);
    zzekpgri_(handle, &child[1], c2page);
    nlkeys = c1page[0];
    nrkeys = c2page[0];
    sum = nlkeys + nrkeys;

/*     The sum of the number of keys in the two input nodes must */
/*     sum exactly to value representing an underflow level of 1 key. */

    if (sum != 81) {
	chkin_("ZZEKTR31", (ftnlen)8);
	setmsg_("Number of keys in nodes LEFT = #; in RIGHT = #; counts summ"
		"ing to # were expected.", (ftnlen)82);
	errint_("#", &nlkeys, (ftnlen)1);
	errint_("#", &nrkeys, (ftnlen)1);
	errint_("#", &c__81, (ftnlen)1);
	sigerr_("SPICE(BUG)", (ftnlen)10);
	chkout_("ZZEKTR31", (ftnlen)8);
	return 0;
    }

/*     Shift the key and data pointer in the root to right to allow */
/*     insertion of NLKEYS new entries on the left.  The child pointers */
/*     need not be shifted; they'll be overwritten later. */

    rpage[(i__1 = nlkeys + 5) < 256 && 0 <= i__1 ? i__1 : s_rnge("rpage", 
	    i__1, "zzektr31_", (ftnlen)283)] = rpage[5];
    rpage[(i__1 = nlkeys + 172) < 256 && 0 <= i__1 ? i__1 : s_rnge("rpage", 
	    i__1, "zzektr31_", (ftnlen)284)] = rpage[172];

/*     Copy in the keys, data pointers, and child pointers from the */
/*     left child into the root.  The number of predecessors of the */
/*     new keys is unchanged by this operation. */

    movei_(&c1page[1], &nlkeys, &rpage[5]);
    movei_(&c1page[128], &nlkeys, &rpage[172]);
    i__1 = nlkeys + 1;
    movei_(&c1page[64], &i__1, &rpage[88]);

/*     Copy in the keys, data pointers, and child pointers from the */
/*     right child into the root.  The number of predecessors of the */
/*     new keys is increased by the value of the last key already */
/*     present. */

    middle = nlkeys + 1;
    delta = rpage[(i__1 = middle + 4) < 256 && 0 <= i__1 ? i__1 : s_rnge(
	    "rpage", i__1, "zzektr31_", (ftnlen)302)];
    i__1 = nrkeys;
    for (i__ = 1; i__ <= i__1; ++i__) {
	rpage[(i__2 = middle + 5 + i__ - 1) < 256 && 0 <= i__2 ? i__2 : 
		s_rnge("rpage", i__2, "zzektr31_", (ftnlen)305)] = c2page[(
		i__3 = i__) < 256 && 0 <= i__3 ? i__3 : s_rnge("c2page", i__3,
		 "zzektr31_", (ftnlen)305)] + delta;
    }
    movei_(&c2page[128], &nrkeys, &rpage[(i__1 = middle + 172) < 256 && 0 <= 
	    i__1 ? i__1 : s_rnge("rpage", i__1, "zzektr31_", (ftnlen)308)]);
    i__2 = nrkeys + 1;
    movei_(&c2page[64], &i__2, &rpage[(i__1 = middle + 88) < 256 && 0 <= i__1 
	    ? i__1 : s_rnge("rpage", i__1, "zzektr31_", (ftnlen)309)]);

/*     Now the root must be updated.  The root now contains */
/*     the maximum allowed number of keys.  The depth of the tree */
/*     has decreased, as well as the number of nodes in the tree. */

    rpage[4] = 82;
    --rpage[3];
    rpage[1] += -2;

/*     Write out the updated root. */

    zzekpgwi_(handle, &root, rpage);

/*     Free the pages occupied by the deleted children. */

    for (i__ = 1; i__ <= 2; ++i__) {
	zzekpgfr_(handle, &c__3, &child[(i__1 = i__ - 1) < 2 && 0 <= i__1 ? 
		i__1 : s_rnge("child", i__1, "zzektr31_", (ftnlen)329)]);
    }
    return 0;
} /* zzektr31_ */

