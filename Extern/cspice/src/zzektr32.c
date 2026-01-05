/* zzektr32.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__122 = 122;
static integer c__3 = 3;

/* $Procedure      ZZEKTR32 ( EK tree, 3-2 merge ) */
/* Subroutine */ int zzektr32_(integer *handle, integer *tree, integer *left, 
	integer *middle, integer *right, integer *parent, integer *lpkidx, 
	logical *undrfl)
{
    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer lsib, msib, rsib, root;
    extern /* Subroutine */ int zzekpgfr_(integer *, integer *, integer *), 
	    zzekpgri_(integer *, integer *, integer *), zzekpgwi_(integer *, 
	    integer *, integer *);
    extern integer zzektrbs_(integer *);
    integer i__, n, ppage[256], rbase;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer nnode;
    extern /* Subroutine */ int movei_(integer *, integer *, integer *);
    integer lpkey, psize, rpkey, c1page[256], c2page[256], c3page[256], 
	    datbas, kidbas;
    extern /* Subroutine */ int dasrdi_(integer *, integer *, integer *, 
	    integer *), dasudi_(integer *, integer *, integer *, integer *);
    integer keybas, sizbas;
    extern /* Subroutine */ int sigerr_(char *, ftnlen);
    integer leftsz, lmidsz, midsiz, nlkeys, nmkeys, npkeys, nrkeys, rmidsz, 
	    rshift;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen), chkout_(char *, ftnlen);
    integer sum;

/* $ Abstract */

/*     Execute a 3-2 merge:  merge three neighboring sibling nodes, two */
/*     of which contain the minimum number of keys and one of which */
/*     has an underflow of one key, into two nodes, each one */
/*     approximately full. */

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
/*     MIDDLE     I   Middle sibling node. */
/*     RIGHT      I   Right sibling node. */
/*     PARENT     I   Common parent node. */
/*     LPKIDX     I   Node-relative index of left parent key of MIDDLE. */
/*     UNDRFL     O   Underflow flag. */

/* $ Detailed_Input */

/*     HANDLE         is a file handle of an EK open for write access. */

/*     TREE           is the root node number of the tree of interest. */

/*     LEFT           is the node number of the left node of a trio of */
/*                    siblings.  LEFT either contains the minimum */
/*                    allowed number of keys  or is underflowing by */
/*                    one key. */

/*     MIDDLE         is the node number of the middle node of a trio of */
/*                    siblings.  MIDDLE either contains the minimum */
/*                    allowed number of keys  or is underflowing by */
/*                    one key. */

/*     RIGHT          is the node number of the right node of a trio of */
/*                    siblings.  The total number of keys in nodes */
/*                    LEFT, MIDDLE and RIGHT amounts to an underflow of 1 */
/*                    key. */

/*     PARENT         is the node number of the common parent of LEFT */
/*                    and RIGHT. */

/*     LPKIDX         is the node-relative index within PARENT of the */
/*                    left parent key of MIDDLE.  This key is the */
/*                    immediate predecessor of the first key in the */
/*                    subtree headed by MIDDLE. */

/* $ Detailed_Output */

/*     UNDRFL         is a logical flag indicating whether the parent */
/*                    node underflowed as a result of the 3-2 merge. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If HANDLE is invalid, the error will be diagnosed by routines */
/*         called by this routine.  The file will not be modified. */

/*     2)  If an I/O error occurs while reading the indicated file, the */
/*         error will be diagnosed by routines called by this routine. */

/*     3)  If LEFT and RIGHT are not neighboring siblings, the error */
/*         SPICE(BUG) is signaled. */

/*     4)  If either LEFT or RIGHT are not children of PARENT, the error */
/*         SPICE(BUG) is signaled. */

/*     5)  If the sum of the number of keys in LEFT and RIGHT does not */
/*         correspond to an underflow of exactly 1 key, the error */
/*         SPICE(BUG) is signaled. */

/* $ Files */

/*     See the EK Required Reading for a discussion of the EK file */
/*     format. */

/* $ Particulars */

/*     Deletions from an EK tree start at a leaf node.  If the node */
/*     underflows, the EK system attempts to shuffle keys at the leaf */
/*     level to resolve the underflow.  That attempt failing, the system */
/*     delegates the problem upward to the next higher level. */

/*     There are two ways to resolve underflow in a non-root node: */
/*     balance the underflowing node with one of its closest siblings */
/*     (see ZZEKTRBN), or if the closest siblings contain the minimum */
/*     number of keys, execute a 3-2 merge. */

/*     A 3-2 merge involves deletion of the middle node of a trio of */
/*     neighboring siblings, two of which contain the minimum */
/*     number of keys and one of which has an underflow of one key, */
/*     and redistributing the keys between the two remaining nodes */
/*     and their common parent so that each of the two remaining siblings */
/*     contains the maximum number of keys.  The parent loses a key in */
/*     the process. */

/*     After the 3-2 merge, the tree is balanced and the siblings */
/*     satisfy the key count invariants.  However, the parent of the */
/*     siblings may underflow by one key. */

/*     Below are the gory details concerning the actions of this routine. */
/*     All of the parameters referred to here (in capital letters) are */
/*     defined in the include file ektree.inc. */

/*     In a 3-2 merge: */

/*        - The left parent key of the middle child is rotated down */
/*          into the left child and is appended on the right to the key */
/*          set of that child.  The left parent key's data pointer moves */
/*          along with the key.  The child pointers of this parent key */
/*          do not move along with the key; these pointers point to the */
/*          left and middle child nodes. */

/*        - The keys of the middle child are divided into three sets: */
/*          a set to be rotated left through the parent node into the */
/*          left child, a singleton set consisting of a key to be moved */
/*          up into the parent, and a set of keys to be rotated right */
/*          through the parent into the right child.  The sizes of the */
/*          leftmost and rightmost of these sets differ by at most 1. */

/*        - The number of keys that are rotated left is picked so that */
/*          after the rotation, the size of the left node will be */
/*          (3*MNKEYC)/2.  The data pointers of these keys move along */
/*          with the keys.  All of the left and right child pointers */
/*          of these keys move along with the keys into the left child. */

/*        - The singleton key in the child moves up into the parent */
/*          node.  Its data pointer moves with it.  After the move into */
/*          the parent node, the left child pointer of this key points */
/*          to the left child; the right child pointer points to the */
/*          right child. */

/*        - The right parent key of the middle child is rotated right */
/*          into the right child.  The data pointer of this key moves */
/*          with the key.  The child pointers of this parent key */
/*          do not move along with the key; these pointers point to the */
/*          middle and right child nodes. */

/*        - The remaining keys in the middle child are rotated right */
/*          into the right child; these become the leftmost keys of */
/*          that child.  The data pointers of these keys move along */
/*          with them.  The child pointers of these keys also move */
/*          along with them. */

/*        - The right child ends up with */

/*             (3*MNKEYC) - (3*MNKEYC)/2 */

/*          keys.  This may be deduced from the facts that the original */
/*          three children had between them an underflow of one key, the */
/*          parent lost a key, and the left child has (3*MNKEYC)/2 keys. */

/*          Since */

/*             MNKEYC    =  MNKIDC - 1 */
/*                       =  (  ( 2*MXKIDC + 1 ) / 3   )  -  1 */
/*                       =  ( 2*MXKIDC - 2 ) / 3 */
/*                       =  ( 2*MXKEYC ) / 3 */

/*          we have */

/*             3*MNKEYC  <  2*MXKEYC */
/*                       - */

/*          If 3*MNKEYC is odd, we have strict inequality and also */


/*             3*MNKEYC  =  2 * ( (3*MNKEYC)/2 )  +  1 */

/*          so */

/*             3*MNKEYC  + 1  =  2 * ( (3*MNKEYC)/2 )  +  2 */

/*                            =  2 * (  (3*MNKEYC)/2  +  1 ) */

/*                            <  2 * MXKEYC */
/*                            - */

/*          So in this case, the larger of the child nodes, which has */
/*          size */

/*             (3*MNKEYC)/2  +  1 */

/*          has a key count no greater than MXKEYC. */

/*          If 3*MNKEYC is even, then the left and right child are the */
/*          same size, and the inequality */

/*             3*MNKEYC  <  2*MXKEYC */
/*                       - */

/*          implies directly that both nodes have size no greater than */
/*          MXKEYC. */

/*          Since both child nodes have size approximately 3/2 * MNKEYC, */
/*          and since MNKEYC is approximately 40, the minimum size */
/*          constraints on the child nodes are easily met. */


/*     As the above description shows, the parent loses a key as a */
/*     result of a 3-2 merge.  This may cause the parent to underflow; */
/*     if it does, the underflow at the parent's level must be resolved. */

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

/*        Fixed typo in comments. */

/* -    Beta Version 1.0.0, 16-NOV-1995 (NJB) */

/* -& */

/*     Non-SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Use discovery check-in for speed. */

/*     The plan is to take three sibling nodes, two of which contain */
/*     the minimum number of keys and one of which is underflowing by one */
/*     key, and to merge these nodes into two nodes.  This process */
/*     reduces the number of nodes in the parent by one and may cause the */
/*     parent to underflow. */

/*     After the merge, the sum of the numbers of keys in the two */
/*     children will be exactly (3*MNKEYC).  The numbers of keys in the */
/*     left and right nodes will be, respectively: */


/*        LSIZE  =   INT (  (3*MNKEYC)/2  ) */
/*        RSIZE  =   (3*MNKEYC)  - LSIZE */

/*     We need to be sure that LSIZE and RSIZE are in the range */

/*        MNKEYC : MXKEYC */


/*     The definition of LSIZE implies that */

/*        LSIZE  =  MNKEYC + INT ( MNKEYC/2 ) */


/*     so */

/*        MNKEYC + INT ( MNKEYC/2 )  <  LSIZE <  (3/2)*MNKEYC */
/*                                   -        - */

/*     and since */

/*        MNKEYC  =  MNKIDC - 1 */
/*                =  INT ( ( 2*MXKIDC + 1 ) / 3 ) - 1 */
/*                =  INT ( ( 2*MXKEYC + 3 ) / 3 ) - 1 */
/*                =  INT ( ( 2*MXKEYC     ) / 3 ) */

/*     we have */

/*        (3/2) * MNKEYC  =  (3/2) * INT ( (2*MXKEYC) / 3 )  <  MXKEYC */
/*                                                           - */

/*     Thus  LSIZE is guaranteed to be in range. */

/*     When MNKEYC is even, RSIZE is equal to LSIZE and thus is */
/*     within bounds.  When MNKEYC is odd, RSIZE exceeds LSIZE by 1, so */

/*        MNKEYC  <  RSIZE */


/*     It remains to be shown that */

/*        RSIZE   <  MXKEYC */
/*                - */

/*     when MNKEYC is odd.  When this is the case, the quantity */

/*        (3/2) * MNKEYC */

/*     is not an integer and therefore is strictly less than MXKEYC. */
/*     This quantity is also greater than LSIZE, so we conclude that */

/*        LSIZE  <  MXKEYC - 1 */
/*               - */

/*     Since RSIZE exceeds LSIZE by 1, we have */

/*        RSIZE  <  MXKEYC */
/*               - */

/*     as we claimed. */


/*     All right, read in the child and parent pages. */

    zzekpgri_(handle, left, c1page);
    zzekpgri_(handle, middle, c2page);
    zzekpgri_(handle, right, c3page);
    zzekpgri_(handle, parent, ppage);

/*     The actual addresses in the parent node depend on whether the */
/*     parent is the root.  Compute the necessary bases to avoid a lot */
/*     of cases. */

    root = *tree;
    if (*parent == root) {
	keybas = 5;
	datbas = 172;
	kidbas = 88;
	sizbas = 5;
    } else {
	keybas = 1;
	datbas = 128;
	kidbas = 64;
	sizbas = 1;
    }

/*     Check the left parent key of the middle child. */

    psize = ppage[(i__1 = sizbas - 1) < 256 && 0 <= i__1 ? i__1 : s_rnge(
	    "ppage", i__1, "zzektr32_", (ftnlen)443)];
    if (*lpkidx < 1 || *lpkidx > psize - 1) {
	chkin_("ZZEKTR32", (ftnlen)8);
	setmsg_("Left parent key of MIDDLE is out of range.  Value is #; val"
		"id range is 1:#", (ftnlen)74);
	errint_("#", lpkidx, (ftnlen)1);
	i__1 = psize - 1;
	errint_("#", &i__1, (ftnlen)1);
	sigerr_("SPICE(BUG)", (ftnlen)10);
	chkout_("ZZEKTR32", (ftnlen)8);
	return 0;
    }

/*     Retain the left and right parent key values of the middle child. */

    lpkey = ppage[(i__1 = keybas + *lpkidx - 1) < 256 && 0 <= i__1 ? i__1 : 
	    s_rnge("ppage", i__1, "zzektr32_", (ftnlen)461)];
    rpkey = ppage[(i__1 = keybas + *lpkidx) < 256 && 0 <= i__1 ? i__1 : 
	    s_rnge("ppage", i__1, "zzektr32_", (ftnlen)462)];

/*     Verify that LEFT, MIDDLE, and RIGHT are siblings, and that PARENT */
/*     is their common parent. */

    lsib = ppage[(i__1 = kidbas + *lpkidx - 1) < 256 && 0 <= i__1 ? i__1 : 
	    s_rnge("ppage", i__1, "zzektr32_", (ftnlen)468)];
    msib = ppage[(i__1 = kidbas + *lpkidx) < 256 && 0 <= i__1 ? i__1 : s_rnge(
	    "ppage", i__1, "zzektr32_", (ftnlen)469)];
    rsib = ppage[(i__1 = kidbas + *lpkidx + 1) < 256 && 0 <= i__1 ? i__1 : 
	    s_rnge("ppage", i__1, "zzektr32_", (ftnlen)470)];
    if (lsib != *left || msib != *middle || rsib != *right) {
	chkin_("ZZEKTR32", (ftnlen)8);
	setmsg_("LEFT, RIGHT, MIDDLE, PARENT, and PKIDX are inconsistent. LE"
		"FT = #; MIDDLE = #; RIGHT = #; PARENT = #; LPKIDX = #; LSIB "
		"derived from PARENT = #; MSIB = #; RSIB = #.", (ftnlen)163);
	errint_("#", left, (ftnlen)1);
	errint_("#", middle, (ftnlen)1);
	errint_("#", right, (ftnlen)1);
	errint_("#", parent, (ftnlen)1);
	errint_("#", lpkidx, (ftnlen)1);
	errint_("#", &lsib, (ftnlen)1);
	errint_("#", &msib, (ftnlen)1);
	errint_("#", &rsib, (ftnlen)1);
	sigerr_("SPICE(BUG)", (ftnlen)10);
	chkout_("ZZEKTR32", (ftnlen)8);
	return 0;
    }

/*     Get the number of keys in the parent. */

    if (*parent == root) {
	npkeys = ppage[4];
    } else {
	npkeys = ppage[0];
    }

/*     Get the number of keys in each child. */

    nlkeys = c1page[0];
    nmkeys = c2page[0];
    nrkeys = c3page[0];
    sum = nlkeys + nmkeys + nrkeys;

/*     The sum of the number of keys in the three input nodes must */
/*     sum exactly to value representing an underflow level of 1 key. */

    if (sum != 122) {
	chkin_("ZZEKTR32", (ftnlen)8);
	setmsg_("Number of keys in nodes LEFT = #; in MIDDLE = #; in RIGHT ="
		" #; counts summing to # were expected.", (ftnlen)97);
	errint_("#", &nlkeys, (ftnlen)1);
	errint_("#", &nmkeys, (ftnlen)1);
	errint_("#", &nrkeys, (ftnlen)1);
	errint_("#", &c__122, (ftnlen)1);
	sigerr_("SPICE(BUG)", (ftnlen)10);
	chkout_("ZZEKTR32", (ftnlen)8);
	return 0;
    }

/*     We're set to carry out the merge.  Here's an overview of what */
/*     gets moved where. */

/*        The left parent key of the middle node moves into the left */
/*        node, at the end of the node. */

/*        The first N-1 keys and N child pointers of the middle node get */
/*        moved into the left node, where */

/*           N  =  LSIZE - ( 1 + NLKEYS ) + 1 */

/*        The Nth key of the middle node moves into the parent, */
/*        replacing the left parent key of the middle node. */

/*        The right parent key of the middle node moves into the right */
/*        node, at the beginning of the node. */

/*        The keys from position N+1 onward in the middle node, as */
/*        well as all of the remaining child pointers, move into the */
/*        right node, at the beginning. */

/*        The right parent key's location is filled in by shifting */
/*        the keys, data pointers, and child pointers in the parent */
/*        to the left by one position.  The child pointer removed by this */
/*        operation is the pointer to the middle child. */

/*        The middle child node disappears. */

/*     Before re-arranging things, we'll need to have on hand the key */
/*     counts for various sets of keys.  We'll use the variable LEFTSZ */
/*     for the number of keys in the subtree headed by LEFT.  We'll */
/*     use the variable LMIDSZ to refer to the `subtree' headed by */
/*     the set of keys in the middle node that will be shifted into */
/*     the left child.  The variable RMSIZE will represent the size of */
/*     the key set moved from the middle child into the right child. */
/*     MIDSIZ will be the key count for the subtree headed by the middle */
/*     child. */

/*     Consistent with usage above, the variable N will represent */
/*     the index of the key in the middle node that will rapturously */
/*     ascend into the parent. */

    if (*lpkidx == 1) {
	leftsz = lpkey - 1;
    } else {
	leftsz = lpkey - ppage[(i__1 = keybas + *lpkidx - 2) < 256 && 0 <= 
		i__1 ? i__1 : s_rnge("ppage", i__1, "zzektr32_", (ftnlen)581)]
		 - 1;
    }
    n = 61 - (nlkeys + 1) + 1;
    lmidsz = c2page[(i__1 = n) < 256 && 0 <= i__1 ? i__1 : s_rnge("c2page", 
	    i__1, "zzektr32_", (ftnlen)586)] - 1;
    midsiz = rpkey - lpkey - 1;
    rmidsz = midsiz - lmidsz - 1;

/*     Move the left parent key into the left child.  The key itself */
/*     doesn't really move; its value is simply re-assigned.  The */
/*     data pointer is copied, however.  The child pointer at location */
/*     LSIZE+1 is unaffected by this move. */

    c1page[(i__1 = nlkeys + 1) < 256 && 0 <= i__1 ? i__1 : s_rnge("c1page", 
	    i__1, "zzektr32_", (ftnlen)596)] = leftsz + 1;
    c1page[(i__1 = nlkeys + 128) < 256 && 0 <= i__1 ? i__1 : s_rnge("c1page", 
	    i__1, "zzektr32_", (ftnlen)597)] = ppage[(i__2 = datbas + *lpkidx 
	    - 1) < 256 && 0 <= i__2 ? i__2 : s_rnge("ppage", i__2, "zzektr32_"
	    , (ftnlen)597)];

/*     Move the first N-1 keys and data pointers, and the first N */
/*     child pointers, from the middle child into the left */
/*     child.  The moved keys will gain LEFTSZ + 1 predecessors. */

    i__1 = n - 1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	c1page[(i__2 = nlkeys + 2 + i__ - 1) < 256 && 0 <= i__2 ? i__2 : 
		s_rnge("c1page", i__2, "zzektr32_", (ftnlen)605)] = c2page[(
		i__3 = i__) < 256 && 0 <= i__3 ? i__3 : s_rnge("c2page", i__3,
		 "zzektr32_", (ftnlen)605)] + leftsz + 1;
    }
    i__2 = n - 1;
    movei_(&c2page[128], &i__2, &c1page[(i__1 = nlkeys + 129) < 256 && 0 <= 
	    i__1 ? i__1 : s_rnge("c1page", i__1, "zzektr32_", (ftnlen)608)]);
    movei_(&c2page[64], &n, &c1page[(i__1 = nlkeys + 65) < 256 && 0 <= i__1 ? 
	    i__1 : s_rnge("c1page", i__1, "zzektr32_", (ftnlen)609)]);

/*     Set the key count in the left child. */

    c1page[0] = 61;

/*     The left child is complete.  Now it's time to set up the right */
/*     child.  First off, we'll shift the node's contents to the right */
/*     by the number of new keys we're going to insert.  Shift the */
/*     rightmost elements first.  The shifted keys will gain RMIDSZ+1 */
/*     predecessors, so  we adjust the keys as we shift them. */

    rshift = nmkeys - n + 1;
    for (i__ = nrkeys; i__ >= 1; --i__) {
	c3page[(i__1 = rshift + 1 + i__ - 1) < 256 && 0 <= i__1 ? i__1 : 
		s_rnge("c3page", i__1, "zzektr32_", (ftnlen)626)] = c3page[(
		i__2 = i__) < 256 && 0 <= i__2 ? i__2 : s_rnge("c3page", i__2,
		 "zzektr32_", (ftnlen)626)] + rmidsz + 1;
    }
    for (i__ = nrkeys; i__ >= 1; --i__) {
	c3page[(i__1 = rshift + 128 + i__ - 1) < 256 && 0 <= i__1 ? i__1 : 
		s_rnge("c3page", i__1, "zzektr32_", (ftnlen)630)] = c3page[(
		i__2 = i__ + 127) < 256 && 0 <= i__2 ? i__2 : s_rnge("c3page",
		 i__2, "zzektr32_", (ftnlen)630)];
    }
    for (i__ = nrkeys + 1; i__ >= 1; --i__) {
	c3page[(i__1 = rshift + 64 + i__ - 1) < 256 && 0 <= i__1 ? i__1 : 
		s_rnge("c3page", i__1, "zzektr32_", (ftnlen)634)] = c3page[(
		i__2 = i__ + 63) < 256 && 0 <= i__2 ? i__2 : s_rnge("c3page", 
		i__2, "zzektr32_", (ftnlen)634)];
    }

/*     The key at location RSHIFT receives the former right parent key */
/*     of the middle child.   The key value is simply assigned; the */
/*     data pointer is copied.  The child pointer at location RSHIFT */
/*     will be set later. */

    c3page[(i__1 = rshift) < 256 && 0 <= i__1 ? i__1 : s_rnge("c3page", i__1, 
	    "zzektr32_", (ftnlen)643)] = rmidsz + 1;
    c3page[(i__1 = rshift + 127) < 256 && 0 <= i__1 ? i__1 : s_rnge("c3page", 
	    i__1, "zzektr32_", (ftnlen)644)] = ppage[(i__2 = datbas + *lpkidx)
	     < 256 && 0 <= i__2 ? i__2 : s_rnge("ppage", i__2, "zzektr32_", (
	    ftnlen)644)];

/*     The first RSHIFT-1 locations in the right child are filled in */
/*     with data from the middle child.  The moved keys lose LMIDSZ+1 */
/*     predecessors. */

    i__1 = rshift - 1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	c3page[(i__2 = i__) < 256 && 0 <= i__2 ? i__2 : s_rnge("c3page", i__2,
		 "zzektr32_", (ftnlen)652)] = c2page[(i__3 = n + 1 + i__ - 1) 
		< 256 && 0 <= i__3 ? i__3 : s_rnge("c2page", i__3, "zzektr32_"
		, (ftnlen)652)] - lmidsz - 1;
    }
    i__2 = rshift - 1;
    movei_(&c2page[(i__1 = n + 128) < 256 && 0 <= i__1 ? i__1 : s_rnge("c2pa"
	    "ge", i__1, "zzektr32_", (ftnlen)655)], &i__2, &c3page[128]);
    movei_(&c2page[(i__1 = n + 64) < 256 && 0 <= i__1 ? i__1 : s_rnge("c2page"
	    , i__1, "zzektr32_", (ftnlen)656)], &rshift, &c3page[64]);

/*     Update the key count in the right child. */

    c3page[0] = 62;

/*     The right child is complete.  It's time to update the parent. */

/*     The key at location N in the middle child replaces the left parent */
/*     key.  The key value is actually re-assigned; the data pointer does */
/*     move.  The left parent key increases by the number of keys moved */
/*     into the subtree headed by the left child. */

    ppage[(i__1 = keybas + *lpkidx - 1) < 256 && 0 <= i__1 ? i__1 : s_rnge(
	    "ppage", i__1, "zzektr32_", (ftnlen)671)] = lpkey + lmidsz + 1;
    ppage[(i__1 = datbas + *lpkidx - 1) < 256 && 0 <= i__1 ? i__1 : s_rnge(
	    "ppage", i__1, "zzektr32_", (ftnlen)672)] = c2page[(i__2 = n + 
	    127) < 256 && 0 <= i__2 ? i__2 : s_rnge("c2page", i__2, "zzektr3"
	    "2_", (ftnlen)672)];

/*     The parent keys, data pointers, and child pointers at locations */
/*     LPKIDX+2 onward get shifted left by one position.  The keys lose */
/*     no  predecessors as the result of these re-arrangements. */

    i__1 = npkeys - 1;
    for (i__ = *lpkidx + 1; i__ <= i__1; ++i__) {
	ppage[(i__2 = keybas + i__ - 1) < 256 && 0 <= i__2 ? i__2 : s_rnge(
		"ppage", i__2, "zzektr32_", (ftnlen)680)] = ppage[(i__3 = 
		keybas + i__) < 256 && 0 <= i__3 ? i__3 : s_rnge("ppage", 
		i__3, "zzektr32_", (ftnlen)680)];
    }
    i__1 = npkeys - 1;
    for (i__ = *lpkidx + 1; i__ <= i__1; ++i__) {
	ppage[(i__2 = datbas + i__ - 1) < 256 && 0 <= i__2 ? i__2 : s_rnge(
		"ppage", i__2, "zzektr32_", (ftnlen)684)] = ppage[(i__3 = 
		datbas + i__) < 256 && 0 <= i__3 ? i__3 : s_rnge("ppage", 
		i__3, "zzektr32_", (ftnlen)684)];
    }
    i__1 = npkeys;
    for (i__ = *lpkidx + 1; i__ <= i__1; ++i__) {
	ppage[(i__2 = kidbas + i__ - 1) < 256 && 0 <= i__2 ? i__2 : s_rnge(
		"ppage", i__2, "zzektr32_", (ftnlen)688)] = ppage[(i__3 = 
		kidbas + i__) < 256 && 0 <= i__3 ? i__3 : s_rnge("ppage", 
		i__3, "zzektr32_", (ftnlen)688)];
    }

/*     Zero out the freed locations. */

    ppage[(i__1 = keybas + npkeys - 1) < 256 && 0 <= i__1 ? i__1 : s_rnge(
	    "ppage", i__1, "zzektr32_", (ftnlen)694)] = 0;
    ppage[(i__1 = datbas + npkeys - 1) < 256 && 0 <= i__1 ? i__1 : s_rnge(
	    "ppage", i__1, "zzektr32_", (ftnlen)695)] = 0;
    ppage[(i__1 = kidbas + npkeys) < 256 && 0 <= i__1 ? i__1 : s_rnge("ppage",
	     i__1, "zzektr32_", (ftnlen)696)] = 0;

/*     The only required change to the parent's metadata is */
/*     updating the key count.  At this point, we can set the */
/*     underflow flag, depending on the status of the parent. */

    if (*parent == root) {
	--ppage[4];
	*undrfl = ppage[4] == 0;
    } else {
	--ppage[0];
	*undrfl = ppage[0] == 40;
    }

/*     The last change we must make is to update the node count in */
/*     the root. */

    if (*parent == root) {
	--ppage[1];
    } else {

/*        We won't read in the whole root page; we'll just get the */
/*        base address of the root and update the affected location. */

	rbase = zzektrbs_(&root);
	i__1 = rbase + 2;
	i__2 = rbase + 2;
	dasrdi_(handle, &i__1, &i__2, &nnode);
	i__1 = rbase + 2;
	i__2 = rbase + 2;
	i__3 = nnode - 1;
	dasudi_(handle, &i__1, &i__2, &i__3);
    }

/*     Write out our updates. */

    zzekpgwi_(handle, parent, ppage);
    zzekpgwi_(handle, left, c1page);
    zzekpgwi_(handle, right, c3page);

/*     Free the page used by the middle child. */

    zzekpgfr_(handle, &c__3, middle);
    return 0;
} /* zzektr32_ */

