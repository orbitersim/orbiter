/* zzhsc.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ZZHSC ( Private---Add-only Character Hash ) */
/* Subroutine */ int zzhsc_0_(int n__, integer *hashsz, integer *hedlst, 
	integer *collst, char *items, char *item, integer *itemat, logical *
	new__, integer *avail, ftnlen items_len, ftnlen item_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer node;
    logical full;
    integer i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen);
    extern logical failed_(void);
    logical lfound;
    integer lookat;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen);
    extern logical return_(void);
    extern integer zzhash2_(char *, integer *, ftnlen);

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Manipulate add-only character hash. */

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

/*     None. */

/* $ Keywords */

/*     PRIVATE UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Entry */
/*     --------  ---  -------------------------------------------------- */
/*     HASHSZ     I   ZZHSCINI */
/*     HEDLST    I/O  ZZHSCINI, ZZHSCADD, ZZHSCCHK, ZZHSCINF */
/*     COLLST    I/O  ZZHSCINI, ZZHSCADD, ZZHSCCHK, ZZHSCAVL, ZZHSCINF */
/*     ITEMS     I/O  ZZHSCINI, ZZHSCADD, ZZHSCCHK, ZZHSCINF */
/*     ITEM       I   ZZHSCADD, ZZHSCCHK, ZZHSCINF */
/*     ITEMAT     O   ZZHSCADD, ZZHSCCHK */
/*     NEW        O   ZZHSCADD */
/*     AVAIL      O   ZZHSCAVL, ZZHSCINF */

/*     LBPOOL     P   (All) */

/* $ Detailed_Input */

/*     See the ENTRY points for a discussion of their arguments. */

/* $ Detailed_Output */

/*     See the ENTRY points for a discussion of their arguments. */

/* $ Parameters */

/*     LBPOOL      is the lower bound of the collision list array. */

/* $ Exceptions */

/*     1) If ZZHSC is called directly, the error SPICE(BOGUSENTRY) is */
/*        signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     ZZHSC should never be called directly, but should instead be */
/*     accessed only through its entry points. */

/*     The purpose of this routine is to manipulate add-only character */
/*     hashes used for buffering purposes by various SPICE subsystems. */
/*     This umbrella has the following entry points: */

/*           ZZHSCINI       Initialize a hash. */

/*           ZZHSCADD       Add an item to a hash. */

/*           ZZHSCCHK       Check if a item is in a hash. */

/*           ZZHSCAVL       Get available room in hash. */

/*           ZZHSCINF       Get h/k information about hash. */

/*     An add-only hash consists of the head node pointer list (HEDLST), */
/*     hash node collision list (COLLST), and the item list (ITEMS). */

/*     The function ZZHASH2 computes the index of an element in the head */
/*     node pointer list (HEDLST). */

/*     The HEDLST element at this index contains either 0, indicating */
/*     that there is not yet data for this hash value in the hash, or */
/*     the index of the head node and its item in the hash collision */
/*     (COLLST) and item (ITEMS) lists. */

/*     The COLLST element at the head node index contain either 0, */
/*     indicating that there is only one item for this hash value in the */
/*     hash, or the index of the next node and its item in the hash */
/*     collision (COLLST) and item (ITEMS) lists. */

/*     The COLLST element at the next node index contain either 0, */
/*     indicating that this is the last item for this hash value in the */
/*     hash, or the index of the next node and its item in the hash */
/*     collision (COLLST) and item (ITEMS) lists. */

/*     And so on. */

/*     Pictorially the hash looks like this: */

/*                     List of head   List of hash      List of */
/*                        nodes        collisions        items */

/*                       HEDLST          COLLST          ITEMS */

/*                                     +---------+ */
/*                                     |         | */
/*                                     +---------+ */
/*                                         ... */
/*                                     +---------+ */
/*                                     | 1st Free| */
/*                                     +---------+ */
/*                                     |  Size   | */
/*                     +---------+     +---------+     +---------+ */
/*                     |         |     |         |     |         | */
/*                     +---------+ !=0 +---------+     +---------+ */
/*                  .->|Head Node| -.  |         |     |         | */
/*                  |  +---------+  |  +---------+     +---------+ */
/*       ZZHASH2(ITEM) |         |  |  |         |     |         | */
/*                     +---------+  |  +---------+     +---------+ */
/*                     |         |  `->|Head of  |     |Item     | */
/*                     |         |     |collision| !=0 |corresp. | */
/*                     |         |     |list for | -.  |to head  | */
/*                     |         |     | ITEM    |  |  |of list  | */
/*                     +---------+     +---------+  |  +---------+ */
/*                     |         |     |         |  |  |         | */
/*                     +---------+     +---------+  |  +---------+ */
/*                     |         |     |         |<-'  |         | */
/*                     |         |     |Next Node| !=0 |Next Item| */
/*                     |         |     |         |--.  |         | */
/*                     +---------+     +---------+  |  +---------+ */
/*                     |         |     |         |  |  |         | */
/*                     +---------+     +---------+  |  +---------+ */
/*                     |         |     |         |  |  |         | */
/*                     +---------+     +---------+  |  +---------+ */
/*                     |         |     |Next Node|<-'  |Next Item| */
/*                     +---------+     +---------+ etc +---------+ */
/*                        ...              ...            ... */
/*                     +---------+     +---------+     +---------+ */
/*                     |         |     |         |     |         | */
/*                     +---------+     +---------+     +---------+ */


/* $ Examples */

/*     An add-only hash used together with flat data arrays provides a */
/*     simple, fast-access buffering mechanism. The subsystems that need */
/*     such buffering would normally initialize the hash, add items to */
/*     the hash and buffer data associated with items in flat data */
/*     arrays, and find items in the hash and, if found, use buffered */
/*     data associated with the items rather than computing/getting */
/*     their data again. */

/*     An add-only hash is normally used in one of the following ways -- */
/*     set up to never be filled up, stop buffering when filled up, and */
/*     trash when filled up and start buffering again. Three examples */
/*     below illustrate each of these ways. */

/*     Example 1: Set up to never be filled up */
/*     --------------------------------------- */

/*     C */
/*     C     Parameters: pool lower boundary, hash size and item */
/*     C     size. */
/*     C */
/*           INTEGER               LBPOOL */
/*           PARAMETER           ( LBPOOL = -5 ) */

/*           INTEGER               HSHSIZ */
/*           PARAMETER           ( HSHSIS = 5003 ) */

/*           INTEGER               NAMSIZ */
/*           PARAMETER           ( NAMSIZ = 32 ) */

/*     C */
/*     C     Hash arrays and data buffer. */
/*     C */
/*           INTEGER               HEDLST  (          HSHSIZ ) */
/*           INTEGER               COLLST  ( LBPOOL : HSHSIZ ) */
/*           CHARACTER*(NAMSIZ)    ITEMS   (          HSHSIZ ) */
/*           INTEGER               DATBUF  (          HSHSIZ ) */

/*     C */
/*     C     Miscellaneous variables. */
/*     C */
/*           CHARACTER*(NAMSIZ)    ITEM */

/*           INTEGER               ITEMAT */
/*           INTEGER               MYDATA */

/*           LOGICAL               FIRST */
/*           INTEGER               AVAIL */
/*           LOGICAL               NEW */

/*     C */
/*     C     Data items. */
/*     C */
/*           DATA FIRST             / .TRUE.  / */

/*     C */
/*     C     Initialize hash. */
/*     C */
/*           IF ( FIRST ) THEN */
/*              CALL ZZHSCINI ( HSHSIZ, HEDLST, COLLST ) */
/*              FIRST = .FALSE. */
/*           END IF */
/*           ... */
/*     C */
/*     C     Check for presence of or add an item to the hash. If the */
/*     C     item is in the hash, NEW will be .FALSE. If the item is */
/*     C     not in the hash, it will be added and NEW will be .TRUE. */
/*     C     This call can fail only if the hash fills up which */
/*     C     should not happen because it was declared to never fill */
/*     C     up. */
/*     C */
/*           CALL ZZHSCADD ( HEDLST, COLLST, ITEMS, ITEM, */
/*          .                                ITEMAT, NEW ) */
/*           IF ( FAILED() ) THEN */
/*              RETURN */
/*           END IF */

/*           IF ( .NOT. NEW ) THEN */
/*     C */
/*     C        Simply use buffered value. */
/*     C */
/*              MYDATA = DATBUF( ITEMAT ) */

/*           ELSE */
/*     C */
/*     C        Get value. */
/*     C */
/*              ... */
/*     C */
/*     C        Buffer value. */
/*     C */
/*              DATBUF( ITEMAT ) = MYDATA */

/*           END IF */



/*     Example 2: Stop buffering when filled up */
/*     ---------------------------------------- */

/*     C */
/*     C     Use the same declarations as the first example. */
/*     C */
/*           ... */
/*     C */
/*     C     Initialize hash. */
/*     C */
/*           IF ( FIRST ) THEN */
/*              CALL ZZHSCINI ( HSHSIZ, HEDLST, COLLST ) */
/*              FIRST = .FALSE. */
/*           END IF */
/*           ... */
/*     C */
/*     C     Check if this item is in the hash. */
/*     C */
/*           CALL ZZHSCCHK ( HEDLST, COLLST, ITEMS, ITEM, ITEMAT ) */

/*           IF ( ITEMAT .NE. 0 ) THEN */
/*     C */
/*     C        Simply use buffered value. */
/*     C */
/*              MYDATA = DATBUF( ITEMAT ) */

/*           ELSE */
/*     C */
/*     C        Get value. */
/*     C */
/*              ... */
/*     C */
/*     C        Buffer value, but only if the hash is not full. */
/*     C */
/*              CALL ZZHSCAVL( COLLST, AVAIL ) */
/*              IF ( AVAIL .GT. 0 ) THEN */
/*                 CALL ZZHSCADD ( HEDLST, COLLST, ITEMS, ITEM, */
/*          .                                  ITEMAT, NEW ) */
/*                 DATBUF( ITEMAT ) = MYDATA */
/*              END IF */

/*           END IF */


/*     Example 3: Trash when filled up and start buffering again */
/*     --------------------------------------------------------- */

/*     C */
/*     C     Use the same declarations as the first example. */
/*     C */
/*           ... */
/*     C */
/*     C     Initialize hash. */
/*     C */
/*           IF ( FIRST ) THEN */
/*              CALL ZZHSCINI ( HSHSIZ, HEDLST, COLLST ) */
/*              FIRST = .FALSE. */
/*           END IF */
/*           ... */
/*     C */
/*     C     Check if this item is in the hash. */
/*     C */
/*           CALL ZZHSCCHK ( HEDLST, COLLST, ITEMS, ITEM, ITEMAT ) */

/*           IF ( ITEMAT .NE. 0 ) THEN */
/*     C */
/*     C        Simply use buffered value. */
/*     C */
/*              MYDATA = DATBUF( ITEMAT ) */

/*           ELSE */
/*     C */
/*     C        Get value. */
/*     C */
/*              ... */
/*     C */
/*     C        Buffer value, if the hash is full trash re-initialize */
/*     C        it first. */
/*     C */
/*              CALL ZZHSCAVL( COLLST, AVAIL ) */
/*              IF ( AVAIL .LE. 0 ) THEN */
/*                 CALL ZZHSCINI ( HSHSIZ, HEDLST, COLLST ) */
/*              END IF */
/*              CALL ZZHSCADD ( HEDLST, COLLST, ITEMS, ITEM, */
/*          .                                  ITEMAT, NEW ) */
/*              DATBUF( ITEMAT ) = MYDATA */

/*           END IF */

/* $ Restrictions */

/*     For sake of speed all entry points do no or minimal error */
/*     checking. It is the responsibility of the caller to ensure that */
/*     all inputs are properly initialized. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 12-OCT-2020 (JDR) (BVS) */

/*        Changed entry point ZZHSCINI to not check RETURN() */
/*        on entry. That entry point now initializes the hash */
/*        as long as the size parameter is greater than zero. */

/*        Fixed short error message in umbrella routine. */

/* -    SPICELIB Version 1.0.0, 31-JUL-2013 (BVS) */

/* -& */
/* $ Index_Entries */

/*     manipulate add-only character hash */

/* -& */

/*     Hash control area items. */


/*     SPICELIB functions. */


/*     Local variables. */


/*     Standard SPICE error handling. */

    /* Parameter adjustments */
    if (hedlst) {
	}
    if (items) {
	}

    /* Function Body */
    switch(n__) {
	case 1: goto L_zzhscini;
	case 2: goto L_zzhscadd;
	case 3: goto L_zzhscchk;
	case 4: goto L_zzhscavl;
	case 5: goto L_zzhscinf;
	}

    if (return_()) {
	return 0;
    } else {
	chkin_("ZZHSC", (ftnlen)5);
    }

/*     Signal bogus entry error and check out. */

    sigerr_("SPICE(BOGUSENTRY)", (ftnlen)17);
    chkout_("ZZHSC", (ftnlen)5);
    return 0;
/* $Procedure ZZHSCINI ( Private---Initialize Add-only Character Hash ) */

L_zzhscini:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Initialize an add-only character hash. */

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

/*     None. */

/* $ Keywords */

/*     PRIVATE UTILITY */

/* $ Declarations */

/*     INTEGER               LBPOOL */
/*     PARAMETER           ( LBPOOL = -5 ) */

/*     INTEGER               HASHSZ */
/*     INTEGER               HEDLST  (          * ) */
/*     INTEGER               COLLST  ( LBPOOL : * ) */

/* $ Brief_I/O */

/*     Variable  I/O  Entry */
/*     --------  ---  -------------------------------------------------- */
/*     HASHSZ     I   Hash size */
/*     HEDLST    I/O  Hash head node pointer list */
/*     COLLST    I/O  Hash node collision list */

/*     LBPOOL     P   Collision list array lower bound */

/* $ Detailed_Input */

/*     HASHSZ      is the hash size. For efficiency reasons it must be a */
/*                 prime number in the range from 1 to INTMAX/68 - 1. */

/*     HEDLST */
/*     COLLST      are the head node pointer list and the node collision */
/*                 list components of an add-only character hash. */

/* $ Detailed_Output */

/*     HEDLST */
/*     COLLST      are the head node pointer list and the node collision */
/*                 list components of an add-only character hash with all */
/*                 elements of the head node pointer list HEDLST set to */
/*                 zero and size and first-free elements of the head */
/*                 node pointer list COLLST set to HASHSZ and 1 */
/*                 correspondingly. */

/* $ Parameters */

/*     LBPOOL      is the lower bound of the collision list array. */

/* $ Exceptions */

/*     1) If HASHSZ is less than 1 or greater than the value that is */
/*        guaranteed not to result in eventual integer overflow in */
/*        ZZHASH2, the error will be signaled by ZZHASH2. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     See the header of the umbrella routine ZZHSC. */

/* $ Examples */

/*     See the header of the umbrella routine ZZHSC. */

/* $ Restrictions */

/*     See the header of the umbrella routine ZZHSC. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     B.V. Semenov    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 12-OCT-2020 (BVS) */

/*        Changed to not check RETURN() on entry. The entry point */
/*        now initializes the hash as long as the size parameter */
/*        is greater than zero. */

/* -    SPICELIB Version 1.0.0, 31-JUL-2013 (BVS) */

/* -& */
/* $ Index_Entries */

/*     initialize add-only character hash */

/* -& */

/*     Standard SPICE error handling. */

    chkin_("ZZHSCINI", (ftnlen)8);
    if (*hashsz > 0) {

/*        Wipe out head node pointer list. */

	i__1 = *hashsz;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    hedlst[i__ - 1] = 0;
	}

/*        Reset control area. */

	collst[5] = *hashsz;
	collst[4] = 1;
    }

/*     The requested number of nodes must be in the valid range. If it */
/*     is not, ZZHASH2 will signal an error. */

    i__ = zzhash2_(" ", hashsz, (ftnlen)1);
    if (failed_()) {
	chkout_("ZZHSCINI", (ftnlen)8);
	return 0;
    }
    chkout_("ZZHSCINI", (ftnlen)8);
    return 0;
/* $Procedure ZZHSCADD ( Private---Add Item to Add-only Character Hash ) */

L_zzhscadd:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Find or add, if not present, an item to an add-only character */
/*     hash. */

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

/*     None. */

/* $ Keywords */

/*     PRIVATE UTILITY */

/* $ Declarations */

/*     INTEGER               LBPOOL */
/*     PARAMETER           ( LBPOOL = -5 ) */

/*     INTEGER               HEDLST  (          * ) */
/*     INTEGER               COLLST  ( LBPOOL : * ) */
/*     CHARACTER*(*)         ITEMS   (          * ) */
/*     CHARACTER*(*)         ITEM */
/*     INTEGER               ITEMAT */
/*     LOGICAL               NEW */

/* $ Brief_I/O */

/*     Variable  I/O  Entry */
/*     --------  ---  -------------------------------------------------- */
/*     HEDLST    I/O  Hash head node pointer list */
/*     COLLST    I/O  Hash node collision list */
/*     ITEMS     I/O  Hash item list */
/*     ITEM       I   Item to be checked for/added */
/*     ITEMAT     O   Item index in node collision and item lists */
/*     NEW        O   Flag indicting if item was added */

/*     LBPOOL     P   Collision list array lower bound */

/* $ Detailed_Input */

/*     HEDLST */
/*     COLLST */
/*     ITEMS       are the components of an add-only character */
/*                 hash. */

/*     ITEM        is an item to find and, if needed, add to the hash. */

/* $ Detailed_Output */

/*     HEDLST */
/*     COLLST */
/*     ITEMS       are the components of an add-only character */
/*                 hash with the new item added to it if needed. */

/*     ITEMAT      is the index of the item in the node collision and */
/*                 item lists. If the item could not be found in or */
/*                 added to the hash, ITEMAT is set to 0. */

/*     NEW         is a flag indicting if item was added to the hash. */

/* $ Parameters */

/*     LBPOOL      is the lower bound of the collision list array. */

/* $ Exceptions */

/*     1) If the hash is full, the error SPICE(HASHISFULL) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     See the header of the umbrella routine ZZHSC. */

/* $ Examples */

/*     See the header of the umbrella routine ZZHSC. */

/* $ Restrictions */

/*     See the header of the umbrella routine ZZHSC. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     B.V. Semenov    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 31-JUL-2013 (BVS) */

/* -& */
/* $ Index_Entries */

/*     add an item to add-only character hash */

/* -& */

/*     Standard SPICE error handling. No checking-in here. We will do it */
/*     when we have to. */

    if (return_()) {
	return 0;
    }

/*     Set flag indicating whether the hash is full. */

    full = collst[4] > collst[5];

/*     Use hash function to get index of the head node. */

    lookat = zzhash2_(item, &collst[5], item_len);
    node = hedlst[lookat - 1];

/*     Set initial values. */

    lfound = FALSE_;
    *new__ = FALSE_;

/*     See if this item (or one colliding with it in the hash scheme) */
/*     has already been stored in the item list. */

    if (node > 0) {

/*        Start at the head node and check each item saved for this hash */
/*        value until we find a item that matches or run out of items in */
/*        this conflict resolution list. */

	while(node > 0 && ! lfound) {
	    lfound = s_cmp(items + (node - 1) * items_len, item, items_len, 
		    item_len) == 0;
	    *itemat = node;
	    node = collst[node + 5];
	}

/*        If we didn't find this item on the conflict resolution list */
/*        and our hash is not full we will add this item to it. */

	if (! lfound && ! full) {

/*           Get next free node. */

	    node = collst[4];

/*           Increment next free node pointer. */

	    ++collst[4];

/*           Set current node pointer to the node we just picked for */
/*           this item. */

	    collst[*itemat + 5] = node;

/*           Set new node pointer to 0, just in case. */

	    collst[node + 5] = 0;

/*           Save item. */

	    s_copy(items + (node - 1) * items_len, item, items_len, item_len);

/*           Set output node ID and new and found flags. */

	    *itemat = node;
	    *new__ = TRUE_;
	    lfound = TRUE_;
	}
    } else if (! full) {

/*        Nothing like this item (in the hashing sense) has been stored */
/*        so far and the hash is not full. */

/*        Get next free node. */

	node = collst[4];

/*        Increment next free node pointer. */

	++collst[4];

/*        Set new node pointer to 0, just in case. */

	collst[node + 5] = 0;

/*        Set the head node pointer to this node. */

	hedlst[lookat - 1] = node;

/*        Save item. */

	s_copy(items + (node - 1) * items_len, item, items_len, item_len);

/*        Set output node ID and new and found flags. */

	*itemat = node;
	*new__ = TRUE_;
	lfound = TRUE_;
    }

/*     Set ITEMAT to 0 if LFOUND is FALSE. */

    if (! lfound) {
	*itemat = 0;
    }

/*     If the item hash was full and we didn't find this item we've got */
/*     an error. Report it and return. */

    if (full && ! lfound) {
	chkin_("ZZHSCADD", (ftnlen)8);
	setmsg_("The hash has no room for any more items.", (ftnlen)40);
	sigerr_("SPICE(HASHISFULL)", (ftnlen)17);
	chkout_("ZZHSCADD", (ftnlen)8);
	return 0;
    }
    return 0;
/* $Procedure ZZHSCCHK ( Private---Find Item in Add-only Character Hash ) */

L_zzhscchk:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Find an item in an add-only character hash. */

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

/*     None. */

/* $ Keywords */

/*     PRIVATE UTILITY */

/* $ Declarations */

/*     INTEGER               LBPOOL */
/*     PARAMETER           ( LBPOOL = -5 ) */

/*     INTEGER               HEDLST  (          * ) */
/*     INTEGER               COLLST  ( LBPOOL : * ) */
/*     CHARACTER*(*)         ITEMS   (          * ) */
/*     CHARACTER*(*)         ITEM */
/*     INTEGER               ITEMAT */

/* $ Brief_I/O */

/*     Variable  I/O  Entry */
/*     --------  ---  -------------------------------------------------- */
/*     HEDLST     I   Hash head node pointer list */
/*     COLLST     I   Hash node collision list */
/*     ITEMS      I   Hash item list */
/*     ITEM       I   Item to find */
/*     ITEMAT     O   Item index in node collision and item lists */

/*     LBPOOL     P   Collision list array lower bound */

/* $ Detailed_Input */

/*     HEDLST */
/*     COLLST */
/*     ITEMS       are the components of an add-only character */
/*                 hash. */

/*     ITEM        is an item to find in the hash. */

/* $ Detailed_Output */

/*     ITEMAT      is the index of the item in the node collision and */
/*                 item lists. If item is not in the hash, ITEMAT is set */
/*                 to 0. */

/* $ Parameters */

/*     LBPOOL      is the lower bound of the collision list array. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     See the header of the umbrella routine ZZHSC. */

/* $ Examples */

/*     See the header of the umbrella routine ZZHSC. */

/* $ Restrictions */

/*     See the header of the umbrella routine ZZHSC. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     B.V. Semenov    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 31-JUL-2013 (BVS) */

/* -& */
/* $ Index_Entries */

/*     find item in add-only character hash */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }

/*     Use hash function to get index of the head node. */

    lookat = zzhash2_(item, &collst[5], item_len);
    node = hedlst[lookat - 1];

/*     Set initial values. */

    lfound = FALSE_;

/*     See if this item (or one colliding with it in the hash scheme) is */
/*     in the item list. */

    if (node > 0) {

/*        Start at the head node and check each item saved for this hash */
/*        value until we find a item that matches or run out of items in */
/*        this conflict resolution list. */

	while(node > 0 && ! lfound) {
	    lfound = s_cmp(items + (node - 1) * items_len, item, items_len, 
		    item_len) == 0;
	    *itemat = node;
	    node = collst[node + 5];
	}
    }

/*     If LFOUND is false, set ITEMAT to 0. */

    if (! lfound) {
	*itemat = 0;
    }
    return 0;
/* $Procedure ZZHSCAVL ( Private---Get room available in Add-only Hash ) */

L_zzhscavl:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Get room available in an add-only character hash. */

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

/*     None. */

/* $ Keywords */

/*     PRIVATE UTILITY */

/* $ Declarations */

/*     INTEGER               LBPOOL */
/*     PARAMETER           ( LBPOOL = -5 ) */

/*     INTEGER               COLLST  ( LBPOOL : * ) */
/*     INTEGER               AVAIL */

/* $ Brief_I/O */

/*     Variable  I/O  Entry */
/*     --------  ---  -------------------------------------------------- */
/*     COLLST     I   Hash node collision list */
/*     AVAIL      O   Room available in the hash */

/*     LBPOOL     P   Collision list array lower bound */

/* $ Detailed_Input */

/*     COLLST      is the add-only character hash node collision */
/*                 list. */

/* $ Detailed_Output */

/*     AVAIL       is the room (number of vacant slots) available in */
/*                 the hash. */

/* $ Parameters */

/*     LBPOOL      is the lower bound of the collision list array. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     See the header of the umbrella routine ZZHSC. */

/* $ Examples */

/*     See the header of the umbrella routine ZZHSC. */

/* $ Restrictions */

/*     See the header of the umbrella routine ZZHSC. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     B.V. Semenov    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 31-JUL-2013 (BVS) */

/* -& */
/* $ Index_Entries */

/*     get room available in add-only character hash */

/* -& */

/*     Set the number of unoccupied slots in the hash. */

    *avail = collst[5] - collst[4] + 1;
    return 0;
/* $Procedure ZZHSCINF ( Private---Get Information about Add-only Hash ) */

L_zzhscinf:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Get information about an add-only character hash. */

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

/*     None. */

/* $ Keywords */

/*     PRIVATE UTILITY */

/* $ Declarations */

/*     INTEGER               LBPOOL */
/*     PARAMETER           ( LBPOOL = -5 ) */

/*     INTEGER               HEDLST  (          * ) */
/*     INTEGER               COLLST  ( LBPOOL : * ) */
/*     CHARACTER*(*)         ITEMS   (          * ) */
/*     CHARACTER*(*)         ITEM */
/*     INTEGER               AVAIL */

/* $ Brief_I/O */

/*     Variable  I/O  Entry */
/*     --------  ---  -------------------------------------------------- */
/*     HEDLST     I   Hash head node pointer list */
/*     COLLST     I   Hash node collision list */
/*     ITEMS      I   Hash item list */
/*     ITEM       I   parameter to report */
/*     AVAIL      O   parameter Value */

/*     LBPOOL     P   Collision list array lower bound */

/* $ Detailed_Input */

/*     HEDLST */
/*     COLLST */
/*     ITEMS       are the components of an add-only character */
/*                 hash. */

/*     ITEM        is the parameter to report: */

/*                    'HASH SIZE' */
/*                    'USED HEADNODE COUNT' */
/*                    'UNUSED HEADNODE COUNT' */
/*                    'USED ITEM COUNT' */
/*                    'UNUSED ITEM COUNT' */
/*                    'LONGEST LIST SIZE' */

/* $ Detailed_Output */

/*     AVAIL       is the value of the parameter of interest. */

/* $ Parameters */

/*     LBPOOL      is the lower bound of the collision list array. */

/* $ Exceptions */

/*     1) If the input ITEM is not recognized, the error */
/*        SPICE(ITEMNOTRECOGNIZED) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     See the header of the umbrella routine ZZHSC. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     See the header of the umbrella routine ZZHSC. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     B.V. Semenov    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 31-JUL-2013 (BVS) */

/* -& */
/* $ Index_Entries */

/*     get information about add-only character hash */

/* -& */

/*     Get the hash size. */

    if (s_cmp(item, "HASH SIZE", item_len, (ftnlen)9) == 0) {
	*avail = collst[5];

/*     Get the count of used nodes in the head list. */

    } else if (s_cmp(item, "USED HEADNODE COUNT", item_len, (ftnlen)19) == 0) 
	    {
	*avail = 0;
	i__1 = collst[5];
	for (i__ = 1; i__ <= i__1; ++i__) {
	    if (hedlst[i__ - 1] != 0) {
		++(*avail);
	    }
	}

/*     Get the count of unused nodes in the head list. */

    } else if (s_cmp(item, "UNUSED HEADNODE COUNT", item_len, (ftnlen)21) == 
	    0) {
	*avail = 0;
	i__1 = collst[5];
	for (i__ = 1; i__ <= i__1; ++i__) {
	    if (hedlst[i__ - 1] == 0) {
		++(*avail);
	    }
	}

/*     Get the count of used slots in the item list. */

    } else if (s_cmp(item, "USED ITEM COUNT", item_len, (ftnlen)15) == 0) {
	*avail = collst[4] - 1;

/*     Get the count of unused slots in the item list. */

    } else if (s_cmp(item, "UNUSED ITEM COUNT", item_len, (ftnlen)17) == 0) {
	*avail = collst[5] - collst[4] + 1;

/*     Get the size of the longest item list for any hash value. */

    } else if (s_cmp(item, "LONGEST LIST SIZE", item_len, (ftnlen)17) == 0) {
	*avail = 0;
	i__1 = collst[5];
	for (i__ = 1; i__ <= i__1; ++i__) {
	    node = hedlst[i__ - 1];
	    lookat = 0;
	    while(node > 0) {
		++lookat;
		node = collst[node + 5];
	    }
	    *avail = max(*avail,lookat);
	}

/*     This parameter is not supported. */

    } else {
	*avail = 0;
	chkin_("ZZHSCINF", (ftnlen)8);
	setmsg_("Parameter '#' is not recognized.", (ftnlen)32);
	errch_("#", item, (ftnlen)1, item_len);
	sigerr_("SPICE(ITEMNOTRECOGNIZED)", (ftnlen)24);
	chkout_("ZZHSCINF", (ftnlen)8);
    }
    return 0;
} /* zzhsc_ */

/* Subroutine */ int zzhsc_(integer *hashsz, integer *hedlst, integer *collst,
	 char *items, char *item, integer *itemat, logical *new__, integer *
	avail, ftnlen items_len, ftnlen item_len)
{
    return zzhsc_0_(0, hashsz, hedlst, collst, items, item, itemat, new__, 
	    avail, items_len, item_len);
    }

/* Subroutine */ int zzhscini_(integer *hashsz, integer *hedlst, integer *
	collst)
{
    return zzhsc_0_(1, hashsz, hedlst, collst, (char *)0, (char *)0, (integer 
	    *)0, (logical *)0, (integer *)0, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int zzhscadd_(integer *hedlst, integer *collst, char *items, 
	char *item, integer *itemat, logical *new__, ftnlen items_len, ftnlen 
	item_len)
{
    return zzhsc_0_(2, (integer *)0, hedlst, collst, items, item, itemat, 
	    new__, (integer *)0, items_len, item_len);
    }

/* Subroutine */ int zzhscchk_(integer *hedlst, integer *collst, char *items, 
	char *item, integer *itemat, ftnlen items_len, ftnlen item_len)
{
    return zzhsc_0_(3, (integer *)0, hedlst, collst, items, item, itemat, (
	    logical *)0, (integer *)0, items_len, item_len);
    }

/* Subroutine */ int zzhscavl_(integer *collst, integer *avail)
{
    return zzhsc_0_(4, (integer *)0, (integer *)0, collst, (char *)0, (char *)
	    0, (integer *)0, (logical *)0, avail, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int zzhscinf_(integer *hedlst, integer *collst, char *items, 
	char *item, integer *avail, ftnlen items_len, ftnlen item_len)
{
    return zzhsc_0_(5, (integer *)0, hedlst, collst, items, item, (integer *)
	    0, (logical *)0, avail, items_len, item_len);
    }

