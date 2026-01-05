/* locati.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure LOCATI ( Locate an identifier in a list ) */
/* Subroutine */ int locati_(integer *id, integer *idsz, integer *list, 
	integer *pool, integer *at, logical *presnt)
{
    /* System generated locals */
    integer list_dim1, list_offset, i__1;

    /* Local variables */
    integer head;
    logical same, more;
    integer last, i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer nfree;
    extern /* Subroutine */ int lnkan_(integer *, integer *);
    integer psize;
    extern /* Subroutine */ int lnkilb_(integer *, integer *, integer *);
    extern integer lnknfn_(integer *);
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen);
    extern integer lnksiz_(integer *);
    extern /* Subroutine */ int lnkxsl_(integer *, integer *, integer *);
    integer new__;

/* $ Abstract */

/*     Find a given identifier, which consists of an integer array, */
/*     within a list of such identifiers, or insert the identifier */
/*     into the list. Return the location of the identifier and a flag */
/*     indicating whether or not the identifier was already present. */

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

/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     ID         I   An array of integers that comprise an identifier. */
/*     IDSZ       I   The number of integer components per identifier. */
/*     LIST      I-O  A list of known identifiers. */
/*     POOL      I-O  A doubly linked list used for search LIST. */
/*     AT        I-O  Location of the ID in the LIST. */
/*     PRESNT     O   Flag indicating if ID was already in LIST. */

/* $ Detailed_Input */

/*     ID       is an integer array that serves as an identifier */
/*              for some object. For example it might be a SPICE */
/*              id code for a planet or satellite; it might be the */
/*              instrument id and mode of operation of an instrument. */
/*              See the $Examples section for more details. */

/*     IDSZ     is the number of components in the array ID. */

/*     LIST     is an array containing several ID's. The array */
/*              should be declared so as to have the same upper */
/*              bound at least as large as the upper bound used */
/*              in the declaration of POOL. */

/*     POOL     is a linked list pool that gives the search order */
/*              for examining LIST to locate ID's. The declaration */
/*              of POOL and LIST need to be compatible. Normally, */
/*              the declaration should look like this: */

/*                 INTEGER   LIST (IDSZ,         LSTSIZ ) */
/*                 INTEGER   POOL (   2, LBPOOL: LSTSIZ ) */

/*              If POOL is declared with the statement */

/*                 INTEGER   POOL (   2, LBPOOL: PSIZE  ) */

/*              then you must make sure that PSIZE is less than */
/*              or equal to LSTSIZ. */

/*              POOL should be initialized before the first */
/*              call to this routine with the SPICE routine */
/*              LNKINI. */

/*     AT       is a value that is set by this routine and that */
/*              you should never reset yourself. It points */
/*              to the head of the linked list used for */
/*              searching LIST. Changing AT will destroy the */
/*              link between POOL and LIST. */

/*              There is one exception to these restrictions. */
/*              The first call to this routine that occurs after */
/*              initializing POOL, AT may have any value. It will */
/*              be set upon output and from that time on, you should */
/*              not alter its value except by calling this routine */
/*              to do so. */

/* $ Detailed_Output */

/*     AT       on output AT points to the location in LIST */
/*              of ID. */

/*     PRESNT   is a logical flag. It indicates whether or not */
/*              ID was already present in the LIST when this */
/*              routine was called. If ID was already in LIST */
/*              PRESNT is returned with the value .TRUE. Otherwise */
/*              it is returned with the value .FALSE. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the value of AT is less than zero or greater than */
/*         the declared size of POOL (except immediately after */
/*         initializing or re-initializing POOL), the */
/*         error SPICE(ADDRESSOUTOFBOUNDS) is signaled. */

/*     2)  If the linked list pool POOL is corrupted by a higher */
/*         level routine, a diagnosis of the problem will be */
/*         made by a routine called by this one. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine serves as a utility for managing the bookkeeping */
/*     needed when using a local buffering scheme which removes */
/*     the last used item when the local buffer becomes full. */

/*     It is primarily a programming utility. Unless you are dealing */
/*     with a problem very similar to the one just described, you */
/*     probably shouldn't be using this routine. */

/*     The example below illustrates the intended use of this */
/*     routine. */

/* $ Examples */

/*     Consider the following programming situation. */

/*     Suppose that a routine is being written that will */
/*     access large amounts of data stored in the SPICE */
/*     kernel pool. Kernel pool access requires overhead that */
/*     may be prohibitive under some circumstances. Buffering */
/*     data locally and only fetching data from the kernel pool */
/*     when it has not been buffered locally, may substantially */
/*     improve the performance of the routine being written. */

/*     However, since FORTRAN does not allow dynamic memory allocation */
/*     the local data storage must be set at compile time. As */
/*     a result the local data buffer might become full during */
/*     an execution of your program. If data for an item needs */
/*     to be fetched from the kernel pool once the buffer has become */
/*     full, you must either repeatedly call the kernel pool to fetch */
/*     the new data or overwrite some of the data in your local buffer. */

/*     This routine helps with the decisions of which items to */
/*     overwrite. In addition it always moves the last requested */
/*     item to the head of the index used for searching the buffered */
/*     ID's. In this way if the same item is needed many times */
/*     in succession, there will be very little overhead associated */
/*     with finding the item. Thus the routine spends its time */
/*     in computing the desired quantities, not in looking up the */
/*     parameters needed for the computation. */

/*     Below is a fragment of code that illustrates how this routine */
/*     should be used. In the situation outlined above. We'll suppose */
/*     that we are fetching MDLSIZ double precision numbers from the */
/*     kernel pool that are associated with the item */

/*         'BODYid_MAGMODEL' */

/*     And that we are computing something with this model data. */


/*        INTEGER               MDLSIZ */
/*        PARAMETER           ( MDLSIZ = xxxxxx ) */

/*        We'll create room to buffer this data for 8 bodies. */


/*        INTEGER               PSIZE */
/*        PARAMETER           ( PSIZE = 8 ) */


/*        The ID's we shall be using are 1-dimensional. They are body */
/*        ID's for planets or and their satellites. */

/*        INTEGER               IDSZ */
/*        PARAMETER           ( IDSZ = 1 ) */

/*        INTEGER               AT */
/*        INTEGER               DIM */
/*        INTEGER               LIST   (   IDSZ,  PSIZE        ) */
/*        INTEGER               POOL   (      2,  LBPOOL:PSIZE ) */

/*        DOUBLE PRECISION      MAGMDL ( MDLSIZ,  PSIZE        ) */
/*        DOUBLE PRECISION      MODEL  ( MDLSIZ                ) */

/*        LOGICAL               FIRST */
/*        LOGICAL               PRESNT */

/*        SAVE */

/*        DATA                  FIRST / .TRUE. / */


/*        The block below handles initializing the linked list pool. */

/*        IF ( FIRST ) THEN */

/*           FIRST = .FALSE. */

/*           CALL LNKINI ( PSIZE, POOL ) */

/*        END IF */

/*        See if the data associated with ID has already been */
/*        buffered. */

/*        CALL LOCATI ( ID, IDSZ, LIST, POOL, AT, PRESNT ) */

/*        IF ( .NOT. PRESNT ) THEN */

/*           The data has not yet been buffered, look it up. Normally */
/*           you might want to check to see if the data exists and */
/*           handle things appropriately if it doesn't but this is just */
/*           to give you the idea... */

/*           CALL BODVCD ( ID, 'MAGMODEL', 3, DIM, MAGMDL ( 1, AT ) ) */

/*        END IF */

/*        Put the model data into the array MODEL for ease of */
/*        reading the rest of the code. */

/*        CALL MOVED ( MAGMDL(1,AT), MDLSIZ, MODEL ) */


/*        Now do whatever processing is needed .... */

/*     There are a few things to note about the code fragment above. */
/*     First the handling of the buffering of data was very easy. */
/*     Second, if this routine is called again using the same ID, */
/*     the buffer will already contain the needed model. Moreover */
/*     the routine LOCATI will return very quickly because the */
/*     ID will already be at the head of the list indexed by POOL. */

/*     You can also easily add an entry point to this routine that */
/*     will force it to look up data from the kernel pool on the */
/*     next call. All that needs to be done is re-initialize the */
/*     linked list pool. */

/*        ENTRY DOLOOK */

/*        CALL LNKINI ( PSIZE, POOL ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.2, 26-OCT-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. Updated */
/*        PRESNT short description in $Brief_I/O section. */

/* -    SPICELIB Version 1.0.1, 24-OCT-2005 (NJB) */

/*        Header update: changed reference to BODVAR to reference */
/*        to BODVCD. */

/* -    SPICELIB Version 1.0.0, 09-APR-1997 (WLT) */

/* -& */
/* $ Index_Entries */

/*     Locate an item in a linked list indexed list of items */
/*     Remove least recently used item buffering */

/* -& */

/*     Spicelib functions */


/*     Linked list parameters */


/*     Local Variables. */

    /* Parameter adjustments */
    list_dim1 = *idsz;
    list_offset = list_dim1 + 1;

    /* Function Body */
    chkin_("LOCATI", (ftnlen)6);

/*     We begin by looking through the list of items at our disposal. */
/*     One way or the other we will need the number of free nodes */
/*     in the linked list. */

    nfree = lnknfn_(pool);
    psize = lnksiz_(pool);
    if (nfree == psize) {

/*        There's nothing in the list so far. Allocate a */
/*        node and begin a list. */

	lnkan_(pool, at);
	i__1 = *idsz;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    list[i__ + *at * list_dim1 - list_offset] = id[i__ - 1];
	}
	*presnt = FALSE_;
	chkout_("LOCATI", (ftnlen)6);
	return 0;
    }
    if (*at <= 0 || *at > psize) {
	setmsg_("The input value for the head of the ID address linked list "
		"is out of bounds. It should be between 0 and #. The value su"
		"pplied was #.", (ftnlen)132);
	errint_("#", &psize, (ftnlen)1);
	errint_("#", at, (ftnlen)1);
	sigerr_("SPICE(ADDRESSOUTOFBOUNDS)", (ftnlen)25);
	chkout_("LOCATI", (ftnlen)6);
	return 0;
    }

/*     If we are still here then there is actually something in */
/*     the list.  We begin at start and traverse the list. */
/*     Since we are unlikely to ever have large ID's (their purpose */
/*     after all is to be a label for something more complex) we */
/*     will handle the cases where IDSZ is 1 or 2 as special */
/*     cases since the tests for equality are a lot easier. */

    same = FALSE_;
    head = *at;
    if (*idsz == 1) {
	same = id[0] == list[*at * list_dim1 + 1 - list_offset];
	more = *at > 0 && ! same;
	while(more) {
	    *at = pool[(*at << 1) + 10];
	    if (*at > 0) {
		same = id[0] == list[*at * list_dim1 + 1 - list_offset];
		more = ! same;
	    } else {
		more = FALSE_;
	    }
	}
    } else if (*idsz == 2) {
	same = id[0] == list[*at * list_dim1 + 1 - list_offset] && id[1] == 
		list[*at * list_dim1 + 2 - list_offset];
	more = *at > 0 && ! same;
	while(more) {
	    *at = pool[(*at << 1) + 10];
	    if (*at > 0) {
		if (id[0] == list[*at * list_dim1 + 1 - list_offset]) {
		    same = id[1] == list[*at * list_dim1 + 2 - list_offset];
		    more = ! same;
		}
	    } else {
		more = FALSE_;
	    }
	}
    } else {
	i__ = 1;
	same = TRUE_;
	while(same && i__ < *idsz) {
	    same = same && id[i__ - 1] == list[i__ + *at * list_dim1 - 
		    list_offset];
	    ++i__;
	}
	more = *at > 0 && ! same;
	while(more) {
	    *at = pool[(*at << 1) + 10];
	    if (*at > 0) {
		i__ = 1;
		same = TRUE_;
		while(same && i__ < *idsz) {
		    same = same && id[i__ - 1] == list[i__ + *at * list_dim1 
			    - list_offset];
		    ++i__;
		}
		more = ! same;
	    } else {
		more = FALSE_;
	    }
	}
    }

/*     The hunting is over either we found it or we need to */
/*     allocate space to put this ID into storage. */

    if (same) {
	*presnt = TRUE_;
	last = pool[(*at << 1) + 11];

/*        If AT is not already at the head of the list, we */
/*        move this node to the front of the list. */

	if (last > 0) {
	    lnkxsl_(at, at, pool);
	    lnkilb_(at, &head, pool);
	}
	chkout_("LOCATI", (ftnlen)6);
	return 0;
    }

/*     If we got to this point, we traversed the entire linked */
/*     list and did not find a matching ID.  AT is negative */
/*     and -AT points to the head of the list. */

    *presnt = FALSE_;

/*     We'll put it in the list. First see if there are any free nodes. */

    if (nfree > 0) {

/*        Allocate a free node and put our ID at the NEW address. */

	lnkan_(pool, &new__);
	i__1 = *idsz;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    list[i__ + new__ * list_dim1 - list_offset] = id[i__ - 1];
	}

/*        Put the new node at the head of the linked list. */

	lnkilb_(&new__, &head, pool);
	*at = new__;
    } else {

/*        The last item in the list is pointed to as being the */
/*        previous item to the head of the list. But we have to */
/*        change the sign to get a legitimate address.  Overwrite */
/*        the ID information in this last slot of the list. */

	last = -pool[(head << 1) + 11];
	i__1 = *idsz;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    list[i__ + last * list_dim1 - list_offset] = id[i__ - 1];
	}

/*        Extract the last item as a sublist and insert it before */
/*        the current head of the list. */

	lnkxsl_(&last, &last, pool);
	lnkilb_(&last, &head, pool);
	*at = last;
    }
    chkout_("LOCATI", (ftnlen)6);
    return 0;
} /* locati_ */

