/* dassdr.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__4 = 4;
static integer c__3 = 3;
static integer c__1 = 1;
static integer c__256 = 256;
static integer c__0 = 0;

/* $Procedure DASSDR ( DAS, segregate data records ) */
/* Subroutine */ int dassdr_(integer *handle)
{
    /* Initialized data */

    static integer next[3] = { 2,3,1 };
    static integer prev[3] = { 3,1,2 };

    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer base;
    char crec[1024];
    doublereal drec[128];
    integer free, irec[256], lrec, dest;
    logical more;
    integer unit, type__, i__, j, n;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer ncomc;
    extern /* Subroutine */ int maxai_(integer *, integer *, integer *, 
	    integer *);
    char savec[1024];
    doublereal saved[128];
    integer recno, savei[256];
    extern integer sumai_(integer *, integer *);
    integer ncomr, total, lword, count[4], ltype, start;
    extern logical failed_(void);
    extern /* Subroutine */ int dasadi_(integer *, integer *, integer *), 
	    cleari_(integer *, integer *);
    integer drbase;
    extern /* Subroutine */ int dasioc_(char *, integer *, integer *, char *, 
	    ftnlen, ftnlen), dasiod_(char *, integer *, integer *, doublereal 
	    *, ftnlen), dasllc_(integer *), dasrdi_(integer *, integer *, 
	    integer *, integer *), dashfs_(integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, integer *),
	     dasudi_(integer *, integer *, integer *, integer *);
    integer minadr, maxadr, scrhan, lastla[3];
    extern /* Subroutine */ int dassih_(integer *, char *, ftnlen), dashlu_(
	    integer *, integer *), daswbr_(integer *), dasrri_(integer *, 
	    integer *, integer *, integer *, integer *);
    integer offset;
    extern /* Subroutine */ int dasioi_(char *, integer *, integer *, integer 
	    *, ftnlen);
    integer lastrc[3];
    extern /* Subroutine */ int dasops_(integer *), dasufs_(integer *, 
	    integer *, integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *), chkout_(char *, ftnlen);
    integer lastwd[3], nresvc;
    extern logical return_(void);
    integer nresvr, savtyp, prvtyp, loc, pos;

/* $ Abstract */

/*     Segregate the data records in a DAS file into clusters, using */
/*     one cluster per data type present in the file. */

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

/*     DAS */

/* $ Keywords */

/*     DAS */
/*     FILES */
/*     ORDER */
/*     SORT */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   DAS file handle. */

/* $ Detailed_Input */

/*     HANDLE   is a file handle of a DAS file opened for writing. */

/* $ Detailed_Output */

/*     None. See $Particulars for a description of the effect of this */
/*     routine. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the input file handle is invalid, an error is signaled by a */
/*         routine in the call tree of this routine. */

/*     2)  If a Fortran read attempted by this routine fails, an error is */
/*         signaled by a routine in the call tree of this routine. The */
/*         state of the DAS file undergoing re-ordering will be */
/*         indeterminate. */

/*     3)  If a Fortran write attempted by this routine fails, an error */
/*         is signaled by a routine in the call tree of this routine. The */
/*         state of the DAS file undergoing re-ordering will be */
/*         indeterminate. */

/*     4)  If any other I/O error occurs during the re-arrangement of the */
/*         records in the indicated DAS file, the error is signaled by a */
/*         routine in the call tree of this routine. */

/* $ Files */

/*     See the description of the argument HANDLE in $Detailed_Input. */

/* $ Particulars */

/*     Normally, there should be no need for routines outside of */
/*     SPICELIB to call this routine. */

/*     The effect of this routine is to re-arrange the data records */
/*     in a DAS file so that the file contains a single cluster for */
/*     each data type present in the file: in the general case, there */
/*     will be a single cluster of each of the integer, double */
/*     precision, and character data types. */

/*     The relative order of data records of a given type is not */
/*     affected by this re-ordering. After the re-ordering, the DAS */
/*     file contains a single directory record that has one descriptor */
/*     for each cluster. After that point, the order in the file of the */
/*     sets of data records of the various data types will be: */

/*        +-------+ */
/*        |  CHAR | */
/*        +-------+ */
/*        |  DP   | */
/*        +-------+ */
/*        |  INT  | */
/*        +-------+ */

/*     Files that contain multiple directory records will have all but */
/*     the first directory record moved to the end of the file when the */
/*     re-ordering is complete. These records are not visible to the */
/*     DAS system and will be overwritten if data is subsequently added */
/*     to the DAS file. */

/*     The purpose of segregating a DAS file's data records into three */
/*     clusters is to make read access more efficient: when a DAS file */
/*     contains a single directory with at most three cluster type */
/*     descriptors, mapping logical to physical addresses can be done */
/*     in constant time. */

/* $ Examples */

/*     1)  Segregate data records in a DAS file designated by */
/*         HANDLE: */

/*            CALL DASSDR ( HANDLE ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     H.A. Neilan        (JPL) */
/*     M.J. Spencer       (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.1.0, 12-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 2.0.1, 19-DEC-1995 (NJB) */

/*        Corrected title of permuted index entry section. */

/* -    SPICELIB Version 2.0.0, 17-NOV-1993 (KRG) */

/*        Added test of FAILED after each DAS call, or sequence of calls, */
/*        which returns immediately if FAILED is .TRUE. This fixes a bug */
/*        where DASOPS signals an error and then DASSDR has a */
/*        segmentation fault. */

/*        Removed references to specific DAS file open routines in the */
/*        $Detailed_Input section of the header. This was done in order */
/*        to minimize documentation changes if the DAS open routines ever */
/*        change. */

/* -    SPICELIB Version 1.2.0, 07-OCT-1993 (NJB) (HAN) (MJS) */

/*        Bug fix: call to CLEARD replaced with call to */
/*        CLEARI. */

/* -    SPICELIB Version 1.1.0, 08-JUL-1993 (NJB) (MJS) */

/*        Bug fix: extraneous commas removed from argument lists */
/*        in calls to DASADI. */

/* -    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) */

/* -& */
/* $ Index_Entries */

/*     segregate the data records in a DAS file */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 2.0.0, 17-NOV-1993 (KRG) */

/*        Added test of failed after each DAS call, or sequence of calls, */
/*        which returns immediately if FAILED is .TRUE. This fixes a bug */
/*        where DASOPS signals an error and then DASSDR has a */
/*        segmentation fault. */

/*        Removed references to specific DAS file open routines in the */
/*        $Detailed_Input section of the header. This was done in order */
/*        to minimize documentation changes if the DAS open routines ever */
/*        change. */

/* -    SPICELIB Version 1.2.0, 07-OCT-1993 (NJB) (HAN) (MJS) */

/*        Bug fix: call to CLEARD replaced with call to */
/*        CLEARI. */

/* -    SPICELIB Version 1.1.0, 08-JUL-1993 (NJB) */

/*        Bug fix: extraneous commas removed from argument lists */
/*        in calls to DASADI. This bug had no visible effect on */
/*        VAX and Sun systems, but generated a compile error under */
/*        Lahey Fortran. */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Data type parameters */


/*     Directory pointer locations (backward and forward): */


/*     Directory address range location base */


/*     Location of first type descriptor */


/*     Local variables */


/*     Saved variables */


/*     NEXT and PREV map the DAS data type codes to their */
/*     successors and predecessors, respectively. */


/*     Initial values */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("DASSDR", (ftnlen)6);
    }

/*     Before starting, make sure that this DAS file is open for */
/*     writing. */

    dassih_(handle, "WRITE", (ftnlen)5);

/*     Get the logical unit for this file. */

    dashlu_(handle, &unit);
    if (failed_()) {
	chkout_("DASSDR", (ftnlen)6);
	return 0;
    }

/*     Write out any buffered records that belong to the file. */

    daswbr_(handle);
    if (failed_()) {
	chkout_("DASSDR", (ftnlen)6);
	return 0;
    }

/*     We're going to re-order the physical records in the DAS file, */
/*     starting with the first record after the first directory. */
/*     The other directory records are moved to the end of the file */
/*     as a result of the re-ordering. */

/*     The re-ordering algorithm is based on that used in the REORDx */
/*     routines.  To use this algorithm, we'll build an order vector */
/*     for the records to be ordered; we'll construct this order vector */
/*     in a scratch DAS file.  First, we'll traverse the directories */
/*     to build up a sort of inverse order vector that tells us the */
/*     final destination and data type of each data record;  from this */
/*     inverse vector we can easily build a true order vector.  The */
/*     cycles of the true order vector can be traversed without */
/*     repetitive searching, and with a minimum of assignment of the */
/*     contents of data records to temporary variables. */


/*     Allocate a scratch DAS file to keep our vectors in. */

    dasops_(&scrhan);
    if (failed_()) {
	chkout_("DASSDR", (ftnlen)6);
	return 0;
    }

/*     Now build up our `inverse order vector'.   This array is an */
/*     inverse order vector only in loose sense:  it actually consists */
/*     of an integer array that contains a sequence of pairs of integers, */
/*     the first of which indicates a data type, and the second of which */
/*     is an ordinal number.  There is one pair for each data record in */
/*     the file.  The ordinal number gives the ordinal position of the */
/*     record described by the number pair, relative to the other records */
/*     of the same type.  Directory records are considered to have type */
/*     `directory', which is represented by the code DIR. */

/*     We also must maintain a count of records of each type. */

    cleari_(&c__4, count);

/*     Get the file summary for the DAS file to be segregated. */

    dashfs_(handle, &nresvr, &nresvc, &ncomr, &ncomc, &free, lastla, lastrc, 
	    lastwd);
    if (failed_()) {
	chkout_("DASSDR", (ftnlen)6);
	return 0;
    }

/*     Find the record and word positions LREC and LWORD of the last */
/*     descriptor in the file, and also find the type of the descriptor */
/*     LTYPE. */

    maxai_(lastrc, &c__3, &lrec, &loc);
    lword = 0;
    for (i__ = 1; i__ <= 3; ++i__) {
	if (lastrc[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("lastrc",
		 i__1, "dassdr_", (ftnlen)462)] == lrec && lastwd[(i__2 = i__ 
		- 1) < 3 && 0 <= i__2 ? i__2 : s_rnge("lastwd", i__2, "dassd"
		"r_", (ftnlen)462)] > lword) {
	    lword = lastwd[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge(
		    "lastwd", i__1, "dassdr_", (ftnlen)465)];
	    ltype = i__;
	}
    }

/*     The first directory starts after the last comment record. */

    recno = nresvr + ncomr + 2;
    while(recno <= lrec && recno > 0) {

/*        Read the directory record. */

	dasrri_(handle, &recno, &c__1, &c__256, irec);
	if (failed_()) {
	    chkout_("DASSDR", (ftnlen)6);
	    return 0;
	}

/*        Increment the directory count. */

	++count[3];

/*        Add the data type (`directory') and count (1) of the current */
/*        record to the inverse order vector. */

	dasadi_(&scrhan, &c__1, &c__4);
	dasadi_(&scrhan, &c__1, &count[3]);
	if (failed_()) {
	    chkout_("DASSDR", (ftnlen)6);
	    return 0;
	}

/*        Set up our `finite state machine' that tells us the data */
/*        types of the records described by the last read directory. */

	type__ = irec[8];
	prvtyp = prev[(i__1 = type__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge(
		"prev", i__1, "dassdr_", (ftnlen)509)];

/*        Now traverse the directory and update the inverse order */
/*        vector based on the descriptors we find. */

	more = TRUE_;
	i__ = 10;
	while(more) {

/*           Obtain the count for the current descriptor. */

	    n = (i__2 = irec[(i__1 = i__ - 1) < 256 && 0 <= i__1 ? i__1 : 
		    s_rnge("irec", i__1, "dassdr_", (ftnlen)523)], abs(i__2));

/*           Update our inverse order vector to describe the positions */
/*           of the N records described by the current descriptor. */

	    i__1 = n;
	    for (j = 1; j <= i__1; ++j) {
		dasadi_(&scrhan, &c__1, &type__);
		i__3 = count[(i__2 = type__ - 1) < 4 && 0 <= i__2 ? i__2 : 
			s_rnge("count", i__2, "dassdr_", (ftnlen)532)] + j;
		dasadi_(&scrhan, &c__1, &i__3);
		if (failed_()) {
		    chkout_("DASSDR", (ftnlen)6);
		    return 0;
		}
	    }

/*           Adjust the count of records of data type TYPE. */

	    count[(i__1 = type__ - 1) < 4 && 0 <= i__1 ? i__1 : s_rnge("count"
		    , i__1, "dassdr_", (ftnlen)544)] = count[(i__2 = type__ - 
		    1) < 4 && 0 <= i__2 ? i__2 : s_rnge("count", i__2, "dass"
		    "dr_", (ftnlen)544)] + n;

/*           Find the next type. */

	    ++i__;
	    if (i__ > 256 || recno == lrec && i__ > lword) {
		more = FALSE_;
	    } else {
		if (irec[(i__1 = i__ - 1) < 256 && 0 <= i__1 ? i__1 : s_rnge(
			"irec", i__1, "dassdr_", (ftnlen)558)] > 0) {
		    type__ = next[(i__1 = type__ - 1) < 3 && 0 <= i__1 ? i__1 
			    : s_rnge("next", i__1, "dassdr_", (ftnlen)559)];
		} else if (irec[(i__1 = i__ - 1) < 256 && 0 <= i__1 ? i__1 : 
			s_rnge("irec", i__1, "dassdr_", (ftnlen)561)] < 0) {
		    type__ = prev[(i__1 = type__ - 1) < 3 && 0 <= i__1 ? i__1 
			    : s_rnge("prev", i__1, "dassdr_", (ftnlen)562)];
		} else {
		    more = FALSE_;
		}
	    }
	}

/*        The forward pointer in this directory tells us where the */
/*        next directory record is.  When there are no more directory */
/*        records, this pointer will be zero. */

	recno = irec[1];
    }

/*     At this point, the inverse order vector is set up.  The array */
/*     COUNT contains counts of the number of records of each type we've */
/*     seen.  Set TOTAL to the total number of records that we've going */
/*     to permute. */

    total = sumai_(count, &c__4);

/*     The next step is to build a true order vector.  Let BASE be */
/*     the base address for the order vector; this address is the */
/*     last logical address of the inverse order vector. */

    base = total << 1;

/*     We'll store the actual order vector in locations BASE + 1 */
/*     through BASE + TOTAL.  In addition, we'll build a parallel array */
/*     that contains, for each element of the order vector, the type of */
/*     data corresponding to that element.  This type vector will */
/*     reside in locations BASE + TOTAL + 1 through BASE + 2*TOTAL. */

/*     Before setting the values of the order vector and its parallel */
/*     type vector, we'll allocate space in the scratch DAS file by */
/*     zeroing out the locations we plan to use.  After this, locations */
/*     BASE+1 through BASE + 2*TOTAL can be written to in random access */
/*     fashion using DASUDI. */


    i__1 = total << 1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	dasadi_(&scrhan, &c__1, &c__0);
    }
    if (failed_()) {
	chkout_("DASSDR", (ftnlen)6);
	return 0;
    }

/*     We note that the way to construct the inverse of a permutation */
/*     SIGMA in a single loop is suggested by the relation */

/*             -1 */
/*        SIGMA   (  SIGMA(I)  )   =   I */

/*     We'll use this method.  In our case, our order vector plays */
/*     the role of */

/*             -1 */
/*        SIGMA */

/*     and the `inverse order vector' plays the role of SIGMA.  We'll */
/*     exclude the first directory from the order vector, since it's */
/*     an exception:  we wish to reserve this record.  Since the first */
/*     element of the order vector (logically) contains the index 1, we */
/*     can ignore it. */


    i__1 = total;
    for (i__ = 2; i__ <= i__1; ++i__) {
	i__2 = (i__ << 1) - 1;
	i__3 = (i__ << 1) - 1;
	dasrdi_(&scrhan, &i__2, &i__3, &type__);
	i__2 = i__ << 1;
	i__3 = i__ << 1;
	dasrdi_(&scrhan, &i__2, &i__3, &dest);
	if (failed_()) {
	    chkout_("DASSDR", (ftnlen)6);
	    return 0;
	}

/*        Set DEST to the destination location, measured as an offset */
/*        from the last comment record, of the Ith record by adding */
/*        on the count of the predecessors of the block of records of */
/*        TYPE. */

	for (j = 1; j <= 3; ++j) {
	    if (type__ > j) {
		dest += count[(i__2 = j - 1) < 4 && 0 <= i__2 ? i__2 : s_rnge(
			"count", i__2, "dassdr_", (ftnlen)659)];
	    }
	}

/*        The destination offset of each record should be incremented to */
/*        allow room for the first directory record.  However, we don't */
/*        need to do this for directory records; they'll already have */
/*        this offset accounted for. */

	if (type__ != 4) {
	    ++dest;
	}

/*        The value of element DEST of the order vector is I. */
/*        Write this value to location BASE + DEST. */

	i__2 = base + dest;
	i__3 = base + dest;
	dasudi_(&scrhan, &i__2, &i__3, &i__);

/*        We want the ith element of the order vector to give us the */
/*        number of the record to move to position i (offset from the */
/*        last comment record),  but we want the corresponding element */
/*        of the type array to give us the type of the record currently */
/*        occupying position i. */

	i__2 = base + i__ + total;
	i__3 = base + i__ + total;
	dasudi_(&scrhan, &i__2, &i__3, &type__);
	if (failed_()) {
	    chkout_("DASSDR", (ftnlen)6);
	    return 0;
	}
    }

/*     Ok, here's what we've got in the scratch file that's still of */
/*     interest: */

/*        -- In integer logical addresses BASE + 1 : BASE + TOTAL, */
/*           we have an order vector.  The Ith element of this */
/*           vector indicates the record that should be moved to */
/*           location DRBASE + I in the DAS file we're re-ordering, */
/*           where DRBASE is the base address of the data records */
/*           (the first directory record follows the record having this */
/*           index). */


/*        -- In integer logical addresses BASE + TOTAL + 1  :  BASE + */
/*           2*TOTAL, we have data type indicators for the records to */
/*           be re-ordered.  The type for the Ith record in the file, */
/*           counted from the last comment record, is located in logical */
/*           address BASE + TOTAL + I. */


    drbase = nresvr + ncomr + 1;

/*     As we traverse the order vector, we flip the sign of elements */
/*     we've accessed, so that we can tell when we encounter an element */
/*     of a cycle that we've already traversed. */

/*     Traverse the order vector.  The variable START indicates the */
/*     first element to look at.  Ignore the first element; it's a */
/*     singleton cycle. */


    start = 2;
    while(start < total) {

/*        Traverse the current cycle of the order vector. */

/*        We `make a hole' in the file by saving the record in position */
/*        START, then we traverse the cycle in reverse order, filling in */
/*        the hole at the ith position with the record whose number is */
/*        the ith element of the order vector.  At the end, we deposit */
/*        the saved record into the `hole' left behind by the last */
/*        record we moved. */

/*        We're going to read and write records to and from the DAS file */
/*        directly, rather than going through the buffering system. */
/*        This will allow us to avoid any untoward interactions between */
/*        the buffers for different data types. */

	i__1 = base + total + start;
	i__2 = base + total + start;
	dasrdi_(&scrhan, &i__1, &i__2, &savtyp);
	i__1 = base + start;
	i__2 = base + start;
	dasrdi_(&scrhan, &i__1, &i__2, &offset);

/*        Save the record at the location DRBASE + START. */

	if (savtyp == 1) {
	    i__1 = drbase + start;
	    dasioc_("READ", &unit, &i__1, savec, (ftnlen)4, (ftnlen)1024);
	} else if (savtyp == 2) {
	    i__1 = drbase + start;
	    dasiod_("READ", &unit, &i__1, saved, (ftnlen)4);
	} else {
	    i__1 = drbase + start;
	    dasioi_("READ", &unit, &i__1, savei, (ftnlen)4);
	}
	if (failed_()) {
	    chkout_("DASSDR", (ftnlen)6);
	    return 0;
	}

/*        Let I be the index of the record that we are going to move */
/*        data into next.  I is an offset from the last comment record. */

	i__ = start;
	while(offset != start) {

/*           Mark the order vector element by writing its negative */
/*           back to the location it came from. */

	    i__1 = base + i__;
	    i__2 = base + i__;
	    i__3 = -offset;
	    dasudi_(&scrhan, &i__1, &i__2, &i__3);

/*           Move the record at location */

/*              DRBASE + OFFSET */

/*           to location */

/*              DRBASE + I */

/*           There is no need to do anything about the corresponding */
/*           elements of the type vector; we won't need them again. */

/*           The read and write operations, as well as the temporary */
/*           record required to perform the move, are dependent on the */
/*           data type of the record to be moved. */

	    i__1 = base + total + offset;
	    i__2 = base + total + offset;
	    dasrdi_(&scrhan, &i__1, &i__2, &type__);
	    if (failed_()) {
		chkout_("DASSDR", (ftnlen)6);
		return 0;
	    }

/*           Only pick records up if we're going to put them down in */
/*           a location other than their original one. */

	    if (i__ != offset) {
		if (type__ == 1) {
		    i__1 = drbase + offset;
		    dasioc_("READ", &unit, &i__1, crec, (ftnlen)4, (ftnlen)
			    1024);
		    i__1 = drbase + i__;
		    dasioc_("WRITE", &unit, &i__1, crec, (ftnlen)5, (ftnlen)
			    1024);
		} else if (type__ == 2) {
		    i__1 = drbase + offset;
		    dasiod_("READ", &unit, &i__1, drec, (ftnlen)4);
		    i__1 = drbase + i__;
		    dasiod_("WRITE", &unit, &i__1, drec, (ftnlen)5);
		} else {
		    i__1 = drbase + offset;
		    dasioi_("READ", &unit, &i__1, irec, (ftnlen)4);
		    i__1 = drbase + i__;
		    dasioi_("WRITE", &unit, &i__1, irec, (ftnlen)5);
		}
		if (failed_()) {
		    chkout_("DASSDR", (ftnlen)6);
		    return 0;
		}
	    }

/*           OFFSET is the index of the next order vector element to */
/*           look at. */

	    i__ = offset;
	    i__1 = base + i__;
	    i__2 = base + i__;
	    dasrdi_(&scrhan, &i__1, &i__2, &offset);
	    i__1 = base + i__ + total;
	    i__2 = base + i__ + total;
	    dasrdi_(&scrhan, &i__1, &i__2, &type__);
	    if (failed_()) {
		chkout_("DASSDR", (ftnlen)6);
		return 0;
	    }
	}

/*        The last value of I is the location in the cycle that element */
/*        START followed.  Therefore, the saved record corresponding */
/*        to index START should be written to this location. */

	if (savtyp == 1) {
	    i__1 = drbase + i__;
	    dasioc_("WRITE", &unit, &i__1, savec, (ftnlen)5, (ftnlen)1024);
	} else if (savtyp == 2) {
	    i__1 = drbase + i__;
	    dasiod_("WRITE", &unit, &i__1, saved, (ftnlen)5);
	} else {
	    i__1 = drbase + i__;
	    dasioi_("WRITE", &unit, &i__1, savei, (ftnlen)5);
	}

/*        Mark the order vector element by writing its negative */
/*        back to the location it came from. */

	i__1 = base + i__;
	i__2 = base + i__;
	i__3 = -start;
	dasudi_(&scrhan, &i__1, &i__2, &i__3);
	if (failed_()) {
	    chkout_("DASSDR", (ftnlen)6);
	    return 0;
	}

/*        Update START so that it points to the first element of a cycle */
/*        of the order vector that has not yet been traversed.  This will */
/*        be the first positive element of the order vector in a location */
/*        indexed higher than the current value of START.  Note that */
/*        this way of updating START guarantees that we don't have to */
/*        backtrack to find an element in the next cycle. */

	offset = -1;
	while(offset < 0 && start < total) {
	    ++start;
	    i__1 = base + start;
	    i__2 = base + start;
	    dasrdi_(&scrhan, &i__1, &i__2, &offset);
	    if (failed_()) {
		chkout_("DASSDR", (ftnlen)6);
		return 0;
	    }
	}

/*        At this point, START is the index of an element in the order */
/*        vector that belongs to a cycle where no routine has gone */
/*        before, or else START is the last index in the order vector, */
/*        in which case we're done. */

    }

/*     At this point, the records in the DAS are organized as follows: */

/*        +----------------------------------+ */
/*        |           File record            |  ( 1 ) */
/*        +----------------------------------+ */
/*        |         Reserved records         |  ( 0 or more ) */
/*        |                                  | */
/*        +----------------------------------+ */
/*        |          Comment records         |  ( 0 or more ) */
/*        |                                  | */
/*        |                                  | */
/*        +----------------------------------+ */
/*        |      First directory  record     |  ( 1 ) */
/*        +----------------------------------+ */
/*        |      Character data records      |  ( 0 or more ) */
/*        |                                  | */
/*        +----------------------------------+ */
/*        |   Double precision data records  |  ( 0 or more ) */
/*        |                                  | */
/*        +----------------------------------+ */
/*        |       Integer data records       |  ( 0 or more ) */
/*        |                                  | */
/*        +----------------------------------+ */
/*        |   Additional directory records   |  ( 0 or more ) */
/*        |                                  | */
/*        +----------------------------------+ */


/*     Not all of the indicated components must be present; only the */
/*     file record and first directory record will exist in all cases. */
/*     The `additional directory records' at the end of the file serve */
/*     no purpose; if more data is appended to the file, they will be */
/*     overwritten. */

/*     The last step in preparing the file is to fill in the first */
/*     directory record with the correct information, and to update */
/*     the file summary. */


    recno = drbase + 1;
    cleari_(&c__256, irec);

/*     Set the logical address ranges in the directory record, for each */
/*     data type. */

    for (type__ = 1; type__ <= 3; ++type__) {
	maxadr = lastla[(i__1 = type__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge(
		"lastla", i__1, "dassdr_", (ftnlen)968)];
	if (maxadr > 0) {
	    minadr = 1;
	} else {
	    minadr = 0;
	}
	irec[(i__1 = type__ << 1) < 256 && 0 <= i__1 ? i__1 : s_rnge("irec", 
		i__1, "dassdr_", (ftnlen)976)] = minadr;
	irec[(i__1 = (type__ << 1) + 1) < 256 && 0 <= i__1 ? i__1 : s_rnge(
		"irec", i__1, "dassdr_", (ftnlen)977)] = maxadr;
    }

/*     Set the descriptors in the directory.  Determine which type */
/*     comes first:  the order of priority is character, double */
/*     precision, integer. */

    pos = 9;
    for (type__ = 1; type__ <= 3; ++type__) {
	if (lastla[(i__1 = type__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("las"
		"tla", i__1, "dassdr_", (ftnlen)990)] > 0) {
	    if (pos == 9) {

/*              This is the first type for which any data is present. */
/*              We must enter a type code at position BEGDSC in the */
/*              directory, and we must enter a count at position */
/*              BEGDSC+1. */

		irec[8] = type__;
		irec[9] = count[(i__1 = type__ - 1) < 4 && 0 <= i__1 ? i__1 : 
			s_rnge("count", i__1, "dassdr_", (ftnlen)1000)];
		lastrc[(i__1 = type__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge(
			"lastrc", i__1, "dassdr_", (ftnlen)1001)] = recno;
		lastwd[(i__1 = type__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge(
			"lastwd", i__1, "dassdr_", (ftnlen)1002)] = 10;
		pos += 2;
		prvtyp = type__;
	    } else {

/*              Place an appropriately signed count at location POS in */
/*              the directory. */

		if (type__ == next[(i__1 = prvtyp - 1) < 3 && 0 <= i__1 ? 
			i__1 : s_rnge("next", i__1, "dassdr_", (ftnlen)1011)])
			 {
		    irec[(i__1 = pos - 1) < 256 && 0 <= i__1 ? i__1 : s_rnge(
			    "irec", i__1, "dassdr_", (ftnlen)1012)] = count[(
			    i__2 = type__ - 1) < 4 && 0 <= i__2 ? i__2 : 
			    s_rnge("count", i__2, "dassdr_", (ftnlen)1012)];
		} else {
		    irec[(i__1 = pos - 1) < 256 && 0 <= i__1 ? i__1 : s_rnge(
			    "irec", i__1, "dassdr_", (ftnlen)1014)] = -count[(
			    i__2 = type__ - 1) < 4 && 0 <= i__2 ? i__2 : 
			    s_rnge("count", i__2, "dassdr_", (ftnlen)1014)];
		}
		lastrc[(i__1 = type__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge(
			"lastrc", i__1, "dassdr_", (ftnlen)1017)] = recno;
		lastwd[(i__1 = type__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge(
			"lastwd", i__1, "dassdr_", (ftnlen)1018)] = pos;
		++pos;
		prvtyp = type__;
	    }
	}
    }

/*     Since we've done away with all but the first directory, the first */
/*     free record is decremented by 1 less than the directory count. */

    free = free - count[3] + 1;

/*     Write out the new directory record.  Don't use the DAS buffered */
/*     write mechanism; this could trash the file by dumping buffered */
/*     records in the wrong places. */

    dasioi_("WRITE", &unit, &recno, irec, (ftnlen)5);

/*     Write out the updated file summary. */

    dasufs_(handle, &nresvr, &nresvc, &ncomr, &ncomc, &free, lastla, lastrc, 
	    lastwd);

/*     Clean up the DAS data buffers:  we don't want buffered scratch */
/*     file records hanging around there.  Then get rid of the scratch */
/*     file. */

    daswbr_(&scrhan);
    dasllc_(&scrhan);
    chkout_("DASSDR", (ftnlen)6);
    return 0;
} /* dassdr_ */

