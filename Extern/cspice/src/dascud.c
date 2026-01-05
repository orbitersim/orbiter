/* dascud.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__3 = 3;
static integer c__256 = 256;
static integer c__2 = 2;

/* $Procedure DASCUD ( DAS, create or update directories ) */
/* Subroutine */ int dascud_(integer *handle, integer *type__, integer *
	nwords)
{
    /* Initialized data */

    static integer next[3] = { 2,3,1 };

    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer free, lrec, last, room, i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer ncomc, descr;
    extern /* Subroutine */ int maxai_(integer *, integer *, integer *, 
	    integer *);
    integer recno, ncomr, lword, ltype, needed;
    extern /* Subroutine */ int cleari_(integer *, integer *);
    integer dscrec, nw, dirrec[256];
    extern /* Subroutine */ int dashfs_(integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, integer *);
    integer minadr, maxadr, lastla[3], rngloc;
    extern /* Subroutine */ int dasufs_(integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, integer *),
	     dasrri_(integer *, integer *, integer *, integer *, integer *), 
	    dasuri_(integer *, integer *, integer *, integer *, integer *);
    integer lastrc[3];
    extern /* Subroutine */ int daswri_(integer *, integer *, integer *), 
	    sigerr_(char *, ftnlen), chkout_(char *, ftnlen);
    integer lastwd[3], nresvc;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen);
    extern logical return_(void);
    integer nresvr, loc;

/* $ Abstract */

/*     Create or update directories in a DAS file to reflect addition */
/*     of a specified number of words of a specified data type. */

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
/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   DAS file handle. */
/*     TYPE       I   Data type specifier. */
/*     NWORDS     I   Number of words of data being added. */
/*     CHAR       P   Parameter indicating character data type. */
/*     DP         P   Parameter indicating double precision data type. */
/*     INT        P   Parameter indicating integer data type. */

/* $ Detailed_Input */

/*     HANDLE   is the file handle of a DAS file open for writing. */

/*     TYPE     is a data type specifier. TYPE may be any of */
/*              the parameters */

/*                 CHAR */
/*                 DP */
/*                 INT */

/*              which indicate `character', `double precision', */
/*              and `integer' respectively. */

/*     NWORDS   is the number of words of data of the data type */
/*              indicated by TYPE whose addition to the indicated */
/*              DAS file is to be accounted for. */

/* $ Detailed_Output */

/*     None. See $Particulars for a description of the action of this */
/*     routine. */

/* $ Parameters */

/*     CHAR, */
/*     DP, */
/*     INT      are data type specifiers which indicate */
/*              `character', `double precision', and `integer' */
/*              respectively. These parameters are used in */
/*              all DAS routines that require a data type */
/*              specifier as input. */

/* $ Exceptions */

/*     1)  If the input handle is invalid, an error is signaled by a */
/*         routine in the call tree of this routine. */

/*     2)  If TYPE is not recognized, the error SPICE(DASINVALIDTYPE) */
/*         is signaled. */

/*     3)  If NWORDS is negative, the error SPICE(VALUEOUTOFRANGE) is */
/*         signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine operates by side effects: the directories in the */
/*     indicated DAS file will be updated to reflect the addition of */
/*     the indicated number of words of the specified data type. */
/*     If necessary, a new directory record will be added to the file */
/*     to hold a new cluster descriptor. */

/*     In addition, the file summary for the indicated DAS file will be */
/*     updated with the new values of the descriptor location and last */
/*     logical address of the indicated type, as well as with the new */
/*     value of the free record pointer. */

/*     This routine is used by the DASADx routines: after each data */
/*     addition, they call this routine to update the directories of the */
/*     affected DAS file. */

/*     Normally, there will be no need for routines outside of SPICELIB */
/*     to call this routine directly. To add data to or update a DAS */
/*     file, the DASADx and DASUDx routines should be used; these */
/*     routines take care of directory creation and updates. */

/* $ Examples */

/*     1)  Update directories after writing N integer words to a */
/*         DAS file designated by HANDLE: */

/*             CALL DASCUD ( HANDLE, INT, N ) */

/* $ Restrictions */

/*     1)  This routine is intended for use by the SPICELIB DAS routines. */
/*         Non-SPICELIB software normally will not need to call this */
/*         routine. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.5.0, 13-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.4.0, 07-AUG-2006 (NJB) */

/*        Bug fix: added initialization of variable LTYPE to support */
/*                 operation under the Macintosh Intel Fortran */
/*                 compiler. Note that this bug did not affect */
/*                 operation of this routine on other platforms. */

/* -    SPICELIB Version 1.3.0, 16-JAN-2003 (NJB) */

/*        Bug fix: fixed previous bug fix. */

/* -    SPICELIB Version 1.2.0, 10-DEC-2002 (NJB) */

/*        Bug fix: now a new, empty directory record with valid */
/*        backward and forward pointers is written immediately */
/*        when it is created. */

/* -    SPICELIB Version 1.1.1, 19-DEC-1995 (NJB) */

/*        Corrected title of permuted index entry section. */

/* -    SPICELIB Version 1.0.1, 26-OCT-1993 (KRG) */

/*        Removed references to specific DAS file open routines in the */
/*        $Detailed_Input section of the header. This was done in order */
/*        to minimize documentation changes if the DAS open routines ever */
/*        change. */

/*        Removed an unused variable. */

/* -    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT) */

/* -& */
/* $ Index_Entries */

/*     update DAS cluster directories */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.4.0 07-AUG-2006 (NJB) */

/*        Bug fix: added initialization of variable LTYPE to support */
/*                 operation under the Macintosh Intel Fortran */
/*                 compiler. Note that this bug did not affect */
/*                 operation of this routine on other platforms. The */
/*                 statement referencing the uninitialized variable */
/*                 was: */

/*           ELSE IF (       ( TYPE   .EQ. LTYPE ) */
/*          .          .AND. ( DSCREC .GT. 0     ) */
/*          .          .AND. ( LWORD  .LT. NWI   )  ) THEN */


/*        In the previous version of the code, LTYPE is uninitialized */
/*        when the DAS file is empty, which implies DSCREC is 0. */
/*        Otherwise LTYPE is initialized. So the value of the logical */
/*        expression is not affected by the uninitialized value of */
/*        LTYPE. */

/*        However, the Intel Fortran compiler for the Mac flags a runtime */
/*        error when the above code is exercised. So LTYPE is now */
/*        initialized to an invalid value prior to execution of this */
/*        code. If the invalid value is ever used, a runtime error */
/*        should result. */


/* -    SPICELIB Version 1.3.0 16-JAN-2003 (NJB) */

/*        Bug fix: fixed previous bug fix. */


/*        The offending line (#778) in previous version) of code is: */

/*           CALL DASWRI ( HANDLE, RECNO, DIRREC ) */

/*        The correct line of code is: */

/*          CALL DASWRI ( HANDLE, FREE, DIRREC ) */


/* -    SPICELIB Version 1.2.0 10-DEC-2002 (NJB) */

/*        Bug fix: now a new, empty directory record with valid */
/*        backward and forward pointers is written immediately */
/*        when it is created. This prevents an unsegregated file */
/*        from being left with an invalid forward pointer. */

/* -    SPICELIB Version 1.0.1, 26-OCT-1993 (KRG) */

/*        Removed references to specific DAS file open routines in the */
/*        $Detailed_Input section of the header. This was done in order */
/*        to minimize documentation changes if the DAS open routines ever */
/*        change. */

/*        Removed an unused variable, PREV. */

/* -    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT) */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Words per data record, for each data type: */


/*     Directory pointer locations (backward and forward): */


/*     Directory address range locations */


/*     Location of first type descriptor */


/*     Local variables */


/*     Saved variables */



/*     NEXT maps the DAS data type codes to their successors. */


/*     Initial values */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("DASCUD", (ftnlen)6);
    }

/*     Here's a preview of coming attractions: */

/*        We're going to update the directories in the indicated */
/*        DAS file to reflect the addition of NWORDS new data words. */
/*        This data is supposed to have been added to the file BEFORE */
/*        this routine is called.  There are several possible states */
/*        the file can be in at the point this routine is called. */


/*           1)  There is already a descriptor of TYPE in the file, and */
/*               the addition of data does not require this descriptor */
/*               to be modified. */

/*               We can tell that we have this case when the file */
/*               summary indicates that, before the addition of data, */
/*               there was room for NWORDS of data in the last data */
/*               record in the file.  Since no new data records were */
/*               required to accommodate the new data, the descriptor */
/*               for TYPE does not have to be updated. */

/*               However, even though the descriptor need not be */
/*               modified, the address range for TYPE covered by the */
/*               directory record containing this last descriptor must be */
/*               updated, as must be the file summary. */


/*           2)  There is already a descriptor of TYPE in the file, and */
/*               in order to describe the new data added to the file, */
/*               it suffices to update this descriptor and the address */
/*               range in the directory containing it. */

/*               This happens when case (1) doesn't apply, and the */
/*               descriptor of TYPE is the last descriptor in the last */
/*               directory, and the descriptor is not in the last */
/*               position (index NWI) of the directory. */

/*               Note that we never update the last descriptor in a */
/*               directory record.  The reason for this is that after */
/*               this descriptor is written, we build a new directory */
/*               record.  All subsequent additions of data are made to */
/*               records that follow this new directory record; */
/*               otherwise, the new directory would get overwritten */
/*               with data. */


/*           3)  A new descriptor of TYPE is needed. */

/*               This can happen in several ways: */

/*               a)  There are no directories in the file yet, in which */
/*                   case space has been reserved for the first */
/*                   directory. */

/*                   This can happen only when the file had no data at */
/*                   all in it before the last addition of data. */

/*                   In this case, we must fill in the first descriptor */
/*                   and the address range for TYPE.  We must also update */
/*                   the file summary, because the descriptor location, */
/*                   last logical address of TYPE, and the free pointer */
/*                   have changed. */

/*               b)  The conditions for cases (1) and (2) are not */
/*                   satisfied, and the current last directory record */
/*                   has room for a new descriptor.  In this case, if */
/*                   the data addition filled in the last data record */
/*                   described by the current last descriptor of type, */
/*                   (which will usually be the case), we must update */
/*                   the appropriate address range in the directory */
/*                   record containing that descriptor.  We will then */
/*                   add a new descriptor to the last directory record */
/*                   and update the address range for TYPE in that */
/*                   record.  The file summary must be updated as well. */

/*                   If the new descriptor we've added went into the */
/*                   last slot in a directory record (index NWI), we */
/*                   also create a new, empty directory record and */
/*                   update the forward pointer of the current directory */
/*                   to point to it.  We also update the file summary */
/*                   so that the free pointer points to the record */
/*                   following the empty directory record. */


/*               c)  The conditions for cases (1) and (2) are not */
/*                   satisfied, and the current last directory record */
/*                   has no room for a new descriptor. */

/*                   In this case, if the data addition filled in the */
/*                   last data record described by the current last */
/*                   descriptor of TYPE, (which will usually be the */
/*                   case), we must update the appropriate address range */
/*                   in the directory record containing that descriptor. */
/*                   We will then add a new descriptor to the empty */
/*                   directory record and initialize the address range */
/*                   for TYPE in that record.  The file summary must be */
/*                   updated as well. */


/*     To start out, we'll need to find out how the file is currently */
/*     disposed.  We'll need the location of the last descriptor of */
/*     TYPE, the last logical address of TYPE, and the location of */
/*     the last descriptor of any type. */

/*     Get the file summary. */

    dashfs_(handle, &nresvr, &nresvc, &ncomr, &ncomc, &free, lastla, lastrc, 
	    lastwd);

/*     Now do all of the data-type-dependent work: */

/*        -- Set the last address of the indicated data type LAST. */

/*        -- Set the physical record of the last descriptor of TYPE. */

/*        -- Set the number of words of data of the specified type per */
/*           physical record NW. */

/*        -- Set the address range location used to pick address ranges */
/*           out of directory records. */


/*     Note that the address and descriptor location information from */
/*     the file summary is assumed NOT to take into account the latest */
/*     data addition. */


    last = lastla[(i__1 = *type__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("las"
	    "tla", i__1, "dascud_", (ftnlen)525)];
    dscrec = lastrc[(i__1 = *type__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge(
	    "lastrc", i__1, "dascud_", (ftnlen)526)];
    if (*type__ == 2) {
	nw = 128;
	rngloc = 5;
    } else if (*type__ == 3) {
	nw = 256;
	rngloc = 7;
    } else if (*type__ == 1) {
	nw = 1024;
	rngloc = 3;
    } else {
	setmsg_("Invalid data type: #. ", (ftnlen)22);
	errint_("#", type__, (ftnlen)1);
	sigerr_("SPICE(DASINVALIDTYPE)", (ftnlen)21);
	chkout_("DASCUD", (ftnlen)6);
	return 0;
    }

/*     Make sure that NWORDS is something sensible. */

    if (*nwords < 0) {
	setmsg_("NWORDS was #; should be non-negative.", (ftnlen)37);
	errint_("#", nwords, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("DASCUD", (ftnlen)6);
	return 0;
    }

/*     Find the record and word positions LREC and LWORD of the last */
/*     descriptor in the file, and also find the type of the descriptor */
/*     LTYPE. */

    maxai_(lastrc, &c__3, &lrec, &loc);
    lword = 0;
    ltype = 0;
    for (i__ = 1; i__ <= 3; ++i__) {
	if (lastrc[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("lastrc",
		 i__1, "dascud_", (ftnlen)577)] == lrec && lastwd[(i__2 = i__ 
		- 1) < 3 && 0 <= i__2 ? i__2 : s_rnge("lastwd", i__2, "dascu"
		"d_", (ftnlen)577)] > lword) {
	    lword = lastwd[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge(
		    "lastwd", i__1, "dascud_", (ftnlen)580)];
	    ltype = i__;
	}
    }

/*     LREC, LWORD, and LTYPE are now the record, word, and data type */
/*     of the last descriptor in the file.  If LREC is zero, there are */
/*     no directories in the file yet.  In this case, LWORD and */
/*     LTYPE are both zero. */


/*     Compute the number of words we have room for in the current */
/*     last data record of the indicated type. */

    if (last > 0) {
	room = nw - (last - (last - 1) / nw * nw);
    } else {
	room = 0;
    }

/*     Compute the number of additional data records needed to */
/*     accommodate (NWORDS - ROOM) additional words of data of type */
/*     TYPE. */

    needed = (*nwords - room + nw - 1) / nw;

/*     Now, update the descriptor directories. */

    if (room >= *nwords && dscrec > 0) {

/*        This is case (1). */

/*        There is already a descriptor of TYPE in the file.  The data */
/*        fits in the current record, so no descriptors have to change. */

/*        Update the address range in the directory record containing */
/*        the last descriptor of TYPE. */

	maxadr = last + *nwords;
	i__1 = rngloc + 1;
	i__2 = rngloc + 1;
	dasuri_(handle, &dscrec, &i__1, &i__2, &maxadr);

/*        The last logical address of TYPE is now MAXADR. */

	lastla[(i__1 = *type__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("lastla",
		 i__1, "dascud_", (ftnlen)633)] = maxadr;

/*        Write out the updated file summary. */

	dasufs_(handle, &nresvr, &nresvc, &ncomr, &ncomc, &free, lastla, 
		lastrc, lastwd);
    } else if (*type__ == ltype && dscrec > 0 && lword < 256) {


/*        This is case (2). */

/*        The descriptor of TYPE is the last descriptor in the */
/*        file but is not in the last location (index NWI) of a */
/*        directory record.  All we have to do is update this last */
/*        descriptor to reflect the addition of the number of needed */
/*        data records. */

/*        Get the old descriptor, since we're going to update it. */


	dasrri_(handle, &dscrec, &lword, &lword, &descr);

/*        Update the descriptor and write it back into the file. */

	if (descr < 0) {
	    descr -= needed;
	} else {
	    descr += needed;
	}
	dasuri_(handle, &dscrec, &lword, &lword, &descr);

/*        Update the address range for this type. */

	maxadr = last + *nwords;
	i__1 = rngloc + 1;
	i__2 = rngloc + 1;
	dasuri_(handle, &dscrec, &i__1, &i__2, &maxadr);

/*        The last logical address of TYPE is now MAXADR.  The first */
/*        free record follows the last data record in use. */

	lastla[(i__1 = *type__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("lastla",
		 i__1, "dascud_", (ftnlen)690)] = maxadr;
	free += needed;

/*        Write out the updated file summary. */

	dasufs_(handle, &nresvr, &nresvc, &ncomr, &ncomc, &free, lastla, 
		lastrc, lastwd);
    } else {

/*        This is case (3).  We need a new descriptor. */

	if (lrec == 0) {

/*           This is case (3a).  We have a virgin directory record. */
/*           Set the number of this record. */

	    recno = nresvr + ncomr + 2;

/*           Start with an empty directory record. */

	    cleari_(&c__256, dirrec);

/*           Add a new descriptor to the directory.  The record */
/*           count is the number of new records required:  NEEDED. */

	    dirrec[8] = *type__;
	    dirrec[9] = needed;

/*           Fill in the address range for TYPE covered by this */
/*           directory. */

	    dirrec[(i__1 = rngloc - 1) < 256 && 0 <= i__1 ? i__1 : s_rnge(
		    "dirrec", i__1, "dascud_", (ftnlen)735)] = 1;
	    dirrec[(i__1 = rngloc) < 256 && 0 <= i__1 ? i__1 : s_rnge("dirrec"
		    , i__1, "dascud_", (ftnlen)736)] = *nwords;

/*           Write out this directory. */

	    daswri_(handle, &recno, dirrec);

/*           Update the file summary:  the location of the descriptor */
/*           and the last logical address for this type must be set. */
/*           The count portion of the descriptor goes after the initial */
/*           data type indicator; this data type indicator is not */
/*           considered to be part of the descriptor. */

/*           The first free record follows the last data record in use. */

	    free = recno + needed + 1;
	    lastla[(i__1 = *type__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge(
		    "lastla", i__1, "dascud_", (ftnlen)753)] = *nwords;
	    lastrc[(i__1 = *type__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge(
		    "lastrc", i__1, "dascud_", (ftnlen)754)] = recno;
	    lastwd[(i__1 = *type__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge(
		    "lastwd", i__1, "dascud_", (ftnlen)755)] = 10;
	    dasufs_(handle, &nresvr, &nresvc, &ncomr, &ncomc, &free, lastla, 
		    lastrc, lastwd);
	} else if (lword < 256) {

/*           This is case (3b).  We have room for another descriptor */
/*           in the current directory record. */

/*           Before adding the new descriptor, we must update the */
/*           directory containing the current last descriptor of TYPE, */
/*           if the range of addresses covered by the cluster it */
/*           describes was increased by the last data addition.  Of */
/*           course, this update is required only if there IS such a */
/*           descriptor, and if it is in a record that precedes LREC. */

	    if (dscrec > 0 && dscrec < lrec && room > 0) {

/*              Update the address range for TYPE in record DSCREC. */
/*              The upper bound is increased by ROOM, since that many */
/*              words of TYPE were added to the last record in the */
/*              last cluster of TYPE described by that directory. */

		maxadr = last + room;
		i__1 = rngloc + 1;
		i__2 = rngloc + 1;
		dasuri_(handle, &dscrec, &i__1, &i__2, &maxadr);
	    }

/*           Make up the new descriptor and write it to the last */
/*           directory, following the current last descriptor.  The */
/*           sign of the new descriptor is a function of the type of */
/*           the current last descriptor. */

	    if (*type__ == next[(i__1 = ltype - 1) < 3 && 0 <= i__1 ? i__1 : 
		    s_rnge("next", i__1, "dascud_", (ftnlen)801)]) {

/*              TYPE is the successor in the type sequence of the type */
/*              of the previous descriptor; use a positive count. */

		descr = needed;
	    } else {
		descr = -needed;
	    }
	    i__1 = lword + 1;
	    i__2 = lword + 1;
	    dasuri_(handle, &lrec, &i__1, &i__2, &descr);

/*           Update the address range for this type.  Some care is needed */
/*           when updating the minimum address:  this value should be */
/*           assigned only if this is the first descriptor of TYPE in */
/*           this directory record. */

	    if (dscrec < lrec) {
		minadr = last + room + 1;
		dasuri_(handle, &lrec, &rngloc, &rngloc, &minadr);
	    }
	    maxadr = last + *nwords;
	    i__1 = rngloc + 1;
	    i__2 = rngloc + 1;
	    dasuri_(handle, &lrec, &i__1, &i__2, &maxadr);

/*           Update the file summary:  the location of the descriptor */
/*           and the last logical address for this type must be set. */

/*           The first free record follows the last data record in use. */

	    free += needed;
	    lastla[(i__1 = *type__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge(
		    "lastla", i__1, "dascud_", (ftnlen)841)] = last + *nwords;
	    lastrc[(i__1 = *type__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge(
		    "lastrc", i__1, "dascud_", (ftnlen)842)] = lrec;
	    lastwd[(i__1 = *type__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge(
		    "lastwd", i__1, "dascud_", (ftnlen)843)] = lword + 1;

/*           Before writing out the summary, see whether we'll need */
/*           a new directory; this will decide whether the first free */
/*           record changes. */

/*           If we just filled in the last descriptor in a directory, */
/*           it's time to add a new directory record to the file. */
/*           All we have to do at the moment is make room for it, and */
/*           set the forward pointer of the current directory record */
/*           to point to the saved record.  Initialize the pointers */
/*           of the new directory record to make the linked list valid. */

	    if (lword + 1 == 256) {

/*              Update the previous directory to point forward to the */
/*              next one. */

		dasuri_(handle, &lrec, &c__2, &c__2, &free);

/*              Prepare the new directory record: clear it, set the */
/*              backward pointer, and write the record. */

		cleari_(&c__256, dirrec);
		dirrec[0] = lrec;
		daswri_(handle, &free, dirrec);

/*              Update the free record number. */

		++free;
	    }

/*           Now write out the file summary. */

	    dasufs_(handle, &nresvr, &nresvc, &ncomr, &ncomc, &free, lastla, 
		    lastrc, lastwd);
	} else {

/*           This is case (3c).  We must put the new descriptor in */
/*           the last directory record, which is currently empty. */

/*           As in case (3b), we may have to update the directory */
/*           containing the current last descriptor of TYPE, if the */
/*           range of addresses covered by the cluster it describes was */
/*           increased by the last data addition.  Of course, this */
/*           update is required only if there IS such a descriptor. */

	    if (dscrec > 0 && room > 0) {

/*              Update the address range for TYPE in record DSCREC. */
/*              The upper bound is increased by ROOM, since that many */
/*              words of TYPE were added to the last record in the */
/*              last cluster of TYPE described by that directory. */

		maxadr = last + room;
		i__1 = rngloc + 1;
		i__2 = rngloc + 1;
		dasuri_(handle, &dscrec, &i__1, &i__2, &maxadr);
	    }

/*           Obtain the record number for this directory. */

	    dasrri_(handle, &lrec, &c__2, &c__2, &recno);

/*           Now fill in the new directory record.  Start with a clean */
/*           record. */

	    cleari_(&c__256, dirrec);

/*           Set the backward pointer, the address range for TYPE, */
/*           initial data type, and record count. */

	    dirrec[0] = lrec;
	    dirrec[(i__1 = rngloc - 1) < 256 && 0 <= i__1 ? i__1 : s_rnge(
		    "dirrec", i__1, "dascud_", (ftnlen)937)] = last + room + 
		    1;
	    dirrec[(i__1 = rngloc) < 256 && 0 <= i__1 ? i__1 : s_rnge("dirrec"
		    , i__1, "dascud_", (ftnlen)938)] = last + *nwords;
	    dirrec[8] = *type__;
	    dirrec[9] = needed;

/*           Write out the record. */

	    daswri_(handle, &recno, dirrec);

/*           Update the file summary to reflect the new record and word */
/*           offsets of the last descriptor of the indicated type.  The */
/*           last address of TYPE has increased also.  The first free */
/*           record lies after the added data records. */

	    free += needed;
	    lastla[(i__1 = *type__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge(
		    "lastla", i__1, "dascud_", (ftnlen)955)] = last + *nwords;
	    lastrc[(i__1 = *type__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge(
		    "lastrc", i__1, "dascud_", (ftnlen)956)] = recno;
	    lastwd[(i__1 = *type__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge(
		    "lastwd", i__1, "dascud_", (ftnlen)957)] = 10;
	    dasufs_(handle, &nresvr, &nresvc, &ncomr, &ncomc, &free, lastla, 
		    lastrc, lastwd);
	}
    }
    chkout_("DASCUD", (ftnlen)6);
    return 0;
} /* dascud_ */

