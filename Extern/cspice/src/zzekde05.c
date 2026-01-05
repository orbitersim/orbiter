/* zzekde05.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c_n1 = -1;
static integer c__2 = 2;

/* $Procedure      ZZEKDE05 ( EK, delete column entry, class 5 ) */
/* Subroutine */ int zzekde05_(integer *handle, integer *segdsc, integer *
	coldsc, integer *recptr)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer i_dnnt(doublereal *);

    /* Local variables */
    integer base;
    extern integer zzekrp2n_(integer *, integer *, integer *);
    integer next;
    extern /* Subroutine */ int zzekpgch_(integer *, char *, ftnlen), 
	    zzekgfwd_(integer *, integer *, integer *, integer *), zzekglnk_(
	    integer *, integer *, integer *, integer *), zzekpgpg_(integer *, 
	    integer *, integer *, integer *), zzekslnk_(integer *, integer *, 
	    integer *, integer *);
    integer p;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer recno, nseen, nelts;
    extern logical failed_(void);
    extern /* Subroutine */ int dasrdd_(integer *, integer *, integer *, 
	    doublereal *), dasrdi_(integer *, integer *, integer *, integer *)
	    , dasudi_(integer *, integer *, integer *, integer *);
    extern logical return_(void);
    doublereal dpnelt;
    integer datptr, nlinks, ptrloc;
    extern /* Subroutine */ int chkout_(char *, ftnlen), setmsg_(char *, 
	    ftnlen), errint_(char *, integer *, ftnlen), errhan_(char *, 
	    integer *, ftnlen), sigerr_(char *, ftnlen), zzekdps_(integer *, 
	    integer *, integer *, integer *);

/* $ Abstract */

/*     Delete a specified class 5 column entry from an EK record. */

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

/*     PRIVATE */
/*     UTILITY */

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


/*     Include Section:  EK Boolean Enumerated Type */


/*        ekbool.inc Version 1   21-DEC-1994 (NJB) */


/*     Within the EK system, boolean values sometimes must be */
/*     represented by integer or character codes.  The codes and their */
/*     meanings are listed below. */

/*     Integer code indicating `true': */


/*     Integer code indicating `false': */


/*     Character code indicating `true': */


/*     Character code indicating `false': */


/*     End Include Section:  EK Boolean Enumerated Type */

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


/*     Include Section:  EK Column Descriptor Parameters */

/*        ekcoldsc.inc Version 6    23-AUG-1995 (NJB) */


/*     Note:  The column descriptor size parameter CDSCSZ  is */
/*     declared separately in the include section CDSIZE$INC.FOR. */

/*     Offset of column descriptors, relative to start of segment */
/*     integer address range.  This number, when added to the last */
/*     integer address preceding the segment, yields the DAS integer */
/*     base address of the first column descriptor.  Currently, this */
/*     offset is exactly the size of a segment descriptor.  The */
/*     parameter SDSCSZ, which defines the size of a segment descriptor, */
/*     is declared in the include file eksegdsc.inc. */


/*     Size of column descriptor */


/*     Indices of various pieces of column descriptors: */


/*     CLSIDX is the index of the column's class code.  (We use the */
/*     word `class' to distinguish this item from the column's data */
/*     type.) */


/*     TYPIDX is the index of the column's data type code (CHR, INT, DP, */
/*     or TIME).  The type is actually implied by the class, but it */
/*     will frequently be convenient to look up the type directly. */



/*     LENIDX is the index of the column's string length value, if the */
/*     column has character type.  A value of IFALSE in this element of */
/*     the descriptor indicates that the strings have variable length. */


/*     SIZIDX is the index of the column's element size value.  This */
/*     descriptor element is meaningful for columns with fixed-size */
/*     entries.  For variable-sized columns, this value is IFALSE. */


/*     NAMIDX is the index of the base address of the column's name. */


/*     IXTIDX is the data type of the column's index.  IXTIDX */
/*     contains a type value only if the column is indexed. For columns */
/*     that are not indexed, the location IXTIDX contains the boolean */
/*     value IFALSE. */


/*     IXPIDX is a pointer to the column's index.  IXTPDX contains a */
/*     meaningful value only if the column is indexed.  The */
/*     interpretation of the pointer depends on the data type of the */
/*     index. */


/*     NFLIDX is the index of a flag indicating whether nulls are */
/*     permitted in the column.  The value at location NFLIDX is */
/*     ITRUE if nulls are permitted and IFALSE otherwise. */


/*     ORDIDX is the index of the column's ordinal position in the */
/*     list of columns belonging to the column's parent segment. */


/*     METIDX is the index of the column's integer metadata pointer. */
/*     This pointer is a DAS integer address. */


/*     The last position in the column descriptor is reserved.  No */
/*     parameter is defined to point to this location. */


/*     End Include Section:  EK Column Descriptor Parameters */

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


/*     Include Section:  EK Data Page Parameters */

/*        ekfilpar.inc  Version 1  03-APR-1995 (NJB) */

/*     These parameters apply to EK files using architecture 4. */
/*     These files use a paged DAS file as their underlying file */
/*     structure. */

/*     In paged DAS EK files, data pages are structured:  they contain */
/*     metadata as well as data.  The metadata is located in the last */
/*     few addresses of each page, so as to interfere as little as */
/*     possible with calculation of data addresses. */

/*     Each data page belongs to exactly one segment.  Some bookkeeping */
/*     information, such as record pointers, is also stored in data */
/*     pages. */

/*     Each page contains a forward pointer that allows rapid lookup */
/*     of data items that span multiple pages.  Each page also keeps */
/*     track of the current number of links from its parent segment */
/*     to the page.  Link counts enable pages to `know' when they */
/*     are no longer in use by a segment; unused pages are deallocated */
/*     and returned to the free list. */

/*     The parameters in this include file depend on the parameters */
/*     declared in the include file ekpage.inc.  If those parameters */
/*     change, this file must be updated.  The specified parameter */
/*     declarations we need from that file are: */

/*        INTEGER               PGSIZC */
/*        PARAMETER           ( PGSIZC = 1024 ) */

/*        INTEGER               PGSIZD */
/*        PARAMETER           ( PGSIZD = 128 ) */

/*        INTEGER               PGSIZI */
/*        PARAMETER           ( PGSIZI = 256 ) */



/*     Character pages use an encoding mechanism to represent integer */
/*     metadata.  Each integer is encoded in five consecutive */
/*     characters. */


/*     Character data page parameters: */


/*     Size of encoded integer: */


/*     Usable page size: */


/*     Location of character forward pointer: */


/*     Location of character link count: */


/*     Double precision data page parameters: */

/*     Usable page size: */


/*     Location of d.p. forward pointer: */


/*     Location of d.p. link count: */


/*     Integer data page parameters: */

/*     Usable page size: */


/*     Location of integer forward pointer: */


/*     Location of integer link count: */


/*     End Include Section:  EK Data Page Parameters */

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


/*     Include Section:  EK Record Pointer Parameters */

/*        ekrecptr.inc Version 2  18-JUL-1995 (NJB) */


/*     This file declares parameters used in EK record pointers. */
/*     Each segment references data in a given record via two levels */
/*     of indirection:  a record number points to a record pointer, */
/*     which is a structured array of metadata and data pointers. */

/*     Record pointers always occupy contiguous ranges of integer */
/*     addresses. */

/*     The parameter declarations in this file depend on the assumption */
/*     that integer pages contain 256 DAS integer words and that the */
/*     maximum number of columns in a segment is 100.  Record pointers */
/*     are stored in integer data pages, so they must fit within the */
/*     usable data area afforded by these pages.  The size of the usable */
/*     data area is given by the parameter IPSIZE which is declared in */
/*     ekdatpag.inc.  The assumed value of IPSIZE is 254. */


/*     The first element of each record pointer is a status indicator. */
/*     The meanings of status indicators depend on whether the parent EK */
/*     is shadowed or not.  For shadowed EKs, allowed status values and */
/*     their meanings are: */

/*        OLD       The record has not been modified since */
/*                  the EK containing the record was opened. */

/*        UPDATE    The record is an update of a previously existing */
/*                  record.  The original record is now on the */
/*                  modified record list. */

/*        NEW       The record has been added since the EK containing the */
/*                  record was opened.  The record is not an update */
/*                  of a previously existing record. */

/*        DELOLD    This status applies only to a backup record. */
/*                  DELOLD status indicates that the record corresponds */
/*                  to a deleted OLD record in the source segment. */

/*        DELNEW    This status applies only to a backup record. */
/*                  DELNEW status indicates that the record corresponds */
/*                  to a deleted NEW record in the source segment. */

/*        DELUPD    This status applies only to a backup record. */
/*                  DELUPD status indicates that the record corresponds */
/*                  to a deleted UPDATEd record in the source segment. */

/*     In EKs that are not shadowed, all records have status OLD. */



/*     The following parameters refer to indices within the record */
/*     pointer structure: */

/*     Index of status indicator: */


/*     Each record pointer contains a pointer to its companion:  for a */
/*     record belonging to a shadowed EK, this is the backup counterpart, */
/*     or if the parent EK is itself a backup EK, a pointer to the */
/*     record's source record.  The pointer is UNINIT (see below) if the */
/*     record is unmodified. */

/*     Record companion pointers contain record numbers, not record */
/*     base addresses. */

/*     Index of record's companion pointer: */


/*     Each data item is referenced by an integer.  The meaning of */
/*     this integer depends on the representation of data in the */
/*     column to which the data item belongs.  Actual lookup of a */
/*     data item must be done by subroutines appropriate to the class of */
/*     the column to which the item belongs.  Note that data items don't */
/*     necessarily occupy contiguous ranges of DAS addresses. */

/*     Base address of data pointers: */


/*     Maximum record pointer size: */


/*     Data pointers are given the value UNINIT to start with; this */
/*     indicates that the data item is uninitialized.  UNINIT is */
/*     distinct from the value NULL.  NOBACK indicates an uninitialized */
/*     backup column entry. */


/*     End Include Section:  EK Record Pointer Parameters */

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


/*     Include Section:  EK Segment Descriptor Parameters */

/*        eksegdsc.inc  Version 8  06-NOV-1995 (NJB) */


/*     All `base addresses' referred to below are the addresses */
/*     *preceding* the item the base applies to.  This convention */
/*     enables simplied address calculations in many cases. */

/*     Size of segment descriptor.  Note:  the include file ekcoldsc.inc */
/*     must be updated if this parameter is changed.  The parameter */
/*     CDOFF in that file should be kept equal to SDSCSZ. */


/*     Index of the segment type code: */


/*     Index of the segment's number.  This number is the segment's */
/*     index in the list of segments contained in the EK to which */
/*     the segment belongs. */


/*     Index of the DAS integer base address of the segment's integer */
/*     meta-data: */


/*     Index of the DAS character base address of the table name: */


/*     Index of the segment's column count: */


/*     Index of the segment's record count: */


/*     Index of the root page number of the record tree: */


/*     Index of the root page number of the character data page tree: */


/*     Index of the root page number of the double precision data page */
/*     tree: */


/*     Index of the root page number of the integer data page tree: */


/*     Index of the `modified' flag: */


/*     Index of the `initialized' flag: */


/*     Index of the shadowing flag: */


/*     Index of the companion file handle: */


/*     Index of the companion segment number: */


/*     The next three items are, respectively, the page numbers of the */
/*     last character, d.p., and integer data pages allocated by the */
/*     segment: */


/*     The next three items are, respectively, the page-relative */
/*     indices of the last DAS word in use in the segment's */
/*     last character, d.p., and integer data pages: */


/*     Index of the DAS character base address of the column name list: */


/*     The last descriptor element is reserved for future use.  No */
/*     parameter is defined to point to this location. */


/*     End Include Section:  EK Segment Descriptor Parameters */

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
/*     SEGDSC     I   Segment descriptor. */
/*     COLDSC     I   Column descriptor. */
/*     RECPTR     I   Record pointer. */

/* $ Detailed_Input */

/*     HANDLE         is a file handle of an EK open for write access. */

/*     SEGDSC         is the descriptor of the segment from which to */
/*                    delete the specified column entry. */

/*     COLDSC         is the descriptor of the column from which to */
/*                    delete the specified column entry. */

/*     RECPTR         is a pointer to the record containing the column */
/*                    entry to delete. */

/* $ Detailed_Output */

/*     None.  See the $Particulars section for a description of the */
/*     effect of this routine. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If HANDLE is invalid, the error will be diagnosed by routines */
/*         called by this routine.  The file will not be modified. */

/*     2)  If an I/O error occurs while reading or writing the indicated */
/*         file, the error will be diagnosed by routines called by this */
/*         routine.  The file may be corrupted. */

/* $ Files */

/*     See the EK Required Reading for a discussion of the EK file */
/*     format. */

/* $ Particulars */

/*     This routine operates by side effects:  it deletes a column entry */
/*     from an EK segment.  The status of the record containing the entry */
/*     is set to `updated'.  The deleted entry is marked as */
/*     `uninitialized'. */

/*     The link counts for the pages containing the deleted column entry */
/*     are decremented.  If the count for a page becomes zero, that page */
/*     is freed.  If the entry to be deleted is already uninitialized */
/*     upon entry to this routine, no link counts are modified.  The */
/*     record containing the entry is still marked `updated' in this */
/*     case. */

/*     The changes made by this routine to the target EK file become */
/*     permanent when the file is closed.  Failure to close the file */
/*     properly will leave it in an indeterminate state. */

/* $ Examples */

/*     See EKDELR. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 07-FEB-2015 (NJB) */

/*        Now uses ERRHAN to insert DAS file name into */
/*        long error message. */

/*        Deleted unneeded declarations and code. */

/* -    Beta Version 1.0.0, 28-SEP-1995 (NJB) */

/* -& */

/*     SPICELIB functions */


/*     Non-SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("ZZEKDE05", (ftnlen)8);

/*     Before trying to actually modify the file, do every error */
/*     check we can. */

/*     Is this file handle valid--is the file open for paged write */
/*     access?  Signal an error if not. */

    zzekpgch_(handle, "WRITE", (ftnlen)5);
    if (failed_()) {
	chkout_("ZZEKDE05", (ftnlen)8);
	return 0;
    }

/*     Compute the data pointer location.  If the data pointer is */
/*     already set to `uninitialized', there's nothing to do.  If */
/*     the element is null, just set it to `uninitialized'.  The */
/*     presence of actual data obligates us to clean up, however. */

    ptrloc = *recptr + 2 + coldsc[8];
    dasrdi_(handle, &ptrloc, &ptrloc, &datptr);
    if (datptr > 0) {

/*        Get the element count for the entry. */

	dasrdd_(handle, &datptr, &datptr, &dpnelt);
	nelts = i_dnnt(&dpnelt);

/*        Set the data pointer to indicate the item is uninitialized. */

	dasudi_(handle, &ptrloc, &ptrloc, &c_n1);

/*        Find the number of the page containing the column entry. */

	zzekpgpg_(&c__2, &datptr, &p, &base);

/*        Look up the forward pointer.  This pointer will be valid */
/*        if the column entry is continued on another page. */

	zzekgfwd_(handle, &c__2, &p, &next);

/*        Get the link count for the current page.  If we have more */
/*        than one link to the page, decrement the link count.  If */
/*        we're down to one link, this deletion will finish off the */
/*        page:  we'll deallocate it. */

	zzekglnk_(handle, &c__2, &p, &nlinks);
	if (nlinks > 1) {
	    i__1 = nlinks - 1;
	    zzekslnk_(handle, &c__2, &p, &i__1);
	} else {

/*           If we removed the last item from the page, we can delete */
/*           the page.  ZZEKDPS adjusts the segment's metadata */
/*           to reflect the deallocation. */

	    zzekdps_(handle, segdsc, &c__2, &p);
	}
/* Computing MIN */
	i__1 = nelts, i__2 = base + 126 - datptr;
	nseen = min(i__1,i__2);
	while(nseen < nelts && ! failed_()) {

/*           The column entry is continued on the page indicated by */
/*           NEXT. */

/*           Get the link count for the current page.  If we have more */
/*           than one link to the page, decrement the link count.  If */
/*           we're down to one link, this deletion will finish off the */
/*           page:  we'll deallocate it. */

	    p = next;
	    zzekgfwd_(handle, &c__2, &p, &next);
	    zzekglnk_(handle, &c__2, &p, &nlinks);
	    if (nlinks > 1) {
		i__1 = nlinks - 1;
		zzekslnk_(handle, &c__2, &p, &i__1);
	    } else {

/*              If we removed the last item from the page, we can delete */
/*              the page.  ZZEKDPS adjusts the segment's metadata */
/*              to reflect the deallocation. */

		zzekdps_(handle, segdsc, &c__2, &p);
	    }
/* Computing MIN */
	    i__1 = nelts, i__2 = nseen + 126;
	    nseen = min(i__1,i__2);
	}
    } else if (datptr == -2) {

/*        Mark the entry as `uninitialized'. */

	dasudi_(handle, &ptrloc, &ptrloc, &c_n1);
    } else if (datptr != -1) {

/*        UNINIT was the last valid possibility.  The data pointer is */
/*        corrupted. */

	recno = zzekrp2n_(handle, &segdsc[1], recptr);
	setmsg_("Data pointer is corrupted. SEGNO = #; COLIDX =  #; RECNO = "
		"#; EK = #", (ftnlen)68);
	errint_("#", &segdsc[1], (ftnlen)1);
	errint_("#", &coldsc[8], (ftnlen)1);
	errint_("#", &recno, (ftnlen)1);
	errhan_("#", handle, (ftnlen)1);
	sigerr_("SPICE(BUG)", (ftnlen)10);
	chkout_("ZZEKDE05", (ftnlen)8);
	return 0;
    }

/*     Set the record's status to indicate that this record is updated. */

    i__1 = *recptr + 1;
    i__2 = *recptr + 1;
    dasudi_(handle, &i__1, &i__2, &c__2);
    chkout_("ZZEKDE05", (ftnlen)8);
    return 0;
} /* zzekde05_ */

