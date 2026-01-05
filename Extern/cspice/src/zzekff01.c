/* zzekff01.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__100 = 100;
static integer c__256 = 256;
static integer c__24 = 24;
static integer c__1 = 1;
static integer c__2 = 2;
static integer c__3 = 3;
static integer c__254 = 254;
static integer c__1014 = 1014;
static integer c__126 = 126;

/* $Procedure      ZZEKFF01 ( EK, finish fast load, segment type 1 ) */
/* Subroutine */ int zzekff01_(integer *handle, integer *segno, integer *
	rcptrs)
{
    /* System generated locals */
    integer i__1, i__2, i__3, i__4, i__5;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer base, tree;
    extern /* Subroutine */ int zzektr1s_(integer *, integer *, integer *, 
	    integer *), zzekmloc_(integer *, integer *, integer *, integer *),
	     zzekpgpg_(integer *, integer *, integer *, integer *), zzekpgwi_(
	    integer *, integer *, integer *), zzektrit_(integer *, integer *);
    integer i__, j, p, ipage[256], mbase, npage, pbase;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer recno, ncols, nrows, adrbuf[100], nr;
    extern logical return_(void);
    integer addrss, colidx, colord[100], pagloc, remain, rpsize, segdsc[24], 
	    stkbas, stkhan, stkseg;
    extern /* Subroutine */ int cleari_(integer *, integer *), setmsg_(char *,
	     ftnlen), errint_(char *, integer *, ftnlen), sigerr_(char *, 
	    ftnlen), chkout_(char *, ftnlen), dasrdi_(integer *, integer *, 
	    integer *, integer *), dasudi_(integer *, integer *, integer *, 
	    integer *);
    integer col, loc, nrp, row;
    extern /* Subroutine */ int zzeksrd_(integer *, integer *, integer *);

/* $ Abstract */

/*     Complete a fast load operation on a new type 1 E-kernel segment. */

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


/*     Include Section:  EK General Limit Parameters */

/*        ekglimit.inc  Version 1    21-MAY-1995 (NJB) */


/*     This file contains general limits for the EK system. */

/*     MXCLSG is the maximum number of columns allowed in a segment. */
/*     This limit applies to logical tables as well, since all segments */
/*     in a logical table must have the same column definitions. */


/*     End Include Section:  EK General Limit Parameters */

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
/*     SEGNO      I   Segment number. */
/*     RCPTRS    I-O  Record pointers. */

/* $ Detailed_Input */

/*     HANDLE         the handle of an EK file that is open for writing. */
/*                    A `begin segment for fast load' operation must */
/*                    have already been performed for the designated */
/*                    segment. */

/*     SEGNO          is the number of the type 1 segment to complete. */

/*     RCPTRS         is an array of record pointers for the input */
/*                    segment.  This array is obtained as an output */
/*                    from EKIFLD, the routine called to initiate a */
/*                    fast load. */

/* $ Detailed_Output */

/*     WORK           is the input work space array, after use.  WORK */
/*                    will generally be modified by this routine. */

/*     See the $Particulars section for a description of the */
/*     effects of this routine. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If HANDLE is invalid, the error will be diagnosed by routines */
/*         called by this routine. */

/*     2)  If an attempt is made to finish a segment other than the */
/*         one last initialized by EKIFLD, the error SPICE(WRONGSEGMENT) */
/*         is signaled. */

/*     3)  If an I/O error occurs while reading or writing the indicated */
/*         file, the error will be diagnosed by routines called by this */
/*         routine. */

/* $ Files */

/*     See the EK Required Reading for a discussion of the EK file */
/*     format. */

/* $ Particulars */

/*     This routine completes a type 1 EK segment after the data has been */
/*     written via the fast column loader routines. */

/* $ Examples */

/*     See EKFFLD. */

/* $ Restrictions */

/*     1)  Only one segment can be created at a time using the fast */
/*         load routines. */

/*     2)  No other EK operation may interrupt a fast load.  For */
/*         example, it is not valid to issue a query while a fast load */
/*         is in progress. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 11-OCT-2021 (NJB) */

/*        All local arrays are now completely initialized. */

/*        Corrected typos in comments. */

/* -    Beta Version 1.0.0, 08-NOV-1995 (NJB) */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("ZZEKFF01", (ftnlen)8);

/*     Initialize all arrays. */

    cleari_(&c__100, adrbuf);
    cleari_(&c__100, colord);
    cleari_(&c__256, ipage);
    cleari_(&c__24, segdsc);

/*     Dig the handle and segment number out of the EK stack.  If the */
/*     stacked values don't match the inputs HANDLE and SEGNO, we've */
/*     got trouble. */

    zzeksrd_(&c__1, &c__1, &stkhan);
    zzeksrd_(&c__2, &c__2, &stkseg);
    if (stkhan != *handle || stkseg != *segno) {
	setmsg_("Attempt to finish fast load of wrong segment.  Input segmen"
		"t number is #; stacked segment number is #.  Input handle is"
		" #; stacked handle is #.", (ftnlen)143);
	errint_("#", segno, (ftnlen)1);
	errint_("#", &stkseg, (ftnlen)1);
	errint_("#", handle, (ftnlen)1);
	errint_("#", &stkhan, (ftnlen)1);
	sigerr_("SPICE(WRONGSEGMENT)", (ftnlen)19);
	chkout_("ZZEKFF01", (ftnlen)8);
	return 0;
    }

/*     Look up the segment descriptor for the indicated segment.  Find */
/*     out how many rows and columns the segment contains. */

    zzekmloc_(handle, segno, &p, &mbase);
    i__1 = mbase + 1;
    i__2 = mbase + 24;
    dasrdi_(handle, &i__1, &i__2, segdsc);
    nrows = segdsc[5];
    ncols = segdsc[4];

/*     Determine the order in which the columns were added.  The order */
/*     may differ from that in which the columns were declared.  The */
/*     ordinal position of each column is stored on the stack right */
/*     before its address data.  COLORD will map ordinal positions given */
/*     by a column declaration to ordinal positions on the stack. */


    i__1 = ncols;
    for (i__ = 1; i__ <= i__1; ++i__) {
	loc = (i__ - 1) * (nrows + 1) + 3;
	zzeksrd_(&loc, &loc, &colidx);
	colord[(i__2 = colidx - 1) < 100 && 0 <= i__2 ? i__2 : s_rnge("colord"
		, i__2, "zzekff01_", (ftnlen)254)] = i__;
    }

/*     We'll need to create a record pointer structure for each row */
/*     in the segment.  We compute the number of record pointers that */
/*     can fit on one page.  We also compute the number of pages we'll */
/*     need to hold the pointers. */

    rpsize = ncols + 2;
    nrp = 254 / rpsize;
    npage = (nrows + nrp - 1) / nrp;

/*     We'll write out record pointers a pageful at a time.  Each */
/*     record pointer is initialized to indicate that the record is */
/*     old, and that there is no corresponding modified record. */

    remain = nrows;
    recno = 0;
    i__1 = npage;
    for (i__ = 1; i__ <= i__1; ++i__) {

/*        Get the base address of the current page.  The address */
/*        can be derived from the address of the first record pointer */
/*        structure on the page. */

	addrss = rcptrs[recno] + 1;
	zzekpgpg_(&c__3, &addrss, &p, &pbase);
	cleari_(&c__254, ipage);

/*        NR is the number of record pointers we'll write to this page. */

	nr = min(nrp,remain);
	i__2 = nr;
	for (j = 1; j <= i__2; ++j) {

/*           Initialize the modified record pointer and status for */
/*           each record pointer on the page. */

	    base = (j - 1) * rpsize;
	    ipage[(i__3 = base) < 256 && 0 <= i__3 ? i__3 : s_rnge("ipage", 
		    i__3, "zzekff01_", (ftnlen)299)] = 1;
	    ipage[(i__3 = base + 1) < 256 && 0 <= i__3 ? i__3 : s_rnge("ipage"
		    , i__3, "zzekff01_", (ftnlen)300)] = -1;
	}

/*        For each column, take NR addresses off the stack and */
/*        write them into the page. */

	i__2 = ncols;
	for (col = 1; col <= i__2; ++col) {

/*           The stack starts out with the target file handle and */
/*           segment number.  Next comes the data for each column. */
/*           Each column is identified by its ordinal position.  The */
/*           addresses for the data of each column follow.  The addresses */
/*           for each column are stored contiguously. */

	    j = colord[(i__3 = col - 1) < 100 && 0 <= i__3 ? i__3 : s_rnge(
		    "colord", i__3, "zzekff01_", (ftnlen)316)];
	    stkbas = (j - 1) * (nrows + 1) + 3;
	    loc = stkbas + recno;
	    i__3 = loc + 1;
	    i__4 = loc + nr;
	    zzeksrd_(&i__3, &i__4, adrbuf);
	    i__3 = nr;
	    for (row = 1; row <= i__3; ++row) {
		base = (row - 1) * rpsize;
		pagloc = base + 2 + col;
		ipage[(i__4 = pagloc - 1) < 256 && 0 <= i__4 ? i__4 : s_rnge(
			"ipage", i__4, "zzekff01_", (ftnlen)326)] = adrbuf[(
			i__5 = row - 1) < 100 && 0 <= i__5 ? i__5 : s_rnge(
			"adrbuf", i__5, "zzekff01_", (ftnlen)326)];
	    }
	}

/*        Write out the initialized pointer page. */

	zzekpgwi_(handle, &p, ipage);
	recno += nr;
	remain -= nr;
    }

/*     Create the record pointer tree for this segment. */

    zzektrit_(handle, &tree);
    zzektr1s_(handle, &tree, &nrows, rcptrs);

/*     Update the record tree pointer and row count in the segment */
/*     descriptor.  Set the records of the last DAS words in use */
/*     to their maximum values, to ensure allocation of new pages */
/*     if further writes are done. */

    zzekmloc_(handle, segno, &p, &base);
    i__1 = base + 7;
    i__2 = base + 7;
    dasudi_(handle, &i__1, &i__2, &tree);
    i__1 = base + 6;
    i__2 = base + 6;
    dasudi_(handle, &i__1, &i__2, &nrows);
    i__1 = base + 19;
    i__2 = base + 19;
    dasudi_(handle, &i__1, &i__2, &c__1014);
    i__1 = base + 20;
    i__2 = base + 20;
    dasudi_(handle, &i__1, &i__2, &c__126);
    i__1 = base + 21;
    i__2 = base + 21;
    dasudi_(handle, &i__1, &i__2, &c__254);
    chkout_("ZZEKFF01", (ftnlen)8);
    return 0;
} /* zzekff01_ */

