/* zzekad03.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c_n2 = -2;
static integer c__1 = 1;
static logical c_false = FALSE_;
static integer c__0 = 0;

/* $Procedure     ZZEKAD03 ( EK, add data to class 3 column ) */
/* Subroutine */ int zzekad03_(integer *handle, integer *segdsc, integer *
	coldsc, integer *recptr, char *cval, logical *isnull, ftnlen cval_len)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    extern /* Subroutine */ int zzekiic1_(integer *, integer *, integer *, 
	    char *, integer *, logical *, ftnlen);
    extern integer zzekrp2n_(integer *, integer *, integer *);
    extern /* Subroutine */ int zzekpgbs_(integer *, integer *, integer *), 
	    zzekglnk_(integer *, integer *, integer *, integer *), zzeksfwd_(
	    integer *, integer *, integer *, integer *), zzekslnk_(integer *, 
	    integer *, integer *, integer *);
    integer n, p, mbase, pbase;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer recno, ncols, itype, lastw;
    extern integer rtrim_(char *, ftnlen);
    integer p2;
    extern /* Subroutine */ int dasudi_(integer *, integer *, integer *, 
	    integer *), dasudc_(integer *, integer *, integer *, integer *, 
	    integer *, char *, ftnlen);
    integer colidx, datptr, nlinks, nwrite, pcount, prvbas, ptrloc, strlen;
    logical fixlen;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen), sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    integer lnb, pos;
    extern /* Subroutine */ int zzeksei_(integer *, integer *, integer *), 
	    zzekaps_(integer *, integer *, integer *, logical *, integer *, 
	    integer *);

/* $ Abstract */

/*     Add a column entry to a class 3 column in a specified EK record. */
/*     Class 3 columns contain scalar, character values. */

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
/*     CVAL       I   Character string value. */
/*     ISNULL     I   Null flag. */

/* $ Detailed_Input */

/*     HANDLE         is a file handle of an EK open for write access. */

/*     SEGDSC         is the descriptor of the segment in which */
/*                    the specified column entry is to be written. */

/*     COLDSC         is the descriptor of the column in which */
/*                    the specified column entry is to be written. */

/*     RECPTR         is a pointer to the record containing the column */
/*                    entry to be written. */

/*     CVAL           is the character string value that will be written */
/*                    to the specified column entry. */

/*     ISNULL         is a logical flag indicating whether the value */
/*                    of the specified column entry is to be set to NULL. */
/*                    If so, the input CVAL is ignored. */

/* $ Detailed_Output */

/*     None.  See the $Particulars section for a description of the */
/*     effect of this routine. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If HANDLE is invalid, the error will be diagnosed by routines */
/*         called by this routine.  The file is not modified. */

/*     2)  If the ordinal position of the column specified by COLDSC */
/*         is out of range, the error SPICE(INVALIDINDEX) is signaled. */
/*         The file is not modified. */

/*     3)  If the input flag ISNULL is .TRUE. but the target column */
/*         does not allow nulls, the error SPICE(BADATTRIBUTE) is */
/*         signaled.  The file is not modified. */

/*     4)  If RECPTR is invalid, a DAS addressing error may occur.  The */
/*         error in *not* trapped in advance.  This routine assumes that */
/*         a valid value of RECPTR has been supplied by the caller. */

/*     3)  If an I/O error occurs while reading or writing the indicated */
/*         file, the error will be diagnosed by routines called by this */
/*         routine.  The file may be corrupted. */

/* $ Files */

/*     See the EK Required Reading for a discussion of the EK file */
/*     format. */

/* $ Particulars */

/*     This routine operates by side effects:  it sets the value of a */
/*     column entry in an EK segment.  If the column is indexed, the */
/*     index is updated to reflect the presence of the new entry.  This */
/*     routine is intended to set values of uninitialized column entries */
/*     only.  To update existing entries, use the ZZEKUExx routines, or */
/*     at the user level, the EKUCEx routines. */

/*     This routine does not participate in shadowing functions.  If the */
/*     target EK is shadowed, the caller is responsible for performing */
/*     necessary backup operations.  If the target EK is not shadowed, */
/*     the target record's status is not modified. */

/*     The changes made by this routine to the target EK file become */
/*     permanent when the file is closed.  Failure to close the file */
/*     properly will leave it in an indeterminate state. */

/* $ Examples */

/*     See EKACEC. */

/* $ Restrictions */

/*     1) This routine cannot be used to update existing column entries. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB) */

/*        Corrected typos in comments. */

/* -    Beta Version 1.0.0, 27-SEP-1995 (NJB) */

/* -& */

/*     SPICELIB functions */


/*     Non-SPICELIB functions */


/*     Local variables */


/*     Use discovery check-in. */

/*     Make sure the column exists. */

    ncols = segdsc[4];
    colidx = coldsc[8];
    if (colidx < 1 || colidx > ncols) {
	chkin_("ZZEKAD03", (ftnlen)8);
	setmsg_("Column index = #; valid range is 1:#.", (ftnlen)37);
	errint_("#", &colidx, (ftnlen)1);
	errint_("#", &ncols, (ftnlen)1);
	sigerr_("SPICE(INVALIDINDEX)", (ftnlen)19);
	chkout_("ZZEKAD03", (ftnlen)8);
	return 0;
    }

/*     If the value is null, make sure that nulls are permitted */
/*     in this column. */

    if (*isnull && coldsc[7] != 1) {
	recno = zzekrp2n_(handle, &segdsc[1], recptr);
	chkin_("ZZEKAD03", (ftnlen)8);
	setmsg_("Column having index # in segment # does not allow nulls, bu"
		"t a null value was supplied for the element in record #.", (
		ftnlen)115);
	errint_("#", &colidx, (ftnlen)1);
	errint_("#", &segdsc[1], (ftnlen)1);
	errint_("#", &recno, (ftnlen)1);
	sigerr_("SPICE(BADATTRIBUTE)", (ftnlen)19);
	chkout_("ZZEKAD03", (ftnlen)8);
	return 0;
    }

/*     Decide the length of the string value.  If the column contains */
/*     variable-length strings, the effective length of the string is */
/*     just the non-blank length of CVAL.  Otherwise, the effective */
/*     string length is the minimum of the non-blank length and the */
/*     column's declared string length.  We don't store trailing blanks. */

    fixlen = coldsc[2] != -1;
    lnb = rtrim_(cval, cval_len);
    if (fixlen) {
	strlen = min(coldsc[2],lnb);
    } else {
	strlen = lnb;
    }

/*     Compute the data pointer location. */

    ptrloc = *recptr + 2 + colidx;
    if (*isnull) {

/*        All we need do is set the data pointer.  The segment's */
/*        metadata are not affected. */

	dasudi_(handle, &ptrloc, &ptrloc, &c_n2);
    } else {

/*        Write out the data value.  If we run out of room in the */
/*        page we're writing to, we allocate a new page and link */
/*        the previous page to it. */

	n = strlen;
	pos = 1;
	lastw = segdsc[18];
	p = segdsc[15];
	pcount = 0;
	while(n > 0) {

/*           Write as much data as possible into the current page. */

	    if (lastw < 1009) {

/*              There's room in the current page.  We never split an */
/*              encoded character count across pages, and we always */
/*              write at least one data character to the current page. */
/*              This practice is slightly wasteful of space but greatly */
/*              simplifies our logic. */

/*              Keep track of the number of pages our string spans. */

		++pcount;

/*              If this is the first data page, write the data pointer */
/*              into the record pointer and the character count into */
/*              the data page. */

		if (pcount == 1) {
		    zzekpgbs_(&c__1, &p, &pbase);
		    datptr = pbase + lastw + 1;
		    dasudi_(handle, &ptrloc, &ptrloc, &datptr);
		    zzeksei_(handle, &datptr, &strlen);

/*                 Advance the data pointer to the first data */
/*                 character's position.  The last word in use */
/*                 increases as well. */

		    datptr += 5;
		    lastw += 5;
		} else {

/*                 We still need the data pointer. */

		    datptr = pbase + 1;
		}

/*              Compute the number of characters to write into this page, */
/*              and write that number of characters. */

/* Computing MIN */
		i__1 = 1014 - lastw;
		nwrite = min(i__1,n);
		i__1 = datptr + nwrite - 1;
		dasudc_(handle, &datptr, &i__1, &c__1, &nwrite, cval + (pos - 
			1), cval_len - (pos - 1));
		n -= nwrite;
		pos += nwrite;

/*              The page containing the data item gains a link. */

		zzekglnk_(handle, &c__1, &p, &nlinks);
		i__1 = nlinks + 1;
		zzekslnk_(handle, &c__1, &p, &i__1);

/*              The last character word in use must be updated. */

		lastw += nwrite;
		segdsc[18] = lastw;

/*              Retain the base address of this data page. */

		prvbas = pbase;
	    } else {

/*              Allocate a data page.  If this is not the first data */
/*              page written to, link the previous page to the current */
/*              one. */

		zzekaps_(handle, segdsc, &c__1, &c_false, &p2, &pbase);
		if (pcount > 0) {
		    zzeksfwd_(handle, &c__1, &p, &p2);
		}

/*              The last character page and word in use must be updated. */

		p = p2;
		lastw = 0;
		segdsc[15] = p;
		segdsc[18] = lastw;

/*              Make sure the link count is zeroed out. */

		zzekslnk_(handle, &c__1, &p, &c__0);
	    }
	}
    }

/*     Write out the updated segment descriptor. */

    mbase = segdsc[2];
    i__1 = mbase + 1;
    i__2 = mbase + 24;
    dasudi_(handle, &i__1, &i__2, segdsc);

/*     If the column is indexed, we must update the index to account */
/*     for the new element. */

    itype = coldsc[5];
    if (itype != -1) {

/*        The column is indexed. */

	if (itype == 1) {

/*           The column has a type 1 index.  Insert the record number */
/*           of the current element at the appropriate location. */

	    zzekiic1_(handle, segdsc, coldsc, cval, recptr, isnull, cval_len);
	} else {
	    chkin_("ZZEKAD03", (ftnlen)8);
	    setmsg_("Column having index # in segment # has index type #.", (
		    ftnlen)52);
	    errint_("#", &colidx, (ftnlen)1);
	    errint_("#", &segdsc[1], (ftnlen)1);
	    errint_("#", &itype, (ftnlen)1);
	    sigerr_("SPICE(INVALIDTYPE)", (ftnlen)18);
	    chkout_("ZZEKAD03", (ftnlen)8);
	    return 0;
	}
    }
    return 0;
} /* zzekad03_ */

