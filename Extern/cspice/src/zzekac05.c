/* zzekac05.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__2 = 2;
static logical c_false = FALSE_;
static integer c__128 = 128;

/* $Procedure     ZZEKAC05 ( EK, add class 5 column to segment ) */
/* Subroutine */ int zzekac05_(integer *handle, integer *segdsc, integer *
	coldsc, doublereal *dvals, integer *entszs, logical *nlflgs)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    doublereal page[128];
    integer nelt, from, size;
    extern /* Subroutine */ int zzekcnam_(integer *, integer *, char *, 
	    ftnlen), zzekpgwd_(integer *, integer *, doublereal *), zzeksfwd_(
	    integer *, integer *, integer *, integer *), zzekspsh_(integer *, 
	    integer *);
    integer i__, n, p, ndata, pbase;
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen);
    integer class__, nlink, p2, nrows;
    extern /* Subroutine */ int cleard_(integer *, doublereal *);
    extern logical return_(void);
    char column[32];
    integer adrbuf[126], bufptr, colidx, cursiz, nulptr, remain, to;
    logical cntinu, fixsiz, newreq, nullok;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen), sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    integer row;
    extern /* Subroutine */ int zzekaps_(integer *, integer *, integer *, 
	    logical *, integer *, integer *);

/* $ Abstract */

/*     Add an entire class 5 column to an EK segment. */

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


/*     Include Section:  EK Column Name Size */

/*        ekcnamsz.inc Version 1    17-JAN-1995 (NJB) */


/*     Size of column name, in characters. */


/*     End Include Section:  EK Column Name Size */

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
/*     HANDLE     I   Handle attached to new EK file. */
/*     SEGDSC     I   Segment descriptor. */
/*     COLDSC     I   Column descriptor. */
/*     DVALS      I   D.p. values to add to column. */
/*     ENTSZS     I   Array of sizes of column entries. */
/*     NLFLGS     I   Array of null flags for column entries. */

/* $ Detailed_Input */

/*     HANDLE         the handle of an EK file that is open for writing. */
/*                    A `begin segment for fast load' operation must */
/*                    have already been performed for the designated */
/*                    segment. */

/*     SEGDSC         is a descriptor for the segment to which data is */
/*                    to be added.  The segment descriptor is not */
/*                    updated by this routine, but some fields in the */
/*                    descriptor will become invalid after this routine */
/*                    returns. */

/*     COLDSC         is a descriptor for the column to be added.  The */
/*                    column attributes must be filled in, but any */
/*                    pointers may be uninitialized. */

/*     ENTSZS         is an array containing sizes of column entries. */
/*                    The Ith element of ENTSZS gives the size of the */
/*                    Ith column entry.  ENTSZS is used only for columns */
/*                    having variable-size entries.  For such columns, */
/*                    the dimension of ENTSZS must be at least NROWS. */
/*                    The size of null entries should be set to zero. */

/*                    For columns having fixed-size entries, the */
/*                    dimension of this array may be any positive value. */

/*     DVALS          is an array containing the entire set of column */
/*                    entries for the specified column.  The entries */
/*                    are listed in row-order:  the column entry for the */
/*                    first row of the segment is first, followed by the */
/*                    column entry for the second row, and so on.  The */
/*                    number of column entries must match the declared */
/*                    number of rows in the segment.  For columns having */
/*                    fixed-size entries, a null entry must be allocated */
/*                    the same amount of space occupied by a non-null */
/*                    entry in the array DVALS.  For columns having */
/*                    variable-size entries, null entries do not require */
/*                    any space in the DVALS array, but in any case must */
/*                    have their allocated space described correctly by */
/*                    the corresponding element of the ENTSZS array */
/*                    (described below). */

/*     ENTSZS         is an array containing sizes of column entries. */
/*                    The Ith element of ENTSZS gives the size of the */
/*                    Ith column entry.  ENTSZS is used only for columns */
/*                    having variable-size entries.  For such columns, */
/*                    the dimension of ENTSZS must be at least NROWS. */
/*                    The size of null entries should be set to zero. */

/*                    For columns having fixed-size entries, the */
/*                    dimension of this array may be any positive value. */

/*     NLFLGS         is an array of logical flags indicating whether */
/*                    the corresponding entries are null.  If the Ith */
/*                    element of NLFLGS is .FALSE., the Ith column entry */
/*                    defined by DVALS is added to the specified segment */
/*                    in the specified kernel file. */

/*                    If the Ith element of NLFGLS is .TRUE., the */
/*                    contents of the Ith column entry are undefined. */

/*                    NLFLGS is used only for columns that allow null */
/*                    values; it's ignored for other columns. */

/* $ Detailed_Output */

/*     None.  See $Particulars for a description of the effect of this */
/*     routine. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If HANDLE is invalid, the error will be diagnosed by routines */
/*         called by this routine. */

/*     2)  If an I/O error occurs while reading or writing the indicated */
/*         file, the error will be diagnosed by routines called by this */
/*         routine. */

/* $ Files */

/*     See the EK Required Reading for a discussion of the EK file */
/*     format. */

/* $ Particulars */

/*     This routine operates by side effects:  it modifies the named */
/*     EK file by adding data to the specified column.  This routine */
/*     writes the entire contents of the specified column in one shot. */
/*     This routine creates columns much more efficiently than can be */
/*     done by sequential calls to EKACED, but has the drawback that */
/*     the caller must use more memory for the routine's inputs.  This */
/*     routine cannot be used to add data to a partially completed */
/*     column. */

/* $ Examples */

/*     See EKACLD. */

/* $ Restrictions */

/*     1)  This routine assumes the EK scratch area has been set up */
/*         properly for a fast load operation.  This routine writes */
/*         to the EK scratch area as well. */

/*     2)  Only one segment can be created at a time using the fast */
/*         load routines. */

/*     3)  No other EK operation may interrupt a fast load.  For */
/*         example, it is not valid to issue a query while a fast load */
/*         is in progress. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 22-JUL-1996 (NJB) */

/*        Bug fix:  case of 100% null data values is now handled */
/*        correctly.  Previous version line was changed from "Beta" */
/*        to "SPICELIB." */

/* -    SPICELIB Version 1.0.0, 23-SEP-1995 (NJB) */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.1.0, 22-JUL-1996 (NJB) */

/*        Bug fix:  case of 100% null data values is now handled */
/*        correctly.  The test to determine when to write a page */
/*        was fixed to handle this case. */

/*        Previous version line was changed from "Beta" */
/*        to "SPICELIB." */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("ZZEKAC05", (ftnlen)8);
    }

/*     Grab the column's attributes. */

    class__ = coldsc[0];
    nulptr = coldsc[7];
    colidx = coldsc[8];
    size = coldsc[3];
    nullok = nulptr != -1;
    fixsiz = size != -1;

/*     This column had better be class 5. */

    if (class__ != 5) {
	zzekcnam_(handle, coldsc, column, (ftnlen)32);
	setmsg_("Column class code # found in descriptor for column #.  Clas"
		"s should be 5.", (ftnlen)73);
	errint_("#", &class__, (ftnlen)1);
	errch_("#", column, (ftnlen)1, (ftnlen)32);
	sigerr_("SPICE(NOCLASS)", (ftnlen)14);
	chkout_("ZZEKAC05", (ftnlen)8);
	return 0;
    }

/*     Push the column's ordinal index on the stack.  This allows us */
/*     to identify the column the addresses belong to. */

    zzekspsh_(&c__1, &colidx);

/*     Find the number of rows in the segment. */

    nrows = segdsc[5];

/*     Record the number of data values to write. */

    if (nullok) {

/*        Sum the sizes of the non-null column entries; these are the */
/*        ones that will take up space. */

	ndata = 0;
	i__1 = nrows;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    if (! nlflgs[i__ - 1]) {
		if (fixsiz) {
		    ndata += size;
		} else {
		    ndata += entszs[i__ - 1];
		}
	    }
	}
    } else {
	if (fixsiz) {
	    ndata = nrows * size;
	} else {
	    ndata = 0;
	    i__1 = nrows;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		ndata += entszs[i__ - 1];
	    }
	}
    }
    if (ndata > 0) {

/*        There's some data to write, so allocate a page.  Also */
/*        prepare a data buffer to be written out as a page. */

	zzekaps_(handle, segdsc, &c__2, &c_false, &p, &pbase);
	cleard_(&c__128, page);
    }

/*     Write the input data out to the target file a page at a time. */
/*     Null values don't get written. */

/*     While we're at it, we'll push onto the EK stack the addresses */
/*     of the column entries.  We use the constant NULL rather than an */
/*     address to represent null entries. */

/*     We'll use FROM to indicate the element of DVALS we're */
/*     considering, TO to indicate the element of PAGE to write */
/*     to, and BUFPTR to indicate the element of ADRBUF to write */
/*     addresses to.  The variable NELT is the count of the column entry */
/*     elements written for the current entry.  The variable N indicates */
/*     the number of d.p. numbers written to the current page. */

    remain = ndata;
    from = 1;
    to = 1;
    bufptr = 1;
    row = 1;
    nelt = 1;
    n = 0;
    nlink = 0;
    while(row <= nrows) {

/*        NEWREQ is set to TRUE if we discover that the next column */
/*        entry must start on a new page. */

	newreq = FALSE_;
	if (nullok && nlflgs[row - 1]) {
	    if (fixsiz) {
		cursiz = size;
	    } else {
		cursiz = entszs[row - 1];
	    }
	    from += cursiz;
	    adrbuf[(i__1 = bufptr - 1) < 126 && 0 <= i__1 ? i__1 : s_rnge(
		    "adrbuf", i__1, "zzekac05_", (ftnlen)417)] = -2;
	    ++bufptr;
	    ++row;
	    nelt = 1;
	    cntinu = FALSE_;
	} else {
	    if (nelt == 1) {

/*              We're about to write out a new column entry.  We must */
/*              insert the element count into the page before writing the */
/*              data.  The link count for the current page must be */
/*              incremented to account for this new entry. */

/*              At this point, we're guaranteed at least two free */
/*              spaces in the current page. */

		if (fixsiz) {
		    cursiz = size;
		} else {
		    cursiz = entszs[row - 1];
		}
		adrbuf[(i__1 = bufptr - 1) < 126 && 0 <= i__1 ? i__1 : s_rnge(
			"adrbuf", i__1, "zzekac05_", (ftnlen)443)] = to + 
			pbase;
		++bufptr;
		page[(i__1 = to - 1) < 128 && 0 <= i__1 ? i__1 : s_rnge("page"
			, i__1, "zzekac05_", (ftnlen)445)] = (doublereal) 
			cursiz;
		++to;
		++n;
		++nlink;
	    }

/*           At this point, there's at least one free space in the */
/*           current page. */

	    page[(i__1 = to - 1) < 128 && 0 <= i__1 ? i__1 : s_rnge("page", 
		    i__1, "zzekac05_", (ftnlen)456)] = dvals[from - 1];
	    ++to;
	    ++n;
	    ++from;
	    --remain;

/*           Decide whether we must continue the current entry on another */
/*           data page. */

	    cntinu = nelt < cursiz && n == 126;
	    if (nelt == cursiz) {

/*              The current element is the last of the current column */
/*              entry. */

/*              Determine whether we must start the next column entry on */
/*              a new page.  To start a column entry on the current page, */
/*              we must have enough room for the element count and at */
/*              least the first entry element. */

		if (remain > 0) {
		    newreq = n > 124;
		}
		nelt = 1;
		++row;
	    } else {
		++nelt;
	    }
	}
	if (bufptr > 126 || row > nrows) {

/*           The address buffer is full or we're out of input values */
/*           to look at, so push the buffer contents on the stack. */

	    i__1 = bufptr - 1;
	    zzekspsh_(&i__1, adrbuf);
	    bufptr = 1;
	}
	if (cntinu || newreq || row > nrows && ndata > 0) {

/*           It's time to write out the current page.  First set the link */
/*           count. */

	    page[127] = (doublereal) nlink;

/*           Write out the data page. */

	    zzekpgwd_(handle, &p, page);

/*           If there's more data to write, allocate another page. */

	    if (remain > 0) {
		zzekaps_(handle, segdsc, &c__2, &c_false, &p2, &pbase);
		cleard_(&c__128, page);
		n = 0;
		nlink = 0;
		to = 1;

/*              If we're continuing an element from the previous page, */
/*              link the previous page to the current one. */

		if (cntinu) {
		    zzeksfwd_(handle, &c__2, &p, &p2);
		}
		p = p2;
	    }

/*           We've allocated a new data page if we needed one. */

	}

/*        We've written out the last completed data page. */

    }

/*     We've processed all entries of the input array. */

    chkout_("ZZEKAC05", (ftnlen)8);
    return 0;
} /* zzekac05_ */

