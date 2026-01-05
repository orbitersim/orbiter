/* ekappr.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure EKAPPR ( EK, append record onto segment ) */
/* Subroutine */ int ekappr_(integer *handle, integer *segno, integer *recno)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    integer nrec;
    extern /* Subroutine */ int zzekpgch_(integer *, char *, ftnlen), 
	    zzekmloc_(integer *, integer *, integer *, integer *);
    integer mbase;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern logical failed_(void);
    integer mp, segdsc[24];
    extern logical return_(void);
    extern /* Subroutine */ int chkout_(char *, ftnlen), dasrdi_(integer *, 
	    integer *, integer *, integer *), ekinsr_(integer *, integer *, 
	    integer *);

/* $ Abstract */

/*     Append a new, empty record at the end of a specified E-kernel */
/*     segment. */

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

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   File handle. */
/*     SEGNO      I   Segment number. */
/*     RECNO      O   Number of appended record. */

/* $ Detailed_Input */

/*     HANDLE   is a file handle of an EK open for write access. */

/*     SEGNO    is the number of the segment to which the record */
/*              is to be added. */

/* $ Detailed_Output */

/*     RECNO    is the number of the record appended by this */
/*              routine. RECNO is used to identify the record */
/*              when writing column entries to it. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If HANDLE is invalid, an error is signaled by a routine in the */
/*         call tree of this routine. The file will not be modified. */

/*     2)  If SEGNO is out of range, the error SPICE(INVALIDINDEX) */
/*         is signaled. The file will not be modified. */

/*     3)  If an I/O error occurs while reading or writing the indicated */
/*         file, the error is signaled by a routine in the call tree of */
/*         this routine. The file may be corrupted. */

/* $ Files */

/*     See the EK Required Reading ek.req for a discussion of the EK file */
/*     format. */

/* $ Particulars */

/*     This routine operates by side effects: It appends a new, empty */
/*     record structure to an EK segment. The ordinal position of the */
/*     new record is one greater than the previous number of records in */
/*     in the segment. */

/*     After a new record has been appended to a segment by this routine, */
/*     the record must be populated with data using the EKACEx */
/*     routines.  EKs are valid only when all of their column entries */
/*     are initialized. */

/*     To insert a record into a segment at a specified ordinal position, */
/*     use the routine EKAPPR. */

/*     This routine cannot be used with the "fast write" suite of */
/*     routines. See the EK Required Reading ek.req for a discussion of */
/*     the fast writers. */

/*     When a record is inserted into an EK file that is not shadowed, */
/*     the status of the record starts out set to OLD. The status */
/*     does not change when data is added to the record. */

/*     If the target EK is shadowed, the new record will be given the */
/*     status NEW. Updating column values in the record does not change */
/*     its status. When changes are committed, the status is set to OLD. */
/*     If a rollback is performed before changes are committed, the */
/*     record is deleted. Closing the target file without committing */
/*     changes implies a rollback. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Append a record to a specified segment. */

/*        Suppose we have an E-kernel which contains records of orders */
/*        for data products. The E-kernel has a table called DATAORDERS */
/*        that consists of the set of columns listed below: */

/*           DATAORDERS */

/*              Column Name     Data Type */
/*              -----------     --------- */
/*              ORDER_ID        INTEGER */
/*              CUSTOMER_ID     INTEGER */
/*              LAST_NAME       CHARACTER*(*) */
/*              FIRST_NAME      CHARACTER*(*) */
/*              ORDER_DATE      TIME */
/*              COST            DOUBLE PRECISION */

/*        The order database also has a table of items that have been */
/*        ordered. The columns of this table are shown below: */

/*           DATAITEMS */

/*              Column Name     Data Type */
/*              -----------     --------- */
/*              ITEM_ID         INTEGER */
/*              ORDER_ID        INTEGER */
/*              ITEM_NAME       CHARACTER*(*) */
/*              DESCRIPTION     CHARACTER*(*) */
/*              PRICE           DOUBLE PRECISION */


/*        We'll suppose that the EK file contains two segments, the */
/*        first containing the DATAORDERS table and the second */
/*        containing the DATAITEMS table. */

/*        This examples creates such EK, with no records in either */
/*        table, and after re-opening the file, inserts a new record */
/*        into the DATAITEMS table. */


/*        Example code begins here. */


/*              PROGRAM EKAPPR_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Include the EK Column Name Size (CNAMSZ) */
/*        C */
/*              INCLUDE 'ekcnamsz.inc' */

/*        C */
/*        C     Local parameters */
/*        C */
/*              CHARACTER*(*)         EKNAME */
/*              PARAMETER           ( EKNAME  = 'ekappr_ex1.bdb' ) */

/*              INTEGER               DECLEN */
/*              PARAMETER           ( DECLEN = 200 ) */

/*              INTEGER               DESCLN */
/*              PARAMETER           ( DESCLN = 80  ) */

/*              INTEGER               NAMLEN */
/*              PARAMETER           ( NAMLEN = 40  ) */

/*              INTEGER               NCOLS */
/*              PARAMETER           ( NCOLS  = 6   ) */

/*        C */
/*        C     Local variables */
/*        C */
/*              CHARACTER*(DECLEN)    CDECLS ( NCOLS ) */
/*              CHARACTER*(CNAMSZ)    CNAMES ( NCOLS ) */
/*              CHARACTER*(DESCLN)    DESCRP */
/*              CHARACTER*(NAMLEN)    IFNAME */
/*              CHARACTER*(NAMLEN)    ITEMNM */

/*              DOUBLE PRECISION      PRICE */

/*              INTEGER               ESIZE */
/*              INTEGER               HANDLE */
/*              INTEGER               ITEMID */
/*              INTEGER               NRESVC */
/*              INTEGER               ORDID */
/*              INTEGER               RECNO */
/*              INTEGER               SEGNO */

/*              LOGICAL               ISNULL */

/*        C */
/*        C     Open a new EK file.  For simplicity, we will not */
/*        C     reserve any space for the comment area, so the */
/*        C     number of reserved comment characters is zero. */
/*        C     The variable IFNAME is the internal file name. */
/*        C */
/*              NRESVC  =  0 */
/*              IFNAME  =  'Test EK/Created 01-JUN-2019' */

/*              CALL EKOPN ( EKNAME, IFNAME, NRESVC, HANDLE ) */

/*        C */
/*        C     Set up the table and column names and declarations */
/*        C     for the DATAORDERS segment.  We'll index all of */
/*        C     the columns.  All columns are scalar, so we omit */
/*        C     the size declaration.  Only the COST column may take */
/*        C     null values. */
/*        C */
/*              CNAMES(1) =  'ORDER_ID' */
/*              CDECLS(1) =  'DATATYPE = INTEGER, INDEXED = TRUE' */

/*              CNAMES(2) =  'CUSTOMER_ID' */
/*              CDECLS(2) =  'DATATYPE = INTEGER, INDEXED = TRUE' */

/*              CNAMES(3) =  'LAST_NAME' */
/*              CDECLS(3) =  'DATATYPE = CHARACTER*(*), ' // */
/*             .             'INDEXED  = TRUE' */

/*              CNAMES(4) =  'FIRST_NAME' */
/*              CDECLS(4) =  'DATATYPE = CHARACTER*(*), ' // */
/*             .             'INDEXED  = TRUE' */

/*              CNAMES(5) =  'ORDER_DATE' */
/*              CDECLS(5) =  'DATATYPE = TIME, INDEXED  = TRUE' */

/*              CNAMES(6) =  'COST' */
/*              CDECLS(6) =  'DATATYPE = DOUBLE PRECISION,' // */
/*             .             'INDEXED  = TRUE,'             // */
/*             .             'NULLS_OK = TRUE' */

/*        C */
/*        C     Start the first segment. Since we have no data for this */
/*        C     segment, start the segment by just defining the new */
/*        C     segment's schema. */
/*        C */
/*              CALL EKBSEG ( HANDLE, 'DATAORDERS', NCOLS, */
/*             .              CNAMES, CDECLS,       SEGNO ) */

/*        C */
/*        C     At this point, the second segment could be */
/*        C     created by an analogous process.  In fact, the */
/*        C     second segment could be created at any time; it is */
/*        C     not necessary to populate the first segment with */
/*        C     data before starting the second segment. */
/*        C */
/*        C     Set up the table and column names and declarations */
/*        C     for the DATAITEMS segment.  We'll index all of */
/*        C     the columns.  All columns are scalar, so we omit */
/*        C     the size declaration. */
/*        C */
/*              CNAMES(1) =  'ITEM_ID' */
/*              CDECLS(1) =  'DATATYPE = INTEGER, INDEXED = TRUE' */

/*              CNAMES(2) =  'ORDER_ID' */
/*              CDECLS(2) =  'DATATYPE = INTEGER, INDEXED = TRUE' */

/*              CNAMES(3) =  'ITEM_NAME' */
/*              CDECLS(3) =  'DATATYPE = CHARACTER*(*),' // */
/*             .             'INDEXED  = TRUE' */

/*              CNAMES(4) =  'DESCRIPTION' */
/*              CDECLS(4) =  'DATATYPE = CHARACTER*(*),' // */
/*             .             'INDEXED  = TRUE' */

/*              CNAMES(5) =  'PRICE' */
/*              CDECLS(5) =  'DATATYPE = DOUBLE PRECISION,' // */
/*             .             'INDEXED  = TRUE' */


/*        C */
/*        C     Start the new segment. Since we have no data for this */
/*        C     segment, start the segment by just defining the new */
/*        C     segment's schema. */
/*        C */
/*              CALL EKBSEG ( HANDLE, 'DATAITEMS', 5, */
/*             .              CNAMES, CDECLS,      SEGNO ) */

/*        C */
/*        C     End the file by a call to EKCLS. */
/*        C */
/*              CALL EKCLS ( HANDLE ) */

/*        C */
/*        C     Now, we want to insert a new record into the DATAITEMS */
/*        C     table. */
/*        C */
/*        C     Open the database for write access.  This call is */
/*        C     made when the file already exists. */
/*        C */
/*              CALL EKOPW ( EKNAME, HANDLE ) */

/*        C */
/*        C     Append a new, empty record to the DATAITEMS */
/*        C     table. Recall that the DATAITEMS table */
/*        C     is in segment number 2.  The call will return */
/*        C     the number of the new, empty record. */
/*        C */
/*              SEGNO = 2 */
/*              CALL EKAPPR ( HANDLE, SEGNO, RECNO ) */

/*        C */
/*        C     At this point, the new record is empty.  A valid EK */
/*        C     cannot contain empty records.  We fill in the data */
/*        C     here.  Data items are filled in one column at a time. */
/*        C     The order in which the columns are filled in is not */
/*        C     important.  We use the EKACEx (add column entry) */
/*        C     routines to fill in column entries.  We'll assume */
/*        C     that no entries are null.  All entries are scalar, */
/*        C     so the entry size is 1. */
/*        C */
/*              ISNULL =  .FALSE. */
/*              ESIZE  =  1 */

/*        C */
/*        C     The following variables will contain the data for */
/*        C     the new record. */
/*        C */
/*              ORDID  =   10011 */
/*              ITEMID =   531 */
/*              ITEMNM =  'Sample item' */
/*              DESCRP =  'This sample item is used only in tests.' */
/*              PRICE  =   1345.678D0 */

/*        C */
/*        C     Note that the names of the routines called */
/*        C     correspond to the data types of the columns:  the */
/*        C     last letter of the routine name is C, I, or D, */
/*        C     depending on the data type. */
/*        C */
/*              CALL EKACEI ( HANDLE, SEGNO,  RECNO, 'ORDER_ID', */
/*             .              ESIZE,  ORDID,  ISNULL               ) */

/*              CALL EKACEI ( HANDLE, SEGNO,  RECNO, 'ITEM_ID', */
/*             .              ESIZE,  ITEMID, ISNULL               ) */

/*              CALL EKACEC ( HANDLE, SEGNO,  RECNO, 'ITEM_NAME', */
/*             .              ESIZE,  ITEMNM, ISNULL               ) */

/*              CALL EKACEC ( HANDLE, SEGNO,  RECNO, 'DESCRIPTION', */
/*             .              ESIZE,  DESCRP, ISNULL               ) */

/*              CALL EKACED ( HANDLE, SEGNO,  RECNO, 'PRICE', */
/*             .              ESIZE,  PRICE,  ISNULL               ) */

/*        C */
/*        C     Close the file to make the update permanent. */
/*        C */
/*              CALL EKCLS ( HANDLE ) */

/*              END */


/*        When this program is executed, no output is presented on */
/*        screen. After run completion, a new EK file exists in the */
/*        output directory. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 02-JUL-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */
/*        Added complete code example. */

/* -    SPICELIB Version 1.0.1, 09-JAN-2002 (NJB) */

/*        Documentation change: instances of the phrase "fast load" */
/*        were replaced with "fast write." */

/* -    SPICELIB Version 1.0.0, 19-DEC-1995 (NJB) */

/* -& */
/* $ Index_Entries */

/*     append record to EK segment */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("EKAPPR", (ftnlen)6);
    }

/*     Before trying to actually write anything, do every error */
/*     check we can. */

/*     Is this file handle valid--is the file open for paged write */
/*     access?  Signal an error if not. */

    zzekpgch_(handle, "WRITE", (ftnlen)5);
    if (failed_()) {
	chkout_("EKAPPR", (ftnlen)6);
	return 0;
    }

/*     Look up the integer metadata page and page base for the segment. */
/*     Given the base address, we can read the pertinent metadata in */
/*     one shot. */

    zzekmloc_(handle, segno, &mp, &mbase);
    if (failed_()) {
	chkout_("EKAPPR", (ftnlen)6);
	return 0;
    }
    i__1 = mbase + 1;
    i__2 = mbase + 24;
    dasrdi_(handle, &i__1, &i__2, segdsc);

/*     Obtain the number of records already present. */

    nrec = segdsc[5];

/*     Insert the new record at the end of the segment. */

    *recno = nrec + 1;
    ekinsr_(handle, segno, recno);
    chkout_("EKAPPR", (ftnlen)6);
    return 0;
} /* ekappr_ */

