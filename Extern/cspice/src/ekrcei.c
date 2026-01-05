/* ekrcei.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* $Procedure EKRCEI ( EK, read column entry element, integer ) */
/* Subroutine */ int ekrcei_(integer *handle, integer *segno, integer *recno, 
	char *column, integer *nvals, integer *ivals, logical *isnull, ftnlen 
	column_len)
{
    extern /* Subroutine */ int zzekcdsc_(integer *, integer *, char *, 
	    integer *, ftnlen), zzeksdsc_(integer *, integer *, integer *), 
	    zzektrdp_(integer *, integer *, integer *, integer *);
    extern integer zzekesiz_(integer *, integer *, integer *, integer *);
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen);
    integer class__;
    logical found;
    integer dtype;
    extern logical failed_(void);
    integer coldsc[11], segdsc[24];
    extern /* Subroutine */ int errhan_(char *, integer *, ftnlen);
    integer recptr;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen), sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), zzekrd01_(integer *, integer *, integer *, integer *, 
	    integer *, logical *), zzekrd04_(integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, logical *, logical *),
	     zzekrd07_(integer *, integer *, integer *, integer *, integer *, 
	    logical *);

/* $ Abstract */

/*     Read data from an integer column in a specified EK record. */

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
/*     FILES */
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
/*     HANDLE     I   Handle attached to EK file. */
/*     SEGNO      I   Index of segment containing record. */
/*     RECNO      I   Record from which data is to be read. */
/*     COLUMN     I   Column name. */
/*     NVALS      O   Number of values in column entry. */
/*     IVALS      O   Integer values in column entry. */
/*     ISNULL     O   Flag indicating whether column entry is null. */

/* $ Detailed_Input */

/*     HANDLE   is an EK file handle. The file may be open for read or */
/*              write access. */

/*     SEGNO    is the index of the segment from which data is to be */
/*              read. */

/*     RECNO    is the index of the record from which data is to be read. */
/*              This record number is relative to the start of the */
/*              segment indicated by SEGNO; the first record in the */
/*              segment has index 1. */

/*     COLUMN   is the name of the column from which data is to be read. */

/* $ Detailed_Output */

/*     NVALS, */
/*     IVALS    are, respectively, the number of values found in the */
/*              specified column entry and the set of values themselves. */

/*              For columns having fixed-size entries, when a a column */
/*              entry is null, NVALS is still set to the column entry */
/*              size. For columns having variable-size entries, NVALS is */
/*              set to 1 for null entries. */

/*     ISNULL   is a logical flag indicating whether the returned column */
/*              entry is null. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If HANDLE is invalid, an error is signaled by a routine in the */
/*         call tree of this routine. */

/*     2)  If SEGNO is out of range, an error is signaled by a routine in */
/*         the call tree of this routine. */

/*     3)  If RECNO is out of range, an error is signaled by a routine in */
/*         the call tree of this routine. */

/*     4)  If COLUMN is not the name of a declared column, an error */
/*         is signaled by a routine in the call tree of this routine. */

/*     5)  If COLUMN specifies a column of whose data type is not */
/*         integer, the error SPICE(WRONGDATATYPE) is signaled. */

/*     6)  If COLUMN specifies a column of whose class is not */
/*         an integer class known to this routine, the error */
/*         SPICE(NOCLASS) is signaled. */

/*     7)  If an attempt is made to read an uninitialized column entry, */
/*         an error is signaled by a routine in the call tree of this */
/*         routine. A null entry is considered to be initialized, but */
/*         entries do not contain null values by default. */

/*     8)  If an I/O error occurs while reading the indicated file, the */
/*         error is signaled by a routine in the call tree of this */
/*         routine. */

/* $ Files */

/*     See the EK Required Reading ek.req for a discussion of the EK file */
/*     format. */

/* $ Particulars */

/*     This routine is a utility that allows an EK file to be read */
/*     directly without using the high-level query interface. */

/* $ Examples */

/*     1)  Read the value in the third record of the column ICOL in */
/*         the fifth segment of an EK file designated by HANDLE. */

/*            CALL EKRCEI ( HANDLE, 5, 3, 'ICOL', N, IVAL, ISNULL ) */

/* $ Restrictions */

/*     1)  EK files open for write access are not necessarily readable. */
/*         In particular, a column entry can be read only if it has been */
/*         initialized. The caller is responsible for determining */
/*         when it is safe to read from files open for write access. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.3.1, 26-MAY-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.3.0, 06-FEB-2015 (NJB) */

/*        Now uses ERRHAN to insert DAS file name into */
/*        long error messages. */

/* -    SPICELIB Version 1.2.0, 20-JUN-1999 (WLT) */

/*        Removed unbalanced call to CHKOUT. */

/* -    SPICELIB Version 1.1.0, 28-JUL-1997 (NJB) */

/*        Bug fix: Record number, not record pointer, is now supplied */
/*        to look up data in the class 7 case. Miscellaneous header */
/*        changes were made as well. */

/* -    SPICELIB Version 1.0.0, 06-NOV-1995 (NJB) */

/* -& */
/* $ Index_Entries */

/*     read integer data from EK column */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.1.0, 28-JUL-1997 (NJB) */

/*        Bug fix: Record number, not record pointer, is now supplied */
/*        to look up data in the class 7 case. For class 7 columns, */
/*        column entry locations are calculated directly from record */
/*        numbers; no indirection is used. */

/*        Miscellaneous header changes were made as well. */

/* -& */

/*     SPICELIB functions */


/*     Non-SPICELIB functions */


/*     Local variables */


/*     Use discovery check-in. */

/*     First step:  find the descriptor for the named segment.  Using */
/*     this descriptor, get the column descriptor. */

    zzeksdsc_(handle, segno, segdsc);
    zzekcdsc_(handle, segdsc, column, coldsc, column_len);
    if (failed_()) {
	return 0;
    }

/*     This column had better be of integer type. */

    dtype = coldsc[1];
    if (dtype != 3) {
	chkin_("EKRCEI", (ftnlen)6);
	setmsg_("Column # is of type #; EKRCEI only works with integer colum"
		"ns.  RECNO = #; SEGNO = #; EK = #.", (ftnlen)93);
	errch_("#", column, (ftnlen)1, column_len);
	errint_("#", &dtype, (ftnlen)1);
	errint_("#", recno, (ftnlen)1);
	errint_("#", segno, (ftnlen)1);
	errhan_("#", handle, (ftnlen)1);
	sigerr_("SPICE(WRONGDATATYPE)", (ftnlen)20);
	chkout_("EKRCEI", (ftnlen)6);
	return 0;
    }

/*     Now it's time to read data from the file.  Call the low-level */
/*     reader appropriate to the column's class. */

    class__ = coldsc[0];
    if (class__ == 1) {

/*        Look up the record pointer for the target record. */

	zzektrdp_(handle, &segdsc[6], recno, &recptr);
	zzekrd01_(handle, segdsc, coldsc, &recptr, ivals, isnull);
	*nvals = 1;
    } else if (class__ == 4) {
	zzektrdp_(handle, &segdsc[6], recno, &recptr);
	*nvals = zzekesiz_(handle, segdsc, coldsc, &recptr);
	zzekrd04_(handle, segdsc, coldsc, &recptr, &c__1, nvals, ivals, 
		isnull, &found);
    } else if (class__ == 7) {

/*        Records in class 7 columns are identified by a record number */
/*        rather than a pointer. */

	zzekrd07_(handle, segdsc, coldsc, recno, ivals, isnull);
	*nvals = 1;
    } else {

/*        This is an unsupported integer column class. */

	*segno = segdsc[1];
	chkin_("EKRCEI", (ftnlen)6);
	setmsg_("Class # from input column descriptor is not a supported int"
		"eger class.  COLUMN = #; RECNO = #; SEGNO = #; EK = #.", (
		ftnlen)113);
	errint_("#", &class__, (ftnlen)1);
	errch_("#", column, (ftnlen)1, column_len);
	errint_("#", recno, (ftnlen)1);
	errint_("#", segno, (ftnlen)1);
	errhan_("#", handle, (ftnlen)1);
	sigerr_("SPICE(NOCLASS)", (ftnlen)14);
	chkout_("EKRCEI", (ftnlen)6);
	return 0;
    }
    return 0;
} /* ekrcei_ */

