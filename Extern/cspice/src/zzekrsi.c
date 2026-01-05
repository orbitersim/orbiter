/* zzekrsi.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure   ZZEKRSI ( EK, read scalar, integer ) */
/* Subroutine */ int zzekrsi_(integer *handle, integer *segdsc, integer *
	coldsc, integer *recptr, integer *eltidx, integer *ival, logical *
	isnull, logical *found)
{
    extern integer zzekrp2n_(integer *, integer *, integer *);
    extern /* Subroutine */ int zzekcnam_(integer *, integer *, char *, 
	    ftnlen), chkin_(char *, ftnlen), errch_(char *, char *, ftnlen, 
	    ftnlen);
    integer class__, recno, segno, dtype;
    extern /* Subroutine */ int errhan_(char *, integer *, ftnlen);
    char column[32];
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen), sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), zzekrd01_(integer *, integer *, integer *, integer *, 
	    integer *, logical *), zzekrd04_(integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, logical *, logical *),
	     zzekrd07_(integer *, integer *, integer *, integer *, integer *, 
	    logical *);

/* $ Abstract */

/*     Read scalar data from an integer column in a specified EK record. */

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

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Handle attached to EK file. */
/*     SEGDSC     I   Segment descriptor. */
/*     COLDSC     I   Column descriptor. */
/*     RECPTR     I   Pointer to record from which data is to be read. */
/*     ELTIDX     I   Index of column entry element to be read. */
/*     IVAL       O   Integer value in column entry. */
/*     ISNULL     O   Flag indicating whether column entry is null. */
/*     FOUND      O   Flag indicting whether entry element was found. */

/* $ Detailed_Input */

/*     HANDLE         is an EK file handle. */

/*     SEGDSC         is the descriptor of the segment from which data is */
/*                    to be read. */

/*     COLDSC         is the column descriptor corresponding to the */
/*                    column from which to read. */

/*     RECPTR         is a pointer to the record from which data is to be */
/*                    read. */

/*     ELTIDX         is the index of the column entry element to read. */
/*                    If the column entry is scalar, this argument is */
/*                    ignored. */

/* $ Detailed_Output */

/*     IVAL           is the specified column entry.  IVAL is valid only */
/*                    when FOUND is set to .TRUE. */

/*     ISNULL         is a logical flag indicating whether the entry is */
/*                    null.  ISNULL is valid only when FOUND is set to */
/*                    .TRUE. */

/*     FOUND          is a logical flag indicating whether the specified */
/*                    column entry element was found.  For vector-valued */
/*                    columns, if ELTIDX refers to a non-existent */
/*                    column entry element, FOUND is set to .FALSE. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If HANDLE is invalid, the error will be diagnosed by routines */
/*         called by this routine. */

/*     2)  If COLDSC is not the descriptor of a declared column, the */
/*         error will be diagnosed by routines called by this routine. */

/*     3)  If COLDSC specifies a column of whose data type is not */
/*         integer, the error SPICE(WRONGDATATYPE) will be signaled. */

/*     4)  If COLDSC specifies a column of whose class is not */
/*         an integer class known to this routine, the error */
/*         SPICE(NOCLASS) will be signaled. */

/*     5)  If the indicated column is array-valued, and if ELTIDX is */
/*         non-positive, the error will be diagnosed by routines called */
/*         by this routine.  However, if ELTIDX is greater than the */
/*         number of elements in the specified column entry, FOUND is */
/*         set to .FALSE. and no error is signaled. */

/*     6)  If an I/O error occurs while reading the indicated file, */
/*         the error will be diagnosed by routines called by this */
/*         routine. */

/* $ Files */

/*     See the EK Required Reading for a discussion of the EK file */
/*     format. */

/* $ Particulars */

/*     The ZZEKRSx routines are low-level readers that expect column */
/*     entries to be defined by descriptors.  Since these routines do not */
/*     look up descriptors, in cases where many successive accesses to */
/*     the same segment and column are required, these routines are */
/*     considerably more efficient than the high-level readers. */

/*     These routines do not participate in tracing. */

/* $ Examples */

/*     See ZZEKECMP. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 06-FEB-2015 (NJB) */

/*        Now uses ERRHAN to insert DAS file name into */
/*        long error messages. */

/* -    Beta Version 1.0.0, 06-NOV-1995 (NJB) */

/* -& */

/*     Non-SPICELIB functions */


/*     Local variables */


/*     Use discovery check-in. */

/*     Nothing found to begin with. */

    *found = FALSE_;

/*     This column had better be of integer type. */

    dtype = coldsc[1];
    if (dtype != 3) {
	zzekcnam_(handle, coldsc, column, (ftnlen)32);
	segno = segdsc[1];
	recno = zzekrp2n_(handle, &segdsc[1], recptr);
	chkin_("ZZEKRSI", (ftnlen)7);
	setmsg_("Column # is of type #; ZZEKRSI only works with integer colu"
		"mns.  RECNO = #; SEGNO = #; EK = #.", (ftnlen)94);
	errch_("#", column, (ftnlen)1, (ftnlen)32);
	errint_("#", &dtype, (ftnlen)1);
	errint_("#", &recno, (ftnlen)1);
	errint_("#", &segno, (ftnlen)1);
	errhan_("#", handle, (ftnlen)1);
	sigerr_("SPICE(WRONGDATATYPE)", (ftnlen)20);
	chkout_("ZZEKRSI", (ftnlen)7);
	return 0;
    }

/*     Now it's time to read data from the file.  Call the low-level */
/*     reader appropriate to the column's class. */

    class__ = coldsc[0];
    if (class__ == 1) {

/*        Class 1 columns contain scalar, integer entries. */

	zzekrd01_(handle, segdsc, coldsc, recptr, ival, isnull);
	*found = TRUE_;
    } else if (class__ == 4) {

/*        Class 4 columns contain integer array entries. */

	zzekrd04_(handle, segdsc, coldsc, recptr, eltidx, eltidx, ival, 
		isnull, found);
    } else if (class__ == 7) {

/*        Class 7 columns are fixed-count columns that contain scalar, */
/*        integer entries. */

	zzekrd07_(handle, segdsc, coldsc, recptr, ival, isnull);
	*found = TRUE_;
    } else {

/*        This is an unsupported integer column class. */

	zzekcnam_(handle, coldsc, column, (ftnlen)32);
	segno = segdsc[1];
	recno = zzekrp2n_(handle, &segdsc[1], recptr);
	chkin_("ZZEKRSI", (ftnlen)7);
	setmsg_("Class # from input column descriptor is not a supported int"
		"eger class.  COLUMN = #; RECNO = #; SEGNO = #; EK = #.", (
		ftnlen)113);
	errint_("#", &class__, (ftnlen)1);
	errch_("#", column, (ftnlen)1, (ftnlen)32);
	errint_("#", &recno, (ftnlen)1);
	errint_("#", &segno, (ftnlen)1);
	errhan_("#", handle, (ftnlen)1);
	sigerr_("SPICE(NOCLASS)", (ftnlen)14);
	chkout_("ZZEKRSI", (ftnlen)7);
	return 0;
    }
    return 0;
} /* zzekrsi_ */

