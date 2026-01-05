/* ekssum.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure EKSSUM ( EK, return segment summary ) */
/* Subroutine */ int ekssum_(integer *handle, integer *segno, char *tabnam, 
	integer *nrows, integer *ncols, char *cnames, char *dtypes, integer *
	sizes, integer *strlns, logical *indexd, logical *nullok, ftnlen 
	tabnam_len, ftnlen cnames_len, ftnlen dtypes_len)
{
    /* Initialized data */

    static char typstr[4*4] = "CHR " "DP  " "INT " "TIME";

    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int zzeksinf_(integer *, integer *, char *, 
	    integer *, char *, integer *, ftnlen, ftnlen);
    integer i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern logical failed_(void);
    integer segdsc[24], cdscrs[1100]	/* was [11][100] */;
    extern logical return_(void);
    extern /* Subroutine */ int chkout_(char *, ftnlen);

/* $ Abstract */

/*     Return summary information for a specified segment in a */
/*     specified EK. */

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
/*     HANDLE     I   Handle of EK. */
/*     SEGNO      I   Number of segment to be summarized. */
/*     TABNAM     O   Name of table containing segment. */
/*     NROWS      O   Number of rows in segment. */
/*     NCOLS      O   Number of columns in segment. */
/*     CNAMES     O   Names of columns in segment. */
/*     DTYPES     O   Data types of columns in segment. */
/*     SIZES      O   Entry sizes of columns in segment. */
/*     STRLNS     O   String lengths of columns in segment. */
/*     INDEXD     O   Flags indicating whether columns are indexed. */
/*     NULLOK     O   Flags indicating whether columns allow nulls. */

/* $ Detailed_Input */

/*     HANDLE   is an EK file handle specifying the EK containing */
/*              the segment to be summarized. */

/*     SEGNO    is the number of the segment whose summary is */
/*              desired. Segments are numbered from 1 to NSEG, */
/*              where NSEG is the count of segments in the file. */

/* $ Detailed_Output */

/*     TABNAM   is the name of the table to which the segment */
/*              belongs. */

/*     NROWS    is the number of rows in the segment. */

/*     NCOLS    is the number of columns in the segment. The */
/*              maximum number of columns in a segment is given */
/*              by the parameter MXCLSG, which is defined in the */
/*              include file */

/*                 ekglimit.inc. */

/*              Currently, this limit is set at 100 columns. */

/*     CNAMES   is an array of names of columns in the segment. */

/*     DTYPES   is an array of data types of columns in the */
/*              segment. Each data type is indicated by a short */
/*              character string. The strings and their meanings */
/*              are: */

/*                 'CHR'       Character type. */
/*                 'DP'        Double precision type. */
/*                 'INT'       Integer type. */
/*                 'TIME'      Time type. */

/*              The Ith element of DTYPES corresponds to the */
/*              column whose name is the Ith element of CNAMES. */

/*     SIZES    is an array of declared sizes of column entries. */
/*              The Ith element of SIZES is the declared size of */
/*              the column whose name is the Ith element of CNAMES. */
/*              Scalar-valued columns have size 1; fixed-size, */
/*              array-valued columns have size greater than 1. */
/*              Array valued columns of variable size have a size */
/*              value of -1. */

/*     STRLNS   is an array of declared string lengths of */
/*              character column entries. These lengths are */
/*              defined only for columns of character type. */
/*              The Ith element of SIZES is the declared size of */
/*              the column whose name is the Ith element of CNAMES, */
/*              if that column has character type; otherwise, the */
/*              Ith element of STRLNS is undefined. For */
/*              character columns having variable string length, */
/*              the returned value of STRLNS is -1. */

/*     INDEXD   is an array of logical flags indicating whether the */
/*              corresponding columns are indexed. The Ith element */
/*              of INDEXD applies to the column whose name is the */
/*              Ith element of CNAMES. */

/*     NULLOK   is an array of logical flags indicating whether the */
/*              corresponding columns allow null values. The Ith */
/*              element of NULLOK applies to the column whose name */
/*              is the Ith element of CNAMES. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If HANDLE is invalid, an error is signaled by a routine in the */
/*         call tree of this routine. The output arguments will not be */
/*         modified. */

/*     2)  If SEGNO is not the index of an existing segment in the */
/*         specified file, the error SPICE(INDEXOUTOFRANGE) is signaled. */
/*         The output arguments will not be modified. */

/*     3)  If an I/O error occurs while attempting to obtain summary */
/*         information for the specified segment, the error is signaled */
/*         by a routine in the call tree of this routine. The output */
/*         arguments may be modified in this case. */

/* $ Files */

/*     See the description of HANDLE in $Detailed_Input. */

/* $ Particulars */

/*     This routine supports the function of summarizing a binary */
/*     EK file, allowing NAIF Toolkit users to determine whether it */
/*     contains data of interest. The routine also also provides */
/*     address information necessary to retrieve information from the */
/*     segment. */

/* $ Examples */

/*     1)  Dump the table and column names of the segments in an EK. */

/*            C */
/*            C     Open the EK for read access and get the number of */
/*            C     segments it contains. */
/*            C */
/*                  CALL EKOPR ( EKNAME, HANDLE ) */

/*                  NSEG = EKNSEG ( HANDLE ) */

/*            C */
/*            C     Loop through the segments, dumping the desired */
/*            C     summary information for each one. */
/*            C */
/*                  WRITE (*,*) ' ' */
/*                  WRITE (*,*) ' ' */
/*                  WRITE (*,*) 'Segment summary for file ', EKNAME */
/*                  WRITE (*,*) ' ' */
/*                  WRITE (*,*) ' ' */

/*                  DO I = 1, NSEG */

/*                     CALL EKSSUM (  HANDLE,  SEGNO,   TABNAM,  NROWS, */
/*                 .                  NCOLS,   CNAMES,  DTYPES,  SIZES, */
/*                 .                  STRLNS,  INDEXD,  NULLOK         ) */

/*                     WRITE (*,*) */
/*                 .   '========================================'      // */
/*                 .   '========================================' */


/*                     WRITE (*,*) 'Table containing segment: ', TABNAM */

/*                     WRITE (*,*) ' ' */
/*                     WRITE (*,*) 'Number of rows:     ', NROWS */
/*                     WRITE (*,*) 'Number of columns:  ', NCOLS */
/*                     WRITE (*,*) ' ' */
/*                     WRITE (*,*) 'Column names and attributes: ' */
/*                     WRITE (*,*) ' ' */

/*                     DO J = 1, NCOLS */

/*                        WRITE (*,*) 'Column:   '//CNAMES(J) */
/*                        WRITE (*,*) ' ' */
/*                        WRITE (*,*) 'Data type: ', DTYPES(J) */
/*                        WRITE (*,*) 'Dimension: ', SIZES(J) */

/*                        IF ( DTYPES(J) .EQ. 'CHR' ) THEN */
/*                           WRITE (*,*) 'String length: ', STRLNS(J) */
/*                        END IF */

/*                        IF ( INDEXD(J) ) THEN */
/*                           WRITE (*,*) 'Indexed' */
/*                        END IF */

/*                        IF ( NULLOK(J) ) THEN */
/*                           WRITE (*,*) 'Nulls allowed' */
/*                        ELSE */
/*                           WRITE (*,*) 'Nulls not allowed' */
/*                        END IF */

/*                        WRITE (*,*) ' ' */
/*                     END DO */

/*                     WRITE (*,*) */
/*                 .   '========================================'      // */
/*                 .   '========================================' */

/*                  END DO */

/*                  END */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */

/* $ Version */

/* -    SPICELIB Version 1.2.0, 02-JUL-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Fixed previous */
/*        version number. */

/* -    SPICELIB Version 1.1.0, 07-JUL-1996 (NJB) */

/*        Bug fix: correct parameter is now used to set dimension */
/*        of local variable SEGDSC. */

/* -    SPICELIB Version 1.0.0, 26-SEP-1995 (NJB) */

/* -& */
/* $ Index_Entries */

/*     return EK segment summary */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.1.0, 07-JUL-1996 (NJB) */

/*        Bug fix: correct parameter SDSCSZ is now used to set dimension */
/*        of local variable SEGDSC. Previously, the parameter */
/*        CDSCSZ had been used. */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Saved variables */


/*     Initial values */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("EKSSUM", (ftnlen)6);
    }

/*     Get the info from a knowledgeable source. */

    zzeksinf_(handle, segno, tabnam, segdsc, cnames, cdscrs, tabnam_len, 
	    cnames_len);
    if (failed_()) {
	chkout_("EKSSUM", (ftnlen)6);
	return 0;
    }
    *nrows = segdsc[5];
    *ncols = segdsc[4];
    i__1 = *ncols;
    for (i__ = 1; i__ <= i__1; ++i__) {
	s_copy(dtypes + (i__ - 1) * dtypes_len, typstr + (((i__3 = cdscrs[(
		i__2 = i__ * 11 - 10) < 1100 && 0 <= i__2 ? i__2 : s_rnge(
		"cdscrs", i__2, "ekssum_", (ftnlen)366)] - 1) < 4 && 0 <= 
		i__3 ? i__3 : s_rnge("typstr", i__3, "ekssum_", (ftnlen)366)) 
		<< 2), dtypes_len, (ftnlen)4);
	sizes[i__ - 1] = cdscrs[(i__2 = i__ * 11 - 8) < 1100 && 0 <= i__2 ? 
		i__2 : s_rnge("cdscrs", i__2, "ekssum_", (ftnlen)368)];
	if (cdscrs[(i__2 = i__ * 11 - 10) < 1100 && 0 <= i__2 ? i__2 : s_rnge(
		"cdscrs", i__2, "ekssum_", (ftnlen)370)] == 1) {
	    strlns[i__ - 1] = cdscrs[(i__2 = i__ * 11 - 9) < 1100 && 0 <= 
		    i__2 ? i__2 : s_rnge("cdscrs", i__2, "ekssum_", (ftnlen)
		    371)];
	} else {
	    strlns[i__ - 1] = 0;
	}
	indexd[i__ - 1] = cdscrs[(i__2 = i__ * 11 - 6) < 1100 && 0 <= i__2 ? 
		i__2 : s_rnge("cdscrs", i__2, "ekssum_", (ftnlen)376)] != -1;
	nullok[i__ - 1] = cdscrs[(i__2 = i__ * 11 - 4) < 1100 && 0 <= i__2 ? 
		i__2 : s_rnge("cdscrs", i__2, "ekssum_", (ftnlen)377)] != -1;
    }
    chkout_("EKSSUM", (ftnlen)6);
    return 0;
} /* ekssum_ */

