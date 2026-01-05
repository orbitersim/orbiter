/* dskopn.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure DSKOPN ( DSK, open new file ) */
/* Subroutine */ int dskopn_(char *fname, char *ifname, integer *ncomch, 
	integer *handle, ftnlen fname_len, ftnlen ifname_len)
{
    extern /* Subroutine */ int chkin_(char *, ftnlen), dlaopn_(char *, char *
	    , char *, integer *, integer *, ftnlen, ftnlen, ftnlen), chkout_(
	    char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Open a new DSK file for subsequent write operations. */

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
/*     DSK */

/* $ Keywords */

/*     DAS */
/*     DSK */
/*     FILES */

/* $ Declarations */

/*     Include file dla.inc */

/*     This include file declares parameters for DLA format */
/*     version zero. */

/*        Version 3.0.1 17-OCT-2016 (NJB) */

/*           Corrected comment: VERIDX is now described as a DAS */
/*           integer address rather than a d.p. address. */

/*        Version 3.0.0 20-JUN-2006 (NJB) */

/*           Changed name of parameter DSCSIZ to DLADSZ. */

/*        Version 2.0.0 09-FEB-2005 (NJB) */

/*           Changed descriptor layout to make backward pointer */
/*           first element.  Updated DLA format version code to 1. */

/*           Added parameters for format version and number of bytes per */
/*           DAS comment record. */

/*        Version 1.0.0 28-JAN-2004 (NJB) */


/*     DAS integer address of DLA version code. */


/*     Linked list parameters */

/*     Logical arrays (aka "segments") in a DAS linked array (DLA) file */
/*     are organized as a doubly linked list.  Each logical array may */
/*     actually consist of character, double precision, and integer */
/*     components.  A component of a given data type occupies a */
/*     contiguous range of DAS addresses of that type.  Any or all */
/*     array components may be empty. */

/*     The segment descriptors in a SPICE DLA (DAS linked array) file */
/*     are connected by a doubly linked list.  Each node of the list is */
/*     represented by a pair of integers acting as forward and backward */
/*     pointers.  Each pointer pair occupies the first two integers of a */
/*     segment descriptor in DAS integer address space.  The DLA file */
/*     contains pointers to the first integers of both the first and */
/*     last segment descriptors. */

/*     At the DLA level of a file format implementation, there is */
/*     no knowledge of the data contents.  Hence segment descriptors */
/*     provide information only about file layout (in contrast with */
/*     the DAF system).  Metadata giving specifics of segment contents */
/*     are stored within the segments themselves in DLA-based file */
/*     formats. */


/*     Parameter declarations follow. */

/*     DAS integer addresses of first and last segment linked list */
/*     pointer pairs.  The contents of these pointers */
/*     are the DAS addresses of the first integers belonging */
/*     to the first and last link pairs, respectively. */

/*     The acronyms "LLB" and "LLE" denote "linked list begin" */
/*     and "linked list end" respectively. */


/*     Null pointer parameter. */


/*     Segment descriptor parameters */

/*     Each segment descriptor occupies a contiguous */
/*     range of DAS integer addresses. */

/*        The segment descriptor layout is: */

/*           +---------------+ */
/*           | BACKWARD PTR  | Linked list backward pointer */
/*           +---------------+ */
/*           | FORWARD PTR   | Linked list forward pointer */
/*           +---------------+ */
/*           | BASE INT ADDR | Base DAS integer address */
/*           +---------------+ */
/*           | INT COMP SIZE | Size of integer segment component */
/*           +---------------+ */
/*           | BASE DP ADDR  | Base DAS d.p. address */
/*           +---------------+ */
/*           | DP COMP SIZE  | Size of d.p. segment component */
/*           +---------------+ */
/*           | BASE CHR ADDR | Base DAS character address */
/*           +---------------+ */
/*           | CHR COMP SIZE | Size of character segment component */
/*           +---------------+ */

/*     Parameters defining offsets for segment descriptor elements */
/*     follow. */


/*     Descriptor size: */


/*     Other DLA parameters: */


/*     DLA format version.  (This number is expected to occur very */
/*     rarely at integer address VERIDX in uninitialized DLA files.) */


/*     Characters per DAS comment record. */


/*     End of include file dla.inc */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     FNAME      I   Name of a DSK file to be opened. */
/*     IFNAME     I   Internal file name. */
/*     NCOMCH     I   Number of comment characters to allocate. */
/*     HANDLE     O   Handle assigned to the opened DSK file. */

/* $ Detailed_Input */

/*     FNAME    is the name of a new DSK file to be created. The */
/*              file will be left opened for write access. */

/*     IFNAME   is the internal file name for the new file. The name */
/*              may contain as many as 60 characters. All characters */
/*              of IFNAME should be printing characters (ASCII codes */
/*              32-126 decimal). This name should uniquely identify */
/*              the file. */

/*     NCOMCH   is the number of comment characters to allocate. */
/*              Allocating comment characters at file creation time */
/*              may reduce the likelihood of having to expand the */
/*              comment area later. */

/* $ Detailed_Output */

/*     HANDLE   is the file handle associated with the file. This */
/*              handle is used to identify the file in subsequent */
/*              calls to other DSK routines. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the input filename is blank, an error is signaled by a */
/*         routine in the call tree of this routine. No file will be */
/*         created. */

/*     2)  If the specified file cannot be opened without exceeding the */
/*         maximum allowed number of open DAS files, an error is signaled */
/*         by a routine in the call tree of this routine. No file will be */
/*         created. */

/*     3)  If the file cannot be opened properly, an error is signaled by */
/*         a routine in the call tree of this routine. No file will be */
/*         created. */

/*     4)  If the initial records in the file cannot be written, an error */
/*         is signaled by a routine in the call tree of this routine. No */
/*         file will be created. */

/*     5)  If no logical units are available, an error is signaled by a */
/*         routine in the call tree of this routine. No file will be */
/*         created. */

/*     6)  If the internal file name contains nonprinting characters */
/*         (ASCII codes decimal 0-31 and 127-255), an error is signaled */
/*         by a routine in the call tree of this routine. No file will be */
/*         created. */

/*     7)  If the number of comment characters allocated NCOMCH is */
/*         negative, an error is signaled by a routine in the call */
/*         tree of this routine. No file will be created. */

/* $ Files */

/*     See argument FNAME. */

/* $ Particulars */

/*     DSK files are built using the DLA low-level format and */
/*     the DAS architecture; DLA files are a specialized type of DAS */
/*     file in which data are organized as a doubly linked list of */
/*     segments. Each segment's data belong to contiguous components of */
/*     character, double precision, and integer type. */

/*     This routine creates a new DSK file and sets the type of the */
/*     file to the mnemonic code passed to it. */

/*     DSK files created by this routine have initialized file records. */
/*     The ID word in a DSK file record has the form */

/*        DAS/DSK */

/*     where the characters following the slash are supplied by the */
/*     caller of this routine. */

/* $ Examples */

/*     1)  Create a new DSK file, using an internal file name that */
/*         attempts to serve as an unique identifier. No room for */
/*         comments will be reserved. */

/*            FNAME  =  'TEST.DSK' */
/*            IFNAME =  'TEST.DSK/NAIF/NJB/20-OCT-2006/14:37:00' */
/*            NCOMCH =   0 */

/*            CALL DSKOPN ( FNAME, IFNAME, NCOMCH, HANDLE ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 02-JUL-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.0, 08-FEB-2017 (NJB) */

/*        Corrected a few header typos. */

/*        29-APR-2010 (NJB) */

/*           Now passes NCOMCH to DLAOPN. */

/*        08-OCT-2009 (NJB) */

/*           Updated header. */

/*        20-OCT-2006 (NJB) */

/* -& */
/* $ Index_Entries */

/*     open a new DSK file */
/*     open a new DSK file with write access */

/* -& */

/*     SPICELIB functions */

    if (return_()) {
	return 0;
    }
    chkin_("DSKOPN", (ftnlen)6);
    dlaopn_(fname, "DSK", ifname, ncomch, handle, fname_len, (ftnlen)3, 
	    ifname_len);
    chkout_("DSKOPN", (ftnlen)6);
    return 0;
} /* dskopn_ */

