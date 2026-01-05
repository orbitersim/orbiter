/* dskcls.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure DSKCLS ( DSK, close file ) */
/* Subroutine */ int dskcls_(integer *handle, logical *optmiz)
{
    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern logical failed_(void);
    extern /* Subroutine */ int dasham_(integer *, char *, ftnlen), dasllc_(
	    integer *), dascls_(integer *);
    char method[10];
    extern /* Subroutine */ int daswbr_(integer *), chkout_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Close a DSK file. */

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
/*     HANDLE     I   Handle assigned to the opened DSK file. */
/*     OPTMIZ     I   Flag indicating whether to segregate the DSK. */

/* $ Detailed_Input */

/*     HANDLE   is the DAS file handle associated with the file. */
/*              The file may be open for read or write access. */

/*     OPTMIZ   is a logical flag indicating whether the DSK */
/*              should be segregated before it is closed. This */
/*              option applies only to files open for write */
/*              access. The value of OPTMIZ has no effect for */
/*              files opened for read access. */

/*              See the DAS Required Reading das.req for a */
/*              discussion of segregation of DAS files. */

/* $ Detailed_Output */

/*     None. This routine operates by side effects. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If an error occurs when the file is closed, the error is */
/*         signaled by a routine in the call tree of this routine. */

/* $ Files */

/*     See argument HANDLE. */

/* $ Particulars */

/*     This routine provides a DSK-level interface for closing DSK files. */

/*     In cases where DSKs opened for write access are to be closed */
/*     without segregation, this interface is slightly simpler than that */
/*     available at the DAS level. */

/* $ Examples */

/*     1) Close a new DSK file using DAS segregation. HANDLE */
/*        is the DAS file handle of the DSK. */

/*        This is the normal choice for DSK creation. */

/*           CALL DSKCLS ( HANDLE, .TRUE. ) */

/*     2) Close a new DSK file without using DAS segregation. The */
/*        close operation will be fast, but reading the file will be */
/*        less efficient than if the file had been segregated. */

/*           CALL DSKCLS ( HANDLE, .TRUE. ) */

/*     3) Close an existing DSK file that had been opened */
/*        for read access. In this case OPTMIZ is ignored: */

/*           CALL DSKCLS ( HANDLE, .FALSE. ) */

/*        or */

/*           CALL DSKCLS ( HANDLE, .TRUE. ) */

/* $ Restrictions */

/*     1)  This routine should not be called by user applications */
/*         that have loaded a DSK file via FURNSH. Such applications */
/*         should call the KEEPER entry points UNLOAD or KCLEAR instead. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 12-OCT-2021 (JDR) (NJB) */

/*        Bug fix: now calls FAILED after call to DASHAM. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.0, 08-FEB-2017 (NJB) */


/*        09-OCT-2009 (NJB) */

/*           Updated header. */

/*        20-OCT-2006 (NJB) */

/*           Original DSKLIB version. */

/* -& */
/* $ Index_Entries */

/*     close a DSK file */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */

    if (return_()) {
	return 0;
    }
    chkin_("DSKCLS", (ftnlen)6);
    if (*optmiz) {

/*        Segregate the file to enable fast read access.  This is */
/*        the "normal" way to close a DSK.  Segregating a large file */
/*        can be slow, however. */

	dascls_(handle);
    } else {

/*        Close the file without first segregating it; this allows */
/*        the caller to close the file quickly, but results in a */
/*        file that will be read more slowly. */

/*        Any buffered data to be written must be explicitly flushed */
/*        to the file, if the file is open for write access. */

	dasham_(handle, method, (ftnlen)10);
	if (failed_()) {
	    chkout_("DSKCLS", (ftnlen)6);
	    return 0;
	}
	if (s_cmp(method, "WRITE ", (ftnlen)10, (ftnlen)6) == 0) {

/*           Write out any buffered records belonging to the */
/*           indicated file. */

	    daswbr_(handle);
	}

/*        Close the file without segregating records. */

	dasllc_(handle);
    }
    chkout_("DSKCLS", (ftnlen)6);
    return 0;
} /* dskcls_ */

