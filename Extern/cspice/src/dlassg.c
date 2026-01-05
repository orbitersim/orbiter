/* dlassg.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure DLASSG ( DLA, same segment? ) */
logical dlassg_(integer *han1, integer *han2, integer *dsc1, integer *dsc2)
{
    /* System generated locals */
    integer i__1, i__2;
    logical ret_val;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer i__;

/* $ Abstract */

/*     Return a logical value indicating whether a two DLA */
/*     segments, each identified by DAS handle and DLA descriptor, */
/*     are in fact the same segment. */

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
/*     DLA */

/* $ Keywords */

/*     DAS */
/*     DLA */
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
/*     HAN1       I   Handle of an open DLA file. */
/*     HAN2       I   Handle of a second open DLA file. */
/*     DSC1       I   DLA descriptor of a segment in the first file. */
/*     DSC2       I   DLA descriptor of a segment in the second file. */

/*     The function returns .TRUE. if and only if the DLA segments */
/*     match. */

/* $ Detailed_Input */

/*     HAN1     is the integer handle associated with a DLA file. */
/*              The file is open for read access. */

/*     HAN2     is the integer handle associated with a second DLA */
/*              file. The file is open for read access. */

/*     DSC1     is the DLA descriptor of a segment in the file */
/*              associated with HAN1. */

/*     DSC2     is the DLA descriptor of a segment in the file */
/*              associated with HAN2. */

/* $ Detailed_Output */

/*     The function returns .TRUE. if and only if the DLA segments */
/*     match. The segments are considered to match if and only if the */
/*     input handles match and all elements of the DLA descriptors */
/*     match. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If any of the inputs are invalid, this routine will */
/*         fail in an unspecified manner. */

/* $ Files */

/*     See description of input arguments HAN1 and HAN2. */

/* $ Particulars */

/*     DLA files are built using the DAS low-level format; DLA files are */
/*     a specialized type of DAS file in which data are organized as a */
/*     doubly linked list of segments. Each segment's data belong to */
/*     contiguous components of character, double precision, and integer */
/*     type. */

/*     This routine supports DLA and DSK routines by enabling */
/*     them to determine whether a given DLA segment matches one */
/*     they've previously examined. This may allow such routines */
/*     to avoid buffering information redundantly. */

/* $ Examples */

/*     1)  A typical use of this routine is to enable a subroutine */
/*         to determine whether a DLA segment identified by a */
/*         handle and DLA descriptor matches one seen previously. */
/*         The logic of such a test can be implemented as follows: */


/*                   SUBROUTINE SUBA ( HANDLE, DLADSC ) */
/*                   IMPLICIT NONE */

/*                   INCLUDE 'dla.inc' */

/*                   INTEGER               HANDLE */
/*                   INTEGER               DLADSC ( * ) */

/*             C */
/*             C     SPICELIB functions */
/*             C */
/*                   LOGICAL               DLASSG */
/*                   LOGICAL               FAILED */
/*             C */
/*             C     Local variables */
/*             C */
/*                   INTEGER               PRVDSC ( DLADSZ ) */
/*                   INTEGER               PRVHAN */

/*             C */
/*             C     Saved variables */
/*             C */
/*                   SAVE                  PRVDSC */
/*                   SAVE                  PRVHAN */

/*             C */
/*             C     Initial values */
/*             C */
/*                   DATA                  PRVHAN / 0 / */

/*                   ... */

/*                   IF ( .NOT. DLASSG( HANDLE, PRVHAN, */
/*                  .                   DLADSC, PRVDSC ) ) THEN */

/*                      [Examine segment] */

/*                      IF ( .NOT. FAILED() ) THEN */
/*             C */
/*             C           Save values only if no error occurred. */
/*             C */
/*                         CALL MOVEI ( DLADSC, DLADSZ, PRVDSC ) */
/*                         PRVHAN = HANDLE */

/*                      END IF */

/*                   END IF */

/*                   [Normal case] */

/*                   ... */

/*                   END */

/* $ Restrictions */

/*     1)  This routine relies on uniqueness of DAS file handles. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 22-JUL-2020 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.0, 19-MAY-2016 (NJB) */

/* -& */
/* $ Index_Entries */

/*     test DLA segments for match */

/* -& */

/*     Local variables */


/*     Give the function an initial value. */

    ret_val = FALSE_;

/*     If the handles don't match, we're done. */

    if (*han1 != *han2) {
	return ret_val;
    }

/*     Compare the DLA descriptors. All elements, including pointers, */
/*     must match in order to have a matching result. */

    for (i__ = 1; i__ <= 8; ++i__) {
	if (dsc1[(i__1 = i__ - 1) < 8 && 0 <= i__1 ? i__1 : s_rnge("dsc1", 
		i__1, "dlassg_", (ftnlen)232)] != dsc2[(i__2 = i__ - 1) < 8 &&
		 0 <= i__2 ? i__2 : s_rnge("dsc2", i__2, "dlassg_", (ftnlen)
		232)]) {
	    return ret_val;
	}
    }

/*     At this point, everything's a match. */

    ret_val = TRUE_;
    return ret_val;
} /* dlassg_ */

