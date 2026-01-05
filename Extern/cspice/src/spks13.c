/* spks13.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure SPKS13 ( S/P Kernel, subset, type 13 ) */
/* Subroutine */ int spks13_(integer *handle, integer *baddr, integer *eaddr, 
	doublereal *begin, doublereal *end)
{
    extern /* Subroutine */ int chkin_(char *, ftnlen), spks09_(integer *, 
	    integer *, integer *, doublereal *, doublereal *), chkout_(char *,
	     ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Extract a subset of the data in an SPK segment of type 13 */
/*     into a new segment. */

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

/*     SPK */
/*     DAF */

/* $ Keywords */

/*     EPHEMERIS */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Handle of file containing source segment. */
/*     BADDR      I   Beginning address in file of source segment. */
/*     EADDR      I   Ending address in file of source segment. */
/*     BEGIN      I   Beginning (initial epoch) of subset. */
/*     END        I   End (final epoch) of subset. */

/* $ Detailed_Input */

/*     HANDLE, */
/*     BADDR, */
/*     EADDR    are the file handle assigned to an SPK file, and the */
/*              beginning and ending addresses of a segment within */
/*              that file. Together they determine a complete set of */
/*              ephemeris data, from which a subset is to be */
/*              extracted. */

/*     BEGIN, */
/*     END      are the initial and final epochs (ephemeris time) */
/*              of the subset. */

/*              The output segment will be padded to the left of */
/*              BEGIN and the right of END with sufficient states to */
/*              ensure that the segment yields an ephemeris identical */
/*              to that given by the source segment. */

/* $ Detailed_Output */

/*     See $Files section. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  This routine relies on the caller to ensure that the */
/*         interval [BEGIN, END] is contained in the coverage */
/*         interval of the segment. */

/*     2)  If BEGIN > END, no data is written to the target file. */

/* $ Files */

/*     Data is extracted from the file connected to the input */
/*     handle, and written to the current DAF open for writing. */

/*     The segment descriptor and summary must already have been written */
/*     prior to calling this routine. The segment must be ended */
/*     external to this routine. */

/* $ Particulars */

/*     This routine is intended solely for use as a utility by the */
/*     routine SPKSUB. */

/*     It transfers a subset of a type 13 SPK data segment to */
/*     a properly initialized segment of a second SPK file. */

/*     The exact structure of a segment of data type 13 is described */
/*     in the section on type 13 in the SPK Required Reading. */

/* $ Examples */

/*     This routine is intended only for use as a utility by SPKSUB. */
/*     To use this routine successfully, you must: */

/*        Open the SPK file from which to extract data. */
/*        Locate the segment from which data should be extracted. */

/*        Open the SPK file to which this data should be written. */
/*        Begin a new segment (array). */
/*        Write the summary information for the array. */

/*        Call this routine to extract the appropriate data from the */
/*        SPK open for read. */

/*        End the array to which this routine writes data. */

/*     Much of this procedure is carried out by the routine SPKSUB. The */
/*     examples of that routine illustrate more fully the process */
/*     described above. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 03-JUN-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.0, 25-FEB-2000 (NJB) */

/* -& */
/* $ Index_Entries */

/*     subset type_13 SPK segment */

/* -& */

/*     SPICELIB functions */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("SPKS13", (ftnlen)6);
    }

/*     The type 9 subsetter knows how to do this job. */

    spks09_(handle, baddr, eaddr, begin, end);
    chkout_("SPKS13", (ftnlen)6);
    return 0;
} /* spks13_ */

