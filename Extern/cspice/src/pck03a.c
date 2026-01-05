/* pck03a.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure PCK03A ( PCK, add data to a type 3 segment ) */
/* Subroutine */ int pck03a_(integer *handle, integer *ncsets, doublereal *
	coeffs, doublereal *epochs)
{
    extern /* Subroutine */ int chkin_(char *, ftnlen), errhan_(char *, 
	    integer *, ftnlen), sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), sgwfpk_(integer *, integer *, doublereal *, integer *, 
	    doublereal *), setmsg_(char *, ftnlen), errint_(char *, integer *,
	     ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Add data to a type 03 PCK segment in the binary PCK file */
/*     associated with HANDLE. See also PCK03B and PCK03E. */

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

/*     PCK */

/* $ Keywords */

/*     PCK */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   The handle of a DAF file open for writing. */
/*     NCSETS     I   The number of Cheby coefficient sets and epochs. */
/*     COEFFS     I   The collection of Cheby coefficient sets. */
/*     EPOCHS     I   The epochs associated with the element sets. */

/* $ Detailed_Input */

/*     HANDLE   is the file handle of a PCK file that has been */
/*              opened for writing. */

/*     NCSETS   is the number of Cheby coefficient sets and epochs */
/*              to be stored in the segment. */

/*     COEFFS   contains a time-ordered array of Chebyshev coefficient */
/*              sets for computing the orientation of a body relative to */
/*              the an inertial frame. The orientation is defined by */
/*              the angles RA, DEC, W and body fixed angular rates for */
/*              each axis of the body fixed coordinate system defined by */
/*              RA, DEC, and W. All of the angles and the angular rates */
/*              of the axes are given in degrees. */

/*              See the $Particulars section for details on how to store */
/*              the coefficient sets in the array. */

/*     EPOCHS   contains the epochs (ephemeris seconds past J2000) */
/*              corresponding to the elements in COEFFS. The I'th */
/*              epoch must equal the epoch of the I'th set of */
/*              coefficients. The epochs must form a strictly increasing */
/*              sequence. */

/* $ Detailed_Output */

/*     None. */

/*     The data is stored in a segment in the binary PCK file */
/*     associated with HANDLE. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the number of coefficient sets and epochs is not positive, */
/*         the error SPICE(INVALIDARGUMENT) is signaled. */

/* $ Files */

/*     See argument HANDLE. */

/* $ Particulars */

/*     This routine adds data to a type 03 PCK segment in the binary */
/*     PCK file that is associated with HANDLE. The segment must have */
/*     been begun by calling PCK03B. */

/*     This routine is one of a set of three routines for creating and */
/*     adding data to type 03 PCK segments. These routines are: */

/*        PCK03B: Begin a type 03 PCK segment. This routine must be */
/*                called before any data may be added to a type 03 */
/*                segment. */

/*        PCK03A: Add data to a type 03 PCK segment. This routine may be */
/*                called any number of times after a call to PCK03B to */
/*                add type 03 records to the PCK segment that was */
/*                started. */

/*        PCK03E: End a type 03 PCK segment. This routine is called to */
/*                make the type 03 segment a permanent addition to the */
/*                PCK file. Once this routine is called, no further type */
/*                03 records may be added to the segment. A new segment */
/*                must be started. */

/*     A type 03 PCK segment consists of coefficient sets for fixed order */
/*     Chebyshev polynomials over consecutive time intervals, where the */
/*     time intervals need not all be of the same length. The Chebyshev */
/*     polynomials represent the orientation of a body specified relative */
/*     to an inertial frame by the angles RA, DEC, W and body fixed */
/*     angular rates for each axis of the body fixed coordinate system */
/*     defined by RA, DEC, and W. All of the angles and the angular rates */
/*     of the axes are given in degrees. */

/*     The orientation data supplied to the type 03 PCK writer is packed */
/*     into an array as a sequence of logical records, */

/*        ----------------------------------------------------- */
/*        | Record 1 | Record 2 | ... | Record N-1 | Record N | */
/*        ----------------------------------------------------- */

/*     with each record has the following format. */

/*           ------------------------------------------------ */
/*           |  The midpoint of the approximation interval  | */
/*           ------------------------------------------------ */
/*           |  The radius of the approximation interval    | */
/*           ------------------------------------------------ */
/*           |  CHBDEG+1 coefficients for RA                | */
/*           ------------------------------------------------ */
/*           |  CHBDEG+1 coefficients for DEC               | */
/*           ------------------------------------------------ */
/*           |  CHBDEG+1 coefficients for W                 | */
/*           ------------------------------------------------ */
/*           |  CHBDEG+1 coefficients for the X-axis rate   | */
/*           ------------------------------------------------ */
/*           |  CHBDEG+1 coefficients for the Y-axis rate   | */
/*           ------------------------------------------------ */
/*           |  CHBDEG+1 coefficients for the Z-axis rate   | */
/*           ------------------------------------------------ */

/* $ Examples */

/*     Assume we have the following for each of the examples that */
/*     follow. */

/*        HANDLE   is the handle of a PCK file opened with write */
/*                 access. */

/*        SEGID    is a character string of no more than 40 characters */
/*                 which provides a pedigree for the data in the PCK */
/*                 segment. */

/*        BODY     is the SPICE ID code for the body whose orientation */
/*                 data is to be placed into the file. */

/*        REFFRM   is the name of the SPICE inertial reference frame */
/*                 the orientation data is relative to. */

/*        FIRST    is the starting epoch, in seconds past J2000, for */
/*                 the orientation data to be placed into the segment. */

/*        LAST     is the ending epoch, in seconds past J2000, for */
/*                 the orientation data to be placed into the segment. */

/*     Example 1: */

/*        For this example, we also assume that: */

/*           N        is the number of type 03 records that we want to */
/*                    put into a segment in PCK file. */

/*           RECRDS   contains N type 03 records packaged for the PCK */
/*                    file. */

/*           ETSTRT   contains the initial epochs for each of the */
/*                    records contained in RECRDS, where */

/*                       ETSTRT(I) < ETSTRT(I+1), I = 1, N-1 */

/*                       ETSTRT(1) <= FIRST, ETSTRT(N) < LAST */

/*                       ETSTRT(I+1), I = 1, N-1, is the ending epoch for */
/*                       record I as well as the initial epoch for record */
/*                       I+1. */

/*        Then the following code fragment demonstrates how to create a */
/*        type 03 PCK segment if all of the data for the segment is */
/*        available at one time. */

/*        C */
/*        C     Begin the segment. */
/*        C */
/*              CALL PCK03B ( HANDLE, SEGID, BODY, REFFRM, */
/*             .              FIRST,  LAST,  CHBDEG        ) */
/*        C */
/*        C     Add the data to the segment all at once. */
/*        C */
/*              CALL PCK03A ( HANDLE, N, RECRDS, ETSTRT ) */
/*        C */
/*        C     End the segment, making the segment a permanent addition */
/*        C     to the PCK file. */
/*        C */
/*              CALL PCK03E ( HANDLE ) */

/*     Example 2: */

/*        In this example we want to add type O3 PCK records, as */
/*        described above in the $Particulars section, to the segment */
/*        being written as they are generated. The ability to write the */
/*        records in this way is useful if computer memory is limited. It */
/*        may also be convenient from a programming perspective to write */
/*        the records one at a time. */

/*        For this example, assume that we want to generate N type 03 PCK */
/*        records, one for each of N time intervals, writing them all to */
/*        the same segment in a PCK file. Let */

/*           N        be the number of type 03 records that we want to */
/*                    generate and put into a segment in an PCK file. */

/*           RECORD   be an array with enough room to hold a single type */
/*                    03 record, i.e. RECORD should have dimension at */
/*                    least 6 * (CHBDEG + 1 ) + 2. */

/*           START    be an array of N times that are the beginning */
/*                    epochs for each of the intervals of interest. The */
/*                    times should be in increasing order and the start */
/*                    time for the first interval should equal the */
/*                    starting time for the segment. */

/*                       START(I) < START(I+1), I = 1, N-1 */

/*                       START(1) = FIRST */

/*           STOP     be an array of N times that are the ending epochs */
/*                    for each of the intervals of interest. The times */
/*                    should be in increasing order and the stop time for */
/*                    interval I should equal the start time for interval */
/*                    I+1, i.e., we want to have continuous coverage in */
/*                    time across all of the records. Also, the stop time */
/*                    for the last interval should equal the ending time */
/*                    for the segment. */

/*                       STOP(I) < STOP(I+1), I = 1, N-1 */

/*                       STOP(I) = START(I+1), I = 1, N-1 */

/*                       STOP(N) = LAST */

/*           GENREC( TIME1, TIME2, RECORD ) */

/*                    be a subroutine that generates a type 03 PCK record */
/*                    for a time interval specified by TIME1 and TIME2. */

/*        Then the following code fragment demonstrates how to create a */
/*        type 03 PCK segment if all of the data for the segment is not */
/*        available at one time. */

/*        C */
/*        C     Begin the segment. */
/*        C */
/*              CALL PCK03B ( HANDLE, SEGID, DESCR, CHBDEG ) */

/*        C */
/*        C     Generate the records and write them to the segment in the */
/*        C     PCK file one at at time. */
/*        C */
/*              DO I = 1, N */

/*                 CALL GENREC ( START(I), STOP(I), RECORD   ) */
/*                 CALL PCK03A ( HANDLE, 1, RECORD, START(I) ) */

/*              END DO */

/*        C */
/*        C     End the segment, making the segment a permanent addition */
/*        C     to the PCK file. */
/*        C */
/*              CALL PCK03E ( HANDLE ) */

/* $ Restrictions */

/*     1)  The type 03 PCK segment to which we are adding data must have */
/*         been started by the routine PCK03B, the routine which begins a */
/*         type 03 PCK segment. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.2, 03-JUN-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.1.1, 03-JAN-2014 (EDW) */

/*        Minor edits to $Procedure; clean trailing whitespace. */
/*        Corrected order of header sections to conform to NAIF */
/*        standard. */

/* -    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW) */

/*        Removed DAFHLU call; replaced ERRFN call with ERRHAN. */

/* -    SPICELIB Version 1.0.0, 06-MAR-1995 (KRG) */

/* -& */
/* $ Index_Entries */

/*     add data to a type_03 PCK segment */

/* -& */

/*     Spicelib functions */


/*     Standard SPICELIB error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("PCK03A", (ftnlen)6);
    }

/*     First, check to see if the number of coefficient sets and epochs */
/*     is positive. */

    if (*ncsets <= 0) {
	setmsg_("The number of coefficient sets and epochs to be added to th"
		"e PCK segment in the file '#' was not positive. Its value wa"
		"s: #.", (ftnlen)124);
	errhan_("#", handle, (ftnlen)1);
	errint_("#", ncsets, (ftnlen)1);
	sigerr_("SPICE(INVALIDARGUMENT)", (ftnlen)22);
	chkout_("PCK03A", (ftnlen)6);
	return 0;
    }

/*     Add the data. */

    sgwfpk_(handle, ncsets, coeffs, ncsets, epochs);

/*     No need to check FAILED() here, since all we do is check out. */
/*     Leave it up to the caller. */

    chkout_("PCK03A", (ftnlen)6);
    return 0;
} /* pck03a_ */

