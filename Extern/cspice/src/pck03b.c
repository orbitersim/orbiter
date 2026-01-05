/* pck03b.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__3 = 3;
static integer c__1 = 1;

/* $Procedure PCK03B ( PCK, begin a type 3 segment ) */
/* Subroutine */ int pck03b_(integer *handle, char *segid, integer *body, 
	char *frame, doublereal *first, doublereal *last, integer *chbdeg, 
	ftnlen segid_len, ftnlen frame_len)
{
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    doublereal descr[5];
    extern logical failed_(void);
    doublereal dcoeff;
    integer ncoeff;
    extern /* Subroutine */ int pckpds_(integer *, char *, integer *, 
	    doublereal *, doublereal *, doublereal *, ftnlen), sigerr_(char *,
	     ftnlen), chkout_(char *, ftnlen), sgbwfs_(integer *, doublereal *
	    , char *, integer *, doublereal *, integer *, integer *, ftnlen), 
	    setmsg_(char *, ftnlen), errint_(char *, integer *, ftnlen);
    extern logical return_(void);
    integer pktsiz;

/* $ Abstract */

/*     Begin a type 03 PCK segment in the binary PCK file associated with */
/*     HANDLE. See also PCK03A and PCK03E. */

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

/* $ Abstract */

/*     Parameter declarations for the generic segments subroutines. */

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

/*      DAF Required Reading */

/* $ Keywords */

/*       GENERIC SEGMENTS */

/* $ Particulars */

/*     This include file contains the parameters used by the generic */
/*     segments subroutines, SGxxxx. A generic segment is a */
/*     generalization of a DAF array which places a particular structure */
/*     on the data contained in the array, as described below. */

/*     This file defines the mnemonics that are used for the index types */
/*     allowed in generic segments as well as mnemonics for the meta data */
/*     items which are used to describe a generic segment. */

/*     A DAF generic segment contains several logical data partitions: */

/*        1) A partition for constant values to be associated with each */
/*           data packet in the segment. */

/*        2) A partition for the data packets. */

/*        3) A partition for reference values. */

/*        4) A partition for a packet directory, if the segment contains */
/*           variable sized packets. */

/*        5) A partition for a reference value directory. */

/*        6) A reserved partition that is not currently used. This */
/*           partition is only for the use of the NAIF group at the Jet */
/*           Propulsion Laboratory (JPL). */

/*        7) A partition for the meta data which describes the locations */
/*           and sizes of other partitions as well as providing some */
/*           additional descriptive information about the generic */
/*           segment. */

/*                 +============================+ */
/*                 |         Constants          | */
/*                 +============================+ */
/*                 |          Packet 1          | */
/*                 |----------------------------| */
/*                 |          Packet 2          | */
/*                 |----------------------------| */
/*                 |              .             | */
/*                 |              .             | */
/*                 |              .             | */
/*                 |----------------------------| */
/*                 |          Packet N          | */
/*                 +============================+ */
/*                 |      Reference Values      | */
/*                 +============================+ */
/*                 |      Packet Directory      | */
/*                 +============================+ */
/*                 |    Reference  Directory    | */
/*                 +============================+ */
/*                 |       Reserved  Area       | */
/*                 +============================+ */
/*                 |     Segment Meta Data      | */
/*                 +----------------------------+ */

/*     Only the placement of the meta data at the end of a generic */
/*     segment is required. The other data partitions may occur in any */
/*     order in the generic segment because the meta data will contain */
/*     pointers to their appropriate locations within the generic */
/*     segment. */

/*     The meta data for a generic segment should only be obtained */
/*     through use of the subroutine SGMETA. The meta data should not be */
/*     written through any mechanism other than the ending of a generic */
/*     segment begun by SGBWFS or SGBWVS using SGWES. */

/* $ Restrictions */

/*     1) If new reference index types are added, the new type(s) should */
/*        be defined to be the consecutive integer(s) after the last */
/*        defined reference index type used. In this way a value for */
/*        the maximum allowed index type may be maintained. This value */
/*        must also be updated if new reference index types are added. */

/*     2) If new meta data items are needed, mnemonics for them must be */
/*        added to the end of the current list of mnemonics and before */
/*        the NMETA mnemonic. In this way compatibility with files having */
/*        a different, but smaller, number of meta data items may be */
/*        maintained. See the description and example below. */

/* $ Author_and_Institution */

/*     N.J. Bachman      (JPL) */
/*     K.R. Gehringer    (JPL) */
/*     W.L. Taber        (JPL) */
/*     F.S. Turner       (JPL) */

/* $ Literature_References */

/*     Generic Segments Required Reading. */
/*     DAF Required Reading. */

/* $ Version */

/* -    SPICELIB Version 1.1.1, 28-JAN-2004 (NJB) */

/*        Header update: equations for comptutations of packet indices */
/*        for the cases of index types 0 and 1 were corrected. */

/* -    SPICELIB Version 1.1.0, 25-09-98 (FST) */

/*        Added parameter MNMETA, the minimum number of meta data items */
/*        that must be present in a generic DAF segment. */

/* -    SPICELIB Version 1.0.0, 04-03-95 (KRG) (WLT) */

/* -& */

/*     Mnemonics for the type of reference value index. */

/*     Two forms of indexing are provided: */

/*        1) An implicit form of indexing based on using two values, a */
/*           starting value, which will have an index of 1, and a step */
/*           size between reference values, which are used to compute an */
/*           index and a reference value associated with a specified key */
/*           value. See the descriptions of the implicit types below for */
/*           the particular formula used in each case. */

/*        2) An explicit form of indexing based on a reference value for */
/*           each data packet. */


/*     Reference Index Type 0 */
/*     ---------------------- */

/*     Implied index. The index and reference value of a data packet */
/*     associated with a specified key value are computed from the two */
/*     generic segment reference values using the formula below. The two */
/*     generic segment reference values, REF(1) and REF(2), represent, */
/*     respectively, a starting value and a step size between reference */
/*     values. The index of the data packet associated with a key value */
/*     of VALUE is given by: */

/*                          /    VALUE - REF(1)    \ */
/*        INDEX = 1  +  INT | -------------------- | */
/*                          \        REF(2)        / */

/*     and the reference value associated with VALUE is given by: */

/*        REFVAL = REF(1) + DBLE (INDEX-1) * REF(2) */


/*     Reference Index Type 1 */
/*     ---------------------- */

/*     Implied index. The index and reference value of a data packet */
/*     associated with a specified key value are computed from the two */
/*     generic segment reference values using the formula below. The two */
/*     generic segment reference values, REF(1) and REF(2), represent, */
/*     respectively, a starting value and a step size between reference */
/*     values. The index of the data packet associated with a key value */
/*     of VALUE is given by: */

/*                          /          VALUE - REF(1)    \ */
/*        INDEX = 1  +  INT | 0.5 + -------------------- | */
/*                          \              REF(2)        / */


/*     and the reference value associated with VALUE is given by: */

/*        REFVAL = REF(1) + DBLE (INDEX-1) * REF(2) */

/*     We get the larger index in the event that VALUE is halfway between */
/*     X(I) and X(I+1), where X(I) = BUFFER(1) + DBLE (I-1) * REFDAT(2). */


/*     Reference Index Type 2 */
/*     ---------------------- */

/*     Explicit index. In this case the number of packets must equal the */
/*     number of reference values. The index of the packet associated */
/*     with a key value of VALUE is the index of the last reference item */
/*     that is strictly less than VALUE. The reference values must be in */
/*     ascending order, REF(I) < REF(I+1). */


/*     Reference Index Type 3 */
/*     ---------------------- */

/*     Explicit index. In this case the number of packets must equal the */
/*     number of reference values. The index of the packet associated */
/*     with a key value of VALUE is the index of the last reference item */
/*     that is less than or equal to VALUE. The reference values must be */
/*     in ascending order, REF(I) < REF(I+1). */


/*     Reference Index Type 4 */
/*     ---------------------- */

/*     Explicit index. In this case the number of packets must equal the */
/*     number of reference values. The index of the packet associated */
/*     with a key value of VALUE is the index of the reference item */
/*     that is closest to the value of VALUE. In the event of a "tie" */
/*     the larger index is selected. The reference values must be in */
/*     ascending order, REF(I) < REF(I+1). */


/*     These parameters define the valid range for the index types. An */
/*     index type code, MYTYPE, for a generic segment must satisfy the */
/*     relation MNIDXT <= MYTYPE <= MXIDXT. */


/*     The following meta data items will appear in all generic segments. */
/*     Other meta data items may be added if a need arises. */

/*       1)  CONBAS  Base Address of the constants in a generic segment. */

/*       2)  NCON    Number of constants in a generic segment. */

/*       3)  RDRBAS  Base Address of the reference directory for a */
/*                   generic segment. */

/*       4)  NRDR    Number of items in the reference directory of a */
/*                   generic segment. */

/*       5)  RDRTYP  Type of the reference directory 0, 1, 2 ... for a */
/*                   generic segment. */

/*       6)  REFBAS  Base Address of the reference items for a generic */
/*                   segment. */

/*       7)  NREF    Number of reference items in a generic segment. */

/*       8)  PDRBAS  Base Address of the Packet Directory for a generic */
/*                   segment. */

/*       9)  NPDR    Number of items in the Packet Directory of a generic */
/*                   segment. */

/*      10)  PDRTYP  Type of the packet directory 0, 1, ... for a generic */
/*                   segment. */

/*      11)  PKTBAS  Base Address of the Packets for a generic segment. */

/*      12)  NPKT    Number of Packets in a generic segment. */

/*      13)  RSVBAS  Base Address of the Reserved Area in a generic */
/*                   segment. */

/*      14)  NRSV    Number of items in the reserved area of a generic */
/*                   segment. */

/*      15)  PKTSZ   Size of the packets for a segment with fixed width */
/*                   data packets or the size of the largest packet for a */
/*                   segment with variable width data packets. */

/*      16)  PKTOFF  Offset of the packet data from the start of a packet */
/*                   record. Each data packet is placed into a packet */
/*                   record which may have some bookkeeping information */
/*                   prepended to the data for use by the generic */
/*                   segments software. */

/*      17)  NMETA   Number of meta data items in a generic segment. */

/*     Meta Data Item  1 */
/*     ----------------- */


/*     Meta Data Item  2 */
/*     ----------------- */


/*     Meta Data Item  3 */
/*     ----------------- */


/*     Meta Data Item  4 */
/*     ----------------- */


/*     Meta Data Item  5 */
/*     ----------------- */


/*     Meta Data Item  6 */
/*     ----------------- */


/*     Meta Data Item  7 */
/*     ----------------- */


/*     Meta Data Item  8 */
/*     ----------------- */


/*     Meta Data Item  9 */
/*     ----------------- */


/*     Meta Data Item 10 */
/*     ----------------- */


/*     Meta Data Item 11 */
/*     ----------------- */


/*     Meta Data Item 12 */
/*     ----------------- */


/*     Meta Data Item 13 */
/*     ----------------- */


/*     Meta Data Item 14 */
/*     ----------------- */


/*     Meta Data Item 15 */
/*     ----------------- */


/*     Meta Data Item 16 */
/*     ----------------- */


/*     If new meta data items are to be added to this list, they should */
/*     be added above this comment block as described below. */

/*        INTEGER               NEW1 */
/*        PARAMETER           ( NEW1   = PKTOFF + 1 ) */

/*        INTEGER               NEW2 */
/*        PARAMETER           ( NEW2   = NEW1   + 1 ) */

/*        INTEGER               NEWEST */
/*        PARAMETER           ( NEWEST = NEW2   + 1 ) */

/*     and then the value of NMETA must be changed as well to be: */

/*        INTEGER               NMETA */
/*        PARAMETER           ( NMETA  = NEWEST + 1 ) */

/*     Meta Data Item 17 */
/*     ----------------- */


/*     Maximum number of meta data items. This is always set equal to */
/*     NMETA. */


/*     Minimum number of meta data items that must be present in a DAF */
/*     generic segment.  This number is to remain fixed even if more */
/*     meta data items are added for compatibility with old DAF files. */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   The handle of a DAF file open for writing. */
/*     SEGID      I   The string to use for segment identifier. */
/*     BODY       I   The NAIF ID code for the body of the segment. */
/*     FRAME      I   The inertial frame for this segment. */
/*     FIRST      I   The first epoch for which the segment is valid. */
/*     LAST       I   The last epoch for which the segment is valid. */
/*     CHBDEG     I   The degree of the Chebyshev Polynomial used. */

/* $ Detailed_Input */

/*     HANDLE   is the file handle of a PCK file that has been */
/*              opened for writing. */

/*     SEGID    is the segment identifier. A PCK segment identifier */
/*              may contain up to 40 printing characters. It may also be */
/*              blank. */

/*     BODY     is the SPICE ID code for the body whose orientation */
/*              information is to be stored in the PCK segment being */
/*              created. */

/*     FRAME    is the inertial reference frame to which the orientation */
/*              data for BODY is relative. */

/*     FIRST    are the bounds on the ephemeris times, expressed as */
/*     LAST     seconds past J2000, for which the states can be used */
/*              to interpolate a state for BODY. */

/*     CHBDEG   is the degree of the Chebyshev Polynomial used for */
/*              each set of Chebyshev coefficients that are to be stored */
/*              in the segment. */

/* $ Detailed_Output */

/*     None. */

/*     The data are stored in the PCK segment in the DAF */
/*     attached to HANDLE. */

/*     See the $Particulars section for details about the */
/*     structure of a type 03 PCK segment. */

/* $ Parameters */

/*     This subroutine makes use of parameters defined in the file */
/*     'sgparam.inc'. */

/* $ Exceptions */

/*     1)  If the degree of the Chebyshev Polynomial to be used for this */
/*         segment is negative, the error SPICE(INVALIDARGUMENT) is */
/*         signaled. */

/*     2)  If there is any error in the structure or content of the */
/*         inputs other than the degree of the Chebyshev Polynomial, the */
/*         error is signaled by a routine in the call tree of this */
/*         routine. */

/*     3)  If a file access error occurs while this routine begins a new */
/*         type 03 segment, the error is signaled by a routine in the */
/*         call tree of this routine. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine begins a type 03 segment in the binary PCK file that */
/*     is associated with HANDLE. The file must have been opened with */
/*     write access. */

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
/*              CALL PCK03B ( HANDLE, SEGID, BODY, REFFRM, */
/*             .              FIRST,  LAST,  CHBDEG        ) */

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

/*     1)  The binary PCK file must be open with write access. */

/*     2)  Only one segment may be written to a particular PCK file at a */
/*         time. All of the data for the segment must be written and the */
/*         segment must be ended before another segment may be started in */
/*         the file. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     B.V. Semenov       (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.2, 03-JUN-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.1, 10-FEB-2014 (EDW) (BVS) */

/*        Minor edits to $Procedure; clean trailing whitespace. */
/*        Corrected order of header sections to conform to NAIF */
/*        standard. */

/*        Removed comments from the $Declarations section. */

/* -    SPICELIB Version 1.0.0, 06-MAR-1995 (KRG) */

/* -& */
/* $ Index_Entries */

/*     begin writing a type_03 PCK segment */

/* -& */

/*     Spicelib functions */


/*     Local Parameters */

/*     DAF ND and NI values for PCK files. */


/*     Length of an PCK descriptor. */


/*     Number of Euler angles. */


/*     The type of this segment. */


/*     The number of constants. */


/*     Local variables */


/*     Standard SPICELIB error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("PCK03B", (ftnlen)6);
    }

/*     First, check the degree of the polynomial to be sure that it is */
/*     not negative. */

    if (*chbdeg < 0) {
	setmsg_("The degree of the Chebyshev Polynomial was negative, #. The"
		" degree of the polynomial must be greater than or equal to z"
		"ero.", (ftnlen)123);
	errint_("#", chbdeg, (ftnlen)1);
	sigerr_("SPICE(INVALIDARGUMENT)", (ftnlen)22);
	chkout_("PCK03B", (ftnlen)6);
	return 0;
    }

/*     Create a descriptor for the segment we are about to write. */

    pckpds_(body, frame, &c__3, first, last, descr, frame_len);
    if (failed_()) {
	chkout_("PCK03B", (ftnlen)6);
	return 0;
    }

/*     We've got a valid descriptor, so compute a few things and begin */
/*     the segment. */

    ncoeff = *chbdeg + 1;
    pktsiz = ncoeff * 6 + 2;
    dcoeff = (doublereal) ncoeff;

/*     For this data type, we want to use an explicit reference value */
/*     index where the reference epochs are in increasing order. We also */
/*     want to have as the index for a particular request epoch the index */
/*     of the greatest reference epoch less than or equal to the request */
/*     epoch. These characteristics are prescribed by the mnemonic EXPLE. */
/*     See the include file 'sgparam.inc' for more details. */

    sgbwfs_(handle, descr, segid, &c__1, &dcoeff, &pktsiz, &c__3, segid_len);
    chkout_("PCK03B", (ftnlen)6);
    return 0;
} /* pck03b_ */

