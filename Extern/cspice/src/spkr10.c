/* spkr10.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__8 = 8;
static integer c__7 = 7;
static integer c__14 = 14;

/* $Procedure SPKR10 ( SPK, read record from SPK type 10 segment ) */
/* Subroutine */ int spkr10_(integer *handle, doublereal *descr, doublereal *
	et, doublereal *record)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer ends[2], indx, from, i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen), moved_(doublereal *, 
	    integer *, doublereal *);
    static logical found;
    static doublereal value;
    static integer to, nepoch, getelm;
    extern /* Subroutine */ int sgfcon_(integer *, doublereal *, integer *, 
	    integer *, doublereal *), sgmeta_(integer *, doublereal *, 
	    integer *, integer *), chkout_(char *, ftnlen), sgfpkt_(integer *,
	     doublereal *, integer *, integer *, doublereal *, integer *), 
	    sgfrvi_(integer *, doublereal *, doublereal *, doublereal *, 
	    integer *, logical *);
    static integer putelm;
    extern logical return_(void);
    static integer set1, set2;

/* $ Abstract */

/*     Read a single SPK data record from a segment of type 10 */
/*     (NORAD two line element sets). */

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

/* $ Keywords */

/*     EPHEMERIS */

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
/*     HANDLE     I   File handle. */
/*     DESCR      I   Segment descriptor. */
/*     ET         I   Target epoch. */
/*     RECORD     O   Data record. */

/* $ Detailed_Input */

/*     HANDLE, */
/*     DESCR    are the file handle and segment descriptor for */
/*              a SPK segment of type 10. */

/*     ET       is a target epoch, for which a data record from */
/*              a specific segment is required. */

/* $ Detailed_Output */

/*     RECORD   is the record from the specified segment which, */
/*              when evaluated at epoch ET, will give the state */
/*              (position and velocity) of some body, relative */
/*              to some center, in some inertial reference frame. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  It is assumed that the descriptor and handle supplied are */
/*         for a properly constructed type 10 segment. No checks are */
/*         performed to ensure this. */

/*     2)  If an error occurs while looking up SPK data, the error is */
/*         signaled by a routine in the call tree of this routine. */

/* $ Files */

/*     See argument HANDLE. */

/* $ Particulars */

/*     See the SPK Required Reading file for a description of the */
/*     structure of a data type 10 segment. */

/* $ Examples */

/*     The data returned by the SPKRnn routine is in its rawest form, */
/*     taken directly from the segment. As such, it will be meaningless */
/*     to a user unless he/she understands the structure of the data type */
/*     completely. Given that understanding, however, the SPKRxx */
/*     routines might be used to "dump" and check segment data for a */
/*     particular epoch. */


/*     C */
/*     C     Get a segment applicable to a specified body and epoch. */
/*     C */
/*           CALL SPKSFS ( BODY, ET, HANDLE, DESCR, IDENT, FOUND ) */

/*     C */
/*     C     Look at parts of the descriptor. */
/*     C */
/*           CALL DAFUS ( DESCR, 2, 6, DCD, ICD ) */
/*           CENTER = ICD( 2 ) */
/*           REF    = ICD( 3 ) */
/*           TYPE   = ICD( 4 ) */

/*           IF ( TYPE .EQ. 1 ) THEN */
/*              CALL SPKR10 ( HANDLE, DESCR, ET, RECORD ) */
/*                  . */
/*                  .  Look at the RECORD data. */
/*                  . */
/*           END IF */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.1, 17-JUN-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. Updated entry */
/*        #2 in $Exceptions section. Moved SPK required reading from */
/*        $Literature_References to $Required_Reading section. */

/* -    SPICELIB Version 1.1.0, 09-MAR-2009 (EDW) */

/*        Removed declaration of unused variable DOINT. */

/* -    SPICELIB Version 1.0.0, 05-JAN-1994 (WLT) */

/* -& */
/* $ Index_Entries */

/*     read record from type_10 SPK segment */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     We have 2 nutation/obliquity terms and their rates giving us */
/*     four angle components for each packet. */


/*     BEGEL1 is the location in the record where the first */
/*     two-line element set will begin. */


/*     BEGEL2 is the location in the record where the second */
/*     two-line element set will begin. */


/*     ENSET1 and ENSET2 are the locations in the record where the */
/*     last element of set 1 and set 2 will be located. */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("SPKR10", (ftnlen)6);

/*     Fetch the constants and store them in the first part of */
/*     the output RECORD. */

    sgfcon_(handle, descr, &c__1, &c__8, record);

/*     Locate the time in the file closest to the input ET. */

    sgfrvi_(handle, descr, et, &value, &indx, &found);

/*     Determine which pair of element sets to choose so that */
/*     they will bracket ET. */

    if (*et <= value) {
/* Computing MAX */
	i__1 = indx - 1;
	from = max(i__1,1);
	to = indx;
    } else {
	sgmeta_(handle, descr, &c__7, &nepoch);
	from = indx;
/* Computing MIN */
	i__1 = indx + 1;
	to = min(i__1,nepoch);
    }

/*     Fetch the element sets */

    sgfpkt_(handle, descr, &from, &to, &record[8], ends);

/*     If the size of the packets is not 14, this is an old style */
/*     two-line element set without nutation information.  We simply */
/*     set all of the angles to zero. */

    if (ends[0] == 10) {

/*        First shift the elements to their proper locations in RECORD */
/*        so there will be room to fill in the zeros. */

	putelm = 32;
	getelm = 28;
	while(getelm > 18) {
	    record[putelm - 1] = record[getelm - 1];
	    --putelm;
	    --getelm;
	}
	set1 = 19;
	set2 = 33;
	for (i__ = 1; i__ <= 4; ++i__) {
	    record[set1 - 1] = 0.;
	    record[set2 - 1] = 0.;
	    ++set1;
	    ++set2;
	}
    }

/*     If we only got one element set, ET  was either before the */
/*     first one in the segment or after the last one in the */
/*     segment.  We simply copy the one fetched a second time so */
/*     that the record is properly constructed. */

    if (from == to) {
	moved_(&record[8], &c__14, &record[22]);
    }
    chkout_("SPKR10", (ftnlen)6);
    return 0;
} /* spkr10_ */

