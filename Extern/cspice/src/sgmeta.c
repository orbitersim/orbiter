/* sgmeta.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__2 = 2;
static integer c__15 = 15;

/* $Procedure SGMETA ( Generic segments: Fetch meta data value ) */
/* Subroutine */ int sgmeta_(integer *handle, doublereal *descr, integer *
	mnemon, integer *value)
{
    /* Initialized data */

    static integer lstbeg = -1;
    static integer lsthan = 0;

    /* System generated locals */
    integer i__1, i__2, i__3;
    static doublereal equiv_0[2];

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer), i_dnnt(doublereal *);

    /* Local variables */
    static integer meta[17];
    integer begm1, i__, begin;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
#define dtemp (equiv_0)
    extern /* Subroutine */ int moved_(doublereal *, integer *, doublereal *);
    doublereal xmeta[17];
#define itemp ((integer *)equiv_0)
    extern /* Subroutine */ int dafgda_(integer *, integer *, integer *, 
	    doublereal *);
    integer niovr2, nd;
    extern logical failed_(void);
    integer ni;
    extern /* Subroutine */ int dafhsf_(integer *, integer *, integer *);
    integer begmta, endmta, ametas;
    static logical nieven;
    static integer ioffst;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    doublereal dmtasz;
    static integer metasz;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen);
    extern logical return_(void);
    integer end;

/* $ Abstract */

/*     Obtain the value of a specified generic segment meta data item. */

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

/*     DAF */

/* $ Keywords */

/*     GENERIC SEGMENTS */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Handle of a DAF open for reading. */
/*     DESCR      I   Descriptor for a generic segment in the DAF. */
/*     MNEMON     I   An integer mnemonic for the desired meta data. */
/*     VALUE      O   The value of the meta data item requested. */

/* $ Detailed_Input */

/*     HANDLE   is the handle of a DAF opened for reading that */
/*              contains the generic segment described by DESCR. */

/*     DESCR    is the descriptor of a generic segment. This must */
/*              be the descriptor for a generic segment in the DAF */
/*              associated with HANDLE. */

/*     MNEMON   is the mnemonic used to represent the desired piece of */
/*              meta data. See the file 'sgparam.inc' for details, the */
/*              mnemonics, and their values. */

/* $ Detailed_Output */

/*     VALUE    is the value of the meta data item associated with */
/*              the mnemonic MNEMON that is in the generic segment */
/*              specified by HANDLE and DESCR. */

/* $ Parameters */

/*     This subroutine makes use of parameters defined in the file */
/*     'sgparam.inc'. */

/* $ Exceptions */

/*     1)  If the mnemonic for the meta data item is not valid, the error */
/*         SPICE(UNKNOWNMETAITEM) is signaled. */

/*     2)  If the last address in the DAF segment that reports the number */
/*         of meta data items that exist in the segment is less than */
/*         MNMETA, the error SPICE(INVALIDMETADATA) is signaled. */

/* $ Files */

/*     See the description of HANDLE above. */

/* $ Particulars */

/*     This routine is a utility for fetching the meta data associated */
/*     with a DAF generic segment. */

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

/*     Only the placement of the meta data at the end of a segment is */
/*     required. The other data partitions may occur in any order in the */
/*     segment because the meta data will contain pointers to the */
/*     appropriate locations of the other data partitions within the */
/*     segment. */

/*     The meta data for the segment should be obtained only through */
/*     use of this routine, SGMETA. */

/* $ Examples */

/*     Suppose that we would like to know how many constants, data */
/*     packets, and reference values are in the generic segment that we */
/*     have located in the DAF file associated with HANDLE. */

/*     C */
/*     C     Get the number of constants. */
/*     C */
/*           CALL SGMETA ( HANDLE, DESCR, NCON, NCONST ) */
/*     C */
/*     C     Get the number of data packets. */
/*     C */
/*           CALL SGMETA ( HANDLE, DESCR, NPKT, NPKTS ) */
/*     C */
/*     C     Get the number of constants. */
/*     C */
/*           CALL SGMETA ( HANDLE, DESCR, NREF, NREFS ) */

/*     C */
/*     C     Print the values. */
/*     C */
/*           WRITE (*, *) 'Number of Constants       : ', NCONST */
/*           WRITE (*, *) 'Number of Data Packets    : ', NPKTS */
/*           WRITE (*, *) 'Number of Reference Values: ', NREFS */

/* $ Restrictions */

/*     1)  The segment described by DESCR MUST be a generic segment, */
/*         otherwise the results of this routine are not predictable. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     W.L. Taber         (JPL) */
/*     F.S. Turner        (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.4.1, 20-AUG-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.4.0, 07-SEP-2001 (EDW) */

/*        Replaced DAFRDA call with DAFGDA. */

/* -    SPICELIB Version 1.3.0, 14-JUN-1999 (FST) */

/*        Altered the check in/out structure to be more reasonable. */
/*        This introduced redundant code, but only to increase the */
/*        efficiency of the normal mode of operation. */

/* -    SPICELIB Version 1.2.0, 24-SEP-1998 (FST) */

/*        Modified the code that handles reading the meta data from the */
/*        DAF to handle the case when the number of meta data items in */
/*        the file exceeds the current maximum defined in sgparam.inc. */
/*        In the event that this situation occurs, the routine loads */
/*        what meta data it can interpret and ignores the rest. In */
/*        this event if NMETA is requested, it is returned as MXMETA in */
/*        sgparam.inc. */

/*        An additional exception is now trapped by the routine. If */
/*        a generic segment in a DAF reports less than the known minimum */
/*        number of meta data items, then the routine signals the */
/*        error SPICE(INVALIDMETADATA). */

/*        The conditions that cause the SPICE(UNKNOWNMETAITEM) to be */
/*        signaled have been altered. Now if the integer mnemonic */
/*        is not between 1 and METASZ inclusive, or NMETA the error */
/*        is signaled. In the versions preceding this change, for */
/*        segments that reported less than NMETA items of meta data */
/*        could not use this routine to request the number of meta */
/*        data items without signaling SPICE(UNKNOWNMETAITEM). */

/* -    SPICELIB Version 1.1.0, 11-APR-1995 (KRG) */

/*        Modified the code that deals with the EQUIVALENCEd part */
/*        descriptor. We now call MOVED rather than using a direct */
/*        assignment. */

/* -    SPICELIB Version 1.0.0, 11-APR-1995 (KRG) (WLT) */

/* -& */
/* $ Index_Entries */

/*     retrieve a meta data value for a generic segment */

/* -& */

/*     Spicelib Functions */


/*     Local Parameters */

/*     Include the mnemonic values for the generic segment declarations. */


/*     Local Variables */


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


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }

/*     Handle the case when we are looking at the same file and segment */
/*     descriptor first.  This will result in duplicated code, but will */
/*     increase efficiency for the usual execution case. We need not */
/*     worry about the first time through, since LSTHAN and LSTBEG are */
/*     set to values that are bogus for actual DAF files. */

    if (*handle == lsthan) {

/*        Get the begin and end values from the descriptor. They are */
/*        located in the last two "integer" positions of the descriptor. */

	if (nieven) {
	    moved_(&descr[ioffst - 1], &c__1, dtemp);
	    begin = itemp[0];
	    end = itemp[1];
	} else {
	    moved_(&descr[ioffst - 1], &c__2, dtemp);
	    begin = itemp[1];
	    end = itemp[2];
	}

/*        Check the segment start address. This will tell us whether we */
/*        are looking at the same segment. */

	if (lstbeg == begin) {

/*        The only acceptable integer mnemonics at this point are 1 */
/*        through METASZ inclusive, and NMETA.  All other requests */
/*        should signal the SPICE(UNKNOWNMETAITEM) error, since the */
/*        current segment has no knowledge of these values. */

	    if (*mnemon <= 0 || *mnemon > metasz && *mnemon != 17) {
		chkin_("SGMETA", (ftnlen)6);
		*value = -1;
		setmsg_("The item requested, #, is not one of the recognized"
			" meta data items associated with this generic segmen"
			"t.", (ftnlen)105);
		errint_("#", mnemon, (ftnlen)1);
		sigerr_("SPICE(UNKNOWNMETAITEM)", (ftnlen)22);
		chkout_("SGMETA", (ftnlen)6);
		return 0;
	    }

/*           Set the value for the desired meta data item and return. */

	    *value = meta[(i__1 = *mnemon - 1) < 17 && 0 <= i__1 ? i__1 : 
		    s_rnge("meta", i__1, "sgmeta_", (ftnlen)371)];
	    return 0;
	}
    }

/*     At this point we are going to have to load the meta data.  If */
/*     the new handle and the old handle are the same, then the above */
/*     code has already retrieved the relevant segment addresses. If not */
/*     we need to fetch them.  First check in. */

    chkin_("SGMETA", (ftnlen)6);
    if (*handle != lsthan) {
	dafhsf_(handle, &nd, &ni);
	if (failed_()) {
	    chkout_("SGMETA", (ftnlen)6);
	    return 0;
	}
	niovr2 = ni / 2;
	nieven = niovr2 << 1 == ni;
	ioffst = nd + niovr2;
	lsthan = *handle;

/*        Get the begin and end values from the descriptor. They are */
/*        located in the last two "integer" positions of the descriptor. */

	if (nieven) {
	    moved_(&descr[ioffst - 1], &c__1, dtemp);
	    begin = itemp[0];
	    end = itemp[1];
	} else {
	    moved_(&descr[ioffst - 1], &c__2, dtemp);
	    begin = itemp[1];
	    end = itemp[2];
	}
    }

/*     Save the new begin address. Remember we have either just computed */
/*     this from the IF block above, or we computed it in the very */
/*     first IF block. */

    lstbeg = begin;

/*     Compute the begin address of the meta data and compute the */
/*     end address of the number we will be collecting. */

    dafgda_(handle, &end, &end, &dmtasz);
    if (failed_()) {
	chkout_("SGMETA", (ftnlen)6);
	return 0;
    }
    metasz = i_dnnt(&dmtasz);

/*     Store the actual meta size in AMETAS, in case METASZ ends up */
/*     being modified to conform to our current understanding of */
/*     meta data items. */

    ametas = metasz;

/*     Check to see if METASZ is an unacceptable value. */

    if (metasz < 15) {
	*value = -1;
	setmsg_("This segment reports that it has # meta data items. Every g"
		"eneric segment must have at least #.", (ftnlen)95);
	errint_("#", &metasz, (ftnlen)1);
	errint_("#", &c__15, (ftnlen)1);
	sigerr_("SPICE(INVALIDMETADATA)", (ftnlen)22);
	chkout_("SGMETA", (ftnlen)6);
	return 0;

/*     If it is not, we may need to fix a few things to work around some */
/*     older files that have been delivered. We perform these kludges */
/*     here. Originally, the number of meta data items was not */
/*     considered to be part of the meta data. It now is, so if we */
/*     encounter an older version of the file, we need to increment the */
/*     meta data size by 1. The number of meta data items is always */
/*     after all of the meta data items, so we can do this. */

    } else if (metasz == 15) {
	++metasz;
	ametas = metasz;

/*     If not check to see if METASZ is greater than the known MXMETA. */
/*     If it is then this segment most likely was constructed from */
/*     some newer version of the toolkit.  Load what meta data we */
/*     currently know about as laid out in sgparam.inc. */

    } else if (metasz > 17) {

/*        Leave AMETAS alone, since we need to know how far back */
/*        into the DAF file to begin reading. */

	metasz = 17;
    }

/*     The address computations that follow are precisely the same */
/*     as the previous version of the file, except when AMETAS is not */
/*     METASZ.  This only happens when METASZ is greater than MXMETA. */

    begmta = end - ametas + 1;
    endmta = begmta + metasz - 1;
    dafgda_(handle, &begmta, &endmta, xmeta);
    if (failed_()) {
	chkout_("SGMETA", (ftnlen)6);
	return 0;
    }

/*     Convert all of the meta data values into integers. */

    i__1 = metasz;
    for (i__ = 1; i__ <= i__1; ++i__) {
	meta[(i__2 = i__ - 1) < 17 && 0 <= i__2 ? i__2 : s_rnge("meta", i__2, 
		"sgmeta_", (ftnlen)510)] = i_dnnt(&xmeta[(i__3 = i__ - 1) < 
		17 && 0 <= i__3 ? i__3 : s_rnge("xmeta", i__3, "sgmeta_", (
		ftnlen)510)]);
    }

/*     The kludge continues... NMETA and MXMETA are ALWAYS the same */
/*     value, and any missing values must appear between the last known */
/*     value, META(METASZ-1), and the end value, META(NMETA), so we zero */
/*     them out. */

    meta[16] = metasz;
    for (i__ = metasz; i__ <= 16; ++i__) {
	meta[(i__1 = i__ - 1) < 17 && 0 <= i__1 ? i__1 : s_rnge("meta", i__1, 
		"sgmeta_", (ftnlen)522)] = 0;
    }

/*     Adjust the bases so that the N'th item of a partition is at */
/*     address META(PARTITION_BASE) + N */

    begm1 = begin - 1;
    meta[0] += begm1;
    meta[5] += begm1;
    meta[2] += begm1;
    meta[7] += begm1;
    meta[10] += begm1;
    meta[12] += begm1;

/*     The only acceptable integer mnemonics at this point are 1 through */
/*     METASZ inclusive, and NMETA.  All other requests should signal */
/*     the SPICE(UNKNOWNMETAITEM) error, since the current segment has */
/*     no knowledge of these values. */

    if (*mnemon <= 0 || *mnemon > metasz && *mnemon != 17) {
	*value = -1;
	setmsg_("The item requested, #, is not one of the recognized meta da"
		"ta items associated with this generic segment.", (ftnlen)105);
	errint_("#", mnemon, (ftnlen)1);
	sigerr_("SPICE(UNKNOWNMETAITEM)", (ftnlen)22);
	chkout_("SGMETA", (ftnlen)6);
	return 0;
    }

/*     Set the value for the desired meta data item, check out if we */
/*     need to, and return. */

    *value = meta[(i__1 = *mnemon - 1) < 17 && 0 <= i__1 ? i__1 : s_rnge(
	    "meta", i__1, "sgmeta_", (ftnlen)562)];
    chkout_("SGMETA", (ftnlen)6);
    return 0;
} /* sgmeta_ */

#undef itemp
#undef dtemp


