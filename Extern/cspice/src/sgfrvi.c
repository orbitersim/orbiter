/* sgfrvi.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__12 = 12;
static integer c__7 = 7;
static integer c__5 = 5;
static integer c__6 = 6;
static integer c__0 = 0;
static integer c__4 = 4;
static integer c__3 = 3;

/* $Procedure SGFRVI ( Generic Segments: Fetch ref. value and index ) */
/* Subroutine */ int sgfrvi_(integer *handle, doublereal *descr, doublereal *
	x, doublereal *value, integer *indx, logical *found)
{
    /* Initialized data */

    static logical first = TRUE_;

    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    logical done;
    integer i__, begin;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    logical myfnd;
    extern /* Subroutine */ int errdp_(char *, doublereal *, ftnlen), dafgda_(
	    integer *, integer *, integer *, doublereal *);
    extern logical failed_(void);
    doublereal endref;
    integer nfetch;
    doublereal buffer[101];
    integer bfindx, remain;
    extern /* Subroutine */ int sgmeta_(integer *, doublereal *, integer *, 
	    integer *);
    doublereal dpimax;
    integer myrefb;
    extern integer lstled_(doublereal *, integer *, doublereal *);
    doublereal dptemp;
    integer fullrd, rdridx, myrdrb;
    extern integer intmax_(void);
    integer mynref;
    logical isdirv;
    integer myindx;
    extern /* Subroutine */ int chkout_(char *, ftnlen), setmsg_(char *, 
	    ftnlen);
    integer mynrdr;
    extern /* Subroutine */ int errint_(char *, integer *, ftnlen);
    doublereal myvalu;
    extern logical return_(void);
    extern /* Subroutine */ int sigerr_(char *, ftnlen);
    integer myrdrt, mynpkt, end;

/* $ Abstract */

/*     Find the reference value associated with the value X and its */
/*     index in a generic segment. The segment is identified by a DAF */
/*     file handle and segment descriptor. */

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
/*     HANDLE     I   The handle of a DAF open for reading. */
/*     DESCR      I   The descriptor for a DAF generic segment. */
/*     X          I   The key value used to find a reference and index. */
/*     VALUE      O   The reference value associated with X. */
/*     INDX       O   The index of VALUE within the reference values. */
/*     FOUND      O   A flag indicating whether values for X were found. */

/* $ Detailed_Input */

/*     HANDLE   is the handle of a DAF open for reading */

/*     DESCR    is the descriptor of the generic segment that we are */
/*              going to search for a reference value to associate with */
/*              X. */

/*     X        is a value for which the associated reference value */
/*              and reference index is requested. */

/* $ Detailed_Output */

/*     VALUE    is the reference value associated with the input value */
/*              X. */

/*     INDX     is the index of VALUE within the set of reference */
/*              values for the generic segment. This value may be used */
/*              to obtain a particular packet of data from the generic */
/*              segment. */

/*     FOUND    is a logical flag indicating whether a reference value */
/*              associated with X was found. If a reference value was */
/*              found, FOUND will have a value of .TRUE.; otherwise it */
/*              will have a value of .FALSE. */

/* $ Parameters */

/*     This subroutine makes use of parameters defined in the file */
/*     'sgparam.inc'. */

/* $ Exceptions */

/*     1)  If the reference directory structure is unrecognized, the */
/*         error SPICE(UNKNOWNREFDIR) is signaled. The most likely cause */
/*         of this error is that an upgrade to your version of the SPICE */
/*         toolkit is needed. */

/*     2)  If a value computed for the index of an implicitly indexed */
/*         generic segment is too large to be represented as an integer, */
/*         the error SPICE(INDEXTOOLARGE) is signaled. */

/* $ Files */

/*     See the description of HANDLE above. */

/* $ Particulars */

/*     This routine allows you to easily find the index and value */
/*     of the reference item that should be associated with a */
/*     value X. Given this information you can then easily retrieve */
/*     the packet that should be associated with X. */

/* $ Examples */

/*     Suppose that you have a generic segment that contains the */
/*     following items. */

/*         1)  Packets that model the motion of a body as a function */
/*             of time over some interval of time. */

/*         2)  Reference values that are the epochs corresponding */
/*             to the beginning of the intervals for the packets. */

/*     To retrieve the correct packet to use to compute the position */
/*     and velocity of the body at a particular epoch,  ET, you could */
/*     use the following code. (Note this block of code assumes that */
/*     you aren't going to run into any exceptional cases such as ET */
/*     falling outside the range of times for which the packets can */
/*     provide ephemeris data.) */

/*        Find out the index of the time that should be associated */
/*        with the ET we've been given */

/*        CALL SGFRVI ( HANDLE, DESCR, ET,  ETFND, INDX, FOUND ) */

/*        Fetch the INDX'th ephemeris packet from the segment. */

/*        CALL SGFPKT ( HANDLE, DESCR, INDX, EPHEM ) */

/* $ Restrictions */

/*     1)  The segment described by DESCR MUST be a generic segment, */
/*         otherwise the results of this routine are not predictable. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     W.L. Taber         (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.1, 26-OCT-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.2.0, 07-SEP-2001 (EDW) */

/*        Replaced DAFRDA call with DAFGDA. */

/* -    SPICELIB Version 1.1.0, 08-MAY-1996 (WLT) */

/*        A bug was found in the EXPCLS index case when the */
/*        trying to retrieve the last value in a generic segment. */
/*        This bug was discovered by the HP compiler complaining */
/*        that an index used was not initialized. */

/*        The offending line was */

/*                 MYVALU = BUFFER(I) */

/*        The corrected line is: */

/*                 MYVALU = BUFFER(BFINDX) */

/* -    SPICELIB Version 1.0.0, 28-MAR-1994 (KRG) (WLT) */

/* -& */
/* $ Index_Entries */

/*     find the index of a reference value in a generic segment */

/* -& */

/*     SPICELIB Functions */


/*     Local Parameters */

/*     Include the mnemonic values for the generic segment declarations. */


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


/*     Local Variables */


/*     Saved Variables */


/*     Initial Values */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("SGFRVI", (ftnlen)6);

/*     Set the value for the maximum index as a double precision number, */
/*     but only do it the first time into the subroutine. */

    if (first) {
	first = FALSE_;
	dpimax = (doublereal) intmax_();
    }

/*     Collect the necessary meta data values common to all cases. */

    sgmeta_(handle, descr, &c__12, &mynpkt);
    sgmeta_(handle, descr, &c__7, &mynref);
    sgmeta_(handle, descr, &c__5, &myrdrt);
    sgmeta_(handle, descr, &c__6, &myrefb);
    if (failed_()) {
	chkout_("SGFRVI", (ftnlen)6);
	return 0;
    }

/*     Check to be sure that we know how to deal with the type of index */
/*     in the segment. The index type should be between the minimum */
/*     allowed index type, MNIDXT, and the maximum allowed index type, */
/*     MXIDXT, as specified in the file 'sgparam.inc'. */

    if (myrdrt < 0 || myrdrt > 4) {
	setmsg_("The generic DAF segment you attempted to read has an unsupp"
		"orted reference directory structure. The integer code given "
		"for this structure is #, and allowed codes are within the ra"
		"nge # to #. The likely cause of this anomaly is your version"
		" of SPICELIB needs updating. Contact your system administrat"
		"or or NAIF for a toolkit update.", (ftnlen)331);
	errint_("#", &myrdrt, (ftnlen)1);
	errint_("#", &c__0, (ftnlen)1);
	errint_("#", &c__4, (ftnlen)1);
	sigerr_("SPICE(UNKNOWNREFDIR)", (ftnlen)20);
	chkout_("SGFRVI", (ftnlen)6);
	return 0;
    }

/*     We don't have an index yet and we initialize things to zero. */

    myfnd = FALSE_;
    myindx = 0;
    myvalu = 0.;

/*     We pass the idiot checks, so lets proceed. We have a IF block for */
/*     each allowed reference directory type code. */

/*        For implicitly indexed data packets, the interval */

/*           [ BUFFER(1), BUFFER(1) + (N - 1) * BUFFER(2) ) */

/*        is divided into subintervals as follows: */

/*           (-infinity, r1), [r_1,r_2) [r_2, r_3), ..., [r_i, r_(i+1)), */
/*            ..., [r_N, +infinity), */

/*        where N = the number of packets in the segment, MYNPKT, and */
/*        r_i = BUFFER(1) + (i-1) * BUFFER(2). */

/*        If X is in [r_i, r_(i+1)), i = 1, N-1, then we found a value */
/*        and the index returned will be i with the reference value */
/*        returned will be r_i. */

/*        If X is in [r_N, +infinity), then we found a value and the */
/*        index returned will be N and the reference value returned will */
/*        be r_N. */

/*        If X is in (-infinity, r1), we have two possibilities: */

/*           1) If the index type is implicit closest, we found a value, */
/*              the index returned will be 1 and the reference value */
/*              returned will be r_1. */

/*           2) If the index type is implicit less than or equal, we do */
/*              not find a value. */

/*        For explicitly indexed packets we simply search the reference */
/*        directory for an appropriate reference value. */

    if (myrdrt != 0 && myrdrt != 1) {

/*        In addition to the meta data items we already have, we also */
/*        need these. */

	sgmeta_(handle, descr, &c__4, &mynrdr);
	sgmeta_(handle, descr, &c__3, &myrdrb);
	if (failed_()) {
	    chkout_("SGFRVI", (ftnlen)6);
	    return 0;
	}

/*        We need to scan the reference directory (if there is one) to */
/*        determine the appropriate block of reference values to read */
/*        from the generic segment. Then we compute the number of */
/*        reference values to fetch and examine. Finally, based on the */
/*        index type we figure out whether we have found a reference */
/*        value or not. It will take a little while to get there, so */
/*        let's get going. */

/*        We have not started yet, so we're not done and we cannot have a */
/*        reference directory value yet. */

	done = FALSE_;
	isdirv = FALSE_;

/*        We have not read any full buffers of reference directory values */
/*        yet, all of the reference directory values remain to be read, */
/*        and we have no index for a reference directory value. */

	fullrd = 0;
	remain = mynrdr;
	rdridx = 0;

/*        Search the reference directory values to select the appropriate */
/*        block of reference values to read. */

	while(! done && remain > 0) {

/*           Read a buffer of reference directory items. */

	    nfetch = min(100,remain);
	    begin = myrdrb + fullrd * 100 + 1;
	    end = begin + nfetch - 1;
	    dafgda_(handle, &begin, &end, buffer);
	    if (failed_()) {
		chkout_("SGFRVI", (ftnlen)6);
		return 0;
	    }

/*           See if X is in the current buffer. */

	    rdridx = lstled_(x, &nfetch, buffer);
	    if (rdridx == 0) {

/*              If not, then X < BUFFER(1) and we're done. This indicates */
/*              that the desired reference value is before, or in, the */
/*              previous block of reference values. */

		done = TRUE_;
	    } else if (rdridx == nfetch) {

/*              If we get the last value of the buffer, then either we */
/*              are done, X = BUFFER(NFETCH), or X > BUFFER(NFETCH). */

		if (*x == buffer[(i__1 = nfetch - 1) < 101 && 0 <= i__1 ? 
			i__1 : s_rnge("buffer", i__1, "sgfrvi_", (ftnlen)425)]
			) {

/*                 If X = BUFFER(NFETCH) we are done, we have a directory */
/*                 value, and it might be a value we want to return. */

		    done = TRUE_;
		    isdirv = TRUE_;
		} else {

/*                 Otherwise, we might have more stuff to read, so update */
/*                 the remainder and the current number of full buffer */
/*                 reads and try the loop again. */

		    remain -= nfetch;
		    if (remain > 0) {

/*                    We don't want to increment FULLRD for a partial */
/*                    buffer read. The arithmetic for the index */
/*                    calculations below will use RDRIDX to deal with */
/*                    this. */

			++fullrd;
		    }
		}
	    } else {

/*              BUFFER(1) <= X < BUFFER(NFETCH), i.e., we have something */
/*              in the buffer. Check to see if X = BUFFER(RDRIDX). If so, */
/*              we are done, we have a directory value, and it might be a */
/*              value we want to return. Otherwise, we are just done. */

		done = TRUE_;
		if (*x == buffer[(i__1 = rdridx - 1) < 101 && 0 <= i__1 ? 
			i__1 : s_rnge("buffer", i__1, "sgfrvi_", (ftnlen)463)]
			) {
		    isdirv = TRUE_;
		}
	    }
	}
	rdridx = fullrd * 100 + rdridx;

/*        There are three cases that we need to consider when X is not a */
/*        reference directory value: */

/*           Case 1: 0 < RDRIDX < MYNRDR (most common first) */
/*           Case 2: RDRIDX = 0 */
/*           Case 3: RDRIDX = MYNRDR */

	if (! isdirv) {
	    if (rdridx > 0 && rdridx < mynrdr) {

/*              If we were able to bracket X before reaching the end of */
/*              the reference directory, then we KNOW that we have a */
/*              candidate for a reference value in the reference data. */
/*              All we need to do is read the reference data and find it */
/*              in the buffer. We also read the reference directory */
/*              values that bracket the desired reference value into */
/*              BUFFER, so that they are there if we need them. */

/* Computing MIN */
		i__1 = 101, i__2 = mynref - rdridx * 100 + 1;
		nfetch = min(i__1,i__2);
		begin = myrefb + rdridx * 100;
		end = begin + nfetch - 1;
		dafgda_(handle, &begin, &end, buffer);
		if (failed_()) {
		    chkout_("SGFRVI", (ftnlen)6);
		    return 0;
		}
		bfindx = lstled_(x, &nfetch, buffer);
		myindx = rdridx * 100 + bfindx - 1;
	    } else if (rdridx == 0) {

/*              The reference value may be one of the reference values */
/*              less than the first reference directory item. So we */
/*              compute the beginning and ending addresses for the data, */
/*              read it in, and try to find a reference value. */

		nfetch = min(101,mynref);
		begin = myrefb + 1;
		end = begin + nfetch - 1;
		dafgda_(handle, &begin, &end, buffer);
		if (failed_()) {
		    chkout_("SGFRVI", (ftnlen)6);
		    return 0;
		}
		bfindx = lstled_(x, &nfetch, buffer);
		myindx = bfindx;
	    } else if (rdridx == mynrdr) {

/*              If we were not able to bracket X before reaching the end */
/*              of the reference directory, then we might have a */
/*              candidate for a reference value in the reference data */
/*              after the last reference directory value. All we need to */
/*              do is read the reference data and look. */

/*              NOTE: NFETCH can never be zero or negative, so we can */
/*              glibly use it. The reason for this is the NFETCH can only */
/*              be zero if the desired reference value is a reference */
/*              directory value, and we already know that the reference */
/*              value we want is not a reference directory value, because */
/*              we are here. For similar reasons, NFETCH can never be */
/*              negative. */

		begin = myrefb + rdridx * 100;
		end = myrefb + mynref;
		nfetch = end - begin + 1;
		dafgda_(handle, &begin, &end, buffer);
		if (failed_()) {
		    chkout_("SGFRVI", (ftnlen)6);
		    return 0;
		}
		bfindx = lstled_(x, &nfetch, buffer);
		myindx = rdridx * 100 + bfindx - 1;
	    }
	} else {

/*           We have a reference directory value, whose index is easy to */
/*           compute. */

	    myindx = rdridx * 100;
	}

/*        Now, if we have a candidate for a reference value, lets make */
/*        sure, based on the type of index we have. */

	if (myrdrt == 2) {

/*           We have a reference value only if X > some reference */
/*           value. */

	    if (! isdirv) {

/*              If the value is not a reference directory value, then */
/*              we have two cases: */

/*                 Case 1: 0 < MYINDX <= MYNREF */
/*                 Case 2: MYINDX = 0 */

		if (myindx > 0 && myindx <= mynref) {

/*                 We found a reference value. The reference value we */
/*                 want is either the value indicated by MYINDX or */
/*                 the reference value immediately preceding MYINDX, */
/*                 if there is such a value. To deal with this we */
/*                 split the test up into two cases. */

		    if (myindx > 1) {

/*                    If X > BUFFER(BFINDX) then we are done, so set the */
/*                    value. If not, then we want the reference value */
/*                    that is immediately before the current one. */

			if (*x > buffer[(i__1 = bfindx - 1) < 101 && 0 <= 
				i__1 ? i__1 : s_rnge("buffer", i__1, "sgfrvi_"
				, (ftnlen)603)]) {
			    myfnd = TRUE_;
			    myvalu = buffer[(i__1 = bfindx - 1) < 101 && 0 <= 
				    i__1 ? i__1 : s_rnge("buffer", i__1, 
				    "sgfrvi_", (ftnlen)606)];
			} else {
			    myfnd = TRUE_;
			    myvalu = buffer[(i__1 = bfindx - 2) < 101 && 0 <= 
				    i__1 ? i__1 : s_rnge("buffer", i__1, 
				    "sgfrvi_", (ftnlen)611)];
			    --myindx;
			}
		    } else {

/*                    Remember, MYINDX is 1 here. If we are greater */
/*                    than the first reference value in the segment, */
/*                    we are done. Otherwise there is no reference */
/*                    value to be associated with X. */

			if (*x > buffer[(i__1 = myindx - 1) < 101 && 0 <= 
				i__1 ? i__1 : s_rnge("buffer", i__1, "sgfrvi_"
				, (ftnlen)623)]) {
			    myfnd = TRUE_;
			    myvalu = buffer[(i__1 = myindx - 1) < 101 && 0 <= 
				    i__1 ? i__1 : s_rnge("buffer", i__1, 
				    "sgfrvi_", (ftnlen)626)];
			} else {

/*                       We did not find a reference value. X was */
/*                       equal to the first reference value of the */
/*                       generic segment. */

			    myfnd = FALSE_;
			}
		    }
		} else if (myindx == 0) {

/*                 We did not find a reference value. X was < the */
/*                 first reference value for the generic segment. */

		    myfnd = FALSE_;
		}
	    } else {

/*              We have a reference directory value, and we are done. */
/*              Either the reference directory value is the one we */
/*              want or the reference value immediately preceding it */
/*              is the one we want. */

		myfnd = TRUE_;
		--myindx;
		begin = myrefb + myindx;
		end = begin;
		dafgda_(handle, &begin, &end, &myvalu);
		if (failed_()) {
		    chkout_("SGFRVI", (ftnlen)6);
		    return 0;
		}
	    }
	} else if (myrdrt == 3) {

/*           We have a reference value only if X >= some reference */
/*           value. At this point, either we have the value and index */
/*           we want or X is before the first reference value of the */
/*           generic segment. We consider two cases, the first when X */
/*           is not a reference directory value, and the second when */
/*           it is. */

	    if (! isdirv) {

/*              If X is not a directory value, then MYINDX is either */
/*              equal to zero, implying that X is before the first */
/*              reference value in the generic segment, or MYINDX > 0, */
/*              implying that we have found a reference value. */

		if (myindx > 0 && myindx <= mynref) {
		    myfnd = TRUE_;
		    myvalu = buffer[(i__1 = bfindx - 1) < 101 && 0 <= i__1 ? 
			    i__1 : s_rnge("buffer", i__1, "sgfrvi_", (ftnlen)
			    692)];
		} else if (myindx == 0) {

/*                 We did not find a reference value. X was < the */
/*                 first reference value for the generic segment. */

		    myfnd = FALSE_;
		}
	    } else {

/*              We have a reference directory value, and it is the one */
/*              we want. */

		myfnd = TRUE_;
		myvalu = *x;
	    }
	} else if (myrdrt == 4) {

/*           We have a reference value for every value of X. If X < */
/*           the first reference value of the generic segment, the */
/*           closest value is the first reference value. If X > the */
/*           last reference value of the generic segment, the closest */
/*           value is the last reference value. For X between the */
/*           first and last reference values we simple take the */
/*           closest reference value to X, resolving a tie by */
/*           accepting the larger reference value. */

	    if (! isdirv) {

/*              If X is not a directory value, then MYINDX is either */
/*              equal to zero, implying that X is before the first */
/*              reference value in the generic segment, */
/*              0 < MYINDX < MYNPKT, implying X is between the first */
/*              and last reference values in the generic segment, or */
/*              MYINDX = MYNPKT implying that X is greater than or */
/*              equal to the last reference value. */

		if (myindx > 0 && myindx < mynref) {
		    i__ = bfindx;

/*                 Find the closest value to X, choosing the larger in */
/*                 the event of a tie. */

		    if (buffer[(i__1 = i__) < 101 && 0 <= i__1 ? i__1 : 
			    s_rnge("buffer", i__1, "sgfrvi_", (ftnlen)742)] - 
			    *x <= *x - buffer[(i__2 = i__ - 1) < 101 && 0 <= 
			    i__2 ? i__2 : s_rnge("buffer", i__2, "sgfrvi_", (
			    ftnlen)742)]) {
			++i__;
			++myindx;
		    }
		    myfnd = TRUE_;
		    myvalu = buffer[(i__1 = i__ - 1) < 101 && 0 <= i__1 ? 
			    i__1 : s_rnge("buffer", i__1, "sgfrvi_", (ftnlen)
			    750)];
		} else if (myindx == 0) {

/*                 X is before the first reference value for the */
/*                 generic segment, so the closest reference value is */
/*                 the first one. */

		    myfnd = TRUE_;
		    myindx = 1;
		    myvalu = buffer[0];
		} else if (myindx == mynref) {

/*                 X is at of after the last reference value for the */
/*                 generic segment, so the closest reference value is */
/*                 the last reference value, which will be in BUFFER. */

		    myfnd = TRUE_;
		    myvalu = buffer[(i__1 = bfindx - 1) < 101 && 0 <= i__1 ? 
			    i__1 : s_rnge("buffer", i__1, "sgfrvi_", (ftnlen)
			    770)];
		}
	    } else {

/*              We have a reference directory value, and it is the one */
/*              we want. */

		myfnd = TRUE_;
		myvalu = *x;
	    }
	}
    } else if (myrdrt == 0) {

/*        Get the begin and end addresses from which to read the */
/*        reference values and get the reference values. */

	begin = myrefb + 1;
	end = myrefb + 2;
	dafgda_(handle, &begin, &end, buffer);
	if (failed_()) {
	    chkout_("SGFRVI", (ftnlen)6);
	    return 0;
	}
	endref = buffer[0] + (doublereal) (mynpkt - 1) * buffer[1];

/*        Compute the index if we can. */

	if (*x < buffer[0]) {

/*           If X is less than BUFFER(1), we do not have a reference */
/*           value. */

	    myfnd = FALSE_;
	} else if (*x > endref) {

/*           If X is greater than ENDREF, then we have a reference */
/*           value, ENDREF. */

	    myfnd = TRUE_;
	    myindx = mynpkt;
	    myvalu = endref;
	} else {

/*           r_1 < X < r_N, i.e., we found a value. Compute the index */
/*           and the reference value. */

	    if (mynpkt > 1) {
		myfnd = TRUE_;

/*              Compute the index. */

		dptemp = (*x - buffer[0]) / buffer[1] + 1.;

/*              Test to see if we can safely convert the index to an */
/*              integer. */

		if (dptemp > dpimax) {
		    setmsg_("The computed index is too large to be represent"
			    "ed as an integer. The most likely problem is tha"
			    "t an incorrect value was stored for the step siz"
			    "e. The value found for the step was: #", (ftnlen)
			    181);
		    errdp_("#", &buffer[1], (ftnlen)1);
		    sigerr_("SPICE(INDEXTOOLARGE)", (ftnlen)20);
		    chkout_("SGFRVI", (ftnlen)6);
		    return 0;
		}
		myindx = (integer) dptemp;
		myindx = min(myindx,mynpkt);
	    } else {

/*              There is only one packet. */

		myindx = 1;
	    }

/*           Compute the reference value. */

	    myvalu = buffer[0] + (doublereal) (myindx - 1) * buffer[1];
	}
    } else if (myrdrt == 1) {

/*        Get the begin and end addresses from which to read the */
/*        reference values and get the reference values. */

	begin = myrefb + 1;
	end = myrefb + 2;
	dafgda_(handle, &begin, &end, buffer);
	if (failed_()) {
	    chkout_("SGFRVI", (ftnlen)6);
	    return 0;
	}
	endref = buffer[0] + (doublereal) (mynpkt - 1) * buffer[1];

/*        Compute the index if we can. */

	if (*x < buffer[0]) {

/*           If X < BUFFER(1), then we found a value, the index */
/*           returned will be 1 and the reference value returned will */
/*           be BUFFER(1). */

	    myfnd = TRUE_;
	    myindx = 1;
	    myvalu = buffer[0];
	} else if (*x > endref) {

/*           If X > ENDREF, then we found a value, the index returned */
/*           will be MYNPKT and the reference value returned will be */
/*           ENDREF. */

	    myfnd = TRUE_;
	    myindx = mynpkt;
	    myvalu = endref;
	} else {

/*           r_1 < X < r_N, i.e., we found a value. Compute the index */
/*           and the reference value. If X is closer to r_I, the index */
/*           returned will be I with a reference value of r_I. If X is */
/*           closer to r_(I+1), the index returned will be I+1 with a */
/*           reference value of r_(I+1). */

	    if (mynpkt > 1) {
		myfnd = TRUE_;

/*              Compute the index. */

		dptemp = (*x - buffer[0]) / buffer[1] + 1.5;
		if (dptemp > dpimax + .5) {
		    setmsg_("The computed index is too large to be represent"
			    "ed as an integer. The most likely problem is tha"
			    "t an incorrect value was stored for the step siz"
			    "e. The value found for the step was: #", (ftnlen)
			    181);
		    errdp_("#", &buffer[1], (ftnlen)1);
		    sigerr_("SPICE(INDEXTOOLARGE)", (ftnlen)20);
		    chkout_("SGFRVI", (ftnlen)6);
		    return 0;
		}
		myindx = (integer) dptemp;
	    } else {

/*              There is only one packet. */

		myindx = 1;
	    }

/*           Compute the reference value. */

	    myvalu = buffer[0] + (doublereal) (myindx - 1) * buffer[1];
	}
    }

/*     At this point, we have either found a value or not. If so, then we */
/*     need to set the index, value, and found flag for output. */
/*     Otherwise, we simply set the found flag. */

    if (myfnd) {
	*indx = myindx;
	*value = myvalu;
    }
    *found = myfnd;
    chkout_("SGFRVI", (ftnlen)6);
    return 0;
} /* sgfrvi_ */

