/* ckw04b.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__6 = 6;
static integer c__0 = 0;
static integer c__3 = 3;

/* $Procedure CKW04B ( CK type 04: Begin a segment ) */
/* Subroutine */ int ckw04b_(integer *handle, doublereal *begtim, integer *
	inst, char *ref, logical *avflag, char *segid, ftnlen ref_len, ftnlen 
	segid_len)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    integer i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen), dafps_(integer *, 
	    integer *, doublereal *, integer *, doublereal *);
    doublereal descr[5];
    extern /* Subroutine */ int errch_(char *, char *, ftnlen, ftnlen);
    integer value;
    doublereal dcoeff;
    integer refcod;
    extern /* Subroutine */ int namfrm_(char *, integer *, ftnlen);
    extern integer lastnb_(char *, ftnlen);
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), sgbwvs_(integer *, doublereal *,
	     char *, integer *, doublereal *, integer *, ftnlen);
    extern logical return_(void);
    doublereal dcd[2];
    integer icd[6];

/* $ Abstract */

/*     Begin a type CK04 segment in the DAF file associated with */
/*     HANDLE. See also CKW04A and CKW04E. */

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

/*     CK */
/*     DAF */

/* $ Keywords */

/*     POINTING */

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

/* $ Abstract */

/*     Declarations of the CK data type specific and general CK low */
/*     level routine parameters. */

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

/*     CK.REQ */

/* $ Keywords */

/*     CK */

/* $ Restrictions */

/*     1) If new CK types are added, the size of the record passed */
/*        between CKRxx and CKExx must be registered as separate */
/*        parameter. If this size will be greater than current value */
/*        of the CKMRSZ parameter (which specifies the maximum record */
/*        size for the record buffer used inside CKPFS) then it should */
/*        be assigned to CKMRSZ as a new value. */

/* $ Author_and_Institution */

/*     N.J. Bachman      (JPL) */
/*     B.V. Semenov      (JPL) */

/* $ Literature_References */

/*     CK Required Reading. */

/* $ Version */

/* -    SPICELIB Version 3.0.0, 27-JAN-2014 (NJB) */

/*        Updated to support CK type 6. Maximum degree for */
/*        type 5 was updated to be consistent with the */
/*        maximum degree for type 6. */

/* -    SPICELIB Version 2.0.0, 19-AUG-2002 (NJB) */

/*        Updated to support CK type 5. */

/* -    SPICELIB Version 1.0.0, 05-APR-1999 (BVS) */

/* -& */

/*     Number of quaternion components and number of quaternion and */
/*     angular rate components together. */


/*     CK Type 1 parameters: */

/*     CK1DTP   CK data type 1 ID; */

/*     CK1RSZ   maximum size of a record passed between CKR01 */
/*              and CKE01. */


/*     CK Type 2 parameters: */

/*     CK2DTP   CK data type 2 ID; */

/*     CK2RSZ   maximum size of a record passed between CKR02 */
/*              and CKE02. */


/*     CK Type 3 parameters: */

/*     CK3DTP   CK data type 3 ID; */

/*     CK3RSZ   maximum size of a record passed between CKR03 */
/*              and CKE03. */


/*     CK Type 4 parameters: */

/*     CK4DTP   CK data type 4 ID; */

/*     CK4PCD   parameter defining integer to DP packing schema that */
/*              is applied when seven number integer array containing */
/*              polynomial degrees for quaternion and angular rate */
/*              components packed into a single DP number stored in */
/*              actual CK records in a file; the value of must not be */
/*              changed or compatibility with existing type 4 CK files */
/*              will be lost. */

/*     CK4MXD   maximum Chebychev polynomial degree allowed in type 4 */
/*              records; the value of this parameter must never exceed */
/*              value of the CK4PCD; */

/*     CK4SFT   number of additional DPs, which are not polynomial */
/*              coefficients, located at the beginning of a type 4 */
/*              CK record that passed between routines CKR04 and CKE04; */

/*     CK4RSZ   maximum size of type 4 CK record passed between CKR04 */
/*              and CKE04; CK4RSZ is computed as follows: */

/*                 CK4RSZ = ( CK4MXD + 1 ) * QAVSIZ + CK4SFT */


/*     CK Type 5 parameters: */


/*     CK5DTP   CK data type 5 ID; */

/*     CK5MXD   maximum polynomial degree allowed in type 5 */
/*              records. */

/*     CK5MET   number of additional DPs, which are not polynomial */
/*              coefficients, located at the beginning of a type 5 */
/*              CK record that passed between routines CKR05 and CKE05; */

/*     CK5MXP   maximum packet size for any subtype.  Subtype 2 */
/*              has the greatest packet size, since these packets */
/*              contain a quaternion, its derivative, an angular */
/*              velocity vector, and its derivative.  See ck05.inc */
/*              for a description of the subtypes. */

/*     CK5RSZ   maximum size of type 5 CK record passed between CKR05 */
/*              and CKE05; CK5RSZ is computed as follows: */

/*                 CK5RSZ = ( CK5MXD + 1 ) * CK5MXP + CK5MET */


/*     CK Type 6 parameters: */


/*     CK6DTP   CK data type 6 ID; */

/*     CK6MXD   maximum polynomial degree allowed in type 6 */
/*              records. */

/*     CK6MET   number of additional DPs, which are not polynomial */
/*              coefficients, located at the beginning of a type 6 */
/*              CK record that passed between routines CKR06 and CKE06; */

/*     CK6MXP   maximum packet size for any subtype.  Subtype 2 */
/*              has the greatest packet size, since these packets */
/*              contain a quaternion, its derivative, an angular */
/*              velocity vector, and its derivative.  See ck06.inc */
/*              for a description of the subtypes. */

/*     CK6RSZ   maximum size of type 6 CK record passed between CKR06 */
/*              and CKE06; CK6RSZ is computed as follows: */

/*                 CK6RSZ = CK6MET + ( CK6MXD + 1 ) * ( CK6PS3 + 1 ) */

/*              where CK6PS3 is equal to the parameter CK06PS3 defined */
/*              in ck06.inc. Note that the subtype having the largest */
/*              packet size (subtype 2) does not give rise to the */
/*              largest record size, because that type is Hermite and */
/*              requires half the window size used by subtype 3 for a */
/*              given polynomial degree. */


/*     The parameter CK6PS3 must be in sync with C06PS3 defined in */
/*     ck06.inc. */



/*     Maximum record size that can be handled by CKPFS. This value */
/*     must be set to the maximum of all CKxRSZ parameters (currently */
/*     CK5RSZ.) */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   The handle of an DAF file open for writing. */
/*     SEGID      I   The string to use for segment identifier. */
/*     INST       I   The NAIF ID code for the SC or instrument. */
/*     AVFLAG     I   The angular rates flag. */
/*     REF        I   The reference frame for this segment. */
/*     BEGTIM     I   The segment coverage start encoded SCLK time */

/* $ Detailed_Input */

/*     HANDLE   is the file handle of a CK file that has been */
/*              opened for writing. */

/*     SEGID    is the segment identifier. CK segment identifier */
/*              may contain up to 40 printing ASCII characters. */

/*     INST     is the SPICE ID for the SC structure or instrument */
/*              whose orientation are to be recorded in a CK file. */

/*     AVFLAG   is a logical flag that indicates whether segment will */
/*              contain angular rate information. */

/*     REF      is the name of a reference frame that pointing is */
/*              given with respect to, for example 'J2000'. */

/*     BEGTIM   is the encoded SCLK time for the start of the segment */
/*              coverage. */

/* $ Detailed_Output */

/*     None. */

/*     The input data is used to create the segment summary for the */
/*     segment being started in the DAF file associated with HANDLE. */

/*     See the $Particulars section for details about the structure of a */
/*     type 4 CK segment. */

/* $ Parameters */

/*     This subroutine makes use of parameters defined in the files */
/*     'sgparam.inc' and 'ckparam.inc'. */

/* $ Exceptions */

/*     1)  If a file access error occurs while this routine begins a new */
/*         type 04 segment, the error is signaled by a routine in the */
/*         call tree of this routine. */

/*     2)  If numeric ID for given reference frame cannot be resolved */
/*         from it's name, the error SPICE(INVALIDREFFRAME) is signaled. */

/*     3)  If SEGID is more than 40 characters long, the error */
/*         SPICE(SEGIDTOOLONG) is signaled. */

/*     4)  If SEGID contains any nonprintable characters, the error */
/*         SPICE(NONPRINTABLECHARS) is signaled. */

/* $ Files */

/*     See HANDLE in the $Detailed_Input section. */

/* $ Particulars */

/*     This routine begins writing a type 4 CK segment to the open DAF */
/*     file that is associated with HANDLE. The file must have been */
/*     opened with write access. */

/*     This routine is one of a set of three routines for creating and */
/*     adding data to type 4 CK segments. These routines are: */

/*        CKW04B: Begin a type 4 CK segment. This routine must be */
/*                called before any data may be added to a type 4 */
/*                segment. */

/*        CKW04A: Add data to a type 4 CK segment. This routine may be */
/*                called any number of times after a call to CKW04B to */
/*                add type 4 records to the CK segment that was */
/*                started. */

/*        CKW04E: End a type 4 CK segment. This routine is called to */
/*                make the type 4 segment a permanent addition to the */
/*                DAF file. Once this routine is called, no further type */
/*                4 records may be added to the segment. A new segment */
/*                must be started. */

/*     A type 4 CK segment consists of coefficient sets for variable */
/*     order Chebyshev polynomials over consecutive time intervals of */
/*     a variable length. The gaps between intervals are allowed. */
/*     The Chebyshev polynomials represent individual quaternion */
/*     components q0, q1, q2 and q3 and individual angular velocities */
/*     AV1, AV2 and AV3 if they are included with the data. */

/*     The pointing data supplied to the type 4 CK writer (CKW04A) */
/*     is packed into an array as a sequence of records, */

/*        ---------------------------------------------------- */
/*        | Record 1 | Record 2 | .. | Record N-1 | Record N | */
/*        ---------------------------------------------------- */

/*     with each record in data packets has the following format. */

/*        ---------------------------------------------------- */
/*        | The midpoint of the approximation interval       | */
/*        ---------------------------------------------------- */
/*        | The radius of the approximation interval         | */
/*        ---------------------------------------------------- */
/*        | Number of coefficients for q0                    | */
/*        ---------------------------------------------------- */
/*        | Number of coefficients for q1                    | */
/*        ---------------------------------------------------- */
/*        | Number of coefficients for q2                    | */
/*        ---------------------------------------------------- */
/*        | Number of coefficients for q3                    | */
/*        ---------------------------------------------------- */
/*        | Number of coefficients for AV1                   | */
/*        ---------------------------------------------------- */
/*        | Number of coefficients for AV2                   | */
/*        ---------------------------------------------------- */
/*        | Number of coefficients for AV3                   | */
/*        ---------------------------------------------------- */
/*        | q0 Cheby coefficients                            | */
/*        ---------------------------------------------------- */
/*        | q1 Cheby coefficients                            | */
/*        ---------------------------------------------------- */
/*        | q2 Cheby coefficients                            | */
/*        ---------------------------------------------------- */
/*        | q3 Cheby coefficients                            | */
/*        ---------------------------------------------------- */
/*        | AV1 Cheby coefficients (optional)                | */
/*        ---------------------------------------------------- */
/*        | AV2 Cheby coefficients (optional)                | */
/*        ---------------------------------------------------- */
/*        | AV3 Cheby coefficients (optional)                | */
/*        ---------------------------------------------------- */

/* $ Examples */

/*     Assume that we have: */

/*        HANDLE   is the handle of an CK file opened with write */
/*                 access. */

/*        SEGID    is a character string of no more than 40 characters */
/*                 which provides a pedigree for the data in the CK */
/*                 segment we will create. */

/*        INST     is the SPICE ID code for the instrument whose */
/*                 pointing data is to be placed into the file. */

/*        AVFLAG   angular rates flag. */

/*        REFFRM   is the name of the SPICE reference frame for the */
/*                 pointing data. */

/*        BEGTIM   is the starting encoded SCLK time for which the */
/*                 segment is valid. */

/*        ENDTIM   is the ending encoded SCLK time for which the segment */
/*                 is valid. */

/*        N        is the number of type 4 records that we want to */
/*                 put into a segment in an CK file. */

/*        NPKTS    is integer array which contains the lengths of */
/*                 variable size data packets */

/*        RECRDS   contains N type 4 records packaged for the CK */
/*                 file. */

/*        SCSTRT   contains the initial encoded SC time for each of */
/*                 the records contained in RECRDS, where */

/*                    SCSTRT(I) < SCSTRT(I+1), I = 1, N-1 */

/*                    SCSTRT(1) <= FIRST, SCSTRT(N) < LAST */

/*     Then the following code fragment demonstrates how to create */
/*     a type 4 CK segment if all of the data for the segment is */
/*     available at one time. */

/*     C */
/*     C     Begin the segment. */
/*     C */
/*           CALL CKW04B ( HANDLE, BEGTIM, INST, REF, AVFLAG, SEGID ) */
/*     C */
/*     C     Add the data to the segment all at once. */
/*     C */
/*           CALL CKW04A ( HANDLE, N, NPKTS, RECRDS, SCSTRT ) */
/*     C */
/*     C     End the segment, making the segment a permanent */
/*     C     addition to the CK file. */
/*     C */
/*           CALL CKW04E ( HANDLE, ENDTIM ) */

/* $ Restrictions */

/*     1)  The file containing the segment should be opened for read */
/*         or write access either by CKOPN or DAFOPW. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */
/*     Y.K. Zaiko         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.2, 17-AUG-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.1, 18-APR-2014 (BVS) */

/*        Minor header edits. */

/* -    SPICELIB Version 1.0.0, 05-MAY-1999 (YKZ) (BVS) */

/* -& */
/* $ Index_Entries */

/*     begin writing a type_4 CK segment */

/* -& */

/*     Spicelib functions */


/*     Local Parameters */


/*     DAF ND and NI values for CK files and length of a DAF descriptor. */


/*     The number of generic segment constants in a type 4 CK segment. */


/*     The integer codes of the first and last printable ASCII */
/*     characters. */


/*     The maximum number of characters allowed in a CK segment */
/*     identifier. */


/*     Local variables */


/*     Standard SPICELIB error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("CKW04B", (ftnlen)6);
    }

/*     Create a descriptor for the segment we are about to write. First */
/*     assign start and stop times. */

    dcd[0] = *begtim;
    dcd[1] = 0.;

/*     Second, resolve reference frame ID code from its name and */
/*     assign it to the corresponding descriptor component. Signal */
/*     an error if frame is not recognized. */

    namfrm_(ref, &refcod, ref_len);
    if (refcod == 0) {
	setmsg_("The reference frame # is not supported.", (ftnlen)39);
	errch_("#", ref, (ftnlen)1, ref_len);
	sigerr_("SPICE(INVALIDREFFRAME)", (ftnlen)22);
	chkout_("CKW04B", (ftnlen)6);
	return 0;
    }
    icd[1] = refcod;

/*     Third, assign values to the rest of the integer components of */
/*     the segment descriptor. */

    icd[0] = *inst;
    icd[2] = 4;
    if (*avflag) {
	icd[3] = 1;
    } else {
	icd[3] = 0;
    }

/*     Now pack the segment descriptor. */

    dafps_(&c__2, &c__6, dcd, icd, descr);

/*     Check that all characters in the SEGID are printable. */

    i__1 = lastnb_(segid, segid_len);
    for (i__ = 1; i__ <= i__1; ++i__) {
	value = *(unsigned char *)&segid[i__ - 1];
	if (value < 32 || value > 126) {
	    setmsg_("The segment identifier contains nonprintable characters",
		     (ftnlen)55);
	    sigerr_("SPICE(NONPRINTABLECHARS)", (ftnlen)24);
	    chkout_("CKW04B", (ftnlen)6);
	    return 0;
	}
    }

/*     Also check if the segment identifier is too long. */

    if (lastnb_(segid, segid_len) > 40) {
	setmsg_("Segment identifier contains more than 40 characters.", (
		ftnlen)52);
	sigerr_("SPICE(SEGIDTOOLONG)", (ftnlen)19);
	chkout_("CKW04B", (ftnlen)6);
	return 0;
    }

/*     We've got a valid descriptor and identifier and can begin */
/*     the segment. For this data type, we want to use an explicit */
/*     reference value index where the reference epochs are in */
/*     increasing order. We also want the index returned for a */
/*     particular request epoch to be the index of the greatest */
/*     reference epoch less than or equal to the request epoch. These */
/*     characteristics are prescribed by the mnemonic EXPLE. See the */
/*     include file 'sgparam.inc' for more details. */

    sgbwvs_(handle, descr, segid, &c__0, &dcoeff, &c__3, segid_len);

/*     No need to check FAILED() here, since all we do after this */
/*     point is checking out. */

    chkout_("CKW04B", (ftnlen)6);
    return 0;
} /* ckw04b_ */

