/* spkw10.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__10 = 10;
static integer c__8 = 8;
static integer c__14 = 14;
static integer c__4 = 4;
static integer c__1 = 1;

/* $Procedure SPKW10 (SPK - write a type 10 segment ) */
/* Subroutine */ int spkw10_(integer *handle, integer *body, integer *center, 
	char *frame, doublereal *first, doublereal *last, char *segid, 
	doublereal *consts, integer *n, doublereal *elems, doublereal *epochs,
	 ftnlen frame_len, ftnlen segid_len)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    integer base;
    doublereal dnut[4];
    integer i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    doublereal descr[6];
    extern /* Subroutine */ int moved_(doublereal *, integer *, doublereal *),
	     sgwes_(integer *);
    integer npkts;
    extern logical failed_(void);
    doublereal packet[14];
    integer nepoch;
    extern /* Subroutine */ int sgbwfs_(integer *, doublereal *, char *, 
	    integer *, doublereal *, integer *, integer *, ftnlen), chkout_(
	    char *, ftnlen), sgwfpk_(integer *, integer *, doublereal *, 
	    integer *, doublereal *), spkpds_(integer *, integer *, char *, 
	    integer *, doublereal *, doublereal *, doublereal *, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int zzwahr_(doublereal *, doublereal *);

/* $ Abstract */

/*     Write an SPK type 10 segment to the file specified by */
/*     the input HANDLE. */

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

/*     NAIF_IDS */
/*     SPK */

/* $ Keywords */

/*     SPK */

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
/*     BODY       I   The NAIF ID code for the body of the segment. */
/*     CENTER     I   The center of motion for BODY. */
/*     FRAME      I   The reference frame for this segment. */
/*     FIRST      I   The first epoch for which the segment is valid. */
/*     LAST       I   The last  epoch for which the segment is valid. */
/*     SEGID      I   The string to use for segment identifier. */
/*     CONSTS     I   Array of geophysical constants for the segment. */
/*     N          I   The number of element/epoch pairs to be stored */
/*     ELEMS      I   The collection of "two-line" element sets. */
/*     EPOCHS     I   The epochs associated with the element sets. */

/* $ Detailed_Input */

/*     HANDLE   is the file handle of an SPK file that has been opened */
/*              for writing. */

/*     BODY     is the NAIF ID for the body whose states are */
/*              to be recorded in an SPK file. */

/*     CENTER   is the NAIF ID for the center of motion associated */
/*              with BODY. */

/*     FRAME    is the reference frame that states are referenced to, */
/*              for example 'J2000'. */

/*     FIRST, */
/*     LAST     are the bounds on the ephemeris times, expressed as */
/*              seconds past J2000, for which the states can be used */
/*              to interpolate a state for BODY. */

/*     SEGID    is the segment identifier. An SPK segment identifier */
/*              may contain up to 40 characters. */

/*     CONSTS   are the geophysical constants needed for evaluation */
/*              of the two line elements sets. The order of these */
/*              constants must be: */

/*                 CONSTS(1) = J2 gravitational harmonic for Earth. */
/*                 CONSTS(2) = J3 gravitational harmonic for Earth. */
/*                 CONSTS(3) = J4 gravitational harmonic for Earth. */

/*              These first three constants are dimensionless. */

/*                 CONSTS(4) = KE: Square root of the GM for Earth where */
/*                             GM is expressed in Earth radii cubed */
/*                             per minutes squared. */

/*                 CONSTS(5) = QO: High altitude bound for atmospheric */
/*                             model in km. */

/*                 CONSTS(6) = SO: Low altitude bound for atmospheric */
/*                             model in km. */

/*                 CONSTS(7) = RE: Equatorial radius of the earth in km. */

/*                 CONSTS(8) = AE: Distance units/earth radius */
/*                             (normally 1). */

/*              Below are currently recommended values for these */
/*              items: */

/*                 J2 =    1.082616D-3 */
/*                 J3 =   -2.53881D-6 */
/*                 J4 =   -1.65597D-6 */

/*              The next item is the square root of GM for the Earth */
/*              given in units of earth-radii**1.5/Minute */

/*                 KE =    7.43669161D-2 */

/*              The next two items define the top and bottom of the */
/*              atmospheric drag model used by the type 10 ephemeris */
/*              type. Don't adjust these unless you understand the full */
/*              implications of such changes. */

/*                 QO =  120.0D0 */
/*                 SO =   78.0D0 */

/*              The ER value is the equatorial radius in km of the Earth */
/*              as used by NORAD. */

/*                 ER = 6378.135D0 */

/*              The value of AE is the number of distance units per */
/*              Earth radii used by the NORAD state propagation */
/*              software. The value should be 1 unless you've got a very */
/*              good understanding of the NORAD routine SGP4 and the */
/*              affect of changing this value. */

/*                 AE =    1.0D0 */

/*     N        is the number of "two-line" element sets and epochs */
/*              to be stored in the segment. */

/*     ELEMS    is a time-ordered array of two-line elements as supplied */
/*              in NORAD two-line element files. The I'th set of */
/*              elements should be stored as shown here: */

/*                 BASE = (I-1)*10 */

/*                 ELEMS( BASE + 1  )  = NDT2O in radians/minute**2 */
/*                 ELEMS( BASE + 2  )  = NDD6O in radians/minute**3 */
/*                 ELEMS( BASE + 3  )  = BSTAR */
/*                 ELEMS( BASE + 4  )  = INCL  in radians */
/*                 ELEMS( BASE + 5  )  = NODE0 in radians */
/*                 ELEMS( BASE + 6  )  = ECC */
/*                 ELEMS( BASE + 7  )  = OMEGA in radians */
/*                 ELEMS( BASE + 8  )  = M0    in radians */
/*                 ELEMS( BASE + 9  )  = N0    in radians/minute */
/*                 ELEMS( BASE + 10 )  = EPOCH of the elements in seconds */
/*                                       past ephemeris epoch J2000. */

/*              The meaning of these variables is defined by the */
/*              format of the two-line element files available from */
/*              NORAD. */

/*     EPOCHS   is an n-dimensional array that contains the epochs */
/*              (ephemeris seconds past J2000) corresponding to the */
/*              elements in ELEMS. The I'th epoch must equal the epoch */
/*              of the I'th element set. EPOCHS must form a strictly */
/*              increasing sequence. */

/* $ Detailed_Output */

/*     None. */

/*     The routine writes an SPK type 10 segment to the file attached to */
/*     HANDLE. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the structure or content of the inputs are invalid, an */
/*         error is signaled by a routine in the call tree of this */
/*         routine. */

/*     2)  If any file access error occurs, the error is signaled by a */
/*         routine in the call tree of this routine. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine writes a type 10 SPK segment to the SPK file open */
/*     for writing that is attached to HANDLE. */

/*     The routine GETELM reads two-line element sets, as those */
/*     distributed by NORAD, and converts them to the elements in units */
/*     suitable for use in this routine. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Suppose that you have collected the two-line element data */
/*        for a spacecraft with NORAD ID 18123. The following example */
/*        code demonstrates how you could go about creating a type 10 */
/*        SPK segment. */

/*        Use the meta-kernel shown below to load the required SPICE */
/*        kernels. */


/*           KPL/MK */

/*           File name: spkw10_ex1.tm */

/*           This meta-kernel is intended to support operation of SPICE */
/*           example programs. The kernels shown here should not be */
/*           assumed to contain adequate or correct versions of data */
/*           required by SPICE-based user applications. */

/*           In order for an application to use this meta-kernel, the */
/*           kernels referenced here must be present in the user's */
/*           current working directory. */

/*           The names and contents of the kernels referenced */
/*           by this meta-kernel are as follows: */

/*              File name           Contents */
/*              ---------           ------------------------------------ */
/*              naif0012.tls        Leapseconds */
/*              geophysical.ker     geophysical constants for evaluation */
/*                                  of two-line element sets. */

/*           \begindata */

/*              KERNELS_TO_LOAD = ( 'naif0012.tls', */
/*                                  'geophysical.ker'  ) */

/*           \begintext */

/*           The geophysical.ker is a PCK file that is provided with the */
/*           SPICE toolkit under the "/data" directory. */

/*           End of meta-kernel */


/*        Example code begins here. */


/*              PROGRAM SPKW10_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     SPICELIB functions. */
/*        C */
/*              DOUBLE PRECISION      SPD */

/*        C */
/*        C     Local parameters. */
/*        C */
/*              INTEGER               NAMLEN */
/*              PARAMETER           ( NAMLEN = 40               ) */

/*              INTEGER               PNAMLN */
/*              PARAMETER           ( PNAMLN = 2                ) */

/*              CHARACTER*(*)         SPK10 */
/*              PARAMETER           ( SPK10  = 'spkw10_ex1.bsp' ) */

/*              INTEGER               TLELLN */
/*              PARAMETER           ( TLELLN = 69               ) */

/*        C */
/*        C     The SPK type 10 segment will contain 18 two-line */
/*        C     elements sets for the NORAD spacecraft 18123 with */
/*        C     respect to the Earth (ID 399) in the J2000 reference */
/*        C     frame. */
/*        C */
/*        C     As stated in the naif_ids required reading, for Earth */
/*        C     orbiting spacecraft lacking a DSN identification code, */
/*        C     the NAIF ID is derived from the tracking ID assigned to */
/*        C     it by NORAD via: */
/*        C */
/*        C        NAIF ID = -100000 - NORAD ID code */
/*        C */
/*              INTEGER               TLESSZ */
/*              PARAMETER           ( TLESSZ = 9       ) */

/*              INTEGER               BODY */
/*              PARAMETER           ( BODY   = -118123 ) */

/*              INTEGER               CENTER */
/*              PARAMETER           ( CENTER = 399     ) */

/*              CHARACTER*(*)         FRMNAM */
/*              PARAMETER           ( FRMNAM = 'J2000' ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              CHARACTER*(NAMLEN)    IFNAME */
/*              CHARACTER*(PNAMLN)    NOADPN ( 8           ) */
/*              CHARACTER*(NAMLEN)    SEGID */
/*              CHARACTER*(TLELLN)    TLE    ( 2  * TLESSZ ) */

/*              DOUBLE PRECISION      CONSTS ( 8           ) */
/*              DOUBLE PRECISION      ELEMS  ( 10 * TLESSZ ) */
/*              DOUBLE PRECISION      EPOCHS (      TLESSZ ) */
/*              DOUBLE PRECISION      FIRST */
/*              DOUBLE PRECISION      LAST */

/*              INTEGER               HANDLE */
/*              INTEGER               I */
/*              INTEGER               N */
/*              INTEGER               NCOMCH */

/*        C */
/*        C     These are the variables that will hold the constants */
/*        C     required by SPK type 10. These constants are available */
/*        C     from the loaded PCK file, which provides the actual */
/*        C     values and units as used by NORAD propagation model. */
/*        C */
/*        C        Constant   Meaning */
/*        C        --------   ------------------------------------------ */
/*        C        J2         J2 gravitational harmonic for Earth. */
/*        C        J3         J3 gravitational harmonic for Earth. */
/*        C        J4         J4 gravitational harmonic for Earth. */
/*        C        KE         Square root of the GM for Earth. */
/*        C        QO         High altitude bound for atmospheric model. */
/*        C        SO         Low altitude bound for atmospheric model. */
/*        C        ER         Equatorial radius of the Earth. */
/*        C        AE         Distance units/earth radius. */
/*        C */
/*              DATA          NOADPN  /  'J2', 'J3', 'J4', 'KE', */
/*             .                         'QO', 'SO', 'ER', 'AE'  / */

/*        C */
/*        C     Define the Two-Line Element sets. */
/*        C */
/*              TLE(1)  = '1 18123U 87 53  A 87324.61041692 -.00000023' */
/*             .      //                   '  00000-0 -75103-5 0 00675' */
/*              TLE(2)  = '2 18123  98.8296 152.0074 0014950 168.7820 ' */
/*             .      //                   '191.3688 14.12912554 21686' */
/*              TLE(3)  = '1 18123U 87 53  A 87326.73487726  .00000045' */
/*             .      //                   '  00000-0  28709-4 0 00684' */
/*              TLE(4)  = '2 18123  98.8335 154.1103 0015643 163.5445 ' */
/*             .      //                   '196.6235 14.12912902 21988' */
/*              TLE(5)  = '1 18123U 87 53  A 87331.40868801  .00000104' */
/*             .      //                   '  00000-0  60183-4 0 00690' */
/*              TLE(6)  = '2 18123  98.8311 158.7160 0015481 149.9848 ' */
/*             .      //                   '210.2220 14.12914624 22644' */
/*              TLE(7)  = '1 18123U 87 53  A 87334.24129978  .00000086' */
/*             .      //                   '  00000-0  51111-4 0 00702' */
/*              TLE(8)  = '2 18123  98.8296 161.5054 0015372 142.4159 ' */
/*             .      //                   '217.8089 14.12914879 23045' */
/*              TLE(9)  = '1 18123U 87 53  A 87336.93227900 -.00000107' */
/*             .      //                   '  00000-0 -52860-4 0 00713' */
/*              TLE(10) = '2 18123  98.8317 164.1627 0014570 135.9191 ' */
/*             .      //                   '224.2321 14.12910572 23425' */
/*              TLE(11) = '1 18123U 87 53  A 87337.28635487  .00000173' */
/*             .      //                   '  00000-0  10226-3 0 00726' */
/*              TLE(12) = '2 18123  98.8284 164.5113 0015289 133.5979 ' */
/*             .      //                   '226.6438 14.12916140 23475' */
/*              TLE(13) = '1 18123U 87 53  A 87339.05673569  .00000079' */
/*             .      //                   '  00000-0  47069-4 0 00738' */
/*              TLE(14) = '2 18123  98.8288 166.2585 0015281 127.9985 ' */
/*             .      //                   '232.2567 14.12916010 24908' */
/*              TLE(15) = '1 18123U 87 53  A 87345.43010859  .00000022' */
/*             .      //                   '  00000-0  16481-4 0 00758' */
/*              TLE(16) = '2 18123  98.8241 172.5226 0015362 109.1515 ' */
/*             .      //                   '251.1323 14.12915487 24626' */
/*              TLE(17) = '1 18123U 87 53  A 87349.04167543  .00000042' */
/*             .      //                   '  00000-0  27370-4 0 00764' */
/*              TLE(18) = '2 18123  98.8301 176.1010 0015565 100.0881 ' */
/*             .      //                   '260.2047 14.12916361 25138' */

/*        C */
/*        C     Load the PCK file that provides the geophysical */
/*        C     constants required for the evaluation of the two-line */
/*        C     elements sets. Load also an LSK, as it is required by */
/*        C     GETELM to perform time conversions. Use a metakernel for */
/*        C     convenience. */
/*        C */
/*              CALL FURNSH ( 'spkw10_ex1.tm' ) */

/*        C */
/*        C     Retrieve the data from the kernel, and place it on */
/*        C     the CONSTS array. */
/*        C */
/*              DO I = 1, 8 */

/*                 CALL BODVCD ( CENTER, NOADPN(I), 1, N, CONSTS(I) ) */

/*              END DO */

/*        C */
/*        C     Convert the Two Line Elements lines to the */
/*        C     element sets. */
/*        C */
/*              DO I = 1, TLESSZ */

/*                 CALL GETELM ( 1950,      TLE( (I-1)*2 + 1 ), */
/*             .                 EPOCHS(I), ELEMS( (I-1)*10 + 1 ) ) */

/*              END DO */

/*        C */
/*        C     Define the beginning and end of the segment to be */
/*        C     -/+ 12 hours from the first and last epochs, */
/*        C     respectively. */
/*        C */
/*              FIRST = EPOCHS(1     ) - 0.5D0 * SPD() */
/*              LAST  = EPOCHS(TLESSZ) + 0.5D0 * SPD() */

/*        C */
/*        C     NCOMCH is the number of characters to reserve for the */
/*        C     kernel's comment area. This example doesn't write */
/*        C     comments, so set to zero. */
/*        C */
/*              NCOMCH = 0 */

/*        C */
/*        C     Internal file name and segment ID. */
/*        C */
/*              IFNAME = 'Test for type 10 SPK internal file name' */
/*              SEGID  = 'SPK type 10 test segment' */

/*        C */
/*        C     Open a new SPK file. */
/*        C */
/*              CALL SPKOPN( SPK10, IFNAME, NCOMCH, HANDLE ) */

/*        C */
/*        C     Now add the segment. */
/*        C */
/*              CALL SPKW10 ( HANDLE, BODY,  CENTER, FRMNAM, */
/*             .              FIRST,  LAST,  SEGID,  CONSTS, */
/*             .              TLESSZ, ELEMS, EPOCHS         ) */

/*        C */
/*        C     Close the SPK file. */
/*        C */
/*              CALL SPKCLS ( HANDLE ) */

/*              END */


/*        When this program is executed, no output is presented on */
/*        screen. After run completion, a new SPK type 10 exists in */
/*        the output directory. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     [1]  F. Hoots and R. Roehrich, "Spacetrack Report #3: Models for */
/*          Propagation of the NORAD Element Sets," U.S. Air Force */
/*          Aerospace Defense Command, Colorado Springs, CO, 1980. */

/*     [2]  F. Hoots, "Spacetrack Report #6: Models for Propagation of */
/*          Space Command Element Sets,"  U.S. Air Force Aerospace */
/*          Defense Command, Colorado Springs, CO, 1986. */

/*     [3]  F. Hoots, P. Schumacher and R. Glover, "History of Analytical */
/*          Orbit Modeling in the U. S. Space Surveillance System," */
/*          Journal of Guidance, Control, and Dynamics. 27(2):174-185, */
/*          2004. */

/*     [4]  D. Vallado, P. Crawford, R. Hujsak and T. Kelso, "Revisiting */
/*          Spacetrack Report #3," paper AIAA 2006-6753 presented at the */
/*          AIAA/AAS Astrodynamics Specialist Conference, Keystone, CO., */
/*          August 21-24, 2006. */

/* $ Author_and_Institution */

/*     M. Costa Sitja     (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 04-NOV-2021 (JDR) (MCS) */

/*        Added IMPLICIT NONE statement. */

/*        Corrected the expected order of QO, SO and ER in the detailed */
/*        description of the input argument GEOPHS and the input element */
/*        names in ELEMS. */

/*        Added Spacetrack Report #3 to literature references and */
/*        NAIF_IDS to the list of required readings. */

/*        Edited the header to comply with NAIF standard. Added complete */
/*        code example based on existing example. */

/* -    SPICELIB Version 1.0.2, 30-OCT-2006 (BVS) */

/*        Deleted "inertial" from the FRAME description in the $Brief_I/O */
/*        section of the header. */

/* -    SPICELIB Version 1.0.1, 21-JUN-1999 (WLT) */

/*        Cleaned up the header. */

/* -    SPICELIB Version 1.0.0, 05-JAN-1994 (WLT) */

/* -& */
/* $ Index_Entries */

/*     Write a type 10 SPK segment */

/* -& */

/*     Spicelib functions */


/*     Local Variables */


/*     The type of this segment */


/*     The number of geophysical constants: */


/*     The number of elements per two-line set: */


/*     Standard SPICELIB error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("SPKW10", (ftnlen)6);

/*     First we need to create a descriptor for the segment */
/*     we are about to write. */

    spkpds_(body, center, frame, &c__10, first, last, descr, frame_len);
    if (failed_()) {
	chkout_("SPKW10", (ftnlen)6);
	return 0;
    }

/*     We've got a valid descriptor, write the data to a DAF */
/*     segment using the generic segment writer. */

    npkts = *n;
    nepoch = *n;
    sgbwfs_(handle, descr, segid, &c__8, consts, &c__14, &c__4, segid_len);
    i__1 = nepoch;
    for (i__ = 1; i__ <= i__1; ++i__) {

/*        Move the elements into the next packet. */

	base = (i__ - 1) * 10;
	moved_(&elems[base], &c__10, packet);

/*        For each epoch, we need to get the nutation in obliquity, */
/*        nutation in longitude and mean obliquity. */

	zzwahr_(&epochs[i__ - 1], dnut);
	packet[11] = dnut[0];
	packet[10] = dnut[1];
	packet[13] = dnut[2];
	packet[12] = dnut[3];

/*        Now write the packet into the generic segment. */

	sgwfpk_(handle, &c__1, packet, &c__1, &epochs[i__ - 1]);
    }
    sgwes_(handle);
    chkout_("SPKW10", (ftnlen)6);
    return 0;
} /* spkw10_ */

