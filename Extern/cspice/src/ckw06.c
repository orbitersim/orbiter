/* ckw06.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__196 = 196;
static integer c__340 = 340;
static integer c__23 = 23;
static integer c__4 = 4;
static integer c__2 = 2;
static integer c__6 = 6;
static integer c__1 = 1;

/* $Procedure CKW06 ( CK, Write segment, type 6 ) */
/* Subroutine */ int ckw06_(integer *handle, integer *inst, char *ref, 
	logical *avflag, doublereal *first, doublereal *last, char *segid, 
	integer *nmini, integer *npkts, integer *subtps, integer *degres, 
	doublereal *packts, doublereal *rates, doublereal *sclkdp, doublereal 
	*ivlbds, logical *sellst, ftnlen ref_len, ftnlen segid_len)
{
    /* Initialized data */

    static integer pktszs[4] = { 8,4,14,7 };

    /* System generated locals */
    integer i__1, i__2, i__3;
    doublereal d__1;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer addr__;
    doublereal qneg[4];
    integer isel;
    extern logical even_(integer *);
    integer ndir, i__, j, k;
    doublereal q[4];
    extern /* Subroutine */ int chkin_(char *, ftnlen), dafps_(integer *, 
	    integer *, doublereal *, integer *, doublereal *);
    doublereal descr[5];
    extern /* Subroutine */ int errch_(char *, char *, ftnlen, ftnlen);
    integer bepix, eepix;
    extern /* Subroutine */ int errdp_(char *, doublereal *, ftnlen), moved_(
	    doublereal *, integer *, doublereal *);
    doublereal prevq[4];
    extern /* Subroutine */ int dafada_(doublereal *, integer *);
    doublereal dc[2];
    extern /* Subroutine */ int dafbna_(integer *, doublereal *, char *, 
	    ftnlen);
    integer ic[6];
    extern logical failed_(void);
    extern /* Subroutine */ int dafena_(void);
    integer segbeg, chrcod, refcod, segend, pktbeg;
    extern /* Subroutine */ int namfrm_(char *, integer *, ftnlen);
    extern integer lastnb_(char *, ftnlen);
    integer pktend;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    extern doublereal vdistg_(doublereal *, doublereal *, integer *);
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen), vminug_(doublereal *, integer *, doublereal *)
	    ;
    integer minisz;
    extern logical vzerog_(doublereal *, integer *), return_(void);
    integer pktdsz, winsiz, pktsiz, subtyp;
    extern logical odd_(integer *);

/* $ Abstract */

/*     Write a type 6 segment to a CK file. */

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
/*     NAIF_IDS */
/*     SCLK */
/*     SPC */
/*     TIME */

/* $ Keywords */

/*     ATTITUDE */
/*     FILES */
/*     POINTING */

/* $ Declarations */
/* $ Abstract */

/*     Declare parameters specific to CK type 06. */

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

/* $ Keywords */

/*     CK */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman      (JPL) */
/*     B.V. Semenov      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 10-MAR-2014 (NJB) (BVS) */

/* -& */

/*     Maximum polynomial degree supported by the current */
/*     implementation of this CK type. */


/*     Integer code indicating `true': */


/*     Integer code indicating `false': */


/*     CK type 6 subtype codes: */


/*     Subtype 0:  Hermite interpolation, 8-element packets. Quaternion */
/*                 and quaternion derivatives only, no angular velocity */
/*                 vector provided. Quaternion elements are listed */
/*                 first, followed by derivatives. Angular velocity is */
/*                 derived from the quaternions and quaternion */
/*                 derivatives. */


/*     Subtype 1:  Lagrange interpolation, 4-element packets. Quaternion */
/*                 only. Angular velocity is derived by differentiating */
/*                 the interpolating polynomials. */


/*     Subtype 2:  Hermite interpolation, 14-element packets. */
/*                 Quaternion and angular angular velocity vector, as */
/*                 well as derivatives of each, are provided. The */
/*                 quaternion comes first, then quaternion derivatives, */
/*                 then angular velocity and its derivatives. */


/*     Subtype 3:  Lagrange interpolation, 7-element packets. Quaternion */
/*                 and angular velocity vector provided.  The quaternion */
/*                 comes first. */


/*     Number of subtypes: */


/*     Packet sizes associated with the various subtypes: */


/*     Maximum packet size for type 6: */


/*     Minimum packet size for type 6: */


/*     The CKPFS record size declared in ckparam.inc must be at least as */
/*     large as the maximum possible size of a CK type 6 record. */

/*     The largest possible CK type 6 record has subtype 3 (note that */
/*     records of subtype 2 have half as many epochs as those of subtype */
/*     3, for a given polynomial degree). A subtype 3 record contains */

/*        - The evaluation epoch */
/*        - The subtype and packet count */
/*        - MAXDEG+1 packets of size C06PS3 */
/*        - MAXDEG+1 time tags */


/*     End of file ck06.inc. */

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
/*     HANDLE     I   Handle of a CK file open for writing. */
/*     INST       I   NAIF instrument ID code. */
/*     REF        I   Reference frame name. */
/*     AVFLAG     I   Flag indicating if the segment will contain angular */
/*                    velocity. */
/*     FIRST      I   Start time of interval covered by segment. */
/*     LAST       I   End time of interval covered by segment. */
/*     SEGID      I   Segment identifier. */
/*     NMINI      I   Number of mini-segments. */
/*     NPKTS      I   Array of packet counts of mini-segments. */
/*     SUBTPS     I   Array of segment subtypes of mini-segments. */
/*     DEGRES     I   Array of polynomial degrees of mini-segments. */
/*     PACKTS     I   Array of data packets of mini-segments. */
/*     RATES      I   Nominal SCLK rates in seconds per tick. */
/*     SCLKDP     I   Array of epochs of mini-segments. */
/*     IVLBDS     I   Mini-segment interval bounds. */
/*     SELLST     I   Interval selection flag. */
/*     MAXDEG     P   Maximum allowed degree of interpolating polynomial. */

/* $ Detailed_Input */

/*     HANDLE   is the handle of a CK file that has been opened */
/*              for writing. */


/*     INST     is a NAIF integer code associated with an */
/*              instrument or spacecraft structure whose */
/*              orientation is described by the segment to be */
/*              created. INST is treated by the SPICE frame */
/*              subsystem as a CK frame class ID (see the */
/*              Frames Required Reading for details). */


/*     AVFLAG   is a logical flag which indicates whether or not */
/*              the segment will contain angular velocity. */


/*     REF      is the NAIF name for a reference frame relative to */
/*              which the pointing (attitude) information for INST */
/*              is specified. */

/*     FIRST, */
/*     LAST     are, respectively, the bounds of the time interval */
/*              over which the segment defines the attitude of */
/*              INST. FIRST and LAST are encoded SCLK times. */

/*              FIRST must be greater than or equal to the first */
/*              mini-segment interval start time; LAST must be */
/*              less than or equal to the last mini-segment */
/*              interval stop time. See the description of IVLBDS */
/*              below. */


/*     SEGID    is the segment identifier. A CK segment */
/*              identifier may contain up to 40 characters. */


/*     NMINI    is the number of mini-segments comprised by */
/*              the input data. Each mini-segment contains data */
/*              that could be stored in a type 5 segment. */
/*              The parameters and data of a mini-segment are: */

/*                 - a packet count */
/*                 - a type 6 subtype */
/*                 - an interpolating polynomial degree */
/*                 - a nominal SCLK rate in seconds/tick */
/*                 - a sequence of type 6 data packets */
/*                 - a sequence of packet epochs */

/*              These inputs are described below. */


/*     NPKTS    is an array of packet counts. The Ith element of */
/*              NPKTS is the packet count of the Ith mini-segment. */

/*              NPKTS has dimension NMINI. */


/*     SUBTPS   is an array of type 6 subtypes. The Ith element of */
/*              SUBTPS is the subtype of the packets associated */
/*              with the Ith mini-segment. */

/*              SUBTPS has dimension NMINI. */


/*     DEGRES   is an array of interpolating polynomial degrees. */
/*              The Ith element of DEGRES is the polynomial degree */
/*              of the packets associated with the Ith */
/*              mini-segment. */

/*              For subtypes 0 and 2, interpolation degrees must be */
/*              equivalent to 3 mod 4, that is, they must be in */
/*              the set */

/*                 { 3, 7, 11, ..., MAXDEG } */

/*              For subtypes 1 and 3, interpolation degrees must */
/*              be odd and must be in the range 1:MAXDEG. */

/*              DEGRES has dimension NMINI. */


/*     PACKTS   is an array of data packets representing the */
/*              orientation of INST relative to the frame REF. The */
/*              packets for a given mini-segment are stored */
/*              contiguously in increasing time order. The order */
/*              of the sets of packets for different mini-segments */
/*              is the same as the order of their corresponding */
/*              mini-segment intervals. */

/*              Each packet contains a SPICE-style quaternion and */
/*              optionally, depending on the segment subtype, */
/*              attitude derivative data, from which a C-matrix */
/*              and an angular velocity vector may be derived. */

/*              See the discussion of quaternion styles in */
/*              $Particulars below. */

/*              The C-matrix CMAT represented by the Ith data */
/*              packet is a rotation matrix that transforms the */
/*              components of a vector expressed in the base frame */
/*              specified by REF to components expressed in the */
/*              instrument fixed frame at the time SCLKDP(I). */

/*              Thus, if a vector V has components x, y, z in the */
/*              base frame, then V has components x', y', z' */
/*              in the instrument fixed frame where: */

/*                 [ x' ]     [          ] [ x ] */
/*                 | y' |  =  |   CMAT   | | y | */
/*                 [ z' ]     [          ] [ z ] */

/*              Attitude derivative information either explicitly */
/*              contained in, or else derived from, PACKTS(I) */
/*              gives the angular velocity of the instrument fixed */
/*              frame at time SCLKDP(I) with respect to the */
/*              reference frame specified by REF. */

/*              The direction of an angular velocity vector gives */
/*              the right-handed axis about which the instrument */
/*              fixed reference frame is rotating. The magnitude */
/*              of the vector is the magnitude of the */
/*              instantaneous velocity of the rotation, in radians */
/*              per second. */

/*              Packet contents and the corresponding */
/*              interpolation methods depend on the segment */
/*              subtype, and are as follows: */

/*                 Subtype 0:  Hermite interpolation, 8-element */
/*                             packets. Quaternion and quaternion */
/*                             derivatives only, no angular */
/*                             velocity vector provided. */
/*                             Quaternion elements are listed */
/*                             first, followed by derivatives. */
/*                             Angular velocity is derived from */
/*                             the quaternions and quaternion */
/*                             derivatives. */

/*                 Subtype 1:  Lagrange interpolation, 4-element */
/*                             packets. Quaternion only. Angular */
/*                             velocity is derived by */
/*                             differentiating the interpolating */
/*                             polynomials. */

/*                 Subtype 2:  Hermite interpolation, 14-element */
/*                             packets. Quaternion and angular */
/*                             velocity vector, as well as */
/*                             derivatives of each, are provided. */
/*                             The quaternion comes first, then */
/*                             quaternion derivatives, then */
/*                             angular velocity and its */
/*                             derivatives. */

/*                 Subtype 3:  Lagrange interpolation, 7-element */
/*                             packets. Quaternion and angular */
/*                             velocity vector provided. The */
/*                             quaternion comes first. */

/*              Angular velocity is always specified relative to */
/*              the base frame. */

/*              Units of the input data are: */

/*                 Quaternions                unitless */
/*                 Quaternion derivatives     1/TDB second */
/*                 Angular velocity           radians/TDB second */
/*                 Angular acceleration       radians/TDB second**2 */

/*              For the Hermite subtypes (0 and 2), quaternion */
/*              representations must be selected so that, for */
/*              consecutive quaternions Q(I) and Q(I+1) in a */
/*              mini-segment, the distance between Q and Q(I+1) is */
/*              less than the distance between Q and -Q(I+1). The */
/*              Lagrange subtypes do not have this restriction. */


/*     RATES    is an array of nominal rates of the spacecraft */
/*              clock associated with INST. The Ith element of */
/*              rates is the clock rate for the packets associated */
/*              with the Ith mini-segment. Units are seconds per */
/*              tick. Spacecraft clock rates are used to scale */
/*              angular velocity to radians/second. */


/*     SCLKDP   is an array containing epochs for all input */
/*              mini-segments. The epochs have a one-to-one */
/*              relationship with the packets in the input */
/*              packet array. The epochs are encoded SCLK times. */

/*              The epochs for a given mini-segment are stored */
/*              contiguously in increasing order. The order of the */
/*              sets of epochs for different mini-segments is the */
/*              same as the order of their corresponding */
/*              mini-segment intervals. */

/*              For each mini-segment, "padding" is allowed: the */
/*              sequence of epochs for that mini-segment may start */
/*              before the corresponding mini-segment interval */
/*              start time and end after the corresponding */
/*              mini-segment interval stop time. Padding is used */
/*              to control behavior of interpolating polynomials */
/*              near mini-segment interval boundaries. */

/*              Due to possible use of padding, the elements of */
/*              SCLKDP, taken as a whole, might not be in */
/*              increasing order. */


/*     IVLBDS   is an array of mini-segment interval boundary */
/*              times. This array is a strictly increasing list of */
/*              the mini-segment interval start times, to which */
/*              the end time for the last interval is appended. */
/*              The interval bounds are encoded SCLK times. */

/*              The Ith mini-segment interval is the time */
/*              coverage interval of the Ith mini-segment (see the */
/*              description of NPKTS above). */

/*              For each mini-segment, the corresponding */
/*              mini-segment interval's start time is greater */
/*              than or equal to the mini-segment's first epoch. */
/*              The interval's stop time may exceed the */
/*              mini-segment's last epoch, allowing a single */
/*              coverage gap to exist between a mini-segment's */
/*              last epoch and its interval stop time. */

/*              The "interpolation interval" of the ith */
/*              mini-segment is contained in the ith mini-segment */
/*              interval: the interpolation interval extends from */
/*              IVLBDS(I) to the minimum of IVLBDS(I+1) and the */
/*              last epoch of the mini-segment. */

/*              For each mini-segment interval other than the */
/*              last, the interval's coverage stop time coincides */
/*              with the coverage start time of the next interval. */

/*              IVLBDS has dimension NMINI+1. */


/*     SELLST   is a logical flag indicating to the CK type 6 */
/*              segment reader CKR06 how to select the */
/*              mini-segment interval when a request time */
/*              coincides with a time boundary shared by two */
/*              mini-segment intervals. When SELLST ("select */
/*              last") is .TRUE., the later interval is selected; */
/*              otherwise the earlier interval is selected. */

/* $ Detailed_Output */

/*     None. See $Particulars for a description of the effect of this */
/*     routine. */

/* $ Parameters */

/*     MAXDEG   is the maximum allowed degree of the interpolating */
/*              polynomial. */

/*              See the INCLUDE file ck06.inc for the value of */
/*              MAXDEG. */

/* $ Exceptions */

/*     If any of the following exceptions occur, this routine will */
/*     return without creating a new segment. */


/*     1)  If FIRST is greater than LAST, the error */
/*         SPICE(BADDESCRTIMES) is signaled. */

/*     2)  If REF is not a recognized name, the error */
/*         SPICE(INVALIDREFFRAME) is signaled. */

/*     3)  If the last non-blank character of SEGID occurs past index */
/*         40, the error SPICE(SEGIDTOOLONG) is signaled. */

/*     4)  If SEGID contains any nonprintable characters, the error */
/*         SPICE(NONPRINTABLECHARS) is signaled. */

/*     5)  If NMINI is not at least 1, the error SPICE(INVALIDCOUNT) */
/*         is signaled. */

/*     6)  If the elements of the array IVLBDS are not in strictly */
/*         increasing order, the error SPICE(BOUNDSOUTOFORDER) is */
/*         signaled. */

/*     7)  If the first interval start time IVLBDS(1) is greater than */
/*         FIRST, or if the last interval end time IVLBDS(NMINI+1) is */
/*         less than LAST, the error SPICE(COVERAGEGAP) is signaled. */

/*     8)  If any packet count in the array NPKTS is not at least 2, the */
/*         error SPICE(TOOFEWPACKETS) is signaled. */

/*     9)  If any subtype code in the array SUBTPS is not recognized, */
/*         the error SPICE(INVALIDSUBTYPE) is signaled. */

/*     10) If any interpolation degree in the array DEGRES is not at */
/*         least 1 or is greater than MAXDEG, the error */
/*         SPICE(INVALIDDEGREE) is signaled. */

/*     11) If the window size implied by any element of the array DEGRES */
/*         is odd, the error SPICE(BADWINDOWSIZE) is signaled. */

/*     12) If the elements of the array SCLKDP corresponding to a given */
/*         mini-segment are not in strictly increasing order, the error */
/*         SPICE(TIMESOUTOFORDER) is signaled. */

/*     13) If the first epoch of a mini-segment exceeds the start time */
/*         of the associated mini-segment interval, or if the last */
/*         epoch of a mini-segment is less than the interval start */
/*         time, the error SPICE(BOUNDSDISAGREE) is signaled. However, */
/*         the last epoch of a mini-segment may be less than the end */
/*         time of the corresponding mini-segment interval. */

/*     14) If any quaternion has magnitude zero, the error */
/*         SPICE(ZEROQUATERNION) is signaled. */

/*     15) If an error occurs while writing the output segment, the error */
/*         is signaled by a routine in the call tree of this routine. */

/*     16) This routine assumes that the rotation between adjacent */
/*         quaternions that are stored in the same interval has a */
/*         rotation angle of THETA radians, where */

/*            0  <  THETA  <  pi. */
/*               _ */

/*         The routines that evaluate the data in the segment produced */
/*         by this routine cannot distinguish between rotations of THETA */
/*         radians, where THETA is in the interval [0, pi), and */
/*         rotations of */

/*            THETA   +   2 * k * pi */

/*         radians, where k is any integer. These "large" rotations will */
/*         yield invalid results when interpolated. The segment creator */
/*         must ensure that the data stored in the segment will not be */
/*         subject to this sort of ambiguity. */

/*     17) For the Hermite subtypes (0 and 2), quaternion */
/*         representations must be selected so that, for consecutive */
/*         quaternions Q(I) and Q(I+1) in a mini-segment, the distance */
/*         between Q and Q(I+1) is less than the distance between Q and */
/*         -Q(I+1). */

/*         If a pair of quaternions violating this condition is found in */
/*         the input array PACKTS, the error SPICE(BADQUATSIGN) is */
/*         signaled. */

/*     18) If any element of the input RATES array is non-positive, the */
/*         error SPICE(INVALIDSCLKRATE) is signaled. */

/* $ Files */

/*     A new type 6 CK segment is written to the CK file attached */
/*     to HANDLE. */

/* $ Particulars */

/*     This routine writes a CK type 6 data segment to the open CK */
/*     file according to the format described in the type 6 section of */
/*     the CK Required Reading. The CK file must have been opened with */
/*     write access. */


/*     Quaternion Styles */
/*     ----------------- */

/*     There are different "styles" of quaternions used in */
/*     science and engineering applications. Quaternion styles */
/*     are characterized by */

/*     -  The order of quaternion elements */

/*     -  The quaternion multiplication formula */

/*     -  The convention for associating quaternions */
/*        with rotation matrices */

/*     Two of the commonly used styles are */

/*        - "SPICE" */

/*           > Invented by Sir William Rowan Hamilton */
/*           > Frequently used in mathematics and physics textbooks */

/*        - "Engineering" */

/*           > Widely used in aerospace engineering applications */


/*     SPICELIB subroutine interfaces ALWAYS use SPICE quaternions. */
/*     Quaternions of any other style must be converted to SPICE */
/*     quaternions before they are passed to SPICELIB routines. */


/*     Relationship between SPICE and Engineering Quaternions */
/*     ------------------------------------------------------ */

/*     Let M be a rotation matrix such that for any vector V, */

/*        M*V */

/*     is the result of rotating V by theta radians in the */
/*     counterclockwise direction about unit rotation axis vector A. */
/*     Then the SPICE quaternions representing M are */

/*        (+/-) (  cos(theta/2), */
/*                 sin(theta/2) A(1), */
/*                 sin(theta/2) A(2), */
/*                 sin(theta/2) A(3)  ) */

/*     while the engineering quaternions representing M are */

/*        (+/-) ( -sin(theta/2) A(1), */
/*                -sin(theta/2) A(2), */
/*                -sin(theta/2) A(3), */
/*                 cos(theta/2)       ) */

/*     For both styles of quaternions, if a quaternion q represents */
/*     a rotation matrix M, then -q represents M as well. */

/*     Given an engineering quaternion */

/*        QENG   = ( q0,  q1,  q2,  q3 ) */

/*     the equivalent SPICE quaternion is */

/*        QSPICE = ( q3, -q0, -q1, -q2 ) */


/*     Associating SPICE Quaternions with Rotation Matrices */
/*     ---------------------------------------------------- */

/*     Let FROM and TO be two right-handed reference frames, for */
/*     example, an inertial frame and a spacecraft-fixed frame. Let the */
/*     symbols */

/*        V    ,   V */
/*         FROM     TO */

/*     denote, respectively, an arbitrary vector expressed relative to */
/*     the FROM and TO frames. Let M denote the transformation matrix */
/*     that transforms vectors from frame FROM to frame TO; then */

/*        V   =  M * V */
/*         TO         FROM */

/*     where the expression on the right hand side represents left */
/*     multiplication of the vector by the matrix. */

/*     Then if the unit-length SPICE quaternion q represents M, where */

/*        q = (q0, q1, q2, q3) */

/*     the elements of M are derived from the elements of q as follows: */

/*          +-                                                         -+ */
/*          |           2    2                                          | */
/*          | 1 - 2*( q2 + q3 )   2*(q1*q2 - q0*q3)   2*(q1*q3 + q0*q2) | */
/*          |                                                           | */
/*          |                                                           | */
/*          |                               2    2                      | */
/*      M = | 2*(q1*q2 + q0*q3)   1 - 2*( q1 + q3 )   2*(q2*q3 - q0*q1) | */
/*          |                                                           | */
/*          |                                                           | */
/*          |                                                   2    2  | */
/*          | 2*(q1*q3 - q0*q2)   2*(q2*q3 + q0*q1)   1 - 2*( q1 + q2 ) | */
/*          |                                                           | */
/*          +-                                                         -+ */

/*     Note that substituting the elements of -q for those of q in the */
/*     right hand side leaves each element of M unchanged; this shows */
/*     that if a quaternion q represents a matrix M, then so does the */
/*     quaternion -q. */

/*     To map the rotation matrix M to a unit quaternion, we start by */
/*     decomposing the rotation matrix as a sum of symmetric */
/*     and skew-symmetric parts: */

/*                                        2 */
/*        M = [ I  +  (1-cos(theta)) OMEGA  ] + [ sin(theta) OMEGA ] */

/*                     symmetric                   skew-symmetric */


/*     OMEGA is a skew-symmetric matrix of the form */

/*                   +-             -+ */
/*                   |  0   -n3   n2 | */
/*                   |               | */
/*         OMEGA  =  |  n3   0   -n1 | */
/*                   |               | */
/*                   | -n2   n1   0  | */
/*                   +-             -+ */

/*     The vector N of matrix entries (n1, n2, n3) is the rotation axis */
/*     of M and theta is M's rotation angle. Note that N and theta */
/*     are not unique. */

/*     Let */

/*        C = cos(theta/2) */
/*        S = sin(theta/2) */

/*     Then the unit quaternions Q corresponding to M are */

/*        Q = +/- ( C, S*n1, S*n2, S*n3 ) */

/*     The mappings between quaternions and the corresponding rotations */
/*     are carried out by the SPICELIB routines */

/*        Q2M {quaternion to matrix} */
/*        M2Q {matrix to quaternion} */

/*     M2Q always returns a quaternion with scalar part greater than */
/*     or equal to zero. */


/*     SPICE Quaternion Multiplication Formula */
/*     --------------------------------------- */

/*     Given a SPICE quaternion */

/*        Q = ( q0, q1, q2, q3 ) */

/*     corresponding to rotation axis A and angle theta as above, we can */
/*     represent Q using "scalar + vector" notation as follows: */

/*        s =   q0           = cos(theta/2) */

/*        v = ( q1, q2, q3 ) = sin(theta/2) * A */

/*        Q = s + v */

/*     Let Q1 and Q2 be SPICE quaternions with respective scalar */
/*     and vector parts s1, s2 and v1, v2: */

/*        Q1 = s1 + v1 */
/*        Q2 = s2 + v2 */

/*     We represent the dot product of v1 and v2 by */

/*        <v1, v2> */

/*     and the cross product of v1 and v2 by */

/*        v1 x v2 */

/*     Then the SPICE quaternion product is */

/*        Q1*Q2 = s1*s2 - <v1,v2>  + s1*v2 + s2*v1 + (v1 x v2) */

/*     If Q1 and Q2 represent the rotation matrices M1 and M2 */
/*     respectively, then the quaternion product */

/*        Q1*Q2 */

/*     represents the matrix product */

/*        M1*M2 */

/* $ Examples */

/*     Suppose that you have states and are prepared to produce */
/*     a segment of type 6 in a CK file. */

/*     The following code fragment could be used to add the new segment */
/*     to a previously opened CK file attached to HANDLE. The file must */
/*     have been opened with write access. */

/*        C */
/*        C     Create a segment identifier. */
/*        C */
/*              SEGID = 'MY_SAMPLE_CK_TYPE_6_SEGMENT' */

/*        C */
/*        C     Write the segment. */
/*        C */
/*              CALL CKW06 ( HANDLE,  INST,    REF,     AVFLAG, */
/*             .             FIRST,   LAST,    SEGID,   NMINI, */
/*             .             NPKTS,   SUBTPS,  DEGRES,  PACKTS, */
/*             .             RATES,   SCLKDP,  IVLBDS,  SELLST  ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.1, 02-JUN-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 2.0.0, 11-AUG-2015 (NJB) */

/*        Added check for invalid SCLK rates. */

/*        Corrected error in header $Exceptions section: changed */
/*        subscript N+1 to NMINI+1. Corrected typo in description */
/*        of subtype 2 data. Added mention of angular acceleration */
/*        units. */

/* -    SPICELIB Version 1.0.0, 14-MAR-2014 (NJB) (BVS) */

/* -& */
/* $ Index_Entries */

/*     write SPK type_6 ephemeris data segment */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Packet structure parameters */


/*     Local variables */


/*     Saved values */


/*     Initial values */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("CKW06", (ftnlen)5);

/*     Start with a parameter compatibility check. */

    if (FALSE_) {
	setmsg_("CK type 6 record size is #, but CKPFS record size is #.is #."
		, (ftnlen)60);
	errint_("#", &c__196, (ftnlen)1);
	errint_("#", &c__340, (ftnlen)1);
	sigerr_("SPICE(BUG)", (ftnlen)10);
	chkout_("CKW06", (ftnlen)5);
	return 0;
    }

/*     Make sure the segment descriptor bounds are */
/*     correctly ordered. */

    if (*last < *first) {
	setmsg_("Segment start time is #; stop time is #; bounds must be in "
		"nondecreasing order.", (ftnlen)79);
	errdp_("#", first, (ftnlen)1);
	errdp_("#", last, (ftnlen)1);
	sigerr_("SPICE(BADDESCRTIMES)", (ftnlen)20);
	chkout_("CKW06", (ftnlen)5);
	return 0;
    }

/*     Get the NAIF integer code for the reference frame. */

    namfrm_(ref, &refcod, ref_len);
    if (refcod == 0) {
	setmsg_("The reference frame # is not supported.", (ftnlen)39);
	errch_("#", ref, (ftnlen)1, ref_len);
	sigerr_("SPICE(INVALIDREFFRAME)", (ftnlen)22);
	chkout_("CKW06", (ftnlen)5);
	return 0;
    }

/*     Check to see if the segment identifier is too long. */

    if (lastnb_(segid, segid_len) > 40) {
	setmsg_("Segment identifier contains more than 40 characters.", (
		ftnlen)52);
	sigerr_("SPICE(SEGIDTOOLONG)", (ftnlen)19);
	chkout_("CKW06", (ftnlen)5);
	return 0;
    }

/*     Now check that all the characters in the segment identifier */
/*     can be printed. */

    i__1 = lastnb_(segid, segid_len);
    for (i__ = 1; i__ <= i__1; ++i__) {
	chrcod = *(unsigned char *)&segid[i__ - 1];
	if (chrcod < 32 || chrcod > 126) {
	    setmsg_("The segment identifier contains nonprintable characters",
		     (ftnlen)55);
	    sigerr_("SPICE(NONPRINTABLECHARS)", (ftnlen)24);
	    chkout_("CKW06", (ftnlen)5);
	    return 0;
	}
    }

/*     The mini-segment count must be positive. */

    if (*nmini < 1) {
	setmsg_("Mini-segment count was #; this count must be positive.", (
		ftnlen)54);
	errint_("#", nmini, (ftnlen)1);
	sigerr_("SPICE(INVALIDCOUNT)", (ftnlen)19);
	chkout_("CKW06", (ftnlen)5);
	return 0;
    }

/*     Make sure the interval bounds form a strictly */
/*     increasing sequence. */

/*     Note that there are NMINI+1 bounds. */

    i__1 = *nmini;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (ivlbds[i__ - 1] >= ivlbds[i__]) {
	    setmsg_("Mini-segment interval bounds at indices # and # are # a"
		    "nd # respectively. The difference is #. The bounds are r"
		    "equired to be strictly increasing.", (ftnlen)145);
	    errint_("#", &i__, (ftnlen)1);
	    i__2 = i__ + 1;
	    errint_("#", &i__2, (ftnlen)1);
	    errdp_("#", &ivlbds[i__ - 1], (ftnlen)1);
	    errdp_("#", &ivlbds[i__], (ftnlen)1);
	    d__1 = ivlbds[i__] - ivlbds[i__ - 1];
	    errdp_("#", &d__1, (ftnlen)1);
	    sigerr_("SPICE(BOUNDSOUTOFORDER)", (ftnlen)23);
	    chkout_("CKW06", (ftnlen)5);
	    return 0;
	}
    }

/*     Make sure the time span of the descriptor doesn't extend */
/*     beyond the span of the interval bounds. */

    if (*first < ivlbds[0] || *last > ivlbds[*nmini]) {
	setmsg_("First mini-segment interval start time is #; segment start "
		"time is #; segment stop time is #; last mini-segment interva"
		"l stop time is #. This sequence of times is required to be n"
		"on-decreasing: segment coverage must be contained within the"
		" union of the mini-segment intervals.", (ftnlen)276);
	errdp_("#", ivlbds, (ftnlen)1);
	errdp_("#", first, (ftnlen)1);
	errdp_("#", last, (ftnlen)1);
	errdp_("#", &ivlbds[*nmini], (ftnlen)1);
	sigerr_("SPICE(COVERAGEGAP)", (ftnlen)18);
	chkout_("CKW06", (ftnlen)5);
	return 0;
    }

/*     Check the input data before writing to the file. */

/*     This order of operations entails some redundant */
/*     calculations, but it allows for rapid error */
/*     detection. */

/*     Initialize the mini-segment packet array indices, */
/*     and those of the mini-segment epoch array as well. */

    pktbeg = 0;
    pktend = 0;
    bepix = 0;
    eepix = 0;
    i__1 = *nmini;
    for (i__ = 1; i__ <= i__1; ++i__) {

/*        First, just make sure the packet count for the current */
/*        mini-segment is at least two. This check reduces our chances */
/*        of a subscript range violation. */

/*        Check the number of packets. */

	if (npkts[i__ - 1] < 2) {
	    setmsg_("At least 2 packets are required for CK type 6. Number o"
		    "f packets supplied was # in mini-segment at index #.", (
		    ftnlen)107);
	    errint_("#", &npkts[i__ - 1], (ftnlen)1);
	    errint_("#", &i__, (ftnlen)1);
	    sigerr_("SPICE(TOOFEWPACKETS)", (ftnlen)20);
	    chkout_("CKW06", (ftnlen)5);
	    return 0;
	}

/*        Set the packet size, which is a function of the subtype. Also */
/*        set the window size. First check the subtype, which will be */
/*        used as an array index. */

	subtyp = subtps[i__ - 1];
	if (subtyp < 0 || subtyp > 3) {
	    setmsg_("Unexpected CK type 6 subtype # found in mini-segment #.",
		     (ftnlen)55);
	    errint_("#", &subtyp, (ftnlen)1);
	    errint_("#", &i__, (ftnlen)1);
	    sigerr_("SPICE(INVALIDSUBTYPE)", (ftnlen)21);
	    chkout_("CKW06", (ftnlen)5);
	    return 0;
	}
	pktsiz = pktszs[(i__2 = subtyp) < 4 && 0 <= i__2 ? i__2 : s_rnge(
		"pktszs", i__2, "ckw06_", (ftnlen)1026)];
	if (odd_(&subtyp)) {
	    winsiz = degres[i__ - 1] + 1;
	} else {
	    winsiz = (degres[i__ - 1] + 1) / 2;
	}

/*        Make sure the SCLK rates in this mini-segment are positive. */

	if (rates[i__ - 1] <= 0.) {
	    setmsg_("SCLK rate at index # was #; rate must be positive.", (
		    ftnlen)50);
	    errint_("#", &i__, (ftnlen)1);
	    errdp_("#", &rates[i__ - 1], (ftnlen)1);
	    sigerr_("SPICE(INVALIDSCLKRATE)", (ftnlen)22);
	    chkout_("CKW06", (ftnlen)5);
	    return 0;
	}

/*        Update the packet range pointers for this mini-segment. */

	pktbeg = pktend + 1;
	pktend = pktbeg - 1 + npkts[i__ - 1] * pktsiz;

/*        Make sure that the degree of the interpolating polynomials is */
/*        in range. */

	if (degres[i__ - 1] < 1 || degres[i__ - 1] > 23) {
	    setmsg_("The interpolating polynomials of mini-segment # have de"
		    "gree #; the valid degree range is [1, #]", (ftnlen)95);
	    errint_("#", &i__, (ftnlen)1);
	    errint_("#", &degres[i__ - 1], (ftnlen)1);
	    errint_("#", &c__23, (ftnlen)1);
	    sigerr_("SPICE(INVALIDDEGREE)", (ftnlen)20);
	    chkout_("CKW06", (ftnlen)5);
	    return 0;
	}

/*        Make sure that the window size is even. */

	if (odd_(&winsiz)) {
	    setmsg_("The interpolating polynomials of mini-segment # have wi"
		    "ndow size # and degree # for CK type 6. The mini-segment"
		    " subtype is #. The degree must be equivalent to 3 mod 4 "
		    "for subtypes 0 or 2 (Hermite interpolation) and odd for "
		    "subtypes 1 or 3 (Lagrange interpolation).", (ftnlen)264);
	    errint_("#", &i__, (ftnlen)1);
	    errint_("#", &winsiz, (ftnlen)1);
	    errint_("#", &degres[i__ - 1], (ftnlen)1);
	    errint_("#", &subtps[i__ - 1], (ftnlen)1);
	    sigerr_("SPICE(BADWINDOWSIZE)", (ftnlen)20);
	    chkout_("CKW06", (ftnlen)5);
	    return 0;
	}

/*        Make sure the epochs of the Ith mini-segment form a */
/*        strictly increasing sequence. */

/*        To start out, determine the indices of the epoch sequence */
/*        of the Ith mini-segment. We'll call the begin and end */
/*        epoch indices BEPIX and EEPIX respectively. */

	bepix = eepix + 1;
	eepix = bepix - 1 + npkts[i__ - 1];
	i__2 = npkts[i__ - 1] - 1;
	for (j = 1; j <= i__2; ++j) {
	    k = bepix + j - 1;
	    if (sclkdp[k - 1] >= sclkdp[k]) {
		setmsg_("In mini-segment #, epoch # having mini-segment-rela"
			"tive index # and array-relative index # is greater t"
			"han or equal to its successor #.", (ftnlen)135);
		errint_("#", &i__, (ftnlen)1);
		errdp_("#", &sclkdp[k - 1], (ftnlen)1);
		errint_("#", &j, (ftnlen)1);
		errint_("#", &k, (ftnlen)1);
		errdp_("#", &sclkdp[k], (ftnlen)1);
		sigerr_("SPICE(TIMESOUTOFORDER)", (ftnlen)22);
		chkout_("CKW06", (ftnlen)5);
		return 0;
	    }
	}

/*        Make sure that the span of the input epochs of the Ith */
/*        mini-segment includes the start of the Ith mini-segment */
/*        interval. Note that the stop time need not be covered, since */
/*        gaps are allowed at the right ends of mini-segment intervals. */

	if (sclkdp[bepix - 1] > ivlbds[i__ - 1]) {
	    setmsg_("Mini-segment interval # start time # precedes mini-segm"
		    "ent's first epoch #.", (ftnlen)75);
	    errint_("#", &i__, (ftnlen)1);
	    errdp_("#", &ivlbds[i__ - 1], (ftnlen)1);
	    errdp_("#", &sclkdp[bepix - 1], (ftnlen)1);
	    sigerr_("SPICE(BOUNDSDISAGREE)", (ftnlen)21);
	    chkout_("CKW06", (ftnlen)5);
	    return 0;
	} else if (sclkdp[eepix - 1] < ivlbds[i__ - 1]) {
	    setmsg_("Mini-segment interval # start time # follows mini-segme"
		    "nt's last epoch #.", (ftnlen)73);
	    errint_("#", &i__, (ftnlen)1);
	    errdp_("#", &ivlbds[i__ - 1], (ftnlen)1);
	    errdp_("#", &sclkdp[eepix - 1], (ftnlen)1);
	    sigerr_("SPICE(BOUNDSDISAGREE)", (ftnlen)21);
	    chkout_("CKW06", (ftnlen)5);
	    return 0;
	}

/*        Make sure that the quaternions are non-zero. This is just a */
/*        check for uninitialized data. */

/*        For the Hermite subtypes, make sure quaternions are suitable */
/*        for interpolation. */

	i__2 = npkts[i__ - 1];
	for (j = 1; j <= i__2; ++j) {

/*           We have to address the quaternion explicitly, since the */
/*           shape of the packet array is not known at compile time. */

	    addr__ = pktbeg + pktsiz * (j - 1);
	    if (vzerog_(&packts[addr__ - 1], &c__4)) {
		setmsg_("The quaternion in packet # within mini-segment # ha"
			"s magnitude zero.", (ftnlen)68);
		errint_("#", &j, (ftnlen)1);
		errint_("#", &i__, (ftnlen)1);
		sigerr_("SPICE(ZEROQUATERNION)", (ftnlen)21);
		chkout_("CKW06", (ftnlen)5);
		return 0;
	    }

/*           For the Hermite subtypes, each quaternion must be closer */
/*           than its negative to its predecessor in the quaternion */
/*           sequence. */

	    if (j > 1 && even_(&subtyp)) {

/*              Compare the distance between the current quaternion */
/*              and its predecessor vs the distance between the */
/*              negative of the current quaternion and its predecessor. */

		moved_(&packts[addr__ - 1], &c__4, q);
		moved_(&packts[addr__ - pktsiz - 1], &c__4, prevq);
		vminug_(q, &c__4, qneg);
		if (vdistg_(prevq, qneg, &c__4) < vdistg_(prevq, q, &c__4)) {
		    setmsg_("The quaternion in packet # within mini-segment "
			    "# is farther than its negative from its predeces"
			    "sor at index #. This makes the quaternion sequen"
			    "ce unsuitable for Hermite interpolation. The qua"
			    "ternions, and if applicable, their derivatives, "
			    "must be adjusted before they are passed to this "
			    "routine.", (ftnlen)295);
		    errint_("#", &j, (ftnlen)1);
		    errint_("#", &i__, (ftnlen)1);
		    i__3 = j - 1;
		    errint_("#", &i__3, (ftnlen)1);
		    sigerr_("SPICE(BADQUATSIGN)", (ftnlen)18);
		    chkout_("CKW06", (ftnlen)5);
		    return 0;
		}
	    }
	}
    }

/*     If we made it this far, we're ready to start writing the segment. */

/*     The type 6 segment structure is eloquently described by this */
/*     diagram from the CK Required Reading: */

/*        +---------------------------------------+ */
/*        | Mini-segment 1                        | */
/*        +---------------------------------------+ */
/*              . */
/*              . */
/*              . */
/*        +---------------------------------------+ */
/*        | Mini-segment N                        | */
/*        +---------------------------------------+ */
/*        | Mini-segment interval 1 start time    | */
/*        +---------------------------------------+ */
/*              . */
/*              . */
/*              . */
/*        +---------------------------------------+ */
/*        | Mini-segment interval N start time    | */
/*        +---------------------------------------+ */
/*        | Mini-segment interval N stop time     | */
/*        +---------------------------------------+ */
/*        | Mini-seg. interval start time 100     | (First interval */
/*        +---------------------------------------+  directory) */
/*              . */
/*              . */
/*              . */
/*        +---------------------------------------+ */
/*        | Mini-seg. ival. start time (N/100)*100| (Last interval */
/*        +---------------------------------------+  directory) */
/*        | Mini-segment 1 start pointer          | */
/*        +---------------------------------------+ */
/*              . */
/*              . */
/*              . */
/*        +---------------------------------------+ */
/*        | Mini-segment N start pointer          | */
/*        +---------------------------------------+ */
/*        | Mini-segment N stop pointer + 1       | */
/*        +---------------------------------------+ */
/*        | Boundary choice flag                  | */
/*        +---------------------------------------+ */
/*        | Number of intervals                   | */
/*        +---------------------------------------+ */

/*     CK type 6 mini-segments have the following structure: */

/*        +-----------------------+ */
/*        | Packet 1              | */
/*        +-----------------------+ */
/*                    . */
/*                    . */
/*                    . */
/*        +-----------------------+ */
/*        | Packet M              | */
/*        +-----------------------+ */
/*        | Epoch 1               | */
/*        +-----------------------+ */
/*                    . */
/*                    . */
/*                    . */
/*        +-----------------------+ */
/*        | Epoch M               | */
/*        +-----------------------+ */
/*        | Epoch 100             | (First time tag directory) */
/*        +-----------------------+ */
/*                    . */
/*                    . */
/*                    . */
/*        +-----------------------+ */
/*        | Epoch ((M-1)/100)*100 | (Last time tag directory) */
/*        +-----------------------+ */
/*        | Clock rate (sec/tick) | */
/*        +-----------------------+ */
/*        | Subtype code          | */
/*        +-----------------------+ */
/*        | Window size           | */
/*        +-----------------------+ */
/*        | Number of packets     | */
/*        +-----------------------+ */

/*     Note that the set of parameters at the end of a mini-segment does */
/*     not contain an mini-segment interval count. This is because, */
/*     unlike a CK type 5 segment, a CK type 6 segment can contain at */
/*     most one gap. If present, the gap is located at the end of */
/*     mini-segment's mini-segment interval. */

/*     Create the segment descriptor. We don't use CKPDS because */
/*     that routine doesn't allow creation of a singleton segment. */

    ic[0] = *inst;
    ic[1] = refcod;
    ic[2] = 6;
    if (*avflag) {
	ic[3] = 1;
    } else {
	ic[3] = 0;
    }
    dc[0] = *first;
    dc[1] = *last;
    dafps_(&c__2, &c__6, dc, ic, descr);

/*     Begin a new segment. */

    dafbna_(handle, descr, segid, segid_len);
    if (failed_()) {
	chkout_("CKW06", (ftnlen)5);
	return 0;
    }

/*     Re-initialize the mini-segment packet array indices, */
/*     and those of the mini-segment epoch array as well. */

    pktbeg = 0;
    pktend = 0;
    bepix = 0;
    eepix = 0;

/*     Write data for each mini-segment to the file. */

    i__1 = *nmini;
    for (i__ = 1; i__ <= i__1; ++i__) {

/*        Set the packet size, which is a function of the subtype. */

	subtyp = subtps[i__ - 1];
	pktsiz = pktszs[(i__2 = subtyp) < 4 && 0 <= i__2 ? i__2 : s_rnge(
		"pktszs", i__2, "ckw06_", (ftnlen)1369)];
	if (odd_(&subtyp)) {
	    winsiz = degres[i__ - 1] + 1;
	} else {
	    winsiz = (degres[i__ - 1] + 1) / 2;
	}

/*        Now that we have the packet size, we can compute */
/*        mini-segment packet index range. We'll let PKTDSZ */
/*        be the total count of packet data entries for this */
/*        mini-segment. */

	pktdsz = npkts[i__ - 1] * pktsiz;
	pktbeg = pktend + 1;
	pktend = pktbeg - 1 + pktdsz;

/*        At this point, we're read to start writing the */
/*        current mini-segment to the file. Start with the */
/*        packet data. */

	dafada_(&packts[pktbeg - 1], &pktdsz);

/*        Write the epochs for this mini-segment. */

	bepix = eepix + 1;
	eepix = bepix - 1 + npkts[i__ - 1];
	dafada_(&sclkdp[bepix - 1], &npkts[i__ - 1]);

/*        Compute the number of epoch directories for the */
/*        current mini-segment. */

	ndir = (npkts[i__ - 1] - 1) / 100;

/*        Write the epoch directories to the segment. */

	i__2 = ndir;
	for (j = 1; j <= i__2; ++j) {
	    k = bepix - 1 + j * 100;
	    dafada_(&sclkdp[k - 1], &c__1);
	}

/*        Write the mini-segment's SCLK rate, subtype, window size, and */
/*        packet count to the segment. */

	dafada_(&rates[i__ - 1], &c__1);
	d__1 = (doublereal) subtps[i__ - 1];
	dafada_(&d__1, &c__1);
	d__1 = (doublereal) winsiz;
	dafada_(&d__1, &c__1);
	d__1 = (doublereal) npkts[i__ - 1];
	dafada_(&d__1, &c__1);
	if (failed_()) {
	    chkout_("CKW06", (ftnlen)5);
	    return 0;
	}
    }

/*     We've finished writing the mini-segments. */

/*     Next write the mini-segment interval bounds. */

    i__1 = *nmini + 1;
    dafada_(ivlbds, &i__1);

/*     Create and write directories for the interval */
/*     bounds. */

/*     The directory count is the interval bound count */
/*     (N+1), minus 1, divided by the directory size. */

    ndir = *nmini / 100;
    i__1 = ndir;
    for (i__ = 1; i__ <= i__1; ++i__) {
	dafada_(&ivlbds[i__ * 100 - 1], &c__1);
    }

/*     Now we compute and write the start/stop pointers */
/*     for each mini-segment. */

/*     The pointers are relative to the DAF address */
/*     preceding the segment. For example, a pointer */
/*     to the first DAF address in the segment has */
/*     value 1. */

    segend = 0;
    i__1 = *nmini;
    for (i__ = 1; i__ <= i__1; ++i__) {

/*        Set the packet size, which is a function of the subtype. Also */
/*        set the window size. First check the subtype, which will be */
/*        used as an array index. */

	pktsiz = pktszs[(i__2 = subtps[i__ - 1]) < 4 && 0 <= i__2 ? i__2 : 
		s_rnge("pktszs", i__2, "ckw06_", (ftnlen)1474)];

/*        In order to compute the end pointer of the current */
/*        mini-segment, we must compute the size, in terms */
/*        of DAF addresses, of this mini-segment. The formula */
/*        for the size is */

/*            size =     n_packets * packet_size */
/*                    +  n_epochs */
/*                    +  n_epoch_directories */
/*                    +  4 */

/*                 =     n_packets * ( packet_size + 1 ) */
/*                    +  ( n_packets - 1 ) / DIRSIZ */
/*                    +  4 */

	minisz = npkts[i__ - 1] * (pktsiz + 1) + (npkts[i__ - 1] - 1) / 100 + 
		4;
	segbeg = segend + 1;
	segend = segbeg + minisz - 1;

/*        Write the mini-segment begin pointer. */

/*        After the loop terminates, the final end pointer, incremented */
/*        by 1, will be written. */

	d__1 = (doublereal) segbeg;
	dafada_(&d__1, &c__1);
    }

/*     Write the last mini-segment end pointer, incremented by one. */
/*     SEGEND was computed on the last iteration of the above loop. */

    d__1 = (doublereal) (segend + 1);
    dafada_(&d__1, &c__1);

/*     Write out the interval selection flag. The input */
/*     boolean value is represented by a numeric constant. */

    if (*sellst) {
	isel = 1;
    } else {
	isel = -1;
    }
    d__1 = (doublereal) isel;
    dafada_(&d__1, &c__1);

/*     Write the mini-segment/mini-segment interval count. */

    d__1 = (doublereal) (*nmini);
    dafada_(&d__1, &c__1);

/*     End the segment. */

    dafena_();
    chkout_("CKW06", (ftnlen)5);
    return 0;
} /* ckw06_ */

