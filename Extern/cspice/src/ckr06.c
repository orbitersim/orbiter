/* ckr06.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__196 = 196;
static integer c__340 = 340;
static integer c__2 = 2;
static integer c__6 = 6;

/* $Procedure CKR06 ( C-kernel, read record from segment, type 6 ) */
/* Subroutine */ int ckr06_(integer *handle, doublereal *descr, doublereal *
	sclkdp, doublereal *tol, logical *needav, doublereal *record, logical 
	*found)
{
    /* Initialized data */

    static integer mxwnsz[4] = { 12,24,12,24 };
    static integer svmiix = -1;
    static integer svminb = -1;
    static integer svn = -1;
    static integer svnpkt = -1;
    static integer svpkdb = -1;
    static integer svpknd = -1;
    static integer svpksz = -1;
    static doublereal svrate = -1.;
    static integer svstyp = -1;
    static integer svwnsz = -1;
    static logical pass1 = TRUE_;
    static integer pktszs[4] = { 8,4,14,7 };
    static integer svbeg = -1;
    static doublereal svbtim = 0.;
    static doublereal svetim = -1.;
    static logical svfnd = FALSE_;
    static integer svhan = 0;
    static logical svlast = FALSE_;

    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer i_dnnt(doublereal *), s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer high;
    doublereal rate;
    integer isel, ndir;
    logical lval;
    integer last, npkt, type__, i__, baddr, n, eaddr;
    doublereal t;
    integer nread;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer minib, minie;
    extern /* Subroutine */ int dafus_(doublereal *, integer *, integer *, 
	    doublereal *, integer *);
    integer ivbas;
    extern /* Subroutine */ int errdp_(char *, doublereal *, ftnlen);
    integer ivbix, iveix, lsize, first, group, rsize;
    extern /* Subroutine */ int dafgda_(integer *, integer *, integer *, 
	    doublereal *);
    doublereal dc[2];
    integer ic[6];
    extern logical failed_(void);
    logical avflag;
    integer begidx, bufbas, dirbas, pkdbas;
    extern doublereal brcktd_(doublereal *, doublereal *, doublereal *);
    doublereal buffer[101];
    integer endidx, remain, timbas;
    logical samseg;
    extern integer lstled_(doublereal *, integer *, doublereal *);
    integer npkdir;
    doublereal lstepc;
    logical samivl;
    doublereal mintim[2];
    extern logical touchl_(logical *);
    integer maxwnd, miniix;
    doublereal contrl[4];
    integer nrcpkt;
    extern integer lstltd_(doublereal *, integer *, doublereal *);
    logical ivlsel;
    integer wndsiz;
    extern logical return_(void);
    logical prvfnd;
    integer pktsiz, subtyp;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen), sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    extern logical odd_(integer *);
    doublereal gap;
    integer low;

/* $ Abstract */

/*     Read a single CK data record from a segment of type 6 */
/*     (ESOC/DDID Piecewise Interpolation). */

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

/*     POINTING */

/* $ Declarations */
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

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   File handle. */
/*     DESCR      I   Segment descriptor. */
/*     SCLKDP     I   Pointing request time. */
/*     TOL        I   Lookup tolerance. */
/*     NEEDAV     I   Angular velocity flag. */
/*     RECORD     O   Data record. */
/*     FOUND      O   Flag indicating whether record was found. */

/* $ Detailed_Input */

/*     HANDLE, */
/*     DESCR    are the file handle and segment descriptor for a CK */
/*              segment of type 6. */

/*     SCLKDP   is an encoded spacecraft clock time indicating the */
/*              epoch for which pointing is desired. */

/*     TOL      is a time tolerance, measured in the same units as */
/*              encoded spacecraft clock. */

/*              When SCLKDP falls between the start time of one of */
/*              the mini-segment intervals and the last time tag of */
/*              that interval, the tolerance has no effect because */
/*              pointing will be returned at the request time. */

/*              However, if the request time falls within a coverage */
/*              gap in one of the intervals, or outside of any */
/*              interval, then the tolerance is used to determine if */
/*              pointing should be returned at the closest epoch for */
/*              which pointing is available. This epoch is either an */
/*              interval's start time or the smaller of an interval's */
/*              end time and its last time tag. */


/*     NEEDAV   is .TRUE. if angular velocity is requested. If the */
/*              input segment descriptor indicates angular velocity */
/*              is absent, the error SPICE(NOAVDATA) is signaled. */

/*              Note: although all subtypes of type 6 records either */
/*              contain or compute angular velocity, a CK creator may */
/*              choose to indicate that the provided angular velocity */
/*              data are not valid; this can be done by setting the */
/*              segment descriptor angular velocity flag to .FALSE. */

/* $ Detailed_Output */

/*     RECORD   is a set of data from the specified segment which, */
/*              when evaluated at epoch SCLKDP, will give the */
/*              attitude and angular velocity of some body, relative */
/*              to the reference frame indicated by DESCR. */

/*              The structure of the record is as follows: */

/*                 +----------------------+ */
/*                 | evaluation epoch     | */
/*                 +----------------------+ */
/*                 | subtype code         | */
/*                 +----------------------+ */
/*                 | number of packets (n)| */
/*                 +----------------------+ */
/*                 | nominal SCLK rate    | */
/*                 +----------------------+ */
/*                 | packet 1             | */
/*                 +----------------------+ */
/*                 | packet 2             | */
/*                 +----------------------+ */
/*                             . */
/*                             . */
/*                             . */
/*                 +----------------------+ */
/*                 | packet n             | */
/*                 +----------------------+ */
/*                 | epochs 1--n          | */
/*                 +----------------------+ */

/*              The packet size is a function of the subtype code. */
/*              All packets in a record have the same size. */


/*     FOUND    is a logical flag indicating whether data were found. */
/*              If NEEDAV is .FALSE., data will be found if the */
/*              request time is within TOL ticks of a time for which */
/*              the segment provides data. If NEEDAV is .TRUE., the */
/*              segment's angular velocity flag must also be set in */
/*              order for data to be found. */

/*              A type 6 segment provides data for times that are */
/*              between its descriptor time bounds and that are */
/*              within the coverage region of a mini-segment */
/*              interval. The coverage region of a mini-segment */
/*              interval extends from its start time to the lesser of */
/*              its stop time and its last time tag. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     This routine roughly follows the pattern established in the */
/*     lower-numbered CK data type readers of not explicitly performing */
/*     error diagnoses. In particular, the C-kernel from which data are */
/*     read is assumed to be valid in most respects. The few exceptions */
/*     that are handled here are listed below. */

/*     1)  If the input HANDLE does not designate a loaded CK file, an */
/*         error is signaled by a routine in the call tree of this */
/*         routine. */

/*     2)  If the segment specified by DESCR is not of data type 6, the */
/*         error SPICE(WRONGCKTYPE) is signaled. */

/*     3)  If the input SCLK value is not within TOL ticks of a time */
/*         for which the segment provides data, FOUND is set to .FALSE. */
/*         and the output record is undefined. */

/*     4)  If the window size is non-positive or greater than the */
/*         maximum allowed value, the error SPICE(INVALIDVALUE) is */
/*         signaled. */

/*     5)  If the window size is not compatible with the segment */
/*         subtype, the error SPICE(INVALIDVALUE) is signaled. */

/*     6)  If the segment subtype is not recognized, the error */
/*         SPICE(INVALIDSUBTYPE) is signaled. */

/*     7)  If the tolerance is negative, the error SPICE(NEGATIVETOL) is */
/*         signaled. */

/*     8)  If an error occurs while trying to read data from a C-kernel, */
/*         the error is signaled by a routine in the call tree of this */
/*         routine. */

/*     9)  If the input segment descriptor indicates that angular */
/*         velocity data are not present, and if the input flag NEEDAV */
/*         is set to .TRUE., the error SPICE(NOAVDATA) is signaled. */

/* $ Files */

/*     See argument HANDLE. */

/* $ Particulars */

/*     See the CK Required Reading file for a description of the */
/*     structure of a data type 6 segment. */

/* $ Examples */

/*     The data returned by the CKRnn routine is in its rawest form, */
/*     taken directly from the segment. As such, it will be meaningless */
/*     to a user unless he/she understands the structure of the data type */
/*     completely. Given that understanding, however, the CKRxx */
/*     routines might be used to "dump" and check segment data for a */
/*     particular epoch. */

/*     The search performed here does not mimic the behavior of the CK */
/*     reader APIs CKGP and CKGPAV, which continue searching when an */
/*     applicable segment doesn't satisfy a pointing request. See the CK */
/*     Required reading for details. */

/*     C */
/*     C     Get a segment applicable to a specified body and epoch. */
/*     C */
/*           CALL CKBSS ( INST,   SCLKDP, TOL,   NEEDAV ) */
/*           CALL CKSNS ( HANDLE, DESCR,  SEGID, SFND   ) */

/*           IF ( .NOT. SFND ) THEN */
/*              [Handle case of pointing not being found] */
/*           END IF */

/*     C */
/*     C     Look at parts of the descriptor. */
/*     C */
/*           CALL DAFUS ( DESCR, 2, 6, DCD, ICD ) */
/*           CENTER = ICD( 2 ) */
/*           REF    = ICD( 3 ) */
/*           TYPE   = ICD( 4 ) */

/*           IF ( TYPE .EQ. 6 ) THEN */

/*              CALL CKR06 ( HANDLE, DESCR, SCLKDP, TOL, NEEDAV, */
/*          .                RECORD, FOUND                       ) */

/*              IF ( .NOT. FOUND ) THEN */
/*                 [Handle case of pointing not being found] */
/*              END IF */

/*              [Look at the RECORD data] */
/*                  . */
/*                  . */
/*                  . */
/*           END IF */

/* $ Restrictions */

/*     1)  Correctness of the C-kernel read by this routine is */
/*         assumed. */

/*     2)  Correctness of inputs must be ensured by the caller of */
/*         this routine. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 06-JUL-2021 (NJB) (JDR) */

/*        Corrected code example: removed comment character preceding */
/*        CKBSS call. Added note regarding difference between this */
/*        search and those performed by the CK reader APIs CKGP and */
/*        CKGPAV. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.0, 14-MAR-2014 (NJB) (BVS) */

/* -& */
/* $ Index_Entries */

/*     read record from type_6 CK segment */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Maximum window sizes, based on subtypes: */


/*     Local variables */


/*     Saved variables */


/*     Initial values */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("CKR06", (ftnlen)5);

/*     Start with a parameter compatibility check on the first */
/*     pass. */

    if (pass1) {
	if (FALSE_) {
	    setmsg_("CK type 6 record size may be as large as #, but CKPFS r"
		    "ecord size (defined in ckparam.inc) is #.", (ftnlen)96);
	    errint_("#", &c__196, (ftnlen)1);
	    errint_("#", &c__340, (ftnlen)1);
	    sigerr_("SPICE(BUG)", (ftnlen)10);
	}

/*        Indicate the first pass was completed. */

	pass1 = FALSE_;
    }

/*     No pointing found so far. */

    *found = FALSE_;

/*     Let PRVFND indicate the last value of FOUND we returned. PRVFND */
/*     allows us to reset SVFND to .FALSE. at the start of this routine, */
/*     so we don't have to do this prior to every return (of which there */
/*     are more than 35). */

    prvfnd = svfnd;

/*     Set the saved value of FOUND so as to reflect the value */
/*     of FOUND we'll return next. */

    svfnd = FALSE_;

/*     "Touch" the input argument NEEDAV to suppress compiler warnings. */

    lval = touchl_(needav);
    lval = touchl_(&lval);

/*     Unpack the segment descriptor, and get the start and end addresses */
/*     of the segment. */

    dafus_(descr, &c__2, &c__6, dc, ic);
    type__ = ic[2];
    avflag = ic[3] == 1;
    baddr = ic[4];
    eaddr = ic[5];

/*     Check whether angular velocity data are requested but not */
/*     available. */

    if (*needav && ! avflag) {
	setmsg_("Segment descriptor indicates angular velocity data are not "
		"available, but such data were requested.", (ftnlen)99);
	sigerr_("SPICE(NOAVDATA)", (ftnlen)15);
	chkout_("CKR06", (ftnlen)5);
	return 0;
    }

/*     Check the tolerance value. */

    if (*tol < 0.) {
	setmsg_("Tolerance must be non-negative but was actually *.", (ftnlen)
		50);
	errdp_("*", tol, (ftnlen)1);
	sigerr_("SPICE(NEGATIVETOL)", (ftnlen)18);
	chkout_("CKR06", (ftnlen)5);
	return 0;
    }

/*     Check the request time and tolerance against the bounds in */
/*     the segment descriptor. */

    if (*sclkdp + *tol < dc[0] || *sclkdp - *tol > dc[1]) {

/*        The request time is too far outside the segment's coverage */
/*        interval for any pointing to satisfy the request. */

	chkout_("CKR06", (ftnlen)5);
	return 0;
    }

/*     Set the request time to use for searching. */

    t = brcktd_(sclkdp, dc, &dc[1]);

/*     From this point onward, we assume the segment was constructed */
/*     correctly. */


/*     Terminology: below, the phrase "base address of 'X'" refers to */
/*     the DAF address immediately preceding X. Base addresses simplify */
/*     mapping DAF array (here "array" means an array stored in */
/*     consecutive DAF addresses, not "segment") indices to DAF */
/*     addresses, since the DAF address of the Ith array element is */
/*     obtained by adding I to the DAF array's base address. */

/*     Key variables: */

/*        Name      Meaning */
/*        ----      ------- */
/*        BADDR     Segment begin DAF address. */

/*        DIRBAS    Base address of mini-segment interval directory. */

/*        EADDR     Segment end DAF address. */

/*        FIRST     Index (mini-segment-relative) of first time tag in */
/*                  sequence transferred to the output record. */

/*        HIGH      Index (mini-segment-relative) of time tag following */
/*                  the tag at index LOW (see description below). */

/*        IVBIX     Index in the mini-segment interval bounds array of */
/*                  the start time of the applicable interval. */

/*        IVLBAS    Base address of mini-segment interval time bounds. */

/*        IVLSEL    Interval selection flag: this routine selects the */
/*                  last applicable interval if true; otherwise it */
/*                  selects the first applicable interval. */

/*        LAST      Index (mini-segment-relative) of last time tag in */
/*                  sequence transferred to the output record. */

/*        LOW       Index (mini-segment-relative) of last time tag less */
/*                  than the request time, or of the first time tag if */
/*                  this tag equals the request time. */

/*        MINIB, */
/*        MINIE     Mini-segment begin and end DAF addresses. These */
/*                  addresses are absolute, not segment-relative. */

/*        MINIIX    Mini-segment/mini-segment interval index. */

/*        N         Count of mini-segments. */

/*        NDIR      Number of mini-segment interval time bounds */
/*                  directories. */

/*        NPKDIR    Number of packet directory entries for current */
/*                  mini-segment. */

/*        NPKT      Packet count for current mini-segment. */

/*        NRCPKT    Output record packet count. Note that this count, */
/*                  due to reduction of order at mini-segment */
/*                  boundaries, may be smaller than the window size */
/*                  stored in the current mini-segment. */

/*        PKDBAS    Base address of packet directory for current */
/*                  mini-segment. */

/*        PKTSIZ    Size of packets of current mini-segment. */

/*        SUBTYP    Subtype of current mini-segment. */

/*        TIMBAS    Base address of time tags of current mini-segment. */

/*        WNDSIZ    Interpolation window size of current mini-segment. */


/*     Re-used variables: the variables shown in the list below */
/*     are used as short-duration variables, much like loop index */
/*     variables; they are re-used as needed. */

/*        BUFBAS */
/*        BUFFER */
/*        GROUP */
/*        NREAD */
/*        REMAIN */


/*     Decide whether we're looking at the same segment we saw on the */
/*     previous call, and whether the mini-segment interval used on */
/*     that call is still applicable. */

/*     Re-use of data from a previous call requires that the saved */
/*     data were set on a successful call. Note that PRVFND can not */
/*     be true on the first pass. */

    samseg = *handle == svhan && baddr == svbeg && prvfnd;

/*     Give SAMIVL an initial value. If we do have the */
/*     same interval, update SAMIVL to indicate this. */

    samivl = FALSE_;
    if (samseg) {

/*        We now assume that all data saved from the last */
/*        read of this segment are valid. */

	if (svlast) {

/*           We pick the last interval containing T. For all intervals */
/*           but the last, T must be less than the interval end time. */
/*           For the last interval, T may equal the end time. */

/*           Note that we don't bother to test for the special case */
/*           where the interval is not the last, there's a gap at the */
/*           end of the interval and T equals the last epoch of the */
/*           interval. In this rare case, we do not reuse the old */
/*           interval data, even though it would be possible to */
/*           add code to do so. */

	    if (svmiix < svn) {
		samivl = t >= svbtim && t < svetim;
	    } else {
		samivl = t >= svbtim && t <= svetim;
	    }
	} else {

/*           We pick the first interval containing T. For all intervals */
/*           but the first, T must be greater than the interval start */
/*           time. For the first interval, T may equal the start time. */

	    if (svmiix > 1) {
		samivl = t > svbtim && t <= svetim;
	    } else {
		samivl = t >= svbtim && t <= svetim;
	    }
	}
    }
    if (samseg && samivl) {

/*        We're looking at the same segment as last time, and the */
/*        mini-segment interval we looked up last time is applicable */
/*        for the input time T. */

/*        Simply restore the segment and interval parameters we */
/*        saved from the previous lookup. */

/*        We don't need to restore the segment start DAF address */
/*        BADDR, since we've already extracted it from DESCR. */

/*        Restore */

/*           - The mini-segment's packet directory count */
/*           - The mini-segment's packet directory base address */

	npkdir = svpknd;
	pkdbas = svpkdb;

/*        Restore */

/*           - The mini-segment/interval count */
/*           - The mini-segment/interval index */
/*           - The mini-segment/interval start pointer */

	n = svn;
	miniix = svmiix;
	minib = svminb;

/*        Restore */

/*           - The mini-segment subtype */
/*           - The mini-segment packet size */
/*           - The mini-segment packet count */
/*           - The mini-segment interpolation window size */
/*           - The mini-segment clock rate */

	subtyp = svstyp;
	pktsiz = svpksz;
	npkt = svnpkt;
	wndsiz = svwnsz;
	rate = svrate;
    } else {

/*        The segment and interval information for the current segment */
/*        must be looked up. */

/*        Perform checks on this segment. */

/*        Make sure that this really is a type 06 data segment. */

	if (type__ != 6) {
	    setmsg_("You are attempting to locate type * data in a type 6 da"
		    "ta segment.", (ftnlen)66);
	    errint_("*", &type__, (ftnlen)1);
	    sigerr_("SPICE(WRONGCKTYPE)", (ftnlen)18);
	    chkout_("CKR06", (ftnlen)5);
	    return 0;
	}

/*        Locate the mini-segment interval that contains the request */
/*        time. If the request time lies a common boundary of two */
/*        intervals, the choice of interval is determined by the */
/*        interval selection flag. */

/*        Before getting started, we need to determine which interval to */
/*        use if the request time lies on a boundary between two */
/*        intervals. The segment's interval selection flag tells us how */
/*        to resolve this. */

	i__1 = eaddr - 1;
	dafgda_(handle, &i__1, &eaddr, contrl);
	if (failed_()) {
	    chkout_("CKR06", (ftnlen)5);
	    return 0;
	}
	isel = i_dnnt(contrl);
	n = i_dnnt(&contrl[1]);
	ivlsel = isel == 1;

/*        Determine the number of interval directory entries in the */
/*        segment. Note that for most CK types, this computation is */
/*        performed by computing */

/*           ( N - 1 ) / DIRSIZ */

/*        where N is the segment's epoch count. */

/*        However the set of items in this case is a sequence */
/*        of N start times followed by a final stop time, so */
/*        the epoch count is */

/*           N + 1 */

/*        and the numerator in the ratio above is incremented by 1. */

	ndir = n / 100;

/*        Note that the directory placement scheme always leaves */
/*        a non-empty group of epochs following the last directory */
/*        entry. */

/*        Let DIRBAS be the base address of the interval directory. */
/*        We'll compute DIRBAS whether or not the interval directory */
/*        is non-empty. */

/*        If the interval directory is non-empty, it spans the address */
/*        range */

/*           DIRBAS+1 : DIRBAS+NDIR */

/*        We compute DIRBAS by starting at the end of the segment */
/*        and skipping over the control area, the mini-segment */
/*        start/stop pointers, and the interval directory itself. */

	dirbas = eaddr - 2 - (n + 1) - ndir;

/*        The way we search the directory depends on the treatment */
/*        of request times that lie on interval boundaries. */

	if (ivlsel) {

/*           We must pick the latest interval containing the request */
/*           time. */

/*           The stop time of the interval we seek is the first interval */
/*           boundary strictly greater than T, unless T is the stop time */
/*           of the final interval. */

/*           We want to find the group of interval boundaries containing */
/*           the stop time of the interval containing T. There are */
/*           NDIR+1 such groups; all but the last have a directory entry */
/*           that coincides with the final epoch of the group. We'll use */
/*           the variable GROUP as the group index. */

/*           If there is an interval directory, search it to determine */
/*           the group of interval times to search next. */

	    if (ndir == 0) {

/*              There's no question about which group of epochs to */
/*              search. */

		group = 1;
	    } else {

/*              The index of the group we seek is the index of the first */
/*              directory entry that is greater than T, if such an entry */
/*              exists. If there's no such entry, the group we seek is */
/*              the final one. */

/*              Find the last directory entry less than or equal to */
/*              the request time. The directory entry after that one, */
/*              if such exists, is the one to pick. */

		nread = min(ndir,101);
		bufbas = dirbas;

/*              Fetch the current batch of directory entries. */

		i__1 = bufbas + 1;
		i__2 = bufbas + nread;
		dafgda_(handle, &i__1, &i__2, buffer);
		if (failed_()) {
		    chkout_("CKR06", (ftnlen)5);
		    return 0;
		}
		remain = ndir - nread;

/*              The variable NREAD always contains a positive value at */
/*              this point, so we can use it as an array index. */

		while(remain > 0 && buffer[(i__1 = nread - 1) < 101 && 0 <= 
			i__1 ? i__1 : s_rnge("buffer", i__1, "ckr06_", (
			ftnlen)926)] <= t) {
		    bufbas += nread;
		    nread = min(remain,101);

/*                 Fetch the current batch of directory entries. */

		    i__1 = bufbas + 1;
		    i__2 = bufbas + nread;
		    dafgda_(handle, &i__1, &i__2, buffer);
		    if (failed_()) {
			chkout_("CKR06", (ftnlen)5);
			return 0;
		    }
		    remain -= nread;
		}

/*              Count the directory entries that are less than or equal */
/*              to T. The number we skipped over before the final loop */
/*              iteration is BUFBAS-DIRBAS; the number of buffered */
/*              entries we're skipping is the number of entries that are */
/*              less than or equal to T. The index of the group of */
/*              epochs containing T exceeds the skipped directory count */
/*              by 1. */

		group = bufbas - dirbas + lstled_(&t, &nread, buffer) + 1;

/*              GROUP is in the range 1 : NDIR+1. */

	    }

/*           Let IVBAS be the base address of the sequence of interval */
/*           time bounds. */

	    ivbas = dirbas - (n + 1);

/*           Now find the index of the last interval boundary less than */
/*           or equal to T. We'll need to read the current group of */
/*           epochs first, so compute the base of the range of addresses */
/*           containing this group. */
	    bufbas = ivbas + (group - 1) * 100;

/*           Compute the number of epochs to read. Note that all groups */
/*           of epochs except the last have DIRSIZ elements. */

	    remain = n + 1 - (group - 1) * 100;

/*           Note that REMAIN is always non-zero, since there's always */
/*           at least one epoch that exceeds the last directory entry. */

	    nread = min(100,remain);
	    i__1 = bufbas + 1;
	    i__2 = bufbas + nread;
	    dafgda_(handle, &i__1, &i__2, buffer);
	    if (failed_()) {
		chkout_("CKR06", (ftnlen)5);
		return 0;
	    }

/*           Find the index of the first epoch greater than T; this is */
/*           the number of epochs that are less than or equal to T, plus */
/*           1. The case where T matches the final epoch must be handled */
/*           here, since in this case no epoch exceeds T. */

	    iveix = bufbas - ivbas + lstled_(&t, &nread, buffer) + 1;
/* Computing MIN */
	    i__1 = iveix, i__2 = n + 1;
	    iveix = min(i__1,i__2);

/*           Backstop test: */

	    if (iveix < 2) {
		setmsg_("IVEIX = #.", (ftnlen)10);
		errint_("#", &iveix, (ftnlen)1);
		sigerr_("SPICE(BUG)", (ftnlen)10);
		chkout_("CKR06", (ftnlen)5);
		return 0;
	    }

/*           The epoch at index IVEIX is the end time of the */
/*           mini-segment interval we'll use. The index of */
/*           the interval itself is IVEIX - 1. */

	    miniix = iveix - 1;
	} else {

/*           IVLSEL is .FALSE., meaning we must pick the first interval */
/*           containing the request time. */
/*           The start time of the interval we seek is the last interval */
/*           boundary strictly less than T, unless T is the start time */
/*           of the first interval. The stop time of this interval is */
/*           the first boundary greater than or equal to T. */

/*           We want to find the group of interval boundaries containing */
/*           the stop time of the interval containing T. There are */
/*           NDIR+1 such groups; all but the last have a directory entry */
/*           that coincides with the final epoch of the group. We'll use */
/*           the variable GROUP as the group index. */

/*           If there is an interval directory, search it to determine */
/*           the group of interval times to search next. */

	    if (ndir == 0) {

/*              There's no question about which group of epochs to */
/*              search. */

		group = 1;
	    } else {

/*              Find the last directory entry strictly less than the */
/*              request time. The directory entry after that one, if */
/*              such exists, is the one to pick. */

		nread = min(ndir,101);
		bufbas = dirbas;
		remain = ndir - nread;

/*              Fetch the current batch of directory entries. */

		i__1 = bufbas + 1;
		i__2 = bufbas + nread;
		dafgda_(handle, &i__1, &i__2, buffer);
		if (failed_()) {
		    chkout_("CKR06", (ftnlen)5);
		    return 0;
		}

/*              The variable NREAD always contains a positive value at */
/*              this point, so we can use it as an array index. */

		while(remain > 0 && buffer[(i__1 = nread - 1) < 101 && 0 <= 
			i__1 ? i__1 : s_rnge("buffer", i__1, "ckr06_", (
			ftnlen)1072)] < t) {
		    bufbas += nread;
		    nread = min(remain,101);

/*                 Fetch the current batch of directory entries. */

		    i__1 = bufbas + 1;
		    i__2 = bufbas + nread;
		    dafgda_(handle, &i__1, &i__2, buffer);
		    if (failed_()) {
			chkout_("CKR06", (ftnlen)5);
			return 0;
		    }
		    remain -= nread;
		}

/*              Count the directory entries that are less than T. The */
/*              number we skipped over before the final loop iteration */
/*              is BUFBAS-DIRBAS; the number of buffered entries we're */
/*              skipping is the number of entries that are less than T. */
/*              The index of the group of epochs containing T exceeds */
/*              the skipped directory count by 1. */

		group = bufbas - dirbas + lstltd_(&t, &nread, buffer) + 1;

/*              GROUP is in the range 1 : NDIR+1. */

	    }

/*           Let IVBAS be the base address of the sequence of interval */
/*           time bounds. */

	    ivbas = dirbas - (n + 1);

/*           Now find the index of the last interval boundary epoch less */
/*           than T. We'll need to read the current group of epochs */
/*           first, so compute the base of the range of addresses */
/*           containing this group. */

	    bufbas = ivbas + (group - 1) * 100;

/*           Compute the number of epochs to read. Note that all groups */
/*           of epochs except the last have DIRSIZ elements. */

	    remain = n + 1 - (group - 1) * 100;

/*           Note that REMAIN is always non-zero, since there's always */
/*           at least one epoch that exceeds the last directory entry. */

	    nread = min(100,remain);
	    i__1 = bufbas + 1;
	    i__2 = bufbas + nread;
	    dafgda_(handle, &i__1, &i__2, buffer);
	    if (failed_()) {
		chkout_("CKR06", (ftnlen)5);
		return 0;
	    }

/*           Find the index of the last interval boundary less than T. */
/*           The case where T matches the first boundary must be handled */
/*           here, since in this case no boundary precedes T. */

	    ivbix = bufbas - ivbas + lstltd_(&t, &nread, buffer);
	    ivbix = max(ivbix,1);

/*           Backstop test: */

	    if (ivbix > n) {
		setmsg_("IVBIX = #.", (ftnlen)10);
		errint_("#", &ivbix, (ftnlen)1);
		sigerr_("SPICE(BUG)", (ftnlen)10);
		chkout_("CKR06", (ftnlen)5);
		return 0;
	    }

/*           The epoch at index IVBIX is the begin time of the */
/*           mini-segment interval we'll use. */

	    miniix = ivbix;
	}

/*        This is the end of the IF block that handles mini-segment */
/*        selection for the two possible values of IVLSEL. */

/*        If the mini-segment we just found has a gap, and if TOL is */
/*        positive, it's possible that the mini-segment we want actually */
/*        is the successor of the one at index MINIIX. We'll check this */
/*        by finding the last epoch of the mini-segment we just */
/*        identified. */

/*        Look up the begin and end pointers of the mini-segment at index */
/*        MINIIX. For the first N-1 mini-segments, the "end pointer" */
/*        of one mini-segment is the "begin" pointer of the next. */

	bufbas = eaddr - 2 - (n + 1) + (miniix - 1);
	i__1 = bufbas + 1;
	i__2 = bufbas + 2;
	dafgda_(handle, &i__1, &i__2, buffer);
	if (failed_()) {
	    chkout_("CKR06", (ftnlen)5);
	    return 0;
	}
	minib = i_dnnt(buffer) + baddr - 1;

/*        Note that the end of the current mini-segment */
/*        precedes the start of the next mini-segment by */
/*        one address. */

	minie = i_dnnt(&buffer[1]) + baddr - 2;

/*        Look up the control area of the mini-segment. */

	i__1 = minie - 3;
	dafgda_(handle, &i__1, &minie, contrl);
	if (failed_()) {
	    chkout_("CKR06", (ftnlen)5);
	    return 0;
	}

/*        Fetch the control area parameters for the mini-segment. */

	rate = contrl[0];
	subtyp = i_dnnt(&contrl[1]);
	wndsiz = i_dnnt(&contrl[2]);
	npkt = i_dnnt(&contrl[3]);

/*        Compute the directory count for the mini-segment. */

	npkdir = (npkt - 1) / 100;

/*        The last epoch of the mini-segment precedes the epoch */
/*        directories and the control area. Look up this epoch. */

	bufbas = minie - 4 - npkdir - 1;
	i__1 = bufbas + 1;
	i__2 = bufbas + 1;
	dafgda_(handle, &i__1, &i__2, &lstepc);
	if (failed_()) {
	    chkout_("CKR06", (ftnlen)5);
	    return 0;
	}

/*        Determine whether the request time is in a gap. */

	if (t > lstepc) {

/*           Yep, T lies in a gap. But we may still be able to */
/*           find data for this request, if the lookup tolerance */
/*           is positive. */

	    if (*tol == 0.) {

/*              We're out of luck. We can't find pointing for this */
/*              request. FOUND is already .FALSE., so just return. */

		chkout_("CKR06", (ftnlen)5);
		return 0;
	    } else {

/*              Determine the distance of T from the nearest epochs. */

/*              Look up the time bounds of the mini-segment at index */
/*              MINIIX. */

		i__1 = ivbas + miniix;
		i__2 = ivbas + miniix + 1;
		dafgda_(handle, &i__1, &i__2, mintim);
		if (failed_()) {
		    chkout_("CKR06", (ftnlen)5);
		    return 0;
		}

/*              See whether T is close enough to a stored epoch for */
/*              us to find pointing. If not, return now. */

		if (t - lstepc > *tol && mintim[1] - t > *tol) {

/*                 We can't find pointing for T. FOUND is already */
/*                 .FALSE., so just return. */

		    chkout_("CKR06", (ftnlen)5);
		    return 0;
		}

/*              Continue to look for pointing usable for time T. */

		if (miniix == n) {

/*                 We're looking at the final mini-segment. If */
/*                 T is close enough to LSTEPC, we can find */
/*                 pointing. */

		    if (t - lstepc <= *tol) {

/*                    We're going to carry on using the current */
/*                    mini-segment. We'll update T to be the last epoch */
/*                    of this mini-segment. */

			t = lstepc;
		    } else {

/*                    T is too far from LSTEPC. We're done. FOUND is */
/*                    already .FALSE., so just return. */

			chkout_("CKR06", (ftnlen)5);
			return 0;
		    }
		} else {

/*                 There's a successor to the current interval. Determine */
/*                 which interval contains an epoch closest to T. */

/*                 Compute the size of the gap at the right end of the */
/*                 interior of the current interval. */

		    gap = mintim[1] - lstepc;
		    if (t - lstepc <= gap / 2) {

/*                    T is closer to LSTEPC than the start time of the */
/*                    next interval. We're going to carry on using the */
/*                    current mini-segment. We'll update T to be the */
/*                    last epoch of this mini-segment. */

			t = lstepc;
		    } else {

/*                    T is closer to the start time of the next interval */
/*                    than to LSTEPC. than the start time of the next */
/*                    interval. We're going to use the next */
/*                    mini-segment. */

			++miniix;

/*                    Update the mini-segment parameters we already */
/*                    found, since these have been superseded. */

/*                    The mini-segment pointers: */

			bufbas = eaddr - 2 - (n + 1) + (miniix - 1);
			i__1 = bufbas + 1;
			i__2 = bufbas + 2;
			dafgda_(handle, &i__1, &i__2, buffer);
			if (failed_()) {
			    chkout_("CKR06", (ftnlen)5);
			    return 0;
			}
			minib = i_dnnt(buffer) + baddr - 1;

/*                    Note that the end of the current mini-segment */
/*                    precedes the start of the next mini-segment by one */
/*                    address. */

			minie = i_dnnt(&buffer[1]) + baddr - 2;

/*                    Look up the control area of the mini-segment. */

			i__1 = minie - 3;
			dafgda_(handle, &i__1, &minie, contrl);
			if (failed_()) {
			    chkout_("CKR06", (ftnlen)5);
			    return 0;
			}

/*                    Fetch the control area parameters for the */
/*                    mini-segment. */

			rate = (doublereal) i_dnnt(contrl);
			subtyp = i_dnnt(&contrl[1]);
			wndsiz = i_dnnt(&contrl[2]);
			npkt = i_dnnt(&contrl[3]);

/*                    Since we have new mini-segment parameters, we need */
/*                    to check them. We'll defer these checks until */
/*                    later, so we can perform one set of checks, */
/*                    regardless of which logic path we followed to */
/*                    select a mini-segment. */

/*                    Compute the directory count for the mini-segment. */

			npkdir = (npkt - 1) / 100;

/*                    We're going to set T to the start time of the */
/*                    current mini-segment interval, which is the stop */
/*                    time of the previous one. */

			t = mintim[1];

/*                    We still need to look up the last epoch of the */
/*                    current mini-segment. We'll use this when we save */
/*                    the time bounds of the mini-segment. */

			bufbas = minie - 4 - npkdir - 1;
			i__1 = bufbas + 1;
			i__2 = bufbas + 1;
			dafgda_(handle, &i__1, &i__2, &lstepc);
			if (failed_()) {
			    chkout_("CKR06", (ftnlen)5);
			    return 0;
			}
		    }
		}

/*              At this point T is set. If we had to update the */
/*              mini-segment index and its parameters, we did so. */

	    }

/*           We've handled the case where T lies in a gap and the */
/*           tolerance is non-zero. */
	}

/*        This is the end of the block that handles the case where T lies */
/*        in a gap. At this point, the following items are set: */

/*           T */
/*           MINIIX */
/*           MINIB */
/*           MINIE */
/*           SUBTYP */
/*           WNDSIZ */
/*           NPKT */
/*           NPKDIR */
/*           RATE */

/*        Look up the time bounds of the mini-segment at index MINIIX. */
/*        These bounds are used quite a bit farther on, when we save */
/*        them for future use. */

	i__1 = ivbas + miniix;
	i__2 = ivbas + miniix + 1;
	dafgda_(handle, &i__1, &i__2, mintim);
	if (failed_()) {
	    chkout_("CKR06", (ftnlen)5);
	    return 0;
	}

/*        From this point onward, we'll work with the mini-segment */
/*        that occupies the address range MINIB : MINIE. */

/*        Set the packet size, which is a function of the subtype. */
/*        Also set the maximum window size. First check the subtype, */
/*        which will be used as an array index. */

	if (subtyp < 0 || subtyp >= 4) {
	    setmsg_("Unexpected CK type 6 subtype # found in type 06 segment"
		    " within mini-segment #.", (ftnlen)78);
	    errint_("#", &subtyp, (ftnlen)1);
	    errint_("#", &miniix, (ftnlen)1);
	    sigerr_("SPICE(INVALIDSUBTYPE)", (ftnlen)21);
	    chkout_("CKR06", (ftnlen)5);
	    return 0;
	}
	pktsiz = pktszs[(i__1 = subtyp) < 4 && 0 <= i__1 ? i__1 : s_rnge(
		"pktszs", i__1, "ckr06_", (ftnlen)1467)];
	maxwnd = mxwnsz[(i__1 = subtyp) < 4 && 0 <= i__1 ? i__1 : s_rnge(
		"mxwnsz", i__1, "ckr06_", (ftnlen)1468)];

/*        Check the window size. */

	if (wndsiz < 2 || wndsiz > maxwnd) {
	    setmsg_("Window size in type 6 segment was #; must be in the ran"
		    "ge 2:# for subtype #. Mini-segment index is #.", (ftnlen)
		    101);
	    errint_("#", &wndsiz, (ftnlen)1);
	    errint_("#", &maxwnd, (ftnlen)1);
	    errint_("#", &subtyp, (ftnlen)1);
	    errint_("#", &miniix, (ftnlen)1);
	    sigerr_("SPICE(INVALIDVALUE)", (ftnlen)19);
	    chkout_("CKR06", (ftnlen)5);
	    return 0;
	}
	if (odd_(&wndsiz)) {
	    setmsg_("Window size in type 06 segment was #; must be even for "
		    "subtype #. Mini-segment index is #.", (ftnlen)90);
	    errint_("#", &wndsiz, (ftnlen)1);
	    errint_("#", &subtyp, (ftnlen)1);
	    errint_("#", &miniix, (ftnlen)1);
	    sigerr_("SPICE(INVALIDVALUE)", (ftnlen)19);
	    chkout_("CKR06", (ftnlen)5);
	    return 0;
	}

/*        Compute the base address of the sequence of packet */
/*        directory entries for the current mini-segment/interval. */

	pkdbas = minib - 1 + npkt * (pktsiz + 1);

/*        The test below is done for safety. No SPICE errors */
/*        should ever be detected at this point. */

	if (failed_()) {
	    chkout_("CKR06", (ftnlen)5);
	    return 0;
	}

/*        If we made it this far, we did so without a SPICE error. We */
/*        have valid segment parameters which can be saved for the next */
/*        call. */

/*        Save */

/*           - The DAF handle */
/*           - The segment begin DAF address */
/*           - The segment's "select last/first interval" flag */

	svhan = *handle;
	svbeg = baddr;
	svlast = ivlsel;

/*        Save the time bounds of the applicable mini-segment/interval. */
/*        We don't want to indicate data availability within a gap, since */
/*        the re-use logic assumes data availability. */

	svbtim = mintim[0];
	svetim = min(mintim[1],lstepc);

/*        Save */

/*           - The mini-segment/interval directory count */
/*           - The mini-segment/interval directory base address */

	svpknd = npkdir;
	svpkdb = pkdbas;

/*        Save */

/*           - The mini-segment/interval count */
/*           - The mini-segment/interval index */
/*           - The mini-segment/interval start pointer */

	svn = n;
	svmiix = miniix;
	svminb = minib;

/*        Save */

/*           - The mini-segment subtype */
/*           - The mini-segment packet size */
/*           - The mini-segment packet count */
/*           - The mini-segment window size */
/*           - The mini-segment clock rate */

	svstyp = subtyp;
	svpksz = pktsiz;
	svnpkt = npkt;
	svwnsz = wndsiz;
	svrate = rate;
    }

/*     We're ready to construct the output record. The first step is to */
/*     identify the indices of the packets and epochs corresponding to */
/*     the request. */

/*     We'll now select the set of packets that define the interpolating */
/*     polynomials.   We'll start out by finding the first directory */
/*     entry that is greater than or equal to the request epoch.  We'll */
/*     use the variable GROUP to indicate the set of epochs to search */
/*     within, once we've found the right directory entry. */

    if (npkdir == 0) {

/*        There's no mystery about which group of epochs to search. */

	group = 1;
    } else {

/*        There's at least one directory entry. Find the first directory */
/*        entry whose time is greater than or equal to the request time, */
/*        if there is such an entry.  We'll search linearly through the */
/*        directory entries, reading up to DIRSIZ of them at a time. */
/*        Having found the correct set of directory entries, we'll */
/*        perform a binary search within that set for the desired entry. */

	bufbas = pkdbas;
	nread = min(npkdir,100);
	remain = npkdir - nread;
	i__1 = bufbas + 1;
	i__2 = bufbas + nread;
	dafgda_(handle, &i__1, &i__2, buffer);
	if (failed_()) {
	    chkout_("CKR06", (ftnlen)5);
	    return 0;
	}
	while(buffer[(i__1 = nread - 1) < 101 && 0 <= i__1 ? i__1 : s_rnge(
		"buffer", i__1, "ckr06_", (ftnlen)1614)] < t && remain > 0) {
	    bufbas += nread;
	    nread = min(remain,100);
	    remain -= nread;

/*           Note:  NREAD is always > 0 here. */

	    i__1 = bufbas + 1;
	    i__2 = bufbas + nread;
	    dafgda_(handle, &i__1, &i__2, buffer);
	    if (failed_()) {
		chkout_("CKR06", (ftnlen)5);
		return 0;
	    }
	}

/*        At this point, BUFBAS - PKDBAS is the number of directory */
/*        entries preceding the one contained in BUFFER(1). */

	group = bufbas - pkdbas + lstltd_(&t, &nread, buffer) + 1;
    }

/*     GROUP now indicates the set of epochs in which to search for the */
/*     request epoch.  If GROUP is 1, the request time lies within the */
/*     inclusive time interval bounded by the first and last epochs of */
/*     the first group.  Otherwise, the request time lies in the time */
/*     interval bounded by the last element of the preceding group and */
/*     the last element of the current group. */

/*     We'll use the variable names BEGIDX and ENDIDX to refer to */
/*     the indices, relative to the set of time tags, of the first */
/*     and last time tags in the set we're going to look up. */

    if (group == 1) {
	begidx = 1;
	endidx = min(npkt,100);
    } else {

/*        If the group index is greater than 1, we'll include the last */
/*        time tag of the previous group in the set of time tags we look */
/*        up.  That way, the request time is bracketed by the time tag */
/*        set we look up. */

	begidx = (group - 1) * 100;
/* Computing MIN */
	i__1 = begidx + 100;
	endidx = min(i__1,npkt);
    }
    timbas = pkdbas - npkt;
    i__1 = timbas + begidx;
    i__2 = timbas + endidx;
    dafgda_(handle, &i__1, &i__2, buffer);
    if (failed_()) {
	chkout_("CKR06", (ftnlen)5);
	return 0;
    }

/*     Find two adjacent epochs bounding the request epoch.  The request */
/*     time cannot be greater than all of epochs in the group, and it */
/*     cannot precede the first element of the group. */

    i__1 = endidx - begidx + 1;
    i__ = lstltd_(&t, &i__1, buffer);

/*     The variables LOW and high are the indices of a pair of time */
/*     tags that bracket the request time. */

    if (i__ == 0) {
	low = 1;
    } else {
	low = begidx + i__ - 1;
    }
    high = low + 1;

/*     Now select the set of packets used for interpolation. Note */
/*     that the window size is known to be even. */

/*     For CK type 6 we allow the window size to shrink when the window */
/*     must be truncated due to proximity to an interval boundary. */

/*     The nominal bracketing epochs we've found are the (WNDSIZ/2)nd */
/*     and (WNDSIZ/2 + 1)st of the interpolating set.  If the */
/*     request time is too close to one end of the coverage interval, */
/*     we reduce the window size, after which one endpoint of the */
/*     window will coincide with an endpoint of the coverage interval. */

/*     Let LSIZE be the size of the "left half" of the window: the size */
/*     set of window epochs to the left of the request time. We want */
/*     this size to be WNDSIZ/2, but if not enough packets are */
/*     available, the set ranges from index 1 to index LOW. */

/* Computing MIN */
    i__1 = wndsiz / 2;
    lsize = min(i__1,low);

/*     RSIZE is defined analogously for the right half of the window. */

/* Computing MIN */
    i__1 = wndsiz / 2, i__2 = npkt - high + 1;
    rsize = min(i__1,i__2);

/*     The actual window size is simply the sum of LSIZE and RSIZE. */

    nrcpkt = lsize + rsize;

/*     FIRST and LAST are the endpoints of the range of indices of */
/*     time tags (and packets) we'll collect in the output record. */

    first = low - lsize + 1;
    last = first + nrcpkt - 1;

/*     We're ready to construct the output record. */

/*     Put the subtype and window size into the output record. */
/*     The fourth element is the nominal SCLK rate. */

    record[0] = t;
    record[1] = (doublereal) subtyp;
    record[2] = (doublereal) nrcpkt;
    record[3] = rate;

/*     Read the packets. */

    i__1 = minib + (first - 1) * pktsiz;
    i__2 = minib + last * pktsiz - 1;
    dafgda_(handle, &i__1, &i__2, &record[4]);

/*     Finally, add the epochs to the output record. */
/*     Read the sequence of time tags. */

    bufbas = minib - 1 + npkt * pktsiz + (first - 1);
    i__1 = bufbas + 1;
    i__2 = bufbas + nrcpkt;
    dafgda_(handle, &i__1, &i__2, &record[nrcpkt * pktsiz + 4]);
    if (failed_()) {
	chkout_("CKR06", (ftnlen)5);
	return 0;
    }

/*     Indicate pointing was found. */

    *found = TRUE_;
    svfnd = TRUE_;
    chkout_("CKR06", (ftnlen)5);
    return 0;
} /* ckr06_ */

