/* ckr05.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__6 = 6;

/* $Procedure CKR05 ( Read CK record from segment, type 05 ) */
/* Subroutine */ int ckr05_(integer *handle, doublereal *descr, doublereal *
	sclkdp, doublereal *tol, logical *needav, doublereal *record, logical 
	*found)
{
    /* Initialized data */

    static integer lbeg = -1;
    static integer lend = -1;
    static integer lhand = 0;
    static doublereal prevn = -1.;
    static doublereal prevnn = -1.;
    static doublereal prevs = -1.;

    /* System generated locals */
    integer i__1, i__2;
    doublereal d__1, d__2;

    /* Builtin functions */
    integer i_dnnt(doublereal *), s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer high;
    doublereal rate;
    integer last, type__, i__, j, n;
    doublereal t;
    integer begin;
    extern /* Subroutine */ int chkin_(char *, ftnlen), dafus_(doublereal *, 
	    integer *, integer *, doublereal *, integer *);
    integer nidir;
    extern doublereal dpmax_(void);
    integer npdir, nsrch;
    extern /* Subroutine */ int errdp_(char *, doublereal *, ftnlen), moved_(
	    doublereal *, integer *, doublereal *);
    integer lsize, first, nints, rsize;
    doublereal start;
    extern /* Subroutine */ int dafgda_(integer *, integer *, integer *, 
	    doublereal *);
    doublereal dc[2];
    integer ic[6];
    extern logical failed_(void);
    integer bufbas, dirbas;
    doublereal hepoch;
    extern doublereal brcktd_(doublereal *, doublereal *, doublereal *);
    doublereal lepoch;
    integer npread, nsread, remain, pbegix, sbegix, timbas;
    doublereal pbuffr[101];
    extern integer lstled_(doublereal *, integer *, doublereal *);
    doublereal sbuffr[103];
    integer pendix, sendix, packsz;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    integer maxwnd;
    doublereal contrl[5];
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen);
    extern integer lstltd_(doublereal *, integer *, doublereal *);
    doublereal nstart;
    extern logical return_(void);
    integer pgroup, sgroup, wndsiz, wstart, subtyp;
    doublereal nnstrt;
    extern logical odd_(integer *);
    integer end, low;

/* $ Abstract */

/*     Read a single CK data record from a segment of type 05 */
/*     (MEX/Rosetta Attitude file interpolation). */

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

/*     Declare parameters specific to CK type 05. */

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

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 20-AUG-2002 (NJB) */

/* -& */

/*     CK type 5 subtype codes: */


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


/*     Packet sizes associated with the various subtypes: */


/*     End of file ck05.inc. */

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
/*     HANDLE     I   File handle. */
/*     DESCR      I   Segment descriptor. */
/*     SCLKDP     I   Pointing request time. */
/*     TOL        I   Lookup tolerance. */
/*     NEEDAV     I   Angular velocity flag. */
/*     RECORD     O   Data record. */
/*     FOUND      O   Flag indicating whether record was found. */

/* $ Detailed_Input */

/*     HANDLE, */
/*     DESCR    are the file handle and segment descriptor for */
/*              a CK segment of type 05. */

/*     SCLKDP   is an encoded spacecraft clock time indicating */
/*              the epoch for which pointing is desired. */

/*     TOL      is a time tolerance, measured in the same units as */
/*              encoded spacecraft clock. */

/*              When SCLKDP falls within the bounds of one of the */
/*              interpolation intervals then the tolerance has no */
/*              effect because pointing will be returned at the */
/*              request time. */

/*              However, if the request time is not in one of the */
/*              intervals, then the tolerance is used to determine */
/*              if pointing at one of the interval endpoints should */
/*              be returned. */

/*     NEEDAV   is .TRUE. if angular velocity is requested. */

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

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     This routine follows the pattern established in the lower-numbered */
/*     CK data type readers of not explicitly performing error */
/*     diagnoses. Exceptions are listed below nonetheless. */

/*     1)  If the input HANDLE does not designate a loaded CK file, an */
/*         error is signaled by a routine in the call tree of this */
/*         routine. */

/*     2)  If the segment specified by DESCR is not of data type 05, */
/*         the error SPICE(WRONGCKTYPE) is signaled. */

/*     3)  If the input SCLK value is not within the range specified */
/*         in the segment descriptor, the error SPICE(TIMEOUTOFBOUNDS) */
/*         is signaled. */

/*     4)  If the window size is non-positive or greater than the */
/*         maximum allowed value, the error SPICE(INVALIDVALUE) is */
/*         signaled. */

/*     5)  If the window size is not compatible with the segment */
/*         subtype, the error SPICE(INVALIDVALUE) is signaled. */

/*     6)  If the segment subtype is not recognized, the error */
/*         SPICE(NOTSUPPORTED) is signaled. */

/*     7)  If the tolerance is negative, the error SPICE(VALUEOUTOFRANGE) */
/*         is signaled. */

/* $ Files */

/*     See argument HANDLE. */

/* $ Particulars */

/*     See the CK Required Reading file for a description of the */
/*     structure of a data type 05 segment. */

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

/*           IF ( TYPE .EQ. 05 ) THEN */

/*              CALL CKR05 ( HANDLE, DESCR, SCLKDP, TOL, NEEDAV, */
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

/*     1)  Correctness of inputs must be ensured by the caller of */
/*         this routine. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */

/* $ Version */

/* -    SPICELIB Version 2.0.1, 06-JUL-2021 (NJB) (JDR) */

/*        Corrected code example: removed comment character preceding */
/*        CKBSS call. Added note regarding difference between this */
/*        search and those performed by the CK reader APIs CKGP and */
/*        CKGPAV. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 2.0.0, 27-JAN-2014 (NJB) */

/*        Increased MAXDEG to 23 for compatibility with CK type 6. */

/* -    SPICELIB Version 1.1.0, 06-SEP-2002 (NJB) */

/* -& */
/* $ Index_Entries */

/*     read record from type_5 CK segment */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Maximum polynomial degree: */


/*     Local variables */


/*     Saved variables */


/*     Initial values */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("CKR05", (ftnlen)5);

/*     No pointing found so far. */

    *found = FALSE_;

/*     Unpack the segment descriptor, and get the start and end addresses */
/*     of the segment. */

    dafus_(descr, &c__2, &c__6, dc, ic);
    type__ = ic[2];
    begin = ic[4];
    end = ic[5];

/*     Make sure that this really is a type 05 data segment. */

    if (type__ != 5) {
	setmsg_("You are attempting to locate type * data in a type 5 data s"
		"egment.", (ftnlen)66);
	errint_("*", &type__, (ftnlen)1);
	sigerr_("SPICE(WRONGCKTYPE)", (ftnlen)18);
	chkout_("CKR05", (ftnlen)5);
	return 0;
    }

/*     Check the tolerance value. */

    if (*tol < 0.) {
	setmsg_("Tolerance must be non-negative but was actually *.", (ftnlen)
		50);
	errdp_("*", tol, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("CKR05", (ftnlen)5);
	return 0;
    }

/*     Check the request time and tolerance against the bounds in */
/*     the segment descriptor. */

    if (*sclkdp + *tol < dc[0] || *sclkdp - *tol > dc[1]) {

/*        The request time is too far outside the segment's coverage */
/*        interval for any pointing to satisfy the request. */

	chkout_("CKR05", (ftnlen)5);
	return 0;
    }

/*     Set the request time to use for searching. */

    t = brcktd_(sclkdp, dc, &dc[1]);

/*     From this point onward, we assume the segment was constructed */
/*     correctly.  In particular, we assume: */

/*        1)  The segment descriptor's time bounds are in order and are */
/*            distinct. */

/*        2)  The epochs in the segment are in strictly increasing */
/*            order. */


/*        3)  The interpolation interval start times in the segment are */
/*            in strictly increasing order. */


/*        4)  The degree of the interpolating polynomial specified by */
/*            the segment is at least 1 and is no larger than MAXDEG. */


    i__1 = end - 4;
    dafgda_(handle, &i__1, &end, contrl);

/*     Check the FAILED flag just in case HANDLE is not attached to */
/*     any DAF file and the error action is not set to ABORT.  We */
/*     do this only after the first call to DAFGDA, as in CKR03. */

    if (failed_()) {
	chkout_("CKR05", (ftnlen)5);
	return 0;
    }
    rate = contrl[0];
    subtyp = i_dnnt(&contrl[1]);
    wndsiz = i_dnnt(&contrl[2]);
    nints = i_dnnt(&contrl[3]);
    n = i_dnnt(&contrl[4]);

/*     Set the packet size, which is a function of the subtype. */

    if (subtyp == 0) {
	packsz = 8;
    } else if (subtyp == 1) {
	packsz = 4;
    } else if (subtyp == 2) {
	packsz = 14;
    } else if (subtyp == 3) {
	packsz = 7;
    } else {
	setmsg_("Unexpected CK type 5 subtype # found in type 5 segment.", (
		ftnlen)55);
	errint_("#", &subtyp, (ftnlen)1);
	sigerr_("SPICE(NOTSUPPORTED)", (ftnlen)19);
	chkout_("CKR05", (ftnlen)5);
	return 0;
    }

/*     Check the window size. */

    if (wndsiz <= 0) {
	setmsg_("Window size in type 05 segment was #; must be positive.", (
		ftnlen)55);
	errint_("#", &wndsiz, (ftnlen)1);
	sigerr_("SPICE(INVALIDVALUE)", (ftnlen)19);
	chkout_("CKR05", (ftnlen)5);
	return 0;
    }
    if (subtyp == 0 || subtyp == 2) {

/*        These are the Hermite subtypes. */

	maxwnd = 12;
	if (wndsiz > maxwnd) {
	    setmsg_("Window size in type 05 segment was #; max allowed value"
		    " is # for subtypes 0 and 2 (Hermite, 8 or 14-element pac"
		    "kets).", (ftnlen)117);
	    errint_("#", &wndsiz, (ftnlen)1);
	    errint_("#", &maxwnd, (ftnlen)1);
	    sigerr_("SPICE(INVALIDVALUE)", (ftnlen)19);
	    chkout_("CKR05", (ftnlen)5);
	    return 0;
	}
	if (odd_(&wndsiz)) {
	    setmsg_("Window size in type 05 segment was #; must be even for "
		    "subtypes 0 and 2 (Hermite, 8 or 14-element packets).", (
		    ftnlen)107);
	    errint_("#", &wndsiz, (ftnlen)1);
	    sigerr_("SPICE(INVALIDVALUE)", (ftnlen)19);
	    chkout_("CKR05", (ftnlen)5);
	    return 0;
	}
    } else if (subtyp == 1 || subtyp == 3) {

/*        These are the Lagrange subtypes. */

	maxwnd = 24;
	if (wndsiz > maxwnd) {
	    setmsg_("Window size in type 05 segment was #; max allowed value"
		    " is # for subtypes 1 and 3 (Lagrange, 4 or 7-element pac"
		    "kets).", (ftnlen)117);
	    errint_("#", &wndsiz, (ftnlen)1);
	    errint_("#", &maxwnd, (ftnlen)1);
	    sigerr_("SPICE(INVALIDVALUE)", (ftnlen)19);
	    chkout_("CKR05", (ftnlen)5);
	    return 0;
	}
	if (odd_(&wndsiz)) {
	    setmsg_("Window size in type 05 segment was #; must be even for "
		    "subtypes 1 and 3 (Lagrange, 4 or 7-element packets).", (
		    ftnlen)107);
	    errint_("#", &wndsiz, (ftnlen)1);
	    sigerr_("SPICE(INVALIDVALUE)", (ftnlen)19);
	    chkout_("CKR05", (ftnlen)5);
	    return 0;
	}
    } else {
	setmsg_("This point should not be reached. Getting here may indicate"
		" that the code needs to updated to handle the new subtype #", 
		(ftnlen)118);
	errint_("#", &subtyp, (ftnlen)1);
	sigerr_("SPICE(NOTSUPPORTED)", (ftnlen)19);
	chkout_("CKR05", (ftnlen)5);
	return 0;
    }

/*     We now need to select the pointing values to interpolate */
/*     in order to satisfy the pointing request.  The first step */
/*     is to use the pointing directories (if any) to locate a set of */
/*     epochs bracketing the request time.  Note that the request */
/*     time might not be bracketed:  it could precede the first */
/*     epoch or follow the last epoch. */

/*     We'll use the variable PGROUP to refer to the set of epochs */
/*     to search.  The first group consists of the epochs prior to */
/*     and including the first pointing directory entry.  The last */
/*     group consists of the epochs following the last pointing */
/*     directory entry.  Other groups consist of epochs following */
/*     one pointing directory entry up to and including the next */
/*     pointing directory entry. */

    npdir = (n - 1) / 100;
    dirbas = begin + n * packsz + n - 1;
    if (npdir == 0) {

/*        There's no mystery about which group of epochs to search. */

	pgroup = 1;
    } else {

/*        There's at least one directory.  Find the first directory */
/*        whose time is greater than or equal to the request time, if */
/*        there is such a directory.  We'll search linearly through the */
/*        directory entries, reading up to DIRSIZ of them at a time. */
/*        Having found the correct set of directory entries, we'll */
/*        perform a binary search within that set for the desired entry. */

	bufbas = dirbas;
	npread = min(npdir,100);
	i__1 = bufbas + 1;
	i__2 = bufbas + npread;
	dafgda_(handle, &i__1, &i__2, pbuffr);
	remain = npdir - npread;
	while(pbuffr[(i__1 = npread - 1) < 101 && 0 <= i__1 ? i__1 : s_rnge(
		"pbuffr", i__1, "ckr05_", (ftnlen)650)] < t && remain > 0) {
	    bufbas += npread;
	    npread = min(remain,100);

/*           Note:  NPREAD is always > 0 here. */

	    i__1 = bufbas + 1;
	    i__2 = bufbas + npread;
	    dafgda_(handle, &i__1, &i__2, pbuffr);
	    remain -= npread;
	}

/*        At this point, BUFBAS - DIRBAS is the number of directory */
/*        entries preceding the one contained in PBUFFR(1). */

/*        PGROUP is one more than the number of directories we've */
/*        passed by. */

	pgroup = bufbas - dirbas + lstltd_(&t, &npread, pbuffr) + 1;
    }

/*     PGROUP now indicates the set of epochs in which to search for the */
/*     request epoch.  The following cases can occur: */

/*        PGROUP = 1 */
/*        ========== */

/*           NPDIR = 0 */
/*           -------- */
/*           The request time may precede the first time tag */
/*           of the segment, exceed the last time tag, or lie */
/*           in the closed interval bounded by these time tags. */

/*           NPDIR >= 1 */
/*           --------- */
/*           The request time may precede the first time tag */
/*           of the group but does not exceed the last epoch */
/*           of the group. */


/*        1 < PGROUP <= NPDIR */
/*        =================== */

/*           The request time follows the last time of the */
/*           previous group and is less than or equal to */
/*           the pointing directory entry at index PGROUP. */

/*        1 < PGROUP = NPDIR + 1 */
/*        ====================== */

/*           The request time follows the last time of the */
/*           last pointing directory entry.  The request time */
/*           may exceed the last time tag. */


/*     Now we'll look up the time tags in the group of epochs */
/*     we've identified. */

/*     We'll use the variable names PBEGIX and PENDIX to refer to */
/*     the indices, relative to the set of time tags, of the first */
/*     and last time tags in the set we're going to look up. */

    if (pgroup == 1) {
	pbegix = 1;
	pendix = min(n,100);
    } else {

/*        If the group index is greater than 1, we'll include the last */
/*        time tag of the previous group in the set of time tags we look */
/*        up.  That way, the request time is strictly bracketed on the */
/*        low side by the time tag set we look up. */

	pbegix = (pgroup - 1) * 100;
/* Computing MIN */
	i__1 = pbegix + 100;
	pendix = min(i__1,n);
    }
    timbas = dirbas - n;
    i__1 = timbas + pbegix;
    i__2 = timbas + pendix;
    dafgda_(handle, &i__1, &i__2, pbuffr);
    npread = pendix - pbegix + 1;

/*     At this point, we'll deal with the cases where T lies outside */
/*     of the range of epochs we've buffered. */

    if (t < pbuffr[0]) {

/*        This can happen only if PGROUP = 1 and T precedes all epochs. */
/*        If the input request time is too far from PBUFFR(1) on */
/*        the low side, we're done. */

	if (*sclkdp + *tol < pbuffr[0]) {
	    chkout_("CKR05", (ftnlen)5);
	    return 0;
	}

/*        Bracket T to move it within the range of buffered epochs. */

	t = pbuffr[0];
    } else if (t > pbuffr[(i__1 = npread - 1) < 101 && 0 <= i__1 ? i__1 : 
	    s_rnge("pbuffr", i__1, "ckr05_", (ftnlen)765)]) {

/*        This can happen only if T follows all epochs. */

	if (*sclkdp - *tol > pbuffr[(i__1 = npread - 1) < 101 && 0 <= i__1 ? 
		i__1 : s_rnge("pbuffr", i__1, "ckr05_", (ftnlen)769)]) {
	    chkout_("CKR05", (ftnlen)5);
	    return 0;
	}

/*        Bracket T to move it within the range of buffered epochs. */

	t = pbuffr[(i__1 = npread - 1) < 101 && 0 <= i__1 ? i__1 : s_rnge(
		"pbuffr", i__1, "ckr05_", (ftnlen)779)];
    }

/*     At this point, */

/*        | T - SCLKDP |  <=  TOL */

/*     Also, one of the following is true: */

/*        T is the first time of the segment */

/*        T is the last time of the segment */

/*        T equals SCLKDP */



/*     Find two adjacent time tags bounding the request epoch.  The */
/*     request time cannot be greater than all of time tags in the */
/*     group, and it cannot precede the first element of the group. */

    i__ = lstltd_(&t, &npread, pbuffr);

/*     The variables LOW and HIGH are the indices of a pair of time */
/*     tags that bracket the request time.  Remember that NPREAD could */
/*     be equal to 1, in which case we would have LOW = HIGH. */

    if (i__ == 0) {

/*        This can happen only if PGROUP = 1 and T = PBUFFR(1). */

	low = 1;
	lepoch = pbuffr[0];
	if (n == 1) {
	    high = 1;
	} else {
	    high = 2;
	}
	hepoch = pbuffr[(i__1 = high - 1) < 101 && 0 <= i__1 ? i__1 : s_rnge(
		"pbuffr", i__1, "ckr05_", (ftnlen)822)];
    } else {
	low = pbegix + i__ - 1;
	lepoch = pbuffr[(i__1 = i__ - 1) < 101 && 0 <= i__1 ? i__1 : s_rnge(
		"pbuffr", i__1, "ckr05_", (ftnlen)827)];
	high = low + 1;
	hepoch = pbuffr[(i__1 = i__) < 101 && 0 <= i__1 ? i__1 : s_rnge("pbu"
		"ffr", i__1, "ckr05_", (ftnlen)830)];
    }

/*     We now need to find the interpolation interval containing */
/*     T, if any.  We may be able to use the interpolation */
/*     interval found on the previous call to this routine.  If */
/*     this is the first call or if the previous interval is not */
/*     applicable, we'll search for the interval. */

/*     First check if the request time falls in the same interval as */
/*     it did last time.  We need to make sure that we are dealing */
/*     with the same segment as well as the same time range. */


/*        PREVS      is the start time of the interval that satisfied */
/*                   the previous request for pointing. */

/*        PREVN      is the start time of the interval that followed */
/*                   the interval specified above. */

/*        PREVNN     is the start time of the interval that followed */
/*                   the interval starting at PREVN. */

/*        LHAND      is the handle of the file that PREVS and PREVN */
/*                   were found in. */

/*        LBEG,      are the beginning and ending addresses of the */
/*        LEND       segment in the file LHAND that PREVS and PREVN */
/*                   were found in. */

    if (*handle == lhand && begin == lbeg && end == lend && t >= prevs && t < 
	    prevn) {
	start = prevs;
	nstart = prevn;
	nnstrt = prevnn;
    } else {

/*        Search for the interpolation interval. */

	nidir = (nints - 1) / 100;
	dirbas = end - 5 - nidir;
	if (nidir == 0) {

/*           There's no mystery about which group of epochs to search. */

	    sgroup = 1;
	} else {

/*           There's at least one directory.  Find the first directory */
/*           whose time is greater than or equal to the request time, if */
/*           there is such a directory.  We'll search linearly through */
/*           the directory entries, reading up to DIRSIZ of them at a */
/*           time. Having found the correct set of directory entries, */
/*           we'll perform a binary search within that set for the */
/*           desired entry. */

	    bufbas = dirbas;
	    nsread = min(nidir,100);
	    remain = nidir - nsread;
	    i__1 = bufbas + 1;
	    i__2 = bufbas + nsread;
	    dafgda_(handle, &i__1, &i__2, sbuffr);
	    while(sbuffr[(i__1 = nsread - 1) < 103 && 0 <= i__1 ? i__1 : 
		    s_rnge("sbuffr", i__1, "ckr05_", (ftnlen)902)] < t && 
		    remain > 0) {
		bufbas += nsread;
		nsread = min(remain,100);
		remain -= nsread;

/*              Note:  NSREAD is always > 0 here. */

		i__1 = bufbas + 1;
		i__2 = bufbas + nsread;
		dafgda_(handle, &i__1, &i__2, sbuffr);
	    }

/*           At this point, BUFBAS - DIRBAS is the number of directory */
/*           entries preceding the one contained in SBUFFR(1). */

/*           SGROUP is one more than the number of directories we've */
/*           passed by. */

	    sgroup = bufbas - dirbas + lstltd_(&t, &nsread, sbuffr) + 1;
	}

/*        SGROUP now indicates the set of interval start times in which */
/*        to search for the request epoch. */

/*        Now we'll look up the time tags in the group of epochs we've */
/*        identified. */

/*        We'll use the variable names SBEGIX and SENDIX to refer to the */
/*        indices, relative to the set of start times, of the first and */
/*        last start times in the set we're going to look up. */

	if (sgroup == 1) {
	    sbegix = 1;
	    sendix = min(nints,102);
	} else {

/*           Look up the start times for the group of interest. Also */
/*           buffer last start time from the previous group. Also, it */
/*           turns out to be useful to pick up two extra start */
/*           times---the first two start times of the next group---if */
/*           they exist. */

	    sbegix = (sgroup - 1) * 100;
/* Computing MIN */
	    i__1 = sbegix + 102;
	    sendix = min(i__1,nints);
	}
	timbas = dirbas - nints;
	i__1 = timbas + sbegix;
	i__2 = timbas + sendix;
	dafgda_(handle, &i__1, &i__2, sbuffr);
	nsread = sendix - sbegix + 1;

/*        Find the last interval start time less than or equal to the */
/*        request time.  We know T is greater than or equal to the */
/*        first start time, so I will be > 0. */

	nsrch = min(101,nsread);
	i__ = lstled_(&t, &nsrch, sbuffr);
	start = sbuffr[(i__1 = i__ - 1) < 103 && 0 <= i__1 ? i__1 : s_rnge(
		"sbuffr", i__1, "ckr05_", (ftnlen)973)];

/*        Let NSTART ("next start") be the start time that follows */
/*        START, if START is not the last start time.  If NSTART */
/*        has a successor, let NNSTRT be that start time. */

	if (i__ < nsread) {
	    nstart = sbuffr[(i__1 = i__) < 103 && 0 <= i__1 ? i__1 : s_rnge(
		    "sbuffr", i__1, "ckr05_", (ftnlen)982)];
	    if (i__ + 1 < nsread) {
		nnstrt = sbuffr[(i__1 = i__ + 1) < 103 && 0 <= i__1 ? i__1 : 
			s_rnge("sbuffr", i__1, "ckr05_", (ftnlen)986)];
	    } else {
		nnstrt = dpmax_();
	    }
	} else {
	    nstart = dpmax_();
	    nnstrt = dpmax_();
	}
    }

/*     If T does not lie within the interpolation interval starting */
/*     at time START, we'll determine whether T is closer to this */
/*     interval or the next.  If the distance between T and the */
/*     closer interval is less than or equal to TOL, we'll map T */
/*     to the closer endpoint of the closer interval.  Otherwise, */
/*     we return without finding pointing. */

    if (hepoch == nstart) {

/*        The first time tag greater than or equal to T is the start */
/*        time of the next interpolation interval. */

/*        The request time lies between interpolation intervals. */
/*        LEPOCH is the last time tag of the first interval; HEPOCH */
/*        is the first time tag of the next interval. */

	if ((d__1 = t - lepoch, abs(d__1)) <= (d__2 = hepoch - t, abs(d__2))) 
		{

/*           T is closer to the first interval... */

	    if ((d__1 = t - lepoch, abs(d__1)) > *tol) {

/*              ...But T is too far from the interval. */

		chkout_("CKR05", (ftnlen)5);
		return 0;
	    }

/*           Map T to the right endpoint of the preceding interval. */

	    t = lepoch;
	    high = low;
	    hepoch = lepoch;
	} else {

/*           T is closer to the second interval... */

	    if ((d__1 = hepoch - t, abs(d__1)) > *tol) {

/*              ...But T is too far from the interval. */

		chkout_("CKR05", (ftnlen)5);
		return 0;
	    }

/*           Map T to the left endpoint of the next interval. */

	    t = hepoch;
	    low = high;
	    lepoch = hepoch;

/*           Since we're going to be picking time tags from the next */
/*           interval, we'll need to adjust START and NSTART. */

	    start = nstart;
	    nstart = nnstrt;
	}
    }

/*     We now have */

/*        LEPOCH < T <  HEPOCH */
/*                -   - */

/*     where LEPOCH and HEPOCH are the time tags at indices */
/*     LOW and HIGH, respectively. */

/*     Now select the set of packets used for interpolation.  Note */
/*     that the window size is known to be even. */

/*     Unlike CK types 8, 9, 12, and 13, for type 05 we adjust */
/*     the window size to keep the request time within the central */
/*     interval of the window. */

/*     The nominal bracketing epochs we've found are the (WNDSIZ/2)nd */
/*     and (WNDSIZ/2 + 1)st of the interpolating set.  If the request */
/*     time is too close to one end of the interpolation interval, we */
/*     reduce the window size, after which one endpoint of the window */
/*     will coincide with an endpoint of the interpolation interval. */

/*     We start out by looking up the set of time tags we'd use */
/*     if there were no gaps in the coverage.  We then trim our */
/*     time tag set to ensure all tags are in the interpolation */
/*     interval.  It's possible that the interpolation window will */
/*     collapse to a single point as a result of this last step. */

/*     Let LSIZE be the size of the "left half" of the window:  the */
/*     size of the set of window epochs to the left of the request time. */
/*     We want this size to be WNDSIZ/2, but if not enough states are */
/*     available, the set ranges from index 1 to index LOW. */

/* Computing MIN */
    i__1 = wndsiz / 2;
    lsize = min(i__1,low);

/*     RSIZE is defined analogously for the right half of the window. */

/* Computing MIN */
    i__1 = wndsiz / 2, i__2 = n - high + 1;
    rsize = min(i__1,i__2);

/*     The window size is simply the sum of LSIZE and RSIZE. */

    wndsiz = lsize + rsize;

/*     FIRST and LAST are the endpoints of the range of indices of */
/*     time tags (and packets) we'll collect in the output record. */

    first = low - lsize + 1;
    last = first + wndsiz - 1;

/*     Buffer the epochs. */

    wstart = begin + n * packsz + first - 1;
    i__1 = wstart + wndsiz - 1;
    dafgda_(handle, &wstart, &i__1, pbuffr);

/*     Discard any epochs less than START or greater than or equal */
/*     to NSTART.  The set of epochs we want ranges from indices */
/*     I+1 to J.  This range is non-empty unless START and NSTART */
/*     are both DPMAX(). */

    i__ = lstltd_(&start, &wndsiz, pbuffr);
    j = lstltd_(&nstart, &wndsiz, pbuffr);
    if (i__ == j) {

/*        Fuggedaboudit. */

	chkout_("CKR05", (ftnlen)5);
	return 0;
    }

/*     Update FIRST, LAST, and WNDSIZ. */

    wndsiz = j - i__;
    first += i__;
    last = first + wndsiz - 1;

/*     Put the subtype into the output record.  The size of the group */
/*     of packets is derived from the subtype, so we need not include */
/*     the size. */

    record[0] = t;
    record[1] = (doublereal) subtyp;
    record[2] = (doublereal) wndsiz;
    record[3] = rate;

/*     Read the packets. */

    i__1 = begin + (first - 1) * packsz;
    i__2 = begin + last * packsz - 1;
    dafgda_(handle, &i__1, &i__2, &record[4]);

/*     Finally, add the epochs to the output record. */

    i__2 = j - i__;
    moved_(&pbuffr[(i__1 = i__) < 101 && 0 <= i__1 ? i__1 : s_rnge("pbuffr", 
	    i__1, "ckr05_", (ftnlen)1175)], &i__2, &record[wndsiz * packsz + 
	    4]);

/*     Save the information about the interval and segment. */

    lhand = *handle;
    lbeg = begin;
    lend = end;
    prevs = start;
    prevn = nstart;
    prevnn = nnstrt;

/*     Indicate pointing was found. */

    *found = TRUE_;
    chkout_("CKR05", (ftnlen)5);
    return 0;
} /* ckr05_ */

