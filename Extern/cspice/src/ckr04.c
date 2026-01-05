/* ckr04.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__6 = 6;
static integer c__7 = 7;
static doublereal c_b18 = 128.;

/* $Procedure CKR04 ( C-kernel, read pointing record, data type 4 ) */
/* Subroutine */ int ckr04_(integer *handle, doublereal *descr, doublereal *
	sclkdp, doublereal *tol, logical *needav, doublereal *record, logical 
	*found)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer nrec, ends, indx;
    doublereal lbnd1, lbnd2, rbnd1;
    integer k;
    extern /* Subroutine */ int chkin_(char *, ftnlen), cknr04_(integer *, 
	    doublereal *, integer *), dafus_(doublereal *, integer *, integer 
	    *, doublereal *, integer *);
    doublereal value;
    logical exist;
    doublereal midpt1, midpt2;
    extern logical failed_(void);
    integer numall;
    extern /* Subroutine */ int sigerr_(char *, ftnlen);
    integer numcft[7];
    extern /* Subroutine */ int chkout_(char *, ftnlen), sgfpkt_(integer *, 
	    doublereal *, integer *, integer *, doublereal *, integer *), 
	    sgfrvi_(integer *, doublereal *, doublereal *, doublereal *, 
	    integer *, logical *);
    doublereal clkout;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen);
    extern logical return_(void);
    doublereal dcd[2];
    integer icd[6];
    extern /* Subroutine */ int zzck4d2i_(doublereal *, integer *, doublereal 
	    *, integer *);
    doublereal rad1, rad2;

/* $ Abstract */

/*     Read a single data record from a type 4 CK segment. */

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
/*     TOL        I   Time tolerance. */
/*     NEEDAV     I   Angular velocity request flag. */
/*     RECORD     O   Pointing data record. */
/*     FOUND      O   .TRUE. when a record covering SCLKDP is found. */

/* $ Detailed_Input */

/*     HANDLE   is the integer handle of the CK file containing the */
/*              segment. */

/*     DESCR    is the descriptor of the segment. */

/*     SCLKDP   is the encoded spacecraft clock time for which */
/*              pointing is being requested. */

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

/*     RECORD   is the record that CKE04 will evaluate to determine */
/*              the pointing and it includes parameters: */

/*              --------------------------------------------------- */
/*              |    Encoded onboard time which is the closest    | */
/*              |  to SCLKDP and belongs to one of approximation  | */
/*              |                   intervals                     | */
/*              --------------------------------------------------- */
/*              |       encoded SCLK time of the midpoint of      | */
/*              |             interpolation interval              | */
/*              --------------------------------------------------- */
/*              |          radii of interpolation interval        | */
/*              |    expressed as double precision SCLK ticks     | */
/*              --------------------------------------------------- */
/*              |         Number of coefficients for q0           | */
/*              --------------------------------------------------- */
/*              |         Number of coefficients for q1           | */
/*              --------------------------------------------------- */
/*              |         Number of coefficients for q2           | */
/*              --------------------------------------------------- */
/*              |         Number of coefficients for q3           | */
/*              --------------------------------------------------- */
/*              |         Number of coefficients for AV1          | */
/*              --------------------------------------------------- */
/*              |         Number of coefficients for AV2          | */
/*              --------------------------------------------------- */
/*              |         Number of coefficients for AV3          | */
/*              --------------------------------------------------- */
/*              |               q0 Cheby coefficients             | */
/*              --------------------------------------------------- */
/*              |               q1 Cheby coefficients             | */
/*              --------------------------------------------------- */
/*              |               q2 Cheby coefficients             | */
/*              --------------------------------------------------- */
/*              |               q3 Cheby coefficients             | */
/*              --------------------------------------------------- */
/*              |         AV1 Cheby coefficients (optional)       | */
/*              --------------------------------------------------- */
/*              |         AV2 Cheby coefficients (optional)       | */
/*              --------------------------------------------------- */
/*              |         AV3 Cheby coefficients (optional)       | */
/*              --------------------------------------------------- */

/*     FOUND    is .TRUE. if a record was found to satisfy the pointing */
/*              request. This occurs when the time for which pointing */
/*              is requested falls inside one of the interpolation */
/*              intervals, or when the request time is within the */
/*              tolerance of an interval endpoint. */

/* $ Parameters */

/*     See 'ckparam.inc'. */

/* $ Exceptions */

/*     1)  If the specified handle does not belong to an open DAF file, */
/*         an error is signaled by a routine in the call tree of this */
/*         routine. */

/*     2)  If the specified descriptor does not belong to a segment data */
/*         format organized in accordance with generic segment */
/*         architecture, an error is signaled by a routine in the call */
/*         tree of this routine. */

/*     3)  If DESCR is not a valid descriptor of a segment in the CK */
/*         file specified by HANDLE, the results of this routine are */
/*         unpredictable. */

/*     4)  If the segment is not of data type 4, as specified in the */
/*         third integer component of the segment descriptor, */
/*         the error SPICE(WRONGDATATYPE) is signaled. */

/*     5)  If angular velocity data was requested but the segment */
/*         contains no such data, the error SPICE(NOAVDATA) is */
/*         signaled. */

/* $ Files */

/*     See argument HANDLE. */

/* $ Particulars */

/*     See the CK Required Reading file for a detailed description of */
/*     the structure of a type 4 pointing segment. */

/*     When the time for which pointing was requested falls within an */
/*     interpolation interval, then FOUND will be true and RECORD will */
/*     contain the set of Chebyshev polynomial coefficients for the */
/*     time interval that brackets the request time. CKE04 will */
/*     evaluate RECORD to give pointing at the request time. */

/*     However, when the request time is not within any of the */
/*     interpolation intervals, then FOUND will be true only if the */
/*     interval endpoint closest to the request time is within the */
/*     tolerance specified by the user. In this case RECORD will */
/*     contain the set of Chebyshev polynomial coefficients for the */
/*     time interval one of the ends of which was within tolerance */
/*     from the request time, and CKE04 will evaluate RECORD to give */
/*     pointing at the time associated with that interval end time. */

/* $ Examples */

/*     The CKRnn routines are usually used in tandem with the CKEnn */
/*     routines, which evaluate the record returned by CKRnn to give */
/*     the pointing information and output time. */

/*     The following code fragment searches backwards through all of the */
/*     segments in a file applicable to the Mars Global Surveyor */
/*     spacecraft bus that are of data type 4, for a particular */
/*     spacecraft clock time. It then evaluates the pointing for that */
/*     epoch and prints the result. */

/*     The search performed here does not mimic the behavior of the CK */
/*     reader APIs CKGP and CKGPAV, which consider data from multiple CK */
/*     files, when available. See the CK Required reading for details. */

/*     C */
/*     C     CK parameters include file. */
/*     C */
/*           INCLUDE               'ckparam.inc' */

/*     C */
/*     C     Local variables */
/*     C */
/*           CHARACTER*(20)        SCLKCH */
/*           CHARACTER*(20)        SCTIME */
/*           CHARACTER*(40)        IDENT */

/*           DOUBLE PRECISION      AV     ( 3 ) */
/*           DOUBLE PRECISION      CLKOUT */
/*           DOUBLE PRECISION      CMAT   ( 3, 3 ) */
/*           DOUBLE PRECISION      DCD    ( 2 ) */
/*           DOUBLE PRECISION      DESCR  ( 5 ) */
/*           DOUBLE PRECISION      RECORD ( CK4RSZ ) */
/*           DOUBLE PRECISION      SCLKDP */
/*           DOUBLE PRECISION      TOL */

/*           INTEGER               HANDLE */
/*           INTEGER               I */
/*           INTEGER               ICD    ( 6 ) */
/*           INTEGER               INST */
/*           INTEGER               SC */

/*           LOGICAL               FND */
/*           LOGICAL               NEEDAV */
/*           LOGICAL               SFND */

/*     C */
/*     C     Initial values. */
/*     C */
/*           SC     = -94 */
/*           INST   = -94000 */
/*           NEEDAV = .FALSE. */

/*     C */
/*     C     Load the MGS SCLK kernel and the C-kernel. */
/*     C */
/*           CALL FURNSH( 'MGS_SCLK.TSC' ) */
/*           CALL DAFOPR( 'MGS_CK4.BC', HANDLE ) */

/*     C */
/*     C     Get the spacecraft clock time. Then encode it for use */
/*     C     in the C-kernel. */
/*     C */
/*           CALL PROMPT( 'Enter SCLK string: ', SCLKCH ) */
/*           CALL SCENCD( SC, SCLKCH, SCLKDP ) */

/*     C */
/*     C     Use a tolerance of 2 seconds (half of the nominal */
/*     C     separation between MGS pointing instances ). */
/*     C */
/*           CALL SCTIKS ( SC, '0000000002:000', TOL ) */

/*     C */
/*     C     Search backwards from the end of the CK file through all */
/*     C     of the segments. */
/*     C */
/*           CALL DAFBBS( HANDLE ) */
/*           CALL DAFFPA( SFND   ) */

/*           FND = .FALSE. */

/*           DO WHILE ( ( SFND ) .AND. ( .NOT. FND ) ) */

/*     C */
/*     C        Get the segment identifier and descriptor. */
/*     C */
/*              CALL DAFGN( IDENT ) */
/*              CALL DAFGS( DESCR ) */

/*     C */
/*     C        Unpack the segment descriptor into its integer and */
/*     C        double precision components. */
/*     C */
/*              CALL DAFUS( DESCR, 2, 6, DCD, ICD ) */

/*     C */
/*     C        Determine if this segment should be processed. */
/*     C */
/*              IF ( ( INST          .EQ. ICD( 1 ) ) .AND. */
/*          .        ( SCLKDP + TOL  .GE. DCD( 1 ) ) .AND. */
/*          .        ( SCLKDP - TOL  .LE. DCD( 2 ) ) .AND. */
/*          .        ( CK4DTP        .EQ. ICD( 3 ) )      ) THEN */

/*     C */
/*     C           Find CK 4 record covering requested time. */
/*     C */
/*                 CALL CKR04( HANDLE, DESCR, SCLKDP, TOL, NEEDAV, */
/*          .                  RECORD, FND ) */

/*                 IF ( FND ) THEN */

/*     C */
/*     C              Compute pointing using found CK 4 record. */
/*     C */
/*                    CALL CKE04( NEEDAV, RECORD, CMAT, AV, CLKOUT) */

/*                    CALL SCDECD( SC, CLKOUT, SCTIME ) */

/*                    WRITE (*,*) */
/*                    WRITE (*,*) 'Segment identifier: ', IDENT */
/*                    WRITE (*,*) */
/*                    WRITE (*,*) 'Pointing returned for time: ', */
/*          .                      SCTIME */
/*                    WRITE (*,*) */
/*                    WRITE (*,*) 'C-matrix:' */
/*                    WRITE (*,*) */
/*                    WRITE (*,*) ( CMAT(1,I), I = 1, 3 ) */
/*                    WRITE (*,*) ( CMAT(2,I), I = 1, 3 ) */
/*                    WRITE (*,*) ( CMAT(3,I), I = 1, 3 ) */
/*                    WRITE (*,*) */

/*                 END IF */

/*              END IF */

/*              CALL DAFFPA ( SFND ) */

/*           END DO */

/* $ Restrictions */

/*     1)  The file containing the segment should be opened for read */
/*         or write access either by CKLPF, DAFOPR, or DAFOPW. */

/*     2)  The record returned by this routine is intended to be */
/*         evaluated by CKE04. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */
/*     E.D. Wright        (JPL) */
/*     Y.K. Zaiko         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.3, 17-AUG-2021 (NJB) (JDR) */

/*        Updated code example to use backwards search. Added */
/*        note regarding difference between this search and those */
/*        performed by the CK reader APIs CKGP and CKGPAV. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.2, 18-APR-2014 (BVS) */

/*        Minor header edits. */

/* -    SPICELIB Version 1.0.1, 22-AUG-2006 (EDW) */

/*        Replaced references to LDPOOL with references */
/*        to FURNSH. */

/* -    SPICELIB Version 1.0.0, 05-MAY-1999 (YKZ) (BVS) */

/* -& */
/* $ Index_Entries */

/*     read record from type_4 CK segment */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("CKR04", (ftnlen)5);
    }

/*     Set initial value of the found flag to "NOT FOUND". */

    *found = FALSE_;

/*     We need to unpack and analyze descriptor components. The */
/*     unpacked descriptor contains the following information */
/*     about the segment: */

/*        DCD(1)  Initial encoded SCLK */
/*        DCD(2)  Final encoded SCLK */
/*        ICD(1)  Instrument */
/*        ICD(2)  Inertial reference frame */
/*        ICD(3)  Data type */
/*        ICD(4)  Angular velocity flag */
/*        ICD(5)  Initial address of segment data */
/*        ICD(6)  Final address of segment data */

    dafus_(descr, &c__2, &c__6, dcd, icd);

/*     Check if the segment is type 4. Signal an error if it's not. */

    if (icd[2] != 4) {
	setmsg_("The segment is not a type 4 segment.  Type is #", (ftnlen)47)
		;
	errint_("#", &icd[2], (ftnlen)1);
	sigerr_("SPICE(WRONGDATATYPE)", (ftnlen)20);
	chkout_("CKR04", (ftnlen)5);
	return 0;
    }
    if (*needav) {

/*        Signal an error if angular velocities are required but */
/*        they are not present in the segment. */

	if (icd[3] != 1) {
	    setmsg_("Segment does not contain angular velocity data.", (
		    ftnlen)47);
	    sigerr_("SPICE(NOAVDATA)", (ftnlen)15);
	    chkout_("CKR04", (ftnlen)5);
	    return 0;
	}
    }

/*     Get number of records (packets) in the segment. */

    cknr04_(handle, descr, &nrec);

/*     Locate the last time in the set of reference epochs less than or */
/*     equal to the input SCLKDP. */

    sgfrvi_(handle, descr, sclkdp, &value, &indx, &exist);
    if (failed_()) {
	chkout_("CKR04", (ftnlen)5);
	return 0;
    }
    if (! exist) {

/*        We didn't find reference value with means that SCLKDP is */
/*        less than the left bound of the first interpolation interval. */
/*        Fetch the first record. */

	indx = 1;
	sgfpkt_(handle, descr, &indx, &indx, record, &ends);
	if (failed_()) {
	    chkout_("CKR04", (ftnlen)5);
	    return 0;
	}
	midpt1 = record[0];
	rad1 = record[1];

/*        Check whether SCLKDP is within TOL of the left bound of the */
/*        first interval. */

	lbnd1 = midpt1 - rad1 - *tol;
	if (*sclkdp >= lbnd1) {
	    *found = TRUE_;
	    clkout = midpt1 - rad1;
	}
    } else {

/*        We found reference value. */

	if (indx >= nrec) {

/*           The SCLKDP is greater than the left bound of the last */
/*           interpolation interval. Fetch the last record. */

	    indx = nrec;
	    sgfpkt_(handle, descr, &indx, &indx, record, &ends);
	    if (failed_()) {
		chkout_("CKR04", (ftnlen)5);
		return 0;
	    }
	    midpt1 = record[0];
	    rad1 = record[1];

/*           Check whether SCLKDP is within TOL of the right bound of */
/*           the last interval. */

	    rbnd1 = midpt1 + rad1 + *tol;
	    if (*sclkdp <= rbnd1) {
		*found = TRUE_;

/*              Check whether SCLKDP falls between right bound of the */
/*              last interval and right bound + TOL. */

		rbnd1 = midpt1 + rad1;
		if (*sclkdp >= rbnd1) {
		    clkout = midpt1 + rad1;
		} else {

/*                 SCLKDP belongs to the last interval */

		    clkout = *sclkdp;
		}
	    }
	} else if (indx >= 1 && indx < nrec) {

/*           The SCLKDP lies between left bound of the first interval */
/*           and the right bound of the interval before the last */
/*           interval. Fetch the found record. */

	    sgfpkt_(handle, descr, &indx, &indx, record, &ends);
	    if (failed_()) {
		chkout_("CKR04", (ftnlen)5);
		return 0;
	    }
	    midpt1 = record[0];
	    rad1 = record[1];

/*           Check whether SCLKDP belongs to current interval. */

	    rbnd1 = midpt1 + rad1;
	    if (*sclkdp <= rbnd1) {
		*found = TRUE_;
		clkout = *sclkdp;
	    } else {

/*              SCLKDP doesn't belong to current interval. Fetch the */
/*              next packet. */

		i__1 = indx + 1;
		i__2 = indx + 1;
		sgfpkt_(handle, descr, &i__1, &i__2, record, &ends);
		if (failed_()) {
		    chkout_("CKR04", (ftnlen)5);
		    return 0;
		}
		midpt2 = record[0];
		rad2 = record[1];

/*              Find the closest interval bound for SCLKDP. */

		rbnd1 = midpt1 + rad1;
		lbnd2 = midpt2 - rad2;
		if (*sclkdp - rbnd1 <= lbnd2 - *sclkdp) {

/*                 SCLKDP is closer to the right bound of current */
/*                 interval. Check whether it's within TOL of it. */

		    rbnd1 = midpt1 + rad1 + *tol;
		    if (*sclkdp <= rbnd1) {
			*found = TRUE_;
			clkout = midpt1 + rad1;

/*                    At this point we need to re-read our current */
/*                    record because it was overwritten by the next */
/*                    record. No FAILED() check here -- we already */
/*                    fetched this packet successfully one call to */
/*                    SGFPKT ago. */

			sgfpkt_(handle, descr, &indx, &indx, record, &ends);
		    }
		} else {

/*                 SCLKDP is closer to the left bound of the next */
/*                 interval. Check whether it's within TOL of it. */

		    lbnd2 = midpt2 - rad2 - *tol;
		    if (*sclkdp >= lbnd2) {
			*found = TRUE_;
			++indx;
			clkout = midpt2 - rad2;
		    }
		}
	    }
	}
    }

/*     If we found the interval on segment the SCLKDP belongs to, then */

    if (*found) {

/*        Decode numbers of polynomial coefficients. */

	zzck4d2i_(&record[2], &c__7, &c_b18, numcft);

/*        Count total number of coefficients. */

	numall = 0;
	for (k = 1; k <= 7; ++k) {
	    numall += numcft[(i__1 = k - 1) < 7 && 0 <= i__1 ? i__1 : s_rnge(
		    "numcft", i__1, "ckr04_", (ftnlen)696)];
	}

/*        Move coefficients to the right and insert numbers of */
/*        coefficients into output RECORD. */

	for (k = numall; k >= 1; --k) {
	    record[k + 9] = record[k + 2];
	}
	for (k = 1; k <= 7; ++k) {
	    record[k + 2] = (doublereal) numcft[(i__1 = k - 1) < 7 && 0 <= 
		    i__1 ? i__1 : s_rnge("numcft", i__1, "ckr04_", (ftnlen)
		    708)];
	}
	record[2] = record[1];
	record[1] = record[0];

/*        Insert CLKOUT into output RECORD */

	record[0] = clkout;
    }

/*     All done. */

    chkout_("CKR04", (ftnlen)5);
    return 0;
} /* ckr04_ */

