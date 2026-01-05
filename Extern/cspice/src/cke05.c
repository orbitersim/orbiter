/* cke05.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__4 = 4;

/* $Procedure CKE05 ( C-Kernel, evaluate, type 5 ) */
/* Subroutine */ int cke05_(logical *needav, doublereal *record, doublereal *
	cmat, doublereal *av, doublereal *clkout)
{
    /* System generated locals */
    integer i__1, i__2, i__3;
    doublereal d__1;

    /* Builtin functions */
    integer i_dnnt(doublereal *), s_rnge(char *, integer, char *, integer);

    /* Local variables */
    doublereal mags, qneg[4], rate;
    integer from;
    extern /* Subroutine */ int vequ_(doublereal *, doublereal *);
    doublereal work[1360]	/* was [680][2] */;
    integer i__, j, n;
    doublereal q[4];
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    doublereal vbuff[6];
    extern /* Subroutine */ int moved_(doublereal *, integer *, doublereal *),
	     vhatg_(doublereal *, integer *, doublereal *), errdp_(char *, 
	    doublereal *, ftnlen), vsclg_(doublereal *, doublereal *, integer 
	    *, doublereal *);
    doublereal state[8];
    extern doublereal vdotg_(doublereal *, doublereal *, integer *);
    extern /* Subroutine */ int vsubg_(doublereal *, doublereal *, integer *, 
	    doublereal *), qdq2av_(doublereal *, doublereal *, doublereal *);
    doublereal dq[4], ds[4];
    integer ub, to;
    doublereal locrec[340], sclddq[4];
    extern /* Subroutine */ int lgrind_(integer *, doublereal *, doublereal *,
	     doublereal *, doublereal *, doublereal *, doublereal *);
    doublereal sclkdp, radtrm[4];
    integer packsz;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    extern doublereal lgrint_(integer *, doublereal *, doublereal *, 
	    doublereal *, doublereal *), vdistg_(doublereal *, doublereal *, 
	    integer *);
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen), vminug_(doublereal *, integer *, doublereal *)
	    , vsclip_(doublereal *, doublereal *), hrmint_(integer *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *);
    extern doublereal vnormg_(doublereal *, integer *);
    extern /* Subroutine */ int xpsgip_(integer *, integer *, doublereal *);
    extern logical return_(void);
    integer newptr;
    extern /* Subroutine */ int q2m_(doublereal *, doublereal *);
    integer xstart, subtyp, ystart, prvptr;

/* $ Abstract */

/*     Evaluate a single data record from a type 5 CK segment. */

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
/*     NEEDAV     I   .TRUE. if angular velocity is requested. */
/*     RECORD    I-O  Data type 5 record. */
/*     CMAT       O   C-matrix. */
/*     AV         O   Angular velocity vector. */
/*     CLKOUT     O   SCLK associated with C-matrix. */

/* $ Detailed_Input */

/*     NEEDAV   is .TRUE. if angular velocity is requested. */

/*     RECORD   is a record from a type 5 CK segment which, when */
/*              evaluated at the epoch contained in its first */
/*              element, will give the attitude and angular velocity */
/*              of a spacecraft structure or instrument relative to a */
/*              base reference frame. */

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
/*                          . */
/*                          . */
/*                          . */
/*                 +----------------------+ */
/*                 | packet n             | */
/*                 +----------------------+ */
/*                 | epochs 1--n          | */
/*                 +----------------------+ */

/*                See the CK Required Reading or the include file */
/*                ck05.inc for details on CK type 5 packet contents. */

/* $ Detailed_Output */

/*     RECORD   has been modified due to its use as a workspace array. */
/*              The contents are undefined. */


/*     CMAT     is a rotation matrix that transforms the components */
/*              of a vector expressed in the base frame given in */
/*              the segment to components expressed in the instrument */
/*              fixed frame at the returned time. */

/*              Thus, if a vector v has components x, y, z in the */
/*              base frame, then v has components x', y', z' in the */
/*              instrument fixed frame where: */

/*                   [ x' ]     [          ] [ x ] */
/*                   | y' |  =  |   CMAT   | | y | */
/*                   [ z' ]     [          ] [ z ] */

/*              If the x', y', z' components are known, use the */
/*              transpose of the C-matrix to determine x, y, z as */
/*              follows. */

/*                   [ x ]      [          ]T    [ x' ] */
/*                   | y |  =   |   CMAT   |     | y' | */
/*                   [ z ]      [          ]     [ z' ] */
/*                            (Transpose of CMAT) */


/*     AV       is the angular velocity vector of the instrument fixed */
/*              frame defined by CMAT. The angular velocity is */
/*              returned only if NEEDAV is .TRUE. */

/*              The direction of the angular velocity vector gives */
/*              the right-handed axis about which the instrument fixed */
/*              reference frame is rotating. The magnitude of AV is */
/*              the magnitude of the instantaneous velocity of the */
/*              rotation, in radians per second. */

/*              The angular velocity vector is returned in component */
/*              form */

/*                       AV = [ AV1  , AV2  , AV3  ] */

/*              which is in terms of the base coordinate frame */
/*              specified in the segment descriptor. */

/*     CLKOUT   is the encoded SCLK associated with the returned */
/*              C-matrix and angular velocity vector. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the input record contains an unrecognized subtype code, */
/*         the error SPICE(NOTSUPPORTED) is signaled. */

/*     2)  If the record subtype is one for which quaternion derivatives */
/*         are stored (subtypes 0 and 2), and if the Ith quaternion in */
/*         the input record is farther than its negative from the (I-1)st */
/*         quaternion in the record, the error SPICE(BADQUATSIGN) is */
/*         signaled. */

/*         For subtypes 1 and 3, this condition is not considered an */
/*         error: the closer to the preceding quaternion of the two */
/*         quaternion representations is used for interpolation. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The exact format and structure of CK type 5 (MEX/Rosetta Attitude */
/*     file interpolation) CK segments is described in the CK Required */
/*     Reading. */

/* $ Examples */

/*     The CKEnn routines are almost always used in conjunction with */
/*     the corresponding CKRnn routines, which read the records from */
/*     CK files. */

/*     The following code fragment searches through all of the segments */
/*     in a file applicable to the Mars Express spacecraft bus that */
/*     are of data type 5, for a particular spacecraft clock time. */
/*     It then evaluates the pointing for that epoch and prints the */
/*     result. */

/*           CHARACTER*(20)        SCLKCH */
/*           CHARACTER*(20)        SCTIME */
/*           CHARACTER*(40)        IDENT */

/*           INTEGER               I */
/*           INTEGER               SC */
/*           INTEGER               INST */
/*           INTEGER               HANDLE */
/*           INTEGER               DTYPE */
/*           INTEGER               ICD      (    6 ) */

/*           DOUBLE PRECISION      SCLKDP */
/*           DOUBLE PRECISION      TOL */
/*           DOUBLE PRECISION      CLKOUT */
/*           DOUBLE PRECISION      DESCR    (    5 ) */
/*           DOUBLE PRECISION      DCD      (    2 ) */
/*           DOUBLE PRECISION      RECORD   (   17 ) */
/*           DOUBLE PRECISION      CMAT     ( 3, 3 ) */
/*           DOUBLE PRECISION      AV       (    3 ) */

/*           LOGICAL               NEEDAV */
/*           LOGICAL               FND */
/*           LOGICAL               SFND */


/*           SC     = -41 */
/*           INST   = -41000 */
/*           DTYPE  =  5 */
/*           NEEDAV = .FALSE. */

/*     C */
/*     C     Load the MEX SCLK kernel and the C-kernel. */
/*     C */
/*           CALL FURNSH ( 'MEX_SCLK.TSC'       ) */
/*           CALL DAFOPR ( 'MEX_CK.BC',  HANDLE ) */
/*     C */
/*     C     Get the spacecraft clock time. Then encode it for use */
/*     C     in the C-kernel. */
/*     C */
/*           WRITE (*,*) 'Enter spacecraft clock time string:' */
/*           READ (*,FMT='(A)') SCLKCH */

/*           CALL SCENCD ( SC, SCLKCH, SCLKDP ) */
/*     C */
/*     C     Use a tolerance of 2 seconds ( half of the nominal */
/*     C     separation between MEX pointing instances ). */
/*     C */
/*           CALL SCTIKS ( SC, '0000000002:000', TOL ) */

/*     C */
/*     C     Search from the beginning of the CK file through all */
/*     C     of the segments. */
/*     C */
/*           CALL DAFBFS ( HANDLE ) */
/*           CALL DAFFNA ( SFND   ) */

/*           FND    = .FALSE. */

/*           DO WHILE ( ( SFND ) .AND. ( .NOT. FND ) ) */

/*     C */
/*     C        Get the segment identifier and descriptor. */
/*     C */
/*              CALL DAFGN ( IDENT ) */
/*              CALL DAFGS ( DESCR ) */
/*     C */
/*     C        Unpack the segment descriptor into its integer and */
/*     C        double precision components. */
/*     C */
/*              CALL DAFUS ( DESCR, 2, 6, DCD, ICD ) */

/*     C */
/*     C        Determine if this segment should be processed. */
/*     C */
/*              IF ( ( INST          .EQ. ICD( 1 ) ) .AND. */
/*          .        ( SCLKDP + TOL  .GE. DCD( 1 ) ) .AND. */
/*          .        ( SCLKDP - TOL  .LE. DCD( 2 ) ) .AND. */
/*          .        ( DTYPE         .EQ. ICD( 3 ) )      ) THEN */


/*                 CALL CKR05 ( HANDLE, DESCR, SCLKDP, TOL, NEEDAV, */
/*          .                   RECORD, FND ) */

/*                 IF ( FND ) THEN */

/*                    CALL CKE05 (NEEDAV,RECORD,CMAT,AV,CLKOUT) */

/*                    CALL SCDECD ( SC, CLKOUT, SCTIME ) */

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

/*              CALL DAFFNA ( SFND ) */

/*           END DO */

/* $ Restrictions */

/*     1)  This routine assumes that the input record is valid. Any */
/*         checking of the input data is assumed to have been performed */
/*         when the source CK file was created. */

/*     2)  This routine assumes that the input data are suitable for the */
/*         interpolation method indicated by the subtype code in the */
/*         input record. Since the mapping of rotations to quaternions */
/*         is multiple-valued, this routine assumes that whichever sign */
/*         minimizes the Euclidean distance between one quaternion and */
/*         the next is the correct sign. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */

/* $ Version */

/* -    SPICELIB Version 3.1.1, 12-AUG-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 3.1.0, 11-AUG-2015 (NJB) */

/*        Bug fix: PRVPTR is now updated at the end of the quaternion */
/*        sequence check for Hermite subtypes. */

/* -    SPICELIB Version 3.0.0, 06-FEB-2014 (NJB) */

/*        Bug fix and functional change: quaternion sign adjustment */
/*        is no longer performed for the Hermite subtypes (0 and 2). */
/*        If a sign adjustment is needed for quaternions belonging to */
/*        a record of Hermite subtype, an error is signaled. Sign */
/*        adjustment is still performed for the Lagrange subtypes. */

/*        Corrected in-line comments concerning change of AV units. */

/* -    SPICELIB Version 2.0.0, 20-NOV-2006 (NJB) */

/*        Bug fix: this routine now assumes that angular velocity */
/*        and quaternion derivative values stored in the input */
/*        record have units of radians/second. */

/*        Bug fix: this routine no longer attempts to determine */
/*        the correct sign of quaternion derivatives. The caller */
/*        must supply quaternion derivatives that are suitable */
/*        for interpolation. */

/* -    SPICELIB Version 1.3.0, 23-OCT-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments in */
/*        XPOSEG and VSCL calls. Replaced header reference to LDPOOL */
/*        with reference to FURNSH. */

/* -    SPICELIB Version 1.2.0, 14-FEB-2003 (NJB) */

/*        Bug fix: angular velocity computation was modified to */
/*        match that used in the corresponding algorithm employed */
/*        by the MEX/Rosetta attitude file reader. The quaternion */
/*        derivative used to derive angular velocity now is the */
/*        derivative of the *unit* quaternion. */

/* -    SPICELIB Version 1.1.0, 06-SEP-2002 (NJB) */

/* -& */
/* $ Index_Entries */

/*     evaluate type_5 CK segment */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.3.0, 23-OCT-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments in */
/*        XPOSEG and VSCL calls. Replaced header reference to LDPOOL */
/*        with reference to FURNSH. */

/* -    SPICELIB Version 1.2.0, 14-FEB-2003 (NJB) */

/*        Bug fix: angular velocity computation was modified to */
/*        match that used in the corresponding algorithm employed */
/*        by the MEX/Rosetta attitude file reader. The quaternion */
/*        derivative used to derive angular velocity now is the */
/*        derivative of the *unit* quaternion. */

/*        Letting Q(t) be the quaternion derived by polynomial */
/*        interpolation, and letting UQ(t) be Q(t)/||Q(t)||, */
/*        the quaternion derivative d(UQ)/dt is now used. */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Index of evaluation epoch in record: */


/*     Index of subtype code in record: */


/*     Index of packet count in record: */


/*     Index at which packets start; packet base: */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("CKE05", (ftnlen)5);

/*     Capture the subtype from the record and set the packet size */
/*     accordingly. */

    subtyp = i_dnnt(&record[1]);
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
	chkout_("CKE05", (ftnlen)5);
	return 0;
    }

/*     Get the packet count and epoch. */

    n = i_dnnt(&record[2]);
    sclkdp = record[0];

/*     Get the nominal clock rate. */

    rate = record[3];

/*     Adjust quaternion "signs" as necessary to minimize distance */
/*     between successive quaternions. This adjustment is performed */
/*     only for subtypes that don't store quaternion derivatives */
/*     (these are the Lagrange subtypes). */

    if (subtyp == 1 || subtyp == 3) {

/*        For these subtypes, only the quaternions themselves need be */
/*        adjusted. */

/*        PRVPTR is the index of the "previous" quaternion---the one to */
/*        which the successor and its negative will be compared. */

	prvptr = 5;
	i__1 = n;
	for (i__ = 2; i__ <= i__1; ++i__) {

/*           NEWPTR points to the quaternion ahead of the one */
/*           pointed to by PRVPTR. */

	    newptr = packsz * (i__ - 1) + 5;
	    vminug_(&record[newptr - 1], &c__4, qneg);

/*           Replace the Ith quaternion with QNEG if QNEG is closer */
/*           than the current quaternion to the previous quaternion. */

	    if (vdistg_(&record[prvptr - 1], qneg, &c__4) < vdistg_(&record[
		    prvptr - 1], &record[newptr - 1], &c__4)) {
		moved_(qneg, &c__4, &record[newptr - 1]);
	    }
	    prvptr = newptr;
	}
    } else {

/*        For the Hermite types, if the quaternions need to be adjusted, */
/*        we have an error condition. */

/*        PRVPTR is the index of the "previous" quaternion---the one to */
/*        which the successor and its negative will be compared. */

	prvptr = 5;
	i__1 = n;
	for (i__ = 2; i__ <= i__1; ++i__) {

/*           NEWPTR points to the quaternion ahead of the one */
/*           pointed to by PRVPTR. */

	    newptr = packsz * (i__ - 1) + 5;
	    vminug_(&record[newptr - 1], &c__4, qneg);

/*           Replace the Ith quaternion with QNEG if QNEG is closer */
/*           than the current quaternion to the previous quaternion. */

	    if (vdistg_(&record[prvptr - 1], qneg, &c__4) < vdistg_(&record[
		    prvptr - 1], &record[newptr - 1], &c__4)) {
		setmsg_("Quaternion sign error: quaternion at index # in the"
			" input record is farther than its negative from the "
			"preceding quaternion in the record. Quaternion is (#"
			", #, #, #); predecessor is (#, #, #, #). This makes "
			"the quaternion sequence unsuitable for Hermite inter"
			"polation. The quaternions, and if applicable, their "
			"derivatives, must be adjusted before they are passed"
			" to this routine.", (ftnlen)380);
		errint_("#", &i__, (ftnlen)1);
		errdp_("#", &record[newptr - 1], (ftnlen)1);
		errdp_("#", &record[newptr], (ftnlen)1);
		errdp_("#", &record[newptr + 1], (ftnlen)1);
		errdp_("#", &record[newptr + 2], (ftnlen)1);
		errdp_("#", &record[prvptr - 1], (ftnlen)1);
		errdp_("#", &record[prvptr], (ftnlen)1);
		errdp_("#", &record[prvptr + 1], (ftnlen)1);
		errdp_("#", &record[prvptr + 2], (ftnlen)1);
		sigerr_("SPICE(BADQUATSIGN)", (ftnlen)18);
		chkout_("CKE05", (ftnlen)5);
		return 0;
	    }
	    prvptr = newptr;
	}
    }
    if (subtyp == 1) {

/*        We perform Lagrange interpolation on each quaternion */
/*        component, and obtain quaternion derivatives from the */
/*        interpolating polynomials.  The quaternion and derivative */
/*        gives us angular velocity. */

/*        We'll transpose the pointing information in the input record so */
/*        that contiguous pieces of it can be shoved directly into the */
/*        interpolation routine LGRINT.  We allow LGRINT to overwrite */
/*        the state values in the input record, since this saves local */
/*        storage and does no harm.  (See the header of LGRINT for a */
/*        description of its work space usage.) */

	n = i_dnnt(&record[2]);
	xpsgip_(&packsz, &n, &record[4]);

/*        We interpolate each state component in turn. */

	xstart = n * packsz + 5;
	i__1 = packsz;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    ystart = n * (i__ - 1) + 5;
	    lgrind_(&n, &record[xstart - 1], &record[ystart - 1], work, &
		    sclkdp, &state[(i__2 = i__ - 1) < 8 && 0 <= i__2 ? i__2 : 
		    s_rnge("state", i__2, "cke05_", (ftnlen)661)], &state[(
		    i__3 = i__ + 3) < 8 && 0 <= i__3 ? i__3 : s_rnge("state", 
		    i__3, "cke05_", (ftnlen)661)]);
	}

/*        The output quaternion is a unitized version of the */
/*        interpolated state. */

	mags = vnormg_(state, &c__4);
	if (mags == 0.) {
	    setmsg_("Quaternion magnitude at SCLK # was zero.", (ftnlen)40);
	    errdp_("#", &sclkdp, (ftnlen)1);
	    sigerr_("SPICE(DIVIDEBYZERO)", (ftnlen)19);
	    chkout_("CKE05", (ftnlen)5);
	    return 0;
	}
	d__1 = 1. / mags;
	vsclg_(&d__1, state, &c__4, q);
	if (*needav) {

/*           Find the time derivative of the unit quaternion: */
/*           Letting S represent the quaternion portion of STATE, we */
/*           have */

/*              Q = S/||S|| */


/*           Then letting < , > denote the 4-dimensional inner product */
/*           operator, we have */


/*                         d(S)/dt      < Q, d(S)/dt > */
/*              d(Q)/dt =  -------  -   -------------- * Q */
/*                          ||S||            ||S|| */


	    moved_(&state[4], &c__4, ds);
	    d__1 = 1. / mags;
	    vsclg_(&d__1, ds, &c__4, sclddq);
	    d__1 = vdotg_(q, ds, &c__4) / mags;
	    vsclg_(&d__1, q, &c__4, radtrm);
	    vsubg_(sclddq, radtrm, &c__4, dq);

/*           Derive angular velocity from Q and dQ/dt: */

	    qdq2av_(q, dq, av);

/*           Scale the AV from radians/tick to radians/second. */

	    d__1 = 1. / rate;
	    vsclip_(&d__1, av);
	}

/*        Q and if required AV have been assigned. */

    } else if (subtyp == 3) {

/*        This is the easiest case:  we perform Lagrange interpolation */
/*        on each quaternion or angular velocity component. */

/*        We'll transpose the pointing information in the input record so */
/*        that contiguous pieces of it can be shoved directly into the */
/*        interpolation routine LGRINT.  We allow LGRINT to overwrite */
/*        the state values in the input record, since this saves local */
/*        storage and does no harm.  (See the header of LGRINT for a */
/*        description of its work space usage.) */

	n = i_dnnt(&record[2]);
	xpsgip_(&packsz, &n, &record[4]);

/*        We interpolate each state component in turn. */

	xstart = n * packsz + 5;
	if (*needav) {
	    ub = packsz;
	} else {
	    ub = 4;
	}
	i__1 = ub;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    ystart = n * (i__ - 1) + 5;
	    state[(i__2 = i__ - 1) < 8 && 0 <= i__2 ? i__2 : s_rnge("state", 
		    i__2, "cke05_", (ftnlen)763)] = lgrint_(&n, &record[
		    xstart - 1], &record[ystart - 1], locrec, &sclkdp);
	}

/*        The output quaternion is a unitized version of the */
/*        interpolated state. */

	vhatg_(state, &c__4, q);
	if (*needav) {

/*           The angular velocity already is in units of radians/second. */

	    vequ_(&state[4], av);
	}

/*        Q and if required AV have been assigned. */

    } else {

/*        We have a Hermite-style subtype.  Whether it's subtype 0 */
/*        or 2, we perform Hermite interpolation on the quaternions. */

/*        We interpolate each quaternion component in turn.  Attitude and */
/*        angular velocity are interpolated separately. */

	xstart = packsz * n + 5;
	for (i__ = 1; i__ <= 4; ++i__) {
	    i__1 = n;
	    for (j = 1; j <= i__1; ++j) {

/*              For the Jth input packet, copy the Ith position and */
/*              velocity components into the local record buffer RECORD. */

/*              In order to perform Hermite interpolation, the */
/*              quaternions and quaternion derivatives must have a */
/*              common time scale. So prior to interpolation, we scale */
/*              the units of the quaternion derivatives from radians/sec */
/*              to radians/tick. */

		from = packsz * (j - 1) + 4 + i__;
		to = (j << 1) - 1;
		locrec[(i__2 = to - 1) < 340 && 0 <= i__2 ? i__2 : s_rnge(
			"locrec", i__2, "cke05_", (ftnlen)815)] = record[from 
			- 1];
		locrec[(i__2 = to) < 340 && 0 <= i__2 ? i__2 : s_rnge("locrec"
			, i__2, "cke05_", (ftnlen)816)] = record[from + 3] * 
			rate;
	    }

/*           Interpolate the Ith quaternion and quaternion derivative */
/*           components. */

	    hrmint_(&n, &record[xstart - 1], locrec, &sclkdp, work, &state[(
		    i__1 = i__ - 1) < 8 && 0 <= i__1 ? i__1 : s_rnge("state", 
		    i__1, "cke05_", (ftnlen)824)], &state[(i__2 = i__ + 3) < 
		    8 && 0 <= i__2 ? i__2 : s_rnge("state", i__2, "cke05_", (
		    ftnlen)824)]);
	}

/*        The output quaternion is a unitized version of the */
/*        interpolated state. */

	mags = vnormg_(state, &c__4);
	if (mags == 0.) {
	    setmsg_("Quaternion magnitude at SCLK # was zero.", (ftnlen)40);
	    errdp_("#", &sclkdp, (ftnlen)1);
	    sigerr_("SPICE(DIVIDEBYZERO)", (ftnlen)19);
	    chkout_("CKE05", (ftnlen)5);
	    return 0;
	}
	d__1 = 1. / mags;
	vsclg_(&d__1, state, &c__4, q);
	if (*needav) {
	    if (subtyp == 0) {

/*              Find the time derivative of the unit quaternion: */
/*              Letting S represent the quaternion portion of STATE, we */
/*              have */

/*                 Q = S/||S|| */


/*              Then letting < , > denote the 4-dimensional inner product */
/*              operator, we have */


/*                            d(S)/dt      < Q, d(S)/dt > */
/*                 d(Q)/dt =  -------  -   -------------- * Q */
/*                             ||S||            ||S|| */


		moved_(&state[4], &c__4, ds);
		d__1 = 1. / mags;
		vsclg_(&d__1, ds, &c__4, sclddq);
		d__1 = vdotg_(q, ds, &c__4) / mags;
		vsclg_(&d__1, q, &c__4, radtrm);
		vsubg_(sclddq, radtrm, &c__4, dq);

/*              Derive angular velocity from Q and dQ/dt: */

		qdq2av_(q, dq, av);

/*              Scale the AV from radians/tick to radians/second. */

		d__1 = 1. / rate;
		vsclip_(&d__1, av);
	    } else {

/*              This is subtype 2; we perform Hermite interpolation on */
/*              the angular velocity and its derivative. */

/*              Now interpolate angular velocity, using separate angular */
/*              velocity data and angular acceleration. */

		for (i__ = 1; i__ <= 3; ++i__) {
		    i__1 = n;
		    for (j = 1; j <= i__1; ++j) {

/*                    For the Jth input packet, copy the Ith position */
/*                    and velocity components into the local record */
/*                    buffer LOCREC.  Note that, as with quaternion */
/*                    derivatives, we must scale angular acceleration */
/*                    from radians/sec**2 to radians/(sec*tick) before */
/*                    interpolating. */

			from = packsz * (j - 1) + 12 + i__;
			to = (j << 1) - 1;
			locrec[(i__2 = to - 1) < 340 && 0 <= i__2 ? i__2 : 
				s_rnge("locrec", i__2, "cke05_", (ftnlen)911)]
				 = record[from - 1];
			locrec[(i__2 = to) < 340 && 0 <= i__2 ? i__2 : s_rnge(
				"locrec", i__2, "cke05_", (ftnlen)912)] = 
				record[from + 2] * rate;
		    }

/*                 Interpolate the Ith angular velocity and angular */
/*                 acceleration components of the attitude. We'll */
/*                 capture the result in a temporary buffer, then */
/*                 transfer the velocity to the output argument AV. */

		    hrmint_(&n, &record[xstart - 1], locrec, &sclkdp, work, &
			    vbuff[(i__1 = i__ - 1) < 6 && 0 <= i__1 ? i__1 : 
			    s_rnge("vbuff", i__1, "cke05_", (ftnlen)922)], &
			    vbuff[(i__2 = i__ + 2) < 6 && 0 <= i__2 ? i__2 : 
			    s_rnge("vbuff", i__2, "cke05_", (ftnlen)922)]);
		}

/*              Fill in the angular velocity in the output angular */
/*              velocity vector using the results of interpolating */
/*              velocity and acceleration. */

/*              The angular velocity is already in units of */
/*              radians/second. */

		vequ_(vbuff, av);
	    }

/*           We've handled the type 0 and type 2 cases. */

	}

/*        We've computed the angular velocity AV for the Hermite */
/*        subtypes, if a.v. was requested. */

    }

/*     We've handled all four subtypes. */


/*     Produce a C-matrix from the interpolated quaternion. Set CLKOUT. */

    q2m_(q, cmat);
    *clkout = record[0];
    chkout_("CKE05", (ftnlen)5);
    return 0;
} /* cke05_ */

