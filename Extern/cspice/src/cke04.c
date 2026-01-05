/* cke04.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__4 = 4;

/* $Procedure CKE04 ( C-kernel, evaluate pointing record, type 4 ) */
/* Subroutine */ int cke04_(logical *needav, doublereal *record, doublereal *
	cmat, doublereal *av, doublereal *clkout)
{
    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer ideg[7];
    doublereal qout[4];
    integer i__;
    doublereal q[4];
    extern /* Subroutine */ int vhatg_(doublereal *, integer *, doublereal *);
    integer basadd;
    extern /* Subroutine */ int chbval_(doublereal *, integer *, doublereal *,
	     doublereal *, doublereal *), q2m_(doublereal *, doublereal *);

/* $ Abstract */

/*     Evaluate a pointing record returned by CKR04 from a CK type 4 */
/*     segment. Return the C-matrix and angular velocity vector */
/*     associated with the time CLKOUT. */

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

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     NEEDAV     I   .TRUE. if angular velocity is requested. */
/*     RECORD     I   Data type 4 pointing record. */
/*     CMAT       O   C-matrix. */
/*     AV         O   Angular velocity vector. */
/*     CLKOUT     O   SCLK associated with C-matrix. */

/* $ Detailed_Input */

/*     NEEDAV   is .TRUE. if angular velocity is requested. */

/*     RECORD   is a set of double precision numbers returned by */
/*              CKR04. RECORD must have the following structure: */

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

/* $ Detailed_Output */

/*     CMAT     is a rotation matrix that transforms the components */
/*              of a vector expressed in the inertial frame given in */
/*              the segment to components expressed in the instrument */
/*              fixed frame at the returned time. */

/*              Thus, if a vector v has components x, y, z in the */
/*              inertial frame, then v has components x', y', z' in */
/*              the instrument fixed frame where: */

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

/*              which is in terms of the inertial coordinate frame */
/*              specified in the segment descriptor. */

/*     CLKOUT   is the encoded SCLK associated with the returned */
/*              C-matrix and angular velocity vector. */

/* $ Parameters */

/*     See 'ckparam.inc'. */

/* $ Exceptions */

/*     Error free. */

/*     1)  No checking is done to determine whether RECORD is valid. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     For a detailed description of the structure of a type 4 pointing */
/*     segment, see the CK Required Reading file. */

/*     The work done by CKE04 is to calculate quaternion and angular */
/*     velocity components using Chebyshev polynomial approximation */
/*     parameters. The second step of evaluation is to convert the */
/*     pointing portion of the record from quaternion form to C-matrix */
/*     form. */

/*     The angular velocity vector will only be returned if it has been */
/*     requested. In other words, if NEEDAV is .TRUE., the routine will */
/*     expect the angular velocity component of the record to be */
/*     present. */

/* $ Examples */

/*     The CKRnn routines are usually used in tandem with the CKEnn */
/*     routines, which evaluate the record returned by CKRnn to give */
/*     the pointing information and output time. */

/*     The following code fragment searches through all of the segments */
/*     in a file applicable to the Mars Global Surveyor spacecraft bus */
/*     that are of data type 4, for a particular spacecraft clock time. */
/*     It then evaluates the pointing for that epoch and prints the */
/*     result. */

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
/*     C     Search from the beginning of the CK file through all */
/*     C     of the segments. */
/*     C */
/*           CALL DAFBFS( HANDLE ) */
/*           CALL DAFFNA( SFND   ) */

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

/*              CALL DAFFNA ( SFND ) */

/*           END DO */

/* $ Restrictions */

/*     1)  No checking is done on the input RECORD. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */
/*     E.D. Wright        (JPL) */
/*     Y.K. Zaiko         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.3, 12-AUG-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.2, 18-APR-2014 (BVS) */

/*        Minor header edits. */

/* -    SPICELIB Version 1.0.1, 22-AUG-2006 (EDW) */

/*        Replaced references to LDPOOL with references */
/*        to FURNSH. */

/* -    SPICELIB Version 1.0.0, 05-MAY-1999 (YKZ) (BVS) */

/* -& */
/* $ Index_Entries */

/*     evaluate CK type_4 pointing data record */

/* -& */

/*     Local variables */


/*     Initial values. */

    av[0] = 0.;
    av[1] = 0.;
    av[2] = 0.;

/*     Read numbers of polynomial coefficients from input record to */
/*     local integer array. */

    for (i__ = 1; i__ <= 7; ++i__) {
	ideg[(i__1 = i__ - 1) < 7 && 0 <= i__1 ? i__1 : s_rnge("ideg", i__1, 
		"cke04_", (ftnlen)377)] = (integer) record[i__ + 2];
    }

/*     Evaluate polynomial function for quaternion components at time */
/*     RECORD( 1 ). */

    basadd = 11;
    for (i__ = 1; i__ <= 4; ++i__) {
	i__3 = ideg[(i__1 = i__ - 1) < 7 && 0 <= i__1 ? i__1 : s_rnge("ideg", 
		i__1, "cke04_", (ftnlen)388)] - 1;
	chbval_(&record[basadd - 1], &i__3, &record[1], record, &q[(i__2 = 
		i__ - 1) < 4 && 0 <= i__2 ? i__2 : s_rnge("q", i__2, "cke04_",
		 (ftnlen)388)]);
	basadd += ideg[(i__1 = i__ - 1) < 7 && 0 <= i__1 ? i__1 : s_rnge(
		"ideg", i__1, "cke04_", (ftnlen)390)];
    }

/*     Normalize quaternion. */

    vhatg_(q, &c__4, qout);

/*     Convert the quaternion to a C-matrix. */

    q2m_(qout, cmat);
    *clkout = record[0];

/*     Check if angular velocities have to be evaluated, then */
/*     evaluate them. */

    if (*needav) {
	for (i__ = 5; i__ <= 7; ++i__) {
	    i__3 = ideg[(i__1 = i__ - 1) < 7 && 0 <= i__1 ? i__1 : s_rnge(
		    "ideg", i__1, "cke04_", (ftnlen)414)] - 1;
	    chbval_(&record[basadd - 1], &i__3, &record[1], record, &av[(i__2 
		    = i__ - 5) < 3 && 0 <= i__2 ? i__2 : s_rnge("av", i__2, 
		    "cke04_", (ftnlen)414)]);
	    basadd += ideg[(i__1 = i__ - 1) < 7 && 0 <= i__1 ? i__1 : s_rnge(
		    "ideg", i__1, "cke04_", (ftnlen)416)];
	}
    }

/*     All done. */

    return 0;
} /* cke04_ */

