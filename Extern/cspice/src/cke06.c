/* cke06.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure CKE06 ( C-Kernel, evaluate, type 6 ) */
/* Subroutine */ int cke06_(logical *needav, doublereal *record, doublereal *
	cmat, doublereal *av, doublereal *clkout)
{
    extern /* Subroutine */ int cke05_(logical *, doublereal *, doublereal *, 
	    doublereal *, doublereal *), chkin_(char *, ftnlen), chkout_(char 
	    *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Evaluate a single data record from a type 6 CK segment. */

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
/*     NEEDAV     I   .TRUE. if angular velocity is requested. */
/*     RECORD    I-O  Data type 6 record. */
/*     CMAT       O   C-matrix. */
/*     AV         O   Angular velocity vector. */
/*     CLKOUT     O   SCLK associated with C-matrix. */

/* $ Detailed_Input */

/*     NEEDAV   is .TRUE. if angular velocity is requested. */

/*     RECORD   is a record from a type 6 CK segment which, when */
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
/*                ck06.inc for details on CK type 6 packet contents. */

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

/*     1)  If the input record contains an unrecognized subtype code, an */
/*         error is signaled by a routine in the call tree of this */
/*         routine. */

/*     2)  If the record subtype is one for which quaternion derivatives */
/*         are stored (subtypes 0 and 2), and if the Ith quaternion in */
/*         the input record is farther than its negative from the (I-1)st */
/*         quaternion in the record, an error is signaled by a routine */
/*         in the call tree of this routine. */

/*         For subtypes 1 and 3, this condition is not considered an */
/*         error: the closer to the preceding quaternion of the two */
/*         quaternion representations is used for interpolation. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The exact format and structure of CK type 6 (MEX/Rosetta Attitude */
/*     file interpolation) CK segments is described in the CK Required */
/*     Reading. */

/* $ Examples */

/*     The CKEnn routines are almost always used in conjunction with */
/*     the corresponding CKRnn routines, which read the records from */
/*     CK files. */

/*     The following code fragment searches through all of the segments */
/*     in a file applicable to the Mars Express spacecraft bus that */
/*     are of data type 6, for a particular spacecraft clock time. */
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
/*           DTYPE  =  6 */
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


/*                 CALL CKR06 ( HANDLE, DESCR, SCLKDP, TOL, NEEDAV, */
/*          .                   RECORD, FND ) */

/*                 IF ( FND ) THEN */

/*                    CALL CKE06 (NEEDAV,RECORD,CMAT,AV,CLKOUT) */

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

/*     1)  This routine performs minimal error checking. The input data */
/*         are assumed to have been checked when the source CK file was */
/*         created. */

/*     2)  With the exception of the check described in item 2 of */
/*         the $Exceptions section above, the input data are assumed to */
/*         be suitable for the interpolation method specified by the */
/*         input record's subtype and packet count (which implies an */
/*         interpolating polynomial degree). */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 12-AUG-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.0, 14-MAR-2014 (NJB) (BVS) */

/* -& */
/* $ Index_Entries */

/*     evaluate type_6 ck_segment */

/* -& */

/*     SPICELIB functions */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("CKE06", (ftnlen)5);

/*     Given that our nominally type 6 input record is actually a */
/*     valid type 5 record, we let the type 5 evaluator do the */
/*     work. */

    cke05_(needav, record, cmat, av, clkout);
    chkout_("CKE06", (ftnlen)5);
    return 0;
} /* cke06_ */

