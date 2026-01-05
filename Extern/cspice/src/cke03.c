/* cke03.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__4 = 4;
static integer c__3 = 3;

/* $Procedure CKE03  ( C-kernel, evaluate pointing record, data type 3 ) */
/* Subroutine */ int cke03_(logical *needav, doublereal *record, doublereal *
	cmat, doublereal *av, doublereal *clkout)
{
    /* System generated locals */
    doublereal d__1;

    /* Local variables */
    doublereal frac, axis[3];
    extern /* Subroutine */ int vequ_(doublereal *, doublereal *), mtxm_(
	    doublereal *, doublereal *, doublereal *), mxmt_(doublereal *, 
	    doublereal *, doublereal *);
    doublereal cmat1[9]	/* was [3][3] */, cmat2[9]	/* was [3][3] */, t, 
	    angle, delta[9]	/* was [3][3] */;
    extern /* Subroutine */ int chkin_(char *, ftnlen), moved_(doublereal *, 
	    integer *, doublereal *), vlcom_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *);
    doublereal q1[4], q2[4], t1, t2;
    extern logical failed_(void);
    extern /* Subroutine */ int raxisa_(doublereal *, doublereal *, 
	    doublereal *), axisar_(doublereal *, doublereal *, doublereal *), 
	    chkout_(char *, ftnlen);
    doublereal av1[3], av2[3];
    extern logical return_(void);
    extern /* Subroutine */ int q2m_(doublereal *, doublereal *);
    doublereal rot[9]	/* was [3][3] */;

/* $ Abstract */

/*     Evaluate a pointing record returned by CKR03 from a CK type 3 */
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
/*     ROTATION */

/* $ Keywords */

/*     POINTING */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     NEEDAV     I   .TRUE. if angular velocity is requested. */
/*     RECORD     I   Data type 3 pointing record. */
/*     CMAT       O   C-matrix. */
/*     AV         O   Angular velocity vector. */
/*     CLKOUT     O   SCLK associated with C-matrix. */

/* $ Detailed_Input */

/*     NEEDAV   is .TRUE. if angular velocity is requested. */

/*     RECORD   is a set of double precision numbers returned by CKR03 */
/*              that contain sufficient information from a type 3 CK */
/*              segment to evaluate the C-matrix and the angular */
/*              velocity vector at a particular time. Depending on */
/*              the contents of RECORD, this routine will either */
/*              interpolate between two pointing instances that */
/*              bracket a request time, or it will simply return the */
/*              pointing given by a single pointing instance. */

/*              When pointing at the request time can be determined */
/*              by linearly interpolating between the two pointing */
/*              instances that bracket that time, the bracketing */
/*              pointing instances are returned in RECORD as follows: */

/*                 RECORD( 1  ) = Left bracketing SCLK time. */

/*                 RECORD( 2  ) = lq0  \ */
/*                 RECORD( 3  ) = lq1   \    Left bracketing */
/*                 RECORD( 4  ) = lq2   /      quaternion. */
/*                 RECORD( 5  ) = lq3  / */

/*                 RECORD( 6  ) = lav1 \     Left bracketing */
/*                 RECORD( 7  ) = lav2  |    angular velocity */
/*                 RECORD( 8  ) = lav3 /       ( optional ) */

/*                 RECORD( 9  ) = Right bracketing SCLK time. */

/*                 RECORD( 10 ) = rq0  \ */
/*                 RECORD( 11 ) = rq1   \    Right bracketing */
/*                 RECORD( 12 ) = rq2   /       quaternion. */
/*                 RECORD( 13 ) = rq3  / */

/*                 RECORD( 14 ) = rav1 \     Right bracketing */
/*                 RECORD( 15 ) = rav2  |    angular velocity */
/*                 RECORD( 16 ) = rav3 /       ( optional ) */

/*                 RECORD( 17 ) = pointing request time */

/*              The quantities lq0 - lq3 and rq0 - rq3 are the */
/*              components of the quaternions that represent the */
/*              C-matrices associated with the times that bracket */
/*              the requested time. */

/*              The quantities lav1, lav2, lav3 and rav1, rav2, rav3 */
/*              are the components of the angular velocity vectors at */
/*              the respective bracketing times. The components of the */
/*              angular velocity vectors are specified relative to the */
/*              inertial reference frame of the segment. */

/*              When the routine is to simply return the pointing */
/*              given by a particular pointing instance, then the */
/*              values of that pointing instance are returned in both */
/*              parts of RECORD ( i.e. RECORD(1-9) and RECORD(10-16) ). */

/* $ Detailed_Output */

/*     CMAT     is a rotation matrix that transforms the components */
/*              of a vector expressed in the inertial frame given in */
/*              the segment to components expressed in the instrument */
/*              fixed frame at the returned time. */

/*              Thus, if a vector v has components x, y, z in the */
/*              inertial frame, then v has components x', y', z' in the */
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

/*              which is in terms of the inertial coordinate frame */
/*              specified in the segment descriptor. */

/*     CLKOUT   is the encoded SCLK associated with the returned */
/*              C-matrix and angular velocity vector. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If inputs are invalid or otherwise in appropriate, such that */
/*         the computed matrix is not a rotation matrix, an error is */
/*         signaled by a routine in the call tree of this routine. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     If the array RECORD contains pointing instances that bracket the */
/*     request time then CKE03 will linearly interpolate between those */
/*     two values to obtain pointing at the request time. If the */
/*     pointing instances in RECORD are for the same time, then this */
/*     routine will simply unpack the record and convert the quaternion */
/*     to a C-matrix. */

/*     The linear interpolation performed by this routine is defined */
/*     as follows: */

/*     1)  Let t be the time for which pointing is requested and */
/*         let CMAT1 and CMAT2 be C-matrices associated with times */
/*         t1 and t2 where: */

/*                t1 < t2,  and  t1 <= t,  and  t <= t2. */

/*     2)  Assume that the spacecraft frame rotates about a fixed */
/*         axis at a constant angular rate from time t1 to time t2. */
/*         The angle and rotation axis can be obtained from the */
/*         rotation matrix ROT12 where: */

/*                            T                       T */
/*                       CMAT2   =  ROT12    *   CMAT1 */

/*            or */
/*                                       T */
/*                       ROT12   =  CMAT2    *   CMAT1 */


/*                       ROT12   ==> ( ANGLE, AXIS ) */


/*     3)  To obtain pointing at time t, rotate the spacecraft frame */
/*         about the vector AXIS from its orientation at time t1 by the */
/*         angle THETA where: */

/*                                            ( t  - t1 ) */
/*                       THETA  =  ANGLE  *   ----------- */
/*                                            ( t2 - t1 ) */

/*     4)  Thus if ROT1t is the matrix that rotates vectors by the */
/*         angle THETA about the vector AXIS, then the output C-matrix */
/*         is given by: */

/*                           T                     T */
/*                       CMAT  =  ROT1t   *   CMAT1 */

/*                                                 T */
/*                       CMAT  =  CMAT1   *   ROT1t */


/*     5)  The angular velocity is treated independently of the */
/*         C-matrix. If it is requested, then the AV at time t is */
/*         the weighted average of the angular velocity vectors at */
/*         the times t1 and t2: */

/*                          ( t  - t1 ) */
/*                    W  =  ----------- */
/*                          ( t2 - t1 ) */


/*                    AV  = ( 1 - W ) * AV1   +   W * AV2 */

/* $ Examples */

/*     The CKRnn routines are usually used in tandem with the CKEnn */
/*     routines, which evaluate the record returned by CKRnn to give */
/*     the pointing information and output time. */

/*     The following code fragment searches through all of the segments */
/*     in a file applicable to the Mars Observer spacecraft bus that */
/*     are of data type 3, for a particular spacecraft clock time. */
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


/*           SC     = -94 */
/*           INST   = -94000 */
/*           DTYPE  =  3 */
/*           NEEDAV = .FALSE. */

/*     C */
/*     C     Load the MO SCLK kernel and the C-kernel. */
/*     C */
/*           CALL FURNSH ( 'MO_SCLK.TSC'       ) */
/*           CALL DAFOPR ( 'MO_CK.BC',  HANDLE ) */
/*     C */
/*     C     Get the spacecraft clock time. Then encode it for use */
/*     C     in the C-kernel. */
/*     C */
/*           WRITE (*,*) 'Enter spacecraft clock time string:' */
/*           READ (*,FMT='(A)') SCLKCH */

/*           CALL SCENCD ( SC, SCLKCH, SCLKDP ) */
/*     C */
/*     C     Use a tolerance of 2 seconds ( half of the nominal */
/*     C     separation between MO pointing instances ). */
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

/*              CALL DAFGN ( IDENT                 ) */
/*              CALL DAFGS ( DESCR                 ) */
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


/*                 CALL CKR03 ( HANDLE, DESCR, SCLKDP, TOL, NEEDAV, */
/*          .                   RECORD, FND ) */

/*                 IF ( FND ) THEN */

/*                    CALL CKE03 (NEEDAV,RECORD,CMAT,AV,CLKOUT) */

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

/*     1)  No explicit checking is done on the input RECORD. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     J.M. Lynch         (JPL) */
/*     F.S. Turner        (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.1.0, 12-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 2.0.1, 22-AUG-2006 (EDW) */

/*        Replaced references to LDPOOL with references */
/*        to FURNSH. */

/* -    SPICELIB Version 2.0.0, 13-JUN-2002 (FST) */

/*        This routine now participates in error handling properly. */

/* -    SPICELIB Version 1.0.0, 25-NOV-1992 (JML) */

/* -& */
/* $ Index_Entries */

/*     evaluate CK type_3 pointing data record */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 2.0.0, 13-JUN-2002 (FST) */

/*        Calls to CHKIN and CHKOUT in the standard SPICE error */
/*        handling style were added. Versions prior to 2.0.0 */
/*        were error free, however changes to RAXISA from error */
/*        free to error signaling forced this update. */

/*        Additionally, FAILED is now checked after the call to */
/*        RAXISA. This prevents garbage from being placed into */
/*        the output arguments. */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("CKE03", (ftnlen)5);
    }

/*     Unpack the record, for easier reading. */

    t = record[16];
    t1 = record[0];
    t2 = record[8];
    moved_(&record[1], &c__4, q1);
    moved_(&record[5], &c__3, av1);
    moved_(&record[9], &c__4, q2);
    moved_(&record[13], &c__3, av2);

/*     If T1 and T2 are the same then no interpolation or extrapolation */
/*     is performed.  Simply convert the quaternion to a C-matrix and */
/*     return. */

    if (t1 == t2) {
	q2m_(q1, cmat);
	*clkout = t1;
	if (*needav) {
	    vequ_(av1, av);
	}
	chkout_("CKE03", (ftnlen)5);
	return 0;
    }

/*     Interpolate between the two pointing instances to obtain pointing */
/*     at the request time. */


/*     Calculate what fraction of the interval the request time */
/*     represents. */

    frac = (t - t1) / (t2 - t1);

/*     Convert the left and right quaternions to C-matrices. */

    q2m_(q1, cmat1);
    q2m_(q2, cmat2);

/*     Find the matrix that rotates the spacecraft instrument frame from */
/*     the orientation specified by CMAT1 to that specified by CMAT2. */
/*     Then find the axis and angle of that rotation matrix. */

/*             T                      T */
/*        CMAT2   =    ROT    *  CMAT1 */

/*                          T */
/*        ROT     =    CMAT2  *  CMAT1 */

    mtxm_(cmat2, cmat1, rot);
    raxisa_(rot, axis, &angle);
    if (failed_()) {
	chkout_("CKE03", (ftnlen)5);
	return 0;
    }

/*     Calculate the matrix that rotates vectors about the vector AXIS */
/*     by the angle ANGLE * FRAC. */

    d__1 = angle * frac;
    axisar_(axis, &d__1, delta);

/*     The interpolated pointing at the request time is given by CMAT */
/*     where: */

/*              T                    T */
/*          CMAT   =  DELTA  *  CMAT1 */

/*     and */
/*                                   T */
/*          CMAT   =  CMAT1  *  DELTA */

    mxmt_(cmat1, delta, cmat);

/*     Set CLKOUT equal to the time that pointing is being returned. */

    *clkout = t;

/*     If angular velocity is requested then take a weighted average */
/*     of the angular velocities at the left and right endpoints. */

    if (*needav) {
	d__1 = 1. - frac;
	vlcom_(&d__1, av1, &frac, av2, av);
    }
    chkout_("CKE03", (ftnlen)5);
    return 0;
} /* cke03_ */

