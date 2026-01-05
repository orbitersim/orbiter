/* cke01.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure CKE01 ( CK evaluate pointing record, data type 1 ) */
/* Subroutine */ int cke01_(logical *needav, doublereal *record, doublereal *
	cmat, doublereal *av, doublereal *clkout)
{
    extern /* Subroutine */ int chkin_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int q2m_(doublereal *, doublereal *);

/* $ Abstract */

/*     Evaluate a pointing record returned by CKR01 from a CK data type 1 */
/*     segment. Return the C-matrix and optionally the angular velocity */
/*     vector associated with the time CLKOUT. */

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
/*     ROTATION */

/* $ Keywords */

/*     POINTING */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     NEEDAV     I   .TRUE. if angular velocity vector is required. */
/*     RECORD     I   Data type 1 pointing record. */
/*     CMAT       O   C-matrix. */
/*     AV         O   Angular velocity vector. */
/*     CLKOUT     O   Output spacecraft clock time. */

/* $ Detailed_Input */

/*     NEEDAV   is .TRUE. when angular velocity data is requested. */

/*     RECORD   is a set of double precision numbers returned by CKR01 */
/*              that contain sufficient information from a data type */
/*              1 pointing segment to evaluate the C-matrix and */
/*              possibly the angular velocity vector (if NEEDAV is */
/*              .TRUE.) for a particular instance. */

/*              The contents of RECORD are as follows: */

/*                 RECORD( 1 ) = CLKOUT */

/*                 RECORD( 2 ) = q0 */
/*                 RECORD( 3 ) = q1 */
/*                 RECORD( 4 ) = q2 */
/*                 RECORD( 5 ) = q3 */

/*                 RECORD( 6 ) = Av1  ] */
/*                 RECORD( 7 ) = Av2  |-- Optional */
/*                 RECORD( 8 ) = Av3  ] */


/*              The quantities q0 - q3 represent a quaternion. */
/*              The quantities Av1, Av2, and Av3 represent the angular */
/*              velocity vector. */

/*              CLKOUT is the encoded spacecraft clock time */
/*              associated with the quaternion and, optionally, the */
/*              angular velocity vector. */

/* $ Detailed_Output */

/*     CMAT     is a rotation matrix that transforms the components of */
/*              of a vector expressed in the reference frame given in */
/*              the segment to components expressed in the instrument */
/*              fixed frame at time CLKOUT. */

/*              Thus, if a vector v has components x, y, z in the */
/*              reference frame, then v has components x', y', z' in */
/*              the instrument fixed frame at time CLKOUT: */

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

/*     AV       is the angular velocity vector. This is returned only */
/*              if it has been requested, as indicated by NEEDAV. In */
/*              other words, if NEEDAV is .TRUE., the angular velocity */
/*              portion of RECORD must be present. */

/*              The angular velocity vector is the vector whose */
/*              direction gives the right-handed axis about which */
/*              the reference frame tied to the instrument is */
/*              instantaneously rotating at time CLKOUT. */

/*              The angular velocity vector is returned in component */
/*              form */

/*                       AV = [ AV1  , AV2  , AV3  ] */

/*              which is in terms of the reference coordinate frame */
/*              specified in the segment descriptor. */

/*              The magnitude of AV is the magnitude of the instantane- */
/*              ous velocity of the rotation, in radians per second. */

/*     CLKOUT   is the encoded spacecraft clock time associated with the */
/*              returned C-matrix and, optionally, the returned angular */
/*              velocity vector. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  No checking is done to determine whether RECORD is a valid */
/*         record. */

/*     2)  If NEEDAV is .TRUE., then RECORD is assumed to contain angular */
/*         velocity data. No checking is performed to verify this */
/*         assumption. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     For a detailed description of the structure of a type 1 pointing */
/*     segment, see the CK Required Reading file. */

/*     The only real work done by CKE01 is to convert the pointing */
/*     portion of the record from quaternion form to C-matrix form. */

/*     The angular velocity vector will only be returned if it has been */
/*     requested. In other words, if NEEDAV is .TRUE., the routine will */
/*     expect the angular velocity component of the record to be present. */

/* $ Examples */

/*     A call to a CKEnn routine is almost always preceded by a call to */
/*     the corresponding CKRnn routine, which gets the logical record */
/*     that CKEnn evaluates. */

/*     The following code fragment searches through a file represented */
/*     by HANDLE for all segments applicable to the Voyager 2 wide angle */
/*     camera, for a particular spacecraft clock time, which have data */
/*     type 1. It then evaluates the pointing for that epoch and prints */
/*     the result. */

/*     C */
/*     C     - Get the spacecraft clock time. Must encode it for use */
/*     C       in the C-kernel. */
/*     C */
/*     C     - Set the time tolerance high to catch anything close to */
/*     C       the input time. */
/*     C */
/*     C     - We don't need angular velocity data. */
/*     C */

/*           SC     = -32 */
/*           INST   = -32002 */
/*           TOL    =  1000.D0 */
/*           NEEDAV = .FALSE. */
/*           DTYPE  =  1 */
/*     C */
/*     C     Load the Voyager 2 spacecraft clock kernel and the C-kernel. */
/*     C */
/*           CALL FURNSH ( 'VGR_SCLK.TSC'        ) */
/*           CALL DAFOPR ( 'VGR2_CK.BC',  HANDLE ) */
/*     C */
/*     C     Convert the input request time to ticks. */
/*     C */
/*           WRITE (*,*) 'Enter spacecraft clock time string:' */
/*           READ (*,FMT='(A)') SCLKCH */
/*           CALL SCENCD ( SC, SCLKCH, SCLKDP ) */

/*     C */
/*     C     Search from the beginning through all segments. */
/*     C */
/*           CALL DAFBFS ( HANDLE ) */
/*           CALL DAFFNA ( SFND   ) */

/*           DO WHILE ( SFND ) */

/*              CALL DAFGN ( IDENT                 ) */
/*              CALL DAFGS ( DESCR                 ) */
/*              CALL DAFUS ( DESCR, 2, 6, DCD, ICD ) */

/*              IF (        INST          .EQ. ICD( 1 ) */
/*          .               DTYPE         .EQ. ICD( 3 ) */
/*          .        .AND.  SCLKDP + TOL  .GE. DCD( 1 ) */
/*          .        .AND.  SCLKDP - TOL  .LE. DCD( 2 )  ) THEN */

/*                 CALL CKR01 ( HANDLE, DESCR, SCLKDP, TOL, NEEDAV, */
/*          .                   RECORD, FOUND ) */

/*                 IF ( FOUND ) THEN */

/*                    CALL CKE01 ( NEEDAV, RECORD, CMAT, AV, CLKOUT ) */

/*                    WRITE (*,*) 'Segment descriptor and identifier:' */
/*                    WRITE (*,*) DCD, ICD */
/*                    WRITE (*,*) IDENT */

/*                    WRITE (*,*) 'C-matrix:' */
/*                    WRITE (*,*) CMAT */

/*                 END IF */

/*              END IF */

/*              CALL DAFFNA ( SFND ) */

/*           END DO */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     J.M. Lynch         (JPL) */
/*     M.J. Spencer       (JPL) */
/*     W.L. Taber         (JPL) */
/*     R.E. Thurman       (JPL) */
/*     I.M. Underwood     (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.3.0, 12-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.2.1, 22-AUG-2006 (EDW) */

/*        Replaced header references to LDPOOL with references */
/*        to FURNSH. */

/* -    SPICELIB Version 1.2.0, 14-NOV-1995 (WLT) */

/*        Changed "inertial frame" to simply reference frame to */
/*        reflect new capabilities of the SPICE system. */

/* -    SPICELIB Version 1.1.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.1.0, 30-AUG-1991 (MJS) (JML) */

/*        1) Previously, in the standard SPICE error handling, the */
/*           logical function RETURN was not written as a function; */
/*           it is now written as a function. */

/*        2) The example program was changed so that the tolerance */
/*           and data type are used in selecting which segments to read. */

/*        3) It was specified that the angular velocity vector */
/*           gives the right-handed axis about which the instrument */
/*           frame rotates. */

/* -    SPICELIB Version 1.0.1, 02-NOV-1990 (JML) */

/*        The example program was corrected so that the input */
/*        instrument code was tested against ICD(1) instead of */
/*        ICD(3). */

/* -    SPICELIB Version 1.0.0, 07-SEP-1990 (RET) (IMU) */

/* -& */
/* $ Index_Entries */

/*     evaluate CK type_1 pointing data record */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.2.0, 14-NOV-1995 (WLT) */

/*        Changed "inertial frame" to simply reference frame to */
/*        reflect new capabilities of the SPICE system. */

/*        This change affects only documentation not code. */

/* -    SPICELIB Version 1.1.0, 30-AUG-1991 (MJS) (JML) */

/*        1) In the standard SPICE error handling, the line: */

/*              IF ( RETURN ) THEN */

/*           was changed to */

/*              IF ( RETURN() ) THEN */

/*        2) The example program was changed so that the tolerance */
/*           and data type are used in selecting  which segments to read. */

/*        3) It was specified that the angular velocity vector */
/*           gives the right-handed axis about which the instrument */
/*           frame rotates. */

/* -    SPICELIB Version 1.0.1, 02-NOV-1990 (JML) */

/*        1) The example program was corrected so that the input */
/*           instrument code was tested against ICD(1) instead of */
/*           ICD(3). */
/*        2) SCLK was removed from the Required Reading section. */

/* -    Beta Version 1.1.0, 29-AUG-1990 (MJS) (JEM) */

/*        The following changes were made as a result of the */
/*        NAIF CK Code and Documentation Review: */

/*        1) The argument SCLK was removed from the calling sequence. */
/*        2) Header was updated. */
/*        3) The call to the routine QUAT2M_3 was replaced by a call to */
/*           the routine Q2M. */

/* -    Beta Version 1.0.0, 18-MAY-1990 (RET) (IMU) */

/* -& */

/*     SPICELIB functions */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("CKE01", (ftnlen)5);
    }

/*     Dissect the record. */

    *clkout = record[0];
    q2m_(&record[1], cmat);
    if (*needav) {
	av[0] = record[5];
	av[1] = record[6];
	av[2] = record[7];
    }
    chkout_("CKE01", (ftnlen)5);
    return 0;
} /* cke01_ */

