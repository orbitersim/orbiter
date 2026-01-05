/* ckpfs.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__6 = 6;

/* $Procedure CKPFS ( C-kernel, get pointing from segment ) */
/* Subroutine */ int ckpfs_(integer *handle, doublereal *descr, doublereal *
	sclkdp, doublereal *tol, logical *needav, doublereal *cmat, 
	doublereal *av, doublereal *clkout, logical *found)
{
    extern /* Subroutine */ int cke01_(logical *, doublereal *, doublereal *, 
	    doublereal *, doublereal *), cke02_(logical *, doublereal *, 
	    doublereal *, doublereal *, doublereal *), cke03_(logical *, 
	    doublereal *, doublereal *, doublereal *, doublereal *), cke04_(
	    logical *, doublereal *, doublereal *, doublereal *, doublereal *)
	    , cke05_(logical *, doublereal *, doublereal *, doublereal *, 
	    doublereal *), cke06_(logical *, doublereal *, doublereal *, 
	    doublereal *, doublereal *), ckr01_(integer *, doublereal *, 
	    doublereal *, doublereal *, logical *, doublereal *, logical *), 
	    ckr02_(integer *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, logical *), ckr03_(integer *, doublereal *, 
	    doublereal *, doublereal *, logical *, doublereal *, logical *), 
	    ckr04_(integer *, doublereal *, doublereal *, doublereal *, 
	    logical *, doublereal *, logical *), ckr05_(integer *, doublereal 
	    *, doublereal *, doublereal *, logical *, doublereal *, logical *)
	    , ckr06_(integer *, doublereal *, doublereal *, doublereal *, 
	    logical *, doublereal *, logical *);
    integer type__;
    extern /* Subroutine */ int chkin_(char *, ftnlen), dafus_(doublereal *, 
	    integer *, integer *, doublereal *, integer *);
    extern logical failed_(void);
    doublereal record[340];
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen);
    extern logical return_(void);
    doublereal dcd[2];
    integer icd[6];

/* $ Abstract */

/*     Evaluate pointing data from a segment for a given time. */

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
/*     HANDLE     I   CK file handle. */
/*     DESCR      I   Segment descriptor. */
/*     SCLKDP     I   Spacecraft clock time. */
/*     TOL        I   Time tolerance. */
/*     NEEDAV     I   .TRUE. when angular velocity data is requested. */
/*     CMAT       O   C-matrix. */
/*     AV         O   Angular velocity vector. */
/*     CLKOUT     O   Output spacecraft clock time. */
/*     FOUND      O   .TRUE. when requested pointing is available. */

/* $ Detailed_Input */

/*     HANDLE   is the handle of the binary CK file containing the */
/*              desired segment. The file should have been opened */
/*              for read access, either by CKLPF or DAFOPR. */

/*     DESCR    is the packed descriptor of the segment. */

/*     SCLKDP   is the encoded spacecraft clock time for which */
/*              pointing is desired. */

/*     TOL      is a time tolerance, measured in the same units as */
/*              encoded spacecraft clock. The C-matrix returned by */
/*              CKPFS is the one whose time is closest to SCLKDP and */
/*              within TOL units of SCLKDP. */

/*     NEEDAV   is .TRUE. when angular velocity data is requested. */

/* $ Detailed_Output */

/*     CMAT     is a rotation matrix that transforms the components of */
/*              of a vector expressed in the reference frame given in */
/*              the segment to components expressed in the instrument */
/*              fixed frame at time CLKOUT. */

/*              Thus, if a vector v has components x, y, z in the */
/*              CK base frame, then v has components x', y', z' in */
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
/*                           (Transpose of CMAT) */

/*     AV       is the angular velocity vector. This is returned only */
/*              if it has been requested, as indicated by NEEDAV. In */
/*              other words, if NEEDAV is .TRUE., then the pointing */
/*              records in the segment must contain AV data. */

/*              The angular velocity vector is the right-handed axis */
/*              about which the reference frame tied to the instrument */
/*              is instantaneously rotating at time CLKOUT. The */
/*              magnitude of AV is the magnitude of the instantaneous */
/*              velocity of the rotation, in radians per second. */

/*              The components of AV are given relative to the */
/*              reference frame specified in the segment descriptor. */

/*     CLKOUT   is the encoded spacecraft clock time associated with */
/*              the returned C-matrix and, optionally, the returned */
/*              angular velocity vector. */

/*     FOUND    is .TRUE. if a C-matrix and an angular velocity vector */
/*              (if requested) were found to satisfy the pointing */
/*              request. FOUND will be false otherwise. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the data type of the segment is not one of those supported */
/*         by this routine, the error SPICE(CKUNKNOWNDATATYPE) is */
/*         signaled. */

/*     2)  If the specified handle does not belong to any file that is */
/*         currently known to be open, an error is signaled by a routine */
/*         in the call tree of this routine. */

/*     3)  If DESCR is not a valid, packed descriptor of a segment in */
/*         the CK file specified by HANDLE, the results of this routine */
/*         are unpredictable. */

/*     4)  If TOL is negative, FOUND is .FALSE. */

/*     5)  If NEEDAV is .TRUE., but the segment doesn't contain AV data, */
/*         an error is signaled by a routine in the call tree of this */
/*         routine. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The structure of this routine is just a big case statement. Each */
/*     segment data type is supported by two routines: */

/*        CKRnn   which reads a single logical pointing record from a */
/*                segment of type nn. (A logical record is defined as */
/*                a collection of numbers sufficient to determine the */
/*                C-matrix, and optionally the angular velocity vector, */
/*                at the input time.) */

/*        CKEnn   which evaluates the pointing record returned by CKRnn */
/*                to give the C-matrix and optionally the angular */
/*                velocity vector at the input time. */

/*     The data type is determined from the segment descriptor, and the */
/*     appropriate routines are called. */

/* $ Examples */

/*     CKPFS allows you to be more selective than CKGP or CKGPAV about */
/*     choosing segments to satisfy CK pointing requests. */

/*     Suppose MOC.BC is a CK file consisting of several segments */
/*     containing Mars Observer Camera pointing data. Each segment */
/*     covers the same time period, but produces different pointing */
/*     values (one segment may contain predict values, another may */
/*     contain telemetry-based values, and others may contain different */
/*     corrected versions). */

/*     The following code fragment shows how different the results are */
/*     for each segment. The program steps through the file segment by */
/*     segment and requests pointing for the same time from each */
/*     segment. The results are printed to the screen. */

/*     GETIME is an imaginary routine used to get an encoded SCLK time */
/*     (SCLKDP) and time tolerance from the user. */

/*           SC     = -94 */
/*           INST   = -94001 */
/*           NEEDAV = .TRUE. */

/*           CALL CKLPF ( 'MOC.BC', HANDLE ) */

/*           CALL GETIME ( SCLKDP, TOL, QUIT ) */

/*     C */
/*     C     For each time, begin a forward search through the file, and */
/*     C     for each segment found, get its descriptor, identifier, and */
/*     C     evaluate the pointing. */
/*     C */
/*           DO WHILE ( .NOT. QUIT ) */

/*              CALL DAFBFS ( HANDLE ) */
/*              CALL DAFFNA ( FOUND  ) */

/*              DO WHILE ( FOUND ) */

/*                 CALL DAFGS ( DESCR ) */
/*                 CALL DAFGN ( IDENT ) */

/*                 CALL CKPFS ( HANDLE, DESCR, SCLKDP, TOL,   NEEDAV, */
/*          .                   CMAT,   AV,    CLKOUT, PFOUND         ) */

/*                 IF ( PFOUND ) THEN */
/*                    WRITE (*,*) 'Segment:          ', IDENT */
/*                    WRITE (*,*) 'C-Matrix:         ', CMAT */
/*                    WRITE (*,*) 'Angular velocity: ', AV */

/*                 ELSE */
/*                    CALL SCDECD ( SC, SCLKDP, SCLKCH ) */
/*                    WRITE (*,*) 'Data not found at time ', SCLKCH */

/*                 END IF */

/*                 CALL DAFFNA ( FOUND ) */

/*              END DO */

/*              CALL GETIME ( SCLKDP, TOL, QUIT ) */

/*           END DO */

/* $ Restrictions */

/*     1)  A C-kernel file should have been loaded by either CKLPF */
/*         or DAFOPR. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     J.M. Lynch         (JPL) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */
/*     R.E. Thurman       (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 6.1.0, 12-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 6.0.0, 24-MAR-2014 (NJB) */

/*        Bug fix: this routine now sets the output FOUND to */
/*        .FALSE. if a SPICE error is detected. */

/*        The routine was updated to handle data type 6 segments. */
/*        Several comment typos were corrected. */

/* -    SPICELIB Version 5.0.0, 19-AUG-2002 (NJB) */

/*        The routine was updated to handle data type 5 segments. */

/* -    SPICELIB Version 4.0.0, 02-MAY-1999 (BVS) */

/*        The routine was updated to handle data type 4 segments. */
/*        The RECSIZ size parameter was eliminated. The dimension */
/*        of the RECORD buffer is now defined by the CKMRSZ parameter */
/*        specified in the 'ckparam.inc' include file. */

/* -    SPICELIB Version 3.0.0, 11-SEP-1992 (JML) */

/*        The routine was updated to handle data type 3 segments. */

/* -    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 2.0.0, 30-AUG-1991 (JML) */

/*         The routine was updated to handle data type 2 segments. */

/*         FOUND is now initialized to false. */

/* -    SPICELIB Version 1.0.1, 02-NOV-1990 (JML) */

/*         The restriction that a C-kernel file must be loaded */
/*         was explicitly stated. */


/* -    SPICELIB Version 1.0.0, 07-SEP-1990 (RET) (IMU) */

/* -& */
/* $ Index_Entries */

/*     get pointing from CK segment */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 6.0.0, 24-MAR-2014 (NJB) */

/*        The routine was updated to handle data type 6 segments. */

/* -    SPICELIB Version 5.0.0, 19-AUG-2002 (NJB) */

/*        The routine was updated to handle data type 5 segments. */

/* -    SPICELIB Version 4.0.0, 02-MAY-1999 (BVS) */

/*        The routine was updated to handle data type 4 segments. */

/*           a) 'ckparam.inc' include file was included. */

/*           b) RECSIZ size parameter was eliminated. */

/*           c) Size of the RECORD was reset to CKMRSZ, parameter */
/*              defined in the 'ckparam.inc' include file. */

/*           d) Calls to CKR04 and CKE04 were added to the case */
/*              statement. */

/* -    SPICELIB Version 3.0.0, 11-SEP-1992 (JML) */

/*        The routine was updated to handle data type 3 segments. */

/*           a) RECSIZ was increased to 17. */

/*           b) Calls to CKR03 and CKE03 were added to the case */
/*              statement. */

/* -    SPICELIB Version 2.0.0, 30-AUG-1991 (JML) */

/*        1) The routine was updated to handle data type 2 segments. */

/*        2) FOUND is initialized to false to guard against it being */
/*           left unchanged from its previous value when an error is */
/*           detected. */

/* -    SPICELIB Version 1.0.1, 02-NOV-1990 (JML) */

/*        1) The restriction that a C-kernel file must be loaded */
/*           was explicitly stated. */

/* -    Beta Version 1.1.0, 30-AUG-1990 (MJS) */

/*        The following changes were made as a result of the */
/*        NAIF CK Code and Documentation Review: */

/*        1) The variable SCLK was changed to SCLKDP. */
/*        2) The declarations for the parameters RECSIZ, NDC, and NIC */
/*           were moved from the "Declarations" section of the header */
/*           to the "Local parameters" section of the code below the */
/*           header. These parameters are not meant to modified by */
/*           users. */
/*        3) The header was updated. */
/*        4) The comments in the code were improved. */

/* -    Beta Version 1.0.0, 07-MAY-1990 (RET) (IMU) */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */

/*        NDC        is the number of double precision components in an */
/*                   unpacked C-kernel segment descriptor. */

/*        NIC        is the number of integer components in an unpacked */
/*                   C-kernel segment descriptor. */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("CKPFS", (ftnlen)5);
    }

/*     Start off with FOUND set to false. */

    *found = FALSE_;

/*     Upgrading CKPFS to accommodate new data types involves following */
/*     these steps: */

/*     1)  Write the two new routines CKRnn and CKEnn. (You may need to */
/*         add or subtract from the arguments used in the existing CKRnn */
/*         and CKEnn calling sequences, but should not have to change */
/*         the inputs or outputs to CKPFS.) */

/*     2)  Insert a new case into the code of CKPFS. */

/*     3)  Depending on the size of RECORD returned from CKRnn, modify */
/*         the parameter RECSIZ.  (You will only need to change it if */
/*         RECSIZ is not large enough for the new CKRnn's RECORD.) */


/*     Unpack the descriptor to see what the data type of the segment is, */
/*     and call the appropriate read-and-evaluate routines. */

    dafus_(descr, &c__2, &c__6, dcd, icd);
    type__ = icd[2];
    if (type__ == 1) {
	ckr01_(handle, descr, sclkdp, tol, needav, record, found);
	if (*found) {
	    cke01_(needav, record, cmat, av, clkout);
	}
    } else if (type__ == 2) {
	ckr02_(handle, descr, sclkdp, tol, record, found);
	if (*found) {
	    cke02_(needav, record, cmat, av, clkout);
	}
    } else if (type__ == 3) {
	ckr03_(handle, descr, sclkdp, tol, needav, record, found);
	if (*found) {
	    cke03_(needav, record, cmat, av, clkout);
	}
    } else if (type__ == 4) {
	ckr04_(handle, descr, sclkdp, tol, needav, record, found);
	if (*found) {
	    cke04_(needav, record, cmat, av, clkout);
	}
    } else if (type__ == 5) {
	ckr05_(handle, descr, sclkdp, tol, needav, record, found);
	if (*found) {
	    cke05_(needav, record, cmat, av, clkout);
	}
    } else if (type__ == 6) {
	ckr06_(handle, descr, sclkdp, tol, needav, record, found);
	if (*found) {
	    cke06_(needav, record, cmat, av, clkout);
	}
    } else {
	setmsg_("The data type # is not currently supported.", (ftnlen)43);
	errint_("#", &type__, (ftnlen)1);
	sigerr_("SPICE(CKUNKNOWNDATATYPE)", (ftnlen)24);
    }

/*     In case an evaluator signaled an error, we check the SPICE */
/*     error status here. If a SPICE error occurred, indicate no */
/*     data were found. */

    if (failed_()) {
	*found = FALSE_;
    }
    chkout_("CKPFS", (ftnlen)5);
    return 0;
} /* ckpfs_ */

