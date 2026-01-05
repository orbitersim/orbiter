/* ckgr05.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__6 = 6;

/* $Procedure CKGR05 ( C-kernel, get record, type 05 ) */
/* Subroutine */ int ckgr05_(integer *handle, doublereal *descr, integer *
	recno, doublereal *record)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer i_dnnt(doublereal *);

    /* Local variables */
    integer addr__, nrec;
    extern /* Subroutine */ int chkin_(char *, ftnlen), dafus_(doublereal *, 
	    integer *, integer *, doublereal *, integer *), dafgda_(integer *,
	     integer *, integer *, doublereal *);
    extern logical failed_(void);
    integer packsz;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen);
    doublereal npoint;
    extern logical return_(void);
    integer subtyp;
    doublereal dcd[2];
    integer beg, icd[6], end;

/* $ Abstract */

/*     Return a specified pointing instance from a CK type 05 segment. */
/*     The segment is identified by a CK file handle and segment */
/*     descriptor. */

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

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   The handle of the file containing the segment. */
/*     DESCR      I   The segment descriptor. */
/*     RECNO      I   The number of the pointing instance to be returned. */
/*     RECORD     O   The pointing record. */

/* $ Detailed_Input */

/*     HANDLE   is the handle of the binary CK file containing the */
/*              desired segment. */

/*     DESCR    is the packed descriptor of the data type 5 segment. */

/*     RECNO    is the number of the discrete pointing instance to be */
/*              returned from the data type 5 segment. */

/* $ Detailed_Output */

/*     RECORD   is the pointing instance indexed by RECNO in the */
/*              segment. The contents are as follows: */

/*                 RECORD( 1 ) = CLKOUT */

/*              CLKOUT is the encoded spacecraft clock time associated */
/*              with the returned pointing values. */

/*                 RECORD( 2 ) = SUBTYP */

/*              SUBTYP is the CK type 5 subtype code. This code */
/*              identifies the structure and meaning of the rest */
/*              of the record. However, all subtypes have a */
/*              quaternion stored in elements 3-6. */

/*                 RECORD( 3 ) = q0 */
/*                 RECORD( 4 ) = q1 */
/*                 RECORD( 5 ) = q2 */
/*                 RECORD( 6 ) = q3 */

/*              Subtype 1 ends here; there are no angular velocity */
/*              data. Angular velocity is derived by differentiating */
/*              Lagrange interpolating polynomials. */

/*                 RECORD(  7 ) =  ] */
/*                 RECORD(  8 ) =  ] --- For subtypes 0 and 2, these */
/*                 RECORD(  9 ) =  ]     elements contain a quaternion */
/*                 RECORD( 10 ) =  ]     derivative. For subtype 3, */
/*                                       elements 7-9 contain an */
/*                                       angular velocity vector; */
/*                                       element 10 is unassigned. */

/*                                       All subtypes except subtype */
/*                                       2 stop here. */

/*                 RECORD( 11 ) =  ] */
/*                 RECORD( 12 ) =  ] --- For subtype 2, these */
/*                 RECORD( 13 ) =  ]     elements contain an angular */
/*                                       velocity vector. */


/*                 RECORD( 14 ) =  ] */
/*                 RECORD( 15 ) =  ] --- For subtype 2, these */
/*                 RECORD( 16 ) =  ]     elements contain the */
/*                                       derivative of an angular */
/*                                       velocity vector. */

/*              The quantities q0 - q3 are the components of the */
/*              quaternion that represents the C-matrix that transforms */
/*              vectors from the inertial reference frame of the */
/*              segment to the instrument frame at time CLKOUT. */

/*              Quaternion derivatives, angular velocity, or the */
/*              derivative of angular velocity are returned only */
/*              these are supported by the segment subtype and */
/*              if the segment descriptor indicates that angular */
/*              velocity is present. */

/*              The components of the angular velocity vector are */
/*              specified relative to the inertial reference frame of */
/*              the segment. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the segment is not of data type 5, the error */
/*         SPICE(CKWRONGDATATYPE) is signaled. */

/*     2)  If RECNO is less than one or greater than the number of */
/*         records in the specified segment, the error */
/*         SPICE(CKNONEXISTREC) is signaled. */

/*     3)  If the specified handle does not belong to any DAF file that */
/*         is currently known to be open, an error is signaled by a */
/*         routine in the call tree of this routine. */

/*     4)  If DESCR is not a valid descriptor of a segment in the CK */
/*         file specified by HANDLE, the results of this routine are */
/*         unpredictable. */

/*     5)  If the segment subtype is not recognized, the error */
/*         SPICE(NOTSUPPORTED) is signaled. */

/* $ Files */

/*     The file specified by HANDLE should be open for read or */
/*     write access. */

/* $ Particulars */

/*     For a detailed description of the structure of a type 5 segment, */
/*     see the CK required reading. */

/*     This is a utility routine that may be used to read the individual */
/*     pointing instances that make up a type 5 segment. It is normally */
/*     used in conjunction with CKNR05, which gives the number of */
/*     pointing instances stored in a segment. */

/* $ Examples */

/*     Suppose that MOC.BC is a CK file that contains segments of */
/*     data type 5. Then the following code fragment extracts the */
/*     SCLK time and boresight vector for each pointing instance */
/*     in the first segment in the file. */


/*           INTEGER               ICD     ( 6 ) */
/*           INTEGER               HANDLE */
/*           INTEGER               NREC */
/*           INTEGER               I */

/*           DOUBLE PRECISION      DCD     ( 2 ) */
/*           DOUBLE PRECISION      DESCR   ( 5 ) */
/*           DOUBLE PRECISION      RECORD  ( 16 ) */
/*           DOUBLE PRECISION      QUAT    ( 4 ) */
/*           DOUBLE PRECISION      BORE    ( 3 ) */
/*           DOUBLE PRECISION      CMAT    ( 3, 3 ) */
/*           DOUBLE PRECISION      SCLKDP */

/*           LOGICAL               FOUND */

/*     C */
/*     C     First load the file. (The file may also be opened by using */
/*     C     CKLPF.) */
/*     C */
/*           CALL DAFOPR ( 'MOC.BC', HANDLE ) */

/*     C */
/*     C     Begin forward search. Find the first array. */
/*     C */
/*           CALL DAFBFS ( HANDLE ) */
/*           CALL DAFFNA ( FOUND  ) */

/*     C */
/*     C     Get segment descriptor. */
/*     C */
/*           CALL DAFGS ( DESCR ) */

/*     C */
/*     C     Unpack the segment descriptor into its double precision */
/*     C     and integer components. */
/*     C */
/*           CALL DAFUS ( DESCR, 2, 6, DCD, ICD ) */

/*     C */
/*     C     The data type for a segment is located in the third integer */
/*     C     component of the descriptor. */
/*     C */
/*           IF ( ICD( 3 ) .EQ. 5 ) THEN */
/*     C */
/*     C        How many records does this segment contain? */
/*     C */
/*              CALL CKNR05 ( HANDLE, DESCR, NREC ) */

/*              DO I = 1, NREC */
/*     C */
/*     C           Get the Ith pointing instance in the segment. */
/*     C */
/*                 CALL CKGR05 ( HANDLE, DESCR, I, RECORD ) */

/*     C */
/*     C           Unpack from RECORD the time tag and quaternion. */
/*     C           The locations of these items in the record are */
/*     C           independent of the subtype. */
/*     C */
/*                 SCLKDP = RECORD ( 1 ) */

/*                 CALL MOVED ( RECORD(3), 4, QUAT ) */

/*     C */
/*     C           The boresight vector is the third row of the C-matrix. */
/*     C */
/*                 CALL Q2M ( QUAT, CMAT ) */

/*                 BORE(1) = CMAT(3,1) */
/*                 BORE(2) = CMAT(3,2) */
/*                 BORE(3) = CMAT(3,3) */
/*     C */
/*     C           Write out the results. */
/*     C */
/*                 WRITE (*,*) 'Record: ', I */
/*                 WRITE (*,*) */
/*                 WRITE (*,*) 'SCLK time = ', SCLKDP */
/*                 WRITE (*,*) */
/*                 WRITE (*,*) 'boresight: ', BORE */

/*              END DO */

/*           END IF */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     J.M. Lynch         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR) (NJB) */

/*        Edited the header to comply with NAIF standard. */

/*        Bug fix: added FAILED tests after all DAFGDA calls but the */
/*        last. */

/*        Added explicit conversion to INTEGER for SUBTYP. */

/* -    SPICELIB Version 1.0.0, 27-AUG-2002 (NJB) (JML) */

/* -& */
/* $ Index_Entries */

/*     get CK type_5 record */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */

/*        NDC        is the number of double precision components in an */
/*                   unpacked C-kernel segment descriptor. */

/*        NIC        is the number of integer components in an unpacked */
/*                   C-kernel segment descriptor. */

/*        DTYPE      is the data type of the segment that this routine */
/*                   operates on. */



/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("CKGR05", (ftnlen)6);

/*     The unpacked descriptor contains the following information */
/*     about the segment: */

/*        DCD(1)  Initial encoded SCLK */
/*        DCD(2)  Final encoded SCLK */
/*        ICD(1)  Instrument */
/*        ICD(2)  Inertial reference frame */
/*        ICD(3)  Data type */
/*        ICD(4)  Angular velocity flag */
/*        ICD(5)  Initial address of segment data */
/*        ICD(6)  Final address of segment data */

/*     From the descriptor, determine */

/*       1 - Is this really a type 5 segment? */
/*       2 - The beginning address of the segment. */
/*       3 - The number of pointing instances in the segment (it's the */
/*           last word in the segment). */
/*       4 - The existence of angular velocity data, which determines how */
/*           big the pointing portion of the returned record will be. */

    dafus_(descr, &c__2, &c__6, dcd, icd);
    if (icd[2] != 5) {
	setmsg_("Data type of the segment should be 5: Passed descriptor sho"
		"ws type = #.", (ftnlen)71);
	errint_("#", &icd[2], (ftnlen)1);
	sigerr_("SPICE(CKWRONGDATATYPE)", (ftnlen)22);
	chkout_("CKGR05", (ftnlen)6);
	return 0;
    }

/*     Capture the segment's address range. */

    beg = icd[4];
    end = icd[5];

/*     Read the subtype from the segment. */

    i__1 = end - 3;
    i__2 = end - 3;
    dafgda_(handle, &i__1, &i__2, &record[1]);
    if (failed_()) {
	chkout_("CKGR05", (ftnlen)6);
	return 0;
    }
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
	chkout_("CKGR05", (ftnlen)6);
	return 0;
    }
    dafgda_(handle, &end, &end, &npoint);
    if (failed_()) {
	chkout_("CKGR05", (ftnlen)6);
	return 0;
    }
    nrec = i_dnnt(&npoint);

/*     If a request was made for a record which doesn't exist, then */
/*     signal an error and leave. */

    if (*recno < 1 || *recno > nrec) {
	setmsg_("Requested record number (#) does not exist. There are # rec"
		"ords in the segment.", (ftnlen)79);
	errint_("#", recno, (ftnlen)1);
	errint_("#", &nrec, (ftnlen)1);
	sigerr_("SPICE(CKNONEXISTREC)", (ftnlen)20);
	chkout_("CKGR05", (ftnlen)6);
	return 0;
    }

/*     Get the pointing record indexed by RECNO. */

    addr__ = beg + packsz * (*recno - 1);
    i__1 = addr__ + packsz - 1;
    dafgda_(handle, &addr__, &i__1, &record[2]);
    if (failed_()) {
	chkout_("CKGR05", (ftnlen)6);
	return 0;
    }

/*     Next get the SCLK time.  Need to go past all of the NREC pointing */
/*     records (PACKSZ * NREC numbers), and then to the RECNOth SCLK */
/*     time. */

    addr__ = beg + packsz * nrec + *recno - 1;
    dafgda_(handle, &addr__, &addr__, record);
    chkout_("CKGR05", (ftnlen)6);
    return 0;
} /* ckgr05_ */

