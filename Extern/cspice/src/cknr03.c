/* cknr03.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__6 = 6;

/* $Procedure CKNR03 ( C-kernel, number of records, type 03 ) */
/* Subroutine */ int cknr03_(integer *handle, doublereal *descr, integer *
	nrec)
{
    /* Builtin functions */
    integer i_dnnt(doublereal *);

    /* Local variables */
    extern /* Subroutine */ int chkin_(char *, ftnlen), dafus_(doublereal *, 
	    integer *, integer *, doublereal *, integer *), dafgda_(integer *,
	     integer *, integer *, doublereal *), sigerr_(char *, ftnlen), 
	    chkout_(char *, ftnlen), setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen);
    doublereal npoint;
    extern logical return_(void);
    doublereal dcd[2];
    integer icd[6];

/* $ Abstract */

/*     Return the number of pointing instances in a CK type 03 segment. */
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
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   The handle of the CK file containing the segment. */
/*     DESCR      I   The descriptor of the type 3 segment. */
/*     NREC       O   The number of pointing instances in the segment. */

/* $ Detailed_Input */

/*     HANDLE   is the handle of the binary CK file containing the */
/*              segment. The file should have been opened for read */
/*              or write access, by CKLPF, DAFOPR or DAFOPW. */

/*     DESCR    is the packed descriptor of a data type 3 CK segment. */

/* $ Detailed_Output */

/*     NREC     is the number of pointing instances in the type 3 */
/*              segment. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the segment indicated by DESCR is not a type 3 segment, */
/*         the error SPICE(CKWRONGDATATYPE) is signaled. */

/*     2)  If the specified handle does not belong to any DAF file that */
/*         is currently known to be open, an error is signaled by a */
/*         routine in the call tree of this routine. */

/*     3)  If DESCR is not a valid descriptor of a segment in the CK */
/*         file specified by HANDLE, the results of this routine are */
/*         unpredictable. */

/* $ Files */

/*     The CK file specified by HANDLE should be open for read or */
/*     write access. */

/* $ Particulars */

/*     For a complete description of the internal structure of a type 3 */
/*     segment, see the CK required reading. */

/*     This routine returns the number of discrete pointing instances */
/*     contained in the specified segment. It is normally used in */
/*     conjunction with CKGR03 which returns the Ith pointing instance */
/*     in the segment. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) The following code example extracts the SCLK time, boresight */
/*        vector, and angular velocity vector for each pointing instance */
/*        in the first segment in a CK file that contains segments of */
/*        data type 3. */

/*        Use the CK kernel below, available in the Venus Express PDS */
/*        archives, as input for the code example. */

/*           VEX_BOOM_V01.BC */

/*        Example code begins here. */


/*              PROGRAM CKNR03_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local variables. */
/*        C */
/*              DOUBLE PRECISION      QUAT    ( 4 ) */
/*              DOUBLE PRECISION      AV      ( 3 ) */
/*              DOUBLE PRECISION      BORE    ( 3 ) */
/*              DOUBLE PRECISION      CMAT    ( 3, 3 ) */
/*              DOUBLE PRECISION      DCD     ( 2 ) */
/*              DOUBLE PRECISION      DESCR   ( 5 ) */
/*              DOUBLE PRECISION      RECORD  ( 8 ) */
/*              DOUBLE PRECISION      SCLKDP */

/*              INTEGER               I */
/*              INTEGER               ICD     ( 6 ) */
/*              INTEGER               HANDLE */
/*              INTEGER               NREC */

/*              LOGICAL               AVSEG */
/*              LOGICAL               FOUND */

/*        C */
/*        C     First load the file (it may also be opened by */
/*        C     using CKLPF). */
/*        C */
/*              CALL DAFOPR ( 'VEX_BOOM_V01.BC', HANDLE ) */

/*        C */
/*        C     Begin forward search.  Find the first array. */
/*        C */
/*              CALL DAFBFS ( HANDLE ) */
/*              CALL DAFFNA ( FOUND  ) */

/*        C */
/*        C     Get segment descriptor. */
/*        C */
/*              CALL DAFGS ( DESCR ) */

/*        C */
/*        C     Unpack the segment descriptor into its double precision */
/*        C     and integer components. */
/*        C */
/*              CALL DAFUS ( DESCR, 2, 6, DCD, ICD ) */

/*        C */
/*        C     The data type for a segment is located in the third */
/*        C     integer component of the descriptor. */
/*        C */
/*              IF ( ICD( 3 ) .EQ. 3 ) THEN */

/*        C */
/*        C        Does the segment contain AV data? */
/*        C */
/*                 AVSEG =  ( ICD(4) .EQ. 1 ) */

/*        C */
/*        C        How many records does this segment contain? */
/*        C */
/*                 CALL CKNR03 ( HANDLE, DESCR, NREC ) */

/*                 DO I = 1, NREC */

/*        C */
/*        C           Get the Ith pointing instance in the segment. */
/*        C */
/*                    CALL CKGR03 ( HANDLE, DESCR, I, RECORD ) */

/*        C */
/*        C           Unpack RECORD into the time, quaternion, and av. */
/*        C */
/*                    SCLKDP = RECORD ( 1 ) */

/*                    CALL MOVED ( RECORD(2), 4, QUAT ) */

/*                    IF  ( AVSEG )  THEN */
/*                       CALL MOVED ( RECORD(6), 3, AV   ) */
/*                    END IF */

/*        C */
/*        C           The boresight vector is the third row of the */
/*        C           C-matrix. */
/*        C */
/*                    CALL Q2M ( QUAT, CMAT ) */

/*                    BORE(1) = CMAT(3,1) */
/*                    BORE(2) = CMAT(3,2) */
/*                    BORE(3) = CMAT(3,3) */

/*        C */
/*        C           Write out the results. */
/*        C */
/*                    WRITE (*,'(A,I2)') 'Record: ', I */
/*                    WRITE (*,'(A,F25.6)')  '   SCLK time       :', */
/*             .                               SCLKDP */
/*                    WRITE (*,'(A,3F14.9)') '   Boresight       :', */
/*             .                               BORE */

/*                    IF ( AVSEG ) THEN */
/*                       WRITE (*,'(A,3F14.9)') '   Angular velocity:', */
/*             .                                  AV */
/*                    END IF */
/*                    WRITE (*,*) */

/*                 END DO */

/*              END IF */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        Record:  1 */
/*           SCLK time       :           2162686.710986 */
/*           Boresight       :  -0.999122830   0.000000000   0.041875654 */
/*           Angular velocity:   0.000000000   0.000000000   0.000000000 */

/*        Record:  2 */
/*           SCLK time       :       54160369751.715164 */
/*           Boresight       :  -0.999122830   0.000000000   0.041875654 */
/*           Angular velocity:   0.000000000   1.176083393   0.000000000 */

/*        Record:  3 */
/*           SCLK time       :       54160454948.487686 */
/*           Boresight       :   0.000000000   0.000000000   1.000000000 */
/*           Angular velocity:   0.000000000   0.000000000   0.000000000 */

/*        Record:  4 */
/*           SCLK time       :      299264885854.937805 */
/*           Boresight       :   0.000000000   0.000000000   1.000000000 */
/*           Angular velocity:   0.000000000   0.000000000   0.000000000 */

/*        Record:  5 */
/*           SCLK time       :     2366007685832.532227 */
/*           Boresight       :   0.000000000   0.000000000   1.000000000 */
/*           Angular velocity:   0.000000000   0.000000000   0.000000000 */

/*        Record:  6 */
/*           SCLK time       :     4432750485810.126953 */
/*           Boresight       :   0.000000000   0.000000000   1.000000000 */
/*           Angular velocity:   0.000000000   0.000000000   0.000000000 */

/*        Record:  7 */
/*           SCLK time       :     6505155594828.757812 */
/*           Boresight       :   0.000000000   0.000000000   1.000000000 */
/*           Angular velocity:   0.000000000   0.000000000   0.000000000 */

/*        Record:  8 */
/*           SCLK time       :     8571898394806.352539 */
/*           Boresight       :   0.000000000   0.000000000   1.000000000 */
/*           Angular velocity:   0.000000000   0.000000000   0.000000000 */

/*        Record:  9 */
/*           SCLK time       :    10638641194783.947266 */
/*           Boresight       :   0.000000000   0.000000000   1.000000000 */
/*           Angular velocity:   0.000000000   0.000000000   0.000000000 */

/*        Record: 10 */
/*           SCLK time       :    12705383994761.541016 */
/*           Boresight       :   0.000000000   0.000000000   1.000000000 */
/*           Angular velocity:   0.000000000   0.000000000   0.000000000 */

/*        Record: 11 */
/*           SCLK time       :    14777789103780.169922 */
/*           Boresight       :   0.000000000   0.000000000   1.000000000 */
/*           Angular velocity:   0.000000000   0.000000000   0.000000000 */

/*        Record: 12 */
/*           SCLK time       :    16844531903757.763672 */
/*           Boresight       :   0.000000000   0.000000000   1.000000000 */
/*           Angular velocity:   0.000000000   0.000000000   0.000000000 */

/*        Record: 13 */
/*           SCLK time       :    18911274703735.359375 */
/*           Boresight       :   0.000000000   0.000000000   1.000000000 */
/*           Angular velocity:   0.000000000   0.000000000   0.000000000 */


/* $ Restrictions */

/*     1)  The binary CK file containing the segment whose descriptor was */
/*         passed to this routine must be opened for read or write access */
/*         by CKLPF, DAFOPR or DAFOPW. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     J.M. Lynch         (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.1, 26-OCT-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. Added */
/*        reference to required CK in example's problem statement. */

/*        Fixed minor language issues in $Abstract, $Brief_I/O, */
/*        $Detailed_Input, $Files and $Restrictions sections. */

/* -    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW) */

/*        Replaced DAFRDA call with DAFGDA. */
/*        Added IMPLICIT NONE. */

/* -    SPICELIB Version 1.0.0, 25-NOV-1992 (JML) */

/* -& */
/* $ Index_Entries */

/*     number of CK type_3 records */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */

/*        NDC        is the number of double precision components in an */
/*                   unpacked C-kernel descriptor. */

/*        NIC        is the number of integer components in an unpacked */
/*                   C-kernel descriptor. */

/*        DTYPE      is the data type of the segment that this routine */
/*                   operates on. */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("CKNR03", (ftnlen)6);
    }

/*     The number of discrete pointing instances contained in a data */
/*     type 3 segment is stored in the last double precision word of */
/*     the segment.  Since the address of the last word is stored in */
/*     the sixth integer component of the segment descriptor, it is */
/*     a trivial matter to extract the count. */

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


    dafus_(descr, &c__2, &c__6, dcd, icd);

/*     If this segment is not of data type 3, then signal an error. */

    if (icd[2] != 3) {
	setmsg_("Data type of the segment should be 3: Passed descriptor sho"
		"ws type = #.", (ftnlen)71);
	errint_("#", &icd[2], (ftnlen)1);
	sigerr_("SPICE(CKWRONGDATATYPE)", (ftnlen)22);
	chkout_("CKNR03", (ftnlen)6);
	return 0;
    }

/*     The number of records is the final word in the segment. */

    dafgda_(handle, &icd[5], &icd[5], &npoint);
    *nrec = i_dnnt(&npoint);
    chkout_("CKNR03", (ftnlen)6);
    return 0;
} /* cknr03_ */

