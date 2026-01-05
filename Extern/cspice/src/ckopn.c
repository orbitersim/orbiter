/* ckopn.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__6 = 6;

/* $Procedure CKOPN ( CK, open new file. ) */
/* Subroutine */ int ckopn_(char *fname, char *ifname, integer *ncomch, 
	integer *handle, ftnlen fname_len, ftnlen ifname_len)
{
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer ncomr;
    extern logical failed_(void);
    extern /* Subroutine */ int dafonw_(char *, char *, integer *, integer *, 
	    char *, integer *, integer *, ftnlen, ftnlen, ftnlen), chkout_(
	    char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Open a new CK file, returning the handle of the opened file. */

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

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     FNAME      I   The name of the CK file to be opened. */
/*     IFNAME     I   The internal filename for the CK. */
/*     NCOMCH     I   The number of characters to reserve for comments. */
/*     HANDLE     O   The handle of the opened CK file. */

/* $ Detailed_Input */

/*     FNAME    is the name of the CK file to be opened. */

/*     IFNAME   is the internal filename for the CK file that is being */
/*              created. The internal filename may be up to 60 */
/*              characters long. If you do not have any conventions */
/*              for tagging your files, an internal filename of */
/*              'CK_file' is perfectly acceptable. You may also leave */
/*              it blank if you like. */

/*     NCOMCH   is the space, measured in characters, to be initially */
/*              set aside for the comment area when a new CK file */
/*              is opened. The amount of space actually set aside may */
/*              be greater than the amount requested, due to the */
/*              manner in which comment records are allocated in an CK */
/*              file. However, the amount of space set aside for */
/*              comments will always be at least the amount that was */
/*              requested. */

/*              The value of NCOMCH should be greater than or equal to */
/*              zero, i.e., 0 <= NCOMCH. A negative value, should one */
/*              occur, will be assumed to be zero. */

/* $ Detailed_Output */

/*     HANDLE   is the handle of the opened CK file. If an error */
/*              occurs the value of this variable will not represent a */
/*              valid handle. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the value of NCOMCH is negative, a value of zero (0) will */
/*         be used for the number of comment characters to be set aside */
/*         for comments. */

/*     2)  If an error occurs while attempting to open a CK file the */
/*         value of HANDLE will not represent a valid file handle. */

/* $ Files */

/*     See FNAME and HANDLE. */

/* $ Particulars */

/*     Open a new CK file, reserving room for comments if requested. */

/*     A CKCLS call should balance every CKOPN call. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Create a CK type 3 segment; fill with data for a simple time */
/*        dependent rotation and angular velocity, and reserve room in */
/*        the CK comments area for 5000 characters. */

/*        Example code begins here. */


/*              PROGRAM CKOPN_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local parameters. */
/*        C */
/*              CHARACTER*(*)         CK3 */
/*              PARAMETER           ( CK3 = 'ckopn_ex1.bc' ) */

/*              DOUBLE PRECISION      SPTICK */
/*              PARAMETER           ( SPTICK = 0.001D0 ) */

/*              INTEGER               INST */
/*              PARAMETER           ( INST = -77703 ) */

/*              INTEGER               MAXREC */
/*              PARAMETER           ( MAXREC = 201 ) */

/*              INTEGER               NAMLEN */
/*              PARAMETER           ( NAMLEN = 42 ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              CHARACTER*(NAMLEN)    REF */
/*              CHARACTER*(NAMLEN)    IFNAME */
/*              CHARACTER*(NAMLEN)    SEGID */

/*              DOUBLE PRECISION      AVVS   (   3,MAXREC ) */
/*              DOUBLE PRECISION      BEGTIM */
/*              DOUBLE PRECISION      ENDTIM */
/*              DOUBLE PRECISION      QUATS  ( 0:3,MAXREC ) */
/*              DOUBLE PRECISION      RATE */
/*              DOUBLE PRECISION      RWMAT  ( 3, 3 ) */
/*              DOUBLE PRECISION      SPACES */
/*              DOUBLE PRECISION      SCLKDP (     MAXREC ) */
/*              DOUBLE PRECISION      STARTS (    MAXREC/2) */
/*              DOUBLE PRECISION      STICKS */
/*              DOUBLE PRECISION      THETA */
/*              DOUBLE PRECISION      WMAT   ( 3, 3 ) */
/*              DOUBLE PRECISION      WQUAT  ( 0:3 ) */

/*              INTEGER               HANDLE */
/*              INTEGER               I */
/*              INTEGER               NCOMCH */
/*              INTEGER               NINTS */

/*              LOGICAL               AVFLAG */

/*        C */
/*        C     NCOMCH is the number of characters to reserve for the */
/*        C     kernel's comment area. This example doesn't write */
/*        C     comments, but it reserves room for 5000 characters. */
/*        C */
/*              NCOMCH = 5000 */

/*        C */
/*        C     The base reference from for the rotation data. */
/*        C */
/*              REF = 'J2000' */

/*        C */
/*        C     Time spacing in encoded ticks and in seconds */
/*        C */
/*              STICKS = 10.D0 */
/*              SPACES = STICKS * SPTICK */

/*        C */
/*        C     Declare an angular rate in radians per sec. */
/*        C */
/*              RATE = 1.D-2 */

/*        C */
/*        C     Internal file name and segment ID. */
/*        C */
/*              SEGID  = 'Test type 3 CK segment' */
/*              IFNAME = 'Test CK type 3 segment created by CKW03' */


/*        C */
/*        C     Open a new kernel. */
/*        C */
/*              CALL CKOPN ( CK3, IFNAME, NCOMCH, HANDLE ) */

/*        C */
/*        C     Create a 3x3 double precision identity matrix. */
/*        C */
/*              CALL IDENT ( WMAT ) */

/*        C */
/*        C     Convert the matrix to quaternion. */
/*        C */
/*              CALL M2Q ( WMAT, WQUAT ) */

/*        C */
/*        C     Copy the work quaternion to the first row of */
/*        C     QUATS. */
/*        C */
/*              CALL MOVED ( WQUAT, 4, QUATS(0,1) ) */

/*        C */
/*        C     Create an angular velocity vector. This vector is in the */
/*        C     REF reference frame and indicates a constant rotation */
/*        C     about the Z axis. */
/*        C */
/*              CALL VPACK ( 0.D0, 0.D0, RATE, AVVS(1,1) ) */

/*        C */
/*        C     Set the initial value of the encoded ticks. */
/*        C */
/*              SCLKDP(1) = 1000.D0 */

/*        C */
/*        C     Fill the rest of the AVVS and QUATS matrices */
/*        C     with simple data. */
/*        C */
/*              DO I = 2, MAXREC */

/*        C */
/*        C        Create the corresponding encoded tick value in */
/*        C        increments of STICKS with an initial value of */
/*        C        1000.0 ticks. */
/*        C */
/*                 SCLKDP(I) = 1000.D0 + (I-1) * STICKS */

/*        C */
/*        C        Create the transformation matrix for a rotation of */
/*        C        THETA about the Z axis. Calculate THETA from the */
/*        C        constant angular rate RATE at increments of SPACES. */
/*        C */
/*                 THETA = (I-1) * RATE * SPACES */
/*                 CALL ROTMAT ( WMAT, THETA, 3, RWMAT ) */

/*        C */
/*        C        Convert the RWMAT matrix to SPICE type quaternion. */
/*        C */
/*                 CALL M2Q ( RWMAT, WQUAT ) */

/*        C */
/*        C        Store the quaternion in the QUATS matrix. */
/*        C        Store angular velocity in AVVS. */
/*        C */
/*                 CALL MOVED ( WQUAT, 4, QUATS(0,I) ) */
/*                 CALL VPACK ( 0.D0, 0.D0, RATE, AVVS(1,I) ) */

/*              END DO */

/*        C */
/*        C     Create an array start times for the interpolation */
/*        C     intervals. The end time for a particular interval is */
/*        C     determined as the time of the final data value prior in */
/*        C      time to the next start time. */
/*        C */
/*              NINTS = MAXREC/2 */
/*              DO I = 1, NINTS */

/*                 STARTS(I) = SCLKDP(I*2 - 1) */

/*              END DO */

/*        C */
/*        C     Set the segment boundaries equal to the first and last */
/*        C     time for the data arrays. */
/*        C */
/*              BEGTIM = SCLKDP(1) */
/*              ENDTIM = SCLKDP(MAXREC) */

/*        C */
/*        C     This segment contains angular velocity. */
/*        C */
/*              AVFLAG = .TRUE. */

/*        C */
/*        C     All information ready to write. Write to a CK type 3 */
/*        C     segment to the file indicated by HANDLE. */
/*        C */
/*              CALL CKW03 ( HANDLE, BEGTIM, ENDTIM, INST,   REF, */
/*             .             AVFLAG, SEGID,  MAXREC, SCLKDP, QUATS, */
/*             .             AVVS,   NINTS,  STARTS                ) */

/*        C */
/*        C     SAFELY close the file. */
/*        C */
/*              CALL CKCLS ( HANDLE ) */

/*              END */


/*        When this program is executed, no output is presented on */
/*        screen. After run completion, a new CK file exists in the */
/*        output directory. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.1.0, 02-JUL-2021 (JDR) */

/*        Changed the output argument name NAME to FNAME for consistency */
/*        with other routines. */

/*        Edited the header to comply with NAIF standard. Added */
/*        complete code example based on existing fragment. */

/*        Extended $Parameters section. */

/* -    SPICELIB Version 2.0.0, 09-NOV-2006 (NJB) */

/*        Routine has been upgraded to support comment */
/*        area allocation using NCOMCH. */

/* -    SPICELIB Version 1.0.0, 26-JAN-1995 (KRG) */

/* -& */
/* $ Index_Entries */

/*     open a new CK file */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     DAF ND and NI values for CK files. */


/*     Length of a DAF comment record, in characters. */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("CKOPN", (ftnlen)5);

/*     Compute the number of comment records that we want to allocate, if */
/*     the number of comment characters requested is greater than zero, */
/*     we always allocate an extra record to account for the end of line */
/*     marks in the comment area. */

    if (*ncomch > 0) {
	ncomr = (*ncomch - 1) / 1000 + 1;
    } else {
	ncomr = 0;
    }

/*     Just do it. All of the error handling is taken care of for us. */

    dafonw_(fname, "CK", &c__2, &c__6, ifname, &ncomr, handle, fname_len, (
	    ftnlen)2, ifname_len);
    if (failed_()) {

/*        If we failed, make sure that HANDLE does not contain a value */
/*        that represents a valid DAF file handle. */

	*handle = 0;
    }
    chkout_("CKOPN", (ftnlen)5);
    return 0;
} /* ckopn_ */

