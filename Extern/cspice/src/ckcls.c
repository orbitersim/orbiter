/* ckcls.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure CKCLS ( CK, Close file ) */
/* Subroutine */ int ckcls_(integer *handle)
{
    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    logical found;
    extern /* Subroutine */ int daffna_(logical *);
    extern logical failed_(void);
    extern /* Subroutine */ int dafbfs_(integer *), dafcls_(integer *);
    char access[5];
    extern /* Subroutine */ int errhan_(char *, integer *, ftnlen), sigerr_(
	    char *, ftnlen), chkout_(char *, ftnlen), setmsg_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Close an open CK file. */

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

/*     None. */

/* $ Keywords */

/*     CK */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Handle of the CK file to be closed. */

/* $ Detailed_Input */

/*     HANDLE   is the handle of the CK file that is to be closed. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If there are no segments in the file, the error */
/*         SPICE(NOSEGMENTSFOUND) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Close the CK file attached to HANDLE. */

/*     The close operation tests the file to ensure the presence of data */
/*     segments. */

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


/*              PROGRAM CKCLS_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local parameters. */
/*        C */
/*              CHARACTER*(*)         CK2 */
/*              PARAMETER           ( CK2 = 'ckcls_ex1.bc' ) */

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
/*              CALL CKOPN ( CK2, IFNAME, NCOMCH, HANDLE ) */

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

/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     F.S. Turner        (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.1, 26-MAY-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. Added */
/*        complete code example based on existing fragment. */

/*        Re-ordered header sections and extended the $Particulars */
/*        section. */

/* -    SPICELIB Version 1.2.0, 07-SEP-2001 (EDW) */

/*        Removed DAFHLU call; replaced ERRFNM call with ERRHAN. */

/* -    SPICELIB Version 1.1.0, 17-FEB-2000 (FST) */

/*        Removed the call to ZZFIXID. This will make all C-kernels */
/*        created with future versions of the toolkit possess the */
/*        unambiguous ID word 'DAF/CK  '. */

/* -    SPICELIB Version 1.0.0, 27-JAN-1995 (KRG) */

/* -& */
/* $ Index_Entries */

/*     close a CK file */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local Variables */


/*     Standard SPICELIB error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("CKCLS", (ftnlen)5);

/*     Get the access method for the file. Currently, if HANDLE < 0, the */
/*     access method is 'WRITE'. If HANDLE > 0, the access method is */
/*     'READ'.  In the future this should make use of the private entry */
/*     in the handle manager umbrella, ZZDDHNFO. */

    if (*handle < 0) {
	s_copy(access, "WRITE", (ftnlen)5, (ftnlen)5);
    } else if (*handle > 0) {
	s_copy(access, "READ", (ftnlen)5, (ftnlen)4);
    }

/*     Fix the ID word if the file is open for writing and close the */
/*     file, or just close the file. */

    if (s_cmp(access, "WRITE", (ftnlen)5, (ftnlen)5) == 0) {

/*        Check to see if there are any segments in the file. If there */
/*        are no segments, we signal an error. This probably indicates a */
/*        programming error of some sort anyway. Why would you create a */
/*        file and put nothing in it? */

	dafbfs_(handle);
	daffna_(&found);
	if (failed_()) {
	    chkout_("CKCLS", (ftnlen)5);
	    return 0;
	}
	if (! found) {
	    setmsg_("No segments were found in the CK file '#'. There must b"
		    "e at least one segment in the file when this subroutine "
		    "is called.", (ftnlen)121);
	    errhan_("#", handle, (ftnlen)1);
	    sigerr_("SPICE(NOSEGMENTSFOUND)", (ftnlen)22);
	    chkout_("CKCLS", (ftnlen)5);
	    return 0;
	}
    }

/*     Close the file. */

    dafcls_(handle);

/*     No need to check FAILED() here, since we just return. The caller */
/*     should check it though. */

    chkout_("CKCLS", (ftnlen)5);
    return 0;
} /* ckcls_ */

