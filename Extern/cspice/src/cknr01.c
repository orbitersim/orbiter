/* cknr01.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__6 = 6;

/* $Procedure CKNR01 ( C-kernel, number of records, type 01 ) */
/* Subroutine */ int cknr01_(integer *handle, doublereal *descr, integer *
	nrec)
{
    doublereal n;
    extern /* Subroutine */ int chkin_(char *, ftnlen), dafus_(doublereal *, 
	    integer *, integer *, doublereal *, integer *), dafgda_(integer *,
	     integer *, integer *, doublereal *), sigerr_(char *, ftnlen), 
	    chkout_(char *, ftnlen), setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen);
    extern logical return_(void);
    doublereal dcd[2];
    integer icd[6];

/* $ Abstract */

/*     Return the number of pointing instances in a CK type 01 segment. */
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
/*     HANDLE     I   The handle of the file containing the segment. */
/*     DESCR      I   The descriptor of the type 1 segment. */
/*     NREC       O   The number of records in the segment. */

/* $ Detailed_Input */

/*     HANDLE   is the handle of the binary CK file containing the */
/*              segment whose descriptor was also passed. The file */
/*              should have been opened for read access, either by */
/*              CKLPF or DAFOPR. */

/*     DESCR    is the packed descriptor of a data type 1 segment. */

/* $ Detailed_Output */

/*     NREC     is the number of pointing records in the type 1 segment. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the segment indicated by DESCR is not a type 1 segment, */
/*         the error SPICE(CKWRONGDATATYPE) is signaled. */

/*     2)  If the specified handle does not belong to any file that is */
/*         currently known to be open, an error is signaled by a routine */
/*         in the call tree of this routine. */

/*     3)  If DESCR is not a valid, packed descriptor of a segment in */
/*         the CK file specified by HANDLE, the results of this routine */
/*         are unpredictable. */

/* $ Files */

/*     The file specified by HANDLE should be open for read access. */

/* $ Particulars */

/*     For a complete description of the internal structure of a type 1 */
/*     segment, see the CK required reading. */

/* $ Examples */

/*     The following code fragment prints the records of the first */
/*     segment in a CK file. Suppose MOC.CK is binary CK file that */
/*     contains segments of data type 1. */

/*           INTEGER               ICD     ( 6 ) */
/*           INTEGER               HANDLE */
/*           INTEGER               NREC */
/*           INTEGER               I */
/*           DOUBLE PRECISION      DCD     ( 2 ) */
/*           DOUBLE PRECISION      DESCR   ( 5 ) */
/*           DOUBLE PRECISION      RECORD  ( 8 ) */
/*           LOGICAL               FOUND */

/*     C */
/*     C     First load the file. (The file may also be opened by using */
/*     C     CKLPF.) */
/*     C */
/*           CALL DAFOPR ( 'MOC.CK', HANDLE ) */

/*     C */
/*     C     Begin forward search.  Find first array. */
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
/*           IF ( ICD( 3 ) .EQ. 1 ) THEN */

/*     C */
/*     C        How many records does this segment contain? */
/*     C */
/*              CALL CKNR01 ( HANDLE, DESCR, NREC ) */

/*              DO I = 1, NREC */

/*     C */
/*     C           Get the record associated with record number I. */
/*     C */
/*                 CALL CKGR01 ( HANDLE, DESCR, I, RECORD ) */
/*                 WRITE (*,*) 'Record ', I, ':' */
/*                 WRITE (*,*)  RECORD */
/*              END DO */

/*           END IF */

/* $ Restrictions */

/*     1)  The binary CK file containing the segment whose descriptor was */
/*         passed to this routine must be opened for read access by */
/*         either CKLPF or DAFOPR. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     J.M. Lynch         (JPL) */
/*     W.L. Taber         (JPL) */
/*     R.E. Thurman       (JPL) */
/*     I.M. Underwood     (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.1, 26-OCT-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW) */

/*        Replaced DAFRDA call with DAFGDA. */
/*        Added IMPLICIT NONE. */

/* -    SPICELIB Version 1.0.3, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.2, 06-MAR-1991 (JML) */

/*        A correction was made to the example program in the */
/*        header. The array of double precision components of */
/*        the descriptor ( DCD ) had originally been declared */
/*        as an integer. */

/* -    SPICELIB Version 1.0.1, 02-NOV-1990 (JML) */

/*        The restriction that a C-kernel file must be loaded */
/*        was explicitly stated. */

/* -    SPICELIB Version 1.0.0, 07-SEP-1990 (RET) (IMU) */

/* -& */
/* $ Index_Entries */

/*     number of CK type_1 records */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.0.2, 06-MAR-1991 (JML) */

/*        A correction was made to the example program in the */
/*        header. The array of double precision components of */
/*        the descriptor ( DCD ) had originally been declared */
/*        as an integer. */

/* -    SPICELIB Version 1.0.1, 02-NOV-1990 (JML) */

/*        1) The restriction that a C-kernel file must be loaded */
/*           was explicitly stated. */
/*        2) Minor changes were made to the wording of the header. */

/* -    Beta Version 1.1.0, 28-AUG-1990 (MJS) (JEM) */

/*        The following changes were made as a result of the */
/*        NAIF CK Code and Documentation Review: */

/*        1) The name of this routine was changed from CK01NR to */
/*           CKNR01 in order to be consistent with the SPICELIB */
/*           naming convention. */
/*        2) The declarations for the parameters NDC and NIC were */
/*           moved from the "Declarations" section of the header to */
/*           the "Local parameters" section of the code below the */
/*           header. These parameters are not meant to modified by */
/*           users. */
/*        3) The variables INTDES and DPDES were changed to ICD and */
/*           DCD. */
/*        4) The header was corrected, improved, and updated to reflect */
/*           the changes. */
/*        5) The in-code comments were improved. */

/* -    Beta Version 1.0.0, 22-MAY-1990 (RET) (IMU) */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */

/*        NDC        is the number of double precision components in an */
/*                   unpacked C-kernel descriptor. */

/*        NIC        is the number of integer components in an unpacked */
/*                   C-kernel descriptor. */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("CKNR01", (ftnlen)6);
    }

/*     The number of pointing records contained in a data type 1 */
/*     segment is stored in the final double precision word of the */
/*     segment.  Since the address of this very word is stored in the */
/*     sixth integer component of the segment descriptor, it is a trivial */
/*     matter to extract the count. */

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

/*     If this segment is not of data type 1, then signal an error. */

    if (icd[2] != 1) {
	setmsg_("Data type of the segment should be 1: Passed descriptor sho"
		"ws type = #.", (ftnlen)71);
	errint_("#", &icd[2], (ftnlen)1);
	sigerr_("SPICE(CKWRONGDATATYPE)", (ftnlen)22);
	chkout_("CKNR01", (ftnlen)6);
	return 0;
    }

/*     The number of records is the final word in the segment. */

    dafgda_(handle, &icd[5], &icd[5], &n);
    *nrec = (integer) n;
    chkout_("CKNR01", (ftnlen)6);
    return 0;
} /* cknr01_ */

