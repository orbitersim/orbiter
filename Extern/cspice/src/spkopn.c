/* spkopn.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__6 = 6;

/* $Procedure SPKOPN ( SPK, open new file. ) */
/* Subroutine */ int spkopn_(char *fname, char *ifname, integer *ncomch, 
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

/*     Create a new SPK file, returning the handle of the opened file. */

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

/*     SPK */

/* $ Keywords */

/*     SPK */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     FNAME      I   The name of the new SPK file to be created. */
/*     IFNAME     I   The internal filename for the SPK file. */
/*     NCOMCH     I   The number of characters to reserve for comments. */
/*     HANDLE     O   The handle of the opened SPK file. */

/* $ Detailed_Input */

/*     FNAME    is the name of the new SPK file to be created. */

/*     IFNAME   is the internal filename for the SPK file that is */
/*              being created. The internal filename may be up to 60 */
/*              characters long. If you do not have any conventions */
/*              for tagging your files, an internal filename of */
/*              'SPK_file' is perfectly acceptable. You may also leave */
/*              it blank if you like. */

/*     NCOMCH   is the space, measured in characters, to be initially */
/*              set aside for the comment area when a new SPK file */
/*              is opened. The amount of space actually set aside may */
/*              be greater than the amount requested, due to the */
/*              manner in which comment records are allocated in an */
/*              SPK file. However, the amount of space set aside for */
/*              comments will always be at least the amount that was */
/*              requested. */

/*              The value of NCOMCH should be greater than or equal to */
/*              zero, i.e., 0 <= NCOMCH. A negative value, should one */
/*              occur, will be assumed to be zero. */

/* $ Detailed_Output */

/*     HANDLE   is the handle of the opened SPK file. If an error */
/*              occurs when opening the file, the value of this */
/*              variable should not be used, as it will not represent */
/*              a valid handle. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the value of NCOMCH is negative, a value of zero (0) will */
/*         be used for the number of comment characters to be set aside */
/*         for comments. */

/*     2)  If an error occurs while attempting to open the SPK file, the */
/*         value of HANDLE will not represent a valid file handle. */

/* $ Files */

/*     See FNAME and HANDLE. */

/* $ Particulars */

/*     Open a new SPK file, reserving room for comments if requested. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) This example demonstrates how to create an SPK type 8 kernel */
/*        containing only one segment, given a time-ordered set of */
/*        discrete states and epochs. */


/*        Example code begins here. */


/*              PROGRAM SPKOPN_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local parameters. */
/*        C */
/*              INTEGER               NAMLEN */
/*              PARAMETER           ( NAMLEN = 42 ) */

/*        C */
/*        C     Define the segment identifier parameters. */
/*        C */
/*              CHARACTER*(*)         SPK8 */
/*              PARAMETER           ( SPK8   = 'spkopn_ex1.bsp' ) */

/*              CHARACTER*(*)         REF */
/*              PARAMETER           ( REF    = 'J2000'          ) */

/*              INTEGER               BODY */
/*              PARAMETER           ( BODY   = 3  ) */

/*              INTEGER               CENTER */
/*              PARAMETER           ( CENTER = 10 ) */

/*              INTEGER               DEGREE */
/*              PARAMETER           ( DEGREE = 3  ) */

/*              INTEGER               NSTATS */
/*              PARAMETER           ( NSTATS = 9  ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              CHARACTER*(NAMLEN)    IFNAME */
/*              CHARACTER*(NAMLEN)    SEGID */

/*              DOUBLE PRECISION      BEGTIM */
/*              DOUBLE PRECISION      FIRST */
/*              DOUBLE PRECISION      LAST */
/*              DOUBLE PRECISION      STATES ( 6, NSTATS ) */
/*              DOUBLE PRECISION      STEP */

/*              INTEGER               HANDLE */
/*              INTEGER               NCOMCH */

/*        C */
/*        C     Set the array of discrete states to write to the SPK */
/*        C     segment. */
/*        C */
/*              DATA                  STATES / */
/*             .        101.D0, 201.D0, 301.D0, 401.D0, 501.D0, 601.D0, */
/*             .        102.D0, 202.D0, 302.D0, 402.D0, 502.D0, 602.D0, */
/*             .        103.D0, 203.D0, 303.D0, 403.D0, 503.D0, 603.D0, */
/*             .        104.D0, 204.D0, 304.D0, 404.D0, 504.D0, 604.D0, */
/*             .        105.D0, 205.D0, 305.D0, 405.D0, 505.D0, 605.D0, */
/*             .        106.D0, 206.D0, 306.D0, 406.D0, 506.D0, 606.D0, */
/*             .        107.D0, 207.D0, 307.D0, 407.D0, 507.D0, 607.D0, */
/*             .        108.D0, 208.D0, 308.D0, 408.D0, 508.D0, 608.D0, */
/*             .        109.D0, 209.D0, 309.D0, 409.D0, 509.D0, 609.D0  / */


/*        C */
/*        C     Set the start and end times of interval covered by */
/*        C     segment, and the time step separating epochs of states. */
/*        C */
/*              FIRST = 100.D0 */
/*              LAST  = 900.D0 */
/*              STEP  = 100.D0 */

/*        C */
/*        C     NCOMCH is the number of characters to reserve for the */
/*        C     kernel's comment area. This example doesn't write */
/*        C     comments, so set to zero. */
/*        C */
/*              NCOMCH = 0 */

/*        C */
/*        C     Internal file name and segment ID. */
/*        C */
/*              IFNAME = 'Type 8 SPK internal file name.' */
/*              SEGID  = 'SPK type 8 test segment' */

/*        C */
/*        C     Open a new SPK file. */
/*        C */
/*              CALL SPKOPN( SPK8, IFNAME, NCOMCH, HANDLE ) */

/*        C */
/*        C     Set the epoch of first state in STATES array to be */
/*        C     the start time of the interval covered by the segment. */
/*        C */
/*              BEGTIM = FIRST */

/*        C */
/*        C     Create a type 8 segment. */
/*        C */
/*              CALL SPKW08 (  HANDLE,  BODY,    CENTER,  REF, */
/*             .               FIRST,   LAST,    SEGID,   DEGREE, */
/*             .               NSTATS,  STATES,  BEGTIM,  STEP     ) */

/*        C */
/*        C     Close the SPK file. */
/*        C */
/*              CALL SPKCLS ( HANDLE ) */

/*              END */


/*        When this program is executed, no output is presented on */
/*        screen. After run completion, a new SPK type 8 exists in */
/*        the output directory. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.1.0, 05-AUG-2021 (JDR) */

/*        Changed the output argument name NAME to FNAME for consistency */
/*        with other routines. */

/*        Edited the header to comply with NAIF standard. Removed */
/*        unnecessary $Revisions section. */

/*        Added complete code example based on SPKW08 example. */

/*        Fixed typo in $Exceptions entry #2. */

/* -    SPICELIB Version 2.0.0, 09-NOV-2006 (NJB) */

/*        Routine has been upgraded to support comment */
/*        area allocation using NCOMCH. */

/* -    SPICELIB Version 1.0.0, 26-JAN-1995 (KRG) */

/* -& */
/* $ Index_Entries */

/*     open a new SPK file */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     DAF ND and NI values for SPK files. */


/*     Length of a DAF comment record, in characters. */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("SPKOPN", (ftnlen)6);

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

    dafonw_(fname, "SPK", &c__2, &c__6, ifname, &ncomr, handle, fname_len, (
	    ftnlen)3, ifname_len);
    if (failed_()) {

/*        If we failed, make sure that HANDLE does not contain a value */
/*        that represents a valid DAF file handle. */

	*handle = 0;
    }
    chkout_("SPKOPN", (ftnlen)6);
    return 0;
} /* spkopn_ */

