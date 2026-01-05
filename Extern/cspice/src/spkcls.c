/* spkcls.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure SPKCLS ( SPK, Close file ) */
/* Subroutine */ int spkcls_(integer *handle)
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

/*     Close an open SPK file. */

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
/*     HANDLE     I   Handle of the SPK file to be closed. */

/* $ Detailed_Input */

/*     HANDLE   is the handle of the SPK file that is to be closed. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If there are no segments in the file, the error */
/*         SPICE(NOSEGMENTSFOUND) is signaled. */

/* $ Files */

/*     See argument HANDLE. */

/* $ Particulars */

/*     Close the SPK file attached to HANDLE. The close operation tests */
/*     the file to ensure the presence of data segments. */

/*     A SPKCLS call should balance each call to SPKOPN. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) This example demonstrates how to create an SPK type 8 kernel */
/*        containing only one segment, given a time-ordered set of */
/*        discrete states and epochs. */


/*        Example code begins here. */


/*              PROGRAM SPKCLS_EX1 */
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
/*              PARAMETER           ( SPK8   = 'spkcls_ex1.bsp' ) */

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

/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     F.S. Turner        (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.1, 28-MAY-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */
/*        Added complete code example, based on the SPKW08 example. */

/*        Updated $Particulars section. Re-ordered header sections. */

/* -    SPICELIB Version 1.2.0, 07-SEP-2001 (EDW) */

/*        Removed DAFHLU call; replaced ERRFN call with ERRHAN. */

/* -    SPICELIB Version 1.1.0, 17-FEB-2000 (FST) */

/*        Removed the call to ZZFIXID. This will make all SPK files */
/*        created with future versions of the toolkit possess the */
/*        unambiguous ID word 'DAF/SPK '. */

/* -    SPICELIB Version 1.0.0, 27-JAN-1995 (KRG) */

/* -& */
/* $ Index_Entries */

/*     close an SPK file */

/* -& */

/*     SPICELIB functions */


/*     Local Parameters */


/*     Local Variables */


/*     Standard SPICELIB error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("SPKCLS", (ftnlen)6);

/*     Get the access method for the file. Currently, if HANDLE < 0, the */
/*     access method is 'WRITE'. If HANDLE > 0, the access method is */
/*     'READ'.  In the future this should make use of the private entry */
/*     in the handle manager umbrella, ZZDDHNFO. */

    if (*handle < 0) {
	s_copy(access, "WRITE", (ftnlen)5, (ftnlen)5);
    } else if (*handle > 0) {
	s_copy(access, "READ", (ftnlen)5, (ftnlen)4);
    }

/*     If the file is open for writing and there are segments in the file */
/*     fix the ID word and close the file, or just close the file. */

    if (s_cmp(access, "WRITE", (ftnlen)5, (ftnlen)5) == 0) {

/*        Check to see if there are any segments in the file. If there */
/*        are no segments, we signal an error. This probably indicates a */
/*        programming error of some sort anyway. Why would you create a */
/*        file and put nothing in it? */

	dafbfs_(handle);
	daffna_(&found);
	if (failed_()) {
	    chkout_("SPKCLS", (ftnlen)6);
	    return 0;
	}
	if (! found) {
	    setmsg_("No segments were found in the SPK file '#'. There must "
		    "be at least one segment in the file when this subroutine"
		    " is called.", (ftnlen)122);
	    errhan_("#", handle, (ftnlen)1);
	    sigerr_("SPICE(NOSEGMENTSFOUND)", (ftnlen)22);
	    chkout_("SPKCLS", (ftnlen)6);
	    return 0;
	}
    }

/*     Close the file. */

    dafcls_(handle);

/*     No need to check FAILED() here, since we only call spicelib */
/*     subroutines and return. The caller should check it though. */

    chkout_("SPKCLS", (ftnlen)6);
    return 0;
} /* spkcls_ */

