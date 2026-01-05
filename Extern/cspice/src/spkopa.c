/* spkopa.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure SPKOPA ( SPK open for addition ) */
/* Subroutine */ int spkopa_(char *file, integer *handle, ftnlen file_len)
{
    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    char arch[8], type__[8];
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen);
    extern logical failed_(void);
    extern /* Subroutine */ int getfat_(char *, char *, char *, ftnlen, 
	    ftnlen, ftnlen), dafopw_(char *, integer *, ftnlen), sigerr_(char 
	    *, ftnlen), chkout_(char *, ftnlen), setmsg_(char *, ftnlen);
    extern logical exists_(char *, ftnlen), return_(void);

/* $ Abstract */

/*     Open an existing SPK file for subsequent write. */

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
/*     FILE       I   The name of an existing SPK file. */
/*     HANDLE     O   Handle attached to the SPK file opened to append. */

/* $ Detailed_Input */

/*     FILE     is the name of an existing SPK file to which */
/*              you wish to append additional SPK segments. */

/* $ Detailed_Output */

/*     HANDLE   is the DAF integer handle that refers to the SPK file */
/*              opened for appending. HANDLE is required by any of the */
/*              SPK writing routines. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     If any of the following exceptions occur, HANDLE will be returned */
/*     with the value 0. */

/*     1)  If the file specified does not exist, the error */
/*         SPICE(FILENOTFOUND) is signaled. */

/*     2)  If the file specified is not an SPK file, the error */
/*         SPICE(FILEISNOTSPK) is signaled. */

/*     3)  If the specified SPK file cannot be opened for writing, an */
/*         error is signaled by a routine in the call tree of this */
/*         routine. */

/*     4)  If the specified SPK file uses a non-native binary file */
/*         format, an error is signaled by a routine in the call tree of */
/*         this routine. */

/*     5)  If the specified SPK file is corrupted or otherwise invalid, */
/*         an error is signaled by a routine in the call tree of this */
/*         routine. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This file provides an interface for opening existing SPK */
/*     files for the addition of SPK segments. If you need */
/*     to open an new SPK file for writing, call the routine SPKOPN. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) This example demonstrates how to add a new segment to a */
/*        new and to an existing SPK file. */

/*        For this example, we will first create an SPK file containing */
/*        only one type 5 segment, given a time-ordered set of */
/*        discrete states and epochs, and the gravitational parameter */
/*        of a central body. */

/*        Then, we will reopen the SPK and add another type 5 segment. */
/*        The example below shows one set of calls that you could */
/*        perform to make the addition. Obviously, there is no need to */
/*        close and re-open the file in order to add multiple segments. */
/*        It is done in this example to demonstrate the use of SPKOPA. */

/*        Note that you could add segments of other data types by */
/*        replacing the call to SPKW05 with a suitably modified call to */
/*        another SPK writing routine. */


/*        Example code begins here. */


/*              PROGRAM SPKOPA_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local parameters. */
/*        C */
/*              INTEGER               NAMLEN */
/*              PARAMETER           ( NAMLEN = 40 ) */

/*        C */
/*        C     Define the segment identifier parameters. */
/*        C */
/*              CHARACTER*(*)         SPK5 */
/*              PARAMETER           ( SPK5  = 'spkopa_ex1.bsp' ) */

/*              CHARACTER*(*)         REF */
/*              PARAMETER           ( REF    = 'J2000'          ) */

/*              DOUBLE PRECISION      GMSUN */
/*              PARAMETER           ( GMSUN  = 132712440023.310D0 ) */

/*              INTEGER               BODY */
/*              PARAMETER           ( BODY   = 3  ) */

/*              INTEGER               CENTER */
/*              PARAMETER           ( CENTER = 10 ) */

/*              INTEGER               NSTATS */
/*              PARAMETER           ( NSTATS = 9  ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              CHARACTER*(NAMLEN)    IFNAME */
/*              CHARACTER*(NAMLEN)    SEGID */

/*              DOUBLE PRECISION      EPOCH1 (    NSTATS ) */
/*              DOUBLE PRECISION      EPOCH2 (    NSTATS ) */
/*              DOUBLE PRECISION      FIRST */
/*              DOUBLE PRECISION      LAST */
/*              DOUBLE PRECISION      STATES ( 6, NSTATS ) */

/*              INTEGER               HANDLE */
/*              INTEGER               NCOMCH */

/*        C */
/*        C     Define the states and epochs. */
/*        C */
/*              DATA                  STATES / */
/*             .       101.D0, 201.D0, 301.D0, 401.D0, 501.D0, 601.D0, */
/*             .       102.D0, 202.D0, 302.D0, 402.D0, 502.D0, 602.D0, */
/*             .       103.D0, 203.D0, 303.D0, 403.D0, 503.D0, 603.D0, */
/*             .       104.D0, 204.D0, 304.D0, 404.D0, 504.D0, 604.D0, */
/*             .       105.D0, 205.D0, 305.D0, 405.D0, 505.D0, 605.D0, */
/*             .       106.D0, 206.D0, 306.D0, 406.D0, 506.D0, 606.D0, */
/*             .       107.D0, 207.D0, 307.D0, 407.D0, 507.D0, 607.D0, */
/*             .       108.D0, 208.D0, 308.D0, 408.D0, 508.D0, 608.D0, */
/*             .       109.D0, 209.D0, 309.D0, 409.D0, 509.D0, 609.D0 / */

/*              DATA                  EPOCH1 / 100.D0, 200.D0, 300.D0, */
/*             .                               400.D0, 500.D0, 600.D0, */
/*             .                               700.D0, 800.D0, 900.D0 / */

/*              DATA                  EPOCH2 / */
/*             .                            1100.D0, 1200.D0, 1300.D0, */
/*             .                            1400.D0, 1500.D0, 1600.D0, */
/*             .                            1700.D0, 1800.D0, 1900.D0 / */

/*        C */
/*        C     Set the start and stop times of interval covered by */
/*        C     the first segment. */
/*        C */
/*              FIRST  = EPOCH1(1) */
/*              LAST   = EPOCH1(NSTATS) */

/*        C */
/*        C     NCOMCH is the number of characters to reserve for the */
/*        C     kernel's comment area. This example doesn't write */
/*        C     comments, so set to zero. */
/*        C */
/*              NCOMCH = 0 */

/*        C */
/*        C     Internal file name and segment ID. */
/*        C */
/*              IFNAME = 'Type 5 SPK internal file name.' */
/*              SEGID  = 'SPK type 5 test segment #1' */

/*        C */
/*        C     Open a new SPK file. */
/*        C */
/*              CALL SPKOPN( SPK5, IFNAME, NCOMCH, HANDLE ) */

/*        C */
/*        C     Write the segment. */
/*        C */
/*              CALL SPKW05 ( HANDLE, BODY,   CENTER, REF, */
/*             .              FIRST,  LAST,   SEGID,  GMSUN, */
/*             .              NSTATS, STATES, EPOCH1        ) */

/*        C */
/*        C     Close the SPK file. */
/*        C */
/*              CALL SPKCLS ( HANDLE ) */

/*        C */
/*        C     At this point we have an existing SPK type 5 kernel */
/*        C     that contains a single segment. Let's now demonstrate */
/*        C     the use of SPKOPA. */
/*        C */
/*        C     Open the an existing SPK file for subsequent write. */
/*        C */
/*              CALL SPKOPA ( SPK5, HANDLE ) */

/*        C */
/*        C     Set the start and stop times of interval covered by */
/*        C     the second segment, and the segment ID. */
/*        C */
/*              FIRST  = EPOCH2(1) */
/*              LAST   = EPOCH2(NSTATS) */

/*              SEGID  = 'SPK type 5 test segment #2' */

/*        C */
/*        C     Now write the second segment. Use the same set of */
/*        C     states time-ordered set of discrete states and the */
/*        C     gravitational parameter. Set the epochs to be EPOCH2. */
/*        C */
/*              CALL SPKW05 ( HANDLE, BODY,   CENTER, REF, */
/*             .              FIRST,  LAST,   SEGID,  GMSUN, */
/*             .              NSTATS, STATES, EPOCH2        ) */

/*        C */
/*        C     Finally, close the file. */
/*        C */
/*              CALL SPKCLS ( HANDLE ) */


/*              END */


/*        When this program is executed, no output is presented on */
/*        screen. After run completion, a new SPK type 5, with two */
/*        segments, exists in the output directory. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Added complete */
/*        code example. */

/*        Updated the contents of $Detailed_Output and $Exceptions. */

/* -    SPICELIB Version 1.0.0, 10-MAR-1999 (WLT) */

/* -& */
/* $ Index_Entries */

/*     Open an existing SPK file for adding segments */

/* -& */

/*     SPICELIB Functions */


/*     Local parameters */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("SPKOPA", (ftnlen)6);

/*     Until we get a legitimate handle we set HANDLE to zero. */

    *handle = 0;

/*     First make sure the file exists. */

    if (! exists_(file, file_len)) {
	setmsg_("The file '#' is not recognized as an existing file. ", (
		ftnlen)52);
	errch_("#", file, (ftnlen)1, file_len);
	sigerr_("SPICE(FILENOTFOUND)", (ftnlen)19);
	chkout_("SPKOPA", (ftnlen)6);
	return 0;
    }

/*     Next make sure it is an SPK file. */

    getfat_(file, arch, type__, file_len, (ftnlen)8, (ftnlen)8);
    if (failed_()) {
	chkout_("SPKOPA", (ftnlen)6);
	return 0;
    }
    if (s_cmp(arch, "DAF", (ftnlen)8, (ftnlen)3) != 0 || s_cmp(type__, "SPK", 
	    (ftnlen)8, (ftnlen)3) != 0) {
	setmsg_("The file '#' was not an SPK file.  The architecture and typ"
		"e of the file were found to be '#' and '#' respectively. ", (
		ftnlen)116);
	errch_("#", file, (ftnlen)1, file_len);
	errch_("#", arch, (ftnlen)1, (ftnlen)8);
	errch_("#", type__, (ftnlen)1, (ftnlen)8);
	sigerr_("SPICE(FILEISNOTSPK)", (ftnlen)19);
	chkout_("SPKOPA", (ftnlen)6);
	return 0;
    }

/*     That's the limit of the checks performed here.  We let DAFOPW */
/*     handle the remaining checks. */

    dafopw_(file, handle, file_len);
    if (failed_()) {
	*handle = 0;
    }
    chkout_("SPKOPA", (ftnlen)6);
    return 0;
} /* spkopa_ */

