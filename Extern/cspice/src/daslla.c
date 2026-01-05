/* daslla.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure DASLLA ( DAS, last logical addresses ) */
/* Subroutine */ int daslla_(integer *handle, integer *lastc, integer *lastd, 
	integer *lasti)
{
    integer free;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer ncomc, ncomr;
    extern /* Subroutine */ int dashfs_(integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, integer *);
    integer lastla[3], lastrc[3];
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    integer lastwd[3], nresvc;
    extern logical return_(void);
    integer nresvr;

/* $ Abstract */

/*     Return last DAS logical addresses of character, double precision */
/*     and integer type that are currently in use in a specified DAS */
/*     file. */

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

/*     DAS */

/* $ Keywords */

/*     ARRAY */
/*     DAS */
/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   DAS file handle. */
/*     LASTC      O   Last character address in use. */
/*     LASTD      O   Last double precision address in use. */
/*     LASTI      O   Last integer address in use. */

/* $ Detailed_Input */

/*     HANDLE   is the file handle of a DAS file whose active */
/*              logical address ranges are desired. */

/* $ Detailed_Output */

/*     LASTC, */
/*     LASTD, */
/*     LASTI    are, respectively, the last 1-based logical addresses of */
/*              character, double precision, and integer type in use in */
/*              the specified DAS file. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the input file handle is invalid, an error is signaled by */
/*         a routine in the call tree of this routine. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine is a utility that allows a calling program to */
/*     find the range of logical addresses currently in use in any */
/*     DAS file. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Create a DAS file containing 10 integers, 5 double precision */
/*        numbers, and 4 characters, then use DASLLA to find the logical */
/*        address ranges in use. */


/*        Example code begins here. */


/*              PROGRAM DASLLA_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local parameters. */
/*        C */
/*              CHARACTER*(*)         FNAME */
/*              PARAMETER           ( FNAME = 'daslla_ex1.das' ) */

/*              INTEGER               IFNMLN */
/*              PARAMETER           ( IFNMLN = 60 ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              CHARACTER*(IFNMLN)    IFNAME */
/*              CHARACTER*(4)         TYPE */

/*              INTEGER               HANDLE */
/*              INTEGER               I */
/*              INTEGER               LASTC */
/*              INTEGER               LASTD */
/*              INTEGER               LASTI */

/*        C */
/*        C     Open a new DAS file. Use the file name as the internal */
/*        C     file name, and reserve no records for comments. */
/*        C */
/*              TYPE   = 'TEST' */
/*              IFNAME = 'TEST.DAS/NAIF/NJB/11-NOV-1992-20:12:20' */

/*              CALL DASONW ( FNAME, TYPE, IFNAME, 0, HANDLE ) */

/*              DO I = 1, 10 */
/*                 CALL DASADI ( HANDLE, 1, I ) */
/*              END DO */

/*              DO I = 1, 5 */
/*                 CALL DASADD ( HANDLE, 1, DBLE(I) ) */
/*              END DO */

/*        C */
/*        C     Add character data to the file. DAS character data are */
/*        C     treated as a character array, not as a string. The */
/*        C     following call adds only the first 4 characters to the */
/*        C     DAS file. */
/*        C */
/*              CALL DASADC ( HANDLE, 4, 1, 4, 'SPUDWXY' ) */

/*        C */
/*        C     Now check the logical address ranges. */
/*        C */
/*              CALL DASLLA ( HANDLE, LASTC, LASTD, LASTI ) */

/*              WRITE (*,*) 'Last character address in use: ', LASTC */
/*              WRITE (*,*) 'Last d.p. address in use     : ', LASTD */
/*              WRITE (*,*) 'Last integer address in use  : ', LASTI */

/*        C */
/*        C     Close the DAS file. */
/*        C */
/*              CALL DASCLS ( HANDLE ) */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Last character address in use:            4 */
/*         Last d.p. address in use     :            5 */
/*         Last integer address in use  :           10 */


/*        Note that after run completion, a new DAS file exists in the */
/*        output directory. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 30-JUN-2021 (JDR) */

/*        Added IMPLICIT NONE statement. Local parameter declarations */
/*        have been moved from the $Declarations section to the */
/*        procedure's code. */

/*        Edited the header to comply with NAIF standard. Added complete */
/*        code example from existing fragment. */

/* -    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) */

/* -& */
/* $ Index_Entries */

/*     return last logical addresses in DAS file */
/*     return logical address range of DAS file */

/* -& */

/*     SPICELIB functions */


/*     Local parameters. */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("DASLLA", (ftnlen)6);

/*     The file summary for the indicated DAS file contains all of the */
/*     information we need. */

    dashfs_(handle, &nresvr, &nresvc, &ncomr, &ncomc, &free, lastla, lastrc, 
	    lastwd);
    *lastc = lastla[0];
    *lastd = lastla[1];
    *lasti = lastla[2];
    chkout_("DASLLA", (ftnlen)6);
    return 0;
} /* daslla_ */

