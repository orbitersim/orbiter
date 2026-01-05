/* spk14e.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure SPK14E ( SPK type 14: End a segment. ) */
/* Subroutine */ int spk14e_(integer *handle)
{
    extern /* Subroutine */ int chkin_(char *, ftnlen), sgwes_(integer *), 
	    chkout_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     End the type 14 SPK segment currently being written to the SPK */
/*     file associated with HANDLE. */

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

/*     SPK */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   The handle of an SPK file open for writing. */

/* $ Detailed_Input */

/*     HANDLE   is the file handle of an SPK file that has been */
/*              opened for writing, and to which a type 14 segment is */
/*              being written. */

/* $ Detailed_Output */

/*     None. The type 14 segment in the SPK file associated with HANDLE */
/*     will be ended, making the addition of the data to the file */
/*     permanent. */

/*     See the $Particulars section for details about the structure of a */
/*     type 14 SPK segment. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If there are no segments currently being written to the file */
/*         associated with HANDLE, an error is signaled by a routine in */
/*         the call tree of this routine. */

/*     2)  If any file access error occurs, the error is signaled by a */
/*         routine in the call tree of this routine. */

/* $ Files */

/*     See the argument HANDLE. */

/* $ Particulars */

/*     This routine ends a type 14 SPK segment which is being written to */
/*     the SPK file associated with HANDLE. Ending the SPK segment is a */
/*     necessary step in the process of making the data a permanent part */
/*     of the SPK file. */

/*     This routine is one of a set of three routines for creating and */
/*     adding data to type 14 SPK segments. These routines are: */

/*        SPK14B: Begin a type 14 SPK segment. This routine must be */
/*                called before any data may be added to a type 14 */
/*                segment. */

/*        SPK14A: Add data to a type 14 SPK segment. This routine may be */
/*                called any number of times after a call to SPK14B to */
/*                add type 14 records to the SPK segment that was */
/*                started. */

/*        SPK14E: End a type 14 SPK segment. This routine is called to */
/*                make the type 14 segment a permanent addition to the */
/*                SPK file. Once this routine is called, no further type */
/*                14 records may be added to the segment. A new segment */
/*                must be started. */

/*     A type 14 SPK segment consists of coefficient sets for fixed order */
/*     Chebyshev polynomials over consecutive time intervals, where the */
/*     time intervals need not all be of the same length. The Chebyshev */
/*     polynomials represent the position, X, Y, and Z coordinates, and */
/*     the velocities, dX/dt, dY/dt, and dZ/dt, of BODY relative to */
/*     CENTER. */

/*     The ephemeris data supplied to the type 14 SPK writer is packed */
/*     into an array as a sequence of logical records, */

/*        ----------------------------------------------------- */
/*        | Record 1 | Record 2 | ... | Record N-1 | Record N | */
/*        ----------------------------------------------------- */

/*     with each record has the following format. */

/*           ------------------------------------------------ */
/*           |  the midpoint of the approximation interval  | */
/*           ------------------------------------------------ */
/*           |   the radius of the approximation interval   | */
/*           ------------------------------------------------ */
/*           |  CHBDEG+1 coefficients for the X coordinate  | */
/*           ------------------------------------------------ */
/*           |  CHBDEG+1 coefficients for the Y coordinate  | */
/*           ------------------------------------------------ */
/*           |  CHBDEG+1 coefficients for the Z coordinate  | */
/*           ------------------------------------------------ */
/*           |   CHBDEG+1 coefficients for the X velocity   | */
/*           ------------------------------------------------ */
/*           |   CHBDEG+1 coefficients for the Y velocity   | */
/*           ------------------------------------------------ */
/*           |   CHBDEG+1 coefficients for the Z velocity   | */
/*           ------------------------------------------------ */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) This example demonstrates how to create an SPK type 14 kernel */
/*        containing only one segment, given a set of Chebyshev */
/*        coefficients and their associated epochs. */


/*        Example code begins here. */


/*              PROGRAM SPK14E_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local parameters. */
/*        C */
/*              INTEGER               NAMLEN */
/*              PARAMETER           ( NAMLEN = 42 ) */

/*        C */
/*        C     Define the segment identifier parameters. */
/*        C */
/*              CHARACTER*(*)         SPK14 */
/*              PARAMETER           ( SPK14  = 'spk14e_ex1.bsp' ) */

/*              CHARACTER*(*)         REF */
/*              PARAMETER           ( REF    = 'J2000'          ) */

/*              INTEGER               BODY */
/*              PARAMETER           ( BODY   = 3  ) */

/*              INTEGER               CENTER */
/*              PARAMETER           ( CENTER = 10 ) */

/*              INTEGER               CHBDEG */
/*              PARAMETER           ( CHBDEG = 2  ) */

/*              INTEGER               NRECS */
/*              PARAMETER           ( NRECS  = 4  ) */

/*              INTEGER               RECSIZ */
/*              PARAMETER           ( RECSIZ = 2 + 6*(CHBDEG+1) ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              CHARACTER*(NAMLEN)    IFNAME */
/*              CHARACTER*(NAMLEN)    SEGID */

/*              DOUBLE PRECISION      EPOCHS ( NRECS + 1 ) */
/*              DOUBLE PRECISION      FIRST */
/*              DOUBLE PRECISION      LAST */
/*              DOUBLE PRECISION      RECRDS ( RECSIZ, NRECS ) */

/*              INTEGER               HANDLE */
/*              INTEGER               NCOMCH */

/*        C */
/*        C     Define the epochs and coefficients. */
/*        C */
/*              DATA                  EPOCHS / */
/*             .                100.D0, 200.D0, 300.D0, 400.D0, 500.D0 / */

/*              DATA                  RECRDS / */
/*             .     150.0D0, 50.0D0, 1.0101D0, 1.0102D0, 1.0103D0, */
/*             .                      1.0201D0, 1.0202D0, 1.0203D0, */
/*             .                      1.0301D0, 1.0302D0, 1.0303D0, */
/*             .                      1.0401D0, 1.0402D0, 1.0403D0, */
/*             .                      1.0501D0, 1.0502D0, 1.0503D0, */
/*             .                      1.0601D0, 1.0602D0, 1.0603D0, */
/*             .     250.0D0, 50.0D0, 2.0101D0, 2.0102D0, 2.0103D0, */
/*             .                      2.0201D0, 2.0202D0, 2.0203D0, */
/*             .                      2.0301D0, 2.0302D0, 2.0303D0, */
/*             .                      2.0401D0, 2.0402D0, 2.0403D0, */
/*             .                      2.0501D0, 2.0502D0, 2.0503D0, */
/*             .                      2.0601D0, 2.0602D0, 2.0603D0, */
/*             .     350.0D0, 50.0D0, 3.0101D0, 3.0102D0, 3.0103D0, */
/*             .                      3.0201D0, 3.0202D0, 3.0203D0, */
/*             .                      3.0301D0, 3.0302D0, 3.0303D0, */
/*             .                      3.0401D0, 3.0402D0, 3.0403D0, */
/*             .                      3.0501D0, 3.0502D0, 3.0503D0, */
/*             .                      3.0601D0, 3.0602D0, 3.0603D0, */
/*             .     450.0D0, 50.0D0, 4.0101D0, 4.0102D0, 4.0103D0, */
/*             .                      4.0201D0, 4.0202D0, 4.0203D0, */
/*             .                      4.0301D0, 4.0302D0, 4.0303D0, */
/*             .                      4.0401D0, 4.0402D0, 4.0403D0, */
/*             .                      4.0501D0, 4.0502D0, 4.0503D0, */
/*             .                      4.0601D0, 4.0602D0, 4.0603D0 / */


/*        C */
/*        C     Set the start and end times of interval covered by */
/*        C     segment. */
/*        C */
/*              FIRST = EPOCHS(1) */
/*              LAST  = EPOCHS(NRECS + 1) */

/*        C */
/*        C     NCOMCH is the number of characters to reserve for the */
/*        C     kernel's comment area. This example doesn't write */
/*        C     comments, so set to zero. */
/*        C */
/*              NCOMCH = 0 */

/*        C */
/*        C     Internal file name and segment ID. */
/*        C */
/*              IFNAME = 'Type 14 SPK internal file name.' */
/*              SEGID  = 'SPK type 14 test segment' */

/*        C */
/*        C     Open a new SPK file. */
/*        C */
/*              CALL SPKOPN( SPK14, IFNAME, NCOMCH, HANDLE ) */

/*        C */
/*        C     Begin the segment. */
/*        C */
/*              CALL SPK14B ( HANDLE, SEGID, BODY, CENTER, REF, */
/*             .              FIRST,  LAST,  CHBDEG            ) */

/*        C */
/*        C     Add the data to the segment all at once. */
/*        C */
/*              CALL SPK14A ( HANDLE, NRECS, RECRDS, EPOCHS ) */

/*        C */
/*        C     End the segment, making the segment a permanent addition */
/*        C     to the SPK file. */
/*        C */
/*              CALL SPK14E ( HANDLE ) */

/*        C */
/*        C     Close the SPK file. */
/*        C */
/*              CALL SPKCLS ( HANDLE ) */

/*              END */


/*        When this program is executed, no output is presented on */
/*        screen. After run completion, a new SPK type 14 exists in */
/*        the output directory. */

/* $ Restrictions */

/*     1)  The type 14 SPK segment being closed must have been started by */
/*         the routine SPK14B, the routine which begins a type 14 SPK */
/*         segment. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 27-AUG-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. Added */
/*        complete example code from existing fragment. */

/*        Updated $Exceptions section. */

/*        Re-ordered header sections. Removed references to other */
/*        routines from $Abstract section (present in $Particulars). */

/* -    SPICELIB Version 1.0.0, 06-MAR-1995 (KRG) */

/* -& */
/* $ Index_Entries */

/*     end a type_14 SPK segment */

/* -& */

/*     Spicelib functions */


/*     Standard SPICELIB error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("SPK14E", (ftnlen)6);
    }

/*     This is simple, just call the routine which ends a generic */
/*     segment. */

    sgwes_(handle);

/*     No need to check FAILED() here, since all we do is check out. */
/*     Leave it up to the caller. */
    chkout_("SPK14E", (ftnlen)6);
    return 0;
} /* spk14e_ */

