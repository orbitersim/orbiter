/* pcke02.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure PCKE02 ( PCK, evaluate data record from type 2 segment ) */
/* Subroutine */ int pcke02_(doublereal *et, doublereal *record, doublereal *
	eulang)
{
    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */
    double d_mod(doublereal *, doublereal *);

    /* Local variables */
    extern /* Subroutine */ int chkin_(char *, ftnlen), spke02_(doublereal *, 
	    doublereal *, doublereal *);
    extern doublereal twopi_(void);
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Evaluate a single PCK data record from a segment of type 2 */
/*     (Chebyshev Polynomials, position only). */

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

/*     PCK */

/* $ Keywords */

/*     ROTATION */
/*     TRANSFORMATION */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     ET         I   Epoch. */
/*     RECORD     I   Data record. */
/*     EULANG     O   Euler angles and their derivatives. */

/* $ Detailed_Input */

/*     ET       is an epoch, at which the Euler angles are to */
/*              be computed. */

/*     RECORD   is a data record which, when evaluated at epoch ET, */
/*              will give the Euler angles of some body. */

/* $ Detailed_Output */

/*     EULANG   is the Euler angles and their derivatives at */
/*              time ET. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The exact format and structure of type 2 (Chebyshev polynomials, */
/*     position only) segments are described in the PCK Required Reading */
/*     file. */

/*     A type 2 segment contains three sets of Chebyshev coefficients, */
/*     one set each for the Euler angles phi, delta and psi.  PCKE02 */
/*     calls the routine SPKE02 for each set to evaluate the polynomial */
/*     AND its first derivative. */

/* $ Examples */

/*     The PCKEnn routines are almost always used in conjunction with */
/*        the corresponding PCKRnn routines, which read the records from */
/*        binary PCK files. */

/*        The data returned by the PCKRnn routine is in its rawest form, */
/*        taken directly from the segment. As such, it will be */
/*        meaningless to a user unless he/she understands the structure */
/*        of the data type completely. Given that understanding, however, */
/*        the PCKRnn routines might be used to examine raw segment data */
/*        before evaluating it with the PCKEnn routines. */


/*     Here we load a binary PCK files and use PCKE02 to get the */
/*     Euler angles. */

/*     C */
/*     C  Load binary PCK file. */
/*     C */
/*        CALL PCKLOF ('example.pck', HANDLE) */


/*     C  Get a segment applicable to a specified body and epoch. */

/*        CALL PCKSFS ( BODY, ET, HANDLE, DESCR, IDENT, FOUND ) */

/*        IF ( FOUND ) THEN */


/*           Look at parts of the descriptor. */

/*           CALL DAFUS ( DESCR, ND, NI, DCD, ICD ) */
/*           TYPE   = ICD( NT ) */
/*           REF    = ICD( NR ) */

/*           IF ( TYPE .EQ. 2 ) THEN */

/*              Read in Chebyshev coefficients from segment. */

/*              CALL PCKR02 ( HANDLE, DESCR, ET, RECORD ) */


/*              Call evaluation routine to get Euler angles */
/*              phi, delta, w. */

/*              CALL PCKE02 ( ET, RECORD, EULANG ) */


/*     The Euler angles and their derivatives are returned */
/*     in EULANG. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     E.D. Wright        (JPL) */
/*     K.S. Zukor         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.0, 20-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.1.1, 03-JAN-2014 (EDW) */

/*        Minor edits to $Procedure; clean trailing whitespace. */
/*        Removed unneeded $Revisions section. */

/* -    SPICELIB Version 1.1.0, 13-MAR-1995 (KSZ) */

/*         Added error handling. */

/* -    SPICELIB Version 1.0.0, 30-SEP-1994 (KSZ) */

/* -& */
/* $ Index_Entries */

/*     get Euler angles and their derivatives */

/* -& */

/*     SPICELIB functions */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("PCKE02", (ftnlen)6);
    }

/*     Call evaluation routine to get Euler angles */
/*     phi, delta, w. */

    spke02_(et, record, eulang);

/*     Mod the 3rd element of the state by TWOPI. */
/*     We do this because we've always done this. */

    d__1 = twopi_();
    eulang[2] = d_mod(&eulang[2], &d__1);
    chkout_("PCKE02", (ftnlen)6);
    return 0;
} /* pcke02_ */

