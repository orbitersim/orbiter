/* pckeul.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__5 = 5;

/* $Procedure PCKEUL ( PCK, get Euler angles at time from PCK file ) */
/* Subroutine */ int pckeul_(integer *body, doublereal *et, logical *found, 
	char *ref, doublereal *eulang, ftnlen ref_len)
{
    integer iref, type__;
    extern /* Subroutine */ int pcke02_(doublereal *, doublereal *, 
	    doublereal *), chkin_(char *, ftnlen);
    doublereal descr[5];
    extern /* Subroutine */ int pckr02_(integer *, doublereal *, doublereal *,
	     doublereal *), dafus_(doublereal *, integer *, integer *, 
	    doublereal *, integer *);
    char ident[40];
    integer handle;
    extern /* Subroutine */ int irfnam_(integer *, char *, ftnlen);
    doublereal record[130];
    extern /* Subroutine */ int pcksfs_(integer *, doublereal *, integer *, 
	    doublereal *, char *, logical *, ftnlen), chkout_(char *, ftnlen);
    extern logical return_(void);
    doublereal dcd[2];
    integer icd[5];

/* $ Abstract */

/*     This routine is obsolete. It supports only the type 02 binary */
/*     PCK format. It is maintained only for backward compatibility */

/*     Return Euler angles and their derivatives and their reference */
/*     frame, given an input time and body and reference frame from */
/*     a PCK binary file. */

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

/*     NAIF_IDS */
/*     ROTATION */
/*     TIME */
/*     PCK */

/* $ Keywords */

/*     ROTATION */
/*     TRANSFORMATION */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     BODY       I   ID code of body */
/*     ET         I   Epoch of transformation */
/*     FOUND      O   .TRUE. if ET, BODY found in a PCK file */
/*     REF        O   Name of inertial ref. frame of state */
/*     EULANG     O   Euler angles and their derivatives. */

/* $ Detailed_Input */

/*     BODY     is the integer ID code of the body for which the */
/*              state transformation matrix is requested. Bodies */
/*              are numbered according to the standard NAIF */
/*              numbering scheme. The numbering scheme is */
/*              explained in the NAIF_IDS required reading file. */

/*     ET       is the epoch at which the state transformation */
/*              matrix is requested. */

/* $ Detailed_Output */

/*     FOUND    if the Euler angles for the requested time */
/*              and body are found in a PCK binary file, */
/*              FOUND is .TRUE. Otherwise, it's false. */

/*     REF      is the name of an inertial ref. frame. */
/*              (See the routine CHGIRF for a full list of names.) */

/*     EULANG   is the Euler angles and their derivatives at */
/*              time ET. The rotation matrix is */
/*              [ EULANG(3) ]  [EULANG(2)] [EULANG(1)] */
/*                           3            1           3 */

/*              and   dEULANG(1)/dt = EULANG(4) */
/*                    dEULANG(2)/dt = EULANG(5) */
/*                    dEULANG(3)/dt = EULANG(6) */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     Here we load a binary PCK files and use PCKEUL to get the */
/*     Euler angles. */

/*     C */
/*     C  Load binary PCK file. */
/*     C */
/*        CALL PCKLOF ('example.pck', HANDLE) */

/*     C  Call routine to get Euler angles phi, delta, w. */

/*        CALL PCKEUL ( BODY, ET, FOUND, REF, EULANG ) */

/*     The Euler angles and their derivatives are returned */
/*     in EULANG. */

/* $ Restrictions */

/*     1)  A binary PCK kernel must be loaded with PCKLOF before */
/*         calling this routine. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     E.D. Wright        (JPL) */
/*     K.S. Zukor         (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.1.0, 20-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 2.0.1, 03-JAN-2014 (EDW) */

/*        Minor edits to $Procedure; clean trailing whitespace. */
/*        Removed unneeded $Revisions section. */

/* -    SPICELIB Version 2.0.0, 21-MAR-1995 (KSZ) */

/*        PCKEUL modified to check in. PCKMAT takes */
/*        over for PCKEUL in many cases. REF now a character. */

/* -    SPICELIB Version 1.1.0, 18-OCT-1994 (KSZ) */

/*        Fixed bug which incorrectly modded DW by two pi. */

/* -    SPICELIB Version 1.0.0, 11-MAR-1994 (KSZ) */

/* -& */
/* $ Index_Entries */

/*     get Euler angles and their derivatives */

/* -& */

/*     SPICELIB functions */


/*     Parameters */

/*     ND    number of double precision components of descriptor */
/*     NI    number of integer components of descriptor */
/*     NR    component number of reference frame in integer */
/*           portion of descriptor */
/*     NS    size of a packed PCK segment descriptor */
/*     NT    component number of data type in integer portion */
/*           of descriptor */


/*  Local Variables */


/*     Standard SPICE Error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("PCKEUL", (ftnlen)6);
    }

/*     Get a segment applicable to a specified body and epoch. */

    pcksfs_(body, et, &handle, descr, ident, found, (ftnlen)40);
    if (*found) {

/*        Look at parts of the descriptor. */

	dafus_(descr, &c__2, &c__5, dcd, icd);
	type__ = icd[2];
	iref = icd[1];
	irfnam_(&iref, ref, ref_len);
	if (type__ == 2) {

/*           Read in Chebyshev coefficients from segment. */

	    pckr02_(&handle, descr, et, record);

/*           Call evaluation routine to get Euler angles */
/*           phi, delta, w. */

	    pcke02_(et, record, eulang);
	} else {

/*           If appropriate data was not found, found is false. */

	    *found = FALSE_;
	}
    }
    chkout_("PCKEUL", (ftnlen)6);
    return 0;
} /* pckeul_ */

