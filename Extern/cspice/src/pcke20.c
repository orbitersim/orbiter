/* pcke20.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure PCKE20 ( PCK, evaluate record, type 20 ) */
/* Subroutine */ int pcke20_(doublereal *et, doublereal *record, doublereal *
	eulang)
{
    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */
    double d_mod(doublereal *, doublereal *);

    /* Local variables */
    extern /* Subroutine */ int chkin_(char *, ftnlen), spke20_(doublereal *, 
	    doublereal *, doublereal *);
    extern doublereal twopi_(void);
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Evaluate a single PCK data record from a segment of type 20 */
/*     (Chebyshev Polynomials, rotation derivative only). */

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

/*     ORIENTATION */
/*     ROTATION */
/*     TRANSFORMATION */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     ET         I   Evaluation epoch. */
/*     RECORD     I   Data record. */
/*     EULANG     O   Euler angles and their derivatives. */

/* $ Detailed_Input */

/*     ET       is an epoch at which the Euler angles state is to be */
/*              computed. The epoch is represented as seconds past */
/*              J2000 TDB. */


/*     RECORD   is a data record which, when evaluated at epoch ET, */
/*              will yield Euler angles and Euler angle rates */
/*              representing the orientation and angular velocity, */
/*              with respect to its base frame, of the reference */
/*              frame associated with the input record. */

/*              The structure of the record is as follows: */

/*                 +--------------------------------------+ */
/*                 | record size (excluding this element) | */
/*                 +--------------------------------------+ */
/*                 | Coverage interval midpoint           | */
/*                 +--------------------------------------+ */
/*                 | Coverage interval radius             | */
/*                 +--------------------------------------+ */
/*                 | Coeffs for ANGLE_1 rate              | */
/*                 +--------------------------------------+ */
/*                 | Coeffs for ANGLE_2 rate              | */
/*                 +--------------------------------------+ */
/*                 | Coeffs for ANGLE_3 rate              | */
/*                 +--------------------------------------+ */
/*                 | ANGLE_1 at interval midpoint         | */
/*                 +--------------------------------------+ */
/*                 | ANGLE_2 at interval midpoint         | */
/*                 +--------------------------------------+ */
/*                 | ANGLE_3 at interval midpoint         | */
/*                 +--------------------------------------+ */

/*              In the above record */

/*                 - Times are expressed as seconds past J2000 TDB. */
/*                 - Angular components have units of radians. */
/*                 - Rate coefficients have units of radians/s. */

/*              RECORD must be declared by the caller with size large */
/*              enough to accommodate the largest record that can be */
/*              returned by PCKR20. */

/* $ Detailed_Output */

/*     EULANG   is a 6-vector containing Euler angles and their */
/*              derivatives at time ET. The angles occupy the first */
/*              three elements of EULANG; the rates follow. The order */
/*              of the components is */

/*                 ( ANGLE_1, ANGLE_2, ANGLE_3, */
/*                   rate_1,  rate_2,  rate_3  ) */

/*              The angular units are radians; the rate units are */
/*              radians/second. */

/*              The Euler angles represent the orientation, relative */
/*              to its base frame, of the PCK frame associated with */
/*              the input record. The angles, which are numbered */
/*              according to their ordinal position in the logical */
/*              records, define a transformation matrix R as follows: */

/*                 R = [ ANGLE_3 ]  [ ANGLE_2 ]  [ ANGLE_1 ] */
/*                                3            1            3 */

/*              Here the notation */

/*                 [ THETA ] */
/*                          i */

/*              denotes a reference frame rotation of THETA radians */
/*              in the right-hand sense about the ith coordinate */
/*              axis. See the Rotation Required Reading for further */
/*              discussion of this notation. */

/*              The matrix R transforms vectors expressed in the base */
/*              frame to vectors expressed in the PCK frame associated */
/*              with RECORD by left multiplication: */

/*                 V    = R * V */
/*                  PCK        FRAME */

/*              In cases where the PCK frame is a body-fixed, */
/*              right-handed frame with its +Z axis aligned with a */
/*              body's north pole, the orientation angles are related */
/*              to right ascension (RA) and declination (DEC) of the */
/*              PCK frame's north pole, and prime meridian */
/*              orientation (W), by the equations */

/*                 ANGLE_1 = RA   + pi/2 radians */
/*                 ANGLE_2 = pi/2 - DEC  radians */
/*                 ANGLE_3 = W           radians */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the input record contains an invalid coefficient count, an */
/*         error is signaled by a routine in the call tree of this */
/*         routine. */

/*     2)  If the input record contains invalid domain transformation */
/*         parameters, an error is signaled by a routine in the */
/*         call tree of this routine. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The exact format and structure of type 20 (Chebyshev polynomials, */
/*     position only) segments are described in the PCK Required Reading */
/*     file. */

/*     A type 20 segment contains three sets of Chebyshev coefficients, */
/*     one set each for the derivatives with respect to time of the */
/*     Euler angles phi, delta and psi. PCKE20 calls the routine SPKE20 */
/*     for each set to evaluate the polynomial and its first derivative. */

/* $ Examples */

/*     The PCKEnn routines are almost always used in conjunction with */
/*        the corresponding PCKRnn routines, which read the records from */
/*        binary PCK files. */

/*        The data returned by the PCKRnn routine are in their rawest */
/*        form, taken directly from the segment. As such, they will be */
/*        meaningless to a user unless he/she understands the structure */
/*        of the data type completely. Given that understanding, however, */
/*        the PCKRnn routines might be used to examine raw segment data */
/*        before evaluating it with the PCKEnn routines. */


/*        Here we load a binary PCK files and use PCKE20 to get the */
/*        Euler angles. */

/*     C */
/*     C     Load binary PCK file. */
/*     C */
/*           CALL PCKLOF ('example.pck', HANDLE) */

/*     C */
/*     C     Get a segment applicable to a specified body and epoch. */
/*     C */
/*           CALL PCKSFS ( BODY, ET, HANDLE, DESCR, IDENT, FOUND ) */

/*           IF ( FOUND ) THEN */
/*     C */
/*     C        Look at parts of the descriptor. */
/*     C */
/*              CALL DAFUS ( DESCR, ND, NI, DCD, ICD ) */
/*              TYPE   = ICD( NT ) */
/*              REF    = ICD( NR ) */

/*              IF ( TYPE .EQ. 20 ) THEN */
/*     C */
/*     C           Read in Chebyshev coefficients from segment. */
/*     C */
/*                 CALL PCKR20 ( HANDLE, DESCR, ET, RECORD ) */
/*     C */
/*     C           Call evaluation routine to get Euler angles */
/*     C           phi, delta, w. */
/*     C */
/*                 CALL PCKE20 ( ET, RECORD, EULANG ) */


/*        The Euler angles and their derivatives are returned */
/*        in EULANG. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     K.S. Zukor         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.0, 17-JAN-2014 (NJB) (KSZ) */

/* -& */
/* $ Index_Entries */

/*     evaluate pck_type_20 record */

/* -& */

/*     SPICELIB functions */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("PCKE20", (ftnlen)6);

/*     Call evaluation routine to get Euler angles */
/*     phi, delta, w. */

    spke20_(et, record, eulang);

/*     Map the third angle into the range (-2pi, 2pi). */

    d__1 = twopi_();
    eulang[2] = d_mod(&eulang[2], &d__1);
    chkout_("PCKE20", (ftnlen)6);
    return 0;
} /* pcke20_ */

