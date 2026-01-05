/* spke02.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure SPKE02 ( SPK, evaluate record, type 2 ) */
/* Subroutine */ int spke02_(doublereal *et, doublereal *record, doublereal *
	xyzdot)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer degp, ncof, i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen), errdp_(char *, 
	    doublereal *, ftnlen);
    integer cofloc;
    extern /* Subroutine */ int chbint_(doublereal *, integer *, doublereal *,
	     doublereal *, doublereal *, doublereal *), sigerr_(char *, 
	    ftnlen), chkout_(char *, ftnlen), setmsg_(char *, ftnlen), 
	    errint_(char *, integer *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Evaluate a single data record from an PCK or SPK segment of type */
/*     2 (Chebyshev Polynomials, 3 components). */

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
/*     PCK */

/* $ Keywords */

/*     EPHEMERIS */

/* $ Declarations */


/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     ET         I   Evaluation epoch. */
/*     RECORD     I   Data record. */
/*     XYZDOT     O   Three function components and their derivatives. */

/* $ Detailed_Input */

/*     ET       is the epoch at which a state vector or Euler angle */
/*              state is to be computed. The epoch is represented as */
/*              seconds past J2000 TDB. */

/*     RECORD   is a data record which, when evaluated at epoch ET, */
/*              will yield three function components and their */
/*              derivatives with respect to time. The record */
/*              structure for SPK type 2 data is: */

/*                 +--------------------------------------+ */
/*                 | record size (excluding this element) | */
/*                 +--------------------------------------+ */
/*                 | Coverage interval midpoint           | */
/*                 +--------------------------------------+ */
/*                 | Coverage interval radius             | */
/*                 +--------------------------------------+ */
/*                 | Coeffs for X position component      | */
/*                 +--------------------------------------+ */
/*                 | Coeffs for Y position component      | */
/*                 +--------------------------------------+ */
/*                 | Coeffs for Z position component      | */
/*                 +--------------------------------------+ */

/*              In the above record */

/*                 - Times are expressed as seconds past J2000 TDB. */
/*                 - Position components have units of km. */

/*              See PCKE02 for a description of PCK type 2 records. */

/*              RECORD must be declared by the caller with size large */
/*              enough to accommodate the largest record that can be */
/*              returned by this routine. See the INCLUDE file */
/*              spkrec.inc for the correct record length. */

/* $ Detailed_Output */

/*     XYZDOT   is a 6-vector. In order, the components of XYZDOT are */
/*              X, Y, Z, X', Y', and Z'. Units for state evaluations */
/*              will be km and km/sec. Units for angles will be */
/*              radians and radians/sec. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the input record contains an invalid coefficient count, */
/*         the error SPICE(INVALIDCOUNT) is signaled. */

/*     2)  If the input record contains invalid domain transformation */
/*         parameters, an error is signaled by a routine in the */
/*         call tree of this routine. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The exact format and structure of type 2 (Chebyshev polynomials, */
/*     position only) segments are described in the SPK and PCK Required */
/*     Reading files. */

/*     A type 2 segment contains three sets of Chebyshev coefficients, */
/*     one set each for components X, Y, and Z. SPKE02 calls the routine */
/*     CHBINT for each set to evaluate the polynomial AND its first */
/*     derivative (which it computes internally) at the input epoch, */
/*     thereby arriving at the complete state. */

/* $ Examples */

/*     The data returned by the routine is in its rawest form, */
/*     taken directly from the segment. As such, it will be meaningless */
/*     to a user unless he/she understands the structure of the data type */
/*     completely. */


/*     C */
/*     C     Get a segment applicable to a specified body and epoch. */
/*     C */
/*           CALL SPKSFS ( BODY, ET, HANDLE, DESCR, IDENT, FOUND ) */

/*     C */
/*     C     Look at parts of the descriptor. */
/*     C */
/*           CALL DAFUS ( DESCR, 2, 6, DCD, ICD ) */
/*           CENTER = ICD( 2 ) */
/*           REF    = ICD( 3 ) */
/*           TYPE   = ICD( 4 ) */

/*           IF ( TYPE .EQ. 2 ) THEN */

/*              CALL SPKR02 ( HANDLE, DESCR, ET, RECORD ) */
/*                  . */
/*                  .  Look at the RECORD data. */
/*                  . */
/*              CALL SPKE02 ( ET, RECORD, XYZDOT ) */
/*                  . */
/*                  .  Check out the evaluated state. */
/*                  . */
/*           END IF */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     H.A. Neilan        (JPL) */
/*     W.L. Taber         (JPL) */
/*     R.E. Thurman       (JPL) */
/*     K.S. Zukor         (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.1, 14-APR-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. Moved SPK */
/*        required reading from $Literature_References to */
/*        $Required_Reading section. */

/* -    SPICELIB Version 2.0.0, 18-JAN-2014 (NJB) */

/*        Added error checks for invalid coefficient counts */
/*        and invalid interval radius. Changed error handling */
/*        style to "discovery." Enhanced header documentation. */

/* -    SPICELIB Version 1.0.4, 22-MAR-1994 (KSZ) */

/*     Comments changed so this can be used as */
/*     a generic Chebyshev evaluator, rather than just for */
/*     SPK type 2 files.  (KSZ) */

/* -    SPICELIB Version 1.0.3, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.2, 23-AUG-1991 (HAN) */

/*        SPK02 was removed from the $Required_Reading section of the */
/*        header. The information in the SPK02 Required Reading file */
/*        is now part of the SPK Required Reading file. */

/* -    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN) */

/*        Literature references added to the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (RET) */

/* -& */
/* $ Index_Entries */

/*     evaluate type_2 SPK segment */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Use discovery check-in. */

    if (return_()) {
	return 0;
    }

/*     The first number in the record is the record size.  Following it */
/*     are two numbers that will be used later, then the three sets of */
/*     coefficients.  The number of coefficients for each variable can */
/*     be determined from the record size, since there are the same */
/*     number of coefficients for each variable. */

    ncof = ((integer) record[0] - 2) / 3;
    if (ncof < 1) {
	chkin_("SPKE02", (ftnlen)6);
	setmsg_("The input record's coefficient count NCOF should be positiv"
		"e but was #.", (ftnlen)71);
	errint_("#", &ncof, (ftnlen)1);
	sigerr_("SPICE(INVALIDCOUNT)", (ftnlen)19);
	chkout_("SPKE02", (ftnlen)6);
	return 0;
    }

/*     Check the radius of the domain interval. */

    if (record[2] <= 0.) {
	chkin_("SPKE02", (ftnlen)6);
	setmsg_("Interval radius must be positive but was #.", (ftnlen)43);
	errdp_("#", &record[2], (ftnlen)1);
	sigerr_("SPICE(INVALIDRADIUS)", (ftnlen)20);
	chkout_("SPKE02", (ftnlen)6);
	return 0;
    }

/*     The degree of each polynomial is one less than the number of */
/*     coefficients. */

    degp = ncof - 1;

/*     Call CHBINT once for each variable to evaluate the position */
/*     and velocity values. */

    for (i__ = 1; i__ <= 3; ++i__) {

/*        The coefficients for each variable are located contiguously, */
/*        following the first three words in the record. */

	cofloc = ncof * (i__ - 1) + 4;

/*        CHBINT needs as input the coefficients, the degree of the */
/*        polynomial, the epoch, and also two variable transformation */
/*        parameters, which are located, in our case, in the second and */
/*        third slots of the record. */

/*        Note that CHBINT is "error free." */

	chbint_(&record[cofloc - 1], &degp, &record[1], et, &xyzdot[(i__1 = 
		i__ - 1) < 6 && 0 <= i__1 ? i__1 : s_rnge("xyzdot", i__1, 
		"spke02_", (ftnlen)308)], &xyzdot[(i__2 = i__ + 2) < 6 && 0 <=
		 i__2 ? i__2 : s_rnge("xyzdot", i__2, "spke02_", (ftnlen)308)]
		);
    }
    return 0;
} /* spke02_ */

