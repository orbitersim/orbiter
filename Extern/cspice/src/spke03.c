/* spke03.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure SPKE03 ( S/P Kernel, evaluate, type 3 ) */
/* Subroutine */ int spke03_(doublereal *et, doublereal *record, doublereal *
	state)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer degp, ncof, i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen), errdp_(char *, 
	    doublereal *, ftnlen), chbval_(doublereal *, integer *, 
	    doublereal *, doublereal *, doublereal *);
    integer cofloc;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Evaluate a single SPK data record from a segment of type 3 */
/*     (Chebyshev Polynomials, position and velocity). */

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

/*     EPHEMERIS */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     ET         I   Evaluation epoch. */
/*     RECORD     I   Data record. */
/*     STATE      O   State (position and velocity). */

/* $ Detailed_Input */

/*     ET       is the epoch at which a state vector or Euler angle */
/*              state is to be computed. The epoch is represented as */
/*              seconds past J2000 TDB. */

/*     RECORD   is a data record which, when evaluated at epoch ET, */
/*              will yield three function components and their */
/*              derivatives with respect to time. The record */
/*              structure for SPK type 3 data is: */

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
/*                 | Coeffs for X velocity component      | */
/*                 +--------------------------------------+ */
/*                 | Coeffs for Y velocity component      | */
/*                 +--------------------------------------+ */
/*                 | Coeffs for Z velocity component      | */
/*                 +--------------------------------------+ */

/*              In the above record */

/*                 - Times are expressed as seconds past J2000 TDB. */
/*                 - Position components have units of km. */
/*                 - Velocity components have units of km/s. */

/*              RECORD must be declared by the caller with size large */
/*              enough to accommodate the largest record that can be */
/*              returned by this routine. See the INCLUDE file */
/*              spkrec.inc for the correct record length. */

/* $ Detailed_Output */

/*     STATE    is the state. In order, X, Y, Z, X', Y', and Z'. */
/*              Units are km and km/sec. */

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

/*     The exact format and structure of type 3 (Chebyshev polynomials, */
/*     position and velocity) segments are described in the SPK */
/*     Required Reading file. */

/*     A type 3 segment contains six sets of Chebyshev coefficients, */
/*     one set each for the position coordinates X, Y, and Z, and one */
/*     set each for the velocity coordinates X', Y', and Z'.  SPKE03 */
/*     calls the routine CHBVAL to evaluate each polynomial, and arrive */
/*     at the complete state. */

/* $ Examples */

/*     The SPKEnn routines are almost always used in conjunction with */
/*     the corresponding SPKRnn routines, which read the records from */
/*     SPK files. */

/*     The data returned by the SPKRnn routine is in its rawest form, */
/*     taken directly from the segment. As such, it will be meaningless */
/*     to a user unless he/she understands the structure of the data type */
/*     completely. Given that understanding, however, the SPKRnn */
/*     routines might be used to examine raw segment data before */
/*     evaluating it with the SPKEnn routines. */


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

/*           IF ( TYPE .EQ. 3 ) THEN */

/*              CALL SPKR03 ( HANDLE, DESCR, ET, RECORD ) */
/*                  . */
/*                  .  Look at the RECORD data. */
/*                  . */
/*              CALL SPKE03 ( ET, RECORD, STATE ) */
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

/* $ Version */

/* -    SPICELIB Version 2.1.0, 14-APR-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Moved SPK */
/*        required reading from $Literature_References to */
/*        $Required_Reading section. */

/* -    SPICELIB Version 2.0.0, 31-DEC-2013 (NJB) */

/*        Added error checks for invalid coefficient counts */
/*        and invalid interval radius. Changed error handling */
/*        style to "discovery." Enhanced header documentation. */

/* -    SPICELIB Version 1.0.3, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.2, 23-AUG-1991 (HAN) */

/*        SPK03 was removed from the $Required_Reading section of the */
/*        header. The information in the SPK03 Required Reading file */
/*        is now part of the SPK Required Reading file. */

/* -    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN) */

/*        Literature references added to the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (RET) */

/* -& */
/* $ Index_Entries */

/*     evaluate type_3 SPK segment */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Use discovery check-in. */

    if (return_()) {
	return 0;
    }

/*     The first number in the record is the record size.  Following it */
/*     are two numbers that will be used later, then the six sets of */
/*     coefficients.  The number of coefficients for each quantity can */
/*     be determined from the record size, since there are the same */
/*     number of coefficients for each quantity. */

    ncof = ((integer) record[0] - 2) / 6;
    if (ncof < 1) {
	chkin_("SPKE03", (ftnlen)6);
	setmsg_("The input record's coefficient count NCOF should be positiv"
		"e but was #.", (ftnlen)71);
	errint_("#", &ncof, (ftnlen)1);
	sigerr_("SPICE(INVALIDCOUNT)", (ftnlen)19);
	chkout_("SPKE03", (ftnlen)6);
	return 0;
    }

/*     Check the radius of the domain interval. */

    if (record[2] <= 0.) {
	chkin_("SPKE03", (ftnlen)6);
	setmsg_("Interval radius must be positive but was #.", (ftnlen)43);
	errdp_("#", &record[2], (ftnlen)1);
	sigerr_("SPICE(INVALIDRADIUS)", (ftnlen)20);
	chkout_("SPKE03", (ftnlen)6);
	return 0;
    }

/*     The degree of each polynomial is one less than the number of */
/*     coefficients. */

    degp = ncof - 1;

/*     Call CHBVAL once for each quantity to evaluate the position */
/*     and velocity values. */

    for (i__ = 1; i__ <= 6; ++i__) {

/*        The coefficients for each variable are located contiguously, */
/*        following the first three words in the record. */

	cofloc = ncof * (i__ - 1) + 4;

/*        CHBVAL needs as input the coefficients, the degree of the */
/*        polynomial, the epoch, and also two variable transformation */
/*        parameters, which are located, in our case, in the second and */
/*        third slots of the record. */

	chbval_(&record[cofloc - 1], &degp, &record[1], et, &state[(i__1 = 
		i__ - 1) < 6 && 0 <= i__1 ? i__1 : s_rnge("state", i__1, 
		"spke03_", (ftnlen)312)]);
    }
    return 0;
} /* spke03_ */

