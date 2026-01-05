/* spke20.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure SPKE20 ( SPK, evaluate Chebyshev polynomials, type 20 ) */
/* Subroutine */ int spke20_(doublereal *et, doublereal *record, doublereal *
	xyzdot)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    extern /* Subroutine */ int vadd_(doublereal *, doublereal *, doublereal *
	    );
    integer degp, ncof, i__, j;
    extern /* Subroutine */ int chkin_(char *, ftnlen), chbigr_(integer *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *), sigerr_(char *, ftnlen), chkout_(char *, ftnlen);
    doublereal intgrl[3];
    integer posloc;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Evaluate a single data record from an SPK or PCK segment of type */
/*     20 (Chebyshev polynomials, velocity only). */

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
/*              structure for SPK type 20 data is: */

/*                 +--------------------------------------+ */
/*                 | record size (excluding this element) | */
/*                 +--------------------------------------+ */
/*                 | Coverage interval midpoint           | */
/*                 +--------------------------------------+ */
/*                 | Coverage interval radius             | */
/*                 +--------------------------------------+ */
/*                 | Coeffs for X velocity component      | */
/*                 +--------------------------------------+ */
/*                 | Coeffs for Y velocity component      | */
/*                 +--------------------------------------+ */
/*                 | Coeffs for Z velocity component      | */
/*                 +--------------------------------------+ */
/*                 | X position component                 | */
/*                 +--------------------------------------+ */
/*                 | Y position component                 | */
/*                 +--------------------------------------+ */
/*                 | Z position component                 | */
/*                 +--------------------------------------+ */

/*              In the above record */

/*                 - Times are expressed as seconds past J2000 TDB. */
/*                 - Position components have units of km. */
/*                 - Velocity coefficients have units of km/s. */

/*              See PCKE20 for a description of PCK type 20 records. */

/*              PCK type 20 records contain coefficients for Euler */
/*              angle rates and Euler angles corresponding to the */
/*              interval midpoint. See PCKE20 for a more detailed */
/*              description of the contents of PCK type 20 records. */

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

/*     This routine evaluates both type 20 SPK and PCK records. */

/*     The exact format and structure of type 20 (Chebyshev polynomials, */
/*     position only) segments are described in the SPK and PCK Required */
/*     Reading files. */

/*     A type 20 record contains three sets of Chebyshev coefficients--- */
/*     one set each for velocity components dX/dt, dY/dt, and dZ/dt. It */
/*     also contains a position vector (or for a PCK record, Euler */
/*     angles) associated with the midpoint of the record's coverage */
/*     interval. The position (or orientation) is obtained from the */
/*     indefinite integral of the velocity and the given vector. */

/* $ Examples */

/*     The data returned by the routine is in its rawest form, */
/*     taken directly from the segment. As such, it will be meaningless */
/*     to a user unless he/she understands the structure of the data type */
/*     completely. */

/*     The code fragment below demonstrates reading and evaluating */
/*     a type 20 SPK record. */


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

/*           IF ( TYPE .EQ. 20 ) THEN */

/*              CALL SPKR20 ( HANDLE, DESCR, ET, RECORD ) */
/*                  . */
/*                  .  Look at the RECORD data. */
/*                  . */
/*              CALL SPKE20 ( ET, RECORD, XYZDOT ) */
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
/*     R.E. Thurman       (JPL) */
/*     K.S. Zukor         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 14-APR-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Moved SPK */
/*        required reading from $Literature_References to */
/*        $Required_Reading section. */

/* -    SPICELIB Version 1.0.0, 17-JAN-2014 (NJB) (RET) (KSZ) */

/* -& */
/* $ Index_Entries */

/*     evaluate type_20 SPK segment */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("SPKE20", (ftnlen)6);

/*     The first number in the record is the record size. This is the */
/*     number of elements in the record, excluding the size itself. */
/*     Following it are the record's midpoint and radius, then the three */
/*     sets of coefficients. The record ends with three values which */
/*     represent either a position or orientation at the record coverage */
/*     interval's midpoint. The number of coefficients for each variable */
/*     can be determined from the record size, since there are the same */
/*     number of coefficients for each variable. */

/*     The number of items counted by RECORD(1), other than the */
/*     Chebyshev coefficients, is 5. */

    ncof = ((integer) record[0] - 5) / 3;
    if (ncof < 1) {
	setmsg_("The input record's coefficient count NCOF should be positiv"
		"e but was #.", (ftnlen)71);
	errint_("#", &ncof, (ftnlen)1);
	sigerr_("SPICE(INVALIDCOUNT)", (ftnlen)19);
	chkout_("SPKE20", (ftnlen)6);
	return 0;
    }

/*     The degree of each polynomial is one less than the number of */
/*     coefficients. */

    degp = ncof - 1;

/*     Pass the Chebyshev coefficient portion of the record into CHBIGR, */
/*     which will evaluate the expansion and its integral for each */
/*     component. The constants of integration are selected so that the */
/*     integrals are zero when the input time is the interval midpoint. */

    for (i__ = 1; i__ <= 3; ++i__) {
	j = (i__ - 1) * ncof + 4;
	chbigr_(&degp, &record[j - 1], &record[1], et, &xyzdot[(i__1 = i__ + 
		2) < 6 && 0 <= i__1 ? i__1 : s_rnge("xyzdot", i__1, "spke20_",
		 (ftnlen)283)], &intgrl[(i__2 = i__ - 1) < 3 && 0 <= i__2 ? 
		i__2 : s_rnge("intgrl", i__2, "spke20_", (ftnlen)283)]);
    }

/*     Add the position vector or Euler angles at the interval midpoint */
/*     to the integral. */

    posloc = ncof * 3 + 4;
    vadd_(&record[posloc - 1], intgrl, xyzdot);
    chkout_("SPKE20", (ftnlen)6);
    return 0;
} /* spke20_ */

