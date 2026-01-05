/* spke17.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure SPKE17 ( Evaluate a type 17 SPK data record) */
/* Subroutine */ int spke17_(doublereal *et, doublereal *recin, doublereal *
	state)
{
    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    doublereal a, h__, k;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    doublereal epoch;
    extern /* Subroutine */ int errdp_(char *, doublereal *, ftnlen);
    doublereal decpol, rapole;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), eqncpv_(doublereal *,
	     doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *), chkout_(char *, ftnlen), setmsg_(char *, ftnlen);
    extern logical return_(void);
    doublereal ecc;

/* $ Abstract */

/*     Evaluate a single SPK data record from a segment of type 17 */
/*     (Equinoctial Elements). */

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
/*     ET         I   Target epoch. */
/*     RECIN      I   Data record. */
/*     STATE      O   State (position and velocity). */

/* $ Detailed_Input */

/*     ET       is a target epoch, specified as ephemeris seconds past */
/*              J2000, at which a state vector is to be computed. */

/*     RECIN    is a data record which, when evaluated at epoch ET, */
/*              will give the state (position and velocity) of some */
/*              body, relative to some center, in some inertial */
/*              reference frame. */

/*              The structure of RECIN is: */

/*                 RECIN (1)  epoch of the elements in ephemeris seconds */
/*                            past J2000. */

/*                 RECIN (2)-RECIN (10) Equinoctial Elements: */

/*                    RECIN (2)  is the semi-major axis (A) of the orbit. */

/*                    RECIN (3)  is the value of H at the specified */
/*                               epoch. ( E*SIN(ARGP+NODE) ). */

/*                    RECIN (4)  is the value of K at the specified epoch */
/*                               ( E*COS(ARGP+NODE) ). */

/*                    RECIN (5)  is the mean longitude (MEAN0+ARGP+NODE) */
/*                               at the epoch of the elements. */

/*                    RECIN (6)  is the value of P */
/*                               (TAN(INC/2)*SIN(NODE)) at the specified */
/*                               epoch. */

/*                    RECIN (7)  is the value of Q */
/*                               (TAN(INC/2)*COS(NODE)) at the specified */
/*                               epoch. */

/*                    RECIN (8)  is the rate of the longitude of periapse */
/*                               (dARGP/dt + dNODE/dt ) at the epoch of */
/*                               the elements. This rate is assumed to */
/*                               hold for all time. */

/*                    RECIN (9)  is the derivative of the mean longitude */
/*                               ( dM/dt + dARGP/dt + dNODE/dt ).  This */
/*                               rate is assumed to be constant. */

/*                    RECIN (10)  is the rate of the longitude of the */
/*                                ascending node ( dNODE/dt). */

/*                 RECIN (11) Right Ascension of the pole of the */
/*                            orbital reference system relative to the */
/*                            reference frame of the associated SPK */
/*                            segment. */

/*                 RECIN (12) Declination of the pole of the */
/*                            orbital reference system relative to */
/*                            the reference frame of the associated */
/*                            SPK segment. */

/* $ Detailed_Output */

/*     STATE    is the state produced by evaluating RECIN at ET. */
/*              Units are km and km/sec. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the eccentricity is greater than 0.9, the error */
/*         SPICE(BADECCENTRICITY) is signaled. */

/*     2)  If the semi-major axis is non-positive, the error */
/*         SPICE(BADSEMIAXIS) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine performs a cursory examination of the elements */
/*     of a type 17 SPK data record and then passes the equinoctial */
/*     elements contained in that record on to the SPICE routine */
/*     EQNCPV for evaluation. */

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

/*           IF ( TYPE .EQ. 17 ) THEN */

/*              CALL SPKR17 ( HANDLE, DESCR, ET, RECORD ) */
/*                  . */
/*                  .  Look at the RECORD data. */
/*                  . */
/*              CALL SPKE17 ( ET, RECORD, STATE ) */
/*                  . */
/*                  .  Check out the evaluated state. */
/*                  . */
/*           END IF */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 27-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.0, 08-JAN-1997 (WLT) */

/* -& */
/* $ Index_Entries */

/*     evaluate type_17 SPK segment */

/* -& */

/*     SPICELIB Functions */


/*     Local Variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("SPKE17", (ftnlen)6);

/*     Fetch the various entities from the input record, first the epoch. */

    epoch = recin[0];
    a = recin[1];
    h__ = recin[2];
    k = recin[3];
    ecc = sqrt(h__ * h__ + k * k);
    rapole = recin[10];
    decpol = recin[11];

/*     Check all the inputs here for obvious failures.  Yes, perhaps */
/*     this is overkill.  However, there is a lot more computation */
/*     going on in this routine so that the small amount of overhead */
/*     here should not be significant. */

    if (a <= 0.) {
	setmsg_("The semi-major axis supplied to the SPK type 17 evaluator w"
		"as non-positive.  This value must be positive. The value sup"
		"plied was #.", (ftnlen)131);
	errdp_("#", &a, (ftnlen)1);
	sigerr_("SPICE(BADSEMIAXIS)", (ftnlen)18);
	chkout_("SPKE17", (ftnlen)6);
	return 0;
    } else if (ecc > .9) {
	setmsg_("The eccentricity supplied for a type 17 segment is greater "
		"than 0.9.  It must be less than 0.9.The value supplied to th"
		"e type 17 evaluator was #. ", (ftnlen)146);
	errdp_("#", &ecc, (ftnlen)1);
	sigerr_("SPICE(BADECCENTRICITY)", (ftnlen)22);
	chkout_("SPKE17", (ftnlen)6);
	return 0;
    }

/*     That's all for here, just plug the elements into the routine */
/*     knows how to evaluate the equinoctial elements. */

    eqncpv_(et, &epoch, &recin[1], &rapole, &decpol, state);

/*     That's all folks.  Check out and return. */

    chkout_("SPKE17", (ftnlen)6);
    return 0;
} /* spke17_ */

