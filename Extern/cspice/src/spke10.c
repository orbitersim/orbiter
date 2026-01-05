/* spke10.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__6 = 6;

/* $Procedure SPKE10 ( Evaluate SPK record, type 10 ) */
/* Subroutine */ int spke10_(doublereal *et, doublereal *record, doublereal *
	state)
{
    /* Initialized data */

    static logical first = TRUE_;

    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */
    double cos(doublereal), sin(doublereal);

    /* Local variables */
    extern /* Subroutine */ int vadd_(doublereal *, doublereal *, doublereal *
	    );
    static doublereal dwdt, mypi;
    extern /* Subroutine */ int vequ_(doublereal *, doublereal *), mxvg_(
	    doublereal *, doublereal *, integer *, integer *, doublereal *);
    static doublereal w;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    static doublereal denom;
    extern /* Subroutine */ int moved_(doublereal *, integer *, doublereal *),
	     vlcom_(doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *);
    static doublereal vcomp[3], numer, s1[6], s2[6], t1, t2;
    extern logical failed_(void);
    extern doublereal pi_(void);
    static doublereal dargdt;
    extern /* Subroutine */ int vlcomg_(integer *, doublereal *, doublereal *,
	     doublereal *, doublereal *, doublereal *), chkout_(char *, 
	    ftnlen);
    static doublereal tmpsta[6];
    extern /* Subroutine */ int zzteme_(doublereal *, doublereal *, 
	    doublereal *);
    extern logical return_(void);
    static doublereal arg;
    extern /* Subroutine */ int xxsgp4e_(doublereal *, doublereal *), 
	    xxsgp4i_(doublereal *, doublereal *, integer *);
    static doublereal j2tm[36]	/* was [6][6] */, tm2j[36]	/* was [6][6] 
	    */;

/* $ Abstract */

/*     Evaluate a single SPK data record from a segment of type 10 */
/*     (NORAD two-line element sets.). This evaluator uses algorithms */
/*     as described in Vallado 2006 [4]. */

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
/* $Procedure ZZSGP4 ( SGP4 parameters ) */

/* $ Abstract */

/*      Parameter assignments for SGP4 algorithm as expressed */
/*      by Vallado [2]. */

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

/*     None. */

/* $ Declarations */

/*     None. */

/* $ Brief_I/O */

/*     None. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     J2    = GEOPHS(K_J2) */
/*     J3    = GEOPHS(K_J3) */
/*     J4    = GEOPHS(K_J4) */
/*     ER    = GEOPHS(K_ER) */
/*     XKE   = GEOPHS(K_KE) */

/*     TUMIN = 1.D0/XKE */
/*     J3OJ2 = J3/J2 */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*   [1] Hoots, F. R., and Roehrich, R. L. 1980. "Models for */
/*       Propagation of the NORAD Element Sets." Spacetrack Report #3. */
/*       U.S. Air Force: Aerospace Defense Command. */

/*   [2] Vallado, David, Crawford, Paul, Hujsak, Richard, and Kelso, T.S. */
/*       2006. Revisiting Spacetrack Report #3. Paper AIAA 2006-6753 */
/*       presented at the AIAA/AAS Astrodynamics Specialist Conference, */
/*       August 21-24, 2006. Keystone, CO. */

/* $ Author_and_Institution */

/*     E. D. Wright    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, MAY-27-2020 (EDW) */

/*        Updated descriptions of GEOPHS constants to be consistent */
/*        with what's used in other routines. */

/* -    SPICELIB Version 1.0.0 22-JUL-2014 (EDW) */

/* -& */
/* $ Index_Entries */

/*  SGP4 */

/* -& */

/*      WGS gravitational constants IDs. */


/*      Gravitational constant indices. */


/*     The following parameters give the indices in the GEOPHS */
/*     array of the various geophysical parameters needed for */
/*     the two line element sets. */

/*     K_J2  --- index of J2 gravitational harmonic for earth */
/*     K_J3  --- index of J3 gravitational harmonic for earth */
/*     K_J4  --- index of J4 gravitational harmonic for earth */
/*     K_KE  --- index of KE = sqrt(GM) in earth-radii**1.5/MIN */
/*     K_QO  --- index of high altitude bound for atmospheric */
/*               model in km */
/*     K_SO  --- index of low altitude bound for atmospheric */
/*               model in km */
/*     K_ER  --- index of earth equatorial radius in km */
/*     K_AE  --- index of distance units/earth radius */


/*     Operation mode values, OPMODE. */


/*     An enumeration of the various components of the */
/*     elements array---ELEMS */

/*     KNDT20  --- location of NDT20 */
/*     KNDD60  --- location of NDD60 */
/*     KBSTAR  --- location of BSTAR */
/*     KINCL   --- location of INCL */
/*     KNODE0  --- location of NODE0 */
/*     KECC    --- location of ECC */
/*     KOMEGA  --- location of OMEGA */
/*     KMO     --- location of MO */
/*     KNO     --- location of NO */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     ET         I   Target epoch. */
/*     RECORD     I   Data record. */
/*     STATE      O   State (position and velocity). */

/* $ Detailed_Input */

/*     ET       is a target epoch, specified as ephemeris seconds past */
/*              J2000, at which a state vector is to be computed. */

/*     RECORD   is a data record which, when evaluated at epoch ET, */
/*              will give the state (position and velocity) of some */
/*              body, relative to some center, in some inertial */
/*              reference frame. */

/*              The structure of RECORD is: */

/*                  RECORD(1) */
/*                     .            Geophysical Constants such as */
/*                     .            GM, J2, J3, J4, etc. */
/*                     . */
/*                  RECORD(NGEO) */

/*                  RECORD(NGEO + 1) */
/*                     . */
/*                     .            elements and epoch for the body */
/*                     .            at epoch 1. */
/*                     . */
/*                  RECORD(NGEO + NELEMS ) */

/*                  RECORD(NGEO + NELEMS + 1) */
/*                     . */
/*                     .            elements and epoch for the body */
/*                     .            at epoch 2. */
/*                     . */
/*                  RECORD(NGEO + 2*NELEMS ) */

/*              Epoch 1 and epoch 2 are the times in the segment that */
/*              bracket ET. If ET is less than the first time in the */
/*              segment then both epochs 1 and 2 are equal to the */
/*              first time. And if ET is greater than the last time */
/*              then, epochs 1 and 2 are set equal to this last time. */

/* $ Detailed_Output */

/*     STATE    is the state produced by evaluating RECORD at ET. */
/*              Units are km and km/sec relative to the J2000 */
/*              reference frame. */

/* $ Parameters */

/*     NGEO     is the number of geophysical constants for SGP4 SPK */
/*              records. */

/*     AFSPC    set the SGP4 propagator to use the original */
/*              Space Track #3 GST algorithm as described in Hoots [1]; */
/*              value defined in zzsgp4.inc. */

/*     IMPRVD   set the SGP4 propagator to use the improved GST */
/*              algorithm as defined in Vallado [4]; value defined in */
/*              zzsgp4.inc. */

/* $ Exceptions */

/*     1)  If a problem occurs when evaluating the two-line elements, an */
/*         error is signaled by a routine in the call tree of this */
/*         routine. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine interpolates a state from the two reference sets */
/*     of two-line element sets contained in RECORD. */

/*     It is assumed that this routine is used in conjunction with */
/*     the routine SPKR10 as shown here: */

/*        CALL SPKR10   ( HANDLE, DESCR, ET, RECORD         ) */
/*        CALL SPKE10   (                ET, RECORD, STATE  ) */

/*     Where it is known in advance that the HANDLE, DESCR pair points */
/*     to a type 10 data segment. */

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

/*           IF ( TYPE .EQ. 10 ) THEN */

/*              CALL SPKR10 ( HANDLE, DESCR, ET, RECORD ) */
/*                  . */
/*                  .  Look at the RECORD data. */
/*                  . */
/*              CALL SPKE10 ( ET, RECORD, STATE ) */
/*                  . */
/*                  .  Check out the evaluated state. */
/*                  . */
/*           END IF */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     [1]  F. Hoots and R. Roehrich, "Spacetrack Report #3: Models for */
/*          Propagation of the NORAD Element Sets," U.S. Air Force */
/*          Aerospace Defense Command, Colorado Springs, CO, 1980. */

/*     [2]  F. Hoots, "Spacetrack Report #6: Models for Propagation of */
/*          Space Command Element Sets,"  U.S. Air Force Aerospace */
/*          Defense Command, Colorado Springs, CO, 1986. */

/*     [3]  F. Hoots, P. Schumacher and R. Glover, "History of Analytical */
/*          Orbit Modeling in the U. S. Space Surveillance System," */
/*          Journal of Guidance, Control, and Dynamics. 27(2):174-185, */
/*          2004. */

/*     [4]  D. Vallado, P. Crawford, R. Hujsak and T. Kelso, "Revisiting */
/*          Spacetrack Report #3," paper AIAA 2006-6753 presented at the */
/*          AIAA/AAS Astrodynamics Specialist Conference, Keystone, CO., */
/*          August 21-24, 2006. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 3.1.0, 10-OCT-2021 (JDR) (EDW) */

/*        Use of modified ZZTEME to eliminate a matrix inversion. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 3.0.0, 18-FEB-2015 (EDW) */

/*        Evaluator now uses Vallado derived propagator as described */
/*        in Vallado 2006 [4]. */

/* -    SPICELIB Version 2.0.0, 01-JAN-2011 (EDW) */

/*        Correction of state transformation calculation. Algorithm */
/*        now computes state transformation as from TEME to J2000. */
/*        The previous version of this routine calculated TETE to */
/*        J2000. */

/* -    SPICELIB Version 1.1.0, 01-SEP-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in MTXV and VADD calls. */

/* -    SPICELIB Version 1.0.0, 18-JUL-1997 (WLT) */

/* -& */
/* $ Index_Entries */

/*     evaluate type_10 SPK segment */

/* -& */

/*     SPICELIB functions */


/*     The nutation in obliquity and longitude as well as their rates */
/*     follow the elements.  So we've got four angles/angle rates */
/*     following the elements */


/*     The locations of the epochs and the starts of the element */
/*     sets are given below. */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("SPKE10", (ftnlen)6);
    }
    if (first) {
	first = FALSE_;
	mypi = pi_();
    }

/*     Fetch the two epochs stored in the record. */

    t1 = record[17];
    t2 = record[31];

/*     Evaluate the two states. Call them s_1(t) and s_2(t). */
/*     Let the position and velocity components be: p_1, v_1, p_2, v_2. */

/*     The final position is a weighted average. */

/*     Let */

/*        W(t) =  0.5 + 0.5*COS( PI*(t-t1)/(t2-t1) ) */

/*     then */

/*        p  = W(t)*p_1(t) + (1 - W(t))*p_2(t) */
/*        v  = W(t)*v_1(t) + (1 - W(t))*v_2(t) + W'(t)*(p_1(t) - p_2(t)) */

/*     If t1 = t2, the state is just s(t1). */

/*     Note: there are a number of weighting schemes we could have */
/*     used.  This one has the nice property that */

/*     The graph of W is symmetric about the point */

/*        ( (t1+t2)/2,  W( (t1+t2)/2 ) ) */

/*     The range of W is from 1 to 0. The derivative of W is */
/*     symmetric and zero at both t1 and t2. */

    if (t1 != t2) {

/*        Initialize then propagate. */

/*        XXSGP4E returns on entry if XXSGP4I signals an error. */


/*        Evaluate first TLE. */

	xxsgp4i_(record, &record[8], &c__1);
	if (failed_()) {
	    chkout_("SPKE10", (ftnlen)6);
	    return 0;
	}

/*        Time from epoch of set 1 in minutes. */

	d__1 = (*et - t1) / 60.;
	xxsgp4e_(&d__1, s1);
	if (failed_()) {
	    chkout_("SPKE10", (ftnlen)6);
	    return 0;
	}

/*        Evaluate second TLE. */

	xxsgp4i_(record, &record[22], &c__1);
	if (failed_()) {
	    chkout_("SPKE10", (ftnlen)6);
	    return 0;
	}

/*        Time from epoch of set 2 in minutes. */

	d__1 = (*et - t2) / 60.;
	xxsgp4e_(&d__1, s2);
	if (failed_()) {
	    chkout_("SPKE10", (ftnlen)6);
	    return 0;
	}

/*        Compute the weighting function that we'll need later */
/*        when we combine states 1 and 2. */

	numer = *et - t1;
	denom = t2 - t1;
	arg = numer * mypi / denom;
	dargdt = mypi / denom;
	w = cos(arg) * .5 + .5;
	dwdt = sin(arg) * -.5 * dargdt;

/*        Now compute the weighted average of the two states. */

	d__1 = 1. - w;
	vlcomg_(&c__6, &w, s1, &d__1, s2, state);
	d__1 = -dwdt;
	vlcom_(&dwdt, s1, &d__1, s2, vcomp);
	vadd_(&state[3], vcomp, &tmpsta[3]);
	vequ_(&tmpsta[3], &state[3]);
    } else {

/*        Evaluate the TLE. */

	xxsgp4i_(record, &record[8], &c__1);
	if (failed_()) {
	    chkout_("SPKE10", (ftnlen)6);
	    return 0;
	}

/*        Time from epoch of set 1 in minutes. */

	d__1 = (*et - t1) / 60.;
	xxsgp4e_(&d__1, state);
	if (failed_()) {
	    chkout_("SPKE10", (ftnlen)6);
	    return 0;
	}
    }

/*     Finally, convert the TEME state to J2000.  First */
/*     calculate the mapping from J2000 to TEME (J2TM), and from */
/*     TEME to J2000 (TM2J) at time ET... */

/*                         -1 */
/*     Note that J2TM = TM2J */

    zzteme_(et, j2tm, tm2j);

/*     ...now convert the TEME state to a J2000 state. */

    mxvg_(tm2j, state, &c__6, &c__6, tmpsta);
    moved_(tmpsta, &c__6, state);
    chkout_("SPKE10", (ftnlen)6);
    return 0;
} /* spke10_ */

