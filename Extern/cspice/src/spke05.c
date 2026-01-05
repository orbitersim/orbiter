/* spke05.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__12 = 12;
static integer c__6 = 6;

/* $Procedure SPKE05 ( Evaluate SPK record, type 5 ) */
/* Subroutine */ int spke05_(doublereal *et, doublereal *record, doublereal *
	state)
{
    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */
    double cos(doublereal), sin(doublereal);

    /* Local variables */
    extern /* Subroutine */ int vadd_(doublereal *, doublereal *, doublereal *
	    );
    doublereal dwdt;
    extern /* Subroutine */ int vequ_(doublereal *, doublereal *);
    doublereal w;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    doublereal denom;
    extern /* Subroutine */ int moved_(doublereal *, integer *, doublereal *),
	     vlcom_(doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *);
    doublereal vcomp[3], numer, s1[6], s2[6], t1, t2;
    extern /* Subroutine */ int prop2b_(doublereal *, doublereal *, 
	    doublereal *, doublereal *);
    doublereal gm;
    extern doublereal pi_(void);
    doublereal dargdt, pv[12]	/* was [6][2] */;
    extern /* Subroutine */ int vlcomg_(integer *, doublereal *, doublereal *,
	     doublereal *, doublereal *, doublereal *), chkout_(char *, 
	    ftnlen);
    extern logical return_(void);
    doublereal arg, vel[3];

/* $ Abstract */

/*     Evaluate a single SPK data record from a segment of type 5 */
/*     (two body propagation between discrete state vectors). */

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
/*                     .            state of the body at epoch 1. */
/*                     . */
/*                     . */
/*                  RECORD(6) */

/*                  RECORD(7) */
/*                     . */
/*                     .            state of the body at epoch 2. */
/*                     . */
/*                  RECORD(12) */
/*                  RECORD(13)      epoch 1 in seconds past 2000. */
/*                  RECORD(14)      epoch 2 in seconds past 2000. */
/*                  RECORD(15)      GM for the center of motion. */

/*              Epoch 1 and epoch 2 are the times in the segment that */
/*              bracket ET. If ET is less than the first time in the */
/*              segment then both epochs 1 and 2 are equal to the */
/*              first time. And if ET is greater than the last time */
/*              then, epochs 1 and 2 are set equal to this last time. */

/* $ Detailed_Output */

/*     STATE    is the state produced by evaluating RECORD at ET. */
/*              Units are km and km/sec. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If there is a problem propagating, subject to the laws of two */
/*         body motion, either of the states from RECORD to the requested */
/*         time ET, an error is signaled by a routine in the call tree of */
/*         this routine. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine interpolates a state from the two reference states */
/*     contained in RECORD. */

/*     It is assumed that this routine is used in conjunction with */
/*     the routine SPKR05 as shown here: */

/*        CALL SPKR05 ( HANDLE, DESCR, ET, RECORD         ) */
/*        CALL SPKE05 (                ET, RECORD, STATE  ) */

/*     Where it is known in advance that the HANDLE, DESCR pair points */
/*     to a type 05 data segment. */

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

/*           IF ( TYPE .EQ. 5 ) THEN */

/*              CALL SPKR05 ( HANDLE, DESCR, ET, RECORD ) */
/*                  . */
/*                  .  Look at the RECORD data. */
/*                  . */
/*              CALL SPKE05 ( ET, RECORD, STATE ) */
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
/*     K.R. Gehringer     (JPL) */
/*     J.M. Lynch         (JPL) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.3.0, 12-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.2.0, 31-AUG-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in VADD call. */

/* -    SPICELIB Version 1.1.0, 29-FEB-1996 (KRG) */

/*        The declaration for the SPICELIB function PI is now */
/*        preceded by an EXTERNAL statement declaring PI to be an */
/*        external function. This removes a conflict with any */
/*        compilers that have a PI intrinsic function. */

/* -    SPICELIB Version 1.0.0, 01-APR-1992 (JML) (WLT) (IMU) */

/* -& */
/* $ Index_Entries */

/*     evaluate type_5 SPK segment */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.2.0, 31-AUG-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in VADD call. */

/* -    SPICELIB Version 1.1.0, 29-FEB-1996 (KRG) */

/*        The declaration for the SPICELIB function PI is now */
/*        preceded by an EXTERNAL statement declaring PI to be an */
/*        external function. This removes a conflict with any */
/*        compilers that have a PI intrinsic function. */

/* -    SPICELIB Version 1.0.0, 01-APR-1992 (JML) (WLT) (IMU) */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("SPKE05", (ftnlen)6);
    }

/*     Unpack the record, for easier reading. */

    moved_(record, &c__12, pv);
    t1 = record[12];
    t2 = record[13];
    gm = record[14];

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


/*        ( (t1+t2)/2,  W( (t1+t2)/2 ) */

/*     The range of W is from 1 to 0.  And the derivative of W is */
/*     symmetric and zero at both t1 and t2. */


    if (t1 != t2) {
	d__1 = *et - t1;
	prop2b_(&gm, pv, &d__1, s1);
	d__1 = *et - t2;
	prop2b_(&gm, &pv[6], &d__1, s2);
	numer = *et - t1;
	denom = t2 - t1;
	arg = numer * pi_() / denom;
	dargdt = pi_() / denom;
	w = cos(arg) * .5 + .5;
	dwdt = sin(arg) * -.5 * dargdt;
	d__1 = 1. - w;
	vlcomg_(&c__6, &w, s1, &d__1, s2, state);
	d__1 = -dwdt;
	vlcom_(&dwdt, s1, &d__1, s2, vcomp);
	vadd_(&state[3], vcomp, vel);
	vequ_(vel, &state[3]);
    } else {
	d__1 = *et - t1;
	prop2b_(&gm, pv, &d__1, state);
    }
    chkout_("SPKE05", (ftnlen)6);
    return 0;
} /* spke05_ */

