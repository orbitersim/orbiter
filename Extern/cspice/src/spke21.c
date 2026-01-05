/* spke21.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__25 = 25;
static integer c__1 = 1;

/* $Procedure SPKE21 ( S/P Kernel, evaluate, type 21 ) */
/* Subroutine */ int spke21_(doublereal *et, doublereal *record, doublereal *
	state)
{
    /* Initialized data */

    static doublereal fc[25] = { 1. };

    /* System generated locals */
    integer i__1, i__2, i__3, i__4, i__5, i__6;

    /* Builtin functions */
    integer i_dnnt(doublereal *), s_rnge(char *, integer, char *, integer);

    /* Local variables */
    static doublereal g[25];
    static integer i__, j;
    static doublereal w[27], delta;
    extern /* Subroutine */ int chkin_(char *, ftnlen), moved_(doublereal *, 
	    integer *, doublereal *);
    static integer kqmax1;
    static doublereal dt[75]	/* was [25][3] */, wc[24];
    static integer kq[3], ks;
    static doublereal tl;
    static integer jx;
    static doublereal tp;
    static integer maxdim;
    static doublereal refvel[3];
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    static doublereal refpos[3];
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen);
    extern logical return_(void);
    static integer mq2, ks1, kqq;
    static doublereal sum;

/* $ Abstract */

/*     Evaluate a single SPK data record from a segment of type 21 */
/*     (Extended Difference Lines). */

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
/*     TIME */

/* $ Keywords */

/*     EPHEMERIS */

/* $ Declarations */
/* $ Abstract */

/*     Declare parameters specific to SPK type 21. */

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

/*     SPK */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 25-DEC-2013 (NJB) */

/* -& */

/*     MAXTRM      is the maximum number of terms allowed in each */
/*                 component of the difference table contained in a type */
/*                 21 SPK difference line. MAXTRM replaces the fixed */
/*                 table parameter value of 15 used in SPK type 1 */
/*                 segments. */

/*                 Type 21 segments have variable size. Let MAXDIM be */
/*                 the dimension of each component of the difference */
/*                 table within each difference line. Then the size */
/*                 DLSIZE of the difference line is */

/*                    ( 4 * MAXDIM ) + 11 */

/*                 MAXTRM is the largest allowed value of MAXDIM. */



/*     End of include file spk21.inc. */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     ET         I   Evaluation epoch. */
/*     RECORD     I   Data record. */
/*     STATE      O   State (position and velocity). */
/*     MAXTRM     P   Maximum number of terms per difference table */
/*                    component. */

/* $ Detailed_Input */

/*     ET       is an epoch at which a state vector is to be */
/*              computed. The epoch is represented as seconds past */
/*              J2000 TDB. */

/*     RECORD   is a data record which, when evaluated at epoch ET, */
/*              will give the state (position and velocity) of an */
/*              ephemeris object, relative to its center of motion, */
/*              in an inertial reference frame. */

/*              The contents of RECORD are as follows: */

/*                 RECORD(1):         The difference table size per */
/*                                    Cartesian component. Call this */
/*                                    size MAXDIM; then the difference */
/*                                    line (MDA) size DLSIZE is */

/*                                      ( 4 * MAXDIM ) + 11 */

/*                 RECORD(2) */
/*                    ... */
/*                 RECORD(1+DLSIZE):  An extended difference line. */
/*                                    The contents are: */

/*                    Dimension  Description */
/*                    ---------  ---------------------------------- */
/*                    1          Reference epoch of difference line */
/*                    MAXDIM     Stepsize function vector */
/*                    1          Reference position vector,  x */
/*                    1          Reference velocity vector,  x */
/*                    1          Reference position vector,  y */
/*                    1          Reference velocity vector,  y */
/*                    1          Reference position vector,  z */
/*                    1          Reference velocity vector,  z */
/*                    MAXDIM,3   Modified divided difference */
/*                               arrays (MDAs) */
/*                    1          Maximum integration order plus 1 */
/*                    3          Integration order array */

/* $ Detailed_Output */

/*     STATE    is the state resulting from evaluation of the input */
/*              record at ET. Units are km and km/sec. */

/* $ Parameters */

/*     MAXTRM   is the maximum number of terms allowed in */
/*              each component of the difference table */
/*              contained in the input argument RECORD. */
/*              See the INCLUDE file spk21.inc for the value */
/*              of MAXTRM. */

/* $ Exceptions */

/*     1)  If the maximum table size of the input record exceeds */
/*         MAXTRM, the error SPICE(DIFFLINETOOLARGE) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The exact format and structure of type 21 (difference lines) */
/*     segments are described in the SPK Required Reading file. */

/*     SPKE21 is a modified version of SPKE01. The routine has been */
/*     generalized to support variable size difference lines. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     F.T. Krogh         (JPL) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 14-APR-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Moved SPK */
/*        required reading from $Literature_References to */
/*        $Required_Reading section. */

/* -    SPICELIB Version 1.0.0, 03-FEB-2014 (NJB) (FTK) (WLT) (IMU) */

/* -& */
/* $ Index_Entries */

/*     evaluate type_21 SPK segment */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */

/*     The names below are original to the routine. They correspond */
/*     roughly to the original memos written by Fred Krogh to explain */
/*     how all this stuff really works. */


/*     Save everything between calls. */


/*     Initial values */


/*     Use discovery check-in. */

/*     If the RETURN function is set, don't even bother with this. */

    if (return_()) {
	return 0;
    }

/*     The first element of the input record is the dimension */
/*     of the difference table MAXDIM. */

    maxdim = i_dnnt(record);
    if (maxdim > 25) {
	chkin_("SPKE21", (ftnlen)6);
	setmsg_("The input record has a maximum table dimension of #, while "
		"the maximum supported by this routine is #. It is possible t"
		"hat this problem is due to your SPICE Toolkit being out of d"
		"ate.", (ftnlen)183);
	errint_("#", &maxdim, (ftnlen)1);
	errint_("#", &c__25, (ftnlen)1);
	sigerr_("SPICE(DIFFLINETOOLARGE)", (ftnlen)23);
	chkout_("SPKE21", (ftnlen)6);
	return 0;
    }

/*     Unpack the contents of the MDA array. */

/*        Name     Dimension  Description */
/*        ------   ---------  ------------------------------- */
/*        TL               1  Reference epoch of record */
/*        G           MAXDIM  Stepsize function vector */
/*        REFPOS           3  Reference position vector */
/*        REFVEL           3  Reference velocity vector */
/*        DT      MAXDIM,NTE  Modified divided difference arrays */
/*        KQMAX1           1  Maximum integration order plus 1 */
/*        KQ             NTE  Integration order array */

/*     For our purposes, NTE is always 3. */

    moved_(&record[1], &c__1, &tl);
    moved_(&record[2], &maxdim, g);

/*     Collect the reference position and velocity. */

    refpos[0] = record[maxdim + 2];
    refvel[0] = record[maxdim + 3];
    refpos[1] = record[maxdim + 4];
    refvel[1] = record[maxdim + 5];
    refpos[2] = record[maxdim + 6];
    refvel[2] = record[maxdim + 7];

/*     Initializing the difference table is one aspect of this routine */
/*     that's a bit different from SPKE01. Here the first dimension of */
/*     the table in the input record can be smaller than MAXTRM. So, we */
/*     must transfer separately the portions of the table corresponding */
/*     to each component. */

    for (i__ = 1; i__ <= 3; ++i__) {
	moved_(&record[i__ * maxdim + 8], &maxdim, &dt[(i__1 = i__ * 25 - 25) 
		< 75 && 0 <= i__1 ? i__1 : s_rnge("dt", i__1, "spke21_", (
		ftnlen)299)]);
    }
    kqmax1 = (integer) record[(maxdim << 2) + 8];
    kq[0] = (integer) record[(maxdim << 2) + 9];
    kq[1] = (integer) record[(maxdim << 2) + 10];
    kq[2] = (integer) record[(maxdim << 2) + 11];

/*     Next we set up for the computation of the various differences */

    delta = *et - tl;
    tp = delta;
    mq2 = kqmax1 - 2;
    ks = kqmax1 - 1;

/*     This is clearly collecting some kind of coefficients. */
/*     The problem is that we have no idea what they are... */

/*     The G coefficients are supposed to be some kind of step size */
/*     vector. */

/*     TP starts out as the delta t between the request time and the */
/*     difference line's reference epoch. We then change it from DELTA */
/*     by the components of the stepsize vector G. */

    i__1 = mq2;
    for (j = 1; j <= i__1; ++j) {

/*        Make sure we're not about to attempt division by zero. */

	if (g[(i__2 = j - 1) < 25 && 0 <= i__2 ? i__2 : s_rnge("g", i__2, 
		"spke21_", (ftnlen)330)] == 0.) {
	    chkin_("SPKE21", (ftnlen)6);
	    setmsg_("A  value of zero was found at index # of the step size "
		    "vector.", (ftnlen)62);
	    errint_("#", &j, (ftnlen)1);
	    sigerr_("SPICE(ZEROSTEP)", (ftnlen)15);
	    chkout_("SPKE21", (ftnlen)6);
	    return 0;
	}
	fc[(i__2 = j) < 25 && 0 <= i__2 ? i__2 : s_rnge("fc", i__2, "spke21_",
		 (ftnlen)342)] = tp / g[(i__3 = j - 1) < 25 && 0 <= i__3 ? 
		i__3 : s_rnge("g", i__3, "spke21_", (ftnlen)342)];
	wc[(i__2 = j - 1) < 24 && 0 <= i__2 ? i__2 : s_rnge("wc", i__2, "spk"
		"e21_", (ftnlen)343)] = delta / g[(i__3 = j - 1) < 25 && 0 <= 
		i__3 ? i__3 : s_rnge("g", i__3, "spke21_", (ftnlen)343)];
	tp = delta + g[(i__2 = j - 1) < 25 && 0 <= i__2 ? i__2 : s_rnge("g", 
		i__2, "spke21_", (ftnlen)344)];
    }

/*     Collect KQMAX1 reciprocals. */

    i__1 = kqmax1;
    for (j = 1; j <= i__1; ++j) {
	w[(i__2 = j - 1) < 27 && 0 <= i__2 ? i__2 : s_rnge("w", i__2, "spke2"
		"1_", (ftnlen)352)] = 1. / (doublereal) j;
    }

/*     Compute the W(K) terms needed for the position interpolation */
/*     (Note,  it is assumed throughout this routine that KS, which */
/*     starts out as KQMAX1-1 (the ``maximum integration'') */
/*     is at least 2. */

    jx = 0;
    ks1 = ks - 1;
    while(ks >= 2) {
	++jx;
	i__1 = jx;
	for (j = 1; j <= i__1; ++j) {
	    w[(i__2 = j + ks - 1) < 27 && 0 <= i__2 ? i__2 : s_rnge("w", i__2,
		     "spke21_", (ftnlen)369)] = fc[(i__3 = j) < 25 && 0 <= 
		    i__3 ? i__3 : s_rnge("fc", i__3, "spke21_", (ftnlen)369)] 
		    * w[(i__4 = j + ks1 - 1) < 27 && 0 <= i__4 ? i__4 : 
		    s_rnge("w", i__4, "spke21_", (ftnlen)369)] - wc[(i__5 = j 
		    - 1) < 24 && 0 <= i__5 ? i__5 : s_rnge("wc", i__5, "spke"
		    "21_", (ftnlen)369)] * w[(i__6 = j + ks - 1) < 27 && 0 <= 
		    i__6 ? i__6 : s_rnge("w", i__6, "spke21_", (ftnlen)369)];
	}
	ks = ks1;
	--ks1;
    }

/*     Perform position interpolation: (Note that KS = 1 right now. */
/*     We don't know much more than that.) */

    for (i__ = 1; i__ <= 3; ++i__) {
	kqq = kq[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("kq", i__1,
		 "spke21_", (ftnlen)383)];
	sum = 0.;
	for (j = kqq; j >= 1; --j) {
	    sum += dt[(i__1 = j + i__ * 25 - 26) < 75 && 0 <= i__1 ? i__1 : 
		    s_rnge("dt", i__1, "spke21_", (ftnlen)387)] * w[(i__2 = j 
		    + ks - 1) < 27 && 0 <= i__2 ? i__2 : s_rnge("w", i__2, 
		    "spke21_", (ftnlen)387)];
	}
	state[(i__1 = i__ - 1) < 6 && 0 <= i__1 ? i__1 : s_rnge("state", i__1,
		 "spke21_", (ftnlen)390)] = refpos[(i__2 = i__ - 1) < 3 && 0 
		<= i__2 ? i__2 : s_rnge("refpos", i__2, "spke21_", (ftnlen)
		390)] + delta * (refvel[(i__3 = i__ - 1) < 3 && 0 <= i__3 ? 
		i__3 : s_rnge("refvel", i__3, "spke21_", (ftnlen)390)] + 
		delta * sum);
    }

/*     Again we need to compute the W(K) coefficients that are */
/*     going to be used in the velocity interpolation. */
/*     (Note, at this point, KS = 1, KS1 = 0.) */

    i__1 = jx;
    for (j = 1; j <= i__1; ++j) {
	w[(i__2 = j + ks - 1) < 27 && 0 <= i__2 ? i__2 : s_rnge("w", i__2, 
		"spke21_", (ftnlen)400)] = fc[(i__3 = j) < 25 && 0 <= i__3 ? 
		i__3 : s_rnge("fc", i__3, "spke21_", (ftnlen)400)] * w[(i__4 =
		 j + ks1 - 1) < 27 && 0 <= i__4 ? i__4 : s_rnge("w", i__4, 
		"spke21_", (ftnlen)400)] - wc[(i__5 = j - 1) < 24 && 0 <= 
		i__5 ? i__5 : s_rnge("wc", i__5, "spke21_", (ftnlen)400)] * w[
		(i__6 = j + ks - 1) < 27 && 0 <= i__6 ? i__6 : s_rnge("w", 
		i__6, "spke21_", (ftnlen)400)];
    }
    --ks;

/*     Perform velocity interpolation: */

    for (i__ = 1; i__ <= 3; ++i__) {
	kqq = kq[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("kq", i__1,
		 "spke21_", (ftnlen)410)];
	sum = 0.;
	for (j = kqq; j >= 1; --j) {
	    sum += dt[(i__1 = j + i__ * 25 - 26) < 75 && 0 <= i__1 ? i__1 : 
		    s_rnge("dt", i__1, "spke21_", (ftnlen)414)] * w[(i__2 = j 
		    + ks - 1) < 27 && 0 <= i__2 ? i__2 : s_rnge("w", i__2, 
		    "spke21_", (ftnlen)414)];
	}
	state[(i__1 = i__ + 2) < 6 && 0 <= i__1 ? i__1 : s_rnge("state", i__1,
		 "spke21_", (ftnlen)417)] = refvel[(i__2 = i__ - 1) < 3 && 0 
		<= i__2 ? i__2 : s_rnge("refvel", i__2, "spke21_", (ftnlen)
		417)] + delta * sum;
    }
    return 0;
} /* spke21_ */

