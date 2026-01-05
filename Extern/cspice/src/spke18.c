/* spke18.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure SPKE18 ( S/P Kernel, evaluate, type 18 ) */
/* Subroutine */ int spke18_(doublereal *et, doublereal *record, doublereal *
	state)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer i_dnnt(doublereal *), s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer from;
    extern /* Subroutine */ int vequ_(doublereal *, doublereal *);
    doublereal work[792]	/* was [396][2] */;
    integer i__, j, n;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    doublereal vbuff[6];
    integer to;
    doublereal locrec[198];
    integer packsz;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    extern doublereal lgrint_(integer *, doublereal *, doublereal *, 
	    doublereal *, doublereal *);
    extern /* Subroutine */ int hrmint_(integer *, doublereal *, doublereal *,
	     doublereal *, doublereal *, doublereal *, doublereal *), setmsg_(
	    char *, ftnlen), errint_(char *, integer *, ftnlen), xpsgip_(
	    integer *, integer *, doublereal *);
    extern logical return_(void);
    integer xstart, subtyp, ystart;

/* $ Abstract */

/*     Evaluate a single data record from a type 18 SPK segment. */

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
/* $ Abstract */

/*     Declare parameters specific to SPK type 18. */

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

/* -    SPICELIB Version 1.0.0, 18-AUG-2002 (NJB) */

/* -& */

/*     SPK type 18 subtype codes: */


/*     Subtype 0:  Hermite interpolation, 12-element packets, order */
/*                 reduction at boundaries to preceding number */
/*                 equivalent to 3 mod 4. */


/*     Subtype 1:  Lagrange interpolation, 6-element packets, order */
/*                 reduction at boundaries to preceding odd number. */


/*     Packet sizes associated with the various subtypes: */


/*     End of include file spk18.inc. */

/* $ Abstract */

/*     Declare SPK data record size.  This record is declared in */
/*     SPKPVN and is passed to SPK reader (SPKRxx) and evaluator */
/*     (SPKExx) routines. */

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

/*     1) If new SPK types are added, it may be necessary to */
/*        increase the size of this record.  The header of SPKPVN */
/*        should be updated as well to show the record size */
/*        requirement for each data type. */

/* $ Author_and_Institution */

/*     N.J. Bachman      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 2.0.0, 05-OCT-2012 (NJB) */

/*        Updated to support increase of maximum degree to 27 for types */
/*        2, 3, 8, 9, 12, 13, 18, and 19. See SPKPVN for a list */
/*        of record size requirements as a function of data type. */

/* -    SPICELIB Version 1.0.0, 16-AUG-2002 (NJB) */

/* -& */

/*     End include file spkrec.inc */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     MAXREC     P   Maximum size of SPK record. */
/*     ET         I   Epoch for which a state is desired. */
/*     RECORD     I   Record from a type 18 SPK segment valid for ET. */
/*     STATE      O   State (position and velocity) at epoch ET. */

/* $ Detailed_Input */

/*     ET       is the epoch for which a state vector is desired. */

/*     RECORD   is a record from a type 18 SPK segment which, when */
/*              evaluated at epoch ET, will give the state */
/*              (position and velocity) of some body, relative to */
/*              some center, in some inertial reference frame. */

/*              The structure of the record is as follows: */

/*                 +----------------------+ */
/*                 | subtype code         | */
/*                 +----------------------+ */
/*                 | number of packets (n)| */
/*                 +----------------------+ */
/*                 | packet 1             | */
/*                 +----------------------+ */
/*                 | packet 2             | */
/*                 +----------------------+ */
/*                          . */
/*                          . */
/*                          . */
/*                 +----------------------+ */
/*                 | packet n             | */
/*                 +----------------------+ */
/*                 | epochs 1--n          | */
/*                 +----------------------+ */

/* $ Detailed_Output */

/*     STATE    is the state vector at epoch ET. Its contents are, in */
/*              order, X, Y, Z, X', Y', and Z'. Units are km and km/sec. */

/* $ Parameters */

/*     MAXREC   is the maximum size of SPK record. See the SPICELIB */
/*              routine SPKPVN for details. */

/* $ Exceptions */

/*     1)  This routine assumes that the input record is valid. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The exact format and structure of type 18 (MEX/Rosetta Orbit */
/*     file interpolation) SPK segments is described in the SPK */
/*     Required Reading. */

/* $ Examples */

/*     The SPKEnn routines are almost always used in conjunction with */
/*     the corresponding SPKRnn routines, which read the records from */
/*     SPK files. */

/*     The data returned by the SPKRnn routine is in a raw form, taken */
/*     directly from the segment. As such, it will be not be directly */
/*     useful to a user unless they have a complete understanding of the */
/*     structure of the data type. Given that understanding, however, */
/*     the SPKRnn routines could be used to "dump" and check segment data */
/*     for a particular epoch before evaluating the record to obtain a */
/*     state vector, as in the example which follows. */


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

/*           IF ( TYPE .EQ. 18 ) THEN */

/*              CALL SPKR18 ( HANDLE, DESCR, ET, RECORD ) */
/*                  . */
/*                  .  Look at the RECORD data. */
/*                  . */
/*              CALL SPKE18 ( ET, RECORD, STATE ) */
/*                  . */
/*                  .  Check out the evaluated state. */
/*                  . */
/*           END IF */

/* $ Restrictions */

/*     1)  This routine assumes that the input record is valid. Any */
/*         checking of the input data is assumed to have been performed */
/*         when the source SPK file was created. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */

/* $ Version */

/* -    SPICELIB Version 1.1.1, 16-AUG-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. Removed */
/*        unnecessary $Revisions section. */

/* -    SPICELIB Version 1.1.0, 05-NOV-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in XPOSEG and LGRINT calls. */

/* -    SPICELIB Version 1.0.0, 17-AUG-2002 (NJB) */

/* -& */
/* $ Index_Entries */

/*     evaluate type_18 SPK segment */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Index of subtype code in record: */


/*     Index of packet count in record: */


/*     Index at which packets start: */


/*     Maximum polynomial degree: */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("SPKE18", (ftnlen)6);

/*     Capture the subtype from the record and set the packet size */
/*     accordingly. */

    subtyp = i_dnnt(record);
    if (subtyp == 0) {
	packsz = 12;
    } else if (subtyp == 1) {
	packsz = 6;
    } else {
	setmsg_("Unexpected SPK type 18 subtype found in type 18 record.", (
		ftnlen)55);
	errint_("#", &subtyp, (ftnlen)1);
	sigerr_("SPICE(INVALIDVALUE)", (ftnlen)19);
	chkout_("SPKE18", (ftnlen)6);
	return 0;
    }

/*     Get the packet count. */

    n = i_dnnt(&record[1]);
    if (subtyp == 1) {

/*        This is the easy case:  we perform Lagrange interpolation */
/*        on each state component. */

/*        We'll transpose the state information in the input record so */
/*        that contiguous pieces of it can be shoved directly into the */
/*        interpolation routine LGRINT. */

	n = i_dnnt(&record[1]);
	xpsgip_(&packsz, &n, &record[2]);

/*        We interpolate each state component in turn. */

	xstart = n * packsz + 3;
	i__1 = packsz;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    ystart = n * (i__ - 1) + 3;
	    state[(i__2 = i__ - 1) < 6 && 0 <= i__2 ? i__2 : s_rnge("state", 
		    i__2, "spke18_", (ftnlen)308)] = lgrint_(&n, &record[
		    xstart - 1], &record[ystart - 1], locrec, et);
	}
    } else {

/*        We interpolate each state component in turn.  Position and */
/*        velocity are interpolated separately. */

	xstart = packsz * n + 3;
	for (i__ = 1; i__ <= 3; ++i__) {
	    i__1 = n;
	    for (j = 1; j <= i__1; ++j) {

/*              For the Jth input packet, copy the Ith position and */
/*              velocity components into the local record buffer LOCREC. */

		from = packsz * (j - 1) + 2 + i__;
		to = (j << 1) - 1;
		locrec[(i__2 = to - 1) < 198 && 0 <= i__2 ? i__2 : s_rnge(
			"locrec", i__2, "spke18_", (ftnlen)333)] = record[
			from - 1];
		locrec[(i__2 = to) < 198 && 0 <= i__2 ? i__2 : s_rnge("locrec"
			, i__2, "spke18_", (ftnlen)334)] = record[from + 2];
	    }

/*           Interpolate the Ith position and velocity components of the */
/*           state.  We'll keep the position and overwrite the velocity. */

	    hrmint_(&n, &record[xstart - 1], locrec, et, work, &state[(i__1 = 
		    i__ - 1) < 6 && 0 <= i__1 ? i__1 : s_rnge("state", i__1, 
		    "spke18_", (ftnlen)342)], &state[(i__2 = i__ + 2) < 6 && 
		    0 <= i__2 ? i__2 : s_rnge("state", i__2, "spke18_", (
		    ftnlen)342)]);
	}

/*        Now interpolate velocity, using separate velocity data and */
/*        acceleration. */

	for (i__ = 1; i__ <= 3; ++i__) {
	    i__1 = n;
	    for (j = 1; j <= i__1; ++j) {

/*              For the Jth input packet, copy the Ith position and */
/*              velocity components into the local record buffer LOCREC. */

		from = packsz * (j - 1) + 2 + packsz / 2 + i__;
		to = (j << 1) - 1;
		locrec[(i__2 = to - 1) < 198 && 0 <= i__2 ? i__2 : s_rnge(
			"locrec", i__2, "spke18_", (ftnlen)366)] = record[
			from - 1];
		locrec[(i__2 = to) < 198 && 0 <= i__2 ? i__2 : s_rnge("locrec"
			, i__2, "spke18_", (ftnlen)367)] = record[from + 2];
	    }

/*           Interpolate the Ith velocity and acceleration components of */
/*           the state.  We'll capture the result in a temporary buffer, */
/*           then transfer the velocity to the output state array. */

	    hrmint_(&n, &record[xstart - 1], locrec, et, work, &vbuff[(i__1 = 
		    i__ - 1) < 6 && 0 <= i__1 ? i__1 : s_rnge("vbuff", i__1, 
		    "spke18_", (ftnlen)376)], &vbuff[(i__2 = i__ + 2) < 6 && 
		    0 <= i__2 ? i__2 : s_rnge("vbuff", i__2, "spke18_", (
		    ftnlen)376)]);
	}

/*        Fill in the velocity in the output state using the results of */
/*        interpolating velocity and acceleration. */

	vequ_(vbuff, &state[3]);
    }
    chkout_("SPKE18", (ftnlen)6);
    return 0;
} /* spke18_ */

