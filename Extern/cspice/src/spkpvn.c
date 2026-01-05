/* spkpvn.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__6 = 6;
static integer c__1 = 1;
static integer c__198 = 198;

/* $Procedure SPKPVN ( S/P Kernel, position and velocity in native frame ) */
/* Subroutine */ int spkpvn_(integer *handle, doublereal *descr, doublereal *
	et, integer *ref, doublereal *state, integer *center)
{
    integer type__;
    extern /* Subroutine */ int chkin_(char *, ftnlen), dafus_(doublereal *, 
	    integer *, integer *, doublereal *, integer *), spke01_(
	    doublereal *, doublereal *, doublereal *), spke02_(doublereal *, 
	    doublereal *, doublereal *), spke03_(doublereal *, doublereal *, 
	    doublereal *), spke10_(doublereal *, doublereal *, doublereal *), 
	    spke05_(doublereal *, doublereal *, doublereal *), spke12_(
	    doublereal *, doublereal *, doublereal *), spke13_(doublereal *, 
	    doublereal *, doublereal *), spke08_(doublereal *, doublereal *, 
	    doublereal *), spke09_(doublereal *, doublereal *, doublereal *), 
	    spke14_(doublereal *, doublereal *, doublereal *), spke15_(
	    doublereal *, doublereal *, doublereal *), spke17_(doublereal *, 
	    doublereal *, doublereal *), spke18_(doublereal *, doublereal *, 
	    doublereal *), spkr01_(integer *, doublereal *, doublereal *, 
	    doublereal *), spkr02_(integer *, doublereal *, doublereal *, 
	    doublereal *), spkr03_(integer *, doublereal *, doublereal *, 
	    doublereal *), spkr05_(integer *, doublereal *, doublereal *, 
	    doublereal *), spkr10_(integer *, doublereal *, doublereal *, 
	    doublereal *), spkr12_(integer *, doublereal *, doublereal *, 
	    doublereal *), spkr08_(integer *, doublereal *, doublereal *, 
	    doublereal *), spkr09_(integer *, doublereal *, doublereal *, 
	    doublereal *), spkr13_(integer *, doublereal *, doublereal *, 
	    doublereal *), spkr14_(integer *, doublereal *, doublereal *, 
	    doublereal *), spkr15_(integer *, doublereal *, doublereal *, 
	    doublereal *), spkr17_(integer *, doublereal *, doublereal *, 
	    doublereal *), spkr18_(integer *, doublereal *, doublereal *, 
	    doublereal *), spkr19_(integer *, doublereal *, doublereal *, 
	    doublereal *), spke19_(doublereal *, doublereal *, doublereal *), 
	    spkr20_(integer *, doublereal *, doublereal *, doublereal *), 
	    spke20_(doublereal *, doublereal *, doublereal *), spkr21_(
	    integer *, doublereal *, doublereal *, doublereal *), spke21_(
	    doublereal *, doublereal *, doublereal *);
    doublereal dc[2];
    integer ic[6];
    extern logical failed_(void);
    doublereal record[198];
    extern /* Subroutine */ int sgfcon_(integer *, doublereal *, integer *, 
	    integer *, doublereal *), sigerr_(char *, ftnlen), chkout_(char *,
	     ftnlen);
    integer recsiz;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Return, for a specified SPK segment and time, the state (position */
/*     and velocity) of the segment's target body relative to its center */
/*     of motion. */

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
/*     HANDLE     I   File handle. */
/*     DESCR      I   Segment descriptor. */
/*     ET         I   Target epoch. */
/*     REF        O   Target reference frame. */
/*     STATE      O   Position, velocity. */
/*     CENTER     O   Center of state. */
/*     MAXREC     P   Maximum length of records returned by SPKRnn. */

/* $ Detailed_Input */

/*     HANDLE, */
/*     DESCR    are the file handle assigned to a SPK file, and the */
/*              descriptor for a segment within the file. Together */
/*              they determine the ephemeris data from which the */
/*              state of the body is to be computed. */

/*     ET       is the epoch (ephemeris time) at which the state */
/*              is to be computed. */

/* $ Detailed_Output */

/*     REF      is the id-code of the reference frame to */
/*              which the vectors returned by the routine belong. */

/*     STATE    is a 6-dimensional vector that contains the position and */
/*              velocity, at epoch ET, for whatever body is covered by */
/*              the specified segment. STATE has six elements: the first */
/*              three contain the body's position; the last three contain */
/*              the body's velocity. These vectors are rotated into the */
/*              specified  reference frame, the origin of which is */
/*              located at the center of motion for the body (see CENTER, */
/*              below). Units are always km and km/sec. */

/*     CENTER   is the integer ID code of the center of motion for */
/*              the state. */

/* $ Parameters */

/*     MAXREC   is the maximum length of a record returned by any of */
/*              data type-specific routines SPKRnn, which are called */
/*              by SPKPVN (see $Particulars). */

/* $ Exceptions */

/*     1)  If the segment type is not supported by the current */
/*         version of SPKPVN, the error SPICE(SPKTYPENOTSUPP) */
/*         is signaled. */

/* $ Files */

/*     See argument HANDLE. */

/* $ Particulars */

/*     SPKPVN is the most basic of the SPK readers, the reader upon */
/*     which SPKPV and SPKGEO, etc. are built. It should not normally */
/*     be called directly except in cases where some optimization is */
/*     required. (That is, where the calling program has prior knowledge */
/*     of the center-barycenter shifts to be performed, or a non-standard */
/*     method of determining the files and segments to be used when */
/*     computing states.) */

/*     This is the only reader which makes distinctions between the */
/*     various segment types in the SPK format. The complete list */
/*     of types currently supported is shown below. */

/*        Type   Description */
/*        ----   ----------------------- */
/*           1   Difference Lines */
/*           2   Chebyshev (P) */
/*           3   Chebyshev (P,V) */
/*           5   Two body propagation between discrete states */
/*           8   Lagrange interpolation, equally spaced discrete states */
/*           9   Lagrange interpolation, unequally spaced discrete states */
/*          12   Hermite interpolation, equally spaced discrete states */
/*          13   Hermite interpolation, unequally spaced discrete states */
/*          14   Chebyshev Unequally spaced */
/*          15   Precessing Ellipse */
/*          17   Equinoctial Elements */
/*          18   ESOC/DDID Hermite/Lagrange Interpolation */
/*          19   ESOC/DDID Piecewise Interpolation */
/*          20   Chebyshev (V) */
/*          21   Extended Modified Difference Array */

/*     SPKPVN is the only reader that needs to be changed in order to */
/*     add a new segment type to the SPK format. If a new data type is */
/*     added, the following steps should be taken: */

/*     1) Write two new routines, SPKRnn and SPKEnn, to read and */
/*        evaluate, respectively, a record from a data type nn segment. */

/*     2) Insert a new case into the body of SPKPVN to accommodate the */
/*        new type. */

/*     3) If necessary, adjust the parameter MAXREC, above, so that it */
/*        is large enough to encompass the maximum size of a record */
/*        returned by SPKRnn and passed to SPKEnn. */

/*        The maximum record lengths for each data type currently */
/*        supported are as follows: */

/*                  Data type       Maximum record length */
/*                  ---------       --------------------- */
/*                      1                    71 */
/*                      2                    87 */
/*                      3                   171 */
/*                      5                    15 */
/*                      8                   171 */
/*                      9                   197 */
/*                     12                    87 */
/*                     13                    99 */
/*                     14                 Variable */
/*                     15                    16 */
/*                     17                    12 */
/*                     18                   198 */
/*                     19                   198 */
/*                     20                   159 */
/*                     21                   112 */

/* $ Examples */

/*     In the following code fragment, an entire SPK file is searched */
/*     for segments containing a particular epoch. For each one found, */
/*     the body, center, segment identifier, and range at the epoch */
/*     are printed out. */

/*        CALL DAFOPR ( 'TEST.SPK', HANDLE ) */
/*        CALL DAFBFS (             HANDLE ) */

/*        CALL DAFFNA ( FOUND  ) */

/*        DO WHILE ( FOUND ) */

/*           CALL DAFGS ( DESCR ) */
/*           CALL DAFUS ( DESCR, 2, 6, DC, IC ) */

/*           IF ( DC(1) .LE. ET  .AND.  ET .LE. DC(2) ) THEN */
/*              CALL SPKPVN ( HANDLE, DESCR, ET, REF, STATE, CENTER ) */
/*              CALL DAFGN  ( IDENT ) */
/*              CALL FRMNAM ( REF, FRAME ) */
/*              WRITE (*,*) */
/*              WRITE (*,*) 'Body   = ', IC(1) */
/*              WRITE (*,*) 'Center = ', CENTER, */
/*              WRITE (*,*) 'ID     = ', IDENT */
/*              WRITE (*,*) 'Frame  = ', FRAME */
/*              WRITE (*,*) 'Range  = ', VNORM ( STATE ) */
/*           END IF */

/*           CALL DAFFNA ( FOUND ) */

/*        END DO */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 4.0.1, 27-AUG-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. Removed */
/*        unnecessary $Revisions section. Improved $Abstract section. */

/* -    SPICELIB Version 4.0.0, 23-DEC-2013 (NJB) */

/*        Added support for types 19, 20 and 21. Added header */
/*        comments giving description for types 18, 19, */
/*        and 21. Removed header reference to type 4. */

/* -    SPICELIB Version 3.0.0, 16-AUG-2002 (NJB) */

/*        Added support for type 18. This routine now uses the */
/*        include file spkrec.inc to declare the record size. */

/*        Corrected header comments giving record sizes for types */
/*        8, 9, 12, 13. */

/* -    SPICELIB Version 2.0.0, 06-NOV-1999 (NJB) */

/*        Added support for types 12 and 13. */

/* -    SPICELIB Version 1.1.0, 07-JAN-1997 (WLT) */

/*        Added support for type 17. */

/* -    SPICELIB Version 1.0.0, 19-SEP-1995 (WLT) */

/* -& */
/* $ Index_Entries */

/*     position and velocity from ephemeris */
/*     SPK file position and velocity */

/* -& */

/*     SPICELIB functions */


/*     Some local space is needed in which to return records, and */
/*     into which to unpack the segment descriptor. */


/*     Local Parameters */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("SPKPVN", (ftnlen)6);
    }

/*     Unpacking the segment descriptor will tell us the center, */
/*     reference frame, and data type for this segment. */

    dafus_(descr, &c__2, &c__6, dc, ic);
    *center = ic[1];
    *ref = ic[2];
    type__ = ic[3];

/*     Each data type has a pair of routines to read and evaluate */
/*     records for that data type. These routines are the only ones */
/*     that actually look inside the segments. */

/*     By the time we have more than 100 data types, we should be */
/*     allowed to use longer variable names. */

    if (type__ == 1) {
	spkr01_(handle, descr, et, record);
	spke01_(et, record, state);
    } else if (type__ == 2) {
	spkr02_(handle, descr, et, record);
	spke02_(et, record, state);
    } else if (type__ == 3) {
	spkr03_(handle, descr, et, record);
	spke03_(et, record, state);

/*     Type 04 is not officially part of the library. */

/*     ELSE IF ( TYPE .EQ. 04 ) THEN */
/*         CALL SPKR04 ( HANDLE, DESCR, ET, RECORD         ) */
/*         CALL SPKE04 (                ET, RECORD, STATE  ) */
    } else if (type__ == 5) {
	spkr05_(handle, descr, et, record);
	spke05_(et, record, state);
    } else if (type__ == 8) {
	spkr08_(handle, descr, et, record);
	spke08_(et, record, state);
    } else if (type__ == 9) {
	spkr09_(handle, descr, et, record);
	spke09_(et, record, state);
    } else if (type__ == 10) {
	spkr10_(handle, descr, et, record);
	spke10_(et, record, state);
    } else if (type__ == 12) {
	spkr12_(handle, descr, et, record);
	spke12_(et, record, state);
    } else if (type__ == 13) {
	spkr13_(handle, descr, et, record);
	spke13_(et, record, state);
    } else if (type__ == 14) {

/*        Fetch the number of Chebyshev coefficients, compute the record */
/*        size needed, and signal an error if there is not enough storage */
/*        in RECORD. The number of coefficients is the first constant */
/*        value in the generic segment. */

	sgfcon_(handle, descr, &c__1, &c__1, record);
	if (failed_()) {
	    chkout_("SPKPVN", (ftnlen)6);
	    return 0;
	}
	recsiz = (integer) record[0] * 6 + 3;
	if (recsiz > 198) {
	    setmsg_("Storage for # double precision numbers is needed for an"
		    " SPK data record and only # locations were available. Up"
		    "date the parameter MAXREC in the subroutine SPKPVN and n"
		    "otify the NAIF group of this problem.", (ftnlen)204);
	    errint_("#", &recsiz, (ftnlen)1);
	    errint_("#", &c__198, (ftnlen)1);
	    sigerr_("SPICE(SPKRECTOOLARGE)", (ftnlen)21);
	    chkout_("SPKPVN", (ftnlen)6);
	    return 0;
	}
	spkr14_(handle, descr, et, record);
	spke14_(et, record, state);
    } else if (type__ == 15) {
	spkr15_(handle, descr, et, record);
	spke15_(et, record, state);
    } else if (type__ == 17) {
	spkr17_(handle, descr, et, record);
	spke17_(et, record, state);
    } else if (type__ == 18) {
	spkr18_(handle, descr, et, record);
	spke18_(et, record, state);
    } else if (type__ == 19) {
	spkr19_(handle, descr, et, record);
	spke19_(et, record, state);
    } else if (type__ == 20) {
	spkr20_(handle, descr, et, record);
	spke20_(et, record, state);
    } else if (type__ == 21) {
	spkr21_(handle, descr, et, record);
	spke21_(et, record, state);
    } else {
	setmsg_("SPK type # is not supported in your version of the SPICE li"
		"brary.  You will need to upgrade your version of the library"
		" to make use of ephemerides that contain this SPK data type. "
		, (ftnlen)180);
	errint_("#", &type__, (ftnlen)1);
	sigerr_("SPICE(SPKTYPENOTSUPP)", (ftnlen)21);
	chkout_("SPKPVN", (ftnlen)6);
	return 0;
    }
    chkout_("SPKPVN", (ftnlen)6);
    return 0;
} /* spkpvn_ */

