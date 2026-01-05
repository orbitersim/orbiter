/* spkpv.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__6 = 6;

/* $Procedure SPKPV ( S/P Kernel, position and velocity ) */
/* Subroutine */ int spkpv_(integer *handle, doublereal *descr, doublereal *
	et, char *ref, doublereal *state, integer *center, ftnlen ref_len)
{
    /* Initialized data */

    static logical first = TRUE_;

    extern /* Subroutine */ int mxvg_(doublereal *, doublereal *, integer *, 
	    integer *, doublereal *), zznamfrm_(integer *, char *, integer *, 
	    char *, integer *, ftnlen, ftnlen), zzctruin_(integer *), chkin_(
	    char *, ftnlen), dafus_(doublereal *, integer *, integer *, 
	    doublereal *, integer *), errch_(char *, char *, ftnlen, ftnlen);
    static char svref[32];
    doublereal xform[36]	/* was [6][6] */, dc[2];
    integer ic[6];
    static integer svctr1[2];
    extern /* Subroutine */ int frmchg_(integer *, integer *, doublereal *, 
	    doublereal *);
    integer irfreq;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen);
    doublereal tstate[6];
    static integer svirfr;
    extern logical return_(void);
    extern /* Subroutine */ int spkpvn_(integer *, doublereal *, doublereal *,
	     integer *, doublereal *, integer *);
    integer irf;

/* $ Abstract */

/*     Return the state (position and velocity) of a target body */
/*     relative to some center of motion in a specified frame. */

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

/*     This include file defines the dimension of the counter */
/*     array used by various SPICE subsystems to uniquely identify */
/*     changes in their states. */

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

/* $ Parameters */

/*     CTRSIZ      is the dimension of the counter array used by */
/*                 various SPICE subsystems to uniquely identify */
/*                 changes in their states. */

/* $ Author_and_Institution */

/*     B.V. Semenov    (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 29-JUL-2013 (BVS) */

/* -& */

/*     End of include file. */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   File handle. */
/*     DESCR      I   Segment descriptor. */
/*     ET         I   Target epoch. */
/*     REF        I   Target reference frame. */
/*     STATE      O   Position, velocity. */
/*     CENTER     O   Center of state. */

/* $ Detailed_Input */

/*     HANDLE, */
/*     DESCR    are the file handle assigned to a SPK file, and the */
/*              descriptor for a segment within the file. Together */
/*              they determine the ephemeris data from which the */
/*              state of the body is to be computed. */

/*     ET       is the epoch (ephemeris time) at which the state */
/*              is to be computed. */

/*     REF      is the name of the reference frame to */
/*              which the vectors returned by the routine should */
/*              be rotated. This may be any frame supported by */
/*              the SPICELIB subroutine FRMCHG. */

/* $ Detailed_Output */

/*     STATE    contains the position and velocity, at epoch ET, */
/*              for whatever body is covered by the specified segment. */
/*              STATE has six elements: the first three contain the */
/*              body's position; the last three contain the body's */
/*              velocity. These vectors are rotated into the */
/*              specified reference frame, the origin of */
/*              which is located at the center of motion for the */
/*              body (see CENTER, below).  Units are always km and */
/*              km/sec. */

/*     CENTER   is the integer ID code of the center of motion for */
/*              the state. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the requested reference frame is not supported by the */
/*         current version of CHGIRF, the error SPICE(SPKREFNOTSUPP) */
/*         is signaled. */

/* $ Files */

/*     See argument HANDLE. */

/* $ Particulars */

/*     Once SPKPV was the most basic of the SPK readers, the reader upon */
/*     which SPKSSB, SPKAPP, and SPKEZ were built. However, its function */
/*     has now largely been replaced by SPKPVN. SPKPV should not normally */
/*     be called except by old software written before the release of */
/*     SPKPVN. This routine should be considered obsolete. */

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
/*              CALL SPKPV ( HANDLE, DESCR, ET, 'J2000', STATE, CENTER ) */
/*              CALL DAFGN ( IDENT ) */

/*              WRITE (*,*) */
/*              WRITE (*,*) 'Body   = ', IC(1) */
/*              WRITE (*,*) 'Center = ', CENTER, */
/*              WRITE (*,*) 'ID     = ', IDENT */
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
/*     K.R. Gehringer     (JPL) */
/*     J.M. Lynch         (JPL) */
/*     H.A. Neilan        (JPL) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */
/*     R.E. Thurman       (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 6.1.1, 14-APR-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. Removed */
/*        unnecessary $Revision section. Moved SPK required reading from */
/*        $Literature_References to $Required_Reading section. */

/* -    SPICELIB Version 6.1.0, 06-DEC-2013 (BVS) (NJB) */

/*        Updated to save the input frame name and POOL state counter */
/*        and to do frame name-ID conversion only if the counter has */
/*        changed. In-line comment regarding frame change was edited. */

/* -    SPICELIB Version 6.0.0, 19-SEP-1995 (WLT) */

/*        The routine was updated to handle non-inertial frames. */

/* -    SPICELIB Version 5.0.0, 13-MAR-1995 (KRG) */

/*        The routine was updated to handle type 14. */

/*        A new exception, 3, was also added. */

/* -    SPICELIB Version 4.0.0, 04-NOV-1994 (WLT) */

/*        The routine was updated to handle type 15. */

/* -    SPICELIB Version 3.0.0, 04-AUG-1993 (NJB) */

/*        The routine was updated to handle types 08 and 09. */

/* -    SPICELIB Version 2.0.0, 01-APR-1992 (JML) */

/*        The routine was updated to handle type 05. */

/* -    SPICELIB Version 1.0.2, 18-JUL-1991 (NJB) */

/*        The description of the output STATE was expanded slightly. */

/* -    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN) */

/*        Literature references added to the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (RET) */

/* -& */
/* $ Index_Entries */

/*     position and velocity from ephemeris */
/*     SPK file position and velocity */

/* -& */

/*     SPICELIB functions */


/*     Local parameters. */


/*     Saved frame name length. */


/*     Some local space is needed in which to return records, and */
/*     into which to unpack the segment descriptor. */


/*     Saved frame name/ID item declarations. */


/*     Saved frame name/ID items. */


/*     Initial values. */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("SPKPV", (ftnlen)5);
    }

/*     Initialization. */

    if (first) {

/*        Initialize counter. */

	zzctruin_(svctr1);
	first = FALSE_;
    }
    dafus_(descr, &c__2, &c__6, dc, ic);
    *center = ic[1];
    irf = ic[2];

/*     Rotate the raw state from its native frame to the one requested */
/*     by the user only if the two frames differ. */

    zznamfrm_(svctr1, svref, &svirfr, ref, &irfreq, (ftnlen)32, ref_len);
    if (irfreq == 0) {
	setmsg_("No support for frame #.", (ftnlen)23);
	errch_("#", ref, (ftnlen)1, ref_len);
	sigerr_("SPICE(SPKREFNOTSUPP)", (ftnlen)20);
    } else if (irfreq != irf) {
	spkpvn_(handle, descr, et, &irf, tstate, center);
	frmchg_(&irf, &irfreq, et, xform);
	mxvg_(xform, tstate, &c__6, &c__6, state);
    } else {
	spkpvn_(handle, descr, et, &irf, state, center);
    }
    chkout_("SPKPV", (ftnlen)5);
    return 0;
} /* spkpv_ */

