/* zzcvstat.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static doublereal c_b6 = 1.;
static integer c__6 = 6;

/* $Procedure ZZCVSTAT ( Constant velocity state ) */
/* Subroutine */ int zzcvstat_0_(int n__, doublereal *et, char *ref, integer *
	center, doublereal *state, ftnlen ref_len)
{
    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int vequ_(doublereal *, doublereal *);
    static doublereal svet;
    extern /* Subroutine */ int mxvg_(doublereal *, doublereal *, integer *, 
	    integer *, doublereal *);
    doublereal delta;
    extern /* Subroutine */ int chkin_(char *, ftnlen), moved_(doublereal *, 
	    integer *, doublereal *), vlcom_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *);
    static char svref[32];
    static integer svctr;
    extern logical failed_(void);
    doublereal xf[36]	/* was [6][6] */;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    doublereal xstate[6];
    extern /* Subroutine */ int sxform_(char *, char *, doublereal *, 
	    doublereal *, ftnlen, ftnlen);
    extern logical return_(void);
    static doublereal svstat[6];

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Umbrella routine for constant velocity state "put" and */
/*     "extrapolate" routines. */

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

/*     Variable  I/O  Entry points */
/*     --------  ---  -------------------------------------------------- */
/*     ET         I   ZZCVSSTA, ZZCVXSTA */
/*     REF        I   ZZCVSSTA, ZZCVXSTA */
/*     CENTER    I-O  ZZCVSSTA, ZZCVXSTA */
/*     STATE     I-O  ZZCVSSTA, ZZCVXSTA */

/* $ Detailed_Input */

/*     See the headers of the entry points for descriptions of */
/*     their inputs. */

/* $ Detailed_Output */

/*     See the headers of the entry points for descriptions of */
/*     their outputs. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     See the headers of the entry points for descriptions of */
/*     their exceptions. */

/* $ Files */

/*     See the entry points ZZCVXSTA. */

/* $ Particulars */

/*     This suite of routines stores and extrapolates a constant-velocity */
/*     ephemeris. There are two entry points: */

/*        ZZCVSSTA  {Store ephemeris parameters} */
/*        ZZCVXSTA  {Extrapolate state} */

/*     The "store" entry point stores parameters that specify a constant- */
/*     velocity ephemeris. The routine accepts as inputs an initial state */
/*     vector, a epoch, a center of motion, and a reference frame name. */

/*     The "extrapolate" entry point extrapolates the input state to a */
/*     given epoch ET via the linear formula */

/*        Final_state =      Initial_position */
/*                      +  ( ET - Initial_ET ) * Initial_velocity */

/*     Following extrapolation, the final state is transformed to the */
/*     output reference frame. */

/* $ Examples */

/*     See usage in CVOSTA, CVTSTA. */

/* $ Restrictions */

/*     1) This routine can store data for only one */
/*        constant-velocity ephemeris. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.0, 11-JAN-2012 (NJB) */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Saved variables */


/*     This routine should never be called. */

    switch(n__) {
	case 1: goto L_zzcvxsta;
	case 2: goto L_zzcvssta;
	}

    chkin_("ZZCVSTAT", (ftnlen)8);
    sigerr_("SPICE(BOGUSENTRY)", (ftnlen)17);
    chkout_("ZZCVSTAT", (ftnlen)8);
    return 0;
/* $Procedure ZZCVXSTA ( Constant velocity state, fetch state ) */

L_zzcvxsta:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Compute target state relative to its center of motion at */
/*     a specified time, in a specified reference frame. */

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

/*     FRAMES */
/*     SPK */
/*     TIME */

/* $ Keywords */

/*     EPHEMERIS */

/* $ Declarations */

/*     DOUBLE PRECISION      ET */
/*     CHARACTER*(*)         REF */
/*     INTEGER               CENTER */
/*     DOUBLE PRECISION      STATE ( 6 ) */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     ET         I   Observer epoch. */
/*     REF        I   Inertial reference frame of output state. */
/*     CENTER     O   ID code of center of motion. */
/*     STATE      O   State vector. */

/* $ Detailed_Input */

/*     ET          is the ephemeris time, expressed as seconds past */
/*                 J2000 TDB, at which to evaluate the constant-velocity */
/*                 ephemeris defined by the latest call to ZZCVSSTA. */

/*     REF         is name of a reference frame with respect to which */
/*                 the output state vector STATE is to be expressed. */

/*                 Case and leading and trailing blanks in REF are not */
/*                 significant. */

/* $ Detailed_Output */

/*     CENTER      is the NAIF ID code of the center of motion */
/*                 associated with the output state vector. */

/*     STATE       is a state vector. The contents of STATE are */
/*                 compatible with output states returned by SPKEZR: the */
/*                 first three components of STARG represent the x-, y- */
/*                 and z-components of the target's position; the last */
/*                 three components form the corresponding velocity */
/*                 vector. */

/*                 Units are always km and km/sec. */

/*                 STATE is obtained by applying linear extrapolation */
/*                 to the state stored by the latest call to ZZCVSSTA. */
/*                 The extrapolation is performed using the difference */
/*                 between ET and the epoch stored with the saved state. */

/*                 STATE contains the result of this extrapolation, */
/*                 expressed in the reference frame designated by the */
/*                 input REF. This frame's orientation is evaluated at */
/*                 the epoch ET. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If REF is not recognized, the error will be diagnosed by */
/*         routines in the call tree of this routine. */

/*     2)  If the loaded kernels provide insufficient data to compute */
/*         the requested state vector, the deficiency will be diagnosed */
/*         by a routine in the call tree of this routine. */

/*     3)  If an error occurs while reading a kernel file, the error */
/*         will be diagnosed by a routine in the call tree of this */
/*         routine. */

/* $ Files */

/*     Appropriate kernels must be loaded by the calling program before */
/*     this routine is called. */

/*     The following data may be required to transform the stored state */
/*     to the output frame REF: */

/*        - PCK data: rotation data will be needed for */
/*          for any frame in the chain connecting the frame */
/*          of the stored state to that of the output state. */

/*        - Frame data: any frame definition required to compute */
/*          the transformation to the output frame must be known */
/*          to SPICE. If the definition is not built in, it must */
/*          be provided via a frame kernel. */

/*        - CK data: if a CK frame is required to compute the */
/*          transformation to the output frame, at least one CK file */
/*          will be needed. */

/*        - SCLK data: if a CK file is needed, an associated SCLK */
/*          kernel is required to enable conversion between encoded SCLK */
/*          (used to time-tag CK data) and barycentric dynamical time */
/*          (TDB). */

/*     In all cases, kernel data are normally loaded once per program */
/*     run, NOT every time this routine is called. */

/* $ Particulars */

/*     This routine returns the state vector stored by the entry point */

/*        ZZCVSSTA */

/*     after transforming the state to the reference frame REF. */

/* $ Examples */

/*     See usage in CVOSTA, CVTSTA. */

/* $ Restrictions */

/*     1) This routine can compute states based only on the most */
/*        recently stored state vector. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.0, 11-JAN-2012 (NJB) */

/* -& */
    if (return_()) {
	return 0;
    }
    chkin_("ZZCVXSTA", (ftnlen)8);

/*     Extrapolate the saved state to the input time. */

    delta = *et - svet;
    vlcom_(&c_b6, svstat, &delta, &svstat[3], xstate);
    vequ_(&svstat[3], &xstate[3]);

/*     Convert the extrapolated state to the requested frame */
/*     at ET. */

    sxform_(svref, ref, et, xf, (ftnlen)32, ref_len);
    if (failed_()) {
	chkout_("ZZCVXSTA", (ftnlen)8);
	return 0;
    }
    mxvg_(xf, xstate, &c__6, &c__6, state);

/*     Set the output center of motion argument as well. */

    *center = svctr;
    chkout_("ZZCVXSTA", (ftnlen)8);
    return 0;
/* $Procedure ZZCVSSTA ( Constant velocity state, store parameters ) */

L_zzcvssta:
/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Save an object's state and the center, time, and frame associated */
/*     with the state. */

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

/*     FRAMES */
/*     SPK */
/*     TIME */

/* $ Keywords */

/*     EPHEMERIS */

/* $ Declarations */

/*     DOUBLE PRECISION      STATE ( 6 ) */
/*     INTEGER               CENTER */
/*     DOUBLE PRECISION      ET */
/*     CHARACTER*(*)         REF */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     STATE      I   State vector. */
/*     CENTER     I   ID code of center of motion. */
/*     ET         I   Observer epoch. */
/*     REF        I   Inertial reference frame of output state. */

/* $ Detailed_Input */

/*     STATE       is a state vector. The contents of STATE are */
/*                 compatible with output states returned by SPKEZR. */

/*     CENTER      is the NAIF ID code of the center of motion associated */
/*                 with the input state vector. */

/*     ET          is the ephemeris time, expressed as seconds past */
/*                 J2000 TDB, associated with the input state. */

/*     REF         is name of a reference frame with respect to which */
/*                 the input STATE is expressed. This routine simply */
/*                 stores REF, so there are no restrictions on the */
/*                 contents of this string. However, at the time */
/*                 the companion entry point ZZCVXSTA is called, the */
/*                 frame designated by REF must be recognized by the */
/*                 SPICE system. */

/* $ Detailed_Output */

/*     None. This routine operates by side effects. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine stores its inputs so that they can be */
/*     used for linear extrapolation by the entry point */

/*        ZZCVXSTA */

/* $ Examples */

/*     See usage in CVOSTA, CVTSTA. */

/* $ Restrictions */

/*     1) This routine can store data for only one */
/*        constant-velocity ephemeris. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.0, 11-JAN-2012 (NJB) */

/* -& */
    if (return_()) {
	return 0;
    }

/*     No SPICE errors are detected here, so we don't check in. */


/*     Save all inputs. */

    moved_(state, &c__6, svstat);
    svctr = *center;
    svet = *et;
    s_copy(svref, ref, (ftnlen)32, ref_len);
    return 0;
} /* zzcvstat_ */

/* Subroutine */ int zzcvstat_(doublereal *et, char *ref, integer *center, 
	doublereal *state, ftnlen ref_len)
{
    return zzcvstat_0_(0, et, ref, center, state, ref_len);
    }

/* Subroutine */ int zzcvxsta_(doublereal *et, char *ref, integer *center, 
	doublereal *state, ftnlen ref_len)
{
    return zzcvstat_0_(1, et, ref, center, state, ref_len);
    }

/* Subroutine */ int zzcvssta_(doublereal *state, integer *center, doublereal 
	*et, char *ref, ftnlen ref_len)
{
    return zzcvstat_0_(2, et, ref, center, state, ref_len);
    }

