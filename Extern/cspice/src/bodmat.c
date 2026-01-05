/* bodmat.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure BODMAT ( Return transformation matrix for a body ) */
/* Subroutine */ int bodmat_(integer *body, doublereal *et, doublereal *tipm)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer i__, j;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    doublereal tsipm[36]	/* was [6][6] */;
    extern logical failed_(void);
    extern /* Subroutine */ int tisbod_(char *, integer *, doublereal *, 
	    doublereal *, ftnlen), chkout_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Return the J2000 to body Equator and Prime Meridian coordinate */
/*     transformation matrix for a specified body. */

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

/*     PCK */
/*     NAIF_IDS */
/*     TIME */

/* $ Keywords */

/*     CONSTANTS */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     BODY       I   ID code of body. */
/*     ET         I   Epoch of transformation. */
/*     TIPM       O   Transformation from Inertial to PM for BODY at ET. */

/* $ Detailed_Input */

/*     BODY     is the integer ID code of the body for which the */
/*              transformation is requested. Bodies are numbered */
/*              according to the standard NAIF numbering scheme. */

/*     ET       is the epoch at which the transformation is */
/*              requested. (This is typically the epoch of */
/*              observation minus the one-way light time from */
/*              the observer to the body at the epoch of */
/*              observation.) */

/* $ Detailed_Output */

/*     TIPM     is the transformation matrix from Inertial to body */
/*              Equator and Prime Meridian. The X axis of the PM */
/*              system is directed to the intersection of the */
/*              equator and prime meridian. The Z axis points north. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If data required to define the body-fixed frame associated */
/*         with BODY are not found in the binary PCK system or the kernel */
/*         pool, the error SPICE(FRAMEDATANOTFOUND) is signaled. In */
/*         the case of IAU style body-fixed frames, the absence of */
/*         prime meridian polynomial data (which are required) is used */
/*         as an indicator of missing data. */

/*     2)  If the test for exception (1) passes, but in fact requested */
/*         data are not available in the kernel pool, an error is */
/*         signaled by a routine in the call tree of this routine. */

/*     3)  If the kernel pool does not contain all of the data required */
/*         to define the number of nutation precession angles */
/*         corresponding to the available nutation precession */
/*         coefficients, the error SPICE(INSUFFICIENTANGLES) is */
/*         signaled. */

/*     4)  If the reference frame REF is not recognized, an error is */
/*         signaled by a routine in the call tree of this routine. */

/*     5)  If the specified body code BODY is not recognized, an error is */
/*         signaled by a routine in the call tree of this routine. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine is related to the more general routine TIPBOD */
/*     which returns a matrix that transforms vectors from a */
/*     specified inertial reference frame to body equator and */
/*     prime meridian coordinates. TIPBOD accepts an input argument */
/*     REF that allows the caller to specify an inertial reference */
/*     frame. */

/*     The transformation represented by BODMAT's output argument TIPM */
/*     is defined as follows: */

/*        TIPM = [W] [DELTA] [PHI] */
/*                 3        1     3 */

/*     If there exists high-precision binary PCK kernel information */
/*     for the body at the requested time, these angles, W, DELTA */
/*     and PHI are computed directly from that file. The most */
/*     recently loaded binary PCK file has first priority followed */
/*     by previously loaded binary PCK files in backward time order. */
/*     If no binary PCK file has been loaded, the text P_constants */
/*     kernel file is used. */

/*     If there is only text PCK kernel information, it is */
/*     expressed in terms of RA, DEC and W (same W as above), where */

/*        RA    = PHI - HALFPI() */
/*        DEC   = HALFPI() - DELTA */

/*     RA, DEC, and W are defined as follows in the text PCK file: */

/*        RA  = RA0  + RA1*T  + RA2*T*T   + a  sin theta */
/*                                           i          i */

/*        DEC = DEC0 + DEC1*T + DEC2*T*T  + d  cos theta */
/*                                           i          i */

/*        W   = W0   + W1*d   + W2*d*d    + w  sin theta */
/*                                           i          i */

/*     where: */

/*        d = days past J2000. */

/*        T = Julian centuries past J2000. */

/*        a , d , and w  arrays apply to satellites only. */
/*         i   i       i */

/*        theta  = THETA0 * THETA1*T are specific to each planet. */
/*             i */

/*     These angles -- typically nodal rates -- vary in number and */
/*     definition from one planetary system to the next. */

/* $ Examples */

/*     In the following code fragment, BODMAT is used to rotate */
/*     the position vector (POS) from a target body (BODY) to a */
/*     spacecraft from inertial coordinates to body-fixed coordinates */
/*     at a specific epoch (ET), in order to compute the planetocentric */
/*     longitude (PCLONG) of the spacecraft. */

/*        CALL BODMAT ( BODY, ET, TIPM ) */
/*        CALL MXV    ( TIPM, POS, POS ) */
/*        CALL RECLAT ( POS, RADIUS, PCLONG, LAT ) */

/*     To compute the equivalent planetographic longitude (PGLONG), */
/*     it is necessary to know the direction of rotation of the target */
/*     body, as shown below. */

/*        CALL BODVCD ( BODY, 'PM', 3, DIM, VALUES ) */

/*        IF ( VALUES(2) .GT. 0.D0 ) THEN */
/*           PGLONG = PCLONG */
/*        ELSE */
/*           PGLONG = TWOPI() - PCLONG */
/*        END IF */

/*     Note that the items necessary to compute the transformation */
/*     TIPM must have been loaded into the kernel pool (by one or more */
/*     previous calls to FURNSH). */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */
/*     K.S. Zukor         (JPL) */

/* $ Version */

/* -    SPICELIB Version 4.2.1, 14-APR-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. Moved NAIF_IDS */
/*        required reading from $Literature_References to */
/*        $Required_Reading section. */

/* -    SPICELIB Version 4.2.0, 27-JUL-2016 (BVS) */

/*        Updated to use the 3x3 top-left corner of the 6x6 matrix */
/*        returned by TISBOD instead of fetching kernel data and doing */
/*        computations in-line. */

/* -    SPICELIB Version 4.1.1, 01-FEB-2008 (NJB) */

/*        The routine was updated to improve the error messages created */
/*        when required PCK data are not found. Now in most cases the */
/*        messages are created locally rather than by the kernel pool */
/*        access routines. In particular missing binary PCK data will */
/*        be indicated with a reasonable error message. */

/* -    SPICELIB Version 4.1.0, 25-AUG-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in MXM call. */

/*        Calls to ZZBODVCD have been replaced with calls to */
/*        BODVCD. */

/* -    SPICELIB Version 4.0.0, 12-FEB-2004 (NJB) */

/*        Code has been updated to support satellite ID codes in the */
/*        range 10000 to 99999 and to allow nutation precession angles */
/*        to be associated with any object. */

/*        Implementation changes were made to improve robustness */
/*        of the code. */

/* -    SPICELIB Version 3.2.0, 22-MAR-1995 (KSZ) */

/*        Gets TSIPM matrix from PCKMAT (instead of Euler angles */
/*        from PCKEUL.) */

/* -    SPICELIB Version 3.0.0, 10-MAR-1994 (KSZ) */

/*        Ability to get Euler angles from binary PCK file added. */
/*        This uses the new routine PCKEUL. */

/* -    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB) */

/*        Updated to handle P_constants referenced to different epochs */
/*        and inertial reference frames. */

/*        The header was updated to specify that the inertial reference */
/*        frame used by BODMAT is restricted to be J2000. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU) */

/* -& */
/* $ Index_Entries */

/*     fetch transformation matrix for a body */
/*     transformation from j2000 position to bodyfixed */
/*     transformation from j2000 to bodyfixed coordinates */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 4.2.0, 27-JUL-2016 (BVS) */

/*        Updated to use the 3x3 top-left corner of the 6x6 matrix */
/*        returned by TISBOD instead of fetching kernel data and doing */
/*        computations in-line. */

/* -    SPICELIB Version 4.1.0, 25-AUG-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in MXM call. */

/*        Calls to ZZBODVCD have been replaced with calls to */
/*        BODVCD. */

/* -    SPICELIB Version 4.0.0, 12-FEB-2004 (NJB) */

/*        Code has been updated to support satellite ID codes in the */
/*        range 10000 to 99999 and to allow nutation precession angles */
/*        to be associated with any object. */

/*        Calls to deprecated kernel pool access routine RTPOOL */
/*        were replaced by calls to GDPOOL. */

/*        Calls to BODVAR have been replaced with calls to */
/*        ZZBODVCD. */

/* -    SPICELIB Version 3.2.0, 22-MAR-1995 (KSZ) */

/*        BODMAT now get the TSIPM matrix from PCKMAT, and */
/*        unpacks TIPM from it. Also the calculated but unused */
/*        variable LAMBDA was removed. */

/* -    SPICELIB Version 3.0.0, 10-MAR-1994 (KSZ) */

/*        BODMAT now uses new software to check for the */
/*        existence of binary PCK files, search the for */
/*        data corresponding to the requested body and time, */
/*        and return the appropriate Euler angles, using the */
/*        new routine PCKEUL. Otherwise the code calculates */
/*        the Euler angles from the P_constants kernel file. */

/* -    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB) */

/*        Updated to handle P_constants referenced to different epochs */
/*        and inertial reference frames. */

/*        The header was updated to specify that the inertial reference */
/*        frame used by BODMAT is restricted to be J2000. */

/*        BODMAT now checks the kernel pool for presence of the */
/*        variables */

/*           BODY#_CONSTANTS_REF_FRAME */

/*        and */

/*           BODY#_CONSTANTS_JED_EPOCH */

/*        where # is the NAIF integer code of the barycenter of a */
/*        planetary system or of a body other than a planet or */
/*        satellite. If either or both of these variables are */
/*        present, the P_constants for BODY are presumed to be */
/*        referenced to the specified inertial frame or epoch. */
/*        If the epoch of the constants is not J2000, the input */
/*        time ET is converted to seconds past the reference epoch. */
/*        If the frame of the constants is not J2000, the rotation from */
/*        the P_constants' frame to body-fixed coordinates is */
/*        transformed to the rotation from J2000 coordinates to */
/*        body-fixed coordinates. */

/*        For efficiency reasons, this routine now duplicates much */
/*        of the code of BODEUL so that it doesn't have to call BODEUL. */
/*        In some cases, BODEUL must covert Euler angles to a matrix, */
/*        rotate the matrix, and convert the result back to Euler */
/*        angles. If this routine called BODEUL, then in such cases */
/*        this routine would convert the transformed angles back to */
/*        a matrix. That would be a bit much.... */

/* -    Beta Version 1.1.0, 16-FEB-1989 (IMU) (NJB) */

/*        $Examples section completed. Declaration of unused variable */
/*        FOUND removed. */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE Error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("BODMAT", (ftnlen)6);
    }

/*     Get 6x6 state transformation from TISBOD. If succeeded, pull out */
/*     left-top 3x3 matrix. */

    tisbod_("J2000", body, et, tsipm, (ftnlen)5);
    if (failed_()) {
	chkout_("BODMAT", (ftnlen)6);
	return 0;
    }
    for (i__ = 1; i__ <= 3; ++i__) {
	for (j = 1; j <= 3; ++j) {
	    tipm[(i__1 = i__ + j * 3 - 4) < 9 && 0 <= i__1 ? i__1 : s_rnge(
		    "tipm", i__1, "bodmat_", (ftnlen)413)] = tsipm[(i__2 = 
		    i__ + j * 6 - 7) < 36 && 0 <= i__2 ? i__2 : s_rnge("tsipm"
		    , i__2, "bodmat_", (ftnlen)413)];
	}
    }
    chkout_("BODMAT", (ftnlen)6);
    return 0;
} /* bodmat_ */

