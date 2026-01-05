/* zztwovxf.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__6 = 6;
static integer c__3 = 3;

/* $Procedure ZZTWOVXF ( Two states defining a frame transformation ) */
/* Subroutine */ int zztwovxf_(doublereal *axdef, integer *indexa, doublereal 
	*plndef, integer *indexp, doublereal *xform)
{
    /* Initialized data */

    static integer seqnce[5] = { 1,2,3,1,2 };

    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer i__, j;
    extern /* Subroutine */ int chkin_(char *, ftnlen), dvhat_(doublereal *, 
	    doublereal *), moved_(doublereal *, integer *, doublereal *);
    integer i1, i2, i3;
    extern logical vzero_(doublereal *);
    extern /* Subroutine */ int cleard_(integer *, doublereal *), sigerr_(
	    char *, ftnlen), chkout_(char *, ftnlen), setmsg_(char *, ftnlen),
	     errint_(char *, integer *, ftnlen), ducrss_(doublereal *, 
	    doublereal *, doublereal *);
    doublereal tmpsta[6];
    extern logical return_(void);

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Find the state transformation to a base frame from the */
/*     right-handed frame defined by two state vectors:  one state */
/*     vector defining a specified axis and a second state vector */
/*     defining a specified coordinate plane. */

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

/*     AXES */
/*     FRAMES */
/*     MATRIX */
/*     TRANSFORMATION */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  ------------------------------------------------- */
/*     AXDEF      I   State defining a principal axis. */
/*     INDEXA     I   Principal axis number of AXDEF (X=1, Y=2, Z=3). */
/*     PLNDEF     I   State defining (with AXDEF) a principal plane. */
/*     INDEXP     I   Second axis number (with INDEXA) of principal */
/*                    plane. */
/*     XFORM      O   Output state transformation matrix. */

/* $ Detailed_Input */

/*     AXDEF      is a "generalized" state vector defining one of the */
/*                principal axes of a reference frame. This vector */
/*                consists of three components of a vector-valued */
/*                function of one independent variable t followed by */
/*                the derivatives of the components with respect to that */
/*                variable: */

/*                   ( a, b, c, da/dt, db/dt, dc/dt ) */

/*                This routine treats the input states as unitless, but */
/*                in most applications the input states represent */
/*                quantities that have associated units. The first three */
/*                components must have the same units, and the units of */
/*                the last three components must be compatible with */
/*                those of the first three:  if the first three */
/*                components of AXDEF */

/*                   ( a, b, c ) */

/*                have units U and t has units T, then the units of */
/*                AXDEF normally would be */

/*                   ( U, U, U, U/T, U/T, U/T ) */

/*                Note that the direction and angular velocity defined */
/*                by AXDEF are actually independent of U, so scaling */
/*                AXDEF doesn't affect the output of this routine. */

/*                AXDEF could represent position and velocity; it could */
/*                also represent velocity and acceleration.  AXDEF could */
/*                for example represent the velocity and acceleration of */
/*                a time-dependent position vector ( x(t), y(t), z(t) ), */
/*                in which case AXDEF would be defined by */

/*                   a     = dx/dt */
/*                   b     = dy/dt */
/*                   c     = dz/dt */

/*                            2      2 */
/*                   da/dt = d x / dt */

/*                            2      2 */
/*                   db/dt = d y / dt */

/*                            2      2 */
/*                   dc/dt = d z / dt */

/*                Below, we'll call the normalized (unit length) version */
/*                of */

/*                   ( a, b, c ) */

/*                the "direction" of AXDEF. */

/*                We call the frame relative to which AXDEF is specified */
/*                the "base frame."  The input state PLNDEF must be */
/*                specified relative to the same base frame. */


/*     INDEXA     is the index of the reference frame axis that is */
/*                parallel to the direction of AXDEF. */

/*                   Value of INDEXA             Axis */

/*                         1                      X */
/*                         2                      Y */
/*                         3                      Z */


/*     PLNDEF     is a state vector defining (with AXDEF) a principal */
/*                plane of the reference frame.  This vector consists */
/*                of three components followed by their derivatives with */
/*                respect to the independent variable t associated with */
/*                AXDEF, so PLNDEF is */

/*                   ( e, f, g, de/dt, df/dt, dg/dt ) */

/*                Below, we'll call the unitized version of */

/*                   ( e, f, g ) */

/*                the "direction" of PLNDEF. */

/*                The second axis of the principal plane containing the */
/*                direction vectors of AXDEF and PLNDEF is perpendicular */
/*                to the first axis and has positive dot product with */
/*                the direction vector of PLNDEF. */

/*                The first three components of PLNDEF must have the */
/*                same units, and the units of the last three components */
/*                must be compatible with those of the first three:  if */
/*                the first three components of PLNDEF */

/*                   ( e, f, g ) */

/*                have units U2 and t has units T, then the units of */
/*                PLNDEF normally would be */

/*                   ( U2, U2, U2, U2/T, U2/T, U2/T ) */

/*                ***For meaningful results, the angular velocities */
/*                   defined by AXDEF and PLNDEF must both have units of */
/*                   1/T.*** */

/*                As with AXDEF, scaling PLNDEF doesn't affect the */
/*                output of this routine. */

/*                AXDEF and PLNDEF must be specified relative to a */
/*                common reference frame, which we call the "base */
/*                frame." */


/*     INDEXP     is the index of  second axis of the principal frame */
/*                determined by AXDEF and PLNDEF.  The association of */
/*                integer values and axes is the same as for INDEXA. */

/* $ Detailed_Output */

/*     XFORM      is the 6x6 matrix that transforms states to the frame */
/*                relative to which AXDEF and PLNDEF are specified (the */
/*                "base frame") from the frame whose axes and derivative */
/*                are determined by AXDEF, PLNDEF, INDEXA and INDEXP. */

/*                The matrix XFORM has the structure shown below: */

/*                    -            - */
/*                   |       :      | */
/*                   |   R   :  0   | */
/*                   | ......:......| */
/*                   |       :      | */
/*                   | dR_dt :  R   | */
/*                   |       :      | */
/*                    -            - */

/*                where R is a rotation matrix that is a function of */
/*                the independent variable associated with AXDEF and */
/*                PLNDEF, and where dR_dt is the derivative of R */
/*                with respect to that independent variable. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If INDEXA or INDEXP is not in the set {1,2,3} the error */
/*        SPICE(BADINDEX) will be signaled. */

/*     2) If INDEXA and INDEXP are the same the error */
/*        SPICE(UNDEFINEDFRAME) will be signaled. */

/*     3) If the cross product of the vectors AXDEF and PLNDEF is zero, */
/*        the error SPICE(DEPENDENTVECTORS) will be signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine exists to support the public routine TWOVXF: */
/*     TWOVXF does its job by calling this routine, inverting the */
/*     matrix returned by this routine, and returning the result. */

/*     The SPICELIB frame subsystem typically requires this routine */
/*     rather than TWOVXF, since the frame subsystem produces */
/*     transformations from frames defined in frame kernels to their */
/*     base frames.  Calling this routine rather than TWOVXF allows */
/*     the frame subsystem to eliminate two unnecessary calls to */
/*     INVSTM. */

/*     Given two linearly independent state vectors AXDEF and PLNDEF, */
/*     define vectors DIR1 and DIR2 by */

/*        DIR1 = ( AXDEF(1),   AXDEF(2),   AXDEF(3)  ) */
/*        DIR2 = ( PLNDEF(1),  PLNDEF(2),  PLNDEF(3) ) */

/*     Then there is a unique right-handed reference frame F having: */

/*        DIR1 lying along the INDEXA axis. */

/*        DIR2 lying in the INDEXA-INDEXP coordinate plane, such that */
/*        the dot product of DIR2 with the positive INDEXP axis is */
/*        positive. */

/*     This routine determines the 6x6 matrix that transforms states */
/*     to the base frame used to represent the input vectors from the */
/*     the frame F determined by AXDEF and PLNDEF.  Thus a state vector */

/*        S       = ( x, y, z, dx/dt, dy/dt, dz/dt ) */
/*         F */

/*     in the reference frame F will be transformed to */

/*        S      = XFORM * S */
/*         base             F */

/*     in the base frame relative to which AXDEF and PLNDEF are */
/*     specified. */

/* $ Examples */

/*     See TWOVXF. */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */
/*     W.M. Owen       (JPL) */
/*     W.L. Taber      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 06-SEP-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in DUCRSS and MOVED calls. */

/* -    SPICELIB Version 1.0.0, 18-DEC-2004 (NJB) (WMO) (WLT) */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.1.0, 06-SEP-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in DUCRSS and MOVED calls. */

/* -& */

/*     SPICELIB functions */


/*     Local Variables */


/*     Saved variables */


/*     Initial values */


/*     Standard SPICE error handling */

    if (return_()) {
	return 0;
    }
    chkin_("ZZTWOVXF", (ftnlen)8);

/*     Check for obvious bad inputs. */

    if (max(*indexp,*indexa) > 3 || min(*indexp,*indexa) < 1) {
	setmsg_("The definition indices must lie in the range from 1 to 3.  "
		"The value of INDEXA was #. The value of INDEXP was #. ", (
		ftnlen)113);
	errint_("#", indexa, (ftnlen)1);
	errint_("#", indexp, (ftnlen)1);
	sigerr_("SPICE(BADINDEX)", (ftnlen)15);
	chkout_("ZZTWOVXF", (ftnlen)8);
	return 0;
    } else if (*indexa == *indexp) {
	setmsg_("The values of INDEXA and INDEXP were the same, namely #.  T"
		"hey are required to be different.", (ftnlen)92);
	errint_("#", indexa, (ftnlen)1);
	sigerr_("SPICE(UNDEFINEDFRAME)", (ftnlen)21);
	chkout_("ZZTWOVXF", (ftnlen)8);
	return 0;
    }

/*     Get indices for right-handed axes: */

/*     First AXDEF ... */

    i1 = *indexa;

/*     ... then the other two. */

    i2 = seqnce[(i__1 = *indexa) < 5 && 0 <= i__1 ? i__1 : s_rnge("seqnce", 
	    i__1, "zztwovxf_", (ftnlen)387)];
    i3 = seqnce[(i__1 = *indexa + 1) < 5 && 0 <= i__1 ? i__1 : s_rnge("seqnce"
	    , i__1, "zztwovxf_", (ftnlen)388)];

/*     Column I1 of XFORM contains a unit vector parallel to AXDEF and */
/*     the derivative of the unit vector. */

    dvhat_(axdef, &xform[(i__1 = i1 * 6 - 6) < 36 && 0 <= i__1 ? i__1 : 
	    s_rnge("xform", i__1, "zztwovxf_", (ftnlen)394)]);

/*     Obtain columns I2 and I3 of XFORM using cross products. */
/*     Which order to use depends on whether INDEXP = I2 (next axis in */
/*     right-handed order) or INDEXP = I3 (previous axis in right-handed */
/*     order). */

/*     Select column indices... */

    if (*indexp == i2) {

/*        We compute the third axis in the sequence, then the second. */

	ducrss_(axdef, plndef, &xform[(i__1 = i3 * 6 - 6) < 36 && 0 <= i__1 ? 
		i__1 : s_rnge("xform", i__1, "zztwovxf_", (ftnlen)408)]);
	ducrss_(&xform[(i__1 = i3 * 6 - 6) < 36 && 0 <= i__1 ? i__1 : s_rnge(
		"xform", i__1, "zztwovxf_", (ftnlen)409)], axdef, tmpsta);
	moved_(tmpsta, &c__6, &xform[(i__1 = i2 * 6 - 6) < 36 && 0 <= i__1 ? 
		i__1 : s_rnge("xform", i__1, "zztwovxf_", (ftnlen)410)]);
    } else {
	ducrss_(plndef, axdef, &xform[(i__1 = i2 * 6 - 6) < 36 && 0 <= i__1 ? 
		i__1 : s_rnge("xform", i__1, "zztwovxf_", (ftnlen)412)]);
	ducrss_(axdef, &xform[(i__1 = i2 * 6 - 6) < 36 && 0 <= i__1 ? i__1 : 
		s_rnge("xform", i__1, "zztwovxf_", (ftnlen)413)], tmpsta);
	moved_(tmpsta, &c__6, &xform[(i__1 = i3 * 6 - 6) < 36 && 0 <= i__1 ? 
		i__1 : s_rnge("xform", i__1, "zztwovxf_", (ftnlen)414)]);
    }

/*     ...and compute the output frame's non-principal unit basis */
/*     vectors and the derivatives of these vectors. */


/*     At this point, we've filled in the left half of XFORM. */

/*     The upper right block is the 3x3 zero matrix. */
/*     The lower right block matches the upper left block. */

    cleard_(&c__3, &xform[18]);
    cleard_(&c__3, &xform[24]);
    cleard_(&c__3, &xform[30]);
    for (j = 1; j <= 3; ++j) {
	for (i__ = 1; i__ <= 3; ++i__) {
	    xform[(i__1 = i__ + 3 + (j + 3) * 6 - 7) < 36 && 0 <= i__1 ? i__1 
		    : s_rnge("xform", i__1, "zztwovxf_", (ftnlen)436)] = 
		    xform[(i__2 = i__ + j * 6 - 7) < 36 && 0 <= i__2 ? i__2 : 
		    s_rnge("xform", i__2, "zztwovxf_", (ftnlen)436)];
	}
    }

/*     Finally, check to see that we actually got something non-zero in */
/*     the first three components of at least one of the columns */
/*     XFORM(1,I2) and XFORM(1,I3) (we need only check one of them since */
/*     they are related by a cross product). */

    if (vzero_(&xform[(i__1 = i2 * 6 - 6) < 36 && 0 <= i__1 ? i__1 : s_rnge(
	    "xform", i__1, "zztwovxf_", (ftnlen)448)])) {
	setmsg_("The direction vectors associated with states AXDEF and PLND"
		"EF are linearly dependent.", (ftnlen)85);
	sigerr_("SPICE(DEPENDENTVECTORS)", (ftnlen)23);
	chkout_("ZZTWOVXF", (ftnlen)8);
	return 0;
    }
    chkout_("ZZTWOVXF", (ftnlen)8);
    return 0;
} /* zztwovxf_ */

