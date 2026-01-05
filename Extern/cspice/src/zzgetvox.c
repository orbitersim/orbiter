/* zzgetvox.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ZZGETVOX ( Coordinates of voxel containing a point ) */
/* Subroutine */ int zzgetvox_(doublereal *voxsiz, doublereal *voxori, 
	integer *nvox, doublereal *xyz, logical *inbox, integer *voxcor)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    doublereal term;
    integer i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen), errdp_(char *, 
	    doublereal *, ftnlen), sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     This subroutine returns the 1-based voxel coordinates of the */
/*     voxel that contains a given point. */

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

/*     PLATE */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     VOXSIZ     I   Voxel size in model units. */
/*     VOXORI     I   3-Vector location of the voxel grid origin */
/*                    in model units. */
/*     NVOX       I   Number of voxel in each dimension. */
/*     XYZ        I   Model grid coordinates of point. */
/*     INBOX      O   Logical value is true if point is inside voxel */
/*                    grid, false if not. */
/*     VOXCOR     O   3-Index of voxel location with the voxel grid. */

/* $ Detailed_Input */

/*     VOXSIZ     is a double precision scalar value giving the size of */
/*                each cubical voxel in model units. */

/*     VOXORI     is a double precision 3-vector indicating the location */
/*                of the voxel grid origin in model units. */

/*     NVOX       is an integer 3-vector giving the length of each */
/*                side of the voxel grid in voxel units. */

/*     XYZ        is a double precision 3-vector containing the location */
/*                of the point in question in model units. */

/* $ Detailed_Output */

/*     INBOX      is a logical scalar indicating whether or not the */
/*                point XYZ lies within the extent of the voxel grid. */

/*     VOXCOR     is an integer 3-vector containing the x, y, and z */
/*                voxel indices of the voxel containing the point. */

/*                VOXCOR is valid only if INBOX is .TRUE. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If VOXSIZ is less than or equal to zero, the error */
/*         SPICE(NONPOSITIVEVALUE) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This is not a general coordinate conversion routine. This routine */
/*     returns valid voxel coordinates only for voxels inside the voxel */
/*     grid. */

/*     Points on the outer surface of the grid, as defined by the voxel */
/*     origin and the grid dimensions, are considered to be inside the */
/*     grid. */

/*     Points outside of the grid are not mapped to voxel coordinates. */

/* $ Examples */


/*   C */
/*   C     Some position in model coordinates, i.e. the body fixed */
/*   C     units and frame coordinates. */
/*   C */
/*         DOUBLE PRECISION       POS(3) */

/*               ... */

/*   C */
/*   C     Retrieve the voxel grid geometry description. */
/*   C */
/*         CALL VOXDIM ( NVOX, VOXSIZ, VOXORI) */

/*   C */
/*   C     Convert the coordinate POS to voxel grid indices. */
/*   C */
/*         CALL ZZGETVOX ( VOXSIZ, VOXORI, NVOX, POS, INBOX, VOXCOR ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */
/*     J.A. Bytof      (JPL) */
/*     E.D. Wright     (JPL */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 01-OCT-2021 (NJB) */

/*        Corrected typos in comments. */

/* -    SPICELIB Version 1.0.0, 18-MAR-2016 (NJB) */

/*        Added error check for non-positive voxel size. */
/*        Made small changes to header. */

/*        17-JAN-2016 (NJB) */

/*           Functional change: routine returns valid voxel coordinates */
/*           only for voxels inside the voxel grid. Points on the outer */
/*           surface of the grid are considered to be inside the grid. */

/*           Changed check-in to discovery style. */

/*           Corrected rounding for voxels on outer edge of grid. */
/*           Updated header. */

/*        08-OCT-2009 (NJB) */

/*           Re-ordered header sections. */

/*        14-SEP-2004 (EDW) */

/*           Corrected VOXID(I) functions to allow for */
/*           negative voxel coordinates. Original functions */
/*           returned values GE 1. */

/*           Added Examples section. */

/*        28-JAN-1999 (JAB) */

/*           Original version. */

/* -& */
/* $ Index_Entries */

/*     returns the voxel grid indices containing a given point */

/* -& */

/*     SPICELIB functions */


/*     Local variables. */


/*     Use discovery check-in. */

    if (return_()) {
	return 0;
    }
    if (*voxsiz <= 0.) {
	chkin_("ZZGETVOX", (ftnlen)8);
	setmsg_("Voxel size was #; must be positive.", (ftnlen)35);
	errdp_("#", voxsiz, (ftnlen)1);
	sigerr_("SPICE(NONPOSITIVEVALUE)", (ftnlen)23);
	chkout_("ZZGETVOX", (ftnlen)8);
	return 0;
    }

/*     Initialize 'point in box' flag and voxel coordinates. The */
/*     coordinates are assigned out-of-range values. */

    *inbox = FALSE_;
    voxcor[0] = 0;
    voxcor[1] = 0;
    voxcor[2] = 0;

/*     Scale the point's coordinates to voxel grid space */
/*     and determine the indices of the voxel that contains it. */

    for (i__ = 1; i__ <= 3; ++i__) {

/*        A Galilean transform. Calculate the voxel coordinate */
/*        corresponding to the body centered coordinate. This */
/*        operation performs the same task as TOGRID, but */
/*        including the operation here improves ZZGETVOX's */
/*        runtime performance. */

	term = (xyz[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("xyz", 
		i__1, "zzgetvox_", (ftnlen)253)] - voxori[(i__2 = i__ - 1) < 
		3 && 0 <= i__2 ? i__2 : s_rnge("voxori", i__2, "zzgetvox_", (
		ftnlen)253)]) / *voxsiz;

/*        Calculate the voxel index for each degree of freedom */
/*        corresponding to the voxel coordinate. */

/*        If the point is outside of the grid, return now. */

	if (term < 0. || term > (doublereal) nvox[(i__1 = i__ - 1) < 3 && 0 <=
		 i__1 ? i__1 : s_rnge("nvox", i__1, "zzgetvox_", (ftnlen)261)]
		) {
	    return 0;
	}

/*        Assign a 1-based value to the Ith component of the voxel's */
/*        coordinates. The outer surface of the grid is considered part */
/*        of the grid. */

/*        Note that TERM is non-negative at this point. */

	if ((integer) term < nvox[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : 
		s_rnge("nvox", i__1, "zzgetvox_", (ftnlen)274)]) {
	    voxcor[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("voxcor",
		     i__1, "zzgetvox_", (ftnlen)276)] = (integer) term + 1;
	} else {

/*           TERM is NVOX(I), since the cases */

/*              TERM > NVOX(I) */
/*              TERM < NVOX(I) */

/*           have been ruled out. */

	    voxcor[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("voxcor",
		     i__1, "zzgetvox_", (ftnlen)287)] = nvox[(i__2 = i__ - 1) 
		    < 3 && 0 <= i__2 ? i__2 : s_rnge("nvox", i__2, "zzgetvox_"
		    , (ftnlen)287)];
	}
    }
    *inbox = TRUE_;
    return 0;
} /* zzgetvox_ */

