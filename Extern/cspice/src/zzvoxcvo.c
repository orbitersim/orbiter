/* zzvoxcvo.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure  ZZVOXCVO ( Voxel to coarse voxel offset ) */
/* Subroutine */ int zzvoxcvo_(integer *vixyz, integer *nvox, integer *cgrscl,
	 integer *cgxyz, integer *cgoff, integer *cgof1d)
{
    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen), sigerr_(char *, 
	    ftnlen), chkout_(char *, ftnlen), setmsg_(char *, ftnlen), 
	    errint_(char *, integer *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Map voxel coordinates to coarse voxel coordinates and offset */
/*     relative to coarse voxel. Offset coordinates are 1-based. */

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

/*     DSK */

/* $ Keywords */

/*     DSK */
/*     PRIVATE */
/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     VIXYX      I   Fine voxel integer coordinates. */
/*     NVOX       I   Fine voxel grid dimensions. */
/*     CGRSCL     I   Coarse voxel scale. */
/*     CGXYZ      O   Coarse voxel coordinates. */
/*     CGOFF      O   3-D voxel offset relative to coarse voxel. */
/*     CGOF1D     O   1-D voxel offset relative to coarse voxel. */

/* $ Detailed_Input */

/*     VIXYZ      is an array containing integer Cartesian coordinates */
/*                of a voxel in the fine voxel grid. Coordinates are */
/*                1-based. */

/*     NVOX       is an array containing the integer extents of the fine */
/*                voxel grid. Units are voxels. */

/*     CGRSCL     is the coarse voxel scale. This is the integer ratio */
/*                of the coarse voxel edge length to the fine voxel */
/*                edge length. */

/* $ Detailed_Output */

/*     CGXYZ      is an array containing integer Cartesian coordinates */
/*                of the coarse voxel that contains the fine voxel */
/*                indexed by VIXYZ. Coordinates are 1-based. */

/*     CGOFF      is an array containing the integer Cartesian */
/*                coordinates of the fine voxel relative to the coarse */
/*                voxel that contains it. Coordinates are 1-based. */

/*     CGOF1D     is the 1-dimensional offset of the input fine voxel */
/*                relative to the coarse voxel that contains it. The */
/*                offset is 1-based. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If any input fine voxel grid dimension is non-positive, */
/*        the error SPICE(VALUEOUTOFRANGE) is signaled. */

/*     2) If the coarse voxel scale is non-positive, the error */
/*        SPICE(VALUEOUTOFRANGE) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine encapsulates a frequently used type 2 DSK address */
/*     calculation. The output offset CGOF1D is useful for locating */
/*     a fine voxel's pointer into the voxel-plate list. */

/* $ Examples */

/*     See usage in the routine ZZMKSPIN. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 23-AUG-2016 (NJB) */

/*        $Exceptions section was updated. */

/*     Based on DSKLIB Version 1.0.0, 20-MAR-2015 (NJB) */

/* -& */
/* $ Index_Entries */

/*     map voxel to coarse voxel offset */

/* -& */

/*     SPICELIB functions */


/*     Local variables */

    if (return_()) {
	return 0;
    }
    for (i__ = 1; i__ <= 3; ++i__) {
	if (nvox[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("nvox", 
		i__1, "zzvoxcvo_", (ftnlen)169)] < 1) {
	    chkin_("ZZVOXCVO", (ftnlen)8);
	    setmsg_("Voxel grid dimensions must be positive but were # # #.", 
		    (ftnlen)54);
	    errint_("#", nvox, (ftnlen)1);
	    errint_("#", &nvox[1], (ftnlen)1);
	    errint_("#", &nvox[2], (ftnlen)1);
	    sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	    chkout_("ZZVOXCVO", (ftnlen)8);
	    return 0;
	}
    }
    for (i__ = 1; i__ <= 3; ++i__) {
	if (vixyz[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("vixyz", 
		i__1, "zzvoxcvo_", (ftnlen)190)] < 1 || vixyz[(i__2 = i__ - 1)
		 < 3 && 0 <= i__2 ? i__2 : s_rnge("vixyz", i__2, "zzvoxcvo_", 
		(ftnlen)190)] > nvox[(i__3 = i__ - 1) < 3 && 0 <= i__3 ? i__3 
		: s_rnge("nvox", i__3, "zzvoxcvo_", (ftnlen)190)]) {
	    chkin_("ZZVOXCVO", (ftnlen)8);
	    setmsg_("Voxel grid coordinates must be inside grid having dimen"
		    "sions # x # x # but were # # #.", (ftnlen)86);
	    errint_("#", nvox, (ftnlen)1);
	    errint_("#", &nvox[1], (ftnlen)1);
	    errint_("#", &nvox[2], (ftnlen)1);
	    errint_("#", vixyz, (ftnlen)1);
	    errint_("#", &vixyz[1], (ftnlen)1);
	    errint_("#", &vixyz[2], (ftnlen)1);
	    sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	    chkout_("ZZVOXCVO", (ftnlen)8);
	    return 0;
	}
    }
    if (*cgrscl < 1) {
	chkin_("ZZVOXCVO", (ftnlen)8);
	setmsg_("Coarse voxel grid scale must be positive but was #.", (
		ftnlen)51);
	errint_("#", nvox, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("ZZVOXCVO", (ftnlen)8);
	return 0;
    }
    for (i__ = 1; i__ <= 3; ++i__) {

/*        Set the Ith coarse grid coordinate. Recall these coordinates */
/*        are 1-based. */

	cgxyz[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("cgxyz", i__1,
		 "zzvoxcvo_", (ftnlen)232)] = (vixyz[(i__2 = i__ - 1) < 3 && 
		0 <= i__2 ? i__2 : s_rnge("vixyz", i__2, "zzvoxcvo_", (ftnlen)
		232)] - 1) / *cgrscl + 1;

/*        Set the Ith coarse grid coordinate offset. These offsets */
/*        are 1-based as well. */

	cgoff[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("cgoff", i__1,
		 "zzvoxcvo_", (ftnlen)238)] = vixyz[(i__2 = i__ - 1) < 3 && 0 
		<= i__2 ? i__2 : s_rnge("vixyz", i__2, "zzvoxcvo_", (ftnlen)
		238)] - *cgrscl * (cgxyz[(i__3 = i__ - 1) < 3 && 0 <= i__3 ? 
		i__3 : s_rnge("cgxyz", i__3, "zzvoxcvo_", (ftnlen)238)] - 1);
    }

/*     Convert the coarse grid-relative offset to a relative */
/*     ID. The ID is a one-dimensional offset. */

    *cgof1d = (cgoff[2] - 1) * *cgrscl * *cgrscl + (cgoff[1] - 1) * *cgrscl + 
	    cgoff[0];
    return 0;
} /* zzvoxcvo_ */

