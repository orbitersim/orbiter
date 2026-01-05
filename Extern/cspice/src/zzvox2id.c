/* zzvox2id.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ZZVOX2ID ( Return voxel index from coords ) */
integer zzvox2id_(integer *vixyz, integer *nvox)
{
    /* System generated locals */
    integer ret_val;

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Given a voxel's grid coordinates and the voxel grid */
/*     dimensions, return the voxel's 1-dimensional index. */

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

/*     COORDINATE */
/*     DSK */
/*     INDEX */
/*     VOXEL */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     VIXYZ      I   Voxel's grid coordinates stored in a */
/*                    three-dimensional vector. */
/*     NVOX       I   Number of voxels in each coordinate direction. */

/*     The function returns the voxel index. */

/* $ Detailed_Input */

/*     VIXYZ      An integer 3-vector storing the voxel grid coordinates */
/*                for the point of interest. */

/*     NVOX       An array of three positive integers defining a voxel */
/*                grid's extents in the X, Y, and Z directions. */

/* $ Detailed_Output */

/*     The function returns the value of the voxel index. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     See usage in ZZMKSPIN. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */
/*     J.A. Bytof      (JPL) */
/*     E.D. Wright     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 08-FEB-2017 (NJB) */

/*        Updated version info. */

/*        17-JAN-2016 (NJB) */

/*           Re-wrote portions of header. */

/*        08-OCT-2009 (NJB) */

/*           Re-ordered header sections. */

/*        23-SEP-2004 (EDW) */

/*           Recast equation to improve runtime performance. */
/*           Edited comments for clarity. */

/*           Subroutine renamed from GETXYZ. */

/*        19-FEB-2000 (JAB) */

/*           Removed SAVED variables. */

/*        04-FEB-1999 (JAB) */

/* -& */
/* $ Index_Entries */

/*     given a voxel's coordinates return the voxel index */

/* -& */

/*     Convert from voxel coordinates to voxel index. A more */
/*     readable form for the following function: */

/*        NX   = NVOX(1) */
/*        NY   = NVOX(2) */
/*        NXNY = NX * NY */

/*        ZZVOX2ID = VIXYZ(1) + (VIXYZ(2)-1)*NX + (VIXYZ(3)-1)*NXNY */

/*     Expressing the function in this format improves runtime */
/*     performance as per Horner's Rule. */

    ret_val = vixyz[0] + nvox[0] * (vixyz[1] - 1 + (vixyz[2] - 1) * nvox[1]);
    return ret_val;
} /* zzvox2id_ */

