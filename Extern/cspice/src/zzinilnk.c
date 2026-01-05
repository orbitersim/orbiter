/* zzinilnk.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      ZZINILNK ( Initialize an AB cell linked-list ) */
/* Subroutine */ int zzinilnk_(integer *maxp, integer *maxc, integer *ncell, 
	integer *pntrs, integer *cells)
{
    /* System generated locals */
    integer i__1;

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

/*     Initialize an AB cell linked-list structure. */

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

/*     None */

/* $ Keywords */

/*     DSK */
/*     Utility */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     MAXP       I   Length of A-value array. */
/*     MAXC       I   Number of cells. */
/*     NCELL      O   Number of cells in use; initialized to zero. */
/*     PNTRS      O   Initialized pointer array. */
/*     CELLS      O   Initialized cell array. */

/* $ Detailed_Input */

/*     MAXP       is the length of the pointer array PNTRS. */

/*     MAXC       is the length of the cell array CELLS. The array has */
/*                dimensions */

/*                   ( 2, MAXC ) */

/*                Elements */

/*                   ( 1, * ) */

/*                normally contain "A-values"; elements */

/*                   ( 2, * ) */

/*                normally contain pointers. */


/* $ Detailed_Output */

/*     NCELL      is the number of cells in use; this argument is */
/*                initialized to zero. This is done as a convenience */
/*                for the calling routine, which usually must keep */
/*                track of the number of cells in use. */

/*     PNTRS      is the pointer array, initialized so that each */
/*                element contains -1, which represents a null */
/*                pointer. */

/*     CELLS      is a cell array, initialized so that each pointer */
/*                element contains -1, and so that each value */
/*                element contains 0. */

/* $ Parameters */

/*     See the DSK type 2 include file */

/*        dsk02.inc */

/* $ Exceptions */

/*     1)  If the pointer array size MAXP is non-positive, the */
/*         error SPICE(VALUEOUTOFRANGE) is signaled. */

/*     2)  If the cell array size MAXC is non-positive, the */
/*         error SPICE(VALUEOUTOFRANGE) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine supports creation of DSK type 2 segments. */
/*     It is used for creation of both voxel-plate association */
/*     data structures and of vertex-plate association data */
/*     structures. */

/* $ Examples */

/*     See usage in ZZMKSPIN. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */
/*     J.A. Bytof      (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 08-FEB-2017 (NJB) */

/*        Updated version info. */

/*        05-JAN-2016 (NJB) */

/*           Added error checks on input dimension arguments. Re-wrote */
/*           some of the argument descriptions. Added private routine */
/*           warning to header abstract. */

/*        08-OCT-2009 (NJB) */

/*           Re-ordered header sections. */

/*        03-FEB-1999 (JAB) */

/* -& */
/* $ Index_Entries */

/*     initialize an AB cell linked-list */

/* -& */
    if (return_()) {
	return 0;
    }
    chkin_("ZZINILNK", (ftnlen)8);
    if (*maxp < 1) {
	setmsg_("Pointer array size MAXP = #; size must be positive.", (
		ftnlen)51);
	errint_("#", maxp, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("ZZINILNK", (ftnlen)8);
	return 0;
    }
    if (*maxc < *maxp) {
	setmsg_("Cell array size MAXC = #; size must be at least as large as"
		" pointer array size #.", (ftnlen)81);
	errint_("#", maxc, (ftnlen)1);
	errint_("#", maxp, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("ZZINILNK", (ftnlen)8);
	return 0;
    }

/*     Initialize pointer array and cells. */

    i__1 = *maxp;
    for (i__ = 1; i__ <= i__1; ++i__) {
	pntrs[i__ - 1] = -1;
    }
    i__1 = *maxc;
    for (i__ = 1; i__ <= i__1; ++i__) {
	cells[(i__ << 1) - 2] = 0;
	cells[(i__ << 1) - 1] = -1;
    }

/*     Set count of cells in use to 0, for the convenience */
/*     of the calling routine. */

    *ncell = 0;
    chkout_("ZZINILNK", (ftnlen)8);
    return 0;
} /* zzinilnk_ */

