/* dsktol.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__6 = 6;

/* $Procedure DSKTOL ( DSK, tolerance umbrella ) */
/* Subroutine */ int dsktol_0_(int n__, integer *keywrd, doublereal *dpval)
{
    /* Initialized data */

    static doublereal dppars[6] = { 1e-10,1e-8,1e-10,1e-7,1e-12,1e-12 };
    static logical isfixd[6] = { FALSE_,FALSE_,FALSE_,FALSE_,TRUE_,TRUE_ };
    static char names[6*6] = "XFRACT" "SGREED" "SGPADM" "PTMEMM" "ANGMRG" 
	    "LONALI";

    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen), sigerr_(char *, ftnlen), chkout_(char *, ftnlen)
	    , setmsg_(char *, ftnlen), errint_(char *, integer *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Umbrella routine for DSK tolerance and margin parameter access */
/*     routines. */

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
/*     MARGIN */
/*     NUMERIC */
/*     TOLERANCE */

/* $ Declarations */

/*     File: dsktol.inc */


/*     This file contains declarations of tolerance and margin values */
/*     used by the DSK subsystem. */

/*     It is recommended that the default values defined in this file be */
/*     changed only by expert SPICE users. */

/*     The values declared in this file are accessible at run time */
/*     through the routines */

/*        DSKGTL  {DSK, get tolerance value} */
/*        DSKSTL  {DSK, set tolerance value} */

/*     These are entry points of the routine DSKTOL. */

/*        Version 1.0.0 27-FEB-2016 (NJB) */




/*     Parameter declarations */
/*     ====================== */

/*     DSK type 2 plate expansion factor */
/*     --------------------------------- */

/*     The factor XFRACT is used to slightly expand plates read from DSK */
/*     type 2 segments in order to perform ray-plate intercept */
/*     computations. */

/*     This expansion is performed to prevent rays from passing through */
/*     a target object without any intersection being detected. Such */
/*     "false miss" conditions can occur due to round-off errors. */

/*     Plate expansion is done by computing the difference vectors */
/*     between a plate's vertices and the plate's centroid, scaling */
/*     those differences by (1 + XFRACT), then producing new vertices by */
/*     adding the scaled differences to the centroid. This process */
/*     doesn't affect the stored DSK data. */

/*     Plate expansion is also performed when surface points are mapped */
/*     to plates on which they lie, as is done for illumination angle */
/*     computations. */

/*     This parameter is user-adjustable. */


/*     The keyword for setting or retrieving this factor is */


/*     Greedy segment selection factor */
/*     ------------------------------- */

/*     The factor SGREED is used to slightly expand DSK segment */
/*     boundaries in order to select segments to consider for */
/*     ray-surface intercept computations. The effect of this factor is */
/*     to make the multi-segment intercept algorithm consider all */
/*     segments that are sufficiently close to the ray of interest, even */
/*     if the ray misses those segments. */

/*     This expansion is performed to prevent rays from passing through */
/*     a target object without any intersection being detected. Such */
/*     "false miss" conditions can occur due to round-off errors. */

/*     The exact way this parameter is used is dependent on the */
/*     coordinate system of the segment to which it applies, and the DSK */
/*     software implementation. This parameter may be changed in a */
/*     future version of SPICE. */


/*     The keyword for setting or retrieving this factor is */


/*     Segment pad margin */
/*     ------------------ */

/*     The segment pad margin is a scale factor used to determine when a */
/*     point resulting from a ray-surface intercept computation, if */
/*     outside the segment's boundaries, is close enough to the segment */
/*     to be considered a valid result. */

/*     This margin is required in order to make DSK segment padding */
/*     (surface data extending slightly beyond the segment's coordinate */
/*     boundaries) usable: if a ray intersects the pad surface outside */
/*     the segment boundaries, the pad is useless if the intercept is */
/*     automatically rejected. */

/*     However, an excessively large value for this parameter is */
/*     detrimental, since a ray-surface intercept solution found "in" a */
/*     segment can supersede solutions in segments farther from the */
/*     ray's vertex. Solutions found outside of a segment thus can mask */
/*     solutions that are closer to the ray's vertex by as much as the */
/*     value of this margin, when applied to a segment's boundary */
/*     dimensions. */

/*     The keyword for setting or retrieving this factor is */


/*     Surface-point membership margin */
/*     ------------------------------- */

/*     The surface-point membership margin limits the distance */
/*     between a point and a surface to which the point is */
/*     considered to belong. The margin is a scale factor applied */
/*     to the size of the segment containing the surface. */

/*     This margin is used to map surface points to outward */
/*     normal vectors at those points. */

/*     If this margin is set to an excessively small value, */
/*     routines that make use of the surface-point mapping won't */
/*     work properly. */


/*     The keyword for setting or retrieving this factor is */


/*     Angular rounding margin */
/*     ----------------------- */

/*     This margin specifies an amount by which angular values */
/*     may deviate from their proper ranges without a SPICE error */
/*     condition being signaled. */

/*     For example, if an input latitude exceeds pi/2 radians by a */
/*     positive amount less than this margin, the value is treated as */
/*     though it were pi/2 radians. */

/*     Units are radians. */


/*     This parameter is not user-adjustable. */

/*     The keyword for retrieving this parameter is */


/*     Longitude alias margin */
/*     ---------------------- */

/*     This margin specifies an amount by which a longitude */
/*     value can be outside a given longitude range without */
/*     being considered eligible for transformation by */
/*     addition or subtraction of 2*pi radians. */

/*     A longitude value, when compared to the endpoints of */
/*     a longitude interval, will be considered to be equal */
/*     to an endpoint if the value is outside the interval */
/*     differs from that endpoint by a magnitude less than */
/*     the alias margin. */


/*     Units are radians. */


/*     This parameter is not user-adjustable. */

/*     The keyword for retrieving this parameter is */


/*     End of include file dsktol.inc */

/* $ Brief_I/O */

/*     VARIABLE  I/O  ENTRY POINTS */
/*     --------  ---  -------------------------------------------------- */
/*     KEYWRD     I   DSKGTL, DSKSTL */
/*     DPVAL     I-O  DSKGTL, DSKSTL */

/* $ Detailed_Input */

/*     See the entry points for descriptions of their input arguments. */

/* $ Detailed_Output */

/*     See the entry points for descriptions of their output arguments. */

/* $ Parameters */

/*     See the include file */

/*        dsktol.inc */

/*     for descriptions and values of the tolerance or margin parameters */
/*     accessed by the entry points of this routine, and of the keyword */
/*     parameters used to refer to them. */

/* $ Exceptions */

/*     1)  If this routine is called directly, the error */
/*         SPICE(BOGUSENTRY) is signaled. */

/*     2)  See the entry points for descriptions of exceptions specific */
/*         to those entry points. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The routines in this package serve to centralize numeric */
/*     tolerance and margin values used by the DSK subsystem. The */
/*     subsystem retrieves values from this package to use at run time. */

/*     The entry points of this routine are */

/*        DSKGTL {DSK, get tolerance value} */
/*        DSKSTL {DSK, set tolerance value} */

/*     To minimize run time overhead, the "keywords" used by these */
/*     routines to identify parameters are actually integer codes. */

/*     SPICE users may override certain values maintained by this */
/*     package; others values are fixed. */

/*     One use of this system would be to disable the "greedy" */
/*     algorithms used to prevent "false miss" ray-surface intercepts */
/*     results. Setting to zero the margins used for this purpose would */
/*     accomplish this. */

/*     It is recommended that any change to the tolerance values made at */
/*     run time be programmed only by SPICE experts. */

/* $ Examples */

/*     See the entry points. */

/* $ Restrictions */

/*     1)  The default settings used by the DSK subsystem should */
/*         be overridden only by expert SPICE users. */

/*     2)  The entry points of this routine do not check the */
/*         validity of new parameter values supplied by the */
/*         calling application. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 09-JUL-2020 (JDR) */

/*        Edited the entry points and umbrella headers to comply with */
/*        NAIF standard. */

/* -    SPICELIB Version 1.0.0, 01-AUG-2016 (NJB) */

/* -& */
/* $ Index_Entries */

/*     DSK tolerance and margin umbrella */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Saved values */


/*     Initial values */

/*     Caution: this array must be declared in */
/*     the order defined by the keywords declared */
/*     in dsktol.inc. */

    switch(n__) {
	case 1: goto L_dskgtl;
	case 2: goto L_dskstl;
	}

    chkin_("DSKTOL", (ftnlen)6);
    sigerr_("SPICE(BOGUSENTRY)", (ftnlen)17);
    chkout_("DSKTOL", (ftnlen)6);
    return 0;
/* $Procedure DSKGTL ( DSK, get tolerance ) */

L_dskgtl:
/* $ Abstract */

/*     Retrieve the value of a specified DSK tolerance or margin */
/*     parameter. */

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
/*     MARGIN */
/*     NUMERIC */
/*     TOLERANCE */

/* $ Declarations */

/*     INTEGER               KEYWRD */
/*     DOUBLE PRECISION      DPVAL */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     KEYWRD     I   Code specifying parameter to retrieve. */
/*     DPVAL      O   Value of parameter. */

/* $ Detailed_Input */

/*     KEYWRD   is an integer code specifying the parameter to */
/*              retrieve. See the include file dsktol.inc for */
/*              a description of the possible keywords. */

/* $ Detailed_Output */

/*     DPVAL    is the value of the parameter specified by KEYWRD. */

/* $ Parameters */

/*     See the include file */

/*        dsktol.inc */

/*     for descriptions and values of the tolerance or margin parameters */
/*     accessed by this routine, and of the keyword parameters used to */
/*     refer to them. */

/* $ Exceptions */

/*     1)  If the input keyword is not recognized, the error */
/*         SPICE(INDEXOUTOFRANGE) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The routines in this package serve to centralize numeric */
/*     tolerance and margin values used by the DSK subsystem. */
/*     The subsystem retrieves values from this package to use */
/*     at run time. */

/*     The entry points of this routine are */

/*        DSKGTL {DSK, get tolerance value} */
/*        DSKSTL {DSK, set tolerance value} */

/*     To minimize run time overhead, the "keywords" used by */
/*     these routines to identify parameters are actually */
/*     integer codes. */

/*     SPICE users may override certain values maintained by */
/*     this package; others values are fixed. It is recommended */
/*     that any change to the tolerance values made at run */
/*     time be performed only by expert SPICE users. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Obtain and display the DSK type 2 plate expansion fraction. */


/*        Example code begins here. */


/*              PROGRAM DSKGTL_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Include files */
/*        C */
/*              INCLUDE 'dsktol.inc' */

/*        C */
/*        C     Local variables */
/*        C */
/*              DOUBLE PRECISION      DPVAL */

/*              CALL DSKGTL ( KEYXFR, DPVAL ) */

/*              WRITE(*,*) 'Plate expansion fraction = ', DPVAL */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Plate expansion fraction =    1.0000000000000000E-010 */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 09-JUL-2020 (JDR) */

/*        Updated the header to comply with NAIF standard. Added */
/*        complete example code. */

/* -    SPICELIB Version 1.0.0, 01-AUG-2016 (NJB) */

/* -& */
/* $ Index_Entries */

/*     retrieve DSK tolerance or margin parameters */

/* -& */

/*     Use discovery check-in. */

    if (*keywrd < 1 || *keywrd > 6) {
	chkin_("DSKGTL", (ftnlen)6);
	setmsg_("Valid keyword range is 1:#; keyword was #.", (ftnlen)42);
	errint_("#", &c__6, (ftnlen)1);
	errint_("#", keywrd, (ftnlen)1);
	sigerr_("SPICE(INDEXOUTOFRANGE)", (ftnlen)22);
	chkout_("DSKGTL", (ftnlen)6);
	return 0;
    }
    *dpval = dppars[(i__1 = *keywrd - 1) < 6 && 0 <= i__1 ? i__1 : s_rnge(
	    "dppars", i__1, "dsktol_", (ftnlen)411)];
    return 0;
/* $Procedure DSKSTL ( DSK, set tolerance ) */

L_dskstl:
/* $ Abstract */

/*     Set the value of a specified DSK tolerance or margin parameter. */

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
/*     MARGIN */
/*     NUMERIC */
/*     TOLERANCE */

/* $ Declarations */

/*     INTEGER               KEYWRD */
/*     DOUBLE PRECISION      DPVAL */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     KEYWRD     I   Code specifying parameter to modify. */
/*     DPVAL      I   New value of parameter. */

/* $ Detailed_Input */

/*     KEYWRD   is an integer code specifying the parameter to */
/*              set. See the include file dsktol.inc for */
/*              a description of the possible keywords. */

/*     DPVAL    is the new value of the parameter specified by KEYWRD. */
/*              This value will be retrieved by future calls to */
/*              to DSKGTL made to retrieve the specified parameter. */

/*              <<< Use extreme caution. This routine performs no */
/*              checks on DPVAL. >>> */

/* $ Detailed_Output */

/*     None. */

/*     This routine operates by side effects. */

/* $ Parameters */

/*     See the include file */

/*        dsktol.inc */

/*     for descriptions and values of the tolerance or margin parameters */
/*     accessed by this routine, and of the keyword parameters used to */
/*     refer to them. */

/* $ Exceptions */

/*     1)  If the input keyword is not recognized, the error */
/*         SPICE(INDEXOUTOFRANGE) is signaled. */

/*     2)  If an attempt is made to modify a fixed parameter, */
/*         the error SPICE(IMMUTABLEVALUE) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The routines in this package serve to centralize numeric */
/*     tolerance and margin values used by the DSK subsystem. */
/*     The subsystem retrieves values from this package to use */
/*     at run time. */

/*     The entry points of this routine are */

/*        DSKGTL {DSK, get tolerance value} */
/*        DSKSTL {DSK, set tolerance value} */

/*     To minimize run time overhead, the "keywords" used by */
/*     these routines to identify parameters are actually */
/*     integer codes. */

/*     SPICE users may override certain values maintained by */
/*     this package; others values are fixed. It is recommended */
/*     that any change to the tolerance values made at run */
/*     time be performed only by expert SPICE users. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Obtain, display, and update the DSK type 2 plate expansion */
/*        fraction. */


/*        Example code begins here. */


/*              PROGRAM DSKSTL_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Include files */
/*        C */
/*              INCLUDE 'dsktol.inc' */

/*        C */
/*        C     Local variables */
/*        C */
/*              DOUBLE PRECISION      DPVAL */

/*              CALL DSKGTL ( KEYXFR, DPVAL ) */

/*              WRITE(*,*) 'Default plate expansion fraction = ', DPVAL */

/*        C */
/*        C     Update the parameter. */
/*        C */
/*              CALL DSKSTL ( KEYXFR, 1.D-8 ) */

/*        C */
/*        C     Verify the update. */
/*        C */
/*              CALL DSKGTL ( KEYXFR, DPVAL ) */

/*              WRITE(*,*) 'New plate expansion fraction     = ', DPVAL */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Default plate expansion fraction =    1.0000000000000000E-010 */
/*         New plate expansion fraction     =    1.0000000000000000E-008 */


/* $ Restrictions */

/*     1)  The default settings used by the DSK subsystem should */
/*         be overridden only by expert SPICE users. */

/*     2)  This routine does not check the  validity of new parameter */
/*         values supplied by the calling application. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 09-JUL-2020 (JDR) */

/*        Updated the header to comply with NAIF standard. Added */
/*        complete example code. */

/* -    SPICELIB Version 1.0.0, 01-AUG-2016 (NJB) */

/* -& */
/* $ Index_Entries */

/*     set DSK tolerance or margin parameters */

/* -& */
    if (return_()) {
	return 0;
    }
    chkin_("DSKSTL", (ftnlen)6);
    if (*keywrd < 1 || *keywrd > 6) {
	setmsg_("Valid keyword range is 1:#; keyword was #.", (ftnlen)42);
	errint_("#", &c__6, (ftnlen)1);
	errint_("#", keywrd, (ftnlen)1);
	sigerr_("SPICE(INDEXOUTOFRANGE)", (ftnlen)22);
	chkout_("DSKSTL", (ftnlen)6);
	return 0;
    }
    if (isfixd[(i__1 = *keywrd - 1) < 6 && 0 <= i__1 ? i__1 : s_rnge("isfixd",
	     i__1, "dsktol_", (ftnlen)645)]) {
	setmsg_("The parameter # cannot be modified.", (ftnlen)35);
	errch_("#", names + ((i__1 = *keywrd - 1) < 6 && 0 <= i__1 ? i__1 : 
		s_rnge("names", i__1, "dsktol_", (ftnlen)648)) * 6, (ftnlen)1,
		 (ftnlen)6);
	sigerr_("SPICE(IMMUTABLEVALUE)", (ftnlen)21);
	chkout_("DSKSTL", (ftnlen)6);
	return 0;
    }

/*     We have a valid parameter index. We don't check */
/*     the new parameter value; the user presumably knows */
/*     the reason for change. */

    dppars[(i__1 = *keywrd - 1) < 6 && 0 <= i__1 ? i__1 : s_rnge("dppars", 
	    i__1, "dsktol_", (ftnlen)660)] = *dpval;
    chkout_("DSKSTL", (ftnlen)6);
    return 0;
} /* dsktol_ */

/* Subroutine */ int dsktol_(integer *keywrd, doublereal *dpval)
{
    return dsktol_0_(0, keywrd, dpval);
    }

/* Subroutine */ int dskgtl_(integer *keywrd, doublereal *dpval)
{
    return dsktol_0_(1, keywrd, dpval);
    }

/* Subroutine */ int dskstl_(integer *keywrd, doublereal *dpval)
{
    return dsktol_0_(2, keywrd, dpval);
    }

