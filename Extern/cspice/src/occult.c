/* occult.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__100 = 100;

/* $Procedure OCCULT ( find occultation type at time ) */
/* Subroutine */ int occult_(char *targ1, char *shape1, char *frame1, char *
	targ2, char *shape2, char *frame2, char *abcorr, char *obsrvr, 
	doublereal *et, integer *ocltid, ftnlen targ1_len, ftnlen shape1_len, 
	ftnlen frame1_len, ftnlen targ2_len, ftnlen shape2_len, ftnlen 
	frame2_len, ftnlen abcorr_len, ftnlen obsrvr_len)
{
    /* Initialized data */

    static char occtyp[9*3] = "PARTIAL  " "ANNULAR  " "FULL     ";

    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    char back[36];
    extern /* Subroutine */ int zzgfocin_(char *, char *, char *, char *, 
	    char *, char *, char *, char *, char *, ftnlen, ftnlen, ftnlen, 
	    ftnlen, ftnlen, ftnlen, ftnlen, ftnlen, ftnlen);
    char shap1[500], shap2[500];
    extern /* Subroutine */ int zzgfocst_(doublereal *, logical *);
    integer i__;
    extern /* Subroutine */ int zzprsmet_(integer *, char *, integer *, char *
	    , char *, logical *, integer *, integer *, char *, char *, ftnlen,
	     ftnlen, ftnlen, ftnlen, ftnlen);
    char bname[36], fname[36];
    extern /* Subroutine */ int chkin_(char *, ftnlen), ucase_(char *, char *,
	     ftnlen, ftnlen), errch_(char *, char *, ftnlen, ftnlen);
    integer index;
    logical found;
    char front[36];
    integer nsurf;
    extern /* Subroutine */ int ljust_(char *, char *, ftnlen, ftnlen), 
	    bods2c_(char *, integer *, logical *, ftnlen);
    logical ellps2;
    char prshp1[9], prshp2[9];
    extern logical failed_(void);
    char bframe[32], fframe[32], bshape[500], bmethd[500], fmethd[500], 
	    fshape[500];
    integer mltfac;
    char pntdef[20];
    extern logical return_(void);
    char subtyp[20];
    integer id1, id2, srflst[100];
    logical ocstat;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), sigerr_(char *, 
	    ftnlen), chkout_(char *, ftnlen);
    char trmtyp[20];
    extern /* Subroutine */ int errint_(char *, integer *, ftnlen);
    logical pri;

/* $ Abstract */

/*     Determine the occultation condition (not occulted, partially */
/*     occulted, etc.) of one target relative to another target as seen */
/*     by an observer at a given time. */

/*     The surfaces of the target bodies may be represented by triaxial */
/*     ellipsoids, points, or by topographic data provided by DSK files. */

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

/*     KERNEL */
/*     SPK */
/*     TIME */

/* $ Keywords */

/*     ELLIPSOID */
/*     GEOMETRY */
/*     OCCULTATION */

/* $ Declarations */
/* $ Abstract */

/*     This file contains public, global parameter declarations */
/*     for the SPICELIB Geometry Finder (GF) subsystem. */

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

/*     GF */

/* $ Keywords */

/*     GEOMETRY */
/*     ROOT */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman      (JPL) */
/*     L.E. Elson        (JPL) */
/*     E.D. Wright       (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 2.0.0  29-NOV-2016 (NJB) */

/*        Upgraded to support surfaces represented by DSKs. */

/*        Bug fix: removed declaration of NVRMAX parameter. */

/* -    SPICELIB Version 1.3.0, 01-OCT-2011 (NJB) */

/*       Added NWILUM parameter. */

/* -    SPICELIB Version 1.2.0, 14-SEP-2010 (EDW) */

/*       Added NWPA parameter. */

/* -    SPICELIB Version 1.1.0, 08-SEP-2009 (EDW) */

/*       Added NWRR parameter. */
/*       Added NWUDS parameter. */

/* -    SPICELIB Version 1.0.0, 21-FEB-2009 (NJB) (LSE) (EDW) */

/* -& */

/*     Root finding parameters: */

/*     CNVTOL is the default convergence tolerance used by the */
/*     high-level GF search API routines. This tolerance is */
/*     used to terminate searches for binary state transitions: */
/*     when the time at which a transition occurs is bracketed */
/*     by two times that differ by no more than CNVTOL, the */
/*     transition time is considered to have been found. */

/*     Units are TDB seconds. */


/*     NWMAX is the maximum number of windows allowed for user-defined */
/*     workspace array. */

/*        DOUBLE PRECISION      WORK   ( LBCELL : MW, NWMAX ) */

/*     Currently no more than twelve windows are required; the three */
/*     extra windows are spares. */

/*     Callers of GFEVNT can include this file and use the parameter */
/*     NWMAX to declare the second dimension of the workspace array */
/*     if necessary. */


/*     Callers of GFIDST should declare their workspace window */
/*     count using NWDIST. */


/*     Callers of GFSEP should declare their workspace window */
/*     count using NWSEP. */


/*     Callers of GFRR should declare their workspace window */
/*     count using NWRR. */


/*     Callers of GFUDS should declare their workspace window */
/*     count using NWUDS. */


/*     Callers of GFPA should declare their workspace window */
/*     count using NWPA. */


/*     Callers of GFILUM should declare their workspace window */
/*     count using NWILUM. */


/*     ADDWIN is a parameter used to expand each interval of the search */
/*     (confinement) window by a small amount at both ends in order to */
/*     accommodate searches using equality constraints. The loaded */
/*     kernel files must accommodate these expanded time intervals. */


/*     FRMNLN is a string length for frame names. */


/*     FOVTLN -- maximum length for FOV string. */


/*     Specify the character strings that are allowed in the */
/*     specification of field of view shapes. */


/*     Character strings that are allowed in the */
/*     specification of occultation types: */


/*     Occultation target shape specifications: */


/*     Specify the number of supported occultation types and occultation */
/*     type string length: */


/*     Instrument field-of-view (FOV) parameters */

/*     Maximum number of FOV boundary vectors: */


/*     FOV shape parameters: */

/*        circle */
/*        ellipse */
/*        polygon */
/*        rectangle */


/*     End of file gf.inc. */

/* $ Abstract */

/*     This file contains public, global parameter declarations */
/*     for the SPICELIB occultation routines. */

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

/*     ELLIPSOID */
/*     GEOMETRY */
/*     OCCULTATION */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     S.C. Krening      (JPL) */
/*     N.J. Bachman      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 24-JAN-2012 (SCK) (NJB) */

/* -& */

/*     The following integer codes indicate the geometric relationship */
/*     of the three bodies. */

/*     The meaning of the sign of each code is given below. */

/*                    Code sign          Meaning */
/*                    ---------          ------------------------------ */
/*                       > 0             The second ellipsoid is */
/*                                       partially or fully occulted */
/*                                       by the first. */

/*                       < 0             The first ellipsoid is */
/*                                       partially of fully */
/*                                       occulted by the second. */

/*                       = 0             No occultation. */

/*     The meanings of the codes are given below. The variable names */
/*     indicate the type of occultation and which target is in the back. */
/*     For example, TOTAL1 represents a total occultation in which the */
/*     first target is in the back (or occulted by) the second target. */

/*                    Name      Code     Meaning */
/*                    ------    -----    ------------------------------ */
/*                    TOTAL1     -3      Total occultation of first */
/*                                       target by second. */

/*                    ANNLR1     -2      Annular occultation of first */
/*                                       target by second.  The second */
/*                                       target does not block the limb */
/*                                       of the first. */

/*                    PARTL1     -1      Partial occultation of first */
/*                                       target by second target. */

/*                    NOOCC       0      No occultation or transit:  both */
/*                                       objects are completely visible */
/*                                       to the observer. */

/*                    PARTL2      1      Partial occultation of second */
/*                                       target by first target. */

/*                    ANNLR2      2      Annular occultation of second */
/*                                       target by first. */

/*                    TOTAL2      3      Total occultation of second */
/*                                       target by first. */


/*     End include file occult.inc */


/*     File: dsk.inc */


/*     Version 1.0.0 05-FEB-2016 (NJB) */

/*     Maximum size of surface ID list. */


/*     End of include file dsk.inc */


/*     File: zzdsk.inc */


/*     Version 4.0.0 13-NOV-2015 (NJB) */

/*        Changed parameter LBTLEN to CVTLEN. */
/*        Added parameter LMBCRV. */

/*     Version 3.0.0 05-NOV-2015 (NJB) */

/*        Added parameters */

/*           CTRCOR */
/*           ELLCOR */
/*           GUIDED */
/*           LBTLEN */
/*           PNMBRL */
/*           TANGNT */
/*           TMTLEN */
/*           UMBRAL */

/*     Version 2.0.0 04-MAR-2015 (NJB) */

/*        Removed declaration of parameter SHPLEN. */
/*        This name is already in use in the include */
/*        file gf.inc. */

/*     Version 1.0.0 26-JAN-2015 (NJB) */


/*     Parameters supporting METHOD string parsing: */


/*     Local method length. */


/*     Length of sub-point type string. */


/*     Length of curve type string. */


/*     Limb type parameter codes. */


/*     Length of terminator type string. */


/*     Terminator type and limb parameter codes. */


/*     Length of aberration correction locus string. */


/*     Aberration correction locus codes. */


/*     End of include file zzdsk.inc */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     TARG1      I   Name or ID of first target. */
/*     SHAPE1     I   Type of shape model used for first target. */
/*     FRAME1     I   Body-fixed, body-centered frame for first body. */
/*     TARG2      I   Name or ID of second target. */
/*     SHAPE2     I   Type of shape model used for second target. */
/*     FRAME2     I   Body-fixed, body-centered frame for second body. */
/*     ABCORR     I   Aberration correction flag. */
/*     OBSRVR     I   Name or ID of the observer. */
/*     ET         I   Time of the observation (seconds past J2000). */
/*     OCLTID     O   Occultation identification code. */

/* $ Detailed_Input */

/*     TARG1    is the name of the first target body. Both object */
/*              names and NAIF IDs are accepted. For example, both */
/*              'Moon' and '301' are accepted. */

/*     SHAPE1   is a string indicating the geometric model used to */
/*              represent the shape of the first target body. The */
/*              supported options are: */

/*                 'ELLIPSOID' */

/*                     Use a triaxial ellipsoid model with radius */
/*                     values provided via the kernel pool. A kernel */
/*                     variable having a name of the form */

/*                        'BODYnnn_RADII' */

/*                     where nnn represents the NAIF integer code */
/*                     associated with the body, must be present in */
/*                     the kernel pool. This variable must be */
/*                     associated with three numeric values giving the */
/*                     lengths of the ellipsoid's X, Y, and Z */
/*                     semi-axes. */

/*                 'POINT' */

/*                     Treat the body as a single point. When a point */
/*                     target is specified, the occultation conditions */
/*                     can only be total, annular, or none. */

/*                 'DSK/UNPRIORITIZED[/SURFACES = <surface list>]' */

/*                     Use topographic data provided by DSK files to */
/*                     model the body's shape. These data must be */
/*                     provided by loaded DSK files. */

/*                     The surface list specification is optional. The */
/*                     syntax of the list is */

/*                        <surface 1> [, <surface 2>...] */

/*                     If present, it indicates that data only for the */
/*                     listed surfaces are to be used; however, data */
/*                     need not be available for all surfaces in the */
/*                     list. If absent, loaded DSK data for any surface */
/*                     associated with the target body are used. */

/*                     The surface list may contain surface names or */
/*                     surface ID codes. Names containing blanks must */
/*                     be delimited by double quotes, for example */

/*                        SURFACES = "Mars MEGDR 128 PIXEL/DEG" */

/*                     If multiple surfaces are specified, their names */
/*                     or IDs must be separated by commas. */

/*                     See the $Particulars section below for details */
/*                     concerning use of DSK data. */

/*              The combinations of the shapes of the target bodies */
/*              TARG1 and TARG2 must be one of: */

/*                 One ELLIPSOID, one POINT */
/*                 Two ELLIPSOIDs */
/*                 One DSK, one POINT */

/*              Case and leading or trailing blanks are not */
/*              significant in the string SHAPE1. */

/*     FRAME1   is the name of the body-fixed, body-centered reference */
/*              frame associated with the first target body. Examples */
/*              of such names are 'IAU_SATURN' (for Saturn) and */
/*              'ITRF93' (for the Earth). */

/*              If the first target body is modeled as a point, FRAME1 */
/*              should be left blank (Ex: ' '). */

/*              Case and leading or trailing blanks bracketing a */
/*              non-blank frame name are not significant in the string. */

/*     TARG2    is the name of the second target body. See the */
/*              description of TARG1 above for more details. */

/*     SHAPE2   is the shape specification for the body designated */
/*              by TARG2. See the description of SHAPE1 above for */
/*              details. */

/*     FRAME2   is the name of the body-fixed, body-centered reference */
/*              frame associated with the second target body. See the */
/*              description of FRAME1 above for more details. */

/*     ABCORR   indicates the aberration corrections to be applied to */
/*              the state of each target body to account for one-way */
/*              light time. Stellar aberration corrections are */
/*              ignored if specified, since these corrections don't */
/*              improve the accuracy of the occultation determination. */

/*              See the header of the SPICE routine SPKEZR for a */
/*              detailed description of the aberration correction */
/*              options. For convenience, the options supported by */
/*              this routine are listed below: */

/*                 'NONE'     Apply no correction. */

/*                 'LT'       "Reception" case: correct for */
/*                            one-way light time using a Newtonian */
/*                            formulation. */

/*                 'CN'       "Reception" case: converged */
/*                            Newtonian light time correction. */

/*                 'XLT'      "Transmission" case: correct for */
/*                            one-way light time using a Newtonian */
/*                            formulation. */

/*                 'XCN'      "Transmission" case: converged */
/*                            Newtonian light time correction. */

/*              Case and blanks are not significant in the string */
/*              ABCORR. */

/*     OBSRVR   is the name of the body from which the occultation */
/*              is observed. See the description of TARG1 above for */
/*              more details. */

/*     ET       is the observation time in seconds past the J2000 */
/*              epoch. */

/* $ Detailed_Output */

/*     OCLTID   is an integer occultation code indicating the geometric */
/*              relationship of the three bodies. */

/*              The meaning of the sign of OCLTID is given below. */

/*                  Code sign          Meaning */
/*                  ---------          ------------------------------ */
/*                     > 0             The second target is */
/*                                     partially or fully occulted */
/*                                     by the first. */

/*                     < 0             The first target is */
/*                                     partially of fully */
/*                                     occulted by the second. */

/*                     = 0             No occultation. */

/*              Possible OCLTID values and meanings are given below. */
/*              The variable names indicate the type of occultation */
/*              and which target is in the back. For example, TOTAL1 */
/*              represents a total occultation in which the first */
/*              target is in the back of (or is occulted by) the */
/*              second target. */

/*              When the target shapes are DSK and POINT, the only */
/*              possible occultation conditions are total, annular, */
/*              or none. */

/*                  Name      Code     Meaning */
/*                  ------    -----    ------------------------------ */
/*                  TOTAL1     -3      Total occultation of first */
/*                                     target by second. */

/*                  ANNLR1     -2      Annular occultation of first */
/*                                     target by second. If the second */
/*                                     target shape is an ellipsoid, */
/*                                     it does not block the limb of */
/*                                     the first. */

/*                  PARTL1     -1      Partial occultation of first */
/*                                     target by second target. */

/*                  NOOCC       0      No occultation or transit: both */
/*                                     objects are completely visible */
/*                                     to the observer. */

/*                  PARTL2      1      Partial occultation of second */
/*                                     target by first target. */

/*                  ANNLR2      2      Annular occultation of second */
/*                                     target by first. */

/*                  TOTAL2      3      Total occultation of second */
/*                                     target by first. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the target or observer body names input by the user are */
/*         not recognized, an error is signaled by a routine in */
/*         the call tree of this routine. */

/*     2)  If the input shapes are not accepted, an error is signaled by */
/*         a routine in the call tree of this routine. */

/*     3)  If both input shapes are points, an error is signaled by a */
/*         routine in the call tree of this routine. */

/*     4)  If the radii of a target body modeled as an ellipsoid cannot */
/*         be determined by searching the kernel pool for a kernel */
/*         variable having a name of the form */

/*            'BODYnnn_RADII' */

/*         where nnn represents the NAIF integer code associated with */
/*         the body, an error is signaled by a routine in the */
/*         call tree of this routine. */

/*     5)  If any of the target or observer bodies (TARG1, TARG2, or */
/*         OBSRVR) are the same, an error is signaled */
/*         by a routine in the call tree of this routine. */

/*     6)  If the loaded kernels provide insufficient data to compute any */
/*         required state vector, an error is signaled by a routine in */
/*         the call tree of this routine. */

/*     7)  If an error occurs while reading an SPK or other kernel, */
/*         the error is signaled by a routine in the call tree */
/*         of this routine. */

/*     8)  If the aberration correction specification ABCORR is invalid, */
/*         an error is signaled by a routine in the call tree of this */
/*         routine. */

/*     9)  If either SHAPE1 or SHAPE2 specifies that the target surface */
/*         is represented by DSK data, and no DSK files are loaded for */
/*         the specified target, an error is signaled by a routine in */
/*         the call tree of this routine. */

/*     10) If either SHAPE1 or SHAPE2 specifies that the target surface */
/*         is represented by DSK data, but the shape specification is */
/*         invalid, an error is signaled by a routine in the call tree */
/*         of this routine. */

/* $ Files */

/*     Appropriate SPICE kernels must be loaded by the calling program */
/*     before this routine is called. */

/*     The following data are required: */

/*     -  SPK data: the calling application must load ephemeris data */
/*        for the target, source and observer that cover the time */
/*        instant specified by the argument ET. If aberration */
/*        corrections are used, the states of the target bodies and of */
/*        the observer relative to the solar system barycenter must be */
/*        calculable from the available ephemeris data. Typically */
/*        ephemeris data */
/*        are made available by loading one or more SPK files via */
/*        FURNSH. */

/*     -  PCK data: bodies modeled as triaxial ellipsoids must have */
/*        semi-axis lengths provided by variables in the kernel pool. */
/*        Typically these data are made available by loading a text */
/*        PCK file via FURNSH. */

/*     -  FK data: if either of the reference frames designated by */
/*        FRAME1 or FRAME2 are not built in to the SPICE system, */
/*        one or more FKs specifying these frames must be loaded. */

/*     The following data may be required: */

/*     -  DSK data: if either SHAPE1 or SHAPE2 indicates that DSK */
/*        data are to be used, DSK files containing topographic data */
/*        for the target body must be loaded. If a surface list is */
/*        specified, data for at least one of the listed surfaces must */
/*        be loaded. */

/*     -  Surface name-ID associations: if surface names are specified */
/*        in SHAPE1 or SHAPE2, the association of these names with */
/*        their corresponding surface ID codes must be established by */
/*        assignments of the kernel variables */

/*           NAIF_SURFACE_NAME */
/*           NAIF_SURFACE_CODE */
/*           NAIF_SURFACE_BODY */

/*        Normally these associations are made by loading a text */
/*        kernel containing the necessary assignments. An example */
/*        of such a set of assignments is */

/*           NAIF_SURFACE_NAME += 'Mars MEGDR 128 PIXEL/DEG' */
/*           NAIF_SURFACE_CODE += 1 */
/*           NAIF_SURFACE_BODY += 499 */

/*     -  CK data: either of the body-fixed frames to which FRAME1 or */
/*        FRAME2 refer might be a CK frame. If so, at least one CK */
/*        file will be needed to permit transformation of vectors */
/*        between that frame and the J2000 frame. */

/*     -  SCLK data: if a CK file is needed, an associated SCLK */
/*        kernel is required to enable conversion between encoded SCLK */
/*        (used to time-tag CK data) and barycentric dynamical time */
/*        (TDB). */

/*     Kernel data are normally loaded once per program run, NOT every */
/*     time this routine is called. */

/* $ Particulars */

/*     This routine supports the target shape combinations */

/*        POINT     - ELLIPSOID */
/*        POINT     - DSK */
/*        ELLIPSOID - ELLIPSOID */

/*     For many purposes, modeling extended bodies as triaxial */
/*     ellipsoids is adequate for determining whether one body is */
/*     occulted by another as seen from a specified observer. */


/*     Using DSK data */
/*     ============== */

/*        DSK loading and unloading */
/*        ------------------------- */

/*        DSK files providing data used by this routine are loaded by */
/*        calling FURNSH and can be unloaded by calling UNLOAD or */
/*        KCLEAR. See the documentation of FURNSH for limits on numbers */
/*        of loaded DSK files. */

/*        For run-time efficiency, it's desirable to avoid frequent */
/*        loading and unloading of DSK files. When there is a reason to */
/*        use multiple versions of data for a given target body---for */
/*        example, if topographic data at varying resolutions are to be */
/*        used---the surface list can be used to select DSK data to be */
/*        used for a given computation. It is not necessary to unload */
/*        the data that are not to be used. This recommendation presumes */
/*        that DSKs containing different versions of surface data for a */
/*        given body have different surface ID codes. */


/*        DSK data priority */
/*        ----------------- */

/*        A DSK coverage overlap occurs when two segments in loaded DSK */
/*        files cover part or all of the same domain---for example, a */
/*        given longitude-latitude rectangle---and when the time */
/*        intervals of the segments overlap as well. */

/*        When DSK data selection is prioritized, in case of a coverage */
/*        overlap, if the two competing segments are in different DSK */
/*        files, the segment in the DSK file loaded last takes */
/*        precedence. If the two segments are in the same file, the */
/*        segment located closer to the end of the file takes */
/*        precedence. */

/*        When DSK data selection is unprioritized, data from competing */
/*        segments are combined. For example, if two competing segments */
/*        both represent a surface as sets of triangular plates, the */
/*        union of those sets of plates is considered to represent the */
/*        surface. */

/*        Currently only unprioritized data selection is supported. */
/*        Because prioritized data selection may be the default behavior */
/*        in a later version of the routine, the UNPRIORITIZED keyword is */
/*        required in the SHAPE1 and SHAPE2 arguments. */


/*        Syntax of the shape input arguments for the DSK case */
/*        ---------------------------------------------------- */

/*        The keywords and surface list in the target shape arguments */
/*        SHAPE1 and SHAPE2, when DSK shape models are specified, are */
/*        called "clauses." The clauses may appear in any order, for */
/*        example */

/*           DSK/<surface list>/UNPRIORITIZED */
/*           DSK/UNPRIORITIZED/<surface list> */
/*           UNPRIORITIZED/<surface list>/DSK */

/*        The simplest form of a target argument specifying use of */
/*        DSK data is one that lacks a surface list, for example: */

/*           'DSK/UNPRIORITIZED' */

/*        For applications in which all loaded DSK data for the target */
/*        body are for a single surface, and there are no competing */
/*        segments, the above string suffices. This is expected to be */
/*        the usual case. */

/*        When, for the specified target body, there are loaded DSK */
/*        files providing data for multiple surfaces for that body, the */
/*        surfaces to be used by this routine for a given call must be */
/*        specified in a surface list, unless data from all of the */
/*        surfaces are to be used together. */

/*        The surface list consists of the string */

/*           SURFACES = */

/*        followed by a comma-separated list of one or more surface */
/*        identifiers. The identifiers may be names or integer codes in */
/*        string format. For example, suppose we have the surface */
/*        names and corresponding ID codes shown below: */

/*           Surface Name                              ID code */
/*           ------------                              ------- */
/*           'Mars MEGDR 128 PIXEL/DEG'                1 */
/*           'Mars MEGDR 64 PIXEL/DEG'                 2 */
/*           'Mars_MRO_HIRISE'                         3 */

/*        If data for all of the above surfaces are loaded, then */
/*        data for surface 1 can be specified by either */

/*           'SURFACES = 1' */

/*        or */

/*           'SURFACES = "Mars MEGDR 128 PIXEL/DEG"' */

/*        Double quotes are used to delimit the surface name because */
/*        it contains blank characters. */

/*        To use data for surfaces 2 and 3 together, any */
/*        of the following surface lists could be used: */

/*           'SURFACES = 2, 3' */

/*           'SURFACES = "Mars MEGDR  64 PIXEL/DEG", 3' */

/*           'SURFACES = 2, Mars_MRO_HIRISE' */

/*           'SURFACES = "Mars MEGDR 64 PIXEL/DEG", Mars_MRO_HIRISE' */

/*        An example of a shape argument that could be constructed */
/*        using one of the surface lists above is */

/*              'DSK/UNPRIORITIZED/SURFACES = ' */
/*           // '"Mars MEGDR 64 PIXEL/DEG", 499003' */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */


/*     1) Find whether MRO is occulted by Mars as seen by */
/*        the DSS-13 ground station at a few specific */
/*        times. */

/*        Use the meta-kernel shown below to load the required SPICE */
/*        kernels. */


/*           KPL/MK */

/*           File: occult_ex1.tm */

/*           This meta-kernel is intended to support operation of SPICE */
/*           example programs. The kernels shown here should not be */
/*           assumed to contain adequate or correct versions of data */
/*           required by SPICE-based user applications. */

/*           In order for an application to use this meta-kernel, the */
/*           kernels referenced here must be present in the user's */
/*           current working directory. */

/*           The names and contents of the kernels referenced */
/*           by this meta-kernel are as follows: */

/*              File name                        Contents */
/*              ---------                        -------- */
/*              de410.bsp                        Planetary ephemeris */
/*              mar063.bsp                       Mars satellite ephemeris */
/*              pck00010.tpc                     Planet orientation and */
/*                                               radii */
/*              naif0011.tls                     Leapseconds */
/*              earth_latest_high_prec.bpc       Earth latest binary PCK */
/*              earthstns_itrf93_050714.bsp      DSN station SPK */
/*              mro_psp35.bsp                    MRO ephemeris */
/*              megr90n000cb_plate.bds           Plate model based on */
/*                                               MEGDR DEM, resolution */
/*                                               4 pixels/degree. */

/*           \begindata */

/*              KERNELS_TO_LOAD = ( 'de410.bsp', */
/*                                  'mar063.bsp', */
/*                                  'mro_psp34.bsp', */
/*                                  'earthstns_itrf93_050714.bsp', */
/*                                  'earth_latest_high_prec.bpc', */
/*                                  'pck00010.tpc', */
/*                                  'naif0011.tls', */
/*                                  'megr90n000cb_plate.bds' */
/*                                ) */
/*           \begintext */

/*           End of meta-kernel */


/*        Example code begins here. */


/*              PROGRAM OCCULT_EX1 */
/*              IMPLICIT NONE */

/*              INCLUDE              'occult.inc' */
/*        C */
/*        C     Local parameters */
/*        C */
/*              CHARACTER*(*)         META */
/*              PARAMETER           ( META  = 'occult_ex1.tm' ) */

/*              CHARACTER*(*)         FRMT */
/*              PARAMETER           ( FRMT  = '(A18,A5,A21,A5,A4,A6)' ) */

/*              INTEGER               CHSIZ */
/*              PARAMETER           ( CHSIZ = 30 ) */

/*        C */
/*        C     Local variables */
/*        C */
/*              CHARACTER*(CHSIZ)     ABCORR */
/*              CHARACTER*(CHSIZ)     FORM */
/*              CHARACTER*(CHSIZ)     OBSRVR */
/*              CHARACTER*(CHSIZ)     SHAPE1 */
/*              CHARACTER*(CHSIZ)     SHAPE2 */
/*              CHARACTER*(CHSIZ)     SHAPES ( 2 ) */
/*              CHARACTER*(CHSIZ)     TARG1 */
/*              CHARACTER*(CHSIZ)     TARG2 */
/*              CHARACTER*(CHSIZ)     TIME */
/*              CHARACTER*(CHSIZ)     TSTART */
/*              CHARACTER*(CHSIZ)     TEND */
/*              CHARACTER*(CHSIZ)     OUTCH ( 4 ) */

/*              DOUBLE PRECISION      ET */
/*              DOUBLE PRECISION      ET1 */
/*              DOUBLE PRECISION      ETEND */

/*              INTEGER               DT */
/*              INTEGER               I */
/*              INTEGER               OCLTID */

/*        C */
/*        C     Saved variables */
/*        C */
/*              SAVE                  OUTCH */
/*              SAVE                  SHAPES */
/*        C */
/*        C     Initial values */
/*        C */
/*              DATA OUTCH ( 1 ) / 'totally occulted by'   / */
/*              DATA OUTCH ( 2 ) / 'transited by'          / */
/*              DATA OUTCH ( 3 ) / 'partially occulted by' / */
/*              DATA OUTCH ( 4 ) / 'not occulted by'       / */

/*              DATA SHAPES      / 'ELLIPSOID', */
/*             .                   'DSK/UNPRIORITIZED' / */

/*        C */
/*        C     Initialize the time range. Set the output time */
/*        C     format to PST. Set DT to an hour interval in */
/*        C     units of seconds. */
/*        C */

/*              TSTART = '2015-FEB-28 1:15:00 UTC' */
/*              TEND   = '2015-FEB-28 2:50:00 UTC' */
/*              FORM   = 'YYYY-MON-DD HR:MN ::UTC-8' */
/*              DT     = 1000 */

/*        C */
/*        C     Initialize the targets, observer, and aberration */
/*        C     correction. */
/*        C */
/*              TARG1  = 'MRO' */
/*              SHAPE1 = 'POINT' */
/*              TARG2  = 'MARS' */
/*              OBSRVR = 'DSS-13' */
/*              ABCORR = 'CN' */

/*        C */
/*        C     Load kernel files via the meta-kernel. */
/*        C */
/*              CALL FURNSH ( META ) */
/*        C */
/*        C     Calculate the start and stop times in ET. */
/*        C */
/*              CALL STR2ET ( TSTART, ET1   ) */
/*              CALL STR2ET ( TEND,   ETEND ) */


/*              DO I = 1, 2 */
/*        C */
/*        C        Set the planet shape model for this pass. */
/*        C */
/*                 SHAPE2 = SHAPES(I) */

/*                 WRITE (*,*) ' ' */
/*                 CALL TOSTDO ( 'Mars shape: '//SHAPE2 ) */
/*                 WRITE (*,*) ' ' */

/*                 ET = ET1 */
/*                 DO WHILE ( ET .LT. ETEND ) */
/*        C */
/*        C           Calculate the type of occultation that */
/*        C           corresponds to time ET. */
/*        C */
/*                    CALL OCCULT ( TARG1,  SHAPE1, ' ', */
/*             .                    TARG2,  SHAPE2, 'IAU_MARS', */
/*             .                    ABCORR, OBSRVR,  ET, OCLTID ) */
/*        C */
/*        C           Output the results. */
/*        C */
/*                    CALL TIMOUT ( ET, FORM, TIME ) */

/*                    IF ( OCLTID .EQ. TOTAL1 ) THEN */
/*                       WRITE (*,FRMT) TIME, TARG1, OUTCH(1), TARG2, */
/*             .                        'wrt ', OBSRVR */

/*                    ELSEIF ( OCLTID .EQ. ANNLR1 ) THEN */
/*                       WRITE (*,FRMT) TIME, TARG1, OUTCH(2), TARG2, */
/*             .                        'wrt ', OBSRVR */

/*                    ELSEIF ( OCLTID .EQ. PARTL1 ) THEN */
/*                       WRITE (*,FRMT) TIME, TARG1, OUTCH(3), TARG2, */
/*             .                        'wrt ', OBSRVR, */
/*             .                        'NOT POSSIBLE FOR POINT' */

/*                    ELSEIF ( OCLTID .EQ. NOOCC ) THEN */
/*                       WRITE (*,FRMT) TIME, TARG1, OUTCH(4), TARG2, */
/*             .                        'wrt ', OBSRVR */

/*                    ELSEIF ( OCLTID .EQ. PARTL2 ) THEN */
/*                       WRITE (*,FRMT) TIME, TARG2, OUTCH(3), TARG1, */
/*             .                        'wrt ', OBSRVR, */
/*             .                        'NOT POSSIBLE FOR POINT' */

/*                    ELSEIF ( OCLTID .EQ. ANNLR2 ) THEN */
/*                       WRITE (*,FRMT) TIME, TARG2, OUTCH(2), TARG1, */
/*             .                        'wrt ', OBSRVR */

/*                    ELSEIF ( OCLTID .EQ. TOTAL2 ) THEN */
/*                       WRITE (*,FRMT) TIME, TARG2, OUTCH(1), TARG1, */
/*             .                        'wrt ', OBSRVR */

/*                    ELSE */
/*                       WRITE (*,*) 'Bad occultation ID:  ', OCLTID */

/*                    END IF */
/*        C */
/*        C           Increment the time. */
/*        C */
/*                    ET = ET + DT */

/*                 END DO */

/*              END DO */
/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        Mars shape: ELLIPSOID */

/*        2015-FEB-27 17:15 MARS transited by         MRO  wrt DSS-13 */
/*        2015-FEB-27 17:31 MRO  not occulted by      MARS wrt DSS-13 */
/*        2015-FEB-27 17:48 MRO  totally occulted by  MARS wrt DSS-13 */
/*        2015-FEB-27 18:04 MRO  totally occulted by  MARS wrt DSS-13 */
/*        2015-FEB-27 18:21 MRO  not occulted by      MARS wrt DSS-13 */
/*        2015-FEB-27 18:38 MARS transited by         MRO  wrt DSS-13 */

/*        Mars shape: DSK/UNPRIORITIZED */

/*        2015-FEB-27 17:15 MARS transited by         MRO  wrt DSS-13 */
/*        2015-FEB-27 17:31 MRO  not occulted by      MARS wrt DSS-13 */
/*        2015-FEB-27 17:48 MRO  totally occulted by  MARS wrt DSS-13 */
/*        2015-FEB-27 18:04 MRO  totally occulted by  MARS wrt DSS-13 */
/*        2015-FEB-27 18:21 MRO  not occulted by      MARS wrt DSS-13 */
/*        2015-FEB-27 18:38 MARS transited by         MRO  wrt DSS-13 */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     S.C. Krening       (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.1, 26-OCT-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. Extended */
/*        $Abstract description. */

/*        Edited meta-kernel and code example to comply with NAIF */
/*        standards for Example sections. */

/* -    SPICELIB Version 2.0.0, 21-FEB-2017 (NJB) */

/*        Added FAILED tests. */

/*        01-MAR-2016 (NJB) */

/*        Upgraded to support surfaces represented by DSKs. Updated */
/*        example program to show use of DSKs. Updated example */
/*        meta-kernel. Corrected various comment typos. */

/* -    SPICELIB Version 1.0.0, 14-NOV-2013 (SCK) (NJB) */

/* -& */
/* $ Index_Entries */

/*     occultation type at a specified time */

/* -& */

/*     SPICELIB functions */


/*     External routines */


/*     Local parameters */


/*     Local variables */


/*     Saved variables */


/*     The variable OCCTYP associates the string of an occultation */
/*     type (from gf.inc) with its positive integer code (from */
/*     occult.inc).  The variable OCCTYP is set up so each string is */
/*     stored at the index relating to that configuration's positive */
/*     integer code.  The positive integer codes assume the first */
/*     target is occulting (in front of) the second target. */

/*                 Ex:  PARTL2 = 1               (from occult.inc) */
/*                      OCCTYP ( 1 ) = 'PARTIAL' (from gf.inc) */

/*     The table below shows the relation between each index of OCCTYP, */
/*     the occultation condition, which target is in front and back, the */
/*     multiplication factor, and the output integer occultation code. */
/*     Note that the output integer occultation code is the integer index */
/*     of OCCTYP multiplied by the multiplication factor. */

/*                 OCLTID = Index * MLTFAC */

/*     MLTFAC is 1 if TARG1 is in front, and -1 if TARG1 is in back. */
/*     The setup of OCCTYP could be changed, but it is important to keep */
/*     the output integer occultation codes consistent with the values */
/*     from occult.inc. */

/*         Index   Occult. Condition   Front   Back   MLTFAC  OCLTID */
/*         -----   -----------------   -----   -----  ------  ------ */
/*           1          Partial        TARG1   TARG2    1       1 */
/*           1          Partial        TARG2   TARG1   -1      -1 */
/*           2          Annular        TARG1   TARG2    1       2 */
/*           2          Annular        TARG2   TARG1   -1      -2 */
/*           3          Total          TARG1   TARG2    1       3 */
/*           3          Total          TARG2   TARG1   -1      -3 */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("OCCULT", (ftnlen)6);

/*     Left justify the shapes and target names and make them upper case. */

    ljust_(shape1, shap1, shape1_len, (ftnlen)500);
    ucase_(shap1, shap1, (ftnlen)500, (ftnlen)500);
    ljust_(shape2, shap2, shape2_len, (ftnlen)500);
    ucase_(shap2, shap2, (ftnlen)500, (ftnlen)500);
    ljust_(targ1, fname, targ1_len, (ftnlen)36);
    ucase_(fname, fname, (ftnlen)36, (ftnlen)36);
    ljust_(targ2, bname, targ2_len, (ftnlen)36);
    ucase_(bname, bname, (ftnlen)36, (ftnlen)36);

/*     The variable ELLPS2 is a flag that indicates whether both targets */
/*     are represented as ellipsoids. */

    ellps2 = s_cmp(shap1, "ELLIPSOID", (ftnlen)500, (ftnlen)9) == 0 && s_cmp(
	    shap2, "ELLIPSOID", (ftnlen)500, (ftnlen)9) == 0;

/*     Parse the input shapes. We need the target ID codes */
/*     for this. */

    if (s_cmp(shap1, "POINT", (ftnlen)500, (ftnlen)5) == 0) {
	s_copy(prshp1, shap1, (ftnlen)9, (ftnlen)9);
    } else {
	bods2c_(fname, &id1, &found, (ftnlen)36);
	if (! found) {
	    setmsg_("First target name # could not be mapped to an ID code.", 
		    (ftnlen)54);
	    errch_("#", fname, (ftnlen)1, (ftnlen)36);
	    sigerr_("SPICE(IDCODENOTFOUND)", (ftnlen)21);
	    chkout_("OCCULT", (ftnlen)6);
	    return 0;
	}
	zzprsmet_(&id1, shap1, &c__100, prshp1, subtyp, &pri, &nsurf, srflst, 
		pntdef, trmtyp, (ftnlen)500, (ftnlen)9, (ftnlen)20, (ftnlen)
		20, (ftnlen)20);
	if (failed_()) {
	    chkout_("OCCULT", (ftnlen)6);
	    return 0;
	}
    }
    if (s_cmp(shap2, "POINT", (ftnlen)500, (ftnlen)5) == 0) {
	s_copy(prshp2, shap2, (ftnlen)9, (ftnlen)9);
    } else {
	bods2c_(bname, &id2, &found, (ftnlen)36);
	if (! found) {
	    setmsg_("Second target name # could not be mapped to an ID code.",
		     (ftnlen)55);
	    errch_("#", bname, (ftnlen)1, (ftnlen)36);
	    sigerr_("SPICE(IDCODENOTFOUND)", (ftnlen)21);
	    chkout_("OCCULT", (ftnlen)6);
	    return 0;
	}
	zzprsmet_(&id2, shap2, &c__100, prshp2, subtyp, &pri, &nsurf, srflst, 
		pntdef, trmtyp, (ftnlen)500, (ftnlen)9, (ftnlen)20, (ftnlen)
		20, (ftnlen)20);
	if (failed_()) {
	    chkout_("OCCULT", (ftnlen)6);
	    return 0;
	}
    }

/*     Test two main cases: */

/*     1) The first target is the front body. */
/*     2) The second target is the front body. */

/*     First, initialize the occultation code to reflect no occultation. */

    *ocltid = 0;
    for (i__ = 1; i__ <= 2; ++i__) {

/*        The first time through, make the first target the */
/*        front. On the second time, make the second target the front. */
/*        For details on the variable MLTFAC, please see the detailed */
/*        explanation of the OCCTYP variable near the start of the code. */

	if (i__ == 1) {
	    s_copy(front, fname, (ftnlen)36, (ftnlen)36);
	    s_copy(fmethd, shap1, (ftnlen)500, (ftnlen)500);
	    s_copy(bmethd, shap2, (ftnlen)500, (ftnlen)500);
	    s_copy(fshape, prshp1, (ftnlen)500, (ftnlen)9);
	    s_copy(fframe, frame1, (ftnlen)32, frame1_len);
	    s_copy(back, bname, (ftnlen)36, (ftnlen)36);
	    s_copy(bshape, prshp2, (ftnlen)500, (ftnlen)9);
	    s_copy(bframe, frame2, (ftnlen)32, frame2_len);
	    mltfac = 1;
	} else {
	    s_copy(front, bname, (ftnlen)36, (ftnlen)36);
	    s_copy(fmethd, shap2, (ftnlen)500, (ftnlen)500);
	    s_copy(bmethd, shap1, (ftnlen)500, (ftnlen)500);
	    s_copy(fshape, prshp2, (ftnlen)500, (ftnlen)9);
	    s_copy(fframe, frame2, (ftnlen)32, frame2_len);
	    s_copy(back, fname, (ftnlen)36, (ftnlen)36);
	    s_copy(bshape, prshp1, (ftnlen)500, (ftnlen)9);
	    s_copy(bframe, frame1, (ftnlen)32, frame1_len);
	    mltfac = -1;
	}

/*        Check if there is any occultation with the current front/back */
/*        configuration. ZZGFOCIN performs initializations. ZZGFOCST */
/*        returns a true/false logical indicating if there is an */
/*        occultation. */

	zzgfocin_("ANY", front, fmethd, fframe, back, bmethd, bframe, obsrvr, 
		abcorr, (ftnlen)3, (ftnlen)36, (ftnlen)500, (ftnlen)32, (
		ftnlen)36, (ftnlen)500, (ftnlen)32, obsrvr_len, abcorr_len);
	zzgfocst_(et, &ocstat);
	if (failed_()) {
	    chkout_("OCCULT", (ftnlen)6);
	    return 0;
	}

/*        If there was an occultation, and both targets are represented */
/*        as ellipsoids, test the three types of occultations: partial, */
/*        annular, and full. Note: If the integer parameters within */
/*        occult.inc are changed, the following DO loop will need to be */
/*        altered. */

	if (ocstat) {

/*           An occultation exists. */

	    if (ellps2) {

/*              Both shapes are ellipsoids. */

		for (index = 1; index <= 3; ++index) {
		    zzgfocin_(occtyp + ((i__1 = index - 1) < 3 && 0 <= i__1 ? 
			    i__1 : s_rnge("occtyp", i__1, "occult_", (ftnlen)
			    1080)) * 9, front, fshape, fframe, back, bshape, 
			    bframe, obsrvr, abcorr, (ftnlen)9, (ftnlen)36, (
			    ftnlen)500, (ftnlen)32, (ftnlen)36, (ftnlen)500, (
			    ftnlen)32, obsrvr_len, abcorr_len);
		    zzgfocst_(et, &ocstat);
		    if (failed_()) {
			chkout_("OCCULT", (ftnlen)6);
			return 0;
		    }

/*                 If the occultation condition is true, return the */
/*                 integer occultation ID code. */

		    if (ocstat) {
			*ocltid = mltfac * index;
			chkout_("OCCULT", (ftnlen)6);
			return 0;
		    }

/*                 End the DO loop that checks the occultation type. */

		}
	    } else if (s_cmp(fshape, "ELLIPSOID", (ftnlen)500, (ftnlen)9) == 
		    0 || s_cmp(fshape, "DSK", (ftnlen)500, (ftnlen)3) == 0) {

/*              The front target is an ellipsoid or DSK shape: this */
/*              is a total occultation. (Other target is a point). */

		*ocltid = mltfac * 3;
		chkout_("OCCULT", (ftnlen)6);
		return 0;
	    } else if (s_cmp(bshape, "ELLIPSOID", (ftnlen)500, (ftnlen)9) == 
		    0 || s_cmp(bshape, "DSK", (ftnlen)500, (ftnlen)3) == 0) {

/*              The back target is an ellipsoid or DSK shape: this is an */
/*              annular occultation. (Other target is a point). */

		*ocltid = mltfac << 1;
		chkout_("OCCULT", (ftnlen)6);
		return 0;
	    }
	}

/*        End the DO loop that checks the front/back orientation of */
/*        the input targets. */

    }

/*     If the occultation searches show that there was no occultation */
/*     at the given time, return an occultation code that indicates */
/*     no occultation. If this part of the code has been reached and */
/*     the occultation code indicates an occultation was found, an error */
/*     has occurred. */

    if (*ocltid != 0) {
	setmsg_("This error should never be reached; the occultation code re"
		"sult # is invalid.", (ftnlen)77);
	errint_("#", ocltid, (ftnlen)1);
	sigerr_("SPICE(BUG)", (ftnlen)10);
	chkout_("OCCULT", (ftnlen)6);
	return 0;
    }
    chkout_("OCCULT", (ftnlen)6);
    return 0;
} /* occult_ */

