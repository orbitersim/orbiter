/* zzsfxcor.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__0 = 0;
static integer c__1 = 1;
static doublereal c_b27 = 1e-14;

/* $Procedure ZZSFXCOR ( Ray-surface intercept core algorithm ) */
/* Subroutine */ int zzsfxcor_(S_fp udnear, S_fp udmaxr, S_fp udrayx, integer 
	*trgcde, doublereal *et, char *abcorr, logical *uselt, logical *usecn,
	 logical *usestl, logical *xmit, char *fixref, integer *obscde, 
	integer *dfrcde, integer *dclass, integer *dcentr, doublereal *dvec, 
	doublereal *spoint, doublereal *trgepc, doublereal *srfvec, logical *
	found, ftnlen abcorr_len, ftnlen fixref_len)
{
    /* Initialized data */

    static logical first = TRUE_;
    static char prvcor[5] = "     ";

    /* System generated locals */
    doublereal d__1, d__2;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int vadd_(doublereal *, doublereal *, doublereal *
	    );
    doublereal dist, udir[3];
    extern /* Subroutine */ int vscl_(doublereal *, doublereal *, doublereal *
	    );
    integer nitr;
    extern doublereal vsep_(doublereal *, doublereal *);
    extern /* Subroutine */ int vsub_(doublereal *, doublereal *, doublereal *
	    ), vequ_(doublereal *, doublereal *);
    doublereal rpos[3], tpos[3], j2dir[3], j2est[3], j2pos[3];
    integer i__;
    doublereal s, range;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    doublereal pnear[3];
    extern doublereal vdist_(doublereal *, doublereal *);
    doublereal xform[9]	/* was [3][3] */;
    extern doublereal vnorm_(doublereal *);
    extern logical vzero_(doublereal *);
    doublereal j2geom[3], r2jmat[9]	/* was [3][3] */, j2tmat[9]	/* 
	    was [3][3] */;
    extern logical failed_(void);
    extern /* Subroutine */ int refchg_(integer *, integer *, doublereal *, 
	    doublereal *);
    doublereal etdiff, lt;
    extern doublereal dasine_(doublereal *, doublereal *);
    doublereal refepc;
    extern doublereal clight_(void);
    doublereal ltdiff;
    static char loccor[5];
    extern doublereal touchd_(doublereal *);
    doublereal ltcent, maxrad, negpos[3], rayalt, reject, relerr, obspos[3], 
	    prevet, srflen, stldir[3], trgdir[3];
    extern logical return_(void);
    doublereal prevlt, ssbost[6], ssbtst[6], stlerr[3], stltmp[3];
    extern /* Subroutine */ int setmsg_(char *, ftnlen), sigerr_(char *, 
	    ftnlen), chkout_(char *, ftnlen), suffix_(char *, integer *, char 
	    *, ftnlen, ftnlen), spkezp_(integer *, doublereal *, char *, char 
	    *, integer *, doublereal *, doublereal *, ftnlen, ftnlen), 
	    vminus_(doublereal *, doublereal *), pxform_(char *, char *, 
	    doublereal *, doublereal *, ftnlen, ftnlen), spkssb_(integer *, 
	    doublereal *, char *, doublereal *, ftnlen), stelab_(doublereal *,
	     doublereal *, doublereal *), stlabx_(doublereal *, doublereal *, 
	    doublereal *), errint_(char *, integer *, ftnlen), nplnpt_(
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *), vhatip_(doublereal *), mxv_(doublereal *, 
	    doublereal *, doublereal *);

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     Find the intersection of a ray and the surface described by */
/*     an ellipsoid or DSK data, where the ray's vertex and the */
/*     target body's center are associated with ephemeris objects. */
/*     Use specified aberration corrections. Use callback routines */
/*     to carry out low-level ray-surface intercept and ray-surface */
/*     near point computations. */

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

/*     GEOMETRY */
/*     INTERCEPT */
/*     INTERSECTION */
/*     RAY */
/*     SURFACE */
/*     TOPOGRAPHY */

/* $ Declarations */
/* $ Abstract */

/*     The parameters below form an enumerated list of the recognized */
/*     frame types. They are: INERTL, PCK, CK, TK, DYN, SWTCH, and ALL. */
/*     The meanings are outlined below. */

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

/*     INERTL      an inertial frame that is listed in the routine */
/*                 CHGIRF and that requires no external file to */
/*                 compute the transformation from or to any other */
/*                 inertial frame. */

/*     PCK         is a frame that is specified relative to some */
/*                 INERTL frame and that has an IAU model that */
/*                 may be retrieved from the PCK system via a call */
/*                 to the routine TISBOD. */

/*     CK          is a frame defined by a C-kernel. */

/*     TK          is a "text kernel" frame.  These frames are offset */
/*                 from their associated "relative" frames by a */
/*                 constant rotation. */

/*     DYN         is a "dynamic" frame.  These currently are */
/*                 parameterized, built-in frames where the full frame */
/*                 definition depends on parameters supplied via a */
/*                 frame kernel. */

/*     SWTCH       is a "switch" frame. These frames have orientation */
/*                 defined by their alignment with base frames selected */
/*                 from a prioritized list. The base frames optionally */
/*                 have associated time intervals of applicability. */

/*     ALL         indicates any of the above classes. This parameter */
/*                 is used in APIs that fetch information about frames */
/*                 of a specified class. */


/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */
/*     B.V. Semenov    (JPL) */
/*     W.L. Taber      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 5.0.0, 08-OCT-2020 (NJB) (BVS) */

/*       The parameter SWTCH was added to support the switch */
/*       frame class. */

/* -    SPICELIB Version 4.0.0, 08-MAY-2012 (NJB) */

/*       The parameter ALL was added to support frame fetch APIs. */

/* -    SPICELIB Version 3.0.0, 28-MAY-2004 (NJB) */

/*       The parameter DYN was added to support the dynamic frame class. */

/* -    SPICELIB Version 2.0.0, 12-DEC-1996 (WLT) */

/*        Various unused frames types were removed and the */
/*        frame time TK was added. */

/* -    SPICELIB Version 1.0.0, 10-DEC-1995 (WLT) */

/* -& */

/*     End of INCLUDE file frmtyp.inc */

/* $ Abstract */

/*     Include file zzabcorr.inc */

/*     SPICE private file intended solely for the support of SPICE */
/*     routines.  Users should not include this file directly due */
/*     to the volatile nature of this file */

/*     The parameters below define the structure of an aberration */
/*     correction attribute block. */

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

/*     An aberration correction attribute block is an array of logical */
/*     flags indicating the attributes of the aberration correction */
/*     specified by an aberration correction string.  The attributes */
/*     are: */

/*        - Is the correction "geometric"? */

/*        - Is light time correction indicated? */

/*        - Is stellar aberration correction indicated? */

/*        - Is the light time correction of the "converged */
/*          Newtonian" variety? */

/*        - Is the correction for the transmission case? */

/*        - Is the correction relativistic? */

/*    The parameters defining the structure of the block are as */
/*    follows: */

/*       NABCOR    Number of aberration correction choices. */

/*       ABATSZ    Number of elements in the aberration correction */
/*                 block. */

/*       GEOIDX    Index in block of geometric correction flag. */

/*       LTIDX     Index of light time flag. */

/*       STLIDX    Index of stellar aberration flag. */

/*       CNVIDX    Index of converged Newtonian flag. */

/*       XMTIDX    Index of transmission flag. */

/*       RELIDX    Index of relativistic flag. */

/*    The following parameter is not required to define the block */
/*    structure, but it is convenient to include it here: */

/*       CORLEN    The maximum string length required by any aberration */
/*                 correction string */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 18-DEC-2004 (NJB) */

/* -& */
/*     Number of aberration correction choices: */


/*     Aberration correction attribute block size */
/*     (number of aberration correction attributes): */


/*     Indices of attributes within an aberration correction */
/*     attribute block: */


/*     Maximum length of an aberration correction string: */


/*     End of include file zzabcorr.inc */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     UDNEAR     I   Callback routine for ray-surface near point. */
/*     UDMAXR     I   Callback routine for target outer bounding surface. */
/*     UDRAYX     I   Callback routine for ray-surface intercept. */
/*     TRGCDE     I   Target body ID code. */
/*     ET         I   Epoch at observer. */
/*     ABCORR     I   Aberration correction string. */
/*     USELT      I   "Use light time" flag. */
/*     USECN      I   "Use converged Newtonian correction" flag. */
/*     USESTL     I   "Use stellar aberration" flag. */
/*     XMIT       I   "Perform transmission corrections" flag. */
/*     FIXREF     I   Name of target body-fixed frame. */
/*     OBSCDE     I   Observer body ID code. */
/*     DFRCDE     I   Ray direction frame ID code. */
/*     DCLASS     I   Ray direction frame class. */
/*     DCENTR     I   Ray direction frame center body ID code. */
/*     DVEC       I   Ray direction vector. */
/*     SPOINT     O   Surface intercept, if found. */
/*     TRGEPC     O   Target epoch, if intercept was found. */
/*     SRFVEC     O   Observer-to-intercept vector, if found. */
/*     FOUND      O   Found flag. */

/* $ Detailed_Input */

/*     UDNEAR     is an external routine that computes the nearest point */
/*                on a bounding surface to the input ray. The caller */
/*                must perform any initialization required by UDNEAR. */

/*                The calling sequence of UDNEAR is */

/*                    CALL UDNEAR ( VERTEX, RAYDIR, ET, PNEAR, DIST ) */

/*                    DOUBLE PRECISION      VERTEX(3) */
/*                    DOUBLE PRECISION      RAYDIR(3) */
/*                    DOUBLE PRECISION      ET */
/*                    DOUBLE PRECISION      PNEAR (3) */
/*                    DOUBLE PRECISION      DIST */


/*     UDMAXR     is an external routine that computes the radius of an */
/*                outer bounding surface for the target body. The caller */
/*                must perform any initialization required by UDMAXR. */

/*                The calling sequence of UDMAXR is */

/*                   CALL UDMAXR ( MAXRAD ) */

/*                   DOUBLE PRECISION      MAXRAD */


/*     UDRAYX     external routine that computes the intercept of the */
/*                input ray on the surface associated with the target */
/*                body. The surface may be represented by an ellipsoid */
/*                or by DSK data. The caller must perform any */
/*                initialization required by UDRAYX. */

/*                The calling sequence of UDRAYX is */

/*                    CALL UDRAYX ( VERTEX, RAYDIR, ET, SPOINT, FOUND ) */

/*                    DOUBLE PRECISION      VERTEX(3) */
/*                    DOUBLE PRECISION      RAYDIR(3) */
/*                    DOUBLE PRECISION      ET */
/*                    DOUBLE PRECISION      SPOINT(3) */
/*                    LOGICAL               FOUND */


/*     TRGCDE     is the body ID of the target. */

/*     ET         is the epoch of the intersection computation, */
/*                expressed as seconds past J2000 TDB. This epoch */
/*                applies to the observer. */

/*     ABCORR     is an aberration correction string. This string */
/*                is present in addition to the following flags */
/*                because it's used as an input to SPKEZP. */

/*     USELT      is a logical flag that is .TRUE. if and only if */
/*                light time corrections are to be performed. */

/*     USECN      is a logical flag that is .TRUE. if and only if */
/*                converged Newtonian light time corrections are to be */
/*                performed. */

/*     USESTL     is a logical flag that is .TRUE. if and only if */
/*                stellar aberration corrections are to be performed. */

/*     XMIT       is a logical flag that is .TRUE. if and only if */
/*                the aberration corrections are to be performed for */
/*                the transmission case. */

/*     FIXREF     is the name of a reference frame fixed to, and */
/*                centered on, the target body. */

/*     OBSCDE     is the body ID code of the observer. */

/*     DFRCDE     is the frame ID code of the frame in which the ray's */
/*                direction vector is expressed. This may be any frame */
/*                supported by the SPICE system, including built-in */
/*                frames (documented in the Frames Required Reading) and */
/*                frames defined by a loaded frame kernel (FK). The */
/*                string DREF is case-insensitive, and leading and */
/*                trailing blanks in DREF are not significant. */

/*                When DRFCDE designates a non-inertial frame, the */
/*                orientation of the frame is evaluated at an epoch */
/*                dependent on the frame's center and, if the center is */
/*                not the observer, on the selected aberration */
/*                correction. See the description of the direction */
/*                vector DVEC for details. */

/*     DCLASS     is the frame class of the frame designated by DFRCDE. */

/*     DCENTR     is the body ID code of the center of the frame */
/*                designated by DFRCDE. */

/*     DVEC       Ray direction vector emanating from the observer. The */
/*                intercept with the target body's surface of the ray */
/*                defined by the observer and DVEC is sought. */

/*                DVEC is specified relative to the reference frame */
/*                designated by DREF. */

/*                Non-inertial reference frames are treated as follows: */
/*                if the center of the frame is at the observer's */
/*                location, the frame is evaluated at ET. If the frame's */
/*                center is located elsewhere, then letting LTCENT be */
/*                the one-way light time between the observer and the */
/*                central body associated with the frame, the */
/*                orientation of the frame is evaluated at ET-LTCENT, */
/*                ET+LTCENT, or ET depending on whether the requested */
/*                aberration correction is, respectively, for received */
/*                radiation, transmitted radiation, or is omitted. */
/*                LTCENT is computed using the method indicated by */
/*                ABCORR. */


/* $ Detailed_Output */

/*     SPOINT      is the surface intercept point on the target body of */
/*                 the ray defined by the observer and the direction */
/*                 vector. If the ray intersects the target body in */
/*                 multiple points, the selected intersection point is */
/*                 the one closest to the observer. The output argument */
/*                 FOUND (see below) indicates whether an intercept was */
/*                 found. */

/*                 SPOINT is expressed in Cartesian coordinates, */
/*                 relative to the target body-fixed frame designated by */
/*                 FIXREF. The body-fixed target frame is evaluated at */
/*                 the intercept epoch TRGEPC (see description below). */

/*                 When light time correction is used, the duration of */
/*                 light travel between SPOINT to the observer is */
/*                 considered to be the one way light time. When both */
/*                 light time and stellar aberration corrections are */
/*                 used, SPOINT is compute such that, when the vector */
/*                 from the observer to SPOINT is corrected for light */
/*                 time and stellar aberration, the resulting vector */
/*                 lies on the ray defined by the observer's location */
/*                 and DVEC. */

/*                 The components of SPOINT are given in units of km. */


/*     TRGEPC      is the "intercept epoch." TRGEPC is defined as */
/*                 follows: letting LT be the one-way light time between */
/*                 the observer and the intercept point, TRGEPC is the */
/*                 epoch ET-LT, ET+LT, or ET depending on whether the */
/*                 requested aberration correction is, respectively, for */
/*                 received radiation, transmitted radiation, or */
/*                 omitted. LT is computed using the method indicated by */
/*                 ABCORR. */

/*                 TRGEPC is expressed as seconds past J2000 TDB. */


/*     SRFVEC      is the vector from the observer's position at ET to */
/*                 the aberration-corrected (or optionally, geometric) */
/*                 position of SPOINT, where the aberration corrections */
/*                 are specified by ABCORR. SRFVEC is expressed in the */
/*                 target body-fixed reference frame designated by */
/*                 FIXREF, evaluated at TRGEPC. */

/*                 The components of SRFVEC are given in units of km. */

/*                 One can use the SPICELIB function VNORM to obtain the */
/*                 distance between the observer and SPOINT: */

/*                    DIST = VNORM ( SRFVEC ) */

/*                 The observer's position OBSPOS, relative to the */
/*                 target body's center, where the center's position is */
/*                 corrected for aberration effects as indicated by */
/*                 ABCORR, can be computed via the call: */

/*                    CALL VSUB ( SPOINT, SRFVEC, OBSPOS ) */

/*                 To transform the vector SRFVEC from a reference frame */
/*                 FIXREF at time TRGEPC to a time-dependent reference */
/*                 frame REF at time ET, the routine PXFRM2 should be */
/*                 called. Let XFORM be the 3x3 matrix representing the */
/*                 rotation from the reference frame FIXREF at time */
/*                 TRGEPC to the reference frame REF at time ET. Then */
/*                 SRFVEC can be transformed to the result REFVEC as */
/*                 follows: */

/*                     CALL PXFRM2 ( FIXREF, REF,    TRGEPC, ET, XFORM ) */
/*                     CALL MXV    ( XFORM,  SRFVEC, REFVEC ) */

/*                 The second example in the Examples header section */
/*                 below presents a complete program that demonstrates */
/*                 this procedure. */


/*     FOUND       A logical flag indicating whether or not the ray */
/*                 intersects the target. If an intersection exists */
/*                 FOUND will be returned as .TRUE. If the ray misses */
/*                 the target, FOUND will be returned as .FALSE. */


/* $ Parameters */

/*     See the include file frmtyp.inc for frame class parameter */
/*     values. */

/* $ Exceptions */

/*     1)  If the observer-target distance is zero, the error */
/*         SPICE(NOSEPARATION) is signaled. */

/*     2)  If an error occurs while performing a SPICE kernel lookup, or */
/*         if an error occurs while performing a geometric computation, */
/*         the error SHOULD be signaled by a routine in the call tree of */
/*         this routine. However, see exception (3) below. */

/*     3)  This routine assumes robust input checking has been */
/*         performed by the caller. This routine may fail in */
/*         an unspecified manner if such checking has not */
/*         been performed. */

/* $ Files */

/*     Appropriate kernels must be loaded by the calling program before */
/*     this routine is called. */

/*     The following data are required: */

/*        - SPK data: ephemeris data for target and observer must be */
/*          loaded. If aberration corrections are used, the states of */
/*          target and observer relative to the solar system barycenter */
/*          must be calculable from the available ephemeris data. */
/*          Typically ephemeris data are made available by loading one */
/*          or more SPK files via FURNSH. */

/*        - PCK data: if the computation method is specified as */
/*          "Ellipsoid," triaxial radii for the target body must be */
/*          loaded into the kernel pool. Typically this is done by */
/*          loading a text PCK file via FURNSH. */

/*        - Further PCK data: rotation data for the target body must */
/*          be loaded. These may be provided in a text or binary PCK */
/*          file. */

/*     The following data may be required: */

/*        - DSK data: if METHOD indicates that DSK data are to be used, */
/*          DSK files containing topographic data for the target body */
/*          must be loaded. If a surface list is specified, data for */
/*          at least one of the listed surfaces must be loaded. */

/*        - Surface name-ID associations: if surface names are specified */
/*          in METHOD, the association of these names with their */
/*          corresponding surface ID codes must be established by */
/*          assignments of the kernel variables */

/*             NAIF_SURFACE_NAME */
/*             NAIF_SURFACE_CODE */
/*             NAIF_SURFACE_BODY */

/*          Normally these associations are made by loading a text */
/*          kernel containing the necessary assignments. An example */
/*          of such an assignment is */

/*             NAIF_SURFACE_NAME += 'Mars MEGDR 128 PIXEL/DEG' */
/*             NAIF_SURFACE_CODE += 1 */
/*             NAIF_SURFACE_BODY += 499 */

/*        - Frame data: if a frame definition is required to convert */
/*          the observer and target states to the body-fixed frame of */
/*          the target, that definition must be available in the kernel */
/*          pool. Similarly, the frame definition required to map */
/*          between the frame designated by DREF and the target */
/*          body-fixed frame must be available. Typically the */
/*          definitions of frames not already built-in to SPICE are */
/*          supplied by loading a frame kernel. */

/*        - CK data: if the frame to which DREF refers is fixed to a */
/*          spacecraft instrument or structure, at least one CK file */
/*          will be needed to permit transformation of vectors between */
/*          that frame and both the J2000 and the target body-fixed */
/*          frames. */

/*        - SCLK data: if a CK file is needed, an associated SCLK */
/*          kernel is required to enable conversion between encoded SCLK */
/*          (used to time-tag CK data) and barycentric dynamical time */
/*          (TDB). */

/*     In all cases, kernel data are normally loaded once per program */
/*     run, NOT every time this routine is called. */


/* $ Particulars */

/*     This routine implements the core ray-surface intercept algorithm */
/*     of the SPICELIB routine SINCPT. Parsing and analysis of inputs */
/*     is largely performed by the caller of this routine. */

/*     Certain operations performed by this routine, which formerly */
/*     were performed in-line by SINCPT, are now handled by callback */
/*     routines. These are: */

/*         - Computing a ray-surface intercept, where the ray's vertex */
/*           is expressed in a known target body-fixed frame and */
/*           represents an offset from the target body center (NOT */
/*           from the frame's center, as is the case in lower-level */
/*           routines. */

/*         - Computing the radius of an outer bounding surface for */
/*           the target body. */

/*         - Computing the nearest point to a ray on a specified */
/*           bounding surface for the target body. This computation */
/*           enables the intercept-observer light time to be estimated */
/*           for cases where the first iteration of the solution results */
/*           in non-intersection geometry. */

/*     By using these callback routines, ZZSFXCOR is able to use a */
/*     single logic case to compute ray-surface intercepts for targets */
/*     modeled as ellipsoids or as surfaces represented by topographic */
/*     data. */

/* $ Examples */

/*     See usage in SINCPT. */

/* $ Restrictions */

/*     1)  This is a private routine. It is meant to be used only by the */
/*         DSK subsystem. */

/*     2)  This routine relies extensively on the calling routine */
/*         to check the inputs passed to this routine. */

/*     3)  This routine relies on the calling routine to perform any */
/*         initialization functions required by the input */
/*         callback routines. */

/*     4)  If the direction vector DVEC is the zero vector, the error */
/*         SPICE(ZEROVECTOR) will be signaled. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.0, 14-JUN-2021 (NJB) */

/*        Updated logic for limb grazing geometry to reduce chance of */
/*        failing to find an intercept. */

/* -    SPICELIB Version 1.0.0, 21-FEB-2017 (NJB) */

/*        Added FAILED tests. */

/*        04-OCT-2016 (NJB) */

/*        Bug fix: initializes FOUND to .FALSE. */

/*        Bug fix: routine now allows observer to be inside */
/*        outer bounding sphere of target. */

/*        Re-named callback routines again. */

/*        01-JUN-2016 (NJB) */

/*           Updated names of callback routines to avoid overlap with */
/*           names of actual SPICELIB routines. */

/*        08-FEB-2016 (NJB) */

/*           Based on version 29-JAN-2015 (NJB) */

/*        Version 1.0.1 29-JAN-2015 (NJB) */

/*           Cleaned up debugging comments. */

/*        Version 1.0.0 15-OCT-2014 (NJB) */

/* -& */
/* $ Index_Entries */

/*     generalized ray-surface intercept */
/*     ray-surface intercept core algorithm */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 2.0.0, 14-JUN-2021 (NJB) */

/*        Updated logic for limb grazing geometry to reduce chance of */
/*        failing to find an intercept. */

/*        In the main solution loop, if the iteration count has not */
/*        reached the termination limit minus one, and if a call to */
/*        UDRAYX does produce an intercept, NPEDLN and NPLNPT are used */
/*        to find the tangent point. That point is used to produce a new */
/*        light time estimate. In some cases, this allows an intercept */
/*        to be found again in a later pass through the loop, and a */
/*        solution intercept to be found at that point or later. */
/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     This value will become system-dependent when systems */
/*     using 128-bit d.p. numbers are supported by SPICELIB. */
/*     CNVLIM, when added to 1.0D0, should yield 1.0D0. */


/*     Round-off error limit for arc sine input: */


/*     Fraction of target body angular radius used to define */
/*     region outside of which rays are immediately rejected */
/*     as non-intersecting. */


/*     Code for the frame J2000. */


/*     Local variables */


/*     Saved variables */


/*     Initial values */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("ZZSFXCOR", (ftnlen)8);

/*     Nothing found yet. */

    *found = FALSE_;

/*     Check for a zero ray direction vector. */

    if (vzero_(dvec)) {
	setmsg_("Input ray direction was the zero vector; this vector must b"
		"e non-zero.", (ftnlen)70);
	sigerr_("SPICE(ZEROVECTOR)", (ftnlen)17);
	chkout_("ZZSFXCOR", (ftnlen)8);
	return 0;
    }

/*     Get the sign S prefixing LT in the expression for TRGEPC. */
/*     When light time correction is not used, setting S = 0 */
/*     allows us to seamlessly set TRGEPC equal to ET. */

    if (*uselt) {
	if (*xmit) {
	    s = 1.;
	} else {
	    s = -1.;
	}
    } else {
	s = 0.;
    }
    if (first || s_cmp(abcorr, prvcor, abcorr_len, (ftnlen)5) != 0) {

/*        Construct aberration correction string without stellar */
/*        aberration specification. */

	if (*uselt) {
	    if (*xmit) {
		s_copy(loccor, "X", (ftnlen)5, (ftnlen)1);
	    } else {
		s_copy(loccor, " ", (ftnlen)5, (ftnlen)1);
	    }
	    if (*usecn) {
		suffix_("CN", &c__0, loccor, (ftnlen)2, (ftnlen)5);
	    } else {
		suffix_("LT", &c__0, loccor, (ftnlen)2, (ftnlen)5);
	    }
	} else {
	    s_copy(loccor, "NONE", (ftnlen)5, (ftnlen)4);
	}
	s_copy(prvcor, abcorr, (ftnlen)5, abcorr_len);
	first = FALSE_;
    }

/*     Determine the position of the observer in target */
/*     body-fixed coordinates. */

/*         -  Call SPKEZP to compute the position of the target body as */
/*            seen from the observing body and the light time (LT) */
/*            between them. We request that the coordinates of POS be */
/*            returned relative to the body fixed reference frame */
/*            associated with the target body, using aberration */
/*            corrections specified by LOCCOR; these are the corrections */
/*            the input argument ABCORR, minus the stellar aberration */
/*            correction if it was called for. */

/*         -  Call VMINUS to negate the direction of the vector (OBSPOS) */
/*            so it will be the position of the observer as seen from */
/*            the target body in target body fixed coordinates. */

/*            Note that this result is not the same as the result of */
/*            calling SPKEZP with the target and observer switched. We */
/*            computed the vector FROM the observer TO the target in */
/*            order to get the proper light time and stellar aberration */
/*            corrections (if requested). Now we need the inverse of */
/*            that corrected vector in order to compute the intercept */
/*            point. */

    spkezp_(trgcde, et, fixref, loccor, obscde, tpos, &lt, fixref_len, (
	    ftnlen)5);
    if (failed_()) {
	chkout_("ZZSFXCOR", (ftnlen)8);
	return 0;
    }

/*     Negate the target's position to obtain the position of the */
/*     observer relative to the target. */

    vminus_(tpos, obspos);

/*     We now need to convert the direction vector into the */
/*     body fixed frame associated with the target. The target */
/*     epoch is dependent on the aberration correction. The */
/*     coefficient S has been set to give us the correct answer */
/*     for each case. */

    *trgepc = *et + s * lt;

/*     Transform the direction vector from frame DREF to the body-fixed */
/*     frame associated with the target. The epoch TRGEPC associated */
/*     with the body-fixed frame has been set already. */

/*     We'll compute the transformation in two parts: first */
/*     from frame DREF to J2000, then from J2000 to the target */
/*     frame. */

    if (*dclass == 1) {

/*        Inertial frames can be evaluated at any epoch. */

	refepc = *et;
    } else if (! (*uselt)) {

/*        We're not using light time corrections (converged or */
/*        otherwise), so there's no time offset. */

	refepc = *et;
    } else if (*dcentr == *obscde) {

/*        If the center of frame DREF is the observer (which is */
/*        usually the case if the observer is a spacecraft), then */
/*        the epoch of frame DREF is simply ET. */

/*        There's no offset between the center for frame DREF */
/*        and the observer. */

	refepc = *et;
    } else {

/*        Find the light time from the observer to the center of */
/*        frame DREF. */

	spkezp_(dcentr, et, "J2000", abcorr, obscde, rpos, &ltcent, (ftnlen)5,
		 abcorr_len);
	if (failed_()) {
	    chkout_("ZZSFXCOR", (ftnlen)8);
	    return 0;
	}
	refepc = *et + s * ltcent;
    }

/*     The epoch REFEPC associated with frame DREF has been set. */

/*     Compute the transformation from frame DREF to J2000 and the */
/*     transformation from J2000 to the target body-fixed frame. */

/*     Map DVEC to both the J2000 and target body-fixed frames. We'll */
/*     store DVEC, expressed relative to the J2000 frame, in the */
/*     variable J2DIR. DVEC in the target body-fixed frame will be */
/*     stored in TRGDIR. */

/*     We may need both versions of DVEC: if we use light time */
/*     correction, we'll update "intercept epoch", and hence the */
/*     transformation between J2000 and the target body-fixed frame. */
/*     The transformation between DREF and J2000 doesn't change, on the */
/*     other hand, so we don't have to recompute J2DIR. We need TRGDIR */
/*     in all cases. */

    refchg_(dfrcde, &c__1, &refepc, r2jmat);
    if (failed_()) {
	chkout_("ZZSFXCOR", (ftnlen)8);
	return 0;
    }
    mxv_(r2jmat, dvec, j2dir);

/*     Save this version of J2DIR as J2GEOM. Later we'll */
/*     modify J2DIR, if necessary, to account for stellar */
/*     aberration. */

    vequ_(j2dir, j2geom);

/*     Map J2DIR (in the J2000 frame) to the target body-fixed */
/*     frame. */

    pxform_("J2000", fixref, trgepc, j2tmat, (ftnlen)5, fixref_len);
    if (failed_()) {
	chkout_("ZZSFXCOR", (ftnlen)8);
	return 0;
    }
    mxv_(j2tmat, j2dir, trgdir);

/*     At this point, */

/*        TRGEPC is set. */
/*        TRGDIR is set. */
/*        J2DIR is set. */


/*     Get the J2000-relative state of the observer relative to */
/*     the solar system barycenter at ET. We'll use this in */
/*     several places later. */

    spkssb_(obscde, et, "J2000", ssbost, (ftnlen)5);
    if (failed_()) {
	chkout_("ZZSFXCOR", (ftnlen)8);
	return 0;
    }

/*     If we're using stellar aberration correction, at this point we'll */
/*     account for it. We're going to find a surface point such that */
/*     the radiation path from that point to the observer, after */
/*     correction for stellar aberration, is parallel to the ray. So */
/*     by applying the inverse of the correction to the ray, we obtain */
/*     the ray with which we must perform our intercept computation. */

    if (*usestl) {

/*        We approximate the inverse stellar aberration correction by */
/*        using the correction for the reverse transmission direction. */
/*        If we're in the reception case, we apply the transmission */
/*        stellar aberration correction to J2DIR and vice versa. */

/*        We iterate our estimates until we have the desired level */
/*        of convergence or reach the iteration limit. */

	nitr = 5;
	if (*xmit) {

/*           Use reception stellar aberration correction */
/*           routine STELAB to generate a first estimate of */
/*           the direction vector after stellar aberration */
/*           has been "removed"---that is, apply the inverse */
/*           of the transmission stellar aberration correction */
/*           mapping to J2DIR. */

	    stelab_(j2dir, &ssbost[3], stldir);

/*           Now improve our estimate. */

	    relerr = 1.;
	    i__ = 1;
	    while(i__ <= nitr && relerr > 1e-17) {

/*              Estimate the error in our previous approximation */
/*              by applying the reception stellar aberration */
/*              to STLDIR and finding the difference with J2DIR. */

		stlabx_(stldir, &ssbost[3], j2est);
		vsub_(j2dir, j2est, stlerr);

/*              Adding the error in the reception mapping to STLDIR */
/*              will give us an improved estimate of the inverse. */

		vadd_(stlerr, stldir, stltmp);
		vequ_(stltmp, stldir);
		relerr = vnorm_(stlerr) / vnorm_(stldir);
		++i__;
	    }

/*           At this point we've found a good estimate of the */
/*           direction vector under the inverse of the transmission */
/*           stellar aberration correction mapping. */

	} else {

/*           Use transmission stellar aberration correction */
/*           routine STLABX to generate a first estimate of */
/*           the direction vector after stellar aberration */
/*           has been "removed." */

	    stlabx_(j2dir, &ssbost[3], stldir);

/*           Now improve our estimate. */

	    relerr = 1.;
	    i__ = 1;
	    while(i__ <= nitr && relerr > 1e-17) {

/*              Estimate the error in our previous approximation */
/*              by applying the reception stellar aberration */
/*              to STLDIR and finding the difference with J2DIR. */

		stelab_(stldir, &ssbost[3], j2est);
		vsub_(j2dir, j2est, stlerr);

/*              Adding the error in the reception mapping to STLDIR */
/*              will give us an improved estimate of the inverse. */

		vadd_(stlerr, stldir, stltmp);
		vequ_(stltmp, stldir);
		relerr = vnorm_(stlerr) / vnorm_(stldir);
		++i__;
	    }

/*           At this point we've found a good estimate of the */
/*           direction vector under the inverse of the reception */
/*           stellar aberration correction mapping. */

	}

/*        Replace the J2000-relative ray direction with the corrected */
/*        direction. */

	vequ_(stldir, j2dir);
	mxv_(j2tmat, j2dir, trgdir);
    }

/*     Find the surface intercept point and distance from observer to */
/*     intercept point using the specified geometric definition. */

/*     Find the surface intercept given the target epoch, */
/*     observer-target position, and target body orientation we've */
/*     already computed. If we're not using light time correction, this */
/*     is all we must do. Otherwise, our result will give us an initial */
/*     estimate of the target epoch, which we'll then improve. */

/*     Make an easy test to see whether we can quit now because an */
/*     intercept cannot exist. If the ray is separated from the */
/*     observer-target center vector by more than (MARGIN * the maximum */
/*     target radius), we're done. Let REJECT be the angular */
/*     separation limit. */

    (*udmaxr)(&maxrad);
    range = vnorm_(obspos);
    if (range == 0.) {

/*        We've already ensured that observer and target are */
/*        distinct, so this should be a very unusual occurrence. */

	setmsg_("Observer-target distance is zero. Observer ID is #; target "
		"ID is #.", (ftnlen)67);
	errint_("#", obscde, (ftnlen)1);
	errint_("#", trgcde, (ftnlen)1);
	sigerr_("SPICE(NOSEPARATION)", (ftnlen)19);
	chkout_("ZZSFXCOR", (ftnlen)8);
	return 0;
    }
    if (range > maxrad * 1.01) {

/*        Compute the arc sine with SPICE error checking. */

	d__1 = maxrad * 1.01 / range;
	reject = dasine_(&d__1, &c_b27);
	vminus_(obspos, negpos);
	if (vsep_(negpos, trgdir) > reject) {

/*           The angular separation of ray and target is too great */
/*           for a solution to exist, even with a better light time */
/*           estimate. */

	    chkout_("ZZSFXCOR", (ftnlen)8);
	    return 0;
	}
    }

/*     Locate the intercept of the ray with the target; if there's no */
/*     intercept, find the closest point on the target to the ray. */

    (*udrayx)(obspos, trgdir, trgepc, spoint, found);
    if (failed_()) {
	chkout_("ZZSFXCOR", (ftnlen)8);
	return 0;
    }

/*     If we found an intercept, and if we're not using light time */
/*     corrections, we're almost done now. We still need SRFVEC. */
/*     SPOINT, TRGEPC, and FOUND have already been set. */

    if (*found && ! (*uselt)) {
	vsub_(spoint, obspos, srfvec);
	chkout_("ZZSFXCOR", (ftnlen)8);
	return 0;
    }

/*     From this point onward, we're dealing with a case calling for */
/*     light time and possibly stellar aberration corrections. */

    if (! (*found)) {

/*        If there's no intercept, we're probably done. However, we need */
/*        to guard against the possibility that the ray does intersect */
/*        the ellipsoid but we haven't discovered it because our first */
/*        light time estimate was too poor. */

/*        We'll make an improved light time estimate as follows: Find */
/*        the nearest point on the ellipsoid to the ray. Find the light */
/*        time between the observer and this point. */

/*        If we're using converged Newtonian corrections, we iterate */
/*        this procedure up to three times. */

	if (*usecn) {
	    nitr = 3;
	} else {
	    nitr = 1;
	}
	i__ = 1;
	while(i__ <= nitr && ! (*found)) {
	    (*udnear)(obspos, trgdir, et, pnear, &rayalt);
	    if (failed_()) {
		chkout_("ZZSFXCOR", (ftnlen)8);
		return 0;
	    }
	    lt = vdist_(obspos, pnear) / clight_();

/*           Use the new light time estimate to repeat the intercept */
/*           computation. */

	    *trgepc = *et + s * lt;

/*           Get the J2000-relative state of the target relative to */
/*           the solar system barycenter at the target epoch. */

	    spkssb_(trgcde, trgepc, "J2000", ssbtst, (ftnlen)5);
	    if (failed_()) {
		chkout_("ZZSFXCOR", (ftnlen)8);
		return 0;
	    }

/*           Find the position of the observer relative to the target. */
/*           Convert this vector from the J2000 frame to the target */
/*           frame at TRGEPC. */

	    vsub_(ssbost, ssbtst, j2pos);
	    pxform_("J2000", fixref, trgepc, xform, (ftnlen)5, fixref_len);
	    if (failed_()) {
		chkout_("ZZSFXCOR", (ftnlen)8);
		return 0;
	    }

/*           Convert the observer's position relative to the target */
/*           from the J2000 frame to the target frame at the target */
/*           epoch. */

	    mxv_(xform, j2pos, obspos);

/*           Convert the ray's direction vector from the J2000 frame */
/*           to the target frame at the target epoch. */

	    mxv_(xform, j2dir, trgdir);

/*           Repeat the intercept computation. */

	    (*udrayx)(obspos, trgdir, trgepc, spoint, found);
	    if (failed_()) {
		chkout_("ZZSFXCOR", (ftnlen)8);
		return 0;
	    }
	    ++i__;
	}

/*        If there's still no intercept, we're done. */

	if (! (*found)) {
	    chkout_("ZZSFXCOR", (ftnlen)8);
	    return 0;
	}
    }

/*     Making it to this point means we've got an intersection, given */
/*     our current light time estimate. It's possible that a better */
/*     light time estimate will yield no intersection. */

/*     Since we're using light time corrections, we're going to make */
/*     an estimate of light time to the intercept point, then re-do */
/*     our computation of the target position and orientation using */
/*     the new light time value. */

    if (*usecn) {
	nitr = 10;
    } else {
	nitr = 1;
    }

/*     Compute new light time estimate and new target epoch. */

    dist = vdist_(obspos, spoint);
    lt = dist / clight_();
    *trgepc = *et + s * lt;
    prevlt = 0.;
    prevet = *trgepc;
    i__ = 0;
    ltdiff = 1.;
    etdiff = 1.;
    while(i__ < nitr && ltdiff > abs(lt) * 1e-17 && etdiff > 0.) {

/*        Get the J2000-relative state of the target relative to */
/*        the solar system barycenter at the target epoch. */

	spkssb_(trgcde, trgepc, "J2000", ssbtst, (ftnlen)5);
	if (failed_()) {
	    chkout_("ZZSFXCOR", (ftnlen)8);
	    return 0;
	}

/*        Find the position of the observer relative to the target. */
/*        Convert this vector from the J2000 frame to the target frame */
/*        at TRGEPC. */

/*        Note that SSBOST contains the J2000-relative state of the */
/*        observer relative to the solar system barycenter at ET. */

	vsub_(ssbost, ssbtst, j2pos);
	pxform_("J2000", fixref, trgepc, xform, (ftnlen)5, fixref_len);
	if (failed_()) {
	    chkout_("ZZSFXCOR", (ftnlen)8);
	    return 0;
	}

/*        Convert the observer's position relative to the target from */
/*        the J2000 frame to the target frame at the target epoch. */

	mxv_(xform, j2pos, obspos);
	vminus_(obspos, negpos);

/*        Convert the ray's direction vector from the J2000 frame */
/*        to the target frame at the target epoch. */

	mxv_(xform, j2dir, trgdir);

/*        Repeat the intercept computation. */

	(*udrayx)(obspos, trgdir, trgepc, spoint, found);
	if (failed_()) {
	    chkout_("ZZSFXCOR", (ftnlen)8);
	    return 0;
	}

/*        If there's no intercept, try to get an estimate of the */
/*        intercept location and the light time by using the nearest */
/*        point to the ellipsoid on the line containing the ray. This is */
/*        useful only if the iteration count has not reached its maximum */
/*        value (the termination value minus one), since the point of */
/*        this is to make it possible to find an intercept. */

/*        Note that an intercept was already found using the initial */
/*        aberration corrections, so we can't get to this case unless */
/*        we have near-intercept geometry. */

	if (! (*found)) {
	    if (i__ < nitr - 1) {

/*              SPOINT is the nearest point to the ellipsoid on the */
/*              line containing the ray. */

		(*udnear)(obspos, trgdir, et, pnear, &rayalt);
		nplnpt_(obspos, trgdir, pnear, spoint, &rayalt);
	    } else {

/*              We're not going to find an intercept. */

		chkout_("ZZSFXCOR", (ftnlen)8);
		return 0;
	    }
	}

/*        Compute the distance between intercept and observer. */

	dist = vdist_(obspos, spoint);

/*        Compute a new light time estimate and a new target epoch. */

	lt = dist / clight_();
	*trgepc = *et + s * lt;

/*        We use the d.p. identity function TOUCHD to force the compiler */
/*        to create double precision arguments from the differences */
/*        LT-PREVLT and TRGEPC-PREVET. Some compilers will perform */
/*        extended-precision register arithmetic, which can prevent a */
/*        difference from rounding to zero. Simply storing the result of */
/*        the subtraction in a double precision variable doesn't solve */
/*        the problem, because that variable can be optimized out of */
/*        existence. */

	d__2 = lt - prevlt;
	ltdiff = (d__1 = touchd_(&d__2), abs(d__1));
	d__2 = *trgepc - prevet;
	etdiff = (d__1 = touchd_(&d__2), abs(d__1));
	prevlt = lt;
	prevet = *trgepc;
	++i__;
    }

/*     FOUND, SPOINT, TRGEPC, and OBSPOS have been set at this point. */
/*     We need SRFVEC. Since OBSPOS doesn't take into account stellar */
/*     aberration, we can' derive SRFVEC from OBSPOS as is done in */
/*     the related routines SUBPNT and SUBSLR. Here, we derive */
/*     SRFVEC from J2GEOM, which is the input ray direction expressed in */
/*     the J2000 frame. We use XFORM, which is computed in the loop */
/*     above, to convert J2GEOM to FIXREF, evaluated at TRGEPC. */

    mxv_(xform, j2geom, udir);
    vhatip_(udir);

/*     Let SRFLEN be the length of SRFVEC; we CAN get this */
/*     length from OBSPOS and SPOINT, since stellar */
/*     aberration correction (as implemented in SPICE) */
/*     doesn't change the length of the vector SPOINT-OBSPOS. */

    srflen = vdist_(spoint, obspos);

/*     Scale UDIR to obtain the desired value of SRFVEC. */

    vscl_(&srflen, udir, srfvec);
    chkout_("ZZSFXCOR", (ftnlen)8);
    return 0;
} /* zzsfxcor_ */

