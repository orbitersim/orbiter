/* dskxsi.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* $Procedure DSKXSI (DSK, ray-surface intercept with source information) */
/* Subroutine */ int dskxsi_(logical *pri, char *target, integer *nsurf, 
	integer *srflst, doublereal *et, char *fixref, doublereal *vertex, 
	doublereal *raydir, integer *maxd, integer *maxi, doublereal *xpt, 
	integer *handle, integer *dladsc, doublereal *dskdsc, doublereal *dc, 
	integer *ic, logical *found, ftnlen target_len, ftnlen fixref_len)
{
    /* Initialized data */

    static logical first = TRUE_;
    static char prvfrm[32] = "                                ";
    static integer prvtcd = 0;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int zzbods2c_(integer *, char *, integer *, 
	    logical *, char *, integer *, logical *, ftnlen, ftnlen), 
	    zzpctrck_(integer *, logical *), zzsbfxri_(integer *, integer *, 
	    integer *, doublereal *, integer *, doublereal *, doublereal *, 
	    doublereal *, integer *, integer *, doublereal *, doublereal *, 
	    integer *, logical *), zzctruin_(integer *), chkin_(char *, 
	    ftnlen), errch_(char *, char *, ftnlen, ftnlen);
    extern logical failed_(void);
    static integer trgcde, fixfid;
    logical frmfnd, trgfnd;
    integer fxcent;
    static integer svtcde, frmctr[2];
    integer fxtpid, fxclss;
    static integer trgctr[2];
    logical newfrm;
    static char svtnam[36];
    extern logical return_(void);
    logical newtrg;
    static logical svtfnd;
    logical update;
    extern /* Subroutine */ int chkout_(char *, ftnlen), setmsg_(char *, 
	    ftnlen), sigerr_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen), namfrm_(char *, integer *, ftnlen), frinfo_(integer *, 
	    integer *, integer *, integer *, logical *);

/* $ Abstract */

/*     Compute a ray-surface intercept using data provided by */
/*     multiple loaded DSK segments. Return information about */
/*     the source of the data defining the surface on which the */
/*     intercept was found: DSK handle, DLA and DSK descriptors, */
/*     and DSK data type-dependent parameters. */

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

/*     CK */
/*     DSK */
/*     FRAMES */
/*     PCK */
/*     SPK */
/*     TIME */

/* $ Keywords */

/*     GEOMETRY */
/*     INTERCEPT */
/*     SURFACE */
/*     TOPOGRAPHY */

/* $ Declarations */

/*     File: dsk.inc */


/*     Version 1.0.0 05-FEB-2016 (NJB) */

/*     Maximum size of surface ID list. */


/*     End of include file dsk.inc */


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


/*     File: dsksrc.inc */


/*     Version 1.0.0 19-FEB-2016 (NJB) */

/*     This file declares sizes of certain output array arguments */
/*     returned by DSKXSI. These arrays contain information on the */
/*     source of surface data on which an intercept was found. */

/*     See the include files */

/*        dla.inc */
/*        dskdsc.inc */

/*     for the declarations of DLA and DSK descriptor sizes, */
/*     respectively. */

/*     Caution: the sizes declared here may be increased */
/*     in a future SPICE Toolkit version. These sizes */
/*     are correct for the N0066 Toolkit. */



/*     Size of double precision source information component: */

/*     Size of integer source information component: */


/*     The contents of the components are DSK data type-dependent. */

/*         Type 2 */
/*         ====== */

/*                    Integer component        Double Precision component */

/*         Element    Contents */
/*         -------    --------------------------------------------------- */
/*            1       Plate ID                 Unused */



/*     End of include file dsksrc.inc */

/* $ Abstract */

/*     Declare public surface name/ID mapping parameters. */

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

/*     NAIF_IDS */

/* $ Keywords */

/*     CONVERSION */
/*     NAME */
/*     STRING */
/*     SURFACE */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 02-DEC-2015 (NJB) */

/* -& */

/*     Maximum number of surface name/ID mapping entries: */


/*     Maximum length of a surface name string: */


/*     End of file srftrn.inc. */

/* $ Abstract */

/*     This include file defines the dimension of the counter */
/*     array used by various SPICE subsystems to uniquely identify */
/*     changes in their states. */

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

/*     CTRSIZ      is the dimension of the counter array used by */
/*                 various SPICE subsystems to uniquely identify */
/*                 changes in their states. */

/* $ Author_and_Institution */

/*     B.V. Semenov    (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 29-JUL-2013 (BVS) */

/* -& */

/*     End of include file. */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     PRI        I   Data prioritization flag. */
/*     TARGET     I   Target body name. */
/*     NSURF      I   Number of surface IDs in list. */
/*     SRFLST     I   Surface ID list. */
/*     ET         I   Epoch, expressed as seconds past J2000 TDB. */
/*     FIXREF     I   Name of target body-fixed reference frame. */
/*     VERTEX     I   Vertex of ray. */
/*     RAYDIR     I   Direction vector of ray. */
/*     MAXD       I   Size of DC array. */
/*     MAXI       I   Size of IC array. */
/*     XPT        O   Intercept point. */
/*     HANDLE     O   Handle of segment contributing surface data. */
/*     DLADSC     O   DLA descriptor of segment. */
/*     DSKDSC     O   DSK descriptor of segment. */
/*     DC         O   Double precision component of source info. */
/*     IC         O   Integer component of source info. */
/*     FOUND      O   Found flag. */
/*     DCSIZE     P   Required size of DC array. */
/*     ICSIZE     P   Required size of IC array. */

/* $ Detailed_Input */

/*     PRI      is a logical flag indicating whether to perform a */
/*              prioritized or unprioritized DSK segment search. In an */
/*              unprioritized search, no segment masks another: data from */
/*              all specified segments are used to define the surface of */
/*              interest. */

/*              The search is unprioritized if and only if PRI is set to */
/*              .FALSE. In the N0066 SPICE Toolkit, this is the only */
/*              allowed value. */

/*     TARGET   is the name of the target body on which a surface */
/*              intercept is sought. */

/*     NSURF, */
/*     SRFLST   are, respectively, a count of surface ID codes in a list */
/*              and an array containing the list. Only DSK segments for */
/*              the body designated by TARGET and having surface IDs in */
/*              this list will be considered in the intercept */
/*              computation. If the list is empty, all DSK segments for */
/*              TARGET will be considered. */

/*     ET       is the epoch of the intersection computation, expressed */
/*              as seconds past J2000 TDB. This epoch is used only for */
/*              DSK segment selection. Segments used in the intercept */
/*              computation must include ET in their time coverage */
/*              intervals. */

/*     FIXREF   is the name of a body-fixed, body-centered reference */
/*              frame associated with the target. The input ray vectors */
/*              are specified in this frame, as is the output intercept */
/*              point. */

/*              The frame designated by FIXREF must have a fixed */
/*              orientation relative to the frame of any DSK segment used */
/*              in the computation. */

/*     VERTEX, */
/*     RAYDIR   are, respectively, the vertex and direction vector of the */
/*              ray to be used in the intercept computation. */

/*              Both the vertex and ray's direction vector must be */
/*              represented in the reference frame designated by FIXREF. */
/*              The vertex is considered to be an offset from the target */
/*              body. */

/*     MAXD, */
/*     MAXI     are, respectively, the declared sizes of the arrays DC */
/*              and IC. MAXD must be at least DCSIZE, while MAXI must be */
/*              at least ICSIZE. See the $Parameters section for details. */

/* $ Detailed_Output */

/*     XPT      is the intercept of the input ray on the surface */
/*              specified by the inputs */

/*                 PRI */
/*                 TARGET */
/*                 NSURF */
/*                 SRFLST */
/*                 ET */

/*              if such an intercept exists. If the ray intersects the */
/*              surface at multiple points, the one closest to the ray's */
/*              vertex is selected. */

/*              XPT is defined if and only if FOUND is .TRUE. */

/*              Units are km. */

/*     HANDLE, */
/*     DLADSC, */
/*     DSKDSC   are, respectively, the DSK file handle, DLA descriptor, */
/*              and DSK descriptor of the DSK file and segment that */
/*              contributed the surface data on which the intercept was */
/*              found. */

/*              These outputs are defined if and only if FOUND is .TRUE. */

/*     DC, */
/*     IC       are, respectively, double precision and integer arrays */
/*              that may contain additional information associated with */
/*              the segment contributing the surface data on which the */
/*              intercept was found. The information is DSK data */
/*              type-dependent. */

/*                 For DSK type 2 segments */

/*                    IC(1) is the intercept plate ID. DC is unused. */

/*              These outputs are defined if and only if FOUND is .TRUE. */

/*              The declared length of DC must be at least DCSIZE; the */
/*              declared length of IC must be at least ICSIZE. See the */
/*              $Parameters section for details. */

/*     FOUND    is a logical flag that is set to .TRUE. if and only if */
/*              and intercept was found. */

/* $ Parameters */

/*     See the include file */

/*        dsksrc.inc */

/*     for declarations of size parameters */

/*        DCSIZE */
/*        ICSIZE */

/*     for the output arguments */

/*        DC */
/*        IC */

/*     See the include files */

/*        dla.inc */
/*        dskdsc.inc */

/*     for declarations of DLA and DSK descriptor sizes and */
/*     documentation of the contents of these descriptors. */

/*     See the include file */

/*        dsktol.inc */

/*     for the values of tolerance parameters used by default by the */
/*     ray-surface intercept algorithm. These are discussed in the */
/*     $Particulars section below. */

/* $ Exceptions */

/*     1)  If the input prioritization flag PRI is set to .TRUE., */
/*         the error SPICE(BADPRIORITYSPEC) is signaled. */

/*     2)  If the input body name TARGET cannot be mapped to an */
/*         ID code, the error SPICE(IDCODENOTFOUND) is signaled. */

/*     3)  If the input frame name FIXREF cannot be mapped to an */
/*         ID code, the error SPICE(IDCODENOTFOUND) is signaled. */

/*     4)  If the frame center associated with FIXREF cannot be */
/*         retrieved, the error SPICE(NOFRAMEINFO) is signaled. */

/*     5)  If the frame center associated with FIXREF is not */
/*         the target body, the error SPICE(INVALIDFRAME) is signaled. */

/*     6)  If MAXD is less than DCSIZE or MAXI is less than ICSIZE, */
/*         the error SPICE(ARRAYTOOSMALL) is signaled. */

/*     7)  If NSURF is less than 0, the error SPICE(INVALIDCOUNT) */
/*         is signaled. */

/*     8)  If an error occurs during the intercept computation, the error */
/*         is signaled by a routine in the call tree of this routine. */

/* $ Files */

/*     Appropriate kernels must be loaded by the calling program before */
/*     this routine is called. */

/*     The following data are required: */

/*     -  SPK data: ephemeris data for the positions of the centers */
/*        of DSK reference frames relative to the target body are */
/*        required if those frames are not centered at the target */
/*        body center. */

/*        Typically ephemeris data are made available by loading one */
/*        or more SPK files via FURNSH. */

/*     -  DSK data: DSK files containing topographic data for the */
/*        target body must be loaded. If a surface list is specified, */
/*        data for at least one of the listed surfaces must be loaded. */

/*     -  Frame data: if a frame definition is required to convert */
/*        DSK segment data to the body-fixed frame designated by */
/*        FIXREF, the target, that definition must be available in the */
/*        kernel pool. Typically the definitions of frames not already */
/*        built-in to SPICE are supplied by loading a frame kernel. */

/*     -  CK data: if the frame to which FIXREF refers is a CK frame, */
/*        and if any DSK segments used in the computation have a */
/*        different frame, at least one CK file will be needed to */
/*        permit transformation of vectors between that frame and both */
/*        the J2000 and the target body-fixed frames. */

/*     -  SCLK data: if a CK file is needed, an associated SCLK */
/*        kernel is required to enable conversion between encoded SCLK */
/*        (used to time-tag CK data) and barycentric dynamical time */
/*        (TDB). */

/*     In all cases, kernel data are normally loaded once per program */
/*     run, NOT every time this routine is called. */

/* $ Particulars */

/*     This is the lowest-level public interface for computing */
/*     ray-surface intercepts, where the surface is modeled using */
/*     topographic data provided by DSK files. The highest-level */
/*     interface for this purpose is SINCPT. */

/*     In cases where the data source information returned by this */
/*     routine are not needed, the routine DSKXV may be more suitable. */

/*     This routine works with multiple DSK files. It places no */
/*     restrictions on the data types or coordinate systems of the DSK */
/*     segments used in the computation. DSK segments using different */
/*     reference frames may be used in a single computation. The only */
/*     restriction is that any pair of reference frames used directly or */
/*     indirectly are related by a constant rotation. */

/*     This routine enables calling applications to identify the source */
/*     of the data defining the surface on which an intercept was found. */
/*     The file, segment, and segment-specific information such as a DSK */
/*     type 2 plate ID are returned. */

/*     This routine can be used for improved efficiency in situations */
/*     in which multiple ray-surface intercepts are to be performed */
/*     using a constant ray vertex. */


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
/*        in a later version of the routine, the presence of the PRI */
/*        argument is required. */


/*        Round-off errors and mitigating algorithms */
/*        ------------------------------------------ */

/*        When topographic data are used to represent the surface of a */
/*        target body, round-off errors can produce some results that */
/*        may seem surprising. */

/*        Note that, since the surface in question might have mountains, */
/*        valleys, and cliffs, the points of intersection found for */
/*        nearly identical sets of inputs may be quite far apart from */
/*        each other: for example, a ray that hits a mountain side in a */
/*        nearly tangent fashion may, on a different host computer, be */
/*        found to miss the mountain and hit a valley floor much farther */
/*        from the observer, or even miss the target altogether. */

/*        Round-off errors can affect segment selection: for example, a */
/*        ray that is expected to intersect the target body's surface */
/*        near the boundary between two segments might hit either */
/*        segment, or neither of them; the result may be */
/*        platform-dependent. */

/*        A similar situation exists when a surface is modeled by a set */
/*        of triangular plates, and the ray is expected to intersect the */
/*        surface near a plate boundary. */

/*        To avoid having the routine fail to find an intersection when */
/*        one clearly should exist, this routine uses two "greedy" */
/*        algorithms: */

/*           1) If the ray passes sufficiently close to any of the */
/*              boundary surfaces of a segment (for example, surfaces of */
/*              maximum and minimum longitude or latitude), that segment */
/*              is tested for an intersection of the ray with the */
/*              surface represented by the segment's data. */

/*              This choice prevents all of the segments from being */
/*              missed when at least one should be hit, but it could, on */
/*              rare occasions, cause an intersection to be found in a */
/*              segment other than the one that would be found if higher */
/*              precision arithmetic were used. */

/*           2) For type 2 segments, which represent surfaces as */
/*              sets of triangular plates, each plate is expanded very */
/*              slightly before a ray-plate intersection test is */
/*              performed. The default plate expansion factor is */

/*                 1 + XFRACT */

/*              where XFRACT is declared in */

/*                 dsktol.inc */

/*              For example, given a value for XFRACT of 1.e-10, the */
/*              sides of the plate are lengthened by 1/10 of a micron */
/*              per km. The expansion keeps the centroid of the plate */
/*              fixed. */

/*              Plate expansion prevents all plates from being missed */
/*              in cases where clearly at least one should be hit. */

/*              As with the greedy segment selection algorithm, plate */
/*              expansion can occasionally cause an intercept to be */
/*              found on a different plate than would be found if higher */
/*              precision arithmetic were used. It also can occasionally */
/*              cause an intersection to be found when the ray misses */
/*              the target by a very small distance. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Compute surface intercepts of rays emanating from a set of */
/*        vertices distributed on a longitude-latitude grid. All */
/*        vertices are outside the target body, and all rays point */
/*        toward the target's center. */

/*        Check intercepts against expected values. Indicate the */
/*        number of errors, the number of computations, and the */
/*        number of intercepts found. */


/*        Use the meta-kernel shown below to load example SPICE */
/*        kernels. */


/*           KPL/MK */

/*           File: dskxsi_ex1.tm */

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
/*              phobos512.bds                    DSK based on */
/*                                               Gaskell ICQ Q=512 */
/*                                               plate model */
/*           \begindata */

/*              KERNELS_TO_LOAD = ( 'phobos512.bds' ) */

/*           \begintext */

/*           End of meta-kernel */


/*        Example code begins here. */


/*              PROGRAM DSKXSI_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Multi-segment spear program. */
/*        C */
/*        C     This program expects all loaded DSKs */
/*        C     to represent the same body and surface. */
/*        C */
/*              INCLUDE 'dla.inc' */
/*              INCLUDE 'dsk.inc' */
/*              INCLUDE 'dskdsc.inc' */
/*              INCLUDE 'dsksrc.inc' */
/*              INCLUDE 'srftrn.inc' */
/*        C */
/*        C     SPICELIB functions */
/*        C */
/*              DOUBLE PRECISION      RPD */
/*              DOUBLE PRECISION      VDIST */

/*              LOGICAL               FAILED */
/*        C */
/*        C     Local parameters */
/*        C */
/*              DOUBLE PRECISION      DTOL */
/*              PARAMETER           ( DTOL   = 1.D-14 ) */

/*              INTEGER               BDNMLN */
/*              PARAMETER           ( BDNMLN = 36 ) */

/*              INTEGER               FILSIZ */
/*              PARAMETER           ( FILSIZ = 255 ) */

/*              INTEGER               FRNMLN */
/*              PARAMETER           ( FRNMLN = 32 ) */

/*              INTEGER               TYPLEN */
/*              PARAMETER           ( TYPLEN = 4 ) */

/*        C */
/*        C     Local variables */
/*        C */
/*              CHARACTER*(FILSIZ)    DSK1 */
/*              CHARACTER*(TYPLEN)    FILTYP */
/*              CHARACTER*(FRNMLN)    FIXREF */
/*              CHARACTER*(FILSIZ)    META */
/*              CHARACTER*(FILSIZ)    SOURCE */
/*              CHARACTER*(BDNMLN)    TARGET */

/*              DOUBLE PRECISION      D */
/*              DOUBLE PRECISION      DC     ( DCSIZE ) */
/*              DOUBLE PRECISION      DSKDSC ( DSKDSZ ) */
/*              DOUBLE PRECISION      ET */
/*              DOUBLE PRECISION      RAYDIR ( 3 ) */
/*              DOUBLE PRECISION      LAT */
/*              DOUBLE PRECISION      LATCRD ( 3 ) */
/*              DOUBLE PRECISION      LATSTP */
/*              DOUBLE PRECISION      LON */
/*              DOUBLE PRECISION      LONSTP */
/*              DOUBLE PRECISION      POLMRG */
/*              DOUBLE PRECISION      R */
/*              DOUBLE PRECISION      RADIUS */
/*              DOUBLE PRECISION      VERTEX ( 3 ) */
/*              DOUBLE PRECISION      XPT    ( 3 ) */
/*              DOUBLE PRECISION      XYZHIT ( 3 ) */

/*              INTEGER               BODYID */
/*              INTEGER               DLADSC ( DLADSZ ) */
/*              INTEGER               DTYPE */
/*              INTEGER               FRAMID */
/*              INTEGER               HANDLE */
/*              INTEGER               IC     ( ICSIZE ) */
/*              INTEGER               NCASES */
/*              INTEGER               NDERR */
/*              INTEGER               NHITS */
/*              INTEGER               NLSTEP */
/*              INTEGER               NSURF */
/*              INTEGER               PLID */
/*              INTEGER               SRFLST ( MAXSRF ) */
/*              INTEGER               SURFID */

/*              LOGICAL               FOUND */


/*              CALL CHKIN ( 'SPEAR' ) */

/*        C */
/*        C     Prompt for the name of the meta-kernel. */
/*        C */
/*              CALL PROMPT ( 'Enter meta-kernel name >  ', META ) */

/*        C */
/*        C     Load the meta-kernel. */
/*        C */
/*              CALL FURNSH ( META ) */

/*        C */
/*        C     Get a handle for one of the loaded DSKs, */
/*        C     then find the first segment and extract */
/*        C     the body and surface IDs. */
/*        C */
/*              CALL KDATA  ( 1,      'DSK',  DSK1, FILTYP, */
/*             .              SOURCE, HANDLE, FOUND ) */

/*              CALL DLABFS ( HANDLE, DLADSC, FOUND ) */

/*              IF ( .NOT. FOUND ) THEN */
/*                 CALL SIGERR ( 'SPICE(NOSEGMENT)' ) */
/*              END IF */

/*              CALL DSKGD ( HANDLE, DLADSC, DSKDSC ) */

/*              BODYID = NINT( DSKDSC(CTRIDX) ) */
/*              SURFID = NINT( DSKDSC(SRFIDX) ) */
/*              FRAMID = NINT( DSKDSC(FRMIDX) ) */

/*              CALL BODC2N ( BODYID, TARGET, FOUND ) */

/*              IF ( .NOT. FOUND ) THEN */
/*                 CALL SETMSG ( 'Cannot map body ID # to a name.' ) */
/*                 CALL ERRINT ( '#',  BODYID                      ) */
/*                 CALL SIGERR ( 'SPICE(BODYNAMENOTFOUND)'         ) */
/*              END IF */

/*              CALL FRMNAM ( FRAMID, FIXREF ) */

/*              IF ( FIXREF .EQ. ' ' ) THEN */
/*                 CALL SETMSG ( 'Cannot map frame ID # to a name.' ) */
/*                 CALL ERRINT ( '#',  FRAMID                       ) */
/*                 CALL SIGERR ( 'SPICE(FRAMENAMENOTFOUND)'         ) */
/*              END IF */

/*        C */
/*        C     Set the magnitude of the ray vertices. Use a large */
/*        C     number to ensure the vertices are outside of */
/*        C     any realistic target. */
/*        C */
/*              R = 1.D10 */

/*        C */
/*        C     Spear the target with rays pointing toward */
/*        C     the origin.  Use a grid of ray vertices */
/*        C     located on a sphere enclosing the target. */
/*        C */
/*        C     The variable POLMRG ("pole margin") can */
/*        C     be set to a small positive value to reduce */
/*        C     the number of intercepts done at the poles. */
/*        C     This may speed up the computation for */
/*        C     the multi-segment case, since rays parallel */
/*        C     to the Z axis will cause all segments converging */
/*        C     at the pole of interest to be tested for an */
/*        C     intersection. */
/*        C */
/*              POLMRG = 5.D-1 */
/*              LATSTP = 1.D0 */
/*              LONSTP = 2.D0 */

/*              NCASES = 0 */
/*              NHITS  = 0 */
/*              NDERR  = 0 */

/*              LON    = -180.D0 */
/*              LAT    = 90.D0 */
/*              NLSTEP = 0 */

/*        C */
/*        C     Set the epoch for interval selection. */
/*        C */
/*              ET     = 0.D0 */

/*              WRITE (*,*) ' ' */
/*              WRITE (*,*) 'Computing intercepts...' */

/*              DO WHILE ( LON .LT. 180.D0 ) */

/*                 DO WHILE ( NLSTEP .LE. 180  ) */

/*                    IF ( LON .EQ. -180.D0 ) THEN */

/*                       LAT = 90.D0 - NLSTEP*LATSTP */

/*                    ELSE */

/*                       IF ( NLSTEP .EQ. 0 ) THEN */

/*                          LAT =  90.D0 - POLMRG */

/*                       ELSE IF ( NLSTEP .EQ. 180 ) THEN */

/*                          LAT = -90.D0 + POLMRG */

/*                       ELSE */

/*                          LAT = 90.D0 - NLSTEP*LATSTP */

/*                       END IF */

/*                    END IF */

/*                    NCASES = NCASES + 1 */

/*                    CALL LATREC ( R, LON*RPD(), LAT*RPD(), VERTEX ) */
/*                    CALL VMINUS ( VERTEX, RAYDIR ) */

/*                    NSURF     = 1 */
/*                    SRFLST(1) = SURFID */

/*                    CALL DSKXSI ( .FALSE., TARGET, NSURF,  SRFLST, */
/*             .                    ET,      FIXREF, VERTEX, RAYDIR, */
/*             .                    DCSIZE,  ICSIZE, XPT,    HANDLE, */
/*             .                    DLADSC, DSKDSC,  DC,     IC, */
/*             .                    FOUND                           ) */

/*                    IF ( .NOT. FAILED() .AND. FOUND ) THEN */
/*        C */
/*        C              Record that a new intercept was found. */
/*        C */
/*                       NHITS = NHITS + 1 */
/*        C */
/*        C              Compute the latitude and longitude of */
/*        C              the intercept. Make sure these agree */
/*        C              well with those of the vertex. */
/*        C */
/*                       CALL RECLAT ( XPT,       LATCRD(1), */
/*             .                       LATCRD(2), LATCRD(3) ) */

/*                       RADIUS = LATCRD(1) */

/*                       CALL LATREC ( RADIUS,   LON*RPD(), */
/*             .                       LAT*RPD(), XYZHIT    ) */

/*                       D = VDIST ( XPT, XYZHIT ) */

/*                       IF ( D/R .GT. DTOL ) THEN */
/*        C */
/*        C                 Get the intercept segment's plate ID if */
/*        C                 applicable. */
/*        C */
/*                          DTYPE = NINT( DSKDSC(TYPIDX) ) */

/*                          WRITE (*,*) '======================' */
/*                          WRITE (*,*) 'LON, LAT       = ', LON, LAT */
/*                          WRITE (*,*) 'Bad intercept' */
/*                          WRITE (*,*) 'Distance error = ', D */
/*                          WRITE (*,*) 'XPT            = ', XPT */
/*                          WRITE (*,*) 'XYZHIT         = ', XYZHIT */

/*                          IF ( DTYPE .EQ. 2 ) THEN */
/*                             PLID = IC(1) */
/*                             WRITE (*,*) 'Plate ID      = ', PLID */
/*                          END IF */

/*                          NDERR = NDERR + 1 */

/*                       END IF */

/*                    ELSE */
/*        C */
/*        C              Missing the target entirely is a fatal error. */
/*        C */
/*                       WRITE (*,*) '======================' */
/*                       WRITE (*,*) 'LON, LAT = ', LON, LAT */
/*                       WRITE (*,*) 'No intercept' */
/*                       WRITE (*,*) 'NCASES = ', NCASES */
/*                       STOP */

/*                    END IF */

/*                    NLSTEP = NLSTEP + 1 */

/*                 END DO */

/*                 LON    = LON + LONSTP */
/*                 LAT    = 90.D0 */
/*                 NLSTEP = 0 */

/*              END DO */

/*              WRITE (*,*) 'Done.' */
/*              WRITE (*,*) ' ' */
/*              WRITE (*,*) 'NCASES = ', NCASES */
/*              WRITE (*,*) 'NHITS  = ', NHITS */
/*              WRITE (*,*) 'NDERR  = ', NDERR */
/*              WRITE (*,*) ' ' */
/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, using as input the meta-kernel dskxsi_ex1.tm, the */
/*        output was: */


/*        Enter meta-kernel name >  dskxsi_ex1.tm */

/*         Computing intercepts... */
/*         Done. */

/*         NCASES =        32580 */
/*         NHITS  =        32580 */
/*         NDERR  =            0 */


/* $ Restrictions */

/*     1)  The frame designated by FIXREF must have a fixed */
/*         orientation relative to the frame of any DSK segment */
/*         used in the computation. This routine has no */
/*         practical way of ensuring that this condition is met; */
/*         so this responsibility is delegated to the calling */
/*         application. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 06-AUG-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/*        Updated code example to prompt for input meta-kernel name and */
/*        set input time to zero. */

/* -    SPICELIB Version 1.0.0, 04-APR-2017 (NJB) */

/*        Original 26-FEB-2016 (NJB) */

/* -& */
/* $ Index_Entries */

/*     DSK ray-surface intercept with source information */
/*     DSK ray-surface intercept with handle and descriptors */
/*     DSK ray-surface intercept with handle and descriptors */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Saved variables */


/*     Initial values */

    if (return_()) {
	return 0;
    }
    chkin_("DSKXSI", (ftnlen)6);
    if (first) {

/*        Initialize counters. */

	zzctruin_(trgctr);
	zzctruin_(frmctr);
	if (failed_()) {
	    chkout_("DSKXSI", (ftnlen)6);
	    return 0;
	}
    }

/*     Reject PRI if not set properly. */

    if (*pri) {
	setmsg_("In the N0066 SPICE Toolkit, PRI must be set to .FALSE., ind"
		"icating that an unprioritized search is to be performed.", (
		ftnlen)115);
	sigerr_("SPICE(BADPRIORITYSPEC)", (ftnlen)22);
	chkout_("DSKXSI", (ftnlen)6);
	return 0;
    }

/*     Reject NSURF if not set properly. Zero is a valid value. */

    if (*nsurf < 0) {
	setmsg_("The surface count NSURF must be non-negative but was #.", (
		ftnlen)55);
	errint_("#", nsurf, (ftnlen)1);
	sigerr_("SPICE(INVALIDCOUNT)", (ftnlen)19);
	chkout_("DSKXSI", (ftnlen)6);
	return 0;
    }

/*     Check output array sizes. */

    if (*maxd < 1 || *maxi < 1) {
	setmsg_("Output array size MAXD must be at least #; output array siz"
		"e MAXI must be at least #. Actual sizes were # and # respect"
		"ively.", (ftnlen)125);
	errint_("#", &c__1, (ftnlen)1);
	errint_("#", &c__1, (ftnlen)1);
	errint_("#", maxd, (ftnlen)1);
	errint_("#", maxi, (ftnlen)1);
	sigerr_("SPICE(ARRAYTOOSMALL)", (ftnlen)20);
	chkout_("DSKXSI", (ftnlen)6);
	return 0;
    }

/*     Obtain integer codes for the target and reference frame. */

    zzbods2c_(trgctr, svtnam, &svtcde, &svtfnd, target, &trgcde, &trgfnd, (
	    ftnlen)36, target_len);
    if (failed_()) {
	chkout_("DSKXSI", (ftnlen)6);
	return 0;
    }
    if (! trgfnd) {
	setmsg_("The target, '#', is not a recognized name for an ephemeris "
		"object. The cause of this problem may be that you need an up"
		"dated version of the SPICE Toolkit, or that you failed to lo"
		"ad a kernel containing a name-ID mapping for this body.", (
		ftnlen)234);
	errch_("#", target, (ftnlen)1, target_len);
	sigerr_("SPICE(IDCODENOTFOUND)", (ftnlen)21);
	chkout_("DSKXSI", (ftnlen)6);
	return 0;
    }
    newfrm = s_cmp(fixref, prvfrm, fixref_len, (ftnlen)32) != 0 || first;
    newtrg = trgcde != prvtcd || first;

/*     Get the frame ID if the pool state has changed. The */
/*     first call to ZZPCKTRCK will indicate an update. */

    zzpctrck_(frmctr, &update);
    if (update || newfrm || newtrg) {
	namfrm_(fixref, &fixfid, fixref_len);
	if (failed_()) {
	    chkout_("DSKXSI", (ftnlen)6);
	    return 0;
	}
	if (fixfid == 0) {
	    setmsg_("Reference frame # is not recognized by the SPICE frame "
		    "subsystem. Possibly a required frame definition kernel h"
		    "as not been loaded.", (ftnlen)130);
	    errch_("#", fixref, (ftnlen)1, fixref_len);
	    sigerr_("SPICE(IDCODENOTFOUND)", (ftnlen)21);
	    chkout_("DSKXSI", (ftnlen)6);
	    return 0;
	}

/*        Determine the attributes of the frame designated by FIXREF. */

	frinfo_(&fixfid, &fxcent, &fxclss, &fxtpid, &frmfnd);
	if (failed_()) {
	    chkout_("DSKXSI", (ftnlen)6);
	    return 0;
	}
	if (! frmfnd) {
	    setmsg_("Attributes for reference frame # could not be obtained "
		    "from the SPICE frame subsystem. Possibly a required fram"
		    "e definition kernel has not been loaded.", (ftnlen)151);
	    errch_("#", fixref, (ftnlen)1, fixref_len);
	    sigerr_("SPICE(NOFRAMEINFO)", (ftnlen)18);
	    chkout_("DSKXSI", (ftnlen)6);
	    return 0;
	}

/*        Make sure that FIXREF is centered at the target body's center. */

	if (fxcent != trgcde) {
	    setmsg_("Reference frame # is not centered at the target body #."
		    " The ID code of the frame center is #.", (ftnlen)93);
	    errch_("#", fixref, (ftnlen)1, fixref_len);
	    errch_("#", target, (ftnlen)1, target_len);
	    errint_("#", &fxcent, (ftnlen)1);
	    sigerr_("SPICE(INVALIDFRAME)", (ftnlen)19);
	    chkout_("DSKXSI", (ftnlen)6);
	    return 0;
	}

/*        We have a valid frame at this point. Save the name. */

	first = FALSE_;
	s_copy(prvfrm, fixref, (ftnlen)32, fixref_len);

/*        Update the previous target ID code as well. */

	prvtcd = trgcde;
    }

/*     TRGCDE and FIXFID are set. */


/*     Perform the intercept computation. */

    zzsbfxri_(&trgcde, nsurf, srflst, et, &fixfid, vertex, raydir, xpt, 
	    handle, dladsc, dskdsc, dc, ic, found);
    chkout_("DSKXSI", (ftnlen)6);
    return 0;
} /* dskxsi_ */

