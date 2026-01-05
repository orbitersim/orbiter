/* ilumin.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ILUMIN ( Illumination angles ) */
/* Subroutine */ int ilumin_(char *method, char *target, doublereal *et, char 
	*fixref, char *abcorr, char *obsrvr, doublereal *spoint, doublereal *
	trgepc, doublereal *srfvec, doublereal *phase, doublereal *incdnc, 
	doublereal *emissn, ftnlen method_len, ftnlen target_len, ftnlen 
	fixref_len, ftnlen abcorr_len, ftnlen obsrvr_len)
{
    extern /* Subroutine */ int chkin_(char *, ftnlen), illumg_(char *, char *
	    , char *, doublereal *, char *, char *, char *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, ftnlen, ftnlen, ftnlen, ftnlen, ftnlen, ftnlen), 
	    chkout_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Find the illumination angles (phase, solar incidence, and */
/*     emission) at a specified surface point of a target body. */

/*     This routine supersedes ILLUM. */

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
/*     FRAMES */
/*     NAIF_IDS */
/*     PCK */
/*     SPK */
/*     TIME */

/* $ Keywords */

/*     ANGLES */
/*     GEOMETRY */
/*     ILLUMINATION */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     METHOD     I   Computation method. */
/*     TARGET     I   Name of target body. */
/*     ET         I   Epoch in ephemeris seconds past J2000 TDB. */
/*     FIXREF     I   Body-fixed, body-centered target body frame. */
/*     ABCORR     I   Desired aberration correction. */
/*     OBSRVR     I   Name of observing body. */
/*     SPOINT     I   Body-fixed coordinates of a target surface point. */
/*     TRGEPC     O   Target surface point epoch. */
/*     SRFVEC     O   Vector from observer to target surface point. */
/*     PHASE      O   Phase angle at the surface point. */
/*     INCDNC     O   Solar incidence angle at the surface point. */
/*     EMISSN     O   Emission angle at the surface point. */

/* $ Detailed_Input */

/*     METHOD   is a short string providing parameters defining */
/*              the computation method to be used. In the syntax */
/*              descriptions below, items delimited by brackets */
/*              are optional. */

/*              METHOD may be assigned the following values: */

/*                 'ELLIPSOID' */

/*                    The illumination angle computation uses a */
/*                    triaxial ellipsoid to model the surface of the */
/*                    target body. The ellipsoid's radii must be */
/*                    available in the kernel pool. */


/*                 'DSK/UNPRIORITIZED[/SURFACES = <surface list>]' */

/*                    The illumination angle computation uses */
/*                    topographic data to model the surface of the */
/*                    target body. These data must be provided by */
/*                    loaded DSK files. */

/*                    The surface list specification is optional. The */
/*                    syntax of the list is */

/*                       <surface 1> [, <surface 2>...] */

/*                    If present, it indicates that data only for the */
/*                    listed surfaces are to be used; however, data */
/*                    need not be available for all surfaces in the */
/*                    list. If absent, loaded DSK data for any surface */
/*                    associated with the target body are used. */

/*                    The surface list may contain surface names or */
/*                    surface ID codes. Names containing blanks must */
/*                    be delimited by double quotes, for example */

/*                       'SURFACES = "Mars MEGDR 128 PIXEL/DEG"' */

/*                    If multiple surfaces are specified, their names */
/*                    or IDs must be separated by commas. */

/*                    See the $Particulars section below for details */
/*                    concerning use of DSK data. */


/*              Neither case nor white space are significant in METHOD, */
/*              except within double-quoted strings representing surface */
/*              names. For example, the string ' eLLipsoid ' is valid. */

/*              Within double-quoted strings representing surface names, */
/*              blank characters are significant, but multiple */
/*              consecutive blanks are considered equivalent to a single */
/*              blank. Case is not significant. So */

/*                 "Mars MEGDR 128 PIXEL/DEG" */

/*              is equivalent to */

/*                 " mars megdr  128  pixel/deg " */

/*              but not to */

/*                 "MARS MEGDR128PIXEL/DEG" */

/*     TARGET   is the name of the target body. TARGET is */
/*              case-insensitive, and leading and trailing blanks in */
/*              TARGET are not significant. Optionally, you may */
/*              supply a string containing the integer ID code for */
/*              the object. For example both 'MOON' and '301' are */
/*              legitimate strings that indicate the Moon is the */
/*              target body. */

/*     ET       is the epoch, expressed as seconds past J2000 TDB, */
/*              for which the apparent illumination angles at the */
/*              specified surface point on the target body, as seen */
/*              from the observing body, are to be computed. */

/*     FIXREF   is the name of the body-fixed, body-centered */
/*              reference frame associated with the target body. The */
/*              input surface point SPOINT and the output vector */
/*              SRFVEC are expressed relative to this reference */
/*              frame. The string FIXREF is case-insensitive, and */
/*              leading and trailing blanks in FIXREF are not */
/*              significant. */

/*     ABCORR   is the aberration correction to be used in computing */
/*              the position and orientation of the target body and */
/*              the location of the Sun. */

/*              For remote sensing applications, where the apparent */
/*              illumination angles seen by the observer are desired, */
/*              normally either of the corrections */

/*                 'LT+S' */
/*                 'CN+S' */

/*              should be used. These and the other supported options */
/*              are described below. ABCORR may be any of the */
/*              following: */

/*                 'NONE'     No aberration correction. */

/*              Let LT represent the one-way light time between the */
/*              observer and the input surface point SPOINT (note: NOT */
/*              between the observer and the target body's center). The */
/*              following values of ABCORR apply to the "reception" case */
/*              in which photons depart from SPOINT at the light-time */
/*              corrected epoch ET-LT and *arrive* at the observer's */
/*              location at ET: */

/*                 'LT'       Correct both the position of SPOINT as */
/*                            seen by the observer, and the position */
/*                            of the Sun as seen by the target, for */
/*                            light time. Correct the orientation of */
/*                            the target for light time. */

/*                 'LT+S'     Correct both the position of SPOINT as */
/*                            seen by the observer, and the position */
/*                            of the Sun as seen by the target, for */
/*                            light time and stellar aberration. */
/*                            Correct the orientation of the target */
/*                            for light time. */

/*                 'CN'       Converged Newtonian light time */
/*                            correction. In solving the light time */
/*                            equations for SPOINT and the Sun, the */
/*                            "CN" correction iterates until the */
/*                            solution converges. */

/*                 'CN+S'     Converged Newtonian light time and */
/*                            stellar aberration corrections. This */
/*                            option produces a solution that is at */
/*                            least as accurate at that obtainable */
/*                            with the 'LT+S' option. Whether the */
/*                            'CN+S' solution is substantially more */
/*                            accurate depends on the geometry of the */
/*                            participating objects and on the */
/*                            accuracy of the input data. In all */
/*                            cases this routine will execute more */
/*                            slowly when a converged solution is */
/*                            computed. */


/*              The following values of ABCORR apply to the */
/*              "transmission" case in which photons *arrive* at */
/*              SPOINT at the light-time corrected epoch ET+LT and */
/*              *depart* from the observer's location at ET: */

/*                 'XLT'      "Transmission" case: correct for */
/*                            one-way light time using a Newtonian */
/*                            formulation. This correction yields the */
/*                            illumination angles at the moment that */
/*                            SPOINT receives photons emitted from the */
/*                            observer's location at ET. */

/*                            The light time correction uses an */
/*                            iterative solution of the light time */
/*                            equation. The solution invoked by the */
/*                            'XLT' option uses one iteration. */

/*                            Both the target position as seen by the */
/*                            observer, and rotation of the target */
/*                            body, are corrected for light time. */

/*                 'XLT+S'    "Transmission" case: correct for */
/*                            one-way light time and stellar */
/*                            aberration using a Newtonian */
/*                            formulation  This option modifies the */
/*                            angles obtained with the 'XLT' option */
/*                            to account for the observer's and */
/*                            target's velocities relative to the */
/*                            solar system barycenter (the latter */
/*                            velocity is used in computing the */
/*                            direction to the apparent illumination */
/*                            source). */

/*                 'XCN'      Converged Newtonian light time */
/*                            correction. This is the same as XLT */
/*                            correction but with further iterations */
/*                            to a converged Newtonian light time */
/*                            solution. */

/*                 'XCN+S'    "Transmission" case: converged */
/*                            Newtonian light time and stellar */
/*                            aberration corrections. This option */
/*                            produces a solution that is at least as */
/*                            accurate at that obtainable with the */
/*                            'XLT+S' option. Whether the 'XCN+S' */
/*                            solution is substantially more accurate */
/*                            depends on the geometry of the */
/*                            participating objects and on the */
/*                            accuracy of the input data. In all */
/*                            cases this routine will execute more */
/*                            slowly when a converged solution is */
/*                            computed. */


/*              Neither case nor white space are significant in */
/*              ABCORR. For example, the string */

/*                'Lt + s' */

/*              is valid. */

/*     OBSRVR   is the name of the observing body. The observing body is */
/*              an ephemeris object: it typically is a spacecraft, an */
/*              extended body, or a surface point for which ephemeris */
/*              data are available. OBSRVR is case-insensitive, and */
/*              leading and trailing blanks in OBSRVR are not */
/*              significant. Optionally, you may supply a string */
/*              containing the integer ID code for the object. For */
/*              example both 'MOON' and '301' are legitimate strings that */
/*              indicate the Moon is the observer. */

/*              OBSRVR may be not be identical to TARGET. */

/*     SPOINT   is a surface point on the target body, expressed in */
/*              Cartesian coordinates, relative to the body-fixed */
/*              target frame designated by FIXREF. */

/*              SPOINT need not be visible from the observer's */
/*              location at the epoch ET. */

/*              The components of SPOINT have units of km. */

/* $ Detailed_Output */

/*     TRGEPC   is the "target surface point epoch." TRGEPC is defined as */
/*              follows: letting LT be the one-way light time between the */
/*              observer and the input surface point SPOINT, TRGEPC is */
/*              either the epoch ET-LT, ET+LT or ET depending on whether */
/*              the requested aberration correction is, respectively, for */
/*              received radiation, transmitted radiation or omitted. LT */
/*              is computed using the method indicated by ABCORR. */

/*              TRGEPC is expressed as seconds past J2000 TDB. */

/*     SRFVEC   is the vector from the observer's position at ET to */
/*              the aberration-corrected (or optionally, geometric) */
/*              position of SPOINT, where the aberration corrections */
/*              are specified by ABCORR. SRFVEC is expressed in the */
/*              target body-fixed reference frame designated by */
/*              FIXREF, evaluated at TRGEPC. */

/*              The components of SRFVEC are given in units of km. */

/*              One can use the SPICELIB function VNORM to obtain the */
/*              distance between the observer and SPOINT: */

/*                 DIST = VNORM ( SRFVEC ) */

/*              The observer's position OBSPOS, relative to the */
/*              target body's center, where the center's position is */
/*              corrected for aberration effects as indicated by */
/*              ABCORR, can be computed via the call: */

/*                 CALL VSUB ( SPOINT, SRFVEC, OBSPOS ) */

/*              To transform the vector SRFVEC from a reference frame */
/*              FIXREF at time TRGEPC to a time-dependent reference */
/*              frame REF at time ET, the routine PXFRM2 should be */
/*              called. Let XFORM be the 3x3 matrix representing the */
/*              rotation from the reference frame FIXREF at time */
/*              TRGEPC to the reference frame REF at time ET. Then */
/*              SRFVEC can be transformed to the result REFVEC as */
/*              follows: */

/*                  CALL PXFRM2 ( FIXREF, REF,    TRGEPC, ET, XFORM ) */
/*                  CALL MXV    ( XFORM,  SRFVEC, REFVEC ) */


/*     The following outputs depend on the existence of a well-defined */
/*     outward normal vector to the surface at SPOINT. See restriction 1. */


/*     PHASE    is the phase angle at SPOINT, as seen from OBSRVR at time */
/*              ET. This is the angle between the negative of the vector */
/*              SRFVEC and the SPOINT-Sun vector at TRGEPC. Units are */
/*              radians. The range of PHASE is [0, pi]. See $Particulars */
/*              below for a detailed discussion of the definition. */

/*     INCDNC   is the solar incidence angle at SPOINT, as seen from */
/*              OBSRVR at time ET. This is the angle between the surface */
/*              normal vector at SPOINT and the SPOINT-Sun vector at */
/*              TRGEPC. Units are radians. The range of INCDNC is */
/*              [0, pi]. See $Particulars below for a detailed discussion */
/*              of the definition. */

/*     EMISSN   is the emission angle at SPOINT, as seen from OBSRVR at */
/*              time ET. This is the angle between the surface normal */
/*              vector at SPOINT and the negative of the vector SRFVEC. */
/*              Units are radians. The range of EMISSN is [0, pi]. See */
/*              $Particulars below for a detailed discussion of the */
/*              definition. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the specified aberration correction is unrecognized, an */
/*         error is signaled by a routine in the call tree of this */
/*         routine. */

/*     2)  If either the target or observer input strings cannot be */
/*         converted to an integer ID code, an error is signaled */
/*         by a routine in the call tree of this routine. */

/*     3)  If OBSRVR and TARGET map to the same NAIF integer ID code, an */
/*         error is signaled by a routine in the call tree of this */
/*         routine. */

/*     4)  If the input target body-fixed frame FIXREF is not */
/*         recognized, an error is signaled by a routine in the */
/*         call tree of this routine. A frame name may fail to be */
/*         recognized because a required frame specification kernel has */
/*         not been loaded; another cause is a misspelling of the frame */
/*         name. */

/*     5)  If the input frame FIXREF is not centered at the target body, */
/*         an error is signaled by a routine in the call tree of this */
/*         routine. */

/*     6)  If the input argument METHOD is not recognized, an error */
/*         is signaled by a routine in the call tree of this */
/*         routine. */

/*     7)  If insufficient ephemeris data have been loaded prior to */
/*         calling ILUMIN, an error is signaled by a */
/*         routine in the call tree of this routine. Note that when */
/*         light time correction is used, sufficient ephemeris data must */
/*         be available to propagate the states of observer, target, and */
/*         the Sun to the solar system barycenter. */

/*     8)  If the computation method specifies an ellipsoidal target */
/*         shape and triaxial radii of the target body have not been */
/*         loaded into the kernel pool prior to calling ILUMIN, an error */
/*         is signaled by a routine in the call tree of this routine. */

/*     9)  If any of the radii of the target body are non-positive, an */
/*         error is signaled by a routine in the call tree of this */
/*         routine. The target must be an extended body. */

/*     10) If PCK data specifying the target body-fixed frame orientation */
/*         have not been loaded prior to calling ILUMIN, an error is */
/*         signaled by a routine in the call tree of this routine. */

/*     11) If METHOD specifies that the target surface is represented by */
/*         DSK data, and no DSK files are loaded for the specified */
/*         target, an error is signaled by a routine in the call tree */
/*         of this routine. */

/*     12) If METHOD specifies that the target surface is represented by */
/*         DSK data, and data representing the portion of the surface on */
/*         which SPOINT is located are not available, an error is */
/*         signaled by a routine in the call tree of this routine. */

/*     13) If METHOD specifies that the target surface is represented */
/*         by DSK data, SPOINT must lie on the target surface, not above */
/*         or below it. A small tolerance is used to allow for round-off */
/*         error in the calculation determining whether SPOINT is on the */
/*         surface. */

/*         If, in the DSK case, SPOINT is too far from the surface, an */
/*         error is signaled by a routine in the call tree of this */
/*         routine. */

/*         If the surface is represented by a triaxial ellipsoid, SPOINT */
/*         is not required to be close to the ellipsoid; however, the */
/*         results computed by this routine will be unreliable if SPOINT */
/*         is too far from the ellipsoid. */

/* $ Files */

/*     Appropriate kernels must be loaded by the calling program before */
/*     this routine is called. */

/*     The following data are required: */

/*     -  SPK data: ephemeris data for target, observer, and the */
/*        illumination source must be loaded. If aberration */
/*        corrections are used, the states of target, observer, and */
/*        the illumination source relative to the solar system */
/*        barycenter must be calculable from the available ephemeris */
/*        data. Typically ephemeris data are made available by loading */
/*        one or more SPK files via FURNSH. */

/*     -  PCK data: rotation data for the target body must be */
/*        loaded. These may be provided in a text or binary PCK file. */

/*     -  Shape data for the target body: */

/*           PCK data: */

/*              If the target body shape is modeled as an ellipsoid, */
/*              triaxial radii for the target body must be loaded into */
/*              the kernel pool. Typically this is done by loading a */
/*              text PCK file via FURNSH. */

/*              Triaxial radii are also needed if the target shape is */
/*              modeled by DSK data, but the DSK NADIR method is */
/*              selected. */

/*           DSK data: */

/*              If the target shape is modeled by DSK data, DSK files */
/*              containing topographic data for the target body must be */
/*              loaded. If a surface list is specified, data for at */
/*              least one of the listed surfaces must be loaded. */

/*     The following data may be required: */

/*     -  Frame data: if a frame definition is required to convert the */
/*        observer and target states to the body-fixed frame of the */
/*        target, that definition must be available in the kernel */
/*        pool. Typically the definition is supplied by loading a */
/*        frame kernel via FURNSH. */

/*     -  Surface name-ID associations: if surface names are specified */
/*        in METHOD, the association of these names with their */
/*        corresponding surface ID codes must be established by */
/*        assignments of the kernel variables */

/*           NAIF_SURFACE_NAME */
/*           NAIF_SURFACE_CODE */
/*           NAIF_SURFACE_BODY */

/*        Normally these associations are made by loading a text */
/*        kernel containing the necessary assignments. An example */
/*        of such an assignment is */

/*           NAIF_SURFACE_NAME += 'Mars MEGDR 128 PIXEL/DEG' */
/*           NAIF_SURFACE_CODE += 1 */
/*           NAIF_SURFACE_BODY += 499 */

/*     In all cases, kernel data are normally loaded once per program */
/*     run, NOT every time this routine is called. */

/* $ Particulars */

/*     SPICELIB contains four routines that compute illumination angles: */

/*        ILLUMF   (same as ILLUMG, except that illumination */
/*                  and visibility flags are returned.) */

/*        ILLUMG   (same as ILUMIN, except that the caller */
/*                  specifies the illumination source.) */

/*        ILUMIN   (this routine) */

/*        ILLUM    (deprecated) */

/*     ILLUMF is the most capable of the set. */


/*     Illumination angles */
/*     =================== */

/*     The term "illumination angles" refers to the following set of */
/*     angles: */


/*        phase angle              Angle between the vectors from the */
/*                                 surface point to the observer and */
/*                                 from the surface point to the Sun. */

/*        solar incidence angle    Angle between the surface normal at */
/*                                 the specified surface point and the */
/*                                 vector from the surface point to the */
/*                                 Sun. */

/*        emission angle           Angle between the surface normal at */
/*                                 the specified surface point and the */
/*                                 vector from the surface point to the */
/*                                 observer. */

/*     The diagram below illustrates the geometric relationships */
/*     defining these angles. The labels for the solar incidence, */
/*     emission, and phase angles are "s.i.", "e.", and "phase". */



/*                                                      * */
/*                                                     Sun */

/*                    surface normal vector */
/*                              ._                 _. */
/*                              |\                 /|  Sun vector */
/*                                \    phase      / */
/*                                 \   .    .    / */
/*                                 .            . */
/*                                   \   ___   / */
/*                              .     \/     \/ */
/*                                    _\ s.i./ */
/*                             .    /   \   / */
/*                             .   |  e. \ / */
/*         *             <--------------- *  surface point on */
/*      viewing            vector            target body */
/*      location           to viewing */
/*      (observer)         location */



/*     Note that if the target-observer vector, the target normal vector */
/*     at the surface point, and the target-sun vector are coplanar, */
/*     then phase is the sum of incidence and emission. This is rarely */
/*     true; usually */

/*        phase angle  <  solar incidence angle + emission angle */

/*     All of the above angles can be computed using light time */
/*     corrections, light time and stellar aberration corrections, or */
/*     no aberration corrections. In order to describe apparent */
/*     geometry as observed by a remote sensing instrument, both */
/*     light time and stellar aberration corrections should be used. */

/*     The way aberration corrections are applied by this routine */
/*     is described below. */

/*        Light time corrections */
/*        ====================== */

/*           Observer-target surface point vector */
/*           ------------------------------------ */

/*           Let ET be the epoch at which an observation or remote */
/*           sensing measurement is made, and let ET - LT ("LT" stands */
/*           for "light time") be the epoch at which the photons */
/*           received at ET were emitted from the surface point SPOINT. */
/*           Note that the light time between the surface point and */
/*           observer will generally differ from the light time between */
/*           the target body's center and the observer. */


/*           Target body's orientation */
/*           ------------------------- */

/*           Using the definitions of ET and LT above, the target body's */
/*           orientation at ET - LT is used. The surface normal is */
/*           dependent on the target body's orientation, so the body's */
/*           orientation model must be evaluated for the correct epoch. */


/*           Target body -- Sun vector */
/*           ------------------------- */

/*           The surface features on the target body near SPOINT will */
/*           appear in a measurement made at ET as they were at ET-LT. */
/*           In particular, lighting on the target body is dependent on */
/*           the apparent location of the Sun as seen from the target */
/*           body at ET-LT. So, a second light time correction is used */
/*           to compute the position of the Sun relative to the surface */
/*           point. */


/*        Stellar aberration corrections */
/*        ============================== */

/*        Stellar aberration corrections are applied only if */
/*        light time corrections are applied as well. */

/*           Observer-target surface point body vector */
/*           ----------------------------------------- */

/*           When stellar aberration correction is performed, the */
/*           direction vector SRFVEC is adjusted so as to point to the */
/*           apparent position of SPOINT: considering SPOINT to be an */
/*           ephemeris object, SRFVEC points from the observer's */
/*           position at ET to the light time and stellar aberration */
/*           corrected position of SPOINT. */

/*           Target body-Sun vector */
/*           ---------------------- */

/*           The target body-Sun vector is the apparent position of the */
/*           Sun, corrected for light time and stellar aberration, as */
/*           seen from the target body at time ET-LT. */


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
/*        required in the METHOD argument. */


/*        Syntax of the METHOD input argument */
/*        ----------------------------------- */

/*        The keywords and surface list in the METHOD argument */
/*        are called "clauses." The clauses may appear in any */
/*        order, for example */

/*           DSK/<surface list>/UNPRIORITIZED */
/*           DSK/UNPRIORITIZED/<surface list> */
/*           UNPRIORITIZED/<surface list>/DSK */

/*        The simplest form of the METHOD argument specifying use of */
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

/*        An example of a METHOD argument that could be constructed */
/*        using one of the surface lists above is */

/*           'DSK/UNPRIORITIZED/SURFACES = "Mars MEGDR 64 PIXEL/DEG", 3' */


/*        Aberration corrections using DSK data */
/*        ------------------------------------- */

/*        For irregularly shaped target bodies, the distance between the */
/*        observer and the nearest surface intercept need not be a */
/*        continuous function of time; hence the one-way light time */
/*        between the intercept and the observer may be discontinuous as */
/*        well. In such cases, the computed light time, which is found */
/*        using an iterative algorithm, may converge slowly or not at */
/*        all. In all cases, the light time computation will terminate, */
/*        but the result may be less accurate than expected. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Find the phase, solar incidence, and emission angles at the */
/*        sub-solar and sub-spacecraft points on Mars as seen from the */
/*        Mars Global Surveyor spacecraft at a specified UTC time. Use */
/*        light time and stellar aberration corrections. */

/*        Use both an ellipsoidal Mars shape model and topographic data */
/*        provided by a DSK file. */

/*        Use the meta-kernel shown below to load the required SPICE */
/*        kernels. */


/*           KPL/MK */

/*           File: ilumin_ex1.tm */

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
/*              de430.bsp                        Planetary ephemeris */
/*              mar097.bsp                       Mars satellite ephemeris */
/*              pck00010.tpc                     Planet orientation and */
/*                                               radii */
/*              naif0011.tls                     Leapseconds */
/*              mgs_ext12_ipng_mgs95j.bsp        MGS ephemeris */
/*              megr90n000cb_plate.bds           Plate model based on */
/*                                               MEGDR DEM, resolution */
/*                                               4 pixels/degree. */

/*           \begindata */

/*              KERNELS_TO_LOAD = ( 'de430.bsp', */
/*                                  'mar097.bsp', */
/*                                  'pck00010.tpc', */
/*                                  'naif0011.tls', */
/*                                  'mgs_ext12_ipng_mgs95j.bsp', */
/*                                  'megr90n000cb_plate.bds'      ) */
/*           \begintext */


/*        Example code begins here. */


/*              PROGRAM ILUMIN_EX1 */
/*              IMPLICIT NONE */
/*        C */
/*        C     SPICELIB functions */
/*        C */
/*              DOUBLE PRECISION      DPR */
/*        C */
/*        C     Local parameters */
/*        C */
/*              CHARACTER*(*)         F1 */
/*              PARAMETER           ( F1     = '(A,F15.9)' ) */

/*              CHARACTER*(*)         F2 */
/*              PARAMETER           ( F2     = '(A)' ) */

/*              CHARACTER*(*)         F3 */
/*              PARAMETER           ( F3     = '(A,2(2X,L))' ) */

/*              CHARACTER*(*)         META */
/*              PARAMETER           ( META   = 'ilumin_ex1.tm' ) */

/*              INTEGER               NAMLEN */
/*              PARAMETER           ( NAMLEN = 32 ) */

/*              INTEGER               TIMLEN */
/*              PARAMETER           ( TIMLEN = 25 ) */

/*              INTEGER               CORLEN */
/*              PARAMETER           ( CORLEN = 5 ) */

/*              INTEGER               MTHLEN */
/*              PARAMETER           ( MTHLEN = 50 ) */

/*              INTEGER               NMETH */
/*              PARAMETER           ( NMETH  = 2 ) */
/*        C */
/*        C     Local variables */
/*        C */
/*              CHARACTER*(CORLEN)    ABCORR */
/*              CHARACTER*(NAMLEN)    FIXREF */
/*              CHARACTER*(MTHLEN)    ILUMTH ( NMETH ) */
/*              CHARACTER*(NAMLEN)    OBSRVR */
/*              CHARACTER*(MTHLEN)    SUBMTH ( NMETH ) */
/*              CHARACTER*(NAMLEN)    TARGET */
/*              CHARACTER*(TIMLEN)    UTC */

/*              DOUBLE PRECISION      ET */
/*              DOUBLE PRECISION      SRFVEC ( 3 ) */
/*              DOUBLE PRECISION      SSCEMI */
/*              DOUBLE PRECISION      SSCPHS */
/*              DOUBLE PRECISION      SSCPT  ( 3 ) */
/*              DOUBLE PRECISION      SSCSOL */
/*              DOUBLE PRECISION      SSLEMI */
/*              DOUBLE PRECISION      SSLPHS */
/*              DOUBLE PRECISION      SSLSOL */
/*              DOUBLE PRECISION      SSOLPT ( 3 ) */
/*              DOUBLE PRECISION      TRGEPC */

/*              INTEGER               I */


/*        C */
/*        C     Initial values */
/*        C */
/*              DATA                  ILUMTH / 'Ellipsoid', */
/*             .                               'DSK/Unprioritized' / */

/*              DATA                  SUBMTH / 'Near Point/Ellipsoid', */
/*             .                            'DSK/Nadir/Unprioritized' / */

/*        C */
/*        C     Load kernel files. */
/*        C */
/*              CALL FURNSH ( META ) */
/*        C */
/*        C     Convert the UTC request time string to seconds past */
/*        C     J2000 TDB. */
/*        C */
/*              UTC = '2003 OCT 13 06:00:00 UTC' */

/*              CALL UTC2ET ( UTC, ET ) */

/*              WRITE (*,F2) ' ' */
/*              WRITE (*,F2) 'UTC epoch is '//UTC */
/*        C */
/*        C     Assign observer and target names. The acronym MGS */
/*        C     indicates Mars Global Surveyor. See NAIF_IDS for a */
/*        C     list of names recognized by SPICE. Also set the */
/*        C     aberration correction flag. */
/*        C */
/*              TARGET = 'Mars' */
/*              OBSRVR = 'MGS' */
/*              FIXREF = 'IAU_MARS' */
/*              ABCORR = 'CN+S' */

/*              DO I = 1, NMETH */
/*        C */
/*        C        Find the sub-solar point on Mars as */
/*        C        seen from the MGS spacecraft at ET. Use the */
/*        C        "near point" style of sub-point definition */
/*        C        when the shape model is an ellipsoid, and use */
/*        C        the "nadir" style when the shape model is */
/*        C        provided by DSK data. This makes it easy to */
/*        C        verify the solar incidence angle when */
/*        C        the target is modeled as an  ellipsoid. */
/*        C */
/*                 CALL SUBSLR ( SUBMTH(I),  TARGET,  ET, */
/*             .                 FIXREF,     ABCORR,  OBSRVR, */
/*             .                 SSOLPT,     TRGEPC,  SRFVEC  ) */
/*        C */
/*        C        Now find the sub-spacecraft point. */
/*        C */
/*                 CALL SUBPNT ( SUBMTH(I),  TARGET,  ET, */
/*             .                 FIXREF,     ABCORR,  OBSRVR, */
/*             .                 SSCPT,      TRGEPC,  SRFVEC ) */
/*        C */
/*        C        Find the phase, solar incidence, and emission */
/*        C        angles at the sub-solar point on Mars as */
/*        C        seen from MGS at time ET. */
/*        C */
/*                 CALL ILUMIN ( ILUMTH(I), TARGET, */
/*             .                 ET,        FIXREF,  ABCORR, */
/*             .                 OBSRVR,    SSOLPT,  TRGEPC, */
/*             .                 SRFVEC,    SSLPHS,  SSLSOL, */
/*             .                 SSLEMI                      ) */
/*        C */
/*        C        Do the same for the sub-spacecraft point. */
/*        C */
/*                 CALL ILUMIN ( ILUMTH(I), TARGET, */
/*             .                 ET,        FIXREF,  ABCORR, */
/*             .                 OBSRVR,    SSCPT,   TRGEPC, */
/*             .                 SRFVEC,    SSCPHS,  SSCSOL, */
/*             .                 SSCEMI                      ) */
/*        C */
/*        C        Convert the angles to degrees and write them out. */
/*        C */
/*                 SSLPHS = DPR() * SSLPHS */
/*                 SSLSOL = DPR() * SSLSOL */
/*                 SSLEMI = DPR() * SSLEMI */

/*                 SSCPHS = DPR() * SSCPHS */
/*                 SSCSOL = DPR() * SSCSOL */
/*                 SSCEMI = DPR() * SSCEMI */

/*                 WRITE (*,F2) ' ' */
/*                 WRITE (*,F2) '   ILUMIN method: '//ILUMTH(I) */
/*                 WRITE (*,F2) '   SUBPNT method: '//SUBMTH(I) */
/*                 WRITE (*,F2) '   SUBSLR method: '//SUBMTH(I) */
/*                 WRITE (*,F2) ' ' */
/*                 WRITE (*,F2) '      Illumination angles at the ' */
/*             .   //           'sub-solar point:' */
/*                 WRITE (*,F2) ' ' */

/*                 WRITE (*,F1) '      Phase angle           (deg.): ', */
/*             .                SSLPHS */
/*                 WRITE (*,F1) '      Solar incidence angle (deg.): ', */
/*             .                SSLSOL */
/*                 WRITE (*,F1) '      Emission angle        (deg.): ', */
/*             .                SSLEMI */
/*                 WRITE (*,F2) ' ' */

/*                 IF ( I .EQ. 1 ) THEN */
/*                    WRITE (*,F2) '        The solar incidence angle ' */
/*             .      //           'should be 0.' */
/*                    WRITE (*,F2) '        The emission and phase ' */
/*             .      //           'angles should be equal.' */
/*                    WRITE (*,F2) ' ' */
/*                 END IF */


/*                 WRITE (*,F2) '      Illumination angles at the ' */
/*             .   //          'sub-s/c point:' */
/*                 WRITE (*,F2) ' ' */
/*                 WRITE (*,F1) '      Phase angle           (deg.): ', */
/*             .               SSCPHS */
/*                 WRITE (*,F1) '      Solar incidence angle (deg.): ', */
/*             .               SSCSOL */
/*                 WRITE (*,F1) '      Emission angle        (deg.): ', */
/*             .               SSCEMI */
/*                 WRITE (*,F2) ' ' */

/*                 IF ( I .EQ. 1 ) THEN */
/*                    WRITE (*,F2) '        The emission angle ' */
/*             .      //           'should be 0.' */
/*                    WRITE (*,F2) '        The solar incidence ' */
/*             .      //           'and phase angles should be equal.' */
/*                 END IF */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        UTC epoch is 2003 OCT 13 06:00:00 UTC */

/*           ILUMIN method: Ellipsoid */
/*           SUBPNT method: Near Point/Ellipsoid */
/*           SUBSLR method: Near Point/Ellipsoid */

/*              Illumination angles at the sub-solar point: */

/*              Phase angle           (deg.):   138.370270685 */
/*              Solar incidence angle (deg.):     0.000000000 */
/*              Emission angle        (deg.):   138.370270685 */

/*                The solar incidence angle should be 0. */
/*                The emission and phase angles should be equal. */

/*              Illumination angles at the sub-s/c point: */

/*              Phase angle           (deg.):   101.439331040 */
/*              Solar incidence angle (deg.):   101.439331041 */
/*              Emission angle        (deg.):     0.000000002 */

/*                The emission angle should be 0. */
/*                The solar incidence and phase angles should be equal. */

/*           ILUMIN method: DSK/Unprioritized */
/*           SUBPNT method: DSK/Nadir/Unprioritized */
/*           SUBSLR method: DSK/Nadir/Unprioritized */

/*              Illumination angles at the sub-solar point: */

/*              Phase angle           (deg.):   138.387071677 */
/*              Solar incidence angle (deg.):     0.967122745 */
/*              Emission angle        (deg.):   137.621480599 */

/*              Illumination angles at the sub-s/c point: */

/*              Phase angle           (deg.):   101.439331359 */
/*              Solar incidence angle (deg.):   101.555993667 */
/*              Emission angle        (deg.):     0.117861156 */


/* $ Restrictions */

/*     1)  Results from this routine are not meaningful if the input */
/*         point lies on a ridge or vertex of a surface represented by */
/*         DSK data, or if for any other reason the direction of the */
/*         outward normal vector at the point is undefined. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     S.C. Krening       (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.1.0, 20-NOV-2021 (NJB) (JDR) */

/*        Changed the name of the argument SOLAR to INCDNC */
/*        for consistency with other illumination angle */
/*        routines. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 2.0.0, 04-APR-2017 (NJB) */

/*        Fixed some header comment typos. */

/*        Now supports DSK usage. No longer includes zzabcorr.inc. */
/*        String 'SUN' passed to ILLUMG has been changed to '10'. */

/*        Now supports transmission aberration corrections. */


/* -    SPICELIB Version 1.2.0, 04-APR-2011 (NJB) (SCK) */

/*        The routine has been completely re-implemented: */
/*        it now calls ILLUMG. */

/*        The meta-kernel used for the header example program */
/*        has been updated. The example program outputs have */
/*        been updated as well. */

/*        References to the new PXFRM2 routine were added */
/*        to the Detailed Output section. */

/* -    SPICELIB Version 1.1.0, 17-MAY-2010 (NJB) */

/*        Bug fix: ILUMIN now returns immediately if a target */
/*        radius lookup fails. */

/* -    SPICELIB Version 1.0.1, 06-FEB-2009 (NJB) */

/*        Typo correction: changed FIXFRM to FIXREF in header */
/*        documentation. Meta-kernel name suffix was changed to */
/*        ".tm" in header code example. */

/* -    SPICELIB Version 1.0.0, 02-MAR-2008 (NJB) */

/* -& */
/* $ Index_Entries */

/*     illumination angles */
/*     lighting angles */
/*     phase angle */
/*     solar incidence angle */
/*     emission angle */

/* -& */

/*     SPICELIB functions */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("ILUMIN", (ftnlen)6);
    illumg_(method, target, "10", et, fixref, abcorr, obsrvr, spoint, trgepc, 
	    srfvec, phase, incdnc, emissn, method_len, target_len, (ftnlen)2, 
	    fixref_len, abcorr_len, obsrvr_len);
    chkout_("ILUMIN", (ftnlen)6);
    return 0;
} /* ilumin_ */

