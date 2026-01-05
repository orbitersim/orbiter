/* edterm.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure EDTERM ( Ellipsoid terminator ) */
/* Subroutine */ int edterm_(char *trmtyp, char *source, char *target, 
	doublereal *et, char *fixref, char *abcorr, char *obsrvr, integer *
	npts, doublereal *trgepc, doublereal *obspos, doublereal *trmpts, 
	ftnlen trmtyp_len, ftnlen source_len, ftnlen target_len, ftnlen 
	fixref_len, ftnlen abcorr_len, ftnlen obsrvr_len)
{
    /* Initialized data */

    static logical first = TRUE_;

    /* System generated locals */
    doublereal d__1;

    /* Local variables */
    extern /* Subroutine */ int zzbods2c_(integer *, char *, integer *, 
	    logical *, char *, integer *, logical *, ftnlen, ftnlen), 
	    zzgftreb_(integer *, doublereal *), zzcorepc_(char *, doublereal *
	    , doublereal *, doublereal *, ftnlen), zznamfrm_(integer *, char *
	    , integer *, char *, integer *, ftnlen, ftnlen), zzedterm_(char *,
	     doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, integer *, doublereal *, ftnlen), zzctruin_(integer 
	    *);
    doublereal r__;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer obsid;
    extern /* Subroutine */ int errch_(char *, char *, ftnlen, ftnlen);
    integer srcid, trgid;
    logical found;
    doublereal ltsrc;
    static logical svfnd1, svfnd2, svfnd3;
    static integer svctr1[2], svctr2[2];
    extern logical failed_(void);
    static integer svctr3[2], svctr4[2];
    integer frcode, frclas;
    doublereal srcrad[3];
    integer center, clssid;
    doublereal trgrad[3];
    static integer svfrcd;
    extern /* Subroutine */ int frinfo_(integer *, integer *, integer *, 
	    integer *, logical *);
    static char svfref[32];
    doublereal lttarg;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    static integer svtgid;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen);
    static char svtarg[36], svscre[36];
    static integer svobsi;
    doublereal srcpos[3];
    static integer svsrci;
    extern /* Subroutine */ int spkezp_(integer *, doublereal *, char *, char 
	    *, integer *, doublereal *, doublereal *, ftnlen, ftnlen);
    extern logical return_(void);
    doublereal trgpos[3];
    static char svobsr[36];
    extern /* Subroutine */ int vminus_(doublereal *, doublereal *);

/* $ Abstract */

/*     Compute a set of points on the umbral or penumbral terminator of */
/*     a specified target body, where the target shape is modeled as an */
/*     ellipsoid. */

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

/*     FRAMES */
/*     PCK */
/*     SPK */
/*     TIME */

/* $ Keywords */

/*     BODY */
/*     GEOMETRY */
/*     MATH */

/* $ Declarations */
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
/*     TRMTYP     I   Terminator type. */
/*     SOURCE     I   Light source. */
/*     TARGET     I   Target body. */
/*     ET         I   Observation epoch. */
/*     FIXREF     I   Body-fixed frame associated with target. */
/*     ABCORR     I   Aberration correction. */
/*     OBSRVR     I   Observer. */
/*     NPTS       I   Number of points in terminator set. */
/*     TRGEPC     O   Epoch associated with target center. */
/*     OBSPOS     O   Position of observer in body-fixed frame. */
/*     TRMPTS     O   Terminator point set. */

/* $ Detailed_Input */

/*     TRMTYP   is a string indicating the type of terminator to */
/*              compute: umbral or penumbral. The umbral terminator */
/*              is the boundary of the portion of the ellipsoid */
/*              surface in total shadow. The penumbral terminator is */
/*              the boundary of the portion of the surface that is */
/*              completely illuminated. Note that in astronomy */
/*              references, the unqualified word "terminator" refers */
/*              to the umbral terminator. Here, the unqualified */
/*              word refers to either type of terminator. */

/*              Possible values of TRMTYP are */

/*                 'UMBRAL' */
/*                 'PENUMBRAL' */

/*              Case and leading or trailing blanks in TRMTYP are */
/*              not significant. */


/*     SOURCE   is the name of the body acting as a light source. */
/*              SOURCE is case-insensitive, and leading and trailing */
/*              blanks in TARGET are not significant. Optionally, you */
/*              may supply a string containing the integer ID code */
/*              for the object. For example both 'SUN' and '10' are */
/*              legitimate strings that indicate the Sun is the light */
/*              source. */

/*              This routine assumes that a kernel variable */
/*              representing the light source's radii is present in */
/*              the kernel pool. Normally the kernel variable would */
/*              be defined by loading a PCK file. */

/*              The shape of the light source is always modeled as a */
/*              sphere, regardless of whether radii defining a */
/*              triaxial ellipsoidal shape model are available in the */
/*              kernel pool. The maximum radius of the body is used */
/*              as the radius of the sphere. */


/*     TARGET   is the name of the target body. TARGET is */
/*              case-insensitive, and leading and trailing blanks in */
/*              TARGET are not significant. Optionally, you may */
/*              supply a string containing the integer ID code for */
/*              the object. For example both 'MOON' and '301' are */
/*              legitimate strings that indicate the moon is the */
/*              target body. */

/*              This routine assumes that a kernel variable */
/*              representing the target's radii is present in the */
/*              kernel pool. Normally the kernel variable would be */
/*              defined by loading a PCK file. */


/*     ET       is the epoch of participation of the observer, */
/*              expressed as ephemeris seconds past J2000 TDB: ET is */
/*              the epoch at which the observer's position is */
/*              computed. */

/*              When aberration corrections are not used, ET is also */
/*              the epoch at which the position and orientation of the */
/*              target body and position of the light source are */
/*              computed. */

/*              When aberration corrections are used, ET is the epoch */
/*              at which the observer's position relative to the */
/*              solar system barycenter is computed; in this case the */
/*              position and orientation of the target body are */
/*              computed at ET-LT, where LT is the one-way light time */
/*              between the target body's center and the observer. */
/*              See the description of ABCORR below for details. */


/*     FIXREF   is the name of the reference frame relative to which */
/*              the output terminator points are expressed. This must */
/*              be a body-centered, body-fixed frame associated with */
/*              the target. The frame's axes must be compatible with */
/*              the triaxial ellipsoidal shape model associated with */
/*              the target body (normally provide via a PCK): this */
/*              routine assumes that the first, second, and third */
/*              axis lengths correspond, respectively, to the x, y, */
/*              and z-axes of the frame designated by FIXREF. */

/*              FIXREF may refer to a built-in frame (documented in */
/*              the Frames Required Reading) or a frame defined by a */
/*              loaded frame kernel (FK). */

/*              The orientation of the frame designated by FIXREF is */
/*              evaluated at epoch of participation of the target */
/*              body. See the descriptions of ET and ABCORR for */
/*              details. */


/*     ABCORR   indicates the aberration correction to be applied */
/*              when computing the observer-target position, the */
/*              orientation of the target body, and the target- */
/*              source position vector. ABCORR may be any of */
/*              the following. */

/*                 'NONE'     Apply no correction. Compute the */
/*                            terminator points using the position */
/*                            of the light source and target, and */
/*                            the orientation of the target, at ET. */

/*              Let LT represent the one-way light time between the */
/*              observer and the target body's center. The following */
/*              values of ABCORR apply to the "reception" case in */
/*              which photons depart from the target body's center at */
/*              the light-time corrected epoch ET-LT and *arrive* at */
/*              the observer's location at ET: */


/*                 'LT'       Correct for one-way light time (also */
/*                            called "planetary aberration") using a */
/*                            Newtonian formulation. This correction */
/*                            yields the location of the terminator */
/*                            points at the approximate time they */
/*                            emitted photons arriving at the */
/*                            observer at ET (the difference between */
/*                            light time to the target center and */
/*                            light time to the terminator points */
/*                            is ignored). */

/*                            The light time correction uses an */
/*                            iterative solution of the light time */
/*                            equation. The solution invoked by the */
/*                            'LT' option uses one iteration. */

/*                            The target position as seen by the */
/*                            observer, the position of the light */
/*                            source as seen from the target at */
/*                            ET-LT, and the rotation of the target */
/*                            body, are corrected for light time. */

/*                 'LT+S'     Correct for one-way light time and */
/*                            stellar aberration using a Newtonian */
/*                            formulation. This option modifies the */
/*                            positions obtained with the 'LT' option */
/*                            to account for the observer's velocity */
/*                            relative to the solar system */
/*                            barycenter. This correction also */
/*                            applies to the position of the light */
/*                            source relative to the target. The */
/*                            result is the apparent terminator as */
/*                            seen by the observer. */

/*                 'CN'       Converged Newtonian light time */
/*                            correction. In solving the light time */
/*                            equation, the 'CN' correction iterates */
/*                            until the solution converges. The */
/*                            position and rotation of the target */
/*                            body and the position of the light */
/*                            source relative to the target are */
/*                            corrected for light time. */

/*                 'CN+S'     Converged Newtonian light time */
/*                            and stellar aberration corrections. */


/*     OBSRVR   is the name of the observing body. This is typically */
/*              a spacecraft, the Earth, or a surface point on the */
/*              Earth. OBSRVR is case-insensitive, and leading and */
/*              trailing blanks in OBSRVR are not significant. */
/*              Optionally, you may supply a string containing the */
/*              integer ID code for the object. For example both */
/*              'EARTH' and '399' are legitimate strings that indicate */
/*              the Earth is the observer. */


/*     NPTS     is the number of terminator points to compute. */

/* $ Detailed_Output */

/*     TRGEPC   is the "target epoch." TRGEPC is defined as follows: */
/*              letting LT be the one-way light time between the */
/*              target center and observer, TRGEPC is either the */
/*              epoch ET-LT or ET depending on whether the requested */
/*              aberration correction is, respectively, for received */
/*              radiation or omitted. LT is computed using the */
/*              method indicated by ABCORR. */

/*              TRGEPC is expressed as seconds past J2000 TDB. */


/*     OBSPOS   is the vector from the center of the target body at */
/*              epoch TRGEPC to the observer at epoch ET. OBSPOS is */
/*              expressed in the target body-fixed reference frame */
/*              FIXREF, which is evaluated at TRGEPC. */

/*              OBSPOS is returned to simplify various related */
/*              computations that would otherwise be cumbersome. For */
/*              example, the vector XVEC from the observer to the */
/*              Ith terminator point can be calculated via the call */

/*                 CALL VSUB ( TRMPTS(1,I), OBSPOS, XVEC ) */

/*              To transform the vector OBSPOS from a reference frame */
/*              FIXREF at time TRGEPC to a time-dependent reference */
/*              frame REF at time ET, the routine PXFRM2 should be */
/*              called. Let XFORM be the 3x3 matrix representing the */
/*              rotation from the reference frame FIXREF at time */
/*              TRGEPC to the reference frame REF at time ET. Then */
/*              OBSPOS can be transformed to the result REFVEC as */
/*              follows: */

/*                  CALL PXFRM2 ( FIXREF, REF,    TRGEPC, ET, XFORM ) */
/*                  CALL MXV    ( XFORM,  OBSPOS, REFVEC ) */


/*     TRMPTS   is an array of points on the umbral or penumbral */
/*              terminator of the ellipsoid, as specified by the */
/*              input argument TRMTYP. The Ith point is contained in */
/*              the array elements */

/*                  TRMPTS(J,I),  J = 1, 2, 3 */

/*              Each terminator point is the point of tangency of a */
/*              plane that is also tangent to the light source. These */
/*              associated points of tangency on the light source */
/*              have uniform distribution in longitude when expressed */
/*              in a cylindrical coordinate system whose Z-axis is */
/*              the target center to source center vector. The */
/*              magnitude of the separation in longitude between the */
/*              tangency points on the light source is */

/*                 2*Pi / NPTS */

/*              If the target is spherical, the terminator points */
/*              also are uniformly distributed in longitude in the */
/*              cylindrical system described above. If the target is */
/*              non-spherical, the longitude distribution of the */
/*              points generally is not uniform. */

/*              The terminator points are expressed in the body-fixed */
/*              reference frame designated by FIXREF. Units are km. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the input frame name FIXREF cannot be mapped */
/*         to a frame ID code, the error SPICE(NOTRANSLATION) is */
/*         signaled. */

/*     2)  If the target name TARGET cannot be mapped */
/*         to a body ID code, the error SPICE(NOTRANSLATION) is */
/*         signaled. */

/*     3)  If the frame designated by FIXREF is not centered */
/*         on the target, the error SPICE(INVALIDFIXREF) is */
/*         signaled. */

/*     4)  If the terminator type is not recognized, an error */
/*         is signaled by a routine in the call tree of */
/*         this routine. */

/*     5)  If the terminator point count NPTS is not at least 1, an error */
/*         is signaled by a routine in the call tree of this routine. */

/*     6)  If the light source has non-positive radius, an error */
/*         is signaled by a routine in the call tree of */
/*         this routine. */

/*     7)  If the light source intersects the smallest sphere centered at */
/*         the origin and containing the ellipsoid, an error is signaled */
/*         by a routine in the call tree of this routine. */

/*     8)  If radii for the target body or light source are not */
/*         available in the kernel pool, an error is signaled by */
/*         a routine in the call tree of this routine. */

/*     9)  If radii are available but either body does not have three */
/*         radii, an error is signaled by a routine in the call tree of */
/*         this routine. */

/*     10) If any of the radii is less-than or equal to zero, an error is */
/*         signaled by a routine in the call tree of this routine. */

/*     11) If any SPK look-up fails, an error is signaled by */
/*         a routine in the call tree of this routine. */

/* $ Files */

/*     Appropriate SPK, PCK, and frame kernels must be loaded by the */
/*     calling program before this routine is called. */

/*     The following data are required: */

/*     -  SPK data: ephemeris data for the target, observer, and light */
/*        source must be loaded. If aberration corrections are used, */
/*        the states of all three objects relative to the solar system */
/*        barycenter must be calculable from the available ephemeris */
/*        data. Typically ephemeris data are made available by loading */
/*        one or more SPK files via FURNSH. */

/*     -  PCK data: triaxial radii for the target body and */
/*        the light source must be loaded into the kernel pool. */
/*        Typically this is done by loading a text PCK file via */
/*        FURNSH. */

/*     -  Further PCK data: rotation data for the target body must */
/*        be loaded. These may be provided in a text or binary PCK */
/*        file. */

/*     -  Frame data: if a frame definition is required to convert */
/*        the observer and target states to the target body-fixed */
/*        frame designated by FIXREF, that definition must be */
/*        available in the kernel pool. Typically the definitions of */
/*        frames not already built-in to SPICE are supplied by loading */
/*        a frame kernel. */

/*     In all cases, kernel data are normally loaded once per program */
/*     run, NOT every time this routine is called. */

/* $ Particulars */

/*     This routine models the boundaries of shadow regions on an */
/*     ellipsoidal target body "illuminated" by a spherical light */
/*     source. Light rays are assumed to travel along straight lines; */
/*     refraction is not modeled. */

/*     Points on the target body's surface are classified according to */
/*     their illumination as follows: */

/*     -  A target surface point X for which no vector from X to any */
/*        point in the light source intersects the target, except at */
/*        X, is considered to be "completely illuminated." */

/*     -  A target surface point X for which each vector from X to a */
/*        point in the light source intersects the target at points */
/*        other than X is considered to be "in total shadow." */

/*     -  All other target points are considered to be in partial */
/*        shadow. */

/*     In this routine, we use the term "umbral terminator" to denote */
/*     the curve usually called the "terminator": this curve is the */
/*     boundary of the portion of the target body's surface that lies in */
/*     total shadow. We use the term "penumbral terminator" to denote */
/*     the boundary of the completely illuminated portion of the */
/*     surface. */

/*     In general, the terminator on an ellipsoid is a more complicated */
/*     curve than the limb (which is always an ellipse). Aside from */
/*     various special cases, the terminator does not lie in a plane. */

/*     However, the condition for a point X on the ellipsoid to lie on */
/*     the terminator is simple: a plane tangent to the ellipsoid at X */
/*     must also be tangent to the light source. If this tangent plane */
/*     does not intersect the vector from the center of the ellipsoid to */
/*     the center of the light source, then X lies on the umbral */
/*     terminator; otherwise X lies on the penumbral terminator. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */


/*     1) Compute sets of umbral and penumbral terminator points on the */
/*        Moon. Perform a consistency check using the solar incidence */
/*        angle at each point. We expect to see a solar incidence angle */
/*        of approximately 90 degrees. Since the solar incidence angle */
/*        is measured between the local outward normal and the */
/*        direction to the center of the Sun, the solar incidence angle */
/*        at an umbral terminator point should exceed 90 degrees by */
/*        approximately the angular radius of the Sun, while the angle */
/*        at a penumbral terminator point should be less than 90 */
/*        degrees by that amount. */

/*        Use the meta-kernel shown below to load the required SPICE */
/*        kernels. */


/*           KPL/MK */

/*           File name: edterm_ex1.tm */

/*           This meta-kernel is intended to support operation of SPICE */
/*           example programs. The kernels shown here should not be */
/*           assumed to contain adequate or correct versions of data */
/*           required by SPICE-based user applications. */

/*           In order for an application to use this meta-kernel, the */
/*           kernels referenced here must be present in the user's */
/*           current working directory. */

/*           The names and contents of the kernels referenced */
/*           by this meta-kernel are as follows: */

/*              File name                     Contents */
/*              ---------                     -------- */
/*              de421.bsp                     Planetary ephemeris */
/*              pck00010.tpc                  Planet orientation and */
/*                                            radii */
/*              naif0010.tls                  Leapseconds */


/*           \begindata */

/*              KERNELS_TO_LOAD = ( 'de421.bsp', */
/*                                  'pck00010.tpc', */
/*                                  'naif0010.tls'  ) */

/*           \begintext */

/*           End of meta-kernel */


/*        Example code begins here. */


/*              PROGRAM EDTERM_EX1 */
/*              IMPLICIT NONE */
/*        C */
/*        C     SPICELIB functions */
/*        C */
/*              DOUBLE PRECISION      DPR */
/*              DOUBLE PRECISION      VDIST */
/*        C */
/*        C     Local parameters */
/*        C */
/*              CHARACTER*(*)         FMT0 */
/*              PARAMETER           ( FMT0   = '(1X,A,I2,A)' ) */

/*              CHARACTER*(*)         FMT1 */
/*              PARAMETER           ( FMT1   = '(1X,A,F18.9)' ) */

/*              CHARACTER*(*)         META */
/*              PARAMETER           ( META   = 'edterm_ex1.tm' ) */

/*              INTEGER               NPTS */
/*              PARAMETER           ( NPTS   = 3 ) */

/*              INTEGER               CORLEN */
/*              PARAMETER           ( CORLEN = 5 ) */

/*              INTEGER               BDNMLN */
/*              PARAMETER           ( BDNMLN = 36 ) */

/*              INTEGER               FRNMLN */
/*              PARAMETER           ( FRNMLN = 32 ) */

/*              INTEGER               TIMLEN */
/*              PARAMETER           ( TIMLEN = 50 ) */

/*              INTEGER               TYPLEN */
/*              PARAMETER           ( TYPLEN = 10 ) */

/*              INTEGER               NTYPES */
/*              PARAMETER           ( NTYPES = 2 ) */
/*        C */
/*        C     Local variables */
/*        C */
/*              CHARACTER*(CORLEN)    ABCORR */
/*              CHARACTER*(FRNMLN)    FIXREF */
/*              CHARACTER*(BDNMLN)    OBSRVR */
/*              CHARACTER*(BDNMLN)    SOURCE */
/*              CHARACTER*(BDNMLN)    TARGET */
/*              CHARACTER*(TYPLEN)    TRMTPS ( NTYPES ) */
/*              CHARACTER*(TIMLEN)    UTC */

/*              DOUBLE PRECISION      ANGRAD */
/*              DOUBLE PRECISION      EMISSN */
/*              DOUBLE PRECISION      ET */
/*              DOUBLE PRECISION      LAT */
/*              DOUBLE PRECISION      LON */
/*              DOUBLE PRECISION      LT */
/*              DOUBLE PRECISION      OBSPOS ( 3 ) */
/*              DOUBLE PRECISION      PHASE */
/*              DOUBLE PRECISION      RADIUS */
/*              DOUBLE PRECISION      S      ( NTYPES ) */
/*              DOUBLE PRECISION      SOLAR */
/*              DOUBLE PRECISION      SRCPOS ( 3 ) */
/*              DOUBLE PRECISION      SRCRAD ( 3 ) */
/*              DOUBLE PRECISION      SRFVEC ( 3 ) */
/*              DOUBLE PRECISION      TRGEPC */
/*              DOUBLE PRECISION      TRMPTS ( 3, NPTS ) */

/*              INTEGER               I */
/*              INTEGER               N */
/*              INTEGER               TRMIDX */

/*              LOGICAL               FIRST */

/*        C */
/*        C     Initial values */
/*        C */
/*              DATA                  FIRST  / .TRUE.               / */
/*              DATA                  TRMTPS / 'UMBRAL', 'PENUMBRAL'/ */
/*              DATA                  S      / -1.D0,    1.D0       / */

/*        C */
/*        C     Load the meta-kernel. */
/*        C */
/*              CALL FURNSH ( META ) */

/*        C */
/*        C     Set the observation time. */
/*        C */
/*              UTC    = '2007 FEB 3 00:00:00.000' */

/*              CALL STR2ET ( UTC, ET ) */

/*        C */
/*        C     Set the participating objects, the reference */
/*        C     frame, and the aberration correction. */
/*        C */
/*              OBSRVR = 'EARTH' */
/*              TARGET = 'MOON' */
/*              SOURCE = 'SUN' */
/*              FIXREF = 'IAU_MOON' */
/*              ABCORR = 'LT+S' */
/*        C */
/*        C     Look up the radii of the Sun. */
/*        C */
/*              CALL BODVRD ( SOURCE, 'RADII', 3, N, SRCRAD ) */

/*        C */
/*        C     Compute terminator points. */
/*        C */
/*              DO TRMIDX = 1, 2 */

/*                 CALL EDTERM ( TRMTPS(TRMIDX), SOURCE, TARGET, */
/*             .                 ET,             FIXREF, ABCORR, */
/*             .                 OBSRVR,         NPTS,   TRGEPC, */
/*             .                 OBSPOS,         TRMPTS          ) */
/*        C */
/*        C        Validate terminator points. */
/*        C */
/*        C        Look up the target-sun vector at the light-time */
/*        C        corrected target epoch. */
/*        C */
/*                 IF ( FIRST ) THEN */

/*                    CALL SPKPOS ( SOURCE, TRGEPC, FIXREF, */
/*             .                    ABCORR, TARGET, SRCPOS, LT ) */
/*                    FIRST = .FALSE. */

/*                 END IF */


/*                 WRITE (*,*) ' ' */
/*                 WRITE (*,*) 'Terminator type: '//TRMTPS(TRMIDX) */

/*                 DO I = 1, NPTS */

/*                    WRITE (*,*) ' ' */

/*                    CALL RECLAT ( TRMPTS(1,I), RADIUS, LON, LAT ) */

/*                    WRITE (*,FMT0) '  Terminator point ', I, ':' */
/*                    WRITE (*,FMT1) */
/*             .            '    Radius                     (km):  ', */
/*             .            RADIUS */
/*                    WRITE (*,FMT1) */
/*             .            '    Planetocentric longitude   (deg): ', */
/*             .            LON*DPR() */
/*                    WRITE (*,FMT1) */
/*             .            '    Planetocentric latitude    (deg): ', */
/*             .            LAT*DPR() */

/*        C */
/*        C           Find the illumination angles at the */
/*        C           Ith terminator point. */
/*        C */
/*                    CALL ILUMIN ( 'Ellipsoid',  TARGET, ET, */
/*             .                     FIXREF,      ABCORR, OBSRVR, */
/*             .                     TRMPTS(1,I), TRGEPC, SRFVEC, */
/*             .                     PHASE,       SOLAR,  EMISSN ) */

/*                    WRITE (*,FMT1) */
/*             .            '    Solar incidence angle      (deg): ', */
/*             .            SOLAR*DPR() */
/*        C */
/*        C           Display the solar incidence angle after */
/*        C           adjusting the angle for the angular radius */
/*        C           of the Sun as seen from the Ith terminator */
/*        C           point. The result should be approximately */
/*        C           90 degrees. */
/*        C */
/*                    ANGRAD = ASIN(   SRCRAD(1) */
/*             .                     / VDIST( SRCPOS, TRMPTS(1,I) ) ) */

/*                    WRITE (*, '(1X,A)' ) */
/*             .            '    Solar incidence angle adjusted for' */
/*                    WRITE (*,FMT1) */
/*             .            '    sun''s angular radius (deg):       ', */
/*             .            (SOLAR + S(TRMIDX)*ANGRAD) * DPR() */
/*                 END DO */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Terminator type: UMBRAL */

/*           Terminator point  1: */
/*             Radius                     (km):      1737.400000000 */
/*             Planetocentric longitude   (deg):      -95.084552819 */
/*             Planetocentric latitude    (deg):        0.004052763 */
/*             Solar incidence angle      (deg):       90.269765815 */
/*             Solar incidence angle adjusted for */
/*             sun's angular radius (deg):             90.000000125 */

/*           Terminator point  2: */
/*             Radius                     (km):      1737.400000000 */
/*             Planetocentric longitude   (deg):       84.228091534 */
/*             Planetocentric latitude    (deg):       59.995755519 */
/*             Solar incidence angle      (deg):       90.269765709 */
/*             Solar incidence angle adjusted for */
/*             sun's angular radius (deg):             90.000000019 */

/*           Terminator point  3: */
/*             Radius                     (km):      1737.400000000 */
/*             Planetocentric longitude   (deg):       87.216417974 */
/*             Planetocentric latitude    (deg):      -59.979550515 */
/*             Solar incidence angle      (deg):       90.269765733 */
/*             Solar incidence angle adjusted for */
/*             sun's angular radius (deg):             90.000000043 */

/*         Terminator type: PENUMBRAL */

/*           Terminator point  1: */
/*             Radius                     (km):      1737.400000000 */
/*             Planetocentric longitude   (deg):       84.914100511 */
/*             Planetocentric latitude    (deg):       -0.004073047 */
/*             Solar incidence angle      (deg):       89.730234402 */
/*             Solar incidence angle adjusted for */
/*             sun's angular radius (deg):             90.000000122 */

/*           Terminator point  2: */
/*             Radius                     (km):      1737.400000000 */
/*             Planetocentric longitude   (deg):      -95.769215814 */
/*             Planetocentric latitude    (deg):      -59.995785101 */
/*             Solar incidence angle      (deg):       89.730234301 */
/*             Solar incidence angle adjusted for */
/*             sun's angular radius (deg):             90.000000021 */

/*           Terminator point  3: */
/*             Radius                     (km):      1737.400000000 */
/*             Planetocentric longitude   (deg):      -92.780892017 */
/*             Planetocentric latitude    (deg):       59.979498997 */
/*             Solar incidence angle      (deg):       89.730234325 */
/*             Solar incidence angle adjusted for */
/*             sun's angular radius (deg):             90.000000044 */


/* $ Restrictions */

/*     1)  This routine models light paths as straight lines. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.0, 01-NOV-2021 (EDW) (JDR) */

/*        Body radii accessed from kernel pool using ZZGFTREB. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.1.0, 31-MAR-2014 (NJB) (BVS) */

/*        A correction was made to the $Detailed_Output section of */
/*        the header: the subroutine name VMINUS was changed to VSUB. */

/*        The header example program was re-written. The metakernel for */
/*        the example program has been updated, as was the program's */
/*        output. */

/*        Various portions of the header were re-written. */

/*        Updated to save the input body names and ZZBODTRN state */
/*        counters and to do name-ID conversions only if the counters */
/*        have changed. */

/*        Updated to save the input frame name and POOL state counter */
/*        and to do frame name-ID conversion only if the counter has */
/*        changed. */

/* -    SPICELIB Version 1.0.0, 03-FEB-2007 (NJB) */

/* -& */
/* $ Index_Entries */

/*     find terminator on ellipsoid */
/*     find umbral terminator on ellipsoid */
/*     find penumbral terminator on ellipsoid */

/* -& */

/*     SPICELIB functions */


/*     Saved body name length. */


/*     Saved frame name length. */


/*     Local variables */


/*     Saved name/ID item declarations. */


/*     Saved frame name/ID item declarations. */


/*     Saved name/ID items. */


/*     Saved frame name/ID items. */


/*     Initial values. */


/*     Standard SPICELIB error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("EDTERM", (ftnlen)6);

/*     Initialization. */

    if (first) {

/*        Initialize counters. */

	zzctruin_(svctr1);
	zzctruin_(svctr2);
	zzctruin_(svctr3);
	zzctruin_(svctr4);
	first = FALSE_;
    }

/*     Get the input frame code and frame info. */

    zznamfrm_(svctr4, svfref, &svfrcd, fixref, &frcode, (ftnlen)32, 
	    fixref_len);
    if (frcode == 0) {
	setmsg_("Input frame # has no associated frame ID code.", (ftnlen)46);
	errch_("#", fixref, (ftnlen)1, fixref_len);
	sigerr_("SPICE(NOTRANSLATION)", (ftnlen)20);
	chkout_("EDTERM", (ftnlen)6);
	return 0;
    }
    frinfo_(&frcode, &center, &frclas, &clssid, &found);
    if (! found) {
	setmsg_("Input frame # has associated frame ID code #, but no info w"
		"as found by FRINFO for this frame.", (ftnlen)93);
	errch_("#", fixref, (ftnlen)1, fixref_len);
	errint_("#", &frcode, (ftnlen)1);
	sigerr_("SPICE(BUG)", (ftnlen)10);
	chkout_("EDTERM", (ftnlen)6);
	return 0;
    }

/*     Get the ID codes of the target, source, and observer. */

    zzbods2c_(svctr1, svtarg, &svtgid, &svfnd1, target, &trgid, &found, (
	    ftnlen)36, target_len);
    if (! found) {
	setmsg_("Input target # has no associated body ID code.", (ftnlen)46);
	errch_("#", target, (ftnlen)1, target_len);
	sigerr_("SPICE(NOTRANSLATION)", (ftnlen)20);
	chkout_("EDTERM", (ftnlen)6);
	return 0;
    }
    zzbods2c_(svctr2, svscre, &svsrci, &svfnd2, source, &srcid, &found, (
	    ftnlen)36, source_len);
    if (! found) {
	setmsg_("Input source # has no associated body ID code.", (ftnlen)46);
	errch_("#", source, (ftnlen)1, source_len);
	sigerr_("SPICE(NOTRANSLATION)", (ftnlen)20);
	chkout_("EDTERM", (ftnlen)6);
	return 0;
    }
    zzbods2c_(svctr3, svobsr, &svobsi, &svfnd3, obsrvr, &obsid, &found, (
	    ftnlen)36, obsrvr_len);
    if (! found) {
	setmsg_("Input observer # has no associated body ID code.", (ftnlen)
		48);
	errch_("#", obsrvr, (ftnlen)1, obsrvr_len);
	sigerr_("SPICE(NOTRANSLATION)", (ftnlen)20);
	chkout_("EDTERM", (ftnlen)6);
	return 0;
    }

/*     If the frame is not centered on the target, reject it. */

    if (center != trgid) {
	setmsg_("Input frame # is not centered on target body #. This frame "
		"must be a body-fixed frame associated with the target.", (
		ftnlen)113);
	errch_("#", fixref, (ftnlen)1, fixref_len);
	errch_("#", target, (ftnlen)1, target_len);
	sigerr_("SPICE(INVALIDFIXREF)", (ftnlen)20);
	chkout_("EDTERM", (ftnlen)6);
	return 0;
    }

/*     Look up the radii associated with the target body. */

    zzgftreb_(&trgid, trgrad);
    if (failed_()) {
	chkout_("EDTERM", (ftnlen)6);
	return 0;
    }

/*     Look up the radii associated with the light source. */

    zzgftreb_(&srcid, srcrad);
    if (failed_()) {
	chkout_("EDTERM", (ftnlen)6);
	return 0;
    }
/* Computing MAX */
    d__1 = max(srcrad[0],srcrad[1]);
    r__ = max(d__1,srcrad[2]);

/*     Look up the observer-target vector and the target-source vector. */
/*     Also set the output OBSPOS. */

    spkezp_(&trgid, et, fixref, abcorr, &obsid, trgpos, &lttarg, fixref_len, 
	    abcorr_len);
    if (failed_()) {
	chkout_("EDTERM", (ftnlen)6);
	return 0;
    }
    zzcorepc_(abcorr, et, &lttarg, trgepc, abcorr_len);
    vminus_(trgpos, obspos);
    spkezp_(&srcid, trgepc, fixref, abcorr, &trgid, srcpos, &ltsrc, 
	    fixref_len, abcorr_len);

/*     We're ready to compute the terminator. */

    zzedterm_(trmtyp, trgrad, &trgrad[1], &trgrad[2], &r__, srcpos, npts, 
	    trmpts, trmtyp_len);
    chkout_("EDTERM", (ftnlen)6);
    return 0;
} /* edterm_ */

