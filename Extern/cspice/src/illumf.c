/* illumf.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__100 = 100;
static doublereal c_b53 = 1.;

/* $Procedure ILLUMF ( Illumination angles, general source, return flags ) */
/* Subroutine */ int illumf_(char *method, char *target, char *ilusrc, 
	doublereal *et, char *fixref, char *abcorr, char *obsrvr, doublereal *
	spoint, doublereal *trgepc, doublereal *srfvec, doublereal *phase, 
	doublereal *incdnc, doublereal *emissn, logical *visibl, logical *lit,
	 ftnlen method_len, ftnlen target_len, ftnlen ilusrc_len, ftnlen 
	fixref_len, ftnlen abcorr_len, ftnlen obsrvr_len)
{
    /* Initialized data */

    static logical first = TRUE_;
    static logical pri = FALSE_;
    static integer nsurf = 0;
    static char prvcor[5] = "     ";
    static char prvmth[500] = "                                             "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "       ";
    static integer shape = 0;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int zzbods2c_(integer *, char *, integer *, 
	    logical *, char *, integer *, logical *, ftnlen, ftnlen);
    extern doublereal vsep_(doublereal *, doublereal *);
    extern /* Subroutine */ int vequ_(doublereal *, doublereal *);
    integer type__;
    static logical xmit;
    extern /* Subroutine */ int zzgftreb_(integer *, doublereal *), zzmaxrad_(
	    doublereal *), zznamfrm_(integer *, char *, integer *, char *, 
	    integer *, ftnlen, ftnlen), zzvalcor_(char *, logical *, ftnlen), 
	    zzsbfnrm_(integer *, integer *, integer *, doublereal *, integer *
	    , doublereal *, doublereal *), zzsudski_(integer *, integer *, 
	    integer *, integer *), zzctruin_(integer *), zzprsmet_(integer *, 
	    char *, integer *, char *, char *, logical *, integer *, integer *
	    , char *, char *, ftnlen, ftnlen, ftnlen, ftnlen, ftnlen), 
	    zzsrftrk_(integer *, logical *), zzraysfx_(doublereal *, 
	    doublereal *, doublereal *, doublereal *, logical *);
    doublereal s, scale, radii[3];
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen);
    logical found;
    extern /* Subroutine */ int vlcom_(doublereal *, doublereal *, doublereal 
	    *, doublereal *, doublereal *);
    extern logical eqstr_(char *, char *, ftnlen, ftnlen);
    static logical uselt, svfnd1, svfnd2;
    static integer svctr1[2], svctr2[2];
    extern logical failed_(void);
    static integer svctr3[2], svctr4[2];
    doublereal lt;
    integer obscde;
    extern doublereal halfpi_(void);
    integer fixfid;
    static integer trgcde;
    doublereal maxrad;
    extern logical return_(void);
    char pntdef[20], shpstr[9], subtyp[20], trmstr[20];
    doublereal normal[3], obspos[3], opstat[6], tistat[6], vertex[3];
    static integer center, srflst[100];
    integer svnsrf, typeid;
    logical attblk[15], surfup;
    static char svtarg[36];
    logical fnd;
    static integer svtcde;
    static char svobsr[36];
    static integer svobsc;
    static char svfref[32];
    static integer svrefc;
    extern /* Subroutine */ int chkout_(char *, ftnlen), setmsg_(char *, 
	    ftnlen), sigerr_(char *, ftnlen), frinfo_(integer *, integer *, 
	    integer *, integer *, logical *), errint_(char *, integer *, 
	    ftnlen), spkcpt_(doublereal *, char *, char *, doublereal *, char 
	    *, char *, char *, char *, doublereal *, doublereal *, ftnlen, 
	    ftnlen, ftnlen, ftnlen, ftnlen, ftnlen), spkcpo_(char *, 
	    doublereal *, char *, char *, char *, doublereal *, char *, char *
	    , doublereal *, doublereal *, ftnlen, ftnlen, ftnlen, ftnlen, 
	    ftnlen, ftnlen), vminus_(doublereal *, doublereal *), surfnm_(
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *), vhatip_(doublereal *);
    doublereal lti, xpt[3];

/* $ Abstract */

/*     Compute the illumination angles---phase, incidence, and */
/*     emission---at a specified point on a target body. Return logical */
/*     flags indicating whether the surface point is visible from */
/*     the observer's position and whether the surface point is */
/*     illuminated. */

/*     The target body's surface is represented using topographic data */
/*     provided by DSK files, or by a reference ellipsoid. */

/*     The illumination source is a specified ephemeris object. */

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

/*     File: dsk.inc */


/*     Version 1.0.0 05-FEB-2016 (NJB) */

/*     Maximum size of surface ID list. */


/*     End of include file dsk.inc */

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
/*     METHOD     I   Computation method. */
/*     TARGET     I   Name of target body. */
/*     ILUSRC     I   Name of illumination source. */
/*     ET         I   Epoch in ephemeris seconds past J2000 TDB. */
/*     FIXREF     I   Body-fixed, body-centered target body frame. */
/*     ABCORR     I   Desired aberration correction. */
/*     OBSRVR     I   Name of observing body. */
/*     SPOINT     I   Body-fixed coordinates of a target surface point. */
/*     TRGEPC     O   Target surface point epoch. */
/*     SRFVEC     O   Vector from observer to target surface point. */
/*     PHASE      O   Phase angle at the surface point. */
/*     INCDNC     O   Source incidence angle at the surface point. */
/*     EMISSN     O   Emission angle at the surface point. */
/*     VISIBL     O   Visibility flag (.TRUE. = visible). */
/*     LIT        O   Illumination flag (.TRUE. = illuminated). */

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
/*              except within double-quoted strings representing */
/*              surfaces. For example, the string ' eLLipsoid ' is valid. */

/*              Within double-quoted strings representing surfaces, blank */
/*              characters are significant, but multiple consecutive */
/*              blanks are considered equivalent to a single blank. Case */
/*              is not significant. So */

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

/*     ILUSRC   is the name of the illumination source. This source */
/*              may be any ephemeris object. Case, blanks, and */
/*              numeric values are treated in the same way as for the */
/*              input TARGET. */

/*     ET       is the epoch, expressed as seconds past J2000 TDB, */
/*              for which the apparent illumination angles at the */
/*              specified surface point on the target body, as seen */
/*              from the observing body, are to be computed. */

/*     FIXREF   is the name of a body-fixed reference frame centered */
/*              on the target body. FIXREF may be any such frame */
/*              supported by the SPICE system, including built-in */
/*              frames (documented in the Frames Required Reading) */
/*              and frames defined by a loaded frame kernel (FK). The */
/*              string FIXREF is case-insensitive, and leading and */
/*              trailing blanks in FIXREF are not significant. */

/*              The input surface point SPOINT and the output vector */
/*              SRFVEC are expressed relative to this reference */
/*              frame. */

/*     ABCORR   is the aberration correction to be used in computing */
/*              the position and orientation of the target body and */
/*              the location of the illumination source. */

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
/*                            of the illumination source as seen by */
/*                            the target, for light time. Correct the */
/*                            orientation of the target for light */
/*                            time. */

/*                 'LT+S'     Correct both the position of SPOINT as */
/*                            seen by the observer, and the position */
/*                            of the illumination source as seen by */
/*                            the target, for light time and stellar */
/*                            aberration. Correct the orientation of */
/*                            the target for light time. */

/*                 'CN'       Converged Newtonian light time */
/*                            correction. In solving the light time */
/*                            equations for SPOINT and the */
/*                            illumination source, the 'CN' */
/*                            correction iterates until the solution */
/*                            converges. */

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
/*              FIXREF at time TRGEPC to a time-dependent reference frame */
/*              REF at ET, the routine PXFRM2 should be called. For */
/*              example, let XFORM be 3x3 matrix representing the */
/*              rotation from the body-fixed reference frame FIXREF at */
/*              time TRGEPC to the time-dependent frame REF at time ET. */
/*              Then SRFVEC can be transformed to the result REFVEC as */
/*              follows: */

/*                  CALL PXFRM2 ( FIXREF, REF,    TRGEPC, ET, XFORM ) */
/*                  CALL MXV    ( XFORM,  SRFVEC, REFVEC ) */


/*     The following outputs depend on the existence of a well-defined */
/*     outward normal vector to the surface at SPOINT. See restriction 1. */


/*     PHASE    is the phase angle at SPOINT, as seen from OBSRVR at time */
/*              ET. This is the angle between the negative of the vector */
/*              SRFVEC and the SPOINT-illumination source vector at */
/*              TRGEPC. Units are radians. The range of PHASE is [0, pi]. */
/*              See $Particulars below for a detailed discussion of the */
/*              definition. */

/*     INCDNC   is the illumination source incidence angle at SPOINT, as */
/*              seen from OBSRVR at time ET. This is the angle between */
/*              the surface normal vector at SPOINT and the SPOINT-source */
/*              vector at TRGEPC. Units are radians. The range of INCDNC */
/*              is [0, pi]. See $Particulars below for a detailed */
/*              discussion of the definition. */

/*     EMISSN   is the emission angle at SPOINT, as seen from OBSRVR at */
/*              time ET. This is the angle between the surface normal */
/*              vector at SPOINT and the negative of the vector SRFVEC. */
/*              Units are radians. The range of EMISSN is [0, pi]. See */
/*              $Particulars below for a detailed discussion of the */
/*              definition. */

/*     VISIBL   is a logical flag indicating whether the surface */
/*              point is visible to the observer. VISIBL takes into */
/*              account whether the target surface occults SPOINT, */
/*              regardless of the emission angle at SPOINT. VISIBL is */
/*              returned with the value .TRUE. if SPOINT is visible; */
/*              otherwise it is .FALSE. */

/*     LIT      is a logical flag indicating whether the surface */
/*              point is illuminated; the point is considered to be */
/*              illuminated if the vector from the point to the */
/*              center of the illumination source doesn't intersect */
/*              the target surface. LIT takes into account whether */
/*              the target surface casts a shadow on SPOINT, */
/*              regardless of the incidence angle at SPOINT. LIT is */
/*              returned with the value .TRUE. if SPOINT is */
/*              illuminated; otherwise it is .FALSE. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the specified aberration correction is relativistic or */
/*         calls for stellar aberration but not light time correction, */
/*         the error SPICE(NOTSUPPORTED) is signaled. */

/*     2)  If the specified aberration correction is any other */
/*         unrecognized value, an error is signaled */
/*         by a routine in the call tree of this routine. */

/*     3)  If any of the target, observer, or illumination source */
/*         input strings cannot be converted to an integer ID code, the */
/*         error SPICE(IDCODENOTFOUND) is signaled. */

/*     4)  If OBSRVR and TARGET map to the same NAIF integer ID code, */
/*         the error SPICE(BODIESNOTDISTINCT) is signaled. */

/*     5)  If the input target body-fixed frame FIXREF is not */
/*         recognized, the error SPICE(NOFRAME) is signaled. A frame */
/*         name may fail to be recognized because a required frame */
/*         specification kernel has not been loaded; another cause is a */
/*         misspelling of the frame name. */

/*     6)  If the input frame FIXREF is not centered at the target body, */
/*         the error SPICE(INVALIDFRAME) is signaled. */

/*     7)  If the input argument METHOD cannot be parsed, an error */
/*         is signaled by either this routine or a routine in the */
/*         call tree of this routine. */

/*     8)  If insufficient ephemeris data have been loaded prior to */
/*         calling ILLUMF, an error is signaled by a */
/*         routine in the call tree of this routine. Note that when */
/*         light time correction is used, sufficient ephemeris data must */
/*         be available to propagate the states of observer, target, and */
/*         the illumination source to the solar system barycenter. */

/*     9)  If the computation method specifies an ellipsoidal target */
/*         shape and triaxial radii of the target body have not been */
/*         loaded into the kernel pool prior to calling ILLUMF, an error */
/*         is signaled by a routine in the call tree of this routine. */

/*     10) If PCK data specifying the target body-fixed frame orientation */
/*         have not been loaded prior to calling ILLUMF, an error is */
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

/*     14) If radii for TARGET are not found in the kernel pool, an error */
/*         is signaled by a routine in the call tree of this routine. */

/*     15) If the size of the TARGET body radii kernel variable is not */
/*         three, an error is signaled by a routine in the call tree of */
/*         this routine. */

/*     16) If any of the three TARGET body radii is less-than or equal to */
/*         zero, an error is signaled by a routine in the call tree of */
/*         this routine. */

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

/*        ILLUMF   (this routine) */

/*        ILLUMG   (same as this routine, except that */
/*                 output flags are not returned.) */

/*        ILUMIN   (same as ILLUMG, except that the sun is fixed */
/*                 as the illumination source.) */

/*        ILLUM    (deprecated) */

/*     This routine is the most capable of the set. */


/*     Illumination angles */
/*     =================== */

/*     The term "illumination angles" refers to the following set of */
/*     angles: */


/*        phase angle              Angle between the vectors from the */
/*                                 surface point to the observer and */
/*                                 from the surface point to the */
/*                                 illumination source. */

/*        incidence angle          Angle between the surface normal at */
/*                                 the specified surface point and the */
/*                                 vector from the surface point to the */
/*                                 illumination source. */

/*        emission angle           Angle between the surface normal at */
/*                                 the specified surface point and the */
/*                                 vector from the surface point to the */
/*                                 observer. */

/*     The diagram below illustrates the geometric relationships */
/*     defining these angles. The labels for the incidence, emission, */
/*     and phase angles are "inc.", "e.", and "phase". */



/*                                                      * */
/*                                              illumination source */

/*                    surface normal vector */
/*                              ._                 _. */
/*                              |\                 /|  illumination */
/*                                \    phase      /    source vector */
/*                                 \   .    .    / */
/*                                 .            . */
/*                                   \   ___   / */
/*                              .     \/     \/ */
/*                                    _\ inc./ */
/*                             .    /   \   / */
/*                             .   |  e. \ / */
/*         *             <--------------- *  surface point on */
/*      viewing            vector            target body */
/*      location           to viewing */
/*      (observer)         location */



/*     Note that if the target-observer vector, the target normal vector */
/*     at the surface point, and the target-illumination source vector */
/*     are coplanar, then phase is the sum of the incidence and emission */
/*     angles. This rarely occurs; usually */

/*        phase angle  <  incidence angle + emission angle */

/*     All of the above angles can be computed using light time */
/*     corrections, light time and stellar aberration corrections, or no */
/*     aberration corrections. In order to describe apparent geometry as */
/*     observed by a remote sensing instrument, both light time and */
/*     stellar aberration corrections should be used. */

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


/*           Target body -- illumination source vector */
/*           ----------------------------------------- */

/*           The surface features on the target body near SPOINT will */
/*           appear in a measurement made at ET as they were at ET-LT. */
/*           In particular, lighting on the target body is dependent on */
/*           the apparent location of the illumination source as seen */
/*           from the target body at ET-LT. So, a second light time */
/*           correction is used to compute the position of the */
/*           illumination source relative to the surface point. */


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

/*           Target body-illumination source vector */
/*           -------------------------------------- */

/*           The target body-illumination source vector is the apparent */
/*           position of the illumination source, corrected for light */
/*           time and stellar aberration, as seen from the target body */
/*           at time ET-LT. */


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

/*        'DSK/UNPRIORITIZED/SURFACES = "Mars MEGDR 64 PIXEL/DEG", 3' */


/*        Aberration corrections */
/*        ---------------------- */

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

/*        Use the meta-kernel shown below to load the required SPICE */
/*        kernels. */


/*           KPL/MK */

/*           File: illumf_ex1.tm */

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

/*           End of meta-kernel */


/*        Example code begins here. */


/*              PROGRAM ILLUMF_EX1 */
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
/*              PARAMETER           ( META   = 'illumf_ex1.tm' ) */

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

/*              LOGICAL               SSCLIT */
/*              LOGICAL               SSCVIS */
/*              LOGICAL               SSLLIT */
/*              LOGICAL               SSLVIS */

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
/*                 CALL ILLUMF ( ILUMTH(I), TARGET,  'SUN', */
/*             .                 ET,        FIXREF,  ABCORR, */
/*             .                 OBSRVR,    SSOLPT,  TRGEPC, */
/*             .                 SRFVEC,    SSLPHS,  SSLSOL, */
/*             .                 SSLEMI,    SSLVIS,  SSLLIT ) */
/*        C */
/*        C        Do the same for the sub-spacecraft point. */
/*        C */
/*                 CALL ILLUMF ( ILUMTH(I), TARGET,  'SUN', */
/*             .                 ET,        FIXREF,  ABCORR, */
/*             .                 OBSRVR,    SSCPT,   TRGEPC, */
/*             .                 SRFVEC,    SSCPHS,  SSCSOL, */
/*             .                 SSCEMI,    SSCVIS,  SSCLIT  ) */
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
/*                 WRITE (*,F2) '   ILLUMF method: '//ILUMTH(I) */
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
/*                 WRITE (*,F3) '      Visible, Lit flags:           ', */
/*             .                SSLVIS, SSLLIT */
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
/*                 WRITE (*,F3) '      Visible, Lit flags:           ', */
/*             .                SSCVIS, SSCLIT */
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

/*           ILLUMF method: Ellipsoid */
/*           SUBPNT method: Near Point/Ellipsoid */
/*           SUBSLR method: Near Point/Ellipsoid */

/*              Illumination angles at the sub-solar point: */

/*              Phase angle           (deg.):   138.370270685 */
/*              Solar incidence angle (deg.):     0.000000000 */
/*              Emission angle        (deg.):   138.370270685 */
/*              Visible, Lit flags:             F  T */

/*                The solar incidence angle should be 0. */
/*                The emission and phase angles should be equal. */

/*              Illumination angles at the sub-s/c point: */

/*              Phase angle           (deg.):   101.439331040 */
/*              Solar incidence angle (deg.):   101.439331041 */
/*              Emission angle        (deg.):     0.000000002 */
/*              Visible, Lit flags:             T  F */

/*                The emission angle should be 0. */
/*                The solar incidence and phase angles should be equal. */

/*           ILLUMF method: DSK/Unprioritized */
/*           SUBPNT method: DSK/Nadir/Unprioritized */
/*           SUBSLR method: DSK/Nadir/Unprioritized */

/*              Illumination angles at the sub-solar point: */

/*              Phase angle           (deg.):   138.387071677 */
/*              Solar incidence angle (deg.):     0.967122745 */
/*              Emission angle        (deg.):   137.621480599 */
/*              Visible, Lit flags:             F  T */

/*              Illumination angles at the sub-s/c point: */

/*              Phase angle           (deg.):   101.439331359 */
/*              Solar incidence angle (deg.):   101.555993667 */
/*              Emission angle        (deg.):     0.117861156 */
/*              Visible, Lit flags:             T  F */


/* $ Restrictions */

/*     1)  Results from this routine are not meaningful if the input */
/*         point lies on a ridge or vertex of a surface represented by */
/*         DSK data, or if for any other reason the direction of the */
/*         outward normal vector at the point is undefined. */

/*     2)  The illumination state indicated by the output argument `lit' */
/*         is computed treating the illumination source as a single */
/*         point. Surface points that are illuminated by part of the */
/*         source are classified as "lit" or not depending on whether the */
/*         center of the source is visible from those points. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.1.0, 20-NOV-2021 (JDR) (EDW) (NJB) */

/*        Bug fix: PRVCOR is no longer set to blank before */
/*        ABCORR is parsed. */

/*        Body radii accessed from kernel pool using ZZGFTREB. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 2.0.0, 04-APR-2017 (NJB) (BVS) */

/*        07-APR-2016 (NJB) (BVS) */

/*        Now uses surface mapping tracking capability. */

/*        Updated surface ID codes in header comments. */

/*       30-MAR-2015 (NJB) */

/*        Now uses illumination angles to determine whether */
/*        self-intersection tests are necessary, for the DSK */
/*        case. Now imports SHPLEN parameter from gf.inc. */

/*        Original version 09-FEB-2015 (NJB) (BVS) */

/* -& */
/* $ Index_Entries */

/*     illumination angles general source with flags */
/*     lighting angles general source with flags */
/*     phase angle general source with flags */
/*     incidence angle general source with flags */
/*     emission angle general source with flags */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Saved body name length. */


/*     Saved frame name length. */


/*     Local variables */


/*     Saved name/ID item declarations. */


/*     Saved frame name/ID item declarations. */


/*     Saved surface name/ID item declarations. */


/*     Saved variables */


/*     Saved name/ID items. */


/*     Saved frame name/ID items. */


/*     Saved surface name/ID items. */


/*     Note: XMIT need not be saved, since it's used only */
/*     for error checking when an aberration correction flag */
/*     is parsed. */


/*     Initial values */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("ILLUMF", (ftnlen)6);

/*     Counter initialization is done separately. */

    if (first) {

/*        Initialize counters. */

	zzctruin_(svctr1);
	zzctruin_(svctr2);
	zzctruin_(svctr3);
    }

/*     If necessary, parse the aberration correction flag. */

    if (first || s_cmp(abcorr, prvcor, abcorr_len, (ftnlen)5) != 0) {

/*        The aberration correction flag differs from the value it */
/*        had on the previous call, if any. Analyze the new flag. */

	zzvalcor_(abcorr, attblk, abcorr_len);
	if (failed_()) {
	    chkout_("ILLUMF", (ftnlen)6);
	    return 0;
	}
/*        Set logical flags indicating the attributes of the requested */
/*        correction: */

/*           XMIT is .TRUE. when the correction is for transmitted */
/*           radiation. */

/*           USELT is .TRUE. when any type of light time correction */
/*           (normal or converged Newtonian) is specified. */

/*           USESTL indicates stellar aberration corrections. */


/*        The above definitions are consistent with those used by */
/*        ZZVALCOR. */

	xmit = attblk[4];
	uselt = attblk[1];

/*        The aberration correction flag is recognized; save it. */

	s_copy(prvcor, abcorr, (ftnlen)5, abcorr_len);

/*        We do NOT set FIRST to .FALSE. here, since we're not */
/*        yet done with it. */

    }

/*     Get the target ID code here, since it will be needed */
/*     for the initialization calls below. */

    zzbods2c_(svctr1, svtarg, &svtcde, &svfnd1, target, &trgcde, &fnd, (
	    ftnlen)36, target_len);
    if (! fnd) {
	setmsg_("The target, '#', is not a recognized name for an ephemeris "
		"object. The cause of this problem may be that you need an up"
		"dated version of the SPICE Toolkit, or that you failed to lo"
		"ad a kernel containing a name-ID mapping for this body.", (
		ftnlen)234);
	errch_("#", target, (ftnlen)1, target_len);
	sigerr_("SPICE(IDCODENOTFOUND)", (ftnlen)21);
	chkout_("ILLUMF", (ftnlen)6);
	return 0;
    }

/*     Check whether the surface name/ID mapping has been updated. */

    zzsrftrk_(svctr4, &surfup);

/*     If necessary, parse the method specification. PRVMTH */
/*     and the derived flags NEAR and ELIPSD start out with */
/*     valid values. PRVMTH records the last valid value of */
/*     METHOD; ELIPSD is the corresponding shape flag. */

    if (first || surfup || s_cmp(method, prvmth, method_len, (ftnlen)500) != 
	    0) {

/*        Set the previous method string to an invalid value, so it */
/*        cannot match any future, valid input. This will force this */
/*        routine to parse the input method on the next call if any */
/*        failure occurs in this branch. Once success is assured, we can */
/*        record the current method in the previous method string. */

	s_copy(prvmth, " ", (ftnlen)500, (ftnlen)1);

/*        Parse the method string. If the string is valid, the */
/*        outputs SHAPE and SUBTYP will always be be set. However, */
/*        SUBTYP is not used in this routine. */

/*        For DSK shapes, the surface list array and count will be set */
/*        if the method string contains a surface list. */

	zzprsmet_(&trgcde, method, &c__100, shpstr, subtyp, &pri, &nsurf, 
		srflst, pntdef, trmstr, method_len, (ftnlen)9, (ftnlen)20, (
		ftnlen)20, (ftnlen)20);
	if (failed_()) {
	    chkout_("ILLUMF", (ftnlen)6);
	    return 0;
	}
	if (eqstr_(shpstr, "ELLIPSOID", (ftnlen)9, (ftnlen)9)) {
	    shape = 1;
	} else if (eqstr_(shpstr, "DSK", (ftnlen)9, (ftnlen)3)) {
	    shape = 2;
	} else {

/*           This is a backstop check. */

	    setmsg_("Returned shape value from method string was <#>.", (
		    ftnlen)48);
	    errch_("#", shpstr, (ftnlen)1, (ftnlen)9);
	    sigerr_("SPICE(BUG)", (ftnlen)10);
	    chkout_("ILLUMF", (ftnlen)6);
	    return 0;
	}

/*        There should be no subtype specification in the method */
/*        string. */

	if (s_cmp(subtyp, " ", (ftnlen)20, (ftnlen)1) != 0) {
	    setmsg_("Spurious sub-observer point type <#> was present in the"
		    " method string #. The sub-observer type is valid in the "
		    "method strings for SUBPNT and SUBSLR, but is not applica"
		    "ble for ILLUMF.", (ftnlen)182);
	    errch_("#", subtyp, (ftnlen)1, (ftnlen)20);
	    errch_("#", method, (ftnlen)1, method_len);
	    sigerr_("SPICE(INVALIDMETHOD)", (ftnlen)20);
	    chkout_("ILLUMF", (ftnlen)6);
	    return 0;
	}
	s_copy(prvmth, method, (ftnlen)500, method_len);
    }

/*     We're done with all tasks that must be executed on the first */
/*     pass. */

    first = FALSE_;

/*     Obtain integer codes for the observer and */
/*     illumination source. */

    zzbods2c_(svctr2, svobsr, &svobsc, &svfnd2, obsrvr, &obscde, &fnd, (
	    ftnlen)36, obsrvr_len);
    if (! fnd) {
	setmsg_("The observer, '#', is not a recognized name for an ephemeri"
		"s object. The cause of this problem may be that you need an "
		"updated version of the SPICE Toolkit, or that you failed to "
		"load a kernel containing a name-ID mapping for this body.", (
		ftnlen)236);
	errch_("#", obsrvr, (ftnlen)1, obsrvr_len);
	sigerr_("SPICE(IDCODENOTFOUND)", (ftnlen)21);
	chkout_("ILLUMF", (ftnlen)6);
	return 0;
    }

/*     Check the observer and target body codes. If they are equal, */
/*     signal an error. */

    if (obscde == trgcde) {
	setmsg_("In computing illumination angles, the observing body and ta"
		"rget body are the same. Both are #.", (ftnlen)94);
	errch_("#", obsrvr, (ftnlen)1, obsrvr_len);
	sigerr_("SPICE(BODIESNOTDISTINCT)", (ftnlen)24);
	chkout_("ILLUMF", (ftnlen)6);
	return 0;
    }

/*     Determine the attributes of the frame designated by FIXREF. */

    zznamfrm_(svctr3, svfref, &svrefc, fixref, &fixfid, (ftnlen)32, 
	    fixref_len);
    frinfo_(&fixfid, &center, &type__, &typeid, &fnd);
    if (failed_()) {
	chkout_("ILLUMF", (ftnlen)6);
	return 0;
    }
    if (! fnd) {
	setmsg_("Reference frame # is not recognized by the SPICE frame subs"
		"ystem. Possibly a required frame definition kernel has not b"
		"een loaded.", (ftnlen)130);
	errch_("#", fixref, (ftnlen)1, fixref_len);
	sigerr_("SPICE(NOFRAME)", (ftnlen)14);
	chkout_("ILLUMF", (ftnlen)6);
	return 0;
    }

/*     Make sure that FIXREF is centered at the target body's center. */

    if (center != trgcde) {
	setmsg_("Reference frame # is not centered at the target body #. The"
		" ID code of the frame center is #.", (ftnlen)93);
	errch_("#", fixref, (ftnlen)1, fixref_len);
	errch_("#", target, (ftnlen)1, target_len);
	errint_("#", &center, (ftnlen)1);
	sigerr_("SPICE(INVALIDFRAME)", (ftnlen)19);
	chkout_("ILLUMF", (ftnlen)6);
	return 0;
    }

/*     Get the sign S prefixing LT in the expression for TRGEPC. */
/*     When light time correction is not used, setting S = 0 */
/*     allows us to seamlessly set TRGEPC equal to ET. */

    if (uselt) {
	if (xmit) {
	    s = 1.;
	} else {
	    s = -1.;
	}
    } else {
	s = 0.;
    }

/*     Look up the state of the surface point relative to the observer. */
/*     The body-fixed frame of the surface point is to be evaluated */
/*     at the epoch of the surface point, not at the epoch of the */
/*     center of the frame; we indicate this by setting the input */
/*     argument REFLOC to 'TARGET'. */

    spkcpt_(spoint, target, fixref, et, fixref, "TARGET", abcorr, obsrvr, 
	    opstat, &lt, target_len, fixref_len, fixref_len, (ftnlen)6, 
	    abcorr_len, obsrvr_len);
    if (failed_()) {
	chkout_("ILLUMF", (ftnlen)6);
	return 0;
    }

/*     TRGEPC is the epoch associated with the surface point. Below, */
/*     since S is set to 0.D0 if no aberration corrections are used, we */
/*     require no logical branch. */

    *trgepc = *et + s * lt;

/*     Now find the state of the illumination source as seen by */
/*     the surface point at TRGEPC. We want to evaluate the orientation */
/*     of the body-fixed frame of the surface point at the epoch */
/*     associated with the surface point, not at the epoch associated */
/*     with the frame's center;  we indicate this by setting the input */
/*     argument REFLOC to 'OBSERVER'. */

    spkcpo_(ilusrc, trgepc, fixref, "OBSERVER", abcorr, spoint, target, 
	    fixref, tistat, &lti, ilusrc_len, fixref_len, (ftnlen)8, 
	    abcorr_len, target_len, fixref_len);
    if (failed_()) {
	chkout_("ILLUMF", (ftnlen)6);
	return 0;
    }

/*     We'll need the negative of the observer-surface point position */
/*     for the following angle computation. Set the output SRFVEC while */
/*     we're at it. */

    vequ_(opstat, srfvec);
    vminus_(srfvec, obspos);

/*     Find the surface normal at SPOINT. This computation depends */
/*     on target body shape model. */

    if (shape == 1) {

/*        We'll need the radii of the target body. */

	zzgftreb_(&trgcde, radii);
	surfnm_(radii, &radii[1], &radii[2], spoint, normal);
	if (failed_()) {
	    chkout_("ILLUMF", (ftnlen)6);
	    return 0;
	}
    } else if (shape == 2) {

/*        Initialize the normal vector algorithm to use a DSK model */
/*        for the surface of the target body. This initialization is */
/*        required to enable later use of ZZRAYSFX and ZZMAXRAD. */

	svnsrf = 0;
	zzsudski_(&trgcde, &svnsrf, srflst, &fixfid);

/*        Compute the outward unit normal at SPOINT on the surface */
/*        defined by the designated DSK data. */

	zzsbfnrm_(&trgcde, &svnsrf, srflst, trgepc, &fixfid, spoint, normal);
	if (failed_()) {
	    chkout_("ILLUMF", (ftnlen)6);
	    return 0;
	}
	vhatip_(normal);
    } else {

/*        We've already checked the computation method input argument, */
/*        so we don't expect to arrive here. This code is present for */
/*        safety. */

	setmsg_("The computation method # was not recognized. ", (ftnlen)45);
	errch_("#", method, (ftnlen)1, method_len);
	sigerr_("SPICE(INVALIDMETHOD)", (ftnlen)20);
	chkout_("ILLUMF", (ftnlen)6);
	return 0;
    }

/*     Check for errors before calling math routines. */


/*     Find the illumination angles. VSEP will give us angular */
/*     separation in radians. */

    *phase = vsep_(obspos, tistat);
    *incdnc = vsep_(normal, tistat);
    *emissn = vsep_(normal, obspos);

/*     Set the visibility and illumination flags. */


/*     Set default values of VISIBL and LIT. */

    *visibl = *emissn <= halfpi_();
    *lit = *incdnc <= halfpi_();
    if (shape == 2) {

/*        There is a possibility that the surface-observer vector */
/*        or the surface-illumination source vector intersects the */
/*        surface. This is possible only if these vectors have */
/*        positive elevation. */
	if (*lit || *visibl) {

/*           We need to check for self-occultation of at least one of */
/*           the lines-of-sight from surface to observer or illumination */
/*           source. */

/*           We'll produce a point slightly above the surface point, */
/*           from which the visibility of the observer and light source */
/*           can be determined. We want to avoid detection of spurious */
/*           intersections near the input surface point. */

/*           Obtain an upper bound on the target body radius. */

	    zzmaxrad_(&maxrad);
	    if (failed_()) {
		chkout_("ILLUMF", (ftnlen)6);
		return 0;
	    }

/*           Compute an offset that can be used to produce a point */
/*           slightly above the surface point, and compute the "raised" */
/*           point. NORMAL is a unit vector here. */

	    scale = maxrad * 1e-10;
	    vlcom_(&c_b53, spoint, &scale, normal, vertex);
	    if (*visibl) {

/*              Find the surface intercept, if any, of a ray emanating */
/*              from VERTEX and pointing toward the observer. */

		zzraysfx_(vertex, obspos, trgepc, xpt, &found);
		*visibl = ! found;
	    }
	    if (*lit) {

/*              Find the surface intercept, if any, of a ray emanating */
/*              from VERTEX and pointing toward the illumination source. */

		zzraysfx_(vertex, tistat, trgepc, xpt, &found);
		*lit = ! found;
	    }
	}
    }

/*     TRGEPC and SRVFEC were already set before the illumination */
/*     angle computation. */

    chkout_("ILLUMF", (ftnlen)6);
    return 0;
} /* illumf_ */

