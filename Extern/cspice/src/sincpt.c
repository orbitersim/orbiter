/* sincpt.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__100 = 100;

/* $Procedure SINCPT ( Surface intercept ) */
/* Subroutine */ int sincpt_(char *method, char *target, doublereal *et, char 
	*fixref, char *abcorr, char *obsrvr, char *dref, doublereal *dvec, 
	doublereal *spoint, doublereal *trgepc, doublereal *srfvec, logical *
	found, ftnlen method_len, ftnlen target_len, ftnlen fixref_len, 
	ftnlen abcorr_len, ftnlen obsrvr_len, ftnlen dref_len)
{
    /* Initialized data */

    static logical first = TRUE_;
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
    static logical usecn = FALSE_;
    static logical uselt = FALSE_;
    static logical usestl = FALSE_;
    static logical xmit = FALSE_;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int zzbods2c_(integer *, char *, integer *, 
	    logical *, char *, integer *, logical *, ftnlen, ftnlen);
    extern /* Subroutine */ int zzmaxrad_();
    extern /* Subroutine */ int zznamfrm_(integer *, char *, integer *, char *
	    , integer *, ftnlen, ftnlen), zzvalcor_(char *, logical *, ftnlen)
	    , zzsuelin_(integer *), zzsudski_(integer *, integer *, integer *,
	     integer *), zzctruin_(integer *), zzsfxcor_(U_fp, U_fp, U_fp, 
	    integer *, doublereal *, char *, logical *, logical *, logical *, 
	    logical *, char *, integer *, integer *, integer *, integer *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, logical *,
	     ftnlen, ftnlen), zzprsmet_(integer *, char *, integer *, char *, 
	    char *, logical *, integer *, integer *, char *, char *, ftnlen, 
	    ftnlen, ftnlen, ftnlen, ftnlen), zzsrftrk_(integer *, logical *);
    extern /* Subroutine */ int zzraysfx_();
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    static integer shape;
    extern /* Subroutine */ int errch_(char *, char *, ftnlen, ftnlen);
    static integer nsurf;
    extern logical eqstr_(char *, char *, ftnlen, ftnlen), vzero_(doublereal *
	    );
    static logical svfnd1, svfnd2;
    static integer svctr1[2], svctr2[2];
    extern logical failed_(void);
    static integer svctr3[2], svctr4[2];
    integer dfrcde;
    static integer svctr5[2];
    integer fxfcde, obscde, dclass, trgcde;
    extern logical return_(void);
    char pntdef[20], shpstr[9], subtyp[20], trmstr[20];
    integer dcentr, dtypid, fxcent, fxclss, fxtyid;
    static integer srflst[100];
    logical attblk[15], surfup;
    static char svtarg[36];
    static integer svtcde;
    static char svobsr[36];
    static integer svobsc;
    static char svfref[32];
    static integer svfxfc;
    logical fnd;
    static char svdref[32];
    static integer svdfrc;
    extern /* Subroutine */ int chkout_(char *, ftnlen), setmsg_(char *, 
	    ftnlen), sigerr_(char *, ftnlen), frinfo_(integer *, integer *, 
	    integer *, integer *, logical *), errint_(char *, integer *, 
	    ftnlen);
    static logical pri;
    extern /* Subroutine */ int zzraynp_();

/* $ Abstract */

/*     Compute, for a given observer and a ray emanating from the */
/*     observer, the surface intercept of the ray on a target body at */
/*     a specified epoch, optionally corrected for light time and */
/*     stellar aberration. */

/*     The surface of the target body may be represented by a triaxial */
/*     ellipsoid or by topographic data provided by DSK files. */

/*     This routine supersedes SRFXPT. */

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
/*     NAIF_IDS */
/*     PCK */
/*     SCLK */
/*     SPK */
/*     TIME */

/* $ Keywords */

/*     GEOMETRY */

/* $ Declarations */

/*     File: dsk.inc */


/*     Version 1.0.0 05-FEB-2016 (NJB) */

/*     Maximum size of surface ID list. */


/*     End of include file dsk.inc */

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
/*     ET         I   Epoch in ephemeris seconds past J2000 TDB. */
/*     FIXREF     I   Body-fixed, body-centered target body frame. */
/*     ABCORR     I   Aberration correction. */
/*     OBSRVR     I   Name of observing body. */
/*     DREF       I   Reference frame of ray's direction vector. */
/*     DVEC       I   Ray's direction vector. */
/*     SPOINT     O   Surface intercept point on the target body. */
/*     TRGEPC     O   Intercept epoch. */
/*     SRFVEC     O   Vector from observer to intercept point. */
/*     FOUND      O   Flag indicating whether intercept was found. */

/* $ Detailed_Input */

/*     METHOD   is a short string providing parameters defining */
/*              the computation method to be used. In the syntax */
/*              descriptions below, items delimited by brackets */
/*              are optional. */

/*              METHOD may be assigned the following values: */

/*                 'ELLIPSOID' */

/*                    The intercept computation uses a triaxial */
/*                    ellipsoid to model the surface of the target */
/*                    body. The ellipsoid's radii must be available */
/*                    in the kernel pool. */


/*                 'DSK/UNPRIORITIZED[/SURFACES = <surface list>]' */

/*                    The intercept computation uses topographic data */
/*                    to model the surface of the target body. These */
/*                    data must be provided by loaded DSK files. */

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

/*                       SURFACES = "Mars MEGDR 128 PIXEL/DEG" */

/*                    If multiple surfaces are specified, their names */
/*                    or IDs must be separated by commas. */

/*                    See the $Particulars section below for details */
/*                    concerning use of DSK data. */


/*              Neither case nor white space are significant in */
/*              METHOD, except within double-quoted strings. For */
/*              example, the string ' eLLipsoid ' is valid. */

/*              Within double-quoted strings, blank characters are */
/*              significant, but multiple consecutive blanks are */
/*              considered equivalent to a single blank. Case is */
/*              not significant. So */

/*                 "Mars MEGDR 128 PIXEL/DEG" */

/*              is equivalent to */

/*                 " mars megdr  128  pixel/deg " */

/*              but not to */

/*                 "MARS MEGDR128PIXEL/DEG" */

/*     TARGET   is the name of the target body. TARGET is */
/*              case-insensitive, and leading and trailing blanks in */
/*              TARGET are not significant. Optionally, you may */
/*              supply a string containing the integer ID code */
/*              for the object. For example both 'MOON' and '301' */
/*              are legitimate strings that indicate the Moon is the */
/*              target body. */

/*              When the target body's surface is represented by a */
/*              tri-axial ellipsoid, this routine assumes that a */
/*              kernel variable representing the ellipsoid's radii is */
/*              present in the kernel pool. Normally the kernel */
/*              variable would be defined by loading a PCK file. */

/*     ET       is the epoch of participation of the observer, */
/*              expressed as ephemeris seconds past J2000 TDB: ET is */
/*              the epoch at which the observer's state is computed. */

/*              When aberration corrections are not used, ET is also */
/*              the epoch at which the state and orientation of the */
/*              target body are computed. */

/*              When aberration corrections are used, the position */
/*              and orientation of the target body are computed at */
/*              ET-LT or ET+LT, where LT is the one-way light time */
/*              between the intercept point and the observer, and the */
/*              sign applied to LT depends on the selected */
/*              correction. See the description of ABCORR below for */
/*              details. */

/*     FIXREF   is the name of a body-fixed reference frame centered */
/*              on the target body. FIXREF may be any such frame */
/*              supported by the SPICE system, including built-in */
/*              frames (documented in the Frames Required Reading) */
/*              and frames defined by a loaded frame kernel (FK). The */
/*              string FIXREF is case-insensitive, and leading and */
/*              trailing blanks in FIXREF are not significant. */

/*              The output intercept point SPOINT and the observer-to- */
/*              intercept vector SRFVEC will be expressed relative to */
/*              this reference frame. */

/*     ABCORR   indicates the aberration corrections to be applied */
/*              when computing the target's position and orientation. */

/*              For remote sensing applications, where the apparent */
/*              surface intercept point seen by the observer is */
/*              desired, normally the correction */

/*                 'CN+S' */

/*              should be used. This and the other supported options */
/*              are described below. ABCORR may be any of the */
/*              following: */

/*                 'NONE'     Apply no correction. Return the */
/*                            geometric surface intercept point on the */
/*                            target body. */

/*              Let LT represent the one-way light time between the */
/*              observer and the surface intercept point (note: NOT */
/*              between the observer and the target body's center). */
/*              The following values of ABCORR apply to the */
/*              "reception" case in which photons depart from the */
/*              intercept point's location at the light-time */
/*              corrected epoch ET-LT and *arrive* at the observer's */
/*              location at ET: */

/*                 'LT'       Correct for one-way light time (also */
/*                            called "planetary aberration") using a */
/*                            Newtonian formulation. This correction */
/*                            yields the location of the surface */
/*                            intercept point at the moment it */
/*                            emitted photons arriving at the */
/*                            observer at ET. */

/*                            The light time correction uses an */
/*                            iterative solution of the light time */
/*                            equation. The solution invoked by the */
/*                            'LT' option uses one iteration. */

/*                            Both the target position as seen by the */
/*                            observer, and rotation of the target */
/*                            body, are corrected for light time. */

/*                 'LT+S'     Correct for one-way light time and */
/*                            stellar aberration using a Newtonian */
/*                            formulation. This option modifies the */
/*                            surface intercept obtained with the */
/*                            'LT' option to account for the */
/*                            observer's velocity relative to the */
/*                            solar system barycenter. These */
/*                            computations yield the apparent surface */
/*                            intercept point. */

/*                 'CN'       Converged Newtonian light time */
/*                            correction. In solving the light time */
/*                            equation, the 'CN' correction iterates */
/*                            until the solution converges. Both the */
/*                            position and rotation of the target */
/*                            body are corrected for light time. */

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

/*                            For reception-case applications */
/*                            involving intercepts near the target */
/*                            body limb, this option should be used. */

/*              The following values of ABCORR apply to the */
/*              "transmission" case in which photons *depart* from */
/*              the observer's location at ET and arrive at the */
/*              intercept point at the light-time corrected epoch */
/*              ET+LT: */

/*                 'XLT'      "Transmission" case: correct for */
/*                            one-way light time using a Newtonian */
/*                            formulation. This correction yields the */
/*                            intercept location at the moment it */
/*                            receives photons emitted from the */
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
/*                            formulation. This option modifies the */
/*                            intercept obtained with the 'XLT' */
/*                            option to account for the observer's */
/*                            velocity relative to the solar system */
/*                            barycenter. */

/*                 'XCN'      Converged Newtonian light time */
/*                            correction. This is the same as 'XLT' */
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

/*                            For transmission-case applications */
/*                            involving intercepts near the target */
/*                            body limb, this option should be used. */

/*              Case and embedded blanks are not significant in */
/*              ABCORR. For example, the string */

/*                'Cn + s' */

/*              is valid. */

/*     OBSRVR   is the name of the observing body. This is typically */
/*              a spacecraft, the earth, or a surface point on the */
/*              earth or on another extended object. */

/*              The observer must be outside the target body. */

/*              OBSRVR is case-insensitive, and leading and */
/*              trailing blanks in OBSRVR are not significant. */
/*              Optionally, you may supply a string containing the */
/*              integer ID code for the object. For example both */
/*              'MOON' and '301' are legitimate strings that indicate */
/*              the Moon is the observer. */

/*     DREF     is the name of the reference frame relative to which */
/*              the ray's direction vector is expressed. This may be */
/*              any frame supported by the SPICE system, including */
/*              built-in frames (documented in the Frames Required */
/*              Reading) and frames defined by a loaded frame kernel */
/*              (FK). The string DREF is case-insensitive, and */
/*              leading and trailing blanks in DREF are not */
/*              significant. */

/*              When DREF designates a non-inertial frame, the */
/*              orientation of the frame is evaluated at an epoch */
/*              dependent on the frame's center and, if the center is */
/*              not the observer, on the selected aberration */
/*              correction. See the description of the direction */
/*              vector DVEC for details. */

/*     DVEC     is a ray direction vector emanating from the observer. */
/*              The intercept with the target body's surface of the ray */
/*              defined by the observer and DVEC is sought. */

/*              DVEC is specified relative to the reference frame */
/*              designated by DREF. */

/*              Non-inertial reference frames are treated as follows: */
/*              if the center of the frame is at the observer's */
/*              location, the frame is evaluated at ET. If the */
/*              frame's center is located elsewhere, then letting */
/*              LTCENT be the one-way light time between the observer */
/*              and the central body associated with the frame, the */
/*              orientation of the frame is evaluated at ET-LTCENT, */
/*              ET+LTCENT, or ET depending on whether the requested */
/*              aberration correction is, respectively, for received */
/*              radiation, transmitted radiation, or is omitted. */
/*              LTCENT is computed using the method indicated by */
/*              ABCORR. */

/* $ Detailed_Output */

/*     SPOINT   is the surface intercept point on the target body of */
/*              the ray defined by the observer and the direction */
/*              vector. If the ray intersects the target body in */
/*              multiple points, the selected intersection point is */
/*              the one closest to the observer. The output argument */
/*              FOUND (see below) indicates whether an intercept was */
/*              found. */

/*              SPOINT is expressed in Cartesian coordinates, */
/*              relative to the target body-fixed frame designated by */
/*              FIXREF. The body-fixed target frame is evaluated at */
/*              the intercept epoch TRGEPC (see description below). */

/*              When light time correction is used, the duration of */
/*              light travel between SPOINT to the observer is */
/*              considered to be the one way light time. When both */
/*              light time and stellar aberration corrections are */
/*              used, SPOINT is compute such that, when the vector */
/*              from the observer to SPOINT is corrected for light */
/*              time and stellar aberration, the resulting vector */
/*              lies on the ray defined by the observer's location */
/*              and DVEC. */

/*              The components of SPOINT are given in units of km. */

/*     TRGEPC   is the "intercept epoch." TRGEPC is defined as */
/*              follows: letting LT be the one-way light time between */
/*              the observer and the intercept point, TRGEPC is the */
/*              epoch ET-LT, ET+LT, or ET depending on whether the */
/*              requested aberration correction is, respectively, for */
/*              received radiation, transmitted radiation, or */
/*              omitted. LT is computed using the method indicated by */
/*              ABCORR. */

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

/*              The second example in the $Examples header section */
/*              below presents a complete program that demonstrates */
/*              this procedure. */

/*     FOUND    is a logical flag indicating whether or not the ray */
/*              intersects the target. If an intersection exists */
/*              FOUND will be returned as .TRUE. If the ray misses */
/*              the target, FOUND will be returned as .FALSE. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the specified aberration correction is unrecognized, an */
/*         error is signaled by a routine in the call tree of this */
/*         routine. */

/*     2)  If either the target or observer input strings cannot be */
/*         converted to an integer ID code, the error */
/*         SPICE(IDCODENOTFOUND) is signaled. */

/*     3)  If OBSRVR and TARGET map to the same NAIF integer ID code, */
/*         the error SPICE(BODIESNOTDISTINCT) is signaled. */

/*     4)  If the input target body-fixed frame FIXREF is not */
/*         recognized, the error SPICE(NOFRAME) is signaled. A frame */
/*         name may fail to be recognized because a required frame */
/*         specification kernel has not been loaded; another cause is a */
/*         misspelling of the frame name. */

/*     5)  If the input frame FIXREF is not centered at the target body, */
/*         the error SPICE(INVALIDFRAME) is signaled. */

/*     6)  If the input argument METHOD cannot be parsed, an error */
/*         is signaled by either this routine or a routine in the */
/*         call tree of this routine. */

/*     7)  If the target and observer have distinct identities but are */
/*         at the same location (for example, the target is Mars and the */
/*         observer is the Mars barycenter), the error */
/*         SPICE(NOSEPARATION) is signaled. */

/*     8)  If insufficient ephemeris data have been loaded prior to */
/*         calling SINCPT, an error is signaled by a */
/*         routine in the call tree of this routine. Note that when */
/*         light time correction is used, sufficient ephemeris data must */
/*         be available to propagate the states of both observer and */
/*         target to the solar system barycenter. */

/*     9)  If the computation method specifies an ellipsoidal target */
/*         shape and triaxial radii of the target body have not been */
/*         loaded into the kernel pool prior to calling SINCPT, an error */
/*         is signaled by a routine in the call tree of this routine. */

/*     10) The target must be an extended body: if any of the radii of */
/*         the target body are non-positive, an error is signaled by a */
/*         routine in the call tree of this routine. */

/*     11) If PCK data specifying the target body-fixed frame orientation */
/*         have not been loaded prior to calling SINCPT, an error is */
/*         signaled by a routine in the call tree of this routine. */

/*     12) If the reference frame designated by DREF is not recognized */
/*         by the SPICE frame subsystem, the error SPICE(NOFRAME) */
/*         is signaled. */

/*     13) If the direction vector DVEC is the zero vector, the error */
/*         SPICE(ZEROVECTOR) is signaled. */

/*     14) If METHOD specifies that the target surface is represented by */
/*         DSK data, and no DSK files are loaded for the specified */
/*         target, an error is signaled by a routine in the call tree */
/*         of this routine. */

/*     15) If METHOD specifies that the target surface is represented */
/*         by DSK data, and DSK data are not available for a portion of */
/*         the target body's surface, an intercept might not be found. */
/*         This routine does not revert to using an ellipsoidal surface */
/*         in this case. */

/* $ Files */

/*     Appropriate kernels must be loaded by the calling program before */
/*     this routine is called. */

/*     The following data are required: */

/*     -  SPK data: ephemeris data for target and observer must be */
/*        loaded. If aberration corrections are used, the states of */
/*        target and observer relative to the solar system barycenter */
/*        must be calculable from the available ephemeris data. */
/*        Typically ephemeris data are made available by loading one */
/*        or more SPK files via FURNSH. */

/*     -  PCK data: if the computation method is specified as */
/*        "Ellipsoid," triaxial radii for the target body must be */
/*        loaded into the kernel pool. Typically this is done by */
/*        loading a text PCK file via FURNSH. */

/*     -  Further PCK data: rotation data for the target body must */
/*        be loaded. These may be provided in a text or binary PCK */
/*        file. */

/*     The following data may be required: */

/*     -  DSK data: if METHOD indicates that DSK data are to be used, */
/*        DSK files containing topographic data for the target body */
/*        must be loaded. If a surface list is specified, data for */
/*        at least one of the listed surfaces must be loaded. */

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

/*     -  Frame data: if a frame definition is required to convert */
/*        the observer and target states to the body-fixed frame of */
/*        the target, that definition must be available in the kernel */
/*        pool. Similarly, the frame definition required to map */
/*        between the frame designated by DREF and the target */
/*        body-fixed frame must be available. Typically the */
/*        definitions of frames not already built-in to SPICE are */
/*        supplied by loading a frame kernel. */

/*     -  CK data: if the frame to which DREF refers is fixed to a */
/*        spacecraft instrument or structure, at least one CK file */
/*        will be needed to permit transformation of vectors between */
/*        that frame and both the J2000 and the target body-fixed */
/*        frames. */

/*     -  SCLK data: if a CK file is needed, an associated SCLK */
/*        kernel is required to enable conversion between encoded SCLK */
/*        (used to time-tag CK data) and barycentric dynamical time */
/*        (TDB). */

/*     In all cases, kernel data are normally loaded once per program */
/*     run, NOT every time this routine is called. */

/* $ Particulars */

/*     Given a ray defined by a direction vector and the location of an */
/*     observer, SINCPT computes the surface intercept point of the ray */
/*     on a specified target body. SINCPT also determines the vector */
/*     from the observer to the surface intercept point. If the ray */
/*     intersects the target in multiple locations, the intercept */
/*     closest to the observer is selected. */

/*     When aberration corrections are used, this routine finds the */
/*     value of SPOINT such that, if SPOINT is regarded as an ephemeris */
/*     object, after the selected aberration corrections are applied to */
/*     the vector from the observer to SPOINT, the resulting vector is */
/*     parallel to the direction vector DVEC. */

/*     This routine computes light time corrections using light time */
/*     between the observer and the surface intercept point, as opposed */
/*     to the center of the target. Similarly, stellar aberration */
/*     corrections done by this routine are based on the direction of */
/*     the vector from the observer to the light-time corrected */
/*     intercept point, not to the target center. This technique avoids */
/*     errors due to the differential between aberration corrections */
/*     across the target body. Therefore it's valid to use aberration */
/*     corrections with this routine even when the observer is very */
/*     close to the intercept point, in particular when the */
/*     observer-intercept point distance is much less than the */
/*     observer-target center distance. It's also valid to use stellar */
/*     aberration corrections even when the intercept point is near or */
/*     on the limb (as may occur in occultation computations using a */
/*     point target). */

/*     When comparing surface intercept point computations with results */
/*     from sources other than SPICE, it's essential to make sure the */
/*     same geometric definitions are used. */


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

/*                 1 + 1.E-10 */

/*              In other words, the sides of the plate are lengthened by */
/*              1/10 of a micron per km. The expansion keeps the centroid */
/*              of the plate fixed. */

/*              Plate expansion prevents all plates from being missed */
/*              in cases where clearly at least one should be hit. */

/*              As with the greedy segment selection algorithm, plate */
/*              expansion can occasionally cause an intercept to be */
/*              found on a different plate than would be found if higher */
/*              precision arithmetic were used. It also can occasionally */
/*              cause an intersection to be found when the ray misses */
/*              the target by a very small distance. */


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

/*     The numerical results shown for these examples may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */


/*     1) The following program computes surface intercept points on Mars */
/*        for the boresight and FOV boundary vectors of the MGS MOC */
/*        narrow angle camera. The intercepts are computed for a single */
/*        observation epoch. Light time and stellar aberration */
/*        corrections are used. For simplicity, camera distortion is */
/*        ignored. */

/*        Intercepts are computed using both triaxial ellipsoid and */
/*        topographic surface models. */

/*        The topographic model is based on data from the MGS MOLA DEM */
/*        megr90n000cb, which has a resolution of 4 pixels/degree. A */
/*        triangular plate model was produced by computing a 720 x 1440 */
/*        grid of interpolated heights from this DEM, then tessellating */
/*        the height grid. The plate model is stored in a type 2 segment */
/*        in the referenced DSK file. */

/*        Use the meta-kernel shown below to load the required SPICE */
/*        kernels. */


/*           KPL/MK */

/*           File: sincpt_ex1.tm */

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
/*              mgs_moc_v20.ti                   MGS MOC instrument */
/*                                               parameters */
/*              mgs_sclkscet_00061.tsc           MGS SCLK coefficients */
/*              mgs_sc_ext12.bc                  MGS s/c bus attitude */
/*              mgs_ext12_ipng_mgs95j.bsp        MGS ephemeris */
/*              megr90n000cb_plate.bds           Plate model based on */
/*                                               MEGDR DEM, resolution */
/*                                               4 pixels/degree. */

/*           \begindata */

/*              KERNELS_TO_LOAD = ( 'de430.bsp', */
/*                                  'mar097.bsp', */
/*                                  'pck00010.tpc', */
/*                                  'naif0011.tls', */
/*                                  'mgs_moc_v20.ti', */
/*                                  'mgs_sclkscet_00061.tsc', */
/*                                  'mgs_sc_ext12.bc', */
/*                                  'mgs_ext12_ipng_mgs95j.bsp', */
/*                                  'megr90n000cb_plate.bds'      ) */
/*           \begintext */

/*           End of meta-kernel */


/*        Example code begins here. */


/*              PROGRAM SINCPT_EX1 */
/*              IMPLICIT NONE */
/*        C */
/*        C     SPICELIB functions */
/*        C */
/*              DOUBLE PRECISION      VNORM */

/*        C */
/*        C     Local parameters */
/*        C */
/*              CHARACTER*(*)         META */
/*              PARAMETER           ( META   = 'sincpt_ex1.tm' ) */

/*              INTEGER               ABCLEN */
/*              PARAMETER           ( ABCLEN = 20 ) */

/*              INTEGER               LNSIZE */
/*              PARAMETER           ( LNSIZE = 78 ) */

/*              INTEGER               METLEN */
/*              PARAMETER           ( METLEN = 40 ) */

/*              INTEGER               NAMLEN */
/*              PARAMETER           ( NAMLEN = 32 ) */

/*              INTEGER               TIMLEN */
/*              PARAMETER           ( TIMLEN = 50 ) */

/*              INTEGER               SHPLEN */
/*              PARAMETER           ( SHPLEN = 80 ) */

/*              INTEGER               NCORNR */
/*              PARAMETER           ( NCORNR = 4 ) */

/*              INTEGER               NMETH */
/*              PARAMETER           ( NMETH  = 2 ) */

/*        C */
/*        C     Local variables */
/*        C */
/*              CHARACTER*(ABCLEN)    ABCORR */
/*              CHARACTER*(NAMLEN)    CAMERA */
/*              CHARACTER*(NAMLEN)    DREF */
/*              CHARACTER*(NAMLEN)    FIXREF */
/*              CHARACTER*(METLEN)    METHDS ( NMETH ) */
/*              CHARACTER*(METLEN)    METHOD */
/*              CHARACTER*(NAMLEN)    OBSRVR */
/*              CHARACTER*(SHPLEN)    SHAPE */
/*              CHARACTER*(NAMLEN)    SRFTYP ( NMETH ) */
/*              CHARACTER*(NAMLEN)    TARGET */
/*              CHARACTER*(LNSIZE)    TITLE */
/*              CHARACTER*(TIMLEN)    UTC */

/*              DOUBLE PRECISION      BOUNDS ( 3, NCORNR ) */
/*              DOUBLE PRECISION      BSIGHT ( 3 ) */
/*              DOUBLE PRECISION      DIST */
/*              DOUBLE PRECISION      DPR */
/*              DOUBLE PRECISION      DVEC   ( 3 ) */
/*              DOUBLE PRECISION      ET */
/*              DOUBLE PRECISION      LAT */
/*              DOUBLE PRECISION      LON */
/*              DOUBLE PRECISION      RADIUS */
/*              DOUBLE PRECISION      SPOINT ( 3 ) */
/*              DOUBLE PRECISION      SRFVEC ( 3 ) */
/*              DOUBLE PRECISION      TRGEPC */

/*              INTEGER               CAMID */
/*              INTEGER               I */
/*              INTEGER               J */
/*              INTEGER               K */
/*              INTEGER               N */

/*              LOGICAL               FOUND */

/*              DATA                  ABCORR / 'CN+S'              / */
/*              DATA                  CAMERA / 'MGS_MOC_NA'        / */
/*              DATA                  FIXREF / 'IAU_MARS'          / */
/*              DATA                  METHDS / 'ELLIPSOID', */
/*             .                               'DSK/UNPRIORITIZED' / */
/*              DATA                  OBSRVR / 'MGS'               / */
/*              DATA                  SRFTYP / 'Ellipsoid', */
/*             .               'MGS/MOLA topography, 4 pixel/deg'  / */
/*              DATA                  TARGET / 'Mars'              / */
/*              DATA                  UTC    / */
/*             .                        '2003 OCT 13 06:00:00 UTC' / */

/*        C */
/*        C     Load kernel files: */
/*        C */
/*              CALL FURNSH ( META ) */

/*        C */
/*        C     Convert the UTC request time to ET (seconds past */
/*        C     J2000, TDB). */
/*        C */
/*              CALL STR2ET ( UTC, ET ) */

/*        C */
/*        C     Get the MGS MOC Narrow angle camera (MGS_MOC_NA) */
/*        C     ID code. Then look up the field of view (FOV) */
/*        C     parameters by calling GETFOV. */
/*        C */
/*              CALL BODN2C ( CAMERA, CAMID, FOUND ) */

/*              IF ( .NOT. FOUND ) THEN */
/*                 CALL SETMSG ( 'Could not find ID code for ' // */
/*             .                 'instrument #.'               ) */
/*                 CALL ERRCH  ( '#', CAMERA                   ) */
/*                 CALL SIGERR ( 'SPICE(NOTRANSLATION)'        ) */
/*              END IF */

/*        C */
/*        C     GETFOV will return the name of the camera-fixed frame */
/*        C     in the string DREF, the camera boresight vector in */
/*        C     the array BSIGHT, and the FOV corner vectors in the */
/*        C     array BOUNDS. */
/*        C */
/*              CALL GETFOV ( CAMID,  NCORNR, SHAPE,  DREF, */
/*             .              BSIGHT, N,      BOUNDS       ) */


/*              WRITE (*,*) ' ' */
/*              WRITE (*,*) 'Surface Intercept Locations for Camera' */
/*              WRITE (*,*) 'FOV Boundary and Boresight Vectors' */
/*              WRITE (*,*) ' ' */
/*              WRITE (*,*) '   Instrument:            ', CAMERA */
/*              WRITE (*,*) '   Epoch:                 ', UTC */
/*              WRITE (*,*) '   Aberration correction: ', ABCORR */

/*        C */
/*        C     Now compute and display the surface intercepts for the */
/*        C     boresight and all of the FOV boundary vectors. */
/*        C */
/*              DO I = 1, NCORNR+1 */

/*                 IF ( I .LE. NCORNR ) THEN */

/*                    TITLE = 'Corner vector #' */
/*                    CALL REPMI ( TITLE, '#', I, TITLE ) */

/*                    CALL VEQU ( BOUNDS(1,I), DVEC ) */

/*                 ELSE */

/*                    TITLE = 'Boresight vector' */
/*                    CALL VEQU ( BSIGHT, DVEC ) */

/*                 END IF */

/*                 WRITE (*,*) ' ' */
/*                 WRITE (*,*) TITLE */

/*                 TITLE = '  Vector in # frame = ' */
/*                 CALL REPMC ( TITLE, '#', DREF, TITLE ) */

/*                 WRITE (*,*) ' ' */
/*                 WRITE (*,*) TITLE */

/*                 IF ( I .LE. NCORNR ) THEN */
/*                    WRITE (*, '(1X,3F20.14)') ( BOUNDS(J,I), J=1,3 ) */
/*                 ELSE */
/*                    WRITE (*, '(1X,3F20.14)') BSIGHT */
/*                 END IF */

/*                 WRITE (*,*) ' ' */
/*                 WRITE (*,*) '  Intercept:' */

/*        C */
/*        C        Compute the surface intercept point using */
/*        C        the specified aberration corrections. Loop */
/*        C        over the set of computation methods. */
/*        C */
/*                 DO K = 1, NMETH */

/*                    METHOD = METHDS(K) */

/*                    CALL SINCPT ( METHOD, TARGET, ET, */
/*             .                    FIXREF, ABCORR, OBSRVR, */
/*             .                    DREF,   DVEC,   SPOINT, */
/*             .                    TRGEPC, SRFVEC, FOUND   ) */

/*                    IF ( FOUND ) THEN */
/*        C */
/*        C              Compute range from observer to apparent */
/*        C              intercept. */
/*        C */
/*                       DIST = VNORM ( SRFVEC ) */
/*        C */
/*        C              Convert rectangular coordinates to */
/*        C              planetocentric latitude and longitude. */
/*        C              Convert radians to degrees. */
/*        C */
/*                       CALL RECLAT ( SPOINT, RADIUS, LON, LAT ) */

/*                       LON = LON * DPR () */
/*                       LAT = LAT * DPR () */
/*        C */
/*        C              Display the results. */
/*        C */

/*                       WRITE (*,*) ' ' */
/*                       CALL TOSTDO ( '     Surface representation: ' */
/*             .         //            SRFTYP(K)                      ) */
/*                       WRITE (*,*) ' ' */
/*                       WRITE (*,*) */
/*             .         '     Radius                   (km)  = ', RADIUS */
/*                       WRITE (*,*) */
/*             .         '     Planetocentric Latitude  (deg) = ', LAT */
/*                       WRITE (*,*) */
/*             .         '     Planetocentric Longitude (deg) = ', LON */
/*                       WRITE (*,*) */
/*             .         '     Range                    (km)  = ', DIST */

/*                    ELSE */

/*                       CALL TOSTDO ( '   Surface representation: ' */
/*             .         //            SRFTYP(K)                     ) */
/*                       WRITE (*,*) '     Intercept not found.' */
/*                       WRITE (*,*) ' ' */

/*                    END IF */

/*                 END DO */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Surface Intercept Locations for Camera */
/*         FOV Boundary and Boresight Vectors */

/*            Instrument:            MGS_MOC_NA */
/*            Epoch:                 2003 OCT 13 06:00:00 UTC */
/*            Aberration correction: CN+S */

/*         Corner vector 1 */

/*           Vector in MGS_MOC_NA frame = */
/*             0.00000185713838   -0.00380156226592    0.99999277403434 */

/*           Intercept: */

/*             Surface representation: Ellipsoid */

/*              Radius                   (km)  =    3384.9411357607282 */
/*              Planetocentric Latitude  (deg) =   -48.477482367206768 */
/*              Planetocentric Longitude (deg) =   -123.47407481971256 */
/*              Range                    (km)  =    388.98308225698986 */

/*             Surface representation: MGS/MOLA topography, 4 pixel/deg */

/*              Radius                   (km)  =    3387.6408267726060 */
/*              Planetocentric Latitude  (deg) =   -48.492259559975267 */
/*              Planetocentric Longitude (deg) =   -123.47541193495911 */
/*              Range                    (km)  =    386.14510040407879 */

/*         Corner vector 2 */

/*           Vector in MGS_MOC_NA frame = */
/*             0.00000185713838    0.00380156226592    0.99999277403434 */

/*           Intercept: */

/*             Surface representation: Ellipsoid */

/*              Radius                   (km)  =    3384.9396985743224 */
/*              Planetocentric Latitude  (deg) =   -48.481636778911913 */
/*              Planetocentric Longitude (deg) =   -123.39881874871132 */
/*              Range                    (km)  =    388.97510005267708 */

/*             Surface representation: MGS/MOLA topography, 4 pixel/deg */

/*              Radius                   (km)  =    3387.6403704507966 */
/*              Planetocentric Latitude  (deg) =   -48.496386688872484 */
/*              Planetocentric Longitude (deg) =   -123.40074354811055 */
/*              Range                    (km)  =    386.13616443321536 */

/*         Corner vector 3 */

/*           Vector in MGS_MOC_NA frame = */
/*            -0.00000185713838    0.00380156226592    0.99999277403434 */

/*           Intercept: */

/*             Surface representation: Ellipsoid */

/*              Radius                   (km)  =    3384.9396897286833 */
/*              Planetocentric Latitude  (deg) =   -48.481662348858336 */
/*              Planetocentric Longitude (deg) =   -123.39882195503854 */
/*              Range                    (km)  =    388.97464113550637 */

/*             Surface representation: MGS/MOLA topography, 4 pixel/deg */

/*              Radius                   (km)  =    3387.6403603146168 */
/*              Planetocentric Latitude  (deg) =   -48.496412042429789 */
/*              Planetocentric Longitude (deg) =   -123.40074672915324 */
/*              Range                    (km)  =    386.13571069851986 */

/*         Corner vector 4 */

/*           Vector in MGS_MOC_NA frame = */
/*            -0.00000185713838   -0.00380156226592    0.99999277403434 */

/*           Intercept: */

/*             Surface representation: Ellipsoid */

/*              Radius                   (km)  =    3384.9411269137695 */
/*              Planetocentric Latitude  (deg) =   -48.477507940479093 */
/*              Planetocentric Longitude (deg) =   -123.47407797517749 */
/*              Range                    (km)  =    388.98262331952731 */

/*             Surface representation: MGS/MOLA topography, 4 pixel/deg */

/*              Radius                   (km)  =    3387.6408166344654 */
/*              Planetocentric Latitude  (deg) =   -48.492284916898356 */
/*              Planetocentric Longitude (deg) =   -123.47541506563023 */
/*              Range                    (km)  =    386.14464664863726 */

/*         Boresight vector */

/*           Vector in MGS_MOC_NA frame = */
/*             0.00000000000000    0.00000000000000    1.00000000000000 */

/*           Intercept: */

/*             Surface representation: Ellipsoid */

/*        [...] */


/*        Warning: incomplete output. Only 100 out of 112 lines have been */
/*        provided. */


/*     2) Use SUBPNT to find the sub-spacecraft point on Mars for the */
/*        Mars Reconnaissance Orbiter spacecraft (MRO) at a specified */
/*        time, using the "near point: ellipsoid" computation method. */
/*        Use both LT+S and CN+S aberration corrections to illustrate */
/*        the differences. */

/*        Convert the spacecraft to sub-observer point vector obtained */
/*        from SUBPNT into the MRO_HIRISE_LOOK_DIRECTION reference frame */
/*        at the observation time. Perform a consistency check with this */
/*        vector: compare the Mars surface intercept of the ray */
/*        emanating from the spacecraft and pointed along this vector */
/*        with the sub-observer point. */

/*        Perform the sub-observer point and surface intercept */
/*        computations using both triaxial ellipsoid and topographic */
/*        surface models. */

/*        For this example, the topographic model is based on the MGS */
/*        MOLA DEM megr90n000eb, which has a resolution of 16 */
/*        pixels/degree. Eight DSKs, each covering longitude and */
/*        latitude ranges of 90 degrees, were made from this data set. */
/*        For the region covered by a given DSK, a grid of approximately */
/*        1500 x 1500 interpolated heights was produced, and this grid */
/*        was tessellated using approximately 4.5 million triangular */
/*        plates, giving a total plate count of about 36 million for the */
/*        entire DSK set. */

/*        All DSKs in the set use the surface ID code 499001, so there */
/*        is no need to specify the surface ID in the METHOD strings */
/*        passed to SINCPT and SUBPNT. */

/*        Use the meta-kernel shown below to load the required SPICE */
/*        kernels. */


/*           KPL/MK */

/*           File: sincpt_ex2.tm */

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
/*              mro_psp4_ssd_mro95a.bsp          MRO ephemeris */
/*              mro_v11.tf                       MRO frame specifications */
/*              mro_sclkscet_00022_65536.tsc     MRO SCLK coefficients */
/*                                               parameters */
/*              mro_sc_psp_070925_071001.bc      MRO attitude */
/*              megr90n000eb_*_plate.bds         Plate model DSKs based */
/*                                               on MEGDR DEM, resolution */
/*                                               16 pixels/degree. */

/*           \begindata */

/*              KERNELS_TO_LOAD = ( */

/*                 'de430.bsp', */
/*                 'mar097.bsp', */
/*                 'pck00010.tpc', */
/*                 'naif0011.tls', */
/*                 'mro_psp4_ssd_mro95a.bsp', */
/*                 'mro_v11.tf', */
/*                 'mro_sclkscet_00022_65536.tsc', */
/*                 'mro_sc_psp_070925_071001.bc', */
/*                 'megr90n000eb_LL000E00N_UR090E90N_plate.bds' */
/*                 'megr90n000eb_LL000E90S_UR090E00S_plate.bds' */
/*                 'megr90n000eb_LL090E00N_UR180E90N_plate.bds' */
/*                 'megr90n000eb_LL090E90S_UR180E00S_plate.bds' */
/*                 'megr90n000eb_LL180E00N_UR270E90N_plate.bds' */
/*                 'megr90n000eb_LL180E90S_UR270E00S_plate.bds' */
/*                 'megr90n000eb_LL270E00N_UR360E90N_plate.bds' */
/*                 'megr90n000eb_LL270E90S_UR360E00S_plate.bds'  ) */

/*           \begintext */

/*           End of meta-kernel */


/*        Example code begins here. */


/*              PROGRAM SINCPT_EX2 */
/*              IMPLICIT NONE */
/*        C */
/*        C     SPICELIB functions */
/*        C */
/*              DOUBLE PRECISION      DPR */
/*              DOUBLE PRECISION      VDIST */
/*              DOUBLE PRECISION      VNORM */

/*        C */
/*        C     Local parameters */
/*        C */
/*              CHARACTER*(*)         META */
/*              PARAMETER           ( META   = 'sincpt_ex2.tm' ) */

/*              CHARACTER*(*)         F1 */
/*              PARAMETER           ( F1     = '(A,F21.9)' ) */

/*              CHARACTER*(*)         F2 */
/*              PARAMETER           ( F2     = '(A)' ) */

/*              INTEGER               FRNMLN */
/*              PARAMETER           ( FRNMLN = 32 ) */

/*              INTEGER               MTHLEN */
/*              PARAMETER           ( MTHLEN = 50 ) */

/*              INTEGER               CORLEN */
/*              PARAMETER           ( CORLEN = 5 ) */

/*              INTEGER               NCORR */
/*              PARAMETER           ( NCORR  = 2 ) */

/*              INTEGER               NMETH */
/*              PARAMETER           ( NMETH  = 2 ) */

/*        C */
/*        C     Local variables */
/*        C */
/*              CHARACTER*(CORLEN)    ABCORR ( NCORR ) */
/*              CHARACTER*(FRNMLN)    FIXREF */
/*              CHARACTER*(FRNMLN)    HIREF */
/*              CHARACTER*(MTHLEN)    SINMTH ( NMETH ) */
/*              CHARACTER*(MTHLEN)    SUBMTH ( NMETH ) */

/*              DOUBLE PRECISION      ALT */
/*              DOUBLE PRECISION      ET */
/*              DOUBLE PRECISION      LAT */
/*              DOUBLE PRECISION      LON */
/*              DOUBLE PRECISION      MROVEC ( 3 ) */
/*              DOUBLE PRECISION      RADIUS */
/*              DOUBLE PRECISION      SPOINT ( 3 ) */
/*              DOUBLE PRECISION      SRFVEC ( 3 ) */
/*              DOUBLE PRECISION      TRGEPC */
/*              DOUBLE PRECISION      XFORM  ( 3, 3 ) */
/*              DOUBLE PRECISION      XEPOCH */
/*              DOUBLE PRECISION      XPOINT ( 3 ) */
/*              DOUBLE PRECISION      XVEC   ( 3 ) */

/*              INTEGER               I */
/*              INTEGER               J */

/*              LOGICAL               FOUND */

/*        C */
/*        C     Initial values */
/*        C */
/*              DATA                  ABCORR / 'LT+S', 'CN+S'         / */
/*              DATA                  FIXREF / 'IAU_MARS'             / */
/*              DATA                  SINMTH / 'Ellipsoid', */
/*             .                               'DSK/Unprioritized'    / */
/*              DATA                  SUBMTH / 'Ellipsoid/Near point', */
/*             .                            'DSK/Unprioritized/Nadir' / */

/*        C */
/*        C     Load kernel files via the meta-kernel. */
/*        C */
/*              CALL FURNSH ( META ) */

/*        C */
/*        C     Convert the TDB request time string to seconds past */
/*        C     J2000, TDB. */
/*        C */
/*              CALL STR2ET ( '2007 SEP 30 00:00:00 TDB', ET ) */

/*        C */
/*        C     Compute the sub-spacecraft point using the */
/*        C     "NEAR POINT: ELLIPSOID" definition. */
/*        C     Compute the results using both LT+S and CN+S */
/*        C     aberration corrections. */
/*        C */
/*        C     Repeat the computation for each method. */
/*        C */
/*        C */
/*              DO I = 1, NMETH */

/*                 WRITE(*,F2) ' ' */
/*                 WRITE(*,F2) 'Sub-observer point computation method = ' */
/*             .               // SUBMTH(I) */

/*                 DO J = 1, NCORR */

/*                    CALL SUBPNT ( SUBMTH(I), */
/*             .                    'Mars', ET,     FIXREF, ABCORR(J), */
/*             .                    'MRO',  SPOINT, TRGEPC, SRFVEC    ) */
/*        C */
/*        C           Compute the observer's altitude above SPOINT. */
/*        C */
/*                    ALT = VNORM ( SRFVEC ) */
/*        C */
/*        C           Express SRFVEC in the MRO_HIRISE_LOOK_DIRECTION */
/*        C           reference frame at epoch ET. Since SRFVEC is */
/*        C           expressed relative to the IAU_MARS frame at */
/*        C           TRGEPC, we must call PXFRM2 to compute the position */
/*        C           transformation matrix from IAU_MARS at TRGEPC to */
/*        C           the MRO_HIRISE_LOOK_DIRECTION frame at time ET. */
/*        C */
/*        C           To make code formatting a little easier, we'll */
/*        C           store the long MRO reference frame name in a */
/*        C           variable: */
/*        C */
/*                    HIREF = 'MRO_HIRISE_LOOK_DIRECTION' */

/*                    CALL PXFRM2 ( FIXREF, HIREF,  TRGEPC, ET, XFORM ) */
/*                    CALL MXV    ( XFORM,  SRFVEC, MROVEC ) */

/*        C */
/*        C           Convert rectangular coordinates to planetocentric */
/*        C           latitude and longitude. Convert radians to degrees. */
/*        C */
/*                    CALL RECLAT ( SPOINT, RADIUS, LON, LAT  ) */

/*                    LON = LON * DPR () */
/*                    LAT = LAT * DPR () */
/*        C */
/*        C           Write the results. */
/*        C */
/*                    WRITE(*,F2) ' ' */
/*                    WRITE(*,F2) '   Aberration correction = ' */
/*             .                // ABCORR(J) */
/*                    WRITE(*,F1) ' ' */
/*                    WRITE(*,F2) '      MRO-to-sub-observer vector in' */
/*                    WRITE(*,F2) '      MRO HIRISE look direction frame' */
/*                    WRITE(*,F1) '        X-component             ' */
/*             .               // '(km) = ', MROVEC(1) */
/*                    WRITE(*,F1) '        Y-component             ' */
/*             .               // '(km) = ', MROVEC(2) */
/*                    WRITE(*,F1) '        Z-component             ' */
/*             .               // '(km) = ', MROVEC(3) */
/*                    WRITE(*,F1) '      Sub-observer point radius ' */
/*             .               // '(km) = ', RADIUS */
/*                    WRITE(*,F1) '      Planetocentric latitude  ' */
/*             .               // '(deg) = ', LAT */
/*                    WRITE(*,F1) '      Planetocentric longitude ' */
/*             .               // '(deg) = ', LON */
/*                    WRITE(*,F1) '      Observer altitude         ' */
/*             .               // '(km) = ',  ALT */

/*        C */
/*        C           Consistency check: find the surface intercept on */
/*        C           Mars of the ray emanating from the spacecraft and */
/*        C           having direction vector MROVEC in the MRO HIRISE */
/*        C           reference frame at ET. Call the intercept point */
/*        C           XPOINT. XPOINT should coincide with SPOINT, up to */
/*        C           a small round-off error. */
/*        C */
/*                    CALL SINCPT ( SINMTH(I), 'Mars', ET,    FIXREF, */
/*             .                    ABCORR(J), 'MRO',  HIREF, MROVEC, */
/*             .                    XPOINT,    XEPOCH, XVEC,  FOUND  ) */

/*                    IF ( .NOT. FOUND ) THEN */
/*                       WRITE (*,F1) 'Bug: no intercept' */
/*                    ELSE */
/*        C */
/*        C              Report the distance between XPOINT and SPOINT. */
/*        C */
/*                       WRITE (*,* ) ' ' */
/*                       WRITE (*,F1) '   Intercept comparison ' */
/*             .         //           'error (km) = ', */
/*             .                      VDIST( XPOINT, SPOINT ) */
/*                    END IF */

/*                 END DO */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        Sub-observer point computation method = Ellipsoid/Near point */

/*           Aberration correction = LT+S */

/*              MRO-to-sub-observer vector in */
/*              MRO HIRISE look direction frame */
/*                X-component             (km) =           0.286933229 */
/*                Y-component             (km) =          -0.260425939 */
/*                Z-component             (km) =         253.816326385 */
/*              Sub-observer point radius (km) =        3388.299078378 */
/*              Planetocentric latitude  (deg) =         -38.799836378 */
/*              Planetocentric longitude (deg) =        -114.995297227 */
/*              Observer altitude         (km) =         253.816622175 */

/*           Intercept comparison error (km) =           0.000002144 */

/*           Aberration correction = CN+S */

/*              MRO-to-sub-observer vector in */
/*              MRO HIRISE look direction frame */
/*                X-component             (km) =           0.286933107 */
/*                Y-component             (km) =          -0.260426683 */
/*                Z-component             (km) =         253.816315915 */
/*              Sub-observer point radius (km) =        3388.299078376 */
/*              Planetocentric latitude  (deg) =         -38.799836382 */
/*              Planetocentric longitude (deg) =        -114.995297449 */
/*              Observer altitude         (km) =         253.816611705 */

/*           Intercept comparison error (km) =           0.000000001 */

/*        Sub-observer point computation method = DSK/Unprioritized/Nadir */

/*           Aberration correction = LT+S */

/*              MRO-to-sub-observer vector in */
/*              MRO HIRISE look direction frame */
/*                X-component             (km) =           0.282372596 */
/*                Y-component             (km) =          -0.256289313 */
/*                Z-component             (km) =         249.784871247 */
/*              Sub-observer point radius (km) =        3392.330239436 */
/*              Planetocentric latitude  (deg) =         -38.800230156 */
/*              Planetocentric longitude (deg) =        -114.995297338 */
/*              Observer altitude         (km) =         249.785162334 */

/*           Intercept comparison error (km) =           0.000002412 */

/*           Aberration correction = CN+S */

/*              MRO-to-sub-observer vector in */
/*              MRO HIRISE look direction frame */
/*                X-component             (km) =           0.282372464 */
/*                Y-component             (km) =          -0.256290075 */
/*                Z-component             (km) =         249.784860121 */
/*              Sub-observer point radius (km) =        3392.330239564 */
/*              Planetocentric latitude  (deg) =         -38.800230162 */
/*              Planetocentric longitude (deg) =        -114.995297569 */
/*              Observer altitude         (km) =         249.785151209 */

/*           Intercept comparison error (km) =           0.000000001 */


/* $ Restrictions */

/*     1)  A cautionary note: if aberration corrections are used, and */
/*         if DREF is the target body-fixed frame, the epoch at which */
/*         that frame is evaluated is offset from ET by the light time */
/*         between the observer and the *center* of the target body. */
/*         This light time normally will differ from the light time */
/*         between the observer and intercept point. Consequently the */
/*         orientation of the target body-fixed frame at TRGEPC will */
/*         not match that of the target body-fixed frame at the epoch */
/*         associated with DREF. As a result, various derived quantities */
/*         may not be as expected: for example, SRFVEC would not be */
/*         parallel to DVEC. */

/*         In many applications the errors arising from this frame */
/*         discrepancy may be insignificant; however a safe approach is */
/*         to always use as DREF a frame other than the target */
/*         body-fixed frame. */

/*     2)  This routine must not be used for cases where the observer */
/*         is inside the target body. This routine does not attempt to */
/*         detect this condition. */

/*         If the observer is a point on a target surface described */
/*         by DSK data, care must be taken to ensure the observer is */
/*         sufficiently far outside the target. The routine should */
/*         not be used for surfaces for which "outside" cannot be */
/*         defined. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     S.C. Krening       (JPL) */
/*     B.V. Semenov       (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 3.1.0, 26-OCT-2021 (JDR) (NJB) */

/*        Bug fix: PRVCOR is no longer set to blank before */
/*        ABCORR is parsed. */

/*        ZZVALCOR is now used instead of ZZPRSCOR. This provides */
/*        better error handling. */

/*        Edits to $Examples section to comply with NAIF standard. */

/*        The header's $Detailed_Input and $Restrictions sections */
/*        were updated to state that the observer must be */
/*        outside the target body. */

/* -    SPICELIB Version 3.0.0, 04-APR-2017 (NJB) */

/*        01-FEB-2016 (NJB) */

/*           Upgraded to support surfaces represented by DSKs. */

/*           Updated kernels are used in header example programs. */

/* -    SPICELIB Version 2.0.0, 31-MAR-2014 (NJB) (SCK) (BVS) */

/*        Bug fix: FIRST is now set to .FALSE. at the completion */
/*        of a successful initialization pass. This does not affect */
/*        the routine's outputs but improves efficiency. */

/*        Bug fix: redundant call to SPKSSB was removed. This does not */
/*        affect the routine's outputs but improves efficiency. */

/*        References to the new PXFRM2 routine were added, which changed */
/*        the Detailed Output section and the second example. Some header */
/*        comment corrections were made. */

/*        Upgrade: this routine now uses ZZVALCOR rather than */
/*        ZZPRSCOR, simplifying the implementation. */

/*        Upgrade: this routine now saves the input body names and */
/*        ZZBODTRN state counters and does name-ID conversions only if */
/*        the counters have changed. */

/*        Upgrade: this routine now saves the input frame names and POOL */
/*        state counters and does frame name-ID conversions only if the */
/*        counters have changed. */

/* -    SPICELIB Version 1.2.0, 07-APR-2010 (NJB) */

/*        Code style improvement: re-use of variables in */
/*        FRINFO calls has been eliminated. There is no impact */
/*        of the behavior of the routine. */

/* -    SPICELIB Version 1.1.0, 17-MAR-2009 (NJB) (EDW) */

/*        Bug fix: quick test for non-intersection is */
/*        no longer performed when observer-target distance */
/*        is less than target's maximum radius. */

/*        Typos in the Detailed Input section's description of DREF */
/*        were corrected. */

/*        In the header examples, meta-kernel names were updated to use */
/*        the suffix */

/*           ".tm" */

/*        Incorrect frame name FIXFRM was changed to FIXREF in */
/*        documentation. */

/*        Typo correction in $Required_Reading, changed FRAME */
/*        to FRAMES. */

/* -    SPICELIB Version 1.0.0, 02-MAR-2008 (NJB) */

/* -& */
/* $ Index_Entries */

/*     find surface intercept point */
/*     find intersection of ray and target body surface */
/*     find intercept of ray on target body surface */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 3.0.0, 04-APR-2017 (NJB) */

/*        Upgraded to support surfaces represented by DSKs. */

/*        The routine was re-written so as to use a private */
/*        routine to implement the intersection algorithm. */
/*        That routine has been generalized so that it does */
/*        not depend on the target surface representation: it */
/*        uses callback routines to compute ray-surface intercepts */
/*        for a specified ray and time, the surface tangency point */
/*        for a given ray, and the radius of an outer bounding */
/*        sphere for the target. */

/* -& */

/*     SPICELIB functions */


/*     EXTERNAL routines */


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


/*     Initial values */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("SINCPT", (ftnlen)6);

/*     Nothing has been found yet. */

    *found = FALSE_;

/*     Counter initialization is done separately. */

    if (first) {

/*        Initialize counters. */

	zzctruin_(svctr1);
	zzctruin_(svctr2);
	zzctruin_(svctr3);
	zzctruin_(svctr4);
	zzctruin_(svctr5);
    }
    if (first || s_cmp(abcorr, prvcor, abcorr_len, (ftnlen)5) != 0) {

/*        The aberration correction flag differs from the value it */
/*        had on the previous call, if any. Analyze the new flag. */

	zzvalcor_(abcorr, attblk, abcorr_len);
	if (failed_()) {
	    chkout_("SINCPT", (ftnlen)6);
	    return 0;
	}

/*        Set logical flags indicating the attributes of the requested */
/*        correction: */

/*           XMIT is .TRUE. when the correction is for transmitted */
/*           radiation. */

/*           USELT is .TRUE. when any type of light time correction */
/*           (normal or converged Newtonian) is specified. */

/*           USECN indicates converged Newtonian light time correction. */

/*           USESTL indicates stellar aberration corrections. */


/*        The above definitions are consistent with those used by */
/*        ZZVALCOR. */

	xmit = attblk[4];
	uselt = attblk[1];
	usecn = attblk[3];
	usestl = attblk[2];

/*        The aberration correction flag is valid; save it. */

	s_copy(prvcor, abcorr, (ftnlen)5, abcorr_len);
    }

/*     Obtain integer codes for the target and observer. */

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
	chkout_("SINCPT", (ftnlen)6);
	return 0;
    }
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
	chkout_("SINCPT", (ftnlen)6);
	return 0;
    }

/*     Check the input body codes. If they are equal, signal */
/*     an error. */

    if (obscde == trgcde) {
	setmsg_("In computing the surface intercept point, the observing bod"
		"y and target body are the same. Both are #.", (ftnlen)102);
	errch_("#", obsrvr, (ftnlen)1, obsrvr_len);
	sigerr_("SPICE(BODIESNOTDISTINCT)", (ftnlen)24);
	chkout_("SINCPT", (ftnlen)6);
	return 0;
    }

/*     Determine the attributes of the frame designated by FIXREF. */

    zznamfrm_(svctr3, svfref, &svfxfc, fixref, &fxfcde, (ftnlen)32, 
	    fixref_len);
    frinfo_(&fxfcde, &fxcent, &fxclss, &fxtyid, &fnd);
    if (failed_()) {
	chkout_("SINCPT", (ftnlen)6);
	return 0;
    }
    if (! fnd) {
	setmsg_("Reference frame # is not recognized by the SPICE frame subs"
		"ystem. Possibly a required frame definition kernel has not b"
		"een loaded.", (ftnlen)130);
	errch_("#", fixref, (ftnlen)1, fixref_len);
	sigerr_("SPICE(NOFRAME)", (ftnlen)14);
	chkout_("SINCPT", (ftnlen)6);
	return 0;
    }

/*     Make sure that FIXREF is centered at the target body's center. */

    if (fxcent != trgcde) {
	setmsg_("Reference frame # is not centered at the target body #. The"
		" ID code of the frame center is #.", (ftnlen)93);
	errch_("#", fixref, (ftnlen)1, fixref_len);
	errch_("#", target, (ftnlen)1, target_len);
	errint_("#", &fxcent, (ftnlen)1);
	sigerr_("SPICE(INVALIDFRAME)", (ftnlen)19);
	chkout_("SINCPT", (ftnlen)6);
	return 0;
    }

/*     Check for a zero ray direction vector. */

    if (vzero_(dvec)) {
	setmsg_("Input ray direction was the zero vector; this vector must b"
		"e non-zero.", (ftnlen)70);
	sigerr_("SPICE(ZEROVECTOR)", (ftnlen)17);
	chkout_("SINCPT", (ftnlen)6);
	return 0;
    }

/*     Determine the attributes of the frame designated by DREF. */

    zznamfrm_(svctr4, svdref, &svdfrc, dref, &dfrcde, (ftnlen)32, dref_len);
    frinfo_(&dfrcde, &dcentr, &dclass, &dtypid, &fnd);
    if (failed_()) {
	chkout_("SINCPT", (ftnlen)6);
	return 0;
    }
    if (! fnd) {
	setmsg_("Reference frame # is not recognized by the SPICE frame subs"
		"ystem. Possibly a required frame definition kernel has not b"
		"een loaded.", (ftnlen)130);
	errch_("#", dref, (ftnlen)1, dref_len);
	sigerr_("SPICE(NOFRAME)", (ftnlen)14);
	chkout_("SINCPT", (ftnlen)6);
	return 0;
    }

/*     Check whether the surface name/ID mapping has been updated. */

    zzsrftrk_(svctr5, &surfup);

/*     Initialize the SINCPT utility package for the next computation. */
/*     The choice of initialization routine depends on the target */
/*     surface type. */

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
	    chkout_("SINCPT", (ftnlen)6);
	    return 0;
	}
	if (eqstr_(shpstr, "ELLIPSOID", (ftnlen)9, (ftnlen)9)) {
	    shape = 1;
	} else if (eqstr_(shpstr, "DSK", (ftnlen)9, (ftnlen)3)) {
	    shape = 2;
	} else {

/*           This is a backstop check. */

	    setmsg_("[1] Returned shape value from method string was <#>.", (
		    ftnlen)52);
	    errch_("#", shpstr, (ftnlen)1, (ftnlen)9);
	    sigerr_("SPICE(BUG)", (ftnlen)10);
	    chkout_("SINCPT", (ftnlen)6);
	    return 0;
	}

/*        There should be no subtype specification in the method */
/*        string. */

	if (s_cmp(subtyp, " ", (ftnlen)20, (ftnlen)1) != 0) {
	    setmsg_("Spurious sub-observer point type <#> was present in the"
		    " method string #. The sub-observer type is valid in the "
		    "method strings for SUBPNT and SUBSLR, but is not applica"
		    "ble for SINCPT.", (ftnlen)182);
	    errch_("#", subtyp, (ftnlen)1, (ftnlen)20);
	    errch_("#", method, (ftnlen)1, method_len);
	    sigerr_("SPICE(INVALIDMETHOD)", (ftnlen)20);
	    chkout_("SINCPT", (ftnlen)6);
	    return 0;
	}
	s_copy(prvmth, method, (ftnlen)500, method_len);
    }

/*     At this point, the first pass actions were successful. */

    first = FALSE_;
    if (shape == 1) {

/*        Initialize the intercept algorithm to use the reference */
/*        ellipsoid of the target body. */

	zzsuelin_(&trgcde);
    } else if (shape == 2) {

/*        This is the DSK case. */

/*        If the method string listed a set of surface IDs, NSURF is */
/*        positive and SRFLST contains those IDs. */

/*        Initialize the intercept algorithm to use a DSK */
/*        model for the surface of the target body. */

	zzsudski_(&trgcde, &nsurf, srflst, &fxfcde);
    } else {

/*        This is a backstop check. */

	setmsg_("[2] Returned shape value from method string was <#>.", (
		ftnlen)52);
	errch_("#", shpstr, (ftnlen)1, (ftnlen)9);
	sigerr_("SPICE(BUG)", (ftnlen)10);
	chkout_("SINCPT", (ftnlen)6);
	return 0;
    }

/*     Perform the intercept computation. */

    zzsfxcor_((U_fp)zzraynp_, (U_fp)zzmaxrad_, (U_fp)zzraysfx_, &trgcde, et, 
	    abcorr, &uselt, &usecn, &usestl, &xmit, fixref, &obscde, &dfrcde, 
	    &dclass, &dcentr, dvec, spoint, trgepc, srfvec, found, abcorr_len,
	     fixref_len);
    chkout_("SINCPT", (ftnlen)6);
    return 0;
} /* sincpt_ */

