/* subpnt.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__100 = 100;

/* $Procedure SUBPNT ( Sub-observer point ) */
/* Subroutine */ int subpnt_(char *method, char *target, doublereal *et, char 
	*fixref, char *abcorr, char *obsrvr, doublereal *spoint, doublereal *
	trgepc, doublereal *srfvec, ftnlen method_len, ftnlen target_len, 
	ftnlen fixref_len, ftnlen abcorr_len, ftnlen obsrvr_len)
{
    /* Initialized data */

    static logical first = TRUE_;
    static logical near__ = TRUE_;
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
    static integer shape = -1;

    /* System generated locals */
    doublereal d__1, d__2;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int zzbods2c_(integer *, char *, integer *, 
	    logical *, char *, integer *, logical *, ftnlen, ftnlen);
    doublereal dvec[3];
    integer nitr;
    extern /* Subroutine */ int vsub_(doublereal *, doublereal *, doublereal *
	    ), vequ_(doublereal *, doublereal *);
    static logical xmit;
    doublereal tpos[3];
    extern /* Subroutine */ int mtxv_(doublereal *, doublereal *, doublereal *
	    ), zzgftreb_(integer *, doublereal *), zznamfrm_(integer *, char *
	    , integer *, char *, integer *, ftnlen, ftnlen), zzvalcor_(char *,
	     logical *, ftnlen);
    doublereal j2pos[3];
    extern /* Subroutine */ int zzsudski_(integer *, integer *, integer *, 
	    integer *), zzctruin_(integer *);
    integer i__;
    extern /* Subroutine */ int zzprsmet_(integer *, char *, integer *, char *
	    , char *, logical *, integer *, integer *, char *, char *, ftnlen,
	     ftnlen, ftnlen, ftnlen, ftnlen), zzsrftrk_(integer *, logical *);
    doublereal s, radii[3], range;
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen);
    static logical usecn;
    extern doublereal vdist_(doublereal *, doublereal *);
    doublereal vtemp[3], xform[9]	/* was [3][3] */;
    extern logical eqstr_(char *, char *, ftnlen, ftnlen);
    static integer nsurf;
    extern doublereal vnorm_(doublereal *);
    static logical uselt, svfnd1, svfnd2;
    doublereal corvj2[3], subvj2[3];
    static integer svctr1[2], svctr2[2];
    extern logical failed_(void);
    static integer svctr3[2], svctr4[2];
    doublereal lt, etdiff;
    integer obscde, fixcid;
    doublereal ltdiff;
    extern doublereal clight_(void);
    integer fixfid, trgcde;
    extern /* Subroutine */ int stelab_(doublereal *, doublereal *, 
	    doublereal *);
    extern doublereal touchd_(doublereal *);
    extern logical return_(void);
    char pntdef[20], shpstr[9];
    static char subtyp[20];
    char trmstr[20];
    doublereal corpos[3], obspos[3], prevet, prevlt, ssbost[6], ssbtst[6], 
	    stloff[3], subvec[3];
    integer fixcls, fixctr;
    static integer srflst[100];
    logical attblk[15], fnd, surfup;
    static logical usestl;
    static char svtarg[36];
    static integer svtcde;
    static char svobsr[36];
    static integer svobsc;
    doublereal alt;
    static char svfref[32];
    static integer svrefc;
    extern /* Subroutine */ int chkout_(char *, ftnlen), setmsg_(char *, 
	    ftnlen), sigerr_(char *, ftnlen), frinfo_(integer *, integer *, 
	    integer *, integer *, logical *), errint_(char *, integer *, 
	    ftnlen), spkezp_(integer *, doublereal *, char *, char *, integer 
	    *, doublereal *, doublereal *, ftnlen, ftnlen);
    static logical pri;
    extern /* Subroutine */ int vminus_(doublereal *, doublereal *), nearpt_(
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *), surfpt_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, logical *)
	    , spkssb_(integer *, doublereal *, char *, doublereal *, ftnlen), 
	    pxform_(char *, char *, doublereal *, doublereal *, ftnlen, 
	    ftnlen), stlabx_(doublereal *, doublereal *, doublereal *), mxv_(
	    doublereal *, doublereal *, doublereal *), zzsbfxr_(integer *, 
	    integer *, integer *, doublereal *, integer *, doublereal *, 
	    doublereal *, doublereal *, logical *);

/* $ Abstract */

/*     Compute the rectangular coordinates of the sub-observer point on */
/*     a target body at a specified epoch, optionally corrected for */
/*     light time and stellar aberration. */

/*     The surface of the target body may be represented by a triaxial */
/*     ellipsoid or by topographic data provided by DSK files. */

/*     This routine supersedes SUBPT. */

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
/*     SPOINT     O   Sub-observer point on the target body. */
/*     TRGEPC     O   Sub-observer point epoch. */
/*     SRFVEC     O   Vector from observer to sub-observer point. */

/* $ Detailed_Input */

/*     METHOD   is a short string providing parameters defining */
/*              the computation method to be used. In the syntax */
/*              descriptions below, items delimited by brackets */
/*              are optional. */

/*              METHOD may be assigned the following values: */

/*                 'NEAR POINT/ELLIPSOID' */

/*                    The sub-observer point computation uses a */
/*                    triaxial ellipsoid to model the surface of the */
/*                    target body. The sub-observer point is defined */
/*                    as the nearest point on the target relative to */
/*                    the observer. */

/*                    The word "NADIR" may be substituted for the phrase */
/*                    "NEAR POINT" in the string above. */

/*                    For backwards compatibility, the older syntax */

/*                       'Near point: ellipsoid' */

/*                    is accepted as well. */


/*                 'INTERCEPT/ELLIPSOID' */

/*                    The sub-observer point computation uses a */
/*                    triaxial ellipsoid to model the surface of the */
/*                    target body. The sub-observer point is defined */
/*                    as the target surface intercept of the line */
/*                    containing the observer and the target's */
/*                    center. */

/*                    For backwards compatibility, the older syntax */

/*                       'Intercept: ellipsoid' */

/*                    is accepted as well. */


/*                 'NADIR/DSK/UNPRIORITIZED[/SURFACES = <surface list>]' */

/*                    The sub-observer point computation uses DSK data */
/*                    to model the surface of the target body. The */
/*                    sub-observer point is defined as the intercept, on */
/*                    the surface represented by the DSK data, of the */
/*                    line containing the observer and the nearest point */
/*                    on the target's reference ellipsoid. If multiple */
/*                    such intercepts exist, the one closest to the */
/*                    observer is selected. */

/*                    Note that this definition of the sub-observer */
/*                    point is not equivalent to the "nearest point on */
/*                    the surface to the observer." The phrase "NEAR */
/*                    POINT" may NOT be substituted for "NADIR" in the */
/*                    string above. */

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


/*                 'INTERCEPT/DSK/UNPRIORITIZED[/SURFACES = */
/*                                              <surface list>]' */

/*                    The sub-observer point computation uses DSK data */
/*                    to model the surface of the target body. The */
/*                    sub-observer point is defined as the target */
/*                    surface intercept of the line containing the */
/*                    observer and the target's center. */

/*                    If multiple such intercepts exist, the one closest */
/*                    to the observer is selected. */

/*                    The surface list specification is optional. The */
/*                    syntax of the list is identical to that for the */
/*                    NADIR option described above. */


/*                 Neither case nor white space are significant in */
/*                 METHOD, except within double-quoted strings. For */
/*                 example, the string ' eLLipsoid/nearpoint ' is valid. */

/*                 Within double-quoted strings, blank characters are */
/*                 significant, but multiple consecutive blanks are */
/*                 considered equivalent to a single blank. Case is */
/*                 not significant. So */

/*                    "Mars MEGDR 128 PIXEL/DEG" */

/*                 is equivalent to */

/*                    " mars megdr  128  pixel/deg " */

/*                 but not to */

/*                    "MARS MEGDR128PIXEL/DEG" */


/*     TARGET   is the name of the target body. The target body is */
/*              an ephemeris object (its trajectory is given by */
/*              SPK data), and is an extended object. */

/*              The string TARGET is case-insensitive, and leading */
/*              and trailing blanks in TARGET are not significant. */
/*              Optionally, you may supply a string containing the */
/*              integer ID code for the object. For example both */
/*              'MOON' and '301' are legitimate strings that indicate */
/*              the Moon is the target body. */

/*              When the target body's surface is represented by a */
/*              tri-axial ellipsoid, this routine assumes that a */
/*              kernel variable representing the ellipsoid's radii is */
/*              present in the kernel pool. Normally the kernel */
/*              variable would be defined by loading a PCK file. */


/*     ET       is the epoch of participation of the observer, */
/*              expressed as ephemeris seconds past J2000 TDB: ET is */
/*              the epoch at which the observer's state is computed. */

/*              When aberration corrections are not used, ET is also */
/*              the epoch at which the position and orientation of */
/*              the target body are computed. */

/*              When aberration corrections are used, the position */
/*              and orientation of the target body are computed at */
/*              ET-LT or ET+LT, where LT is the one-way light time */
/*              between the sub-observer point and the observer, and */
/*              the sign applied to LT depends on the selected */
/*              correction. See the description of ABCORR below for */
/*              details. */


/*     FIXREF   is the name of a body-fixed reference frame centered */
/*              on the target body. FIXREF may be any such frame */
/*              supported by the SPICE system, including built-in */
/*              frames (documented in the Frames Required Reading) */
/*              and frames defined by a loaded frame kernel (FK). The */
/*              string FIXREF is case-insensitive, and leading and */
/*              trailing blanks in FIXREF are not significant. */

/*              The output sub-observer point SPOINT and the */
/*              observer-to-sub-observer point vector SRFVEC will be */
/*              expressed relative to this reference frame. */

/*     ABCORR   indicates the aberration corrections to be applied */
/*              when computing the target's position and orientation. */

/*              For remote sensing applications, where the apparent */
/*              sub-observer point seen by the observer is desired, */
/*              normally either of the corrections */

/*                 'LT+S' */
/*                 'CN+S' */

/*              should be used. These and the other supported options */
/*              are described below. ABCORR may be any of the */
/*              following: */

/*                 'NONE'     Apply no correction. Return the */
/*                            geometric sub-observer point on the */
/*                            target body. */

/*              Let LT represent the one-way light time between the */
/*              observer and the sub-observer point (note: NOT */
/*              between the observer and the target body's center). */
/*              The following values of ABCORR apply to the */
/*              "reception" case in which photons depart from the */
/*              sub-observer point's location at the light-time */
/*              corrected epoch ET-LT and *arrive* at the observer's */
/*              location at ET: */


/*                 'LT'       Correct for one-way light time (also */
/*                            called "planetary aberration") using a */
/*                            Newtonian formulation. This correction */
/*                            yields the location of sub-observer */
/*                            point at the moment it emitted photons */
/*                            arriving at the observer at ET. */

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
/*                            sub-observer point obtained with the */
/*                            'LT' option to account for the */
/*                            observer's velocity relative to the */
/*                            solar system barycenter. These */
/*                            corrections yield the apparent */
/*                            sub-observer point. */

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
/*                            with the `LT+S' option. Whether the */
/*                            'CN+S' solution is substantially more */
/*                            accurate depends on the geometry of the */
/*                            participating objects and on the */
/*                            accuracy of the input data. In all */
/*                            cases this routine will execute more */
/*                            slowly when a converged solution is */
/*                            computed. */


/*              The following values of ABCORR apply to the */
/*              "transmission" case in which photons *depart* from */
/*              the observer's location at ET and arrive at the */
/*              sub-observer point at the light-time corrected epoch */
/*              ET+LT: */

/*                 'XLT'      "Transmission" case: correct for */
/*                            one-way light time using a Newtonian */
/*                            formulation. This correction yields the */
/*                            sub-observer location at the moment it */
/*                            receives photons emitted from the */
/*                            observer's location at ET. */

/*                            The light time correction uses an */
/*                            iterative solution of the light time */
/*                            equation. The solution invoked by the */
/*                            'LT' option uses one iteration. */

/*                            Both the target position as seen by the */
/*                            observer, and rotation of the target */
/*                            body, are corrected for light time. */

/*                 'XLT+S'    "Transmission" case: correct for */
/*                            one-way light time and stellar */
/*                            aberration using a Newtonian */
/*                            formulation  This option modifies the */
/*                            sub-observer point obtained with the */
/*                            'XLT' option to account for the */
/*                            observer's velocity relative to the */
/*                            solar system barycenter. */

/*                 'XCN'      Converged Newtonian light time */
/*                            correction. This is the same as 'XLT' */
/*                            correction but with further iterations */
/*                            to a converged Newtonian light time */
/*                            solution. */

/*                 'XCN+S'    "Transmission" case: converged */
/*                            Newtonian light time and stellar */
/*                            aberration corrections. */


/*              Neither case nor white space are significant in */
/*              ABCORR. For example, the string */

/*                'Lt + s' */

/*              is valid. */


/*     OBSRVR   is the name of the observing body. The observing body */
/*              is an ephemeris object: it typically is a spacecraft, */
/*              the earth, or a surface point on the earth. OBSRVR is */
/*              case-insensitive, and leading and trailing blanks in */
/*              OBSRVR are not significant. Optionally, you may */
/*              supply a string containing the integer ID code for */
/*              the object. For example both 'MOON' and '301' are */
/*              legitimate strings that indicate the Moon is the */
/*              observer. */

/* $ Detailed_Output */

/*     SPOINT   is the sub-observer point on the target body. */

/*              For target shapes modeled by ellipsoids, the */
/*              sub-observer point is defined either as the point on */
/*              the target body that is closest to the observer, or */
/*              the target surface intercept of the line from the */
/*              observer to the target's center. */

/*              For target shapes modeled by topographic data */
/*              provided by DSK files, the sub-observer point is */
/*              defined as the target surface intercept of the line */
/*              from the observer to either the nearest point on the */
/*              reference ellipsoid, or to the target's center. If */
/*              multiple such intercepts exist, the one closest to */
/*              the observer is selected. */

/*              The input argument METHOD selects the target shape */
/*              model and sub-observer point definition to be used. */

/*              SPOINT is expressed in Cartesian coordinates, */
/*              relative to the body-fixed target frame designated by */
/*              FIXREF. The body-fixed target frame is evaluated at */
/*              the sub-observer epoch TRGEPC (see description below). */

/*              When light time correction is used, the duration of */
/*              light travel between SPOINT to the observer is */
/*              considered to be the one way light time. */

/*              When aberration corrections are used, SPOINT is */
/*              computed using target body position and orientation */
/*              that have been adjusted for the corrections */
/*              applicable to SPOINT itself rather than to the target */
/*              body's center. In particular, if the stellar */
/*              aberration correction applicable to SPOINT is */
/*              represented by a shift vector S, then the light-time */
/*              corrected position of the target is shifted by S */
/*              before the sub-observer point is computed. */

/*              The components of SPOINT have units of km. */


/*     TRGEPC   is the "sub-observer point epoch." TRGEPC is defined */
/*              as follows: letting LT be the one-way light time */
/*              between the observer and the sub-observer point, */
/*              TRGEPC is the epoch ET-LT, ET+LT, or ET depending on */
/*              whether the requested aberration correction is, */
/*              respectively, for received radiation, transmitted */
/*              radiation, or omitted. LT is computed using the */
/*              method indicated by ABCORR. */

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

/*     6)  If the input argument METHOD is not recognized, the error */
/*         SPICE(INVALIDMETHOD) is signaled by this routine, or, the */
/*         error is signaled by a routine in the call tree of this */
/*         routine. */

/*     7)  If the sub-observer point type is not specified or is not */
/*         recognized, the error SPICE(INVALIDSUBTYPE) is signaled. */

/*     8)  If the target and observer have distinct identities but are */
/*         at the same location (for example, the target is Mars and the */
/*         observer is the Mars barycenter), the error */
/*         SPICE(NOSEPARATION) is signaled. */

/*     9)  If insufficient ephemeris data have been loaded prior to */
/*         calling SUBPNT, an error is signaled by a */
/*         routine in the call tree of this routine. Note that when */
/*         light time correction is used, sufficient ephemeris data must */
/*         be available to propagate the states of both observer and */
/*         target to the solar system barycenter. */

/*     10) If the computation method specifies an ellipsoidal target */
/*         shape and triaxial radii of the target body have not been */
/*         loaded into the kernel pool prior to calling SUBPNT, an error */
/*         is signaled by a routine in the call tree of this routine. */

/*     11) The target must be an extended body, and must have a shape */
/*         for which a sub-observer point can be defined. */

/*         If the target body's shape is modeled by DSK data, the shape */
/*         must be such that the specified sub-observer point */
/*         definition is applicable. For example, if the target shape */
/*         is a torus, both the NADIR and INTERCEPT definitions might */
/*         be inapplicable, depending on the relative locations of the */
/*         observer and target. */

/*     12) If PCK data specifying the target body-fixed frame orientation */
/*         have not been loaded prior to calling SUBPNT, an error is */
/*         signaled by a routine in the call tree of this routine. */

/*     13) If METHOD specifies that the target surface is represented by */
/*         DSK data, and no DSK files are loaded for the specified */
/*         target, an error is signaled by a routine in the call tree */
/*         of this routine. */

/*     14) If METHOD specifies that the target surface is represented */
/*         by DSK data, and the ray from the observer to the */
/*         sub-observer point doesn't intersect the target body's */
/*         surface, the error SPICE(SUBPOINTNOTFOUND) is signaled. */

/*     15) If the surface intercept on the target body's reference */
/*         ellipsoid of the observer to target center vector cannot not */
/*         be computed, the error SPICE(DEGENERATECASE) is signaled. Note */
/*         that this is a very rare case. */

/*     16) If radii for TARGET are not found in the kernel pool, an error */
/*         is signaled by a routine in the call tree of this routine. */

/*     17) If the size of the TARGET body radii kernel variable is not */
/*         three, an error is signaled by a routine in the call tree of */
/*         this routine. */

/*     18) If any of the three TARGET body radii is less-than or equal to */
/*         zero, an error is signaled by a routine in the call tree of */
/*         this routine. */

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

/*     For ellipsoidal target bodies, there are two different popular */
/*     ways to define the sub-observer point: "nearest point on the */
/*     target to the observer" or "target surface intercept of the line */
/*     containing observer and target." These coincide when the target */
/*     is spherical and generally are distinct otherwise. */

/*     For target body shapes modeled using topographic data provided by */
/*     DSK files, the "surface intercept" notion is valid, but the */
/*     "nearest point on the surface" computation is both inefficient to */
/*     execute and may fail to yield a result that is "under" the */
/*     observer in an intuitively clear way. The NADIR option for DSK */
/*     shapes instead finds the surface intercept of a ray that passes */
/*     through the nearest point on the target reference ellipsoid. For */
/*     shapes modeled using topography, there may be multiple */
/*     ray-surface intercepts; the closest one to the observer is */
/*     selected. */

/*     The NADIR definition makes sense only if the target shape is */
/*     reasonably close to the target's reference ellipsoid. If the */
/*     target is very different---the nucleus of comet */
/*     Churyumov-Gerasimenko is an example---the intercept definition */
/*     should be used. */

/*     This routine computes light time corrections using light time */
/*     between the observer and the sub-observer point, as opposed to */
/*     the center of the target. Similarly, stellar aberration */
/*     corrections done by this routine are based on the direction of */
/*     the vector from the observer to the light-time corrected */
/*     sub-observer point, not to the target center. This technique */
/*     avoids errors due to the differential between aberration */
/*     corrections across the target body. Therefore it's valid to use */
/*     aberration corrections with this routine even when the observer */
/*     is very close to the sub-observer point, in particular when the */
/*     observer to sub-observer point distance is much less than the */
/*     observer to target center distance. */

/*     When comparing sub-observer point computations with results from */
/*     sources other than SPICE, it's essential to make sure the same */
/*     geometric definitions are used. */


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

/*           NADIR/DSK/UNPRIORITIZED/<surface list> */
/*           DSK/NADIR/<surface list>/UNPRIORITIZED */
/*           UNPRIORITIZED/<surface list>/DSK/NADIR */

/*        The simplest form of the METHOD argument specifying use of */
/*        DSK data is one that lacks a surface list, for example: */

/*           'NADIR/DSK/UNPRIORITIZED' */
/*           'INTERCEPT/DSK/UNPRIORITIZED' */

/*        For applications in which all loaded DSK data for the target */
/*        body are for a single surface, and there are no competing */
/*        segments, the above strings suffice. This is expected to be */
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

/*        'NADIR/DSK/UNPRIORITIZED/SURFACES= "Mars MEGDR 64 PIXEL/DEG",3' */



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


/*     1) Find the sub-Earth point on Mars for a specified time. */

/*        Compute the sub-Earth points using both triaxial ellipsoid */
/*        and topographic surface models. Topography data are provided by */
/*        a DSK file. For the ellipsoid model, use both the "intercept" */
/*        and "near point" sub-observer point definitions; for the DSK */
/*        case, use both the "intercept" and "nadir" definitions. */

/*        Display the locations of both the Earth and the sub-Earth */
/*        point relative to the center of Mars, in the IAU_MARS */
/*        body-fixed reference frame, using both planetocentric and */
/*        planetographic coordinates. */

/*        The topographic model is based on data from the MGS MOLA DEM */
/*        megr90n000cb, which has a resolution of 4 pixels/degree. A */
/*        triangular plate model was produced by computing a 720 x 1440 */
/*        grid of interpolated heights from this DEM, then tessellating */
/*        the height grid. The plate model is stored in a type 2 segment */
/*        in the referenced DSK file. */

/*        Use the meta-kernel shown below to load the required SPICE */
/*        kernels. */


/*           KPL/MK */

/*           File: subpnt_ex1.tm */

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
/*              megr90n000cb_plate.bds           Plate model based on */
/*                                               MEGDR DEM, resolution */
/*                                               4 pixels/degree. */

/*           \begindata */

/*              KERNELS_TO_LOAD = ( 'de430.bsp', */
/*                                  'mar097.bsp', */
/*                                  'pck00010.tpc', */
/*                                  'naif0011.tls', */
/*                                  'megr90n000cb_plate.bds' ) */
/*           \begintext */

/*           End of meta-kernel */


/*        Example code begins here. */


/*              PROGRAM SUBPNT_EX1 */
/*              IMPLICIT NONE */
/*        C */
/*        C     SPICELIB functions */
/*        C */
/*              DOUBLE PRECISION      DPR */
/*              DOUBLE PRECISION      VNORM */
/*        C */
/*        C     Local parameters */
/*        C */
/*              CHARACTER*(*)         META */
/*              PARAMETER           ( META   = 'subpnt_ex1.tm' ) */

/*              CHARACTER*(*)         FM */
/*              PARAMETER           ( FM     =  '(A,F21.9)' ) */

/*              INTEGER               MTHLEN */
/*              PARAMETER           ( MTHLEN = 50 ) */

/*              INTEGER               NMETH */
/*              PARAMETER           ( NMETH  = 4 ) */

/*        C */
/*        C     Local variables */
/*        C */
/*              CHARACTER*(MTHLEN)    METHOD ( NMETH ) */

/*              DOUBLE PRECISION      ET */
/*              DOUBLE PRECISION      F */
/*              DOUBLE PRECISION      OBSPOS ( 3 ) */
/*              DOUBLE PRECISION      ODIST */
/*              DOUBLE PRECISION      OPCLAT */
/*              DOUBLE PRECISION      OPCLON */
/*              DOUBLE PRECISION      OPCRAD */
/*              DOUBLE PRECISION      OPGALT */
/*              DOUBLE PRECISION      OPGLAT */
/*              DOUBLE PRECISION      OPGLON */
/*              DOUBLE PRECISION      RADII  ( 3 ) */
/*              DOUBLE PRECISION      RE */
/*              DOUBLE PRECISION      RP */
/*              DOUBLE PRECISION      SPCLAT */
/*              DOUBLE PRECISION      SPCLON */
/*              DOUBLE PRECISION      SPCRAD */
/*              DOUBLE PRECISION      SPGALT */
/*              DOUBLE PRECISION      SPGLAT */
/*              DOUBLE PRECISION      SPGLON */
/*              DOUBLE PRECISION      SPOINT ( 3 ) */
/*              DOUBLE PRECISION      SRFVEC ( 3 ) */
/*              DOUBLE PRECISION      TRGEPC */

/*              INTEGER               I */
/*              INTEGER               N */
/*        C */
/*        C     Saved variables */
/*        C */
/*              SAVE                  METHOD */
/*        C */
/*        C     Initial values */
/*        C */
/*              DATA                  METHOD / 'Intercept/ellipsoid', */
/*             .                               'Near point/ellipsoid', */
/*             .                      'Intercept/DSK/Unprioritized', */
/*             .                      'Nadir/DSK/Unprioritized'      / */

/*        C */
/*        C     Load kernel files via the meta-kernel. */
/*        C */
/*              CALL FURNSH ( META ) */

/*        C */
/*        C     Convert the UTC request time string seconds past */
/*        C     J2000, TDB. */
/*        C */
/*              CALL STR2ET ( '2008 AUG 11 00:00:00', ET ) */

/*        C */
/*        C     Look up the target body's radii. We'll use these to */
/*        C     convert Cartesian to planetographic coordinates. Use */
/*        C     the radii to compute the flattening coefficient of */
/*        C     the reference ellipsoid. */
/*        C */
/*              CALL BODVRD ( 'MARS', 'RADII', 3, N, RADII ) */

/*        C */
/*        C     Let RE and RP be, respectively, the equatorial and */
/*        C     polar radii of the target. */
/*        C */
/*              RE = RADII( 1 ) */
/*              RP = RADII( 3 ) */

/*              F  = ( RE - RP ) / RE */

/*        C */
/*        C     Compute sub-observer point using light time and */
/*        C     stellar aberration corrections. Use both ellipsoid */
/*        C     and DSK shape models, and use all of the */
/*        C     "near point," "intercept," and "nadir" sub-observer */
/*        C     point definitions. */
/*        C */
/*              DO I = 1, NMETH */

/*                 CALL SUBPNT ( METHOD(I), */
/*             .                'MARS',  ET,     'IAU_MARS', 'CN+S', */
/*             .                'EARTH', SPOINT, TRGEPC,     SRFVEC ) */
/*        C */
/*        C        Compute the observer's distance from SPOINT. */
/*        C */
/*                 ODIST  = VNORM ( SRFVEC ) */

/*        C */
/*        C        Convert the sub-observer point's rectangular */
/*        C        coordinates to planetographic longitude, latitude */
/*        C        and altitude. Convert radians to degrees. */
/*        C */
/*                 CALL RECPGR ( 'MARS', SPOINT, RE,    F, */
/*             .                 SPGLON, SPGLAT, SPGALT   ) */

/*                 SPGLON = SPGLON * DPR () */
/*                 SPGLAT = SPGLAT * DPR () */

/*        C */
/*        C        Convert sub-observer point's rectangular coordinates */
/*        C        to planetocentric radius, longitude, and latitude. */
/*        C        Convert radians to degrees. */
/*        C */
/*                 CALL RECLAT ( SPOINT, SPCRAD, SPCLON, SPCLAT ) */

/*                 SPCLON = SPCLON * DPR () */
/*                 SPCLAT = SPCLAT * DPR () */

/*        C */
/*        C        Compute the observer's position relative to the center */
/*        C        of the target, where the center's location has been */
/*        C        adjusted using the aberration corrections applicable */
/*        C        to the sub-point. Express the observer's location in */
/*        C        planetographic coordinates. */
/*        C */
/*                 CALL VSUB ( SPOINT, SRFVEC, OBSPOS ) */

/*                 CALL RECPGR ( 'MARS', OBSPOS, RE,    F, */
/*             .                 OPGLON, OPGLAT, OPGALT   ) */

/*                 OPGLON = OPGLON * DPR () */
/*                 OPGLAT = OPGLAT * DPR () */

/*        C */
/*        C        Convert the observer's rectangular coordinates to */
/*        C        planetocentric radius, longitude, and latitude. */
/*        C        Convert radians to degrees. */
/*        C */
/*                 CALL RECLAT ( OBSPOS, OPCRAD, OPCLON, OPCLAT ) */

/*                 OPCLON = OPCLON * DPR () */
/*                 OPCLAT = OPCLAT * DPR () */

/*        C */
/*        C        Write the results. */
/*        C */
/*                 WRITE(*,FM) ' ' */
/*                 WRITE(*,* ) 'Computation method = ', METHOD(I) */
/*                 WRITE(*,FM) ' ' */
/*                 WRITE(*,FM) */
/*             .   '  Observer ALT relative to spheroid (km) =', OPGALT */
/*                 WRITE(*,FM) */
/*             .   '  Length of SRFVEC                  (km) =', ODIST */
/*                 WRITE(*,FM) */
/*             .   '  Sub-observer point ALT            (km) =', SPGALT */
/*                 WRITE(*,FM) */
/*             .   '  Sub-observer planetographic LON  (deg) =', SPGLON */
/*                 WRITE(*,FM) */
/*             .   '  Observer planetographic LON      (deg) =', OPGLON */
/*                 WRITE(*,FM) */
/*             .   '  Sub-observer planetographic LAT  (deg) =', SPGLAT */
/*                 WRITE(*,FM) */
/*             .   '  Observer planetographic LAT      (deg) =', OPGLAT */
/*                 WRITE(*,FM) */
/*             .   '  Sub-observer planetocentric LON  (deg) =', SPCLON */
/*                 WRITE(*,FM) */
/*             .   '  Observer planetocentric LON      (deg) =', OPCLON */
/*                 WRITE(*,FM) */
/*             .   '  Sub-observer planetocentric LAT  (deg) =', SPCLAT */
/*                 WRITE(*,FM) */
/*             .   '  Observer planetocentric LAT      (deg) =', OPCLAT */
/*                 WRITE(*,FM) ' ' */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Computation method = Intercept/ellipsoid */

/*          Observer ALT relative to spheroid (km) =  349199089.540947020 */
/*          Length of SRFVEC                  (km) =  349199089.577642620 */
/*          Sub-observer point ALT            (km) =          0.000000000 */
/*          Sub-observer planetographic LON  (deg) =        199.302305028 */
/*          Observer planetographic LON      (deg) =        199.302305028 */
/*          Sub-observer planetographic LAT  (deg) =         26.262401237 */
/*          Observer planetographic LAT      (deg) =         25.994936751 */
/*          Sub-observer planetocentric LON  (deg) =        160.697694972 */
/*          Observer planetocentric LON      (deg) =        160.697694972 */
/*          Sub-observer planetocentric LAT  (deg) =         25.994934171 */
/*          Observer planetocentric LAT      (deg) =         25.994934171 */


/*         Computation method = Near point/ellipsoid */

/*          Observer ALT relative to spheroid (km) =  349199089.540938556 */
/*          Length of SRFVEC                  (km) =  349199089.540938556 */
/*          Sub-observer point ALT            (km) =          0.000000000 */
/*          Sub-observer planetographic LON  (deg) =        199.302305029 */
/*          Observer planetographic LON      (deg) =        199.302305029 */
/*          Sub-observer planetographic LAT  (deg) =         25.994936751 */
/*          Observer planetographic LAT      (deg) =         25.994936751 */
/*          Sub-observer planetocentric LON  (deg) =        160.697694971 */
/*          Observer planetocentric LON      (deg) =        160.697694971 */
/*          Sub-observer planetocentric LAT  (deg) =         25.729407227 */
/*          Observer planetocentric LAT      (deg) =         25.994934171 */


/*         Computation method = Intercept/DSK/Unprioritized */

/*          Observer ALT relative to spheroid (km) =  349199089.541017115 */
/*          Length of SRFVEC                  (km) =  349199091.785406590 */
/*          Sub-observer point ALT            (km) =         -2.207669751 */
/*          Sub-observer planetographic LON  (deg) =        199.302304999 */
/*          Observer planetographic LON      (deg) =        199.302304999 */
/*          Sub-observer planetographic LAT  (deg) =         26.262576677 */
/*          Observer planetographic LAT      (deg) =         25.994936751 */
/*          Sub-observer planetocentric LON  (deg) =        160.697695001 */
/*          Observer planetocentric LON      (deg) =        160.697695001 */
/*          Sub-observer planetocentric LAT  (deg) =         25.994934171 */
/*          Observer planetocentric LAT      (deg) =         25.994934171 */


/*         Computation method = Nadir/DSK/Unprioritized */

/*          Observer ALT relative to spheroid (km) =  349199089.541007578 */
/*          Length of SRFVEC                  (km) =  349199091.707172215 */
/*          Sub-observer point ALT            (km) =         -2.166164622 */
/*          Sub-observer planetographic LON  (deg) =        199.302305000 */
/*          Observer planetographic LON      (deg) =        199.302305000 */
/*          Sub-observer planetographic LAT  (deg) =         25.994936751 */
/*          Observer planetographic LAT      (deg) =         25.994936751 */
/*          Sub-observer planetocentric LON  (deg) =        160.697695000 */
/*          Observer planetocentric LON      (deg) =        160.697695000 */
/*          Sub-observer planetocentric LAT  (deg) =         25.729237570 */
/*          Observer planetocentric LAT      (deg) =         25.994934171 */


/*     2) Use SUBPNT to find the sub-spacecraft point on Mars for the */
/*        Mars Reconnaissance Orbiter spacecraft (MRO) at a specified */
/*        time, using both the 'Ellipsoid/Near point' computation method */
/*        and an ellipsoidal target shape, and the */
/*        'DSK/Unprioritized/Nadir' method and a DSK-based shape model. */

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

/*           File: subpnt_ex2.tm */

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


/*              PROGRAM SUBPNT_EX2 */
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
/*              PARAMETER           ( META   = 'subpnt_ex2.tm' ) */

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
/*        c           expressed relative to the IAU_MARS frame at */
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
/*             .      // ABCORR(J) */
/*                    WRITE(*,F1) ' ' */
/*                    WRITE(*,F2) '      MRO-to-sub-observer vector in' */
/*                    WRITE(*,F2) '      MRO HIRISE look direction frame' */
/*                    WRITE(*,F1) '        X-component             ' */
/*             .      // '(km) = ', MROVEC(1) */
/*                    WRITE(*,F1) '        Y-component             ' */
/*             .      // '(km) = ', MROVEC(2) */
/*                    WRITE(*,F1) '        Z-component             ' */
/*             .      // '(km) = ', MROVEC(3) */
/*                    WRITE(*,F1) '      Sub-observer point radius ' */
/*             .      // '(km) = ', RADIUS */
/*                    WRITE(*,F1) '      Planetocentric latitude  ' */
/*             .      // '(deg) = ', LAT */
/*                    WRITE(*,F1) '      Planetocentric longitude ' */
/*             .      // '(deg) = ', LON */
/*                    WRITE(*,F1) '      Observer altitude         ' */
/*             .      // '(km) = ', ALT */

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

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     S.C. Krening       (JPL) */
/*     B.V. Semenov       (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.1.0, 01-NOV-2021 (JDR) (EDW) (NJB) */

/*        Bug fix: PRVCOR is no longer set to blank before */
/*        ABCORR is parsed. */

/*        Body radii accessed from kernel pool using ZZGFTREB. */

/*        Edited the header to comply with NAIF standard. */
/*        Changed code examples' output to comply with maximum */
/*        line length for header comments. */

/*        Bug fix: TRGEPC is now initialized prior to first use. */
/*        Previously the lack of initialization could cause this routine */
/*        to fail to find DSK data within the time bounds of a DSK */
/*        segment. */

/* -    SPICELIB Version 2.0.0, 04-APR-2017 (NJB) */

/*        Added FAILED tests. */

/*        01-JUL-2016 (NJB) */

/*        Now uses surface mapping tracking capability. */
/*        Updated header. Changed aberration correction */
/*        in example 1 to CN+S. */

/*        09-FEB-2015 (NJB) */

/*        Updated code to support use of surface list. */
/*        Updated header to document DSK capabilities. */
/*        Added check for invalid sub-point type. */

/*        24-DEC-2014 (NJB) */

/*        Updated to support surfaces represented by DSK data. */

/*        Bug fix: set initial value of PRVMTH to a valid */
/*        value. */

/* -    SPICELIB Version 1.3.0, 31-MAR-2014 (BVS) */

/*        Updated to save the input body names and ZZBODTRN state */
/*        counters and to do name-ID conversions only if the counters */
/*        have changed. */

/*        Updated to save the input frame name and POOL state counter */
/*        and to do frame name-ID conversion only if the counter has */
/*        changed. */

/*        Updated to call LJUCRS instead of CMPRSS/UCASE. */

/* -    SPICELIB Version 1.2.0, 02-APR-2012 (NJB) (SCK) */

/*        Bug fix: FIRST is now set to .FALSE. at the completion */
/*        of a successful initialization pass. This does not affect */
/*        the routine's outputs but improves efficiency. */

/*        References to the new PXFRM2 routine were added, which changed */
/*        the Detailed Output section and the second example. */

/*        Upgrade: this routine now uses ZZVALCOR rather than */
/*        ZZPRSCOR, simplifying the implementation. */

/* -    SPICELIB Version 1.1.0, 18-MAY-2010 (NJB) */

/*        Bug fix: calls to FAILED() have been added after */
/*        SPK calls, target radius lookup, near point */
/*        and surface intercept computations. */

/* -    SPICELIB Version 1.0.1, 06-FEB-2009 (NJB) */

/*        Typo correction: changed FIXFRM to FIXREF in header */
/*        documentation. Meta-kernel name suffix was changed to */
/*        ".tm" in header code example. */

/* -    SPICELIB Version 1.0.0, 02-MAR-2008 (NJB) */

/* -& */
/* $ Index_Entries */

/*     find sub-observer point on target body */
/*     find sub-spacecraft point on target body */
/*     find nearest point to observer on target body */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     This value will become system-dependent when systems */
/*     using 128-bit d.p. numbers are supported by SPICELIB. */
/*     CNVLIM, when added to 1.0D0, should yield 1.0D0. */


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
    chkin_("SUBPNT", (ftnlen)6);

/*     Counter initialization is done separately. */

    if (first) {

/*        Initialize counters. */

	zzctruin_(svctr1);
	zzctruin_(svctr2);
	zzctruin_(svctr3);
    }
    if (first || s_cmp(abcorr, prvcor, abcorr_len, (ftnlen)5) != 0) {

/*        The aberration correction flag differs from the value it */
/*        had on the previous call, if any. Analyze the new flag. */

	zzvalcor_(abcorr, attblk, abcorr_len);
	if (failed_()) {
	    chkout_("SUBPNT", (ftnlen)6);
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

/*        The aberration correction flag is recognized; save it. */

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
	chkout_("SUBPNT", (ftnlen)6);
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
	chkout_("SUBPNT", (ftnlen)6);
	return 0;
    }

/*     Check the input body codes.  If they are equal, signal */
/*     an error. */

    if (obscde == trgcde) {
	setmsg_("In computing the sub-observer point, the observing body and"
		" target body are the same. Both are #.", (ftnlen)97);
	errch_("#", obsrvr, (ftnlen)1, obsrvr_len);
	sigerr_("SPICE(BODIESNOTDISTINCT)", (ftnlen)24);
	chkout_("SUBPNT", (ftnlen)6);
	return 0;
    }

/*     Determine the attributes of the frame designated by FIXREF. */

    zznamfrm_(svctr3, svfref, &svrefc, fixref, &fixfid, (ftnlen)32, 
	    fixref_len);
    frinfo_(&fixfid, &fixctr, &fixcls, &fixcid, &fnd);
    if (failed_()) {
	chkout_("SUBPNT", (ftnlen)6);
	return 0;
    }
    if (! fnd) {
	setmsg_("Reference frame # is not recognized by the SPICE frame subs"
		"ystem. Possibly a required frame definition kernel has not b"
		"een loaded.", (ftnlen)130);
	errch_("#", fixref, (ftnlen)1, fixref_len);
	sigerr_("SPICE(NOFRAME)", (ftnlen)14);
	chkout_("SUBPNT", (ftnlen)6);
	return 0;
    }

/*     Make sure that FIXREF is centered at the target body's center. */

    if (fixctr != trgcde) {
	setmsg_("Reference frame # is not centered at the the target body #."
		" The ID code of the frame center is #.", (ftnlen)97);
	errch_("#", fixref, (ftnlen)1, fixref_len);
	errch_("#", target, (ftnlen)1, target_len);
	errint_("#", &fixctr, (ftnlen)1);
	sigerr_("SPICE(INVALIDFRAME)", (ftnlen)19);
	chkout_("SUBPNT", (ftnlen)6);
	return 0;
    }

/*     Check whether the surface name/ID mapping has been updated. */

    zzsrftrk_(svctr4, &surfup);

/*     If necessary, parse the method specification. PRVMTH records the */
/*     last valid value of METHOD; PRI, NEAR, SHAPE, NSURF, and SRFLST */
/*     are the corresponding saved variables. */

    if (first || surfup || s_cmp(method, prvmth, method_len, (ftnlen)500) != 
	    0) {

/*        Make sure parsed values from METHOD can't be reused before */
/*        we've checked them. */

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
	    chkout_("SUBPNT", (ftnlen)6);
	    return 0;
	}
	if (s_cmp(subtyp, " ", (ftnlen)20, (ftnlen)1) == 0) {
	    setmsg_("Sub-observer point type was invalid or was not found in"
		    " the method string #.", (ftnlen)76);
	    errch_("#", method, (ftnlen)1, method_len);
	    sigerr_("SPICE(INVALIDSUBTYPE)", (ftnlen)21);
	    chkout_("SUBPNT", (ftnlen)6);
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
	    chkout_("SUBPNT", (ftnlen)6);
	    return 0;
	}
	if (shape == 1) {

/*           Allow both "near point" and "nadir" expressions the */
/*           ellipsoid case, since in these case, these are equivalent. */

	    near__ = eqstr_(subtyp, "NEAR POINT", (ftnlen)20, (ftnlen)10) || 
		    eqstr_(subtyp, "NADIR", (ftnlen)20, (ftnlen)5);
	} else {

/*           "near point" is not supported for DSKs. */

	    near__ = eqstr_(subtyp, "NADIR", (ftnlen)20, (ftnlen)5);
	}
	if (! near__) {
	    if (! eqstr_(subtyp, "INTERCEPT", (ftnlen)20, (ftnlen)9)) {
		setmsg_("Invalid sub-observer point type <#> was found in th"
			"e method string #.", (ftnlen)69);
		errch_("#", subtyp, (ftnlen)1, (ftnlen)20);
		errch_("#", method, (ftnlen)1, method_len);
		sigerr_("SPICE(INVALIDSUBTYPE)", (ftnlen)21);
		chkout_("SUBPNT", (ftnlen)6);
		return 0;
	    }
	}

/*        Save the current value of METHOD. */

	s_copy(prvmth, method, (ftnlen)500, method_len);
    }

/*     At this point, the first pass actions were successful. */

    first = FALSE_;
    if (shape == 2) {

/*        This is the DSK case. */

/*        Initialize the intercept algorithm to use a DSK */
/*        model for the surface of the target body. */

	zzsudski_(&trgcde, &nsurf, srflst, &fixfid);
    } else if (shape != 1) {
	setmsg_("Computation method argument was <#>; this string must speci"
		"fy a supported shape model and computation type. See the des"
		"cription of METHOD in the header of SUBPNT for details.", (
		ftnlen)174);
	errch_("#", method, (ftnlen)1, method_len);
	sigerr_("SPICE(INVALIDMETHOD)", (ftnlen)20);
	chkout_("SUBPNT", (ftnlen)6);
	return 0;
    }
    if (failed_()) {
	chkout_("SUBPNT", (ftnlen)6);
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

/*     Determine the position of the observer in the target body-fixed */
/*     frame. This is a first estimate. */

/*         -  Call SPKEZP to compute the position of the target body as */
/*            seen from the observing body and the light time (LT) */
/*            between them. We request that the coordinates of POS be */
/*            returned relative to the body fixed reference frame */
/*            associated with the target body, using aberration */
/*            corrections specified by the input argument ABCORR. */

/*         -  Call VMINUS to negate the direction of the vector (OBSPOS) */
/*            so it will be the position of the observer as seen from */
/*            the target body in target body fixed coordinates. */

/*            Note that this result is not the same as the result of */
/*            calling SPKEZP with the target and observer switched. We */
/*            computed the vector FROM the observer TO the target in */
/*            order to get the proper light time and stellar aberration */
/*            corrections (if requested). Now we need the inverse of */
/*            that corrected vector in order to compute the sub-observer */
/*            point. */

    spkezp_(&trgcde, et, fixref, abcorr, &obscde, tpos, &lt, fixref_len, 
	    abcorr_len);
    if (failed_()) {
	chkout_("SUBPNT", (ftnlen)6);
	return 0;
    }

/*     Negate the target's position to obtain the position of the */
/*     observer relative to the target. */

    vminus_(tpos, obspos);

/*     Make a first estimate of the target epoch. */

    *trgepc = *et + s * lt;

/*     Find the sub-observer point given the target epoch, */
/*     observer-target position, and target body orientation we've */
/*     already computed. If we're not using light time correction, this */
/*     is all we need do. Otherwise, our result will give us an initial */
/*     estimate of the target epoch, which we'll then improve. */


/*     Get the radii of the target body from the kernel pool. */

    zzgftreb_(&trgcde, radii);
    if (failed_()) {
	chkout_("SUBPNT", (ftnlen)6);
	return 0;
    }
    range = vnorm_(obspos);
    if (range == 0.) {

/*        We've already ensured that observer and target are */
/*        distinct, so this should be a very unusual occurrence. */

	setmsg_("Observer-target distance is zero. Observer is #; target is "
		"#.", (ftnlen)61);
	errch_("#", obsrvr, (ftnlen)1, obsrvr_len);
	errch_("#", target, (ftnlen)1, target_len);
	sigerr_("SPICE(NOSEPARATION)", (ftnlen)19);
	chkout_("SUBPNT", (ftnlen)6);
	return 0;
    }

/*     Make a first estimate of the sub-observer point. The algorithm */
/*     we use depends on the sub-observer point definition. */

    if (near__) {

/*        Locate the nearest point to the observer on the target */
/*        body's reference ellipsoid. */

	nearpt_(obspos, radii, &radii[1], &radii[2], spoint, &alt);
	if (failed_()) {
	    chkout_("SUBPNT", (ftnlen)6);
	    return 0;
	}

/*        If the target is an ellipsoid, the NEARPT call above does */
/*        the trick. For DSKs, we define a ray emanating from the */
/*        observer and passing through the near point on the */
/*        reference ellipsoid. The closest ray-DSK surface intercept */
/*        to the observer is the initial estimate of the sub-point. */

	if (shape == 2) {

/*           Generate the ray direction; find the DSK intercept. */

	    vsub_(spoint, obspos, dvec);
	    zzsbfxr_(&trgcde, &nsurf, srflst, trgepc, &fixfid, obspos, dvec, 
		    spoint, &fnd);
	    if (failed_()) {
		chkout_("SUBPNT", (ftnlen)6);
		return 0;
	    }
	    if (! fnd) {
		setmsg_("No sub-observer point was found on the surface defi"
			"ned by DSK data.Observer is #; target is #. This pro"
			"blem can occur for bodies having shapes not well mod"
			"eled by ellipsoids. Consider using the \"Intercept: "
			"DSK\" computation method.", (ftnlen)230);
		errch_("#", obsrvr, (ftnlen)1, obsrvr_len);
		errch_("#", target, (ftnlen)1, target_len);
		sigerr_("SPICE(SUBPOINTNOTFOUND)", (ftnlen)23);
		chkout_("SUBPNT", (ftnlen)6);
		return 0;
	    }

/*           Re-compute the altitude using the intercept on the DSK */
/*           surface. */

	    vsub_(spoint, obspos, srfvec);
	    alt = vnorm_(srfvec);
	}
    } else {

/*        This is the case for the "intercept" sub-point definition. */

	if (shape == 1) {

/*           Locate the surface intercept of the ray from the */
/*           observer to the target center. */

	    surfpt_(obspos, tpos, radii, &radii[1], &radii[2], spoint, &fnd);
	    if (failed_()) {
		chkout_("SUBPNT", (ftnlen)6);
		return 0;
	    }
	    if (! fnd) {

/*              If there's no intercept, we have a numerical problem. */

		setmsg_("No intercept of observer-target ray was found.", (
			ftnlen)46);
		sigerr_("SPICE(DEGENERATECASE)", (ftnlen)21);
		chkout_("SUBPNT", (ftnlen)6);
		return 0;
	    }
	    alt = vdist_(obspos, spoint);
	} else {

/*           Generate the ray direction; find the DSK intercept. */

	    vminus_(obspos, dvec);
	    zzsbfxr_(&trgcde, &nsurf, srflst, trgepc, &fixfid, obspos, dvec, 
		    spoint, &fnd);
	    if (failed_()) {
		chkout_("SUBPNT", (ftnlen)6);
		return 0;
	    }
	    if (! fnd) {
		setmsg_("No sub-observer point was found on the surface defi"
			"ned by DSK data.Observer is #; target is #. This pro"
			"blem can occur for a body having an irregular shape "
			"such that the origin of the body-fixed reference fra"
			"me is outside of the body. A torus is an example of "
			"such a shape.", (ftnlen)272);
		errch_("#", obsrvr, (ftnlen)1, obsrvr_len);
		errch_("#", target, (ftnlen)1, target_len);
		sigerr_("SPICE(SUBPOINTNOTFOUND)", (ftnlen)23);
		chkout_("SUBPNT", (ftnlen)6);
		return 0;
	    }

/*           Re-compute the altitude using the intercept on the DSK */
/*           surface. */

	    vsub_(spoint, obspos, srfvec);
	    alt = vnorm_(srfvec);
	}
    }
    if (failed_()) {
	chkout_("SUBPNT", (ftnlen)6);
	return 0;
    }

/*     If we're not using light time and stellar aberration */
/*     corrections, we're almost done now. Note that we need only */
/*     check for use of light time corrections, because use of */
/*     stellar aberration corrections alone has been prevented by an */
/*     earlier check. */
    if (! uselt) {
	*trgepc = *et;

/*        The TRGEPC value we'll return is just the input time. */
/*        The previous call to SPKEZP call yielded */
/*        the vector OBSPOS. SPOINT was set immediately above. The */
/*        only output left to compute is SRFVEC. */

	vsub_(spoint, obspos, srfvec);
	chkout_("SUBPNT", (ftnlen)6);
	return 0;
    }

/*     Compute the one-way light time and target epoch based on our */
/*     first computation of SPOINT. The coefficient S has been */
/*     set to give us the correct answer for each aberration */
/*     correction case. */

    lt = alt / clight_();
    *trgepc = *et + s * lt;

/*     We'll now make an improved sub-observer point estimate using */
/*     the previous estimate of the sub-observer point. The number of */
/*     iterations depends on the light time correction type. */
    if (usecn) {
	nitr = 5;
    } else {
	nitr = 1;
    }

/*     Get the J2000-relative state of the observer relative to */
/*     the solar system barycenter at ET. */

    spkssb_(&obscde, et, "J2000", ssbost, (ftnlen)5);
    if (failed_()) {
	chkout_("SUBPNT", (ftnlen)6);
	return 0;
    }

/*     Initialize the variables required to evaluate the */
/*     loop termination condition. */

    i__ = 0;
    ltdiff = 1.;
    etdiff = 1.;
    prevlt = lt;
    prevet = *trgepc;
    while(i__ < nitr && ltdiff > abs(lt) * 1e-17 && etdiff > 0.) {

/*        Get the J2000-relative state of the target relative to */
/*        the solar system barycenter at the target epoch. */

	spkssb_(&trgcde, trgepc, "J2000", ssbtst, (ftnlen)5);
	if (failed_()) {
	    chkout_("SUBPNT", (ftnlen)6);
	    return 0;
	}

/*        Find the position of the observer relative to the target. */
/*        Convert this vector from the J2000 frame to the target */
/*        frame at TRGEPC. */

	vsub_(ssbost, ssbtst, j2pos);
	pxform_("J2000", fixref, trgepc, xform, (ftnlen)5, fixref_len);
	if (failed_()) {
	    chkout_("SUBPNT", (ftnlen)6);
	    return 0;
	}
	mxv_(xform, j2pos, obspos);

/*        If we're using stellar aberration corrections, adjust the */
/*        observer position to account for the stellar aberration */
/*        correction applicable to SPOINT. */

	if (usestl) {

/*           We want to apply the stellar aberration correction that */
/*           applies to our current estimate of the sub-observer point */
/*           location, NOT the correction for the target body's center. */
/*           In most cases the two corrections will be similar, but they */
/*           might not be---consider the case of a highly prolate target */
/*           body where the observer is close to one "end" of the body. */

/*           Find the vector from the observer to the estimated */
/*           sub-observer point. Find the stellar aberration offset */
/*           STLOFF for this vector. Note that all vectors are expressed */
/*           relative to the target body-fixed frame at TRGEPC. We must */
/*           perform our corrections in an inertial frame. */

	    vsub_(spoint, obspos, subvec);
	    mtxv_(xform, subvec, subvj2);
	    if (xmit) {
		stlabx_(subvj2, &ssbost[3], corvj2);
	    } else {
		stelab_(subvj2, &ssbost[3], corvj2);
	    }
	    mxv_(xform, corvj2, corpos);
	    vsub_(corpos, subvec, stloff);

/*           In principle, we want to shift the target body position */
/*           relative to the solar system barycenter by STLOFF, but we */
/*           can skip this step and just re-compute the observer's */
/*           location relative to the target body's center by */
/*           subtracting off STLOFF. */

	    vsub_(obspos, stloff, vtemp);
	    vequ_(vtemp, obspos);
	}

/*        Find the sub-observer point using the current estimated */
/*        geometry. */

	if (near__) {

/*           Locate the nearest point to the observer on the target's */
/*           reference ellipsoid. */

	    nearpt_(obspos, radii, &radii[1], &radii[2], spoint, &alt);
	    if (failed_()) {
		chkout_("SUBPNT", (ftnlen)6);
		return 0;
	    }

/*           If the target is an ellipsoid, the NEARPT call above */
/*           does the trick. For DSKs, we define a ray emanating from */
/*           the observer and passing through the near point on the */
/*           reference ellipsoid. The closest ray-DSK surface */
/*           intercept to the observer is the initial estimate of the */
/*           sub-point. */
	    if (shape == 2) {

/*              Generate the ray direction; find the DSK intercept. */

		vsub_(spoint, obspos, dvec);
		zzsbfxr_(&trgcde, &nsurf, srflst, trgepc, &fixfid, obspos, 
			dvec, spoint, &fnd);
		if (failed_()) {
		    chkout_("SUBPNT", (ftnlen)6);
		    return 0;
		}
		if (! fnd) {
		    setmsg_("No sub-observer point was found on the surface "
			    "defined by DSK data.Observer is #; target is #. "
			    "This problem can occur for bodies having shapes "
			    "not well modeled by ellipsoids. Consider using t"
			    "he \"Intercept: DSK\" computation method.", (
			    ftnlen)230);
		    errch_("#", obsrvr, (ftnlen)1, obsrvr_len);
		    errch_("#", target, (ftnlen)1, target_len);
		    sigerr_("SPICE(SUBPOINTNOTFOUND)", (ftnlen)23);
		    chkout_("SUBPNT", (ftnlen)6);
		    return 0;
		}

/*              Re-compute the altitude using the intercept on the */
/*              DSK surface. */

		vsub_(spoint, obspos, srfvec);
		alt = vnorm_(srfvec);
	    }
	} else {

/*           This is the "intercept" case. */

/*           Generate the ray direction. */

	    vminus_(obspos, dvec);

/*           Locate the surface intercept of the ray from the */
/*           observer to the target center. */

	    if (shape == 1) {
		surfpt_(obspos, dvec, radii, &radii[1], &radii[2], spoint, &
			fnd);
		if (failed_()) {
		    chkout_("SUBPNT", (ftnlen)6);
		    return 0;
		}
		if (! fnd) {

/*                 If there's no intercept, we have a numerical */
/*                 problem. */

		    setmsg_("No intercept of observer-target ray was found.", 
			    (ftnlen)46);
		    sigerr_("SPICE(DEGENERATECASE)", (ftnlen)21);
		    chkout_("SUBPNT", (ftnlen)6);
		    return 0;
		}
		alt = vdist_(obspos, spoint);
	    } else {

/*              Find the ray-DSK surface intercept. */

		zzsbfxr_(&trgcde, &nsurf, srflst, trgepc, &fixfid, obspos, 
			dvec, spoint, &fnd);
		if (failed_()) {
		    chkout_("SUBPNT", (ftnlen)6);
		    return 0;
		}
		if (! fnd) {
		    setmsg_("No sub-observer point was found on the surface "
			    "defined by DSK data.Observer is #; target is #. "
			    "This problem can occur for a body having an irre"
			    "gular shape such that the origin of the body-fix"
			    "ed reference frame is outside of the body. A tor"
			    "us is an example of such a shape.", (ftnlen)272);
		    errch_("#", obsrvr, (ftnlen)1, obsrvr_len);
		    errch_("#", target, (ftnlen)1, target_len);
		    sigerr_("SPICE(SUBPOINTNOTFOUND)", (ftnlen)23);
		    chkout_("SUBPNT", (ftnlen)6);
		    return 0;
		}

/*              Compute the altitude using the intercept on the DSK */
/*              surface. */

		vsub_(spoint, obspos, srfvec);
		alt = vnorm_(srfvec);
	    }
	}
	if (failed_()) {
	    chkout_("SUBPNT", (ftnlen)6);
	    return 0;
	}

/*        Compute a new light time estimate and new target epoch. */

	lt = alt / clight_();
	*trgepc = *et + s * lt;

/*        At this point, we have new estimates of the sub-observer */
/*        point SPOINT, the observer altitude ALT, the target epoch */
/*        TRGEPC, and the position of the observer relative to the */
/*        target OBSPOS. */

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

/*     SPOINT, TRGEPC, and OBSPOS have been set at this point. Compute */
/*     SRFVEC. */

    vsub_(spoint, obspos, srfvec);
    chkout_("SUBPNT", (ftnlen)6);
    return 0;
} /* subpnt_ */

