/* subslr.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__100 = 100;
static integer c__10 = 10;

/* $Procedure SUBSLR ( Sub-solar point ) */
/* Subroutine */ int subslr_(char *method, char *target, doublereal *et, char 
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
    static integer shape = 1;

    /* System generated locals */
    doublereal d__1, d__2;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int vadd_(doublereal *, doublereal *, doublereal *
	    ), zzbods2c_(integer *, char *, integer *, logical *, char *, 
	    integer *, logical *, ftnlen, ftnlen);
    doublereal dvec[3];
    integer nitr;
    extern /* Subroutine */ int vsub_(doublereal *, doublereal *, doublereal *
	    ), vequ_(doublereal *, doublereal *);
    static logical xmit;
    doublereal spos[3], tpos[3];
    extern /* Subroutine */ int zzgftreb_(integer *, doublereal *), zznamfrm_(
	    integer *, char *, integer *, char *, integer *, ftnlen, ftnlen), 
	    zzvalcor_(char *, logical *, ftnlen);
    doublereal j2pos[3];
    extern /* Subroutine */ int zzsudski_(integer *, integer *, integer *, 
	    integer *), zzctruin_(integer *);
    integer i__;
    extern /* Subroutine */ int zzprsmet_(integer *, char *, integer *, char *
	    , char *, logical *, integer *, integer *, char *, char *, ftnlen,
	     ftnlen, ftnlen, ftnlen, ftnlen), zzsrftrk_(integer *, logical *);
    doublereal s, radii[3];
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen);
    static logical usecn;
    extern doublereal vdist_(doublereal *, doublereal *);
    doublereal xform[9]	/* was [3][3] */;
    static integer nsurf;
    extern logical eqstr_(char *, char *, ftnlen, ftnlen);
    static logical uselt;
    doublereal sunst[6];
    static logical svfnd1, svfnd2;
    static integer svctr1[2], svctr2[2];
    extern logical failed_(void);
    static integer svctr3[2], svctr4[2];
    doublereal lt, etdiff;
    integer obscde, fixcid;
    doublereal ltdiff;
    extern doublereal clight_(void);
    integer fixfid, trgcde;
    extern doublereal touchd_(doublereal *);
    extern logical return_(void);
    char pntdef[20], shpstr[9], subtyp[20], trmstr[20];
    doublereal altsun, obspos[3], obsrng, prevet, prevlt, ssbost[6], ssbtst[6]
	    , sslrlt, sslrst[6];
    integer fixcls, fixctr;
    static integer srflst[100];
    logical attblk[15], fnd, surfup;
    static logical usestl;
    static char svtarg[36];
    static integer svtcde;
    static char svobsr[36];
    static integer svobsc;
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
	    ftnlen);
    doublereal slt;
    extern /* Subroutine */ int spkcpo_(char *, doublereal *, char *, char *, 
	    char *, doublereal *, char *, char *, doublereal *, doublereal *, 
	    ftnlen, ftnlen, ftnlen, ftnlen, ftnlen, ftnlen), spkcpt_(
	    doublereal *, char *, char *, doublereal *, char *, char *, char *
	    , char *, doublereal *, doublereal *, ftnlen, ftnlen, ftnlen, 
	    ftnlen, ftnlen, ftnlen), mxv_(doublereal *, doublereal *, 
	    doublereal *), zzsbfxr_(integer *, integer *, integer *, 
	    doublereal *, integer *, doublereal *, doublereal *, doublereal *,
	     logical *);

/* $ Abstract */

/*     Compute the rectangular coordinates of the sub-solar point on */
/*     a target body at a specified epoch, optionally corrected for */
/*     light time and stellar aberration. */

/*     The surface of the target body may be represented by a triaxial */
/*     ellipsoid or by topographic data provided by DSK files. */

/*     This routine supersedes SUBSOL. */

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
/*     SPOINT     O   Sub-solar point on the target body. */
/*     TRGEPC     O   Sub-solar point epoch. */
/*     SRFVEC     O   Vector from observer to sub-solar point. */

/* $ Detailed_Input */

/*     METHOD   is a short string providing parameters defining */
/*              the computation method to be used. In the syntax */
/*              descriptions below, items delimited by brackets */
/*              are optional. */

/*              METHOD may be assigned the following values: */

/*                 'NEAR POINT/ELLIPSOID' */

/*                    The sub-solar point computation uses a triaxial */
/*                    ellipsoid to model the surface of the target body. */
/*                    The sub-solar point is defined as the nearest */
/*                    point on the target relative to the sun. */

/*                    The word "NADIR" may be substituted for the phrase */
/*                    "NEAR POINT" in the string above. */

/*                    For backwards compatibility, the older syntax */

/*                       'Near point: ellipsoid' */

/*                    is accepted as well. */


/*                 'INTERCEPT/ELLIPSOID' */

/*                    The sub-solar point computation uses a triaxial */
/*                    ellipsoid to model the surface of the target body. */
/*                    The sub-solar point is defined as the target */
/*                    surface intercept of the line containing the sun */
/*                    and the target's center. */

/*                    For backwards compatibility, the older syntax */

/*                       'Intercept: ellipsoid' */

/*                    is accepted as well. */


/*                 'NADIR/DSK/UNPRIORITIZED[/SURFACES = <surface list>]' */

/*                    The sub-solar point computation uses DSK data to */
/*                    model the surface of the target body. The */
/*                    sub-solar point is defined as the intercept, on */
/*                    the surface represented by the DSK data, of the */
/*                    line containing the sun and the nearest point on */
/*                    the target's reference ellipsoid. If multiple such */
/*                    intercepts exist, the one closest to the sun is */
/*                    selected. */

/*                    Note that this definition of the sub-solar point */
/*                    is not equivalent to the "nearest point on the */
/*                    surface to the sun." The phrase "NEAR POINT" may */
/*                    NOT be substituted for "NADIR" in the string */
/*                    above. */

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

/*                    The sub-solar point computation uses DSK data to */
/*                    model the surface of the target body. The */
/*                    sub-solar point is defined as the target surface */
/*                    intercept of the line containing the sun and the */
/*                    target's center. */

/*                    If multiple such intercepts exist, the one closest */
/*                    to the sun is selected. */

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
/*              the target body and the position of the Sun are */
/*              computed. */

/*              When aberration corrections are used, ET is the epoch */
/*              at which the observer's state relative to the solar */
/*              system barycenter is computed; in this case the */
/*              position and orientation of the target body are */
/*              computed at ET-LT, where LT is the one-way light time */
/*              between the sub-solar point and the observer. See the */
/*              description of ABCORR below for details. */


/*     FIXREF   is the name of a body-fixed reference frame centered */
/*              on the target body. FIXREF may be any such frame */
/*              supported by the SPICE system, including built-in */
/*              frames (documented in the Frames Required Reading) */
/*              and frames defined by a loaded frame kernel (FK). The */
/*              string FIXREF is case-insensitive, and leading and */
/*              trailing blanks in FIXREF are not significant. */

/*              The output sub-solar point SPOINT and the */
/*              observer-to-sub-solar point vector SRFVEC will be */
/*              expressed relative to this reference frame. */


/*     ABCORR   indicates the aberration correction to be applied */
/*              when computing the target position and orientation */
/*              and the position of the Sun. */

/*              For remote sensing applications, where the apparent */
/*              sub-solar point seen by the observer is desired, */
/*              normally either of the corrections */

/*                 'LT+S' */
/*                 'CN+S' */

/*              should be used. These and the other supported options */
/*              are described below. ABCORR may be any of the */
/*              following: */

/*                 'NONE'     Apply no correction. Return the */
/*                            geometric sub-solar point on the target */
/*                            body. */

/*              Let LT represent the one-way light time between the */
/*              observer and the sub-solar point (note: NOT between */
/*              the observer and the target body's center). The */
/*              following values of ABCORR apply to the "reception" */
/*              case in which photons depart from the sub-solar */
/*              point's location at the light-time corrected epoch */
/*              ET-LT and *arrive* at the observer's location at ET: */

/*                 'LT'       Correct for one-way light time (also */
/*                            called "planetary aberration") using a */
/*                            Newtonian formulation. This correction */
/*                            yields the location of sub-solar */
/*                            point at the moment it emitted photons */
/*                            arriving at the observer at ET. */

/*                            The light time correction uses an */
/*                            iterative solution of the light time */
/*                            equation. The solution invoked by the */
/*                            'LT' option uses one iteration. */

/*                            The target position and orientation as */
/*                            seen by the observer are corrected for */
/*                            light time. The position of the Sun */
/*                            relative to the target is corrected for */
/*                            one-way light time between the Sun and */
/*                            target. */

/*                 'LT+S'     Correct for one-way light time and */
/*                            stellar aberration using a Newtonian */
/*                            formulation. This option modifies the */
/*                            sub-solar point obtained with the 'LT' */
/*                            option to account for the observer's */
/*                            velocity relative to the solar system */
/*                            barycenter. These corrections yield */
/*                            the apparent sub-solar point. */

/*                 'CN'       Converged Newtonian light time */
/*                            correction. In solving the light time */
/*                            equation, the 'CN' correction iterates */
/*                            until the solution converges. Both the */
/*                            position and rotation of the target */
/*                            body, and the position of the Sun, are */
/*                            corrected for light time. */

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

/*              The observer may coincide with the target. */

/* $ Detailed_Output */

/*     SPOINT   is the sub-solar point on the target body. */

/*              For target shapes modeled by ellipsoids, the */
/*              sub-solar point is defined either as the point on the */
/*              target body that is closest to the sun, or the target */
/*              surface intercept of the line from the sun to the */
/*              target's center. */

/*              For target shapes modeled by topographic data */
/*              provided by DSK files, the sub-solar point is defined */
/*              as the target surface intercept of the line from the */
/*              sun to either the nearest point on the reference */
/*              ellipsoid, or to the target's center. If multiple */
/*              such intercepts exist, the one closest to the sun is */
/*              selected. */

/*              The input argument METHOD selects the target shape */
/*              model and sub-solar point definition to be used. */

/*              SPOINT is expressed in Cartesian coordinates, */
/*              relative to the body-fixed target frame designated by */
/*              FIXREF. The body-fixed target frame is evaluated at */
/*              the sub-solar point epoch TRGEPC (see description */
/*              below). */

/*              When aberration corrections are used, SPOINT is */
/*              computed using target body position and orientation */
/*              that have been adjusted for the corrections */
/*              applicable to SPOINT itself rather than to the target */
/*              body's center. In particular, if the stellar */
/*              aberration correction applicable to SPOINT is */
/*              represented by a shift vector S, then the light-time */
/*              corrected position of the target is shifted by S */
/*              before the sub-solar point is computed. */

/*              The components of SPOINT have units of km. */


/*     TRGEPC   is the "sub-solar point epoch." TRGEPC is defined as */
/*              follows: letting LT be the one-way light time between */
/*              the observer and the sub-solar point, TRGEPC is */
/*              either the epoch ET-LT or ET depending on whether the */
/*              requested aberration correction is, respectively, for */
/*              received radiation or omitted. LT is computed using */
/*              the method indicated by ABCORR. */

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

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the specified aberration correction is unrecognized, an */
/*         error is signaled by a routine in the call tree of this */
/*         routine. */

/*     2)  If transmission aberration corrections are specified, the */
/*         error SPICE(NOTSUPPORTED) is signaled. */

/*     3)  If either the target or observer input strings cannot be */
/*         converted to an integer ID code, the error */
/*         SPICE(IDCODENOTFOUND) is signaled. */

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

/*     7)  If the sub-solar point type is not specified or is not */
/*         recognized, the error SPICE(INVALIDSUBTYPE) is signaled. */

/*     8)  If insufficient ephemeris data have been loaded prior to */
/*         calling SUBSLR, an error is signaled by a */
/*         routine in the call tree of this routine. Note that when */
/*         light time correction is used, sufficient ephemeris data must */
/*         be available to propagate the states of observer, target, and */
/*         the Sun to the solar system barycenter. */

/*     9)  If the computation method specifies an ellipsoidal target */
/*         shape and triaxial radii of the target body have not been */
/*         loaded into the kernel pool prior to calling SUBSLR, an error */
/*         is signaled by a routine in the call tree of this routine. */

/*     10) The target must be an extended body, and must have a shape */
/*         for which a sub-solar point can be defined. */

/*         If the target body's shape is modeled by DSK data, the shape */
/*         must be such that the specified sub-solar point definition is */
/*         applicable. For example, if the target shape is a torus, both */
/*         the NADIR and INTERCEPT definitions might be inapplicable, */
/*         depending on the relative locations of the sun and target. */

/*     11) If PCK data specifying the target body-fixed frame orientation */
/*         have not been loaded prior to calling SUBSLR, an error is */
/*         signaled by a routine in the call tree of this routine. */

/*     12) If METHOD specifies that the target surface is represented by */
/*         DSK data, and no DSK files are loaded for the specified */
/*         target, an error is signaled by a routine in the call tree */
/*         of this routine. */

/*     13) If METHOD specifies that the target surface is represented */
/*         by DSK data, and the ray from the observer to the */
/*         sub-observer point doesn't intersect the target body's */
/*         surface, the error SPICE(SUBPOINTNOTFOUND) is signaled. */

/*     14) If the surface intercept on the target body's reference */
/*         ellipsoid of the observer to target center vector cannot not */
/*         be computed, the error SPICE(DEGENERATECASE) is signaled. Note */
/*         that this is a very rare case. */

/*     15) If the target body is the sun, the error SPICE(INVALIDTARGET) */
/*         is signaled. */

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

/*     -  SPK data: ephemeris data for target, observer, and Sun must */
/*        be loaded. If aberration corrections are used, the states of */
/*        target, observer, and the Sun relative to the solar system */
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

/*     There are two different popular ways to define the sub-solar */
/*     point: "nearest point on target to the Sun" or "target surface */
/*     intercept of the line containing the Sun and target." These */
/*     coincide when the target is spherical and generally are distinct */
/*     otherwise. */

/*     This routine computes light time corrections using light time */
/*     between the observer and the sub-solar point, as opposed to the */
/*     center of the target. Similarly, stellar aberration corrections */
/*     done by this routine are based on the direction of the vector */
/*     from the observer to the light-time corrected sub-solar point, */
/*     not to the target center. This technique avoids errors due to the */
/*     differential between aberration corrections across the target */
/*     body. Therefore it's valid to use aberration corrections with */
/*     this routine even when the observer is very close to the */
/*     sub-solar point, in particular when the observer to sub-solar */
/*     point distance is much less than the observer to target center */
/*     distance. */

/*     When comparing sub-solar point computations with results from */
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

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as input, */
/*     the compiler and supporting libraries, and the machine specific */
/*     arithmetic implementation. */


/*     1) Find the sub-solar point on Mars as seen from the Earth for a */
/*        specified time. */

/*        Compute the sub-solar point using both triaxial ellipsoid */
/*        and topographic surface models. Topography data are provided by */
/*        a DSK file. For the ellipsoid model, use both the "intercept" */
/*        and "near point" sub-observer point definitions; for the DSK */
/*        case, use both the "intercept" and "nadir" definitions. */

/*        Display the locations of both the sun and the sub-solar */
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

/*           File: subslr_ex1.tm */

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


/*              PROGRAM SUBSLR_EX1 */
/*              IMPLICIT NONE */
/*        C */
/*        C     SPICELIB functions */
/*        C */
/*              DOUBLE PRECISION      DPR */
/*        C */
/*        C     Local parameters */
/*        C */
/*              CHARACTER*(*)         META */
/*              PARAMETER           ( META   = 'subslr_ex1.tm' ) */

/*              CHARACTER*(*)         FM */
/*              PARAMETER           ( FM     =  '(A,F18.9)' ) */

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
/*              DOUBLE PRECISION      SUNLT */
/*              DOUBLE PRECISION      SUNPOS ( 3 ) */
/*              DOUBLE PRECISION      SUNST  ( 6 ) */
/*              DOUBLE PRECISION      SUPCLN */
/*              DOUBLE PRECISION      SUPCLT */
/*              DOUBLE PRECISION      SUPCRD */
/*              DOUBLE PRECISION      SUPGAL */
/*              DOUBLE PRECISION      SUPGLN */
/*              DOUBLE PRECISION      SUPGLT */
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
/*        C     Convert the UTC request time to ET (seconds past */
/*        C     J2000, TDB). */
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
/*        C     Compute the sub-solar point using light time and stellar */
/*        C     aberration corrections. Use the "target surface */
/*        C     intercept" definition of sub-solar point on the first */
/*        C     loop iteration, and use the "near point" definition on */
/*        C     the second. */
/*        C */
/*              DO I = 1, NMETH */

/*                 CALL SUBSLR ( METHOD(I), */
/*             .                'MARS',  ET,     'IAU_MARS', 'CN+S', */
/*             .                'EARTH', SPOINT, TRGEPC,     SRFVEC ) */
/*        C */
/*        C        Convert the sub-solar point's rectangular coordinates */
/*        C        to planetographic longitude, latitude and altitude. */
/*        C        Convert radians to degrees. */
/*        C */
/*                 CALL RECPGR ( 'MARS', SPOINT, RE,    F, */
/*             .                 SPGLON, SPGLAT, SPGALT   ) */

/*                 SPGLON = SPGLON * DPR () */
/*                 SPGLAT = SPGLAT * DPR () */

/*        C */
/*        C        Convert sub-solar point's rectangular coordinates to */
/*        C        planetocentric radius, longitude, and latitude. */
/*        C        Convert radians to degrees. */
/*        C */
/*                 CALL RECLAT ( SPOINT, SPCRAD, SPCLON, SPCLAT ) */

/*                 SPCLON = SPCLON * DPR () */
/*                 SPCLAT = SPCLAT * DPR () */

/*        C */
/*        C        Compute the Sun's apparent position relative to the */
/*        C        sub-solar point at TRGEPC. Add the position of */
/*        C        the sub-solar point relative to the target's center */
/*        C        to obtain the position of the sun relative to the */
/*        C        target's center. Express the latter position in */
/*        C        planetographic coordinates. */
/*        C */
/*                 CALL SPKCPO ( 'SUN',  TRGEPC, 'IAU_MARS', 'OBSERVER', */
/*             .                 'CN+S', SPOINT, 'MARS',     'IAU_MARS', */
/*             .                 SUNST,  SUNLT                          ) */

/*                 CALL VADD ( SUNST, SPOINT, SUNPOS ) */

/*                 CALL RECPGR ( 'MARS', SUNPOS, RE,    F, */
/*             .                 SUPGLN, SUPGLT, SUPGAL   ) */

/*                 SUPGLN = SUPGLN * DPR () */
/*                 SUPGLT = SUPGLT * DPR () */

/*        C */
/*        C        Convert the Sun's rectangular coordinates to */
/*        C        planetocentric radius, longitude, and latitude. */
/*        C        Convert radians to degrees. */
/*        C */
/*                 CALL RECLAT ( SUNPOS, SUPCRD, SUPCLN, SUPCLT ) */

/*                 SUPCLN = SUPCLN * DPR () */
/*                 SUPCLT = SUPCLT * DPR () */

/*        C */
/*        C        Write the results. */
/*        C */
/*                 WRITE(*,FM) ' ' */
/*                 WRITE(*,* ) 'Computation method = ', METHOD(I) */
/*                 WRITE(*,FM) ' ' */
/*                 WRITE(*,FM) '  Sub-solar point altitude            ' */
/*             .   // '(km) = ', SPGALT */
/*                 WRITE(*,FM) '  Sub-solar planetographic longitude ' */
/*             .   // '(deg) = ', SPGLON */
/*                 WRITE(*,FM) '  Sun''s planetographic longitude     ' */
/*             .   // '(deg) = ', SUPGLN */
/*                 WRITE(*,FM) '  Sub-solar planetographic latitude  ' */
/*             .   // '(deg) = ', SPGLAT */
/*                 WRITE(*,FM) '  Sun''s planetographic latitude      ' */
/*             .   // '(deg) = ', SUPGLT */
/*                 WRITE(*,FM) '  Sub-solar planetocentric longitude ' */
/*             .   // '(deg) = ', SPCLON */
/*                 WRITE(*,FM) '  Sun''s planetocentric longitude     ' */
/*             .   // '(deg) = ', SUPCLN */
/*                 WRITE(*,FM) '  Sub-solar planetocentric latitude  ' */
/*             .   // '(deg) = ', SPCLAT */
/*                 WRITE(*,FM) '  Sun''s planetocentric latitude      ' */
/*             .   // '(deg) = ', SUPCLT */
/*                 WRITE(*,FM) ' ' */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Computation method = Intercept/ellipsoid */

/*          Sub-solar point altitude            (km) =        0.000000000 */
/*          Sub-solar planetographic longitude (deg) =      175.810675508 */
/*          Sun's planetographic longitude     (deg) =      175.810675508 */
/*          Sub-solar planetographic latitude  (deg) =       23.668550281 */
/*          Sun's planetographic latitude      (deg) =       23.420823362 */
/*          Sub-solar planetocentric longitude (deg) =     -175.810675508 */
/*          Sun's planetocentric longitude     (deg) =     -175.810675508 */
/*          Sub-solar planetocentric latitude  (deg) =       23.420819936 */
/*          Sun's planetocentric latitude      (deg) =       23.420819936 */


/*         Computation method = Near point/ellipsoid */

/*          Sub-solar point altitude            (km) =       -0.000000000 */
/*          Sub-solar planetographic longitude (deg) =      175.810675408 */
/*          Sun's planetographic longitude     (deg) =      175.810675408 */
/*          Sub-solar planetographic latitude  (deg) =       23.420823362 */
/*          Sun's planetographic latitude      (deg) =       23.420823362 */
/*          Sub-solar planetocentric longitude (deg) =     -175.810675408 */
/*          Sun's planetocentric longitude     (deg) =     -175.810675408 */
/*          Sub-solar planetocentric latitude  (deg) =       23.175085578 */
/*          Sun's planetocentric latitude      (deg) =       23.420819936 */


/*         Computation method = Intercept/DSK/Unprioritized */

/*          Sub-solar point altitude            (km) =       -4.052254284 */
/*          Sub-solar planetographic longitude (deg) =      175.810675512 */
/*          Sun's planetographic longitude     (deg) =      175.810675512 */
/*          Sub-solar planetographic latitude  (deg) =       23.668848891 */
/*          Sun's planetographic latitude      (deg) =       23.420823362 */
/*          Sub-solar planetocentric longitude (deg) =     -175.810675512 */
/*          Sun's planetocentric longitude     (deg) =     -175.810675512 */
/*          Sub-solar planetocentric latitude  (deg) =       23.420819936 */
/*          Sun's planetocentric latitude      (deg) =       23.420819936 */


/*         Computation method = Nadir/DSK/Unprioritized */

/*          Sub-solar point altitude            (km) =       -4.022302438 */
/*          Sub-solar planetographic longitude (deg) =      175.810675412 */
/*          Sun's planetographic longitude     (deg) =      175.810675412 */
/*          Sub-solar planetographic latitude  (deg) =       23.420823362 */
/*          Sun's planetographic latitude      (deg) =       23.420823362 */
/*          Sub-solar planetocentric longitude (deg) =     -175.810675412 */
/*          Sun's planetocentric longitude     (deg) =     -175.810675412 */
/*          Sub-solar planetocentric latitude  (deg) =       23.174793924 */
/*          Sun's planetocentric latitude      (deg) =       23.420819936 */


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

/* -    SPICELIB Version 3.1.0, 01-NOV-2021 (JDR) (NJB) (EDW) */

/*        Bug fix: PRVCOR is no longer set to blank before */
/*        ABCORR is parsed. */

/*        Body radii accessed from kernel pool using ZZGFTREB. */

/*        Edited the header to comply with NAIF standard. */
/*        Changed code example and its output to comply with maximum */
/*        line length for header comments. */

/* -    SPICELIB Version 3.0.0, 04-APR-2017 (NJB) */

/*        Added FAILED tests. */

/*       14-JUL-2016 (NJB) */

/*        Now uses surface mapping tracking capability. */
/*        Updated header. */

/*       09-FEB-2015 (NJB) */

/*        Support for surface specification was added. */
/*        Header was updated to document DSK features. */

/*       24-DEC-2014 (NJB) */

/*        Updated to support DSK data. */

/* -    SPICELIB Version 2.0.0, 31-MAR-2014 (NJB) (SCK) (BVS) */

/*        Bug fix: stellar aberration is no longer applied to the */
/*        observer-to-estimated sub-solar point vector while solving for */
/*        the sub-solar point. This correction involved unnecessary code */
/*        but did not affect this routine's outputs. */

/*        Bug fix: FIRST is now set to .FALSE. at the completion of a */
/*        successful initialization pass. This does not affect the */
/*        routine's outputs but improves efficiency. */

/*        $Exceptions removed: the observer and target are now */
/*        permitted to coincide. */

/*        Upgrade: the algorithm for finding the apparent state of the */
/*        sun as seen from the estimated sub-solar point has been */
/*        improved. */

/*        Upgrade: this routine now uses ZZVALCOR rather than ZZPRSCOR, */
/*        simplifying the implementation. */

/*        The header example program was updated to reflect the new */
/*        method of computing the apparent sun location, and the set */
/*        of kernels referenced by the example meta-kernel were updated. */
/*        The display of the program's output was updated accordingly. */

/*        References to the new PXFRM2 routine were added, which changed */
/*        the Detailed Output section. */

/*        Updated to save the input body names and ZZBODTRN state */
/*        counters and to do name-ID conversions only if the counters */
/*        have changed. */

/*        Updated to save the input frame name and POOL state counter */
/*        and to do frame name-ID conversion only if the counter has */
/*        changed. */

/*        Updated to call LJUCRS instead of CMPRSS/UCASE. */

/* -    SPICELIB Version 1.1.0, 18-MAY-2010 (NJB) */

/*        Bug fix: calls to FAILED() have been added after */
/*        SPK calls, target radius lookup, near point */
/*        and surface intercept computations. */

/* -    SPICELIB Version 1.0.1, 17-MAR-2009 (NJB) */

/*        Typo correction: changed FIXFRM to FIXREF in header */
/*        documentation. Meta-kernel name suffix was changed to */
/*        ".tm" in header code example. */

/*        Typo correction in $Required_Reading, changed */
/*        FRAME to FRAMES. */

/* -    SPICELIB Version 1.0.0, 02-MAR-2008 (NJB) */

/* -& */
/* $ Index_Entries */

/*     find sub-solar point on target body */
/*     find nearest point to sun on target body */

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
    chkin_("SUBSLR", (ftnlen)6);

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
	    chkout_("SUBSLR", (ftnlen)6);
	    return 0;
	}

/*        Reject an aberration correction flag calling for transmission */
/*        corrections. */

	if (attblk[4]) {
	    setmsg_("Aberration correction flag # calls for transmission-sty"
		    "le corrections.", (ftnlen)70);
	    errch_("#", abcorr, (ftnlen)1, abcorr_len);
	    sigerr_("SPICE(NOTSUPPORTED)", (ftnlen)19);
	    chkout_("SUBSLR", (ftnlen)6);
	    return 0;
	}

/*        Set logical flags indicating the attributes of the requested */
/*        correction: */

/*           XMIT is .TRUE. when the correction is for transmitted */
/*           radiation. */

/*           USELT is .TRUE. when any type of light time correction */
/*           (normal or converged Newtonian) is specified. */

/*           USECN indicates converged Newtonian light time correction. */

/*        The above definitions are consistent with those used by */
/*        ZZPRSCOR. */

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
	chkout_("SUBSLR", (ftnlen)6);
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
	chkout_("SUBSLR", (ftnlen)6);
	return 0;
    }

/*     The target body may not be the sun. */

    if (trgcde == 10) {
	setmsg_("The target body is the sun; the sub-solar point is undefine"
		"d for this case.", (ftnlen)75);
	sigerr_("SPICE(INVALIDTARGET)", (ftnlen)20);
	chkout_("SUBSLR", (ftnlen)6);
	return 0;
    }

/*     Determine the attributes of the frame designated by FIXREF. */

    zznamfrm_(svctr3, svfref, &svrefc, fixref, &fixfid, (ftnlen)32, 
	    fixref_len);
    frinfo_(&fixfid, &fixctr, &fixcls, &fixcid, &fnd);
    if (failed_()) {
	chkout_("SUBSLR", (ftnlen)6);
	return 0;
    }
    if (! fnd) {
	setmsg_("Reference frame # is not recognized by the SPICE frame subs"
		"ystem. Possibly a required frame definition kernel has not b"
		"een loaded.", (ftnlen)130);
	errch_("#", fixref, (ftnlen)1, fixref_len);
	sigerr_("SPICE(NOFRAME)", (ftnlen)14);
	chkout_("SUBSLR", (ftnlen)6);
	return 0;
    }

/*     Make sure that FIXREF is centered at the target body's center. */

    if (fixctr != trgcde) {
	setmsg_("Reference frame # is not centered at the target body #. The"
		" ID code of the frame center is #.", (ftnlen)93);
	errch_("#", fixref, (ftnlen)1, fixref_len);
	errch_("#", target, (ftnlen)1, target_len);
	errint_("#", &fixctr, (ftnlen)1);
	sigerr_("SPICE(INVALIDFRAME)", (ftnlen)19);
	chkout_("SUBSLR", (ftnlen)6);
	return 0;
    }

/*     Check whether the surface name/ID mapping has been updated. */

    zzsrftrk_(svctr4, &surfup);

/*     If necessary, parse the method specification. PRVMTH */
/*     and the derived variables NEAR and SHAPE start out with */
/*     valid values. PRVMTH records the last valid value of */
/*     METHOD; NEAR and SHAPE are the corresponding variables. */

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
	    chkout_("SUBSLR", (ftnlen)6);
	    return 0;
	}
	if (s_cmp(subtyp, " ", (ftnlen)20, (ftnlen)1) == 0) {
	    setmsg_("Sub-solar point type is required but was not found in t"
		    "he method string #.", (ftnlen)74);
	    errch_("#", method, (ftnlen)1, method_len);
	    sigerr_("SPICE(INVALIDSUBTYPE)", (ftnlen)21);
	    chkout_("SUBSLR", (ftnlen)6);
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
	    chkout_("SUBSLR", (ftnlen)6);
	    return 0;
	}
	if (shape == 1) {

/*           Allow both "near point" and "nadir" expressions */
/*           the ellipsoid case, since these are equivalent. */

	    near__ = eqstr_(subtyp, "NEAR POINT", (ftnlen)20, (ftnlen)10) || 
		    eqstr_(subtyp, "NADIR", (ftnlen)20, (ftnlen)5);
	} else {

/*           "near point" is not supported for DSKs. */

	    near__ = eqstr_(subtyp, "NADIR", (ftnlen)20, (ftnlen)5);
	}
	if (! near__) {
	    if (! eqstr_(subtyp, "INTERCEPT", (ftnlen)20, (ftnlen)9)) {
		setmsg_("Invalid sub-solar point type <#> was found in the m"
			"ethod string #.", (ftnlen)66);
		errch_("#", subtyp, (ftnlen)1, (ftnlen)20);
		errch_("#", method, (ftnlen)1, method_len);
		sigerr_("SPICE(INVALIDSUBTYPE)", (ftnlen)21);
		chkout_("SUBSLR", (ftnlen)6);
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
		"fy a supported shape model and computation type. See the hea"
		"der of SUBSLR for details.", (ftnlen)145);
	errch_("#", method, (ftnlen)1, method_len);
	sigerr_("SPICE(INVALIDMETHOD)", (ftnlen)20);
	chkout_("SUBSLR", (ftnlen)6);
	return 0;
    }
    if (failed_()) {
	chkout_("SUBSLR", (ftnlen)6);
	return 0;
    }

/*     Get the sign S prefixing LT in the expression for TRGEPC. */
/*     When light time correction is not used, setting S = 0 */
/*     allows us to seamlessly set TRGEPC equal to ET. */

    if (uselt) {
	s = -1.;
    } else {
	s = 0.;
    }

/*     Determine the position of the observer in target body-fixed */
/*     coordinates. This is a first estimate. */

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
/*            that corrected vector in order to compute the sub-solar */
/*            point. */

    spkezp_(&trgcde, et, fixref, abcorr, &obscde, tpos, &lt, fixref_len, 
	    abcorr_len);
    if (failed_()) {
	chkout_("SUBSLR", (ftnlen)6);
	return 0;
    }

/*     Negate the target's position to obtain the position of the */
/*     observer relative to the target. */

    vminus_(tpos, obspos);

/*     Make a first estimate of the target epoch. */

    *trgepc = *et + s * lt;

/*     Find the sub-solar point given the target epoch, observer-target */
/*     position, and target body orientation we've already computed. If */
/*     we're not using light time correction, this is all we need do. */
/*     Otherwise, our result will give us an initial estimate of the */
/*     target epoch, which we'll then improve. */


/*     Get the radii of the target body from the kernel pool. */

    zzgftreb_(&trgcde, radii);
    if (failed_()) {
	chkout_("SUBSLR", (ftnlen)6);
	return 0;
    }

/*     Get the position of the Sun SPOS as seen from the target */
/*     in the target body-fixed frame at TRGEPC. */

    spkezp_(&c__10, trgepc, fixref, abcorr, &trgcde, spos, &slt, fixref_len, 
	    abcorr_len);
    if (failed_()) {
	chkout_("SUBSLR", (ftnlen)6);
	return 0;
    }

/*     Make a first estimate of the sub-solar point. The algorithm */
/*     we use depends on the sub-solar point definition. */

    if (near__) {

/*        Locate the nearest point to the Sun on the target's */
/*        reference ellipsoid. */

	nearpt_(spos, radii, &radii[1], &radii[2], spoint, &altsun);
	if (failed_()) {
	    chkout_("SUBSLR", (ftnlen)6);
	    return 0;
	}

/*        If the target is an ellipsoid, the NEARPT call above does the */
/*        trick. For DSKs, we define a ray emanating from the sun and */
/*        passing through the near point on the reference ellipsoid. The */
/*        closest ray-DSK surface intercept to the sun is the initial */
/*        estimate of the sub-solar point. */

	if (shape == 2) {

/*           Generate the ray direction; find the DSK intercept. */

	    vsub_(spoint, spos, dvec);
	    zzsbfxr_(&trgcde, &nsurf, srflst, trgepc, &fixfid, spos, dvec, 
		    spoint, &fnd);
	    if (failed_()) {
		chkout_("SUBSLR", (ftnlen)6);
		return 0;
	    }
	    if (! fnd) {
		setmsg_("No sub-solar point was found on the surface defined"
			" by DSK data. Observer is #; target is #. This probl"
			"em can occur for bodies having shapes not well model"
			"ed by ellipsoids. Consider using the \"Intercept: DSK"
			"\" computation method.", (ftnlen)228);
		errch_("#", obsrvr, (ftnlen)1, obsrvr_len);
		errch_("#", target, (ftnlen)1, target_len);
		sigerr_("SPICE(SUBPOINTNOTFOUND)", (ftnlen)23);
		chkout_("SUBSLR", (ftnlen)6);
		return 0;
	    }
	}
    } else {

/*        This is the case for the "intercept" sub-solar point */
/*        definition. */

/*        Generate the ray direction. */

	vminus_(spos, dvec);
	if (shape == 1) {

/*           Locate the surface intercept of the ray from the */
/*           sun to the target center. */

	    surfpt_(spos, dvec, radii, &radii[1], &radii[2], spoint, &fnd);
	    if (failed_()) {
		chkout_("SUBSLR", (ftnlen)6);
		return 0;
	    }
	    if (! fnd) {

/*              If there's no intercept, we have a numerical problem. */

		setmsg_("No intercept of sun-target ray was found.", (ftnlen)
			41);
		sigerr_("SPICE(DEGENERATECASE)", (ftnlen)21);
		chkout_("SUBSLR", (ftnlen)6);
		return 0;
	    }
	} else {

/*           Find the DSK intercept. */

	    zzsbfxr_(&trgcde, &nsurf, srflst, trgepc, &fixfid, spos, dvec, 
		    spoint, &fnd);
	    if (failed_()) {
		chkout_("SUBSLR", (ftnlen)6);
		return 0;
	    }
	    if (! fnd) {
		setmsg_("No sub-solar point was found on the surface defined"
			" by DSK data. Observer is #; target is #. This probl"
			"em can occur for a body having an irregular shape su"
			"ch that the origin of the body-fixed reference frame"
			" is outside of the body. A torus is an example of su"
			"ch a shape.", (ftnlen)270);
		errch_("#", obsrvr, (ftnlen)1, obsrvr_len);
		errch_("#", target, (ftnlen)1, target_len);
		sigerr_("SPICE(SUBPOINTNOTFOUND)", (ftnlen)23);
		chkout_("SUBSLR", (ftnlen)6);
		return 0;
	    }
	}
    }
    if (failed_()) {
	chkout_("SUBSLR", (ftnlen)6);
	return 0;
    }

/*     If we're not using light time and stellar aberration */
/*     corrections, we're almost done now. Note that we need only */
/*     check for use of light time corrections, because use of */
/*     stellar aberration corrections alone has been prevented by an */
/*     earlier check. */

    if (! uselt) {

/*        The TRGEPC value we'll return is simply the input time. The */
/*        previous call to SPKEZP call yielded the vector OBSPOS. SPOINT */
/*        was set immediately above. The only output left to compute is */
/*        SRFVEC. */

	*trgepc = *et;
	vsub_(spoint, obspos, srfvec);
	chkout_("SUBSLR", (ftnlen)6);
	return 0;
    }

/*     Compute the range from the observer to the sub-solar */
/*     point. We'll use this range for a light time estimate. */

    obsrng = vdist_(obspos, spoint);

/*     Compute the one-way light time and target epoch based on our */
/*     first computation of SPOINT. The coefficient S has been */
/*     set to give us the correct answer for each aberration */
/*     correction case. */

    lt = obsrng / clight_();
    *trgepc = *et + s * lt;

/*     We'll now make an improved sub-solar point estimate using the */
/*     previous estimate of the sub-solar point. The number of */
/*     iterations depends on the light time correction type. */

    if (usecn) {
	nitr = 10;
    } else {
	nitr = 1;
    }

/*     Get the J2000-relative state of the observer relative to */
/*     the solar system barycenter at ET. */

    spkssb_(&obscde, et, "J2000", ssbost, (ftnlen)5);
    if (failed_()) {
	chkout_("SUBSLR", (ftnlen)6);
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
	    chkout_("SUBSLR", (ftnlen)6);
	    return 0;
	}

/*        Find the position of the observer relative to the target. */
/*        Convert this vector from the J2000 frame to the target */
/*        frame at TRGEPC. */

	vsub_(ssbost, ssbtst, j2pos);
	pxform_("J2000", fixref, trgepc, xform, (ftnlen)5, fixref_len);
	if (failed_()) {
	    chkout_("SUBSLR", (ftnlen)6);
	    return 0;
	}
	mxv_(xform, j2pos, obspos);

/*        Note: if we're using stellar aberration correction, we do not */
/*        apply the stellar aberration correction of the estimated */
/*        sub-solar point as seen by the observer to the next estimate */
/*        of the sun-solar point. The location of this point depends on */
/*        the illumination of the target, which is a function of the */
/*        observer-surface point light time. This is the only way in */
/*        which the observer plays a role in determining the sub-solar */
/*        point. */

/*        Stellar aberration of the sun's position relative to the */
/*        sub-solar point *is* used. */

/*        First find the apparent position of the sun as seen from the */
/*        estimated sub-solar point. */

	spkcpo_("SUN", trgepc, fixref, "OBSERVER", abcorr, spoint, target, 
		fixref, sunst, &slt, (ftnlen)3, fixref_len, (ftnlen)8, 
		abcorr_len, target_len, fixref_len);

/*        Create the target-center to sun vector. */

	vadd_(sunst, spoint, spos);
	if (failed_()) {
	    chkout_("SUBSLR", (ftnlen)6);
	    return 0;
	}

/*        Find the sub-solar point using the current estimated */
/*        geometry. */

	if (near__) {

/*           Locate the nearest point to the sun on the target's */
/*           reference ellipsoid. */

	    nearpt_(spos, radii, &radii[1], &radii[2], spoint, &altsun);
	    if (failed_()) {
		chkout_("SUBSLR", (ftnlen)6);
		return 0;
	    }

/*           If the target is an ellipsoid, the NEARPT call above does */
/*           the trick. For DSKs, we define a ray emanating from the sun */
/*           and passing through the near point on the reference */
/*           ellipsoid. The closest ray-DSK surface intercept to the sun */
/*           is the initial estimate of the sub-solar point. */

	    if (shape == 2) {

/*              Locate the surface intercept of the ray from the */
/*              Sun to the target center. */

		vsub_(spoint, spos, dvec);
		zzsbfxr_(&trgcde, &nsurf, srflst, trgepc, &fixfid, spos, dvec,
			 spoint, &fnd);
		if (failed_()) {
		    chkout_("SUBSLR", (ftnlen)6);
		    return 0;
		}
		if (! fnd) {
		    setmsg_("No sub-solar point was found on the surface def"
			    "ined by DSK data. Observer is #; target is #. Th"
			    "is problem can occur for bodies having shapes no"
			    "t well modeled by ellipsoids. Consider using the "
			    "\"Intercept: DSK\" computation method.", (ftnlen)
			    228);
		    errch_("#", obsrvr, (ftnlen)1, obsrvr_len);
		    errch_("#", target, (ftnlen)1, target_len);
		    sigerr_("SPICE(SUBPOINTNOTFOUND)", (ftnlen)23);
		    chkout_("SUBSLR", (ftnlen)6);
		    return 0;
		}
	    }
	} else {
/*           This is the "intercept" case. */

/*           Generate the ray direction. */

	    vminus_(spos, dvec);
	    if (shape == 1) {

/*              Locate the surface intercept of the ray from the */
/*              sun to the target center. */

		surfpt_(spos, dvec, radii, &radii[1], &radii[2], spoint, &fnd)
			;
		if (failed_()) {
		    chkout_("SUBSLR", (ftnlen)6);
		    return 0;
		}
		if (! fnd) {

/*                 If there's no intercept, we have a numerical problem. */

		    setmsg_("No intercept of sun-target ray was found.", (
			    ftnlen)41);
		    sigerr_("SPICE(DEGENERATECASE)", (ftnlen)21);
		    chkout_("SUBSLR", (ftnlen)6);
		    return 0;
		}
	    } else {

/*              Find the DSK intercept. */

		zzsbfxr_(&trgcde, &nsurf, srflst, trgepc, &fixfid, spos, dvec,
			 spoint, &fnd);
		if (failed_()) {
		    chkout_("SUBSLR", (ftnlen)6);
		    return 0;
		}
		if (! fnd) {
		    setmsg_("No sub-solar point was found on the surface def"
			    "ined by DSK data. Observer is #; target is #. Th"
			    "is problem can occur for a body having an irregu"
			    "lar shape such that the origin of the body-fixed"
			    " reference frame is outside of the body. A torus"
			    " is an example of such a shape.", (ftnlen)270);
		    errch_("#", obsrvr, (ftnlen)1, obsrvr_len);
		    errch_("#", target, (ftnlen)1, target_len);
		    sigerr_("SPICE(SUBPOINTNOTFOUND)", (ftnlen)23);
		    chkout_("SUBSLR", (ftnlen)6);
		    return 0;
		}
	    }
	}

/*        Update the observer to sub-solar point range. */

	obsrng = vdist_(obspos, spoint);

/*        Compute a new light time estimate and new target epoch. */

	lt = obsrng / clight_();
	*trgepc = *et + s * lt;

/*        At this point, we have new estimates of the sub-solar point */
/*        SPOINT, the observer altitude OBSRNG, the target epoch */
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
	spkssb_(&trgcde, trgepc, "J2000", ssbtst, (ftnlen)5);
	if (failed_()) {
	    chkout_("SUBSLR", (ftnlen)6);
	    return 0;
	}

/*        Find the position of the observer relative to the target. */
/*        Convert this vector from the J2000 frame to the target */
/*        frame at TRGEPC. */

	vsub_(ssbost, ssbtst, j2pos);
	pxform_("J2000", fixref, trgepc, xform, (ftnlen)5, fixref_len);
	if (failed_()) {
	    chkout_("SUBSLR", (ftnlen)6);
	    return 0;
	}
	mxv_(xform, j2pos, obspos);
    }

/*     SPOINT and TRGEPC have been set. Compute SRFVEC, using */
/*     both light time and stellar aberration corrections if */
/*     these have been requested by the caller. Note that */
/*     the position of the target body was computed without */
/*     stellar aberration corrections, so we may not be able */
/*     to use it for this computation. */

    if (usestl) {
	spkcpt_(spoint, target, fixref, et, fixref, "TARGET", abcorr, obsrvr, 
		sslrst, &sslrlt, target_len, fixref_len, fixref_len, (ftnlen)
		6, abcorr_len, obsrvr_len);
	if (failed_()) {
	    chkout_("SUBSLR", (ftnlen)6);
	    return 0;
	}
	vequ_(sslrst, srfvec);
    } else {
	vsub_(spoint, obspos, srfvec);
    }
    chkout_("SUBSLR", (ftnlen)6);
    return 0;
} /* subslr_ */

