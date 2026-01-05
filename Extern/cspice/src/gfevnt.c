/* gfevnt.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__8 = 8;
static integer c__10 = 10;
static integer c__7 = 7;

/* $Procedure GFEVNT ( GF, Geometric event finder ) */
/* Subroutine */ int gfevnt_(U_fp udstep, U_fp udrefn, char *gquant, integer *
	qnpars, char *qpnams, char *qcpars, doublereal *qdpars, integer *
	qipars, logical *qlpars, char *op, doublereal *refval, doublereal *
	tol, doublereal *adjust, doublereal *cnfine, logical *rpt, U_fp 
	udrepi, U_fp udrepu, U_fp udrepf, integer *mw, integer *nw, 
	doublereal *work, logical *bail, L_fp udbail, doublereal *result, 
	ftnlen gquant_len, ftnlen qpnams_len, ftnlen qcpars_len, ftnlen 
	op_len)
{
    /* Initialized data */

    static char dref[80] = "                                                "
	    "                                ";
    static logical first = TRUE_;
    static char qnames[80*8] = "ANGULAR SEPARATION                          "
	    "                                    " "DISTANCE                 "
	    "                                                       " "COORDI"
	    "NATE                                                            "
	    "          " "RANGE RATE                                         "
	    "                             " "PHASE ANGLE                     "
	    "                                                " "ILLUMINATION "
	    "ANGLE                                                           "
	    "   " "                                                          "
	    "                      " "                                       "
	    "                                         ";
    static char cnames[80*7] = ">                                           "
	    "                                    " "=                        "
	    "                                                       " "<     "
	    "                                                                "
	    "          " "ABSMAX                                             "
	    "                             " "ABSMIN                          "
	    "                                                " "LOCMAX       "
	    "                                                                "
	    "   " "LOCMIN                                                    "
	    "                      ";
    static char qpars[80*10*8] = "TARGET1                                   "
	    "                                      " "FRAME1                 "
	    "                                                         " "SHAP"
	    "E1                                                              "
	    "            " "TARGET2                                          "
	    "                               " "FRAME2                        "
	    "                                                  " "SHAPE2     "
	    "                                                                "
	    "     " "OBSERVER                                                "
	    "                        " "ABCORR                               "
	    "                                           " "                  "
	    "                                                              " 
	    "                                                               "
	    "                 " "TARGET                                      "
	    "                                    " "OBSERVER                 "
	    "                                                       " "ABCORR"
	    "                                                                "
	    "          " "                                                   "
	    "                             " "                                "
	    "                                                " "             "
	    "                                                                "
	    "   " "                                                          "
	    "                      " "                                       "
	    "                                         " "                    "
	    "                                                            " 
	    "                                                               "
	    "                 " "TARGET                                      "
	    "                                    " "OBSERVER                 "
	    "                                                       " "ABCORR"
	    "                                                                "
	    "          " "COORDINATE SYSTEM                                  "
	    "                             " "COORDINATE                      "
	    "                                                " "REFERENCE FRA"
	    "ME                                                              "
	    "   " "VECTOR DEFINITION                                         "
	    "                      " "METHOD                                 "
	    "                                         " "DVEC                "
	    "                                                            " 
	    "DREF                                                           "
	    "                 " "TARGET                                      "
	    "                                    " "OBSERVER                 "
	    "                                                       " "ABCORR"
	    "                                                                "
	    "          " "                                                   "
	    "                             " "                                "
	    "                                                " "             "
	    "                                                                "
	    "   " "                                                          "
	    "                      " "                                       "
	    "                                         " "                    "
	    "                                                            " 
	    "                                                               "
	    "                 " "TARGET                                      "
	    "                                    " "OBSERVER                 "
	    "                                                       " "ILLUM "
	    "                                                                "
	    "          " "ABCORR                                             "
	    "                             " "                                "
	    "                                                " "             "
	    "                                                                "
	    "   " "                                                          "
	    "                      " "                                       "
	    "                                         " "                    "
	    "                                                            " 
	    "                                                               "
	    "                 " "TARGET                                      "
	    "                                    " "ILLUM                    "
	    "                                                       " "OBSERV"
	    "ER                                                              "
	    "          " "ABCORR                                             "
	    "                             " "REFERENCE FRAME                 "
	    "                                                " "ANGTYP       "
	    "                                                                "
	    "   " "METHOD                                                    "
	    "                      " "SPOINT                                 "
	    "                                         " "                    "
	    "                                                            " 
	    "                                                               "
	    "                 " "TARGET1                                     "
	    "                                    " "TARGET2                  "
	    "                                                       " "OBSERV"
	    "ER                                                              "
	    "          " "ABCORR                                             "
	    "                             " "REFERENCE FRAME                 "
	    "                                                " "             "
	    "                                                                "
	    "   " "                                                          "
	    "                      " "                                       "
	    "                                         " "                    "
	    "                                                            " 
	    "                                                               "
	    "                 " "TARGET                                      "
	    "                                    " "OBSERVER                 "
	    "                                                       " "ABCORR"
	    "                                                                "
	    "          " "REFERENCE FRAME                                    "
	    "                             " "                                "
	    "                                                " "             "
	    "                                                                "
	    "   " "                                                          "
	    "                      " "                                       "
	    "                                         " "                    "
	    "                                                            " 
	    "                                                               "
	    "                 ";

    /* System generated locals */
    integer work_dim1, work_offset, i__1, i__2, i__3;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_rnge(char *, integer, char *, integer), s_cmp(char *, char *, 
	    ftnlen, ftnlen);

    /* Local variables */
    doublereal dvec[3];
    extern /* Subroutine */ int vequ_(doublereal *, doublereal *);
    extern /* Subroutine */ int zzgfdidc_(), zzgfpadc_(), zzgfildc_();
    extern /* Subroutine */ int zzgfdiin_(char *, char *, char *, ftnlen, 
	    ftnlen, ftnlen);
    extern /* Subroutine */ int zzgfdigq_();
    extern /* Subroutine */ int zzgfpain_(char *, char *, char *, char *, 
	    ftnlen, ftnlen, ftnlen, ftnlen);
    extern /* Subroutine */ int zzgfpagq_(), zzgfspdc_(), zzgfrrdc_();
    extern /* Subroutine */ int zzgfilin_(char *, char *, char *, char *, 
	    char *, char *, char *, doublereal *, ftnlen, ftnlen, ftnlen, 
	    ftnlen, ftnlen, ftnlen, ftnlen);
    extern /* Subroutine */ int zzgfilgq_();
    extern /* Subroutine */ int zzgfcslv_(char *, char *, char *, char *, 
	    char *, char *, char *, doublereal *, char *, char *, char *, 
	    doublereal *, doublereal *, doublereal *, U_fp, U_fp, logical *, 
	    U_fp, U_fp, U_fp, logical *, L_fp, integer *, integer *, 
	    doublereal *, doublereal *, doublereal *, ftnlen, ftnlen, ftnlen, 
	    ftnlen, ftnlen, ftnlen, ftnlen, ftnlen, ftnlen, ftnlen);
    extern /* Subroutine */ int zzgfudlt_();
    extern /* Subroutine */ int zzgfspin_(char *, char *, char *, char *, 
	    char *, ftnlen, ftnlen, ftnlen, ftnlen, ftnlen);
    extern /* Subroutine */ int zzgfspgq_(), zzgfrrgq_();
    extern /* Subroutine */ int zzgfrelx_(U_fp, U_fp, U_fp, U_fp, U_fp, char *
	    , doublereal *, doublereal *, doublereal *, doublereal *, integer 
	    *, integer *, doublereal *, logical *, U_fp, U_fp, U_fp, char *, 
	    char *, logical *, L_fp, doublereal *, ftnlen, ftnlen, ftnlen), 
	    zzgfrrin_(char *, char *, char *, doublereal *, ftnlen, ftnlen, 
	    ftnlen);
    integer i__;
    char frame[80*2];
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    char shape[80*2];
    extern /* Subroutine */ int ucase_(char *, char *, ftnlen, ftnlen), 
	    errch_(char *, char *, ftnlen, ftnlen);
    char cpars[80*10];
    extern /* Subroutine */ int repmi_(char *, char *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen);
    char illum[80], quant[80];
    integer npass;
    extern /* Subroutine */ int ljust_(char *, char *, ftnlen, ftnlen);
    integer qtnum;
    char of[80*2];
    doublereal dt;
    char vecdef[80];
    extern integer isrchc_(char *, integer *, char *, ftnlen, ftnlen);
    extern logical return_(void);
    char abcorr[80], angtyp[80], cornam[80], corsys[80], method[80], obsrvr[
	    80], pnames[80*10], target[80], rptpre[55*2];
    static char srcpre[55*2*8], srcsuf[13*2*8];
    logical localx;
    char ref[80];
    integer loc;
    logical noadjx;
    doublereal spoint[3];
    extern /* Subroutine */ int setmsg_(char *, ftnlen), sigerr_(char *, 
	    ftnlen), chkout_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen);
    char uop[6];

/* $ Abstract */

/*     Determine time intervals when a specified geometric quantity */
/*     satisfies a specified mathematical condition. */

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
/*     SPK */
/*     TIME */
/*     NAIF_IDS */
/*     FRAMES */

/* $ Keywords */

/*     EPHEMERIS */
/*     EVENT */
/*     GEOMETRY */
/*     SEARCH */

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

/*     SPICE private include file intended solely for the support of */
/*     SPICE routines. Users should not include this routine in their */
/*     source code due to the volatile nature of this file. */

/*     This file contains private, global parameter declarations */
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
/*     E.D. Wright       (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 17-FEB-2009 (NJB) (EDW) */

/* -& */

/*     The set of supported coordinate systems */

/*        System          Coordinates */
/*        ----------      ----------- */
/*        Rectangular     X, Y, Z */
/*        Latitudinal     Radius, Longitude, Latitude */
/*        Spherical       Radius, Colatitude, Longitude */
/*        RA/Dec          Range, Right Ascension, Declination */
/*        Cylindrical     Radius, Longitude, Z */
/*        Geodetic        Longitude, Latitude, Altitude */
/*        Planetographic  Longitude, Latitude, Altitude */

/*     Below we declare parameters for naming coordinate systems. */
/*     User inputs naming coordinate systems must match these */
/*     when compared using EQSTR. That is, user inputs must */
/*     match after being left justified, converted to upper case, */
/*     and having all embedded blanks removed. */


/*     Below we declare names for coordinates. Again, user */
/*     inputs naming coordinates must match these when */
/*     compared using EQSTR. */


/*     Note that the RA parameter value below matches */

/*        'RIGHT ASCENSION' */

/*     when extra blanks are compressed out of the above value. */


/*     Parameters specifying types of vector definitions */
/*     used for GF coordinate searches: */

/*     All string parameter values are left justified, upper */
/*     case, with extra blanks compressed out. */

/*     POSDEF indicates the vector is defined by the */
/*     position of a target relative to an observer. */


/*     SOBDEF indicates the vector points from the center */
/*     of a target body to the sub-observer point on */
/*     that body, for a given observer and target. */


/*     SOBDEF indicates the vector points from the center */
/*     of a target body to the surface intercept point on */
/*     that body, for a given observer, ray, and target. */


/*     Number of workspace windows used by ZZGFREL: */


/*     Number of additional workspace windows used by ZZGFLONG: */


/*     Index of "existence window" used by ZZGFCSLV: */


/*     Progress report parameters: */

/*     MXBEGM, */
/*     MXENDM    are, respectively, the maximum lengths of the progress */
/*               report message prefix and suffix. */

/*     Note: the sum of these lengths, plus the length of the */
/*     "percent complete" substring, should not be long enough */
/*     to cause wrap-around on any platform's terminal window. */


/*     Total progress report message length upper bound: */


/*     End of file zzgf.inc. */

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
/*     MAXPAR     P   Maximum number of parameters required to define */
/*                    any quantity. */
/*     CNVTOL     P   Default convergence tolerance. */
/*     UDSTEP     I   Name of the routine that computes and returns a */
/*                    time step. */
/*     UDREFN     I   Name of the routine that computes a refined time. */
/*     GQUANT     I   Type of geometric quantity. */
/*     QNPARS     I   Number of quantity definition parameters. */
/*     QPNAMS     I   Names of quantity definition parameters. */
/*     QCPARS     I   Array of character quantity definition parameters. */
/*     QDPARS     I   Array of double precision quantity definition */
/*                    parameters. */
/*     QIPARS     I   Array of integer quantity definition parameters. */
/*     QLPARS     I   Array of logical quantity definition parameters. */
/*     OP         I   Operator that either looks for an extreme value */
/*                    (max, min, local, absolute) or compares the */
/*                    geometric quantity value and a number. */
/*     REFVAL     I   Reference value. */
/*     TOL        I   Convergence tolerance in seconds */
/*     ADJUST     I   Absolute extremum adjustment value. */
/*     CNFINE     I   SPICE window to which the search is restricted. */
/*     RPT        I   Progress reporter on (.TRUE.) or off (.FALSE.). */
/*     UDREPI     I   Function that initializes progress reporting. */
/*     UDREPU     I   Function that updates the progress report. */
/*     UDREPF     I   Function that finalizes progress reporting. */
/*     MW         I   Size of workspace windows. */
/*     NW         I   The number of workspace windows needed for the */
/*                    search. */
/*     WORK       O   Array containing workspace windows. */
/*     BAIL       I   Logical indicating program interrupt monitoring. */
/*     UDBAIL     I   Name of a routine that signals a program interrupt. */
/*     RESULT    I-O  SPICE window containing results. */

/* $ Detailed_Input */

/*     UDSTEP   is the name of the user specified routine that computes a */
/*              time step in an attempt to find a transition of the state */
/*              of the specified coordinate. In the context of this */
/*              routine's algorithm, a "state transition" occurs where */
/*              the geometric state changes from being in the desired */
/*              geometric condition event to not, or vice versa. */

/*              This routine relies on UDSTEP returning step sizes small */
/*              enough so that state transitions within the confinement */
/*              window are not overlooked. There must never be two roots */
/*              A and B separated by less than STEP, where STEP is the */
/*              minimum step size returned by UDSTEP for any value of ET */
/*              in the interval [A, B]. */

/*              The calling sequence for UDSTEP is: */

/*                 CALL UDSTEP ( ET, STEP ) */

/*              where: */

/*                 ET      is the input start time from which the */
/*                         algorithm is to search forward for a state */
/*                         transition. ET is expressed as seconds past */
/*                         J2000 TDB. */

/*                 STEP    is the output step size. STEP indicates how */
/*                         far to advance ET so that ET and ET+STEP may */
/*                         bracket a state transition and definitely do */
/*                         not bracket more than one state transition. */
/*                         Units are TDB seconds. */

/*              If a constant step size is desired, the SPICELIB routine */

/*                 GFSTEP */

/*              may be used as the step size function. This is the */
/*              default option. If GFSTEP is used, the step size must be */
/*              set by calling */

/*                 CALL GFSSTP ( STEP ) */

/*              prior to calling this routine. */

/*     UDREFN   is the name of the user specified routine that computes a */
/*              refinement in the times that bracket a transition point. */
/*              In other words, once a pair of times have been detected */
/*              such that the system is in different states at each of */
/*              the two times, UDREFN selects an intermediate time which */
/*              should be closer to the transition state than one of the */
/*              two known times. */

/*              The calling sequence for UDREFN is: */

/*                 CALL UDREFN ( T1, T2, S1, S2, T ) */

/*              where the inputs are: */

/*                 T1    is a time when the system is in state S1. T1 is */
/*                       expressed as seconds past J2000 TDB. */

/*                 T2    is a time when the system is in state S2. T2 is */
/*                       expressed as seconds past J2000 TDB. T2 is */
/*                       assumed to be larger than T1. */

/*                 S1    is the state of the system at time T1. S1 is a */
/*                       LOGICAL value. */

/*                 S2    is the state of the system at time T2. S2 is a */
/*                       LOGICAL value. */

/*              UDREFN may use or ignore the S1 and S2 values. */

/*              The output is: */

/*                 T     is next time to check for a state transition. */
/*                       T has value between T1 and T2. T is expressed */
/*                       as seconds past J2000 TDB. */

/*              If a simple bisection method is desired, the SPICELIB */
/*              routine */

/*                 GFREFN */

/*              may be used as the refinement function. This is the */
/*              default option. */

/*     GQUANT   is a string containing the name of a geometric quantity. */
/*              The times when this quantity satisfies a condition */
/*              specified by the arguments OP and ADJUST (described */
/*              below) are to be found. */

/*              Each quantity is specified by the quantity name given in */
/*              argument GQUANT, and by a set of parameters specified by */
/*              the arguments */

/*                 QNPARS */
/*                 QPNAMS */
/*                 QCPARS */
/*                 QDPARS */
/*                 QIPARS */
/*                 QLPARS */

/*              For each quantity listed here, we also show how to set up */
/*              these input arguments to define the quantity. See the */
/*              detailed discussion of these arguments below for further */
/*              information. */

/*              GQUANT may be any of the strings: */

/*                 'ANGULAR SEPARATION' */
/*                 'COORDINATE' */
/*                 'DISTANCE' */
/*                 'ILLUMINATION ANGLE' */
/*                 'PHASE ANGLE' */
/*                 'RANGE RATE' */

/*              GQUANT strings are case insensitive. Values, meanings, */
/*              and associated parameters are discussed below. */

/*              The aberration correction parameter indicates the */
/*              aberration corrections to be applied to the state of the */
/*              target body to account for one-way light time and stellar */
/*              aberration. If relevant, it applies to the rotation of */
/*              the target body as well. */

/*              Supported aberration correction options for observation */
/*              (case where radiation is received by observer at ET) are: */

/*                 'NONE'          No correction. */
/*                 'LT'            Light time only. */
/*                 'LT+S'          Light time and stellar aberration. */
/*                 'CN'            Converged Newtonian (CN) light time. */
/*                 'CN+S'          CN light time and stellar aberration. */

/*              Supported aberration correction options for transmission */
/*              (case where radiation is emitted from observer at ET) */
/*              are: */

/*                 'XLT'           Light time only. */
/*                 'XLT+S'         Light time and stellar aberration. */
/*                 'XCN'           Converged Newtonian (CN) light time. */
/*                 'XCN+S'         CN light time and stellar aberration. */

/*              For detailed information, see the geometry finder */
/*              required reading, gf.req. */

/*              Case, leading and trailing blanks are not significant in */
/*              aberration correction parameter strings. */


/*              ANGULAR SEPARATION */

/*                 is the apparent angular separation of two target */
/*                 bodies as seen from an observing body. */

/*                 Quantity Parameters: */

/*                    QNPARS    = 8 */
/*                    QPNAMS(1) = 'TARGET1' */
/*                    QPNAMS(2) = 'FRAME1' */
/*                    QPNAMS(3) = 'SHAPE1' */
/*                    QPNAMS(4) = 'TARGET2' */
/*                    QPNAMS(5) = 'FRAME2' */
/*                    QPNAMS(6) = 'SHAPE2' */
/*                    QPNAMS(7) = 'OBSERVER' */
/*                    QPNAMS(8) = 'ABCORR' */

/*                    QCPARS(1) = <name of first target> */
/*                    QCPARS(2) = <name of body-fixed frame */
/*                                          of first target> */
/*                    QCPARS(3) = <shape of first target> */
/*                    QCPARS(4) = <name of second target> */
/*                    QCPARS(5) = <name of body-fixed frame */
/*                                         of second target> */
/*                    QCPARS(6) = <shape of second target> */
/*                    QCPARS(7) = <name of observer> */
/*                    QCPARS(8) = <aberration correction> */

/*                 The target shape model specifiers may be set to either */
/*                 of the values */

/*                    'POINT' */
/*                    'SPHERE' */

/*                 The shape models for the two bodies need not match. */

/*                 Spherical models have radii equal to the longest */
/*                 equatorial radius of the PCK-based tri-axial */
/*                 ellipsoids used to model the respective bodies. When */
/*                 both target bodies are modeled as spheres, the angular */
/*                 separation between the bodies is the angle between the */
/*                 closest points on the limbs of the spheres, as viewed */
/*                 from the vantage point of the observer. If the limbs */
/*                 overlap, the angular separation is negative. */

/*                 (In this case, the angular separation is the angle */
/*                 between the centers of the spheres minus the sum of */
/*                 the apparent angular radii of the spheres.) */


/*              COORDINATE */

/*                 is a coordinate of a specified vector in a specified */
/*                 reference frame and coordinate system. For example, a */
/*                 coordinate can be the Z component of the earth-sun */
/*                 vector in the J2000 reference frame, or the latitude */
/*                 of the nearest point on Mars to an orbiting */
/*                 spacecraft, expressed relative to the IAU_MARS */
/*                 reference frame. */

/*                 The method by which the vector is defined is indicated */
/*                 by the */

/*                    'VECTOR DEFINITION' */

/*                 parameter. Allowed values and meanings of this */
/*                 parameter are: */

/*                    'POSITION' */

/*                       The vector is defined by the position of a */
/*                       target relative to an observer. */

/*                    'SUB-OBSERVER POINT' */

/*                       The vector is the sub-observer point on a */
/*                       specified target body. */

/*                    'SURFACE INTERCEPT POINT' */

/*                       The vector is defined as the intercept point of */
/*                       a vector from the observer to the target body. */

/*                 Some vector definitions, such as the sub-observer */
/*                 point, may be specified by a variety of methods, so a */
/*                 parameter is provided to select the computation */
/*                 method. The computation method parameter name is */

/*                    'METHOD' */

/*                 If the vector definition is */

/*                    'POSITION' */

/*                 the 'METHOD' parameter must be set to blank: */

/*                    ' ' */

/*                 If the vector definition is */

/*                    'SUB-OBSERVER POINT' */

/*                 the 'METHOD' parameter must be set to either: */

/*                    'Near point: ellipsoid' */
/*                    'Intercept: ellipsoid' */

/*                 If the vector definition is */

/*                    'SURFACE INTERCEPT POINT' */

/*                 the 'METHOD' parameter must be set to: */

/*                    'Ellipsoid' */

/*                       The intercept computation uses a triaxial */
/*                       ellipsoid to model the surface of the target */
/*                       body. The ellipsoid's radii must be available in */
/*                       the kernel pool. */

/*                 The supported coordinate systems and coordinate names: */

/*                    Coordinate System  Coordinates        Range */
/*                    -----------------  -----------------  ------------ */

/*                    'RECTANGULAR'      'X' */
/*                                       'Y' */
/*                                       'Z' */

/*                    'LATITUDINAL'      'RADIUS' */
/*                                       'LONGITUDE'        (-Pi,Pi] */
/*                                       'LATITUDE'         [-Pi/2,Pi/2] */

/*                    'RA/DEC'           'RANGE' */
/*                                       'RIGHT ASCENSION'  [0,2Pi) */
/*                                       'DECLINATION'      [-Pi/2,Pi/2] */

/*                    'SPHERICAL'        'RADIUS' */
/*                                       'COLATITUDE'       [0,Pi] */
/*                                       'LONGITUDE'        (-Pi,Pi] */

/*                    'CYLINDRICAL'      'RADIUS' */
/*                                       'LONGITUDE'        [0,2Pi) */
/*                                       'Z' */

/*                    'GEODETIC'         'LONGITUDE'        (-Pi,Pi] */
/*                                       'LATITUDE'         [-Pi/2,Pi/2] */
/*                                       'ALTITUDE' */

/*                    'PLANETOGRAPHIC'   'LONGITUDE'        [0,2Pi) */
/*                                       'LATITUDE'         [-Pi/2,Pi/2] */
/*                                       'ALTITUDE' */

/*                 When geodetic coordinates are selected, the radii used */
/*                 are those of the central body associated with the */
/*                 reference frame. For example, if IAU_MARS is the */
/*                 reference frame, then geodetic coordinates are */
/*                 calculated using the radii of Mars taken from a SPICE */
/*                 planetary constants kernel. One cannot ask for */
/*                 geodetic coordinates for a frame which doesn't have an */
/*                 extended body as its center. */

/*                 Reference frame names must be recognized by the SPICE */
/*                 frame subsystem. */

/*                 Quantity Parameters: */

/*                    QNPARS     = 10 */
/*                    QPNAMS(1)  = 'TARGET' */
/*                    QPNAMS(2)  = 'OBSERVER' */
/*                    QPNAMS(3)  = 'ABCORR' */
/*                    QPNAMS(4)  = 'COORDINATE SYSTEM' */
/*                    QPNAMS(5)  = 'COORDINATE' */
/*                    QPNAMS(6)  = 'REFERENCE FRAME' */
/*                    QPNAMS(7)  = 'VECTOR DEFINITION' */
/*                    QPNAMS(8)  = 'METHOD' */
/*                    QPNAMS(9)  = 'DREF' */
/*                    QPNAMS(10) = 'DVEC' */

/*                 Only 'SURFACE INTERCEPT POINT' searches make use of */
/*                 the 'DREF' and 'DVEC' parameters. */

/*                    QCPARS(1) = <name of target> */
/*                    QCPARS(2) = <name of observer> */
/*                    QCPARS(3) = <aberration correction> */
/*                    QCPARS(4) = <coordinate system name> */
/*                    QCPARS(5) = <coordinate name> */
/*                    QCPARS(6) = <target reference frame name> */
/*                    QCPARS(7) = <vector definition> */
/*                    QCPARS(8) = <computation method> */
/*                    QCPARS(9) = <reference frame of DVEC pointing */
/*                                        vector, defined in QDPARS> */

/*                    QDPARS(1) = <DVEC pointing vector x component */
/*                                                      from observer> */
/*                    QDPARS(2) = <DVEC pointing vector y component */
/*                                                      from observer> */
/*                    QDPARS(3) = <DVEC pointing vector z component */
/*                                                      from observer> */

/*              DISTANCE */

/*                 is the apparent distance between a target body and an */
/*                 observing body. Distances are always measured between */
/*                 centers of mass. */

/*                 Quantity Parameters: */

/*                    QNPARS    = 3 */
/*                    QPNAMS(1) = 'TARGET' */
/*                    QPNAMS(2) = 'OBSERVER' */
/*                    QPNAMS(3) = 'ABCORR' */

/*                    QCPARS(1) = <name of target> */
/*                    QCPARS(2) = <name of observer> */
/*                    QCPARS(3) = <aberration correction> */


/*              ILLUMINATION ANGLE */

/*                 is any of the illumination angles */

/*                    emission */
/*                    phase */
/*                    solar incidence */

/*                 defined at a surface point on a target body. These */
/*                 angles are defined as in the SPICELIB routine ILUMIN. */

/*                 Quantity Parameters: */

/*                    QNPARS    = 8 */
/*                    QPNAMS(1) = 'TARGET' */
/*                    QPNAMS(2) = 'ILLUM' */
/*                    QPNAMS(3) = 'OBSERVER' */
/*                    QPNAMS(4) = 'ABCORR' */
/*                    QPNAMS(5) = 'FRAME' */
/*                    QPNAMS(6) = 'ANGTYP' */
/*                    QPNAMS(7) = 'METHOD' */
/*                    QPNAMS(8) = 'SPOINT' */

/*                    QCPARS(1) =  <name of target> */
/*                    QCPARS(1) =  <name of illumination source> */
/*                    QCPARS(3) =  <name of observer> */
/*                    QCPARS(4) =  <aberration correction> */
/*                    QCPARS(5) =  <target body-fixed frame> */
/*                    QCPARS(6) =  <type of illumination angle> */
/*                    QCPARS(7) =  <computation method> */

/*                 The surface point is specified using rectangular */
/*                 coordinates in the specified body-fixed frame. */

/*                    QDPARS(1) =  <X coordinate of surface point> */
/*                    QDPARS(2) =  <Y coordinate of surface point> */
/*                    QDPARS(3) =  <Z coordinate of surface point> */


/*              PHASE ANGLE */

/*                 is the apparent phase angle between a target body */
/*                 center and an illuminating body center as seen from an */
/*                 observer. */

/*                 Quantity Parameters: */

/*                    QNPARS    = 4 */
/*                    QPNAMS(1) = 'TARGET' */
/*                    QPNAMS(2) = 'OBSERVER' */
/*                    QPNAMS(3) = 'ABCORR' */
/*                    QPNAMS(4) = 'ILLUM' */

/*                    QCPARS(1) =  <name of target> */
/*                    QCPARS(2) =  <name of observer> */
/*                    QCPARS(3) =  <aberration correction> */
/*                    QCPARS(4) =  <name of illuminating body> */


/*              RANGE RATE */

/*                 is the apparent range rate between a target body and */
/*                 an observing body. */

/*                 Quantity Parameters: */

/*                    QNPARS    = 3 */
/*                    QPNAMS(1) = 'TARGET' */
/*                    QPNAMS(2) = 'OBSERVER' */
/*                    QPNAMS(3) = 'ABCORR' */

/*                    QCPARS(1) = <name of target> */
/*                    QCPARS(2) = <name of observer> */
/*                    QCPARS(3) = <aberration correction> */

/*     QNPARS   is the count of quantity parameter definition parameters. */
/*              These parameters supply the quantity-specific information */
/*              needed to fully define the quantity used in the search */
/*              performed by this routine. */

/*     QPNAMS   is an array of names of quantity definition parameters. */
/*              The names occupy elements 1:QNPARS of this array. The */
/*              value associated with the Ith element of QPNAMS is */
/*              located in element I of the parameter value argument */
/*              having data type appropriate for the parameter: */

/*                 Data Type                      Argument */
/*                 ---------                      -------- */
/*                 Character strings              QCPARS */
/*                 Double precision numbers       QDPARS */
/*                 Integers                       QIPARS */
/*                 Logicals                       QLPARS */

/*              The order in which the parameter names are listed is */
/*              unimportant, as long as the corresponding parameter */
/*              values are listed in the same order. */

/*              The names in QPNAMS are case-insensitive. */

/*              See the description of the input argument GQUANT for a */
/*              discussion of the parameter names and values associated */
/*              with a given quantity. */

/*     QCPARS, */
/*     QDPARS, */
/*     QIPARS, */
/*     QLPARS   are, respectively, parameter value arrays of types */

/*                  CHARACTER*(*)       QCPARS */
/*                  DOUBLE PRECISION    QDPARS */
/*                  INTEGER             QIPARS */
/*                  LOGICAL             QLPARS */

/*              The value associated with the Ith name in the array */
/*              QPNAMS resides in the Ith element of whichever of these */
/*              arrays has the appropriate data type. */

/*              All of these arrays should be declared with dimension at */
/*              least QNPARS. */

/*              The names in the array QCPARS are case-insensitive. */

/*              Note that there is no required order for QPNAMS/Q*PARS */
/*              pairs. */

/*              See the description of the input argument GQUANT for a */
/*              discussion of the parameter names and values associated */
/*              with a given quantity. */

/*     OP       is a scalar string comparison operator indicating the */
/*              numeric constraint of interest. Values are: */

/*                 '>'        value of geometric quantity greater than */
/*                            some reference (REFVAL). */

/*                 '='        value of geometric quantity equal to some */
/*                            reference (REFVAL). */

/*                 '<'        value of geometric quantity less than some */
/*                            reference (REFVAL). */

/*                 'ABSMAX'   The geometric quantity is at an absolute */
/*                            maximum. */

/*                 'ABSMIN'   The geometric quantity is at an absolute */
/*                            minimum. */

/*                 'LOCMAX'   The geometric quantity is at a local */
/*                            maximum. */

/*                 'LOCMIN'   The geometric quantity is at a local */
/*                            minimum. */

/*              The caller may indicate that the region of interest is */
/*              the set of time intervals where the quantity is within a */
/*              specified distance of an absolute extremum. The argument */
/*              ADJUST (described below) is used to specified this */
/*              distance. */

/*              Local extrema are considered to exist only in the */
/*              interiors of the intervals comprising the confinement */
/*              window: a local extremum cannot exist at a boundary point */
/*              of the confinement window. */

/*              Case is not significant in the string OP. */

/*     REFVAL   is the reference value used to define an equality or */
/*              inequality to be satisfied by the geometric quantity. The */
/*              units of REFVAL are radians, radians/sec, km, or km/sec */
/*              as appropriate. */

/*     TOL      is a tolerance value used to determine convergence of */
/*              root-finding operations. TOL is measured in ephemeris */
/*              seconds and must be greater than zero. */

/*     ADJUST   is the amount by which the quantity is allowed to vary */
/*              from an absolute extremum. */

/*              If the search is for an absolute minimum is performed, */
/*              the resulting window contains time intervals when the */
/*              geometric quantity GQUANT has values between ABSMIN and */
/*              ABSMIN + ADJUST. */

/*              If the search is for an absolute maximum, the */
/*              corresponding range is  between ABSMAX - ADJUST and */
/*              ABSMAX. */

/*              ADJUST is not used for searches for local extrema, */
/*              equality or inequality conditions and must have value */
/*              zero for such searches. ADJUST must not be negative. */

/*     CNFINE   is a SPICE window that confines the time period over */
/*              which the specified search is conducted. CNFINE may */
/*              consist of a single interval or a collection of */
/*              intervals. */

/*              In some cases the confinement window can be used to */
/*              greatly reduce the time period that must be searched */
/*              for the desired solution. See the $Particulars section */
/*              below for further discussion. */

/*              See the $Examples section below for a code example */
/*              that shows how to create a confinement window. */

/*              CNFINE must be initialized by the caller via the */
/*              SPICELIB routine SSIZED. */

/*              In some cases the observer's state may be computed at */
/*              times outside of CNFINE by as much as 2 seconds. See */
/*              $Particulars for details. */

/*     RPT      is a logical variable which controls whether the progress */
/*              reporter is enabled. When RPT is .TRUE., progress */
/*              reporting is enabled and the routines UDREPI, UDREPU, and */
/*              UDREPF (see descriptions below) are used to report */
/*              progress. */

/*     UDREPI   is the name of the user specified routine that */
/*              initializes a progress report. When progress reporting is */
/*              enabled, UDREPI is called at the start of a search. The */
/*              calling sequence of UDREPI is */

/*                 UDREPI ( CNFINE, SRCPRE, SRCSUF ) */

/*                 DOUBLE PRECISION    CNFINE ( LBCELL : * ) */
/*                 CHARACTER*(*)       SRCPRE */
/*                 CHARACTER*(*)       SRCSUF */

/*              where */

/*                 CNFINE */

/*              is a confinement window specifying the time period over */
/*              which a search is conducted, and */

/*                 SRCPRE */
/*                 SRCSUF */

/*              are prefix and suffix strings used in the progress */
/*              report: these strings are intended to bracket a */
/*              representation of the fraction of work done. For example, */
/*              when the SPICELIB progress reporting functions are used, */
/*              if SRCPRE and SRCSUF are, respectively, */

/*                 'Occultation/transit search' */
/*                 'done.' */

/*              the progress report display at the end of the search will */
/*              be: */

/*                 Occultation/transit search 100.00% done. */

/*              If the user doesn't wish to provide a custom set of */
/*              progress reporting functions, the SPICELIB routine */

/*                 GFREPI */

/*              may be used. */

/*     UDREPU   is the name of the user specified routine that updates */
/*              the progress report for a search. The calling sequence of */
/*              UDREPU is */

/*                 UDREPU (IVBEG, IVEND, ET ) */

/*                 DOUBLE PRECISION      ET */
/*                 DOUBLE PRECISION      IVBEG */
/*                 DOUBLE PRECISION      IVEND */

/*              where ET is an epoch belonging to the confinement window, */
/*              IVBEG and IVEND are the start and stop times, */
/*              respectively of the current confinement window interval. */
/*              The ratio of the measure of the portion of CNFINE that */
/*              precedes ET to the measure of CNFINE would be a logical */
/*              candidate for the searches completion percentage; however */
/*              the method of measurement is up to the user. */

/*              If the user doesn't wish to provide a custom set of */
/*              progress reporting functions, the SPICELIB routine */

/*                 GFREPU */

/*              may be used. */

/*     UDREPF   is the name of the user specified routine that finalizes */
/*              a progress report. UDREPF has no arguments. */

/*              If the user doesn't wish to provide a custom set of */
/*              progress reporting functions, the SPICELIB routine */

/*                 GFREPF */

/*              may be used. */

/*     MW       is a parameter specifying the length of the SPICE */
/*              windows in the workspace array WORK (see description */
/*              below) used by this routine. */

/*              MW should be set to a number at least twice as large */
/*              as the maximum number of intervals required by any */
/*              workspace window. In many cases, it's not necessary to */
/*              compute an accurate estimate of how many intervals are */
/*              needed; rather, the user can pick a size considerably */
/*              larger than what's really required. */

/*              However, since excessively large arrays can prevent */
/*              applications from compiling, linking, or running */
/*              properly, sometimes MW must be set according to */
/*              the actual workspace requirement. A rule of thumb */
/*              for the number of intervals NINTVLS needed is */

/*                 NINTVLS  =  2*N  +  ( M / STEP ) */

/*              where */

/*                 N     is the number of intervals in the confinement */
/*                       window */

/*                 M     is the measure of the confinement window, in */
/*                       units of seconds */

/*                 STEP  is the search step size in seconds */

/*              MW should then be set to */

/*                 2 * NINTVLS */

/*     NW       is a parameter specifying the number of SPICE windows */
/*              in the workspace array WORK (see description below) */
/*              used by this routine. (The reason this dimension is */
/*              an input argument is that this allows run-time */
/*              error checking to be performed.) */

/*     BAIL     is a logical flag indicating whether or not interrupt */
/*              signaling handling is enabled. When BAIL is set to */
/*              .TRUE., the input function UDBAIL (see description below) */
/*              is used to determine whether an interrupt has been */
/*              issued. */

/*     UDBAIL   is the name of the user specified routine that indicates */
/*              whether an interrupt signal has been issued (for example, */
/*              from the keyboard). UDBAIL has no arguments and returns */
/*              a LOGICAL value. The return value is .TRUE. if an */
/*              interrupt has been issued; otherwise the value is .FALSE. */

/*              GFEVNT uses UDBAIL only when BAIL (see above) is set */
/*              to .TRUE., indicating that interrupt handling is */
/*              enabled. When interrupt handling is enabled, GFEVNT */
/*              and routines in its call tree will call UDBAIL to */
/*              determine whether to terminate processing and return */
/*              immediately. */

/*              If interrupt handing is not enabled, a logical */
/*              function must still be passed as an input argument. */
/*              The SPICELIB function */

/*                 GFBAIL */

/*              may be used for this purpose. */

/*     RESULT   is a double precision SPICE window which will contain */
/*              the search results. RESULT must be declared and */
/*              initialized with sufficient size to capture the full */
/*              set of time intervals within the search region on which */
/*              the specified condition is satisfied. */

/*              RESULT must be initialized by the caller via the */
/*              SPICELIB routine SSIZED. */

/*              If RESULT is non-empty on input, its contents will be */
/*              discarded before GFEVNT conducts its search. */

/* $ Detailed_Output */

/*     WORK     is an array used to store workspace windows. */

/*              This array should be declared by the caller as shown: */

/*                 DOUBLE PRECISION     WORK ( LBCELL : MW,  NW ) */

/*              WORK need not be initialized by the caller. */

/*              WORK is modified by this routine. The caller should */
/*              re-initialize this array before attempting to use it for */
/*              any other purpose. */

/*     RESULT   is a SPICE window representing the set of time intervals, */
/*              within the confinement period, when the specified */
/*              geometric event occurs. */

/*              The endpoints of the time intervals comprising RESULT */
/*              are interpreted as seconds past J2000 TDB. */

/*              If the search is for local extrema, or for absolute */
/*              extrema with ADJUST set to zero, then normally each */
/*              interval of RESULT will be a singleton: the left and */
/*              right endpoints of each interval will be identical. */

/*              If no times within the confinement window satisfy the */
/*              search criteria, RESULT will be returned with a */
/*              cardinality of zero. */

/* $ Parameters */

/*     LBCELL   is the SPICE cell lower bound. */

/*     MAXPAR   is the maximum number of parameters required to define */
/*              any quantity. MAXPAR may grow if new quantities require */
/*              more parameters. MAXPAR is currently set to 10. */

/*     CNVTOL   is the default convergence tolerance used by the */
/*              high-level GF search API routines. This tolerance is */
/*              used to terminate searches for binary state transitions: */
/*              when the time at which a transition occurs is bracketed */
/*              by two times that differ by no more than */
/*              SPICE_GF_CNVTOL, the transition time is considered */
/*              to have been found. */

/* $ Exceptions */

/*     1)  There are varying requirements on how distinct the three */
/*         objects, QCPARS, must be. If the requirements are not met, an, */
/*         an error is signaled by a routine in the call tree of this */
/*         routine. */

/*         When GQUANT has value 'ANGULAR SEPARATION' then all three */
/*         must be distinct. */

/*         When GQUANT has value of either */

/*            'DISTANCE' */
/*            'COORDINATE' */
/*            'RANGE RATE' */

/*         the QCPARS(1) and QCPARS(2) objects must be distinct. */

/*     2)  If any of the bodies involved do not have NAIF ID codes, an */
/*         error is signaled by a routine in the call tree of this */
/*         routine. */

/*     3)  If the value of GQUANT is not recognized as a valid value, */
/*         the error SPICE(NOTRECOGNIZED) is signaled. */

/*     4)  If the number of quantity definition parameters, QNPARS is */
/*         greater than the maximum allowed value, MAXPAR, the error */
/*         SPICE(INVALIDCOUNT) is signaled. */

/*     5)  If the proper required parameters are not supplied in QNPARS, */
/*         the error SPICE(MISSINGVALUE) is signaled. */

/*     6)  If the comparison operator, OP, is not recognized, the error */
/*         SPICE(NOTRECOGNIZED) is signaled. */

/*     7)  If the window size MW is less than 2, an error is signaled by */
/*         a routine in the call tree of this routine. */

/*     8)  If the number of workspace windows NW is too small for the */
/*         required search, an error is signaled by a routine in the call */
/*         tree of this routine. */

/*     9)  If TOL is not greater than zero, an error is signaled by a */
/*         routine in the call tree of this routine. */

/*     10) If ADJUST is negative, an error is signaled by a routine in */
/*         the call tree of this routine. */

/*     11) If ADJUST has a non-zero value when OP has any value other */
/*         than 'ABSMIN' or 'ABSMAX', an error is signaled by a routine */
/*         in the call tree of this routine. */

/*     12) The user must take care when searching for an extremum */
/*         ('ABSMAX', 'ABSMIN', 'LOCMAX', 'LOCMIN') of an angular */
/*         quantity. Problems are most common when using the 'COORDINATE' */
/*         value of GQUANT with 'LONGITUDE' or 'RIGHT ASCENSION' values */
/*         for the coordinate name. Since these quantities are cyclical, */
/*         rather than monotonically increasing or decreasing, an */
/*         extremum may be hard to interpret. In particular, if an */
/*         extremum is found near the cycle boundary (-Pi for */
/*         'LONGITUDE', 2*Pi for 'RIGHT ASCENSION') it may not be */
/*         numerically reasonable. For example, the search for times when */
/*         a longitude coordinate is at its absolute maximum may result */
/*         in a time when the longitude value is -Pi, due to roundoff */
/*         error. */

/*     13) If operation of this routine is interrupted, the output result */
/*         window will be invalid. */

/* $ Files */

/*     Appropriate SPK and PCK kernels must be loaded by the */
/*     calling program before this routine is called. */

/*     The following data are required: */

/*     -  SPK data: ephemeris data for target, source and observer that */
/*        describes the ephemeris of these objects for the period */
/*        defined by the confinement window, CNFINE must be */
/*        loaded. If aberration corrections are used, the states of */
/*        target and observer relative to the solar system barycenter */
/*        must be calculable from the available ephemeris data. */
/*        Typically ephemeris data are made available by loading one */
/*        or more SPK files via FURNSH. */

/*     -  PCK data: bodies are assumed to be spherical and must have a */
/*        radius loaded from the kernel pool. Typically this is done by */
/*        loading a text PCK file via FURNSH. If the bodies are */
/*        triaxial, the largest radius is chosen as that of the */
/*        equivalent spherical body. */

/*     -  In some cases the observer's state may be computed at times */
/*        outside of CNFINE by as much as 2 seconds; data required to */
/*        compute this state must be provided by loaded kernels. See */
/*        $Particulars for details. */

/*     In all cases, kernel data are normally loaded once per program */
/*     run, NOT every time this routine is called. */

/* $ Particulars */

/*     This routine provides the SPICE GF subsystem's general interface */
/*     to determines time intervals when the value of some */
/*     geometric quantity related to one or more objects and an observer */
/*     satisfies a user specified constraint. It puts these times in a */
/*     result window called RESULT. It does this by first finding */
/*     windows when the quantity of interest is either monotonically */
/*     increasing or decreasing. These windows are then manipulated to */
/*     give the final result. */

/*     Applications that require do not require support for progress */
/*     reporting, interrupt handling, non-default step or refinement */
/*     functions, or non-default convergence tolerance normally should */
/*     call a high level geometry quantity routine rather than */
/*     this routine. */

/*     The Search Process */
/*     ================== */

/*     Regardless of the type of constraint selected by the caller, this */
/*     routine starts the search for solutions by determining the time */
/*     periods, within the confinement window, over which the specified */
/*     geometric quantity function is monotone increasing and monotone */
/*     decreasing. Each of these time periods is represented by a SPICE */
/*     window. Having found these windows, all of the quantity */
/*     function's local extrema within the confinement window are known. */
/*     Absolute extrema then can be found very easily. */

/*     Within any interval of these "monotone" windows, there will be at */
/*     most one solution of any equality constraint. Since the boundary */
/*     of the solution set for any inequality constraint is contained in */
/*     the union of */

/*     -  the set of points where an equality constraint is met */

/*     -  the boundary points of the confinement window */

/*     the solutions of both equality and inequality constraints can be */
/*     found easily once the monotone windows have been found. */


/*     Step Size */
/*     ========= */

/*     The monotone windows (described above) are found using a two-step */
/*     search process. Each interval of the confinement window is */
/*     searched as follows: first, the input step size is used to */
/*     determine the time separation at which the sign of the rate of */
/*     change of quantity function will be sampled. Starting at */
/*     the left endpoint of an interval, samples will be taken at each */
/*     step. If a change of sign is found, a root has been bracketed; at */
/*     that point, the time at which the time derivative of the quantity */
/*     function is zero can be found by a refinement process, for */
/*     example, using a binary search. */

/*     Note that the optimal choice of step size depends on the lengths */
/*     of the intervals over which the quantity function is monotone: */
/*     the step size should be shorter than the shortest of these */
/*     intervals (within the confinement window). */

/*     The optimal step size is *not* necessarily related to the lengths */
/*     of the intervals comprising the result window. For example, if */
/*     the shortest monotone interval has length 10 days, and if the */
/*     shortest result window interval has length 5 minutes, a step size */
/*     of 9.9 days is still adequate to find all of the intervals in the */
/*     result window. In situations like this, the technique of using */
/*     monotone windows yields a dramatic efficiency improvement over a */
/*     state-based search that simply tests at each step whether the */
/*     specified constraint is satisfied. The latter type of search can */
/*     miss solution intervals if the step size is longer than the */
/*     shortest solution interval. */

/*     Having some knowledge of the relative geometry of the targets and */
/*     observer can be a valuable aid in picking a reasonable step size. */
/*     In general, the user can compensate for lack of such knowledge by */
/*     picking a very short step size; the cost is increased computation */
/*     time. */

/*     Note that the step size is not related to the precision with which */
/*     the endpoints of the intervals of the result window are computed. */
/*     That precision level is controlled by the convergence tolerance. */


/*     Convergence Tolerance */
/*     ===================== */

/*     Once a root has been bracketed, a refinement process is used to */
/*     narrow down the time interval within which the root must lie. */
/*     This refinement process terminates when the location of the root */
/*     has been determined to within an error margin called the */
/*     "convergence tolerance," passed to this routine as 'tol'. */

/*     The GF subsystem defines a parameter, CNVTOL (from gf.inc), as a */
/*     default tolerance. This represents a "tight" tolerance value */
/*     so that the tolerance doesn't become the limiting factor in the */
/*     accuracy of solutions found by this routine. In general the */
/*     accuracy of input data will be the limiting factor. */

/*     Making the tolerance tighter than CNVTOL is unlikely to */
/*     be useful, since the results are unlikely to be more accurate. */
/*     Making the tolerance looser will speed up searches somewhat, */
/*     since a few convergence steps will be omitted. However, in most */
/*     cases, the step size is likely to have a much greater affect */
/*     on processing time than would the convergence tolerance. */


/*     The Confinement Window */
/*     ====================== */

/*     The simplest use of the confinement window is to specify a time */
/*     interval within which a solution is sought. However, the */
/*     confinement window can, in some cases, be used to make searches */
/*     more efficient. Sometimes it's possible to do an efficient search */
/*     to reduce the size of the time period over which a relatively */
/*     slow search of interest must be performed. */

/*     Certain types of searches require the state of the observer, */
/*     relative to the solar system barycenter, to be computed at times */
/*     slightly outside the confinement window CNFINE. The time window */
/*     that is actually used is the result of "expanding" CNFINE by a */
/*     specified amount "T": each time interval of CNFINE is expanded by */
/*     shifting the interval's left endpoint to the left and the right */
/*     endpoint to the right by T seconds. Any overlapping intervals are */
/*     merged. (The input argument CNFINE is not modified.) */

/*     The window expansions listed below are additive: if both */
/*     conditions apply, the window expansion amount is the sum of the */
/*     individual amounts. */

/*     -  If a search uses an equality constraint, the time window */
/*        over which the state of the observer is computed is expanded */
/*        by 1 second at both ends of all of the time intervals */
/*        comprising the window over which the search is conducted. */

/*     -  If a search uses stellar aberration corrections, the time */
/*        window over which the state of the observer is computed is */
/*        expanded as described above. */

/*     When light time corrections are used, expansion of the search */
/*     window also affects the set of times at which the light time- */
/*     corrected state of the target is computed. */

/*     In addition to the possible 2 second expansion of the search */
/*     window that occurs when both an equality constraint and stellar */
/*     aberration corrections are used, round-off error should be taken */
/*     into account when the need for data availability is analyzed. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Conduct a DISTANCE search using the default GF progress */
/*        reporting capability. */

/*        The program will use console I/O to display a simple */
/*        ASCII-based progress report. */

/*        The program will find local maximums of the distance from */
/*        Earth to Moon with light time and stellar aberration */
/*        corrections to model the apparent positions of the Moon. */

/*        Use the meta-kernel shown below to load the required SPICE */
/*        kernels. */


/*           KPL/MK */

/*           File name: gfevnt_ex1.tm */

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
/*              de414.bsp                     Planetary ephemeris */
/*              pck00008.tpc                  Planet orientation and */
/*                                            radii */
/*              naif0009.tls                  Leapseconds */


/*           \begindata */

/*              KERNELS_TO_LOAD = ( 'de414.bsp', */
/*                                  'pck00008.tpc', */
/*                                  'naif0009.tls'  ) */

/*           \begintext */

/*           End of meta-kernel */


/*        Example code begins here. */


/*              PROGRAM GFEVNT_EX1 */
/*              IMPLICIT              NONE */

/*        C */
/*        C     SPICELIB functions */
/*        C */
/*              DOUBLE PRECISION      SPD */
/*              INTEGER               WNCARD */

/*              INCLUDE               'gf.inc' */

/*        C */
/*        C     Local variables and initial parameters. */
/*        C */
/*              INTEGER               LBCELL */
/*              PARAMETER           ( LBCELL = -5 ) */

/*              INTEGER               LNSIZE */
/*              PARAMETER           ( LNSIZE = 80 ) */

/*              INTEGER               MAXPAR */
/*              PARAMETER           ( MAXPAR = 8 ) */

/*              INTEGER               MAXVAL */
/*              PARAMETER           ( MAXVAL = 20000 ) */


/*              INTEGER               STRSIZ */
/*              PARAMETER           ( STRSIZ = 40 ) */

/*              INTEGER               I */

/*        C */
/*        C     Confining window */
/*        C */
/*              DOUBLE PRECISION      CNFINE ( LBCELL : 2 ) */

/*        C */
/*        C     Confining window beginning and ending time strings. */
/*        C */
/*              CHARACTER*(STRSIZ)    BEGSTR */
/*              CHARACTER*(STRSIZ)    ENDSTR */

/*        C */
/*        C     Confining window beginning and ending times */
/*        C */
/*              DOUBLE PRECISION      BEGTIM */
/*              DOUBLE PRECISION      ENDTIM */

/*        C */
/*        C     Result window beginning and ending times for intervals. */
/*        C */
/*              DOUBLE PRECISION      BEG */
/*              DOUBLE PRECISION      END */

/*        C */
/*        C     Geometric quantity results window, work window, */
/*        C     bail switch and progress reporter switch. */
/*        C */
/*              DOUBLE PRECISION      RESULT ( LBCELL : MAXVAL ) */
/*              DOUBLE PRECISION      WORK   ( LBCELL : MAXVAL, NWDIST ) */

/*              LOGICAL               BAIL */
/*              LOGICAL               GFBAIL */
/*              EXTERNAL              GFBAIL */
/*              LOGICAL               RPT */

/*        C */
/*        C     Step size. */
/*        C */
/*              DOUBLE PRECISION      STEP */

/*        C */
/*        C     Geometric quantity name. */
/*        C */
/*              CHARACTER*(LNSIZE)    EVENT */

/*        C */
/*        C     Relational string */
/*        C */
/*              CHARACTER*(STRSIZ)    RELATE */

/*        C */
/*        C     Quantity definition parameter arrays: */
/*        C */
/*              INTEGER               QNPARS */
/*              CHARACTER*(LNSIZE)    QPNAMS ( MAXPAR ) */
/*              CHARACTER*(LNSIZE)    QCPARS ( MAXPAR ) */
/*              DOUBLE PRECISION      QDPARS ( MAXPAR ) */
/*              INTEGER               QIPARS ( MAXPAR ) */
/*              LOGICAL               QLPARS ( MAXPAR ) */

/*        C */
/*        C     Routines to set step size, refine transition times */
/*        C     and report work. */
/*        C */
/*              EXTERNAL              GFREFN */
/*              EXTERNAL              GFREPI */
/*              EXTERNAL              GFREPU */
/*              EXTERNAL              GFREPF */
/*              EXTERNAL              GFSTEP */

/*        C */
/*        C     Reference and adjustment values. */
/*        C */
/*              DOUBLE PRECISION      REFVAL */
/*              DOUBLE PRECISION      ADJUST */

/*              INTEGER               COUNT */

/*        C */
/*        C     Saved variables */
/*        C */
/*        C     The confinement, workspace and result windows CNFINE, */
/*        C     WORK and RESULT are saved because this practice helps to */
/*        C     prevent stack overflow. */
/*        C */
/*              SAVE                  CNFINE */
/*              SAVE                  RESULT */
/*              SAVE                  WORK */

/*        C */
/*        C     Load leapsecond and spk kernels. The name of the */
/*        C     meta kernel file shown here is fictitious; you */
/*        C     must supply the name of a file available */
/*        C     on your own computer system. */

/*              CALL FURNSH ('gfevnt_ex1.tm') */

/*        C */
/*        C     Set a beginning and end time for confining window. */
/*        C */
/*              BEGSTR = '2001 jan 01 00:00:00.000' */
/*              ENDSTR = '2001 jun 30 00:00:00.000' */

/*              CALL STR2ET ( BEGSTR, BEGTIM ) */
/*              CALL STR2ET ( ENDSTR, ENDTIM ) */

/*        C */
/*        C     Set condition for extremum. */
/*        C */
/*              RELATE  =   'LOCMAX' */

/*        C */
/*        C     Set reference value (if needed) and absolute extremum */
/*        C     adjustment (if needed). */
/*        C */
/*              REFVAL   =    0.D0 */
/*              ADJUST   =    0.D0 */

/*        C */
/*        C     Set quantity. */
/*        C */
/*              EVENT    =   'DISTANCE' */

/*        C */
/*        C     Turn on progress reporter and initialize the windows. */
/*        C */
/*              RPT    = .TRUE. */
/*              BAIL   = .FALSE. */

/*              CALL SSIZED ( 2,      CNFINE ) */
/*              CALL SSIZED ( MAXVAL, RESULT ) */

/*        C */
/*        C     Add 2 points to the confinement interval window. */
/*        C */
/*              CALL WNINSD ( BEGTIM, ENDTIM, CNFINE ) */

/*        C */
/*        C     Define input quantities. */
/*        C */
/*              QPNAMS(1) = 'TARGET' */
/*              QCPARS(1) = 'MOON' */

/*              QPNAMS(2) = 'OBSERVER' */
/*              QCPARS(2) = 'EARTH' */

/*              QPNAMS(3) = 'ABCORR' */
/*              QCPARS(3) = 'LT+S' */

/*              QNPARS    = 3 */

/*        C */
/*        C     Set the step size to 1 day and convert to seconds. */
/*        C */
/*              STEP = 1.D-3*SPD() */

/*              CALL GFSSTP ( STEP ) */

/*        C */
/*        C    Look for solutions. */
/*        C */
/*              CALL GFEVNT ( GFSTEP,     GFREFN,   EVENT, */
/*             .              QNPARS,     QPNAMS,   QCPARS, */
/*             .              QDPARS,     QIPARS,   QLPARS, */
/*             .              RELATE,     REFVAL,   CNVTOL, */
/*             .              ADJUST,     CNFINE,   RPT, */
/*             .              GFREPI,     GFREPU,   GFREPF, */
/*             .              MAXVAL,     NWDIST,   WORK, */
/*             .              BAIL,       GFBAIL,   RESULT ) */


/*        C */
/*        C     Check the number of intervals in the result window. */
/*        C */
/*              COUNT = WNCARD(RESULT) */

/*              WRITE (*,*) 'Found ', COUNT, ' intervals in RESULT' */
/*              WRITE (*,*) ' ' */

/*        C */
/*        C     List the beginning and ending points in each interval. */
/*        C */
/*              DO I = 1, COUNT */

/*                CALL WNFETD ( RESULT, I, BEG, END  ) */

/*                CALL TIMOUT ( BEG, */
/*             .                'YYYY-MON-DD HR:MN:SC.###### ' */
/*             .  //            '(TDB) ::TDB ::RND',  BEGSTR ) */
/*                CALL TIMOUT ( END, */
/*             .                'YYYY-MON-DD HR:MN:SC.###### ' */
/*             . //             '(TDB) ::TDB ::RND',  ENDSTR ) */

/*                WRITE (*,*) 'Interval ',  I */
/*                WRITE (*,*) 'Beginning TDB ', BEGSTR */
/*                WRITE (*,*) 'Ending TDB    ', ENDSTR */
/*                WRITE (*,*) ' ' */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        Distance pass 1 of 1 100.00% done. */

/*         Found            6  intervals in RESULT */

/*         Interval            1 */
/*         Beginning TDB 2001-JAN-24 19:22:01.436672 (TDB) */
/*         Ending TDB    2001-JAN-24 19:22:01.436672 (TDB) */

/*         Interval            2 */
/*         Beginning TDB 2001-FEB-20 21:52:07.914964 (TDB) */
/*         Ending TDB    2001-FEB-20 21:52:07.914964 (TDB) */

/*         Interval            3 */
/*         Beginning TDB 2001-MAR-20 11:32:03.182345 (TDB) */
/*         Ending TDB    2001-MAR-20 11:32:03.182345 (TDB) */

/*         Interval            4 */
/*         Beginning TDB 2001-APR-17 06:09:00.877038 (TDB) */
/*         Ending TDB    2001-APR-17 06:09:00.877038 (TDB) */

/*         Interval            5 */
/*         Beginning TDB 2001-MAY-15 01:29:28.532819 (TDB) */
/*         Ending TDB    2001-MAY-15 01:29:28.532819 (TDB) */

/*         Interval            6 */
/*         Beginning TDB 2001-JUN-11 19:44:10.855458 (TDB) */
/*         Ending TDB    2001-JUN-11 19:44:10.855458 (TDB) */


/*        Note that the progress report has the format shown below: */

/*           Distance pass 1 of 1   6.02% done. */

/*        The completion percentage was updated approximately once per */
/*        second. */

/*        When the program was interrupted at an arbitrary time, */
/*        the output was: */

/*           Distance pass 1 of 1  13.63% done. */
/*           Search was interrupted. */

/*        This message was written after an interrupt signal */
/*        was trapped. By default, the program would have terminated */
/*        before this message could be written. */

/* $ Restrictions */

/*     1)  The kernel files to be used by GFEVNT must be loaded (normally */
/*         via the SPICELIB routine FURNSH) before GFEVNT is called. */

/*     2)  If using the default, constant step size routine, GFSTEP, the */
/*         entry point GFSSTP must be called prior to calling this */
/*         routine. The call syntax for GFSSTP: */

/*            CALL GFSSTP ( STEP ) */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.1.0, 27-OCT-2021 (JDR) (BVS) (NJB) */

/*        Edited the header to comply with NAIF standard. */

/*        Updated description of WORK and RESULT arguments in $Brief_I/O, */
/*        $Detailed_Input and $Detailed_Output. */

/*        Added SAVE statements for CNFINE, WORK and RESULT variables in */
/*        code example. */

/*        Added SAVE statement for DREF. */

/*        Fixed typo in $Exceptions entry #5, which referred to a non */
/*        existing input argument, replaced entry #7 by new entries #7 */
/*        and #8, replaced entry #10 by new entries #10 and #11, and */
/*        added entry #13. */

/*        Added descriptions of MAXPAR and CNVTOL to the $Brief_I/O and */
/*        $Parameters sections. */

/*        Moved declaration of MAXPAR into the $Declarations section. */

/*        Updated header to describe use of expanded confinement window. */

/* -    SPICELIB Version 2.0.0, 05-SEP-2012 (EDW) (NJB) */

/*        Edit to comments to correct search description. */

/*        Edit to $Index_Entries. */

/*        Added geometric quantities: */

/*           Phase Angle */
/*           Illumination Angle */

/*        Code edits to implement use of ZZGFRELX in event calculations: */

/*           Range rate */
/*           Separation angle */
/*           Distance */
/*           Coordinate */

/*        The code changes for ZZGFRELX use should not affect the */
/*        numerical results of GF computations. */

/* -    SPICELIB Version 1.1.0, 09-OCT-2009 (NJB) (EDW) */

/*        Edits to argument descriptions. */

/*        Added geometric quantities: */

/*           Range Rate */

/* -    SPICELIB Version 1.0.0, 19-MAR-2009 (NJB) (EDW) */

/* -& */
/* $ Index_Entries */

/*     GF mid-level geometric condition solver */

/* -& */

/*     SPICELIB functions */


/*     Angular separation routines. */


/*     Distance routines. */


/*     Range rate routines. */


/*     Phase angle routines. */


/*     Illumination angle routines. */


/*     Event quantity codes: */


/*     Number of supported quantities: */


/*     Number of supported comparison operators: */


/*     Assorted string lengths: */


/*     MAXOP is the maximum string length for comparison operators. */
/*     MAXOP may grow if new comparisons are added. */


/*     MAXCLN is the maximum character string length of the quantity */
/*     parameter names and character quantity parameters. */


/*     Local variables */


/*     Saved variables */


/*     Initial values */

    /* Parameter adjustments */
    work_dim1 = *mw + 6;
    work_offset = work_dim1 - 5;

    /* Function Body */

/*     Below we initialize the list of quantity names. Restrict this list */
/*     to those events supported with test families. */


/*     Below we initialize the list of comparison operator names. */


/*     Below we initialize the list of quantity parameter names. */
/*     Each quantity has its own list of parameter names. */

/*     NOTE:  ALL of the initializers below must be updated when */
/*     the parameter MAXPAR is increased.  The number blank string */
/*     initial values must be increased so that the total number */
/*     of values for each array is MAXPAR. */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("GFEVNT", (ftnlen)6);
    if (first) {

/*        Set the progress report prefix and suffix strings for */
/*        each quantity. No need to set coordinate quantity strings. */
/*        The coordinate solver performs that function. */

	first = FALSE_;
	s_copy(srcpre, "Angular separation pass 1 of #", (ftnlen)55, (ftnlen)
		30);
	s_copy(srcpre + 55, "Angular separation pass 2 of #", (ftnlen)55, (
		ftnlen)30);
	s_copy(srcpre + 110, "Distance pass 1 of # ", (ftnlen)55, (ftnlen)21);
	s_copy(srcpre + 165, "Distance pass 2 of # ", (ftnlen)55, (ftnlen)21);
	s_copy(srcpre + 660, "Angular Rate pass 1 of #", (ftnlen)55, (ftnlen)
		24);
	s_copy(srcpre + 715, "Angular Rate pass 2 of #", (ftnlen)55, (ftnlen)
		24);
	s_copy(srcpre + 330, "Range Rate pass 1 of #", (ftnlen)55, (ftnlen)22)
		;
	s_copy(srcpre + 385, "Range Rate pass 2 of #", (ftnlen)55, (ftnlen)22)
		;
	s_copy(srcpre + 440, "Phase angle search pass 1 of #", (ftnlen)55, (
		ftnlen)30);
	s_copy(srcpre + 495, "Phase angle search pass 2 of #", (ftnlen)55, (
		ftnlen)30);
	s_copy(srcpre + 770, "Diameter pass 1 of #", (ftnlen)55, (ftnlen)20);
	s_copy(srcpre + 825, "Diameter pass 2 of #", (ftnlen)55, (ftnlen)20);
	s_copy(srcpre + 550, "Illumination angle pass 1 of #", (ftnlen)55, (
		ftnlen)30);
	s_copy(srcpre + 605, "Illumination angle pass 2 of #", (ftnlen)55, (
		ftnlen)30);
	s_copy(srcsuf, "done.", (ftnlen)13, (ftnlen)5);
	s_copy(srcsuf + 13, "done.", (ftnlen)13, (ftnlen)5);
	s_copy(srcsuf + 26, "done.", (ftnlen)13, (ftnlen)5);
	s_copy(srcsuf + 39, "done.", (ftnlen)13, (ftnlen)5);
	s_copy(srcsuf + 156, "done.", (ftnlen)13, (ftnlen)5);
	s_copy(srcsuf + 169, "done.", (ftnlen)13, (ftnlen)5);
	s_copy(srcsuf + 78, "done.", (ftnlen)13, (ftnlen)5);
	s_copy(srcsuf + 91, "done.", (ftnlen)13, (ftnlen)5);
	s_copy(srcsuf + 104, "done.", (ftnlen)13, (ftnlen)5);
	s_copy(srcsuf + 117, "done.", (ftnlen)13, (ftnlen)5);
	s_copy(srcsuf + 182, "done.", (ftnlen)13, (ftnlen)5);
	s_copy(srcsuf + 195, "done.", (ftnlen)13, (ftnlen)5);
	s_copy(srcsuf + 130, "done.", (ftnlen)13, (ftnlen)5);
	s_copy(srcsuf + 143, "done.", (ftnlen)13, (ftnlen)5);
    }

/*     Make sure the requested quantity is one we recognize. */

    ljust_(gquant, quant, gquant_len, (ftnlen)80);
    ucase_(quant, quant, (ftnlen)80, (ftnlen)80);
    qtnum = isrchc_(quant, &c__8, qnames, (ftnlen)80, (ftnlen)80);
    if (qtnum == 0) {
	setmsg_("The geometric quantity, # is not recognized. Supported quan"
		"tities are: DISTANCE, PHASE ANGLE, COORDINATE, RANGE RATE, A"
		"NGULAR SEPARATION,ILLUMINATION ANGLE.", (ftnlen)156);
	errch_("#", gquant, (ftnlen)1, gquant_len);
	sigerr_("SPICE(NOTRECOGNIZED)", (ftnlen)20);
	chkout_("GFEVNT", (ftnlen)6);
	return 0;
    }

/*     Check number of quantity definition parameters. */

    if (*qnpars < 0 || *qnpars > 10) {
	setmsg_("Number of quantity definition parameters = #;  must be in r"
		"ange 0:#.", (ftnlen)68);
	errint_("#", qnpars, (ftnlen)1);
	errint_("#", &c__10, (ftnlen)1);
	sigerr_("SPICE(INVALIDCOUNT)", (ftnlen)19);
	chkout_("GFEVNT", (ftnlen)6);
	return 0;
    }

/*     Make left-justified, upper case copies of parameter names. */

    i__1 = *qnpars;
    for (i__ = 1; i__ <= i__1; ++i__) {
	ljust_(qpnams + (i__ - 1) * qpnams_len, pnames + ((i__2 = i__ - 1) < 
		10 && 0 <= i__2 ? i__2 : s_rnge("pnames", i__2, "gfevnt_", (
		ftnlen)1972)) * 80, qpnams_len, (ftnlen)80);
	ucase_(pnames + ((i__2 = i__ - 1) < 10 && 0 <= i__2 ? i__2 : s_rnge(
		"pnames", i__2, "gfevnt_", (ftnlen)1973)) * 80, pnames + ((
		i__3 = i__ - 1) < 10 && 0 <= i__3 ? i__3 : s_rnge("pnames", 
		i__3, "gfevnt_", (ftnlen)1973)) * 80, (ftnlen)80, (ftnlen)80);
	ljust_(qcpars + (i__ - 1) * qcpars_len, cpars + ((i__2 = i__ - 1) < 
		10 && 0 <= i__2 ? i__2 : s_rnge("cpars", i__2, "gfevnt_", (
		ftnlen)1975)) * 80, qcpars_len, (ftnlen)80);
	ucase_(cpars + ((i__2 = i__ - 1) < 10 && 0 <= i__2 ? i__2 : s_rnge(
		"cpars", i__2, "gfevnt_", (ftnlen)1976)) * 80, cpars + ((i__3 
		= i__ - 1) < 10 && 0 <= i__3 ? i__3 : s_rnge("cpars", i__3, 
		"gfevnt_", (ftnlen)1976)) * 80, (ftnlen)80, (ftnlen)80);
    }

/*     Make sure all parameters have been supplied for the requested */
/*     quantity. */

    for (i__ = 1; i__ <= 10; ++i__) {
	if (s_cmp(qpars + ((i__1 = i__ + qtnum * 10 - 11) < 80 && 0 <= i__1 ? 
		i__1 : s_rnge("qpars", i__1, "gfevnt_", (ftnlen)1986)) * 80, 
		" ", (ftnlen)80, (ftnlen)1) != 0) {

/*           The Ith parameter must be supplied by the caller. */

	    loc = isrchc_(qpars + ((i__1 = i__ + qtnum * 10 - 11) < 80 && 0 <=
		     i__1 ? i__1 : s_rnge("qpars", i__1, "gfevnt_", (ftnlen)
		    1990)) * 80, qnpars, pnames, (ftnlen)80, (ftnlen)80);
	    if (loc == 0) {
		setmsg_("The parameter # is required in order to compute eve"
			"nts pertaining to the quantity #; this parameter was"
			" not supplied.", (ftnlen)117);
		errch_("#", qpars + ((i__1 = i__ + qtnum * 10 - 11) < 80 && 0 
			<= i__1 ? i__1 : s_rnge("qpars", i__1, "gfevnt_", (
			ftnlen)1999)) * 80, (ftnlen)1, (ftnlen)80);
		errch_("#", qnames + ((i__1 = qtnum - 1) < 8 && 0 <= i__1 ? 
			i__1 : s_rnge("qnames", i__1, "gfevnt_", (ftnlen)2000)
			) * 80, (ftnlen)1, (ftnlen)80);
		sigerr_("SPICE(MISSINGVALUE)", (ftnlen)19);
		chkout_("GFEVNT", (ftnlen)6);
		return 0;
	    }
	}
    }

/*     Capture as local variables those parameters passed from the */
/*     callers. */

/*     If the PNAMES array contains any of the parameters */

/*        TARGET */
/*        OBSERVER */
/*        ILLUM */
/*        TARGET1 */
/*        FRAME1 */
/*        SHAPE1 */
/*        TARGET2 */
/*        FRAME2 */
/*        SHAPE2 */
/*        ABCORR */
/*        REFERENCE FRAME */
/*        DREF */
/*        DVEC */

/*     copy the value corresponding to the parameter to a local variable. */

/*     These operations demonstrate the need for associative arrays */
/*     as part of Fortran. */


/*     -TARGET- */

    loc = isrchc_("TARGET", qnpars, pnames, (ftnlen)6, (ftnlen)80);
    if (loc > 0) {
	s_copy(target, cpars + ((i__1 = loc - 1) < 10 && 0 <= i__1 ? i__1 : 
		s_rnge("cpars", i__1, "gfevnt_", (ftnlen)2045)) * 80, (ftnlen)
		80, (ftnlen)80);
    }

/*     -OBSERVER- */

    loc = isrchc_("OBSERVER", qnpars, pnames, (ftnlen)8, (ftnlen)80);
    if (loc > 0) {
	s_copy(obsrvr, cpars + ((i__1 = loc - 1) < 10 && 0 <= i__1 ? i__1 : 
		s_rnge("cpars", i__1, "gfevnt_", (ftnlen)2057)) * 80, (ftnlen)
		80, (ftnlen)80);
    }

/*     -ILLUM- */

    loc = isrchc_("ILLUM", qnpars, pnames, (ftnlen)5, (ftnlen)80);
    if (loc > 0) {
	s_copy(illum, cpars + ((i__1 = loc - 1) < 10 && 0 <= i__1 ? i__1 : 
		s_rnge("cpars", i__1, "gfevnt_", (ftnlen)2069)) * 80, (ftnlen)
		80, (ftnlen)80);
    }

/*     -TARGET1- */

    loc = isrchc_("TARGET1", qnpars, pnames, (ftnlen)7, (ftnlen)80);
    if (loc > 0) {
	s_copy(of, cpars + ((i__1 = loc - 1) < 10 && 0 <= i__1 ? i__1 : 
		s_rnge("cpars", i__1, "gfevnt_", (ftnlen)2081)) * 80, (ftnlen)
		80, (ftnlen)80);
    }

/*     -TARGET2- */

    loc = isrchc_("TARGET2", qnpars, pnames, (ftnlen)7, (ftnlen)80);
    if (loc > 0) {
	s_copy(of + 80, cpars + ((i__1 = loc - 1) < 10 && 0 <= i__1 ? i__1 : 
		s_rnge("cpars", i__1, "gfevnt_", (ftnlen)2093)) * 80, (ftnlen)
		80, (ftnlen)80);
    }

/*     -FRAME1- */

    loc = isrchc_("FRAME1", qnpars, pnames, (ftnlen)6, (ftnlen)80);
    if (loc > 0) {
	s_copy(frame, cpars + ((i__1 = loc - 1) < 10 && 0 <= i__1 ? i__1 : 
		s_rnge("cpars", i__1, "gfevnt_", (ftnlen)2105)) * 80, (ftnlen)
		80, (ftnlen)80);
    }

/*     -FRAME2- */

    loc = isrchc_("FRAME2", qnpars, pnames, (ftnlen)6, (ftnlen)80);
    if (loc > 0) {
	s_copy(frame + 80, cpars + ((i__1 = loc - 1) < 10 && 0 <= i__1 ? i__1 
		: s_rnge("cpars", i__1, "gfevnt_", (ftnlen)2116)) * 80, (
		ftnlen)80, (ftnlen)80);
    }

/*     -SHAPE1- */

    loc = isrchc_("SHAPE1", qnpars, pnames, (ftnlen)6, (ftnlen)80);
    if (loc > 0) {
	s_copy(shape, cpars + ((i__1 = loc - 1) < 10 && 0 <= i__1 ? i__1 : 
		s_rnge("cpars", i__1, "gfevnt_", (ftnlen)2128)) * 80, (ftnlen)
		80, (ftnlen)80);
    }

/*     -SHAPE2- */

    loc = isrchc_("SHAPE2", qnpars, pnames, (ftnlen)6, (ftnlen)80);
    if (loc > 0) {
	s_copy(shape + 80, cpars + ((i__1 = loc - 1) < 10 && 0 <= i__1 ? i__1 
		: s_rnge("cpars", i__1, "gfevnt_", (ftnlen)2140)) * 80, (
		ftnlen)80, (ftnlen)80);
    }

/*     -ABCORR- */

    loc = isrchc_("ABCORR", qnpars, pnames, (ftnlen)6, (ftnlen)80);
    if (loc > 0) {
	s_copy(abcorr, cpars + ((i__1 = loc - 1) < 10 && 0 <= i__1 ? i__1 : 
		s_rnge("cpars", i__1, "gfevnt_", (ftnlen)2152)) * 80, (ftnlen)
		80, (ftnlen)80);
    }

/*     -REFERENCE FRAME- */

    loc = isrchc_("REFERENCE FRAME", qnpars, pnames, (ftnlen)15, (ftnlen)80);
    if (loc > 0) {
	s_copy(ref, cpars + ((i__1 = loc - 1) < 10 && 0 <= i__1 ? i__1 : 
		s_rnge("cpars", i__1, "gfevnt_", (ftnlen)2164)) * 80, (ftnlen)
		80, (ftnlen)80);
    }

/*     -COORDINATE SYSTEM- */

    loc = isrchc_("COORDINATE SYSTEM", qnpars, qpnams, (ftnlen)17, qpnams_len)
	    ;
    if (loc > 0) {
	s_copy(corsys, qcpars + (loc - 1) * qcpars_len, (ftnlen)80, 
		qcpars_len);
    }

/*     -COORDINATE- */

    loc = isrchc_("COORDINATE", qnpars, qpnams, (ftnlen)10, qpnams_len);
    if (loc > 0) {
	s_copy(cornam, qcpars + (loc - 1) * qcpars_len, (ftnlen)80, 
		qcpars_len);
    }

/*     -VECTOR DEFINITION- */

    loc = isrchc_("VECTOR DEFINITION", qnpars, qpnams, (ftnlen)17, qpnams_len)
	    ;
    if (loc > 0) {
	s_copy(vecdef, qcpars + (loc - 1) * qcpars_len, (ftnlen)80, 
		qcpars_len);
    }

/*     -DVEC- */

    loc = isrchc_("DVEC", qnpars, pnames, (ftnlen)4, (ftnlen)80);
    if (loc > 0) {
	vequ_(qdpars, dvec);
    }

/*     -METHOD- */

    loc = isrchc_("METHOD", qnpars, qpnams, (ftnlen)6, qpnams_len);
    if (loc > 0) {
	s_copy(method, qcpars + (loc - 1) * qcpars_len, (ftnlen)80, 
		qcpars_len);
    }

/*     -DREF- */

    loc = isrchc_("DREF", qnpars, pnames, (ftnlen)4, (ftnlen)80);
    if (loc > 0) {
	s_copy(dref, cpars + ((i__1 = loc - 1) < 10 && 0 <= i__1 ? i__1 : 
		s_rnge("cpars", i__1, "gfevnt_", (ftnlen)2235)) * 80, (ftnlen)
		80, (ftnlen)80);
    }

/*     -ANGTYP- */

    loc = isrchc_("ANGTYP", qnpars, pnames, (ftnlen)6, (ftnlen)80);
    if (loc > 0) {
	s_copy(angtyp, cpars + ((i__1 = loc - 1) < 10 && 0 <= i__1 ? i__1 : 
		s_rnge("cpars", i__1, "gfevnt_", (ftnlen)2246)) * 80, (ftnlen)
		80, (ftnlen)80);
    }

/*     -SPOINT- */

    loc = isrchc_("SPOINT", qnpars, pnames, (ftnlen)6, (ftnlen)80);
    if (loc > 0) {
	vequ_(qdpars, spoint);
    }

/*     Make sure that the requested comparison operation is one we */
/*     recognize. */

    ljust_(op, uop, op_len, (ftnlen)6);
    ucase_(uop, uop, (ftnlen)6, (ftnlen)6);
    loc = isrchc_(uop, &c__7, cnames, (ftnlen)6, (ftnlen)80);
    if (loc == 0) {
	setmsg_("The comparison operator, # is not recognized.  Supported op"
		"erators are: >, =, <, ABSMAX, ABSMIN, LOCMAX, LOCMIN. ", (
		ftnlen)113);
	errch_("#", op, (ftnlen)1, op_len);
	sigerr_("SPICE(NOTRECOGNIZED)", (ftnlen)20);
	chkout_("GFEVNT", (ftnlen)6);
	return 0;
    }

/*     If progress reporting is enabled, set the report prefix array */
/*     according to the quantity and the relational operator. */

    if (*rpt) {

/*        We'll use the logical flag LOCALX to indicate a local extremum */
/*        operator and the flag NOADJX to indicate an absolute extremum */
/*        operator with zero adjustment. */

	localx = s_cmp(uop, "LOCMIN", (ftnlen)6, (ftnlen)6) == 0 || s_cmp(uop,
		 "LOCMAX", (ftnlen)6, (ftnlen)6) == 0;
	noadjx = *adjust == 0. && (s_cmp(uop, "ABSMIN", (ftnlen)6, (ftnlen)6) 
		== 0 || s_cmp(uop, "ABSMAX", (ftnlen)6, (ftnlen)6) == 0);
	if (localx || noadjx) {

/*           These operators correspond to 1-pass searches. */

	    npass = 1;
	} else {
	    npass = 2;
	}

/*        Fill in the prefix strings. */

/*        Note that we've already performed error checks on QTNUM. */

	i__1 = npass;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    repmi_(srcpre + ((i__2 = i__ + (qtnum << 1) - 3) < 16 && 0 <= 
		    i__2 ? i__2 : s_rnge("srcpre", i__2, "gfevnt_", (ftnlen)
		    2325)) * 55, "#", &npass, rptpre + ((i__3 = i__ - 1) < 2 
		    && 0 <= i__3 ? i__3 : s_rnge("rptpre", i__3, "gfevnt_", (
		    ftnlen)2325)) * 55, (ftnlen)55, (ftnlen)1, (ftnlen)55);
	}
    }

/*     Here's where the real work gets done:  we solve for the */
/*     result window.  The code below is quantity-specific.  However, */
/*     in each case, we always initialize the utility routines for */
/*     the quantity of interest, then call the generic relation */
/*     pre-image solver ZZGFREL. */

    if (qtnum == 1) {

/*        Separation condition initializer. */

	zzgfspin_(of, obsrvr, shape, frame, abcorr, (ftnlen)80, (ftnlen)80, (
		ftnlen)80, (ftnlen)80, (ftnlen)80);
	zzgfrelx_((U_fp)udstep, (U_fp)udrefn, (U_fp)zzgfspdc_, (U_fp)
		zzgfudlt_, (U_fp)zzgfspgq_, op, refval, tol, adjust, cnfine, 
		mw, nw, work, rpt, (U_fp)udrepi, (U_fp)udrepu, (U_fp)udrepf, 
		rptpre, srcsuf, bail, (L_fp)udbail, result, op_len, (ftnlen)
		55, (ftnlen)13);
    } else if (qtnum == 2) {

/*        Distance condition initializer. */

	zzgfdiin_(target, abcorr, obsrvr, (ftnlen)80, (ftnlen)80, (ftnlen)80);
	zzgfrelx_((U_fp)udstep, (U_fp)udrefn, (U_fp)zzgfdidc_, (U_fp)
		zzgfudlt_, (U_fp)zzgfdigq_, op, refval, tol, adjust, cnfine, 
		mw, nw, work, rpt, (U_fp)udrepi, (U_fp)udrepu, (U_fp)udrepf, 
		rptpre, srcsuf + 26, bail, (L_fp)udbail, result, op_len, (
		ftnlen)55, (ftnlen)13);
    } else if (qtnum == 3) {

/*        Solve for a coordinate condition. ZZGFCSLV calls the coordinate */
/*        event initializer. */

	zzgfcslv_(vecdef, method, target, ref, abcorr, obsrvr, dref, dvec, 
		corsys, cornam, op, refval, tol, adjust, (U_fp)udstep, (U_fp)
		udrefn, rpt, (U_fp)udrepi, (U_fp)udrepu, (U_fp)udrepf, bail, (
		L_fp)udbail, mw, nw, work, cnfine, result, (ftnlen)80, (
		ftnlen)80, (ftnlen)80, (ftnlen)80, (ftnlen)80, (ftnlen)80, (
		ftnlen)80, (ftnlen)80, (ftnlen)80, op_len);
    } else if (qtnum == 7) {

/*        d( sep ) */
/*        --------     ---Not yet implemented--- */
/*        dt */

    } else if (qtnum == 4) {

/*        Range rate condition initializer. */

/*        Default the interval for the QDERIV call in ZZGFRRDC to one */
/*        TDB second. This should have a function interface to */
/*        reset. */

	dt = 1.;
	zzgfrrin_(target, abcorr, obsrvr, &dt, (ftnlen)80, (ftnlen)80, (
		ftnlen)80);
	zzgfrelx_((U_fp)udstep, (U_fp)udrefn, (U_fp)zzgfrrdc_, (U_fp)
		zzgfudlt_, (U_fp)zzgfrrgq_, op, refval, tol, adjust, cnfine, 
		mw, nw, work, rpt, (U_fp)udrepi, (U_fp)udrepu, (U_fp)udrepf, 
		rptpre, srcsuf + 78, bail, (L_fp)udbail, result, op_len, (
		ftnlen)55, (ftnlen)13);
    } else if (qtnum == 5) {

/*        Phase angle condition initializer. */

	zzgfpain_(target, illum, abcorr, obsrvr, (ftnlen)80, (ftnlen)80, (
		ftnlen)80, (ftnlen)80);
	zzgfrelx_((U_fp)udstep, (U_fp)udrefn, (U_fp)zzgfpadc_, (U_fp)
		zzgfudlt_, (U_fp)zzgfpagq_, op, refval, tol, adjust, cnfine, 
		mw, nw, work, rpt, (U_fp)udrepi, (U_fp)udrepu, (U_fp)udrepf, 
		rptpre, srcsuf + 104, bail, (L_fp)udbail, result, op_len, (
		ftnlen)55, (ftnlen)13);
    } else if (qtnum == 8) {

/*                ---Not yet implemented--- */

    } else if (qtnum == 6) {

/*        Illumination angle condition initializer. */

	zzgfilin_(method, angtyp, target, illum, ref, abcorr, obsrvr, spoint, 
		(ftnlen)80, (ftnlen)80, (ftnlen)80, (ftnlen)80, (ftnlen)80, (
		ftnlen)80, (ftnlen)80);
	zzgfrelx_((U_fp)udstep, (U_fp)udrefn, (U_fp)zzgfildc_, (U_fp)
		zzgfudlt_, (U_fp)zzgfilgq_, op, refval, tol, adjust, cnfine, 
		mw, nw, work, rpt, (U_fp)udrepi, (U_fp)udrepu, (U_fp)udrepf, 
		rptpre, srcsuf + 130, bail, (L_fp)udbail, result, op_len, (
		ftnlen)55, (ftnlen)13);
    } else {

/*        QTNUM is not a recognized event code. This block should */
/*        never execute since we already checked the input quantity */
/*        name string. */

	setmsg_("Unknown event '#'. This error indicates a bug. Please conta"
		"ct NAIF.", (ftnlen)67);
	errch_("#", gquant, (ftnlen)1, gquant_len);
	sigerr_("SPICE(BUG)", (ftnlen)10);
	chkout_("GFEVNT", (ftnlen)6);
	return 0;
    }
    chkout_("GFEVNT", (ftnlen)6);
    return 0;
} /* gfevnt_ */

