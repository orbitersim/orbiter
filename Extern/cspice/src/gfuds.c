/* gfuds.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__5 = 5;
static integer c_n1 = -1;
static integer c__3 = 3;
static integer c__0 = 0;
static logical c_false = FALSE_;

/* $Procedure GFUDS ( GF, user defined scalar ) */
/* Subroutine */ int gfuds_(U_fp udfuns, U_fp udqdec, char *relate, 
	doublereal *refval, doublereal *adjust, doublereal *step, doublereal *
	cnfine, integer *mw, integer *nw, doublereal *work, doublereal *
	result, ftnlen relate_len)
{
    /* System generated locals */
    integer work_dim1, work_offset, i__1;

    /* Local variables */
    extern /* Subroutine */ int zzgfudlt_();
    extern /* Subroutine */ int zzgfrelx_(U_fp, U_fp, U_fp, U_fp, U_fp, char *
	    , doublereal *, doublereal *, doublereal *, doublereal *, integer 
	    *, integer *, doublereal *, logical *, U_fp, U_fp, U_fp, char *, 
	    char *, logical *, L_fp, doublereal *, ftnlen, ftnlen, ftnlen), 
	    chkin_(char *, ftnlen);
    extern integer sized_(doublereal *);
    extern logical gfbail_();
    logical ok;
    extern /* Subroutine */ int scardd_(integer *, doublereal *);
    extern /* Subroutine */ int gfrefn_(), gfrepf_();
    extern logical return_(void);
    extern /* Subroutine */ int gfrepi_(), gfrepu_(), gfstep_();
    char rptpre[1*2], rptsuf[1*2];
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen), sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), gfsstp_(doublereal *);
    extern logical odd_(integer *);
    doublereal tol;
    extern /* Subroutine */ int zzholdd_(integer *, integer *, logical *, 
	    doublereal *);

/* $ Abstract */

/*     Perform a GF search on a user defined scalar quantity. */

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
/*     WINDOWS */

/* $ Keywords */

/*     EPHEMERIS */
/*     EVENT */
/*     SEARCH */
/*     WINDOW */

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

/*     SPICE private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     This file contains parameter declarations for the ZZHOLDD */
/*     routine. */

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

/*     None. */

/* $ Declarations */

/*     None. */

/* $ Brief_I/O */

/*     None. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     GEN       general value, primarily for testing. */

/*     GF_REF    user defined GF reference value. */

/*     GF_TOL    user defined GF convergence tolerance. */

/*     GF_DT     user defined GF step for numeric differentiation. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     E.D. Wright    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0  03-DEC-2013 (EDW) */

/* -& */

/*     OP codes. The values exist in the integer domain */
/*     [ -ZZNOP, -1], */


/*     Current number of OP codes. */


/*     ID codes. The values exist in the integer domain */
/*     [ 1, NID], */


/*     General use, primarily testing. */


/*     The user defined GF reference value. */


/*     The user defined GF convergence tolerance. */


/*     The user defined GF step for numeric differentiation. */


/*     Current number of ID codes, dimension of array */
/*     in ZZHOLDD. Bad things can happen if this parameter */
/*     does not have the proper value. */


/*     End of file zzholdd.inc. */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     LBCELL     P   SPICE Cell lower bound. */
/*     CNVTOL     P   Convergence tolerance. */
/*     UDFUNS     I   Name of the routine that computes a scalar */
/*                    quantity corresponding to an ET. */
/*     UDQDEC     I   Name of the routine that computes whether the */
/*                    scalar quantity is decreasing. */
/*     RELATE     I   Operator that either looks for an extreme value */
/*                    (max, min, local, absolute) or compares the */
/*                    geometric quantity value and a number. */
/*     REFVAL     I   Value used as reference for scalar quantity */
/*                    condition. */
/*     ADJUST     I   Allowed variation for absolute extremal */
/*                    geometric conditions. */
/*     STEP       I   Step size used for locating extrema and roots. */
/*     CNFINE     I   SPICE window to which the search is confined. */
/*     MW         I   Size of workspace windows. */
/*     NW         I   Number of workspace windows. */
/*     WORK       O   Array containing workspace windows. */
/*     RESULT    I-O  SPICE window containing results. */

/* $ Detailed_Input */

/*     UDFUNS   is the routine that returns the value of the scalar */
/*              quantity of interest at time ET. The calling sequence for */
/*              UDFUNS is: */

/*                 CALL UDFUNS ( ET, VALUE ) */

/*              where: */

/*                 ET      is a double precision value representing */
/*                         ephemeris time, expressed as seconds past */
/*                         J2000 TDB, at which to determine the scalar */
/*                         value. */

/*                 VALUE   is the value of the scalar quantity at ET. */

/*     UDQDEC   is the name of the routine that determines if the scalar */
/*              quantity calculated by UDFUNS is decreasing. The calling */
/*              sequence of UDQDEC is: */

/*                 CALL UDQDEC ( UDFUNS, ET, ISDECR ) */

/*              where: */

/*                 UDFUNS   is the name of the scalar function as defined */
/*                          above. */

/*                 ET       is a double precision value representing */
/*                          ephemeris time, expressed as seconds past */
/*                          J2000 TDB, at which to determine the time */
/*                          derivative of UDFUNS. */

/*                 ISDECR   is a logical output variable indicating */
/*                          whether or not the scalar value returned by */
/*                          UDFUNS is decreasing. ISDECR returns .TRUE. */
/*                          if the time derivative of UDFUNS at ET is */
/*                          negative. */

/*     RELATE   is the scalar string comparison operator indicating */
/*              the numeric constraint of interest. Values are: */

/*                 '>'        value of scalar quantity greater than some */
/*                            reference (REFVAL). */

/*                 '='        value of scalar quantity equal to some */
/*                            reference (REFVAL). */

/*                 '<'        value of scalar quantity less than some */
/*                            reference (REFVAL). */

/*                 'ABSMAX'   The scalar quantity is at an absolute */
/*                            maximum. */

/*                 'ABSMIN'   The scalar quantity is at an absolute */
/*                            minimum. */

/*                 'LOCMAX'   The scalar quantity is at a local */
/*                            maximum. */

/*                 'LOCMIN'   The scalar quantity is at a local */
/*                            minimum. */

/*              The caller may indicate that the region of interest */
/*              is the set of time intervals where the quantity is */
/*              within a specified distance of an absolute extremum. */
/*              The argument ADJUST (described below) is used to */
/*              specified this distance. */

/*              Local extrema are considered to exist only in the */
/*              interiors of the intervals comprising the confinement */
/*              window:  a local extremum cannot exist at a boundary */
/*              point of the confinement window. */

/*              RELATE is insensitive to case, leading and */
/*              trailing blanks. */

/*     REFVAL   is the reference value used to define an equality or */
/*              inequality to  satisfied by the scalar quantity. */
/*              The units of REFVAL are those of the scalar quantity. */

/*     ADJUST   is the amount by which the quantity is allowed to vary */
/*              from an absolute extremum. */

/*              If the search is for an absolute minimum is performed, */
/*              the resulting window contains time intervals when the */
/*              geometric quantity value has values between ABSMIN and */
/*              ABSMIN + ADJUST. */

/*              If the search is for an absolute maximum, the */
/*              corresponding range is between ABSMAX - ADJUST and */
/*              ABSMAX. */

/*              ADJUST is not used for searches for local extrema, */
/*              equality or inequality conditions and must have value */
/*              zero for such searches. */

/*     STEP     is the double precision time step size to use in */
/*              the search. */

/*              STEP must be short enough to for a search using this */
/*              step size to locate the time intervals where the */
/*              scalar quantity function is monotone increasing or */
/*              decreasing. However, STEP must not be *too* short, */
/*              or the search will take an unreasonable amount of time. */

/*              The choice of STEP affects the completeness but not */
/*              the precision of solutions found by this routine; the */
/*              precision is controlled by the convergence tolerance. */
/*              See the discussion of the parameter CNVTOL for */
/*              details. */

/*              STEP has units of TDB seconds. */

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

/*              Certain computations can expand the time window over */
/*              which UDFUNS and UDQDEC require data. See $Particulars */
/*              for details. */

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

/*              NW must be at least as large as the parameter NWUDS. */

/*     RESULT   is a double precision SPICE window which will contain */
/*              the search results. RESULT must be declared and */
/*              initialized with sufficient size to capture the full */
/*              set of time intervals within the search region on which */
/*              the specified condition is satisfied. */

/*              RESULT must be initialized by the caller via the */
/*              SPICELIB routine SSIZED. */

/*              If RESULT is non-empty on input, its contents will be */
/*              discarded before GFUDS conducts its search. */

/* $ Detailed_Output */

/*     WORK     is an array used to store workspace windows. */

/*              This array should be declared by the caller as shown: */

/*                  DOUBLE PRECISION     WORK ( LBCELL : MW,  NW ) */

/*              WORK need not be initialized by the caller. */

/*              WORK is modified by this routine. The caller should */
/*              re-initialize this array before attempting to use it for */
/*              any other purpose. */

/*     RESULT   is a SPICE window containing the time intervals within */
/*              the confinement window, during which the specified */
/*              condition on the scalar quantity is met. */

/*              The endpoints of the time intervals comprising RESULT are */
/*              interpreted as seconds past J2000 TDB. */

/*              If the search is for local extrema, or for absolute */
/*              extrema with ADJUST set to zero, then normally each */
/*              interval of RESULT will be a singleton: the left and */
/*              right endpoints of each interval will be identical. */

/*              If no times within the confinement window satisfy the */
/*              search criteria, RESULT will be returned with a */
/*              cardinality of zero. */

/* $ Parameters */

/*     LBCELL   is the integer value defining the lower bound for */
/*              SPICE Cell arrays (a SPICE window is a kind of cell). */

/*     CNVTOL   is the convergence tolerance used for finding */
/*              endpoints of the intervals comprising the result */
/*              window. CNVTOL is also used for finding intermediate */
/*              results; in particular, CNVTOL is used for finding the */
/*              windows on which the range rate is increasing */
/*              or decreasing. CNVTOL is used to determine when binary */
/*              searches for roots should terminate: when a root is */
/*              bracketed within an interval of length CNVTOL; the */
/*              root is considered to have been found. */

/*              The accuracy, as opposed to precision, of roots found */
/*              by this routine depends on the accuracy of the input */
/*              data. In most cases, the accuracy of solutions will be */
/*              inferior to their precision. */

/*     See INCLUDE file gf.inc for declarations and descriptions of */
/*     parameters used throughout the GF system. */

/* $ Exceptions */

/*     1)  In order for this routine to produce correct results, */
/*         the step size must be appropriate for the problem at hand. */
/*         Step sizes that are too large may cause this routine to miss */
/*         roots; step sizes that are too small may cause this routine */
/*         to run unacceptably slowly and in some cases, find spurious */
/*         roots. */

/*         This routine does not diagnose invalid step sizes, except that */
/*         if the step size is non-positive, an error is signaled by a */
/*         routine in the call tree of this routine. */

/*     2)  Due to numerical errors, in particular, */

/*            - truncation error in time values */
/*            - finite tolerance value */
/*            - errors in computed geometric quantities */

/*         it is *normal* for the condition of interest to not always be */
/*         satisfied near the endpoints of the intervals comprising the */
/*         RESULT window. One technique to handle such a situation, */
/*         slightly contract RESULT using the window routine WNCOND. */

/*     3)  If the workspace window size MW is less than 2 or not an even */
/*         value, the error SPICE(INVALIDDIMENSION) is signaled. */

/*     4)  If the number of workspace windows NW is too small for the */
/*         required search, an error is signaled by a routine in the call */
/*         tree of this routine. */

/*     5)  If the size of the SPICE window RESULT is less than 2 or not */
/*         an even value, the error SPICE(INVALIDDIMENSION) is signaled. */

/*     6)  If RESULT has insufficient capacity to contain the */
/*         number of intervals on which the specified condition */
/*         is met, an error is signaled by a routine in the call */
/*         tree of this routine. */

/*     7)  If the window count NW is less than NWUDS, the error */
/*         SPICE(INVALIDDIMENSION) is signaled. */

/*     8)  If an error (typically cell overflow) occurs during */
/*         window arithmetic, the error is signaled by a routine */
/*         in the call tree of this routine. */

/*     9)  If the relational operator RELATE is not recognized, an */
/*         error is signaled by a routine in the call tree of this */
/*         routine. */

/*     10) If ADJUST is negative, an error is signaled by a routine in */
/*         the call tree of this routine. */

/*     11) If a non-zero value is provided for ADJUST when RELATE has any */
/*         value other than 'ABSMIN' or 'ABSMAX', an error is signaled by */
/*         a routine in the call tree of this routine. */

/*     12) If required ephemerides or other kernel data are not */
/*         available, an error is signaled by a routine in the call tree */
/*         of this routine. */

/* $ Files */

/*     Appropriate kernels must be loaded by the calling program before */
/*     this routine is called. */

/*     If the scalar function requires access to ephemeris data: */

/*     -  SPK data: ephemeris data for any body over the */
/*        time period defined by the confinement window must be */
/*        loaded. If aberration corrections are used, the states of */
/*        target and observer relative to the solar system barycenter */
/*        must be calculable from the available ephemeris data. */
/*        Typically ephemeris data are made available by loading one */
/*        or more SPK files via FURNSH. */

/*     -  If non-inertial reference frames are used, then PCK */
/*        files, frame kernels, C-kernels, and SCLK kernels may be */
/*        needed. */

/*     -  Certain computations can expand the time window over which */
/*        UDFUNS and UDQDEC require data; such data must be provided by */
/*        loaded kernels. See $Particulars for details. */

/*     In all cases, kernel data are normally loaded once per program */
/*     run, NOT every time this routine is called. */

/* $ Particulars */

/*     This routine determines a set of one or more time intervals */
/*     within the confinement window when the scalar function */
/*     satisfies a caller-specified constraint. The resulting set of */
/*     intervals is returned as a SPICE window. */

/*     UDQDEC Default Template */
/*     ======================= */

/*     The user must supply a routine to determine whether sign of the */
/*     time derivative of UDFUNS is positive or negative at ET. For */
/*     cases where UDFUNS is numerically well behaved, the user */
/*     may find it convenient to use a routine based on the below */
/*     template. UDDC determines the truth of the expression */

/*        d (UDFUNS) */
/*        --         < 0 */
/*        dt */

/*     using the library routine UDDF to numerically calculate the */
/*     derivative of UDFUNS using a three-point estimation. */
/*     Please see the $Examples section for an example of GFDECR use. */

/*           SUBROUTINE GFDECR ( UDFUNS, ET, ISDECR ) */
/*           IMPLICIT NONE */

/*           EXTERNAL              UDFUNS */
/*           EXTERNAL              UDDF */

/*           DOUBLE PRECISION      ET */
/*           LOGICAL               ISDECR */

/*           DOUBLE PRECISION      DT */

/*           DT =  h, double precision interval size */

/*           CALL UDDC ( UDFUNS, ET, DT, ISDECR ) */

/*           END */

/*     The Search Process */
/*     ================== */

/*     Regardless of the type of constraint selected by the caller, this */
/*     routine starts the search for solutions by determining the time */
/*     periods, within the confinement window, over which the specified */
/*     scalar function is monotone increasing and monotone */
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
/*     "convergence tolerance." The default convergence tolerance */
/*     used by this routine is set by the parameter CNVTOL (defined */
/*     in gf.inc). */

/*     The value of CNVTOL is set to a "tight" value so that the */
/*     tolerance doesn't become the limiting factor in the accuracy of */
/*     solutions found by this routine. In general the accuracy of input */
/*     data will be the limiting factor. */

/*     The user may change the convergence tolerance from the default */
/*     CNVTOL value by calling the routine GFSTOL, e.g. */

/*        CALL GFSTOL( tolerance value ) */

/*     Call GFSTOL prior to calling this routine. All subsequent */
/*     searches will use the updated tolerance value. */

/*     Setting the tolerance tighter than CNVTOL is unlikely to be */
/*     useful, since the results are unlikely to be more accurate. */
/*     Making the tolerance looser will speed up searches somewhat, */
/*     since a few convergence steps will be omitted. However, in most */
/*     cases, the step size is likely to have a much greater effect */
/*     on processing time than would the convergence tolerance. */


/*     The Confinement Window */
/*     ====================== */

/*     The simplest use of the confinement window is to specify a time */
/*     interval within which a solution is sought. However, the */
/*     confinement window can, in some cases, be used to make searches */
/*     more efficient. Sometimes it's possible to do an efficient search */
/*     to reduce the size of the time period over which a relatively */
/*     slow search of interest must be performed. */

/*     Certain user-defined computations may expand the window over */
/*     which computations are performed. Here "expansion" of a window by */
/*     an amount "T" means that the left endpoint of each interval */
/*     comprising the window is shifted left by T, the right endpoint of */
/*     each interval is shifted right by T, and any overlapping */
/*     intervals are merged. Note that the input window CNFINE itself is */
/*     not modified. */

/*     If a search uses an equality constraint, the time window over */
/*     which the functions UDFUNS and UDQDEC are called is expanded by 1 */
/*     second. */

/*     Computation of observer-target states by SPKEZR or SPKEZ, using */
/*     stellar aberration corrections, requires the state of the */
/*     observer, relative to the solar system barycenter, to be computed */
/*     at times offset from the input time by +/- 1 second. If the input */
/*     time ET is used by UDFUNS or UDQDEC to compute such a state, the */
/*     window over which the observer state is computed is expanded by */
/*     1 second. */

/*     The window expansions described above are additive: if both */
/*     conditions apply, the window expansion amount is the sum of the */
/*     individual amounts. */

/*     When light time corrections are used in the computation of */
/*     observer-target states, expansion of the search window also */
/*     affects the set of times at which the light time-corrected states */
/*     of the targets are computed. */

/*     In addition to the possible expansion of the search window that */
/*     occurs when both an equality constraint and stellar aberration */
/*     corrections are used, round-off error should be taken into */
/*     account when the need for data availability is analyzed. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Conduct a search on the range-rate of the vector from the Sun */
/*        to the Moon. Define a function to calculate the value. */

/*        Use the meta-kernel shown below to load the required SPICE */
/*        kernels. */


/*           KPL/MK */

/*           File name: gfuds_ex1.tm */

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


/*              PROGRAM GFUDS_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Include GF parameter declarations: */
/*        C */
/*              INCLUDE 'gf.inc' */

/*        C */
/*        C     User defined external routines */
/*        C */
/*              EXTERNAL     GFQ */
/*              EXTERNAL     GFDECR */

/*        C */
/*        C     SPICELIB functions */
/*        C */
/*              DOUBLE PRECISION      SPD */
/*              DOUBLE PRECISION      DVNORM */
/*              INTEGER               WNCARD */

/*        C */
/*        C     Local parameters */
/*        C */
/*              INTEGER               LBCELL */
/*              PARAMETER           ( LBCELL = -5 ) */

/*        C */
/*        C     Use the parameter MAXWIN for both the result window size */
/*        C     and the workspace size. */
/*        C */
/*              INTEGER               MAXWIN */
/*              PARAMETER           ( MAXWIN = 20000 ) */

/*        C */
/*        C     Length of strings: */
/*        C */
/*              INTEGER               TIMLEN */
/*              PARAMETER           ( TIMLEN = 26 ) */

/*              INTEGER               NLOOPS */
/*              PARAMETER           ( NLOOPS = 7 ) */

/*        C */
/*        C     Local variables */
/*        C */
/*              CHARACTER*(TIMLEN)    TIMSTR */
/*              CHARACTER*(TIMLEN)    RELATE (NLOOPS) */

/*              DOUBLE PRECISION      ADJUST */
/*              DOUBLE PRECISION      CNFINE ( LBCELL : 2 ) */
/*              DOUBLE PRECISION      DRDT */
/*              DOUBLE PRECISION      ET0 */
/*              DOUBLE PRECISION      ET1 */
/*              DOUBLE PRECISION      FINISH */
/*              DOUBLE PRECISION      LT */
/*              DOUBLE PRECISION      POS    ( 6 ) */
/*              DOUBLE PRECISION      REFVAL */
/*              DOUBLE PRECISION      RESULT ( LBCELL : MAXWIN ) */
/*              DOUBLE PRECISION      START */
/*              DOUBLE PRECISION      STEP */
/*              DOUBLE PRECISION      WORK   ( LBCELL : MAXWIN, NWUDS ) */

/*              INTEGER               I */
/*              INTEGER               J */

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

/*              DATA                  RELATE / '=', */
/*             .                               '<', */
/*             .                               '>', */
/*             .                               'LOCMIN', */
/*             .                               'ABSMIN', */
/*             .                               'LOCMAX', */
/*             .                               'ABSMAX'  / */

/*        C */
/*        C     Load kernels. */
/*        C */
/*              CALL FURNSH ( 'gfuds_ex1.tm' ) */

/*        C */
/*        C     Initialize windows. */
/*        C */
/*              CALL SSIZED ( MAXWIN, RESULT ) */
/*              CALL SSIZED ( 2,      CNFINE ) */

/*              CALL SCARDD ( 0,      CNFINE ) */

/*        C */
/*        C     Store the time bounds of our search interval in */
/*        C     the confinement window. */
/*        C */
/*              CALL STR2ET ( '2007 JAN 1', ET0 ) */
/*              CALL STR2ET ( '2007 APR 1', ET1 ) */

/*              CALL WNINSD ( ET0, ET1, CNFINE ) */

/*        C */
/*        C     Search using a step size of 1 day (in units of seconds). */
/*        C     The reference value is .3365 km/s - a range rate value */
/*        C     known to exist during the confinement window. We're not */
/*        C     using the adjustment feature, so we set ADJUST to zero. */
/*        C */
/*              STEP   = SPD() */
/*              REFVAL = .3365D0 */
/*              ADJUST = 0.D0 */

/*              DO J=1, NLOOPS */

/*                 WRITE(*,*) 'Relation condition: ', RELATE(J) */

/*        C */
/*        C        Perform the search. The SPICE window RESULT contains */
/*        C        the set of times when the condition is met. */
/*        C */
/*                 CALL GFUDS ( GFQ,    GFDECR, RELATE(J), */
/*             .                REFVAL, ADJUST,      STEP, CNFINE, */
/*             .                MAXWIN,  NWUDS,      WORK, RESULT ) */


/*        C */
/*        C        Display the results. */
/*        C */
/*                 IF ( WNCARD(RESULT) .EQ. 0 ) THEN */

/*                    WRITE (*, '(A)') 'Result window is empty.' */

/*                 ELSE */

/*                    DO I = 1, WNCARD(RESULT) */

/*        C */
/*        C              Fetch the endpoints of the Ith interval */
/*        C              of the result window. */
/*        C */
/*                       CALL WNFETD ( RESULT, I, START, FINISH ) */

/*                       CALL SPKEZR ( 'MOON',  START, 'J2000', 'NONE', */
/*             .                       'SUN', POS,   LT               ) */
/*                       DRDT = DVNORM(POS) */

/*                       CALL TIMOUT ( START, */
/*             .                       'YYYY-MON-DD HR:MN:SC.###', */
/*             .                       TIMSTR                     ) */

/*                       WRITE (*, '(A,F16.9)' ) 'Start time, drdt = '// */
/*             .                                 TIMSTR, DRDT */

/*                       CALL SPKEZR ( 'MOON',  FINISH, 'J2000', 'NONE', */
/*             .                       'SUN', POS,     LT              ) */
/*                       DRDT = DVNORM(POS) */

/*                       CALL TIMOUT ( FINISH, */
/*             .                       'YYYY-MON-DD HR:MN:SC.###', */
/*             .                       TIMSTR                    ) */

/*                       WRITE (*, '(A,F16.9)' ) 'Stop time,  drdt = '// */
/*             .                              TIMSTR, DRDT */
/*                    END DO */

/*                 END IF */

/*                 WRITE(*,*) ' ' */

/*              END DO */

/*              END */



/*        C-Procedure GFQ */

/*              SUBROUTINE GFQ ( ET, VALUE ) */
/*              IMPLICIT NONE */

/*        C- Abstract */
/*        C */
/*        C     User defined geometric quantity function. In this case, */
/*        C     the range from the sun to the Moon at TDB time ET. */
/*        C */

/*              DOUBLE PRECISION      ET */
/*              DOUBLE PRECISION      VALUE */

/*        C */
/*        C     Local variables. */
/*        C */
/*              INTEGER               TARG */
/*              INTEGER               OBS */

/*              CHARACTER*(12)        REF */
/*              CHARACTER*(12)        ABCORR */

/*              DOUBLE PRECISION      STATE ( 6 ) */
/*              DOUBLE PRECISION      LT */
/*              DOUBLE PRECISION      DVNORM */

/*        C */
/*        C     Initialization. Retrieve the vector from the Sun to */
/*        C     the Moon in the J2000 frame, without aberration */
/*        C     correction. */
/*        C */
/*              TARG   = 301 */
/*              REF    = 'J2000' */
/*              ABCORR = 'NONE' */
/*              OBS    = 10 */

/*              CALL SPKEZ ( TARG, ET, REF, ABCORR, OBS, STATE, LT ) */

/*        C */
/*        C     Calculate the scalar range rate corresponding the */
/*        C     STATE vector. */
/*        C */
/*              VALUE = DVNORM( STATE ) */

/*              END */




/*        C-Procedure GFDECR */

/*              SUBROUTINE GFDECR ( UDFUNS, ET, ISDECR ) */
/*              IMPLICIT NONE */

/*        C- Abstract */
/*        C */
/*        C     User defined function to detect if the function */
/*        C     derivative is negative (the function is decreasing) */
/*        C     at TDB time ET. */
/*        C */

/*              EXTERNAL              UDFUNS */
/*              EXTERNAL              UDDF */

/*              DOUBLE PRECISION      ET */
/*              LOGICAL               ISDECR */

/*              DOUBLE PRECISION      DT */

/*              DT = 1.D0 */

/*        C */
/*        C     Determine if GFQ is decreasing at ET. */
/*        C */
/*        C     UDDC - the default GF function to determine if */
/*        C                the derivative of the user defined */
/*        C                function is negative at ET. */
/*        C */
/*        C     UDFUNS - the user defined scalar quantity function. */
/*        C */
/*              CALL UDDC ( UDFUNS, ET, DT, ISDECR ) */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Relation condition: = */
/*        Start time, drdt = 2007-JAN-02 00:35:19.574       0.336500000 */
/*        Stop time,  drdt = 2007-JAN-02 00:35:19.574       0.336500000 */
/*        Start time, drdt = 2007-JAN-19 22:04:54.899       0.336500000 */
/*        Stop time,  drdt = 2007-JAN-19 22:04:54.899       0.336500000 */
/*        Start time, drdt = 2007-FEB-01 23:30:13.428       0.336500000 */
/*        Stop time,  drdt = 2007-FEB-01 23:30:13.428       0.336500000 */
/*        Start time, drdt = 2007-FEB-17 11:10:46.540       0.336500000 */
/*        Stop time,  drdt = 2007-FEB-17 11:10:46.540       0.336500000 */
/*        Start time, drdt = 2007-MAR-04 15:50:19.929       0.336500000 */
/*        Stop time,  drdt = 2007-MAR-04 15:50:19.929       0.336500000 */
/*        Start time, drdt = 2007-MAR-18 09:59:05.959       0.336500000 */
/*        Stop time,  drdt = 2007-MAR-18 09:59:05.959       0.336500000 */

/*         Relation condition: < */
/*        Start time, drdt = 2007-JAN-02 00:35:19.574       0.336500000 */
/*        Stop time,  drdt = 2007-JAN-19 22:04:54.899       0.336500000 */
/*        Start time, drdt = 2007-FEB-01 23:30:13.428       0.336500000 */
/*        Stop time,  drdt = 2007-FEB-17 11:10:46.540       0.336500000 */
/*        Start time, drdt = 2007-MAR-04 15:50:19.929       0.336500000 */
/*        Stop time,  drdt = 2007-MAR-18 09:59:05.959       0.336500000 */

/*         Relation condition: > */
/*        Start time, drdt = 2007-JAN-01 00:00:00.000       0.515522367 */
/*        Stop time,  drdt = 2007-JAN-02 00:35:19.574       0.336500000 */
/*        Start time, drdt = 2007-JAN-19 22:04:54.899       0.336500000 */
/*        Stop time,  drdt = 2007-FEB-01 23:30:13.428       0.336500000 */
/*        Start time, drdt = 2007-FEB-17 11:10:46.540       0.336500000 */
/*        Stop time,  drdt = 2007-MAR-04 15:50:19.929       0.336500000 */
/*        Start time, drdt = 2007-MAR-18 09:59:05.959       0.336500000 */
/*        Stop time,  drdt = 2007-APR-01 00:00:00.000       0.793546222 */

/*         Relation condition: LOCMIN */
/*        Start time, drdt = 2007-JAN-11 07:03:58.988      -0.803382743 */
/*        Stop time,  drdt = 2007-JAN-11 07:03:58.988      -0.803382743 */
/*        Start time, drdt = 2007-FEB-10 06:26:15.438      -0.575837623 */
/*        Stop time,  drdt = 2007-FEB-10 06:26:15.438      -0.575837623 */
/*        Start time, drdt = 2007-MAR-12 03:28:36.404      -0.441800446 */
/*        Stop time,  drdt = 2007-MAR-12 03:28:36.404      -0.441800446 */

/*         Relation condition: ABSMIN */
/*        Start time, drdt = 2007-JAN-11 07:03:58.988      -0.803382743 */
/*        Stop time,  drdt = 2007-JAN-11 07:03:58.988      -0.803382743 */

/*         Relation condition: LOCMAX */
/*        Start time, drdt = 2007-JAN-26 02:27:33.767       1.154648992 */
/*        Stop time,  drdt = 2007-JAN-26 02:27:33.767       1.154648992 */
/*        Start time, drdt = 2007-FEB-24 09:35:07.816       1.347132236 */
/*        Stop time,  drdt = 2007-FEB-24 09:35:07.816       1.347132236 */
/*        Start time, drdt = 2007-MAR-25 17:26:56.150       1.428141707 */
/*        Stop time,  drdt = 2007-MAR-25 17:26:56.150       1.428141707 */

/*         Relation condition: ABSMAX */
/*        Start time, drdt = 2007-MAR-25 17:26:56.150       1.428141707 */
/*        Stop time,  drdt = 2007-MAR-25 17:26:56.150       1.428141707 */


/* $ Restrictions */

/*     1)  Any kernel files required by this routine must be loaded */
/*         (normally via the SPICELIB routine FURNSH) before this routine */
/*         is called. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.1, 21-OCT-2021 (JDR) (NJB) */

/*        Edited the header to comply with NAIF standard. */

/*        Updated description of WORK and RESULT arguments in $Brief_I/O, */
/*        $Detailed_Input and $Detailed_Output. */

/*        Added SAVE statements for CNFINE, WORK and RESULT variables in */
/*        code example. */

/*        Updated header to describe use of expanded confinement window. */

/* -    SPICELIB Version 1.1.0, 15-JUL-2014 (EDW) */

/*        Correction to description of UDQDEC to show UDFUNS as */
/*        an argument. */

/*        Edit to comments to correct search description. */

/*        Implemented use of ZZHOLDD to allow user to alter convergence */
/*        tolerance. */

/*        Removed the STEP > 0 error check. The GFSSTP call includes */
/*        the check. */

/*        Removed ZZGFREF call. That call now occurs in ZZGFRELX. Update */
/*        to ZZGFRELX argument list to reflect this change in */
/*        functionality. */

/*        Added RETURN() check. */

/* -    SPICELIB Version 1.0.0, 16-FEB-2010 (EDW) (NJB) */

/* -& */
/* $ Index_Entries */

/*     GF user defined scalar function search */

/* -& */

/*     SPICELIB functions. */


/*     Local variables. */


/*     Dummy variables. */


/*     Standard SPICE error handling. */

    /* Parameter adjustments */
    work_dim1 = *mw + 6;
    work_offset = work_dim1 - 5;

    /* Function Body */
    if (return_()) {
	return 0;
    }
    chkin_("GFUDS", (ftnlen)5);

/*     Confirm minimum number of windows. */

    if (*nw < 5) {
	setmsg_("Workspace window count was #; count must be at least #.", (
		ftnlen)55);
	errint_("#", nw, (ftnlen)1);
	errint_("#", &c__5, (ftnlen)1);
	sigerr_("SPICE(INVALIDDIMENSION)", (ftnlen)23);
	chkout_("GFUDS", (ftnlen)5);
	return 0;
    }

/*     Confirm minimum window sizes. */

    if (*mw < 2 || odd_(mw)) {
	setmsg_("Workspace window size was #; size must be at least 2 and an"
		" even value.", (ftnlen)71);
	errint_("#", mw, (ftnlen)1);
	sigerr_("SPICE(INVALIDDIMENSION)", (ftnlen)23);
	chkout_("GFUDS", (ftnlen)5);
	return 0;
    }

/*     Check the result window size. */

    i__1 = sized_(result);
    if (sized_(result) < 2 || odd_(&i__1)) {
	setmsg_("Result window size was #; size must be at least 2 and an ev"
		"en value.", (ftnlen)68);
	i__1 = sized_(result);
	errint_("#", &i__1, (ftnlen)1);
	sigerr_("SPICE(INVALIDDIMENSION)", (ftnlen)23);
	chkout_("GFUDS", (ftnlen)5);
	return 0;
    }

/*     Set the step size. */

    gfsstp_(step);

/*     Retrieve the convergence tolerance, if set. */

    zzholdd_(&c_n1, &c__3, &ok, &tol);

/*     Use the default value CNVTOL if no stored tolerance value. */

    if (! ok) {
	tol = 1e-6;
    }

/*     Initialize the RESULT window to empty. */

    scardd_(&c__0, result);

/*     Call ZZGFRELX to do the event detection work. */

    zzgfrelx_((U_fp)gfstep_, (U_fp)gfrefn_, (U_fp)udqdec, (U_fp)zzgfudlt_, (
	    U_fp)udfuns, relate, refval, &tol, adjust, cnfine, mw, nw, work, &
	    c_false, (U_fp)gfrepi_, (U_fp)gfrepu_, (U_fp)gfrepf_, rptpre, 
	    rptsuf, &c_false, (L_fp)gfbail_, result, relate_len, (ftnlen)1, (
	    ftnlen)1);
    chkout_("GFUDS", (ftnlen)5);
    return 0;
} /* gfuds_ */

