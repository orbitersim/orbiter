/* trgsep.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure TRGSEP ( Separation quantity from observer ) */
doublereal trgsep_(doublereal *et, char *targ1, char *shape1, char *frame1, 
	char *targ2, char *shape2, char *frame2, char *obsrvr, char *abcorr, 
	ftnlen targ1_len, ftnlen shape1_len, ftnlen frame1_len, ftnlen 
	targ2_len, ftnlen shape2_len, ftnlen frame2_len, ftnlen obsrvr_len, 
	ftnlen abcorr_len)
{
    /* System generated locals */
    doublereal ret_val;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern logical failed_(void), return_(void);
    extern doublereal zzsepq_(doublereal *, integer *, integer *, doublereal *
	    , doublereal *, integer *, char *, char *, ftnlen, ftnlen);
    integer frames[2];
    extern /* Subroutine */ int zzspin_(char *, char *, char *, char *, char *
	    , char *, char *, char *, integer *, integer *, doublereal *, 
	    integer *, ftnlen, ftnlen, ftnlen, ftnlen, ftnlen, ftnlen, ftnlen,
	     ftnlen), chkout_(char *, ftnlen);
    integer bod[2];
    doublereal rad[2];
    char ref[5];
    integer obs;

/* $ Abstract */

/*     Compute the angular separation in radians between two spherical */
/*     or point objects. */

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

/*     ABCORR */

/* $ Keywords */

/*     ANGLE */
/*     GEOMETRY */

/* $ Declarations */
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

/*     Include file zzdyn.inc */

/*     SPICE private file intended solely for the support of SPICE */
/*     routines.  Users should not include this file directly due */
/*     to the volatile nature of this file */

/*     The parameters defined below are used by the SPICELIB dynamic */
/*     frame subsystem. */

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

/*     This file declares parameters required by the dynamic */
/*     frame routines of the SPICELIB frame subsystem. */

/* $ Restrictions */

/*     The parameter BDNMLN is this routine must be kept */
/*     consistent with the parameter MAXL defined in */

/*        zzbodtrn.inc */


/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 2.0.0, 22-SEP-2020 (NJB) */

/*        Updated to support the product frame family. */

/* -    SPICELIB Version 1.1.0, 12-JAN-2005 (NJB) */

/*        Parameters KWX, KWY, KWZ renamed to KVX, KVY, KVZ. */

/* -    SPICELIB Version 1.0.0, 22-DEC-2004 (NJB) */

/* -& */

/*     String length parameters */
/*     ======================== */


/*     Kernel variable name length.  This parameter must be */
/*     kept consistent with the parameter MAXLEN used in the */
/*     POOL umbrella routine. */


/*     Length of a character kernel pool datum. This parameter must be */
/*     kept consistent with the parameter MAXCHR used in the POOL */
/*     umbrella routine. */


/*     Reference frame name length.  This parameter must be */
/*     kept consistent with the parameter WDSIZE used in the */
/*     FRAMEX umbrella routine. */


/*     Body name length.  This parameter is used to provide a level */
/*     of indirection so the dynamic frame source code doesn't */
/*     have to change if the name of this SPICELIB-scope parameter */
/*     is changed.  The value MAXL used here is defined in the */
/*     INCLUDE file */

/*        zzbodtrn.inc */

/*     Current value of MAXL = 36 */


/*     Numeric parameters */
/*     =================================== */

/*     The parameter MAXCOF is the maximum number of polynomial */
/*     coefficients that may be used to define an Euler angle */
/*     in an "Euler frame" definition */


/*     The parameter MXNFAC is the maximum number of factors in */
/*     a product frame. */


/*     The parameter LBSEP is the default angular separation limit for */
/*     the vectors defining a two-vector frame.  The angular separation */
/*     of the vectors must differ from Pi and 0 by at least this amount. */


/*     The parameter QEXP is used to determine the width of */
/*     the interval DELTA used for the discrete differentiation */
/*     of velocity in the routines ZZDYNFRM, ZZDYNROT, and their */
/*     recursive analogs.  This parameter is appropriate for */
/*     64-bit IEEE double precision numbers; when SPICELIB */
/*     is hosted on platforms where longer mantissas are supported, */
/*     this parameter (and hence this INCLUDE file) will become */
/*     platform-dependent. */

/*     The choice of QEXP is based on heuristics.  It's believed to */
/*     be a reasonable choice obtainable without expensive computation. */

/*     QEXP is the largest power of 2 such that */

/*        1.D0 + 2**QEXP  =  1.D0 */

/*     Given an epoch T0 at which a discrete derivative is to be */
/*     computed, this choice provides a value of DELTA that usually */
/*     contributes no round-off error in the computation of the function */
/*     evaluation epochs */

/*        T0 +/- DELTA */

/*     while providing the largest value of DELTA having this form that */
/*     causes the order of the error term O(DELTA**2) in the quadratic */
/*     function approximation to round to zero.  Note that the error */
/*     itself will normally be small but doesn't necessarily round to */
/*     zero.  Note also that the small function approximation error */
/*     is not a measurement of the error in the discrete derivative */
/*     itself. */

/*     For ET values T0 > 2**27 seconds past J2000, the value of */
/*     DELTA will be set to */

/*        T0 * 2**QEXP */

/*     For smaller values of T0, DELTA should be set to 1.D0. */


/*     Frame kernel parameters */
/*     ======================= */

/*     Parameters relating to kernel variable names (keywords) start */
/*     with the letters */

/*        KW */

/*     Parameters relating to kernel variable values start with the */
/*     letters */

/*        KV */


/*     Generic parameters */
/*     --------------------------------- */

/*     Token used to build the base frame keyword: */


/*     Frame definition style parameters */
/*     --------------------------------- */

/*     Token used to build the frame definition style keyword: */


/*     Token indicating parameterized dynamic frame. */


/*     Freeze epoch parameters */
/*     --------------------------------- */

/*     Token used to build the freeze epoch keyword: */


/*     Rotation state parameters */
/*     --------------------------------- */

/*     Token used to build the rotation state keyword: */


/*     Token indicating rotating rotation state: */


/*     Token indicating inertial rotation state: */


/*     Frame family parameters */
/*     --------------------------------- */

/*     Token used to build the frame family keyword: */


/*     Token indicating mean equator and equinox of date frame. */


/*     Token indicating mean ecliptic and equinox of date frame. */


/*     Token indicating true equator and equinox of date frame. */


/*     Token indicating two-vector frame. */


/*     Token indicating Euler frame. */


/*     Token indicating product frame. */


/*     "Of date" frame family parameters */
/*     --------------------------------- */

/*     Token used to build the precession model keyword: */


/*     Token used to build the nutation model keyword: */


/*     Token used to build the obliquity model keyword: */


/*     Mathematical models used to define "of date" frames will */
/*     likely accrue over time.  We will simply assign them */
/*     numbers. */


/*     Token indicating the Lieske earth precession model: */


/*     Token indicating the IAU 1980 earth nutation model: */


/*     Token indicating the IAU 1980 earth mean obliqity of */
/*     date model.  Note the name matches that of the preceding */
/*     nutation model---this is intentional.  The keyword */
/*     used in the kernel variable definition indicates what */
/*     kind of model is being defined. */


/*     Two-vector frame family parameters */
/*     --------------------------------- */

/*     Token used to build the vector axis keyword: */


/*     Tokens indicating axis values: */


/*     Prefixes used for primary and secondary vector definition */
/*     keywords: */


/*     Token used to build the vector definition keyword: */


/*     Token indicating observer-target position vector: */


/*     Token indicating observer-target velocity vector: */


/*     Token indicating observer-target near point vector: */


/*     Token indicating constant vector: */


/*     Token used to build the vector observer keyword: */


/*     Token used to build the vector target keyword: */


/*     Token used to build the vector frame keyword: */


/*     Token used to build the vector aberration correction keyword: */


/*     Token used to build the constant vector specification keyword: */


/*     Token indicating rectangular coordinates used to */
/*     specify constant vector: */


/*     Token indicating latitudinal coordinates used to */
/*     specify constant vector: */


/*     Token indicating RA/DEC coordinates used to */
/*     specify constant vector: */


/*     Token used to build the cartesian vector literal keyword: */


/*     Token used to build the constant vector latitude keyword: */


/*     Token used to build the constant vector longitude keyword: */


/*     Token used to build the constant vector right ascension keyword: */


/*     Token used to build the constant vector declination keyword: */


/*     Token used to build the angular separation tolerance keyword: */


/*     See the section "Physical unit parameters" below for additional */
/*     parameters applicable to two-vector frames. */


/*     Euler frame family parameters */
/*     --------------------------------- */

/*     Token used to build the epoch keyword: */


/*     Token used to build the Euler axis sequence keyword: */


/*     Tokens used to build the Euler angle coefficients keywords: */


/*     See the section "Physical unit parameters" below for additional */
/*     parameters applicable to Euler frames. */


/*     Product frame family parameters */
/*     --------------------------------- */


/*     Physical unit parameters */
/*     --------------------------------- */

/*     Token used to build the units keyword: */


/*     Token indicating radians: */


/*     Token indicating degrees: */


/*     End of include file zzdyn.inc */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     ET         I   Ephemeris seconds past J2000 TDB. */
/*     TARG1      I   First target body name. */
/*     SHAPE1     I   First target body shape. */
/*     FRAME1     I   Reference frame of first target. */
/*     TARG2      I   Second target body name. */
/*     SHAPE2     I   First target body shape. */
/*     FRAME2     I   Reference frame of second target. */
/*     OBSRVR     I   Observing body name. */
/*     ABCORR     I   Aberration corrections flag. */

/*     The function returns the angular separation between two targets, */
/*     TARG1 and TARG2, as seen from an observer OBSRVR, possibly */
/*     corrected for aberration corrections. */

/* $ Detailed_Input */

/*     ET       is the time in ephemeris seconds past J2000 TDB at */
/*              which the separation is to be measured. */

/*     TARG1    is the string naming the first body of interest. You can */
/*              also supply the integer ID code for the object as an */
/*              integer string. For example both 'MOON' and '301' */
/*              are legitimate strings that indicate the moon is the */
/*              target body. */

/*     SHAPE1   is the string naming the geometric model used to */
/*              represent the shape of the TARG1 body. Models */
/*              supported by this routine: */

/*                 'SPHERE'        Treat the body as a sphere with */
/*                                 radius equal to the maximum value of */
/*                                 BODYnnn_RADII. */

/*                 'POINT'         Treat the body as a point; */
/*                                 radius has value zero. */

/*              The SHAPE1 string lacks sensitivity to case, leading */
/*              and trailing blanks. */

/*     FRAME1   is the string naming the body-fixed reference frame */
/*              corresponding to TARG1. TRGSEP does not currently use */
/*              this argument's value, its use is reserved for future */
/*              shape models. The value 'NULL' will suffice for */
/*              'POINT' and 'SPHERE' shaped bodies. */

/*     TARG2    is the string naming the second body of interest. You can */
/*              also supply the integer ID code for the object as an */
/*              integer string. For example both 'MOON' and '301' */
/*              are legitimate strings that indicate the moon is the */
/*              target body. */

/*     SHAPE2   is the string naming the geometric model used to */
/*              represent the shape of the TARG2. Models supported by */
/*              this routine: */

/*                 'SPHERE'        Treat the body as a sphere with */
/*                                 radius equal to the maximum value of */
/*                                 BODYnnn_RADII. */

/*                 'POINT'         Treat the body as a single point; */
/*                                 radius has value zero. */

/*              The SHAPE2 string lacks sensitivity to case, leading */
/*              and trailing blanks. */

/*     FRAME2   is the string naming the body-fixed reference frame */
/*              corresponding to TARG2. TRGSEP does not currently use */
/*              this argument's value, its use is reserved for future */
/*              shape models. The value 'NULL' will suffice for */
/*              'POINT' and 'SPHERE' shaped bodies. */

/*     OBSRVR   is the string naming the observing body. Optionally, you */
/*              may supply the ID code of the object as an integer */
/*              string. For example, both 'EARTH' and '399' are */
/*              legitimate strings to supply to indicate the */
/*              observer is Earth. */

/*     ABCORR   is the string description of the aberration corrections */
/*              to apply to the state evaluations to account for */
/*              one-way light time and stellar aberration. */

/*              This routine accepts the same aberration corrections */
/*              as does the SPICE routine SPKEZR. See the header of */
/*              SPKEZR for a detailed description of the aberration */
/*              correction options. For convenience, the options are */
/*              listed below: */

/*                 'NONE'     Apply no correction. */

/*                 'LT'       "Reception" case: correct for */
/*                            one-way light time using a Newtonian */
/*                            formulation. */

/*                 'LT+S'     "Reception" case: correct for */
/*                            one-way light time and stellar */
/*                            aberration using a Newtonian */
/*                            formulation. */

/*                 'CN'       "Reception" case: converged */
/*                            Newtonian light time correction. */

/*                 'CN+S'     "Reception" case: converged */
/*                            Newtonian light time and stellar */
/*                            aberration corrections. */

/*                 'XLT'      "Transmission" case: correct for */
/*                            one-way light time using a Newtonian */
/*                            formulation. */

/*                 'XLT+S'    "Transmission" case: correct for */
/*                            one-way light time and stellar */
/*                            aberration using a Newtonian */
/*                            formulation. */

/*                 'XCN'      "Transmission" case: converged */
/*                            Newtonian light time correction. */

/*                 'XCN+S'    "Transmission" case: converged */
/*                            Newtonian light time and stellar */
/*                            aberration corrections. */

/*              The ABCORR string lacks sensitivity to case, leading */
/*              and trailing blanks. */

/* $ Detailed_Output */

/*     The function returns the angular separation between two targets, */
/*     TARG1 and TARG2, as seen from an observer OBSRVR expressed in */
/*     radians. */

/*     The observer is the angle's vertex. The angular separation between */
/*     the targets may be measured between the centers or figures (limbs) */
/*     of the targets, depending on whether the target shapes are modeled */
/*     as spheres or points. */

/*     If the target shape is either a spheroid or an ellipsoid, the */
/*     radius used to compute the limb will be the largest of the radii */
/*     of the target's tri-axial ellipsoid model. */

/*     If the targets are modeled as points the result ranges from 0 */
/*     to Pi radians or 180 degrees. */

/*     If the target shapes are modeled as spheres or ellipsoids, the */
/*     function returns a negative value when the bodies overlap */
/*     (occult). Note that in this situation the function returns 0 when */
/*     the limbs of the bodies start or finish the overlap. */

/*     The positions of the targets may optionally be corrected for light */
/*     time and stellar aberration. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the three objects TARG1, TARG2 and OBSRVR are not */
/*         distinct, an error is signaled by a routine in the call tree */
/*         of this routine. */

/*     2)  If the object names for TARG1, TARG2 or OBSRVR cannot resolve */
/*         to a NAIF body ID, an error is signaled by a routine in the */
/*         call tree of this routine. */

/*     3)  If the reference frame associated with TARG1, FRAME1, is not */
/*         centered on TARG1, or if the reference frame associated with */
/*         TARG2, FRAME2, is not centered on TARG2, an error is signaled */
/*         by a routine in the call tree of this routine. This */
/*         restriction does not apply to shapes 'SPHERE' and 'POINT', for */
/*         which the frame input is ignored. */

/*     4)  If the frame name for FRAME1 or FRAME2 cannot resolve to a */
/*         NAIF frame ID, an error is signaled by a routine in the call */
/*         tree of this routine. */

/*     5)  If the body shape for TARG1, SHAPE1, or the body shape for */
/*         TARG2, SHAPE2, is not recognized, an error is signaled by a */
/*         routine in the call tree of this routine. */

/*     6)  If the requested aberration correction ABCORR is not */
/*         recognized, an error is signaled by a routine in the call tree */
/*         of this routine. */

/*     7)  If either one or both targets' shape is modeled as sphere, and */
/*         the required PCK data has not been loaded, an error is */
/*         signaled by a routine in the call tree of this routine. */

/*     8)  If the ephemeris data required to perform the needed state */
/*         look-ups are not loaded, an error is signaled by a routine in */
/*         the call tree of this routine. */

/*     9)  If the observer OBSRVR is located within either one of the */
/*         targets, an error is signaled by a routine in the call tree of */
/*         this routine. */

/*     10) If an error is signaled, the function returns a meaningless */
/*         result. */

/* $ Files */

/*     Appropriate SPICE kernels must be loaded by the calling program */
/*     before this routine is called. */

/*     The following data are required: */

/*     -  An SPK file (or files) containing ephemeris data sufficient to */
/*        compute the position of each of the targets with respect to the */
/*        observer. If aberration corrections are used, the states of */
/*        target and observer relative to the solar system barycenter */
/*        must be calculable from the available ephemeris data. */

/*     -  A PCK file containing the targets' tri-axial ellipsoid model, */
/*        if the targets are modeled as spheres. */

/*     -  If non-inertial reference frames are used, then PCK files, */
/*        frame kernels, C-kernels, and SCLK kernels may be needed. */

/* $ Particulars */

/*     This routine determines the apparent separation between the */
/*     two objects as observed from a third. The value reported is */
/*     corrected for light time. Moreover, if at the time this routine */
/*     is called, stellar aberration corrections are enabled, this */
/*     correction will also be applied to the apparent positions of the */
/*     centers of the two objects. */

/*     Please refer to the Aberration Corrections Required Reading */
/*     (abcorr.req) for detailed information describing the nature and */
/*     calculation of the applied corrections. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Calculate the apparent angular separation of the Earth and */
/*        Moon as observed from the Sun at a TDB time known as a time */
/*        of maximum separation. Calculate and output the separation */
/*        modeling the Earth and Moon as point bodies and as spheres. */
/*        Provide the result in degrees. */

/*        Use the meta-kernel shown below to load the required SPICE */
/*        kernels. */


/*           KPL/MK */

/*           File name: trgsep_ex1.tm */

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
/*              pck00009.tpc                  Planet orientation and */
/*                                            radii */
/*              naif0009.tls                  Leapseconds */

/*           \begindata */

/*              KERNELS_TO_LOAD = ( 'de421.bsp', */
/*                                  'pck00009.tpc', */
/*                                  'naif0009.tls'  ) */

/*           \begintext */

/*           End of meta-kernel */


/*        Example code begins here. */


/*              PROGRAM TRGSEP_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     SPICELIB functions */
/*        C */
/*              DOUBLE PRECISION      TRGSEP */
/*              DOUBLE PRECISION      DPR */


/*        C */
/*        C     Local variables. */
/*        C */
/*              CHARACTER*(32)        TARG  (2) */
/*              CHARACTER*(32)        SHAPE (2) */
/*              CHARACTER*(32)        FRAME (2) */
/*              CHARACTER*(64)        TDBSTR */
/*              CHARACTER*(32)        OBSRVR */
/*              CHARACTER*(32)        ABCORR */

/*              DOUBLE PRECISION      ET */
/*              DOUBLE PRECISION      VALUE */

/*              DATA             FRAME  / 'IAU_MOON', 'IAU_EARTH' / */

/*              DATA             TARG   / 'MOON', 'EARTH'   / */

/*              DATA             SHAPE  / 'POINT', 'SPHERE' / */


/*        C */
/*        C     Load the kernels. */
/*        C */
/*              CALL FURNSH( 'trgsep_ex1.tm') */

/*              TDBSTR = '2007-JAN-11 11:21:20.213872 (TDB)' */
/*              OBSRVR = 'SUN' */
/*              ABCORR = 'LT+S' */

/*              CALL STR2ET ( TDBSTR, ET ) */

/*              VALUE = TRGSEP( ET, */
/*             .             TARG(1),  SHAPE(1), FRAME(1), */
/*             .             TARG(2),  SHAPE(1), FRAME(2), */
/*             .             OBSRVR,   ABCORR ) */

/*              WRITE(*, FMT='(A,A6,A6)') 'Bodies:          ', */
/*             .                                      TARG(1), TARG(2) */
/*              WRITE(*, FMT='(A,A6)')    'as seen from:    ', OBSRVR */
/*              WRITE(*, FMT='(A,A36)')   'at TDB time:     ', TDBSTR */
/*              WRITE(*, FMT='(A,A)')     'with correction: ', ABCORR */
/*              WRITE(*,*) */

/*              WRITE(*, FMT='(A)') 'Apparent angular separation:' */
/*              WRITE(*, FMT='(A,F12.8)') */
/*             .     '   point body models  (deg.): ', */
/*             .                                        VALUE * DPR() */

/*              VALUE = TRGSEP( ET, */
/*             .             TARG(1),  SHAPE(2), FRAME(1), */
/*             .             TARG(2),  SHAPE(2), FRAME(2), */
/*             .             OBSRVR, ABCORR ) */

/*              WRITE(*, FMT='(A,F12.8)') */
/*             .     '   sphere body models (deg.): ', */
/*             .                                        VALUE * DPR() */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        Bodies:          MOON  EARTH */
/*        as seen from:    SUN */
/*        at TDB time:     2007-JAN-11 11:21:20.213872 (TDB) */
/*        with correction: LT+S */

/*        Apparent angular separation: */
/*           point body models  (deg.):   0.15729276 */
/*           sphere body models (deg.):   0.15413221 */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     M. Costa Sitja     (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 07-AUG-2021 (EDW) (JDR) (MCS) */

/*        Based on code originally found in zzgfspu.f. */

/* -& */
/* $ Index_Entries */

/*     compute the angular separation between two target bodies */

/* -& */

/*     SPICELIB functions. */


/*     Local Variables */


/*     Set an initial value to return in case of error. */

    ret_val = 0.;
    s_copy(ref, "J2000", (ftnlen)5, (ftnlen)5);

/*     Standard SPICE error handling. */

    if (return_()) {
	return ret_val;
    }
    chkin_("TRGSEP", (ftnlen)6);

/*     Argument check and initialization. */

    zzspin_(targ1, shape1, frame1, targ2, shape2, frame2, obsrvr, abcorr, bod,
	     frames, rad, &obs, targ1_len, shape1_len, frame1_len, targ2_len, 
	    shape2_len, frame2_len, obsrvr_len, abcorr_len);
    if (failed_()) {
	ret_val = 0.;
	chkout_("TRGSEP", (ftnlen)6);
	return ret_val;
    }

/*     Perform the calculation. */

    ret_val = zzsepq_(et, bod, &bod[1], rad, &rad[1], &obs, abcorr, ref, 
	    abcorr_len, (ftnlen)5);
    if (failed_()) {
	ret_val = 0.;
	chkout_("TRGSEP", (ftnlen)6);
	return ret_val;
    }
    chkout_("TRGSEP", (ftnlen)6);
    return ret_val;
} /* trgsep_ */

