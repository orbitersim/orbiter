/* phaseq.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__0 = 0;

/* $Procedure PHASEQ ( Phase angle quantity between bodies centers ) */
doublereal phaseq_(doublereal *et, char *target, char *illmn, char *obsrvr, 
	char *abcorr, ftnlen target_len, ftnlen illmn_len, ftnlen obsrvr_len, 
	ftnlen abcorr_len)
{
    /* Initialized data */

    static logical first = TRUE_;

    /* System generated locals */
    doublereal ret_val;

    /* Local variables */
    extern /* Subroutine */ int zzbods2c_(integer *, char *, integer *, 
	    logical *, char *, integer *, logical *, ftnlen, ftnlen);
    integer targ;
    extern /* Subroutine */ int zzvalcor_(char *, logical *, ftnlen), 
	    zzctruin_(integer *), chkin_(char *, ftnlen), errch_(char *, char 
	    *, ftnlen, ftnlen);
    integer illum;
    static logical svfnd1, svfnd2, svfnd3;
    static integer svctr1[2], svctr2[2];
    extern logical failed_(void);
    static integer svctr3[2], svicde;
    logical attblk[15];
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    char xbcorr[32];
    static integer svtgid;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), ljucrs_(integer *, 
	    char *, char *, ftnlen, ftnlen);
    static char svtarg[36], svilmn[36];
    static integer svobsn;
    extern logical return_(void);
    static char svobsr[36];
    logical fnd;
    integer obs;
    extern /* Subroutine */ int zzgfpaq_(doublereal *, integer *, integer *, 
	    integer *, char *, doublereal *, ftnlen);

/* $ Abstract */

/*     Compute the apparent phase angle for a target, observer, */
/*     illuminator set of ephemeris objects. */

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

/*     EPHEMERIS */
/*     GEOMETRY */
/*     PHASE ANGLE */
/*     SEARCH */

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
/*     ET         I   Ephemeris seconds past J2000 TDB. */
/*     TARGET     I   Target body name. */
/*     ILLMN      I   Illuminating body name. */
/*     OBSRVR     I   Observer body. */
/*     ABCORR     I   Aberration correction flag. */

/*     The function returns the value of phase angle. */

/* $ Detailed_Input */

/*     ET       is the time in ephemeris seconds past J2000 TDB at which */
/*              to compute the phase angle. */

/*     TARGET   is the name of the target body. Optionally, you may */
/*              supply a string containing the integer ID code */
/*              for the object. For example both 'MOON' and '301' */
/*              are legitimate strings that indicate the Moon is the */
/*              target body. The TARGET string lack sensitivity to */
/*              case, leading and trailing blanks. */

/*     ILLMN    is the name of the illuminating body. Optionally, you may */
/*              supply a string containing the integer ID code */
/*              for the object. For example both 'SUN' and '10' */
/*              are legitimate strings that indicate the sun is the */
/*              illuminating body. The ILLMN string lack sensitivity */
/*              to case, leading and trailing blanks. */

/*              In most cases, ILLMN is the sun. */

/*     OBSRVR   is the name of the observer body. Optionally, you may */
/*              supply a string containing the integer ID code */
/*              for the object. For example both 'MOON' and '301' */
/*              are legitimate strings that indicate the Moon is the */
/*              observer body. The OBSRVR string lack sensitivity */
/*              to case, leading and trailing blanks. */

/*     ABCORR   is the string description of the aberration corrections */
/*              to apply to the state evaluations to account for one-way */
/*              light time and stellar aberration. The ABCORR string lack */
/*              sensitivity to case, leading and trailing blanks. */

/*              This routine accepts only reception mode aberration */
/*              corrections. See the header of SPKEZR for a detailed */
/*              description of the aberration correction options. */
/*              For convenience, the appropriate aberration options are */
/*              listed below: */

/*                 'NONE'     Apply no correction. Returns the "true" */
/*                            geometric state. */

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

/* $ Detailed_Output */

/*     The function returns the optionally light-time corrected phase */
/*     angle between TARGET and ILLMN as observed from OBSRVR. */

/*     The range of the phase angle is [0, pi]. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the body name to SPICE ID look-up fails for any of the */
/*         TARGET, ILLMN, or OBSRVR names, the error */
/*         SPICE(IDCODENOTFOUND) is signaled. */

/*     2)  If the aberration correct, ABCORR, indicates a transmission */
/*         based correction, the error SPICE(INVALIDOPTION) is signaled. */

/*     3)  If the TARGET, ILLMN, and OBSRVR are not unique, the error */
/*         SPICE(BODIESNOTDISTINCT) is signaled. */

/* $ Files */

/*   Appropriate kernels must be loaded by the calling program before */
/*   this routine is called. */

/*   The following data are required: */

/*   -  SPK data: ephemeris data for the observer, illuminator, and */
/*      target must be loaded. If aberration corrections are used, */
/*      the states of the ephemeris bodies relative to the solar */
/*      system barycenter must be calculable from the available */
/*      ephemeris data. Typically ephemeris data are made */
/*      available by loading one or more SPK files using FURNSH. */

/*   The following data may be required: */

/*   -  Frame data: if a frame definition not built into SPICE is */
/*      required, that definition must be available in the kernel */
/*      pool. Typically frame definitions are supplied by loading a */
/*      frame kernel using FURNSH. */

/*   -  Orientation data: if a CK based frame is used in this routine's */
/*      state computation, then at least one CK and corresponding SCLK */
/*      kernel is required. If dynamic frames are used, additional */
/*      SPK, PCK, CK, or SCLK kernels may be required. */

/* $ Particulars */

/*     This routine returns the phase angle using the location of the */
/*     bodies (if point objects) or the center of the bodies (if finite */
/*     bodies). */



/*                       ILLMN     OBSRVR */
/*       ILLMN as seen      ^       / */
/*       from TARGET at     |      / */
/*       ET - LT.           |     / */
/*                         >|..../< phase angle */
/*                          |   / */
/*                        . |  / */
/*                      .   | / */
/*                     .    |v        TARGET as seen from OBSRVR */
/*               SEP   .  TARGET      at ET */
/*                      .  / */
/*                        / */
/*                       v */



/*        PI = SEP + PHASE */

/*        so */

/*        PHASE = PI - SEP */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Determine the time windows from December 1, 2006 UTC to */
/*        January 31, 2007 UTC for which the sun-moon-earth configuration */
/*        phase angle satisfies the relation conditions with respect to a */
/*        reference value of 0.57598845 radians (the phase angle at */
/*        January 1, 2007 00:00:00.000 UTC, 33.001707 degrees). Also */
/*        determine the time windows corresponding to the local maximum */
/*        and minimum phase angles, and the absolute maximum and minimum */
/*        phase angles during the search interval. The configuration */
/*        defines the Sun as the illuminator, the Moon as the target, and */
/*        the Earth as the observer. */

/*        Use the meta-kernel shown below to load the required SPICE */
/*        kernels. */


/*           KPL/MK */

/*           File name: phaseq_ex1.tm */

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


/*              PROGRAM PHASEQ_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Include GF parameter declarations: */
/*        C */
/*              INCLUDE 'gf.inc' */

/*        C */
/*        C     SPICELIB functions */
/*        C */
/*              DOUBLE PRECISION      SPD */
/*              DOUBLE PRECISION      PHASEQ */

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
/*              PARAMETER           ( MAXWIN = 1000 ) */

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
/*              CHARACTER*(TIMLEN)    RELATE (NLOOPS) */
/*              CHARACTER*(6)         ABCORR */
/*              CHARACTER*(6)         ILLMN */
/*              CHARACTER*(6)         OBSRVR */
/*              CHARACTER*(6)         TARGET */
/*              CHARACTER*(TIMLEN)    TIMSTR */

/*              DOUBLE PRECISION      CNFINE ( LBCELL : 2 ) */
/*              DOUBLE PRECISION      RESULT ( LBCELL : MAXWIN ) */
/*              DOUBLE PRECISION      WORK   ( LBCELL : MAXWIN, NWPA ) */
/*              DOUBLE PRECISION      ADJUST */
/*              DOUBLE PRECISION      ET0 */
/*              DOUBLE PRECISION      ET1 */
/*              DOUBLE PRECISION      FINISH */
/*              DOUBLE PRECISION      PHASE */
/*              DOUBLE PRECISION      REFVAL */
/*              DOUBLE PRECISION      START */
/*              DOUBLE PRECISION      STEP */

/*              INTEGER               I */
/*              INTEGER               J */


/*        C */
/*        C     The relation values for the search. */
/*        C */
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
/*              CALL FURNSH ( 'phaseq_ex1.tm' ) */

/*        C */
/*        C     Initialize windows. */
/*        C */
/*              CALL SSIZED ( MAXWIN, RESULT ) */
/*              CALL SSIZED ( 2,      CNFINE ) */

/*        C */
/*        C     Store the time bounds of our search interval in */
/*        C     the confinement window. */
/*        C */
/*              CALL STR2ET ( '2006 DEC 01', ET0 ) */
/*              CALL STR2ET ( '2007 JAN 31', ET1 ) */

/*              CALL WNINSD ( ET0, ET1, CNFINE ) */

/*        C */
/*        C     Search using a step size of 1 day (in units of seconds). */
/*        C     The reference value is 0.57598845. We're not using the */
/*        C     adjustment feature, so we set ADJUST to zero. */
/*        C */
/*              STEP   = SPD() */
/*              REFVAL = 0.57598845D0 */
/*              ADJUST = 0.D0 */

/*        C */
/*        C     Define the values for target, observer, illuminator, and */
/*        C     aberration correction. */
/*        C */
/*              TARGET = 'MOON' */
/*              ILLMN  = 'SUN' */
/*              ABCORR = 'LT+S' */
/*              OBSRVR = 'EARTH' */

/*              DO J=1, NLOOPS */

/*                 WRITE(*,*) 'Relation condition: ', RELATE(J) */

/*        C */
/*        C        Perform the search. The SPICE window RESULT contains */
/*        C        the set of times when the condition is met. */
/*        C */
/*                 CALL GFPA (  TARGET,    ILLMN,  ABCORR, OBSRVR, */
/*             .                RELATE(J), REFVAL, ADJUST, STEP, */
/*             .                CNFINE,    MAXWIN, NWPA,   WORK, */
/*             .                RESULT ) */

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

/*                       PHASE = PHASEQ( START, TARGET, ILLMN, OBSRVR, */
/*             .                         ABCORR ) */
/*                       CALL TIMOUT ( START, */
/*             .                       'YYYY-MON-DD HR:MN:SC.###', */
/*             .                       TIMSTR                          ) */

/*                       WRITE (*, '(2X,A,F16.9)') 'Start time = ' */
/*             .                               //   TIMSTR, PHASE */


/*                       PHASE = PHASEQ( FINISH, TARGET, ILLMN, OBSRVR, */
/*             .                         ABCORR ) */
/*                       CALL TIMOUT ( FINISH, */
/*             .                       'YYYY-MON-DD HR:MN:SC.###', */
/*             .                       TIMSTR                          ) */

/*                       WRITE (*, '(2X,A,F16.9)') 'Stop time  = ' */
/*             .                                //  TIMSTR, PHASE */

/*                    END DO */

/*                 END IF */

/*                 WRITE(*,*) ' ' */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Relation condition: = */
/*          Start time = 2006-DEC-02 13:31:34.414       0.575988450 */
/*          Stop time  = 2006-DEC-02 13:31:34.414       0.575988450 */
/*          Start time = 2006-DEC-07 14:07:55.470       0.575988450 */
/*          Stop time  = 2006-DEC-07 14:07:55.470       0.575988450 */
/*          Start time = 2006-DEC-31 23:59:59.997       0.575988450 */
/*          Stop time  = 2006-DEC-31 23:59:59.997       0.575988450 */
/*          Start time = 2007-JAN-06 08:16:25.512       0.575988450 */
/*          Stop time  = 2007-JAN-06 08:16:25.512       0.575988450 */
/*          Start time = 2007-JAN-30 11:41:32.557       0.575988450 */
/*          Stop time  = 2007-JAN-30 11:41:32.557       0.575988450 */

/*         Relation condition: < */
/*          Start time = 2006-DEC-02 13:31:34.414       0.575988450 */
/*          Stop time  = 2006-DEC-07 14:07:55.470       0.575988450 */
/*          Start time = 2006-DEC-31 23:59:59.997       0.575988450 */
/*          Stop time  = 2007-JAN-06 08:16:25.512       0.575988450 */
/*          Start time = 2007-JAN-30 11:41:32.557       0.575988450 */
/*          Stop time  = 2007-JAN-31 00:00:00.000       0.468279091 */

/*         Relation condition: > */
/*          Start time = 2006-DEC-01 00:00:00.000       0.940714974 */
/*          Stop time  = 2006-DEC-02 13:31:34.414       0.575988450 */
/*          Start time = 2006-DEC-07 14:07:55.470       0.575988450 */
/*          Stop time  = 2006-DEC-31 23:59:59.997       0.575988450 */
/*          Start time = 2007-JAN-06 08:16:25.512       0.575988450 */
/*          Stop time  = 2007-JAN-30 11:41:32.557       0.575988450 */

/*         Relation condition: LOCMIN */
/*          Start time = 2006-DEC-05 00:16:50.317       0.086121423 */
/*          Stop time  = 2006-DEC-05 00:16:50.317       0.086121423 */
/*          Start time = 2007-JAN-03 14:18:31.977       0.079899769 */
/*          Stop time  = 2007-JAN-03 14:18:31.977       0.079899769 */

/*         Relation condition: ABSMIN */
/*          Start time = 2007-JAN-03 14:18:31.977       0.079899769 */
/*          Stop time  = 2007-JAN-03 14:18:31.977       0.079899769 */

/*         Relation condition: LOCMAX */
/*          Start time = 2006-DEC-20 14:09:10.392       3.055062862 */
/*          Stop time  = 2006-DEC-20 14:09:10.392       3.055062862 */
/*          Start time = 2007-JAN-19 04:27:54.600       3.074603891 */
/*          Stop time  = 2007-JAN-19 04:27:54.600       3.074603891 */

/*         Relation condition: ABSMAX */
/*          Start time = 2007-JAN-19 04:27:54.600       3.074603891 */
/*          Stop time  = 2007-JAN-19 04:27:54.600       3.074603891 */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 31-AUG-2021 (JDR) (EDW) */

/*        Edited the header to comply with NAIF standards. */

/*        Fixed typos in the Header. Renamed example's meta-kernel. */
/*        Removed reference to private routine ZZGFPAQ from $Particulars */
/*        section. */

/* -    SPICELIB Version 1.0.0, 27-MAR-2014 (EDW) (BVS) */

/* -& */
/* $ Index_Entries */

/*     compute phase angle for arbitrary illumination source */

/* -& */

/*     SPICELIB functions. */


/*     Local parameters */


/*     Saved body name length. */


/*     Local Variables. */


/*     Saved name/ID item declarations. */


/*     Saved name/ID items. */


/*     Initial values. */

    ret_val = 0.;

/*     Standard SPICE error handling. */

    if (return_()) {
	return ret_val;
    }
    chkin_("PHASEQ", (ftnlen)6);

/*     Initialization. */

    if (first) {

/*        Initialize counters. */

	zzctruin_(svctr1);
	zzctruin_(svctr2);
	zzctruin_(svctr3);
	first = FALSE_;
    }

/*     Obtain integer codes for the target, illuminator, and observer. */

    zzbods2c_(svctr1, svtarg, &svtgid, &svfnd1, target, &targ, &fnd, (ftnlen)
	    36, target_len);
    if (! fnd) {
	setmsg_("The target, '#', is not a recognized name for an ephemeris "
		"object. The cause of this problem may be that you need an up"
		"dated version of the SPICE Toolkit. ", (ftnlen)155);
	errch_("#", target, (ftnlen)1, target_len);
	sigerr_("SPICE(IDCODENOTFOUND)", (ftnlen)21);
	chkout_("PHASEQ", (ftnlen)6);
	return ret_val;
    }
    zzbods2c_(svctr2, svilmn, &svicde, &svfnd2, illmn, &illum, &fnd, (ftnlen)
	    36, illmn_len);
    if (! fnd) {
	setmsg_("The illuminator, '#', is not a recognized name for an ephem"
		"eris object. The cause of this problem may be that you need "
		"an updated version of the SPICE Toolkit. ", (ftnlen)160);
	errch_("#", illmn, (ftnlen)1, illmn_len);
	sigerr_("SPICE(IDCODENOTFOUND)", (ftnlen)21);
	chkout_("PHASEQ", (ftnlen)6);
	return ret_val;
    }
    zzbods2c_(svctr3, svobsr, &svobsn, &svfnd3, obsrvr, &obs, &fnd, (ftnlen)
	    36, obsrvr_len);
    if (! fnd) {
	setmsg_("The observer, '#', is not a recognized name for an ephemeri"
		"s object. The cause of this problem may be that you need an "
		"updated version of the SPICE Toolkit. ", (ftnlen)157);
	errch_("#", obsrvr, (ftnlen)1, obsrvr_len);
	sigerr_("SPICE(IDCODENOTFOUND)", (ftnlen)21);
	chkout_("PHASEQ", (ftnlen)6);
	return ret_val;
    }

/*     Squeeze all blanks out of the aberration correction */
/*     string; ensure the string is in upper case. */

    ljucrs_(&c__0, abcorr, xbcorr, abcorr_len, (ftnlen)32);

/*     Check the aberration correction. If SPKEZR can't handle it, */
/*     neither can we. */

    zzvalcor_(xbcorr, attblk, (ftnlen)32);
    if (failed_()) {
	chkout_("PHASEQ", (ftnlen)6);
	return ret_val;
    }

/*     Restrict correction to reception cases. The attribute block ID */
/*     for transmit corrections is XMTIDX. */

    if (attblk[4]) {
	setmsg_("Invalid aberration correction '#'. Phase angle geometry cal"
		"culations currently restricted to reception cases.", (ftnlen)
		109);
	errch_("#", abcorr, (ftnlen)1, abcorr_len);
	sigerr_("SPICE(INVALIDOPTION)", (ftnlen)20);
	chkout_("PHASEQ", (ftnlen)6);
	return ret_val;
    }

/*     Make sure the observer, illuminator, and target are distinct. */

    if (targ == obs || targ == illum || obs == illum) {
	setmsg_("The observer, illuminator, and target must be distinct obje"
		"cts, but are not: OBSRVR = #, TARGET = #, are not: ILLMN= #.",
		 (ftnlen)119);
	errch_("#", obsrvr, (ftnlen)1, obsrvr_len);
	errch_("#", target, (ftnlen)1, target_len);
	errch_("#", illmn, (ftnlen)1, illmn_len);
	sigerr_("SPICE(BODIESNOTDISTINCT)", (ftnlen)24);
	chkout_("PHASEQ", (ftnlen)6);
	return ret_val;
    }

/*     Call the routine to calculate the phase angle */

    zzgfpaq_(et, &targ, &illum, &obs, xbcorr, &ret_val, (ftnlen)32);

/*     All done. */

    chkout_("PHASEQ", (ftnlen)6);
    return ret_val;
} /* phaseq_ */

