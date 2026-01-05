/* ckgp.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__6 = 6;
static integer c__9 = 9;

/* $Procedure CKGP ( C-kernel, get pointing ) */
/* Subroutine */ int ckgp_(integer *inst, doublereal *sclkdp, doublereal *tol,
	 char *ref, doublereal *cmat, doublereal *clkout, logical *found, 
	ftnlen ref_len)
{
    /* Initialized data */

    static logical first = TRUE_;

    logical pfnd, sfnd;
    integer sclk;
    extern /* Subroutine */ int sct2e_(integer *, doublereal *, doublereal *),
	     zznamfrm_(integer *, char *, integer *, char *, integer *, 
	    ftnlen, ftnlen);
    integer type1, type2;
    extern /* Subroutine */ int zzctruin_(integer *);
    char segid[40];
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    doublereal descr[5];
    extern /* Subroutine */ int dafus_(doublereal *, integer *, integer *, 
	    doublereal *, integer *), ckbss_(integer *, doublereal *, 
	    doublereal *, logical *), ckpfs_(integer *, doublereal *, 
	    doublereal *, doublereal *, logical *, doublereal *, doublereal *,
	     doublereal *, logical *), moved_(doublereal *, integer *, 
	    doublereal *), cksns_(integer *, doublereal *, char *, logical *, 
	    ftnlen);
    static char svref[32];
    logical gotit;
    static integer svctr1[2];
    extern logical failed_(void);
    doublereal av[3], et;
    integer handle;
    extern /* Subroutine */ int refchg_(integer *, integer *, doublereal *, 
	    doublereal *);
    logical needav;
    extern /* Subroutine */ int ckmeta_(integer *, char *, integer *, ftnlen);
    integer refseg, center;
    extern /* Subroutine */ int frinfo_(integer *, integer *, integer *, 
	    integer *, logical *);
    integer refreq, typeid;
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    doublereal tmpmat[9]	/* was [3][3] */;
    static integer svrefr;
    extern logical return_(void);
    doublereal dcd[2];
    integer icd[6];
    extern /* Subroutine */ int mxm_(doublereal *, doublereal *, doublereal *)
	    ;
    doublereal rot[9]	/* was [3][3] */;

/* $ Abstract */

/*     Get pointing (attitude) for a specified spacecraft clock time. */

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
/*     SCLK */

/* $ Keywords */

/*     POINTING */

/* $ Declarations */
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
/*     INST       I   NAIF ID of instrument, spacecraft, or structure. */
/*     SCLKDP     I   Encoded spacecraft clock time. */
/*     TOL        I   Time tolerance. */
/*     REF        I   Reference frame. */
/*     CMAT       O   C-matrix pointing data. */
/*     CLKOUT     O   Output encoded spacecraft clock time. */
/*     FOUND      O   .TRUE. when requested pointing is available. */

/* $ Detailed_Input */

/*     INST     is the NAIF integer ID for the instrument, spacecraft, or */
/*              other structure for which pointing is requested. For */
/*              brevity we will refer to this object as the "instrument," */
/*              and the frame fixed to this object as the "instrument */
/*              frame" or "instrument-fixed" frame. */

/*     SCLKDP   is the encoded spacecraft clock time for which pointing */
/*              is requested. */

/*              The SPICELIB routines SCENCD and SCE2C respectively */
/*              convert spacecraft clock strings and ephemeris time to */
/*              encoded spacecraft clock. The inverse conversions are */
/*              performed by SCDECD and SCT2E. */

/*     TOL      is a time tolerance in ticks, the units of encoded */
/*              spacecraft clock time. */

/*              The SPICELIB routine SCTIKS converts a spacecraft clock */
/*              tolerance duration from its character string */
/*              representation to ticks. SCFMT performs the inverse */
/*              conversion. */

/*              The C-matrix returned by CKGP is the one whose time tag */
/*              is closest to SCLKDP and within TOL units of SCLKDP. */
/*              (More in $Particulars, below.) */

/*              In general, because using a non-zero tolerance affects */
/*              selection of the segment from which the data is obtained, */
/*              users are strongly discouraged from using a non-zero */
/*              tolerance when reading CKs with continuous data. Using a */
/*              non-zero tolerance should be reserved exclusively to */
/*              reading CKs with discrete data because in practice */
/*              obtaining data from such CKs using a zero tolerance is */
/*              often not possible due to time round off. */

/*     REF      is the desired reference frame for the returned pointing. */
/*              The returned C-matrix CMAT gives the orientation of the */
/*              instrument designated by INST relative to the frame */
/*              designated by REF. When a vector specified relative to */
/*              frame REF is left- multiplied by CMAT, the vector is */
/*              rotated to the frame associated with INST. See the */
/*              discussion of CMAT below for details. */

/*              Consult the SPICE document "Frames" for a discussion */
/*              of supported reference frames. */

/* $ Detailed_Output */

/*     CMAT     is a rotation matrix that transforms the components of a */
/*              vector expressed in the reference frame specified by REF */
/*              to components expressed in the frame tied to the */
/*              instrument, spacecraft, or other structure at time CLKOUT */
/*              (see below). */

/*              Thus, if a vector v has components x,y,z in the REF */
/*              reference frame, then v has components x',y',z' in the */
/*              instrument fixed frame at time CLKOUT: */

/*                   .-   -.     .-        -. .-   -. */
/*                   |  x' |     |          | |  x  | */
/*                   |  y' |  =  |   CMAT   | |  y  | */
/*                   |  z' |     |          | |  z  | */
/*                   `-   -'     `-        -' `-   -' */

/*              If you know x', y', z', use the transpose of the */
/*              C-matrix to determine x, y, z as follows: */

/*                   .-   -.      .-        -.T  .-   -. */
/*                   |  x  |      |          |   |  x' | */
/*                   |  y  |  =   |   CMAT   |   |  y' | */
/*                   |  z  |      |          |   |  z' | */
/*                   `-   -'      `-        -'   `-   -' */

/*                            (Transpose of CMAT) */

/*     CLKOUT   is the encoded spacecraft clock time associated with the */
/*              returned C-matrix. This value may differ from the */
/*              requested time, but never by more than the input */
/*              tolerance TOL. */

/*              The $Particulars section below describes the search */
/*              algorithm used by CKGP to satisfy a pointing request. */
/*              This algorithm determines the pointing instance (and */
/*              therefore the associated time value) that is returned. */

/*     FOUND    is .TRUE. if a record was found to satisfy the pointing */
/*              request. FOUND will be .FALSE. otherwise. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If a C-kernel file has not been loaded using FURNSH prior to */
/*         a call to this routine, an error is signaled by a routine in */
/*         the call tree of this routine. */

/*     2)  If TOL is negative, found is set to .FALSE. */

/*     3)  If REF is not a supported reference frame, an error is */
/*         signaled by a routine in the call tree of this routine and */
/*         FOUND is set to .FALSE. */

/* $ Files */

/*     CKGP searches through files loaded by FURNSH to locate a */
/*     segment that can satisfy the request for pointing for instrument */
/*     INST at time SCLKDP. You must load a C-kernel file using FURNSH */
/*     prior to calling this routine. */

/* $ Particulars */

/*     How the tolerance argument is used */
/*     ================================== */


/*     Reading a type 1 CK segment (discrete pointing instances) */
/*     --------------------------------------------------------- */

/*     In the diagram below */

/*        - "0" is used to represent discrete pointing instances */
/*          (quaternions and associated time tags). */

/*        - "( )" are used to represent the end points of the time */
/*          interval covered by a segment in a CK file. */

/*        - SCLKDP is the time at which you requested pointing. */
/*          The location of SCLKDP relative to the time tags of the */
/*          pointing instances is indicated by the "+" sign. */

/*        - TOL is the time tolerance specified in the pointing */
/*          request. The square brackets "[ ]" represent the */
/*          endpoints of the time interval */

/*             SCLKDP-TOL : SCLKDP+TOL */

/*        - The quaternions occurring in the segment need not be */
/*          evenly spaced in time. */


/*     Case 1: pointing is available */
/*     ------------------------------ */

/*                              SCLKDP */
/*                                   \   TOL */
/*                                    | / */
/*                                    |/\ */
/*     Your request                [--+--] */
/*                                 .  .  . */
/*     Segment      (0-----0--0--0--0--0--0---0--0------------0--0--0--0) */
/*                                     ^ */
/*                                     | */
/*                         CKGP returns this instance. */


/*     Case 2: pointing is not available */
/*     ---------------------------------- */

/*                                                   SCLKDP */
/*                                                      \   TOL */
/*                                                       | / */
/*                                                       |/\ */
/*     Your request                                   [--+--] */
/*                                                    .  .  . */
/*     Segment      (0-----0--0--0--0--0--0---0--0--0---------0--0--0--0) */


/*                         CKGP returns no pointing; the output */
/*                         FOUND flag is set to .FALSE. */



/*     Reading a type 2, 3, 4, or 5 CK segment (continuous pointing) */
/*     ------------------------------------------------------------- */

/*     In the diagrams below */

/*        - "==" is used to represent periods of continuous pointing. */

/*        - "--" is used to represent gaps in the pointing coverage. */

/*        - "( )" are used to represent the end points of the time */
/*          interval covered by a segment in a CK file. */

/*        - SCLKDP is the time at which you requested pointing. */
/*          The location of SCLKDP relative to the time tags of the */
/*          pointing instances is indicated by the "+" sign. */

/*        - TOL is the time tolerance specified in the pointing */
/*          request. The square brackets "[ ]" represent the */
/*          endpoints of the time interval */

/*             SCLKDP-TOL : SCLKDP+TOL */

/*        - The quaternions occurring in the periods of continuous */
/*          pointing need not be evenly spaced in time. */


/*     Case 1: pointing is available at the request time */
/*     -------------------------------------------------- */

/*                             SCLKDP */
/*                                   \   TOL */
/*                                    | / */
/*                                    |/\ */
/*     Your request                [--+--] */
/*                                 .  .  . */
/*                                 .  .  . */
/*                                 .  .  . */
/*     Segment            (==---===========---=======----------===--) */
/*                                    ^ */
/*                                    | */

/*                   The request time lies within an interval where */
/*                   continuous pointing is available. CKGP returns */
/*                   pointing at the requested epoch. */


/*     Case 2: pointing is available "near" the request time */
/*     ------------------------------------------------------ */

/*                                    SCLKDP */
/*                                          \   TOL */
/*                                           | / */
/*                                           |/\ */
/*     Your request                       [--+--] */
/*                                        .  .  . */
/*     Segment            (==---===========----=======---------===--) */
/*                                             ^ */
/*                                             | */

/*                   The request time lies in a gap: an interval where */
/*                   continuous pointing is *not* available.  CKGP */
/*                   returns pointing for the epoch closest to the */
/*                   request time SCLKDP. */


/*     Case 3: pointing is not available */
/*     ---------------------------------- */

/*                                                 SCLKDP */
/*                                                       \   TOL */
/*                                                        | / */
/*                                                        |/\ */
/*     Your request                                    [--+--] */
/*                                                     .  .  . */
/*     Segment            (==---===========----=======---------===--) */

/*                         CKGP returns no pointing; the output */
/*                         FOUND flag is set to .FALSE. */



/*     Tolerance and segment priority */
/*     ============================== */

/*     CKGP searches through loaded C-kernels to satisfy a pointing */
/*     request. Last-loaded files are searched first. Individual files */
/*     are searched in backwards order, so that between competing */
/*     segments (segments containing data for the same object, for */
/*     overlapping time ranges), the one closest to the end of the file */
/*     has highest priority. */

/*     The search ends when a segment is found that can provide pointing */
/*     for the specified instrument at a time falling within the */
/*     specified tolerance on either side of the request time. Within */
/*     that segment, the instance closest to the input time is located */
/*     and returned. */

/*     The following four cases illustrate this search procedure. */
/*     Segments A and B are in the same file, with segment A located */
/*     further towards the end of the file than segment B. Both segments */
/*     A and B contain discrete pointing data, indicated by the number */
/*     0. */


/*     Case 1: Pointing is available in the first segment searched. */
/*              Because segment A has the highest priority and can */
/*              satisfy the request, segment B is not searched. */


/*                                  SCLKDP */
/*                                        \  TOL */
/*                                         | / */
/*                                         |/\ */
/*     Your request                     [--+--] */
/*                                      .  .  . */
/*     Segment A          (0-----------------0--------0--0-----0) */
/*                                           ^ */
/*                                           | */
/*                                           | */
/*                               CKGP returns this instance */

/*     Segment B     (0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0) */



/*     Case 2: Pointing is not available in the first segment searched. */
/*              Because segment A cannot satisfy the request, segment B */
/*              is searched. */


/*                             SCLKDP */
/*                                  \   TOL */
/*                                   | / */
/*                                   |/\ */
/*     Your request               [--+--] */
/*                                .  .  . */
/*     Segment A          (0-----------------0--------0--0-----0) */
/*                                .  .  . */
/*     Segment B     (0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0--0) */
/*                                   ^ */
/*                                   | */
/*                       CKGP returns this instance */


/*     Segments that contain continuous pointing data are searched in */
/*     the same manner as segments containing discrete pointing data. */
/*     For request times that fall within the bounds of continuous */
/*     intervals, CKGP will return pointing at the request time. When */
/*     the request time does not fall within an interval, then a time at */
/*     an endpoint of an interval may be returned if it is the closest */
/*     time in the segment to the user request time and is also within */
/*     the tolerance. */

/*     In the following examples, segment A is located further towards */
/*     the end of the file than segment C. Segment A contains discrete */
/*     pointing data and segment C contains continuous data, indicated */
/*     by the "=" character. */


/*     Case 3: Pointing is not available in the first segment searched. */
/*              Because segment A cannot satisfy the request, segment C */
/*              is searched. */

/*                             SCLKDP */
/*                                   \  TOL */
/*                                    | / */
/*                                    |/\ */
/*     Your request                [--+--] */
/*                                 .  .  . */
/*                                 .  .  . */
/*     Segment A          (0-----------------0--------0--0-----0) */
/*                                 .  .  . */
/*                                 .  .  . */
/*     Segment C          (---=============-----====--------==--) */
/*                                    ^ */
/*                                    | */
/*                                    | */
/*                         CKGP returns this instance */


/*     In the next case, assume that the order of segments A and C in the */
/*     file is reversed: A is now closer to the front, so data from */
/*     segment C are considered first. */


/*     Case 4: Pointing is available in the first segment searched. */
/*              Because segment C has the highest priority and can */
/*              satisfy the request, segment A is not searched. */

/*                                             SCLKDP */
/*                                            / */
/*                                           |  TOL */
/*                                           | / */
/*                                           |/\ */
/*     Your request                       [--+--] */
/*                                        .  .  . */
/*                                        .  .  . */
/*     Segment C          (---=============-----====--------==--) */
/*                                             ^ */
/*                                             | */
/*                                CKGP returns this instance */

/*     Segment A          (0-----------------0--------0--0-----0) */
/*                                           ^ */
/*                                           | */
/*                                     "Best" answer */


/*     The next case illustrates an unfortunate side effect of using */
/*     a non-zero tolerance when reading multi-segment CKs with */
/*     continuous data. In all cases when the look-up interval */
/*     formed using tolerance overlaps a segment boundary and */
/*     the request time falls within the coverage of the lower */
/*     priority segment, the data at the end of the higher priority */
/*     segment will be picked instead of the data from the lower */
/*     priority segment. */


/*     Case 5: Pointing is available in the first segment searched. */
/*              Because segment C has the highest priority and can */
/*              satisfy the request, segment A is not searched. */

/*                                             SCLKDP */
/*                                            / */
/*                                           |  TOL */
/*                                           | / */
/*                                           |/\ */
/*     Your request                       [--+--] */
/*                                        .  .  . */
/*                                        .  .  . */
/*     Segment C                                (===============) */
/*                                              ^ */
/*                                              | */
/*                                CKGP returns this instance */

/*     Segment A          (=====================) */
/*                                           ^ */
/*                                           | */
/*                                     "Best" answer */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as input, */
/*     the compiler and supporting libraries, and the machine specific */
/*     arithmetic implementation. */

/*     1) The following example program uses CKGP to get C-matrices */
/*        for a set of images whose SCLK counts (un-encoded character */
/*        string versions) are contained in the array SCLKCH. */

/*        If available, the program will get the corrected pointing */
/*        values. */

/*        For each C-matrix, a unit pointing vector is constructed and */
/*        printed. */

/*        Use the CK kernel below to load the CASSINI image navigated */
/*        spacecraft pointing and orientation data. */

/*           04153_04182ca_ISS.bc */


/*        Use the SCLK kernel below to load the CASSINI spacecraft clock */
/*        time correlation data required for the conversion between */
/*        spacecraft clock string representation and double precision */
/*        encoding of spacecraft clock counts. */

/*           cas00071.tsc */


/*        Example code begins here. */


/*              PROGRAM CKGP_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Constants for this program. */
/*        C */
/*        C     -- The code for the CASSINI spacecraft clock is -82. */
/*        C */
/*        C     -- The code for CASSINI spacecraft reference frame */
/*        C        is -82000. */
/*        C */
/*        C    --  Spacecraft clock times for successive CASSINI */
/*        C        navigation images always differ by more than 1.0. */
/*        C        This is an acceptable tolerance, and must be */
/*        C        converted to "ticks" (units of encoded SCLK) for */
/*        C        input to CKGP. */
/*        C */
/*        C     -- The reference frame we want is J2000. */
/*        C */
/*        C     -- The CASSINI ISS camera boresight */
/*        C        in the spacecraft frame is */
/*        C        (0.0005760, -0.99999982, -0.0001710). */
/*        C */
/*              INTEGER               FILEN */
/*              PARAMETER           ( FILEN  = 255 ) */

/*              INTEGER               NPICS */
/*              PARAMETER           ( NPICS  = 2 ) */

/*              INTEGER               TIMLEN */
/*              PARAMETER           ( TIMLEN = 30 ) */

/*              INTEGER               REFLEN */
/*              PARAMETER           ( REFLEN = 32 ) */

/*              CHARACTER*(TIMLEN)    CLKCH */
/*              CHARACTER*(FILEN)     CK */
/*              CHARACTER*(REFLEN)    REF */
/*              CHARACTER*(FILEN)     SCLK */
/*              CHARACTER*(TIMLEN)    SCLKCH ( NPICS ) */
/*              CHARACTER*(TIMLEN)    TOL */

/*              DOUBLE PRECISION      CLKOUT */
/*              DOUBLE PRECISION      CMAT   ( 3, 3 ) */
/*              DOUBLE PRECISION      SCLKDP */
/*              DOUBLE PRECISION      TOLTIK */
/*              DOUBLE PRECISION      ISSFIX ( 3 ) */
/*              DOUBLE PRECISION      VINERT ( 3 ) */

/*              INTEGER               SC */
/*              INTEGER               I */
/*              INTEGER               INST */

/*              LOGICAL               FOUND */

/*              DATA                  SCLKCH /  '1465644281.0', */
/*             .                                '1465644351.0' / */

/*              DATA                  ISSFIX /  0.00057600D0, */
/*             .                               -0.99999982D0, */
/*             .                               -0.00017100D0  / */

/*              CK         = '04153_04182ca_ISS.bc' */
/*              SCLK       = 'cas00071.tsc' */
/*              SC         = -82 */
/*              INST       = -82000 */
/*              TOL        = '1.0' */
/*              REF        = 'J2000' */

/*        C */
/*        C     Load the CK file. */
/*        C */
/*              CALL FURNSH ( CK ) */

/*        C */
/*        C     Need to load a CASSINI SCLK kernel to convert from */
/*        C     clock string to ticks.  Although not required for */
/*        C     the CASSINI spacecraft clock, most modern spacecraft */
/*        C     clocks require a leapseconds kernel to be loaded in */
/*        C     addition to an SCLK kernel. */
/*        C */
/*              CALL FURNSH ( SCLK ) */

/*        C */
/*        C     Convert tolerance from CASSINI formatted character */
/*        C     string SCLK to ticks, which are units of encoded SCLK. */
/*        C */
/*              CALL SCTIKS ( SC, TOL, TOLTIK ) */


/*              DO I = 1, NPICS */
/*        C */
/*        C        CKGP requires encoded spacecraft clock. */
/*        C */
/*                 CALL SCENCD ( SC, SCLKCH( I ), SCLKDP ) */

/*                 CALL CKGP ( INST,   SCLKDP, TOLTIK, REF, CMAT, */
/*             .               CLKOUT, FOUND                      ) */

/*                 IF ( FOUND ) THEN */

/*        C */
/*        C           Use the transpose of the C-matrix to transform the */
/*        C           boresight vector from camera-fixed to reference */
/*        C           coordinates. */
/*        C */
/*                    CALL MTXV   ( CMAT, ISSFIX, VINERT ) */
/*                    CALL SCDECD ( SC,   CLKOUT, CLKCH  ) */

/*                    WRITE(*,*) 'Requested SCLK time : ', SCLKCH(I) */
/*                    WRITE(*,*) '   CASSINI SCLK time: ', CLKCH */
/*                    WRITE(*,'(A,3F11.7)') */
/*             .             '    CASSINI ISS boresight:', VINERT */
/*                    WRITE(*,*) ' ' */

/*                 ELSE */

/*                    WRITE (*,*) 'Pointing not found for time ', */
/*             .                                            SCLKCH(I) */

/*                 END IF */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Requested SCLK time : 1465644281.0 */
/*            CASSINI SCLK time: 1/1465644281.171 */
/*            CASSINI ISS boresight:  0.9376789  0.3444125  0.0462419 */

/*         Requested SCLK time : 1465644351.0 */
/*            CASSINI SCLK time: 1/1465644351.071 */
/*            CASSINI ISS boresight:  0.9376657  0.3444504  0.0462266 */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     C.H. Acton         (JPL) */
/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     J.M. Lynch         (JPL) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */
/*     R.E. Thurman       (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 5.4.1, 26-MAY-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */
/*        Updated the code, input times and kernel set to work with */
/*        PDS archived CASSINI data. */

/* -    SPICELIB Version 5.4.0, 23-SEP-2013 (BVS) */

/*        Updated to save the input frame name and POOL state counter */
/*        and to do frame name-ID conversion only if the counter has */
/*        changed. */

/* -    SPICELIB Version 5.3.1, 09-JUN-2010 (BVS) */

/*        Header update: description of the tolerance and $Particulars */
/*        section were expanded to address some problems arising from */
/*        using a non-zero tolerance. */

/* -    SPICELIB Version 5.3.0, 23-APR-2010 (NJB) */

/*        Bug fix: this routine now obtains the rotation */
/*        from the request frame to the applicable CK segment's */
/*        base frame via a call to REFCHG. Formerly the routine */
/*        used FRMCHG, which required that angular velocity data */
/*        be available for this transformation. */

/* -    SPICELIB Version 5.2.0, 25-AUG-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in MXM call. */

/* -    SPICELIB Version 5.1.2, 29-JAN-2004 (NJB) */

/*        Header update: description of input argument REF was */
/*        expanded. */

/* -    SPICELIB Version 5.1.1, 27-JUL-2003 (CHA) (NJB) */

/*        Various header corrections were made. */

/* -    SPICELIB Version 3.2.0, 23-FEB-1999 (WLT) */

/*        The previous editions of this routine did not properly handle */
/*        the case when TOL was negative. The routine now returns a */
/*        value of .FALSE. for FOUND as is advertised above. */

/* -    SPICELIB Version 3.1.0, 13-APR-1998 (WLT) */

/*        The call to CHKOUT in the case when FAILED returned the */
/*        value .TRUE. used to check out with the name 'CKGPAV'. This */
/*        has been changed to a CKGP. */

/* -    SPICELIB Version 3.0.0, 19-SEP-1994 (WLT) */

/*        The routine was upgraded to support non-inertial frames. */

/* -    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 2.0.0, 30-AUG-1991 (JML) */

/*        The $Particulars section was updated to show how the */
/*        search algorithm processes segments with continuous */
/*        pointing data. */

/*        The example program now loads an SCLK kernel. */

/*        FAILED is checked after the call to IRFROT to handle the */
/*        case where the reference frame is invalid and the error */
/*        handling is not set to abort. */

/*        FAILED is checked in the DO WHILE loop to handle the case */
/*        where an error is detected by a SPICELIB routine inside the */
/*        loop and the error handling is not set to abort. */

/* -    SPICELIB Version 1.0.1, 02-NOV-1990 (JML) */

/*        The restriction that a C-kernel file must be loaded */
/*        was explicitly stated. */


/* -    SPICELIB Version 1.0.0, 07-SEP-1990 (RET) (IMU) */

/* -& */
/* $ Index_Entries */

/*     get CK pointing */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 5.2.0, 25-AUG-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in MXM call. */

/* -    SPICELIB Version 3.1.0, 13-APR-1998 (WLT) */

/*        A call to FRINFO did not have enough arguments and */
/*        went undetected until Howard Taylor of ACT. Many */
/*        thanks go out to Howard for tracking down this error. */

/* -    SPICELIB Version 3.0.0, 19-SEP-1994 (WLT) */

/*        The routine was upgraded to support non-inertial frames. */

/*        Calls to NAMIRF and IRFROT were replaced with calls to */
/*        NAMFRM and FRMCHG respectively. */


/* -    SPICELIB Version 1.0.2, 30-AUG-1991 (JML) */

/*        1) The $Particulars section was updated to show how the */
/*           search algorithm processes segments with continuous */
/*           pointing data. */

/*        2) The example program now loads an SCLK kernel. */

/*        3) FAILED is checked after the call to IRFROT to handle the */
/*           case where the reference frame is invalid and the error */
/*           handling is not set to abort. */

/*        4) FAILED is checked in the DO WHILE loop to handle the case */
/*           where an error is detected by a SPICELIB routine inside the */
/*           loop and the error handling is not set to abort. */

/* -    SPICELIB Version 1.0.1, 02-NOV-1990 (JML) */

/*        1) The restriction that a C-kernel file must be loaded */
/*           was explicitly stated. */
/*        2) Minor changes were made to the wording of the header. */


/* -    Beta Version 1.1.0, 29-AUG-1990 (MJS) */

/*        The following changes were made as a result of the */
/*        NAIF CK Code and Documentation Review: */

/*        1) The variable SCLK was changed to SCLKDP. */
/*        2) The variable INSTR was changed to INST. */
/*        3) The variable IDENT was changed to SEGID. */
/*        4) The declarations for the parameters NDC, NIC, NC, and */
/*           IDLEN were moved from the "Declarations" section of the */
/*           header to the "Local parameters" section of the code below */
/*           the header. These parameters are not meant to modified by */
/*           users. */
/*        5) The header was updated to reflect the changes. */

/* -    Beta Version 1.0.0, 04-MAY-1990 (RET) (IMU) */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */

/*        NDC        is the number of double precision components in an */
/*                   unpacked C-kernel segment descriptor. */

/*        NIC        is the number of integer components in an unpacked */
/*                   C-kernel segment descriptor. */

/*        NC         is the number of components in a packed C-kernel */
/*                   descriptor.  All DAF summaries have this formulaic */
/*                   relationship between the number of its integer and */
/*                   double precision components and the number of packed */
/*                   components. */

/*        IDLEN      is the length of the C-kernel segment identifier. */
/*                   All DAF names have this formulaic relationship */
/*                   between the number of summary components and */
/*                   the length of the name (You will notice that */
/*                   a name and a summary have the same length in bytes.) */


/*     Saved frame name length. */


/*     Local variables */


/*     Saved frame name/ID item declarations. */


/*     Saved frame name/ID items. */


/*     Initial values. */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("CKGP", (ftnlen)4);
    }

/*     Initialization. */

    if (first) {

/*        Initialize counter. */

	zzctruin_(svctr1);
	first = FALSE_;
    }

/*     Don't need angular velocity data. */
/*     Assume the segment won't be found until it really is. */

    needav = FALSE_;
    *found = FALSE_;

/*     If the tolerance is less than zero, we go no further. */

    if (*tol < 0.) {
	chkout_("CKGP", (ftnlen)4);
	return 0;
    }

/*     Begin a search for this instrument and time, and get the first */
/*     applicable segment. */

    ckbss_(inst, sclkdp, tol, &needav);
    cksns_(&handle, descr, segid, &sfnd, (ftnlen)40);

/*     Keep trying candidate segments until a segment can produce a */
/*     pointing instance within the specified time tolerance of the */
/*     input time. */

/*     Check FAILED to prevent an infinite loop if an error is detected */
/*     by a SPICELIB routine and the error handling is not set to abort. */

    while(sfnd && ! failed_()) {
	ckpfs_(&handle, descr, sclkdp, tol, &needav, cmat, av, clkout, &pfnd);
	if (pfnd) {

/*           Found one. If the C-matrix doesn't already rotate from the */
/*           requested frame, convert it to one that does. */

	    dafus_(descr, &c__2, &c__6, dcd, icd);
	    refseg = icd[1];

/*           Look up the id code for the requested reference frame. */

	    zznamfrm_(svctr1, svref, &svrefr, ref, &refreq, (ftnlen)32, 
		    ref_len);
	    if (refreq != refseg) {

/*              We may need to convert the output ticks CLKOUT to ET */
/*              so that we can get the needed state transformation */
/*              matrix.  This is the case if either of the frames */
/*              is non-inertial. */

		frinfo_(&refreq, &center, &type1, &typeid, &gotit);
		frinfo_(&refseg, &center, &type2, &typeid, &gotit);
		if (type1 == 1 && type2 == 1) {

/*                 Any old value of ET will do in this case.  We'll */
/*                 use zero. */

		    et = 0.;
		} else {

/*                 Look up the spacecraft clock id to use to convert */
/*                 the output CLKOUT to ET. */

		    ckmeta_(inst, "SCLK", &sclk, (ftnlen)4);
		    sct2e_(&sclk, clkout, &et);
		}

/*              Get the transformation from the requested frame to */
/*              the segment frame at ET. */

		refchg_(&refreq, &refseg, &et, rot);

/*              If REFCHG detects that the reference frame is invalid */
/*              then return from this routine with FOUND equal to false. */

		if (failed_()) {
		    chkout_("CKGP", (ftnlen)4);
		    return 0;
		}

/*              Transform the attitude information: convert CMAT so that */
/*              it maps from request frame to C-matrix frame. */

		mxm_(cmat, rot, tmpmat);
		moved_(tmpmat, &c__9, cmat);
	    }
	    *found = TRUE_;
	    chkout_("CKGP", (ftnlen)4);
	    return 0;
	}
	cksns_(&handle, descr, segid, &sfnd, (ftnlen)40);
    }
    chkout_("CKGP", (ftnlen)4);
    return 0;
} /* ckgp_ */

