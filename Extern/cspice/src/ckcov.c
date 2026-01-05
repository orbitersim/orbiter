/* ckcov.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__6 = 6;

/* $Procedure CKCOV ( CK coverage ) */
/* Subroutine */ int ckcov_(char *ckfnm, integer *idcode, logical *needav, 
	char *level, doublereal *tol, char *timsys, doublereal *cover, ftnlen 
	ckfnm_len, ftnlen level_len, ftnlen timsys_len)
{
    /* System generated locals */
    integer i__1;
    doublereal d__1;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen), s_rnge(char *, integer, 
	    char *, integer);

    /* Local variables */
    char arch[80];
    logical avok;
    extern /* Subroutine */ int sct2e_(integer *, doublereal *, doublereal *);
    integer i__;
    extern /* Subroutine */ int dafgs_(doublereal *);
    integer clkid;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    doublereal descr[5];
    extern /* Subroutine */ int dafus_(doublereal *, integer *, integer *, 
	    doublereal *, integer *), errch_(char *, char *, ftnlen, ftnlen);
    doublereal dctol[2];
    logical istdb, found;
    extern /* Subroutine */ int errdp_(char *, doublereal *, ftnlen);
    integer dtype;
    extern logical eqstr_(char *, char *, ftnlen, ftnlen);
    doublereal dc[2];
    integer ic[6];
    extern /* Subroutine */ int daffna_(logical *);
    extern logical failed_(void);
    extern /* Subroutine */ int dafbfs_(integer *);
    doublereal et;
    integer handle, segbeg;
    extern /* Subroutine */ int dafcls_(integer *), ckmeta_(integer *, char *,
	     integer *, ftnlen);
    integer segend;
    extern /* Subroutine */ int getfat_(char *, char *, char *, ftnlen, 
	    ftnlen, ftnlen), dafopr_(char *, integer *, ftnlen), sigerr_(char 
	    *, ftnlen);
    logical seglvl;
    extern /* Subroutine */ int chkout_(char *, ftnlen), setmsg_(char *, 
	    ftnlen), wninsd_(doublereal *, doublereal *, doublereal *), 
	    errint_(char *, integer *, ftnlen);
    char kertyp[80];
    extern logical return_(void);
    extern /* Subroutine */ int zzckcv01_(integer *, integer *, integer *, 
	    integer *, doublereal *, char *, doublereal *, ftnlen), zzckcv02_(
	    integer *, integer *, integer *, integer *, doublereal *, char *, 
	    doublereal *, ftnlen), zzckcv03_(integer *, integer *, integer *, 
	    integer *, doublereal *, char *, doublereal *, ftnlen), zzckcv04_(
	    integer *, integer *, integer *, integer *, doublereal *, char *, 
	    doublereal *, ftnlen), zzckcv05_(integer *, integer *, integer *, 
	    integer *, doublereal *, doublereal *, char *, doublereal *, 
	    ftnlen), zzckcv06_(integer *, integer *, integer *, integer *, 
	    doublereal *, doublereal *, char *, doublereal *, ftnlen);

/* $ Abstract */

/*     Find the coverage window for a specified object in a specified CK */
/*     file. */

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

/*     CELLS */
/*     DAF */
/*     CK */
/*     TIME */
/*     WINDOWS */

/* $ Keywords */

/*     POINTING */
/*     TIME */
/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     CKFNM      I   Name of CK file. */
/*     IDCODE     I   ID code of object. */
/*     NEEDAV     I   Flag indicating whether angular velocity is needed. */
/*     LEVEL      I   Coverage level: 'SEGMENT' OR 'INTERVAL'. */
/*     TOL        I   Tolerance in ticks. */
/*     TIMSYS     I   Time system used to represent coverage. */
/*     COVER     I-O  Window giving coverage for IDCODE. */

/* $ Detailed_Input */

/*     CKFNM    is the name of a C-kernel. */

/*     IDCODE   is the integer ID code of an object, normally a */
/*              spacecraft structure or instrument, for which pointing */
/*              data are expected to exist in the specified CK file. */

/*     NEEDAV   is a logical variable indicating whether only segments */
/*              having angular velocity are to be considered when */
/*              determining coverage. When NEEDAV is .TRUE., segments */
/*              without angular velocity don't contribute to the coverage */
/*              window; when NEEDAV is .FALSE., all segments for IDCODE */
/*              may contribute to the coverage window. */

/*     LEVEL    is the level (granularity) at which the coverage is */
/*              examined. Allowed values and corresponding meanings are: */

/*                 'SEGMENT'    The output coverage window contains */
/*                              intervals defined by the start and stop */
/*                              times of segments for the object */
/*                              designated by IDCODE. */

/*                 'INTERVAL'   The output coverage window contains */
/*                              interpolation intervals of segments for */
/*                              the object designated by IDCODE. For type */
/*                              1 segments, which don't have */
/*                              interpolation intervals, each epoch */
/*                              associated with a pointing instance is */
/*                              treated as a singleton interval; these */
/*                              intervals are added to the coverage */
/*                              window. */

/*                              All interpolation intervals are */
/*                              considered to lie within the segment */
/*                              bounds for the purpose of this summary: */
/*                              if an interpolation interval extends */
/*                              beyond the segment coverage interval, */
/*                              only its intersection with the segment */
/*                              coverage interval is considered to */
/*                              contribute to the total coverage. */

/*     TOL      is a tolerance value expressed in ticks of the spacecraft */
/*              clock associated with IDCODE. Before each interval is */
/*              inserted into the coverage window, the interval is */
/*              intersected with the segment coverage interval, then if */
/*              the intersection is non-empty, it is expanded by TOL: the */
/*              left endpoint of the intersection interval is reduced by */
/*              TOL and the right endpoint is increased by TOL. Adjusted */
/*              interval endpoints, when expressed as encoded SCLK, never */
/*              are less than zero ticks. Any intervals that overlap as a */
/*              result of the expansion are merged. */

/*              The coverage window returned when TOL > 0 indicates the */
/*              coverage provided by the file to the CK readers CKGPAV */
/*              and CKGP when that value of TOL is passed to them as an */
/*              input. */

/*     TIMSYS   is a string indicating the time system used in the output */
/*              coverage window. TIMSYS may have the values: */

/*                  'SCLK'    Elements of COVER are expressed in encoded */
/*                            SCLK ("ticks"), where the clock is */
/*                            associated with the object designated by */
/*                            IDCODE. */

/*                  'TDB'     Elements of COVER are expressed as seconds */
/*                            past J2000 TDB. */


/*     COVER    is an initialized SPICE window data structure. COVER */
/*              optionally may contain coverage data on input; on output, */
/*              the data already present in COVER will be combined with */
/*              coverage found for the object designated by IDCODE in the */
/*              file CKFNM. */

/*              If COVER contains no data on input, its size and */
/*              cardinality still must be initialized. */

/* $ Detailed_Output */

/*     COVER    is a SPICE window data structure which represents the */
/*              merged coverage for IDCODE. When the coverage level is */
/*              'INTERVAL', this is the set of time intervals for which */
/*              data for IDCODE are present in the file CKFNM, merged */
/*              with the set of time intervals present in COVER on input. */
/*              The merged coverage is represented as the union of one or */
/*              more disjoint time intervals. The window COVER contains */
/*              the pairs of endpoints of these intervals. */

/*              When the coverage level is 'SEGMENT', COVER is computed */
/*              in a manner similar to that described above, but the */
/*              coverage intervals used in the computation are those of */
/*              segments rather than interpolation intervals within */
/*              segments. */

/*              When TOL is > 0, the intervals comprising the coverage */
/*              window for IDCODE are expanded by TOL and any intervals */
/*              overlapping as a result are merged. The resulting window */
/*              is returned in COVER. The expanded window in no case */
/*              extends beyond the segment bounds in either direction by */
/*              more than TOL. */

/*              The interval endpoints contained in COVER are encoded */
/*              spacecraft clock times if TIMSYS is 'SCLK'; otherwise the */
/*              times are converted from encoded spacecraft clock to */
/*              seconds past J2000 TDB. */

/*              See the $Examples section below for a complete example */
/*              program showing how to retrieve the endpoints from COVER. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the input file has transfer format, the error */
/*         SPICE(INVALIDFORMAT) is signaled. */

/*     2)  If the input file is not a transfer file but has architecture */
/*         other than DAF, the error SPICE(INVALIDARCHTYPE) is signaled. */

/*     3)  If the input file is a binary DAF file of type other than CK, */
/*         the error SPICE(INVALIDFILETYPE) is signaled. */

/*     4)  If the CK file cannot be opened or read, an error is signaled */
/*         by a routine in the call tree of this routine. The output */
/*         window will not be modified. */

/*     5)  If the size of the output window argument COVER is */
/*         insufficient to contain the actual number of intervals in the */
/*         coverage window for IDCODE, an error is signaled by a routine */
/*         in the call tree of this routine. */

/*     6)  If TOL is negative, the error SPICE(VALUEOUTOFRANGE) is */
/*         signaled. */

/*     7)  If LEVEL is not recognized, the error SPICE(INVALIDOPTION) */
/*         is signaled. */

/*     8)  If TIMSYS is not recognized, the error SPICE(NOTSUPPORTED) */
/*         is signaled. */

/*     9)  If a time conversion error occurs, the error is signaled by a */
/*         routine in the call tree of this routine. */

/*     10) If the output time system is TDB, the CK subsystem must be */
/*         able to map IDCODE to the ID code of the associated spacecraft */
/*         clock. If this mapping cannot be performed, an error is */
/*         signaled by a routine in the call tree of this routine. */

/*     11) If the input CK type is not one of the supported CK types, the */
/*         error SPICE(NOTSUPPORTED) is signaled. This problem may */
/*         indicate the version of the SPICE Toolkit being used is */
/*         outdated and a new version is required. */

/* $ Files */

/*     This routine reads a C-kernel. */

/*     If the output time system is 'TDB', then a leapseconds kernel */
/*     and an SCLK kernel for the spacecraft clock associated with */
/*     IDCODE must be loaded before this routine is called. */

/*     If the ID code of the clock associated with IDCODE is not */
/*     equal to */

/*        IDCODE / 1000 */

/*     then the kernel variable */

/*        CK_<IDCODE>_SCLK */

/*     must be present in the kernel pool to identify the clock */
/*     associated with IDCODE. This variable must contain the ID code */
/*     to be used for conversion between SCLK and TDB. Normally this */
/*     variable is provided in a text kernel loaded via FURNSH. */

/* $ Particulars */

/*     This routine provides an API via which applications can determine */
/*     the coverage a specified CK file provides for a specified */
/*     object. */

/* $ Examples */

/*     The numerical results shown for these examples may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Display the interval-level coverage for each object in a */
/*        specified CK file. Use tolerance of zero ticks. Do not */
/*        request angular velocity. Express the results in the TDB time */
/*        system. */

/*        Find the set of objects in the file. Loop over the contents */
/*        of the ID code set: find the coverage for each item in the */
/*        set and display the coverage. */


/*        Example code begins here. */


/*              PROGRAM CKCOV_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     SPICELIB functions */
/*        C */
/*              INTEGER               WNCARD */
/*              INTEGER               CARDI */
/*        C */
/*        C     Local parameters */
/*        C */
/*        C */
/*        C     Declare the coverage window.  Make enough room */
/*        C     for MAXIV intervals. */
/*        C */
/*              INTEGER               FILSIZ */
/*              PARAMETER           ( FILSIZ = 255 ) */

/*              INTEGER               LBCELL */
/*              PARAMETER           ( LBCELL = -5 ) */

/*              INTEGER               MAXIV */
/*              PARAMETER           ( MAXIV  = 100000 ) */

/*              INTEGER               WINSIZ */
/*              PARAMETER           ( WINSIZ = 2 * MAXIV ) */

/*              INTEGER               TIMLEN */
/*              PARAMETER           ( TIMLEN = 50 ) */

/*              INTEGER               MAXOBJ */
/*              PARAMETER           ( MAXOBJ = 1000 ) */

/*        C */
/*        C     Local variables */
/*        C */
/*              CHARACTER*(FILSIZ)    CKFNM */
/*              CHARACTER*(FILSIZ)    LSK */
/*              CHARACTER*(FILSIZ)    SCLK */
/*              CHARACTER*(TIMLEN)    TIMSTR */

/*              DOUBLE PRECISION      B */
/*              DOUBLE PRECISION      COVER ( LBCELL : WINSIZ ) */
/*              DOUBLE PRECISION      E */

/*              INTEGER               I */
/*              INTEGER               IDS   ( LBCELL : MAXOBJ ) */
/*              INTEGER               J */
/*              INTEGER               NIV */

/*        C */
/*        C     Load a leapseconds kernel and SCLK kernel for output */
/*        C     time conversion.  Note that we assume a single spacecraft */
/*        C     clock is associated with all of the objects in the CK. */
/*        C */
/*              CALL PROMPT ( 'Name of leapseconds kernel > ', LSK  ) */
/*              CALL FURNSH ( LSK ) */

/*              CALL PROMPT ( 'Name of SCLK kernel        > ', SCLK ) */
/*              CALL FURNSH ( SCLK ) */

/*        C */
/*        C     Get name of CK file. */
/*        C */
/*              CALL PROMPT ( 'Name of CK file            > ', CKFNM ) */

/*        C */
/*        C     Initialize the set IDS. */
/*        C */
/*              CALL SSIZEI ( MAXOBJ, IDS ) */

/*        C */
/*        C     Initialize the window COVER. */
/*        C */
/*              CALL SSIZED ( WINSIZ, COVER ) */

/*        C */
/*        C     Find the set of objects in the CK file. */
/*        C */
/*              CALL CKOBJ ( CKFNM, IDS ) */

/*        C */
/*        C     We want to display the coverage for each object.  Loop */
/*        C     over the contents of the ID code set, find the coverage */
/*        C     for each item in the set, and display the coverage. */
/*        C */
/*              DO I = 1, CARDI( IDS ) */
/*        C */
/*        C        Find the coverage window for the current */
/*        C        object. Empty the coverage window each time */
/*        C        so we don't include data for the previous object. */
/*        C */
/*                 CALL SCARDD ( 0,   COVER ) */
/*                 CALL CKCOV  ( CKFNM,       IDS(I),  .FALSE., */
/*             .                 'INTERVAL',  0.D0,    'TDB',    COVER ) */

/*        C */
/*        C        Get the number of intervals in the coverage */
/*        C        window. */
/*        C */
/*                 NIV = WNCARD( COVER ) */

/*        C */
/*        C        Display a simple banner. */
/*        C */
/*                 WRITE (*,*) '========================================' */
/*                 WRITE (*,*) 'Coverage for object ', IDS(I) */

/*        C */
/*        C        Convert the coverage interval start and stop */
/*        C        times to TDB calendar strings. */
/*        C */
/*                 DO J = 1, NIV */
/*        C */
/*        C           Get the endpoints of the Jth interval. */
/*        C */
/*                    CALL WNFETD ( COVER, J, B, E ) */
/*        C */
/*        C           Convert the endpoints to TDB calendar */
/*        C           format time strings and display them. */
/*        C */
/*                    CALL TIMOUT ( B, */
/*             .                    'YYYY MON DD HR:MN:SC.###### ' // */
/*             .                    '(TDB) ::TDB', */
/*             .                    TIMSTR                           ) */
/*                    WRITE (*,*) ' ' */
/*                    WRITE (*,*) 'Interval: ', J */
/*                    WRITE (*,*) 'Start:    ', TIMSTR */

/*                    CALL TIMOUT ( E, */
/*             .                    'YYYY MON DD HR:MN:SC.###### ' // */
/*             .                    '(TDB) ::TDB', */
/*             .                    TIMSTR                          ) */
/*                    WRITE (*,*) 'Stop:     ', TIMSTR */
/*                    WRITE (*,*) ' ' */

/*                 END DO */

/*                 WRITE (*,*) '========================================' */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, using the LSK file named naif0010.tls, the SCLK file */
/*        named cas00145.tsc and the CK file named 08052_08057ra.bc, the */
/*        output was: */


/*        Name of leapseconds kernel > naif0010.tls */
/*        Name of SCLK kernel        > cas00145.tsc */
/*        Name of CK file            > 08052_08057ra.bc */
/*         ======================================== */
/*         Coverage for object       -82000 */

/*         Interval:            1 */
/*         Start:    2008 FEB 21 00:01:07.771186 (TDB) */
/*         Stop:     2008 FEB 23 22:53:30.001738 (TDB) */


/*         Interval:            2 */
/*         Start:    2008 FEB 23 22:58:13.999732 (TDB) */
/*         Stop:     2008 FEB 24 02:22:25.913175 (TDB) */


/*         Interval:            3 */
/*         Start:    2008 FEB 24 02:27:49.910886 (TDB) */
/*         Stop:     2008 FEB 24 19:46:33.470587 (TDB) */


/*         Interval:            4 */
/*         Start:    2008 FEB 24 19:49:33.469315 (TDB) */
/*         Stop:     2008 FEB 25 04:25:21.250677 (TDB) */


/*         Interval:            5 */
/*         Start:    2008 FEB 25 04:29:33.248897 (TDB) */
/*         Stop:     2008 FEB 25 15:23:44.971594 (TDB) */


/*         Interval:            6 */
/*         Start:    2008 FEB 25 15:24:12.971396 (TDB) */
/*         Stop:     2008 FEB 25 20:25:04.843864 (TDB) */


/*         Interval:            7 */
/*         Start:    2008 FEB 25 20:25:48.843553 (TDB) */
/*         Stop:     2008 FEB 26 00:01:04.752306 (TDB) */

/*         ======================================== */


/*     2) Find the segment-level coverage for the object designated by */
/*        IDCODE provided by the set of CK files loaded via a */
/*        metakernel. (The metakernel must also specify leapseconds and */
/*        SCLK kernels.) Use tolerance of zero ticks. Do not request */
/*        angular velocity. Express the results in the TDB time system. */

/*        Use the meta-kernel shown below to load the required SPICE */
/*        kernels. */


/*           KPL/MK */

/*           File name: ckcov_ex2.tm */

/*           This meta-kernel is intended to support operation of SPICE */
/*           example programs. The kernels shown here should not be */
/*           assumed to contain adequate or correct versions of data */
/*           required by SPICE-based user applications. */

/*           In order for an application to use this meta-kernel, the */
/*           kernels referenced here must be present in the user's */
/*           current working directory. */

/*           The names and contents of the kernels referenced */
/*           by this meta-kernel are as follows: */

/*             File name                      Contents */
/*             ---------                      -------- */
/*             naif0010.tls                   Leapseconds */
/*             cas00145.tsc                   Cassini SCLK */
/*             08052_08057ra.bc               Orientation for Cassini */

/*           \begindata */

/*             KERNELS_TO_LOAD = ( 'naif0010.tls' */
/*                                 'cas00145.tsc' */
/*                                 '08052_08057ra.bc') */

/*           \begintext */

/*           End of meta-kernel */


/*        Example code begins here. */


/*              PROGRAM CKCOV_EX2 */
/*              IMPLICIT NONE */
/*        C */
/*        C     SPICELIB functions */
/*        C */
/*              INTEGER               WNCARD */

/*        C */
/*        C     Local parameters */
/*        C */
/*              INTEGER               LBCELL */
/*              PARAMETER           ( LBCELL = -5 ) */

/*              INTEGER               FILSIZ */
/*              PARAMETER           ( FILSIZ = 255 ) */

/*              INTEGER               LNSIZE */
/*              PARAMETER           ( LNSIZE = 80 ) */

/*              INTEGER               MAXCOV */
/*              PARAMETER           ( MAXCOV = 100000 ) */

/*              INTEGER               TIMLEN */
/*              PARAMETER           ( TIMLEN = 50 ) */

/*        C */
/*        C     Local variables */
/*        C */
/*              CHARACTER*(FILSIZ)    FILE */
/*              CHARACTER*(LNSIZE)    IDCH */
/*              CHARACTER*(FILSIZ)    META */
/*              CHARACTER*(FILSIZ)    SOURCE */
/*              CHARACTER*(TIMLEN)    TIMSTR */
/*              CHARACTER*(LNSIZE)    TYPE */

/*              DOUBLE PRECISION      B */
/*              DOUBLE PRECISION      COVER  ( LBCELL : 2*MAXCOV ) */
/*              DOUBLE PRECISION      E */

/*              INTEGER               COUNT */
/*              INTEGER               HANDLE */
/*              INTEGER               I */
/*              INTEGER               IDCODE */
/*              INTEGER               NIV */

/*              LOGICAL               FOUND */

/*        C */
/*        C     Prompt for the metakernel name; load the metakernel. */
/*        C     The metakernel lists the CK files whose coverage */
/*        C     for IDCODE we'd like to determine.  The metakernel */
/*        C     must also specify a leapseconds kernel and an SCLK */
/*        C     kernel for the clock associated with IDCODE. */
/*        C */
/*              CALL PROMPT ( 'Enter name of metakernel > ', META ) */

/*              CALL FURNSH ( META ) */

/*        C */
/*        C     Get the ID code of interest. */
/*        C */
/*              CALL PROMPT ( 'Enter ID code            > ', IDCH ) */

/*              CALL PRSINT ( IDCH,  IDCODE ) */

/*        C */
/*        C     Initialize the coverage window. */
/*        C */
/*              CALL SSIZED ( MAXCOV, COVER ) */

/*        C */
/*        C     Find out how many kernels are loaded.  Loop over the */
/*        C     kernels:  for each loaded CK file, add its coverage */
/*        C     for IDCODE, if any, to the coverage window. */
/*        C */
/*              CALL KTOTAL ( 'CK', COUNT ) */

/*              DO I = 1, COUNT */

/*                 CALL KDATA ( I,       'CK',    FILE,  TYPE, */
/*             .                SOURCE,  HANDLE,  FOUND       ) */

/*                 CALL CKCOV ( FILE,      IDCODE,  .FALSE., */
/*             .                'SEGMENT', 0.D0,    'TDB',    COVER ) */

/*              END DO */

/*        C */
/*        C     Display results. */
/*        C */
/*        C     Get the number of intervals in the coverage */
/*        C     window. */
/*        C */
/*              NIV = WNCARD( COVER ) */

/*        C */
/*        C     Display a simple banner. */
/*        C */
/*              WRITE (*,*) ' ' */
/*              WRITE (*,*) 'Coverage for object ', IDCODE */

/*        C */
/*        C     Convert the coverage interval start and stop */
/*        C     times to TDB calendar strings. */
/*        C */
/*              DO I = 1, NIV */
/*        C */
/*        C        Get the endpoints of the Ith interval. */
/*        C */
/*                 CALL WNFETD ( COVER, I, B, E ) */
/*        C */
/*        C        Convert the endpoints to TDB calendar */
/*        C        format time strings and display them. */
/*        C */
/*                 CALL TIMOUT ( B, */
/*             .                 'YYYY MON DD HR:MN:SC.###### ' // */
/*             .                 '(TDB) ::TDB', */
/*             .                 TIMSTR                           ) */
/*                 WRITE (*,*) ' ' */
/*                 WRITE (*,*) 'Interval: ', I */
/*                 WRITE (*,*) 'Start:    ', TIMSTR */

/*                 CALL TIMOUT ( E, */
/*             .                 'YYYY MON DD HR:MN:SC.###### ' // */
/*             .                 '(TDB) ::TDB', */
/*             .                 TIMSTR                           ) */
/*                 WRITE (*,*) 'Stop:     ', TIMSTR */
/*                 WRITE (*,*) ' ' */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, using the meta-kernel file named ckcov_ex2.tm and */
/*        the NAIF ID "-82000" (Cassini spacecraft bus), the output was: */


/*        Enter name of metakernel > ckcov_ex2.tm */
/*        Enter ID code            > -82000 */

/*         Coverage for object       -82000 */

/*         Interval:            1 */
/*         Start:    2008 FEB 21 00:01:07.771186 (TDB) */
/*         Stop:     2008 FEB 26 00:01:04.752306 (TDB) */


/* $ Restrictions */

/*     1)  When this routine is used to accumulate coverage for IDCODE */
/*         provided by multiple CK files, the inputs NEEDAV, LEVEL, TOL, */
/*         and TIMSYS  must have the same values for all files in order */
/*         for the result to be meaningful. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.1.0, 08-OCT-2021 (JDR) */

/*        Bug fix: added call to FAILED after call to GETFAT. */

/*        Changed input argument name "CK" to "CKFNM" for consistency */
/*        with other routines. */

/*        Edited the header to comply with NAIF standard. Added solutions */
/*        using CASSINI data. Fixed a bug on Example #2. Added entry #11 */
/*        in $Exceptions section and corrected short error messages in */
/*        entry #2 and #3. */

/* -    SPICELIB Version 2.0.0, 05-JAN-2014 (NJB) (BVS) */

/*        Updated index entries. */

/*        Last update was 05-JAN-2014 (NJB) (BVS) */

/*           Updated to support type 6. */

/* -    SPICELIB Version 1.0.1, 30-NOV-2007 (NJB) */

/*        Corrected bug in first program in header $Examples section: */
/*        program now empties the coverage window prior to collecting */
/*        data for the current object. Updated examples to use WNCARD */
/*        rather than CARDD. */

/* -    SPICELIB Version 1.0.0, 07-JAN-2005 (NJB) */

/* -& */
/* $ Index_Entries */

/*     get coverage window for ck_object */
/*     get coverage start and stop time for ck_object */
/*     get coverage start and stop time for CK frame */
/*     get coverage start and stop time for CK instrument */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("CKCOV", (ftnlen)5);

/*     Check tolerance value. */

    if (*tol < 0.) {
	setmsg_("Tolerance must be non-negative; actual value was #.", (
		ftnlen)51);
	errdp_("#", tol, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("CKCOV", (ftnlen)5);
	return 0;
    }

/*     Use a logical flag to indicate whether this is a segment-level */
/*     coverage description. */

    seglvl = eqstr_(level, "SEGMENT", level_len, (ftnlen)7);

/*     Check coverage level keyword. */

    if (! (seglvl || eqstr_(level, "INTERVAL", level_len, (ftnlen)8))) {
	setmsg_("Allowed values of LEVEL are # and #; actual value was #.", (
		ftnlen)56);
	errch_("#", "SEGMENT", (ftnlen)1, (ftnlen)7);
	errch_("#", "INTERVAL", (ftnlen)1, (ftnlen)8);
	errch_("#", level, (ftnlen)1, level_len);
	sigerr_("SPICE(INVALIDOPTION)", (ftnlen)20);
	chkout_("CKCOV", (ftnlen)5);
	return 0;
    }

/*     See whether GETFAT thinks we've got a CK file. */

    getfat_(ckfnm, arch, kertyp, ckfnm_len, (ftnlen)80, (ftnlen)80);
    if (failed_()) {
	chkout_("CKCOV", (ftnlen)5);
	return 0;
    }
    if (s_cmp(arch, "XFR", (ftnlen)80, (ftnlen)3) == 0) {
	setmsg_("Input file # has architecture #. The file must be a binary "
		"CK file to be readable by this routine.  If the input file i"
		"s an CK file in transfer format, run TOBIN on the file to co"
		"nvert it to binary format.", (ftnlen)205);
	errch_("#", ckfnm, (ftnlen)1, ckfnm_len);
	errch_("#", arch, (ftnlen)1, (ftnlen)80);
	sigerr_("SPICE(INVALIDFORMAT)", (ftnlen)20);
	chkout_("CKCOV", (ftnlen)5);
	return 0;
    } else if (s_cmp(arch, "DAF", (ftnlen)80, (ftnlen)3) != 0) {
	setmsg_("Input file # has architecture #. The file must be a binary "
		"CK file to be readable by this routine.  Binary CK files hav"
		"e DAF architecture.  If you expected the file to be a binary"
		" CK file, the problem may be due to the file being an old no"
		"n-native file lacking binary file format information. It's a"
		"lso possible the file has been corrupted.", (ftnlen)340);
	errch_("#", ckfnm, (ftnlen)1, ckfnm_len);
	errch_("#", arch, (ftnlen)1, (ftnlen)80);
	sigerr_("SPICE(INVALIDARCHTYPE)", (ftnlen)22);
	chkout_("CKCOV", (ftnlen)5);
	return 0;
    } else if (s_cmp(kertyp, "CK", (ftnlen)80, (ftnlen)2) != 0) {
	setmsg_("Input file # has file type #. The file must be a binary CK "
		"file to be readable by this routine. If you expected the fil"
		"e to be a binary CK file, the problem may be due to the file"
		" being an old non-native file lacking binary file format inf"
		"ormation. It's also possible the file has been corrupted.", (
		ftnlen)296);
	errch_("#", ckfnm, (ftnlen)1, ckfnm_len);
	errch_("#", kertyp, (ftnlen)1, (ftnlen)80);
	sigerr_("SPICE(INVALIDFILETYPE)", (ftnlen)22);
	chkout_("CKCOV", (ftnlen)5);
	return 0;
    }

/*     Set a logical flag indicating whether the time system is SCLK. */

    istdb = eqstr_(timsys, "TDB", timsys_len, (ftnlen)3);

/*     Check time system. */

    if (! istdb) {
	if (! eqstr_(timsys, "SCLK", timsys_len, (ftnlen)4)) {
	    setmsg_("Time system spec TIMSYS was #; allowed values are SCLK "
		    "and TDB.", (ftnlen)63);
	    errch_("#", timsys, (ftnlen)1, timsys_len);
	    sigerr_("SPICE(NOTSUPPORTED)", (ftnlen)19);
	    chkout_("CKCOV", (ftnlen)5);
	    return 0;
	}
    }

/*     If the output time system is TDB, find the clock ID associated */
/*     with IDCODE. */

    if (istdb) {
	ckmeta_(idcode, "SCLK", &clkid, (ftnlen)4);
	if (failed_()) {
	    chkout_("CKCOV", (ftnlen)5);
	    return 0;
	}
    }

/*     Open the file for reading. */

    dafopr_(ckfnm, &handle, ckfnm_len);
    if (failed_()) {
	chkout_("CKCOV", (ftnlen)5);
	return 0;
    }

/*     We will examine each segment descriptor in the file, and */
/*     we'll update our coverage bounds according to the data found */
/*     in these descriptors. */

/*     If TOL > 0, we'll apply TOL after we've found the coverage */
/*     for the zero-tolerance case. */

/*     If the time system is TDB, we'll convert the times to TDB */
/*     at the end of this routine. */

/*     Start a forward search. */

    dafbfs_(&handle);

/*     Find the next DAF array. */

    daffna_(&found);
    while(found) {

/*        Note:  we check FAILED() at the bottom of this loop; this */
/*        routine returns if FAILED() returns .TRUE. at that point. */

/*        Fetch and unpack the segment descriptor. */

	dafgs_(descr);
	dafus_(descr, &c__2, &c__6, dc, ic);

/*        Let AVOK indicate whether the segment satisfies the */
/*        angular velocity restriction. */

	avok = ic[3] == 1 || ! (*needav);
	if (ic[0] == *idcode && avok) {

/*           This segment is for the body of interest.  If angular */
/*           velocity is needed, this segment has it. */

	    if (seglvl) {

/*              This is a segment-level summary. */

/*              Insert the coverage bounds into the coverage window. */
/*              Adjust the interval using the tolerance. */

/* Computing MAX */
		d__1 = dc[0] - *tol;
		dctol[0] = max(d__1,0.);
		dctol[1] = dc[1] + *tol;

/*              Convert the time to TDB if necessary. */

		if (istdb) {

/*                 Convert the time bounds to TDB before inserting */
/*                 into the window. */

		    for (i__ = 1; i__ <= 2; ++i__) {
			sct2e_(&clkid, &dctol[(i__1 = i__ - 1) < 2 && 0 <= 
				i__1 ? i__1 : s_rnge("dctol", i__1, "ckcov_", 
				(ftnlen)998)], &et);
			dctol[(i__1 = i__ - 1) < 2 && 0 <= i__1 ? i__1 : 
				s_rnge("dctol", i__1, "ckcov_", (ftnlen)999)] 
				= et;
		    }
		}
		if (dctol[0] <= dctol[1]) {
		    wninsd_(dctol, &dctol[1], cover);
		}
	    } else {

/*              We're looking for an interval-level coverage window. */
/*              This information must be retrieved in a */
/*              data-type-dependent fashion.  The coverage routines */
/*              we'll call will, if necessary, adjust intervals by TOL */
/*              and convert interval times to TDB. */

		dtype = ic[2];
		segbeg = ic[4];
		segend = ic[5];
		if (dtype == 1) {
		    zzckcv01_(&handle, &segbeg, &segend, &clkid, tol, timsys, 
			    cover, timsys_len);
		} else if (dtype == 2) {
		    zzckcv02_(&handle, &segbeg, &segend, &clkid, tol, timsys, 
			    cover, timsys_len);
		} else if (dtype == 3) {
		    zzckcv03_(&handle, &segbeg, &segend, &clkid, tol, timsys, 
			    cover, timsys_len);
		} else if (dtype == 4) {
		    zzckcv04_(&handle, &segbeg, &segend, &clkid, tol, timsys, 
			    cover, timsys_len);
		} else if (dtype == 5) {
		    zzckcv05_(&handle, &segbeg, &segend, &clkid, dc, tol, 
			    timsys, cover, timsys_len);
		} else if (dtype == 6) {
		    zzckcv06_(&handle, &segbeg, &segend, &clkid, dc, tol, 
			    timsys, cover, timsys_len);
		} else {
		    setmsg_("Supported CK data types are 1, 2, 3, 4, 5.  Dat"
			    "a type of segment: #. This problem may indicate "
			    "that you need to update your SPICE Toolkit.", (
			    ftnlen)138);
		    errint_("#", &dtype, (ftnlen)1);
		    sigerr_("SPICE(NOTSUPPORTED)", (ftnlen)19);
		    chkout_("CKCOV", (ftnlen)5);
		    return 0;
		}
	    }
	}
	daffna_(&found);
	if (failed_()) {
	    chkout_("CKCOV", (ftnlen)5);
	    return 0;
	}
    }

/*     COVER now represents the coverage of the entire file at the */
/*     granularity indicated by LEVEL, combined with the coverage */
/*     contained in COVER on input. */

/*     Release the file. */

    dafcls_(&handle);
    chkout_("CKCOV", (ftnlen)5);
    return 0;
} /* ckcov_ */

