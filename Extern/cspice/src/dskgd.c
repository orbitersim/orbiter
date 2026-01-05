/* dskgd.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure DSKGD ( DSK, return DSK segment descriptor  ) */
/* Subroutine */ int dskgd_(integer *handle, integer *dladsc, doublereal *
	dskdsc)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer dpbase;
    extern /* Subroutine */ int dasrdd_(integer *, integer *, integer *, 
	    doublereal *), sigerr_(char *, ftnlen);
    integer dpsize;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen), chkout_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Return the DSK descriptor from a DSK segment identified */
/*     by a DAS handle and DLA descriptor. */

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

/*     DAS */
/*     DSK */
/*     NAIF_IDS */

/* $ Keywords */

/*     DAS */
/*     DSK */
/*     FILES */
/*     TOPOGRAPHY */

/* $ Declarations */

/*     Include file dla.inc */

/*     This include file declares parameters for DLA format */
/*     version zero. */

/*        Version 3.0.1 17-OCT-2016 (NJB) */

/*           Corrected comment: VERIDX is now described as a DAS */
/*           integer address rather than a d.p. address. */

/*        Version 3.0.0 20-JUN-2006 (NJB) */

/*           Changed name of parameter DSCSIZ to DLADSZ. */

/*        Version 2.0.0 09-FEB-2005 (NJB) */

/*           Changed descriptor layout to make backward pointer */
/*           first element.  Updated DLA format version code to 1. */

/*           Added parameters for format version and number of bytes per */
/*           DAS comment record. */

/*        Version 1.0.0 28-JAN-2004 (NJB) */


/*     DAS integer address of DLA version code. */


/*     Linked list parameters */

/*     Logical arrays (aka "segments") in a DAS linked array (DLA) file */
/*     are organized as a doubly linked list.  Each logical array may */
/*     actually consist of character, double precision, and integer */
/*     components.  A component of a given data type occupies a */
/*     contiguous range of DAS addresses of that type.  Any or all */
/*     array components may be empty. */

/*     The segment descriptors in a SPICE DLA (DAS linked array) file */
/*     are connected by a doubly linked list.  Each node of the list is */
/*     represented by a pair of integers acting as forward and backward */
/*     pointers.  Each pointer pair occupies the first two integers of a */
/*     segment descriptor in DAS integer address space.  The DLA file */
/*     contains pointers to the first integers of both the first and */
/*     last segment descriptors. */

/*     At the DLA level of a file format implementation, there is */
/*     no knowledge of the data contents.  Hence segment descriptors */
/*     provide information only about file layout (in contrast with */
/*     the DAF system).  Metadata giving specifics of segment contents */
/*     are stored within the segments themselves in DLA-based file */
/*     formats. */


/*     Parameter declarations follow. */

/*     DAS integer addresses of first and last segment linked list */
/*     pointer pairs.  The contents of these pointers */
/*     are the DAS addresses of the first integers belonging */
/*     to the first and last link pairs, respectively. */

/*     The acronyms "LLB" and "LLE" denote "linked list begin" */
/*     and "linked list end" respectively. */


/*     Null pointer parameter. */


/*     Segment descriptor parameters */

/*     Each segment descriptor occupies a contiguous */
/*     range of DAS integer addresses. */

/*        The segment descriptor layout is: */

/*           +---------------+ */
/*           | BACKWARD PTR  | Linked list backward pointer */
/*           +---------------+ */
/*           | FORWARD PTR   | Linked list forward pointer */
/*           +---------------+ */
/*           | BASE INT ADDR | Base DAS integer address */
/*           +---------------+ */
/*           | INT COMP SIZE | Size of integer segment component */
/*           +---------------+ */
/*           | BASE DP ADDR  | Base DAS d.p. address */
/*           +---------------+ */
/*           | DP COMP SIZE  | Size of d.p. segment component */
/*           +---------------+ */
/*           | BASE CHR ADDR | Base DAS character address */
/*           +---------------+ */
/*           | CHR COMP SIZE | Size of character segment component */
/*           +---------------+ */

/*     Parameters defining offsets for segment descriptor elements */
/*     follow. */


/*     Descriptor size: */


/*     Other DLA parameters: */


/*     DLA format version.  (This number is expected to occur very */
/*     rarely at integer address VERIDX in uninitialized DLA files.) */


/*     Characters per DAS comment record. */


/*     End of include file dla.inc */


/*     Include file dskdsc.inc */

/*     This include file declares parameters for DSK segment descriptors. */

/* -       SPICELIB Version 1.0.0 08-FEB-2017 (NJB) */

/*           Updated version info. */

/*           22-JAN-2016 (NJB) */

/*              Added parameter for data class 2. Changed name of data */
/*              class 1 parameter. Corrected data class descriptions. */

/*           13-MAY-2010 (NJB) */

/*              Descriptor now contains two ID codes, one for the */
/*              surface, one for the associated ephemeris object. This */
/*              supports association of multiple surfaces with one */
/*              ephemeris object without creating file management */
/*              issues. */

/*              Room was added for coordinate system definition */
/*              parameters. */

/*               Flag arrays and model ID/component entries were deleted. */

/*            11-SEP-2008 (NJB) */


/*     DSK segment descriptors are implemented as an array of d.p. */
/*     numbers.  Note that each integer descriptor datum occupies one */
/*     d.p. value. */




/*     Segment descriptor parameters */

/*     Each segment descriptor occupies a contiguous */
/*     range of DAS d.p. addresses. */

/*        The DSK segment descriptor layout is: */

/*           +---------------------+ */
/*           | Surface ID code     | */
/*           +---------------------+ */
/*           | Center ID code      | */
/*           +---------------------+ */
/*           | Data class code     | */
/*           +---------------------+ */
/*           | Data type           | */
/*           +---------------------+ */
/*           | Ref frame code      | */
/*           +---------------------+ */
/*           | Coord sys code      | */
/*           +---------------------+ */
/*           | Coord sys parameters|  {10 elements} */
/*           +---------------------+ */
/*           | Min coord 1         | */
/*           +---------------------+ */
/*           | Max coord 1         | */
/*           +---------------------+ */
/*           | Min coord 2         | */
/*           +---------------------+ */
/*           | Max coord 2         | */
/*           +---------------------+ */
/*           | Min coord 3         | */
/*           +---------------------+ */
/*           | Max coord 3         | */
/*           +---------------------+ */
/*           | Start time          | */
/*           +---------------------+ */
/*           | Stop time           | */
/*           +---------------------+ */

/*     Parameters defining offsets for segment descriptor elements */
/*     follow. */


/*     Surface ID code: */


/*     Central ephemeris object NAIF ID: */


/*     Data class: */

/*     The "data class" is a code indicating the category of */
/*     data contained in the segment. */


/*     Data type: */


/*     Frame ID: */


/*     Coordinate system code: */


/*     Coordinate system parameter start index: */


/*     Number of coordinate system parameters: */


/*     Ranges for coordinate bounds: */


/*     Coverage time bounds: */


/*     Descriptor size (24): */


/*     Data class values: */

/*        Class 1 indicates a surface that can be represented as a */
/*                single-valued function of its domain coordinates. */

/*                An example is a surface defined by a function that */
/*                maps each planetodetic longitude and latitude pair to */
/*                a unique altitude. */


/*        Class 2 indicates a general surface. Surfaces that */
/*                have multiple points for a given pair of domain */
/*                coordinates---for example, multiple radii for a given */
/*                latitude and longitude---belong to class 2. */



/*     Coordinate system values: */

/*        The coordinate system code indicates the system to which the */
/*        tangential coordinate bounds belong. */

/*        Code 1 refers to the planetocentric latitudinal system. */

/*        In this system, the first tangential coordinate is longitude */
/*        and the second tangential coordinate is latitude. The third */
/*        coordinate is radius. */



/*        Code 2 refers to the cylindrical system. */

/*        In this system, the first tangential coordinate is radius and */
/*        the second tangential coordinate is longitude. The third, */
/*        orthogonal coordinate is Z. */



/*        Code 3 refers to the rectangular system. */

/*        In this system, the first tangential coordinate is X and */
/*        the second tangential coordinate is Y. The third, */
/*        orthogonal coordinate is Z. */



/*        Code 4 refers to the planetodetic/geodetic system. */

/*        In this system, the first tangential coordinate is longitude */
/*        and the second tangential coordinate is planetodetic */
/*        latitude. The third, orthogonal coordinate is altitude. */



/*     End of include file dskdsc.inc */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Handle of a DSK file. */
/*     DLADSC     I   DLA segment descriptor. */
/*     DSKDSC     O   DSK segment descriptor. */

/* $ Detailed_Input */

/*     HANDLE   is the handle of a DSK file that is open for */
/*              read access. */

/*     DLADSC   is the DLA segment descriptor corresponding to */
/*              a DSK segment. */

/* $ Detailed_Output */

/*     DSKDSC   is the DSK segment descriptor of the segment */
/*              designated by the input handle and DLA descriptor. */

/* $ Parameters */

/*     See the include file */

/*        dla.inc */

/*     for declarations of DLA descriptor sizes and documentation of the */
/*     contents of DLA descriptors. */

/*     See the include file */

/*        dskdsc.inc */

/*     for declarations of DSK descriptor sizes and documentation of the */
/*     contents of DSK descriptors. */

/* $ Exceptions */

/*     1)  If the size of the double precision component of the */
/*         segment is smaller than that of a DSK descriptor, the */
/*         error SPICE(INVALIDFORMAT) is signaled. */

/*     2)  If the input handle is invalid, an error is signaled by a */
/*         routine in the call tree of this routine. */

/*     3)  If the input DLA descriptor is invalid, the effect of this */
/*         routine is undefined. The error *may* be diagnosed by */
/*         routines in the call tree of this routine, but there are no */
/*         guarantees. */

/*     4)  If any DAS read error is detected, the error is signaled by a */
/*         routine in the call tree of this routine. */

/* $ Files */

/*     See input argument HANDLE. */

/* $ Particulars */

/*     This is a convenience routine intended for use by low-level */
/*     routines that read DSK segments. This routine may also be called */
/*     by user applications that must access DSK files at the segment */
/*     level. */

/* $ Examples */

/*     The numerical results shown for these examples may differ across */
/*     platforms. The results depend on the SPICE kernels used as input, */
/*     the compiler and supporting libraries, and the machine specific */
/*     arithmetic implementation. */

/*     1) Dump the DSK descriptors of a DSK file. */


/*        Example code begins here. */


/*              PROGRAM DSKGD_EX1 */
/*              IMPLICIT NONE */

/*              INCLUDE 'dla.inc' */
/*              INCLUDE 'dskdsc.inc' */
/*              INCLUDE 'dsk02.inc' */

/*              INTEGER               FILSIZ */
/*              PARAMETER           ( FILSIZ = 255 ) */

/*              CHARACTER*(FILSIZ)    DSK */

/*              DOUBLE PRECISION      DSKDSC ( DSKDSZ ) */

/*              INTEGER               DLADSC ( DLADSZ ) */
/*              INTEGER               HANDLE */
/*              INTEGER               I */
/*              INTEGER               NXTDSC ( DLADSZ ) */

/*              LOGICAL               FOUND */


/*              CALL PROMPT ( 'Enter DSK name > ', DSK ) */
/*        C */
/*        C     Open the DSK file and begin a forward search */
/*        C     for segments. */
/*        C */
/*              CALL DASOPR ( DSK, HANDLE ) */

/*              CALL DLABFS ( HANDLE, NXTDSC, FOUND ) */

/*              DO WHILE ( FOUND ) */
/*        C */
/*        C        Make the DLA descriptor we just fetched */
/*        C        the current one. */
/*        C */
/*                 CALL MOVEI ( NXTDSC, DLADSZ, DLADSC ) */

/*                 CALL DSKGD ( HANDLE, DLADSC, DSKDSC ) */

/*                 WRITE (*,*) 'DSK descriptor contents: ' */

/*                 DO I = 1, DSKDSZ */
/*                    WRITE (*,*) DSKDSC(I) */
/*                 END DO */
/*        C */
/*        C        Find the next segment, if it exists. */
/*        C */
/*                 CALL DLAFNS ( HANDLE, DLADSC, NXTDSC, FOUND ) */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, using the DSK file named phobos512.bds, the output */
/*        was: */


/*        Enter DSK name > phobos512.bds */
/*         DSK descriptor contents: */
/*           401.00000000000000 */
/*           401.00000000000000 */
/*           1.0000000000000000 */
/*           2.0000000000000000 */
/*           10021.000000000000 */
/*           1.0000000000000000 */
/*           0.0000000000000000 */
/*           0.0000000000000000 */
/*           0.0000000000000000 */
/*           0.0000000000000000 */
/*           0.0000000000000000 */
/*           0.0000000000000000 */
/*           0.0000000000000000 */
/*           0.0000000000000000 */
/*           0.0000000000000000 */
/*           0.0000000000000000 */
/*          -3.1415926535897931 */
/*           3.1415926535897931 */
/*          -1.5707963267948966 */
/*           1.5707963267948966 */
/*           8.0496322487215526 */
/*           13.940939832123945 */
/*          -1577879958.8160586 */
/*           1577880066.1839132 */


/*     2) Again, dump the DSK descriptors of a DSK file, this time */
/*        interpreting the descriptor information and displaying */
/*        it in a user-friendly form. This display is a simplified */
/*        version of that created by the utility DSKBRIEF. */

/*        This program requests the name of an optional meta-kernel. */
/*        The meta-kernel can be used to define surface name-ID */
/*        associations. If no meta-kernel is needed, the user can */
/*        enter a carriage return at the prompt for this file. */


/*        Example code begins here. */


/*              PROGRAM DSKGD_EX2 */
/*              IMPLICIT NONE */

/*              INCLUDE 'dla.inc' */
/*              INCLUDE 'dskdsc.inc' */
/*              INCLUDE 'dsk02.inc' */
/*              INCLUDE 'srftrn.inc' */

/*        C */
/*        C     SPICELIB functions */
/*        C */
/*              DOUBLE PRECISION      DPR */

/*        C */
/*        C     Local parameters */
/*        C */
/*              CHARACTER*(*)         FMT1 */
/*              PARAMETER           ( FMT1 = '(A,2(F19.12))' ) */

/*              CHARACTER*(*)         FMT2 */
/*              PARAMETER           ( FMT2 = '(A,I3)' ) */

/*              INTEGER               BDNMLN */
/*              PARAMETER           ( BDNMLN = 36 ) */

/*              INTEGER               FILSIZ */
/*              PARAMETER           ( FILSIZ = 255 ) */

/*              INTEGER               FRNMLN */
/*              PARAMETER           ( FRNMLN = 32 ) */

/*              INTEGER               NAMLEN */
/*              PARAMETER           ( NAMLEN = 30 ) */

/*              INTEGER               TIMLEN */
/*              PARAMETER           ( TIMLEN = 40 ) */

/*              INTEGER               NSYS */
/*              PARAMETER           ( NSYS   = 4 ) */

/*              INTEGER               NCLASS */
/*              PARAMETER           ( NCLASS = 2 ) */

/*              INTEGER               CLNMLN */
/*              PARAMETER           ( CLNMLN = 25 ) */

/*        C */
/*        C     Local variables */
/*        C */
/*              CHARACTER*(BDNMLN)    BODNAM */
/*              CHARACTER*(TIMLEN)    BTIME */
/*              CHARACTER*(CLNMLN)    CLSNMS ( NCLASS ) */
/*              CHARACTER*(FILSIZ)    DSK */
/*              CHARACTER*(TIMLEN)    ETIME */
/*              CHARACTER*(FRNMLN)    FRAME */
/*              CHARACTER*(FILSIZ)    META */
/*              CHARACTER*(SFNMLN)    SRFNAM */
/*              CHARACTER*(NAMLEN)    SYSNAM */
/*              CHARACTER*(NAMLEN)    SYSNMS ( NSYS ) */

/*              DOUBLE PRECISION      DSKDSC ( DSKDSZ ) */
/*              DOUBLE PRECISION      F */
/*              DOUBLE PRECISION      RE */
/*              DOUBLE PRECISION      RP */

/*              INTEGER               BODYID */
/*              INTEGER               CORSYS */
/*              INTEGER               DCLASS */
/*              INTEGER               DLADSC ( DLADSZ ) */
/*              INTEGER               DTYPE */
/*              INTEGER               FRAMID */
/*              INTEGER               HANDLE */
/*              INTEGER               NXTDSC ( DLADSZ ) */
/*              INTEGER               SEGNO */
/*              INTEGER               SURFID */

/*              LOGICAL               FOUND */
/*              LOGICAL               ISNAME */

/*        C */
/*        C     Initial values */
/*        C */
/*              DATA                  CLSNMS / 'Single-valued surface', */
/*             .                               'General surface'       / */

/*              DATA                  SYSNMS / 'Latitudinal', */
/*             .                               'Cylindrical', */
/*             .                               'Rectangular', */
/*             .                               'Planetodetic' / */


/*              CALL PROMPT ( 'Enter DSK name         > ', DSK  ) */
/*              CALL PROMPT ( 'Enter meta-kernel name > ', META ) */

/*              IF ( META .NE. ' ' ) THEN */
/*                 CALL FURNSH ( META ) */
/*              END IF */

/*        C */
/*        C     Open the DLA file and begin a forward search */
/*        C     for segments. */
/*        C */
/*              CALL DASOPR ( DSK, HANDLE ) */

/*              SEGNO = 0 */

/*              CALL DLABFS ( HANDLE, NXTDSC, FOUND ) */

/*              DO WHILE ( FOUND ) */

/*                 SEGNO = SEGNO + 1 */
/*        C */
/*        C        Make the DLA descriptor we just fetched */
/*        C        the current one. */
/*        C */
/*                 CALL MOVEI ( NXTDSC, DLADSZ, DLADSC ) */

/*                 CALL DSKGD ( HANDLE, DLADSC, DSKDSC ) */

/*                 BODYID = NINT( DSKDSC(CTRIDX) ) */
/*                 SURFID = NINT( DSKDSC(SRFIDX) ) */
/*                 FRAMID = NINT( DSKDSC(FRMIDX) ) */
/*                 DTYPE  = NINT( DSKDSC(TYPIDX) ) */
/*                 DCLASS = NINT( DSKDSC(CLSIDX) ) */

/*                 CALL BODC2S ( BODYID, BODNAM ) */
/*                 CALL SRFC2S ( SURFID, BODYID, SRFNAM, ISNAME ) */
/*                 CALL FRMNAM ( FRAMID, FRAME  ) */

/*                 IF ( FRAME .EQ. ' ' ) THEN */
/*                    CALL INTSTR ( FRAMID, FRAME ) */
/*                 END IF */

/*                 CALL ETCAL ( DSKDSC(BTMIDX), BTIME ) */
/*                 CALL ETCAL ( DSKDSC(ETMIDX), ETIME ) */

/*                 CORSYS = NINT( DSKDSC(SYSIDX) ) */

/*                 SYSNAM = SYSNMS( CORSYS ) */

/*                 WRITE (*,*)    '====================================' */
/*                 WRITE (*,FMT2) ' DSK descriptor for segment ', */
/*             .                  SEGNO */
/*                 WRITE (*,*)    '  Body:              ', BODNAM */
/*                 WRITE (*,*)    '  Surface:           ', SRFNAM */
/*                 WRITE (*,*)    '  Frame:             ', FRAME */
/*                 WRITE (*,*)    '  Start time (TDB):  ', BTIME */
/*                 WRITE (*,*)    '  Stop time  (TDB):  ', ETIME */
/*                 WRITE (*,*)    '  Data type:         ', DTYPE */
/*                 WRITE (*,*)    '  Data class:        ', DCLASS, ' ', */
/*             .                                        CLSNMS(DCLASS) */
/*                 WRITE (*,*)    '  Coordinate system: ', SYSNAM */

/*                 IF ( CORSYS .EQ. PDTSYS ) THEN */

/*                    RE = DSKDSC(PARIDX  ) */
/*                    F  = DSKDSC(PARIDX+1) */
/*                    RP = RE * ( 1.D0 - F ) */

/*                    WRITE (*,*) '     Equatorial radius (km): ', RE */
/*                    WRITE (*,*) '     Polar radius      (km): ', RP */

/*                 END IF */

/*                 WRITE (*,*) '  Segment boundaries:' */

/*                 IF ( CORSYS .EQ. LATSYS ) THEN */

/*                    WRITE (*,FMT1) '    Longitude (deg):   ', */
/*             .                  DPR() * DSKDSC(MN1IDX), */
/*             .                  DPR() * DSKDSC(MX1IDX) */
/*                    WRITE (*,FMT1) '    Latitude  (deg):   ', */
/*             .                  DPR() * DSKDSC(MN2IDX), */
/*             .                  DPR() * DSKDSC(MX2IDX) */
/*                    WRITE (*,FMT1) '    Radius     (km):   ', */
/*             .                          DSKDSC(MN3IDX), */
/*             .                          DSKDSC(MX3IDX) */

/*                 ELSE IF ( CORSYS .EQ. CYLSYS ) THEN */

/*                    CALL SETMSG ( 'Coordinate system was ' */
/*             .      //            'Cylindrical'           ) */
/*                    CALL SIGERR ( 'SPICE(NOTSUPPORTED)'   ) */


/*                 ELSE IF ( CORSYS .EQ. RECSYS ) THEN */

/*                    WRITE (*,FMT1) '    X-coordinate (km): ', */
/*             .                          DSKDSC(MN1IDX), */
/*             .                          DSKDSC(MX1IDX) */
/*                    WRITE (*,FMT1) '    Y-coordinate (km): ', */
/*             .                          DSKDSC(MN2IDX), */
/*             .                          DSKDSC(MX2IDX) */
/*                    WRITE (*,FMT1) '    Z-coordinate (km): ', */
/*             .                          DSKDSC(MN3IDX), */
/*             .                          DSKDSC(MX3IDX) */

/*                 ELSE IF ( CORSYS .EQ. PDTSYS ) THEN */

/*                    WRITE (*,FMT1) '    Longitude (deg):   ', */
/*             .                  DPR() * DSKDSC(MN1IDX), */
/*             .                  DPR() * DSKDSC(MX1IDX) */
/*                    WRITE (*,FMT1) '    Latitude  (deg):   ', */
/*             .                  DPR() * DSKDSC(MN2IDX), */
/*             .                  DPR() * DSKDSC(MX2IDX) */
/*                    WRITE (*,FMT1) '    Altitude   (km):   ', */
/*             .                          DSKDSC(MN3IDX), */
/*             .                          DSKDSC(MX3IDX) */
/*                 END IF */
/*        C */
/*        C        Find the next segment, if it exists. */
/*        C */
/*                 CALL DLAFNS ( HANDLE, DLADSC, NXTDSC, FOUND ) */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, using the DSK file named phobos512.bds and an empty */
/*        string instead of the meta-kernel name, the output was: */


/*        Enter DSK name         > phobos512.bds */
/*        Enter meta-kernel name > */
/*         ==================================== */
/*         DSK descriptor for segment   1 */
/*           Body:              PHOBOS */
/*           Surface:           401 */
/*           Frame:             IAU_PHOBOS */
/*           Start time (TDB):  1950 JAN 01 00:00:41.183 */
/*           Stop time  (TDB):  2050 JAN 01 00:01:06.183 */
/*           Data type:                    2 */
/*           Data class:                   1  Single-valued surface */
/*           Coordinate system: Latitudinal */
/*           Segment boundaries: */
/*            Longitude (deg):     -180.000000000000   180.000000000000 */
/*            Latitude  (deg):      -90.000000000000    90.000000000000 */
/*            Radius     (km):        8.049632248722    13.940939832124 */


/*     3) Again, dump the DSK descriptors of a DSK file, using the */
/*        program from example 2, but this time reading the DSK file */

/*           phobos_3_3_3seg.bds */

/*        which can be created by running an example program from */
/*        DSKW02. Use the meta-kernel shown below to demonstrate surface */
/*        name-ID mapping. */


/*           KPL/MK */

/*           File: dskgd_ex3.tm */

/*           This meta-kernel is intended to support operation of SPICE */
/*           example programs. The file contents shown here should not be */
/*           assumed to contain adequate or correct versions of data */
/*           required by SPICE-based user applications. */


/*           \begindata */

/*           NAIF_SURFACE_NAME += ( 'Phobos example surface 1', */
/*                                  'Phobos example surface 2', */
/*                                  'Phobos example surface 3' ) */
/*           NAIF_SURFACE_CODE += (   1,   2,   3 ) */
/*           NAIF_SURFACE_BODY += ( 401, 401, 401 ) */

/*           \begintext */

/*           End of meta-kernel */


/*        When Example #2 was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, using the DSK file named phobos_3_3_3seg.bds and the */
/*        meta-kernel dskgd_ex3.tm, the output was: */


/*        Enter DSK name         > phobos_3_3_3seg.bds */
/*        Enter meta-kernel name > dskgd_ex3.tm */
/*         ==================================== */
/*         DSK descriptor for segment   1 */
/*           Body:              PHOBOS */
/*           Surface:           Phobos example surface 1 */
/*           Frame:             IAU_PHOBOS */
/*           Start time (TDB):  1950 JAN 01 00:00:00.000 */
/*           Stop time  (TDB):  2050 JAN 01 00:00:00.000 */
/*           Data type:                    2 */
/*           Data class:                   2  General surface */
/*           Coordinate system: Latitudinal */
/*           Segment boundaries: */
/*            Longitude (deg):     -180.000000000000   180.000000000000 */
/*            Latitude  (deg):      -90.000000000000    90.000000000000 */
/*            Radius     (km):        8.225298075974    14.011768145626 */
/*         ==================================== */
/*         DSK descriptor for segment   2 */
/*           Body:              PHOBOS */
/*           Surface:           Phobos example surface 2 */
/*           Frame:             IAU_PHOBOS */
/*           Start time (TDB):  1950 JAN 01 00:00:00.000 */
/*           Stop time  (TDB):  2050 JAN 01 00:00:00.000 */
/*           Data type:                    2 */
/*           Data class:                   2  General surface */
/*           Coordinate system: Rectangular */
/*           Segment boundaries: */
/*            X-coordinate (km):     -1.300000000000     1.310000000000 */
/*            Y-coordinate (km):     -1.210000000000     1.200000000000 */
/*            Z-coordinate (km):     -9.452932357788     9.638179779053 */
/*         ==================================== */
/*         DSK descriptor for segment   3 */
/*           Body:              PHOBOS */
/*           Surface:           Phobos example surface 3 */
/*           Frame:             IAU_PHOBOS */
/*           Start time (TDB):  1950 JAN 01 00:00:00.000 */
/*           Stop time  (TDB):  2050 JAN 01 00:00:00.000 */
/*           Data type:                    2 */
/*           Data class:                   2  General surface */
/*           Coordinate system: Planetodetic */
/*              Equatorial radius (km):    13.000000000000000 */
/*              Polar radius      (km):    9.0999999999999996 */
/*           Segment boundaries: */
/*            Longitude (deg):     -180.000000000000   180.000000000000 */
/*            Latitude  (deg):      -90.000000000000    90.000000000000 */
/*            Altitude   (km):       -3.728668683604     1.372015791081 */


/* $ Restrictions */

/*     1)  See Exception #3. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 09-JUL-2020 (JDR) (BVS) */

/*        Edited the header to comply with NAIF standard. Extended the */
/*        $Exceptions section and updated the $Restrictions. */

/* -    SPICELIB Version 1.0.0, 08-FEB-2017 (NJB) */

/*        Updated version info. */

/*        22-JAN-2016 (NJB) */

/*           Added new header example programs and updated existing */
/*           example program. Made minor changes to code to enhance */
/*           readability. Corrected header typo. */

/*        09-OCT-2009 (NJB) */

/* -& */
/* $ Index_Entries */

/*     return DSK segment descriptor */

/* -& */

/*     SPICELIB functions */


/*     Local variables */

    if (return_()) {
	return 0;
    }
    chkin_("DSKGD", (ftnlen)5);

/*     Fetch the base address and size of the DP component of the */
/*     indicated segment. */

    dpbase = dladsc[4];
    dpsize = dladsc[5];

/*     If we don't have enough d.p. elements to hold a descriptor, */
/*     something's wrong. */

    if (dpsize < 24) {
	setmsg_("Size of d.p. component of segment is #; cannot extract desc"
		"riptor.  This is a file format error which may be indicative"
		" of a corrupted file.", (ftnlen)140);
	errint_("#", &dpsize, (ftnlen)1);
	sigerr_("SPICE(INVALIDFORMAT)", (ftnlen)20);
	chkout_("DSKGD", (ftnlen)5);
	return 0;
    }

/*     Extract the descriptor. */

    i__1 = dpbase + 1;
    i__2 = dpbase + 24;
    dasrdd_(handle, &i__1, &i__2, dskdsc);
    chkout_("DSKGD", (ftnlen)5);
    return 0;
} /* dskgd_ */

