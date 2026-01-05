/* dafana.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__5000 = 5000;
static integer c__20 = 20;
static integer c__1 = 1;
static integer c__128 = 128;

/* $Procedure DAFANA ( DAF, add new array ) */
/* Subroutine */ int dafana_0_(int n__, integer *handle, doublereal *sum, 
	char *name__, doublereal *data, integer *n, ftnlen name_len)
{
    /* Initialized data */

    static logical first = TRUE_;
    static integer sthead = -1;
    static integer stfptr = -1;

    /* System generated locals */
    integer i__1, i__2, i__3, i__4, i__5;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static integer cloc, dloc, free, stfh[20], word, prev, next, i__, p;
    extern logical elemi_(integer *, integer *);
    extern /* Subroutine */ int chkin_(char *, ftnlen), dafps_(integer *, 
	    integer *, doublereal *, integer *, doublereal *);
    static integer bward;
    extern /* Subroutine */ int dafus_(doublereal *, integer *, integer *, 
	    doublereal *, integer *);
    static integer fward;
    extern /* Subroutine */ int errch_(char *, char *, ftnlen, ftnlen), 
	    moved_(doublereal *, integer *, doublereal *);
    static logical found;
    static integer nextp;
    static doublereal dc[124];
    static integer ic[250], nd;
    extern logical failed_(void);
    static char dafnam[255];
    static integer ni;
    extern /* Subroutine */ int dafhof_(integer *), dafhfn_(integer *, char *,
	     ftnlen), dafwda_(integer *, integer *, integer *, doublereal *), 
	    dafhsf_(integer *, integer *, integer *), dafsih_(integer *, char 
	    *, ftnlen);
    static char ifname[60];
    extern /* Subroutine */ int cleard_(integer *, doublereal *), dafrcr_(
	    integer *, integer *, char *, ftnlen), dafrdr_(integer *, integer 
	    *, integer *, integer *, doublereal *, logical *), dafrfr_(
	    integer *, integer *, integer *, char *, integer *, integer *, 
	    integer *, ftnlen);
    static char namrec[1000];
    static logical staddg[20];
    extern /* Subroutine */ int dafwdr_(integer *, integer *, doublereal *), 
	    dafwcr_(integer *, integer *, char *, ftnlen), dafarw_(integer *, 
	    integer *, integer *), dafrwa_(integer *, integer *, integer *), 
	    errhan_(char *, integer *, ftnlen);
    static integer stbegn[20];
    extern /* Subroutine */ int dafwfr_(integer *, integer *, integer *, char 
	    *, integer *, integer *, integer *, ftnlen);
    static integer stfree[20];
    static char stname[1000*20];
    extern /* Subroutine */ int sigerr_(char *, ftnlen);
    static integer narray;
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    static doublereal sumrec[128];
    static char stifnm[60*20];
    static integer namsiz, opnset[5006];
    extern /* Subroutine */ int ssizei_(integer *, integer *);
    static integer stlast[20];
    extern /* Subroutine */ int setmsg_(char *, ftnlen);
    extern logical return_(void);
    static integer stpool[20];
    extern /* Subroutine */ int errint_(char *, integer *, ftnlen);
    static integer stfrst[20];
    static doublereal stlsum[2500]	/* was [125][20] */;
    static integer sumsiz;

/* $ Abstract */

/*     Add a new array to an existing DAF. */

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

/*     DAF */

/* $ Keywords */

/*     FILES */

/* $ Declarations */

/* $ Abstract */

/*     Parameter declarations for the DAF/DAS handle manager. */

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

/*     DAF, DAS */

/* $ Keywords */

/*     PRIVATE */

/* $ Particulars */

/*     This include file contains parameters defining limits and */
/*     integer codes that are utilized in the DAF/DAS handle manager */
/*     routines. */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     F.S. Turner       (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 2.6.0, 28-NOV-2021 (BVS) */

/*        Updated for MAC-OSX-M1-64BIT-CLANG_C. */

/* -    SPICELIB Version 2.5.0, 10-MAR-2014 (BVS) */

/*        Updated for SUN-SOLARIS-64BIT-INTEL. */

/* -    SPICELIB Version 2.4.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-LINUX-64BIT-IFORT. */

/* -    SPICELIB Version 2.3.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-CYGWIN-GFORTRAN. */

/* -    SPICELIB Version 2.2.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-CYGWIN-64BIT-GFORTRAN. */

/* -    SPICELIB Version 2.1.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-CYGWIN-64BIT-GCC_C. */

/* -    SPICELIB Version 2.0.0, 12-APR-2012 (BVS) */

/*        Increased FTSIZE (from 1000 to 5000). */

/* -    SPICELIB Version 1.20.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-INTEL. */

/* -    SPICELIB Version 1.19.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-INTEL-CC_C. */

/* -    SPICELIB Version 1.18.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C. */

/* -    SPICELIB Version 1.17.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-64BIT-NATIVE_C. */

/* -    SPICELIB Version 1.16.0, 13-MAY-2010 (BVS) */

/*        Updated for PC-WINDOWS-64BIT-IFORT. */

/* -    SPICELIB Version 1.15.0, 13-MAY-2010 (BVS) */

/*        Updated for PC-LINUX-64BIT-GFORTRAN. */

/* -    SPICELIB Version 1.14.0, 13-MAY-2010 (BVS) */

/*        Updated for PC-64BIT-MS_C. */

/* -    SPICELIB Version 1.13.0, 13-MAY-2010 (BVS) */

/*        Updated for MAC-OSX-64BIT-INTEL_C. */

/* -    SPICELIB Version 1.12.0, 13-MAY-2010 (BVS) */

/*        Updated for MAC-OSX-64BIT-IFORT. */

/* -    SPICELIB Version 1.11.0, 13-MAY-2010 (BVS) */

/*        Updated for MAC-OSX-64BIT-GFORTRAN. */

/* -    SPICELIB Version 1.10.0, 18-MAR-2009 (BVS) */

/*        Updated for PC-LINUX-GFORTRAN. */

/* -    SPICELIB Version 1.9.0, 18-MAR-2009 (BVS) */

/*        Updated for MAC-OSX-GFORTRAN. */

/* -    SPICELIB Version 1.8.0, 19-FEB-2008 (BVS) */

/*        Updated for PC-LINUX-IFORT. */

/* -    SPICELIB Version 1.7.0, 14-NOV-2006 (BVS) */

/*        Updated for PC-LINUX-64BIT-GCC_C. */

/* -    SPICELIB Version 1.6.0, 14-NOV-2006 (BVS) */

/*        Updated for MAC-OSX-INTEL_C. */

/* -    SPICELIB Version 1.5.0, 14-NOV-2006 (BVS) */

/*        Updated for MAC-OSX-IFORT. */

/* -    SPICELIB Version 1.4.0, 14-NOV-2006 (BVS) */

/*        Updated for PC-WINDOWS-IFORT. */

/* -    SPICELIB Version 1.3.0, 26-OCT-2005 (BVS) */

/*        Updated for SUN-SOLARIS-64BIT-GCC_C. */

/* -    SPICELIB Version 1.2.0, 03-JAN-2005 (BVS) */

/*        Updated for PC-CYGWIN_C. */

/* -    SPICELIB Version 1.1.0, 03-JAN-2005 (BVS) */

/*        Updated for PC-CYGWIN. */

/* -    SPICELIB Version 1.0.1, 17-JUL-2002 */

/*        Added MAC-OSX environments. */

/* -    SPICELIB Version 1.0.0, 07-NOV-2001 */

/* -& */

/*     Unit and file table size parameters. */

/*     FTSIZE     is the maximum number of files (DAS and DAF) that a */
/*                user may have open simultaneously. */


/*     RSVUNT     is the number of units protected from being locked */
/*                to a particular handle by ZZDDHHLU. */


/*     SCRUNT     is the number of units protected for use by scratch */
/*                files. */


/*     UTSIZE     is the maximum number of logical units this manager */
/*                will utilize at one time. */


/*     Access method enumeration.  These parameters are used to */
/*     identify which access method is associated with a particular */
/*     handle.  They need to be synchronized with the STRAMH array */
/*     defined in ZZDDHGSD in the following fashion: */

/*        STRAMH ( READ   ) = 'READ' */
/*        STRAMH ( WRITE  ) = 'WRITE' */
/*        STRAMH ( SCRTCH ) = 'SCRATCH' */
/*        STRAMH ( NEW    ) = 'NEW' */

/*     These values are used in the file table variable FTAMH. */


/*     Binary file format enumeration.  These parameters are used to */
/*     identify which binary file format is associated with a */
/*     particular handle.  They need to be synchronized with the STRBFF */
/*     array defined in ZZDDHGSD in the following fashion: */

/*        STRBFF ( BIGI3E ) = 'BIG-IEEE' */
/*        STRBFF ( LTLI3E ) = 'LTL-IEEE' */
/*        STRBFF ( VAXGFL ) = 'VAX-GFLT' */
/*        STRBFF ( VAXDFL ) = 'VAX-DFLT' */

/*     These values are used in the file table variable FTBFF. */


/*     Some random string lengths... more documentation required. */
/*     For now this will have to suffice. */


/*     Architecture enumeration.  These parameters are used to identify */
/*     which file architecture is associated with a particular handle. */
/*     They need to be synchronized with the STRARC array defined in */
/*     ZZDDHGSD in the following fashion: */

/*        STRARC ( DAF ) = 'DAF' */
/*        STRARC ( DAS ) = 'DAS' */

/*     These values will be used in the file table variable FTARC. */


/*     For the following environments, record length is measured in */
/*     characters (bytes) with eight characters per double precision */
/*     number. */

/*     Environment: Sun, Sun FORTRAN */
/*     Source:      Sun Fortran Programmer's Guide */

/*     Environment: PC, MS FORTRAN */
/*     Source:      Microsoft Fortran Optimizing Compiler User's Guide */

/*     Environment: Macintosh, Language Systems FORTRAN */
/*     Source:      Language Systems FORTRAN Reference Manual, */
/*                  Version 1.2, page 12-7 */

/*     Environment: PC/Linux, g77 */
/*     Source:      Determined by experiment. */

/*     Environment: PC, Lahey F77 EM/32 Version 4.0 */
/*     Source:      Lahey F77 EM/32 Language Reference Manual, */
/*                  page 144 */

/*     Environment: HP-UX 9000/750, FORTRAN/9000 Series 700 computers */
/*     Source:      FORTRAN/9000 Reference-Series 700 Computers, */
/*                  page 5-110 */

/*     Environment: NeXT Mach OS (Black Hardware), */
/*                  Absoft Fortran Version 3.2 */
/*     Source:      NAIF Program */


/*     The following parameter defines the size of a string used */
/*     to store a filenames on this target platform. */


/*     The following parameter controls the size of the character record */
/*     buffer used to read data from non-native files. */

/* $ Brief_I/O */

/*     VARIABLE  I/O  ENTRY POINTS */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   DAFBNA, DAFCAD */
/*     SUM        I   DAFBNA */
/*     NAME       I   DAFBNA */
/*     DATA       I   DAFADA */
/*     N          I   DAFADA */
/*     TBSIZE     P   DAFANA */

/* $ Detailed_Input */

/*     HANDLE   is the handle of a DAF opened for write access */
/*              by a previous call to DAFOPW or DAFOPN. */

/*     SUM      is the summary for the array being added. */

/*     NAME     is the name of the array being added. */

/*     DATA     contains all or part of the data in the array. */

/*     N        is the number of elements in DATA. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     TBSIZE   is the size of the file table maintained internally */
/*              by DAFANA,  TBSIZE is the maximum number of DAFs */
/*              that can be in use simultaneously by this routine. */

/* $ Exceptions */

/*     1)  If DAFANA is called directly, the error SPICE(BOGUSENTRY) */
/*         is signaled. */

/*     2)  See entry points DAFBNA, DAFADA, DAFENA, and DAFCAD */
/*         for exceptions specific to those entry points. */

/* $ Files */

/*     See argument HANDLE, above. */

/* $ Particulars */

/*     DAFANA serves as an umbrella, allowing data to be shared by its */
/*     entry points: */

/*        DAFBNA         Begin new array. */
/*        DAFADA         Add data to array. */
/*        DAFCAD         Continue adding data. */
/*        DAFENA         End new array. */

/*     The main function of these entry points is to simplify the */
/*     addition of new arrays to existing DAFs. */

/*     An application can add data to a single DAF, or to multiple DAFs, */
/*     simultaneously. In the case of writing to a single DAF, the */
/*     creation of a new array requires four steps: */

/*        1) Open a DAF for write access, using either DAFOPW */
/*           (if the file already exists) or DAFOPN (if it does not). */

/*              CALL DAFOPW ( FNAME, HANDLE ) */

/*        2) Begin the new DAF by calling DAFBNA, */

/*              CALL DAFBNA ( HANDLE, SUM, NAME ) */

/*        3) Add data to the array by calling DAFADA as many times */
/*           as necessary, */

/*              CALL GET_DATA ( DATA, N, FOUND ) */

/*              DO WHILE ( FOUND ) */
/*                 CALL DAFADA   ( DATA, N        ) */
/*                 CALL GET_DATA ( DATA, N, FOUND ) */
/*              END DO */

/*        4) End the array by calling DAFENA, */

/*              CALL DAFENA */

/*     Note that the data can be added in chunks of any size, so long */
/*     as the chunks are ordered correctly. */

/*     In applications that add data to multiple DAFs simultaneously, it */
/*     is necessary to specify which DAF to add data to. The DAFANA */
/*     entry points that allow specification of a DAF via a file handle */
/*     argument are DAFBNA (DAF, begin new array) and DAFCAD (DAF, */
/*     continue adding data).  As in the single-DAF case, arrays are */
/*     started by calls to DAFBNA, and data is added to arrays by calls */
/*     to DAFADA. The last DAF designated by the input file handle */
/*     supplied to DAFBNA or DAFCAD is the `current DAF'. If a */
/*     DAF contains an array started by a call to DAFBNA but not yet */
/*     completed by a call to DAFENA, we call this array the `current */
/*     array' for that DAF. Each call to DAFADA will add data to the */
/*     current array in the current DAF. A call to DAFENA will make the */
/*     current array in the current DAF a permanent addition to that DAF. */

/*     The notion of `current DAF' as discussed here applies only to */
/*     DAFs acted upon by entry points of DAFANA. In DAFFA, there is a */
/*     DAF that is treated as the `current DAF' for searching; there is */
/*     no connection between the DAFs regarded as current by DAFANA and */
/*     DAFFA. */

/*     In the following example, we write data obtained from the routine */
/*     GET_DATA into two separate DAFs. The first N/2 elements of the */
/*     array DATA will be written to the first DAF; the rest of the */
/*     array will be written to the second DAF. */


/*        1) Open the DAFs for write access, using either DAFOPW */
/*           (if the files already exist) or DAFOPN (if they do not). */

/*              CALL DAFOPW ( FNAME1, HANDL1 ) */
/*              CALL DAFOPW ( FNAME2, HANDL2 ) */

/*        2) Begin the new DAFs by calling DAFBNA, */

/*              CALL DAFBNA ( HANDL1, SUM1, NAME1 ) */
/*              CALL DAFBNA ( HANDL2, SUM2, NAME2 ) */

/*        3) Add data to the arrays by calling DAFCAD and DAFADA as many */
/*           times as necessary, selecting the file to add data to by */
/*           calling DAFCAD: */

/*              CALL GET_DATA ( DATA, N, FOUND ) */

/*              DO WHILE ( FOUND ) */

/*                 CALL DAFCAD   ( HANDL1                          ) */
/*                 CALL DAFADA   ( DATA,               N/2         ) */

/*                 CALL DAFCAD   ( HANDL2                          ) */
/*                 CALL DAFADA   ( DATA( N/2 + 1 ),    N - N/2     ) */

/*                 CALL GET_DATA ( DATA, N, FOUND ) */

/*              END DO */

/*        4) End each array by calling DAFENA, selecting the file */
/*           in which to end the array by calling DAFCAD: */

/*              CALL DAFCAD ( HANDL1 ) */
/*              CALL DAFENA */

/*              CALL DAFCAD ( HANDL2 ) */
/*              CALL DAFENA */

/* $ Examples */

/*     The numerical results shown for these examples may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) The following example illustrates one possible way to copy */
/*        an array from one DAF to another, N words at a time. */

/*        Use the CK kernel below as the original DAF file. */

/*           vo2_swu_ck2.bc */


/*        Example code begins here. */


/*              PROGRAM DAFANA_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local parameters */
/*        C */
/*              INTEGER               MAXNSZ */
/*              PARAMETER           ( MAXNSZ = 1000 ) */

/*              INTEGER               MAXND */
/*              PARAMETER           ( MAXND  = 124  ) */

/*              INTEGER               MAXNI */
/*              PARAMETER           ( MAXNI  = 250  ) */

/*              INTEGER               NWORDS */
/*              PARAMETER           ( NWORDS = 100  ) */

/*              INTEGER               MAXSUM */
/*              PARAMETER           ( MAXSUM = 125  ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              CHARACTER*(MAXNSZ)    NAME */

/*              DOUBLE PRECISION      DATA   ( NWORDS ) */
/*              DOUBLE PRECISION      DC     ( MAXND  ) */
/*              DOUBLE PRECISION      SUM    ( MAXSUM ) */

/*              INTEGER               BIDX */
/*              INTEGER               CHUNK */
/*              INTEGER               EIDX */
/*              INTEGER               IC     ( MAXNI  ) */
/*              INTEGER               ND */
/*              INTEGER               NI */
/*              INTEGER               ORIGIN */
/*              INTEGER               TARGET */

/*              LOGICAL               FOUND */

/*        C */
/*        C     Open the origin DAF file for reading. */
/*        C */
/*              CALL DAFOPR ( 'vo2_swu_ck2.bc', ORIGIN ) */

/*        C */
/*        C     Start forward search in origin DAF. */
/*        C */
/*              CALL DAFBFS ( ORIGIN ) */

/*        C */
/*        C     Find the first array in origin DAF. */
/*        C */
/*              CALL DAFFNA ( FOUND  ) */

/*        C */
/*        C     Get the summary and name of the current array in the */
/*        C     ORIGIN DAF file */
/*        C */
/*              CALL DAFGS  ( SUM  ) */
/*              CALL DAFGN  ( NAME ) */

/*        C */
/*        C     Unpack the summary. */
/*        C */
/*              CALL DAFHSF ( ORIGIN, ND, NI ) */
/*              CALL DAFUS  ( SUM,    ND, NI, DC, IC ) */

/*        C */
/*        C     Open the target DAF file for writing. Use 'CK' as */
/*        C     data type, and reserve no records for comments. */
/*        C */
/*              CALL DAFONW ( 'dafana_ex1.bc', 'CK', ND, NI, */
/*             .              'CK file created for example 1 DAFANA', 0, */
/*             .              TARGET ) */

/*        C */
/*        C     Begin a new array in the target DAF file, using the */
/*        C     origin SUM and NAME. */
/*        C */
/*              CALL DAFBNA ( TARGET, SUM, NAME ) */

/*        C */
/*        C     Copy the complete array for the first segment of the */
/*        C     origin DAF file. */
/*        C */
/*              BIDX = IC(NI-1) */
/*              EIDX = IC(NI  ) */

/*              DO WHILE ( BIDX .LE. EIDX ) */

/*                 CHUNK = MIN ( BIDX + NWORDS - 1, EIDX ) */

/*                 CALL DAFGDA ( ORIGIN, BIDX, CHUNK, DATA ) */
/*                 CALL DAFADA ( DATA,   NWORDS ) */

/*                 BIDX = BIDX + NWORDS */

/*              END DO */

/*        C */
/*        C     End the new array in the target DAF. */
/*        C */
/*              CALL DAFENA */

/*        C */
/*        C     Close the DAF files. */
/*        C */
/*              CALL DAFCLS ( ORIGIN ) */
/*              CALL DAFCLS ( TARGET ) */

/*              END */


/*        When this program is executed, no output is presented on */
/*        screen. After run completion, a new CK file exists in the */
/*        output directory. */


/*     2)  A simple example demonstrating simultaneous addition */
/*         of data to multiple DAFs. */

/*         Assume we have data from a text file containing three */
/*         columns of numbers. We will write the data from each */
/*         column out to a separate DAF. */

/*         To confirm that the DAFs created by this program contain the */
/*         correct contents, we will read the data from each DAF and */
/*         combine it to create a matrix. This matrix should contain */
/*         the same data as the file we assumed to be the source for */
/*         our dataset. */

/*         The format of the output text should be as follows: */

/*            .-                   -. */
/*            |  n11    n12    n13  | */
/*            |  n21    n22    n23  | */
/*            |   .      .      .   | */
/*            |   .      .      .   | */
/*            |   .      .      .   | */
/*            `-                   -' */

/*         where the symbol nij indicates the jth number on the ith line */
/*         of the source data file. */


/*        Example code begins here. */


/*              PROGRAM DAFANA_EX2 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Assume we have read columns of d.p. numbers */
/*        C     from a text file. Write the data from each */
/*        C     column into a separate DAF.  Read these DAFs */
/*        C     and create a matrix containing the same data */
/*        C     as assumed input text file. */
/*        C */
/*        C     Since we do not need to retain any descriptive */
/*        C     information about the DAFs inside of the files */
/*        C     themselves, we'll use a summary format having */
/*        C     two integer components (the minimum--these are */
/*        C     reserved for use by the DAF routines) and zero */
/*        C     double precision components. */
/*        C */
/*        C     The internal file names and array names will */
/*        C     simply indicate the data sources. */
/*        C */

/*        C */
/*        C     Local parameters */
/*        C */
/*              INTEGER               FNMLEN */
/*              PARAMETER           ( FNMLEN = 20 ) */

/*              INTEGER               LINLEN */
/*              PARAMETER           ( LINLEN = 80 ) */

/*              INTEGER               MAXLNS */
/*              PARAMETER           ( MAXLNS =  9 ) */

/*              INTEGER               MAXCOL */
/*              PARAMETER           ( MAXCOL =  3 ) */

/*              INTEGER               ND */
/*              PARAMETER           ( ND     =  0 ) */

/*              INTEGER               NDAF */
/*              PARAMETER           ( NDAF   =  3 ) */

/*              INTEGER               NI */
/*              PARAMETER           ( NI     =  2 ) */

/*              INTEGER               NUMLEN */
/*              PARAMETER           ( NUMLEN = 30 ) */

/*              INTEGER               SIG */
/*              PARAMETER           ( SIG    = 10 ) */

/*        C */
/*        C     Local variables */
/*        C */
/*              CHARACTER*(FNMLEN)    DAFNAM ( NDAF   ) */
/*              CHARACTER*(FNMLEN)    INFILE */
/*              CHARACTER*(LINLEN)    LINE */
/*              CHARACTER*(NUMLEN)    NUMCH */
/*              CHARACTER*(LINLEN)    PRSERR */
/*              CHARACTER*(FNMLEN)    RESULT */

/*              DOUBLE PRECISION      DC     ( 1      ) */
/*              DOUBLE PRECISION      NUMBER ( MAXLNS, MAXCOL ) */
/*              DOUBLE PRECISION      NUMDP */
/*              DOUBLE PRECISION      SUMMRY ( 1      ) */

/*              INTEGER               FA */
/*              INTEGER               HAN    ( NDAF   ) */
/*              INTEGER               I */
/*              INTEGER               IA */
/*              INTEGER               IC     ( NI     ) */
/*              INTEGER               J */
/*              INTEGER               LENGTH */
/*              INTEGER               NCOLS */
/*              INTEGER               PTR */

/*              LOGICAL               EOF */
/*              LOGICAL               FOUND */

/*        C */
/*        C     Initial values */
/*        C */
/*              DATA                  DAFNAM   /  'COLUMN1.DAF', */
/*             .                                  'COLUMN2.DAF', */
/*             .                                  'COLUMN3.DAF'  / */

/*              DATA                  NUMBER  / */
/*             .               11.D0, 21.D0, 31.D0, 41.D0, 51.D0, */
/*             .               61.D0, 71.D0, 81.D0, 91.D0, */
/*             .               12.D0, 22.D0, 32.D0, 42.D0, 52.D0, */
/*             .               62.D0, 72.D0, 82.D0, 92.D0, */
/*             .               13.D0, 23.D0, 33.D0, 43.D0, 52.D0, */
/*             .               63.D0, 73.D0, 83.D0, 93.D0        / */

/*        C */
/*        C     Create the new DAFs, and start a new array in each */
/*        C     one.  Just use the file name for the internal file */
/*        C     name and array name, for each DAF.  No assignments */
/*        C     are required for the array summaries. */
/*        C */
/*              DO I = 1, NDAF */

/*                 CALL DAFOPN ( DAFNAM(I), ND, NI, */
/*             .                 DAFNAM(I), 0,  HAN(I) ) */

/*                 CALL DAFBNA ( HAN(I), SUMMRY, DAFNAM(I) ) */

/*              END DO */

/*        C */
/*        C     At this point, we assume that we have read the */
/*        C     file line by line. Add the numbers from each column */
/*        C     to the corresponding DAF. */
/*        C */
/*              DO I = 1, MAXLNS */

/*        C */
/*        C        Add the number from the ith column to the array */
/*        C        in the ith DAF.  We'll use DAFCAD to select */
/*        C        the correct DAF to add data to. */
/*        C */
/*                 DO J = 1, NDAF */
/*                    CALL DAFCAD ( HAN(J)         ) */
/*                    CALL DAFADA ( NUMBER(I,J), 1 ) */
/*                 END DO */

/*              END DO */

/*        C */
/*        C     Finish ("end") the arrays.  Again, we'll use DAFCAD */
/*        C     to select the DAFs in which the arrays are to be */
/*        C     finished.  After finishing each array, close the DAF */
/*        C     containing it. */
/*        C */
/*              DO I = 1, NDAF */
/*                 CALL DAFCAD ( HAN(I) ) */
/*                 CALL DAFENA */
/*                 CALL DAFCLS ( HAN(I) ) */
/*              END DO */

/*        C */
/*        C     Now for the verification step.  We'll try to */
/*        C     print a matrix containing the same data as */
/*        C     the original input file.  The format of the numbers, */
/*        C     the delimiters separating the numbers, spacing, and */
/*        C     non-printing characters may differ. */
/*        C */
/*        C     Open the DAFs for reading. */
/*        C */
/*              DO I = 1, NDAF */
/*                 CALL DAFOPR ( DAFNAM(I), HAN(I) ) */
/*              END DO */

/*        C */
/*        C     Obtain the start and end addresses of the */
/*        C     data in each DAF.  To do this, we'll need to */
/*        C     obtain and unpack the array summaries. */
/*        C */
/*        C     If all went well, the addresses should be the */
/*        C     same for each DAF.  We'll assume that the initial */
/*        C     and final addresses in the first DAF are correct */
/*        C     for all three. */
/*        C */
/*              CALL DAFBFS ( HAN(1) ) */
/*              CALL DAFFNA ( FOUND  ) */
/*              CALL DAFGS  ( SUMMRY ) */
/*              CALL DAFUS  ( SUMMRY, ND, NI, DC, IC ) */

/*              IA      =  IC( NI-1 ) */
/*              FA      =  IC( NI   ) */
/*              LENGTH  =  FA - IA + 1 */

/*        C */
/*        C     Now read numbers from the DAFs and build up */
/*        C     lines of text.  Print these lines out. */
/*        C */
/*              DO I = 0,  LENGTH - 1 */

/*                 LINE = ' ' */

/*                 DO J = 1, NDAF */

/*                    CALL DAFRDA ( HAN(J), IA+I, IA+I, NUMDP ) */

/*        C */
/*        C           Convert the double precision number to a */
/*        C           character string, and append it to the current */
/*        C           line. */
/*        C */
/*                    CALL DPSTR  ( NUMDP,  SIG,        NUMCH ) */
/*                    CALL SUFFIX ( NUMCH,  3,          LINE  ) */

/*                 END DO */

/*                 WRITE(*,*) LINE */

/*              END DO */

/*        C */
/*        C     Close the DAFs. */
/*        C */
/*              DO I = 1, NDAF */
/*                 CALL DAFCLS( HAN(I) ) */
/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*             1.100000000E+01    1.200000000E+01    1.300000000E+01 */
/*             2.100000000E+01    2.200000000E+01    2.300000000E+01 */
/*             3.100000000E+01    3.200000000E+01    3.300000000E+01 */
/*             4.100000000E+01    4.200000000E+01    4.300000000E+01 */
/*             5.100000000E+01    5.200000000E+01    5.200000000E+01 */
/*             6.100000000E+01    6.200000000E+01    6.300000000E+01 */
/*             7.100000000E+01    7.200000000E+01    7.300000000E+01 */
/*             8.100000000E+01    8.200000000E+01    8.300000000E+01 */
/*             9.100000000E+01    9.200000000E+01    9.300000000E+01 */


/*        Note that after run completion, three new DAF files exist in */
/*        the output directory. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     H.A. Neilan        (JPL) */
/*     W.L. Taber         (JPL) */
/*     F.S. Turner        (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 3.1.0, 13-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header of DAFANA and all entry points to comply with */
/*        NAIF standard. Added complete code examples to DAFANA $Examples */
/*        section based on the existing fragments. */

/* -    SPICELIB Version 3.0.0, 16-NOV-2001 (FST) */

/*        Updated the entry points of DAFANA to enable its */
/*        internal state table size, TBSIZE, to be smaller */
/*        than the file table maintained by DAFAH: FTSIZE. */

/* -    SPICELIB Version 2.1.0, 11-JUL-1995 (KRG) */

/*        Updated to remove potential compiler warnings from the */
/*        truncation of double precision numbers to integers. */

/*        Also changed was a numeric constant from 1.D0 to the */
/*        equivalent, but more aesthetically pleasing 1.0D0. */

/* -    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB) (WLT) */

/*        Updated to support simultaneous writes to multiple DAFs. */
/*        The $Examples section of this routine now illustrates */
/*        usage of the routine DAFCAD. */

/* -    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN) */

/*        Literature references added to the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) */

/* -& */
/* $ Index_Entries */

/*     add new DAF array */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 3.0.0, 16-NOV-2001 (FST) */

/*        This umbrella and its entry points were updated to */
/*        work properly with the changes in the DAF system as */
/*        a result of its utilization of the new handle manager. */

/*        Since DAFAH now tracks FTSIZE files as defined in */
/*        the include file 'zzddhman.inc', it was decided that */
/*        in the interest of releasing the toolkit this module */
/*        would undergo simple changes.  As such most previous */
/*        references to FTSIZE in this umbrella have been replaced */
/*        with TBSIZE where appropriate.  DAFBNA now signals an */
/*        error if there is not enough room to add a new DAF's */
/*        dossier to the state table.  Also, after attempting to */
/*        clean up all files listed in the state table that are */
/*        not currently open, DAFBNA attempts to locate the */
/*        first dossier with STADDG set to FALSE.  This is then */
/*        freed to make room for the new DAF.  If DAFBNA fails */
/*        to locate such a dossier in the state table, it */
/*        signals the error SPICE(STFULL). */

/*        The parameter FILEN was removed, as it is defined */
/*        on an environmental basis in the include file */
/*        'zzddhman.inc'. */


/* -    SPICELIB Version 2.1.0, 11-JUL-1995 (KRG) */

/*        Updated to remove potential compiler warnings from the */
/*        truncation of double precision numbers to integers. Two */
/*        assignments to NARRAY were updated, being changed from: */

/*           NARRAY = SUMREC(ARYCNT) */

/*        to */

/*           NARRAY = IDINT ( SUMREC(ARYCNT) ) */

/*        Also changed was a numeric constant from 1.D0 to the */
/*        equivalent, but more aesthetically pleasing 1.0D0. */

/* -    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB) (WLT) */

/*        Updated to support simultaneous writes to multiple DAFs. */

/*        In previous versions of DAFANA, data could be added to only */
/*        one DAF array at a time.  In fact, DAFAH allowed only one */
/*        DAF to be open for writing at any time.  Therefore, there was */
/*        no question about which DAF was being operated on by either of */
/*        the DAFANA entry points that don't accept file handles as */
/*        input arguments:  DAFADA and DAFENA.  In the current version */
/*        of DAFANA, the entry points that don't accept file handles as */
/*        inputs operate on the `current DAF'.  The current DAF is the */
/*        last one in which a new array was started by DAFBNA, or in */
/*        which addition of data to an array was continued by the new */
/*        entry point DAFCAD.  DAFCAD was added to allow users to set */
/*        the current DAF, so that additions of data to arrays in */
/*        multiple DAFs can be interleaved. */

/*        Note that the notion of `current DAF' as discussed here applies */
/*        only to DAFs acted upon by entry points of DAFANA. In DAFFA, */
/*        there is a DAF that is treated as the `current DAF' for */
/*        searching; there is no connection between the DAFs regarded */
/*        as current by DAFANA and DAFFA. */

/*        The two principal changes to DAFANA are the addition of the */
/*        new entry point DAFCAD, and the addition of a data structure */
/*        called the `state table'. The state table is a collection of */
/*        parallel arrays that maintain information about the state */
/*        of each data addition that is currently in progress. The */
/*        state table arrays are indexed by a singly linked list pool; */
/*        this mechanism allows addition and deletion of information */
/*        about data additions without requiring movement of data */
/*        already in the state table. */

/*        The linked list pool contains an `active' list and a `free' */
/*        list. Nodes in the active list are used to index elements of */
/*        the state table where information about additions in progress */
/*        is stored. The head node of the active list is of particular */
/*        significance: the state information pointed to by this node */
/*        is that of the current DAF. Nodes in the free list index */
/*        elements of the state table that are available for use. */

/*        When an array is started in a DAF that is not already `known' */
/*        to DAFANA, information about the DAF is added to the state */
/*        table. If there are no free elements in the state table, */
/*        the routine starting the array (DAFBNA) will perform garbage */
/*        collection: the routine will test the handles of each file */
/*        about which information in stored in the state table to see */
/*        whether that file is still open. Nodes containing information */
/*        about DAFs that are no longer open will be moved to the free */
/*        list. */

/*        Whenever a DAF becomes the current DAF, the linked list */
/*        that indexes the state table is adjusted so that the node */
/*        pointing to information about the current DAF is at the head */
/*        of the active list. This way, a slight efficiency is gained */
/*        when repeated data additions are made to the same DAF, since */
/*        the linear search through the state table for information on */
/*        that DAF will be shortened. */

/*        Since the algorithms for maintenance of linked lists are well */
/*        known, they are not documented here. However, see the */
/*        internals of the SPICELIB routine SPKBSR for a nice diagram */
/*        describing a similar data structure. */

/*        The state table contains two arrays that are quite large: */
/*        there are buffers that contain the name and array summary for */
/*        each array under construction. A parallel situation exists */
/*        in DAFFA, where there are buffers that contain the last */
/*        character record and summary record read from each DAF. The */
/*        total storage required for these arrays (in DAFANA and DAFFA */
/*        together) is 4000 * TBSIZE bytes. For this reason, it may be */
/*        a good idea to reduce the value of TBSIZE in SPICELIB versions */
/*        for machines where memory is scarce. */

/*        On a completely different topic: the local declarations in */
/*        DAFANA have been alphabetized and separated by type, except */
/*        for those relating to the state table. Several hard-coded */
/*        constants have been replaced by parameters. */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     State variables. */

/*     These variables define the state of each DAF to which data */
/*     is currently being added.  For each DAF that we're writing to, we */
/*     maintain a copy of: */

/*        STFH           File handle. */

/*        STIFNM         Internal file name. */

/*        STADDG         (`State table: adding') Flag indicating */
/*                       whether addition of data to an array is in */
/*                       progress. */

/*        STFRST         Record number of initial summary record. */

/*        STLAST         Record number of final summary record. */

/*        STBEGN         Beginning address of new array. */

/*        STFREE         Address of next free word. */

/*        STLSUM         Local copy of the array summary for the current */
/*                       array. */

/*        STNAME         Local copy of the array name for the current */
/*                       array. */


/*     These variables are maintained in a table of parallel arrays; */
/*     the size of the table is TBSIZE. */



/*     The table of state variables is indexed by a singly linked list */
/*     of pointers.  This mechanism avoids the work of moving */
/*     the state variable data about as information about DAFs is */
/*     added to or deleted from the table. */

/*     The structure containing the linked list pointers is called a */
/*     `pool.'  The pool contains a list of `active' nodes and a list */
/*     of free nodes.  The head nodes of the active and free lists are */
/*     maintained as the variables STHEAD (`state table head') and */
/*     STFPTR (`state table free pointer'), respectively.  Every node in */
/*     the pool is on exactly one of these lists. */


/*     The pool starts out with all of the nodes on the free list. */
/*     DAFBNA initializes the pool.  As new DAFs are written to, */
/*     DAFBNA adds information about them to the state table.  Every */
/*     time a DAF array is started by DAFBNA, or selected for */
/*     continuation by DAFCAD, the routine in question `moves' the */
/*     DAF's state information to the head of the active list, if the */
/*     state information is not already there.  This re-organization is */
/*     accomplished by deleting the node for the DAF from its current */
/*     position in the active list and inserting the node at the head of */
/*     the list.  Thus, the change is made merely by setting pointers, */
/*     not by moving chunks of data in the state table. */

/*     It may happen that there is no room left in the state table */
/*     to accommodate information about a new DAF.  In this case, */
/*     garbage collection must be performed:  DAFBNA frees all nodes in */
/*     the table that index DAFs that are not currently open. */

/*     Note that the routine DAFADA does not modify the state table; it */
/*     merely adds data to the DAF that is at the head of the active */
/*     list. */


/*     Other local variables */


/*     Save everything between calls */


/*     Initial values */

    /* Parameter adjustments */
    if (sum) {
	}
    if (data) {
	}

    /* Function Body */
    switch(n__) {
	case 1: goto L_dafbna;
	case 2: goto L_dafada;
	case 3: goto L_dafena;
	case 4: goto L_dafcad;
	}


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("DAFANA", (ftnlen)6);
	sigerr_("SPICE(BOGUSENTRY)", (ftnlen)17);
	chkout_("DAFANA", (ftnlen)6);
    }
    return 0;
/* $Procedure DAFBNA ( DAF, begin new array ) */

L_dafbna:
/* $ Abstract */

/*     Begin a new array in a DAF. */

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

/*     DAF */

/* $ Keywords */

/*     FILES */

/* $ Declarations */

/*     IMPLICIT NONE */

/*     INTEGER               HANDLE */
/*     DOUBLE PRECISION      SUM     ( * ) */
/*     CHARACTER*(*)         NAME */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Handle of DAF. */
/*     SUM        I   Summary of new array. */
/*     NAME       I   Name of new array. */

/* $ Detailed_Input */

/*     HANDLE   is the handle of a DAF opened for write access */
/*              by a previous call to DAFOPW or DAFOPN. */

/*     SUM      is the summary of a new array to be added to the */
/*              specified file. The addresses (the final two integer */
/*              components) need not be filled in. */

/*     NAME     is the name of the new array. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the input handle is not that of a DAF that is open for */
/*         writing, an error is signaled by a routine in the call tree of */
/*         this routine. These files are implicitly of the native binary */
/*         file format. */

/*     2)  If the input array name is too long to fit in the number */
/*         of characters allowed by the summary format of the DAF */
/*         designated by HANDLE, the excess characters are truncated. */
/*         No error is signaled. */

/*     3)  If there is not enough room in the state table to add */
/*         the DAF associated with HANDLE, the error SPICE(STFULL) */
/*         is signaled. */

/* $ Files */

/*     See argument HANDLE, above. */

/* $ Particulars */

/*     Only one array can be added to a DAF at any one time, so */
/*     calling DAFBNA cancels any addition to the file specified */
/*     by HANDLE that may be in progress. No warning is issued. */

/* $ Examples */

/*     See $Examples in DAFANA. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     H.A. Neilan        (JPL) */
/*     W.L. Taber         (JPL) */
/*     F.S. Turner        (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 3.1.0, 06-JUL-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 3.0.0, 16-NOV-2001 (FST) */

/*        Updated DAFBNA to support changes made to the DAF */
/*        system that utilize the new handle manager. See */
/*        the $Revisions section of DAFANA for a detailed */
/*        discussion of the changes. */

/* -    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB) (WLT) */

/*        Modified to support simultaneous writes to multiple DAFs. */

/* -    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN) */

/*        Literature references added to the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) */

/* -& */
/* $ Index_Entries */

/*     begin new DAF array */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB) (WLT) */

/*        Modified to support simultaneous writes to multiple DAFs. */
/*        DAFBNA now adds information about DAFs to the state table, */
/*        deletes information about closed DAFs from the state table, */
/*        and initializes the state pool. */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("DAFBNA", (ftnlen)6);
    }

/*     Check out the file handle before going any further. */

    dafsih_(handle, "WRITE", (ftnlen)5);
    if (failed_()) {
	chkout_("DAFBNA", (ftnlen)6);
	return 0;
    }

/*     Initialize the state table pool, if this hasn't been done yet. */
/*     Also initialize the cell used to obtain the set of handles of */
/*     open DAFs. */

    if (first) {
	ssizei_(&c__5000, opnset);
	for (i__ = 1; i__ <= 19; ++i__) {
	    stpool[(i__1 = i__ - 1) < 20 && 0 <= i__1 ? i__1 : s_rnge("stpool"
		    , i__1, "dafana_", (ftnlen)1197)] = i__ + 1;
	}
	stpool[19] = -1;
	stfptr = 1;
	sthead = -1;
	first = FALSE_;
    }

/*     We know that the beginning of the array will be the first */
/*     free address in the file. We also need the summary format. */
/*     Get both items from the file record. */

/*     We won't use the information we're obtaining now until */
/*     after we've placed the state information for the current */
/*     DAF at the head of the active list, but we want to make sure */
/*     that we can actually read the file record first.  So, we */
/*     do the read now and avoid modifying the active list if the */
/*     read fails. */

    dafrfr_(handle, &nd, &ni, ifname, &fward, &bward, &free, (ftnlen)60);

/*     If we couldn't read the file record, bail out now. */

    if (failed_()) {
	chkout_("DAFBNA", (ftnlen)6);
	return 0;
    }

/*     See whether we already have an entry for this DAF in the */
/*     state table.  Find the previous node if possible. */

    p = sthead;
    prev = -1;
    found = FALSE_;
    while(p != -1 && ! found) {
	if (stfh[(i__1 = p - 1) < 20 && 0 <= i__1 ? i__1 : s_rnge("stfh", 
		i__1, "dafana_", (ftnlen)1239)] == *handle) {
	    found = TRUE_;
	} else {
	    prev = p;
	    p = stpool[(i__1 = p - 1) < 20 && 0 <= i__1 ? i__1 : s_rnge("stp"
		    "ool", i__1, "dafana_", (ftnlen)1243)];
	}
    }

/*     At this point, either FOUND is false, or P points to a */
/*     state table entry describing the DAF indicated by HANDLE. */
/*     In the latter case, PREV is the predecessor of P. */


    if (found) {

/*        We already have a dossier on this DAF.  We already have */
/*        the information on the summary format, but we must re-set */
/*        the rest of our state information. */

/*        Rather than doing the update here, we do it outside of this */
/*        IF block.  That way, the update gets done in just one place. */
/*        This just makes life easier:  if the collection of state */
/*        variables is changed, there are fewer places to forget to */
/*        make the required code changes. */

/*        Move the node for this DAF to the head of the active list, */
/*        if it is not already there: */

/*           - Make the predecessor of P point to the successor of P. */

/*           - Make P point to the head of the active list. */

/*           - Make P the active list head node. */


	if (p != sthead) {

/*           P is in the active list, but is not at the head.  So, */
/*           the predecessor of P is not NIL. */

	    stpool[(i__1 = prev - 1) < 20 && 0 <= i__1 ? i__1 : s_rnge("stpo"
		    "ol", i__1, "dafana_", (ftnlen)1281)] = stpool[(i__2 = p - 
		    1) < 20 && 0 <= i__2 ? i__2 : s_rnge("stpool", i__2, 
		    "dafana_", (ftnlen)1281)];
	    stpool[(i__1 = p - 1) < 20 && 0 <= i__1 ? i__1 : s_rnge("stpool", 
		    i__1, "dafana_", (ftnlen)1282)] = sthead;
	    sthead = p;
	}
    } else {

/*        We don't yet have any information on this DAF.  Make a new */
/*        state table entry for the DAF.  We may need to make room for */
/*        the new information by freeing space allocated to DAFs that */
/*        are no longer open. */

	if (stfptr == -1) {

/*           Oops, we're out of space.  Time for garbage collection. */
/*           Test each file handle to see whether it designates a DAF */
/*           that is still open.  DAFHOF will tell us which handles */
/*           point to open DAFs. */

	    dafhof_(opnset);
	    p = sthead;
	    prev = -1;

/*           For every DAF file represented in the state table, we'll */
/*           delete the corresponding state information if the DAF is */
/*           now closed.  We traverse the active list, examining each */
/*           file handle as we go. */

	    while(p != -1) {
		if (elemi_(&stfh[(i__1 = p - 1) < 20 && 0 <= i__1 ? i__1 : 
			s_rnge("stfh", i__1, "dafana_", (ftnlen)1315)], 
			opnset)) {

/*                 The file is open. Have a look at the next node. */

		    prev = p;
		    p = stpool[(i__1 = p - 1) < 20 && 0 <= i__1 ? i__1 : 
			    s_rnge("stpool", i__1, "dafana_", (ftnlen)1320)];
		} else {

/*                 This file handle is not on the list, so free the */
/*                 node pointing to the information about the DAF it */
/*                 designated: */

/*                    - Save the successor of P. */

/*                    - Link the predecessor of node P to the successor */
/*                      of P, if the predecessor is not NIL. */

/*                    - If it happens that P is the head node of the */
/*                      active list, set the head equal to the */
/*                      successor of P. */

/*                    - Link P into the free list. */

/*                    - Set P equal to its saved successor. */

/*                    - (PREV remains unchanged.) */


		    nextp = stpool[(i__1 = p - 1) < 20 && 0 <= i__1 ? i__1 : 
			    s_rnge("stpool", i__1, "dafana_", (ftnlen)1344)];
		    if (p == sthead) {

/*                    Re-assign STHEAD so that we don't lose the head */
/*                    of the active list.  P has no predecessor in this */
/*                    case, so there's no need to set the forward pointer */
/*                    of node PREV. */

			sthead = nextp;
		    } else {

/*                    Since P is not the head node of the active list, */
/*                    PREV is not NIL, so we'll need to set the forward */
/*                    pointer of node PREV. */

			stpool[(i__1 = prev - 1) < 20 && 0 <= i__1 ? i__1 : 
				s_rnge("stpool", i__1, "dafana_", (ftnlen)
				1361)] = nextp;
		    }
		    stpool[(i__1 = p - 1) < 20 && 0 <= i__1 ? i__1 : s_rnge(
			    "stpool", i__1, "dafana_", (ftnlen)1366)] = 
			    stfptr;
		    stfptr = p;
		    p = nextp;
		}
	    }

/*           At this point, we've freed all nodes from the active */
/*           list that were used to index information about DAFs that */
/*           are no longer open.  Now see if we still need to make */
/*           room.  If so, locate the first dossier with STADDG(P) */
/*           set to FALSE.  We know then that this file is not */
/*           currently involved in an array addition. */

	    if (stfptr == -1) {
		found = FALSE_;
		p = sthead;
		prev = -1;
		while(p != -1 && ! found) {

/*                 If STADDG(P) is TRUE, then we must continue */
/*                 searching. */

		    if (staddg[(i__1 = p - 1) < 20 && 0 <= i__1 ? i__1 : 
			    s_rnge("staddg", i__1, "dafana_", (ftnlen)1394)]) 
			    {
			prev = p;
			p = stpool[(i__1 = p - 1) < 20 && 0 <= i__1 ? i__1 : 
				s_rnge("stpool", i__1, "dafana_", (ftnlen)
				1397)];
		    } else {
			found = TRUE_;

/*                    No array is presently being added to the DAF */
/*                    associated with this dossier, so free the */
/*                    node pointing to the information about the DAF it */
/*                    designated: */

/*                    - Save the successor of P. */

/*                    - Link the predecessor of node P to the successor */
/*                      of P, if the predecessor is not NIL. */

/*                    - If it happens that P is the head node of the */
/*                      active list, set the head equal to the */
/*                      successor of P. */

/*                    - Link P into the free list. */

/*                    - Set P equal to its saved successor. */

/*                    - (PREV remains unchanged.) */


			nextp = stpool[(i__1 = p - 1) < 20 && 0 <= i__1 ? 
				i__1 : s_rnge("stpool", i__1, "dafana_", (
				ftnlen)1424)];
			if (p == sthead) {

/*                       Re-assign STHEAD so that we don't lose the head */
/*                       of the active list.  P has no predecessor in */
/*                       this case, so there's no need to set the */
/*                       forward pointer of node PREV. */

			    sthead = nextp;
			} else {

/*                       Since P is not the head node of the active list, */
/*                       PREV is not NIL, so we'll need to set the */
/*                       forward pointer of node PREV. */

			    stpool[(i__1 = prev - 1) < 20 && 0 <= i__1 ? i__1 
				    : s_rnge("stpool", i__1, "dafana_", (
				    ftnlen)1441)] = nextp;
			}
			stpool[(i__1 = p - 1) < 20 && 0 <= i__1 ? i__1 : 
				s_rnge("stpool", i__1, "dafana_", (ftnlen)
				1446)] = stfptr;
			stfptr = p;
			p = nextp;
		    }
		}
	    }

/*           Now, check to see if there is now room to add the dossier */
/*           for the new DAF to the state table.  If not signal an error. */

	    if (stfptr == -1) {
		setmsg_("Attempt to initiate create a new array in DAF '#' h"
			"as failed. DAFANA's state table has room to manage w"
			"riting to # new arrays simultaneously, but there is "
			"no room left in the table for this DAF.", (ftnlen)194)
			;
		errhan_("#", handle, (ftnlen)1);
		errint_("#", &c__20, (ftnlen)1);
		sigerr_("SPICE(STFULL)", (ftnlen)13);
		chkout_("DAFBNA", (ftnlen)6);
		return 0;
	    }
	}

/*        If we reach here, then we have room in the state table for */
/*        the new DAF.  The first free node is indicated by SFTPTR. */
/*        Allocate this node and use it to index the state information */
/*        for the new DAF. */

	p = stfptr;

/*        Update the free list pointer, link P to the previous head */
/*        of the active list, and make P the head of the active list. */

	stfptr = stpool[(i__1 = p - 1) < 20 && 0 <= i__1 ? i__1 : s_rnge(
		"stpool", i__1, "dafana_", (ftnlen)1490)];
	stpool[(i__1 = p - 1) < 20 && 0 <= i__1 ? i__1 : s_rnge("stpool", 
		i__1, "dafana_", (ftnlen)1491)] = sthead;
	sthead = p;
    }

/*     At this point, P is the head node of the active list, and P is */
/*     the index in the state table of the information for the current */
/*     DAF. */


/*     Set the state information for the current array. */

    stfh[(i__1 = p - 1) < 20 && 0 <= i__1 ? i__1 : s_rnge("stfh", i__1, "daf"
	    "ana_", (ftnlen)1505)] = *handle;
    s_copy(stifnm + ((i__1 = p - 1) < 20 && 0 <= i__1 ? i__1 : s_rnge("stifnm"
	    , i__1, "dafana_", (ftnlen)1506)) * 60, ifname, (ftnlen)60, (
	    ftnlen)60);
    staddg[(i__1 = p - 1) < 20 && 0 <= i__1 ? i__1 : s_rnge("staddg", i__1, 
	    "dafana_", (ftnlen)1507)] = TRUE_;
    stfrst[(i__1 = p - 1) < 20 && 0 <= i__1 ? i__1 : s_rnge("stfrst", i__1, 
	    "dafana_", (ftnlen)1508)] = fward;
    stlast[(i__1 = p - 1) < 20 && 0 <= i__1 ? i__1 : s_rnge("stlast", i__1, 
	    "dafana_", (ftnlen)1509)] = bward;
    stbegn[(i__1 = p - 1) < 20 && 0 <= i__1 ? i__1 : s_rnge("stbegn", i__1, 
	    "dafana_", (ftnlen)1510)] = free;
    stfree[(i__1 = p - 1) < 20 && 0 <= i__1 ? i__1 : s_rnge("stfree", i__1, 
	    "dafana_", (ftnlen)1511)] = free;

/*     Find out how big the array summary is supposed to be. */

    dafhsf_(&stfh[(i__1 = p - 1) < 20 && 0 <= i__1 ? i__1 : s_rnge("stfh", 
	    i__1, "dafana_", (ftnlen)1516)], &nd, &ni);
    sumsiz = nd + (ni + 1) / 2;

/*     Set the local copies of the array's summary and name. */

    moved_(sum, &sumsiz, &stlsum[(i__1 = p * 125 - 125) < 2500 && 0 <= i__1 ? 
	    i__1 : s_rnge("stlsum", i__1, "dafana_", (ftnlen)1523)]);
    s_copy(stname + ((i__1 = p - 1) < 20 && 0 <= i__1 ? i__1 : s_rnge("stname"
	    , i__1, "dafana_", (ftnlen)1525)) * 1000, name__, (ftnlen)1000, 
	    name_len);
    chkout_("DAFBNA", (ftnlen)6);
    return 0;
/* $Procedure DAFADA ( DAF, add data to array ) */

L_dafada:
/* $ Abstract */

/*     Add one or more double precision words of data to the newest */
/*     array in the current DAF. */

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

/*     DAF */

/* $ Keywords */

/*     FILES */

/* $ Declarations */

/*     IMPLICIT NONE */

/*     DOUBLE PRECISION      DATA     ( * ) */
/*     INTEGER               N */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     DATA       I   Elements of the new array. */
/*     N          I   Number of elements in DATA. */

/* $ Detailed_Input */

/*     DATA     is an arbitrary number of double precision words to */
/*              be added to the data in the array being created. */

/*     N        is the number of double precision words in DATA. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If there are no DAFs to which data is currently being added, */
/*         the error SPICE(DAFNOWRITE) is signaled. */

/*     2)  If a new array has not been started in the current DAF (by a */
/*         call to DAFBNA), the error SPICE(DAFNEWCONFLICT) is signaled. */

/*     3)  If N is less than one, no data are added to the file. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     DAFADA adds data to the last array begun by DAFBNA or selected */
/*     by DAFCAD. */

/*     Data can be added to a DAF in chunks of any size, so long */
/*     as the chunks are added in the proper order. */

/* $ Examples */

/*     See $Examples in DAFANA. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     H.A. Neilan        (JPL) */
/*     W.L. Taber         (JPL) */
/*     F.S. Turner        (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 3.1.0, 06-JUL-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 3.0.0, 16-NOV-2001 (FST) */

/*        Updated entry points to support changes made to the DAF */
/*        system that utilize the new handle manager. See */
/*        the $Revisions section of DAFANA for a detailed */
/*        discussion of the changes. */

/* -    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB) (WLT) */

/*        Updated to work with new DAF routines that allow writing */
/*        to multiple DAFs simultaneously. Functionality for */
/*        applications that write to one DAF at a time is unchanged. */

/* -    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN) */

/*        Literature references added to the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) */

/* -& */
/* $ Index_Entries */

/*     add data to DAF array */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB) (WLT) */

/*        Updated to work with new DAF routines that allow writing */
/*        to multiple DAFs simultaneously. Functionality for */
/*        applications that write to one DAF at a time is unchanged. */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("DAFADA", (ftnlen)6);
    }

/*     This routine operates on the DAF at the head of the active list. */

    p = sthead;

/*     We must make sure that the requested addition can be performed. */
/*     We don't validate the file handle here because this is one place */
/*     where we are concerned about speed.  The low-level writer routine */
/*     DAFWDR will handle the check. */

    if (p == -1) {
	setmsg_("No DAF is currently being written.", (ftnlen)34);
	sigerr_("SPICE(DAFNOWRITE)", (ftnlen)17);
	chkout_("DAFADA", (ftnlen)6);
	return 0;

/*     An array cannot be extended unless begun first. */

    } else if (! staddg[(i__1 = p - 1) < 20 && 0 <= i__1 ? i__1 : s_rnge(
	    "staddg", i__1, "dafana_", (ftnlen)1732)]) {

/*        Validate the current handle, then get the name of the DAF. */

	dafsih_(&stfh[(i__1 = p - 1) < 20 && 0 <= i__1 ? i__1 : s_rnge("stfh",
		 i__1, "dafana_", (ftnlen)1736)], "WRITE", (ftnlen)5);
	if (failed_()) {
	    chkout_("DAFADA", (ftnlen)6);
	    return 0;
	}
	dafhfn_(&stfh[(i__1 = p - 1) < 20 && 0 <= i__1 ? i__1 : s_rnge("stfh",
		 i__1, "dafana_", (ftnlen)1743)], dafnam, (ftnlen)255);
	setmsg_("An attempt was made to add data to an array that has not ye"
		"t been begun, in file #.", (ftnlen)83);
	errch_("#", dafnam, (ftnlen)1, (ftnlen)255);
	sigerr_("SPICE(DAFNEWCONFLICT)", (ftnlen)21);
	chkout_("DAFADA", (ftnlen)6);
	return 0;

/*     Start adding data at the first free address, then update that */
/*     address to get ready for the next addition. */

    } else if (*n >= 1) {
	i__4 = stfree[(i__3 = p - 1) < 20 && 0 <= i__3 ? i__3 : s_rnge("stfr"
		"ee", i__3, "dafana_", (ftnlen)1757)] + *n - 1;
	dafwda_(&stfh[(i__1 = p - 1) < 20 && 0 <= i__1 ? i__1 : s_rnge("stfh",
		 i__1, "dafana_", (ftnlen)1757)], &stfree[(i__2 = p - 1) < 20 
		&& 0 <= i__2 ? i__2 : s_rnge("stfree", i__2, "dafana_", (
		ftnlen)1757)], &i__4, data);
	stfree[(i__1 = p - 1) < 20 && 0 <= i__1 ? i__1 : s_rnge("stfree", 
		i__1, "dafana_", (ftnlen)1758)] = stfree[(i__2 = p - 1) < 20 
		&& 0 <= i__2 ? i__2 : s_rnge("stfree", i__2, "dafana_", (
		ftnlen)1758)] + *n;
    }
    chkout_("DAFADA", (ftnlen)6);
    return 0;
/* $Procedure DAFENA ( DAF, end new array ) */

L_dafena:
/* $ Abstract */

/*     End the addition of data to the newest array in the current DAF. */

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

/*     DAF */

/* $ Keywords */

/*     FILES */

/* $ Declarations */

/*     None. */

/* $ Brief_I/O */

/*     None. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If there are no DAFs to which data is currently being added, */
/*         the error SPICE(DAFNOWRITE) is signaled, or the error will */
/*         be detected by routines called by this routine. */

/*     2)  If a new array has not been started in the current DAF (by a */
/*         call to DAFBNA), the error SPICE(DAFNEWCONFLICT) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     DAFENA makes the current array a permanent addition to the */
/*     current DAF. */

/*     The pointers within the file are not changed until an array */
/*     is ended successfully. If an error occurs or if the current */
/*     DAF is closed before DAFENA is called, the last array will */
/*     not be visible to the DAF reader routines. */

/* $ Examples */

/*     See $Examples in DAFANA. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     H.A. Neilan        (JPL) */
/*     W.L. Taber         (JPL) */
/*     F.S. Turner        (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 3.1.0, 06-JUL-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Removed */
/*        unnecessary entries from $Revisions section. */

/* -    SPICELIB Version 3.0.0, 16-NOV-2001 (FST) */

/*        Updated entry points to support changes made to the DAF */
/*        system that utilize the new handle manager. See */
/*        the $Revisions section of DAFANA for a detailed */
/*        discussion of the changes. */

/* -    SPICELIB Version 2.1.0, 11-JUL-1995 (KRG) */

/*        Updated to remove potential compiler warnings from the */
/*        truncation of double precision numbers to integers. */

/*        Also changed was a numeric constant from 1.D0 to the */
/*        equivalent, but more aesthetically pleasing 1.0D0. */

/* -    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB) (WLT) */

/*        Updated to work with new DAF routines that allow writing */
/*        to multiple DAFs simultaneously. Functionality for */
/*        applications that write to one DAF at a time is unchanged. */

/* -    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN) */

/*        Literature references added to the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) */

/* -& */
/* $ Index_Entries */

/*     end new DAF array */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 2.1.0, 11-JUL-1995 (KRG) */

/*        Updated to remove potential compiler warnings from the */
/*        truncation of double precision numbers to integers. Two */
/*        assignments to NARRAY were updated, being changed from: */

/*           NARRAY = SUMREC(ARYCNT) */

/*        to */

/*           NARRAY = IDINT ( SUMREC(ARYCNT) ) */

/*        Also changed was a numeric constant from 1.D0 to the */
/*        equivalent, but more aesthetically pleasing 1.0D0. */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("DAFENA", (ftnlen)6);
    }

/*     This routine operates on the DAF at the head of the active list. */

    p = sthead;
    if (p == -1) {
	setmsg_("No DAF is currently being written.", (ftnlen)34);
	sigerr_("SPICE(DAFNOWRITE)", (ftnlen)17);
	chkout_("DAFENA", (ftnlen)6);
	return 0;

/*     A new array cannot be ended unless begun first. */

    } else if (! staddg[(i__1 = p - 1) < 20 && 0 <= i__1 ? i__1 : s_rnge(
	    "staddg", i__1, "dafana_", (ftnlen)1971)]) {

/*        Validate the current handle, then get the name of the DAF. */

	dafsih_(&stfh[(i__1 = p - 1) < 20 && 0 <= i__1 ? i__1 : s_rnge("stfh",
		 i__1, "dafana_", (ftnlen)1975)], "WRITE", (ftnlen)5);
	if (failed_()) {
	    chkout_("DAFENA", (ftnlen)6);
	    return 0;
	}
	dafhfn_(&stfh[(i__1 = p - 1) < 20 && 0 <= i__1 ? i__1 : s_rnge("stfh",
		 i__1, "dafana_", (ftnlen)1982)], dafnam, (ftnlen)255);
	setmsg_("An attempt was made to end an array that has not yet been b"
		"egun, in file #.", (ftnlen)75);
	errch_("#", dafnam, (ftnlen)1, (ftnlen)255);
	sigerr_("SPICE(DAFNEWCONFLICT)", (ftnlen)21);
	chkout_("DAFENA", (ftnlen)6);
	return 0;
    }

/*     No more data. The array ends just before the next free */
/*     address. The summary should be complete except for the */
/*     initial and final addresses of the data, of which we */
/*     have been keeping track. */

    dafhsf_(&stfh[(i__1 = p - 1) < 20 && 0 <= i__1 ? i__1 : s_rnge("stfh", 
	    i__1, "dafana_", (ftnlen)1998)], &nd, &ni);
    dafus_(&stlsum[(i__1 = p * 125 - 125) < 2500 && 0 <= i__1 ? i__1 : s_rnge(
	    "stlsum", i__1, "dafana_", (ftnlen)2000)], &nd, &ni, dc, ic);
    ic[(i__1 = ni - 2) < 250 && 0 <= i__1 ? i__1 : s_rnge("ic", i__1, "dafan"
	    "a_", (ftnlen)2002)] = stbegn[(i__2 = p - 1) < 20 && 0 <= i__2 ? 
	    i__2 : s_rnge("stbegn", i__2, "dafana_", (ftnlen)2002)];
    ic[(i__1 = ni - 1) < 250 && 0 <= i__1 ? i__1 : s_rnge("ic", i__1, "dafan"
	    "a_", (ftnlen)2003)] = stfree[(i__2 = p - 1) < 20 && 0 <= i__2 ? 
	    i__2 : s_rnge("stfree", i__2, "dafana_", (ftnlen)2003)] - 1;
    dafps_(&nd, &ni, dc, ic, &stlsum[(i__1 = p * 125 - 125) < 2500 && 0 <= 
	    i__1 ? i__1 : s_rnge("stlsum", i__1, "dafana_", (ftnlen)2005)]);

/*     The summary should be stored in the final summary record (the */
/*     one at the end of the file). Get that entire record, and the */
/*     corresponding name record. */

    dafrdr_(&stfh[(i__1 = p - 1) < 20 && 0 <= i__1 ? i__1 : s_rnge("stfh", 
	    i__1, "dafana_", (ftnlen)2012)], &stlast[(i__2 = p - 1) < 20 && 0 
	    <= i__2 ? i__2 : s_rnge("stlast", i__2, "dafana_", (ftnlen)2012)],
	     &c__1, &c__128, sumrec, &found);
    i__3 = stlast[(i__2 = p - 1) < 20 && 0 <= i__2 ? i__2 : s_rnge("stlast", 
	    i__2, "dafana_", (ftnlen)2013)] + 1;
    dafrcr_(&stfh[(i__1 = p - 1) < 20 && 0 <= i__1 ? i__1 : s_rnge("stfh", 
	    i__1, "dafana_", (ftnlen)2013)], &i__3, namrec, (ftnlen)1000);
    narray = (integer) sumrec[2];

/*     The number of arrays determines where the summary and name */
/*     are stored within the summary record. Adding this array increases */
/*     the number of arrays by one. */

    sumsiz = nd + (ni + 1) / 2;
    dloc = narray * sumsiz + 4;
    moved_(&stlsum[(i__1 = p * 125 - 125) < 2500 && 0 <= i__1 ? i__1 : s_rnge(
	    "stlsum", i__1, "dafana_", (ftnlen)2024)], &sumsiz, &sumrec[(i__2 
	    = dloc - 1) < 128 && 0 <= i__2 ? i__2 : s_rnge("sumrec", i__2, 
	    "dafana_", (ftnlen)2024)]);
    namsiz = sumsiz << 3;
    cloc = narray * namsiz + 1;
    s_copy(namrec + (cloc - 1), stname + ((i__1 = p - 1) < 20 && 0 <= i__1 ? 
	    i__1 : s_rnge("stname", i__1, "dafana_", (ftnlen)2029)) * 1000, 
	    cloc + namsiz - 1 - (cloc - 1), (ftnlen)1000);
    sumrec[2] += 1.;
    narray = (integer) sumrec[2];

/*     Usually, adding an array does not fill the final summary */
/*     record, and it can simply be replaced. */

    if (narray < 125 / sumsiz) {
	dafwdr_(&stfh[(i__1 = p - 1) < 20 && 0 <= i__1 ? i__1 : s_rnge("stfh",
		 i__1, "dafana_", (ftnlen)2040)], &stlast[(i__2 = p - 1) < 20 
		&& 0 <= i__2 ? i__2 : s_rnge("stlast", i__2, "dafana_", (
		ftnlen)2040)], sumrec);
	i__3 = stlast[(i__2 = p - 1) < 20 && 0 <= i__2 ? i__2 : s_rnge("stla"
		"st", i__2, "dafana_", (ftnlen)2041)] + 1;
	dafwcr_(&stfh[(i__1 = p - 1) < 20 && 0 <= i__1 ? i__1 : s_rnge("stfh",
		 i__1, "dafana_", (ftnlen)2041)], &i__3, namrec, (ftnlen)1000)
		;

/*     When the record becomes full, a new one must be written. */
/*     However, this fact should be transparent to the user. */

    } else {

/*        The new summary record will be stored in the next free record */
/*        in the file. This summary record should point to it. */

/*        To find out which record the next free address is in, we use */
/*        DAFARW (`address to record and word'). */

	i__2 = stfree[(i__1 = p - 1) < 20 && 0 <= i__1 ? i__1 : s_rnge("stfr"
		"ee", i__1, "dafana_", (ftnlen)2056)] - 1;
	dafarw_(&i__2, &next, &word);
	++next;
	sumrec[0] = (doublereal) next;
	dafwdr_(&stfh[(i__1 = p - 1) < 20 && 0 <= i__1 ? i__1 : s_rnge("stfh",
		 i__1, "dafana_", (ftnlen)2060)], &stlast[(i__2 = p - 1) < 20 
		&& 0 <= i__2 ? i__2 : s_rnge("stlast", i__2, "dafana_", (
		ftnlen)2060)], sumrec);
	i__3 = stlast[(i__2 = p - 1) < 20 && 0 <= i__2 ? i__2 : s_rnge("stla"
		"st", i__2, "dafana_", (ftnlen)2061)] + 1;
	dafwcr_(&stfh[(i__1 = p - 1) < 20 && 0 <= i__1 ? i__1 : s_rnge("stfh",
		 i__1, "dafana_", (ftnlen)2061)], &i__3, namrec, (ftnlen)1000)
		;

/*        The new summary record should point backwards to the one just */
/*        written, and should point forwards to nothing. Of course, */
/*        it contains no summaries, and no names. */

	cleard_(&c__128, sumrec);
	sumrec[0] = 0.;
	sumrec[1] = (doublereal) stlast[(i__1 = p - 1) < 20 && 0 <= i__1 ? 
		i__1 : s_rnge("stlast", i__1, "dafana_", (ftnlen)2070)];
	sumrec[2] = 0.;
	s_copy(namrec, " ", (ftnlen)1000, (ftnlen)1);
	dafwdr_(&stfh[(i__1 = p - 1) < 20 && 0 <= i__1 ? i__1 : s_rnge("stfh",
		 i__1, "dafana_", (ftnlen)2074)], &next, sumrec);
	i__2 = next + 1;
	dafwcr_(&stfh[(i__1 = p - 1) < 20 && 0 <= i__1 ? i__1 : s_rnge("stfh",
		 i__1, "dafana_", (ftnlen)2075)], &i__2, namrec, (ftnlen)1000)
		;

/*        If a new summary record  was added, the first free address */
/*        lies just beyond the end of the matching character record. */

/*        We use DAFRWA (`record and word to address') to calculate */
/*        the next free address. */

	stlast[(i__1 = p - 1) < 20 && 0 <= i__1 ? i__1 : s_rnge("stlast", 
		i__1, "dafana_", (ftnlen)2084)] = next;
	i__3 = stlast[(i__1 = p - 1) < 20 && 0 <= i__1 ? i__1 : s_rnge("stla"
		"st", i__1, "dafana_", (ftnlen)2085)] + 2;
	dafrwa_(&i__3, &c__1, &stfree[(i__2 = p - 1) < 20 && 0 <= i__2 ? i__2 
		: s_rnge("stfree", i__2, "dafana_", (ftnlen)2085)]);
    }

/*     The new value STFREE(P) must be rewritten in the file record each */
/*     time a new array is added. If a new record was added, the new */
/*     value of STLAST(P) will be rewritten as well. */

    dafwfr_(&stfh[(i__1 = p - 1) < 20 && 0 <= i__1 ? i__1 : s_rnge("stfh", 
	    i__1, "dafana_", (ftnlen)2094)], &nd, &ni, stifnm + ((i__2 = p - 
	    1) < 20 && 0 <= i__2 ? i__2 : s_rnge("stifnm", i__2, "dafana_", (
	    ftnlen)2094)) * 60, &stfrst[(i__3 = p - 1) < 20 && 0 <= i__3 ? 
	    i__3 : s_rnge("stfrst", i__3, "dafana_", (ftnlen)2094)], &stlast[(
	    i__4 = p - 1) < 20 && 0 <= i__4 ? i__4 : s_rnge("stlast", i__4, 
	    "dafana_", (ftnlen)2094)], &stfree[(i__5 = p - 1) < 20 && 0 <= 
	    i__5 ? i__5 : s_rnge("stfree", i__5, "dafana_", (ftnlen)2094)], (
	    ftnlen)60);

/*     Ready for another array. */

    staddg[(i__1 = p - 1) < 20 && 0 <= i__1 ? i__1 : s_rnge("staddg", i__1, 
	    "dafana_", (ftnlen)2105)] = FALSE_;
    chkout_("DAFENA", (ftnlen)6);
    return 0;
/* $Procedure DAFCAD ( DAF, continue adding data ) */

L_dafcad:
/* $ Abstract */

/*     Select a DAF that already has a new array in progress as the */
/*     one to continue adding data to. */

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

/*     DAF */

/* $ Keywords */

/*     FILES */

/* $ Declarations */

/*     IMPLICIT NONE */

/*     INTEGER               HANDLE */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Handle of DAF to continue adding data to. */

/* $ Detailed_Input */

/*     HANDLE   is the handle of a DAF that is open for write */
/*              access and in which a new array has been */
/*              started by a call to DAFBNA. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the input handle is not that of a DAF that is open for */
/*         writing, an error is signaled by a routine in the call tree of */
/*         this routine. */

/*     2)  If no array is currently being added to in the file indicated */
/*         by HANDLE, the error SPICE(NOARRAYSTARTED) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     DAFCAD supports simultaneous addition of data to arrays in */
/*     multiple DAFs. In applications that use this capability, */
/*     DAFCAD should be called prior to each call to DAFADA or DAFENA */
/*     to specify which DAF is to be acted upon. */

/*     Here is a code fragment that adds a new array to each of N */
/*     existing DAFs, simultaneously. The data to be added to each */
/*     is broken up into M chunks; one chunk is written to each DAF */
/*     at a time. The data is contained in the array CHUNK, dimensioned */

/*         DOUBLE PRECISION      CHUNK ( MAXDAT, M, N ) */

/*     The actual amount of data in the Jth chunk for the Ith file is */
/*     given by */

/*         AMOUNT (J,I) */



/*         DO I = 1, N */
/*            CALL DAFOPW ( HANDLE(I) ) */
/*            CALL DAFBNA ( HANDLE(I) ) */
/*         END DO */

/*         DO J = 1, M */

/*            DO I = 1, N */
/*               CALL DAFCAD  ( HANDLE(I)                  ) */
/*               CALL DAFADA  ( CHUNK(1,J,I),  AMOUNT(J,I) ) */
/*            END DO */

/*         END DO */

/*         DO I = 1, N */
/*            CALL DAFCAD  ( HANDLE(I) ) */
/*            CALL DAFENA */
/*         END DO */


/*     Note that if we write all of the data for each array to just one */
/*     DAF at a time, we don't need to use DAFCAD: */

/*        DO I = 1, N */

/*           CALL DAFOPW ( HANDLE(I) ) */
/*           CALL DAFBNA ( HANDLE(I) ) */

/*           DO J = 1, M */
/*              CALL DAFADA ( CHUNK(1,J,I),  AMOUNT(J,I) ) */
/*           END DO */

/*           CALL DAFENA */

/*        END DO */

/* $ Examples */

/*     See $Examples in DAFANA. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */
/*     F.S. Turner        (JPL) */

/* $ Version */

/* -    SPICELIB Version 3.1.0, 06-JUL-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Edited entry #2 */
/*        in $Exceptions section: No write in progress is detected by */
/*        this routine. */

/* -    SPICELIB Version 3.0.0, 16-NOV-2001 (FST) */

/*        Updated entry points to support changes made to the DAF */
/*        system that utilize the new handle manager. See */
/*        the $Revisions section of DAFANA for a detailed */
/*        discussion of the changes. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 04-SEP-1991 (NJB) (WLT) */

/* -& */
/* $ Index_Entries */

/*     continue adding data to a DAF */
/*     select a DAF to continue adding data to */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("DAFCAD", (ftnlen)6);
    }

/*     Check out the file handle before going any further. */

    dafsih_(handle, "WRITE", (ftnlen)5);
    if (failed_()) {
	chkout_("DAFCAD", (ftnlen)6);
	return 0;
    }

/*     See whether we already have an entry for this DAF in the */
/*     state table.  Find the previous node if possible. */

    p = sthead;
    prev = -1;
    found = FALSE_;
    while(p != -1 && ! found) {
	if (stfh[(i__1 = p - 1) < 20 && 0 <= i__1 ? i__1 : s_rnge("stfh", 
		i__1, "dafana_", (ftnlen)2336)] == *handle) {
	    found = TRUE_;
	} else {
	    prev = p;
	    p = stpool[(i__1 = p - 1) < 20 && 0 <= i__1 ? i__1 : s_rnge("stp"
		    "ool", i__1, "dafana_", (ftnlen)2340)];
	}
    }

/*     Either FOUND is false, or P is the index in the state table of */
/*     the DAF specified by HANDLE, and PREV is the predecessor of P. */


/*     You can't continue writing to a DAF that you're not */
/*     already writing to. */

    if (! found) {
	dafhfn_(handle, dafnam, (ftnlen)255);
	setmsg_("No write in progress to #. (Handle was #.) ", (ftnlen)43);
	errch_("#", dafnam, (ftnlen)1, (ftnlen)255);
	errint_("#", handle, (ftnlen)1);
	sigerr_("SPICE(NOARRAYSTARTED)", (ftnlen)21);
	chkout_("DAFCAD", (ftnlen)6);
	return 0;
    } else if (! staddg[(i__1 = p - 1) < 20 && 0 <= i__1 ? i__1 : s_rnge(
	    "staddg", i__1, "dafana_", (ftnlen)2364)]) {
	dafhfn_(handle, dafnam, (ftnlen)255);
	setmsg_("No write in progress to #. (Handle was #.) ", (ftnlen)43);
	errch_("#", dafnam, (ftnlen)1, (ftnlen)255);
	errint_("#", handle, (ftnlen)1);
	sigerr_("SPICE(NOARRAYSTARTED)", (ftnlen)21);
	chkout_("DAFCAD", (ftnlen)6);
	return 0;
    }

/*     Move the node for this DAF to the head of the active list, */
/*     if it is not already there: */

/*        - Make the predecessor of P point to the successor of P. */

/*        - Make P point to the head of the active list. */

/*        - Make P the active list head node. */


    if (p != sthead) {

/*        P is in the active list, but is not at the head.  So, */
/*        the predecessor of P is not NIL. */

	stpool[(i__1 = prev - 1) < 20 && 0 <= i__1 ? i__1 : s_rnge("stpool", 
		i__1, "dafana_", (ftnlen)2393)] = stpool[(i__2 = p - 1) < 20 
		&& 0 <= i__2 ? i__2 : s_rnge("stpool", i__2, "dafana_", (
		ftnlen)2393)];
	stpool[(i__1 = p - 1) < 20 && 0 <= i__1 ? i__1 : s_rnge("stpool", 
		i__1, "dafana_", (ftnlen)2394)] = sthead;
	sthead = p;
    }
    chkout_("DAFCAD", (ftnlen)6);
    return 0;
} /* dafana_ */

/* Subroutine */ int dafana_(integer *handle, doublereal *sum, char *name__, 
	doublereal *data, integer *n, ftnlen name_len)
{
    return dafana_0_(0, handle, sum, name__, data, n, name_len);
    }

/* Subroutine */ int dafbna_(integer *handle, doublereal *sum, char *name__, 
	ftnlen name_len)
{
    return dafana_0_(1, handle, sum, name__, (doublereal *)0, (integer *)0, 
	    name_len);
    }

/* Subroutine */ int dafada_(doublereal *data, integer *n)
{
    return dafana_0_(2, (integer *)0, (doublereal *)0, (char *)0, data, n, (
	    ftnint)0);
    }

/* Subroutine */ int dafena_(void)
{
    return dafana_0_(3, (integer *)0, (doublereal *)0, (char *)0, (doublereal 
	    *)0, (integer *)0, (ftnint)0);
    }

/* Subroutine */ int dafcad_(integer *handle)
{
    return dafana_0_(4, handle, (doublereal *)0, (char *)0, (doublereal *)0, (
	    integer *)0, (ftnint)0);
    }

