/* getfat.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static logical c_false = FALSE_;
static integer c__1 = 1;

/* $Procedure GETFAT ( Get file architecture and type ) */
/* Subroutine */ int getfat_(char *file, char *arch, char *kertyp, ftnlen 
	file_len, ftnlen arch_len, ftnlen kertyp_len)
{
    /* System generated locals */
    cilist ci__1;
    olist o__1;
    cllist cl__1;
    inlist ioin__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen), f_inqu(inlist *), f_open(
	    olist *), s_rdue(cilist *), do_uio(integer *, char *, ftnlen), 
	    e_rdue(void), f_clos(cllist *), s_rsfe(cilist *), do_fio(integer *
	    , char *, ftnlen), e_rsfe(void);

    /* Local variables */
    extern /* Subroutine */ int zzddhfnh_(char *, integer *, logical *, 
	    ftnlen), zzddhgsd_(char *, integer *, char *, ftnlen, ftnlen), 
	    zzddhnfo_(integer *, char *, integer *, integer *, integer *, 
	    logical *, ftnlen), zzddhhlu_(integer *, char *, logical *, 
	    integer *, ftnlen);
    integer i__;
    char fname[255];
    extern /* Subroutine */ int chkin_(char *, ftnlen), ucase_(char *, char *,
	     ftnlen, ftnlen), errch_(char *, char *, ftnlen, ftnlen);
    logical found, exist;
    extern /* Subroutine */ int ljust_(char *, char *, ftnlen, ftnlen), 
	    idw2at_(char *, char *, char *, ftnlen, ftnlen, ftnlen);
    integer handle;
    extern /* Subroutine */ int dafcls_(integer *);
    char filarc[32];
    integer intbff;
    logical opened;
    extern /* Subroutine */ int dafopr_(char *, integer *, ftnlen);
    integer intarc;
    char idword[12];
    integer intamn, number;
    logical diropn;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), getlun_(integer *), setmsg_(char *, ftnlen);
    integer iostat;
    extern /* Subroutine */ int errint_(char *, integer *, ftnlen), nextwd_(
	    char *, char *, char *, ftnlen, ftnlen, ftnlen);
    char tmpwrd[12];
    extern logical return_(void);
    extern /* Subroutine */ int zzckspk_(integer *, char *, ftnlen);

    /* Fortran I/O blocks */
    static cilist io___14 = { 1, 0, 1, 0, 1 };


/* $ Abstract */

/*     Determine the architecture and type of SPICE kernels. */

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

/*     KERNEL */
/*     UTILITY */

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

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     FILE       I   The name of a file to be examined. */
/*     ARCH       O   The architecture of the kernel file. */
/*     KERTYP     O   The type of the kernel file. */

/* $ Detailed_Input */

/*     FILE     is the name of a SPICE kernel file whose architecture */
/*              and type are desired. */

/* $ Detailed_Output */

/*     ARCH     is the file architecture of the SPICE kernel file */
/*              specified by FILE. If the architecture cannot be */
/*              determined or is not recognized the value '?' is */
/*              returned. */

/*              Architectures currently recognized are: */

/*                 DAF -- The file is based on the DAF architecture. */
/*                 DAS -- The file is based on the DAS architecture. */
/*                 XFR -- The file is in a SPICE transfer file format. */
/*                 DEC -- The file is an old SPICE decimal text file. */
/*                 ASC -- An ASCII text file. */
/*                 KPL -- Kernel Pool File (i.e., a text kernel) */
/*                 TXT -- An ASCII text file. */
/*                 TE1 -- Text E-Kernel type 1. */
/*                 ?   -- The architecture could not be determined. */

/*              This variable must be at least 3 characters long. */

/*     KERTYP   is the type of the SPICE kernel file. If the type */
/*              can not be determined the value '?' is returned. */

/*              Kernel file types may be any sequence of at most four */
/*              printing characters. NAIF has reserved for its use */
/*              types which contain all upper case letters. */

/*              A file type of 'PRE' means that the file is a */
/*              pre-release file. */

/*              This variable may be at most 4 characters long. */

/* $ Parameters */

/*     RECL     is the record length of a binary kernel file. Each */
/*              record must be large enough to hold 128 double */
/*              precision numbers. The units in which the record */
/*              length must be specified vary from environment to */
/*              environment. For example, VAX Fortran requires */
/*              record lengths to be specified in longwords, */
/*              where two longwords equal one double precision */
/*              number. */

/* $ Exceptions */

/*     1)  If the filename specified is blank, the error */
/*         SPICE(BLANKFILENAME) is signaled. */

/*     2)  If any inquire on the filename specified by FILE, required to */
/*         obtain information about the physical file, fails for some */
/*         reason, the error SPICE(INQUIREERROR) is signaled. */

/*     3)  If the file specified by FILE does not exist, the error */
/*         SPICE(FILENOTFOUND) is signaled. */

/*     4)  If the file specified by FILE is already open but not through */
/*         SPICE interfaces, the error SPICE(EXTERNALOPEN) is signaled. */

/*     5)  If an attempt to open the file specified by FILE fails when */
/*         this routine requires that it succeed, the error */
/*         SPICE(FILEOPENFAILED) is signaled. */

/*     6)  If an attempt to read the file specified by FILE fails when */
/*         this routine requires that it succeed, the error */
/*         SPICE(FILEREADFAILED) is signaled. */

/*     7)  If an issue is detected during the opening the input file or */
/*         the process to determine its architecture and type, an error */
/*         is signaled by a routine in the call tree of this routine. */

/*     8)  If the ID word in a DAF based kernel is 'NAIF/DAF', then the */
/*         algorithm GETFAT uses to distinguish between CK and SPK */
/*         kernels may result in an indeterminate KERTYP if the SPK or */
/*         CK files have invalid first segments. */

/* $ Files */

/*     The SPICE kernel file specified by FILE is opened and then */
/*     closed by this routine to determine its file architecture and */
/*     type. Filenames of open files should not be passed to this */
/*     routine. */

/* $ Particulars */

/*     This subroutine is a support utility routine that determines the */
/*     architecture and type of a SPICE kernel file. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Determine the file architecture and file type of all the */
/*        kernels loaded through a meta-kernel, and of a kernel in */
/*        transfer format. */

/*        Use the SPK kernel below to provide an example of a kernel in */
/*        transfer format. */

/*           earthstns_itrf93_050714.xsp */


/*        Use the meta-kernel shown below to load the other types of */
/*        SPICE kernels. */


/*           KPL/MK */

/*           File: getfat_ex1.tm */

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
/*              mgs_moc_v20.ti                   MGS MOC instrument */
/*                                               parameters */
/*              mgs_sclkscet_00061.tsc           MGS SCLK coefficients */
/*              mgs_sc_ext12.bc                  MGS s/c bus attitude */
/*              mgs_ext12_ipng_mgs95j.bsp        MGS ephemeris */
/*              megr90n000cb_plate.bds           Plate model based on */
/*                                               MEGDR DEM, resolution */
/*                                               4 pixels/degree. */

/*           \begindata */

/*              KERNELS_TO_LOAD = ( 'de430.bsp', */
/*                                  'mar097.bsp', */
/*                                  'pck00010.tpc', */
/*                                  'naif0011.tls', */
/*                                  'mgs_moc_v20.ti', */
/*                                  'mgs_sclkscet_00061.tsc', */
/*                                  'mgs_sc_ext12.bc', */
/*                                  'mgs_ext12_ipng_mgs95j.bsp', */
/*                                  'megr90n000cb_plate.bds'      ) */
/*           \begintext */

/*           End of meta-kernel */


/*        Example code begins here. */


/*              PROGRAM GETFAT_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local parameters. */
/*        C */
/*              INTEGER               ARCHLN */
/*              PARAMETER           ( ARCHLN = 4 ) */

/*              INTEGER               FILSIZ */
/*              PARAMETER           ( FILSIZ = 256 ) */

/*              INTEGER               KTYPLN */
/*              PARAMETER           ( KTYPLN = 5 ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              CHARACTER*(ARCHLN)    ARCH */
/*              CHARACTER*(FILSIZ)    FNAME */
/*              CHARACTER*(28)        FNAME1 */
/*              CHARACTER*(KTYPLN)    KTYPE */
/*              CHARACTER*(FILSIZ)    SOURCE */

/*              INTEGER               COUNT */
/*              INTEGER               HANDLE */
/*              INTEGER               I */

/*              LOGICAL               FOUND */

/*        C */
/*        C     Check the file architecture and type of an SPK */
/*        C     in transfer format. */
/*        C */
/*              FNAME1 = 'earthstns_itrf93_050714.xsp' */

/*              CALL GETFAT ( FNAME1, ARCH, KTYPE ) */

/*              WRITE(*,*) 'File name     : ', FNAME1 */
/*              WRITE(*,*) '  Architecture: ', ARCH */
/*              WRITE(*,*) '  Kernel type : ', KTYPE */
/*              WRITE(*,*) ' ' */

/*        C */
/*        C     Load the kernels. */
/*        C */
/*              CALL FURNSH ( 'getfat_ex1.tm' ) */

/*        C */
/*        C     Get the file architecture and kernel type for each of */
/*        C     the kernels in the kernel pool. */
/*        C */
/*              CALL KTOTAL ( 'ALL', COUNT ) */

/*              DO I= 1, COUNT */

/*                 CALL KDATA ( I,     'ALL', FNAME, KTYPE, SOURCE, */
/*             .                HANDLE, FOUND                      ) */

/*                 CALL GETFAT ( FNAME, ARCH, KTYPE ) */

/*                 WRITE(*,*) 'File name     : ', FNAME */
/*                 WRITE(*,*) '  Source      : ', SOURCE */
/*                 WRITE(*,*) '  Architecture: ', ARCH */
/*                 WRITE(*,*) '  Kernel type : ', KTYPE */
/*                 WRITE(*,*) ' ' */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         File name     : earthstns_itrf93_050714.xsp */
/*           Architecture: XFR */
/*           Kernel type : DAF */

/*         File name     : getfat_ex1.tm */
/*           Source      : */
/*           Architecture: KPL */
/*           Kernel type : MK */

/*         File name     : de430.bsp */
/*           Source      : getfat_ex1.tm */
/*           Architecture: DAF */
/*           Kernel type : SPK */

/*         File name     : mar097.bsp */
/*           Source      : getfat_ex1.tm */
/*           Architecture: DAF */
/*           Kernel type : SPK */

/*         File name     : pck00010.tpc */
/*           Source      : getfat_ex1.tm */
/*           Architecture: KPL */
/*           Kernel type : PCK */

/*         File name     : naif0011.tls */
/*           Source      : getfat_ex1.tm */
/*           Architecture: KPL */
/*           Kernel type : LSK */

/*         File name     : mgs_moc_v20.ti */
/*           Source      : getfat_ex1.tm */
/*           Architecture: KPL */
/*           Kernel type : IK */

/*         File name     : mgs_sclkscet_00061.tsc */
/*           Source      : getfat_ex1.tm */
/*           Architecture: KPL */
/*           Kernel type : SCLK */

/*         File name     : mgs_sc_ext12.bc */
/*           Source      : getfat_ex1.tm */
/*           Architecture: DAF */
/*           Kernel type : CK */

/*         File name     : mgs_ext12_ipng_mgs95j.bsp */
/*           Source      : getfat_ex1.tm */
/*           Architecture: DAF */
/*           Kernel type : SPK */

/*         File name     : megr90n000cb_plate.bds */
/*           Source      : getfat_ex1.tm */
/*           Architecture: DAS */
/*           Kernel type : DSK */


/* $ Restrictions */

/*     1)  In order to properly determine the type of DAF based binary */
/*         kernels, the routine requires that their first segments and */
/*         the meta data necessary to address them are valid. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     H.A. Neilan        (JPL) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */
/*     F.S. Turner        (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 5.1.0, 28-NOV-2021 (BVS) */

/*        Updated for MAC-OSX-M1-64BIT-CLANG_C. */

/* -    SPICELIB Version 5.0.1, 07-AUG-2020 (JDR) */

/*        Edited the header to comply with NAIF standard. Added complete */
/*        code example to $Examples section. */

/* -    SPICELIB Version 5.0.0, 05-FEB-2015 (NJB) */

/*        Updated to support integration of DAS into the */
/*        handle manager subsystem. Now opened DAS files */
/*        must be known to that subsystem; if this routine */
/*        encounters an open, unrecognized DAS file, an */
/*        error is signaled. */

/*        Corrected various typos in comments. */

/* -    SPICELIB Version 4.25.0, 10-MAR-2014 (BVS) */

/*        Updated for SUN-SOLARIS-64BIT-INTEL. */

/* -    SPICELIB Version 4.24.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-LINUX-64BIT-IFORT. */

/* -    SPICELIB Version 4.23.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-CYGWIN-GFORTRAN. */

/* -    SPICELIB Version 4.22.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-CYGWIN-64BIT-GFORTRAN. */

/* -    SPICELIB Version 4.21.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-CYGWIN-64BIT-GCC_C. */

/* -    SPICELIB Version 4.20.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-INTEL. */

/* -    SPICELIB Version 4.19.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-INTEL-CC_C. */

/* -    SPICELIB Version 4.18.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C. */

/* -    SPICELIB Version 4.17.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-64BIT-NATIVE_C. */

/* -    SPICELIB Version 4.16.0, 13-MAY-2010 (BVS) */

/*        Updated for PC-WINDOWS-64BIT-IFORT. */

/* -    SPICELIB Version 4.15.0, 13-MAY-2010 (BVS) */

/*        Updated for PC-LINUX-64BIT-GFORTRAN. */

/* -    SPICELIB Version 4.14.0, 13-MAY-2010 (BVS) */

/*        Updated for PC-64BIT-MS_C. */

/* -    SPICELIB Version 4.13.0, 13-MAY-2010 (BVS) */

/*        Updated for MAC-OSX-64BIT-INTEL_C. */

/* -    SPICELIB Version 4.12.0, 13-MAY-2010 (BVS) */

/*        Updated for MAC-OSX-64BIT-IFORT. */

/* -    SPICELIB Version 4.11.0, 13-MAY-2010 (BVS) */

/*        Updated for MAC-OSX-64BIT-GFORTRAN. */

/* -    SPICELIB Version 4.10.0, 18-MAR-2009 (BVS) */

/*        Updated for PC-LINUX-GFORTRAN. */

/* -    SPICELIB Version 4.9.0, 18-MAR-2009 (BVS) */

/*        Updated for MAC-OSX-GFORTRAN. */

/* -    SPICELIB Version 4.8.0, 19-FEB-2008 (BVS) */

/*        Updated for PC-LINUX-IFORT. */

/* -    SPICELIB Version 4.7.0, 14-NOV-2006 (BVS) */

/*        Updated for PC-LINUX-64BIT-GCC_C. */

/* -    SPICELIB Version 4.6.0, 14-NOV-2006 (BVS) */

/*        Updated for MAC-OSX-INTEL_C. */

/* -    SPICELIB Version 4.5.0, 14-NOV-2006 (BVS) */

/*        Updated for MAC-OSX-IFORT. */

/* -    SPICELIB Version 4.4.0, 14-NOV-2006 (BVS) */

/*        Updated for PC-WINDOWS-IFORT. */

/* -    SPICELIB Version 4.3.0, 26-OCT-2005 (BVS) */

/*        Updated for SUN-SOLARIS-64BIT-GCC_C. */

/* -    SPICELIB Version 4.2.0, 03-JAN-2005 (BVS) */

/*        Updated for PC-CYGWIN_C. */

/* -    SPICELIB Version 4.1.0, 03-JAN-2005 (BVS) */

/*        Updated for PC-CYGWIN. */

/* -    SPICELIB Version 4.0.2, 24-APR-2003 (EDW) */

/*        Added MAC-OSX-F77 to the list of platforms */
/*        that require READONLY to read write protected */
/*        kernels. */

/* -    SPICELIB Version 4.0.1, 17-JUL-2002 (BVS) */

/*        Added MAC-OSX environments. */

/* -    SPICELIB Version 4.0.0, 22-AUG-2001 (WLT) (FST) (EDW) */

/*        Added code so that the architecture and type of open binary */
/*        SPICE kernels can be determined. */

/*        Added exception for MACPPC_C (CodeWarrior Mac classic). */
/*        Reduced RECL value to 12 to prevent expression of */
/*        the fseek bug. */

/* -    SPICELIB Version 3.2.0, 06-DEC-1999 (WLT) */

/*        The heuristics for distinguishing between CK and SPK have */
/*        been enhanced so that the routine is no longer requires */
/*        that TICKS in C-kernels be positive or integral. */

/* -    SPICELIB Version 3.1.4, 08-OCT-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given. New */
/*        environments are WIN-NT */

/* -    SPICELIB Version 3.1.3, 22-SEP-1999 (NJB) */

/*        CSPICE environments were added. Some typos were corrected. */

/* -    SPICELIB Version 3.1.2, 28-JUL-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given. New */
/*        environments are PC-DIGITAL, SGI-O32 and SGI-N32. */

/* -    SPICELIB Version 3.1.1, 18-MAR-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given. Previously, */
/*        environments such as SUN-SUNOS and SUN-SOLARIS were implied */
/*        by the environment label SUN. */

/* -    SPICELIB Version 3.1.0, 11-FEB-1999 (FST) */

/*        Added an integrality check to Test 3. If LASTDP is not */
/*        an integral value, then GETFAT simply returns KERTYP = '?', */
/*        since it is of an indeterminate type. */

/* -    SPICELIB Version 3.0.0, 07-APR-1998 (NJB) */

/*        Module was updated for the PC-LINUX platform. */

/* -    SPICELIB Version 2.0.0, 19-DEC-1995 (KRG) */

/*        Added several new features to the subroutine: */

/*         - Error handling has been enhanced. */
/*         - Several new file architectures have been added. */

/*        Removed the mention of 1000 characters as a candidate for the */
/*        record length of a file. */

/*        Added the exception for a blank filename to the header. The */
/*        error is signaled, but it was not listed in the header. */

/*        Added IOSTAT values to the appropriate error messages. */

/*        Non-printing characters are replaced with blanks in the ID */
/*        word when it is read. This deals with the case where a */
/*        platform allows a text file to be opened as an unformatted */
/*        file and the ID word does not completely fill 8 characters. */

/* -    SPICELIB Version 1.4.0, 05-JAN-1995 (HAN) */

/*        Removed ENV11 since it is now the same as ENV2. */
/*        Removed ENV10 since it is the same as the VAX environment. */

/* -    SPICELIB Version 1.3.0, 30-AUG-1994 (HAN) */

/*        Added two new environments, DEC Alpha/OpenVMS and */
/*        Sun/Solaris, to the source master file. */

/* -    SPICELIB Version 1.2.0, 25-MAR-1994 (HAN) */

/*        Added two new environments, DEC Alpha/OpenVMS and */
/*        Sun/Solaris, to the source master file. */

/* -    SPICELIB Version 1.1.0, 25-MAR-1994 (HAN) */

/*        Modified master source code file to use READONLY on platforms */
/*        that support it. Also, changed some local declaration comment */
/*        lines to match the standard NAIF template. */

/* -    SPICELIB Version 1.0.0, 24-JUL-1993 (WLT) (HAN) (KRG) */

/* -& */
/* $ Index_Entries */

/*     determine the architecture and type of a kernel file */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 4.0.0, 22-AUG-2001 (WLT) (FST) (EDW) */

/*        Added code so that the architecture and type of open binary */
/*        SPICE kernels can be determined. This uses the new DAF/DAS */
/*        handle manager as well as examination of handles of open DAS */
/*        files. Currently the handle manager deals only with DAF */
/*        files. This routine should be updated again when the DAS */
/*        system is integrated with the handle manager. */

/*        Some slight changes were required to support ZZDDHFNH on */
/*        the VAX environment. This resulted in the addition of */
/*        the logical USEFNH that is set to true in most */
/*        environments, and never used again other than to allow */
/*        the invocation of the ZZDDHFNH module. */

/* -    SPICELIB Version 2.0.0, 19-DEC-1995 (KRG) */

/*        Added several new features to the subroutine: */

/*         - Error handling has been enhanced. */
/*         - Several new file architectures have been added. */

/*        Removed the mention of 1000 characters as a candidate for the */
/*        record length of a file. It seems unlikely that we will */
/*        encounter an environment where 1000 characters of storage is */
/*        larger than the storage necessary for 128 double precision */
/*        numbers; typically there are 8 characters per double precision */
/*        number, yielding 1024 characters. */

/*        Added the exception for a blank filename to the header. The */
/*        error is signaled, but it was not listed in the header. */

/*        Added IOSTAT values to the appropriate error messages. */

/*        Non-printing characters are replaced with blanks in the ID */
/*        word when it is read. This deals with the case where a */
/*        platform allows a text file to be opened as an unformatted */
/*        file and the ID word does not completely fill 8 characters. */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Set the length of a SPICE kernel file ID word. */


/*     Set minimum and maximum values for the range of ASCII printing */
/*     characters. */


/*     Local Variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("GETFAT", (ftnlen)6);
    }

/*     Initialize the temporary storage variables that we use. */

    s_copy(idword, " ", (ftnlen)12, (ftnlen)1);

/*     If the filename we have is blank, signal an error and return. */

    if (s_cmp(file, " ", file_len, (ftnlen)1) == 0) {
	setmsg_("The file name is blank.", (ftnlen)23);
	sigerr_("SPICE(BLANKFILENAME)", (ftnlen)20);
	chkout_("GETFAT", (ftnlen)6);
	return 0;
    }

/*     See if this is a binary file that is currently open */
/*     within the SPICE binary file management subsystem.  At */
/*     the moment, as far as we know, the file is not opened. */

    opened = FALSE_;
    zzddhfnh_(file, &handle, &found, file_len);
    if (found) {

/*        If the file was recognized, we need to get the unit number */
/*        associated with it. */

	zzddhnfo_(&handle, fname, &intarc, &intbff, &intamn, &found, (ftnlen)
		255);

/*        Translate the architecture ID to a string and retrieve the */
/*        logical unit to use with this file. */

	zzddhgsd_("ARCH", &intarc, filarc, (ftnlen)4, (ftnlen)32);
	zzddhhlu_(&handle, filarc, &c_false, &number, (ftnlen)32);
	opened = TRUE_;
    } else {

/*        We'll do a bit of inquiring before we try opening anything. */

	ioin__1.inerr = 1;
	ioin__1.infilen = file_len;
	ioin__1.infile = file;
	ioin__1.inex = &exist;
	ioin__1.inopen = &opened;
	ioin__1.innum = 0;
	ioin__1.innamed = 0;
	ioin__1.inname = 0;
	ioin__1.inacc = 0;
	ioin__1.inseq = 0;
	ioin__1.indir = 0;
	ioin__1.infmt = 0;
	ioin__1.inform = 0;
	ioin__1.inunf = 0;
	ioin__1.inrecl = 0;
	ioin__1.innrec = 0;
	ioin__1.inblank = 0;
	iostat = f_inqu(&ioin__1);

/*        Not too likely, but if the INQUIRE statement fails... */

	if (iostat != 0) {
	    setmsg_("IOSTAT error in INQUIRE statement. IOSTAT = #.", (ftnlen)
		    46);
	    errint_("#", &iostat, (ftnlen)1);
	    sigerr_("SPICE(INQUIREERROR)", (ftnlen)19);
	    chkout_("GETFAT", (ftnlen)6);
	    return 0;
	}

/*        Note: the following two tests MUST be performed in the order */
/*        in which they appear, since in some environments files that do */
/*        not exist are considered to be open. */

	if (! exist) {
	    setmsg_("The kernel file '#' does not exist.", (ftnlen)35);
	    errch_("#", file, (ftnlen)1, file_len);
	    sigerr_("SPICE(FILENOTFOUND)", (ftnlen)19);
	    chkout_("GETFAT", (ftnlen)6);
	    return 0;
	}

/*        Reject open files not known to the handle manager subsystem. */

	if (opened) {

/*           Open files that are not opened within the SPICE */
/*           binary file management subsystem are forbidden fruit. */
/*           All we can do is signal an error letting the caller */
/*           know that we are helpless in this case. */

	    setmsg_("The file '#' is already open.", (ftnlen)29);
	    errch_("#", file, (ftnlen)1, file_len);
	    sigerr_("SPICE(EXTERNALOPEN)", (ftnlen)19);
	    chkout_("GETFAT", (ftnlen)6);
	    return 0;
	}
    }

/*     Open the file with a record length of RECL (the length of the */
/*     DAF and DAS records). We assume, for now, that opening the file as */
/*     a direct access file will work. */

    diropn = TRUE_;

/*     If the file is not already open (probably the case that */
/*     happens most frequently) we try opening it for direct access */
/*     and see if we can locate the idword. */

    if (! opened) {
	getlun_(&number);
	o__1.oerr = 1;
	o__1.ounit = number;
	o__1.ofnmlen = file_len;
	o__1.ofnm = file;
	o__1.orl = 1024;
	o__1.osta = "OLD";
	o__1.oacc = "DIRECT";
	o__1.ofm = 0;
	o__1.oblnk = 0;
	iostat = f_open(&o__1);

/*     If we had trouble opening the file, try opening it as a */
/*     sequential file. */

	if (iostat != 0) {
	    diropn = FALSE_;
	    o__1.oerr = 1;
	    o__1.ounit = number;
	    o__1.ofnmlen = file_len;
	    o__1.ofnm = file;
	    o__1.orl = 0;
	    o__1.osta = "OLD";
	    o__1.oacc = "SEQUENTIAL";
	    o__1.ofm = 0;
	    o__1.oblnk = 0;
	    iostat = f_open(&o__1);

/*        If we still have problems opening the file, we don't have a */
/*        clue about the file architecture and type. */

	    if (iostat != 0) {
		s_copy(arch, "?", arch_len, (ftnlen)1);
		s_copy(kertyp, "?", kertyp_len, (ftnlen)1);
		setmsg_("Attempt to open the file '#' failed. IOSTAT = #.", (
			ftnlen)48);
		errch_("#", file, (ftnlen)1, file_len);
		errint_("#", &iostat, (ftnlen)1);
		sigerr_("SPICE(FILEOPENFAILED)", (ftnlen)21);
		chkout_("GETFAT", (ftnlen)6);
		return 0;
	    }
	}
    }

/*     We opened the file successfully, so let's try to read from the */
/*     file. We need to be sure to use the correct form of the read */
/*     statement, depending on whether the file was opened with direct */
/*     access or sequential access. */

    if (diropn) {
	io___14.ciunit = number;
	iostat = s_rdue(&io___14);
	if (iostat != 0) {
	    goto L100001;
	}
	iostat = do_uio(&c__1, tmpwrd, (ftnlen)12);
	if (iostat != 0) {
	    goto L100001;
	}
	iostat = e_rdue();
L100001:

/*        If we couldn't read from the file as a direct access file with */
/*        a fixed record length, then try to open the file as a */
/*        sequential file and read from it. */

	if (iostat != 0) {
	    if (opened) {

/*              Something has gone wrong here.  The file was opened */
/*              as either a DAF or DAS prior to the call to GETFAT. */
/*              We retrieved the unit number maintained by the */
/*              underlying binary file management system, but we */
/*              were unable to read the file as direct access. */
/*              There's nothing we can do but abandon our quest to */
/*              determine the type of the file. */

		setmsg_("The file '#' is opened as a binary SPICE kernel.  B"
			"ut it cannot be read using a direct access read. The"
			" value of IOSTAT returned by the attempted READ is #"
			". ", (ftnlen)157);
		errch_("#", file, (ftnlen)1, file_len);
		errint_("#", &iostat, (ftnlen)1);
		sigerr_("SPICE(FILEREADFAILED)", (ftnlen)21);
		chkout_("GETFAT", (ftnlen)6);
		return 0;
	    }

/*           If we reach this point, the file was opened locally */
/*           as a direct access file.  We could not read it that */
/*           way, so we'll try using a sequential read.   However, */
/*           we first need to close the file and then reopen it */
/*           for sequential reading. */

	    cl__1.cerr = 0;
	    cl__1.cunit = number;
	    cl__1.csta = 0;
	    f_clos(&cl__1);
	    o__1.oerr = 1;
	    o__1.ounit = number;
	    o__1.ofnmlen = file_len;
	    o__1.ofnm = file;
	    o__1.orl = 0;
	    o__1.osta = "OLD";
	    o__1.oacc = "SEQUENTIAL";
	    o__1.ofm = 0;
	    o__1.oblnk = 0;
	    iostat = f_open(&o__1);

/*           If we could not open the file, we don't have a clue about */
/*           the file architecture and type. */

	    if (iostat != 0) {
		s_copy(arch, "?", arch_len, (ftnlen)1);
		s_copy(kertyp, "?", kertyp_len, (ftnlen)1);
		setmsg_("Attempt to open the file '#' failed. IOSTAT = #.", (
			ftnlen)48);
		errch_("#", file, (ftnlen)1, file_len);
		errint_("#", &iostat, (ftnlen)1);
		sigerr_("SPICE(FILEOPENFAILED)", (ftnlen)21);
		chkout_("GETFAT", (ftnlen)6);
		return 0;
	    }

/*           Try to read from the file. */

	    ci__1.cierr = 1;
	    ci__1.ciend = 1;
	    ci__1.ciunit = number;
	    ci__1.cifmt = "(A)";
	    iostat = s_rsfe(&ci__1);
	    if (iostat != 0) {
		goto L100002;
	    }
	    iostat = do_fio(&c__1, tmpwrd, (ftnlen)12);
	    if (iostat != 0) {
		goto L100002;
	    }
	    iostat = e_rsfe();
L100002:
	    ;
	}
    } else {
	ci__1.cierr = 1;
	ci__1.ciend = 1;
	ci__1.ciunit = number;
	ci__1.cifmt = "(A)";
	iostat = s_rsfe(&ci__1);
	if (iostat != 0) {
	    goto L100003;
	}
	iostat = do_fio(&c__1, tmpwrd, (ftnlen)12);
	if (iostat != 0) {
	    goto L100003;
	}
	iostat = e_rsfe();
L100003:
	;
    }

/*     If we had an error while reading, we don't recognize this file. */

    if (iostat != 0) {
	s_copy(arch, "?", arch_len, (ftnlen)1);
	s_copy(kertyp, "?", kertyp_len, (ftnlen)1);
	cl__1.cerr = 0;
	cl__1.cunit = number;
	cl__1.csta = 0;
	f_clos(&cl__1);
	setmsg_("Attempt to read from file '#' failed. IOSTAT = #.", (ftnlen)
		49);
	errch_("#", file, (ftnlen)1, file_len);
	errint_("#", &iostat, (ftnlen)1);
	sigerr_("SPICE(FILEREADFAILED)", (ftnlen)21);
	chkout_("GETFAT", (ftnlen)6);
	return 0;
    }

/*     Close the file (if we opened it here), as we do not need it */
/*     to be open any more. */

    if (! opened) {
	cl__1.cerr = 0;
	cl__1.cunit = number;
	cl__1.csta = 0;
	f_clos(&cl__1);
    }

/*     At this point, we have a candidate for an ID word. To avoid */
/*     difficulties with Fortran I/O and other things, we will now */
/*     replace any non printing ASCII characters with blanks. */

    for (i__ = 1; i__ <= 12; ++i__) {
	if (*(unsigned char *)&tmpwrd[i__ - 1] < 32 || *(unsigned char *)&
		tmpwrd[i__ - 1] > 126) {
	    *(unsigned char *)&tmpwrd[i__ - 1] = ' ';
	}
    }

/*     Identify the architecture and type, if we can. */

    ljust_(tmpwrd, tmpwrd, (ftnlen)12, (ftnlen)12);
    ucase_(tmpwrd, tmpwrd, (ftnlen)12, (ftnlen)12);
    nextwd_(tmpwrd, idword, tmpwrd, (ftnlen)12, (ftnlen)12, (ftnlen)12);
    if (s_cmp(idword, "DAFETF", (ftnlen)12, (ftnlen)6) == 0) {

/*        We have a DAF encoded transfer file. */

	s_copy(arch, "XFR", arch_len, (ftnlen)3);
	s_copy(kertyp, "DAF", kertyp_len, (ftnlen)3);
    } else if (s_cmp(idword, "DASETF", (ftnlen)12, (ftnlen)6) == 0) {

/*        We have a DAS encoded transfer file. */

	s_copy(arch, "XFR", arch_len, (ftnlen)3);
	s_copy(kertyp, "DAS", kertyp_len, (ftnlen)3);
    } else if (s_cmp(idword, "'NAIF/DAF'", (ftnlen)10, (ftnlen)10) == 0) {

/*        We have an old DAF decimal text file. */

	s_copy(arch, "DEC", arch_len, (ftnlen)3);
	s_copy(kertyp, "DAF", kertyp_len, (ftnlen)3);
    } else if (s_cmp(idword, "NAIF/DAS", (ftnlen)8, (ftnlen)8) == 0) {

/*        We have a pre release DAS binary file. */

	s_copy(arch, "DAS", arch_len, (ftnlen)3);
	s_copy(kertyp, "PRE", kertyp_len, (ftnlen)3);
    } else {

/*        Get the architecture and type from the ID word, if we can. */

	idw2at_(idword, arch, kertyp, (ftnlen)8, arch_len, kertyp_len);
    }

/*     If the architecture is DAF and the type is unknown, '?', then we */
/*     have either an SPK file, a CK file, or something we don't */
/*     understand. Let's check it out. */

    if (s_cmp(arch, "DAF", arch_len, (ftnlen)3) == 0 && s_cmp(kertyp, "?", 
	    kertyp_len, (ftnlen)1) == 0) {

/*        We have a DAF file and we do not know what the type is. This */
/*        situation can occur for older SPK and CK files, before the ID */
/*        word was used to store type information. */

/*        We use Bill's (WLT'S) magic heuristics to determine the type */
/*        of the file. */

/*        Open the file and pass the handle to the private routine */
/*        that deals with the dirty work. */

	dafopr_(file, &handle, file_len);
	zzckspk_(&handle, kertyp, kertyp_len);
	dafcls_(&handle);
    }
    chkout_("GETFAT", (ftnlen)6);
    return 0;
} /* getfat_ */

