/* zzddhivf.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ZZDDHIVF ( Private --- DDH Identify VAX DAF File Format ) */
/* Subroutine */ int zzddhivf_(char *nsum, integer *bff, logical *found, 
	ftnlen nsum_len)
{
    /* System generated locals */
    char ch__1[1];

    /* Local variables */
    integer leader, trailr;

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Identify VAX DAF file format. */

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

/*     PRIVATE */

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
/*     NSUM       I   String storing the 8 bytes of the FDREC NSUM DP. */
/*     BFF        O   Integer code indicating the binary file format. */
/*     FOUND      O   Logical indicating that BFF was determined. */

/* $ Detailed_Input */

/*     NSUM       is a string whose first 8 bytes contain NSUM (the third */
/*                double precision number) from the first descriptor */
/*                record of a non-empty DAF suspected to be in one of */
/*                the VAX binary file formats. */

/* $ Detailed_Output */

/*     BFF        is an integer that signals whether NSUM indicates the */
/*                DAF is VAX-DFLT or VAX-GFLT.  Possible values are: */

/*                   VAXGFL */
/*                   VAXDFL */

/*                as defined in the include file 'zzddhman.inc'.  See it */
/*                for details. */

/*     FOUND      is a logical that indicates whether the ZZDDHVFF check */
/*                was successful.  If TRUE, BFF contains the code for */
/*                VAX-DFLT or VAX-GFLT binary file format.  If FALSE, */
/*                then BFF is untouched. */

/* $ Parameters */

/*     See the include file 'zzddhman.inc'. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     This routine examines a series of bytes from a potential pre-N0050 */
/*     DAF to determine its architecture, but does not access the file */
/*     itself. */

/* $ Particulars */

/*     This routine examines the bit patterns stored in NSUM to determine */
/*     which of the two VAX binary file formats are used to store the */
/*     double precision values in the DAF file. */

/* $ Examples */

/*     See ZZDDHPPF for sample usage. */

/* $ Restrictions */

/*     1) The first 8 bytes of NSUM must contain the third double */
/*        precision value from the first descriptor record in a DAF */
/*        file not in BIG-IEEE binary file format. */

/*     2) The DAF file from which NSUM is extracted must be correct */
/*        or this routine may produce incorrect results. */

/*     3) Assumes CHARACTER*(1) is byte sized. */

/*     4) Assumes that ICHAR(CHAR(CHARACTER)) yields an integer with */
/*        the same bit pattern as the source character. */

/* $ Literature_References */

/*     1) Binary File Formats and Code Relying on Function Not Addressed */
/*        by the ANSI '77 Fortran Standard. */

/* $ Author_and_Institution */

/*     F.S. Turner     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.26.0, 28-NOV-2021 (BVS) */

/*        Updated for MAC-OSX-M1-64BIT-CLANG_C. */

/* -    SPICELIB Version 1.25.1, 02-OCT-2021 (NJB) */

/*        Corrected typo in comments. */

/* -    SPICELIB Version 1.25.0, 10-MAR-2014 (BVS) */

/*        Updated for SUN-SOLARIS-64BIT-INTEL. */

/* -    SPICELIB Version 1.24.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-LINUX-64BIT-IFORT. */

/* -    SPICELIB Version 1.23.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-CYGWIN-GFORTRAN. */

/* -    SPICELIB Version 1.22.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-CYGWIN-64BIT-GFORTRAN. */

/* -    SPICELIB Version 1.21.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-CYGWIN-64BIT-GCC_C. */

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

/* -    SPICELIB Version 1.0.1, 17-JUL-2002 (BVS) */

/*        Added MAC-OSX environments. */

/* -    SPICELIB Version 1.0.0, 04-OCT-2001 (FST) */


/* -& */

/*     Local Variables */


/*     Statement Functions */


/*     Statement Function Definitions */

/*     This function controls the conversion of characters to integers. */
/*     On some supported environments, ICHAR is not sufficient to */
/*     produce the desired results.  This however, is not the case */
/*     with this particular environment. */


/*     Before diving right into the code that examines the bit patterns */
/*     stored in NSUM, review exactly what checks require completion and */
/*     why they function. */

/*     When this module is invoked, we already know that the DAF from */
/*     which NSUM was extracted is little endian, and that it is not */
/*     a LTL-IEEE file.  This leaves us with one of 3 sources for */
/*     NSUM: */

/*       (a) A VAX D-Floating file */
/*       (b) A VAX G-Floating file */
/*       (c) A damaged file */

/*     In the case of (c) the algorithm outlined below is not guaranteed */
/*     to produce correct results.  If the case is either (a) or (b), */
/*     then the routine will correctly determine the source binary file */
/*     format.  Here's why: */

/*        NSUM is the third double precision number from the first */
/*        descriptor record of a non-empty DAF file.  This number is */
/*        an integral valued DP bounded between 1 and 125 inclusive. */

/*        An examination of a binary file created with the following */
/*        code fragment: */

/*           INCLUDE              'zzddhman.inc' */

/*           DOUBLE PRECISION      DPDATA ( 125 ) */
/*           INTEGER               I */
/*           INTEGER               LUN */
/*             . */
/*             . */
/*             . */
/*           CALL GETLUN( LUN ) */

/*           DO I = 1, 125 */
/*              DPDATA (I) = DBLE (I) */
/*           END DO */

/*           OPEN ( UNIT   = LUN, */
/*          .       FILE   = FNAME, */
/*          .       STATUS = 'NEW', */
/*          .       ACCESS = 'DIRECT', */
/*          .       RECL   = RECL      ) */

/*           WRITE ( UNIT = LUN, REC = 1 ) ( DPDATA(I), I = 1, 125 ) */

/*           END */

/*        This source file was compiled on a VMS VAX system both with */
/*        G-Floating and D-Floating options, and executed to produce */
/*        the binary file of interest.  The bit patterns for each of */
/*        the 125 entries were compared using the UNIX command 'od'. */

/*        This comparison yielded the fact that these two sets of 125 */
/*        bit patterns did not intersect, and all that remained was to */
/*        uncover an efficient means of identifying which set a */
/*        particular member belonged to. */

/*        The following was observed: */

/*           With the exception of the first entry representing the */
/*           number 1.0D0 in the D-Floating case, all entries */
/*           appeared as: (hexadecimal byte dump from 'od' output) */

/*              0041 0000 0000 0000 */
/*              4041 0000 0000 0000 */
/*              8041 0000 0000 0000 */
/*                       . */
/*                       . */
/*                       . */
/*              f643 0000 0000 0000 */
/*              f843 0000 0000 0000 */
/*              fa43 0000 0000 0000 */

/*           While the G-Floating case: */

/*              1040 0000 0000 0000 */
/*              2040 0000 0000 0000 */
/*              2840 0000 0000 0000 */
/*                       . */
/*                       . */
/*                       . */
/*              7e40 00c0 0000 0000 */
/*              7f40 0000 0000 0000 */
/*              7f40 0040 0000 0000 */

/*           The important thing to note is that the fourth entry in */
/*           G-Floating bit patterns is always '0', and in the */
/*           D-Floating case (with the exception of the first entry) */
/*           is always non-zero.  The first entry in the D-Floating */
/*           table is: */

/*              8040 0000 0000 0000 */

/*           It also happens to be the case that the leading value */
/*           of all G-Floating cases are numbers less than 8. */
/*           Constructing a series of tests around these observations */
/*           will produce correct results.  When the input file meets */
/*           the restrictions non-empty and correct. */

/*     So now all that remains is to lay out the specifics of the test. */
/*     First extract the leading 4 bits from NSUM(1:1) and the trailing */
/*     four bits from NSUM(2:2).  Then enter this IF/ELSE IF block: */

/*        If the value of the leading 4 bits from NSUM(1:1) is 8 and */
/*        the trailing 4 bits from NSUM(2:2) are 0, then the file is */
/*        of the D-Floating binary format. */

/*        Else if the value of the trailing 4 bits of NSUM(2:2) is */
/*        non-zero, then the file is also of the D-Floating binary */
/*        format. */

/*        Else if the value of the leading 4 bits of NSUM(1:1) is */
/*        strictly less than 8 and the trailing bits of NSUM(2:2) */
/*        are 0, then the file is of the G-Floating binary format. */

/*        Else the file is not of VAX type. */

/*     This routine could be reimplemented to examine all 8 bytes of */
/*     each double precision number and compare it to two tables of */
/*     values.  In the interest of simplicity the preceding option */
/*     was selected. */



/*     Convert the first and second characters in NSUM to integers. */

    *(unsigned char *)&ch__1[0] = *(unsigned char *)nsum;
    leader = *(unsigned char *)&ch__1[0];
    *(unsigned char *)&ch__1[0] = *(unsigned char *)&nsum[1];
    trailr = *(unsigned char *)&ch__1[0];

/*     Shift the trailing 4 bits off LEADER. */

    leader /= 16;

/*     Subtract the leading bits off TRAILR. */

    trailr -= trailr / 16 << 4;

/*     Now determine what file we are looking at. */

    if (leader == 8 && trailr == 0) {
	*found = TRUE_;
	*bff = 4;
    } else if (trailr != 0) {
	*found = TRUE_;
	*bff = 4;
    } else if (leader < 8 && trailr == 0) {
	*found = TRUE_;
	*bff = 3;
    } else {
	*found = FALSE_;
    }
    return 0;
} /* zzddhivf_ */

