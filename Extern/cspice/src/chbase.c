/* chbase.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure CHBASE ( Character set base ) */
integer chbase_(void)
{
    /* System generated locals */
    integer ret_val;

/* $ Abstract */

/*     Return the base value used to encode unsigned integer values */
/*     in character strings. */

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

/*     CONSTANTS */

/* $ Declarations */
/* $ Brief_I/O */

/*     The function returns the base value used to encode unsigned */
/*     integer values in character strings. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     CHBASE is the base used by ENCHAR and DECHAR to encode and decode */
/*     non-negative integers to and from character strings. Its value is */
/*     determined by the size of the character set available for a given */
/*     machine and compiler. Strictly speaking, CHBASE is one more than */
/*     the biggest positive integer which can be handled by both the */
/*     CHAR and ICHAR intrinsic functions (which are used by ENCHAR and */
/*     DECHAR). That is, CHBASE is the first positive integer for which */
/*     the logical expression */

/*           ( ICHAR ( CHAR ( CHBASE ) ) .EQ. CHBASE ) */

/*     is .FALSE. */

/*     Note that CHBASE can be (and probably is) different from the */
/*     number of characters in the character set used by the processor. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The function always returns a constant value, set by the user */
/*     prior to compilation. */

/*     CHBASE should always be at least 128 (the size of the ASCII */
/*     character set), and will usually be 256 for machines which use */
/*     eight bits to represent a single character. The following list */
/*     contains the values of CHBASE for a range of environments. */

/*     Environment: VAX/VMS, VAX FORTRAN */
/*     Value:       256 */

/*     Environment: Sun, Sun FORTRAN */
/*     Value:       256 */

/*     Environment: PC, MS FORTRAN */
/*     Value:       256 */

/*     Environment: Macintosh, Language Systems FORTRAN */
/*     Value:       256 */

/*     Environment: PC, Lahey F77 EM/32 Version 4.0 */
/*     Value:       256 */

/*     Environment: HP-UX 9000/750, FORTRAN/9000 Series 700 computers */
/*     Value:       256 */

/*     Environment: Silicon Graphics IRIX OS, SGI FORTRAN 77 */
/*     Value:       256 */

/*     Environment: DEC Alpha 3000/4000, OSF/1, DEC FORTRAN-77 */
/*     Value:       256 */

/*     Environment: NeXT/Mach OS, Absoft Fortran */
/*     Value:       256 */

/*     Environment: PC/Linux, Fort77 */
/*     Value:       128 */


/*     For other machines, the value can be determined by running */
/*     the following simple program: */

/*            INTEGER          CHBASE */
/*            DATA             CHBASE    / 0 / */

/*            DO WHILE ( .TRUE. ) */

/*               IF ( ICHAR (CHAR ( CHBASE ) ) .EQ. CHBASE ) THEN */
/*                  CHBASE = CHBASE + 1 */
/*               ELSE */
/*                  WRITE (6,*) 'CHBASE for this machine is : ', CHBASE */
/*                  STOP */
/*               END IF */

/*            END DO */
/*            END */

/* $ Examples */

/*     See ENCHAR, DECHAR. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     [1]  "Programming in VAX FORTRAN", Digital Equipment Corporation, */
/*          Section 8.3, pp.8-6, September 1984. */

/*     [2]  "Microsoft FORTRAN Reference", Microsoft Corporation, */
/*          Section 5.1.1, p.241, 1989. */

/*     [3]  "Language Systems FORTRAN Reference Manual", Language Systems */
/*          Corporation, version 1.2.1, pp.3-20. */

/*     [4]  "Lahey F77L EM/32 FORTRAN Language Reference Manual", p.222, */
/*          Note 20. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     H.A. Neilan        (JPL) */
/*     B.V. Semenov       (JPL) */
/*     M.J. Spencer       (JPL) */
/*     W.L. Taber         (JPL) */
/*     F.S. Turner        (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.27.0, 28-NOV-2021 (BVS) */

/*        Updated for MAC-OSX-M1-64BIT-CLANG_C. */

/* -    SPICELIB Version 2.26.1, 12-AUG-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 2.26.0, 10-MAR-2014 (BVS) */

/*        Updated for SUN-SOLARIS-64BIT-INTEL. */

/* -    SPICELIB Version 2.25.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-LINUX-64BIT-IFORT. */

/* -    SPICELIB Version 2.24.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-CYGWIN-GFORTRAN. */

/* -    SPICELIB Version 2.23.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-CYGWIN-64BIT-GFORTRAN. */

/* -    SPICELIB Version 2.22.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-CYGWIN-64BIT-GCC_C. */

/* -    SPICELIB Version 2.21.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-INTEL. */

/* -    SPICELIB Version 2.20.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-INTEL-CC_C. */

/* -    SPICELIB Version 2.19.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C. */

/* -    SPICELIB Version 2.18.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-64BIT-NATIVE_C. */

/* -    SPICELIB Version 2.17.0, 13-MAY-2010 (BVS) */

/*        Updated for PC-WINDOWS-64BIT-IFORT. */

/* -    SPICELIB Version 2.16.0, 13-MAY-2010 (BVS) */

/*        Updated for PC-LINUX-64BIT-GFORTRAN. */

/* -    SPICELIB Version 2.15.0, 13-MAY-2010 (BVS) */

/*        Updated for PC-64BIT-MS_C. */

/* -    SPICELIB Version 2.14.0, 13-MAY-2010 (BVS) */

/*        Updated for MAC-OSX-64BIT-INTEL_C. */

/* -    SPICELIB Version 2.13.0, 13-MAY-2010 (BVS) */

/*        Updated for MAC-OSX-64BIT-IFORT. */

/* -    SPICELIB Version 2.12.0, 13-MAY-2010 (BVS) */

/*        Updated for MAC-OSX-64BIT-GFORTRAN. */

/* -    SPICELIB Version 2.11.0, 18-MAR-2009 (BVS) */

/*        Updated for PC-LINUX-GFORTRAN. */

/* -    SPICELIB Version 2.10.0, 18-MAR-2009 (BVS) */

/*        Updated for MAC-OSX-GFORTRAN. */

/* -    SPICELIB Version 2.9.0, 19-FEB-2008 (BVS) */

/*        Updated for PC-LINUX-IFORT. */

/* -    SPICELIB Version 2.8.0, 14-NOV-2006 (BVS) */

/*        Updated for PC-LINUX-64BIT-GCC_C. */

/* -    SPICELIB Version 2.7.0, 14-NOV-2006 (BVS) */

/*        Updated for MAC-OSX-INTEL_C. */

/* -    SPICELIB Version 2.6.0, 14-NOV-2006 (BVS) */

/*        Updated for MAC-OSX-IFORT. */

/* -    SPICELIB Version 2.5.0, 14-NOV-2006 (BVS) */

/*        Updated for PC-WINDOWS-IFORT. */

/* -    SPICELIB Version 2.4.0, 26-OCT-2005 (BVS) */

/*        Updated for SUN-SOLARIS-64BIT-GCC_C. */

/* -    SPICELIB Version 2.3.0, 03-JAN-2005 (BVS) */

/*        Updated for PC-CYGWIN_C. */

/* -    SPICELIB Version 2.2.0, 03-JAN-2005 (BVS) */

/*        Updated for PC-CYGWIN. */

/* -    SPICELIB Version 2.1.1, 17-JUL-2002 (BVS) */

/*        Added MAC-OSX environments. */

/* -    SPICELIB Version 2.1.0, 05-DEC-2001 (FST) */

/*        Updated the value for PC-LINUX environment. */

/* -    SPICELIB Version 2.0.4, 08-OCT-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given. New */
/*        environments are WIN-NT */

/* -    SPICELIB Version 2.0.3, 24-SEP-1999 (NJB) */

/*        CSPICE environments were added. Some typos were corrected. */

/* -    SPICELIB Version 2.0.2, 28-JUL-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given. New */
/*        environments are PC-DIGITAL, SGI-O32 and SGI-N32. */

/* -    SPICELIB Version 2.0.1, 18-MAR-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given. Previously, */
/*        environments such as SUN-SUNOS and SUN-SOLARIS were implied */
/*        by the environment label SUN. */

/* -    SPICELIB Version 2.0.0, 05-APR-1998 (NJB) */

/*        Added reference to the PC-LINUX environment. */

/* -    SPICELIB Version 1.5.0, 03-NOV-1993 (HAN) */

/*        Module was updated to include the character base */
/*        value for the Silicon Graphics, DEC Alpha-OSF/1, and */
/*        NeXT platforms. */

/* -    SPICELIB Version 1.4.0, 06-OCT-1992 (HAN) */

/*        Module was updated to include the character base */
/*        value for the Hewlett Packard UX 9000/750 environment, */
/*        and the value for the Sun was changed from 128 to 256. */
/*        Both changes are the result of running the program in */
/*        the $Particulars section of the header on both machines. */

/* -    SPICELIB Version 1.3.1, 10-MAR-1999 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.3.0, 13-NOV-1991 (MJS) */

/*        Module was updated to include the character base */
/*        value for the Lahey FORTRAN EM/32 environment (PC). */

/* -    SPICELIB Version 1.2.0, 07-DEC-1990 (MJS) */

/*        Module was updated to include the character base */
/*        value for the Macintosh. */

/* -    SPICELIB Version 1.1.0, 09-MAR-1990 (HAN) */

/*        Module was updated to include the character base */
/*        value for the Sun. Sources for the values contained */
/*        in this module are now specified in the $Literature_References */
/*        section. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU) */

/* -& */
/* $ Index_Entries */

/*     base for encoding integers in character_string */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 2.1.0, 05-DEC-2001 (FST) */

/*        It was discovered that linux distributions shipping */
/*        versions of g77 derived off of gcc versions 2.96-3.00 */
/*        suffer from in implementation change in ICHAR that */
/*        requires CHBASE to change to 128. Since restricting */
/*        CHBASE to 128 has little impact on other linux */
/*        environments utilizing other versions of g77 or fort77, */
/*        we elected to make the change to all environments */
/*        rather than complicate this issue by forking a new one. */

/* -    SPICELIB Version 1.4.0, 06-OCT-1992 (HAN) */

/*        Module was updated to include the character base */
/*        value for the Hewlett Packard UX 9000/750 environment, */
/*        and the value for the Sun was changed from 128 to 256. */
/*        Both changes are the result of running the program in */
/*        the $Particulars section of the header on both machines. */

/*        The previous Sun value was computed on the Sun3 and was */
/*        not updated when we moved to the Sun4. Everything passed */
/*        the suite of test programs that would have indicated a bug. */

/*        The code was also reformatted so that a utility program can */
/*        create the file for each environment. */

/* -    Beta Version 1.1.0, 16-FEB-1989 (HAN) (NJB) */

/*        Contents of the $Exceptions section was changed */
/*        to "error free" to reflect the decision that the */
/*        module will never participate in error handling. */

/*        Missing parentheses added to CHBASE declaration. */

/* -& */

/*     We have provided values for several popular machines. Remove */
/*     the comment character in front of the value for your machine, */
/*     or provide your own value. Numbers are provided in a variety */
/*     of formats: decimal, hex, and binary. These last two formats */
/*     are not portable; but then, neither are the values. */


/*     VAX, VAX FORTRAN */
/*     Sun, Sun FORTRAN */
/*     IBM PC, Microsoft FORTRAN, Lahey EM/32 FORTRAN */
/*     Macintosh, Language Systems FORTRAN */
/*     HP-UX 9000/750, FORTRAN/9000 Series 700 computers */
/*     Silicon Graphics, IRIX OS, SGI FORTRAN 77 */
/*     DEC Alpha, OSF/1, DEC FORTRAN-77 */
/*     NeXT, Mach OS, Absoft Fortran 77 */

    ret_val = 256;
    return ret_val;
} /* chbase_ */

