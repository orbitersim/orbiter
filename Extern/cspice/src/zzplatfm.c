/* zzplatfm.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__6 = 6;

/* $Procedure ZZPLATFM ( Private --- Get platform attributes ) */
/* Subroutine */ int zzplatfm_(char *key, char *value, ftnlen key_len, ftnlen 
	value_len)
{
    /* Initialized data */

    static logical first = TRUE_;

    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    extern /* Subroutine */ int ucase_(char *, char *, ftnlen, ftnlen);
    integer index;
    extern /* Subroutine */ int ljust_(char *, char *, ftnlen, ftnlen);
    extern integer isrchc_(char *, integer *, char *, ftnlen, ftnlen);
    static char keyval[64*6];
    char keycpy[64];
    static char attcpy[32*7];

/* $ Abstract */

/*     Return platform ID and various attributes of the intended */
/*     environment */

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

/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     KEY        I   String indicating what information to return. */
/*     VALUE      O   String containing the requested information. */

/* $ Detailed_Input */

/*     KEY         is a string value that indicates which platform */
/*                 specific information is desired.  Acceptable inputs */
/*                 are: */

/*                    'SYSTEM'      -  System Identification String */
/*                    'O/S'         -  Operating System or Environment */
/*                    'COMPILER'    -  NAIF Supported Compiler */
/*                    'FILE_FORMAT' -  Native Binary File Format */
/*                    'TEXT_FORMAT' -  Native Text File Format */
/*                    'READS_BFF'   -  List of supported binary file */
/*                                     formats. */

/*                 Note: The comparison is case-insensitive, and the */
/*                       supplied value must fit into a string buffer */
/*                       KYSIZE characters in length. */

/* $ Detailed_Output */

/*     VALUE       is the string that holds the information requested */
/*                 by the input string KEY. VALUE must be able to */
/*                 contain the maximum number of characters returned */
/*                 by this routine, WDSIZE, or truncation will occur. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If the KEY is invalid, then VALUE is set to the value */
/*        stored in the character string parameter DEFRPY defined */
/*        below. */

/*     2) If VALUE is not large enough to contain the requested */
/*        KEY's value, then truncation will occur. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine serves to identify the platform and compiler */
/*     used in creating SPICELIB.  It is provided so that routines */
/*     and programs can make run-time decisions based upon the */
/*     ambient Fortran environment. */

/*     Operating Systems: */

/*        This routine is now aware of the operating systems for which */
/*        the code is intended for compilation.  In some cases this may */
/*        be more than one operating system, particularly in the case */
/*        of the PC. */

/*     Binary File Format: */

/*        This routine now adds the capability to return at run time */
/*        the binary file architecture that is native to the system. */

/*     Text File Format: */

/*        This routine now has the capability to return at run time */
/*        the mechanism (or line terminator) used to delimit lines */
/*        in text files.  In most cases it will return common labels */
/*        for the special characters FORTRAN considers line break */
/*        indicators. */

/*     Binary File Formats Read: */

/*        This returns a space delimited list of all the binary file */
/*        formats this environment can read for DAF/DAS based files. */

/* $ Examples */

/*     This routine could be used so that a single routine */
/*     could be written that translates the meaning of IOSTAT values */
/*     that depend upon the machine and compiler.  At run time */
/*     the routine could look up the appropriate message to associate */
/*     with an IOSTAT value. */

/* $ Restrictions */

/*     1) VALUE must be large enough to contain the requested */
/*        information, otherwise truncation will occur. */

/*     2) The string passed in via the KEY input must be capable */
/*        of being properly copied into the KEYCPY buffer internal */
/*        to this routine. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */
/*     F.S. Turner     (JPL) */
/*     E.D. Wright     (JPL) */
/*     B.V. Semenov    (JPL) */

/* $ Version */

/* -    SPICELIB Version 3.1.0, 28-NOV-2021 (BVS) */

/*        Updated for: */

/*           MAC-OSX-M1-64BIT-CLANG_C */

/*        environment. */

/* -    SPICELIB Version 3.0.0, 10-MAR-2014 (BVS) */

/*        Updated for: */

/*           PC-CYGWIN-64BIT-GCC_C */
/*           PC-CYGWIN-64BIT-GFORTRAN */
/*           PC-CYGWIN-GFORTRAN */
/*           PC-LINUX-64BIT-IFORT */
/*           SUN-SOLARIS-64BIT-INTEL */

/*        environments. */

/* -    SPICELIB Version 2.9.0, 16-MAR-2010 (EDW) */

/*        Updated for: */

/*           MAC-OSX-64BIT-INTEL_C */
/*           PC-64BIT-MS_C */
/*           SUN-SOLARIS-64BIT-NATIVE_C */
/*           MAC-OSX-64BIT-GFORTRAN */
/*           MAC-OSX-64BIT-IFORT */
/*           PC-LINUX-64BIT-GFORTRAN */
/*           PC-WINDOWS-64BIT-IFORT */
/*           SUN-SOLARIS-INTEL-64BIT-CC_C */
/*           SUN-SOLARIS-INTEL-CC_C */
/*           SUN-SOLARIS-INTEL */

/*        environments. */

/* -    SPICELIB Version 2.8.0, 12-JAN-2009 (EDW) */

/*        Added MAC-OSX-GFORTRAN PC-LINUX-GFORTRAN environments. */

/* -    SPICELIB Version 2.7.0, 19-FEB-2008 (BVS) */

/*        Added PC-LINUX-IFORT environment. */

/* -    SPICELIB Version 2.6.0, 15-NOV-2006 (NJB) */

/*        Added PC-WINDOWS-IFORT, MAC-OSX-IFORT, and MAC-OSX-INTEL_C */
/*        environments. */

/* -    SPICELIB Version 2.5.0, 21-FEB-2006 (NJB) */

/*        Added PC-LINUX-64BIT-GCC_C environment. */

/*        Corrected error in in-line comments:  changed keyword */
/*        from FILE_ARCH to FILE_FORMAT. */

/* -    SPICELIB Version 2.4.0, 14-MAR-2005 (BVS) */

/*        Added SUN-SOLARIS-64BIT-GCC_C environment. */

/* -    SPICELIB Version 2.3.0, 31-DEC-2004 (BVS) */

/*        Added PC CYGWIN environments. Changed OS for PC-LAHEY, */
/*        PC-DIGITAL, and PC-MS_C to 'MICROSOFT WINDOWS'. */

/* -    SPICELIB Version 2.2.0, 07-JUL-2002 (EDW) */

/*        Added Mac OS X Unix environment. */

/* -    SPICELIB Version 2.1.0, 06-FEB-2002 (FST) */

/*        Updated the 'TEXT_FORMAT' key value for the PC-LINUX_C */
/*        environment.  Previous versions incorrectly indicated */
/*        'CR-LF' as line terminators. */

/* -    SPICELIB Version 2.0.0, 05-JUN-2001 (FST) */

/*        Added TEXT_FORMAT and READS_BFF key/value pairs. */
/*        Modified the header slightly to improve word choice; */
/*        specifically binary file format replaces file */
/*        architecture. */

/*        Updated the compiler entry for the PC-LINUX */
/*        environment to refer to g77 as opposed to f2c. */

/*        Updated the compiler entry for the MACPPC environment. */
/*        This environment is now officially tied to Absoft */
/*        Fortran. */

/*        Updated the compiler entry for the PC-LAHEY environment. */
/*        The compiler for this environment is LF95, the latest */
/*        offering from Lahey. */

/* -    SPICELIB Version 1.0.0, 22-FEB-1999 (FST) */

/* -& */
/* $ Index_Entries */

/*     fetch platform dependent information */

/* -& */

/*     SPICELIB Functions */


/*     Local Parameters */


/*     Array index parameters for each of the key/value pairs. */

/*     SYSTEM Index. */


/*     O/S Index. */


/*     Compiler Index. */


/*     Binary File Format Index. */


/*     Text File Format Index */


/*     Reads Binary File Format Index. */


/*     Size of the buffer in which KEY is placed. */


/*     Maximum Size of local string returned in VALUE */


/*     Number of Platform Dependent values stored here. */


/*     Default Reply in the event of an invalid KEY. */


/*     Local Variables */


/*     Saved Variables */


/*     Data Statements */


/*     Make the initial assignments to the saved character array. */

    if (first) {

/*        Store the keys in the KEYVAL array. */

	s_copy(keyval, "SYSTEM", (ftnlen)64, (ftnlen)6);
	s_copy(keyval + 64, "O/S", (ftnlen)64, (ftnlen)3);
	s_copy(keyval + 128, "COMPILER", (ftnlen)64, (ftnlen)8);
	s_copy(keyval + 192, "FILE_FORMAT", (ftnlen)64, (ftnlen)11);
	s_copy(keyval + 256, "TEXT_FORMAT", (ftnlen)64, (ftnlen)11);
	s_copy(keyval + 320, "READS_BFF", (ftnlen)64, (ftnlen)9);

/*        Set the default reply to be the zero'th component of ATTCPY. */
/*        This obviates IF-THEN-ELSE branching all together. */

	s_copy(attcpy, "<UNAVAILABLE>                   ", (ftnlen)32, (
		ftnlen)32);

/*        Platform/Environment specific assignments follow. */

	s_copy(attcpy + 32, "PC", (ftnlen)32, (ftnlen)2);
	s_copy(attcpy + 64, "LINUX", (ftnlen)32, (ftnlen)5);
	s_copy(attcpy + 96, "GCC/64BIT", (ftnlen)32, (ftnlen)9);
	s_copy(attcpy + 128, "LTL-IEEE", (ftnlen)32, (ftnlen)8);
	s_copy(attcpy + 160, "LF", (ftnlen)32, (ftnlen)2);
	s_copy(attcpy + 192, "BIG-IEEE LTL-IEEE", (ftnlen)32, (ftnlen)17);

/*        Don't execute these assignments again. */

	first = FALSE_;
    }

/*     Determine which KEY was passed in; do this by converting KEY */
/*     to the known member of the equivalence class of possible */
/*     values. */

    ucase_(key, keycpy, key_len, (ftnlen)64);
    ljust_(keycpy, keycpy, (ftnlen)64, (ftnlen)64);

/*     Find out which key we were given.  In the event that one of the */
/*     KEYVALs (or some equivalent string) was not passed in, ISRCHC */
/*     returns a value of zero. */

    index = isrchc_(keycpy, &c__6, keyval, (ftnlen)64, (ftnlen)64);
    s_copy(value, attcpy + (((i__1 = index) < 7 && 0 <= i__1 ? i__1 : s_rnge(
	    "attcpy", i__1, "zzplatfm_", (ftnlen)433)) << 5), value_len, (
	    ftnlen)32);
    return 0;
} /* zzplatfm_ */

