/* writln.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* $Procedure WRITLN ( Write a text line to a logical unit ) */
/* Subroutine */ int writln_(char *line, integer *unit, ftnlen line_len)
{
    /* System generated locals */
    cilist ci__1;

    /* Builtin functions */
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void);

    /* Local variables */
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern integer rtrim_(char *, ftnlen);
    extern /* Subroutine */ int errfnm_(char *, integer *, ftnlen), sigerr_(
	    char *, ftnlen), chkout_(char *, ftnlen), setmsg_(char *, ftnlen);
    integer iostat;
    extern /* Subroutine */ int errint_(char *, integer *, ftnlen);

/* $ Abstract */

/*     Write a single line of text to the Fortran logical unit UNIT. */

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

/*     ASCII */
/*     FILES */
/*     TEXT */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     LINE       I   The line which is to be written to UNIT. */
/*     UNIT       I   The Fortran unit number to use for output. */

/* $ Detailed_Input */

/*     LINE     is the text line which is to be written to UNIT. */

/*              The value of this variable is not modified. */

/*     UNIT     is the Fortran unit number for the output. This may be */
/*              either the unit number for the terminal, or the unit */
/*              number of a previously opened text file. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If an error occurs while attempting to write to the text */
/*         file attached to UNIT, the error SPICE(FILEWRITEFAILED) is */
/*         signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine will write a single text line to the device */
/*     specified by UNIT. UNIT may be the terminal, or it may be */
/*     a logical unit number obtained from a Fortran OPEN or INQUIRE */
/*     statement. When written, the line will have trailing spaces */
/*     removed. */

/* $ Examples */

/*     CALL WRITLN( LINE, UNIT ) */

/*     You have now written a line of text to unit UNIT. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     H.A. Neilan        (JPL) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.27.0, 28-NOV-2021 (BVS) */

/*        Updated for MAC-OSX-M1-64BIT-CLANG_C. */

/* -    SPICELIB Version 2.26.0, 16-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Removed */
/*        unnecessary $Revisions section. */

/*        Removed implementation detail (discovery check-in) from */
/*        $Exceptions section. */

/* -    SPICELIB Version 2.25.0, 10-MAR-2014 (BVS) */

/*        Updated for SUN-SOLARIS-64BIT-INTEL. */

/* -    SPICELIB Version 2.24.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-LINUX-64BIT-IFORT. */

/* -    SPICELIB Version 2.23.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-CYGWIN-GFORTRAN. */

/* -    SPICELIB Version 2.22.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-CYGWIN-64BIT-GFORTRAN. */

/* -    SPICELIB Version 2.21.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-CYGWIN-64BIT-GCC_C. */

/* -    SPICELIB Version 2.20.1, 18-MAY-2010 (BVS) */

/*        Removed "C$" marker from text in the header. */

/* -    SPICELIB Version 2.20.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-INTEL. */

/* -    SPICELIB Version 2.19.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-INTEL-CC_C. */

/* -    SPICELIB Version 2.18.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C. */

/* -    SPICELIB Version 2.17.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-64BIT-NATIVE_C. */

/* -    SPICELIB Version 2.16.0, 13-MAY-2010 (BVS) */

/*        Updated for PC-WINDOWS-64BIT-IFORT. */

/* -    SPICELIB Version 2.15.0, 13-MAY-2010 (BVS) */

/*        Updated for PC-LINUX-64BIT-GFORTRAN. */

/* -    SPICELIB Version 2.14.0, 13-MAY-2010 (BVS) */

/*        Updated for PC-64BIT-MS_C. */

/* -    SPICELIB Version 2.13.0, 13-MAY-2010 (BVS) */

/*        Updated for MAC-OSX-64BIT-INTEL_C. */

/* -    SPICELIB Version 2.12.0, 13-MAY-2010 (BVS) */

/*        Updated for MAC-OSX-64BIT-IFORT. */

/* -    SPICELIB Version 2.11.0, 13-MAY-2010 (BVS) */

/*        Updated for MAC-OSX-64BIT-GFORTRAN. */

/* -    SPICELIB Version 2.10.0, 18-MAR-2009 (BVS) */

/*        Updated for PC-LINUX-GFORTRAN. */

/* -    SPICELIB Version 2.9.0, 18-MAR-2009 (BVS) */

/*        Updated for MAC-OSX-GFORTRAN. */

/* -    SPICELIB Version 2.8.0, 19-FEB-2008 (BVS) */

/*        Updated for PC-LINUX-IFORT. */

/* -    SPICELIB Version 2.7.0, 14-NOV-2006 (BVS) */

/*        Updated for PC-LINUX-64BIT-GCC_C. */

/* -    SPICELIB Version 2.6.0, 14-NOV-2006 (BVS) */

/*        Updated for MAC-OSX-INTEL_C. */

/* -    SPICELIB Version 2.5.0, 14-NOV-2006 (BVS) */

/*        Updated for MAC-OSX-IFORT. */

/* -    SPICELIB Version 2.4.0, 14-NOV-2006 (BVS) */

/*        Updated for PC-WINDOWS-IFORT. */

/* -    SPICELIB Version 2.3.0, 26-OCT-2005 (BVS) */

/*        Updated for SUN-SOLARIS-64BIT-GCC_C. */

/* -    SPICELIB Version 2.2.0, 03-JAN-2005 (BVS) */

/*        Updated for PC-CYGWIN_C. */

/* -    SPICELIB Version 2.1.0, 03-JAN-2005 (BVS) */

/*        Updated for PC-CYGWIN. */

/* -    SPICELIB Version 2.0.5, 17-JUL-2002 (BVS) */

/*        Added MAC-OSX environments. */

/* -    SPICELIB Version 2.0.4, 08-OCT-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given. New */
/*        environments are WIN-NT */

/* -    SPICELIB Version 2.0.3, 16-SEP-1999 (NJB) */

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

/* -    SPICELIB Version 2.0.0, 08-APR-1998 (NJB) */

/*        Module was updated for the PC-LINUX platform. */

/* -    SPICELIB Version 1.1.1, 20-AUG-1996 (WLT) */

/*        Corrected the heading for the $Index_Entries section. */

/* -    SPICELIB Version 1.1.0, 29-FEB-1996 (KRG) */

/*        For the Macintosh, we need to use real Fortran I/O, i.e., */
/*        using the first column for carriage control. The change */
/*        was to move the MAC environment indicator from one */
/*        environment case to the other. */

/*        Also, for UNIX environments, the parameter STDOUT is no */
/*        longer defined. This only appears for platforms that */
/*        need it to differentiate between writing to a file and */
/*        the terminal screen (standard output), currently: VAX, */
/*        PC-LAHEY, PC-MS, and MAC. */

/* -    SPICELIB Version 1.0.0, 20-DEC-1995 (KRG) (HAN) */

/*        The routine graduated */

/*     Beta Version 3.1.0, 18-AUG-1995 (KRG) */

/*        Moved the PC-LAHEY environment indicator from one environment */
/*        case to the other. The Lahey compiler on the PC does treat text */
/*        files and the standard output device differently. */

/*     Beta Version 3.0.1, 01-JAN-1995 (KRG) */

/*        Moved the description of the input variable UNIT from the $ */
/*        $Detailed_Output section of the header to the correct location */
/*        in the $Detailed_Input section of the header. */

/*     Beta Version 3.0.0, 11-JUL-1994 (HAN) */

/*        Edited master source file to correct the code for the */
/*        PC/Microsoft FORTRAN PowerStation environment. It should use */
/*        the same code as the VAX, not the PC/Lahey Fortran code. Also, */
/*        code was included for the DEC Alpha OpenVMS/DEC Fortran and */
/*        Sun Solaris/Sun Fortran environments. */

/*     Beta Version 2.0.0, 30-MAR-1994 (HAN) */

/*        Edited master source file to include new environments: */
/*        Silicon Graphics IRIX/Silicon Graphics Fortran, */
/*        DEC Alpha-OSF/1, and NeXT/Absoft Fortran. */

/*     Beta Version 1.0.0, 17-DEC-1992 (KRG) */

/* -& */
/* $ Index_Entries */

/*     write a text line to a logical unit */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     UNIX based fortran implementations typically do not distinguish */
/*     between a text file and the standard output unit, so no leading */
/*     vertical spacing character is required. */

    ci__1.cierr = 1;
    ci__1.ciunit = *unit;
    ci__1.cifmt = "(A)";
    iostat = s_wsfe(&ci__1);
    if (iostat != 0) {
	goto L100001;
    }
    iostat = do_fio(&c__1, line, rtrim_(line, line_len));
    if (iostat != 0) {
	goto L100001;
    }
    iostat = e_wsfe();
L100001:

/*     Check to see if we got a write error, and signal it if we did. */
/*     Also check in and check out. */

    if (iostat != 0) {
	chkin_("WRITLN", (ftnlen)6);
	setmsg_("Error Writing to file: #. IOSTAT = #.", (ftnlen)37);
	errfnm_("#", unit, (ftnlen)1);
	errint_("#", &iostat, (ftnlen)1);
	sigerr_("SPICE(FILEWRITEFAILED)", (ftnlen)22);
	chkout_("WRITLN", (ftnlen)6);
	return 0;
    }
    return 0;
} /* writln_ */

