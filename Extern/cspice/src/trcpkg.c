/* trcpkg.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__5 = 5;
static integer c__3 = 3;
static integer c__1 = 1;
static integer c__0 = 0;

/* $Procedure TRCPKG ( Trace package ) */
/* Subroutine */ int trcpkg_0_(int n__, integer *depth, integer *index, char *
	module, char *trace, char *name__, ftnlen module_len, ftnlen 
	trace_len, ftnlen name_len)
{
    /* Initialized data */

    static logical notrc = FALSE_;
    static integer frzcnt = 0;
    static integer frzovr = 0;
    static integer maxdep = 0;
    static integer modcnt = 0;
    static integer ovrflw = 0;

    /* System generated locals */
    address a__1[5], a__2[3];
    integer i__1, i__2, i__3[5], i__4[3], i__5;
    char ch__1[149], ch__2[64];

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer i_len(char *, ftnlen), s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen);

    /* Local variables */
    integer i__, l;
    static char stack[32*100];
    integer first;
    extern integer rtrim_(char *, ftnlen);
    extern logical failed_(void);
    char device[255];
    extern /* Subroutine */ int getact_(integer *);
    integer action;
    extern /* Subroutine */ int getdev_(char *, ftnlen);
    char tmpnam[80];
    extern integer frstnb_(char *, ftnlen);
    extern /* Subroutine */ int wrline_(char *, char *, ftnlen, ftnlen);
    static char frozen[32*100];
    extern /* Subroutine */ int suffix_(char *, integer *, char *, ftnlen, 
	    ftnlen);
    char string[11];
    extern /* Subroutine */ int intstr_(integer *, char *, ftnlen);

/* $ Abstract */

/*     Maintain a trace of subroutine calls for error messages. */

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

/*     ERROR */

/* $ Keywords */

/*     ERROR */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  ENTRY POINTS */
/*     --------  ---  -------------------------------------------------- */
/*     DEPTH      O   TRCDEP */
/*     DEPTH      O   TRCMXD */
/*     INDEX      I   TRCNAM */
/*     NAME       O   TRCNAM */
/*     MODULE     I   CHKIN, CHKOUT */
/*     TRACE      O   QCKTRC */
/*     FILEN      P */
/*     NAMLEN     P */
/*     MAXMOD     P */

/* $ Detailed_Input */

/*     See the ENTRY points for discussions of their arguments. */

/* $ Detailed_Output */

/*     See the ENTRY points for discussions of their arguments. */

/* $ Parameters */

/*     FILEN    is the maximum length of a file name. */

/*     NAMLEN   is the maximum length of the significant */
/*              portion of a module name. */

/*     MAXMOD   is the maximum storage depth for names in the */
/*              traceback stack. */

/* $ Exceptions */

/*     1)  If TRCPKG is called directly, the error SPICE(BOGUSENTRY) is */
/*         signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The entry points declared in this routine are: */

/*       CHKIN */
/*       CHKOUT */
/*       TRCDEP */
/*       TRCMXD */
/*       TRCNAM */
/*       QCKTRC */
/*       FREEZE */
/*       TRCOFF */

/*     This routine serves as an umbrella that allows the entry */
/*     points to share data. TRCPKG should never be called directly. */

/*     See the subroutine ERRACT for descriptions of the error actions */
/*     and codes. */

/* $ Examples */

/*     See the entry points CHKIN, CHKOUT, TRCDEP, TRCMXD, TRCNAM, */
/*     QCKTRC, FREEZE, and TRCOFF for examples. */

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

/* -    SPICELIB Version 4.28.0, 28-NOV-2021 (BVS) */

/*        Updated for MAC-OSX-M1-64BIT-CLANG_C. */

/* -    SPICELIB Version 4.27.0, 03-JUN-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Added entry in */
/*        $Index_Entries. */

/* -    SPICELIB Version 4.26.0, 10-MAR-2014 (BVS) */

/*        Updated for SUN-SOLARIS-64BIT-INTEL. */

/* -    SPICELIB Version 4.25.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-LINUX-64BIT-IFORT. */

/* -    SPICELIB Version 4.24.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-CYGWIN-GFORTRAN. */

/* -    SPICELIB Version 4.23.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-CYGWIN-64BIT-GFORTRAN. */

/* -    SPICELIB Version 4.22.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-CYGWIN-64BIT-GCC_C. */

/* -    SPICELIB Version 4.21.0, 29-JUL-2013 (BVS) */

/*        Changed logic in the CHKIN and CHKOUT entries to check if the */
/*        first character of the input value is not a space and, if so, */
/*        bypass the call to FRSTNB. This change speeds up the execution */
/*        by ~20%. */

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

/* -    SPICELIB Version 4.0.5, 17-JUL-2002 (BVS) */

/*        Added MAC-OSX environments. */

/* -    SPICELIB Version 4.0.4, 08-OCT-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given. New */
/*        environments are WIN-NT */

/* -    SPICELIB Version 4.0.3, 24-SEP-1999 (NJB) */

/*        CSPICE environments were added. Some typos were corrected. */

/* -    SPICELIB Version 4.0.2, 28-JUL-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given. New */
/*        environments are PC-DIGITAL, SGI-O32 and SGI-N32. */

/* -    SPICELIB Version 4.0.1, 18-MAR-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given. Previously, */
/*        environments such as SUN-SUNOS and SUN-SOLARIS were implied */
/*        by the environment label SUN. */

/* -    SPICELIB Version 4.0.0, 07-APR-1998 (NJB) */

/*        Module was updated for the PC-LINUX platform. */

/*        Bug fix: the previous version of entry point CHKOUT failed to */
/*        make a correct module name comparison when the input name */
/*        exceeded NAMLEN characters in length. Now only the initial */
/*        NAMLEN non-blank characters (at most) of the input name are */
/*        used in the comparison. */

/* -    SPICELIB Version 3.0.0, 12-MAR-1996 (KRG) */

/*        The structure of this routine has completely changed. A stack, */
/*        implemented as an array of character strings, is now used to */
/*        store subroutine names that use the CHKIN and CHKOUT entry */
/*        points. This change simplified the individual entry points as */
/*        well as speeding up the process of checking in and checking */
/*        out. */

/*        The error action mechanism has been changed as well. GETACT */
/*        now uses an integer code rather than a short character */
/*        string to represent the error action. The entry points affected */
/*        by this change are: TRCDEP, TRCNAM, QCKTRC. */

/* -    SPICELIB Version 2.0.0, 11-NOV-1993 (HAN) */

/*        Module was updated to include the values for FILEN and */
/*        NAMLEN for the Silicon Graphics, DEC Alpha-OSF/1, and */
/*        NeXT platforms. The previous value of 256 for Unix */
/*        platforms was changed to 255. */

/* -    SPICELIB Version 1.3.0, 23-OCT-1992 (NJB) */

/*        Bug fix made to routine QCKTRC:  a section of code which */
/*        itself is exercised only if a bug is present inserted the */
/*        wrong variable into an error message. */

/* -    SPICELIB Version 1.2.0, 12-OCT-1992 (HAN) */

/*        Module was updated to include the values of the parameters */
/*        for the Hewlett Packard UX 9000/750 environment. */

/* -    SPICELIB Version 1.1.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.1.0, 18-JUN-1990 (NJB) */

/*        Added declarations for trace disabling. Re-organized */
/*        declarations. Updated comments. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (NJB) */

/* -& */
/* $ Index_Entries */

/*     error trace subsystem */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 4.0.0, 07-APR-1998 (NJB) */

/*        Module was updated for the PC-LINUX platform. */

/*        Bug fix: the previous version of entry point CHKOUT failed to */
/*        make a correct module name comparison when the input name */
/*        exceeded NAMLEN characters in length. Now only the initial */
/*        NAMLEN non-blank characters (at most) of the input name are */
/*        used in the comparison. */

/* -    SPICELIB Version 3.0.0, 12-MAR-1996 (KRG) */

/*        The structure of this routine has completely changed. A stack, */
/*        implemented as an array of character strings, is now used to */
/*        store subroutine names that use the CHKIN and CHKOUT entry */
/*        points. This change simplified the individual entry points as */
/*        well as speeding up the process of checking in and checking */
/*        out. */

/*        The error action mechanism has been changed as well. GETACT */
/*        now uses an integer code rather than a short character */
/*        string to represent the error action. The entry points affected */
/*        by this change are: TRCDEP, TRCNAM, QCKTRC. */

/* -    SPICELIB Version 2.0.0, 11-NOV-1993 (HAN) */

/*        Module was updated to include the values for FILEN and */
/*        NAMLEN for the Silicon Graphics, DEC Alpha-OSF/1, and */
/*        NeXT platforms. The previous value of 256 for Unix */
/*        platforms was changed to 255. */

/* -    SPICELIB Version 1.3.0, 23-OCT-1992 (NJB) */

/*        Bug fix made to routine QCKTRC:  a section of code which */
/*        itself is exercised only if a bug is present inserted the */
/*        wrong variable into an error message. */

/* -     SPICELIB Version 1.2.0, 12-OCT-1992 (HAN) */

/*        Module was updated to include the values of the parameters */
/*        for the Hewlett Packard UX 9000/750 environment. */

/*        The code was also reformatted so that a utility program can */
/*        create the source file for a specific environment given a */
/*        master source file. */

/* -    SPICELIB Version 1.1.0, 18-JUN-1990 (NJB) */

/*        Added declarations for trace disabling. Re-organized */
/*        declarations. Updated comments to reflect inclusion */
/*        of the new entry point TRCOFF. Also updated the header */
/*        to make the style more parallel to other SPICELIB */
/*        umbrella routines. Updated the description line and */
/*        abstract, in particular. */

/* -    Beta Version 1.0.1, 08-FEB-1989 (NJB) */

/*        Warnings added to discourage use of this routine. */

/* -& */

/*     SPICELIB functions: */


/*     Local parameters */

/*     This is the length for a local temporary string used to help */
/*     format error messages. It and the character string are only */
/*     present to avoid real or potential problems with pedantic */
/*     FORTRAN compilers. 80 characters should be more than sufficient */
/*     to contain a module name. */


/*     The integer mnemonic for the RETURN error action. */


/*     Local Variables: */


/*     Saved variables: */


/*     Initial values: */

    switch(n__) {
	case 1: goto L_chkin;
	case 2: goto L_chkout;
	case 3: goto L_trcdep;
	case 4: goto L_trcmxd;
	case 5: goto L_trcnam;
	case 6: goto L_qcktrc;
	case 7: goto L_freeze;
	case 8: goto L_trcoff;
	}


/*     Executable Code: */

    wrline_("SCREEN", "SPICE(BOGUSENTRY)", (ftnlen)6, (ftnlen)17);
    wrline_("SCREEN", "TRCPKG: You have called an entry that performs no run"
	    "-time function. ", (ftnlen)6, (ftnlen)69);
    return 0;
/* $Procedure CHKIN ( Module Check In ) */

L_chkin:
/* $ Abstract */

/*     Inform the SPICELIB error handling mechanism of entry into a */
/*     routine. */

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

/*     ERROR */

/* $ Keywords */

/*     ERROR */

/* $ Declarations */

/*     IMPLICIT NONE */

/*     CHARACTER*(*)          MODULE */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  --------------------------------------------------- */
/*     MODULE     I   The name of the calling routine. */
/*     FILEN      P   Maximum length of file name. */

/* $ Detailed_Input */

/*     MODULE   is the name of the routine calling CHKIN. The */
/*              named routine is supposed to be `checking in' */
/*              when it calls CHKIN; that is, the call should be */
/*              the first executable statement following the */
/*              reference to the function RETURN (which should be */
/*              the first executable statement). */

/*              Only the first NAMLEN non-blank characters in */
/*              a module name are stored for use in a traceback */
/*              by this subroutine. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     FILEN    is the maximum file name length that can be */
/*              accommodated by this routine. */

/* $ Exceptions */

/*     CHKIN does not signal errors; rather it writes error messages, */
/*     so as to avoid recursion. */

/*     1)  If the traceback storage area overflows, the short error */
/*         message SPICE(TRACEBACKOVERFLOW) is written to the error */
/*         output device. */

/*     2)  If the input argument MODULE is blank, the short error message */
/*         SPICE(BLANKMODULENAME) is written to the error output device. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine is part of the SPICELIB error handling mechanism. */

/*     Conceptually, the effect of this routine is to `push' the */
/*     supplied module name onto a stack. The routine CHKOUT performs */
/*     the inverse, or `pop', operation. */

/*     Every routine that participates in the traceback scheme should */
/*     have a call to CHKIN as the second executable statement. The */
/*     first executable statements should be: */

/*        IF ( RETURN() ) THEN */
/*           RETURN */
/*        ELSE */
/*           CALL CHKIN ( module ) */
/*        END IF */

/*     Here module is the name of the routine in which this code appears. */

/*     The line of code preceding the END or any RETURN statement should */
/*     be */

/*         CALL CHKOUT ( module ) */


/*     All SPICELIB routines should call CHKIN and CHKOUT, unless they */
/*     are classified as `error free'. Programs linked with SPICELIB */
/*     may also use CHKIN and CHKOUT. */

/*     Routines that don't call CHKIN and CHKOUT won't appear in the */
/*     traceback. */

/*     All routines that call CHKIN must also call CHKOUT, or else the */
/*     trace mechanism will become very confused. */

/*     It is possible to disable check-ins (and check-outs) by calling */
/*     the entry point TRCOFF. CHKIN and CHKOUT will return immediately */
/*     upon entry after TRCOFF has been called. It is not possible to */
/*     re-enable check-ins and check-outs after calling TRCOFF. Routines */
/*     that don't call CHKIN and CHKOUT won't appear in the traceback. */

/* $ Examples */

/*     See `Particulars' for an example of how to call this routine. */

/* $ Restrictions */

/*     1)  Routines that call this routine must call CHKOUT immediately */
/*         prior to any RETURN or END statement. */

/*     2)  Module names are assumed to have no embedded blanks. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 4.2.0, 03-JUN-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 4.1.0, 29-JUL-2013 (BVS) */

/*        Changed logic to check if the first character of the input */
/*        value is not a space and, if so, bypass the call to FRSTNB. */
/*        This change speeds up the execution by ~20%. */

/* -    SPICELIB Version 4.0.5, 17-JUL-2002 (BVS) */

/*        Added MAC-OSX environments. */

/* -    SPICELIB Version 4.0.4, 08-OCT-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given. New */
/*        environments are WIN-NT */

/* -    SPICELIB Version 4.0.3, 24-SEP-1999 (NJB) */

/*        CSPICE environments were added. Some typos were corrected. */

/* -    SPICELIB Version 4.0.2, 28-JUL-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given. New */
/*        environments are PC-DIGITAL, SGI-O32 and SGI-N32. */

/* -    SPICELIB Version 4.0.1, 18-MAR-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given. Previously, */
/*        environments such as SUN-SUNOS and SUN-SOLARIS were implied */
/*        by the environment label SUN. */

/* -    SPICELIB Version 4.0.0, 07-APR-1998 (NJB) */

/*        Module was updated for the PC-LINUX platform. */

/*        Bug fix: the previous version of entry point CHKOUT failed to */
/*        make a correct module name comparison when the input name */
/*        exceeded NAMLEN characters in length. Now only the initial */
/*        NAMLEN non-blank characters (at most) of the input name are */
/*        used in the comparison. */

/* -    SPICELIB Version 3.0.0, 12-MAR-1996 (KRG) */

/*        The structure of this routine has completely changed. A stack, */
/*        implemented as an array of character strings, is now used to */
/*        store subroutine names that use the CHKIN and CHKOUT entry */
/*        points. This change simplified the individual entry points as */
/*        well as speeding up the process of checking in and checking */
/*        out. */

/*        The short error dealing with embedded blanks has been removed, */
/*        since the new implementation is not hampered by Embedded */
/*        blanks. */

/* -    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 2.0.0, 15-JUN-1990 (NJB) */

/*        Disabling of check-ins implemented. Many parts of the */
/*        header have be re-written. Weird spacing ameliorated. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (NJB) */

/* -& */
/* $ Index_Entries */

/*     module check in */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 4.0.0, 07-APR-1998 (NJB) */

/*        Module was updated for the PC-LINUX platform. */

/*        Bug fix: the previous version of entry point CHKOUT failed to */
/*        make a correct module name comparison when the input name */
/*        exceeded NAMLEN characters in length. Now only the initial */
/*        NAMLEN non-blank characters (at most) of the input name are */
/*        used in the comparison. */

/* -    SPICELIB Version 3.0.0, 12-MAR-1996 (KRG) */

/*        The structure of this routine has completely changed. A stack, */
/*        implemented as an array of character strings, is now used to */
/*        store subroutine names that use the CHKIN and CHKOUT entry */
/*        points. This change simplified the individual entry points as */
/*        well as speeding up the process of checking in and checking */
/*        out. */

/*        The short error dealing with embedded blanks has been removed, */
/*        since the new implementation is not hampered by Embedded */
/*        blanks. */

/* -    SPICELIB Version 2.0.0, 15-JUN-1990  (NJB) */

/*        Disabling of check-ins implemented. Many parts of the */
/*        header have be re-written. Weird spacing ameliorated. */

/* -    Beta Version 1.1.1, 10-FEB-1988  (NJB) */

/*        Parameter declarations documented. $Parameters section added, */
/*        and parameter declarations listed in `Brief I/O'. */

/* -    Beta Version 1.1.0, 27-OCT-1988  (NJB) */

/*        Cosmetic improvement to code. Condensed a continued */
/*        statement into one line. */

/* -& */

/*     Get out immediately if tracing is disabled. */

    if (notrc) {
	return 0;
    }

/*     Get the position of the first and last non-blank characters in */
/*     input module name, and set the length of the module name. */

    if (*(unsigned char *)module != ' ') {
	first = 1;
    } else {
	first = frstnb_(module, module_len);
    }

/*     Check to see if the module name is blank. */

    if (first > 0) {

/*        If there is room for the name, place it at the top of the */
/*        stack. If not, increment the overflow counter and signal an */
/*        error. */

	if (modcnt < 100) {
	    ++modcnt;
	    s_copy(stack + (((i__1 = modcnt - 1) < 100 && 0 <= i__1 ? i__1 : 
		    s_rnge("stack", i__1, "trcpkg_", (ftnlen)812)) << 5), 
		    module + (first - 1), (ftnlen)32, module_len - (first - 1)
		    );
	} else {
	    ++ovrflw;
	    getdev_(device, (ftnlen)255);
	    wrline_(device, "SPICE(TRACEBACKOVERFLOW)", (ftnlen)255, (ftnlen)
		    24);
	    wrline_(device, "CHKIN:  The trace storage is completely full.  "
		    "No further module names can be added.", (ftnlen)255, (
		    ftnlen)84);
	}

/*        Keep track of the maximum depth encountered. */

	if (modcnt + ovrflw > maxdep) {
	    maxdep = modcnt + ovrflw;
	}
    } else {
	getdev_(device, (ftnlen)255);
	wrline_(device, "SPICE(BLANKMODULENAME)", (ftnlen)255, (ftnlen)22);
	wrline_(device, "CHKIN:  An attempt to check in was made without sup"
		"plying a module name.", (ftnlen)255, (ftnlen)72);
    }

/*     We're done now, so return. */

    return 0;
/* $Procedure CHKOUT ( Module Check Out ) */

L_chkout:
/* $ Abstract */

/*     Inform the SPICELIB error handling mechanism of exit from a */
/*     routine. */

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

/*     ERROR */

/* $ Keywords */

/*     ERROR */

/* $ Declarations */

/*     IMPLICIT NONE */

/*     CHARACTER*(*)        MODULE */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     MODULE     I   The name of the calling routine. */
/*     NAMLEN     P   Maximum module name length. */
/*     FILEN      P   Maximum file name length. */

/* $ Detailed_Input */

/*     MODULE   is the name of the routine calling CHKOUT. The */
/*              named routine is supposed to be `checking out' */
/*              when it calls CHKOUT; that is, the call should be */
/*              the last executable statement preceding any exit */
/*              from the routine. */

/*              Only the first NAMLEN non-blank characters in */
/*              a module name are used when checking out. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     FILEN    is the maximum file name length that can be */
/*              accommodated by this routine. */

/*     NAMLEN   is the maximum module name length that can be */
/*              accommodated by this routine. */

/* $ Exceptions */

/*     CHKOUT does not signal errors; rather it writes error messages, */
/*     so as to avoid recursion. */

/*     1)  If the input module name MODULE does not match the name popped */
/*         from the trace stack, the short error message */
/*         SPICE(NAMESDONOTMATCH) is written to the error output device. */

/*     2)  If the trace stack is empty, the short error message */
/*         SPICE(TRACESTACKEMPTY) is written to the error output device. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine is part of the SPICELIB error handling mechanism. */

/*     Conceptually, the effect of this routine is to `pop' a module */
/*     name from a stack. The routine CHKIN performs the inverse, or */
/*     `push' operation. */

/*     Every routine that participates in the traceback scheme should */
/*     have a call to CHKIN as the second executable statement. */
/*     The first executable statements should be: */

/*        IF ( RETURN() ) THEN */
/*           RETURN */
/*        ELSE */
/*           CALL CHKIN ( module ) */
/*        END IF */

/*     Here module is the name of the routine in which this code appears. */

/*     The line of code preceding the END or any RETURN statement */
/*     should be */

/*        CALL CHKOUT  ( module ) */

/*     All SPICELIB routines should call CHKIN and CHKOUT, unless they */
/*     are classified as `error free'. Programs linked with SPICELIB */
/*     may also use CHKIN and CHKOUT. */

/*     Routines that don't call CHKIN and CHKOUT won't appear in the */
/*     traceback. */

/*     All routines that call CHKIN must also call CHKOUT, or else the */
/*     trace mechanism will become very confused. */

/*     It is possible to disable check-ins (and check-outs) by calling */
/*     the entry point TRCOFF. CHKIN and CHKOUT will return immediately */
/*     upon entry after TRCOFF has been called. It is not possible to */
/*     re-enable check-ins and check-outs after calling TRCOFF. Routines */
/*     that don't call CHKIN and CHKOUT won't appear in the traceback. */

/* $ Examples */

/*     1)  Call CHKOUT before a RETURN statement: */

/*            IF ( FAILED() ) THEN */
/*               CALL CHKOUT ( module ) */
/*               RETURN */
/*            END IF */


/*     2)  Call CHKOUT before an END statement: */

/*            CALL CHKOUT ( module ) */
/*            END */


/*     3)  Only ONE call to CHKOUT is needed here: */

/*            CALL CHKOUT ( module ) */
/*            RETURN */
/*            END */

/* $ Restrictions */

/*     1)  Routines that call this routine must call CHKIN as the second */
/*         executable statement. (The first is a call to RETURN). */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 4.2.0, 03-JUN-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 4.1.0, 29-JUL-2013 (BVS) */

/*        Changed logic to check if the first character of the input */
/*        value is not a space and, if so, bypass the call to FRSTNB. */
/*        This change speeds up the execution by ~20%. */

/* -    SPICELIB Version 4.0.5, 17-JUL-2002 (BVS) */

/*        Added MAC-OSX environments. */

/* -    SPICELIB Version 4.0.4, 08-OCT-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given. New */
/*        environments are WIN-NT */

/* -    SPICELIB Version 4.0.3, 24-SEP-1999 (NJB) */

/*        CSPICE environments were added. Some typos were corrected. */

/* -    SPICELIB Version 4.0.2, 28-JUL-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given. New */
/*        environments are PC-DIGITAL, SGI-O32 and SGI-N32. */

/* -    SPICELIB Version 4.0.1, 18-MAR-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given. Previously, */
/*        environments such as SUN-SUNOS and SUN-SOLARIS were implied */
/*        by the environment label SUN. */

/* -    SPICELIB Version 4.0.0, 30-OCT-1997 (NJB) */

/*        Module was updated for the PC-LINUX platform. */

/*        Bug fix: The previous version failed to make a correct */
/*        module name comparison when the input name exceeded NAMLEN */
/*        characters in length. Now only the initial NAMLEN non-blank */
/*        characters (at most) of the input name are used in the */
/*        comparison. */

/* -    SPICELIB Version 3.0.0, 12-MAR-1996 (KRG) */

/*        The structure of this routine has completely changed. A stack, */
/*        implemented as an array of character strings, is now used to */
/*        store subroutine names that use the CHKIN and CHKOUT entry */
/*        points. This change simplified the individual entry points as */
/*        well as speeding up the process of checking in and checking */
/*        out. */

/* -    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 2.0.0, 15-JUN-1990 (NJB) */

/*        Disabling of check-ins implemented. Many parts of the */
/*        header have be re-written. Weird spacing ameliorated. */
/*        Removed a bug check. Short error messages made more */
/*        specific. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (NJB) */

/* -& */
/* $ Index_Entries */

/*     module check out */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 4.0.0, 30-OCT-1997 (NJB) */

/*        Module was updated for the PC-LINUX platform. */

/*        Bug fix: The previous version failed to make a correct */
/*        module name comparison when the input name exceeded NAMLEN */
/*        characters in length. Now only the initial NAMLEN non-blank */
/*        characters (at most) of the input name are used in the */
/*        comparison. */

/* -    SPICELIB Version 3.0.0, 12-MAR-1996 (KRG) */

/*        The structure of this routine has completely changed. A stack, */
/*        implemented as an array of character strings, is now used to */
/*        store subroutine names that use the CHKIN and CHKOUT entry */
/*        points. This change simplified the individual entry points as */
/*        well as speeding up the process of checking in and checking */
/*        out. */

/* -    SPICELIB Version 2.0.0, 15-JUN-1990 (NJB) */

/*        Disabling of check-ins implemented. Many parts of the */
/*        header have be re-written. Weird spacing ameliorated. */
/*        Removed a bug check. Short error messages changed from */
/*        SPICE(INVALIDCHECKOUT) to SPICE(NAMESDONOTMATCH) and */
/*        SPICE(TRACESTACKEMPTY). */

/* -    Beta Version 1.1.1, 10-FEB-1988 (NJB) */

/*        Parameter declarations documented. $Parameters section added, */
/*        and parameter declarations listed in `Brief I/O'. */

/* -    Beta Version 1.1.0, 27-OCT-1988 (NJB) */

/*        Cosmetic improvement to code. Removed a blank line */
/*        separating the first line of a statement from the next */
/*        continuation line, and condensed and re-organized */
/*        the statement. Note:  the precompiler failed to properly */
/*        convert the original statement into standard FORTRAN. */

/* -& */

/*     Get out immediately if tracing is disabled. */

    if (notrc) {
	return 0;
    }

/*     Check to be sure we can remove a module name from the stack, */
/*     i.e., that we have not overflowed. */

    if (ovrflw == 0) {

/*        We are not in overflow mode, compare the module name on */
/*        the top of the stack with the module name passed to us. If */
/*        they differ, it's an error. Regardless, we decrement the */
/*        module count. */

	if (modcnt > 0) {

/*           Make the comparison using at most NAMLEN characters of the */
/*           initial non-blank sub-string of MODULE. */

	    if (*(unsigned char *)module != ' ') {
		first = 1;
	    } else {
		first = frstnb_(module, module_len);
	    }
/* Computing MIN */
	    i__1 = i_len(module, module_len), i__2 = first + 31;
	    l = min(i__1,i__2);
	    if (s_cmp(stack + (((i__1 = modcnt - 1) < 100 && 0 <= i__1 ? i__1 
		    : s_rnge("stack", i__1, "trcpkg_", (ftnlen)1184)) << 5), 
		    module + (first - 1), (ftnlen)32, l - (first - 1)) != 0) {
		s_copy(tmpnam, module + (first - 1), (ftnlen)80, module_len - 
			(first - 1));
		getdev_(device, (ftnlen)255);
		wrline_(device, "SPICE(NAMESDONOTMATCH)", (ftnlen)255, (
			ftnlen)22);
/* Writing concatenation */
		i__3[0] = 19, a__1[0] = "CHKOUT:  Caller is ";
		i__3[1] = rtrim_(tmpnam, (ftnlen)80), a__1[1] = tmpnam;
		i__3[2] = 17, a__1[2] = "; popped name is ";
		i__3[3] = rtrim_(stack + (((i__2 = modcnt - 1) < 100 && 0 <= 
			i__2 ? i__2 : s_rnge("stack", i__2, "trcpkg_", (
			ftnlen)1189)) << 5), (ftnlen)32), a__1[3] = stack + ((
			(i__1 = modcnt - 1) < 100 && 0 <= i__1 ? i__1 : 
			s_rnge("stack", i__1, "trcpkg_", (ftnlen)1189)) << 5);
		i__3[4] = 1, a__1[4] = ".";
		s_cat(ch__1, a__1, i__3, &c__5, (ftnlen)149);
		wrline_(device, ch__1, (ftnlen)255, rtrim_(tmpnam, (ftnlen)80)
			 + 36 + rtrim_(stack + (((i__2 = modcnt - 1) < 100 && 
			0 <= i__2 ? i__2 : s_rnge("stack", i__2, "trcpkg_", (
			ftnlen)1189)) << 5), (ftnlen)32) + 1);
	    }
	    --modcnt;
	} else {
	    getdev_(device, (ftnlen)255);
	    wrline_(device, "SPICE(TRACESTACKEMPTY)", (ftnlen)255, (ftnlen)22)
		    ;
	    wrline_(device, "CHKOUT: An attempt to check out was made when n"
		    "o modules were checked in.", (ftnlen)255, (ftnlen)73);
	}
    } else {

/*        Overflow case: just decrement the overflow count. */

	--ovrflw;
    }

/*     Return to the caller. */

    return 0;
/* $Procedure TRCDEP ( Traceback depth ) */

L_trcdep:
/* $ Abstract */

/*     Return the number of modules in the traceback representation. */

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

/*     ERROR */

/* $ Keywords */

/*     ERROR */

/* $ Declarations */

/*     IMPLICIT NONE */

/*     INTEGER               DEPTH */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  --------------------------------------------------- */
/*     DEPTH      O   The number of modules in the traceback. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     DEPTH    is the number of module names in the traceback */
/*              representation. */

/*              The module names represent modules in a call chain, */
/*              with the first name being the top-level module, */
/*              and the name with index DEPTH being the lowest */
/*              level module. */

/*              The meaning of the traceback depends on the state */
/*              of the error handling mechanism. There are two */
/*              cases: */

/*                 1. In 'RETURN' mode, when an error is */
/*                     signaled, the traceback at that point is */
/*                     saved.  TRCDEP, TRCNAM, and QCKTRC will */
/*                     return values pertaining to the saved */
/*                     traceback. */

/*                 2. In all other modes, the traceback represents */
/*                     the CURRENT call chain.  TRCDEP, TRCNAM, */
/*                     and QCKTRC will return values pertaining to */
/*                     the current trace representation. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine is part of the SPICELIB error handling mechanism. */

/* $ Examples */

/*     1)  You can use this routine, together with TRCNAM, to create a */
/*         traceback report. We might wish to create such a report when */
/*         we have detected an error condition (see FAILED). */

/*         In this example, we assume that the error has already been */
/*         detected, and that we wish to create a traceback report. We */
/*         assume the existence of two user-supplied routines: */

/*            USER_TRACE_FORMAT   --   creates a traceback report in the */
/*                                     format preferred by the user */

/*            USER_TRACE_INIT     --   indicates that a traceback report */
/*                                     is to be created; it also */
/*                                     indicates how many module names */
/*                                     will be in the report */

/*            C */
/*            C     Get the trace depth, and retrieve that number of */
/*            C     module names from the traceback representation. */
/*            C     Call USER_TRACE_INIT to indicate that a traceback */
/*            C     report is to be created containing `DEPTH' */
/*            C     number of module names. Input each of these names, */
/*            C     as they are retrieved, to USER_TRACE_FORMAT. */
/*            C */

/*                 CALL TRCDEP           ( DEPTH ) */

/*                 CALL USER_TRACE_INIT  ( DEPTH ) */


/*                 DO INDEX = 1, DEPTH */

/*                    CALL TRCNAM     ( INDEX, MODULE ) */

/*                    CALL USER_TRACE_FORMAT ( MODULE ) */

/*                 END DO */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 4.1.0, 03-JUN-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 4.0.5, 17-JUL-2002 (BVS) */

/*        Added MAC-OSX environments. */

/* -    SPICELIB Version 4.0.4, 08-OCT-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given. New */
/*        environments are WIN-NT */

/* -    SPICELIB Version 4.0.3, 24-SEP-1999 (NJB) */

/*        CSPICE environments were added. Some typos were corrected. */

/* -    SPICELIB Version 4.0.2, 28-JUL-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given. New */
/*        environments are PC-DIGITAL, SGI-O32 and SGI-N32. */

/* -    SPICELIB Version 4.0.1, 18-MAR-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given. Previously, */
/*        environments such as SUN-SUNOS and SUN-SOLARIS were implied */
/*        by the environment label SUN. */

/* -    SPICELIB Version 2.0.0, 12-MAR-1996 (KRG) */

/*        The structure of this routine has completely changed. A stack, */
/*        implemented as an array of character strings, is now used to */
/*        store subroutine names that use the CHKIN and CHKOUT entry */
/*        points. This change simplified the individual entry points as */
/*        well as speeding up the process of checking in and checking */
/*        out. */

/*        The error action mechanism has been changed as well. GETACT */
/*        now uses an integer code rather than a short character */
/*        string to represent the error action. The entry points affected */
/*        by this change are: TRCDEP, TRCNAM, QCKTRC. */

/* -    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.1, 15-JUN-1990 (NJB) */

/*        Some comments updated. Some cosmetic changes too. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (NJB) */

/* -& */
/* $ Index_Entries */

/*     traceback depth */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 2.0.0, 12-MAR-1996 (KRG) */

/*        The structure of this routine has completely changed. A stack, */
/*        implemented as an array of character strings, is now used to */
/*        store subroutine names that use the CHKIN and CHKOUT entry */
/*        points. This change simplified the individual entry points as */
/*        well as speeding up the process of checking in and checking */
/*        out. */

/*        The error action mechanism has been changed as well. GETACT */
/*        now uses an integer code rather than a short character */
/*        string to represent the error action. The entry points affected */
/*        by this change are: TRCDEP, TRCNAM, QCKTRC. */

/* -    SPICELIB Version 1.0.1, 15-JUN-1990  (NJB) */

/*        Some comments updated. Some cosmetic changes too. */

/* -& */

/*     Find the error handling mode. */

    getact_(&action);

/*     If we're in 'RETURN' mode, and an error has occurred, we want to */
/*     use the frozen version of the traceback.  Otherwise, we want to */
/*     get the use the current module stack depth. */

    if (action == 3 && failed_()) {
	*depth = frzcnt + frzovr;
    } else {
	*depth = modcnt + ovrflw;
    }

/*     Return to the caller. */

    return 0;
/* $Procedure TRCMXD ( Maximum traceback depth encountered. ) */

L_trcmxd:
/* $ Abstract */

/*     Return the maximum number of modules encountered in the */
/*     traceback so far. */

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

/*     ERROR */

/* $ Keywords */

/*     ERROR */

/* $ Declarations */

/*     IMPLICIT NONE */

/*     INTEGER               DEPTH */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  --------------------------------------------------- */
/*     DEPTH      O   The maximum number of modules encountered. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     DEPTH    is the maximum number of module names encountered in the */
/*              traceback stack. This would be the longest call chain */
/*              that occurred during the run of a program. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine is part of the SPICELIB error handling mechanism. */

/* $ Examples */

/*     1)  You can use this routine to determine the length of the */
/*         longest sequence of subroutine calls in a program. Suppose */
/*         that you have a program, PROGRAM, that uses the SPICELIB */
/*         error handling with CHKIN and CHKOUT, and has three */
/*         subroutines, SUB_A, SUB_B, and SUB_C. The program and */
/*         subroutines have the following relationships: */

/*             PROGRAM calls SUB_A and SUB_C */
/*             SUB_C   calls SUB_B */

/*         If at the end of the program you were to call TRCMXD, */

/*            CALL TRCMXD ( MAXDEP ) */

/*         to obtain the maximum depth reached, MAXDEP would have a */
/*         value of three (3), because the program checked in, SUB_C */
/*         checked in, and SUB_B checked in during the longest call */
/*         chain in the program. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 4.1.0, 03-JUN-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 4.0.5, 17-JUL-2002 (BVS) */

/*        Added MAC-OSX environments. */

/* -    SPICELIB Version 4.0.4, 08-OCT-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given. New */
/*        environments are WIN-NT */

/* -    SPICELIB Version 4.0.3, 24-SEP-1999 (NJB) */

/*        CSPICE environments were added. Some typos were corrected. */

/* -    SPICELIB Version 4.0.2, 28-JUL-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given. New */
/*        environments are PC-DIGITAL, SGI-O32 and SGI-N32. */

/* -    SPICELIB Version 4.0.1, 18-MAR-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given. Previously, */
/*        environments such as SUN-SUNOS and SUN-SOLARIS were implied */
/*        by the environment label SUN. */

/* -    SPICELIB Version 1.0.0, 12-MAR-1996 (KRG) */

/* -& */
/* $ Index_Entries */

/*     traceback maximum depth */

/* -& */

/*     It doesn't get any easier than this, simply set the maximum */
/*     depth and return. */

    *depth = maxdep;
    return 0;
/* $Procedure TRCNAM ( Get Module Name from Traceback ) */

L_trcnam:
/* $ Abstract */

/*     Return the name of the module having the specified position in */
/*     the trace representation. The first module to check in is at */
/*     position 1. */

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

/*     ERROR */

/* $ Keywords */

/*     ERROR */

/* $ Declarations */

/*     IMPLICIT NONE */

/*     INTEGER               INDEX */
/*     CHARACTER*(*)         NAME */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     INDEX      I   The position of the requested module name. */
/*     NAME       O   The name in the #INDEX position in the traceback. */
/*     FILEN      P   Maximum file name length. */

/* $ Detailed_Input */

/*     INDEX    is the position in the traceback of the requested */
/*              module name. The first module to check in is in */
/*              the first position; the last to check in the */
/*              position indicated by the argument, DEPTH, */
/*              returned by TRCDEP. Note that the first module to */
/*              check in is at the top of the traced call chain. */

/* $ Detailed_Output */

/*     NAME     is the name of the module in the position within */
/*              the traceback indicated by INDEX. */

/*              The meaning of the traceback depends on the state */
/*              of the error handling mechanism. There are two */
/*              cases: */

/*                 1. In 'RETURN' mode, when an error is */
/*                     signaled, the traceback at that point is */
/*                     saved.  TRCDEP, TRCNAM, and QCKTRC will */
/*                     return values pertaining to the saved */
/*                     traceback. */

/*                 2. In all other modes, the traceback represents */
/*                     the CURRENT call chain.  TRCDEP, TRCNAM, */
/*                     and QCKTRC will return values pertaining to */
/*                     the current trace representation. */

/* $ Parameters */

/*     FILEN    is the maximum file name length that can be */
/*              accommodated by this routine. */

/* $ Exceptions */

/*     Because this routine is below SIGERR in the calling hierarchy, */
/*     this routine can not call SIGERR in the event of an error. */
/*     Therefore, this routine outputs error messages, rather than */
/*     signaling errors. */

/*     1)  This routine detects the condition of INDEX being out of */
/*         range. The short error message set in that case is */
/*         SPICE(INVALIDINDEX). */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine is part of the SPICELIB error handling mechanism. */

/* $ Examples */

/*     1)  You can use this routine, together with TRCNAM, to create a */
/*         traceback report. We might wish to create such a report when */
/*         we have detected an error condition (see FAILED). */

/*         In this example, we assume that the error has already been */
/*         detected, and that we wish to create a traceback report. We */
/*         assume the existence of two user-supplied routines: */

/*            USER_TRACE_FORMAT   --   creates a traceback report in the */
/*                                     format preferred by the user */

/*            USER_TRACE_INIT     --   indicates that a traceback report */
/*                                     is to be created; it also */
/*                                     indicates how many module names */
/*                                     will be in the report */

/*            C */
/*            C     Get the trace depth, and retrieve that number of */
/*            C     module names from the traceback representation. */
/*            C     Call USER_TRACE_INIT to indicate that a traceback */
/*            C     report is to be created containing `DEPTH' */
/*            C     number of module names. Input each of these names, */
/*            C     as they are retrieved, to USER_TRACE_FORMAT. */
/*            C */

/*                 CALL TRCDEP           ( DEPTH ) */

/*                 CALL USER_TRACE_INIT  ( DEPTH ) */


/*                 DO INDEX = 1, DEPTH */

/*                    CALL TRCNAM     ( INDEX, MODULE ) */

/*                    CALL USER_TRACE_FORMAT ( MODULE ) */

/*                 END DO */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 4.1.0, 03-JUN-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 4.0.5, 17-JUL-2002 (BVS) */

/*        Added MAC-OSX environments. */

/* -    SPICELIB Version 4.0.4, 08-OCT-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given. New */
/*        environments are WIN-NT */

/* -    SPICELIB Version 4.0.3, 24-SEP-1999 (NJB) */

/*        CSPICE environments were added. Some typos were corrected. */

/* -    SPICELIB Version 4.0.2, 28-JUL-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given. New */
/*        environments are PC-DIGITAL, SGI-O32 and SGI-N32. */

/* -    SPICELIB Version 4.0.1, 18-MAR-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given. Previously, */
/*        environments such as SUN-SUNOS and SUN-SOLARIS were implied */
/*        by the environment label SUN. */

/* -    SPICELIB Version 2.0.0, 12-MAR-1996 (KRG) */

/*        The structure of this routine has completely changed. A stack, */
/*        implemented as an array of character strings, is now used to */
/*        store subroutine names that use the CHKIN and CHKOUT entry */
/*        points. This change simplified the individual entry points as */
/*        well as speeding up the process of checking in and checking */
/*        out. */

/*        The exception: */

/*           2)  If INDEX is in range, but no module name is found */
/*               at the indicated location in the trace representation, */
/*               the error message SPICE(INVALIDINDEX) is set. */

/*        has been removed. The only way in which a module name cannot */
/*        be found for a specified index is if we have overflowed the */
/*        stack storage for module names, and in this case we return the */
/*        message '<Name Not Available>'. */

/*        The error action mechanism has been changed as well. GETACT */
/*        now uses an integer code rather than a short character */
/*        string to represent the error action. The entry points affected */
/*        by this change are: TRCDEP, TRCNAM, QCKTRC. */

/* -    SPICELIB Version 1.1.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.1.0, 15-JUN-1990 (NJB) */

/*        Error messages streamlined. Some comments updated. */
/*        Some cosmetic changes too. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (NJB) */

/* -& */
/* $ Index_Entries */

/*     get module name from traceback */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 2.0.0, 12-MAR-1996 (KRG) */

/*        The structure of this routine has completely changed. A stack, */
/*        implemented as an array of character strings, is now used to */
/*        store subroutine names that use the CHKIN and CHKOUT entry */
/*        points. This change simplified the individual entry points as */
/*        well as speeding up the process of checking in and checking */
/*        out. */

/*        The exception: */

/*           2)  If INDEX is in range, but no module name is found */
/*               at the indicated location in the trace representation, */
/*               the error message SPICE(INVALIDINDEX) is set. */

/*        has been removed. The only way in which a module name cannot */
/*        be found for a specified index is if we have overflowed the */
/*        stack storage for module names, and in this case we return the */
/*        message '<Name Not Available>'. */

/*        The error action mechanism has been changed as well. GETACT */
/*        now uses an integer code rather than a short character */
/*        string to represent the error action. The entry points affected */
/*        by this change are: TRCDEP, TRCNAM, QCKTRC. */

/* -    SPICELIB Version 1.1.0, 15-JUN-1990  (NJB) */

/*        Error messages streamlined. Some comments updated. */
/*        Some cosmetic changes too. */

/* -    Beta Version 1.1.1, 10-FEB-1988  (NJB) */

/*        Parameter declarations documented. $Parameters section added, */
/*        and parameter declarations listed in `Brief I/O'. */

/* -    Beta Version 1.1.0, 27-OCT-1988  (NJB) */

/*        Added test for failure to remove name from trace */
/*        representation. If LOC equals 0 on return from */
/*        NTHWD, the error SPICE(INVALIDINDEX) is reported. */
/*        SIGERR is not called; that would be overly recursive. */

/*        Cosmetic changes to header and code were made. Indentation */
/*        of some header items was changed, and some blank lines */
/*        were removed from the code. */

/* -& */

/*     Get the error handling mode. */

    getact_(&action);

/*     If we're in 'RETURN' mode, and an error has occurred, we want to */
/*     use the frozen version of the traceback.  Otherwise, we want to */
/*     get the module name from the current traceback. */

    if (action == 3 && failed_()) {

/*        Check the input index. It must be positive and less than the */
/*        current stack depth. */

	if (*index <= 0 || *index > frzcnt + frzovr) {

/*           Invalid index...we output the error messages directly */
/*           in this case: */

	    getdev_(device, (ftnlen)255);
	    wrline_(device, "SPICE(INVALIDINDEX)", (ftnlen)255, (ftnlen)19);
	    intstr_(index, string, (ftnlen)11);
/* Writing concatenation */
	    i__4[0] = 52, a__2[0] = "TRCNAM: An invalid index was input.  Th"
		    "e value was: ";
	    i__4[1] = rtrim_(string, (ftnlen)11), a__2[1] = string;
	    i__4[2] = 1, a__2[2] = ".";
	    s_cat(ch__2, a__2, i__4, &c__3, (ftnlen)64);
	    wrline_(device, ch__2, (ftnlen)255, rtrim_(string, (ftnlen)11) + 
		    53);
	    return 0;
	}

/*        We're OK, so get the name or not available. */

	if (*index <= 100) {
	    s_copy(name__, frozen + (((i__1 = *index - 1) < 100 && 0 <= i__1 ?
		     i__1 : s_rnge("frozen", i__1, "trcpkg_", (ftnlen)1998)) 
		    << 5), name_len, (ftnlen)32);
	} else {
	    s_copy(name__, "<Overflow No Name Available>", name_len, (ftnlen)
		    28);
	}
    } else {

/*        Otherwise, use current traceback: */

/*        Check the input index. It must be positive and less than the */
/*        current stack depth. */

	if (*index <= 0 || *index > modcnt + ovrflw) {

/*           Invalid index...we output the error messages directly */
/*           in this case: */

	    getdev_(device, (ftnlen)255);
	    wrline_(device, "SPICE(INVALIDINDEX)", (ftnlen)255, (ftnlen)19);
	    intstr_(index, string, (ftnlen)11);
/* Writing concatenation */
	    i__4[0] = 52, a__2[0] = "TRCNAM: An invalid index was input.  Th"
		    "e value was: ";
	    i__4[1] = rtrim_(string, (ftnlen)11), a__2[1] = string;
	    i__4[2] = 1, a__2[2] = ".";
	    s_cat(ch__2, a__2, i__4, &c__3, (ftnlen)64);
	    wrline_(device, ch__2, (ftnlen)255, rtrim_(string, (ftnlen)11) + 
		    53);
	    return 0;
	}

/*        We're OK, so get the name or name not available. */

	if (*index <= 100) {
	    s_copy(name__, stack + (((i__1 = *index - 1) < 100 && 0 <= i__1 ? 
		    i__1 : s_rnge("stack", i__1, "trcpkg_", (ftnlen)2028)) << 
		    5), name_len, (ftnlen)32);
	} else {
	    s_copy(name__, "<Overflow No Name Available>", name_len, (ftnlen)
		    28);
	}
    }
    return 0;
/* $Procedure QCKTRC ( Get Quick Traceback ) */

L_qcktrc:
/* $ Abstract */

/*     Return a string containing a traceback. */

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

/*     ERROR */

/* $ Keywords */

/*     ERROR */

/* $ Declarations */

/*     IMPLICIT NONE */

/*     CHARACTER*(*)         TRACE */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     TRACE      O   A traceback report. */
/*     NAMLEN     P   Maximum module name length. */
/*     FILEN      P   Maximum file name length. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     TRACE    is a list of module names, delimited by the */
/*              string, ' -->'.  An example would be */

/*                 'SPUD -->SPAM -->FOOBAR'. */

/*              In general, the meaning of the trace is as */
/*              follows: */

/*              The first name in the list is the name of the first */
/*              module to check in (that hasn't yet checked out). */
/*              The last name is the name of the module at the end */
/*              of the call chain; this is the last module that */
/*              checked in. */

/*              The meaning of the traceback depends on the state */
/*              of the error handling mechanism.  There are two */
/*              cases: */

/*                 1.  In 'RETURN' mode, when an error is */
/*                     signaled, the traceback at that point is */
/*                     saved.  TRCDEP, TRCNAM, and QCKTRC will */
/*                     return values pertaining to the saved */
/*                     traceback. */

/*                 2. In all other modes, the traceback represents */
/*                     the CURRENT call chain.  TRCDEP, TRCNAM, */
/*                     and QCKTRC will return values pertaining to */
/*                     the current trace representation. */

/*              Any module names exceeding NAMLEN characters in */
/*              length are truncated on the right. */

/* $ Parameters */

/*     FILEN    is the maximum file name length that can be */
/*              accommodated by this routine. */

/*     NAMLEN   is the maximum module name length that can be */
/*              accommodated by this routine. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine is part of the SPICELIB error handling mechanism. */

/* $ Examples */

/*     1)  Here's an example of how to use this routine: */

/*           C */
/*           C     We call RDTEXT and test for an error condition. */
/*           C     If an error occurred, we get the traceback and */
/*           C     long error message and output them using the */
/*           C     user-defined routine, USER_ERROR. */
/*           C */

/*                 CALL RDTEXT ( FILE, LINE, EOF ) */

/*                 IF ( FAILED() ) THEN */

/*                    CALL QCKTRC     ( TRACE ) */
/*                    CALL USER_ERROR ( TRACE ) */

/*                    CALL GETMSG     ( 'LONG', MSG ) */
/*                    CALL USER_ERROR (         MSG ) */

/*                 END IF */

/* $ Restrictions */

/*     1)  It is assumed no module names exceed NAMLEN characters in */
/*         length. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 4.1.0, 03-JUN-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 4.0.5, 17-JUL-2002 (BVS) */

/*        Added MAC-OSX environments. */

/* -    SPICELIB Version 4.0.4, 08-OCT-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given. New */
/*        environments are WIN-NT */

/* -    SPICELIB Version 4.0.3, 24-SEP-1999 (NJB) */

/*        CSPICE environments were added. Some typos were corrected. */

/* -    SPICELIB Version 4.0.2, 28-JUL-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given. New */
/*        environments are PC-DIGITAL, SGI-O32 and SGI-N32. */

/* -    SPICELIB Version 4.0.1, 18-MAR-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given. Previously, */
/*        environments such as SUN-SUNOS and SUN-SOLARIS were implied */
/*        by the environment label SUN. */

/* -    SPICELIB Version 2.0.0, 12-MAR-1996 (KRG) */

/*        The structure of this routine has completely changed. A stack, */
/*        implemented as an array of character strings, is now used to */
/*        store subroutine names that use the CHKIN and CHKOUT entry */
/*        points. This change simplified the individual entry points as */
/*        well as speeding up the process of checking in and checking */
/*        out. */

/*        The error action mechanism has been changed as well. GETACT */
/*        now uses an integer code rather than a short character */
/*        string to represent the error action. The entry points affected */
/*        by this change are: TRCDEP, TRCNAM, QCKTRC. */

/* -    SPICELIB Version 1.2.0, 23-OCT-1992 (NJB) */

/*        Bug fix made to routine QCKTRC:  a section of code which */
/*        itself is exercised only if a bug is present inserted the */
/*        wrong variable into an error message. */

/* -    SPICELIB Version 1.1.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.1.0, 15-JUN-1990 (NJB) */

/*        Error messages streamlined. Some comments updated. */
/*        Some cosmetic changes too. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (NJB) */

/* -& */
/* $ Index_Entries */

/*     get quick traceback */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 2.0.0, 12-MAR-1996 (KRG) */

/*        The structure of this routine has completely changed. A stack, */
/*        implemented as an array of character strings, is now used to */
/*        store subroutine names that use the CHKIN and CHKOUT entry */
/*        points. This change simplified the individual entry points as */
/*        well as speeding up the process of checking in and checking */
/*        out. */

/*        The error action mechanism has been changed as well. GETACT */
/*        now uses an integer code rather than a short character */
/*        string to represent the error action. The entry points affected */
/*        by this change are: TRCDEP, TRCNAM, QCKTRC. */

/* -    SPICELIB Version 1.2.0, 23-OCT-1992 (NJB) */

/*        Bug fix made to routine QCKTRC:  a section of code which */
/*        itself is exercised only if a bug is present inserted the */
/*        wrong variable into an error message. The variable in */
/*        question was the input argument INDEX; the correct variable */
/*        to insert in the message is the local variable POS. */

/* -    SPICELIB Version 1.1.0, 15-JUN-1990  (NJB) */

/*        Error messages streamlined. Some comments updated. */
/*        Some cosmetic changes too. Use of SUFFIX made more */
/*        rational. */

/* -    Beta Version 1.1.1, 10-FEB-1988  (NJB) */

/*        Parameter declarations documented. $Parameters section added, */
/*        and parameter declarations listed in `Brief I/O'. */

/* -    Beta Version 1.1.0, 06-OCT-1988  (NJB) */

/*        Added test for failure to remove name from trace */
/*        representation. If LOC equals 0 on return from */
/*        NTHWD, the error SPICE(INVALIDINDEX) is reported. */
/*        SIGERR is not called; that would be overly recursive. */

/*        Also, some cosmetic changes to code were made. Some */
/*        unnecessary continuation lines were removed. */

/* -& */

/*     Be sure that the output string is empty. */

    s_copy(trace, " ", trace_len, (ftnlen)1);

/*     Get the error handling mode. */

    getact_(&action);

/*     If we're in 'RETURN' mode, and an error has occurred, we want to */
/*     use the frozen version of the traceback.  Otherwise, we want to */
/*     use the current traceback. */

    if (action == 3 && failed_()) {
	i__1 = frzcnt;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    if (i__ > 1) {
		suffix_("-->", &c__1, trace, (ftnlen)3, trace_len);
		suffix_(frozen + (((i__2 = i__ - 1) < 100 && 0 <= i__2 ? i__2 
			: s_rnge("frozen", i__2, "trcpkg_", (ftnlen)2335)) << 
			5), &c__1, trace, (ftnlen)32, trace_len);
	    } else {
		suffix_(frozen + (((i__2 = i__ - 1) < 100 && 0 <= i__2 ? i__2 
			: s_rnge("frozen", i__2, "trcpkg_", (ftnlen)2337)) << 
			5), &c__0, trace, (ftnlen)32, trace_len);
	    }
	}
	if (frzovr > 0) {
	    suffix_("-->", &c__1, trace, (ftnlen)3, trace_len);
	    if (frzovr > 1) {
		intstr_(&frzovr, string, (ftnlen)11);
		suffix_("<", &c__1, trace, (ftnlen)1, trace_len);
		suffix_(string, &c__0, trace, (ftnlen)11, trace_len);
		suffix_("Names Overflowed>", &c__1, trace, (ftnlen)17, 
			trace_len);
	    } else {
		suffix_("<One Name Overflowed>", &c__1, trace, (ftnlen)21, 
			trace_len);
	    }
	}
    } else {
	i__1 = modcnt;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    if (i__ > 1) {
		suffix_("-->", &c__1, trace, (ftnlen)3, trace_len);
		suffix_(stack + (((i__2 = i__ - 1) < 100 && 0 <= i__2 ? i__2 :
			 s_rnge("stack", i__2, "trcpkg_", (ftnlen)2362)) << 5)
			, &c__1, trace, (ftnlen)32, trace_len);
	    } else {
		suffix_(stack + (((i__2 = i__ - 1) < 100 && 0 <= i__2 ? i__2 :
			 s_rnge("stack", i__2, "trcpkg_", (ftnlen)2364)) << 5)
			, &c__0, trace, (ftnlen)32, trace_len);
	    }
	}
	if (ovrflw > 0) {
	    suffix_("-->", &c__1, trace, (ftnlen)3, trace_len);
	    if (ovrflw > 1) {
		intstr_(&ovrflw, string, (ftnlen)11);
		suffix_("<", &c__1, trace, (ftnlen)1, trace_len);
		suffix_(string, &c__0, trace, (ftnlen)11, trace_len);
		suffix_("Names Overflowed>", &c__1, trace, (ftnlen)17, 
			trace_len);
	    } else {
		suffix_("<One Name Overflowed>", &c__1, trace, (ftnlen)21, 
			trace_len);
	    }
	}
    }
    return 0;
/* $Procedure FREEZE ( Get frozen copy of traceback ) */

L_freeze:
/* $ Abstract */

/*     Make a copy of the current traceback. This copy is frozen, i.e. */
/*     unchanged, until the next call to FREEZE. DO NOT CALL THIS */
/*     ROUTINE. */

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

/*     ERROR */

/* $ Keywords */

/*     ERROR */

/* $ Declarations */

/*     IMPLICIT NONE */

/* $ Brief_I/O */

/*     None. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     DO NOT CALL THIS ROUTINE. */

/*     When the error response action is 'RETURN', and an error is */
/*     signaled, a copy of the traceback is saved for later retrieval */
/*     by the application program.  This is called the `frozen' version */
/*     of the traceback. FREEZE is used to create this frozen version. */

/*     This routine is called by the SPICELIB routines SIGERR and RESET. */

/* $ Examples */

/*     1) */
/*         C */
/*         C     Create a frozen traceback: */
/*         C */
/*               CALL FREEZE */

/* $ Restrictions */

/*     1)  For SPICELIB error handling only. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 4.1.0, 03-JUN-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Added entry to */
/*        $Index_Entries. */

/* -    SPICELIB Version 4.0.5, 17-JUL-2002 (BVS) */

/*        Added MAC-OSX environments. */

/* -    SPICELIB Version 4.0.4, 08-OCT-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given. New */
/*        environments are WIN-NT */

/* -    SPICELIB Version 4.0.3, 24-SEP-1999 (NJB) */

/*        CSPICE environments were added. Some typos were corrected. */

/* -    SPICELIB Version 4.0.2, 28-JUL-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given. New */
/*        environments are PC-DIGITAL, SGI-O32 and SGI-N32. */

/* -    SPICELIB Version 4.0.1, 18-MAR-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given. Previously, */
/*        environments such as SUN-SUNOS and SUN-SOLARIS were implied */
/*        by the environment label SUN. */

/* -    SPICELIB Version 2.0.0, 12-MAR-1996 (KRG) */

/*        The structure of this routine has completely changed. A stack, */
/*        implemented as an array of character strings, is now used to */
/*        store subroutine names that use the CHKIN and CHKOUT entry */
/*        points. This change simplified the individual entry points as */
/*        well as speeding up the process of checking in and checking */
/*        out. */

/* -    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.1, 15-JUN-1990 (NJB) */

/*        Some comments changed. Cosmetic changes too. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (NJB) */

/* -& */
/* $ Index_Entries */

/*     get frozen copy of traceback */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 2.0.0, 12-MAR-1996 (KRG) */

/*        The structure of this routine has completely changed. A stack, */
/*        implemented as an array of character strings, is now used to */
/*        store subroutine names that use the CHKIN and CHKOUT entry */
/*        points. This change simplified the individual entry points as */
/*        well as speeding up the process of checking in and checking */
/*        out. */

/* -    SPICELIB Version 1.0.1, 15-JUN-1990  (NJB) */

/*       Some comments changed. Cosmetic changes too. */

/* -    Beta Version 1.0.1, 08-FEB-1989 (NJB) */

/*        Warnings added to discourage use of this routine in */
/*        non-error-handling code. */

/* -& */

/*     Create a frozen version of the traceback. To do this, we move */
/*     the current traceback state into the freezer.. */

    frzcnt = modcnt;
    frzovr = ovrflw;
    i__1 = modcnt;
    for (i__ = 1; i__ <= i__1; ++i__) {
	s_copy(frozen + (((i__2 = i__ - 1) < 100 && 0 <= i__2 ? i__2 : s_rnge(
		"frozen", i__2, "trcpkg_", (ftnlen)2592)) << 5), stack + (((
		i__5 = i__ - 1) < 100 && 0 <= i__5 ? i__5 : s_rnge("stack", 
		i__5, "trcpkg_", (ftnlen)2592)) << 5), (ftnlen)32, (ftnlen)32)
		;
    }
    return 0;
/* $Procedure TRCOFF ( Turn tracing off ) */

L_trcoff:
/* $ Abstract */

/*     Disable tracing. */

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

/*     ERROR */

/* $ Keywords */

/*     ERROR */

/* $ Declarations */

/*     IMPLICIT NONE */

/* $ Brief_I/O */

/*     None. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine disables tracing. Checking in or out does not modify */
/*     the current traceback any further after TRCOFF is called. The */
/*     routines TRCNAM, TRCDEP, and QCKTRC will return information */
/*     based on the traceback at the point where TRCOFF is called. */

/*     Once tracing has been disabled, it cannot be re-enabled. */

/*     Additionally, TRCOFF blanks out the existing trace, since the */
/*     trace will usually be invalid at the time an error is signaled. */
/*     The frozen copy of the trace, if there is one, is not modified. */

/* $ Examples */

/*     1)    C */
/*           C     Program initialization: */
/*           C */
/*                      . */
/*                      . */
/*                      . */
/*           C */
/*           C     We disable tracing to enhance speed: */
/*           C */
/*                 CALL TRCOFF */
/*           C */
/*           C     More initialization code: */
/*           C */
/*                      . */
/*                      . */
/*                      . */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 4.1.0, 03-JUN-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 4.0.5, 17-JUL-2002 (BVS) */

/*        Added MAC-OSX environments. */

/* -    SPICELIB Version 4.0.4, 08-OCT-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given. New */
/*        environments are WIN-NT */

/* -    SPICELIB Version 4.0.3, 24-SEP-1999 (NJB) */

/*        CSPICE environments were added. Some typos were corrected. */

/* -    SPICELIB Version 4.0.2, 28-JUL-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given. New */
/*        environments are PC-DIGITAL, SGI-O32 and SGI-N32. */

/* -    SPICELIB Version 4.0.1, 18-MAR-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given. Previously, */
/*        environments such as SUN-SUNOS and SUN-SOLARIS were implied */
/*        by the environment label SUN. */

/* -    SPICELIB Version 2.0.0, 12-MAR-1996 (KRG) */

/*        The structure of this routine has completely changed. A stack, */
/*        implemented as an array of character strings, is now used to */
/*        store subroutine names that use the CHKIN and CHKOUT entry */
/*        points. This change simplified the individual entry points as */
/*        well as speeding up the process of checking in and checking */
/*        out. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 11-JUL-1990 (NJB) */

/* -& */
/* $ Index_Entries */

/*     turn tracing off */

/* -& */

/*     Indicate that tracing is disabled: */

    notrc = TRUE_;

/*     The stack depth becomes 0 (it will be referenced if TRCDEP is */
/*     called). The overflow count set to 0 as well, for consistency; */
/*     it will not be referenced again after this code is executed. */

    modcnt = 0;
    ovrflw = 0;
    return 0;
} /* trcpkg_ */

/* Subroutine */ int trcpkg_(integer *depth, integer *index, char *module, 
	char *trace, char *name__, ftnlen module_len, ftnlen trace_len, 
	ftnlen name_len)
{
    return trcpkg_0_(0, depth, index, module, trace, name__, module_len, 
	    trace_len, name_len);
    }

/* Subroutine */ int chkin_(char *module, ftnlen module_len)
{
    return trcpkg_0_(1, (integer *)0, (integer *)0, module, (char *)0, (char *
	    )0, module_len, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int chkout_(char *module, ftnlen module_len)
{
    return trcpkg_0_(2, (integer *)0, (integer *)0, module, (char *)0, (char *
	    )0, module_len, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int trcdep_(integer *depth)
{
    return trcpkg_0_(3, depth, (integer *)0, (char *)0, (char *)0, (char *)0, 
	    (ftnint)0, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int trcmxd_(integer *depth)
{
    return trcpkg_0_(4, depth, (integer *)0, (char *)0, (char *)0, (char *)0, 
	    (ftnint)0, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int trcnam_(integer *index, char *name__, ftnlen name_len)
{
    return trcpkg_0_(5, (integer *)0, index, (char *)0, (char *)0, name__, (
	    ftnint)0, (ftnint)0, name_len);
    }

/* Subroutine */ int qcktrc_(char *trace, ftnlen trace_len)
{
    return trcpkg_0_(6, (integer *)0, (integer *)0, (char *)0, trace, (char *)
	    0, (ftnint)0, trace_len, (ftnint)0);
    }

/* Subroutine */ int freeze_(void)
{
    return trcpkg_0_(7, (integer *)0, (integer *)0, (char *)0, (char *)0, (
	    char *)0, (ftnint)0, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int trcoff_(void)
{
    return trcpkg_0_(8, (integer *)0, (integer *)0, (char *)0, (char *)0, (
	    char *)0, (ftnint)0, (ftnint)0, (ftnint)0);
    }

