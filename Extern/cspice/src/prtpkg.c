/* prtpkg.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;

/* $Procedure PRTPKG ( Declare Arguments for Error Message Routines ) */
logical prtpkg_0_(int n__, logical *short__, logical *long__, logical *expl, 
	logical *trace, logical *dfault, char *type__, ftnlen type_len)
{
    /* Initialized data */

    static logical svshrt = TRUE_;
    static logical svexpl = TRUE_;
    static logical svlong = TRUE_;
    static logical svtrac = TRUE_;
    static logical svdflt = TRUE_;

    /* System generated locals */
    address a__1[2];
    integer i__1[2];
    logical ret_val;
    char ch__1[96];

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen), s_cat(char *,
	     char **, integer *, integer *, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int ucase_(char *, char *, ftnlen, ftnlen);
    char ltype[10];
    extern /* Subroutine */ int ljust_(char *, char *, ftnlen, ftnlen);
    char device[255];
    extern /* Subroutine */ int getdev_(char *, ftnlen), wrline_(char *, char 
	    *, ftnlen, ftnlen);
    char loctyp[10];

/* $ Abstract */

/*     Declare the arguments for the error message selection entry */
/*     points.  DO NOT CALL THIS ROUTINE. */

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
/*     SHORT      I   SETPRT */
/*     EXPL       I   SETPRT */
/*     LONG       I   SETPRT */
/*     TRACE      I   SETPRT */
/*     DFAULT     I   SETPRT */
/*     TYPE       I   MSGSEL */
/*     FILEN      P   MSGSEL */

/* $ Detailed_Input */

/*     See the ENTRY points for discussions of their arguments. */

/* $ Detailed_Output */

/*     See the ENTRY points for discussions of their arguments. */

/* $ Parameters */

/*     See the ENTRY points for discussions of their parameters. */

/* $ Exceptions */

/*     1)  If PRTPKG is called directly, the error SPICE(BOGUSENTRY) is */
/*         signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     DO NOT CALL THIS ROUTINE. */

/*     The entry points declared in this routine are: */

/*        SETPRT */
/*        MSGSEL */

/*     There is no reason to call this subroutine. The purpose of this */
/*     subroutine is to make the declarations required by the various */
/*     entry points. This routine has no run-time function. */

/* $ Examples */

/*     None.  DO NOT CALL THIS ROUTINE. */

/* $ Restrictions */

/*     1)  DO NOT CALL THIS ROUTINE. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     H.A. Neilan        (JPL) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 3.27.0, 28-NOV-2021 (BVS) */

/*        Updated for MAC-OSX-M1-64BIT-CLANG_C. */

/* -    SPICELIB Version 3.26.0, 17-JUN-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header of the PRTPKG umbrella routine and all its */
/*        entry entry points to comply with NAIF standard. */

/* -    SPICELIB Version 3.25.0, 10-MAR-2014 (BVS) */

/*        Updated for SUN-SOLARIS-64BIT-INTEL. */

/* -    SPICELIB Version 3.24.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-LINUX-64BIT-IFORT. */

/* -    SPICELIB Version 3.23.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-CYGWIN-GFORTRAN. */

/* -    SPICELIB Version 3.22.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-CYGWIN-64BIT-GFORTRAN. */

/* -    SPICELIB Version 3.21.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-CYGWIN-64BIT-GCC_C. */

/* -    SPICELIB Version 3.20.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-INTEL. */

/* -    SPICELIB Version 3.19.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-INTEL-CC_C. */

/* -    SPICELIB Version 3.18.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C. */

/* -    SPICELIB Version 3.17.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-64BIT-NATIVE_C. */

/* -    SPICELIB Version 3.16.0, 13-MAY-2010 (BVS) */

/*        Updated for PC-WINDOWS-64BIT-IFORT. */

/* -    SPICELIB Version 3.15.0, 13-MAY-2010 (BVS) */

/*        Updated for PC-LINUX-64BIT-GFORTRAN. */

/* -    SPICELIB Version 3.14.0, 13-MAY-2010 (BVS) */

/*        Updated for PC-64BIT-MS_C. */

/* -    SPICELIB Version 3.13.0, 13-MAY-2010 (BVS) */

/*        Updated for MAC-OSX-64BIT-INTEL_C. */

/* -    SPICELIB Version 3.12.0, 13-MAY-2010 (BVS) */

/*        Updated for MAC-OSX-64BIT-IFORT. */

/* -    SPICELIB Version 3.11.0, 13-MAY-2010 (BVS) */

/*        Updated for MAC-OSX-64BIT-GFORTRAN. */

/* -    SPICELIB Version 3.10.0, 18-MAR-2009 (BVS) */

/*        Updated for PC-LINUX-GFORTRAN. */

/* -    SPICELIB Version 3.9.0, 18-MAR-2009 (BVS) */

/*        Updated for MAC-OSX-GFORTRAN. */

/* -    SPICELIB Version 3.8.0, 19-FEB-2008 (BVS) */

/*        Updated for PC-LINUX-IFORT. */

/* -    SPICELIB Version 3.7.0, 14-NOV-2006 (BVS) */

/*        Updated for PC-LINUX-64BIT-GCC_C. */

/* -    SPICELIB Version 3.6.0, 14-NOV-2006 (BVS) */

/*        Updated for MAC-OSX-INTEL_C. */

/* -    SPICELIB Version 3.5.0, 14-NOV-2006 (BVS) */

/*        Updated for MAC-OSX-IFORT. */

/* -    SPICELIB Version 3.4.0, 14-NOV-2006 (BVS) */

/*        Updated for PC-WINDOWS-IFORT. */

/* -    SPICELIB Version 3.3.0, 26-OCT-2005 (BVS) */

/*        Updated for SUN-SOLARIS-64BIT-GCC_C. */

/* -    SPICELIB Version 3.2.0, 03-JAN-2005 (BVS) */

/*        Updated for PC-CYGWIN_C. */

/* -    SPICELIB Version 3.1.0, 03-JAN-2005 (BVS) */

/*        Updated for PC-CYGWIN. */

/* -    SPICELIB Version 3.0.5, 17-JUL-2002 (BVS) */

/*        Added MAC-OSX environments. */

/* -    SPICELIB Version 3.0.4, 08-OCT-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given. New */
/*        environments are WIN-NT */

/* -    SPICELIB Version 3.0.3, 24-SEP-1999 (NJB) */

/*        CSPICE environments were added. Some typos were corrected. */

/* -    SPICELIB Version 3.0.2, 28-JUL-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given. New */
/*        environments are PC-DIGITAL, SGI-O32 and SGI-N32. */

/* -    SPICELIB Version 3.0.1, 18-MAR-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given. Previously, */
/*        environments such as SUN-SUNOS and SUN-SOLARIS were implied */
/*        by the environment label SUN. */

/* -    SPICELIB Version 3.0.0, 07-APR-1998 (NJB) */

/*        Module was updated for the PC-LINUX platform. */

/* -    SPICELIB Version 2.0.0, 09-NOV-1993 (HAN) */

/*         Module was updated to include the value for FILEN */
/*         for the Silicon Graphics, DEC Alpha-OSF/1, and */
/*         NeXT platforms. Also, the previous value of 256 for */
/*         Unix platforms was changed to 255. */

/* -    SPICELIB Version 1.1.0, 12-OCT-1992 (HAN) */

/*         Updated module for multiple environments. */

/*         The code was also reformatted so that a utility program can */
/*         create the source file for a specific environment given a */
/*         master source file. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*         Comment section for permuted index source lines was added */
/*         following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (NJB) */

/* -& */
/* $ Index_Entries */

/*     None. */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 3.0.0, 07-APR-1998 (NJB) */

/*        Module was updated for the PC-LINUX platform. */

/* -     SPICELIB Version 2.0.0, 9-NOV-1993 (HAN) */

/*         Module was updated to include the value for FILEN */
/*         for the Silicon Graphics, DEC Alpha-OSF/1, and */
/*         NeXT platforms. Also, the previous value of 256 for */
/*         Unix platforms was changed to 255. */

/* -     SPICELIB Version 1.1.0, 12-OCT-1992 (HAN) */

/*         Updated module for multiple environments. */

/*         The code was also reformatted so that a utility program can */
/*         create the source file for a specific environment given a */
/*         master source file. */

/* -     Beta Version 1.1.0, 13-DEC-1989 (NJB) */

/*         PRTPKG, though it performs no run-time function, must */
/*         still return a value, in order to comply with the Fortran */
/*         standard. So, now it does. */

/* -     Beta Version 1.0.1, 08-FEB-1989 (NJB) */

/*         Warnings added to discourage use of this routine. */
/*         Parameter declarations moved to "Declarations" section. */
/*         Two local declarations moved to the correct location. */

/* -& */

/*     SPICELIB functions */


/*     Local variables: */


/*     Saved variables: */


/*     Initial values: */

    switch(n__) {
	case 1: goto L_setprt;
	case 2: goto L_msgsel;
	}


/*     Executable Code: */

    getdev_(device, (ftnlen)255);
    wrline_(device, "PRTPKG:  You have called an entry point which has no ru"
	    "n-time function; this may indicate a program bug.  Please check "
	    "the PRTPKG documentation.  ", (ftnlen)255, (ftnlen)146);
    wrline_(device, "SPICE(BOGUSENTRY)", (ftnlen)255, (ftnlen)17);
    ret_val = FALSE_;
    return ret_val;
/* $Procedure SETPRT ( Store Error Message Types to be Output ) */

L_setprt:
/* $ Abstract */

/*     Store (a representation of) the selection of types of error */
/*     messages to be output.  DO NOT CALL THIS ROUTINE. */

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

/*     LOGICAL               SHORT */
/*     LOGICAL               EXPL */
/*     LOGICAL               LONG */
/*     LOGICAL               TRACE */
/*     LOGICAL               DFAULT */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     SHORT      I   Select output of short error message? */
/*     EXPL       I   Select output of explanation of short message? */
/*     LONG       I   Select output of long error message? */
/*     TRACE      I   Select output of traceback? */
/*     DFAULT     I   Select output of default message? */

/* $ Detailed_Input */

/*     SHORT    indicates whether the short error message is selected */
/*              as one of the error messages to be output when an error */
/*              is detected. A value of .TRUE. indicates that the */
/*              short error message IS selected. */

/*     EXPL     indicates whether the explanatory text for the short */
/*              error message is selected as one of the error messages */
/*              to be output when an error is detected. A value of */
/*              .TRUE. indicates that the explanatory text for the */
/*              short error message IS selected. */

/*     LONG     indicates whether the long error message is selected */
/*              as one of the error messages to be output when an error */
/*              is detected. A value of .TRUE. indicates that the */
/*              long error message IS selected. */

/*     TRACE    indicates whether the traceback is selected */
/*              as one of the error messages to be output when an error */
/*              is detected. A value of .TRUE. indicates that the */
/*              traceback IS selected. */

/*     DFAULT   indicates whether the default message is selected */
/*              as one of the error messages to be output when an error */
/*              is detected. A value of .TRUE. indicates that the */
/*              default message IS selected. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     DO NOT CALL THIS ROUTINE. */

/*     The effect of this routine is an ENVIRONMENTAL one. This */
/*     routine performs no output;  it stores the error message */
/*     selection provided as input. */

/*     Note that the actual output of error messages depends not */
/*     only on the selection made using this routine, but also */
/*     on the selection of the error output device (see ERRDEV) */
/*     and the choice of error response action (see ERRACT). If */
/*     the action is not 'IGNORE' (possible choices are */
/*     'IGNORE', 'ABORT', 'DEFAULT', 'REPORT', and 'RETURN'), */
/*     the selected error messages will be written to the chosen */
/*     output device when an error is detected. */

/* $ Examples */

/*     1. In this example, the short and long messages are selected. */

/*     C */
/*     C     Select short and long error messages for output */
/*     C     (We don't examine the status returned because no */
/*     C     errors are detected by SETPRT): */
/*     C */

/*           STATUS = SETPRT ( .TRUE., .FALSE., .TRUE., .FALSE., */
/*          .                  .FALSE.                          ) */

/* $ Restrictions */

/*     1)  DO NOT CALL THIS ROUTINE. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 3.1.0, 17-JUN-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 3.0.5, 17-JUL-2002 (BVS) */

/*        Added MAC-OSX environments. */

/* -    SPICELIB Version 3.0.4, 08-OCT-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given. New */
/*        environments are WIN-NT */

/* -    SPICELIB Version 3.0.2, 28-JUL-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given. New */
/*        environments are PC-DIGITAL, SGI-O32 and SGI-N32. */

/* -    SPICELIB Version 3.0.1, 18-MAR-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given. Previously, */
/*        environments such as SUN-SUNOS and SUN-SOLARIS were implied */
/*        by the environment label SUN. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*         Comment section for permuted index source lines was added */
/*         following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (NJB) */

/* -& */
/* $ Index_Entries */

/*     None. */

/* -& */
/* $ Revisions */

/* -     Beta Version 1.0.1, 08-FEB-1989 (NJB) */

/*         Warnings added to discourage use of this routine in */
/*         non-error-handling code. $Parameters section added. */

/* -& */

/*     Executable Code: */

    if (*short__) {
	svshrt = TRUE_;
    } else {
	svshrt = FALSE_;
    }
    if (*expl) {
	svexpl = TRUE_;
    } else {
	svexpl = FALSE_;
    }
    if (*long__) {
	svlong = TRUE_;
    } else {
	svlong = FALSE_;
    }
    if (*trace) {
	svtrac = TRUE_;
    } else {
	svtrac = FALSE_;
    }
    if (*dfault) {
	svdflt = TRUE_;
    } else {
	svdflt = FALSE_;
    }

/*     We assign a value to SETPRT, but this value is */
/*     not meaningful... */

    ret_val = TRUE_;
    return ret_val;
/* $Procedure MSGSEL  ( Is This Message Type Selected for Output? ) */

L_msgsel:
/* $ Abstract */

/*     Indicate whether the specified message type has been selected */
/*     for output. */

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

/*     CHARACTER*(*)         TYPE */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     TYPE       I   Type of message whose selection status is queried. */
/*     FILEN      P   Maximum length of a file name. */

/*     The function returns the value .TRUE. if the message type */
/*     indicated by TYPE has been selected for output to the error output */
/*     device. */

/* $ Detailed_Input */

/*     TYPE     is the type of error message. */

/*              Possible values of TYPE are 'SHORT', 'EXPLAIN', 'LONG', */
/*              'DEFAULT', and 'TRACEBACK'. */

/* $ Detailed_Output */

/*     The function returns the value .TRUE. if the message type */
/*     indicated by TYPE has been selected for output to the error */
/*     output device. */

/* $ Parameters */

/*     FILEN    is the maximum length of a file name. */

/* $ Exceptions */

/*     1)  If an invalid value of TYPE is detected, the error */
/*         SPICE(INVALIDMSGTYPE) is signaled. The handling of this error */
/*         is a special case; to avoid recursion problems, SIGERR is not */
/*         called when the error is detected. Instead, the short and long */
/*         error messages are output directly. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine is part of the SPICELIB error handling mechanism. */

/*     Note that even though a given type of message may have been */
/*     selected for output, the output device and error response */
/*     action must also have been selected appropriately. */
/*     Use ERRDEV to choose the output device for error messages. */
/*     Use ERRACT to choose the error response action. Any action */
/*     other than 'IGNORE' will result in error messages being */
/*     written to the error output device when errors are detected. */
/*     See ERRACT for details. */

/* $ Examples */

/*     1. We want to know if the short message has been selected */
/*         for output: */

/*         C */
/*         C     Test whether the short message has been selected: */
/*         C */

/*               SELECT = MSGSEL ( 'SHORT' ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 3.1.0, 17-JUN-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Corrected */
/*        type declaration in $Declarations section. */

/* -    SPICELIB Version 3.0.5, 17-JUL-2002 (BVS) */

/*        Added MAC-OSX environments. */

/* -    SPICELIB Version 3.0.4, 08-OCT-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given. New */
/*        environments are WIN-NT */

/* -    SPICELIB Version 3.0.2, 28-JUL-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given. New */
/*        environments are PC-DIGITAL, SGI-O32 and SGI-N32. */

/* -    SPICELIB Version 3.0.1, 18-MAR-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given. Previously, */
/*        environments such as SUN-SUNOS and SUN-SOLARIS were implied */
/*        by the environment label SUN. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*         Comment section for permuted index source lines was added */
/*         following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (NJB) */

/* -& */
/* $ Index_Entries */

/*     None. */

/* -& */
/* $ Revisions */

/* -     Beta Version 1.0.1, 08-FEB-1989 (NJB) */

/*         $Parameters section added; parameter declaration added */
/*         to brief I/O section as well. */

/* -& */

/*     Executable Code: */

    ljust_(type__, ltype, type_len, (ftnlen)10);
    ucase_(ltype, ltype, (ftnlen)10, (ftnlen)10);
    if (s_cmp(ltype, "SHORT", (ftnlen)10, (ftnlen)5) == 0) {
	ret_val = svshrt;
    } else if (s_cmp(ltype, "EXPLAIN", (ftnlen)10, (ftnlen)7) == 0) {
	ret_val = svexpl;
    } else if (s_cmp(ltype, "LONG", (ftnlen)10, (ftnlen)4) == 0) {
	ret_val = svlong;
    } else if (s_cmp(ltype, "TRACEBACK", (ftnlen)10, (ftnlen)9) == 0) {
	ret_val = svtrac;
    } else if (s_cmp(ltype, "DEFAULT", (ftnlen)10, (ftnlen)7) == 0) {
	ret_val = svdflt;
    } else {

/*        Bad value of type!  We have a special case here; to */
/*        avoid recursion, we output the messages directly, */
/*        rather than call SIGERR. */

	getdev_(device, (ftnlen)255);
	wrline_(device, "SPICE(INVALIDMSGTYPE)", (ftnlen)255, (ftnlen)21);
	wrline_(device, " ", (ftnlen)255, (ftnlen)1);
	s_copy(loctyp, type__, (ftnlen)10, type_len);

/*        Note:  What looks like a typo below isn't; there's */
/*        a line break after the substring 'specified' of */
/*        the "word" 'specifiedwas'. */

/* Writing concatenation */
	i__1[0] = 86, a__1[0] = "MSGSEL:  An invalid error message type was "
		"supplied as input; the type specifiedwas:  ";
	i__1[1] = 10, a__1[1] = loctyp;
	s_cat(ch__1, a__1, i__1, &c__2, (ftnlen)96);
	wrline_(device, ch__1, (ftnlen)255, (ftnlen)96);
    }
    return ret_val;
} /* prtpkg_ */

logical prtpkg_(logical *short__, logical *long__, logical *expl, logical *
	trace, logical *dfault, char *type__, ftnlen type_len)
{
    return prtpkg_0_(0, short__, long__, expl, trace, dfault, type__, 
	    type_len);
    }

logical setprt_(logical *short__, logical *expl, logical *long__, logical *
	trace, logical *dfault)
{
    return prtpkg_0_(1, short__, long__, expl, trace, dfault, (char *)0, (
	    ftnint)0);
    }

logical msgsel_(char *type__, ftnlen type_len)
{
    return prtpkg_0_(2, (logical *)0, (logical *)0, (logical *)0, (logical *)
	    0, (logical *)0, type__, type_len);
    }

