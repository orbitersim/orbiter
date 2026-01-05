/* prompt.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* $Procedure PROMPT ( Prompt a user for a string ) */
/* Subroutine */ int prompt_(char *dspmsg, char *buffer, ftnlen dspmsg_len, 
	ftnlen buffer_len)
{
    /* System generated locals */
    integer i__1, i__2;
    cilist ci__1;

    /* Builtin functions */
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void),
	     s_rsfe(cilist *), e_rsfe(void), i_len(char *, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen), sigerr_(char *, ftnlen), chkout_(char *, ftnlen)
	    , setmsg_(char *, ftnlen);
    integer iostat;
    extern /* Subroutine */ int errint_(char *, integer *, ftnlen);

/* $ Abstract */

/*     Prompt a user for keyboard input. */

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
/*     DSPMSG     I   The prompt to use when asking for input. */
/*     BUFFER     O   The response typed by a user. */

/* $ Detailed_Input */

/*     DSPMSG   is a character string that will be displayed from the */
/*              current cursor position and describes the input that */
/*              the user is expected to enter. The string DSPMSG should */
/*              be relatively short, i.e., 50 or fewer characters, so */
/*              that a response may be typed on the line where the */
/*              prompt appears. */

/*              All characters (including trailing blanks) in DSPMSG */
/*              are considered significant and will be displayed. */

/* $ Detailed_Output */

/*     BUFFER   is a character string that contains the string */
/*              entered by the user. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     This subroutine uses discovery check-in so that it may be called */
/*     after an error has occurred. */

/*     1)  If the attempt to write the prompt to the standard output */
/*         device fails, returning an IOSTAT value not equal to zero, the */
/*         error SPICE(WRITEFAILED) is signaled. */

/*     2)  If the attempt to read the response from the standard input */
/*         device fails, returning an IOSTAT value not equal to zero, the */
/*         error SPICE(READFAILED) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This is a utility that allows you to "easily" request information */
/*     from a program user. At a high level, it frees you from the */
/*     peculiarities of a particular implementation of FORTRAN cursor */
/*     control. */

/* $ Examples */

/*     The numerical results shown for these examples may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Suppose you have an interactive program that computes state */
/*        vectors by calling SPKEZR. The program prompts the user for */
/*        the inputs to SPKEZR. After each prompt is written, the program */
/*        leaves the cursor at the end of the string as shown here: */

/*           Enter UTC epoch  > _ */

/*        (The underscore indicates the cursor position). */

/*        Use the meta-kernel shown below to load the required SPICE */
/*        kernels. */


/*           KPL/MK */

/*           File: prompt_ex1.tm */

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
/*              naif0011.tls                     Leapseconds */


/*           \begindata */

/*              KERNELS_TO_LOAD = ( 'de430.bsp', */
/*                                  'mar097.bsp', */
/*                                  'naif0011.tls' ) */

/*           \begintext */

/*           End of meta-kernel */


/*        Example code begins here. */


/*              PROGRAM PROMPT_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local parameters */
/*        C */
/*              CHARACTER*(*)         FMT */
/*              PARAMETER           ( FMT    = '(A,F22.10)' ) */
/*              INTEGER               STRLEN */
/*              PARAMETER           ( STRLEN = 36 ) */

/*        C */
/*        C     Local variables */
/*        C */
/*              CHARACTER*(STRLEN)    OBS */
/*              CHARACTER*(STRLEN)    TARGET */
/*              CHARACTER*(STRLEN)    EPOCH */

/*              DOUBLE PRECISION      ET */
/*              DOUBLE PRECISION      LT */
/*              DOUBLE PRECISION      STATE ( 6 ) */

/*              INTEGER               I */

/*        C */
/*        C     Load kernel. */
/*        C */
/*              CALL FURNSH( 'prompt_ex1.tm' ) */

/*        C */
/*        C     Prompt for the required inputs. */
/*        C */
/*              CALL PROMPT ( 'Enter UTC epoch             > ', EPOCH  ) */
/*              CALL PROMPT ( 'Enter observer name         > ', OBS    ) */
/*              CALL PROMPT ( 'Enter target name           > ', TARGET ) */

/*        C */
/*        C     Convert the UTC request time to ET (seconds past */
/*        C     J2000, TDB). */
/*        C */
/*              CALL STR2ET( EPOCH, ET ) */

/*        C */
/*        C     Look up the state vector at the requested ET */
/*        C */
/*              CALL SPKEZR( TARGET, ET, 'J2000', 'NONE', OBS, STATE, LT) */

/*        C */
/*        C     Output... */
/*        C */
/*              WRITE(*,*) ' ' */
/*              WRITE(*,FMT) 'Epoch               : ', ET */
/*              WRITE(*,FMT) '   x-position   (km): ', STATE(1) */
/*              WRITE(*,FMT) '   y-position   (km): ', STATE(2) */
/*              WRITE(*,FMT) '   z-position   (km): ', STATE(3) */
/*              WRITE(*,FMT) '   x-velocity (km/s): ', STATE(4) */
/*              WRITE(*,FMT) '   y-velocity (km/s): ', STATE(5) */
/*              WRITE(*,FMT) '   z-velocity (km/s): ', STATE(6) */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, using the time string '2017-07-14T19:46:00' as epoch, */
/*        'MARS' as target and 'EARTH' as observer, the output was: */


/*        Enter UTC epoch             > 2017-07-14T19:46:00 */
/*        Enter observer name         > MARS */
/*        Enter target name           > EARTH */

/*        Epoch               :   553333628.1837273836 */
/*           x-position   (km):   173881563.8231496215 */
/*           y-position   (km):  -322898311.5398598909 */
/*           z-position   (km):  -147992421.0068917871 */
/*           x-velocity (km/s):          47.4619819770 */
/*           y-velocity (km/s):          19.0770886182 */
/*           z-velocity (km/s):           7.9424268278 */


/* $ Restrictions */

/*     1)  This routine is environment specific. Standard FORTRAN does */
/*         not provide for user control of cursor position after write */
/*         statements. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 3.27.0, 28-NOV-2021 (BVS) */

/*        Updated for MAC-OSX-M1-64BIT-CLANG_C. */

/* -    SPICELIB Version 3.26.0, 13-AUG-2021 (JDR) */

/*        Changed argument names PRMPT and STRING to DSPMSG and BUFFER */
/*        for consistency with other routines. */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */
/*        Added complete code example. */

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

/* -    SPICELIB Version 3.0.0, 08-APR-1998 (NJB) */

/*        Module was updated for the PC-LINUX platform. */

/* -    SPICELIB Version 2.0.0, 20-JUL-1995 (WLT) (KRG) */

/*        This routine now participates in error handling. It */
/*        checks to make sure no I/O errors have occurred while */
/*        attempting to write to standard output or read from standard */
/*        input. It uses discovery checkin if an error is detected. */

/*        Restructured the subroutine a little bit; the writing of the */
/*        prompt is the only bit that is environment specific, so the */
/*        code was rearranged to reflect this. There is now only a single */
/*        READ statement. */

/* -    SPICELIB Version 1.0.0, 15-OCT-1992 (WLT) */

/* -& */
/* $ Index_Entries */

/*     Prompt for keyboard input */
/*     Prompt for input with a user supplied message */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 3.0.0, 08-APR-1998 (NJB) */

/*        Module was updated for the PC-LINUX platform. */

/* -    SPICELIB Version 2.0.0, 20-JUL-1995 (WLT) (KRG) */

/*        This routine now participates in error handling. It */
/*        checks to make sure no I/O errors have occurred while */
/*        attempting to write to standard output or read from standard */
/*        input. It uses discovery checkin if an error is detected. */

/*        Restructured the subroutine a little bit; the writing of the */
/*        prompt is the only bit that is environment specific, so the */
/*        code was rearranged to reflect this. There is now only a single */
/*        READ statement. */

/* -& */

/*     Local variables */




/*     The code below should be used in the following environments: */

/*     SUN/Fortran, */
/*     HP/HP-Fortran, */
/*     Silicon Graphics/Silicon Graphics Fortran, */
/*     DEC Alpha-OSF/1--DEC Fortran, */
/*     NeXT/Absoft Fortran */
/*     PC Linux/Fort77 */

    ci__1.cierr = 1;
    ci__1.ciunit = 6;
    ci__1.cifmt = "(A,$)";
    iostat = s_wsfe(&ci__1);
    if (iostat != 0) {
	goto L100001;
    }
    iostat = do_fio(&c__1, dspmsg, dspmsg_len);
    if (iostat != 0) {
	goto L100001;
    }
    iostat = e_wsfe();
L100001:

/*     If none of the write statements above works on a particular */
/*     unsupported platform, read on... */

/*     Although, this isn't really what you want, if you need to port */
/*     this quickly to an environment that does not support the format */
/*     statement in any of the cases above, you can comment out the */
/*     write statement above and un-comment the write statement below. */
/*     In this way you can get a program working quickly in the new */
/*     environment while you figure out how to control cursor */
/*     positioning. */

/*      WRITE (*,*, IOSTAT=IOSTAT ) DSPMSG */

/*     Check for a write error. It's not likely, but the standard output */
/*     can be redirected. Better safe than confused later. */

    if (iostat != 0) {
	chkin_("PROMPT", (ftnlen)6);
	setmsg_("An error occurred while attempting to write a prompt to the"
		" standard output device, possibly because standard output ha"
		"s been redirected to a file. There is not much that can be d"
		"one about this if it happens. We do not try to determine whe"
		"ther standard output has been redirected, so be sure that th"
		"ere are sufficient resources available for the operation bei"
		"ng performed.", (ftnlen)372);
	sigerr_("SPICE(WRITEFAILED)", (ftnlen)18);
	chkout_("PROMPT", (ftnlen)6);
	return 0;
    }

/*     Now that we've written out the prompt and there was no error, we */
/*     can read in the response. */

    ci__1.cierr = 1;
    ci__1.ciend = 1;
    ci__1.ciunit = 5;
    ci__1.cifmt = "(A)";
    iostat = s_rsfe(&ci__1);
    if (iostat != 0) {
	goto L100002;
    }
    iostat = do_fio(&c__1, buffer, buffer_len);
    if (iostat != 0) {
	goto L100002;
    }
    iostat = e_rsfe();
L100002:
    if (iostat != 0) {
	chkin_("PROMPT", (ftnlen)6);
	setmsg_("An error occurred while attempting to retrieve a reply to t"
		"he prompt \"#\".  A possible cause is that you have exhauste"
		"d the input buffer while attempting to type your response.  "
		"It may help if you limit your response to # or fewer charact"
		"ers. ", (ftnlen)242);
	errch_("#", dspmsg, (ftnlen)1, dspmsg_len);
/* Computing MIN */
	i__2 = i_len(buffer, buffer_len);
	i__1 = min(i__2,131);
	errint_("#", &i__1, (ftnlen)1);
	sigerr_("SPICE(READFAILED)", (ftnlen)17);
	chkout_("PROMPT", (ftnlen)6);
	return 0;
    }
    return 0;
} /* prompt_ */

