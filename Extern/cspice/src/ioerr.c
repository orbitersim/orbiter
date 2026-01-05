/* ioerr.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__0 = 0;
static integer c__2 = 2;

/* $Procedure IOERR ( I/O error message writer ) */
/* Subroutine */ int ioerr_(char *action, char *file, integer *iostat, ftnlen 
	action_len, ftnlen file_len)
{
    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    char error[320], iochar[10];
    extern /* Subroutine */ int setmsg_(char *, ftnlen), suffix_(char *, 
	    integer *, char *, ftnlen, ftnlen), intstr_(integer *, char *, 
	    ftnlen);

/* $ Abstract */

/*     Set the long error message equal to a standard I/O error message */
/*     composed from an action, the name of a file, and a value of */
/*     IOSTAT. */

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

/*     ERROR */
/*     FILES */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     ACTION     I   Action which caused the error. */
/*     FILE       I   The name of the file involved. */
/*     IOSTAT     I   The value of IOSTAT returned by ACTION. */

/* $ Detailed_Input */

/*     ACTION   is the action which caused the error. This may */
/*              be the name of a basic operation, such as 'OPEN', */
/*              'READ', or 'WRITE', or may be more sophisticated, */
/*              for example, 'add an empty cluster header to'. */

/*     FILE     is the name of the file involved in the error. */
/*              This may be the system or logical name of a file */
/*              ('USER$DISK:[USER.SUB]TEMP.DAT', 'PLNEPH'), or one */
/*              of the standard files ('SYS$INPUT', 'SYS$OUTPUT'). */

/*     IOSTAT   is the value of IOSTAT returned by ACTION. This */
/*              is appended to the end of the error message. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The input arguments are inserted into the standard form shown */
/*     below. Spaces are inserted where needed. Leading and trailing */
/*     spaces are removed. */

/*     The long error message is set equal to a standard I/O error */
/*     message, of the form: */

/*                 An error occurred while --------1---------- */
/*                 -------2-------.  The value of IOSTAT returned */
/*                 was --3--. */

/*               where the values of ACTION, FILE, and IOSTAT are */
/*               assigned to positions 1, 2 and 3 */
/*               respectively. */

/*     If the length of the entire composed message exceeds 320 */
/*     characters, it is truncated. */

/*     SIGERR must be called following a call to this routine to */
/*     actually output the resulting long error message to the error */
/*     output device. */

/* $ Examples */

/*     The following example illustrates the use of IOERR. */

/*           CALL IOERR ( 'adding a new header to', */
/*                        EPHEM, */
/*                        24                      ) */

/*     The resulting error message would be: */

/*           'An error occurred while adding a new header */
/*            to LIBDISK:[EPHEM.NESYS]VGR2_T860502.GEF.  The value */
/*            of IOSTAT returned was 24.' */

/*     Note that the user is not responsible for adding and eliminating */
/*     spaces to make the string readable. That is all done */
/*     automatically. */

/*     It is possible to omit the name of the file entirely, as in the */
/*     following (somewhat frivolous) example. */

/*           CALL IOERR ( 'cleaning a fish', */
/*                        ' ', */
/*                        -3                                ) */

/*     The resulting error message would be: */

/*           'An error occurred while cleaning a fish. */
/*            The value of IOSTAT returned was -3.' */

/*     In fact, if the value of IOSTAT is zero, the last part of the */
/*     message is omitted entirely, as in the following example. */

/*           CALL IOERR ( 'writing the status line to', */
/*                        'SYS$OUTPUT', */
/*                        0                                 ) */

/*     The resulting error message would be: */

/*           'An error occurred while writing the status */
/*            line to SYS$OUTPUT.' */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 03-JUN-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (NJB) */

/* -& */
/* $ Index_Entries */

/*     i/o error message writer */

/* -& */
/* $ Revisions */

/* -    Beta Version 2.0.0, 20-DEC-1988 (NJB) */

/*        IOERR now sets the long error message equal to the */
/*        constructed message, rather than returning the constructed */
/*        message to the caller.  IOERR's argument list has been */
/*        changed accordingly, and a call to SETMSG has been added. */
/*        Also, the name of the calling routine no longer appears */
/*        in the constructed message. */

/* -& */

/*     Local variables */


/*     First comes some standard stuff. */

    s_copy(error, "An error occurred while", (ftnlen)320, (ftnlen)23);

/*     Next comes the action that caused the error, and the file name. */
/*     There should be at least one space between each of these pieces, */
/*     but not more than one. */

    suffix_(action, &c__1, error, action_len, (ftnlen)320);
    suffix_(file, &c__1, error, file_len, (ftnlen)320);
    suffix_(".", &c__0, error, (ftnlen)1, (ftnlen)320);

/*     More standard stuff. If IOSTAT is zero, there is no need for this */
/*     part of the message. */

    if (*iostat != 0) {
	suffix_("The value of IOSTAT returned was", &c__2, error, (ftnlen)32, 
		(ftnlen)320);

/*        IOSTAT must be written to a character variable first. */
/*        Attempting to write it directly to ERROR could cause a */
/*        boo-boo if we have already overrun the length of ERROR. */

	intstr_(iostat, iochar, (ftnlen)10);
	suffix_(iochar, &c__1, error, (ftnlen)10, (ftnlen)320);
	suffix_(".", &c__0, error, (ftnlen)1, (ftnlen)320);
    }

/*     The message has been constructed.  Set the long error message */
/*     equal to the constructed message. */

    setmsg_(error, (ftnlen)320);
    return 0;
} /* ioerr_ */

