/* errfnm.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ERRFNM ( Insert filename into long error message text ) */
/* Subroutine */ int errfnm_(char *marker, integer *unit, ftnlen marker_len)
{
    /* System generated locals */
    inlist ioin__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer f_inqu(inlist *), s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    char name__[128];
    extern /* Subroutine */ int errch_(char *, char *, ftnlen, ftnlen);
    integer iostat;

/* $ Abstract */

/*     Substitute the first occurrence of a marker in the current long */
/*     error message with the name of the file attached to the logical */
/*     unit number. */

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
/*     STRING */
/*     UNITS */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     MARKER     I   A substring in the error message that is to be */
/*                    replaced. */
/*     UNIT       I   Logical unit number attached to a file. */
/*     FILEN      P   Maximum length of filename. */

/* $ Detailed_Input */

/*     MARKER   is a character string which marks a position in */
/*              the long error message where a character string */
/*              is to be substituted. Leading and trailing blanks */
/*              in MARKER are not significant. */

/*              Case IS significant;  'XX' is considered to be */
/*              a different marker from 'xx'. */

/*     UNIT     is the logical unit number attached to a file. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     FILEN    is the maximum file name length that can be */
/*              accommodated by this routine. */

/* $ Exceptions */

/*     Error free. */

/*     1)  If the logical unit number is not attached to a file, the */
/*         string inserted into the long error message is: */

/*         '<unavailable from the system>' */

/*     2)  If the FORTRAN INQUIRE statement fails to execute properly, */
/*         the string inserted into the long error message is: */

/*         '<unavailable from the system>' */

/* $ Files */

/*     See "Detailed_Input" description of the variable UNIT. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     1. The following code fragment reads a record from a file */
/*        then checks to see if the read was successful. If the */
/*        read failed, an error message is constructed that */
/*        specifies the record number, the filename and the value */
/*        of IOSTAT. */

/*        ERRFNM is used to replace the marker in the long error */
/*        message with the name of the file. */


/*        READ ( UNIT, REC=RECNUM, IOSTAT=IOSTAT ) RECORD */

/*         IF ( IOSTAT .NE. 0 ) THEN */

/*            CALL SETMSG ( 'Error reading record number # from ' // */
/*      .                   'file FILENAME. The value of IOSTAT ' // */
/*      .                   'was #.'                              ) */

/*            CALL ERRINT ( '#',         RECNUM   ) */
/*            CALL ERRFNM ( 'FILENAME',  UNIT     ) */
/*            CALL ERRINT ( '#',         IOSTAT   ) */
/*            CALL SIGERR ( 'SPICE(READFAILURE)'  ) */
/*            CALL CHKOUT ( 'SAMPLE'              ) */
/*            RETURN */

/*         END IF */


/*         If the unit is attached to the file SAMPLE.DAT, RECNUM */
/*         is 15 and IOSTAT is 36, and the INQUIRE statement in */
/*         this routine executed successfully, the long error */
/*         message is: */

/*           'Error reading record number 15 from file SAMPLE.DAT. */
/*            The value of IOSTAT was 36.' */


/*         If the unit is not attached to a file or if the INQUIRE */
/*         statement in this routine failed to execute successfully, */
/*         the long error message is: */

/*           'Error reading record number 15 from file */
/*           <unavailable from the system>. The value of IOSTAT */
/*           was 36.' */


/*     2. Note that the case of the marker is significant. The following */
/*        code fragment contains a call to ERRFNM using a marker */
/*        that does not appear in the long error message. */


/*        READ ( UNIT, REC=RECNUM, IOSTAT=IOSTAT ) RECORD */

/*         IF ( IOSTAT .NE. 0 ) THEN */

/*            CALL SETMSG ( 'Error reading record number # from ' // */
/*      .                   'file FILENAME. The value of IOSTAT ' // */
/*      .                   'was #.'                              ) */

/*            CALL ERRINT ( '#',         RECNUM   ) */
/*            CALL ERRFNM ( 'filename',  UNIT     ) */
/*            CALL ERRINT ( '#',         IOSTAT   ) */
/*            CALL SIGERR ( 'SPICE(READFAILURE)'  ) */
/*            CALL CHKOUT ( 'SAMPLE'              ) */
/*            RETURN */

/*         END IF */


/*         If the marker is not found, ERRFNM does not substitute */
/*         the filename for the marker. The long error message in */
/*         this case is: */

/*           'Error reading record number 15 from file FILENAME. */
/*            The value of IOSTAT was 36.' */

/* $ Restrictions */

/*     1)  The filename length is restricted by the parameter FILEN. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     H.A. Neilan        (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 02-JUN-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 05-APR-1991 (HAN) */

/* -& */
/* $ Index_Entries */

/*     insert filename into long error message */

/* -& */

/*     Local variables */


/*     Initialize the variables. */

    s_copy(name__, " ", (ftnlen)128, (ftnlen)1);

/*     Get the name of the file attached to the logical unit number. */

    ioin__1.inerr = 1;
    ioin__1.inunit = *unit;
    ioin__1.infile = 0;
    ioin__1.inex = 0;
    ioin__1.inopen = 0;
    ioin__1.innum = 0;
    ioin__1.innamed = 0;
    ioin__1.innamlen = 128;
    ioin__1.inname = name__;
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

/*     If the INQUIRE statement executed successfully and the unit */
/*     was attached to a file, we have a filename. */

/*     If the INQUIRE statement didn't execute successfully the value */
/*     of IOSTAT is not equal to zero. If the unit is not connected to */
/*     a file the filename is blank. If either of these two things */
/*     are true, we must construct a string indicating that the */
/*     filename was unavailable from the system. */

    if (iostat != 0 || s_cmp(name__, " ", (ftnlen)128, (ftnlen)1) == 0) {
	s_copy(name__, "<unavailable from the system>", (ftnlen)128, (ftnlen)
		29);
    }

/*     Let the error handling routine take it from here. */

    errch_(marker, name__, marker_len, (ftnlen)128);
    return 0;
} /* errfnm_ */

