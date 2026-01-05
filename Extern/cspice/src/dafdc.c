/* dafdc.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure DAFDC ( DAF delete comments ) */
/* Subroutine */ int dafdc_(integer *handle)
{
    integer free;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer bward, fward, ncomr, nd;
    extern logical failed_(void);
    integer ni;
    extern /* Subroutine */ int dafsih_(integer *, char *, ftnlen);
    char ifname[60];
    extern /* Subroutine */ int dafrfr_(integer *, integer *, integer *, char 
	    *, integer *, integer *, integer *, ftnlen), dafrrr_(integer *, 
	    integer *), chkout_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Delete the entire comment area of a previously opened binary */
/*     DAF attached to HANDLE. */

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

/*     DAF */

/* $ Keywords */

/*     None. */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   The handle of a binary DAF opened for writing. */

/* $ Detailed_Input */

/*     HANDLE   is the handle of a binary DAF that is to have its entire */
/*              comment area deleted. The DAF must have been opened */
/*              with write access. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the binary DAF attached to HANDLE is not open with write */
/*         access, an error is signaled by a routine in the call tree of */
/*         this routine. */

/* $ Files */

/*     See argument HANDLE in $Detailed_Input. */

/* $ Particulars */

/*     A binary DAF contains an area which is reserved for storing */
/*     annotations or descriptive textual information about the data */
/*     contained in a file. This area is referred to as the ``comment */
/*     area'' of the file. The comment area of a DAF is a line */
/*     oriented medium for storing textual information. The comment */
/*     area preserves any leading or embedded white space in the line(s) */
/*     of text which are stored, so that the appearance of the of */
/*     information will be unchanged when it is retrieved (extracted) at */
/*     some other time. Trailing blanks, however, are NOT preserved, */
/*     due to the way that character strings are represented in */
/*     standard Fortran 77. */

/*     This routine will delete the entire comment area from the binary */
/*     DAF attached to HANDLE. The size of the binary DAF will remain */
/*     unchanged. The space that was used by the comment records */
/*     is reclaimed. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Delete the entire comment area of a DAF file. Note that this */
/*        action should only be performed if fresh new comments are to */
/*        be placed within the DAF file. */

/*        Use the SPK kernel below as input DAF file for the program. */

/*           earthstns_itrf93_201023.bsp */


/*        Example code begins here. */


/*              PROGRAM DAFDC_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     SPICELIB functions */
/*        C */
/*              INTEGER               RTRIM */

/*        C */
/*        C     Local parameters */
/*        C */
/*              CHARACTER*(*)         KERNEL */
/*              PARAMETER           ( KERNEL = */
/*             .                         'earthstns_itrf93_201023.bsp' ) */

/*              INTEGER               BUFFSZ */
/*              PARAMETER           ( BUFFSZ = 10 ) */

/*              INTEGER               LINLEN */
/*              PARAMETER           ( LINLEN = 1000 ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              CHARACTER*(LINLEN)    BUFFER ( BUFFSZ ) */

/*              INTEGER               HANDLE */
/*              INTEGER               I */
/*              INTEGER               N */

/*              LOGICAL               DONE */

/*        C */
/*        C     Open a DAF for write. Return a HANDLE referring to the */
/*        C     file. */
/*        C */
/*              CALL DAFOPW ( KERNEL, HANDLE ) */

/*        C */
/*        C     Print the first 10 lines of comments from the DAF file. */
/*        C */
/*              WRITE(*,'(A)') 'Comment area of input DAF file ' */
/*             .            // '(max. 10 lines): ' */
/*              WRITE(*,'(A)') '---------------------------------------' */
/*             .            // '-----------------------' */

/*              CALL DAFEC  ( HANDLE, BUFFSZ, N, BUFFER, DONE ) */

/*              DO I = 1, N */

/*                 WRITE (*,*) BUFFER(I)(:RTRIM(BUFFER(I))) */

/*              END DO */

/*              WRITE(*,'(A)') '---------------------------------------' */
/*             .            // '-----------------------' */
/*              WRITE(*,*) ' ' */
/*              WRITE(*,*) 'Deleting entire comment area...' */

/*        C */
/*        C     Delete all the comments from the DAF file. */
/*        C */
/*              CALL DAFDC ( HANDLE ) */

/*        C */
/*        C     Close the DAF file and re-open it for read */
/*        C     access to work around the DAFEC restriction */
/*        C     on comments not to be modified while they are */
/*        C     being extracted. */
/*        C */
/*              CALL DAFCLS( HANDLE  ) */

/*              CALL DAFOPR( KERNEL, HANDLE  ) */

/*        C */
/*        C     Check if the comments have indeed been deleted. */
/*        C */
/*              CALL DAFEC  ( HANDLE, BUFFSZ, N, BUFFER, DONE ) */

/*              IF ( DONE .AND. N .EQ. 0 ) THEN */

/*                 WRITE(*,*) ' ' */
/*                 WRITE(*,*) '   Successful operation.' */

/*              ELSE */

/*                 WRITE(*,*) ' ' */
/*                 WRITE(*,*) '   Operation failed.' */

/*              END IF */

/*        C */
/*        C     Safely close the DAF. */
/*        C */
/*              CALL DAFCLS ( HANDLE ) */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        Comment area of input DAF file (max. 10 lines): */
/*        -------------------------------------------------------------- */

/*            SPK for DSN Station Locations */
/*            ========================================================*** */

/*            Original file name:                   earthstns_itrf93_2*** */
/*            Creation date:                        2020 October 28 12:30 */
/*            Created by:                           Nat Bachman  (NAIF*** */


/*            Introduction */
/*        -------------------------------------------------------------- */

/*         Deleting entire comment area... */

/*            Successful operation. */


/*        Warning: incomplete output. 3 lines extended past the right */
/*        margin of the header and have been truncated. These lines are */
/*        marked by "***" at the end of each line. */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 25-NOV-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */
/*        Added complete code example. */

/* -    SPICELIB Version 1.0.0, 23-SEP-1994 (KRG) */

/* -& */
/* $ Index_Entries */

/*     delete DAF comment area */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */

/*     Length of a DAF file internal filename. */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("DAFDC", (ftnlen)5);
    }

/*     Verify that the DAF attached to HANDLE was opened with write */
/*     access. */

    dafsih_(handle, "WRITE", (ftnlen)5);
    if (failed_()) {
	chkout_("DAFDC", (ftnlen)5);
	return 0;
    }

/*     Read the file record to obtain the current number of comment */
/*     records in the DAF attached to HANDLE. We will also get back some */
/*     extra stuff that we do not use. */

    dafrfr_(handle, &nd, &ni, ifname, &fward, &bward, &free, (ftnlen)60);
    ncomr = fward - 2;
    if (failed_()) {
	chkout_("DAFDC", (ftnlen)5);
	return 0;
    }

/*     Now we will attempt to remove the comment records, if there are */
/*     any, otherwise we do nothing. */

    if (ncomr > 0) {

/*        We have some comment records, so remove them. */

	dafrrr_(handle, &ncomr);
	if (failed_()) {
	    chkout_("DAFDC", (ftnlen)5);
	    return 0;
	}
    }

/*     We're done now, so goodbye. */

    chkout_("DAFDC", (ftnlen)5);
    return 0;
} /* dafdc_ */

