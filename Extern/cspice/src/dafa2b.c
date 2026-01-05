/* dafa2b.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure DAFA2B ( DAF, ASCII to binary ) */
/* Subroutine */ int dafa2b_(char *ascii, char *binary, integer *resv, ftnlen 
	ascii_len, ftnlen binary_len)
{
    /* System generated locals */
    cllist cl__1;

    /* Builtin functions */
    integer f_clos(cllist *);

    /* Local variables */
    integer unit;
    extern /* Subroutine */ int chkin_(char *, ftnlen), daft2b_(integer *, 
	    char *, integer *, ftnlen);
    extern logical failed_(void);
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int txtopr_(char *, integer *, ftnlen);

/* $ Abstract */

/*     Deprecated: This routine has been superseded by the SPICELIB */
/*     routine DAFTB. NAIF supports this routine only to provide backward */
/*     compatibility. */

/*     Convert an ASCII (text) DAF to an equivalent binary DAF. */

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

/*     FILES */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     ASCII      I   Name of an existing ASCII (text) DAF. */
/*     BINARY     I   Name of a binary DAF to be created. */
/*     RESV       I   Number of records to reserve. */

/* $ Detailed_Input */

/*     ASCII    is the name of an existing ASCII (text) DAF. */

/*     BINARY   is the name of the binary DAF to be created. */
/*              The binary DAF contains the same data as the */
/*              ASCII DAF, but in a form more suitable for use */
/*              by application programs. */

/*     RESV     is the number of records to be reserved in the */
/*              binary DAF. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If an error occurs while converting the input ASCII text DAF */
/*         file to binary format, the error is signaled by a routine in */
/*         the call tree of this routine. */

/*     2)  If an error occurs while writing data to the output binary DAF */
/*         file, the error is signaled by a routine in the call tree of */
/*         this routine. */

/* $ Files */

/*     See arguments ASCII, BINARY. */

/* $ Particulars */

/*     This routine has been made obsolete by the new DAF text to binary */
/*     conversion routine DAFTB. This routine remains available for */
/*     reasons of backward compatibility. We strongly recommend that the */
/*     conversion routine DAFTB be used for any new software development. */
/*     Please see the header of the routine DAFTB for details. */

/*     This routine is used for converting older DAF text files, which */
/*     use a decimal format for numbers, into their equivalent binary */
/*     formats. Note that the routine DAFTB makes use of a text file */
/*     format that is incompatible with the text file format expected by */
/*     the routines called by this routine. */

/*     Note that you must select the number of records to be reserved */
/*     in the binary DAF. The contents of reserved records are ignored */
/*     by the normal transfer process. */

/* $ Examples */

/*     DAFB2A and DAFA2B are typically used to transfer files. */
/*     If file A.DAF is a binary DAF in environment 1, it */
/*     can be transferred to environment 2 in three steps. */

/*        1) Convert it to ASCII, */

/*              CALL DAFB2A ( 'A.DAF', 'A.ASCII' ) */

/*        2) Transfer the ASCII file, using FTP, Kermit, or some other */
/*           file transfer utility, */

/*              ftp> put a.ascii */

/*        3) Convert it to binary on the new machine, */

/*              CALL DAFA2B ( 'A.ASCII', 'A.DAF', RESV ) */

/*     Note that DAFB2A and DAFA2B work in any standard Fortran-77 */
/*     environment. */

/* $ Restrictions */

/*     1)  DAFA2B cannot be executed while any other DAF is open */
/*         for writing. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     H.A. Neilan        (JPL) */
/*     W.L. Taber         (JPL) */
/*     I.M. Underwood     (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.1.0, 26-OCT-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Updated */
/*        $Exceptions section to better describe the issues detected by */
/*        this routine. Moved DAF required reading from */
/*        $Literature_References to $Required_Reading section. */

/* -    SPICELIB Version 2.0.1, 26-JUL-2012 (EDW) */

/*        Edited $Abstract section to use "Deprecated" keyword */
/*        and state replacement routine. */

/*        Eliminated unneeded $Revisions section. */

/* -    SPICELIB Version 2.0.0, 30-SEP-1993 (KRG) */

/*        This routine was completely rewritten to make use of the */
/*        routines DAFT2B and TXTOPR, for converting a text file to */
/*        binary and opening a text file. It now simply calls the */
/*        routine DAFT2B after opening the text file. */

/*        Added a statement to the $Particulars section to the effect */
/*        that this routine has been made obsolete by the introduction of */
/*        the routine DAFTB, and that the use of the new routine is */
/*        strongly recommended for new software development. */

/*        Modified the $Abstract section to reflect the fact that this */
/*        routine is obsolete. */

/* -    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN) */

/*        Literature references added to the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) */

/* -& */
/* $ Index_Entries */

/*     DEPRECATED ascii DAF to binary */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("DAFA2B", (ftnlen)6);
    }

/*     Open the ASCII file for reading. If an error occurs, then check */
/*     out and return. An appropriate error message will have already */
/*     been set. */

    txtopr_(ascii, &unit, ascii_len);
    if (failed_()) {
	chkout_("DAFA2B", (ftnlen)6);
	return 0;
    }

/*     Call DAFT2B to perform the conversion. If it fails, then just */
/*     check out and return, as an appropriate error message should have */
/*     already been set. Also close the text file that we opened. */

    daft2b_(&unit, binary, resv, binary_len);
    if (failed_()) {
	cl__1.cerr = 0;
	cl__1.cunit = unit;
	cl__1.csta = 0;
	f_clos(&cl__1);
	chkout_("DAFA2B", (ftnlen)6);
	return 0;
    }

/*     Close the file. */

    cl__1.cerr = 0;
    cl__1.cunit = unit;
    cl__1.csta = 0;
    f_clos(&cl__1);
    chkout_("DAFA2B", (ftnlen)6);
    return 0;
} /* dafa2b_ */

