/* isopen.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ISOPEN ( Is a file currently open? ) */
logical isopen_(char *file, ftnlen file_len)
{
    /* System generated locals */
    logical ret_val;
    inlist ioin__1;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen), f_inqu(inlist *);

    /* Local variables */
    extern /* Subroutine */ int chkin_(char *, ftnlen), sigerr_(char *, 
	    ftnlen), chkout_(char *, ftnlen), setmsg_(char *, ftnlen);
    integer iostat;
    extern /* Subroutine */ int errint_(char *, integer *, ftnlen);
    logical myopen;
    extern logical return_(void);
    logical exists;

/* $ Abstract */

/*     Determine whether a named file is currently open. */

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

/*     FILES */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     FILE       I   Name of the file in question. */

/*     The function returns the value .TRUE. if the file is open, .FALSE. */
/*     otherwise. */

/* $ Detailed_Input */

/*     FILE     is the name of the file in question. */

/* $ Detailed_Output */

/*     The function returns the value .TRUE. if the file is open, .FALSE. */
/*     otherwise. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the filename is blank, the error SPICE(BLANKFILENAME) is */
/*         signaled. */

/*     2)  If an error occurs during the execution of the Fortran INQUIRE */
/*         statement, the error SPICE(INQUIREFAILED) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Use the Fortran INQUIRE statement to determine the open status */
/*     of FILE. */

/* $ Examples */

/*     The following code fragment illustrates the use of ISOPEN. */

/*           IF ( .NOT. ISOPEN ( FILE ) ) THEN */
/*              Open the file here */
/*           ELSE */
/*              ERROR = 'Input file is already open.' */
/*           END IF */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.0, 12-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.1.0, 29-FEB-1996 (KRG) */

/*        Added a local logical variable that is used as temporary */
/*        storage for the results from the INQUIRE statement rather */
/*        than using the function name. This solved a problem on the */
/*        macintosh. */

/* -    SPICELIB Version 1.0.0, 05-OCT-1994 (KRG) */

/* -& */
/* $ Index_Entries */

/*     test for file already open */
/*     is a file open */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	ret_val = FALSE_;
	return ret_val;
    } else {
	chkin_("ISOPEN", (ftnlen)6);
    }

/*     First we test to see if the filename is blank. */

    if (s_cmp(file, " ", file_len, (ftnlen)1) == 0) {
	ret_val = FALSE_;
	setmsg_("The file name is blank. ", (ftnlen)24);
	sigerr_("SPICE(BLANKFILENAME)", (ftnlen)20);
	chkout_("ISOPEN", (ftnlen)6);
	return ret_val;
    }

/*     So simple, it defies explanation. */

    ioin__1.inerr = 1;
    ioin__1.infilen = file_len;
    ioin__1.infile = file;
    ioin__1.inex = &exists;
    ioin__1.inopen = &myopen;
    ioin__1.innum = 0;
    ioin__1.innamed = 0;
    ioin__1.inname = 0;
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
    if (iostat != 0) {
	ret_val = FALSE_;
	setmsg_("Value of IOSTAT was *.", (ftnlen)22);
	errint_("*", &iostat, (ftnlen)1);
	sigerr_("SPICE(INQUIREFAILED)", (ftnlen)20);
	chkout_("ISOPEN", (ftnlen)6);
	return ret_val;
    }

/*     A file cannot be open if it does not exist. We do actually need to */
/*     check this because some operating environments return .TRUE. for */
/*     the value of OPENED if a file does not exist. */

    if (! exists) {
	myopen = FALSE_;
    }

/*     Set the function value, check out, and return. */

    ret_val = myopen;
    chkout_("ISOPEN", (ftnlen)6);
    return ret_val;
} /* isopen_ */

