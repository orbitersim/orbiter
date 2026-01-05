/* fn2lun.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure FN2LUN ( Map name of open file to its logical unit. ) */
/* Subroutine */ int fn2lun_(char *filnam, integer *lunit, ftnlen filnam_len)
{
    /* System generated locals */
    inlist ioin__1;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen), f_inqu(inlist *);

    /* Local variables */
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen);
    logical opened;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen);
    integer iostat;
    extern /* Subroutine */ int errint_(char *, integer *, ftnlen);
    extern logical return_(void);
    logical exists;

/* $ Abstract */

/*     Map the name of an open file to its associated logical unit. */

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
/*     FILNAM     I   Name of the file to be mapped to its logical unit. */
/*     LUNIT      O   The logical unit associated with the filename. */

/* $ Detailed_Input */

/*     FILNAM   is the filename that is to be mapped to its associated */
/*              Fortran logical unit. */

/* $ Detailed_Output */

/*     LUNIT    is the Fortran logical unit that is associated with the */
/*              filename FILNAM. The file must be open for this routine */
/*              to work properly. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the filename is blank, the error SPICE(BLANKFILENAME) is */
/*         signaled. */

/*     2)  If an error occurs during the execution of the Fortran INQUIRE */
/*         statement, the error SPICE(INQUIREFAILED) is signaled. */

/*     3)  If the filename is not associated with an open file, the */
/*         error SPICE(FILENOTOPEN) is signaled. */

/*     4)  If the filename is not associated with an existing file, the */
/*         error SPICE(FILEDOESNOTEXIST) is signaled. */

/*     5)  In the event of an error the contents of the variable LUNIT */
/*         are not defined, and should not be used. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Use the Fortran INQUIRE statement to determine the filename */
/*     that is associated with the Fortran logical unit LUNIT. */

/* $ Examples */

/*     The following code fragment illustrates the use of FN2LUN. */

/*     C */
/*     C      Convert the logical unit to its filename and display it. */
/*     C */
/*            CALL FN2LUN ( FNAME, LUNIT ) */
/*            WRITE (*,*) 'The logical unit is: ', LUNIT */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.0, 16-AUG-1994 (KRG) */

/* -& */
/* $ Index_Entries */

/*     map filename to logical unit */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("FN2LUN", (ftnlen)6);
    }

/*     First we test to see if the filename is blank. */

    if (s_cmp(filnam, " ", filnam_len, (ftnlen)1) == 0) {
	setmsg_("The filename is blank.", (ftnlen)22);
	sigerr_("SPICE(BLANKFILENAME)", (ftnlen)20);
	chkout_("FN2LUN", (ftnlen)6);
	return 0;
    }

/*     So simple, it defies explanation: just INQUIRE. */

    ioin__1.inerr = 1;
    ioin__1.infilen = filnam_len;
    ioin__1.infile = filnam;
    ioin__1.inex = &exists;
    ioin__1.inopen = &opened;
    ioin__1.innum = &*lunit;
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
	setmsg_("INQUIRE error on file '#'. The value of IOSTAT is: #.", (
		ftnlen)53);
	errch_("#", filnam, (ftnlen)1, filnam_len);
	errint_("#", &iostat, (ftnlen)1);
	sigerr_("SPICE(INQUIREFAILED)", (ftnlen)20);
	chkout_("FN2LUN", (ftnlen)6);
	return 0;
    }

/*     A file cannot be open if it does not exist. We need to check this */
/*     because for some environments files are considered to be open if */
/*     they do not exist. */

    if (! exists) {
	setmsg_("No file with the name '#' was found.", (ftnlen)36);
	errch_("#", filnam, (ftnlen)1, filnam_len);
	sigerr_("SPICE(FILEDOESNOTEXIST)", (ftnlen)23);
	chkout_("FN2LUN", (ftnlen)6);
	return 0;
    }

/*     Now check to see if the file is opened. If not, then it is an */
/*     error, there cannot be a logical unit associated with it.. */

    if (! opened) {
	setmsg_("There was not an open file associated with the filename '#'."
		, (ftnlen)60);
	errch_("#", filnam, (ftnlen)1, filnam_len);
	sigerr_("SPICE(FILENOTOPEN)", (ftnlen)18);
	chkout_("FN2LUN", (ftnlen)6);
	return 0;
    }
    chkout_("FN2LUN", (ftnlen)6);
    return 0;
} /* fn2lun_ */

