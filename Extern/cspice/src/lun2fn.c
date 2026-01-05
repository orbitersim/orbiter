/* lun2fn.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure LUN2FN ( Map logical unit of open file to its name. ) */
/* Subroutine */ int lun2fn_(integer *lunit, char *filnam, ftnlen filnam_len)
{
    /* System generated locals */
    inlist ioin__1;

    /* Builtin functions */
    integer f_inqu(inlist *);

    /* Local variables */
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    logical opened;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen);
    integer iostat;
    extern /* Subroutine */ int errint_(char *, integer *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Map the logical unit of an open file to its associated filename. */

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
/*     LUNIT      I   A logical unit to be mapped to a filename. */
/*     FILNAM     O   Name of the file associated with LUNIT. */

/* $ Detailed_Input */

/*     LUNIT    is the Fortran logical unit that is to be mapped to the */
/*              filename with which it is associated. The file must be */
/*              open for this routine to work properly. */

/* $ Detailed_Output */

/*     FILNAM   is the filename that is associated with the Fortran */
/*              logical unit LUNIT. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the logical unit is not positive, the error */
/*         SPICE(INVALIDARGUMENT) is signaled. */

/*     2)  If an error occurs during the execution of the Fortran INQUIRE */
/*         statement, the error SPICE(INQUIREFAILED) is signaled. */

/*     3)  If the logical unit is not attached to an open file, the */
/*         error SPICE(FILENOTOPEN) is signaled. */

/*     4)  In the event of an error the contents of the variable FILNAM */
/*         are not defined and should not be used. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Uses the Fortran INQUIRE statement to determine the filename that */
/*     is associated with the Fortran logical unit LUNIT. */

/* $ Examples */

/*     The following code fragment illustrates the use of LUN2FN. */

/*     C */
/*     C      Convert the logical unit to its filename and display it. */
/*     C */
/*            CALL LUN2FN ( UNIT1, FNAME1 ) */
/*            WRITE (*,*) 'The filename is: ', FNAME1 */

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

/*     map logical unit to filename */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("LUN2FN", (ftnlen)6);
    }

/*     First we test to see if the filename is blank. */

    if (*lunit <= 0) {
	setmsg_("The Fortran logical unit was not positive: #.", (ftnlen)45);
	errint_("#", lunit, (ftnlen)1);
	sigerr_("SPICE(INVALIDARGUMENT)", (ftnlen)22);
	chkout_("LUN2FN", (ftnlen)6);
	return 0;
    }

/*     So simple, it defies explanation: just INQUIRE. */

    ioin__1.inerr = 1;
    ioin__1.inunit = *lunit;
    ioin__1.infile = 0;
    ioin__1.inex = 0;
    ioin__1.inopen = &opened;
    ioin__1.innum = 0;
    ioin__1.innamed = 0;
    ioin__1.innamlen = filnam_len;
    ioin__1.inname = filnam;
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
	setmsg_("An error occurred while INQUIRing on unit #. The IOSTAT val"
		"ue is #.", (ftnlen)67);
	errint_("#", lunit, (ftnlen)1);
	errint_("#", &iostat, (ftnlen)1);
	sigerr_("SPICE(INQUIREFAILED)", (ftnlen)20);
	chkout_("LUN2FN", (ftnlen)6);
	return 0;
    }

/*     If there is no open file associated with the logical unit LUNIT */
/*     we cannot get a filename. So signal an error. */

    if (! opened) {
	setmsg_("There was no open file associated with the logical unit #.", 
		(ftnlen)58);
	errint_("#", lunit, (ftnlen)1);
	sigerr_("SPICE(FILENOTOPEN)", (ftnlen)18);
	chkout_("LUN2FN", (ftnlen)6);
	return 0;
    }

/*     If we made it to here, we are done. Just check out and return. */

    chkout_("LUN2FN", (ftnlen)6);
    return 0;
} /* lun2fn_ */

