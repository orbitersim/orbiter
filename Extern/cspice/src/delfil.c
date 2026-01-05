/* delfil.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure DELFIL ( Delete a file  ) */
/* Subroutine */ int delfil_(char *filnam, ftnlen filnam_len)
{
    /* System generated locals */
    olist o__1;
    cllist cl__1;
    inlist ioin__1;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen), f_inqu(inlist *), f_open(
	    olist *), f_clos(cllist *);

    /* Local variables */
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen);
    integer lunit;
    logical opened;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), getlun_(integer *), setmsg_(char *, ftnlen);
    integer iostat;
    extern /* Subroutine */ int errint_(char *, integer *, ftnlen);
    extern logical return_(void);
    logical exists;

/* $ Abstract */

/*     Delete a file. */

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
/*     FILNAM     I   The name of a file to be deleted. */

/* $ Detailed_Input */

/*     FILNAM   is the name of a file that is to be deleted. Upon */
/*              successful completion of this routine this file will */
/*              no longer exist. The file to be deleted must be closed */
/*              when this routine is called. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the file name is blank, the error SPICE(BLANKFILENAME) */
/*         is signaled. */

/*     2)  If the inquire on the filename specified by FILNAM fails for */
/*         some reason, the error SPICE(INQUIREERROR) is signaled. */

/*     3)  If the file specified by FILNAM is already open, the error */
/*         SPICE(FILECURRENTLYOPEN) is signaled. */

/*     4)  If the file specified by FILNAM does not exist, the error */
/*         SPICE(NOSUCHFILE) is signaled. */

/*     5)  If the attempt to open the file specified by FILNAM fails, */
/*         the error SPICE(FILEOPENFAILED) is signaled. */

/*     6)  If the attempt to close the file with STATUS='DELETE' fails, */
/*         the error SPICE(FILEDELETEFAILED) is signaled. */

/* $ Files */

/*     The  file specified by FILNAM is opened and then closed by this */
/*     routine with STATUS = 'DELETE' to delete it. The file must be */
/*     closed for this routine to delete it. */

/* $ Particulars */

/*     This subroutine is a support utility that deletes a file. */

/* $ Examples */

/*     Suppose you wish to delete a file named 'delete.me' in the */
/*     current directory. The code fragment below would accomplish this. */

/*        FILE = 'delete.me' */
/*        CALL DELFIL ( FILE ) */

/* $ Restrictions */

/*     1)  The file to be deleted must be closed when this routine is */
/*         invoked. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 02-JUN-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.0, 20-DEC-1995 (KRG) */

/* -& */
/* $ Index_Entries */

/*     delete a file */

/* -& */

/*     Spicelib Routines */


/*     Local Variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("DELFIL", (ftnlen)6);
    }

/*     Check to see if the filename we got is blank. If it is, signal an */
/*     error and return. */

    if (s_cmp(filnam, " ", filnam_len, (ftnlen)1) == 0) {
	setmsg_("The file name is blank.", (ftnlen)23);
	sigerr_("SPICE(BLANKFILENAME)", (ftnlen)20);
	chkout_("DELFIL", (ftnlen)6);
	return 0;
    }

/*     We inquire before we try opening anything to see if the file */
/*     exists or is currently open. */

    ioin__1.inerr = 1;
    ioin__1.infilen = filnam_len;
    ioin__1.infile = filnam;
    ioin__1.inex = &exists;
    ioin__1.inopen = &opened;
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

/*     Not too likely, but if the INQUIRE statement fails signal an error */
/*     and return. */

    if (iostat != 0) {
	setmsg_("INQUIRE statement failed for file '#'. IOSTAT = #.", (ftnlen)
		50);
	errch_("#", filnam, (ftnlen)1, filnam_len);
	errint_("#", &iostat, (ftnlen)1);
	sigerr_("SPICE(INQUIREFAILED)", (ftnlen)20);
	chkout_("DELFIL", (ftnlen)6);
	return 0;
    }

/*     The file ought to exist if you're trying to delete it. If not, */
/*     signal an error and return. */

    if (! exists) {
	setmsg_("The file '#' does not exist.", (ftnlen)28);
	errch_("#", filnam, (ftnlen)1, filnam_len);
	sigerr_("SPICE(NOSUCHFILE)", (ftnlen)17);
	chkout_("DELFIL", (ftnlen)6);
	return 0;
    }

/*     The file that is to be deleted should not be in use, indicated by */
/*     it being open, by anything when we try to delete it. If it is */
/*     open, signal an error and return. */

    if (opened) {
	setmsg_("The file '#' is currently open and cannot be deleted.", (
		ftnlen)53);
	errch_("#", filnam, (ftnlen)1, filnam_len);
	sigerr_("SPICE(FILECURRENTLYOPEN)", (ftnlen)24);
	chkout_("DELFIL", (ftnlen)6);
	return 0;
    }

/*     Get an available logical unit and attempt to open the file. */

    getlun_(&lunit);
    o__1.oerr = 1;
    o__1.ounit = lunit;
    o__1.ofnmlen = filnam_len;
    o__1.ofnm = filnam;
    o__1.orl = 0;
    o__1.osta = "OLD";
    o__1.oacc = 0;
    o__1.ofm = 0;
    o__1.oblnk = 0;
    iostat = f_open(&o__1);

/*     If we had trouble opening the file, signal an appropriate error */
/*     and return. */

    if (iostat != 0) {
	setmsg_("Attempt to open the file '#' failed.", (ftnlen)36);
	errch_("#", filnam, (ftnlen)1, filnam_len);
	sigerr_("SPICE(FILEOPENFAILED)", (ftnlen)21);
	chkout_("DELFIL", (ftnlen)6);
	return 0;
    }

/*     We opened the file successfully, so let's try to close it with */
/*     STATUS = 'DELETE'. If this fails, attempt to just close the file, */
/*     signal an error and return. */

    cl__1.cerr = 1;
    cl__1.cunit = lunit;
    cl__1.csta = "DELETE";
    iostat = f_clos(&cl__1);
    if (iostat != 0) {
	cl__1.cerr = 0;
	cl__1.cunit = lunit;
	cl__1.csta = 0;
	f_clos(&cl__1);
	setmsg_("Attempt to delete the file '#' failed.", (ftnlen)38);
	errch_("#", filnam, (ftnlen)1, filnam_len);
	sigerr_("SPICE(FILEDELETEFAILED)", (ftnlen)23);
	chkout_("DELFIL", (ftnlen)6);
	return 0;
    }
    chkout_("DELFIL", (ftnlen)6);
    return 0;
} /* delfil_ */

