/* dasdc.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure DASDC    ( DAS delete comments ) */
/* Subroutine */ int dasdc_(integer *handle)
{
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer ncomc, ncomr;
    extern logical failed_(void);
    char ifname[60];
    extern /* Subroutine */ int dassih_(integer *, char *, ftnlen), dasrcr_(
	    integer *, integer *), dasrfr_(integer *, char *, char *, integer 
	    *, integer *, integer *, integer *, ftnlen, ftnlen), daswfr_(
	    integer *, char *, char *, integer *, integer *, integer *, 
	    integer *, ftnlen, ftnlen);
    char idword[8];
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    integer nresvc;
    extern logical return_(void);
    integer nresvr;

/* $ Abstract */

/*     Delete the entire comment area of a previously opened binary */
/*     DAS file. */

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

/*     DAS */

/* $ Keywords */

/*     None. */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   The handle of a binary DAS file opened for writing. */

/* $ Detailed_Input */

/*     HANDLE   is the handle of a binary DAS file that is to have its */
/*              entire comment area deleted. The DAS file should have */
/*              been opened with write access. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the binary DAS file attached to HANDLE is not open with */
/*         write access, an error is signaled by a routine in the call */
/*         tree of this routine. */

/* $ Files */

/*     See argument HANDLE in $Detailed_Input. */

/* $ Particulars */

/*     Binary DAS files contain an area which is reserved for storing */
/*     annotations or descriptive textual information about the data */
/*     contained in a file. This area is referred to as the ``comment */
/*     area'' of the file. The comment area of a DAS file is a line */
/*     oriented medium for storing textual information. The comment area */
/*     preserves any leading or embedded white space in the line(s) of */
/*     text which are stored, so that the appearance of the information */
/*     will be unchanged when it is retrieved (extracted) at some other */
/*     time. Trailing blanks, however, are NOT preserved, due to the way */
/*     that character strings are represented in standard Fortran 77. */

/*     This routine will delete the entire comment area from the binary */
/*     DAS file attached to HANDLE. The size of the binary DAS file will */
/*     remain unchanged. The space that was used by the comment records */
/*     is reclaimed. */

/* $ Examples */

/*     Let */

/*           HANDLE   be the handle for a DAS file which has been opened */
/*                    with write access. */

/*     The call */

/*           CALL DASDC ( HANDLE ) */

/*     will delete the entire comment area of the binary DAS file */
/*     attached to HANDLE. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 02-JUN-2021 (JDR) */

/*        Added IMPLICIT NONE standard. */

/*        Edited the header to comply with NAIF standard. Removed */
/*        unnecessary $Revisions section. */

/* -    SPICELIB Version 1.0.2, 11-NOV-2016 (NJB) */

/*        Fixed typo in $Particulars header section. */

/* -    SPICELIB Version 1.0.1, 26-OCT-1993 (KRG) */

/*        Changed the $Brief_I/O description of handle. It now mentions */
/*        that the file must be open for writing. Also added a statement */
/*        to the $Detailed_Input section to the effect that the DAS file */
/*        should have been opened with write access. */

/* -    SPICELIB Version 1.0.0, 24-NOV-1992 (KRG) */

/* -& */
/* $ Index_Entries */

/*     delete DAS comment area */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */

/*     Length of a DAS file ID word. */


/*     Length of a DAS file internal filename. */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("DASDC", (ftnlen)5);
    }

/*     Verify that the DAS file attached to HANDLE is opened with write */
/*     access. */

    dassih_(handle, "WRITE", (ftnlen)5);
    if (failed_()) {
	chkout_("DASDC", (ftnlen)5);
	return 0;
    }

/*     Read the file record to obtain the current number of comment */
/*     records in the DAS file attached to HANDLE. We will also get */
/*     back some extra stuff that we do not use. */

    dasrfr_(handle, idword, ifname, &nresvr, &nresvc, &ncomr, &ncomc, (ftnlen)
	    8, (ftnlen)60);
    if (failed_()) {
	chkout_("DASDC", (ftnlen)5);
	return 0;
    }

/*     Now we will attempt to remove the comment records, if there are */
/*     any, otherwise we do nothing. */

    if (ncomr > 0) {
	dasrcr_(handle, &ncomr);
	if (failed_()) {
	    chkout_("DASDC", (ftnlen)5);
	    return 0;
	}

/*        Now we need to update the DAS file record. */

/*        Read in the updated file record since it has been modified: */
/*        we deleted all of the comment records. */

	dasrfr_(handle, idword, ifname, &nresvr, &nresvc, &ncomr, &ncomc, (
		ftnlen)8, (ftnlen)60);
	if (failed_()) {
	    chkout_("DASDC", (ftnlen)5);
	    return 0;
	}

/*        Zero out the number of comment characters, and write the */
/*        updated file record to the file. */

	ncomc = 0;
	daswfr_(handle, idword, ifname, &nresvr, &nresvc, &ncomr, &ncomc, (
		ftnlen)8, (ftnlen)60);
	if (failed_()) {
	    chkout_("DASDC", (ftnlen)5);
	    return 0;
	}
    }

/*     We're done now, so goodbye. */

    chkout_("DASDC", (ftnlen)5);
    return 0;
} /* dasdc_ */

