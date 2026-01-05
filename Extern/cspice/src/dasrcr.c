/* dasrcr.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static logical c_false = FALSE_;
static integer c__3 = 3;
static integer c__256 = 256;

/* $Procedure DASRCR ( DAS, remove comment records ) */
/* Subroutine */ int dasrcr_(integer *handle, integer *n)
{
    /* Initialized data */

    static integer next[3] = { 2,3,1 };
    static integer prev[3] = { 3,1,2 };

    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer base;
    char recc[1024];
    doublereal recd[128];
    integer free, reci[256], lrec, nrec, unit, type__;
    extern /* Subroutine */ int zzddhhlu_(integer *, char *, logical *, 
	    integer *, ftnlen);
    integer i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer ncomc;
    extern /* Subroutine */ int maxai_(integer *, integer *, integer *, 
	    integer *);
    integer ncomr, lword;
    extern logical failed_(void);
    extern /* Subroutine */ int cleari_(integer *, integer *), dasioc_(char *,
	     integer *, integer *, char *, ftnlen, ftnlen), dasiod_(char *, 
	    integer *, integer *, doublereal *, ftnlen);
    integer dirrec[256];
    extern /* Subroutine */ int dashfs_(integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, integer *),
	     dasioi_(char *, integer *, integer *, integer *, ftnlen), 
	    dassih_(integer *, char *, ftnlen);
    integer lastla[3];
    extern /* Subroutine */ int daswbr_(integer *);
    integer lindex;
    extern /* Subroutine */ int dasufs_(integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, integer *);
    integer lastrc[3], nshift;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    integer lastwd[3], nresvc;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen);
    extern logical return_(void);
    integer nresvr, loc, pos;

/* $ Abstract */

/*     Decrease the size of the comment area in a DAS file to reclaim */
/*     space freed by the removal of a specified number of comment */
/*     records. */

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

/*     DAS */
/*     FILES */
/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   A DAS file handle. */
/*     N          I   Number of comment records to remove. */

/* $ Detailed_Input */

/*     HANDLE   is the handle of an existing DAS file opened for */
/*              comment area modification by DASOPC. */

/*     N        is the number of records to remove from the end of */
/*              the comment area. of the specified file. If NCOMR */
/*              is the number of comment records present in the */
/*              file on input, then on output the number of comment */
/*              records will be MAX ( 0,  NCOMR - N ). */

/* $ Detailed_Output */

/*     None. See $Particulars for a description of the effect of this */
/*     routine. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the input handle is invalid, an error is signaled by a */
/*         routine in the call tree of this routine. */

/*     2)  If an I/O error occurs during the removal process, the error */
/*         is signaled by a routine in the call tree of this routine. The */
/*         DAS file will probably be corrupted in this case. */

/* $ Files */

/*     See the description of the argument HANDLE in $Detailed_Input. */

/* $ Particulars */

/*     This routine is used to reclaim freed space in the comment area */
/*     of a DAS file subsequent to removal of comments from the file. */
/*     Any existing directory records and data records will be shifted */
/*     up by N records. */

/*     This routine updates the file record of the specified DAS file */
/*     to reflect the addition of records to the file's comment area. */
/*     Also, the file summary obtainable from DASHFS will be updated to */
/*     reflect the addition of comment records. */

/*     The disk space occupied by the specified DAS file will not */
/*     decrease as a result of calling this routine, but the number of */
/*     records occupied by meaningful data will decrease. The useful */
/*     records in the file can be copied by DAS routines to create a */
/*     new, smaller file which contains only the meaningful data. */

/*     This routine may be used only on existing DAS files opened by */
/*     DASOPC. */

/*     The association of DAS logical addresses and data within the */
/*     specified file will remain unaffected by use of this routine. */

/*     Normally, SPICELIB applications will not call this routine */
/*     directly, but will remove comments by calling DASRC. */

/*     This routine has an inverse DASACR, which appends a specified */
/*     number of records to the end of the comment area. */

/* $ Examples */

/*     C */
/*     C     Open an existing DAS file for modification of */
/*     C     the comment area. We'll presume that the file */
/*     C     contains 20 comment records. */
/*     C */
/*           CALL DASOPC ( DAS, HANDLE ) */

/*     C */
/*     C     Remove the last 10 comment records from the file. */
/*     C */
/*           CALL DASRCR ( HANDLE, 10  ) */

/*     C */
/*     C     Close the file. */
/*     C */
/*           CALL DASCLS ( HANDLE ) */

/* $ Restrictions */

/*     1)  The DAS file must have a binary file format native to the host */
/*         system. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.3.0, 02-JUN-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.2.0, 05-FEB-2015 (NJB) */

/*        Updated to support integration with the handle */
/*        manager subsystem. */

/*        Cleaned up use of unnecessary variables and unneeded */
/*        declarations. */

/* -    SPICELIB Version 1.0.0, 15-NOV-1992 (NJB) (WLT) */

/* -& */
/* $ Index_Entries */

/*     remove comment records from a DAS file */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Words per data record, for each data type: */


/*     Data type parameters */


/*     Directory pointer location (forward): */


/*     Location of first type descriptor */


/*     Local variables */


/*     Saved variables */


/*     NEXT and PREV map the DAS data type codes to their */
/*     successors and predecessors, respectively. */


/*     Initial values */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("DASRCR", (ftnlen)6);

/*     Make sure this DAS file is open for writing.  Signal an error if */
/*     not. */

    dassih_(handle, "WRITE", (ftnlen)5);

/*     Get the logical unit for this DAS file. */

    zzddhhlu_(handle, "DAS", &c_false, &unit, (ftnlen)3);
    if (failed_()) {
	chkout_("DASRCR", (ftnlen)6);
	return 0;
    }

/*     It's a mistake to use a negative value of N. */

    if (*n < 0) {
	setmsg_("Number of comment records to remove must be non-negative.  "
		"Actual number requested was #.", (ftnlen)89);
	errint_("#", n, (ftnlen)1);
	sigerr_("SPICE(DASINVALIDCOUNT)", (ftnlen)22);
	chkout_("DASRCR", (ftnlen)6);
	return 0;
    }

/*     Before doing anything to the file, make sure that the DASRWR */
/*     data buffers do not contain any updated records for this file. */
/*     All of the record numbers that pertain to this file and remain */
/*     in the DASRWR buffers will be invalidated after this routine */
/*     returns. */

/*     DASWBR flushes buffered records to the file. */

    daswbr_(handle);

/*     Grab the file summary for this DAS file.  Find the number of */
/*     reserved records and the number of the first free record. */

    dashfs_(handle, &nresvr, &nresvc, &ncomr, &ncomc, &free, lastla, lastrc, 
	    lastwd);

/*     Determine the size of the record shift we'll actually perform. */

    nshift = min(*n,ncomr);

/*     Find the record and word positions LREC and LWORD of the last */
/*     descriptor in the file. */

    maxai_(lastrc, &c__3, &lrec, &loc);
    lword = 0;
    for (i__ = 1; i__ <= 3; ++i__) {
	if (lastrc[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("lastrc",
		 i__1, "dasrcr_", (ftnlen)361)] == lrec && lastwd[(i__2 = i__ 
		- 1) < 3 && 0 <= i__2 ? i__2 : s_rnge("lastwd", i__2, "dasrc"
		"r_", (ftnlen)361)] > lword) {
	    lword = lastwd[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge(
		    "lastwd", i__1, "dasrcr_", (ftnlen)364)];
	}
    }

/*     LREC and LWORD are now the record and word index of the last */
/*     descriptor in the file. If LREC is zero, there are no directories */
/*     in the file yet. However, even DAS files that don't contain any */
/*     data have their first directory records zeroed out, and this */
/*     should remain true after the removal of the comment records. */

    if (lrec == 0) {

/*        Just write the zero-filled record to record number */

/*           NRESVR + NCOMR + 2 - NSHIFT */

	cleari_(&c__256, dirrec);
	i__1 = nresvr + ncomr + 2 - nshift;
	dasioi_("WRITE", &unit, &i__1, dirrec, (ftnlen)5);
    } else {

/*        There really is stuff to move.  For each directory record, */
/*        move the record and then all of the records described by that */
/*        record.  We start at the beginning of the data area and move */
/*        downwards in the file as we go. */

	nrec = nresvr + ncomr + 2;
	while(nrec <= lrec && nrec != 0) {

/*           Read the current directory record and move it. */

	    dasioi_("READ", &unit, &nrec, dirrec, (ftnlen)4);
	    i__1 = nrec - nshift;
	    dasioi_("WRITE", &unit, &i__1, dirrec, (ftnlen)5);

/*           For each descriptor in the current directory, move the */
/*           cluster of data records it refers to. */

/*           Find the data type, size, and base record number of the */
/*           first cluster described by the current directory.  Also */
/*           find the index within the directory of the directory's */
/*           last descriptor. */

	    type__ = dirrec[8];
	    base = nrec + 1;
	    if (nrec == lrec) {
		lindex = lword;
	    } else {
		lindex = 256;
	    }

/*           We'll now traverse the directory in forward order, keeping */
/*           track of cluster sizes and types as we go. */

/*           POS will be the index of the descriptor of the current */
/*           cluster. */

	    pos = 10;
	    while(pos <= lindex) {
		if (pos > 10) {

/*                 We'll need to determine the type of the current */
/*                 cluster.  If the descriptor contains a positive */
/*                 value, the data type of the cluster it refers to is */
/*                 the successor of the previous type, according to our */
/*                 ordering of types. */

		    if (dirrec[(i__1 = pos - 1) < 256 && 0 <= i__1 ? i__1 : 
			    s_rnge("dirrec", i__1, "dasrcr_", (ftnlen)439)] > 
			    0) {
			type__ = next[(i__1 = type__ - 1) < 3 && 0 <= i__1 ? 
				i__1 : s_rnge("next", i__1, "dasrcr_", (
				ftnlen)440)];
		    } else {
			type__ = prev[(i__1 = type__ - 1) < 3 && 0 <= i__1 ? 
				i__1 : s_rnge("prev", i__1, "dasrcr_", (
				ftnlen)442)];
		    }

/*                 Update the cluster base record number. */

		    base += (i__2 = dirrec[(i__1 = pos - 2) < 256 && 0 <= 
			    i__1 ? i__1 : s_rnge("dirrec", i__1, "dasrcr_", (
			    ftnlen)448)], abs(i__2));
		}

/*              BASE and TYPE now are correctly set for the current */
/*              cluster.  Move the cluster. */

		i__3 = base + (i__2 = dirrec[(i__1 = pos - 1) < 256 && 0 <= 
			i__1 ? i__1 : s_rnge("dirrec", i__1, "dasrcr_", (
			ftnlen)456)], abs(i__2)) - 1;
		for (i__ = base; i__ <= i__3; ++i__) {
		    if (type__ == 1) {
			dasioc_("READ", &unit, &i__, recc, (ftnlen)4, (ftnlen)
				1024);
			i__1 = i__ - nshift;
			dasioc_("WRITE", &unit, &i__1, recc, (ftnlen)5, (
				ftnlen)1024);
		    } else if (type__ == 2) {
			dasiod_("READ", &unit, &i__, recd, (ftnlen)4);
			i__1 = i__ - nshift;
			dasiod_("WRITE", &unit, &i__1, recd, (ftnlen)5);
		    } else {
			dasioi_("READ", &unit, &i__, reci, (ftnlen)4);
			i__1 = i__ - nshift;
			dasioi_("WRITE", &unit, &i__1, reci, (ftnlen)5);
		    }
		}

/*              The next descriptor to look at is the next one in the */
/*              current directory. */

		++pos;
	    }

/*           Find the next directory record. */

	    nrec = dirrec[0];
	}
    }

/*     Update the file summary.  The number of comment records and the */
/*     number of the first free record have been decremented by NSHIFT. */
/*     The numbers of the records containing the last descriptor of each */
/*     type have been decremented by NSHIFT only if they were non-zero. */


/*     The call to DASUFS will update the file record as well as the */
/*     file summary. */

    ncomr -= nshift;
    free -= nshift;
    for (i__ = 1; i__ <= 3; ++i__) {
	if (lastrc[(i__3 = i__ - 1) < 3 && 0 <= i__3 ? i__3 : s_rnge("lastrc",
		 i__3, "dasrcr_", (ftnlen)509)] != 0) {
	    lastrc[(i__3 = i__ - 1) < 3 && 0 <= i__3 ? i__3 : s_rnge("lastrc",
		     i__3, "dasrcr_", (ftnlen)510)] = lastrc[(i__1 = i__ - 1) 
		    < 3 && 0 <= i__1 ? i__1 : s_rnge("lastrc", i__1, "dasrcr_"
		    , (ftnlen)510)] - nshift;
	}
    }
    dasufs_(handle, &nresvr, &nresvc, &ncomr, &ncomc, &free, lastla, lastrc, 
	    lastwd);
    chkout_("DASRCR", (ftnlen)6);
    return 0;
} /* dasrcr_ */

