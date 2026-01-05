/* dasacr.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static logical c_false = FALSE_;
static integer c__3 = 3;
static integer c__256 = 256;

/* $Procedure DASACR ( DAS, add comment records ) */
/* Subroutine */ int dasacr_(integer *handle, integer *n)
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
    integer free, reci[256], lrec, nrec, prec, unit, type__;
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
	     dassih_(integer *, char *, ftnlen), dasioi_(char *, integer *, 
	    integer *, integer *, ftnlen);
    integer lastla[3];
    extern /* Subroutine */ int daswbr_(integer *);
    integer lindex;
    extern /* Subroutine */ int dasufs_(integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, integer *);
    integer lastrc[3];
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    integer lastwd[3], nresvc;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen);
    extern logical return_(void);
    integer nresvr, nxttyp, loc, pos;

/* $ Abstract */

/*     Increase the size of the comment area in a DAS file to accommodate */
/*     a specified number of additional comment records. */

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
/*     N          I   Number of comment records to append to the comment */
/*                    area of the specified file. */

/* $ Detailed_Input */

/*     HANDLE   is the handle of an existing DAS file opened for */
/*              comment area modification by DASOPC. */

/*     N        is the number of records to append to the comment */
/*              area. If NCOMR is the number of comment records */
/*              present in the file on input, then on output the */
/*              number of comment records will be NCOMR + N. */

/* $ Detailed_Output */

/*     None. See $Particulars for a description of the effect of this */
/*     routine. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the input handle is invalid, an error is signaled by a */
/*         routine in the call tree of this routine. */

/*     2)  If an I/O error occurs during the addition process, the error */
/*         is signaled by a routine in the call tree of this routine. The */
/*         DAS file will probably be corrupted in this case. */

/* $ Files */

/*     See the description of the argument HANDLE in $Detailed_Input. */

/* $ Particulars */

/*     This routine is used to create space in the comment area of a DAS */
/*     file to allow addition of comments to the file. If there are */
/*     comment records present in the file at the time this routine is */
/*     called, the number of comment records specified by the input */
/*     argument N will be appended to the existing comment records. */
/*     In any case, any existing directory records and data records will */
/*     be shifted down by N records. */

/*     This routine updates the file record of the specified DAS file */
/*     to reflect the addition of records to the file's comment area. */
/*     Also, the file summary obtainable from DASHFS will be updated to */
/*     reflect the addition of comment records. */

/*     This routine may be used only on existing DAS files opened by */
/*     DASOPW. */

/*     The association of DAS logical addresses and data within the */
/*     specified file will remain unaffected by use of this routine. */

/*     Normally, SPICELIB applications will not call this routine */
/*     directly, but will add comments by calling DASAC. */

/*     This routine has an inverse DASRCR, which removes a specified */
/*     number of records from the end of the comment area. */

/* $ Examples */

/*     1)  Make room for 10 comment records in the comment area of a */
/*         new DAS file. */

/*            C */
/*            C     Create a new DAS file. */
/*            C */
/*                  CALL DASOPW ( DAS, HANDLE ) */

/*            C */
/*            C     Now add 10 comment records to the comment area. */
/*            C */
/*                  CALL DASACR ( HANDLE, 10 ) */

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

/* -    SPICELIB Version 1.3.0, 13-AUG-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.2.0, 05-FEB-2015 (NJB) */

/*        Updated to support integration with the handle */
/*        manager subsystem. */

/*        Cleaned up use of unnecessary variables and unneeded */
/*        declarations. */

/* -    SPICELIB Version 1.1.0, 11-OCT-1996 (NJB) */

/*        Bug fix: backward and forward directory record pointers */
/*        are now updated when directory records are moved. */

/* -    SPICELIB Version 1.0.0, 01-FEB-1993 (NJB) (WLT) */

/* -& */
/* $ Index_Entries */

/*     add comment records to a DAS file */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.1.0, 11-OCT-1996 (NJB) */

/*        Bug fix: backward and forward directory record pointers */
/*        are now updated when directory records are moved. */

/*        Because these pointers are not used by the DAS software */
/*        once a DAS file is segregated, this bug had no effect on */
/*        DAS files that were created and closed via DASCLS, then */
/*        commented via the commnt utility. */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Words per data record, for each data type: */


/*     Data type parameters */


/*     Directory pointer locations (backward and forward): */


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
    chkin_("DASACR", (ftnlen)6);

/*     Programmer's note: the calls to */

/*        DASIOC */
/*        DASIOD */
/*        DASIOI */

/*     for read access are valid only for native format DAS files. */
/*     If this routine is updated to support writing to non-native */
/*     DAS files, at least the calls to the numeric I/O routines */
/*     will need to be replaced. (Consider using ZZDASGRD, ZZDASGRI.) */

/*     Make sure this DAS file is open for writing.  Signal an error if */
/*     not. */

    dassih_(handle, "WRITE", (ftnlen)5);

/*     Get the logical unit for this DAS file. */

    zzddhhlu_(handle, "DAS", &c_false, &unit, (ftnlen)3);
    if (failed_()) {
	chkout_("DASACR", (ftnlen)6);
	return 0;
    }

/*     It's a mistake to use a negative value of N. */

    if (*n < 0) {
	setmsg_("Number of comment records to add must be non-negative.  Act"
		"ual number requested was #.", (ftnlen)86);
	errint_("#", n, (ftnlen)1);
	sigerr_("SPICE(DASINVALIDCOUNT)", (ftnlen)22);
	chkout_("DASACR", (ftnlen)6);
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
/*     comment records and the number of the first free record. */

    dashfs_(handle, &nresvr, &nresvc, &ncomr, &ncomc, &free, lastla, lastrc, 
	    lastwd);

/*     Find the record and word positions LREC and LWORD of the last */
/*     descriptor in the file. */

    maxai_(lastrc, &c__3, &lrec, &loc);
    lword = 0;
    for (i__ = 1; i__ <= 3; ++i__) {
	if (lastrc[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("lastrc",
		 i__1, "dasacr_", (ftnlen)382)] == lrec && lastwd[(i__2 = i__ 
		- 1) < 3 && 0 <= i__2 ? i__2 : s_rnge("lastwd", i__2, "dasac"
		"r_", (ftnlen)382)] > lword) {
	    lword = lastwd[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge(
		    "lastwd", i__1, "dasacr_", (ftnlen)385)];
	}
    }

/*     LREC and LWORD are now the record and word index of the last word */
/*     of the last descriptor in the file. If LREC is zero, there are no */
/*     directories in the file yet. However, even DAS files that don't */
/*     contain any data have their first directory records zeroed out, */
/*     and this should remain true after the addition of the comment */
/*     records. */

    if (lrec == 0) {

/*        Just write the zero-filled record to record number */

/*           NRESVR + NCOMR + N + 2 */

	cleari_(&c__256, dirrec);
	i__1 = nresvr + ncomr + *n + 2;
	dasioi_("WRITE", &unit, &i__1, dirrec, (ftnlen)5);
    } else {

/*        There really is stuff to move.  For each directory record, */
/*        move all of the records described by that directory.  We start */
/*        with the last directory and work our way toward the beginning */
/*        of the file. */

	nrec = lrec;
	while(nrec > 0) {

/*           For each descriptor in the current directory, move the */
/*           cluster of data records it refers to. */

/*           Read the current directory record. */

	    dasioi_("READ", &unit, &nrec, dirrec, (ftnlen)4);

/*           Find the data type, size, and base record number of the */
/*           last cluster in the current directory.  To do this, */
/*           traverse the directory record, keeping track of the record */
/*           count and data types of descriptors as we go. */

	    type__ = dirrec[8];
	    base = nrec + 1;
	    if (nrec == lrec) {
		lindex = lword;
	    } else {
		lindex = 256;
	    }
	    i__1 = lindex;
	    for (i__ = 11; i__ <= i__1; ++i__) {
		if (dirrec[(i__2 = i__ - 1) < 256 && 0 <= i__2 ? i__2 : 
			s_rnge("dirrec", i__2, "dasacr_", (ftnlen)444)] < 0) {
		    type__ = prev[(i__2 = type__ - 1) < 3 && 0 <= i__2 ? i__2 
			    : s_rnge("prev", i__2, "dasacr_", (ftnlen)445)];
		} else {
		    type__ = next[(i__2 = type__ - 1) < 3 && 0 <= i__2 ? i__2 
			    : s_rnge("next", i__2, "dasacr_", (ftnlen)447)];
		}
		base += (i__3 = dirrec[(i__2 = i__ - 2) < 256 && 0 <= i__2 ? 
			i__2 : s_rnge("dirrec", i__2, "dasacr_", (ftnlen)450)]
			, abs(i__3));
	    }

/*           TYPE and BASE are now the data type and base record number */
/*           of the last cluster described by the current directory. */

/*           We'll now traverse the directory in reverse order, keeping */
/*           track of cluster sizes and types as we go. */

/*           POS will be the index of the descriptor of the current */
/*           cluster. */

	    pos = lindex;
	    while(pos > 9) {
		if (pos < lindex) {

/*                 We'll need to determine the type of the current */
/*                 cluster.  If the next descriptor contains a positive */
/*                 value, the data type of the cluster it refers to is */
/*                 the successor of the current type, according to our */
/*                 ordering of types. */

		    if (dirrec[(i__1 = pos) < 256 && 0 <= i__1 ? i__1 : 
			    s_rnge("dirrec", i__1, "dasacr_", (ftnlen)476)] > 
			    0) {

/*                    This assignment and the one below in the ELSE */
/*                    block are performed from the second loop iteration */
/*                    onward. NXTTYP is initialized on the first loop */
/*                    iteration. */

			type__ = prev[(i__1 = nxttyp - 1) < 3 && 0 <= i__1 ? 
				i__1 : s_rnge("prev", i__1, "dasacr_", (
				ftnlen)483)];
		    } else {
			type__ = next[(i__1 = nxttyp - 1) < 3 && 0 <= i__1 ? 
				i__1 : s_rnge("next", i__1, "dasacr_", (
				ftnlen)485)];
		    }

/*                 Update the cluster base record number. */

		    base -= (i__2 = dirrec[(i__1 = pos - 1) < 256 && 0 <= 
			    i__1 ? i__1 : s_rnge("dirrec", i__1, "dasacr_", (
			    ftnlen)491)], abs(i__2));
		}

/*              Move the current cluster. */

		i__3 = base;
		for (i__ = base + (i__2 = dirrec[(i__1 = pos - 1) < 256 && 0 
			<= i__1 ? i__1 : s_rnge("dirrec", i__1, "dasacr_", (
			ftnlen)498)], abs(i__2)) - 1; i__ >= i__3; --i__) {
		    if (type__ == 1) {
			dasioc_("READ", &unit, &i__, recc, (ftnlen)4, (ftnlen)
				1024);
			i__1 = i__ + *n;
			dasioc_("WRITE", &unit, &i__1, recc, (ftnlen)5, (
				ftnlen)1024);
		    } else if (type__ == 2) {
			dasiod_("READ", &unit, &i__, recd, (ftnlen)4);
			i__1 = i__ + *n;
			dasiod_("WRITE", &unit, &i__1, recd, (ftnlen)5);
		    } else {
			dasioi_("READ", &unit, &i__, reci, (ftnlen)4);
			i__1 = i__ + *n;
			dasioi_("WRITE", &unit, &i__1, reci, (ftnlen)5);
		    }
		}

/*              The next descriptor to look at is the preceding one in */
/*              the directory. */

		--pos;
		nxttyp = type__;
	    }

/*           Find the preceding directory record. */

	    prec = dirrec[0];

/*           Update the backward and forward pointers in the current */
/*           directory record.  However, don't modify null pointers. */

	    if (dirrec[1] > 0) {
		dirrec[1] += *n;
	    }
	    if (dirrec[0] > 0) {
		dirrec[0] += *n;
	    }

/*           Move the current directory record. */

	    i__3 = nrec + *n;
	    dasioi_("WRITE", &unit, &i__3, dirrec, (ftnlen)5);

/*           Consider the previous directory. */

	    nrec = prec;
	}
    }

/*     Update the file summary.  The number of comment records and the */
/*     number of the first free record have been incremented by N. */
/*     The numbers of the records containing the last descriptor of each */
/*     type have been incremented by N only if they were non-zero. */

/*     The call to DASUFS will update the file record as well as the */
/*     file summary. */

    ncomr += *n;
    free += *n;
    for (i__ = 1; i__ <= 3; ++i__) {
	if (lastrc[(i__3 = i__ - 1) < 3 && 0 <= i__3 ? i__3 : s_rnge("lastrc",
		 i__3, "dasacr_", (ftnlen)573)] != 0) {
	    lastrc[(i__3 = i__ - 1) < 3 && 0 <= i__3 ? i__3 : s_rnge("lastrc",
		     i__3, "dasacr_", (ftnlen)574)] = lastrc[(i__1 = i__ - 1) 
		    < 3 && 0 <= i__1 ? i__1 : s_rnge("lastrc", i__1, "dasacr_"
		    , (ftnlen)574)] + *n;
	}
    }
    dasufs_(handle, &nresvr, &nresvc, &ncomr, &ncomc, &free, lastla, lastrc, 
	    lastwd);
    chkout_("DASACR", (ftnlen)6);
    return 0;
} /* dasacr_ */

