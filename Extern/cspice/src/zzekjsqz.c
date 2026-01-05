/* zzekjsqz.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure  ZZEKJSQZ ( Private: EK, join row set squeeze ) */
/* Subroutine */ int zzekjsqz_(integer *jrsbas)
{
    /* System generated locals */
    integer i__1, i__2, i__3, i__4;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer ntab, size;
    extern /* Subroutine */ int zzeksupd_(integer *, integer *, integer *);
    integer i__, j, delta, rbase, nrloc, ptarg, ntloc, rtarg, vtarg;
    extern logical failed_(void);
    integer rc, nr, segvec[10], pcpair[2], ptbase, setbas, cntloc, nsvdel, 
	    nrvdel, svbase, nsvloc, ptrloc, rowvec[11], sizloc, newnsv, 
	    rvsize, svsize, nsv;
    extern /* Subroutine */ int zzeksrd_(integer *, integer *, integer *);

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Compress a join row set by eliminating segment vectors for */
/*     which there are no corresponding row vectors. */

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

/*     EK */

/* $ Keywords */

/*     EK */
/*     PRIVATE */

/* $ Declarations */
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


/*     Include Section:  EK Join Row Set Parameters */

/*        ekjrs.inc  Version 1    07-FEB-1995 (NJB) */


/*     Maximum number of join row sets in a join row set union: */


/*     The layout of a join row set in the EK scratch area is shown */
/*     below: */

/*        +--------------------------------------------+ */
/*        |              join row set size             |  1 element */
/*        +--------------------------------------------+ */
/*        |    number of row vectors in join row set   |  1 element */
/*        +--------------------------------------------+ */
/*        |               table count (TC)             |  1 element */
/*        +--------------------------------------------+ */
/*        |          segment vector count (SVC)        |  1 element */
/*        +--------------------------------------------+ */
/*        |               segment vector 1             |  TC elements */
/*        +--------------------------------------------+ */
/*                              . */
/*                              . */
/*                              . */
/*        +--------------------------------------------+ */
/*        |               segment vector SVC           |  TC elements */
/*        +--------------------------------------------+ */
/*        |   segment vector 1 row set base address    |  1 element */
/*        +--------------------------------------------+ */
/*        |      segment vector 1 row count (RC_1)     |  1 element */
/*        +--------------------------------------------+ */
/*                              . */
/*                              . */
/*                              . */
/*        +--------------------------------------------+ */
/*        |  segment vector SVC row set base address   |  1 element */
/*        +--------------------------------------------+ */
/*        |   segment vector SVC row count (RC_SVC)    |  1 element */
/*        +--------------------------------------------+ */
/*        | Augmented row vectors for segment vector 1 |  (TC+1)*RC_1 */
/*        +--------------------------------------------+  elements */
/*                              . */
/*                              . */
/*                              . */
/*        +--------------------------------------------+ */
/*        |Augmented row vectors for segment vector SVC|  (TC+1)*RC_SVC1 */
/*        +--------------------------------------------+  elements */


/*     The following parameters indicate positions of elements in the */
/*     join row set structure: */


/*     Base-relative index of join row set size */


/*     Index of row vector count */


/*     Index of table count */


/*     Index of segment vector count */


/*     Base address of first segment vector */



/*     End Include Section:  EK Join Row Set Parameters */

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


/*     Include Section:  EK Query Limit Parameters */

/*        ekqlimit.inc  Version 3    16-NOV-1995 (NJB) */

/*           Parameter MAXCON increased to 1000. */

/*        ekqlimit.inc  Version 2    01-AUG-1995 (NJB) */

/*           Updated to support SELECT clause. */


/*        ekqlimit.inc  Version 1    07-FEB-1995 (NJB) */


/*     These limits apply to character string queries input to the */
/*     EK scanner.  This limits are part of the EK system's user */
/*     interface:  the values should be advertised in the EK required */
/*     reading document. */


/*     Maximum length of an input query:  MAXQRY.  This value is */
/*     currently set to twenty-five 80-character lines. */


/*     Maximum number of columns that may be listed in the */
/*     `order-by clause' of a query:  MAXSEL.  MAXSEL = 50. */


/*     Maximum number of tables that may be listed in the `FROM */
/*     clause' of a query: MAXTAB. */


/*     Maximum number of relational expressions that may be listed */
/*     in the `constraint clause' of a query: MAXCON. */

/*     This limit applies to a query when it is represented in */
/*     `normalized form': that is, the constraints have been */
/*     expressed as a disjunction of conjunctions of relational */
/*     expressions. The number of relational expressions in a query */
/*     that has been expanded in this fashion may be greater than */
/*     the number of relations in the query as orginally written. */
/*     For example, the expression */

/*             ( ( A LT 1 ) OR ( B GT 2 ) ) */
/*        AND */
/*             ( ( C NE 3 ) OR ( D EQ 4 ) ) */

/*     which contains 4 relational expressions, expands to the */
/*     equivalent normalized constraint */

/*             (  ( A LT 1 ) AND ( C NE 3 )  ) */
/*        OR */
/*             (  ( A LT 1 ) AND ( D EQ 4 )  ) */
/*        OR */
/*             (  ( B GT 2 ) AND ( C NE 3 )  ) */
/*        OR */
/*             (  ( B GT 2 ) AND ( D EQ 4 )  ) */

/*     which contains eight relational expressions. */



/*     MXJOIN is the maximum number of tables that can be joined. */


/*     MXJCON is the maximum number of join constraints allowed. */


/*     Maximum number of order-by columns that may be used in the */
/*     `order-by clause' of a query: MAXORD. MAXORD = 10. */


/*     Maximum number of tokens in a query: 500. Tokens are reserved */
/*     words, column names, parentheses, and values. Literal strings */
/*     and time values count as single tokens. */


/*     Maximum number of numeric tokens in a query: */


/*     Maximum total length of character tokens in a query: */


/*     Maximum length of literal string values allowed in queries: */
/*     MAXSTR. */


/*     End Include Section:  EK Query Limit Parameters */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     JRSBAS     I   Scratch area base address of join row set. */

/* $ Detailed_Input */

/*     JRSBAS         is the base address, in the scratch area, of a */
/*                    join row set to be compressed. */

/* $ Detailed_Output */

/*     None.  See $Particulars for a description of the effect of this */
/*     routine. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If JRSBAS is not the base address of a structurally valid */
/*         join row set, the results of this routine will be */
/*         unpredictable. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine operates by side effects:  it modifies the join row */
/*     set designated by the input argument JRSBAS.  Every row vector */
/*     marked for deletion is removed.  Every empty segment vector is */
/*     removed, along with the row count and row vector base for that */
/*     segment vector.  The join row set is compressed to remove all */
/*     gaps.   All counts are updated to reflect the updated join row */
/*     set. */

/*     The purpose of the compression performed by this routine is to */
/*     save work during joins by reducing the size of the cartesian */
/*     products of sets of segment vectors.  Also, special cases */
/*     involving null segment vectors can be avoided by this clean-up */
/*     mechanism.  Finally, it may be possible to save space in the EK */
/*     scratch area freed by the compression. */

/* $ Examples */

/*     See EKSRCH. */

/* $ Restrictions */

/*     1) Relies on the EK scratch area. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.1, 02-OCT-2021 (NJB) */

/*        Corrected typo in comments. */

/* -    SPICELIB Version 1.1.0, 07-AUG-2006 (NJB) */

/*        Bug fix:  added initialization of variable NRVDEL to support */
/*                  operation under the Macintosh Intel Fortran */
/*                  compiler. Note that this bug did not affect */
/*                  operation of this routine on other platforms. */

/* -    SPICELIB Version 1.0.0, 10-OCT-1995 (NJB) */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.1.0, 07-AUG-2006 (NJB) */

/*        Bug fix:  added initialization of variable NRVDEL to support */
/*                  operation under the Macintosh Intel Fortran */
/*                  compiler. Note that this bug did not affect */
/*                  operation of this routine on other platforms. The */
/*                  statement referencing the uninitialized variable */
/*                  was: */

/*           IF (  ( RC .EQ. 0 ) .OR. ( NRVDEL .EQ. RC )  ) THEN */

/*        In the previous version of the code, NRVDEL is uninitialized */
/*        when NRVDEL is 0.  NRVDEL *is* initialized when RC is */
/*        non-zero, so the logical value of the IF expression is not */
/*        affected by the lack of proper initialization. */

/*        However, the Intel Fortran compiler for the Mac flags a runtime */
/*        error when the above code is exercised.  So NRVDEL is now */
/*        initialized prior to the above IF statement. */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Use discovery check-in. */


/*     Look up the counts that are of interest: */

/*       -- The table count */
/*       -- The segment vector count */
/*       -- The join row set size */

/*     Save the address of each count. */

    sizloc = *jrsbas + 1;
    nsvloc = *jrsbas + 4;
    ntloc = *jrsbas + 3;
    zzeksrd_(&sizloc, &sizloc, &size);
    zzeksrd_(&ntloc, &ntloc, &ntab);
    zzeksrd_(&nsvloc, &nsvloc, &nsv);
    if (failed_()) {
	return 0;
    }

/*     Set the sizes of segment and row vectors. */

    svsize = ntab;
    rvsize = ntab + 1;

/*     For each segment vector, obtain the row count.  Clean up after */
/*     null segment vectors:  compress out the space allocated for their */
/*     row vector pointers.  Keep track of the number of deletions. */

    nsvdel = 0;
    nrvdel = 0;
    vtarg = *jrsbas + 4;
    i__1 = nsv;
    for (i__ = 1; i__ <= i__1; ++i__) {

/*        The location of the row count is CNTLOC.  The row vector base */
/*        pointer precedes the row count. */

	cntloc = *jrsbas + 4 + nsv * svsize + (i__ - 1 << 1) + 2;
	ptrloc = cntloc - 1;
	zzeksrd_(&cntloc, &cntloc, &rc);
	if (rc > 0) {

/*           The row vector set for this segment vector is non-empty. */
/*           scan the rows, looking for those marked for deletion, and */
/*           update the row count to reflect the number of rows that */
/*           we're going to keep. */

	    zzeksrd_(&ptrloc, &ptrloc, &setbas);
	    nrvdel = 0;
	    i__2 = rc;
	    for (j = 1; j <= i__2; ++j) {
		rbase = *jrsbas + setbas + (j - 1) * rvsize;
		i__3 = rbase + 1;
		i__4 = rbase + 1;
		zzeksrd_(&i__3, &i__4, rowvec);
		if (rowvec[0] == 0) {
		    ++nrvdel;
		}
	    }
	}

/*        Compute the base address of the current segment vector. */

	svbase = *jrsbas + 4 + (i__ - 1) * svsize;
	if (rc == 0 || nrvdel == rc) {

/*           We're going to delete the current segment vector.  We'll */
/*           just skip over it without advancing our target pointers. */

	    ++nsvdel;
	} else if (nsvdel > 0) {

/*           We need to shift the current segment vector to its */
/*           destination. */

	    i__2 = svbase + 1;
	    i__3 = svbase + svsize;
	    zzeksrd_(&i__2, &i__3, segvec);
	    i__2 = vtarg + 1;
	    i__3 = vtarg + svsize;
	    zzeksupd_(&i__2, &i__3, segvec);
	    vtarg += svsize;
	} else {

/*           No segment vectors have been deleted yet.  We still must */
/*           update the target in case we shift vectors later on in this */
/*           loop. */

	    vtarg += svsize;
	}
    }

/*     At this point, we've compressed out the null segment vectors. */
/*     The next step is to compress out the row vector counts and row */
/*     vector pointers that corresponded to those segment vectors.  We */
/*     also want to remove the gap between the segment vectors and the */
/*     row vector pointer/count pairs. */

/*     We need to do this only if we deleted some segment vectors. */

    if (nsvdel > 0) {
	newnsv = nsv - nsvdel;
	ptarg = *jrsbas + 4 + newnsv * svsize;
	i__1 = nsv;
	for (i__ = 1; i__ <= i__1; ++i__) {

/*           The row count is RC. */

	    svsize = ntab;
	    cntloc = *jrsbas + 4 + nsv * svsize + (i__ - 1 << 1) + 2;
	    zzeksrd_(&cntloc, &cntloc, &rc);
	    ptbase = cntloc - 2;
	    if (rc > 0) {

/*              Shift the current row vector pointer and row vector */
/*              count. */

		i__2 = ptbase + 1;
		i__3 = ptbase + 2;
		zzeksrd_(&i__2, &i__3, pcpair);
		i__2 = ptarg + 1;
		i__3 = ptarg + 2;
		zzeksupd_(&i__2, &i__3, pcpair);
		ptarg += 2;
	    }
	}
    } else {
	newnsv = nsv;
    }

/*     Update the segment vector count. */

    zzeksupd_(&nsvloc, &nsvloc, &newnsv);

/*     Remove any gaps that may exist between any of the row vectors, */
/*     or between the end of the segment vector's row vector counts */
/*     and base addresses and the first row vector. */

/*     The initial target location is the first element following the */
/*     last segment vector's row vector count.  RTARG is used as a base */
/*     address; it precedes this location by 1. */

/*     If we deleted any segment vectors, the segment vector pointers */
/*     embedded in the row vectors must change.  Make these updates */
/*     if necessary. */


    rtarg = *jrsbas + 4 + newnsv * (svsize + 2);
    i__1 = newnsv;
    for (i__ = 1; i__ <= i__1; ++i__) {

/*        Find the row count and row pointer for the current segment */
/*        vector. */

	cntloc = *jrsbas + 4 + newnsv * svsize + (i__ - 1 << 1) + 2;
	zzeksrd_(&cntloc, &cntloc, &rc);
	ptrloc = cntloc - 1;

/*        Get the row vector set base pointer.  After capturing the */
/*        current value, we'll update this pointer to account for */
/*        the shifting of row vectors. */

	zzeksrd_(&ptrloc, &ptrloc, &setbas);
	rbase = *jrsbas + setbas;
	delta = rtarg - rbase;
	i__2 = setbas + delta;
	zzeksupd_(&ptrloc, &ptrloc, &i__2);

/*        Shift the row vectors for the current segment vector, */
/*        leaving behind the row vectors marked for deletion. */

	nrvdel = 0;
	i__2 = rc;
	for (j = 1; j <= i__2; ++j) {
	    i__3 = rbase + 1;
	    i__4 = rbase + rvsize;
	    zzeksrd_(&i__3, &i__4, rowvec);
	    if (rowvec[0] == 0) {

/*              This row vector is to be deleted; don't copy it. */

		rbase += rvsize;
		++nrvdel;
	    } else {

/*              The segment vector pointer is base-relative. */

		rowvec[(i__3 = rvsize - 1) < 11 && 0 <= i__3 ? i__3 : s_rnge(
			"rowvec", i__3, "zzekjsqz_", (ftnlen)419)] = (i__ - 1)
			 * svsize + 4;
		i__3 = rtarg + 1;
		i__4 = rtarg + rvsize;
		zzeksupd_(&i__3, &i__4, rowvec);
		rbase += rvsize;
		rtarg += rvsize;
	    }
	}

/*        Update the row count for the current segment vector, if */
/*        necessary.  Note that no segment vector will become empty */
/*        as a result of the row vector deletions we've done; we */
/*        already eliminated any segment vectors for which that */
/*        could happen, before we entered this loop. */

	if (nrvdel > 0) {
	    i__2 = rc - nrvdel;
	    zzeksupd_(&cntloc, &cntloc, &i__2);
	}
    }

/*     Update the total row count and size of the join row set. */

    nr = 0;
    i__1 = newnsv;
    for (i__ = 1; i__ <= i__1; ++i__) {
	cntloc = *jrsbas + 4 + newnsv * svsize + (i__ - 1 << 1) + 2;
	zzeksrd_(&cntloc, &cntloc, &rc);
	nr += rc;
    }
    nrloc = *jrsbas + 2;
    size = newnsv * (svsize + 2) + 4 + nr * rvsize;
    zzeksupd_(&nrloc, &nrloc, &nr);
    zzeksupd_(&sizloc, &sizloc, &size);
    return 0;
} /* zzekjsqz_ */

