/* zzekweed.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__0 = 0;

/* $Procedure  ZZEKWEED ( Private: EK, weed out redundant row vectors ) */
/* Subroutine */ int zzekweed_(integer *njrs, integer *bases, integer *nrows)
{
    /* System generated locals */
    integer i__1, i__2, i__3, i__4, i__5, i__6, i__7;

    /* Local variables */
    integer cand, base, ndel, ntab, pred;
    extern /* Subroutine */ int zzeksupd_(integer *, integer *, integer *), 
	    zzekvset_(integer *, integer *);
    integer i__, j;
    extern /* Subroutine */ int zzekjsqz_(integer *), chkin_(char *, ftnlen);
    integer nrloc;
    extern logical sameai_(integer *, integer *, integer *);
    integer nr, csgbas, candsv[10], psgbas, crwbas, crwvec[11], ncndrv, 
	    ncndsv, prwbas;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), setmsg_(char *, 
	    ftnlen);
    integer nsvloc, predsv[10], prwvec[11];
    extern /* Subroutine */ int errint_(char *, integer *, ftnlen), chkout_(
	    char *, ftnlen);
    integer nprdrv, nprdsv, rvsize, svsize, loc;
    logical hit;
    integer crv, csv, prv, psv;
    extern /* Subroutine */ int zzeksrd_(integer *, integer *, integer *);

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Weed out redundant, fully qualified row vectors from a join row */
/*     set union. */

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
/*     NJRS      I-O  Number of join row sets in union. */
/*     BASES     I-O  Scratch area base addresses of join row sets. */
/*     NROWS      O   Total number of row vectors in join row set union. */

/* $ Detailed_Input */

/*     NJRS           is the number of join row sets in a join row set */
/*                    union to be weeded. */

/*     BASES          is an array of base addresses, in the scratch area, */
/*                    of a collection of join row sets from which */
/*                    redundant row vectors are to be weeded out.  A row */
/*                    vector is is redundant if and only if it is */
/*                    identical to another row vector, and the qualifying */
/*                    segment vectors of the two row vectors are */
/*                    identical as well. */

/* $ Detailed_Output */

/*     NJRS           is the number of join row sets after redundant */
/*                    rows have been removed.  If any join row sets */
/*                    become empty as a result of this weeding-out, */
/*                    the count of join row sets is reduced accordingly. */

/*     BASES          is the set of bases of join rows in the join row */
/*                    set union after weeding has been completed. */
/*                    Bases of empty join row sets are compressed out; */
/*                    the valid elements of the array are the first */
/*                    NJRS elements of BASES, where NJRS has been */
/*                    updated by this routine. */

/*     NROWS          is the total number of rows in the join row set */
/*                    union after the weeding process is finished. */

/*     See $Particulars for a more detailed description of the effect of */
/*     this routine. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If JRSBAS is not the base address of a structurally valid */
/*         join row set union, the results of this routine will be */
/*         unpredictable. */

/*     2)  If NJRS is non-positive, or if NJRS exceeds the maximum */
/*         allowed number of constraint relations MAXCON, the error */
/*         SPICE(INVALIDCOUNT) will be signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine operates by side effects:  it modifies the join row */
/*     set designated by the input argument JRSBAS.  Every redundant */
/*     row vector is removed, and join row sets from which row vectors */
/*     are removed are compressed.  Empty join row sets are removed */
/*     from the union, as reflected by updates to NJRS and BASES. */

/*     The principal purpose of this routine is to support execution of */
/*     queries involving OR clauses; such queries may cause row vectors */
/*     satisfying both disjuncts to be included multiple times in the */
/*     set of matching row vectors. */

/*     The layout of a join row set in the EK scratch area is shown */
/*     in the join row set parameter include file. */

/* $ Examples */

/*     See EKSRCH. */

/* $ Restrictions */

/*     1) Loading or unloading EK files between name resolution of the */
/*        the input query and passing the query to this routine will */
/*        invalidate the checking done by this routine, and may cause */
/*        the routine to fail. */

/*     2) Assumes redundant row vectors never occur in any join row set. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */

/* $ Version */

/* -    Beta Version 1.1.1,  03-OCT-2021 (NJB) */

/*        Corrected typos in comments. */

/* -    Beta Version 1.1.0,  8-JAN-1996 (WLT) */

/*        Replaced a call to REPMI with ERRINT in the first */
/*        error check. */

/* -    Beta Version 1.0.0, 11-OCT-1995 (NJB) */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Use discovery check-in. */

    if (*njrs < 1 || *njrs > 200) {
	chkin_("ZZEKWEED", (ftnlen)8);
	setmsg_("The number of join row sets in the union is #", (ftnlen)45);
	errint_("#", njrs, (ftnlen)1);
	sigerr_("SPICE(INVALIDCOUNT)", (ftnlen)19);
	chkout_("ZZEKWEED", (ftnlen)8);
	return 0;
    }

/*     Make sure that the addressing routines are properly initialized. */

    zzekvset_(njrs, bases);

/*     Get the segment vector and row vector sizes.  The sizes that */
/*     apply to the first join row set will suffice throughout. */

    loc = bases[0] + 3;
    zzeksrd_(&loc, &loc, &ntab);
    svsize = ntab;
    rvsize = ntab + 1;

/*     Mark redundant rows vectors for deletion.  One saving grace is */
/*     that redundant rows can never occur in the same join row set, as */
/*     long as that join row set represents a set of rows satisfying */
/*     a conjunction of constraints. */

    i__1 = *njrs;
    for (cand = 2; cand <= i__1; ++cand) {

/*        We'll compare row vectors in the CAND join row set to row */
/*        vectors in the preceding join row sets.  Only row vectors */
/*        corresponding to matching segment vectors need be compared. */
/*        Therefore, we'll loop over the segment vectors in the CAND */
/*        join row set, and for each such segment vector, loop over the */
/*        segment vectors in the preceding join row sets.  If a match */
/*        occurs, we'll compare row vectors corresponding to those */
/*        segment vectors. */

/*        NCNDSV will contain the number of segment vectors in the */
/*        `candidate' join row set. */

	nsvloc = bases[cand - 1] + 4;
	zzeksrd_(&nsvloc, &nsvloc, &ncndsv);
	i__2 = ncndsv;
	for (csv = 1; csv <= i__2; ++csv) {

/*           Look up the candidate segment vector. */

	    csgbas = bases[cand - 1] + 4 + (csv - 1) * svsize;
	    i__3 = csgbas + 1;
	    i__4 = csgbas + svsize;
	    zzeksrd_(&i__3, &i__4, candsv);

/*           Get the row vector count and base address of the set of */
/*           row vectors for the candidate segment vector, in case */
/*           we need them.  (Referring to the diagram of the join */
/*           row set structure in the join row set parameter include */
/*           file may be helpful here.) */

	    base = bases[cand - 1] + 4 + ncndsv * svsize + (csv - 1 << 1);
	    i__3 = base + 1;
	    i__4 = base + 1;
	    zzeksrd_(&i__3, &i__4, &crwbas);
	    crwbas += bases[cand - 1];
	    i__3 = base + 2;
	    i__4 = base + 2;
	    zzeksrd_(&i__3, &i__4, &ncndrv);

/*           For the current predecessor join row set, look up the */
/*           segment vectors in that join row set and compare them to the */
/*           candidate. */

	    i__3 = cand - 1;
	    for (pred = 1; pred <= i__3; ++pred) {

/*              Get the count of segment vectors in the current */
/*              predecessor join row set. */

		nsvloc = bases[pred - 1] + 4;
		zzeksrd_(&nsvloc, &nsvloc, &nprdsv);
		i__4 = nprdsv;
		for (psv = 1; psv <= i__4; ++psv) {

/*                 Look up the predecessor segment vector. */

		    psgbas = bases[pred - 1] + 4 + (psv - 1) * svsize;
		    i__5 = csgbas + 1;
		    i__6 = csgbas + svsize;
		    zzeksrd_(&i__5, &i__6, predsv);

/*                 Compare the segment vectors and hope for the best. */

		    if (sameai_(candsv, predsv, &svsize)) {

/*                    Unfortunately, the two segment vectors match, so */
/*                    there's something to do.  We'll have to compare */
/*                    every row vector corresponding to the candidate */
/*                    segment vector with every row vector corresponding */
/*                    to the predecessor. */

/*                    Get the row vector count and base address of the */
/*                    set of row vectors for the current predecessor */
/*                    segment vector.  We already have on hand the */
/*                    corresponding quantities for the candidate */
/*                    segment vector. */

			base = bases[pred - 1] + 4 + nprdsv * svsize + (psv - 
				1 << 1);
			i__5 = base + 1;
			i__6 = base + 1;
			zzeksrd_(&i__5, &i__6, &prwbas);
			prwbas += bases[pred - 1];
			i__5 = base + 2;
			i__6 = base + 2;
			zzeksrd_(&i__5, &i__6, &nprdrv);

/*                    Compare all row vectors. */

			i__5 = ncndrv;
			for (crv = 1; crv <= i__5; ++crv) {
			    base = crwbas + (crv - 1) * rvsize;
			    i__6 = base + 1;
			    i__7 = base + rvsize;
			    zzeksrd_(&i__6, &i__7, crwvec);
			    prv = 1;
			    hit = FALSE_;
			    while(prv <= nprdrv && ! hit) {
				base = prwbas + (prv - 1) * rvsize;
				i__6 = base + 1;
				i__7 = base + rvsize;
				zzeksrd_(&i__6, &i__7, prwvec);
				if (sameai_(crwvec, prwvec, &rvsize)) {

/*                             The row vectors, together with their */
/*                             qualifying segment vectors, match.  The */
/*                             higher-indexed vector is considered */
/*                             redundant.  To mark this vector for */
/*                             deletion, we simply zero out the first */
/*                             element of the row vector.  This makes the */
/*                             row vector invalid, so it will not match */
/*                             any valid row vector we see later. */

				    base = crwbas + (crv - 1) * rvsize;
				    i__6 = base + 1;
				    i__7 = base + 1;
				    zzeksupd_(&i__6, &i__7, &c__0);
				    hit = TRUE_;
				} else {
				    ++prv;
				}
			    }
			}
		    }

/*                 We've finished comparing row vectors for a pair of */
/*                 segment vectors, if it was necessary to do so. */

		}

/*              We've compared all segment vectors in the current */
/*              predecessor join row set with the candidate segment */
/*              vector. */

	    }

/*           We've compared all segment vectors in all predecessor join */
/*           row sets to the current segment vector. */

	}

/*        We've compared the candidate join row set to its predecessors. */

    }

/*     We've compared all of the join row sets. */


/*     Now, clean up the join row set union by compressing out deleted */
/*     rows, segment vectors, and join row sets. */

    j = 1;
    ndel = 0;
    i__1 = *njrs;
    for (i__ = 1; i__ <= i__1; ++i__) {

/*        Compress the current join row set.  If it ends up empty, */
/*        expel it from the union. */

	zzekjsqz_(&bases[i__ - 1]);
	nrloc = bases[i__ - 1] + 2;
	zzeksrd_(&nrloc, &nrloc, &nr);
	if (nr == 0) {

/*           This entire join row set can be deleted from the union. */
/*           Consider the next join row set. */

	    ++ndel;
	} else {
	    bases[j - 1] = bases[i__ - 1];
	    ++j;
	}
    }
    *njrs -= ndel;

/*     Count the rows remaining after our clean-up operation. */

    *nrows = 0;
    i__1 = *njrs;
    for (i__ = 1; i__ <= i__1; ++i__) {
	nrloc = bases[i__ - 1] + 2;
	zzeksrd_(&nrloc, &nrloc, &nr);
	*nrows += nr;
    }
    return 0;
} /* zzekweed_ */

