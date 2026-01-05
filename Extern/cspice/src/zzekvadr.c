/* zzekvadr.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__200 = 200;
static integer c__10 = 10;

/* $Procedure  ZZEKVADR  ( Compute row vector address ) */
/* Subroutine */ int zzekvadr_0_(int n__, integer *njrs, integer *bases, 
	integer *rwvidx, integer *rwvbas, integer *sgvbas)
{
    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    static integer rbas[200];
    extern /* Subroutine */ int zzekstop_(integer *);
    static integer i__, j;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    static integer ntabs, svbas[200];
    extern /* Subroutine */ int cleari_(integer *, integer *);
    static integer begidx[200], reloff, addrss;
    extern /* Subroutine */ int sigerr_(char *, ftnlen);
    extern integer lstlei_(integer *, integer *, integer *);
    extern /* Subroutine */ int chkout_(char *, ftnlen), setmsg_(char *, 
	    ftnlen);
    static integer jrsidx;
    extern /* Subroutine */ int errint_(char *, integer *, ftnlen);
    extern logical return_(void);
    static integer maxrwv, svnjrs, top, nsv;
    extern /* Subroutine */ int zzeksrd_(integer *, integer *, integer *);

/* $ Abstract */

/*     Compute the EK scratch area base address of the row vector having */
/*     a specified index, given a union of EK join row sets and a row */
/*     vector index. Also return the base address of the row */
/*     vector's parent segment vector. */

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
/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Entry points */
/*     --------  ---  -------------------------------------------------- */
/*     NJRS       I   ZZEKVSET */
/*     BASES      I   ZZEKVSET */
/*     RWVIDX     I   ZZEKVACL */
/*     RWVBAS     O   ZZEKVACL */
/*     SGVBAS     O   ZZEKVACL */
/*     MXJOIN     P   Maximum number of tables that can be joined. */
/*     MXJRS      P   Maximum number of join row sets allowed in union. */

/* $ Detailed_Input */

/*     See the entry points for a discussion of their arguments. */

/* $ Detailed_Output */

/*     See the entry points for a discussion of their arguments. */

/* $ Parameters */

/*     MXJOIN         is the maximum number of tables that can be joined. */

/*     MXJRS          is the maximum number of join row sets allowed in */
/*                    in the input union identified by BASES and NJRS. */

/* $ Exceptions */

/*     1)  This is an umbrella routine which contains declarations */
/*         for its entry points.  This routine should never be called */
/*         directly.  If it is, the error SPICE(BOGUSENTRY) will be */
/*         signaled. */

/*     See the entry points for discussions of the exceptions specific */
/*     to those entry points. */

/* $ Files */

/*     1)  This routine uses the EK scratch area, which employs a scratch */
/*         DAS file. */

/* $ Particulars */

/*     In the course of query resolution, the EK system builds a set of */
/*     data structures called `join row sets' that represent the rows */
/*     that satisfy the query constraints.  These rows belong to a table */
/*     formed by taking the Cartesian product of the tables in the FROM */
/*     clause of the query.  One join row set is formed for each */
/*     conjunction of join constraints; the total number of join row sets */
/*     is equal to the number of conjunctions of join constraints in */
/*     the query.  Join row sets are described below. */

/*     This group of routines allows the EK system to view the rows */
/*     matching a query as a sequence of vectors, where each vector is an */
/*     n-tuple of row numbers designating rows in segments of the */
/*     Cartesian product of tables specified in the input query.  These */
/*     vectors are called `row vectors'.  Each row vector also points to */
/*     a vector of segments that contain the rows represented by the row */
/*     vector. */

/*     These routines centralize the calculations needed to locate the */
/*     nth row vector. */

/*     Each join row set consists of: */

/*         - a base address in the scratch area */
/*         - a table count */
/*         - a segment vector count */
/*         - a set of segment vectors */
/*         - a set of segment vector row vector base addresses */
/*           (these are relative to the base of the join row set) */
/*         - a set of segment vector row vector counts */
/*         - a set of row vectors, augmented by offsets of their */
/*           parent segment vectors (these offsets are at the */
/*           end of each row vector) */

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
/*        | Augmented row vectors for segment vector 1 |  TC*(RC_1 + 1 ) */
/*        +--------------------------------------------+  elements */
/*                              . */
/*                              . */
/*                              . */
/*        +--------------------------------------------+ */
/*        |Augmented row vectors for segment vector SVC|  TC*(RC_SVC + 1) */
/*        +--------------------------------------------+  elements */


/* $ Examples */

/*     1)  For a given join row set union, initialize the addressing */
/*         routines, then look up row vectors. */


/*            C */
/*            C     Tell the addressing routines where the join row set */
/*            C     union is.  NJRS is the number of join row sets in */
/*            C     the union, BASES is an array of EK scratch area base */
/*            C     addresses of each join row set.  A base address is */
/*            C     the predecessor of the first address actually */
/*            C     occupied by a join row set. */
/*            C */
/*                  CALL ZZEKVSET ( NJRS, BASES ) */

/*            C */
/*            C     Find the base address of the each row vector, as well */
/*            C     as the base address of the corresponding segment */
/*            C     vector. */
/*            C */
/*                  DO I = 1, NJRS */

/*                     CALL EKVCAL ( I, RWVBAS, SGVBAS ) */

/*                     [Do something with the row vector....] */

/*                  END DO */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.2, 01-OCT-2021 (NJB) */

/*        Updated header Abstract section. Corrected typos in comments. */

/* -    SPICELIB Version 1.0.1, 06-SEP-2006 (NJB) */

/*        Filled in Particulars section of header in entry point */
/*        ZZEKVCAL.  Changed previous version line's product from "Beta" */
/*        to "SPICELIB" both here and in ZZEKVCAL. */

/* -    SPICELIB Version 1.0.0, 28-SEP-1994 (NJB) (WLT) */

/* -& */
/* $ Index_Entries */

/*     EK row vector address calculation */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Include Section:  EK Join Row Set Parameters */

/*        JRS$INC Version 1    17-SEP-1994 (NJB) */

/*     Base-relative index of join row set size */


/*     Index of row vector count */


/*     Index of table count */


/*     Index of segment vector count */


/*     Base address of first segment vector */



/*     End Include Section:  EK Join Row Set Parameters */


/*     Local variables */


/*     Saved variables */


/*     Standard SPICE error handling. */

    /* Parameter adjustments */
    if (bases) {
	}

    /* Function Body */
    switch(n__) {
	case 1: goto L_zzekvset;
	case 2: goto L_zzekvcal;
	}

    if (return_()) {
	return 0;
    } else {
	chkin_("ZZEKVADR", (ftnlen)8);
    }

/*     Never come here. */

    sigerr_("SPICE(BOGUSENTRY)", (ftnlen)17);
    chkout_("ZZEKVADR", (ftnlen)8);
    return 0;
/* $Procedure  ZZEKVSET  ( Row vector address calculation set-up ) */

L_zzekvset:
/* $ Abstract */

/*     Given a union of EK join row sets, prepare EKVCAL to */
/*     compute addresses of row vectors in that union. */

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
/*     UTILITY */

/* $ Declarations */

/*     INTEGER               NJRS */
/*     INTEGER               BASES  ( * ) */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     NJRS       I   Number of join row sets in union. */
/*     BASES      I   EK scratch area base addresses of join row sets. */

/* $ Detailed_Input */

/*     NJRS           is the number of join row sets in a join row set */
/*                    for which address calculations will be performed. */

/*     BASES          is an array of base addresses of the join row sets */
/*                    comprising the union.  These addresses are the */
/*                    predecessors of the addresses actually occupied by */
/*                    the join row sets.  There are NJRS base addresses */
/*                    in the array.  The order in which addresses are */
/*                    listed in BASES determines the order of the union */
/*                    of the row vectors:  the first row vector in the */
/*                    join row set whose base address is BASES(1) has */
/*                    index 1, and so on.  The last row vector in the */
/*                    join row set whose base address is BASES(NJRS) has */
/*                    the highest index of any row vector in the union. */

/* $ Detailed_Output */

/*     None.  See $Particulars for a discussion of the effect of this */
/*     routine. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the join row set count is less than 1 or greater than */
/*         MXJRS, the error SPICE(INVALIDCOUNT) is signaled. */

/*     2)  If any base address is less than zero or greater than TOP, */
/*         the EK scratch area stack top, the error */
/*         SPICE(BADADDRESS) is signaled. */

/*     3)  If the table count for any join row set is less than 1 or */
/*         greater than MXJOIN, the error SPICE(INVALIDCOUNT) is */
/*         signaled. */

/*     4)  If the table count for any join row set unequal to the count */
/*         for the first join row set, the error SPICE(INVALIDCOUNT) is */
/*         signaled. */

/*     5)  If any join row set has a row vector count that is less than */
/*         zero or greater than TOP, the EK scratch area stack top, the */
/*         error SPICE(BADADDRESS) is signaled. */

/*     6)  If any join row set has a segment vector count that is less */
/*         than zero or greater than TOP, the EK scratch area stack top, */
/*         the error SPICE(BADADDRESS) is signaled. */

/* $ Files */

/*     1)  This routine uses the EK scratch area, which employs a scratch */
/*         DAS file. */

/* $ Particulars */

/*     This routine speeds up EK row vectors address calculations by */
/*     centralizing the activities that need be performed only once */
/*     for a series of address  calculations for a given join row set */
/*     union. */

/* $ Examples */

/*     See the $Examples section of ZZEKVADR. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    Beta Version 1.0.0, 28-SEP-1994 (NJB) (WLT) */

/* -& */
/* $ Index_Entries */

/*     EK row vector address calculation */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("ZZEKVSET", (ftnlen)8);
    }

/*     Validate join row set count. */

    if (*njrs < 1 || *njrs > 200) {
	setmsg_("Number of join row sets was #; valid range is 1:#", (ftnlen)
		49);
	errint_("#", njrs, (ftnlen)1);
	errint_("#", &c__200, (ftnlen)1);
	sigerr_("SPICE(INVALIDCOUNT)", (ftnlen)19);
	chkout_("ZZEKVSET", (ftnlen)8);
	return 0;
    }

/*     Validate the join row set bases. */

    zzekstop_(&top);
    i__1 = *njrs;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (bases[i__ - 1] < 0 || bases[i__ - 1] > top) {
	    setmsg_("Base address # was #; valid range is 1:#", (ftnlen)40);
	    errint_("#", &i__, (ftnlen)1);
	    errint_("#", &bases[i__ - 1], (ftnlen)1);
	    errint_("#", &top, (ftnlen)1);
	    sigerr_("SPICE(BADADDRESS)", (ftnlen)17);
	    chkout_("ZZEKVSET", (ftnlen)8);
	    return 0;
	}
	svbas[(i__2 = i__ - 1) < 200 && 0 <= i__2 ? i__2 : s_rnge("svbas", 
		i__2, "zzekvadr_", (ftnlen)530)] = bases[i__ - 1];
    }

/*     Validate and save the table count.  It's an error for this */
/*     count not to be identical for all of the join row sets in the */
/*     union. */

    addrss = bases[0] + 3;
    zzeksrd_(&addrss, &addrss, &ntabs);
    if (ntabs < 1 || ntabs > 10) {
	setmsg_("Table count for first join row set was #; valid range is 1:#"
		, (ftnlen)60);
	errint_("#", &ntabs, (ftnlen)1);
	errint_("#", &c__10, (ftnlen)1);
	sigerr_("SPICE(INVALIDCOUNT)", (ftnlen)19);
	chkout_("ZZEKVSET", (ftnlen)8);
	return 0;
    }
    i__1 = *njrs;
    for (i__ = 2; i__ <= i__1; ++i__) {
	addrss = bases[i__ - 1] + 3;
	zzeksrd_(&addrss, &addrss, &j);
	if (j != ntabs) {
	    setmsg_("Join row set # contains # tables; first join row set co"
		    "ntains # tables.  These counts are supposed to match.", (
		    ftnlen)108);
	    errint_("#", &i__, (ftnlen)1);
	    errint_("#", &j, (ftnlen)1);
	    errint_("#", &ntabs, (ftnlen)1);
	    sigerr_("SPICE(INVALIDCOUNT)", (ftnlen)19);
	    chkout_("ZZEKVSET", (ftnlen)8);
	    return 0;
	}
    }

/*     Validate the row vector counts for each join row set. */
/*     These counts must be in range.  Save the start indices of */
/*     the row vectors in each join row set. */

    cleari_(&c__200, begidx);
    begidx[0] = 1;
    i__1 = *njrs;
    for (i__ = 1; i__ <= i__1; ++i__) {
	addrss = bases[i__ - 1] + 2;
	zzeksrd_(&addrss, &addrss, &j);
	if (j < 0 || j > top) {
	    setmsg_("Join row set # has row count #; valid range is 0:#", (
		    ftnlen)50);
	    errint_("#", &i__, (ftnlen)1);
	    errint_("#", &j, (ftnlen)1);
	    errint_("#", &top, (ftnlen)1);
	    sigerr_("SPICE(INVALIDCOUNT)", (ftnlen)19);
	    chkout_("ZZEKVSET", (ftnlen)8);
	    return 0;
	}
	if (i__ < *njrs) {
	    begidx[(i__2 = i__) < 200 && 0 <= i__2 ? i__2 : s_rnge("begidx", 
		    i__2, "zzekvadr_", (ftnlen)602)] = begidx[(i__3 = i__ - 1)
		     < 200 && 0 <= i__3 ? i__3 : s_rnge("begidx", i__3, "zze"
		    "kvadr_", (ftnlen)602)] + j;
	}
    }

/*     Retain the index of the last row vector. */

    maxrwv = begidx[(i__1 = *njrs - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge(
	    "begidx", i__1, "zzekvadr_", (ftnlen)612)] + j;

/*     Save the base addresses of the row vectors in each join row set. */
/*     Validate the segment vector counts while we're at it. */

    i__1 = *njrs;
    for (i__ = 1; i__ <= i__1; ++i__) {
	addrss = bases[i__ - 1] + 4;
	zzeksrd_(&addrss, &addrss, &nsv);
	if (nsv < 0) {
	    setmsg_("Join row set # has segment vector count #; count must b"
		    "e non-negative.", (ftnlen)70);
	    errint_("#", &i__, (ftnlen)1);
	    errint_("#", &nsv, (ftnlen)1);
	    errint_("#", &top, (ftnlen)1);
	    sigerr_("SPICE(INVALIDCOUNT)", (ftnlen)19);
	    chkout_("ZZEKVSET", (ftnlen)8);
	    return 0;
	}
	rbas[(i__2 = i__ - 1) < 200 && 0 <= i__2 ? i__2 : s_rnge("rbas", i__2,
		 "zzekvadr_", (ftnlen)637)] = addrss + nsv * (ntabs + 2);
    }

/*     Retain the count of join row sets in the union. */

    svnjrs = *njrs;
    chkout_("ZZEKVSET", (ftnlen)8);
    return 0;
/* $Procedure  ZZEKVCAL  ( Row vector address calculation  ) */

L_zzekvcal:
/* $ Abstract */

/*     Find the EK scratch area base address of a row vector and the */
/*     corresponding segment vector, where the row vector has a */
/*     specified index within a union of join row sets. */

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
/*     UTILITY */

/* $ Declarations */

/*     INTEGER               RWVIDX */
/*     INTEGER               RWVBAS */
/*     INTEGER               SGVBAS */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     RWVIDX     I   Index of row vector. */
/*     RWVBAS     O   EK scratch area base address of row vector. */
/*     SGVBAS     O   Base address of parent segment vector. */

/* $ Detailed_Input */

/*     RWVIDX         is the index of a row vector in a join row set */
/*                    union.  The union is presumed to have been */
/*                    specified by a call to ZZEKVSET. */

/* $ Detailed_Output */

/*     RWVBAS         is the EK scratch area base address of the row */
/*                    vector specified by RWVIDX.  This address is */
/*                    the predecessor of the first address occupied by */
/*                    the row vector.  The row vector occupies NTAB */
/*                    consecutive addresses, where NTAB is the common */
/*                    table count for all join row sets in the union */
/*                    containing the specified row vector. */

/*     SGVBAS         is the EK scratch area base address of the segment */
/*                    vector corresponding to the specified row vector. */
/*                    The segment vector also occupies NTAB consecutive */
/*                    addresses. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the input index is less than 1 or greater than */
/*         the highest index in the join row set union being addressed, */
/*         the error SPICE(INVALIDINDEX) is signaled. */

/* $ Files */

/*     1)  This routine uses the EK scratch area, which employs a scratch */
/*         DAS file. */

/* $ Particulars */

/*     See header of umbrella routine ZZEKVADR. */

/* $ Examples */

/*     See the $Examples section of ZZEKVADR. */

/* $ Restrictions */

/*     1)  ZZEKVSET must be called before this routine is called for the */
/*         first time. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 06-SEP-2006 (NJB) */

/*        Filled in Particulars section of header.  Changed */
/*        previous version line's product from "Beta" to "SPICELIB." */

/* -    SPICELIB Version 1.0.0, 22-SEP-1994 (NJB) (WLT) */

/* -& */
/* $ Index_Entries */

/*     EK row vector address calculation */

/* -& */

/*     Use discovery check-in for speed; don't check RETURN. */


/*     If the index is out of range, that's an error. */

    if (*rwvidx < 1 || *rwvidx > maxrwv) {
	chkin_("ZZEKVCAL", (ftnlen)8);
	setmsg_("Row vector index was #; valid range is 0:#", (ftnlen)42);
	errint_("#", rwvidx, (ftnlen)1);
	errint_("#", &maxrwv, (ftnlen)1);
	sigerr_("SPICE(INVALIDINDEX)", (ftnlen)19);
	chkout_("ZZEKVCAL", (ftnlen)8);
	return 0;
    }

/*     Identify the join row set containing the indicated row.  Our error */
/*     check guarantees a non-zero result. */

    jrsidx = lstlei_(rwvidx, &svnjrs, begidx);

/*     Compute the offset of the indicated row vector relative to the */
/*     first row vector in the parent join row set.  This offset is one */
/*     less than the relative index of the row vector, multiplied by */
/*     the augmented row vector size. */

    reloff = (*rwvidx - begidx[(i__1 = jrsidx - 1) < 200 && 0 <= i__1 ? i__1 :
	     s_rnge("begidx", i__1, "zzekvadr_", (ftnlen)818)]) * (ntabs + 1);

/*     Find the base address of the row vector. */

    *rwvbas = rbas[(i__1 = jrsidx - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge(
	    "rbas", i__1, "zzekvadr_", (ftnlen)823)] + reloff;

/*     Compute the base address of the parent segment vector.  The base- */
/*     relative address of the segment vector is stored at the end of the */
/*     row vector. */

    i__1 = *rwvbas + ntabs + 1;
    i__2 = *rwvbas + ntabs + 1;
    zzeksrd_(&i__1, &i__2, sgvbas);
    *sgvbas = svbas[(i__1 = jrsidx - 1) < 200 && 0 <= i__1 ? i__1 : s_rnge(
	    "svbas", i__1, "zzekvadr_", (ftnlen)832)] + *sgvbas;
    return 0;
} /* zzekvadr_ */

/* Subroutine */ int zzekvadr_(integer *njrs, integer *bases, integer *rwvidx,
	 integer *rwvbas, integer *sgvbas)
{
    return zzekvadr_0_(0, njrs, bases, rwvidx, rwvbas, sgvbas);
    }

/* Subroutine */ int zzekvset_(integer *njrs, integer *bases)
{
    return zzekvadr_0_(1, njrs, bases, (integer *)0, (integer *)0, (integer *)
	    0);
    }

/* Subroutine */ int zzekvcal_(integer *rwvidx, integer *rwvbas, integer *
	sgvbas)
{
    return zzekvadr_0_(2, (integer *)0, (integer *)0, rwvidx, rwvbas, sgvbas);
    }

