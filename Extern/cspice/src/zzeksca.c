/* zzeksca.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c_b65 = 2500000;

/* $Procedure      ZZEKSCA ( EK, scratch area ) */
/* Subroutine */ int zzeksca_0_(int n__, integer *n, integer *beg, integer *
	end, integer *idata, integer *top)
{
    /* Initialized data */

    static logical first = TRUE_;
    static integer t = 0;

    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    static integer base, b, e, i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    static integer lastc, lastd, lasti, numrd, start;
    extern logical failed_(void);
    static integer rb;
    extern /* Subroutine */ int dasadi_(integer *, integer *, integer *), 
	    cleari_(integer *, integer *), daslla_(integer *, integer *, 
	    integer *, integer *), dasllc_(integer *);
    static integer rt;
    extern /* Subroutine */ int dasrdi_(integer *, integer *, integer *, 
	    integer *);
    static integer numadd;
    extern /* Subroutine */ int dasudi_(integer *, integer *, integer *, 
	    integer *);
    static integer remain, scrhan;
    extern /* Subroutine */ int daswbr_(integer *);
    static integer scrtch[2500000];
    extern /* Subroutine */ int dasops_(integer *), sigerr_(char *, ftnlen), 
	    chkout_(char *, ftnlen), setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Manage the EK scratch area. */

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
/*     N          I   ZZEKSPSH, ZZEKSPOP, ZZEKSDEC */
/*     BEG        I   ZZEKSUPD, ZZEKSRD */
/*     END        I   ZZEKSUPD, ZZEKSRD */
/*     IDATA     I-O  ZZEKSPSH, ZZEKSPOP, ZZEKSUPD, ZZEKSRD */
/*     TOP        O   ZZEKSTOP */

/* $ Detailed_Input */

/*     See the entry points for descriptions of their inputs. */

/* $ Detailed_Output */

/*     See the entry points for descriptions of their outputs. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If this routine is called directly, the error SPICE(BOGUSENTRY) */
/*        is signaled. */

/*     See the entry points for discussions of exceptions specific to */
/*     those routines. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The specific implementation of the EK scratch area is NOT */
/*     considered part of the specification of this suite of routines: */
/*     the implementation may be changed without notice.  However, */
/*     some aspects of the current implementation, such as scratch */
/*     file usage, are visible to users and therefore are discussed */
/*     in this subroutine header. */

/*     The EK system, in searching for events that satisfy a query, */
/*     produces intermediate results that require a potentially very */
/*     large amount of storage, more than can be expected to be */
/*     available in the form of memory.  On the other hand, in order */
/*     to achieve reasonable query response time, these intermediate */
/*     results must be capable of being accessed quickly.  The EK */
/*     scratch area provides a storage location that uses a combination */
/*     of memory and disk storage to give the EK system a large storage */
/*     area, part of which can be rapidly accessed. */

/*     The logical structure of the EK scratch area is that of a large */
/*     one-dimensional integer stack.  The indices of the  elements of */
/*     this stack are referred to as scratch area `addresses'.  Scratch */
/*     area addresses start at 1 and increase.  The maximum address is */
/*     the maximum integer representable on the host computer, but the */
/*     maximum usable address depends on the disk storage available */
/*     to the calling program at the time the program is run. */

/*     The EK scratch area has access routines that allow a calling */
/*     program to write to and read from it. Calling routines must */
/*     coordinate their use of the scratch area:  the scratch area is */
/*     effectively a global data structure.  Routines outside of the EK */
/*     system should not use the scratch area. */

/*     The EK scratch area routines are: */

/*        ZZEKSCA  ( EK scratch area umbrella routine ) */
/*        ZZEKSTOP ( EK scratch area, return stack pointer ) */
/*        ZZEKSPSH ( EK scratch area, push data onto stack ) */
/*        ZZEKSDEC ( EK scratch area, decrement stack pointer ) */
/*        ZZEKSPOP ( EK scratch area, pop data from stack ) */
/*        ZZEKSUPD ( EK scratch area, update data ) */
/*        ZZEKSRD  ( EK scratch area, read data ) */
/*        ZZEKSCLN ( EK scratch area, clean up ) */

/* $ Examples */

/*     1)  Push data on the scratch area stack. */

/*            C */
/*            C     Push N items onto the stack. */
/*            C */
/*                  CALL ZZEKSPSH ( N, DATA ) */


/*     2)  Update a range of addresses that may span the stack top. */

/*            C */
/*            C     Since we can't leave a gap between the stack top */
/*            C     and the start of the range of addresses we write to, */
/*            C     we'll need to know where the top is.  The address */
/*            C     range to update is BEG:END. */
/*            C */
/*                  CALL ZZEKSTOP ( TOP ) */

/*                  IF ( BEG .GT. TOP ) THEN */

/*                     [ Handle error case ] */

/*                  ELSE */

/*                     CALL ZZEKSUPD ( BEG, END, DATA ) */

/*                  END IF */



/*     3)  Read from the scratch area. */

/*            C */
/*            C     Read the contents of the scratch area address */
/*            C     range BEG:END into the integer array DATA: */
/*            C */
/*                  CALL ZZEKSTOP ( TOP ) */

/*                  IF ( BEG .GT. TOP ) THEN */

/*                     [ Handle error case ] */

/*                  ELSE */

/*                     CALL ZZEKSRD ( BEG, END, DATA ) */

/*                  END IF */

/* $ Restrictions */

/*     1)  The current implementation of this suite of routines opens */
/*         a scratch file.  The logical unit connected to the scratch */
/*         file counts against the total that may be used by the calling */
/*         program.  Also, the scratch file, if written to, will occupy */
/*         additional disk storage. */

/*     2)  This suite of routines should not be used by routines outside */
/*         of the EK system. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 3.2.1, 03-OCT-2021 (NJB) */

/*        Corrected typos in comments. */

/* -    SPICELIB Version 3.2.0, 28-JUN-2005 (NJB) */

/*        Increased buffer size from 500K to 2M integers. */

/* -    SPICELIB Version 3.1.0, 29-JUL-2003 (NJB) */

/*        Added DASWBR call to entry point ZZEKCLN.  This call frees */
/*        the buffer records used by the scratch file. */

/* -    SPICELIB Version 3.0.0, 13-DEC-2001 (NJB) */

/*        Added entry point ZZEKCLN. */

/* -    Beta Version 2.0.0, 02-NOV-1995 (NJB) */

/*        Updated for EK architecture 3. */

/* -    Beta Version 1.1.0, 01-AUG-1994 (NJB) */

/*        Scratch area buffer size increased to 500K integers. */
/*        On 32-bit systems, this amounts to 2Mb of storage. */

/* -    Beta Version 1.0.1, 25-FEB-1993 (NJB) */

/*        Documented. */

/* -    Beta Version 1.0.0, 16-DEC-1992 (NJB) */

/* -& */
/* $ Index_Entries */

/*     manage the EK scratch area */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 3.2.0, 28-JUN-2005 (NJB) */

/*        Increased buffer size from 500K to 2M integers. */

/* -    Beta Version 2.0.0, 08-SEP-1994 (NJB) */

/*        Updated for EK architecture 3. */

/* -    Beta Version 1.1.0, 01-AUG-1994 (NJB) */

/*        Scratch area buffer size increased to 500K integers. */
/*        On 32-bit systems, this amounts to 2Mb of storage. */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     The parameter MEMSIZ is the size of an integer array used as */
/*     part of the scratch area.  The first MEMSIZ scratch area addresses */
/*     refer to elements of this array.  Additional storage is supplied */
/*     by the integer logical array of a scratch DAS file; the first */
/*     word of the scratch DAS file corresponds to scratch area address */
/*     MEMSIZ + 1. */


/*     Local variables */


/*     Saved variables */


/*     Initial values */

    /* Parameter adjustments */
    if (idata) {
	}

    /* Function Body */
    switch(n__) {
	case 1: goto L_zzekstop;
	case 2: goto L_zzekspsh;
	case 3: goto L_zzekspop;
	case 4: goto L_zzeksdec;
	case 5: goto L_zzeksupd;
	case 6: goto L_zzeksrd;
	case 7: goto L_zzekscln;
	}


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("ZZEKSCA", (ftnlen)7);
    }

/*     This routine should never be called directly. */

    sigerr_("SPICE(BOGUSENTRY)", (ftnlen)17);
    chkout_("ZZEKSCA", (ftnlen)7);
    return 0;
/* $Procedure    ZZEKSTOP  ( EK scratch area, stack top ) */

L_zzekstop:
/* $ Abstract */

/*     Obtain last address in use in EK scratch area. */

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

/*     INTEGER               TOP */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     TOP        O   EK scratch area stack top. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     TOP            is the last address of the EK scratch area stack */
/*                    top.  This is the highest EK scratch area address */
/*                    currently in use. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The EK scratch area stack top ranges from zero to, theoretically, */
/*     the largest integer representable on the host system. */
/*     and never decreases during a program run.  Data pushed on the */
/*     EK stack is inserted at address TOP+1 and occupies a contiguous */
/*     range of addresses that extends upwards from this address. */

/* $ Examples */

/*     See the header of the umbrella routine ZZEKSCA for an example */
/*     of use of this routine. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    Beta Version 2.0.0, 08-SEP-1994 (NJB) */

/*        Updated for EK architecture 3. */

/* -    Beta Version 1.0.0, 25-FEB-1993 (NJB) */

/* -& */
/* $ Index_Entries */

/*     read from EK scratch area */

/* -& */
    *top = t;
    return 0;
/* $Procedure    ZZEKSPSH  ( EK scratch area, push data ) */

L_zzekspsh:
/* $ Abstract */

/*     Push the contents of an integer array onto the EK scratch area */
/*     stack. */

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

/*     INTEGER               N */
/*     INTEGER               IDATA ( * ) */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     N          I   Number of integers to push. */
/*     IDATA      I   Integer data. */

/* $ Detailed_Input */

/*     N              is the number of integers in the array IDATA to */
/*                    append to the EK scratch area.  The data is */
/*                    stored in scratch area addresses T+1:T+N, */
/*                    where T is the EK scratch area stack top prior to */
/*                    the call to ZZEKSPSH. */

/*     IDATA          is an integer array containing data to append to */
/*                    the EK scratch area.  The first N elements of */
/*                    IDATA are appended to the EK scratch area, in */
/*                    order. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If N is non-positive, this routine simply returns.  No error */
/*         is signaled. */

/*     2)  If an I/O error occurs during the data addition, the error */
/*         will be diagnosed by routines called by this routine. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Let TOP be the EK scratch area stack top prior to a call to this */
/*     routine.  Data that is appended to the EK scratch area by this */
/*     routine is inserted at address TOP+1 and occupies a contiguous */
/*     range of addresses that extends upwards from this address. */

/*     As a side effect of calling this routine, TOP is set to TOP + N. */

/* $ Examples */

/*     See the header of the umbrella routine ZZEKSCA for an example */
/*     of use of this routine. */

/* $ Restrictions */

/*     1)  This routine must execute quickly.  Therefore, it checks in */
/*         only if it detects an error.  If an error is signaled by a */
/*         routine called by this routine, this routine will not appear */
/*         in the SPICELIB traceback display.  Also, in the interest */
/*         of speed, this routine does not test the value of the SPICELIB */
/*         function RETURN upon entry. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB) */

/*        Corrected typos in comments. */

/* -    Beta Version 1.0.0, 15-JAN-1995 (NJB) */

/* -& */
/* $ Index_Entries */

/*     push integer data onto EK scratch area stack */

/* -& */

/*     No checking in here. */


/*     First time through, open a scratch DAS file. */

    if (first) {
	first = FALSE_;
	dasops_(&scrhan);
	if (failed_()) {
	    return 0;
	}
    }

/*     Go back if there's no data to write. */

    if (*n < 1) {
	return 0;
    }

/*     Add as much data as possible to our big array. */

    if (t < 2500000) {
/* Computing MIN */
	i__1 = *n, i__2 = 2500000 - t;
	numadd = min(i__1,i__2);
	i__1 = numadd;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    scrtch[(i__2 = t + i__ - 1) < 2500000 && 0 <= i__2 ? i__2 : 
		    s_rnge("scrtch", i__2, "zzeksca_", (ftnlen)632)] = idata[
		    i__ - 1];
	}
	t += numadd;
	if (numadd == *n) {
	    return 0;
	}
	remain = *n - numadd;
	start = numadd + 1;
	if (remain == 0) {
	    return 0;
	}
    } else {
	remain = *n;
	start = 1;
    }

/*     At this point, REMAIN and START are set, and T reflects the */
/*     amount of data we've pushed so far.  If we got this far, */
/*     we'll need to put the rest of the data in the scratch DAS. */

/*     The DAS system requires separate operations for updating */
/*     an existing range of addresses and for appending data. */
/*     We need to know the last integer address in use in the DAS */
/*     file in order to determine which part of the data will */
/*     be written to addresses previously written to, and which */
/*     part will be appended. */

    daslla_(&scrhan, &lastc, &lastd, &lasti);

/*     To simplify our arithmetic, we'll work with a variable RT */
/*     that represents the stack top measured relative to the base */
/*     of the DAS integer array.  At this point, RT is greater than */
/*     or equal to zero. */

    rt = t - 2500000;
    if (rt < lasti) {

/*        Some data can be added by updating DAS addresses.  The */
/*        available range for updating is B:E, where B and E are */
/*        calculated below.  This case can occur only when LASTI > 0. */

	b = rt + 1;
/* Computing MIN */
	i__1 = lasti, i__2 = rt + remain;
	e = min(i__1,i__2);
	dasudi_(&scrhan, &b, &e, &idata[start - 1]);
	numadd = e - b + 1;
	start += numadd;
	remain -= numadd;
	t += numadd;
	if (remain == 0) {
	    return 0;
	}
    }

/*     At this point, START and REMAIN are set, and T reflects the */
/*     amount of data we've pushed so far..  The remaining data */
/*     must be appended to the scratch DAS file. */

    dasadi_(&scrhan, &remain, &idata[start - 1]);
    t += remain;
    return 0;
/* $Procedure    ZZEKSPOP  ( EK scratch area, pop data ) */

L_zzekspop:
/* $ Abstract */

/*     Pop a specified number of elements from the top of the EK scratch */
/*     area stack, transferring this data to an integer array. */

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

/*     INTEGER               N */
/*     INTEGER               IDATA ( * ) */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     N          I   Number of integers to pop. */
/*     IDATA      O   Integer data. */

/* $ Detailed_Input */

/*     N              is the number of integers to pop from the */
/*                    EK scratch area stack.  The data is */
/*                    read from the scratch area addresses T-N+1:T, */
/*                    where T is the stack top prior to the call to */
/*                    ZZEKSPOP. */

/* $ Detailed_Output */

/*     IDATA          is an integer array containing data read from */
/*                    the EK scratch area.  The first N elements of */
/*                    IDATA assigned the values occupying the top N */
/*                    elements of the EK stack. */
/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If N is non-positive or if N is greater than the number of */
/*         items on the stack, the error SPICE(INVALIDCOUNT) is */
/*         signaled. */

/*     2)  If an I/O error occurs during the data read, the error */
/*         will be diagnosed by routines called by this routine. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Let TOP be the EK scratch area stack top prior to a call to this */
/*     routine.  Data that is read from the EK scratch area by this */
/*     routine is transferred from addresses TOP-N+1 to TOP and occupies */
/*     to the range of addresses 1 to N in the array IDATA. */

/*     As a side effect of calling this routine, TOP is set to TOP - N. */

/* $ Examples */

/*     See the header of the umbrella routine ZZEKSCA for an example */
/*     of use of this routine. */

/* $ Restrictions */

/*     1)  This routine must execute quickly.  Therefore, it checks in */
/*         only if it detects an error.  If an error is signaled by a */
/*         routine called by this routine, this routine will not appear */
/*         in the SPICELIB traceback display.  Also, in the interest */
/*         of speed, this routine does not test the value of the SPICELIB */
/*         function RETURN upon entry. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB) */

/*        Corrected typos in comments. */

/* -    Beta Version 1.0.0, 10-SEP-1994 (NJB) */

/* -& */
/* $ Index_Entries */

/*     pop integer data from EK scratch area stack */

/* -& */

/*     No checking in here. */


/*     First time through, open a scratch DAS file. */

    if (first) {
	first = FALSE_;
	dasops_(&scrhan);
	if (failed_()) {
	    return 0;
	}
    }

/*     You can't pop a negative number of elements. */

    if (*n < 0) {
	chkin_("ZZEKSPOP", (ftnlen)8);
	setmsg_("Pop count must be non-negative; call requests popping # ele"
		"ments.", (ftnlen)65);
	errint_("#", n, (ftnlen)1);
	sigerr_("SPICE(INVALIDCOUNT)", (ftnlen)19);
	chkout_("ZZEKSPOP", (ftnlen)8);
	return 0;

/*     It's an error to try to pop more data than we have on the */
/*     stack. */

    } else if (*n > t) {
	chkin_("ZZEKSPOP", (ftnlen)8);
	setmsg_("EK stack pointer = #; call requests popping # items.", (
		ftnlen)52);
	errint_("#", &t, (ftnlen)1);
	errint_("#", n, (ftnlen)1);
	sigerr_("SPICE(INVALIDCOUNT)", (ftnlen)19);
	chkout_("ZZEKSPOP", (ftnlen)8);
	return 0;
    }

/*     Read as much data as possible from our big array. */

    base = t - *n;
    if (base < 2500000) {
/* Computing MIN */
	i__1 = *n, i__2 = 2500000 - base;
	numrd = min(i__1,i__2);
	i__1 = numrd;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    idata[i__ - 1] = scrtch[(i__2 = base + i__ - 1) < 2500000 && 0 <= 
		    i__2 ? i__2 : s_rnge("scrtch", i__2, "zzeksca_", (ftnlen)
		    907)];
	}
	if (numrd == *n) {
	    t -= numrd;
	    return 0;
	}
	remain = *n - numrd;
	base = 2500000;
	start = numrd + 1;
    } else {
	remain = *n;
	start = 1;
    }

/*     At this point, REMAIN, START and BASE are set.  If we got this */
/*     far, we'll need to read the rest of the data from the scratch DAS. */
/*     Compute the base address to read from relative to the start of */
/*     the DAS array. */

    rb = base - 2500000;
    b = rb + 1;
    e = rb + remain;
    dasrdi_(&scrhan, &b, &e, &idata[start - 1]);
    t -= *n;
    return 0;
/* $Procedure    ZZEKSDEC  ( EK scratch area, decrement stack pointer ) */

L_zzeksdec:
/* $ Abstract */

/*     Decrement the EK scratch area stack pointer by a specified count. */

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

/*     INTEGER               N */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     N          I   Decrement count. */

/* $ Detailed_Input */

/*     N              is the number to subtract from the EK scratch */
/*                    area stack pointer. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If N is non-positive or if N is greater than the number of */
/*         items on the stack, the error SPICE(INVALIDCOUNT) is */
/*         signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Let TOP be the EK scratch area stack top prior to a call to this */
/*     routine.  The  effect of calling this routine is that TOP is set */
/*     to TOP - N. */

/* $ Examples */

/*     See the header of the umbrella routine ZZEKSCA for an example */
/*     of use of this routine. */

/* $ Restrictions */

/*     1)  This routine must execute quickly.  Therefore, it checks in */
/*         only if it detects an error.  If an error is signaled by a */
/*         routine called by this routine, this routine will not appear */
/*         in the SPICELIB traceback display.  Also, in the interest */
/*         of speed, this routine does not test the value of the SPICELIB */
/*         function RETURN upon entry. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB) */

/*        Corrected typos in comments. */

/* -    Beta Version 1.0.0, 10-SEP-1994 (NJB) */

/* -& */
/* $ Index_Entries */

/*     decrement EK scratch area stack pointer */

/* -& */

/*     No checking in here. */


/*     First time through, open a scratch DAS file. */

    if (first) {
	first = FALSE_;
	dasops_(&scrhan);
	if (failed_()) {
	    return 0;
	}
    }

/*     Catch non-positive decrement requests. */

    if (*n < 0) {
	chkin_("ZZEKSDEC", (ftnlen)8);
	setmsg_("Decrement value must be non-negative; call requests decreme"
		"nt by #.", (ftnlen)67);
	errint_("#", n, (ftnlen)1);
	sigerr_("SPICE(INVALIDCOUNT)", (ftnlen)19);
	chkout_("ZZEKSDEC", (ftnlen)8);
	return 0;

/*     It's an error to try to decrement the pointer by more than */
/*     the current stack depth. */

    } else if (*n > t) {
	chkin_("ZZEKSDEC", (ftnlen)8);
	setmsg_("EK stack pointer = #; call requests  decrement by #.", (
		ftnlen)52);
	errint_("#", &t, (ftnlen)1);
	errint_("#", n, (ftnlen)1);
	sigerr_("SPICE(INVALIDCOUNT)", (ftnlen)19);
	chkout_("ZZEKSDEC", (ftnlen)8);
	return 0;
    }
    t -= *n;
    return 0;
/* $Procedure    ZZEKSUPD  ( EK scratch area, update ) */

L_zzeksupd:
/* $ Abstract */

/*     Update the contents of a range of addresses already in use in the */
/*     EK scratch area. */

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

/*     INTEGER               BEG */
/*     INTEGER               END */
/*     INTEGER               IDATA ( * ) */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     BEG, */
/*     END        I   Begin and end addresses of range to update. */
/*     IDATA      I   Integer data. */

/* $ Detailed_Input */

/*     BEG, */
/*     END            are the first and last of a range of EK scratch */
/*                    area addresses to write to.  BEG and END must */
/*                    satisfy the relations */

/*                       1  <  BEG  <  END  <  TOP */
/*                          -       -       - */

/*                    where TOP is the last EK scratch area stack top */
/*                    at the time this routine is called. */

/*     IDATA          is an integer array containing data to write to */
/*                    the specified range of addresses in the EK scratch */
/*                    area.  The first END-BEG+1 elements of IDATA are */
/*                    written to the specified range in the EK scratch */
/*                    area, in order. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If either of BEG or END are outside of the range 1:TOP, */
/*         where TOP is the EK scratch area stack top, the error */
/*         SPICE(INVALIDADDRESS) is signaled. */

/*     2)  If END < BEG, this routine simply returns.  No error */
/*         is signaled. */

/*     3)  If an I/O error occurs during the data addition, the error */
/*         will be diagnosed by routines called by this routine. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Let TOP be the EK scratch area stack top prior to a call to this */
/*     routine.  This routine is used to modify values in the scratch */
/*     area that lie in the address range 1:TOP. */

/* $ Examples */

/*     See the header of the umbrella routine ZZEKSCA for an example */
/*     of use of this routine. */

/* $ Restrictions */

/*     1)  This routine must execute quickly.  Therefore, it checks in */
/*         only if it detects an error.  If an error is signaled by a */
/*         routine called by this routine, this routine will not appear */
/*         in the SPICELIB traceback display.  Also, in the interest */
/*         of speed, this routine does not test the value of the SPICELIB */
/*         function RETURN upon entry. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.1, 03-OCT-2021 (NJB) */

/*        Corrected typos in comments. */

/* -    Beta Version 2.0.0, 23-FEB-1995 (NJB) */

/*        Updated for EK architecture 3. */

/* -    Beta Version 1.0.0, 25-FEB-1993 (NJB) */

/* -& */
/* $ Index_Entries */

/*     update data in EK scratch area */

/* -& */

/*     No checking in here. */


/*     Validate the addresses. */

    if (*beg < 1 || *beg > t) {
	chkin_("ZZEKSUPD", (ftnlen)8);
	setmsg_("Start address BEG was #; valid range is 1:#", (ftnlen)43);
	errint_("#", beg, (ftnlen)1);
	errint_("#", &t, (ftnlen)1);
	sigerr_("SPICE(INVALIDADDRESS)", (ftnlen)21);
	chkout_("ZZEKSUPD", (ftnlen)8);
	return 0;
    } else if (*end < 1 || *end > t) {
	chkin_("ZZEKSUPD", (ftnlen)8);
	setmsg_("End address END was #; valid range is 1:#", (ftnlen)41);
	errint_("#", end, (ftnlen)1);
	errint_("#", &t, (ftnlen)1);
	sigerr_("SPICE(INVALIDADDRESS)", (ftnlen)21);
	chkout_("ZZEKSUPD", (ftnlen)8);
	return 0;
    } else if (*beg > *end) {
	return 0;
    }
    if (*end <= 2500000) {

/*        If the entire range is in memory, fine.  Update the range */
/*        now. */

	i__1 = *end;
	for (i__ = *beg; i__ <= i__1; ++i__) {
	    scrtch[(i__2 = i__ - 1) < 2500000 && 0 <= i__2 ? i__2 : s_rnge(
		    "scrtch", i__2, "zzeksca_", (ftnlen)1316)] = idata[i__ - *
		    beg];
	}
    } else if (*beg <= 2500000) {

/*        Update the portion of the address range that's in memory. */

	for (i__ = *beg; i__ <= 2500000; ++i__) {
	    scrtch[(i__1 = i__ - 1) < 2500000 && 0 <= i__1 ? i__1 : s_rnge(
		    "scrtch", i__1, "zzeksca_", (ftnlen)1325)] = idata[i__ - *
		    beg];
	}

/*        Now update the rest of the range, which is in the scratch */
/*        DAS file. */

	i__1 = *end - 2500000;
	dasudi_(&scrhan, &c__1, &i__1, &idata[2500000 - *beg + 1]);
    } else {

/*        The whole range is in the DAS file. */

	i__1 = *beg - 2500000;
	i__2 = *end - 2500000;
	dasudi_(&scrhan, &i__1, &i__2, idata);
    }
    return 0;
/* $Procedure    ZZEKSRD  ( EK scratch area, read ) */

L_zzeksrd:
/* $ Abstract */

/*     Read from a range of addresses in the EK scratch area. */

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

/*     INTEGER               BEG */
/*     INTEGER               END */
/*     INTEGER               IDATA ( * ) */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     BEG, */
/*     END        I   Begin and end addresses of range to read from. */
/*     IDATA      O   Integer data. */

/* $ Detailed_Input */

/*     BEG, */
/*     END            are the first and last of a range of EK scratch */
/*                    area addresses to read from. BEG and END must */
/*                    satisfy the relations */

/*                       1  <  BEG  <  END  <  LAST */
/*                          -       -       - */

/*                    where LAST is the last EK scratch area address */
/*                    in use at the time this routine is called. */

/* $ Detailed_Output */

/*     IDATA          is an integer array containing data read from the */
/*                    range of addresses BEG:END in the EK scratch area. */
/*                    The first END-BEG+1 elements of IDATA are assigned */
/*                    in order using the contents of this address range. */
/*                    IDATA must have dimension at least END-BEG+1. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If either of BEG or END are outside of the range 1:LAST, */
/*         where LAST is the last address already in use in the EK */
/*         scratch area, the error SPICE(INVALIDADDRESS) is signaled. */

/*     2)  If END < BEG, this routine simply returns.  No error */
/*         is signaled. */

/*     3)  If an I/O error occurs during the read, the error will be */
/*         diagnosed by routines called by this routine. */

/*     4)  If IDATA has dimension less than END-BEG+1, the results of */
/*         a call to this routine will be unpredictable, except that */
/*         you can safely predict they'll be wrong. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Let LAST be the last address in use in the EK scratch area prior */
/*     to a call to this routine.  This routine is used to read values */
/*     in the scratch area that lie in the address range 1:LAST. */

/* $ Examples */

/*     See the header of the umbrella routine ZZEKSCA for an example */
/*     of use of this routine. */

/* $ Restrictions */

/*     1)  This routine must execute quickly.  Therefore, it checks in */
/*         only if it detects an error.  If an error is signaled by a */
/*         routine called by this routine, this routine will not appear */
/*         in the SPICELIB traceback display.  Also, in the interest */
/*         of speed, this routine does not test the value of the SPICELIB */
/*         function RETURN upon entry. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB) */

/*        Corrected typos in comments. */

/* -    Beta Version 1.0.0, 23-FEB-1995 (NJB) */

/* -& */
/* $ Index_Entries */

/*     read from EK scratch area */

/* -& */

/*     No checking in here. */


/*     Validate the addresses. */

    if (*beg < 1 || *beg > t) {
	chkin_("ZZEKSRD", (ftnlen)7);
	setmsg_("Start address BEG was #; valid range is 1:#", (ftnlen)43);
	errint_("#", beg, (ftnlen)1);
	errint_("#", &t, (ftnlen)1);
	sigerr_("SPICE(INVALIDADDRESS)", (ftnlen)21);
	chkout_("ZZEKSRD", (ftnlen)7);
	return 0;
    } else if (*end < 1 || *end > t) {
	chkin_("ZZEKSRD", (ftnlen)7);
	setmsg_("End address END was #; valid range is 1:#", (ftnlen)41);
	errint_("#", end, (ftnlen)1);
	errint_("#", &t, (ftnlen)1);
	sigerr_("SPICE(INVALIDADDRESS)", (ftnlen)21);
	chkout_("ZZEKSRD", (ftnlen)7);
	return 0;
    } else if (*beg > *end) {
	return 0;
    }
    if (*end <= 2500000) {

/*        If the entire range is in memory, fine.  Read from the range */
/*        now. */

	i__1 = *end;
	for (i__ = *beg; i__ <= i__1; ++i__) {
	    idata[i__ - *beg] = scrtch[(i__2 = i__ - 1) < 2500000 && 0 <= 
		    i__2 ? i__2 : s_rnge("scrtch", i__2, "zzeksca_", (ftnlen)
		    1536)];
	}
    } else if (*beg <= 2500000) {

/*        Read from the portion of the address range that's in memory. */

	for (i__ = *beg; i__ <= 2500000; ++i__) {
	    idata[i__ - *beg] = scrtch[(i__1 = i__ - 1) < 2500000 && 0 <= 
		    i__1 ? i__1 : s_rnge("scrtch", i__1, "zzeksca_", (ftnlen)
		    1545)];
	}

/*        Now read the rest of the range, which is in the scratch */
/*        DAS file. */

	i__1 = *end - 2500000;
	dasrdi_(&scrhan, &c__1, &i__1, &idata[2500000 - *beg + 1]);
    } else {

/*        The whole range is in the DAS file. */

	i__1 = *beg - 2500000;
	i__2 = *end - 2500000;
	dasrdi_(&scrhan, &i__1, &i__2, idata);
    }
    return 0;
/* $Procedure    ZZEKSCLN  ( EK scratch area, clean up ) */

L_zzekscln:
/* $ Abstract */

/*     Clean up:  re-initialize the EK scratch area; unload the */
/*     scratch file. */

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

/*     None. */

/* $ Brief_I/O */

/*     None. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     This routine unloads the scratch DAS used by this system. */

/* $ Particulars */

/*     This routine is intended to enable test software to unload */
/*     the scratch DAS file used by the EK scratch area routines. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     1) Many EK routines operate by side effects on the EK scratch */
/*        area, so this routine must be used with caution. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 3.1.0, 29-JUL-2003 (NJB) */

/*        Added DASWBR call.  This call frees the buffer records used by */
/*        the scratch file. */

/* -    SPICELIB Version 3.0.0, 27-DEC-2001 (NJB) */

/* -& */
/* $ Index_Entries */

/*     clean up EK scratch area */

/* -& */

/*     No checking in here. */


/*     Clean out the stack buffer. */

    cleari_(&c_b65, scrtch);
    t = 0;

/*     If FIRST has been set to .FALSE., we've an open scratch DAS */
/*     to dispose of. */

    if (! first) {

/*        Write out the buffered records belonging to the scratch file; */
/*        this will cause them to be returned to the free list. */

	daswbr_(&scrhan);

/*        Dump the scratch DAS. */

	dasllc_(&scrhan);
    }

/*     Tell the system to re-initialize on the next pass. */

    first = TRUE_;
    return 0;
} /* zzeksca_ */

/* Subroutine */ int zzeksca_(integer *n, integer *beg, integer *end, integer 
	*idata, integer *top)
{
    return zzeksca_0_(0, n, beg, end, idata, top);
    }

/* Subroutine */ int zzekstop_(integer *top)
{
    return zzeksca_0_(1, (integer *)0, (integer *)0, (integer *)0, (integer *)
	    0, top);
    }

/* Subroutine */ int zzekspsh_(integer *n, integer *idata)
{
    return zzeksca_0_(2, n, (integer *)0, (integer *)0, idata, (integer *)0);
    }

/* Subroutine */ int zzekspop_(integer *n, integer *idata)
{
    return zzeksca_0_(3, n, (integer *)0, (integer *)0, idata, (integer *)0);
    }

/* Subroutine */ int zzeksdec_(integer *n)
{
    return zzeksca_0_(4, n, (integer *)0, (integer *)0, (integer *)0, (
	    integer *)0);
    }

/* Subroutine */ int zzeksupd_(integer *beg, integer *end, integer *idata)
{
    return zzeksca_0_(5, (integer *)0, beg, end, idata, (integer *)0);
    }

/* Subroutine */ int zzeksrd_(integer *beg, integer *end, integer *idata)
{
    return zzeksca_0_(6, (integer *)0, beg, end, idata, (integer *)0);
    }

/* Subroutine */ int zzekscln_(void)
{
    return zzeksca_0_(7, (integer *)0, (integer *)0, (integer *)0, (integer *)
	    0, (integer *)0);
    }

