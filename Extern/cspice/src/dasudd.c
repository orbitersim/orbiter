/* dasudd.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;

/* $Procedure DASUDD ( DAS, update data, double precision ) */
/* Subroutine */ int dasudd_(integer *handle, integer *first, integer *last, 
	doublereal *data)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    integer n;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer lastc, lastd, recno, lasti, numdp;
    extern /* Subroutine */ int dasa2l_(integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *);
    extern logical failed_(void);
    integer clbase;
    extern /* Subroutine */ int daslla_(integer *, integer *, integer *, 
	    integer *), dasurd_(integer *, integer *, integer *, integer *, 
	    doublereal *);
    integer clsize;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen);
    integer wordno;
    extern logical return_(void);
    integer nwritn;

/* $ Abstract */

/*     Update data in a specified range of double precision addresses */
/*     in a DAS file. */

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

/*     ARRAY */
/*     ASSIGNMENT */
/*     DAS */
/*     FILES */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   DAS file handle. */
/*     FIRST, */
/*     LAST       I   Range of d.p. addresses to write to. */
/*     DATA       I   An array of d.p. numbers. */

/* $ Detailed_Input */

/*     HANDLE   is a file handle of a DAS file opened for writing. */

/*     FIRST, */
/*     LAST     are the first and last of a range of DAS logical */
/*              addresses of double precision numbers to update. These */
/*              addresses satisfy the inequality */

/*                 1  <=   FIRST   <=   LAST   <=   LASTD */

/*              where LASTD is the last double precision logical */
/*              address in use in the DAS file designated by */
/*              HANDLE. */

/*     DATA     is an array of double precision numbers. The */
/*              array elements DATA(1) through DATA(N) will be */
/*              written to the indicated DAS file, where N is */
/*              LAST - FIRST + 1. */

/* $ Detailed_Output */

/*     None. */

/*     See $Particulars for a description of the effect of this routine. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the input file handle is invalid, an error is */
/*         signaled by a routine in the call tree of this routine. */

/*     2)  Only logical addresses that already contain data may be */
/*         updated: if either FIRST or LAST are outside the range */

/*            [ 1,  LASTD ] */

/*         where LASTD is the last double precision logical address */
/*         that currently contains data in the indicated DAS file, the */
/*         error SPICE(INVALIDADDRESS) is signaled. */
/*         The DAS file will not be modified. */

/*     3)  If FIRST > LAST but both addresses are valid, this routine */
/*         will not modify the indicated DAS file. No error will be */
/*         signaled. */

/*     4)  If an I/O error occurs during the data update attempted */
/*         by this routine, the error is signaled by a routine in the */
/*         call tree of this routine. */

/* $ Files */

/*     See the description of the argument HANDLE in $Detailed_Input. */

/* $ Particulars */

/*     This routine replaces the double precision data in the specified */
/*     range of logical addresses within a DAS file with the contents of */
/*     the input array DATA. */

/*     The actual physical write operations that update the indicated */
/*     DAS file with the contents of the input array DATA might not take */
/*     place before this routine returns, since the DAS system buffers */
/*     data that is written as well as data that is read. In any case, */
/*     the data will be flushed to the file at the time the file is */
/*     closed, if not earlier. A physical write of all buffered */
/*     records can be forced by calling the SPICELIB routine DASWBR */
/*     (DAS, write buffered records). */

/*     In order to append double precision data to a DAS file, filling */
/*     in a range of double precision logical addresses that starts */
/*     immediately after the last double precision logical address */
/*     currently in use, the SPICELIB routine DASADD (DAS add data, */
/*     double precision) should be used. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Write to addresses 1 through 200 in a DAS file in random-access */
/*        fashion by updating the file. Recall that data must be present */
/*        in the file before it can be updated. */


/*        Example code begins here. */


/*              PROGRAM DASUDD_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local parameters. */
/*        C */
/*              CHARACTER*(*)         FNAME */
/*              PARAMETER           ( FNAME = 'dasudd_ex1.das' ) */

/*              CHARACTER*(*)         TYPE */
/*              PARAMETER           ( TYPE  = 'TEST' ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              DOUBLE PRECISION      DATA   ( 200 ) */

/*              INTEGER               HANDLE */
/*              INTEGER               I */
/*              INTEGER               J */

/*        C */
/*        C     Open a new DAS file. Use the file name as the internal */
/*        C     file name, and reserve no records for comments. */
/*        C */
/*              CALL DASONW ( FNAME, TYPE, FNAME, 0, HANDLE ) */

/*        C */
/*        C     Append 200 double precision numbers to the file; */
/*        C     after the data are present, we're free to update it */
/*        C     in any order we please.  (CLEARD zeros out a double */
/*        C     precision array.) */
/*        C */
/*              CALL CLEARD (          200,  DATA ) */
/*              CALL DASADD ( HANDLE,  200,  DATA ) */

/*        C */
/*        C     Now the double precision logical addresses 1:200 */
/*        C     can be written to in random-access fashion.  We'll */
/*        C     fill them in reverse order. */
/*        C */
/*              DO I = 200, 1, -1 */
/*                 CALL DASUDD ( HANDLE, I, I, DBLE(I) ) */
/*              END DO */

/*        C */
/*        C     Close the file. */
/*        C */
/*              CALL DASCLS ( HANDLE ) */

/*        C */
/*        C     Now make sure that we updated the file properly. */
/*        C     Open the file for reading and dump the contents */
/*        C     of the double precision logical addresses 1:200. */
/*        C */
/*              CALL DASOPR ( FNAME, HANDLE ) */

/*              CALL CLEARD (              200,  DATA  ) */
/*              CALL DASRDD (  HANDLE,  1, 200,  DATA  ) */

/*              WRITE (*,*) */
/*              WRITE (*,*) 'Data from "', FNAME, '":' */
/*              WRITE (*,*) */
/*              DO I = 1, 25 */
/*                 WRITE (*,'(8F7.1)') (DATA((I-1)*8+J), J = 1, 8) */
/*              END DO */

/*        C */
/*        C     Close the file. */
/*        C */
/*              CALL DASCLS ( HANDLE ) */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Data from "dasudd_ex1.das": */

/*            1.0    2.0    3.0    4.0    5.0    6.0    7.0    8.0 */
/*            9.0   10.0   11.0   12.0   13.0   14.0   15.0   16.0 */
/*           17.0   18.0   19.0   20.0   21.0   22.0   23.0   24.0 */
/*           25.0   26.0   27.0   28.0   29.0   30.0   31.0   32.0 */
/*           33.0   34.0   35.0   36.0   37.0   38.0   39.0   40.0 */
/*           41.0   42.0   43.0   44.0   45.0   46.0   47.0   48.0 */
/*           49.0   50.0   51.0   52.0   53.0   54.0   55.0   56.0 */
/*           57.0   58.0   59.0   60.0   61.0   62.0   63.0   64.0 */
/*           65.0   66.0   67.0   68.0   69.0   70.0   71.0   72.0 */
/*           73.0   74.0   75.0   76.0   77.0   78.0   79.0   80.0 */
/*           81.0   82.0   83.0   84.0   85.0   86.0   87.0   88.0 */
/*           89.0   90.0   91.0   92.0   93.0   94.0   95.0   96.0 */
/*           97.0   98.0   99.0  100.0  101.0  102.0  103.0  104.0 */
/*          105.0  106.0  107.0  108.0  109.0  110.0  111.0  112.0 */
/*          113.0  114.0  115.0  116.0  117.0  118.0  119.0  120.0 */
/*          121.0  122.0  123.0  124.0  125.0  126.0  127.0  128.0 */
/*          129.0  130.0  131.0  132.0  133.0  134.0  135.0  136.0 */
/*          137.0  138.0  139.0  140.0  141.0  142.0  143.0  144.0 */
/*          145.0  146.0  147.0  148.0  149.0  150.0  151.0  152.0 */
/*          153.0  154.0  155.0  156.0  157.0  158.0  159.0  160.0 */
/*          161.0  162.0  163.0  164.0  165.0  166.0  167.0  168.0 */
/*          169.0  170.0  171.0  172.0  173.0  174.0  175.0  176.0 */
/*          177.0  178.0  179.0  180.0  181.0  182.0  183.0  184.0 */
/*          185.0  186.0  187.0  188.0  189.0  190.0  191.0  192.0 */
/*          193.0  194.0  195.0  196.0  197.0  198.0  199.0  200.0 */


/*        Note that after run completion, a new DAS file exists in the */
/*        output directory. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.3.0, 16-JUL-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Updated entries in $Revisions section. */

/*        Edited the header to comply with NAIF standard. Fixed */
/*        bugs in the code example and modified the output presentation */
/*        to comply with the maximum line length for header comments. */

/* -    SPICELIB Version 1.2.0, 10-APR-2014 (NJB) */

/*        Deleted declarations of unused parameters. */

/*        Corrected header comments: routine that flushes */
/*        written, buffered records is DASWBR, not DASWUR. */

/* -    SPICELIB Version 1.1.1, 19-DEC-1995 (NJB) */

/*        Corrected title of permuted index entry section. */

/* -    SPICELIB Version 1.1.0, 12-MAY-1994 (KRG) (NJB) */

/*        Test of FAILED() added to loop termination conditions. */

/*        Removed references to specific DAS file open routines in the */
/*        $Detailed_Input section of the header. This was done in order */
/*        to minimize documentation changes if the DAS open routines ever */
/*        change. */

/*        Modified the $Examples section to demonstrate the new ID word */
/*        format which includes a file type and to include a call to the */
/*        new routine DASONW, open new for write, which makes use of the */
/*        file type. Also,  a variable for the type of the file to be */
/*        created was added. */

/* -    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) */

/* -& */
/* $ Index_Entries */

/*     update double precision data in a DAS file */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.1.0, 12-MAY-1994 (KRG) (NJB) */

/*        Test of FAILED() added to loop termination condition. Without */
/*        this test, an infinite loop could result if DASA2L or DASURD */
/*        signaled an error inside the loop. */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("DASUDD", (ftnlen)6);
    }

/*     Get the last logical addresses in use in this DAS file. */

    daslla_(handle, &lastc, &lastd, &lasti);

/*     Validate the input addresses. */

    if (*first < 1 || *first > lastd || *last < 1 || *last > lastd) {
	setmsg_("FIRST was #. LAST was #. Valid range is [1,#].", (ftnlen)46);
	errint_("#", first, (ftnlen)1);
	errint_("#", last, (ftnlen)1);
	errint_("#", &lastd, (ftnlen)1);
	sigerr_("SPICE(INVALIDADDRESS)", (ftnlen)21);
	chkout_("DASUDD", (ftnlen)6);
	return 0;
    }

/*     Let N be the number of addresses to update. */

    n = *last - *first + 1;

/*     We will use the variables RECNO and WORDNO to determine where to */
/*     write data in the DAS file.  RECNO will be the record containing */
/*     the physical location  to write to;  WORDNO will be the word */
/*     location that we will write to next. */

/*     Find the first location to write to.  CLBASE and CLSIZE are the */
/*     base record number and size of the cluster of d.p. records that */
/*     the address FIRST lies within. */

    dasa2l_(handle, &c__2, first, &clbase, &clsize, &recno, &wordno);

/*     Set the number of double precision words already written.  Keep */
/*     writing to the file until this number equals the number of */
/*     elements in DATA. */

/*     Note that if N is non-positive, the loop doesn't get exercised. */


    nwritn = 0;
    while(nwritn < n && ! failed_()) {

/*        Write as much data as we can (or need to) into the current */
/*        record.  We assume that CLBASE, RECNO, WORDNO, and NWRITN have */
/*        been set correctly at this point. */

/*        Find out how many words to write into the current record. */
/*        There may be no space left in the current record. */

/* Computing MIN */
	i__1 = n - nwritn, i__2 = 128 - wordno + 1;
	numdp = min(i__1,i__2);
	if (numdp > 0) {

/*           Write NUMDP words into the current record. */

	    i__1 = wordno + numdp - 1;
	    dasurd_(handle, &recno, &wordno, &i__1, &data[nwritn]);
	    nwritn += numdp;
	    wordno += numdp;
	} else {

/*           It's time to start on a new record.  If the record we */
/*           just finished writing to (or just attempted writing to, */
/*           if it was full) was not the last of the cluster, the next */
/*           record to write to is the immediate successor of the last */
/*           one.  Otherwise, we'll have to look up the location of the */
/*           next d.p. logical address. */

	    if (recno < clbase + clsize - 1) {
		++recno;
		wordno = 1;
	    } else {
		i__1 = *first + nwritn;
		dasa2l_(handle, &c__2, &i__1, &clbase, &clsize, &recno, &
			wordno);
	    }
	}
    }
    chkout_("DASUDD", (ftnlen)6);
    return 0;
} /* dasudd_ */

