/* ckr03.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__6 = 6;

/* $Procedure CKR03 ( C-kernel, read pointing record, data type 3 ) */
/* Subroutine */ int ckr03_(integer *handle, doublereal *descr, doublereal *
	sclkdp, doublereal *tol, logical *needav, doublereal *record, logical 
	*found)
{
    /* Initialized data */

    static doublereal prevs = -1.;
    static doublereal prevn = -1.;
    static integer lhand = 0;
    static integer lbeg = -1;
    static integer lend = -1;

    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer i_dnnt(doublereal *), s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer addr__, skip, psiz, i__, n;
    doublereal ldiff;
    integer laddr;
    doublereal rdiff;
    integer raddr;
    extern /* Subroutine */ int chkin_(char *, ftnlen), dafus_(doublereal *, 
	    integer *, integer *, doublereal *, integer *);
    integer nidir;
    doublereal lsclk;
    extern doublereal dpmax_(void);
    extern /* Subroutine */ int moved_(doublereal *, integer *, doublereal *);
    integer nrdir;
    doublereal rsclk;
    integer group;
    doublereal start;
    extern /* Subroutine */ int dafgda_(integer *, integer *, integer *, 
	    doublereal *);
    extern logical failed_(void);
    integer grpadd;
    doublereal buffer[100];
    integer remain, dirloc;
    extern integer lstled_(doublereal *, integer *, doublereal *);
    integer numrec;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen);
    extern integer lstltd_(doublereal *, integer *, doublereal *);
    integer numint;
    doublereal nstart;
    extern logical return_(void);
    doublereal dcd[2];
    integer beg, icd[6], end;
    logical fnd;

/* $ Abstract */

/*     Read a pointing record from a CK segment, data type 3. */

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

/*     CK */
/*     DAF */

/* $ Keywords */

/*     POINTING */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   File handle. */
/*     DESCR      I   Segment descriptor. */
/*     SCLKDP     I   Pointing request time. */
/*     TOL        I   Time tolerance. */
/*     NEEDAV     I   Angular velocity request flag. */
/*     RECORD     O   Pointing data record. */
/*     FOUND      O   .TRUE. when data is found. */

/* $ Detailed_Input */

/*     HANDLE   is the integer handle of the CK file containing the */
/*              segment. */

/*     DESCR    is the descriptor of the segment. */

/*     SCLKDP   is the encoded spacecraft clock time for which */
/*              pointing is being requested. */

/*     TOL      is a time tolerance, measured in the same units as */
/*              encoded spacecraft clock. */

/*              When SCLKDP falls within the bounds of one of the */
/*              interpolation intervals then the tolerance has no */
/*              effect because pointing will be returned at the */
/*              request time. */

/*              However, if the request time is not in one of the */
/*              intervals, then the tolerance is used to determine */
/*              if pointing at one of the interval endpoints should */
/*              be returned. */

/*     NEEDAV   is .TRUE. if angular velocity is requested. */

/* $ Detailed_Output */

/*     RECORD   is the record that CKE03 will evaluate to determine */
/*              the pointing. */

/*              When the request time falls within an interval for */
/*              which linear interpolation is valid, the values of */
/*              the two pointing instances that bracket the request */
/*              time are returned in RECORD as follows: */

/*                 RECORD( 1  ) = Left bracketing SCLK time. */

/*                 RECORD( 2  ) = lq0  \ */
/*                 RECORD( 3  ) = lq1   \    Left bracketing */
/*                 RECORD( 4  ) = lq2   /      quaternion. */
/*                 RECORD( 5  ) = lq3  / */

/*                 RECORD( 6  ) = lav1 \     Left bracketing */
/*                 RECORD( 7  ) = lav2       angular velocity */
/*                 RECORD( 8  ) = lav3 /       ( optional ) */

/*                 RECORD( 9  ) = Right bracketing SCLK time. */

/*                 RECORD( 10 ) = rq0  \ */
/*                 RECORD( 11 ) = rq1   \    Right bracketing */
/*                 RECORD( 12 ) = rq2   /       quaternion. */
/*                 RECORD( 13 ) = rq3  / */

/*                 RECORD( 14 ) = rav1 \     Right bracketing */
/*                 RECORD( 15 ) = rav2       angular velocity */
/*                 RECORD( 16 ) = rav3 /       ( optional ) */

/*                 RECORD( 17 ) = pointing request time, SCLKDP. */

/*              The quantities lq0 - lq3 and rq0 - rq3 are the */
/*              components of the quaternions that represent the */
/*              C-matrices associated with the times that bracket */
/*              the requested time. */

/*              The quantities lav1, lav2, lav3 and rav1, rav2, rav3 */
/*              are the components of the angular velocity vectors at */
/*              the respective bracketing times. The components of the */
/*              angular velocity vectors are specified relative to */
/*              the inertial reference frame of the segment. */

/*              If the request time does not fall within an */
/*              interpolation interval, but is within TOL of an */
/*              interval endpoint, the values of that pointing */
/*              instance are returned in both parts of RECORD */
/*              ( i.e. RECORD(1-9) and RECORD(10-16) ). */

/*     FOUND    is .TRUE. if a record was found to satisfy the pointing */
/*              request. This occurs when the time for which pointing */
/*              is requested falls inside one of the interpolation */
/*              intervals, or when the request time is within the */
/*              tolerance of an interval endpoint. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the specified handle does not belong to an open DAF file, */
/*         an error is signaled by a routine in the call tree of this */
/*         routine. */

/*     2)  If DESCR is not a valid descriptor of a segment in the CK */
/*         file specified by HANDLE, the results of this routine are */
/*         unpredictable. */

/*     3)  If the segment is not of data type 3, as specified in the */
/*         third integer component of the segment descriptor, */
/*         the error SPICE(WRONGDATATYPE) is signaled. */

/*     4)  If angular velocity data was requested but the segment */
/*         contains no such data, the error SPICE(NOAVDATA) is signaled. */

/* $ Files */

/*     The file containing the segment is specified by its handle and */
/*     should be opened for read or write access, either by CKLPF, */
/*     DAFOPR, or DAFOPW. */

/* $ Particulars */

/*     See the CK Required Reading file for a detailed description of */
/*     the structure of a type 3 pointing segment. */

/*     When the time for which pointing was requested falls within an */
/*     interpolation interval, then FOUND will be true and RECORD will */
/*     contain the pointing instances in the segment that bracket the */
/*     request time.  CKE03 will evaluate RECORD to give pointing at */
/*     the request time. */

/*     However, when the request time is not within any of the */
/*     interpolation intervals, then FOUND will be true only if the */
/*     interval endpoint closest to the request time is within the */
/*     tolerance specified by the user. In this case both parts of */
/*     RECORD will contain this closest pointing instance, and CKE03 */
/*     will evaluate RECORD to give pointing at the time associated */
/*     with the returned pointing instance. */

/* $ Examples */

/*     The CKRnn routines are usually used in tandem with the CKEnn */
/*     routines, which evaluate the record returned by CKRnn to give */
/*     the pointing information and output time. */

/*     The following code fragment searches backwards through all of the */
/*     segments in a file applicable to the Mars Observer spacecraft bus */
/*     that are of data type 3, for a particular spacecraft clock time. */
/*     It then evaluates the pointing for that epoch and prints the */
/*     result. */

/*     The search performed here does not mimic the behavior of the CK */
/*     reader APIs CKGP and CKGPAV, which consider data from multiple CK */
/*     files, when available. See the CK Required reading for details. */

/*           CHARACTER*(20)        SCLKCH */
/*           CHARACTER*(20)        SCTIME */
/*           CHARACTER*(40)        IDENT */

/*           INTEGER               I */
/*           INTEGER               SC */
/*           INTEGER               INST */
/*           INTEGER               HANDLE */
/*           INTEGER               DTYPE */
/*           INTEGER               ICD      (    6 ) */

/*           DOUBLE PRECISION      SCLKDP */
/*           DOUBLE PRECISION      TOL */
/*           DOUBLE PRECISION      CLKOUT */
/*           DOUBLE PRECISION      DESCR    (    5 ) */
/*           DOUBLE PRECISION      DCD      (    2 ) */
/*           DOUBLE PRECISION      RECORD   (   17 ) */
/*           DOUBLE PRECISION      CMAT     ( 3, 3 ) */
/*           DOUBLE PRECISION      AV       (    3 ) */

/*           LOGICAL               NEEDAV */
/*           LOGICAL               FND */
/*           LOGICAL               SFND */


/*           SC     = -94 */
/*           INST   = -94000 */
/*           DTYPE  =  3 */
/*           NEEDAV = .FALSE. */

/*     C */
/*     C     Load the MO SCLK kernel and the C-kernel. */
/*     C */
/*           CALL FURNSH ( 'MO_SCLK.TSC'       ) */
/*           CALL DAFOPR ( 'MO_CK.BC',  HANDLE ) */
/*     C */
/*     C     Get the spacecraft clock time. Then encode it for use */
/*     C     in the C-kernel. */
/*     C */
/*           WRITE (*,*) 'Enter spacecraft clock time string:' */
/*           READ (*,FMT='(A)') SCLKCH */

/*           CALL SCENCD ( SC, SCLKCH, SCLKDP ) */
/*     C */
/*     C     Use a tolerance of 2 seconds ( half of the nominal */
/*     C     separation between MO pointing instances ). */
/*     C */
/*           CALL SCTIKS ( SC, '0000000002:000', TOL ) */

/*     C */
/*     C     Search backwards from the end of the CK file through all */
/*     C     of the segments. */
/*     C */
/*           CALL DAFBBS ( HANDLE ) */
/*           CALL DAFFPA ( SFND   ) */

/*           FND    = .FALSE. */

/*           DO WHILE ( ( SFND ) .AND. ( .NOT. FND ) ) */

/*     C */
/*     C        Get the segment identifier and descriptor. */
/*     C */

/*              CALL DAFGN ( IDENT                 ) */
/*              CALL DAFGS ( DESCR                 ) */
/*     C */
/*     C        Unpack the segment descriptor into its integer and */
/*     C        double precision components. */
/*     C */
/*              CALL DAFUS ( DESCR, 2, 6, DCD, ICD ) */

/*     C */
/*     C        Determine if this segment should be processed. */
/*     C */
/*              IF ( ( INST          .EQ. ICD( 1 ) ) .AND. */
/*          .        ( SCLKDP + TOL  .GE. DCD( 1 ) ) .AND. */
/*          .        ( SCLKDP - TOL  .LE. DCD( 2 ) ) .AND. */
/*          .        ( DTYPE         .EQ. ICD( 3 ) )      ) THEN */


/*                 CALL CKR03 ( HANDLE, DESCR, SCLKDP, TOL, NEEDAV, */
/*          .                   RECORD, FND ) */

/*                 IF ( FND ) THEN */

/*                    CALL CKE03 (NEEDAV,RECORD,CMAT,AV,CLKOUT) */

/*                    CALL SCDECD ( SC, CLKOUT, SCTIME ) */

/*                    WRITE (*,*) */
/*                    WRITE (*,*) 'Segment identifier: ', IDENT */
/*                    WRITE (*,*) */
/*                    WRITE (*,*) 'Pointing returned for time: ', */
/*          .                      SCTIME */
/*                    WRITE (*,*) */
/*                    WRITE (*,*) 'C-matrix:' */
/*                    WRITE (*,*) */
/*                    WRITE (*,*) ( CMAT(1,I), I = 1, 3 ) */
/*                    WRITE (*,*) ( CMAT(2,I), I = 1, 3 ) */
/*                    WRITE (*,*) ( CMAT(3,I), I = 1, 3 ) */
/*                    WRITE (*,*) */

/*                 END IF */

/*              END IF */

/*              CALL DAFFPA ( SFND ) */

/*           END DO */

/* $ Restrictions */

/*     1)  The file containing the segment should be opened for read */
/*         or write access either by CKLPF, DAFOPR, or DAFOPW. */

/*     2)  The record returned by this routine is intended to be */
/*         evaluated by CKE03. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     J.M. Lynch         (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.2, 12-AUG-2021 (NJB) (JDR) */

/*        Updated code example to use backwards search. Added */
/*        note regarding difference between this search and those */
/*        performed by the CK reader APIs CKGP and CKGPAV. */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.1.1, 22-AUG-2006 (EDW) */

/*        Replaced references to LDPOOL with references */
/*        to FURNSH. */

/* -    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW) */

/*        Replaced DAFRDA call with DAFGDA. */
/*        Added IMPLICIT NONE. */

/* -    SPICELIB Version 1.0.0, 25-NOV-1992 (JML) */

/* -& */
/* $ Index_Entries */

/*     read CK type_3 pointing data record */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */

/*        DIRSIZ     is the directory size. */

/*        BUFSIZ     is the maximum number of double precision numbers */
/*                   that we will read from the DAF file at one time. */
/*                   BUFSIZ is normally set equal to DIRSIZ. */

/*        ND         is the number of double precision components in an */
/*                   unpacked C-kernel segment descriptor. */

/*        NI         is the number of integer components in an unpacked */
/*                   C-kernel segment descriptor. */

/*        QSIZ       is the number of double precision numbers making up */
/*                   the quaternion portion of a pointing record. */

/*        QAVSIZ     is the number of double precision numbers making up */
/*                   the quaternion and angular velocity portion of a */
/*                   pointing record. */

/*        DTYPE      is the data type of the segment that this routine */
/*                   operates on. */



/*     Local variables */


/*     Saved variables. */


/*     Initial values. */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("CKR03", (ftnlen)5);
    }

/*     Start off with FOUND equal to false just in case a SPICELIB error */
/*     is signaled and the return mode is not set to ABORT. */

    *found = FALSE_;

/*     We need to look at a few of the descriptor components. */

/*     The unpacked descriptor contains the following information */
/*     about the segment: */

/*        DCD(1)  Initial encoded SCLK */
/*        DCD(2)  Final encoded SCLK */
/*        ICD(1)  Instrument */
/*        ICD(2)  Inertial reference frame */
/*        ICD(3)  Data type */
/*        ICD(4)  Angular velocity flag */
/*        ICD(5)  Initial address of segment data */
/*        ICD(6)  Final address of segment data */

    dafus_(descr, &c__2, &c__6, dcd, icd);

/*     Check to make sure that the segment is type 3. */

    if (icd[2] != 3) {
	setmsg_("The segment is not a type 3 segment.  Type is #", (ftnlen)47)
		;
	errint_("#", &icd[2], (ftnlen)1);
	sigerr_("SPICE(WRONGDATATYPE)", (ftnlen)20);
	chkout_("CKR03", (ftnlen)5);
	return 0;
    }

/*     Does this segment contain angular velocity? */

    if (icd[3] == 1) {
	psiz = 7;
    } else {
	psiz = 4;
	if (*needav) {
	    setmsg_("Segment does not contain angular velocity data.", (
		    ftnlen)47);
	    sigerr_("SPICE(NOAVDATA)", (ftnlen)15);
	    chkout_("CKR03", (ftnlen)5);
	    return 0;
	}
    }

/*     The beginning and ending addresses of the segment are in the */
/*     descriptor. */

    beg = icd[4];
    end = icd[5];

/*     The procedure used in finding a record to satisfy the request */
/*     for pointing is as follows: */

/*        1) Find the two pointing instances in the segment that bracket */
/*           the request time. */

/*           The pointing instance that brackets the request time on the */
/*           left is defined to be the one associated with the largest */
/*           time in the segment that is less than or equal to SCLKDP. */

/*           The pointing instance that brackets the request time on the */
/*           right is defined to be the one associated with the first */
/*           time in the segment greater than SCLKDP. */

/*           Since the times in the segment are strictly increasing the */
/*           left and right bracketing pointing instances are always */
/*           adjacent. */

/*        2) Determine if the bracketing times are in the same */
/*           interpolation interval. */

/*        3) If they are, then pointing at the request time may be */
/*           linearly interpolated from the bracketing times. */

/*        4) If the times that bracket the request time are not in the */
/*           same interval then, since they are adjacent in the segment */
/*           and since intervals begin and end at actual times, they must */
/*           both be interval endpoints.  Return the pointing instance */
/*           associated with the endpoint closest to the request time, */
/*           provided that it is within the tolerance. */


/*     Get the number of intervals and pointing instances ( records ) */
/*     in this segment, and from that determine the number of respective */
/*     directory epochs. */

    i__1 = end - 1;
    dafgda_(handle, &i__1, &end, buffer);
    numint = i_dnnt(buffer);
    numrec = i_dnnt(&buffer[1]);
    nidir = (numint - 1) / 100;
    nrdir = (numrec - 1) / 100;

/*     Check the FAILED flag just in case HANDLE is not attached to */
/*     any DAF file and the error action is not set to ABORT. You need */
/*     need to do this only after the first call to DAFGDA. */

    if (failed_()) {
	chkout_("CKR03", (ftnlen)5);
	return 0;
    }

/*     To find the times that bracket the request time we will first */
/*     find the greatest directory time less than the request time. */
/*     This will narrow down the search to a group of DIRSIZ or fewer */
/*     times where the Jth group is defined to contain SCLK times */
/*     ((J-1)*DIRSIZ + 1) through (J*DIRSIZ). */

/*     For example if DIRSIZ = 100 then: */

/*                         group   first time #     last time # */
/*                         -----  ---------------   ------------ */
/*                           1            1             100 */
/*                           2          101             200 */
/*                           .            .               . */
/*                           .            .               . */
/*                          10          901            1000 */
/*                           .            .               . */
/*                           .            .               . */
/*                     NRDIR+1     (NRDIR)*100+1     NUMREC */


/*     Thus if the Ith directory time is the largest one less than */
/*     our request time SCLKDP, then we know that: */

/*       SCLKS ( DIRSIZ * I ) <  SCLKDP  <= SCLKS ( DIRSIZ * (I+1) ) */

/*     where SCLKS is taken to be the array of NUMREC times associated */
/*     with the pointing instances. */

/*     Therefore, at least one of the bracketing times will come from */
/*     the (I+1)th group. */


/*     There is only one group if there are no directory epochs. */

    if (nrdir == 0) {
	group = 1;
    } else {

/*        Compute the location of the first directory epoch.  From the */
/*        beginning of the segment, we need to go through all of the */
/*        pointing numbers (PSIZ*NUMREC of them) and then through all of */
/*        the NUMREC SCLK times. */

	dirloc = beg + (psiz + 1) * numrec;

/*        Search through the directory times.  Read in as many as BUFSIZ */
/*        directory epochs at a time for comparison. */

	fnd = FALSE_;
	remain = nrdir;
	group = 0;
	while(! fnd) {

/*           The number of records to read into the buffer. */

	    n = min(remain,100);
	    i__1 = dirloc + n - 1;
	    dafgda_(handle, &dirloc, &i__1, buffer);
	    remain -= n;

/*           Determine the last directory element in BUFFER that's less */
/*           than SCLKDP. */

	    i__ = lstltd_(sclkdp, &n, buffer);
	    if (i__ < n) {
		group = group + i__ + 1;
		fnd = TRUE_;
	    } else if (remain == 0) {

/*              The request time is greater than the last directory time */
/*              so we want the last group in the segment. */

		group = nrdir + 1;
		fnd = TRUE_;
	    } else {

/*              Need to read another block of directory times. */

		dirloc += n;
		group += n;
	    }
	}
    }

/*     Now we know which group of DIRSIZ (or less) times to look at. */
/*     Out of the NUMREC SCLK times, the number that we should skip over */
/*     to get to the proper group is DIRSIZ * ( GROUP - 1 ). */

    skip = (group - 1) * 100;

/*     From this we can compute the address in the segment of the group */
/*     of times we want.  From the beginning, we need to pass through */
/*     PSIZ * NUMREC pointing numbers to get to the first SCLK time. */
/*     Then we skip over the number just computed above. */

    grpadd = beg + numrec * psiz + skip;

/*     The number of times that we have to look at may be less than */
/*     DIRSIZ.  However many there are, go ahead and read them into the */
/*     buffer. */

/* Computing MIN */
    i__1 = 100, i__2 = numrec - skip;
    n = min(i__1,i__2);
    i__1 = grpadd + n - 1;
    dafgda_(handle, &grpadd, &i__1, buffer);

/*     Find the largest time in the group less than or equal to the input */
/*     time. */

    i__ = lstled_(sclkdp, &n, buffer);

/*     Find the pointing instances in the segment that bracket the */
/*     request time and calculate the addresses for the pointing data */
/*     associated with these times. For cases in which the request time */
/*     is equal to one of the times in the segment, that time will be */
/*     the left bracketing time of the returned pair. */

/*     Need to handle the cases when the request time is greater than */
/*     the last or less than the first time in the segment separately. */

    if (i__ == 0) {
	if (group == 1) {

/*           The time occurs before the first time in the segment. Since */
/*           this time cannot possibly be in any of the intervals, the */
/*           first time can satisfy the request for pointing only if it */
/*           is within the tolerance of the request time. */

	    if (buffer[0] - *sclkdp <= *tol) {
		record[0] = buffer[0];
		record[8] = buffer[0];

/*              Calculate the address of the quaternion and angular */
/*              velocity data.  Then read it from the file. */

		i__1 = beg + psiz - 1;
		dafgda_(handle, &beg, &i__1, buffer);
		moved_(buffer, &psiz, &record[1]);
		moved_(buffer, &psiz, &record[9]);
		record[16] = *sclkdp;
		*found = TRUE_;
	    }
	    chkout_("CKR03", (ftnlen)5);
	    return 0;
	} else {

/*           The first time in the current group brackets the request */
/*           time on the right and the last time from the preceding */
/*           group brackets on the left. */

	    rsclk = buffer[0];
	    raddr = beg + skip * psiz;
	    i__1 = grpadd - 1;
	    i__2 = grpadd - 1;
	    dafgda_(handle, &i__1, &i__2, &lsclk);
	    laddr = raddr - psiz;
	}
    } else if (i__ == n) {

/*        There are two possible cases, but the same action can handle */
/*        both. */

/*        1) If this is the last group ( NRDIR + 1 ) then the request */
/*           time occurs on or after the last time in the segment. */
/*           In either case this last time can satisfy the request for */
/*           pointing only if it is within the tolerance of the request */
/*           time. */

/*        2) The request time is greater than or equal to the last time */
/*           in this group. Since this time is the same as the (I+1)th */
/*           directory time, and since the search on the directory times */
/*           used a strictly less than test, we know that the request */
/*           time must be equal to this time.  Just return the pointing */
/*           instance associated with the request time.  ( Note that */
/*           SCLKDP - BUFFER(N) will be zero in this case. ) */

	if (*sclkdp - buffer[(i__1 = n - 1) < 100 && 0 <= i__1 ? i__1 : 
		s_rnge("buffer", i__1, "ckr03_", (ftnlen)841)] <= *tol) {
	    record[0] = buffer[(i__1 = n - 1) < 100 && 0 <= i__1 ? i__1 : 
		    s_rnge("buffer", i__1, "ckr03_", (ftnlen)843)];
	    record[8] = buffer[(i__1 = n - 1) < 100 && 0 <= i__1 ? i__1 : 
		    s_rnge("buffer", i__1, "ckr03_", (ftnlen)844)];

/*           Calculate the address of the quaternion and angular */
/*           velocity data.  Then read it from the file. */

	    addr__ = beg + psiz * (skip + n - 1);
	    i__1 = addr__ + psiz - 1;
	    dafgda_(handle, &addr__, &i__1, buffer);
	    moved_(buffer, &psiz, &record[1]);
	    moved_(buffer, &psiz, &record[9]);
	    record[16] = *sclkdp;
	    *found = TRUE_;
	}
	chkout_("CKR03", (ftnlen)5);
	return 0;
    } else {

/*        The bracketing times are contained in this group. */

	lsclk = buffer[(i__1 = i__ - 1) < 100 && 0 <= i__1 ? i__1 : s_rnge(
		"buffer", i__1, "ckr03_", (ftnlen)870)];
	rsclk = buffer[(i__1 = i__) < 100 && 0 <= i__1 ? i__1 : s_rnge("buff"
		"er", i__1, "ckr03_", (ftnlen)871)];
	laddr = beg + (skip + i__ - 1) * psiz;
	raddr = laddr + psiz;
    }

/*     At this point we have the two times in the segment that bracket */
/*     the request time.  We also have the addresses of the pointing */
/*     data associated with those times. The task now is to determine */
/*     if the bracketing times fall in the same interval.  If so then */
/*     we can interpolate between them.  If they don't then return */
/*     pointing for whichever of the two times is closest to the */
/*     request time, provided that it is within the tolerance. */


/*     Find the interpolation interval that the request time is in and */
/*     determine if the bracketing SCLK's are both in it. */

/*     First check if the request time falls in the same interval as */
/*     it did last time.  We need to make sure that we are dealing */
/*     with the same segment as well as the same time range. */


/*     PREVS      is the start time of the interval that satisfied */
/*                the previous request for pointing. */

/*     PREVN      is the start time of the interval that followed */
/*                the interval specified above. */

/*     LHAND      is the handle of the file that PREVS and PREVN */
/*                were found in. */

/*     LBEG,      are the beginning and ending addresses of the */
/*     LEND       segment in the file LHAND that PREVS and PREVN */
/*                were found in. */

    if (*handle == lhand && beg == lbeg && end == lend && *sclkdp >= prevs && 
	    *sclkdp < prevn) {
	start = prevs;
	nstart = prevn;
    } else {

/*        The START times of all of the intervals are stored in the */
/*        segment and a directory of every hundredth START is also */
/*        stored. The procedure to find the bracketing interval start */
/*        times is identical to the one used above for finding the */
/*        bracketing times. */

/*        The directory epochs narrow down the search for the times that */
/*        bracket the request time to a group of DIRSIZ or fewer records. */


/*        There is only one group if there are no directory epochs. */

	if (nidir == 0) {
	    group = 1;
	} else {

/*           Compute the location of the first directory epoch.  From the */
/*           beginning of the segment, we need to go through all of the */
/*           pointing numbers (PSIZ*NUMREC of them), then through all of */
/*           the NUMREC SCLK times and NRDIR directory times, and then */
/*           finally through the NUMINT interval start times. */

	    dirloc = beg + (psiz + 1) * numrec + nrdir + numint;

/*           Locate the largest directory time less than the */
/*           request time SCLKDP. */

/*           Read in as many as BUFSIZ directory epochs at a time for */
/*           comparison. */

	    fnd = FALSE_;
	    remain = nidir;
	    group = 0;
	    while(! fnd) {

/*              The number of records to read into the buffer. */

		n = min(remain,100);
		i__1 = dirloc + n - 1;
		dafgda_(handle, &dirloc, &i__1, buffer);
		remain -= n;

/*              Determine the last directory element in BUFFER that's */
/*              less than SCLKDP. */

		i__ = lstltd_(sclkdp, &n, buffer);
		if (i__ < n) {
		    group = group + i__ + 1;
		    fnd = TRUE_;
		} else if (remain == 0) {

/*                 The request time is greater than the last directory */
/*                 time so we want the last group in the segment. */

		    group = nidir + 1;
		    fnd = TRUE_;
		} else {

/*                 Need to read another block of directory times. */

		    dirloc += n;
		    group += n;
		}
	    }
	}

/*        Now we know which group of DIRSIZ (or less) times to look at. */
/*        Out of the NUMINT SCLK START times, the number that we should */
/*        skip over to get to the proper group is DIRSIZ * ( GROUP - 1 ). */

	skip = (group - 1) * 100;

/*        From this we can compute the address in the segment of the */
/*        group of times we want.  To get to the first interval start */
/*        time we must pass over PSIZ * NUMREC pointing numbers, NUMREC */
/*        SCLK times, and NRDIR SCLK directory times.  Then we skip */
/*        over the number just computed above. */

	grpadd = beg + (psiz + 1) * numrec + nrdir + skip;

/*        The number of times that we have to look at may be less than */
/*        DIRSIZ.  However many there are, go ahead and read them into */
/*        the buffer. */

/* Computing MIN */
	i__1 = 100, i__2 = numint - skip;
	n = min(i__1,i__2);
	i__1 = grpadd + n - 1;
	dafgda_(handle, &grpadd, &i__1, buffer);

/*        Find the index of the largest time in the group that is less */
/*        than or equal to the input time. */

	i__ = lstled_(sclkdp, &n, buffer);
	if (i__ == 0) {

/*           The first start time in the buffer is the start of the */
/*           interval following the one containing the request time. */

/*           We don't need to check if GROUP = 1 because the case of */
/*           the request time occurring before the first time in the */
/*           segment has already been handled. */

	    nstart = buffer[0];
	    addr__ = grpadd - 1;
	    dafgda_(handle, &addr__, &addr__, &start);
	} else if (i__ == n) {
	    if (group == nidir + 1) {

/*              This is the last interval in the segment. */

		start = buffer[(i__1 = n - 1) < 100 && 0 <= i__1 ? i__1 : 
			s_rnge("buffer", i__1, "ckr03_", (ftnlen)1055)];
		nstart = dpmax_();
	    } else {

/*              The last START time in this group is equal to the */
/*              request time. */

		start = buffer[(i__1 = n - 1) < 100 && 0 <= i__1 ? i__1 : 
			s_rnge("buffer", i__1, "ckr03_", (ftnlen)1064)];
		addr__ = grpadd + n;
		dafgda_(handle, &addr__, &addr__, &nstart);
	    }
	} else {

/*           The bracketing START times are contained in this group. */

	    start = buffer[(i__1 = i__ - 1) < 100 && 0 <= i__1 ? i__1 : 
		    s_rnge("buffer", i__1, "ckr03_", (ftnlen)1076)];
	    nstart = buffer[(i__1 = i__) < 100 && 0 <= i__1 ? i__1 : s_rnge(
		    "buffer", i__1, "ckr03_", (ftnlen)1077)];
	}

/*        Save the information about the interval and segment. */

	lhand = *handle;
	lbeg = beg;
	lend = end;
	prevs = start;
	prevn = nstart;
    }

/*     Check and see if the bracketing pointing instances belong */
/*     to the same interval.  If they do then we can interpolate */
/*     between them, if not then check to see if the closer of */
/*     the two to the request time lies within the tolerance. */

/*     The left bracketing time will always belong to the same */
/*     interval as the request time, therefore we need to check */
/*     only that the right bracketing time is less than the start */
/*     time of the next interval. */

    if (rsclk < nstart) {
	record[0] = lsclk;
	i__1 = laddr + psiz - 1;
	dafgda_(handle, &laddr, &i__1, &record[1]);
	record[8] = rsclk;
	i__1 = raddr + psiz - 1;
	dafgda_(handle, &raddr, &i__1, &record[9]);
	record[16] = *sclkdp;
	*found = TRUE_;
    } else {
	ldiff = *sclkdp - lsclk;
	rdiff = rsclk - *sclkdp;
	if (ldiff <= *tol || rdiff <= *tol) {

/*           Return the pointing instance closest to the request time. */

/*           If the request time is midway between LSCLK and RSCLK then */
/*           grab the pointing instance associated with the greater time. */

	    if (ldiff < rdiff) {
		record[0] = lsclk;
		record[8] = lsclk;
		i__1 = laddr + psiz - 1;
		dafgda_(handle, &laddr, &i__1, buffer);
		moved_(buffer, &psiz, &record[1]);
		moved_(buffer, &psiz, &record[9]);
	    } else {
		record[0] = rsclk;
		record[8] = rsclk;
		i__1 = raddr + psiz - 1;
		dafgda_(handle, &raddr, &i__1, buffer);
		moved_(buffer, &psiz, &record[1]);
		moved_(buffer, &psiz, &record[9]);
	    }
	    record[16] = *sclkdp;
	    *found = TRUE_;
	}
    }
    chkout_("CKR03", (ftnlen)5);
    return 0;
} /* ckr03_ */

