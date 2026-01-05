/* cknr02.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__6 = 6;

/* $Procedure CKNR02 ( C-kernel, number of records, type 02 ) */
/* Subroutine */ int cknr02_(integer *handle, doublereal *descr, integer *
	nrec)
{
    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */
    integer i_dnnt(doublereal *);

    /* Local variables */
    extern /* Subroutine */ int chkin_(char *, ftnlen), dafus_(doublereal *, 
	    integer *, integer *, doublereal *, integer *), sigerr_(char *, 
	    ftnlen), chkout_(char *, ftnlen), setmsg_(char *, ftnlen), 
	    errint_(char *, integer *, ftnlen);
    integer arrsiz;
    extern logical return_(void);
    doublereal dcd[2];
    integer beg, icd[6], end;

/* $ Abstract */

/*     Return the number of pointing records in a CK type 02 segment. */
/*     The segment is identified by a CK file handle and segment */
/*     descriptor. */

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
/*     HANDLE     I   The handle of the CK file containing the segment. */
/*     DESCR      I   The descriptor of the type 2 segment. */
/*     NREC       O   The number of records in the segment. */

/* $ Detailed_Input */

/*     HANDLE   is the handle of the binary CK file containing the */
/*              segment. The file should have been opened for read or */
/*              write access, by CKLPF, DAFOPR or DAFOPW. */

/*     DESCR    is the packed descriptor of a data type 2 CK segment. */

/* $ Detailed_Output */

/*     NREC     is the number of pointing records in the type 2 segment */
/*              associated with HANDLE and DESCR. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the segment indicated by DESCR is not a type 2 segment, */
/*         the error SPICE(CKWRONGDATATYPE) is signaled. */

/*     2)  If the specified handle does not belong to any file that is */
/*         currently known to be open, an error is signaled by a routine */
/*         in the call tree of this routine. */

/*     3)  If DESCR is not a valid descriptor of a segment in the CK */
/*         file specified by HANDLE, the results of this routine are */
/*         unpredictable. */

/* $ Files */

/*     The CK file specified by HANDLE should be open for read or write */
/*     access. */

/* $ Particulars */

/*     For a complete description of the internal structure of a type 2 */
/*     segment, see the CK required reading. */

/*     This routine returns the number of pointing records contained */
/*     in the specified segment. It is normally used in conjunction */
/*     with CKGR02, which returns the Ith record in the segment. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) The following code example extracts the start and end SCLK */
/*        time, seconds per tick rate, platform's +Z axis direction, */
/*        and angular velocity vector for each pointing instance in */
/*        the first segment in a CK file that contains segments of data */
/*        type 2. */

/*        Use the CK kernel below, available in the Viking Orbiter PDS */
/*        archives, as input for the code example. */

/*           vo2_swu_ck2.bc */

/*        Example code begins here. */


/*              PROGRAM CKNR02_EX1 */
/*              IMPLICIT NONE */

/*              DOUBLE PRECISION      AV      ( 3 ) */
/*              DOUBLE PRECISION      CMAT    ( 3, 3 ) */
/*              DOUBLE PRECISION      DCD     ( 2  ) */
/*              DOUBLE PRECISION      DESCR   ( 5  ) */
/*              DOUBLE PRECISION      QUAT    ( 4 ) */
/*              DOUBLE PRECISION      RECORD  ( 10 ) */
/*              DOUBLE PRECISION      SCLKE */
/*              DOUBLE PRECISION      SCLKR */
/*              DOUBLE PRECISION      SCLKS */
/*              DOUBLE PRECISION      Z       ( 3 ) */

/*              INTEGER               ICD     ( 6 ) */
/*              INTEGER               HANDLE */
/*              INTEGER               NREC */
/*              INTEGER               I */

/*              LOGICAL               FOUND */

/*        C */
/*        C     First load the file. (The file may also be opened by */
/*        C     using CKLPF). */
/*        C */
/*              CALL DAFOPR ( 'vo2_swu_ck2.bc', HANDLE ) */

/*        C */
/*        C     Begin forward search.  Find the first array. */
/*        C */
/*              CALL DAFBFS ( HANDLE ) */
/*              CALL DAFFNA ( FOUND  ) */

/*        C */
/*        C     Get segment descriptor. */
/*        C */
/*              CALL DAFGS ( DESCR ) */

/*        C */
/*        C     Unpack the segment descriptor into its double precision */
/*        C     and integer components. */
/*        C */
/*              CALL DAFUS ( DESCR, 2, 6, DCD, ICD ) */

/*        C */
/*        C     The data type for a segment is located in the third */
/*        C     integer component of the descriptor. */
/*        C */
/*              IF ( ICD( 3 ) .EQ. 2 ) THEN */

/*        C */
/*        C        How many records does this segment contain? */
/*        C */
/*                 CALL CKNR02 ( HANDLE, DESCR, NREC ) */

/*                 DO I = 1, NREC */

/*        C */
/*        C           Get the Ith record in the segment. */
/*        C */
/*                    CALL CKGR02 ( HANDLE, DESCR, I, RECORD ) */

/*        C */
/*        C           Unpack RECORD into the start and end time, rate in */
/*        C           TDB seconds/tick, quaternion, and av. */
/*        C */
/*                    SCLKS = RECORD ( 1 ) */
/*                    SCLKE = RECORD ( 2 ) */
/*                    SCLKR = RECORD ( 3 ) */

/*                    CALL MOVED ( RECORD(4), 4, QUAT ) */
/*                    CALL MOVED ( RECORD(8), 3, AV   ) */

/*        C */
/*        C           The +Z axis direction is the third row of the */
/*        C           C-matrix. */
/*        C */
/*                    CALL Q2M ( QUAT, CMAT ) */

/*                    Z(1) = CMAT(3,1) */
/*                    Z(2) = CMAT(3,2) */
/*                    Z(3) = CMAT(3,3) */

/*        C */
/*        C           Write out the results. */
/*        C */
/*                    WRITE (*,'(A,I2)') 'Record: ', I */
/*                    WRITE (*,'(A,F21.6)') '   Start encoded SCLK:', */
/*             .                              SCLKS */
/*                    WRITE (*,'(A,F21.6)') '   End encoded SCLK  :', */
/*             .                              SCLKE */
/*                    WRITE (*,'(A,F21.6)') '   TDB Seconds/tick  :', */
/*             .                              SCLKR */
/*                    WRITE (*,'(A,3F13.8)') '   +Z axis           :', */
/*             .                              Z */
/*                    WRITE (*,'(A,3F13.8)') '   Angular velocity  :', */
/*             .                              AV */
/*                    WRITE (*,*) */

/*                 END DO */

/*              END IF */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        Record:  1 */
/*           Start encoded SCLK:   32380393707.000015 */
/*           End encoded SCLK  :   32380395707.000015 */
/*           TDB Seconds/tick  :             0.001000 */
/*           +Z axis           :   0.62277152  -0.29420673   0.72498141 */
/*           Angular velocity  :   0.00000000   0.00000000   0.00000000 */

/*        Record:  2 */
/*           Start encoded SCLK:   32380402605.999947 */
/*           End encoded SCLK  :   32380404605.999947 */
/*           TDB Seconds/tick  :             0.001000 */
/*           +Z axis           :   0.62172600  -0.27894910   0.73187716 */
/*           Angular velocity  :   0.00000000   0.00000000   0.00000000 */

/*        Record:  3 */
/*           Start encoded SCLK:   32380412542.000053 */
/*           End encoded SCLK  :   32380414542.000053 */
/*           TDB Seconds/tick  :             0.001000 */
/*           +Z axis           :   0.62183610  -0.26307233   0.73764003 */
/*           Angular velocity  :   0.00000000   0.00000000   0.00000000 */

/*        Record:  4 */
/*           Start encoded SCLK:   32827264875.000000 */
/*           End encoded SCLK  :   32827266875.000000 */
/*           TDB Seconds/tick  :             0.001000 */
/*           +Z axis           :   0.82984105  -0.44796078   0.33270853 */
/*           Angular velocity  :   0.00000000   0.00000000   0.00000000 */

/*        Record:  5 */
/*           Start encoded SCLK:   32827403805.999992 */
/*           End encoded SCLK  :   32827405805.999992 */
/*           TDB Seconds/tick  :             0.001000 */
/*           +Z axis           :   0.80812500  -0.44911178   0.38109395 */
/*           Angular velocity  :   0.00000000   0.00000000   0.00000000 */

/*        Record:  6 */
/*           Start encoded SCLK:   32827412705.000042 */
/*           End encoded SCLK  :   32827414705.000042 */
/*           TDB Seconds/tick  :             0.001000 */
/*           +Z axis           :   0.81120505  -0.43593555   0.38975193 */
/*           Angular velocity  :   0.00000000   0.00000000   0.00000000 */

/*        Record:  7 */
/*           Start encoded SCLK:   32827417284.000038 */
/*           End encoded SCLK  :   32827419284.000038 */
/*           TDB Seconds/tick  :             0.001000 */
/*           +Z axis           :   0.81313834  -0.42861613   0.39382008 */
/*           Angular velocity  :   0.00000000   0.00000000   0.00000000 */

/*        Record:  8 */
/*           Start encoded SCLK:   33793314593.000053 */
/*           End encoded SCLK  :   33793316593.000053 */
/*           TDB Seconds/tick  :             0.001000 */
/*           +Z axis           :   0.79860617  -0.37840751   0.46801275 */
/*           Angular velocity  :   0.00000000   0.00000000   0.00000000 */

/*        Record:  9 */
/*           Start encoded SCLK:   33793332478.000046 */
/*           End encoded SCLK  :   33793334478.000046 */
/*           TDB Seconds/tick  :             0.001000 */
/*           +Z axis           :   0.77861783  -0.39171670   0.49021658 */
/*           Angular velocity  :   0.00000000   0.00000000   0.00000000 */

/*        Record: 10 */
/*           Start encoded SCLK:   33793341463.000061 */
/*           End encoded SCLK  :   33793343463.000061 */
/*           TDB Seconds/tick  :             0.001000 */
/*           +Z axis           :   0.76852381  -0.39797437   0.50098659 */
/*           Angular velocity  :   0.00000000   0.00000000   0.00000000 */

/*        Record: 11 */
/*           Start encoded SCLK:   33793350363.000034 */
/*           End encoded SCLK  :   33793352363.000034 */
/*           TDB Seconds/tick  :             0.001000 */
/*           +Z axis           :   0.75779934  -0.40484364   0.51170478 */
/*           Angular velocity  :   0.00000000   0.00000000   0.00000000 */

/*        Record: 12 */
/*           Start encoded SCLK:   33984028250.000000 */
/*           End encoded SCLK  :   33984030250.000000 */
/*           TDB Seconds/tick  :             0.001000 */
/*           +Z axis           :   0.77099184  -0.39926339   0.49614546 */
/*           Angular velocity  :   0.00000000   0.00000000   0.00000000 */

/*        Record: 13 */
/*           Start encoded SCLK:   33984046134.999992 */
/*           End encoded SCLK  :   33984048134.999992 */
/*           TDB Seconds/tick  :             0.001000 */
/*           +Z axis           :   0.75024440  -0.41218993   0.51694564 */
/*           Angular velocity  :   0.00000000   0.00000000   0.00000000 */

/*        Record: 14 */
/*           Start encoded SCLK:   33984055121.000053 */
/*           End encoded SCLK  :   33984057121.000053 */
/*           TDB Seconds/tick  :             0.001000 */
/*           +Z axis           :   0.73947359  -0.41886863   0.52699894 */
/*           Angular velocity  :   0.00000000   0.00000000   0.00000000 */

/*        Record: 15 */
/*           Start encoded SCLK:   33984220835.999966 */

/*        [...] */


/*        Warning: incomplete output. Only 100 out of 875 lines have been */
/*        provided. */


/* $ Restrictions */

/*     1)  The binary CK file containing the segment whose descriptor was */
/*         passed to this routine must be opened for read or write access */
/*         by CKLPF, DAFOPR or DAFOPW. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     J.M. Lynch         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR) */

/*        Added IMPLICIT NONE statement. */

/*        Edited the header to comply with NAIF standard. Created */
/*        complete code example from existing code fragment. */

/*        Improved text in the $Abstract section. */

/* -    SPICELIB Version 1.0.0, 25-NOV-1992 (JML) */

/* -& */
/* $ Index_Entries */

/*     number of CK type_2 records */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */

/*        NDC        is the number of double precision components in an */
/*                   unpacked C-kernel descriptor. */

/*        NIC        is the number of integer components in an unpacked */
/*                   C-kernel descriptor. */

/*        DTYPE      is the data type. */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("CKNR02", (ftnlen)6);
    }

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

/*     If this segment is not of data type 2, then signal an error. */

    if (icd[2] != 2) {
	setmsg_("Data type of the segment should be 2: Passed descriptor sho"
		"ws type = #.", (ftnlen)71);
	errint_("#", &icd[2], (ftnlen)1);
	sigerr_("SPICE(CKWRONGDATATYPE)", (ftnlen)22);
	chkout_("CKNR02", (ftnlen)6);
	return 0;
    }

/*     The beginning and ending addresses of the segment are in the */
/*     descriptor. */

    beg = icd[4];
    end = icd[5];

/*     Calculate the number of pointing records in the segment from */
/*     the physical size of the segment and knowledge of its structure. */

/*        Based on the structure of a type 2 segment, the size of a */
/*        segment with N pointing intervals is given as follows: */

/*           ARRSIZ  =  PSIZ * N  +  2 * N  +  ( N-1 ) / 100       (1) */

/*        In the above equation PSIZ is eight and integer arithmetic is */
/*        used.  This equation is equivalent to: */


/*           100 * ARRSIZ  =  1000 * N  + ( N-1 ) * 100            (2) */
/*                                        ------- */
/*                                          100 */

/*        If we can eliminate the integer division then, since all of */
/*        the other values represent whole numbers, we can solve the */
/*        equation for N in terms of ARRSIZ by using double precision */
/*        arithmetic and then rounding the result to the nearest integer. */

/*        This next equation uses double precision arithmetic and is */
/*        equivalent to (2): */

/*           100 * ARRSIZ  = 1000 * N + ( N-1 ) - ( N-1 ) MOD 100  (3) */

/*        Which means: */

/*           100 * ARRSIZ + 1     ( N-1 ) MOD 100 */
/*           ----------------  +  ---------------   =   N          (4) */
/*                1001                 1001 */

/*         Since the second term on the left side of (4) is always less */
/*         than 0.1, the first term will always round to the correct */
/*         value of N. */

    arrsiz = end - beg + 1;
    d__1 = ((doublereal) arrsiz * 100. + 1.) / 1001.;
    *nrec = i_dnnt(&d__1);
    chkout_("CKNR02", (ftnlen)6);
    return 0;
} /* cknr02_ */

