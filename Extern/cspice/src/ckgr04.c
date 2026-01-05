/* ckgr04.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__6 = 6;
static integer c__7 = 7;
static doublereal c_b15 = 128.;

/* $Procedure CKGR04 ( C-kernel, get record, type 04 ) */
/* Subroutine */ int ckgr04_(integer *handle, doublereal *descr, integer *
	recno, doublereal *record)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer nrec, ends[1], k;
    extern /* Subroutine */ int chkin_(char *, ftnlen), cknr04_(integer *, 
	    doublereal *, integer *), dafus_(doublereal *, integer *, integer 
	    *, doublereal *, integer *);
    integer numall;
    extern /* Subroutine */ int sigerr_(char *, ftnlen);
    integer numcft[7];
    extern /* Subroutine */ int chkout_(char *, ftnlen), sgfpkt_(integer *, 
	    doublereal *, integer *, integer *, doublereal *, integer *), 
	    setmsg_(char *, ftnlen), errint_(char *, integer *, ftnlen);
    extern logical return_(void);
    doublereal dcd[2];
    integer icd[6];
    extern /* Subroutine */ int zzck4d2i_(doublereal *, integer *, doublereal 
	    *, integer *);

/* $ Abstract */

/*     Return a specified pointing record from a CK type 04 segment. */
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
/* $ Abstract */

/*     Declarations of the CK data type specific and general CK low */
/*     level routine parameters. */

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

/*     CK.REQ */

/* $ Keywords */

/*     CK */

/* $ Restrictions */

/*     1) If new CK types are added, the size of the record passed */
/*        between CKRxx and CKExx must be registered as separate */
/*        parameter. If this size will be greater than current value */
/*        of the CKMRSZ parameter (which specifies the maximum record */
/*        size for the record buffer used inside CKPFS) then it should */
/*        be assigned to CKMRSZ as a new value. */

/* $ Author_and_Institution */

/*     N.J. Bachman      (JPL) */
/*     B.V. Semenov      (JPL) */

/* $ Literature_References */

/*     CK Required Reading. */

/* $ Version */

/* -    SPICELIB Version 3.0.0, 27-JAN-2014 (NJB) */

/*        Updated to support CK type 6. Maximum degree for */
/*        type 5 was updated to be consistent with the */
/*        maximum degree for type 6. */

/* -    SPICELIB Version 2.0.0, 19-AUG-2002 (NJB) */

/*        Updated to support CK type 5. */

/* -    SPICELIB Version 1.0.0, 05-APR-1999 (BVS) */

/* -& */

/*     Number of quaternion components and number of quaternion and */
/*     angular rate components together. */


/*     CK Type 1 parameters: */

/*     CK1DTP   CK data type 1 ID; */

/*     CK1RSZ   maximum size of a record passed between CKR01 */
/*              and CKE01. */


/*     CK Type 2 parameters: */

/*     CK2DTP   CK data type 2 ID; */

/*     CK2RSZ   maximum size of a record passed between CKR02 */
/*              and CKE02. */


/*     CK Type 3 parameters: */

/*     CK3DTP   CK data type 3 ID; */

/*     CK3RSZ   maximum size of a record passed between CKR03 */
/*              and CKE03. */


/*     CK Type 4 parameters: */

/*     CK4DTP   CK data type 4 ID; */

/*     CK4PCD   parameter defining integer to DP packing schema that */
/*              is applied when seven number integer array containing */
/*              polynomial degrees for quaternion and angular rate */
/*              components packed into a single DP number stored in */
/*              actual CK records in a file; the value of must not be */
/*              changed or compatibility with existing type 4 CK files */
/*              will be lost. */

/*     CK4MXD   maximum Chebychev polynomial degree allowed in type 4 */
/*              records; the value of this parameter must never exceed */
/*              value of the CK4PCD; */

/*     CK4SFT   number of additional DPs, which are not polynomial */
/*              coefficients, located at the beginning of a type 4 */
/*              CK record that passed between routines CKR04 and CKE04; */

/*     CK4RSZ   maximum size of type 4 CK record passed between CKR04 */
/*              and CKE04; CK4RSZ is computed as follows: */

/*                 CK4RSZ = ( CK4MXD + 1 ) * QAVSIZ + CK4SFT */


/*     CK Type 5 parameters: */


/*     CK5DTP   CK data type 5 ID; */

/*     CK5MXD   maximum polynomial degree allowed in type 5 */
/*              records. */

/*     CK5MET   number of additional DPs, which are not polynomial */
/*              coefficients, located at the beginning of a type 5 */
/*              CK record that passed between routines CKR05 and CKE05; */

/*     CK5MXP   maximum packet size for any subtype.  Subtype 2 */
/*              has the greatest packet size, since these packets */
/*              contain a quaternion, its derivative, an angular */
/*              velocity vector, and its derivative.  See ck05.inc */
/*              for a description of the subtypes. */

/*     CK5RSZ   maximum size of type 5 CK record passed between CKR05 */
/*              and CKE05; CK5RSZ is computed as follows: */

/*                 CK5RSZ = ( CK5MXD + 1 ) * CK5MXP + CK5MET */


/*     CK Type 6 parameters: */


/*     CK6DTP   CK data type 6 ID; */

/*     CK6MXD   maximum polynomial degree allowed in type 6 */
/*              records. */

/*     CK6MET   number of additional DPs, which are not polynomial */
/*              coefficients, located at the beginning of a type 6 */
/*              CK record that passed between routines CKR06 and CKE06; */

/*     CK6MXP   maximum packet size for any subtype.  Subtype 2 */
/*              has the greatest packet size, since these packets */
/*              contain a quaternion, its derivative, an angular */
/*              velocity vector, and its derivative.  See ck06.inc */
/*              for a description of the subtypes. */

/*     CK6RSZ   maximum size of type 6 CK record passed between CKR06 */
/*              and CKE06; CK6RSZ is computed as follows: */

/*                 CK6RSZ = CK6MET + ( CK6MXD + 1 ) * ( CK6PS3 + 1 ) */

/*              where CK6PS3 is equal to the parameter CK06PS3 defined */
/*              in ck06.inc. Note that the subtype having the largest */
/*              packet size (subtype 2) does not give rise to the */
/*              largest record size, because that type is Hermite and */
/*              requires half the window size used by subtype 3 for a */
/*              given polynomial degree. */


/*     The parameter CK6PS3 must be in sync with C06PS3 defined in */
/*     ck06.inc. */



/*     Maximum record size that can be handled by CKPFS. This value */
/*     must be set to the maximum of all CKxRSZ parameters (currently */
/*     CK5RSZ.) */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   The handle of the file containing the segment. */
/*     DESCR      I   The segment descriptor. */
/*     RECNO      I   The number of the pointing record to be returned. */
/*     RECORD     O   The pointing record. */

/* $ Detailed_Input */

/*     HANDLE   is the handle of the binary CK file containing the */
/*              desired segment. The file should have been opened */
/*              for read or write access, either by CKLPF, DAFOPR, */
/*              or DAFOPW. */

/*     DESCR    is the packed descriptor of the data type 4 segment. */

/*     RECNO    is the number of the pointing record to be returned */
/*              from the data type 4 segment. */

/* $ Detailed_Output */

/*     RECORD   is the pointing record indexed by RECNO in the */
/*              segment. The contents of the record are as follows: */

/*              --------------------------------------------------- */
/*              | The midpoint of the approximation interval      | */
/*              --------------------------------------------------- */
/*              | The radius of the approximation interval        | */
/*              --------------------------------------------------- */
/*              | Number of coefficients for q0                   | */
/*              --------------------------------------------------- */
/*              | Number of coefficients for q1                   | */
/*              --------------------------------------------------- */
/*              | Number of coefficients for q2                   | */
/*              --------------------------------------------------- */
/*              | Number of coefficients for q3                   | */
/*              --------------------------------------------------- */
/*              | Number of coefficients for AV1                  | */
/*              --------------------------------------------------- */
/*              | Number of coefficients for AV2                  | */
/*              --------------------------------------------------- */
/*              | Number of coefficients for AV3                  | */
/*              --------------------------------------------------- */
/*              | q0 Cheby coefficients                           | */
/*              --------------------------------------------------- */
/*              | q1 Cheby coefficients                           | */
/*              --------------------------------------------------- */
/*              | q2 Cheby coefficients                           | */
/*              --------------------------------------------------- */
/*              | q3 Cheby coefficients                           | */
/*              --------------------------------------------------- */
/*              | AV1 Cheby coefficients (optional)               | */
/*              --------------------------------------------------- */
/*              | AV2 Cheby coefficients (optional)               | */
/*              --------------------------------------------------- */
/*              | AV3 Cheby coefficients (optional)               | */
/*              --------------------------------------------------- */

/* $ Parameters */

/*     See 'ckparam.inc'. */

/* $ Exceptions */

/*     1)  If the segment is not of data type 4, the error */
/*         SPICE(CKWRONGDATATYPE) is signaled. */

/*     2)  If RECNO is less than one or greater than the number of */
/*         records in the specified segment, the error */
/*         SPICE(CKNONEXISTREC) is signaled. */

/*     3)  If the specified handle does not belong to any DAF file that */
/*         is currently known to be open, an error is signaled by a */
/*         routine in the call tree of this routine. */

/*     4)  If DESCR is not a valid descriptor of a segment in the CK */
/*         file specified by HANDLE, the results of this routine are */
/*         unpredictable. */

/* $ Files */

/*     The file specified by HANDLE should be open for read or */
/*     write access. */

/* $ Particulars */

/*     For a detailed description of the structure of a type 4 segment, */
/*     see the CK required reading. */

/*     This is a utility routine that may be used to read the individual */
/*     pointing records that make up a type 4 segment. It is normally */
/*     used in conjunction with CKNR04, which gives the number of */
/*     pointing records stored in a segment. */

/* $ Examples */

/*     Suppose that DATA.BC is a CK file that contains segments of */
/*     data type 4. Then the following code fragment extracts the */
/*     data packets contained in the segment. */

/*     C */
/*     C     CK parameters include file. */
/*     C */
/*           INCLUDE               'ckparam.inc' */

/*     C */
/*     C     Local variables. */
/*     C */
/*           DOUBLE PRECISION      DCD    ( 2 ) */
/*           DOUBLE PRECISION      DESCR  ( 5 ) */
/*           DOUBLE PRECISION      PKTDAT ( CK4RSZ ) */

/*           INTEGER               AVFLAG */
/*           INTEGER               HANDLE */
/*           INTEGER               I */
/*           INTEGER               ICD    ( 6 ) */
/*           INTEGER               K */
/*           INTEGER               LASTAD */
/*           INTEGER               NCOEF  ( QAVSIZ ) */
/*           INTEGER               NREC */

/*           LOGICAL               FOUND */
/*     C */
/*     C     First load the file. (The file may also be opened by using */
/*     C     CKLPF.) */
/*     C */
/*           CALL DAFOPR ( 'DATA.BC', HANDLE ) */
/*     C */
/*     C     Begin forward search. Find the first array. */
/*     C */
/*           CALL DAFBFS ( HANDLE ) */
/*           CALL DAFFNA ( FOUND  ) */
/*     C */
/*     C     Get segment descriptor. */
/*     C */
/*           CALL DAFGS ( DESCR ) */
/*     C */
/*     C     Unpack the segment descriptor into its double precision */
/*     C     and integer components. */
/*     C */
/*           CALL DAFUS ( DESCR, 2, 6, DCD, ICD ) */

/*           IF ( ICD( 3 ) .EQ. 4 ) THEN */
/*     C */
/*     C        How many records does this segment contain? */
/*     C */
/*              CALL CKNR04 ( HANDLE, DESCR, NREC ) */

/*              DO I = 1, NREC */
/*     C */
/*     C           Get the data records stored in the segment. */
/*     C */
/*                 CALL CKGR04 ( HANDLE, DESCR, I, PKTDAT ) */
/*     C */
/*     C           Print data packet contents. Print coverage interval */
/*     C           midpoint & radii first. */
/*     C */
/*                 WRITE (2,*) PKTDAT (1) */
/*                 WRITE (2,*) PKTDAT (2) */
/*     C */
/*     C           Decode numbers of coefficients. */
/*     C */
/*                 CALL ZZCK4D2I ( PKTDAT(3), QAVSIZ, CK4PCD, NCOEF ) */
/*     C */
/*     C           Print number of coefficients for Q0, Q1, Q2 and Q3. */
/*     C */
/*                 WRITE (2,FMT='(I2,6X,I2)') NCOEF( 1 ), NCOEF( 2 ) */
/*                 WRITE (2,FMT='(I2,6X,I2)') NCOEF( 3 ), NCOEF( 4 ) */
/*     C */
/*     C           Print number coefficients for AV1, AV2 and AV3. */
/*     C */
/*                 WRITE (2,FMT='(I2,6X,I2)') NCOEF( 5 ), NCOEF( 6 ) */
/*                 WRITE (2,FMT='(I2,6X,I2)') NCOEF( 7 ) */
/*     C */
/*     C           Print Cheby coefficients. */
/*     C */
/*                 LASTAD = 0 */

/*                 DO K = 1, QAVSIZ */
/*                    LASTAD = LASTAD + NCOEF( K ) */
/*                 END DO */

/*                 DO K = 4, LASTAD + 4 */
/*                    WRITE (2,*) PKTDAT (K) */
/*                 END DO */

/*              END DO */

/*           END IF */

/* $ Restrictions */

/*     1)  The binary CK file containing the segment whose descriptor */
/*         was passed to this routine must be opened for read or write */
/*         access by either CKLPF, DAFOPR, or DAFOPW. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */
/*     Y.K. Zaiko         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.2, 26-OCT-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.1, 18-APR-2014 (BVS) */

/*        Minor header edits. */

/* -    SPICELIB Version 1.0.0, 05-MAY-1999 (YKZ) (BVS) */

/* -& */
/* $ Index_Entries */

/*     get CK type_4 record */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Length (in DPs) of non-coefficient front part of RECORD when */
/*     it contains decoded numbers of coefficients. It is one less */
/*     than the length of the same part in a record exchanged between */
/*     CKR04 and CKE04 because it doesn't contain time at which */
/*     pointing has to be evaluated. */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("CKGR04", (ftnlen)6);
    }

/*     Unpack descriptor and check segment data type. Signal an error */
/*     if it's not 4. */

    dafus_(descr, &c__2, &c__6, dcd, icd);
    if (icd[2] != 4) {
	setmsg_("Data type of the segment should be 4: Passed  descriptor sh"
		"ows type = #.", (ftnlen)72);
	errint_("#", &icd[2], (ftnlen)1);
	sigerr_("SPICE(CKWRONGDATATYPE)", (ftnlen)22);
	chkout_("CKGR04", (ftnlen)6);
	return 0;
    }

/*     If a request was made for a data record which doesn't */
/*     exist, then signal an error and leave. */

    cknr04_(handle, descr, &nrec);
    if (*recno < 1 || *recno > nrec) {
	setmsg_("Requested record number (#) does not exist. There are # rec"
		"ords in the segment.", (ftnlen)79);
	errint_("#", recno, (ftnlen)1);
	errint_("#", &nrec, (ftnlen)1);
	sigerr_("SPICE(CKNONEXISTREC)", (ftnlen)20);
	chkout_("CKGR04", (ftnlen)6);
	return 0;
    }

/*     Get the data record indexed by RECNO. */

    sgfpkt_(handle, descr, recno, recno, record, ends);

/*     Decode 7 numbers of coefficients from double precision value. */

    zzck4d2i_(&record[2], &c__7, &c_b15, numcft);

/*     Compute total number of coefficients in the fetched packet. */

    numall = 0;
    for (k = 1; k <= 7; ++k) {
	numall += numcft[(i__1 = k - 1) < 7 && 0 <= i__1 ? i__1 : s_rnge(
		"numcft", i__1, "ckgr04_", (ftnlen)378)];
    }

/*     Move polynomial coefficients to the right to free space for */
/*     decoded numbers of coefficients and insert these numbers */
/*     starting from the third position. */

    for (k = numall; k >= 1; --k) {
	record[k + 8] = record[k + 2];
    }
    for (k = 1; k <= 7; ++k) {
	record[k + 1] = (doublereal) numcft[(i__1 = k - 1) < 7 && 0 <= i__1 ? 
		i__1 : s_rnge("numcft", i__1, "ckgr04_", (ftnlen)391)];
    }

/*     All done. */

    chkout_("CKGR04", (ftnlen)6);
    return 0;
} /* ckgr04_ */

