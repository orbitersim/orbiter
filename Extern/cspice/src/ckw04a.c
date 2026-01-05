/* ckw04a.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__142 = 142;
static integer c__7 = 7;
static doublereal c_b20 = 128.;

/* $Procedure CKW04A ( CK type 04: Add data to a segment ) */
/* Subroutine */ int ckw04a_(integer *handle, integer *npkts, integer *pktsiz,
	 doublereal *pktdat, doublereal *sclkdp)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer k;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer dispm, kk;
    extern /* Subroutine */ int errhan_(char *, integer *, ftnlen);
    integer displm;
    extern /* Subroutine */ int sigerr_(char *, ftnlen);
    integer numcft[7];
    extern /* Subroutine */ int chkout_(char *, ftnlen), setmsg_(char *, 
	    ftnlen), errint_(char *, integer *, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int sgwvpk_(integer *, integer *, integer *, 
	    doublereal *, integer *, doublereal *), zzck4i2d_(integer *, 
	    integer *, doublereal *, doublereal *);

/* $ Abstract */

/*     Add data to a type 4 CK segment currently being written to */
/*     the file associated with HANDLE. See also CKW04B and CKW04E. */

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
/*     HANDLE     I   The handle of an DAF file opened for writing. */
/*     NPKTS      I   Number of data packets to write to a segment. */
/*     PKTSIZ     I   The numbers of values in the data packets */
/*     PKTDAT     I   The data packets. */
/*     SCLKDP     I   The SCLK times associated with the data packets. */

/* $ Detailed_Input */

/*     HANDLE   is the file handle of a CK file in which a CK type 4 */
/*              segment is currently being written. */

/*     NPKTS    is the number of data packets to write to a segment. */

/*     PKTSIZ   is the number of values in all data packets. */

/*     PKTDAT   is the data packets. The data packets in this array */
/*              must be organized as described in the $Particulars */
/*              section of the header. */

/*     SCLKDP   contains the initial SCLK times corresponding to the */
/*              Chebyshev coefficients in PKTSIZ. The I'th time is */
/*              start time of the I'th packet coverage interval. */
/*              The times must form a strictly increasing sequence. */

/* $ Detailed_Output */

/*     None. */

/*     Data is stored in a segment in the DAF file associated with */
/*     HANDLE. */

/* $ Parameters */

/*     See 'ckparam.inc'. */

/* $ Exceptions */

/*     1)  If the number of coefficient sets and epochs is not positive, */
/*         the error SPICE(INVALIDARGUMENT) is signaled. */

/*     2)  If size of any input packet is greater that maximum allowed */
/*         type 4 CK record size minus one, the error */
/*         SPICE(INVALIDARGUMENT) is signaled. */

/* $ Files */

/*     See HANDLE in the $Detailed_Input section. */

/* $ Particulars */

/*     This routine adds data to a type 4 CK segment that is currently */
/*     being written to the associated with HANDLE. The segment must */
/*     have been started by a call to the routine CKW04B, the routine */
/*     which begins a type 4 CK segment. */

/*     This routine is one of a set of three routines for creating and */
/*     adding data to type 4 CK segments. These routines are: */

/*        CKW04B: Begin a type 4 CK segment. This routine must be */
/*                called before any data may be added to a type 4 */
/*                segment. */

/*        CKW04A: Add data to a type 4 CK segment. This routine may be */
/*                called any number of times after a call to CKW04B to */
/*                add type 4 records to the CK segment that was */
/*                started. */

/*        CKW04E: End a type 4 CK segment. This routine is called to */
/*                make the type 4 segment a permanent addition to the */
/*                DAF file. Once this routine is called, no further type */
/*                4 records may be added to the segment. A new segment */
/*                must be started. */

/*     A type 4 CK segment consists of coefficient sets for variable */
/*     order Chebyshev polynomials over consecutive time intervals of a */
/*     variable length. The gaps between intervals are allowed. The */
/*     Chebyshev polynomials represent individual SPICE-style quaternion */
/*     components q0, q1, q2 and q3 and individual angular velocities */
/*     AV1, AV2 and AV3 if they are included with the data. */

/*     See the discussion of quaternion styles below. */

/*     The pointing data supplied to the type 4 CK writer (CKW04A) */
/*     is packed into an array as a sequence of records, */

/*        ---------------------------------------------------- */
/*        | Record 1 | Record 2 | .. | Record N-1 | Record N | */
/*        ---------------------------------------------------- */

/*     with each record in data packets has the following format. */

/*        ---------------------------------------------------- */
/*        | The midpoint of the approximation interval       | */
/*        ---------------------------------------------------- */
/*        | The radius of the approximation interval         | */
/*        ---------------------------------------------------- */
/*        | Number of coefficients for q0                    | */
/*        ---------------------------------------------------- */
/*        | Number of coefficients for q1                    | */
/*        ---------------------------------------------------- */
/*        | Number of coefficients for q2                    | */
/*        ---------------------------------------------------- */
/*        | Number of coefficients for q3                    | */
/*        ---------------------------------------------------- */
/*        | Number of coefficients for AV1                   | */
/*        ---------------------------------------------------- */
/*        | Number of coefficients for AV2                   | */
/*        ---------------------------------------------------- */
/*        | Number of coefficients for AV3                   | */
/*        ---------------------------------------------------- */
/*        | q0 Cheby coefficients                            | */
/*        ---------------------------------------------------- */
/*        | q1 Cheby coefficients                            | */
/*        ---------------------------------------------------- */
/*        | q2 Cheby coefficients                            | */
/*        ---------------------------------------------------- */
/*        | q3 Cheby coefficients                            | */
/*        ---------------------------------------------------- */
/*        | AV1 Cheby coefficients (optional)                | */
/*        ---------------------------------------------------- */
/*        | AV2 Cheby coefficients (optional)                | */
/*        ---------------------------------------------------- */
/*        | AV3 Cheby coefficients (optional)                | */
/*        ---------------------------------------------------- */



/*     Quaternion Styles */
/*     ----------------- */

/*     There are different "styles" of quaternions used in */
/*     science and engineering applications. Quaternion styles */
/*     are characterized by */

/*     -  The order of quaternion elements */

/*     -  The quaternion multiplication formula */

/*     -  The convention for associating quaternions */
/*        with rotation matrices */

/*     Two of the commonly used styles are */

/*        - "SPICE" */

/*           > Invented by Sir William Rowan Hamilton */
/*           > Frequently used in mathematics and physics textbooks */

/*        - "Engineering" */

/*           > Widely used in aerospace engineering applications */


/*     SPICELIB subroutine interfaces ALWAYS use SPICE quaternions. */
/*     Quaternions of any other style must be converted to SPICE */
/*     quaternions before they are passed to SPICELIB routines. */


/*     Relationship between SPICE and Engineering Quaternions */
/*     ------------------------------------------------------ */

/*     Let M be a rotation matrix such that for any vector V, */

/*        M*V */

/*     is the result of rotating V by theta radians in the */
/*     counterclockwise direction about unit rotation axis vector A. */
/*     Then the SPICE quaternions representing M are */

/*        (+/-) (  cos(theta/2), */
/*                 sin(theta/2) A(1), */
/*                 sin(theta/2) A(2), */
/*                 sin(theta/2) A(3)  ) */

/*     while the engineering quaternions representing M are */

/*        (+/-) ( -sin(theta/2) A(1), */
/*                -sin(theta/2) A(2), */
/*                -sin(theta/2) A(3), */
/*                 cos(theta/2)       ) */

/*     For both styles of quaternions, if a quaternion q represents */
/*     a rotation matrix M, then -q represents M as well. */

/*     Given an engineering quaternion */

/*        QENG   = ( q0,  q1,  q2,  q3 ) */

/*     the equivalent SPICE quaternion is */

/*        QSPICE = ( q3, -q0, -q1, -q2 ) */


/*     Associating SPICE Quaternions with Rotation Matrices */
/*     ---------------------------------------------------- */

/*     Let FROM and TO be two right-handed reference frames, for */
/*     example, an inertial frame and a spacecraft-fixed frame. Let the */
/*     symbols */

/*        V    ,   V */
/*         FROM     TO */

/*     denote, respectively, an arbitrary vector expressed relative to */
/*     the FROM and TO frames. Let M denote the transformation matrix */
/*     that transforms vectors from frame FROM to frame TO; then */

/*        V   =  M * V */
/*         TO         FROM */

/*     where the expression on the right hand side represents left */
/*     multiplication of the vector by the matrix. */

/*     Then if the unit-length SPICE quaternion q represents M, where */

/*        q = (q0, q1, q2, q3) */

/*     the elements of M are derived from the elements of q as follows: */

/*          +-                                                         -+ */
/*          |           2    2                                          | */
/*          | 1 - 2*( q2 + q3 )   2*(q1*q2 - q0*q3)   2*(q1*q3 + q0*q2) | */
/*          |                                                           | */
/*          |                                                           | */
/*          |                               2    2                      | */
/*      M = | 2*(q1*q2 + q0*q3)   1 - 2*( q1 + q3 )   2*(q2*q3 - q0*q1) | */
/*          |                                                           | */
/*          |                                                           | */
/*          |                                                   2    2  | */
/*          | 2*(q1*q3 - q0*q2)   2*(q2*q3 + q0*q1)   1 - 2*( q1 + q2 ) | */
/*          |                                                           | */
/*          +-                                                         -+ */

/*     Note that substituting the elements of -q for those of q in the */
/*     right hand side leaves each element of M unchanged; this shows */
/*     that if a quaternion q represents a matrix M, then so does the */
/*     quaternion -q. */

/*     To map the rotation matrix M to a unit quaternion, we start by */
/*     decomposing the rotation matrix as a sum of symmetric */
/*     and skew-symmetric parts: */

/*                                        2 */
/*        M = [ I  +  (1-cos(theta)) OMEGA  ] + [ sin(theta) OMEGA ] */

/*                     symmetric                   skew-symmetric */


/*     OMEGA is a skew-symmetric matrix of the form */

/*                   +-             -+ */
/*                   |  0   -n3   n2 | */
/*                   |               | */
/*         OMEGA  =  |  n3   0   -n1 | */
/*                   |               | */
/*                   | -n2   n1   0  | */
/*                   +-             -+ */

/*     The vector N of matrix entries (n1, n2, n3) is the rotation axis */
/*     of M and theta is M's rotation angle. Note that N and theta */
/*     are not unique. */

/*     Let */

/*        C = cos(theta/2) */
/*        S = sin(theta/2) */

/*     Then the unit quaternions Q corresponding to M are */

/*        Q = +/- ( C, S*n1, S*n2, S*n3 ) */

/*     The mappings between quaternions and the corresponding rotations */
/*     are carried out by the SPICELIB routines */

/*        Q2M {quaternion to matrix} */
/*        M2Q {matrix to quaternion} */

/*     M2Q always returns a quaternion with scalar part greater than */
/*     or equal to zero. */


/*     SPICE Quaternion Multiplication Formula */
/*     --------------------------------------- */

/*     Given a SPICE quaternion */

/*        Q = ( q0, q1, q2, q3 ) */

/*     corresponding to rotation axis A and angle theta as above, we can */
/*     represent Q using "scalar + vector" notation as follows: */

/*        s =   q0           = cos(theta/2) */

/*        v = ( q1, q2, q3 ) = sin(theta/2) * A */

/*        Q = s + v */

/*     Let Q1 and Q2 be SPICE quaternions with respective scalar */
/*     and vector parts s1, s2 and v1, v2: */

/*        Q1 = s1 + v1 */
/*        Q2 = s2 + v2 */

/*     We represent the dot product of v1 and v2 by */

/*        <v1, v2> */

/*     and the cross product of v1 and v2 by */

/*        v1 x v2 */

/*     Then the SPICE quaternion product is */

/*        Q1*Q2 = s1*s2 - <v1,v2>  + s1*v2 + s2*v1 + (v1 x v2) */

/*     If Q1 and Q2 represent the rotation matrices M1 and M2 */
/*     respectively, then the quaternion product */

/*        Q1*Q2 */

/*     represents the matrix product */

/*        M1*M2 */

/* $ Examples */

/*     Assume that we have: */

/*        HANDLE   is the handle of an CK file opened with write */
/*                 access. */

/*        SEGID    is a character string of no more than 40 characters */
/*                 which provides a pedigree for the data in the CK */
/*                 segment we will create. */

/*        INST     is the SPICE ID code for the instrument whose */
/*                 pointing data is to be placed into the file. */

/*        AVFLAG   angular rates flag. */

/*        REFFRM   is the name of the SPICE reference frame for the */
/*                 pointing data. */

/*        BEGTIM   is the starting encoded SCLK time for which the */
/*                 segment is valid. */

/*        ENDTIM   is the ending encoded SCLK time for which the segment */
/*                 is valid. */

/*        N        is the number of type 4 records that we want to */
/*                 put into a segment in an CK file. */

/*        NPKTS    is integer array which contains the lengths of */
/*                 variable size data packets */

/*        RECRDS   contains N type 4 records packaged for the CK */
/*                 file. */

/*        SCSTRT   contains the initial encoded SC time for each of */
/*                 the records contained in RECRDS, where */

/*                    SCSTRT(I) < SCSTRT(I+1), I = 1, N-1 */

/*                    SCSTRT(1) <= FIRST, SCSTRT(N) < LAST */

/*     Then the following code fragment demonstrates how to create */
/*     a type 4 CK segment if all of the data for the segment is */
/*     available at one time. */

/*     C */
/*     C     Begin the segment. */
/*     C */
/*           CALL CKW04B ( HANDLE, BEGTIM, INST, REF, AVFLAG, SEGID ) */
/*     C */
/*     C     Add the data to the segment all at once. */
/*     C */
/*           CALL CKW04A ( HANDLE, N, NPKTS, RECRDS, SCSTRT ) */
/*     C */
/*     C     End the segment, making the segment a permanent */
/*     C     addition to the CK file. */
/*     C */
/*           CALL CKW04E ( HANDLE, ENDTIM ) */

/* $ Restrictions */

/*     1)  The type 4 CK segment to which the data is added must have */
/*         been started by the routine CKW04B, the routine which begins */
/*         a type 4 CK segment. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     B.V. Semenov       (JPL) */
/*     E.D. Wright        (JPL) */
/*     Y.K. Zaiko         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.3, 02-JUN-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.1.2, 18-APR-2014 (BVS) */

/*        Minor header edits. */

/* -    SPICELIB Version 1.1.1, 26-FEB-2008 (NJB) */

/*        Updated header; added information about SPICE */
/*        quaternion conventions. */

/* -    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW) */

/*        Removed DAFHLU call; replaced ERRFNM call with ERRHAN. */
/*        Added IMPLICIT NONE. */

/* -    SPICELIB Version 1.0.0, 05-MAY-1999 (YKZ) (BVS) */

/* -& */
/* $ Index_Entries */

/*     add data to a type_4 CK segment */

/* -& */

/*     Spicelib functions. */


/*     Local parameters. */


/*     The number of elements by which coefficients in each packet */
/*     have to be shifted to the left after numbers of coefficients */
/*     were packed into a single integer. */


/*     Local Variables. */


/*     Standard SPICELIB error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("CKW04A", (ftnlen)6);
    }

/*     First, check if the number of coefficient sets and epochs */
/*     is positive and whether each packet is smaller than the */
/*     maximum size of a record that CKPFS can handle. */

    i__1 = *npkts;
    for (k = 1; k <= i__1; ++k) {
	if (pktsiz[k - 1] <= 0) {
	    setmsg_("The number of coefficient sets and epochs in the # data"
		    " packet (record) to be added to the DAF segment in the f"
		    "ile '#' was not positive. Its value was: #.", (ftnlen)154)
		    ;
	    errint_("#", &k, (ftnlen)1);
	    errhan_("#", handle, (ftnlen)1);
	    errint_("#", &pktsiz[k - 1], (ftnlen)1);
	    sigerr_("SPICE(INVALIDARGUMENT)", (ftnlen)22);
	    chkout_("CKW04A", (ftnlen)6);
	    return 0;
	}

/*        We do .GE. comparison because a type 4 CK record passed */
/*        inside CKPFS will have one more element -- time at which */
/*        the pointing will be evaluated. */

	if (pktsiz[k - 1] >= 143) {
	    setmsg_("The total size of the # data packet (record) to be adde"
		    "d to the DAF segment in the file '#' is greater than the"
		    " maximum allowed type 4 record size #. Its value was: #.",
		     (ftnlen)167);
	    errint_("#", &k, (ftnlen)1);
	    errhan_("#", handle, (ftnlen)1);
	    errint_("#", &c__142, (ftnlen)1);
	    errint_("#", &pktsiz[k - 1], (ftnlen)1);
	    sigerr_("SPICE(INVALIDARGUMENT)", (ftnlen)22);
	    chkout_("CKW04A", (ftnlen)6);
	    return 0;
	}
    }
    displm = 0;
    dispm = 0;

/*     The cycle below encodes groups of numbers of coefficients in */
/*     data packets to single double precision numbers and shift */
/*     data in packets to the left to decrease the data packet */
/*     lengths. */

    i__1 = *npkts;
    for (k = 1; k <= i__1; ++k) {

/*        Encode integer numbers of coefficients for each component */
/*        to single double precision variable */

	for (kk = 1; kk <= 7; ++kk) {
	    numcft[(i__2 = kk - 1) < 7 && 0 <= i__2 ? i__2 : s_rnge("numcft", 
		    i__2, "ckw04a_", (ftnlen)591)] = (integer) pktdat[kk + 2 
		    + displm - 1];
	}
	zzck4i2d_(numcft, &c__7, &c_b20, &pktdat[dispm + 2]);

/*        Shift coefficients sets to the left to overwrite numbers of */
/*        packets */

	i__2 = pktsiz[k - 1];
	for (kk = 4; kk <= i__2; ++kk) {
	    pktdat[kk + dispm - 1] = pktdat[kk + 6 + displm - 1];
	}

/*        Shift middle value and radii of interval */

	pktdat[dispm] = pktdat[displm];
	pktdat[dispm + 1] = pktdat[displm + 1];
	displm += pktsiz[k - 1];

/*        Length of each data packet became less for 6 elements because */
/*        of encoding of 7 double precision numbers, which are the */
/*        numbers of polynomial coefficients, to one double precision */
/*        number */

	pktsiz[k - 1] += -6;
	dispm += pktsiz[k - 1];
    }

/*     Add the data. */

    sgwvpk_(handle, npkts, pktsiz, pktdat, npkts, sclkdp);

/*     No need to check FAILED() here, since all we do is check out. */
/*     Leave it up to the caller. */

    chkout_("CKW04A", (ftnlen)6);
    return 0;
} /* ckw04a_ */

