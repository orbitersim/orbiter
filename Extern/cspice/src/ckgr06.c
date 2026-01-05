/* ckgr06.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__6 = 6;

/* $Procedure CKGR06 ( C-kernel, get record, type 06 ) */
/* Subroutine */ int ckgr06_(integer *handle, doublereal *descr, integer *
	msno, integer *recno, doublereal *record)
{
    /* Initialized data */

    static integer pktszs[4] = { 8,4,14,7 };

    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer i_dnnt(doublereal *), s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer nrec;
    doublereal rate;
    integer baddr, eaddr;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer minib, minie;
    extern /* Subroutine */ int dafus_(doublereal *, integer *, integer *, 
	    doublereal *, integer *), dafgda_(integer *, integer *, integer *,
	     doublereal *);
    doublereal dc[2];
    integer ic[6];
    extern logical failed_(void);
    doublereal dpdata[1];
    integer epcbas, epaddr, bufbas;
    doublereal buffer[4];
    integer nepdir, ptrbas;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen);
    integer nintvl;
    extern logical return_(void);
    integer pktsiz, subtyp;

/* $ Abstract */

/*     Return a specified pointing record from a type 6 CK segment, given */
/*     the CK file's handle and the segment's descriptor. */

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

/*     Declare parameters specific to CK type 06. */

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

/* $ Keywords */

/*     CK */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman      (JPL) */
/*     B.V. Semenov      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 10-MAR-2014 (NJB) (BVS) */

/* -& */

/*     Maximum polynomial degree supported by the current */
/*     implementation of this CK type. */


/*     Integer code indicating `true': */


/*     Integer code indicating `false': */


/*     CK type 6 subtype codes: */


/*     Subtype 0:  Hermite interpolation, 8-element packets. Quaternion */
/*                 and quaternion derivatives only, no angular velocity */
/*                 vector provided. Quaternion elements are listed */
/*                 first, followed by derivatives. Angular velocity is */
/*                 derived from the quaternions and quaternion */
/*                 derivatives. */


/*     Subtype 1:  Lagrange interpolation, 4-element packets. Quaternion */
/*                 only. Angular velocity is derived by differentiating */
/*                 the interpolating polynomials. */


/*     Subtype 2:  Hermite interpolation, 14-element packets. */
/*                 Quaternion and angular angular velocity vector, as */
/*                 well as derivatives of each, are provided. The */
/*                 quaternion comes first, then quaternion derivatives, */
/*                 then angular velocity and its derivatives. */


/*     Subtype 3:  Lagrange interpolation, 7-element packets. Quaternion */
/*                 and angular velocity vector provided.  The quaternion */
/*                 comes first. */


/*     Number of subtypes: */


/*     Packet sizes associated with the various subtypes: */


/*     Maximum packet size for type 6: */


/*     Minimum packet size for type 6: */


/*     The CKPFS record size declared in ckparam.inc must be at least as */
/*     large as the maximum possible size of a CK type 6 record. */

/*     The largest possible CK type 6 record has subtype 3 (note that */
/*     records of subtype 2 have half as many epochs as those of subtype */
/*     3, for a given polynomial degree). A subtype 3 record contains */

/*        - The evaluation epoch */
/*        - The subtype and packet count */
/*        - MAXDEG+1 packets of size C06PS3 */
/*        - MAXDEG+1 time tags */


/*     End of file ck06.inc. */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   The handle of the file containing the segment. */
/*     DESCR      I   The segment descriptor. */
/*     MSNO       I   Index of the mini-segment containing the record. */
/*     RECNO      I   Index of the pointing record to be returned. */
/*     RECORD     O   The pointing record. */

/* $ Detailed_Input */

/*     HANDLE   is the handle of the binary CK file containing the */
/*              segment. Normally the CK file should be open for read */
/*              access. See the $Files section below for details. */

/*     DESCR    is the DAF descriptor of the type 6 segment. */

/*     RECNO    is the number of the discrete pointing instance to be */
/*              returned from the specified type 6 segment. */

/* $ Detailed_Output */

/*     RECORD   is the pointing record indexed by RECNO in the */
/*              segment. The contents are as follows: */

/*                 RECORD( 1 ) = CLKOUT */

/*              CLKOUT is the encoded spacecraft clock time associated */
/*              with the returned pointing values. */

/*                 RECORD( 2 ) = SUBTYP */

/*              SUBTYP is the CK type 6 subtype code. This code */
/*              identifies the structure and meaning of the rest */
/*              of the record. However, all subtypes have a */
/*              quaternion stored in elements 4-7. */

/*                 RECORD( 3 ) = RATE */

/*              RATE is the nominal SCLK rate expressed in units of */
/*              seconds per tick. This rate is required to convert */
/*              quaternion or angular velocity derivatives from units */
/*              of radians/tick to radians/s. */

/*                 RECORD( 4 ) = q0 */
/*                 RECORD( 5 ) = q1 */
/*                 RECORD( 6 ) = q2 */
/*                 RECORD( 7 ) = q3 */

/*              Subtype 1 ends here; there are no angular velocity */
/*              data. Angular velocity is derived by differentiating */
/*              Lagrange interpolating polynomials. */

/*                 RECORD(  8 ) =  ] */
/*                 RECORD(  9 ) =  ] --- For subtypes 0 and 2, these */
/*                 RECORD( 10 ) =  ]     elements contain a quaternion */
/*                 RECORD( 11 ) =  ]     derivative. For subtype 3, */
/*                                       elements 8-10 contain an */
/*                                       angular velocity vector; */
/*                                       element 11 is unassigned. */

/*                                       All subtypes except subtype */
/*                                       2 stop here. */

/*                 RECORD( 12 ) =  ] */
/*                 RECORD( 13 ) =  ] --- For subtype 2, these */
/*                 RECORD( 14 ) =  ]     elements contain an angular */
/*                                       velocity vector. */


/*                 RECORD( 15 ) =  ] */
/*                 RECORD( 16 ) =  ] --- For subtype 2, these */
/*                 RECORD( 17 ) =  ]     elements contain the */
/*                                       derivative of an angular */
/*                                       velocity vector. */

/*              The quantities q0 - q3 are the components of the */
/*              quaternion that represents the C-matrix that transforms */
/*              vectors from the inertial reference frame of the */
/*              segment to the instrument frame at time CLKOUT. */

/*              Quaternion derivatives, angular velocity, or the */
/*              derivative of angular velocity are valid only if */
/*              these are supported by the segment subtype and */
/*              if the segment descriptor indicates that angular */
/*              velocity is present. */

/*              The components of the angular velocity vector are */
/*              specified relative to the inertial reference frame of */
/*              the segment. */

/*              Units of angular velocity and of quaternion */
/*              derivatives are radians/second and 1/second */
/*              respectively. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the segment is not of data type 6, the error */
/*         SPICE(CKWRONGDATATYPE) is signaled. */

/*     2)  If MSNO is less than one or greater than the number of */
/*         mini-segments in the specified segment, the error */
/*         SPICE(INDEXOUTOFRANGE) is signaled. */

/*     3)  If RECNO is less than one or greater than the number of */
/*         records in the specified segment, the error */
/*         SPICE(CKNONEXISTREC) is signaled. */

/*     4)  If the specified handle does not belong to any DAF file that */
/*         is currently known to be open, an error is signaled by a */
/*         routine in the call tree of this routine. */

/*     5)  If DESCR is not a valid descriptor of a valid segment in the */
/*         CK file specified by HANDLE, the results of this routine are */
/*         unpredictable. */

/*     6)  If the segment subtype is not recognized, the error */
/*         SPICE(NOTSUPPORTED) is signaled. */

/* $ Files */

/*     The CK file specified by HANDLE may be open for read or write */
/*     access. Normally, the file should have been opened for read */
/*     access. If the file is open for write access, the calling */
/*     application must ensure integrity of the CK segment being read. */
/*     If the structure of the segment is invalid---for example, if the */
/*     segment has been partially written---this routine will either */
/*     return invalid results, or it will cause a system-level runtime */
/*     error. */

/* $ Particulars */

/*     Note that the mini-segment interpolation window size is not */
/*     returned in the pointing record; this parameter is not required */
/*     in order to interpret the record. Call CKMP06 to obtain the */
/*     window size. */

/*     For a complete description of the internal structure of a type 6 */
/*     segment, see the CK Required Reading. */

/*     This routine is normally used in conjunction with CKNM06 and */
/*     CKGM06 to obtain time tags and packet data from a specified type */
/*     6 CK segment. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */


/*     1) The following program dumps records from a CK file that */
/*        contains only type 6 segments. */

/*        Example code begins here. */


/*              PROGRAM CKGR06_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Dump all records from a CK that */
/*        C     contains only segments of type 6. */
/*        C */
/*              INCLUDE 'ck06.inc' */

/*        C */
/*        C     Local parameters */
/*        C */
/*              INTEGER               ND */
/*              PARAMETER           ( ND     = 2 ) */

/*              INTEGER               NI */
/*              PARAMETER           ( NI     = 6 ) */

/*              INTEGER               DSCSIZ */
/*              PARAMETER           ( DSCSIZ = 5 ) */

/*              INTEGER               FILSIZ */
/*              PARAMETER           ( FILSIZ = 255 ) */

/*        C */
/*        C     RECSIZ is the size of the largest pointing */
/*        C     record, which corresponds to subtype 2. */
/*        C */
/*              INTEGER               RECSIZ */
/*              PARAMETER           ( RECSIZ = C06PS2 + 3 ) */

/*        C */
/*        C     Local variables */
/*        C */
/*              CHARACTER*(FILSIZ)    CK */

/*              DOUBLE PRECISION      DC     ( ND ) */
/*              DOUBLE PRECISION      DESCR  ( DSCSIZ ) */
/*              DOUBLE PRECISION      IVLBDS ( 2 ) */
/*              DOUBLE PRECISION      LSTEPC */
/*              DOUBLE PRECISION      RATE */
/*              DOUBLE PRECISION      RECORD ( RECSIZ ) */

/*              INTEGER               DTYPE */
/*              INTEGER               HANDLE */
/*              INTEGER               IC     ( NI ) */
/*              INTEGER               RECNO */
/*              INTEGER               MSNO */
/*              INTEGER               NMINI */
/*              INTEGER               NREC */
/*              INTEGER               SEGNO */
/*              INTEGER               SUBTYP */
/*              INTEGER               WINSIZ */

/*              LOGICAL               FOUND */


/*              CALL PROMPT ( 'Enter name of CK to dump > ', CK ) */

/*              CALL DAFOPR ( CK, HANDLE ) */

/*        C */
/*        C     Dump data from each CK segment. */
/*        C */
/*              SEGNO = 0 */

/*              CALL DAFBFS ( HANDLE ) */
/*              CALL DAFFNA ( FOUND  ) */

/*              DO WHILE ( FOUND ) */

/*                 SEGNO = SEGNO + 1 */

/*                 WRITE (*,*) ' ' */
/*                 WRITE (*,*) ' ' */
/*                 WRITE (*,*) 'Segment number: ', SEGNO */

/*        C */
/*        C        Fetch and unpack the descriptor of the */
/*        C        current segment; check the data type. */
/*        C */
/*                 CALL DAFGS ( DESCR ) */
/*                 CALL DAFUS ( DESCR, ND, NI, DC, IC ) */

/*                 DTYPE = IC(3) */

/*                 IF ( DTYPE .NE. 6 ) THEN */

/*                    CALL SETMSG ( 'Data type must be 6 but was #.' ) */
/*                    CALL ERRINT ( '#',  DTYPE                      ) */
/*                    CALL SIGERR ( 'SPICE(NOTSUPPORTED)'            ) */

/*                 END IF */

/*        C */
/*        C        Get the mini-segment count for this */
/*        C        segment. */
/*        C */
/*                 CALL CKNM06 ( HANDLE, DESCR, NMINI ) */

/*        C */
/*        C        Dump data from each mini-segment. */
/*        C */
/*                 DO MSNO = 1, NMINI */

/*        C */
/*        C           Get the mini-segment's record count */
/*        C           and time bounds. */
/*        C */
/*                    CALL CKMP06 ( HANDLE, DESCR,  MSNO, */
/*             .                    RATE,   SUBTYP, WINSIZ, */
/*             .                    NREC,   IVLBDS, LSTEPC ) */

/*                    WRITE (*,*) ' ' */
/*                    WRITE (*,*) '   Mini-segment number: ', MSNO */
/*                    WRITE (*,*) '      Rate:           ',   RATE */
/*                    WRITE (*,*) '      Subtype:        ',   SUBTYP */
/*                    WRITE (*,*) '      Window size:    ',   WINSIZ */
/*                    WRITE (*,*) '      Interval start: ',   IVLBDS(1) */
/*                    WRITE (*,*) '      Interval stop:  ',   IVLBDS(2) */
/*                    WRITE (*,*) '      Last epoch:     ',   LSTEPC */
/*                    WRITE (*,*) ' ' */

/*                    DO RECNO = 1, NREC */

/*                       CALL CKGR06 ( HANDLE, DESCR, */
/*             .                       MSNO,   RECNO,  RECORD ) */

/*                       WRITE (*,*) '      Record number: ', RECNO */
/*                       WRITE (*,*) '         SCLKDP:     ', RECORD(1) */
/*                       WRITE (*,*) '         Clock rate: ', RECORD(3) */

/*                       IF ( SUBTYP .EQ. C06TP0 ) THEN */

/*                          WRITE (*,*) '         Q(0): ', RECORD(4) */
/*                          WRITE (*,*) '         Q(1): ', RECORD(5) */
/*                          WRITE (*,*) '         Q(2): ', RECORD(6) */
/*                          WRITE (*,*) '         Q(3): ', RECORD(7) */
/*                          WRITE (*,*) '    d Q(0)/dt: ', RECORD(8) */
/*                          WRITE (*,*) '    d Q(1)/dt: ', RECORD(9) */
/*                          WRITE (*,*) '    d Q(2)/dt: ', RECORD(10) */
/*                          WRITE (*,*) '    d Q(3)/dt: ', RECORD(11) */

/*                       ELSE IF ( SUBTYP .EQ. C06TP1 ) THEN */

/*                          WRITE (*,*) '         Q(0): ', RECORD(4) */
/*                          WRITE (*,*) '         Q(1): ', RECORD(5) */
/*                          WRITE (*,*) '         Q(2): ', RECORD(6) */
/*                          WRITE (*,*) '         Q(3): ', RECORD(7) */

/*                       ELSE IF ( SUBTYP .EQ. C06TP2 ) THEN */

/*                          WRITE (*,*) '         Q(0): ', RECORD(4) */
/*                          WRITE (*,*) '         Q(1): ', RECORD(5) */
/*                          WRITE (*,*) '         Q(2): ', RECORD(6) */
/*                          WRITE (*,*) '         Q(3): ', RECORD(7) */
/*                          WRITE (*,*) '    d Q(0)/dt: ', RECORD(8) */
/*                          WRITE (*,*) '    d Q(1)/dt: ', RECORD(9) */
/*                          WRITE (*,*) '    d Q(2)/dt: ', RECORD(10) */
/*                          WRITE (*,*) '    d Q(3)/dt: ', RECORD(11) */
/*                          WRITE (*,*) '        AV(1): ', RECORD(12) */
/*                          WRITE (*,*) '        AV(2): ', RECORD(13) */
/*                          WRITE (*,*) '        AV(3): ', RECORD(14) */
/*                          WRITE (*,*) '   d AV(1)/dt: ', RECORD(15) */
/*                          WRITE (*,*) '   d AV(2)/dt: ', RECORD(16) */
/*                          WRITE (*,*) '   d AV(3)/dt: ', RECORD(17) */

/*                       ELSE IF ( SUBTYP .EQ. C06TP3 ) THEN */

/*                          WRITE (*,*) '         Q(0): ', RECORD(4) */
/*                          WRITE (*,*) '         Q(1): ', RECORD(5) */
/*                          WRITE (*,*) '         Q(2): ', RECORD(6) */
/*                          WRITE (*,*) '         Q(3): ', RECORD(7) */
/*                          WRITE (*,*) '        AV(1): ', RECORD(8) */
/*                          WRITE (*,*) '        AV(2): ', RECORD(9) */
/*                          WRITE (*,*) '        AV(3): ', RECORD(10) */

/*                       ELSE */
/*                          CALL SETMSG ( 'Subtype # is not ' */
/*             .            //            'recognized.'         ) */
/*                          CALL ERRINT ( '#', SUBTYP           ) */
/*                          CALL SIGERR ( 'SPICE(NOTSUPPORTED)' ) */
/*                       END IF */

/*                       WRITE (*,*) ' ' */

/*                   END DO */

/*                 END DO */

/*                 CALL DAFFNA ( FOUND ) */

/*              END DO */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, using the Rosetta CK file named */
/*        RATT_DV_257_02_01_T6_00344.BC, the output was: */


/*        Enter name of CK to dump > RATT_DV_257_02_01_T6_00344.BC */


/*         Segment number:            1 */

/*            Mini-segment number:            1 */
/*               Rate:              1.5258789062500000E-005 */
/*               Subtype:                   1 */
/*               Window size:              10 */
/*               Interval start:    24471796593941.691 */
/*               Interval stop:     24472844252095.523 */
/*               Last epoch:        24472844252095.523 */

/*               Record number:            1 */
/*                  SCLKDP:        24471796593941.691 */
/*                  Clock rate:    1.5258789062500000E-005 */
/*                  Q(0):  -0.95514652599900884 */
/*                  Q(1):   0.16277660709912350 */
/*                  Q(2):   0.11688592199582469 */
/*                  Q(3):  -0.21802883133317097 */

/*               Record number:            2 */
/*                  SCLKDP:        24472234538651.801 */
/*                  Clock rate:    1.5258789062500000E-005 */
/*                  Q(0):  -0.95746293340938016 */
/*                  Q(1):   0.14880147654385018 */
/*                  Q(2):   0.12021705739210503 */
/*                  Q(3):  -0.21603405018065600 */

/*               Record number:            3 */
/*                  SCLKDP:        24472676416997.039 */
/*                  Clock rate:    1.5258789062500000E-005 */
/*                  Q(0):  -0.95956954083287593 */
/*                  Q(1):   0.13478976855182764 */
/*                  Q(2):   0.12355113537344563 */
/*                  Q(3):  -0.21399329790313779 */

/*               Record number:            4 */
/*                  SCLKDP:        24472844252095.523 */
/*                  Clock rate:    1.5258789062500000E-005 */
/*                  Q(0):  -0.96030932381589129 */
/*                  Q(1):   0.12949634043544370 */
/*                  Q(2):   0.12480922302154081 */
/*                  Q(3):  -0.21321200307405938 */


/*            Mini-segment number:            2 */
/*               Rate:              1.5258789062500000E-005 */
/*               Subtype:                   1 */
/*               Window size:              10 */
/*               Interval start:    24472844252095.523 */
/*               Interval stop:     24472863912889.105 */
/*               Last epoch:        24472863912889.105 */

/*               Record number:            1 */
/*                  SCLKDP:        24472844252095.523 */
/*                  Clock rate:    1.5258789062500000E-005 */
/*                  Q(0):  -0.96030932403888680 */
/*                  Q(1):   0.12949633879120778 */
/*                  Q(2):   0.12480922338599261 */
/*                  Q(3):  -0.21321200285498659 */

/*               Record number:            2 */
/*                  SCLKDP:        24472851309816.297 */
/*                  Clock rate:    1.5258789062500000E-005 */
/*                  Q(0):  -0.96035266600496283 */
/*                  Q(1):   0.12922730685291675 */
/*                  Q(2):   0.12480259688433022 */
/*                  Q(3):  -0.21318389214860939 */

/*               Record number:            3 */
/*                  SCLKDP:        24472859879905.805 */
/*                  Clock rate:    1.5258789062500000E-005 */
/*                  Q(0):  -0.96041575813224878 */
/*                  Q(1):   0.12886248165419970 */
/*                  Q(2):   0.12474605317805663 */
/*                  Q(3):  -0.21315359384649502 */

/*               Record number:            4 */
/*                  SCLKDP:        24472863912889.105 */
/*                  Clock rate:    1.5258789062500000E-005 */
/*                  Q(0):  -0.96043784177251290 */
/*                  Q(1):   0.12871819083493355 */
/*                  Q(2):   0.12475418449192528 */
/*                  Q(3):  -0.21313651233726627 */


/*            Mini-segment number:            3 */
/*               Rate:              1.5258789062500000E-005 */
/*               Subtype:                   1 */
/*               Window size:              10 */
/*               Interval start:    24472863912889.105 */
/*               Interval stop:     24473139163999.207 */
/*               Last epoch:        24473139163999.207 */

/*               Record number:            1 */
/*                  SCLKDP:        24472863912889.105 */
/*                  Clock rate:    1.5258789062500000E-005 */
/*                  Q(0):  -0.96043784177455394 */
/*                  Q(1):   0.12871819083614683 */

/*        [...] */


/*        Warning: incomplete output. Only 100 out of 2378824 lines have */
/*        been provided. */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     J.M. Lynch         (JPL) */
/*     B.V. Semenov       (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 06-JUL-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 1.0.0, 14-MAR-2014 (NJB) (JML) (BVS) */

/* -& */
/* $ Index_Entries */

/*     get CK type_6 record */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */

/*        ND         is the number of double precision components in an */
/*                   unpacked C-kernel descriptor. */

/*        NI         is the number of integer components in an unpacked */
/*                   C-kernel descriptor. */

/*        DTYPE      is the data type of the segment that this routine */
/*                   operates on. */


/*     Mini-segment control area size: */


/*     Local variables */


/*     Saved variables */


/*     Initial values */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("CKGR06", (ftnlen)6);

/*     The number of discrete pointing instances contained in a data */
/*     type 6 segment is stored in the last double precision word of the */
/*     segment. Since the address of the last word is stored in the */
/*     sixth integer component of the segment descriptor, it is a */
/*     trivial matter to extract the count. */

/*     The unpacked descriptor contains the following information */
/*     about the segment: */

/*        DC(1)  Initial encoded SCLK */
/*        DC(2)  Final encoded SCLK */

/*        IC(1)  CK frame class ID (aka "instrument") */
/*        IC(2)  Inertial reference frame */
/*        IC(3)  Data type */
/*        IC(4)  Angular velocity flag */
/*        IC(5)  Initial address of segment data */
/*        IC(6)  Final address of segment data */


    dafus_(descr, &c__2, &c__6, dc, ic);

/*     If this segment is not of data type 6, then signal an error. */

    if (ic[2] != 6) {
	setmsg_("Data type of the segment should be 6: Passed descriptor sho"
		"ws type = #.", (ftnlen)71);
	errint_("#", &ic[2], (ftnlen)1);
	sigerr_("SPICE(CKWRONGDATATYPE)", (ftnlen)22);
	chkout_("CKGR06", (ftnlen)6);
	return 0;
    }

/*     Check the mini-segment index. */

/*     The number of mini-segments is the final word in the segment. */

    baddr = ic[4];
    eaddr = ic[5];
    dafgda_(handle, &eaddr, &eaddr, dpdata);
    if (failed_()) {
	chkout_("CKGR06", (ftnlen)6);
	return 0;
    }
    nintvl = i_dnnt(dpdata);
    if (*msno < 1 || *msno > nintvl) {
	setmsg_("Mini-segment index must be in range 1:# but was #.", (ftnlen)
		50);
	errint_("#", &nintvl, (ftnlen)1);
	errint_("#", msno, (ftnlen)1);
	sigerr_("SPICE(INDEXOUTOFRANGE)", (ftnlen)22);
	chkout_("CKGR06", (ftnlen)6);
	return 0;
    }

/*     Set the base address of the mini-segment pointers. There */
/*     are NINTVL+1 pointers, and these precede the control area. */

    ptrbas = eaddr - 2 - (nintvl + 1);

/*     Compute the mini-segment pointers as absolute */
/*     DAF addresses. The stored value is a relative address. */

    bufbas = ptrbas + *msno - 1;
    i__1 = bufbas + 1;
    i__2 = bufbas + 2;
    dafgda_(handle, &i__1, &i__2, buffer);
    if (failed_()) {
	chkout_("CKGR06", (ftnlen)6);
	return 0;
    }
    minib = baddr - 1 + i_dnnt(buffer);
    minie = baddr - 1 + i_dnnt(&buffer[1]) - 1;

/*     Fetch the control area of the mini-segment. */

    bufbas = minie - 4;
    i__1 = bufbas + 1;
    i__2 = bufbas + 4;
    dafgda_(handle, &i__1, &i__2, buffer);
    if (failed_()) {
	chkout_("CKGR06", (ftnlen)6);
	return 0;
    }

/*     Fetch the SCLK rate (seconds per tick), mini-segment */
/*     subtype, and record count. */

    rate = buffer[0];
    subtyp = i_dnnt(&buffer[1]);
    nrec = i_dnnt(&buffer[3]);

/*     Compute the packet size for this mini-segment. This will */
/*     be used a bit later. We'll also check the subtype. */

    if (subtyp < 0 || subtyp > 3) {
	setmsg_("Unexpected CK type 6 subtype # found in mini-segment #.", (
		ftnlen)55);
	errint_("#", &subtyp, (ftnlen)1);
	errint_("#", msno, (ftnlen)1);
	sigerr_("SPICE(NOTSUPPORTED)", (ftnlen)19);
	chkout_("CKGR06", (ftnlen)6);
	return 0;
    }
    pktsiz = pktszs[(i__1 = subtyp) < 4 && 0 <= i__1 ? i__1 : s_rnge("pktszs",
	     i__1, "ckgr06_", (ftnlen)772)];

/*     Check the record index. */

    if (*recno < 1 || *recno > nrec) {
	setmsg_("Record index must be in range 1:# but was #.", (ftnlen)44);
	errint_("#", &nrec, (ftnlen)1);
	errint_("#", recno, (ftnlen)1);
	sigerr_("SPICE(CKNONEXISTREC)", (ftnlen)20);
	chkout_("CKGR06", (ftnlen)6);
	return 0;
    }

/*     The epochs of the mini-segment precede the */
/*     mini-segment's control area and the epoch directories. */

    nepdir = (nrec - 1) / 100;
    epcbas = minie - 4 - nepdir - nrec;

/*     Fetch the mini-segment's epoch at index RECNO into */
/*     element 1 of the output record. */

    epaddr = epcbas + *recno;
    dafgda_(handle, &epaddr, &epaddr, record);

/*     Transfer the subtype and rate to the output record. */

    record[1] = (doublereal) subtyp;
    record[2] = rate;

/*     Locate the data packet at index RECNO. */

    bufbas = minib - 1 + (*recno - 1) * pktsiz;
    i__1 = bufbas + 1;
    i__2 = bufbas + pktsiz;
    dafgda_(handle, &i__1, &i__2, &record[3]);

/*     The record is complete if DAFGDA did its job. We don't */
/*     check FAILED here since we're about to return. */

    chkout_("CKGR06", (ftnlen)6);
    return 0;
} /* ckgr06_ */

