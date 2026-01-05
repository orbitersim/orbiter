/* getelm.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure GETELM ( Get the components from two-line elements ) */
/* Subroutine */ int getelm_(integer *frstyr, char *lines, doublereal *epoch, 
	doublereal *elems, ftnlen lines_len)
{
    extern /* Subroutine */ int zzgetelm_(integer *, char *, doublereal *, 
	    doublereal *, logical *, char *, ftnlen, ftnlen), chkin_(char *, 
	    ftnlen), errch_(char *, char *, ftnlen, ftnlen);
    char error[256];
    logical ok;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Parse the "lines" of a two-line element set, returning the */
/*     elements in units suitable for use in SPICE software. */

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

/*     PARSING */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     FRSTYR     I   Year of earliest representable two-line elements. */
/*     LINES      I   A pair of "lines" containing two-line elements. */
/*     EPOCH      O   The epoch of the elements in seconds past J2000. */
/*     ELEMS      O   The elements converted to SPICE units. */

/* $ Detailed_Input */

/*     FRSTYR   is the first year possible for two-line elements. */
/*              Since two-line elements allow only two digits for */
/*              the year, some conventions must be followed concerning */
/*              which century the two digits refer to. FRSTYR */
/*              is the year of the earliest representable elements. */
/*              The two-digit year is mapped to the year in */
/*              the interval from FRSTYR to FRSTYR + 99 that */
/*              has the same last two digits as the two digit */
/*              year in the element set. For example if FRSTYR */
/*              is set to 1960  then the two digit years are mapped */
/*              as shown in the table below: */

/*                 Two-line         Maps to */
/*                 element year */
/*                    00            2000 */
/*                    01            2001 */
/*                    02            2002 */
/*                     .              . */
/*                     .              . */
/*                     .              . */
/*                    58            2058 */
/*                    59            2059 */
/*                   -------------------- */
/*                    60            1960 */
/*                    61            1961 */
/*                    62            1962 */
/*                     .              . */
/*                     .              . */
/*                     .              . */
/*                    99            1999 */

/*              Note that if Space Command should decide to represent */
/*              years in 21st century as 100 + the last two digits */
/*              of the year (for example: 2015 is represented as 115) */
/*              instead of simply dropping the first two digits of */
/*              the year, this routine will correctly map the year */
/*              as long as you set FRSTYR to some value between 1900 */
/*              and 1999. */

/*     LINES    is a pair of lines of text that comprise a Space */
/*              command "two-line element" set. These text lines */
/*              should be the same as they are presented in the */
/*              two-line element files available from Space Command */
/*              (formerly NORAD). See $Particulars for a detailed */
/*              description of the format. */

/* $ Detailed_Output */

/*     EPOCH    is the epoch of the two-line elements supplied via */
/*              the input array LINES. EPOCH is returned in TDB */
/*              seconds past J2000. */

/*     ELEMS    is an array containing the elements from the two-line */
/*              set supplied via the array LINES. The elements are */
/*              in units suitable for use by the SPICE routines */
/*              EV2LIN and SPKW10. */

/*              Also note that the elements XNDD6O and BSTAR */
/*              incorporate the exponential factor present in the */
/*              input two-line elements in LINES. (See $Particulars */
/*              below). */

/*                 ELEMS (  1 ) = NDT20 in radians/minute**2 */
/*                 ELEMS (  2 ) = NDD60 in radians/minute**3 */
/*                 ELEMS (  3 ) = BSTAR */
/*                 ELEMS (  4 ) = INCL  in radians */
/*                 ELEMS (  5 ) = NODE0 in radians */
/*                 ELEMS (  6 ) = ECC */
/*                 ELEMS (  7 ) = OMEGA in radians */
/*                 ELEMS (  8 ) = M0    in radians */
/*                 ELEMS (  9 ) = N0    in radians/minute */
/*                 ELEMS ( 10 ) = EPOCH of the elements in seconds */
/*                                past ephemeris epoch J2000. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If an error occurs while trying to parse the two-line element */
/*         set, the error SPICE(BADTLE) is signaled and a description of */
/*         the detected issue in the "two-line element" set is reported */
/*         on the long error message. */

/* $ Files */

/*     You must have loaded a SPICE leapseconds kernel into the */
/*     kernel pool prior to calling this routine. */

/* $ Particulars */

/*     This routine parses a Space Command Two-line element set and */
/*     returns the orbital elements properly scaled and in units */
/*     suitable for use by other SPICE software. Input elements */
/*     are provided in two-lines in accordance with the format */
/*     required by the two-line element sets available from Space */
/*     Command (formerly NORAD). See [1] and [2] for details. */

/*     Each of these lines is 69 characters long. The following table */
/*     define each of the individual fields for lines 1 and 2. */

/*        Line  Column  Type  Description */
/*        ----  ------  ----  ------------------------------------------ */
/*          1      01     N   Line number of Element Data (always 1) */
/*          1    03-07    N   Satellite number (NORAD catalog number) */
/*          1      08     A   Classification (U:Unclassified; S:Secret) */
/*          1    10-11    N   International designator (last two digits */
/*                            of launch year). */
/*          1    12-14    N   International designator (launch number of */
/*                            the year). */
/*          1    15-17    A   International designator (piece of the */
/*                            launch) */
/*          1    19-20    N   Epoch year (last two digits of year). */
/*          1    21-32    N   Epoch (day of the year and portion of the */
/*                            day) */
/*          1    34-43    N   NDT20: first time derivative of Mean */
/*                                   Motion */
/*          1    45-52    N   NDD60: Second time derivative of Mean */
/*                                   Motion (decimal point assumed) */
/*          1    54-61    N   BSTAR drag term (decimal point assumed) */
/*          1      63     N   Ephemeris type */
/*          1    65-68    N   Element number */
/*          1      69     N   Checksum. */

/*          2      01     N   Line number of Element Data (always 2) */
/*          2    03-07    N   Satellite number (must be the same as in */
/*                            line 1) */
/*          2    09-16    N   INCL: Inclination, in degrees */
/*          2    18-25    N   NODE0: Right Ascension of the Ascending */
/*                                   Node, in degrees */
/*          2    27-33    N   ECC: Eccentricity (decimal point assumed) */
/*          2    35-42    N   OMEGA: Argument of Perigee, in degrees */
/*          2    44-51    N   M0: Mean Anomaly, in degrees */
/*          2    53-63    N   N0: Mean Motion (revolutions per day) */
/*          2    64-68    N   Revolution number at epoch */
/*          2      69     N   Checksum */

/*     The column type A indicates "characters A-Z", the type N means */
/*     "numeric." */

/*     Column refers to the substring within the line, e.g. */


/*  1 22076U 92052A   97173.53461370 -.00000038  00000-0  10000-3 0   594 */
/*  2 22076  66.0378 163.4372 0008359 278.7732  81.2337 12.80930736227550 */
/*  ^ */
/*  123456789012345678901234567890123456789012345678901234567890123456789 */
/*           1         2         3         4         5         6 */


/*        In this example, the satellite number (column 03-07) is 22076. */


/*     The "raw" elements used by this routine in the first lines are */
/*     described in detail below as in several instances exponents and */
/*     decimal points are implied. Note that the input units are */
/*     degrees, degrees/day**n and revolutions/day. */

/*     The epoch (column 19-32; line 1) has a format NNNNN.NNNNNNNN, */
/*     where: */

/*                Fraction */
/*            DOY  of day */
/*            --- -------- */
/*          NNNNN.NNNNNNNN */
/*          -- */
/*        Year */

/*     An epoch of 00001.00000000 corresponds to 00:00:00 UTC on */
/*     2000 January 01. */

/*     The first derivative of Mean Motion (column 34-43, line 1), has */
/*     a format +.NNNNNNNN, where the first character could be either a */
/*     plus sign, a minus sign or a space. */

/*     The second derivative of Mean Motion (column 45-52, line 1) and */
/*     the BSTAR drag term (see [1] for details -- column 54-61, line 1) */
/*     have a format +NNNNN-N, where the first character could be either */
/*     a plus sign, a minus sign or a space, the decimal point is */
/*     assumed, and the exponent is marked by the sign (+/-) at */
/*     character 6 (column 51 and 60 for the second derivative and BSTAR */
/*     drag term respectively). */

/*     The "raw" elements in the second line consists primarily of mean */
/*     elements calculated using the SGP4/SDP4 orbital model (See [1]). */
/*     The Inclination, the Right Ascension of the Ascending Node, the */
/*     Argument of Perigee and the Mean Anomaly have units of degrees */
/*     and can range from 0 up to 360 degrees, except for the */
/*     Inclination that ranges from 0 to 180 degrees. The Eccentricity */
/*     value is provided with an assumed leading decimal point. For */
/*     example, a value of 9790714 corresponds to an eccentricity of */
/*     0.9790714. The Mean motion is measured in revolutions per day and */
/*     its format is NN.NNNNNNN. */

/*     This routine extracts these values, "inserts" the implied */
/*     decimal points and exponents and then converts the inputs */
/*     to units of radians, radians/minute, radians/minute**2, and */
/*     radians/minute**3 */

/* $ Examples */

/*     The numerical results shown for these examples may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Suppose that you have collected the two-line element data */
/*        for a spacecraft with NORAD ID 18123. The following example */
/*        code demonstrates how you could go about creating a type 10 */
/*        SPK segment. */

/*        Use the meta-kernel shown below to load the required SPICE */
/*        kernels. */


/*           KPL/MK */

/*           File name: getelm_ex1.tm */

/*           This meta-kernel is intended to support operation of SPICE */
/*           example programs. The kernels shown here should not be */
/*           assumed to contain adequate or correct versions of data */
/*           required by SPICE-based user applications. */

/*           In order for an application to use this meta-kernel, the */
/*           kernels referenced here must be present in the user's */
/*           current working directory. */

/*           The names and contents of the kernels referenced */
/*           by this meta-kernel are as follows: */

/*              File name           Contents */
/*              ---------           ------------------------------------ */
/*              naif0012.tls        Leapseconds */
/*              geophysical.ker     geophysical constants for evaluation */
/*                                  of two-line element sets. */

/*           The geophysical.ker is a PCK file that is provided with the */
/*           SPICE toolkit under the "/data" directory. */

/*           \begindata */

/*              KERNELS_TO_LOAD = ( 'naif0012.tls', */
/*                                  'geophysical.ker'  ) */

/*           \begintext */

/*           End of meta-kernel */


/*        Example code begins here. */


/*              PROGRAM GETELM_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     SPICELIB functions. */
/*        C */
/*              DOUBLE PRECISION      SPD */

/*        C */
/*        C     Local parameters. */
/*        C */
/*              INTEGER               NAMLEN */
/*              PARAMETER           ( NAMLEN = 40               ) */

/*              INTEGER               PNAMLN */
/*              PARAMETER           ( PNAMLN = 2                ) */

/*              CHARACTER*(*)         SPK10 */
/*              PARAMETER           ( SPK10  = 'getelm_ex1.bsp' ) */

/*              INTEGER               TLELLN */
/*              PARAMETER           ( TLELLN = 69               ) */

/*        C */
/*        C     The SPK type 10 segment will contain 18 two-line */
/*        C     elements sets for the NORAD spacecraft 18123 with */
/*        C     respect to the Earth (ID 399) in the J2000 reference */
/*        C     frame. */
/*        C */
/*        C     As stated in the naif_ids required reading, for Earth */
/*        C     orbiting spacecraft lacking a DSN identification code, */
/*        C     the NAIF ID is derived from the tracking ID assigned to */
/*        C     it by NORAD via: */
/*        C */
/*        C        NAIF ID = -100000 - NORAD ID code */
/*        C */
/*              INTEGER               TLESSZ */
/*              PARAMETER           ( TLESSZ = 9       ) */

/*              INTEGER               BODY */
/*              PARAMETER           ( BODY   = -118123 ) */

/*              INTEGER               CENTER */
/*              PARAMETER           ( CENTER = 399     ) */

/*              CHARACTER*(*)         FRMNAM */
/*              PARAMETER           ( FRMNAM = 'J2000' ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              CHARACTER*(NAMLEN)    IFNAME */
/*              CHARACTER*(PNAMLN)    NOADPN ( 8           ) */
/*              CHARACTER*(NAMLEN)    SEGID */
/*              CHARACTER*(TLELLN)    TLE    ( 2  * TLESSZ ) */

/*              DOUBLE PRECISION      CONSTS ( 8           ) */
/*              DOUBLE PRECISION      ELEMS  ( 10 * TLESSZ ) */
/*              DOUBLE PRECISION      EPOCHS (      TLESSZ ) */
/*              DOUBLE PRECISION      FIRST */
/*              DOUBLE PRECISION      LAST */

/*              INTEGER               HANDLE */
/*              INTEGER               I */
/*              INTEGER               N */
/*              INTEGER               NCOMCH */

/*        C */
/*        C     These are the variables that will hold the constants */
/*        C     required by SPK type 10. These constants are available */
/*        C     from the loaded PCK file, which provides the actual */
/*        C     values and units as used by NORAD propagation model. */
/*        C */
/*        C        Constant   Meaning */
/*        C        --------   ------------------------------------------ */
/*        C        J2         J2 gravitational harmonic for Earth. */
/*        C        J3         J3 gravitational harmonic for Earth. */
/*        C        J4         J4 gravitational harmonic for Earth. */
/*        C        KE         Square root of the GM for Earth. */
/*        C        QO         High altitude bound for atmospheric model. */
/*        C        SO         Low altitude bound for atmospheric model. */
/*        C        ER         Equatorial radius of the Earth. */
/*        C        AE         Distance units/earth radius. */
/*        C */
/*              DATA          NOADPN  /  'J2', 'J3', 'J4', 'KE', */
/*             .                         'QO', 'SO', 'ER', 'AE'  / */

/*        C */
/*        C     Define the Two-Line Element sets. */
/*        C */
/*              TLE(1)  = '1 18123U 87 53  A 87324.61041692 -.00000023' */
/*             .      //                   '  00000-0 -75103-5 0 00675' */
/*              TLE(2)  = '2 18123  98.8296 152.0074 0014950 168.7820 ' */
/*             .      //                   '191.3688 14.12912554 21686' */
/*              TLE(3)  = '1 18123U 87 53  A 87326.73487726  .00000045' */
/*             .      //                   '  00000-0  28709-4 0 00684' */
/*              TLE(4)  = '2 18123  98.8335 154.1103 0015643 163.5445 ' */
/*             .      //                   '196.6235 14.12912902 21988' */
/*              TLE(5)  = '1 18123U 87 53  A 87331.40868801  .00000104' */
/*             .      //                   '  00000-0  60183-4 0 00690' */
/*              TLE(6)  = '2 18123  98.8311 158.7160 0015481 149.9848 ' */
/*             .      //                   '210.2220 14.12914624 22644' */
/*              TLE(7)  = '1 18123U 87 53  A 87334.24129978  .00000086' */
/*             .      //                   '  00000-0  51111-4 0 00702' */
/*              TLE(8)  = '2 18123  98.8296 161.5054 0015372 142.4159 ' */
/*             .      //                   '217.8089 14.12914879 23045' */
/*              TLE(9)  = '1 18123U 87 53  A 87336.93227900 -.00000107' */
/*             .      //                   '  00000-0 -52860-4 0 00713' */
/*              TLE(10) = '2 18123  98.8317 164.1627 0014570 135.9191 ' */
/*             .      //                   '224.2321 14.12910572 23425' */
/*              TLE(11) = '1 18123U 87 53  A 87337.28635487  .00000173' */
/*             .      //                   '  00000-0  10226-3 0 00726' */
/*              TLE(12) = '2 18123  98.8284 164.5113 0015289 133.5979 ' */
/*             .      //                   '226.6438 14.12916140 23475' */
/*              TLE(13) = '1 18123U 87 53  A 87339.05673569  .00000079' */
/*             .      //                   '  00000-0  47069-4 0 00738' */
/*              TLE(14) = '2 18123  98.8288 166.2585 0015281 127.9985 ' */
/*             .      //                   '232.2567 14.12916010 24908' */
/*              TLE(15) = '1 18123U 87 53  A 87345.43010859  .00000022' */
/*             .      //                   '  00000-0  16481-4 0 00758' */
/*              TLE(16) = '2 18123  98.8241 172.5226 0015362 109.1515 ' */
/*             .      //                   '251.1323 14.12915487 24626' */
/*              TLE(17) = '1 18123U 87 53  A 87349.04167543  .00000042' */
/*             .      //                   '  00000-0  27370-4 0 00764' */
/*              TLE(18) = '2 18123  98.8301 176.1010 0015565 100.0881 ' */
/*             .      //                   '260.2047 14.12916361 25138' */

/*        C */
/*        C     Load the PCK file that provides the geophysical */
/*        C     constants required for the evaluation of the two-line */
/*        C     elements sets. Load also an LSK, as it is required by */
/*        C     GETELM to perform time conversions. Use a metakernel for */
/*        C     convenience. */
/*        C */
/*              CALL FURNSH ( 'getelm_ex1.tm' ) */

/*        C */
/*        C     Retrieve the data from the kernel, and place it on */
/*        C     the CONSTS array. */
/*        C */
/*              DO I = 1, 8 */

/*                 CALL BODVCD ( CENTER, NOADPN(I), 1, N, CONSTS(I) ) */

/*              END DO */

/*        C */
/*        C     Convert the Two Line Elements lines to the */
/*        C     element sets. */
/*        C */
/*              DO I = 1, TLESSZ */

/*                 CALL GETELM ( 1950,      TLE( (I-1)*2 + 1 ), */
/*             .                 EPOCHS(I), ELEMS( (I-1)*10 + 1 ) ) */

/*              END DO */

/*        C */
/*        C     Define the beginning and end of the segment to be */
/*        C     -/+ 12 hours from the first and last epochs, */
/*        C     respectively. */
/*        C */
/*              FIRST = EPOCHS(1     ) - 0.5D0 * SPD() */
/*              LAST  = EPOCHS(TLESSZ) + 0.5D0 * SPD() */

/*        C */
/*        C     NCOMCH is the number of characters to reserve for the */
/*        C     kernel's comment area. This example doesn't write */
/*        C     comments, so set to zero. */
/*        C */
/*              NCOMCH = 0 */

/*        C */
/*        C     Internal file name and segment ID. */
/*        C */
/*              IFNAME = 'Test for type 10 SPK internal file name' */
/*              SEGID  = 'SPK type 10 test segment' */

/*        C */
/*        C     Open a new SPK file. */
/*        C */
/*              CALL SPKOPN( SPK10, IFNAME, NCOMCH, HANDLE ) */

/*        C */
/*        C     Now add the segment. */
/*        C */
/*              CALL SPKW10 ( HANDLE, BODY,  CENTER, FRMNAM, */
/*             .              FIRST,  LAST,  SEGID,  CONSTS, */
/*             .              TLESSZ, ELEMS, EPOCHS         ) */

/*        C */
/*        C     Close the SPK file. */
/*        C */
/*              CALL SPKCLS ( HANDLE ) */

/*              END */


/*        When this program is executed, no output is presented on */
/*        screen. After run completion, a new SPK type 10 exists in */
/*        the output directory. */


/*     2) Suppose you have a set of two-line elements for the LUME 1 */
/*        cubesat. This example shows how you can use this routine */
/*        together with the routine EVSGP4 to propagate a state to an */
/*        epoch of interest. */

/*        Use the meta-kernel from the previous example to load the */
/*        required SPICE kernels. */


/*        Example code begins here. */


/*              PROGRAM GETELM_EX2 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local parameters. */
/*        C */
/*              CHARACTER*(*)         TIMSTR */
/*              PARAMETER           ( TIMSTR = '2020-05-26 02:25:00' ) */

/*              INTEGER               PNAMLN */
/*              PARAMETER           ( PNAMLN = 2  ) */

/*              INTEGER               TLELLN */
/*              PARAMETER           ( TLELLN = 69 ) */

/*        C */
/*        C     The LUME-1 cubesat is an Earth orbiting object; set */
/*        C     the center ID to the Earth ID. */
/*        C */
/*              INTEGER               CENTER */
/*              PARAMETER           ( CENTER  = 399     ) */

/*        C */
/*        C     Local variables. */
/*        C */
/*              CHARACTER*(PNAMLN)    NOADPN ( 8  ) */
/*              CHARACTER*(TLELLN)    TLE    ( 2  ) */

/*              DOUBLE PRECISION      ELEMS  ( 10 ) */
/*              DOUBLE PRECISION      EPOCH */
/*              DOUBLE PRECISION      ET */
/*              DOUBLE PRECISION      GEOPHS ( 8  ) */
/*              DOUBLE PRECISION      STATE  ( 6  ) */

/*              INTEGER               I */
/*              INTEGER               N */

/*        C */
/*        C     These are the variables that will hold the constants */
/*        C     required by EV2LIN. These constants are available from */
/*        C     the loaded PCK file, which provides the actual values */
/*        C     and units as used by NORAD propagation model. */
/*        C */
/*        C        Constant   Meaning */
/*        C        --------   ------------------------------------------ */
/*        C        J2         J2 gravitational harmonic for Earth. */
/*        C        J3         J3 gravitational harmonic for Earth. */
/*        C        J4         J4 gravitational harmonic for Earth. */
/*        C        KE         Square root of the GM for Earth. */
/*        C        QO         High altitude bound for atmospheric model. */
/*        C        SO         Low altitude bound for atmospheric model. */
/*        C        ER         Equatorial radius of the Earth. */
/*        C        AE         Distance units/earth radius. */
/*        C */
/*              DATA          NOADPN  /  'J2', 'J3', 'J4', 'KE', */
/*             .                         'QO', 'SO', 'ER', 'AE'  / */

/*        C */
/*        C     Define the Two-Line Element set for LUME-1. */
/*        C */
/*              TLE(1)  = '1 43908U 18111AJ  20146.60805006  .00000806' */
/*             .      //                   '  00000-0  34965-4 0  9999' */
/*              TLE(2)  = '2 43908  97.2676  47.2136 0020001 220.6050 ' */
/*             .      //                   '139.3698 15.24999521 78544' */

/*        C */
/*        C     Load the PCK file that provides the geophysical */
/*        C     constants required for the evaluation of the two-line */
/*        C     elements sets. Load also an LSK, as it is required by */
/*        C     GETELM to perform time conversions. Use a metakernel for */
/*        C     convenience. */
/*        C */
/*              CALL FURNSH ( 'getelm_ex1.tm' ) */

/*        C */
/*        C     Retrieve the data from the kernel, and place it on */
/*        C     the GEOPHS array. */
/*        C */
/*              DO I = 1, 8 */

/*                 CALL BODVCD ( CENTER, NOADPN(I), 1, N, GEOPHS(I) ) */

/*              END DO */

/*        C */
/*        C     Convert the Two Line Elements lines to the element sets. */
/*        C     Set the lower bound for the years to be the beginning */
/*        C     of the space age. */
/*        C */
/*              CALL GETELM ( 1957, TLE, EPOCH, ELEMS ) */

/*        C */
/*        C     Now propagate the state using EV2LIN to the epoch of */
/*        C     interest. */
/*        C */
/*              CALL STR2ET ( TIMSTR, ET ) */
/*              CALL EVSGP4 ( ET, GEOPHS, ELEMS, STATE ) */

/*        C */
/*        C     Display the results. */
/*        C */
/*              WRITE(*,'(2A)')       'Epoch   : ', TIMSTR */
/*              WRITE(*,'(A,3F16.8)') 'Position:', (STATE(I), I=1,3) */
/*              WRITE(*,'(A,3F16.8)') 'Velocity:', (STATE(I), I=4,6) */


/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*        Epoch   : 2020-05-26 02:25:00 */
/*        Position:  -4644.60403398  -5038.95025539   -337.27141116 */
/*        Velocity:     -0.45719025      0.92884817     -7.55917355 */


/* $ Restrictions */

/*     1)  The format of the two-line elements suffer from a "millennium" */
/*         problem --- only two digits are used for the year of the */
/*         elements. It is not clear how Space Command will deal with */
/*         this problem. NAIF hopes that by adjusting the input FRSTYR */
/*         you should be able to use this routine well into the 21st */
/*         century. */

/*         The approach taken to mapping the two-digit year to the */
/*         full year is given by the code below. Here, YR is the */
/*         integer obtained by parsing the two-digit year from the first */
/*         line of the elements. */

/*            BEGYR = (FRSTYR/100)*100 */
/*            YEAR  = BEGYR + YR */

/*            IF ( YEAR .LT. FRSTYR ) THEN */
/*               YEAR = YEAR + 100 */
/*            END IF */

/*         This mapping will be changed if future two-line element */
/*         representations make this method of computing the full year */
/*         inaccurate. */

/* $ Literature_References */

/*     [1]  F. Hoots and R. Roehrich, "Spacetrack Report #3: Models for */
/*          Propagation of the NORAD Element Sets," U.S. Air Force */
/*          Aerospace Defense Command, Colorado Springs, CO, 1980. */

/*     [2]  "SDC/SCC Two Card Element Set - Transmission Format," */
/*          ADCOM/DO Form 12. */

/*     [3]  F. Hoots, "Spacetrack Report #6: Models for Propagation of */
/*          Space Command Element Sets,"  U.S. Air Force Aerospace */
/*          Defense Command, Colorado Springs, CO, 1986. */

/*     [4]  F. Hoots, P. Schumacher and R. Glover, "History of Analytical */
/*          Orbit Modeling in the U. S. Space Surveillance System," */
/*          Journal of Guidance, Control, and Dynamics. 27(2):174-185, */
/*          2004. */

/*     [5]  D. Vallado, P. Crawford, R. Hujsak and T. Kelso, "Revisiting */
/*          Spacetrack Report #3," paper AIAA 2006-6753 presented at the */
/*          AIAA/AAS Astrodynamics Specialist Conference, Keystone, CO., */
/*          August 21-24, 2006. */

/* $ Author_and_Institution */

/*     J. Diaz del Rio    (ODC Space) */
/*     W.L. Taber         (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 3.1.0, 06-NOV-2021 (JDR) (MCS) */

/*        Changed the output array declaration from assumed-size array */
/*        to a constant size array. */

/*        Edited the header to comply with NAIF standard. Added complete */
/*        code examples; second one based on existing code fragments. */

/*        Corrected the input element names in ELEMS. */

/*        Updated $Particulars to describe in detail the TLE format, */
/*        $Restrictions to provide indications on the "millennium" */
/*        problem of TLE data, and $Literature_References to point to */
/*        the sources of the detailed documentation. */

/* -    SPICELIB Version 3.0.0, 30-MAR-2004 (EDW) */

/*        Routine now passes inputs to ZZGETELM then responds to */
/*        any error condition. */

/* -    SPICELIB Version 2.0.0, 03-MAR-2000 (WLT) */

/*        The routine was modified to check that all of the terms */
/*        in the two-line element set are parsed correctly. */

/* -    SPICELIB Version 1.0.0, 26-JUN-1997 (WLT) */

/* -& */
/* $ Index_Entries */

/*     Parse two-line elements */

/* -& */

/*     Spicelib functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("GETELM", (ftnlen)6);

/*     Pass the input to the parse routine... */

    zzgetelm_(frstyr, lines, epoch, elems, &ok, error, lines_len, (ftnlen)256)
	    ;

/*     ...check for an error parsing the TLE pair. Signal an */
/*     error if OK equals .FALSE. */

    if (! ok) {
	setmsg_("Error in TLE set. #", (ftnlen)19);
	errch_("#", error, (ftnlen)1, (ftnlen)256);
	sigerr_("SPICE(BADTLE)", (ftnlen)13);
	chkout_("GETELM", (ftnlen)6);
	return 0;
    }
    chkout_("GETELM", (ftnlen)6);
    return 0;
} /* getelm_ */

