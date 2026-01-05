/*

-Procedure ckw05_c ( Write CK segment, type 5 )

-Abstract

   Write a type 5 segment to a CK file.

-Disclaimer

   THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
   CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
   GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
   ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
   PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
   TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
   WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
   PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
   SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
   SOFTWARE AND RELATED MATERIALS, HOWEVER USED.

   IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
   BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
   LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
   INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
   REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
   REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.

   RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
   THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
   CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
   ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.

-Required_Reading

   CK
   NAIF_IDS
   ROTATION
   TIME

-Keywords

   FILES
   POINTING

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"
   #undef ckw05_c


   void ckw05_c ( SpiceInt            handle,
                  SpiceCK05Subtype    subtyp,
                  SpiceInt            degree,
                  SpiceDouble         begtim,
                  SpiceDouble         endtim,
                  SpiceInt            inst,
                  ConstSpiceChar    * ref,
                  SpiceBoolean        avflag,
                  ConstSpiceChar    * segid,
                  SpiceInt            n,
                  ConstSpiceDouble    sclkdp [],
                  const void        * packts,
                  SpiceDouble         rate,
                  SpiceInt            nints,
                  ConstSpiceDouble    starts []    )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     I   Handle of an open CK file.
   subtyp     I   CK type 5 subtype code.
   degree     I   Degree of interpolating polynomials.
   begtim     I   The beginning encoded SCLK of the segment.
   endtim     I   The ending encoded SCLK of the segment.
   inst       I   The NAIF instrument ID code.
   ref        I   The reference frame of the segment.
   avflag     I   SPICETRUE if the segment will contain angular velocity.
   segid      I   Segment identifier.
   n          I   Number of packets.
   sclkdp     I   Encoded SCLK times.
   packts     I   Array of packets.
   rate       I   Nominal SCLK rate in seconds per tick.
   nints      I   Number of intervals.
   starts     I   Encoded SCLK interval start times.
   MAXDEG     P   Maximum allowed degree of interpolating polynomial.

-Detailed_Input

   handle      is the handle of the CK file to which the segment will be
               written. The file must have been opened with write
               access.

   subtyp      is an integer code indicating the subtype of the
               segment to be created.

   degree      is the degree of the polynomials used to interpolate the
               quaternions contained in the input packets. All
               components of the quaternions are interpolated by
               polynomials of fixed degree.

   begtim,
   endtim      are the beginning and ending encoded SCLK times
               for which the segment provides pointing information.
               begtim must be less than or equal to endtim, and at least
               one data packet must have a time tag t such that

                         begtim  <  t  <  endtim
                                 -     -

   inst        is the NAIF integer ID code for the instrument.

   ref         is a character string which specifies the
               reference frame of the segment. This should be one of
               the frames supported by the SPICELIB routine NAMFRM
               which is an entry point of FRAMEX.

               The rotation matrices represented by the quaternions
               that are to be written to the segment transform the
               components of vectors from the inertial reference frame
               specified by ref to components in the instrument fixed
               frame. Also, the components of the angular velocity
               vectors to be written to the segment should be given
               with respect to ref.

               ref should be the name of one of the frames supported
               by the SPICELIB routine NAMFRM.


   avflag      is a boolean flag which indicates whether or not the
               segment will contain angular velocity.

   segid       is the segment identifier.  A CK segment identifier may
               contain up to 40 characters, excluding the terminating
               null.

   packts      contains a time-ordered array of data packets
               representing the orientation of inst relative to the
               frame ref. Each packet contains a SPICE-style quaternion
               and optionally, depending on the segment subtype,
               attitude derivative data, from which a C-matrix and an
               angular velocity vector may be derived.

               See the discussion of "Quaternion Styles" in the
               -Particulars section below.

               The C-matrix represented by the Ith data packet is a
               rotation matrix that transforms the components of a
               vector expressed in the base frame specified by ref to
               components expressed in the instrument fixed frame at the
               time sclkdp(I).

               Thus, if a vector v has components x, y, z in the base
               frame, then v has components x', y', z' in the instrument
               fixed frame where:

                  [ x' ]     [          ] [ x ]
                  | y' |  =  |   cmat   | | y |
                  [ z' ]     [          ] [ z ]


               The attitude derivative information in packts[i] gives
               the angular velocity of the instrument fixed frame at
               time sclkdp[i] with respect to the reference frame
               specified by ref.

               The direction of an angular velocity vector gives the
               right-handed axis about which the instrument fixed
               reference frame is rotating. The magnitude of the vector
               is the magnitude of the instantaneous velocity of the
               rotation, in radians per second.

               Packet contents and the corresponding interpolation
               methods depend on the segment subtype, and are as
               follows:

                  Subtype 0:  Hermite interpolation, 8-element packets.
                              Quaternion and quaternion derivatives
                              only, no angular velocity vector provided.
                              Quaternion elements are listed first,
                              followed by derivatives. Angular velocity
                              is derived from the quaternions and
                              quaternion derivatives.

                  Subtype 1:  Lagrange interpolation, 4-element packets.
                              Quaternion only. Angular velocity is
                              derived by differentiating the
                              interpolating polynomials.

                  Subtype 2:  Hermite interpolation, 14-element packets.
                              Quaternion and angular angular velocity
                              vector, as well as derivatives of each,
                              are provided. The quaternion comes first,
                              then quaternion derivatives, then angular
                              velocity and its derivatives.

                  Subtype 3:  Lagrange interpolation, 7-element packets.
                              Quaternion and angular velocity vector
                              provided. The quaternion comes first.

               Angular velocity is always specified relative to the base
               frame.

   rate        is the nominal rate of the spacecraft clock associated
               with inst. Units are seconds per tick. rate is used to
               scale angular velocity to radians/second.

   nints       is the number of intervals that the pointing instances
               are partitioned into.

   starts      are the start times of each of the interpolation
               intervals. These times must be strictly increasing and
               must coincide with times for which the segment contains
               pointing.

-Detailed_Output

   None. See -Files section.

-Parameters

   MAXDEG     is the maximum allowed degree of the interpolating
              polynomial. If the value of MAXDEG is increased, the
              CSPICE routine ckpfs_ must be changed accordingly. In
              particular, the size of the record passed to ckrNN_ and
              ckeNN_ must be increased, and comments describing the
              record size must be changed.

-Exceptions

   If any of the following exceptions occur, this routine will
   return without creating a new segment.

   1)  If `handle' is not the handle of a C-kernel opened for writing,
       an error is signaled by a routine in the call tree of this
       routine.

   2)  If the last non-blank character of `segid' occurs past index 40,
       the error SPICE(SEGIDTOOLONG) is signaled by a routine in the
       call tree of this routine.

   3)  If `segid' contains any nonprintable characters, the error
       SPICE(NONPRINTABLECHARS) is signaled by a routine in the call
       tree of this routine.

   4)  If the first encoded SCLK time is negative, the error
       SPICE(INVALIDSCLKTIME) is signaled by a routine in the call
       tree of this routine. If any subsequent times are negative the
       error will be detected in exception (5).

   5)  If the encoded SCLK times are not strictly increasing, the
       error SPICE(TIMESOUTOFORDER) is signaled by a routine in the
       call tree of this routine.

   6)  If the name of the reference frame is not one of those
       supported by the routine FRAMEX, the error
       SPICE(INVALIDREFFRAME) is signaled by a routine in the call
       tree of this routine.

   7)  If the number of packets `n' is not at least 1, the error
       SPICE(TOOFEWPACKETS) is signaled by a routine in the call tree
       of this routine.

   8)  If `nints', the number of interpolation intervals, is less than
       or equal to 0, the error SPICE(INVALIDNUMINTS) is signaled by
       a routine in the call tree of this routine.

   9)  If the encoded SCLK interval start times are not strictly
       increasing, the error SPICE(TIMESOUTOFORDER) is signaled by a
       routine in the call tree of this routine.

   10) If an interval start time does not coincide with a time for
       which there is an actual pointing instance in the segment, the
       error SPICE(INVALIDSTARTTIME) is signaled by a routine in the
       call tree of this routine.

   11) This routine assumes that the rotation between adjacent
       quaternions that are stored in the same interval has a
       rotation angle of `theta' radians, where

          0  <=  theta  <  pi.

       The routines that evaluate the data in the segment produced
       by this routine cannot distinguish between rotations of `theta'
       radians, where `theta' is in the interval [0, pi), and
       rotations of

          theta   +   2 * k * pi

       radians, where k is any integer. These "large" rotations will
       yield invalid results when interpolated. You must ensure that
       the data stored in the segment will not be subject to this
       sort of ambiguity.

   12) If any quaternion has magnitude zero, the error
       SPICE(ZEROQUATERNION) is signaled by a routine in the call
       tree of this routine.

   13) If the interpolation window size implied by `degree' is not
       even, the error SPICE(INVALIDDEGREE) is signaled by a routine
       in the call tree of this routine. The window size is degree+1
       for Lagrange subtypes and is (degree+1)/2 for Hermite
       subtypes.

   14) If an unrecognized subtype code is supplied, the error
       SPICE(NOTSUPPORTED) is signaled by a routine in the call tree
       of this routine.

   15) If `degree' is not at least 1 or is greater than MAXDEG, the
       error SPICE(INVALIDDEGREE) is signaled by a routine in the
       call tree of this routine.

   16) If the segment descriptor bounds are out of order, the error
       SPICE(BADDESCRTIMES) is signaled by a routine in the call tree
       of this routine.

   17) If there is no element of `sclkdp' that lies between `begtim' and
       `endtim' inclusive, the error SPICE(EMPTYSEGMENT) is signaled by
       a routine in the call tree of this routine.

   18) If `rate' is zero, the error SPICE(INVALIDVALUE) is signaled by
       a routine in the call tree of this routine.

   19) If any of the `ref' or `segid' input string pointers is null,
       the error SPICE(NULLPOINTER) is signaled.

   20) If any of the `ref' or `segid' input strings has zero length,
       the error SPICE(EMPTYSTRING) is signaled.

-Files

   A new type 5 CK segment is written to the CK file attached
   to handle.

-Particulars

   This routine writes a CK type 5 data segment to the open CK
   file according to the format described in the type 5 section of
   the CK Required Reading. The CK file must have been opened with
   write access.


   Quaternion Styles
   -----------------

   There are different "styles" of quaternions used in
   science and engineering applications. Quaternion styles
   are characterized by

   -  The order of quaternion elements

   -  The quaternion multiplication formula

   -  The convention for associating quaternions
      with rotation matrices

   Two of the commonly used styles are

      - "SPICE"

         > Invented by Sir William Rowan Hamilton
         > Frequently used in mathematics and physics textbooks

      - "Engineering"

         > Widely used in aerospace engineering applications


   CSPICE function interfaces ALWAYS use SPICE quaternions.
   Quaternions of any other style must be converted to SPICE
   quaternions before they are passed to CSPICE functions.


   Relationship between SPICE and Engineering Quaternions
   ------------------------------------------------------

   Let M be a rotation matrix such that for any vector V,

      M*V

   is the result of rotating V by theta radians in the
   counterclockwise direction about unit rotation axis vector A.
   Then the SPICE quaternions representing M are

      (+/-) (  cos(theta/2),
               sin(theta/2) A(1),
               sin(theta/2) A(2),
               sin(theta/2) A(3)  )

   while the engineering quaternions representing M are

      (+/-) ( -sin(theta/2) A(1),
              -sin(theta/2) A(2),
              -sin(theta/2) A(3),
               cos(theta/2)       )

   For both styles of quaternions, if a quaternion q represents
   a rotation matrix M, then -q represents M as well.

   Given an engineering quaternion

      QENG   = ( q0,  q1,  q2,  q3 )

   the equivalent SPICE quaternion is

      QSPICE = ( q3, -q0, -q1, -q2 )


   Associating SPICE Quaternions with Rotation Matrices
   ----------------------------------------------------

   Let FROM and TO be two right-handed reference frames, for
   example, an inertial frame and a spacecraft-fixed frame. Let the
   symbols

      V    ,   V
       FROM     TO

   denote, respectively, an arbitrary vector expressed relative to
   the FROM and TO frames. Let M denote the transformation matrix
   that transforms vectors from frame FROM to frame TO; then

      V   =  M * V
       TO         FROM

   where the expression on the right hand side represents left
   multiplication of the vector by the matrix.

   Then if the unit-length SPICE quaternion q represents M, where

      q = (q0, q1, q2, q3)

   the elements of M are derived from the elements of q as follows:

        +-                                                         -+
        |           2    2                                          |
        | 1 - 2*( q2 + q3 )   2*(q1*q2 - q0*q3)   2*(q1*q3 + q0*q2) |
        |                                                           |
        |                                                           |
        |                               2    2                      |
    M = | 2*(q1*q2 + q0*q3)   1 - 2*( q1 + q3 )   2*(q2*q3 - q0*q1) |
        |                                                           |
        |                                                           |
        |                                                   2    2  |
        | 2*(q1*q3 - q0*q2)   2*(q2*q3 + q0*q1)   1 - 2*( q1 + q2 ) |
        |                                                           |
        +-                                                         -+

   Note that substituting the elements of -q for those of q in the
   right hand side leaves each element of M unchanged; this shows
   that if a quaternion q represents a matrix M, then so does the
   quaternion -q.

   To map the rotation matrix M to a unit quaternion, we start by
   decomposing the rotation matrix as a sum of symmetric
   and skew-symmetric parts:

                                      2
      M = [ I  +  (1-cos(theta)) OMEGA  ] + [ sin(theta) OMEGA ]

                   symmetric                   skew-symmetric


   OMEGA is a skew-symmetric matrix of the form

                 +-             -+
                 |  0   -n3   n2 |
                 |               |
       OMEGA  =  |  n3   0   -n1 |
                 |               |
                 | -n2   n1   0  |
                 +-             -+

   The vector N of matrix entries (n1, n2, n3) is the rotation axis
   of M and theta is M's rotation angle. Note that N and theta
   are not unique.

   Let

      C = cos(theta/2)
      S = sin(theta/2)

   Then the unit quaternions Q corresponding to M are

      Q = +/- ( C, S*n1, S*n2, S*n3 )

   The mappings between quaternions and the corresponding rotations
   are carried out by the CSPICE routines

      q2m_c {quaternion to matrix}
      m2q_c {matrix to quaternion}

   m2q_c always returns a quaternion with scalar part greater than
   or equal to zero.


   SPICE Quaternion Multiplication Formula
   ---------------------------------------

   Given a SPICE quaternion

      Q = ( q0, q1, q2, q3 )

   corresponding to rotation axis A and angle theta as above, we can
   represent Q using "scalar + vector" notation as follows:

      s =   q0           = cos(theta/2)

      v = ( q1, q2, q3 ) = sin(theta/2) * A

      Q = s + v

   Let Q1 and Q2 be SPICE quaternions with respective scalar
   and vector parts s1, s2 and v1, v2:

      Q1 = s1 + v1
      Q2 = s2 + v2

   We represent the dot product of v1 and v2 by

      <v1, v2>

   and the cross product of v1 and v2 by

      v1 x v2

   Then the SPICE quaternion product is

      Q1*Q2 = s1*s2 - <v1,v2>  + s1*v2 + s2*v1 + (v1 x v2)

   If Q1 and Q2 represent the rotation matrices M1 and M2
   respectively, then the quaternion product

      Q1*Q2

   represents the matrix product

      M1*M2

-Examples

   This example code fragment writes a type 5 C-kernel segment
   for the Mars Express spacecraft bus to a previously opened CK
   file attached to handle.

      /.
      Include CSPICE interface definitions.
      ./
      #include "SpiceUsr.h"
                .
                .
                .
      /.
      Assume arrays of quaternions, angular velocities, and the
      associated SCLK times are produced elsewhere. The software
      that calls ckw05_c must then decide how to partition these
      pointing instances into intervals over which linear
      interpolation between adjacent points is valid.
      ./
                 .
                 .
                 .

      /.
      The subroutine ckw05_c needs the following items for the
      segment descriptor:

         1) SCLK limits of the segment.
         2) Instrument code.
         3) Reference frame.
         4) The angular velocity flag.

      ./

      begtim = SCLK [      0 ];
      endtim = SCLK [ nrec-1 ];

      inst   =  -41000;
      ref    =  "J2000";
      avflag =  SPICETRUE;

      segid  = "MEX spacecraft bus - data type 5";

      /.
      Write the segment.
      ./
      ckw05_c ( handle,  subtyp,  degree,  begtim,  endtim,  inst,
                ref,     avflag,  segid,   n,       sclkdp,  packts,
                rate,    nints,   starts                             );
                .
                .
                .
      /.
      After all segments are written, close the C-kernel.
      ./
      ckcls_c ( handle );

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   K.R. Gehringer      (JPL)
   J.M. Lynch          (JPL)
   W.L. Taber          (JPL)

-Version

   -CSPICE Version 2.0.1, 10-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 2.0.0, 01-JUN-2010 (NJB)

       The check for non-unit quaternions has been replaced
       with a check for zero-length quaternions. (The
       implementation of the check is located in ckw05_.)

   -CSPICE Version 1.0.2, 27-FEB-2008 (NJB)

       Updated header; added information about SPICE
       quaternion conventions.

   -CSPICE Version 1.0.1, 07-JAN-2005 (NJB)

       Description in -Detailed_Input header section of
       constraints on BEGTIM and ENDTIM was corrected

   -CSPICE Version 1.0.0, 30-AUG-2002 (NJB) (WLT) (KRG) (JML)

-Index_Entries

   write CK type_5 data segment

-&
*/

{ /* Begin ckw05_c */


   /*
   Local variables
   */
   logical                 avf;

   SpiceInt                locSubtype;




   /*
   Participate in error tracingx.
   */
   if ( return_c() )
   {
      return;
   }
   chkin_c ( "ckw05_c" );


   /*
   Check the input strings to make sure the pointers
   are non-null and the string lengths are non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "ckw05_c", ref   );
   CHKFSTR ( CHK_STANDARD, "ckw05_c", segid );


   /*
   Get a type logical copy of the a.v. flag.  Get a type SpiceInt
   copy of the CK type 5 subtype.
   */
   avf        = (logical)  avflag;

   locSubtype = (SpiceInt) subtyp;


   /*
   Write the segment.  Note that the packet array
   DOES NOT require transposition!
   */
   ckw05_( ( integer    * ) &handle,
           ( integer    * ) &locSubtype,
           ( integer    * ) &degree,
           ( doublereal * ) &begtim,
           ( doublereal * ) &endtim,
           ( integer    * ) &inst,
           ( char       * ) ref,
           ( logical    * ) &avf,
           ( char       * ) segid,
           ( integer    * ) &n,
           ( doublereal * ) sclkdp,
           ( doublereal * ) packts,
           ( doublereal * ) &rate,
           ( integer    * ) &nints,
           ( doublereal * ) starts,
           ( ftnlen       ) strlen(ref),
           ( ftnlen       ) strlen(segid)  );


   chkout_c ( "ckw05_c" );

} /* End ckw05_c */
