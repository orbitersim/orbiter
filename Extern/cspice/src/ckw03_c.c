/*

-Procedure ckw03_c ( C-Kernel, write segment to C-kernel, data type 3 )

-Abstract

   Add a type 3 segment to a C-kernel.

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
   DAF
   SCLK

-Keywords

   POINTING
   UTILITY

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"
   #include "SpiceZim.h"
   #undef    ckw03_c


   void ckw03_c ( SpiceInt            handle,
                  SpiceDouble         begtim,
                  SpiceDouble         endtim,
                  SpiceInt            inst,
                  ConstSpiceChar    * ref,
                  SpiceBoolean        avflag,
                  ConstSpiceChar    * segid,
                  SpiceInt            nrec,
                  ConstSpiceDouble    sclkdp [],
                  ConstSpiceDouble    quats  [][4],
                  ConstSpiceDouble    avvs   [][3],
                  SpiceInt            nints,
                  ConstSpiceDouble    starts []    )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     I   Handle of an open CK file.
   begtim     I   The beginning encoded SCLK of the segment.
   endtim     I   The ending encoded SCLK of the segment.
   inst       I   The NAIF instrument ID code.
   ref        I   The reference frame of the segment.
   avflag     I   SPICETRUE if the segment will contain angular velocity.
   segid      I   Segment identifier.
   nrec       I   Number of pointing records.
   sclkdp     I   Encoded SCLK times.
   quats      I   Quaternions representing instrument pointing.
   avvs       I   Angular velocity vectors.
   nints      I   Number of intervals.
   starts     I   Encoded SCLK interval start times.

-Detailed_Input

   handle      is the handle of the CK file to which the segment will
               be written. The file must have been opened with write
               access.

   begtim      is the beginning encoded SCLK time of the segment. This
               value should be less than or equal to the first time in
               the segment.

   endtim      is the encoded SCLK time at which the segment ends.
               This value should be greater than or equal to the last
               time in the segment.

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

   nrec        is the number of pointing instances in the segment.

   sclkdp      are the encoded spacecraft clock times associated with
               each pointing instance. These times must be strictly
               increasing.

   quats       is an array of SPICE-style quaternions representing a
               sequence of C-matrices. See the discussion of "Quaternion
               Styles" in the -Particulars section below.

               The C-matrix represented by the ith quaternion in
               quats is a rotation matrix that transforms the
               components of a vector expressed in the inertial
               frame specified by ref to components expressed in
               the instrument fixed frame at the time sclkdp[i].

               Thus, if a vector V has components x, y, z in the
               inertial frame, then V has components x', y', z' in
               the instrument fixed frame where:

                    [ x' ]     [          ] [ x ]
                    | y' |  =  |   cmat   | | y |
                    [ z' ]     [          ] [ z ]

   avvs        are the angular velocity vectors ( optional ).

               The ith vector in avvs gives the angular velocity of
               the instrument fixed frame at time sclkdp[i]. The
               components of the angular velocity vectors should
               be given with respect to the inertial reference frame
               specified by ref.

               The direction of an angular velocity vector gives
               the right-handed axis about which the instrument fixed
               reference frame is rotating. The magnitude of the
               vector is the magnitude of the instantaneous velocity
               of the rotation, in radians per second.

               If avflag is SPICEFALSE then this array is ignored by the
               routine; however it still must be supplied as part of
               the calling sequence.

   nints       is the number of intervals that the pointing instances
               are partitioned into.

   starts      are the start times of each of the interpolation
               intervals. These times must be strictly increasing
               and must coincide with times for which the segment
               contains pointing.

-Detailed_Output

   None. See -Files section.

-Parameters

   None.

-Exceptions

   1)  If `handle' is not the handle of a C-kernel opened for writing,
       an error is signaled by a routine in the call tree of this
       routine.

   2)  If `segid' is more than 40 characters long, the error
       SPICE(SEGIDTOOLONG) is signaled by a routine in the call tree
       of this routine.

   3)  If `segid' contains any non-printable characters, the error
       SPICE(NONPRINTABLECHARS) is signaled by a routine in the call
       tree of this routine.

   4)  If the first encoded SCLK time is negative, the error
       SPICE(INVALIDSCLKTIME) is signaled by a routine in the call
       tree of this routine.

   5)  If the second encoded SCLK or any subsequent times, or if the
       encoded SCLK times are not strictly increasing, the error
       SPICE(TIMESOUTOFORDER) is signaled by a routine in the call
       tree of this routine.

   6)  If `begtim' is greater than sclkdp[0] or `endtim' is less than
       sclkdp[nrec-1], the error SPICE(INVALIDDESCRTIME) is signaled by
       a routine in the call tree of this routine.

   7)  If the name of the reference frame is not one of those
       supported by the CSPICE routine namfrm_c, the error
       SPICE(INVALIDREFFRAME) is signaled by a routine in the call
       tree of this routine.

   8)  If `nrec', the number of pointing records, is less than or equal
       to 0, the error SPICE(INVALIDNUMREC) is signaled by a routine
       in the call tree of this routine.

   9)  If `nints', the number of interpolation intervals, is less than
       or equal to 0, the error SPICE(INVALIDNUMINT) is signaled by a
       routine in the call tree of this routine.

   10) If the encoded SCLK interval start times are not strictly
       increasing, the error SPICE(TIMESOUTOFORDER) is signaled by a
       routine in the call tree of this routine.

   11) If an interval start time does not coincide with a time for
       which there is an actual pointing instance in the segment, the
       error SPICE(INVALIDSTARTTIME) is signaled by a routine in the
       call tree of this routine.

   12) This routine assumes that the rotation between adjacent
       quaternions that are stored in the same interval has a
       rotation angle of `theta' radians, where

          0  <=  theta  <  pi.

       The routines that evaluate the data in the segment produced
       by this routine cannot distinguish between rotations of `theta'
       radians, where `theta' is in the interval [0, pi), and
       rotations of

          theta   +   2 * k * pi

       radians, where k is any integer. These `large' rotations
       will yield invalid results when interpolated. You must
       ensure that the data stored in the segment will not be
       subject to this sort of ambiguity.

   13) If any quaternion has magnitude zero, the error
       SPICE(ZEROQUATERNION) is signaled by a routine in the call
       tree of this routine.

   14) If the start time of the first interval and the time of the
       first pointing instance are not the same, the error
       SPICE(TIMESDONTMATCH) is signaled by a routine in the call
       tree of this routine.

   15) If any of the `ref' or `segid' input string pointers is null,
       the error SPICE(NULLPOINTER) is signaled.

   16) If any of the `ref' or `segid' input strings has zero length,
       the error SPICE(EMPTYSTRING) is signaled.

-Files

   This routine adds a type 3 segment to a C-kernel. The C-kernel
   may be either a new one or an existing one opened for writing.

-Particulars

   For a detailed description of a type 3 CK segment please see the
   CK Required Reading.

   This routine relieves the user from performing the repetitive
   calls to the DAF routines necessary to construct a CK segment.


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

        .-                                                         -.
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
        `-                                                         -.

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

                 .-             -.
                 |  0   -n3   n2 |
                 |               |
       OMEGA  =  |  n3   0   -n1 |
                 |               |
                 | -n2   n1   0  |
                 `-             -'

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

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Create a CK type 3 segment; fill with data for a simple time
      dependent rotation and angular velocity.

      Example code begins here.


      /.
         Program ckw03_ex1
      ./
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters.
         ./
         #define CK3          "ckw03_ex1.bc"
         #define SPTICK       0.001
         #define INST         -77703
         #define MAXREC       201

         /.
         Local variables.
         ./
         SpiceChar          * ref;
         SpiceChar          * ifname;
         SpiceChar          * segid;

         SpiceDouble          avvs   [MAXREC][3];
         SpiceDouble          begtim;
         SpiceDouble          endtim;
         SpiceDouble          quats  [MAXREC][4];
         SpiceDouble          rate;
         SpiceDouble          rwmat  [3][3];
         SpiceDouble          spaces;
         SpiceDouble          sclkdp [MAXREC];
         SpiceDouble          starts [MAXREC/2];
         SpiceDouble          sticks;
         SpiceDouble          theta;
         SpiceDouble          wmat   [3][3];
         SpiceDouble          wquat  [4];

         SpiceInt             handle;
         SpiceInt             i;
         SpiceInt             ncomch;
         SpiceInt             nints;

         SpiceBoolean         avflag;

         /.
         `ncomch' is the number of characters to reserve for the
         kernel's comment area. This example doesn't write
         comments, so set to zero.
         ./
         ncomch = 0;

         /.
         The base reference from for the rotation data.
         ./
         ref = "J2000";

         /.
         Time spacing in encoded ticks and in seconds
         ./
         sticks = 10.0;
         spaces = sticks * SPTICK;

         /.
         Declare an angular rate in radians per sec.
         ./
         rate = 1.e-2;

         /.
         Internal file name and segment ID.
         ./
         segid  = "Test type 3 CK segment";
         ifname = "Test CK type 3 segment created by ckw03_c";

         /.
         Open a new kernel.
         ./
         ckopn_c ( CK3, ifname, ncomch, &handle );

         /.
         Create a 3x3 double precision identity matrix.
         ./
         ident_c ( wmat );

         /.
         Convert the matrix to quaternion.
         ./
         m2q_c ( wmat, wquat );

         /.
         Copy the work quaternion to the first row of
         `quats'.
         ./
         moved_c ( wquat, 4, quats[0] );

         /.
         Create an angular velocity vector. This vector is in the
         `ref' reference frame and indicates a constant rotation
         about the Z axis.
         ./
         vpack_c ( 0.0, 0.0, rate, avvs[0] );

         /.
         Set the initial value of the encoded ticks.
         ./
         sclkdp[0] = 1000.0;

         /.
         Fill the rest of the `avvs' and `quats' matrices
         with simple data.
         ./
         for ( i = 1; i < MAXREC; i++ )
         {

            /.
            Create the corresponding encoded tick value in
            increments of `sticks' with an initial value of
            1000.0 ticks.
            ./
            sclkdp[i] = 1000.0 + i * sticks;

            /.
            Create the transformation matrix for a rotation of
            `theta' about the Z axis. Calculate `theta' from the
            constant angular rate `rate' at increments of `spaces'.
            ./
            theta = i * rate * spaces;
            rotmat_c ( wmat, theta, 3, rwmat );

            /.
            Convert the `rwmat' matrix to SPICE type quaternion.
            ./
            m2q_c ( rwmat, wquat );

            /.
            Store the quaternion in the `quats' matrix.
            Store angular velocity in `avvs'.
            ./
            moved_c ( wquat, 4, quats[i] );
            vpack_c ( 0.0, 0.0, rate, avvs[i] );

         }

         /.
         Create an array start times for the interpolation
         intervals. The end time for a particular interval is
         determined as the time of the final data value prior in
          time to the next start time.
         ./
         nints = MAXREC/2;
         for ( i = 0; i < nints; i++ )
         {

            starts[i] = sclkdp[i*2];

         }

         /.
         Set the segment boundaries equal to the first and last
         time for the data arrays.
         ./
         begtim = sclkdp[0];
         endtim = sclkdp[MAXREC-1];

         /.
         This segment contains angular velocity.
         ./
         avflag = SPICETRUE;

         /.
         All information ready to write. Write to a CK type 3
         segment to the file indicated by `handle'.
         ./
         ckw03_c ( handle, begtim, endtim, INST, ref,   avflag, segid,
                   MAXREC, sclkdp, quats,  avvs, nints, starts       );

         /.
         SAFELY close the file.
         ./
         ckcls_c ( handle );

         return ( 0 );
      }


      When this program is executed, no output is presented on
      screen. After run completion, a new CK file exists in the
      output directory.

-Restrictions

   1)  The creator of the segment is given the responsibility for
       determining whether it is reasonable to interpolate between
       two given pointing values.

   2)  This routine assumes that the rotation between adjacent
       quaternions that are stored in the same interval has a
       rotation angle of THETA radians, where

           0  <=  THETA  <  pi.

       The routines that evaluate the data in the segment produced
       by this routine cannot distinguish between rotations of THETA
       radians, where THETA is in the interval [0, pi), and
       rotations of

           THETA   +   2 * k * pi

       radians, where k is any integer. These "large" rotations will
       yield invalid results when interpolated. You must ensure that
       the data stored in the segment will not be subject to this
       sort of ambiguity.

   3)  All pointing instances in the segment must belong to one and
       only one of the intervals.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   B.V. Semenov        (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 2.0.1, 10-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard. Created
       complete code example from existing fragment.

   -CSPICE Version 2.0.0, 01-JUN-2010 (NJB)

       The check for non-unit quaternions has been replaced
       with a check for zero-length quaternions. (The
       implementation of the check is located in ckw03_.)

   -CSPICE Version 1.4.2, 27-FEB-2008 (NJB)

       Updated header; added information about SPICE
       quaternion conventions.

   -CSPICE Version 1.4.1, 27-SEP-2005 (BVS)

       Added an item for SPICE(TIMESDONTMATCH) exception to the
       -Exceptions section of the header.

   -CSPICE Version 1.3.1, 07-JAN-2004 (EDW)

       Trivial typo correction in index entries section.

   -CSPICE Version 1.3.0, 28-AUG-2001 (NJB)

       Changed prototype: inputs  sclkdp, quats, avvs, and starts
       are now const-qualified. Implemented interface macros for
       casting these inputs to const.

   -CSPICE Version 1.2.0, 02-SEP-1999 (NJB)

       Local type logical variable now used for angular velocity
       flag used in interface of ckw03_.

   -CSPICE Version 1.1.0, 08-FEB-1998 (NJB)

       References to C2F_CreateStr_Sig were removed; code was
       cleaned up accordingly. String checks are now done using
       the macro CHKFSTR.

   -CSPICE Version 1.0.0, 25-OCT-1997 (NJB)

       Based on SPICELIB Version 2.0.0, 28-DEC-1993 (WLT)

-Index_Entries

   write CK type_3 pointing data segment

-&
*/

{ /* Begin ckw03_c */



   /*
   Local variables
   */
   logical                 avf;


   /*
   Participate in error handling.
   */
   chkin_c ( "ckw03_c" );


   /*
   Check the input strings to make sure the pointers
   are non-null and the string lengths are non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "ckw03_c", ref   );
   CHKFSTR ( CHK_STANDARD, "ckw03_c", segid );


   /*
   Get a type logical copy of the a.v. flag.
   */
   avf = avflag;


   /*
   Write the segment.  Note that the quaternion and angular velocity
   arrays DO NOT require transposition!
   */

   ckw03_( ( integer    * ) &handle,
           ( doublereal * ) &begtim,
           ( doublereal * ) &endtim,
           ( integer    * ) &inst,
           ( char       * ) ref,
           ( logical    * ) &avf,
           ( char       * ) segid,
           ( integer    * ) &nrec,
           ( doublereal * ) sclkdp,
           ( doublereal * ) quats,
           ( doublereal * ) avvs,
           ( integer    * ) &nints,
           ( doublereal * ) starts,
           ( ftnlen       ) strlen(ref),
           ( ftnlen       ) strlen(segid)  );


   chkout_c ( "ckw03_c" );

} /* End ckw03_c */
