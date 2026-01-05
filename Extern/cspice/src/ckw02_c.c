/*

-Procedure ckw02_c ( C-Kernel, write segment to C-kernel, data type 2 )

-Abstract

   Write a type 2 segment to a C-kernel.

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
   #undef    ckw02_c


   void ckw02_c ( SpiceInt            handle,
                  SpiceDouble         begtim,
                  SpiceDouble         endtim,
                  SpiceInt            inst,
                  ConstSpiceChar    * ref,
                  ConstSpiceChar    * segid,
                  SpiceInt            nrec,
                  ConstSpiceDouble    start  [],
                  ConstSpiceDouble    stop   [],
                  ConstSpiceDouble    quats  [][4],
                  ConstSpiceDouble    avvs   [][3],
                  ConstSpiceDouble    rates  []    )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     I   Handle of an open CK file.
   begtim     I   The beginning encoded SCLK of the segment.
   endtim     I   The ending encoded SCLK of the segment.
   inst       I   The NAIF instrument ID code.
   ref        I   The reference frame of the segment.
   segid      I   Segment identifier.
   nrec       I   Number of pointing records.
   start      I   Encoded SCLK interval start times.
   stop       I   Encoded SCLK interval stop times.
   quats      I   Quaternions representing instrument pointing.
   avvs       I   Angular velocity vectors.
   rates      I   Number of seconds per tick for each interval.

-Detailed_Input

   handle      is the handle of the CK file to which the segment will
               be written. The file must have been opened with write
               access.

   begtim      is the beginning encoded SCLK time of the segment. This
               value should be less than or equal to the first START
               time in the segment.

   endtim      is the encoded SCLK time at which the segment ends.
               This value should be greater than or equal to the last
               STOP time in the segment.

   inst        is the NAIF integer ID code for the instrument.

   ref         is a character string that specifies the
               reference frame of the segment. This should be one of
               the frames supported by the SPICELIB routine NAMFRM
               which is an entry point of FRAMEX.

   segid       is the segment identifier.  A CK segment identifier may
               contain up to 40 characters.

   nrec        is the number of pointing intervals that will be
               written to the segment.

   start       are the start times of each interval in encoded
               spacecraft clock. These times must be strictly
               increasing.

   stop        are the stop times of each interval in encoded
               spacecraft clock. These times must be greater than
               the START times that they correspond to but less
               than or equal to the START time of the next interval.

   quats       are the quaternions representing the C-matrices
               associated with the start times of each interval. See the
               discussion of "Quaternion Styles" in the -Particulars
               section below.

   avvs        are the angular velocity vectors for each interval.

   rates       are the number of seconds per encoded spacecraft clock
               tick for each interval.

               In most applications this value will be the same for
               each interval within a segment. For example, when
               constructing a predict C-kernel for Mars Observer, the
               rate would be 1/256 for each interval since this is
               the smallest time unit expressible by the MO clock. The
               nominal seconds per tick rates for Galileo and Voyager
               are 1/120 and 0.06 respectively.

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

   3)  If `segid' contains any nonprintable characters, the error
       SPICE(NONPRINTABLECHARS) is signaled by a routine in the call
       tree of this routine.

   4)  If the first `start' time is negative, the error
       SPICE(INVALIDSCLKTIME) is signaled by a routine in the call
       tree of this routine.

   5)  If the second or any subsequent `start' times are negative, the
       error SPICE(TIMESOUTOFORDER) is signaled by a routine in the
       call tree of this routine.

   6)  If any of the `stop' times are negative, the error
       SPICE(DEGENERATEINTERVAL) is signaled by a routine in the call
       tree of this routine.

   7)  If the `stop' time of any of the intervals is less than or equal
       to the `start' time, the error SPICE(DEGENERATEINTERVAL) is
       signaled by a routine in the call tree of this routine.

   8)  If the `start' times are not strictly increasing, the error
       SPICE(TIMESOUTOFORDER) is signaled by a routine in the call
       tree of this routine.

   9)  If the `stop' time of one interval is greater than the `start'
       time of the next interval, the error SPICE(BADSTOPTIME)
       is signaled by a routine in the call tree of this routine.

   10) If `begtim' is greater than start[0] or `endtim' is less than
       stop[nrec-1], the error SPICE(INVALIDDESCRTIME) is
       signaled by a routine in the call tree of this routine.

   11) If the name of the reference frame is not one of those
       supported by the routine namfrm_c, the error
       SPICE(INVALIDREFFRAME) is signaled by a routine in the call
       tree of this routine.

   12) If `nrec', the number of pointing records, is less than or equal
       to 0, the error SPICE(INVALIDNUMRECS) is signaled by a routine
       in the call tree of this routine.

   13) If any quaternion has magnitude zero, the error
       SPICE(ZEROQUATERNION) is signaled by a routine in the call
       tree of this routine.

   14) If any of the `ref' or `segid' input string pointers is null,
       the error SPICE(NULLPOINTER) is signaled.

   15) If any of the `ref' or `segid' input strings has zero length,
       the error SPICE(EMPTYSTRING) is signaled.

-Files

   This routine adds a type 2 segment to a C-kernel. The C-kernel
   may be either a new one or an existing one opened for writing.

-Particulars

   For a detailed description of a type 2 CK segment please see the
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
        `-                                                         -'

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

   1) The following example creates a CK file with a type-2 segment,
      with data for a simple time dependent rotation and angular
      velocity.

      Example code begins here.


      /.
         Program ckw02_ex1
      ./
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters.
         ./
         #define CK2          "ckw02_ex1.bc"
         #define SPTICK       0.001
         #define INST         -77702
         #define MAXREC       21

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
         SpiceDouble          rates  [MAXREC];
         SpiceDouble          rwmat  [3][3];
         SpiceDouble          spaces;
         SpiceDouble          starts [MAXREC];
         SpiceDouble          stops  [MAXREC];
         SpiceDouble          sticks;
         SpiceDouble          theta;
         SpiceDouble          wmat   [3][3];
         SpiceDouble          wquat  [4];

         SpiceInt             handle;
         SpiceInt             i;
         SpiceInt             ncomch;

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
         segid  = "Test type 2 CK segment";
         ifname = "Test CK type 2 segment created by ckw02_c";

         /.
         Open a new kernel.
         ./
         ckopn_c ( CK2, ifname, ncomch, &handle );

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
         Set the initial value of the encoded ticks. The interval
         associated with each quaternion will start at the epoch
         of the quaternion and will extend 0.8 * sticks forward in
         time, leaving small gaps between the intervals.

         The clock rates array will have a constant SPTICK value.
         ./
         starts[0] = 1000.0;
         stops[0]  = starts[0] + ( 0.8 * sticks );
         rates[0]  = SPTICK;

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
            starts[i] = 1000.0 + i * sticks;
            stops[i]  = starts[i] + ( 0.8 * sticks );
            rates[i]  = SPTICK;

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
         Set the segment boundaries equal to the first and last
         time for the data arrays.
         ./
         begtim = starts[0];
         endtim = stops[MAXREC-1];

         /.
         All information ready to write. Write to a CK type 2
         segment to the file indicated by `handle'.
         ./
         ckw02_c ( handle, begtim, endtim, INST,  ref,  segid,
                   MAXREC, starts, stops,  quats, avvs, rates );

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

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 2.0.1, 10-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard. Added complete
       code example from existing fragment.

   -CSPICE Version 2.0.0, 01-JUN-2010 (NJB)

       The check for non-unit quaternions has been replaced
       with a check for zero-length quaternions. (The
       implementation of the check is located in ckw02_.)

   -CSPICE Version 1.2.1, 27-FEB-2008 (NJB)

       Updated header; added information about SPICE
       quaternion conventions.

   -CSPICE Version 1.2.0, 28-AUG-2001 (NJB)

       Changed prototype: inputs start, stop, sclkdp, quats,
       and avvs are now const-qualified. Implemented interface
       macros for casting these inputs to const.

   -CSPICE Version 1.1.0, 08-FEB-1998 (NJB)

       References to C2F_CreateStr_Sig were removed; code was
       cleaned up accordingly. String checks are now done using
       the macro CHKFSTR.

   -CSPICE Version 1.0.0, 25-OCT-1997 (NJB)

      Based on SPICELIB Version 2.0.0, 28-DEC-1993 (WLT)

-Index_Entries

   write CK type_2 pointing data segment

-&
*/

{ /* Begin ckw02_c */

   /*
   Participate in error handling.
   */
   chkin_c ( "ckw02_c" );

   /*
   Check the input strings to make sure the pointers
   are non-null and the string lengths are non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "ckw02_c", ref   );
   CHKFSTR ( CHK_STANDARD, "ckw02_c", segid );


   /*
   Write the segment.  Note that the quaternion and angular velocity
   arrays DO NOT require transposition!
   */

   ckw02_( ( integer    * ) &handle,
           ( doublereal * ) &begtim,
           ( doublereal * ) &endtim,
           ( integer    * ) &inst,
           ( char       * ) ref,
           ( char       * ) segid,
           ( integer    * ) &nrec,
           ( doublereal * ) start,
           ( doublereal * ) stop,
           ( doublereal * ) quats,
           ( doublereal * ) avvs,
           ( doublereal * ) rates,
           ( ftnlen       ) strlen(ref),
           ( ftnlen       ) strlen(segid)  );


   chkout_c ( "ckw02_c" );

} /* End ckw02_c */
