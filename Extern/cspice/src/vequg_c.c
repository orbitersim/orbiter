/*

-Procedure vequg_c ( Vector equality, general dimension )

-Abstract

   Make one double precision vector of arbitrary dimension equal
   to another.

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

   None.

-Keywords

   ASSIGNMENT
   VECTOR

*/

   #include "SpiceUsr.h"
   #include "SpiceZmc.h"
   #undef    vequg_c


   void vequg_c ( ConstSpiceDouble  * vin,
                  SpiceInt            ndim,
                  SpiceDouble       * vout )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   vin        I   Double precision n-dimensional vector.
   ndim       I   Dimension of `vin' (and also `vout').
   vout       O   Double precision n-dimensional vector set equal
                  to `vin'.

-Detailed_Input

   vin         is an arbitrary, double precision n-dimensional vector.

   ndim        is the dimension of `vin' and `vout'.

-Detailed_Output

   vout        is a double precision n-dimensional vector set equal
               to `vin'.

-Parameters

   None.

-Exceptions

   Error free.

-Files

   None.

-Particulars

   The code simply sets each component of `vout' equal to the
   corresponding component of `vin'.

   Note that this routine may be used in place of moved_c, which
   sets each output array element equal to the corresponding
   input array element.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Lets assume we have a pointing record that contains the
      start time of an interpolation interval, the components of
      the quaternion that represents the C-matrix associated with
      the start time of the interval, and the angular velocity vector
      of the interval. The following example demonstrates how to
      extract the time, the quaternion and the angular velocity
      vector into separate variables for their processing.


      Example code begins here.


      /.
         Program vequg_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local variables.
         ./
         SpiceDouble          av     [3];
         SpiceDouble          quat   [4];
         SpiceDouble          time;

         /.
         Define the pointing record. We would normally obtain it
         from, e.g. CK readers or other non SPICE data files.
         ./
         SpiceDouble          record [8] = { 283480.753,        0.99999622,
                                                  0.0,          0.0,
                                                 -0.0027499965, 0.0,
                                                  0.0,          0.01 };

         /.
         Get the time, quaternion and angular velocity vector
         into separate variables.
         ./
         time = record[0];

         vequg_c ( record+1, 4, quat );
         vequ_c  ( record+5,    av   );

         /.
         Display the contents of the variables.
         ./
         printf( "Time            : %10.3f\n", time );

         printf( "Quaternion      :\n" );
         printf( "%15.10f %14.10f %14.10f %14.10f\n",
                   quat[0], quat[1], quat[2], quat[3] );
         printf( "Angular velocity:\n" );
         printf( "%15.10f %14.10f %14.10f\n", av[0], av[1], av[2] );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Time            : 283480.753
      Quaternion      :
         0.9999962200   0.0000000000   0.0000000000  -0.0027499965
      Angular velocity:
         0.0000000000   0.0000000000   0.0100000000


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   W.M. Owen           (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.1.0, 23-JUL-2020 (JDR)

       Edited the header to comply with NAIF standard. Added complete
       code example based on existing example.

       Removed check for "ndim" being positive in order to replicate
       behaviour of SPICELIB equivalent routine.

   -CSPICE Version 1.0.0, 23-AUG-1999 (EDW) (NJB) (WMO)

-Index_Entries

   assign an n-dimensional vector to another

-&
*/

{ /* Begin vequg_c */

   /*
   Error free:  no error tracing required.
   */


   /*
   Do the equality thing if `ndim' is positive
   */
   if ( ndim > 0 )
   {
      MOVED ( vin, ndim, vout );
   }

} /* End vequg_c */
