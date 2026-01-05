/*

-Procedure vupack_c ( Unpack three scalar components from a vector )

-Abstract

   Unpack three scalar components from a vector.

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

   VECTOR

*/

   #include "SpiceUsr.h"
   #undef   vupack_c


   void vupack_c ( ConstSpiceDouble     v[3],
                   SpiceDouble        * x,
                   SpiceDouble        * y,
                   SpiceDouble        * z     )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   v          I   Input 3-dimensional vector.
   x,
   y,
   z          O   Scalar components of the vector.

-Detailed_Input

   v           is a double precision 3-dimensional vector.

-Detailed_Output

   x,
   y,
   z           are the double precision scalar components of the
               vector `v'. The following equalities hold:

                  v[0] = x
                  v[1] = y
                  v[2] = z

-Parameters

   None.

-Exceptions

   Error free.

-Files

   None.

-Particulars

   Basically, this is just shorthand notation for the common
   sequence

      x = v[0];
      y = v[1];
      z = v[2];

   The routine is useful largely for two reasons. First, it
   reduces the chance that the programmer will make a "cut and
   paste" mistake, like

      x = v[0];
      y = v[0];
      z = v[0];

   Second, it makes conversions between equivalent units simpler,
   and clearer. For instance, the sequence

      x = v[0] * rpd_c();
      y = v[1] * rpd_c();
      z = v[2] * rpd_c();

   can be replaced by the (nearly) equivalent sequence

      vscl_c   ( rpd_c(),  v,   v  );
      vupack_c ( v,  &x,  &y,  &z  );

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Suppose that you have an instrument kernel that provides,
      within a single keyword, the three frequencies used by the
      instrument, and that you want to use these frequencies
      independently within your code.

      The following code example demonstrates how to use vupack_c
      to get these frequencies into independent scalar variables.

      Use the kernel shown below, an IK defining the three
      frequencies used by an instrument with NAIF ID -999001.


         KPL/IK

         File name: vupack_ex1.ti

         The keyword below define the three frequencies used by a
         hypothetical instrument (NAIF ID -999001). They correspond
         to three filters: red, green and blue. Frequencies are
         given in micrometers.

         \begindata

            INS-999001_FREQ_RGB   = (  0.65,  0.55, 0.475 )
            INS-999001_FREQ_UNITS = ( 'MICROMETERS'       )

         \begintext


         End of IK


      Example code begins here.


      /.
         Program vupack_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters.
         ./
         #define IKNAME       "vupack_ex1.ti"
         #define KEYWRD       "INS-999001_FREQ_RGB"

         /.
         Local variables.
         ./
         SpiceDouble          ddata  [3];
         SpiceDouble          red;
         SpiceDouble          green;
         SpiceDouble          blue;

         SpiceInt             n;

         SpiceBoolean         found;

         /.
         Load the instrument kernel.
         ./
         furnsh_c ( IKNAME );

         /.
         Get the frequency data from the kernel pool.
         ./
         gdpool_c ( KEYWRD, 0, 3, &n, ddata, &found );

         if ( found )
         {
            vupack_c ( ddata, &red, &green, &blue );
            printf( "Blue  (nm):  %5.2f\n", blue  * 1000.0 );
            printf( "Green (nm):  %5.2f\n", green * 1000.0 );
            printf( "Red   (nm):  %5.2f\n", red   * 1000.0 );
         }
         else
         {
            printf( "No data found in the kernel pool for %s\n", KEYWRD );
         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Blue  (nm):  475.00
      Green (nm):  550.00
      Red   (nm):  650.00


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   I.M. Underwood      (JPL)

-Version

   -CSPICE Version 1.0.1, 03-SEP-2020 (JDR)

       Edited the header to comply with NAIF standard. Added complete
       code example.

   -CSPICE Version 1.0.0, 07-NOV-2006 (NJB)

       Corrected header errors that claimed this routine
       performs the function of vpack_c.

   -CSPICE Version 1.0.0, 28-JUN-1999 (IMU) (NJB)

-Index_Entries

   unpack three scalar components from a vector

-&
*/

{ /* Begin vupack_c */


   *x = v[0];
   *y = v[1];
   *z = v[2];


} /* End vupack_c */
