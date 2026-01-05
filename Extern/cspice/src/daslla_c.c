/*

-Procedure daslla_c ( DAS, last logical addresses )

-Abstract

   Return last DAS logical addresses of character, double precision
   and integer type that are currently in use in a specified DAS
   file.

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

   DAS

-Keywords

   ARRAY
   DAS
   UTILITY

*/
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"

   void daslla_c ( SpiceInt            handle,
                   SpiceInt          * lastc,
                   SpiceInt          * lastd,
                   SpiceInt          * lasti  )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     I   DAS file handle.
   lastc      O   Last character address in use.
   lastd      O   Last double precision address in use.
   lasti      O   Last integer address in use.

-Detailed_Input

   handle      is the file handle of a DAS file whose active
               logical address ranges are desired.

-Detailed_Output

   lastc,
   lastd,
   lasti       are, respectively, the last 1-based logical addresses of
               character, double precision, and integer type in use in
               the specified DAS file.

-Parameters

   None.

-Exceptions

   1)  If the input file handle is invalid, an error is signaled by
       a routine in the call tree of this routine.

-Files

   None.

-Particulars

   This routine is a utility that allows a calling program to
   find the range of logical addresses currently in use in any
   DAS file.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Create a DAS file containing 10 integers, 5 double precision
      numbers, and 4 characters, then use daslla_c to find the logical
      address ranges in use.


      Example code begins here.


      /.
         Program daslla_ex1
      ./
      #include <stdio.h>
      #include <string.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters.
         ./
         #define FNAME        "daslla_ex1.das"

         /.
         Local variables.
         ./
         ConstSpiceChar     * ifname;
         ConstSpiceChar     * type;

         SpiceDouble          dbli;

         SpiceInt             handle;
         SpiceInt             i;
         SpiceInt             lastc;
         SpiceInt             lastd;
         SpiceInt             lasti;

         /.
         Open a new DAS file. Use the file name as the internal
         file name, and reserve no records for comments.
         ./
         type   = "TEST";
         ifname = "TEST.DAS/NAIF/NJB/11-NOV-1992-20:12:20";

         dasonw_c ( FNAME, type, ifname, 0, &handle );

         for ( i = 1; i <= 10; i++ )
         {
            dasadi_c ( handle, 1, &i );
         }

         for ( i = 1; i <= 5; i++ )
         {
            dbli = (double)i;
            dasadd_c ( handle, 1, &dbli );
         }

         dasadc_c ( handle, 4, 0, 3, 5, "SPUD" );

         /.
         Now check the logical address ranges.
         ./
         daslla_c ( handle, &lastc, &lastd, &lasti );

         printf( "Last character address in use: %d\n", lastc );
         printf( "Last d.p. address in use     : %d\n", lastd );
         printf( "Last integer address in use  : %d\n", lasti );

         /.
         Close the DAS file.
         ./
         dascls_c ( handle );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Last character address in use: 4
      Last d.p. address in use     : 5
      Last integer address in use  : 10


      Note that after run completion, a new DAS file exists in the
      output directory.

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.0, 30-JUN-2021 (JDR)

-Index_Entries

   return last logical addresses in DAS file
   return logical address range of DAS file

-&
*/

{ /* Begin daslla_c */

   /*
   Participate in error tracing.
   */
   chkin_c ( "daslla_c" );

   /*
   Call the f2c'd Fortran routine.
   */
   daslla_ (  ( integer    * ) &handle,
              ( integer    * )  lastc,
              ( integer    * )  lastd,
              ( integer    * )  lasti  );

   chkout_c ( "daslla_c" );

} /* End daslla_c */
