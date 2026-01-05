/*

-Procedure eknseg_c ( EK, number of segments in file )

-Abstract

   Return the number of segments in a specified EK.

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

   EK

-Keywords

   EK
   UTILITY

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"


   SpiceInt eknseg_c ( SpiceInt handle )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     I   EK file handle.

   The function returns the number of segments in the specified
   E-kernel.

-Detailed_Input

   handle      is the handle of an EK file opened for read access.

-Detailed_Output

   The function returns the number of segments in the EK identified
   by `handle'.

-Parameters

   None.

-Exceptions

   1)  If `handle' is invalid, an error is signaled by a routine in the
       call tree of this routine. eknseg_c will return the value zero.

   2)  If an I/O error occurs while trying to read the EK, the error
       is signaled by a routine in the call tree of this routine.
       eknseg_c will return the value zero.

-Files

   See the description of handle in -Detailed_Input.

-Particulars

   This routine is used to support the function of summarizing an
   EK file. Given the number of segments in the file, a program
   can use ekssum_c in a loop to summarize each of them.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Find the number of segments on an EK.

      Use the EK kernel below as test input file for loading the
      experiment database. This kernel contains the Deep
      Impact spacecraft sequence data based on the integrated
      Predicted Events File covering the whole primary mission.

         dif_seq_050112_050729.bes


      Example code begins here.


      /.
         Program eknseg_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local variables.
         ./
         SpiceInt             handle;
         SpiceInt             nseg;

         /.
         Open the EK file, returning the file handle
         associated with the open file to the variable named
         `handle'.
         ./
         ekopr_c ( "dif_seq_050112_050729.bes", &handle );

         /.
         Return the number of segments in the EK.
         ./
         nseg = eknseg_c ( handle );
         printf( "Number of segments =  %d\n", nseg );

         /.
         Close the file.
         ./
         ekcls_c ( handle );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Number of segments =  2


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.1, 10-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard. Added
       complete code example.

   -CSPICE Version 1.0.0, 24-FEB-1999 (NJB)

-Index_Entries

   return number of segments in an E-kernel

-&
*/

{ /* Begin eknseg_c */

   /*
   Local variables
   */
   SpiceInt                n;


   /*
   Participate in error tracing.
   */

   chkin_c ( "eknseg_c" );

   /*
   We capture the value returned by eknseg_ rather than return it
   directly, so we can check out.
   */

   n  =  eknseg_ ( (integer *) &handle );


   /*
   Check out here, since it's our last chance.
   */
   chkout_c ( "eknseg_c" );


   return (n);


} /* End eknseg_c */
