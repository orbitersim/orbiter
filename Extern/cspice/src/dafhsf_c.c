/*

-Procedure dafhsf_c ( DAF, handle to summary format )

-Abstract

   Return the summary format associated with a handle.

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

   DAF

-Keywords

   CONVERSION
   DAF
   FILES

*/
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"


   void dafhsf_c ( SpiceInt            handle,
                   SpiceInt          * nd,
                   SpiceInt          * ni      )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     I   Handle of a DAF file.
   nd         O   Number of double precision components in summaries.
   ni         O   Number of integer components in summaries.

-Detailed_Input

   handle      is the handle associated with a previously opened
               DAF file.

-Detailed_Output

   nd,
   ni          are the numbers of double precision and integer
               components, respectively, in each array summary
               in the specified file.

-Parameters

   None.

-Exceptions

   1)  If the specified handle does not belong to any file that is
       currently known to be open, the error SPICE(DAFNOSUCHHANDLE)
       is signaled by a routine in the call tree of this routine.

-Files

   None.

-Particulars

   The summary format must be known in order to pack or unpack
   an array summary. See the DAF Required Reading for a discussion
   of summary formats.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Find the number of d.p. `words' in a DAF having an
      arbitrary summary format.

      Use the SPK kernel below as input DAF file for the program.

         de421.bsp


      Example code begins here.


      /.
         Program dafhsf_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Count the number of d.p. words of data in a
         `daf'.  Exclude array summaries, reserved records,
         the file record, and character records.
         ./
         #define MAXND        124
         #define MAXNI        250
         #define MAXSUM       125

         /.
         Local variables.
         ./
         SpiceChar          * daf;

         SpiceDouble          dc     [MAXND];
         SpiceDouble          sum    [MAXSUM];

         SpiceInt             fa;
         SpiceInt             handle;
         SpiceInt             ia;
         SpiceInt             ic     [MAXNI];
         SpiceInt             n;
         SpiceInt             nd;
         SpiceInt             ni;

         SpiceBoolean         found;

         daf = "de421.bsp";

         /.
         Open the `daf' and find the summary format.
         ./
         dafopr_c ( daf, &handle );
         dafhsf_c ( handle, &nd, &ni );

         /.
         Start a forward search and examine each array in
         turn.
         ./
         dafbfs_c ( handle );
         daffna_c ( &found );

         n = 0;
         while ( found )
         {

            /.
            Obtain the array summary, unpack it, and get
            the initial and final array addresses from
            the integer descriptor component.
            ./
            dafgs_c ( sum );
            dafus_c ( sum, nd, ni, dc, ic );

            ia  =  ic[ ni - 2];
            fa  =  ic[ ni - 1];

            n   =  fa - ia + 1 + n;

            daffna_c ( &found );

         }

         printf( "Number of d.p. words is  %d\n", n );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Number of d.p. words is  2098004


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.0, 09-JUL-2021 (JDR)

-Index_Entries

   handle to DAF summary format

-&
*/

{ /* Begin dafhsf_c */

   /*
   Participate in error tracing.
   */
   chkin_c ( "dafhsf_c" );

   /*
   Call the f2c'd Fortran routine.
   */
   dafhsf_ (  ( integer    * ) &handle,
              ( integer    * )  nd,
              ( integer    * )  ni     );

   chkout_c ( "dafhsf_c" );

} /* End dafhsf_c */
