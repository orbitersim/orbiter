/*

-Procedure ldpool_c ( Load variables from a kernel file into the pool )

-Abstract

   Load the variables contained in a NAIF ASCII kernel file into the
   kernel pool.

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

   KERNEL

-Keywords

   CONSTANTS
   FILES

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"

   void ldpool_c ( ConstSpiceChar * fname )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   fname      I   Name of the text kernel file.

-Detailed_Input

   fname       is the name of the text kernel file whose variables will
               be loaded into the pool.

-Detailed_Output

   None.

-Parameters

   None.

-Exceptions

   1)  If an I/O error occurs while opening or reading a text kernel,
       the error is signaled by a routine in the call tree of this
       routine.

   2)  If any text kernel parsing error occurs, the error is signaled
       by a routine in the call tree of this routine.

   3)  If a kernel pool overflow is detected, an error is signaled by
       a routine in the call tree of this routine.

   4)  If the `fname' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   5)  If the `fname' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

-Files

   See `fname' in -Detailed_Input.

-Particulars

   Text kernels input to this routine need not have native line
   terminators for the platform. Lower level CSPICE routines can
   read and process non-native text files. This functionality does
   not exist in the Fortran SPICELIB.

   Only text kernel readers include the non-native read capability,
   (ldpool_c and furnsh_c), the generic text file line reader, rdtext_c
   requires native text files.

   Please refer to kernel.req for additional information.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) The following program demonstrates how to load the variables
      contained in a NAIF ASCII kernel file into the kernel pool
      and how to determine the properties of a stored kernel
      variable.

      The program prompts for text kernel name and for the name of
      a kernel variable. If the variable is present in the kernel
      pool, the dimension and type of the variable are displayed.


      Example code begins here.


      /.
         Program ldpool_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {
         /.
         Local constants
         ./
         #define FILSIZ          256
         #define KVNMLN          33

         /.
         Local variables
         ./
         SpiceBoolean            found;

         SpiceChar               fname  [ FILSIZ ];
         SpiceChar               varnam [ KVNMLN ];
         SpiceChar               vtype  [ 1 ];
         SpiceInt                n;

         /.
         Prompt for the name of a text-kernel file.
         ./
         prompt_c ( "Enter text-kernel name        > ",
                     FILSIZ, fname );

         /.
         Load the kernel.  The same operation could be done using
         a furnsh_c call.
         ./
         ldpool_c ( fname );

         prompt_c ( "Enter name of kernel variable > ",
                     KVNMLN, varnam                      );

         dtpool_c ( varnam, &found, &n, vtype );

         if ( found )
         {
            printf ( "\n"
                     "Properties of variable %s:\n"
                     "\n"
                     "   Size:   %d\n",
                     varnam,
                     n                           );

            if ( vtype[0] == 'C' )
            {
               printf ( "   Type:   Character\n" );
            }
            else
            {
               printf ( "   Type:   Numeric\n" );
            }
         }
         else
         {
            printf ( "%s is not present in the kernel pool.\n", varnam );
         }

         return(0);
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, using the PCK file gm_de431.tpc to ask for the
      variable "BODY000_GMLIST", the output was:


      Enter text-kernel name        > gm_de431.tpc
      Enter name of kernel variable > BODY000_GMLIST

      Properties of variable BODY000_GMLIST:

         Size:   65
         Type:   Numeric


-Restrictions

   1)  Normally SPICE applications should load kernels via the
       furnsh_c routine.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   B.V. Semenov        (JPL)
   W.L. Taber          (JPL)
   R.E. Thurman        (JPL)
   I.M. Underwood      (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 2.1.0, 10-AUG-2021 (JDR)

       Changed input argument name "filename" to "fname" for
       consistency with other routines.

       Edited the header to comply with NAIF standard. Extended -Exceptions
       section to include errors detected by the underlying routine(s). Added
       -Restrictions entry.

       Added complete code example.

   -CSPICE Version 2.0.2, 27-FEB-2008 (BVS)

       Corrected the contents of the -Required_Reading section of
       the header.

   -CSPICE Version 2.0.1, 17-OCT-2005 (EDW)

       Added text to -Particulars section informing of the
       non-native kernel text file reading capability.

   -CSPICE Version 2.0.0, 08-FEB-1998 (NJB)

       Input argument kernel was changed to type ConstSpiceChar * and
       was given the new name "fname."

       Re-implemented routine without dynamically allocated, temporary
       strings. Made several corrections to the code example. Renamed
       the argument "fname" to "kernel" for consistency with the
       header comments.

   -CSPICE Version 1.0.0, 25-OCT-1997 (EDW) (IMU) (RET) (WLT)

-Index_Entries

   LOAD variables from a text kernel file into the pool

-&
*/

{ /* Begin ldpool_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "ldpool_c" );


   /*
   Check the input string fname to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "ldpool_c", fname );


   /*
   Call the f2c'd Fortran routine.
   */
   ldpool_ ( ( char   * ) fname,
             ( ftnlen   ) strlen(fname) );


   chkout_c ( "ldpool_c" );


} /* End ldpool_c */
