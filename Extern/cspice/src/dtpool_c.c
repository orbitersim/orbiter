/*

-Procedure dtpool_c (Data for a kernel pool variable)

-Abstract

   Return the data about a kernel pool variable.

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
   #include <string.h>
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"


   void dtpool_c ( ConstSpiceChar   * name,
                   SpiceBoolean     * found,
                   SpiceInt         * n,
                   SpiceChar          type [1] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   name       I   Name of the variable whose value is to be returned.
   found      O   SPICETRUE if variable is in pool.
   n          O   Number of values returned for `name'.
   type       O   Type of the variable: 'C', 'N', or 'X'

-Detailed_Input

   name        is the name of the variable whose values are to be
               returned.

-Detailed_Output

   found       is SPICETRUE if the variable is in the pool;
               SPICEFALSE if it is not.

   n           is the number of values associated with `name'.
               If `name' is not present in the pool `n' will be returned
               with the value 0.

   type        is a single character indicating the type of the variable
               associated with `name'.

                   'C' if the data is character data
                   'N' if the data is numeric.
                   'X' if there is no variable `name' in the pool.

-Parameters

   None.

-Exceptions

   1)  If the name requested is not in the kernel pool, `found'
       will be set to SPICEFALSE, `n' to zero and `type' to 'X'.

   2)  If the `name' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   3)  If the `name' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

-Files

   None.

-Particulars

   This routine allows you to determine whether or not a kernel
   pool variable is present and to determine its size and type
   if it is.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) The following program demonstrates how to determine the
      properties of a stored kernel variable. The program prompts
      for text kernel name and for the name of a kernel variable.
      If the variable is present in the kernel pool, the dimension
      and type of the variable are displayed.


      Example code begins here.


      /.
         Program dtpool_ex1
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
         Load the kernel.
         ./
         furnsh_c ( fname );

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
      platform, using the FK file named cas_v40.tf to ask for the
      variable "FRAME_-82104_NAME", the output was:


      Enter text-kernel name        > cas_v40.tf
      Enter name of kernel variable > FRAME_-82104_NAME

      Properties of variable FRAME_-82104_NAME:

         Size:   1
         Type:   Character


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   W.L. Taber          (JPL)

-Version

   -CSPICE Version 1.2.1, 10-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.2.0, 04-JUN-2014 (NJB)

       Header example code fragment was replaced with
       a complete program.

   -CSPICE Version 1.1.0, 17-OCT-1999 (NJB)

       Local type logical variable now used for found flag used in
       interface of dtpool_.

   -CSPICE Version 1.0.0, 10-MAR-1999 (NJB) (WLT)

-Index_Entries

   return summary information about a kernel pool variable

-&
*/

{ /* Begin dtpool_c */

   /*
   Local variables
   */
   logical                 fnd;


   /*
   Participate in error tracing.
   */
   chkin_c ( "dtpool_c" );


   /*
   Check the input string name to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "dtpool_c", name );


   /*
   Call the f2c'd routine.
   */
   dtpool_ ( ( char     * ) name,
             ( logical  * ) &fnd,
             ( integer  * ) n,
             ( char     * ) type,
             ( ftnlen     ) strlen(name),
             ( ftnlen     ) 1             );

   /*
   Assign the SpiceBoolean found flag.
   */

   *found = fnd;


   chkout_c ( "dtpool_c" );

} /* End dtpool_c */
