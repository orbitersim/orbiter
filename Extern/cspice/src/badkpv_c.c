/*

-Procedure badkpv_c ( Bad Kernel Pool Variable )

-Abstract

   Determine if a kernel pool variable is present and if so
   that it has the correct size and type.

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

   ERROR
   KERNEL

-Keywords

   ERROR

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"


   SpiceBoolean badkpv_c ( ConstSpiceChar    *caller,
                           ConstSpiceChar    *name,
                           ConstSpiceChar    *comp,
                           SpiceInt           size,
                           SpiceInt           divby,
                           SpiceChar          type   )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   caller     I   Name of the routine calling this routine.
   name       I   Name of a kernel pool variable.
   comp       I   Comparison operator.
   size       I   Expected size of the kernel pool variable.
   divby      I   A divisor of the size of the kernel pool variable.
   type       I   Expected type of the kernel pool variable.

   The function returns SPICEFALSE if the kernel pool variable is OK.

-Detailed_Input

   caller      is the name of the routine calling this routine
               to check correctness of kernel pool variables.

   name        is the name of a kernel pool variable that the
               calling program expects to be present in the
               kernel pool.

   comp        is the comparison operator to use when comparing
               the number of components of the kernel pool variable
               specified by `name' with the integer `size'. If `dim' is
               is the actual size of the kernel pool variable then
               badkpv_c will check that the sentence

                  dim comp size

               is a true statement. If it is not a true statement
               an error will be signaled.

               Allowed values for `comp' and their meanings are:

                  "="      dim == size
                  "<"      dim <  size
                  ">"      dim >  size
                  "=>"     dim >= size
                  "<="     dim <= size

   size        is an integer to compare with the actual
               number of components of the kernel pool variable
               specified by `name'.

   divby       is an integer that is one of the factors of the
               actual dimension of the specified kernel pool variable.
               In other words, it is expected that `divby' evenly
               divides the actual dimension of `name'. In those
               cases in which the factors of the dimension of `name'
               are not important, set `divby' to 1 in the calling
               program.

   type        is the expected type of the kernel pool variable.
               Recognized values are

                  "C" for character type
                  "N" for numeric type (integer and double precision)

               The case of `type' is insignificant. If the value
               of `type' is not one of the 2 values given above
               no check for the type of the variable will be
               performed.

-Detailed_Output

   The function returns the value SPICEFALSE if the kernel pool
   variable has the expected properties. Otherwise the routine
   signals an error and returns the value SPICETRUE.

-Parameters

   None.

-Exceptions

   1)  If the kernel pool variable specified by `name' is not present
       in the kernel pool, the error SPICE(VARIABLENOTFOUND) is
       signaled by a routine in the call tree of this routine and the
       routine will return the value SPICETRUE.

   2)  If the comparison operator specified by `comp' is unrecognized,
       the error SPICE(UNKNOWNCOMPARE) is signaled by a routine in
       the call tree of this routine and the routine will return the
       value SPICETRUE.

   3)  If the expected type of the kernel pool variable `type' is not
       one of the supported types, the error SPICE(INVALIDTYPE) is
       signaled by a routine in the call tree of this routine and the
       routine will return the value SPICETRUE.

   4)  If the comparison of the actual size of the kernel pool
       variable with `size' is not satisfied, the error
       SPICE(BADVARIABLESIZE) is signaled by a routine in the call
       tree of this routine and the routine will return the value
       SPICETRUE.

   5)  If the variable does not have the expected type, the error
       SPICE(BADVARIABLETYPE) is signaled by a routine in the call
       tree of this routine and the routine will return the value
       SPICETRUE.

   6)  If any of the `caller', `name' or `comp' input string pointers
       is null, the error SPICE(NULLPOINTER) is signaled. The
       function returns the value SPICETRUE.

   7)  If any of the `caller', `name' or `comp' input strings has
       zero length, the error SPICE(EMPTYSTRING) is signaled. The
       function returns the value SPICETRUE.

-Files

   None.

-Particulars

   This routine takes care of routine checking that often needs
   to be done by programs and routines that rely upon kernel
   pool variables being present and having the correct attributes.

   It checks for the presence of the kernel pool variable and
   examines the type and dimension of the variable to make sure
   they conform to the requirements of the calling routine.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Suppose that you need to fetch a number of variables
      from the kernel pool and want to check that the requested
      items are in fact available prior to performing further
      computations. The code example shows how you might use
      this routine to handle the details of checking of
      the various items.

      Although by default the SPICE error handling system will
      report the error and halt the execution of the program, in
      this example we have decided to change this behavior to
      display the error messages and continue the execution of
      the program.

      Use the kernel shown below to define some variables related
      to the Earth.


         KPL/PCK

         File name: badkpv_ex1.tpc

         The contents of this kernel are not intended for
         real applications. Use only with this example.

         \begindata

            BODY_399_DATA  = ( 3.1416, 2.71828, 0.5, 12.0 )
            BODY_399_NAMES = ( 'PI', 'E', 'HALF', 'DOZEN' )

         \begintext

         End of constants kernel


      Example code begins here.


      /.
         Program badkpv_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local parameters.
         ./
         #define CALLER       "BADKPV_EX1"

         /.
         Local variables.
         ./
         SpiceChar          * comp;
         SpiceChar          * name;
         SpiceChar            type;

         SpiceInt             divby;
         SpiceInt             size;

         /.
         Load the test kernel.
         ./
         furnsh_c ( "badkpv_ex1.tpc" );

         /.
         Change the default behavior of the SPICE error handling
         system to print out all messages and continue the
         execution of the program.  We do this for demonstration
         purposes.  Please, refrain from changing the default
         behavior on real applications.
         ./
         erract_c ( "SET", 7, "REPORT" );

         /.
         Assume that we need some data for body 399 and we expect
         there to be an even number of items available and at
         least 4 such items. Moreover we expect these items to be
         numeric. Note that the variable assignments below are
         present only to assist in understanding the calls to
         badkpv_c.
         ./
         name  = "BODY_399_DATA";
         comp  = "=>";
         size  =  4;
         divby =  2;
         type  = 'N';

         if ( ! badkpv_c ( CALLER, name, comp, size, divby, type ) )
         {

            printf( "Expected form of variable %s found in kernel pool.\n",
                                                                       name );

         }

         /.
         In addition we need the names given to these items.
         Improperly indicate the array has type numeric.
         ./
         name  = "BODY_399_NAMES";
         comp  = "=>";
         size  =  4;
         divby =  1;
         type  = 'N';

         if ( ! badkpv_c ( CALLER, name, comp, size, divby, type ) )
         {

            printf( "Expected form of variable %s found in kernel pool.\n",
                                                                       name );

         }

         /.
         Change the behavior of the SPICE error handling to
         its default.
         ./
         erract_c ( "SET", 8, "DEFAULT" );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Expected form of variable BODY_399_DATA found in kernel pool.

      =====================================================================

      Toolkit version: N0066

      SPICE(BADVARIABLETYPE) --

      BADKPV_EX1: The kernel pool variable 'BODY_399_NAMES' must be of type
      "NUMERIC". However, the current type is character.

      A traceback follows.  The name of the highest level module is first.
      badkpv_c --> BADKPV

      =====================================================================


      Note that, as expected, the error SPICE(BADVARIABLETYPE) is
      signaled by the second badkpv_c call, since we have improperly
      indicated that the requested array is numeric, when actually
      it is of character type.

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   W.L. Taber          (JPL)

-Version

   -CSPICE Version 1.1.0, 13-AUG-2021 (JDR)

       Added exception SPICE(INVALIDTYPE) for the case of unknown
       expected kernel pool variable type.

       Edited the header to comply with NAIF standard. Added complete
       code example based on existing fragment. Added required
       readings references.

   -CSPICE Version 1.0.0, 07-JUL-2000 (WLT) (NJB)

-Index_Entries

   Check the properties of a kernel pool variable

-&
*/

{ /* Begin badkpv_c */


   /*
   Local variables
   */
   logical                 isbad;


   /*
   Participate in error tracing.
   */
   chkin_c ( "badkpv_c" );


   /*
   Check the input strings to make sure the pointers are non-null
   and the string lengths are non-zero.
   */
   CHKFSTR_VAL ( CHK_STANDARD, "badkpv_c", caller, SPICETRUE );
   CHKFSTR_VAL ( CHK_STANDARD, "badkpv_c", name,   SPICETRUE );
   CHKFSTR_VAL ( CHK_STANDARD, "badkpv_c", comp,   SPICETRUE );

   /*
   Let the f2c'd routine do all the work.
   */
   isbad = badkpv_ (  (char     *)  caller,
                      (char     *)  name,
                      (char     *)  comp,
                      (integer  *)  &size,
                      (integer  *)  &divby,
                      (char     *)  &type,
                      (ftnlen    )  strlen(caller),
                      (ftnlen    )  strlen(name),
                      (ftnlen    )  strlen(comp),
                      (ftnlen    )  1              );


   chkout_c ( "badkpv_c" );

   return (  (SpiceBoolean) isbad  );

} /* End badkpv_c */
