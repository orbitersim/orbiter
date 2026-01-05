/*

-Procedure clpool_c ( Clear the pool of kernel variables )

-Abstract

   Remove all kernel variables from the kernel pool. Watches
   on kernel variables are retained.

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


   void clpool_c ( void )

/*

-Brief_I/O

   None.

-Detailed_Input

   None.

-Detailed_Output

   None.

-Parameters

   None.

-Exceptions

   Error free.

   1)  All known agents (those established through the CSPICE
       routine swpool_c) will be "notified" that their watched
       variables have been updated whenever clpool_c is called.

-Files

   None.

-Particulars

   clpool_c clears the pool of kernel variables maintained by
   the kernel POOL subsystem. All the variables in the pool are deleted.
   However, all watcher information is retained.

   Each watched variable will be regarded as having been updated.
   Any agent associated with that variable will have a notice
   posted for it indicating that its watched variable has been
   updated.

   Application programs can delete watches by calling the SPICELIB
   routine DWPOOL. See the header of that routine for details.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) This code example demonstrates how to assign values to kernel
      pool variables, how to check for the existence of kernel pool
      variables and how to clear the kernel pool, i.e. how to delete
      all variable assignments loaded into the kernel pool.

      Place a value into the kernel pool and check for the variable
      to which the value has been assigned. Clear the kernel pool
      and check for that variable again.

      Example code begins here.


      /.
         Program clpool_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {

         /.
         Local variables
         ./
         SpiceDouble          dvals  [1];

         SpiceInt             n;

         SpiceBoolean         found;

         /.
         Place a value into the kernel pool. Recall the routines
         for direct insertion of pool assignments have arrays for
         input.
         ./
         dvals[0] = -666.0;
         pdpool_c ( "TEST_VAR", 1, dvals );

         /.
         Check for the variable assignment to TEST_VAR.
         ./
         gdpool_c ( "TEST_VAR", 0, 1, &n, dvals, &found );

         printf( "First call to gdpool_c:\n" );

         if ( found )
         {
            printf( "   TEST_VAR value: %7.2f\n", dvals[0] );
         }
         else
         {
            printf( "   TEST_VAR not in kernel pool.\n" );
         }

         /.
         Now clear the kernel pool.
         ./
         clpool_c();

         /.
         Again, check for the TEST_VAR assignment.
         ./
         gdpool_c ( "TEST_VAR", 0, 1, &n, dvals, &found );

         printf( "Second call to gdpool_c:\n" );

         if ( found )
         {
            printf( "   TEST_VAR value: %5.2f\n", dvals[0] );
         }
         else
         {
            printf( "   TEST_VAR not in kernel pool.\n" );
         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      First call to gdpool_c:
         TEST_VAR value: -666.00
      Second call to gdpool_c:
         TEST_VAR not in kernel pool.


-Restrictions

   1)  This routine should not be used to unload kernels that
       have been loaded via furnsh_c.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   W.L. Taber          (JPL)
   I.M. Underwood      (JPL)

-Version

   -CSPICE Version 1.0.2, 13-AUG-2021 (NJB) (JDR)

       Edited the header to comply with NAIF standard. Added complete
       code example.

       Added -Restrictions section.

   -CSPICE Version 1.0.1, 01-JUL-2014 (NJB)

       Updated comments regarding behavior of the watcher
       subsystem.

   -CSPICE Version 1.0.0, 18-JUN-1999 (IMU) (WLT) (NJB)

-Index_Entries

   CLEAR the pool of kernel variables

-&
*/

{ /* Begin clpool_c */



   /*
   Participate in error tracing.
   */
   chkin_c ( "clpool_c" );

   /*
   Just call the f2c'd routine.
   */
   clpool_();


   chkout_c ( "clpool_c" );

} /* End clpool_c */
