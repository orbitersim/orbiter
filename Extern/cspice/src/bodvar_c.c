/*

-Procedure bodvar_c ( Return values from the kernel pool )

-Abstract

   Deprecated: This routine has been superseded by bodvcd_c and
   bodvrd_c. This routine is supported for purposes of backward
   compatibility only.

   Return the values of some item for any body in the
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
   PCK
   SPK

-Keywords

   CONSTANTS

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"

   void bodvar_c ( SpiceInt           body,
                   ConstSpiceChar   * item,
                   SpiceInt         * dim,
                   SpiceDouble      * values )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   body       I   ID code of body.
   item       I   Item for which values are desired.
   dim        O   Number of values returned.
   values     O   Values.

-Detailed_Input

   body        is the ID code of the body for which `item' is
               requested.

   item        is the item to be returned. Together, the body and
               item name combine to form a variable name, e.g.,

                  "BODY599_RADII"
                  "BODY401_POLE_RA"

-Detailed_Output

   dim         is the number of values associated with the variable.

   values      are the values associated with the variable.

-Parameters

   None.

-Exceptions

   1)  If the requested item is not found, the error
       SPICE(KERNELVARNOTFOUND) is signaled by a routine in the call
       tree of this routine.

   2)  If the `item' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   3)  If the `item' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

-Files

   None.

-Particulars

   None.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Retrieve the Earth's radii values from the kernel pool

      Use the PCK kernel below to load the required triaxial
      ellipsoidal shape model for the Earth.

         pck00008.tpc


      Example code begins here.


      /.
         Program bodvar_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {
         /.
         Local constants.
         ./
         #define  BODYID         399

         /.
         Local variables.
         ./
         SpiceBoolean            found;
         SpiceDouble             radii [ 3 ];
         SpiceInt                dim;

         /.
         Load a PCK file.
         ./
         furnsh_c( "pck00008.tpc" );

         /.
         Test if Earth's radii values exist in the
         kernel pool.

         The procedure searches for the kernel variable
         BODY399_RADII.
         ./
         found = bodfnd_c( BODYID, "RADII" );

         /.
         If found, retrieve the values.
         ./
         if ( found )
         {
            bodvar_c( BODYID, "RADII", &dim, radii );

            printf ( "%d RADII: %10.3f %10.3f %10.3f\n",
                     (int)BODYID, radii[0], radii[1], radii[2] );
         }
         else
         {
            printf ( "No RADII data found for object %d\n",
                     (int)BODYID                          );
         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      399 RADII:   6378.140   6378.140   6356.750


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   B.V. Semenov        (JPL)
   W.L. Taber          (JPL)
   I.M. Underwood      (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 2.0.5, 06-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard. Added complete
       code example. Moved SPK required reading from -Literature_References
       to -Required_Reading section.

   -CSPICE Version 2.0.4, 19-MAY-2010 (BVS)

       Index lines now states that this routine is deprecated.

   -CSPICE Version 2.0.3, 27-OCT-2005 (NJB)

       Routine is now deprecated.

   -CSPICE Version 2.0.2, 08-JAN-2004 (EDW)

       Trivial typo corrected.

   -CSPICE Version 2.0.1, 08-FEB-1998 (EDW)

       Corrected and clarified header entries.

   -CSPICE Version 2.0.0, 06-JAN-1998 (NJB)

       Input argument item was changed to type ConstSpiceChar *.

       References to C2F_CreateStr_Sig were removed; code was
       cleaned up accordingly. String checks are now done using
       the macro CHKFSTR.

   -CSPICE Version 1.0.0, 25-OCT-1997 (EDW) (WLT) (IMU)

-Index_Entries

   DEPRECATED fetch constants for a body from the kernel pool
   DEPRECATED physical constants for a body

-&
*/

{ /* Begin  bodvar_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "bodvar_c" );


   /*
   Check the input string to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "bodfnd_c", item );


   /*
   Call the f2c'd routine.
   */
   bodvar_( ( integer    * ) &body,
            ( char       * ) item,
            ( integer    * ) dim,
            ( doublereal * ) values,
            ( ftnlen       ) strlen(item) );


   chkout_c ( "bodvar_c" );

} /* End  bodvar_c*/
