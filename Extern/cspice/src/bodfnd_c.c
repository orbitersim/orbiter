/*

-Procedure bodfnd_c ( Find values from the kernel pool )

-Abstract

   Determine whether values exist for some item for any body
   in the kernel pool.

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
   NAIF_IDS
   PCK

-Keywords

   CONSTANTS

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"

   SpiceBoolean bodfnd_c ( SpiceInt           body,
                           ConstSpiceChar   * item )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   body       I   ID code of body.
   item       I   Item to find ("RADII", "NUT_AMP_RA", etc.).

   The function returns the value SPICETRUE if the item is in the
   kernel pool, and is SPICEFALSE if it is not.

-Detailed_Input

   body        is the ID code of the body for which the item is
               requested. Bodies are numbered according to the
               standard NAIF numbering scheme.

   item        is the item to be returned. Together, the body and
               item name combine to form a variable name, e.g.,

                     "BODY599_RADII"
                     "BODY4_POLE_RA"

-Detailed_Output

   The function returns the value SPICETRUE if the item is in the
   kernel pool, and is SPICEFALSE if it is not.

-Parameters

   None.

-Exceptions

   1)  If the `item' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled. The function returns the value
       SPICEFALSE.

   2)  If the `item' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled. The function returns the value
       SPICEFALSE.

-Files

   None.

-Particulars

   The CSPICE routines bodvcd_c and bodvrd_c, which return values from
   the kernel pool, signal an error if the specified item is not found.
   In many cases, this is appropriate. However, sometimes the program
   may attempt to recover, by providing default values, prompting for
   replacements, and so on.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Test if the Earth's radii values exist in the kernel pool.

      Use the PCK kernel below to load the required triaxial
      ellipsoidal shape model for the Earth.

         pck00008.tpc


      Example code begins here.


      /.
         Program bodfnd_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {
         /.
         Local constants.
         ./
         #define  bodyid         399

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
         found = bodfnd_c( bodyid, "RADII" );

         /.
         If found, retrieve the values.
         ./
         if ( found )
         {
            bodvcd_c( bodyid, "RADII", 3, &dim, radii );

            printf ( "%d RADII: %10.3f %10.3f %10.3f\n",
                     (int)bodyid, radii[0], radii[1], radii[2] );
         }
         else
         {
            printf ( "No RADII data found for object %d\n",
                     (int)bodyid                          );
         }
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
   H.A. Neilan         (JPL)
   W.L. Taber          (JPL)
   I.M. Underwood      (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 2.0.3, 08-JUL-2021 (JDR)

       Edited the header to comply with NAIF standard. Added
       complete code examples.

   -CSPICE Version 2.0.2, 24-OCT-2005 (NJB)

       Header updates: reference to bodvar_c was replaced with
       reference to bodvcd_c. The string "AXES" and variable `axes'
       were replaced with the string "RADII" and variable `radii'
       throughout the header. A few other minor header edits were
       made.

   -CSPICE Version 2.0.1, 08-FEB-1998 (EDW)

       Corrected and clarified header entries.

   -CSPICE Version 2.0.0, 06-JAN-1998 (NJB)

       Input argument item was changed to type ConstSpiceChar *.

       References to C2F_CreateStr_Sig were removed; code was
       cleaned up accordingly. String checks are now done using
       the macro CHKFSTR_VAL.

   -CSPICE Version 1.0.0, 25-OCT-1997 (EDW) (WLT) (IMU) (HAN)

-Index_Entries

   find constants for a body in the kernel pool

-&
*/

{ /* Begin bodfnd_c */

   /*
   Local variables.
   */
   SpiceBoolean    result;


   /*
   Participate in error tracing.
   */
   chkin_c ( "bodfnd_c" );


   /*
   Check the input string to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR_VAL ( CHK_STANDARD, "bodfnd_c", item, SPICEFALSE );


   /*
   Execute the f2c'd routine.
   */
   result =  (SpiceBoolean) bodfnd_( ( integer    * ) &body,
                                     ( char       * ) item,
                                     ( ftnlen       ) strlen(item) );


   /*
   We now have a true or false.  Tell the caller the value.  It may need
   to know.
   */
   chkout_c ( "bodfnd_c" );

   return ( result );

} /* End bodfnd_c */
