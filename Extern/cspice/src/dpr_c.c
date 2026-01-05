/*

-Procedure dpr_c ( Degrees per radian )

-Abstract

   Return the number of degrees per radian.

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

   CONSTANTS

*/

   #include <math.h>
   #include "SpiceUsr.h"

   SpiceDouble dpr_c ( void )

/*

-Brief_I/O

   The function returns the number of degrees per radian.

-Detailed_Input

   None.

-Detailed_Output

   The function returns the number of degrees per radian: 180/pi.
   The value of pi is determined by the ACOS function. That is,

         dpr_c = 180. / acos ( -1. );

-Parameters

   None.

-Exceptions

   Error free.

-Files

   None.

-Particulars

   When he function is referenced, the value is computed
   as shown above.

-Examples

   The code fragment below illustrates the use of dpr_c.

      Convert all angles to degrees for output.

              clock = clock * dpr_c();
              cone  = cone  * dpr_c();
              twist = twist * dpr_c();

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)
   W.L. Taber          (JPL)
   I.M. Underwood      (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.0.1, 02-JUN-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.0.0, 16-APR-1999 (EDW) (WLT) (IMU)

-Index_Entries

   degrees per radian

-&
*/

{ /* Begin dpr_c */

     /*
     Local Variables
     */

     static SpiceDouble  value = 0.;

     if (value == 0.)
        {
         value = 180. / acos(-1.);
        }


     /*
     What is there to say?
     */

     return value;

} /* End dpr_c */
