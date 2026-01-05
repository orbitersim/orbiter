/*

-Procedure dvpool_c  ( Delete a variable from the kernel pool )

-Abstract

   Delete a variable from the kernel pool.

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
   #include "SpiceZmc.h"


   void dvpool_c ( ConstSpiceChar  * name )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   name       I   Name of the kernel variable to be deleted.

-Detailed_Input

   name        is the name of the kernel pool variable to delete.
               The name and associated values are removed from the
               kernel pool, freeing the occupied space.

               If watches are set on the variable designated by
               name, the corresponding agents are placed on the list
               of agents to be notified of a kernel variable update.

-Detailed_Output

   None.

-Parameters

   None.

-Exceptions

   1)  If the specified variable is not present in the kernel pool,
       this routine simply returns. No error is signaled.

   2)  If the `name' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   3)  If the `name' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

-Files

   None.

-Particulars

   This routine enables users to selectively remove variables from
   the kernel pool, as opposed to having to clear the pool and
   reload it.

   Note that it is not necessary to remove kernel variables in order
   to simply update them; this routine should be used only when
   variables are to be removed.

-Examples

   1) Remove triaxial radii of Jupiter from the kernel pool.

         #include "SpiceUsr.h"
               .
               .
               .
         dvpool_c ( "BODY599_RADII" );

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   W.L. Taber          (JPL)

-Version

   -CSPICE Version 1.0.1, 04-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.0.0, 05-JUN-1999 (NJB) (WLT)

-Index_Entries

   delete a kernel pool variable

-&
*/

{ /* Begin dvpool_c */



   /*
   Use discovery check-in.
   */


   /*
   Check the kernel variable name to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_DISCOVER, "dvpool_c", name );


   /*
   Call the f2c'd routine.
   */

   dvpool_ (  ( char    * ) name,
              ( ftnlen    ) strlen(name) );


} /* End dvpool_c */
