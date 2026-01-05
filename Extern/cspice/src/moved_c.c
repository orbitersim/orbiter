/*

-Procedure moved_c  ( Move a double precision array to another )

-Abstract

   Copy the elements of one double precision array into another
   array.

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

   ARRAY

*/
   #include "SpiceUsr.h"
   #include "SpiceZmc.h"
   #include "SpiceZfc.h"

   void moved_c  ( ConstSpiceDouble    arrfrm [],
                   SpiceInt            ndim,
                   SpiceDouble         arrto  [] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   arrfrm     I   Double precision array to be moved.
   ndim       I   Number of elements to copy, i.e. the dimension
                  of `arrfrm' and `arrto'.
   arrto      O   Destination array.

-Detailed_Input

   arrfrm      Array from which to copy items.

   ndim        Number of items to copy.

-Detailed_Output

   arrto       Array to which items should be copied.

-Parameters

   None.

-Exceptions

   Error free.

   1)  If ndim < 1, the output array is returned unchanged.

-Files

   None.

-Particulars

   This routine is simply shorthand for the following 3 lines of
   code.

      for ( i = 0; i < ndim; i++ )
      {
         arrto[i] = arrfrm[i];
      }

-Examples

   Often one needs to make a temporary copy of an array so that
   it can be manipulated without altering the original array.
   As pointed out in particulars, you could just do this within
   the code that needs the copy. However, if you have several
   arrays to copy, you can cut the number of lines of code that
   are needed by a third.

   For example:

      for ( i = 0; i < 19; i++ )
      {
         tempa[i] = a[i];
      }

      for ( i = 0; i < 38; i++ )
      {
         tempb[i] = b[i];
      }

   Can be rewritten as

      moved_c ( a, 19, tempa )
      moved_c ( b, 38, tempb )

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.0, 08-FEB-2021 (JDR)

-Index_Entries

   move a d.p. array to another d.p. array

-&
*/

{ /* Begin moved_c */

   /*
   Error free:  no error tracing required.
   */

   /*
   Call the MOVED macro.
   */
   MOVED( arrfrm, ndim, arrto );

} /* End moved_c */
