/*

-Procedure isrchd_c  ( Search in a double precision array )

-Abstract

   Search for a given value within a double precision array. Return
   the index of the first matching array entry, or -1 if the key value
   was not found.

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
   SEARCH

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZim.h"
   #undef   isrchd_c

   SpiceInt isrchd_c ( SpiceDouble         value,
                       SpiceInt            ndim,
                       ConstSpiceDouble  * array  )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   value      I   Key value to be found in array.
   ndim       I   Dimension of array.
   array      I   Double Precision array to search.

   The function returns the index of the first matching array
   element or -1 if the value is not found.

-Detailed_Input

   value       is the key value to be found in the array.

   ndim        is the dimension of the array.

   array       is the double precision array to be searched.

-Detailed_Output

   The function returns the index of the first matching array
   element in array. If value is not found, isrchd_c returns -1.

-Parameters

   None.

-Exceptions

   Error free.

   1)  If ndim < 1, the function value is -1.

-Files

   None.

-Particulars

   None.

-Examples

   The following table shows the value of isrchd_c given the contents
   of array and value:


      array                        value   isrchd_c
   --------------------------      -----   --------
   1.0D0, 0.0D0, 4.0D0, 2.0D0      4.0D0       2
   1.0D0, 0.0D0, 4.0D0, 2.0D0      2.OD0       3
   1.0D0, 0.0D0, 4.0D0, 2.0D0      3.0D0      -1

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   W.M. Owen           (JPL)

-Version

   -CSPICE Version 1.0.1, 03-JUL-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.0.0, 08-JUL-2002 (NJB) (WMO)

-Index_Entries

   search in an double precision array

-&
*/

{ /* Begin isrchd_c */

   /*
   Local variables
   */
   SpiceInt                loc ;


   /*
   Call the f2c'd routine.
   */
   loc =  (SpiceInt) isrchd_ ( (doublereal *) &value,
                               (integer    *) &ndim,
                               (doublereal *) array   );

   /*
   Convert loc to a C-style index.
   */
   loc-- ;

   return ( loc );

} /* End isrchd_c */
