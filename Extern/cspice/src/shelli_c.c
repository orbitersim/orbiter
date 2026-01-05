/*

-Procedure shelli_c ( Shell sort an integer array )

-Abstract

   Sort an integer array using the Shell Sort algorithm.

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
   SORT

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"

   void shelli_c ( SpiceInt     ndim,
                   SpiceInt   * array )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   ndim       I   Dimension of the array.
   array     I-O  The array.

-Detailed_Input

   ndim        is the number of elements in the array to be sorted.

   array       on input, is the array to be sorted.

-Detailed_Output

   array       on output, contains the same elements, sorted
               in increasing order. The actual sorting is done
               in place in array.

-Parameters

   None.

-Exceptions

   Error free.

   1)  If ndim < 2, this routine does not modify the array.

-Files

   None.

-Particulars

   The Shell Sort Algorithm is well known.

-Examples

   Let array contain the following elements:

       99
       33
       55
       44
      -77
       66

   Then after a call to shelli_c, the array would be ordered as
   follows:

      -77
       33
       44
       55
       66
       99

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   I.M. Underwood      (JPL)

-Version

   -CSPICE Version 1.0.1, 05-JUL-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.0.0, 08-JUL-2002 (NJB) (IMU)

-Index_Entries

   shell sort an integer array

-&
*/

{ /* Begin shelli_c */

   shelli_ ( ( integer * ) &ndim,
             ( integer * ) array  );

} /* End shelli_c */
