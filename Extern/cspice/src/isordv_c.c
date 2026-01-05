/*

-Procedure isordv_c ( Is array an order vector? )

-Abstract

   Determine whether an array of n items contains the integers
   0 through n-1.

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

   SEARCH
   SORT
   UTILITY

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #undef    isordv_c

   SpiceBoolean isordv_c ( ConstSpiceInt  * array,
                           SpiceInt         n      )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   array      I   Array of integers.
   n          I   Number of integers in array.

   The function returns SPICETRUE if the array contains the integers
   0 through n-1, otherwise it returns SPICEFALSE.

-Detailed_Input

   array       is an array of integers. Often this will be an array
               that is a candidate order vector to be passed to
               a routine for re-ordering some parallel array.

   n           is the number of elements in array.

-Detailed_Output

   The function returns SPICETRUE if the array contains the integers
   1 through n. Otherwise it returns SPICEFALSE.

-Parameters

   None.

-Exceptions

   1)  If n < 1, the function returns SPICEFALSE.

   2)  If memory cannot be allocated to create the temporary variable
       required for the execution of the underlying Fortran routine,
       the error SPICE(MALLOCFAILED) is signaled. The function
       returns the value SPICEFALSE.

-Files

   None.

-Particulars

   This function provides a simple means of determining whether
   or not an array of n integers contains exactly the integers
   0 through n-1. An array with this property is called an
   "order vector." Order vectors are returned by the CSPICE
   routines

      orderc_c
      orderd_c
      orderi_c

   and are accepted as input by the CSPICE routines

      reordc_c
      reordd_c
      reordi_c
      reordl_c

-Examples

   1) Suppose you wished to reorder an array of strings based upon
      a ranking array supplied by a user. If the ranking array
      contains any duplicates or refers to indices that are out
      of the range of valid indices for the array of strings,
      the attempt to reorder the array of strings cannot succeed.
      Its usually better to detect such a possibility before
      you begin trying to reorder the array of strings. This routine
      will detect the error.

      The code fragment below illustrates this idea.

         #include "SpiceUsr.h"
                  .
                  .
                  .

         if ( isordv_c ( ordvec, n ) )
         {
            ...reorder the input array of strings

            reordc_c ( ordvec, n, lenvals, strings );
         }
         else
         {
            ...state the problem and let the user decide what
            to do about it.
                  .
                  .
                  .
         }

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   W.L. Taber          (JPL)
   I.M. Underwood      (JPL)

-Version

   -CSPICE Version 1.1.1, 03-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.1.0, 16-FEB-2005 (NJB)

       Bug fix: dynamic memory is now freed.

   -CSPICE Version 1.0.0, 10-JUL-2002 (NJB) (WLT) (IMU)

-Index_Entries

   test whether an integer array is an order vector

-&
*/

{ /* Begin isordv_c */


   /*
   Local variables
   */
   SpiceBoolean            retval;

   SpiceInt                i;
   SpiceInt                vSize;
   SpiceInt              * ordvec;




   /*
   This routine uses discovery check-in.

   Initialize the return value.
   */
   retval = SPICEFALSE;

   /*
   Nothing to check if the array is empty.
   */
   if ( n < 1 )
   {
      return ( retval );
   }

   /*
   Get a local copy of the input array; increment each element
   of this local array.  If the array is a C-style order vector, this
   operation maps the vector to Fortran style.
   */
   vSize  = n * sizeof(SpiceInt);

   ordvec = (SpiceInt *) malloc( vSize );

   if ( ordvec == 0 )
   {
      chkin_c  ( "isordv_c"                                );
      setmsg_c ( "Failure on malloc call to create array "
                 "for Fortran-style order vector.  Tried "
                 "to allocate # bytes."                    );
      errint_c ( "#",  vSize                               );
      sigerr_c ( "SPICE(MALLOCFAILED)"                     );
      chkout_c ( "isordv_c"                                );
      return   (  retval                                   );
   }

   for ( i = 0;  i < n;  i++ )
   {
      ordvec[i] = array[i] + 1;
   }


   retval =  (SpiceBoolean) isordv_ ( (integer *) ordvec,
                                      (integer *) &n     );

   free   ( ordvec );

   return ( retval );

} /* End isordv_c */
