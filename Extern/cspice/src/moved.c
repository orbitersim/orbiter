/*

-Procedure moved_  ( Move a double precision array to another )

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
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"
   
   
   int moved_ ( doublereal   * arrfrm,
                integer      * ndim,
                doublereal   * arrto  ) 

/*

-Brief_I/O
 
   VARIABLE  I/O              DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   arrfrm     I     Double precision array to be moved. 
   ndim       I     Number of elements to copy, i.e. the dimension 
                    of arrfrm and arrto. 
   arrto      O     Destination array. 
 
-Detailed_Input
 
   arrfrm     Array from which to copy items. 
 
   ndim       Number of items to copy. 
 
-Detailed_Output
 
   arrto      Array to which items should be copied. 
 
-Parameters
 
   None. 
 
-Particulars
 
   This routine should not be called by user applications.  It exists
   solely for the use of CSPICE functions produced by running f2c 
   on Fortran code.
   
-Examples
 
   This function encapsulates the following memmove call:
 
      memmove ( (void*) arrto, 
                (void*) arrfrm, 
                sizeof(SpiceDouble) * ndim );
                
   where ndim is the number of double precision elements of the array
   arrfrm.

   This call can be rewritten as 
 
      moved_ ( arrfrm, &ndim, arrto );

 
-Restrictions
 
   1) This function should not be called directly by user's application
      software.  
 
-Exceptions
 
   Error free. 
 
-Files
 
   None. 
 
-Author_and_Institution
 
   K.R. Gehringer  (JPL) 
   W.M. Owen       (JPL) 
   W.L. Taber      (JPL) 
 
-Literature_References
 
   None. 
 
-Version
 
   -CSPICE Version 1.1.0, 14-SEP-1999 (NJB)
   
      Now avoids passing non-positive byte count to memmove.

   -CSPICE Version 1.0.0, 04-NOV-1998 (NJB)

-Index_Entries
 
   move a d.p. array to another d.p. array 
 
-&
*/

{ /* Begin moved_ */


   if ( *ndim > 0 )
   {
      MOVED ( arrfrm, (*ndim), arrto );
   }
   
   return ( 0 );
   

} /* End moved_ */


