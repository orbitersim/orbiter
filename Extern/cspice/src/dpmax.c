/*

-Procedure dpmax_ ( Largest DP number )

-Abstract
 
   Return the value of the largest (positive) number representable 
   in a double precision variable. 
 
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

   #include <float.h>
   #include "SpiceUsr.h"

   SpiceDouble dpmax_ () 

/*

-Brief_I/O
 
   The function returns the value of the largest (positive) number 
   that can be represented in a double precision variable. 
 
-Detailed_Input
 
   None. 
 
-Detailed_Output
 
   The function returns the value of the largest (positive) number 
   that can be represented in a double precision variable. 
 
   This value varies from machine to machine.  The value is defined by 
   the macro DBL_MAX from the ANSI standard header file float.h.
   According to the ANSI standard, DBL_MAX must be at least
   
      1.E+37 
         
 
-Parameters
 
   None. 
 
-Exceptions
 
   Error free. 
 
-Files
 
   None. 
 
-Particulars
 
   This function replaces that produced by running f2c on the Fortran
   SPICELIB file dpmax.f. 
 
-Examples
 
   The following code fragments illustrate the use of dpmax_. 
   Note in the second example that the smallest negative number 
   is not necessarily the negative of the largest positive number. 
 
   1) Set a range variable for a star or ephemeris object.
      
      /.
      Compute the distance to each object. For stars, use 
      a "very large" distance. 
      ./
      
      for ( i = 0;  i < n;  i++ )
      {
         if (  strcmp ( type[i], "star" )  ) 
         {
            /.
            The object is not a star.
            ./
            
            range[i] = vnorm_c ( state[i] );
         }
         else 
         {
            range[i] = sqrt ( dpmax_() ) / 2.;
         }
      }



   2) Initialize a CSPICE "window."
   
      /.
      The window originally has one interval, from "minus 
      infinity" to "plus infinity". 
      ./
      
      winsiz    =  2;
      window[0] =  dpmin_();
      window[1] =  dpmax_() ;

      scardd_ ( &winsiz, window ); 
      
 
-Restrictions
 
   1) This routine should not be called from within users' applications.
      Instead, use dpmax_c. 
 
-Literature_References
 
   None.
    
-Author_and_Institution
 
   N.J. Bachman    (JPL) 
   H.A. Neilan     (JPL) 
   M.J. Spencer    (JPL) 
   W.L. Taber      (JPL) 
   I.M. Underwood  (JPL) 
 
-Version
 
   -CSPICE Version 1.0.0, 19-OCT-1998 (NJB)

-Index_Entries
 
   largest d.p. number 
 
-&
*/

{ /* Begin dpmax_ */


   return ( DBL_MAX );
   

} /* End dpmax_ */

