/*

-Procedure intmax_ ( Largest integer number )

-Abstract
 
   Return the value of the largest positive number representable 
   in a variable of type "integer."
 
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

   #include <limits.h>
   #include "SpiceUsr.h"

   SpiceInt intmax_ () 

/*

-Brief_I/O
 
   The function returns the value of the largest positive number 
   that can be represented in a variable of type "integer." 
 
-Detailed_Input
 
   None. 
 
-Detailed_Output
 
   The function returns the value of the largest positive number 
   that can be represented in an "integer" variable, where integer
   is a typedef defined in f2c.h.  The typedef SpiceInt always maps
   to the same type as does the f2c typedef integer.
   
   The returned value will be greater than or equal to 2147483647.
   See the Particulars section for details.

-Parameters
 
   None. 
 
-Exceptions
 
   Error free. 
 
-Files
 
   None. 
 
-Particulars
 
   This function replaces that produced by running f2c on the Fortran
   SPICELIB file intmax.f. 

   When translating Fortran code, f2c maps Fortran variables of type
   INTEGER to C variables of type "integer," where integer is a typedef
   defined in the f2c header file f2c.h.  On all supported platforms, 
   Fortran INTEGERS occupy at least 32 bits.  On most platforms, this 
   means that the typedef integer translates to type long.  There are 
   some exceptional platforms on which an integer translates to type 
   int.  The mapping must provide compatibility with the f2c typedef
   doublereal:  integers must occupy half the storage of doublereals in 
   order for these types to correctly represent the Fortran types 
   INTEGER and DOUBLE PRECISION.
   
   On systems where the typedef integer maps to type long, the return
   value is defined by the macro LONG_MAX from the ANSI standard header
   file limits.h. According to the ANSI standard, LONG_MAX must be at  
   least
   
      2147483647 
    
   This is 
   
        31
       2   - 1 
      
   On systems where the typedef integer maps to type int, the value is
   defined by the macro INT_MAX from the ANSI standard header file 
   limits.h. According to the ANSI standard, INT_MAX must be at least
      
      32767
    
   This is 
   
       15
      2   - 1 
            
   In practice however, the typedef integer will map to type int only
   if ints occupy at least four bytes, so the value of INT_MAX will
   actually be at least 2147483647.
   
   
-Examples
 
   The following code fragment illustrates the use of intmax_. 
 
      /.
      Separate a double into integer and fractional components.
      If the integer component is out of range, avoid overflow 
      by making it as large as possible. 
      ./
      #include <math.h>
               .
               .
               .
      fract = modf ( dvalue, &integralDP ); 
      
      if (  integralDP  >  (double)intmax_()  ) 
      { 
         ivalue = intmax_();
      }
      else if (  integralDP  <  (double)intmin_()  )   
      {
         ivalue = intmin_(); 
      }
      else 
      {
         ivalue = (long)( integralDP );
      }

 
-Restrictions
 
   1) This routine should not be called from within users' applications.
      Instead, use intmax_c. 
 
-Literature_References
 
   None.
 
-Author_and_Institution
 
   N.J. Bachman    (JPL) 
   H.A. Neilan     (JPL) 
   B.V. Semenov    (JPL) 
   M.J. Spencer    (JPL) 
   W.L. Taber      (JPL) 
   I.M. Underwood  (JPL) 
   E.D. Wright     (JPL) 
 
-Version

   -CSPICE Version 2.3.0, 28-NOV-2021 (BVS)

      Updated for:
       
         MAC-OSX-M1-64BIT-CLANG_C
          
      environment. Added the corresponding tag:
       
         CSPICE_MAC_OSX_M1_64BIT_CLANG

      tag to the #ifdefs set.

   -CSPICE Version 2.2.0, 10-MAR-2014 (BVS)

       Updated for:
       
          PC-CYGWIN-64BIT-GCC_C
          
       environment. Added the corresponding tag:
       
          CSPICE_PC_CYGWIN_64BIT_GCC

       tag to the #ifdefs set.
        
   -CSPICE Version 2.1.0, 14-MAY-2010 (EDW)(BVS)

       Updated for:
       
          MAC-OSX-64BIT-INTEL_C
          PC-64BIT-MS_C
          SUN-SOLARIS-64BIT-NATIVE_C
          SUN-SOLARIS-INTEL-64BIT-CC_C

       environments. Added the corresponding tags:
       
          CSPICE_MAC_OSX_INTEL_64BIT_GCC
          CSPICE_PC_64BIT_MS
          CSPICE_SUN_SOLARIS_64BIT_NATIVE
          CSPICE_SUN_SOLARIS_INTEL_64BIT_CC

       tag to the #ifdefs set.

   -CSPICE Version 2.0.0, 21-FEB-2006 (NJB)

       Updated to support the PC Linux 64 bit mode/gcc platform.

   -CSPICE Version 1.2.0, 27-JAN-2003 (NJB)

       Updated to support the Sun Solaris 64 bit mode/gcc platform.

   -CSPICE Version 1.1.0, 29-JAN-1999 (NJB)

       Updated to select INT_MAX or LONG_MAX depending on the 
       host environment.
     
   -CSPICE Version 1.0.0, 16-OCT-1998 (NJB)

-Index_Entries
 
   largest integer number 
 
-&
*/

{ /* Begin intmax_ */

   #ifdef CSPICE_ALPHA_DIGITAL_UNIX
        
       return ( INT_MAX );

   #elif defined( CSPICE_PC_CYGWIN_64BIT_GCC )
        
       return ( INT_MAX );

   #elif defined( CSPICE_PC_LINUX_64BIT_GCC )
        
       return ( INT_MAX );

   #elif defined( CSPICE_SUN_SOLARIS_64BIT_GCC )

       return ( INT_MAX );

   #elif defined( CSPICE_MAC_OSX_INTEL_64BIT_GCC )

       return ( INT_MAX );

   #elif defined( CSPICE_MAC_OSX_M1_64BIT_CLANG )

       return ( INT_MAX );

   #elif defined( CSPICE_PC_64BIT_MS )

       return ( INT_MAX );

   #elif defined( CSPICE_SUN_SOLARIS_64BIT_NATIVE )

       return ( INT_MAX );

   #elif defined( CSPICE_SUN_SOLARIS_INTEL_64BIT_CC )

       return ( INT_MAX );

   #else

       return ( LONG_MAX );

   #endif
        

} /* End intmax_ */

