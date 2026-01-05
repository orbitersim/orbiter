/*

-Procedure zzadfunc_c ( Private - GF, f(x) adapter )

-Abstract
 
   Provide an f2c-style interface allowing f2c'd Fortran
   code to call a CSPICE-style routine that calculates
   the gfuds_c scalar quantity value.

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
   UTILITY 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZad.h"

   int zzadfunc_c ( doublereal   * et,
                    doublereal   * value )

/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   et         I   Epoch of interest in TDB seconds 
   value      O   Function value at 'et'
 
-Detailed_Input
  
   et         The epoch in TDB seconds for which to calculate the user
              defined scalar quantity function.

-Detailed_Output  
 
   value     The double precision value of the scalar quantity function
             at 'et'.
 
-Parameters
 
   None. 
 
-Exceptions
 
   1) A run-time error will result if this routine is called before
      a valid pointer to a CSPICE-style function has been stored via
      a call to zzadfunc_c.

      The argument list of the stored function must match that of
      udfunc (refer to gfuds_c.c).
 
-Files
 
   None. 
 
-Particulars
 
   This routine is meant to be passed to f2c'd Fortran GF code that
   requires a user defined scalar value function as an argument.

   This routine calls the CSPICE-style scalar value function passed
   to a CSPICE wrapper for use by an intermediate-level GF
   function. A pointer to this function must be stored via a call 
   to zzadsave_c before this routine is called.

-Examples
 
   None. 
 
-Restrictions
 
   1) This function is intended only for internal use by GF routines.   
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman   (JPL)
   L.S. Elson     (JPL)
   W.L. Taber     (JPL) 
   I.M. Underwood (JPL) 
   E.D. Wright    (JPL)  
 
-Version
 
   -CSPICE Version 1.0.0, 24-OCT-2008 (EDW)

-Index_Entries
 
   adapter for gf scalar value routine

-&
*/

{ /* Begin zzadfunc_c */


   /*
   Local variables 
   */
   void           ( * fPtr ) ( SpiceDouble,
                               SpiceDouble * );

   /*
   Participate in error tracing.
   */

   if ( return_c() )
      {
      return ( 0 );
      }
   chkin_c ( "zzadfunc_c" );

   /*
   Retrieve the stored pointer for the passed-in function; cast
   the pointer from (void *) to that of a function whose argument
   list matches that of "udfunc."
   */

   fPtr = (  void (*) (SpiceDouble, SpiceDouble*)  ) zzadget_c ( UDFUNC );

   /*
   Call the stored function. 
   */
   
   (*fPtr) ( (SpiceDouble)(*et), (SpiceDouble *)value );


   chkout_c ( "zzadfunc_c" );

   return ( 0 );

} /* End zzadfunc_c */
