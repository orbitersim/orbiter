/*

-Procedure zzadqdec_c ( Private - GF, user defined boolean adapter )

-Abstract
 
   Provide an f2c-style interface allowing f2c'd Fortran
   code to call a CSPICE-style GF routine that determines
   the value of a user defined boolean function.

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

   int zzadqdec_c ( U_fp           udfunc,
                    doublereal   * et,
                    logical      * xbool )

/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   udfunc     I   Name of scalar function of interest.
   et         I   Epoch of interest in TDB seconds. 
   xbool      O   Boolean value at `et'.
 
-Detailed_Input

   udfunc     the name of the external routine that returns the
              value of the scalar quantity of interest at time `et'.

   et         a double precision value representing
              ephemeris time, expressed as seconds past
              J2000 TDB, at which to evaluate "udfunb."

-Detailed_Output  

   xbool      the value of the boolean quantity function at `et'.

-Parameters
 
   None. 
 
-Exceptions
 
   1) A run-time error will result if this routine is called before
      a valid pointer to a CSPICE-style function has been stored via
      a call to zzadqdec_c.

      The argument list of the stored function must match that of
      udqdec (refer to gfuds_c.c).
 
-Files
 
   None. 
 
-Particulars
 
   This routine is meant to be passed to f2c'd Fortran GF code that
   requires a derivative sign test function as an argument.

   This routine calls the CSPICE-style derivative test function
   passed to a CSPICE wrapper for use by an intermediate-level GF
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

   -CSPICE Version 2.0.0, 23-OCT-2013 (EDW)

 
   -CSPICE Version 1.0.0, 21-DEC-2008 (EDW)

-Index_Entries
 
   adapter for gf user defined boolean quantity

-&
*/

   { /* Begin zzadqdec_c */

   /*
   Local variables 
   */
   void           ( * fPtr ) ( void ( * ) ( SpiceDouble,
                                            SpiceDouble  *),
                               SpiceDouble,
                               SpiceBoolean * );

   void           ( * fPtr2) ( SpiceDouble,
                               SpiceDouble * );

   SpiceBoolean       bool_loc;

   /*
   Participate in error tracing.
   */

   if ( return_c() )
      {
      return ( 0 );
      }
   chkin_c ( "zzadqdec_c" );

   /*
   Retrieve the stored pointer for the passed-in function; cast
   the pointer from (void *) to that of a function whose argument
   list matches that of "udqdec."
   */
   fPtr = ( void (*) ( void ( * ) ( SpiceDouble, SpiceDouble  *),
                       SpiceDouble, 
                       SpiceBoolean*) ) zzadget_c ( UDQDEC );

   /*
   Retrieve the stored pointer for the user defined scalar function. The
   'udfunc' pointer passed to zzadqdec_c as an argument corresponds to 
   the adapter for the scalar function, but the function pointer 
   argument in 'fPtr' requires the non-adapter pointer. Ignore 'udfunc'.
   */
   fPtr2= ( void (*) (SpiceDouble, SpiceDouble*) ) zzadget_c ( UDFUNC );

   /*
   Call the stored function. 
   */
   (*fPtr) ( fPtr2, (SpiceDouble)(*et), (SpiceBoolean *) &bool_loc );

   /*
   Cast the "SpiceBoolean" to "logical" to prevent any future size mismatches
   or compiler warnings.
   */
   *xbool = (logical) bool_loc;

   chkout_c ( "zzadqdec_c" );

   return ( 0 );

   } /* End zzadqdec_c */
