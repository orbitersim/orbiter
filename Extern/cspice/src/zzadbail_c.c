/*

-Procedure zzadbail_c (GF, bail out inquiry adapter )

-Abstract
 
   Provide an f2c-style interface allowing f2c'd Fortran code to call a
   CSPICE-style GF bail out inquiry function.
 
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
 
   UTILITY 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"
   #include "SpiceZst.h"
   #include "SpiceZad.h"
   #undef   zzadbail_c

   logical zzadbail_c ()

 
/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   
   The function returns the logical value SPICEFALSE.

-Detailed_Input

   None.

-Detailed_Output
 
   The function returns the logical value SPICEFALSE.   
 
-Parameters
 
   None. 
 
-Exceptions
 
   1) A run-time error will result if this routine is called before
      a valid pointer to a CSPICE-style GF bail out inquiry function
      has been stored via a call to zzadsave_c.

      The argument list of the stored function must match that of
      gfbail_c.
    
-Files
 
   None. 
 
-Particulars
  
   This routine is meant to be passed to f2c'd Fortran GF code that
   requires a "bail out" inquiry function input argument. This 
   function tests whether the current GF search should be terminated
   in response to an interrupt.

   The argument list of this routine matches that of the f2c'd routine

      gfbail_

   This routine calls the CSPICE-style bail out inquiry function passed
   into a CSPICE wrapper for an intermediate-level GF function. A
   pointer to this bail out inquiry function must be stored via a call
   to zzadsave_c before this routine is called.
 
   The argument list of the function referenced by the saved pointer
   must match that of
 
      gfbail_c

-Examples

   None.

-Restrictions
 
   None. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman   (JPL)
   W.L. Taber     (JPL) 
   I.M. Underwood (JPL) 
   L.S. Elson     (JPL) 
   E.D. Wright    (JPL)  
 
-Version
 
   -CSPICE Version 1.0.0, 25-MAR-2008 (NJB)

-Index_Entries
 
   adapter for gf bail out inquiry
 
-&
*/

{ /* Begin zzadbail_c */


   /*
   Local variables 
   */
   logical                 retval;

   SpiceBoolean        ( * fPtr ) ();
            

   /*
   Participate in error tracing.
   */
   if ( return_c() )
   {
      return ( (logical)SPICEFALSE );
   }
   chkin_c ( "zzadbail_c" );


   /*
   Retrieve the stored pointer for the passed-in function; cast
   the pointer from (void *) to that of a function whose argument
   list matches that of gfbail_c.
   */

   fPtr = (  SpiceBoolean (*) ()  )  zzadget_c ( UDBAIL );
 
   /*
   Call the CSPICE-style bail-out function. 
   */

   retval = (logical) (  ( *fPtr )()  );
              

   chkout_c ( "zzadbail_c" );

   return ( retval );

} /* End zzadbail_c */
