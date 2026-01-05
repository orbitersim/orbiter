/*

-Procedure zzadrepf_c (GF, progress report termination adapter )

-Abstract
 
   Provide an f2c-style interface allowing f2c'd Fortran code to call a
   CSPICE-style GF progress reporting termination function.
 
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
   #undef   zzadrepf_c

   int zzadrepf_c ()

 
/*

-Brief_I/O

   None.

-Detailed_Input

   None.

-Detailed_Output
 
   None. 
 
-Parameters
 
   None. 
 
-Exceptions
 
   1) A run-time error will result if this routine is called before
      a valid pointer to a CSPICE-style GF progress reporting
      termination function has been stored via a call to zzadsave_c.

      The argument list of the stored function must match that of
      gfrepf_c.
    
-Files
 
   None. 
 
-Particulars
  
   This routine is meant to be passed to f2c'd Fortran GF code that
   requires a progress reporting termination function input argument.
   The argument list of this routine matches that of the f2c'd routine

      gfrepf_

   This routine calls the CSPICE-style progress reporting termination
   function passed into a CSPICE wrapper for an intermediate-level GF
   function. A pointer to this progress reporting termination function
   must be stored via a call to zzadsave_c before this routine is
   called.
 
   The argument list of the function referenced by the saved pointer
   must match that of
 
      gfrepf_c

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
 
   -CSPICE Version 1.0.0, 29-MAR-2008 (NJB)

-Index_Entries
 
   terminate a gf progress report
 
-&
*/

{ /* Begin zzadrepf_c */


   /*
   Local variables 
   */
   void               ( * fPtr ) ();
                           

   /*
   Participate in error tracing.
   */
   if ( return_c() )
   {
      return ( 0 );
   }
   chkin_c ( "zzadrepf_c" );


   /*
   Retrieve the stored pointer for the passed-in function; cast
   the pointer from (void *) to that of a function whose argument
   list matches that of gfrepf_c.
   */

   fPtr = (  void (*) ()  )  zzadget_c ( UDREPF );
 
   /*
   Call the CSPICE-style progress report termination function. 
   */
   
   ( *fPtr ) ();
              

   chkout_c ( "zzadrepf_c" );

   return ( 0 );

} /* End zzadrepf_c */
