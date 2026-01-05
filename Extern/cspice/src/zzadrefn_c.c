/*

-Procedure zzadrefn_c ( GF, adapter for refinement function )

-Abstract
 
   Provide an f2c-style interface allowing f2c'd Fortran
   code to call a CSPICE-style GF refinement function.
 
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


   int zzadrefn_c ( doublereal  * t1, 
                    doublereal  * t2, 
                    logical     * s1, 
                    logical     * s2, 
                    doublereal  * t   )

/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   t1         I   One of two times bracketing a state change. 
   t2         I   The other time that brackets a state change. 
   s1         I   State at t1.
   s2         I   State at t2.
   t          O   New time at which to check for transition.
 
-Detailed_Input
 
   t1         One of two times bracketing a state change. 
              `t1' is expressed as seconds past J2000 TDB.  

   t2         The other time that brackets a state change. 
              `t2' is expressed as seconds past J2000 TDB.  
 
   n1         Number of times state state of interest 
              matched the value at t1.
 
   n2         Number of times state state of interest 
              matched the value at t2.
 
-Detailed_Output
 
   t          is the value returned by the stored, passed-in
              refinement function. 
 
-Parameters
 
   None. 
 
-Exceptions

   1) A run-time error will result if this routine is called before
      a valid pointer to a CSPICE-style GF refinement function has
      been stored via a call to zzadsave_c.

      The argument list of the stored function must match that of
      gfrefn_c.
  
-Files
 
   None. 
 
-Particulars
 
   This routine is meant to be passed to f2c'd Fortran GF code
   that requires a refinement function input argument. The argument
   list of this routine matches that of the f2c'd routine

      gfrefn_

   This routine calls the CSPICE-style refinement function passed
   into a CSPICE wrapper for an intermediate-level GF function.
   A pointer to this refinement function must be stored via
   a call to zzadsave_c before this routine is called.
 
-Examples
 
    None. 
 
-Restrictions
 
    No errors are returned by this routine. 
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman   (JPL)
 
-Version
    
   -CSPICE Version 1.0.0, 09-MAR-2009 (NJB)

-Index_Entries
 
   adapter for gf refinement function
 
-&
*/

{ /* Begin zzadrefn_c */

   
   /*
   Local variables 
   */
   SpiceBoolean            bs1;
   SpiceBoolean            bs2;

   void                ( * fPtr ) ( SpiceDouble,
                                    SpiceDouble,
                                    SpiceBoolean,
                                    SpiceBoolean,
                                    SpiceDouble * );

   /*
   Participate in error tracing.
   */
   if ( return_c() )
   {
      return ( 0 );
   }
   chkin_c ( "zzadrefn_c" );


   /*
   Retrieve the stored pointer for the passed-in function; cast
   the pointer from (void *) to that of a function whose argument
   list matches that of gfrefn_c.
   */

   fPtr = (  void (*) ( SpiceDouble, 
                        SpiceDouble,
                        SpiceBoolean,
                        SpiceBoolean,
                        SpiceDouble * )  )   zzadget_c ( UDREFN );

   /*
   Call the stored function. 
   */
   
   bs1 = (SpiceBoolean) (*s1);
   bs2 = (SpiceBoolean) (*s2);

   (*fPtr) ( (SpiceDouble  ) (*t1),
             (SpiceDouble  ) (*t2),
             bs1,
             bs2,
             (SpiceDouble *) t       );


   chkout_c ( "zzadrefn_c" );

   return ( 0 );


} /* End zzadrefn_c */
