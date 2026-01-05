/*

-Procedure zzadstep_c ( GF, adapter for step size function )

-Abstract
 
   Provide an f2c-style interface allowing f2c'd Fortran
   code to call a CSPICE-style GF stepsize function.

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

   int zzadstep_c ( doublereal  * time,
                    doublereal  * step  ) 

/*

-Brief_I/O
 
   VARIABLE  I/O  DESCRIPTION 
   --------  ---  -------------------------------------------------- 
   time       I   Time from which the next step will be taken. 
   step       O   Time step to take. 
 
-Detailed_Input
  
   time     is the input start time from which the algorithm is to
            search forward for a state transition. `time' is expressed
            as seconds past J2000 TDB.  
 

-Detailed_Output  
 
   step     is the output step size. `step' is the value stored via the
            last call to gfsstp_c. Units are TDB seconds.
 
-Parameters
 
   None. 
 
-Exceptions
 
   1) A run-time error will result if this routine is called before
      a valid pointer to a CSPICE-style GF step size function has
      been stored via a call to zzadsave_c.

      The argument list of the stored function must match that of
      gfstep_c.
 
-Files
 
   None. 
 
-Particulars
 
   This routine is meant to be passed to f2c'd Fortran GF code
   that requires a step size function input argument. The argument
   list of this routine matches that of the f2c'd routine

      gfstep_

   This routine calls the CSPICE-style stepsize function passed
   into a CSPICE wrapper for an intermediate-level GF function.
   A pointer to this step size function must be stored via
   a call to zzadsave_c before this routine is called.

   When set properly, `step' indicates how far to advance `time' so
   that `time' and `time+step' may bracket a state transition and
   definitely do not bracket more than one state transition.

   The calling application can change the step size value via the entry
   point gfsstp_c.
 
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
 
   -CSPICE Version 1.0.0, 24-MAR-2008 (NJB)

-Index_Entries
 
   adapter for gf step size function
 

-&
*/

{ /* Begin zzadstep_c */


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
   chkin_c ( "zzadstep_c" );

   /*
   Retrieve the stored pointer for the passed-in function; cast
   the pointer from (void *) to that of a function whose argument
   list matches that of gfstep_c.
   */

   fPtr = (  void (*) (SpiceDouble, SpiceDouble*)  )  zzadget_c ( UDSTEP );

   /*
   Call the stored function. 
   */
   
   (*fPtr) ( (SpiceDouble)(*time), (SpiceDouble *)step );


   chkout_c ( "zzadstep_c" );

   return ( 0 );

} /* End zzadstep_c */
