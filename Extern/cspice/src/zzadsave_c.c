/*

-Procedure zzadsave_c ( Save passed-in function pointer arguments )

-Abstract
 
   CSPICE Private routine intended solely for the support of CSPICE
   routines.  Users should not call this routine directly due
   to the volatile nature of this routine.

   Save passed-in function pointer arguments to make them available
   for use by CSPICE adapter functions.
 
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
 
   None 
 
-Keywords
 
   None 
 
*/

   #include "SpiceUsr.h"
   #include "SpiceZst.h"
   #include "SpiceZad.h"

   /*
   Static file scope variables 

   The function pointer list is accessed by the functions

      zzadsave_c ( Save a function pointer for adapter use )
      zzadget_c  ( Get a function pointer for adapter use  )
   
   */
   static void *     funcPtrList [ SPICE_N_PASSED_IN_FUNC ];




   void zzadsave_c ( SpicePassedInFunc    funcID,
                     void               * funcPtr ) 
/*

-Brief_I/O

   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   funcID     I   Enum constant identifying function.
   funcPtr    I   Function pointer acting as a passed-in argument.
 
-Detailed_Input
 
   funcID         is an ID of type SpicePassedInFunc; `funcID'
                  identifies the function pointed to by the
                  input argument `funcPtr'.
   
   funcPtr        is a function pointer acting as a passed-in 
                  argument to a CSPICE wrapper. This function 
                  pointer is to be stored so that the function
                  it points to can be called by a CSPICE adapter. 
 
-Detailed_Output
 
   None. This routine operates by side effects.
 
-Parameters
 
   None. 
 
-Exceptions
 
   1) If the input ID `funcID' is not in the range corresponding
      to the values of the enum SpicePassedInFunc, the error
      SPICE(VALUEOUTOFRANGE) is signaled.
 
-Files
 
   None. 
 
-Particulars
 
   This private utility package provides access to a static array
   of function pointers. The package contains the two functions

      zzadsave_c ( Save a function pointer for adapter use )
      zzadget_c  ( Get a function pointer for adapter use  )

   These two function are present in the same file because the
   functions share data. The shared array is declared at file
   scope rather than as an extern variable to limit access. The
   effect is similar to that of sharing data between Fortran 
   entry points.

   The stored function pointers are associated with passed-in function
   pointer arguments of CSPICE wrappers. These functions are called
   by CSPICE adapter routines. 
 
-Examples
 
   1) Store a pointer to the default GF step routine. Retrieve
      the pointer and call the function using this pointer.


         #include <stdio.h>
         #include "SpiceUsr.h"
         #include "SpiceZfc.h"
         #include "SpiceZad.h"

         int main()
         {

            /.
            Declare `userstepPtr' as a pointer to a function of the 
            type of the GF default step function:
            ./
            void                ( * userstepPtr ) ( SpiceDouble    et,
                                                    SpiceDouble  * step );

            SpiceDouble             et;
            SpiceDouble             step;


            /.
            Store a pointer to the GF default step function. 
            ./
            zzadsave_c ( UDSTEP, gfstep_c );

            /.
            Set step size to 5 minutes (units are seconds).
            ./
            gfsstp_c ( 300.0 );

            /.
            Fetch the desired pointer and cast to the type of the GF
            step function: 
            ./
            userstepPtr = ( void (*)(SpiceDouble, 
                                     SpiceDouble*) ) zzadget_c( UDSTEP );

            /.
            Call the function for a given ET and retrieve the step size: 
            ./
            et = 1.e8;

            userstepPtr ( et, &step );

            printf ( "Returned step size was %f\n", step );

            return ( 0 );
         }


-Restrictions
 
   1) These utilities must be used only to store function pointers
      to be used by existing CSPICE adapter routines. See the header
      file 

         SpiceZad.h

      for the list of supported routines.
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman   (JPL) 
 
-Version
 
   -CSPICE Version 1.0.0, 09-MAR-2009 (NJB)

-Index_Entries
 
   save passed-in function pointer argument
 
-&
*/

{ /* Begin zzadsave_c */

  

   /*
   Participate in error tracing.
   */
   chkin_c ( "zzadsave_c" );


   /*
   Make sure function ID is in range.
   */

   if (  ( funcID < 0 ) || ( funcID >= SPICE_N_PASSED_IN_FUNC )  )
   {
      setmsg_c ( "Input function ID was #; valid range is 0:#. "
                 "Function ID doesn't correspond to a known "
                 "passed-in function argument."                  );
      errint_c ( "#",  (SpiceInt) funcID                         );
      errint_c ( "#",  (SpiceInt) SPICE_N_PASSED_IN_FUNC - 1     );
      sigerr_c ( "SPICE(VALUEOUTOFRANGE)"                        );
      chkout_c ( "zzadsave_c"                                    );
      return;      
   }
   

   /*
   Store the function pointer at the index indicated by the 
   function ID.
   */   
   funcPtrList[ funcID ] = funcPtr;


   chkout_c ( "zzadsave_c" );

} /* End zzadsave_c */




/*

-Procedure zzadget_c ( Get passed-in function pointer arguments )

-Abstract
 
   CSPICE Private routine intended solely for the support of CSPICE
   routines.  Users should not call this routine directly due
   to the volatile nature of this routine.

   Get passed-in function pointer arguments to make them available
   for use by CSPICE adapter functions.
 
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
 
   None 
 
-Keywords
 
   None 
 
*/

   void * zzadget_c ( SpicePassedInFunc    funcID )
 
/*

-Brief_I/O

   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   funcID     I   Enum constant identifying function.
   
   This function returns the specified function pointer acting as 
   a passed-in argument.
 
-Detailed_Input
 
   funcID         is an ID of type SpicePassedInFunc; `funcID'
                  identifies the function whose saved pointer
                  is to be returned.

-Detailed_Output
 
   This function returns the specified function pointer. The caller
   should cast the pointer to the correct type to allow compile-time
   type checking.
 
-Parameters
 
   None. 
 
-Exceptions
 
   1) If the input ID `funcID' is not in the range corresponding
      to the values of the enum SpicePassedInFunc, the error
      SPICE(VALUEOUTOFRANGE) is signaled.
 
-Files
 
   None. 
 
-Particulars
 
   See the Particulars section of zzadsave_c.
 
-Examples
 
   See the Examples section of zzadsave_c.
   
-Restrictions
 
   1) These utilities must be used only to store function pointers
      to be used by existing CSPICE adapter routines. See the header
      file 

         SpiceZad.h

      for the list of supported routines.
 
-Literature_References
 
   None. 
 
-Author_and_Institution
 
   N.J. Bachman   (JPL) 
 
-Version
 
   -CSPICE Version 1.0.0, 09-MAR-2009 (NJB)

-Index_Entries
 
   get passed-in function pointer argument
 
-&
*/

{ /* Begin zzadget_c */

  
   /*
   Participate in error tracing.
   */
   chkin_c ( "zzadget_c" );

   /*
   Make sure function ID is in range.
   */
   if (  ( funcID < 0 ) || ( funcID >= SPICE_N_PASSED_IN_FUNC )  )
   {
      setmsg_c ( "Input function ID was #; valid range is 0:#. "
                 "Function ID doesn't correspond to a known "
                 "passed-in function argument."                  );
      errint_c ( "#",  (SpiceInt) funcID                         );
      errint_c ( "#",  (SpiceInt) SPICE_N_PASSED_IN_FUNC - 1     );
      sigerr_c ( "SPICE(VALUEOUTOFRANGE)"                        );
      chkout_c ( "zzadget_c"                                     );

      /*
      Return an invalid pointer if we can't perform the lookup. 
      */
      return ( 0 );      
   }
   
   /*
   Check-out now since this is a non-void function. 
   */
   chkout_c ( "zzadget_c" );

   /*
   Return the function pointer as a void pointer.
   */   
   return (  funcPtrList[ funcID ]  );


} /* End zzadget_c */
