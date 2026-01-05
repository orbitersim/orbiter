/*

-Procedure zzgetcml_c ( Get the command line )

-Abstract

   Store the contents of argv and argc for later access.

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

   #include <string.h>
   #include <stdlib.h>
   #include <stdio.h>

   #include "SpiceUsr.h"
   #include "SpiceZpl.h"

   #ifdef CSPICE_MACPPC
   
   #include <console.h>

   #endif

   void zzgetcml_c ( SpiceInt         *  argc,
                     SpiceChar      ***  argv,
                     SpiceBoolean        init )

/*

-Brief_I/O

   Variable  I/O  Description
   --------  ---  --------------------------------------------------
   argc      I/O   The number of command line arguments.
   argv      I/O   The vector of command line arguments.
   init       I    Boolean indicating whether the call should
                   initialize the internal storage variables.

-Detailed_Input

   argc      contains the number of command line arguments.

   argv      is the vector of space delimited command line arguments.
             Each entry entry contains one argument.  argv[0] holds the
             command name.

   init      contains SPICETRUE if the call is to store the argv and argc
             data, and SPICEFALSE if the call is to retrieve the data.

-Detailed_Output

   See above.

-Parameters

   None.

-Exceptions

   1).  The first call to this routine should be from putcml_c.  This
        stores the data values.  If putcml_c does not make the first
        call, the error SPICE(PUTCMLNOTCALLED) signals

   2).  Only one putcml_c call should be made in any given program. The error
        SPICE(PUTCMLCALLEDTWICE) signals for all subsequent putcml_c calls.

-Files

   None.

-Particulars

   Do not directly call zzgetcml_c!

   This routine allows access to argv and argc from any program module.
   The routine must be initialized in the main module prior to any
   retrieval.  Initialization occurs in putcml_c, access to stored
   information occurs via getcml_c.  

-Examples

   #include <stdio.h>
   #include <stdlib.h>

   #include "SpiceUsr.h"

   SpiceInt     i;

   void main( int argc, char *argv[] )
      {


      /. Store argv and argc for latter access. ./

      putcml_c ( argc, argv );


      ..... other CRA stuff .....
      .....                 .....
      
      goop1();
      
      .....      stuff      ..... 

      goop2();


      return 0;
      }

   void goop1 ()
      {

      SpiceInt      argc;
      SpiceChar  ** argv;


      /. Retrieve the argc and argv values ./
      
      getcml_c ( &argc, &argv );
      
      for ( i=0; i<argc; ++i)
         {
         printf ( "Argv %d: %s\n", i, argv[i] );
         }

      }

   void goop2 ()
      {
   
      SpiceInt      argc;
      SpiceChar  ** argv;

      /. Retrieve the argc and argv values ./

      getcml_c ( &argc, &argv );
      
      for ( i=0; i<argc; ++i)
         {
         printf ( "Argv %d: %s\n", i, argv[i] );
         }

      }


-Restrictions

   This routine includes ifdef delimited code specific to the 
   Metrowerks compiler for the classic Macintosh operating system.
   NAIF no longer supports that environment and so has not
   tested this routine in that environment.

-Literature_References

   None.

-Author_and_Institution

   E.D. Wright    (JPL)

-Version

   -CSPICE Version 1.2.0  19-JAN-2010   (EDW)
   
      Copy operation now performs a deep copy of argv rather than a shallow copy
      of pointer values.

   -CSPICE Version 1.1.1  08-MAR-2002   (EDW)

      Corrected typo in header. Procedure renamed from getclm_c to zzgetcml.
   
   -CSPICE Version 1.1.0  11-OCT-2001   (EDW)

      Included Mac PPC classic specific code for the Metrowerks compiler. The
      code causes a window to display for input of command line arguments.
       
      Update examples.

   -CSPICE Version 1.0.0  08-FEB-1998   (EDW)

-Index_Entries

   store/retrieve argc argv

-&
*/

{

   /* Local variables */

   static SpiceInt          CML_argc;
   static SpiceChar      ** CML_argv;

   static SpiceBoolean      first    = SPICETRUE;

   SpiceInt                 i;


   /* Participate in error tracing. */

   chkin_c ( "zzgetcml_c" );


   /*
   On first entry into the routine, store the information
   then leave. If not first, load the stored information into the
   return variables.

   Check that putcml_c called this routine before getcml_c, and that
   there is only one call to putcml_c.
   */

   if ( first && init )
      {

      /* 
      First call from putcml_c.  Deep store the values for later use.
      If a Mac, and using CodeWarrior, open a window for command line 
      access.
      */

      #ifdef CSPICE_MACPPC

      *argc = (SpiceInt) ccommand ( argv);

      #endif

      CML_argc = *argc;
   
      /*
      Allocate an array of pointers for the argv array.
      */
      CML_argv = (SpiceChar**)malloc( sizeof(SpiceChar*) * CML_argc );

      /*
      Check for a malloc failure. Signal a SPICE error if error found.
      */
      if ( CML_argv == NULL )
         {

         setmsg_c ( "Malloc failed to allocate space for a "
                    "SpiceChar* array of length #. ");
         errint_c ( "#", (SpiceInt) CML_argc );
         sigerr_c ( "SPICE(MALLOCFAILED)"    );
         chkout_c ( "zzgetcml_c"             );
         return;
         }

      /*
      Copy from argv to the local CML_argv array.
      */
      for ( i=0; i< *argc; ++i)
         {
         SpiceInt len = strlen((*argv)[i]);
         
         /*
         Allocate the needed memory for each argv string.
         */
         CML_argv[i] = (SpiceChar*)malloc( sizeof(SpiceChar) 
                                           *
                                          (1 + len) 
                                          );

         if ( CML_argv[i] == NULL )
            {

            setmsg_c ( "Malloc failed to allocate space for a "
                       "SpiceChar array of length #. ");
            errint_c ( "#", (SpiceInt) (1 + len) );
            sigerr_c ( "SPICE(MALLOCFAILED)"     );
            chkout_c ( "zzgetcml_c"              );
            return;
            }

         (void)strncpy( CML_argv[i], (*argv)[i], 1 + len );
         }

      /* 
      Set the first flag to false. This block needs run only once. 
      */

      first     = SPICEFALSE;

      }

   else if ( first && !init )
      {

      /*
      This was the first call, but the call did not come from
      putcml_c
      */

      setmsg_c ( "getcml_c called without putcml_c initialization" );
      sigerr_c ( "SPICE(PUTCMLNOTCALLED)"                          );
      chkout_c ( "zzgetcml_c" );
      return;
      }

   else if ( !first && init )
      {

      /* This is not the first call, but the call came from putcml_c */

      setmsg_c ( "Illegal attempt to reinitialize with putcml_c" );
      sigerr_c ( "SPICE(PUTCMLCALLEDTWICE)"                      );
      chkout_c ( "zzgetcml_c" );
      return;
      }

   else
      {

      /* A non-first getcml_c call. return the stored values of argv and argc.*/

      *argc     = CML_argc;
      *argv     = CML_argv;

      }


   /* Check out and return. */

   chkout_c ( "zzgetcml_c" );

}


