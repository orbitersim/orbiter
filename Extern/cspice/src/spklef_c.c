/*

-Procedure spklef_c (  S/P Kernel, Load ephemeris file )

-Abstract

   Load an ephemeris file for use by the readers. Return that file's
   handle, to be used by other SPK routines to refer to the file.

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

   SPK

-Keywords

   EPHEMERIS
   FILES

*/
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"


   void spklef_c ( ConstSpiceChar * fname,
                   SpiceInt       * handle )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   fname      I   Name of the file to be loaded.
   handle     O   Loaded file's handle.

-Detailed_Input

   fname       is a string containing the name of the file to be loaded.

-Detailed_Output

   handle      is an integer handle assigned to the file upon loading.
               Almost every other SPK routine will subsequently use this
               number to refer to the file.

-Parameters

   FTSIZE      is the maximum number of SPK files that may
               be loaded simultaneously under any circumstances.
               FTSIZE is currently set to match the maximum number
               of DAF files that may be loaded simultaneously.

-Exceptions

   1)  If an attempt is made to open more DAF files than is specified by the
       parameter FTSIZE in the DAF subsystem, an error is signaled by a
       routine in the call tree of this routine.

   2)  If an attempt is made to load more files than is specified by
       the local parameter FTSIZE, and if the DAF system has room to
       load another file, the error SPICE(SPKFILETABLEFULL) is
       signaled by a routine in the call tree of this routine. The
       current setting of FTSIZE does not allow this situation to
       arise: the DAF system will trap the error before this routine
       has the chance.

   3)  If the `fname' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   4)  If the `fname' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

-Files

   A file specified by `fname', to be loaded. The file is assigned a
   handle by spklef_c, which will be used by most other routines to
   refer to it.

-Particulars

   Loading an SPK file makes the file's data accessible to the CSPICE
   SPK readers spkezr_c and spkez_c.

   The maximum number of SPK files that may be loaded at any time is
   given by the parameter FTSIZE. After this limit it reached, it is
   necessary to unload an SPK file before another can be loaded. The
   function spkuef_c is provided to unload files from the SPK system.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Load a planetary ephemeris SPK; then look up a series of
      geometric states of the Earth relative to the solar system
      barycenter, referenced to the J2000 frame.

      Use the SPK kernel below to load the required ephemerides
      for the Earth and the Earth Barycenter relative to the solar
      system barycenter.

         de405.bsp


      Example code begins here.


      /.
         Program spklef_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {
         /.
         Local constants.
         ./
         #define        MAXITR        5
         #define        ET0           -315576000.0
         #define        STEP          3600.0

         #define        ABCORR        "NONE"
         #define        FRAME         "J2000"
         #define        OBSERVER      "SOLAR SYSTEM BARYCENTER"
         #define        SPK           "de405.bsp"
         #define        TARGET        "EARTH"

         /.
         Local variables.
         ./
         SpiceInt       handle;
         SpiceInt       i;

         SpiceDouble    et;
         SpiceDouble    lt;
         SpiceDouble    state [6];


         /.
         Load the SPK file.
         ./
         spklef_c ( SPK, &handle );

         /.
         Step through a series of epochs, looking up a state vector
         at each one.
         ./
         for ( i = 0;  i < MAXITR;  i++ )
         {
            et  =  ET0 + i*STEP;

            spkezr_c ( TARGET,    et,     FRAME,  ABCORR,
                       OBSERVER,  state,  &lt             );

            printf( "\net = %20.10f\n\n",                   et       );
            printf( "J2000 x-position (km):   %20.10f\n",   state[0] );
            printf( "J2000 y-position (km):   %20.10f\n",   state[1] );
            printf( "J2000 z-position (km):   %20.10f\n",   state[2] );
            printf( "J2000 x-velocity (km/s): %20.10f\n",   state[3] );
            printf( "J2000 y-velocity (km/s): %20.10f\n",   state[4] );
            printf( "J2000 z-velocity (km/s): %20.10f\n\n", state[5] );
         }

         /.
         Unload the SPK kernel. This isn't necessary in a stand-
         alone program, but it's good practice in subroutines
         because it frees program and system resources.
         ./
         spkuef_c( handle );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      et = -315576000.0000000000

      J2000 x-position (km):   -26772058.9514643848
      J2000 y-position (km):   132760135.1677220613
      J2000 z-position (km):    57557579.2735445350
      J2000 x-velocity (km/s):       -29.7772753957
      J2000 y-velocity (km/s):        -5.0656884328
      J2000 z-velocity (km/s):        -2.1979102802


      et = -315572400.0000000000

      J2000 x-position (km):   -26879249.7439419106
      J2000 y-position (km):   132741862.7243705541
      J2000 z-position (km):    57549651.2066062242
      J2000 x-velocity (km/s):       -29.7731620671
      J2000 y-velocity (km/s):        -5.0856683968
      J2000 z-velocity (km/s):        -2.2065710777


      et = -315568800.0000000000

      J2000 x-position (km):   -26986425.6981768459
      J2000 y-position (km):   132723518.3595090210
      J2000 z-position (km):    57541691.9637668282
      J2000 x-velocity (km/s):       -29.7690319295
      J2000 y-velocity (km/s):        -5.1056448242
      J2000 z-velocity (km/s):        -2.2152302239


      et = -315565200.0000000000

      J2000 x-position (km):   -27093586.7536762133
      J2000 y-position (km):   132705102.0859030634
      J2000 z-position (km):    57533701.5509854183
      J2000 x-velocity (km/s):       -29.7648849936
      J2000 y-velocity (km/s):        -5.1256176961
      J2000 z-velocity (km/s):        -2.2238877108


      et = -315561600.0000000000

      J2000 x-position (km):   -27200732.8499865979
      J2000 y-position (km):   132686613.9163857996
      J2000 z-position (km):    57525679.9742503539
      J2000 x-velocity (km/s):       -29.7607212708
      J2000 y-velocity (km/s):        -5.1455869940
      J2000 z-velocity (km/s):        -2.2325435301


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   B.V. Semenov        (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 2.1.0, 10-AUG-2021 (JDR)

       Changed the input argument name "filename" to "fname" for
       consistency with other routines.

       Edited the header to comply with NAIF standard. Created complete code
       example from existing code fragment, adding a call to "spkuef_c" for
       unloading the SPK kernel.

       Added -Parameters section providing information about FTSIZE.

       Moved SPK required reading from -Literature_References to
       -Required_Reading section. Added entries #3 and #4 to -Exceptions
       section.

   -CSPICE Version 2.0.3, 04-FEB-2008 (BVS)

       Removed duplicate header section '-Exceptions'.

   -CSPICE Version 2.0.2, 16-JAN-2008 (EDW)

       Corrected typos in header titles:

       Detailed Input to -Detailed_Input
       Detailed Output to -Detailed_Output

   -CSPICE Version 2.0.1, 10-NOV-2006 (EDW)

       Added -Keywords and -Parameters section headers.
       Reordered section headers.

   -CSPICE Version 2.0.0, 08-FEB-1998 (NJB)

       Input argument fname changed to type ConstSpiceChar *.
       References to C2F_CreateStr_Sig were removed; code was
       cleaned up accordingly. String checks are now done using
       the macro CHKFSTR.

   -CSPICE Version 1.0.0, 25-OCT-1997 (NJB) (EDW)

-Index_Entries

   load SPK ephemeris file

-&
*/

{  /* Begin spklef_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "spklef_c" );


   /*
   Check the input string fname to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "spklef_c", fname );


   /*
   Call the f2c'd Fortran routine.
   */
   spklef_ ( ( char     * )  fname,
             ( integer  * )  handle,
             ( ftnlen     )  strlen(fname) );


   chkout_c ( "spklef_c" );

} /* end spklef_c */
