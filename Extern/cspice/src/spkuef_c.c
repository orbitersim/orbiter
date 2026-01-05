/*

-Procedure spkuef_c ( SPK Kernel, Unload ephemeris file )

-Abstract

   Unload an ephemeris file so that it will no longer be searched by
   the readers.

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
   #include "SpiceZst.h"


   void spkuef_c ( SpiceInt handle )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     I   Handle of file to be unloaded

-Detailed_Input

   handle      is the integer handle assigned to the file upon loading.

-Detailed_Output

   None.

-Parameters

   None.

-Exceptions

   1)  Unloading a file that has not been loaded is a no-op.
       No error is signaled.

-Files

   The file referred to by `handle' is unloaded.

-Particulars

   A file is removed from consideration by the readers by a call to
   spkuef_c.

   The file table entry corresponding to the file referenced by
   `handle', is removed. Any segment table entry which came from the
   specified file is also deleted.

   If the file specified by `handle' is not currently loaded in the
   SPK system, no action is taken.

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
         Program spkuef_ex1
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

   J. Diaz del Rio     (ODC Space)
   J.M. Lynch          (JPL)
   R.E. Thurman        (JPL)
   I.M. Underwood      (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.0.2, 10-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard. Added complete
       code example.

       Removed the reference to the SPK required reading from the
       -Literature_References section.

   -CSPICE Version 1.0.1, 02-JUL-2003 (EDW)

       Corrected trivial typo in the Version 1.0.0 line.
       The typo caused an integrity check script to fail.

   -CSPICE Version 1.0.0, 19-OCT-1997 (EDW) (RET) (IMU) (JML)

-Index_Entries

   unload SPK ephemeris file

-&
*/

{ /* Begin spkuef_c */

   /*
   Participate in error handling
   */
   chkin_c( "spkuef_c");


   /*
   Call the f2c'd Fortran routine.
   */
   spkuef_ ( &handle );


   chkout_c( "spkuef_c");

} /* end spkuef_c */
