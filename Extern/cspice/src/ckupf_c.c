/*

-Procedure ckupf_c ( CK, Unload pointing file )

-Abstract

   Unload a CK pointing file so that it will no longer be searched
   by the readers.

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

   CK
   DAF

-Keywords

   POINTING

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"

   void ckupf_c ( SpiceInt handle )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     I   Handle of CK file to be unloaded

-Detailed_Input

   handle      is the integer handle assigned to the CK file upon
               loading.

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

   Unloading a file with ckupf_c removes that file from consideration
   by the CK readers. In doing so, it frees up space for another
   file to be loaded.

-Examples

   The numerical results shown for these examples may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Unload a CK kernel specified by an integer handle, making
      room to load another CK.

         ckupf_c ( handle );

   2) Load a CASSINI CK file and obtain the position transformation
      matrix from J2000 to the spacecraft reference frame provided
      by the CK and its angular velocity vector at a given spacecraft
      clock time.


      Use the CK kernel below to load the CASSINI image navigated
      spacecraft pointing and orientation data.

         04153_04182ca_ISS.bc


      In order to convert from spacecraft clock time to "ticks,"
      (units of encoded SCLK) as required by ckgpav_c, we will need
      to load as well a CASSINI SCLK.

      Use the SCLK kernel below to load the CASSINI spacecraft clock
      time correlation data required for the conversion between
      spacecraft clock string representation and double precision
      encoding of spacecraft clock counts.

         cas00071.tsc


      Example code begins here.


      /.
         Program ckupf_ex2
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main ()
      {
         /.
         Constants for this program:

         -- The code for the CASSINI spacecraft clock is -82.

         -- The code for CASSINI spacecraft reference frame is -82000.

         -- Tolerance: 1 second. It must be converted to "ticks"
            (units of encoded SCLK) for input to ckgpav_c.

         -- The reference frame we want is J2000.
         ./
         #define   SC        -82
         #define   INST      -82000
         #define   REF       "J2000"
         #define   TOL       "1.0"
         #define   MAXCLK    30

         #define   CK        "04153_04182ca_ISS.bc"
         #define   SCLK      "cas00071.tsc"

         /.
         Local variables.
         ./
         SpiceBoolean        found;

         SpiceChar         * sclkch  = "1465644281.0";

         SpiceChar           clkch   [MAXCLK];

         SpiceDouble         av      [3];
         SpiceDouble         cmat    [3][3];
         SpiceDouble         clkout;

         SpiceDouble         sclkdp;
         SpiceDouble         toltik;

         SpiceInt            i;
         SpiceInt            handle;

         /.
         Load the CK for read access. This call may be replaced (as
         recommended by NAIF) by furnsh_c.
         ./
         cklpf_c ( CK, &handle );

         /.
         We need to load a CASSINI SCLK kernel to convert from
         clock string to ticks.  Although not required for
         the CASSINI spacecraft clock, most modern spacecraft
         clocks require a leapseconds kernel to be loaded in
         addition to an SCLK kernel.
         ./
         furnsh_c ( SCLK );

         /.
         Convert tolerance from CASSINI formatted character string
         SCLK to ticks, which are units of encoded SCLK.
         ./
         sctiks_c ( SC, TOL, &toltik );

         /.
         ckgpav_c requires encoded spacecraft clock time.
         ./
         scencd_c ( SC, sclkch, &sclkdp );

         ckgpav_c ( INST,  sclkdp,  toltik,   REF,
                    cmat,  av,      &clkout,  &found );

         /.
         Display the results.
         ./
         if ( found )
         {
            scdecd_c ( SC, clkout, MAXCLK, clkch  );

            printf ( "Requested SCLK time         : %s\n", sclkch );
            printf ( "   CASSINI SCLK time        : %s\n", clkch  );
            printf ( "   J2000 to S/C frame matrix:\n\n" );
            for ( i = 0; i < 3; i++ )
            {
               printf("%20.10f %19.10f %19.10f\n",
                      cmat[i][0], cmat[i][1], cmat[i][2] );
            }
            printf ( "\n" );
            printf ( "   Angular velocity vector  : "
                     "%10.7f %10.7f %10.7f\n", av[0], av[1], av[2] );
         }
         else
         {
               printf ( "Pointing not found for time %s\n", sclkch );
         }

         /.
         Close the CK file. This call may be replaced (as
         recommended by NAIF) by unload_c, if furnsh_c has
         been used to load the file.
         ./
         ckupf_c ( handle );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Requested SCLK time         : 1465644281.0
         CASSINI SCLK time        : 1/1465644281.171
         J2000 to S/C frame matrix:

             -0.3353514559        0.8643744402        0.3746948467
             -0.9378874268       -0.3438519652       -0.0461844200
              0.0889189272       -0.3669095980        0.9259971767

         Angular velocity vector  :  0.0000000  0.0000000  0.0000000


   3) The following example extracts the first 20 lines of the
      comment area of a CK, displaying the comments on the terminal
      screen.


      Example code begins here.


      /.
         Program ckupf_ex3
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {

         /.
         Local parameters.
         ./
         #define FILSIZ          256
         #define LINLEN          1001
         #define BUFFSZ          20

         /.
         Local variables.
         ./
         SpiceBoolean            done;

         SpiceChar               ckname [FILSIZ];
         SpiceChar               buffer [BUFFSZ][LINLEN];

         SpiceInt                handle;
         SpiceInt                i;
         SpiceInt                n;


         prompt_c ( "Enter name of CK > ", FILSIZ, ckname );

         /.
         Open the CK for read access. This operation could have
         been done with dafopr_c.
         ./
         cklpf_c ( ckname, &handle );

         /.
         Extract up to 20 lines from the comment area of the
         loaded CK file and display them on the terminal screen.
         ./
         dafec_c ( handle, BUFFSZ, LINLEN, &n, buffer, &done );

         for ( i = 0;  i < n;  i++ )
         {
               printf ( "%s\n", buffer[i] );
         }

         /.
         Close the CK file. This operation could have been done
         with dafcls_c.
         ./
         ckupf_c ( handle );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, using the Cassini CK file named 04161_04164ra.bc as
      input CK file, the output was:


      Enter name of CK > 04161_04164ra.bc
      \beginlabel
      PDS_VERSION_ID               = PDS3
      RECORD_TYPE                  = FIXED_LENGTH
      RECORD_BYTES                 = 1024
      ^SPICE_KERNEL                = "04161_04164ra.bc"
      MISSION_NAME                 = "CASSINI-HUYGENS"
      SPACECRAFT_NAME              = "CASSINI ORBITER"
      DATA_SET_ID                  = "CO-S/J/E/V-SPICE-6-V1.0"
      KERNEL_TYPE_ID               = CK
      PRODUCT_ID                   = "04161_04164ra.bc"
      PRODUCT_CREATION_TIME        = 2005-06-29T21:28:09
      PRODUCER_ID                  = "CASSINI_AACS/JPL"
      MISSION_PHASE_NAME           = "SCIENCE CRUISE"
      PRODUCT_VERSION_TYPE         = ACTUAL
      PLATFORM_OR_MOUNTING_NAME    = "N/A"
      START_TIME                   = 2004-06-09T12:00:03.631
      STOP_TIME                    = 2004-06-12T11:58:57.943
      SPACECRAFT_CLOCK_START_COUNT = "1/1465475046.160"
      SPACECRAFT_CLOCK_STOP_COUNT  = "1/1465734182.160"
      TARGET_NAME                  = "N/A"


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)
   J.M. Lynch          (JPL)
   B.V. Semenov        (JPL)
   R.E. Thurman        (JPL)
   I.M. Underwood      (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.0.3, 06-JUL-2021 (JDR)

       Updated the header to comply with NAIF standard. Added complete
       code examples.

   -CSPICE Version 1.0.2, 31-JAN-2008 (BVS)

       Removed '-Revisions' from the header.

   -CSPICE Version 1.0.1, 03-JUN-2003 (EDW)

       Correct typo in Procedure line.

   -CSPICE Version 1.0.0, 08-FEB-1998 (EDW) (JML) (RET) (IMU)

-Index_Entries

   unload CK pointing file

-&
*/

{ /* Begin ckupf_c */

   /*
   Participate in error handling
   */

   chkin_c ( "ckupf_c");


   /*
   Call the f2c'd Fortran routine.
   */
   ckupf_ ( &handle );


   chkout_c ( "ckupf_c");

} /* End ckupf_c */
