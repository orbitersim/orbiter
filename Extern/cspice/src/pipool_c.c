/*

-Procedure pipool_c ( Put integers into the kernel pool )

-Abstract

   Provide toolkit programmers a method for programmatically
   inserting integer data into the kernel pool.

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

   POOL

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"
   #include "SpiceZim.h"
   #undef    pipool_c


   void pipool_c ( ConstSpiceChar  * name,
                   SpiceInt          n,
                   ConstSpiceInt   * ivals )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   name       I   The kernel pool name to associate with values.
   n          I   The number of values to insert.
   ivals      I   An array of integers to insert into the pool.

-Detailed_Input

   name        is the name of the kernel pool variable to associate
               with the values supplied in the array ivals. 'name' is
               restricted to a length of 32 characters or less.

   n           is the number of values to insert into the kernel pool.

   ivals       is an array of integers to insert into the kernel pool.

-Detailed_Output

   None.

-Parameters

   None.

-Exceptions

   1)  If `name' is already present in the kernel pool and there
       is sufficient room to hold all values supplied in `ivals',
       the old values associated with `name' will be overwritten.

   2)  If there is not sufficient room to insert a new variable into
       the kernel pool and `name' is not already present in the kernel
       pool, an error is signaled by a routine in the call tree of
       this routine.

   3)  If there is not sufficient room to insert the values
       associated with `name', the error SPICE(NOMOREROOM) is signaled
       by a routine in the call tree of this routine.

   4)  If the kernel pool variable name length exceeds its maximum
       allowed length (see Kernel Required Reading, kernel.req), the
       error SPICE(BADVARNAME) is signaled by a routine in the call
       tree of this routine.

   5)  If the `name' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   6)  If the `name' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

-Files

   None.

-Particulars

   This entry point provides a programmatic interface for inserting
   data into the SPICE kernel pool without reading an external file.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as input,
   the compiler and supporting libraries, and the machine specific
   arithmetic implementation.

   1) The following example code shows how a topocentric frame for a
      point on the surface of the earth may be defined at run time using
      pcpool_c, pdpool_c, and pipool_c. In this example, the surface
      point is associated with the body code 300000. To facilitate
      testing, the location of the surface point coincides with that of
      the DSN station DSS-12; the reference frame MYTOPO defined here
      coincides with the reference frame DSS-12_TOPO.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File name: pipool_ex1.tm

         This meta-kernel is intended to support operation of SPICE
         example programs. The kernels shown here should not be
         assumed to contain adequate or correct versions of data
         required by SPICE-based user applications.

         In order for an application to use this meta-kernel, the
         kernels referenced here must be present in the user's
         current working directory.

         The names and contents of the kernels referenced
         by this meta-kernel are as follows:

            File name                        Contents
            ---------                        --------
            earth_720101_070426.bpc          Earth historical
                                             binary PCK
            earth_topo_050714.tf             DSN station FK

         \begindata

         KERNELS_TO_LOAD = ( 'earth_720101_070426.bpc',
                             'earth_topo_050714.tf'    )

         \begintext

         End of meta-kernel.


      Example code begins here.


      /.
         Program pipool_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {
         /.
         The first angle is the negative of the longitude of the
         surface point; the second angle is the negative of the
         point's colatitude.
         ./
         SpiceDouble             angles [3]      =  { -243.1945102442646,
                                                       -54.7000629043147,
                                                       180.0              };

         SpiceDouble             et              =    0.0;
         SpiceDouble             rmat   [3][3];

         SpiceInt                axes   [3]      =  { 3, 2, 3 };
         SpiceInt                center          =    300000;
         SpiceInt                frclass         =    4;
         SpiceInt                frclsid         =    1500000;
         SpiceInt                frcode          =    1500000;

         /.
         Define the MYTOPO reference frame.

         Note that the third argument in the pcpool_c calls is
         the length of the final string argument, including the
         terminating null character.
         ./
         pipool_c ( "FRAME_MYTOPO",            1,     &frcode   );
         pcpool_c ( "FRAME_1500000_NAME",      1, 7,  "MYTOPO"  );
         pipool_c ( "FRAME_1500000_CLASS",     1,     &frclass  );
         pipool_c ( "FRAME_1500000_CLASS_ID",  1,     &frclsid  );
         pipool_c ( "FRAME_1500000_CENTER",    1,     &center   );

         pcpool_c ( "OBJECT_300000_FRAME",     1, 7,  "MYTOPO"  );

         pcpool_c ( "TKFRAME_MYTOPO_RELATIVE", 1, 7,  "ITRF93"  );
         pcpool_c ( "TKFRAME_MYTOPO_SPEC",     1, 7,  "ANGLES"  );
         pcpool_c ( "TKFRAME_MYTOPO_UNITS",    1, 8,  "DEGREES" );
         pipool_c ( "TKFRAME_MYTOPO_AXES",     3,     axes      );
         pdpool_c ( "TKFRAME_MYTOPO_ANGLES",   3,     angles    );

         /.
         Load a high precision binary earth PCK. Also load a
         topocentric frame kernel for DSN stations. Use a meta-kernel
         for convenience.
         ./
         furnsh_c ( "pipool_ex1.tm" );

         /.
         Look up transformation from DSS-12_TOPO frame to MYTOPO frame.
         This transformation should differ by round-off error from
         the identity matrix.
         ./
         pxform_c ( "DSS-12_TOPO", "MYTOPO", et, rmat );

         printf   ( "\n"
                    "DSS-12_TOPO to MYTOPO transformation at "
                    "et %15.6f: \n"
                    "\n"
                    "   %19.16f  %19.16f  %19.16f\n"
                    "   %19.16f  %19.16f  %19.16f\n"
                    "   %19.16f  %19.16f  %19.16f\n",
                    et,
                    rmat[0][0],  rmat[0][1],  rmat[0][2],
                    rmat[1][0],  rmat[1][1],  rmat[1][2],
                    rmat[2][0],  rmat[2][1],  rmat[2][2]       );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      DSS-12_TOPO to MYTOPO transformation at et        0.000000:

          1.0000000000000000   0.0000000000000000   0.0000000000000001
          0.0000000000000000   1.0000000000000000  -0.0000000000000000
          0.0000000000000001  -0.0000000000000000   1.0000000000000000


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   W.L. Taber          (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.2.4, 27-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard. Added
       example's meta-kernel. Reformatted example's output.

   -CSPICE Version 1.2.3, 17-JAN-2014 (NJB)

       Updated -Index_Entries section.

   -CSPICE Version 1.2.2, 10-FEB-2010 (EDW)

       Added mention of the restriction on kernel pool variable
       names to 32 characters or less.

       Reordered header sections to conform to SPICE convention.

   -CSPICE Version 1.2.1, 17-NOV-2005 (NJB)

       Replaced code fragment in -Examples section of header with
       smaller, complete program.

   -CSPICE Version 1.2.0, 28-AUG-2001 (NJB)

       Const-qualified input array ivals.

   -CSPICE Version 1.0.0, 03-JUN-1999 (NJB) (WLT)

-Index_Entries

   Set the value of an integer_variable in the kernel_pool

-&
*/

{ /* Begin pipool_c */



   /*
   Participate in error tracing.
   */
   chkin_c ( "pipool_c" );


   /*
   Check the input kernel variable name to make sure the pointer is
   non-null and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "pipool_c", name );

   /*
   Call the f2c'd routine.
   */
   pipool_ ( ( char        * ) name,
             ( integer     * ) &n,
             ( integer     * ) ivals,
             ( ftnlen        ) strlen(name) );

   chkout_c ( "pipool_c" );

} /* End pipool_c */
