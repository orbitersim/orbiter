/*

-Procedure scpart_c ( Spacecraft Clock Partition Information )

-Abstract

   Get spacecraft clock partition information from a spacecraft
   clock kernel file.

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

   SCLK

-Keywords

   TIME

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"

   void scpart_c ( SpiceInt            sc,
                   SpiceInt          * nparts,
                   SpiceDouble         pstart [],
                   SpiceDouble         pstop  [] )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   sc         I   NAIF spacecraft identification code.
   nparts     O   The number of spacecraft clock partitions.
   pstart     O   Array of partition start times.
   pstop      O   Array of partition stop times.
   SPICE_SCLK_MXPART
              P   Maximum number of partitions.

-Detailed_Input

   sc          is the NAIF ID for the spacecraft whose clock partition
               information is being requested.

-Detailed_Output

   nparts      is the number of spacecraft clock time partitions
               described in the kernel file for spacecraft `sc'.

   pstart      is an array containing `nparts' partition start times
               represented as double precision, encoded SCLK
               ("ticks"). The values contained in `pstart' are whole
               numbers.

   pstop       is an array containing `nparts' partition end times
               represented as double precision, encoded SCLK
               ("ticks"). The values contained in `pstop' are whole
               numbers.

-Parameters

   See the include file

      SpiceSCLK.h

   for sizes and limits used by the SCLK system.

   SPICE_SCLK_MXPART

               is the maximum number of spacecraft clock partitions
               expected in the kernel file for any one spacecraft.
               See the header file SpiceSCLK.h for this parameter's
               value.

-Exceptions

   1)  If the kernel variables containing the spacecraft clock
       partition start and stop times have not been loaded in the
       kernel pool, an error is signaled by a routine in the call
       tree of this routine.

   2)  If the number of start and stop times are different, the error
       SPICE(NUMPARTSUNEQUAL) is signaled by a routine in the call
       tree of this routine.

-Files

   An SCLK kernel containing spacecraft clock partition start
   and stop times for the spacecraft clock indicated by `sc' must
   be loaded into the kernel pool.

-Particulars

   scpart_c looks for two variables in the kernel pool for each
   spacecraft's partition information. If sc = -nn, then the names of
   the variables are

      SCLK_PARTITION_START_nn
      SCLK_PARTITION_END_nn

   The start and stop times returned are in units of "ticks".

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as input,
   the compiler and supporting libraries, and the machine specific
   arithmetic implementation.

   1) The following code example finds partition start and stop
      times for the Stardust spacecraft from a spacecraft clock
      kernel file. Since those times are always returned in units
      of ticks, the program uses scfmt_c to print the times in
      Stardust clock format.

      Use the SCLK kernel below to load the Stardust time
      correlation data and spacecraft clock partition information.

         sdu_sclkscet_00074.tsc


      Example code begins here.


      /.
         Program scpart_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {
         /.
         Local parameters.
         ./
         #define                CLKLEN   31

         /.
         Local variables.
         ./
         SpiceChar              start  [ CLKLEN ];
         SpiceChar              stop   [ CLKLEN ];

         SpiceDouble            pstart [ SPICE_SCLK_MXPART ];
         SpiceDouble            pstop  [ SPICE_SCLK_MXPART ];

         SpiceInt               sc;
         SpiceInt               i;
         SpiceInt               nparts;

         /.
         Assign the value for the Stardust spacecraft ID.
         ./
         sc = -29;

         /.
         Load the SCLK file.
         ./
         furnsh_c ( "sdu_sclkscet_00074.tsc" );

         /.
         Retrieve the arrays for `pstart' and `pstop' and the
         number of partitions within the SCLK.
         ./
         scpart_c ( sc, &nparts, pstart, pstop );

         /.
         Loop over each array value.
         ./
         for ( i = 0;  i < nparts;  i++ )
         {
            scfmt_c ( sc, pstart[ i ], CLKLEN, start );
            scfmt_c ( sc, pstop [ i ], CLKLEN, stop  );

            printf ( "\n"
                     "Partition: %d\n"
                     "   Start : %s\n"
                     "   Stop  : %s\n",
                      (int)i,
                      start,
                      stop                );
         }

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Partition: 0
         Start : 0000000000.000
         Stop  : 0602741011.080

      Partition: 1
         Start : 0602741014.217
         Stop  : 0605660648.173

      Partition: 2
         Start : 0605660649.000
         Stop  : 0631375256.224

      Partition: 3
         Start : 0631375257.000
         Stop  : 0633545577.218

      Partition: 4
         Start : 0633545578.000
         Stop  : 0644853954.043

      Partition: 5
         Start : 0644853954.000
         Stop  : 0655316480.089

      Partition: 6
         Start : 0655316480.000
         Stop  : 0660405279.066

      Partition: 7
         Start : 0660405279.000
         Stop  : 0670256568.229

      Partition: 8
         Start : 0670256569.000
         Stop  : 0674564039.091

      Partition: 9
         Start : 0674564040.000
         Stop  : 4294537252.255


-Restrictions

   1)  This routine assumes that an SCLK kernel appropriate to the
       spacecraft identified by `sc' has been loaded into the kernel
       pool.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.1.2, 07-AUG-2021 (JDR)

       Edited to header to comply with NAIF standard. Added
       reference to required SCLK and created complete example from
       existing code fragment using Stardust PDS archived data.

       Removed unnecessary include file "SpiceZst.h".

   -CSPICE Version 1.1.1, 19-MAR-2014 (NJB)

       Minor header comment updates were made.

   -CSPICE Version 1.1.0, 11-FEB-2008 (NJB)

       Definition of constant macro MXPART was deleted.
       Documentation was updated to reflect current
       MXPART value of 9999.

   -CSPICE Version 1.0.1, 14-AUG-2006 (EDW)

       Replace mention of ldpool_c with furnsh_c.

   -CSPICE Version 1.0.0, 08-FEB-1998 (NJB)

       Based on SPICELIB Version 1.1.0, 22-MAR-1993 (JML)

-Index_Entries

   spacecraft_clock partition information

-&
*/

{ /* Begin scpart_c */

   /*
   Participate in error tracing.
   */
   chkin_c ( "scpart_c" );

   /*
   Unlike most of the wrappers, this one reads the
   partition data directly into the callers' buffers.

   We rely on the scpart_ to check for an excessive
   partition count.
   */
   scpart_ (  ( integer    * ) &sc,
              ( integer    * )  nparts,
              ( doublereal * )  pstart,
              ( doublereal * )  pstop  );

   chkout_c ( "scpart_c" );

} /* End scpart_c */
