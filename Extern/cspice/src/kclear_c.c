/*

-Procedure kclear_c ( Keeper clear )

-Abstract

   Clear the KEEPER subsystem: unload all kernels, clear the kernel
   pool, and re-initialize the subsystem. Existing watches on kernel
   variables are retained.

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

   KERNEL

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"

   void kclear_c ( void )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   None.

-Detailed_Input

   None. This routine operates by side effects. See -Particulars
   below.

-Detailed_Output

   None.

-Parameters

   None.

-Exceptions

   1)  If an error occurs when setting a kernel pool watch or
       checking watched variables, the error is signaled by a routine
       in the call tree of this routine.

-Files

   See -Particulars.

-Particulars

   This entry point allows you re-initialize the KEEPER system with
   a single call. The KEEPER system is the kernel management system
   underlying the set of CSPICE APIs

      furnsh_c
      ktotal_c
      kdata_c
      kinfo_c
      kclear_c
      unload_c

   This routine unloads all kernels from their kernel-type-specific
   kernel management subsystems (SPKBSR, CKBSR, etc.), clears the
   kernel pool, clears KEEPER's internal file database, and re-sets
   the watch status for the kernel variables used to load kernels
   via meta-kernels. As a side effect of clearing the kernel pool,
   all watched variables are marked as updated. Note that clearing
   the kernel pool does not delete watchers.

   This capability, though implemented in Fortran, is particularly
   relevant to SPICE implementations such as Icy, for which the
   state of the KEEPER system persists after any Icy-based IDL
   script is run. Successive runs of Icy-based scripts may perform
   in unexpected ways when scripts access data loaded during runs of
   previous scripts.

   Cleaning up after such programs using explicit unload_c commands is
   tedious and error-prone. One call to this routine sets the
   KEEPER system to its initial state, preventing unintentional
   interaction between scripts via KEEPER's state.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Load a meta-kernel containing three kernels, and count the
      number of files in the kernel pool before and after calling
      cspice_kclear.

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File name: kclear_ex1.tm

         This meta-kernel is intended to support operation of SPICE
         example programs. The kernels shown here should not be
         assumed to contain adequate or correct versions of data
         required by SPICE-based user applications.

         In order for an application to use this meta-kernel, the
         kernels referenced here must be present in the user's
         current working directory.

         The names and contents of the kernels referenced
         by this meta-kernel are as follows:

            File name                     Contents
            ---------                     --------
            de421.bsp                     Planetary ephemeris
            pck00008.tpc                  Planet orientation and
                                          radii
            naif0009.tls                  Leapseconds


         \begindata

            KERNELS_TO_LOAD = ( 'de421.bsp',
                                'pck00008.tpc',
                                'naif0009.tls'  )

         \begintext

         End of meta-kernel


      Example code begins here.


      /.
         Program kclear_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      SpiceInt        count;

      int main()
      {
         /.
         Load the meta kernel.
         ./
         furnsh_c( "kclear_ex1.tm" );

         /.
         Count the number of loaded kernel files.
         ./
         ktotal_c ( "ALL", &count );

         printf ( "Count of loaded kernels before "
                  "kclear_c call: %d\n", count );

         /.
         Clear the KEEPER system, retrieve the number of loaded
         after the clear.
         ./
         kclear_c ( );

         ktotal_c ( "ALL", &count );

         printf ( "Count of loaded kernels after "
                  "kclear_c call : %d\n", count );

      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Count of loaded kernels before kclear_c call: 4
      Count of loaded kernels after kclear_c call : 0


-Restrictions

   1)  Calling this routine will wipe out any kernel pool data
       inserted via the CSPICE API routines to put data into the
       kernel pool (pcpool_c, pdpool_c and pipool_c).

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.2, 10-AUG-2021 (JDR)

       Edited -Examples section to comply with NAIF standard. Added
       complete example.

       Improved -Restrictions section.

   -CSPICE Version 1.0.1, 01-JUL-2014 (NJB)

       The header -Particulars section was updated to more
       completely describe the effect of this routine on
       kernel pool watchers. Header section order was corrected.

   -CSPICE Version 1.0.0, 15-NOV-2006 (NJB)

-Index_Entries

   Re-initialize the keeper system
   Clear the keeper system
   Unload all kernels

-&
*/

{ /* Begin kclear_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "kclear_c" );


   kclear_();


   chkout_c ( "kclear_c" );

} /* End kclear_c */
