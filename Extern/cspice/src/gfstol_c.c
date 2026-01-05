/*

-Procedure gfstol_c ( GF, set a tolerance value for GF )

-Abstract

   Override the default GF convergence value used in the high
   level GF routines.

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

   GF

-Keywords

   GEOMETRY

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"

   void gfstol_c ( SpiceDouble value )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   value      I   Double precision value returned or to store.

-Detailed_Input

   value       is the scalar double precision value to use as the GF subsystem
               convergence tolerance. This value will override the default
               tolerance, SPICE_GF_CNVTOL, defined in SpiceGF.h Units are
               TDB seconds.

-Detailed_Output

   None.

-Parameters

   None.

-Exceptions

   1)  If `value' is not strictly greater-than-zero, the error
       SPICE(INVALIDTOLERANCE) is signaled by a routine in the call
       tree of this routine.

-Files

   None.

-Particulars

   The high level GF routines (see gf.req for a listing) use a
   default value for the convergence tolerance, SPICE_GF_CNVTOL,
   defined in SpiceGF.h. It may occur that a GF search run needs a
   different convergence tolerance. gfstol_c programmatically changes
   the tolerance used by those routines.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) In 14 A.D., the Roman princeps Tiberius sent his son Drusus to subdue
      a revolt of a Roman Legion stationed in Pannonia. A Lunar eclipse
      occurred during this mission.

      Perform a search for occultation events of the sun by earth as
      observed from the Moon center. Search during the interval from
      14 A.D. SEP 1 to 14 A.D. SEP 30 (Julian).

      Use the meta-kernel shown below to load the required SPICE
      kernels.


         KPL/MK

         File name: gfstol_ex1.tm

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
            de408.bsp                     Planetary ephemeris covering
                                          year 14 AD
            pck00009.tpc                  Planet orientation and
                                          radii
            naif0009.tls                  Leapseconds

         \begindata

            KERNELS_TO_LOAD = ( 'de408.bsp',
                                'pck00009.tpc',
                                'naif0009.tls'  )

         \begintext

         End of meta-kernel


      Example code begins here.


      /.
         Program gfstol_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
         {
         /.
         Local constants
         ./

         #define TIMFMT          "YYYY ERA MON DD HR:MN:SC.#### ::JCAL"
         #define MAXWIN          200
         #define TIMLEN          41

         /.
         Local variables
         ./
         SPICEDOUBLE_CELL      ( cnfine, MAXWIN );
         SPICEDOUBLE_CELL      ( result, MAXWIN );

         SpiceChar             * win0;
         SpiceChar             * win1;
         SpiceChar               begstr [ TIMLEN ];
         SpiceChar               endstr [ TIMLEN ];

         SpiceDouble             et0;
         SpiceDouble             et1;
         SpiceDouble             left;
         SpiceDouble             right;
         SpiceDouble             step;

         SpiceInt                i;

         /.
         Load kernels.
         ./
         furnsh_c ( "gfstol_ex1.tm" );

         /.
         Obtain the TDB time bounds of the confinement
         window, which is a single interval in this case.
         ./
         win0 = "14 A.D. SEP 1  00:00:00";
         win1 = "14 A.D. SEP 30 00:00:00";

         str2et_c ( win0, &et0 );
         str2et_c ( win1, &et1 );

         /.
         Insert the time bounds into the confinement
         window.
         ./
         wninsd_c ( et0, et1, &cnfine );

         /.
         Select a 3-minute step. We'll ignore any occultations
         lasting less than 3 minutes.
         ./
         step = 180.0;

         /.
         Perform the search. 'et[0]' and 'et[1]' have values ~-6*10^10,
         SPICE_GF_CNVTOL has value 10^-6, so double precision addition or
         subtraction of 'et[0]' and 'et[1]' with SPICE_GF_CNVTOL returns
         a result indistinguishable from 'et[0]' and 'et[1]'.

         Reduce the GF convergence tolerance by an order of magnitude
         to resolve this condition.
         ./
         gfstol_c( 1.e-5 );

         gfoclt_c ( "any",
                    "earth",   "ellipsoid",  "iau_earth",
                    "sun",     "ellipsoid",  "iau_sun",
                    "lt",      "moon",      step,
                    &cnfine,   &result                 );

         if ( wncard_c(&result) == 0 )
            {
            printf ( "No occultation was found.\n" );
            }
         else
            {
            for ( i = 0;  i < wncard_c(&result); i++ )
               {
               /.
               Fetch and display each occultation interval.
               ./
               wnfetd_c ( &result, i, &left, &right );

               timout_c ( left,  TIMFMT, TIMLEN, begstr );
               timout_c ( right, TIMFMT, TIMLEN, endstr );

               printf ( "Interval %d\n"
                        "   Start time: %s\n"
                        "   Stop time:  %s\n",
                        (int)i, begstr, endstr      );
               }
            }

         return ( 0 );
         }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Interval 0
         Start time:   14 A.D. SEP 27 05:02:02.8250
         Stop time:    14 A.D. SEP 27 09:33:31.6995


-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.0.2, 06-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.

       Fixed short error message in -Exceptions section.

   -CSPICE Version 1.0.1, 28-JUN-2016 (EDW)

       Edit to header, correct Required Reading entry eliminating ".REQ"
       suffix.

       Edit to Example code, SpiceInts output as ints using
       explicit casting.

   -CSPICE Version 1.0.0, 27-SEP-2010 (EDW)

-Index_Entries

   change default convergence tolerance for GF routines

-&
*/

   { /* Begin gfstol_c */

   /*
   Participate in error tracing.
   */

   if ( return_c() )
      {
      return;
      }

   chkin_c ( "gfstol_c" );

   /*
   Let the f2c'd routine do the work.
   */

   gfstol_ (  (doublereal * ) &value );

   chkout_c ( "gfstol_c" );

   } /* End gfstol_c */
