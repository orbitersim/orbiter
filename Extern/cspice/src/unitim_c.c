/*

-Procedure unitim_c ( Uniform time scale transformation )

-Abstract

   Transform time from one uniform scale to another. The uniform
   time scales are TAI, GPS, TT, TDT, TDB, ET, JED, JDTDB, JDTDT.

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

   TIME

-Keywords

   CONVERSION
   TIME
   UTILITY

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"

   SpiceDouble unitim_c ( SpiceDouble        epoch,
                          ConstSpiceChar   * insys,
                          ConstSpiceChar   * outsys )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   epoch      I   An epoch to be converted.
   insys      I   The time scale associated with the input epoch.
   outsys     I   The time scale associated with the function value.

   The function returns the d.p. in outsys that is equivalent to the
   epoch on the insys time scale.

-Detailed_Input

   epoch       is an epoch relative to the `insys' time scale.

   insys       is a time scale. Acceptable values are:

                  "TAI"     International Atomic Time.
                  "TDB"     Barycentric Dynamical Time.
                  "TDT"     Terrestrial Dynamical Time.
                  "TT"      Terrestrial Time, identical to TDT.
                  "ET"      Ephemeris time (in the SPICE system, this is
                            equivalent to TDB).
                  "JDTDB"   Julian Date relative to TDB.
                  "JDTDT"   Julian Date relative to TDT.
                  "JED"     Julian Ephemeris date (in the SPICE system
                            this is equivalent to JDTDB).
                  "GPS"     Global Positioning System Time.

               The routine is not sensitive to the case of the
               characters in `insys'; "tai" "Tai" and "TAI" are all
               equivalent from the point of view of this routine.

   outsys      is the time scale to which `epoch' should be converted.
               Acceptable values are the same as for `insys'. The
               routine is not sensitive to the case of `outsys'.

-Detailed_Output

   The function returns the time in the system specified by `outsys'
   that is equivalent to the `epoch' in the `insys' time scale.

-Parameters

   None.

-Exceptions

   1)  The kernel pool must contain the variables:

          'DELTET/DELTA_T_A'
          'DELTET/K'
          'DELTET/EB'
          'DELTET/M'

       If these are not present, the error SPICE(MISSINGTIMEINFO) is
       signaled by a routine in the call tree of this routine. (These
       variables are typically inserted into the kernel pool by
       loading a leapseconds kernel with the SPICE routine furnsh_c.)

   2)  If the names of either the input or output time types are
       unrecognized, the error SPICE(BADTIMETYPE) is signaled by a
       routine in the call tree of this routine.

   3)  If any of the `insys' or `outsys' input string pointers is
       null, the error SPICE(NULLPOINTER) is signaled. The function
       returns the value 0.0.

   4)  If any of the `insys' or `outsys' input strings has zero
       length, the error SPICE(EMPTYSTRING) is signaled. The function
       returns the value 0.0.

-Files

   None.

-Particulars

   We use the term uniform time scale to refer to those
   representations of time that are numeric (each epoch is
   represented by a number) and additive. A numeric time system is
   additive if given the representations, `e1' and `e2', of any pair of
   successive epochs, the time elapsed between the epochs is given by
   e2 - e1.

   Given an epoch in one of the uniform time scales specified by
   `insys', the function returns the equivalent representation in the
   scale specified by `outsys'. A list of the recognized uniform time
   scales is given in the detailed input for `insys'.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Convert a calendar date in UTC string format to Julian Ephemeris
      date.

      Use the LSK kernel below to load the leap seconds and time
      constants required for the conversions.

         naif0012.tls


      Example code begins here.


      /.
         Program unitim_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main( )
      {
         /.
         Local constants.
         ./
         #define        UTCSTR        "Dec 19 2003  16:48:00"

         /.
         Local variables.
         ./
         SpiceDouble             et;
         SpiceDouble             jed;

         /.
         Load the LSK file.
         ./
         furnsh_c ( "naif0012.tls" );

         /.
         Convert input UTC string to Ephemeris Time.
         ./
         str2et_c ( UTCSTR, &et );
         printf ( "UTC time             : %s\n", UTCSTR  );
         printf ( "Ephemeris time       : %21.9f\n", et  );

         /.
         Convert the Ephemeris Time to Julian ephemeris date, i.e.
         Julian date relative to TDB time scale.
         ./
         jed = unitim_c ( et, "ET", "JED" );
         printf ( "Julian Ephemeris Date: %21.9f\n", jed );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      UTC time             : Dec 19 2003  16:48:00
      Ephemeris time       :   125124544.183560610
      Julian Ephemeris Date:     2452993.200742865


-Restrictions

   1)  The appropriate variable must be loaded into the SPICE kernel
       pool (normally by loading a leapseconds kernel with furnsh_c)
       prior to calling this routine.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   H.A. Neilan         (JPL)
   W.L. Taber          (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.2.0, 05-AUG-2021 (EDW) (JDR)

       Added time system name "TT" (Terrestrial Time) as alternate
       assignment of "TDT" (Terrestrial Dynamical Time).

       Included GPS time system mapping.

       Edited the header to comply with NAIF standard. Added complete
       code example.

   -CSPICE Version 1.1.1, 14-AUG-2006 (EDW)

       Replace mention of ldpool_c with furnsh_c.

   -CSPICE Version 1.1.0, 08-FEB-1998 (NJB)

       Re-implemented routine without dynamically allocated, temporary
       strings.

   -CSPICE Version 1.0.0, 25-OCT-1997 (EDW) (HAN) (WLT)

-Index_Entries

   Transform between two uniform numeric time systems
   Transform between two additive numeric time systems
   Convert one uniform numeric time system to another
   Convert one additive numeric time system to another

-&
*/

{ /* Begin unitim_c */

   /*
   Local variables
   */
   SpiceDouble             result;


   /*
   Participate in error tracing.
   */
   chkin_c ( "unitim_c");


   /*
   Check the input string insys to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR_VAL ( CHK_STANDARD, "unitim_c", insys, 0. );

   /*
   Check the input string outsys to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR_VAL ( CHK_STANDARD, "unitim_c", outsys, 0. );

   /*
   Call the f2c'd routine.
   */
   result = (SpiceDouble) unitim_( ( doublereal * ) &epoch,
                                   ( char       * ) insys,
                                   ( char       * ) outsys,
                                   ( ftnlen       ) strlen(insys),
                                   ( ftnlen       ) strlen(outsys) );

   chkout_c ( "unitim_c");

   return ( result );


} /* End unitim_c */
