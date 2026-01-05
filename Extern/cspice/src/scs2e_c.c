/*

-Procedure scs2e_c ( SCLK string to ET )

-Abstract

   Convert a spacecraft clock string to ephemeris seconds past
   J2000 (ET).

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
   SCLK
   SPK
   TIME

-Keywords

   CONVERSION
   TIME

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"

   void scs2e_c ( SpiceInt          sc,
                  ConstSpiceChar  * sclkch,
                  SpiceDouble     * et      )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   sc         I   NAIF integer code for a spacecraft.
   sclkch     I   An SCLK string.
   et         O   Ephemeris time, seconds past J2000.

-Detailed_Input

   sc          is a NAIF ID code for a spacecraft, one of whose
               clock values is represented by `sclkch'. The set of
               supported spacecraft clocks is listed in the SCLK
               Required Reading.

   sclkch      is a character string representation of the
               spacecraft clock value that corresponds to `et', for
               the spacecraft specified by the input argument `sc'.
               `sclkch' is an absolute spacecraft clock time, so
               partition information should be included in this
               string. The precise format of `sclkch' is specified
               in the SCLK Required Reading.

-Detailed_Output

   et          is the epoch, specified as ephemeris seconds past
               J2000, that corresponds to `sclkch'.

-Parameters

   None.

-Exceptions

   1)  If an SCLK kernel has not been loaded, does not contain all of
       the required data, or contains invalid data, an error is
       signaled by a routine in the call tree of this routine. The
       output argument `et' will not be modified. This routine assumes
       that that an SCLK kernel appropriate to the spacecraft clock
       identified by the input argument `sc' has been loaded.

   2)  If a leapseconds kernel is required for conversion between
       SCLK and ET but is not loaded, an error is signaled by a
       routine in the call tree of this routine. The output argument
       `et' will not be modified. When using SCLK kernels that map SCLK
       to a time system other than ET (also called barycentric
       dynamical time---`TDB'), it is necessary to have a leapseconds
       kernel loaded at the time this routine is called.

       The time system that an SCLK kernel maps SCLK to is indicated
       by the variable SCLK_TIME_SYSTEM_nn in the kernel, where nn
       is the negative of the NAIF integer code for the spacecraft.
       The time system used in a kernel is TDB if and only if the
       variable is assigned the value 1.

   3)  If the value of `sclkch' is invalid, an error is signaled by a
       routine in the call tree of this routine. The output argument
       `et' will not be modified.

   4)  If the `sclkch' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   5)  If the `sclkch' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

-Files

   None.

-Particulars

   This routine is provided as a convenience; it is simply shorthand
   for the code fragment

      scencd_c ( sc, sclkch, &sclkdp );
      sct2e_c  ( sc, sclkdp, &et     );

   See the SCLK Required Reading for a list of the entire set of
   SCLK conversion routines.

-Examples

   1)  Find the state (position and velocity) of Jupiter, as seen
       from the Galileo spacecraft, at the epoch corresponding to
       the SCLK value

          2 / 3110578:89:09

       The digit "2", followed by the forward slash, indicates that
       the time value is in the second mission partition.


       During program initialization, load the leapseconds and
       SCLK kernels. We will pretend that these files are named
       "leapseconds.ker" and "gllsclk.ker".  To use this code
       fragment, you must substitute the actual names of these
       kernel files for the names used here.

          /.
          Load leapseconds and SCLK kernels:
          ./
          furnsh_c ( "leapseconds.ker" );
          furnsh_c ( "gllsclk.ker"     );

          /.
          Load an SPK file (again, a fictitious file)
          containing an ephemeris for Jupiter and the
          GLL orbiter's trajectory.
          ./
          spklef_c ( "gllspk.ker", &handle );

          /.
          The Galileo spacecraft ID is -77. Convert our SCLK
          string to ephemeris seconds past J2000, which is the
          time representation expected by spkez_c.
          ./
          scs2e_c ( -77, "2 / 3110578:89:09", &et );

          /.
          Find the state of Jupiter (body 599) as seen from Galileo
          at time et. To use spkez_c, you must first load an SPK
          kernel, using the routine spklef_c.
          ./
          spkez_c ( 599, et, refsys, corr, -77, state, &lt );



   2)  Convert a Voyager 2 SCLK value to UTC, using calendar format,
       with 3 digits of precision in the seconds component.

       Again, your initialization code must load the leapseconds
       and SCLK kernels:

          /.
          Load leapseconds and SCLK kernels:
          ./
          furnsh_c ( "leapseconds.ker" );
          furnsh_c ( "vgr2sclk.ker"    );


       To find the UTC value corresponding to Voyager 2 SCLK
       string

          11389.20.768

       you can use the code fragment

          scs2e_c  ( -32,  "11389.29.768",          &et  );
          et2utc_c (  et,  "c",      3,    UTCLEN,  utc  );

       where UTCLEN is a constant indicating the available
       room in the string utc. A value of 25 characters suffices.

-Restrictions

   1)  An SCLK kernel appropriate to the spacecraft clock identified
       by `sc' must be loaded at the time this routine is called.

   2)  If the SCLK kernel used with this routine does not map SCLK
       directly to barycentric dynamical time, a leapseconds kernel
       must be loaded at the time this routine is called.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.1.2, 05-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard. Moved CK and SPK
       required readings from -Literature_References to -Required_Reading
       section.

       Added entries #4 and #5 to -Exceptions section. Extended -Particulars.

   -CSPICE Version 1.1.1, 14-AUG-2006 (EDW)

       Replace mention of ldpool_c with furnsh_c.

   -CSPICE Version 1.1.0, 08-FEB-1998 (NJB)

        References to C2F_CreateStr_Sig were removed; code was
        cleaned up accordingly. String checks are now done using
        the macro CHKFSTR.

   -CSPICE Version 1.0.0, 25-OCT-1997 (NJB)

       Based on SPICELIB Version 1.0.2, 10-APR-1992 (NJB) (WLT)

-Index_Entries

   spacecraft_clock string to ephemeris time

-&
*/

{ /* Begin scs2e_c */


   /*
   Participate in error handling
   */
   chkin_c ( "scs2e_c");


   /*
   Check the input string to make sure the pointer
   is non-null and the string length is non-zero.
   */
   CHKFSTR ( CHK_STANDARD, "scs2e_c", sclkch );


   /*
   Carry out the conversion.
   */
   scs2e_ (  ( integer    * ) &sc,
             ( char       * ) sclkch,
             ( doublereal * ) et,
             ( ftnlen       ) strlen(sclkch)  );


   chkout_c ( "scs2e_c");

} /* End scs2e_c */
