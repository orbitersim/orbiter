/*

-Procedure spkuds_c ( SPK - unpack segment descriptor )

-Abstract

   Unpack the contents of an SPK segment descriptor.

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

   SPK

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZim.h"
   #undef    spkuds_c


   void spkuds_c ( ConstSpiceDouble     descr [5],
                   SpiceInt           * body,
                   SpiceInt           * center,
                   SpiceInt           * frame,
                   SpiceInt           * type,
                   SpiceDouble        * first,
                   SpiceDouble        * last,
                   SpiceInt           * baddrs,
                   SpiceInt           * eaddrs     )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   descr      I   An SPK segment descriptor.
   body       O   The NAIF ID code for the body of the segment.
   center     O   The center of motion for `body'.
   frame      O   The code for the frame of this segment.
   type       O   The type of SPK segment.
   first      O   The first epoch for which the segment is valid.
   last       O   The last  epoch for which the segment is valid.
   baddrs     O   Beginning DAF address of the segment.
   eaddrs     O   Ending DAF address of the segment.

-Detailed_Input

   descr       is an SPK segment descriptor.

-Detailed_Output

   body        is the NAIF ID code for the body of the segment.

   center      is the center of motion for `body'.

   frame       is the SPICE integer code for the frame to which states
               for the body are be referenced.

   type        is the type of SPK segment.

   first       is the first epoch for which the segment has
               ephemeris data.

   last        is the last epoch for which the segment has
               ephemeris data.

   baddrs      is the starting address of the data associated
               with this descriptor.

   eaddrs      is the last address of the data associated with
               this descriptor.

-Parameters

   None.

-Exceptions

   Error free.

   1)  If the input descriptor `descr' is invalid, it's possible for
       the output times to contain bit patterns that don't represent
       normal double precision values. This error is not diagnosed.

-Files

   None.

-Particulars

   This routine extracts the contents of an SPK segment
   descriptor into the components needed for reading and
   evaluating the data stored in the segment. It serves
   as a macro for expanding the SPK segment descriptor.

-Examples

   Suppose you wished to summarize a particular SPK segment
   and that you have the descriptor for that segment in hand.
   The following code fragment shows how you might use this
   routine to create a summary message concerning the segment.

       #include <stdio.h>
       #include "SpiceUsr.h"

       #define   TIMLEN       35
             .
             .
             .

       spkuds_c ( descr,  &body, &center, &frame,
                  &type,  &first, &last,  &baddr, &eaddr );

       /.
       Convert the start and stop times to TDB calendar strings.
       ./
       etcal_c ( first, TIMLEN, fstcal );
       etcal_c ( last,  TIMLEN, lstcal );

       printf ( "\n"
                "Body     : %d\n"
                "Center   : %d\n"
                "Frame ID : %d\n"
                "Data Type: %d\n"
                "\n"
                "Segment Start :  %s\n"
                "Segment Stop  :  %s\n",
                body,
                center,
                frame,
                type,
                fstcal,
                lstcal                   );

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   K.R. Gehringer      (JPL)
   W.L. Taber          (JPL)

-Version

   -CSPICE Version 1.2.0, 17-JUN-2021 (JDR)

       Changed the output argument names "begin" and "end" to "baddrs" and
       "eaddrs" for consistency with other routines.

       Edited the header to comply with NAIF standard. Added -Exceptions
       section.

   -CSPICE Version 1.1.0, 24-JUL-2001 (NJB)

       Changed prototype: input descr is now type (ConstSpiceDouble *).
       Implemented interface macro for casting input descr to const.

   -CSPICE Version 1.0.0, 22-JUL-1999 (NJB) (WLT) (KRG)

-Index_Entries

   Unpack and SPK segment descriptor

-&
*/

{ /* Begin spkuds_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "spkuds_c" );


   spkuds_ (  ( doublereal  * ) descr,
              ( integer     * ) body,
              ( integer     * ) center,
              ( integer     * ) frame,
              ( integer     * ) type,
              ( doublereal  * ) first,
              ( doublereal  * ) last,
              ( integer     * ) baddrs,
              ( integer     * ) eaddrs    );


   chkout_c ( "spkuds_c" );

} /* End spkuds_c */
