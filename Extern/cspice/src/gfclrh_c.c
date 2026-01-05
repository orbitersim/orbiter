/*

-Procedure gfclrh_c ( GF, clear interrupt signal handler status )

-Abstract

   Clear the interrupt signal handler status, so that future calls
   to gfbail_c will indicate no interrupt was received.

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
   SEARCH
   UTILITY

*/

   #include <signal.h>
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"


   void gfclrh_c ( void )

/*

-Brief_I/O

   None. This routine operates by side effects; see -Particulars
   below.

-Detailed_Input

   None.

-Detailed_Output

   None.

-Parameters

   None.

-Exceptions

   Error free.

-Files

   None.

-Particulars

   This routine clears the interrupt signal handler status maintained
   by the GF subsystem. Calling this routine causes the GF interrupt
   signal receipt indicator function gfbail_c to return SPICEFALSE
   until the next interrupt signal is received by a signal handler
   established by the GF system.

   User applications that use default GF interrupt handling should call
   this routine after processing an interrupt signal.

-Examples

   1) The code fragment below is from an example program in
      the header of gfocce_c. The code includes a search,
      a call to gfbail_c, which is made to determine whether
      an interrupt signal was received, and a call to
      gfclrh_c to clear the interrupt signal status.

            ...

            /.
            Turn on interrupt handling and progress reporting.
            ./
            bail = SPICETRUE;
            rpt  = SPICETRUE;

            /.
            Perform the search.
            ./
            gfocce_c ( "ANY",
                       "MOON",     "ellipsoid",  "IAU_MOON",
                       "SUN",      "ellipsoid",  "IAU_SUN",
                       "LT",       "EARTH",      CNVTOL,
                       gfstep_c,   gfrefn_c,     rpt,
                       gfrepi_c,   gfrepu_c,     gfrepf_c,
                       bail,       gfbail_c,     &cnfine,
                       &result                              );


            if ( gfbail_c() )
            {
               /.
               Clear the CSPICE interrupt indication. This is
               an essential step for programs that continue
               running after an interrupt; gfbail_c will
               continue to return SPICETRUE until this step
               has been performed.
               ./
               gfclrh_c();


               /.
               We've trapped an interrupt signal. In a realistic
               application, the program would continue operation
               from this point. In this simple example, we simply
               display a message and quit.
               ./
               printf ( "\nSearch was interrupted.\n\nThis message "
                        "was written after an interrupt signal\n"
                        "was trapped. By default, the program "
                        "would have terminated \nbefore this message "
                        "could be written.\n\n"                       );
            }
            else


            ...

-Restrictions

   1)  This routine has no visible effect on operation of user applications
       unless GF interrupt handling is enabled and gfbail_c is used as
       the interrupt signal receipt indicator.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   L.S. Elson          (JPL)

-Version

   -CSPICE Version 1.0.1, 05-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.0.0, 05-FEB-2009 (NJB) (LSE)

-Index_Entries

   GF clear interrupt signal status

-&
*/

{ /* Begin gfclrh_c */


   /*
   Clear the saved interrupt signal handler status.
   */

   zzgfsavh_c ( SPICEFALSE );
}
