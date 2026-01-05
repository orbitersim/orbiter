/*

-Procedure gfbail_c ( GF, interrupt signal indicator )

-Abstract

   Indicate whether an interrupt signal (SIGINT) has been received.

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

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"


   SpiceBoolean gfbail_c ()

/*

-Brief_I/O

   The function returns SPICETRUE if an interrupt signal has
   been received by the GF handler.

-Detailed_Input

   None.

-Detailed_Output

   The function returns SPICETRUE if an interrupt signal has been
   received by the GF handler gfinth_c since the first setting of the
   handler or the last call to gfclrh_c, whichever is most recent.
   Otherwise the function returns SPICEFALSE.

-Parameters

   None.

-Exceptions

   Error free.

-Files

   None.

-Particulars

   This routine returns the interrupt signal handler status maintained
   by the GF subsystem.

   User applications that use default GF interrupt handling should call
   this routine after each call to a GF API that can process an
   interrupt signal. In general, if this routine indicates that an
   interrupt signal was received, any GF processing that was interrupted
   should be presumed to have invalid results.

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
       unless GF interrupt handling is enabled and this routine is used as
       the interrupt signal receipt indicator.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   L.S. Elson          (JPL)

-Version

   -CSPICE Version 1.0.1, 02-JUN-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.0.0, 05-FEB-2009 (NJB) (LSE)

-Index_Entries

   GF interrupt signal receipt indicator

-&
*/

{ /* Begin gfbail_c */

   /*
   Return the saved interrupt status.
  */
   return (  zzgfgeth_c() );


} /* End gfbail_c */
