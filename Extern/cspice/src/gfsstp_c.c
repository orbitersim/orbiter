/*

-Procedure gfsstp_c ( Geometry finder set step size )

-Abstract

   Set the step size to be returned by gfstep_c.

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
   TIME

-Keywords

   GEOMETRY
   SEARCH
   UTILITY

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"

   void gfsstp_c ( SpiceDouble  step )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   step       I   Time step to take.

-Detailed_Input

   step        is the output step size to be returned by the next call
               to gfstep_c. Units are TDB seconds.

               `step' is used in the GF search root-bracketing process.
               `step' indicates how far to advance the gfstep_c input
               argument `time' so that `time' and time+step may bracket a
               state transition and definitely do not bracket more than
               one state transition.

-Detailed_Output

   None.

-Parameters

   None.

-Exceptions

   1)  If the input step size is non-positive, the error
       SPICE(INVALIDSTEP) is signaled by a routine in the call tree
       of this routine. The stored step value is not updated.

-Files

   None.

-Particulars

   This routine sets the step size to be returned by the
   next call to gfstep_c.

-Examples

   1) User applications can pass gfstep_c to mid-level GF API routines
      expecting a step size routine as an input argument. Before such
      a call is made, the value of the step to be returned by gfstep_c
      must be set via a call to this routine.

      For example, the GF API routine gfocce_c can be called as shown
      in the code fragment below.

            /.
            Select a twenty-second step. We'll ignore any occultations
            lasting less than 20 seconds.
            ./
            step = 20.0;
            gfsstp_c ( step );

            /.
            Perform the search.
            ./
            gfocce_c ( "ANY",
                       "MOON",     "ellipsoid",  "IAU_MOON",
                       "SUN",      "ellipsoid",  "IAU_SUN",
                       "LT",       "EARTH",      CNVTOL,
                       gfstep_c,   gfrefn_c,     rpt,
                       gfrepi_c,   gfrepu_c,     gfrepf_c,
                       bail,       gfbail_c,     cnfine,
                       &result                              );

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   L.S. Elson          (JPL)

-Version

   -CSPICE Version 1.0.1, 06-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.0.0, 15-APR-2009 (LSE) (NJB)

-Index_Entries

   GF set constant step size

-&
*/

{ /* Begin gfsstp_c */



   /*
   Participate in error tracing.
   */

   if ( return_c() )
   {
      return;
   }

   chkin_c ( "gfsstp_c" );

   /*
   Let the f2c'd routine do the work.
   */

   gfsstp_ (  (doublereal * ) &step );

   chkout_c ( "gfsstp_c" );

} /* End gfsstp_c */
