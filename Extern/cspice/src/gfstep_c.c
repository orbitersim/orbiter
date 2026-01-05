/*

-Procedure gfstep_c ( Geometry finder step size )

-Abstract

   Return the time step set by the most recent call to gfsstp_c.

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

   void gfstep_c ( SpiceDouble    time,
                   SpiceDouble  * step )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   time       I   Ignored ET value.
   step       O   Time step to take.

-Detailed_Input

   time        is an ignored double precision number. This argument
               is present so the argument list of this routine is
               compatible with the GF step size routine argument list
               specification.

               When this routine is called from within the GF
               root-finding system, either the initial ET value of the
               current interval of the confinement window, or the
               value resulting from the last search step, is passed in
               via the `time' argument.

-Detailed_Output

   step        is the output step size. This is the value set by the
               most recent call to gfsstp_c. Units are TDB seconds.

               `step' is used in the GF search root-bracketing process.
               `step' indicates how far to advance `time' so that `time' and
               time+step may bracket a state transition and definitely
               do not bracket more than one state transition.

-Parameters

   None.

-Exceptions

   1)  If this routine is called before a step size has been
       set via a call to gfsstp_c, the error SPICE(NOTINITIALIZED)
       is signaled by a routine in the call tree of this routine.

-Files

   None.

-Particulars

   This routine returns the time step set by the most recent call to
   gfsstp_c.

-Examples

   1) User applications can pass gfstep_c to mid-level GF API routines
      expecting a step size routine as an input argument. For
      example, the GF API routine gfocce_c can be called as shown
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

   GF get constant step size

-&
*/

{ /* Begin gfstep_c */


   /*
   Participate in error tracing.
   */

   if ( return_c() )
   {
      return;
   }
   chkin_c ( "gfstep_c" );

   /*
   Let the f2c'd routine do the work.
   */

   gfstep_ (  ( doublereal *  ) &time,
              ( doublereal *  ) step   );

   chkout_c ( "gfstep_c" );

} /* End gfstep_c */
