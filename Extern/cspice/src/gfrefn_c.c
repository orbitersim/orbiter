/*

-Procedure gfrefn_c ( GF, default refinement estimator )

-Abstract

   Estimate, using a bisection method, the next abscissa value at
   which a state change occurs. This is the default GF refinement
   method.

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

   SEARCH
   UTILITY

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"

   void gfrefn_c ( SpiceDouble     t1,
                   SpiceDouble     t2,
                   SpiceBoolean    s1,
                   SpiceBoolean    s2,
                   SpiceDouble   * t  )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   t1         I   One of two values bracketing a state change.
   t2         I   The other value that brackets a state change.
   s1         I   State at `t1'.
   s2         I   State at `t2'.
   t          O   New value at which to check for transition.

-Detailed_Input

   t1          is one of two abscissa values (usually times)
               bracketing a state change.

   t2          is the other abscissa value that brackets a state change.

   s1          is the system state at `t1'. This argument is provided
               for forward compatibility; it's not currently used.

   s2          is the system state at `t2'. This argument is provided
               for forward compatibility; it's not currently used.

-Detailed_Output

   t           is the midpoint of `t1' and `t2'.

-Parameters

   None.

-Exceptions

   Error free.

-Files

   None.

-Particulars

   "Refinement" means reducing the size of a bracketing interval on
   the real line in which a solution is known to lie. In the GF
   setting, the solution is the time of a state transition of a
   binary function.

   This routine supports solving for locations of bracketed state
   transitions by the bisection method. This is the default
   refinement method used by the GF system.

   The argument list of this routine is compatible with the GF
   system's general root finding routine. Refinement routines created
   by users must have the same argument list in order to be used by
   the GF mid-level APIs such as gfocce_c and gffove_c.

-Examples

   The following code fragment from an example program in the header of
   gfocce_c shows the address of this routine passed as the 12th argument.

      /.
      Perform the search.
      ./
      gfocce_c ( "ANY",
                 "MOON",     "ellipsoid",  "IAU_MOON",
                 "SUN",      "ellipsoid",  "IAU_SUN",
                 "LT",       "EARTH",      CNVTOL,
                 &gfstep_c,  &gfrefn_c,    rpt,
                 &gfrepi_c,  &gfrepu_c,    &gfrepf_c,
                 bail,       &gfbail_c,    &cnfine,
                 &result                              );

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.0.1, 01-NOV-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.0.0, 15-APR-2009 (NJB) (EDW)

-Index_Entries

   GF standard step refinement

-&
*/

{ /* Begin gfrefn_c */


   /*
   Local variables
   */
   logical                 ls1;
   logical                 ls2;


   /*
   This routine is error free; tracing is not performed.
   */

   /*
   Let the f2c'd routine do the work.
   */
   ls1 = (logical) s1;
   ls2 = (logical) s2;

   gfrefn_ ( ( doublereal * ) &t1,
             ( doublereal * ) &t2,
             ( logical    * ) &ls1,
             ( logical    * ) &ls2,
             ( doublereal * ) t     );

} /* End gfrefn_c */
