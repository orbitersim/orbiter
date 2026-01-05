/*

-Procedure chkin_c ( module Check In )

-Abstract

   Inform the CSPICE error handling mechanism of entry into a
   routine.

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

   ERROR

-Keywords

   ERROR

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"


   void chkin_c ( ConstSpiceChar * module )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  ---------------------------------------------------
   module     I   The name of the calling routine.

-Detailed_Input

   module      is the name of the routine calling chkin_c. The
               named routine is supposed to be `checking in'
               when it calls chkin_c; that is, the call should be
               the first executable statement following the
               reference to the function return_c() (which should be
               the first executable statement).

-Detailed_Output

   None.

-Parameters

   None.

-Exceptions

   1)  If the `module' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   2)  If the `module' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

   The underlying f2c'd CSPICE routine chkin_ does not signal errors;
   rather it writes error messages, so as to avoid recursion. The
   errors detected by chkin_ are:

   3)  If the traceback storage area overflows, the short error
       message SPICE(TRACEBACKOVERFLOW) is written to the error
       output device.

   4)  If the input argument module is blank, the short error message
       SPICE(BLANKMODULENAME) is written to the error output device.

-Files

   None.

-Particulars

   This routine is part of the CSPICE error handling mechanism.

   Conceptually, the effect of this routine is to `push' the
   supplied module name onto a stack. The routine chkout_c performs
   the inverse, or `pop', operation.

   Every routine that participates in the traceback scheme should
   have a call to chkin_c as the second executable statement. The
   first executable statements should be:

      if ( return_c() )
         {
         return;
         }
      else
         {
         chkin_c ( module );
         }


   Here module is the name of the routine in which this code appears.

   The line of code preceding the exit or any return statement should
   be

       chkout_c ( module );


   All CSPICE routines should call chkin_c and chkout_c, unless they
   are classified as `error free'. Programs linked with CSPICE
   may also use chkin_c and chkout_c.

   Routines that don't call chkin_c and chkout_c won't appear in the
   traceback.

   All routines that call chkin_c must also call chkout_c, or else the
   trace mechanism will become very confused and require therapy.

   It is possible to disable check-ins (and check-outs) by calling
   the trcoff_c. chkin_c and chkout_c will return immediately
   upon entry after trcoff_c has been called. It is not possible to
   re-enable check-ins and check-outs after calling trcoff_c. Routines
   that don't call chkin_c and chkout_c won't appear in the traceback.

-Examples

   See `Particulars' for an example of how to call this routine.

-Restrictions

   1)  Routines that call this routine must call chkout_c immediately
       prior to any return or exit statement.

   2)  Module names are assumed to have no embedded blanks.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 2.0.4, 13-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 2.0.3, 23-JUL-2001 (NJB)

       Tabs removed from source file.

   -CSPICE Version 2.0.2, 25-MAR-1998 (EDW)

       Minor corrections to header.

   -CSPICE Version 2.0.1, 08-FEB-1998 (EDW)

       Corrected and clarified header entries.

   -CSPICE Version 2.0.0, 09-JAN-1998 (NJB)

       Input argument filename was changed to type ConstSpiceChar *.

       Re-implemented routine without dynamically allocated, temporary
       strings.

   -CSPICE Version 1.0.0, 25-OCT-1997 (EDW)

-Index_Entries

   module check in

-&
*/

{ /* Begin chkin_c */


   /*
   Check the input string module to make sure the pointer is non-null
   and the string length is non-zero.  Use discovery check-in.  If an
   error is found, this wrapper will be called recursively, but that
   should not cause a problem.
   */
   CHKFSTR ( CHK_DISCOVER, "chkin_c", module );


   /*
   Call the f2c'd Fortran routine.
   */
   chkin_ ( ( char    * ) module,
            ( ftnlen    ) strlen(module) );



} /* end chkin_c */
