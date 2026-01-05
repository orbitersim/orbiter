/*

-Procedure chkout_c ( Module Check Out )

-Abstract

   Inform the CSPICE error handling mechanism of exit from a
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


   void chkout_c ( ConstSpiceChar  * module )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   module     I   The name of the calling routine.

-Detailed_Input

   module      is the name of the routine calling chkout_c. The
               named routine is supposed to be `checking out'
               when it calls chkout_c; that is, the call should be
               the last executable statement preceding any exit
               from the routine.

-Detailed_Output

   None.

-Parameters

   None.

-Exceptions

   1)  If the `module' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   2)  If the `module' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

   The underlying f2c'd CSPICE routine chkout_ does not signal errors;
   rather it writes error messages, so as to avoid recursion. The
   errors detected by chkout_ are:

   3)  If the input module name module does not match the name popped
       from the trace stack, the short error message
       SPICE(NAMESDONOTMATCH) is written to the error output device.

   4)  If the trace stack is empty, the short error message
       SPICE(TRACESTACKEMPTY) is written to the error output device.

-Files

   None.

-Particulars

   This routine is part of the CSPICE error handling mechanism.

   Conceptually, the effect of this routine is to `pop' a module
   name from a stack. The routine chkin_c performs the inverse, or
   `push' operation.

   Every routine that participates in the traceback scheme should
   have a call to chkin_c as the second executable statement.
   The first executable statements should be:

      if ( return_c() )
         {
         return;
         }
      else
         {
         chkin_c ( module );
         }


   Here module is the name of the routine in which this code appears.

   The line of code preceding the exit or any return statement
   should be

      chkout_c  ( module );

   All CSPICE routines should call chkin_c and chkout_c, unless they
   are classified as `error free'. Programs linked with CSPICE
   may also use chkin_c and chkout_c.

   Routines that don't call chkin_c and chkout_c won't appear in the
   traceback.

   All routines that call chkin_c must also call chkout_c, or else the
   trace mechanism will become very confused and need alot of therapy.

   It is possible to disable check-ins (and check-outs) by calling
   the trcoff_c. chkin_c and chkout_c will return immediately
   upon entry after trcoff_c has been called. It is not possible to
   re-enable check-ins and check-outs after calling trcoff_c. Routines
   that don't call chkin_c and chkout_c won't appear in the traceback.

-Examples

   1)  Call chkout_c before a return statement:

          if ( failed() )
             {
             chkout_c ( module );
             return;
             }


   2)  Call chkout_c before an exit statement:

          chkout_c ( module );
          exit;


   3)  Only ONE call to chkout_c is needed here:

          chkout_c ( module ) ;
          return;

-Restrictions

   1)  Routines that call this routine must call chkin_c as the second
       executable statement. (The first is a call to return_c() ).

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 2.0.2, 02-JUN-2021 (JDR)

       Edited the header to comply with NAIF standard. Added entries
       #1 and #2 to -Exceptions section.

   -CSPICE Version 2.0.1, 08-FEB-1998 (EDW)

       Corrected and clarified header entries.

   -CSPICE Version 2.0.0, 09-JAN-1998 (NJB)

       Input argument filename was changed to type ConstSpiceChar *.

       Re-implemented routine without dynamically allocated, temporary
       strings.

   -CSPICE Version 1.0.0, 25-OCT-1997 (EDW)

-Index_Entries

   module check out

-&
*/

{ /* Begin chkout_c */

   /*
   Check the input string module to make sure the pointer is non-null
   and the string length is non-zero.  Use discovery check-in.  If an
   error is found, this wrapper will be called recursively, but that
   should not cause a problem.
   */
   CHKFSTR ( CHK_DISCOVER, "chkout_c", module );

   /*
   Call the f2c'd Fortran routine.
   */
   chkout_ ( ( char   * ) module,
             ( ftnlen   ) strlen(module) );


} /* End chkout_c */
