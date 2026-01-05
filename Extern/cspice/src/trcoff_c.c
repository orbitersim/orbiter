/*

-Procedure trcoff_c  ( Turn tracing off )

-Abstract

   Disable tracing.

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


   void trcoff_c ( void )

/*

-Brief_I/O

   None.

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

   This routine disables tracing. Checking in or out does not modify
   the current traceback any further after trcoff_c is called. The
   routines trcnam_, trcdep_, and qcktrc_ will return information
   based on the traceback at the point where trcoff_c is called.

   Once tracing has been disabled, it cannot be re-enabled.

   Additionally, trcoff_c blanks out the existing trace, since the
   trace will usually be invalid at the time an error is signaled.
   The frozen copy of the trace, if there is one, is not modified.

-Examples

   1)    /.
               Program initialization:

                    .
                    .
                    .

               We disable tracing to enhance speed:
         ./
               trcoff_c ();
         /.
               More initialization code:
         ./
                    .
                    .
                    .

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.0.1, 13-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.0.0, 08-FEB-1998 (EDW)

-Index_Entries

   turn tracing off

-&
*/

{ /* Begin trcoff_c */

   /*
   Call the f2c'd routine
   */
   trcoff_();



} /* End trcoff_c */
