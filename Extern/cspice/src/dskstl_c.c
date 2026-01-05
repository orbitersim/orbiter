/*

-Procedure dskstl_c ( DSK, set tolerance )

-Abstract

   Set the value of a specified DSK tolerance or margin parameter.

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

   DSK

-Keywords

   DSK
   MARGIN
   NUMERIC
   TOLERANCE

*/
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"


   void dskstl_c ( SpiceInt        keywrd,
                   SpiceDouble     dpval  )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   keywrd     I   Code specifying parameter to set.
   dpval      I   Value of parameter.

-Detailed_Input

   keywrd      is an integer code specifying the parameter to set. See
               the include file SpiceDtl.h for a description of the
               possible keywords.

   dpval       is the new value of the parameter specified by `keywrd'.

               <<< Use extreme caution. This routine performs no
               checks on DPVAL. >>>

-Detailed_Output

   None.

   This routine operates by side effects.

-Parameters

   See the header file

      SpiceDtl.h

   for descriptions and values of the tolerance or margin parameters
   accessed by this routine, and of the keyword parameters used to
   refer to them.

-Exceptions

   1)  If the input keyword is not recognized, the error
       SPICE(INDEXOUTOFRANGE) is signaled by a routine in the call
       tree of this routine.

   2)  If an attempt is made to modify a fixed parameter, the error
       SPICE(IMMUTABLEVALUE) is signaled by a routine in the call
       tree of this routine.

-Files

   None.

-Particulars

   The DSK tolerance routines centralize numeric tolerance and margin
   values used by the DSK subsystem. The DSK subsystem retrieves values
   from the DSK tolerance subsystem to use at run time.

   The DSK tolerance access functions are

      dskgtl_c {DSK, get tolerance value}
      dskstl_c {DSK, set tolerance value}

   To minimize run time overhead, the "keywords" used by these routines
   to identify parameters are actually integer codes.

   SPICE users may override certain values maintained by this subsystem;
   others values are fixed. It is recommended that any change to the
   tolerance values made at run time be performed only by expert SPICE
   users.

-Examples

   The numerical results shown for this example may differ across
   platforms. The results depend on the SPICE kernels used as
   input, the compiler and supporting libraries, and the machine
   specific arithmetic implementation.

   1) Obtain, display, and update the DSK type 2 plate expansion
      fraction.


      Example code begins here.


      /.
         Program dskstl_ex1
      ./
      #include <stdio.h>
      #include "SpiceUsr.h"

      int main()
      {
         SpiceDouble             dpval;

         dskgtl_c ( SPICE_DSK_KEYXFR, &dpval );

         printf ( "Default plate expansion fraction = %e\n", dpval );

         /.
         Update the parameter.
         ./
         dskstl_c ( SPICE_DSK_KEYXFR, 1.e-8  );

         /.
         Verify the update.
         ./
         dskgtl_c ( SPICE_DSK_KEYXFR, &dpval );

         printf ( "New plate expansion fraction     = %e\n", dpval );

         return ( 0 );
      }


      When this program was executed on a Mac/Intel/cc/64-bit
      platform, the output was:


      Default plate expansion fraction = 1.000000e-10
      New plate expansion fraction     = 1.000000e-08


-Restrictions

   1)  The default settings used by the DSK subsystem should be
       overridden only by expert SPICE users.

   2)  This routine do not check the validity of new parameter values
       supplied by the calling application.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)

-Version

   -CSPICE Version 1.0.1, 10-AUG-2021 (JDR)

       Corrected -Procedure section's description.

       Edited the header to comply with NAIF standard. Added example's
       solution. Corrected CSPICE include file name reference in
       "keywrd" description.

   -CSPICE Version 1.0.0, 27-FEB-2016 (NJB)

-Index_Entries

   set DSK tolerance or margin parameters

-&
*/

{ /* Begin dskstl_c */



   /*
   Participate in error tracing.
   */
   chkin_c ( "dskstl_c" );


   dskstl_ ( (integer     *) &keywrd,
             (doublereal  *) &dpval   );


   chkout_c ( "dskstl_c" );

} /* End dskstl_c */
