/*

-Procedure dafrda_c ( DAF, read data from address )

-Abstract

   Deprecated: This routine has been superseded by the CSPICE
   routines dafgda_c and dafgsr_c. This routine is supported for 
   purposes of backward compatibility only.

   Read the double precision data bounded by two addresses within
   a DAF.

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

   DAF

-Keywords

   FILES

*/
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"


   void dafrda_c ( SpiceInt       handle,
                   SpiceInt       begin,
                   SpiceInt       end,
                   SpiceDouble  * data )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     I   Handle of a DAF.
   begin,
   end        I   Initial, final address within file.
   data       O   Data contained between begin and end.

-Detailed_Input

   handle      is the handle of a DAF.

   begin,
   end         are the initial and final addresses of a contiguous
               set of double precision numbers within a DAF.
               Presumably, these make up all or part of a particular
               array.

               Note that CSPICE DAF addresses begin at 1 as in the
               FORTRAN version of the SPICE Toolkit.

-Detailed_Output

   data        are the double precision data contained between
               the specified addresses within the specified file.

-Parameters

   None.

-Exceptions

   1)  If `begin' is zero or negative, the error SPICE(DAFNEGADDR)
       is signaled by a routine in the call tree of this routine.

   2)  If begin > end, the error SPICE(DAFBEGGTEND) is signaled by a
       routine in the call tree of this routine.

   3)  If the file associated with `handle' is not of the native
       binary file format, the error SPICE(UNSUPPORTEDBFF) is
       signaled by a routine in the call tree of this routine.

   4)  If `handle' is invalid, an error is signaled by a routine in
       the call tree of this routine.

-Files

   None.

-Particulars

   The principal reason that DAFs are so easy to use is that
   the data in each DAF are considered to be one long contiguous
   set of double precision numbers. You can grab data from anywhere
   within a DAF without knowing (or caring) about the physical
   records in which they are stored.

   This routine has been made obsolete by the routines dafgda_c and
   dafgsr_c. This routine is supported for reasons of backward
   compatibility only. New software development should utilize
   dafgda_c or dafgsr_c.

-Examples

   The following code fragment illustrates the use of dafrda_c
   to read data from an imaginary array. The array begins with a
   directory containing 11 epochs. Each pair of epochs bounds
   an interval, and each interval is covered by a set of eight
   osculating elements.

      #include "SpiceUsr.h"

         .
         .
         .

      dafus_c ( sum, nd, ni, dc, ic );
      begin = ic[4];
      end   = ic[5];

      dafrda_c ( handle, begin, begin+10, epochs );

      for ( i = 0;  i < 10;  i++ )
      {
         if (     ( et > epochs[i]   )
              &&  ( et < epochs[i+1] ) )
         {
            offset = ic[4] + 11 + (i - 1) * 8;
            dafrda_c ( handle, offset+1, offset+8, elements );
            return;
         }
      }

-Restrictions

   1)  This routine is deprecated. See the routines dafgda_c and
       dafgsr_c.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   B.V. Semenov        (JPL)
   W.L. Taber          (JPL)
   R.E. Thurman        (JPL)
   F.S. Turner         (JPL)
   I.M. Underwood      (JPL)
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.0.4, 26-OCT-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.0.3, 19-MAY-2010 (BVS)

       Index line now states that this routine is deprecated.

   -CSPICE Version 1.0.2, 23-JAN-2008 (EDW)

       Removed a spurious and unneeded "-Declarations"
       tag. The tag's presence prevented the HTML API doc
       script from parsing the function description.

   -CSPICE Version 1.0.1, 27-OCT-2003 (NJB) (FST)

       The header now states that this routine is deprecated.
       The -Exceptions header section has been extended.
       Minor additional header updates were made.

   -CSPICE Version 1.0.0, 14-DEC-1999 (NJB) (RET) (IMU) (WLT)

-Index_Entries

   DEPRECATED read data from DAF address

-&
*/

{ /* Begin dafrda_c */

   /*
   Participate in error tracing.
   */
   chkin_c ( "dafrda_c" );

   dafrda_ ( ( integer    * ) &handle,
             ( integer    * ) &begin,
             ( integer    * ) &end,
             ( doublereal * ) data );

   chkout_c ( "dafrda_c" );

} /* End of dafrda_c */
