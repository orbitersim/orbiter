/*

-Procedure dafgsr_c ( DAF, get summary/descriptor record )

-Abstract

   Read a portion of the contents of a summary record in a DAF file.

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
   #include "SpiceZst.h"

   void dafgsr_c  ( SpiceInt        handle,
                    SpiceInt        recno,
                    SpiceInt        begin,
                    SpiceInt        end,
                    SpiceDouble   * data,
                    SpiceBoolean  * found  )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     I   Handle of DAF.
   recno      I   Record number.
   begin      I   First word to read from record.
   end        I   Last word to read from record.
   data       O   Contents of record.
   found      O   SPICETRUE if record is found.

-Detailed_Input

   handle      is the handle associated with a DAF.

   recno       is the record number of a particular double precision
               record within the DAF, whose contents are to be read. DAF
               record numbers start at 1.

   begin       is the first word in the specified record to be returned.
               Word numbers range from 1 to 128.

   end         is the final word in the specified record to be returned.
               Word numbers range from 1 to 128.

-Detailed_Output

   data        contains the specified portion (from `begin' to `end',
               inclusive) of the specified record from the specified
               file.

   found       is SPICETRUE when the specified record is found, and is
               SPICEFALSE otherwise.

-Parameters

   None.

-Exceptions

   1)  If `handle' does not belong to any file that is currently open,
       an error is signaled by a routine in the call tree of this
       routine.

   2)  If an error occurs while reading the record, the error is
       signaled by a routine in the call tree of this routine.

   3)  Bad values for `begin' and `end' ( begin < 1, end > 128,
       end < begin ) are not diagnosed. See -Particulars and
       -Restrictions for the effect of this routine in this case.

-Files

   The input `handle' must refer to a DAF file that is open for read
   or write access.

-Particulars

   dafgsr_c checks the DAF record buffer to see if the requested
   record can be returned without actually reading it from
   external storage. If not, it reads the record and stores
   it in the buffer, typically removing another record from
   the buffer as a result.

   Once in the buffer, the specified portion of the record is
   returned, using the following control loop.

      for ( j = 0, i = max(1,begin);  i <= min(128,end);  i++, j++ )
      {
         data[j] =  buffered_DAF_record[i];
      }

   Therefore bad values for `begin' and `end' (begin < 1, end < begin,
   etc.) are not signaled as errors, but result in the actions
   implied by the above.

-Examples

   The following code fragment illustrates one way that dafgsr_c
   and dafwdr_ can be used to update part of a summary record.
   If the record does not yet exist, we can assume that it is
   filled with zeros.

      #include "SpiceUsr.h"
      #include "SpiceZfc.h"

      SpiceInt    size   = 128;
      SpiceInt    recno;
      SpiceInt    handle;
          .
          .
          .
      dafgsr_c ( handle, recno, 1, 128, drec, &found );

      if ( !found )
      {
          cleard_ ( &size, drec );
      }

      for ( i = first;  i <= last;  i++ )
      {
          drec[i] = new_value[i];
      }

      dafwdr_ ( &handle, &recno, drec );

   Note that since only entire records may be written using dafwdr_,
   the entire record needs to be read also.

-Restrictions

   1)  Bad values for `begin' and `end' ( begin < 1, end > 128,
       end < begin ) are not signaled as errors. The effects of
       such assignments on the returned data are defined by the
       following control structure:

          for ( j = 0, i = max(1,begin);  i <= min(128,end);  i++, j++ )
          {
             data[j] =  buffered_DAF_record[i];
          }

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   F.S. Turner         (JPL)

-Version

   -CSPICE Version 1.1.0, 08-AUG-2021 (JDR) (NJB)

       Bug fix: now the number of records read is not incremented once
       it reaches intmax_c, if it does.

       Edited the header to comply with NAIF standard.

       Updated -Detailed_Input, -Exceptions, -Particulars and -Restrictions
       sections.

   -CSPICE Version 1.0.0, 17-JUN-2009 (NJB) (FST)

-Index_Entries

   read DAF summary record

-&
*/

{ /* Begin dafgsr_c */

   /*
   Local variables
   */
   logical                 fnd;


   /*
   Participate in error tracing.
   */
   chkin_c ( "dafgsr_c" );


   dafgsr_ ( ( integer    * ) &handle,
             ( integer    * ) &recno,
             ( integer    * ) &begin,
             ( integer    * ) &end,
             ( doublereal * ) data,
             ( logical    * ) &fnd   );

   *found = (SpiceBoolean) fnd;


   chkout_c ( "dafgsr_c" );

} /* End dafgsr_c */
