/*

-Procedure dafgn_c ( DAF, get array name )

-Abstract

   Return (get) the name for the current array in the current DAF.

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
   #include "SpiceZmc.h"
   #include "SpiceZst.h"


   void dafgn_c ( SpiceInt     namlen,
                  SpiceChar  * name   )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   namlen     I   Length of array name string.
   name       O   Name of current array.

-Detailed_Input

   namlen      is the length of the `name' string, including room for the null
               terminator. For a DAF with summary parameters ND and NI, the
               maximum length of an array name is

                                     (NI + 1)
                  NC  =   8 * ( ND + -------- ) (Note that this is
                                         2       integer division.)

               Given NC, `namlen' should be set equal to NC+1.

-Detailed_Output

   name        is the name for the current array (the array found by the
               latest call to daffna_c or daffpa_c).

-Parameters

   None.

-Exceptions

   1)  If this routine is called when no search is in progress in the
       current DAF, the error SPICE(DAFNOSEARCH) is signaled by a
       routine in the call tree of this routine.

   2)  If the DAF for which the "current" array's name is to be
       returned has actually been closed, an error is signaled by a
       routine in the call tree of this routine.

   3)  If no array is current in the current DAF, the error
       SPICE(NOCURRENTARRAY) is signaled by a routine in the call
       tree of this routine. There is no current array when a search
       is started by dafbfs_c or dafbbs_c, but no calls to daffna_c or
       daffpa_c have been made yet, or whenever daffna_c or daffpa_c return
       the value SPICEFALSE in the `found' argument.

   4)  If the `name' output string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   5)  If the `name' output string has length less than two
       characters, the error SPICE(STRINGTOOSHORT) is signaled, since
       the output string is too short to contain one character of
       output data plus a null terminator.

-Files

   None.

-Particulars

   The DAF search routines are:

      dafbfs_c       Begin forward search.
      daffna         Find next array.

      dafbbs_c       Begin backward search.
      daffpa_c       Find previous array.

      dafgs_c        Get summary.
      dafgn_c        Get name.
      dafgh_c        Get handle.

      dafcs_c        Continue search.

   The main function of these entry points is to allow the
   contents of any DAF to be examined on an array-by-array
   basis.

   Conceptually, the arrays in a DAF form a doubly linked list,
   which can be searched in either of two directions: forward or
   backward. It is possible to search multiple DAFs simultaneously.

   dafbfs_c (begin forward search) and daffna are used to search the
   arrays in a DAF in forward order. In applications that search a
   single DAF at a time, the normal usage is

      dafbfs_c ( handle );
      daffna_c ( &found );

      while ( found )
      {
         dafgs_c ( sum  );
         dafgn_c ( name );
          .
          .

         daffna_c ( &found );
      }


   dafbbs_c (begin backward search) and daffpa_c are used to search the
   arrays in a DAF in backward order. In applications that search
   a single DAF at a time, the normal usage is

      dafbbs_c ( handle );
      daffpa_c ( &found );

      while ( found )
      {
         dafgs_c ( sum  );
         dafgn_c ( name );
          .
          .

         daffpa_c ( &found );
      }


   In applications that conduct multiple searches simultaneously,
   the above usage must be modified to specify the handle of the
   file to operate on, in any case where the file may not be the
   last one specified by dafbfs_c or dafbbs_c. The routine dafcs_c
   (DAF, continue search) is used for this purpose. Below, we
   give an example of an interleaved search of two files specified
   by the handles handl1 and handl2. The directions of searches
   in different DAFs are independent; here we conduct a forward
   search on one file and a backward search on the other.
   Throughout, we use dafcs to specify which file to operate on,
   before calling daffna_c, daffpa_c, dafgs_c, or dafgn_c.


      dafbfs_c ( handl1 );
      dafbbs_c ( handl2 );

      dafcs_c  ( handl1  );
      daffna_c ( &found1 );

      dafcs_c  ( handl2  );
      daffpa_c ( &found2 );

      while ( found1 || found2 )
      {
         if ( found1 )
         {
            dafcs_c ( handl1 );
            dafgs_c ( sum    );
            dafgn_c ( name   );
             .
             .
            dafcs_c  ( handl1  );
            daffna_c ( &found1 );
         }

         if ( found2 )
         {
            dafcs_c ( handl2 );
            dafgs_c ( sum    );
            dafgn_c ( name   );
             .
             .
            dafcs_c  ( handl2  );
            daffpa_c ( &found2 );
         }
      }


   At any time, the latest array found (whether by daffna_c or daffpa_c)
   is regarded as the "current" array for the file in which the
   array was found. The last DAF in which a search was started,
   executed, or continued by any of dafbfs_c, dafbbs_c, daffna_c,
   daffpa_c or dafcs_c is regarded as the "current" DAF. The summary
   and name for the current array in the current DAF can be obtained
   separately, as shown above, by calls to dafgs_c (get summary) and
   dafgn_c (get name).  The handle of the current DAF can also be
   obtained by calling dafgh_c (get handle).

   Once a search has been begun, it may be continued in either
   direction. That is, daffpa_c may be used to back up during a
   forward search, and daffna_c may be used to advance during a
   backward search.

-Examples

   1) See -Particulars.

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   W.L. Taber          (JPL)
   I.M. Underwood      (JPL)

-Version

   -CSPICE Version 1.1.0, 25-AUG-2021 (JDR)

       Changed the input argument name "lenout" to "namlen" for consistency
       with other routines.

       Edited the header to comply with NAIF standard. Moved DAF required
       reading from -Literature_References to -Required_Reading section.

       Fixed typo in -Exceptions entry #3: daffpa_c is used to find the
       previous array, not the non existing API dafbna_c.

   -CSPICE Version 1.0.0, 01-AUG-1999 (NJB) (WLT) (IMU)

-Index_Entries

   get DAF array name

-&
*/

{ /* Begin dafgn_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "dafgn_c" );

   /*
   Make sure the output string has at least enough room for one output
   character and a null terminator.  Also check for a null pointer.
   */
   CHKOSTR ( CHK_STANDARD, "dafgn_c", name, namlen );


   dafgn_ ( ( char   * ) name,
            ( ftnlen   ) namlen-1 );

   /*
   Convert the output string to C style.
   */
   F2C_ConvertStr ( namlen, name );


   chkout_c ( "dafgn_c" );

} /* End dafgn_c */
