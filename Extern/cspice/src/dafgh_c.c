/*

-Procedure dafgh_c ( DAF, get handle )

-Abstract

   Return (get) the handle of the DAF currently being searched.

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

   void dafgh_c ( SpiceInt  * handle )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   handle     O   Handle for current DAF.

-Detailed_Input

   None.

-Detailed_Output

   handle      is the handle for the current DAF (the handle connected
               to the DAF that is currently being actively searched).

-Parameters

   None.

-Exceptions

   1)  If this routine is called when no search is in progress in the
       current DAF, the error SPICE(DAFNOSEARCH) is signaled by a routine in
       the call tree of this routine.

   2)  If the DAF whose handle is to be returned has actually been
       closed, an error is signaled by a routine in the call tree of
       this routine.

-Files

   This routine returns the handle of a DAF that is currently
   being searched.

-Particulars

   Under rare circumstances, it may be necessary to identify the
   particular DAF that is being searched (such as when the search is
   begun by one module and continued by another).

-Examples

   Consider a program like the following, which examines the
   individual arrays in a DAF and examines the contents of those
   meeting certain criteria.

      #include "SpiceUsr.h"
          ...
      dafopw_c ( fname, &handle );
      dafbfs_c ( handle );
      daffna_c ( &found );

      while ( found )
      {
         check_segment ( status );

         if (  eqstr_c( status, "EXAMINE" )  )
         {
            examine_segment();
         }

         daffna_c ( &found );
      }

   The function check_segment, which assumes that a search is in
   progress, gets the summary and name for the current array, and
   uses them to decide whether the data in the array merit further
   consideration.

      void check_segment ( status )
        ...
      dafgs_c ( sum  );
      dafgn_c ( name );
      dafus_c ( sum, nd, ni, dc, ic );
        ...
      [ set status ]
        ...


   The function examine_segment examines the data in
   the array itself. In order to do do, it needs to have access
   not only to the summary, but to the handle of the file
   containing the array. This is provided by dafgh_c.

      void examine_segment()
        ...
      dafgs_c ( sum );
      dafus_c ( sum, nd, ni, dc, ic );
      dafgh_c ( &handle );

      dafgda_c ( handle, begin, end, data );
        ...

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

   -CSPICE Version 1.0.1, 13-APR-2021 (JDR)

       Edited the header to comply with NAIF standard. Moved DAF required
       reading from -Literature_References to -Required_Reading section.

   -CSPICE Version 1.0.0, 19-JUL-2011 (NJB) (WLT) (IMU)

-Index_Entries

   get DAF handle

-&
*/

{ /* Begin dafgh_c */


   /*
   Participate in error tracing.
   */
   chkin_c ( "dafgh_c" );


   dafgh_ ( (integer *) handle );


   chkout_c ( "dafgh_c" );

} /* End dafgh_c */
