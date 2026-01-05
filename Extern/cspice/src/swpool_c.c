/*

-Procedure swpool_c ( Set watch on a pool variable )

-Abstract

   Add a name to the list of agents to notify whenever a member of
   a list of kernel variables is updated.

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

   KERNEL

-Keywords

   CONSTANTS
   FILES

*/
   #include <stdlib.h>
   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZst.h"
   #include "SpiceZmc.h"
   #include "SpiceZim.h"
   #undef    swpool_c


   void swpool_c ( ConstSpiceChar    * agent,
                   SpiceInt            nnames,
                   SpiceInt            namlen,
                   const void        * names   )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   agent      I   The name of an agent to be notified after updates.
   nnames     I   The number of variables to associate with agent.
   namlen     I   Length of strings in the names array.
   names      I   Variable names whose update causes the notice.

-Detailed_Input

   agent       is the name of a routine or entry point (agency) that
               will want to know when a some variables in the kernel
               pool have been updated.

   nnames      is the number of kernel pool variable names that will
               be associated with agent.

   namlen      is the length of the strings in the array `names',
               including the null terminators.

   names       is an array of names of variables in the kernel pool.
               Whenever any of these is updated, a notice will be
               posted for agent so that one can quickly check
               whether needed data has been modified.

-Detailed_Output

   None.

-Parameters

   MAXAGT      the maximum number of agents that can be associated with a given
               kernel variable. MAXAGT is currently set to 1000 agents.

-Exceptions

   1)  If sufficient room is not available to hold a new kernel
       variable name, the error SPICE(KERVARSETOVERFLOW) is signaled
       by a routine in the call tree of this routine.

   2)  If sufficient room is not available to hold a new agent name,
       the error SPICE(TOOMANYWATCHES) is signaled by a routine in
       the call tree of this routine.

   3)  If any kernel variable in the array `names' is already watched
       by MAXAGT agents, and `agent' is not already associated with
       that kernel variable, the error SPICE(AGENTLISTOVERFLOW) is
       signaled by a routine in the call tree of this routine.

   4)  If the `agent' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   5)  If the `agent' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

   6)  If the `names' input array pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   7)  If the `names' input array strings have length less than two
       characters, the error SPICE(STRINGTOOSHORT) is signaled.

-Files

   None.

-Particulars

   The kernel pool is a convenient place to store a wide variety of
   data needed by routines in CSPICE and routines that interface with
   CSPICE routines. However, when a single name has a large quantity
   of data associated with it, it becomes inefficient to constantly
   query the kernel pool for values that are not updated on a frequent
   basis.

   This entry point allows a routine to instruct the kernel pool to
   post a message whenever a particular value gets updated. In this
   way, a routine can quickly determine whether or not data it requires
   has been updated since the last time the data was accessed. This
   makes it reasonable to buffer the data in local storage and update
   it only when a variable in the kernel pool that affects this data
   has been updated.

   Note that swpool_c has a side effect. Whenever a call to swpool_c
   is made, the agent specified in the calling sequence is added to the
   list of agents that should be notified that an update of its
   variables has occurred. In other words the code

       swpool_c ( agent, nnames, namlen, names   );
       cvpool_c ( agent,                 &update );

   will always return update as SPICETRUE.

   This feature allows for a slightly cleaner use of swpool_c and
   cvpool_c as shown in the example below. Because swpool_c
   automatically loads agent into the list of agents to notify of a
   kernel pool update, you do not have to include the code for fetching
   the initial values of the kernel variables in the initialization
   portion of a subroutine. Instead, the code for the first fetch from
   the pool is the same as the code for fetching when the pool is
   updated.

-Examples

   Suppose that you have an application subroutine, MYTASK, that
   needs to access a large data set in the kernel pool. If this
   data could be kept in local storage and kernel pool queries
   performed only when the data in the kernel pool has been
   updated, the routine can perform much more efficiently.

   The code fragment below illustrates how you might make use of this
   feature.

      #include "SpiceUsr.h"
           .
           .
           .
      /.
      On the first call to this routine establish those variables
      that we will want to read from the kernel pool only when
      new values have been assigned.
      ./
      if ( first )
      {
         first = SPICEFALSE;
         swpool_c ( "MYTASK", nnames, namlen, names );
      }

      /.
      If any of the variables has been updated, fetch them from the
      kernel pool.
      ./

      cvpool_c ( "MYTASK", &update );

      if ( update )
      {
         for ( i = 0;  i < NVAR;  i++ )
         {
            gdpool_c( MYTASK_VAR[i], 1, NMAX, n[i], val[i], &found[i] );
         }
      }

-Restrictions

   None.

-Literature_References

   None.

-Author_and_Institution

   N.J. Bachman        (JPL)
   J. Diaz del Rio     (ODC Space)
   W.L. Taber          (JPL)

-Version

   -CSPICE Version 1.4.0, 04-AUG-2021 (JDR)

       Changed the input argument name "lenvals" to "namlen" for consistency
       with other routines.

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.3.0, 27-AUG-2002 (NJB)

       Call to C2F_CreateStrArr_Sig replaced with call to C2F_MapStrArr.

   -CSPICE Version 1.2.0, 28-AUG-2001 (NJB)

       Const-qualified input array names.

   -CSPICE Version 1.1.0, 14-FEB-2000 (NJB)

        Calls to C2F_CreateStrArr replaced with calls to error-signaling
        version of this routine:  C2F_CreateStrArr_Sig.

   -CSPICE Version 1.0.0, 05-JUN-1999 (NJB) (WLT)

-Index_Entries

   Watch for an update to a kernel pool variable
   Notify a routine of an update to a kernel pool variable

-&
*/

{ /* Begin swpool_c */


   /*
   Local variables
   */

   SpiceChar             * fCvalsArr;

   SpiceInt                fCvalsLen;


   /*
   Participate in error tracing.
   */
   chkin_c ( "swpool_c" );


   /*
   Make sure the input string pointer for agent is non-null
   and that the length is sufficient.
   */
   CHKFSTR ( CHK_STANDARD, "swpool_c", agent );

   /*
   Make sure the input string pointer for the names array is non-null
   and that the length namlen is sufficient.
   */
   CHKOSTR ( CHK_STANDARD, "swpool_c", names, namlen );

   /*
   Create a Fortran-style string array.
   */
   C2F_MapStrArr ( "swpool_c",
                   nnames, namlen, names, &fCvalsLen,  &fCvalsArr );

   if ( failed_c() )
   {
      chkout_c ( "swpool_c" );
      return;
   }


   /*
   Call the f2c'd routine.
   */
   swpool_ (  ( char       * ) agent,
              ( integer    * ) &nnames,
              ( char       * ) fCvalsArr,
              ( ftnlen       ) strlen(agent),
              ( ftnlen       ) fCvalsLen      );


   /*
   Free the dynamically allocated array.
   */
   free ( fCvalsArr );


   chkout_c ( "swpool_c" );

} /* End swpool_c */
