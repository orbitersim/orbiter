/*

-Procedure cvpool_c ( Check variable in the pool for update)

-Abstract

   Indicate whether or not any watched kernel variables that have a
   specified agent on their notification list have been updated.

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

   SYMBOLS
   UTILITY

*/

   #include "SpiceUsr.h"
   #include "SpiceZfc.h"
   #include "SpiceZmc.h"


   void cvpool_c ( ConstSpiceChar  * agent,
                   SpiceBoolean    * update )

/*

-Brief_I/O

   VARIABLE  I/O  DESCRIPTION
   --------  ---  --------------------------------------------------
   agent      I   Name of the agent to check for notices.
   update     O   SPICETRUE if variables for `agent' have been updated.

-Detailed_Input

   agent       is the name of a function or significant portion of code
               that needs to access variables in the kernel pool.
               Generally this agent will buffer these variables
               internally and fetch them from the kernel pool only when
               they are updated.

-Detailed_Output

   update      is a logical flag that will be set to SPICETRUE if the
               variables in the kernel pool that are associated with `agent'
               have been updated since the last call to cvpool_c.

               `update' will be set to SPICETRUE on the first call made for
               the specified agent, whether or not the associated
               variables have been updated since the agent was placed
               on their notification list, as long as the agent is
               associated with any watched variables.

-Parameters

   See function szpool_c.

-Exceptions

   1)  If the `agent' input string pointer is null, the error
       SPICE(NULLPOINTER) is signaled.

   2)  If the `agent' input string has zero length, the error
       SPICE(EMPTYSTRING) is signaled.

-Files

   None.

-Particulars

   This entry point allows the calling program to determine whether or
   not variables associated with with agent have been updated. Making
   use of this entry point in conjunction with the entry point swpool_c
   (set watch on pool variables) modules can buffer kernel pool
   variables they need and fetch values from the kernel pool only when
   variables have been updated.

   Note that the call to cvpool_c has a side effect. Two consecutive
   calls to cvpool_c with the same agent will always result in the
   update being SPICEFALSE on the second call. In other words, if you
   imbed the following two lines of code in a piece of code

      cvpool_c ( agent, &update );
      cvpool_c ( agent, &update );

   and then test update, it will be SPICEFALSE. The idea is that once
   a call to cvpool_c has been made, the kernel pool has performed its
   duty and notified the calling routine that one of the agent's
   variables has been updated. Consequently, on the second call to
   cvpool_c above, the kernel pool will not have any updates to report
   about any of agent's variables.

   If, on the other hand, you have code such as

      cvpool_c ( agent, &update );
      furnsh_c ( "myfile.dat"   );
      cvpool_c ( agent, &update );

   the value of update will be true if one of the variables associated
   with agent was updated by the call to furnsh_c (and that variable
   has been specified as one to watch by call a call to swpool_c).

   It should also be noted that any call to cvpool_c that occurs
   immediately after a call to swpool_c will result in update being
   returned as SPICETRUE  In other words, code such as shown below,
   will always result in the value of UPDATE as being returned
   SPICETRUE:

      swpool_c ( agent, nnames, namelen, names   );
      cvpool_c ( agent,                  &update );

   See the header for swpool_c for a full discussion of this
   feature.

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
         swpool_c ( "MYTASK", nnames, namelen, names );
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
   E.D. Wright         (JPL)

-Version

   -CSPICE Version 1.0.3, 04-AUG-2021 (JDR)

       Edited the header to comply with NAIF standard.

   -CSPICE Version 1.0.2, 30-JUN-2014 (NJB)

       Description of the output variable `update' now mentions that
       the initial value of SPICETRUE will be returned after an agent
       is associated with kernel variables.

   -CSPICE Version 1.0.1, 14-AUG-2006 (EDW)

      Replace mention of ldpool_c with furnsh_c.

   -CSPICE Version 1.0.0, 05-JUN-1999 (NJB) (WLT)

-Index_Entries

   Check the kernel pool for updated variables

-&
*/

{ /* Begin cvpool_c */


   /*
   Local variables
   */
   logical                 upd;


   /*
   Use discovery check-in.
   */

   /*
   Check the input agent name to make sure the pointer is non-null
   and the string length is non-zero.
   */
   CHKFSTR ( CHK_DISCOVER, "cvpool_c", agent );


   /*
   Call the f2c'd routine.
   */
   cvpool_ (  ( char    * ) agent,
              ( logical * ) &upd,
              ( ftnlen    ) strlen(agent)  );


   /*
   Assign the SpiceBoolean output argument.
   */

   *update = upd;


} /* End cvpool_c */
