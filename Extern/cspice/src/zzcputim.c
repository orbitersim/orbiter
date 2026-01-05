/*

-Procedure zzcputim ( CPU Time )

-Abstract
 
   Fetch the current CPU date and time and store the result 
   as a double precision 6-vector. 
 
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
 
   None. 
 
-Keywords
 
   TIME 
   UTILITY 
 
*/
   #include <time.h>
   #include "SpiceUsr.h"

   int zzcputim_ ( SpiceDouble *tvec ) 

/*

-Brief_I/O
 
   Variable  I/O  Description 
   --------  ---  -------------------------------------------------- 
   tvec       O   contains year, month, day, hours, minutes, seconds 
  
   The function returns a status value that indicates whether an error
   occurred.  This is in addition to standard CSPICE error handling.
   
-Detailed_Input
 
   None. 
 
-Detailed_Output
 
   tvec       is a 6-vector containing the current system time. 
              The various components have the following meaning 
 
                 tvec[0]  --- current calendar year 
                 tvec[1]  --- current month 
                 tvec[2]  --- current day of month 
                 tvec[3]  --- current hour. Hours have a range from 
                              0 to 23.  0 corresponds to system 
                              midnight. 
                 tvec[4]  --- current minutes 
                 tvec[5]  --- current seconds   
 
              All six components will be double precision 
              integers.  (They truncate without change.) 

   The function returns a status value that indicates whether an error
   occurred.  This is in addition to standard CSPICE error handling.
   
-Parameters
 
   None. 
 
-Exceptions
 
   1) If the system time cannot be obtained, the error 
      SPICE(CLIBCALLFAILED) is signaled.  The returned status value
      from the C library routine "time" is output as a function return
      value.
 
-Files
 
   None. 
 
-Particulars
 
   This routine returns the components of the current date and 
   time as determined by the system clock. 
 
   This routine replaces the routine produced by running f2c on
   zzcputim.f.
   
-Examples
 
   Example 1. 
 
   The following routine illustrates how you might use zzcputim_
   to generate a "time stamp" that might be used to tag data 
   you plan to write to a file. 
 
      #include "SpiceUsr.h"
      
      void tstamp ( SpiceChar *stamp ) 
      {

         SpiceDouble   tvec[6]; 
 
         /.
         First fetch the current system time. 
         ./
         zzcputim_ ( tvec ); 
 
         /.
         Now form a time stamp of the form YYYYYMMDDhhmmss.
         ./
         dpfmt_c ( tvec[0], "0yyyy", 5, stamp    ); 
         dpfmt_c ( tvec[1], "0m",    2, stamp+5  ); 
         dpfmt_c ( tvec[2], "0d",    2, stamp+7  ); 
         dpfmt_c ( tvec[3], "0H",    2, stamp+9  ); 
         dpfmt_c ( tvec[4], "0M",    2, stamp+11 ); 
         dpfmt_c ( tvec[5], "0S",    2, stamp+13 ); 
      }
 
 
-Restrictions
 
   None. 
 
-Literature_References
 
   None.
    
-Author_and_Institution
 
   N.J. Bachman   (JPL) 
   K.R. Gehringer (JPL) 
   B.V. Semenov   (JPL) 
 
-Version
 
   -CSPICE Version 1.0.0, 18-FEB-2008 (KRG)(NJB)(BVS)

      Initial version. Except for the function name it is identical 
      to the cputim.c provided in CSUPPORT from 1999 through 2007.

-Index_Entries
 
   get system date and time 
 
-&
*/

{ /* Begin zzcputim_c */

   /*
   Local constants
   */
   
   /*
   The C library routine time returns the value -1 if "the time is
   not available" according to K&R, Second Edition.
   
   We also return this non-zero value if the call to the C library 
   routine localtime gives us a null pointer.
   */
   #define   TIME_ERROR   -1
   
   
   /*
   Local variables
   */
   int                     status;
   time_t                  localTime;
   struct tm             * timeStruct;
   
   
   /*
   Participate in error tracing.
   */
   chkin_c ( "zzcputim_" );

   /*
   Get the local time.  The returned status will be TIME_ERROR if
   an error occurred.
   */
   status  =  time ( &localTime );
   
   if ( status == TIME_ERROR )
   {
      setmsg_c ( "C function \"time\" returned status #." );
      errint_c ( "#",  status                             );
      sigerr_c ( "SPICE(CLIBCALLFAILED)"                  );
      chkout_c ( "zzcputim_"                              );
      return   ( status                                   );
   }
   
   /*
   Get a local pointer to a "tm" structure representing the time.
   We can extract integer components from this structure.
   */
   timeStruct = localtime ( &localTime );

   if (  timeStruct == (struct tm *) NULL  )
   {
      setmsg_c ( "C function \"localtime\" returned null pointer." );
      sigerr_c ( "SPICE(CLIBCALLFAILED)"                           );
      chkout_c ( "zzcputim_"                                       );
      return   ( TIME_ERROR                                        );
   }

   /*
   Set the output time vector.  Conversion from int to double is
   automatic.  Return a value of 0 indicating "success."
   */
   tvec[0] = timeStruct-> tm_year + 1900;
   tvec[1] = timeStruct-> tm_mon  + 1;
   tvec[2] = timeStruct-> tm_mday;
   tvec[3] = timeStruct-> tm_hour;
   tvec[4] = timeStruct-> tm_min;
   tvec[5] = timeStruct-> tm_sec;


   chkout_c ( "zzcputim_" );
   return   ( 0         );

} /* End zzcputim_ */
