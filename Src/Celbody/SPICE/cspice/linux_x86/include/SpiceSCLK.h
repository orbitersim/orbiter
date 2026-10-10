
/*

-Header_File SpiceSCLK.h ( CSPICE SCLK-specific definitions )

-Abstract

  Perform CSPICE definitions to support SCLK wrapper interfaces,
  including macros.

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

   SCLK

-Particulars

   This header defines macros that may be referenced in application
   code that calls CSPICE SCLK interfaces.

   The parameters below define sizes and limits used by the SCLK system.

   Macros
   ======

      Name                  Description
      ----                  -----------
      SPICE_SCLK_MXPART     is the maximum number of partitions in a given
                            SCLK file.

-Literature_References

   None.

-Author_and_Institution

   J. Diaz del Rio     (ODC Space)

-Restrictions

   It is recommended that the default values defined in this file be
   changed only by expert SPICE users.

-Version

   -CSPICE Version 1.0.0, 16-SEP-2020 (JDR)

*/


#ifndef HAVE_SPICE_SCLK_H

   #define HAVE_SPICE_SCLK_H


   /*
   Maximum number of partitions in a SCLK file:
   */
   #define  SPICE_SCLK_MXPART               9999


#endif

/*
End of header file SpiceSCLK.h
*/
