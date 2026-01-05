/* dasrwr.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__10 = 10;
static integer c__1 = 1;
static integer c__128 = 128;
static logical c_false = FALSE_;
static integer c__256 = 256;
static integer c__1024 = 1024;

/* $Procedure DASRWR ( DAS, read/write records ) */
/* Subroutine */ int dasrwr_0_(int n__, integer *handle, integer *recno, char 
	*recc, doublereal *recd, integer *reci, integer *first, integer *last,
	 doublereal *datad, integer *datai, char *datac, ftnlen recc_len, 
	ftnlen datac_len)
{
    /* Initialized data */

    static logical pass1 = TRUE_;
    static integer rnbufi[10] = { 0,0,0,0,0,0,0,0,0,0 };
    static integer hnbufc[10] = { 0,0,0,0,0,0,0,0,0,0 };
    static integer hnbufd[10] = { 0,0,0,0,0,0,0,0,0,0 };
    static integer hnbufi[10] = { 0,0,0,0,0,0,0,0,0,0 };
    static logical upbufc[10] = { FALSE_,FALSE_,FALSE_,FALSE_,FALSE_,FALSE_,
	    FALSE_,FALSE_,FALSE_,FALSE_ };
    static logical upbufd[10] = { FALSE_,FALSE_,FALSE_,FALSE_,FALSE_,FALSE_,
	    FALSE_,FALSE_,FALSE_,FALSE_ };
    static logical upbufi[10] = { FALSE_,FALSE_,FALSE_,FALSE_,FALSE_,FALSE_,
	    FALSE_,FALSE_,FALSE_,FALSE_ };
    static integer headc = 0;
    static integer headd = 0;
    static integer headi = 0;
    static integer usedc = 0;
    static integer unit = -1;
    static integer wrunit = -1;
    static integer usedd = 0;
    static integer usedi = 0;
    static char rcbufc[1024*10] = "                                         "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                       " "                                      "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                          " "                                   "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                             " "                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                " "                             "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                   " "                          "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                      " "                       "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                         " "                    "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                            " "                 "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                               " "              "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                  ";
    static doublereal rcbufd[1280]	/* was [128][10] */ = { 0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
	    0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0. 
	    };
    static integer rcbufi[2560]	/* was [256][10] */ = { 0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0 };
    static integer rnbufc[10] = { 0,0,0,0,0,0,0,0,0,0 };
    static integer rnbufd[10] = { 0,0,0,0,0,0,0,0,0,0 };

    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static integer node, next;
    extern /* Subroutine */ int zzdasgrd_(integer *, integer *, doublereal *),
	     zzddhhlu_(integer *, char *, logical *, integer *, ftnlen), 
	    zzdasgri_(integer *, integer *, integer *), chkin_(char *, ftnlen)
	    , lnkan_(integer *, integer *), moved_(doublereal *, integer *, 
	    doublereal *);
    static integer poolc[32]	/* was [2][16] */, poold[32]	/* was [2][16]
	     */;
    extern /* Subroutine */ int movei_(integer *, integer *, integer *);
    static integer pooli[32]	/* was [2][16] */;
    extern integer lnktl_(integer *, integer *);
    extern logical failed_(void);
    extern /* Subroutine */ int dasioc_(char *, integer *, integer *, char *, 
	    ftnlen, ftnlen), dasiod_(char *, integer *, integer *, doublereal 
	    *, ftnlen), dasioi_(char *, integer *, integer *, integer *, 
	    ftnlen), lnkilb_(integer *, integer *, integer *), dassih_(
	    integer *, char *, ftnlen), errhan_(char *, integer *, ftnlen), 
	    sigerr_(char *, ftnlen), chkout_(char *, ftnlen), lnkini_(integer 
	    *, integer *), setmsg_(char *, ftnlen), errint_(char *, integer *,
	     ftnlen), lnkxsl_(integer *, integer *, integer *), lnkfsl_(
	    integer *, integer *, integer *);
    extern logical return_(void);

/* $ Abstract */

/*     Read and write DAS physical records. */

/* $ Disclaimer */

/*     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE */
/*     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S. */
/*     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE */
/*     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE */
/*     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS" */
/*     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY */
/*     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A */
/*     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC */
/*     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE */
/*     SOFTWARE AND RELATED MATERIALS, HOWEVER USED. */

/*     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA */
/*     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT */
/*     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND, */
/*     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS, */
/*     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE */
/*     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY. */

/*     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF */
/*     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY */
/*     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE */
/*     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE. */

/* $ Required_Reading */

/*     DAS */

/* $ Keywords */

/*     ASSIGNMENT */
/*     DAS */
/*     FILES */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  ENTRY POINTS */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   RRD, RRI, RRC, WRD, WRI, WRC, URD, URI, URC */
/*     RECNO      I   RRD, RRI, RRC, WRD, WRI, WRC, URD, URI, URC */
/*     RECC       I   WRC */
/*     RECD       I   WRD */
/*     RECI       I   WRI */
/*     FIRST      I   RRD, RRI, RRC, URD, URI, URC */
/*     LAST       I   RRD, RRI, RRC, URD, URI, URC */
/*     DATAD      O   RRD, URD */
/*     DATAI      O   RRI, URI */
/*     DATAC      O   RRC, URC */
/*     NWD        P   RRD, WRD, URD */
/*     NWI        P   RRI, WRI, URI */
/*     NWC        P   RRC, WRC, URC */
/*     BUFSZD     P   RRD, WRD, URD */
/*     BUFSZI     P   RRI, WRI, URI */
/*     BUFSZC     P   RRC, WRC, URC */

/* $ Detailed_Input */

/*     See the entry points for a discussion of their inputs. */

/* $ Detailed_Output */

/*     See the entry points for a discussion of their outputs. */

/* $ Parameters */

/*     NWD      is the number of DPs in a single DAS record */
/*              containing DPs. */

/*     NWI      is the number of integers in a single DAS record */
/*              containing integers. */

/*     NWC      is the number of characters in a single DAS record */
/*              containing characters. */

/*     BUFSZD, */
/*     BUFSZI, */
/*     BUFSZC   are, respectively, the number of records in the */
/*              data buffers for double precision, integer, and */
/*              character records. */

/* $ Exceptions */

/*     1)  If this routine is called directly, the error */
/*         SPICE(BOGUSENTRY) is signaled. */

/*     2)  See the entry points for discussions of their exceptions. */

/* $ Files */

/*     See the description of the argument HANDLE in the headers of */
/*     the entry points for a description of files accessed by this */
/*     set of routines. */

/* $ Particulars */

/*     This suite of routines provides buffered read and write access to */
/*     DAS files. The purpose of this feature is to increase the */
/*     performance of application programs that access DAS files: in */
/*     particular, repeated reads from or writes to a given record */
/*     should be relatively fast, because the contents of the most */
/*     recently accessed records are buffered in memory. Thus DASRWR */
/*     and its entry points act as a miniature virtual memory system for */
/*     DAS files. */

/*     These routines are intended primarily for use by other SPICELIB */
/*     routines; users' application programs will not normally need to */
/*     call these routines. Writing to a DAS file with these routines */
/*     demands a particularly circumspect approach: it's quite easy to */
/*     end up with something other than a DAS file if one misuses the */
/*     routines. */

/*     The entry points of DASRWR support writing, reading, and updating */
/*     the records in a DAS file. The distinction between writing and */
/*     updating is that any record may be written (as long as the record */
/*     belongs to a file open for writing), but only existing records */
/*     may be updated.  `Writing' a record sets the values of all of */
/*     the elements of the record, while a subrange of the elements of an */
/*     existing record may be `updated'. */

/*     For each of these three operations, there are three DAS routines, */
/*     one for each supported data type. The names of the routines are */

/*        -- For writing:     DASWRC,  DASWRD,  DASWRI */
/*        -- For updating:    DASURC,  DASURD,  DASURI */
/*        -- For reading:     DASRRC,  DASRRD,  DASRRI */

/*     Users should note that, unlike in the case of SPICELIB's DAF */
/*     routines, the DAS routines buffer data that is written as well */
/*     as data that is read. Consequently a DAS file does not */
/*     necessarily yet contain, at any moment, all of the data that */
/*     has been written to it by the DASWRx or DASURx routines. The */
/*     written data that is buffered is written out when the need */
/*     to buffer additional data requires it, and also when the user */
/*     commands the closure of a file that has been written. So, at */
/*     the time a DAS file is closed, the contents of the physical file */
/*     do reflect what has been `written' to the file by the DASWRx and */
/*     DASURx entry points. */

/*     At any time, an application program can force the DAS system to */
/*     write to a DAS file any buffered records maintained for that */
/*     file. The entry point DASWBR (DAS, write buffered records) */
/*     provides this capability. */

/*     DASRWR contains three record buffers: one of character type, */
/*     one of double precision type, and one of integer type. Each */
/*     buffer has enough room for an integer number of records. The */
/*     sizes of the buffers are parameterized and can be increased if */
/*     necessary. When contemplating the revision of the buffer */
/*     sizes selected by NAIF, SPICELIB users should take note of the */
/*     following points: */

/*        -- Changing values of parameters in NAIF subroutines may cause */
/*           a maintenance burden for the users of the modified NAIF */
/*           code, since any changes made to a SPICELIB routine will have */
/*           to be made to any new version of that routine released by */
/*           NAIF in a later version of SPICELIB. */

/*        -- The effect of buffer size on the speed with which an */
/*           application executes is highly dependent on the specific */
/*           application. In some cases, increasing the buffer sizes */
/*           may slow the application down. */

/* $ Examples */

/*     See the entry points for examples specific to those routines. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.1.0, 07-OCT-2021 (NJB) (JDR) */

/*        Added initializers for record buffers. */

/*        Edited the headers of DASRWR and all its entry points to comply */
/*        with NAIF standard. */

/*        Cleaned up the $Revisions history. */

/* -    SPICELIB Version 2.0.0, 05-FEB-2015 (NJB) */

/*        Upgraded to support handle manager integration and */
/*        reading of non-native files. */

/* -    SPICELIB Version 1.1.1, 10-FEB-2014 (BVS) */

/*        Added description of NWD, NWI, and NWC to the $Parameters */
/*        and $Brief_I/O sections of the header. */

/* -    SPICELIB Version 1.1.0, 17-NOV-1995 (NJB) */

/*        Made modifications to the DASRRx routines to enhance */
/*        efficiency. Removed references to the function RETURN. */

/*        Removed weird spaces from ENTRY statements. */

/* -    SPICELIB Version 1.0.1, 28-OCT-1993 (KRG) */

/*        Removed references to specific DAS file open routines in the */
/*        $Detailed_Input section of the header for each entry point. */
/*        This was done in order to minimize documentation changes if the */
/*        DAS open routines ever change. */

/* -    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT) */

/* -& */
/* $ Index_Entries */

/*     read and write DAS physical records */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 2.0.0, 05-FEB-2015 (NJB) */

/*        Upgraded to support handle manager integration and */
/*        reading of non-native files. */

/* -    SPICELIB Version 1.1.0, 17-NOV-1995 (NJB) */

/*        Made modifications to the DASRRx routines to enhance */
/*        efficiency. Removed references to the function RETURN. */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     The data structure maintained by this set of routines consists */
/*     of three record buffers, one each for use with records of double */
/*     precision, integer, and character data types. */

/*     Each buffer consists of five parallel arrays; the arrays contain: */

/*        -- data records */
/*        -- Fortran record numbers */
/*        -- file handles */
/*        -- Update flags */

/*     In addition, for each buffer there is a doubly linked list that */
/*     points to the buffer and keeps track of the order in which the */
/*     records in the buffer were accessed.  The three linked lists are */
/*     maintained in a doubly linked list pool structure.  The logical */
/*     structure of each buffer is illustrated below.  All of the array */
/*     elements in the same row are associated with the data record in */
/*     that row. */



/*     Linked          Record       Record   Handles   Update */
/*      List           buffer       Numbers            Flags */

/*      +---+      +------------+    +---+    +---+    +---+ */
/*      |   | ---> |            |    |   |    |   |    |   | */
/*      +---+      +------------+    +---+    +---+    +---+ */
/*      |   | ---> |            |    |   |    |   |    |   | */
/*      +---+      +------------+    +---+    +---+    +---+ */
/*        .              .             .        .        . */
/*        .              .             .        .        . */
/*        .              .             .        .        . */
/*      +---+      +------------+    +---+    +---+    +---+ */
/*      |   | ---> |            |    |   |    |   |    |   | */
/*      +---+      +------------+    +---+    +---+    +---+ */



/*     Other local variables */


/*     Saved variables */


/*     Initial values */

    /* Parameter adjustments */
    if (recd) {
	}
    if (reci) {
	}
    if (datad) {
	}
    if (datai) {
	}

    /* Function Body */
    switch(n__) {
	case 1: goto L_dasrrd;
	case 2: goto L_dasrri;
	case 3: goto L_dasrrc;
	case 4: goto L_daswrd;
	case 5: goto L_daswri;
	case 6: goto L_daswrc;
	case 7: goto L_dasurd;
	case 8: goto L_dasuri;
	case 9: goto L_dasurc;
	case 10: goto L_daswbr;
	}


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("DASRWR", (ftnlen)6);
    }

/*     Never come here. */

    sigerr_("SPICE(BOGUSENTRY)", (ftnlen)17);
    chkout_("DASRWR", (ftnlen)6);
    return 0;
/* $Procedure DASRRD ( DAS, read record, double precision ) */

L_dasrrd:
/* $ Abstract */

/*     Read DAS double precision physical records. */

/* $ Disclaimer */

/*     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE */
/*     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S. */
/*     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE */
/*     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE */
/*     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS" */
/*     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY */
/*     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A */
/*     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC */
/*     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE */
/*     SOFTWARE AND RELATED MATERIALS, HOWEVER USED. */

/*     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA */
/*     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT */
/*     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND, */
/*     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS, */
/*     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE */
/*     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY. */

/*     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF */
/*     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY */
/*     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE */
/*     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE. */

/* $ Required_Reading */

/*     DAS */

/* $ Keywords */

/*     ASSIGNMENT */
/*     DAS */
/*     FILES */

/* $ Declarations */

/*     INTEGER               HANDLE */
/*     INTEGER               RECNO */
/*     INTEGER               FIRST */
/*     INTEGER               LAST */
/*     DOUBLE PRECISION      DATAD   ( * ) */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Handle of DAS file. */
/*     RECNO      I   Record number. */
/*     FIRST, */
/*     LAST       I   First and last indices of range within record. */
/*     DATAD      O   Double precision data read from record. */
/*     BUFSZD     P   Number of records in the DP record buffer. */
/*     NWD        P   Number of DP in a single DAS DP record. */

/* $ Detailed_Input */

/*     HANDLE   is the handle of an open DAS file. */

/*     RECNO    is the number of a record in a DAS file. */

/*     FIRST, */
/*     LAST     are the first and last indices of a range of */
/*              double precision numbers to be read from the */
/*              indicated record. The record contains NWD */
/*              double precision numbers; these have indices */
/*              ranging from 1 to NWD. */

/* $ Detailed_Output */

/*     DATAD    is a double precision array containing the */
/*              elements FIRST through LAST of the specified */
/*              record. The record element FIRST is placed */
/*              in DATAD(1), the record element FIRST+1 is placed */
/*              in DATAD(2), and so on; the record element LAST is */
/*              placed in DATAD(LAST-FIRST+1). */

/* $ Parameters */

/*     NWD      is the number of DPs in a single DAS record */
/*              containing DPs. */

/*     BUFSZD   is the number of records in the double precision */
/*              record buffer. */

/* $ Exceptions */

/*     1)  If the input file handle is invalid, an error is signaled by a */
/*         routine in the call tree of this routine. The output argument */
/*         DATAD will not be modified. */

/*     2)  If a read operation attempted by this routine fails, an error */
/*         is signaled by a routine in the call tree of this routine. The */
/*         output argument DATAD will not be modified. */

/*     3)  If a write operation attempted by this routine fails, an error */
/*         is signaled by a routine in the call tree of this routine. The */
/*         output argument DATAD will not be modified. This routine may */
/*         write out updated, buffered records in order to make room in */
/*         the double precision buffer for a newly read record. Note that */
/*         the file written to may be different than the file designated */
/*         by HANDLE if multiple DAS files are open for writing. */

/*     4)  If FIRST or LAST is not in the range [1, NWD], the error */
/*         SPICE(INDEXOUTOFRANGE) is signaled. The output argument */
/*         DATAD will not be modified. */

/*     5)  If FIRST > LAST, this routine will return without modifying */
/*         the output argument DATAD. */

/* $ Files */

/*     See the description of the argument HANDLE in $Detailed_Input. */

/* $ Particulars */

/*     Routines outside of SPICELIB will normally have no need to call */
/*     this routine. */

/*     This routine can be used to read from a DAS file that is open for */
/*     reading or for writing. Any buffered double precision record */
/*     can be read with this routine. In particular, records that have */
/*     been written to the DAS double precision record buffer but have */
/*     not yet been written out to the DAS file they're intended to go */
/*     to ARE visible to this routine. */

/*     This routine should be used to read only records that contain */
/*     double precision data. */

/* $ Examples */

/*     1)  Read the 10th through 100th d.p. numbers from record number 9 */
/*         in a DAS file designated by HANDLE. */

/*             CALL DASRRD ( HANDLE, 9, 10, 100, DATAD ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.1, 22-FEB-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 2.0.0, 05-FEB-2015 (NJB) */

/*        Upgraded to support handle manager integration and */
/*        reading of non-native files. */

/* -    SPICELIB Version 1.1.1, 10-FEB-2014 (BVS) */

/*        Added description of NWD to the $Parameters and $Brief_I/O */
/*        sections of the header. */

/* -    SPICELIB Version 1.1.0, 03-NOV-1995 (NJB) */

/*        Made modifications to enhance efficiency. Removed references */
/*        to the function RETURN. */

/*        Removed weird spaces from ENTRY statement. */

/* -    SPICELIB Version 1.0.1, 28-OCT-1993 (KRG) */

/*        Removed references to specific DAS file open routines in the */
/*        $Detailed_Input section of the header. This was done in order */
/*        to minimize documentation changes if the DAS open routines ever */
/*        change. */

/* -    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT) */

/* -& */
/* $ Index_Entries */

/*     read DAS double precision physical records */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.1.0, 03-NOV-1995 (NJB) */

/*        Made modifications to enhance efficiency. Removed references */
/*        to the function RETURN. For buffered reads, MOVED is not */
/*        called when a single word is to be read. */

/*        Removed weird spaces from ENTRY statement. */

/* -    SPICELIB Version 1.0.1, 28-OCT-1993 (KRG) */

/*        Removed references to specific DAS file open routines in the */
/*        $Detailed_Input section of the header. This was done in order */
/*        to minimize documentation changes if the DAS open routines ever */
/*        change. */

/* -    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT) */

/* -& */

/*     If it hasn't been done yet, initialize the pointer list pools. */

    if (pass1) {
	lnkini_(&c__10, poold);
	lnkini_(&c__10, pooli);
	lnkini_(&c__10, poolc);
	pass1 = FALSE_;
    }

/*     Check FIRST and LAST.  Use discovery check-in. */

    if (*first < 1 || *first > 128 || *last < 1 || *last > 128) {
	chkin_("DASRRD", (ftnlen)6);
	setmsg_("Array indices FIRST and LAST were #,  #; allowed range for "
		"both is [#, #]. File was #, record number was #.", (ftnlen)
		107);
	errint_("#", first, (ftnlen)1);
	errint_("#", last, (ftnlen)1);
	errint_("#", &c__1, (ftnlen)1);
	errint_("#", &c__128, (ftnlen)1);
	errhan_("#", handle, (ftnlen)1);
	errint_("#", recno, (ftnlen)1);
	sigerr_("SPICE(INDEXOUTOFRANGE)", (ftnlen)22);
	chkout_("DASRRD", (ftnlen)6);
	return 0;
    }

/*     There's nothing to do if LAST < FIRST.  (We're not checked in at */
/*     this point.) */

    if (*last < *first) {
	return 0;
    }

/*     See whether record number RECNO in file HANDLE is buffered.  We'll */
/*     search through the list of buffered records starting at the head */
/*     of the list.  If we find the desired record, transfer the */
/*     requested data to the array DATAD and return without further ado. */

    node = headd;
    while(node > 0) {
	if (*handle == hnbufd[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : 
		s_rnge("hnbufd", i__1, "dasrwr_", (ftnlen)737)] && *recno == 
		rnbufd[(i__2 = node - 1) < 10 && 0 <= i__2 ? i__2 : s_rnge(
		"rnbufd", i__2, "dasrwr_", (ftnlen)737)]) {

/*           Found it.  Move this record to the head of the list. */
/*           Update our head pointer as required. */

	    if (node != headd) {
		lnkxsl_(&node, &node, poold);
		lnkilb_(&node, &headd, poold);
		headd = node;
	    }

/*           Don't forget to return the requested data. */

	    if (*first == *last) {
		datad[0] = rcbufd[(i__1 = *first + (node << 7) - 129) < 1280 
			&& 0 <= i__1 ? i__1 : s_rnge("rcbufd", i__1, "dasrwr_"
			, (ftnlen)757)];
	    } else {
		i__2 = *last - *first + 1;
		moved_(&rcbufd[(i__1 = *first + (node << 7) - 129) < 1280 && 
			0 <= i__1 ? i__1 : s_rnge("rcbufd", i__1, "dasrwr_", (
			ftnlen)761)], &i__2, datad);
	    }

/*           We haven't checked in, so don't check out. */

	    return 0;
	}
	node = poold[(i__1 = (node << 1) + 10) < 32 && 0 <= i__1 ? i__1 : 
		s_rnge("poold", i__1, "dasrwr_", (ftnlen)772)];
    }

/*     The record wasn't buffered.  We need to allocate entries to */
/*     hold the record contents.  If the buffer isn't full, just */
/*     select a free set of entries.  If the buffer is full, use */
/*     the set of entries at the tail of the list. */

/*     Since we're now going to do a file read, it doesn't slow */
/*     us down much to check in, comparatively speaking. */

    chkin_("DASRRD", (ftnlen)6);
    if (usedd == 10) {

/*        Grab the buffer entry at the tail end of the list. */

	node = lnktl_(&headd, poold);
	lnkxsl_(&node, &node, poold);

/*        If the allocated buffer entry was updated, write it out. */

	if (upbufd[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("upbu"
		"fd", i__1, "dasrwr_", (ftnlen)798)]) {

/*           We'll need a logical unit in order to write to the file. */

	    zzddhhlu_(&hnbufd[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : 
		    s_rnge("hnbufd", i__1, "dasrwr_", (ftnlen)802)], "DAS", &
		    c_false, &wrunit, (ftnlen)3);
	    if (failed_()) {
		chkout_("DASRRD", (ftnlen)6);
		return 0;
	    }
	    dasiod_("WRITE", &wrunit, &rnbufd[(i__1 = node - 1) < 10 && 0 <= 
		    i__1 ? i__1 : s_rnge("rnbufd", i__1, "dasrwr_", (ftnlen)
		    809)], &rcbufd[(i__2 = (node << 7) - 128) < 1280 && 0 <= 
		    i__2 ? i__2 : s_rnge("rcbufd", i__2, "dasrwr_", (ftnlen)
		    809)], (ftnlen)5);
	    if (failed_()) {
		chkout_("DASRRD", (ftnlen)6);
		return 0;
	    }
	}
    } else {

/*        Allocate a new set of buffer entries, but don't link */
/*        them into the list yet. */

	lnkan_(poold, &node);
	++usedd;
    }

/*     Try to read the record. */

    zzdasgrd_(handle, recno, &rcbufd[(i__1 = (node << 7) - 128) < 1280 && 0 <=
	     i__1 ? i__1 : s_rnge("rcbufd", i__1, "dasrwr_", (ftnlen)835)]);
    if (failed_()) {
	chkout_("DASRRD", (ftnlen)6);
	return 0;
    }

/*     The read was successful.  Link the node pointing to the buffer */
/*     entries for this record in before the current head of the */
/*     list, thus putting them at the head. */

/*     Set the file handle, record number, and update flag for */
/*     this record. */

    lnkilb_(&node, &headd, poold);
    hnbufd[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("hnbufd", i__1,
	     "dasrwr_", (ftnlen)852)] = *handle;
    rnbufd[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("rnbufd", i__1,
	     "dasrwr_", (ftnlen)853)] = *recno;
    upbufd[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("upbufd", i__1,
	     "dasrwr_", (ftnlen)854)] = FALSE_;
    headd = node;

/*     Don't forget to return the requested data. */

    i__2 = *last - *first + 1;
    moved_(&rcbufd[(i__1 = *first + (node << 7) - 129) < 1280 && 0 <= i__1 ? 
	    i__1 : s_rnge("rcbufd", i__1, "dasrwr_", (ftnlen)860)], &i__2, 
	    datad);
    chkout_("DASRRD", (ftnlen)6);
    return 0;
/* $Procedure DASRRI ( DAS, read record, integer ) */

L_dasrri:
/* $ Abstract */

/*     Read DAS integer physical records. */

/* $ Disclaimer */

/*     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE */
/*     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S. */
/*     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE */
/*     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE */
/*     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS" */
/*     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY */
/*     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A */
/*     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC */
/*     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE */
/*     SOFTWARE AND RELATED MATERIALS, HOWEVER USED. */

/*     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA */
/*     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT */
/*     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND, */
/*     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS, */
/*     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE */
/*     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY. */

/*     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF */
/*     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY */
/*     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE */
/*     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE. */

/* $ Required_Reading */

/*     DAS */

/* $ Keywords */

/*     ASSIGNMENT */
/*     DAS */
/*     FILES */

/* $ Declarations */

/*     INTEGER               HANDLE */
/*     INTEGER               RECNO */
/*     INTEGER               FIRST */
/*     INTEGER               LAST */
/*     INTEGER               DATAI   ( * ) */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Handle of DAS file. */
/*     RECNO      I   Record number. */
/*     FIRST, */
/*     LAST       I   First and last indices of range within record. */
/*     DATAI      O   Integer data read from record. */
/*     BUFSZI     P   Number of records in the integer record buffer. */
/*     NWI        P   Number of integers in a single DAS integer record. */

/* $ Detailed_Input */

/*     HANDLE   is the handle of an open DAS file. */

/*     RECNO    is the number of a record in a DAS file. */

/*     FIRST, */
/*     LAST     are the first and last indices of a range of */
/*              integers to be read from the indicated record. */
/*              The record contains NWI integers; these have */
/*              indices ranging from 1 to NWI. */

/* $ Detailed_Output */

/*     DATAI    is an integer array containing the elements FIRST */
/*              through LAST of the specified record. The record */
/*              element FIRST is placed in DATAI(1), the record */
/*              element FIRST+1 is placed in DATAI(2), and so on; */
/*              the record element LAST is placed in */
/*              DATAI(LAST-FIRST+1). */

/* $ Parameters */

/*     NWI      is the number of integers in a single DAS record */
/*              containing integers. */

/*     BUFSZI   is the number of records in the integer record */
/*              buffer. */

/* $ Exceptions */

/*     1)  If the input file handle is invalid, an error is signaled */
/*         by a routine in the call tree of this routine. The */
/*         output argument DATAI will not be modified. */

/*     2)  If a read operation attempted by this routine fails, an error */
/*         is signaled by a routine in the call tree of this routine. */
/*         The output argument DATAI will not be modified. */

/*     3)  If a write operation attempted by this routine fails, an */
/*         error is signaled by a routine in the call tree of this */
/*         routine. The output argument DATAI will not be modified. This */
/*         routine may write out updated, buffered records in order to */
/*         make room in the integer buffer for a newly read record. Note */
/*         that the file written to may be different than the file */
/*         designated by HANDLE if multiple DAS files are open for */
/*         writing. */

/*     4)  If FIRST or LAST is not in the range [1, NWI], the error */
/*         SPICE(INDEXOUTOFRANGE) is signaled. The output argument */
/*         DATAI will not be modified. */

/*     5)  If FIRST > LAST, this routine will return without modifying */
/*         the output argument DATAI. */

/* $ Files */

/*     See the description of the argument HANDLE in $Detailed_Input. */

/* $ Particulars */

/*     Routines outside of SPICELIB will normally have no need to call */
/*     this routine. */

/*     This routine can be used to read from a DAS file that is open for */
/*     reading or writing. Any buffered integer record can be read with */
/*     this routine. In particular, records that have been written to */
/*     the DAS integer record buffer but have not yet been written out */
/*     to the DAS file they're intended to go to ARE visible to this */
/*     routine. */

/*     This routine should be used to read only records that contain */
/*     integer data. */

/* $ Examples */

/*     1)  Read the 10th through 100th integers from record number 9 */
/*         in a DAS file designated by HANDLE. */

/*             CALL DASRRI ( HANDLE, 9, 10, 100, DATAI ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.1, 22-FEB-2021 (JDR) */

/*        Updated the header to comply with NAIF standard. Cleaned up */
/*        the $Revisions history. */

/* -    SPICELIB Version 2.0.0, 05-FEB-2015 (NJB) */

/*        Upgraded to support handle manager integration and */
/*        reading of non-native files. */

/* -    SPICELIB Version 1.1.1, 10-FEB-2014 (BVS) */

/*        Added description of NWI to the $Parameters and $Brief_I/O */
/*        sections of the header. */

/* -    SPICELIB Version 1.1.0, 03-NOV-1995 (NJB) */

/*        Made modifications to enhance efficiency. Removed references */
/*        to the function RETURN. */

/*        Removed weird spaces from ENTRY statement. */

/* -    SPICELIB Version 1.0.1, 28-OCT-1993 (KRG) */

/*        Removed references to specific DAS file open routines in the */
/*        $Detailed_Input section of the header. This was done in order */
/*        to minimize documentation changes if the DAS open routines ever */
/*        change. */

/* -    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT) */

/* -& */
/* $ Index_Entries */

/*     read DAS integer physical records */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 2.0.0, 05-FEB-2015 (NJB) */

/*        Upgraded to support handle manager integration and */
/*        reading of non-native files. */

/* -    SPICELIB Version 1.1.0, 03-NOV-1995 (NJB) */

/*        Made modifications to enhance efficiency. Removed references */
/*        to the function RETURN. For buffered reads, MOVEI is not */
/*        called when a single word is to be read. */

/* -& */

/*     Non-standard SPICE error handling. */

/*     If it hasn't been done yet, initialize the pointer list pools. */

    if (pass1) {
	lnkini_(&c__10, poold);
	lnkini_(&c__10, pooli);
	lnkini_(&c__10, poolc);
	pass1 = FALSE_;
    }

/*     Check FIRST and LAST.  Use discovery check-in. */

    if (*first < 1 || *first > 256 || *last < 1 || *last > 256) {
	chkin_("DASRRI", (ftnlen)6);
	setmsg_("Array indices FIRST and LAST were #,  #; allowed range for "
		"both is [#, #]. File was #, record number was #.", (ftnlen)
		107);
	errint_("#", first, (ftnlen)1);
	errint_("#", last, (ftnlen)1);
	errint_("#", &c__1, (ftnlen)1);
	errint_("#", &c__256, (ftnlen)1);
	errhan_("#", handle, (ftnlen)1);
	errint_("#", recno, (ftnlen)1);
	sigerr_("SPICE(INDEXOUTOFRANGE)", (ftnlen)22);
	chkout_("DASRRI", (ftnlen)6);
	return 0;
    }

/*     There's nothing to do if LAST < FIRST.  (We're not checked in at */
/*     this point.) */

    if (*last < *first) {
	return 0;
    }

/*     See whether record number RECNO in file HANDLE is buffered.  We'll */
/*     search through the list of buffered records starting at the head */
/*     of the list.  If we find the desired record, transfer the */
/*     requested data to the array DATAI and return without further ado. */

    node = headi;
    while(node > 0) {
	if (*handle == hnbufi[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : 
		s_rnge("hnbufi", i__1, "dasrwr_", (ftnlen)1149)] && *recno == 
		rnbufi[(i__2 = node - 1) < 10 && 0 <= i__2 ? i__2 : s_rnge(
		"rnbufi", i__2, "dasrwr_", (ftnlen)1149)]) {


/*           Found it.  Move this record to the head of the list. */
/*           Update our head pointer as required. */

	    if (node != headi) {
		lnkxsl_(&node, &node, pooli);
		lnkilb_(&node, &headi, pooli);
		headi = node;
	    }

/*           Don't forget to return the requested data. */

	    if (*first == *last) {
		datai[0] = rcbufi[(i__1 = *first + (node << 8) - 257) < 2560 
			&& 0 <= i__1 ? i__1 : s_rnge("rcbufi", i__1, "dasrwr_"
			, (ftnlen)1170)];
	    } else {
		i__2 = *last - *first + 1;
		movei_(&rcbufi[(i__1 = *first + (node << 8) - 257) < 2560 && 
			0 <= i__1 ? i__1 : s_rnge("rcbufi", i__1, "dasrwr_", (
			ftnlen)1174)], &i__2, datai);
	    }

/*           We haven't checked in, so don't check out. */

	    return 0;
	}
	node = pooli[(i__1 = (node << 1) + 10) < 32 && 0 <= i__1 ? i__1 : 
		s_rnge("pooli", i__1, "dasrwr_", (ftnlen)1185)];
    }

/*     The record wasn't buffered.  We need to allocate entries to */
/*     hold the record contents.  If the buffer isn't full, just */
/*     select a free set of entries.  If the buffer is full, use */
/*     the set of entries at the tail of the list. */

/*     Since we're now going to do a file read, it doesn't slow */
/*     us down much to check in, comparatively speaking. */

    chkin_("DASRRI", (ftnlen)6);
    if (usedi == 10) {

/*        Grab the buffer entry at the tail end of the list. */

	node = lnktl_(&headi, pooli);
	lnkxsl_(&node, &node, pooli);

/*        If the allocated buffer entry was updated, write it out. */

	if (upbufi[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("upbu"
		"fi", i__1, "dasrwr_", (ftnlen)1211)]) {
	    zzddhhlu_(&hnbufi[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : 
		    s_rnge("hnbufi", i__1, "dasrwr_", (ftnlen)1213)], "DAS", &
		    c_false, &wrunit, (ftnlen)3);
	    dasioi_("WRITE", &wrunit, &rnbufi[(i__1 = node - 1) < 10 && 0 <= 
		    i__1 ? i__1 : s_rnge("rnbufi", i__1, "dasrwr_", (ftnlen)
		    1215)], &rcbufi[(i__2 = (node << 8) - 256) < 2560 && 0 <= 
		    i__2 ? i__2 : s_rnge("rcbufi", i__2, "dasrwr_", (ftnlen)
		    1215)], (ftnlen)5);
	    if (failed_()) {
		chkout_("DASRRI", (ftnlen)6);
		return 0;
	    }
	}
    } else {

/*        Allocate a new set of buffer entries, but don't link */
/*        them into the list yet. */

	lnkan_(pooli, &node);
	++usedi;
    }

/*     Try to read the record. */

    zzdasgri_(handle, recno, &rcbufi[(i__1 = (node << 8) - 256) < 2560 && 0 <=
	     i__1 ? i__1 : s_rnge("rcbufi", i__1, "dasrwr_", (ftnlen)1241)]);
    if (failed_()) {
	chkout_("DASRRI", (ftnlen)6);
	return 0;
    }

/*     The read was successful.  Link the node pointing to the buffer */
/*     entries for this record in before the current head of the */
/*     list, thus putting them at the head. */

/*     Set the file handle, record number, and update flag for */
/*     this record. */

    lnkilb_(&node, &headi, pooli);
    hnbufi[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("hnbufi", i__1,
	     "dasrwr_", (ftnlen)1258)] = *handle;
    rnbufi[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("rnbufi", i__1,
	     "dasrwr_", (ftnlen)1259)] = *recno;
    upbufi[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("upbufi", i__1,
	     "dasrwr_", (ftnlen)1260)] = FALSE_;
    headi = node;

/*     Don't forget to return the requested data. */

    i__2 = *last - *first + 1;
    movei_(&rcbufi[(i__1 = *first + (node << 8) - 257) < 2560 && 0 <= i__1 ? 
	    i__1 : s_rnge("rcbufi", i__1, "dasrwr_", (ftnlen)1266)], &i__2, 
	    datai);
    chkout_("DASRRI", (ftnlen)6);
    return 0;
/* $Procedure DASRRC ( DAS, read record, character ) */

L_dasrrc:
/* $ Abstract */

/*     Read DAS character physical records. */

/* $ Disclaimer */

/*     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE */
/*     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S. */
/*     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE */
/*     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE */
/*     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS" */
/*     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY */
/*     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A */
/*     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC */
/*     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE */
/*     SOFTWARE AND RELATED MATERIALS, HOWEVER USED. */

/*     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA */
/*     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT */
/*     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND, */
/*     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS, */
/*     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE */
/*     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY. */

/*     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF */
/*     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY */
/*     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE */
/*     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE. */

/* $ Required_Reading */

/*     DAS */

/* $ Keywords */

/*     ASSIGNMENT */
/*     DAS */
/*     FILES */

/* $ Declarations */

/*     INTEGER               HANDLE */
/*     INTEGER               RECNO */
/*     INTEGER               FIRST */
/*     INTEGER               LAST */
/*     CHARACTER*(*)         DATAC */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Handle of DAS file. */
/*     RECNO      I   Record number. */
/*     FIRST, */
/*     LAST       I   First and last indices of range within record. */
/*     DATAC      O   Character data read from record. */
/*     BUFSZC     P   Number of records in the character record buffer. */
/*     NWC        P   Number of characters in a single DAS char. record. */

/* $ Detailed_Input */

/*     HANDLE   is the handle of an open DAS file. */

/*     RECNO    is the number of a record in a DAS file. */

/*     FIRST, */
/*     LAST     are the first and last indices of a range of */
/*              characters to be read from the indicated record. */
/*              The record contains NWC characters; these have */
/*              indices ranging from 1 to NWC. */

/* $ Detailed_Output */

/*     DATAC    is a character string containing the elements */
/*              FIRST through LAST of the specified record. The */
/*              record element FIRST is placed in DATAC(1:1), the */
/*              record element FIRST+1 is placed in DATAC(2:2), */
/*              and so on; the record element LAST is placed in */
/*              DATAC( LAST-FIRST+1 : LAST-FIRST+1 ). */

/* $ Parameters */

/*     NWC      is the number of characters in a single DAS record */
/*              containing characters. */

/*     BUFSZC   is the number of records in the character record */
/*              buffer. */

/* $ Exceptions */

/*     1)  If the input file handle is invalid, an error is */
/*         signaled by a routine in the call tree of this routine. The */
/*         output argument DATAC will not be modified. */

/*     2)  If a read operation attempted by this routine fails, an error */
/*         is signaled by a routine in the call tree of this routine. */
/*         The output argument DATAC will not be modified. */

/*     3)  If a write operation attempted by this routine fails, an */
/*         error is signaled by a routine in the call tree of this */
/*         routine. The output argument DATAC will not be modified. This */
/*         routine may write out updated, buffered records in order to */
/*         make room in the character buffer for a newly read record. */
/*         Note that the file written to may be different than the file */
/*         designated by HANDLE if multiple DAS files are open for */
/*         writing. */

/*     4)  If FIRST or LAST is not in the range [1, NWC], the error */
/*         SPICE(INDEXOUTOFRANGE) is signaled. The output argument */
/*         DATAC will not be modified. */

/*     5)  If FIRST > LAST, this routine will return without modifying */
/*         the output argument DATAC. */

/* $ Files */

/*     See the description of the argument HANDLE in $Detailed_Input. */

/* $ Particulars */

/*     Routines outside of SPICELIB will normally have no need to call */
/*     this routine. */

/*     This routine can be used to read from a DAS file that is open for */
/*     reading or writing. Any buffered character record can be read */
/*     with this routine. In particular, records that have been */
/*     written to the DAS character record buffer but have not yet been */
/*     written out to the DAS file they're intended to go to ARE */
/*     visible to this routine. */

/*     This routine should be used to read only records that contain */
/*     character data. */

/* $ Examples */

/*     1)  Read the 10th through 100th characters from record number 9 */
/*         in a DAS file designated by HANDLE. */

/*             CALL DASRRC ( HANDLE, 9, 10, 100, DATAC ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.1, 22-FEB-2021 (JDR) */

/*        Updated the header to comply with NAIF standard. Cleaned up */
/*        the $Revisions history. */

/* -    SPICELIB Version 2.0.0, 05-FEB-2015 (NJB) */

/*        Upgraded to support handle manager integration. */

/* -    SPICELIB Version 1.1.1, 10-FEB-2014 (BVS) */

/*        Added description of NWC to the $Parameters and $Brief_I/O */
/*        sections of the header. */

/* -    SPICELIB Version 1.1.0, 09-NOV-1995 (NJB) */

/*        Made modifications to enhance efficiency. Removed references */
/*        to the function RETURN. */

/*        Removed weird spaces from ENTRY statement. */

/* -    SPICELIB Version 1.0.1, 28-OCT-1993 (KRG) */

/*        Removed references to specific DAS file open routines in the */
/*        $Detailed_Input section of the header. This was done in order */
/*        to minimize documentation changes if the DAS open routines ever */
/*        change. */

/* -    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT) */

/* -& */
/* $ Index_Entries */

/*     read DAS character physical records */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 2.0.0, 05-FEB-2015 (NJB) */

/*        Upgraded to support handle manager integration. */

/* -    SPICELIB Version 1.1.0, 09-NOV-1995 (NJB) */

/*        Made modifications to enhance efficiency. Removed references */
/*        to the function RETURN. */

/* -& */

/*     If it hasn't been done yet, initialize the pointer list pools. */

    if (pass1) {
	lnkini_(&c__10, poold);
	lnkini_(&c__10, pooli);
	lnkini_(&c__10, poolc);
	pass1 = FALSE_;
    }

/*     Check FIRST and LAST.  Use discovery check-in. */

    if (*first < 1 || *first > 1024 || *last < 1 || *last > 1024) {
	chkin_("DASRRC", (ftnlen)6);
	setmsg_("Array indices FIRST and LAST were #,  #; allowed range for "
		"both is [#, #]. File was #, record number was #.", (ftnlen)
		107);
	errint_("#", first, (ftnlen)1);
	errint_("#", last, (ftnlen)1);
	errint_("#", &c__1, (ftnlen)1);
	errint_("#", &c__1024, (ftnlen)1);
	errhan_("#", handle, (ftnlen)1);
	errint_("#", recno, (ftnlen)1);
	sigerr_("SPICE(INDEXOUTOFRANGE)", (ftnlen)22);
	chkout_("DASRRC", (ftnlen)6);
	return 0;
    }

/*     There's nothing to do if LAST < FIRST.  (We're not checked in at */
/*     this point.) */

    if (*last < *first) {
	return 0;
    }

/*     See whether record number RECNO in file HANDLE is buffered.  We'll */
/*     search through the list of buffered records starting at the head */
/*     of the list.  If we find the desired record, transfer the */
/*     requested data to the array DATAC and return without further ado. */

    node = headc;
    while(node > 0) {
	if (*handle == hnbufc[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : 
		s_rnge("hnbufc", i__1, "dasrwr_", (ftnlen)1551)] && *recno == 
		rnbufc[(i__2 = node - 1) < 10 && 0 <= i__2 ? i__2 : s_rnge(
		"rnbufc", i__2, "dasrwr_", (ftnlen)1551)]) {


/*           Found it.  Move this record to the head of the list. */
/*           Update our head pointer as required. */

	    if (node != headc) {
		lnkxsl_(&node, &node, poolc);
		lnkilb_(&node, &headc, poolc);
		headc = node;
	    }

/*           Don't forget to return the requested data. */

	    s_copy(datac, rcbufc + ((((i__1 = node - 1) < 10 && 0 <= i__1 ? 
		    i__1 : s_rnge("rcbufc", i__1, "dasrwr_", (ftnlen)1570)) <<
		     10) + (*first - 1)), datac_len, *last - (*first - 1));

/*           We haven't checked in, so don't check out. */

	    return 0;
	}
	node = poolc[(i__1 = (node << 1) + 10) < 32 && 0 <= i__1 ? i__1 : 
		s_rnge("poolc", i__1, "dasrwr_", (ftnlen)1579)];
    }

/*     The record wasn't buffered.  We need to allocate entries to */
/*     hold the record contents.  If the buffer isn't full, just */
/*     select a free set of entries.  If the buffer is full, use */
/*     the set of entries at the tail of the list. */

/*     Since we're now going to do a file read, it doesn't slow */
/*     us down much to check in, comparatively speaking. */

    chkin_("DASRRC", (ftnlen)6);
    if (usedc == 10) {

/*        Grab the buffer entry at the tail end of the list. */

	node = lnktl_(&headc, poolc);
	lnkxsl_(&node, &node, poolc);

/*        If the allocated buffer entry was updated, write it out. */

	if (upbufc[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("upbu"
		"fc", i__1, "dasrwr_", (ftnlen)1605)]) {
	    zzddhhlu_(&hnbufc[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : 
		    s_rnge("hnbufc", i__1, "dasrwr_", (ftnlen)1607)], "DAS", &
		    c_false, &wrunit, (ftnlen)3);
	    dasioc_("WRITE", &wrunit, &rnbufc[(i__1 = node - 1) < 10 && 0 <= 
		    i__1 ? i__1 : s_rnge("rnbufc", i__1, "dasrwr_", (ftnlen)
		    1609)], rcbufc + (((i__2 = node - 1) < 10 && 0 <= i__2 ? 
		    i__2 : s_rnge("rcbufc", i__2, "dasrwr_", (ftnlen)1609)) <<
		     10), (ftnlen)5, (ftnlen)1024);
	    if (failed_()) {
		chkout_("DASRRC", (ftnlen)6);
		return 0;
	    }
	}
    } else {

/*        Allocate a new set of buffer entries, but don't link */
/*        them into the list yet. */

	lnkan_(poolc, &node);
	++usedc;
    }

/*     Try to read the record. */

    zzddhhlu_(handle, "DAS", &c_false, &unit, (ftnlen)3);
    dasioc_("READ", &unit, recno, rcbufc + (((i__1 = node - 1) < 10 && 0 <= 
	    i__1 ? i__1 : s_rnge("rcbufc", i__1, "dasrwr_", (ftnlen)1637)) << 
	    10), (ftnlen)4, (ftnlen)1024);
    if (failed_()) {
	chkout_("DASRRC", (ftnlen)6);
	return 0;
    }

/*     The read was successful.  Link the node pointing to the buffer */
/*     entries for this record in before the current head of the */
/*     list, thus putting them at the head. */

/*     Set the file handle, record number, and update flag for */
/*     this record. */

    lnkilb_(&node, &headc, poolc);
    hnbufc[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("hnbufc", i__1,
	     "dasrwr_", (ftnlen)1654)] = *handle;
    rnbufc[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("rnbufc", i__1,
	     "dasrwr_", (ftnlen)1655)] = *recno;
    upbufc[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("upbufc", i__1,
	     "dasrwr_", (ftnlen)1656)] = FALSE_;
    headc = node;

/*     Don't forget to return the requested data. */

    s_copy(datac, rcbufc + ((((i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : 
	    s_rnge("rcbufc", i__1, "dasrwr_", (ftnlen)1662)) << 10) + (*first 
	    - 1)), datac_len, *last - (*first - 1));
    chkout_("DASRRC", (ftnlen)6);
    return 0;
/* $Procedure DASWRD ( DAS, write record, double precision ) */

L_daswrd:
/* $ Abstract */

/*     Write DAS double precision physical records. */

/* $ Disclaimer */

/*     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE */
/*     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S. */
/*     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE */
/*     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE */
/*     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS" */
/*     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY */
/*     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A */
/*     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC */
/*     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE */
/*     SOFTWARE AND RELATED MATERIALS, HOWEVER USED. */

/*     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA */
/*     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT */
/*     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND, */
/*     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS, */
/*     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE */
/*     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY. */

/*     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF */
/*     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY */
/*     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE */
/*     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE. */

/* $ Required_Reading */

/*     DAS */

/* $ Keywords */

/*     ASSIGNMENT */
/*     DAS */
/*     FILES */

/* $ Declarations */

/*     INTEGER               HANDLE */
/*     INTEGER               RECNO */
/*     DOUBLE PRECISION      RECD   ( NWD ) */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Handle of DAS file. */
/*     RECNO      I   Record number. */
/*     RECD       I   Double precision data to be written to record. */
/*     BUFSZD     P   Number of records in the DP record buffer. */
/*     NWD        P   Number of DP in a single DAS DP record. */

/* $ Detailed_Input */

/*     HANDLE   is the handle of a DAS file opened for writing. */

/*     RECNO    is the number of a record in a DAS file. */

/*     RECD     is an array of NWD double precision numbers. The */
/*              contents of this array are to be written to the */
/*              physical file record having number RECNO. */

/* $ Detailed_Output */

/*     None. See $Particulars for a description of the action of this */
/*     routine. */

/* $ Parameters */

/*     NWD      is the number of DPs in a single DAS record */
/*              containing DPs. */

/*     BUFSZD   is the number of records in the double precision */
/*              record buffer. */

/* $ Exceptions */

/*     1)  If the input file handle is invalid, an error is signaled by a */
/*         routine in the call tree of this routine. The DAS file */
/*         designated by HANDLE will not be modified. */

/*     2)  If a write operation attempted by this routine fails, an error */
/*         is signaled by a routine in the call tree of this routine. The */
/*         status of the DAS file written to is uncertain in this case. */
/*         Note that the file written to may be different than the file */
/*         designated by HANDLE if multiple DAS files are open for */
/*         writing. */

/* $ Files */

/*     See the description of the argument HANDLE in $Detailed_Input. */

/* $ Particulars */

/*     Routines outside of SPICELIB will normally have no need to call */
/*     this routine. */

/*     This routine can be used to write to only DAS files that are open */
/*     for writing. Records written via this routine will always be */
/*     buffered immediately, but may not be written to the file until */
/*     they are cleared from the double precision buffer to make room */
/*     for other records, or until they are explicitly forced to to be */
/*     written via a call to DASWBR. In any case, at the moment this */
/*     routine returns, the data supplied on input may be read back by */
/*     DASRRD or updated by DASURD. */

/*     Closing a DAS file via DASCLS forces any remaining updated data */
/*     records buffered by this routine to be written to the file. */

/* $ Examples */

/*     1)  Write an array of NWD double precision numbers to the 9th */
/*         record in a DAS file designated by HANDLE. */

/*            DOUBLE PRECISION        RECD */

/*                         . */
/*                         . */
/*                         . */

/*            DO I = 1, NWD */
/*               RECD(I) = DBLE(I) */
/*            END DO */

/*            CALL DASWRD ( HANDLE, 9, RECD ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.1, 22-FEB-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 2.0.0, 30-JUL-2014 (NJB) */

/*        Upgraded to support handle manager integration. */

/* -    SPICELIB Version 1.0.3, 10-FEB-2014 (BVS) */

/*        Added description of NWD to the $Parameters and $Brief_I/O */
/*        sections of the header. */

/* -    SPICELIB Version 1.0.2, 03-NOV-1995 (NJB) */

/*        Removed weird spaces from ENTRY statement. */

/* -    SPICELIB Version 1.0.1, 28-OCT-1993 (KRG) */

/*        Removed references to specific DAS file open routines in the */
/*        $Detailed_Input section of the header. This was done in order */
/*        to minimize documentation changes if the DAS open routines ever */
/*        change. */

/* -    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT) */

/* -& */
/* $ Index_Entries */

/*     write DAS double precision physical records */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.0.1, 28-OCT-1993 (KRG) */

/*        Removed references to specific DAS file open routines in the */
/*        $Detailed_Input section of the header. This was done in order */
/*        to minimize documentation changes if the DAS open routines ever */
/*        change. */

/* -    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT) */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("DASWRD", (ftnlen)6);

/*     Check that the file is open for writing.  Signal an error if not. */

    dassih_(handle, "WRITE", (ftnlen)5);
    if (failed_()) {
	chkout_("DASWRD", (ftnlen)6);
	return 0;
    }

/*     If it hasn't been done yet, initialize the pointer list pools. */

    if (pass1) {
	lnkini_(&c__10, poold);
	lnkini_(&c__10, pooli);
	lnkini_(&c__10, poolc);
	pass1 = FALSE_;
    }

/*     See whether double precision record number RECNO from file HANDLE */
/*     is buffered.  We'll search through the list of buffered records */
/*     starting at the head of the list.  If the record is already */
/*     buffered, we'll update the buffer entry, but we'll defer writing */
/*     the record out until we need to free a record, or until the */
/*     d.p. buffer is flushed, whichever comes first. */

    node = headd;
    while(node > 0) {
	if (*handle == hnbufd[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : 
		s_rnge("hnbufd", i__1, "dasrwr_", (ftnlen)1917)] && *recno == 
		rnbufd[(i__2 = node - 1) < 10 && 0 <= i__2 ? i__2 : s_rnge(
		"rnbufd", i__2, "dasrwr_", (ftnlen)1917)]) {

/*           Found it.  Update the buffered record. */

	    moved_(recd, &c__128, &rcbufd[(i__1 = (node << 7) - 128) < 1280 &&
		     0 <= i__1 ? i__1 : s_rnge("rcbufd", i__1, "dasrwr_", (
		    ftnlen)1922)]);

/*           Set the update flag, indicating that this buffer entry */
/*           has been modified. */

	    upbufd[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("upbu"
		    "fd", i__1, "dasrwr_", (ftnlen)1928)] = TRUE_;

/*           Put the information about this record at the head of the */
/*           active list, if it is not already there. */

	    if (node != headd) {
		lnkxsl_(&node, &node, poold);
		lnkilb_(&node, &headd, poold);
		headd = node;
	    }
	    chkout_("DASWRD", (ftnlen)6);
	    return 0;
	}
	node = poold[(i__1 = (node << 1) + 10) < 32 && 0 <= i__1 ? i__1 : 
		s_rnge("poold", i__1, "dasrwr_", (ftnlen)1947)];
    }

/*     The record we're writing to is not buffered.  We'll allocate */
/*     a buffer entry.  If the record buffer is full, we'll */
/*     commandeer the least recently accessed record.  Before using */
/*     this record, we'll write its contents out to the corresponding */
/*     file, if the record has been updated. */

    if (usedd < 10) {

/*        There's a free buffer entry available.  Just allocate it. */

	lnkan_(poold, &node);
	++usedd;
    } else {

/*        Grab the buffer entry at the tail end of the list. */

	node = lnktl_(&headd, poold);
	lnkxsl_(&node, &node, poold);

/*        If the allocated record was updated, write it out. */

	if (upbufd[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("upbu"
		"fd", i__1, "dasrwr_", (ftnlen)1977)]) {
	    zzddhhlu_(&hnbufd[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : 
		    s_rnge("hnbufd", i__1, "dasrwr_", (ftnlen)1979)], "DAS", &
		    c_false, &wrunit, (ftnlen)3);
	    dasiod_("WRITE", &wrunit, &rnbufd[(i__1 = node - 1) < 10 && 0 <= 
		    i__1 ? i__1 : s_rnge("rnbufd", i__1, "dasrwr_", (ftnlen)
		    1981)], &rcbufd[(i__2 = (node << 7) - 128) < 1280 && 0 <= 
		    i__2 ? i__2 : s_rnge("rcbufd", i__2, "dasrwr_", (ftnlen)
		    1981)], (ftnlen)5);
	    if (failed_()) {
		chkout_("DASWRD", (ftnlen)6);
		return 0;
	    }
	}
    }

/*     Now update the allocated buffer entry with the input data. */

    moved_(recd, &c__128, &rcbufd[(i__1 = (node << 7) - 128) < 1280 && 0 <= 
	    i__1 ? i__1 : s_rnge("rcbufd", i__1, "dasrwr_", (ftnlen)1998)]);

/*     Set the update flag, indicating that this buffer entry */
/*     has been modified. Also set the handle and record number */
/*     entries. */

    upbufd[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("upbufd", i__1,
	     "dasrwr_", (ftnlen)2005)] = TRUE_;
    hnbufd[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("hnbufd", i__1,
	     "dasrwr_", (ftnlen)2006)] = *handle;
    rnbufd[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("rnbufd", i__1,
	     "dasrwr_", (ftnlen)2007)] = *recno;

/*     Link this buffer entry to the head of the list. */

    lnkilb_(&node, &headd, poold);
    headd = node;
    chkout_("DASWRD", (ftnlen)6);
    return 0;
/* $Procedure DASWRI ( DAS, write record, integer ) */

L_daswri:
/* $ Abstract */

/*     Write DAS integer physical records. */

/* $ Disclaimer */

/*     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE */
/*     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S. */
/*     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE */
/*     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE */
/*     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS" */
/*     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY */
/*     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A */
/*     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC */
/*     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE */
/*     SOFTWARE AND RELATED MATERIALS, HOWEVER USED. */

/*     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA */
/*     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT */
/*     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND, */
/*     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS, */
/*     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE */
/*     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY. */

/*     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF */
/*     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY */
/*     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE */
/*     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE. */

/* $ Required_Reading */

/*     DAS */

/* $ Keywords */

/*     ASSIGNMENT */
/*     DAS */
/*     FILES */

/* $ Declarations */

/*     INTEGER               HANDLE */
/*     INTEGER               RECNO */
/*     INTEGER               RECI   ( NWI ) */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Handle of DAS file. */
/*     RECNO      I   Record number. */
/*     RECI       I   Integer data to be written to record. */
/*     BUFSZI     P   Number of records in the integer record buffer. */
/*     NWI        P   Number of integers in a single DAS integer record. */

/* $ Detailed_Input */

/*     HANDLE   is the handle of a DAS file opened for writing. */

/*     RECNO    is the number of a record in a DAS file. */

/*     RECI     is an array of NWI integers. The contents of this */
/*              array are to be written to the physical file */
/*              record having number RECNO. */

/* $ Detailed_Output */

/*     None. See $Particulars for a description of the action of this */
/*     routine. */

/* $ Parameters */

/*     NWI      is the number of integers in a single DAS record */
/*              containing integers. */

/*     BUFSZI   is the number of records in the integer record */
/*              buffer. */

/* $ Exceptions */

/*     1)  If the input file handle is invalid, an error is signaled by a */
/*         routine in the call tree of this routine. The DAS file */
/*         designated by HANDLE will not be modified. */

/*     2)  If a write operation attempted by this routine fails, an error */
/*         is signaled by a routine in the call tree of this routine. The */
/*         status of the DAS file written to is uncertain in this case. */
/*         Note that the file written to may be different than the file */
/*         designated by HANDLE if multiple DAS files are open for */
/*         writing. */

/* $ Files */

/*     See the description of the argument HANDLE in $Detailed_Input. */

/* $ Particulars */

/*     Routines outside of SPICELIB will normally have no need to call */
/*     this routine. */

/*     This routine can be used to write to only DAS files that are open */
/*     for writing. Records written via this routine will always be */
/*     buffered immediately, but may not be written to the file until */
/*     they are cleared from the integer buffer to make room for other */
/*     records, or until they are explicitly forced to to be written via */
/*     a call to DASWBR. In any case, at the moment this routine */
/*     returns, the data supplied on input may be read back by DASRRI */
/*     or updated by DASURI. */

/*     Closing a DAS file via DASCLS forces any remaining updated data */
/*     records buffered by this routine to be written to the file. */

/* $ Examples */

/*     1)  Write an array of NWI integers to the 9th record in a DAS */
/*         file designated by HANDLE. */

/*            INTEGER                RECI ( NWI ) */
/*                         . */
/*                         . */
/*                         . */

/*            DO I = 1, NWI */
/*               RECI(I) = I */
/*            END DO */

/*            CALL DASWRI ( HANDLE, 9, RECI ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.1, 22-FEB-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 2.0.0, 30-JUL-2014 (NJB) */

/*        Upgraded to support handle manager integration. */

/* -    SPICELIB Version 1.0.3, 10-FEB-2014 (BVS) */

/*        Added description of NWI to the $Parameters and $Brief_I/O */
/*        sections of the header. */

/* -    SPICELIB Version 1.0.2, 03-NOV-1995 (NJB) */

/*        Removed weird spaces from ENTRY statement. */

/* -    SPICELIB Version 1.0.1, 28-OCT-1993 (KRG) */

/*        Removed references to specific DAS file open routines in the */
/*        $Detailed_Input section of the header. This was done in order */
/*        to minimize documentation changes if the DAS open routines ever */
/*        change. */

/* -    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT) */

/* -& */
/* $ Index_Entries */

/*     write DAS integer physical records */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.0.1, 28-OCT-1993 (KRG) */

/*        Removed references to specific DAS file open routines in the */
/*        $Detailed_Input section of the header. This was done in order */
/*        to minimize documentation changes if the DAS open routines ever */
/*        change. */

/* -    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT) */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("DASWRI", (ftnlen)6);

/*     Check that the file is open for writing.  Signal an error if not. */

    dassih_(handle, "WRITE", (ftnlen)5);
    if (failed_()) {
	chkout_("DASWRI", (ftnlen)6);
	return 0;
    }

/*     If it hasn't been done yet, initialize the pointer list pools. */

    if (pass1) {
	lnkini_(&c__10, poold);
	lnkini_(&c__10, pooli);
	lnkini_(&c__10, poolc);
	pass1 = FALSE_;
    }

/*     See whether integer record number RECNO from file HANDLE is */
/*     buffered.  We'll search through the list of buffered records */
/*     starting at the head of the list.  If the record is already */
/*     buffered, we'll update the buffer entry, but we'll defer writing */
/*     the record out until we need to free a record, or until the */
/*     integer buffer is flushed, whichever comes first. */

    node = headi;
    while(node > 0) {
	if (*handle == hnbufi[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : 
		s_rnge("hnbufi", i__1, "dasrwr_", (ftnlen)2268)] && *recno == 
		rnbufi[(i__2 = node - 1) < 10 && 0 <= i__2 ? i__2 : s_rnge(
		"rnbufi", i__2, "dasrwr_", (ftnlen)2268)]) {

/*           Found it.  Update the buffered record. */

	    movei_(reci, &c__256, &rcbufi[(i__1 = (node << 8) - 256) < 2560 &&
		     0 <= i__1 ? i__1 : s_rnge("rcbufi", i__1, "dasrwr_", (
		    ftnlen)2273)]);

/*           Set the update flag, indicating that this buffer entry */
/*           has been modified. */

	    upbufi[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("upbu"
		    "fi", i__1, "dasrwr_", (ftnlen)2279)] = TRUE_;

/*           Put the information about this record at the head of the */
/*           active list, if it is not already there. */

	    if (node != headi) {
		lnkxsl_(&node, &node, pooli);
		lnkilb_(&node, &headi, pooli);
		headi = node;
	    }
	    chkout_("DASWRI", (ftnlen)6);
	    return 0;
	}
	node = pooli[(i__1 = (node << 1) + 10) < 32 && 0 <= i__1 ? i__1 : 
		s_rnge("pooli", i__1, "dasrwr_", (ftnlen)2298)];
    }

/*     The record we're writing to is not buffered.  We'll allocate */
/*     a buffer entry.  If the record buffer is full, we'll */
/*     commandeer the least recently accessed record.  Before using */
/*     this record, we'll write its contents out to the corresponding */
/*     file, if the record has been updated. */

    if (usedi < 10) {

/*        There's a free buffer entry available.  Just allocate it. */

	lnkan_(pooli, &node);
	++usedi;
    } else {

/*        Grab the buffer entry at the tail end of the list. */

	node = lnktl_(&headi, pooli);
	lnkxsl_(&node, &node, pooli);

/*        If the allocated record was updated, write it out. */

	if (upbufi[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("upbu"
		"fi", i__1, "dasrwr_", (ftnlen)2327)]) {
	    zzddhhlu_(&hnbufi[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : 
		    s_rnge("hnbufi", i__1, "dasrwr_", (ftnlen)2329)], "DAS", &
		    c_false, &wrunit, (ftnlen)3);
	    dasioi_("WRITE", &wrunit, &rnbufi[(i__1 = node - 1) < 10 && 0 <= 
		    i__1 ? i__1 : s_rnge("rnbufi", i__1, "dasrwr_", (ftnlen)
		    2331)], &rcbufi[(i__2 = (node << 8) - 256) < 2560 && 0 <= 
		    i__2 ? i__2 : s_rnge("rcbufi", i__2, "dasrwr_", (ftnlen)
		    2331)], (ftnlen)5);
	    if (failed_()) {
		chkout_("DASWRI", (ftnlen)6);
		return 0;
	    }
	}
    }

/*     Now update the allocated buffer entry with the input data. */

    movei_(reci, &c__256, &rcbufi[(i__1 = (node << 8) - 256) < 2560 && 0 <= 
	    i__1 ? i__1 : s_rnge("rcbufi", i__1, "dasrwr_", (ftnlen)2348)]);

/*     Set the update flag, indicating that this buffer entry */
/*     has been modified.  Also set the handle and record number */
/*     entries. */

    zzddhhlu_(handle, "DAS", &c_false, &unit, (ftnlen)3);
    upbufi[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("upbufi", i__1,
	     "dasrwr_", (ftnlen)2357)] = TRUE_;
    hnbufi[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("hnbufi", i__1,
	     "dasrwr_", (ftnlen)2358)] = *handle;
    rnbufi[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("rnbufi", i__1,
	     "dasrwr_", (ftnlen)2359)] = *recno;

/*     Link this buffer entry to the head of the list. */

    lnkilb_(&node, &headi, pooli);
    headi = node;
    chkout_("DASWRI", (ftnlen)6);
    return 0;
/* $Procedure DASWRC ( DAS, write record, character ) */

L_daswrc:
/* $ Abstract */

/*     Write DAS character physical records. */

/* $ Disclaimer */

/*     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE */
/*     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S. */
/*     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE */
/*     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE */
/*     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS" */
/*     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY */
/*     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A */
/*     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC */
/*     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE */
/*     SOFTWARE AND RELATED MATERIALS, HOWEVER USED. */

/*     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA */
/*     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT */
/*     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND, */
/*     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS, */
/*     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE */
/*     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY. */

/*     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF */
/*     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY */
/*     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE */
/*     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE. */

/* $ Required_Reading */

/*     DAS */

/* $ Keywords */

/*     ASSIGNMENT */
/*     DAS */
/*     FILES */

/* $ Declarations */

/*     INTEGER               HANDLE */
/*     INTEGER               RECNO */
/*     CHARACTER*(*)         RECC */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Handle of DAS file. */
/*     RECNO      I   Record number. */
/*     RECC       I   Character data to be written to record. */
/*     BUFSZC     P   Number of records in the character record buffer. */
/*     NWC        P   Number of characters in a single DAS char. record. */

/* $ Detailed_Input */

/*     HANDLE   is the handle of a DAS file opened for writing. */

/*     RECNO    is the number of a record in a DAS file. */

/*     RECC     is a string of length NWC. The contents of this */
/*              string are to be written to the physical file */
/*              record having number RECNO. */

/* $ Detailed_Output */

/*     None. See $Particulars for a description of the action of this */
/*     routine. */

/* $ Parameters */

/*     NWC      is the number of characters in a single DAS record */
/*              containing characters. */

/*     BUFSZC   is the number of records in the character record */
/*              buffer. */

/* $ Exceptions */

/*     1)  If the input file handle is invalid, an error is signaled by a */
/*         routine in the call tree of this routine. The DAS file */
/*         designated by HANDLE will not be modified. */

/*     2)  If a write operation attempted by this routine fails, an error */
/*         is signaled by a routine in the call tree of this routine. The */
/*         status of the DAS file written to is uncertain in this case. */
/*         Note that the file written to may be different than the file */
/*         designated by HANDLE if multiple DAS files are open for */
/*         writing. */

/* $ Files */

/*     See the description of the argument HANDLE in $Detailed_Input. */

/* $ Particulars */

/*     Routines outside of SPICELIB will normally have no need to call */
/*     this routine. */

/*     This routine can be used to write to only DAS files that are open */
/*     for writing. Records written via this routine will always be */
/*     buffered immediately, but may not be written to the file until */
/*     they are cleared from the character buffer to make room for other */
/*     records, or until they are explicitly forced to to be written via */
/*     a call to DASWBR. In any case, at the moment this routine */
/*     returns, the data supplied on input may be read back by DASRRC */
/*     or updated by DASURC. */

/*     Closing a DAS file via DASCLS forces any remaining updated data */
/*     records buffered by this routine to be written to the file. */

/* $ Examples */

/*     1)  Write a string of NWC characters to the 9th record in a DAS */
/*         file designated by HANDLE. */

/*            CHARACTER*(NWC)           RECC */

/*                         . */
/*                         . */
/*                         . */

/*            RECC = 'This example string is blank-padded on the '    // */
/*           .       'right.  All of the trailing blanks will be '    // */
/*           .       'written to the DAS file by the following call.' */

/*            CALL DASWRC ( HANDLE, 9, RECC ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.1, 22-FEB-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 2.0.0, 30-JUL-2014 (NJB) */

/*        Upgraded to support handle manager integration. */

/* -    SPICELIB Version 1.0.3, 10-FEB-2014 (BVS) */

/*        Added description of NWC to the $Parameters and $Brief_I/O */
/*        sections of the header. */

/* -    SPICELIB Version 1.0.2, 03-NOV-1995 (NJB) */

/*        Removed weird spaces from ENTRY statement. */

/* -    SPICELIB Version 1.0.1, 28-OCT-1993 (KRG) */

/*        Removed references to specific DAS file open routines in the */
/*        $Detailed_Input section of the header. This was done in order */
/*        to minimize documentation changes if the DAS open routines ever */
/*        change. */

/* -    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT) */

/* -& */
/* $ Index_Entries */

/*     write DAS character physical records */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.0.1, 28-OCT-1993 (KRG) */

/*        Removed references to specific DAS file open routines in the */
/*        $Detailed_Input section of the header. This was done in order */
/*        to minimize documentation changes if the DAS open routines ever */
/*        change. */

/* -    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT) */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("DASWRC", (ftnlen)6);

/*     Check that the file is open for writing.  Signal an error if not. */

    dassih_(handle, "WRITE", (ftnlen)5);
    if (failed_()) {
	chkout_("DASWRC", (ftnlen)6);
	return 0;
    }

/*     If it hasn't been done yet, initialize the pointer list pools. */

    if (pass1) {
	lnkini_(&c__10, poold);
	lnkini_(&c__10, pooli);
	lnkini_(&c__10, poolc);
	pass1 = FALSE_;
    }

/*     See whether character record number RECNO from file HANDLE is */
/*     buffered.  We'll search through the list of buffered records */
/*     starting at the head of the list.  If the record is already */
/*     buffered, we'll update the buffer entry, but we'll defer writing */
/*     the record out until we need to free a record, or until the */
/*     character buffer is flushed, whichever comes first. */

    node = headc;
    while(node > 0) {
	if (*handle == hnbufc[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : 
		s_rnge("hnbufc", i__1, "dasrwr_", (ftnlen)2621)] && *recno == 
		rnbufc[(i__2 = node - 1) < 10 && 0 <= i__2 ? i__2 : s_rnge(
		"rnbufc", i__2, "dasrwr_", (ftnlen)2621)]) {

/*           Found it.  Update the buffered record. */

	    s_copy(rcbufc + (((i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : 
		    s_rnge("rcbufc", i__1, "dasrwr_", (ftnlen)2626)) << 10), 
		    recc, (ftnlen)1024, recc_len);

/*           Set the update flag, indicating that this buffer entry */
/*           has been modified. */

	    upbufc[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("upbu"
		    "fc", i__1, "dasrwr_", (ftnlen)2632)] = TRUE_;

/*           Put the information about this record at the head of the */
/*           active list, if it is not already there. */

	    if (node != headc) {
		lnkxsl_(&node, &node, poolc);
		lnkilb_(&node, &headc, poolc);
		headc = node;
	    }
	    chkout_("DASWRC", (ftnlen)6);
	    return 0;
	}
	node = poolc[(i__1 = (node << 1) + 10) < 32 && 0 <= i__1 ? i__1 : 
		s_rnge("poolc", i__1, "dasrwr_", (ftnlen)2651)];
    }

/*     The record we're writing to is not buffered.  We'll allocate */
/*     a buffer entry.  If the record buffer is full, we'll */
/*     commandeer the least recently accessed record.  Before using */
/*     this record, we'll write its contents out to the corresponding */
/*     file, if the record has been updated. */

    if (usedc < 10) {

/*        There's a free buffer entry available.  Just allocate it. */

	lnkan_(poolc, &node);
	++usedc;
    } else {

/*        Grab the buffer entry at the tail end of the list. */

	node = lnktl_(&headc, poolc);
	lnkxsl_(&node, &node, poolc);

/*        If the allocated record was updated, write it out. */

	if (upbufc[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("upbu"
		"fc", i__1, "dasrwr_", (ftnlen)2680)]) {
	    zzddhhlu_(&hnbufc[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : 
		    s_rnge("hnbufc", i__1, "dasrwr_", (ftnlen)2682)], "DAS", &
		    c_false, &wrunit, (ftnlen)3);
	    dasioc_("WRITE", &wrunit, &rnbufc[(i__1 = node - 1) < 10 && 0 <= 
		    i__1 ? i__1 : s_rnge("rnbufc", i__1, "dasrwr_", (ftnlen)
		    2684)], rcbufc + (((i__2 = node - 1) < 10 && 0 <= i__2 ? 
		    i__2 : s_rnge("rcbufc", i__2, "dasrwr_", (ftnlen)2684)) <<
		     10), (ftnlen)5, (ftnlen)1024);
	    if (failed_()) {
		chkout_("DASWRC", (ftnlen)6);
		return 0;
	    }
	}
    }

/*     Now update the allocated buffer entry with the input data. */

    s_copy(rcbufc + (((i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge(
	    "rcbufc", i__1, "dasrwr_", (ftnlen)2701)) << 10), recc, (ftnlen)
	    1024, recc_len);

/*     Set the update flag, indicating that this buffer entry */
/*     has been modified.  Also set the handle and record number */
/*     entries. */

    upbufc[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("upbufc", i__1,
	     "dasrwr_", (ftnlen)2708)] = TRUE_;
    hnbufc[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("hnbufc", i__1,
	     "dasrwr_", (ftnlen)2709)] = *handle;
    rnbufc[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("rnbufc", i__1,
	     "dasrwr_", (ftnlen)2710)] = *recno;

/*     Link this buffer entry to the head of the list. */

    lnkilb_(&node, &headc, poolc);
    headc = node;
    chkout_("DASWRC", (ftnlen)6);
    return 0;
/* $Procedure DASURD ( DAS, update record, double precision ) */

L_dasurd:
/* $ Abstract */

/*     Update DAS double precision physical records. */

/* $ Disclaimer */

/*     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE */
/*     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S. */
/*     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE */
/*     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE */
/*     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS" */
/*     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY */
/*     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A */
/*     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC */
/*     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE */
/*     SOFTWARE AND RELATED MATERIALS, HOWEVER USED. */

/*     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA */
/*     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT */
/*     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND, */
/*     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS, */
/*     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE */
/*     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY. */

/*     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF */
/*     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY */
/*     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE */
/*     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE. */

/* $ Required_Reading */

/*     DAS */

/* $ Keywords */

/*     ASSIGNMENT */
/*     DAS */
/*     FILES */

/* $ Declarations */

/*     INTEGER               HANDLE */
/*     INTEGER               RECNO */
/*     INTEGER               FIRST */
/*     INTEGER               LAST */
/*     DOUBLE PRECISION      DATAD  ( * ) */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Handle of DAS file. */
/*     RECNO      I   Record number. */
/*     FIRST, */
/*     LAST       I   First and last indices of range within record. */
/*     DATAD      I   Double precision data to write to record. */
/*     BUFSZD     P   Number of records in the DP record buffer. */
/*     NWD        P   Number of DPs in a single DAS DP record. */

/* $ Detailed_Input */

/*     HANDLE   is the handle of a DAS file opened for writing. */

/*     RECNO    is the number of a record in a DAS file. */

/*     FIRST, */
/*     LAST     are the first and last indices of a range of */
/*              elements to be updated in the indicated record. */
/*              The record contains NWD double precision numbers; */
/*              these have indices ranging from 1 to NWD. */

/*     DATAD    is a double precision array to be written to */
/*              elements FIRST through LAST of the specified */
/*              record. The array element DATAD(1) is placed in */
/*              record element FIRST, the array element DATAD(2) */
/*              is placed in record element FIRST+1, and so on; */
/*              the array element DATAD(LAST-FIRST+1) is placed in */
/*              the record element LAST. */

/* $ Detailed_Output */

/*     None. See $Particulars for a description of the action of this */
/*     routine. */

/* $ Parameters */

/*     NWD      is the number of DPs in a single DAS record */
/*              containing DPs. */

/*     BUFSZD   is the number of records in the double precision */
/*              record buffer. */

/* $ Exceptions */

/*     1)  This routine may be used to update only records that have */
/*         already been written by DASWRD or that already exist in the */
/*         file designated by HANDLE. Attempting to update a record */
/*         that hasn't yet been written will cause the read operation */
/*         performed by this routine to fail. */

/*         If a read operation attempted by this routine fails for this */
/*         or any other reason, an error is signaled by a routine in the */
/*         call tree of this routine. The indicated record will not be */
/*         modified. */

/*     2)  If a write operation attempted by this routine fails, an error */
/*         is signaled by a routine in the call tree of this routine. The */
/*         status of the DAS file written to is uncertain in this case. */
/*         Note that the file written to may be different than the file */
/*         designated by HANDLE if multiple DAS files are open for */
/*         writing. */

/*     3)  If the input file handle is invalid, an error is signaled by a */
/*         routine in the call tree of this routine. The indicated record */
/*         will not be modified. */

/*     4)  If FIRST or LAST is not in the range [1, NWD], the error */
/*         SPICE(INDEXOUTOFRANGE) is signaled. The indicated */
/*         record will not be modified. */

/*     5)  If FIRST > LAST, this routine will return without modifying */
/*         the indicated record. */

/* $ Files */

/*     See the description of the argument HANDLE in $Detailed_Input. */

/* $ Particulars */

/*     Routines outside of SPICELIB will normally have no need to call */
/*     this routine. */

/*     This routine can be used to update any existing record in a DAS */
/*     file that is open for writing, or any record that has been */
/*     `written' by DASWRD, whether or not that record has yet been */
/*     physically written to the file it belongs to. Records that have */
/*     never been written cannot be updated. */

/*     Because the DAS system buffers records that are written, multiple */
/*     updates of parts of a record can be made without incurring a */
/*     large number of file reads and writes. */

/*     This routine should be used to update only records that contain */
/*     double precision data. */

/* $ Examples */

/*     1)  Update the 10th through 100th d.p. numbers in record number 9 */
/*         in a DAS file designated by HANDLE. */

/*             DOUBLE PRECISION      DATAD ( 100 ) */

/*                         . */
/*                         . */
/*                         . */

/*             DO I = 1, 91 */
/*                DATAD  =  DBLE(I) */
/*             END DO */

/*             CALL DASURD ( HANDLE, 9, 10, 100, DATAD ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.1, 22-FEB-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 2.0.0, 05-FEB-2015 (NJB) */

/*        Upgraded to support handle manager integration. */

/* -    SPICELIB Version 1.0.3, 10-FEB-2014 (BVS) */

/*        Added description of NWD to the $Parameters and $Brief_I/O */
/*        sections of the header. */

/* -    SPICELIB Version 1.0.2, 03-NOV-1995 (NJB) */

/*        Removed weird spaces from ENTRY statement. */

/* -    SPICELIB Version 1.0.1, 28-OCT-1993 (KRG) */

/*        Removed references to specific DAS file open routines in the */
/*        $Detailed_Input section of the header. This was done in order */
/*        to minimize documentation changes if the DAS open routines ever */
/*        change. */

/* -    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT) */

/* -& */
/* $ Index_Entries */

/*     update DAS double precision physical records */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.0.1, 28-OCT-1993 (KRG) */

/*        Removed references to specific DAS file open routines in the */
/*        $Detailed_Input section of the header. This was done in order */
/*        to minimize documentation changes if the DAS open routines ever */
/*        change. */

/* -    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT) */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("DASURD", (ftnlen)6);

/*     Check that the file is open for writing.  Signal an error if not. */

    dassih_(handle, "WRITE", (ftnlen)5);
    if (failed_()) {
	chkout_("DASURD", (ftnlen)6);
	return 0;
    }

/*     If it hasn't been done yet, initialize the pointer list pools. */

    if (pass1) {
	lnkini_(&c__10, poold);
	lnkini_(&c__10, pooli);
	lnkini_(&c__10, poolc);
	pass1 = FALSE_;
    }

/*     If FIRST or LAST are out of range, no dice. */

    if (*first < 1 || *first > 128 || *last < 1 || *last > 128) {
	setmsg_("Array indices FIRST and LAST were #,  #; allowed range for "
		"both is [#, #]. File was #, record number was #.", (ftnlen)
		107);
	errint_("#", first, (ftnlen)1);
	errint_("#", last, (ftnlen)1);
	errint_("#", &c__1, (ftnlen)1);
	errint_("#", &c__128, (ftnlen)1);
	errhan_("#", handle, (ftnlen)1);
	errint_("#", recno, (ftnlen)1);
	sigerr_("SPICE(INDEXOUTOFRANGE)", (ftnlen)22);
	chkout_("DASURD", (ftnlen)6);
	return 0;
    }

/*     There's nothing to do if LAST < FIRST. */

    if (*last < *first) {
	chkout_("DASURD", (ftnlen)6);
	return 0;
    }

/*     See whether double precision record number RECNO from file HANDLE */
/*     is buffered.  We'll search through the list of buffered records */
/*     starting at the head of the list.  If the record is already */
/*     buffered, we'll update the buffer entry, but we'll defer writing */
/*     the record out until we need to free a record, or until the */
/*     d.p. buffer is flushed, whichever comes first. */

    node = headd;
    while(node > 0) {
	if (*handle == hnbufd[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : 
		s_rnge("hnbufd", i__1, "dasrwr_", (ftnlen)3036)] && *recno == 
		rnbufd[(i__2 = node - 1) < 10 && 0 <= i__2 ? i__2 : s_rnge(
		"rnbufd", i__2, "dasrwr_", (ftnlen)3036)]) {

/*           Found it.  Update the buffered record. */

	    i__2 = *last - *first + 1;
	    moved_(datad, &i__2, &rcbufd[(i__1 = *first + (node << 7) - 129) <
		     1280 && 0 <= i__1 ? i__1 : s_rnge("rcbufd", i__1, "dasr"
		    "wr_", (ftnlen)3041)]);

/*           Set the update flag, indicating that this buffer entry */
/*           has been modified. */

	    upbufd[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("upbu"
		    "fd", i__1, "dasrwr_", (ftnlen)3047)] = TRUE_;

/*           Put the information about this record at the head of the */
/*           active list, if it is not already there. */

	    if (node != headd) {
		lnkxsl_(&node, &node, poold);
		lnkilb_(&node, &headd, poold);
		headd = node;
	    }
	    chkout_("DASURD", (ftnlen)6);
	    return 0;
	}
	node = poold[(i__1 = (node << 1) + 10) < 32 && 0 <= i__1 ? i__1 : 
		s_rnge("poold", i__1, "dasrwr_", (ftnlen)3066)];
    }

/*     The record we're writing to is not buffered.  In order to */
/*     update this record, we'll need to read it first.  But before */
/*     we do that, we'll need to allocate a buffer entry.  If the record */
/*     buffer is full, we'll commandeer the least recently accessed */
/*     record.  Before using this record, we'll write its contents out */
/*     to the corresponding file, if the record has been updated. */

    if (usedd < 10) {

/*        There's a free buffer entry available.  Just allocate it. */

	lnkan_(poold, &node);
	++usedd;
    } else {

/*        Grab the buffer entry at the tail end of the list. */

	node = lnktl_(&headd, poold);
	lnkxsl_(&node, &node, poold);

/*        If the allocated record was updated, write it out. */

	if (upbufd[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("upbu"
		"fd", i__1, "dasrwr_", (ftnlen)3097)]) {
	    zzddhhlu_(&hnbufd[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : 
		    s_rnge("hnbufd", i__1, "dasrwr_", (ftnlen)3099)], "DAS", &
		    c_false, &wrunit, (ftnlen)3);
	    dasiod_("WRITE", &wrunit, &rnbufd[(i__1 = node - 1) < 10 && 0 <= 
		    i__1 ? i__1 : s_rnge("rnbufd", i__1, "dasrwr_", (ftnlen)
		    3101)], &rcbufd[(i__2 = (node << 7) - 128) < 1280 && 0 <= 
		    i__2 ? i__2 : s_rnge("rcbufd", i__2, "dasrwr_", (ftnlen)
		    3101)], (ftnlen)5);
	    if (failed_()) {
		chkout_("DASURD", (ftnlen)6);
		return 0;
	    }
	}
    }

/*     Now try to read the record we're going to update. */

    zzdasgrd_(handle, recno, &rcbufd[(i__1 = (node << 7) - 128) < 1280 && 0 <=
	     i__1 ? i__1 : s_rnge("rcbufd", i__1, "dasrwr_", (ftnlen)3119)]);
    if (failed_()) {
	chkout_("DASURD", (ftnlen)6);
	return 0;
    }

/*     The read was successful, so set the record number, handle, */
/*     and update flag for this buffer entry, and link these buffer */
/*     entries in before the current head of the list, thus putting */
/*     them at the head. */

/*     Update the head pointer. */

    lnkilb_(&node, &headd, poold);
    hnbufd[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("hnbufd", i__1,
	     "dasrwr_", (ftnlen)3136)] = *handle;
    rnbufd[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("rnbufd", i__1,
	     "dasrwr_", (ftnlen)3137)] = *recno;
    upbufd[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("upbufd", i__1,
	     "dasrwr_", (ftnlen)3138)] = TRUE_;
    headd = node;

/*     At long last, make the requested update.  Note that we don't */
/*     have to write the record back to the file; that will get done */
/*     automatically before or at the time the file is closed. */

    i__2 = *last - *first + 1;
    moved_(datad, &i__2, &rcbufd[(i__1 = *first + (node << 7) - 129) < 1280 &&
	     0 <= i__1 ? i__1 : s_rnge("rcbufd", i__1, "dasrwr_", (ftnlen)
	    3146)]);
    chkout_("DASURD", (ftnlen)6);
    return 0;
/* $Procedure DASURI ( DAS, update record, integer ) */

L_dasuri:
/* $ Abstract */

/*     Update DAS integer physical records. */

/* $ Disclaimer */

/*     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE */
/*     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S. */
/*     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE */
/*     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE */
/*     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS" */
/*     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY */
/*     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A */
/*     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC */
/*     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE */
/*     SOFTWARE AND RELATED MATERIALS, HOWEVER USED. */

/*     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA */
/*     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT */
/*     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND, */
/*     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS, */
/*     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE */
/*     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY. */

/*     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF */
/*     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY */
/*     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE */
/*     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE. */

/* $ Required_Reading */

/*     DAS */

/* $ Keywords */

/*     ASSIGNMENT */
/*     DAS */
/*     FILES */

/* $ Declarations */

/*     INTEGER               HANDLE */
/*     INTEGER               RECNO */
/*     INTEGER               FIRST */
/*     INTEGER               LAST */
/*     INTEGER               DATAI  ( * ) */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Handle of DAS file. */
/*     RECNO      I   Record number. */
/*     FIRST, */
/*     LAST       I   First and last indices of range within record. */
/*     DATAI      I   Integer data to write to record. */
/*     BUFSZI     P   Number of records in the integer record buffer. */
/*     NWI        P   Number of integers in a single DAS integer record. */

/* $ Detailed_Input */

/*     HANDLE   is the handle of a DAS file opened for writing. */

/*     RECNO    is the number of a record in a DAS file. */

/*     FIRST, */
/*     LAST     are the first and last indices of a range of */
/*              elements to be updated in the indicated record. */
/*              The record contains NWI integers; these have */
/*              indices ranging from 1 to NWI. */

/*     DATAI    is an integer array to be written to elements FIRST */
/*              through LAST of the specified record. The array */
/*              element DATAI(1) is placed in record element FIRST, */
/*              the array element DATAI(2) is placed in record */
/*              element FIRST+1, and so on; the array element */
/*              DATAI(LAST-FIRST+1) is placed in the record element */
/*              LAST. */

/* $ Detailed_Output */

/*     None. See $Particulars for a description of the action of this */
/*     routine. */

/* $ Parameters */

/*     NWI      is the number of integers in a single DAS record */
/*              containing integers. */

/*     BUFSZI   is the number of records in the integer record */
/*              buffer. */

/* $ Exceptions */

/*     1)  This routine may be used to update only records that have */
/*         already been written by DASWRI or that already exist in the */
/*         file designated by HANDLE. Attempting to update a record */
/*         that hasn't yet been written will cause the read operation */
/*         performed by this routine to fail. */

/*         If a read operation attempted by this routine fails for this */
/*         or any other reason, an error is signaled by a routine in the */
/*         call tree of this routine. The indicated record will not be */
/*         modified. */

/*     2)  If a write operation attempted by this routine fails, an error */
/*         is signaled by a routine in the call tree of this routine. The */
/*         status of the DAS file written to is uncertain in this case. */
/*         Note that the file written to may be different than the file */
/*         designated by HANDLE if multiple DAS files are open for */
/*         writing. */

/*     3)  If the input file handle is invalid, an error is signaled by a */
/*         routine in the call tree of this routine. The indicated record */
/*         will not be modified. */

/*     4)  If FIRST or LAST is not in the range [1, NWI], the error */
/*         SPICE(INDEXOUTOFRANGE) is signaled. The indicated */
/*         record will not be modified. */

/*     5)  If FIRST > LAST, this routine will return without modifying */
/*         the indicated record. */

/* $ Files */

/*     See the description of the argument HANDLE in $Detailed_Input. */

/* $ Particulars */

/*     Routines outside of SPICELIB will normally have no need to call */
/*     this routine. */

/*     This routine can be used to update any existing record in a DAS */
/*     file that is open for writing, or any record that has been */
/*     `written' by DASWRI, whether or not that record has yet been */
/*     physically written to the file it belongs to. Records that have */
/*     never been written cannot be updated. */

/*     Because the DAS system buffers records that are written, multiple */
/*     updates of parts of a record can be made without incurring a */
/*     large number of file reads and writes. */

/*     This routine should be used to update only records that contain */
/*     integer data. */

/* $ Examples */

/*     1)  Update the 10th through 100th integers in record number 9 */
/*         in a DAS file designated by HANDLE. */

/*             INTEGER               DATAI ( 100 ) */

/*                         . */
/*                         . */
/*                         . */

/*             DO I = 1, 91 */
/*                DATAI  =  I */
/*             END DO */

/*             CALL DASURI ( HANDLE, 9, 10, 100, DATAI ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.1, 22-FEB-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 2.0.0, 05-FEB-2015 (NJB) */

/*        Upgraded to support handle manager integration. */

/* -    SPICELIB Version 1.0.3, 10-FEB-2014 (BVS) */

/*        Added description of NWI to the $Parameters and $Brief_I/O */
/*        sections of the header. */

/* -    SPICELIB Version 1.0.2, 03-NOV-1995 (NJB) */

/*        Removed weird spaces from ENTRY statement. */

/* -    SPICELIB Version 1.0.1, 28-OCT-1993 (KRG) */

/*        Removed references to specific DAS file open routines in the */
/*        $Detailed_Input section of the header. This was done in order */
/*        to minimize documentation changes if the DAS open routines ever */
/*        change. */

/* -    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT) */

/* -& */
/* $ Index_Entries */

/*     update DAS integer physical records */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.0.1, 28-OCT-1993 (KRG) */

/*        Removed references to specific DAS file open routines in the */
/*        $Detailed_Input section of the header. This was done in order */
/*        to minimize documentation changes if the DAS open routines ever */
/*        change. */

/* -    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT) */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("DASURI", (ftnlen)6);

/*     Check that the file is open for writing.  Signal an error if not. */

    dassih_(handle, "WRITE", (ftnlen)5);
    if (failed_()) {
	chkout_("DASURI", (ftnlen)6);
	return 0;
    }

/*     If it hasn't been done yet, initialize the pointer list pools. */

    if (pass1) {
	lnkini_(&c__10, poold);
	lnkini_(&c__10, pooli);
	lnkini_(&c__10, poolc);
	pass1 = FALSE_;
    }

/*     If FIRST or LAST are out of range, no dice. */

    if (*first < 1 || *first > 256 || *last < 1 || *last > 256) {
	setmsg_("Array indices FIRST and LAST were #,  #; allowed range for "
		"both is [#, #]. File was #, record number was #.", (ftnlen)
		107);
	errint_("#", first, (ftnlen)1);
	errint_("#", last, (ftnlen)1);
	errint_("#", &c__1, (ftnlen)1);
	errint_("#", &c__256, (ftnlen)1);
	errhan_("#", handle, (ftnlen)1);
	errint_("#", recno, (ftnlen)1);
	sigerr_("SPICE(INDEXOUTOFRANGE)", (ftnlen)22);
	chkout_("DASURI", (ftnlen)6);
	return 0;
    }

/*     There's nothing to do if LAST < FIRST. */

    if (*last < *first) {
	chkout_("DASURI", (ftnlen)6);
	return 0;
    }

/*     See whether integer record number RECNO from file HANDLE is */
/*     buffered.  We'll search through the list of buffered records */
/*     starting at the head of the list.  If the record is already */
/*     buffered, we'll update the buffer entry, but we'll defer writing */
/*     the record out until we need to free a record, or until the */
/*     integer buffer is flushed, whichever comes first. */

    node = headi;
    while(node > 0) {
	if (*handle == hnbufi[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : 
		s_rnge("hnbufi", i__1, "dasrwr_", (ftnlen)3467)] && *recno == 
		rnbufi[(i__2 = node - 1) < 10 && 0 <= i__2 ? i__2 : s_rnge(
		"rnbufi", i__2, "dasrwr_", (ftnlen)3467)]) {

/*           Found it.  Update the buffered record. */

	    i__2 = *last - *first + 1;
	    movei_(datai, &i__2, &rcbufi[(i__1 = *first + (node << 8) - 257) <
		     2560 && 0 <= i__1 ? i__1 : s_rnge("rcbufi", i__1, "dasr"
		    "wr_", (ftnlen)3472)]);

/*           Set the update flag, indicating that this buffer entry */
/*           has been modified. */

	    upbufi[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("upbu"
		    "fi", i__1, "dasrwr_", (ftnlen)3478)] = TRUE_;

/*           Put the information about this record at the head of the */
/*           active list, if it is not already there. */

	    if (node != headi) {
		lnkxsl_(&node, &node, pooli);
		lnkilb_(&node, &headi, pooli);
		headi = node;
	    }
	    chkout_("DASURI", (ftnlen)6);
	    return 0;
	}
	node = pooli[(i__1 = (node << 1) + 10) < 32 && 0 <= i__1 ? i__1 : 
		s_rnge("pooli", i__1, "dasrwr_", (ftnlen)3497)];
    }

/*     The record we're writing to is not buffered.  We'll allocate */
/*     a buffer entry.  If the record buffer is full, we'll */
/*     commandeer the least recently accessed record.  Before using */
/*     this record, we'll write its contents out to the corresponding */
/*     file, if the record has been updated. */

    if (usedi < 10) {

/*        There's a free buffer entry available.  Just allocate it. */

	lnkan_(pooli, &node);
	++usedi;
    } else {

/*        Grab the buffer entry at the tail end of the list. */

	node = lnktl_(&headi, pooli);
	lnkxsl_(&node, &node, pooli);

/*        If the allocated record was updated, write it out. */

	if (upbufi[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("upbu"
		"fi", i__1, "dasrwr_", (ftnlen)3526)]) {
	    zzddhhlu_(&hnbufi[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : 
		    s_rnge("hnbufi", i__1, "dasrwr_", (ftnlen)3528)], "DAS", &
		    c_false, &wrunit, (ftnlen)3);
	    dasioi_("WRITE", &wrunit, &rnbufi[(i__1 = node - 1) < 10 && 0 <= 
		    i__1 ? i__1 : s_rnge("rnbufi", i__1, "dasrwr_", (ftnlen)
		    3530)], &rcbufi[(i__2 = (node << 8) - 256) < 2560 && 0 <= 
		    i__2 ? i__2 : s_rnge("rcbufi", i__2, "dasrwr_", (ftnlen)
		    3530)], (ftnlen)5);
	    if (failed_()) {
		chkout_("DASURI", (ftnlen)6);
		return 0;
	    }
	}
    }

/*     Now try to read the record we're going to update. */

    zzdasgri_(handle, recno, &rcbufi[(i__1 = (node << 8) - 256) < 2560 && 0 <=
	     i__1 ? i__1 : s_rnge("rcbufi", i__1, "dasrwr_", (ftnlen)3547)]);
    if (failed_()) {
	chkout_("DASURI", (ftnlen)6);
	return 0;
    }

/*     The read was successful, so set the record number, handle, */
/*     and update flag for this buffer entry, and link these buffer */
/*     entries in before the current head of the list, thus putting */
/*     them at the head. */

/*     Update the head pointer. */

    lnkilb_(&node, &headi, pooli);
    hnbufi[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("hnbufi", i__1,
	     "dasrwr_", (ftnlen)3564)] = *handle;
    rnbufi[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("rnbufi", i__1,
	     "dasrwr_", (ftnlen)3565)] = *recno;
    upbufi[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("upbufi", i__1,
	     "dasrwr_", (ftnlen)3566)] = TRUE_;
    headi = node;

/*     At long last, make the requested update.  Note that we don't */
/*     have to write the record back to the file; that will get done */
/*     automatically before or at the time the file is closed. */

    i__2 = *last - *first + 1;
    movei_(datai, &i__2, &rcbufi[(i__1 = *first + (node << 8) - 257) < 2560 &&
	     0 <= i__1 ? i__1 : s_rnge("rcbufi", i__1, "dasrwr_", (ftnlen)
	    3574)]);
    chkout_("DASURI", (ftnlen)6);
    return 0;
/* $Procedure DASURC ( DAS, update record, character ) */

L_dasurc:
/* $ Abstract */

/*     Update DAS character physical records. */

/* $ Disclaimer */

/*     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE */
/*     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S. */
/*     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE */
/*     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE */
/*     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS" */
/*     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY */
/*     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A */
/*     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC */
/*     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE */
/*     SOFTWARE AND RELATED MATERIALS, HOWEVER USED. */

/*     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA */
/*     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT */
/*     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND, */
/*     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS, */
/*     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE */
/*     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY. */

/*     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF */
/*     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY */
/*     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE */
/*     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE. */

/* $ Required_Reading */

/*     DAS */

/* $ Keywords */

/*     ASSIGNMENT */
/*     DAS */
/*     FILES */

/* $ Declarations */

/*     INTEGER               HANDLE */
/*     INTEGER               RECNO */
/*     INTEGER               FIRST */
/*     INTEGER               LAST */
/*     CHARACTER*(*)         DATAC */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Handle of DAS file. */
/*     RECNO      I   Record number. */
/*     FIRST, */
/*     LAST       I   First and last indices of range within record. */
/*     DATAC      I   Character data to write to record. */
/*     BUFSZC     P   Number of records in the character record buffer. */
/*     NWC        P   Number of characters in a single DAS char. record. */

/* $ Detailed_Input */

/*     HANDLE   is the handle of a DAS file opened for writing. */

/*     RECNO    is the number of a record in a DAS file. */

/*     FIRST, */
/*     LAST     are the first and last indices of a range of */
/*              elements to be updated in the indicated record. */
/*              The record contains NWC characters; these have */
/*              indices ranging from 1 to NWC. */

/*     DATAC    is a character string to be written to elements */
/*              FIRST through LAST of the specified record. The */
/*              character DATAC(1:1) is placed in record element */
/*              FIRST, the character DATAC(2) is placed in record */
/*              element FIRST+1, and so on; the character */
/*              DATAC(LAST-FIRST+1) is placed in the record element */
/*              LAST. */

/* $ Detailed_Output */

/*     None. See $Particulars for a description of the action of this */
/*     routine. */

/* $ Parameters */

/*     NWC      is the number of characters in a single DAS record */
/*              containing characters. */

/*     BUFSZC   is the number of records in the character record */
/*              buffer. */

/* $ Exceptions */

/*     1)  This routine may be used to update only records that have */
/*         already been written by DASWRC or that already exist in the */
/*         file designated by HANDLE. Attempting to update a record */
/*         that hasn't yet been written will cause the read operation */
/*         performed by this routine to fail. */

/*         If a read operation attempted by this routine fails for this */
/*         or any other reason, an error is signaled by a routine in the */
/*         call tree of this routine. The indicated record will not be */
/*         modified. */

/*     2)  If a write operation attempted by this routine fails, an error */
/*         is signaled by a routine in the call tree of this routine. The */
/*         status of the DAS file written to is uncertain in this case. */
/*         Note that the file written to may be different than the file */
/*         designated by HANDLE if multiple DAS files are open for */
/*         writing. */

/*     3)  If the input file handle is invalid, an error is signaled by a */
/*         routine in the call tree of this routine. The indicated record */
/*         will not be modified. */

/*     4)  If FIRST or LAST is not in the range [1, NWC], the error */
/*         SPICE(INDEXOUTOFRANGE) is signaled. The indicated */
/*         record will not be modified. */

/*     5)  If FIRST > LAST, this routine will return without modifying */
/*         the indicated record. */

/* $ Files */

/*     See the description of the argument HANDLE in $Detailed_Input. */

/* $ Particulars */

/*     Routines outside of SPICELIB will normally have no need to call */
/*     this routine. */

/*     This routine can be used to update any existing record in a DAS */
/*     file that is open for writing, or any record that has been */
/*     `written' by DASWRC, whether or not that record has yet been */
/*     physically written to the file it belongs to. Records that have */
/*     never been written cannot be updated. */

/*     Because the DAS system buffers records that are written, multiple */
/*     updates of parts of a record can be made without incurring a */
/*     large number of file reads and writes. */

/*     Any buffered character record can be updated with this routine. */
/*     In particular, records that have been written to the DAS character */
/*     record buffer but have not yet been written out to the DAS file */
/*     they're intended to go to ARE visible to this routine. */

/*     This routine should be used to update only records that contain */
/*     character data. */

/* $ Examples */

/*     1)  Update the 10th through 100th characters in record number 9 */
/*         in a DAS file designated by HANDLE. */

/*             CHARACTER*(100)       DATAC */

/*                         . */
/*                         . */
/*                         . */

/*             DATAC = 'The first 91 characters of this string, '      // */
/*            .        'including trailing blanks, will be written '   // */
/*            .        'to the indicated DAS file.' */

/*             CALL DASURC ( HANDLE, 9, 10, 100, DATAC ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     B.V. Semenov       (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.1, 22-FEB-2021 (JDR) */

/*        Edited the header to comply with NAIF standard. */

/* -    SPICELIB Version 2.0.0, 05-FEB-2015 (NJB) */

/*        Upgraded to support handle manager integration. */

/* -    SPICELIB Version 1.0.3, 10-FEB-2014 (BVS) */

/*        Added description of NWC to the $Parameters and $Brief_I/O */
/*        sections of the header. */

/* -    SPICELIB Version 1.0.2, 03-NOV-1995 (NJB) */

/*        Removed weird spaces from ENTRY statement. */

/* -    SPICELIB Version 1.0.1, 28-OCT-1993 (KRG) */

/*        Removed references to specific DAS file open routines in the */
/*        $Detailed_Input section of the header. This was done in order */
/*        to minimize documentation changes if the DAS open routines ever */
/*        change. */

/* -    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT) */

/* -& */
/* $ Index_Entries */

/*     update DAS character physical records */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.0.1, 28-OCT-1993 (KRG) */

/*        Removed references to specific DAS file open routines in the */
/*        $Detailed_Input section of the header. This was done in order */
/*        to minimize documentation changes if the DAS open routines ever */
/*        change. */

/* -    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT) */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("DASURC", (ftnlen)6);

/*     Check that the file is open for writing.  Signal an error if not. */

    dassih_(handle, "WRITE", (ftnlen)5);
    if (failed_()) {
	chkout_("DASURC", (ftnlen)6);
	return 0;
    }

/*     If it hasn't been done yet, initialize the pointer list pools. */

    if (pass1) {
	lnkini_(&c__10, poold);
	lnkini_(&c__10, pooli);
	lnkini_(&c__10, poolc);
	pass1 = FALSE_;
    }

/*     If FIRST or LAST are out of range, no dice. */

    if (*first < 1 || *first > 1024 || *last < 1 || *last > 1024) {
	setmsg_("String indices FIRST and LAST were #,  #; allowed range for"
		" both is [#, #]. File was #, record number was #.", (ftnlen)
		108);
	errint_("#", first, (ftnlen)1);
	errint_("#", last, (ftnlen)1);
	errint_("#", &c__1, (ftnlen)1);
	errint_("#", &c__1024, (ftnlen)1);
	errhan_("#", handle, (ftnlen)1);
	errint_("#", recno, (ftnlen)1);
	sigerr_("SPICE(INDEXOUTOFRANGE)", (ftnlen)22);
	chkout_("DASURC", (ftnlen)6);
	return 0;
    }

/*     There's nothing to do if LAST < FIRST. */

    if (*last < *first) {
	chkout_("DASURC", (ftnlen)6);
	return 0;
    }

/*     See whether character record number RECNO from file HANDLE is */
/*     buffered.  We'll search through the list of buffered records */
/*     starting at the head of the list.  If the record is already */
/*     buffered, we'll update the buffer entry, but we'll defer writing */
/*     the record out until we need to free a record, or until the */
/*     character buffer is flushed, whichever comes first. */

    node = headc;
    while(node > 0) {
	if (*handle == hnbufc[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : 
		s_rnge("hnbufc", i__1, "dasrwr_", (ftnlen)3901)] && *recno == 
		rnbufc[(i__2 = node - 1) < 10 && 0 <= i__2 ? i__2 : s_rnge(
		"rnbufc", i__2, "dasrwr_", (ftnlen)3901)]) {

/*           Found it.  Update the buffered record. */

	    s_copy(rcbufc + ((((i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : 
		    s_rnge("rcbufc", i__1, "dasrwr_", (ftnlen)3906)) << 10) + 
		    (*first - 1)), datac, *last - (*first - 1), datac_len);

/*           Set the update flag, indicating that this buffer entry */
/*           has been modified. */

	    upbufc[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("upbu"
		    "fc", i__1, "dasrwr_", (ftnlen)3912)] = TRUE_;

/*           Put the information about this record at the head of the */
/*           active list, if it is not already there. */

	    if (node != headc) {
		lnkxsl_(&node, &node, poolc);
		lnkilb_(&node, &headc, poolc);
		headc = node;
	    }
	    chkout_("DASURC", (ftnlen)6);
	    return 0;
	}
	node = poolc[(i__1 = (node << 1) + 10) < 32 && 0 <= i__1 ? i__1 : 
		s_rnge("poolc", i__1, "dasrwr_", (ftnlen)3931)];
    }

/*     The record we're writing to is not buffered.  We'll allocate */
/*     a buffer entry.  If the record buffer is full, we'll */
/*     commandeer the least recently accessed record.  Before using */
/*     this record, we'll write its contents out to the corresponding */
/*     file, if the record has been updated. */

    if (usedc < 10) {

/*        There's a free buffer entry available.  Just allocate it. */

	lnkan_(poolc, &node);
	++usedc;
    } else {

/*        Grab the buffer entry at the tail end of the list. */

	node = lnktl_(&headc, poolc);
	lnkxsl_(&node, &node, poolc);

/*        If the allocated record was updated, write it out. */

	if (upbufc[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("upbu"
		"fc", i__1, "dasrwr_", (ftnlen)3960)]) {
	    zzddhhlu_(&hnbufc[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : 
		    s_rnge("hnbufc", i__1, "dasrwr_", (ftnlen)3963)], "DAS", &
		    c_false, &wrunit, (ftnlen)3);
	    dasioc_("WRITE", &wrunit, &rnbufc[(i__1 = node - 1) < 10 && 0 <= 
		    i__1 ? i__1 : s_rnge("rnbufc", i__1, "dasrwr_", (ftnlen)
		    3965)], rcbufc + (((i__2 = node - 1) < 10 && 0 <= i__2 ? 
		    i__2 : s_rnge("rcbufc", i__2, "dasrwr_", (ftnlen)3965)) <<
		     10), (ftnlen)5, (ftnlen)1024);
	    if (failed_()) {
		chkout_("DASURC", (ftnlen)6);
		return 0;
	    }
	}
    }

/*     Now try to read the record we're going to update. */

    zzddhhlu_(handle, "DAS", &c_false, &unit, (ftnlen)3);
    dasioc_("READ", &unit, recno, rcbufc + (((i__1 = node - 1) < 10 && 0 <= 
	    i__1 ? i__1 : s_rnge("rcbufc", i__1, "dasrwr_", (ftnlen)3984)) << 
	    10), (ftnlen)4, (ftnlen)1024);
    if (failed_()) {
	chkout_("DASURC", (ftnlen)6);
	return 0;
    }

/*     The read was successful, so set the record number, handle, */
/*     and update flag for this buffer entry, and link these buffer */
/*     entries in before the current head of the list, thus putting */
/*     them at the head. */

/*     Update the head pointer. */

    lnkilb_(&node, &headc, poolc);
    hnbufc[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("hnbufc", i__1,
	     "dasrwr_", (ftnlen)4001)] = *handle;
    rnbufc[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("rnbufc", i__1,
	     "dasrwr_", (ftnlen)4002)] = *recno;
    upbufc[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("upbufc", i__1,
	     "dasrwr_", (ftnlen)4003)] = TRUE_;
    headc = node;

/*     At long last, make the requested update.  Note that we don't */
/*     have to write the record back to the file; that will get done */
/*     automatically before or at the time the file is closed. */

    s_copy(rcbufc + ((((i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge(
	    "rcbufc", i__1, "dasrwr_", (ftnlen)4011)) << 10) + (*first - 1)), 
	    datac, *last - (*first - 1), datac_len);
    chkout_("DASURC", (ftnlen)6);
    return 0;
/* $Procedure DASWBR ( DAS, write buffered records ) */

L_daswbr:
/* $ Abstract */

/*     Write out all buffered records of a specified DAS file. */

/* $ Disclaimer */

/*     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE */
/*     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S. */
/*     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE */
/*     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE */
/*     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS" */
/*     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY */
/*     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A */
/*     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC */
/*     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE */
/*     SOFTWARE AND RELATED MATERIALS, HOWEVER USED. */

/*     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA */
/*     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT */
/*     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND, */
/*     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS, */
/*     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE */
/*     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY. */

/*     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF */
/*     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY */
/*     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE */
/*     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE. */

/* $ Required_Reading */

/*     DAS */

/* $ Keywords */

/*     ASSIGNMENT */
/*     DAS */
/*     FILES */

/* $ Declarations */

/*     INTEGER               HANDLE */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Handle of DAS file. */

/* $ Detailed_Input */

/*     HANDLE   is the handle of a DAS file opened for writing. */

/* $ Detailed_Output */

/*     None. */

/*     See $Particulars for a description of the action of this routine. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the input file handle is invalid, an error is signaled */
/*         by a routine in the call tree of this routine. The indicated */
/*         file will not be modified. */

/*     2)  If a write operation attempted by this routine fails, an */
/*         error is signaled by a routine in the call tree of this */
/*         routine. The status of the DAS file written to is uncertain */
/*         in this case. */

/* $ Files */

/*     See the description of the argument HANDLE in $Detailed_Input. */

/* $ Particulars */

/*     This routine writes buffered records out to the DAS file to which */
/*     they correspond. */

/*     Because the DAS system buffers records that are written as well as */
/*     those that are read, data supplied to the DAS add data (DASADC, */
/*     DASADD, DASADI) and DAS update data (DASUDC, DASUDD, DASUDI) */
/*     routines on input has not necessarily been physically written to */
/*     the DAS file specified by the caller of those routines, at the */
/*     time those routines return. Before closing a DAS file that has */
/*     been opened for writing, the DAS system must write out to the file */
/*     any updated records present in the DAS buffers. The SPICELIB */
/*     routine DASCLS uses this routine to perform this function. The */
/*     routines DASAC and DASDC, through the use of the SPICELIB routines */
/*     DASACR and DASRCR, which respectively add comment records to or */
/*     delete comment records from a DAS file, use this routine to ensure */
/*     that the SPICELIB routine DASRWR record buffers don't become out */
/*     of sync with the file they operate upon. */

/*     In addition, this routine can be used by application programs */
/*     that create or update DAS files. The reason for calling this */
/*     routine directly would be to provide a measure of safety when */
/*     writing a very large file: if the file creation or update were */
/*     interrupted, the amount of work lost due to the loss of buffered, */
/*     unwritten records could be reduced. */

/*     However, routines outside of SPICELIB will generally not need to */
/*     call this routine directly. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Write a DAS file by adding data to it over multiple passes. */
/*        Avoid spending time on file segregation between writes. */

/*        Each pass opens the file, adds character, double precision, */
/*        and integer data to the file, writes out buffered data by */
/*        calling DASWBR, and closes the file without segregating the */
/*        data by calling DASLLC. */

/*        The program also checks the file: after the final write, */
/*        the program reads the data and compares it to expected values. */

/*        Note that most user-oriented applications should segregate a */
/*        DAS file after writing it, since this greatly enhances file */
/*        reading efficiency. The technique demonstrated here may be */
/*        useful for cases in which a file will be written via many */
/*        small data additions, and in which the file is read between */
/*        write operations. */


/*        Example code begins here. */


/*              PROGRAM DASWBR_EX1 */
/*              IMPLICIT NONE */

/*        C */
/*        C     Local parameters */
/*        C */
/*              INTEGER               FILSIZ */
/*              PARAMETER           ( FILSIZ = 255 ) */

/*              INTEGER               FTYPLN */
/*              PARAMETER           ( FTYPLN = 3 ) */

/*              INTEGER               CHRLEN */
/*              PARAMETER           ( CHRLEN = 50 ) */

/*              INTEGER               IBUFSZ */
/*              PARAMETER           ( IBUFSZ = 20 ) */

/*              INTEGER               DBUFSZ */
/*              PARAMETER           ( DBUFSZ = 30 ) */

/*        C */
/*        C     Local variables */
/*        C */
/*              CHARACTER*(CHRLEN)    CHRBUF */
/*              CHARACTER*(FILSIZ)    FNAME */
/*              CHARACTER*(FTYPLN)    FTYPE */
/*              CHARACTER*(CHRLEN)    XCHRBF */

/*              DOUBLE PRECISION      DPBUF  ( DBUFSZ ) */
/*              DOUBLE PRECISION      XDPBUF ( DBUFSZ ) */

/*              INTEGER               FIRSTC */
/*              INTEGER               FIRSTD */
/*              INTEGER               FIRSTI */
/*              INTEGER               HANDLE */
/*              INTEGER               I */
/*              INTEGER               INTBUF ( IBUFSZ ) */
/*              INTEGER               J */
/*              INTEGER               LASTC */
/*              INTEGER               LASTD */
/*              INTEGER               LASTI */
/*              INTEGER               NCALL */
/*              INTEGER               NCOMR */
/*              INTEGER               NPASS */
/*              INTEGER               PASSNO */
/*              INTEGER               XINTBF ( IBUFSZ ) */


/*        C */
/*        C     Initial values */
/*        C */
/*              DATA                  FNAME  / 'daswbr_ex1.das' / */
/*              DATA                  FTYPE  / 'ANG' / */
/*              DATA                  NCALL  / 1000  / */
/*              DATA                  NCOMR  / 10    / */
/*              DATA                  NPASS  / 3     / */

/*        C */
/*        C     Open a new DAS file. We'll allocate NCOMR records */
/*        C     for comments. The file type is not one of the standard */
/*        C     types recognized by SPICE; however it can be used to */
/*        C     ensure the database file is of the correct type. */
/*        C */
/*        C     We'll use the file name as the internal file name. */
/*        C */
/*              CALL DASONW ( FNAME, FTYPE, FNAME, NCOMR, HANDLE ) */

/*        C */
/*        C     Add data of character, integer, and double precision */
/*        C     types to the file in interleaved fashion. We'll add to */
/*        C     the file over NPASS "passes," in each of which we close */
/*        C     the file after writing. */
/*        C */
/*              DO PASSNO = 1, NPASS */

/*                 IF ( PASSNO .GT. 1 ) THEN */

/*                    WRITE (*,*) 'Opening file for write access...' */

/*                    CALL DASOPW( FNAME, HANDLE ) */

/*                 END IF */

/*                 DO I = 1, NCALL */
/*        C */
/*        C           Add string data to the file. */
/*        C */
/*                    CHRBUF = 'Character value #' */
/*                    CALL REPMI( CHRBUF, '#', I, CHRBUF ) */

/*                    CALL DASADC ( HANDLE, CHRLEN, 1, CHRLEN, CHRBUF ) */

/*        C */
/*        C           Add double precision data to the file. */
/*        C */
/*                    DO J = 1, DBUFSZ */
/*                       DPBUF(J) = DBLE( 100000000*PASSNO + 100*I + J ) */
/*                    END DO */

/*                    CALL DASADD ( HANDLE, DBUFSZ, DPBUF ) */

/*        C */
/*        C           Add integer data to the file. */
/*        C */
/*                    DO J = 1, IBUFSZ */
/*                       INTBUF(J) = 100000000*PASSNO  +  100 * I  +  J */
/*                    END DO */

/*                    CALL DASADI ( HANDLE, IBUFSZ, INTBUF ) */

/*                 END DO */

/*        C */
/*        C        Write buffered data to the file. */
/*        C */
/*                 WRITE (*,*) 'Writing buffered data...' */
/*                 CALL DASWBR ( HANDLE ) */

/*        C */
/*        C        Close the file without segregating it. */
/*        C */
/*                 WRITE (*,*) 'Closing DAS file...' */
/*                 CALL DASLLC ( HANDLE ) */

/*              END DO */

/*              WRITE (*,*) 'File write is done.' */

/*        C */
/*        C     Check file contents. */
/*        C */
/*              CALL DASOPR( FNAME, HANDLE ) */

/*        C */
/*        C     Read data from the file; compare to expected values. */
/*        C */
/*        C     Initialize end addresses. */
/*        C */
/*              LASTC = 0 */
/*              LASTD = 0 */
/*              LASTI = 0 */

/*              DO PASSNO = 1, NPASS */

/*                 DO I = 1, NCALL */

/*        C */
/*        C           Check string data. */
/*        C */
/*                    XCHRBF = 'Character value #' */
/*                    CALL REPMI( XCHRBF, '#', I, XCHRBF ) */

/*                    FIRSTC = LASTC + 1 */
/*                    LASTC  = LASTC + CHRLEN */

/*                    CALL DASRDC ( HANDLE, FIRSTC, LASTC, */
/*             .                    1,      CHRLEN, CHRBUF ) */

/*                    IF ( CHRBUF .NE. XCHRBF ) THEN */
/*                       WRITE (*,*) 'Character data mismatch: ' */
/*                       WRITE (*,*) 'PASS     = ', PASSNO */
/*                       WRITE (*,*) 'I        = ', I */
/*                       WRITE (*,*) 'Expected = ', XCHRBF */
/*                       WRITE (*,*) 'Actual   = ', CHRBUF */
/*                       STOP */
/*                    END IF */

/*        C */
/*        C           Check double precision data. */
/*        C */
/*                    DO J = 1, DBUFSZ */
/*                       XDPBUF(J) = DBLE(   100000000*PASSNO */
/*             .                           + 100*I + J        ) */
/*                    END DO */

/*                    FIRSTD = LASTD + 1 */
/*                    LASTD  = LASTD + DBUFSZ */

/*                    CALL DASRDD ( HANDLE, FIRSTD, LASTD, DPBUF ) */

/*                    DO J = 1, DBUFSZ */

/*                       IF ( DPBUF(J) .NE. XDPBUF(J) ) THEN */

/*                          WRITE (*,*) */
/*             .                   'Double precision data mismatch: ' */
/*                          WRITE (*,*) 'PASS     = ', PASSNO */
/*                          WRITE (*,*) 'I        = ', I */
/*                          WRITE (*,*) 'J        = ', J */
/*                          WRITE (*,*) 'Expected = ', XDPBUF(J) */
/*                          WRITE (*,*) 'Actual   = ', DPBUF(J) */
/*                          STOP */

/*                       END IF */

/*                    END DO */

/*        C */
/*        C           Check integer data. */
/*        C */
/*                    DO J = 1, IBUFSZ */
/*                       XINTBF(J) = 100000000*PASSNO  +  100 * I  +  J */
/*                    END DO */

/*                    FIRSTI = LASTI + 1 */
/*                    LASTI  = LASTI + IBUFSZ */

/*                    CALL DASRDI ( HANDLE, FIRSTI, LASTI, INTBUF ) */

/*                    DO J = 1, IBUFSZ */

/*                       IF ( INTBUF(J) .NE. XINTBF(J) ) THEN */

/*                          WRITE (*,*) 'Integer data mismatch: ' */
/*                          WRITE (*,*) 'PASS     = ', PASSNO */
/*                          WRITE (*,*) 'I        = ', I */
/*                          WRITE (*,*) 'J        = ', J */
/*                          WRITE (*,*) 'Expected = ', XINTBF(J) */
/*                          WRITE (*,*) 'Actual   = ', INTBUF(J) */
/*                          STOP */

/*                       END IF */

/*                    END DO */

/*                 END DO */

/*              END DO */

/*              WRITE (*,*) 'File check is done.' */

/*        C */
/*        C     Close the file. */
/*        C */
/*              CALL DASCLS ( HANDLE ) */

/*              END */


/*        When this program was executed on a Mac/Intel/gfortran/64-bit */
/*        platform, the output was: */


/*         Writing buffered data... */
/*         Closing DAS file... */
/*         Opening file for write access... */
/*         Writing buffered data... */
/*         Closing DAS file... */
/*         Opening file for write access... */
/*         Writing buffered data... */
/*         Closing DAS file... */
/*         File write is done. */
/*         File check is done. */


/*        Note that after run completion, a new DAS file exists in the */
/*        output directory. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */
/*     J. Diaz del Rio    (ODC Space) */
/*     K.R. Gehringer     (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.1, 19-MAY-2021 (NJB) (JDR) */

/*        Edited the header to comply with NAIF standard. Added complete */
/*        code example. */

/*        Updated $Particulars in order to provide information about the */
/*        high level APIs that actually use this routine. */

/* -    SPICELIB Version 2.0.0, 30-JUL-2014 (NJB) */

/*        Upgraded to support handle manager integration. */

/* -    SPICELIB Version 1.0.2, 03-NOV-1995 (NJB) */

/*        Removed weird spaces from ENTRY statement. */

/* -    SPICELIB Version 1.0.1, 28-OCT-1993 (KRG) */

/*        Removed references to specific DAS file open routines in the */
/*        $Detailed_Input section of the header. This was done in order */
/*        to minimize documentation changes if the DAS open routines ever */
/*        change. */

/* -    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT) */

/* -& */
/* $ Index_Entries */

/*     write buffered records to a DAS file */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.0.1, 28-OCT-1993 (KRG) */

/*        Removed references to specific DAS file open routines in the */
/*        $Detailed_Input section of the header. This was done in order */
/*        to minimize documentation changes if the DAS open routines ever */
/*        change. */

/* -    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT) */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("DASWBR", (ftnlen)6);

/*     Check that the file is open for writing.  Signal an error if not. */

    dassih_(handle, "WRITE", (ftnlen)5);
    if (failed_()) {
	chkout_("DASWBR", (ftnlen)6);
	return 0;
    }

/*     If it hasn't been done yet, initialize the pointer list pools. */

    if (pass1) {
	lnkini_(&c__10, poold);
	lnkini_(&c__10, pooli);
	lnkini_(&c__10, poolc);
	pass1 = FALSE_;
    }

/*     Obtain a logical unit for HANDLE. */

    zzddhhlu_(handle, "DAS", &c_false, &wrunit, (ftnlen)3);
    if (failed_()) {
	chkout_("DASWBR", (ftnlen)6);
	return 0;
    }

/*     For each buffer, find the records belonging to this file, and */
/*     write them out to the file. */

/*     Double precision records first. */

    node = headd;
    while(node > 0) {
	if (*handle == hnbufd[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : 
		s_rnge("hnbufd", i__1, "dasrwr_", (ftnlen)4540)]) {

/*           This record belongs to the file of interest, so write the */
/*           the record out. */

	    dasiod_("WRITE", &wrunit, &rnbufd[(i__1 = node - 1) < 10 && 0 <= 
		    i__1 ? i__1 : s_rnge("rnbufd", i__1, "dasrwr_", (ftnlen)
		    4545)], &rcbufd[(i__2 = (node << 7) - 128) < 1280 && 0 <= 
		    i__2 ? i__2 : s_rnge("rcbufd", i__2, "dasrwr_", (ftnlen)
		    4545)], (ftnlen)5);
	    if (failed_()) {
		chkout_("DASWBR", (ftnlen)6);
		return 0;
	    }

/*           The record is no longer in use; return it to the */
/*           free list.  But grab the successor first.  Update */
/*           the head of the list, if the node we're freeing is */
/*           the head node.  Decrement the number of used d.p. */
/*           buffer elements. */

	    next = poold[(i__1 = (node << 1) + 10) < 32 && 0 <= i__1 ? i__1 : 
		    s_rnge("poold", i__1, "dasrwr_", (ftnlen)4562)];
	    if (node == headd) {
		headd = next;
	    }
	    lnkfsl_(&node, &node, poold);
	    node = next;
	    --usedd;
	} else {

/*           Just get the next node. */

	    node = poold[(i__1 = (node << 1) + 10) < 32 && 0 <= i__1 ? i__1 : 
		    s_rnge("poold", i__1, "dasrwr_", (ftnlen)4577)];
	}
    }

/*     Next, integer records. */

    node = headi;
    while(node > 0) {
	if (*handle == hnbufi[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : 
		s_rnge("hnbufi", i__1, "dasrwr_", (ftnlen)4592)]) {

/*           This record belongs to the file of interest, so write the */
/*           the record out. */

	    dasioi_("WRITE", &wrunit, &rnbufi[(i__1 = node - 1) < 10 && 0 <= 
		    i__1 ? i__1 : s_rnge("rnbufi", i__1, "dasrwr_", (ftnlen)
		    4597)], &rcbufi[(i__2 = (node << 8) - 256) < 2560 && 0 <= 
		    i__2 ? i__2 : s_rnge("rcbufi", i__2, "dasrwr_", (ftnlen)
		    4597)], (ftnlen)5);
	    if (failed_()) {
		chkout_("DASWBR", (ftnlen)6);
		return 0;
	    }

/*           The record is no longer in use; return it to the */
/*           free list.  But grab the successor first.  Update */
/*           the head of the list, if the node we're freeing is */
/*           the head node.  Decrement the number of used integer */
/*           buffer elements. */

	    next = pooli[(i__1 = (node << 1) + 10) < 32 && 0 <= i__1 ? i__1 : 
		    s_rnge("pooli", i__1, "dasrwr_", (ftnlen)4614)];
	    if (node == headi) {
		headi = next;
	    }
	    lnkfsl_(&node, &node, pooli);
	    node = next;
	    --usedi;
	} else {

/*           Just get the next node. */

	    node = pooli[(i__1 = (node << 1) + 10) < 32 && 0 <= i__1 ? i__1 : 
		    s_rnge("pooli", i__1, "dasrwr_", (ftnlen)4629)];
	}
    }

/*     And last, character records. */

    node = headc;
    while(node > 0) {
	if (*handle == hnbufc[(i__1 = node - 1) < 10 && 0 <= i__1 ? i__1 : 
		s_rnge("hnbufc", i__1, "dasrwr_", (ftnlen)4644)]) {

/*           This record belongs to the file of interest, so write the */
/*           the record out. */

	    dasioc_("WRITE", &wrunit, &rnbufc[(i__1 = node - 1) < 10 && 0 <= 
		    i__1 ? i__1 : s_rnge("rnbufc", i__1, "dasrwr_", (ftnlen)
		    4649)], rcbufc + (((i__2 = node - 1) < 10 && 0 <= i__2 ? 
		    i__2 : s_rnge("rcbufc", i__2, "dasrwr_", (ftnlen)4649)) <<
		     10), (ftnlen)5, (ftnlen)1024);
	    if (failed_()) {
		chkout_("DASWBR", (ftnlen)6);
		return 0;
	    }

/*           The record is no longer in use; return it to the */
/*           free list.  But grab the successor first.  Update */
/*           the head of the list, if the node we're freeing is */
/*           the head node.  Decrement the number of used character */
/*           buffer elements. */

	    next = poolc[(i__1 = (node << 1) + 10) < 32 && 0 <= i__1 ? i__1 : 
		    s_rnge("poolc", i__1, "dasrwr_", (ftnlen)4666)];
	    if (node == headc) {
		headc = next;
	    }
	    lnkfsl_(&node, &node, poolc);
	    node = next;
	    --usedc;
	} else {

/*           Just get the next node. */

	    node = poolc[(i__1 = (node << 1) + 10) < 32 && 0 <= i__1 ? i__1 : 
		    s_rnge("poolc", i__1, "dasrwr_", (ftnlen)4681)];
	}
    }
    chkout_("DASWBR", (ftnlen)6);
    return 0;
} /* dasrwr_ */

/* Subroutine */ int dasrwr_(integer *handle, integer *recno, char *recc, 
	doublereal *recd, integer *reci, integer *first, integer *last, 
	doublereal *datad, integer *datai, char *datac, ftnlen recc_len, 
	ftnlen datac_len)
{
    return dasrwr_0_(0, handle, recno, recc, recd, reci, first, last, datad, 
	    datai, datac, recc_len, datac_len);
    }

/* Subroutine */ int dasrrd_(integer *handle, integer *recno, integer *first, 
	integer *last, doublereal *datad)
{
    return dasrwr_0_(1, handle, recno, (char *)0, (doublereal *)0, (integer *)
	    0, first, last, datad, (integer *)0, (char *)0, (ftnint)0, (
	    ftnint)0);
    }

/* Subroutine */ int dasrri_(integer *handle, integer *recno, integer *first, 
	integer *last, integer *datai)
{
    return dasrwr_0_(2, handle, recno, (char *)0, (doublereal *)0, (integer *)
	    0, first, last, (doublereal *)0, datai, (char *)0, (ftnint)0, (
	    ftnint)0);
    }

/* Subroutine */ int dasrrc_(integer *handle, integer *recno, integer *first, 
	integer *last, char *datac, ftnlen datac_len)
{
    return dasrwr_0_(3, handle, recno, (char *)0, (doublereal *)0, (integer *)
	    0, first, last, (doublereal *)0, (integer *)0, datac, (ftnint)0, 
	    datac_len);
    }

/* Subroutine */ int daswrd_(integer *handle, integer *recno, doublereal *
	recd)
{
    return dasrwr_0_(4, handle, recno, (char *)0, recd, (integer *)0, (
	    integer *)0, (integer *)0, (doublereal *)0, (integer *)0, (char *)
	    0, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int daswri_(integer *handle, integer *recno, integer *reci)
{
    return dasrwr_0_(5, handle, recno, (char *)0, (doublereal *)0, reci, (
	    integer *)0, (integer *)0, (doublereal *)0, (integer *)0, (char *)
	    0, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int daswrc_(integer *handle, integer *recno, char *recc, 
	ftnlen recc_len)
{
    return dasrwr_0_(6, handle, recno, recc, (doublereal *)0, (integer *)0, (
	    integer *)0, (integer *)0, (doublereal *)0, (integer *)0, (char *)
	    0, recc_len, (ftnint)0);
    }

/* Subroutine */ int dasurd_(integer *handle, integer *recno, integer *first, 
	integer *last, doublereal *datad)
{
    return dasrwr_0_(7, handle, recno, (char *)0, (doublereal *)0, (integer *)
	    0, first, last, datad, (integer *)0, (char *)0, (ftnint)0, (
	    ftnint)0);
    }

/* Subroutine */ int dasuri_(integer *handle, integer *recno, integer *first, 
	integer *last, integer *datai)
{
    return dasrwr_0_(8, handle, recno, (char *)0, (doublereal *)0, (integer *)
	    0, first, last, (doublereal *)0, datai, (char *)0, (ftnint)0, (
	    ftnint)0);
    }

/* Subroutine */ int dasurc_(integer *handle, integer *recno, integer *first, 
	integer *last, char *datac, ftnlen datac_len)
{
    return dasrwr_0_(9, handle, recno, (char *)0, (doublereal *)0, (integer *)
	    0, first, last, (doublereal *)0, (integer *)0, datac, (ftnint)0, 
	    datac_len);
    }

/* Subroutine */ int daswbr_(integer *handle)
{
    return dasrwr_0_(10, handle, (integer *)0, (char *)0, (doublereal *)0, (
	    integer *)0, (integer *)0, (integer *)0, (doublereal *)0, (
	    integer *)0, (char *)0, (ftnint)0, (ftnint)0);
    }

