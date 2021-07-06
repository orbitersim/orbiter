#ifndef __CONVERT_H
#define __CONVERT_H

#include <time.h>
#include <math.h>
#include <stdio.h>

const double day = (1.0/86400.0);
// Julian days/second

inline double Day (double sec)
{ return sec * day; }
// Convert a time interval from seconds to days

inline double JD (time_t t)
{ return 2440587.5 + (double)t * day; }
// Return Julian Date (JD) for time "t" as returned by the "time"
// function (seconds since 00:00 January 1970 UTC)

inline double MJD (time_t t)
{ return 40587.0 + (double)t * day; }
// Return Modified Julian Date (MJD) for time "t" as returned
// by the "time" function (seconds since 00:00 January 1970 UTC)

inline double Jepoch2MJD (double J)
{ return (J-2000.0)*365.25 + 51544.5; }
// Convert a Julian epoch J into a Modified Julian Date

inline double MJD2Jepoch (double mjd)
{ return 2000.0 + (mjd-51544.5)/365.25; }
// Convert a Modified Julian Date mjd to a Julian epoch

inline double JC2MJD (double jc)
{ return jc*36525.0+51544.5; }
// Convert Julian Century for equinopticum 2000 into Modified Julian Date

inline double MJD2JC (double mjd)
{ return (mjd-51544.5)/36525.0; }
// Convert Modified Julian Date mjd to Julian Century for equinopticum 2000

double date2mjd (struct tm *date);
// Convert a date and time (UT) into MJD format

struct tm *mjddate (double mjd);
// Converts MJD value into UT and returns in tm structure

char *DateStr (double mjd);

#endif // !__CONVERT_H