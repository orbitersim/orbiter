#include "Convert.h"

// transforms a date and time (UT) into MJD format
double date2mjd (struct tm *date)
{
  int b;
  int y = date->tm_year+1900;
  int m = date->tm_mon;
  int d = date->tm_mday;
  double a = 10000.0*y + 100.0*m + d;
  if (m <= 2) m += 12, y--;
  if (a <= 15821004.1) b = (y+4716)/4 - 1181;
  else                 b = y/400 - y/100 + y/4;
  return 365.0*y - 679004.0 + b + (int)(30.6001*(m+1)) + d +
	  date->tm_hour/24.0 + date->tm_min/1440.0 + date->tm_sec/86400.0;
}

// transforms a MJD value into date and time structures (GMT, if MJD had no offsets)
struct tm *mjddate (double mjd)
{
	static struct tm date;
	double ijd, c, e, h;
	int a, b, f;

	h = 24.0 * modf (mjd, &ijd);
	if (ijd < -100840) {
		c = ijd + 2401525.0;
	} else {
		b = (int)((ijd + 532784.75) / 36524.25);
		c = ijd + 2401526.0 + (b - b/4);
	}
	a = (int)((c-122.1)/365.25);
	e = 365.0 * a + a/4;
	f = (int)((c-e)/30.6001);
	date.tm_wday  = ((int)mjd + 3) % 7;
	date.tm_mday  = (int)(c-e+0.5) - (int)(30.6001*f);
	date.tm_mon   = f-1 - 12 * (f/14);
	date.tm_year  = a-4715 - ((7 + date.tm_mon)/10) - 1900;
	date.tm_hour  = (int)h;
	date.tm_min   = (int)(h = 60.0 * (h - date.tm_hour));
	date.tm_sec   = (int)(h = 60.0 * (h - date.tm_min));
	date.tm_isdst = 0;
	return &date;
}

char *DateStr (double mjd)
{
	static char datestr[256];
	static char wdaystr[7][4] = {"Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"};
	static char monstr[13][4] = {"","Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};

	double ijd, c, e, h;
	int a, b, f, wday, mday, mon, year, hour, min, sec;

	h = 24.0 * modf (mjd, &ijd);
	if (ijd < -100840) {
		c = ijd + 2401525.0;
	} else {
		b = (int)((ijd + 532784.75) / 36524.25);
		c = ijd + 2401526.0 + (b - b/4);
	}
	a = (int)((c-122.1)/365.25);
	e = 365.0 * a + a/4;
	f = (int)((c-e)/30.6001);
	wday  = ((int)mjd + 3) % 7;
	mday  = (int)(c-e+0.5) - (int)(30.6001*f);
	mon   = f-1 - 12 * (f/14);
	year  = a-4715 - ((7 + mon)/10);
	hour  = (int)h;
	min   = (int)(h = 60.0 * (h - hour));
	sec   = (int)(h = 60.0 * (h - min));
	
	sprintf (datestr, "%s %s %02d %02d:%02d:%02d %d", wdaystr[wday], monstr[mon], mday,
		hour, min, sec, year);
	return datestr;
}
