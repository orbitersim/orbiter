// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include <math.h>
#include <stdio.h>
#include <string.h>
#include <fstream>
#include "Astro.h"
#include "Vecmat.h"

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
	int a, f;

	h = 24.0 * modf (mjd, &ijd);
	if (ijd < -100840) {
		c = ijd + 2401525.0;
	} else {
		int b = (int)((ijd + 532784.75) / 36524.25);
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
	int a, f, wday, mday, mon, year, hour, min, sec;

	h = 24.0 * modf (mjd, &ijd);
	if (ijd < -100840) {
		c = ijd + 2401525.0;
	} else {
		int b = (int)((ijd + 532784.75) / 36524.25);
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

double Obliquity (double jc)
{
	return 0.4090928042 - (2.269655248e-4 + (2.860400719e-9 - 8.789672039e-9*jc)*jc)*jc;
}

void Equ2Ecl (double cosob, double sinob, double ra, double dc, double &l, double &b)
{
	double sinra = sin (ra), cosra = cos (ra);
	double sindc = sin (dc), cosdc = cos (dc);
	l = atan2 (sinra*cosdc*cosob + sindc*sinob, cosra*cosdc);
	b = asin  (sindc*cosob - sinra*cosdc*sinob);
}

void Ecl2Equ (double cosob, double sinob, double l, double b, double &ra, double &dc)
{
	double sinl = sin (l), cosl = cos (l);
	double sinb = sin (b), cosb = cos (b);
	ra = atan2 (sinl*cosb*cosob - sinb*sinob, cosb*cosl);
	dc = asin  (sinb*cosob + cosb*sinl*sinob);
}

void Orthodome (double lng1, double lat1, double lng2, double lat2,
				double &dist, double &dir)
{
	double A = lng2-lng1;
	double dlng = fabs(A);
	double dlat = fabs(lat2-lat1);
	if (dlat < 1e-14) {
		dist = dlng;
		dir = (lng2 > lng1 ? PI05 : 3*PI05);
		return;
	} else if (dlng < 1e-14) {
		dist = dlat;
		dir = (lat2 > lat1 ? 0.0 : PI);
	} else {
		double sinA  = sin(A),    cosA  = cos(A);
		double slat1 = sin(lat1), clat1 = cos(lat1);
		double slat2 = sin(lat2), clat2 = cos(lat2);
		double cosa  = slat2*slat1 + clat2*clat1*cosA;
		dist = acos (cosa);
		dir = atan2 (sinA*clat2, clat1*slat2 - slat1*clat2*cosA);
		if (dir < 0.0) dir += Pi2;     // 0 <= dir < 2pi
	}
}

double Orthodome (double lng1, double lat1, double lng2, double lat2)
{
	double cosA  = cos(lng2-lng1);
	double slat1 = sin(lat1), clat1 = cos(lat1);
	double slat2 = sin(lat2), clat2 = cos(lat2);
	double cosa  = slat2*slat1 + clat2*clat1*cosA;
	return acos (cosa);
}

static char strbuf[256];

char *DistStr (double dist, int precision)
{
	double absd = fabs (dist);
	if (absd < 1e4) {
		if		(absd < 1e2)  sprintf (strbuf, "% 0.*f", precision-2, dist);
		else if (absd < 1e3)  sprintf (strbuf, "% 0.*f", precision-3, dist);
		else                  sprintf (strbuf, "% 0.*fk", precision-1, dist*1e-3);
	} else if (absd < 1e7) {
		if      (absd < 1e5)  sprintf (strbuf, "% 0.*fk", precision-2, dist*1e-3);
		else if (absd < 1e6)  sprintf (strbuf, "% 0.*fk", precision-3, dist*1e-3);
		else                  sprintf (strbuf, "% 0.*fM", precision-1, dist*1e-6);
	} else if (absd < 1e10) {
		if      (absd < 1e8)  sprintf (strbuf, "% 0.*fM", precision-2, dist*1e-6);
		else if (absd < 1e9)  sprintf (strbuf, "% 0.*fM", precision-3, dist*1e-6);
		else                  sprintf (strbuf, "% 0.*fG", precision-1, dist*1e-9);
	} else if (absd < 1e2*AU) {
		if      (absd < 1e11) sprintf (strbuf, "% 0.*fG", precision-2, dist*1e-9);
		else if (absd < 1e12) sprintf (strbuf, "% 0.*fG", precision-3, dist*1e-9);
		else                  sprintf (strbuf, "% 0.*fAU",precision-2, dist*iAU);
	} else {
		sprintf (strbuf, "% 0.0fAU", dist*iAU);
		//FloatStr (dist*iparsec, precision+1);
		//strcat (strbuf, "pc");
	}
	return strbuf;
}

char *FloatStr (double f, int precision)
{
	double absf = fabs (f);
	if (absf < 1e4) {
		if		(absf < 1e2)  sprintf (strbuf, "% 0.*f", precision-2, f);
		else if (absf < 1e3)  sprintf (strbuf, "% 0.*f", precision-3, f);
		else                  sprintf (strbuf, "% 0.*fk", precision-1, f*1e-3);
	} else if (absf < 1e7) {
		if      (absf < 1e5)  sprintf (strbuf, "% 0.*fk", precision-2, f*1e-3);
		else if (absf < 1e6)  sprintf (strbuf, "% 0.*fk", precision-3, f*1e-3);
		else                  sprintf (strbuf, "% 0.*fM", precision-1, f*1e-6);
	} else if (absf < 1e10) {
		if      (absf < 1e8)  sprintf (strbuf, "% 0.*fM", precision-2, f*1e-6);
		else if (absf < 1e9)  sprintf (strbuf, "% 0.*fM", precision-3, f*1e-6);
		else                  sprintf (strbuf, "% 0.*fG", precision-1, f*1e-9);
	} else if (absf < 1e13) {
		if      (absf < 1e11) sprintf (strbuf, "% 0.*fG", precision-2, f*1e-9);
		else if (absf < 1e12) sprintf (strbuf, "% 0.*fG", precision-3, f*1e-9);
		else                  sprintf (strbuf, "% 0.*fT", precision-1, f*1e-12);
	} else {
		if      (absf < 1e14) sprintf (strbuf, "% 0.*fT", precision-2, f*1e-12);
		else if (absf < 1e15) sprintf (strbuf, "% 0.*fT", precision-3, f*1e-12);
		else                  sprintf (strbuf, "% 0.0e", f);
	}
	return strbuf;
}

char *SciStr (double f, int precision, char prefix)
{
	const char *fmtstr[3] = {
		"%0.*g", "% 0.*g", "%+0.*g"
	};
	const char *fstr = fmtstr[0];
	if (prefix == ' ') fstr = fmtstr[1];
	else if (prefix == '+') fstr = fmtstr[2];

	int i, len, e;
	sprintf (strbuf, fstr, precision, f);
	len = strlen (strbuf);
	for (i = 0; i < len; i++) {
		if (strbuf[i] == 'e') {
			sscanf(strbuf+i+1, "%d", &e);
			sprintf (strbuf+i, u8"·10^%d", e);
		}
	}
	return strbuf;
}
