// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include <stdio.h>
#include <string.h>

int Date2Int (char *date)
{
	static char *mstr[12] = {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};
	char ms[32];
	int day, month, year, v;
	sscanf (date, "%s%d%d", ms, &day, &year);
	for (month = 0; month < 12; month++)
		if (!_strnicmp (ms, mstr[month], 3)) break;
	v = (year%100)*10000 + (month+1)*100 + day;
	return v;
}


int main (int argc, char *argv[])
{
	int dateint = Date2Int (__DATE__);
	printf ("%06d\n", dateint);
	return 0;
}