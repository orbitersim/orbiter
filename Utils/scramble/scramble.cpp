// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// Simple scrambling of text strings

#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include <fstream>
#include <iostream>

using namespace std;

char datestr[64];
char versionstr[64];

void Setup ()
{
	char *monstr[12] = {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};
	char cbuf[256];
	int mon, day, year;

	// prepare compile date string
	_strdate (cbuf);
	sscanf (cbuf, "%d/%d/%d", &mon, &day, &year);
	sprintf (datestr, "%d %s 20%02d", day, monstr[mon-1], year);

	// prepare version string
	sprintf (versionstr, "%02d%02d%02d", year, mon, day);
}

int main (int argc, char *argv[])
{
	int i, j, k, kk, key;
	char cbuf[2048];
	char obuf[4096];
	unsigned char val;
	bool scrambleall = false;
	Setup();
	ifstream ifs(argv[1]);
	ofstream ofs(argv[2]);
	cout << "Nargs = " << argc << endl;
	cout << "Reading from " << argv[1] << endl;
	cout << "Writing to   " << argv[2] << endl;
	//ofs << "// Generated with scramble.exe from " << argv[1] << std::endl;
	while (ifs.getline (cbuf, 1024)) {
		if (!strncmp (cbuf, "@@@", 3)) {
			scrambleall = !scrambleall;
			if (scrambleall) {
				key = rand() % 256, k = 0;
				obuf[0] = key;
				ofs << obuf[0];
			} else {
				ofs << '\0' << '\0';
			}
			continue;
		}
		if (scrambleall) {
			for (i = 0; cbuf[i]; i++) {
				val = (unsigned char)(k&1 ? cbuf[i]+key : cbuf[i]-key);
				sprintf (obuf+i, "%c", val);
				k++;
			}
			val = (unsigned char)(k&1 ? '\r'+key : '\r'-key);
			sprintf (obuf+i++, "%c", val);
			k++;
			val = (unsigned char)(k&1 ? '\n'+key : '\n'-key);
			sprintf (obuf+i++, "%c", val);
			k++;
			ofs << obuf;
		} else {
			i = j = 0;
parse1:
			for (; cbuf[i] && strncmp (cbuf+i, "@@", 2); i++)
				obuf[j++] = cbuf[i];
			if (!strncmp (cbuf+i, "@@", 2)) {
				key = rand() % 256;
				sprintf (obuf+j, "\"\\%03o", key); // save key as first character
				i += 2; j = strlen(obuf); k = 0;
				while (strncmp (cbuf+i, "@@", 2)) {
					if (!strncmp (cbuf+i, "__DATE__", 8)) {
						for (kk = 0; datestr[kk]; kk++) {
							val = (unsigned char)(k&1 ? datestr[kk]+key : datestr[kk]-key);
							sprintf (obuf+j, "\\%03o", val);
							j += 4; k++;
						}
						i += 8;
					} else if (!strncmp (cbuf+i, "__VERSION__", 11)) {
						for (kk = 0; versionstr[kk]; kk++) {
							val = (unsigned char)(k&1 ? versionstr[kk]+key : versionstr[kk]-key);
							sprintf (obuf+j, "\\%03o", val);
							j += 4; k++;
						}
						i += 11;
					} else {
						val = (unsigned char)(k&1 ? cbuf[i]+key : cbuf[i]-key);
						sprintf (obuf+j, "\\%03o", val);
						j += 4; i++; k++;
					}
				}
				strcpy (obuf+j, "\\000\"");
				i += 2; j += 5;
				goto parse1;
			}
			obuf[j] = '\0';
			ofs << obuf << endl;
		}
	}
	return 0;
}
