// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include <iostream>
#include <fstream>
#include <string.h>
#include <stdio.h>

using namespace std;

char *zipper_path = "\\Program Files\\7-Zip\\7z";
char *prefix_full = "a orbiter.zip";

// List of zip-file only files
const int nzip = 1;
char *zfile[nzip] = {"vcredist\\vcredist_x86.exe"};

char *getfile(char *line)
{
	static char *SolutionDir = "\\Source\\Orbiter\\";
	static char cbuf[1024];
	char *pc, *fname;
	pc = strstr(line, "File");
	if (!pc) return NULL;
	pc = strstr(pc, "Source");
	if (!pc) return NULL;
	for (pc = pc+5; *pc!='"' && *pc != '\0'; pc++);
	if (*pc == '\0') return false;
	fname = ++pc;
	for (; *pc != '"'; pc++);
	*pc = '\0';
	if (!fname) return NULL;

	if (!strncmp (fname, "$(var.SolutionDir)", 18)) {
		strcpy (cbuf, fname+18);
	} else {
		strcpy (cbuf, fname);
	}
	return cbuf;
}

void addtolist (ofstream &ofs, char *fname) {
	ofs << fname << endl;
}

int main (int argc, char *argv[])
{
	char cbuf[1024], *fname;

	if (argc < 2) {
		cerr << "Not enough parameters!" << endl;
		return 1;
	} else {
		cout << "Reading file list from " << argv[1] << endl;
	}

	ifstream ifs(argv[1]);
	ofstream ofs("filelist.dat");

	while (ifs.getline (cbuf, 1024)) {
		if (fname = getfile (cbuf)) {
			addtolist (ofs, fname);
		}
	}
	//for (int i = 0; i < nzip; i++)
	//	addtolist(ofs, zfile[i]);

	return 0;
}