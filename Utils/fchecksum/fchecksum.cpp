// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// Create a simple checksum from a file and store it in a text file
// in a format that can be #include'd by a source file.

#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include <fstream>
#include <iostream>
#include <windows.h>

int main (int argc, char *argv[])
{
	if (argc != 3) {
		std::cerr << "fchecksum: Usage: fchecksum <infile> <outfile>" << std::endl;
		return 1;
	}
	std::cout << "fchecksum\n";
	std::cout << "Reading from " << argv[1] << std::endl;
	std::cout << "Writing to   " << argv[2] << std::endl;

	BYTE sum[64];
	memset(sum, 0, 64);
	BYTE b;

	// read the image
	FILE *f = fopen(argv[1], "rb");
	int i = 0;
	if (f) {
		while (fread(&b, 1, 1, f)) {
			sum[i++ % 64] += b;
		}
		fclose(f);
	}

	// write out the checksum data in c-readable format
	std::ofstream ofs(argv[2]);
	ofs << "static const BYTE chksum[64] =\n{";
	for (i = 0; i < 64; i++) {
		int v = (int)sum[i];
		ofs << v << (i < 63 ? ',' : '}');
	}
	ofs << ';' << std::endl;
	ofs.close();

	return 0;
}
