// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include <iostream>
#include <fstream>
#include <stdio.h>
#include <time.h>
#include "Mesh.h"

using namespace std;

struct Param {
	char meshname[1024];
	char outname[1024];
	char suffix[256];
	bool outlua;
};

void PrintUsage()
{
	std::cout << "Scans a mesh file and generates a header file containing mesh group\n";
	std::cout << "identifiers.\n\n";
	std::cout << "Usage: meshc /I <meshfile> /O <header file> /P <suffix> [/L]\n";
	std::cout << "  <meshfile>:    Orbiter mesh file to be scanned\n";
	std::cout << "  <header file>: Output header file name\n";
	std::cout << "  <suffix>:      Variable name suffix\n";
	std::cout << "  /L:            Optional argument, output a Lua file when provided\n\n";
	std::cout << "Any mandatory parameters not provided on the command line are queried interactively.\n\n";
}

void ParseError()
{
	std::cerr << "Error parsing command line." << std::endl;
	std::cerr << "Terminating." << std::endl;
	exit(1);
}

void ParseArgs(int argc, char *argv[], Param *param)
{
	param->meshname[0] = '\0';
	param->outname[0] = '\0';
	param->suffix[0] = '\0';
	param->outlua = false;

	for (int i = 1; i < argc; i++) {
		char *a = argv[i];

		if (a[0] != '/')
			ParseError();
		switch (a[1]) {
		case 'I':
			if (i == argc-1)
				ParseError();
			strcpy(param->meshname, argv[++i]);
			break;
		case 'O':
			if (i == argc - 1)
				ParseError();
			strcpy(param->outname, argv[++i]);
			break;
		case 'P':
			if (i == argc - 1)
				ParseError();
			strcpy(param->suffix, argv[++i]);
			break;
		case 'L':
			param->outlua = true;
			break;
		case 'H':
			PrintUsage();
			exit(0);
		}
	}
}

static void outC(const Param& param, const Mesh& mesh)
{
	char cbuf[256], label[256];
	struct tm* now;
	time_t aclock;
	time(&aclock);
	now = localtime(&aclock);

	ofstream ofs(param.outname);
	ofs << "// ========================================================\n";
	ofs << "// Mesh resource file for " << param.meshname << endl;
	ofs << "// Generated with meshc on " << asctime(now);
	ofs << "// ========================================================\n";

	ofs << "\n// Number of mesh groups:\n";
	ofs << "#define NGRP" << param.suffix << " " << mesh.nGroup() << endl;

	ofs << "\n// Number of materials:\n";
	ofs << "#define NMAT" << param.suffix << " " << mesh.nMaterial() << endl;

	ofs << "\n// Number of textures:\n";
	ofs << "#define NTEX" << param.suffix << " " << mesh.nTexture() << endl;

	ifstream ifs(param.meshname);
	int grp = 0;
	bool havelabel = false;
	while (ifs.getline(cbuf, 256)) {
		if (!_strnicmp(cbuf, "GEOM", 4))
			grp++;
		else if (!_strnicmp(cbuf, "LABEL", 5)) {
			if (!havelabel) {
				ofs << "\n// Named mesh groups:\n";
				havelabel = true;
			}
			sscanf(cbuf + 5, "%250s", label);
			ofs << "#define GRP_" << label << param.suffix << ' ' << grp << endl;
		}
	}
	ifs.close();
}
static void outLua(const Param& param, const Mesh& mesh)
{
	char cbuf[256], label[256];
	struct tm* now;
	time_t aclock;
	time(&aclock);
	now = localtime(&aclock);

	ofstream ofs(param.outname);
	ofs << "-----------------------------------------------------------\n";
	ofs << "-- Mesh resource file for " << param.meshname << endl;
	ofs << "-- Generated with meshc on " << asctime(now);
	ofs << "-- example usage:\n";
	ofs << "-- local meshres = require 'meshres.lua'\n";
	ofs << "-- print(meshres.NTEX)\n";
	ofs << "-----------------------------------------------------------\n";
	ofs << "local module={GRP={}}\n";

	ofs << "\n-- Number of mesh groups:\n";
	ofs << "module.NGRP" << param.suffix << " = " << mesh.nGroup() << endl;

	ofs << "\n-- Number of materials:\n";
	ofs << "module.NMAT" << param.suffix << " = " << mesh.nMaterial() << endl;

	ofs << "\n-- Number of textures:\n";
	ofs << "module.NTEX" << param.suffix << " = " << mesh.nTexture() << endl;

	ifstream ifs(param.meshname);
	int grp = 0;
	bool havelabel = false;
	while (ifs.getline(cbuf, 256)) {
		if (!_strnicmp(cbuf, "GEOM", 4))
			grp++;
		else if (!_strnicmp(cbuf, "LABEL", 5)) {
			if (!havelabel) {
				ofs << "\n-- Named mesh groups:\n";
				havelabel = true;
			}
			sscanf(cbuf + 5, "%250s", label);
			ofs << "module.GRP." << label << param.suffix << " = " << grp << endl;
		}
	}
	ofs << "\nreturn module\n";
	ifs.close();
}

int main (int argc, char *argv[])
{
	Mesh mesh;
	Param param;

	cout << "+-----------------------------------------------------------------------+\n";
	cout << "|                   meshc: Mesh compiler for ORBITER                    |\n";
	cout << "|        Build: " << __DATE__ << "      (c) 2001-2018 Martin Schweiger         |\n";
	cout << "+-----------------------------------------------------------------------+\n\n";

	ParseArgs(argc, argv, &param);

	if (!param.meshname[0]) {
		cout << "Mesh file name:\n";
		cout << ">> ";
		cin >> param.meshname;
		cout << endl;
	}

	if (!param.suffix[0]) {
		cout << "Variable suffix ('-' for none):\n>> ";
		cin >> param.suffix;
		cout << endl;
	}
	if (!strcmp(param.suffix, "-")) param.suffix[0] = '\0';

	if (!param.outname[0]) {
		cout << "Output file name ('-' for default meshres.h):\n>> ";
		cin >> param.outname;
		cout << endl;
	}
	if (!strcmp(param.outname, "-")) strcpy(param.outname, "meshres.h");

	char pwd[1024];
	_fullpath(pwd, ".\\", 1024);
	cout << "Current directory is " << pwd << endl;

	cout << "Reading mesh from " << param.meshname << endl;
	ifstream ifs (param.meshname);
	ifs >> mesh;
	ifs.close();
	if (!ifs.good()) {
		cout << "Error reading mesh file." << endl;
		exit(1);
	}
	if (!mesh.nGroup()) {
		cout << "Warning: mesh contains no groups." << endl;
	}

	if(param.outlua)
		outLua(param, mesh);
	else
		outC(param, mesh);

	cout << "Wrote mesh parameters to " << param.outname << endl << endl;

	return 0;
}