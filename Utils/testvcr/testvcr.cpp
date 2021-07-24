// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// this dummy executable simply checks for the presence of the required
// dynamic C++ runtime libraries.
// Return value 0: ok
//              1: failure (library link error)
// Note: Make sure to set runtime libraries to "Multithreaded DLL" in
// Properties/Code generation

#include <math.h>
#include <iostream>

using namespace std;

int main (int argc, char *argv[])
{
	cout << "Testing C++ runtime library linkage" << endl;

	double a = 10.0;
	double b = sqrt(a);

	return 0;
}
