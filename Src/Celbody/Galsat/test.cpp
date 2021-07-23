// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include <fstream.h>
#include <math.h>

const double c0 = 299792458;
const double tauA = 499.004783806;
const double AU = c0*tauA;

int cd2com (void);
void galsat (double *r, double *rorb, double tjd, int ksat, int kflag);

int main (void)
{
    double r[6], rorb[6];
	double mjd;
    int ksat = 4;
    int kflag = 1;
    int j;

    cd2com();

	ofstream ofs ("callisto.dat");

    for (mjd = 53371; mjd < 53431; mjd += 0.1) {
		galsat (r, rorb, mjd+2400000.5, ksat, kflag);
		for (j = 0; j < 3; j++) r[j] *= AU; // convert metres
		ofs << mjd << '\t' << r[0] << '\t' << r[2] << '\t' << r[1] << endl;
    }
    return 0;
}
