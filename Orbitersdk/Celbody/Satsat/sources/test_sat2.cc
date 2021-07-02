#include <stdio.h>

const double c0 = 299792458;
const double tauA = 499.004783806;

int posired (double, int, double*, double*);
void ReadData (int res);

int main (void)
{
    double AU = c0*tauA;
    double r[6], rorb[6];
    double mjd = 53371; // Jan 1, 2005
    double jd = 2400000.5 + mjd;
    int ksat = 1;
    int kflag = 1;
    int iwrite = 1;
    int i, j;

    int icrt = 1;
    //lecser ("redtass7.dat", &icrt);  // old version
    ReadData (icrt);                 // new version

    for (ksat = 1; ksat <= 7; ksat++) {
	for (i = 0; i < 100; i++) {
	    posired (jd, ksat-1, rorb, rorb+3);
	    for (j = 0; j < 3; j++) rorb[j] *= AU; // convert metres
	    printf ("mjd=%f, rorb: %0.8g %0.8g %0.8g\n", mjd, rorb[0], rorb[1], rorb[2]);
	    jd += 0.01;
	    mjd += 0.01;
	}
    }

    return 0;
}
