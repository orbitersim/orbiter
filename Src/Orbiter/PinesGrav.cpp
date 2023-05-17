/*

	Copyright(c) Matthew Hume
	Licensed under the MIT License

	All non-origional code is Public Domain and was taken from NASA/NTRS
	The following functions are heavily based on the works of Samual Pines:

	Pines, S., "Uniform Representation of the Gravitational Potential and its Derivatives," AIAA Journal,
	Vol. 11, No. 11, Nov. 1973, pp. 1508-1511.

	And the examples found in:

	Normalization and Implementation of Three Gravitational Acceleration Models
	Randy A. Eckman
	Aaron J. Brown
	Daniel R. Adamo


*/


#include <fstream>
#include <cmath>
#include "Vecmat.h"
#include "PinesGrav.h"
#include "Orbiter.h"

PinesGravProp::PinesGravProp(CelestialBody* celestialbody)
{
	parentBody = celestialbody;

	refRad = 0.0;
	GM = 0.0;
	degree = 0;
	order = 0;
	normalized = 0;
	referenceLat = 0.0;
	referenceLon = 0.0;
	C = NULL;
	S = NULL;
	A = NULL;
	R = NULL;
	I = NULL;
	numCoeff = 0;
	CoeffCutoff = 0;

	r = 0.0;
	s = 0.0;
	t = 0.0;
	u = 0.0;
	rho = 0.0;
	rhop = 0.0;

	g1temp = 0.0;
	g2temp = 0.0;
	g3temp = 0.0;
	g4temp = 0.0;

	g1 = 0.0;
	g2 = 0.0;
	g3 = 0.0;
	g4 = 0.0;
}

PinesGravProp::~PinesGravProp()
{
	delete[] C;
	delete[] S;
	delete[] A;
	delete[] R;
	delete[] I;
}

inline void PinesGravProp::GenerateAssocLegendreMatrix(int maxDegree)
{
	A[0] = sqrt(2.0);

	for (int m = 0; m <= (maxDegree + 2); m++) {

		if (m != 0) {
			A[NM(m, m)] = sqrt(1. + (1. / (2. * (double)m))) * A[NM(m - 1, m - 1)]; // diagonal terms
		}

		if (m != (maxDegree + 1)) {
			A[NM(m + 1, m)] = sqrt(2. * (double)m + 3.) * u * A[NM(m, m)]; // off-diagonal terms
		}

		if (m < maxDegree) {
			for (int n = m + 2; n <= (maxDegree + 2); n++) {
				double ALPHA_NUM = (2. * (double)n + 1.) * (2. * (double)n - 1.);
				double ALPHA_DEN = ((double)n - (double)m) * ((double)n + (double)m);
				double ALPHA = sqrt(ALPHA_NUM / ALPHA_DEN);
				double BETA_NUM = (2. * (double)n + 1.) * ((double)n - (double)m - 1.) * ((double)n + (double)m - 1.);
				double BETA_DEN = (2. * (double)n - 3.) * ((double)n + (double)m) * ((double)n - (double)m);
				double BETA = sqrt(BETA_NUM / BETA_DEN);


				A[NM(n, m)] = ALPHA * u * A[NM(n - 1, m)] - BETA * A[NM(n - 2, m)]; // remaining terms in the column
			}
		}
	}

	for (int n = 0; n <= (maxDegree + 2); n++) {
		A[NM(n, 0)] = A[NM(n, 0)] * sqrt(0.5);
	}

}

int PinesGravProp::readGravModel(char* filename, int cutoff, int &actualLoadedTerms, int &maxModelTerms)
{
	FILE* gravModelFile = nullptr;
	char gravFileLine[512];
	bool isEOF = false;
	CoeffCutoff = cutoff;
	unsigned int linecount = 0;
	unsigned int cutoffLines = (cutoff * cutoff + cutoff) / 2 + cutoff;
	unsigned int maxLines = cutoffLines;

	try {
		C = new double[(size_t)NM(cutoff + 1, cutoff + 1)];
		S = new double[(size_t)NM(cutoff + 1, cutoff + 1)];
		R = new double[(size_t)cutoff + 2];
		I = new double[(size_t)cutoff + 2];
		A = new double[NM((size_t)cutoff + 3, (size_t)cutoff + 3)];
	}
	catch (std::bad_alloc) {
		return 2; //Could not allocate space
	}
	numCoeff = 0;

	C[0] = 0;	//This needs to be 0 unless you want the point-mass gravity as well.
	S[0] = 0; 

	int file_error = fopen_s(&gravModelFile, filename, "rt");

	if (file_error == 0 && gravModelFile) {
		while (fgets(gravFileLine, 511, gravModelFile))
		{
			if (feof(gravModelFile))
			{
				break;
			}

			if (linecount == 0) {
				if (!sscanf(gravFileLine, " %lf , %lf , %*lf , %d , %d , %d , %lf , %lf \n",
					&refRad,
					&GM,
					&order,
					&degree,
					&normalized,
					&referenceLat,
					&referenceLon)) {
					return 3; //Bad first line format
				}
				maxLines = NM(order, degree);

				if (maxLines < cutoffLines) {
					cutoffLines = maxLines;
					CoeffCutoff = (int)std::lround((sqrt(9+8*cutoffLines)-3)/2); //Solution to n for (2^2+n)/n+2 = orderDegree
				}
				numCoeff = linecount += 2;
			}
			else if (linecount <= cutoffLines + 1) {

				unsigned int lineindex = linecount - 1; //this is so we start loading coefficients at n=1

				if (!sscanf(gravFileLine, " %*d , %*d , %lf , %lf , %*lf , %*lf \n",
					&C[lineindex],
					&S[lineindex])) {
					return 4;//Bad coefficient line format
				}
				numCoeff = linecount++;
			}
		}
		actualLoadedTerms = NM(CoeffCutoff, CoeffCutoff);
		maxModelTerms = NM(order,degree);
		return 0; //successfully loaded gravity coefficients
	}
	else {
		return 1; //Could Not Open File
	}
}

Vector PinesGravProp::GetPinesGrav(const Vector rpos, const int maxDegree, const int maxOrder)
{
	r = rpos.length();
	s = rpos.x / r;
	t = rpos.y / r;
	u = rpos.z / r;

	rho = GM / (r * refRad);
	rhop = refRad / r;

	R[0] = 0.0;
	I[0] = 0.0;
	R[1] = 1.0;
	I[1] = 0.0;

	for (int m = 2; m <= maxOrder + 1; m++) {
		R[m] = s * R[m - 1] - t * I[m - 1];
		I[m] = s * I[m - 1] + t * R[m - 1];
	}

	g1temp = 0.0;
	g2temp = 0.0;
	g3temp = 0.0;
	g4temp = 0.0;

	g1 = 0.0;
	g2 = 0.0;
	g3 = 0.0;
	g4 = 0.0;

	int nmodel = 0;
	GenerateAssocLegendreMatrix(maxDegree);
	for (int n = 0; n <= maxDegree; n++) {

		g1temp = 0.0;
		g2temp = 0.0;
		g3temp = 0.0;
		g4temp = 0.0;

		double SM = 0.5;

		if (n > maxOrder)
			nmodel = maxOrder;
		else
			nmodel = n;


		for (int m = 0; m <= nmodel; m++) {

			double D = C[NM(n, m)] * R[m + 1] + S[NM(n, m)] * I[m + 1];
			double E = C[NM(n, m)] * R[m] + S[NM(n, m)] * I[m];
			double F = S[NM(n, m)] * R[m] - C[NM(n, m)] * I[m];


			double ALPHA = sqrt(SM * ((double)n - (double)m) * ((double)n + (double)m + 1));

			g1temp = g1temp + A[NM(n, m)] * (double)m * E;
			g2temp = g2temp + A[NM(n, m)] * (double)m * F;
			g3temp = g3temp + ALPHA * A[NM(n, m + 1)] * D;
			g4temp = g4temp + (((double)n + (double)m + 1) * A[NM(n, m)] + ALPHA * u * A[NM(n, m + 1)]) * D;

			if (m == 0) { SM = 1.0; ; }
		}
		rho = rhop * rho;

		g1 = g1 + rho * g1temp;
		g2 = g2 + rho * g2temp;
		g3 = g3 + rho * g3temp;
		g4 = g4 + rho * g4temp;

	}

	Vector gperturbed;

	gperturbed.x = (g1 - g4 * s);
	gperturbed.y = (g2 - g4 * t);
	gperturbed.z = (g3 - g4 * u);

	return gperturbed;
}