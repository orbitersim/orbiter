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

#ifndef __PINESGRAV_H
#define __PINESGRAV_H
class CelestialBody;

class PinesGravProp
{
public:
	PinesGravProp(CelestialBody* celestialbody);
	~PinesGravProp();
	int readGravModel(char* filename, int cutoff, int& actualLoadedTerms, int& maxModelTerms);
	Vector GetPinesGrav(const Vector rposmax, const int maxDegree, const int maxOrder);
	inline unsigned int GetCoeffCutoff() const { return CoeffCutoff; }
private:
	CelestialBody* parentBody;
	inline void GenerateAssocLegendreMatrix(int maxDegree);

	static inline unsigned int NM(unsigned int n, unsigned int m) { return (n * n + n) / 2 + m; }

	double refRad;
	double GM;
	unsigned int degree;
	unsigned int order;
	unsigned int normalized;
	unsigned int CoeffCutoff;
	double referenceLat;
	double referenceLon;
	double* __restrict C;
	double* __restrict S;
	double* __restrict A;
	double* __restrict R;
	double* __restrict I;
	unsigned long int numCoeff;

	double r, s, t, u;
	double rho, rhop;

	double g1temp;
	double g2temp;
	double g3temp;
	double g4temp;

	double g1;
	double g2;
	double g3;
	double g4;
};

#endif