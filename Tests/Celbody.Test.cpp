// not upstream: loads the ported Celbody modules (.so) the way Orbiter does and checks their ephemerides and atmospheres

#include "OrbiterAPI.h"
#include "CelBodyAPI.h"
#include <catch2/catch_test_macros.hpp>
#include <dlfcn.h>
#include <link.h>
#include <strings.h>
#include <algorithm>
#include <cmath>
#include <cstdarg>
#include <cstdio>
#include <cstring>
#include <fstream>
#include <string>
#include <vector>

// exe side the modules bind at dlopen: test doubles of Orbiter's versions
static const double mjd0 = 51544.5; // simulation starts at J2000
static std::vector<void*> initlib_handles;

// GetProcAddress semantics: dlsym also searches dependencies, keep only the module's own symbol
static void *OwnProc (void *hModule, const char *name)
{
	void *proc = dlsym (hModule, name);
	struct link_map *lm;
	Dl_info info;
	if (proc && (dlinfo (hModule, RTLD_DI_LINKMAP, &lm) || !dladdr (proc, &info) || strcmp (info.dli_fname, lm->l_name))) proc = 0;
	return proc;
}

DLLEXPORT void InitLib (void *hModule)
{
	initlib_handles.push_back (hModule);
	void (*DLLInit)(void*) = (void(*)(void*))OwnProc (hModule, "InitModule");
	if (DLLInit) (*DLLInit)(hModule);
}

DLLEXPORT int Date2Int (char *date)
{
	return 1;
}

double oapiTime2MJD (double simt) { return mjd0 + simt/86400.0; }
double oapiGetSimMJD () { return mjd0; }

void oapiWriteLogV (const char *format, ...)
{
	va_list ap;
	va_start (ap, format);
	vprintf (format, ap);
	va_end (ap);
	printf ("\n");
}

void __writeLogError (const char *func, const char *file, int line, const char *format, ...)
{
	va_list ap;
	va_start (ap, format);
	printf ("ERROR: ");
	vprintf (format, ap);
	va_end (ap);
	printf ("\n");
}

// FILEHANDLE here is a std::string* holding the config file path
bool oapiReadItem_float (FILEHANDLE f, char *item, double &d)
{
	if (!f) return false;
	std::ifstream ifs (oapiResolvePath (((std::string*)f)->c_str()));
	std::string line;
	size_t n = strlen (item);
	while (std::getline (ifs, line)) {
		size_t p = line.find_first_not_of (" \t");
		if (p == std::string::npos || strncasecmp (line.c_str()+p, item, n)) continue;
		p = line.find ('=', p+n);
		if (p != std::string::npos) return sscanf (line.c_str()+p+1, "%lf", &d) == 1;
	}
	return false;
}

void oapiGetGlobalPos (OBJHANDLE hObj, VECTOR3 *pos)
{
	*pos = _V(AU, 0, 0);
}

void oapiGlobalToEqu (OBJHANDLE hObj, const VECTOR3 &glob, double *lng, double *lat, double *rad)
{
	*rad = length (glob);
	*lng = atan2 (glob.z, glob.x);
	*lat = asin (glob.y / *rad);
}

// CELBODY/CELBODY2/ATMOSPHERE as in Src/Orbiter/Celbody.cpp, minus the parts that need the simulation
CELBODY::CELBODY () { version = 1; }
bool CELBODY::bEphemeris () const { return false; }
void CELBODY::clbkInit (FILEHANDLE cfg) {}
int CELBODY::clbkEphemeris (double mjd, int req, double *ret) { return 0; }
int CELBODY::clbkFastEphemeris (double simt, int req, double *ret) { return 0; }
bool CELBODY::clbkAtmParam (double alt, ATMPARAM *prm) { return false; }

void CELBODY::Pol2Crt (double *pol, double *crt)
{
	double rad  = pol[2] * AU;
	double cosp = cos(pol[0]), sinp = sin(pol[0]);
	double cost = cos(pol[1]), sint = sin(pol[1]);
	double xz   = rad * cost;
	crt[0] = xz  * cosp;
	crt[2] = xz  * sinp;
	crt[1] = rad * sint;
	double vl = xz  * pol[3];
	double vb = rad * pol[4];
	double vr = pol[5] * AU;
	crt[3] = cosp*cost*vr - cosp*sint*vb - sinp*vl;
	crt[4] = sint*     vr + cost*     vb;
	crt[5] = sinp*cost*vr - sinp*sint*vb + cosp*vl;
}

CELBODY2::CELBODY2 (OBJHANDLE hCBody): CELBODY ()
{
	version++;
	hBody = hCBody;
	atm = NULL;
	hAtmModule = NULL;
}

CELBODY2::~CELBODY2 () {}
void CELBODY2::clbkInit (FILEHANDLE cfg) { CELBODY::clbkInit (cfg); } // atmosphere modules are loaded by the test itself
double CELBODY2::SidRotPeriod () const { return 86164.1; }

ATMOSPHERE::ATMOSPHERE (CELBODY2 *body) { cbody = body; }

bool ATMOSPHERE::clbkConstants (ATMCONST *atmc) const
{
	atmc->R = 286.91;
	atmc->gamma = 1.4;
	return false;
}

bool ATMOSPHERE::clbkParams (const PRM_IN *prm_in, PRM_OUT *prm_out) { return false; }

// the loader steps Orbiter takes for a Celbody module: LoadLibrary (DllMain -> InitLib), InitInstance, clbkInit
struct CelbodyModule {
	void *hDLL = 0;
	CELBODY *body = 0;
	std::string cfg;

	CelbodyModule (const char *name, const char *dir = "Modules/Celbody/")
	{
		cfg = std::string ("Config/") + name + ".cfg";
		std::string path = std::string (dir) + name + ".so";
		hDLL = dlopen (path.c_str(), RTLD_NOW);
		if (!hDLL) { printf ("%s\n", dlerror()); return; }
		CELBODY *(*init)(OBJHANDLE) = (CELBODY*(*)(OBJHANDLE))OwnProc (hDLL, "InitInstance");
		if (!init) return;
		body = init ((OBJHANDLE)this);
		body->clbkInit ((FILEHANDLE)&cfg);
	}

	~CelbodyModule ()
	{
		void (*exit)(CELBODY*) = (void(*)(CELBODY*))OwnProc (hDLL, "ExitInstance");
		if (body && exit) exit (body);
		if (hDLL) dlclose (hDLL);
	}

	// position/velocity block and its flags for the requested time
	int Ephem (double mjd, double *s)
	{
		double ret[12];
		int flg = body->clbkEphemeris (mjd, EPHEM_TRUEPOS | EPHEM_TRUEVEL, ret);
		int ofs = (flg & EPHEM_TRUEPOS ? 0 : 6);
		for (int i = 0; i < 6; i++) s[i] = ret[ofs+i];
		return flg;
	}

	int FastEphem (double simt, double *s)
	{
		double ret[12];
		int flg = body->clbkFastEphemeris (simt, EPHEM_TRUEPOS | EPHEM_TRUEVEL, ret);
		int ofs = (flg & EPHEM_TRUEPOS ? 0 : 6);
		for (int i = 0; i < 6; i++) s[i] = ret[ofs+i];
		return flg;
	}
};

static double Distance (const double *s, int flg)
{
	return (flg & EPHEM_POLAR ? s[2]*AU : sqrt (s[0]*s[0] + s[1]*s[1] + s[2]*s[2]));
}

static void PolarPos (const double *s, double *p)
{
	double rad = s[2]*AU;
	p[0] = rad*cos(s[1])*cos(s[0]);
	p[1] = rad*sin(s[1]);
	p[2] = rad*cos(s[1])*sin(s[0]);
}

struct BodyRange { const char *name; double rmin, rmax, vtol = 1e-5, itol = 10.0; };

// distance from the parent (or barycentre) at J2000 must lie between periapsis and apoapsis (+-1%)
// vtol/itol: TASS17 (Satsat) returns osculating two-body velocities, not the derivative of its perturbed positions
static const BodyRange bodies[] = {
	{"Sun",       1e6,      2.0e9},
	{"Mercury",   0.303*AU, 0.472*AU},
	{"Venus",     0.711*AU, 0.736*AU},
	{"Earth",     0.973*AU, 1.027*AU},
	{"Mars",      1.367*AU, 1.683*AU},
	{"Jupiter",   4.90*AU,  5.51*AU},
	{"Saturn",    8.94*AU,  10.16*AU},
	{"Uranus",    18.1*AU,  20.3*AU},
	{"Neptune",   29.5*AU,  30.7*AU},
	{"Moon",      3.52e8,   4.11e8},
	{"Io",        4.17e8,   4.27e8},
	{"Europa",    6.57e8,   6.84e8},
	{"Ganymede",  1.058e9,  1.083e9},
	{"Callisto",  1.845e9,  1.921e9},
	{"Mimas",     1.80e8,   1.91e8, 5e-5, 10.0},
	{"Enceladus", 2.35e8,   2.41e8, 5e-5, 10.0},
	{"Tethys",    2.91e8,   2.98e8, 5e-5, 10.0},
	{"Dione",     3.73e8,   3.82e8, 5e-5, 10.0},
	{"Rhea",      5.21e8,   5.33e8, 5e-5, 10.0},
	{"Titan",     1.174e9,  1.269e9, 5e-5, 10.0},
	{"Hyperion",  1.286e9,  1.680e9, 2e-3, 200.0},
	{"Iapetus",   3.424e9,  3.700e9, 5e-5, 10.0},
};

TEST_CASE("Celbody modules load through the Orbitersdk entry point", "[celbody]")
{
	for (const BodyRange &b : bodies) {
		INFO(b.name);
		initlib_handles.clear();
		CelbodyModule m (b.name);
		REQUIRE(m.hDLL);
		REQUIRE(m.body);
		CHECK(std::find (initlib_handles.begin(), initlib_handles.end(), m.hDLL) != initlib_handles.end());
		CHECK(m.body->Version() == 2);
		CHECK(m.body->bEphemeris());
		CHECK(OwnProc (m.hDLL, "GetModuleVersion"));
	}
}

TEST_CASE("Earth VSOP87B at J2000 matches the VSOP87 check values", "[celbody]")
{
	CelbodyModule m ("Earth");
	REQUIRE(m.body);
	double s[6];
	int flg = m.Ephem (mjd0, s);
	CHECK(flg & EPHEM_POLAR);
	CHECK(fabs (s[0] - 1.7519238681) < 1e-7);
	CHECK(fabs (s[1] - (-0.0000039656)) < 1e-7);
	CHECK(fabs (s[2] - 0.9833276819) < 1e-7);
}

TEST_CASE("Distances at J2000 lie inside each orbit", "[celbody]")
{
	for (const BodyRange &b : bodies) {
		CelbodyModule m (b.name);
		REQUIRE(m.body);
		double s[6];
		int flg = m.Ephem (mjd0, s);
		double r = Distance (s, flg);
		INFO(b.name << " r=" << r);
		CHECK(r > b.rmin);
		CHECK(r < b.rmax);
	}
}

TEST_CASE("Velocities match the derivative of the positions", "[celbody]")
{
	const double dt = 10.0; // [s]
	for (const BodyRange &b : bodies) {
		CelbodyModule m (b.name);
		REQUIRE(m.body);
		double s[6], s0[6], s1[6];
		int flg = m.Ephem (mjd0, s);
		m.Ephem (mjd0 - dt/86400.0, s0);
		m.Ephem (mjd0 + dt/86400.0, s1);
		double vmax = std::max ({fabs (s[3]), fabs (s[4]), fabs (s[5])});
		for (int i = 0; i < 3; i++) {
			double d = s1[i] - s0[i];
			if ((flg & EPHEM_POLAR) && i == 0) d = remainder (d, 2.0*PI);
			double fd = d / (2.0*dt);
			double tol = (flg & EPHEM_POLAR ? b.vtol*fabs (s[i+3]) + 1e-15 : b.vtol*vmax + 1e-6);
			INFO(b.name << " component " << i << " fd=" << fd << " v=" << s[i+3]);
			CHECK(fabs (fd - s[i+3]) <= tol);
		}
	}
}

TEST_CASE("Interpolated ephemerides follow the exact ones", "[celbody]")
{
	for (const BodyRange &b : bodies) {
		CelbodyModule m (b.name);
		REQUIRE(m.body);
		double maxerr = 0.0;
		for (double simt = 0.0; simt <= 7200.0; simt += 37.0) {
			double f[6], e[6], pf[3], pe[3];
			int flg = m.FastEphem (simt, f);
			m.Ephem (oapiTime2MJD (simt), e);
			if (flg & EPHEM_POLAR) PolarPos (f, pf), PolarPos (e, pe);
			else for (int i = 0; i < 3; i++) pf[i] = f[i], pe[i] = e[i];
			double err = sqrt ((pf[0]-pe[0])*(pf[0]-pe[0]) + (pf[1]-pe[1])*(pf[1]-pe[1]) + (pf[2]-pe[2])*(pf[2]-pe[2]));
			maxerr = std::max (maxerr, err);
		}
		INFO(b.name << " max interpolation error " << maxerr << " m");
		CHECK(maxerr < b.itol);
	}
}

struct AtmCase { const char *body, *module; double alt, rhomin, rhomax; };

TEST_CASE("Atmosphere modules give physical densities", "[celbody]")
{
	static const AtmCase cases[] = {
		{"Earth", "EarthAtm2006",       0.0,   1.20,   1.25},
		{"Earth", "EarthAtmJ71G",       400e3, 1e-13,  1e-10},
		{"Earth", "EarthAtmNRLMSISE00", 400e3, 1e-13,  1e-10},
		{"Mars",  "MarsAtm2006",        0.0,   0.005,  0.05},
		{"Venus", "VenusAtm2006",       0.0,   40.0,   90.0},
	};
	for (const AtmCase &c : cases) {
		INFO(c.module);
		CelbodyModule m (c.body);
		REQUIRE(m.body);
		std::string dir = std::string ("Modules/Celbody/") + c.body + "/Atmosphere/";
		initlib_handles.clear();
		void *hAtm = dlopen ((dir + c.module + ".so").c_str(), RTLD_NOW);
		REQUIRE(hAtm);
		CHECK(std::find (initlib_handles.begin(), initlib_handles.end(), hAtm) != initlib_handles.end());
		ATMOSPHERE *(*create)(CELBODY2*) = (ATMOSPHERE*(*)(CELBODY2*))OwnProc (hAtm, "CreateAtmosphere");
		void (*destroy)(ATMOSPHERE*) = (void(*)(ATMOSPHERE*))OwnProc (hAtm, "DeleteAtmosphere");
		REQUIRE(create);
		REQUIRE(destroy);
		ATMOSPHERE *atm = create ((CELBODY2*)m.body);
		ATMOSPHERE::PRM_IN in;
		memset (&in, 0, sizeof(in));
		in.alt = c.alt;
		in.flag = ATMOSPHERE::PRM_ALT;
		ATMOSPHERE::PRM_OUT out;
		CHECK(atm->clbkParams (&in, &out));
		INFO("rho=" << out.rho << " T=" << out.T << " p=" << out.p);
		CHECK(out.rho > c.rhomin);
		CHECK(out.rho < c.rhomax);
		destroy (atm);
		dlclose (hAtm);
	}
}
