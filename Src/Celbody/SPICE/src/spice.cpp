#define STRICT
#define ORBITER_MODULE
#include <string>
#include <iostream>
#include <set>
#include <vector>
#include "orbitersdk.h"
// #include "OrbiterAPI.h"
// #include "CelbodyAPI.h"
#include "SpiceUsr.h"

using namespace std;

static set<string> loaded_kernels;

bool load_kernel(const string& kernel_name)
{
	char s[1024];
	if (loaded_kernels.find(kernel_name) != loaded_kernels.end())
		return true;
	furnsh_c(kernel_name.c_str());
	if (failed_c())
	{
		getmsg_c("long", sizeof(s), s);
		reset_c();
		return false;
	}
	sprintf_s(s, 1024, "spice.dll: Kernel %s - OK.", kernel_name.c_str());
	loaded_kernels.insert(kernel_name);
	oapiWriteLog(s);
	return true;
}

bool get_id(char* name, int* id)
{
	SpiceInt spiceID = 0;
	SpiceBoolean found = SPICEFALSE;
	if (strlen(name) > 0)
	{
		bodn2c_c(name, &spiceID, &found);
		if (found)
		{
			*id = (int)spiceID;
		}
		else
		{
			long spiceIDlng;
			if (sscanf_s(name, " %ld", &spiceIDlng) == 1)
			{
				*id = (int)spiceIDlng;
				found = SPICETRUE;
			}
		}
	}
	return found == SPICETRUE;
}

class SpiceBody : public CELBODY2
{
public:
	SpiceBody(OBJHANDLE hCBody);
	bool bEphemeris() const;
	void clbkInit(FILEHANDLE cfg);
	int clbkEphemeris(double mjd, int req, double* ret);
	int clbkFastEphemeris(double simt, int req, double* ret);
	bool clbkAtmParam(double alt, ATMPARAM* prm);

	inline bool LegacyAtmosphereInterface() const
	{
		return false;
	};

	inline ATMOSPHERE* GetAtmosphere() const
	{
		return atm;
	};

	void SpiceBody::show_error(char* s);

private:
	char body_name[256];
	char origin_name[256];
	char bary_name[256];
	char parent_bary_name[256];
	double kernel_begin;
	double kernel_end;
	int body_id;
	int origin_id;
	int bary_id;
	int parent_bary_id;
	bool error;
	double elts_begin[8];
	double elts_end[8];
	double elts_sys_begin[8];
	double elts_sys_end[8];
	void SpiceBody::Interpolate(double simt, double* r);
	double interval;
	int response = -1;
	int prev_response = -1;
	double t_beg = -1.0e99;
	double t_end = -1.0e99;
	double sp_beg[12];
	double sp_end[12];
	double SpiceBody::calc_gm(double* state1, double* state2, double dt);
	double gm, gm_sys;
};

void SpiceBody::show_error(char* s)
{
	oapiWriteLog(s);
	error = true;
}

SpiceBody::SpiceBody(OBJHANDLE hCBody) : CELBODY2(hCBody)
{
	// add constructor code here
	version = 2;
	kernel_begin = 0;
	kernel_end = 0;
	error = false;
	atm = NULL;
}

bool SpiceBody::bEphemeris() const
{
	// class supports ephemeris calculation
	return !error;
}

void trimspaces(string& str)
{
	size_t startpos = str.find_first_not_of(" \t\r\n;,");
	size_t endpos = str.find_last_not_of(" \t\r\n;,");
	if ((string::npos == startpos) || (string::npos == endpos))
	{
		str = "";
	}
	else
		str = str.substr(startpos, endpos - startpos + 1);
}

int stringsplit(string str, const string delim, vector<string>& results)
{
	int cutAt;
	int cnt = 0;
	while ((cutAt = str.find_first_of(delim)) != str.npos)
	{
		if (cutAt > 0)
		{
			results.push_back(str.substr(0, cutAt));
			cnt++;
		}
		str = str.substr(cutAt + 1);
	}
	trimspaces(str);
	if (str.length() > 0)
	{
		results.push_back(str);
		cnt++;
	}
	return cnt;
}

void SpiceBody::clbkInit(FILEHANDLE cfg)
{
	// read parameters from config file (e.g. tolerance limits, etc)
	// perform any required initialisation (e.g. read perturbation terms from data files)
	char s[256];
	vector<string> kernels;
	bool default_interval = true;

	// old atmosphere!
	if (oapiReadItem_string(cfg, "Module_Atm", s))
	{
		// oapiWriteLog(s);
		if (!LoadAtmosphereModule(s))
		{
			sprintf_s(s, 256, "spice.dll: Error in %s - Module_Atm is wrong!", cfg);
			show_error(s);
			return;
		};
		// if (atm==NULL) oapiWriteLog("NULL atm");
	};

	if (!oapiReadItem_string(cfg, "Kernel", s))
	{
		sprintf_s(s, 256, "spice.dll: Error in %s - Not specified kernel file!", cfg);
		show_error(s);
		return;
	}

	if (!stringsplit(s, ",", kernels))
	{
		sprintf_s(s, 256, "spice.dll: Error in %s - Kernel parameter is wrong!", cfg);
		show_error(s);
		return;
	}

	if (!oapiReadItem_string(cfg, "Body", body_name))
	{
		sprintf_s(s, 256, "spice.dll: Error in %s - Not specified: body name!", cfg);
		show_error(s);
		return;
	}

	if (!oapiReadItem_string(cfg, "Barycenter", bary_name))
	{
		sprintf_s(s, 256, "spice.dll: Error in %s - Not specified: barycenter name!", cfg);
		show_error(s);
		return;
	}

	if (!oapiReadItem_float(cfg, "Interval", interval))
	{
		interval = 60.0;
	}

	if (!oapiReadItem_string(cfg, "ParentBarycenter", parent_bary_name))
	{
		sprintf_s(s, 256, "spice.dll: Error in %s - Not specified the ParentBarycenter!", cfg);
		show_error(s);
		return;
	}

	if (!get_id(parent_bary_name, &parent_bary_id))
	{
		sprintf_s(s, 256, "spice.dll: Couldn't find SPICE ID for %s", parent_bary_name);
		show_error(s);
		return;
	}

	if (!oapiReadItem_string(cfg, "Origin", origin_name))
	{
		sprintf_s(s, 256, "spice.dll: Error in %s - Not specified the origin name!", cfg);
		show_error(s);
		return;
	}

	if (!oapiReadItem_float(cfg, "GM", gm))
	{
		gm = -1.0;
	}

	if (!oapiReadItem_float(cfg, "GMSystem", gm_sys))
	{
		gm_sys = -1.0;
	}


	if (oapiReadItem_float(cfg, "Beginning", kernel_begin))
	{
		kernel_begin = (kernel_begin - 51544.5) * 86400.0;
		if (oapiReadItem_float(cfg, "Ending", kernel_end))
		{
			kernel_end = (kernel_end - 51544.5) * 86400.0;
			if (kernel_end > kernel_begin)
				default_interval = false;
		}
	}

	for (vector<string>::iterator i = kernels.begin(); i != kernels.end(); i++)
	{
		trimspaces(*i);
		if (!load_kernel(*i))
		{
			sprintf_s(s, 256, "spice.dll: Couldn't load kernel: %s", (*i).c_str());
			show_error(s);
			return;
		}
	}

	if (!get_id(body_name, &body_id))
	{
		sprintf_s(s, 256, "spice.dll: Couldn't find SPICE ID for %s", body_name);
		show_error(s);
		return;
	}

	if (!get_id(origin_name, &origin_id))
	{
		sprintf_s(s, 256, "spice.dll: Couldn't find SPICE ID for %s", origin_name);
		show_error(s);
		return;
	}

	if (!get_id(bary_name, &bary_id))
	{
		sprintf_s(s, 256, "spice.dll: Couldn't find SPICE ID for %s", bary_name);
		show_error(s);
		return;
	}

	SpiceInt spkCount = 0;
	ktotal_c("spk", &spkCount);
	const int MaxIntervals = 10;
	SPICEDOUBLE_CELL(targetCoverage, MaxIntervals * 2);
	scard_c(0, &targetCoverage);
	for (SpiceInt i = 0; i < spkCount; i++)
	{
		SpiceChar filename[512];
		SpiceChar filetype[32];
		SpiceChar source[256];
		SpiceInt handle;
		SpiceBoolean found;
		kdata_c(i, "spk", sizeof(filename), sizeof(filetype), sizeof(source), filename, filetype, source, &handle, &found);
		if (body_id != 0)
		{
			spkcov_c(filename, body_id, &targetCoverage);
		}
	}

	SpiceInt nIntervals = card_c(&targetCoverage) / 2;
	if (nIntervals <= 0 && body_id != 0)
	{
		sprintf_s(s, 256, "spice.dll: Couldn't find object %s in SPICE kernel pool!", body_name);
		show_error(s);
		if (failed_c())
		{
			reset_c();
		}
		return;
	}

	if (default_interval)
	{
		if (body_id == 0)
		{
			kernel_begin = -1.0e50;
			kernel_end = +1.0e50;
		}
		else
		{
			wnfetd_c(&targetCoverage, 0, &kernel_begin, &kernel_end);
			kernel_begin += 0.001;
			kernel_end -= 0.001;
		}
	}
	else
	{
		kernel_begin += 0.001;
		kernel_end -= 0.001;
		if (body_id != 0 && !wnincd_c(kernel_begin, kernel_end, &targetCoverage))
		{
			sprintf_s(s, 256, "spice.dll: Specified time interval for target %s not available.", body_name);
			show_error(s);
			return;
		}
	}

	double state1[6];
	double state2[6];
	double lt = 0.0;
	double dt = 10;

	if (body_id != 10)
	{
		spkgeo_c(bary_id, kernel_begin, "ECLIPJ2000", parent_bary_id, state1, &lt);
		if (gm <= 0.0) {
			spkgeo_c(bary_id, kernel_begin + dt, "ECLIPJ2000", parent_bary_id, state2, &lt);
			gm = calc_gm(state1, state2, dt);
		}
		oscelt_c(state1, kernel_begin, gm, elts_begin);

		spkgeo_c(bary_id, kernel_end, "ECLIPJ2000", parent_bary_id, state1, &lt);
		if (gm <= 0.0) {
			spkgeo_c(bary_id, kernel_end - dt, "ECLIPJ2000", parent_bary_id, state2, &lt);
			gm = calc_gm(state1, state2, dt);
		}
		oscelt_c(state1, kernel_end, gm, elts_end);

		if (body_id != bary_id)
		{
			spkgeo_c(body_id, kernel_begin, "ECLIPJ2000", bary_id, state1, &lt);
			if (gm_sys <= 0.0) {
				spkgeo_c(body_id, kernel_begin + dt, "ECLIPJ2000", bary_id, state2, &lt);
				gm_sys = calc_gm(state1, state2, dt);
			}
			oscelt_c(state1, kernel_begin, gm_sys, elts_sys_begin);

			spkgeo_c(body_id, kernel_end, "ECLIPJ2000", bary_id, state1, &lt);
			if (gm_sys <= 0.0) {
				spkgeo_c(body_id, kernel_end - dt, "ECLIPJ2000", bary_id, state2, &lt);
				gm_sys = calc_gm(state1, state2, dt);
			}
			oscelt_c(state1, kernel_end, gm_sys, elts_sys_end);
		}
	}
	else
	{
		spkgeo_c(body_id, kernel_begin, "ECLIPJ2000", 0, state1, &lt);
		if (gm <= 0.0) {
			spkgeo_c(body_id, kernel_begin + dt, "ECLIPJ2000", 0, state2, &lt);
			gm = calc_gm(state1, state2, dt);
		}
		oscelt_c(state1, kernel_begin, gm, elts_begin);

		spkgeo_c(body_id, kernel_end, "ECLIPJ2000", 0, state1, &lt);
		if (gm <= 0.0) {
			spkgeo_c(body_id, kernel_end - dt, "ECLIPJ2000", 0, state2, &lt);
			gm = calc_gm(state1, state2, dt);
		}
		oscelt_c(state1, kernel_end - dt, gm, elts_end);
	}

	if (failed_c())
	{
		char errMsg[1024];
		getmsg_c("long", sizeof(errMsg), errMsg);
		show_error(errMsg);
		reset_c();
		return;
	}

	sprintf_s(s, 256, "spice.dll: %s (MJD %.1f - %.1f) - OK",
		body_name, kernel_begin / 86400.0 + 51544.5, kernel_end / 86400.0 + 51544.5);
	oapiWriteLog(s);

	// caching
	double t0 = (oapiTime2MJD(0) - 51544.5) * 86400.0;
	if (t0 < kernel_begin)
		t0 = kernel_begin;
	double t1 = t0 + 365.0 * 86400.0;
	if (t1 > kernel_end)
		t1 = kernel_end;
	for (double t = t0; t < t1; t += 86400.0)
		spkgeo_c(body_id, t, "ECLIPJ2000", origin_id, state1, &lt);

	CELBODY2::clbkInit(cfg);
}

bool SpiceBody::clbkAtmParam(double alt, ATMPARAM* prm)
{
	ATMOSPHERE::PRM_IN prm_in;
	prm_in.alt = alt;
	prm_in.lat = 0.0;
	prm_in.lng = 0.0;
	prm_in.ap = 0.0;
	prm_in.f107 = 0.0;
	prm_in.f107bar = 0.0;
	prm_in.flag = ATMOSPHERE::PRM_ALT;
	ATMOSPHERE::PRM_OUT prm_out;
	prm->p = 0.0;
	prm->rho = 0.0;
	prm->T = 0.0;
	if (atm == NULL)
	{
		// oapiWriteLog("null ATM");
		return false;
	};
	bool res = atm->clbkParams(&prm_in, &prm_out);
	if (!res)
		oapiWriteLog("false ATM");
	prm->p = prm_out.p;
	prm->rho = prm_out.rho;
	prm->T = prm_out.T;
	return res;
}

int SpiceBody::clbkEphemeris(double mjd, int req, double* r)
{
	// return planet position and velocity for Modified Julian date mjd in ret
	double state[6] = { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 };
	double* elts;
	double lt = 0.0;
	int resp = 0;

	if (error)
		return 0;

	r[0] = r[1] = r[2] = r[3] = r[4] = r[5] = r[6] = r[7] = r[8] = r[9] = r[10] = r[11] = 0.0;

	double t = (mjd - 51544.5) * 86400.0;

	if ((t >= kernel_begin) && (t <= kernel_end))
	{
		//----------------SPICE------------------------------
		lt = 0.0;
		spkgeo_c(body_id, t, "ECLIPJ2000", origin_id, state, &lt);
		if (req & (EPHEM_TRUEPOS | EPHEM_TRUEVEL))
		{
			r[6] = r[0] = state[0] * 1000.0;
			r[8] = r[2] = state[1] * 1000.0;
			r[7] = r[1] = state[2] * 1000.0;
			r[9] = r[3] = state[3] * 1000.0;
			r[11] = r[5] = state[4] * 1000.0;
			r[10] = r[4] = state[5] * 1000.0;
			resp |= EPHEM_TRUEPOS | EPHEM_TRUEVEL;
		}
		if ((body_id != 10) && (req & (EPHEM_BARYPOS | EPHEM_BARYVEL)))
		{
			resp |= EPHEM_BARYPOS | EPHEM_BARYVEL;
			if (bary_id != body_id)
			{
				// body with sattelits - return TRUEPOS/TRUEVEL/BARYPOS/BARYVEL relative to PARENTBODY
				lt = 0.0;
				spkgeo_c(bary_id, t, "ECLIPJ2000", origin_id, state, &lt);
				r[6] = state[0] * 1000.0;
				r[8] = state[1] * 1000.0;
				r[7] = state[2] * 1000.0;
				r[9] = state[3] * 1000.0;
				r[11] = state[4] * 1000.0;
				r[10] = state[5] * 1000.0;
			}
			else
			{
				// body without sattelits  return TRUEPOS/TRUEVEL = BARYPOS/BARYVEL relative to PARENTBODY
				resp |= EPHEM_BARYISTRUE;
			}
		}
	}
	else
	{
		// elliptical orbit
		if (t < kernel_begin)
		{
			elts = elts_begin;
		}
		else
		{
			elts = elts_end;
		}
		if ((body_id != bary_id) && (body_id != 10))
		{
			// body with sattelites
			conics_c(elts, t, state);
			r[6] = state[0] * 1000.0;
			r[8] = state[1] * 1000.0;
			r[7] = state[2] * 1000.0;
			r[9] = state[3] * 1000.0;
			r[11] = state[4] * 1000.0;
			r[10] = state[5] * 1000.0;
			resp = EPHEM_BARYPOS | EPHEM_BARYVEL | EPHEM_PARENTBARY;
			if (t < kernel_begin)
			{
				elts = elts_sys_begin;
			}
			else
			{
				elts = elts_sys_end;
			}
			conics_c(elts, t, state);
			r[0] = r[6] + state[0] * 1000.0;
			r[2] = r[8] + state[1] * 1000.0;
			r[1] = r[7] + state[2] * 1000.0;
			r[3] = r[9] + state[3] * 1000.0;
			r[5] = r[11] + state[4] * 1000.0;
			r[4] = r[10] + state[5] * 1000.0;
			resp |= EPHEM_TRUEPOS | EPHEM_TRUEVEL;
		}
		else
		{
			// body without sattelits - return TRUEPOS/TRUEVEL relative to PARENTBARY
			conics_c(elts, t, state);
			r[0] = r[6] = state[0] * 1000.0;
			r[2] = r[8] = state[1] * 1000.0;
			r[1] = r[7] = state[2] * 1000.0;
			r[3] = r[9] = state[3] * 1000.0;
			r[5] = r[11] = state[4] * 1000.0;
			r[4] = r[10] = state[5] * 1000.0;
			resp = EPHEM_TRUEPOS | EPHEM_TRUEVEL;
			if (body_id != 10)
			{
				resp |= EPHEM_BARYPOS | EPHEM_BARYVEL | EPHEM_PARENTBARY | EPHEM_BARYISTRUE;
			}
		}
	}

	if (failed_c())
	{
		char errMsg[1024];
		getmsg_c("long", sizeof(errMsg), errMsg);
		show_error(errMsg);
		reset_c();
		return 0;
	}

	return resp;
}

int SpiceBody::clbkFastEphemeris(double simt, int req, double* r)
{
	// char s[1024];
	if (error)
		return 0;

	if (simt > t_beg && simt <= t_end)
	{
		Interpolate(simt, r);
		// sprintf_s(s, 1024, "I %s;%.8f;%d;%.8f;%.8f;%.8f;%.8f;%.8f;%.8f;%.8f;%.8f;%.8f;%.8f;%.8f;%.8f",
		//	body_name, simt, response, r[0], r[1], r[2], r[3], r[4], r[5], r[6], r[7], r[8], r[9], r[10], r[11]);
		// oapiWriteLog(s);
	}
	else if (simt > t_end && simt <= t_end + interval)
	{
		t_beg = t_end;
		memcpy(sp_beg, sp_end, 12 * sizeof(double));
		t_end = t_beg + interval;
		response = clbkEphemeris(oapiTime2MJD(t_end), req, sp_end);

		if (prev_response != response)
		{
			t_beg = simt;
			t_end = simt;
			response = clbkEphemeris(oapiTime2MJD(simt), req, r);
			memcpy(sp_end, r, 12 * sizeof(double));
		}
		else
		{
			Interpolate(simt, r);
		}
	}
	else
	{
		t_beg = simt;
		t_end = simt;
		response = clbkEphemeris(oapiTime2MJD(simt), req, r);
		memcpy(sp_end, r, 12 * sizeof(double));
	}

	prev_response = response;

	return response;
}

void SpiceBody::Interpolate(double simt, double* r)
{
	double tT = (simt - t_beg) / interval;
	double tT2 = tT * tT;
	double tT3 = tT * tT * tT;
	double p1 = 2 * tT3 - 3 * tT2 + 1;
	double p2 = (tT3 - 2 * tT2 + tT) * interval;
	double p3 = 3 * tT2 - 2 * tT3;
	double p4 = (tT3 - tT2) * interval;
	double v1 = (6 * tT2 - 6 * tT) / interval;
	double v2 = 3 * tT2 - 4 * tT + 1;
	double v3 = (6 * tT - 6 * tT2) / interval;
	double v4 = 3 * tT2 - 2 * tT;

	for (int i = 0; i < 3; ++i)
	{
		r[i] = p1 * sp_beg[i] + p2 * sp_beg[i + 3] + p3 * sp_end[i] + p4 * sp_end[i + 3];
		r[i + 3] = v1 * sp_beg[i] + v2 * sp_beg[i + 3] + v3 * sp_end[i] + v4 * sp_end[i + 3];
		if (response & (EPHEM_BARYPOS | EPHEM_BARYVEL) && !(response & EPHEM_BARYISTRUE))
		{
			r[i + 6] = p1 * sp_beg[i + 6] + p2 * sp_beg[i + 9] + p3 * sp_end[i + 6] + p4 * sp_end[i + 9];
			r[i + 9] = v1 * sp_beg[i + 6] + v2 * sp_beg[i + 9] + v3 * sp_end[i + 6] + v4 * sp_end[i + 9];
		}
		else
		{
			r[i + 6] = r[i];
			r[i + 9] = r[i + 3];
		}
	}
}

double SpiceBody::calc_gm(double* state1, double* state2, double dt)
{
	double g = 0.0;
	double r1 = 0.0;
	double r2 = 0.0;
	double state_diff[6];
	int i;

	for (i = 0; i < 6; ++i)
	{
		state_diff[i] = state2[i] - state1[i];
	}

	for (i = 0; i < 3; ++i)
	{
		g += state_diff[i + 3] * state_diff[i + 3];
		r1 += state1[i] * state1[i];
		r2 += state2[i] * state2[i];
	}

	r1 = sqrt(r1);
	r2 = sqrt(r2);
	r1 = (r1 + r2) / 2.0;
	g = sqrt(g) / dt * r1 * r1;
	return g;
}

DLLCLBK void InitModule(HINSTANCE hModule)
{
	// module initialisation
	loaded_kernels.clear();
	remove("SPICE.ERR");
	errdev_c("SET", 1024, "SPICE.ERR");
	errprt_c("SET", 1024, "ALL");
	erract_c("SET", 1024, "REPORT");
	trcoff_c();
}
DLLCLBK void ExitModule(HINSTANCE hModule)
{
	// module cleanup
}
DLLCLBK CELBODY* InitInstance(OBJHANDLE hBody)
{
	// instance initialisation
	return new SpiceBody(hBody);
}
DLLCLBK void ExitInstance(CELBODY* body)
{
	// instance cleanup
	delete (SpiceBody*)body;
}
