// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#define STRICT 1
#define OAPI_IMPLEMENTATION

#include "CelSphereAPI.h"
#include "Log.h"

// ==============================================================

oapi::CelestialSphere::CelestialSphere(oapi::GraphicsClient* gc)
	: m_gc(gc)
{
}

// --------------------------------------------------------------

const std::vector<oapi::CelestialSphere::StarRenderRec> oapi::CelestialSphere::LoadStars()
{
	// User settings for star rendering
	StarRenderPrm* prm = (StarRenderPrm*)m_gc->GetConfigParam(CFGPRM_STARRENDERPRM);

	// Read the star database and convert to render parameters
	return StarData2RenderData(LoadStarData(prm->mag_lo), *prm);
}

// --------------------------------------------------------------

const std::vector<oapi::CelestialSphere::StarRec> oapi::CelestialSphere::LoadStarData(double maxAppMag) const
{
#pragma pack(push,1)
	struct StarRecPack { // packed version for reading from binary file
		float lng;
		float lat;
		float mag;
		WORD specidx;
	};
#pragma pack(pop)

	std::vector<StarRec> rec;
	rec.resize(0x20000); // should be large enough to hold the entire Hipparcos list

	FILE* f = fopen("Star.bin", "rb");
	if (f) {
		const int chunksize = 0x1000;
		StarRecPack* packBuf = new StarRecPack[chunksize + 1]; // "+1": padding for avoiding reading out of bounds on packed data
		int i, s, n = 0;
		while (s = fread(packBuf, sizeof(StarRecPack), chunksize, f)) {
			for (i = 0; i < s && packBuf[i].mag < maxAppMag; i++, n++) {
				rec[n].lng = (double)packBuf[i].lng;
				rec[n].lat = (double)packBuf[i].lat;
				rec[n].mag = (double)packBuf[i].mag;
				rec[n].specidx = packBuf[i].specidx;
			}
			if (i < s)
				break;
			if (rec.size() < n + chunksize)
				rec.resize(n + chunksize);
		}
		delete[]packBuf;
		fclose(f);
		rec.resize(n);
		rec.shrink_to_fit();
		LOGOUT("Loaded %d records from star database", n);
	}
	else {
		LOGOUT_WARN("Star data base for celestial sphere (Star.bin) not found. Disabling background stars.");
	}
	return rec;

}

// --------------------------------------------------------------

const std::vector<oapi::CelestialSphere::StarRenderRec> oapi::CelestialSphere::StarData2RenderData(const std::vector<oapi::CelestialSphere::StarRec>& starRec, const StarRenderPrm& prm) const
{
	std::vector<StarRenderRec> renderRec;
	double a, b;
	float c;

	if (prm.mag_lo <= prm.mag_hi) {
		LOGOUT_WARN("Inconsistent magnitude limits for background star brightness. Disabling background stars.");
		return renderRec;
	}

	if (prm.map_log) { // scaling factors for logarithmic brightness mapping
		a = -log(prm.brt_min) / (prm.mag_lo - prm.mag_hi);
	}
	else {              // scaling factors for linear brightness mapping
		a = (1.0 - prm.brt_min) / (prm.mag_hi - prm.mag_lo);
		b = prm.brt_min - prm.mag_lo * a;
	}

	renderRec.resize(starRec.size());
	for (size_t i = 0; i < starRec.size(); i++) {
		const StarRec& rec = starRec[i];

		// position
		double rlat = rec.lat, rlng = rec.lng;
		double xz = cos(rlat);
		renderRec[i].x = (float)(xz * cos(rlng));
		renderRec[i].z = (float)(xz * sin(rlng));
		renderRec[i].y = (float)(sin(rlat));

		// brightness from apparent magnitude
		if (prm.map_log)
			c = (float)min(1.0, max(prm.brt_min, ::exp(-(rec.mag - prm.mag_hi) * a)));
		else
			c = (float)min(1.0, max(prm.brt_min, a * rec.mag + b));
		renderRec[i].brightness = c;

		// colour from spectral class index
		double r_scale = (rec.specidx < 25 ? rec.specidx / 25.0 * (1.0 - 0.75) + 0.75 :
			1.0);
		double g_scale = (rec.specidx < 20 ? rec.specidx / 20.0 * (1.0 - 0.85) + 0.85 :
			rec.specidx < 50 ? 1.0 :
			(70 - rec.specidx) / 20.0 * (1.0 - 0.75) + 0.75);
		double b_scale = (rec.specidx < 30 ? 1.0 :
			(70 - rec.specidx) / 40.0 * (1.0 - 0.6) + 0.6);

		// rescale for overall brightness
		double rescale = 3.0 / (r_scale + g_scale + b_scale); // rescale to maintain brightness
		renderRec[i].r = min(c * rescale * r_scale, 1.0);
		renderRec[i].g = min(c * rescale * g_scale, 1.0);
		renderRec[i].b = min(c * rescale * b_scale, 1.0);
	}
	return renderRec;
}

