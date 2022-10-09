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

const std::vector<oapi::CelestialSphere::StarRenderRec> oapi::CelestialSphere::LoadStars() const
{
	// User settings for star rendering
	StarRenderPrm* prm = (StarRenderPrm*)m_gc->GetConfigParam(CFGPRM_STARRENDERPRM);

	// Read the star database and convert to render parameters
	return StarData2RenderData(LoadStarData(prm->mag_lo), *prm);
}

// --------------------------------------------------------------

const std::vector<VECTOR3> oapi::CelestialSphere::LoadConstellationLines() const
{
	// Read the constellation line database and convert to render parameters
	return ConstellationLineData2RenderData(LoadConstellationLineData());
}

// --------------------------------------------------------------

const std::vector<oapi::CelestialSphere::StarDataRec> oapi::CelestialSphere::LoadStarData(double maxAppMag) const
{
#pragma pack(push,1)
	struct StarDataRecPacked { // packed version for reading from binary file
		float lng;
		float lat;
		float mag;
		WORD specidx;
	};
#pragma pack(pop)

	std::vector<StarDataRec> rec;
	rec.resize(0x20000); // should be large enough to hold the entire Hipparcos list

	FILE* f = fopen("Star.bin", "rb");
	if (f) {
		const int chunksize = 0x1000;
		StarDataRecPacked* packBuf = new StarDataRecPacked[chunksize + 1]; // "+1": padding for avoiding reading out of bounds on packed data
		int i, s, n = 0;
		while (s = fread(packBuf, sizeof(StarDataRecPacked), chunksize, f)) {
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

const std::vector<oapi::CelestialSphere::StarRenderRec> oapi::CelestialSphere::StarData2RenderData(const std::vector<oapi::CelestialSphere::StarDataRec>& starRec, const StarRenderPrm& prm) const
{
	std::vector<StarRenderRec> renderRec;
	double a, b, c;

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
		const StarDataRec& rec = starRec[i];

		// position
		double rlat = rec.lat, rlng = rec.lng;
		double xz = cos(rlat);
		renderRec[i].pos.x = xz * cos(rlng);
		renderRec[i].pos.z = xz * sin(rlng);
		renderRec[i].pos.y = sin(rlat);

		// brightness from apparent magnitude
		if (prm.map_log)
			c = min(1.0, max(prm.brt_min, ::exp(-(rec.mag - prm.mag_hi) * a)));
		else
			c = min(1.0, max(prm.brt_min, a * rec.mag + b));
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
		renderRec[i].col.x = min(c * rescale * r_scale, 1.0);
		renderRec[i].col.y = min(c * rescale * g_scale, 1.0);
		renderRec[i].col.z = min(c * rescale * b_scale, 1.0);
	}
	return renderRec;
}

// --------------------------------------------------------------

const std::vector<oapi::CelestialSphere::LineDataRec> oapi::CelestialSphere::LoadConstellationLineData() const
{
#pragma pack(push,1)
	struct LineDataRecPacked { // packed version for reading from binary file
		float lng1;
		float lat1;
		float lng2;
		float lat2;
	};
#pragma pack(pop)

	std::vector<LineDataRec> rec;
	rec.resize(0x1000);

	FILE* f = fopen("Constell.bin", "rb");
	if (f) {
		const int chunksize = 0x1000;
		LineDataRecPacked* packBuf = new LineDataRecPacked[chunksize + 1]; // "+1": padding for avoiding reading out of bounds on packed data
		int i, s, n = 0;
		while (s = fread(packBuf, sizeof(LineDataRecPacked), chunksize, f)) {
			for (i = 0; i < s; i++, n++) {
				rec[n].lng1 = (double)packBuf[i].lng1;
				rec[n].lat1 = (double)packBuf[i].lat1;
				rec[n].lng2 = (double)packBuf[i].lng2;
				rec[n].lat2 = (double)packBuf[i].lat2;
			}
			if (s < chunksize)
				break;
			else
				rec.resize(n + chunksize);
		}
		fclose(f);
		rec.resize(n);
		rec.shrink_to_fit();
	}
	else {
		LOGOUT_WARN("Constellation data base for celestial sphere (Constell.bin) not found. Disabling constellation lines.");
	}
	return rec;
}

// --------------------------------------------------------------

const std::vector<VECTOR3> oapi::CelestialSphere::ConstellationLineData2RenderData(const std::vector<oapi::CelestialSphere::LineDataRec>& clineRec) const
{
	std::vector<VECTOR3> renderRec;
	renderRec.resize(clineRec.size() * 2);
	for (int i = 0; i < clineRec.size(); i++) {
		double lng1 = (double)clineRec[i].lng1;
		double lat1 = (double)clineRec[i].lat1;
		double lng2 = (double)clineRec[i].lng2;
		double lat2 = (double)clineRec[i].lat2;
		double xz = cos(lat1);
		renderRec[i * 2].x = (float)(xz * cos(lng1));
		renderRec[i * 2].z = (float)(xz * sin(lng1));
		renderRec[i * 2].y = (float)sin(lat1);
		xz = cos(lat2);
		renderRec[i * 2 + 1].x = (float)(xz * cos(lng2));
		renderRec[i * 2 + 1].z = (float)(xz * sin(lng2));
		renderRec[i * 2 + 1].y = (float)sin(lat2);
	}
	return renderRec;
}
