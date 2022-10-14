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
	LoadConstellationLabels();

	gc->clbkGetViewportSize(&m_viewW, &m_viewH);
	m_cLabelFont = gc->clbkCreateFont(max(m_viewH / 50, 14), true, "Arial", FONT_ITALIC);
	m_markerFont = gc->clbkCreateFont(max(m_viewH / 75, 12), true, "Arial");
	for (int i = 0; i < 7; i++)
		m_markerPen[i] = gc->clbkCreatePen(1, 0, MarkerColor(i));
}

// --------------------------------------------------------------

oapi::CelestialSphere::~CelestialSphere()
{
	m_gc->clbkReleaseFont(m_cLabelFont);
	m_gc->clbkReleaseFont(m_markerFont);
	for (int i = 0; i < 7; i++)
		m_gc->clbkReleasePen(m_markerPen[i]);
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

void oapi::CelestialSphere::LoadConstellationLabels()
{
	// Read constellation label database
	m_cLabel = ConstellationLabelData2RenderData(LoadConstellationLabelData());
}


// --------------------------------------------------------------

void oapi::CelestialSphere::RenderConstellationLabels(oapi::Sketchpad** ppSkp, bool fullName)
{
	EnsureMarkerDrawingContext(ppSkp, m_cLabelFont, 0xA0A0A0);

	for (auto it = m_cLabel.begin(); it != m_cLabel.end(); it++) {
		const std::string& label = (*it).label[fullName ? 0 : 1];
		RenderMarker(*ppSkp, (*it).pos, std::string(), label, -1, 0);
	}
}

// --------------------------------------------------------------

void oapi::CelestialSphere::RenderCelestialMarkers(oapi::Sketchpad** ppSkp)
{
	oapi::Font* font = m_markerFont;
	const std::vector<oapi::GraphicsClient::LABELLIST>& markerLists = m_gc->GetCelestialMarkers();
	for (auto it = markerLists.begin(); it != markerLists.end(); it++) {
		if ((*it).active) {
			int size = (int)(m_viewH / 80.0 * (*it).size + 0.5);
			int col = (*it).colour;
			const std::vector<oapi::GraphicsClient::LABELSPEC>& ls = (*it).marker;
			EnsureMarkerDrawingContext(ppSkp, font, MarkerColor(col), MarkerPen(col));
			font = nullptr; // need to set it only once
			for (auto mkr = ls.begin(); mkr != ls.end(); mkr++) {
				RenderMarker(*ppSkp, (*mkr).pos, (*mkr).label[0], (*mkr).label[1], (*it).shape, size);
			}
		}
	}
}

// --------------------------------------------------------------

oapi::Font* oapi::CelestialSphere::MarkerFont() const
{
	return m_markerFont;
}

// --------------------------------------------------------------

oapi::Pen* oapi::CelestialSphere::MarkerPen(DWORD idx) const
{
	return (idx < 7 ? m_markerPen[idx] : 0);
}

// --------------------------------------------------------------

COLORREF oapi::CelestialSphere::MarkerColor(DWORD idx) const
{
	COLORREF col[7] = { 0x00FFFF, 0xFFFF00, 0x4040FF, 0xFF00FF, 0x40FF40, 0xFF8080, 0xFFFFFF };
	return (idx < 7 ? col[idx] : 0xFFFFFF);
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

const std::vector<oapi::CelestialSphere::StarRenderRec> oapi::CelestialSphere::StarData2RenderData(const std::vector<oapi::CelestialSphere::StarDataRec>& starDataRec, const StarRenderPrm& prm) const
{
	std::vector<StarRenderRec> starRenderRec;
	double a, b, c;

	if (prm.mag_lo <= prm.mag_hi) {
		LOGOUT_WARN("Inconsistent magnitude limits for background star brightness. Disabling background stars.");
		return starRenderRec;
	}

	if (prm.map_log) { // scaling factors for logarithmic brightness mapping
		a = -log(prm.brt_min) / (prm.mag_lo - prm.mag_hi);
	}
	else {              // scaling factors for linear brightness mapping
		a = (1.0 - prm.brt_min) / (prm.mag_hi - prm.mag_lo);
		b = prm.brt_min - prm.mag_lo * a;
	}

	starRenderRec.resize(starDataRec.size());
	for (size_t i = 0; i < starDataRec.size(); i++) {
		const StarDataRec& rec = starDataRec[i];

		// position
		double rlat = rec.lat, rlng = rec.lng;
		double xz = cos(rlat);
		starRenderRec[i].pos.x = xz * cos(rlng);
		starRenderRec[i].pos.z = xz * sin(rlng);
		starRenderRec[i].pos.y = sin(rlat);

		// brightness from apparent magnitude
		if (prm.map_log)
			c = min(1.0, max(prm.brt_min, ::exp(-(rec.mag - prm.mag_hi) * a)));
		else
			c = min(1.0, max(prm.brt_min, a * rec.mag + b));
		starRenderRec[i].brightness = c;

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
		starRenderRec[i].col.x = min(c * rescale * r_scale, 1.0);
		starRenderRec[i].col.y = min(c * rescale * g_scale, 1.0);
		starRenderRec[i].col.z = min(c * rescale * b_scale, 1.0);
	}
	return starRenderRec;
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

const std::vector<VECTOR3> oapi::CelestialSphere::ConstellationLineData2RenderData(const std::vector<oapi::CelestialSphere::LineDataRec>& lineDataRec) const
{
	std::vector<VECTOR3> lineRenderRec;
	lineRenderRec.resize(lineDataRec.size() * 2);
	for (int i = 0; i < lineDataRec.size(); i++) {
		double lng1 = (double)lineDataRec[i].lng1;
		double lat1 = (double)lineDataRec[i].lat1;
		double lng2 = (double)lineDataRec[i].lng2;
		double lat2 = (double)lineDataRec[i].lat2;
		double xz = cos(lat1);
		lineRenderRec[i * 2].x = (float)(xz * cos(lng1));
		lineRenderRec[i * 2].z = (float)(xz * sin(lng1));
		lineRenderRec[i * 2].y = (float)sin(lat1);
		xz = cos(lat2);
		lineRenderRec[i * 2 + 1].x = (float)(xz * cos(lng2));
		lineRenderRec[i * 2 + 1].z = (float)(xz * sin(lng2));
		lineRenderRec[i * 2 + 1].y = (float)sin(lat2);
	}
	return lineRenderRec;
}

// --------------------------------------------------------------

const std::vector<oapi::GraphicsClient::ConstLabelRec> oapi::CelestialSphere::LoadConstellationLabelData() const
{
	std::vector<GraphicsClient::ConstLabelRec> rec;

	FILE* f = fopen("Constell2.bin", "rb");
	if (f) {
		double pos[2];
		char abbr[4] = "xxx";
		int nfull = 256;
		char* full = new char[nfull];
		int labelLen;
		GraphicsClient::ConstLabelRec r;
		while (fread(pos, sizeof(double), 2, f) == 2) {
			r.lngCnt = pos[0];
			r.latCnt = pos[1];
			if (fread(abbr, sizeof(char), 3, f) != 3)
				break;
			r.abbrLabel = abbr;
			if (!fread(&labelLen, sizeof(int), 1, f))
				break;
			if (labelLen >= nfull) {
				char* tmp = new char[nfull = labelLen + 1];
				delete[]full;
				full = tmp;
			}
			if (fread(full, sizeof(char), labelLen, f) != labelLen)
				break;
			full[labelLen] = '\0';
			r.fullLabel = full;
			rec.push_back(r);
		}
		delete[]full;
		fclose(f);
	}
	else {
		LOGOUT_WARN("Constellation data base for celestial sphere (Constell2.bin) not found. Disabling constellation labels.");
	}
	return rec;
}

// --------------------------------------------------------------

const std::vector<oapi::GraphicsClient::LABELSPEC> oapi::CelestialSphere::ConstellationLabelData2RenderData(const std::vector<GraphicsClient::ConstLabelRec>& clabelRec) const
{
	std::vector<GraphicsClient::LABELSPEC> renderRec;
	renderRec.resize(clabelRec.size());
	for (int i = 0; i < clabelRec.size(); i++) {
		renderRec[i].label[0] = clabelRec[i].fullLabel;
		renderRec[i].label[1] = clabelRec[i].abbrLabel;
		double xz = cos(clabelRec[i].latCnt);
		renderRec[i].pos.x = xz * cos(clabelRec[i].lngCnt);
		renderRec[i].pos.z = xz * sin(clabelRec[i].lngCnt);
		renderRec[i].pos.y = sin(clabelRec[i].latCnt);
	}
	return renderRec;
}

// --------------------------------------------------------------

void oapi::CelestialSphere::RenderMarker(oapi::Sketchpad* pSkp, const VECTOR3& rdir, const std::string& label1, const std::string& label2, int mode, int scale)
{
	if (!pSkp)
		return;

	int x, y, len;
	if (!scale) scale = m_viewH / 80;

	if (EclDir2WindowPos(rdir, x, y)) {

		switch (mode) {

		case 0: // box
			pSkp->Rectangle(x - scale, y - scale, x + scale + 1, y + scale + 1);
			break;

		case 1: // circle
			pSkp->Ellipse(x - scale, y - scale, x + scale + 1, y + scale + 1);
			break;

		case 2: // diamond
			pSkp->MoveTo(x, y - scale);
			pSkp->LineTo(x + scale, y); pSkp->LineTo(x, y + scale);
			pSkp->LineTo(x - scale, y); pSkp->LineTo(x, y - scale);
			break;

		case 3: { // nabla
			int scl1 = (int)(scale * 1.1547);
			pSkp->MoveTo(x, y - scale);
			pSkp->LineTo(x + scl1, y + scale); pSkp->LineTo(x - scl1, y + scale); pSkp->LineTo(x, y - scale);
		} break;

		case 4: { // delta
			int scl1 = (int)(scale * 1.1547);
			pSkp->MoveTo(x, y + scale);
			pSkp->LineTo(x + scl1, y - scale); pSkp->LineTo(x - scl1, y - scale); pSkp->LineTo(x, y + scale);
		} break;

		case 5: { // crosshair
			int scl1 = scale / 4;
			pSkp->MoveTo(x, y - scale); pSkp->LineTo(x, y - scl1);
			pSkp->MoveTo(x, y + scale); pSkp->LineTo(x, y + scl1);
			pSkp->MoveTo(x - scale, y); pSkp->LineTo(x - scl1, y);
			pSkp->MoveTo(x + scale, y); pSkp->LineTo(x + scl1, y);
		} break;

		case 6: { // rotated crosshair
			int scl1 = scale / 4;
			pSkp->MoveTo(x - scale, y - scale); pSkp->LineTo(x - scl1, y - scl1);
			pSkp->MoveTo(x - scale, y + scale); pSkp->LineTo(x - scl1, y + scl1);
			pSkp->MoveTo(x + scale, y - scale); pSkp->LineTo(x + scl1, y - scl1);
			pSkp->MoveTo(x + scale, y + scale); pSkp->LineTo(x + scl1, y + scl1);
		} break;
		}

		if (len = label1.size())
			pSkp->Text(x, y - scale, label1.c_str(), len);
		if (len = label2.size())
			pSkp->Text(x, y + scale + 15, label2.c_str(), len);
	}
}

// --------------------------------------------------------------

void oapi::CelestialSphere::EnsureMarkerDrawingContext(oapi::Sketchpad** ppSkp, oapi::Font* font, COLORREF textcol, oapi::Pen* pen)
{
	if (!*ppSkp) {
		*ppSkp = m_gc->clbkGetSketchpad(0);
		(*ppSkp)->SetBackgroundMode(oapi::Sketchpad::BK_TRANSPARENT);
		(*ppSkp)->SetTextAlign(oapi::Sketchpad::CENTER, oapi::Sketchpad::BOTTOM);
	}
	if (font)
		(*ppSkp)->SetFont(font);
	if (textcol)
		(*ppSkp)->SetTextColor(textcol);
	if (pen)
		(*ppSkp)->SetPen(pen);
}

// --------------------------------------------------------------

MATRIX3 oapi::CelestialSphere::Celestial2Ecliptic() const
{
	// Set up rotation for celestial grid rendering
	double eps, lan;
	MATRIX3 R;
	OBJHANDLE hEarth = oapiGetGbodyByName("Earth");
	if (hEarth) {  // use current Earth precession axis
		eps = oapiGetPlanetObliquity(hEarth);
		lan = oapiGetPlanetTheta(hEarth);
	}
	else {         // default: use the J2000 ecliptic
		eps = 0.4092797095927;
		lan = 0.0;
	}
	double coso = cos(eps), sino = sin(eps);
	double cosl = cos(lan), sinl = sin(lan);
	R.m11 = cosl;         R.m12 = 0.0f;  R.m13 = sinl;
	R.m21 = -sino * sinl; R.m22 = coso;  R.m23 = sino * cosl;
	R.m31 = -coso * sinl; R.m32 = -sino; R.m33 = coso * cosl;

	return R;
}
