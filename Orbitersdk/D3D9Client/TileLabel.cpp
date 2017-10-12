// ==============================================================
// TileLabel.cpp
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2017 Martin Schweiger (martins/apogee)
//                    Peter Schneider (Kuddel)
// ==============================================================

#include "TileLabel.h"
#include <limits>
#include <sstream>

TileLabel *TileLabel::Create (const SurfTile *stile)
{
	TileLabel *label = new TileLabel(stile);
	label->Read();  // read label list from tile file

	const int lvlshift = 5;
	SurfTile *ancestor = NULL;
	if (stile->lvl >= lvlshift) {
		QuadTreeNode<SurfTile> *nd4 = stile->node->Ancestor(lvlshift);
		if (nd4) ancestor = nd4->Entry();
	} else if (stile->lvl >= lvlshift-3) {
		ancestor = stile->smgr->GlobalTile(stile->lvl-lvlshift);
	}
	if (ancestor) label->ExtractAncestorData(ancestor);

	if (label->nlabel || label->nrenderlabel) {
		return label;
	} else {
		delete label;
		return NULL;
	}
}

TileLabel::TileLabel (const SurfTile *stile)
	: tile(stile)
	, nlabel(0), nrenderlabel(0),
	nbuf(0), nrenderbuf(0)
{
}

TileLabel::~TileLabel ()
{
	if (nbuf)
	{
		for (DWORD i = 0; i < nlabel; ++i) {
			delete label[i];
		}
		delete []label;
	}
	if (nrenderbuf)
	{
		delete []renderlabel;
		// delete the list, not the labels themselves, since they are just
		// references to ancestor entries
	}
}


// ---------------------------------------------------------------------------
// String Helper
// ---------------------------------------------------------------------------

static double toDoubleOrNaN (const std::string &str)
{
	return (str[0] == 'N' || str[0] == 'n') // "NaN" or "nan"?
		? std::numeric_limits<double>::quiet_NaN()
        : atof(str.c_str());
}

static int _wbufferSize = 0;
static std::auto_ptr<WCHAR> _wbuffer; // this should get destroyed @ shutdown

static LPWSTR GetWBuffer (const std::string &name, int *_len, int _stopLen = -1)
{
	LPWSTR dst = NULL;
	int len = MultiByteToWideChar(CP_UTF8, 0, name.c_str(), _stopLen, NULL, 0);
	if (len) {
		// Grow buffer?
		if (len > _wbufferSize) {
			_wbuffer.reset(new WCHAR[len+16]);
			_wbufferSize = len+16;
		}
		dst = _wbuffer.get();
		MultiByteToWideChar(CP_UTF8, 0, name.c_str(), _stopLen, dst, len);
	}
	*_len = len ? len - 1 : 0;
	return dst;
}

// ---------------------------------------------------------------------------

static void appendName (LPSTR *buffer, int *len, const std::string &name)
{
	size_t _len = name.size();
	if (!_len) return;

	// Create new name buffer
	size_t size = *len + _len + (*len ? 2 : 1); // either '\n' + '\0' or just '\0'
	LPSTR dst = new CHAR[size];

	// Previous content?
	size_t offs = 0;
	if (*len) {
		strcpy_s(dst, size, *buffer);
		dst[*len] = '\n';
		offs = *len + 1;
		delete[] *buffer;
	}

	// Copy new 'name'
	strcpy_s(dst+ offs, size-offs, name.c_str());

	*buffer = dst;
	*len = size - 1; // length is WITHOUT terminating zero
}

// ---------------------------------------------------------------------------

void TileLabel::StoreLabel (TLABEL *l, const std::string &name)
{
	// Check if we've already a location..
	for (DWORD i = 0; i < nlabel; ++i) {
		if (l->lat == label[i]->lat && l->lng == label[i]->lng && l->labeltype == label[i]->labeltype) {
			appendName(&label[i]->label, &label[i]->len, name);
			delete l;
			return;
		}
	}

	// Grow buffer?
	if (nlabel == nbuf) {
		TLABEL **tmp = new TLABEL*[nbuf += 16];
		if (nlabel) {
			memcpy(tmp, label, nlabel * sizeof(TLABEL*));
			delete[]label;
		}
		label = tmp;
	}

	// new loation
	appendName(&l->label, &l->len, name);
	label[nlabel++] = l;
}

// ---------------------------------------------------------------------------

bool TileLabel::Read ()
{
	char path[MAX_PATH], texpath[MAX_PATH];
	int lvl = tile->lvl;
	int ilat = tile->ilat;
	int ilng = tile->ilng;
	double lat, lng;
	char typestr;
	std::string altstr, name;

	// --- Easter Egg ;) ---
	//static bool done = false;
	//if (!done && lvl==8 && ilat==53 && ilng==265 && !strcmp(tile->mgr->CbodyName(),"Earth")) {
	//	TLABEL *item = new TLABEL;
	//	item->lat = 52.12949 * RAD;
	//	item->lng = 6.90243 * RAD;
	//	item->alt = 40;
	//	item->labeltype = 'C';
	//	StoreLabel(item, " Kuddel\nwas here!");
	//	done = true;
	//}

	if (tile->smgr->DoLoadIndividualFiles(4)) { // try loading from individual tile file
		sprintf_s(path, MAX_PATH, "%s\\Label\\%02d\\%06d\\%06d.lab", tile->mgr->CbodyName(), lvl+4, ilat, ilng);
		tile->mgr->Client()->TexturePath(path, texpath);

		std::ifstream ifs(texpath);
		while (ifs >> typestr >> lat >> lng >> altstr >> std::ws) {
			std::getline(ifs, name, '\n');
			TLABEL *item = new TLABEL;
			item->lat = lat * RAD;
			item->lng = lng * RAD;
			item->alt = toDoubleOrNaN(altstr);
			item->labeltype = typestr;
			StoreLabel(item, name);
		}
	}
	if (!nlabel && tile->smgr->ZTreeManager(4)) { // try loading from compressed archive
		BYTE *buf;
		ZTreeMgr *mgr = tile->smgr->ZTreeManager(4);
		DWORD ndata = mgr->ReadData(lvl+4, ilat, ilng, &buf);
		if (ndata) {
			std::istringstream iss((char*)buf);
			while (/*iss.tellg() < ndata &&*/ iss >> typestr >> lat >> lng >> altstr >> std::ws) {
				std::getline(iss, name, '\n');

				TLABEL *item = new TLABEL;
				item->lat = lat * RAD;
				item->lng = lng * RAD;
				item->alt = toDoubleOrNaN(altstr);
				item->labeltype = typestr;
				StoreLabel(item, name);
			}
			tile->smgr->ZTreeManager(4)->ReleaseData(buf);
		}
	}
	return (nlabel > 0);
}

bool TileLabel::ExtractAncestorData (const SurfTile *atile)
{
	if (!atile) { return false; }

	TileLabel *atl = atile->label;
	if (atl && atl->nlabel) {
		TLABEL **alabel = atl->label;
		double lat, lng, latmin, latmax, lngmin, lngmax;
		tile->Extents(&latmin, &latmax, &lngmin, &lngmax);
		for (DWORD i = 0; i < atl->nlabel; i++) {
			lat = alabel[i]->lat;
			lng = alabel[i]->lng;
			if (lat >= latmin && lat < latmax && lng >= lngmin && lng < lngmax) {
				if (nrenderlabel == nrenderbuf) { // grow buffer
					TLABEL **tmp = new TLABEL*[nrenderbuf+=16];
					if (nrenderlabel) {
						memcpy(tmp, renderlabel, nrenderlabel*sizeof(TLABEL*));
						delete []renderlabel;
					}
					renderlabel = tmp;
				}
				renderlabel[nrenderlabel++] = alabel[i];
				if (!alabel[i]->pos.x && !alabel[i]->pos.y && !alabel[i]->pos.z) {
					if (_isnan(alabel[i]->alt))
						alabel[i]->alt = Elevation(lat, lng, latmin, latmax, lngmin, lngmax, tile->mgr->ElevRes());
					double rad = tile->mgr->CbodySize() + alabel[i]->alt;
					oapiEquToLocal(tile->mgr->Cbody(), lng, lat, rad, &alabel[i]->pos);
				}
			}
		}
	}
	return (nrenderlabel > 0);
}

double TileLabel::Elevation (double lat, double lng, double latmin, double latmax, double lngmin, double lngmax, double elev_res) const
{
	INT16 *elev = tile->ggelev;
	if (!elev) return 0.0;

	int blockRes = tile->mgr->GridRes();
	INT16 *elev_base = elev+TILE_ELEVSTRIDE+1; // strip padding
	double latidx = (lat-latmin) * blockRes/(latmax-latmin);
	double lngidx = (lng-lngmin) * blockRes/(lngmax-lngmin);
	int lat0 = (int)latidx;
	int lng0 = (int)lngidx;
	elev = elev_base + lat0*TILE_ELEVSTRIDE + lng0;
	double w_lat = latidx-lat0;
	double w_lng = lngidx-lng0;
	double e01 = elev[0]*(1.0-w_lng) + elev[1]*w_lng;
	double e02 = elev[TILE_ELEVSTRIDE]*(1.0-w_lng) + elev[TILE_ELEVSTRIDE+1]*w_lng;
	double e = e01*(1.0-w_lat) + e02*w_lat;
	return e*elev_res;
}

void TileLabel::Render (oapi::Sketchpad2 *skp, oapi::Font **labelfont, int *fontidx)
{
	if (!nrenderlabel) { return; }// nothing to render

	const int symscale[4] = {5, 6, 8, 10};
	DWORD i;
	COLORREF col, pcol = 0;
	char symbol;
	int x, y, nl, scale, len, partLen;
	const oapi::GraphicsClient::LABELTYPE *lspec;
	VECTOR3 sp, dir;
	bool active;
	const OBJHANDLE &hPlanet = tile->mgr->Cbody();
	Scene *pScene = tile->mgr->Client()->GetScene();

    if (pScene->GetCameraProxyBody() != hPlanet) { return; } // do not render other body's labels

	VECTOR3 Ppl;
	oapiGetGlobalPos(hPlanet, &Ppl);                 // planet global position
	const VECTOR3 *Pcam = &pScene->GetCamera()->pos; // camera global position
	MATRIX3 Rpl;
	oapiGetRotationMatrix(hPlanet, &Rpl);            // planet rotation matrix
	VECTOR3 campos = tmul(Rpl, *Pcam - Ppl);         // camera pos in planet frame


	// Get current "rotation step"
	static bool currentRotStep = false;
	double now = oapiGetSysTime();
	static double lastT = now;
	if (now - lastT > 1.5)
	{
		lastT = now;
		currentRotStep = !currentRotStep;
	}

	for (i = 0; i < nrenderlabel; ++i) {
		VECTOR3 camlabelpos = campos-renderlabel[i]->pos;
		if (dotp (renderlabel[i]->pos, camlabelpos) >= 0.0) {
			double fontscale = 1e4/length(camlabelpos)*(13-min(tile->lvl,12)*1);
			int idx = max(0, min(3, (int)fontscale));
			if (idx != *fontidx) {
				skp->SetFont(labelfont[idx]);
				*fontidx = idx;
			}
			scale = symscale[idx];
			sp = mul(Rpl, renderlabel[i]->pos) + Ppl - *Pcam;
			dir = unit(sp);
			if (pScene->CameraDirection2Viewport(dir, x, y)) {

				symbol = 0; // undefined
				if (nl = tile->smgr->Client()->GetSurfaceMarkerLegend(hPlanet, &lspec)) {
					for (int j = 0; j < nl; ++j) {
						if (renderlabel[i]->labeltype == lspec[j].labelId) {
							symbol = lspec[j].markerId;
							col = lspec[j].col;
							active = lspec[j].active;
							break;
						}
					}
				}
				if (!active) continue;

				if (!symbol) { // default
					symbol = 'S';
					col = RGB(255,255,255);
				}
				if (col != pcol) {
					skp->SetTextColor(col);
					pcol = col;
				}
				switch (symbol) {
				case 'O': // circle
					skp->Ellipse(x-scale, y-scale, x+scale+1, y+scale+1);
					break;
				case 'D': // Delta
					skp->MoveTo(x, y-scale);
					skp->LineTo(x-scale, y+scale+1);
					skp->LineTo(x+scale+1, y+scale+1);
					skp->LineTo(x, y-scale);
					break;
				case 'N': // Nabla
					skp->MoveTo(x, y+scale+1);
					skp->LineTo(x-scale, y-scale);
					skp->LineTo(x+scale+1, y-scale);
					skp->LineTo(x, y+scale+1);
					break;
				default:
					skp->Rectangle(x-scale, y-scale, x+scale+1, y+scale+1);
					break;
				}

				partLen = LimitAndRotateLongLabelList(renderlabel[i], currentRotStep);

				LPWSTR wname = GetWBuffer(renderlabel[i]->label, &len, partLen+1);
				skp->TextW(x + scale + 2, y - scale - 1, wname, len);
			}
		}
	}
}

//std::vector<std::wstring> split(std::wstring s)
//{
//	static std::vector<std::wstring> parts;
//	std::wstringstream iss(s);
//	std::wstring part;
//
//	parts.clear();
//	while (std::getline(iss, part, L'\n')) {
//		parts.push_back(part);
//	}
//
//	return parts;
//}

//std::wstring join(std::vector<std::wstring> v)
//{
//	static std::wstring s;
//	std::wstring first(v.front());
//	v.erase(v.begin());
//	s = first;
//	for (auto it = v.begin(); it != v.end(); ++it) {
//		s += L'\n' + *it;
//	}
//	return s;
//}

std::vector<std::string> split (std::string s)
{
	static std::vector<std::string> parts;
	std::stringstream iss(s);
	std::string part;

	parts.clear();
	while (std::getline(iss, part, '\n')) {
		parts.push_back(part);
	}

	return parts;
}

std::string join (std::vector<std::string> v)
{
	static std::string s;
	std::string first(v.front());
	v.erase(v.begin());
	s = first;
	for (auto it = v.begin(); it != v.end(); ++it) {
		s += '\n' + *it;
	}
	return s;
}

int TileLabel::LimitAndRotateLongLabelList(TLABEL *l, bool rotStep)
{
	int partLen = l->len;

	// Count lines
	int nLines = 1;
	for (CHAR *c = l->label; *c; ++c) {
		if (*c == '\n') {
			++nLines;
		}
	}

	if (nLines > 3)
	{
		if (rotStep == l->rotStep)
		{
			l->rotStep = !l->rotStep;
			// split
			auto lines = split(l->label);
			// roll over
			std::string first = lines.front();
			lines.erase(lines.begin());
			lines.push_back(first);
			// re-join
			std::string label = join(lines);
			strcpy_s(l->label, l->len+1, label.c_str());
		}

		// Calculate "render stop" length
		nLines = 0;
		for (CHAR *c = l->label; *c; ++c) {
			if (*c == '\n') {
				if (++nLines == 4) { // render 4 lines
					partLen = c - l->label;
					break;
				}
			}
		}

	}
	return partLen;
}
