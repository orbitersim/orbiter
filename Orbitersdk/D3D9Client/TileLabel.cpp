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

#pragma warning(disable : 4996)
static char *nameBuffer (const std::string &name)
{
	int len = name.size();
	char *dst = new char[len+1];
	dst[name.copy(dst, len, 1)] = '\0';
	return dst;
}
#pragma warning(default : 4996)

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

	if (tile->smgr->DoLoadIndividualFiles(4)) { // try loading from individual tile file
		sprintf_s(path, MAX_PATH, "%s\\Label\\%02d\\%06d\\%06d.lab", tile->mgr->CbodyName(), lvl+4, ilat, ilng);
		tile->mgr->Client()->TexturePath(path, texpath);

		std::ifstream ifs(texpath);
		if (ifs.good()) {
			ifs >> typestr >> lat >> lng >> altstr;
			std::getline(ifs, name, '\n');

			while(ifs.good()) {
				if (nlabel == nbuf) { // grow buffer
					TLABEL **tmp = new TLABEL*[nbuf+=16];
					if (nlabel) {
						memcpy(tmp, label, nlabel*sizeof(TLABEL*));
						delete []label;
					}
					label = tmp;
				}
				label[nlabel] = new TLABEL;
				label[nlabel]->lat = lat * RAD;
				label[nlabel]->lng = lng * RAD;
				label[nlabel]->alt = toDoubleOrNaN(altstr);
				label[nlabel]->labeltype = typestr;
				label[nlabel]->pos.x = label[nlabel]->pos.y = label[nlabel]->pos.z = 0.0;
				label[nlabel++]->label = nameBuffer(name);

				ifs >> typestr >> lat >> lng >> altstr;
				std::getline(ifs, name, '\n');
			}
		}
	}
	if (!nlabel && tile->smgr->ZTreeManager(4)) { // try loading from compressed archive
		BYTE *buf;
		ZTreeMgr *mgr = tile->smgr->ZTreeManager(4);
		DWORD ndata = mgr->ReadData(lvl+4, ilat, ilng, &buf);
		if (ndata) {
			std::istringstream iss((char*)buf);
			iss >> typestr >> lat >> lng >> altstr;
			std::getline(iss, name, '\n');

			while (iss.good()) {
				if (nlabel == nbuf) { // grow buffer
					TLABEL **tmp = new TLABEL*[nbuf+=16];
					if (nlabel) {
						memcpy(tmp, label, nlabel*sizeof(TLABEL*));
						delete []label;
					}
					label = tmp;
				}
				label[nlabel] = new TLABEL;
				label[nlabel]->lat = lat * RAD;
				label[nlabel]->lng = lng * RAD;
				label[nlabel]->alt = toDoubleOrNaN(altstr);
				label[nlabel]->labeltype = typestr;
				label[nlabel]->pos.x = label[nlabel]->pos.y = label[nlabel]->pos.z = 0.0;
				label[nlabel++]->label = nameBuffer(name);

				if (iss.tellg() >= ndata) break;

				iss >> typestr >> lat >> lng >> altstr;
				std::getline(iss, name, '\n');
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
	int x, y, nl, scale;
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

				// Crude, but seems to work (UTF-8 -> WC -> ANSI)
				static WCHAR wlabel[256];
				static CHAR label[256];
				MultiByteToWideChar(CP_UTF8, 0, renderlabel[i]->label, -1, wlabel, 256);
				WideCharToMultiByte(CP_ACP, 0, wlabel, -1, label, 256, NULL, false);

				//skp->TextW(x+scale+2, y-scale-1, wlabel, wcslen(wlabel));
				skp->TextEx(float(x + scale + 4), float(y - scale - 1), label);
			}
		}
	}
}
