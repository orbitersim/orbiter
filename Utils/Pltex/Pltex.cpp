// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// =========================================================================
// pltex for Orbiter:
// Create texture files for planetary surfaces and cloud layers
// =========================================================================

// =========================================================================
// File formats
//
// 1. Contents file for planet surface at resolution levels 1-8
// ------------------------------------------------------------
// Naming convention: <planet>_lmask.bin
// This file is only required if the planet contains a land-water mask for
// specular reflection and/or a night (city) light texture, in
// <planet>_lmask.tex. If the planet contains only a simple surface texture
// (<planet>.tex), neither <planet>_lmask.tex nor <planet>_lmask.bin are
// required.
//
// File format: (binary)
// 'PLTA0100'   8 bytes (ID+version string)
// hsize        4 bytes (size of header in bytes)
// flag         4 bytes (bitflag content information)
// npatch       4 bytes (number of patch entries)
// minres       1 byte  (min. resolution level)
// maxres       1 byte  (max. resolution level)
// followed by npatch x patch entries, 2 bytes each
// The 'flag' entry contains:
// - bit 0: some patches contain diffuse (land) parts
// - bit 1: some patches contain specular (water) parts
// - bit 2: some patches contain night lights
//
// byte 0 of each patch entry contains:
// - bit 0: patch contains diffuse (land) parts
// - bit 1: patch contains specular (water) parts
// - bit 2: patch contains night lights
// byte 1 is currently not used and set to 0
//
// A texture for this patch will be present in <planet>_lmask.tex if
// - bits 0 and 1 are set (land-water mask in the alpha channel), or
// - bit 2 is set (city lights in the RGB channel)
// =========================================================================

#include <iostream>
#include <iomanip>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <io.h>
#include <conio.h>
#include <fcntl.h>
#include <process.h>
#include <direct.h>
#include <windows.h>
#include <math.h>
#include <ddraw.h>

using namespace std;

static const int patchidx[9] = {0, 1, 2, 3, 5, 13, 37, 137, 501};
// index to the first texture of a given surface patch level

static const char *TileID = "PLTS0101";

typedef union {
	BYTE data[3];
	struct { BYTE r, g, b; };
} RGB;

typedef BYTE Alpha;

#pragma pack(push,1)
struct TILEFILESPEC {  // format for tile information file entry
	DWORD sidx;        // position of surface texture in file (or -1 if texture not present)
	DWORD midx;        // position of land/water mask texture in file (or -1 if not present; only required if tile contains both land and water areas)
	DWORD eidx;        // position of elevation data block (currently not used and always set to -1)
	DWORD flags;       // see below
	DWORD subidx[4];   // subtile indices
};
// Bitflags in TILEFILESPEC.flags:
// bit 0: tile area contains land
// bit 1: tile area contains water
// bit 2: tile area contains city lights

struct LMASKFILEHEADER { // file header for contents file at level 1-8
	char id[8];          //    ID+version string
	DWORD hsize;         //    header size
	DWORD flag;          //    bitflag content information
	DWORD npatch;        //    number of patches
	BYTE minres;         //    min. resolution level
	BYTE maxres;         //    max. resolution level
};
#pragma pack(pop)

struct PATCHDATA {
	int which;
	RGB *img, *patch;
	RGB *limg, *lpatch;
	Alpha *aimg, *apatch;
	int mapw, maph;
	int maxlevel;
	bool mixed, skipwater, nlights, mipmap;
	double landlimit, mixed_tol, light_tol;
	double latmin, latmax;
	double lngmin, lngmax;
	int ntile, ntot, sout, mout, sidx, midx;
	DWORD patchflag;
	TILEFILESPEC *tfs;
	int ntfs;
	FILE *texf, *mtexf;
};

struct MERGEDATA {
	TILEFILESPEC *td1, *td2, *tdm;
	DWORD ntile, ns, nm;
	FILE *texf1, *texf2, *texfm;
	FILE *maskf1, *maskf2, *maskfm;
};

const int HEMISPHERE_BOTH = 0;
const int HEMISPHERE_NORTH = 1;
const int HEMISPHERE_SOUTH = 2;

const int PS = 256; // patch size: size of patch textures
const DWORD MAXLEVEL = 14; // max supported resolution level

// min. map dimensions for given level
const int LevelW[10] = {0, PS/4, PS/2, PS, 2*PS, 4*PS, 8*PS, 16*PS, 32*PS, 64*PS};
const int LevelH[10] = {0, PS/4, PS/2, PS, PS,   2*PS, 4*PS,  8*PS, 16*PS, 32*PS};

void CreateGlobalSurface ();
void CreateLocalArea ();
bool CreateSubPatch (double baselat0, double baselat1, double baselng0, double baselng1, int idx, PATCHDATA &pd, int lvl);
void CreateCloudMap ();
void MergeTextures ();
void MergeTrees (MERGEDATA &md, DWORD idx1, DWORD idx2, DWORD idxm, bool baselvl);
void SortTextures (const char *rootname);
void CopyTexturesAtLevel (TILEFILESPEC *td, DWORD baseidx, DWORD lvl, DWORD tgtlvl, DWORD &texidx, DWORD &maskidx,
	FILE *srctexf, FILE *srcmaskf, FILE *tgttexf, FILE *tgtmaskf);

void SetOutputHeader (BITMAPFILEHEADER &bmfh, BITMAPINFOHEADER &bmih, LONG w, LONG h);
RGB *BinaryCompress (RGB *src, LONG srcw, LONG srch, LONG tgtw, long tgth);

BYTE *Compress (int nch, BYTE *src, LONG srcw, LONG srch, LONG tgtw, long tgth, int which = HEMISPHERE_BOTH);
RGB *CompressRGB (RGB *src, LONG srcw, LONG srch, LONG tgtw, long tgth, int which = HEMISPHERE_BOTH);
Alpha *CompressAlpha (Alpha *src, LONG srcw, LONG srch, LONG tgtw, long tgth, int which = HEMISPHERE_BOTH);
// Compress bitmap/alphamap from srcw/srch to tgtw, tgth (either all or north/south hemisphere only)

void ExtractPatch (int nch, BYTE *src, LONG srcw, LONG srch, LONG x0, LONG y0, BYTE *tgt, LONG tgtw, LONG tgth);
void ExtractPatchRotated (int nch, BYTE *src, LONG srcw, LONG srch, LONG x0, LONG y0, BYTE *tgt, LONG tgtw, LONG tgth);
void ExtractPatchRGB (RGB *src, LONG srcw, LONG srch, LONG x0, LONG y0, RGB *tgt, LONG tgtw, LONG tgth, int which = HEMISPHERE_BOTH);
void ExtractPatchAlpha (Alpha *src, LONG srcw, LONG srch, LONG x0, LONG y0, Alpha *tgt, LONG tgtw, LONG tgth, int which = HEMISPHERE_BOTH);
void ExtractPatchRGBA (RGB *src, Alpha *asrc, LONG srcw, LONG srch, LONG x0, LONG y0, RGB *tgt, Alpha *atgt, LONG tgtw, LONG tgth);
void ExtractPatchRotatedRGBA (RGB *src, Alpha *asrc, LONG srcw, LONG srch, LONG x0, LONG y0, RGB *tgt, Alpha *atgt, LONG tgtw, LONG tgth);
void ExtractPatchRGBA (RGB *src, Alpha *asrc, LONG srcw, LONG srch, LONG x0, LONG y0, RGB *tgt, Alpha *atgt, LONG tgtw, LONG tgth, int which);

void SamplePatch (RGB *img, LONG imgw, LONG imgh, double maplng0, double maplng1, double maplat0, double maplat1,
				  RGB *tgt, LONG tgtw, LONG tgth, double tgtlng0, double tgtlng1, double tgtlat0, double tgtlat1, int which);
void SampleAPatch (Alpha *img, LONG imgw, LONG imgh, double maplng0, double maplng1, double maplat0, double maplat1,
				   Alpha *tgt, LONG tgtw, LONG tgth, double tgtlng0, double tgtlng1, double tgtlat0, double tgtlat1, int which);

DWORD CatDDS (FILE *dds, RGB *img, Alpha *aimg, LONG imgw, LONG imgh, bool force = false, bool mipmap = false);
// Adds the specified texture with optional alpha channel to the texture file.
// Returns the total size of the texture data, including any mipmaps (or 0 if no
// data were written because of inconsistent parameters)

WORD CatMaskDDS (FILE *texf, RGB *img, Alpha *aimg, LONG imgw, LONG imgh);

DWORD CopyDDS (FILE *ftgt, FILE *fsrc, DWORD idx, bool idx_is_ofs = false);
// Copy a texture from a given position in a source file to the end of a target file.
// Returns the size of the copied data block, including any mipmaps (or 0 if no
// data were written because of an error)

void ReadBMP_data (char *fname, LONG &mapw, LONG &maph, WORD &bpp);
// Read image width, height, and bit depth from BMP file

RGB *ReadBMP (char *fname, LONG &mapw, LONG &maph, WORD &bpp);
// Read bitmap file into RGB buffer and return dimensions and max texture level

RGB *ReadBMP_band (char *fname, LONG &mapw, LONG &maph, WORD &bpp, LONG line0, LONG nlines);
// Read band from bitmap file into RGB buffer

Alpha *ReadBMPAlpha (char *fname, LONG &mapw, LONG &maph, WORD &bpp);
// Read blue channel as alpha information from bitmap file

Alpha *ReadBMPAlpha_band (char *fname, LONG &mapw, LONG &maph, WORD &bpp, LONG line0, LONG nlines);
// Read band from bitmap (blue channel) as alpha information

LONG TransparentCount (Alpha *atgt, LONG w, LONG h);
// count transparent pixels in a patch

LONG BrightCount (RGB *tgt, LONG w, LONG h);
// count pixels brighter than medium grey in a patch

void ErodeLights (RGB *limg, Alpha *aimg, LONG imgw, LONG imgh);
// Erase emissive pixels from water edges to avoid visual artefacts

void DeleteTargets (RGB **tgt, Alpha **atgt = 0, RGB **ltgt = 0);
void FatalError (const char *msg);
void InitProgress (int ntot, int len);
void SetProgress (int p);
void IncProgress ();


const double eps = 1e-10;
const char *dxtex = ".\\dxtex.exe";
char g_cwd[256];
char fname[256] = "\0";
char aname[256] = "\0";
char lname[256] = "\0";
bool have_alpha = false;
bool binary_alpha = true;
bool selective_alpha = false;
int g_minres = 0, g_maxres = 0;
double g_tol = 0.0;        // tolerance for suppressing opaque/transparent pixels in a tile
double g_light_tol = 0.0;  // tolerance for suppressing light pixels in a tile
int g_nsuppressed = 0;     // number of opacity/transparency suppressed tiles

const int nband = 8;
const int np[8] = {6,12,18,24,28,30,32,32};

//FILE *alphabin_f = 0;

int main (int argc, char *argv[])
{
	int i;
	char task;

	if (!_getcwd (g_cwd, 256)) FatalError ("Cannot get working directory");
	strcat (g_cwd, "\\");

	for (i = 1; i < argc; i++) {
		if (argv[i][0] != '-') FatalError ("Command line parsing error");
		switch (argv[i][1]) {
		case 'i':
			strcpy (fname, argv[++i]);
			break;
		case 'a':
			strcpy (aname, argv[++i]);
			have_alpha = true;
			break;
		case 'l':
			sscanf (argv[++i], "%d", &g_minres);
			break;
		case 'h':
			sscanf (argv[++i], "%d", &g_maxres);
			break;
		}
	}

	cout << "+-----------------------------------------------------------------------+\n";
	cout << "|             pltex: Planetary texture manager for ORBITER              |\n";
	cout << "|        Build: " << __DATE__ << "      (c) 2001-2008 Martin Schweiger         |\n";
	cout << "+-----------------------------------------------------------------------+\n\n";

	for (;;) {
		cout << "Select a task:\n\n";
		cout << "(G) Create a global planetary surface texture (resolution level <= 8)\n";
		cout << "(L) Create a local high-resolution surface area (resolution level > 8)\n";
		cout << "(C) Create an opaque or semi-transparent cloud map\n";
		cout << "(M) Merge high-resolution texture files\n";
		cout << "(Q) Quit\n";
		cout << "\n>> [G|L|C|M|Q]: ";
		cin >> task;
		cout << endl << endl;
		switch (toupper(task)) {
		case 'G':
			CreateGlobalSurface();
			MessageBeep (-1);
			return 0;
		case 'L':
			CreateLocalArea();
			MessageBeep (-1);
			return 0;
		case 'C':
			CreateCloudMap();
			MessageBeep (-1);
			return 0;
		case 'M':
			MergeTextures();
			MessageBeep (-1);
			return 0;
		case 'S': // undocumented
			SortTextures (0);
			MessageBeep (-1);
			return 0;
		case 'Q':
			return 0;
		}
	}
}

void CreateGlobalSurface ()
{
	int i, k, which, lmax;
	int minres = g_minres, maxres = g_maxres;
	char c;
	LONG mapw, maph;
	WORD bpp;
	DWORD patchflag;
	DWORD idx = 0;
	RGB *img = 0, *tgt, *patch = 0;
	RGB *limg = 0, *ltgt = 0, *lpatch = 0;
	Alpha *aimg = 0, *atgt = 0, *apatch = 0;
	FILE *texf = 0, *mtexf = 0;
	int maxlevel = 8;
	char *cbuf = g_cwd+strlen(g_cwd);
	bool mixed = false, nlights = false, needmask = false;
	double mixed_tol;
	WORD pflag[501];

	cout << "              CREATE GLOBAL PLANETARY SURFACE TEXTURE\n";
	cout << "              ---------------------------------------\n\n";

	if (!fname[0]) {
		cout << "Enter the file name for the bitmap representing the planetary surface\n";
		cout << "(must be in 8-bit or 24-bit BMP format). The bitmap must contain the\n";
		cout << "global surface in cylindrical projection, from 180 deg West (left\n";
		cout << "edge) to 180 deg East (right edge), and from 90 deg South (bottom\n";
		cout << "edge) to 90 deg North (top edge). The width:height ratio of the\n";
		cout << "bitmap should be approximately 2:1 for best results.\n\n";
		cout << ">> Surface map file name (.bmp): ";
		cin >> fname;
		if (!_stricmp (fname+(strlen(fname)-4), ".bmp"))
			fname[strlen(fname)-4] = '\0';
		cout << endl;
	}

	cout << "\nDoes the surface contain any areas that should show specular light\n";
	cout << "reflection (e.g. oceans, rivers or icecaps)?\n\n";
	cout << "(D) Only diffusely reflecting surface\n";
	cout << "(S) Only specularly reflecting surface\n";
	cout << "(B) Both diffuse and specular surfaces\n";
	cout << "\n>> [D|S|B]: ";
	cin >> c;
	cout << endl;

	switch (toupper(c)) {
	case 'D':
		patchflag = 1;
		break;
	case 'S':
		patchflag = 2;
		break;
	case 'B':
		patchflag = 0; // needs to be determined individually
		mixed = true;
		break;
	default:
		FatalError ("Invalid selection");
		break;
	}

	if (mixed) {
		cout << "\nTo prepare a mixed-reflection surface, a bitmap containing a land-water\n";
		cout << "mask is required. This bitmap must be of the same size and cover the same\n";
		cout << "area as the surface bitmap. It must be in 8-bit or 24-bit BMP format, and\n";
		cout << "contain only 2 colours: white for any specular reflection areas (water),\n";
		cout << "and black for diffuse reflection areas (land).\n\n";
		cout << ">> Mask map file name (.bmp): ";
		cin >> aname;
		if (!_stricmp (aname+(strlen(aname)-4), ".bmp"))
			aname[strlen(aname)-4] = '\0';
		cout << endl;

		cout << "\nTiles which contain a mixture of land and water surfaces require larger\n";
		cout << "texture files and can degrade rendering performance. Therefore, mixed\n";
		cout << "tiles which contain only a small proportion of land or water pixels can be\n";
		cout << "rendered entirely as water or land tiles, respectively, to improve\n";
		cout << "performance. Up to what percentage of land or water area should a tile be\n";
		cout << "considered uniformly as water or land surface? A higher percentage will\n";
		cout << "decrease texture file sizes and improve performance, but can lead to errors\n";
		cout << "in specular reflection. A value of 0 will force dual rendering even if only\n";
		cout << "a single pixel is marked as land or water surface. [Recommended value: 0.2]\n";
		cout << "\n>> Mixed cover limit for uniform rendering (%) [0-100]: ";
		cin >> mixed_tol;
		cout << endl;
		mixed_tol *= 0.01;
		g_tol = mixed_tol;
	}

	cout << "\nCity lights: Does the surface contain areas that emit light at night\n";
	cout << "(illuminated cities etc.)?\n\n";
	cout << "(N) No\n";
	cout << "(Y) Yes\n";
	cout << "\n>> [N|Y]: ";
	cin >> c;
	cout << endl;
	switch (toupper(c)) {
	case 'N':
		nlights = false;
		break;
	case 'Y':
		nlights = true;
		break;
	default:
		FatalError ("Invalid selection");
		break;
	}

	if (nlights) {
		cout << "\nTo prepare city-light textures, a bitmap containing the distribution of\n";
		cout << "emissive light during nighttime is required. This bitmap must be of the\n";
		cout << "same size and cover the same area as the surface bitmap. It must be in\n";
		cout << "8-bit or 24-bit BMP format. It should be black in non-lit areas, and bright\n";
		cout << "(but not necessarily white) in lit areas.\n\n";
		cout << ">> City light map file name: (.bmp): ";
		cin >> lname;
		if (!_stricmp (lname+(strlen(lname)-4), ".bmp"))
			lname[strlen(lname)-4] = '\0';
		cout << endl;

		cout << "\nTiles which contain only a very small amount of city lights can be\n";
		cout << "rendered more efficiently without a light texture.\n";
		cout << "Up to what percentage of city light coverage should a tile be rendered\n";
		cout << "without lights? A higher percentage will decrease texture file size and\n";
		cout << "improve performance, but can lead to the loss of night lights in sparsely\n";
		cout << "lit areas. A value of 0 will force the inclusion of light textures even\n";
		cout << "if only a single pixel is illuminated. [Recommended value: 0.1]\n";
		cout << "\n>> Cutoff limit for city lights (%) [0-100]: ";
		cin >> g_light_tol;
		g_light_tol *= 0.01;
	}
	needmask = mixed || nlights;

	if (maxres < 1) {
		cout << "\nSpecify the resolution range for the texture map. You need to enter\n";
		cout << "two numbers: the lowest and highest resolution to generate. The valid\n";
		cout << "range for both is 1 (lowest) to 8 (highest). The upper limit must be\n";
		cout << "greater or equal to the lower limit. The lower limit should normally\n";
		cout << "be 1. If you set the upper limit to 8, pltex will produce all\n";
		cout << "resolutions supported by the bitmap size.\n";
		cout << "\n>> Resolution range (1..8) [min max]: ";
		cin >> minres >> maxres;
		cout << endl;
	}
	if (minres < 1 || maxres > maxlevel || minres > maxres) FatalError ("Invalid resolution range");

	ReadBMP_data (fname, mapw, maph, bpp);

	// check that required resolution level is supported by map
	for (lmax = maxlevel; lmax && (LevelW[lmax] > mapw || LevelH[lmax] > maph); lmax--);
	if (!lmax) FatalError ("Bitmap too small to generate textures");
	if (lmax < minres) FatalError ("Bitmap too small to generate requested resolution levels");
	if (lmax < maxres) {
		maxres = lmax;
		cout << "Warning: Bitmap only supports resolution level up to " << maxres << endl;
		cout << "         Resolution limit adjusted" << endl;
	}

	img = ReadBMP (fname, mapw, maph, bpp);
	patch = new RGB[PS*PS];
	strcpy (cbuf, fname);
	strcat (cbuf, ".tex");
	texf = fopen (cbuf, "wb");

	if (mixed) {
		LONG amapw, amaph;
		WORD abpp;
		ReadBMP_data (aname, amapw, amaph, abpp);
		if (amapw != mapw || amaph != maph)
			FatalError ("Incompatible bitmap sizes");
		aimg = ReadBMPAlpha (aname, mapw, maph, bpp);
		apatch = new Alpha[PS*PS];
		memset (apatch, 0, sizeof(Alpha)*PS*PS);
	}

	if (nlights) {
		LONG nmapw, nmaph;
		WORD nbpp;
		ReadBMP_data (lname, nmapw, nmaph, nbpp);
		if (nmapw != mapw || nmaph != maph)
			FatalError ("Incompatible bitmap sizes");
		limg = ReadBMP (lname, mapw, maph, bpp);
	}

	if (needmask) {
		lpatch = new RGB[PS*PS];
		memset (lpatch, 0, sizeof(RGB)*PS*PS);
		//apatch = new Alpha[PS*PS];
		//memset (apatch, 0, sizeof(Alpha)*PS*PS);
		strcpy (cbuf, fname);
		strcat (cbuf, "_lmask.tex");
		mtexf = fopen (cbuf, "wb");
		strcpy (cbuf, fname);
		strcat (cbuf, "_lmask.bin");
		selective_alpha = true;
	}

	cout << "-------------------------------------------------\n";
	cout << "Surface bitmap file: " << fname << ".bmp\n";
	if (mixed)
		cout << "Land-water mask file: " << aname << ".bmp\n";
	if (nlights)
		cout << "City lights file: " << lname << ".bmp\n";
	cout << "Bitmap dimensions: " << mapw << "(W) x " << maph << "(H) x " << bpp << "(BPP)\n";
	cout << "Using patch size " << PS << " x " << PS << endl;
	cout << "Reflection mode: ";
	switch (patchflag) {
	case 0: cout << "diffuse+specular\n"; break;
	case 1: cout << "diffuse only\n"; break;
	case 2: cout << "specular only\n"; break;
	}
	cout << "Resolution range: " << minres << " - " << maxres << endl;

	cout << "\nProcessing ..." << endl << endl;
	cout << "Level |  Resolution | patches" << endl;
	cout << "-----------------------------" << endl;

	if (minres <= 1 && maxres >= 1) { // resolution level 1: 64x64
		cout << "  1   | " << setw(4) << PS/4 << " x " << setw(4) << PS/4 << " |    1     ";
		InitProgress (1, 40);
		tgt  = CompressRGB (img, mapw, maph, PS/4, PS/4);
		CatDDS (texf, tgt, 0, PS/4, PS/4);
		if (mixed)    atgt = CompressAlpha (aimg, mapw, maph, PS/4, PS/4);
		if (nlights)  ltgt = CompressRGB (limg, mapw, maph, PS/4, PS/4);
		if (needmask) pflag[idx++] = CatMaskDDS (mtexf, ltgt, atgt, PS/4, PS/4);
		DeleteTargets (&tgt, &atgt, &ltgt);
		IncProgress ();
	}

	if (minres <= 2 && maxres >= 2) { // resolution level 2: 128x128
		cout << "  2   | " << setw(4) << PS/2 << " x " << setw(4) << PS/2 << " |    1     ";
		InitProgress (1, 40);
		tgt  = CompressRGB (img, mapw, maph, PS/2, PS/2);
		CatDDS (texf, tgt, 0, PS/2, PS/2);
		if (mixed)    atgt = CompressAlpha (aimg, mapw, maph, PS/2, PS/2);
		if (nlights)  ltgt = CompressRGB (limg, mapw, maph, PS/2, PS/2);
		if (needmask) pflag[idx++] = CatMaskDDS (mtexf, ltgt, atgt, PS/2, PS/2);
		DeleteTargets (&tgt, &atgt, &ltgt);
		IncProgress ();
	}

	if (minres <= 3 && maxres >= 3) { // resolution level 3: 256x256
		cout << "  3   | " << setw(4) << PS << " x " << setw(4) << PS << " |    1     ";
		InitProgress (1, 40);
		tgt  = CompressRGB (img, mapw, maph, PS, PS);
		CatDDS (texf, tgt, 0, PS, PS);
		if (mixed)    atgt = CompressAlpha (aimg, mapw, maph, PS, PS);
		if (nlights)  ltgt = CompressRGB (limg, mapw, maph, PS, PS);
		if (needmask) pflag[idx++] = CatMaskDDS (mtexf, ltgt, atgt, PS, PS);
		DeleteTargets (&tgt, &atgt, &ltgt);
		IncProgress ();
	}

	if (minres <= 4 && maxres >= 4) { // resolution level 4: 512x256 (2 bitmaps)
		cout << "  4   | " << setw(4) << 2*PS << " x " << setw(4) << PS << " |    2     ";
		InitProgress (2, 40);
		tgt  = CompressRGB (img, mapw, maph, 2*PS, PS);
		if (nlights) ltgt = CompressRGB (limg, mapw, maph, 2*PS, PS);
		if (mixed)   atgt = CompressAlpha (aimg, mapw, maph, 2*PS, PS);
		for (i = 0; i < 2; i++) {
			ExtractPatchRGB (tgt, 2*PS, PS, i*PS, 0, patch, PS, PS);
			CatDDS (texf, patch, 0, PS, PS);
			if (needmask) {
				if (nlights) ExtractPatchRGB (ltgt, 2*PS, PS, i*PS, 0, lpatch, PS, PS);
				if (mixed)   ExtractPatchAlpha (atgt, 2*PS, PS, i*PS, 0, apatch, PS, PS);
				pflag[idx++] = CatMaskDDS (mtexf, lpatch, apatch, PS, PS);
			}
			IncProgress ();
		}
		DeleteTargets (&tgt, &atgt, &ltgt);
	}

	if (minres <= 5 && maxres >= 5) { // resolution level 5: 1024x512 (8 bitmaps)
		cout << "  5   | " << setw(4) << 4*PS << " x " << setw(4) << 2*PS << " |    8     ";
		InitProgress (8, 40);
		tgt  = CompressRGB   (img, mapw, maph, 4*PS, 2*PS);
		if (nlights) ltgt = CompressRGB   (limg, mapw, maph, 4*PS, 2*PS);
		if (mixed)   atgt = CompressAlpha (aimg, mapw, maph, 4*PS, 2*PS);
		for (which = HEMISPHERE_NORTH; which <= HEMISPHERE_SOUTH; which++) {
			for (i = 0; i < 4; i++) {
				ExtractPatchRGB (tgt, 4*PS, 2*PS, i*PS, PS, patch, PS, PS, which);
				CatDDS (texf, patch, 0, PS, PS);
				if (needmask) {
					if (nlights) ExtractPatchRGB (ltgt, 4*PS, 2*PS, i*PS, PS, lpatch, PS, PS, which);
					if (mixed)   ExtractPatchAlpha (atgt, 4*PS, 2*PS, i*PS, PS, apatch, PS, PS, which);
					pflag[idx++] = CatMaskDDS (mtexf, lpatch, apatch, PS, PS);
				}
				IncProgress();
			}
		}
		DeleteTargets (&tgt, &atgt, &ltgt);
	}

	if (minres <= 6 && maxres >= 6) { // resolution level 6: 2048x1024 (24 bitmaps)
		cout << "  6   | " << setw(4) << 8*PS << " x " << setw(4) << 4*PS << " |   24     ";
		InitProgress (24, 40);
		for (which = HEMISPHERE_NORTH; which <= HEMISPHERE_SOUTH; which++) {
			tgt  = CompressRGB   (img, mapw, maph, 4*PS, 4*PS, which);
			if (nlights) ltgt = CompressRGB   (limg, mapw, maph, 4*PS, 4*PS, which);
			if (mixed)   atgt = CompressAlpha (aimg, mapw, maph, 4*PS, 4*PS, which);
			for (i = 0; i < 4; i++) {
				ExtractPatchRGB (tgt, 4*PS, 2*PS, i*PS, PS, patch, PS, PS, which);
				CatDDS (texf, patch, 0, PS, PS);
				if (needmask) {
					if (nlights) ExtractPatchRGB (ltgt, 4*PS, 2*PS, i*PS, PS, lpatch, PS, PS, which);
					if (mixed)   ExtractPatchAlpha (atgt, 4*PS, 2*PS, i*PS, PS, apatch, PS, PS, which);
					pflag[idx++] = CatMaskDDS (mtexf, lpatch, apatch, PS, PS);
				}
				IncProgress();
			}
			DeleteTargets (&tgt, &atgt, &ltgt);
			tgt  = CompressRGB   (img, mapw, maph, 8*PS, 4*PS, which);
			if (nlights) ltgt = CompressRGB   (limg, mapw, maph, 8*PS, 4*PS, which);
			if (mixed)   atgt = CompressAlpha (aimg, mapw, maph, 8*PS, 4*PS, which);
			for (i = 0; i < 8; i++) {
				ExtractPatchRGB (tgt, 8*PS, 2*PS, i*PS, 0, patch, PS, PS, which);
				CatDDS (texf, patch, 0, PS, PS);
				if (needmask) {
					if (nlights) ExtractPatchRGB (ltgt, 8*PS, 2*PS, i*PS, 0, lpatch, PS, PS, which);
					if (mixed)   ExtractPatchAlpha (atgt, 8*PS, 2*PS, i*PS, 0, apatch, PS, PS, which);
					pflag[idx++] = CatMaskDDS (mtexf, lpatch, apatch, PS, PS);
				}
				IncProgress();
			}
			DeleteTargets (&tgt, &atgt, &ltgt);
		}
	}

	if (minres <= 7 && maxres >= 7) { // resolution level 7: 4096x2048 (100 bitmaps)
		cout << "  7   | " << setw(4) << 16*PS << " x " << setw(4) << 8*PS << " |  100     ";
		InitProgress (100, 40);
		for (which = HEMISPHERE_NORTH; which <= HEMISPHERE_SOUTH; which++) {
			tgt  = CompressRGB   (img, mapw, maph, 6*PS, 8*PS, which);
			ltgt = CompressRGB   (limg, mapw, maph, 6*PS, 8*PS, which);
			atgt = CompressAlpha (aimg, mapw, maph, 6*PS, 8*PS, which);
			for (i = 0; i < 6; i++) {
				ExtractPatchRGB (tgt, 6*PS, 4*PS, i*PS, 3*PS, patch, PS, PS, which);
				ExtractPatchRGB (ltgt, 6*PS, 4*PS, i*PS, 3*PS, lpatch, PS, PS, which);
				ExtractPatchAlpha (atgt, 6*PS, 4*PS, i*PS, 3*PS, apatch, PS, PS, which);
				CatDDS (texf, patch, 0, PS, PS);
				if (needmask) pflag[idx++] = CatMaskDDS (mtexf, lpatch, apatch, PS, PS);
				IncProgress();
			}
			DeleteTargets (&tgt, &atgt, &ltgt);
			tgt  = CompressRGB   (img, mapw, maph, 12*PS, 8*PS, which);
			ltgt = CompressRGB   (limg, mapw, maph, 12*PS, 8*PS, which);
			atgt = CompressAlpha (aimg, mapw, maph, 12*PS, 8*PS, which);
			for (i = 0; i < 12; i++) {
				ExtractPatchRGB (tgt, 12*PS, 4*PS, i*PS, 2*PS, patch, PS, PS, which);
				ExtractPatchRGB (ltgt, 12*PS, 4*PS, i*PS, 2*PS, lpatch, PS, PS, which);
				ExtractPatchAlpha (atgt, 12*PS, 4*PS, i*PS, 2*PS, apatch, PS, PS, which);
				CatDDS (texf, patch, 0, PS, PS);
				if (needmask) pflag[idx++] = CatMaskDDS (mtexf, lpatch, apatch, PS, PS);
				IncProgress();
			}
			DeleteTargets (&tgt, &atgt, &ltgt);
			tgt  = CompressRGB   (img, mapw, maph, 16*PS, 8*PS, which);
			ltgt = CompressRGB   (limg, mapw, maph, 16*PS, 8*PS, which);
			atgt = CompressAlpha (aimg, mapw, maph, 16*PS, 8*PS, which);
			for (k = 0; k < 2; k++) {
				for (i = 0; i < 16; i++) {
					ExtractPatchRGB (tgt, 16*PS, 4*PS, i*PS, PS-k*PS, patch, PS, PS, which);
					ExtractPatchRGB (ltgt, 16*PS, 4*PS, i*PS, PS-k*PS, lpatch, PS, PS, which);
					ExtractPatchAlpha (atgt, 16*PS, 4*PS, i*PS, PS-k*PS, apatch, PS, PS, which);
					CatDDS (texf, patch, 0, PS, PS);
					if (needmask) pflag[idx++] = CatMaskDDS (mtexf, lpatch, apatch, PS, PS);
					IncProgress();
				}
			}
			DeleteTargets (&tgt, &atgt, &ltgt);
		}
	}

	if (minres <= 8 && maxres >= 8) { // resolution level 8: 8192x4096 (364 bitmaps)
		cout << "  8   | " << setw(4) << 32*PS << " x " << setw(4) << 16*PS << " |  364     ";
		InitProgress (364, 40);
		for (which = HEMISPHERE_NORTH; which <= HEMISPHERE_SOUTH; which++) {
			tgt  = CompressRGB   (img, mapw, maph, 6*PS, 16*PS, which);
			ltgt = CompressRGB   (limg, mapw, maph, 6*PS, 16*PS, which);
			atgt = CompressAlpha (aimg, mapw, maph, 6*PS, 16*PS, which);
			for (i = 0; i < 6; i++) {
				ExtractPatchRGB (tgt, 6*PS, 8*PS, i*PS, 7*PS, patch, PS, PS, which);
				ExtractPatchRGB (ltgt, 6*PS, 8*PS, i*PS, 7*PS, lpatch, PS, PS, which);
				ExtractPatchAlpha (atgt, 6*PS, 8*PS, i*PS, 7*PS, apatch, PS, PS, which);
				CatDDS (texf, patch, 0, PS, PS);
				if (needmask) pflag[idx++] = CatMaskDDS (mtexf, lpatch, apatch, PS, PS);
				IncProgress();
			}
			DeleteTargets (&tgt, &atgt, &ltgt);
			tgt  = CompressRGB   (img, mapw, maph, 12*PS, 16*PS, which);
			ltgt = CompressRGB   (limg, mapw, maph, 12*PS, 16*PS, which);
			atgt = CompressAlpha (aimg, mapw, maph, 12*PS, 16*PS, which);
			for (i = 0; i < 12; i++) {
				ExtractPatchRGB (tgt, 12*PS, 8*PS, i*PS, 6*PS, patch, PS, PS, which);
				ExtractPatchRGB (ltgt, 12*PS, 8*PS, i*PS, 6*PS, lpatch, PS, PS, which);
				ExtractPatchAlpha (atgt, 12*PS, 8*PS, i*PS, 6*PS, apatch, PS, PS, which);
				CatDDS (texf, patch, 0, PS, PS);
				if (needmask) pflag[idx++] = CatMaskDDS (mtexf, lpatch, apatch, PS, PS);
				IncProgress();
			}
			DeleteTargets (&tgt, &atgt, &ltgt);
			tgt  = CompressRGB   (img, mapw, maph, 18*PS, 16*PS, which);
			ltgt = CompressRGB   (limg, mapw, maph, 18*PS, 16*PS, which);
			atgt = CompressAlpha (aimg, mapw, maph, 18*PS, 16*PS, which);
			for (i = 0; i < 18; i++) {
				ExtractPatchRGB (tgt, 18*PS, 8*PS, i*PS, 5*PS, patch, PS, PS, which);
				ExtractPatchRGB (ltgt, 18*PS, 8*PS, i*PS, 5*PS, lpatch, PS, PS, which);
				ExtractPatchAlpha (atgt, 18*PS, 8*PS, i*PS, 5*PS, apatch, PS, PS, which);
				CatDDS (texf, patch, 0, PS, PS);
				if (needmask) pflag[idx++] = CatMaskDDS (mtexf, lpatch, apatch, PS, PS);
				IncProgress();
			}
			DeleteTargets (&tgt, &atgt, &ltgt);
			tgt  = CompressRGB   (img, mapw, maph, 24*PS, 16*PS, which);
			ltgt = CompressRGB   (limg, mapw, maph, 24*PS, 16*PS, which);
			atgt = CompressAlpha (aimg, mapw, maph, 24*PS, 16*PS, which);
			for (i = 0; i < 24; i++) {
				ExtractPatchRGB (tgt, 24*PS, 8*PS, i*PS, 4*PS, patch, PS, PS, which);
				ExtractPatchRGB (ltgt, 24*PS, 8*PS, i*PS, 4*PS, lpatch, PS, PS, which);
				ExtractPatchAlpha (atgt, 24*PS, 8*PS, i*PS, 4*PS, apatch, PS, PS, which);
				CatDDS (texf, patch, 0, PS, PS);
				if (needmask) pflag[idx++] = CatMaskDDS (mtexf, lpatch, apatch, PS, PS);
				IncProgress();
			}
			DeleteTargets (&tgt, &atgt, &ltgt);
			tgt  = CompressRGB   (img, mapw, maph, 28*PS, 16*PS, which);
			ltgt = CompressRGB   (limg, mapw, maph, 28*PS, 16*PS, which);
			atgt = CompressAlpha (aimg, mapw, maph, 28*PS, 16*PS, which);
			for (i = 0; i < 28; i++) {
				ExtractPatchRGB (tgt, 28*PS, 8*PS, i*PS, 3*PS, patch, PS, PS, which);
				ExtractPatchRGB (ltgt, 28*PS, 8*PS, i*PS, 3*PS, lpatch, PS, PS, which);
				ExtractPatchAlpha (atgt, 28*PS, 8*PS, i*PS, 3*PS, apatch, PS, PS, which);
				CatDDS (texf, patch, 0, PS, PS);
				if (needmask) pflag[idx++] = CatMaskDDS (mtexf, lpatch, apatch, PS, PS);
				IncProgress();
			}
			DeleteTargets (&tgt, &atgt, &ltgt);
			tgt  = CompressRGB   (img, mapw, maph, 30*PS, 16*PS, which);
			ltgt = CompressRGB   (limg, mapw, maph, 30*PS, 16*PS, which);
			atgt = CompressAlpha (aimg, mapw, maph, 30*PS, 16*PS, which);
			for (i = 0; i < 30; i++) {
				ExtractPatchRGB (tgt, 30*PS, 8*PS, i*PS, 2*PS, patch, PS, PS, which);
				ExtractPatchRGB (ltgt, 30*PS, 8*PS, i*PS, 2*PS, lpatch, PS, PS, which);
				ExtractPatchAlpha (atgt, 30*PS, 8*PS, i*PS, 2*PS, apatch, PS, PS, which);
				CatDDS (texf, patch, 0, PS, PS);
				if (needmask) pflag[idx++] = CatMaskDDS (mtexf, lpatch, apatch, PS, PS);
				IncProgress();
			}
			DeleteTargets (&tgt, &atgt, &ltgt);
			tgt  = CompressRGB   (img, mapw, maph, 32*PS, 16*PS, which);
			ltgt = CompressRGB   (limg, mapw, maph, 32*PS, 16*PS, which);
			atgt = CompressAlpha (aimg, mapw, maph, 32*PS, 16*PS, which);
			for (k = 0; k < 2; k++) {
				for (i = 0; i < 32; i++) {
					ExtractPatchRGB (tgt, 32*PS, 8*PS, i*PS, PS-k*PS, patch, PS, PS, which);
					ExtractPatchRGB (ltgt, 32*PS, 8*PS, i*PS, PS-k*PS, lpatch, PS, PS, which);
					ExtractPatchAlpha (atgt, 32*PS, 8*PS, i*PS, PS-k*PS, apatch, PS, PS, which);
					CatDDS (texf, patch, 0, PS, PS);
					if (needmask) pflag[idx++] = CatMaskDDS (mtexf, lpatch, apatch, PS, PS);
					IncProgress();
				}
			}
			DeleteTargets (&tgt, &atgt, &ltgt);
		}
	}

	// write table of contents
	if (needmask) {
		LMASKFILEHEADER lmfh = {
			{'P','L','T','A','0','1','0','0'},
			sizeof (LMASKFILEHEADER),
			(mixed ? 3:patchflag) | (nlights ? 4:0),
			idx,
			(BYTE)minres,
			(BYTE)maxres
		};
		strcpy (cbuf, fname); strcat (cbuf, "_lmask.bin");
		FILE *binf = fopen (cbuf, "wb");
		fwrite (&lmfh, sizeof (LMASKFILEHEADER), 1, binf);
		fwrite (pflag, sizeof(WORD), idx, binf);
		fclose (binf);
	}

	// cleanup
	if (texf) fclose (texf);
	if (mtexf) fclose (mtexf);

	if (img)    delete []img;
	if (limg)   delete []limg;
	if (aimg)   delete []aimg;
	if (patch)  delete []patch;
	if (lpatch) delete []lpatch;
	if (apatch) delete []apatch;

	if (g_nsuppressed)
		cout << "\nMarginal transparency/opacity suppressed in " << g_nsuppressed << " tiles" << endl;

	cout << endl << "Surface texture map written to " << fname << ".tex" << endl;
	if (mixed) {
		cout << "Land-water mask texture map written to " << fname << "_lmask.tex" << endl;
		cout << "Surface descriptor file written to " << fname << "_lmask.bin" << endl;
	}
	cout << "\nTo use the new surface in Orbiter:\n";
	cout << "Rename all output files by replacing '" << fname << "' with the\n";
	cout << "planet name, and move them to the Orbiter\\Textures2 folder" << endl;
}

void CreateLocalArea ()
{
	const double eps = 1e-8;
	int i, j, k, which, maxlvl;
	int band, bd, idx = 0, sidx = 0, midx = 0;
	int ntot = 0, sout = 0, mout = 0;
	LONG mapw, maph;
	WORD bpp, abpp, nbpp;
	DWORD patchflag;
	RGB *img, *tgt, *patch = 0;
	RGB *limg = 0, *lpatch = 0;
	Alpha *aimg = 0, *atgt, *apatch = 0;
	char c;
	char *cbuf = g_cwd+strlen(g_cwd);
	FILE *texf = 0, *mtexf = 0;
	bool mixed = false, skipwater = false, nlights = false, needmask = false, mipmap = false, global;
	double transp, landlimit, mixed_tol, light_tol;
	double lngmin, lngmax, latmin, latmax , lng0, lng1, lat0, lat1;
	int ntfs = 364*5;
	TILEFILESPEC *tfs = new TILEFILESPEC[ntfs];
	memset (tfs, 0, ntfs*sizeof(TILEFILESPEC));
	int ntile = 364;
	for (i = 0; i < ntile; i++) tfs[i].sidx = tfs[i].midx = (DWORD)-1;

	cout << "        CREATE LOCAL HIGH-RESOLUTION PLANETARY SURFACE PATCH\n";
	cout << "        ----------------------------------------------------\n\n";

	if (!fname[0]) {
		cout << "Enter the file name for the bitmap representing the planetary surface\n";
		cout << "area (must be in 8-bit or 24-bit BMP format). The bitmap must contain a\n";
		cout << "surface patch in cylindrical projection, with longitude linear along the\n";
		cout << "horizontal axis, and latitude linear along the vertical axis.\n\n";
		cout << ">> Surface map file name (.bmp): ";
		cin >> fname;
		if (!_stricmp (fname+(strlen(fname)-4), ".bmp"))
			fname[strlen(fname)-4] = '\0';
		cout << endl;
	}

	cout << "\nWhat latitude and longitude range does the bitmap patch cover?\n";
	cout << "For global coverage, the limits should be longitude = -180 ... 180 and\n";
	cout << "latitude = -90 ... 90, but local areas with smaller coverage can also be\n";
	cout << "used.\n\n";
	cout << ">> longitude range (deg) [min max]: ";
	cin  >> lngmin >> lngmax;
	cout << ">> latitude range (deg) [min max]: ";
	cin  >> latmin >> latmax;
	if (lngmin < -180 || lngmax > 180 || lngmin >= lngmax)
		FatalError ("Invalid longitude range");
	if (latmin < -90 || latmax > 90 || latmin >= latmax)
		FatalError ("Invalid latitude range");
	global = (lngmin == -180 && lngmax == 180 && latmin == -90 && latmax == 90);

	cout << "\nDoes the surface patch contain any areas that should show specular light\n";
	cout << "reflection (e.g. oceans, rivers or icecaps)?\n\n";
	cout << "(D) Only diffusely reflecting surface\n";
	cout << "(S) Only specularly reflecting surface\n";
	cout << "(B) Both diffuse and specular surfaces\n";
	cout << "\n>> [D|S|B]: ";
	cin >> c;
	cout << endl;

	switch (toupper(c)) {
	case 'D':
		patchflag = 1;
		break;
	case 'S':
		patchflag = 2;
		break;
	case 'B':
		patchflag = 0; // needs to be determined individually
		mixed = true;
		break;
	default:
		FatalError ("Invalid selection");
		break;
	}

	if (mixed) {
		cout << "\nTo prepare a mixed-reflection surface, a bitmap containing a land-water\n";
		cout << "mask is required. This bitmap must be of the same size and cover the same\n";
		cout << "area as the surface bitmap. It must be in 8-bit or 24-bit BMP format, and\n";
		cout << "contain only 2 colours: white for any specular reflection areas (water),\n";
		cout << "and black for diffuse reflection areas (land).\n\n";
		cout << ">> Mask map file name (.bmp): ";
		cin >> aname;
		if (!_stricmp (aname+(strlen(aname)-4), ".bmp"))
			aname[strlen(aname)-4] = '\0';
		cout << endl;

		cout << "\nTiles which contain a mixture of land and water surfaces require larger\n";
		cout << "texture files and can degrade rendering performance. Therefore, mixed\n";
		cout << "tiles which contain only a small proportion of land or water pixels can be\n";
		cout << "rendered entirely as water or land tiles, respectively, to improve\n";
		cout << "performance. Up to what percentage of land or water area should a tile be\n";
		cout << "considered uniformly as water or land surface? A higher percentage will\n";
		cout << "decrease texture file sizes and improve performance, but can lead to errors\n";
		cout << "in specular reflection. A value of 0 will force dual rendering even if only\n";
		cout << "a single pixel is marked as land or water surface. [Recommended value: 0.2]\n";
		cout << "\n>> Mixed cover limit for uniform rendering (%) [0-100]: ";
		cin >> mixed_tol;
		cout << endl;
		mixed_tol *= 0.01;
	}

	if (patchflag != 1) {
		cout << "\nWater surfaces usually contain less detail and can be rendered at a lower\n";
		cout << "resolution than land surfaces. To decrease the texture file sizes and\n";
		cout << "improve Orbiter performance, tiles that contain only water surfaces can be\n";
		cout << "excluded from the high-resolution tiles, forcing Orbiter to render them at\n";
		cout << "resolution level 8. [Recommended value: Y]\n";
		cout << "\n>> Exclude water-only tiles (yes|no)? [Y|N]: ";
		cin >> c;
		cout << endl;
		if (toupper(c) == 'Y') {
			if (patchflag == 2) FatalError ("Nothing to do!");
			skipwater = true;
			cout << "\nUp to which percentage of land cover should a tile be excluded?\n";
			cout << "A higher percentage will cause more tiles to be excluded from high-\n";
			cout << "resolution rendering, even if they contain some land area. A percentage\n";
			cout << "of 0 will exclude only tiles that don't contain any land area at all.\n";
			cout << "[Recommended value: 1.0]\n";
			cout << "\n>> Land cover limit for exclusion (%) [0-100]: ";
			cin >> landlimit;
			cout << endl;
			landlimit *= 0.01;
		}
	}

	cout << "\nCity lights: Does the surface contain areas that emit light at night\n";
	cout << "(illuminated cities etc.)?\n\n";
	cout << "(N) No\n";
	cout << "(Y) Yes\n";
	cout << "\n>> [N|Y]: ";
	cin >> c;
	cout << endl;
	switch (toupper(c)) {
	case 'N':
		nlights = false;
		break;
	case 'Y':
		nlights = true;
		break;
	default:
		FatalError ("Invalid selection");
		break;
	}

	if (nlights) {
		cout << "\nTo prepare city-light textures, a bitmap containing the distribution of\n";
		cout << "emissive light during nighttime is required. This bitmap must be of the\n";
		cout << "same size and cover the same area as the surface bitmap. It must be in\n";
		cout << "8-bit or 24-bit BMP format. It should be black in non-lit areas, and bright\n";
		cout << "(but not necessarily white) in lit areas.\n\n";
		cout << ">> City light map file name: (.bmp): ";
		cin >> lname;
		if (!_stricmp (lname+(strlen(lname)-4), ".bmp"))
			lname[strlen(lname)-4] = '\0';
		cout << endl;

		cout << "\nTiles which contain only a very small amount of city lights can be\n";
		cout << "rendered more efficiently without a light texture.\n";
		cout << "Up to what percentage of city light coverage should a tile be rendered\n";
		cout << "without lights? A higher percentage will decrease texture file size and\n";
		cout << "improve performance, but can lead to the loss of night lights in sparsely\n";
		cout << "lit areas. A value of 0 will force the inclusion of light textures even\n";
		cout << "if only a single pixel is illuminated. [Recommended value: 0.1]\n";
		cout << "\n>> Cutoff limit for city lights (%) [0-100]: ";
		cin >> light_tol;
		light_tol *= 0.01;
	}
	needmask = mixed || nlights;

	ReadBMP_data (fname, mapw, maph, bpp);
	patch = new RGB[PS*PS];
	texf = fopen ("tmp_tile.tex", "wb");

	// figure out max. resolution supported by bitmap
	const double scl9 = 4096.0/180.0;
	double scl = (double)maph/(latmax-latmin);
	maxlvl = 8;
	while (scl >= 2.0*scl9-eps) {
		maxlvl++;
		scl *= 0.5;
	}
	maxlvl = min (MAXLEVEL, maxlvl); // max. currently supported level
	if (maxlvl < 9) FatalError ("Bitmap resolution insufficient for level 9");
	if (maxlvl > 9) {
		cout << endl << "The bitmaps support resolutions up to level " << maxlvl << ".\n";
		cout << "Please specify up to which level you want to generate textures.\n";
		cout << "\n>> Resolution level [9-" << maxlvl << "]: ";
		cin >> i;
		if (i < 9 || i > maxlvl) FatalError ("Invalid resolution level");
		maxlvl = i;
	}

	cout << endl << "Create mipmaps for surface tiles? Mipmaps can improve the visual\n";
	cout << "appearance of planet surfaces, but increase the size of the texture file.\n";
	cout << "\n>> [Y|N]: ";
	cin  >> c;
	cout << endl;
	switch (toupper(c)) {
	case 'N':
		mipmap = false;
		break;
	case 'Y':
		mipmap = true;
		break;
	default:
		FatalError ("Invalid selection");
		break;
	}

	if (mixed) {
		LONG amapw, amaph;
		ReadBMP_data (aname, amapw, amaph, abpp);
		if (amapw != mapw || amaph != maph)
			FatalError ("Incompatible bitmap sizes");
	}

	if (nlights) {
		LONG nmapw, nmaph;
		ReadBMP_data (lname, nmapw, nmaph, nbpp);
		if (nmapw != mapw || nmaph != maph)
			FatalError ("Incompatible bitmap sizes");
	}

	if (needmask) {
		lpatch = new RGB[PS*PS];
		memset (lpatch, 0, sizeof(RGB)*PS*PS);
		apatch = new Alpha[PS*PS];
		memset (apatch, 0, sizeof(Alpha)*PS*PS);
		mtexf = fopen ("tmp_tile_lmask.tex", "wb");
	}

	cout << "-------------------------------------------------\n";
	cout << "Surface bitmap file: " << fname << ".bmp\n";
	if (mixed)
		cout << "Land-water mask file: " << aname << ".bmp\n";
	if (nlights)
		cout << "City lights file: " << lname << ".bmp\n";
	cout << "Bitmap dimensions: " << mapw << "(W) x " << maph << "(H) x " << bpp << "(BPP)\n";
	cout << "Using patch size: " << PS << " x " << PS << endl;
	cout << "Coverage: ";
	if (global) cout << "global" << endl;
	else cout << lngmin << " < lng < " << lngmax << ", " << latmin << " < lat < " << latmax << endl;
	cout << "Reflection mode: ";
	switch (patchflag) {
	case 0: cout << "diffuse+specular\n"; break;
	case 1: cout << "diffuse only\n"; break;
	case 2: cout << "specular only\n"; break;
	}
	if (skipwater) cout << "Excluding water-only tiles (up to " << landlimit*100.0 << "% land cover)\n";
	cout << endl;

	cout << "Processing ..." << endl;
	cout << "Level 9+\t" << flush;
	InitProgress (364, 40);

	if (0 /*global*/) { // global coverage

		for (which = HEMISPHERE_NORTH; which <= HEMISPHERE_SOUTH; which++) {
			for (band = 0; band < 8; band++) {
				bd = (which == HEMISPHERE_NORTH ? 15-band : band);
				img = ReadBMP_band (fname, mapw, maph, bpp, bd*2*PS, 2*PS);
				tgt = CompressRGB (img, mapw, 2*PS, np[band]*2*PS, 2*PS);
				if (mixed) {
					aimg = ReadBMPAlpha_band (aname, mapw, maph, abpp, bd*2*PS, 2*PS);
					atgt = CompressAlpha (aimg, mapw, 2*PS, np[band]*2*PS, 2*PS);
				}
				for (i = 0; i < np[band]; i++) {
					for (k = 0; k < 2; k++) {
						for (j = 0; j < 2; j++) {
							ntot++;
							ExtractPatchRGBA (tgt, atgt, np[band]*2*PS, 2*PS, (i*2+j)*PS, (1-k)*PS, patch, apatch, PS, PS, which);
							tfs[idx].subidx[k*2+j] = ntile;
							if (mixed) {
								transp = (double)TransparentCount (apatch, PS, PS)/(double)(PS*PS);
								if (skipwater && (transp < landlimit)) {
									tfs[ntile].sidx = (DWORD)-1;
									tfs[ntile].midx = (DWORD)-1;
									tfs[ntile].flags = 0;
									continue;
								} else {
									if (transp < mixed_tol) patchflag = 2;
									else if (1.0-transp < mixed_tol) patchflag = 1;
									else patchflag = 3;
								}
							}
							CatDDS (texf, patch, 0, PS, PS, true, mipmap);
							tfs[ntile].sidx = sidx++;
							sout++;
							if (patchflag == 3) {
								CatDDS (mtexf, lpatch, apatch, PS, PS, true);
								tfs[ntile].midx = midx++;
								mout++;
							}
							tfs[ntile].flags = patchflag;
							ntile++;
						}
					}
					idx++;
					IncProgress ();
				}
				DeleteTargets (&tgt, &atgt);
				delete []img;
				if (aimg) delete []aimg;
			}
		}

	} else { // local coverage

		img = ReadBMP (fname, mapw, maph, bpp);
		if (nlights) limg = ReadBMP (lname, mapw, maph, nbpp);
		if (mixed) aimg = ReadBMPAlpha (aname, mapw, maph, abpp);
		PATCHDATA pd = {0, img, patch, limg, lpatch, aimg, apatch, mapw, maph, maxlvl, mixed, skipwater, nlights, mipmap, landlimit, mixed_tol, light_tol,
			latmin, latmax, lngmin, lngmax, ntile, ntot, sout, mout, sidx, midx, patchflag, tfs, ntfs, texf, mtexf};

		for (which = HEMISPHERE_NORTH; which <= HEMISPHERE_SOUTH; which++) {
			pd.which = which;
			for (band = 0; band < nband; band++) { // level-8 latitude bands
				for (i = 0; i < np[band]; i++) {   // level-8 longitude tiles
					lat0 = (double)(nband-1-band)/(double)nband * 90.0;
					lat1 = lat0 + 90.0/(double)nband;
					lng0 = (double)i/(double)np[band] * 360.0 - 180.0;
					lng1 = lng0 + 360.0/(double)np[band];
					CreateSubPatch (lat0, lat1, lng0, lng1, idx, pd, 9);
					idx++;
					IncProgress();
				}
			}
		}

		delete []img;
		if (limg) delete []limg;
		if (aimg) delete []aimg;
		ntile = pd.ntile; sout = pd.sout; mout = pd.mout; tfs = pd.tfs;
	}

	FILE *binf = fopen ("tmp_tile.bin", "wb");
	fwrite (TileID, 1, 8, binf);             // header: file format id + version
	fwrite (&ntile, sizeof(DWORD), 1, binf); // header: number of tile descriptors
	fwrite (tfs, sizeof(TILEFILESPEC), ntile, binf);
	fclose (binf);
	if (texf) fclose (texf);
	if (mtexf) fclose (mtexf);
	delete []tfs;
	if (patch) delete []patch;
	if (apatch) delete []apatch;
	if (lpatch) delete []lpatch;

	SortTextures ("tmp");
	remove ("tmp_tile.bin");
	strcpy (cbuf, fname); strcat (cbuf, "_tile.bin");
	remove (cbuf);
	rename ("tmp_tile.bin.sort", cbuf);
	remove ("tmp_tile.tex");
	strcpy (cbuf, fname); strcat (cbuf, "_tile.tex");
	remove (cbuf);
	rename ("tmp_tile.tex.sort", cbuf);

	if (mixed) {
		remove ("tmp_tile_lmask.tex");
		strcpy (cbuf, fname); strcat (cbuf, "_tile_lmask.tex");
		remove (cbuf);
		rename ("tmp_tile_lmask.tex.sort", cbuf);
	}

	cout << endl << "Wrote " << ntile << " tile descriptors for level 8 and higher." << endl;
	cout << "Wrote " << sout << " surface textures and " << mout << " mask textures." << endl << endl;

	cout << "To use the new surface in Orbiter:\n";
	cout << "* rename " << fname << "_tile.bin to <planet>_tile.bin\n";
	cout << "* rename " << fname << "_tile.tex to <planet>_tile.tex\n";
	if (mixed)
		cout << "* rename " << fname << "_tile_lmask.tex to <planet>_tile_lmask.tex\n";
	cout << "(where <planet> is the name of the planet), and move both files to\n";
	cout << "the Orbiter\\Textures2 folder" << endl;
	
}

bool CreateSubPatch (double baselat0, double baselat1, double baselng0, double baselng1, int idx, PATCHDATA &pd, int lvl)
{
	int k, j;
	DWORD patchflag = pd.patchflag;
	DWORD tsize;
	double lat0, lat1, lng0, lng1, nlat0, nlat1, nlng0, nlng1, transp, lights;
	const double eps = 1e-8;
	bool bIsTile = false;

	for (k = 0; k < 2; k++) {      // patch latitude subdivision
		for (j = 0; j < 2; j++) {  // patch longitude subdivision
			bool bSubtile = false;
			pd.ntot++;
			nlat0 = lat0 = baselat0 + (baselat1-baselat0)*0.5*(1-k);
			nlng0 = lng0 = baselng0 + (baselng1-baselng0)*0.5*j;
			nlat1 = lat1 = lat0 + (baselat1-baselat0)*0.5;
			nlng1 = lng1 = lng0 + (baselng1-baselng0)*0.5;
			if (pd.which == HEMISPHERE_SOUTH) {
				lat0 = -lat1;
				lng0 = -lng1;
				lat1 = lat0 + (baselat1-baselat0)*0.5;
				lng1 = lng0 + (baselng1-baselng0)*0.5;
			}
			pd.tfs[idx].subidx[k*2+j] = 0; // no subtile by default

			// check for partial coverage of patch within bitmap support
			if (lat0 <= pd.latmax-eps && lat1 >= pd.latmin+eps && lng0 <= pd.lngmax-eps && lng1 >= pd.lngmin+eps) {

				if (pd.ntile >= pd.ntfs) { // need to reallocate list of contents
					TILEFILESPEC *tmp = new TILEFILESPEC[pd.ntfs+256];
					memset (tmp, 0, (pd.ntfs+256)*sizeof(TILEFILESPEC));
					memcpy (tmp, pd.tfs, pd.ntfs*sizeof(TILEFILESPEC));
					delete []pd.tfs;
					pd.tfs = tmp;
					pd.ntfs += 256;
				}
				pd.tfs[pd.ntile].flags = 0;
				pd.tfs[pd.ntile].sidx = (DWORD)-1;
				pd.tfs[pd.ntile].midx = (DWORD)-1;
				pd.tfs[pd.ntile].eidx = (DWORD)-1;
				pd.tfs[idx].subidx[k*2+j] = pd.ntile;

				// check for full coverage of patch within bitmap support
				if (pd.latmin-eps <= lat0 && pd.latmax+eps >= lat1 && pd.lngmin-eps <= lng0 && pd.lngmax+eps >= lng1) {

					bSubtile = true; // subtile is supported by bitmap coverage
					SamplePatch (pd.img, pd.mapw, pd.maph, pd.lngmin, pd.lngmax, pd.latmin, pd.latmax, pd.patch, PS, PS, lng0, lng1, lat0, lat1, pd.which);
					if (pd.mixed) {
						SampleAPatch (pd.aimg, pd.mapw, pd.maph, pd.lngmin, pd.lngmax, pd.latmin, pd.latmax, pd.apatch, PS, PS, lng0, lng1, lat0, lat1, pd.which);
						transp = (double)TransparentCount (pd.apatch, PS, PS)/(double)(PS*PS);
						if (pd.skipwater && (transp < pd.landlimit)) {
							pd.tfs[idx].subidx[k*2+j] = 0;
							continue;
						} else {
							if (transp < pd.mixed_tol) patchflag = 2;
							else if (1.0-transp < pd.mixed_tol) patchflag = 1;
							else patchflag = 3;
						}
					}
					if (pd.nlights) {
						SamplePatch (pd.limg, pd.mapw, pd.maph, pd.lngmin, pd.lngmax, pd.latmin, pd.latmax, pd.lpatch, PS, PS, lng0, lng1, lat0, lat1, pd.which);
						lights = (double)BrightCount (pd.lpatch, PS, PS)/(double)(PS*PS);
						if (lights >= pd.light_tol) patchflag |= 4;
					}
					tsize = CatDDS (pd.texf, pd.patch, 0, PS, PS, true, pd.mipmap);
					pd.tfs[pd.ntile].sidx = pd.sidx;
					pd.sidx += tsize;
					//pd.tfs[pd.ntile].sidx = pd.sidx++;
					if (((patchflag & 3) == 3) || (patchflag & 4)) {
						ErodeLights (pd.lpatch, pd.apatch, PS, PS);
						tsize = CatDDS (pd.mtexf, pd.lpatch, pd.apatch, PS, PS, true);
						pd.tfs[pd.ntile].midx = pd.midx;
						pd.midx += tsize;
						//pd.tfs[pd.ntile].midx = pd.midx++;
						pd.mout++;
					} else {
						pd.tfs[pd.ntile].midx = (DWORD)-1;
					}
					pd.tfs[pd.ntile].flags = patchflag;
					pd.sout++;

				}
				pd.ntile++;

				// now recursively go down to higher resolutions
				if (lvl < pd.maxlevel) {
					bool bsub = CreateSubPatch (nlat0, nlat1, nlng0, nlng1, pd.ntile-1, pd, lvl+1);
					bSubtile = bSubtile || bsub;
				}

				if (!bSubtile) { // remove tile description
					pd.tfs[idx].subidx[k*2+j] = 0;
					pd.ntile--;
				}

				bIsTile = bIsTile || bSubtile;
			}
		}
	}
	return bIsTile;
}

void SortTextures (const char *rootname)
{
	char cbuf[256], rtname[256];
	char idstr[9] = "        ";
	DWORD i, ntd, texofs, maskofs, level;
	FILE *binf, *texf, *maskf, *binfm, *texfm, *maskfm;
	TILEFILESPEC *td;

	if (!rootname) {
		cout << "                         SORT TEXTURE FILES\n";
		cout << "                         ------------------\n\n";
		cout << "Enter the base name for the set of texture files.\n";
		cout << ">> Base name: ";
		cin >> rtname;
		rootname = rtname;
	}

	cout << "\nSorting texture files ..." << endl;

	strcpy (cbuf, rootname); strcat (cbuf, "_tile.bin");
	if (!(binf = fopen (cbuf, "rb"))) FatalError ("File not found");
	strcpy (cbuf, rootname); strcat (cbuf, "_tile.tex");
	if (!(texf = fopen (cbuf, "rb"))) FatalError ("File not found");
	strcpy (cbuf, rootname); strcat (cbuf, "_tile_lmask.tex");
	maskf = fopen (cbuf, "rb");
	strcpy (cbuf, rootname); strcat (cbuf, "_tile.bin.sort");
	if (!(binfm = fopen (cbuf, "wb"))) FatalError ("Could not open output file");
	strcpy (cbuf, rootname); strcat (cbuf, "_tile.tex.sort");
	if (!(texfm = fopen (cbuf, "wb"))) FatalError ("Could not open output file");
	if (maskf) {
		strcpy (cbuf, rootname); strcat (cbuf, "_tile_lmask.tex.sort");
		if (!(maskfm = fopen (cbuf, "wb"))) FatalError ("Could not open output file");
	} else maskfm = 0;

	fread (idstr, 1, 8, binf);  // read file format id + version number
	if (strncmp (idstr, TileID, 4)) fseek (binf, 0, SEEK_SET); // no header: old version

	fread (&ntd, sizeof(DWORD), 1, binf);
	td = new TILEFILESPEC[ntd]; fread (td, sizeof(TILEFILESPEC), ntd, binf);

	texofs = maskofs = 0;
	for (level = 9; level <= MAXLEVEL; level++) {
		cout << "Level " << level << " \t";
		InitProgress (364, 40);
		for (i = 0; i < 364; i++) {
			CopyTexturesAtLevel (td, i, 9, level, texofs, maskofs, texf, maskf, texfm, maskfm);
			IncProgress();
		}
	}

	// write out modified table of contents
	fwrite (TileID, 1, 8, binfm);
	fwrite (&ntd, sizeof(DWORD), 1, binfm);
	fwrite (td, sizeof (TILEFILESPEC), ntd, binfm);

	delete []td;
	fclose (binf);   fclose (binfm);
	fclose (texf);   fclose (texfm);
	if (maskf) fclose (maskf);
	if (maskfm) fclose (maskfm);
}

void CopyTexturesAtLevel (TILEFILESPEC *td, DWORD baseidx, DWORD lvl, DWORD tgtlvl, DWORD &texofs, DWORD &maskofs,
	FILE *srctexf, FILE *srcmaskf, FILE *tgttexf, FILE *tgtmaskf)
{
	DWORD i, subidx, tsize;

	for (i = 0; i < 4; i++) {
		if (subidx = td[baseidx].subidx[i]) {
			if (lvl < tgtlvl) {
				CopyTexturesAtLevel (td, subidx, lvl+1, tgtlvl, texofs, maskofs, srctexf, srcmaskf, tgttexf, tgtmaskf);
			} else {
				TILEFILESPEC &tds = td[subidx];
				if (tds.sidx != (DWORD)-1) {
					tsize = CopyDDS (tgttexf, srctexf, tds.sidx, true);
					tds.sidx = texofs;
					texofs += tsize;
					//tds.sidx = texidx++;
				}
				if (srcmaskf && tgtmaskf && tds.midx != (DWORD)-1) {
					tsize = CopyDDS (tgtmaskf, srcmaskf, tds.midx, true);
					tds.midx = maskofs;
					maskofs += tsize;
					//tds.midx = maskidx++;
				}
			}
		}
	}
}

void MergeTextures ()
{
	char fname2[256], cbuf[256], errstr[256] = "File not found: ";
	char idstr[9] = "        ";
	FILE *binf1, *binf2, *texf1, *texf2, *texfm, *maskf1, *maskf2, *maskfm = NULL;
	DWORD i, ntd1, ntd2, ntdm;
	TILEFILESPEC *td1, *td2, *tdm;

	cout << "                MERGE HIGH-RESOLUTION TEXTURE FILES\n";
	cout << "                -----------------------------------\n\n";

	cout << "Merge a local high resolution surface texture into a planetary texture\n";
	cout << "definition.\n\n\n";

	cout << "Enter the base name for the first set of texture files. This is the\n";
	cout << "set that will be updated. Example: \"Textures2\\Earth\".\n";
	cout << "The following files must exist:\n";
	cout << "  <basename1>_tile.bin       (tile descriptor file)\n";
	cout << "  <basename1>_tile.tex       (surface texture file)\n";
	cout << "  <basename1>_tile_lmask.tex (land-water mask file; optional)\n\n";
	cout << ">> Base name 1: ";
	cin >> fname;

	cout << "\n\nEnter the base name for the second set of texture files. This is the\n";
	cout << "set that will be merged into the first set.\n";
	cout << "The following files must exist:\n";
	cout << "  <basename2>_tile.bin       (tile descriptor file)\n";
	cout << "  <basename2>_tile.tex       (surface texture file)\n";
	cout << "  <basename2>_tile_lmask.tex (land-water mask file; optional)\n\n";
	cout << ">> Base name 2: ";
	cin >> fname2;

	strcpy (cbuf, fname); strcat (cbuf, "_tile.bin");
	if (!(binf1 = fopen (cbuf, "rb"))) {
		strcat (errstr, cbuf);
		FatalError (errstr);
	}
	strcpy (cbuf, fname); strcat (cbuf, "_tile.tex");
	if (!(texf1 = fopen (cbuf, "rb"))) {
		strcat (errstr, cbuf);
		FatalError (errstr);
	}
	strcpy (cbuf, fname); strcat (cbuf, "_tile_lmask.tex");
	maskf1 = fopen (cbuf, "rb");
	strcpy (cbuf, fname2); strcat (cbuf, "_tile.bin");
	if (!(binf2 = fopen (cbuf, "rb"))) {
		strcat (errstr, cbuf);
		FatalError (errstr);
	}
	strcpy (cbuf, fname2); strcat (cbuf, "_tile.tex");
	if (!(texf2 = fopen (cbuf, "rb"))) {
		strcat (errstr, cbuf);
		FatalError (errstr);
	}
	strcpy (cbuf, fname2); strcat (cbuf, "_tile_lmask.tex");
	maskf2 = fopen (cbuf, "rb");

	if (!(texfm = fopen ("merge_tile.tex", "wb"))) FatalError ("Could not open output file");
	if (maskf1 || maskf2)
		if (!(maskfm = fopen ("merge_tile_lmask.tex", "wb"))) FatalError ("Could not open output file");

	fread (idstr, 1, 8, binf1);
	if (strncmp (idstr, TileID, 4)) fseek (binf1, 0, SEEK_SET); // old format
	fread (idstr, 1, 8, binf2);
	if (strncmp (idstr, TileID, 4)) fseek (binf2, 0, SEEK_SET); // old format
	fread (&ntd1, sizeof(DWORD), 1, binf1);
	fread (&ntd2, sizeof(DWORD), 1, binf2);
	td1 = new TILEFILESPEC[ntd1]; fread (td1, sizeof(TILEFILESPEC), ntd1, binf1);
	td2 = new TILEFILESPEC[ntd2]; fread (td2, sizeof(TILEFILESPEC), ntd2, binf2);
	tdm = new TILEFILESPEC[ntd1+ntd2]; // max size of merged descriptors
	ntdm = 364;

	cout << "\n-------------------------------------------------\n\n";
	cout << "Tile set 1 (" << fname  << ") contains " << ntd1 << " tile descriptors\n";
	cout << "Tile set 2 (" << fname2 << ") contains " << ntd2 << " tile descriptors\n";
	cout << endl << "Merging  \t";
	InitProgress (364, 40);

	MERGEDATA md = {td1, td2, tdm, ntdm, 0, 0, texf1, texf2, texfm, maskf1, maskf2, maskfm};

	for (i = 0; i < 364; i++) { // loop over level 8 patches
		MergeTrees (md, i, i, i, true);
		IncProgress ();
	}

	FILE *binfm = fopen ("merge_tile.bin", "wb");
	fwrite (TileID, 1, 8, binfm);
	fwrite (&md.ntile, sizeof(DWORD), 1, binfm); // header: number of tile descriptors
	fwrite (tdm, sizeof(TILEFILESPEC), md.ntile, binfm);
	fclose (binfm);
	fclose (binf1);
	fclose (binf2);
	fclose (texf1);
	fclose (texf2);
	fclose (texfm);
	delete []td1;
	delete []td2;
	if (maskf1) fclose (maskf1);
	if (maskf2) fclose (maskf2);
	if (maskfm) fclose (maskfm);

	SortTextures ("merge");
	remove ("merge_tile.bin");
	rename ("merge_tile.bin.sort", "merge_tile.bin");
	remove ("merge_tile.tex");
	rename ("merge_tile.tex.sort", "merge_tile.tex");
	if (maskfm) {
		remove ("merge_tile_lmask.tex");
		rename ("merge_tile_lmask.tex.sort", "merge_tile_lmask.tex");
	}

	cout << endl << "Wrote " << md.ntile << " tile descriptors for merged texture set." << endl;
	cout << endl << "Wrote " << md.ns << " surface tiles and " << md.nm << " mask tiles." << endl;
	cout << "Created files merge_tile.bin and merge_tile.tex." << endl << endl;

	cout << "To use the merged surface in Orbiter:\n";
	cout << "* rename merge_tile.bin to <planet>_tile.bin\n";
	cout << "* rename merge_tile.tex to <planet>_tile.tex\n";
	cout << "(where <planet> is the name of the planet), and overwrite the original\n";
	cout << "files in the Orbiter\\Textures2 folder." << endl;

}

void MergeTrees (MERGEDATA &md, DWORD idx1, DWORD idx2, DWORD idxm, bool baselvl)
{
	DWORD tsize;
	TILEFILESPEC *t1 = (baselvl || idx1 ? md.td1 + idx1 : 0);
	TILEFILESPEC *t2 = (baselvl || idx2 ? md.td2 + idx2 : 0);
	if (!t1 && !t2) return;
	TILEFILESPEC *tm = md.tdm + idxm;
	tm->sidx = (DWORD)-1;
	tm->midx = (DWORD)-1;
	tm->eidx = (DWORD)-1;

	// merge surface textures
	if (t2 && t2->sidx != (DWORD)-1) {
		if (!(tsize = CopyDDS (md.texfm, md.texf2, t2->sidx, true)))
			FatalError ("Texture file parse error");
		tm->sidx = md.ns;
		md.ns += tsize;
		//tm->sidx = md.ns++;
		if (md.maskf2 && t2->midx != (DWORD)-1) {
			if (!(tsize = CopyDDS (md.maskfm, md.maskf2, t2->midx, true)))
				FatalError ("Mask file parse error");
			tm->midx = md.nm;
			md.nm += tsize;
			//tm->midx = md.nm++;
		}
		tm->flags = t2->flags;
	} else if (t1 && t1->sidx != (DWORD)-1) {
		if (!(tsize = CopyDDS (md.texfm, md.texf1, t1->sidx, true)))
			FatalError ("Texture file parse error");
		tm->sidx = md.ns;
		md.ns += tsize;
		//tm->sidx = md.ns++;
		if (md.maskf1 && t1->midx != (DWORD)-1) {
			if (!(tsize = CopyDDS (md.maskfm, md.maskf1, t1->midx, true)))
				FatalError ("Mask file parse error");
			tm->midx = md.nm;
			md.nm += tsize;
			//tm->midx = md.nm++;
		}
		tm->flags = t1->flags;
	}

	if (idxm == md.ntile) md.ntile++;

	for (int j = 0; j < 4; j++) {
		DWORD sub1 = (t1 ? t1->subidx[j] : 0);
		DWORD sub2 = (t2 ? t2->subidx[j] : 0);
		if (sub1 || sub2) {
			tm->subidx[j] = md.ntile;
			MergeTrees (md, sub1, sub2, md.ntile, false);
		} else {
			tm->subidx[j] = 0;
		}
	}
}

void CreateCloudMap ()
{
	int minres = g_minres, maxres = g_maxres;
	int maxlevel = 8;
	int i, k, lmax, which;
	bool transp;
	char c;
	char *cbuf = g_cwd+strlen(g_cwd);
	DWORD patchflag;
	WORD bpp;
	LONG mapw, maph;
	RGB *img = 0, *patch = 0, *tgt = 0;
	Alpha *aimg = 0, *apatch = 0, *atgt = 0;
	FILE *texf = 0;
	binary_alpha = false;
	g_tol = 0;

	cout << "                      CREATE GLOBAL CLOUD MAP\n";
	cout << "                      -----------------------\n\n";

	cout << "Enter the file name for the bitmap defining the colour map for the global\n";
	cout << "cloud layer (must be in 8-bit or 24-bit BMP format). The bitmap must\n";
	cout << "contain the global cloud distribution over 360 deg longitude and 180 deg\n";
	cout << "latitude. The width:height ratio of the bitmap should be approximately\n";
	cout << "2:1 for best results.\n\n";
	cout << ">> Cloud colour map file name (.bmp): ";
	cin >> fname;
	if (!_stricmp (fname+(strlen(fname)-4), ".bmp"))
		fname[strlen(fname)-4] = '\0';
	cout << endl;

	cout << "\nDo you want to generate a solid (opaque), or a partially transparent\n";
	cout << "cloud layer?\n\n";
	cout << "(S) Solid cloud layer\n";
	cout << "(T) Transparent cloud layer\n";
	cout << "\n>> [S|T]: ";
	cin >> c;
	cout << endl;

	switch (toupper(c)) {
	case 'S':
		transp = false;
		patchflag = 1;
		break;
	case 'T':
		transp = true;
		patchflag = 2;
		break;
	default:
		FatalError ("Invalid selection");
		break;
	}

	if (patchflag == 2) {
		cout << "\nTo create a semi-transparent cloud layer, you also need to provide\n";
		cout << "a bitmap containing the opacity information of the cloud layer. This\n";
		cout << "bitmap must be of the same size and cover the same area as the cloud\n";
		cout << "colour bitmap. It must be in 8-bit or 24-bit BMP format. It should\n";
		cout << "contain only grey levels, where the brightness encodes opacity. White\n";
		cout << "pixels are fully opaque, black pixels are fully transparent.\n\n";
		cout << ">> Opacity map file name (.bmp): ";
		cin >> aname;
		if (!_stricmp (aname+(strlen(aname)-4), ".bmp"))
			aname[strlen(aname)-4] = '\0';
		cout << endl;
	}

	cout << "\nSpecify the resolution range for the cloud map. You need to enter\n";
	cout << "two numbers: the lowest and highest resolution to generate. The valid\n";
	cout << "range for both is 1 (lowest) to 8 (highest). The upper limit must be\n";
	cout << "greater or equal to the lower limit. The lower limit should normally\n";
	cout << "be 1. If you set the upper limit to 8, pltex will produce all\n";
	cout << "resolutions supported by the bitmap size.\n";
	cout << "\n>> Resolution range (1..8) [min max]: ";
	cin >> minres >> maxres;
	cout << endl;
	if (minres < 1 || maxres > maxlevel || minres > maxres) FatalError ("Invalid resolution range");

	ReadBMP_data (fname, mapw, maph, bpp);

	// check that required resolution level is supported by map
	for (lmax = maxlevel; lmax && (LevelW[lmax] > mapw || LevelH[lmax] > maph); lmax--);
	if (!lmax) FatalError ("Bitmap too small to generate textures");
	if (lmax < minres) FatalError ("Bitmap too small to generate requested resolution levels");
	if (lmax < maxres) {
		maxres = lmax;
		cout << "Warning: Bitmap only supports resolution level up to " << maxres << endl;
		cout << "         Resolution limit adjusted" << endl;
	}

	img = ReadBMP (fname, mapw, maph, bpp);
	patch = new RGB[PS*PS];
	strcpy (cbuf, fname);
	strcat (cbuf, ".tex");
	texf = fopen (cbuf, "wb");

	if (transp) {
		LONG amapw, amaph;
		WORD abpp;
		ReadBMP_data (aname, amapw, amaph, abpp);
		if (amapw != mapw || amaph != maph)
			FatalError ("Incompatible bitmap sizes");
		aimg = ReadBMPAlpha (aname, mapw, maph, bpp);
		apatch = new Alpha[PS*PS];
		memset (apatch, 0, sizeof(Alpha)*PS*PS);
	}

	cout << "-------------------------------------------------\n";
	cout << "Cloud colour bitmap file: " << fname << ".bmp\n";
	if (transp)
		cout << "Cloud opacity file: " << aname << ".bmp\n";
	cout << "Bitmap dimensions: " << mapw << "(W) x " << maph << "(H) x " << bpp << "(BPP)\n";
	cout << "Using patch size " << PS << " x " << PS << endl;
	cout << "Resolution range: " << minres << " - " << maxres << endl;

	cout << "\nProcessing ..." << endl << endl;
	cout << "Level |  Resolution | patches" << endl;
	cout << "-----------------------------" << endl;

	if (minres <= 1 && maxres >= 1) { // resolution level 1: 64x64
		cout << "  1   | " << setw(4) << PS/4 << " x " << setw(4) << PS/4 << " |    1     ";
		InitProgress (1, 40);
		tgt  = CompressRGB (img, mapw, maph, PS/4, PS/4);
		atgt = CompressAlpha (aimg, mapw, maph, PS/4, PS/4);
		CatDDS (texf, tgt, atgt, PS/4, PS/4);
		DeleteTargets (&tgt, &atgt, 0);
		IncProgress ();
	}

	if (minres <= 2 && maxres >= 2) { // resolution level 2: 128x128
		cout << "  2   | " << setw(4) << PS/2 << " x " << setw(4) << PS/2 << " |    1     ";
		InitProgress (1, 40);
		tgt  = CompressRGB (img, mapw, maph, PS/2, PS/2);
		atgt = CompressAlpha (aimg, mapw, maph, PS/2, PS/2);
		CatDDS (texf, tgt, atgt, PS/2, PS/2);
		DeleteTargets (&tgt, &atgt, 0);
		IncProgress ();
	}

	if (minres <= 3 && maxres >= 3) { // resolution level 3: 256x256
		cout << "  3   | " << setw(4) << PS << " x " << setw(4) << PS << " |    1     ";
		InitProgress (1, 40);
		tgt  = CompressRGB (img, mapw, maph, PS, PS);
		atgt = CompressAlpha (aimg, mapw, maph, PS, PS);
		CatDDS (texf, tgt, atgt, PS, PS);
		DeleteTargets (&tgt, &atgt, 0);
		IncProgress ();
	}

	if (minres <= 4 && maxres >= 4) { // resolution level 4: 512x256 (2 bitmaps)
		cout << "  4   | " << setw(4) << 2*PS << " x " << setw(4) << PS << " |    2     ";
		InitProgress (2, 40);
		tgt  = CompressRGB (img, mapw, maph, 2*PS, PS);
		atgt = CompressAlpha (aimg, mapw, maph, 2*PS, PS);
		for (i = 0; i < 2; i++) {
			ExtractPatchRGB (tgt, 2*PS, PS, i*PS, 0, patch, PS, PS);
			ExtractPatchAlpha (atgt, 2*PS, PS, i*PS, 0, apatch, PS, PS);
			CatDDS (texf, patch, apatch, PS, PS);
			IncProgress ();
		}
		DeleteTargets (&tgt, &atgt, 0);
	}

	if (minres <= 5 && maxres >= 5) { // resolution level 5: 1024x512 (8 bitmaps)
		cout << "  5   | " << setw(4) << 4*PS << " x " << setw(4) << 2*PS << " |    8     ";
		InitProgress (8, 40);
		tgt  = CompressRGB   (img, mapw, maph, 4*PS, 2*PS);
		atgt = CompressAlpha (aimg, mapw, maph, 4*PS, 2*PS);
		for (which = HEMISPHERE_NORTH; which <= HEMISPHERE_SOUTH; which++) {
			for (i = 0; i < 4; i++) {
				ExtractPatchRGB (tgt, 4*PS, 2*PS, i*PS, PS, patch, PS, PS, which);
				ExtractPatchAlpha (atgt, 4*PS, 2*PS, i*PS, PS, apatch, PS, PS, which);
				CatDDS (texf, patch, apatch, PS, PS);
				IncProgress();
			}
		}
		DeleteTargets (&tgt, &atgt, 0);
	}

	if (minres <= 6 && maxres >= 6) { // resolution level 6: 2048x1024 (24 bitmaps)
		cout << "  6   | " << setw(4) << 8*PS << " x " << setw(4) << 4*PS << " |   24     ";
		InitProgress (24, 40);
		for (which = HEMISPHERE_NORTH; which <= HEMISPHERE_SOUTH; which++) {
			tgt  = CompressRGB   (img, mapw, maph, 4*PS, 4*PS, which);
			atgt = CompressAlpha (aimg, mapw, maph, 4*PS, 4*PS, which);
			for (i = 0; i < 4; i++) {
				ExtractPatchRGB (tgt, 4*PS, 2*PS, i*PS, PS, patch, PS, PS, which);
				ExtractPatchAlpha (atgt, 4*PS, 2*PS, i*PS, PS, apatch, PS, PS, which);
				CatDDS (texf, patch, apatch, PS, PS);
				IncProgress();
			}
			DeleteTargets (&tgt, &atgt, 0);
			tgt  = CompressRGB   (img, mapw, maph, 8*PS, 4*PS, which);
			atgt = CompressAlpha (aimg, mapw, maph, 8*PS, 4*PS, which);
			for (i = 0; i < 8; i++) {
				ExtractPatchRGB (tgt, 8*PS, 2*PS, i*PS, 0, patch, PS, PS, which);
				ExtractPatchAlpha (atgt, 8*PS, 2*PS, i*PS, 0, apatch, PS, PS, which);
				CatDDS (texf, patch, apatch, PS, PS);
				IncProgress();
			}
			DeleteTargets (&tgt, &atgt, 0);
		}
	}

	if (minres <= 7 && maxres >= 7) { // resolution level 7: 4096x2048 (100 bitmaps)
		cout << "  7   | " << setw(4) << 16*PS << " x " << setw(4) << 8*PS << " |  100     ";
		InitProgress (100, 40);
		for (which = HEMISPHERE_NORTH; which <= HEMISPHERE_SOUTH; which++) {
			tgt  = CompressRGB   (img, mapw, maph, 6*PS, 8*PS, which);
			atgt = CompressAlpha (aimg, mapw, maph, 6*PS, 8*PS, which);
			for (i = 0; i < 6; i++) {
				ExtractPatchRGB (tgt, 6*PS, 4*PS, i*PS, 3*PS, patch, PS, PS, which);
				ExtractPatchAlpha (atgt, 6*PS, 4*PS, i*PS, 3*PS, apatch, PS, PS, which);
				CatDDS (texf, patch, apatch, PS, PS);
				IncProgress();
			}
			DeleteTargets (&tgt, &atgt, 0);
			tgt  = CompressRGB   (img, mapw, maph, 12*PS, 8*PS, which);
			atgt = CompressAlpha (aimg, mapw, maph, 12*PS, 8*PS, which);
			for (i = 0; i < 12; i++) {
				ExtractPatchRGB (tgt, 12*PS, 4*PS, i*PS, 2*PS, patch, PS, PS, which);
				ExtractPatchAlpha (atgt, 12*PS, 4*PS, i*PS, 2*PS, apatch, PS, PS, which);
				CatDDS (texf, patch, apatch, PS, PS);
				IncProgress();
			}
			DeleteTargets (&tgt, &atgt, 0);
			tgt  = CompressRGB   (img, mapw, maph, 16*PS, 8*PS, which);
			atgt = CompressAlpha (aimg, mapw, maph, 16*PS, 8*PS, which);
			for (k = 0; k < 2; k++) {
				for (i = 0; i < 16; i++) {
					ExtractPatchRGB (tgt, 16*PS, 4*PS, i*PS, PS-k*PS, patch, PS, PS, which);
					ExtractPatchAlpha (atgt, 16*PS, 4*PS, i*PS, PS-k*PS, apatch, PS, PS, which);
					CatDDS (texf, patch, apatch, PS, PS);
					IncProgress();
				}
			}
			DeleteTargets (&tgt, &atgt, 0);
		}
	}

	if (minres <= 8 && maxres >= 8) { // resolution level 8: 8192x4096 (364 bitmaps)
		cout << "  8   | " << setw(4) << 32*PS << " x " << setw(4) << 16*PS << " |  364     ";
		InitProgress (364, 40);
		for (which = HEMISPHERE_NORTH; which <= HEMISPHERE_SOUTH; which++) {
			tgt  = CompressRGB   (img, mapw, maph, 6*PS, 16*PS, which);
			atgt = CompressAlpha (aimg, mapw, maph, 6*PS, 16*PS, which);
			for (i = 0; i < 6; i++) {
				ExtractPatchRGB (tgt, 6*PS, 8*PS, i*PS, 7*PS, patch, PS, PS, which);
				ExtractPatchAlpha (atgt, 6*PS, 8*PS, i*PS, 7*PS, apatch, PS, PS, which);
				CatDDS (texf, patch, apatch, PS, PS);
				IncProgress();
			}
			DeleteTargets (&tgt, &atgt, 0);
			tgt  = CompressRGB   (img, mapw, maph, 12*PS, 16*PS, which);
			atgt = CompressAlpha (aimg, mapw, maph, 12*PS, 16*PS, which);
			for (i = 0; i < 12; i++) {
				ExtractPatchRGB (tgt, 12*PS, 8*PS, i*PS, 6*PS, patch, PS, PS, which);
				ExtractPatchAlpha (atgt, 12*PS, 8*PS, i*PS, 6*PS, apatch, PS, PS, which);
				CatDDS (texf, patch, apatch, PS, PS);
				IncProgress();
			}
			DeleteTargets (&tgt, &atgt, 0);
			tgt  = CompressRGB   (img, mapw, maph, 18*PS, 16*PS, which);
			atgt = CompressAlpha (aimg, mapw, maph, 18*PS, 16*PS, which);
			for (i = 0; i < 18; i++) {
				ExtractPatchRGB (tgt, 18*PS, 8*PS, i*PS, 5*PS, patch, PS, PS, which);
				ExtractPatchAlpha (atgt, 18*PS, 8*PS, i*PS, 5*PS, apatch, PS, PS, which);
				CatDDS (texf, patch, apatch, PS, PS);
				IncProgress();
			}
			DeleteTargets (&tgt, &atgt, 0);
			tgt  = CompressRGB   (img, mapw, maph, 24*PS, 16*PS, which);
			atgt = CompressAlpha (aimg, mapw, maph, 24*PS, 16*PS, which);
			for (i = 0; i < 24; i++) {
				ExtractPatchRGB (tgt, 24*PS, 8*PS, i*PS, 4*PS, patch, PS, PS, which);
				ExtractPatchAlpha (atgt, 24*PS, 8*PS, i*PS, 4*PS, apatch, PS, PS, which);
				CatDDS (texf, patch, apatch, PS, PS);
				IncProgress();
			}
			DeleteTargets (&tgt, &atgt, 0);
			tgt  = CompressRGB   (img, mapw, maph, 28*PS, 16*PS, which);
			atgt = CompressAlpha (aimg, mapw, maph, 28*PS, 16*PS, which);
			for (i = 0; i < 28; i++) {
				ExtractPatchRGB (tgt, 28*PS, 8*PS, i*PS, 3*PS, patch, PS, PS, which);
				ExtractPatchAlpha (atgt, 28*PS, 8*PS, i*PS, 3*PS, apatch, PS, PS, which);
				CatDDS (texf, patch, apatch, PS, PS);
				IncProgress();
			}
			DeleteTargets (&tgt, &atgt, 0);
			tgt  = CompressRGB   (img, mapw, maph, 30*PS, 16*PS, which);
			atgt = CompressAlpha (aimg, mapw, maph, 30*PS, 16*PS, which);
			for (i = 0; i < 30; i++) {
				ExtractPatchRGB (tgt, 30*PS, 8*PS, i*PS, 2*PS, patch, PS, PS, which);
				ExtractPatchAlpha (atgt, 30*PS, 8*PS, i*PS, 2*PS, apatch, PS, PS, which);
				CatDDS (texf, patch, apatch, PS, PS);
				IncProgress();
			}
			DeleteTargets (&tgt, &atgt, 0);
			tgt  = CompressRGB   (img, mapw, maph, 32*PS, 16*PS, which);
			atgt = CompressAlpha (aimg, mapw, maph, 32*PS, 16*PS, which);
			for (k = 0; k < 2; k++) {
				for (i = 0; i < 32; i++) {
					ExtractPatchRGB (tgt, 32*PS, 8*PS, i*PS, PS-k*PS, patch, PS, PS, which);
					ExtractPatchAlpha (atgt, 32*PS, 8*PS, i*PS, PS-k*PS, apatch, PS, PS, which);
					CatDDS (texf, patch, apatch, PS, PS);
					IncProgress();
				}
			}
			DeleteTargets (&tgt, &atgt, 0);
		}
	}

	// cleanup
	if (texf) fclose (texf);
	if (img)    delete []img;
	if (aimg)   delete []aimg;
	if (patch)  delete []patch;
	if (apatch) delete []apatch;

	cout << endl << "Cloud map written to " << fname << ".tex" << endl;
	cout << "\nTo use the new cloud map in Orbiter:\n";
	cout << "Rename to <planet>_cloud.tex and move to Orbiter\\Textures2 folder" << endl;
}

void SetOutputHeader (BITMAPFILEHEADER &bmfh, BITMAPINFOHEADER &bmih, LONG w, LONG h)
{
	char *id = (char*)&bmfh.bfType;
	id[0] = 'B';
	id[1] = 'M';
	bmfh.bfSize = sizeof (BITMAPFILEHEADER) + sizeof (BITMAPINFOHEADER) + w*h*3;
	bmfh.bfReserved1 = 0;
	bmfh.bfReserved2 = 0;
	bmfh.bfOffBits = sizeof (BITMAPINFOHEADER) + sizeof (BITMAPFILEHEADER);

	bmih.biSize = sizeof (BITMAPINFOHEADER);
	bmih.biWidth = w;
	bmih.biHeight = h;
	bmih.biPlanes = 1;
	bmih.biBitCount = 24;
	bmih.biCompression = BI_RGB;
	bmih.biSizeImage = 0;
	bmih.biXPelsPerMeter = 1;
	bmih.biYPelsPerMeter = 1;
	bmih.biClrUsed = 0;
	bmih.biClrImportant = 0;
}

RGB *CompressRGB (RGB *src, LONG srcw, LONG srch, LONG tgtw, long tgth, int which)
{
	return (RGB*)Compress (3, (BYTE*)src, srcw, srch, tgtw, tgth, which);
}

Alpha *CompressAlpha (Alpha *src, LONG srcw, LONG srch, LONG tgtw, long tgth, int which)
{
	return (Alpha*)Compress (1, (BYTE*)src, srcw, srch, tgtw, tgth, which);
}

BYTE *Compress (int nch, BYTE *src, LONG srcw, LONG srch, LONG tgtw, long tgth, int which)
{
	if (!src) return 0;

	LONG i, j, i0, i1, j0, j1, col;
	LONG srcj0 = (which != HEMISPHERE_NORTH ? 0 : srch/2);
	LONG srcj1 = (which != HEMISPHERE_SOUTH ? srch : srch/2);
	LONG nj = (which == HEMISPHERE_BOTH ? tgth : tgth/2);
	LONG tgtofs = (which != HEMISPHERE_NORTH ? 0 : tgth/2);
	double x, y, x0, x1, y0, y1, w0, w1, dx, dy;

	float *tmp1 = new float[srcw*nj];
	float *tmp2 = new float[tgtw*nj];
	float *tgtp;

	BYTE *srcp, *tgt = new BYTE[nch*tgtw*nj];

	for (col = 0; col < nch; col++) {
		dx = (double)tgtw/(double)srcw;
		dy = (double)tgth/(double)srch;
		for (i = 0; i < srcw*nj; i++) tmp1[i] = 0;
		for (i = 0; i < tgtw*nj; i++) tmp2[i] = 0;

		// compress vertical
		for (j = srcj0; j < srcj1; j++) {
			y = (double)(j+0.5) * dy;
			y0 = y - 0.5*dy + eps;
			y1 = y + 0.5*dy - eps;
			j0 = (int)y0 - tgtofs;
			j1 = (int)y1 - tgtofs;
			if (j0 == j1) {
				srcp = src + j*srcw*nch;
				tgtp = tmp1 + j0*srcw;
				for (i = 0; i < srcw; i++) tgtp[i] += (float)srcp[i*nch+col];
			} else {
				if (j1 > j0+1) { // sanity check
					cerr << "Internal error 1" << endl;
					exit (1);
				}
				w0 = (ceil(y0)-y0)/dy;
				w1 = (y1-floor(y1))/dy;
				srcp = src + j*srcw*nch;
				tgtp = tmp1 + j0*srcw;
				for (i = 0; i < srcw; i++) {
					tgtp[i] += (float)(srcp[i*nch+col] * w0);
					tgtp[i+srcw] += (float)(srcp[i*nch+col] * w1);
				}
			}
		}
		// compress horizontal
		for (i = 0; i < srcw; i++) {
			x = (double)(i+0.5) * dx; // x-position of source pixel centre in target map
			x0 = x - 0.5*dx + eps;    // left edge of source pixel in target map
			x1 = x + 0.5*dx - eps;    // right edge of source pixel in target map
			i0 = (int)x0;
			i1 = (int)x1;
			if (i0 == i1) {
				for (j = 0; j < nj; j++) tmp2[j*tgtw+i0] += tmp1[j*srcw+i];
			} else {
				if (i1 > i0+1) { // sanity check
					cerr << "Internal error 2" << endl;
					exit (1);
				}
				w0 = (ceil(x0)-x0)/dx;
				w1 = (x1-floor(x1))/dx;
				for (j = 0; j < nj; j++) {
					tmp2[j*tgtw+i0] += (float)(tmp1[j*srcw+i] * w0);
					tmp2[j*tgtw+i1] += (float)(tmp1[j*srcw+i] * w1);
				}
			}
		}

		// store back to target bitmap
		for (i = 0; i < tgtw*nj; i++)
			tgt[i*nch+col] = (BYTE)(tmp2[i]*dx*dy + 0.49);
	}

	delete []tmp1;
	delete []tmp2;
	return tgt;
}

RGB *BinaryCompress (RGB *src, LONG srcw, LONG srch, LONG tgtw, long tgth)
{
	LONG nw = srcw, nh = srch, nw2, nh2, i, j;
	RGB *tgt = src, *osrc = src, *srcp, *tgtp;
	while (nw > tgtw) {
		nw2 = nw/2;
		tgt = new RGB[nw2*nh];
		for (j = 0; j < nh; j++) {
			srcp = src + j*nw;
			tgtp = tgt + j*nw2;
			for (i = 0; i < nw2; i++) {
				tgtp[i].r = (BYTE)(((DWORD)srcp[2*i].r + (DWORD)srcp[2*i+1].r)/2);
				tgtp[i].g = (BYTE)(((DWORD)srcp[2*i].g + (DWORD)srcp[2*i+1].g)/2);
				tgtp[i].b = (BYTE)(((DWORD)srcp[2*i].b + (DWORD)srcp[2*i+1].b)/2);
			}
		}
		if (src != osrc) delete []src;
		src = tgt;
		nw = nw2;
	}
	while (nh > tgth) {
		nh2 = nh/2;
		tgt = new RGB[nw*nh2];
		for (j = 0; j < nh2; j++) {
			srcp = src + j*2*nw;
			tgtp = tgt + j*nw;
			for (i = 0; i < nw; i++) {
				tgtp[i].r = (BYTE)(((DWORD)srcp[i].r + (DWORD)srcp[i+nw].r)/2);
				tgtp[i].g = (BYTE)(((DWORD)srcp[i].g + (DWORD)srcp[i+nw].g)/2);
				tgtp[i].b = (BYTE)(((DWORD)srcp[i].b + (DWORD)srcp[i+nw].b)/2);
			}
		}
		if (src != osrc) delete []src;
		src = tgt;
		nh = nh2;
	}
	return tgt;
}

void ExtractPatchRGB (RGB *src, LONG srcw, LONG srch, LONG x0, LONG y0, RGB *tgt, LONG tgtw, LONG tgth, int which)
{
	if (which != HEMISPHERE_SOUTH) ExtractPatch (3, (BYTE*)src, srcw, srch, x0, y0, (BYTE*)tgt, tgtw, tgth);
	else                           ExtractPatchRotated (3, (BYTE*)src, srcw, srch, x0, y0, (BYTE*)tgt, tgtw, tgth);
}

void ExtractPatchAlpha (Alpha *src, LONG srcw, LONG srch, LONG x0, LONG y0, Alpha *tgt, LONG tgtw, LONG tgth, int which)
{
	if (which != HEMISPHERE_SOUTH) ExtractPatch (1, (BYTE*)src, srcw, srch, x0, y0, (BYTE*)tgt, tgtw, tgth);
	else                           ExtractPatchRotated (1, (BYTE*)src, srcw, srch, x0, y0, (BYTE*)tgt, tgtw, tgth);
}

void ExtractPatchRGBA (RGB *src, Alpha *asrc, LONG srcw, LONG srch, LONG x0, LONG y0, RGB *tgt, Alpha *atgt, LONG tgtw, LONG tgth, int which)
{
	if (which != HEMISPHERE_SOUTH) ExtractPatchRGBA (src, asrc, srcw, srch, x0, y0, tgt, atgt, tgtw, tgth);
	else                           ExtractPatchRotatedRGBA (src, asrc, srcw, srch, x0, y0, tgt, atgt, tgtw, tgth);
}

void ExtractPatchRGBA (RGB *src, Alpha *asrc, LONG srcw, LONG srch, LONG x0, LONG y0, RGB *tgt, Alpha *atgt, LONG tgtw, LONG tgth)
{
	// extract patches for RGB and alpha channels
	ExtractPatch (3, (BYTE*)src, srcw, srch, x0, y0, (BYTE*)tgt, tgtw, tgth);
	ExtractPatch (1, (BYTE*)asrc, srcw, srch, x0, y0, (BYTE*)atgt, tgtw, tgth);
}

void ExtractPatchRotatedRGBA (RGB *src, Alpha *asrc, LONG srcw, LONG srch, LONG x0, LONG y0, RGB *tgt, Alpha *atgt, LONG tgtw, LONG tgth)
{
	// extract patches for RGB and alpha channels
	ExtractPatchRotated (3, (BYTE*)src, srcw, srch, x0, y0, (BYTE*)tgt, tgtw, tgth);
	ExtractPatchRotated (1, (BYTE*)asrc, srcw, srch, x0, y0, (BYTE*)atgt, tgtw, tgth);
}

void ExtractPatch (int nch, BYTE *src, LONG srcw, LONG srch, LONG x0, LONG y0, BYTE *tgt, LONG tgtw, LONG tgth)
{
	if (!src) return;

	LONG i, j, k;
	BYTE *srcp, *tgtp;

	for (j = 0; j < tgth; j++) {
		srcp = src + ((y0+j)*srcw + x0)*nch;
		tgtp = tgt + j*tgtw*nch;
		for (i = 0; i < tgtw; i++)
			for (k = 0; k < nch; k++)
				tgtp[i*nch+k] = srcp[i*nch+k];
	}
}

void ExtractPatchRotated (int nch, BYTE *src, LONG srcw, LONG srch, LONG x0, LONG y0, BYTE *tgt, LONG tgtw, LONG tgth)
{
	if (!src) return;

	LONG i, j, k;
	BYTE *srcp, *tgtp;

	for (j = 0; j < tgth; j++) {
		srcp = src + (srch-1-y0-j)*srcw*nch;
		tgtp = tgt + j*tgtw*nch;
		for (i = 0; i < tgtw; i++)
			for (k = 0; k < nch; k++)
				tgtp[i*nch+k] = srcp[(srcw-1-x0-i)*nch+k];
	}
}

void SamplePatch (RGB *img, LONG imgw, LONG imgh, double maplng0, double maplng1, double maplat0, double maplat1,
				  RGB *tgt, LONG tgtw, LONG tgth, double tgtlng0, double tgtlng1, double tgtlat0, double tgtlat1, int which)
{
	const double eps = 1e-10;
	LONG i, j;
	double tgt_dlat = (tgtlat1-tgtlat0)/tgth;
	double tgt_dlng = (tgtlng1-tgtlng0)/tgtw;
	double x, y, lng, lat, latw0, latw1, lngw0, lngw1, sum;
	int x0, x1, y0, y1, ch, idx;
	for (j = 0; j < tgth; j++) {
		lat = tgtlat0 + tgt_dlat * (j+0.5);
		y = (lat-maplat0)/(maplat1-maplat0)*imgh;
		y0 = (int)floor (y-0.5);
		if (y0 < 0) {
			y0 = y1 = 0; latw0 = 1.0; latw1 = 0.0;
		} else if (y0 >= imgh-1) {
			y0 = y1 = imgh-1; latw0 = 0.0; latw1 = 1.0;
		} else {
			y1 = y0+1; latw1 = y-0.5-y0; latw0 = 1.0-latw1;
		}
		for (i = 0; i < tgtw; i++) {
			lng = tgtlng0 + tgt_dlng * (i+0.5);
			x = (lng-maplng0)/(maplng1-maplng0)*imgw;
			x0 = (int)floor (x-0.5);
			if (x0 < 0) {
				x0 = x1 = 0; lngw0 = 1.0; lngw1 = 0.0;
			} else if (x0 >= imgw-1) {
				x0 = x1 = imgw-1; lngw0 = 0.0; lngw1 = 1.0;
			} else {
				x1 = x0+1; lngw1 = x-0.5-x0; lngw0 = 1.0-lngw1;
			}
			if (which == HEMISPHERE_NORTH)
				idx = j*tgtw+i;
			else
				idx = tgtw*tgth - (j*tgtw+i) - 1;
			for (ch = 0; ch < 3; ch++) {
				sum =  img[y0*imgw+x0].data[ch] * latw0*lngw0;
				sum += img[y0*imgw+x1].data[ch] * latw0*lngw1;
				sum += img[y1*imgw+x0].data[ch] * latw1*lngw0;
				sum += img[y1*imgw+x1].data[ch] * latw1*lngw1;
				tgt[idx].data[ch] = (int)(sum+0.5);
			}
		}
	}
}

void SampleAPatch (Alpha *img, LONG imgw, LONG imgh, double maplng0, double maplng1, double maplat0, double maplat1,
				   Alpha *tgt, LONG tgtw, LONG tgth, double tgtlng0, double tgtlng1, double tgtlat0, double tgtlat1, int which)
{
	const double eps = 1e-10;
	LONG i, j;
	double tgt_dlat = (tgtlat1-tgtlat0)/tgth;
	double tgt_dlng = (tgtlng1-tgtlng0)/tgtw;
	double x, y, lng, lat, latw0, latw1, lngw0, lngw1, sum;
	int x0, x1, y0, y1, idx;
	for (j = 0; j < tgth; j++) {
		lat = tgtlat0 + tgt_dlat * (j+0.5);
		y = (lat-maplat0)/(maplat1-maplat0)*imgh;
		y0 = (int)floor (y-0.5);
		if (y0 < 0) {
			y0 = y1 = 0; latw0 = 1.0; latw1 = 0.0;
		} else if (y0 >= imgh-1) {
			y0 = y1 = imgh-1; latw0 = 0.0; latw1 = 1.0;
		} else {
			y1 = y0+1; latw1 = y-0.5-y0; latw0 = 1.0-latw1;
		}
		for (i = 0; i < tgtw; i++) {
			lng = tgtlng0 + tgt_dlng * (i+0.5);
			x = (lng-maplng0)/(maplng1-maplng0)*imgw;
			x0 = (int)floor (x-0.5);
			if (x0 < 0) {
				x0 = x1 = 0; lngw0 = 1.0; lngw1 = 0.0;
			} else if (x0 >= imgw-1) {
				x0 = x1 = imgw-1; lngw0 = 0.0; lngw1 = 1.0;
			} else {
				x1 = x0+1; lngw1 = x-0.5-x0; lngw0 = 1.0-lngw1;
			}
			if (which == HEMISPHERE_NORTH)
				idx = j*tgtw+i;
			else
				idx = tgtw*tgth - (j*tgtw+i) - 1;
			sum =  img[y0*imgw+x0] * latw0*lngw0;
			sum += img[y0*imgw+x1] * latw0*lngw1;
			sum += img[y1*imgw+x0] * latw1*lngw0;
			sum += img[y1*imgw+x1] * latw1*lngw1;
			tgt[idx] = (int)(sum+0.5);
		}
	}
}

void ErodeLights (RGB *limg, Alpha *aimg, LONG imgw, LONG imgh)
{
	// remove lights from any water pixels including a 1-pixel border along
	// coastlines to avoid edge artefacts
	static RGB zero = {0,0,0};
	LONG i, j, ii, jj, imin, imax, jmin, jmax;
	for (i = 0; i < imgh; i++) {
		imin = max (0, i-1);
		imax = min (imgh-1, i+1);
		for (j = 0; j < imgw; j++) {
			if (aimg[i*imgw+j] >= 128) { // water pixel
				jmin = max (0, j-1);
				jmax = min (imgw-1, j+1);
				for (ii = imin; ii <= imax; ii++)
					for (jj = jmin; jj <= jmax; jj++)
						limg[ii*imgw+jj] = zero;
			}
		}
	}
}

DWORD CatDDS (FILE *texf, RGB *img, Alpha *aimg, LONG imgw, LONG imgh, bool force, bool mipmap)
{
	const char *bmpname  = "tmp.bmp";
	const char *abmpname = "tmp_a.bmp";
	const char *ddsname  = "tmp.dds";

	static RGB *rgbdummy = 0;
	if (!img) {
		if (!rgbdummy) {
			rgbdummy = new RGB[PS*PS];
			memset (rgbdummy, 0, sizeof(RGB)*PS*PS);
		}
		img = rgbdummy;
	}

	FILE *bmpf;
	BITMAPFILEHEADER bmfh;
	BITMAPINFOHEADER bmih;
	int i, res, ddssize = 0, fh;
	bool bopaque = false, btransparent = false;
	int nopaque, ntransparent;

	SetOutputHeader (bmfh, bmih, imgw, imgh);
	bmpf = fopen (bmpname, "wb");
	if (!bmpf) FatalError ("Could not open temporary bitmap file.");
	fwrite (&bmfh, sizeof(BITMAPFILEHEADER), 1, bmpf);
	fwrite (&bmih, sizeof(BITMAPINFOHEADER), 1, bmpf);
	fwrite (img, 3, imgw*imgh, bmpf);
	fclose (bmpf);

	if (aimg) {
		bmpf = fopen (abmpname, "wb");
		if (!bmpf) FatalError ("Could not open temporary alpha file.");
		fwrite (&bmfh, sizeof(BITMAPFILEHEADER), 1, bmpf);
		fwrite (&bmih, sizeof(BITMAPINFOHEADER), 1, bmpf);

		// make alpha binary black/white, and count opaque/transparent pixels 
		for (i = nopaque = ntransparent = 0; i < imgw*imgh; i++) {
			if (binary_alpha) {
				aimg[i] = 255 - aimg[i];   // invert
				if (aimg[i] < 128) aimg[i] =   0, ntransparent++;
				else               aimg[i] = 255, nopaque++;
			} else {
				if (aimg[i] < 128) ntransparent++;
				else               nopaque++;
			}
		}
		if (ntransparent > imgw*imgh * g_tol) btransparent = true;
		else if (ntransparent) { // suppress transparent pixels
			for (i = 0; i < imgw*imgh; i++) aimg[i] = 255;
			g_nsuppressed++;
		}
		if (nopaque > imgw*imgh * g_tol) bopaque = true;
		else if (nopaque) { // suppress opaque pixels
			for (i = 0; i < imgw*imgh; i++) aimg[i] = 0;
			g_nsuppressed++;
		}

		for (i = 0; i < imgw*imgh; i++) {
			RGB rgb;
			rgb.r = rgb.g = rgb.b = aimg[i];
			fwrite (&rgb, 3, 1, bmpf);
		}
		fclose (bmpf);

		res = _spawnl (_P_WAIT, dxtex, dxtex, bmpname, "-a", abmpname, mipmap ? "-m" : "", binary_alpha ? "DXT1" : "DXT5", ddsname, NULL);
	} else {
		res = _spawnl (_P_WAIT, dxtex, dxtex, bmpname, mipmap ? "-m" : "", "DXT1", ddsname, NULL);
	}
	if (res != 0) FatalError ("Executing dxtex failed.");

	// get file size
    if ((fh = _open (ddsname, _O_RDONLY)) == -1) FatalError ("Could not open DDS file");
    ddssize = _filelength (fh);
	_close (fh);
	
	if (force || !aimg || !selective_alpha || (bopaque && btransparent)) {
		FILE *ddsf = fopen (ddsname, "rb");
		BYTE *buf = new BYTE[ddssize];
		fread (buf, 1, ddssize, ddsf);
		fclose (ddsf);
		fwrite (buf, 1, ddssize, texf);
		delete []buf;
	} else ddssize = 0;

	remove (bmpname);
	remove (ddsname);
	if (aimg) remove (abmpname);
	return (DWORD)ddssize;
}

WORD CatMaskDDS (FILE *texf, RGB *img, Alpha *aimg, LONG imgw, LONG imgh)
{
	// Takes an RGB patch (for city lights) if provided, and an alpha patch (for
	// land-water mask) if provided, and generates a binary alpha (DXT1) DDS patch
	// from it.
	// Return value is bit-flag of what was written:
	// bit 0: patch contains land portion   (alpha)
	// bit 1: patch contains water portion  (alpha)
	// bit 2: patch contains city lights    (RGB)

	const char *bmpname  = "tmp.bmp";
	const char *abmpname = "tmp_a.bmp";
	const char *ddsname  = "tmp.dds";

	static RGB *rgbdummy = 0;
	static RGB  zero = {0,0,0};

	FILE *bmpf;
	BITMAPFILEHEADER bmfh;
	BITMAPINFOHEADER bmih;
	int i, res, ddssize, fh;
	bool bopaque = false, btransparent = false;
	bool brgb = false, balpha = false;
	int nlight, nopaque, ntransparent;
	WORD flag = 0;
	bool written = true;

	SetOutputHeader (bmfh, bmih, imgw, imgh);

	if (img)  { // city-light texture provided
		// count light pixels
		for (i = nlight = 0; i < imgw*imgh; i++) {
			if ((int)img[i].r + (int)img[i].g + (int)img[i].b > 32*3)
				nlight++;
			//else
			//	img[i].r = img[i].g = img[i].b = 0; // suppress dark pixels to black
		}
		if (nlight > imgw*imgh * g_light_tol) { // # light pixels above threshold?
			brgb = true;
			flag |= 4; // city lights present
		}
	}

	if (!brgb) { // provide dummy RGB patch
		if (!rgbdummy) {
			rgbdummy = new RGB[PS*PS];
			memset (rgbdummy, 0, sizeof(RGB)*PS*PS);
		}
		img = rgbdummy;
	}

	if (aimg) { // land-water mask provided
		// make alpha binary black/white, and count opaque/transparent pixels 
		for (i = nopaque = ntransparent = 0; i < imgw*imgh; i++) {
			aimg[i] = 255 - aimg[i];   // invert
			if (aimg[i] < 128) aimg[i] =   0, ntransparent++;
			else               aimg[i] = 255, nopaque++;
		}
		if (ntransparent > imgw*imgh * g_tol) { // # transparent pixels above threshold?
			btransparent = true;
			flag |= 2; // water present
		} else if (ntransparent) {              // suppress transparent pixels
			for (i = 0; i < imgw*imgh; i++) aimg[i] = 255;
			g_nsuppressed++;
		}
		if (nopaque > imgw*imgh * g_tol) {      // # opaque pixels above threshold?
			flag |= 1; // land present
			bopaque = true;
		} else if (nopaque) {                   // suppress opaque pixels
			for (i = 0; i < imgw*imgh; i++) aimg[i] = 0;
			g_nsuppressed++;
		}
		balpha = btransparent && bopaque;
		// we only need to include an alpha channel if the patch contains both
		// opaque and transparent pixels, i.e. it is a mixed land-water patch
	}

	if (!brgb && !balpha) return flag;
	// nothing to do: patch contains no lights and no land-water mask

	// remove lights from any water pixels including a 1-pixel border along
	// coastlines to avoid edge artefacts
	if (brgb && balpha) {
		int imin, imax, ii, jmin, jmax, j, jj;
		for (i = 0; i < imgh; i++) {
			imin = max (0, i-1);
			imax = min (imgh-1, i+1);
			for (j = 0; j < imgw; j++) {
				jmin = max (0, j-1);
				jmax = min (imgw-1, j+1);
				if (aimg[i*imgw+j] == 0) // water pixel
					for (ii = imin; ii <= imax; ii++)
						for (jj = jmin; jj <= jmax; jj++)
							img[ii*imgw+jj] = zero;
			}
		}
	}

	// create a bitmap for the RGB channels
	bmpf = fopen (bmpname, "wb");
	if (!bmpf) FatalError ("Could not open temporary bitmap file.");
	fwrite (&bmfh, sizeof(BITMAPFILEHEADER), 1, bmpf);
	fwrite (&bmih, sizeof(BITMAPINFOHEADER), 1, bmpf);
	fwrite (img, 3, imgw*imgh, bmpf);
	fclose (bmpf);

	// create a bitmap for the alpha channel
	if (balpha) {
		bmpf = fopen (abmpname, "wb");
		if (!bmpf) FatalError ("Could not open temporary alpha file.");
		fwrite (&bmfh, sizeof(BITMAPFILEHEADER), 1, bmpf);
		fwrite (&bmih, sizeof(BITMAPINFOHEADER), 1, bmpf);
		for (i = 0; i < imgw*imgh; i++) {
			RGB rgb;
			rgb.r = rgb.g = rgb.b = aimg[i];
			fwrite (&rgb, 3, 1, bmpf);
		}
		fclose (bmpf);
	}

	// now convert to DXT1 texture
	if (balpha)
		res = _spawnl (_P_WAIT, dxtex, dxtex, bmpname, "-a", abmpname, "DXT1", ddsname, NULL);
	else
		res = _spawnl (_P_WAIT, dxtex, dxtex, bmpname, "DXT1", ddsname, NULL);
	if (res != 0) FatalError ("Executing dxtex failed.");

	// get file size
    if ((fh = _open (ddsname, _O_RDONLY)) == -1) FatalError ("Could not open DDS file");
    ddssize = _filelength (fh);
	_close (fh);
	
	// append to texture file
	FILE *ddsf = fopen (ddsname, "rb");
	BYTE *buf = new BYTE[ddssize];
	fread (buf, 1, ddssize, ddsf);
	fclose (ddsf);
	fwrite (buf, 1, ddssize, texf);
	delete[]buf;

	remove (bmpname);
	remove (ddsname);
	if (aimg) remove (abmpname);
	return flag;
}

void ReadBMP_data (char *fname, LONG &mapw, LONG &maph, WORD &bpp)
{
	char cbuf[256], *id;
	FILE *fbmp;
	BITMAPFILEHEADER bmfh;
	BITMAPINFO *bmi;

	strcpy (cbuf, fname);
	strcat (cbuf, ".bmp");
	fbmp = fopen (cbuf, "rb");
	if (!fbmp) FatalError ("Input file not found");
	if (!fread (&bmfh, sizeof (BITMAPFILEHEADER), 1, fbmp))
		FatalError ("Cannot read bitmap file header");
	id = (char*)&bmfh.bfType;
	if (id[0] != 'B' || id[1] != 'M') FatalError ("Wrong input file format");

	BYTE *tmp = new BYTE[bmfh.bfOffBits];
	fread (tmp, 1, bmfh.bfOffBits-sizeof(BITMAPFILEHEADER), fbmp);
	bmi = (BITMAPINFO*)tmp;

	mapw = bmi->bmiHeader.biWidth;
	maph = bmi->bmiHeader.biHeight;
	bpp  = bmi->bmiHeader.biBitCount;

	fclose(fbmp);
	delete []tmp;
}

RGB *ReadBMP (char *fname, LONG &mapw, LONG &maph, WORD &bpp)
{
	char cbuf[256], *id;
	FILE *fbmp;
	BITMAPFILEHEADER bmfh;
	BITMAPINFO *bmi;
	DWORD i, imgsize;
	RGB *img;

	strcpy (cbuf, fname);
	strcat (cbuf, ".bmp");
	fbmp = fopen (cbuf, "rb");
	if (!fbmp) FatalError ("Input file not found");
	if (!fread (&bmfh, sizeof (BITMAPFILEHEADER), 1, fbmp))
		FatalError ("Cannot read bitmap file header");
	id = (char*)&bmfh.bfType;
	if (id[0] != 'B' || id[1] != 'M') FatalError ("Wrong input file format");

	BYTE *tmp = new BYTE[bmfh.bfOffBits];
	fread (tmp, 1, bmfh.bfOffBits-sizeof(BITMAPFILEHEADER), fbmp);
	bmi = (BITMAPINFO*)tmp;

	mapw = bmi->bmiHeader.biWidth;
	maph = bmi->bmiHeader.biHeight;
	bpp  = bmi->bmiHeader.biBitCount;
	imgsize = mapw*maph;
	if (bmi->bmiHeader.biCompression != BI_RGB)
		FatalError ("Cannot process compressed source bitmaps");

	img = new RGB[imgsize];
	switch (bpp) {
	case 8: {
		BYTE b;
		for (i = 0; i < imgsize; i++) {
			fread (&b, 1, 1, fbmp);
			img[i].r = bmi->bmiColors[b].rgbRed;
			img[i].g = bmi->bmiColors[b].rgbGreen;
			img[i].b = bmi->bmiColors[b].rgbBlue;
		}}
		break;
	case 24:
		fread (img, sizeof(RGB), imgsize, fbmp);
		break;
	default:
		FatalError ("Unsupported source colour depth");
	}
	fclose(fbmp);
	delete []tmp;
	return img;
}

RGB *ReadBMP_band (char *fname, LONG &mapw, LONG &maph, WORD &bpp, LONG line0, LONG nlines)
{
	char cbuf[256], *id;
	FILE *fbmp;
	BITMAPFILEHEADER bmfh;
	BITMAPINFO *bmi;
	DWORD imgsize;
	LONG i, j;
	RGB *img;

	strcpy (cbuf, fname);
	strcat (cbuf, ".bmp");
	fbmp = fopen (cbuf, "rb");
	if (!fbmp) FatalError ("Input file not found");
	if (!fread (&bmfh, sizeof (BITMAPFILEHEADER), 1, fbmp))
		FatalError ("Cannot read bitmap file header");
	id = (char*)&bmfh.bfType;
	if (id[0] != 'B' || id[1] != 'M') FatalError ("Wrong input file format");

	BYTE *tmp = new BYTE[bmfh.bfOffBits];
	fread (tmp, 1, bmfh.bfOffBits-sizeof(BITMAPFILEHEADER), fbmp);
	bmi = (BITMAPINFO*)tmp;

	if (line0+nlines > bmi->bmiHeader.biHeight) FatalError ("Error extracting bitmap band");

	mapw = bmi->bmiHeader.biWidth;
	maph = nlines;
	bpp  = bmi->bmiHeader.biBitCount;
	imgsize = mapw*maph;
	if (bmi->bmiHeader.biCompression != BI_RGB)
		FatalError ("Cannot process compressed source bitmaps");

	img = new RGB[imgsize];
	switch (bpp) {
	case 8: {
		BYTE *line = new BYTE[mapw];
		for (j = 0; j < line0; j++)
			fread (line, 1, mapw, fbmp); // skip these lines
		for (j = 0; j < nlines; j++) {
			fread (line, 1, mapw, fbmp);
			for (i = 0; i < mapw; i++) {
				img[j*mapw+i].r = bmi->bmiColors[line[i]].rgbRed;
				img[j*mapw+i].g = bmi->bmiColors[line[i]].rgbGreen;
				img[j*mapw+i].b = bmi->bmiColors[line[i]].rgbBlue;
			}
		}
		delete []line;
		}
		break;
	case 24:
		for (j = 0; j < line0; j++)
			fread (img, sizeof(RGB), mapw, fbmp); // skip these lines
		for (j = 0; j < nlines; j++)
			fread (img+j*mapw, sizeof(RGB), mapw, fbmp);
		break;
	default:
		FatalError ("Unsupported source colour depth");
	}
	fclose(fbmp);
	delete []tmp;
	return img;
}

Alpha *ReadBMPAlpha (char *fname, LONG &mapw, LONG &maph, WORD &bpp)
{
	char cbuf[256], *id;
	FILE *fbmp;
	BITMAPFILEHEADER bmfh;
	BITMAPINFO *bmi;
	DWORD i, imgsize;
	Alpha *aimg;

	strcpy (cbuf, fname);
	strcat (cbuf, ".bmp");
	fbmp = fopen (cbuf, "rb");
	if (!fbmp) FatalError ("Input file not found");
	if (!fread (&bmfh, sizeof (BITMAPFILEHEADER), 1, fbmp))
		FatalError ("Cannot read bitmap file header");
	id = (char*)&bmfh.bfType;
	if (id[0] != 'B' || id[1] != 'M') FatalError ("Wrong input file format");

	BYTE *tmp = new BYTE[bmfh.bfOffBits];
	fread (tmp, 1, bmfh.bfOffBits-sizeof(BITMAPFILEHEADER), fbmp);
	bmi = (BITMAPINFO*)tmp;

	mapw = bmi->bmiHeader.biWidth;
	maph = bmi->bmiHeader.biHeight;
	bpp  = bmi->bmiHeader.biBitCount;
	imgsize = mapw*maph;
	if (bmi->bmiHeader.biCompression != BI_RGB)
		FatalError ("Cannot process compressed source bitmaps");

	aimg = new Alpha[imgsize];
	switch (bpp) {
	case 8: {
		BYTE b;
		for (i = 0; i < imgsize; i++) {
			fread (&b, 1, 1, fbmp);
			aimg[i] = bmi->bmiColors[b].rgbBlue;
		}}
		break;
	case 24: {
		RGB rgb;
		for (i = 0; i < imgsize; i++) {
			fread (&rgb, sizeof(RGB), 1, fbmp);
			aimg[i] = rgb.b;
		}}
		break;
	default:
		FatalError ("Unsupported source colour depth");
	}
	fclose(fbmp);
	delete []tmp;
	return aimg;
}

Alpha *ReadBMPAlpha_band (char *fname, LONG &mapw, LONG &maph, WORD &bpp, LONG line0, LONG nlines)
{
	char cbuf[256], *id;
	FILE *fbmp;
	BITMAPFILEHEADER bmfh;
	BITMAPINFO *bmi;
	DWORD imgsize;
	LONG i, j;
	Alpha *aimg;

	strcpy (cbuf, fname);
	strcat (cbuf, ".bmp");
	fbmp = fopen (cbuf, "rb");
	if (!fbmp) FatalError ("Input file not found");
	if (!fread (&bmfh, sizeof (BITMAPFILEHEADER), 1, fbmp))
		FatalError ("Cannot read bitmap file header");
	id = (char*)&bmfh.bfType;
	if (id[0] != 'B' || id[1] != 'M') FatalError ("Wrong input file format");

	BYTE *tmp = new BYTE[bmfh.bfOffBits];
	fread (tmp, 1, bmfh.bfOffBits-sizeof(BITMAPFILEHEADER), fbmp);
	bmi = (BITMAPINFO*)tmp;

	if (line0+nlines > bmi->bmiHeader.biHeight) FatalError ("Error extracting bitmap band");

	mapw = bmi->bmiHeader.biWidth;
	maph = nlines;
	bpp  = bmi->bmiHeader.biBitCount;
	imgsize = mapw*maph;
	if (bmi->bmiHeader.biCompression != BI_RGB)
		FatalError ("Cannot process compressed source bitmaps");

	aimg = new Alpha[imgsize];
	switch (bpp) {
	case 8: {
		BYTE *line = new BYTE[mapw];
		for (j = 0; j < line0; j++)
			fread (line, 1, mapw, fbmp); // skip these lines
		for (j = 0; j < nlines; j++) {
			fread (line, 1, mapw, fbmp);
			for (i = 0; i < mapw; i++) {
				aimg[j*mapw+i] = bmi->bmiColors[line[i]].rgbBlue;
			}
		}
		delete []line;
		}
		break;
	case 24:
		RGB rgb;
		for (j = 0; j < line0; j++)
			for (i = 0; i < mapw; i++)
				fread (&rgb, sizeof(RGB), 1, fbmp); // skip these lines
		for (j = 0; j < nlines; j++)
			for (i = 0; i < mapw; i++) {
				fread (&rgb, sizeof(RGB), 1, fbmp);
				aimg[j*mapw+i] = rgb.b;
			}
		break;
	default:
		FatalError ("Unsupported source colour depth");
	}
	fclose(fbmp);
	delete []tmp;
	return aimg;
}

LONG TransparentCount (Alpha *atgt, LONG w, LONG h)
{
	LONG i, n = w*h, transp = 0;
	for (i = 0; i < n; i++)
		if (atgt[i] < 128) transp++;
	return transp;
}

LONG BrightCount (RGB *tgt, LONG w, LONG h)
{
	LONG i, n = w*h, light = 0;
	for (i = 0; i < n; i++)
		if ((DWORD)tgt[i].r + (DWORD)tgt[i].g + (DWORD)tgt[i].b > 64*3) light++;
	return light;
}

void DeleteTargets (RGB **tgt, Alpha **atgt, RGB **ltgt)
{
	if (tgt  && *tgt)  delete []*tgt,  *tgt = 0;
	if (atgt && *atgt) delete []*atgt, *atgt = 0;
	if (ltgt && *ltgt) delete []*ltgt, *ltgt = 0;
}

void FatalError (const char *msg)
{
	cerr << endl << "pltex ERROR: " << msg << endl;
	cerr << "Press a key to terminate." << endl;
	while (_kbhit()) _getch();
	while (!_kbhit());
	exit (1);
}

static int prog, prog_p, prog_ntot, prog_len;

void InitProgress (int ntot, int len)
{
	prog_ntot = ntot;
	prog_len = len;
	prog = 0;
	prog_p = 0;
	cout << '|' << flush;
}

void SetProgress (int p)
{
	int i, len = (p*prog_len)/prog_ntot;
	prog_p = p;
	if (len > prog) {
		for (i = 0; i < len-prog; i++)
			cout << '=' << flush;
		prog = len;
	}
	if (p == prog_ntot)
		cout << '|' << endl;
}

void IncProgress ()
{
	SetProgress (prog_p+1);
}

DWORD CopyDDS (FILE *ftgt, FILE *fsrc, DWORD idx, bool idx_is_ofs)
{
	static DWORD maxmip = 0; // max number of mipmap levels
	static BYTE **mipbuf;    // texture buffer for all mipmap levels
	static DWORD *mipsize;   // texture size for all mipmap levels

	static const DWORD MAXMIP = 32;
	DWORD i, j, s, dwMagic, size, tsize;
	DDSURFACEDESC2 ddsd;

	if (!maxmip) {
		for (i = PS; i; i>>= 1) maxmip++; // max mipmap levels
		mipbuf = new BYTE*[maxmip];
		mipsize = new DWORD[maxmip];
		for (i = 0; i < maxmip; i++) mipsize[i] = 0;
	}

	if (idx_is_ofs) {
		fseek (fsrc, idx, SEEK_SET); // jump directly to texture offset
		idx = 0;
	} else {                         // run through textures
		fseek (fsrc, 0, SEEK_SET);
	}

	for (i = 0; i <= idx; i++) {

		if (!fread (&dwMagic, sizeof(DWORD), 1, fsrc)) return 0;
		if (dwMagic != MAKEFOURCC('D','D','S',' ')) return 0;

		fread (&ddsd, sizeof(DDSURFACEDESC2), 1, fsrc);
		if (!(ddsd.dwFlags & DDSD_LINEARSIZE)) return 0;
		size = ddsd.dwLinearSize;
		if (size > mipsize[0]) { // re-allocate buffers
			for (j = 0, s = size; j < maxmip; j++) {
				if (mipsize[j]) delete []mipbuf[j];
				mipsize[j] = s;
				mipbuf[j] = new BYTE[s];
				s >>= 2;
				s = max (s, 8); // Minimum texture size. This appears to be
					            // correct for DXT1, but may differ for other formats!
			}
		}
		fread (mipbuf[0], size, 1, fsrc);
		if (ddsd.dwFlags & DDSD_MIPMAPCOUNT) {
			for (j = 1, s = size; j < ddsd.dwMipMapCount; j++) {
				s >>= 2;
				s = max (s, 8);
				fread (mipbuf[j], s, 1, fsrc);
			}
		}
	}

	fwrite (&dwMagic, sizeof(DWORD), 1, ftgt);          tsize = sizeof(DWORD);
	fwrite (&ddsd, sizeof(DDSURFACEDESC2), 1, ftgt);    tsize += sizeof(DDSURFACEDESC2);
	fwrite (mipbuf[0], size, 1, ftgt);                  tsize += size;
	if (ddsd.dwFlags & DDSD_MIPMAPCOUNT) {
		for (j = 1, s = size; j < ddsd.dwMipMapCount; j++) {
			s >>= 2;
			s = max (s, 8);
			fwrite (mipbuf[j], s, 1, ftgt);             tsize += s;
		}
	}

	return tsize;
}
