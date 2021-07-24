// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// =========================================================================
// plsplit for Orbiter:
// Split a bitmap file into 512x512 patches and put them into the proper
// directory structure
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
#include <shlobj.h>
#include <wincodec.h>

using namespace std;

typedef union { // Coour triplet in BGR format (since 24-bit BMP files store them in this order)
	BYTE data[3];
	struct { BYTE b, g, r; };
} BGR;

bool operator!= (const BGR &c1, const BGR &c2)
{
	return c1.b != c2.b || c1.g != c2.g || c1.r != c2.r;
}

typedef BYTE Alpha;

int PS = 512; // patch size: size of patch textures
const char *dxtex = ".\\dxtex.exe";

char g_cwd[256];
double g_tol = 0.0;        // tolerance for suppressing opaque/transparent pixels in a tile
int g_nsuppressed = 0;     // number of opacity/transparency suppressed tiles

IWICImagingFactory *g_pIWICFactory;

void ReadBMP_header (char *fname, LONG &mapw, LONG &maph, WORD &bpp);
// Read image width, height, and bit depth from BMP file

BGR *ReadBMP (char *fname, LONG &mapw, LONG &maph, WORD &bpp);
// Read bitmap BMP file into BGR buffer and return dimensions and max texture level

Alpha *ReadBMPAlpha (char *fname, LONG &mapw, LONG &maph, WORD &bpp);
// Read water mask from blue channel of bitmap

BGR *ReadPNG (char *fname, LONG &mapw, LONG &maph, WORD &bpp);
// Read bitmap PNG file into BGR buffer and return dimensions and max texture level

DWORD WriteDDS (BGR *img, Alpha *aimg, LONG imgw, LONG imgh, const char *root,
				const char *layer, int res, int ilng, int ilat, bool mipmap = false, bool binary_alpha = true);

void SetOutputHeader (BITMAPFILEHEADER &bmfh, BITMAPINFOHEADER &bmih, LONG w, LONG h);

void FatalError (char *msg);
void InitProgress (int ntot, int len);
void SetProgress (int p);
void IncProgress ();

bool MakePath (const char *fname);

void SplitBitmap ();
void SplitBitmap_cloud ();

// ==============================================================================

int main (int argc, char *argv[])
{
	if (!_getcwd (g_cwd, 256)) FatalError ("Cannot get working directory");
	strcat (g_cwd, "\\");

    // Create WIC factory for formatted image output
    HRESULT hr = CoCreateInstance (
        CLSID_WICImagingFactory,
        NULL,
        CLSCTX_INPROC_SERVER,
        IID_PPV_ARGS(&g_pIWICFactory)
    );
	if (hr != S_OK)
		g_pIWICFactory = NULL;
		
	char cmd;

	std::cout << "plsplit: Orbiter planetary texture generation tool.\n\n";
	std::cout << "(S) Generate textures for a planetary surface\n";
	std::cout << "(C) Generate cloud textures\n";
	std::cout << "[S|C] >> ";
	std::cin >> cmd;

	switch (toupper(cmd)) {
	case 'S':
		SplitBitmap ();
		break;
	case 'C':
		SplitBitmap_cloud();
		break;
	default:
		FatalError ("Invalid selection");
		break;
	}

	return 0;
}

// ==============================================================================

void ReadBMP_header (char *fname, LONG &mapw, LONG &maph, WORD &bpp)
{
	char cbuf[256], *id;
	FILE *fbmp;
	BITMAPFILEHEADER bmfh;
	BITMAPINFO *bmi;

	strcpy (cbuf, fname);
	//strcat (cbuf, ".bmp");
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

// ==============================================================================

BGR *ReadBMP (char *fname, LONG &mapw, LONG &maph, WORD &bpp)
{
	// Note: The image data are oriented from bottom to top of the map

	char cbuf[256], *id;
	FILE *fbmp;
	BITMAPFILEHEADER bmfh;
	BITMAPINFO *bmi;
	DWORD i, imgsize;
	BGR *img;

	strcpy (cbuf, fname);
	//strcat (cbuf, ".bmp");
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

	img = new BGR[imgsize];
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
		fread (img, sizeof(BGR), imgsize, fbmp);
		break;
	default:
		FatalError ("Unsupported source colour depth");
	}
	fclose(fbmp);
	delete []tmp;
	return img;
}

// ==============================================================================

Alpha *ReadBMPAlpha (char *fname, LONG &mapw, LONG &maph, WORD &bpp)
{
	char cbuf[256], *id;
	FILE *fbmp;
	BITMAPFILEHEADER bmfh;
	BITMAPINFO *bmi;
	DWORD i, imgsize;
	Alpha *aimg;

	strcpy (cbuf, fname);
	//strcat (cbuf, ".bmp");
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
		BGR bgr;
		for (i = 0; i < imgsize; i++) {
			fread (&bgr, sizeof(BGR), 1, fbmp);
			aimg[i] = bgr.b;
		}}
		break;
	default:
		FatalError ("Unsupported source colour depth");
	}
	fclose(fbmp);
	delete []tmp;
	return aimg;
}

// ==============================================================================

BGR *ReadPNG (char *fname, LONG &mapw, LONG &maph, WORD &bpp)
{
	HRESULT hr;
	wchar_t wfname[256];
	mbstowcs (wfname, fname, 256);
	IWICBitmapDecoder *pIDecoder;
    IWICBitmapFrameDecode *pFrame = NULL;
	BGR *img = NULL;

	hr = g_pIWICFactory->CreateDecoderFromFilename (wfname, NULL, GENERIC_READ,
		WICDecodeMetadataCacheOnDemand, &pIDecoder);

    if (SUCCEEDED(hr)) {
       hr = pIDecoder->GetFrame(0, &pFrame);
    }

	if (SUCCEEDED(hr)) {
		UINT mapw, maph;
		pFrame->GetSize (&mapw, &maph);
		UINT imgsize = mapw*maph;
		img = new BGR[imgsize];
		pFrame->CopyPixels (NULL, mapw*3, imgsize*3, (BYTE*)img);
	}

	pIDecoder->Release();

	return img;
}

// ==============================================================================

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

// ==============================================================================

void FatalError (char *msg)
{
	cerr << endl << "pltex ERROR: " << msg << endl;
	cerr << "Press a key to terminate." << endl;
	while (_kbhit()) _getch();
	while (!_kbhit());
	exit (1);
}

// ==============================================================================

static int prog, prog_p, prog_ntot, prog_len;

void InitProgress (int ntot, int len)
{
	prog_ntot = ntot;
	prog_len = len;
	prog = 0;
	prog_p = 0;
	std::cout << '|' << flush;
}

// ==============================================================================

void SetProgress (int p)
{
	int i, len = (p*prog_len)/prog_ntot;
	prog_p = p;
	if (len > prog) {
		for (i = 0; i < len-prog; i++)
			std::cout << '=' << flush;
		prog = len;
	}
	if (p == prog_ntot)
		std::cout << '|' << endl;
}

// ==============================================================================

void IncProgress ()
{
	SetProgress (prog_p+1);
}

// ==============================================================================

bool MakePath (const char *fname)
{
	char cbuf[256];
	size_t i, len = strlen(fname);
	for (i = len; i > 0; i--)
		if (fname[i-1] == '\\') break;
	if (!i) return false;
	if (fname[0] != '\\' && fname[1] != ':') {
		GetCurrentDirectory (256, cbuf);
		len = strlen(cbuf);
		cbuf[len++] = '\\';
	} else len = 0;
	strncpy_s (cbuf+len, 256-len, fname, i);
	int res = SHCreateDirectoryEx (NULL, cbuf, NULL);
	return res == ERROR_SUCCESS;
}

// ==============================================================================

template <typename T>
void ExtractPatch (const T *img, T *patch, LONG py, LONG px, LONG mapw, LONG maph)
{
	LONG x, y;
	LONG xofs = px*PS;
	LONG yofs = py*PS;

	for (y = 0; y < PS; y++)
		for (x = 0; x < PS; x++)
			patch[(PS-y-1)*PS+x] = img[(maph-(yofs+y)-1)*mapw+(x+xofs)];
}

// ==============================================================================
// returns true if patch is not homogeneous

template <typename T>
bool PatchHasFeatures (const T *patch)
{
	T val = patch[0];
	for (int i = 1; i < PS*PS; i++)
		if (patch[i] != val) return true;
	return false;
}

// ==============================================================================
// returns true if patch only contains specular pixels

bool PureSpecular (const Alpha *apatch)
{
	for (int i = 0; i < PS*PS; i++)
		if (apatch[i] < 128) return false;
	return true;
}

// ==============================================================================
// returns true if patch only contains specular pixels

bool PureDiffuse (const Alpha *apatch)
{
	for (int i = 0; i < PS*PS; i++)
		if (apatch[i] > 128) return false;
	return true;
}

// ==============================================================================

DWORD WriteDDS (BGR *img, Alpha *aimg, LONG imgw, LONG imgh, const char *root, const char *layer, int lvl, int ilng, int ilat,
				bool mipmap, bool binary_alpha)
{
	const char *bmpname  = "tmp.bmp";
	const char *abmpname = "tmp_a.bmp";
	char ddsname[256];
	sprintf (ddsname, "%s\\%s\\%02d\\%06d\\%06d.dds", root, layer, lvl, ilat, ilng);
	MakePath (ddsname);
	cout << "Writing  patch  " << ddsname << endl;

	static BGR *bgrdummy = 0;
	if (!img) {
		if (!bgrdummy) {
			bgrdummy = new BGR[PS*PS];
			memset (bgrdummy, 0, sizeof(BGR)*PS*PS);
		}
		img = bgrdummy;
	}

	FILE *bmpf;
	BITMAPFILEHEADER bmfh;
	BITMAPINFOHEADER bmih;
	int i, ddssize = 0;
	intptr_t res;
	bool bopaque = false, btransparent = false;
	int nopaque, ntransparent;

	SetOutputHeader (bmfh, bmih, imgw, imgh);
	bmpf = fopen (bmpname, "wb");
	if (!bmpf) FatalError ("Could not open temporary bitmap file.");
	fwrite (&bmfh, sizeof(BITMAPFILEHEADER), 1, bmpf);
	fwrite (&bmih, sizeof(BITMAPINFOHEADER), 1, bmpf);
	fwrite (img, 3, imgw*imgh, bmpf);
	fclose (bmpf);

	if (aimg) { // add an alpha layer to the patch
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
		for (i = 0; i < imgw*imgh; i++) {
			BGR bgr;
			bgr.r = bgr.g = bgr.b = aimg[i];
			fwrite (&bgr, 3, 1, bmpf);
		}
		fclose (bmpf);

		res = _spawnl (_P_WAIT, dxtex, dxtex, bmpname, "-a", abmpname, mipmap ? "-m" : "", binary_alpha ? "DXT1" : "DXT5", ddsname, NULL);
	} else {
		res = _spawnl (_P_WAIT, dxtex, dxtex, bmpname, mipmap ? "-m" : "", "DXT1", ddsname, NULL);
	}
	if (res != 0) FatalError ("Executing dxtex failed.");

	remove (bmpname);
	if (aimg) remove (abmpname);
	return (DWORD)ddssize;
}

// ==============================================================================

void SplitBitmap ()
{
	int level;

	cout << "Generate tiles for resolution level: ";
	cin >> level;

	if (level < 3) PS /= 2;
	if (level < 2) PS /= 2;

	char fname[256], aname[256], lname[256], root[256], c;
	LONG mapw, maph;
	LONG px, py, nx, ny;
	WORD bpp, abpp, lbpp;
	int lvl, nlng, nlat, ilng0, ilat0;
	int nwritten = 0, nskipped = 0;
	double lng0, lat0;
	BGR *img = 0;
	BGR *patch = new BGR[PS*PS];
	BGR *limg = 0;
	BGR *lpatch = new BGR[PS*PS];
	Alpha *aimg = 0;
	Alpha *apatch = new Alpha[PS*PS];
	bool has_wmask;
	bool skip_specular;
	bool has_lights;
	bool output_mask;

	memset (lpatch, 0, PS*PS*sizeof(BGR));
	memset (apatch, 0, PS*PS*sizeof(Alpha));

	cout << "Enter the file name for the bitmap representing the planetary surface\n";
	cout << "area (must be in 8-bit or 24-bit BMP format). The bitmap must contain a\n";
	cout << "surface patch in cylindrical projection, with longitude linear along the\n";
	cout << "horizontal axis, and latitude linear along the vertical axis.\n\n";
	cout << ">> Surface map file name (.bmp): ";
	cin >> fname;
	//if (!_stricmp (fname+(strlen(fname)-4), ".bmp"))
	//	fname[strlen(fname)-4] = '\0';
	cout << endl;

	ReadBMP_header (fname, mapw, maph, bpp);

	cout << "Longitude of top left corner of the bitmap [deg, -180...180]: ";
	cin >> lng0;

	cout << "Latitude of top left corner of the bitmap [deg, -90...90]: ";
	cin >> lat0;

	cout << "\nDoes the surface contain any areas that should show specular light\n";
	cout << "reflection (e.g. oceans, rivers or icecaps)?\n\n";
	cout << "(N) No, only diffusely reflecting surface\n";
	cout << "(Y) Yes, the whole or part of the surface is reflective\n";
	cout << "\n>> [N|Y]: ";
	cin >> c;
	cout << endl;
	has_wmask = (toupper(c) == 'Y');

	if (has_wmask) {
		cout << "\nTo prepare a surface with reflecting areas, a bitmap containing a land-water\n";
		cout << "mask is required. This bitmap must be of the same size and cover the same\n";
		cout << "area as the surface bitmap. It must be in 8-bit or 24-bit BMP format, and\n";
		cout << "contain only 2 colours: white for any specular reflection areas (water),\n";
		cout << "and black for diffuse reflection areas (land).\n\n";
		cout << ">> Mask map file name (.bmp): ";
		cin >> aname;
		//if (!_stricmp (aname+(strlen(aname)-4), ".bmp"))
		//	aname[strlen(aname)-4] = '\0';
		cout << endl;
		LONG amapw, amaph;
		ReadBMP_header (aname, amapw, amaph, abpp);
		if (amapw != mapw || amaph != maph)
			FatalError ("Bitmap sizes do not match");

		cout << "\nSkip purely specular tiles? This can be useful at high resolution levels,\n";
		cout << "if specular surfaces represent water bodies without high spatial frequency\n";
		cout << "content, to avoid generation of redundant tiles.\n";
		cout << "\n>> [N|Y]: ";
		cin >> c;
		cout << endl;
		skip_specular = (toupper(c) == 'Y');
	}

	cout << "\nCity lights: Does the surface contain areas that emit light at night\n";
	cout << "(illuminated cities etc.)?\n\n";
	cout << "(N) No\n";
	cout << "(Y) Yes\n";
	cout << "\n>> [N|Y]: ";
	cin >> c;
	cout << endl;
	has_lights = (toupper(c) == 'Y');
	output_mask = has_wmask || has_lights;

	if (has_lights) {
		cout << "\nTo prepare city-light textures, a bitmap containing the distribution of\n";
		cout << "emissive light during nighttime is required. This bitmap must be of the\n";
		cout << "same size and cover the same area as the surface bitmap. It must be in\n";
		cout << "8-bit or 24-bit BMP format. It should be black in non-lit areas, and bright\n";
		cout << "(but not necessarily white) in lit areas.\n\n";
		cout << ">> City light map file name: (.bmp): ";
		cin >> lname;
		//if (!_stricmp (lname+(strlen(lname)-4), ".bmp"))
		//	lname[strlen(lname)-4] = '\0';
		cout << endl;
		LONG lmapw, lmaph;
		ReadBMP_header (lname, lmapw, lmaph, lbpp);
		if (lmapw != mapw || lmaph != maph)
			FatalError ("Bitmap sizes do not match");
	}

	cout << "Output root path: ";
	cin >> root;

	cout << "Image dimensions: " << mapw << "(W) x " << maph << "(H) x " << bpp << "(BPP)" << endl;
	nx = mapw/PS;
	ny = maph/PS;
	cout << "Patch grid: " << nx << " x " << ny << " patches of " << PS << " x " << PS << " pixels" << endl;
	if (mapw != nx*PS || maph != ny*PS) {
		cerr << "Warning: Bitmap dimensions are not a multiple of patch size." << endl;
	}

	cout << "Loading bitmap ..." << endl;
	img = ReadBMP (fname, mapw, maph, bpp);
	if (has_wmask)
		aimg = ReadBMPAlpha (aname, mapw, maph, abpp);
	if (has_lights)
		limg = ReadBMP (lname, mapw, maph, lbpp);

	nlng = 2;
	nlat = 1;
	for (lvl = 4; lvl < level; lvl++) {
		nlng *= 2;
		nlat *= 2;
	}

	double dlng = 360.0/nlng;
	ilng0 = (int)((lng0+180)/dlng+0.5);
	ilat0 = (int)((90-lat0)/dlng+0.5);

	for (py = 0; py < ny; py++) {
		for (px = 0; px < nx; px++) {
			ExtractPatch<BGR> (img, patch, py, px, mapw, maph);
			bool genpatch = PatchHasFeatures<BGR> (patch);
			bool skipspec = false;
			if (output_mask) {
				if (has_wmask) {
					ExtractPatch<Alpha> (aimg, apatch, py, px, mapw, maph);
					if (skip_specular) skipspec = PureSpecular (apatch);
					genpatch = !skipspec && (genpatch || !PureDiffuse (apatch));
					//genpatch = !skipspec && (genpatch || PatchHasFeatures<Alpha> (apatch));
				}
				if (has_lights && !skipspec) {
					ExtractPatch<BGR> (limg, lpatch, py, px, mapw, maph);
					genpatch = genpatch || PatchHasFeatures<BGR> (lpatch);
				}
				if (genpatch && ((has_lights && PatchHasFeatures<BGR> (lpatch)) || (has_wmask && !PureDiffuse (apatch)))) {
					if (!has_lights) memset (lpatch, 0, PS*PS*sizeof(BGR));
					if (!has_wmask) memset (apatch, 0, PS*PS*sizeof(Alpha));
					WriteDDS (lpatch, apatch, PS, PS, root, "Mask", level, ilng0+px, ilat0+py);
					nwritten++;
				} else {
					char cbuf[256];
					sprintf (cbuf, "%s\\%s\\%02d\\%06d\\%06d.dds", root, "Mask", level, ilat0+py, ilng0+px);
					cout << "Skipping patch  " << cbuf << endl;
					nskipped++;
				}
			}
			if (genpatch) {
				WriteDDS (patch, 0, PS, PS, root, "Surf", level, ilng0+px, ilat0+py);
				nwritten++;
			} else {
				char cbuf[256];
				sprintf (cbuf, "%s\\%s\\%02d\\%06d\\%06d.dds", root, "Surf", level, ilat0+py, ilng0+px);
				cout << "Skipping patch  " << cbuf << endl;
				nskipped++;
			}
		}
	}
	cout << "Wrote " << nwritten << " patches, skipped " << nskipped << endl;

	delete []img;
	if (aimg) delete []aimg;
	delete []patch;
}

// ==============================================================================

void SplitBitmap_cloud ()
{
	int level, lvl, r, g, b, nlng, nlat, ilng0, ilat0;
	LONG mapw=0, maph=0, amapw, amaph, px, py, nx, ny;
	WORD bpp=0, abpp;
	double lng0, lat0;
	char cmd, fname[256], aname[256], root[256];
	bool use_homog_col=false, has_wmask=false;

	cout << "Generate tiles for resolution level: ";
	cin >> level;

	if (level < 3) PS /= 2;
	if (level < 2) PS /= 2;

	BGR *img = 0;
	BGR *patch = new BGR[PS*PS];
	Alpha *aimg = 0;
	Alpha *apatch = new Alpha[PS*PS];

	memset (apatch, 0, PS*PS*sizeof(Alpha));

	cout << "Cloud colour information:\n";
	cout << "(H) Use homogeneous cloud colour\n";
	cout << "(T) Load texture for cloud colour\n";
	cout << "[H|T] >> ";
    cin >> cmd;
	switch (toupper(cmd)) {
	case 'H':
		cout << "Specify RGB components for cloud colour (3 integer values in the range\n";
		cout << "0..255, separated by space).\n";
		cout << "[R G B] >> ";
		cin >> r >> g >> b;
		use_homog_col = true;
		break;
	case 'T':
		cout << "Enter the file name for the bitmap representing the cloud colour information\n";
		cout << "(must be in 8-bit or 24-bit BMP format). The bitmap must contain a cloud\n";
		cout << "patch in cylindrical projection, with longitude linear along the horizontal\n";
		cout << "axis, and latitude linear along the vertical axis.\n";
		cout << "[Cloud colour map file name] >> ";
		cin >> fname;
		ReadBMP_header (fname, mapw, maph, bpp);
		use_homog_col = false;
		break;
	default:
		FatalError("Invalid selection");
		break;
	}

	cout << "Cloud opacity information:\n";
	cout << "(S) Use solid cloud layer (no transparency)\n";
	cout << "(T) Load texture for cloud opacity\n";
	cout << "[S|T] >> ";
	cin >> cmd;
	switch (toupper(cmd)) {
	case 'S':
		has_wmask = false;
		break;
	case 'T':
		cout << "Enter the file name for the bitmap representing the cloud opacity information\n";
		cout << "(must be in 8-bit or 24-bit BMP format). The bitmap must contain a cloud\n";
		cout << "patch in cylindrical projection, with longitude linear along the horizontal\n";
		cout << "axis, and latitude linear along the vertical axis.\n";
		cout << "[Cloud opacity map file name] >> ";
		cin >> aname;
		ReadBMP_header (aname, amapw, amaph, abpp);
		if (use_homog_col) {
			mapw = amapw;
			maph = amaph;
		} else {
			if (mapw != amapw || maph != amaph)
				FatalError ("Bitmap sizes do not match");
		}
		has_wmask = true;
		break;
	default:
		FatalError("Invalid selection");
		break;
	}

	cout << "Longitude of top left corner of the bitmap [deg, -180...180]: ";
	cin >> lng0;

	cout << "Latitude of top left corner of the bitmap [deg, -90...90]: ";
	cin >> lat0;

	if (!mapw || !maph) {
		cout << "Bitmap size (width and height in pixels, values must be multiples of " << PS << ")\n";
		cout << "[W H] >> ";
		cin >> mapw >> maph;
	}

	cout << "Output root path: ";
	cin >> root;

	cout << "Image dimensions: " << mapw << "(W) x " << maph << "(H)" << endl;
	nx = mapw/PS;
	ny = maph/PS;
	cout << "Patch grid: " << nx << " x " << ny << " patches of " << PS << " x " << PS << " pixels" << endl;
	if (mapw != nx*PS || maph != ny*PS) {
		cerr << "Warning: Bitmap dimensions are not a multiple of patch size." << endl;
	}

	cout << "Loading bitmap ..." << endl;
	if (use_homog_col) {
		img = new BGR[mapw*maph];
		for (LONG i = 0; i < mapw*maph; i++) {
			img[i].r = (BYTE)r;
			img[i].g = (BYTE)g;
			img[i].b = (BYTE)b;
		}
	} else {
		img = ReadBMP (fname, mapw, maph, bpp);
	}

	if (!has_wmask) {
		aimg = new Alpha[mapw*maph];
		for (LONG i = 0; i < mapw*maph; i++)
			aimg[i] = 255;
	} else {
		aimg = ReadBMPAlpha (aname, mapw, maph, abpp);
	}

	nlng = 2;
	nlat = 1;
	for (lvl = 4; lvl < level; lvl++) {
		nlng *= 2;
		nlat *= 2;
	}

	double dlng = 360.0/nlng;
	ilng0 = (int)((lng0+180)/dlng+0.5);
	ilat0 = (int)((90-lat0)/dlng+0.5);

	for (py = 0; py < ny; py++) {
		for (px = 0; px < nx; px++) {
			ExtractPatch<BGR> (img, patch, py, px, mapw, maph);
			if (has_wmask) {
				ExtractPatch<Alpha> (aimg, apatch, py, px, mapw, maph);
				WriteDDS (patch, apatch, PS, PS, root, "Cloud", level, ilng0+px, ilat0+py, false, false);
			} else
				WriteDDS (patch, 0, PS, PS, root, "Cloud", level, ilng0+px, ilat0+py);
		}
	}
	cout << "Wrote " << nx*ny << " patches" << endl;

	delete []img;
	if (aimg) delete []aimg;
	delete []patch;
	delete []apatch;
}