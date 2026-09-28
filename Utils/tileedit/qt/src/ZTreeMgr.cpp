#include "ZTreeMgr.h"
#include "zlib.h"
#ifdef __linux__
#include <cstring> // came with windows.h: memcmp/strlen/strcpy
#endif // __linux__

// =======================================================================
// File header for compressed tree files

TreeFileHeader::TreeFileHeader ()
{
	magic[0] = 'T';
	magic[1] = 'X';
	magic[2] = 1;
	magic[3] = 0;
	size = sizeof(TreeFileHeader);
	flags = 0;
	nodeCount = 0;
	dataOfs = size;
	dataLength = 0;
	rootPos1 = rootPos2 = rootPos3 = rootPos4[0] = rootPos4[1] = (DWORD)-1;
}

// -----------------------------------------------------------------------

size_t TreeFileHeader::fwrite(FILE *f)
{
	return ::fwrite(this, sizeof(TreeFileHeader), 1, f);
}

// -----------------------------------------------------------------------

bool TreeFileHeader::fread(FILE *f)
{
	BYTE buf[4];
	DWORD sz, flags;
	if (::fread(buf, 1, 4, f) < 4 || memcmp(buf, magic, 4))
		return false;
	if (::fread(&sz, sizeof(DWORD), 1, f) != 1 || sz != size)
		return false;
	::fread(&flags, sizeof(DWORD), 1, f);
	::fread(&dataOfs, sizeof(DWORD), 1, f);
#ifndef __linux__
	::fread(&dataLength, sizeof(__int64), 1, f);
#else // __linux__
	::fread(&dataLength, sizeof(int64_t), 1, f);
#endif // __linux__
	::fread(&nodeCount, sizeof(DWORD), 1, f);
	::fread(&rootPos1, sizeof(DWORD), 1, f);
	::fread(&rootPos2, sizeof(DWORD), 1, f);
	::fread(&rootPos3, sizeof(DWORD), 1, f);
	::fread(rootPos4, sizeof(DWORD), 2, f);
	return true;
}

// =======================================================================
// Tree table of contents

TreeTOC::TreeTOC()
{
	ntree = 0;
	ntreebuf = 0;
	tree = NULL;
	totlength = 0;
}

// -----------------------------------------------------------------------

TreeTOC::~TreeTOC()
{
	if (ntreebuf)
		delete []tree;
}

// -----------------------------------------------------------------------

size_t TreeTOC::fread(DWORD size, FILE *f)
{
	if (ntreebuf != size) {
		TreeNode *tmp = new TreeNode[size];
		if (ntreebuf) delete []tree;
		tree = tmp;
		ntree = ntreebuf = size;
	}
	return ::fread(tree, sizeof(TreeNode), size, f);
}

// =======================================================================
// ZTreeMgr class: manage a single layer tree for a planet

ZTreeMgr *ZTreeMgr::CreateFromFile(const char *PlanetPath, Layer _layer)
{
	ZTreeMgr *mgr = new ZTreeMgr(PlanetPath, _layer);
	if (!mgr->TOC().size()) {
		delete mgr;
		mgr = 0;
	}
	return mgr;
}

// -----------------------------------------------------------------------

ZTreeMgr::ZTreeMgr(const char *PlanetPath, Layer _layer)
{
	path = new char[strlen(PlanetPath)+1];
	strcpy(path, PlanetPath);
	layer = _layer;
	treef = 0;
	OpenArchive();
}

// -----------------------------------------------------------------------

ZTreeMgr::~ZTreeMgr()
{
	delete []path;
	if (treef) fclose(treef);
}

// -----------------------------------------------------------------------

bool ZTreeMgr::OpenArchive()
{
	const char *name[6] = { "Surf", "Mask", "Elev", "Elev_mod", "Label", "Cloud" };
#ifndef __linux__
	char fname[256];
	sprintf (fname, "%s\\Archive\\%s.tree", path, name[layer]);
#else // __linux__
	char fname[1024]; // was 256: Linux paths aren't capped at MAX_PATH
	sprintf (fname, "%s/Archive/%s.tree", path, name[layer]);
#endif // __linux__
	treef = fopen(fname, "rb");
	if (!treef) return false;

	TreeFileHeader tfh;
	if (!tfh.fread(treef)) {
		fclose(treef);
		treef = 0;
		return false;
	}
	rootPos1 = tfh.rootPos1;
	rootPos2 = tfh.rootPos2;
	rootPos3 = tfh.rootPos3;
	for (int i = 0; i < 2; i++)
		rootPos4[i] = tfh.rootPos4[i];
#ifndef __linux__
	dofs = (__int64)tfh.dataOfs;
#else // __linux__
	dofs = (int64_t)tfh.dataOfs;
#endif // __linux__

	if (!toc.fread(tfh.nodeCount, treef)) {
		fclose(treef);
		treef = 0;
		return false;
	}
	toc.totlength = tfh.dataLength;

	return true;
}

// -----------------------------------------------------------------------

DWORD ZTreeMgr::Idx(int lvl, int ilat, int ilng) const
{
	if (lvl <= 4) {
		return (lvl == 1 ? rootPos1 : lvl == 2 ? rootPos2 : lvl == 3 ? rootPos3 : rootPos4[ilng]);
	} else {
		int plvl = lvl-1;
		int pilat = ilat/2;
		int pilng = ilng/2;
		DWORD pidx = Idx(plvl, pilat, pilng);
		if (pidx == (DWORD)-1)
			return pidx;
		int cidx = ((ilat&1) << 1) + (ilng&1);
		return toc[pidx].child[cidx];
	}
}

// -----------------------------------------------------------------------

DWORD ZTreeMgr::ReadData(DWORD idx, BYTE **outp) const
{
	if (idx == (DWORD)-1) return 0; // sanity check

	DWORD esize = NodeSizeInflated(idx);
	if (!esize) // node doesn't have data, but has descendants with data
		return 0;

#ifndef __linux__
	if (_fseeki64(treef, toc[idx].pos+dofs, SEEK_SET))
#else // __linux__
	if (fseeko(treef, toc[idx].pos+dofs, SEEK_SET))
#endif // __linux__
		return 0;

	DWORD zsize = NodeSizeDeflated(idx);
	BYTE *zbuf = new BYTE[zsize];	
	fread(zbuf, 1, zsize, treef);

	BYTE *ebuf = new BYTE[esize];

	DWORD ndata = Inflate(zbuf, zsize, ebuf, esize);
	delete []zbuf;

	if (!ndata) {
		delete []ebuf;
		ebuf = 0;
	}
	*outp = ebuf;
	return ndata;
}

// -----------------------------------------------------------------------

DWORD ZTreeMgr::Inflate(const BYTE *inp, DWORD ninp, BYTE *outp, DWORD noutp) const
{
#ifndef __linux__
	DWORD ndata = noutp;
#else // __linux__
	uLongf ndata = noutp; // zlib's uLongf is 64-bit on Linux, 32-bit (= DWORD) on Windows
#endif // __linux__
	if (uncompress (outp, &ndata, inp, ninp) != Z_OK)
		return 0;
	return ndata;
}

// -----------------------------------------------------------------------

void ZTreeMgr::ReleaseData(BYTE *data) const
{
	delete []data;
}
