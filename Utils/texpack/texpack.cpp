// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include <iostream>
#include <string>
#include <windows.h>
#include <direct.h>
#include <Shlwapi.h>
#include <zlib.h>

#define TREE_DEFLATE 1

//==============================================================================
// local prototypes

// check if the file for a particular tile exists in the directory tree
bool exist_file(const char *root, const char *layer, const char *ext, int lvl, int ilng, int ilat);

// deflate a data block
// this is assumed to work in a single step. output buffer "outp" of size "noutp" must be large
// enough to hold the entire deflated data block
// Returns deflated block size
DWORD deflate_node_data(BYTE *inp, DWORD ninp, BYTE *outp, DWORD noutp);

// inflate data block
DWORD inflate_node_data(BYTE *inp, DWORD ninp, BYTE *outp, DWORD noutp);

//==============================================================================
// A single MemTree node

struct MemTreeNode {
	MemTreeNode (int _lvl, int _ilat, int _ilng): lvl(_lvl), ilat(_ilat), ilng(_ilng)
	{ for (int i = 0; i < 4; i++) child[i] = 0; }

	int lvl;
	int ilat, ilng;
	MemTreeNode *child[4];
};

//==============================================================================
// Represents the tile tree in memory (including missing links)

class MemTree {
public:
	MemTree (const char *rootpath, const char *layer);
	~MemTree ();
	void AddLevel(int lvl);
	void AddLevels(int minlvl, int maxlvl);
	int NodeCount() const;
	const MemTreeNode *FindNode(int lvl, int ilat, int ilng) const;

protected:
	MemTreeNode *InsertNode(int lvl, int ilat, int ilng);
	void SubtreeCount(MemTreeNode *node, int &count) const;
	MemTreeNode *FindNode(int lvl, int ilat, int ilng);
	void DeleteSubtree (MemTreeNode *node);

private:
	MemTreeNode *root1;
	MemTreeNode *root2;
	MemTreeNode *root3;
	MemTreeNode *root4[2];
	char path[256];
	char ext[16];
};

// -----------------------------------------------------------------------------

MemTree::MemTree(const char *rootpath, const char *layer)
{
	root1 = root2 = root3 = root4[0] = root4[1] = 0;
	sprintf(path, "%s\\%s", rootpath, layer);
	if (!stricmp(layer, "Surf"))
		strcpy(ext, "dds");
	else if (!stricmp(layer, "Mask"))
		strcpy(ext, "dds");
	else if (!stricmp(layer, "Cloud"))
		strcpy(ext, "dds");
	else if (!stricmp(layer, "Elev"))
		strcpy(ext, "elv");
	else if (!stricmp(layer, "Elev_mod"))
		strcpy(ext, "elv");
	else if (!stricmp(layer, "Label"))
		strcpy(ext, "lab");
	else ext[0] = '\0';
}

// -----------------------------------------------------------------------------

MemTree::~MemTree ()
{
	DeleteSubtree(root1);
	DeleteSubtree(root2);
	DeleteSubtree(root3);
	for (int i = 0; i < 2; i++)
		DeleteSubtree(root4[i]);
}

// -----------------------------------------------------------------------------

void MemTree::DeleteSubtree (MemTreeNode *node)
{
	if (!node) return;

	for (int i = 0; i < 4; i++)
		DeleteSubtree (node->child[i]);
	delete node;
}

// -----------------------------------------------------------------------------

void MemTree::AddLevels(int minlvl, int maxlvl)
{
	for (int lvl = minlvl; lvl <= maxlvl; lvl++)
		AddLevel(lvl);
}

// -----------------------------------------------------------------------------

void MemTree::AddLevel(int lvl)
{
	char lvlpath[256];
	sprintf(lvlpath, "%s\\%02d", path, lvl);
	if (PathFileExists(lvlpath)) {
		WIN32_FIND_DATA fdata, fdata2;
		strcat(lvlpath, "\\*");
		HANDLE h = FindFirstFile(lvlpath, &fdata);
		BOOL ok = (h != INVALID_HANDLE_VALUE);
		while (ok) {
			bool match = (strlen(fdata.cFileName) == 6);
			for (int i = 0; i < 6; i++)
				match = match && fdata.cFileName[i] >= '0' && fdata.cFileName[i] <= '9';
			if (match) {
				int ilat, ilng;
				sscanf(fdata.cFileName, "%d", &ilat);
				char latpath[256];
				strcpy(latpath, lvlpath); strcpy(latpath+strlen(latpath)-1, fdata.cFileName);
				strcat(latpath, "\\*."); strcat(latpath, ext);
				HANDLE h2 = FindFirstFile(latpath, &fdata2);
				BOOL ok2 = (h2 != INVALID_HANDLE_VALUE);
				while (ok2) {
					sscanf(fdata2.cFileName, "%d", &ilng);
					InsertNode(lvl, ilat, ilng);
					ok2 = FindNextFile(h2, &fdata2);
				}
				FindClose(h2);
			}
			ok = FindNextFile(h, &fdata);
		}
		FindClose (h);
	}
}

// -----------------------------------------------------------------------------

int MemTree::NodeCount() const
{
	int count = 0;
	if (root1) count++;
	if (root2) count++;
	if (root3) count++;
	for (int i = 0; i < 2; i++)
		SubtreeCount(root4[i], count);
	return count;
}

// -----------------------------------------------------------------------------

void MemTree::SubtreeCount(MemTreeNode *node, int &count) const
{
	if (node) {
		count++;
		for (int i = 0; i < 4; i++) SubtreeCount(node->child[i], count);
	}
}

// -----------------------------------------------------------------------------

MemTreeNode *MemTree::InsertNode(int lvl, int ilat, int ilng)
{
	if (lvl == 1) {
		return (root1 = new MemTreeNode(lvl, ilat, ilng));
	} else if (lvl == 2) {
		return (root2 = new MemTreeNode(lvl, ilat, ilng));
	} else if (lvl == 3) {
		return (root3 = new MemTreeNode(lvl, ilat, ilng));
	} else if (lvl == 4) {
		return (root4[ilng] = new MemTreeNode(lvl, ilat, ilng));
	} else {
		MemTreeNode *parent = FindNode(lvl-1, ilat/2, ilng/2);
		if (!parent) parent = InsertNode(lvl-1, ilat/2, ilng/2);
		return (parent->child[((ilat&1) << 1) + (ilng&1)] = new MemTreeNode(lvl, ilat, ilng));
	}
}

// -----------------------------------------------------------------------------

const MemTreeNode *MemTree::FindNode(int lvl, int ilat, int ilng) const
{
	if (lvl == 1) {
		return root1;
	} else if (lvl == 2) {
		return root2;
	} else if (lvl == 3) {
		return root3;
	} else if (lvl == 4) {
		return root4[ilng];
	} else {
		const MemTreeNode *parent = FindNode(lvl-1, ilat/2, ilng/2);
		if (!parent) return 0;
		return parent->child[((ilat&1) << 1) + (ilng&1)];
	}
}

// -----------------------------------------------------------------------------

MemTreeNode *MemTree::FindNode(int lvl, int ilat, int ilng)
{
	if (lvl == 1) {
		return root1;
	} else if (lvl == 2) {
		return root2;
	} else if (lvl == 3) {
		return root3;
	} else if (lvl == 4) {
		return root4[ilng];
	} else {
		MemTreeNode *parent = FindNode(lvl-1, ilat/2, ilng/2);
		if (!parent) return 0;
		return parent->child[((ilat&1) << 1) + (ilng&1)];
	}
}


//==============================================================================
// Table of contents entry for a tree node

struct TOCEntry {
	__int64 pos;     // file position of compressed data block (from end of TOC)
	DWORD size;      // uncompressed data size
	DWORD child[4];  // array positions of the children ((DWORD)-1=no child)

	TOCEntry() {
		pos = 0;
		size = 0;
		for (int i = 0; i < 4; i++) child[i] = -1;
	}
};

//==============================================================================
// Tree file table of contents

class TreeTOC {
public:
	TreeTOC(const char *_root, const char *_layer, const MemTree *tree); // build the TOC from a tree
	TreeTOC(const char *_root, const char *_layer);
	~TreeTOC();
	TOCEntry &operator[](int idx);
	DWORD length() const { return header.ntoc; }
	__int64 DataSize() const { return header.totlength; }
	size_t fwrite(FILE *f);
	size_t fread(FILE *f);
	void WriteData(FILE *f);
	void ExtractData(FILE *f, int maxlevel);

protected:
	int AddSubtree(const MemTreeNode *node);
	void WriteSubtreeData(const MemTreeNode *node, FILE *f);
	void ExtractSubtreeData (DWORD idx, int lvl, int ilat, int ilng, FILE *f, int maxlevel);

private:
	struct Header {     // TOC file header
		BYTE magic[4];      // file ID and version
		DWORD size;         // header size [BYTE]
		DWORD flags;		// bit flags
		DWORD dataOfs;      // file offset of start of data block (header + TOC)
		__int64 totlength;  // total deflated data size
		DWORD ntoc;         // number of tree nodes
		DWORD rootPos1;     // array index of level 1 tilWriteSubtreeDatae ((DWORD)-1 for not present)
		DWORD rootPos2;     // array index of level 2 tile ((DWORD)-1 for not present)
		DWORD rootPos3;     // array index of level 3 tile ((DWORD)-1 for not present)
		DWORD rootPos4[2];  // array indices of level 4 tiles (quadtree roots; (DWORD)-1 for not present)
	} header;

	TOCEntry *toc;      // array of tree nodes

	char ext[16];       // file extension for this layer
	bool deflateData;   // compress data?
	char *root, *layer;
	const MemTree *mtree;
};

// -----------------------------------------------------------------------------

TreeTOC::TreeTOC(const char *_root, const char *_layer, const MemTree *tree): mtree(tree)
{
	deflateData = true;

	root = new char[strlen(_root)+1]; strcpy(root, _root);
	layer = new char[strlen(_layer)+1]; strcpy(layer, _layer);

	if (!stricmp(layer, "Surf"))
		strcpy(ext, "dds");
	else if (!stricmp(layer, "Mask"))
		strcpy(ext, "dds");
	else if (!stricmp(layer, "Cloud"))
		strcpy(ext, "dds");
	else if (!stricmp(layer, "Elev"))
		strcpy(ext, "elv");
	else if (!stricmp(layer, "Elev_mod"))
		strcpy(ext, "elv");
	else if (!stricmp(layer, "Label"))
		strcpy(ext, "lab");
	else ext[0] = '\0';

	header.magic[0] = 'T';
	header.magic[1] = 'X';
	header.magic[2] = 1;
	header.magic[3] = 0;
	header.size = sizeof(Header);
	header.flags = 0;
	if (deflateData) header.flags |= TREE_DEFLATE;
	header.ntoc = 0;
	header.totlength = 0;

	toc = new TOCEntry[tree->NodeCount()];
	header.rootPos1 = AddSubtree(tree->FindNode(1, 0, 0));
	header.rootPos2 = AddSubtree(tree->FindNode(2, 0, 0));
	header.rootPos3 = AddSubtree(tree->FindNode(3, 0, 0));
	for (int i = 0; i < 2; i++)
		header.rootPos4[i] = AddSubtree(tree->FindNode(4, 0, i));

	header.dataOfs = header.size + header.ntoc*sizeof(TOCEntry);
}

// -----------------------------------------------------------------------------

TreeTOC::TreeTOC(const char *_root, const char *_layer): mtree(0)
{
	deflateData = true;

	root = new char[strlen(_root)+1]; strcpy(root, _root);
	layer = new char[strlen(_layer)+1]; strcpy(layer, _layer);

	if (!stricmp(layer, "Surf"))
		strcpy(ext, "dds");
	else if (!stricmp(layer, "Mask"))
		strcpy(ext, "dds");
	else if (!stricmp(layer, "Cloud"))
		strcpy(ext, "dds");
	else if (!stricmp(layer, "Elev"))
		strcpy(ext, "elv");
	else if (!stricmp(layer, "Elev_mod"))
		strcpy(ext, "elv");
	else if (!stricmp(layer, "Label"))
		strcpy(ext, "lab");
	else ext[0] = '\0';

	header.magic[0] = 'T';
	header.magic[1] = 'X';
	header.magic[2] = 1;
	header.magic[3] = 0;
	header.size = sizeof(Header);
	header.flags = 0;
	if (deflateData) header.flags |= TREE_DEFLATE;
	header.ntoc = 0;
	header.totlength = 0;

	toc = 0;
	header.rootPos1 = 0;
	header.rootPos2 = 0;
	header.rootPos3 = 0;
	for (int i = 0; i < 2; i++)
		header.rootPos4[i] = 0;
}

// -----------------------------------------------------------------------------

TreeTOC::~TreeTOC()
{
	delete []toc;
	delete []root;
	delete []layer;
}

// -----------------------------------------------------------------------------

int TreeTOC::AddSubtree(const MemTreeNode *node)
{
	static DWORD nbuf = 32768;
	static BYTE *buf = new BYTE[nbuf];   // uncompressed data buffer

	static DWORD nzbuf = 1024000;
	static BYTE *zbuf = new BYTE[nzbuf]; // compressed data buffer

	if (node) {
		int lvl = node->lvl;
		int ilat = node->ilat;
		int ilng = node->ilng;
		int idx = header.ntoc;
		if (exist_file(root, layer, ext, lvl, ilat, ilng)) {
			char path[256];
			LARGE_INTEGER sz;
			DWORD ndata;
			sprintf(path, "%s\\%s\\%02d\\%06d\\%06d.%s", root, layer, lvl, ilat, ilng, ext);
			HANDLE hFile = CreateFile(path, GENERIC_READ, 0, 0, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
			GetFileSizeEx(hFile, &sz);
			if (sz.LowPart > nbuf) { // grow data buffer
				BYTE *tmp = new BYTE[nbuf = sz.LowPart];
				delete[]buf;
				buf = tmp;
			}
			DWORD nread;
			ReadFile(hFile, buf, sz.LowPart, &nread, NULL);
			CloseHandle(hFile);
			if (nread < sz.LowPart) {
				std::cerr << "Unexpected end of file" << std::endl;
				exit(1);
			}
			if (deflateData) {
				ndata = deflate_node_data(buf, sz.LowPart, zbuf, nzbuf);
			} else {
				ndata = sz.LowPart;
			}
			std::cout << "adding node " << path << std::endl;
			toc[idx].size = sz.LowPart;
			toc[idx].pos = header.totlength;
			header.totlength += ndata;
		} else {
			toc[idx].size = 0;
			toc[idx].pos = header.totlength;
		}
		header.ntoc++;

		if (lvl >= 4) {
			for (int i = 0; i < 4; i++)
				toc[idx].child[i] = AddSubtree(node->child[i]);
		}
		return idx;
	}
	return -1;
}

// -----------------------------------------------------------------------------

TOCEntry &TreeTOC::operator[](int idx)
{
	if (idx >= 0 && idx < header.ntoc)
		return toc[idx];
	else
		exit(1);
}

// -----------------------------------------------------------------------------

size_t TreeTOC::fwrite(FILE *f)
{
	size_t n = 0;
	n += ::fwrite(&header, sizeof(Header), 1, f);
	n += ::fwrite(toc, sizeof(TOCEntry), header.ntoc, f);
	return n;
}

// -----------------------------------------------------------------------------

size_t TreeTOC::fread(FILE *f)
{
	size_t n = ::fread(&header, sizeof(Header), 1, f);
	if (n) {
		if (toc) delete []toc;
		toc = new TOCEntry[header.ntoc];
		n += ::fread(toc, sizeof(TOCEntry), header.ntoc, f);
	}
	return n;
}

// -----------------------------------------------------------------------------

void TreeTOC::WriteData(FILE *f)
{
	WriteSubtreeData(mtree->FindNode(1, 0, 0), f);
	WriteSubtreeData(mtree->FindNode(2, 0, 0), f);
	WriteSubtreeData(mtree->FindNode(3, 0, 0), f);
	for (int i = 0; i < 2; i++)
		WriteSubtreeData(mtree->FindNode(4, 0, i), f);
}

// -----------------------------------------------------------------------------

void TreeTOC::WriteSubtreeData(const MemTreeNode *node, FILE *f)
{
	static DWORD nbuf = 32768;
	static BYTE *buf = new BYTE[nbuf];   // uncompressed data buffer

	static DWORD nzbuf = 1024000;
	static BYTE *zbuf = new BYTE[nzbuf]; // compressed data buffer

	if (node) {
		int lvl = node->lvl;
		int ilat = node->ilat;
		int ilng = node->ilng;
		if (exist_file(root, layer, ext, lvl, ilat, ilng)) {
			char path[256];
			LARGE_INTEGER sz;
			DWORD ndata;
			sprintf(path, "%s\\%s\\%02d\\%06d\\%06d.%s", root, layer, lvl, ilat, ilng, ext);
			HANDLE hFile = CreateFile(path, GENERIC_READ, 0, 0, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
			GetFileSizeEx(hFile, &sz);
			if (sz.LowPart > nbuf) { // grow data buffer
				BYTE *tmp = new BYTE[nbuf = sz.LowPart];
				delete[]buf;
				buf = tmp;
			}
			DWORD nread;
			ReadFile(hFile, buf, sz.LowPart, &nread, NULL);
			CloseHandle(hFile);
			if (nread < sz.LowPart) {
				std::cerr << "Unexpected end of file" << std::endl;
				exit(1);
			}
			if (deflateData) {
				ndata = deflate_node_data(buf, sz.LowPart, zbuf, nzbuf);
				std::cout << "deflating " << path << " [" << (ndata * 100) / sz.LowPart << "%]" << std::endl;
				::fwrite(zbuf, 1, ndata, f);
			} else {
				ndata = sz.LowPart;
				std::cout << "copying " << path << std::endl;
				::fwrite(buf, 1, ndata, f);
			}
		}

		if (lvl >= 4) {
			for (int i = 0; i < 4; i++)
				WriteSubtreeData(node->child[i], f);
		}
	}
}

// -----------------------------------------------------------------------------

void TreeTOC::ExtractData(FILE *f, int maxlevel)
{
	ExtractSubtreeData(header.rootPos1, 1, 0, 0, f, maxlevel);
	ExtractSubtreeData(header.rootPos2, 2, 0, 0, f, maxlevel);
	ExtractSubtreeData(header.rootPos3, 3, 0, 0, f, maxlevel);
	for (int i = 0; i < 2; i++)
		ExtractSubtreeData(header.rootPos4[i], 4, 0, i, f, maxlevel);
}

// -----------------------------------------------------------------------------

void TreeTOC::ExtractSubtreeData (DWORD idx, int lvl, int ilat, int ilng, FILE *f, int maxlevel)
{
	if (lvl > maxlevel) return;

	if (idx >= header.ntoc) return; // sanity check
	TOCEntry *entry = toc+idx;

	DWORD esize = entry->size;
	if (!esize) return; // node contains no data

	DWORD zsize = (DWORD)((idx < header.ntoc-1 ? toc[idx+1].pos : header.totlength) - entry->pos);
	BYTE *zbuf = new BYTE[zsize];

	_fseeki64(f, (__int64)header.dataOfs + entry->pos, SEEK_SET);
	int nread = ::fread(zbuf, 1, zsize, f);

	BYTE *ebuf = new BYTE[esize];
	inflate_node_data(zbuf, zsize, ebuf, esize);

	char fname[256];
	sprintf (fname, "%s\\%s", root, layer);
	_mkdir(fname);
	sprintf (fname+strlen(fname), "\\%02d", lvl);
	_mkdir(fname);
	sprintf (fname+strlen(fname), "\\%06d", ilat);
	_mkdir(fname);
	sprintf (fname+strlen(fname), "\\%06d.%s", ilng, ext);
	std::cout << "inflating " << fname << std::endl;
	FILE *fout = fopen(fname, "wb");
	::fwrite(ebuf, esize, 1, fout);
	fclose(fout);

	if (lvl >= 4) { // recursion
		for (int ch = 0; ch < 4; ch++) {
			if (entry->child[ch])
				ExtractSubtreeData (entry->child[ch], lvl+1, ilat*2+ch/2, ilng*2+(ch%2), f, maxlevel);
		}
	}

	delete []zbuf;
	delete []ebuf;
}

//==============================================================================

int maxlevel = 0;
enum OP_MODE {
	OP_ARCHIVE, OP_EXTRACT
} mode = OP_ARCHIVE;

int main(int narg, char *arg[])
{
	if (narg < 3) {
		std::cerr << "\ntexpack: Orbiter texture tree packing tool" << std::endl;
		std::cerr << "  Packs the files of a planet texture layer directory tree" << std::endl;
		std::cerr << "  into a single compressed archive file." << std::endl;
		std::cerr << "\nUsage: texpack <Planet-tree-root> <Layer> [<Flags>]" << std::endl;
		std::cerr << "\n<Planet-tree-root>:" << std::endl;
		std::cerr << "  Path to planet textures, e.g." << std::endl;
		std::cerr << "  c:\\Orbiter\\Textures\\Earth" << std::endl;
		std::cerr << "\n<Layer>:" << std::endl;
		std::cerr << "  Surf     pack surface layer tiles" << std::endl;
		std::cerr << "  Mask     pack water mask and night light texture tiles" << std::endl;
		std::cerr << "  Elev     pack elevation tiles" << std::endl;
		std::cerr << "  Elev_mod pack elevation modification tiles" << std::endl;
		std::cerr << "  Cloud    pack cloud tiles" << std::endl;
		std::cerr << "  Label    pack surface label tiles" << std::endl;
		std::cerr << "\n<Flags>:" << std::endl;
		std::cerr << "  -e   : unpack compressed archive into individual tiles" << std::endl;
		std::cerr << "  -L<x>: pack/unpack tiles up to maximum level <x>" << std::endl;
		exit(1);
	}

	const char *root = arg[1];
	const char *layer = arg[2];

	for (int i = 3; i < narg; i++) {
		if (arg[i][0] != '-') continue;
		switch(arg[i][1]) {
		case 'e':
			mode = OP_EXTRACT;
			break;
		case 'L':
			if (sscanf(arg[i]+2, "%d", &maxlevel))
			break;
		}
	}

	std::cout << (mode == OP_ARCHIVE ? "Packing " : "Unpacking ") << layer << " layer for " << root << std::endl;
	if (maxlevel)
		std::cout << "Max. level: " << maxlevel << std::endl;
	else
		maxlevel = 19;

	if (mode == OP_ARCHIVE) {

		// build the tree of existing tiles in memory
		std::cout << "\nBuilding tile tree ..." << std::endl;
		MemTree tree(root, layer);
		tree.AddLevels(1, maxlevel);
		int nnode = tree.NodeCount();

		// construct the TOC from the tree
		TreeTOC toc(root, layer, &tree);

		char outf[256];
		sprintf(outf, "%s\\Archive", root);
		_mkdir(outf);
		sprintf(outf+strlen(outf), "\\%s.tree", layer);
		FILE *f = fopen(outf, "wb");

		// write table of contents
		toc.fwrite(f);
		toc.WriteData(f);

		fclose(f);

		std::cout << std::endl << "Quadtree data written to " << outf << std::endl;
		std::cout << toc.length() << " nodes" << std::endl;
		std::cout << toc.DataSize() << " bytes of data" << std::endl;

	} else {

		TreeTOC toc(root, layer);
		char fname[256];
		sprintf(fname, "%s\\Archive\\%s.tree", root, layer);
		FILE *f = fopen(fname, "rb");
		toc.fread(f);
		toc.ExtractData(f, maxlevel);
		fclose(f);

		std::cout << std::endl << "Quadtree data extracted from " << fname << std::endl;
		std::cout << toc.length() << " nodes" << std::endl;

	}

	return 0;
}

bool exist_file(const char *root, const char *layer, const char *ext, int lvl, int ilat, int ilng)
{
	char path[256];
	sprintf(path, "%s\\%s\\%02d\\%06d\\%06d.%s", root, layer, lvl, ilat, ilng, ext);
	return PathFileExists(path) == TRUE;
}

DWORD deflate_node_data(BYTE *inp, DWORD ninp, BYTE *outp, DWORD noutp)
{
	int ret, flush;
	z_stream strm;
	strm.zalloc = Z_NULL;
	strm.zfree = Z_NULL;
	strm.opaque = Z_NULL;
	ret = deflateInit(&strm, Z_DEFAULT_COMPRESSION);

	strm.avail_in = ninp;
	flush = Z_FINISH;
	strm.next_in = inp;

	strm.avail_out = noutp;
	strm.next_out = outp;
	ret = deflate(&strm, flush);
	if (ret != Z_STREAM_END) {
		if (ret == Z_STREAM_ERROR) {
		}
		exit(1);
	}
	if (strm.avail_in > 0) {
		// problem - data left in input buffer
		exit(1);
	}
	deflateEnd(&strm);
	return strm.total_out;
}

DWORD inflate_node_data(BYTE *inp, DWORD ninp, BYTE *outp, DWORD noutp)
{
	DWORD ndata = noutp;
	if (uncompress(outp, &ndata, inp, ninp) != Z_OK)
		return 0;
#ifdef UNDEF
	int ret, ndata;
	z_stream strm;
	strm.zalloc = Z_NULL;
	strm.zfree = Z_NULL;
	strm.opaque = Z_NULL;
	strm.avail_in = 0;
	strm.next_in = Z_NULL;
	ret = inflateInit(&strm);
	if (ret != Z_OK) return 0;
	strm.avail_in = ninp;
	strm.next_in = inp;
	strm.avail_out = noutp;
	strm.next_out = outp;
	ret = inflate(&strm, Z_FINISH);
	ndata = noutp - strm.avail_out;
	if (ret != Z_STREAM_END) // didn't process all input
		return 0;
	inflateEnd(&strm);
#endif
	return ndata;
}