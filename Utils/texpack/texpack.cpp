// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include <iostream>
#include <string>
#ifndef __linux__
#include <windows.h>
#include <direct.h>
#include <Shlwapi.h>
#else // __linux__
#include <cstdio>
#include <cstring>
#include <strings.h>
#include "OrbiterPlatform.h" // windows.h left out: BYTE/DWORD/BOOL
#include <sys/stat.h> // direct.h: _mkdir -> mkdir
#include <dirent.h>   // Shlwapi.h left out: PathFileExists -> access, FindFirstFile -> readdir
#include <unistd.h>
#endif // __linux__
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
#ifndef __linux__
	sprintf(path, "%s\\%s", rootpath, layer);
	if (!stricmp(layer, "Surf"))
#else // __linux__
	sprintf(path, "%s/%s", rootpath, layer);
	if (!strcasecmp(layer, "Surf"))
#endif // __linux__
		strcpy(ext, "dds");
#ifndef __linux__
	else if (!stricmp(layer, "Mask"))
#else // __linux__
	else if (!strcasecmp(layer, "Mask"))
#endif // __linux__
		strcpy(ext, "dds");
#ifndef __linux__
	else if (!stricmp(layer, "Cloud"))
#else // __linux__
	else if (!strcasecmp(layer, "Cloud"))
#endif // __linux__
		strcpy(ext, "dds");
#ifndef __linux__
	else if (!stricmp(layer, "Elev"))
#else // __linux__
	else if (!strcasecmp(layer, "Elev"))
#endif // __linux__
		strcpy(ext, "elv");
#ifndef __linux__
	else if (!stricmp(layer, "Elev_mod"))
#else // __linux__
	else if (!strcasecmp(layer, "Elev_mod"))
#endif // __linux__
		strcpy(ext, "elv");
#ifndef __linux__
	else if (!stricmp(layer, "Label"))
#else // __linux__
	else if (!strcasecmp(layer, "Label"))
#endif // __linux__
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

#ifdef __linux__
// not upstream: readdir filtered by a "*.<ext>" pattern (case-sensitive, like the access/fopen of the tile that follow)
static dirent *readdir_ext(DIR *h, const char *ext)
{
	for (dirent *fdata; (fdata = readdir(h)); ) {
		const char *dot = strrchr(fdata->d_name, '.');
		if (dot ? !strcmp(dot+1, ext) : !ext[0]) return fdata;
	}
	return 0;
}

// -----------------------------------------------------------------------------

#endif // __linux__
void MemTree::AddLevel(int lvl)
{
	char lvlpath[256];
#ifndef __linux__
	sprintf(lvlpath, "%s\\%02d", path, lvl);
	if (PathFileExists(lvlpath)) {
		WIN32_FIND_DATA fdata, fdata2;
		strcat(lvlpath, "\\*");
		HANDLE h = FindFirstFile(lvlpath, &fdata);
		BOOL ok = (h != INVALID_HANDLE_VALUE);
#else // __linux__
	sprintf(lvlpath, "%s/%02d", path, lvl);
	if (access(lvlpath, F_OK) == 0) {
		dirent *fdata, *fdata2;
		DIR *h = opendir(lvlpath); // FindFirstFile/FindNextFile -> opendir/readdir
		BOOL ok = (h && (fdata = readdir(h)));
#endif // __linux__
		while (ok) {
#ifndef __linux__
			bool match = (strlen(fdata.cFileName) == 6);
#else // __linux__
			bool match = (strlen(fdata->d_name) == 6);
#endif // __linux__
			for (int i = 0; i < 6; i++)
#ifndef __linux__
				match = match && fdata.cFileName[i] >= '0' && fdata.cFileName[i] <= '9';
#else // __linux__
				match = match && fdata->d_name[i] >= '0' && fdata->d_name[i] <= '9';
#endif // __linux__
			if (match) {
				int ilat, ilng;
#ifndef __linux__
				sscanf(fdata.cFileName, "%d", &ilat);
#else // __linux__
				sscanf(fdata->d_name, "%d", &ilat);
#endif // __linux__
				char latpath[256];
#ifndef __linux__
				strcpy(latpath, lvlpath); strcpy(latpath+strlen(latpath)-1, fdata.cFileName);
				strcat(latpath, "\\*."); strcat(latpath, ext);
				HANDLE h2 = FindFirstFile(latpath, &fdata2);
				BOOL ok2 = (h2 != INVALID_HANDLE_VALUE);
#else // __linux__
				strcpy(latpath, lvlpath); strcat(latpath, "/"); strcat(latpath, fdata->d_name);
				DIR *h2 = opendir(latpath); // "*.<ext>" pattern: readdir_ext
				BOOL ok2 = (h2 && (fdata2 = readdir_ext(h2, ext)));
#endif // __linux__
				while (ok2) {
#ifndef __linux__
					sscanf(fdata2.cFileName, "%d", &ilng);
#else // __linux__
					sscanf(fdata2->d_name, "%d", &ilng);
#endif // __linux__
					InsertNode(lvl, ilat, ilng);
#ifndef __linux__
					ok2 = FindNextFile(h2, &fdata2);
#else // __linux__
					ok2 = ((fdata2 = readdir_ext(h2, ext)) != 0);
#endif // __linux__
				}
#ifndef __linux__
				FindClose(h2);
#else // __linux__
				if (h2) closedir(h2);
#endif // __linux__
			}
#ifndef __linux__
			ok = FindNextFile(h, &fdata);
#else // __linux__
			ok = ((fdata = readdir(h)) != 0);
#endif // __linux__
		}
#ifndef __linux__
		FindClose (h);
#else // __linux__
		if (h) closedir (h);
#endif // __linux__
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
#ifndef __linux__
	__int64 pos;     // file position of compressed data block (from end of TOC)
#else // __linux__
	int64_t pos;     // file position of compressed data block (from end of TOC)
#endif // __linux__
	DWORD size;      // uncompressed data size
	DWORD child[4];  // array positions of the children ((DWORD)-1=no child)

	TOCEntry() {
		pos = 0;
		size = 0;
		for (int i = 0; i < 4; i++) child[i] = -1;
	}
};
#ifdef __linux__
static_assert(sizeof(TOCEntry) == 32, "TOCEntry: .tree file layout"); // not upstream: MSVC layout check
#endif // __linux__

//==============================================================================
// Tree file table of contents

class TreeTOC {
public:
	TreeTOC(const char *_root, const char *_layer, const MemTree *tree); // build the TOC from a tree
	TreeTOC(const char *_root, const char *_layer);
	~TreeTOC();
	TOCEntry &operator[](int idx);
	DWORD length() const { return header.ntoc; }
#ifndef __linux__
	__int64 DataSize() const { return header.totlength; }
#else // __linux__
	int64_t DataSize() const { return header.totlength; }
#endif // __linux__
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
#ifndef __linux__
		__int64 totlength;  // total deflated data size
#else // __linux__
		int64_t totlength;  // total deflated data size
#endif // __linux__
		DWORD ntoc;         // number of tree nodes
		DWORD rootPos1;     // array index of level 1 tilWriteSubtreeDatae ((DWORD)-1 for not present)
		DWORD rootPos2;     // array index of level 2 tile ((DWORD)-1 for not present)
		DWORD rootPos3;     // array index of level 3 tile ((DWORD)-1 for not present)
		DWORD rootPos4[2];  // array indices of level 4 tiles (quadtree roots; (DWORD)-1 for not present)
	} header;
#ifdef __linux__
	static_assert(sizeof(Header) == 48, "Header: .tree file layout"); // not upstream: MSVC layout check
#endif // __linux__

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

#ifndef __linux__
	if (!stricmp(layer, "Surf"))
#else // __linux__
	if (!strcasecmp(layer, "Surf"))
#endif // __linux__
		strcpy(ext, "dds");
#ifndef __linux__
	else if (!stricmp(layer, "Mask"))
#else // __linux__
	else if (!strcasecmp(layer, "Mask"))
#endif // __linux__
		strcpy(ext, "dds");
#ifndef __linux__
	else if (!stricmp(layer, "Cloud"))
#else // __linux__
	else if (!strcasecmp(layer, "Cloud"))
#endif // __linux__
		strcpy(ext, "dds");
#ifndef __linux__
	else if (!stricmp(layer, "Elev"))
#else // __linux__
	else if (!strcasecmp(layer, "Elev"))
#endif // __linux__
		strcpy(ext, "elv");
#ifndef __linux__
	else if (!stricmp(layer, "Elev_mod"))
#else // __linux__
	else if (!strcasecmp(layer, "Elev_mod"))
#endif // __linux__
		strcpy(ext, "elv");
#ifndef __linux__
	else if (!stricmp(layer, "Label"))
#else // __linux__
	else if (!strcasecmp(layer, "Label"))
#endif // __linux__
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

#ifndef __linux__
	if (!stricmp(layer, "Surf"))
#else // __linux__
	if (!strcasecmp(layer, "Surf"))
#endif // __linux__
		strcpy(ext, "dds");
#ifndef __linux__
	else if (!stricmp(layer, "Mask"))
#else // __linux__
	else if (!strcasecmp(layer, "Mask"))
#endif // __linux__
		strcpy(ext, "dds");
#ifndef __linux__
	else if (!stricmp(layer, "Cloud"))
#else // __linux__
	else if (!strcasecmp(layer, "Cloud"))
#endif // __linux__
		strcpy(ext, "dds");
#ifndef __linux__
	else if (!stricmp(layer, "Elev"))
#else // __linux__
	else if (!strcasecmp(layer, "Elev"))
#endif // __linux__
		strcpy(ext, "elv");
#ifndef __linux__
	else if (!stricmp(layer, "Elev_mod"))
#else // __linux__
	else if (!strcasecmp(layer, "Elev_mod"))
#endif // __linux__
		strcpy(ext, "elv");
#ifndef __linux__
	else if (!stricmp(layer, "Label"))
#else // __linux__
	else if (!strcasecmp(layer, "Label"))
#endif // __linux__
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
#ifndef __linux__
			LARGE_INTEGER sz;
#else // __linux__
			struct { DWORD LowPart; } sz; // LARGE_INTEGER left out: only its low part is used
#endif // __linux__
			DWORD ndata;
#ifndef __linux__
			sprintf(path, "%s\\%s\\%02d\\%06d\\%06d.%s", root, layer, lvl, ilat, ilng, ext);
			HANDLE hFile = CreateFile(path, GENERIC_READ, 0, 0, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
			GetFileSizeEx(hFile, &sz);
#else // __linux__
			sprintf(path, "%s/%s/%02d/%06d/%06d.%s", root, layer, lvl, ilat, ilng, ext);
			FILE *hFile = fopen(path, "rb"); // CreateFile/GetFileSizeEx/ReadFile/CloseHandle -> stdio
			struct stat st; sz.LowPart = (hFile && !fstat(fileno(hFile), &st)) ? (DWORD)st.st_size : 0;
#endif // __linux__
			if (sz.LowPart > nbuf) { // grow data buffer
				BYTE *tmp = new BYTE[nbuf = sz.LowPart];
				delete[]buf;
				buf = tmp;
			}
			DWORD nread;
#ifndef __linux__
			ReadFile(hFile, buf, sz.LowPart, &nread, NULL);
			CloseHandle(hFile);
#else // __linux__
			nread = hFile ? (DWORD)::fread(buf, 1, sz.LowPart, hFile) : 0;
			if (hFile) fclose(hFile);
#endif // __linux__
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
#ifndef __linux__
			LARGE_INTEGER sz;
#else // __linux__
			struct { DWORD LowPart; } sz; // LARGE_INTEGER left out: only its low part is used
#endif // __linux__
			DWORD ndata;
#ifndef __linux__
			sprintf(path, "%s\\%s\\%02d\\%06d\\%06d.%s", root, layer, lvl, ilat, ilng, ext);
			HANDLE hFile = CreateFile(path, GENERIC_READ, 0, 0, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
			GetFileSizeEx(hFile, &sz);
#else // __linux__
			sprintf(path, "%s/%s/%02d/%06d/%06d.%s", root, layer, lvl, ilat, ilng, ext);
			FILE *hFile = fopen(path, "rb"); // CreateFile/GetFileSizeEx/ReadFile/CloseHandle -> stdio
			struct stat st; sz.LowPart = (hFile && !fstat(fileno(hFile), &st)) ? (DWORD)st.st_size : 0;
#endif // __linux__
			if (sz.LowPart > nbuf) { // grow data buffer
				BYTE *tmp = new BYTE[nbuf = sz.LowPart];
				delete[]buf;
				buf = tmp;
			}
			DWORD nread;
#ifndef __linux__
			ReadFile(hFile, buf, sz.LowPart, &nread, NULL);
			CloseHandle(hFile);
#else // __linux__
			nread = hFile ? (DWORD)::fread(buf, 1, sz.LowPart, hFile) : 0;
			if (hFile) fclose(hFile);
#endif // __linux__
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

#ifndef __linux__
	_fseeki64(f, (__int64)header.dataOfs + entry->pos, SEEK_SET);
#else // __linux__
	fseeko(f, (int64_t)header.dataOfs + entry->pos, SEEK_SET);
#endif // __linux__
	int nread = ::fread(zbuf, 1, zsize, f);

	BYTE *ebuf = new BYTE[esize];
	inflate_node_data(zbuf, zsize, ebuf, esize);

	char fname[256];
#ifndef __linux__
	sprintf (fname, "%s\\%s", root, layer);
	_mkdir(fname);
	sprintf (fname+strlen(fname), "\\%02d", lvl);
	_mkdir(fname);
	sprintf (fname+strlen(fname), "\\%06d", ilat);
	_mkdir(fname);
	sprintf (fname+strlen(fname), "\\%06d.%s", ilng, ext);
#else // __linux__
	sprintf (fname, "%s/%s", root, layer);
	mkdir(fname, 0777);
	sprintf (fname+strlen(fname), "/%02d", lvl);
	mkdir(fname, 0777);
	sprintf (fname+strlen(fname), "/%06d", ilat);
	mkdir(fname, 0777);
	sprintf (fname+strlen(fname), "/%06d.%s", ilng, ext);
#endif // __linux__
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
#ifndef __linux__
		std::cerr << "  c:\\Orbiter\\Textures\\Earth" << std::endl;
#else // __linux__
		std::cerr << "  ~/Orbiter/Textures/Earth" << std::endl;
#endif // __linux__
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
#ifndef __linux__
		sprintf(outf, "%s\\Archive", root);
		_mkdir(outf);
		sprintf(outf+strlen(outf), "\\%s.tree", layer);
#else // __linux__
		sprintf(outf, "%s/Archive", root);
		mkdir(outf, 0777);
		sprintf(outf+strlen(outf), "/%s.tree", layer);
#endif // __linux__
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
#ifndef __linux__
		sprintf(fname, "%s\\Archive\\%s.tree", root, layer);
#else // __linux__
		sprintf(fname, "%s/Archive/%s.tree", root, layer);
#endif // __linux__
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
#ifndef __linux__
	sprintf(path, "%s\\%s\\%02d\\%06d\\%06d.%s", root, layer, lvl, ilat, ilng, ext);
	return PathFileExists(path) == TRUE;
#else // __linux__
	sprintf(path, "%s/%s/%02d/%06d/%06d.%s", root, layer, lvl, ilat, ilng, ext);
	return access(path, F_OK) == 0; // PathFileExists
#endif // __linux__
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
#ifndef __linux__
	DWORD ndata = noutp;
#else // __linux__
	uLongf ndata = noutp; // zlib's uLongf is 64-bit on LP64
#endif // __linux__
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
