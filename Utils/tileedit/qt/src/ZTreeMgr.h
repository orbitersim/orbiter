// =======================================================================
// ZTreeMgr.h
// Manage compressed and packed tile trees for planetary surface and cloud layers.
// =======================================================================

#ifndef __ZTREEMGR_H
#define __ZTREEMGR_H

#include <iostream>
#include <windows.h>

// =======================================================================
// Tree node structure

struct TreeNode {
	__int64 pos;  // file position of node data
	DWORD size;   // data block size [bytes]
	DWORD child[4]; // array index positions of the children ((DWORD)-1=no child)

	TreeNode() {
		pos = 0;
		size = 0;
		for (int i = 0; i < 4; i++) child[i] = (DWORD)-1;
	}
};

// =======================================================================
// File header for compressed tree files

class TreeFileHeader {
	friend class ZTreeMgr;

public:
	TreeFileHeader();
	size_t TreeFileHeader::fwrite(FILE *f);
	bool TreeFileHeader::fread(FILE *f);

private:
	BYTE magic[4];      // file ID and version
	DWORD size;         // header size [bytes]
	DWORD flags;        // bit flags
	DWORD dataOfs;      // file offset of start of data block (header + TOC)
	__int64 dataLength; // total length of compressed data block
	DWORD nodeCount;    // total number of tree nodes
	DWORD rootPos1;     // index of level-1 tile ((DWORD)-1 for not present)
	DWORD rootPos2;     // index of level-2 tile ((DWORD)-1 for not present)
	DWORD rootPos3;     // index of level-3 tile ((DWORD)-1 for not present)
	DWORD rootPos4[2];  // index of the level-4 tiles (quadtree roots; (DWORD)-1 for not present)
};

// =======================================================================
// Tree table of contents

class TreeTOC {
	friend class ZTreeMgr;

public:
	TreeTOC();
	~TreeTOC();
	size_t fread(DWORD size, FILE *f);
	inline DWORD size() const { return ntree; }
	inline const TreeNode &operator[](int idx) const { return tree[idx]; }

	inline DWORD NodeSizeDeflated(DWORD idx) const
	{ return (DWORD)((idx < ntree-1 ? tree[idx+1].pos : totlength) - tree[idx].pos); }

	inline DWORD NodeSizeInflated(DWORD idx) const
	{ return tree[idx].size; }

private:
	TreeNode *tree;    // array containing all tree node entries
	DWORD ntree;       // number of entries
	DWORD ntreebuf;    // array size
	__int64 totlength; // total data size (deflated)
};

// =======================================================================
// ZTreeMgr class: manage a single layer tree for a planet

class ZTreeMgr {
public:
	enum Layer { LAYER_SURF, LAYER_MASK, LAYER_ELEV, LAYER_ELEVMOD, LAYER_LABEL, LAYER_CLOUD };
	static ZTreeMgr *CreateFromFile(const char *PlanetPath, Layer _layer);
	ZTreeMgr(const char *PlanetPath, Layer _layer);
	~ZTreeMgr();
	const TreeTOC &TOC() const { return toc; }

	DWORD Idx(int lvl, int ilat, int ilng) const;
	// return the array index of an arbitrary tile ((DWORD)-1: not present)

	DWORD ReadData(DWORD idx, BYTE **outp) const;

	inline DWORD ReadData(int lvl, int ilat, int ilng, BYTE **outp) const
	{ return ReadData(Idx(lvl, ilat, ilng), outp); }

	void ReleaseData(BYTE *data) const;

	inline DWORD NodeSizeDeflated(DWORD idx) const { return toc.NodeSizeDeflated(idx); }
	inline DWORD NodeSizeInflated(DWORD idx) const { return toc.NodeSizeInflated(idx); }

protected:
	bool OpenArchive();
	DWORD Inflate(const BYTE *inp, DWORD ninp, BYTE *outp, DWORD noutp) const;

private:
	char *path;
	Layer layer;
	FILE *treef;
	TreeTOC toc;
	DWORD rootPos1;    // index of level-1 tile ((DWORD)-1 for not present)
	DWORD rootPos2;    // index of level-2 tile ((DWORD)-1 for not present)
	DWORD rootPos3;    // index of level-3 tile ((DWORD)-1 for not present)
	DWORD rootPos4[2]; // index of the level-4 tiles (quadtree roots; (DWORD)-1 for not present)
	__int64 dofs;
};

#endif // !__ZTREEMGR_H
