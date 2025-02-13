// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// =======================================================================
// ZTreeMgr.h
// Manage compressed and packed tile trees for planetary surface and cloud layers.
// =======================================================================

#ifndef __ZTREEMGR_H
#define __ZTREEMGR_H

#include <iostream>

// =======================================================================
// Tree node structure

struct TreeNode {
	uint64_t pos;  // file position of node data
	uint32_t size;   // data block size [bytes]
	uint32_t child[4]; // array index positions of the children ((uint32_t)-1=no child)

	TreeNode() {
		pos = 0;
		size = 0;
		for (int i = 0; i < 4; i++) child[i] = (uint32_t)-1;
	}
};

// =======================================================================
// File header for compressed tree files

class TreeFileHeader {
	friend class ZTreeMgr;

public:
	TreeFileHeader();
	size_t fwrite(FILE *f);
	bool fread(FILE *f);

private:
	uint8_t magic[4];      // file ID and version
	uint32_t size;         // header size [bytes]
	uint32_t flags;        // bit flags
	uint32_t dataOfs;      // file offset of start of data block (header + TOC)
	uint64_t dataLength; // total length of compressed data block
	uint32_t nodeCount;    // total number of tree nodes
	uint32_t rootPos1;     // index of level-1 tile ((uint32_t)-1 for not present)
	uint32_t rootPos2;     // index of level-2 tile ((uint32_t)-1 for not present)
	uint32_t rootPos3;     // index of level-3 tile ((uint32_t)-1 for not present)
	uint32_t rootPos4[2];  // index of the level-4 tiles (quadtree roots; (uint32_t)-1 for not present)
};

// =======================================================================
// Tree table of contents

class TreeTOC {
	friend class ZTreeMgr;

public:
	TreeTOC();
	~TreeTOC();
	size_t fread(uint32_t size, FILE *f);
	inline uint32_t size() const { return ntree; }
	inline const TreeNode &operator[](int idx) const { return tree[idx]; }

	inline uint32_t NodeSizeDeflated(uint32_t idx) const
	{ return (uint32_t)((idx < ntree-1 ? tree[idx+1].pos : totlength) - tree[idx].pos); }

	inline uint32_t NodeSizeInflated(uint32_t idx) const
	{ return tree[idx].size; }

private:
	TreeNode *tree;    // array containing all tree node entries
	uint32_t ntree;       // number of entries
	uint32_t ntreebuf;    // array size
	uint64_t totlength; // total data size (deflated)
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

	uint32_t Idx(int lvl, int ilat, int ilng);
	// return the array index of an arbitrary tile ((uint32_t)-1: not present)

	uint32_t ReadData(uint32_t idx, uint8_t **outp);

	inline uint32_t ReadData(int lvl, int ilat, int ilng, uint8_t **outp)
	{ return (ilat < 0 || ilng < 0) ? 0 : ReadData(Idx(lvl, ilat, ilng), outp); }

	void ReleaseData(uint8_t *data);

	inline uint32_t NodeSizeDeflated(uint32_t idx) const { return toc.NodeSizeDeflated(idx); }
	inline uint32_t NodeSizeInflated(uint32_t idx) const { return toc.NodeSizeInflated(idx); }

protected:
	bool OpenArchive();
	uint32_t Inflate(const uint8_t *inp, uint32_t ninp, uint8_t *outp, uint32_t noutp);

private:
	char *path;
	Layer layer;
	FILE *treef;
	TreeTOC toc;
	uint32_t rootPos1;    // index of level-1 tile ((uint32_t)-1 for not present)
	uint32_t rootPos2;    // index of level-2 tile ((uint32_t)-1 for not present)
	uint32_t rootPos3;    // index of level-3 tile ((uint32_t)-1 for not present)
	uint32_t rootPos4[2]; // index of the level-4 tiles (quadtree roots; (uint32_t)-1 for not present)
	uint64_t dofs;
};

#endif // !__ZTREEMGR_H
