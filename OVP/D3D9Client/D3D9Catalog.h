// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2012 - 2016 Jarmo Nikkanen
// ==============================================================

#ifndef __D3D9CATALOG_H
#define __D3D9CATALOG_H

#include <set>
#include <map>
#include <list>
#include <string>
#include <assert.h>
#include <mutex>
#include <d3dx9.h>
#include "OrbiterAPI.h"

template <typename T>
class D3D9Catalog {
public:
	D3D9Catalog ()					{}
	~D3D9Catalog ()					{ Clear(); }

	void	Add (T entry)			{ _data.insert(entry); }
	void	Clear ()				{ _data.clear();  }
//	T		Seek (T entry) const	{ return _data.find(entry) - _data.begin(); }
	size_t	CountEntries () const	{ return _data.size(); }
	bool	Remove (T entry)		{ return _data.erase(entry) == 1; }

	typedef std::set<T> TSet;
	typedef typename TSet::iterator iterator;
	typedef typename TSet::const_iterator const_iterator;

	iterator		begin () const	{ return _data.begin(); }
	iterator		end () const	{ return _data.end(); }
	const_iterator	cbegin () const	{ return _data.cbegin(); }
	const_iterator	cend ()	const	{ return _data.cend(); }

//	iterator rbegin() const			{ return _data.rbegin(); }
//	iterator rend() const			{ return _data.rend(); }
//	const_iterator crbegin() const	{ return _data.crbegin(); }
//	const_iterator crend() const	{ return _data.crend(); }

private:
	TSet _data;
};


// ---------------------------------------------------------------
// Memory Manager
// ---------------------------------------------------------------

template <typename T>
class Memgr
{
private:

	std::string name;
	std::map<DWORD, std::list<T*>> Fre;
	std::map<T*, DWORD> Rsv;
	std::mutex mm;

public:
	Memgr(std::string n) : name(n)
	{
	}

	~Memgr()
	{
#ifdef _DEBUG
		size_t size = 0; DWORD ent = 0;
		for (auto x : Fre) for (auto y : x.second) { ent++; size += x.first; delete y; }
		oapiWriteLogV("Memgr[%s] Total of %u bytes in %u entries", name.c_str(), size * sizeof(T), ent);
		if (Rsv.size() == 0) oapiWriteLogV("Memgr[%s] All clear",name.c_str());
		else for (auto x : Rsv) {
			oapiWriteLogV("Memgr[%s] Leaking %u bytes", name.c_str(), x.second * sizeof(T));
			delete x.first;
		}
#else
		for (auto x : Fre) for (auto y : x.second) delete y;
		for (auto x : Rsv) delete x.first;
#endif // DEBUG
	}

	T* New(DWORD size)
	{
		mm.lock();
		if (Fre.find(size) != Fre.end()) { // Do we have entries of size 'size'
			auto& r = Fre[size];
			if (r.size() > 0) {			// Any any unused exists ?
				auto p = r.front();		// Get top entry
				r.pop_front();			// Remove it
				Rsv[p] = size;			// List it as used
				mm.unlock();
				return p;
			}
		}
		auto x = new T[size];			// Allocate new entry
		Rsv[x] = size;					// List it as used
		mm.unlock();
		return x;
	}

	void Free(T* p)
	{
		mm.lock();
		auto it = Rsv.find(p);			// Find the entry (log2 complexity)
		assert(it != Rsv.end());
		Fre[it->second].push_front(p);	// Add it in a fron of free entries
		Rsv.erase(it);					// Remove from used (reserved)
		mm.unlock();
	}

	size_t UsedSize()
	{
		mm.lock();
		size_t ec = 0;
		for (auto x : Rsv) ec += x.second * sizeof(T);
		mm.unlock();
		return ec;
	}

	size_t FreeSize()
	{
		mm.lock();
		size_t ec = 0;
		for (auto x : Fre) for (auto y : x.second) { es += x.first * sizeof(T); }
		mm.unlock();
		return ec;
	}
};



// ---------------------------------------------------------------
// Object Manager, Base
// ---------------------------------------------------------------

template <typename T>
class Objmgr
{

public:
	Objmgr(LPDIRECT3DDEVICE9 pD, std::string n) : name(n), pDev(pD)	{ }
	~Objmgr() {
		Fre.clear();
		Rsv.clear();
	}

	void CleanUp()
	{
		mm.lock();
#ifdef _DEBUG
		size_t size = 0; DWORD ent = 0;
		for (auto x : Fre) for (auto y : x.second) { ent++; size += UnitSize(x.first); Delete(y); }
		oapiWriteLogV("Objmgr[%s] Total of %u bytes in %u entries", name.c_str(), size, ent);
		if (Rsv.size() == 0) oapiWriteLogV("Objmgr[%s] All clear", name.c_str());
		else for (auto x : Rsv) {
			oapiWriteLogV("Objmgr[%s] Leaking %u bytes", name.c_str(), UnitSize(x.second));
			Delete(x.first);
		}
#else
		for (auto x : Fre) for (auto y : x.second) Delete(y);
		for (auto x : Rsv) Delete(x.first);
#endif // DEBUG
		mm.unlock();
	}


	T New(DWORD size)
	{
		mm.lock();
		auto a = Fre.find(size);
		auto b = Fre.end();
		if (a != b) { // Do we have entries of size 'size'
			auto& r = Fre[size];
			if (r.size() > 0) {			// Any any unused exists ?
				auto p = r.front();		// Get top entry
				r.pop_front();			// Remove it
				Rsv[p] = size;			// List it as used
				mm.unlock();
				return p;
			}
		}
		auto x = Alloc(size);
		Rsv[x] = size;					// List it as used
		mm.unlock();
		return x;
	}

	void Free(T p)
	{
		mm.lock();
		auto it = Rsv.find(p);			// Find the entry (log2 complexity)
		assert(it != Rsv.end());
		Fre[it->second].push_front(p);	// Add it in a fron of free entries
		Rsv.erase(it);					// Remove from used (reserved)
		mm.unlock();
	}

	size_t UsedSize()
	{
		mm.lock();
		size_t s = 0;
		for (auto x : Rsv) s += UnitSize(x.second);
		mm.unlock();
		return s;
	}

	size_t FreeSize()
	{
		mm.lock();
		size_t s = 0;
		for (auto x : Fre) for (auto y : x.second) { s += UnitSize(x.first); }
		mm.unlock();
		return s;
	}

	int UsedCount()
	{
		mm.lock();
		int c = Rsv.size();
		mm.unlock();
		return c;
	}

	int FreeCount()
	{
		mm.lock();
		int c = 0;
		for (auto x : Fre) c += x.second.size();
		mm.unlock();
		return c;
	}

protected:
	virtual T Alloc(DWORD size) { assert(false); return nullptr; };
	virtual void Delete(T x) { assert(false); };
	virtual size_t UnitSize(DWORD size) { assert(false); return size; }
	LPDIRECT3DDEVICE9 pDev;

private:
	std::string name;
	std::map<DWORD, std::list<T>> Fre;
	std::map<T, DWORD> Rsv;
	std::mutex mm;
};



// ---------------------------------------------------------------
// Tile Texture Manager
// ---------------------------------------------------------------

template <typename T>
class Texmgr : public Objmgr<T>
{
public:
	Texmgr(LPDIRECT3DDEVICE9 pD, std::string n) : Objmgr(pD, n) { }
	~Texmgr() {}

	T New(DWORD size, D3DFORMAT Format)
	{
		DWORD fmt = 0;
		if (Format == D3DFMT_X8B8G8R8) fmt = 1;
		if (Format == D3DFMT_DXT1) fmt = 2;
		if (Format == D3DFMT_DXT3) fmt = 3;
		if (Format == D3DFMT_DXT5) fmt = 4;

		return Objmgr::New(size + (fmt << 16));
	}

protected:

	T Alloc(DWORD prm)
	{
		LPDIRECT3DTEXTURE9 pT = nullptr;
		D3DFORMAT Format = D3DFMT_A8B8G8R8;

		UINT size = prm & 0xFFFF;
		UINT frmt = prm >> 16;
		UINT Mips = (Config->TileMipmaps == 1) ? 6 : 1;

		if (frmt == 1) Format = D3DFMT_X8B8G8R8;
		if (frmt == 2) Format = D3DFMT_DXT1;
		if (frmt == 3) Format = D3DFMT_DXT3;
		if (frmt == 4) Format = D3DFMT_DXT5;

		if(D3DXCreateTexture(pDev, size, size, Mips, 0, Format, D3DPOOL_DEFAULT, &pT) != S_OK)
		{
			oapiWriteLog("Failed to create texture for surface tile. Likely [Out of Video Memory]");
			abort();
		}
		return (T)pT;
	}

	void Delete(T x) { x->Release(); }
	size_t UnitSize(DWORD size) { return (size & 0xFFFF) * (size & 0xFFFF); }
};



// ---------------------------------------------------------------
// Tile VertexBuffer Manager
// ---------------------------------------------------------------

template <typename T>
class Vtxmgr : public Objmgr<T>
{
public:
	Vtxmgr(LPDIRECT3DDEVICE9 pD, std::string n) : Objmgr(pD, n) { }
protected:
	T Alloc(DWORD size)
	{
		LPDIRECT3DVERTEXBUFFER9 pVB = nullptr;
		if (pDev->CreateVertexBuffer(size * sizeof(VERTEX_2TEX), D3DUSAGE_DYNAMIC | D3DUSAGE_WRITEONLY, 0, D3DPOOL_DEFAULT, &pVB, NULL) != S_OK)
		{
			oapiWriteLog("Failed to create vertex buffer for surface tile. Likely [Out of Video Memory]");
			abort();
		}
		return (T)pVB;
	}
	void Delete(T x) { x->Release(); }
	size_t UnitSize(DWORD size) { return size * sizeof(VERTEX_2TEX); }
};



// ---------------------------------------------------------------
// Tile IndexBuffer Manager
// ---------------------------------------------------------------

template <typename T>
class Idxmgr : public Objmgr<T>
{
public:
	Idxmgr(LPDIRECT3DDEVICE9 pD, std::string n) : Objmgr(pD, n) { }
protected:
	T Alloc(DWORD size)
	{
		LPDIRECT3DINDEXBUFFER9 pIB = nullptr;
		if (pDev->CreateIndexBuffer(size * sizeof(WORD) * 3, D3DUSAGE_DYNAMIC | D3DUSAGE_WRITEONLY, D3DFMT_INDEX16, D3DPOOL_DEFAULT, &pIB, NULL) != S_OK)
		{
			oapiWriteLog("Failed to create index buffer for surface tile. Likely [Out of Video Memory]");
			abort();
		}
		return (T)pIB;
	}
	void Delete(T x) { x->Release(); }
	size_t UnitSize(DWORD size) { return size * sizeof(WORD) * 3; }
};

#endif // !__D3D9EXTRA_H
