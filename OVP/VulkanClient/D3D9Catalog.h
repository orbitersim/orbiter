// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2012-2026 Jarmo Nikkanen
// ==============================================================

#ifndef __D3D9CATALOG_H
#define __D3D9CATALOG_H

#include <set>
#include <map>
#include <list>
#include <string>
#include <assert.h>
#include <mutex>
#include <memory>
#include "OrbiterAPI.h"
#include "D3D9Util.h"   // d3dx9.h; VERTEX_2TEX (g++ resolves non-dependent names at the template definition)
#include "D3D9Config.h" // Config, for the same reason

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
		for (auto x : Fre) {
			size_t size = 0; DWORD ent = 0;
			for (auto y : x.second) { ent++; size += x.first; delete y; }
			oapiWriteLogV("Memgr[%s] Size[%u]: Total of %u bytes in %u entries", name.c_str(), x.first, size * sizeof(T), ent);
		}
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
		for (auto x : Fre) for (auto y : x.second) { ec += x.first * sizeof(T); } // es: upstream typo, MSVC never parsed it
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
	Objmgr(VkDev *pD, std::string n) : name(n), pDev(pD)	{ }
	~Objmgr() {
		Fre.clear();
		Rsv.clear();
	}

	void CleanUp()
	{
		pDev->WaitIdle();
		mm.lock();
		for (auto x : Pnd) Delete(x.first); // not upstream: entries still waiting for the GPU
		Pnd.clear();
#ifdef _DEBUG
		for (auto x : Fre) {
			size_t size = 0; DWORD ent = 0;
			for (auto y : x.second) { ent++; size += UnitSize(x.first); Delete(y); }
			oapiWriteLogV("Objmgr[%s] Size[%u]: Total of %u bytes in %u entries", name.c_str(), x.first, size, ent);
		}	
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
		Pnd[p] = it->second;			// not upstream: back on the free list only when the GPU is done with the frames that may read it
		Rsv.erase(it);					// Remove from used (reserved)
		mm.unlock();
		std::weak_ptr<bool> w = alive;
		pDev->Defer([this, p, w]() {
			if (w.expired()) return;
			mm.lock();
			auto q = Pnd.find(p);
			if (q != Pnd.end()) { Fre[q->second].push_front(p); Pnd.erase(q); } // Add it in a fron of free entries
			mm.unlock();
		});
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
	VkDev *pDev;

private:
	std::string name;
	std::map<DWORD, std::list<T>> Fre;
	std::map<T, DWORD> Rsv;
	std::map<T, DWORD> Pnd;			// not upstream: freed, the GPU may still read them
	std::shared_ptr<bool> alive = std::make_shared<bool>(true);
	std::mutex mm;
};



// ---------------------------------------------------------------
// Tile Texture Manager
// ---------------------------------------------------------------

template <typename T>
class Texmgr : public Objmgr<T>
{
public:
	Texmgr(VkDev *pD, std::string n) : Objmgr<T>(pD, n) { }
	~Texmgr() {}

	T New(DWORD size, VkFormat Format)
	{
		DWORD fmt = 0; // X8B8G8R8 (1) and A8B8G8R8 (0) are both R8G8B8A8
		if (Format == VK_FORMAT_BC1_RGBA_UNORM_BLOCK) fmt = 2;
		if (Format == VK_FORMAT_BC2_UNORM_BLOCK) fmt = 3;
		if (Format == VK_FORMAT_BC3_UNORM_BLOCK) fmt = 4;

		return Objmgr<T>::New(size + (fmt << 16));
	}

protected:

	T Alloc(DWORD prm)
	{
		VkTex *pT = nullptr;
		VkFormat Format = VK_FORMAT_R8G8B8A8_UNORM;

		UINT size = prm & 0xFFFF;
		UINT frmt = prm >> 16;
		UINT Mips = (Config->TileMipmaps == 1) ? 6 : 1;

		if (frmt == 2) Format = VK_FORMAT_BC1_RGBA_UNORM_BLOCK;
		if (frmt == 3) Format = VK_FORMAT_BC2_UNORM_BLOCK;
		if (frmt == 4) Format = VK_FORMAT_BC3_UNORM_BLOCK;

		pT = new VkTex(this->pDev, size, size, Mips, Format, VK_IMAGE_USAGE_SAMPLED_BIT | VK_IMAGE_USAGE_TRANSFER_DST_BIT);
		if (pT->img == VK_NULL_HANDLE)
		{
			oapiWriteLog("Failed to create texture for surface tile. Likely [Out of Video Memory]");
			abort();
		}
		return (T)pT;
	}

	void Delete(T x) {
		delete x; // Release
	}
	size_t UnitSize(DWORD size) { return (size & 0xFFFF) * (size & 0xFFFF); }
};



// ---------------------------------------------------------------
// Tile VertexBuffer Manager
// ---------------------------------------------------------------

template <typename T>
class Vtxmgr : public Objmgr<T>
{
public:
	Vtxmgr(VkDev *pD, std::string n) : Objmgr<T>(pD, n) { }
protected:
	T Alloc(DWORD size)
	{
		VkBuf *pVB = new VkBuf(this->pDev, size * sizeof(VERTEX_2TEX), VK_BUFFER_USAGE_VERTEX_BUFFER_BIT, true); // D3DUSAGE_DYNAMIC
		if (pVB->buf == VK_NULL_HANDLE)
		{
			oapiWriteLog("Failed to create vertex buffer for surface tile. Likely [Out of Video Memory]");
			abort();
		}
		return (T)pVB;
	}
	void Delete(T x) {
		delete x; // Release
	}
	size_t UnitSize(DWORD size) { return size * sizeof(VERTEX_2TEX); }
};



// ---------------------------------------------------------------
// Tile IndexBuffer Manager
// ---------------------------------------------------------------

template <typename T>
class Idxmgr : public Objmgr<T>
{
public:
	Idxmgr(VkDev *pD, std::string n) : Objmgr<T>(pD, n) { }
protected:
	T Alloc(DWORD size)
	{
		VkBuf *pIB = new VkBuf(this->pDev, size * sizeof(WORD) * 3, VK_BUFFER_USAGE_INDEX_BUFFER_BIT, true); // D3DUSAGE_DYNAMIC, D3DFMT_INDEX16
		if (pIB->buf == VK_NULL_HANDLE)
		{
			oapiWriteLog("Failed to create index buffer for surface tile. Likely [Out of Video Memory]");
			abort();
		}
		return (T)pIB;
	}
	void Delete(T x) {
		delete x; // Release
	}
	size_t UnitSize(DWORD size) { return size * sizeof(WORD) * 3; }
};

#endif // !__D3D9EXTRA_H

