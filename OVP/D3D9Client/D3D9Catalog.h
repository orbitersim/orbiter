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

public:
	Memgr(std::string n) : name(n)
	{
	}

	~Memgr()
	{
#ifdef _DEBUG
		size_t size = 0; DWORD ent = 0;
		for (auto x : Fre) for (auto y : x.second) { ent++; size += x.first * sizeof(T); delete y; }
		oapiWriteLogV("Memgr[%s] Total of %u bytes in %u entries", name.c_str(), size, ent);
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
		if (Fre.find(size) != Fre.end()) { // Do we have entries of size 'size'
			auto& r = Fre[size];
			if (r.size() > 0) {			// Any any unused exists ?
				auto p = r.front();		// Get top entry
				r.pop_front();			// Remove it
				Rsv[p] = size;			// List it as used
				return p;
			}
		}
		auto x = new T[size];			// Allocate new entry
		Rsv[x] = size;					// List it as used
		return x;
	}

	void Free(T* p)
	{
		auto it = Rsv.find(p);			// Find the entry (log2 complexity)
		assert(it != Rsv.end());
		Fre[it->second].push_front(p);	// Add it in a fron of free entries
		Rsv.erase(it);					// Remove from used (reserved)
	}
};

#endif // !__D3D9EXTRA_H
