// ==============================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// Copyright (C) 2012 - 2016 Jarmo Nikkanen
// ==============================================================

#ifndef __D3D9CATALOG_H
#define __D3D9CATALOG_H

#include <set>

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

#endif // !__D3D9EXTRA_H
