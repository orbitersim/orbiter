// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __MEMSTAT_H
#define __MEMSTAT_H

class MemStat {
public:
	MemStat ();
	~MemStat ();

	long HeapUsage ();
};

#endif // !__MEMSTAT_H
