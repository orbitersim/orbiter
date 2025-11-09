#pragma once
// Virtual filesystem

#include "OrbiterAPI.h"
#include <functional>

namespace VFS
{
	OAPIFUNC FILE *fopen(const char * path, const char *mode);
	OAPIFUNC bool is_directory(const char *path);
	OAPIFUNC bool exists(const char *path);
	OAPIFUNC void enumerate(const char *dir, std::function<void(const char *)> callback);
	OAPIFUNC bool has_extension(const char *path, const char *ext);
	OAPIFUNC bool is_regular_file(const char *);
	OAPIFUNC const char *basename(const char *path);
	OAPIFUNC const char *dirname(const char *path, char *dst);
	OAPIFUNC const char *stem(const char *path, char *dst);
	OAPIFUNC const char *realpath(const char *path, char *dst);
	OAPIFUNC const char *realpath_ns(const char *path);
	OAPIFUNC void create_directory(const char *path);
	OAPIFUNC void remove_all(const char *);
	OAPIFUNC void *LoadModule(const char *path);
	OAPIFUNC void AddOverlay(const char *path);
	OAPIFUNC void SetWritePath(const char *path);
	OAPIFUNC std::string GetWritePath();

	class OAPIFUNC ifstream : public std::ifstream
	{
	public:
		ifstream() {}
		ifstream(const char *path, std::ios_base::openmode mode = std::ios_base::in) {
			open(path, mode);
		}
		void open(const char *path, std::ios_base::openmode mode = std::ios_base::in);
	};

	class OAPIFUNC ofstream : public std::ofstream
	{
	public:
		ofstream() {}
		ofstream(const char *path, std::ios_base::openmode mode = std::ios_base::out) {
			open(path, mode);
		}
		void open(const char *path, std::ios_base::openmode mode = std::ios_base::out);
	};
};
