#define OAPI_IMPLEMENTATION

#include "VFS.h"
#include <filesystem>
#include <list>
#include <string>
#include <set>

static std::list<std::filesystem::path> s_overlays = { "./" };
//static std::list<std::string> s_whiteouts;
static std::filesystem::path s_writePath;

// Get the real path of a file, given a virtual path
// When opening a file for modification, we need to copy if to the write
// directory if it's not already there
static std::string GetRealPath(const char *path, bool modify = false)
{
	if(s_writePath.empty()) modify = false;

	std::error_code ec;
	// First check in the write path
	auto wpath = s_writePath / path;
	if(std::filesystem::exists(wpath, ec)) {
		return wpath.string();
	}

	// The check in the different overlays
	for(auto &&overlay: s_overlays) {
		auto rpath = overlay / path;
		if(std::filesystem::exists(rpath, ec)) {
			if(modify) {
				std::filesystem::create_directories(wpath.parent_path(), ec);
				// Copy if it's a file
				if(std::filesystem::is_regular_file(rpath)) {
					std::filesystem::copy(rpath, wpath, ec);
				}
				return wpath.string();
			} else {
				return rpath.string();
			}
		}
	}

	// Not found - return the write path in case we want to create the entry
	if(modify) {
		std::filesystem::create_directories(wpath.parent_path(), ec);
	}
	return wpath.string();
}

static const char *last_separator(const char *path)
{
	const char *backslash = strrchr(path, '\\');
	const char *slash = strrchr(path, '/');

	const char *ret = NULL;
	if(backslash > ret) ret = backslash;
	if(slash > ret) ret = slash;
	return ret;
}


namespace VFS
{
	DLLEXPORT void AddOverlay(const char *path)
	{
		s_overlays.push_back(path);

		char modpath[MAX_PATH];
		strcpy(modpath, path);
		strcat(modpath, "\\Modules");
		char *ppath = getenv ("PATH");
		char *cbuf = new char[strlen(ppath)+strlen(modpath)+16];
		if (ppath) {
			sprintf (cbuf, "PATH=%s;%s", ppath, modpath);
		} else {
			sprintf (cbuf, "PATH=%s", modpath);
		}
		_putenv (cbuf);
		delete []cbuf;
	}

	DLLEXPORT void SetWritePath(const char *path)
	{
		s_writePath = path;

		char modpath[MAX_PATH];
		strcpy(modpath, path);
		strcat(modpath, "\\Modules");
		char *ppath = getenv ("PATH");
		char *cbuf = new char[strlen(ppath)+strlen(modpath)+16];
		if (ppath) {
			sprintf (cbuf, "PATH=%s;%s", ppath, modpath);
		} else {
			sprintf (cbuf, "PATH=%s", modpath);
		}
		_putenv (cbuf);
		delete []cbuf;
	}
	
	DLLEXPORT std::string GetWritePath()
	{
		return s_writePath.string();
	}

	void ifstream::open(const char *path, std::ios_base::openmode mode)
	{
		std::string rpath = GetRealPath(path);
		//printf("ifstream f=%s -> %s\n", path, rpath.c_str());
		//fflush(stdout);
		std::ifstream::open(rpath, mode);
	}
	void ofstream::open(const char *path, std::ios_base::openmode mode)
	{
		std::string rpath = GetRealPath(path, true);
		//printf("ofstream f=%s -> %s\n",path, rpath.c_str());
		//fflush(stdout);
		std::ofstream::open(rpath, mode);
	}

	DLLEXPORT FILE *fopen(const char * path, const char *mode)
	{
		bool modify = true;
		// Only modes "r", "rb" and "rt" don't imply file modification
		if(mode[0] == 'r' && (mode[1] == 'b' || mode[1] == 't' || mode[1] == '\0'))
			modify = false;

		std::string rpath = GetRealPath(path, modify);
		//printf("mode=%s f=%s -> %s\n",mode, path, rpath.c_str());
		//fflush(stdout);
		FILE *ret;
#ifdef _WIN32
//		if (fopen_s(&ret, rpath.c_str(), mode)) return NULL;
		// Open file without exclusive access so we can e.g.
		// open Orbiter.log while the program is running
		ret = _fsopen(rpath.c_str(), mode, _SH_DENYNO );
#else
		ret =  ::fopen(path, mode);
#endif
		return ret;
	}

	DLLEXPORT bool is_directory(const char *path)
	{
		std::string rpath = GetRealPath(path);
		std::error_code ec;
		return std::filesystem::is_directory(rpath, ec );
	}

	DLLEXPORT bool exists(const char *path)
	{
		std::string rpath = GetRealPath(path);
		std::error_code ec;
		return std::filesystem::exists(rpath, ec );
	}

	DLLEXPORT void enumerate(const char *dir, std::function<void(const char *)> callback)
	{
		std::error_code ec;
		std::set<std::string> mergednames;
		std::string sdir = std::string(dir) + "/";

		auto wpath = s_writePath / dir;
		for (const auto& entry : std::filesystem::directory_iterator(wpath, ec)) {
			mergednames.insert(sdir + entry.path().filename().string());
		}

		for(auto &&overlay: s_overlays) {
			auto rpath = overlay / dir;

			for (const auto& entry : std::filesystem::directory_iterator(rpath, ec)) {
				mergednames.insert(sdir + entry.path().filename().string());
			}
		}

		for(auto &&name: mergednames) {
			callback(name.c_str());
		}
	}

	DLLEXPORT void create_directory(const char *path)
	{
		std::string rpath = GetRealPath(path, true);
		std::filesystem::create_directory(rpath);
	}

	DLLEXPORT void *LoadModule(const char *path) {
		std::string rpath = GetRealPath(path);
		//printf("LoadModules f=%s -> %s\n", path, rpath.c_str());
		//fflush(stdout);

		return LoadLibrary(rpath.c_str());
	}

	DLLEXPORT void remove_all(const char *path)
	{
		std::string rpath = GetRealPath(path, true);
		std::filesystem::remove_all(rpath);
	}

	DLLEXPORT bool is_regular_file(const char *path)
	{
		std::string rpath = GetRealPath(path);
		std::error_code ec;
		return std::filesystem::is_regular_file(rpath, ec);
	}


	DLLEXPORT const char *dirname(const char *path, char *dst)
	{
		strcpy(dst, path);

		char *lsep = (char *)last_separator(dst); // safe because dst is non const

		if(!lsep) {
			lsep = dst;
		}

		*lsep = '\0';

		return dst;
	}

	DLLEXPORT const char *stem(const char *path, char *dst)
	{
		strcpy(dst, basename(path));

		char *fext = strrchr(dst, '.');
		if(fext) {
			*fext = '\0';
		}
		return dst;
	}

	DLLEXPORT bool has_extension(const char *path, const char *extension)
	{
		const char *fext = strrchr(path, '.');
		if(fext) {
			return !strcmp(fext + 1, extension);
		}
		return *extension == '\0';
	}

	DLLEXPORT const char *basename(const char *path)
	{
		const char *lsep = last_separator(path);
		if(!lsep)
			return path;
		return lsep + 1;
	}

	DLLEXPORT const char *realpath(const char *path, char *dst)
	{
		std::string rpath = GetRealPath(path);
		strcpy(dst, rpath.c_str());
		return dst;
	}

	DLLEXPORT const char *realpath_ns(const char *path)
	{
		static char rpath[MAX_PATH];
		return realpath(path, rpath);
	}

};
