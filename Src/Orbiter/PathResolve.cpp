// not upstream: Linux counterpart of Windows' case-insensitive, '\'-separated file name lookup

#define OAPI_IMPLEMENTATION
#include "OrbiterAPI.h"
#include <string>
#include <vector>
#include <strings.h>
#include <sys/stat.h>
#include <dirent.h>

static bool Exists (const std::string &p)
{
	struct stat st;
	return stat (p.c_str(), &st) == 0;
}

static std::string Join (const std::string &dir, const std::string &name)
{
	if (dir.empty()) return name;
	if (dir.back() == '/') return dir + name;
	return dir + '/' + name;
}

DLLEXPORT std::string oapiResolvePath (const char *path)
{
	std::string p (path ? path : "");
	for (char &c : p) if (c == '\\') c = '/';
	if (p.empty() || Exists (p)) return p;

	bool absolute = (p[0] == '/');
	bool trailing = (p.back() == '/');
	std::vector<std::string> comp;
	for (size_t i = 0, j; i < p.size(); i = j + 1) {
		j = p.find ('/', i);
		if (j == std::string::npos) j = p.size();
		if (j > i) comp.push_back (p.substr (i, j - i));
	}

	std::string cur = (absolute ? "/" : "");
	for (size_t k = 0; k < comp.size(); k++) {
		std::string cand = Join (cur, comp[k]);
		if (comp[k] == "." || comp[k] == ".." || Exists (cand)) { cur = cand; continue; }
		bool found = false;
		if (DIR *d = opendir (cur.empty() ? "." : cur.c_str())) {
			while (struct dirent *e = readdir (d)) {
				if (!strcasecmp (e->d_name, comp[k].c_str())) {
					cand = Join (cur, e->d_name);
					found = true;
					break;
				}
			}
			closedir (d);
		}
		if (!found) { // no match on disk: keep the rest as spelled (new file, or genuinely missing)
			for (; k < comp.size(); k++) cur = Join (cur, comp[k]);
			break;
		}
		cur = cand;
	}
	if (trailing && (cur.empty() || cur.back() != '/')) cur += '/';
	return cur;
}
