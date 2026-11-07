// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include "Help.h"
#include "OrbiterPlatform.h"
#include <filesystem>
#include <fstream>
#include <regex>
#include <string>

namespace fs = std::filesystem;

namespace {

std::string Normalise (const char *p)
{
	std::string s = p ? p : "";
	for (char &c : s) if (c == '\\') c = '/';
	return s;
}

fs::path FindCaseInsensitive (const fs::path &p)
{
	std::error_code ec;
	if (fs::exists (p, ec)) return p;
	fs::path parent = p.parent_path ();
	if (parent.empty ()) parent = ".";
	fs::path dir = FindCaseInsensitive (parent);
	for (auto &e : fs::directory_iterator (dir, ec)) {
		std::string a = e.path ().filename ().string (), b = p.filename ().string ();
		if (a.size () == b.size () && strncasecmp (a.c_str (), b.c_str (), a.size ()) == 0)
			return e.path ();
	}
	return p;
}

std::string HhpValue (const fs::path &hhp, const char *key)
{
	std::ifstream ifs (hhp);
	std::string line, k = std::string (key) + "=";
	while (std::getline (ifs, line)) {
		if (!line.empty () && line.back () == '\r') line.pop_back ();
		if (line.compare (0, k.size (), k) == 0) return line.substr (k.size ());
	}
	return "";
}

}

void OpenHelp (const char *file, const char *topic)
{
	std::string f = Normalise (file);
	std::string t = Normalise (topic);
	size_t sep = f.find ("::");
	if (sep != std::string::npos) {
		if (t.empty ()) t = f.substr (sep + 2);
		f = f.substr (0, sep);
	}
	if (f.size () > 4 && strcasecmp (f.c_str () + f.size () - 4, ".chm") == 0)
		f = f.substr (0, f.size () - 4);
	fs::path dir = FindCaseInsensitive (fs::path (f));

	while (!t.empty () && t[0] == '/') t.erase (0, 1);
	if (!t.empty ()) {
		std::string page = t, anchor;
		size_t hash = page.find ('#');
		if (hash != std::string::npos) { anchor = page.substr (hash); page = page.substr (0, hash); }
		if (page.find ('.') == std::string::npos) page += ".htm";
		t = page + anchor;
	}

	std::string toc, title = dir.filename ().string ();
	std::error_code ec;
	for (auto &e : fs::directory_iterator (dir, ec)) {
		if (strcasecmp (e.path ().extension ().string ().c_str (), ".hhp") != 0) continue;
		std::string c = HhpValue (e.path (), "Contents file");
		std::string ttl = HhpValue (e.path (), "Title");
		std::string def = HhpValue (e.path (), "Default topic");
		if (!c.empty ()) toc = (dir / c).string ();
		if (!ttl.empty ()) title = ttl;
		if (t.empty ()) t = Normalise (def.c_str ());
		break;
	}
	if (t.empty ()) t = "index.htm";

	oplat::ShowHelp (dir.string (), t, toc, title);
}

void OpenDefaultHelp (const char *topic)
{
	OpenHelp ("Html/orbiter.chm", topic);
}
