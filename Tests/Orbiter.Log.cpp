// Copyright (c) 2026 Douglas Beachy
// Licensed under the MIT License

// Crash tests for Orbiter's log writer (Src/Orbiter/Log.cpp)

#include <direct.h>
#include <filesystem>
#include <fstream>
#include <iterator>
#include <string>
#include <vector>

#include "Log.h"
#include "TimeData.h"

#undef min
#undef max

#include "catch2/catch_all.hpp"

// Log.cpp refers to these Orbiter globals
char DBG_MSG[256];
TimeData td;

namespace fs = std::filesystem;

static std::vector<std::string> s_console;

static void CaptureConsole(const char *msg)
{
	s_console.push_back(msg);
}

static std::string ReadLog(const fs::path &path)
{
	std::ifstream in(path);
	return std::string(std::istreambuf_iterator<char>(in), std::istreambuf_iterator<char>());
}

TEST_CASE("Text with printf codes is logged literally", "[log]")
{
	const fs::path path = fs::temp_directory_path() / "Orbiter.Log.test.log";
	InitLog(path.string().c_str(), false);

	char name[] = "Sound\\bad%s%n.wav";
	LogOut(name);
	LogOut_Error("func", "file.cpp", 1, name);
	LogOut_Warning("func", "file.cpp", 1, name);
	SetLogVerbosity(true);
	LogOutFine(name);
	SetLogVerbosity(false);
	LogOut("module %s, API %06d", "XRSound", 241030);   // with arguments it is still a format

	SetLastError(ERROR_BAD_EXE_FORMAT);   // the system text contains "%1"
	LogOut_LastError("func", "file.cpp", 1);
	SetLastError(0x20000001);             // no system text for this code
	LogOut_LastError("func", "file.cpp", 2);

	char cwd[512];
	REQUIRE(_getcwd(cwd, 512));
	REQUIRE(_chdir(fs::temp_directory_path().string().c_str()) == 0);
	CHECKCWD(cwd, "Modules\\bad%s%n.dll");
	char now[512];
	REQUIRE(_getcwd(now, 512));
	CHECK(std::string(now) == cwd);

	const std::string text = ReadLog(path);
	size_t count = 0;
	for (size_t pos = text.find(name); pos != std::string::npos; pos = text.find(name, pos + 1)) count++;
	CHECK(count == 4);
	CHECK(text.find("module XRSound, API 241030") != std::string::npos);
	CHECK(text.find("%1") != std::string::npos);
	CHECK(text.find("Unknown error") != std::string::npos);
	CHECK(text.find("CWD modified by module Modules\\bad%s%n.dll - Fixing.") != std::string::npos);

	std::error_code ec;
	fs::remove(path, ec);
}

TEST_CASE("Logging still works when the log file can't be opened", "[log]")
{
	const fs::path path = fs::temp_directory_path() / "Orbiter.Log.missing-folder" / "Orbiter.log";
	InitLog(path.string().c_str(), false);
	s_console.clear();
	SetLogOutFunc(&CaptureConsole);
	SetLogVerbosity(true);

	LogOut("line %d", 1);
	LogOutFine("line %d", 2);

	SetLogVerbosity(false);
	SetLogOutFunc(nullptr);
	CHECK_FALSE(fs::exists(path));
	REQUIRE(s_console.size() == 2);
	CHECK(s_console[0] == "line 1");
	CHECK(s_console[1] == "line 2");
}
