// Unit tests for Orbiter's log file writer (Src/Orbiter/Log.cpp).
//
// Each test case logs to a fresh file under the system temp directory and reads it back.  A message with no arguments must be
// written as plain text: a '%' in it (e.g., in an add-on's file name) used to be read as a printf format, which garbled the
// line, and "%s" or "%n" could crash Orbiter.  The console copy of each line (used when Orbiter runs without graphics) is
// checked through the log callback.

#include <algorithm>
#include <filesystem>
#include <fstream>
#include <iterator>
#include <sstream>
#include <direct.h>
#include <string>
#include <vector>
#include <process.h>

#include "Log.h"
#include "TimeData.h"

// these collide with std::min/max and Windows SDK macros
#undef min
#undef max

#include "catch2/catch_all.hpp"

// Log.cpp refers to these Orbiter globals; the functions under test never use them
char DBG_MSG[256];
TimeData td;

namespace {

namespace fs = std::filesystem;

std::vector<std::string> s_consoleLines;

void CaptureConsoleLine(const char *msg)
{
	s_consoleLines.push_back(msg);
}

// Points the log at a fresh file under the system temp directory, and on destruction detaches the console callback and deletes the file.
class TempLog
{
public:
	TempLog()
	{
		static int s_counter = 0;
		m_path = fs::temp_directory_path() / ("Orbiter.Log.test-" + std::to_string(_getpid()) + "-" + std::to_string(++s_counter) + ".log");
		InitLog(m_path.string().c_str(), false);
		s_consoleLines.clear();
		SetLogOutFunc(&CaptureConsoleLine);
	}

	~TempLog()
	{
		SetLogOutFunc(nullptr);
		SetLogVerbosity(false);
		std::error_code ec;
		fs::remove(m_path, ec);
	}

	// Returns the logged lines without their "0000000.000: " timestamps; the first line ("**** <file name>") is the InitLog header.
	std::vector<std::string> Lines() const
	{
		std::ifstream in(m_path);
		std::vector<std::string> lines;
		std::string line;
		while (std::getline(in, line))
		{
			const bool isHeader = (line.compare(0, 4, "****") == 0);
			const size_t colon = line.find(": ");     // the timestamp itself contains no ": "
			lines.push_back((!isHeader && (colon != std::string::npos)) ? line.substr(colon + 2) : line);
		}
		return lines;
	}

private:
	fs::path m_path;
};

bool Contains(const std::vector<std::string> &lines, const std::string &text)
{
	return (std::find(lines.begin(), lines.end(), text) != lines.end());
}

bool AnyLineContains(const std::vector<std::string> &lines, const std::string &text)
{
	return std::any_of(lines.begin(), lines.end(), [&text](const std::string &line) { return (line.find(text) != std::string::npos); });
}

} // namespace

TEST_CASE("A message with no arguments is written as plain text", "[log]")
{
	TempLog log;
	LogOut("100% done: Sound\\bad%s%n.wav and %d%%");

	const auto lines = log.Lines();
	CHECK(Contains(lines, "100% done: Sound\\bad%s%n.wav and %d%%"));
	REQUIRE(s_consoleLines.size() == 1);
	CHECK(s_consoleLines[0] == "100% done: Sound\\bad%s%n.wav and %d%%");
}

TEST_CASE("A message with arguments is still a printf format", "[log]")
{
	TempLog log;
	LogOut("module %s loaded, API %06d, %.1f%%", "XRSound", 241030, 99.5f);   // the float is promoted to double, as before

	CHECK(Contains(log.Lines(), "module XRSound loaded, API 241030, 99.5%"));
	REQUIRE(s_consoleLines.size() == 1);
	CHECK(s_consoleLines[0] == "module XRSound loaded, API 241030, 99.5%");    // the console copy formats the same arguments again
}

TEST_CASE("Error and warning messages with no arguments are plain text", "[log]")
{
	TempLog log;
	char vesselName[] = "GL-01 %s%n";      // e.g., a name from a scenario file, logged from a char buffer
	LogOut_Error("func", "file.cpp", 12, vesselName);
	LogOut_Warning("func", "file.cpp", 34, "CWD modified by module 100%s - Fixing.");

	const auto lines = log.Lines();
	CHECK(Contains(lines, "GL-01 %s%n"));
	CHECK(Contains(lines, "CWD modified by module 100%s - Fixing."));
	CHECK(Contains(lines, "[func | file.cpp | 12]"));
	CHECK(Contains(lines, "[func | file.cpp | 34]"));
}

TEST_CASE("Error and warning messages with arguments are still printf formats", "[log]")
{
	TempLog log;
	LogOut_Error("func", "file.cpp", 56, "File not found: %s", "Config\\100%.cfg");
	LogOut_Warning("func", "file.cpp", 78, "%d modules", 3);

	const auto lines = log.Lines();
	CHECK(Contains(lines, "File not found: Config\\100%.cfg"));
	CHECK(Contains(lines, "3 modules"));
}

TEST_CASE("The fine-detail log writes plain and formatted messages once each", "[log]")
{
	TempLog log;
	LogOutFine("hidden unless fine logging is on");
	SetLogVerbosity(true);
	LogOutFine("fine %s %d", "detail", 7);

	const auto lines = log.Lines();
	CHECK_FALSE(Contains(lines, "hidden unless fine logging is on"));
	CHECK(Contains(lines, "fine detail 7"));
	REQUIRE(s_consoleLines.size() == 1);
	CHECK(s_consoleLines[0] == "fine detail 7");
}

TEST_CASE("A Windows error message is logged as plain text", "[log]")
{
	TempLog log;
	// The system text for this error contains the unfilled inserts "%1" and "%2", which used to be read as a printf format.
	SetLastError(ERROR_MR_MID_NOT_FOUND);
	LogOut_LastError("func", "file.cpp", 7);

	const auto lines = log.Lines();
	CHECK(AnyLineContains(lines, "%1"));
	CHECK(Contains(lines, "[func | file.cpp | 7]"));
}

TEST_CASE("A DirectX error's source location is logged in full", "[log]")
{
	TempLog log;
	// __FILE__ is a full path, which used to be formatted into a 256-character buffer
	const std::string longFile = "C:\\" + std::string(300, 'd') + "\\100%s.cpp";
	LogOut_DDErr(E_FAIL, "func", longFile.c_str(), 9);
	LogOut_DIErr(E_FAIL, "func", longFile.c_str(), 10);

	const auto lines = log.Lines();
	CHECK(Contains(lines, ">>> [func | " + longFile + " | 9]"));
	CHECK(Contains(lines, ">>> [func | " + longFile + " | 10]"));
}

TEST_CASE("A log file name longer than 256 characters is used in full", "[log]")
{
	// The log file name used to be copied into a 256-character buffer without a length check.  A 258-character path overflowed
	// it, while still fitting Windows' 260-character path limit (longer paths need long-path support to open at all).
	const fs::path folder = fs::temp_directory_path() / ("Orbiter.Log.test-" + std::to_string(_getpid()) + "-longname");
	const int nameLength = 258 - (static_cast<int>(folder.string().size()) + 1) - 4;	// + 1 for the backslash, - 4 for ".log"
	REQUIRE(nameLength > 0);	// the temp folder's path leaves room for the name
	const fs::path longLog = folder / (std::string(static_cast<size_t>(nameLength), 'n') + ".log");
	REQUIRE(longLog.string().size() == 258);
	fs::create_directories(folder);
	InitLog(longLog.string().c_str(), false);
	LogOut("written to the long-named log");

	std::ifstream in(longLog);
	const std::string text((std::istreambuf_iterator<char>(in)), std::istreambuf_iterator<char>());
	in.close();
	CHECK(text.find("written to the long-named log") != std::string::npos);
	std::error_code ec;
	fs::remove_all(folder, ec);
}

TEST_CASE("A log file that cannot be opened is skipped, and the console still gets the line", "[log]")
{
	TempLog log;
	InitLog((fs::temp_directory_path() / "no-such-folder-for-Orbiter-Log-test" / "Orbiter.log").string().c_str(), false);
	LogOut("still shown on the console");      // used to crash writing to a null file handle

	REQUIRE(s_consoleLines.size() == 1);
	CHECK(s_consoleLines[0] == "still shown on the console");
}

TEST_CASE("The working-folder warning names the module literally", "[log]")
{
	TempLog log;
	const std::string original = fs::current_path().string();
	fs::current_path(fs::temp_directory_path());
	CHECKCWD(original.c_str(), "Mod%sule");

	CHECK(fs::current_path().string() == original);
	CHECK(Contains(log.Lines(), "CWD modified by module Mod%sule - Fixing."));
}

TEST_CASE("The console copy of a long line is complete", "[log]")
{
	TempLog log;
	const std::string longPath = "Modules\\" + std::string(2000, 'a') + ".dll";
	LogOut("loading %s", longPath.c_str());

	CHECK(Contains(log.Lines(), "loading " + longPath));
	REQUIRE(s_consoleLines.size() == 1);
	CHECK(s_consoleLines[0] == "loading " + longPath);     // the console copy used to stop at 255 characters
}
