// not upstream: unit tests for Src/Orbiter/PathResolve (Windows file name semantics on Linux)
#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <fstream>
#include <unistd.h>
#include "OrbiterAPI.h"

namespace fs = std::filesystem;

struct TmpTree {
	fs::path root, old;
	TmpTree () {
		char tmpl[] = "/tmp/ob_resolve_XXXXXX";
		root = mkdtemp (tmpl);
		fs::create_directories (root / "Config" / "Vessels");
		fs::create_directories (root / "Scenarios" / "Quicksave");
		std::ofstream (root / "Config" / "Earth.cfg") << "x\n";
		std::ofstream (root / "Config" / "Vessels" / "DeltaGlider.cfg") << "x\n";
		old = fs::current_path ();
		fs::current_path (root);
	}
	~TmpTree () { fs::current_path (old); fs::remove_all (root); }
};

TEST_CASE("Backslashes and case are resolved against the disk", "[resolve]")
{
	TmpTree t;
	REQUIRE(oapiResolvePath ("Config\\Earth.cfg") == "Config/Earth.cfg");
	REQUIRE(oapiResolvePath ("CONFIG\\earth.CFG") == "Config/Earth.cfg");
	REQUIRE(oapiResolvePath (".\\config\\vessels\\deltaglider.cfg") == "./Config/Vessels/DeltaGlider.cfg");
	REQUIRE(oapiResolvePath ((t.root.string() + "\\config\\EARTH.cfg").c_str()) == (t.root / "Config" / "Earth.cfg").string());
}

TEST_CASE("Unmatched tails are kept for files to be created", "[resolve]")
{
	TmpTree t;
	REQUIRE(oapiResolvePath ("scenarios\\quicksave\\New Save.scn") == "Scenarios/Quicksave/New Save.scn");
	REQUIRE(oapiResolvePath ("Scenarios\\Missing\\a.scn") == "Scenarios/Missing/a.scn");
	REQUIRE(oapiResolvePath ("scenarios\\") == "Scenarios/");
	REQUIRE(oapiResolvePath ("") == "");
}
