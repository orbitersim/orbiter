// Copyright (c) 2026 Douglas Beachy
// Licensed under the MIT License

// Tests for reading config file items (Src/Orbiter/Config.cpp)

#include <sstream>
#include <string>

#include "Config.h"

#undef min
#undef max

#include "catch2/catch_all.hpp"

TEST_CASE("A long config value is cut off to the caller's buffer", "[config]")
{
	const std::string value(400, 'x');
	std::istringstream is("Module = " + value + "\n");

	char buf[256];
	REQUIRE(GetItemString(is, "Module", buf, sizeof(buf)));
	CHECK(std::string(buf) == value.substr(0, sizeof(buf) - 1));
}

TEST_CASE("Items after an over-long line are still found", "[config]")
{
	std::istringstream is("Help = " + std::string(600, 'h') + "\nModule = Planet\n");

	char buf[256];
	REQUIRE(GetItemString(is, "Module", buf, sizeof(buf)));
	CHECK(std::string(buf) == "Planet");
}

TEST_CASE("An over-long last line with no newline ends the search cleanly", "[config]")
{
	std::istringstream is("Module = Planet\nHelp = " + std::string(600, 'h'));

	char buf[256];
	CHECK_FALSE(GetItemString(is, "Name", buf, sizeof(buf)));
	REQUIRE(GetItemString(is, "Module", buf, sizeof(buf)));
	CHECK(std::string(buf) == "Planet");
}

TEST_CASE("A short config value is read whole", "[config]")
{
	std::istringstream is("Name = Earth\nModule = Planet\n");

	char buf[512];   // the three-argument GetItemString needs 512
	REQUIRE(GetItemString(is, "Module", buf, sizeof(buf)));
	CHECK(std::string(buf) == "Planet");
	REQUIRE(GetItemString(is, "Name", buf));
	CHECK(std::string(buf) == "Earth");
}
