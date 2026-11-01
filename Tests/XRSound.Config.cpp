// Copyright (c) 2026 Douglas Beachy
// Licensed under the MIT License

// Crash tests for XRSound's config file parser (Sound/XRSound/src/Utils/ConfigFileParser.cpp)

#include <filesystem>
#include <fstream>
#include <iterator>
#include <string>
#include <vector>

#include "ConfigFileParser.h"

#undef min
#undef max

#include "catch2/catch_all.hpp"

namespace fs = std::filesystem;

class TestParser : public ConfigFileParser
{
public:
	TestParser(const char *pCfg, const char *pLog) : ConfigFileParser(pCfg, pLog) {}
	using ConfigFileParser::ValidateDouble;
	std::vector<std::string> names;

protected:
	bool ParseLine(const char *pSection, const char *pName, const char *pValue, const bool bParsingOverrideFile) override
	{
		names.push_back(pName);
		return true;
	}
};

static std::string ReadFile(const fs::path &path)
{
	std::ifstream in(path);
	return std::string(std::istreambuf_iterator<char>(in), std::istreambuf_iterator<char>());
}

TEST_CASE("Long config text is cut off in the log", "[xrsound]")
{
	const fs::path cfg = fs::temp_directory_path() / "XRSound.Config.test.cfg";
	const fs::path log = fs::temp_directory_path() / "XRSound.Config.test.log";
	std::error_code ec;
	fs::remove(log, ec);
	{
		std::ofstream out(cfg);
		out << "[SECTION]\n";
		out << "LongLine" << std::string(600, 'x') << "END\n";                   // no '='
		out << std::string(MAX_NAME_LENGTH, 'n') << " =1\n";                     // name one too long for its buffer
		out << "Value=" << std::string(MAX_VALUE_LENGTH, 'v') << "\n";           // value one too long for its buffer
		out << "Good = 1\n";
	}
	{
		TestParser parser(cfg.string().c_str(), log.string().c_str());
		CHECK_FALSE(parser.ParseFile());
		CHECK(parser.names == std::vector<std::string>{ "Good" });

		CHECK_FALSE(parser.ValidateDouble(1e300, 0.02, 1.0));
	}

	const std::string text = ReadFile(log);
	CHECK(text.find("Line='LongLine") != std::string::npos);
	CHECK(text.find("xxxEND") == std::string::npos);
	CHECK(text.find("name parameter too long") != std::string::npos);
	CHECK(text.find("value parameter too long") != std::string::npos);
	CHECK(text.find("Double value '1000000") != std::string::npos);

	fs::remove(cfg, ec);
	fs::remove(log, ec);
}
