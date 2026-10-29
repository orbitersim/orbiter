// Unit tests for XRSound's configuration file reader (XRSoundConfigFileParser, built on ConfigFileParser).
//
// These tests never touch the audio engine, so they need no sound card and no running Orbiter.  Every .cfg file
// (and the one small "wav" file) is created at runtime in a fresh folder under the system temp directory, and
// the folder is deleted when each test case ends.
//
// The parser hard-codes its log file name ("XRSound.log", relative to the working directory) and resolves the
// sound file paths in an [animation_N] section relative to the working directory, too.  So each test case
// makes its temp folder the working directory for its lifetime; that redirects the parser's log into the
// temp folder and lets the .cfg files and the wav path be given as plain relative paths.

#include <chrono>
#include <cmath>
#include <filesystem>
#include <fstream>
#include <iterator>
#include <sstream>
#include <string>
#include <process.h>

#include "XRSoundConfigFileParser.h"

// these collide with std::min/max and Windows SDK macros
#undef min
#undef max
#undef small
#undef far
#undef near

#include "catch2/catch_all.hpp"

namespace {

namespace fs = std::filesystem;

// Creates a fresh folder under the system temp directory, makes it the working directory, and on destruction
// restores the previous working directory and deletes the folder.
// Declare it BEFORE any XRSoundConfigFileParser in a test case: the parser holds XRSound.log open, and the folder
// cannot be deleted until the parser has been destroyed (locals are destroyed in reverse order of declaration).
class TempConfigFolder
{
public:
    TempConfigFolder() : m_previousDir(fs::current_path())
    {
        const auto ticks = std::chrono::steady_clock::now().time_since_epoch().count();
        m_dir = fs::temp_directory_path() / ("XRSoundConfigTest-" + std::to_string(_getpid()) + "-" + std::to_string(ticks));
        fs::create_directories(m_dir);
        fs::current_path(m_dir);
    }

    ~TempConfigFolder()
    {
        std::error_code ec;
        fs::current_path(m_previousDir, ec);
        fs::remove_all(m_dir, ec);
    }

    TempConfigFolder(const TempConfigFolder &) = delete;
    TempConfigFolder &operator=(const TempConfigFolder &) = delete;

    // Writes a file (creating any subfolders) relative to the temp folder.
    void Write(const std::string &relativePath, const std::string &content) const
    {
        const fs::path path = m_dir / relativePath;
        if (path.has_parent_path())
            fs::create_directories(path.parent_path());
        std::ofstream out(path, std::ios::binary);
        out << content;
    }

    // Returns the whole parser log (XRSound.log in the temp folder).
    std::string ReadLog() const
    {
        std::ifstream in(m_dir / XRSOUND_LOG_FILE, std::ios::binary);
        return std::string(std::istreambuf_iterator<char>(in), std::istreambuf_iterator<char>());
    }

private:
    fs::path m_previousDir;
    fs::path m_dir;
};

const char *GLOBAL_CFG = "XRSound.cfg";
const char *OVERRIDE_CFG = "XRSound-Test.cfg";

// Builds the text of an override file holding one [animation_N] section with a single "Volume" line.
std::string AnimationVolumeCfg(const std::string &volumeText, const int animationID = 3)
{
    return "[animation_" + std::to_string(animationID) + "]\nVolume = " + volumeText + "\n";
}

}  // namespace

TEST_CASE("Animation Volume in an override file is read", "[xrsound][config][animation]")
{
    TempConfigFolder folder;
    folder.Write(GLOBAL_CFG, "[SYSTEM]\nMasterVolume = 1.0\n");
    folder.Write(OVERRIDE_CFG, AnimationVolumeCfg("0.3"));

    XRSoundConfigFileParser parser(GLOBAL_CFG);
    CHECK(parser.ParseFile(GLOBAL_CFG));
    CHECK(parser.ParseFile(OVERRIDE_CFG));
    CHECK_FALSE(parser.ParseFailed());

    const AnimationSounds *pAnimation = parser.GetAnimationSounds(3);
    REQUIRE(pAnimation != nullptr);
    CHECK(pAnimation->GetVolume() == Catch::Approx(0.3f));
    CHECK(folder.ReadLog().find("animation ID 3 volume = 0.300") != std::string::npos);
}

TEST_CASE("Animation section without a Volume line defaults to 1.0", "[xrsound][config][animation]")
{
    TempConfigFolder folder;
    folder.Write(GLOBAL_CFG, "[SYSTEM]\nMasterVolume = 1.0\n");
    // PlaybackType is a valid animation key that needs no sound file
    folder.Write(OVERRIDE_CFG, "[animation_5]\nPlaybackType = BothViewFar\n");

    XRSoundConfigFileParser parser(GLOBAL_CFG);
    CHECK(parser.ParseFile(GLOBAL_CFG));
    CHECK(parser.ParseFile(OVERRIDE_CFG));
    CHECK_FALSE(parser.ParseFailed());

    const AnimationSounds *pAnimation = parser.GetAnimationSounds(5);
    REQUIRE(pAnimation != nullptr);
    CHECK(pAnimation->GetVolume() == 1.0f);
    CHECK(pAnimation->GetPlaybackType() == XRSound::PlaybackType::BothViewFar);
}

TEST_CASE("Animation Volume boundaries 0 and 1.0 are accepted", "[xrsound][config][animation]")
{
    TempConfigFolder folder;
    folder.Write(GLOBAL_CFG, "[SYSTEM]\nMasterVolume = 1.0\n");
    // one section per boundary, so each value is read independently
    folder.Write(OVERRIDE_CFG, AnimationVolumeCfg("0", 1) + AnimationVolumeCfg("1.0", 2));

    XRSoundConfigFileParser parser(GLOBAL_CFG);
    CHECK(parser.ParseFile(GLOBAL_CFG));
    CHECK(parser.ParseFile(OVERRIDE_CFG));
    CHECK_FALSE(parser.ParseFailed());

    REQUIRE(parser.GetAnimationSounds(1) != nullptr);
    REQUIRE(parser.GetAnimationSounds(2) != nullptr);
    CHECK(parser.GetAnimationSounds(1)->GetVolume() == 0.0f);
    CHECK(parser.GetAnimationSounds(2)->GetVolume() == 1.0f);
}

TEST_CASE("Invalid animation Volume is rejected and the volume stays 1.0", "[xrsound][config][animation]")
{
    const std::string invalidValue = GENERATE(as<std::string>{}, "2", "-0.1", "abc", "nan");
    CAPTURE(invalidValue);

    TempConfigFolder folder;
    folder.Write(GLOBAL_CFG, "[SYSTEM]\nMasterVolume = 1.0\n");
    folder.Write(OVERRIDE_CFG, AnimationVolumeCfg(invalidValue));

    XRSoundConfigFileParser parser(GLOBAL_CFG);
    CHECK(parser.ParseFile(GLOBAL_CFG));
    CHECK_FALSE(parser.ParseFailed());      // the global file is fine, so only the override file can fail

    CHECK_FALSE(parser.ParseFile(OVERRIDE_CFG));
    CHECK(parser.ParseFailed());

    const AnimationSounds *pAnimation = parser.GetAnimationSounds(3);
    REQUIRE(pAnimation != nullptr);         // the section was created; only the bad value was refused
    CHECK(pAnimation->GetVolume() == 1.0f);
    CHECK(folder.ReadLog().find("ERROR: Invalid Volume '" + invalidValue + "'") != std::string::npos);
}

TEST_CASE("Invalid animation Volume overrides an earlier valid Volume with the default", "[xrsound][config][animation]")
{
    TempConfigFolder folder;
    folder.Write(GLOBAL_CFG, "[SYSTEM]\nMasterVolume = 1.0\n");
    folder.Write(OVERRIDE_CFG, "[animation_3]\nVolume = 0.3\nVolume = 5\n");

    XRSoundConfigFileParser parser(GLOBAL_CFG);
    CHECK(parser.ParseFile(GLOBAL_CFG));
    CHECK_FALSE(parser.ParseFile(OVERRIDE_CFG));
    CHECK(parser.ParseFailed());

    REQUIRE(parser.GetAnimationSounds(3) != nullptr);
    CHECK(parser.GetAnimationSounds(3)->GetVolume() == 1.0f);
}

TEST_CASE("An [animation_N] section in the global file is refused", "[xrsound][config][animation]")
{
    TempConfigFolder folder;
    folder.Write(GLOBAL_CFG, "[animation_3]\nVolume = 0.3\n");

    XRSoundConfigFileParser parser(GLOBAL_CFG);

    // the line is refused with a warning, so the parse as a whole is reported as failed
    CHECK_FALSE(parser.ParseFile());
    CHECK(parser.ParseFailed());

    CHECK(parser.GetAnimationSounds(3) == nullptr);
    CHECK(parser.GetAllAnimationSounds().empty());
    CHECK(folder.ReadLog().find("[animation_*] properties are inherently vessel class-specific") != std::string::npos);
}

TEST_CASE("Animation wav paths must be readable files", "[xrsound][config][animation]")
{
    TempConfigFolder folder;
    folder.Write(GLOBAL_CFG, "[SYSTEM]\nMasterVolume = 1.0\n");
    folder.Write("sounds/gear.wav", "RIFF");    // contents don't matter: the parser only checks that the file can be opened

    SECTION("readable file is accepted")
    {
        folder.Write(OVERRIDE_CFG, "[animation_2]\nOpenCloseSoundID = 7\nOpening = sounds/gear.wav\n");

        XRSoundConfigFileParser parser(GLOBAL_CFG);
        CHECK(parser.ParseFile(GLOBAL_CFG));
        CHECK(parser.ParseFile(OVERRIDE_CFG));
        CHECK_FALSE(parser.ParseFailed());

        const AnimationSounds *pAnimation = parser.GetAnimationSounds(2);
        REQUIRE(pAnimation != nullptr);
        CHECK(pAnimation->GetOpenCloseSoundID() == 7);
        REQUIRE(pAnimation->GetWavForAnimationState(AnimationState::StateType::Opening) != nullptr);
        CHECK(std::string(pAnimation->GetWavForAnimationState(AnimationState::StateType::Opening)) == "sounds/gear.wav");
    }

    SECTION("missing file is rejected and no sound is stored")
    {
        folder.Write(OVERRIDE_CFG, "[animation_2]\nOpenCloseSoundID = 7\nOpening = sounds/missing.wav\n");

        XRSoundConfigFileParser parser(GLOBAL_CFG);
        CHECK(parser.ParseFile(GLOBAL_CFG));
        CHECK_FALSE(parser.ParseFile(OVERRIDE_CFG));
        CHECK(parser.ParseFailed());

        const AnimationSounds *pAnimation = parser.GetAnimationSounds(2);
        REQUIRE(pAnimation != nullptr);
        CHECK(pAnimation->GetWavForAnimationState(AnimationState::StateType::Opening) == nullptr);
        CHECK(folder.ReadLog().find("Sound file does not exist or is not readable: 'sounds/missing.wav'") != std::string::npos);
    }
}

TEST_CASE("MasterVolume: valid value accepted, out-of-range rejected and reset to the default", "[xrsound][config][system]")
{
    TempConfigFolder folder;

    SECTION("valid value")
    {
        folder.Write(GLOBAL_CFG, "[SYSTEM]\nMasterVolume = 0.4\n");
        XRSoundConfigFileParser parser(GLOBAL_CFG);
        CHECK(parser.MasterVolume == XRSoundConfigFileParser::MASTER_VOLUME_DEFAULT);   // before parsing
        CHECK(parser.ParseFile());
        CHECK_FALSE(parser.ParseFailed());
        CHECK(parser.MasterVolume == Catch::Approx(0.4f));
    }

    SECTION("minimum")
    {
        folder.Write(GLOBAL_CFG, "[SYSTEM]\nMasterVolume = 0\n");
        XRSoundConfigFileParser parser(GLOBAL_CFG);
        CHECK(parser.ParseFile());
        CHECK(parser.MasterVolume == XRSoundConfigFileParser::MASTER_VOLUME_MIN);
    }

    SECTION("maximum")
    {
        // start from a non-default value so reading the maximum is distinguishable from a reset to the default
        folder.Write(GLOBAL_CFG, "[SYSTEM]\nMasterVolume = 0.4\nMasterVolume = 1.0\n");
        XRSoundConfigFileParser parser(GLOBAL_CFG);
        CHECK(parser.ParseFile());
        CHECK_FALSE(parser.ParseFailed());
        CHECK(parser.MasterVolume == XRSoundConfigFileParser::MASTER_VOLUME_MAX);
    }

    SECTION("out-of-range value is rejected: VALIDATE_FLOAT resets the field to the default (it does not clamp)")
    {
        // set a non-default valid value first, so a "reset to default" is distinguishable from "kept the old value"
        folder.Write(GLOBAL_CFG, "[SYSTEM]\nMasterVolume = 0.4\nMasterVolume = 7\n");
        XRSoundConfigFileParser parser(GLOBAL_CFG);
        CHECK_FALSE(parser.ParseFile());
        CHECK(parser.ParseFailed());
        CHECK(parser.MasterVolume == XRSoundConfigFileParser::MASTER_VOLUME_DEFAULT);
        CHECK(folder.ReadLog().find("Float value '7.000000' is above maximum value of '1.000000'") != std::string::npos);
    }

    SECTION("negative value is rejected")
    {
        folder.Write(GLOBAL_CFG, "[SYSTEM]\nMasterVolume = -0.5\n");
        XRSoundConfigFileParser parser(GLOBAL_CFG);
        CHECK_FALSE(parser.ParseFile());
        CHECK(parser.ParseFailed());
        CHECK(parser.MasterVolume == XRSoundConfigFileParser::MASTER_VOLUME_DEFAULT);
    }
}

TEST_CASE("UpdateInterval: valid value accepted, out-of-range rejected and reset to the default", "[xrsound][config][system]")
{
    TempConfigFolder folder;

    SECTION("valid value")
    {
        folder.Write(GLOBAL_CFG, "[SYSTEM]\nUpdateInterval = 0.1\n");
        XRSoundConfigFileParser parser(GLOBAL_CFG);
        CHECK(parser.UpdateInterval == XRSoundConfigFileParser::UPDATE_INTERVAL_DEFAULT);
        CHECK(parser.ParseFile());
        CHECK_FALSE(parser.ParseFailed());
        CHECK(parser.UpdateInterval == Catch::Approx(0.1));
    }

    SECTION("boundaries are accepted")
    {
        const char *pBoundary = GENERATE("0.02", "1.0");
        CAPTURE(pBoundary);
        folder.Write(GLOBAL_CFG, std::string("[SYSTEM]\nUpdateInterval = ") + pBoundary + "\n");
        XRSoundConfigFileParser parser(GLOBAL_CFG);
        CHECK(parser.ParseFile());
        CHECK_FALSE(parser.ParseFailed());
        CHECK(parser.UpdateInterval == Catch::Approx(std::stod(pBoundary)));
    }

    SECTION("below the minimum")
    {
        folder.Write(GLOBAL_CFG, "[SYSTEM]\nUpdateInterval = 0.1\nUpdateInterval = 0.001\n");
        XRSoundConfigFileParser parser(GLOBAL_CFG);
        CHECK_FALSE(parser.ParseFile());
        CHECK(parser.ParseFailed());
        CHECK(parser.UpdateInterval == XRSoundConfigFileParser::UPDATE_INTERVAL_DEFAULT);
    }

    SECTION("above the maximum")
    {
        folder.Write(GLOBAL_CFG, "[SYSTEM]\nUpdateInterval = 0.1\nUpdateInterval = 5\n");
        XRSoundConfigFileParser parser(GLOBAL_CFG);
        CHECK_FALSE(parser.ParseFile());
        CHECK(parser.ParseFailed());
        CHECK(parser.UpdateInterval == XRSoundConfigFileParser::UPDATE_INTERVAL_DEFAULT);
    }

    SECTION("non-numeric value")
    {
        folder.Write(GLOBAL_CFG, "[SYSTEM]\nUpdateInterval = fast\n");
        XRSoundConfigFileParser parser(GLOBAL_CFG);
        CHECK_FALSE(parser.ParseFile());
        CHECK(parser.ParseFailed());
        CHECK(parser.UpdateInterval == XRSoundConfigFileParser::UPDATE_INTERVAL_DEFAULT);
    }
}

TEST_CASE("A value in the override file wins over the global file", "[xrsound][config][override]")
{
    TempConfigFolder folder;
    folder.Write(GLOBAL_CFG,
        "[ATC]\nVolume = 0.5\nMinDelay = 20\n"
        "[SOUND_FILES]\nCrash = Default/Crash.flac\nMetalCrunch = Default/Crunch.flac\n");
    folder.Write(OVERRIDE_CFG,
        "[ATC]\nVolume = 0.25\n"
        "[SOUND_FILES]\nCrash = Custom/Boom.flac\n");

    XRSoundConfigFileParser parser(GLOBAL_CFG);
    CHECK(parser.ParseFile(GLOBAL_CFG));
    CHECK(parser.ATCVolume == Catch::Approx(0.5f));
    CHECK(parser.Crash == "Default/Crash.flac");

    CHECK(parser.ParseFile(OVERRIDE_CFG));
    CHECK_FALSE(parser.ParseFailed());

    // overridden keys changed...
    CHECK(parser.ATCVolume == Catch::Approx(0.25f));
    CHECK(parser.Crash == "Custom/Boom.flac");
    // ...and keys the override file does not mention keep the global value
    CHECK(parser.ATCMinDelay == 20);
    CHECK(parser.MetalCrunch == "Default/Crunch.flac");
}

TEST_CASE("[SYSTEM] settings cannot be overridden by a vessel class override file", "[xrsound][config][override]")
{
    TempConfigFolder folder;
    folder.Write(GLOBAL_CFG, "[SYSTEM]\nMasterVolume = 0.6\n");
    folder.Write(OVERRIDE_CFG, "[SYSTEM]\nMasterVolume = 0.2\n");

    XRSoundConfigFileParser parser(GLOBAL_CFG);
    CHECK(parser.ParseFile(GLOBAL_CFG));
    CHECK_FALSE(parser.ParseFile(OVERRIDE_CFG));
    CHECK(parser.ParseFailed());
    CHECK(parser.MasterVolume == Catch::Approx(0.6f));
    CHECK(folder.ReadLog().find("cannot override [SYSTEM] settings via a vessel class override file") != std::string::npos);
}

TEST_CASE("ParseFailed reports only the current parse cycle", "[xrsound][config][override]")
{
    TempConfigFolder folder;
    folder.Write(OVERRIDE_CFG, AnimationVolumeCfg("0.3"));

    SECTION("a global file fixed since the last parse no longer reports failure")
    {
        XRSoundConfigFileParser parser(GLOBAL_CFG);
        folder.Write(GLOBAL_CFG, "[SYSTEM]\nthis line has no equals sign\n");
        CHECK_FALSE(parser.ParseFile(GLOBAL_CFG));
        CHECK(parser.ParseFailed());

        folder.Write(GLOBAL_CFG, "[SYSTEM]\nMasterVolume = 1.0\n");
        CHECK(parser.ParseFile(GLOBAL_CFG));
        CHECK_FALSE(parser.ParseFailed());
    }

    SECTION("a good override file does not hide a failure in the global file of the same cycle")
    {
        XRSoundConfigFileParser parser(GLOBAL_CFG);
        folder.Write(GLOBAL_CFG, "[SYSTEM]\nthis line has no equals sign\n");
        CHECK_FALSE(parser.ParseFile(GLOBAL_CFG));
        CHECK(parser.ParseFile(OVERRIDE_CFG));
        CHECK(parser.ParseFailed());
    }
}

TEST_CASE("Music order and play enums map to the public enums and back", "[xrsound][config][mapping]")
{
    using Parser = XRSoundConfigFileParser;

    SECTION("MusicOrder round trip")
    {
        for (const auto publicValue : { XRSound::MusicOrder::Sequential, XRSound::MusicOrder::Random })
        {
            Parser::SeqRandom internalValue = Parser::SeqRandom::Sequential;
            if (publicValue == XRSound::MusicOrder::Sequential)
                internalValue = Parser::SeqRandom::Random;      // start from the opposite value so the call must write it
            CHECK(Parser::MusicOrderToInternal(static_cast<int>(publicValue), internalValue));
            CHECK(Parser::MusicOrderToPublic(internalValue) == static_cast<int>(publicValue));
        }

        // direct mapping (a swap applied in both directions would still round-trip)
        Parser::SeqRandom order = Parser::SeqRandom::Random;
        CHECK(Parser::MusicOrderToInternal(static_cast<int>(XRSound::MusicOrder::Sequential), order));
        CHECK(order == Parser::SeqRandom::Sequential);
        CHECK(Parser::MusicOrderToInternal(static_cast<int>(XRSound::MusicOrder::Random), order));
        CHECK(order == Parser::SeqRandom::Random);

        Parser::SeqRandom unchanged = Parser::SeqRandom::Random;
        CHECK_FALSE(Parser::MusicOrderToInternal(99, unchanged));
        CHECK(unchanged == Parser::SeqRandom::Random);
    }

    SECTION("MusicPlay round trip")
    {
        for (const auto publicValue : { XRSound::MusicPlay::Off, XRSound::MusicPlay::Space, XRSound::MusicPlay::On })
        {
            Parser::MusicPlay internalValue = Parser::MusicPlay::Off;
            if (publicValue == XRSound::MusicPlay::Off)
                internalValue = Parser::MusicPlay::On;          // start from a different value so the call must write it
            CHECK(Parser::MusicPlayToInternal(static_cast<int>(publicValue), internalValue));
            CHECK(Parser::MusicPlayToPublic(internalValue) == static_cast<int>(publicValue));
        }

        // direct mapping (a swap applied in both directions would still round-trip)
        Parser::MusicPlay play = Parser::MusicPlay::Off;
        CHECK(Parser::MusicPlayToInternal(static_cast<int>(XRSound::MusicPlay::Off), play));
        CHECK(play == Parser::MusicPlay::Off);
        CHECK(Parser::MusicPlayToInternal(static_cast<int>(XRSound::MusicPlay::Space), play));
        CHECK(play == Parser::MusicPlay::Space);
        CHECK(Parser::MusicPlayToInternal(static_cast<int>(XRSound::MusicPlay::On), play));
        CHECK(play == Parser::MusicPlay::On);

        Parser::MusicPlay unchanged = Parser::MusicPlay::Space;
        CHECK_FALSE(Parser::MusicPlayToInternal(-1, unchanged));
        CHECK(unchanged == Parser::MusicPlay::Space);
    }
}
