#include "Interpreter.h"

#include <memory>

#define CATCH_CONFIG_MAIN  // This tells Catch to provide a main() - only do this in one cpp file
#include "catch2\catch_all.hpp"

using std::make_unique;
using std::string;

// Test that interpreter is created and destroyed without exceptions
TEST_CASE("Create and test Lua interpreter", "[LuaInterpreter]" ) 
{
	auto interp = make_unique<Interpreter>();
	interp->Initialise();

	string script = "print(\"Hello World\")";
	interp->RunChunk(script.data(), script.size());

	script = "a = 1 + 3";
	interp->RunChunk(script.data(), script.size());
	auto L = interp->GetState();
	lua_getglobal(L, "a");
	REQUIRE(lua_tointeger(L, -1) == 4);
};
