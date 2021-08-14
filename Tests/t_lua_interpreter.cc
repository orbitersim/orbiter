#include "Interpreter.h"

#include <memory>

using std::make_unique;

// Test that interpreter is created and destroyed without exceptions
void t_LuaInterpreter_TestCreateDestroy() {
	auto interp = make_unique<Interpreter>();
}

void t_LuaInterpreter_BasicScript() {
	auto interp = make_unique<Interpreter>();
	interp->Initialise();
	auto script = "print(\"Hello World\")";
	auto result = interp->RunChunk(script, strlen(script));
}

int main()
{
	t_LuaInterpreter_TestCreateDestroy();
	t_LuaInterpreter_BasicScript();
	return 0;
}
