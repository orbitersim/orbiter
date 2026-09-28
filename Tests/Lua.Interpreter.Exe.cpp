// not upstream: test doubles for the exe-side symbols LuaInterpreter.so binds at load time (the test calls none of them)

#define TESTEXPORT __attribute__((visibility("default")))

TESTEXPORT void InitLib (void *hModule) {}

extern "C" {
TESTEXPORT const void *mfd2_vtable[64] __asm__("_ZTV4MFD2") = {}; // vtable for MFD2 placeholder
TESTEXPORT const void *mfd2_typeinfo[4] __asm__("_ZTI4MFD2") = {}; // typeinfo for MFD2 placeholder
}
