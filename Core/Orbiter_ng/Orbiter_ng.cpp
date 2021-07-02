#include <windows.h>
#include <process.h>

INT WINAPI WinMain (HINSTANCE hInstance, HINSTANCE, LPSTR strCmdLine, INT nCmdShow)
{
	const char *cmd = "modules\\server\\orbiter.exe";
	char *args[1] = {NULL};
	_execl(cmd, cmd, NULL);
	return 0;
}