// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include <windows.h>
#include <process.h>

INT WINAPI WinMain (HINSTANCE hInstance, HINSTANCE, LPSTR strCmdLine, INT nCmdShow)
{
	const char *cmd = "Modules\\Server\\Orbiter.exe";
	return _execl(cmd, cmd, strCmdLine, NULL);
}