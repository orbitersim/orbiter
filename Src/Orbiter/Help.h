// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __HELP_H
#define __HELP_H

#define STRICT 1
#include <windows.h>

void OpenHelp (HWND hWnd, const char *file, const char *topic);

void OpenDefaultHelp (HWND hWnd, const char *topic);
// use this only for opening a help window outside the simulation,
// e.g. from the Launchpad dialog. For in-game help, use the mechanism in
// Dialogs.cpp instead.

#endif __HELP_H