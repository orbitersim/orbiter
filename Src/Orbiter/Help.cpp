// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include "Help.h"
#include <htmlhelp.h>
#include <stdio.h>

void OpenHelp (HWND hWnd, const char *file, const char *topic)
{
	char topic_file[256];
	sprintf (topic_file, "%s.htm", topic);
	HtmlHelp (hWnd, file, HH_DISPLAY_TOPIC, (DWORD_PTR)topic_file);
}

void OpenDefaultHelp (HWND hWnd, const char *topic)
{
	OpenHelp (hWnd, "html\\orbiter.chm", topic);
}
