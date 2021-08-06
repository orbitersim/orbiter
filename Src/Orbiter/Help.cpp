// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include "Help.h"
#include <htmlhelp.h>
#include <stdio.h>
#include "resource.h"

void OpenHelp (HWND hWnd, HINSTANCE hInstance, const char *file, const char *topic)
{
	char topic_file[256];
	sprintf (topic_file, "%s.htm", topic);
	HtmlHelp (hWnd, file, HH_DISPLAY_TOPIC, (DWORD_PTR)topic_file);
}

void OpenScenarioHelp (HWND hWnd, HINSTANCE hInstance, const char *scenario, const char *topic)
{
	char path[256];
	sprintf (path, "html\\scenarios\\%s.chm", scenario);
	OpenHelp (hWnd, hInstance, path, topic);
}

void OpenDefaultHelp (HWND hWnd, HINSTANCE hInstance, const char *topic)
{
	OpenHelp (hWnd, hInstance, "html\\orbiter.chm", topic);
}

void OpenCredits (HWND hWnd, HINSTANCE hInstance)
{
	HtmlHelp (hWnd, "html\\Credit.chm", HH_DISPLAY_TOPIC, (DWORD_PTR)"Credit.htm");
}
