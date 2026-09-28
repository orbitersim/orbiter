// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include "Help.h"
#ifndef __linux__
#include <htmlhelp.h>
#else // __linux__
#include "ChmHelp.h"
#endif // __linux__
#include <stdio.h>
#ifdef __linux__
#include <string.h>
#include <strings.h>
#endif // __linux__

#ifndef __linux__
void OpenHelp (HWND hWnd, const char *file, const char *topic)
#else // __linux__
void OpenHelp (QWidget *hWnd, const char *file, const char *topic)
#endif // __linux__
{
	char topic_file[256];
#ifndef __linux__
	if (strlen(topic) < 4 || strnicmp(topic + strlen(topic) - 4, ".htm", 4))
		sprintf(topic_file, "%s.htm", topic);
#else // __linux__
	if (strlen(topic) < 4 || strncasecmp(topic + strlen(topic) - 4, ".htm", 4))
		snprintf(topic_file, 256, "%s.htm", topic);
#endif // __linux__
	else
#ifndef __linux__
		strcpy(topic_file, topic);
	HtmlHelp (hWnd, file, HH_DISPLAY_TOPIC, (DWORD_PTR)topic_file);
#else // __linux__
		snprintf(topic_file, 256, "%s", topic);
	HtmlHelp (hWnd, file, topic_file);
#endif // __linux__
}

#ifndef __linux__
void OpenDefaultHelp (HWND hWnd, const char *topic)
#else // __linux__
void OpenDefaultHelp (QWidget *hWnd, const char *topic)
#endif // __linux__
{
	OpenHelp (hWnd, "html\\orbiter.chm", topic);
}
