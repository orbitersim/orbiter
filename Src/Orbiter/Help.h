// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __HELP_H
#define __HELP_H

// Open a help page. 'file' names a help document as used throughout Orbiter
// ("Html/<subdir>/<name>.chm"); 'topic' is a page within it (e.g. "/intro.htm").
void OpenHelp (const char *file, const char *topic);

void OpenDefaultHelp (const char *topic);

#endif // !__HELP_H
