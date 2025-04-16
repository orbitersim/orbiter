// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// =======================================================================
// key mapping interface
// =======================================================================

#ifndef __KEYMAP_H
#define __KEYMAP_H

#include "windows.h"
#include "Orbitersdk.h"

// key modifier list
#define OKMOD_LSHIFT 0x0100
#define OKMOD_RSHIFT 0x0200
#define OKMOD_SHIFT  (OKMOD_LSHIFT|OKMOD_RSHIFT)
#define OKMOD_LCTRL  0x0400
#define OKMOD_RCTRL  0x0800
#define OKMOD_CTRL   (OKMOD_LCTRL|OKMOD_RCTRL)
#define OKMOD_LALT   0x1000
#define OKMOD_RALT   0x2000
#define OKMOD_ALT    (OKMOD_LALT|OKMOD_RALT)

class Keymap {
public:
	Keymap ();

	void SetDefault ();
	// set orbiter-default key mapping

	bool Read (const char *fname);
	// parse keymap table from file

	void Write (const char *fname);
	// write keymap table to file

	bool IsLogicalKey (char *kstate, int lfunc) const;
	// return true if logical function lfunc is selected by key state kstate

	bool IsLogicalKey (DWORD &key, char *kstate, int lfunc, bool clearkey = true) const;
	// return true if logical function lfunc is selected by key and kstate
	// (kstate only used for modifier keys)
	// if clearkey == true then key is set to 0 on return, if a match is found

private:
	bool IsMatchingModifier (char *kstate, WORD key) const;

	bool ScanStr (char *cbuf, WORD &key) const;
	char *PrintStr (char *cbuf, WORD &key) const;

	WORD func[LKEY_COUNT];
	// list of logical function keys
	// LOBYTE: key id; HIBYTE: modifier (see modifier list)
};

#endif // !__KEYMAP_H