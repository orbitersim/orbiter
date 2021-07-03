// =======================================================================
// key mapping interface
// =======================================================================

#ifndef __KEYMAP_H
#define __KEYMAP_H

#include "windows.h"
#include "Orbitersdk.h"

// key modifier list
#define KMOD_LSHIFT 0x0100
#define KMOD_RSHIFT 0x0200
#define KMOD_SHIFT  (KMOD_LSHIFT|KMOD_RSHIFT)
#define KMOD_LCTRL  0x0400
#define KMOD_RCTRL  0x0800
#define KMOD_CTRL   (KMOD_LCTRL|KMOD_RCTRL)
#define KMOD_LALT   0x1000
#define KMOD_RALT   0x2000
#define KMOD_ALT    (KMOD_LALT|KMOD_RALT)

class Keymap {
public:
	Keymap ();

	void SetDefault ();
	// set orbiter-default key mapping

	bool Read (char *fname);
	// parse keymap table from file

	void Write (char *fname);
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