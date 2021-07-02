#ifndef __TRACKIR_H
#define __TRACKIR_H

#include "orbitersdk.h"
#include "camapi.h"

// ==============================================================

class TrackIRconfig;

class TrackIR: public ExternalCameraControl {
	friend class TrackIRconfig;
public:
	TrackIR ();
	~TrackIR ();
	inline bool Connected() const { return connected; }
	bool ReadData ();
	void StartSimulation (HWND hWnd);
	void EndSimulation ();
	bool clbkPoll (CamData *data);

private:
	bool connected; // connected to TrackIR server?
	char dllPath[256];
	char cVersion[16];
	unsigned long laststate;
};

// ==============================================================

struct GPARAMS {
	HINSTANCE hInst;
	TrackIRconfig *item;
};

#endif // !__TRACKIR_H