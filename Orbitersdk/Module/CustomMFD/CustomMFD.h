#ifndef __CUSTOMMFD_H
#define __CUSTOMMFD_H

class AscentMFD: public GraphMFD {
public:
	AscentMFD (DWORD w, DWORD h, VESSEL *vessel);
	~AscentMFD ();
	bool ConsumeKeyBuffered (DWORD key);
	bool ConsumeButton (int bt, int event);
	char *ButtonLabel (int bt);
	int  ButtonMenu (const MFDBUTTONMENU **menu) const;
	void Update (HDC hDC);
	bool SetAltRange (char *rstr);
	bool SetVradRange (char *rstr);
	bool SetVtanRange (char *rstr);
	void WriteStatus (FILEHANDLE scn) const;
	void ReadStatus (FILEHANDLE scn);
	void StoreStatus (void) const;
	void RecallStatus (void);
	static int MsgProc (UINT msg, UINT mfd, WPARAM wparam, LPARAM lparam);

private:
	void InitReferences (void);
	OBJHANDLE ref;
	double tgt_alt;
	bool  alt_auto;
	bool  vrad_auto, vtan_auto;
	float minpitch, maxpitch;
	int page;
	float *ref_alt;
	float *ref_tvel;
	int mfd;

	// transient parameter storage
	static struct SavePrm {
		int page;
		float altmin, altmax;
		float vradmin, vradmax;
		float vtanmin, vtanmax;
	} saveprm;
};

#endif //!__CUSTOMMFD_H