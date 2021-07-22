// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __HUD_H
#define __HUD_H

#include "GraphicsAPI.h"
#include "Mesh.h"
#include "Vessel.h"

class Pane;

// =======================================================================
// class HUD

class HUD {
	friend class HUD_Orbit; // temporary
public:
	HUD (const Pane *_pane);
	virtual ~HUD();
	virtual int Mode() const = 0;
	virtual void SwitchColour (int idx);
	void Draw (oapi::Sketchpad *skp);
	void DrawDefault (oapi::Sketchpad *skp);
	void RenderDefault ();
	void RenderCustom (MESHHANDLE hMesh, SURFHANDLE *hTex);
	virtual void Resize (bool isVC);
	static HUD *Create (std::ifstream &ifs, const Pane *_pane, oapi::GraphicsClient *gc);
	void Write (std::ostream &ofs) const;
	virtual void SelectReference () {}
	virtual void ProcessMessage (int msg, void *data) {}

	void Render ();

	virtual DWORD Configure (DWORD item, DWORD value) { return 0; }
	// generic HUD customisation which can be used by individual HUD modes
	// to set vessel-specific behaviour

protected:
	SURFHANDLE LoadTexture (int idx);
	virtual char *ModeIDString() const = 0;
	virtual void Display (oapi::Sketchpad *skp) = 0;

	virtual void UpdateMesh (int &ivtx, int &iidx) {}
	// Update 2D-mesh elements for rendered HUD version

	bool DrawCenterMarker (oapi::Sketchpad *skp) const;

	void DrawLadderBar (oapi::Sketchpad *skp, double lwcosa, double lwsina,
		double dcosa, double dsina, int phi10, bool mark_subzero = false);

	void DrawTopRibbon (oapi::Sketchpad *skp, double value);
	void DrawTiltedRibbon (oapi::Sketchpad *skp, double phi, double alpha);

	void AddMesh_Billboard (int &ivtx, int &iidx, double x0, double y0, double dx, double dy, int texx, int texy, int texw, int texh);
	void AddMesh_CenterMarker (int &ivtx, int &iidx);
	bool AddMesh_Marker (int &ivtx, int &iidx, const Vector &dir, int markerid, double *xcnt=0, double *ycnt=0);
	void AddMesh_DirectionMarker (int &ivtx, int &iidx, const Vector &dir, bool prograde=false, double *xcnt=0, double *ycnt=0);
	void AddMesh_Readout (int &ivtx, int &iidx, int side, const char *str, int label=0);

	void AddMesh_LadderBar (int &ivtx, int &iidx, double sina, double cosa,
		double d, int phi10, bool mark_subzero);

	void AddMesh_HeadingTape (int &ivtx, int &iidx, double hdg, bool marker=false, double markerhdg=0.0);
	void AddMesh_AzimuthTape (int &ivtx, int &iidx, double phi, double alpha);
	void AddMesh_DockApproachGates (int &ivtx, int &iidx, const Body *tgt, const PortSpec *ps);

	int TexBltString (const char *str, int tgtx, int tgty);

	bool GlobalToHUD (const Vector &dir, int &x, int &y) const;
	bool GlobalToHUD (const Vector &dir, double &x, double &y) const;
	// maps a global direction into HUD pixel coordinates (also works for VC)
	// 'dir' must be unit length

	bool GlobalDrawMarker (oapi::Sketchpad *skp, const Vector &dir, int style) const;
	// draw a marker on the HUD pointing in global direction dir
	// return value indicates dir within sight
	// styles (can be combined with OR):
	// 1=rectangle, 2=circle, 4=cross

	oapi::IVECTOR2 *OffscreenDirMarker (const Vector &dir) const;
	// returns points to draw a triangle to point in the direction of global direction dir
	// from viewport centre. Used to indicate direction of offscreen markers

	virtual void WriteParams (std::ostream &ofs) const = 0;
	virtual void ReadParams (std::ifstream &ifs) {}

	const Pane *pane;         // pane instance
	oapi::GraphicsClient *gc; // client instance
	int colidx;               // HUD colour index
	Vector HUDofs;            // HUD centre offset from observer point [pixel]
	double ladder_width;      // elevation ladder parameters
	double ladder_range;
	int HRES05, VRES05;       // semi-dimensions
	bool bCNTvis, bCNTforward; // flags for centre visible and centre in forward direction
	oapi::Font *font;         // HUD standard text font
	oapi::Font *font2;        // HUD auxiliary font (small, proportional)
	int fW, fH;               // HUD font metrics
	int boxx;
	bool transp;			  // render HUD transparent

	// virtual cockpit parameters
	bool bVC;                 // virtual cockpit flag
	Vector VCcnt;             // HUD centre in VC [(m,m,m)]
	double VCscale;           // HUD semi-size in VC [m]

	HUDPAINTSPEC spec;        // parameters required for painting the HUD (defined in Orbitersdk.h)
	static Mesh hudMesh;      // mesh for drawing HUD elements
	SURFHANDLE hudTex;        // HUD texture elements

	struct {
		double scal;
	} renderprm;

private:
	GroupSpec *EnsureMeshBuffer(int grp, int nvtx, int nidx);

public:
	inline const HUDPAINTSPEC *PaintSpec() const { return &spec; }
};

// =======================================================================
// class HUD_Orbit
// Displays orbit-related information on top of the 3D window
// (artificial orbital plane etc.)

class HUD_Orbit: public HUD {
public:
	HUD_Orbit (const Pane *_pane);
	~HUD_Orbit ();
	int Mode() const { return HUD_ORBIT; }
	void SwitchColour (int idx);
	void SetReference (const Body *bref);
	const Body *GetReference () const { return ref; }
	void SelectReference ();

protected:
	char *ModeIDString() const { return "ORBIT"; }
	void Display (oapi::Sketchpad *skp);
	void UpdateMesh (int &ivtx, int &iidx);
	void WriteParams (std::ostream &ofs) const;
	void ReadParams (std::ifstream &ifs);

private:
	const Body *ref; // orbit reference body (0 for auto)
	static bool ClbkName_Ref (InputBox*, char *str, void *data);
	bool SelectRef (char *str);
	char refname[64];
	int refwidth;

	static struct SavePrm {    // persistent data storage
		Vessel *vessel;        //     current vessel;
		const Body *ref;       //     orbit reference body (0 for auto)
	} saveprm;
};

// =======================================================================
// class HUD_Surface
// Displays surface-mode hud (horizon flight path ladder etc.)

class HUD_Surface: public HUD {
public:
	HUD_Surface (const Pane *_pane);
	int Mode() const { return HUD_SURFACE; }
	void SwitchColour (int idx);

protected:
	char *ModeIDString() const { return "SRFCE"; }
	void Display (oapi::Sketchpad *skp);
	void UpdateMesh (int &ivtx, int &iidx);
	void WriteParams (std::ostream &ofs) const;

private:
	char refname[64];
	int refwidth;
	static double as_plimit; // pressure limit for airspeed display (use groundspeed at lower pressure)
};

// =======================================================================
// class HUD_Docking
// Displays docking hud (used for docking to orbital stations)
// (target marker, velocity marker etc.)

class HUD_Docking: public HUD {
public:
	HUD_Docking (const Pane *_pane);
	~HUD_Docking ();
	int Mode() const { return HUD_DOCKING; }
	void SwitchColour (int idx);
	void SetNav (DWORD setnv);
	DWORD GetNav () const { return nv; }
	void SetReference (const Body *ref, int port);
	void SelectReference ();
	void SelectReferenceOld ();
	// legacy method for reference station selection
	void ProcessMessage (int msg, void *data);

protected:
	char *ModeIDString() const { return "DOCK"; }
	void Display (oapi::Sketchpad *skp);
	void UpdateMesh (int &ivtx, int &iidx);
	void WriteParams (std::ostream &ofs) const;
	void ReadParams (std::ifstream &ifs);

private:
	DWORD nv;        // slaved NAV receiver

	// Legacy code
	bool bUseLegacyReference;  // flag for old-style reference selection
	const Body *Legacy_ref;    // target station/vessel (legacy method)
	int Legacy_port;           // target docking port (legacy method)
	static bool ClbkName_Ref (InputBox*, char *str, void *data);
	bool SelectRef (char *str);
	char refname[128], navname[128], vstr1[128], pstr1[128];
	int refwidth, navwidth, vstr1width, pstr1width;

	static struct SavePrm {    // persistent data storage
		Vessel *vessel;        //     current vessel
		DWORD nv;              //     slaved NAV receiver
		const Body *lref;      //     legacy reference
		int lport;             //     legacy port
		bool luse;             //     use legacy mechanism?
	} saveprm;
};

#endif // !__HUD_H