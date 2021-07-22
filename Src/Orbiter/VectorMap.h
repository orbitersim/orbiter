// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// =======================================================================
// A class for drawing vector-based maps onto bitmaps with GDI
// =======================================================================

#ifndef __VECTORMAP_H
#define __VECTORMAP_H

//#define ASYNC_DRAWMAP

#include "Orbiter.h"
#include "Planet.h"
#include "Element.h"

#define NVTX_CIRCLE 64

#define DISP_GRIDLINE    0x0001
#define DISP_COASTLINE   0x0002
#define DISP_CONTOURS    0x0004
#define DISP_VESSEL      0x0008
#define DISP_FOCUSONLY   0x0010
#define DISP_HORIZONLINE 0x0020
#define DISP_NAVAID      0x0040
#define DISP_BASE        0x0080
#define DISP_MOON        0x0100
#define DISP_CUSTOM1     0x0200
#define DISP_ORBITPLANE  0x0400
#define DISP_GROUNDTRACK 0x0800
#define DISP_ORBITFOCUS  0x1000
#define DISP_ORBITSEL    0x2000
#define DISP_TERMINATOR  0xC000

#define DISP_TERMINATOR_NONE  0x0000
#define DISP_TERMINATOR_LINE  0x4000
#define DISP_TERMINATOR_SHADE 0x8000
#define DISP_TERMINATOR_BOTH  0xC000

// flag values for RedrawFlag. These are set by the main thread to signal tasks to
// the drawing thread, and reset by the drawing thread once the task is complete
#define TASKID_ABORT     0x2 // abort current operation
#define TASKID_TERMINATE 0x3 // terminate thread
#define TASKID_DRAW      0x4 // redraw canvas

extern Orbiter *g_pOrbiter;

struct VPoint {
	double lng, lat;
};

struct VPointGT {
	double lng, lat;
	double rad;
	double t, dt;
};

struct PolyLineSpec {
	int vofs;   // offset of first node in vertex list
	int nvtx;   // number of nodes in the list
	bool close; // line forms a closed figure
};

struct PolyLineSet {
	PolyLineSet();
	~PolyLineSet();
	void Clear();
	int Load (const char *path, int type_id);
	int type;
	VPoint *vtx;
	int nvtx;
	PolyLineSpec *poly;
	int npoly;
};

struct CustomMkrSpec {
	CustomMkrSpec();
	~CustomMkrSpec();
	void Connect (oapi::GraphicsClient::LABELLIST *ll);
	void Convert();
	oapi::GraphicsClient::LABELLIST *list;
	bool active;
	VPoint *vtx;
	int nvtx;
};

struct CustomMkrSet {
	CustomMkrSet();
	~CustomMkrSet();
	void Clear();
	void Connect (oapi::GraphicsClient::LABELLIST *ll, int nlist);
	CustomMkrSpec *set;
	DWORD nset;
};

struct Groundtrack {
	Groundtrack();
	~Groundtrack();
	void Reset (const CelestialBody *body, const Elements *_el);
	void CalcPoint (VPointGT &p, double *angvel = NULL);
	double VtxDst (const VPointGT &vp1, const VPointGT &vp2);
	void Update();
	VPointGT *vtx; // groundtrack vertex points
	int nvtx;    // number of vertices
	int vfirst, vlast, vcurr, vupdt; // index of start, end and current vertex
	const Elements *el;
	const CelestialBody *cbody;
	double prad; // planet radius
	double omega_curr, omega_updt; // angular velocity at current/update position
	static const double tgtstep;
	static const double tstep_max;
};

// =======================================================================

class VectorMap {
public:
	VectorMap ();
	VectorMap (const Planet *pl);
	~VectorMap ();

	void Update ();
	//void UpdateGroundtrack ();
	bool SetCBody (const CelestialBody *body);
	const CelestialBody *GetCBody() const { return cbody; }
	const Planet *GetPlanet() const { return planet; }
	//void SetBase (const Base *base);
	void SetLabelSize(int size);
	void SetCanvas (void *device_context, int w, int h);
	void SetMapArea (double lngcnt, double latcnt, double lngext, double latext);
	void SetCenter (double lngcnt, double latcnt);
	void SetZoom (double zoom);
	void SetCenterMode (int center) { centermode = center; }
	int GetCenterMode () const { return centermode; }
	void SetDisplayFlags (DWORD flag);
	void ToggleDisplayFlags (DWORD flag);
	DWORD GetDisplayFlags () const { return dispflag; }
	void SetFindFlags (DWORD flag) { findflag = flag; }
	DWORD GetFindFlags () const { return findflag; }

	// Returns the drawing bitmap and HDC.
	// Note: waits for the drawing thread to finish
	HBITMAP GetMap ();
	HDC GetDeviceContext();

	void DrawMap ();    // redraw directly

	inline double CntLng() const { return lngc; }
	inline double CntLat() const { return latc; }
	inline double ZoomFac() const { return zoom; }

	CustomMkrSet &GetCustomMarkerSet () { return mkrset; }
	
	bool GetMapPos (double lng, double lat, int &x, int &y) const;
	bool GetMapCrd (int x, int y, double &lng, double &lat) const;

	struct DRAWDATA { // dynamic data required for drawing map elements
		bool focus_disp;            // display focus vessel?
		double focuslng, focuslat;  // position of focus vessel
		double focusrad;            // focus vessel-planet distance in units of planet radius
		Elements focusel;           // focus vessel orbital elements
		char focusname[64];         // focus vessel name
		bool tgtv_disp;             // display target vessel?
		double tgtvlng, tgtvlat;    // position of target vessel
		double tgtvrad;             // target vessel-planet distance in units of planet radius
		Elements tgtvel;            // target vessel orbital elements
		char tgtname[64];           // target vessel name
		bool tgtb_disp;             // display target base?
		double tgtblng, tgtblat;    // position of target base
		char basename[64];          // target base name
		bool sun_disp;              // draw terminator line?
		double sunlng, sunlat;      // position of the sun
	} drawdata;

	struct OBJTYPE {
		const void *obj;
		int type;
	};
	const OBJTYPE FindObject (int x, int y) const;
	bool SetSelection (const OBJTYPE &obj);
	void UnsetSelection ();
	OBJTYPE GetSelection () const { return selection; }
	bool GetObjPos (const OBJTYPE &obj, double &lng, double &lat);

protected:
	void SetDefaults ();
	void AllocCustomResources ();
	void CheckSelection ();

	inline int mapx (double lng) const
	{
		while (lng > lngc+PI) lng -= PI2;
		while (lng < lngc-PI) lng += PI2;
		return (int)(mapx_scale * (mapx_ofs+lng));
	}
	inline int mapy (double lat) const
	{   //return (int)(ch * (dlat-lat+latc)/(2.0*dlat));
		return (int)(mapy_scale * (mapy_ofs-lat));
	}

	// drawing logical object sets
	void DrawMap_engine ();// redraw map
	void DrawGridlines ();
	void DrawPolySet (const PolyLineSet *pls);
	void DrawPolyline (int type, VPoint *vp, int n, bool close = true);
	void DrawNavaids ();
	void DrawVessels ();
	void DrawMoons ();
	void DrawVesselOrbit (Vessel *v);
	void DrawBases ();
	void DrawCustomMarkerSet (int idx);
	void DrawTerminatorLine (double sunlng, double sunlat);
	void DrawSunnySide (double sunlng, double sunlat, bool terminator);
	void DrawOrbitPlane (const Elements *el, int which);
	void DrawGroundtrack (Groundtrack &gt, int which);
	void DrawGroundtrack_past (Groundtrack &gt, int which);
	void DrawGroundtrack_future (Groundtrack &gt, int which);
	void DrawHorizon (double lng, double lat, double rad, bool focus);
	void DrawGroundtrackLine (int type, VPointGT *vp, int n, int n0, int n1);

	// drawing primitives
	void DrawMarker (double lng, double lat, const char *name, int which); // which: 0=focusobj, 1=orbittarget, 2=basetarget

	void DrawSelectionMarker (const OBJTYPE obj);

	// calculate the vertex points of a great circle on the sphere
	// The circle describes the intersection of the sphere with an
	// orbital plane, where the orbit is given by elements el
	void CalcOrbitProj (const Elements *el, const CelestialBody *body, VPoint *p);

	// calculate vertex points of a great circle on the sphere
	// lng, lat: direction of the normal (north pole)
	VPoint *GreatCircle (double lng, double lat);

	// calculate vertex points of a small-circle on the sphere
	// lng, lat: centre direction of the circle
	// dst: distance of small-circle plane from planet centre (unit radius)
	VPoint *SmallCircle (double lng, double lat, double dst);

	const CelestialBody *cbody;
	const Planet *planet;
	//const Base *btgt; // surface base target

	Groundtrack gt_this, gt_tgt;

	PolyLineSet coast, contour;  // map vector line sets

	oapi::GraphicsClient::LABELLIST *mkrlist; // custom labels
	int nmkrlist;
	CustomMkrSet mkrset;

	int cw, ch;         // canvas dimensions [pixel]
	int cntx, cnty;     // map centre coordinates [pixel]
	double lngc, latc;  // map centre coordinates [rad]
	double dlng, dlat;  // map semi-width and height [rad]
	double lngmin, lngmax, latmin, latmax; // map bounding box [rad]
	double zoom;        // magnification factor
	double scalefac;    // scaling factor for mapping
	int labelsize;      // font size for in-map labels
	double mapx_ofs, mapx_scale; // parameters for longitude mapping
	double mapy_ofs, mapy_scale; // parameters for latitude mapping
	int centermode;     // 0=none, 1=keep focusobject centered, 2=keep selection centered
	DWORD dispflag;     // bitflags for display elements
	DWORD findflag;     // bitflags for search elements
	OBJTYPE selection;  // selected object

private:
	static bool bsetup;
	static double cosp[NVTX_CIRCLE]; // precalculate cosines and sines of nodes on a circle
	static double sinp[NVTX_CIRCLE];

	// ------------------------------------------------------------------
	// Drawing resources

	HDC       hDCmem;   // memory device context
	HBITMAP   hBmpDraw; // bitmap for background drawing
	HPEN      penGridline;
	HPEN      penCoast;
	HPEN      penContour;
	HPEN      penTerminator;
	HPEN      penOrbitFuture[3]; // 0=focus, 1=vessel, 2=moon
	HPEN      penOrbitPast[3]; // 0=focus, 1=vessel, 2=moon
	HPEN      penFocusHorizon;
	HPEN      penTargetHorizon;
	HPEN      penNavmkr;
	HPEN      penBase;
	HPEN      penSelection;
	HPEN      penMarker[3];
	HPEN     *penCustomMkr;
	HBRUSH    brushDay;
	HFONT     fontLabel;
	COLORREF *colCustomMkr;
	DWORD     nCustomMkr;

private:
	void InitGDIResources();
	void CloseGDIResources();

#ifdef ASYNC_DRAWMAP
	// ------------------------------------------------------------------
	// Thread interface for asynchronous drawing
protected:
	HANDLE hRedrawThread;   // redraw thread handle
	HANDLE hActivateThread; // thread activation event handle; set by the main thread to start thread execution
	HANDLE hCommMutex;      // mutex for protecting communication between main and drawing threads

	// Blocking wait: returns after thread has completed its current operation (if any)
	// abortOp: this flag asks the thread to terminate its current operation. Use this
	// if the results of the current operation are obsolete and you want the function
	// to return as quickly as possible
	void WaitThread (bool abortOp = false);

	// Thread communication data. This structure should only be accessed
	// after acquiring the hCommMutex.
	struct THREADDATA {
		int taskid;             // task identifier
	} threaddata;

public:
	// Returns true if thread is busy, false if waiting
	// is it safe to do this without locking the mutex?
	inline bool ThreadBusy () const
	{ return threaddata.taskid != 0; }

	// Request asynchronous map redraw
	bool AsyncDrawMap ();

protected:
	// the following functions (th*) are accessed only by the thread
	void thEngine ();  // redraw thread loop

private:
	static DWORD WINAPI Redraw_ThreadProc (void*); // thread entry point
	// ------------------------------------------------------------------
#endif // ASYNC_DRAWMAP
};

#endif // !__VECTORMAP_H