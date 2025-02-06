// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ======================================================================
// Map window
// ======================================================================

#ifndef __DLGMAP_H
#define __DLGMAP_H

#include "DlgMgr.h"
#include "VectorMap.h"

class CelestialBody;
class DlgMap : public ImGuiDialog {
	void Reset();
public:
    DlgMap();
	void Display() override;
    void OnDraw() override;
    void DrawMap();
    void DrawMenu();
    void SetBody(const char *body);
    void AddCbodyNode(const CelestialBody *cbody);
    void DrawTree();
	bool FindTarget(int mx, int my);
	bool SetSelection(const char *name);

    std::unique_ptr<VectorMap> vectormap;
    double updTmin = 0.0, updTmax = 0.0;
    std::string planet;
    double zoom = 1.0;
	bool enableInfo;
	int selectionfilter;
	char searchbuf[128];
	CFG_MAPPRM *prm;
};

#endif // !__DLGMAP_H
