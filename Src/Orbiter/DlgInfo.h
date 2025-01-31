// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ======================================================================
// Object info window
// ======================================================================

#ifndef __DLGINFO_H
#define __DLGINFO_H
#include "OrbiterAPI.h"

class CelestialBody;
class Vessel;
class Base;
class Body;
class DlgInfo : public ImGuiDialog {
public:
    DlgInfo();
    void OnDraw() override;
    void AddCbodyNode(const CelestialBody *cbody);
    void DrawTree();
    void DrawInfo();
    void DrawInfoVessel(Vessel *);
    void DrawInfoCelestialBody(CelestialBody *);
    void DrawInfoBase(Base *);
	void SetBody(Body *);
    std::string m_SelectedTarget;
};

#endif // !__DLGINFO_H