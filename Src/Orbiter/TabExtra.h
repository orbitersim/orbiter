// Copyright (c) Martin Schweiger
// Licensed under the MIT License

//-----------------------------------------------------------------------------
// Launchpad tab declaration: class ExtraTab
// Tab for additional/advanced parameters
//-----------------------------------------------------------------------------

#ifndef __TABEXTRA_H
#define __TABEXTRA_H

#include "LpadTab.h"
#include "OrbiterAPI.h"
#include <list>
#include <memory>
#include <string>
#include <vector>

class BuiltinLaunchpadItem;

namespace orbiter {

	class ExtraTab : public LaunchpadTab {
		friend class LaunchpadDialog;
	public:
		ExtraTab(LaunchpadDialog* lp);
		~ExtraTab();

		void Draw();

		void GetConfig(const Config* cfg);
		void SetConfig(Config* cfg);

	private:
		ExtraItem* Register(BuiltinLaunchpadItem* item, ExtraItem* parent = 0);
		void DrawNode(const ExtraItem* node);
		bool Exists(const LaunchpadItem* item, const std::list<std::unique_ptr<ExtraItem>>& list) const;
		void OpenItem(LaunchpadItem* item);

		std::vector<BuiltinLaunchpadItem*> m_builtin;
		LaunchpadItem* m_sel;
		float m_listWidth;
	};

}

//-----------------------------------------------------------------------------
// Additional built-in parameter settings (under the "Extra" tab)
//-----------------------------------------------------------------------------

class BuiltinLaunchpadItem: public LaunchpadItem {
public:
	BuiltinLaunchpadItem(orbiter::ExtraTab* tab);
	virtual ~BuiltinLaunchpadItem();
	Config* Cfg() const;

protected:
	void Open(ImGuiDialog* dlg);
	orbiter::ExtraTab* pTab;
	std::unique_ptr<ImGuiDialog> m_dlg;
};

//=============================================================================
// Physics engine: Parameters for dynamic state propagation
//=============================================================================
class ExtraPropagation: public BuiltinLaunchpadItem {
public:
	ExtraPropagation(orbiter::ExtraTab* tab): BuiltinLaunchpadItem(tab) {}
	char* Name();
	char* Description();
};

//-----------------------------------------------------------------------------
// Physics engine: Parameters for linear state propagation
//-----------------------------------------------------------------------------
class ExtraDynamics: public BuiltinLaunchpadItem {
public:
	ExtraDynamics(orbiter::ExtraTab* tab): BuiltinLaunchpadItem(tab) {}
	char* Name();
	char* Description();
	bool clbkOpen();
};

//-----------------------------------------------------------------------------
// Physics engine: Parameters for orbit stabilisation
//-----------------------------------------------------------------------------
class ExtraStabilisation: public BuiltinLaunchpadItem {
public:
	ExtraStabilisation(orbiter::ExtraTab* tab): BuiltinLaunchpadItem(tab) {}
	char* Name();
	char* Description();
	bool clbkOpen();
};

//=============================================================================
// Instruments and panels: configuration parameters for instrumentation
//=============================================================================
class ExtraInstruments: public BuiltinLaunchpadItem {
public:
	ExtraInstruments(orbiter::ExtraTab* tab): BuiltinLaunchpadItem(tab) {}
	char* Name();
	char* Description();
};

//-----------------------------------------------------------------------------
// Instruments and panels: MFDs
//-----------------------------------------------------------------------------
class ExtraMfdConfig: public BuiltinLaunchpadItem {
public:
	ExtraMfdConfig(orbiter::ExtraTab* tab): BuiltinLaunchpadItem(tab) {}
	char* Name();
	char* Description();
	bool clbkOpen();
};

//=============================================================================
// Root item for vessel configurations (sub-items to be added by modules)
//=============================================================================
class ExtraVesselConfig: public BuiltinLaunchpadItem {
public:
	ExtraVesselConfig(orbiter::ExtraTab* tab): BuiltinLaunchpadItem(tab) {}
	char* Name();
	char* Description();
};

//=============================================================================
// Root item for planet configurations (sub-items to be added by modules)
//=============================================================================
class ExtraPlanetConfig: public BuiltinLaunchpadItem {
public:
	ExtraPlanetConfig(orbiter::ExtraTab* tab): BuiltinLaunchpadItem(tab) {}
	char* Name();
	char* Description();
};

//=============================================================================
// Debugging parameters
//=============================================================================
class ExtraDebug: public BuiltinLaunchpadItem {
public:
	ExtraDebug(orbiter::ExtraTab* tab): BuiltinLaunchpadItem(tab) {}
	char* Name();
	char* Description();
};

//-----------------------------------------------------------------------------
// Debugging parameters: shutdown options
//-----------------------------------------------------------------------------
class ExtraShutdown: public BuiltinLaunchpadItem {
public:
	ExtraShutdown(orbiter::ExtraTab* tab): BuiltinLaunchpadItem(tab) {}
	char* Name();
	char* Description();
	bool clbkOpen();
};

//-----------------------------------------------------------------------------
// Debugging parameters: fixed time steps
//-----------------------------------------------------------------------------
class ExtraFixedStep: public BuiltinLaunchpadItem {
public:
	ExtraFixedStep(orbiter::ExtraTab* tab): BuiltinLaunchpadItem(tab) {}
	char* Name();
	char* Description();
	bool clbkOpen();
};

//-----------------------------------------------------------------------------
// Debugging parameters: rendering options
//-----------------------------------------------------------------------------
class ExtraRenderingOptions: public BuiltinLaunchpadItem {
public:
	ExtraRenderingOptions(orbiter::ExtraTab* tab): BuiltinLaunchpadItem(tab) {}
	char* Name();
	char* Description();
	bool clbkOpen();
};

//-----------------------------------------------------------------------------
// Debugging parameters: performance options
//-----------------------------------------------------------------------------
class ExtraPerformanceSettings: public BuiltinLaunchpadItem {
public:
	ExtraPerformanceSettings(orbiter::ExtraTab* tab): BuiltinLaunchpadItem(tab) {}
	char* Name();
	char* Description();
	bool clbkOpen();
};

//-----------------------------------------------------------------------------
// Debugging parameters: launchpad options
//-----------------------------------------------------------------------------
class ExtraLaunchpadOptions: public BuiltinLaunchpadItem {
public:
	ExtraLaunchpadOptions(orbiter::ExtraTab* tab): BuiltinLaunchpadItem(tab) {}
	char* Name();
	char* Description();
	bool clbkOpen();
};

//-----------------------------------------------------------------------------
// Debugging parameters: log file options
//-----------------------------------------------------------------------------
class ExtraLogfileOptions: public BuiltinLaunchpadItem {
public:
	ExtraLogfileOptions(orbiter::ExtraTab* tab): BuiltinLaunchpadItem(tab) {}
	char* Name();
	char* Description();
	bool clbkOpen();
};

#endif // !__TABEXTRA_H
