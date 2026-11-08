// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __LAUNCHPAD_H
#define __LAUNCHPAD_H

#include "OrbiterAPI.h"
#include "Config.h"
#include <list>
#include <memory>
#include <string>
#include <vector>

class Orbiter;
struct ImGuiContext;
struct ImFont;
namespace oapi { class GraphicsClient; }

namespace orbiter {

	class LaunchpadTab;
	class ScenarioTab;
	class ExtraTab;
	class LaunchpadRenderer;

	// Node in the tree of "Extra" items. Items are registered by the core
	// and by plugin modules (oapiRegisterLaunchpadItem).
	struct ExtraItem {
		LaunchpadItem *item;
		ExtraItem *parent;
		std::list<std::unique_ptr<ExtraItem>> children;
	};

	//-----------------------------------------------------------------------------
	// Name: class LaunchpadDialog
	// Desc: Handles the startup window ("Launchpad")
	//-----------------------------------------------------------------------------
	class LaunchpadDialog {
		friend class ::Orbiter;
		friend class LaunchpadTab;

	public:
		enum Page { PG_SCN = 0, PG_OPT, PG_MOD, PG_VID, PG_EXT, PG_ABT, PG_COUNT };

		LaunchpadDialog(Orbiter* app);
		~LaunchpadDialog();

		bool Create(bool startvideotab = false);
		// create the Launchpad window
		// If return value==false then the window could not be created

		void Show(); // Show the launchpad window
		void Hide(); // Hide the launchpad window

		inline bool Visible() const { return m_bVisible; }

		void RenderFrame();
		// Draw and present one frame of the Launchpad UI

		inline Orbiter* App() const { return pApp; }
		inline Config* Cfg() const { return pCfg; }
		LaunchpadTab* GetTab(UINT i) const;
		inline ScenarioTab* GetScenarioTab() const { return pScenario; }
		inline ExtraTab* GetExtraTab() const { return pExtra; }
		void SwitchTabPage(int pg);

		void EnableLaunchButton(bool enable);
		// Enable/disable "Launch Orbiter" button

		void LaunchScenario(const char* scenario);
		// Save the current settings and start a simulation session

		void RequestExit();
		// Close the Launchpad and terminate Orbiter

		ExtraItem* RegisterExtraParam(LaunchpadItem* item, ExtraItem* parent = 0);
		// Register an item in the "Extra" list. If parent=0, the item is registered
		// as a root (top level) item. Otherwise it appears as a sub-item under
		// the parent item.

		bool UnregisterExtraParam(LaunchpadItem* item);
		// Unregister an item in the "Extra" list.

		ExtraItem* FindExtraParam(const char* name, const ExtraItem* parent = 0);
		// Return item 'name' below parent 'parent', or NULL if not found

		inline const std::list<std::unique_ptr<ExtraItem>>& ExtraItems() const { return m_extraRoot; }

		void WriteExtraParams();
		// allow all externally registered "Extra" items to write their data to file
		// (internal "extra" items use the Config class to write to Orbiter.cfg)

		void UpdateConfig();
		// save current dialog settings in application configuration

		void ShowWaitPage(bool show, long mem_committed = 0);
		void UpdateWaitProgress();
		long mem_wait; // amount of memory to be deallocated during wait
		long mem0;     // initial memory status

		void OnGraphicsClientLoaded(oapi::GraphicsClient* gc, const char* moduleName);

		// Dialogs opened while no simulation session is running (e.g. the
		// settings of "Extra" items) are hosted by the Launchpad.
		void OpenDialog(ImGuiDialog* dlg);
		bool CloseDialog(ImGuiDialog* dlg);

		// Load an image file as a texture for ImGui::Image. Returns 0 on failure.
		// Release with ReleaseImage.
		void* LoadImage(const char* path, int* width = 0, int* height = 0);
		void ReleaseImage(void* texture);

		ImFont* HeadingFont() const { return m_headingFont; }
		float UIScale() const { return m_scale; }

	private:
		Orbiter* pApp;           // application pointer
		Config* pCfg;            // config pointer
		HWND hWnd;               // launchpad window
		std::vector<LaunchpadTab*> TabList;
		int m_page;
		int m_requestPage;
		ScenarioTab* pScenario;
		ExtraTab* pExtra;
		bool m_bVisible;
		bool m_bLaunchEnabled;
		std::string m_pendingLaunch;
		bool m_bWait;
		float m_waitProgress;
		std::list<std::unique_ptr<ExtraItem>> m_extraRoot;
		std::list<ImGuiDialog*> m_dialogs;
		ImGuiContext* m_ctx;
		LaunchpadRenderer* m_renderer;
		ImFont* m_headingFont;
		float m_scale;
		time_t m_idleStart;

		void AddTab(LaunchpadTab* tab);
		void InitImGui();
		void ShutdownImGui();
		void DrawUI();
		void DrawWaitPage();
		void SetDemoMode();
		bool SelectDemoScenario(std::string& scenario);
		void CheckDemoTimer();
		void ResetIdleTimer();
		ExtraItem* FindNode(LaunchpadItem* item, std::list<std::unique_ptr<ExtraItem>>& list);

		static LRESULT WndProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
	};

}

#endif // !__LAUNCHPAD_H
