// Copyright (c) Martin Schweiger
// Licensed under the MIT License

//-----------------------------------------------------------------------------
// Launchpad tab declaration: ScenarioTab
// Scenario selection
//-----------------------------------------------------------------------------

#ifndef __TABSCENARIO_H
#define __TABSCENARIO_H

#include <CommCtrl.h>
#include "LpadTab.h"
#include "CustomControls.h"
#include <filesystem>
#include "UIUtil.h"
namespace fs = std::filesystem;

namespace orbiter {

	struct ScenarioTreeItem {
		std::shared_ptr<Image> icon;
		std::shared_ptr<Image> selIcon;
		fs::path path;
		std::string name;
	};

	struct ScenarioTree {
		ScenarioTreeItem item;
		std::vector<ScenarioTree> children;
	};

	class ScenarioTab : public orbiter::LaunchpadTab2 {
	public:
		explicit ScenarioTab(const orbiter::LaunchpadDialog2* lp);

		void GetConfig(const Config* cfg) override;
		void SetConfig(Config* cfg) override;

		void OnDraw(LocalImCtx &ctx) override;

	protected:
		void RefreshList(bool preserveSelection);
		// refresh the scenario list

		void ScenarioChanged();
		// Respond to a user scenario selection

		void SaveCurScenario();
		// open dialog to allow saving of current scenario, if available

		int SaveCurScenarioAs(const char* name, const char* desc, bool replace = false);
		// copy current scenario file into 'name', replacing description with 'desc'.
		// return value: 0=ok, 1=failed, 2=file exists (only checked if replace=false)

		void ClearQSFolder();
		// remove all scenarios in the quicksave folder
	private:
		ScenarioTree BuildScnTree(const ScenarioTreeItem& root);
		void RenderTree(const ScenarioTree& tree);

		std::shared_ptr<Image> img_folder1;
		std::shared_ptr<Image> img_folder2;
		std::shared_ptr<Image> img_scn1;
		ScenarioTree tree;
		fs::path selection;
		std::vector<std::shared_ptr<Image>> loadedImages;
		std::string desc;
		int scnListW = 0;
		bool startPaused;
		std::string saveScnName;
		std::string saveScnDesc;
	};

}

#endif // !__TABSCENARIO_H