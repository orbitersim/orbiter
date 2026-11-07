// Copyright (c) Martin Schweiger
// Licensed under the MIT License

//-----------------------------------------------------------------------------
// Launchpad tab declaration: ScenarioTab
// Scenario selection
//-----------------------------------------------------------------------------

#ifndef __TABSCENARIO_H
#define __TABSCENARIO_H

#include "LpadTab.h"
#include <filesystem>
#include <string>
#include <vector>
namespace fs = std::filesystem;

namespace orbiter {

	class ScenarioTab : public orbiter::LaunchpadTab {
	public:
		ScenarioTab(LaunchpadDialog* lp);
		~ScenarioTab();

		void Draw();

		void GetConfig(const Config* cfg);
		void SetConfig(Config* cfg);

		void Activated();
		void LaunchpadShowing(bool show);

		std::string SelectedScenario() const;
		// path of the selected scenario relative to the scenario folder,
		// without extension. Empty if a folder or nothing is selected.

	private:
		struct Node {
			std::string name;
			bool dir;
			std::vector<Node> children;
			bool operator== (const Node& n) const { return name == n.name && dir == n.dir && children == n.children; }
		};

		struct Run {
			std::string text;
			std::string url;
		};

		struct Block {
			enum Type { HEADING, TEXT, BULLET, IMAGE } type;
			std::vector<Run> runs;
			bool tight = false;
			std::string image;
			bool fullWidth = false;
			bool loaded = false;
			void* tex = nullptr;
			int w = 0, h = 0;
		};

		fs::path ScnDir() const;
		void RefreshList(bool preserveSelection);
		void ScanDirectory(const fs::path& path, Node& node);
		const Node* FindNode(const std::string& path) const;
		void Select(const std::string& path, bool reveal);
		void DrawNode(const Node& node, const std::string& path);

		void LoadDescription();
		void ClearDescription();
		void DrawDescription();
		void DrawRuns(const std::vector<Run>& runs);
		void ParseText(const std::string& text);
		void ParseHtml(const std::string& html, const fs::path& base);

		void DrawSaveDialog();
		void DrawClearDialog();
		void BeginClearQuicksaves();
		int SaveCurScenarioAs(const std::string& name, const std::string& desc, bool replace);
		// 0=ok, 1=failed, 2=file exists (only checked if replace=false)

		Node m_root;
		std::string m_sel;
		bool m_selDir;
		bool m_reveal;
		std::vector<Block> m_desc;
		fs::path m_descBase;
		int m_descMode;
		double m_lastScan;
		float m_listWidth;
		bool m_paused;

		bool m_openSave;
		bool m_openNoState;
		char m_saveName[64];
		char m_saveDesc[4096];
		bool m_saveExists;
		std::string m_saveError;

		bool m_openClear;
		std::vector<std::string> m_quicksaves;
	};

}

#endif // !__TABSCENARIO_H
