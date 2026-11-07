// Copyright (c) Martin Schweiger
// Licensed under the MIT License

//-----------------------------------------------------------------------------
// Launchpad tab declaration: DefVideoTab
// Graphics engine selection
//-----------------------------------------------------------------------------

#ifndef __TABVIDEO_H
#define __TABVIDEO_H

#include "LpadTab.h"
#include <filesystem>
#include <string>
#include <vector>
namespace fs = std::filesystem;

namespace oapi { class GraphicsClient; }

namespace orbiter {

	class DefVideoTab : public LaunchpadTab {
	public:
		DefVideoTab(LaunchpadDialog* lp);

		void Draw();

		void Activated();

		void OnGraphicsClientLoaded(oapi::GraphicsClient* gc, const char* moduleName);

		void SetConfig(Config* cfg);

	protected:
		void EnumerateClients();

		void ScanDir(const fs::path& dir);
		// scan directory dir (relative to Orbiter root) for graphics clients

		void SelectClientIndex(size_t idx);

	private:
		struct Client {
			std::string name;
			std::string path;
			std::string info;
		};
		std::vector<Client> m_clients;
		size_t idxClient;
		std::string strInfo;
	};

}

#endif // !__TABVIDEO_H
