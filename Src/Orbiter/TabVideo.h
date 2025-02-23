// Copyright (c) Martin Schweiger
// Licensed under the MIT License

//-----------------------------------------------------------------------------
// Launchpad tab declaration: class DefVideoTab
// Tab for default video device parameters
//-----------------------------------------------------------------------------

#ifndef __TABVIDEO_H
#define __TABVIDEO_H

#include "LpadTab.h"
namespace fs = std::filesystem;

namespace orbiter {

	class DefVideoTab : public LaunchpadTab2 {
	public:
		explicit DefVideoTab(LaunchpadDialog2* lp);
		~DefVideoTab() override;

		void OnGraphicsClientLoaded(oapi::GraphicsClient* gc, std::string_view moduleName);

		void SetConfig(Config* cfg) override;
	protected:
		void EnumerateClients();

		void ScanDir(const fs::path &dir);
		// scan directory dir (relative to Orbiter root) for graphics clients
		// and enter them in the combo box

		void SelectClientIndex(size_t idx);

		void OnDraw(WithLpImCtx &ctx) override;
	private:
		std::string m_info;
		std::vector<std::shared_ptr<LpImage>> m_loadedImages;
		struct GcEntry {
			fs::path path;
			std::string name;
			std::string mdinfo;
			std::string category;
		};
		static GcEntry LoadInfoFile(const fs::path& path);
		std::vector<GcEntry> m_gcs;
		size_t m_selgc; // N.B.: 0 is always the "console" GC
	};

}

#endif // !__TABVIDEO_H
