// Copyright (c) Martin Schweiger
// Licensed under the MIT License

//-----------------------------------------------------------------------------
// Launchpad tab declaration: class ModuleTab
// Tab for module activation/deactivation
//-----------------------------------------------------------------------------

#ifndef __TABMODULE_H
#define __TABMODULE_H

#include "LpadTab.h"
#include <variant>

namespace orbiter {

class ModuleTab : public LaunchpadTab2 {
public:
  struct ModuleInfo {
    std::string name;
    std::string info;
    bool active;
    bool locked;
  };
  struct ModuleCategory {
    std::string name;
    std::vector<ModuleInfo> items;
  };

  explicit ModuleTab(LaunchpadDialog2 *lp);
  ~ModuleTab() override;

  void GetConfig(const Config *cfg) override;
  void SetConfig(Config *cfg) override;

  void OnDraw(WithLpImCtx &ctx) override;

protected:
  void RefreshLists();
  // Update active and inactive module lists

  void DeactivateAll();
  // deactivate all modules

  void ActivateFromList();
  // synchronises module activation with list by activating/deactivating
  // appropriate modules

  void InitActivation();
  // activate modules listed in config file and tick entries in list

  ModuleCategory &GetCategoryItem(std::string_view catName);

private:
  std::vector<ModuleCategory> categories;
  std::string selection = std::string();
  bool selectionIsCat = false;
  int splitWidth = 0;
  std::string desc;
  // !doExpandCollapse => nothing happens
  // !collapseExpand && doExpandCollapse => collapse all
  //  collapseExpand && doExpandCollapse => expand all
  bool collapseExpand = true;
  bool doExpandCollapse = true;
  std::vector<std::shared_ptr<LpImage>> loadedImages;
};
} // namespace orbiter

#endif // !__TABMODULE_H
