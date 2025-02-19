// Copyright (c) Martin Schweiger
// Licensed under the MIT License

// ======================================================================
// Template for simulation options pages
// ======================================================================

/************************************************************************
 * \file OptionsPages.h
 * \brief Template for simulation options pages
 */

#ifndef __OPTIONSPAGES_H
#define __OPTIONSPAGES_H

#include "CustomControls.h"
#include "OrbiterAPI.h"
#include <CommCtrl.h>
#include <windows.h>

#include <GraphicsAPI.h>
#include <UIUtil.h>

class OptionsPage;
class Config;

/************************************************************************
 * \brief Container class for options pages
 */
class OptionsPageContainer {
  public:
    /**
     * \brief Enumerates where the options pages are shown.
     */
    enum Originator {
        LAUNCHPAD, ///< Show in Launchpad dialog
        INLINE     ///< Show as inline dialog during a simulation session
    };

    OptionsPageContainer(Originator orig, Config *cfg);
    ~OptionsPageContainer();

    std::optional<std::shared_ptr<OptionsPage>> CurrentPage();

    [[nodiscard]] Originator Environment() const { return m_orig; }

    Config *Cfg() { return m_cfg; }

    /**
     * \brief Update dialog controls from config settings
     */
    void UpdatePages(bool resetView);

    /**
     * \brief Update config object from dialog controls
     */
    void UpdateConfig() const;

    void SwitchPage(std::string_view name);

    void OnDraw(oapi::WithImCtx &ctx);

  protected:
    /**
     * \brief Adds a new options page.
     * \param pPage pointer to new page
     * \param parent ID of parent page
     */
    size_t AddPage(std::shared_ptr<OptionsPage> pPage,
                   std::optional<size_t> parent = std::nullopt);

    [[nodiscard]] std::optional<std::shared_ptr<OptionsPage>>
    FindPage(std::string_view name) const;

    void SwitchPage(const std::shared_ptr<OptionsPage> &page);
    void SwitchPage(size_t page);

    void CreatePages();

    void Clear();

  private:
    struct OptionTreeEntry {
        std::shared_ptr<OptionsPage> page;
        std::optional<std::shared_ptr<OptionsPage>> parent;
    };
    Originator m_orig;
    Config *m_cfg;
    std::vector<OptionTreeEntry> m_pages;
    std::optional<OptionTreeEntry *> m_currentPage;
    int splitWidth;
    void RenderTree(oapi::WithImCtx &ctx, OptionTreeEntry *parent);
};

/************************************************************************
 * \brief Base class for options dialog pages.
 */
class OptionsPage {
  public:
    /**
     * \brief OptionsPage constructor.
     * \param container Container owning the page
     * \param name The name of the page, as shown in the options list.
     */
    explicit OptionsPage(OptionsPageContainer *container, std::string name);

    /**
     * \brief OptionsPage destructor.
     */
    virtual ~OptionsPage() = default;

    /**
     * \brief Returns the container object owning the page.
     */
    [[nodiscard]] OptionsPageContainer *Container() const {
        return m_container;
    }

    [[nodiscard]] Config *Cfg() const { return m_container->Cfg(); }

    [[nodiscard]] const std::string &Name() const { return m_name; }

    /**
     * \brief Update the dialog controls from config settings.
     */
    virtual void UpdateControls() {}

    /**
     * \brief Update config object from dialog control states.
     *    Only required for pages which don't react directly to controls
     *    being modified.
     */
    virtual void UpdateConfig() {}

  protected:
    friend OptionsPageContainer;
    virtual void OnDraw(oapi::WithImCtx &ctx) = 0;

  private:
    OptionsPageContainer *m_container; ///< container owning the page
    std::string m_name;
};

/************************************************************************
 * \brief Page for visual parameters
 */
class OptionsPage_Visual : public OptionsPage {
  public:
    explicit OptionsPage_Visual(OptionsPageContainer *container);

  protected:
    void OnDraw(oapi::WithImCtx &ctx) override;
};

/************************************************************************
 * \brief Page for physics engine options
 */
class OptionsPage_Physics : public OptionsPage {
  public:
    explicit OptionsPage_Physics(OptionsPageContainer *container);

  protected:
    void OnDraw(oapi::WithImCtx &ctx) override;
};

/************************************************************************
 * \brief Page for instrument and panel options
 */
class OptionsPage_Instrument : public OptionsPage {
  public:
    explicit OptionsPage_Instrument(OptionsPageContainer *container);

  protected:
    void OnDraw(oapi::WithImCtx &ctx) override;
};

/************************************************************************
 * \brief Page for vessel options
 */
class OptionsPage_Vessel : public OptionsPage {
  public:
    explicit OptionsPage_Vessel(OptionsPageContainer *container);

  protected:
    void OnDraw(oapi::WithImCtx &ctx) override;
};

/************************************************************************
 * \brief Page for user interface options
 */
class OptionsPage_UI : public OptionsPage {
  public:
    explicit OptionsPage_UI(OptionsPageContainer *container);

  protected:
    void OnDraw(oapi::WithImCtx &ctx) override;
};

/************************************************************************
 * \brief Page for joystick options
 */
class OptionsPage_Joystick : public OptionsPage {
  public:
    explicit OptionsPage_Joystick(OptionsPageContainer *container);
    ~OptionsPage_Joystick() override;

    OptionsPage_Joystick(const OptionsPage_Joystick &) = delete;
    OptionsPage_Joystick &operator=(const OptionsPage_Joystick &) = delete;

  protected:
    void OnDraw(oapi::WithImCtx &ctx) override;

  private:
    int m_njoy;
    SDL_JoystickID *m_joylist;
};

/************************************************************************
 * \brief Page for celestial sphere rendering options.
 */
class OptionsPage_CelSphere : public OptionsPage {
  public:
    explicit OptionsPage_CelSphere(OptionsPageContainer *container);

  protected:
    void OnDraw(oapi::WithImCtx &ctx) override;
    void PopulateStarmapList();
    void PopulateBgImageList();

  private:
    std::vector<std::pair<std::string, std::string>> m_pathStarmap;
    std::vector<std::pair<std::string, std::string>> m_pathBgImage;
};

/************************************************************************
 * \brief Main page for visual helpers options.
 */
class OptionsPage_VisHelper : public OptionsPage {
  public:
    explicit OptionsPage_VisHelper(OptionsPageContainer *container);

  protected:
    void OnDraw(oapi::WithImCtx &ctx) override;
};

/************************************************************************
 * \brief Visual helpers "Planetarium" options page.
 */
class OptionsPage_Planetarium : public OptionsPage {
  public:
    explicit OptionsPage_Planetarium(OptionsPageContainer *container);

  protected:
    void OnDraw(oapi::WithImCtx &ctx) override;
};

/************************************************************************
 * \brief Visual helpers "Labels" options page.
 */
class OptionsPage_Labels : public OptionsPage {
  public:
    explicit OptionsPage_Labels(OptionsPageContainer *container);

  protected:
    void OnDraw(oapi::WithImCtx &ctx) override;
    // void ScanPsysBodies();
    // void UpdateFeatureList();
    // void RescanFeatures();
};

/************************************************************************
 * \brief Visual helpers "Body force vectors" options page.
 */
class OptionsPage_Forces : public OptionsPage {
  public:
    explicit OptionsPage_Forces(OptionsPageContainer *container);

  protected:
    void OnDraw(oapi::WithImCtx &ctx) override;
};

/************************************************************************
 * \brief Visual helpers "Object frame axes" options page.
 */
class OptionsPage_Axes : public OptionsPage {
public:
	explicit OptionsPage_Axes(OptionsPageContainer* container);

protected:
    void OnDraw(oapi::WithImCtx &ctx) override;
};

#endif // !__OPTIONSPAGES_H
