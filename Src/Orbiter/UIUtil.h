#ifndef __SDLUTIL_H
#define __SDLUTIL_H

#include <SDL3/SDL.h>
#include <SDL3/SDL_gpu.h>
#include <SDL3_image/SDL_image.h>
#include "imgui.h"
#include "Orbiter.h"
#include "Log.h"
#include "SDLWrappers.h"

class ImGuiMgr;

class LocalImCtx {
public:
    LocalImCtx(const LocalImCtx &) = delete;

    LocalImCtx &operator=(const LocalImCtx &) = delete;

    ~LocalImCtx() {
        if (lastContext)
            ImGui::SetCurrentContext(lastContext);
    };

    ImGuiMgr *operator->() const {
        return inner;
    }

    bool ConsumeEvent(const SDL_Event &event, bool &wantsOut) const;

    bool BeginFrame() const;

    void EndFrame() const;

private:
    friend class ImGuiMgr;

    LocalImCtx(ImGuiMgr *inner, ImGuiContext *lastContext) : inner(inner),
        lastContext(lastContext) {
    };
    ImGuiMgr *inner;
    ImGuiContext *lastContext;
};

class ImGuiMgr {
private:
    friend class std::unique_ptr<ImGuiMgr>;

    ImGuiMgr() = default;

public:
    ImGuiMgr(const ImGuiMgr &) = delete;

    ImGuiMgr &operator=(const ImGuiMgr &) = delete;

    ImGuiMgr(Orbiter *app, const std::shared_ptr<Window> &window);

    virtual ~ImGuiMgr();

    LocalImCtx PushLocal() {
        const auto lastContext = ImGui::GetCurrentContext();
        ImGui::SetCurrentContext(m_context);
        return {this, lastContext};
    };

    [[nodiscard]] const std::shared_ptr<Window> &Win() const {
        return m_window;
    }

    [[nodiscard]] ImFont *DefaultFont() const { return m_defaultFont; }
    [[nodiscard]] ImFont *ItalicFont() const { return m_italicFont; }
    [[nodiscard]] ImFont *BoldFont() const { return m_boldFont; }
    [[nodiscard]] ImFont *BoldItalicFont() const { return m_boldItalicFont; }
    [[nodiscard]] ImFont *HdgFont() const { return m_hdgFont; }
    [[nodiscard]] ImFont *MonoFont() const { return m_monoFont; }
    [[nodiscard]] Orbiter *App() const { return m_app; }

private:
    Orbiter *m_app;
    std::shared_ptr<Window> m_window;
    ImGuiContext *m_context;
    ImFont *m_defaultFont;
    ImFont *m_italicFont;
    ImFont *m_boldFont;
    ImFont *m_boldItalicFont;
    ImFont *m_hdgFont;
    ImFont *m_monoFont;
};


class Image {
private:
    void Load(const std::shared_ptr<Window>& win,
              SDL_Surface *origSurface,
              std::string_view path);

public:
    Image(const Image &) = delete;

    Image &operator=(const Image &) = delete;

    // N.B. this takes ownership of the surface and will destroy it.
    Image(const std::shared_ptr<Window> &win,
          SDL_Surface *origSurface,
          std::string_view path) { Load(win, origSurface, path); };

    Image(const std::shared_ptr<Window> &win,
          std::string_view path) {
        auto surface = IMG_Load(path.data());
        if (!surface) {
            LOGOUT_ERR("Warning: load image `%s` failed: %s", path,
                       SDL_GetError());
            throw std::runtime_error("Failed to load image");
        }
        Load(win, surface, path);
    };

    Image(const LocalImCtx &ctx,
          const std::string_view path) : Image(ctx->Win(),
                                               path) {
    };

    ~Image();

    [[nodiscard]] int Width() const { return m_surface->w; }
    [[nodiscard]] int Height() const { return m_surface->h; }
    [[nodiscard]] const std::string &Path() const { return m_path; }

    SDL_GPUTextureSamplerBinding *Binding() { return &m_binding; }

private:
    std::string m_path;
    std::shared_ptr<Window> m_window;
    SDL_Surface *m_surface;
    SDL_GPUTexture *m_texture;
    SDL_GPUSampler *m_sampler;
    SDL_GPUTextureSamplerBinding m_binding;

    Image() = default;
};

namespace ImGui {
    void Markdown(LocalImCtx &ctx, const std::string &md,
                  std::vector<std::shared_ptr<::Image> > &loadedImages);

    bool InputText(const char *label, std::string &buf,
                   ImGuiInputTextFlags flags = 0,
                   ImGuiInputTextCallback callback = nullptr,
                   void *user_data = nullptr);

    bool InputTextWithHint(const char *label, const char *hint,
                           std::string &buf,
                           ImGuiInputTextFlags flags = 0,
                           ImGuiInputTextCallback callback = nullptr,
                           void *user_data = nullptr);

    bool InputTextMultiline(const char *label, std::string &buf,
                            const ImVec2 &size = ImVec2(0, 0),
                            ImGuiInputTextFlags flags = 0,
                            ImGuiInputTextCallback callback = nullptr,
                            void *user_data = nullptr);
}

#endif //SDLUTIL_H
