#ifndef __SDLUTIL_H
#define __SDLUTIL_H

#include <OrbiterAPI.h>
#include <SDL3/SDL.h>
#include <SDL3/SDL_gpu.h>
#include <SDL3_image/SDL_image.h>
#include "imgui.h"
#include "Orbiter.h"

class ImGuiMgr;

class WithLocalContext {
public:
    WithLocalContext(WithLocalContext const &) = delete;

    void operator=(WithLocalContext const &) = delete;

    ~WithLocalContext() {
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

    WithLocalContext(ImGuiMgr *inner, ImGuiContext *lastContext) : inner(inner),
        lastContext(lastContext) {
    };
    ImGuiMgr *inner;
    ImGuiContext *lastContext;
};

class ImGuiMgr {
public:
    // N.B. this structure does NOT take ownership of device and window, they must be destroyed independently
    ImGuiMgr(Orbiter *app, SDL_GPUDevice *device, SDL_Window *window);

    virtual ~ImGuiMgr();

    WithLocalContext PushLocal() {
        const auto lastContext = ImGui::GetCurrentContext();
        ImGui::SetCurrentContext(m_context);
        return WithLocalContext(this, lastContext);
    };

    SDL_GPUDevice *Device() const { return m_device; }
    SDL_Window *Window() const { return m_window; }
    ImFont *DefaultFont() const { return m_defaultFont; }
    ImFont *ItalicFont() const { return m_italicFont; }
    ImFont *BoldFont() const { return m_boldFont; }
    ImFont *BoldItalicFont() const { return m_boldItalicFont; }
    ImFont *HdgFont() const { return m_hdgFont; }
    ImFont *MonoFont() const { return m_monoFont; }
    Orbiter *App() const { return m_app; }

private:
    Orbiter *m_app;
    SDL_GPUDevice *m_device;
    SDL_Window *m_window;
    ImGuiContext *m_context;
    ImFont *m_defaultFont;
    ImFont *m_italicFont;
    ImFont *m_boldFont;
    ImFont *m_boldItalicFont;
    ImFont *m_hdgFont;
    ImFont *m_monoFont;
};


class Image {
public:
    // N.B. this takes ownership of the surface and will destroy it.
    Image(SDL_GPUDevice *device, SDL_Window *window, SDL_Surface *surface,
          const std::string &path);

    Image(SDL_GPUDevice *device, SDL_Window *window,
          const std::string &path) : Image(
        device, window, IMG_Load(path.c_str()), path) {
    }

    ~Image();

    int Width() const { return m_surface->w; }
    int Height() const { return m_surface->h; }
    const std::string &Path() const { return m_path; }

    bool Valid() const {
        return m_device != nullptr && m_surface != nullptr && m_texture !=
               nullptr && m_sampler != nullptr;
    }

    SDL_GPUTextureSamplerBinding *Binding() { return &m_binding; }

private:
    std::string m_path;
    SDL_GPUDevice *m_device;
    SDL_Surface *m_surface;
    SDL_GPUTexture *m_texture;
    SDL_GPUSampler *m_sampler;
    SDL_GPUTextureSamplerBinding m_binding;
};


void Markdown(WithLocalContext &ctx, const std::string &md,
              std::vector<Image *> &loadedImages);

namespace ImGui {
    bool InputText(const char *label, std::string &buf,
                 ImGuiInputTextFlags flags = 0,
                 ImGuiInputTextCallback callback = nullptr,
                 void *user_data = nullptr);
    bool InputTextWithHint(const char *label, const char *hint, std::string &buf,
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
