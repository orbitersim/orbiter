#ifndef __SDLUTIL_H
#define __SDLUTIL_H

#include "GraphicsAPI.h"
#include "Log.h"
#include "Orbiter.h"
#include "SDLWrappers.h"
#include "imgui.h"
#include <SDL3/SDL.h>
#include <SDL3/SDL_gpu.h>
#include <SDL3_image/SDL_image.h>

class WithLpImCtx;
template <typename Elem> class SimpleTree {
  public:
    explicit SimpleTree(Elem item) : item(item), children() {};

    Elem item;
    std::vector<SimpleTree> children;
};

class LpImCtx final : public oapi::ImCtxBase {
  public:
    LpImCtx(Orbiter *app, const std::shared_ptr<sdl::ManagedWindow> &window);

    ~LpImCtx() override;

    [[nodiscard]] const std::shared_ptr<sdl::ManagedWindow> &Win() const {
        return m_window;
    }

  private:
    std::shared_ptr<sdl::ManagedWindow> m_window;

  protected:
    bool ConsumeEvent(const SDL_Event &event, bool &wantsOut) override;

    [[nodiscard]] bool BeginFrame() override;

    void EndFrame() override;
};

class LpImage {
  private:
    void Load(const std::shared_ptr<sdl::ManagedWindow> &win,
              SDL_Surface *origSurface, std::string_view path);

  public:
    LpImage(const LpImage &) = delete;

    LpImage &operator=(const LpImage &) = delete;

    // N.B. this takes ownership of the surface and will destroy it.
    LpImage(const std::shared_ptr<sdl::ManagedWindow> &win,
            SDL_Surface *origSurface, std::string_view path) {
        Load(win, origSurface, path);
    };

    LpImage(const std::shared_ptr<sdl::ManagedWindow> &win,
            std::string_view path) {
        auto surface = IMG_Load(path.data());
        if (!surface) {
            LOGOUT_ERR("Warning: load image `%s` failed: %s", path,
                       SDL_GetError());
            throw std::runtime_error("Failed to load image");
        }
        Load(win, surface, path);
    };

    LpImage(const WithLpImCtx &ctx, const std::string_view path);

    ~LpImage();

    [[nodiscard]] int Width() const { return m_surface->w; }
    [[nodiscard]] int Height() const { return m_surface->h; }
    [[nodiscard]] const std::string &Path() const { return m_path; }

    ImTextureID TexID() { return reinterpret_cast<ImTextureID>(&m_binding); }

  private:
    std::string m_path;
    std::shared_ptr<sdl::ManagedWindow> m_window;
    SDL_Surface *m_surface;
    SDL_GPUTexture *m_texture;
    SDL_GPUSampler *m_sampler;
    SDL_GPUTextureSamplerBinding m_binding;

    LpImage() = default;
};

class WithLpImCtx final : public oapi::WithImCtx {
  public:
    WithLpImCtx(const WithImCtx &ctx)
        : WithImCtx(ctx), inner(dynamic_cast<LpImCtx *>(ctx.Inner())) {}

    LpImCtx *operator->() const { return inner; }
    [[nodiscard]] LpImCtx *Inner() const { return inner; }

  private:
    LpImCtx *inner;
};

namespace ImGui {
void Markdown(const WithLpImCtx &ctx, std::string_view md,
              std::vector<std::shared_ptr<::LpImage>> &loadedImages);

void Markdown(const WithLpImCtx &ctx, std::string_view md);

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
} // namespace ImGui

#endif // __SDLUTIL_H
