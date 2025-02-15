#ifndef __SDLUTIL_H
#define __SDLUTIL_H

#include "Log.h"
#include "Orbiter.h"
#include "SDLWrappers.h"
#include "imgui.h"
#include <SDL3/SDL.h>
#include <SDL3/SDL_gpu.h>
#include <SDL3_image/SDL_image.h>
#include "GraphicsAPI.h"

template<typename Elem>
class SimpleTree {
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

  oapi::WithImCtx<LpImCtx> PushLocal() {
    return PushLocal_<LpImCtx>();
  }

private:
  friend oapi::WithImCtx<LpImCtx>;

  std::shared_ptr<sdl::ManagedWindow> m_window;
protected:
  bool ConsumeEvent(const SDL_Event &event, bool &wantsOut) override;

  [[nodiscard]] bool BeginFrame() override;

  void EndFrame() override;
};

class LpImage {
private:
  void Load(const std::shared_ptr<sdl::ManagedWindow> &win, SDL_Surface *origSurface,
            std::string_view path);

public:
  LpImage(const LpImage &) = delete;

  LpImage &operator=(const LpImage &) = delete;

  // N.B. this takes ownership of the surface and will destroy it.
  LpImage(const std::shared_ptr<sdl::ManagedWindow> &win, SDL_Surface *origSurface,
          std::string_view path) {
    Load(win, origSurface, path);
  };

  LpImage(const std::shared_ptr<sdl::ManagedWindow> &win, std::string_view path) {
    auto surface = IMG_Load(path.data());
    if (!surface) {
      LOGOUT_ERR("Warning: load image `%s` failed: %s", path, SDL_GetError());
      throw std::runtime_error("Failed to load image");
    }
    Load(win, surface, path);
  };

  LpImage(const oapi::WithImCtx<LpImCtx> &ctx, const std::string_view path)
      : LpImage(ctx->Win(), path) {};

  ~LpImage();

  [[nodiscard]] int Width() const { return m_surface->w; }
  [[nodiscard]] int Height() const { return m_surface->h; }
  [[nodiscard]] const std::string &Path() const { return m_path; }

  SDL_GPUTextureSamplerBinding *Binding() { return &m_binding; }

private:
  std::string m_path;
  std::shared_ptr<sdl::ManagedWindow> m_window;
  SDL_Surface *m_surface;
  SDL_GPUTexture *m_texture;
  SDL_GPUSampler *m_sampler;
  SDL_GPUTextureSamplerBinding m_binding;

  LpImage() = default;
};

namespace ImGui {
void Markdown(oapi::WithImCtx<LpImCtx> &ctx, std::string_view md,
              std::vector<std::shared_ptr<::LpImage>> &loadedImages);

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
