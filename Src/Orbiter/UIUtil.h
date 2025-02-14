#ifndef __SDLUTIL_H
#define __SDLUTIL_H

#include "Log.h"
#include "Orbiter.h"
#include "SDLWrappers.h"
#include "imgui.h"
#include <SDL3/SDL.h>
#include <SDL3/SDL_gpu.h>
#include <SDL3_image/SDL_image.h>
#include <type_traits>

class LpImCtx;

template <class Ctx> class WithImCtx;

template <class Ctx> class ImCtxBase {
public:
  ImCtxBase(const ImCtxBase &) = delete;

  ImCtxBase &operator=(const ImCtxBase &) = delete;

  WithImCtx<Ctx> PushLocal() {
    const auto lastContext = ImGui::GetCurrentContext();
    ImGui::SetCurrentContext(m_context);
    return WithImCtx<Ctx>(static_cast<Ctx*>(this), lastContext);
  };

  [[nodiscard]] ImFont *DefaultFont() const { return m_defaultFont; }
  [[nodiscard]] ImFont *ItalicFont() const { return m_italicFont; }
  [[nodiscard]] ImFont *BoldFont() const { return m_boldFont; }
  [[nodiscard]] ImFont *BoldItalicFont() const { return m_boldItalicFont; }
  [[nodiscard]] ImFont *HdgFont() const { return m_hdgFont; }
  [[nodiscard]] ImFont *MonoFont() const { return m_monoFont; }
  [[nodiscard]] Orbiter *App() const { return m_app; }

protected:
  ImCtxBase() = default;
  Orbiter *m_app;
  ImGuiContext *m_context;
  ImFont *m_defaultFont;
  ImFont *m_italicFont;
  ImFont *m_boldFont;
  ImFont *m_boldItalicFont;
  ImFont *m_hdgFont;
  ImFont *m_monoFont;
};

template <class Ctx> class WithImCtx {
public:
  WithImCtx(const WithImCtx &) = delete;

  WithImCtx &operator=(const WithImCtx &) = delete;

  ~WithImCtx() {
    if (lastContext)
      ImGui::SetCurrentContext(lastContext);
  }

  Ctx *operator->() const { return inner; }

  bool ConsumeEvent(const SDL_Event &event, bool &wantsOut) const {
    return inner->ConsumeEvent(event, wantsOut);
  };

  [[nodiscard]] bool BeginFrame() const { return inner->BeginFrame(); };

  void EndFrame() const { return inner->EndFrame(); };

private:
  friend ImCtxBase<Ctx>;
  friend Ctx;
  WithImCtx(Ctx *inner, ImGuiContext *lastContext)
      : inner(inner), lastContext(lastContext) {};
  Ctx *inner;
  ImGuiContext *lastContext;
};

class LpImCtx final : public ImCtxBase<LpImCtx> {
  friend std::unique_ptr<LpImCtx>;

public:
  LpImCtx(Orbiter *app, const std::shared_ptr<Window> &window);

  ~LpImCtx();

  [[nodiscard]] const std::shared_ptr<Window> &Win() const { return m_window; }

private:
  friend WithImCtx<LpImCtx>;

  bool ConsumeEvent(const SDL_Event &event, bool &wantsOut) const;

  [[nodiscard]] bool BeginFrame() const;

  void EndFrame() const;

  std::shared_ptr<Window> m_window;
};

class LpImage {
private:
  void Load(const std::shared_ptr<Window> &win, SDL_Surface *origSurface,
            std::string_view path);

public:
  LpImage(const LpImage &) = delete;

  LpImage &operator=(const LpImage &) = delete;

  // N.B. this takes ownership of the surface and will destroy it.
  LpImage(const std::shared_ptr<Window> &win, SDL_Surface *origSurface,
        std::string_view path) {
    Load(win, origSurface, path);
  };

  LpImage(const std::shared_ptr<Window> &win, std::string_view path) {
    auto surface = IMG_Load(path.data());
    if (!surface) {
      LOGOUT_ERR("Warning: load image `%s` failed: %s", path, SDL_GetError());
      throw std::runtime_error("Failed to load image");
    }
    Load(win, surface, path);
  };

  LpImage(const WithImCtx<LpImCtx> &ctx, const std::string_view path)
      : LpImage(ctx->Win(), path) {};

  ~LpImage();

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

  LpImage() = default;
};

namespace ImGui {
void Markdown(WithImCtx<LpImCtx> &ctx, const std::string &md,
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
