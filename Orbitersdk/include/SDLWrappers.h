#ifndef SDLWRAPPERS_H
#define SDLWRAPPERS_H

#include <SDL3/SDL.h>
#include <string>
#include <windows.h>
#include <OrbiterAPI.h>

namespace sdl {

class OAPIFUNC UnmanagedWindow {
public:
  UnmanagedWindow(const UnmanagedWindow &) = delete;
  UnmanagedWindow &operator=(const UnmanagedWindow &) = delete;

  UnmanagedWindow(UnmanagedWindow &&other) noexcept : m_inner(other.m_inner) {
    other.m_inner = nullptr;
  }

  UnmanagedWindow(std::string_view title, int width, int height,
                  SDL_WindowFlags flags);

  ~UnmanagedWindow();

  [[nodiscard]] SDL_Window *Inner() const { return m_inner; }

  [[nodiscard]] HWND Win32Handle() const {
    return (HWND)SDL_GetPointerProperty(SDL_GetWindowProperties(m_inner),
                                        SDL_PROP_WINDOW_WIN32_HWND_POINTER,
                                        nullptr);
  }

private:
  SDL_Window *m_inner = nullptr;
};

class OAPIFUNC ManagedWindow {
public:
  ManagedWindow(const ManagedWindow &) = delete;

  ManagedWindow &operator=(const ManagedWindow &) = delete;

  ManagedWindow(ManagedWindow &&other) noexcept
      : m_inner(other.m_inner), m_device(other.m_device) {
    other.m_inner = nullptr;
    other.m_device = nullptr;
  }

  ManagedWindow(std::string_view title, int width, int height,
                SDL_WindowFlags flags);

  ~ManagedWindow();

  [[nodiscard]] SDL_Window *Inner() const { return m_inner; }
  [[nodiscard]] SDL_GPUDevice *Device() const { return m_device; }

private:
  SDL_Window *m_inner = nullptr;
  SDL_GPUDevice *m_device = nullptr;
};

} // namespace sdl

#endif // SDLWRAPPERS_H
