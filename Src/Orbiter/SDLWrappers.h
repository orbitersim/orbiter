#ifndef SDLWRAPPERS_H
#define SDLWRAPPERS_H

#include <SDL3/SDL.h>
#include <string>

class Window {
public:
    Window(const Window &) = delete;

    Window &operator=(const Window &) = delete;

    Window(Window &&other) noexcept : m_inner(other.m_inner),
                                      m_device(other.m_device) {
        other.m_inner = nullptr;
        other.m_device = nullptr;
    }

    Window(std::string_view title, int width, int height,
           SDL_WindowFlags flags);

    ~Window();

    SDL_Window *Inner() const { return m_inner; }
    SDL_GPUDevice *Device() const { return m_device; }

private:
    SDL_Window *m_inner = nullptr;
    SDL_GPUDevice *m_device = nullptr;
};

#endif //SDLWRAPPERS_H
