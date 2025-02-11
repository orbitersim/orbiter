#include "SDLWrappers.h"

#include <Log.h>
#include <stdexcept>

sdl::ManagedWindow::ManagedWindow(const std::string_view title, const int width,
                                  const int height,
                                  const SDL_WindowFlags flags) {
    m_inner = SDL_CreateWindow(title.data(), width, height, flags);
    if (m_inner == nullptr) {
        LOGOUT_ERR("SDL_CreateWindow() failed: %s", SDL_GetError());
        throw std::runtime_error("Failed to create window");
    }

#ifdef _DEBUG
    m_device = SDL_CreateGPUDevice(SDL_GPU_SHADERFORMAT_SPIRV |
                                       SDL_GPU_SHADERFORMAT_DXIL |
                                       SDL_GPU_SHADERFORMAT_METALLIB,
                                   true, nullptr);
#else
    m_device = SDL_CreateGPUDevice(SDL_GPU_SHADERFORMAT_SPIRV |
                                       SDL_GPU_SHADERFORMAT_DXIL |
                                       SDL_GPU_SHADERFORMAT_METALLIB,
                                   false, nullptr);
#endif
    if (m_device == nullptr) {
        LOGOUT_ERR("SDL_CreateGPUDevice() failed: %s", SDL_GetError());
        throw std::runtime_error("Failed to create GPU device");
    }

    if (!SDL_ClaimWindowForGPUDevice(m_device, m_inner)) {
        LOGOUT_ERR("SDL_ClaimWindowForGPUDevice() failed: %s", SDL_GetError());
        SDL_DestroyGPUDevice(m_device);
        SDL_DestroyWindow(m_inner);
        throw std::runtime_error("Failed to claim window");
    }

    if (!SDL_SetGPUSwapchainParameters(m_device, m_inner,
                                       SDL_GPU_SWAPCHAINCOMPOSITION_SDR,
                                       SDL_GPU_PRESENTMODE_VSYNC)) {
        LOGOUT_ERR("SDL_SetGPUSwapchainParameters() failed: %s",
                   SDL_GetError());
        SDL_ReleaseWindowFromGPUDevice(m_device, m_inner);
        SDL_DestroyGPUDevice(m_device);
        SDL_DestroyWindow(m_inner);
        throw std::runtime_error("Failed to set swapchain parameters");
    };
}

sdl::ManagedWindow::~ManagedWindow() {
    if (m_device && m_inner)
        SDL_ReleaseWindowFromGPUDevice(m_device, m_inner);
    if (m_device)
        SDL_DestroyGPUDevice(m_device);
    if (m_inner)
        SDL_DestroyWindow(m_inner);
}

sdl::UnmanagedWindow::UnmanagedWindow(const std::string_view title,
                                      const int width, const int height,
                                      const SDL_WindowFlags flags) {
    m_inner = SDL_CreateWindow(title.data(), width, height, flags);
    if (m_inner == nullptr) {
        LOGOUT_ERR("SDL_CreateWindow() failed: %s", SDL_GetError());
        throw std::runtime_error("Failed to create window");
    }
}

sdl::UnmanagedWindow::~UnmanagedWindow() {
    if (m_inner)
        SDL_DestroyWindow(m_inner);
}
