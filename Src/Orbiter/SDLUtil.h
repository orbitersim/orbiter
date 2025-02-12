#ifndef __SDLUTIL_H
#define __SDLUTIL_H

#include <OrbiterAPI.h>
#include <SDL3/SDL.h>
#include <SDL3/SDL_gpu.h>
#include <SDL3_image/SDL_image.h>

class Image {
public:
    // N.B. this takes ownership of the surface and will destroy it.
    Image(SDL_GPUDevice *device, SDL_Window *window, SDL_Surface *surface);

    Image(SDL_GPUDevice *device, SDL_Window *window, const char *path) : Image(device, window, IMG_Load(path)) {
    }

    ~Image();

    bool Valid() const {
        return m_device != nullptr && m_surface != nullptr && m_texture != nullptr && m_sampler != nullptr;
    }

    SDL_GPUTextureSamplerBinding *Binding() { return &m_binding; }

private:
    SDL_GPUDevice *m_device;
    SDL_Surface *m_surface;
    SDL_GPUTexture *m_texture;
    SDL_GPUSampler *m_sampler;
    SDL_GPUTextureSamplerBinding m_binding;
};

#endif //SDLUTIL_H
