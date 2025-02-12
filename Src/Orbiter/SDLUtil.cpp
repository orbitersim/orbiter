#include "SDLUtil.h"

Image::Image(SDL_GPUDevice *device, SDL_Window *window, SDL_Surface *surface) : m_device(device), m_surface(nullptr),
    m_texture(nullptr),
    m_sampler(nullptr), m_binding({}) {
    auto format = SDL_PIXELFORMAT_ABGR8888;
    if (surface->format != format) {
        m_surface = SDL_ConvertSurface(surface, format);
        SDL_DestroySurface(surface);
    } else {
        m_surface = surface;
    }
    SDL_GPUTextureCreateInfo texture_info = {};
    texture_info.type = SDL_GPU_TEXTURETYPE_2D;
    texture_info.format = SDL_GPU_TEXTUREFORMAT_R8G8B8A8_UNORM;
    texture_info.usage = SDL_GPU_TEXTUREUSAGE_SAMPLER;
    texture_info.width = m_surface->w;
    texture_info.height = m_surface->h;
    texture_info.layer_count_or_depth = 1;
    texture_info.num_levels = 1;
    texture_info.sample_count = SDL_GPU_SAMPLECOUNT_1;
    m_texture = SDL_CreateGPUTexture(m_device, &texture_info);

    SDL_GPUTransferBufferCreateInfo buf_info = {};
    buf_info.usage = SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD;
    buf_info.size = m_surface->w * m_surface->h * 4;

    auto transfer_buf = SDL_CreateGPUTransferBuffer(
        m_device,
        &buf_info
    );

    auto transfer_ptr = static_cast<Uint8 *>(SDL_MapGPUTransferBuffer(
        m_device,
        transfer_buf,
        false
    ));
    SDL_memcpy(transfer_ptr, m_surface->pixels, m_surface->w * m_surface->h * 4);
    SDL_UnmapGPUTransferBuffer(m_device, transfer_buf);

    auto command_buffer = SDL_AcquireGPUCommandBuffer(m_device);
    auto copy_pass = SDL_BeginGPUCopyPass(command_buffer);
    SDL_GPUTextureTransferInfo src_info = {};
    src_info.transfer_buffer = transfer_buf;

    SDL_GPUTextureRegion dst_info = {};
    dst_info.texture = m_texture;
    dst_info.w = m_surface->w;
    dst_info.h = m_surface->h;
    dst_info.d = 1;

    SDL_UploadToGPUTexture(copy_pass, &src_info, &dst_info, false);
    SDL_EndGPUCopyPass(copy_pass);
    SDL_SubmitGPUCommandBuffer(command_buffer);
    SDL_ReleaseGPUTransferBuffer(m_device, transfer_buf);

    SDL_GPUSamplerCreateInfo sampler_info = {};
    sampler_info.min_filter = SDL_GPU_FILTER_LINEAR;
    sampler_info.mag_filter = SDL_GPU_FILTER_LINEAR;
    sampler_info.mipmap_mode = SDL_GPU_SAMPLERMIPMAPMODE_NEAREST;
    sampler_info.address_mode_u = SDL_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE;
    sampler_info.address_mode_v = SDL_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE;
    sampler_info.address_mode_w = SDL_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE;
    m_sampler = SDL_CreateGPUSampler(m_device, &sampler_info);

    m_binding.texture = m_texture;
    m_binding.sampler = m_sampler;
}

Image::~Image() {
    if (Valid()) {
        SDL_ReleaseGPUSampler(m_device, m_sampler);
        SDL_ReleaseGPUTexture(m_device, m_texture);
        SDL_DestroySurface(m_surface);
    }
}
