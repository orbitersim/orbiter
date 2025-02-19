#include "UIUtil.h"

#include <IconsFontAwesome6.h>

#include "imgui.h"
#include "imgui_impl_sdl3.h"
#include "imgui_impl_sdlgpu3.h"
#include "imgui_md.h"

LpImCtx::LpImCtx(Orbiter *app,
                 const std::shared_ptr<sdl::ManagedWindow> &window)
    : ImCtxBase(app, ImGui::CreateContext()), m_window(window) {
    const auto savedContext = ImGui::GetCurrentContext();
    ImGui::SetCurrentContext(savedContext);

    if (!ImGui_ImplSDL3_InitForSDLGPU(m_window->Inner()))
        throw std::runtime_error("Failed to initialize ImGui/SDLGPU3");

    ImGui_ImplSDLGPU3_InitInfo init_info = {};
    init_info.GpuDevice = m_window->Device();
    init_info.ColorTargetFormat =
        SDL_GetGPUSwapchainTextureFormat(m_window->Device(), m_window->Inner());
    init_info.MSAASamples = SDL_GPU_SAMPLECOUNT_1;
    if (!ImGui_ImplSDLGPU3_Init(&init_info))
        throw std::runtime_error("Failed to initialize ImGui/SDLGPU3");

    if (savedContext)
        ImGui::SetCurrentContext(savedContext);
}

LpImCtx::~LpImCtx() {
    if (m_context) {
        WithLpImCtx _ = PushLocal();
        SDL_WaitForGPUIdle(m_window->Device());

        ImGui_ImplSDL3_Shutdown();
        ImGui_ImplSDLGPU3_Shutdown();
    }
}

LpImage::LpImage(const WithLpImCtx &ctx, const std::string_view path)
    : LpImage(ctx->Win(), path) {};

bool EventIsKeyboard(Uint32 type) {
    return type >= SDL_EVENT_KEY_UP && type < SDL_EVENT_MOUSE_MOTION;
}

bool EventIsMouse(Uint32 type) {
    return type >= SDL_EVENT_MOUSE_MOTION &&
           type < SDL_EVENT_JOYSTICK_AXIS_MOTION;
}

bool LpImCtx::ConsumeEvent(const SDL_Event &event, bool &wantsOut) {
    bool consumed = ImGui_ImplSDL3_ProcessEvent(&event);
    ImGuiIO &io = ImGui::GetIO();
    if ((io.WantCaptureMouse && EventIsMouse(event.type)) ||
        (io.WantCaptureMouse && EventIsKeyboard(event.type))) {
        consumed = true;
    }
    if (event.type == SDL_EVENT_QUIT ||
        (event.type == SDL_EVENT_WINDOW_CLOSE_REQUESTED &&
         event.window.windowID == SDL_GetWindowID((*this).Win()->Inner()))) {
        wantsOut = true;
        return true;
    }

    return consumed;
}

bool LpImCtx::BeginFrame() {
    if (SDL_GetWindowFlags(Win()->Inner()) & SDL_WINDOW_MINIMIZED) {
        return false;
    }
    ImGui_ImplSDLGPU3_NewFrame();
    ImGui_ImplSDL3_NewFrame();
    ImGui::NewFrame();

    return true;
}

void LpImCtx::EndFrame() {
    ImGui::Render();
    ImDrawData *draw_data = ImGui::GetDrawData();
    const bool is_minimized =
        (draw_data->DisplaySize.x <= 0.0f || draw_data->DisplaySize.y <= 0.0f);

    SDL_GPUCommandBuffer *command_buffer =
        SDL_AcquireGPUCommandBuffer((*this).Win()->Device());
    SDL_GPUTexture *swapchain_texture;
    SDL_AcquireGPUSwapchainTexture(command_buffer, (*this).Win()->Inner(),
                                   &swapchain_texture, nullptr, nullptr);
    if (swapchain_texture != nullptr && !is_minimized) {
        Imgui_ImplSDLGPU3_PrepareDrawData(draw_data, command_buffer);

        SDL_GPUColorTargetInfo target_info = {};
        target_info.texture = swapchain_texture;
        // #F0F4F8
        target_info.clear_color = SDL_FColor{0.941, 0.957, 0.973, 1.0};
        target_info.load_op = SDL_GPU_LOADOP_CLEAR;
        target_info.store_op = SDL_GPU_STOREOP_STORE;
        target_info.mip_level = 0;
        target_info.layer_or_depth_plane = 0;
        target_info.cycle = false;
        SDL_GPURenderPass *render_pass =
            SDL_BeginGPURenderPass(command_buffer, &target_info, 1, nullptr);

        ImGui_ImplSDLGPU3_RenderDrawData(draw_data, command_buffer,
                                         render_pass);

        SDL_EndGPURenderPass(render_pass);
    }

    SDL_SubmitGPUCommandBuffer(command_buffer);
}

void LpImage::Load(const std::shared_ptr<sdl::ManagedWindow> &win,
                   SDL_Surface *origSurface, std::string_view path) {
    if (!origSurface)
        throw std::invalid_argument("Image::Load: surface was null");
    auto format = SDL_PIXELFORMAT_ABGR8888;
    m_surface = origSurface;
    if (origSurface->format != format) {
        m_surface = SDL_ConvertSurface(origSurface, format);
        SDL_DestroySurface(origSurface);
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
    m_texture = SDL_CreateGPUTexture(win->Device(), &texture_info);
    if (!m_texture) {
        SDL_DestroySurface(m_surface);
        throw std::runtime_error("Failed to create texture");
    }

    SDL_GPUTransferBufferCreateInfo buf_info = {};
    buf_info.usage = SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD;
    buf_info.size = m_surface->w * m_surface->h * 4;

    auto transfer_buf = SDL_CreateGPUTransferBuffer(win->Device(), &buf_info);
    if (!transfer_buf) {
        SDL_ReleaseGPUTexture(win->Device(), m_texture);
        SDL_DestroySurface(m_surface);
        throw std::runtime_error("Failed to create transfer buffer");
    }

    auto transfer_ptr = static_cast<Uint8 *>(
        SDL_MapGPUTransferBuffer(win->Device(), transfer_buf, false));
    if (!transfer_ptr) {
        SDL_ReleaseGPUTransferBuffer(win->Device(), transfer_buf);
        SDL_ReleaseGPUTexture(win->Device(), m_texture);
        SDL_DestroySurface(m_surface);
        throw std::runtime_error("Failed to map transfer buffer");
    }
    SDL_memcpy(transfer_ptr, m_surface->pixels,
               m_surface->w * m_surface->h * 4);
    SDL_UnmapGPUTransferBuffer(win->Device(), transfer_buf);

    auto command_buffer = SDL_AcquireGPUCommandBuffer(win->Device());
    if (!command_buffer) {
        SDL_ReleaseGPUTransferBuffer(win->Device(), transfer_buf);
        SDL_ReleaseGPUTexture(win->Device(), m_texture);
        SDL_DestroySurface(m_surface);
        throw std::runtime_error("Failed to acquire command buffer");
    }
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
    if (!SDL_SubmitGPUCommandBuffer(command_buffer)) {
        SDL_ReleaseGPUTransferBuffer(win->Device(), transfer_buf);
        SDL_ReleaseGPUTexture(win->Device(), m_texture);
        SDL_DestroySurface(m_surface);
        throw std::runtime_error("Failed to submit command buffer");
    }
    SDL_ReleaseGPUTransferBuffer(win->Device(), transfer_buf);

    SDL_GPUSamplerCreateInfo sampler_info = {};
    sampler_info.min_filter = SDL_GPU_FILTER_LINEAR;
    sampler_info.mag_filter = SDL_GPU_FILTER_LINEAR;
    sampler_info.mipmap_mode = SDL_GPU_SAMPLERMIPMAPMODE_NEAREST;
    sampler_info.address_mode_u = SDL_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE;
    sampler_info.address_mode_v = SDL_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE;
    sampler_info.address_mode_w = SDL_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE;
    m_sampler = SDL_CreateGPUSampler(win->Device(), &sampler_info);
    if (!m_sampler) {
        SDL_ReleaseGPUTexture(win->Device(), m_texture);
        SDL_DestroySurface(m_surface);
        throw std::runtime_error("Failed to create sampler");
    }

    m_path = std::string(path);
    m_window = win;
    m_binding = {m_texture, m_sampler};
}

LpImage::~LpImage() {
    SDL_ReleaseGPUSampler(m_window->Device(), m_sampler);
    SDL_ReleaseGPUTexture(m_window->Device(), m_texture);
    SDL_DestroySurface(m_surface);
}

class orbiter_md final : public imgui_md {
  public:
    orbiter_md(
        const WithLpImCtx &ctx,
        const std::optional<std::vector<std::shared_ptr<LpImage>> *> images)
        : ctx(ctx), images(images) {}

    const WithLpImCtx &ctx;
    std::optional<std::vector<std::shared_ptr<LpImage>> *> images;

    [[nodiscard]] ImFont *get_font() const override {
        if (m_is_code || m_is_code_block)
            return ctx->MonoFont();
        if (m_is_em && m_is_strong)
            return ctx->BoldItalicFont();

        if (m_is_em)
            return ctx->ItalicFont();

        if (m_is_strong)
            return ctx->BoldFont();

        if (m_hlevel == 1)
            return ctx->HdgFont();

        return ctx->DefaultFont();
    }

    bool get_image(image_info &nfo) const override {
        if (!images.has_value())
            return false;
        auto images = this->images.value();
        std::shared_ptr<LpImage> image = nullptr;
        if (m_img_src.empty()) {
            return false;
        }
        for (const auto &loadedImg : *images) {
            if (loadedImg->Path() == m_img_src) {
                image = loadedImg;
            }
        }
        if (!image) {
            image = std::make_shared<LpImage>(
                dynamic_cast<LpImCtx *>(ctx.Inner())->Win(), m_img_src);
            images->push_back(image);
        }

        nfo.size = ImVec2(static_cast<float>(image->Width()),
                          static_cast<float>(image->Height()));
        nfo.texture_id = image->TexID();
        nfo.uv0 = {0, 0};
        nfo.uv1 = {1, 1};
        nfo.col_tint = {1, 1, 1, 1};
        nfo.col_border = {0, 0, 0, 0};
        return true;
    };

    void open_url() const override {
        if (!m_href.empty() && !m_is_image)
            SDL_OpenURL(m_href.c_str());
    };
};

void ImGui::Markdown(const WithLpImCtx &ctx, const std::string_view md,
                     std::vector<std::shared_ptr<::LpImage>> &loadedImages) {
    orbiter_md printer = {ctx, {&loadedImages}};
    printer.print(md.data(), md.data() + md.size());
}

void ImGui::Markdown(const WithLpImCtx &ctx, const std::string_view md) {
    orbiter_md printer = {ctx, std::nullopt};
    printer.print(md.data(), md.data() + md.size());
}

struct InputTextUserData {
    std::string &buf;
    ImGuiInputTextCallback chainCb;
    void *chainData;
};

int InputTextCallback(ImGuiInputTextCallbackData *data) {
    const auto userdata = static_cast<InputTextUserData *>(data->UserData);
    if (data->EventFlag == ImGuiInputTextFlags_CallbackResize) {
        // TODO: more efficient to use some kind of scale factor?
        if (data->BufTextLen != userdata->buf.size() &&
            data->BufTextLen <= userdata->buf.capacity())
            userdata->buf.resize(data->BufTextLen);
        if (data->BufTextLen <= userdata->buf.capacity())
            return 0;
        userdata->buf.reserve(data->BufTextLen + 1);
        data->Buf = userdata->buf.data();
    } else if (userdata->chainCb != nullptr) {
        data->UserData = userdata->chainData;
        return userdata->chainCb(data);
    }
    return 0;
}

bool ImGui::InputText(const char *label, std::string &buf,
                      ImGuiInputTextFlags flags,
                      const ImGuiInputTextCallback callback, void *user_data) {
#ifdef _DEBUG
    assert((flags & ImGuiInputTextFlags_CallbackResize) == 0);
#endif
    flags |= ImGuiInputTextFlags_CallbackResize;

    InputTextUserData data{buf, callback, user_data};
    return ImGui::InputText(label, buf.data(), buf.capacity(), flags,
                            InputTextCallback, &data);
}

bool ImGui::InputTextWithHint(const char *label, const char *hint,
                              std::string &buf, ImGuiInputTextFlags flags,
                              const ImGuiInputTextCallback callback,
                              void *user_data) {
#ifdef _DEBUG
    assert((flags & ImGuiInputTextFlags_CallbackResize) == 0);
#endif
    flags |= ImGuiInputTextFlags_CallbackResize;

    InputTextUserData data{buf, callback, user_data};
    return ImGui::InputTextWithHint(label, hint, buf.data(), buf.capacity(),
                                    flags, InputTextCallback, &data);
}

bool ImGui::InputTextMultiline(const char *label, std::string &buf,
                               const ImVec2 &size, ImGuiInputTextFlags flags,
                               const ImGuiInputTextCallback callback,
                               void *user_data) {
#ifdef _DEBUG
    assert((flags & ImGuiInputTextFlags_CallbackResize) == 0);
#endif
    flags |= ImGuiInputTextFlags_CallbackResize;

    InputTextUserData data{buf, callback, user_data};
    return ImGui::InputTextMultiline(label, buf.data(), buf.capacity(), size,
                                     flags, InputTextCallback, &data);
}
