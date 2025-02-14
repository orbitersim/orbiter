#include "UIUtil.h"

#include <IconsFontAwesome6.h>

#include "imgui_md.h"
#include "imgui_impl_sdl3.h"
#include "imgui_impl_sdlgpu3.h"
#include "imgui.h"

// Styling adapted from https://gist.github.com/dougbinks/8089b4bbaccaaf6fa204236978d165a9
static void ImGuiSetStyle(bool bStyleDark_, float alpha_) {
    // Setup Dear ImGui style
    ImGui::StyleColorsClassic();
    ImGui::StyleColorsLight();
    ImGuiStyle &style = ImGui::GetStyle();

    style.Alpha = 1.0f;
    style.FrameRounding = 3.0f;
    style.WindowRounding = 3.0f;
    style.ChildRounding = 3.0f;
    style.PopupRounding = 3.0f;
    style.ScrollbarRounding = 3.0f;
    style.GrabRounding = 3.0f;
    style.TabRounding = 3.0f;
    style.WindowMenuButtonPosition = ImGuiDir_Right;
    return;
    // light style from Pacôme Danhiez (user itamago) https://github.com/ocornut/imgui/pull/511#issuecomment-175719267
    style.Colors[ImGuiCol_Text] = ImVec4(0.00f, 0.00f, 0.00f, 1.00f);
    style.Colors[ImGuiCol_TextDisabled] = ImVec4(0.60f, 0.60f, 0.60f, 1.00f);
    style.Colors[ImGuiCol_WindowBg] = ImVec4(0.94f, 0.94f, 0.94f, 0.94f);
    style.Colors[ImGuiCol_PopupBg] = ImVec4(1.00f, 1.00f, 1.00f, 0.94f);
    style.Colors[ImGuiCol_Border] = ImVec4(0.00f, 0.00f, 0.00f, 0.39f);
    style.Colors[ImGuiCol_BorderShadow] = ImVec4(1.00f, 1.00f, 1.00f, 0.10f);
    style.Colors[ImGuiCol_FrameBg] = ImVec4(1.00f, 1.00f, 1.00f, 0.94f);
    style.Colors[ImGuiCol_FrameBgHovered] = ImVec4(0.26f, 0.59f, 0.98f, 0.40f);
    style.Colors[ImGuiCol_FrameBgActive] = ImVec4(0.26f, 0.59f, 0.98f, 0.67f);
    style.Colors[ImGuiCol_TitleBg] = ImVec4(0.96f, 0.96f, 0.96f, 1.00f);
    style.Colors[ImGuiCol_TitleBgCollapsed] =
            ImVec4(1.00f, 1.00f, 1.00f, 0.51f);
    style.Colors[ImGuiCol_TitleBgActive] = ImVec4(0.82f, 0.82f, 0.82f, 1.00f);
    style.Colors[ImGuiCol_MenuBarBg] = ImVec4(0.86f, 0.86f, 0.86f, 1.00f);
    style.Colors[ImGuiCol_ScrollbarBg] = ImVec4(0.98f, 0.98f, 0.98f, 0.53f);
    style.Colors[ImGuiCol_ScrollbarGrab] = ImVec4(0.69f, 0.69f, 0.69f, 1.00f);
    style.Colors[ImGuiCol_ScrollbarGrabHovered] = ImVec4(
        0.59f, 0.59f, 0.59f, 1.00f);
    style.Colors[ImGuiCol_ScrollbarGrabActive] = ImVec4(
        0.49f, 0.49f, 0.49f, 1.00f);
    style.Colors[ImGuiCol_CheckMark] = ImVec4(0.26f, 0.59f, 0.98f, 1.00f);
    style.Colors[ImGuiCol_SliderGrab] = ImVec4(0.24f, 0.52f, 0.88f, 1.00f);
    style.Colors[ImGuiCol_SliderGrabActive] =
            ImVec4(0.26f, 0.59f, 0.98f, 1.00f);
    style.Colors[ImGuiCol_Button] = ImVec4(0.26f, 0.59f, 0.98f, 0.40f);
    style.Colors[ImGuiCol_ButtonHovered] = ImVec4(0.26f, 0.59f, 0.98f, 1.00f);
    style.Colors[ImGuiCol_ButtonActive] = ImVec4(0.06f, 0.53f, 0.98f, 1.00f);
    style.Colors[ImGuiCol_Header] = ImVec4(0.26f, 0.59f, 0.98f, 0.31f);
    style.Colors[ImGuiCol_HeaderHovered] = ImVec4(0.26f, 0.59f, 0.98f, 0.80f);
    style.Colors[ImGuiCol_HeaderActive] = ImVec4(0.26f, 0.59f, 0.98f, 1.00f);
    style.Colors[ImGuiCol_ResizeGrip] = ImVec4(1.00f, 1.00f, 1.00f, 0.50f);
    style.Colors[ImGuiCol_ResizeGripHovered] = ImVec4(
        0.26f, 0.59f, 0.98f, 0.67f);
    style.Colors[ImGuiCol_ResizeGripActive] =
            ImVec4(0.26f, 0.59f, 0.98f, 0.95f);
    style.Colors[ImGuiCol_PlotLines] = ImVec4(0.39f, 0.39f, 0.39f, 1.00f);
    style.Colors[ImGuiCol_PlotLinesHovered] =
            ImVec4(1.00f, 0.43f, 0.35f, 1.00f);
    style.Colors[ImGuiCol_PlotHistogram] = ImVec4(0.90f, 0.70f, 0.00f, 1.00f);
    style.Colors[ImGuiCol_PlotHistogramHovered] = ImVec4(
        1.00f, 0.60f, 0.00f, 1.00f);
    style.Colors[ImGuiCol_TextSelectedBg] = ImVec4(0.26f, 0.59f, 0.98f, 0.35f);

    if (bStyleDark_) {
        for (int i = 0; i <= ImGuiCol_COUNT; i++) {
            ImVec4 &col = style.Colors[i];
            float H, S, V;
            ImGui::ColorConvertRGBtoHSV(col.x, col.y, col.z, H, S, V);

            if (S < 0.1f) {
                V = 1.0f - V;
            }
            ImGui::ColorConvertHSVtoRGB(H, S, V, col.x, col.y, col.z);
            if (col.w < 1.00f) {
                col.w *= alpha_;
            }
        }
    } else {
        for (int i = 0; i <= ImGuiCol_COUNT; i++) {
            ImVec4 &col = style.Colors[i];
            if (col.w < 1.00f) {
                col.x *= alpha_;
                col.y *= alpha_;
                col.z *= alpha_;
                col.w *= alpha_;
            }
        }
    }
}

LpImCtx::LpImCtx(
    Orbiter *app,
    const std::shared_ptr<Window> &window) : ImCtxBase() {
    m_app = app;
    m_window = window;
#ifdef _DEBUG
    IMGUI_CHECKVERSION();
#endif
    m_context = ImGui::CreateContext();
    auto savedContext = ImGui::GetCurrentContext();
    ImGui::SetCurrentContext(savedContext);

    ImGuiIO &io = ImGui::GetIO();
    if (!m_app->IsFullscreen())
        io.ConfigFlags |= ImGuiConfigFlags_ViewportsEnable;
    io.ConfigFlags |= ImGuiConfigFlags_NavEnableKeyboard;
    io.IniFilename = nullptr;
    io.LogFilename = nullptr;

    ImGuiSetStyle(false, 1.0f);

    ImFontConfig config;

    static const ImWchar icons_ranges[] = {ICON_MIN_FA, ICON_MAX_FA, 0};
    ImFontConfig icons_config;
    icons_config.MergeMode = true;
    icons_config.PixelSnapH = true;
    icons_config.FontDataOwnedByAtlas = false;
    icons_config.GlyphOffset.y = 1;

    const CFG_FONTPRM &prm = m_app->Cfg()->CfgFontPrm;

    auto defaultFontFile = std::string(prm.ImGui_FontName).append(
        "-Regular.ttf");
    auto italicFontFile = std::string(prm.ImGui_FontName).append("-Italic.ttf");
    auto boldFontFile = std::string(prm.ImGui_FontName).append("-Bold.ttf");
    auto boldItalicFontFile = std::string(prm.ImGui_FontName).append(
        "-BoldItalic.ttf");

    m_defaultFont = io.Fonts->AddFontFromFileTTF(
        defaultFontFile.c_str(), prm.ImGui_FontSize, &config,
        ImGui::GetIO().Fonts->GetGlyphRangesJapanese());
    io.Fonts->AddFontFromFileTTF("fa-solid-900.ttf", prm.ImGui_FontSize,
                                 &icons_config, icons_ranges);
    m_italicFont = io.Fonts->AddFontFromFileTTF(italicFontFile.c_str(),
                                                prm.ImGui_FontSize, &config,
                                                ImGui::GetIO().Fonts->
                                                GetGlyphRangesJapanese());
    io.Fonts->AddFontFromFileTTF("fa-solid-900.ttf", prm.ImGui_FontSize,
                                 &icons_config, icons_ranges);
    m_boldFont = io.Fonts->AddFontFromFileTTF(boldFontFile.c_str(),
                                              prm.ImGui_FontSize, &config,
                                              ImGui::GetIO().Fonts->
                                              GetGlyphRangesJapanese());
    io.Fonts->AddFontFromFileTTF("fa-solid-900.ttf", prm.ImGui_FontSize,
                                 &icons_config, icons_ranges);
    m_boldItalicFont = io.Fonts->AddFontFromFileTTF(
        boldItalicFontFile.c_str(), prm.ImGui_FontSize, &config,
        ImGui::GetIO().Fonts->GetGlyphRangesJapanese());
    io.Fonts->AddFontFromFileTTF("fa-solid-900.ttf", prm.ImGui_FontSize,
                                 &icons_config, icons_ranges);
    m_hdgFont = io.Fonts->AddFontFromFileTTF(defaultFontFile.c_str(),
                                             prm.ImGui_FontSize * 1.5f,
                                             &config,
                                             ImGui::GetIO().Fonts->
                                             GetGlyphRangesJapanese());
    io.Fonts->AddFontFromFileTTF("fa-solid-900.ttf", prm.ImGui_FontSize,
                                 &icons_config, icons_ranges);
    // weird alignment issues otherwise, yippee (sarcastic)
    config.GlyphOffset.y = 1;
    m_monoFont = io.Fonts->AddFontFromFileTTF("Cousine-Regular.ttf",
                                              prm.ImGui_FontSize, &config,
                                              ImGui::GetIO().Fonts->
                                              GetGlyphRangesJapanese());
    io.Fonts->Build();

    if (!ImGui_ImplSDL3_InitForSDLGPU(m_window->Inner()))
        throw std::runtime_error("Failed to initialize ImGui/SDLGPU3");

    ImGui_ImplSDLGPU3_InitInfo init_info = {};
    init_info.GpuDevice = m_window->Device();
    init_info.ColorTargetFormat = SDL_GetGPUSwapchainTextureFormat(
        m_window->Device(), m_window->Inner());
    init_info.MSAASamples = SDL_GPU_SAMPLECOUNT_1;
    if (!ImGui_ImplSDLGPU3_Init(&init_info))
        throw std::runtime_error("Failed to initialize ImGui/SDLGPU3");

    if (savedContext)
        ImGui::SetCurrentContext(savedContext);
}

LpImCtx::~LpImCtx() {
    if (m_context) {
        WithImCtx<LpImCtx> _ = PushLocal();
        SDL_WaitForGPUIdle(m_window->Device());

        ImGui_ImplSDL3_Shutdown();
        ImGui_ImplSDLGPU3_Shutdown();
        ImGui::DestroyContext();
    }
}

bool EventIsKeyboard(Uint32 type) {
    return type >= SDL_EVENT_KEY_UP && type < SDL_EVENT_MOUSE_MOTION;
}

bool EventIsMouse(Uint32 type) {
    return type >= SDL_EVENT_MOUSE_MOTION && type <
           SDL_EVENT_JOYSTICK_AXIS_MOTION;
}

bool LpImCtx::ConsumeEvent(const SDL_Event &event,
                              bool &wantsOut) const {
    bool consumed = ImGui_ImplSDL3_ProcessEvent(&event);
    ImGuiIO &io = ImGui::GetIO();
    if ((io.WantCaptureMouse && EventIsMouse(event.type)) || (
            io.WantCaptureMouse && EventIsKeyboard(event.type))) {
        consumed = true;
    }
    if (event.type == SDL_EVENT_QUIT || (
            event.type == SDL_EVENT_WINDOW_CLOSE_REQUESTED && event.window.
            windowID ==
            SDL_GetWindowID((*this).Win()->Inner()))) {
        wantsOut = true;
        return true;
    }

    return consumed;
}

bool LpImCtx::BeginFrame() const {
    if (SDL_GetWindowFlags(Win()->Inner()) & SDL_WINDOW_MINIMIZED) {
        return false;
    }
    ImGui_ImplSDLGPU3_NewFrame();
    ImGui_ImplSDL3_NewFrame();
    ImGui::NewFrame();

    return true;
}

void LpImCtx::EndFrame() const {
    ImGui::Render();
    ImDrawData *draw_data = ImGui::GetDrawData();
    const bool is_minimized = (draw_data->DisplaySize.x <= 0.0f || draw_data->
                               DisplaySize.y <= 0.0f);

    SDL_GPUCommandBuffer *command_buffer = SDL_AcquireGPUCommandBuffer(
        (*this).Win()->Device());
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
        SDL_GPURenderPass *render_pass = SDL_BeginGPURenderPass(
            command_buffer, &target_info, 1, nullptr);

        ImGui_ImplSDLGPU3_RenderDrawData(draw_data, command_buffer,
                                         render_pass);

        SDL_EndGPURenderPass(render_pass);
    }

    SDL_SubmitGPUCommandBuffer(command_buffer);
}


void LpImage::Load(
    const std::shared_ptr<Window> &win, SDL_Surface *origSurface,
    std::string_view path) {
    if (!origSurface)
        throw std::invalid_argument(
            "Image::Load: surface was null");
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

    auto transfer_buf = SDL_CreateGPUTransferBuffer(
        win->Device(),
        &buf_info
    );
    if (!transfer_buf) {
        SDL_ReleaseGPUTexture(win->Device(), m_texture);
        SDL_DestroySurface(m_surface);
        throw std::runtime_error("Failed to create transfer buffer");
    }

    auto transfer_ptr = static_cast<Uint8 *>(SDL_MapGPUTransferBuffer(
        win->Device(),
        transfer_buf,
        false
    ));
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

struct MarkdownUserData {
    WithImCtx<LpImCtx> &ctx;
    std::vector<LpImage *> &images;
};

class orbiter_md : public imgui_md {
public:
    orbiter_md(WithImCtx<LpImCtx> &ctx,
               std::vector<std::shared_ptr<LpImage> > &images) : ctx(ctx),
        images(images) {
    }

    WithImCtx<LpImCtx> &ctx;
    std::vector<std::shared_ptr<LpImage> > &images;

    ImFont *get_font() const override {
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
        std::shared_ptr<LpImage> image = nullptr;
        if (m_img_src.empty()) {
            return false;
        }
        for (const auto loadedImg: images) {
            if (loadedImg->Path() == m_img_src) {
                image = loadedImg;
            }
        }
        if (!image) {
            image = std::make_shared<LpImage>(ctx->Win(),
                                            m_img_src);
            images.push_back(image);
        }

        nfo.size = ImVec2(image->Width(), image->Height());
        nfo.texture_id = reinterpret_cast<ImTextureID>(image->Binding());
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

void ImGui::Markdown(WithImCtx<LpImCtx> &ctx, const std::string &md,
                     std::vector<std::shared_ptr<::LpImage> > &loadedImages) {
    orbiter_md printer = {ctx, loadedImages};
    printer.print(md.c_str(), md.c_str() + md.size());
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
        if (data->BufTextLen != userdata->buf.size() && data->BufTextLen <=
            userdata->buf.capacity())
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
                      const ImGuiInputTextCallback callback,
                      void *user_data) {
    ImGui::PushStyleVar(ImGuiStyleVar_FrameBorderSize, 1.0f);
#ifdef _DEBUG
    assert((flags & ImGuiInputTextFlags_CallbackResize) == 0);
#endif
    flags |= ImGuiInputTextFlags_CallbackResize;

    InputTextUserData data{buf, callback, user_data};
    const bool res = ImGui::InputText(label, buf.data(), buf.capacity(), flags,
                                      InputTextCallback, &data);
    ImGui::PopStyleVar();
    return res;
}

bool ImGui::InputTextWithHint(const char *label, const char *hint,
                              std::string &buf,
                              ImGuiInputTextFlags flags,
                              const ImGuiInputTextCallback callback,
                              void *user_data) {
    ImGui::PushStyleVar(ImGuiStyleVar_FrameBorderSize, 1.0f);
#ifdef _DEBUG
    assert((flags & ImGuiInputTextFlags_CallbackResize) == 0);
#endif
    flags |= ImGuiInputTextFlags_CallbackResize;

    InputTextUserData data{buf, callback, user_data};
    const bool res = ImGui::InputTextWithHint(label, hint, buf.data(),
                                              buf.capacity(), flags,
                                              InputTextCallback, &data);
    ImGui::PopStyleVar();
    return res;
}

bool ImGui::InputTextMultiline(const char *label, std::string &buf,
                               const ImVec2 &size,
                               ImGuiInputTextFlags flags,
                               const ImGuiInputTextCallback callback,
                               void *user_data) {
    ImGui::PushStyleVar(ImGuiStyleVar_FrameBorderSize, 1.0f);
#ifdef _DEBUG
    assert((flags & ImGuiInputTextFlags_CallbackResize) == 0);
#endif
    flags |= ImGuiInputTextFlags_CallbackResize;

    InputTextUserData data{buf, callback, user_data};
    const bool res = ImGui::InputTextMultiline(label, buf.data(),
                                               buf.capacity(), size, flags,
                                               InputTextCallback, &data);
    ImGui::PopStyleVar();
    return res;
}
