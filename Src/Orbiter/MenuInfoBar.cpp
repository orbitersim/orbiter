// Copyright (c) Martin Schweiger
// Licensed under the MIT License
#define IMGUI_DEFINE_MATH_OPERATORS
#include "OrbiterAPI.h"
#include "imgui.h"
#include "imgui_internal.h"

#include "MenuInfoBar.h"
#include "Camera.h"
#include "Dialogs.h"
#include "DlgMenuCfg.h"

// =======================================================================
// Externs

extern Orbiter *g_pOrbiter;
extern Camera *g_camera;
extern TimeData td;

const ImGuiWindowFlags INFOFLAGS = ImGuiWindowFlags_NoTitleBar |
	ImGuiWindowFlags_NoInputs |
	ImGuiWindowFlags_NoNav |
	ImGuiWindowFlags_NoScrollbar |
	ImGuiWindowFlags_AlwaysAutoResize |
	ImGuiWindowFlags_NoCollapse |
	ImGuiWindowFlags_NoMove |
	ImGuiWindowFlags_NoSavedSettings |
	ImGuiWindowFlags_NoFocusOnAppearing |
	ImGuiWindowFlags_NoBringToFrontOnFocus |
	ImGuiWindowFlags_NoDocking;


class DynamicMenuBar: public ImGuiDialog
{
	static inline const float strength = 0.3f;
	CFG_UIPRM &prm;

	// Until we get it from C++20
	static inline float lerp(float a, float b, float t) {
			return a + t * (b - a);
	}
public:
	struct MenuItem {
		std::string label;
		SURFHANDLE texture;
		CustomFunc func;
		void *context;
		ImTextureID texId;
		int id;
		float lastItemSize;
		bool dragging;
		float animStateScroll;
		float animStateScale;
		bool enabled;
		MenuItem(const char *label_, const char *path, int id_, CustomFunc func_, void *context_):label(label_),func(func_),context(context_),id(id_) {
			texture = oapiLoadTexture(path);
			if(!texture) oapiAddNotification(OAPINOTIF_ERROR, "Cannot find menu item texture", path);
			texId = ImGui::GetImTextureID(texture);
			lastItemSize = g_pOrbiter->Cfg()->CfgUIPrm.MenuButtonSize;
			dragging = false;
			enabled = true;
			animStateScroll = 0.0f;
			animStateScale = 0.0f;
		}
		~MenuItem() {
			if(texture)
				oapiDestroySurface(texture);
		}

		// We don't want to be able to copy because of the SURFHANDLE
		MenuItem(const MenuItem&) = delete;
		MenuItem& operator=(const MenuItem&) = delete;
		MenuItem(MenuItem&& other) noexcept {
			*this = std::move(other);
		}
		MenuItem& MenuItem::operator=(MenuItem&& other) noexcept {
			label = std::move(other.label);
			texture = other.texture;
			other.texture = NULL;
			func = other.func;
			context = other.context;
			texId = other.texId;
			lastItemSize = other.lastItemSize;
			id = other.id;
			dragging = other.dragging;
			enabled = other.enabled;
			animStateScroll = other.animStateScroll;
			animStateScale = other.animStateScale;
			return *this;
		}
	};

	const ImGuiWindowFlags MENUFLAGS = ImGuiWindowFlags_NoTitleBar |
		ImGuiWindowFlags_NoScrollbar |
		ImGuiWindowFlags_AlwaysAutoResize |
		ImGuiWindowFlags_NoCollapse |
		ImGuiWindowFlags_NoMove |
		ImGuiWindowFlags_NoSavedSettings |
		ImGuiWindowFlags_NoBackground |
		ImGuiWindowFlags_NoFocusOnAppearing |
		ImGuiWindowFlags_DockNodeHost |
		ImGuiWindowFlags_NoBringToFrontOnFocus |
		ImGuiWindowFlags_NoDocking;
	void OnDraw() {}
	std::vector<MenuItem> items;
	float animState;
	float animStateSpacing;
	std::vector<MenuInfoBar::MenuPreference> preferedOrder;

	// Dragging
	ImVec2 dragPos = {0,0};
	bool isDragging;
	bool oneDrag;
	bool isDraggingOutside;
	int swapId = -1;
	ImTextureID dragTexId = 0;
	int dragId = -1;
	float holePos;


	DynamicMenuBar(): ImGuiDialog("DynamicMenu"), prm(g_pOrbiter->Cfg()->CfgUIPrm) {
		animState = 0.0f;
		isDragging = false;
		oneDrag = false;
		isDraggingOutside = false;
		animStateSpacing = prm.MenuButtonSpacing;

		FILEHANDLE f = oapiOpenFile("MenuInfoBar.cfg", FILE_IN, CONFIG);
		if(f) {
			for(int i = 1;;i++) {
				char buf[64];
				char dst[256];
				bool state;
				sprintf(buf, "MenuItem%d", i);
				bool s = oapiReadItem_string (f, buf, dst);
				sprintf(buf, "MenuItemEnabled%d", i);
				s &= oapiReadItem_bool (f, buf, state);
				if(!s)
					break;
				preferedOrder.emplace_back(MenuInfoBar::MenuPreference{dst, state});
			}

			oapiCloseFile(f, FILE_IN);
		}

		items.reserve(g_pOrbiter->menuitems.size());
		for(auto &item: g_pOrbiter->menuitems) {
			AddMenuItem(item.label.c_str(), item.imagepath.c_str(), item.id, item.func, item.context);
		}
		// Save the current state if the file did not exist
		if(!f) {
			SavePreferedOrder();
		}
	}
	// Test if labela appears before labelb in preferedOrder 
	bool Before(const char *labela, const char *labelb) {
		for(const auto &item: preferedOrder) {
			if(item.label == labela)
				return true;
			if(item.label == labelb)
				return false;
		}
		return false;
	}

	void AddMenuItem(const char *label, const char *imagepath, int id, CustomFunc func, void *context = NULL) {
		const auto &known = std::find_if(preferedOrder.begin(), preferedOrder.end(), [label](const auto &item){ return item.label == label;});
		if(known != preferedOrder.end()) {
			// Insert the new item so that it respects the order provided by preferedOrder
			for(auto it = items.begin(); it!=items.end();++it) {
				if(!Before(it->label.c_str(), label)) {
					auto newItem = items.emplace(it, label, imagepath, id, func, context);
					newItem->enabled = known->enabled;
					return;
				}
			}
			// Add to the back if no candidate was found
			auto &newItem = items.emplace_back(label, imagepath, id, func, context);
			newItem.enabled = known->enabled;
		} else {
			// Unknown item, push to the back
			items.emplace_back(label, imagepath, id, func, context);
		}
	}
	void DelMenuItem(int id) {
		items.erase(std::remove_if(items.begin(), items.end(), [id](const auto &item) { return item.id == id; }), items.end());
		SavePreferedOrder();
	}
	void SyncPreferedOrder() {
		// Refresh item state from preferedOrder
		// Used when changing properties from DlgMenuCfg
		for(auto &item: items) {
			const auto &known = std::find_if(preferedOrder.begin(), preferedOrder.end(), [&item](const auto &el){ return el.label == item.label;});
			if(known != preferedOrder.end()) {
				if(item.enabled != known->enabled) {
					if(!known->enabled) {
						item.animStateScale = 1.0f;
					} else {
						item.animStateScale = -1.0f;
						item.enabled = true;
					}
				}
			}
		}
		SavePreferedOrder();
	}
	void SavePreferedOrder() {
		preferedOrder.clear();
		FILEHANDLE f = oapiOpenFile("MenuInfoBar.cfg", FILE_OUT, CONFIG);
		int i = 0;
		for(const auto &item:items) {
			char buf[64];
			i++;
			preferedOrder.emplace_back(MenuInfoBar::MenuPreference{item.label, item.enabled});
			sprintf(buf, "MenuItem%d", i);
			oapiWriteItem_string(f, buf, const_cast<char *>(item.label.c_str()));
			sprintf(buf, "MenuItemEnabled%d", i);
			oapiWriteItem_bool(f, buf, item.enabled);
		}
		oapiCloseFile(f, FILE_OUT);
	}

	void SwapItems() {
		if(swapId != -1) {
			auto swappedItem = std::find_if(items.begin(), items.end(), [this](const MenuItem &a){return a.id == swapId;});
			auto draggedItem = std::find_if(items.begin(), items.end(), [](const MenuItem &a){return a.dragging;});
			// If the user moves the mouse quickly, or if there are disabled items,
			// there can be several buttons between the "hole" and the target.
			// We need to do a swap for each.
			if(draggedItem != items.end() && swappedItem != items.end()) {
				if(draggedItem < swappedItem) {
					for (auto it = draggedItem; it != swappedItem; ++it) {
						(it + 1)->animStateScroll = -1.0;
						std::iter_swap(it, it + 1);
					}
				} else {
					auto rdraggedItem = std::make_reverse_iterator(draggedItem + 1);
					auto rswappedItem = std::make_reverse_iterator(swappedItem + 1);
					for (auto it = rdraggedItem; it != rswappedItem; ++it) {
						(it + 1)->animStateScroll = 1.0;
						std::iter_swap(it, it + 1);
					}
				}
			}
			swapId = -1;
		}
	}
	void Display() {
		BeginMenuInfoBar();
		ImGuiWindow* window = ImGui::GetCurrentWindow();
		if (!window->SkipItems) {
			ImGui::PushStyleColor(ImGuiCol_Button, ImVec4(0.f, 0.f, 0.f, prm.MenuOpacity/10.0f));
			ImGui::PushStyleColor(ImGuiCol_ButtonActive, ImVec4(0.f, 0.f, 0.f, prm.MenuOpacity/10.0f));
			ImGui::PushStyleColor(ImGuiCol_ButtonHovered, ImVec4(0.6f, 0.6f, 0.6f, 0.6f));
			ImGui::PushStyleVar(ImGuiStyleVar_FramePadding, ImVec2(0,0));
			SwapItems();
			// MenuInfoBarItem uses isDragging so we cannot clear it here
			// oneDrag will be set in case one item is being dragged this frame
			oneDrag = false;
			for(auto &item: items) {
				if(item.enabled) {
					if(MenuInfoBarItem(item)) {
						item.func(item.context);
					}
				}
			}
			if(isDragging && !oneDrag) {
				SavePreferedOrder();
			}
			isDragging = oneDrag;
			ImGui::PopStyleVar();
			ImGui::PopStyleColor(3);
		}
		EndMenuInfoBar();
	}

	void BeginMenuInfoBar() {
		ImGuiViewport *viewport = ImGui::GetMainViewport();
		// Force viewport, we don't want the menu bar to pop out
		// of the main window when we scroll it outside of view
		ImGui::SetNextWindowViewport(viewport->ID);

		// Calculate the menu position
		ImGuiStyle &style = ImGui::GetStyle();
		ImGuiContext &g = *GImGui;
		float yoffset = 0;
			
		switch(prm.MenuMode) {
		case 0: // show
			break;
		case 1: // hide
			yoffset -= style.WindowPadding.y;
			yoffset -= prm.MenuButtonHoverSize;
			yoffset -= g.FontSize;
			break;
		case 2: // auto-hide
			yoffset -= style.WindowPadding.y;
			yoffset -= prm.MenuButtonSize;
			if(!prm.bMenuLabelAlways)
				yoffset -= g.FontSize;
			yoffset *= (1.0f - animState);
			break;
		}
		// The menu is always centered horizontally by using ImVec2(0.5, 0) as a pivot
		// and targeting the middle of the main window
		ImGui::SetNextWindowPos(ImVec2(viewport->Pos.x + viewport->Size.x / 2.0f, viewport->Pos.y + yoffset), ImGuiCond_Always, ImVec2(0.5, 0));
		
		if(ImGui::Begin("##MenuInfoBar", nullptr, MENUFLAGS)) {
			ImGuiStyle &style = ImGui::GetStyle();
			ImVec2 bbmin = ImGui::GetCursorScreenPos() - style.WindowPadding;
			ImVec2 bbmax = bbmin + ImGui::GetContentRegionAvail() + style.WindowPadding * 2.0f;
			// Expand the bounding box a bit above the window so that the menubar
			// doesn't disappear when the user overshoots when the mouse cursor
			bbmin.y-=100;
			bool isHovering = ImGui::IsMouseHoveringRect(bbmin, bbmax, false);

			isDraggingOutside = !isHovering && isDragging;

			if(isHovering || isDragging) {
				if(ImGui::IsMouseClicked(ImGuiMouseButton_Right)) {
					g_pOrbiter->DlgMgr()->EnsureEntry<DlgMenuCfg> ();
				}

				// Lower the menu
				if (animState < 1.0f) {
					animState += prm.MenuScrollspeed * g.IO.DeltaTime;
					if(animState > 1.0f) animState = 1.0f;
				}
			} else {
				// Release focus so we can use the keyboard without having to click
				// on the screen after interacting with the menu bar
				if(ImGui::IsWindowFocused())
					ImGui::SetWindowFocus(NULL);

				// Retract the menu
				if (animState > 0.0f) {
					animState -= prm.MenuScrollspeed * g.IO.DeltaTime;
					if(animState < 0.0f) animState = 0.0f;
				}
			}

			// Remove the button if it's dropped outside the menubar
			if(ImGui::IsMouseReleased(ImGuiMouseButton_Left) && isDraggingOutside) {
				int itemsEnabled = std::count_if(items.begin(), items.end(), [](const auto &item){return item.enabled;});
				if(itemsEnabled > 1) { // Prevent removing the last button
					auto draggedItem = std::find_if(items.begin(), items.end(), [](const MenuItem &a){return a.dragging;});
					if(draggedItem != items.end()) {
						draggedItem->animStateScale = 1.0;
					}
				}
			}
		}
	}
	bool MenuInfoBarItem(MenuItem &item) {
		// Filter button spacing in time to prevent excessive jittering
		// when modifying MenuButtonSpacing
		if(animStateSpacing < prm.MenuButtonSpacing) animStateSpacing+=0.1f;
		else if(animStateSpacing > prm.MenuButtonSpacing) animStateSpacing-=0.1f;
		ImGui::SameLine(0.0f, animStateSpacing);
		ImGuiWindow* window = ImGui::GetCurrentWindow();
		ImGuiContext& g = *GImGui;

		// Compute the item's position and size
		float buttonCenterPos = ImGui::GetCursorScreenPos().x + item.lastItemSize / 2.0f;
		float deltaPos = (ImGui::GetMousePos().x - buttonCenterPos) / item.lastItemSize;

		// Keep the button fully sized when hovered
		if(fabs(deltaPos) < 0.5f) {
			deltaPos = 0;
		}

		// Use a gaussian bell contour to compute the button size
		// given its distance from the cursor
		float scale = exp(-deltaPos * deltaPos * strength) * animState;
		float buttonSize = lrint(lerp(prm.MenuButtonSize, prm.MenuButtonHoverSize, scale));
		item.lastItemSize = buttonSize;
		
		// Do a scale animation got the button
		bool disableText = false;
		if(item.animStateScale > 0.0f) {
			disableText = true;
			item.animStateScale -= prm.MenuScrollspeed * g.IO.DeltaTime;
			if(item.animStateScale <= 0.0f) {
				item.animStateScale = 0.0f;
				// Button has shrunk away, disable it
				item.enabled = false;
				SavePreferedOrder();
				return false;
			}
			buttonSize *= item.animStateScale;
		} else if(item.animStateScale < 0.0f) {
			disableText = true;
			item.animStateScale += prm.MenuScrollspeed * g.IO.DeltaTime;
			if(item.animStateScale >= 0.0f) {
				item.animStateScale = 0.0f;
			}
			buttonSize *= 1.0f + item.animStateScale;
		}

		ImRect bb(window->DC.CursorPos, window->DC.CursorPos + ImVec2(buttonSize, buttonSize));
		const char *label = item.label.c_str();
		bool ret = false;
		
		if(item.dragging) {
			// If this button is currently being dragged, we use an invisible
			// button to keep a "hole" in the menu bar and keep track of its
			// original position.
			// It will be drawn at its dragged position in EndMenuInfoBar()
			ImGui::InvisibleButton(label, ImVec2(buttonSize, buttonSize));
			holePos = bb.Min.x;
		} else {
			// If another button is being dragged, test if the current one needs to
			// be swapped with it
			if(isDragging) {
				// Different check if the button is to the right or to the left of the "hole"
				if(dragPos.x < holePos) {
					if(dragPos.x < bb.Min.x && (dragPos.x + prm.MenuButtonHoverSize > bb.Min.x)) {
						swapId = item.id;
					}
				} else {
					if((dragPos.x + prm.MenuButtonHoverSize)> bb.Max.x && (dragPos.x < bb.Max.x)) {
						swapId = item.id;
					}
				}
			}
			ImVec2 textsize = ImGui::CalcTextSize(label, NULL, true);
			ImVec2 textpos = ImVec2(bb.Min.x + (bb.Max.x - bb.Min.x) / 2.0f - textsize.x / 2.0f, bb.Max.y);
			if (item.animStateScroll != 0.0f) {
				// If animState != 0, it means the current button has been swapped recently
				// We do the swapping animation by using an invisible button at its final position
				// and drawing it's animated position with the DrawList 
				if(item.animStateScroll < 0.0) {
					item.animStateScroll += prm.MenuScrollspeed * g.IO.DeltaTime;
					if(item.animStateScroll > 0.0) item.animStateScroll = 0.0;
				} else if(item.animStateScroll > 0.0) {
					item.animStateScroll -= prm.MenuScrollspeed * g.IO.DeltaTime;
					if(item.animStateScroll < 0.0) item.animStateScroll = 0.0;
				}
				ImGuiContext& g = *GImGui;
				ImGui::InvisibleButton(label, ImVec2(buttonSize, buttonSize));
				ImVec2 offset = ImVec2(item.animStateScroll * buttonSize, 0);
				window->DrawList->AddImage(item.texId, bb.Min - offset, bb.Max - offset);
				window->DrawList->AddText(textpos - offset, 0xffffffff, label);
			} else {
				ret = ImGui::ImageButton(label, item.texId, ImVec2(buttonSize, buttonSize));
				if(!disableText)
					window->DrawList->AddText(textpos, 0xffffffff, label);
			}
		}

		bool active = ImGui::IsItemActive();
		bool dragging = ImGui::IsMouseDragging(ImGuiMouseButton_Left);

		if(active && dragging) {
			// This item is being pressed and dragged.
			// If it's the first time we save the item's position and texture ID.
			// If not, we update the drag position according to the mouse movement.
			// dragPos will be used for drawing the button later and  for checking
			// if we need to swap items
			oneDrag = true;
			ImGuiIO& io = ImGui::GetIO();
			if(!item.dragging) {
				dragPos = ImGui::GetItemRectMin();
				item.dragging = true;
				dragTexId = item.texId;
			} else {
				dragPos += io.MouseDelta;
			}
		} else if(!dragging) {
			item.dragging = false;
		}
		return ret;
	}

	void EndMenuInfoBar() {
		// If we're dragging a button, draw it last from here so it stays on top of the other items
		if(isDragging) {
			ImGuiWindow* window = ImGui::GetCurrentWindow();
			int itemsEnabled = std::count_if(items.begin(), items.end(), [](const auto &item){return item.enabled;});
			ImVec2 imageSize = ImVec2(prm.MenuButtonHoverSize, prm.MenuButtonHoverSize);
			if(isDraggingOutside && itemsEnabled > 1) {
				// If it's outside the menu and it's not the only button remaining,
				// add a red background and show a removal message
				window->DrawList->AddRectFilled(dragPos, dragPos + imageSize, 0xbfbfbfff);
				const char *label = "Remove from menu";
				ImVec2 textSize = ImGui::CalcTextSize(label, NULL, true);
				window->DrawList->AddText(dragPos + ImVec2(prm.MenuButtonHoverSize / 2.0f - textSize.x / 2.0f, prm.MenuButtonHoverSize), 0xffffffff, label);
			} else {
				window->DrawList->AddRectFilled(dragPos, dragPos + imageSize, 0xbfbfbfbf);
			}
			window->DrawList->AddImage(dragTexId, dragPos, dragPos + imageSize);
		}

		// Add some padding at the bottom so we can hover over the menu bar even when it's fully hidden
		if(prm.MenuMode != 0) {
			ImGui::NewLine();
			ImGui::NewLine();
		}
		ImGui::End();
	}
};


class InfoFrameRate {
	static const int HISTORY_SIZE = 64;
	float history[HISTORY_SIZE];
	float fpsMax;
	int historyOffset;
public:
	InfoFrameRate() {
		for(int i = 0; i < HISTORY_SIZE; i++) {
			history[i] = 0.1;
		}
		historyOffset = 0;
		fpsMax = 0.0f;
	}
	void Update() {
		int offset = ((int)td.SysT0) % HISTORY_SIZE;
		if(offset != historyOffset) {
			historyOffset = offset;
			history[historyOffset] = 0.0f;
			UpdateMinMax();
		}
		ImGuiContext &g = *GImGui;
		float fps = g.IO.Framerate;
		history[historyOffset] = fps;
		fpsMax = std::max(fpsMax, fps);
	}
	void UpdateMinMax() {
		fpsMax = 0.0;
		for(int i = 0; i < HISTORY_SIZE; i++) {
			if(history[i] > fpsMax) {
				fpsMax = history[i];
			}
		}
	}
	void Display() {
		Update();
		ImVec4 white{1,1,1,1};
		ImGuiContext &g = *GImGui;

		ImGui::BeginChild("ChildL", ImVec2(ImGui::GetContentRegionAvail().x - HISTORY_SIZE - 8.0, 0), ImGuiChildFlags_AutoResizeY);
		ImGui::TextColored(white, "F/s:%.0f", g.IO.Framerate);
		ImGui::TextColored(white, "ΔT/F:%0.*fs", td.SimDT < 1e-1 ? 3 : td.SimDT < 1 ? 2 : td.SimDT < 10 ? 1 : 0, td.SimDT);
		ImGui::EndChild();
		ImGui::SameLine();
		ImGui::BeginChild("ChildR");//, ImVec2(ImGui::GetContentRegionAvail().x / 3.0f * 2.0f, 0), ImGuiChildFlags_AutoResizeY);
		if(fpsMax != 0.0) {
			ImGui::PushStyleColor(ImGuiCol_Text, ImVec4(1,1,1,1));
			ImGui::PushStyleColor(ImGuiCol_PopupBg, ImVec4(0,0,0,0));
			ImGui::PushStyleColor(ImGuiCol_FrameBg, ImVec4(0.3,0.3,0.3,0.3));
			ImGui::PushStyleColor(ImGuiCol_PlotLines, ImVec4(0,1,0,1));
			ImGui::PushStyleColor(ImGuiCol_Border, ImVec4(0.7,0.7,0.7,1));
			ImGui::PushStyleVar(ImGuiStyleVar_FrameBorderSize, 1.0);
			ImGui::PushStyleVar(ImGuiStyleVar_FrameRounding, 0.0);
			char fps[16];
			sprintf(fps, "%d", (int)fpsMax);
			ImGui::PlotLines("###FPS", history, HISTORY_SIZE, historyOffset + 1, fps, 0.0, fpsMax, ImVec2(HISTORY_SIZE, ImGui::GetContentRegionAvail().y));
			ImGui::PopStyleVar(2);
			ImGui::PopStyleColor(5);
		}
		ImGui::EndChild();
	}
};

const float OPACITY_NONE = 0.001f;
void AnimateOpacity(float &animStateOpacity)
{
	CFG_UIPRM &prm = g_pOrbiter->Cfg()->CfgUIPrm;
	if(prm.InfoMode == 0) { // show
		animStateOpacity = 1.0f;
		return;
	}
	ImGuiIO& io = ImGui::GetIO();
	bool show = io.MousePos.y < (ImGui::GetCursorScreenPos().y + ImGui::GetContentRegionAvail().y);
	if(show) {
		if(animStateOpacity < 1.0f) {
			animStateOpacity += prm.MenuScrollspeed * io.DeltaTime / 3.0f;
			if(animStateOpacity > 1.0f) animStateOpacity = 1.0f;
		}
	} else {
		if(animStateOpacity > OPACITY_NONE) {
			animStateOpacity -= prm.MenuScrollspeed * io.DeltaTime / 3.0f;
			// Don't set it to 0 or ImGui will optimize the drawing away and return false on Begin
			if(animStateOpacity < OPACITY_NONE) animStateOpacity = OPACITY_NONE;
		}
	}
}

class InfoTime: public ImGuiDialog {
	InfoFrameRate *fps;
	float animStateOpacity;
public:
	InfoTime(InfoFrameRate *fps_):ImGuiDialog("InfoTime"),fps(fps_),animStateOpacity(OPACITY_NONE) {}
	void OnDraw() {}

	void DrawSymbol(int type) {
		float alpha = ImGui::GetStyle().Alpha;
		ImVec2 pos = ImGui::GetCursorScreenPos();
		ImDrawList* drawList = ImGui::GetWindowDrawList();
		ImVec2 size = ImGui::CalcTextSize("__");
		ImVec2 endpos = pos + size;
		drawList->AddRect(pos, endpos, ImGui::ColorConvertFloat4ToU32(ImVec4(0.75,0.75,0.75,alpha)));
		switch(type) {
		case 0: // REC
			if(fmod (td.SysT1, 1.0) < 0.5) {
				ImVec2 center = (pos + endpos)/2.0;
				drawList->AddCircleFilled(center, size.y/4.0, ImGui::ColorConvertFloat4ToU32(ImVec4(1.0,0.0,0.0,alpha)));
			}
			break;
		case 1: // Pause
			{
				ImVec2 tl = pos + ImVec2(size.x / 4.5f, size.y / 4.0f);
				ImVec2 br = pos + ImVec2(size.x / 4.5f * 2.0f, size.y / 4.0f * 3.0f);
				drawList->AddRectFilled(tl, br, ImGui::ColorConvertFloat4ToU32(ImVec4(1.0,1.0,0.0,alpha)));
				tl.x += size.x/4.5f*1.5f;
				br.x += size.x/4.5f*1.5f;
				drawList->AddRectFilled(tl, br, ImGui::ColorConvertFloat4ToU32(ImVec4(1.0,1.0,0.0,alpha)));
				drawList->AddText(pos + ImVec2(size.x + 2,0), ImGui::ColorConvertFloat4ToU32(ImVec4(1.0,1.0,1.0,alpha)), "Pause");
			}
			break;
		case 2: // Fast forward
			{
				ImVec2 p1 = pos + ImVec2(size.x / 4.5f, size.y / 5.0f);
				ImVec2 p2 = pos + ImVec2(size.x / 4.5f * 2.0f, size.y / 2.0f);
				ImVec2 p3 = pos + ImVec2(size.x / 4.5f, size.y / 5.0f * 4.0f);
				drawList->AddTriangleFilled(p1, p2, p3, ImGui::ColorConvertFloat4ToU32(ImVec4(1.0,1.0,1.0,alpha)));
				p1.x += size.x/4.5f*1.5f;
				p2.x += size.x/4.5f*1.5f;
				p3.x += size.x/4.5f*1.5f;
				drawList->AddTriangleFilled(p1, p2, p3, ImGui::ColorConvertFloat4ToU32(ImVec4(1.0,1.0,1.0,alpha)));
				char buf[64];
				sprintf(buf, "%0.*fx", td.Warp() < 9.99 ? 1:0, td.Warp());
				drawList->AddText(pos + ImVec2(size.x + 2,0), ImGui::ColorConvertFloat4ToU32(ImVec4(1.0,1.0,1.0,alpha)), buf);
			}
			break;
		case 3: // Playback
			{
				ImVec2 p1 = pos + ImVec2(size.x / 4.5f * 2.0f, size.y / 5.0f);
				ImVec2 p2 = pos + ImVec2(size.x / 4.5f * 3.0f, size.y / 2.0f);
				ImVec2 p3 = pos + ImVec2(size.x / 4.5f * 2.0f, size.y / 5.0f * 4.0f);
				drawList->AddTriangleFilled(p1, p2, p3, ImGui::ColorConvertFloat4ToU32(ImVec4(1.0,1.0,0.0,alpha)));
			}
		}
		ImGui::NewLine();
	}

	void Display() {
		CFG_UIPRM &prm = g_pOrbiter->Cfg()->CfgUIPrm;
		if(prm.InfoMode == 1)
			return;
		ImGuiViewport *viewport = ImGui::GetMainViewport();
		ImGui::SetNextWindowViewport(viewport->ID);
		ImGui::SetNextWindowPos(viewport->Pos+ImVec2(viewport->Size.x,0),ImGuiCond_Always, ImVec2(1,0));
		
		ImGui::PushStyleColor(ImGuiCol_Border, ImVec4(0.f, 0.f, 0.f, 0.f));
		ImGui::PushStyleVar(ImGuiStyleVar_Alpha, animStateOpacity);
		ImGui::SetNextWindowBgAlpha(prm.InfoOpacity/10.0f * animStateOpacity);
		ImGui::PushStyleColor(ImGuiCol_ChildBg, ImVec4(0.f, 0.f, 0.f, 0.f));
		if(ImGui::Begin("##InfoTime", nullptr, INFOFLAGS)) {
			AnimateOpacity(animStateOpacity);

			ImVec4 white{1,1,1,1};
			ImVec4 red{1,0,0,1};
			ImVec4 yellow{1,1,0,1};
			ImGui::PushFont(ImGuiFont::MONO);
			ImGui::TextColored(white, DateStr (td.MJD1));
			ImGui::TextColored(white, "MJD %0.4f", td.MJD1);

			if(g_pOrbiter->RecorderStatus() == 1) {
				ImGui::SameLine();
				DrawSymbol(0);
			} else if(g_pOrbiter->IsPlayback()) {
				ImGui::SameLine();
				DrawSymbol(3);
			}
			if(td.SimT1 < 1e7) {
				ImGui::TextColored(white, "Sim % 9.0fs", td.SimT1);
			} else {
				ImGui::TextColored(white, "Sim ..%07.0fs", fmod (td.SimT1, 1e7));
			}

			if(!g_pOrbiter->IsRunning()) {
				ImGui::SameLine();
				DrawSymbol(1);
			} else if(td.Warp() != 1.0 || prm.bWarpAlways) {
				ImGui::SameLine();
				DrawSymbol(2);
			}

			if(prm.FPS == 2)
				fps->Display();

			ImGui::PopFont();
		}
		if(ImGui::IsWindowFocused(ImGuiFocusedFlags_ChildWindows))
			ImGui::SetWindowFocus(NULL);
		ImGui::End();

		ImGui::PopStyleVar();
		ImGui::PopStyleColor(2);
	}
};

class InfoTarget: public ImGuiDialog {
	InfoFrameRate *fps;
	float animStateOpacity;
public:
	InfoTarget(InfoFrameRate *fps_):ImGuiDialog("InfoTarget"),fps(fps_),animStateOpacity(OPACITY_NONE) {}
	void OnDraw() {}
	void Display() {
		CFG_UIPRM &prm = g_pOrbiter->Cfg()->CfgUIPrm;
		if(prm.InfoMode == 1) {
			animStateOpacity = OPACITY_NONE;
			return;
		}
		ImGuiViewport *viewport = ImGui::GetMainViewport();
		ImGui::SetNextWindowViewport(viewport->ID);
		ImGui::SetNextWindowPos(viewport->Pos);
		ImGui::PushStyleColor(ImGuiCol_Border, ImVec4(0.f, 0.f, 0.f, 0.f));
		ImGui::PushStyleVar(ImGuiStyleVar_Alpha, animStateOpacity);
		ImGui::SetNextWindowBgAlpha(prm.InfoOpacity/10.0f * animStateOpacity);
		ImGui::PushStyleColor(ImGuiCol_ChildBg, ImVec4(0.f, 0.f, 0.f, 0.f));
		if(ImGui::Begin("##InfoTarget", nullptr, INFOFLAGS)) {
			AnimateOpacity(animStateOpacity);

			ImVec4 white{1,1,1,1};
			ImGui::PushFont(ImGuiFont::MONO);

			Body *tgt = g_camera->Target();
			ImGui::TextColored(white, "Tgt %s", tgt->Name());

			if (g_camera->IsExternal()) {
				switch (g_camera->GetExtMode()) {
					case CAMERA_TARGETRELATIVE:   ImGui::TextColored(white, "Cam track (rel-pos)"); break;
					case CAMERA_ABSDIRECTION:     ImGui::TextColored(white, "Cam track (abs-dir)"); break;
					case CAMERA_GLOBALFRAME:      ImGui::TextColored(white, "Cam track (global frame)"); break;
					case CAMERA_TARGETTOOBJECT:   ImGui::TextColored(white, "Cam target to %s", g_camera->GetDirRef()->Name()); break;
					case CAMERA_TARGETFROMOBJECT: ImGui::TextColored(white, "Cam target from %s", g_camera->GetDirRef()->Name()); break;
					case CAMERA_GROUNDOBSERVER:   ImGui::TextColored(white, "Cam ground %s", g_camera->GroundObserver_TargetLock() ? "(tgt-lock)" : "(free)"); break;
				}
			} else {
				ImGui::TextColored(white, "Cam Cockpit");
			}
			ImGui::TextColored(white, "FoV % 0.0f°", 2.0*Deg(g_camera->Aperture()));
			if (g_camera->IsExternal()) {
				ImGui::SameLine();
				ImGui::TextColored(white, "   Dst %s", DistStr (g_camera->Distance())+1);
			} else {
				ImGui::SameLine();
				ImGui::TextColored(ImVec4(0,0,0,0), "   Dst %s", DistStr (g_camera->Distance())+1);
			}

			if(prm.FPS == 1)
				fps->Display();

			ImGui::PopFont();
		}
		if(ImGui::IsWindowFocused(ImGuiFocusedFlags_ChildWindows))
			ImGui::SetWindowFocus(NULL);
		ImGui::End();
		ImGui::PopStyleVar();
		ImGui::PopStyleColor(2);
	}
};

// =======================================================================
// class MenuInfoBar
// =======================================================================

MenuInfoBar::MenuInfoBar ()
{
	dynMenu  = std::make_unique<DynamicMenuBar>();
	infoFps  = std::make_unique<InfoFrameRate>();
	infoTime = std::make_unique<InfoTime>(infoFps.get());
	infoTgt  = std::make_unique<InfoTarget>(infoFps.get());
	oapiOpenDialog(dynMenu.get());
	oapiOpenDialog(infoTime.get());
	oapiOpenDialog(infoTgt.get());
}

MenuInfoBar::~MenuInfoBar ()
{
}
void MenuInfoBar::RegisterMenuItem(const char *label, const char *imagepath, int id, CustomFunc func, void *context)
{
	dynMenu->AddMenuItem(label, imagepath, id, func, context);
}

void MenuInfoBar::UnregisterMenuItem(int id)
{
	dynMenu->DelMenuItem(id);
}

std::vector<MenuInfoBar::MenuPreference> &MenuInfoBar::GetPreferences()
{
	return dynMenu->preferedOrder;
}

void MenuInfoBar::SyncPreferences()
{
	return dynMenu->SyncPreferedOrder();
}
