// =================================================================================================================================
//
// Copyright (C) 2019-2026 Jarmo Nikkanen
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation
// files (the "Software"), to use, copy, modify, merge, publish, distribute, interact with the Software and sublicense copies
// of the Software, subject to the following conditions:
//
// a) You do not sell, rent or auction the Software.
// b) You do not collect distribution fees.
// c) If the Software is distributed in an object code form, it must inform that the source code is available and how to obtain it.
// d) You do not remove or alter any copyright notices contained within the Software.
// e) This copyright notice must be included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
// OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
// IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
// =================================================================================================================================


#include "WindowMgr.h"
#include "D3D9Config.h"
#include "Log.h"
#include "imgui.h"
#include <algorithm>
#include <cmath>
#include <cfloat>

#define APPNODE(x) ((Node *)x)

class WindowManager *g_pWM = NULL;

list<gcGUIApp *> g_gcGUIAppList;

static const float DOCK_TOP = 60.0f;
static const DWORD MSG_RETURN_NODE = 0x100;

// ===============================================================================================
//
class gcGUIHost : public ImGuiDialog
{
public:
	gcGUIHost(WindowManager *pMgr) : ImGuiDialog("##gcGUI"), pMgr(pMgr) {}
	void Display() override { pMgr->Render(); }
	void OnDraw() override {}

private:
	WindowManager *pMgr;
};

// ===============================================================================================
//
static ImU32 _Colour(DWORD dwABGR)
{
	return IM_COL32(dwABGR & 0xFF, (dwABGR >> 8) & 0xFF, (dwABGR >> 16) & 0xFF, 0xFF);
}







// ===============================================================================================
// Node Implementation
// ===============================================================================================
//
Node::Node(SideBar *pSB, const char *label, gcGUIDrawProc pDraw, void *pUser, DWORD color, DWORD flags, Node *pP) :
	pApp(NULL), pSB(pSB), pParent(pP), pDraw(pDraw), pUser(pUser), Color(color), Flags(flags),
	yTop(0.0f), yBottom(0.0f), bOpen((flags & gcGUI::NF_COLLAPSED) == 0), bClose(false)
{
	if (label) Label = label;
	if (pParent) pApp = pParent->pApp;
	pSB->AddWindow(this);
}

// ===============================================================================================
//
Node::~Node()
{
}

// ===============================================================================================
//
bool Node::HasTitle() const
{
	if (Flags & gcGUI::NF_NOTITLE) return false;
	if ((pParent == NULL) && (Config->gcGUIMode == 3) && pDraw) return false;
	return true;
}

// ===============================================================================================
//
void Node::Draw(float yShift)
{
	WindowManager *pMgr = pSB->GetWM();

	ImGui::PushID(this);

	ImVec2 p = ImGui::GetCursorScreenPos();
	yTop = p.y - yShift;

	if (HasTitle()) {

		float fs = ImGui::GetFontSize();
		float h = pParent ? fs * 1.4f : fs * 1.8f;
		float w = ImGui::GetContentRegionAvail().x;
		float bw = bClose ? h : 0.0f;

		ImGui::InvisibleButton("##title", ImVec2(std::max(1.0f, w - bw), h));
		bool bHover = ImGui::IsItemHovered();
		if (ImGui::IsItemActivated()) pMgr->TitleDown(this);
		if (ImGui::IsItemActive()) pMgr->TitleHeld(this);
		if (ImGui::IsItemDeactivated()) pMgr->TitleUp(this, bHover);

		ImDrawList *dl = ImGui::GetWindowDrawList();
		ImU32 tc = pParent ? IM_COL32(0, 0, 0, 255) : IM_COL32(255, 255, 255, 255);

		dl->AddRectFilled(p, ImVec2(p.x + w, p.y + h), _Colour(Color));

		float a = fs * 0.3f;
		float cx = p.x + h * 0.5f;
		float cy = p.y + h * 0.5f;
		if (bOpen) dl->AddTriangleFilled(ImVec2(cx - a, cy - a * 0.6f), ImVec2(cx + a, cy - a * 0.6f), ImVec2(cx, cy + a * 0.6f), tc);
		else dl->AddTriangleFilled(ImVec2(cx - a * 0.6f, cy - a), ImVec2(cx + a * 0.6f, cy), ImVec2(cx - a * 0.6f, cy + a), tc);

		dl->PushClipRect(p, ImVec2(p.x + w - bw, p.y + h), true);
		dl->AddText(ImVec2(p.x + h, p.y + (h - fs) * 0.5f), tc, Label.c_str());
		dl->PopClipRect();

		if (bClose) {
			ImGui::SameLine(0.0f, 0.0f);
			ImVec2 q = ImGui::GetCursorScreenPos();
			if (ImGui::InvisibleButton("##close", ImVec2(h, h))) pMgr->CloseClicked(this);
			float c = h * 0.2f;
			ImVec2 m(q.x + h * 0.5f, q.y + h * 0.5f);
			float t = ImGui::IsItemHovered() ? 2.0f : 1.5f;
			dl->AddLine(ImVec2(m.x - c, m.y - c), ImVec2(m.x + c, m.y + c), tc, t);
			dl->AddLine(ImVec2(m.x - c, m.y + c), ImVec2(m.x + c, m.y - c), tc, t);
		}
	}

	if ((bOpen || !HasTitle()) && pDraw) {
		ImGui::PushID("content");
		pDraw(HNODE(this), pUser);
		ImGui::PopID();
	}

	yBottom = ImGui::GetCursorScreenPos().y - yShift;

	ImGui::PopID();
}








// ===============================================================================================
// WindowManager Implementation
// ===============================================================================================
//
WindowManager::WindowManager(HWND hAppMainWindow, HINSTANCE _hInst, bool bWindowed)
{
	g_pWM = NULL;
	hMainWnd = hAppMainWindow;
	hInst = _hInst;
	sbDrag = NULL;
	sbDragSrc = NULL;
	sbDest = NULL;
	pInsTgt = NULL;
	pInsAfter = NULL;
	bInsValid = false;
	dnNode = NULL;
	xOffset = yOffset = 0.0f;
	bWin = bWindowed;
	bRender = false;
	nextuid = 0;

	pHost = new gcGUIHost(this);
	oapiOpenDialog(pHost);

	// Must be last one
	g_pWM = this;
}


// ===============================================================================================
//
WindowManager::~WindowManager()
{
	oapiCloseDialog(pHost);
	delete pHost;
	pHost = NULL;

	for (SideBar* sb : sbList) delete sb;
	sbList.clear();

	for (Node *pn : qDelete) delete pn;
	qDelete.clear();

	if (g_pWM == this) g_pWM = NULL;
}


// ===============================================================================================
//
bool WindowManager::IsOK() const
{
	return pHost != NULL;
}


// ===============================================================================================
//
int WindowManager::GetMode() const
{
	return Config->gcGUIMode;
}


// ===============================================================================================
//
float WindowManager::GetWidth() const
{
	return ImGui::GetFontSize() * 20.0f;
}


// ===============================================================================================
//
void WindowManager::Render()
{
	bRender = true;

	vector<SideBar*> bars = sbList;

	for (SideBar *sb : bars) sb->ManageButtons();
	for (SideBar *sb : bars) sb->Draw();

	if (sbDrag) {
		ImVec2 mp = ImGui::GetIO().MousePos;
		if (ImGui::IsMouseDown(ImGuiMouseButton_Left)) {
			Drag(mp.x, mp.y);
			SideBar *pTgt = FindDestination();
			if (pTgt && pTgt->IsOpen()) pTgt->TryInsert(sbDrag);
		}
		else {
			SideBar *pTgt = FindDestination();
			if (pTgt) pTgt->Apply();
			EndDrag();
		}
	}

	if (dnNode && !ImGui::IsMouseDown(ImGuiMouseButton_Left)) dnNode = NULL;

	bRender = false;

	Dispatch();
}


// ===============================================================================================
//
void WindowManager::Dispatch()
{
	while (!qMsg.empty()) {

		Message m = qMsg.front();
		qMsg.erase(qMsg.begin());

		if (!DoesExist(m.pNode)) continue;

		switch (m.uMsg) {

		case gcGUI::MSG_OPEN_NODE:
		case gcGUI::MSG_CLOSE_NODE:
			if (m.pNode->pApp) m.pNode->pApp->clbkMessage(m.uMsg, HNODE(m.pNode), 0);
			break;

		case gcGUI::MSG_CLOSE_APP:
			if (m.pNode->pApp && m.pNode->pApp->clbkMessage(gcGUI::MSG_CLOSE_APP, NULL, 0)) {
				if (DoesExist(m.pNode)) CloseWindow(m.pNode);
			}
			break;

		case MSG_RETURN_NODE:
			CloseWindow(m.pNode);
			break;
		}
	}

	for (Node *pn : qDelete) delete pn;
	qDelete.clear();
}


// ===============================================================================================
//
void WindowManager::TitleDown(Node *pNode)
{
	dnNode = pNode;
}


// ===============================================================================================
//
void WindowManager::TitleHeld(Node *pNode)
{
	if (dnNode != pNode) return;
	if (GetMode() >= 3) return;

	SideBar *pSB = pNode->GetSideBar();
	ImVec2 d = ImGui::GetMouseDragDelta(ImGuiMouseButton_Left, 0.0f);
	ImVec2 mp = ImGui::GetIO().MousePos;

	// Begin Moving a Window
	//
	if (pSB->IsFloater() && (pSB->GetTopNode() == pNode) && (fabs(d.x) > 1.0f || fabs(d.y) > 1.0f)) {
		SBRect r = pSB->GetRect();
		SetOffset(mp.x - r.left, mp.y - r.top);
		BeginMove(pNode, mp.x, mp.y);
		dnNode = NULL;
		return;
	}

	// Detach a window from a dock
	//
	if (GetMode() == 1 && fabs(d.x) > 25.0f) {
		SBRect r = pSB->GetRect();
		SetOffset(mp.x - r.left, mp.y - pNode->yTop);
		StartDrag(pNode, mp.x, mp.y);
		dnNode = NULL;
	}
}


// ===============================================================================================
//
void WindowManager::TitleUp(Node *pNode, bool bInside)
{
	if (dnNode == pNode && bInside && pNode->GetSideBar() && !sbDrag) {
		pNode->bOpen = !pNode->bOpen;
		qMsg.push_back({ pNode, DWORD(pNode->bOpen ? gcGUI::MSG_OPEN_NODE : gcGUI::MSG_CLOSE_NODE) });
	}
	dnNode = NULL;
}


// ===============================================================================================
//
void WindowManager::CloseClicked(Node *pNode)
{
	if (pNode->pParent) qMsg.push_back({ pNode, MSG_RETURN_NODE });
	else qMsg.push_back({ pNode, DWORD(gcGUI::MSG_CLOSE_APP) });
}


// ===============================================================================================
//
SideBar * WindowManager::GetDock(DWORD state)
{
	for (SideBar *sb : sbList) if (sb->GetStyle() == int(state)) return sb;
	SideBar *pSB = new SideBar(this, state);
	sbList.push_back(pSB);
	return pSB;
}


// ===============================================================================================
// Virtual
//
HNODE WindowManager::RegisterApplication(gcGUIApp *pPtr, const char *label, gcGUIDrawProc pDraw, void *pUser, DWORD docked, DWORD color, DWORD flags)
{
	if (std::find(g_gcGUIAppList.begin(), g_gcGUIAppList.end(), pPtr) == g_gcGUIAppList.end()) g_gcGUIAppList.push_back(pPtr);

	if (color == 0) color = 0xC0A020;

	// Always "Float" in these modes
	if (GetMode() >= 2) docked = gcGUI::DS_FLOAT;
	if (docked != gcGUI::DS_LEFT && docked != gcGUI::DS_RIGHT) docked = gcGUI::DS_FLOAT;

	SideBar *pSB = (docked == gcGUI::DS_FLOAT) ? NewSideBar(NULL) : GetDock(docked);

	Node *pAp = new Node(pSB, label, pDraw, pUser, color, flags, NULL);
	pAp->SetApp(pPtr);
	return HNODE(pAp);
}



// ===============================================================================================
// Virtual
//
HNODE WindowManager::RegisterSubsection(HNODE hNode, const char *label, gcGUIDrawProc pDraw, void *pUser, DWORD color, DWORD flags)
{
	if (!hNode || APPNODE(hNode)->pParent != NULL) {
		LogErr("RegisterSubsection Failed. Parent cannot be an other subnode");
		return NULL;
	}

	SideBar *pSB = APPNODE(hNode)->GetSideBar();
	if (color == 0) color = pSB->GetAutoColor();
	Node *pAp = new Node(pSB, label, pDraw, pUser, color, flags, APPNODE(hNode));
	pSB->Sort();
	return HNODE(pAp);
}


// ===============================================================================================
// Virtual
//
void WindowManager::UpdateStatus(HNODE hNode, const char *label, gcGUIDrawProc pDraw, void *pUser, DWORD color)
{
	Node *pAp = APPNODE(hNode);
	pAp->pDraw = pDraw;
	pAp->pUser = pUser;
	if (label) pAp->Label = label;
	else pAp->Label.clear();
	if (color != 0) pAp->ReColorize(color);
	UpdateStatus(hNode);
}


// ===============================================================================================
// Virtual
//
void WindowManager::DisplayWindow(HNODE hNode, bool bShow)
{
	SideBar *pSB = APPNODE(hNode)->GetSideBar();

	if (pSB) {
		if (pSB->IsFloater()) {
			if (bShow) {
				pSB->ManageButtons();
				pSB->Sort();
				pSB->Show(true);
			}
			else pSB->Show(false);
		}
	}
}


// ===============================================================================================
// Virtual
//
HNODE WindowManager::GetNode(void *pUser)
{
	if (!pUser) return NULL;
	for (SideBar* sb : sbList) for (Node *nd : sb->wList) if (nd->pUser == pUser) return HNODE(nd);
	return NULL;
}


// ===============================================================================================
// Virtual
//
void *WindowManager::GetUserData(HNODE hNode)
{
	return APPNODE(hNode)->pUser;
}


// ===============================================================================================
// Virtual
//
void WindowManager::OpenNode(HNODE hNode, bool bOpen)
{
	APPNODE(hNode)->bOpen = bOpen;
}


// ===============================================================================================
// Virtual
//
bool WindowManager::IsOpen(HNODE hNode)
{
	return APPNODE(hNode)->bOpen;
}


// ===============================================================================================
// Virtual
//
bool WindowManager::UnRegister(HNODE hNode)
{
	if (!DoesExist(APPNODE(hNode))) return false;

	if (APPNODE(hNode)->IsRoot()) return false;

	SideBar *pSB = APPNODE(hNode)->GetSideBar();
	pSB->RemoveWindow(APPNODE(hNode));
	if (pSB->IsEmpty() && pSB->IsFloater()) ReleaseSideBar(pSB);
	DeleteNode(APPNODE(hNode));
	return true;
}


// ===============================================================================================
//
void WindowManager::DeleteNode(Node *pNode)
{
	if (dnNode == pNode) dnNode = NULL;
	if (pInsAfter == pNode) pInsAfter = NULL, bInsValid = false;
	if (bRender) qDelete.push_back(pNode);
	else delete pNode;
}


// ===============================================================================================
//
bool WindowManager::DoesExist(Node *pn)
{
	for (SideBar *sb : sbList) if (sb->DoesExists(pn)) return true;
	return false;
}


// ===============================================================================================
//
void WindowManager::UpdateStatus(HNODE hNode)
{
	if (!hNode) return;
	SideBar *pSB = APPNODE(hNode)->GetSideBar();
	if (pSB) pSB->ManageButtons();
}


// ===============================================================================================
//
void WindowManager::CloseWindow(Node *pAp)
{
	SideBar *pSB = pAp->GetSideBar();
	Node *pPar = pAp->pParent;

	if (pPar) {
		pSB->RemoveWindow(pAp);
		if (pSB->IsEmpty() && pSB->IsFloater()) ReleaseSideBar(pSB);

		pSB = pPar->GetSideBar();
		pSB->AddWindow(pAp);
		pSB->Sort();
		pSB->ManageButtons();
	}
	else {
		if (GetMode() >= 2) {

			gcGUIApp *pApp = pAp->pApp;

			if (sbDrag == pSB || sbDragSrc == pSB) EndDrag();

			for (SideBar *sb : sbList) {
				if (sb == pSB) continue;
				vector<Node*> orphans;
				for (Node *nd : sb->wList) if (nd->pParent == pAp) orphans.push_back(nd);
				for (Node *nd : orphans) {
					sb->RemoveWindow(nd);
					DeleteNode(nd);
				}
				if (sb->IsEmpty() && sb->IsFloater()) ReleaseSideBar(sb);
			}

			for (Node *nd : pSB->wList) {
				if (dnNode == nd) dnNode = NULL;
				if (pInsAfter == nd) pInsAfter = NULL, bInsValid = false;
			}

			sbList.erase(std::remove(sbList.begin(), sbList.end(), pSB), sbList.end());
			delete pSB;

			bool bInUse = false;
			for (SideBar *sb : sbList) for (Node *nd : sb->wList) if (nd->pApp == pApp) bInUse = true;
			if (!bInUse) g_gcGUIAppList.remove(pApp);
		}
	}
}


// ===============================================================================================
//
SideBar *WindowManager::NewSideBar(Node *pAN)
{
	for (SideBar* sb : sbList) if (sb->IsInactive() && sb->IsEmpty()) {
		sb->SetState(gcGUI::DS_FLOAT);
		sb->Show(false);
		return sb;
	}

	SideBar *pSB = new SideBar(this, gcGUI::DS_FLOAT);
	sbList.push_back(pSB);
	return pSB;
}


// ===============================================================================================
//
void WindowManager::ReleaseSideBar(SideBar *pSB)
{
	if (!pSB->IsFloater()) return;
	pSB->SetState(gcGUI::INACTIVE);
	pSB->Show(false);
	if (pInsTgt == pSB) pInsTgt = NULL, pInsAfter = NULL, bInsValid = false;
}


// ===============================================================================================
//
void WindowManager::SetOffset(float x, float y)
{
	xOffset = x;
	yOffset = y;
}


// ===============================================================================================
//
SideBar* WindowManager::StartDrag(Node *pAN, float x, float y)
{
	sbDragSrc = pAN->GetSideBar();
	SideBar *pSBOld = pAN->GetSideBar();
	SideBar *pSBNew = NewSideBar(pAN);

	pSBOld->RemoveWindow(pAN);
	pSBNew->AddWindow(pAN);

	if (pAN->pParent == NULL) {

		vector<Node*> tmp;

		for (Node * an : pSBOld->wList) if (an->pParent == pAN) tmp.push_back(an);

		for (Node * an : tmp)
		{
			pSBOld->RemoveWindow(an);
			pSBNew->AddWindow(an);
		}
	}

	sbDrag = pSBNew;

	pSBNew->ResetWindow(x - xOffset, y - yOffset);

	if (pSBOld->IsEmpty() && pSBOld->IsFloater() && pSBOld != pSBNew) ReleaseSideBar(pSBOld);

	return pSBNew;
}


// ===============================================================================================
//
void WindowManager::BeginMove(Node *pAN, float x, float y)
{
	sbDrag = pAN->GetSideBar();
}


// ===============================================================================================
//
void WindowManager::Drag(float x, float y)
{
	if (sbDrag) {

		if (sbDrag->GetTopNode() == NULL) return;

		SBRect r = sbDrag->GetRect();
		float w = r.right - r.left;
		float t = sbDrag->GetTopNode()->CellSize();
		if (t <= 0.0f) t = ImGui::GetFontSize() * 1.8f;

		x -= xOffset;
		y -= yOffset;

		const ImGuiViewport *vp = ImGui::GetMainViewport();
		float left = vp->WorkPos.x;
		float top = vp->WorkPos.y;
		float right = vp->WorkPos.x + vp->WorkSize.x;
		float bottom = vp->WorkPos.y + vp->WorkSize.y;

		if ((x + w) > right) x = right - w;
		if ((y + t) > bottom) y = bottom - t;
		if (x < left) x = left;
		if (y < top) y = top;

		sbDrag->ResetWindow(x, y);
	}
}


// ===============================================================================================
//
void WindowManager::EndDrag()
{
	sbDrag = NULL;
	sbDragSrc = NULL;
	sbDest = NULL;
	pInsTgt = NULL;
	pInsAfter = NULL;
	bInsValid = false;
}


// ===============================================================================================
//
SideBar *WindowManager::FindDestination()
{
	if (GetMode() != 1 || !sbDrag) return NULL;

	float z = 0.0f;
	SideBar *pOld = sbDest;
	sbDest = NULL;

	SBRect d = sbDrag->GetRect();

	for (SideBar* sb : sbList) {
		if (!sb->IsShown()) continue;
		if (sb != sbDrag) {
			SBRect r = sb->GetRect();
			float w = std::min(r.right, d.right) - std::max(r.left, d.left);
			float h = std::min(r.bottom, d.bottom) - std::max(r.top, d.top);
			if (w <= 0.0f || h <= 0.0f) continue;
			float a = w * h;
			if (a > z) { z = a;	sbDest = sb; }
		}
	}

	if (sbDest != pOld) {
		pInsAfter = NULL;
		bInsValid = false;
	}

	pInsTgt = sbDest;
	return sbDest;
}








// ===============================================================================================
// SideBar Implementation
// ===============================================================================================
//
SideBar::SideBar(class WindowManager *_pMgr, DWORD _state)
{
	pMgr = _pMgr;
	state = _state;
	bVisible = false;
	bSetPos = false;
	bFirst = true;
	xpos = ypos = 0.0f;
	width = height = 0.0f;
	toppos = 0.0f;
	cidx = 0;
	uid = pMgr->nextuid++;
}


// ===============================================================================================
//
SideBar::~SideBar()
{
	for (Node *v : wList) pMgr->DeleteNode(v);
	wList.clear();
}


// ===============================================================================================
//
bool SideBar::IsShown() const
{
	if (state == gcGUI::INACTIVE) return false;
	if (wList.empty()) return false;
	if (state == gcGUI::DS_FLOAT) return bVisible;
	return true;
}


// ===============================================================================================
//
float SideBar::GetWidth() const
{
	return pMgr->GetWidth();
}


// ===============================================================================================
//
void SideBar::ManageButtons()
{
	for (Node* nd : wList)
	{
		nd->bClose = false;

		bool bRootIncluded = false;
		if (nd->pParent) if (nd->pParent->GetSideBar() == this) bRootIncluded = true;
		if (nd->pParent == NULL) bRootIncluded = true;

		if (state == gcGUI::DS_FLOAT && !bRootIncluded) nd->bClose = true;
		if (pMgr->GetMode() == 2) if (nd->pParent == NULL) nd->bClose = true;
	}
}


// ===============================================================================================
//
void SideBar::ResetWindow(float x, float y)
{
	xpos = x;
	ypos = y;
	bSetPos = true;
	bFirst = false;
	bVisible = true;
}


// ===============================================================================================
//
SBRect SideBar::GetRect() const
{
	return { xpos, ypos, xpos + width, ypos + height };
}


// ===============================================================================================
//
DWORD SideBar::GetAutoColor()
{
	static DWORD color[] = { 0xf5d0ff, 0xfffebb, 0xbbfffc, 0xbaffc4, 0xffc0c0, 0xc6c5ff, 0 };
	DWORD c = color[cidx]; cidx++;
	if (color[cidx] == 0) cidx = 0;
	return c;
}


// ===============================================================================================
//
void SideBar::AddWindow(Node *pAp, bool bSetupOnly)
{
	pAp->pSB = this;
	if (!bSetupOnly) wList.push_back(pAp);
}


// ===============================================================================================
//
void SideBar::RemoveWindow(class Node *pAp)
{
	for (DWORD i = 0; i < wList.size(); i++) {
		if (wList[i] == pAp) {
			wList.erase(wList.begin() + i);
			return;
		}
	}
}


// ===============================================================================================
//
bool SideBar::IsOpen() const
{
	return IsShown();
}


// ===============================================================================================
//
void SideBar::GetVisualList(vector<Node*> &tmp)
{
	if (pMgr->GetMode() == 3) {
		for (Node* ap : wList) {
			if (ap->pParent && ap->pParent->pSB == this && !ap->pParent->bOpen && ap->pParent->HasTitle()) continue;
			tmp.push_back(ap);
		}
	}
	else
	{
		for (Node* ap : wList) {
			Node* pPar = ap->pParent;
			if (pPar) {
				if (pPar->pSB == this) {
					if (pPar->bOpen) tmp.push_back(ap);	// Visible Child
					else continue; // Hidden Child
				}
				else tmp.push_back(ap); // Foreing Child
			}
			else tmp.push_back(ap);	// Root Node
		}
	}
}


// ===============================================================================================
//
void SideBar::Sort()
{
	vector<Node*> tmp;
	for (Node* ap : wList) {
		Node* pPar = ap->pParent;
		if (pPar == NULL) {
			tmp.push_back(ap);	// Root Node
			for (Node* ch : wList) if (ch->pParent == ap) tmp.push_back(ch); // Child
		}
		else if (pPar->GetSideBar()!=this) tmp.push_back(ap); // Foreing Child
	}
	wList = tmp;
}


// ===============================================================================================
//
bool SideBar::Insert(Node *pNode, Node *pAfter)
{
	if (!pAfter) {
		wList.insert(wList.begin(), pNode);
		AddWindow(pNode, true);
		return true;
	}
	for (DWORD i = 0; i < wList.size(); i++) {
		if (wList[i] == pAfter) {
			wList.insert(wList.begin() + i + 1, pNode);
			AddWindow(pNode, true);
			return true;
		}
	}
	wList.push_back(pNode);
	AddWindow(pNode, true);
	return true;
}


// ===============================================================================================
//
Node *SideBar::GetTopNode()
{
	if (wList.size() == 0) return NULL;
	return wList.front();
}


// ===============================================================================================
//
bool SideBar::DoesExists(Node *pX)
{
	for (Node* ap : wList) if (ap == pX) return true;
	return false;
}


// ===============================================================================================
//
float SideBar::ComputeLength() const
{
	float y = 0.0f;
	for (Node* ap : wList) y += std::max(0.0f, ap->CellSize());
	return y;
}


// ===============================================================================================
//
Node *SideBar::FindClosest(vector<Node*> &vis, Node *pRoot, float yval)
{
	float d = FLT_MAX;
	Node *out = NULL;
	if (!pRoot) d = fabs(toppos - yval);
	for (Node* ap : vis) {
		float y = ap->yBottom;
		if (ap->pParent == pRoot || ap == pRoot || ap == vis.back()) {
			if (fabs(y - yval) < d) d = fabs(y - yval), out = ap;
		}
	}
	return out;
}


// ===============================================================================================
//
bool SideBar::TryInsert(SideBar *sbIn)
{
	if (pMgr->InsertTarget() != this) return false;

	vector<Node*> drawList;
	GetVisualList(drawList);

	Node *pNode = sbIn->GetTopNode();
	if (!pNode) return false;

	Node *pParent = pNode->pParent;
	if (!DoesExists(pParent)) pParent = NULL;

	Node *pPrev = pMgr->pInsAfter;
	bool bPrev = pMgr->bInsValid;

	pMgr->pInsAfter = FindClosest(drawList, pParent, sbIn->GetRect().top);
	pMgr->bInsValid = true;

	return (pPrev != pMgr->pInsAfter) || !bPrev;
}


// ===============================================================================================
//
bool SideBar::Apply()
{
	SideBar *pDG = pMgr->GetDraged();

	if (pDG && pDG != this && (pMgr->InsertTarget() == this) && pMgr->bInsValid)
	{
		vector<Node*> moving = pDG->wList;
		Node *pAfter = pMgr->pInsAfter;

		for (Node *pSrc : moving) {
			Insert(pSrc, pAfter);
			pAfter = pSrc;
		}

		pDG->wList.clear();
		pMgr->ReleaseSideBar(pDG);

		pMgr->pInsTgt = NULL;
		pMgr->pInsAfter = NULL;
		pMgr->bInsValid = false;

		Sort();
		ManageButtons();
		return true;
	}

	return false;
}


// ===============================================================================================
//
void SideBar::Spacer(float h)
{
	if (h <= 0.0f) return;
	ImVec2 p = ImGui::GetCursorScreenPos();
	float w = ImGui::GetContentRegionAvail().x;
	ImGui::GetWindowDrawList()->AddRectFilled(p, ImVec2(p.x + w, p.y + h), ImGui::GetColorU32(ImGuiCol_FrameBg));
	ImGui::Dummy(ImVec2(w, h));
}


// ===============================================================================================
//
void SideBar::Draw()
{
	if (!IsShown()) return;

	int mode = pMgr->GetMode();
	const ImGuiViewport *vp = ImGui::GetMainViewport();
	float w = GetWidth();
	bool bTitle = false;
	bool bOpenWnd = true;

	ImGuiWindowFlags fl = ImGuiWindowFlags_NoCollapse | ImGuiWindowFlags_NoSavedSettings;

	if (state == gcGUI::DS_LEFT || state == gcGUI::DS_RIGHT) {
		float top = vp->WorkPos.y + DOCK_TOP;
		float x = (state == gcGUI::DS_LEFT) ? vp->WorkPos.x : vp->WorkPos.x + vp->WorkSize.x - w;
		ImGui::SetNextWindowPos(ImVec2(x, top), ImGuiCond_Always);
		ImGui::SetNextWindowSize(ImVec2(w, std::max(1.0f, vp->WorkPos.y + vp->WorkSize.y - top)), ImGuiCond_Always);
		fl |= ImGuiWindowFlags_NoTitleBar | ImGuiWindowFlags_NoResize | ImGuiWindowFlags_NoMove;
	}
	else {
		if (bSetPos) {
			ImGui::SetNextWindowPos(ImVec2(xpos, ypos), ImGuiCond_Always);
			bSetPos = false;
		}
		else if (bFirst) {
			float off = float(uid % 8) * ImGui::GetFontSize() * 1.5f;
			ImGui::SetNextWindowPos(ImVec2(vp->WorkPos.x + w + off, vp->WorkPos.y + DOCK_TOP + off), ImGuiCond_Always);
		}
		bFirst = false;
		ImGui::SetNextWindowSizeConstraints(ImVec2(w, 0.0f), ImVec2(w, FLT_MAX));
		fl |= ImGuiWindowFlags_AlwaysAutoResize;
		if (mode == 3) bTitle = true;
		else fl |= ImGuiWindowFlags_NoTitleBar;
	}

	char id[300];
	Node *pTop = GetTopNode();
	if (bTitle && pTop) snprintf(id, sizeof(id), "%s###gcGUIBar%d", pTop->Label.c_str(), uid);
	else snprintf(id, sizeof(id), "##gcGUIBar%d", uid);

	bool bVis = ImGui::Begin(id, bTitle ? &bOpenWnd : NULL, fl);

	ImVec2 wp = ImGui::GetWindowPos();
	ImVec2 ws = ImGui::GetWindowSize();
	if (!bSetPos) xpos = wp.x, ypos = wp.y;
	width = ws.x;
	height = ws.y;

	if (bVis) {

		toppos = ImGui::GetCursorScreenPos().y;

		vector<Node*> drawList;
		GetVisualList(drawList);

		bool bInsert = (pMgr->InsertTarget() == this) && pMgr->bInsValid && pMgr->GetDraged();
		float insH = bInsert ? pMgr->GetDraged()->ComputeLength() : 0.0f;
		float shift = 0.0f;

		if (bInsert && pMgr->InsertAfter() == NULL) {
			float y0 = ImGui::GetCursorScreenPos().y;
			Spacer(insH);
			shift += ImGui::GetCursorScreenPos().y - y0;
		}

		for (size_t i = 0; i < drawList.size(); i++)
		{
			Node *ap = drawList[i];

			if (i > 0 && ap->pParent == NULL) ImGui::Separator();

			ap->Draw(shift);

			if (bInsert && pMgr->InsertAfter() == ap) {
				float y0 = ImGui::GetCursorScreenPos().y;
				Spacer(insH);
				shift += ImGui::GetCursorScreenPos().y - y0;
			}
		}
	}

	ImGui::End();

	if (!bOpenWnd && pTop) pMgr->CloseClicked(pTop);
}
