// Copyright (c) EvESpirit
// Licensed under the MIT License
// ==============================================================
// Scatterer.cpp - Core engine surface scatter (data + collision only)
// ==============================================================

#include "Scatterer.h"
#include "Planet.h"
#include "Orbiter.h"
#include "Log.h"
#include <algorithm>
#include <cmath>
#include <cstring>
#include <fstream>
#include <sstream>
#include <string>
#include <map>

extern Orbiter *g_pOrbiter;

#pragma pack(push,1)
struct DDS_PIXELFORMAT {
    uint32_t dwSize;
    uint32_t dwFlags;
    uint32_t dwFourCC;
    uint32_t dwRGBBitCount;
    uint32_t dwRBitMask;
    uint32_t dwGBitMask;
    uint32_t dwBBitMask;
    uint32_t dwABitMask;
};

struct DDS_HEADER {
    uint32_t dwSize;
    uint32_t dwFlags;
    uint32_t dwHeight;
    uint32_t dwWidth;
    uint32_t dwPitchOrLinearSize;
    uint32_t dwDepth;
    uint32_t dwMipMapCount;
    uint32_t dwReserved1[11];
    DDS_PIXELFORMAT ddspf;
    uint32_t dwCaps;
    uint32_t dwCaps2;
    uint32_t dwCaps3;
    uint32_t dwCaps4;
    uint32_t dwReserved2;
};
#pragma pack(pop)

static void DecodeDXT1Pixel(const uint8_t *data, int width, int x, int y, uint8_t &r, uint8_t &g, uint8_t &b) {
    int bx = x / 4, by = y / 4, bw = width / 4;
    const uint8_t *block = data + (by * bw + bx) * 8;
    uint16_t c0 = *(const uint16_t*)(block + 0);
    uint16_t c1 = *(const uint16_t*)(block + 2);
    uint32_t bits = *(const uint32_t*)(block + 4);
    int cx = x % 4, cy = y % 4, bit_idx = (cy * 4 + cx) * 2;
    uint32_t code = (bits >> bit_idx) & 3;
    int r0 = ((c0 >> 11) & 31) * 255 / 31, g0 = ((c0 >> 5) & 63) * 255 / 63, b0 = (c0 & 31) * 255 / 31;
    int r1 = ((c1 >> 11) & 31) * 255 / 31, g1 = ((c1 >> 5) & 63) * 255 / 63, b1 = (c1 & 31) * 255 / 31;
    if (c0 > c1) {
        if (code == 0) { r=r0; g=g0; b=b0; } else if (code == 1) { r=r1; g=g1; b=b1; }
        else if (code == 2) { r=(2*r0+r1)/3; g=(2*g0+g1)/3; b=(2*b0+b1)/3; } else { r=(r0+2*r1)/3; g=(g0+2*g1)/3; b=(b0+2*b1)/3; }
    } else {
        if (code == 0) { r=r0; g=g0; b=b0; } else if (code == 1) { r=r1; g=g1; b=b1; }
        else if (code == 2) { r=(r0+r1)/2; g=(g0+g1)/2; b=(b0+b1)/2; } else { r=0; g=0; b=0; }
    }
}

static void GetPixelRGB(const Scatterer::ScatterTileData &td, int x, int y, uint8_t &r, uint8_t &g, uint8_t &b) {
    if (!td.data) { r = 255; g = 128; b = 0; return; }
    if (x < 0) x = 0; if (x >= td.width) x = td.width - 1;
    if (y < 0) y = 0; if (y >= td.height) y = td.height - 1;
    if (td.isDXT1) {
        DecodeDXT1Pixel(td.data, td.width, x, y, r, g, b);
    } else if (td.isDXT5) {
        int bx = x / 4, by = y / 4, bw = td.width / 4;
        const uint8_t *block = td.data + (by * bw + bx) * 16 + 8;
        uint16_t c0 = *(const uint16_t*)(block + 0);
        uint16_t c1 = *(const uint16_t*)(block + 2);
        uint32_t bits = *(const uint32_t*)(block + 4);
        int cx = x % 4, cy = y % 4, bit_idx = (cy * 4 + cx) * 2;
        uint32_t code = (bits >> bit_idx) & 3;
        int r0 = ((c0 >> 11) & 31) * 255 / 31, g0 = ((c0 >> 5) & 63) * 255 / 63, b0 = (c0 & 31) * 255 / 31;
        int r1 = ((c1 >> 11) & 31) * 255 / 31, g1 = ((c1 >> 5) & 63) * 255 / 63, b1 = (c1 & 31) * 255 / 31;
        if (code == 0) { r=r0; g=g0; b=b0; } else if (code == 1) { r=r1; g=g1; b=b1; }
        else if (code == 2) { r=(2*r0+r1)/3; g=(2*g0+g1)/3; b=(2*b0+b1)/3; } else { r=(r0+2*r1)/3; g=(g0+2*g1)/3; b=(b0+2*b1)/3; }
    } else if (td.bpp > 0) {
        uint32_t pixel = 0;
        int idx = (y * td.width + x) * td.bpp;
        if (td.bpp == 1) pixel = td.data[idx];
        else if (td.bpp == 2) pixel = *(const uint16_t*)(td.data + idx);
        else if (td.bpp == 3) pixel = td.data[idx] | (td.data[idx+1]<<8) | (td.data[idx+2]<<16);
        else if (td.bpp == 4) pixel = *(const uint32_t*)(td.data + idx);
        
        if (td.bpp == 1) {
            r = g = b = (uint8_t)pixel;
        } else {
            auto getVal = [](uint32_t px, uint32_t mask) -> uint8_t {
                if (!mask) return 0;
                int shift = 0; uint32_t m = mask;
                while ((m & 1) == 0) { m >>= 1; shift++; }
                return (uint8_t)(((px & mask) >> shift) * 255 / m);
            };
            r = getVal(pixel, td.rMask);
            g = getVal(pixel, td.gMask);
            b = getVal(pixel, td.bMask);
        }
    }
}

// ---- PRNG helpers ----

uint32_t Scatterer::HashTile(uint32_t seed, int lvl, int ilat, int ilng) {
  uint32_t h = seed;
  h ^= (uint32_t)lvl * 2654435761u;
  h ^= (uint32_t)ilat * 2246822519u;
  h ^= (uint32_t)ilng * 3266489917u;
  h ^= h >> 16; h *= 0x85ebca6bu;
  h ^= h >> 13; h *= 0xc2b2ae35u;
  h ^= h >> 16;
  return h ? h : 1u;
}

uint32_t Scatterer::XorShift32(uint32_t &state) {
  state ^= state << 13; state ^= state >> 17; state ^= state << 5;
  return state;
}

float Scatterer::RandFloat(uint32_t &state) {
  return float(XorShift32(state) & 0x00FFFFFFu) / float(0x01000000u);
}

float Scatterer::RandRange(uint32_t &state, float lo, float hi) {
  return lo + RandFloat(state) * (hi - lo);
}

VECTOR3 Scatterer::Norm3(const VECTOR3 &v) {
  double len = sqrt(v.x*v.x + v.y*v.y + v.z*v.z);
  if (len < 1e-12) return _V(0,1,0);
  return _V(v.x/len, v.y/len, v.z/len);
}

// Collision geometry from loaded meshes

void Scatterer::BuildCollisionGeometry() {
  for (int i = 0; i < 3; i++) {
    m_collGeom[i].clear();
    m_meshBottomExtent[i].clear();
    m_meshCount[i] = 0;
  }

  if (m_cfg.sMeshPrefix[0] == '\0') {
    // Build simple icosphere collision geometry procedurally for each size class
    for (int sc = 0; sc < 3; sc++) {
      CollisionGeom cg;
      cg.maxRadius = 0.0f;

      // Simple 8-triangle octahedron as collision hull
      float s = (sc == 0) ? 0.5f : 1.0f;
      VECTOR3 top = _V(0, s*1.2, 0), bot = _V(0, -s*0.2, 0);
      VECTOR3 pts[4] = {_V(s,s*0.5,0), _V(0,s*0.5,s), _V(-s,s*0.5,0), _V(0,s*0.5,-s)};

      for (int i = 0; i < 4; i++) {
        int j = (i+1)%4;
        CollisionGeom::Tri t1 = {top, pts[i], pts[j]};
        CollisionGeom::Tri t2 = {bot, pts[j], pts[i]};
        cg.tris.push_back(t1);
        cg.tris.push_back(t2);
        for (auto *p : {&pts[i], &pts[j], &top, &bot}) {
          float r = (float)sqrt(p->x*p->x + p->y*p->y + p->z*p->z);
          if (r > cg.maxRadius) cg.maxRadius = r;
        }
      }

      m_collGeom[sc].push_back(cg);
      m_meshBottomExtent[sc].push_back(s * 0.2f);
      m_meshCount[sc] = 1;
    }
    return;
  }

  // Load collision geometry from mesh files
  char exePath[MAX_PATH];
  GetModuleFileNameA(NULL, exePath, MAX_PATH);
  char *lastSlash = strrchr(exePath, '\\');
  if (lastSlash) *(lastSlash + 1) = '\0';

  char searchPath[MAX_PATH];
  sprintf_s(searchPath, sizeof(searchPath), "%sMeshes\\%s*.msh", exePath, m_cfg.sMeshPrefix);

  WIN32_FIND_DATAA fd;
  std::vector<std::string> foundFiles;
  HANDLE hFind = FindFirstFileA(searchPath, &fd);
  if (hFind != INVALID_HANDLE_VALUE) {
    do { foundFiles.push_back(fd.cFileName); } while (FindNextFileA(hFind, &fd));
    FindClose(hFind);
  }
  std::sort(foundFiles.begin(), foundFiles.end());

  std::string dirPath = "";
  std::string prefix(m_cfg.sMeshPrefix);
  size_t slashPos = prefix.find_last_of("\\/");
  if (slashPos != std::string::npos) dirPath = prefix.substr(0, slashPos + 1);

  for (const auto &fname : foundFiles) {
    std::string base = fname;
    if (base.length() > 4) base.erase(base.length() - 4);

    std::string lowerBase = base;
    std::transform(lowerBase.begin(), lowerBase.end(), lowerBase.begin(), ::tolower);

    std::string relativePath = dirPath + base;
    MESHHANDLE hMesh = oapiLoadMeshGlobal(relativePath.c_str());
    if (!hMesh) continue;

    float minY = 0.0f;
    DWORD nGrp = oapiMeshGroupCount(hMesh);
    for (DWORD g = 0; g < nGrp; g++) {
      MESHGROUPEX *grp = oapiMeshGroupEx(hMesh, g);
      if (grp && grp->Vtx) {
        for (DWORD v = 0; v < grp->nVtx; v++)
          if (grp->Vtx[v].y < minY) minY = grp->Vtx[v].y;
      }
    }
    float bottomExtent = -minY;

    CollisionGeom cg;
    cg.maxRadius = 0.0f;
    for (DWORD gi = 0; gi < nGrp; gi++) {
      MESHGROUPEX *grp = oapiMeshGroupEx(hMesh, gi);
      if (!grp || !grp->Vtx || !grp->Idx) continue;
      for (DWORD t = 0; t + 2 < grp->nIdx; t += 3) {
        CollisionGeom::Tri tri;
        WORD i0 = grp->Idx[t], i1 = grp->Idx[t+1], i2 = grp->Idx[t+2];
        tri.v0 = _V(grp->Vtx[i0].x, grp->Vtx[i0].y, grp->Vtx[i0].z);
        tri.v1 = _V(grp->Vtx[i1].x, grp->Vtx[i1].y, grp->Vtx[i1].z);
        tri.v2 = _V(grp->Vtx[i2].x, grp->Vtx[i2].y, grp->Vtx[i2].z);
        cg.tris.push_back(tri);
        for (auto *vv : {&tri.v0, &tri.v1, &tri.v2}) {
          float r = (float)length(*vv);
          if (r > cg.maxRadius) cg.maxRadius = r;
        }
      }
    }

    bool isSmall  = (lowerBase.find("small")  != std::string::npos);
    bool isMedium = (lowerBase.find("medium") != std::string::npos);
    bool isLarge  = (lowerBase.find("large")  != std::string::npos);

    if (!isSmall && !isMedium && !isLarge) {
      for (int i = 0; i < 3; i++) {
        m_collGeom[i].push_back(cg);
        m_meshBottomExtent[i].push_back(bottomExtent);
      }
    } else {
      if (isSmall)  { m_collGeom[0].push_back(cg); m_meshBottomExtent[0].push_back(bottomExtent); }
      if (isMedium) { m_collGeom[1].push_back(cg); m_meshBottomExtent[1].push_back(bottomExtent); }
      if (isLarge)  { m_collGeom[2].push_back(cg); m_meshBottomExtent[2].push_back(bottomExtent); }
    }
  }

  // Fill empty pools from non-empty ones
  for (int i = 0; i < 3; i++) {
    if (m_collGeom[i].empty()) {
      for (int j = 0; j < 3; j++) {
        if (!m_collGeom[j].empty()) {
          m_collGeom[i] = m_collGeom[j];
          m_meshBottomExtent[i] = m_meshBottomExtent[j];
          break;
        }
      }
    }
    m_meshCount[i] = (int)m_collGeom[i].size();
  }
}

// Constructor / Destructor

Scatterer::Scatterer(Planet *planet)
    : m_planet(planet), m_seed(0),
      m_lastDensityMult(g_pOrbiter->Cfg()->CfgVisualPrm.fScatterDensityMult) {
  memset(&m_cfg, 0, sizeof(m_cfg));
  m_meshCount[0] = m_meshCount[1] = m_meshCount[2] = 0;
  m_scatterTreeMgr = nullptr;
  m_bScatterDirExists = false;

  // Read config from planet - it must have been parsed already
  // (Planet stores it in ScatterCfg)
  memcpy(&m_cfg, &planet->ScatterCfg, sizeof(ScattererCfg));

  char cbuf[256], path[256], fname[256];
  if (planet->Name()) {
    g_pOrbiter->Cfg()->PTexPath(cbuf, planet->Name());
    m_scatterTreeMgr = ZTreeMgr::CreateFromFile(cbuf, ZTreeMgr::LAYER_SCATTER);
    sprintf(fname, "%s\\Scatter", planet->Name());
    g_pOrbiter->Cfg()->PTexPath(path, fname);
    DWORD attr = GetFileAttributesA(path);
    m_bScatterDirExists = (attr != INVALID_FILE_ATTRIBUTES && (attr & FILE_ATTRIBUTE_DIRECTORY));
  }

  const char *name = planet->Name();
  uint32_t h = 5381u;
  if (name)
    for (const char *p = name; *p; p++)
      h = h * 33u + (uint32_t)*p;
  h ^= m_cfg.uSeed;
  m_seed = h ? h : 1u;

  BuildCollisionGeometry();
  LoadBaseClearZones();
}

Scatterer::~Scatterer() {
  std::lock_guard<std::mutex> lock(m_cacheMutex);
  m_cache.clear();
  for (auto &pair : m_mapCache) {
      if (pair.second.originalData) {
          if (m_scatterTreeMgr && !m_bScatterDirExists) m_scatterTreeMgr->ReleaseData(pair.second.originalData);
          else delete [] pair.second.originalData;
      }
  }
  m_mapCache.clear();
  if (m_scatterTreeMgr) delete m_scatterTreeMgr;
}

// Base clear zones

void Scatterer::LoadBaseClearZones() {
  m_clearZones.clear();
  OBJHANDLE hPlanet = (OBJHANDLE)m_planet;
  if (!hPlanet) return;

  DWORD nBases = oapiGetBaseCount(hPlanet);
  if (nBases == 0) return;

  for (DWORD b = 0; b < nBases; b++) {
    OBJHANDLE hBase = oapiGetBaseByIndex(hPlanet, b);
    if (!hBase) continue;

    double baseLng = 0.0, baseLat = 0.0;
    oapiGetBaseEquPos(hBase, &baseLng, &baseLat);

    const char *cfgFile = oapiGetObjectFileName(hBase);
    if (!cfgFile || !cfgFile[0]) continue;

    std::ifstream fs(cfgFile);
    if (fs.fail()) continue;

    int zonesFound = 0;
    std::string line;
    while (std::getline(fs, line) && zonesFound < MAX_CLEAR_AREAS_PER_BASE) {
      if (line.find("AREA_TO_CLEAR_") == std::string::npos) continue;
      size_t eq = line.find('=');
      if (eq == std::string::npos) continue;
      std::string values = line.substr(eq + 1);
      for (char &c : values) if (c == ',') c = ' ';

      float x = 0.0f, y = 0.0f;
      std::istringstream iss(values);
      if (!(iss >> x >> y)) continue;
      if (x <= 0.0f || y <= 0.0f) continue;

      ClearZone cz;
      cz.baseLng = baseLng; cz.baseLat = baseLat;
      cz.halfExtX = x; cz.halfExtY = y;
      m_clearZones.push_back(cz);
      zonesFound++;
    }
  }
}

bool Scatterer::IsInClearZone(double lng, double lat) const {
  if (m_clearZones.empty()) return false;
  double planetRad = m_planet->Size();

  for (const auto &cz : m_clearZones) {
    double maxArc = (double)std::max(cz.halfExtX, cz.halfExtY) / planetRad * 1.5;
    double dLat = lat - cz.baseLat;
    if (fabs(dLat) > maxArc) continue;
    double dLng = lng - cz.baseLng;
    while (dLng > PI) dLng -= 2.0 * PI;
    while (dLng < -PI) dLng += 2.0 * PI;
    if (fabs(dLng) > maxArc) continue;
    double dx = dLng * cos(lat) * planetRad;
    double dy = dLat * planetRad;
    if (fabs(dx) <= (double)cz.halfExtX && fabs(dy) <= (double)cz.halfExtY)
      return true;
  }
  return false;
}

// Scatter generation

void Scatterer::LoadScatterMapTile(int lvl, int ilat, int ilng, ScatterTileData &tileData) const {
  BYTE *buf = nullptr;
  DWORD size = 0;
  
  int cur_lvl = lvl;
  int cur_ilat = ilat;
  int cur_ilng = ilng;

  while (cur_lvl >= 0) {
      if (m_bScatterDirExists) {
          char fname[256], path[256];
          sprintf(fname, "%s\\Scatter\\%02d\\%06d\\%06d.dds", m_planet->Name(), cur_lvl, cur_ilat, cur_ilng);
          g_pOrbiter->Cfg()->PTexPath(path, fname);
          FILE *f = fopen(path, "rb");
          if (f) {
              fseek(f, 0, SEEK_END);
              size = ftell(f);
              fseek(f, 0, SEEK_SET);
              if (size > 0) {
                  buf = new BYTE[size];
                  fread(buf, 1, size, f);
              }
              fclose(f);
          }
      }
      
      if (!buf && m_scatterTreeMgr) {
          // Scatter map directory levels are offset by 4 from ZTreeMgr quadtree levels:
          // Dir level 0 (1 lat x 2 lng) = tree level 4 (rootPos4)
          size = m_scatterTreeMgr->ReadData(cur_lvl + 4, cur_ilat, cur_ilng, &buf);
      }
      
      if (buf) break; // Successfully found a tile
      
      // Fallback to parent level
      cur_lvl--;
      cur_ilat /= 2;
      cur_ilng /= 2;
  }
  
  if (!buf) return;

  tileData.lvl_loaded = cur_lvl;
  tileData.ilat_loaded = cur_ilat;
  tileData.ilng_loaded = cur_ilng;
  
  // Parse DDS header
  if (size > 128 && memcmp(buf, "DDS ", 4) == 0) {
      DDS_HEADER *hdr = (DDS_HEADER*)(buf + 4);
      tileData.width = hdr->dwWidth;
      tileData.height = hdr->dwHeight;
      tileData.originalData = buf;
      tileData.data = buf; // retain ownership
      
      uint32_t flags = hdr->ddspf.dwFlags;
      if (flags & 0x4) { // DDPF_FOURCC
          uint32_t fcc = hdr->ddspf.dwFourCC;
          if (fcc == 0x31545844) tileData.isDXT1 = true; // DXT1
          else if (fcc == 0x35545844) tileData.isDXT5 = true; // DXT5
          tileData.data = buf + 128;
      } else if (flags & 0x40) { // DDPF_RGB
          tileData.bpp = hdr->ddspf.dwRGBBitCount / 8;
          tileData.rMask = hdr->ddspf.dwRBitMask;
          tileData.gMask = hdr->ddspf.dwGBitMask;
          tileData.bMask = hdr->ddspf.dwBBitMask;
          tileData.aMask = hdr->ddspf.dwABitMask;
          tileData.data = buf + 128;
      } else {
          // Unsupported DDS format
          if (m_scatterTreeMgr && !m_bScatterDirExists) m_scatterTreeMgr->ReleaseData(buf);
          else delete [] buf;
          tileData.data = nullptr;
          tileData.originalData = nullptr;
      }
  } else {
      if (m_scatterTreeMgr && !m_bScatterDirExists) m_scatterTreeMgr->ReleaseData(buf);
      else delete [] buf;
      tileData.data = nullptr;
      tileData.originalData = nullptr;
  }
}

const std::vector<ScatterInstance>&
Scatterer::GetScatterForTile(int lvl, int ilat, int ilng) const {
  TileKey key = {lvl, ilat, ilng};
  std::lock_guard<std::mutex> lock(m_cacheMutex);
  
  float currentDensityMult = g_pOrbiter->Cfg()->CfgVisualPrm.fScatterDensityMult;
  if (currentDensityMult != m_lastDensityMult) {
    m_cache.clear();
    m_lastDensityMult = currentDensityMult;
  }
  
  auto it = m_cache.find(key);
  if (it != m_cache.end()) return it->second;

  auto &rocks = m_cache[key];

  double tileSize = PI / double(1 << lvl);
  double latMin = PI * 0.5 - tileSize * (ilat + 1);
  double latMax = PI * 0.5 - tileSize * ilat;
  double lngMin = -PI + tileSize * ilng;

  double planetRad = m_planet->Size();
  double midLat = (latMin + latMax) * 0.5;
  double areaM2 = (tileSize * planetRad) * (tileSize * cos(midLat) * planetRad);
  if (areaM2 < 1.0) return rocks;

  if (!m_cfg.bEnabled) return rocks;
  if (!g_pOrbiter->Cfg()->CfgVisualPrm.bSurfaceScatter) return rocks;
  
  float densityMult = g_pOrbiter->Cfg()->CfgVisualPrm.fScatterDensityMult;
  if (densityMult <= 0.0f) return rocks;
  
  float density = m_cfg.fDensity;  // densityMult applied as probability filter below

  // Scale density down for extremely tiny bodies (like Phobos/Deimos) to prevent visual overcrowding
  // due to their tight surface curvature and full-body rendering within the draw distance.
  float curRad = (float)m_planet->Size();
  float sizeScale = 1.0f;
  if (curRad < 50000.0f) {
      sizeScale = curRad / 50000.0f;
      if (sizeScale < 0.1f) sizeScale = 0.1f;
  }

  int maxCandidates = (int)(density * areaM2 * sizeScale);
  if (maxCandidates > 250000) maxCandidates = 250000;
  if (maxCandidates <= 0) return rocks;

  int targetRocks = 25000;

  uint32_t rng = HashTile(m_seed, lvl, ilat, ilng);
  rocks.reserve(std::min(maxCandidates, targetRocks));

  for (int i = 0; i < maxCandidates; i++) {
    double lat = latMin + RandFloat(rng) * (latMax - latMin);
    double lng = lngMin + RandFloat(rng) * tileSize;

    ScatterTileData td;
    {
      auto mt = m_mapCache.find(key);
      if (mt == m_mapCache.end()) {
          LoadScatterMapTile(lvl, ilat, ilng, td);
          m_mapCache[key] = td;
      } else {
          td = mt->second;
      }
    }
    
    uint8_t red = 255, green = 127, blue = 0;
    if (td.data) {
        // Calculate the exact world bounds of the loaded parent tile
        double td_tileSize = PI / double(1 << td.lvl_loaded);
        double td_latMax = PI * 0.5 - td_tileSize * td.ilat_loaded;
        double td_lngMin = -PI + td_tileSize * td.ilng_loaded;

        float tx = (float)((lng - td_lngMin) / td_tileSize);
        float ty = (float)((td_latMax - lat) / td_tileSize);
        int px = (int)(tx * td.width);
        int py = (int)(ty * td.height);
        GetPixelRGB(td, px, py, red, green, blue);
        
        if (i == 0) {
            char logbuf[256];
            sprintf(logbuf, "Scatterer Debug: req_lvl=%d, loaded_lvl=%d, tx=%.3f, ty=%.3f, px=%d, py=%d, red=%d, maxCandidates=%d", lvl, td.lvl_loaded, tx, ty, px, py, red, maxCandidates);
            oapiWriteLog(logbuf);
        }
    } else if (i == 0) {
        char logbuf[256];
        sprintf(logbuf, "Scatterer Debug: req_lvl=%d, NO DATA LOADED", lvl);
        oapiWriteLog(logbuf);
    }
    
    // Determine probability based on quadtree presence
    float prob = 1.0f;
    if (m_bScatterDirExists || m_scatterTreeMgr) {
        // Base floor: 25% probability everywhere (the Moon almost always has scatter).
        // Map data adds up to 75% more, using sqrt() to stretch the compressed low-end
        // Diviner values into visible density differences. The densest areas hit 100%.
        const float BASE_PROB = 0.25f;
        if (td.data) {
            float mapFactor = sqrtf(red / 91.0f);
            if (mapFactor > 1.0f) mapFactor = 1.0f;
            prob = BASE_PROB + (1.0f - BASE_PROB) * mapFactor;
        }
        else {
            // Missing tile = barren mare plain
            prob = 0.08f;
        }
    } else {
        // Procedural Density Fallback for bodies without a quadtree
        // Create natural clumping using coarse lat/lng clustering
        int coarse_lat = (int)(lat * 40.0);
        int coarse_lng = (int)(lng * 40.0);
        uint32_t clump_rng = HashTile(m_seed, 0, coarse_lat, coarse_lng);
        float noise = RandFloat(clump_rng); // 0.0 to 1.0
        
        // Base 5% + up to 45% based on noise (average ~27.5%)
        prob = 0.05f + 0.45f * noise;
    }
    prob *= densityMult;
    
    if (RandFloat(rng) > prob) {
        // Consume the same RNG calls that an accepted rock would use,
        // keeping the sequence synchronised so that lowering density
        // only removes rocks without reshuffling the survivors.
        RandFloat(rng);              // size-class roll
        RandFloat(rng);              // scale (RandRange)
        RandFloat(rng);              // meshIndex
        RandFloat(rng);              // rotY
        continue;
    }

    ScatterInstance rock;
    double clat = cos(lat), slat = sin(lat), clng = cos(lng), slng = sin(lng);
    rock.localPos = _V(clat * clng, slat, clat * slng);
    rock.elevation = (float)oapiSurfaceElevation((OBJHANDLE)m_planet, lng, lat);

    // We only have a 1-channel density map, so rely on the procedural engine's
    // uniform distribution logic for the rock size class sorting.
    float r = RandFloat(rng);
    
    if (r < m_cfg.fRatioSmall) {
      rock.sizeClass = 0;
      rock.scale = RandRange(rng, m_cfg.fSizeSmall[0], m_cfg.fSizeSmall[1]);
    } else if (r < m_cfg.fRatioSmall + m_cfg.fRatioMedium) {
      rock.sizeClass = 1;
      rock.scale = RandRange(rng, m_cfg.fSizeMedium[0], m_cfg.fSizeMedium[1]);
    } else {
      rock.sizeClass = 2;
      rock.scale = RandRange(rng, m_cfg.fSizeLarge[0], m_cfg.fSizeLarge[1]);
    }

    int poolSize = m_meshCount[rock.sizeClass];
    if (poolSize <= 0) poolSize = 1;
    rock.meshIndex = (uint8_t)(RandFloat(rng) * poolSize);
    if (rock.meshIndex >= poolSize) rock.meshIndex = poolSize - 1;
    rock.rotY = RandFloat(rng) * 6.283185f;

    if (!m_clearZones.empty()) {
      double rockLat = asin(rock.localPos.y);
      double rockLng = atan2(rock.localPos.z, rock.localPos.x);
      if (IsInClearZone(rockLng, rockLat)) continue;
    }
    rocks.push_back(rock);
    if (rocks.size() >= (size_t)targetRocks) break;
  }
  return rocks;
}

// Raycast

float Scatterer::RaycastMeshY(const CollisionGeom &geom, float lx, float lz) {
  float bestY = -1e30f;
  for (const auto &tri : geom.tris) {
    float ax = (float)tri.v0.x, az = (float)tri.v0.z;
    float bx = (float)tri.v1.x, bz = (float)tri.v1.z;
    float cx = (float)tri.v2.x, cz = (float)tri.v2.z;
    float d00 = (bx-ax)*(bx-ax) + (bz-az)*(bz-az);
    float d01 = (bx-ax)*(cx-ax) + (bz-az)*(cz-az);
    float d11 = (cx-ax)*(cx-ax) + (cz-az)*(cz-az);
    float d20 = (lx-ax)*(bx-ax) + (lz-az)*(bz-az);
    float d21 = (lx-ax)*(cx-ax) + (lz-az)*(cz-az);
    float denom = d00*d11 - d01*d01;
    if (fabs(denom) < 1e-12f) continue;
    float v = (d11*d20 - d01*d21) / denom;
    float w = (d00*d21 - d01*d20) / denom;
    float u = 1.0f - v - w;
    if (u >= -0.01f && v >= -0.01f && w >= -0.01f) {
      float y = (float)(u*tri.v0.y + v*tri.v1.y + w*tri.v2.y);
      if (y > bestY) bestY = y;
    }
  }
  return bestY;
}

// Elevation modifier

double Scatterer::GetElevationModifier(double lng, double lat) const {
  if (!m_cfg.bEnabled) return 0.0;

  double planetRad = m_planet->Size();
  double tgtTileSize = m_cfg.fDrawDist * 2.0 / planetRad;
  int lvl = 1;
  while ((PI / double(1 << lvl)) > tgtTileSize && lvl < 15) lvl++;
  if (lvl < 4) lvl = 4;

  double tileSize = PI / double(1 << lvl);
  int centerIlat = (int)((PI * 0.5 - lat) / tileSize);
  int centerIlng = (int)((lng + PI) / tileSize);
  int nLngBands = 1 << (lvl + 1);

  double maxElev = 0.0;
  for (int dlat = -1; dlat <= 1; dlat++) {
    int tilat = centerIlat + dlat;
    if (tilat < 0 || tilat >= (1 << lvl)) continue;
    for (int dlng = -1; dlng <= 1; dlng++) {
      int tilng = (centerIlng + dlng + nLngBands) % nLngBands;
      const auto &rocks = GetScatterForTile(lvl, tilat, tilng);
      for (const auto &rock : rocks) {
        if (rock.meshIndex >= (int)m_collGeom[rock.sizeClass].size()) continue;
        const CollisionGeom &cg = m_collGeom[rock.sizeClass][rock.meshIndex];
        double boundingR = (double)rock.scale * cg.maxRadius;

        float bottomOfs = 0.0f;
        if (rock.meshIndex < (int)m_meshBottomExtent[rock.sizeClass].size())
          bottomOfs = m_meshBottomExtent[rock.sizeClass][rock.meshIndex];

        double rockLat = asin(rock.localPos.y);
        double rockLng = atan2(rock.localPos.z, rock.localPos.x);
        double dLat = lat - rockLat, dLng = lng - rockLng;
        while (dLng > PI) dLng -= 2.0*PI;
        while (dLng < -PI) dLng += 2.0*PI;
        double dx_m = dLng * cos(lat) * planetRad;
        double dy_m = dLat * planetRad;
        if (dx_m*dx_m + dy_m*dy_m > boundingR*boundingR) continue;

        float cosR = cosf(-rock.rotY), sinR = sinf(-rock.rotY);
        float localX = (float)((dx_m*cosR + dy_m*sinR) / rock.scale);
        float localZ = (float)((-dx_m*sinR + dy_m*cosR) / rock.scale);
        float meshY = RaycastMeshY(cg, localX, localZ);
        if (meshY > -1e20f) {
          double h = ((double)meshY + (double)bottomOfs) * rock.scale;
          if (h > 0.0 && h > maxElev) maxElev = h;
        }
      }
    }
  }
  return maxElev;
}

// Collisions

Scatterer::CollisionResult
Scatterer::CheckCollision(const VECTOR3 *hullPts, int nPts,
                            const VECTOR3 &vesselPosLocal, double vesselRadius,
                            float maxCollisionDist) const {
  CollisionResult result = {false, {0,0,0}, 0.0, {0,0,0}};
  if (nPts <= 0 || !hullPts || !m_cfg.bEnabled) return result;

  double planetRad = oapiGetSize((OBJHANDLE)m_planet);
  if (planetRad < 1.0) return result;

  double vLen = length(vesselPosLocal);
  if (vLen < 1.0) return result;
  double vAlt = vLen - planetRad;
  if (vAlt > 500.0) return result;

  double vLat = asin(vesselPosLocal.y / vLen);
  double vLng = atan2(vesselPosLocal.z, vesselPosLocal.x);

  float drawDist = m_cfg.fDrawDist;
  int lvl = (int)(log2(PI / (drawDist / planetRad)));
  if (lvl < 1) lvl = 1;
  if (lvl > 19) lvl = 19;

  int nLat = 1 << lvl, nLng = 2 * nLat;
  int centerIlat = (int)((PI*0.5 - vLat) * nLat / PI);
  int centerIlng = (int)((vLng + PI) * nLng / (2.0*PI));
  centerIlat = std::max(0, std::min(centerIlat, nLat-1));
  centerIlng = ((centerIlng % nLng) + nLng) % nLng;

  double maxCollDist2 = (double)maxCollisionDist * (double)maxCollisionDist;
  double deepestPen = 0.0;
  int searchR = 2;

  for (int dLat = -searchR; dLat <= searchR; dLat++) {
    int tilat = centerIlat + dLat;
    if (tilat < 0 || tilat >= nLat) continue;
    for (int dLng = -searchR; dLng <= searchR; dLng++) {
      int tilng = ((centerIlng + dLng) % nLng + nLng) % nLng;
      const auto &rocks = GetScatterForTile(lvl, tilat, tilng);

      for (const auto &rock : rocks) {
        if (rock.meshIndex >= (int)m_collGeom[rock.sizeClass].size()) continue;
        const CollisionGeom &cg = m_collGeom[rock.sizeClass][rock.meshIndex];

        float bottomOfs = 0.0f;
        if (rock.meshIndex < (int)m_meshBottomExtent[rock.sizeClass].size())
          bottomOfs = m_meshBottomExtent[rock.sizeClass][rock.meshIndex];

        double rockAlt = planetRad + (double)rock.elevation + (double)bottomOfs * rock.scale;
        double rockWX = rock.localPos.x * rockAlt;
        double rockWY = rock.localPos.y * rockAlt;
        double rockWZ = rock.localPos.z * rockAlt;

        double dx = vesselPosLocal.x - rockWX;
        double dy = vesselPosLocal.y - rockWY;
        double dz = vesselPosLocal.z - rockWZ;
        double dist2Center = dx*dx + dy*dy + dz*dz;
        if (dist2Center > maxCollDist2) continue;

        double distCenter = sqrt(dist2Center);
        double rockBoundR = (double)rock.scale * cg.maxRadius;
        if (distCenter > vesselRadius + rockBoundR + 5.0) continue;

        VECTOR3 up = Norm3(rock.localPos);
        VECTOR3 right, fwd;
        if (fabs(up.y) < 0.99)
          right = Norm3(_V(-up.z, 0, up.x));
        else
          right = _V(1, 0, 0);
        fwd = crossp(up, right);

        float cy = cosf(rock.rotY), sy = sinf(rock.rotY);
        VECTOR3 rr = right*cy + fwd*sy;
        VECTOR3 ff = fwd*cy - right*sy;
        float invScale = 1.0f / rock.scale;

        for (int p = 0; p < nPts; p++) {
          double hx = hullPts[p].x - rockWX;
          double hy = hullPts[p].y - rockWY;
          double hz = hullPts[p].z - rockWZ;
          double ptDist2 = hx*hx + hy*hy + hz*hz;
          if (ptDist2 > rockBoundR*rockBoundR) continue;

          float localX = (float)(hx*rr.x + hy*rr.y + hz*rr.z) * invScale;
          float localY = (float)(hx*up.x + hy*up.y + hz*up.z) * invScale;
          float localZ = (float)(hx*ff.x + hy*ff.y + hz*ff.z) * invScale;

          float meshY = RaycastMeshY(cg, localX, localZ);
          if (meshY <= -1e20f) continue;
          if (localY < -bottomOfs) continue;

          if (localY < meshY) {
            double pen = ((double)meshY - (double)localY) * rock.scale;
            if (pen > deepestPen) {
              deepestPen = pen;
              result.hit = true;
              result.contactPtLocal = hullPts[p];
              if (distCenter > 1e-6) {
                result.normal = _V(dx/distCenter, dy/distCenter, dz/distCenter);
              } else {
                result.normal = up;
              }
              double sphereOverlap = rockBoundR - sqrt(ptDist2);
              result.depth = (sphereOverlap > 0) ? std::min(pen, sphereOverlap) : std::min(pen, 0.01);
            }
          }
        }
      }
    }
  }
  return result;
}
