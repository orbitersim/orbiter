// ==============================================================
// RockScatter.cpp
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Dual licensed under GPL v3 and LGPL v3
// ==============================================================

#include "RockScatter.h"
#include "D3D9Config.h"
#include "Log.h"
#include "OapiExtension.h"
#include "Scene.h"
#include "VPlanet.h"
#include <algorithm>
#include <cmath>
#include <cstring>
#include <fstream>
#include <sstream>
#include <string>

// Quality multipliers for rock density: Off / Low / Medium / High
static const float g_qualityMult[] = {0.0f, 0.25f, 0.6f, 1.0f};

// Draw distance multipliers (config)
static inline float GetLodCull(int sizeClass) {
  switch (sizeClass) {
  case 0:
    return Config->fRockDistSmall;
  case 1:
    return Config->fRockDistMedium;
  case 2:
    return Config->fRockDistLarge;
  default:
    return 1.0f;
  }
}

// PRNG helpers

uint32_t RockScatter::HashTile(uint32_t seed, int lvl, int ilat, int ilng) {
  uint32_t h = seed;
  h ^= (uint32_t)lvl * 2654435761u;
  h ^= (uint32_t)ilat * 2246822519u;
  h ^= (uint32_t)ilng * 3266489917u;
  h ^= h >> 16;
  h *= 0x85ebca6bu;
  h ^= h >> 13;
  h *= 0xc2b2ae35u;
  h ^= h >> 16;
  return h ? h : 1u;
}

uint32_t RockScatter::XorShift32(uint32_t &state) {
  state ^= state << 13;
  state ^= state >> 17;
  state ^= state << 5;
  return state;
}

float RockScatter::RandFloat(uint32_t &state) {
  return float(XorShift32(state) & 0x00FFFFFFu) / float(0x01000000u);
}

float RockScatter::RandRange(uint32_t &state, float lo, float hi) {
  return lo + RandFloat(state) * (hi - lo);
}

// Basic meshgen using vertex displacement (icosphere)

// normalise a D3DXVECTOR3
static D3DXVECTOR3 Norm3(const D3DXVECTOR3 &v) {
  D3DXVECTOR3 o;
  D3DXVec3Normalize(&o, &v);
  return o;
}

// subdivide one triangle of an icosphere
static void SubdivideIco(std::vector<NTVERTEX> &verts, std::vector<WORD> &idxs,
                         std::map<uint64_t, WORD> &midCache, WORD i0, WORD i1,
                         WORD i2, int depth) {
  if (depth == 0) {
    idxs.push_back(i0);
    idxs.push_back(i1);
    idxs.push_back(i2);
    return;
  }
  // get or create midpoint vertex
  auto getMid = [&](WORD a, WORD b) -> WORD {
    uint64_t key = (uint64_t)min(a, b) << 32 | (uint64_t)max(a, b);
    auto it = midCache.find(key);
    if (it != midCache.end())
      return it->second;
    D3DXVECTOR3 p = Norm3(D3DXVECTOR3((verts[a].x + verts[b].x) * 0.5f,
                                      (verts[a].y + verts[b].y) * 0.5f,
                                      (verts[a].z + verts[b].z) * 0.5f));
    NTVERTEX nv;
    memset(&nv, 0, sizeof(nv));
    nv.x = p.x;
    nv.y = p.y;
    nv.z = p.z;
    nv.nx = p.x;
    nv.ny = p.y;
    nv.nz = p.z;
    WORD idx = (WORD)verts.size();
    verts.push_back(nv);
    midCache[key] = idx;
    return idx;
  };
  WORD m01 = getMid(i0, i1), m12 = getMid(i1, i2), m02 = getMid(i0, i2);
  SubdivideIco(verts, idxs, midCache, i0, m01, m02, depth - 1);
  SubdivideIco(verts, idxs, midCache, i1, m12, m01, depth - 1);
  SubdivideIco(verts, idxs, midCache, i2, m02, m12, depth - 1);
  SubdivideIco(verts, idxs, midCache, m01, m12, m02, depth - 1);
}

void RockScatter::CreateIcosphereMesh(int subdivisions, UINT seed,
                                      float baseScale,
                                      std::vector<NTVERTEX> &outVerts,
                                      std::vector<WORD> &outIdxs) {
  // Assembly
  const float t = (1.0f + sqrtf(5.0f)) * 0.5f;
  D3DXVECTOR3 baseVerts[] = {
      Norm3({-1, t, 0}),  Norm3({1, t, 0}),   Norm3({-1, -t, 0}),
      Norm3({1, -t, 0}),  Norm3({0, -1, t}),  Norm3({0, 1, t}),
      Norm3({0, -1, -t}), Norm3({0, 1, -t}),  Norm3({t, 0, -1}),
      Norm3({t, 0, 1}),   Norm3({-t, 0, -1}), Norm3({-t, 0, 1})};
  static const WORD baseTris[] = {
      0, 11, 5,  0, 5,  1, 0, 1, 7, 0, 7,  10, 0, 10, 11, 1, 5, 9, 5, 11,
      4, 11, 10, 2, 10, 7, 6, 7, 1, 8, 3,  9,  4, 3,  4,  2, 3, 2, 6, 3,
      6, 8,  3,  8, 9,  4, 9, 5, 2, 4, 11, 6,  2, 10, 8,  6, 7, 9, 8, 1};

  outVerts.resize(12);
  for (int i = 0; i < 12; i++) {
    memset(&outVerts[i], 0, sizeof(NTVERTEX));
    outVerts[i].x = baseVerts[i].x;
    outVerts[i].y = baseVerts[i].y;
    outVerts[i].z = baseVerts[i].z;
    outVerts[i].nx = baseVerts[i].x;
    outVerts[i].ny = baseVerts[i].y;
    outVerts[i].nz = baseVerts[i].z;
  }

  outIdxs.clear();
  std::map<uint64_t, WORD> midCache;
  for (int i = 0; i < 20; i++)
    SubdivideIco(outVerts, outIdxs, midCache, baseTris[i * 3],
                 baseTris[i * 3 + 1], baseTris[i * 3 + 2], subdivisions);

  // displace vertices using seeded noise to make a rock shape
  uint32_t rng = seed ? seed : 42u;
  for (auto &v : outVerts) {
    float disp = 1.0f + (RandFloat(rng) - 0.5f) * 0.6f; // +-30%
    v.x *= disp * baseScale;
    v.y *= disp * baseScale * 0.7f; // flatten vertically
    v.z *= disp * baseScale;

    // without this they're kinda sunken in
    v.y += baseScale * 0.5f;
  }

  // recompute normals
  for (auto &v : outVerts) {
    v.nx = 0;
    v.ny = 0;
    v.nz = 0;
  }
  for (size_t i = 0; i + 2 < outIdxs.size(); i += 3) {
    auto &a = outVerts[outIdxs[i]], &b = outVerts[outIdxs[i + 1]],
         &c = outVerts[outIdxs[i + 2]];
    D3DXVECTOR3 e1(b.x - a.x, b.y - a.y, b.z - a.z),
        e2(c.x - a.x, c.y - a.y, c.z - a.z), n;
    D3DXVec3Cross(&n, &e1, &e2);
    D3DXVec3Normalize(&n, &n);
    a.nx += n.x;
    a.ny += n.y;
    a.nz += n.z;
    b.nx += n.x;
    b.ny += n.y;
    b.nz += n.z;
    c.nx += n.x;
    c.ny += n.y;
    c.nz += n.z;
  }
  for (auto &v : outVerts) {
    D3DXVECTOR3 n = Norm3({v.nx, v.ny, v.nz});
    v.nx = n.x;
    v.ny = n.y;
    v.nz = n.z;
  }

  // reverse winding order for DX
  // The icosphere meshgen produces CCW faces, which are culled by
  // D3DCULL_CCW. Swapping the indices after normal computation makes them CW
  // (visible from outside). We fix that here.
  for (size_t i = 0; i + 2 < outIdxs.size(); i += 3) {
    std::swap(outIdxs[i + 1], outIdxs[i + 2]);
  }
}

void RockScatter::CreateRockMeshes() {
  const RockScatterCfg &cfg = m_planet->RockCfg;

  if (!cfg.sMeshPrefix.empty()) {
    WIN32_FIND_DATAA fd;
    char searchPath[MAX_PATH];

    // absolute path to orbiter root dir
    char exePath[MAX_PATH];
    GetModuleFileNameA(NULL, exePath, MAX_PATH);
    // strip filename to get directory
    char *lastSlash = strrchr(exePath, '\\');
    if (lastSlash)
      *(lastSlash + 1) = '\0';

    sprintf_s(searchPath, sizeof(searchPath), "%sMeshes\\%s*.msh", exePath,
              cfg.sMeshPrefix.c_str());
    LogAlw("RockScatter: Mesh prefix = '%s'", cfg.sMeshPrefix.c_str());
    LogAlw("RockScatter: Searching for meshes with pattern '%s'", searchPath);

    std::vector<std::string> foundFiles;
    HANDLE hFind = FindFirstFileA(searchPath, &fd);
    if (hFind != INVALID_HANDLE_VALUE) {
      do {
        foundFiles.push_back(fd.cFileName);
      } while (FindNextFileA(hFind, &fd));
      FindClose(hFind);
    }

    // sort to guarantee determinism
    std::sort(foundFiles.begin(), foundFiles.end());

    std::string dirPath = "";
    size_t slashPos = cfg.sMeshPrefix.find_last_of("\\/");
    if (slashPos != std::string::npos) {
      dirPath = cfg.sMeshPrefix.substr(0, slashPos + 1);
    }

    for (const auto &fname : foundFiles) {
      std::string base = fname;
      if (base.length() > 4)
        base.erase(base.length() - 4);

      std::string lowerBase = base;
      std::transform(lowerBase.begin(), lowerBase.end(), lowerBase.begin(),
                     ::tolower);

      std::string relativePath = dirPath + base;
      MESHHANDLE hMesh = oapiLoadMeshGlobal(relativePath.c_str());
      if (hMesh) {
        // bottom extent
        float minY = 0.0f;
        DWORD nGrp = oapiMeshGroupCount(hMesh);
        for (DWORD g = 0; g < nGrp; g++) {
          MESHGROUPEX *grp = oapiMeshGroupEx(hMesh, g);
          if (grp && grp->Vtx) {
            for (DWORD v = 0; v < grp->nVtx; v++) {
              if (grp->Vtx[v].y < minY)
                minY = grp->Vtx[v].y;
            }
          }
        }
        float bottomExtent = -minY; // positive value = distance below origin

        // Build collision geometry from all mesh groups
        CollisionGeom cg;
        cg.maxRadius = 0.0f;
        for (DWORD cgi = 0; cgi < nGrp; cgi++) {
          MESHGROUPEX *cgrp = oapiMeshGroupEx(hMesh, cgi);
          if (cgrp && cgrp->Vtx && cgrp->Idx) {
            for (DWORD t = 0; t + 2 < cgrp->nIdx; t += 3) {
              CollisionGeom::Tri tri;
              WORD i0 = cgrp->Idx[t], i1 = cgrp->Idx[t + 1],
                   i2 = cgrp->Idx[t + 2];
              tri.v0 = D3DXVECTOR3(cgrp->Vtx[i0].x, cgrp->Vtx[i0].y,
                                   cgrp->Vtx[i0].z);
              tri.v1 = D3DXVECTOR3(cgrp->Vtx[i1].x, cgrp->Vtx[i1].y,
                                   cgrp->Vtx[i1].z);
              tri.v2 = D3DXVECTOR3(cgrp->Vtx[i2].x, cgrp->Vtx[i2].y,
                                   cgrp->Vtx[i2].z);
              cg.tris.push_back(tri);
              float r0 = D3DXVec3Length(&tri.v0);
              float r1 = D3DXVec3Length(&tri.v1);
              float r2 = D3DXVec3Length(&tri.v2);
              if (r0 > cg.maxRadius)
                cg.maxRadius = r0;
              if (r1 > cg.maxRadius)
                cg.maxRadius = r1;
              if (r2 > cg.maxRadius)
                cg.maxRadius = r2;
            }
          }
        }

        D3D9Mesh *d9m = new D3D9Mesh(hMesh, true);
        if (d9m) {
          bool isSmall = (lowerBase.find("small") != std::string::npos);
          bool isMedium = (lowerBase.find("medium") != std::string::npos);
          bool isLarge = (lowerBase.find("large") != std::string::npos);

          if (!isSmall && !isMedium && !isLarge) {
            m_meshPool[0].push_back(d9m);
            m_meshBottomExtent[0].push_back(bottomExtent);
            m_collGeom[0].push_back(cg);
            m_meshPool[1].push_back(d9m);
            m_meshBottomExtent[1].push_back(bottomExtent);
            m_collGeom[1].push_back(cg);
            m_meshPool[2].push_back(d9m);
            m_meshBottomExtent[2].push_back(bottomExtent);
            m_collGeom[2].push_back(cg);
          } else {
            if (isSmall) {
              m_meshPool[0].push_back(d9m);
              m_meshBottomExtent[0].push_back(bottomExtent);
              m_collGeom[0].push_back(cg);
            }
            if (isMedium) {
              m_meshPool[1].push_back(d9m);
              m_meshBottomExtent[1].push_back(bottomExtent);
              m_collGeom[1].push_back(cg);
            }
            if (isLarge) {
              m_meshPool[2].push_back(d9m);
              m_meshBottomExtent[2].push_back(bottomExtent);
              m_collGeom[2].push_back(cg);
            }
          }
          LogAlw("RockScatter: Mesh '%s' bottomExtent=%.2f", fname.c_str(),
                 bottomExtent);
        }
      }
    }

    if (!m_meshPool[0].empty() || !m_meshPool[1].empty() ||
        !m_meshPool[2].empty()) {
      // we have custom meshes

      // If any pool is completely empty due to naming, copy from another pool,
      // else we crash
      for (int i = 0; i < 3; i++) {
        if (m_meshPool[i].empty()) {
          for (int j = 0; j < 3; j++) {
            if (!m_meshPool[j].empty()) {
              m_meshPool[i] = m_meshPool[j];
              m_meshBottomExtent[i] = m_meshBottomExtent[j];
              m_collGeom[i] = m_collGeom[j];
              break;
            }
          }
        }
      }
      LogAlw("RockScatter: Loaded %u custom meshes with prefix '%s'",
             foundFiles.size(), cfg.sMeshPrefix.c_str());
      LogAlw(
          "RockScatter: m_meshPool sizes -> Small: %u, Medium: %u, Large: %u",
          m_meshPool[0].size(), m_meshPool[1].size(), m_meshPool[2].size());
      return; // Skip procedural generation
    }
  }

  // Small (icosahedron, 0 subdiv), Medium (1 subdiv), Large (2 subdiv)
  int subdiv[] = {0, 1, 2};
  float scales[] = {0.5f, 1.0f, 1.0f};

  // Default material
  MATERIAL mtrl;
  memset(&mtrl, 0, sizeof(mtrl));
  mtrl.diffuse = {0.45f, 0.42f, 0.40f, 1.0f};
  mtrl.ambient = {0.15f, 0.14f, 0.13f, 1.0f};
  mtrl.specular = {0.05f, 0.05f, 0.05f, 1.0f};
  mtrl.emissive = {0.0f, 0.0f, 0.0f, 1.0f};
  mtrl.power = 5.0f;

  for (int i = 0; i < 3; i++) {
    std::vector<NTVERTEX> verts;
    std::vector<WORD> idxs;
    CreateIcosphereMesh(subdiv[i], m_seed + (i + 1) * 7919u, scales[i], verts,
                        idxs);

    if (verts.empty() || idxs.empty())
      continue;

    MESHGROUPEX grpex;
    memset(&grpex, 0, sizeof(grpex));
    grpex.Vtx = verts.data();
    grpex.Idx = idxs.data();
    grpex.nVtx = (DWORD)verts.size();
    grpex.nIdx = (DWORD)idxs.size();
    grpex.MtrlIdx = 0;
    grpex.TexIdx = 0;
    grpex.Flags = 0;
    grpex.UsrFlag = 0;
    grpex.zBias = 0;

    D3D9Mesh *procMesh = new D3D9Mesh(&grpex, &mtrl, NULL);
    m_meshPool[i].push_back(procMesh);

    CollisionGeom cg;
    cg.maxRadius = 0.0f;
    for (DWORD t = 0; t + 2 < (DWORD)idxs.size(); t += 3) {
      CollisionGeom::Tri tri;
      tri.v0 =
          D3DXVECTOR3(verts[idxs[t]].x, verts[idxs[t]].y, verts[idxs[t]].z);
      tri.v1 = D3DXVECTOR3(verts[idxs[t + 1]].x, verts[idxs[t + 1]].y,
                           verts[idxs[t + 1]].z);
      tri.v2 = D3DXVECTOR3(verts[idxs[t + 2]].x, verts[idxs[t + 2]].y,
                           verts[idxs[t + 2]].z);
      cg.tris.push_back(tri);
      float r0 = D3DXVec3Length(&tri.v0);
      float r1 = D3DXVec3Length(&tri.v1);
      float r2 = D3DXVec3Length(&tri.v2);
      if (r0 > cg.maxRadius)
        cg.maxRadius = r0;
      if (r1 > cg.maxRadius)
        cg.maxRadius = r1;
      if (r2 > cg.maxRadius)
        cg.maxRadius = r2;
    }
    m_collGeom[i].push_back(cg);
  }
}

RockScatter::RockScatter(vPlanet *planet, LPDIRECT3DDEVICE9 pDev)
    : m_planet(planet), m_pDev(pDev), m_seed(0),
      m_lastDensityMult(Config->fRockDensityMult) {
  const char *name = planet->GetName();
  uint32_t h = 5381u;
  if (name)
    for (const char *p = name; *p; p++)
      h = h * 33u + (uint32_t)*p;
  h ^= planet->RockCfg.uSeed;
  m_seed = h ? h : 1u;

  CreateRockMeshes();
  LoadBaseClearZones();
  LogAlw("RockScatter: Initialised for '%s' (seed=%u, drawDist=%.0f m, "
         "density=%.4f, clearZones=%u)",
         name ? name : "?", m_seed, planet->RockCfg.fDrawDist,
         planet->RockCfg.fDensity, (unsigned)m_clearZones.size());
}

RockScatter::~RockScatter() {
  std::vector<D3D9Mesh *> deleted;
  for (int i = 0; i < 3; i++) {
    for (auto mesh : m_meshPool[i]) {
      if (std::find(deleted.begin(), deleted.end(), mesh) == deleted.end()) {
        delete mesh;
        deleted.push_back(mesh);
      }
    }
  }
  {
    std::lock_guard<std::mutex> lock(m_cacheMutex);
    m_cache.clear();
  }
}

// Load clear zones from base config files

void RockScatter::LoadBaseClearZones() {
  m_clearZones.clear();

  OBJHANDLE hPlanet = m_planet->Object();
  if (!hPlanet)
    return;

  DWORD nBases = oapiGetBaseCount(hPlanet);
  if (nBases == 0)
    return;

  double planetRad = m_planet->GetSize();

  for (DWORD b = 0; b < nBases; b++) {
    OBJHANDLE hBase = oapiGetBaseByIndex(hPlanet, b);
    if (!hBase)
      continue;

    // Get base position in radians
    double baseLng = 0.0, baseLat = 0.0;
    oapiGetBaseEquPos(hBase, &baseLng, &baseLat);

    // Open base config file
    const char *cfgFile = oapiGetObjectFileName(hBase);
    if (!cfgFile || !cfgFile[0])
      continue;

    std::ifstream fs(cfgFile);
    if (fs.fail())
      continue;

    int zonesFound = 0;
    std::string line;

    while (std::getline(fs, line) && zonesFound < MAX_CLEAR_AREAS_PER_BASE) {
      // Look for AREA_TO_CLEAR_N = X, Y
      size_t pos = line.find("AREA_TO_CLEAR_");
      if (pos == std::string::npos)
        continue;

      // Find the '=' sign
      size_t eq = line.find('=', pos);
      if (eq == std::string::npos)
        continue;

      // Parse the two values after '='
      std::string values = line.substr(eq + 1);
      // Replace commas with spaces for easier parsing
      for (char &c : values) {
        if (c == ',')
          c = ' ';
      }

      float x = 0.0f, y = 0.0f;
      std::istringstream iss(values);
      if (!(iss >> x >> y))
        continue;

      // Both extents must be positive and non-zero
      if (x <= 0.0f || y <= 0.0f)
        continue;

      ClearZone cz;
      cz.baseLng = baseLng;
      cz.baseLat = baseLat;
      cz.halfExtX = x;
      cz.halfExtY = y;
      m_clearZones.push_back(cz);
      zonesFound++;

      char baseName[64];
      oapiGetObjectName(hBase, baseName, 64);
      LogAlw("RockScatter: Base '%s' clear zone #%d: halfExtX=%.1f m, "
             "halfExtY=%.1f m",
             baseName, zonesFound, x, y);
    }
  }

  if (!m_clearZones.empty()) {
    LogAlw("RockScatter: Loaded %u total clear zones from %u bases on '%s'",
           (unsigned)m_clearZones.size(), nBases, m_planet->GetName());
  }
}

// Check if a surface position falls inside any base clear zone

bool RockScatter::IsInClearZone(double lng, double lat) const {
  if (m_clearZones.empty())
    return false;

  double planetRad = m_planet->GetSize();

  for (const auto &cz : m_clearZones) {
    // Quick angular rejection before expensive trig
    // Max extent in radians (generous upper bound)
    double maxArc = (double)max(cz.halfExtX, cz.halfExtY) / planetRad * 1.5;

    double dLat = lat - cz.baseLat;
    if (fabs(dLat) > maxArc)
      continue;

    double dLng = lng - cz.baseLng;
    while (dLng > PI)
      dLng -= 2.0 * PI;
    while (dLng < -PI)
      dLng += 2.0 * PI;
    if (fabs(dLng) > maxArc)
      continue;

    // Convert angular deltas to surface metres
    double dx = dLng * cos(lat) * planetRad; // east-west (metres)
    double dy = dLat * planetRad;            // north-south (metres)

    // Rectangle test: [-halfExtX, -halfExtY] to [+halfExtX, +halfExtY]
    if (fabs(dx) <= (double)cz.halfExtX && fabs(dy) <= (double)cz.halfExtY) {
      return true;
    }
  }
  return false;
}

// Deterministic rock generation

const std::vector<RockScatter::RockInstance> &
RockScatter::GetRocksForTile(int lvl, int ilat, int ilng) const {
  TileKey key = {lvl, ilat, ilng};
  std::lock_guard<std::mutex> lock(m_cacheMutex);
  auto it = m_cache.find(key);
  if (it != m_cache.end())
    return it->second;

  auto &rocks = m_cache[key];
  const RockScatterCfg &cfg = m_planet->RockCfg;

  // Compute tile bounds
  double tileSize = PI / double(1 << lvl);
  double latMin = PI * 0.5 - tileSize * (ilat + 1);
  double latMax = PI * 0.5 - tileSize * ilat;
  double lngMin = -PI + tileSize * ilng;
  double lngMax = lngMin + tileSize;

  // Approximate tile area in m² at this level
  double planetRad = m_planet->GetSize();
  double midLat = (latMin + latMax) * 0.5;
  double areaM2 = (tileSize * planetRad) * (tileSize * cos(midLat) * planetRad);
  if (areaM2 < 1.0)
    return rocks;

  // Quality-adjusted density
  if (Config->bRockEnable == 0 || Config->fRockDensityMult <= 0.0f)
    return rocks;
  float density = cfg.fDensity * Config->fRockDensityMult;

  int nRocks = (int)(density * areaM2);
  // cap per tile to keep memory sane
  if (nRocks > 2000)
    nRocks = 2000;
  if (nRocks <= 0)
    return rocks;

  uint32_t rng = HashTile(m_seed, lvl, ilat, ilng);
  rocks.reserve(nRocks);

  for (int i = 0; i < nRocks; i++) {
    RockInstance rock;

    // Random position within tile (lat/lng)
    double lat = latMin + RandFloat(rng) * (latMax - latMin);
    double lng = lngMin + RandFloat(rng) * (lngMax - lngMin);

    // Unit sphere position
    double clat = cos(lat), slat = sin(lat);
    double clng = cos(lng), slng = sin(lng);
    rock.localPos =
        D3DXVECTOR3(float(clat * clng), float(slat), float(clat * slng));

    // Elevation – query the actual terrain elevation
    double elev = oapiSurfaceElevation(m_planet->Object(), lng, lat);
    rock.elevation = (float)elev;

    // Size class based on configured ratios
    float r = RandFloat(rng);
    if (r < cfg.fRatioSmall) {
      rock.sizeClass = 0;
      rock.scale = RandRange(rng, cfg.fSizeSmall[0], cfg.fSizeSmall[1]);
    } else if (r < cfg.fRatioSmall + cfg.fRatioMedium) {
      rock.sizeClass = 1;
      rock.scale = RandRange(rng, cfg.fSizeMedium[0], cfg.fSizeMedium[1]);
    } else {
      rock.sizeClass = 2;
      rock.scale = RandRange(rng, cfg.fSizeLarge[0], cfg.fSizeLarge[1]);
    }

    // Select a deterministic mesh from the corresponding pool
    int poolSize = m_meshPool[rock.sizeClass].size();
    rock.meshIndex = (uint8_t)(RandFloat(rng) * poolSize);
    if (rock.meshIndex >= poolSize)
      rock.meshIndex = poolSize - 1;

    // Random Y-rotation
    rock.rotY = RandFloat(rng) * 6.283185f;

    // clear zone filtering
    // All RNG values have already been consumed above, so skipping
    // this rock does NOT alter the deterministic sequence for
    // subsequent rocks in this tile.
    if (!m_clearZones.empty()) {
      double rockLat = asin(rock.localPos.y);
      double rockLng = atan2(rock.localPos.z, rock.localPos.x);
      if (IsInClearZone(rockLng, rockLat))
        continue;
    }

    rocks.push_back(rock);
  }

  return rocks;
}

// Frame

void RockScatter::Render(LPDIRECT3DDEVICE9 pDev) {
  if (Config->bRockEnable == 0)
    return;
  if (m_meshPool[0].empty() && m_meshPool[1].empty() && m_meshPool[2].empty())
    return;

  // Invalidate cache dynamically if density multiplier changed
  if (m_lastDensityMult != Config->fRockDensityMult) {
    std::lock_guard<std::mutex> lock(m_cacheMutex);
    m_cache.clear();
    m_lastDensityMult = Config->fRockDensityMult;
  }

  const RockScatterCfg &cfg = m_planet->RockCfg;
  const Scene *scn = m_planet->GetScene();
  if (!scn)
    return;

  // Only render when this planet is the camera proxy
  if (scn->GetCameraProxyVisual() != m_planet)
    return;

  float activeDrawDist = cfg.fDrawDist * Config->fRockDistMult;
  float fov = (float)scn->GetCameraAperture();

  // Camera and vessel positions
  VECTOR3 camGlob = scn->GetCameraGPos();
  VECTOR3 planGlob = m_planet->GlobalPos();
  VECTOR3 camRel = camGlob - planGlob;

  OBJHANDLE hFocus = oapiGetFocusObject();
  VECTOR3 vesselRel = camRel;
  if (hFocus) {
    VECTOR3 vesselGlob;
    oapiGetGlobalPos(hFocus, &vesselGlob);
    vesselRel = vesselGlob - planGlob;
  }

  // Rotate into planet frame
  MATRIX3 grot;
  oapiGetRotationMatrix(m_planet->Object(), &grot);
  MATRIX3 igrot = {grot.m11, grot.m21, grot.m31, grot.m12, grot.m22,
                   grot.m32, grot.m13, grot.m23, grot.m33};
  VECTOR3 vesselLocal = mul(igrot, vesselRel);

  double planetRad = m_planet->GetSize();
  double vesselAlt = length(vesselLocal) - planetRad;
  if (vesselAlt > activeDrawDist * 3.0f)
    return;

  // If the camera is extremely far away, stop rendering completely. Otherwise
  // we crash.
  double camAlt = length(camRel) - planetRad;
  if (camAlt > activeDrawDist * 10.0f)
    return;

  double vLen = length(vesselLocal);
  double y_norm = vLen > 0.0 ? (vesselLocal.y / vLen) : 0.0;
  if (y_norm > 1.0)
    y_norm = 1.0;
  if (y_norm < -1.0)
    y_norm = -1.0;

  double vesselLat = asin(y_norm);
  double vesselLng = atan2(vesselLocal.z, vesselLocal.x);

  // get tile level
  double tgtTileSize = cfg.fDrawDist * 2.0 / planetRad;
  int lvl = 1;
  while ((PI / double(1 << lvl)) > tgtTileSize && lvl < 15)
    lvl++;
  if (lvl < 4)
    lvl = 4;

  double tileSize = PI / double(1 << lvl);
  int vesselIlat = (int)((PI * 0.5 - vesselLat) / tileSize);
  int vesselIlng = (int)((vesselLng + PI) / tileSize);
  int nLngBands = 1 << (lvl + 1);
  int searchR = max(2, min(25, (int)ceil(Config->fRockDistMult * 0.5)));

  D3D9Sun sunParams = m_planet->GetObjectAtmoParams(camRel);
  float drawDist2 = activeDrawDist * activeDrawDist;

  // per-frame constants
  D3DXVECTOR3 vesselLocalDX((float)vesselLocal.x, (float)vesselLocal.y,
                            (float)vesselLocal.z);

  // Camera forward vector for frustum culling (planet-local frame)
  VECTOR3 camLocalV = mul(igrot, camRel);
  double camLen = length(camLocalV);
  D3DXVECTOR3 camFwd;
  if (camLen > 1.0) {
    // Camera looks toward planet center from orbit, but for surface-level
    // the camera forward is roughly -(camera position normalized)
    // We use the actual view direction from the oapi
    VECTOR3 camDir;
    oapiCameraGlobalDir(&camDir);
    VECTOR3 camDirLocal = mul(igrot, camDir);
    camFwd = D3DXVECTOR3((float)camDirLocal.x, (float)camDirLocal.y,
                         (float)camDirLocal.z);
    D3DXVec3Normalize(&camFwd, &camFwd);
  } else {
    camFwd = D3DXVECTOR3(0, 1, 0);
  }

  // Set cull mode ONCE before the entire rock loop
  m_pDev->SetRenderState(D3DRS_CULLMODE, D3DCULL_NONE);

  // Collect and batch rocks by (sizeClass, meshIndex)
  //  We use a simple approach: iterate all mesh variants, for each one
  //  set up the batch, then iterate tiles and draw matching rocks.
  //  This minimises state changes to one Begin/End per mesh variant.

  struct BatchKey {
    uint8_t sizeClass;
    uint8_t meshIndex;
  };

  // Build a flat list of all unique mesh variants in use
  // (at most 3 size classes × N meshes per class, typically < 10 total)
  struct MeshBatch {
    uint8_t sizeClass;
    uint8_t meshIndex;
    D3D9Mesh *mesh;
  };

  std::vector<MeshBatch> batches;
  batches.reserve(16);
  for (int sc = 0; sc < 3; sc++) {
    for (uint8_t mi = 0; mi < (uint8_t)m_meshPool[sc].size(); mi++) {
      MeshBatch b;
      b.sizeClass = (uint8_t)sc;
      b.meshIndex = mi;
      b.mesh = m_meshPool[sc][mi];
      batches.push_back(b);
    }
  }

  int renderedRocks = 0;

  for (const auto &batch : batches) {
    if (!batch.mesh)
      continue;

    // Begin batch for this mesh variant — sets up ALL shader state once
    batch.mesh->RenderBatchBegin(&sunParams);

    for (int dlat = -searchR; dlat <= searchR; dlat++) {
      int tilat = vesselIlat + dlat;
      if (tilat < 0 || tilat >= (1 << lvl))
        continue;

      for (int dlng = -searchR; dlng <= searchR; dlng++) {
        int tilng = (vesselIlng + dlng) % nLngBands;
        if (tilng < 0)
          tilng += nLngBands;

        const auto &rocks = GetRocksForTile(lvl, tilat, tilng);

        for (const auto &rock : rocks) {
          // Skip rocks not belonging to this batch
          if (rock.sizeClass != batch.sizeClass ||
              rock.meshIndex != batch.meshIndex)
            continue;

          // World position
          float bottomOfs = 0.0f;
          if (rock.meshIndex < m_meshBottomExtent[rock.sizeClass].size())
            bottomOfs = m_meshBottomExtent[rock.sizeClass][rock.meshIndex];
          D3DXVECTOR3 rockGeo =
              rock.localPos *
              (float)(planetRad + rock.elevation + bottomOfs * rock.scale);

          // Distance from vessel (not camera) for culling
          D3DXVECTOR3 diff = rockGeo - vesselLocalDX;
          float dist2 = D3DXVec3LengthSq(&diff);

          // LOD sub-cull by size class
          float lodCull = GetLodCull(rock.sizeClass);
          float classDist2 = drawDist2 * lodCull * lodCull;
          if (dist2 > classDist2)
            continue;

          // skip rocks behind camera
          D3DXVECTOR3 rockCamLocal =
              rockGeo - D3DXVECTOR3((float)camLocalV.x, (float)camLocalV.y,
                                    (float)camLocalV.z);
          float dotFwd = D3DXVec3Dot(&rockCamLocal, &camFwd);
          // Skip rocks more than 10m behind the camera (generous margin)
          if (dotFwd < -10.0f)
            continue;

          // culling
          float cdist = D3DXVec3Length(&rockCamLocal);
          if (cdist > activeDrawDist * 2.5f)
            continue;

          float scr_size = (rock.scale * 5.0f / max(1.0f, cdist)) / fov *
                           1080.0f; // assuming 5m base mesh size
          if (scr_size < 1.0f)
            continue;

          // Build world matrix — localPos is already unit-length, skip Norm3
          D3DXVECTOR3 up = rock.localPos;
          D3DXVECTOR3 right, fwd;

          if (fabsf(up.y) < 0.99f)
            right = D3DXVECTOR3(-up.z, 0, up.x);
          else
            right = D3DXVECTOR3(1, 0, 0);
          // Normalize right (was implicitly done by Norm3 before but whatever)
          D3DXVec3Normalize(&right, &right);
          D3DXVec3Cross(&fwd, &up, &right);

          // Apply Y rotation
          float cy = cosf(rock.rotY), sy = sinf(rock.rotY);
          D3DXVECTOR3 rr = right * cy + fwd * sy;
          D3DXVECTOR3 ff = fwd * cy - right * sy;

          // Rotate axes into world frame
          VECTOR3 wRight = mul(grot, _V(rr.x, rr.y, rr.z));
          VECTOR3 wUp = mul(grot, _V(up.x, up.y, up.z));
          VECTOR3 wFwd = mul(grot, _V(ff.x, ff.y, ff.z));

          // Camera-relative position, double precision
          VECTOR3 rGeo = _V(rockGeo.x, rockGeo.y, rockGeo.z);
          VECTOR3 rGeoRot = mul(grot, rGeo);
          VECTOR3 rockCam = rGeoRot - camRel;

          float s = rock.scale;
          D3DXMATRIX mWorld(
              (float)wRight.x * s, (float)wRight.y * s, (float)wRight.z * s, 0,
              (float)wUp.x * s, (float)wUp.y * s, (float)wUp.z * s, 0,
              (float)wFwd.x * s, (float)wFwd.y * s, (float)wFwd.z * s, 0,
              (float)rockCam.x, (float)rockCam.y, (float)rockCam.z, 1);

          if (std::isnan(mWorld._41) || std::isnan(mWorld._42) ||
              std::isnan(mWorld._43) || std::isinf(mWorld._41) ||
              std::isinf(mWorld._42) || std::isinf(mWorld._43))
            continue;

          // Only update world matrix + commit + draw
          batch.mesh->RenderBatchInstance(&mWorld);
          renderedRocks++;
        }
      }
    }

    batch.mesh->RenderBatchEnd();
  }
}

void RockScatter::RenderShadows(LPDIRECT3DDEVICE9 pDev, float alpha) {
  if (Config->bRockEnable == 0)
    return;
  if (m_meshPool[0].empty() && m_meshPool[1].empty() && m_meshPool[2].empty())
    return;

  const RockScatterCfg &cfg = m_planet->RockCfg;
  const Scene *scn = m_planet->GetScene();
  if (!scn)
    return;

  if (scn->GetCameraProxyVisual() != m_planet)
    return;

  float activeDrawDist = cfg.fDrawDist * Config->fRockDistMult;
  float fov = (float)scn->GetCameraAperture();

  VECTOR3 camGlob = scn->GetCameraGPos();
  VECTOR3 planGlob = m_planet->GlobalPos();
  VECTOR3 camRel = camGlob - planGlob;

  OBJHANDLE hFocus = oapiGetFocusObject();
  VECTOR3 vesselRel = camRel;
  if (hFocus) {
    VECTOR3 vesselGlob;
    oapiGetGlobalPos(hFocus, &vesselGlob);
    vesselRel = vesselGlob - planGlob;
  }

  MATRIX3 grot;
  oapiGetRotationMatrix(m_planet->Object(), &grot);
  MATRIX3 igrot = {grot.m11, grot.m21, grot.m31, grot.m12, grot.m22,
                   grot.m32, grot.m13, grot.m23, grot.m33};
  VECTOR3 vesselLocal = mul(igrot, vesselRel);

  VECTOR3 sd;
  oapiGetGlobalPos(m_planet->Object(), &sd);
  normalise(sd);
  D3DXVECTOR3 lsun = D3DXVEC(tmul(grot, sd));

  double planetRad = m_planet->GetSize();
  double vesselAlt = length(vesselLocal) - planetRad;
  if (vesselAlt > activeDrawDist * 3.0f)
    return;

  VECTOR3 camLocalV = mul(igrot, camRel);
  double camAlt = length(camLocalV) - planetRad;
  if (camAlt > activeDrawDist * 20.0f)
    return;

  double vLen = length(vesselLocal);
  double y_norm = vLen > 0.0 ? (vesselLocal.y / vLen) : 0.0;
  if (y_norm > 1.0)
    y_norm = 1.0;
  if (y_norm < -1.0)
    y_norm = -1.0;

  double vesselLat = asin(y_norm);
  double vesselLng = atan2(vesselLocal.z, vesselLocal.x);

  double tgtTileSize = cfg.fDrawDist * 2.0 / planetRad;
  int lvl = 1;
  while ((PI / double(1 << lvl)) > tgtTileSize && lvl < 15)
    lvl++;
  if (lvl < 4)
    lvl = 4;
  double tileSize = PI / double(1 << lvl);

  int vesselIlat = (int)((PI * 0.5 - vesselLat) / tileSize);
  int vesselIlng = (int)((vesselLng + PI) / tileSize);
  int nLngBands = 1 << (lvl + 1);
  int searchR = max(2, min(25, (int)ceil(Config->fRockDistMult * 0.5)));

  float drawDist2 = activeDrawDist * activeDrawDist;
  D3DXVECTOR4 param = D9OffsetRange(planetRad, 30e3);

  // Hoisted per-frame constant
  D3DXVECTOR3 vesselLocalDX((float)vesselLocal.x, (float)vesselLocal.y,
                            (float)vesselLocal.z);

  pDev->SetRenderState(D3DRS_CULLMODE, D3DCULL_NONE);

  // batch by mesh variant
  struct MeshBatch {
    uint8_t sizeClass;
    uint8_t meshIndex;
    D3D9Mesh *mesh;
  };

  std::vector<MeshBatch> batches;
  batches.reserve(16);
  for (int sc = 0; sc < 3; sc++) {
    for (uint8_t mi = 0; mi < (uint8_t)m_meshPool[sc].size(); mi++) {
      MeshBatch b;
      b.sizeClass = (uint8_t)sc;
      b.meshIndex = mi;
      b.mesh = m_meshPool[sc][mi];
      batches.push_back(b);
    }
  }

  int renderedRocks = 0;

  for (const auto &batch : batches) {
    if (!batch.mesh)
      continue;

    batch.mesh->RenderShadowBatchBegin(&param);

    for (int dlat = -searchR; dlat <= searchR; dlat++) {
      int tilat = vesselIlat + dlat;
      if (tilat < 0 || tilat >= (1 << lvl))
        continue;

      for (int dlng = -searchR; dlng <= searchR; dlng++) {
        int tilng = (vesselIlng + dlng) % nLngBands;
        if (tilng < 0)
          tilng += nLngBands;
        const auto &rocks = GetRocksForTile(lvl, tilat, tilng);

        for (const auto &rock : rocks) {
          if (rock.sizeClass != batch.sizeClass ||
              rock.meshIndex != batch.meshIndex)
            continue;

          float bottomOfs = 0.0f;
          if (rock.meshIndex < m_meshBottomExtent[rock.sizeClass].size())
            bottomOfs = m_meshBottomExtent[rock.sizeClass][rock.meshIndex];

          float rElev =
              (float)(planetRad + rock.elevation + bottomOfs * rock.scale);
          D3DXVECTOR3 rockGeo = rock.localPos * rElev;

          // Distance cull (hoisted vesselLocalDX)
          D3DXVECTOR3 diff = rockGeo - vesselLocalDX;
          if (D3DXVec3LengthSq(&diff) > drawDist2 * GetLodCull(rock.sizeClass) *
                                            GetLodCull(rock.sizeClass))
            continue;

          // localPos is already unit-length — skip Norm3
          D3DXVECTOR3 up = rock.localPos;

          float nd = D3DXVec3Dot(&up, &lsun);
          if (nd > -0.01f)
            continue;

          // sub-pixel and max distance culling for shadows
          D3DXVECTOR3 rockCamLocal =
              rockGeo - D3DXVECTOR3((float)camLocalV.x, (float)camLocalV.y,
                                    (float)camLocalV.z);
          float cdist = D3DXVec3Length(&rockCamLocal);
          if (cdist > activeDrawDist * 2.5f)
            continue;

          float scr_size =
              (rock.scale * 5.0f / max(1.0f, cdist)) / fov * 1080.0f;
          if (scr_size < 1.0f)
            continue;

          D3DXVECTOR3 right, fwd;
          if (fabsf(up.y) < 0.99f)
            right = D3DXVECTOR3(-up.z, 0, up.x);
          else
            right = D3DXVECTOR3(1, 0, 0);
          D3DXVec3Normalize(&right, &right);
          D3DXVec3Cross(&fwd, &up, &right);

          float cy = cosf(rock.rotY), sy = sinf(rock.rotY);
          D3DXVECTOR3 rr = right * cy + fwd * sy;
          D3DXVECTOR3 ff = fwd * cy - right * sy;

          VECTOR3 wRight = mul(grot, _V(rr.x, rr.y, rr.z));
          VECTOR3 wUp = mul(grot, _V(up.x, up.y, up.z));
          VECTOR3 wFwd = mul(grot, _V(ff.x, ff.y, ff.z));

          VECTOR3 rGeo = _V(rockGeo.x, rockGeo.y, rockGeo.z);
          VECTOR3 rGeoRot = mul(grot, rGeo);
          VECTOR3 rockCam = rGeoRot - camRel;

          float s = rock.scale;
          D3DXMATRIX mWorld(
              (float)wRight.x * s, (float)wRight.y * s, (float)wRight.z * s, 0,
              (float)wUp.x * s, (float)wUp.y * s, (float)wUp.z * s, 0,
              (float)wFwd.x * s, (float)wFwd.y * s, (float)wFwd.z * s, 0,
              (float)rockCam.x, (float)rockCam.y, (float)rockCam.z, 1);

          if (std::isnan(mWorld._41) || std::isnan(mWorld._42) ||
              std::isnan(mWorld._43) || std::isinf(mWorld._41) ||
              std::isinf(mWorld._42) || std::isinf(mWorld._43))
            continue;

          D3DXVECTOR3 rockLsun;
          rockLsun.x = D3DXVec3Dot(&lsun, &rr);
          rockLsun.y = nd;
          rockLsun.z = D3DXVec3Dot(&lsun, &ff);

          float zo = bottomOfs;
          float ofs = zo / nd;

          D3DXMATRIX mProj;
          D3DXMatrixIdentity(&mProj);
          mProj._21 = -rockLsun.x / nd;
          mProj._22 = 0.0f;
          mProj._23 = -rockLsun.z / nd;
          mProj._41 = -rockLsun.x * ofs;
          mProj._42 = -bottomOfs;
          mProj._43 = -rockLsun.z * ofs;

          D3DXVECTOR4 nrml(0, 0, 0, 1.0f);

          float scale = (-nd - 0.07f) * 25.0f;
          scale = (1.0f - alpha) * max(0.0f, min(1.0f, scale));

          batch.mesh->RenderShadowBatchInstance(scale, &mProj, &mWorld, &nrml,
                                                &param);
          renderedRocks++;
        }
      }
    }

    batch.mesh->RenderShadowBatchEnd();
  }
}

// Per-triangle downward raycast through collision mesh
// Returns the highest Y coordinate at (localX, localZ), or -1e30 if miss

float RockScatter::RaycastMeshY(const CollisionGeom &geom, float lx, float lz) {
  float bestY = -1e30f;
  for (const auto &tri : geom.tris) {
    // 2D point-in-triangle test on XZ plane using barycentric coordinates
    float ax = tri.v0.x, az = tri.v0.z;
    float bx = tri.v1.x, bz = tri.v1.z;
    float cx = tri.v2.x, cz = tri.v2.z;

    float d00 = (bx - ax) * (bx - ax) + (bz - az) * (bz - az);
    float d01 = (bx - ax) * (cx - ax) + (bz - az) * (cz - az);
    float d11 = (cx - ax) * (cx - ax) + (cz - az) * (cz - az);
    float d20 = (lx - ax) * (bx - ax) + (lz - az) * (bz - az);
    float d21 = (lx - ax) * (cx - ax) + (lz - az) * (cz - az);
    float denom = d00 * d11 - d01 * d01;
    if (fabs(denom) < 1e-12f)
      continue;

    float v = (d11 * d20 - d01 * d21) / denom;
    float w = (d00 * d21 - d01 * d20) / denom;
    float u = 1.0f - v - w;

    if (u >= -0.01f && v >= -0.01f && w >= -0.01f) {
      // Interpolate Y at this XZ position
      float y = u * tri.v0.y + v * tri.v1.y + w * tri.v2.y;
      if (y > bestY)
        bestY = y;
    }
  }
  return bestY;
}

double RockScatter::GetElevationModifier(double lng, double lat) const {
  if (!Config->bRockCollision || Config->bRockEnable == 0)
    return 0.0;
  const RockScatterCfg &cfg = m_planet->RockCfg;
  if (!cfg.bEnabled)
    return 0.0;

  double planetRad = m_planet->GetSize();

  // Use the same tile level computation as the renderer
  double tgtTileSize = cfg.fDrawDist * 2.0 / planetRad;
  int lvl = 1;
  while ((PI / double(1 << lvl)) > tgtTileSize && lvl < 15)
    lvl++;
  if (lvl < 4)
    lvl = 4;

  double tileSize = PI / double(1 << lvl);
  int centerIlat = (int)((PI * 0.5 - lat) / tileSize);
  int centerIlng = (int)((lng + PI) / tileSize);
  int nLngBands = 1 << (lvl + 1);

  double maxAddedElev = 0.0;

  for (int dlat = -1; dlat <= 1; dlat++) {
    int tilat = centerIlat + dlat;
    if (tilat < 0 || tilat >= (1 << lvl))
      continue;

    for (int dlng = -1; dlng <= 1; dlng++) {
      int tilng = (centerIlng + dlng + nLngBands) % nLngBands;

      const std::vector<RockInstance> &rocks =
          GetRocksForTile(lvl, tilat, tilng);

      for (const auto &rock : rocks) {
        // Quick bounding sphere check first
        if (rock.meshIndex >= m_collGeom[rock.sizeClass].size())
          continue;
        const CollisionGeom &cg = m_collGeom[rock.sizeClass][rock.meshIndex];
        double boundingR = (double)rock.scale * cg.maxRadius;

        // Get the bottom extent since rendering shifts the mesh up by this
        // amount so the mesh bottom sits on the surface
        float bottomOfs = 0.0f;
        if (rock.meshIndex < m_meshBottomExtent[rock.sizeClass].size())
          bottomOfs = m_meshBottomExtent[rock.sizeClass][rock.meshIndex];

        double rockLat = asin(rock.localPos.y);
        double rockLng = atan2(rock.localPos.z, rock.localPos.x);

        double dLat = lat - rockLat;
        double dLng = lng - rockLng;
        while (dLng > PI)
          dLng -= 2.0 * PI;
        while (dLng < -PI)
          dLng += 2.0 * PI;

        // Surface distance from rock center
        double dx_m = dLng * cos(lat) * planetRad;
        double dy_m = dLat * planetRad;
        double dist2 = dx_m * dx_m + dy_m * dy_m;

        if (dist2 > boundingR * boundingR)
          continue;

        // Transform query point into rock's local mesh space:
        // 1) Un-rotate by rock.rotY
        // 2) Un-scale by rock.scale
        float cosR = cosf(-rock.rotY);
        float sinR = sinf(-rock.rotY);
        float localX = (float)((dx_m * cosR + dy_m * sinR) / rock.scale);
        float localZ = (float)((-dx_m * sinR + dy_m * cosR) / rock.scale);

        // Raycast downward through mesh triangles at this XZ
        float meshY = RaycastMeshY(cg, localX, localZ);
        if (meshY > -1e20f) {
          // This shifts the mesh up by bottomOfs so the bottom
          // sits on the surface. Ground level in mesh space = -bottomOfs.
          // Visible height above ground = meshY - (-bottomOfs) = meshY +
          // bottomOfs
          double heightAboveGround =
              ((double)meshY + (double)bottomOfs) * rock.scale;
          if (heightAboveGround > 0.0 && heightAboveGround > maxAddedElev) {
            maxAddedElev = heightAboveGround;
          }
        }
      }
    }
  }

  return maxAddedElev;
}

// Mesh-to-mesh collisions

RockScatter::CollisionResult
RockScatter::CheckCollision(const VECTOR3 *hullPts, int nPts,
                            const VECTOR3 &vesselPosLocal, double vesselRadius,
                            float maxCollisionDist) const {
  CollisionResult result = {false, {0, 0, 0}, 0.0, {0, 0, 0}};

  if (!Config->bRockCollision || nPts <= 0 || !hullPts)
    return result;

  double planetRad = oapiGetSize(m_planet->Object());
  if (planetRad < 1.0)
    return result;

  // Vessel center lat/lng for tile lookup
  double vLen = sqrt(vesselPosLocal.x * vesselPosLocal.x +
                     vesselPosLocal.y * vesselPosLocal.y +
                     vesselPosLocal.z * vesselPosLocal.z);
  if (vLen < 1.0)
    return result;

  double vLat = asin(vesselPosLocal.y / vLen);
  double vLng = atan2(vesselPosLocal.z, vesselPosLocal.x);
  double vAlt = vLen - planetRad;
  if (vAlt > 500.0)
    return result;

  const RockScatterCfg &cfg = m_planet->RockCfg;
  if (!cfg.bEnabled)
    return result;

  // Use the same tile level as the renderer
  float drawDist = cfg.fDrawDist;
  int lvl = (int)(log2(PI / (drawDist / planetRad)));
  if (lvl < 1)
    lvl = 1;
  if (lvl > 19)
    lvl = 19;

  int nLat = 1 << lvl;
  int nLng = 2 * nLat;

  int centerIlat = (int)((PI05 - vLat) * nLat / PI);
  int centerIlng = (int)((vLng + PI) * nLng / PI2);
  if (centerIlat < 0)
    centerIlat = 0;
  if (centerIlat >= nLat)
    centerIlat = nLat - 1;
  centerIlng = ((centerIlng % nLng) + nLng) % nLng;

  // Maximum collision distance — only rocks closer than this are checked
  double maxCollDist2 = (double)maxCollisionDist * (double)maxCollisionDist;

  int searchR = 2;
  double deepestPen = 0.0;

  static int collLogCount = 0;
  bool doLog = (collLogCount % 300 == 0);

  int rocksChecked = 0;
  int rocksInRange = 0;

  for (int dLat = -searchR; dLat <= searchR; dLat++) {
    int tilat = centerIlat + dLat;
    if (tilat < 0 || tilat >= nLat)
      continue;
    for (int dLng = -searchR; dLng <= searchR; dLng++) {
      int tilng = ((centerIlng + dLng) % nLng + nLng) % nLng;

      const auto &rocks = GetRocksForTile(lvl, tilat, tilng);

      for (const auto &rock : rocks) {
        if (rock.meshIndex >= m_collGeom[rock.sizeClass].size())
          continue;
        const CollisionGeom &cg = m_collGeom[rock.sizeClass][rock.meshIndex];

        // Get bottom extent for vertical shift
        float bottomOfs = 0.0f;
        if (rock.meshIndex < m_meshBottomExtent[rock.sizeClass].size())
          bottomOfs = m_meshBottomExtent[rock.sizeClass][rock.meshIndex];

        // Rock world position (planet-local), shifted up by bottomOfs
        double rockAlt =
            planetRad + (double)rock.elevation + (double)bottomOfs * rock.scale;
        double rockWX = (double)rock.localPos.x * rockAlt;
        double rockWY = (double)rock.localPos.y * rockAlt;
        double rockWZ = (double)rock.localPos.z * rockAlt;

        // Distance from vessel center to rock center (double precision)
        double dx = vesselPosLocal.x - rockWX;
        double dy = vesselPosLocal.y - rockWY;
        double dz = vesselPosLocal.z - rockWZ;
        double dist2Center = dx * dx + dy * dy + dz * dz;

        // Only collide with rocks that are WITHIN the collision distance
        if (dist2Center > maxCollDist2)
          continue;
        rocksInRange++;

        double distCenter = sqrt(dist2Center);
        double rockBoundR = (double)rock.scale * cg.maxRadius;
        if (distCenter > vesselRadius + rockBoundR + 5.0)
          continue;
        rocksChecked++;

        // Build rock's local coordinate frame
        D3DXVECTOR3 up = Norm3(rock.localPos);
        D3DXVECTOR3 right, fwd;
        if (fabsf(up.y) < 0.99f)
          right = Norm3(D3DXVECTOR3(-up.z, 0, up.x));
        else
          right = D3DXVECTOR3(1, 0, 0);
        D3DXVec3Cross(&fwd, &up, &right);

        float cy = cosf(rock.rotY), sy = sinf(rock.rotY);
        D3DXVECTOR3 rr = right * cy + fwd * sy;
        D3DXVECTOR3 ff = fwd * cy - right * sy;

        float invScale = 1.0f / rock.scale;

        // Test each hull vertex
        for (int p = 0; p < nPts; p++) {
          // Vector from rock center to hull point
          double hx = hullPts[p].x - rockWX;
          double hy = hullPts[p].y - rockWY;
          double hz = hullPts[p].z - rockWZ;

          // Quick sphere rejection per-point
          double ptDist2 = hx * hx + hy * hy + hz * hz;
          if (ptDist2 > rockBoundR * rockBoundR)
            continue;

          // Transform to rock mesh-local coordinates
          // Dot products with rock axes
          float localX = (float)(hx * (double)rr.x + hy * (double)rr.y +
                                 hz * (double)rr.z) *
                         invScale;
          float localY = (float)(hx * (double)up.x + hy * (double)up.y +
                                 hz * (double)up.z) *
                         invScale;
          float localZ = (float)(hx * (double)ff.x + hy * (double)ff.y +
                                 hz * (double)ff.z) *
                         invScale;

          // Raycast downward through mesh triangles at this XZ
          float meshY = RaycastMeshY(cg, localX, localZ);
          if (meshY <= -1e20f)
            continue; // outside mesh footprint

          // Point must be above mesh bottom
          if (localY < -bottomOfs)
            continue;

          if (localY < meshY) {
            // Point is inside the rock mesh, so penetration = meshY - localY
            double pen = ((double)meshY - (double)localY) * rock.scale;
            if (pen > deepestPen) {
              deepestPen = pen;
              result.hit = true;

              // Store the actual hull contact point in planet-local frame
              result.contactPtLocal = hullPts[p];

              // Push-out normal: direction from rock center to vessel CoM.
              // This always pushes the vessel AWAY from the rock regardless
              // of impact angle. Using the rock's up vector caused "climbing"
              // because side impacts were pushed upward instead of back. This
              // isn't great but it's all I've got.
              double toVLen = distCenter;
              if (toVLen > 1e-6) {
                result.normal.x = dx / toVLen;
                result.normal.y = dy / toVLen;
                result.normal.z = dz / toVLen;
              } else {
                result.normal.x = (double)up.x;
                result.normal.y = (double)up.y;
                result.normal.z = (double)up.z;
              }

              // Depth: use the minimum of mesh penetration and the
              // bounding sphere overlap along the push-out direction.
              // The mesh depth (meshY - localY) is measured along the
              // up-axis and can be huge for side impacts. The sphere
              // overlap gives a physically reasonable push-out distance
              // for the rock-center-to-vessel direction.
              double sphereOverlap = rockBoundR - sqrt(ptDist2);
              if (sphereOverlap > 0)
                result.depth = min(pen, sphereOverlap);
              else
                result.depth = min(pen, 0.01); // just barely inside
            }
          }
        }
      }
    }
  }

  if (doLog || result.hit) {
    collLogCount++;
    LogAlw("RockCollision::CheckCollision: nPts=%d rocksInRange=%d "
           "rocksChecked=%d hit=%d depth=%.4f",
           nPts, rocksInRange, rocksChecked, result.hit ? 1 : 0, result.depth);
  } else {
    collLogCount++;
  }

  return result;
}
