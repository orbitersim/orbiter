//
// Tentacle Precompiled Header
// (c) Microsoft 1999
//

#pragma once
#ifndef __TENTACLE_PCH_HPP
#define __TENTACLE_PCH_HPP

#include <d3d.h>
#include <d3dx.h>
#include <mmsystem.h>
#include <stdio.h>

#include "resource.h"
#include "ctentacle.hpp"
#include "clensflare.hpp"
#include "cframetimer.hpp"


#define RELEASE(obj) if(obj) { obj->Release(); obj = NULL; } else 0

#endif // __TENTACLE_PCH_HPP