//
// Particle Precompiled Header
// (c) Microsoft 1999
//

#pragma once
#ifndef __PARTICLE_PCH_HPP
#define __PARTICLE_PCH_HPP

#include <d3d.h>
#include <d3dx.h>
#include <mmsystem.h>
#include <string.h>
#include <stdio.h>

#include "cframetimer.hpp"
#include "cparticle.hpp"
#include "cground.hpp"
#include "resource.h"


#define RELEASE(obj) if(obj) { obj->Release(); obj = NULL; } else 0

#endif // __PARTICLE_PCH_HPP