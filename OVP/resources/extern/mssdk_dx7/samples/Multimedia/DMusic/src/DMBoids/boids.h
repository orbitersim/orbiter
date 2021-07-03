/*
**-----------------------------------------------------------------------------
**  File:       boids.h
**  Purpose:    header file for boid flocking
**  Notes:
**
**  Copyright (c) 1995-1999 by Microsoft, all rights reserved
**-----------------------------------------------------------------------------
*/


#ifndef BOIDS_H
#define BOIDS_H

#include "Common.h"

typedef struct t_boid {
	D3DMATRIX	world;		// matrix representing the boids location/orientation
	D3DVECTOR	loc;		// location
	D3DVECTOR	dir;		// cur direction
	D3DVECTOR	separation_force;
	D3DVECTOR	alignment_force;
	D3DVECTOR	cohesion_force;
	D3DVECTOR	migratory_force;
	D3DVECTOR	obstacle_force;
	int			num_neighbors;

	D3DVECTOR	delta_pos;	// change in position from flock centering
	D3DVECTOR	delta_dir;	// change in direction
	int			delta_cnt;	// number of boids that influence this delta_dir
	float		speed;
	float		yaw, pitch, roll, dyaw;
	D3DVECTOR	color;
} Boid;

typedef struct t_obstacle {
	D3DVECTOR	loc;
	float		radius;
} Obstacle;

typedef struct t_flock {
	int			num_boids;
	Boid		*boids;
	int			num_obs;
	Obstacle	*obs;
	float		**dist;	// 2-d array of boid distances, yuk what a waste
	D3DVECTOR	goal;
} Flock;


// Boids Functions
void UpdateFlock (Flock flock);

#endif // BOIDS_H


