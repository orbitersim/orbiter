/*
**----------------------------------------------------------------------------
**
**  File:       flock.cpp
**  Purpose:    code for the boid flocking
**  Notes:
**
**	Copyright (C) 1995-1999 Microsoft Corporation. All Rights Reserved.
**----------------------------------------------------------------------------
*/

#include "Common.h"
#include "Debug.h"
#include "d3dutils.h"
#include "boids.h"
#include "input.h"
#include "stdio.h"
#include "music.h"

const float InfluenceRadius = 10.0f;	// outside of this range forces are considered to be 0

const float CollisionFraction = 0.8f;
const float InvCollisionFraction = 1.0f/(1.0f-CollisionFraction);

const float NormalSpeed = 0.1f;
const float	AngleTweak = 0.02f;
const float PitchToSpeedRatio = 0.002f;

// more arbitray constants that look cool
const float	separation_scale	= 0.05f;
const float	alignment_scale		= 0.1f;
const float	cohesion_scale		= 1.0f;
const float	migratory_scale		= 0.4f;
const float	obstacle_scale		= 1.0f;

extern BoidMusic	g_Music;

void	
UpdateFlock (Flock flock)
{
	int		i, j;
	float	dist;
	static int lastobj = -1;

	// first update the dist array 0.0..1.0 with 0.0 being furthest away
	for (i=0; i<flock.num_boids; i++) {
		for (j=i+1; j<flock.num_boids; j++) {
			dist = Magnitude(flock.boids[i].loc - flock.boids[j].loc);
			flock.dist[i][j] = flock.dist[j][i] = dist;
		}
		flock.dist[i][i] = 0.0f;

		// init boid forces
		flock.boids[i].separation_force = D3DVECTOR(0.0f);
		flock.boids[i].alignment_force = D3DVECTOR(0.0f);
		flock.boids[i].cohesion_force = D3DVECTOR(0.0f);
		flock.boids[i].migratory_force = D3DVECTOR(0.0f);
		flock.boids[i].obstacle_force = D3DVECTOR(0.0f);
		flock.boids[i].num_neighbors = 0;
	}

	// for each boid calculate the individual forces affecting it

	for (i=0; i<flock.num_boids; i++) {
		// add in effects from other boids
		for (j=i+1; j<flock.num_boids; j++) {
			// if i is near j have them influence each other
			if (flock.dist[i][j] < InfluenceRadius) {
				D3DVECTOR	diff = (flock.boids[i].loc - flock.boids[j].loc)/flock.dist[i][j];
				
				// sum seperation force
				flock.boids[i].separation_force += diff/flock.dist[i][j];
				flock.boids[j].separation_force -= diff/flock.dist[i][j];

				// sum alignment force (actually summing the directions of the neighbors)
				flock.boids[i].alignment_force += flock.boids[j].dir/flock.dist[i][j];
				flock.boids[j].alignment_force += flock.boids[i].dir/flock.dist[i][j];
				
				// sum cohesion force (actually we're summing neighbor locations)				
				flock.boids[i].cohesion_force += flock.boids[j].loc;
				flock.boids[j].cohesion_force += flock.boids[i].loc;

				flock.boids[i].num_neighbors++;
				flock.boids[j].num_neighbors++;
			}
		}

		// add in any obstacle forces
		for (j=0; j<flock.num_obs; j++) {
			D3DVECTOR	ob = flock.boids[i].loc - flock.obs[j].loc;
			float		radius = flock.obs[j].radius;

			// ignore object if already past
			if (DotProduct(ob, flock.boids[i].dir) > 0.0f)
				continue;

			dist = Magnitude(ob) - radius;

			if (dist < InfluenceRadius) {
				if ((lastobj != j) && (dist < 5.0f))
				{
					lastobj = j;
					char string[100];
					sprintf(string,"About to hit sphere %d, Distance is %f, Influence Radius is %f\n",j,dist,InfluenceRadius);
					OutputDebugString(string);
					g_Music.Transition();
				}
				ob /= dist;	// normalize
				dist -= radius;
				if (dist < 0.01f) {
					dist = 0.01f;
				}
				flock.boids[i].obstacle_force += ob/dist;
			}
		}

		D3DVECTOR	diff;
		float		mag;

		// find cohesion force
		if (flock.boids[i].num_neighbors != 0) {
			flock.boids[i].cohesion_force /= (float)flock.boids[i].num_neighbors;	// find average location of neighbors
			diff = flock.boids[i].cohesion_force - flock.boids[i].loc;	// find delta to center of flock
			mag = Magnitude(diff);
			if (mag > 0.0f) {
				flock.boids[i].cohesion_force = diff/mag;	// normalized
			} else {
				flock.boids[i].cohesion_force = D3DVECTOR(0.0f);
			}
		}

		// find the alignment force
		if (flock.boids[i].num_neighbors != 0) {
			flock.boids[i].alignment_force /= (float)flock.boids[i].num_neighbors;
			mag = Magnitude(flock.boids[i].alignment_force);
			if (mag > 0.0f) {
				flock.boids[i].alignment_force /= mag;	// normalize
				diff = flock.boids[i].alignment_force - flock.boids[i].dir;
				flock.boids[i].alignment_force = diff/mag;
			} 
		}

		// finally, the migratory force
		flock.boids[i].migratory_force = Normalize(flock.goal-flock.boids[i].loc);
	}

	// update the boids
	for (i=0; i<flock.num_boids; i++) {
		D3DVECTOR	force(0.0f);		// final force affecting the boid

		// note that the order the forces are added does have an effect since
		// the summed magnitude is limited to 1.0

		if (!keys.obstacle) {
			// add in obstacle force
			force += flock.boids[i].obstacle_force;
		}
		
		if (!keys.separation) {
			// add in separation force
			force += flock.boids[i].separation_force;
		}
		
		if (!keys.alignment) {
			// add in alignment force
			force += flock.boids[i].alignment_force * alignment_scale;
		}

		if (!keys.cohesion) {
			// add in cohesion force
			force += flock.boids[i].cohesion_force;
		}

		if (!keys.migratory) {
			// add in migratory force
			force += flock.boids[i].migratory_force;
		}

		// Ok, now we have a final force to apply to the boid.
		// Normalize it if too big.
		float	mag = Magnitude(force);
		if (mag > 1.0f) {
			force /= mag;
		}

		// first deal with pitch changes
		if (force.y > 0.01) {			// we're too low
			flock.boids[i].pitch += AngleTweak;
			if (flock.boids[i].pitch > 0.8f)
				flock.boids[i].pitch = 0.8f;
		} else if (force.y < -0.01) {	// we're too high
			flock.boids[i].pitch -= AngleTweak;
			if (flock.boids[i].pitch < -0.8f)
				flock.boids[i].pitch = -0.8f;
		} else {
			// add damping
			flock.boids[i].pitch *= 0.98f;
		}

		// speed up or slow down depending on angle of attack
		flock.boids[i].speed -= flock.boids[i].pitch * PitchToSpeedRatio;
		// damp back to normal
		flock.boids[i].speed = (flock.boids[i].speed-NormalSpeed)*0.99f + NormalSpeed;

		// limit speed changes to +- 50% from normal
		if (flock.boids[i].speed < NormalSpeed/2) {
			flock.boids[i].speed = NormalSpeed/2;
		}
		if (flock.boids[i].speed > NormalSpeed*5) {
			flock.boids[i].speed = NormalSpeed*5;
		}

		// now figure out yaw changes
		D3DVECTOR offset = force;
		offset.y = 0.0f;
		D3DVECTOR delta = flock.boids[i].dir;

		if (Magnitude(offset) > 0.0f) {
			offset = Normalize(offset);
		}
		float	dot = DotProduct(offset, delta);
		// speed up slightly if not turning much
		if (dot > 0.7f) {
			dot -= 0.7f;
			flock.boids[i].speed += dot * 0.005f;
		}
		offset = CrossProduct(offset, delta);
		dot = (1.0f-dot)/2.0f * 0.07f;
		if (offset.y > 0.05f) {
			flock.boids[i].dyaw = (flock.boids[i].dyaw*19.0f + dot) * 0.05f;
		} else if (offset.y < -0.05f) {
			flock.boids[i].dyaw = (flock.boids[i].dyaw*19.0f - dot) * 0.05f;
		} else {
			flock.boids[i].dyaw *= 0.98f;	// damp it
		}
		flock.boids[i].yaw += flock.boids[i].dyaw;
		flock.boids[i].roll = -flock.boids[i].dyaw * 20.0f;

		// take new info and create a new world matrix
		// first translate into place, then set orientation, then scale (if needed)
		flock.boids[i].world = TranslateMatrix(flock.boids[i].loc);
		flock.boids[i].world = MatrixMult(flock.boids[i].world, MatrixMult(MatrixMult(RotateYMatrix(flock.boids[i].yaw), 
								RotateXMatrix(flock.boids[i].pitch)), RotateZMatrix(flock.boids[i].roll)));

		// now extract the boid's direction out of the matrix
		flock.boids[i].dir[0] = flock.boids[i].world(2, 0);
		flock.boids[i].dir[1] = flock.boids[i].world(2, 1);
		flock.boids[i].dir[2] = flock.boids[i].world(2, 2);

		// and update the boid's location
		flock.boids[i].loc += flock.boids[i].dir * flock.boids[i].speed;
	}	// end of loop for each boid

}	// end of UpdateFlock()


/*
**-----------------------------------------------------------------------------
** End of File
**-----------------------------------------------------------------------------
*/


