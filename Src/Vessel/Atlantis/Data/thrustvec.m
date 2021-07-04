% This script calculates the moments induced by the thrusters for
% the shuttle model for two configurations:
%
% 1. Launch (orbiter+ET+SRBs)
% 2. Post SRB-separation (orbiter+ET)
%
% The idea is to set up thrust directions for given thrust position so
% that moments are minimised

clear all
close all
format long

% SSME thrust reference positions in orbiter frame
ref_ssme0 = [-1.55,-0.37,-12.5];
ref_ssme1 = [ 1.55,-0.37,-12.5];
ref_ssme2 = [ 0.0,  2.7, -12.5];
% SRB thrust reference position in SRB frame
ref_srb   = [ 0.0,  0.0, -20.4];
    
%% Calculate the thrust moments for launch configuration

% Component offsets from global CG
ofs_orbiter=[0.0, 6.07,-7.895];
ofs_srb_left=[-6.2,-1.91,-5.68];
ofs_srb_right=[6.2,-1.91,-5.68];

% Global thrust positions
p_ssme0 = ofs_orbiter+ref_ssme0;
p_ssme1 = ofs_orbiter+ref_ssme1;
p_ssme2 = ofs_orbiter+ref_ssme2;
p_srb_left  = ofs_srb_left+ref_srb;
p_srb_right = ofs_srb_right+ref_srb;

phi_srb = 1.36*pi/180; % assumed srb thrust tilt
d_srb_left = [0,sin(phi_srb),cos(phi_srb)];
d_srb_right = [0,sin(phi_srb),cos(phi_srb)];

phi01 = 0;                      % assumed thrust tilt for lower SSMEs
phi2  = 5.2517704038977*pi/180; % assumed thrust tilt for upper SSME
dx = 0.0696517;            % assumed x-divergence for lower SSMEs
scl = sqrt(1-dx^2);
d_ssme0 = [ dx,-scl*sin(phi01),scl*cos(phi01)];
d_ssme1 = [-dx,-scl*sin(phi01),scl*cos(phi01)];
d_ssme2 = [ 0.0,-sin(phi2),cos(phi2)];

th_srb = 1202020.0*9.81 * 1.0;  % single SRB thrust
th_ssme = 1668652.0 * 1.0;      % single SSME thrust

mom_srb_left = cross(p_srb_left, d_srb_left) * th_srb;
mom_srb_right = cross(p_srb_right, d_srb_right) * th_srb;
mom_ssme0 = cross(p_ssme0, d_ssme0) * th_ssme;
mom_ssme1 = cross(p_ssme1, d_ssme1) * th_ssme;
mom_ssme2 = cross(p_ssme2, d_ssme2) * th_ssme;

mom_srb = mom_srb_left + mom_srb_right;
mom_ssme = mom_ssme0 + mom_ssme1 + mom_ssme2;
mom_tot = mom_srb + mom_ssme;

mom_tot

%% Calculate the thrust moments for orbiter+tank configuration

% Component offsets from global CG
ofs_orbiter_withtank = [ 0.0, 4.64,-9.285];

% thrust reference positions for orbiter+tank configuration
p_ssme0 = ofs_orbiter_withtank + ref_ssme0;
p_ssme1 = ofs_orbiter_withtank + ref_ssme1;
p_ssme2 = ofs_orbiter_withtank + ref_ssme2;

% thrust directions for orbiter+tank configuration
% Note: these all go through the CG, so individually don't induce a moment
d_ssme0 = [ 0.0696517, -0.1918793,  0.9789438];
d_ssme1 = [-0.0696517, -0.1918793,  0.9789438];
d_ssme2 = [ 0,         -0.3192929,  0.9476561];

mom_ssme0 = cross(p_ssme0, d_ssme0);
mom_ssme1 = cross(p_ssme1, d_ssme1);
mom_ssme2 = cross(p_ssme2, d_ssme2);

mom_ssme = mom_ssme0 + mom_ssme1 + mom_ssme2;
mom_tot = mom_ssme;

mom_tot

%% Calculate the thrust moments for orbiter OMS

p_omsl = [-2.6,  3.3, -12.8];
p_omsr = [ 2.6,  3.3, -12.8];

d_omsl = -p_omsl/norm(p_omsl);
d_omsr = -p_omsr/norm(p_omsr);

mom_omsl = cross(p_omsl, d_omsl);
mom_omsr = cross(p_omsr, d_omsr);
mom_oms = mom_omsl + mom_omsr;
mom_tot = mom_oms;

mom_tot