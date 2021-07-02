% Return mesh vertices and indices for the DeltaGlider propellant status
% display, either for 2D panel (target='2D') or virtual cockpit
% (target='VC')
% prm contains the transformation parameters to the desired location

function [vtx,idx] = fuel_disp(target,prm)

% Parameters for propellant status display
tex.w =  512;                      % texture width [px]
tex.h = 1024;                      % texture height [px]

psd.tx0 = tex.w-423.5;
psd.ty0 = 187.5;
psd.tdx = 267;
psd.tdy = 167;
psd.fblock.w = 28;
psd.fblock.h = 86;
psd.fblock.x0 = 5.5+[0:2]*92;
psd.fblock.y0 = 51;
psd.green.tx = tex.w-157;
psd.green.ty = 1;

nml = [0,0,-1];
dx = -46*2;

vtx = [0,0,0,                nml, psd.tx0/tex.w, psd.ty0/tex.h; ...  % display background
       0,psd.tdy,0,          nml, psd.tx0/tex.w, (psd.ty0-psd.tdy)/tex.h; ...
       0,0,0,                nml, psd.tx0/tex.w, psd.ty0/tex.h; ...
       0,psd.tdy,0,          nml, psd.tx0/tex.w, (psd.ty0-psd.tdy)/tex.h; ...
       psd.tdx+dx,0,0,       nml,(psd.tx0+psd.tdx+dx)/tex.w, psd.ty0/tex.h; ...
       psd.tdx+dx,psd.tdy,0, nml,(psd.tx0+psd.tdx+dx)/tex.w, (psd.ty0-psd.tdy)/tex.h; ...
       psd.tdx,0,0,          nml, (psd.tx0+psd.tdx)/tex.w, psd.ty0/tex.h; ...
       psd.tdx,psd.tdy,0,    nml, (psd.tx0+psd.tdx)/tex.w, (psd.ty0-psd.tdy)/tex.h];
idx = [0,1,2; 3,2,1; 2,3,4; 5,4,3; 4,5,6; 7,6,5];

for i=1:3
    x0 = psd.fblock.x0(i);
    y0 = psd.fblock.y0;
    z0 = -0.0002;
    w  = psd.fblock.w;
    h  = psd.fblock.h;
    vtx = [vtx; ...
          x0,  y0,  z0, nml, psd.green.tx/tex.w,psd.green.ty/tex.h; ...
          x0+w,y0,  z0, nml, psd.green.tx/tex.w,psd.green.ty/tex.h; ...
          x0,  y0+h,z0, nml, psd.green.tx/tex.w,psd.green.ty/tex.h; ...
          x0+w,y0+h,z0, nml, psd.green.tx/tex.w,psd.green.ty/tex.h];
    idx = [idx; ...
           [0,3,1; 0,2,3]+4+i*4];
end    

if nargin >= 1 && strcmp(target,'VC')
    prm.scale.x = prm.w/psd.tdx;
    prm.scale.y = prm.h/psd.tdy;
    disp('Computing propellant status display for VC');
    vtx = transform_vtx_vc(vtx,prm);    % map vertices
else
    disp('Computing propellant status display for 2D panel');
    vtx = transform_vtx_2d(vtx,prm);    % map vertices
end

end

% ------------------------------------------------------------------------
% Map into virtual cockpit position
function vtx = transform_vtx_vc(vtx,prm)
    vtx(:,1) = vtx(:,1)*prm.scale.x + prm.frame.border_lr;
    vtx(:,2) = vtx(:,2)*prm.scale.y - prm.frame.h + prm.frame.border_tb;
    vtx(:,3) = vtx(:,3);
end

% ------------------------------------------------------------------------
% Map into 2D panel position
function vtx = transform_vtx_2d(vtx,prm)

% shift position
vtx(:,1) = prm.x0+vtx(:,1);
vtx(:,2) = prm.y0-vtx(:,2);

% flatten out z-coords
vtx(:,3) = 0;

end