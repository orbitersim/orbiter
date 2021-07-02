% Return mesh vertices and indices for DeltaGlider HSI (compass)
% instrument, either for 2D panel (target='2D') or virtual cockpit
% (target='VC')
% prm contains the transformation parameters to the desired location

function [vtx,idx] = hsi_disp(target,prm)

% texture layout
tex.w =  512;                      % texture width [px]
tex.h = 1024;                      % texture height [px]
tex.hsi.x0 = tex.w-312;
tex.hsi.y0 = tex.h-252;
tex.rose.x = tex.hsi.x0+2;
tex.rose.w = 152;
tex.rose.y = tex.hsi.y0+2;
tex.rose.h = tex.rose.w;
tex.arrow.y = tex.hsi.y0+172.5;
tex.arrow.h = 17;
tex.dev.y = tex.arrow.y+17.5;
tex.dev.h = 10;
tex.scale.x = tex.hsi.x0+12;
tex.scale.w = 82;
tex.scale.y = tex.hsi.y0+216;
tex.scale.h = 12;

ur0 = tex.rose.x/tex.w;
vr0 = tex.rose.y/tex.h;
ur1 = (tex.rose.x+tex.rose.w)/tex.w;
vr1 = (tex.rose.y+tex.rose.h)/tex.h;

ub0 = (tex.hsi.x0+0.5)/tex.w;
ub1 = (tex.hsi.x0+155.5)/tex.w;
vb0 = (tex.hsi.y0+171.5)/tex.h;
vb1 = (tex.hsi.y0+154.5)/tex.h;

ug0 = (tex.hsi.x0+94.5)/tex.w;
vg0 = (tex.hsi.y0+216.5)/tex.h;
ug1 = (tex.hsi.x0+107.5)/tex.w;
vg1 = (tex.hsi.y0+227.5)/tex.h;

us0 = (tex.hsi.x0+0.5)/tex.w;
vs0 = (tex.arrow.y+tex.arrow.h+18)/tex.h;
us1 = (tex.hsi.x0+18)/tex.w;
vs1 = (tex.arrow.y+tex.arrow.h+1)/tex.h;

ud0 = tex.scale.x/tex.w;
vd0 = tex.scale.y/tex.h;
ud1 = (tex.scale.x+tex.scale.w)/tex.w;
vd1 = (tex.scale.y+tex.scale.h)/tex.h;

uc0 = tex.rose.x/tex.w;
vc0 = (tex.arrow.y+tex.arrow.h)/tex.h;
uc1 = (tex.rose.x+tex.rose.w)/tex.w;
vc1 = tex.arrow.y/tex.h;

ui0 = (tex.hsi.x0+91)/tex.w;
vi0 = tex.dev.y/tex.h;
ui1 = (tex.hsi.x0+91+64)/tex.w;
vi1 = (tex.dev.y+tex.dev.h)/tex.h;

vt0 = (tex.hsi.y0+231.5)/tex.h;
vt1 = (tex.hsi.y0+248)/tex.h;

up0 = (tex.w-483)/tex.w;           % cover plate tex coords
up1 = (tex.w-313)/tex.w;
vp0 = (tex.h-164)/tex.h;
vp1 = (tex.h-334)/tex.h;

z0 = -0.0005;
z1 = -0.001;
z2 = -0.0015;

nml = [0,0,-1];

vtx = [-60.5, 60.5,0,  nml, ur0,vr0; ... % compass rose
        60.5, 60.5,0,  nml, ur1,vr0; ...
       -60.5,-60.5,0,  nml, ur0,vr1; ...
        60.5,-60.5,0,  nml, ur1,vr1; ...
       -57  , 72  ,0,  nml, 0,0; ...     % course readout background
       -36  , 72  ,0,  nml, 0,0; ...
       -57  , 60  ,0,  nml, 0,0; ...
       -36  , 60  ,0,  nml, 0,0; ...
        62  , 60  ,z0, nml, ub0,vb0; ... % glideslope background
        76.5, 60  ,z0, nml, ub0,vb1; ...
        62  ,-60  ,z0, nml, ub1,vb0; ...
        76.5,-60  ,z0, nml, ub1,vb1; ...
        -6.2, 61  ,z0, nml, us0,vs0; ... % source bearing indicator
         6.2, 61  ,z0, nml, us0,vs1; ...
        -6.2, 45  ,z0, nml, us1,vs0; ...
         6.2, 45  ,z0, nml, us1,vs1; ...
       -32.2,  4.7,z0, nml, ud0,vd0; ... % deviation scale
        32.2,  4.7,z0, nml, ud1,vd0; ...
       -32.2, -4.7,z0, nml, ud0,vd1; ...
        32.2, -4.7,z0, nml, ud1,vd1; ...
        -6.2, 60.5,z1, nml, uc0,vc0; ... % course indicator
         6.2, 60.5,z1, nml, uc0,vc1; ...
        -6.2,-60.5,z1, nml, uc1,vc0; ...
         6.2,-60.5,z1, nml, uc1,vc1; ...
       -3.65,26.82,z1, nml, ui0,vi0; ... % deviation indicator
        3.65,26.82,z1, nml, ui0,vi1; ...
       -3.65,-26.82,z1,nml, ui1,vi0; ...
        3.65,-26.82,z1,nml, ui1,vi1; ...
        64  ,  4  ,z1, nml, ug0,vg0; ... % glideslope indicator
        76.5,  4  ,z1, nml, ug1,vg0; ...
        64  , -4  ,z1, nml, ug0,vg1; ...
        76.5, -4  ,z1, nml, ug1,vg1; ...
       -57  , 72  ,z0, nml, 0,vt0; ...   % course readout
       -50  , 72  ,z0, nml, 0,vt0; ...
       -57  , 60  ,z0, nml, 0,vt1; ...
       -50  , 60  ,z0, nml, 0,vt1; ...
       -50  , 72  ,z0, nml, 0,vt0; ...
       -43  , 72  ,z0, nml, 0,vt0; ...
       -50  , 60  ,z0, nml, 0,vt1; ...
       -43  , 60  ,z0, nml, 0,vt1; ...
       -43  , 72  ,z0, nml, 0,vt0; ...
       -36  , 72  ,z0, nml, 0,vt0; ...
       -43  , 60  ,z0, nml, 0,vt1; ...
       -36  , 60  ,z0, nml, 0,vt1; ...
       -85  ,-85  ,z2, nml, up0,vp0; ...
        85  ,-85  ,z2, nml, up1,vp0; ...
       -85  , 85  ,z2, nml, up0,vp1; ...
        85  , 85  ,z2, nml, up1,vp1];

idx = [ 0, 1, 2;  3, 2, 1; ...          % compass rose
        4, 5, 6;  7, 6, 5; ...          % course readout background
        8, 9,10; 11,10, 9; ...          % glideslope background
       12,13,14; 15,14,13; ...          % source bearing indicator
       16,17,18; 19,18,17; ...          % deviation scale
   	   24,25,26; 27,26,25; ...          % deviation indicator
       20,21,22; 23,22,21; ...          % course indicator
	   28,29,30; 31,30,29; ...          % glideslope indicator
	   32,33,34; 35,34,33; ...          % course readout
	   36,37,38; 39,38,37; ...
	   40,41,42; 43,42,41; ...
       44,46,45; 47,45,46];             % cover plate

if nargin >= 1 && strcmp(target,'VC')
    disp('Computing HSI instrument for VC');
    vtx = transform_vtx_vc(vtx,prm);    % map vertices
else
    disp('Computing HSI instrument for 2D panel');
    vtx = vtx(1:end-4,:);               % remove cover plate
    vtx = transform_vtx_2d(vtx,prm);    % map vertices
    idx = idx(1:end-2,:);               % remove cover plate
end

end

% ------------------------------------------------------------------------
% Map into virtual cockpit position
function vtx = transform_vtx_vc(vtx,prm)

rad = prm.rad;
%tilt = prm.tilt;  % assume tilt around x-axis
%cnt = prm.cnt;

scale = rad/108;
%cosa = cos(tilt);
%sina = sin(tilt);

% transform vertex positions
vtx(:,1) = vtx(:,1)*scale;
vtx(:,2) = vtx(:,2)*scale;
%x = vtx(:,1);
%y = vtx(:,2)*cosa - vtx(:,3)*sina;
%z = vtx(:,2)*sina + vtx(:,3)*cosa;
%vtx(:,1) = x + cnt.x;
%vtx(:,2) = y + cnt.y;
%vtx(:,3) = z + cnt.z;

% transform vertex normals
%ny = vtx(:,5)*cosa - vtx(:,6)*sina;
%nz = vtx(:,5)*sina + vtx(:,6)*cosa;
%vtx(:,5) = ny;
%vtx(:,6) = nz;

end

% ------------------------------------------------------------------------
% Map into 2D panel position
function vtx = transform_vtx_2d(vtx,prm)

% coords of horizon instrument centre in panel
xcnt = 641; % prm.cnt.x;
ycnt = 473; % prm.cnt.y;

% shift position
vtx(:,1) = xcnt+vtx(:,1);
vtx(:,2) = ycnt-vtx(:,2);

% flatten out z-coords
vtx(:,3) = 0;

end