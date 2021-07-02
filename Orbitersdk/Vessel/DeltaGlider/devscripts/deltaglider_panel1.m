% Create the mesh for the Deltaglider 2D main panel

function deltaglider_panel1

disp('**** Generating DG cockpit mesh for 2D overhead panel');

% global parameters
global g;
g.tex0.w = 2048; % texture 0 width (main panel texture)
g.tex0.h = 1024; % texture 0 height (main panel texture)
g.tex1.w =  512; % texture 1 width (instrument texture)
g.tex1.h = 1024; % texture 1 height (instrument texture)
g.nml = [0,0,-1];

%path = 'c:\source\orbiter\meshes\DG\';
path = 'C:\Source\Orbiter\Meshes\DG\';
fid = fopen([path 'deltaglider_p1_part.msh'],'wt');

switch1.w  =   26;
switch1.h  =   52;
switch1.tu0 =  53;
switch1.tv0 =  337;

% Main panel parameters
panel0.w = 1280;
panel0.h = 283;
panel0.x0 = 0;
panel0.w1 = 255;
panel0.w2 = 425;
panel0.h1 = 136;
panel0.y0 = 572;
panel0.tx0 = 0;
panel0.ty0 = g.tex0.h-panel0.y0-panel0.h;

% Parameters for hatch switch
hatch_switch = switch1;
hatch_switch.x0 = 434;
hatch_switch.y0 = 192;

% Parameters for inner airlock switch
ilock_switch = switch1;
ilock_switch.x0 = 480;
ilock_switch.y0 = 192;

% Parameters for outer airlock switch
olock_switch = switch1;
olock_switch.x0 = 526;
olock_switch.y0 = 192;

% Parameters for dock/landing light switch
dlight_switch = switch1;
dlight_switch.x0 = 708;
dlight_switch.y0 = 192;

% Parameters for strobe/beacon light switch
slight_switch = switch1;
slight_switch.x0 = 754;
slight_switch.y0 = 192;

% Parameters for nav light switch
nlight_switch = switch1;
nlight_switch.x0 = 800;
nlight_switch.y0 = 192;

%Parameters for radiator switch
rad_switch = switch1;
rad_switch.x0 = 846;
rad_switch.y0 = 192;

% Parameters for pressure valve switches
valve_switch = switch1;
valve_switch.x = 388 + [0:4]*46;
valve_switch.y0 = 42;

% Parameters for coolant pump switch
pump_switch = switch1;
pump_switch.x0 = 708;
pump_switch.y0 = 11;

% Generate instruments below main panel ----------------------------------

% main panel -------------------------------------------------------------

[vtx,idx] = make_mainpanel(panel0);
write_group (fid, vtx,idx,0,1,'MAINPANEL');

% Generate instruments above main panel ----------------------------------

vtx = [];
idx = [];

[mvtx,midx] = make_switch1(hatch_switch);
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);

[mvtx,midx] = make_switch1(ilock_switch);
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);

[mvtx,midx] = make_switch1(olock_switch);
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);

[mvtx,midx] = make_switch1(dlight_switch);
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);

[mvtx,midx] = make_switch1(slight_switch);
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);

[mvtx,midx] = make_switch1(nlight_switch);
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);

for i=1:5
    valve_switch.x0 = valve_switch.x(i);
    [mvtx,midx] = make_switch1(valve_switch);
    [vtx,idx] = add_to_group(vtx,idx,mvtx,midx);
end

[mvtx,midx] = make_switch1(rad_switch);
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);

[mvtx,midx] = make_switch1(pump_switch);
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);

% Write group of instruments rendered above main panel
write_group (fid, vtx,idx,0,1,'INSTRUMENTS_ABOVE');

fclose(fid);

end

% =========================================================================
% main panel background
function [vtx,idx] = make_mainpanel(prm)
    disp('Computing main panel for 2D panel');
    global g;
    vtx = [0,           0,     0, g.nml, prm.tx0/g.tex0.w,               prm.ty0/g.tex0.h; ...
           prm.w,       0,     0, g.nml, (prm.tx0+prm.w)/g.tex0.w,       prm.ty0/g.tex0.h; ...
           0,           prm.h1,0, g.nml, prm.tx0/g.tex0.w,               (prm.ty0+prm.h1)/g.tex0.h; ...
           prm.w,       prm.h1,0, g.nml, (prm.tx0+prm.w)/g.tex0.w,       (prm.ty0+prm.h1)/g.tex0.h; ...
           prm.w1,      prm.h1,0, g.nml, (prm.tx0+prm.w1)/g.tex0.w,      (prm.ty0+prm.h1)/g.tex0.h; ...
           prm.w-prm.w1,prm.h1,0, g.nml, (prm.tx0+prm.w-prm.w1)/g.tex0.w,(prm.ty0+prm.h1)/g.tex0.h; ...
           prm.w2,      prm.h, 0, g.nml, (prm.tx0+prm.w2)/g.tex0.w,      (prm.ty0+prm.h)/g.tex0.h; ...
           prm.w-prm.w2,prm.h, 0, g.nml, (prm.tx0+prm.w-prm.w2)/g.tex0.w,(prm.ty0+prm.h)/g.tex0.h];
           
    idx = [0,1,2; 3,2,1; 4,5,6; 7,6,5];
end

% =========================================================================
% switch1
function [vtx,idx] = make_switch1(prm)
    global g;
    vtx = [prm.x0,      prm.y0,      0, g.nml, prm.tu0/g.tex0.w,        prm.tv0/g.tex0.h; ...
           prm.x0+prm.w,prm.y0,      0, g.nml, (prm.tu0+prm.w)/g.tex0.w,prm.tv0/g.tex0.h; ...
           prm.x0,      prm.y0+prm.h,0, g.nml, prm.tu0/g.tex0.w,        (prm.tv0+prm.h)/g.tex0.h; ...
           prm.x0+prm.w,prm.y0+prm.h,0, g.nml, (prm.tu0+prm.w)/g.tex0.w,(prm.tv0+prm.h)/g.tex0.h];
    idx = [0,1,2; 1,3,2];       
end
