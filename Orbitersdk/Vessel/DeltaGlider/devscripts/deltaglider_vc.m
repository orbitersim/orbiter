function deltaglider_vc

RAD = pi/180.0;
global fhd;

% Global parameter structure
global g; % global parameters
g.ngroup = 0; % total number of groups written
g.ngroup_expected = 0; % number of groups as defined in file
g.nvtx = 0;        % total number of vertices written
g.texsize = 1024; % texture size
g.texsize_instr.w = 512;
g.texsize_instr.h = 1024;
g.panelstencil_ofs = -1e-4;
g.panelshadow_ofs = -1e-4;
g.shadow_ofs = -2e-4;
g.label_ofs = -3e-4;
g.defscale = 4.508e-4; % default scaling from pixels to mesh units for 1024 texture

root = '..\..\..\..\';
meshpath = [root 'meshes\DG\'];
srcpath = [root 'orbitersdk\samples\deltaglider\'];
fid = fopen([meshpath 'deltaglider_vc.msh'],'wt');
fhd = fopen([srcpath, 'dg_vc_anim.h'],'wt');
writeh_string(fhd, ['// Created by deltaglider_vc.m ' date '\n']);
writeh_string(fhd, '#ifndef __DG_VC_ANIM_H');
writeh_string(fhd, '#define __DG_VC_ANIM_H\n');

% Mesh groups in the order they should be written
ng = 0;
ng=ng+1; group(ng) = struct('name','VC4_DEFAULT', 'mat',6, 'tex',20);
ng=ng+1; group(ng) = struct('name','PANELS1', 'mat',15, 'tex',17);
ng=ng+1; group(ng) = struct('name','DIAL1', 'mat',6, 'tex',20);
ng=ng+1; group(ng) = struct('name','BUTTON2', 'mat',6, 'tex',20);
ng=ng+1; group(ng) = struct('name','BUTTON3', 'mat',28, 'tex',20);
ng=ng+1; group(ng) = struct('name','SWITCH1', 'mat',6, 'tex',20);
ng=ng+1; group(ng) = struct('name','SWITCH2', 'mat',6, 'tex',20);
ng=ng+1; group(ng) = struct('name','NOSECONE_LEVER', 'mat',6, 'tex',20);
ng=ng+1; group(ng) = struct('name','GEAR_LEVER', 'mat',6, 'tex',20);
ng=ng+1; group(ng) = struct('name','UNDOCK_LEVER', 'mat',6, 'tex',20);
ng=ng+1; group(ng) = struct('name','AIRBRAKE_LEVER', 'mat',6, 'tex',20);
ng=ng+1; group(ng) = struct('name','ETRIM_WHEEL', 'mat',6, 'tex',20);
ng=ng+1; group(ng) = struct('name','HUD_BRIGHTNESS', 'mat',6, 'tex',20);
ng=ng+1; group(ng) = struct('name','INSTR_BRIGHTNESS', 'mat',6, 'tex',20);
ng=ng+1; group(ng) = struct('name','FLOOD_BRIGHTNESS', 'mat',6, 'tex',20);
ng=ng+1; group(ng) = struct('name','COOLING_REFTEMP_DIAL', 'mat',6, 'tex',20);
ng=ng+1; group(ng) = struct('name','COOLING_PUMP_DIAL', 'mat',6, 'tex',20);
ng=ng+1; group(ng) = struct('name','ANGVEL_DISP', 'mat',18, 'tex',19);
ng=ng+1; group(ng) = struct('name','ANGVEL_DISP_OVR', 'mat',18, 'tex',19);
ng=ng+1; group(ng) = struct('name','PROPELLANT_STATUS', 'mat',19, 'tex',19);
ng=ng+1; group(ng) = struct('name','BLITTGT1', 'mat',18, 'tex',14);
ng=ng+1; group(ng) = struct('name','VC4_LIT', 'mat',29, 'tex',20);
ng=ng+1; group(ng) = struct('name','VC_INSTR', 'mat',19, 'tex',19);
ng=ng+1; group(ng) = struct('name','HSI', 'mat',19, 'tex',19);
ng=ng+1; group(ng) = struct('name','PANELSTENCIL', 'mat',6, 'tex',11);
ng=ng+1; group(ng) = struct('name','PANELSHADOW', 'mat',27, 'tex',11);
ng=ng+1; group(ng) = struct('name','SHADOW', 'mat',6, 'tex',11);
ng=ng+1; group(ng) = struct('name','LIT_LABELS', 'mat',12, 'tex',11);
ng=ng+1; group(ng) = struct('name','LIT_SURF', 'mat',29, 'tex',11);

% Parameters for default panel type
defpanel.edge = 0.002; % width of rounded edges

% Default sizes for button 1 (large square buttons with labels, used eg for
% MFD)
btn1.w = 0.0168;
btn1.h = btn1.w;
btn1.depth = 0.005;

% Default sizes for button 2 (small square pushbutton without label)
btn2.w = 0.009;
btn2.h = btn2.w;
btn2.depth = 0.005;

% Default sizes for button 3 (large rectangular button with lit labels)
btn3.w = 40*g.defscale;
btn3.h = 30*g.defscale;
btn3.depth = 0.007;

% Default sizes for dial 1
dial1.rad = 0.007;
dial1.h = 0.015;

% Default sizes for dial 2
dial2.rad = 0.011;
dial2.h = 0.015;

% Default sizes for switch 1 (flip switch)
switch1.rad = 0.004;
switch1.len = 0.025;

% Default sizes for switch 2 (rocker switch)
switch2.len = 52*g.defscale;
switch2.wid = 18*g.defscale;

% Panel reference z-pos
zref = 7.3;
ptexscale = 1/4.5e-4;

%-------------------------------------------------------------------------
% Main panel parameters
%-------------------------------------------------------------------------
vc.main.ref = [0,1.24,zref];  % reference point: top centre of panel
vc.main.tilt = 20.0*RAD;      % panel tilt angle
vc.main.w = 0.56;             % panel width
vc.main.w_bottom = 0.80;      % panel lower edge width
vc.main.w_cnt    = 0.743;
vc.main.w_top    = 0.61;      % panel upper edge width
vc.main.h = 0.21;             % panel height
vc.main.h_cnt    = 0.115;
vc.main.y1 = vc.main.ref(2);  % panel top y-pos
vc.main.z1 = vc.main.ref(3);  % panel top z-pos

% AOA tape
vc.main.aoa.ref = [-0.026,-0.154,0];
vc.main.aoa.w = 0.049;
vc.main.aoa.h = 0.090;
vc.main.aoa.depth = 0.006;
vc.main.aoa.tapew = 0.028;
vc.main.aoa.tapeh = 0.078;
vc.main.aoa.readoutw = 0.008;
vc.main.aoa.u0 = texcrd(24);
vc.main.aoa.u1 = texcrd(104);
vc.main.aoa.v0 = texcrd(124);
vc.main.aoa.v1 = texcrd(64);
vc.main.aoa.screw.u0 = texcrd(485);
vc.main.aoa.screw.u1 = texcrd(456);
vc.main.aoa.screw.v0 = texcrd(36);
vc.main.aoa.screw.v1 = texcrd(7);

% VS tape
vc.main.vs.ref = [0.026,-0.154,0];
vc.main.vs.w = 0.049;
vc.main.vs.h = 0.090;
vc.main.vs.depth = 0.006;
vc.main.vs.tapew = 0.028;
vc.main.vs.tapeh = 0.078;
vc.main.vs.readoutw = 0.008;
vc.main.vs.u0 = texcrd(24);
vc.main.vs.u1 = texcrd(104);
vc.main.vs.v0 = texcrd(124);
vc.main.vs.v1 = texcrd(64);
vc.main.vs.screw.u0 = texcrd(485);
vc.main.vs.screw.u1 = texcrd(456);
vc.main.vs.screw.v0 = texcrd(36);
vc.main.vs.screw.v1 = texcrd(7);

%-------------------------------------------------------------------------
% Triangular panel left of left MFD
%-------------------------------------------------------------------------
vc.main.lpanel = defpanel;
vc.main.lpanel.ref = [-0.28,-0.004,-2*vc.main.lpanel.edge]; % reference point in mainpanel frame: top right corner of flat part of panel
vc.main.lpanel.x0 = vc.main.lpanel.ref(1);
vc.main.lpanel.w_top = 0.024;
vc.main.lpanel.w_cnt = 0.090;
vc.main.lpanel.w_bottom = 0.116;
vc.main.lpanel.h = vc.main.h - 0.011;
vc.main.lpanel.h_cnt = vc.main.h_cnt-0.010;
vc.main.lpanel.u0 = 0;
vc.main.lpanel.ub = vc.main.lpanel.w_bottom/0.1;
vc.main.lpanel.ut = vc.main.lpanel.w_top/0.1;
vc.main.lpanel.v0 = 0;
vc.main.lpanel.v1 = vc.main.lpanel.h/0.1;

% AF selector dial
vc.main.lpanel.afdial = dial2; % copy default dial2 parameters
vc.main.lpanel.afdial.ref = [-0.028,-0.155,0]; % reference point (dial axis) in lpanel frame

% AF selector label
vc.main.lpanel.aflabel.ref = vc.main.lpanel.afdial.ref; % reference point (dial axis) in lpanel frame
vc.main.lpanel.aflabel.u0 = texcrd(411.5);
vc.main.lpanel.aflabel.u1 = texcrd(514.5);
vc.main.lpanel.aflabel.v0 = texcrd( 69.5);
vc.main.lpanel.aflabel.v1 = texcrd(  0.5);
vc.main.lpanel.aflabel.w = (514.5-411.5)*g.defscale;
vc.main.lpanel.aflabel.h = (69.5-0.5)*g.defscale;
vc.main.lpanel.aflabel.y0 = 8*g.defscale;
vc.main.lpanel.aflabel.xc = 3*g.defscale;

%-------------------------------------------------------------------------
% Triangular panel right of right MFD
%-------------------------------------------------------------------------
vc.main.rpanel = vc.main.lpanel; % clone from left panel
vc.main.rpanel.ref(1) = -vc.main.rpanel.ref(1); % reference point in mainpanel frame: top right corner of flat part of panel
vc.main.rpanel.x0 = vc.main.rpanel.ref(1);

%-------------------------------------------------------------------------
% Angular velocity/acceleration display right panel next to right MFD
%-------------------------------------------------------------------------
vc.main.rpanel.avel.w = 0.075;
vc.main.rpanel.avel.h = vc.main.rpanel.avel.w * (238.5-0.5)/(255.5-65.5);
vc.main.rpanel.avel.ref = [0.006+vc.main.rpanel.avel.w/2,0.009-vc.main.rpanel.h,0];
vc.main.rpanel.avel.u = [65.5,255.5]./256;
vc.main.rpanel.avel.v = [238.5,0.5]./256;
vc.main.rpanel.avel.rim = 0.0015;
vc.main.rpanel.avel.depth = 0.002;

vc.main.rpanel.avel_label.ref = [vc.main.rpanel.avel.ref(1),vc.main.rpanel.avel.ref(2)+vc.main.rpanel.avel.h+0.004,0]; % reference point: bottom centre of label
vc.main.rpanel.avel_label.u0 = texcrd(189.5);
vc.main.rpanel.avel_label.u1 = texcrd(349.5);
vc.main.rpanel.avel_label.v0 = texcrd(241.5);
vc.main.rpanel.avel_label.v1 = texcrd(217.5);
vc.main.rpanel.avel_label.w = (349.5-189.5)*g.defscale;
vc.main.rpanel.avel_label.h = (241.5-217.5)*g.defscale;

%-------------------------------------------------------------------------
% Dashboard panel area on top of main panel
%-------------------------------------------------------------------------
vc.dash.ref = [0,vc.main.y1+0.01,zref-0.03]; % reference point in global frame: bottom edge centre
vc.dash.w = vc.main.w;
vc.dash.w_top = vc.dash.w/3;
vc.dash.h = 0.07;
vc.dash.h_edge = vc.dash.h/3;
vc.dash.y0 = vc.dash.ref(2);
vc.dash.z0 = vc.dash.ref(3);
vc.dash.ledge.y = -0.01; % connecting edge to main panel
vc.dash.ledge.z =  0.03;

%-------------------------------------------------------------------------
% The actual dashboard panel surface
%-------------------------------------------------------------------------
vc.dash.panel = defpanel;
vc.dash.panel.ref = [0,-0.005+vc.dash.panel.edge,-vc.dash.panel.edge]; % reference point in dash frame: bottom centre of flat part of panel
vc.dash.panel.h = vc.dash.h - 2*vc.dash.panel.edge + 0.005;
vc.dash.panel.h_edge = vc.dash.h_edge - 2*vc.dash.panel.edge + 0.005;
vc.dash.panel.w = vc.dash.w - 2*vc.dash.panel.edge;
vc.dash.panel.w_top = vc.dash.w_top;

% RCS mode dial
vc.dash.panel.rcsdial = dial2;  % copy default dial2 parameters
vc.dash.panel.rcsdial.ref = [0.065,0.03,0]; % reference point (dial axis) in panel frame

% RCS mode dial label
vc.dash.panel.rcslabel.ref = vc.dash.panel.rcsdial.ref;
vc.dash.panel.rcslabel.u0 = texcrd(411.5);
vc.dash.panel.rcslabel.u1 = texcrd(508.5);
vc.dash.panel.rcslabel.v0 = texcrd(140.5);
vc.dash.panel.rcslabel.v1 = texcrd( 71.5);
vc.dash.panel.rcslabel.w = (508.5-411.5)*g.defscale;
vc.dash.panel.rcslabel.h = (140.5-71.5)*g.defscale;
vc.dash.panel.rcslabel.y0 = 8*g.defscale;
vc.dash.panel.rcslabel.xc = 0;

% HUD controls
vc.dash.panel.hud.ref = [0,vc.dash.panel.rcsdial.ref(2)+vc.dash.panel.rcslabel.h+vc.dash.panel.rcslabel.y0,0];  % reference point in dash frame: top centre of "HUD MODE" label, to make flush with RCS label

% HUD mode buttons
vc.dash.panel.hud.modebtns.ref = [0,-0.018,0]; % reference point: centre of middle button
for i=1:3
    tmp = btn3;
    tmp.ref = [(i-2)*43*g.defscale, 0, 0]; % reference point (button centre)
    tmp.label = struct('u0',texcrd(634.5),'u1',texcrd(689.5),'v0',texcrd(22.5+12*i),'v1',texcrd(11.5+12*i));
    tmp.ofs = g.shadow_ofs - (i-1)*1e-4;
    vc.dash.panel.hud.modebtns.btn(i) = tmp;
end

vc.dash.panel.hud.brtdial = dial1;
vc.dash.panel.hud.brtdial.ref = [-0.0104,-0.040,0]; % reference point (dial axis)

vc.dash.panel.hud.colbtn = btn2;
vc.dash.panel.hud.colbtn.ref = [0.0165,-0.04,0]; % reference point (button centre)

vc.dash.panel.hud.label.ref = [0,0,0]; % reference point in hud frame: top centre of "HUD MODE" label, to make flush with RCS label
vc.dash.panel.hud.label.u0a = texcrd(515.5);  % "a": is for "HUD MODE" label
vc.dash.panel.hud.label.u1a = texcrd(591.5);
vc.dash.panel.hud.label.v0a = texcrd( 11.5);
vc.dash.panel.hud.label.v1a = texcrd(  0.5);
vc.dash.panel.hud.label.wa  = (591.5-515.5)*g.defscale;
vc.dash.panel.hud.label.ha  = (11.5-0.5)*g.defscale;
vc.dash.panel.hud.label.u0b = texcrd(516.5);  % "b": is for the rest of the label
vc.dash.panel.hud.label.u1b = texcrd(630.5);
vc.dash.panel.hud.label.v0b = texcrd( 75.5);
vc.dash.panel.hud.label.v1b = texcrd( 12.5);
vc.dash.panel.hud.label.wb  = (630.5-516.5)*g.defscale;
vc.dash.panel.hud.label.hb  = (75.5-12.5)*g.defscale;
vc.dash.panel.hud.label.x0b = -vc.dash.panel.hud.label.wb/2 - 5*g.defscale;
vc.dash.panel.hud.label.y1b = -67*g.defscale;

% NAV Prog buttons
vc.dash.panel.navbtns.ref = [0.14,0.011,0]; % reference point (bottom centre of button block)
xref = [-64.5,-21.5,-21.5,21.5,21.5,64.5]*g.defscale;
yref = [15,15,48,15,48,15]*g.defscale;
for i=1:6
    tmp = btn3;
    tmp.ref = [xref(i),yref(i),0]; % reference point: button centre
    tmp.label = struct('u0',texcrd(634.5),'u1',texcrd(689.5),'v0',texcrd(58.5+12*i),'v1',texcrd(47.5+12*i));
    tmp.ofs = g.shadow_ofs - (i-1)*1e-4; % re-arrange that
    vc.dash.panel.navbtns.btn(i) = tmp;
end

vc.dash.panel.navbtns.label.ref = [0,0.040,0]; % reference point in nav frame: top centre of label
vc.dash.panel.navbtns.label.u0 = texcrd(358.5);
vc.dash.panel.navbtns.label.u1 = texcrd(516.5);
vc.dash.panel.navbtns.label.v0 = texcrd(153.5);
vc.dash.panel.navbtns.label.v1 = texcrd(141.5);
vc.dash.panel.navbtns.label.w = (516.5-358.5)*g.defscale;
vc.dash.panel.navbtns.label.h = (153.5-141.5)*g.defscale;

%-------------------------------------------------------------------------
% glare shield above dash panel
%-------------------------------------------------------------------------
vc.dash.glare.ref = [0,vc.dash.h,0]; % reference point in dash frame: top centre of dash panel
vc.dash.glare.tilt = 20*RAD;
vc.dash.glare.w_top = vc.dash.w_top;
vc.dash.glare.w_mid = vc.dash.w * 1.05;
vc.dash.glare.y_mid = -2/3*vc.dash.h*1.05 + 0.0025;
vc.dash.glare.depth = 0.03;
vc.dash.glare.thickness = 0.005;
vc.dash.glare.sidetilt = RAD*35; %25;
vc.dash.glare.sidelen = 0.14;

%-------------------------------------------------------------------------
% Overhead panel parameters
%-------------------------------------------------------------------------
vc.over.ref = [0,1.65,6.95]; % reference point in global frame: front centre of overhead panel
vc.over.tilt = -75*RAD;
vc.over.w = 0.4;
vc.over.h = 0.5;
vc.over.yref = 1.65;
vc.over.zref = 6.95;

%-------------------------------------------------------------------------
% Overhead light covers
%-------------------------------------------------------------------------
vc.over.light.x0 = 0.05;
vc.over.light.y0 = 0.2;
vc.over.light.w = 0.06;
vc.over.light.h = vc.over.light.w*128/60;
vc.over.light.depth = 0.007;
vc.over.light.u = texcrd([0.5,127.5]);
vc.over.light.v = texcrd([188.5,247.5]);

%-------------------------------------------------------------------------
% Overhead left front panel
%-------------------------------------------------------------------------
vc.over.lfpanel = defpanel;
vc.over.lfpanel.w = 0.147;
vc.over.lfpanel.h = 0.06;
vc.over.lfpanel.corner = 0.015;
vc.over.lfpanel.ref = [-vc.over.w/2+0.005,vc.over.lfpanel.h+3*vc.over.lfpanel.edge,-vc.over.lfpanel.edge]; % reference point: top left panel edge

vc.over.lfpanel.light.ref = [0.02,-0.005,0]; % reference point: top left label corner

vc.over.lfpanel.light.label.ref = [0,0,0]; % reference point: top left label corner
vc.over.lfpanel.light.label.u0 = texcrd(358.5);
vc.over.lfpanel.light.label.u1 = texcrd(596.5);
vc.over.lfpanel.light.label.v0 = texcrd(245.5);
vc.over.lfpanel.light.label.v1 = texcrd(153.5);
vc.over.lfpanel.light.label.w = (596.5-358.5)*g.defscale;
vc.over.lfpanel.light.label.h = (245.5-153.5)*g.defscale;

vc.over.lfpanel.light.switch1 = switch1;
vc.over.lfpanel.light.switch1.ref = [88.5,-55.5,0]*g.defscale;

vc.over.lfpanel.light.switch2 = switch1;
vc.over.lfpanel.light.switch2.ref = [143.5,-55.5,0]*g.defscale;

vc.over.lfpanel.light.dial1 = dial1;
vc.over.lfpanel.light.dial1.ref = [24.5,-55.5,0]*g.defscale;

vc.over.lfpanel.light.dial2 = dial1;
vc.over.lfpanel.light.dial2.ref = [208.5,-55.5,0]*g.defscale;

%-------------------------------------------------------------------------
% Overhead right front panel
%-------------------------------------------------------------------------
vc.over.rfpanel = defpanel;
vc.over.rfpanel.w = 0.147;
vc.over.rfpanel.h = 0.06;
vc.over.rfpanel.corner = 0.015;
vc.over.rfpanel.ref = [vc.over.w/2-0.005,vc.over.rfpanel.h+3*vc.over.rfpanel.edge,-vc.over.rfpanel.edge]; % reference point: top right panel edge

vc.over.rfpanel.light.ref = [-vc.over.rfpanel.w+0.005,-0.005,0]; % reference point: top left label corner

vc.over.rfpanel.light.label.ref = [0,0,0]; % reference point: top left label corner
vc.over.rfpanel.light.label.u0 = texcrd(305.5);
vc.over.rfpanel.light.label.u1 = texcrd(616.5);
vc.over.rfpanel.light.label.v0 = texcrd(340.5);
vc.over.rfpanel.light.label.v1 = texcrd(248.4);
vc.over.rfpanel.light.label.w = (616.5-305.5)*g.defscale;
vc.over.rfpanel.light.label.h = (340.5-248.4)*g.defscale;

vc.over.rfpanel.light.switch1 = switch1;
vc.over.rfpanel.light.switch1.ref = [28.5,-55.5,0]*g.defscale;

vc.over.rfpanel.light.switch2 = switch1;
vc.over.rfpanel.light.switch2.ref = [104.5,-55.5,0]*g.defscale;

vc.over.rfpanel.light.switch3 = switch1;
vc.over.rfpanel.light.switch3.ref = [154.5,-55.5,0]*g.defscale;

vc.over.rfpanel.light.switch4 = switch1;
vc.over.rfpanel.light.switch4.ref = [204.5,-55.5,0]*g.defscale;

vc.over.rfpanel.light.switch5 = switch1;
vc.over.rfpanel.light.switch5.ref = [280.5,-55.5,0]*g.defscale;

%-------------------------------------------------------------------------
% Overhead left mid panel
%-------------------------------------------------------------------------
vc.over.lmpanel = defpanel;
vc.over.lmpanel.w = 0.147;
vc.over.lmpanel.h = 0.128;
vc.over.lmpanel.ref = [-vc.over.w/2+0.005,vc.over.lfpanel.h+vc.over.lmpanel.h+5*vc.over.lmpanel.edge,-vc.over.lmpanel.edge]; % reference point: top left panel corner

% pressure displays
vc.over.lmpanel.pressuredisp.w = 246*g.defscale;
vc.over.lmpanel.pressuredisp.h = 45*g.defscale;
vc.over.lmpanel.pressuredisp.ref = [(vc.over.lmpanel.w-vc.over.lmpanel.pressuredisp.w)/2,-0.057,0]; % reference point: top left display corner
vc.over.lmpanel.pressuredisp.rim = 0.0015;
vc.over.lmpanel.pressuredisp.depth = 0.0018;

% pressure/lock/hatch/valve labels
vc.over.lmpanel.label.valves.ref = [vc.over.lmpanel.w/2,-0.056,0]; % reference point: bottom centre of label
vc.over.lmpanel.label.valves.u0 = texcrd(0.5);
vc.over.lmpanel.label.valves.u1 = texcrd(321.5);
vc.over.lmpanel.label.valves.v0 = texcrd(464.5);
vc.over.lmpanel.label.valves.v1 = texcrd(346.5);
vc.over.lmpanel.label.valves.w = (321.5-0.5)*g.defscale;
vc.over.lmpanel.label.valves.h = (464.5-346.5)*g.defscale;

vc.over.lmpanel.label.locks.ref = [vc.over.lmpanel.w/2,-0.079,0]; % reference point: top centre of label
vc.over.lmpanel.label.locks.u0 = texcrd(135.5);
vc.over.lmpanel.label.locks.u1 = texcrd(304.5);
vc.over.lmpanel.label.locks.v0a = texcrd(286.5);
vc.over.lmpanel.label.locks.v1a = texcrd(242.5);
vc.over.lmpanel.label.locks.v0b = texcrd(295.5);
vc.over.lmpanel.label.locks.v1b = texcrd(287.5);
vc.over.lmpanel.label.locks.w = (304.5-135.5)*g.defscale;
vc.over.lmpanel.label.locks.ha = (286.5-242.5)*g.defscale;
vc.over.lmpanel.label.locks.hb = (295.5-287.5)*g.defscale;

vc.over.lmpanel.label.press.u0 = texcrd(150.5);
vc.over.lmpanel.label.press.u1 = texcrd(187.5);
vc.over.lmpanel.label.press.v0 = texcrd(240.5);
vc.over.lmpanel.label.press.v1 = texcrd(190.5);
vc.over.lmpanel.label.press.w = (187.5-150.5)*g.defscale;
vc.over.lmpanel.label.press.h = (240.5-190.5)*g.defscale;
vc.over.lmpanel.label.press.ref = [(vc.over.lmpanel.w+vc.over.lmpanel.pressuredisp.w)/2+0.002,-0.057-vc.over.lmpanel.pressuredisp.h/2,0]; % reference point: left centre of label

% switch: hatch open/close
vc.over.lmpanel.switch1 = switch1;
vc.over.lmpanel.switch1.ref = [vc.over.lmpanel.w/2-60*g.defscale,-0.079-67.5*g.defscale,0];

% switch: inner airlock door open/close
vc.over.lmpanel.switch2 = switch1;
vc.over.lmpanel.switch2.ref = [vc.over.lmpanel.w/2,-0.079-67.5*g.defscale,0];

% switch: outer airlock door open/close
vc.over.lmpanel.switch3 = switch1;
vc.over.lmpanel.switch3.ref = [vc.over.lmpanel.w/2+60*g.defscale,-0.079-67.5*g.defscale,0];

% switch: cabin O2/N2 supply valve
vc.over.lmpanel.switch4 = switch1;
vc.over.lmpanel.switch4.ref = [vc.over.lmpanel.w/2-120*g.defscale,-0.056+65*g.defscale,0];

% switch: cabin depressurization valve
vc.over.lmpanel.switch5 = switch1;
vc.over.lmpanel.switch5.ref = [vc.over.lmpanel.w/2-60*g.defscale,-0.056+65*g.defscale,0];

% switch: airlock/cabin equalization valve
vc.over.lmpanel.switch6 = switch1;
vc.over.lmpanel.switch6.ref = [vc.over.lmpanel.w/2,-0.056+65*g.defscale,0];

% switch: airlock depressurization valve
vc.over.lmpanel.switch7 = switch1;
vc.over.lmpanel.switch7.ref = [vc.over.lmpanel.w/2+60*g.defscale,-0.056+65*g.defscale,0];

% switch: airlock O2/N2 supply valve
vc.over.lmpanel.switch8 = switch1;
vc.over.lmpanel.switch8.ref = [vc.over.lmpanel.w/2+120*g.defscale,-0.056+65*g.defscale,0];

%-------------------------------------------------------------------------
% Overhead right mid panel
%-------------------------------------------------------------------------
vc.over.rmpanel = defpanel;
vc.over.rmpanel.w = 0.147;
vc.over.rmpanel.h = 0.128;
vc.over.rmpanel.ref = [vc.over.w/2-0.005-vc.over.rmpanel.w,vc.over.rfpanel.h+vc.over.rmpanel.h+5*vc.over.rmpanel.edge,-vc.over.rmpanel.edge]; % reference point: top left panel corner

% labels
vc.over.rmpanel.label.ref = [0.02,-0.045,0]; % reference point: pump switch centre
vc.over.rmpanel.label.u0 = texcrd(365.5);
vc.over.rmpanel.label.u1 = texcrd(578.5);
vc.over.rmpanel.label.v0 = texcrd(433.5);
vc.over.rmpanel.label.v1 = texcrd(350.5);
vc.over.rmpanel.label.h = (578.5-365.5)*g.defscale;
vc.over.rmpanel.label.w = (433.5-350.5)*g.defscale;

% temperature displays
vc.over.rmpanel.tempdisp.w = 224*g.defscale;
vc.over.rmpanel.tempdisp.h = 200*g.defscale;
vc.over.rmpanel.tempdisp.ref = [0.04,-0.031,0]; % reference point: top left display corner
vc.over.rmpanel.tempdisp.rim = 0.0015;
vc.over.rmpanel.tempdisp.depth = 0.0018;

% switch: pump on/off
vc.over.rmpanel.switch1 = switch1;
vc.over.rmpanel.switch1.ref = [0.02,-0.045,0];

% pump volume dial
vc.over.rmpanel.pumpdial = dial1;
vc.over.rmpanel.pumpdial.ref = [0.02,-0.045-60*g.defscale,0];

% reference temperature dial
vc.over.rmpanel.tempdial = dial1;
vc.over.rmpanel.tempdial.ref = [0.02,-0.045-146*g.defscale,0];

%-------------------------------------------------------------------------
% Lower panel section
%-------------------------------------------------------------------------

vc.lower.ref = [0,vc.main.y1-vc.main.h*cos(vc.main.tilt),vc.main.z1-vc.main.h*sin(vc.main.tilt)]; % reference point: top centre of panel

%-------------------------------------------------------------------------
% Middle part of lower panel section
%-------------------------------------------------------------------------

vc.lower.mid.ref = [0,0,0];  % reference point: top center
vc.lower.mid.w = 0.58;
vc.lower.mid.h = 0.0956;
vc.lower.mid.corner_w = 0.06;
vc.lower.mid.corner_h = 0.06;

%-------------------------------------------------------------------------
% heading display
%-------------------------------------------------------------------------

vc.lower.mid.hdg.ref = [0, -0.045, 0];       % reference point: centre of display
vc.lower.mid.hdg.pos = 0;
vc.lower.mid.hdg.disp = 0.085;               % MFD display width,height
vc.lower.mid.hdg.infrm_w = 0.003;            % width of frame inner edge
vc.lower.mid.hdg.t_frmh  = 0.006;            % height of top border
vc.lower.mid.hdg.lr_frmw = vc.lower.mid.hdg.t_frmh; % width of left,right border
vc.lower.mid.hdg.b_frmh  = vc.lower.mid.hdg.t_frmh; % height of bottom border
vc.lower.mid.hdg.icorner = 0.003;            % size of inner corner roundness
vc.lower.mid.hdg.w = vc.lower.mid.hdg.disp + 2*(vc.lower.mid.hdg.infrm_w+vc.lower.mid.hdg.lr_frmw); % total width of MFD
vc.lower.mid.hdg.h = vc.lower.mid.hdg.disp + 2*vc.lower.mid.hdg.infrm_w+vc.lower.mid.hdg.b_frmh+vc.lower.mid.hdg.t_frmh; % total height of MFD
vc.lower.mid.hdg.outer_depth = 0.008;        % depth of front plate from panel
vc.lower.mid.hdg.inner_depth = 0.01;         % depth of display from front plate
vc.lower.mid.hdg.cnt_dst = -vc.lower.mid.hdg.w/2; % distance of inner edge from centre
vc.lower.mid.hdg.corner = 0.007;             % size of outer corner roundness
vc.lower.mid.hdg.t0 = texcrd(24);
vc.lower.mid.hdg.t1 = texcrd(104);
vc.lower.mid.hdg.v0 = texcrd(128);
vc.lower.mid.hdg.v1 = texcrd(64);
vc.lower.mid.hdg.view.rad = 0.060;

%-------------------------------------------------------------------------
% propellant status display
%-------------------------------------------------------------------------

vc.lower.mid.psd.ref = [0.11,0,0]; % reference point: top left corner
vc.lower.mid.psd.w = 0.14;
vc.lower.mid.psd.h = vc.lower.mid.psd.w*0.6255;
vc.lower.mid.psd.frame.border_lr = 0.008;
vc.lower.mid.psd.frame.border_tb = 0.004;
vc.lower.mid.psd.frame.w = vc.lower.mid.psd.w + 2*vc.lower.mid.psd.frame.border_lr;
vc.lower.mid.psd.frame.h = vc.lower.mid.psd.h + 2*vc.lower.mid.psd.frame.border_tb;
vc.lower.mid.psd.frame.depth = 0.005;

%-------------------------------------------------------------------------
% Gimbal control subplanel
%-------------------------------------------------------------------------

vc.lower.mid.gimbalpanel = defpanel;
vc.lower.mid.gimbalpanel.ref = [-vc.lower.mid.w/2+0.004,0,-2*vc.lower.mid.gimbalpanel.edge]; % reference point: top left corner
vc.lower.mid.gimbalpanel.w = (656-140)*g.defscale-0.001;
vc.lower.mid.gimbalpanel.h = (450-247)*g.defscale;
vc.lower.mid.gimbalpanel.z0 = -0.004;

% gimbal panel labels
vc.lower.mid.gimbalpanel.label.ref = [0,0,0]; % reference point: top left corner of label
vc.lower.mid.gimbalpanel.label.ofs = g.label_ofs;
vc.lower.mid.gimbalpanel.label.u0 = texcrd(0);
vc.lower.mid.gimbalpanel.label.u1 = texcrd(486);
vc.lower.mid.gimbalpanel.label.v0 = texcrd(206);
vc.lower.mid.gimbalpanel.label.v1 = texcrd(7);
vc.lower.mid.gimbalpanel.label.w = (626-140)*g.defscale;
vc.lower.mid.gimbalpanel.label.h = (446-247)*g.defscale;

% gimbal control dial
vc.lower.mid.gimbalpanel.mdial = dial2;
vc.lower.mid.gimbalpanel.mdial.ref = [59,-86,0]*g.defscale;

% hover control dial on front slope panel
vc.lower.mid.gimbalpanel.hdial = dial2;
vc.lower.mid.gimbalpanel.hdial.ref = [273,-86,0]*g.defscale;

% gimbal/hover manual control switches
vc.lower.mid.gimbalpanel.switch = switch2;
vc.lower.mid.gimbalpanel.switch.ref = [0,0,0]; % reference point: top left corner of panel
vc.lower.mid.gimbalpanel.switch.gp.ref = [161, -80,0]*g.defscale;  % centre between gimbal pitch switches
vc.lower.mid.gimbalpanel.switch.gy.ref = [161,-164,0]*g.defscale;  % centre between gimbal yaw switches
vc.lower.mid.gimbalpanel.switch.hp.ref = [375, -80,0]*g.defscale;  % centre of hover pitch switch
vc.lower.mid.gimbalpanel.switch.hr.ref = [375,-151,0]*g.defscale;  % centre of hover roll switch
vc.lower.mid.gimbalpanel.switch.ha.ref = [452,-128,0]*g.defscale;  % centre of hover hold alt preset switch

% gimbal indicator display
vc.lower.mid.gimbalpanel.gdisp.ref = [0.00295,-0.06,0.002]; % reference point: top left corner of display
vc.lower.mid.gimbalpanel.gdisp.w = 0.05;
vc.lower.mid.gimbalpanel.gdisp.h = vc.lower.mid.gimbalpanel.gdisp.w * (428-383)/(236-149);
vc.lower.mid.gimbalpanel.gdisp.rim = 0.0015;
vc.lower.mid.gimbalpanel.gdisp.u0 = texcrd(149);
vc.lower.mid.gimbalpanel.gdisp.u1 = texcrd(236);
vc.lower.mid.gimbalpanel.gdisp.v0 = texcrd(428);
vc.lower.mid.gimbalpanel.gdisp.v1 = texcrd(383);
vc.lower.mid.gimbalpanel.gdisp.ind.cnt0x = vc.lower.mid.gimbalpanel.gdisp.w*(171.5-149)/(236-149);
vc.lower.mid.gimbalpanel.gdisp.ind.cnt1x = vc.lower.mid.gimbalpanel.gdisp.w*(213.5-149)/(236-149);
vc.lower.mid.gimbalpanel.gdisp.ind.cnty  = -vc.lower.mid.gimbalpanel.gdisp.h/2;
vc.lower.mid.gimbalpanel.gdisp.ind.dx    = vc.lower.mid.gimbalpanel.gdisp.w * 4.5/(236-149);
vc.lower.mid.gimbalpanel.gdisp.ind.u0    = texcrd(2,g.texsize_instr.w);
vc.lower.mid.gimbalpanel.gdisp.ind.v0    = texcrd(2,g.texsize_instr.h);
vc.lower.mid.gimbalpanel.gdisp.ind.v1    = texcrd(12,g.texsize_instr.h);

% hover indicator display
vc.lower.mid.gimbalpanel.hdisp.h = vc.lower.mid.gimbalpanel.gdisp.h;
vc.lower.mid.gimbalpanel.hdisp.w = vc.lower.mid.gimbalpanel.hdisp.h;
vc.lower.mid.gimbalpanel.hdisp.ref = [vc.lower.mid.gimbalpanel.gdisp.ref(1) + 214*g.defscale + (vc.lower.mid.gimbalpanel.gdisp.w - vc.lower.mid.gimbalpanel.hdisp.w)/2,-0.06,0.002];
vc.lower.mid.gimbalpanel.hdisp.u0 = texcrd(149);
vc.lower.mid.gimbalpanel.hdisp.u1 = texcrd(194);
vc.lower.mid.gimbalpanel.hdisp.v0 = texcrd(428);
vc.lower.mid.gimbalpanel.hdisp.v1 = texcrd(383);
vc.lower.mid.gimbalpanel.hdisp.ind.cntx = vc.lower.mid.gimbalpanel.hdisp.w/2;
vc.lower.mid.gimbalpanel.hdisp.ind.cnty = -vc.lower.mid.gimbalpanel.hdisp.h/2;
vc.lower.mid.gimbalpanel.hdisp.ind.dx = vc.lower.mid.gimbalpanel.gdisp.ind.dx;
vc.lower.mid.gimbalpanel.hdisp.ind.u0 = vc.lower.mid.gimbalpanel.gdisp.ind.u0;
vc.lower.mid.gimbalpanel.hdisp.ind.v0 = vc.lower.mid.gimbalpanel.gdisp.ind.v0;
vc.lower.mid.gimbalpanel.hdisp.ind.v1 = vc.lower.mid.gimbalpanel.gdisp.ind.v1;

% holdalt indicator display
vc.lower.mid.gimbalpanel.adisp.h = 0.01;
vc.lower.mid.gimbalpanel.adisp.w = 0.03;
vc.lower.mid.gimbalpanel.adisp.ref = [vc.lower.mid.gimbalpanel.hdisp.ref(1) + 185*g.defscale,-0.035,0.002];

% hover altitude subpanel
vc.lower.mid.gimbalpanel.halt.ref = [430,-30,0]*g.defscale;

% hover hold alt button
vc.lower.mid.gimbalpanel.halt.btn = btn3;
vc.lower.mid.gimbalpanel.halt.btn.ref = [35,-20,0]*g.defscale;
vc.lower.mid.gimbalpanel.halt.btn.label = struct('u0',texcrd(634.5),'u1',texcrd(689.5),'v0',texcrd(22.5),'v1',texcrd(11.5));

% hover hold alt display
vc.lower.mid.gimbalpanel.halt.disp.ref = [2,-47.5,0]*g.defscale;
vc.lower.mid.gimbalpanel.halt.disp.w = 0.0305;
vc.lower.mid.gimbalpanel.halt.disp.h = 0.0105;
vc.lower.mid.gimbalpanel.halt.disp.depth = 0.0018;

% hover hold alt "current" button
vc.lower.mid.gimbalpanel.halt.curbtn = btn2;
vc.lower.mid.gimbalpanel.halt.curbtn.ref = [64,-98,0]*g.defscale; % reference point (button centre)

% hover hold alt "Alt" and "VSPD" selector buttons
vc.lower.mid.gimbalpanel.halt.modebtns.ref = [35,-140,0]*g.defscale; % reference point: centre of button pair
for i=1:2
    tmp = btn3;
    tmp.w = 30*g.defscale;
    tmp.ref = [(i-1.5)*33*g.defscale, 0, 0]; % reference point (button centre)
    tmp.label = struct('u0',texcrd(647.5),'u1',texcrd(676.5),'v0',texcrd(130.5+12*i),'v1',texcrd(119.5+12*i)); % FIX!
    tmp.ofs = g.shadow_ofs - (i-1)*1e-4;
    vc.lower.mid.gimbalpanel.halt.modebtns.btn(i) = tmp;
end

%-------------------------------------------------------------------------
% Left part of lower panel section
%-------------------------------------------------------------------------

vc.lower.left.ref = [-vc.lower.mid.w/2,0,0];  % reference point: top right corner
vc.lower.left.y0 = 0.85-vc.lower.left.ref(2)-vc.lower.ref(2); % connect to throttle panel
vc.lower.left.z0 = 7.09-vc.lower.left.ref(3)-vc.lower.ref(3); % connect to throttle panel
vc.lower.left.wt = (vc.main.w_bottom-vc.lower.mid.w)/2;
vc.lower.left.wb = 0.138;
vc.lower.left.h = hypot(vc.lower.left.z0,vc.lower.left.y0);
vc.lower.left.tilt = atan(vc.lower.left.z0/vc.lower.left.y0);

vc.lower.mid.tilt = vc.lower.left.tilt;

%-------------------------------------------------------------------------
% Right part of lower panel section
%-------------------------------------------------------------------------

% supporting structure
vc.lower.right.ref = [vc.lower.mid.w/2,0,0]; % reference point: top left corner
vc.lower.right.y0 = 0.85-vc.lower.right.ref(2)-vc.lower.ref(2); % connect to throttle panel
vc.lower.right.z0 = 7.09-vc.lower.right.ref(3)-vc.lower.ref(3); % connect to throttle panel
vc.lower.right.wt = (vc.main.w_bottom-vc.lower.mid.w)/2;
vc.lower.right.wb = 0.138;
vc.lower.right.h = hypot(vc.lower.right.z0,vc.lower.right.y0);
vc.lower.right.tilt = atan(vc.lower.right.z0/vc.lower.right.y0);

% panel
vc.lower.right.panel = defpanel;
edge = defpanel.edge;
vc.lower.right.panel.ref = [edge,0,-2*edge]; % reference point in lower.right frame: top left corner of flat part of panel
vc.lower.right.panel.w_top = vc.lower.right.wt-2*edge;
vc.lower.right.panel.w_bottom = vc.lower.right.wb-2*edge;
vc.lower.right.panel.h = vc.lower.right.h-edge;
vc.lower.right.panel.u0 = 0;
vc.lower.right.panel.ub = vc.lower.right.panel.w_bottom/0.1;
vc.lower.right.panel.ut = vc.lower.right.panel.w_top/0.1;
vc.lower.right.panel.v0 = 0;
vc.lower.right.panel.v1 = vc.lower.right.panel.h/0.1;

% undock subpanel
vc.lower.right.panel.undock.ref = [0.03,-0.098,0]; % centre of slit (relative to panel)
vc.lower.right.panel.undock.slit.w = 0.035;
vc.lower.right.panel.undock.slit.h = 0.12;
vc.lower.right.panel.undock.slit.depth = 0.03;
vc.lower.right.panel.undock.llabel.w = 0.009;
vc.lower.right.panel.undock.rlabel.w = 0.012;

% undock lever
vc.lower.right.panel.undock.lever.w = vc.lower.right.panel.undock.slit.w - 2*0.002;
vc.lower.right.panel.undock.lever.h = vc.lower.right.panel.undock.slit.h - 0.03;
vc.lower.right.panel.undock.lever.corner = 0.002;
vc.lower.right.panel.undock.lever.top_tickness = 0.006;
vc.lower.right.panel.undock.lever.bottom_thickness = vc.lower.right.panel.undock.slit.depth-0.005;
vc.lower.right.panel.undock.lever.x0 = -vc.lower.right.panel.undock.lever.w/2;
vc.lower.right.panel.undock.lever.y0 = -vc.lower.right.panel.undock.slit.h/2 + 0.002;
vc.lower.right.panel.undock.lever.v = texcrd([180.5,245.5]);
vc.lower.right.panel.undock.lever.u = texcrd([449.5,634.5]);

vc.lower.right.panel.undock.label.u0 = texcrd(346.5);
vc.lower.right.panel.undock.label.u1 = texcrd(358.5);
vc.lower.right.panel.undock.label.v0 = texcrd(159.5);
vc.lower.right.panel.undock.label.v1 = texcrd(  0.5);
vc.lower.right.panel.undock.label.w = (358.5-346.5)*g.defscale;
vc.lower.right.panel.undock.label.h = (159.5-0.5)*g.defscale;
vc.lower.right.panel.undock.label.x0 = -vc.lower.right.panel.undock.slit.w/2 - 0.003;
vc.lower.right.panel.undock.label.ycnt = 0;

% Nosecone subpanel (relative to top left corner of right slope panel)
vc.lower.right.panel.ncone.ref = [0.082,-0.108,0.003]; % reference point: slit centre (relative to panel frame)
vc.lower.right.panel.ncone.x = [vc.lower.right.panel.ncone.ref(1)+0.014, vc.lower.right.panel.ncone.ref(1)-0.022];
vc.lower.right.panel.ncone.y = [vc.lower.right.panel.ncone.ref(2)-0.06,  vc.lower.right.panel.ncone.ref(2)+0.08];
vc.lower.right.panel.ncone.z = 0;
vc.lower.right.panel.ncone.slit.h2 = 80.5/ptexscale;
vc.lower.right.panel.ncone.slit.w2 = 0.008;
vc.lower.right.panel.ncone.slit.depth = 0.03;
vc.lower.right.panel.ncone.u = texcrd(g.texsize-54+[-0.014,0.022]*ptexscale);
vc.lower.right.panel.ncone.v = texcrd(402+[0.06,-0.08]*ptexscale);

% Nosecone lever on right slope panel (relative to top left corner of
% right slope panel)
vc.lower.right.panel.ncone.lever.depth = 0.05;
vc.lower.right.panel.ncone.lever.stick.rad = 0.006;     % stick radius
vc.lower.right.panel.ncone.lever.stick.length = 0.08;   % stick length
vc.lower.right.panel.ncone.lever.handle.rad = 0.008;    % handle radius
vc.lower.right.panel.ncone.lever.handle.length = 0.035; % handle length
vc.lower.right.panel.ncone.lever.handle.corner = 0.002; % handle roundness
vc.lower.right.panel.ncone.lever.handle.nseg = 12;
vc.lower.right.panel.ncone.lever.axis.rad = 0.035; % radius of rotation axis
vc.lower.right.panel.ncone.lever.axis.u = texcrd([64.5,95.5]);
vc.lower.right.panel.ncone.lever.axis.v = texcrd([179.5,172.5]);

vc.lower.right.panel.ncone.label.u0 = texcrd(310.5);
vc.lower.right.panel.ncone.label.u1 = texcrd(345.5);
vc.lower.right.panel.ncone.label.v0 = texcrd(164.5);
vc.lower.right.panel.ncone.label.v1 = texcrd(  0.5);
vc.lower.right.panel.ncone.label.w = (345.5-310.5)*g.defscale;
vc.lower.right.panel.ncone.label.h = (164.5-0.5)*g.defscale;
vc.lower.right.panel.ncone.label.x0 = vc.lower.right.panel.ncone.ref(1) - vc.lower.right.panel.ncone.slit.w2;% - 0.001;
vc.lower.right.panel.ncone.label.ycnt = vc.lower.right.panel.ncone.ref(2);

vc.lower.right.panel.ncone.sub.x0 = vc.lower.right.panel.ncone.ref(1) + vc.lower.right.panel.ncone.slit.w2 + 0.007;
vc.lower.right.panel.ncone.sub.x1 = vc.lower.right.panel.ncone.ref(1) - vc.lower.right.panel.ncone.slit.w2 - 0.015;
vc.lower.right.panel.ncone.sub.y0 = vc.lower.right.panel.ncone.ref(2) - vc.lower.right.panel.ncone.slit.h2 - vc.lower.right.panel.ncone.slit.w2*2;
vc.lower.right.panel.ncone.sub.y1 = vc.lower.right.panel.ncone.ref(2) + 0.096;
vc.lower.right.panel.ncone.sub.rim = 0.003;

% Nosecone indicator lights
vc.lower.right.panel.ncone.indicator.w = 0.03;
vc.lower.right.panel.ncone.indicator.h = vc.lower.right.panel.ncone.indicator.w*63/59;
vc.lower.right.panel.ncone.indicator.xcnt = -0.004; % from slit centre
vc.lower.right.panel.ncone.indicator.ycnt = vc.lower.right.panel.ncone.slit.h2+0.04; % from slit centre
vc.lower.right.panel.ncone.indicator.z0 = -0.005;
vc.lower.right.panel.ncone.indicator.u = texcrd([60.5,119.5]);
vc.lower.right.panel.ncone.indicator.v = texcrd([307.5,248.5]);

% Retro engine cover switch (relative to top left corner of rcpanel)
vc.lower.right.panel.rcover.ref = [0.025,-0.205,-0.002];
vc.lower.right.panel.rcover.switch = switch1;
vc.lower.right.panel.rcover.label.u0 = texcrd(359.5);
vc.lower.right.panel.rcover.label.u1 = texcrd(410.5);
vc.lower.right.panel.rcover.label.v0 = texcrd( 98.5);
vc.lower.right.panel.rcover.label.v1 = texcrd(0.5);
vc.lower.right.panel.rcover.label.w = (410.5-359.5)*g.defscale;
vc.lower.right.panel.rcover.label.h = (98.5-0.5)*g.defscale;
vc.lower.right.panel.rcover.label.x0 = -vc.lower.right.panel.rcover.label.w/2;
vc.lower.right.panel.rcover.label.y0 = -33.5*g.defscale;
vc.lower.right.panel.rcover.ind.w = 0.007;
vc.lower.right.panel.rcover.ind.h = 0.003;
vc.lower.right.panel.rcover.ind.u0 = texcrd(60);
vc.lower.right.panel.rcover.ind.u1 = texcrd(60);
vc.lower.right.panel.rcover.ind.v0 = texcrd(265);
vc.lower.right.panel.rcover.ind.v1 = texcrd(265);

% Escape ladder switch (relative to top left corner of rcpanel)
vc.lower.right.panel.ladder.ref = [0.06,-0.205,-0.002];
vc.lower.right.panel.ladder.switch = switch1;
vc.lower.right.panel.ladder.label.u0 = texcrd(616.5);
vc.lower.right.panel.ladder.label.u1 = texcrd(673.5);
vc.lower.right.panel.ladder.label.v0 = texcrd(346.5);
vc.lower.right.panel.ladder.label.v1 = texcrd(248.5);
vc.lower.right.panel.ladder.label.w = (673.5-616.5)*g.defscale;
vc.lower.right.panel.ladder.label.h = (346.5-248.5)*g.defscale;
vc.lower.right.panel.ladder.label.x0 = -vc.lower.right.panel.ladder.label.w/2;
vc.lower.right.panel.ladder.label.y0 = -33.5*g.defscale;
vc.lower.right.panel.ladder.ind.w = 0.007;
vc.lower.right.panel.ladder.ind.h = 0.003;
vc.lower.right.panel.ladder.ind.u0 = texcrd(60);
vc.lower.right.panel.ladder.ind.u1 = texcrd(60);
vc.lower.right.panel.ladder.ind.v0 = texcrd(265);
vc.lower.right.panel.ladder.ind.v1 = texcrd(265);

% dock seal indicator
vc.lower.right.panel.seal.ref = [0.03,-0.025,0];
vc.lower.right.panel.seal.ind.w = 0.02;
vc.lower.right.panel.seal.ind.h = 0.005;
vc.lower.right.panel.seal.ind.u0 = texcrd(60);
vc.lower.right.panel.seal.ind.u1 = texcrd(60);
vc.lower.right.panel.seal.ind.v0 = texcrd(265);
vc.lower.right.panel.seal.ind.v1 = texcrd(265);
vc.lower.right.panel.seal.label.u0 = texcrd(304.5);
vc.lower.right.panel.seal.label.u1 = texcrd(345.5);
vc.lower.right.panel.seal.label.v0 = texcrd(204.5);
vc.lower.right.panel.seal.label.v1 = texcrd(180.5);
vc.lower.right.panel.seal.label.w = (345.5-304.5)*g.defscale;
vc.lower.right.panel.seal.label.h = (204.5-180.5)*g.defscale;
vc.lower.right.panel.seal.label.x0 = -vc.lower.right.panel.seal.label.w/2;
vc.lower.right.panel.seal.label.y0 = 15*g.defscale;

%-------------------------------------------------------------------------
% Left part of lower panel section
%-------------------------------------------------------------------------

% supporting structure
vc.lower.left.ref = [-vc.lower.mid.w/2,0,0]; % reference point: top right corner
vc.lower.left.y0 = 0.85-vc.lower.left.ref(2)-vc.lower.ref(2); % connect to throttle panel
vc.lower.left.z0 = 7.09-vc.lower.right.ref(3)-vc.lower.ref(3); % connect to throttle panel
vc.lower.left.wt = (vc.main.w_bottom-vc.lower.mid.w)/2;
vc.lower.left.wb = 0.138;
vc.lower.left.h = hypot(vc.lower.left.z0,vc.lower.left.y0);
vc.lower.left.tilt = atan(vc.lower.left.z0/vc.lower.left.y0);

% panel
vc.lower.left.panel = defpanel;
edge = defpanel.edge;
vc.lower.left.panel.ref = [-edge,0,-2*edge]; % reference point in lower.left frame: top right corner of flat part of panel
vc.lower.left.panel.w_top = vc.lower.left.wt-2*edge;
vc.lower.left.panel.w_bottom = vc.lower.left.wb-2*edge;
vc.lower.left.panel.h = vc.lower.left.h-edge;
vc.lower.left.panel.u0 = 0;
vc.lower.left.panel.ub = vc.lower.left.panel.w_bottom/0.1;
vc.lower.left.panel.ut = vc.lower.left.panel.w_top/0.1;
vc.lower.left.panel.v0 = 0;
vc.lower.left.panel.v1 = vc.lower.left.panel.h/0.1;

% airbrake subpanel
vc.lower.left.panel.abrake.ref = [-0.023,-0.043,0]; % centre of slit (relative to panel)
vc.lower.left.panel.abrake.slit.w = 0.014;
vc.lower.left.panel.abrake.slit.h = 0.08;
vc.lower.left.panel.abrake.llabel.w = 0.009;
vc.lower.left.panel.abrake.rlabel.w = 0.012;

% airbrake lever
vc.lower.left.panel.abrake.lever.rot_ref = [0,-vc.lower.left.panel.abrake.slit.h/2+0.02,0.08];
vc.lower.left.panel.abrake.lever.stick.h = 0.012;
vc.lower.left.panel.abrake.lever.stick.w = 0.008;
vc.lower.left.panel.abrake.lever.stick.z = [0.01,-0.04];
vc.lower.left.panel.abrake.lever.handle.w = 0.04;
vc.lower.left.panel.abrake.lever.handle.depth = 0.01;
vc.lower.left.panel.abrake.lever.handle.corner = 0.002;
vc.lower.left.panel.abrake.lever.handle.u = texcrd(4);
vc.lower.left.panel.abrake.lever.handle.v = texcrd(180);

% elevator trim subpanel
vc.lower.left.panel.etrim.ref = [-0.039,-0.15,0]; % centre of slit (relative to panel)
vc.lower.left.panel.etrim.slit.w = 0.017;
vc.lower.left.panel.etrim.slit.h = 0.1;
vc.lower.left.panel.etrim.scale.w = 0.01;
vc.lower.left.panel.etrim.scale.h = 0.07;
vc.lower.left.panel.etrim.scale.z = 0.002;
vc.lower.left.panel.etrim.scale.u0 = texcrd(g.texsize-60.5);
vc.lower.left.panel.etrim.scale.u1 = texcrd(g.texsize-39.5);
vc.lower.left.panel.etrim.scale.v0 = texcrd(189);
vc.lower.left.panel.etrim.scale.v1 = texcrd(34);
vc.lower.left.panel.etrim.label.u0 = texcrd(238);
vc.lower.left.panel.etrim.label.u1 = texcrd(285);
vc.lower.left.panel.etrim.label.v0 = texcrd(214.5);
vc.lower.left.panel.etrim.label.v1 = texcrd(  0.5);
vc.lower.left.panel.etrim.label.w  = (285-238)*g.defscale;
vc.lower.left.panel.etrim.label.h  = (214.5-0.5)*g.defscale;
vc.lower.left.panel.etrim.label.x0 = vc.lower.left.panel.etrim.slit.w/2 + 0.003;
vc.lower.left.panel.etrim.label.ycnt = 0;

% Elevator trim wheel and indicator on left slope panel
vc.lower.left.panel.etrim.wheel.rad = 0.065;
vc.lower.left.panel.etrim.wheel.rot_ref = [0,0,vc.lower.left.panel.etrim.wheel.rad*0.7];
vc.lower.left.panel.etrim.wheel.aperture = pi*0.62;
vc.lower.left.panel.etrim.wheel.rot = -pi/2;
vc.lower.left.panel.etrim.wheel.nseg = 10;

% Landing gear subpanel (relative to top right corner of left slope panel)
vc.lower.left.panel.gear.ref = [-0.082,-0.108,0.003]; % reference point: slit centre (relative to panel frame)
vc.lower.left.panel.gear.x = [vc.lower.left.panel.gear.ref(1)-0.014, vc.lower.left.panel.gear.ref(1)+0.022];
vc.lower.left.panel.gear.y = [vc.lower.left.panel.gear.ref(2)-0.06,  vc.lower.left.panel.gear.ref(2)+0.08];
vc.lower.left.panel.gear.z = 0;
vc.lower.left.panel.gear.slit.h2 = 80.5/ptexscale;
vc.lower.left.panel.gear.slit.w2 = 0.008;
vc.lower.left.panel.gear.slit.depth = 0.03;
vc.lower.left.panel.gear.u = texcrd(g.texsize-54+[-0.014,0.022]*ptexscale);
vc.lower.left.panel.gear.v = texcrd(402+[0.06,-0.08]*ptexscale);

% Landing gear lever on left slope panel (relative to top right corner of
% left slope panel)
vc.lower.left.panel.gear.lever.depth = 0.05;
vc.lower.left.panel.gear.lever.stick.rad = 0.006;   % stick radius
vc.lower.left.panel.gear.lever.stick.length = 0.08; % stick length
vc.lower.left.panel.gear.lever.knob.rad = 0.017;    % knob radius
vc.lower.left.panel.gear.lever.knob.w = 0.015;      % knob width
vc.lower.left.panel.gear.lever.axis.rad = 0.035; % radius of rotation axis
vc.lower.left.panel.gear.lever.axis.u = texcrd([64.5,95.5]);
vc.lower.left.panel.gear.lever.axis.v = texcrd([179.5,172.5]);

vc.lower.left.panel.gear.label.u0 = texcrd(285.5);
vc.lower.left.panel.gear.label.u1 = texcrd(309.5);
vc.lower.left.panel.gear.label.v0 = texcrd(164.5);
vc.lower.left.panel.gear.label.v1 = texcrd(  0.5);
vc.lower.left.panel.gear.label.w = (309.5-285.5)*g.defscale;
vc.lower.left.panel.gear.label.h = (164.5-0.5)*g.defscale;
vc.lower.left.panel.gear.label.ycnt = 0;

vc.lower.left.panel.gear.sub.x0 = vc.lower.left.panel.gear.ref(1) - vc.lower.left.panel.gear.slit.w2 - 0.007;
vc.lower.left.panel.gear.sub.x1 = vc.lower.left.panel.gear.ref(1) + vc.lower.left.panel.gear.slit.w2 + 0.015;
vc.lower.left.panel.gear.sub.y0 = vc.lower.left.panel.gear.ref(2) - vc.lower.left.panel.gear.slit.h2 - vc.lower.left.panel.gear.slit.w2*2;
vc.lower.left.panel.gear.sub.y1 = vc.lower.left.panel.gear.ref(2) + 0.096;
vc.lower.left.panel.gear.sub.rim = 0.003;

% Gear status indicator lights
vc.lower.left.panel.gear.indicator.w = 0.03;
vc.lower.left.panel.gear.indicator.h = vc.lower.left.panel.gear.indicator.w*63/59;
vc.lower.left.panel.gear.indicator.xcnt = 0.004; % from slit centre
vc.lower.left.panel.gear.indicator.ycnt = vc.lower.left.panel.gear.slit.h2+0.04; % from slit centre
vc.lower.left.panel.gear.indicator.z0 = -0.005;
vc.lower.left.panel.gear.indicator.u = texcrd([0.5,59.5]);
vc.lower.left.panel.gear.indicator.v = texcrd([311.5,248.5]);













% Scram temperatur display on overhead panel
vc.over.scrmtemp.w = 0.04;
vc.over.scrmtemp.h = vc.over.scrmtemp.w*1.5482;
vc.over.scrmtemp.x0 = -0.12-vc.over.scrmtemp.w;
vc.over.scrmtemp.z0 = 0.1;
vc.over.scrmtemp.y0 = -0.005;
vc.over.scrmtemp.u = [0.002,0.365];
vc.over.scrmtemp.v = [0.564,0.002];














% front slope panel parameters
fslope.w = 0.58;
fslope.x0 = -fslope.w/2;
fslope.yref = vc.main.y1 - vc.main.h*cos(vc.main.tilt);
fslope.zref = vc.main.z1 - vc.main.h*sin(vc.main.tilt);
fslope.h = 0.0956;
fslope.corner.h = 0.06;
fslope.corner.w = 0.06;


% ------------------------------------------------------------------------
% support structure for the left bottom panel
%-------------------------------------------------------------------------
lslope.ref = [fslope.x0, fslope.yref, fslope.zref]; % reference point: top right corner
lslope.x0 = lslope.ref(1);
lslope.y0 = 0.85;  % connects the bottom to the throttle panel
lslope.z0 = 7.09;  %
lslope.y1 = lslope.ref(2);
lslope.z1 = lslope.ref(3);
lslope.h = hypot(lslope.z1-lslope.z0,lslope.y1-lslope.y0);
lslope.w_top = (vc.main.w_bottom-fslope.w)/2;
lslope.w_bottom = 0.138;
lslope.tilt = atan((lslope.z1-lslope.z0)/(lslope.y1-lslope.y0));
lslope.scale = g.defscale;

fslope.tilt = lslope.tilt;
writeh_scalar (fhd, 'vc_lpanel_tilt', fslope.tilt, 'Lower front panel: tilt from vertical');

%-------------------------------------------------------------------------
% Panel on the left side of the lower slope (on top of lslope)
%-------------------------------------------------------------------------
lslope.panel = defpanel;
lslope.panel.ref = [-lslope.panel.edge,0,-2*lslope.panel.edge]; % reference point in lslope frame: top right corner of flat part of panel
lslope.panel.x0 = lslope.panel.ref(1);
lslope.panel.w_top = lslope.w_top - 2*lslope.panel.edge;
lslope.panel.w_bottom = lslope.w_bottom - 2*lslope.panel.edge;
lslope.panel.h = lslope.h - lslope.panel.edge;
lslope.panel.u0 = 0;
lslope.panel.ub = lslope.panel.w_bottom/0.1;
lslope.panel.ut = lslope.panel.w_top/0.1;
lslope.panel.v0 = 0;
lslope.panel.v1 = lslope.panel.h/0.1;

% airbrake lever
lslope.panel.abrake.slit.w = 0.014;
lslope.panel.abrake.slit.h = 0.08;
lslope.panel.abrake.slit.xcnt = -0.023;
lslope.panel.abrake.slit.ycnt = -0.043;
lslope.panel.abrake.llabel.w = 0.009;
lslope.panel.abrake.rlabel.w = 0.012;

% ------------------------------------------------------------------------
% right slope panel parameters
rslope.x0 = -fslope.x0;
rslope.y0 = 0.85;
rslope.z0 = 7.09; % 7.12569;
rslope.yref = fslope.yref;
rslope.zref = fslope.zref;
rslope.h = hypot(lslope.ref(3)-lslope.z0,lslope.ref(2)-lslope.y0);
rslope.w_top = (vc.main.w_bottom-fslope.w)/2;
rslope.w_bottom = 0.138;
rslope.tilt = atan((lslope.ref(3)-lslope.z0)/(lslope.ref(2)-lslope.y0));
rslope.scale = g.defscale;

% left side panel parameters
lside.yref = lslope.y0;
lside.zref = lslope.z0;
lside.x0   = lslope.x0;
lside.h    = lside.zref-6.9223;
lside.w    = lslope.w_bottom;
lside.tilt = pi/2;

% right side panel parameters
rside.yref = 0.85;
rside.zref = 7.09;
rside.x0 = -fslope.x0;
rside.h = 0.35;
rside.w = 0.138;
rside.tilt = pi/2;

% status indicator subpanel (relative to top left corner of right side
% panel)
rside.status.yref = -0.005;
rside.status.xref = 0.01;
rside.status.zref = 0.002;
rside.status.w = 0.12;
rside.status.h = rside.status.w*0.2;
rside.status.gap = 0.002;

% MFD frame parameters
vc.main.mfd.disp = 0.16;                % MFD display width,height
vc.main.mfd.infrm_w = 0.003;            % width of frame inner edge
vc.main.mfd.lr_frmw = vc.main.mfd.disp * 0.15;  % width of left,right border
vc.main.mfd.b_frmh  = vc.main.mfd.lr_frmw;      % height of bottom border
vc.main.mfd.t_frmh  = vc.main.mfd.b_frmh * 0.5; % height of top border
vc.main.mfd.icorner = 0.003;            % size of inner corner roundness
vc.main.mfd.w = vc.main.mfd.disp + 2*(vc.main.mfd.infrm_w+vc.main.mfd.lr_frmw); % total width of MFD
vc.main.mfd.h = vc.main.mfd.disp + 2*vc.main.mfd.infrm_w+vc.main.mfd.b_frmh+vc.main.mfd.t_frmh; % total height of MFD
vc.main.mfd.outer_depth = 0.01;         % depth of front plate from panel
vc.main.mfd.inner_depth = 0.008;        % depth of display from front plate
vc.main.mfd.cnt_dst = 0.058;            % distance of inner edge from centre
vc.main.mfd.top_dst = 0;                % distance of top edge from panel top edge
vc.main.mfd.corner = 0.007;             % size of outer corner roundness
vc.main.mfd.t0 = texcrd(0);
vc.main.mfd.t1 = texcrd(128);
vc.main.mfd.v0 = texcrd(128);
vc.main.mfd.v1 = texcrd(0);
vc.main.mfd.btnw = 0.7*vc.main.mfd.lr_frmw;     % MFD button width
vc.main.mfd.btnh = vc.main.mfd.btnw;            % MFD button height
vc.main.mfd.btngap = vc.main.mfd.btnh*0.31;     % gap between buttons
vc.main.mfd.btn_depth = 0.005;          % button depth above front plate

% Artificial horizon frame parameters
vc.main.hor = vc.main.mfd;
vc.main.hor.disp = 0.085;
vc.main.hor.t_frmh = 0.006;
vc.main.hor.lr_frmw = vc.main.hor.t_frmh;      % width of left,right border
vc.main.hor.b_frmh  = vc.main.hor.t_frmh;      % height of bottom border
vc.main.hor.w = vc.main.hor.disp + 2*(vc.main.hor.infrm_w+vc.main.hor.lr_frmw); % total width of MFD
vc.main.hor.h = vc.main.hor.disp + 2*vc.main.hor.infrm_w+vc.main.hor.b_frmh+vc.main.hor.t_frmh; % total height of MFD
vc.main.hor.cnt_dst = -vc.main.hor.w/2;             % distance of inner edge from centre
vc.main.hor.outer_depth = 0.008;         % depth of front plate from panel
vc.main.hor.inner_depth = 0.01;        % depth of display from front plate
vc.main.hor.top_dst = 0.002;
vc.main.hor.t0 = texcrd(24);
vc.main.hor.t1 = texcrd(104);
vc.main.hor.v0 = texcrd(128)*vc.main.hor.h/vc.main.mfd.h;
vc.main.hor.v1 = texcrd(0);

% Artificial horizon display parameters
vc.main.hor.view.rad = 0.055; % view radius
vc.main.hor.view.tilt = vc.main.tilt;
vc.main.hor.view.cnt.x = 0;
vc.main.hor.view.cnt.y = 1.189;
vc.main.hor.view.cnt.z = 7.285;

% Heading indicator frame parameters
vc.main.hdg = vc.main.hor;
vc.main.hdg.top_dst = vc.main.hor.top_dst + vc.main.hor.h + 0.004;
vc.main.hdg.v0 = texcrd(128);
vc.main.hdg.v1 = texcrd(64);
vc.main.hdg.pos = 0;
vc.main.hdg.ref = [0,-vc.main.hdg.h,0];

% Heading indicator display parameters
vc.main.hdg.view.rad = 0.060;
vc.main.hdg.view.tilt = vc.main.hor.view.tilt;
vc.main.hdg.view.cnt.x = 0;
vc.main.hdg.view.cnt.y = 1.0903;
vc.main.hdg.view.cnt.z = 7.2491;

% HUD parameters
hud.pane.w = 0.1780;
hud.pane.h = 0.2;
hud.pane.corner = 0.03;
hud.pane.depth = 0.009;
hud.pane.yref = 1.4750;
hud.pane.zref = 7.0700;
hud.pane.tilt = 26*RAD;
hud.frame.width = 0.0015;
hud.frame.depth = 0.0015;
hud.frame.grip = 0.03;
hud.joint.yref = 1.5836;
hud.joint.zref = 7.1280;
hud.joint.h = 0.02;
hud.joint.width = 0.04;
hud.joint.depth = 0.007;
hud.joint.rad = 0.006;
hud.joint.nseg = 16;
hud.rail.ycnt = 0.99;
hud.rail.zcnt = 6.53;
hud.rail.depth = 0.007;
hud.rail.rad = 0.8426-hud.rail.depth/2;
hud.rail.phi0 = 46*RAD;
hud.rail.dphi = 2.5*RAD;
hud.rail.nseg = 6;
hud.rail.hingew = 0.008;
hud.proj.dist = 0.22;
hud.proj.tuberad = 0.04;
hud.proj.tubelen = 0.15;
hud.proj.nseg = 16;

% HUD logical surface parameters
hud.surf.w  = 0.15;
hud.surf.y0 = 1.3869;
hud.surf.y1 = 1.5369;
hud.surf.z  = 7.09;
hud.surf.u0 = 0.0005;
hud.surf.u1 = 0.9995;
hud.surf.v0 = 0.0005;
hud.surf.v1 = 0.9995;

% ========================================================================
% Material section
% ========================================================================
mat(1).name = 'LIT_BUTTON';
mat(1).diffuse  = [1.0 1.0 1.0 1.0];
mat(1).ambient  = [1.0 1.0 1.0 1.0];
mat(1).specular = [0.1 0.1 0.1 1.0 5.0];
mat(1).emissive = [0.6 0.6 0.6 1];

mat(2).name = 'dgint1_1';
mat(2).diffuse  = [1.000 1.000 1.000 1.000];
mat(2).ambient  = [0.690 0.690 0.690 1.000];
mat(2).specular = [0.784 0.784 0.784 1.000];
mat(2).emissive = [0.800 0.800 0.800 1];

mat(3).name = 'dgint1_1_A';
mat(3).diffuse  = [1.000 1.000 1.000 1.000];
mat(3).ambient  = [0.686 0.722 0.733 1.000];
mat(3).specular = [0.859 0.910 0.933 1.000 40];
mat(3).emissive = [0.800 0.800 0.800 1];

mat(4).name = 'dgint1_1_B';
mat(4).diffuse  = [1.000 1.000 1.000 1.000];
mat(4).ambient  = [0.678 0.498 0.427 1.000];
mat(4).specular = [0.973 0.949 0.886 1.000 25];
mat(4).emissive = [0.800 0.800 0.800 1];

mat(5).name = 'dgint1_1_M';
mat(5).diffuse  = [1.000 1.000 1.000 1.000];
mat(5).ambient  = [0.765 0.765 0.765 1.000];
mat(5).specular = [0.271 0.271 0.271 1.000];
mat(5).emissive = [0.800 0.800 0.800 1];

mat(6).name = 'dgint1_3';
mat(6).diffuse  = [1.0 1.0 1.0 1.0];
mat(6).ambient  = [1.0 1.0 1.0 1.0];
mat(6).specular = [0.1 0.1 0.1 1.0 5.0];
mat(6).emissive = [0.15 0.15 0.15 1];

mat(7).name = 'dgint1_3_A';
mat(7).diffuse  = [1.000 1.000 1.000 1.000];
mat(7).ambient  = [0.686 0.725 0.733 1.000];
mat(7).specular = [0.863 0.914 0.937 1.000 40];
mat(7).emissive = [0.800 0.800 0.800 1];

mat(8).name = 'hud_lens';
mat(8).diffuse  = [1.000 1.000 1.000 1.000];
mat(8).ambient  = [0.882 0.882 0.882 1.000];
mat(8).specular = [0.898 0.898 0.898 1.000];
mat(8).emissive = [0.8 1 0.8 1];

mat(9).name = 'dgint1_3_M';
mat(9).diffuse  = [1.000 1.000 1.000 1.000];
mat(9).ambient  = [0.765 0.765 0.765 1.000];
mat(9).specular = [0.271 0.271 0.271 1.000];
mat(9).emissive = [0.800 0.800 0.800 1];

mat(10).name = 'dgip1_1';
mat(10).diffuse  = [1.000 1.000 1.000 1.000];
mat(10).ambient  = [0.882 0.882 0.882 1.000];
mat(10).specular = [0.898 0.898 0.898 1.000];
mat(10).emissive = [0.800 0.800 0.800 1];

mat(11).name = 'dgip1_2';
mat(11).diffuse  = [1.000 1.000 1.000 1.000];
mat(11).ambient  = [0.882 0.882 0.882 1.000];
mat(11).specular = [0.898 0.898 0.898 1.000];
mat(11).emissive = [0.800 0.800 0.800 1];

mat(12).name = 'glowlabel';
mat(12).diffuse  = [1.0 1.0 1.0 1.0];
mat(12).ambient  = [1.0 1.0 1.0 1.0];
mat(12).specular = [0.1 0.1 0.1 1.0 5.0];
mat(12).emissive = [0.15 0.15 0.15 1];

mat(13).name = 'dgip1_4';
mat(13).diffuse  = [1.000 1.000 1.000 1.000];
mat(13).ambient  = [0.882 0.882 0.882 1.000];
mat(13).specular = [0.898 0.898 0.898 1.000];
mat(13).emissive = [0.800 0.800 0.800 1];

mat(14).name = 'dgip1_5';
mat(14).diffuse  = [1.000 1.000 1.000 1.000];
mat(14).ambient  = [0.882 0.882 0.882 1.000];
mat(14).specular = [0.898 0.898 0.898 1.000];
mat(14).emissive = [0.800 0.800 0.800 1];

mat(15).name = 'panels';
mat(15).diffuse  = [0.5 0.6 0.6 1];
mat(15).ambient  = [0.5 0.6 0.6 1];
mat(15).specular = [0.2 0.2 0.2 1 10.0];
mat(15).emissive = [0.15 0.15 0.15 1];

mat(16).name = 'dgpilot1_1';
mat(16).diffuse  = [1.000 1.000 1.000 1.000];
mat(16).ambient  = [0.882 0.882 0.882 1.000];
mat(16).specular = [0.898 0.898 0.898 1.000];
mat(16).emissive = [0.800 0.800 0.800 1];

mat(17).name = 'glider4_2';
mat(17).diffuse  = [1.000 1.000 1.000 1.000];
mat(17).ambient  = [1.000 1.000 1.000 1.000];
mat(17).specular = [1.000 1.000 1.000 1.000];
mat(17).emissive = [0 0 0 1];

mat(18).name = 'inst1_6';
mat(18).diffuse  = [0.000 0.000 0.000 1.000];
mat(18).ambient  = [0.000 0.000 0.000 1.000];
mat(18).specular = [0.700 0.700 0.700 1.000 100];
mat(18).emissive = [1.000 1.000 1.000 1.000];

mat(19).name = 'instrument';
mat(19).diffuse  = [0.000 0.000 0.000 1.000];
mat(19).ambient  = [0.000 0.000 0.000 1.000];
mat(19).specular = [0.500 0.500 0.500 1.000 100];
mat(19).emissive = [1.000 1.000 1.000 1.000];

mat(20).name = 'pnsgr1';
mat(20).diffuse  = [1.000 1.000 1.000 1.000];
mat(20).ambient  = [0.882 0.882 0.882 1.000];
mat(20).specular = [0.898 0.898 0.898 1.000];
mat(20).emissive = [0.800 0.800 0.800 1];

mat(21).name = 'psng4';
mat(21).diffuse  = [1.000 1.000 1.000 1.000];
mat(21).ambient  = [0.882 0.882 0.882 1.000];
mat(21).specular = [0.898 0.898 0.898 1.000];
mat(21).emissive = [0.800 0.800 0.800 1];

mat(22).name = 'psngr2';
mat(22).diffuse  = [1.000 1.000 1.000 1.000];
mat(22).ambient  = [0.882 0.882 0.882 1.000];
mat(22).specular = [0.898 0.898 0.898 1.000];
mat(22).emissive = [0.800 0.800 0.800 1];

mat(23).name = 'psngr3';
mat(23).diffuse  = [1.000 1.000 1.000 1.000];
mat(23).ambient  = [0.882 0.882 0.882 1.000];
mat(23).specular = [0.898 0.898 0.898 1.000];
mat(23).emissive = [0.800 0.800 0.800 1];

mat(24).name = 'cockpitglass';
mat(24).diffuse  = [0.235 0.235 0.235 0.150];
mat(24).ambient  = [0.235 0.235 0.235 0.150];
mat(24).specular = [1.000 1.000 1.000 1.000 100];
mat(24).emissive = [0 0 0 1];

mat(25).name = 'visor';
mat(25).diffuse  = [0.365 0.302 0.114 0.500];
mat(25).ambient  = [0.855 0.733 0.400 0.500];
mat(25).specular = [0.620 0.635 0.949 0.500 40];
mat(25).emissive = [0.800 0.800 0.800 1];

mat(26).name = 'HUD_glass';
mat(26).diffuse  = [0.204 0.329 0.106 0.150];
mat(26).ambient  = [0.204 0.329 0.106 1];
mat(26).specular = [0.851 0.859 0.784 1 5];
mat(26).emissive = [0.375 0.75 0.375 0.150];

mat(27).name = 'black';
mat(27).diffuse  = [0 0 0 1];
mat(27).ambient  = [0 0 0 1];
mat(27).specular = [0 0 0 1];
mat(27).emissive = [0 0 0 0];

mat(28).name = 'shinysurf';
mat(28).diffuse  = [1.0 1.0 1.0 1.0];
mat(28).ambient  = [1.0 1.0 1.0 1.0];
mat(28).specular = [0.5 0.5 0.5 1.0 50.0];
mat(28).emissive = [0.15 0.15 0.15 1];

mat(29).name = 'LIT_SURF';
mat(29).diffuse = [0 0 0 0];
mat(29).ambient = [0.3 0.3 0.3 1];
mat(29).specular = [0.3 0.3 0.3 1 100];
mat(29).emissive = [0.8 0.8 0.8 1];

% ========================================================================
% Texture section
% ========================================================================
tex(1).name = 'DG\\DGMK4_4.dds';
tex(2).name = 'DG\\DGMK4_2.dds';
tex(3).name = 'DG\\DGMK4_3.dds';
tex(4).name = 'DG\\PSNGR2.dds';
tex(5).name = 'DG\\PSNGR1.dds';
tex(6).name = 'DG\\PSNGR3.dds';
tex(7).name = 'DG\\PSNGR4.dds';
tex(8).name = 'DG\\DGPILOT1.dds';
tex(9).name = 'DG\\DGIP_01.dds';
tex(10).name = 'DG\\DGIP_04.dds';
tex(11).name = 'DG\\dg_vc5.dds';
tex(12).name = 'DG\\DGIP_03.dds';
tex(13).name = 'DG\\DGIP_05.dds';
tex(14).name = 'DG\\blittgt1.dds D';
tex(15).name = 'DG\\DG_VC2.dds';
tex(16).name = 'DG\\DG_VC1.dds D';
tex(17).name = 'generic\\noise1.dds';
tex(18).name = 'DG\\DG_VC3.dds D';
tex(19).name = 'DG\\dg_instr.dds';
tex(20).name = 'DG\\dg_vc4.dds';

% ========================================================================
% Copy the first part of the VC definition (everything except cockpit
% panels and HUD)
% ========================================================================
g.mat_used = zeros(length(mat),1);
g.tex_used = zeros(length(tex),1);

fid1 = fopen('deltaglider_vc_part1.msh','rt');
while 1
    tline = fgets(fid1);
    if ~ischar(tline), break, end
    if strncmpi(tline,'GROUPS',6)
        g.ngroup_expected = sscanf(tline(7:end),'%d');
    elseif strncmpi(tline,'GEOM',4)
        g.ngroup = g.ngroup+1;
        n = sscanf(tline(5:end),'%d');
        g.nvtx = g.nvtx + n(1);
    elseif strncmpi(tline,'MATERIAL',8)
        m = sscanf(tline(9:end),'%d');
        if m > 0
            g.mat_used(m) = 1;
        end
    elseif strncmpi(tline,'TEXTURE',7)
        m = sscanf(tline(8:end),'%d');
        if m > 0
            g.tex_used(m) = 1;
        end
    end
    fprintf(fid,tline);
end
fclose(fid1);

% ========================================================================
% Computation section: generate vertices
% ========================================================================
vtx = [];
idx = [];
fprintf('\n');

% Make everything
[meshlist,prms] = make_vc(vc);
writeh_prms(fhd,prms);

% generate the left MFD
[mvtx,midx] = make_mfd (vc.main.z1,vc.main.y1,vc.main.mfd,-1);
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);

% generate the right MFD
[mvtx,midx] = make_mfd (vc.main.z1,vc.main.y1,vc.main.mfd,1);
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);

% Generate the artificial horizon
[mvtx,midx] = make_mfd (vc.main.z1,vc.main.y1,vc.main.hor,0);
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);

% Generate heading indicator
%[mvtx,midx] = make_mfd (vc.main.z1,vc.main.y1,vc.main.hdg,0);
%[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);

% Tilt the entire main panel around its top edge
vtx = tilt_main_panel(vtx,vc.main);

% HUD projector casing
[mvtx,midx] = make_hud_projector(hud);
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);

% Side wall for left side panels
[mvtx,midx] = make_lside_wall(lslope,lside);
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);

% right side panel background
[mvtx,midx] = make_rside_panel(rside);
mvtx = tilt_side_panel(mvtx,rside);
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);

% Side wall for right side panels
[mvtx,midx] = make_rside_wall(rslope,rside);
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);

write_group (fid, vtx,idx,6,20,'VC_INTERIOR');

% Artificial horizon display
[vtx,idx] = horizon('VC',vc.main.hor.view);
write_group (fid, vtx,idx,19,19,'HORIZON');

% Heading indicator display
%[vtx,idx] = hsi_disp('VC',vc.main.hdg.view);
%write_group (fid, vtx,idx,19,19,'HSI');

% HUD projector lens
[vtx,idx] = make_hud_lens(hud);
write_group (fid, vtx,idx,8,0,'HUD_LENS');

% Generate left MFD display (in new group)
[vtx,idx] = make_mfd_disp (vc.main.z1,vc.main.y1,vc.main.mfd,true);
vtx = tilt_main_panel(vtx,vc.main);
write_group (fid, vtx,idx,19,0,'LMFD_DISPLAY');

% Generate right MFD display (in new group)
[vtx,idx] = make_mfd_disp (vc.main.z1,vc.main.y1,vc.main.mfd,false);
vtx = tilt_main_panel(vtx,vc.main);
write_group (fid, vtx,idx,19,0,'RMFD_DISPLAY');

% Generate left MFD buttons (new group each)
vtx = [];
idx = [];
for i=0:5
    [mvtx,midx] = make_mfd_button(vc.main.z1,vc.main.y1,vc.main.mfd,true,0,i);
    [vtx,idx] = add_to_group(vtx,idx,mvtx,midx);
end
vtx = tilt_main_panel(vtx,vc.main);
write_group(fid, vtx,idx,1,20,'LMFD_LBUTTONS');
vtx = [];
idx = [];
for i=0:5
    [mvtx,midx] = make_mfd_button(vc.main.z1,vc.main.y1,vc.main.mfd,true,1,i);
    [vtx,idx] = add_to_group(vtx,idx,mvtx,midx);
end
vtx = tilt_main_panel(vtx,vc.main);
write_group(fid, vtx,idx,1,20,'LMFD_RBUTTONS');
vtx = [];
idx = [];
for i=0:1
    [mvtx,midx] = make_mfd_button(vc.main.z1,vc.main.y1,vc.main.mfd,true,2,i);
    [vtx,idx] = add_to_group(vtx,idx,mvtx,midx);
end
vtx = tilt_main_panel(vtx,vc.main);
write_group(fid, vtx,idx,1,20,'LMFD_BBUTTONS');

% Generate right MFD buttons (new group each)
vtx = [];
idx = [];
for i=0:5
    [mvtx,midx] = make_mfd_button(vc.main.z1,vc.main.y1,vc.main.mfd,false,0,i);
    [vtx,idx] = add_to_group(vtx,idx,mvtx,midx);
end
vtx = tilt_main_panel(vtx,vc.main);
write_group(fid, vtx,idx,1,20,'RMFD_LBUTTONS');
vtx = [];
idx = [];
for i=0:5
    [mvtx,midx] = make_mfd_button(vc.main.z1,vc.main.y1,vc.main.mfd,false,1,i);
    [vtx,idx] = add_to_group(vtx,idx,mvtx,midx);
end
vtx = tilt_main_panel(vtx,vc.main);
write_group(fid, vtx,idx,1,20,'RMFD_RBUTTONS');
vtx = [];
idx = [];
for i=0:1
    [mvtx,midx] = make_mfd_button(vc.main.z1,vc.main.y1,vc.main.mfd,false,2,i);
    [vtx,idx] = add_to_group(vtx,idx,mvtx,midx);
end
vtx = tilt_main_panel(vtx,vc.main);
write_group(fid, vtx,idx,1,20,'RMFD_BBUTTONS');

% Generate scram temperature display on overhead panel
[vtx,idx] = make_scramtemp (vc.over);
vtx = tilt_overhead_panel(vtx,vc.over);
write_group(fid,vtx,idx,18,18,'SCRAM_TEMP');

% Generate status indicators on right side panel
[vtx,idx] = make_statusind(rside);
vtx = tilt_side_panel(vtx,rside);
write_group(fid,vtx,idx,19,15,'STATUS_INDICATOR');

% Generate HUD retraction rail
[vtx,idx] = make_hud_rail(hud);
write_group(fid, vtx, idx, 6, 0, 'HUD_RAIL');

% Generate HUD frame
[vtx,idx] = make_hud_frame(hud);
write_group(fid, vtx, idx, 6, 0, 'HUD_FRAME');

% Compact groups together
meshlist = compact_mesh(meshlist);

% Write new-style groups
write_groups(fid,meshlist,group);

% Generate HUD pane
[vtx,idx] = make_hud_pane(hud);
write_group(fid, vtx, idx, 26, 0, 'HUD_PANE');

% Generate glass surfaces
[vtx,idx] = make_statusind_glass (rside);
vtx = tilt_side_panel(vtx,rside);
write_group(fid, vtx, idx, 26, 0, 'GLASS_SURF');

% Generate logical surface for HUD display
[vtx,idx] = make_hudsurf (hud);
write_group(fid, vtx, idx, 0, 0, 'HUDDISP', 7);

% Write material specifications
write_materials (fid,mat);

% Write texture list
write_textures (fid,tex);

fclose(fid);
writeh_string(fhd, '#endif // !__DG_VC_ANIM_H');
fclose(fhd);

disp(['Groups written: ' num2str(g.ngroup)]);
disp(['Vertices written: ' num2str(g.nvtx)]);
disp(['Materials written: ' num2str(length(mat))]);
disp(['Textures written: ' num2str(length(tex))]);

if g.ngroup ~= g.ngroup_expected
    warning('Wrote %d groups, but header says %d:\nFix manually in deltaglider_vc_part1.msh and run again', ...
        g.ngroup, g.ngroup_expected);
end
for i=1:length(mat)
    if g.mat_used(i) == 0
        warning('Material %d is not used by any group', i);
    end
end
for i=1:length(tex)
    if g.tex_used(i) == 0
        warning('Texture %d is not used by any group', i);
    end
end

end

% =========================================================================
% Shift all vertices in a list by a common vector
function vtx = shiftvtx(vtx,disp)
for i=1:size(vtx,1)
    vtx(i,1:3) = vtx(i,1:3) + disp;
end
end

% =========================================================================
% Tilt all vertices of the main panel around its top edge
function vtx = tilt_main_panel(vtx,prm)
vtx = tilt_x(vtx,prm.y1,prm.z1,prm.tilt);
end

% =========================================================================
% Tilt all vertices of the slope panels
function vtx = tilt_slope_panel(vtx,prm)
if isfield(prm,'yref')
    ry = prm.yref; % Obsolete
    rz = prm.zref;
else
    ry = prm.ref(2);
    rz = prm.ref(3);
end
vtx = tilt_x(vtx,ry,rz,prm.tilt);
end

% =========================================================================
% Tilt all vertices of the slope panels
function vtx = tilt_overhead_panel(vtx,prm)
vtx = tilt_x(vtx,prm.yref,prm.zref,prm.tilt);
end

% =========================================================================
% Tilt all vertices of the slope panels
function vtx = tilt_side_panel(vtx,prm)
if isfield(prm,'yref')
    ry = prm.yref; % Obsolete
    rz = prm.zref;
else
    ry = prm.ref(2);
    rz = prm.ref(3);
end
vtx = tilt_x(vtx,ry,rz,prm.tilt);
end

% =========================================================================
% Apply transformation to a mesh: shift and rotation
function meshlist = transform(T,meshlist)
if isfield(T,'tilt')
    R = [1,0,0;  0,cos(T.tilt),-sin(T.tilt); 0,sin(T.tilt),cos(T.tilt)];
end
for i=1:length(meshlist)
    if isfield(T,'tilt') % rotate around x-axis
        for j=1:size(meshlist(i).vtx,1)
            meshlist(i).vtx(j,1:3) = (R*meshlist(i).vtx(j,1:3)')';
            if size(meshlist(i).vtx,2) > 3
                meshlist(i).vtx(j,4:6) = (R*meshlist(i).vtx(j,4:6)')';
            end
        end
    end
    if isfield(T,'ref') % shift by reference vector
        for j=1:size(meshlist(i).vtx,1)
            meshlist(i).vtx(j,1:3) = meshlist(i).vtx(j,1:3)+T.ref;
        end
    end
end
end

% =========================================================================
% Apply transformation to a parameter set: shift and rotation
function prms = transform_prms(T,prms)
if isfield(T,'tilt')
    R = [1,0,0;  0,cos(T.tilt),-sin(T.tilt); 0,sin(T.tilt),cos(T.tilt)];
end
for i=1:length(prms)
    if isfield(T,'tilt')
        if isfield(prms{i},'mousearea')
            for j=1:size(prms{i}.mousearea,1)
                prms{i}.mousearea(j,1:3) = (R*prms{i}.mousearea(j,1:3)')';
            end
        end
        if isfield(prms{i},'mousepoint')
            for j=1:size(prms{i}.mousepoint,1)
                prms{i}.mousepoint(j,1:3) = (R*prms{i}.mousepoint(j,1:3)')';
            end
        end
        if isfield(prms{i},'axis')
            prms{i}.axis = (R*prms{i}.axis')';
        end
        if isfield(prms{i},'ref')
            prms{i}.ref = (R*prms{i}.ref')';
        end
    end
    if isfield(T,'ref')
        if isfield(prms{i},'mousearea')
            for j=1:size(prms{i}.mousearea,1)
                prms{i}.mousearea(j,1:3) = prms{i}.mousearea(j,1:3)+T.ref;
            end
        end
        if isfield(prms{i},'mousepoint')
            for j=1:size(prms{i}.mousepoint,1)
                prms{i}.mousepoint(j,1:3) = prms{i}.mousepoint(j,1:3)+T.ref;
            end
        end
        if isfield(prms{i},'ref')
            for j=1:size(prms{i}.ref,1)
                prms{i}.ref(j,:) = prms{i}.ref(j,:)+T.ref;
            end
        end
    end
end
end

% =========================================================================
% merge sublist into meshlist, combining any groups with identical names
function meshlist = meshmerge(meshlist,sublist)
meshlist = [meshlist, sublist];
end

% =========================================================================
% Merge all groups with identical names into a single group
function meshlist = compact_mesh(inlist)
global fhd;
meshlist = [];
for i=1:length(inlist)
    found = false;
    for j=1:length(meshlist)
        if strcmpi(meshlist(j).grpname, inlist(i).grpname)
            if ~isempty(inlist(i).name)
                writeh_int(fhd, ['VC_' inlist(i).name '_vofs'], size(meshlist(j).vtx,1));
            end
            meshlist(j) = add_to_mesh(meshlist(j),inlist(i));
            found = true;
            break;
        end
    end
    if found==false
        if ~isempty(inlist(i).name)
            writeh_int(fhd, ['VC_' inlist(i).name '_vofs'], 0);
        end
        if isempty(meshlist)
            meshlist = inlist(i);
        else
            meshlist(length(meshlist)+1) = inlist(i);
        end
    end
end
end

% =========================================================================
% add submesh to mesh
function mesh = add_to_mesh(mesh,submesh)
mesh.idx = [mesh.idx; submesh.idx + size(mesh.vtx,1)];
mesh.vtx = [mesh.vtx; submesh.vtx];
end

% =========================================================================
% write the entire mesh list to a file
function write_groups(fid,meshlist,order)
for i=1:length(meshlist)
    meshlist(i).written = false;
end
% write out groups in prescribed order
for g=1:length(order)
    for i=1:length(meshlist)
        if strcmpi(order(g).name,meshlist(i).grpname)
            write_grp(fid,order(g),meshlist(i));
            meshlist(i).written = true;
        end
    end
end
% write out any remaining unordered groups
for i=1:length(meshlist)
    if meshlist(i).written == false
        write_grp(fid,struct('mat',0,'tex',0),meshlist(i));
        meshlist(i).written = true;
    end
end
end

% =========================================================================
% Write an individual mesh group to a file
function write_grp(fid,prm,mesh)
global g;
nvtx = size(mesh.vtx,1);
nn   = size(mesh.vtx,2);
nidx = size(mesh.idx,1);
fprintf(fid, 'MATERIAL %d\n', prm.mat);
fprintf(fid, 'TEXTURE %d\n', prm.tex);
if isfield(prm,'flag')
    fprintf(fid, 'FLAG %d\n', prm.flag);
end
fprintf(fid, 'LABEL %s\n', mesh.grpname);
fprintf(fid, 'GEOM %d %d\n', nvtx, nidx);
for i=1:nvtx
    for j=1:nn
        fprintf(fid, '%0.5f', mesh.vtx(i,j));
        if j==nn
            fprintf(fid, '\n');
        else
            fprintf(fid, ' ');
        end
    end
end
for i=1:nidx
    fprintf(fid, '%d %d %d\n', mesh.idx(i,1), mesh.idx(i,2), mesh.idx(i,3));
end

% update statistics
if isfield(g,'ngroup')
    g.ngroup = g.ngroup+1;
else
    g.ngroup = 1;
end
if isfield(g,'nvtx')
    g.nvtx = g.nvtx + nvtx;
else
    g.nvtx = nvtx;
end
if prm.mat > 0
    g.mat_used(prm.mat) = 1;
end
if prm.tex > 0
    g.tex_used(prm.tex) = 1;
end
end

% =========================================================================
% =========================================================================
% Top-level mesh generator: make the entire VC
% =========================================================================
% =========================================================================

function [meshlist,prms] = make_vc(this)
% main panel
[meshlist,prms] = make_mainpanel(this.main);

% lower main panel section
[sublist,subprms] = make_lower(this.lower);
meshlist = meshmerge(meshlist,sublist);
prms = [prms; subprms];

% dash panel
[sublist,subprms] = make_dash(this.dash);
meshlist = meshmerge(meshlist,sublist);
prms = [prms; subprms];

% overhead panel
[sublist,subprms] = make_over(this.over);
meshlist = meshmerge(meshlist,sublist);
prms = [prms; subprms];
end

% =========================================================================
% =========================================================================
% Level 1 components
% =========================================================================
% =========================================================================

% -------------------------------------------------------------------------
% Main panel (housing MFDs, horizon, left and right subpanels)
% -------------------------------------------------------------------------

function [meshlist,prms] = make_mainpanel(this)
% make mainpanel background
mfd = this.mfd;
hor = this.hor;
hdg = this.hdg;
z = 0;
y1 = 0;
y0 = y1 - this.h;
ym = y0 + this.h_cnt;
x0 = -mfd.cnt_dst-mfd.corner;
x1 = -x0;
X0 = -hor.w/2+hor.corner;
X1 = -X0;
Y1 = y1 - hor.top_dst - hor.corner;
Y0 = y1 - hor.top_dst - hor.h+hor.corner;
Yt1 = this.aoa.ref(2)+this.aoa.h/2-0.002;
Yt0 = this.aoa.ref(2)-this.aoa.h/2+0.002;
Xt0 = this.aoa.ref(1)+this.aoa.w/2-0.002;
Xt1 = -Xt0;
%Yh = y1 - hdg.top_dst - hdg.corner;
Ym = y1 - mfd.h - mfd.top_dst + mfd.corner;
X0m = x0-mfd.w+2*mfd.corner;
X1m = - X0m;
t0 = texcrd(48.5);
t1 = texcrd(95.5);
t2 = (ym-y0)/(y1-y0)*(t1-t0) + t0;
u = texcrd(134);
vtx = [x0,y0,z,   0,0,-1, 0,u; ...
       x1,y0,z,   0,0,-1, 0,u; ...
       x0,y1,z,   0,0,-1, 0,u; ...
       x1,y1,z,   0,0,-1, 0,u; ...
       X0,Y0,z,   0,0,-1, 0,u; ...
       X1,Y0,z,   0,0,-1, 0,u; ...
       X0,Y1,z,   0,0,-1, 0,u; ...
       X1,Y1,z,   0,0,-1, 0,u; ...
       X0,Yt0,z,  0,0,-1, 0,u; ...
       Xt0,Yt0,z, 0,0,-1, 0,u; ...
       Xt1,Yt0,z, 0,0,-1, 0,u; ...
       X1,Yt0,z,  0,0,-1, 0,u; ...
       X0,Yt1,z,  0,0,-1, 0,u; ...
       Xt0,Yt1,z, 0,0,-1, 0,u; ...
       Xt1,Yt1,z, 0,0,-1, 0,u; ...
       X1,Yt1,z,  0,0,-1, 0,u];
idx = [0,12,8; 0,8,9; 0,9,1; 1,9,10; 1,10,11; 1,11,15; 9,13,10; 10,13,14; ...
       0,4,12; 1,15,5; 12,4,15; 15,4,5; 0,2,4; 1,5,3; 2,6,4; 3,5,7; ...
       2,7,6; 2,3,7];
for i=1:size(vtx,1)
    vtx(i,7) = t0+(t1-t0)*(vtx(i,2)-y0)/(y1-y0);
end

%T0 = t0 + (t1-t0)/(y1-y0)*(Y0-y0);
%T1 = t0 + (t1-t0)/(y1-y0)*(Y1-y0);
%Th = t0 + (t1-t0)/(y1-y0)*(Yh-y0);
%vtx = [x0,y0,z,  0,0,-1, t0,u; ...  % middle part around artifical horizon and hsi
%    x1,y0,z,  0,0,-1, t0,u; ...
%    x0,y1,z,  0,0,-1, t1,u; ...
%    x1,y1,z,  0,0,-1, t1,u; ...
%    X0,Y0,z,  0,0,-1, T0,u; ...
%    X1,Y0,z,  0,0,-1, T0,u; ...
%    X0,Y1,z,  0,0,-1, T1,u; ...
%    X1,Y1,z,  0,0,-1, T1,u; ...
%    X0,Yh,z,  0,0,-1, Th,u; ...
%    X1,Yh,z,  0,0,-1, Th,u; ...
%    X0,y0,z,  0,0,-1, t0,u; ...
%    X1,y0,z,  0,0,-1, t0,u];
%idx = [0,2,6; 0,6,10; 3,1,7; 1,11,7; 3,7,6; 3,6,2; 4,5,8; 8,5,9];

mvtx = [X0m,y0,z, 0,0,-1, t0,u; ...  % below left MFD
    x0,y0,z, 0,0,-1, t0,u; ...
    X0m,Ym,z, 0,0,-1, t0,u; ...
    x0,Ym,z, 0,0,-1, t0,u];
midx = [0,2,1; 2,3,1];
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);

mvtx = [X1m,y0,z, 0,0,-1, t0,u; ...  % below right MFD
    x1,y0,z,  0,0,-1, t0,u; ...
    X1m,Ym,z, 0,0,-1, t0,u; ...
    x1,Ym,z,  0,0,-1, t0,u];
midx = [0,1,2; 2,1,3];
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);

cy1 = 0.02;
cy2 = 0.025;
xt = this.w_top/2;
xb = this.w_bottom/2;
xc = this.w_cnt/2;
cx1 = xt + (xt-xb)/this.h * cy1;
cx2 = xt - 0.03;
us = texcrd(142);
ts1 = texcrd(48);
ts2 = texcrd(48);
mvtx = [X0m,y1,z,  0,0,-1,  t1,u; ... % left of left MFD
    X0m,y0,z,  0,0,-1,  t0,u; ...
    -xb,y0,z, 0,0,-1,   t0,u; ...
    -xt,y1,z, 0,0,-1,   t1,u; ...
    -xc,ym,z, 0,0,-1,   t2,u; ...
    -cx1,cy1,z, 0,0,-1, t1,u; ...
    -cx2,cy2,z, 0,0,-1, t1,u; ...
    -cx2,y1,z,  0,0,-1, t1,u; ...

    -xb,y0,z,      1,0,0,   ts2,us; ... % connect to dashboard edge
    -xc,ym,z,      1,0,0,   ts1,us; ...
    -xc-0.011,ym-0.01,z-0.053, 1,0,0,   ts1,us];
midx = [2,4,1; 1,4,0; 4,3,0; 3,5,7; 7,5,6; 8,10,9];
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);

mvtx = [X1m,y1,z,  0,0,-1, t1,u; ... % right of right MFD
    X1m,y0,z,  0,0,-1, t0,u; ...
    xb,y0,z, 0,0,-1, t0,u; ...
    xt,y1,z, 0,0,-1, t1,u; ...
    xc,ym,z, 0,0,-1, t2,u; ...
    cx1,cy1,z, 0,0,-1, t1,u; ...
    cx2,cy2,z, 0,0,-1, t1,u; ...
    cx2,y1,z,  0,0,-1, t1,u; ...

    xb,y0,z,      -1,0,0,   ts2,us; ... % connect to dashboard edge
    xc,ym,z,      -1,0,0,   ts1,us; ...
    xc+0.011,ym-0.01,z-0.053, -1,0,0,   ts1,us];
midx = [2,1,4; 1,0,4; 4,0,3; 3,7,5; 7,6,5; 8,9,10];
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);
meshlist(1) = struct('vtx',vtx, 'idx',idx, 'grpname','VC4_DEFAULT', 'name','');
prms = {};

% make children
[sublist,subprms] = make_aoatape(this.aoa);
meshlist = meshmerge(meshlist,sublist);
prms = [prms; subprms];

[sublist,subprms] = make_vstape(this.vs);
meshlist = meshmerge(meshlist,sublist);
prms = [prms; subprms];

[sublist,subprms] = make_mainpanel_lpanel(this.lpanel); % left panel
meshlist = meshmerge(meshlist,sublist);
prms = [prms; subprms];

[sublist,subprms] = make_mainpanel_rpanel(this.rpanel); % right panel
meshlist = meshmerge(meshlist,sublist);
prms = [prms; subprms];

% map into parent frame
meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);
end

% -------------------------------------------------------------------------
% Lower section of main panel
% -------------------------------------------------------------------------

function [meshlist,prms] = make_lower(this)
meshlist = [];
prms = {};

% make children
[sublist,subprms] = make_lower_mid(this.mid);
meshlist = meshmerge(meshlist,sublist);
prms = [prms; subprms];

[sublist,subprms] = make_lower_left(this.left);
meshlist = meshmerge(meshlist,sublist);
prms = [prms; subprms];

[sublist,subprms] = make_lower_right(this.right);
meshlist = meshmerge(meshlist,sublist);
prms = [prms; subprms];

% map into parent frame
meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);
end

function [meshlist,prms] = make_lower_mid(this)
psd = this.psd;
hdg = this.hdg;
x0 = -this.w/2;
x1 = x0 + this.w;
X0 = psd.ref(1);
X1 = X0 + psd.frame.w;
X0h = hdg.ref(1)-hdg.w/2+hdg.lr_frmw;
X1h = X0h + hdg.w-2*hdg.lr_frmw;
y1 = 0;
y0 = y1 - this.h;
z0 = 0;
u1 = texcrd(71);
u0 = texcrd(71);
v = texcrd(142);
vtx = [X1,y1,z0, 0,0,-1, u1,v; ... % the part right of the propellant status display
    x1,y1,z0, 0,0,-1, u1,v; ...
    X1,y0,z0, 0,0,-1, u0,v; ...
    x1-this.corner_w,y0,z0, 0,0,-1, u0,v; ...
    x1,y0-this.corner_h,z0, 0,0,-1, u0,v; ...

    x0,y1,z0, 0,0,-1, u1,v; ... % the part left of the HSI display
    x0,y0-this.corner_h,z0, 0,0,-1, u0,v; ...
    x0+this.corner_w,y0,z0, 0,0,-1, u0,v; ...
    X0h,y0,z0, 0,0,-1, u0,v; ...
    X0h,y1,z0, 0,0,-1, u1,v; ...
    
    X1h,y0,z0, 0,0,-1, u0,v; ... % the part between HSI and propellant status display
    X0, y0,z0, 0,0,-1, u0,v; ...
    X1h,y1,z0, 0,0,-1, u1,v; ...
    X0, y1,z0, 0,0,-1, u1,v];
idx = [0,1,2; 2,1,4; 2,4,3; 5,7,6; 5,9,7; 7,9,8; 10,12,11; 12,13,11];
meshlist = struct('vtx',vtx, 'idx',idx, 'grpname','VC4_DEFAULT','name','');
prms = {};

% make children
[sublist,subprms] = make_hsi (this.hdg);
meshlist = meshmerge(meshlist,sublist);
prms = [prms; subprms];

sublist = make_psd (this.psd);
meshlist = meshmerge(meshlist,sublist);

[sublist,subprms] = make_gimbalpanel(this.gimbalpanel);
meshlist = meshmerge(meshlist,sublist);
prms = [prms; subprms];

% map into parent frame
meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);
end

% -------------------------------------------------------------------------

function [meshlist,prms] = make_lower_left(this)
meshlist = [];
prms = {};

[sublist,subprms] = make_lslopepanel(this.panel);
meshlist = meshmerge(meshlist,sublist);
prms = [prms; subprms];

% map into parent frame
meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);
end

% -------------------------------------------------------------------------

function [meshlist,prms] = make_lower_right(this)
meshlist = [];
prms = {};

[sublist,subprms] = make_rslopepanel(this.panel);
meshlist = meshmerge(meshlist,sublist);
prms = [prms; subprms];

% map into parent frame
meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);
end

% -------------------------------------------------------------------------
% Dashboard panel (housing HUD and NAVMODE buttons, RCS mode dial)
% -------------------------------------------------------------------------

function [meshlist,prms] = make_dash(this)
% Dashboard panel background
z = 0;
y0 = 0;
x0 = -this.w/2;
x1 = -x0;
v = texcrd(142);
u0 = texcrd(0.5);
vtx = [x0,y0,z, 0,-1,0, u0,v; ...
    x1,y0,z, 0,-1,0, u0,v; ...
    x0,this.ledge.y,this.ledge.z, 0,-1,0, u0,v; ...  % connect to main panel
    x1,this.ledge.y,this.ledge.z, 0,-1,0, u0,v];
idx = [0,1,2; 2,1,3];
meshlist(1) = struct('vtx',vtx, 'idx',idx, 'grpname','VC4_DEFAULT', 'name','');
prms = {};

% make children
[sublist,subprms] = make_dash_panel(this.panel); % panel surface
meshlist = meshmerge(meshlist,sublist);
prms = [prms; subprms];

[sublist,subprms] = make_dash_glareshield(this.glare); % glare shield
meshlist = meshmerge(meshlist,sublist);
prms = [prms; subprms];

% map into parent frame
meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);
end

% -------------------------------------------------------------------------
% Overhead panel
% -------------------------------------------------------------------------

function [meshlist,prms] = make_over(this)
proj.corner = 0.015;
panel.thickness = 0.005;
x0 = -this.w/2;
x1 = -x0;
y1 = this.h;
X0 = -0.043;
X1 = -X0;
X0a = -0.03;
X1a = -X0a;
Y0 = 0.055;
Y0a = 0.08;
Y0b = 0.095;
z1 = panel.thickness;
v = texcrd(142);
u0 = texcrd(12);
u1 = texcrd(1);
vtx = [x0,0,0, 0,0,-1, u0,v; ...
    x1,0,0, 0,0,-1, u0,v; ...
    x0,y1,0, 0,0,-1, u0,v; ...
    x1,y1,0, 0,0,-1, u0,v; ...
    X0-proj.corner,0,0,  0,0,-1, u0,v; ...
    X1+proj.corner,0,0,  0,0,-1, u0,v; ...
    X0,Y0,0, 0,0,-1, u0,v; ...
    X1,Y0,0, 0,0,-1, u0,v; ...
    X0,proj.corner,0, 0,0,-1, u0,v; ...
    X1,proj.corner,0, 0,0,-1, u0,v; ...
    X0a,Y0a,0,         0,0,-1, u0,v; ...
    X1a,Y0a,0,         0,0,-1, u0,v; ...
    0,Y0b,0,           0,0,-1, u0,v; ...
    X0,proj.corner,0,  1,0,0, u1,v; ...
    X0,Y0,0,           1,0,0, u1,v; ...
    X0,proj.corner,z1, 1,0,0, u1,v; ...
    X0,Y0,z1,           1,0,0, u1,v; ...
    X1,proj.corner,0, -1,0,0, u1,v; ...
    X1,Y0,0,           -1,0,0, u1,v; ...
    X1,proj.corner,z1, -1,0,0, u1,v; ...
    X1,Y0,z1,           -1,0,0, u1,v];
idx = [0,8,4; 1,5,9; 0,6,8; 1,9,7; 0,2,6; 1,7,3; 2,10,6; 3,7,11; 3,11,12; 2,12,10; 3,12,2; ...
    13,14,15; 15,14,16; 17,19,18; 18,19,20];

meshlist(1) = struct('vtx',vtx, 'idx',idx, 'grpname','VC4_DEFAULT', 'name','');
prms = {};

% make children
sublist = make_over_lightcover(this.light,'right');
meshlist = meshmerge(meshlist,sublist);

sublist = make_over_lightcover(this.light,'left');
meshlist = meshmerge(meshlist,sublist);

[sublist,subprms] = make_over_lfpanel(this.lfpanel);
meshlist = meshmerge(meshlist,sublist);
prms = [prms; subprms];

[sublist,subprms] = make_over_rfpanel(this.rfpanel);
meshlist = meshmerge(meshlist,sublist);
prms = [prms; subprms];

[sublist,subprms] = make_over_lmpanel(this.lmpanel);
meshlist = meshmerge(meshlist,sublist);
prms = [prms; subprms];

[sublist,subprms] = make_over_rmpanel(this.rmpanel);
meshlist = meshmerge(meshlist,sublist);
prms = [prms; subprms];

% map into parent frame
meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);
end

% =========================================================================
% =========================================================================
% Level 2 components
% =========================================================================
% =========================================================================

% -------------------------------------------------------------------------
% Dashboard panel
% -------------------------------------------------------------------------

function [meshlist,prms] = make_dash_panel(this)
global g;
x0 = -this.w/2;
x1 = -x0;
X0 = -this.w_top/2;
X1 = -X0;
y0 = 0;
y1 = this.h;
Y0 = this.h_edge;
z0 = 0;
z1 = this.edge;
d = this.edge;
vtx = [x0,y0,z0, 0,0,-1, 0,0; ...
    x1,y0,z0, 0,0,-1, 0,0; ...
    x0,Y0,z0, 0,0,-1, 0,0; ...
    x1,Y0,z0, 0,0,-1, 0,0; ...
    X0,y1,z0, 0,0,-1, 0,0; ...
    X1,y1,z0, 0,0,-1, 0,0; ...

    x0,y0-d,z1, 0,-1,0, 0,0; ... % outer rim
    x1,y0-d,z1, 0,-1,0, 0,0; ...
    x1+d,y0,z1, 1,0,0,  0,0; ...
    x1+d,Y0,z1, 1,0,0,  0,0; ...
    x0-d,y0,z1, -1,0,0, 0,0; ...
    x0-d,Y0,z1, -1,0,0, 0,0; ...
    X0,y1+d,z1, 0,1,0,  0,0; ...
    X1,y1+d,z1, 0,1,0,  0,0; ...
    x0,Y0+d,z1, 0,1,0,  0,0; ...
    x1,Y0+d,z1, 0,1,0,  0,0];

idx = [0,2,1; 1,2,3; 3,2,4; 3,4,5; ...
    0,1,7; 0,7,6; 1,3,9; 1,9,8; 7,1,8; 2,0,10; 2,10,11; 0,6,10; 4,12,5; 12,13,5; 2,14,4; 14,12,4; 11,14,2; 5,15,3; 5,13,15; 3,15,9];

for i=1:size(vtx,1)
    vtx(i,7) = (vtx(i,1)-x0)/0.1;
    vtx(i,8) = (vtx(i,2)-y0)/0.1;
end

meshlist(1) = struct('vtx',vtx, 'idx',idx, 'grpname','PANELS1', 'name','');

% shadow layer
vtx = growmesh(vtx,-g.panelshadow_ofs);
u = texcrd(1022);
v0 = texcrd(503);
v1 = texcrd(350);
for i=1:size(vtx,1)
    vtx(i,7) = u;
    vtx(i,8) = (vtx(i,2)-y0)/(y1-y0)*(v1-v0) + v0;
end
meshlist(2) = struct('vtx',vtx, 'idx',idx, 'grpname','PANELSHADOW', 'name','');

prms = {};

% make children
[sublist,subprms] = make_rcsdial(this.rcsdial);
meshlist = meshmerge(meshlist, sublist);
prms = [prms; subprms];

sublist = make_rcslabel(this.rcslabel);
meshlist = meshmerge(meshlist, sublist);

[sublist,subprms] = make_hudctrl(this.hud);
meshlist = meshmerge(meshlist, sublist);
prms = [prms; subprms];

[sublist,subprms] = make_rcsprog(this.navbtns);
meshlist = meshmerge(meshlist,sublist);
prms = [prms; subprms];

% map into parent frame
meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);
end

% -------------------------------------------------------------------------
% Dashboard glare shield
% -------------------------------------------------------------------------

function [meshlist,prms] = make_dash_glareshield(this)
z0 = 0.02;
z1 = -this.depth;
z0a = 0.055;
z1a = z1 + 0.01;
z2a = z1 + 0.04;
y1 = 0;
Y1 = y1 + this.thickness;
x0 = -this.w_top/2;
x1 = -x0;
x0a = -this.w_mid/2;
x1a = -x0a;
v = texcrd(139);
u0 = texcrd(49);
u1 = texcrd(73);
u2 = texcrd(96);

y1a = this.y_mid;
Y1a = y1a + this.thickness;
vtx = [x0,y1,z0, 0,-1,0, u2,v; ...
    x1,y1,z0, 0,-1,0, u2,v; ...
    x0,y1,z1, 0,-1,0, u1,v; ...
    x1,y1,z1, 0,-1,0, u1,v; ...
    x0,Y1,z1, 0,1,0,  u0,v; ...
    x1,Y1,z1, 0,1,0,  u0,v; ...
    x0,Y1,z0, 0,1,0,  u0,v; ...
    x1,Y1,z0, 0,1,0,  u0,v; ...
    x0,y1,z1, 0,0,-1, u1,v; ...
    x1,y1,z1, 0,0,-1, u1,v; ...

    x0a,y1a,z0a,0.3,-1,0, u2,v; ...
    x0,y1,z0,   0.3,-1,0, u2,v; ...
    x0a,y1a,z1a,0.3,-1,0, u1,v; ...
    x0,y1,z1,   0.3,-1,0, u1,v; ...
    x0a,Y1a,z1a,-0.3,1,0, u0,v; ...
    x0,Y1,z1,   -0.3,1,0, u0,v; ...
    x0a,Y1a,z0a,-0.3,1,0, u0,v; ...
    x0,Y1,z0,   -0.3,1,0, u0,v; ...
    x0a,y1a,z1a, 0,0,-1, u1,v; ...
    x0,y1,z1,    0,0,-1, u1,v; ...

    x1a,y1a,z0a,-0.3,-1,0, u2,v; ...
    x1,y1,z0,   -0.3,-1,0, u2,v; ...
    x1a,y1a,z1a -0.3,-1,0, u1,v; ...
    x1,y1,z1,   -0.3,-1,0, u1,v; ...
    x1a,Y1a,z1a 0.3,1,0,   u0,v; ...
    x1,Y1,z1,   0.3,1,0,   u0,v; ...
    x1a,Y1a,z0a,0.3,1,0,   u0,v; ...
    x1,Y1,z0,   0.3,1,0,   u0,v; ...
    x1a,y1a,z1a 0,0,-1, u1,v; ...
    x1,y1,z1,   0,0,-1, u1,v];
idx = [0,2,1; 1,2,3; 8,4,9; 9,4,5; 4,6,5; 5,6,7; ...
    10,12,11; 11,12,13; 18,14,19; 19,14,15; 14,16,15; 15,16,17; ...
    20,21,22; 21,23,22; 28,29,24; 29,25,24; 24,25,26; 25,27,26];

% left side part of the shield
alpha = this.sidetilt; sina = sin(alpha); cosa = cos(alpha);
x0 = this.thickness;
x1 = 0;
y0 = 0;
y1 = -this.sidelen;
dy = this.thickness*0.4;
dyu = -0.02;
mvtx = [x0,y0-dy,z1a,  1,0,0,  u1,v; ...
    x0,y1+dyu,z2a, 1,0,0,  u1,v; ...
    x1,y0,z1a,    -1,0,0,  u0,v; ...
    x1,y1+dyu,z2a,-1,0,0,  u0,v; ...
    x0,y0-dy,z0a,  1,0,0,  u2,v; ...
    x0,y1,z0a,     1,0,0,  u2,v; ...
    x1,y0,z0a,    -1,0,0,  u0,v; ...
    x1,y1,z0a,    -1,0,0,  u0,v; ...
    x0,y0-dy,z1a,  0,0,-1,  u1,v; ...
    x0,y1+dyu,z2a, 0,0,-1,  u1,v];
R = [cosa,sina,0;  -sina,cosa,0;  0,0,1];
for i=1:size(mvtx,1)
    mvtx(i,1:3) = (R*mvtx(i,1:3)')' + [x0a,Y1a,0];
    mvtx(i,4:6) = (R*mvtx(i,4:6)')';
end
midx = [8,9,3; 8,3,2; 0,4,1; 1,4,5; 2,3,7; 2,7,6];
[vtx,idx] = add_to_group (vtx,idx,mvtx,midx);

% right side part of the shield
mvtx = [-x0,y0-dy,z1a, -1,0,0, u1,v; ...
    -x0,y1+dyu,z2a,-1,0,0, u1,v; ...
    -x1,y0,z1a,     1,0,0, u0,v; ...
    -x1,y1+dyu,z2a  1,0,0, u0,v; ...
    -x0,y0-dy,z0a, -1,0,0, u2,v; ...
    -x0,y1,z0a,    -1,0,0, u2,v; ...
    -x1,y0,z0a,     1,0,0, u0,v; ...
    -x1,y1,z0a,     1,0,0, u0,v; ...
    -x0,y0-dy,z1a,  0,0,-1, u1,v; ...
    -x0,y1+dyu,z2a, 0,0,-1, u1,v];
R = [cosa,-sina,0;  sina,cosa,0;  0,0,1];
for i=1:size(mvtx,1)
    mvtx(i,1:3) = (R*mvtx(i,1:3)')' + [x1a,Y1a,0];
    mvtx(i,4:6) = (R*mvtx(i,4:6)')';
end
midx = [8,3,9; 8,2,3; 0,1,4; 1,5,4; 2,7,3; 2,6,7];
[vtx,idx] = add_to_group (vtx,idx,mvtx,midx);

meshlist(1) = struct('vtx',vtx, 'idx',idx, 'grpname','VC4_DEFAULT', 'name','');
prms = {};

% map into parent frame
meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);
end

% -------------------------------------------------------------------------
% Panel left of the left MFD
% -------------------------------------------------------------------------

function [meshlist,prms] = make_mainpanel_lpanel(this)
global g;
x0 = 0;
xb = -this.w_bottom;
xc = -this.w_cnt;
xt = -this.w_top;
y1 = 0;
y0 = -this.h;
yc = y0 + this.h_cnt;
z0 = 0;
z1 = 2*this.edge;
orim = this.edge;

vtx = [xb,y0,z0, 0,0,-1, 0,0; ...
    x0,y0,z0, 0,0,-1, 0,0; ...
    xt,y1,z0, 0,0,-1, 0,0; ...
    x0,y1,z0, 0,0,-1, 0,0; ...
    xc,yc,z0, 0,0,-1, 0,0; ...

    x0+orim,y0,z0+orim, 1,0,0,  0,0; ... % outer rim
    x0+orim,y1,z0+orim, 1,0,0,  0,0; ...
    x0,y1+orim,z0+orim, 0,1,0,  0,0; ...
    xt,y1+orim,z0+orim, 0,1,0,  0,0; ...
    x0,y0-orim,z0+orim, 0,-1,0, 0,0; ...
    xb,y0-orim,z0+orim, 0,-1,0, 0,0; ...
    xb-orim,y0,z0+orim, -1,0,0, 0,0; ...
    xt-orim,y1,z0+orim, -1,0,0, 0,0; ...
    xc-orim,yc,z0+orim, -1,0,0, 0,0; ...
    x0+orim,y0,z1,       1,0,0, 0,0; ...
    x0+orim,y1,z1,       1,0,0, 0,0; ...
    x0,y1+orim,z1,       0,1,0, 0,0; ...
    xt,y1+orim,z1,       0,1,0, 0,0];
idx = [1,2,3; 1,4,2; 1,0,4; ...
    1,3,5; 5,3,6; 3,2,7; 7,2,8; 3,7,6; 1,10,0; 1,9,10; 1,5,9; 0,11,13; 0,13,4; 0,10,11; 2,4,12; 12,4,13; 8,2,12; ...
    5,6,14; 14,6,15; 16,7,8; 16,8,17; 15,6,7; 15,7,16];

for i=1:size(vtx,1)
    vtx(i,7) = this.u0 + (this.ub-this.u0)/(xb-x0)*(vtx(i,1)-x0);
    vtx(i,8) = this.v0 + (this.v1-this.v0)/(y1-y0)*(vtx(i,2)-y0);
end

meshlist(1) = struct('vtx',vtx, 'idx',idx, 'grpname','PANELS1', 'name','');

% now make the shadow layer
u = texcrd(1022);
v0 = texcrd(503);
v1 = texcrd(300);
vtx = growmesh(vtx,-g.panelshadow_ofs);
for i=1:size(vtx,1)
    vtx(i,7) = u;
    vtx(i,8) = (vtx(i,2)-y0)/(y1-y0)*(v1-v0) + v0;
end
meshlist(2) = struct('vtx',vtx, 'idx',idx, 'grpname','PANELSHADOW', 'name','');

prms = {};

% make children
[sublist,subprms] = make_afdial(this.afdial);
meshlist = meshmerge(meshlist, sublist);
prms = [prms; subprms];

sublist = make_aflabel(this.aflabel);
meshlist = meshmerge(meshlist, sublist);

% map into parent frame
meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);
end

% -------------------------------------------------------------------------
% Panel right of the right MFD
% -------------------------------------------------------------------------

function [meshlist,prms] = make_mainpanel_rpanel(this)
global g;
avel = this.avel;
x0 = 0;
xb = this.w_bottom;
xc = this.w_cnt;
xt = this.w_top;
y1 = 0;
y0 = -this.h;
yc = y0 + this.h_cnt;
z0 = 0;
z1 = 2*this.edge;
Z0 = z0 + avel.depth;
orim = this.edge;
irim = avel.rim;
irim2 = irim/2;
X0 = x0 + avel.ref(1) - avel.w/2;
X1 = X0 + avel.w;
Y0 = y1 + avel.ref(2);
Y1 = Y0 + avel.h;
vtx = [xb,y0,z0, 0,0,-1, 0,0; ...
    x0,y0,z0, 0,0,-1, 0,0; ...
    xt,y1,z0, 0,0,-1, 0,0; ...
    x0,y1,z0, 0,0,-1, 0,0; ...
    xc,yc,z0, 0,0,-1, 0,0; ...

    x0-orim,y0,z0+orim, -1,0,0,  0,0; ... % outer rim
    x0-orim,y1,z0+orim, -1,0,0,  0,0; ...
    x0,y1+orim,z0+orim,  0,1,0,  0,0; ...
    xt,y1+orim,z0+orim,  0,1,0,  0,0; ...
    x0,y0-orim,z0+orim,  0,-1,0, 0,0; ...
    xb,y0-orim,z0+orim,  0,-1,0, 0,0; ...
    xb+orim,y0,z0+orim,  1,0,0, 0,0; ...
    xt+orim,y1,z0+orim,  1,0,0, 0,0; ...
    xc+orim,yc,z0+orim,  1,0,0, 0,0; ...
    x0-orim,y0,z1,       1,0,0, 0,0; ...
    x0-orim,y1,z1,       1,0,0, 0,0; ...
    x0,y1+orim,z1,       0,1,0, 0,0; ...
    xt,y1+orim,z1,       0,1,0, 0,0; ...

    X0,Y0-irim,z0, 0,0,-1, 0,0; ... % hole for angular rate indicators
    X1,Y0-irim,z0, 0,0,-1, 0,0; ...
    X0-irim,Y0,z0, 0,0,-1, 0,0; ...
    X1+irim,Y0,z0, 0,0,-1, 0,0; ...
    X0-irim,Y1,z0, 0,0,-1, 0,0; ...
    X1+irim,Y1,z0, 0,0,-1, 0,0; ...
    X0,Y1+irim,z0, 0,0,-1, 0,0; ...
    X1,Y1+irim,z0, 0,0,-1, 0,0; ...

    X0+irim2,Y0,Z0, 0,1,0, 0,0; ... % rim for angular rate display
    X1-irim2,Y0,Z0, 0,1,0, 0,0; ...
    X0,Y0+irim2,Z0, 1,0,0, 0,0; ...
    X1,Y0+irim2,Z0, -1,0,0, 0,0; ...
    X0,Y1-irim2,Z0, 1,0,0, 0,0; ...
    X1,Y1-irim2,Z0, -1,0,0, 0,0; ...
    X0+irim2,Y1,Z0, 0,-1,0, 0,0; ...
    X1-irim2,Y1,Z0, 0,-1,0, 0,0];
idx = [1,20,18; 1,18,19; 1,19,0; 0,19,21; 0,21,23; 0,23,4; 4,23,25; 4,25,2; 2,25,24; 2,24,3; 3,24,22; 1,3,22; 1,22,20; ...
    1,5,3; 5,6,3; 3,7,2; 7,8,2; 3,6,7; 1,0,10; 1,10,9; 1,9,5; 0,13,11; 0,4,13; 0,11,10; 2,12,4; 12,13,4; 8,12,2; ...
    5,14,6; 14,15,6; 16,8,7; 16,17,8; 15,7,6; 15,16,7; ...
    18,28,26; 18,20,28; 26,19,18; 26,27,19; 27,21,19; 27,29,21; 29,31,23; 29,23,21; 31,25,23; 31,33,25; 33,24,25; 33,32,24; 32,30,22; 32,22,24; 30,28,20; 30,20,22];

for i=1:size(vtx,1)
    vtx(i,7) = this.u0 + (this.ub-this.u0)/(xb-x0)*(vtx(i,1)-x0);
    vtx(i,8) = this.v0 + (this.v1-this.v0)/(y1-y0)*(vtx(i,2)-y0);
end

meshlist(1) = struct('vtx',vtx, 'idx',idx, 'grpname','PANELS1', 'name','');

% now make the shadow layer
u = texcrd(1022);
v0 = texcrd(503);
v1 = texcrd(300);
vtx = growmesh(vtx,-g.panelshadow_ofs);
for i=1:size(vtx,1)
    vtx(i,7) = u;
    vtx(i,8) = (vtx(i,2)-y0)/(y1-y0)*(v1-v0) + v0;
end
meshlist(2) = struct('vtx',vtx, 'idx',idx, 'grpname','PANELSHADOW', 'name','');

prms = {};

% make children
sublist = make_rpanel_angveldisp(this.avel);
meshlist = meshmerge(meshlist,sublist);

sublist = make_rpanel_angveldisp_label(this.avel_label);
meshlist = meshmerge(meshlist,sublist);

% map into parent frame
meshlist = transform(this,meshlist);
end

% -------------------------------------------------------------------------
% Front left overhead panel
% -------------------------------------------------------------------------

function [meshlist,prms] = make_over_lfpanel(this)
d = this.edge;
d2 = d/sqrt(2);
n2 = 1/sqrt(2);
x0 = 0;
x1 = x0 + this.w;
X0 = x1 - this.corner;
y1 = 0;
y0 = y1 - this.h;
Y0 = y0 + this.corner;
z0 = 0;
z1 = d;
vtx = [x0,y0,z0, 0,0,-1, 0,0; ...
    X0,y0,z0, 0,0,-1, 0,0; ...
    x1,Y0,z0, 0,0,-1, 0,0; ...
    x0,y1,z0, 0,0,-1, 0,0; ...
    x1,y1,z0, 0,0,-1, 0,0; ...

    x0,y1+d,z1, 0,1,0, 0,0; ... % outer rim
    x1,y1+d,z1, 0,1,0, 0,0; ...
    x1+d,y1,z1, 1,0,0, 0,0; ...
    x1+d,Y0,z1, 1,0,0, 0,0; ...
    x0-d,y1,z1, -1,0,0, 0,0; ...
    x0-d,y0,z1, -1,0,0, 0,0; ...
    x0,y0-d,z1, 0,-1,0, 0,0; ...
    X0,y0-d,z1, 0,-1,0, 0,0; ...
    X0+d2,y0-d2,z1, n2,-n2,0, 0,0; ...
    x1+d2,Y0-d2,z1, n2,-n2,0, 0,0];

idx = [0,3,1; 1,3,4; 1,4,2; ...
    4,3,5; 4,5,6; 4,7,2; 2,7,8; 4,6,7; 3,0,10; 3,10,9; 5,3,9; 0,1,12; 0,12,11; 0,11,10; 1,2,14; 1,14,13; 1,13,12; 2,8,14];

for i=1:size(vtx,1)
    vtx(i,7) = (vtx(i,1)-x0)/0.1;
    vtx(i,8) = (vtx(i,2)-y0)/0.1;
end

meshlist(1) = struct('vtx',vtx, 'idx',idx, 'grpname','PANELS1', 'name','');
prms = {};

% make children
[sublist,subprms] = make_over_lfpanel_light(this.light);
meshlist = meshmerge(meshlist, sublist);
prms = [prms; subprms];

% map into parent frame
meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);
end

% -------------------------------------------------------------------------
% Front right overhead panel
% -------------------------------------------------------------------------

function [meshlist,prms] = make_over_rfpanel(this)
d = this.edge;
d2 = d/sqrt(2);
n2 = 1/sqrt(2);
x0 = 0;
x1 = x0 - this.w;
X0 = x1 + this.corner;
y1 = 0;
y0 = y1 - this.h;
Y0 = y0 + this.corner;
z0 = 0;
z1 = d;
vtx = [x0,y0,z0, 0,0,-1, 0,0; ...
    X0,y0,z0, 0,0,-1, 0,0; ...
    x1,Y0,z0, 0,0,-1, 0,0; ...
    x0,y1,z0, 0,0,-1, 0,0; ...
    x1,y1,z0, 0,0,-1, 0,0; ...

    x0,y1+d,z1, 0,1,0, 0,0; ... % outer rim
    x1,y1+d,z1, 0,1,0, 0,0; ...
    x1-d,y1,z1, -1,0,0, 0,0; ...
    x1-d,Y0,z1, -1,0,0, 0,0; ...
    x0+d,y1,z1, 1,0,0, 0,0; ...
    x0+d,y0,z1, 1,0,0, 0,0; ...
    x0,y0-d,z1, 0,-1,0, 0,0; ...
    X0,y0-d,z1, 0,-1,0, 0,0; ...
    X0-d2,y0-d2,z1, n2,-n2,0, 0,0; ...
    x1-d2,Y0-d2,z1, n2,-n2,0, 0,0];

idx = [0,1,3; 1,4,3; 1,2,4; ...
    4,5,3; 4,6,5; 4,2,7; 2,8,7; 4,7,6; 3,10,0; 3,9,10; 5,9,3; 0,12,1; 0,11,12; 0,10,11; 1,14,2; 1,13,14; 1,12,13; 2,14,8];

for i=1:size(vtx,1)
    vtx(i,7) = (x0-vtx(i,1))/0.1;
    vtx(i,8) = (y0-vtx(i,2))/0.1;
end

meshlist(1) = struct('vtx',vtx, 'idx',idx, 'grpname','PANELS1', 'name','');
prms = {};

% make children
[sublist,subprms] = make_over_rfpanel_light(this.light);
meshlist = meshmerge(meshlist, sublist);
prms = [prms; subprms];

% map into parent frame
meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);
end

% -------------------------------------------------------------------------
% Mid left overhead panel
% -------------------------------------------------------------------------

function [meshlist,prms] = make_over_lmpanel(this)
pdisp = this.pressuredisp;
d = this.edge;
x0 = 0;
x1 = x0 + this.w;
X0 = x0 + pdisp.ref(1);
X1 = X0 + pdisp.w;
y1 = 0;
y0 = y1 - this.h;
Y1 = y1 + pdisp.ref(2);
Y0 = Y1 - pdisp.h;
z0 = 0;
z1 = d;
Z0 = z0 + pdisp.depth;
irim = pdisp.rim;
irim2 = irim/2;
vtx = [x0,y0,z0, 0,0,-1, 0,0; ... % flat part of panel
    x1,y0,z0, 0,0,-1, 0,0; ...
    x0,y1,z0, 0,0,-1, 0,0; ...
    x1,y1,z0, 0,0,-1, 0,0; ...

    x0,y0-d,z1, 0,-1,0, 0,0; ... % outer rim
    x1,y0-d,z1, 0,-1,0, 0,0; ...
    x1+d,y0,z1, 1,0,0, 0,0; ...
    x1+d,y1,z1, 1,0,0, 0,0; ...
    x0,y1+d,z1, 0,1,0, 0,0; ...
    x1,y1+d,z1, 0,1,0, 0,0; ...
    x0-d,y0,z1, -1,0,0, 0,0; ...
    x0-d,y1,z1, -1,0,0, 0,0; ...

    X0,Y0-irim,z0, 0,0,-1, 0,0; ... % hole for pressure display
    X1,Y0-irim,z0, 0,0,-1, 0,0; ...
    X0-irim,Y0,z0, 0,0,-1, 0,0; ...
    X1+irim,Y0,z0, 0,0,-1, 0,0; ...
    X0-irim,Y1,z0, 0,0,-1, 0,0; ...
    X1+irim,Y1,z0, 0,0,-1, 0,0; ...
    X0,Y1+irim,z0, 0,0,-1, 0,0; ...
    X1,Y1+irim,z0, 0,0,-1, 0,0; ...

    X0+irim2,Y0,Z0, 0,1,0, 0,0; ... % rim for pressure display
    X1-irim2,Y0,Z0, 0,1,0, 0,0; ...
    X0,Y0+irim2,Z0, 1,0,0, 0,0; ...
    X1,Y0+irim2,Z0, -1,0,0, 0,0; ...
    X0,Y1-irim2,Z0, 1,0,0, 0,0; ...
    X1,Y1-irim2,Z0, -1,0,0, 0,0; ...
    X0+irim2,Y1,Z0, 0,-1,0, 0,0; ...
    X1-irim2,Y1,Z0, 0,-1,0, 0,0];

idx = [0,14,12; 0,12,13; 0,13,1; 1,13,15; 1,15,17; 1,17,3; 3,17,19; 3,19,18; 3,18,2; 2,18,16; 2,16,14; 2,14,0; ...
    4,0,5; 5,0,1; 1,3,6; 6,3,7; 3,2,8; 3,8,9; 10,11,0; 0,11,2; 1,6,5; 3,9,7; 2,11,8; 0,4,10; ...
    12,22,20; 12,14,22; 20,13,12; 20,21,13; 21,15,13; 21,23,15; 23,25,17; 23,17,15; 25,19,17; 25,27,19; 27,18,19; 27,26,18; 26,24,16; 26,16,18; 24,22,14; 24,14,16];

for i=1:size(vtx,1)
    vtx(i,7) = (vtx(i,1)-x0)/0.1;
    vtx(i,8) = (vtx(i,2)-y0)/0.1;
end

meshlist(1) = struct('vtx',vtx, 'idx',idx, 'grpname','PANELS1', 'name','');
prms = {};

% make children
sublist = make_pressuredisp(this.pressuredisp);
meshlist = meshmerge(meshlist, sublist);

sublist = make_pressurelabels(this.label);
meshlist = meshmerge(meshlist, sublist);

[sublist,subprms] = make_over_lmpanel_switch1(this.switch1);
meshlist = meshmerge(meshlist, sublist);
prms = [prms; subprms];

[sublist,subprms] = make_over_lmpanel_switch2(this.switch2);
meshlist = meshmerge(meshlist, sublist);
prms = [prms; subprms];

[sublist,subprms] = make_over_lmpanel_switch3(this.switch3);
meshlist = meshmerge(meshlist, sublist);
prms = [prms; subprms];

[sublist,subprms] = make_over_lmpanel_switch4(this.switch4);
meshlist = meshmerge(meshlist, sublist);
prms = [prms; subprms];

[sublist,subprms] = make_over_lmpanel_switch5(this.switch5);
meshlist = meshmerge(meshlist, sublist);
prms = [prms; subprms];

[sublist,subprms] = make_over_lmpanel_switch6(this.switch6);
meshlist = meshmerge(meshlist, sublist);
prms = [prms; subprms];

[sublist,subprms] = make_over_lmpanel_switch7(this.switch7);
meshlist = meshmerge(meshlist, sublist);
prms = [prms; subprms];

[sublist,subprms] = make_over_lmpanel_switch8(this.switch8);
meshlist = meshmerge(meshlist, sublist);
prms = [prms; subprms];

% map into parent frame
meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);
end

function [meshlist,prms] = make_over_lmpanel_switch1(this)
[meshlist,prms] = switch1(this.rad,this.len,'HATCH_SWITCH');
meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);
end

function [meshlist,prms] = make_over_lmpanel_switch2(this)
[meshlist,prms] = switch1(this.rad,this.len,'ILOCK_SWITCH');
meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);
end

function [meshlist,prms] = make_over_lmpanel_switch3(this)
[meshlist,prms] = switch1(this.rad,this.len,'OLOCK_SWITCH');
meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);
end

function [meshlist,prms] = make_over_lmpanel_switch4(this)
[meshlist,prms] = switch1(this.rad,this.len,'CABIN_O2_SWITCH');
meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);
end

function [meshlist,prms] = make_over_lmpanel_switch5(this)
[meshlist,prms] = switch1(this.rad,this.len,'VALVE1_SWITCH');
meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);
end

function [meshlist,prms] = make_over_lmpanel_switch6(this)
[meshlist,prms] = switch1(this.rad,this.len,'VALVE2_SWITCH');
meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);
end

function [meshlist,prms] = make_over_lmpanel_switch7(this)
[meshlist,prms] = switch1(this.rad,this.len,'VALVE3_SWITCH');
meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);
end

function [meshlist,prms] = make_over_lmpanel_switch8(this)
[meshlist,prms] = switch1(this.rad,this.len,'LOCK_O2_SWITCH');
meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);
end

% -------------------------------------------------------------------------
% Mid right overhead panel
% -------------------------------------------------------------------------

function [meshlist,prms] = make_over_rmpanel(this)
tdisp = this.tempdisp;
d = this.edge;
x0 = 0;
x1 = x0 + this.w;
X0 = x0 + tdisp.ref(1);
X1 = X0 + tdisp.w;
y1 = 0;
y0 = y1 - this.h;
Y1 = y1 + tdisp.ref(2);
Y0 = Y1 - tdisp.h;
z0 = 0;
z1 = d;
Z0 = z0 + tdisp.depth;
irim = tdisp.rim;
irim2 = irim/2;
vtx = [x0,y0,z0, 0,0,-1, 0,0; ... % flat part of panel
	x1,y0,z0, 0,0,-1, 0,0; ...
	x0,y1,z0, 0,0,-1, 0,0; ...
	x1,y1,z0, 0,0,-1, 0,0; ...
	
    x0,y0-d,z1, 0,-1,0, 0,0; ... % outer rim
    x1,y0-d,z1, 0,-1,0, 0,0; ...
    x1+d,y0,z1, 1,0,0, 0,0; ...
    x1+d,y1,z1, 1,0,0, 0,0; ...
    x0,y1+d,z1, 0,1,0, 0,0; ...
    x1,y1+d,z1, 0,1,0, 0,0; ...
    x0-d,y0,z1, -1,0,0, 0,0; ...
    x0-d,y1,z1, -1,0,0, 0,0; ...
	
    X0,Y0-irim,z0, 0,0,-1, 0,0; ... % hole for temperature display
    X1,Y0-irim,z0, 0,0,-1, 0,0; ...
    X0-irim,Y0,z0, 0,0,-1, 0,0; ...
    X1+irim,Y0,z0, 0,0,-1, 0,0; ...
    X0-irim,Y1,z0, 0,0,-1, 0,0; ...
    X1+irim,Y1,z0, 0,0,-1, 0,0; ...
    X0,Y1+irim,z0, 0,0,-1, 0,0; ...
    X1,Y1+irim,z0, 0,0,-1, 0,0; ...
	
    X0+irim2,Y0,Z0, 0,1,0, 0,0; ... % rim for temperature display
    X1-irim2,Y0,Z0, 0,1,0, 0,0; ...
    X0,Y0+irim2,Z0, 1,0,0, 0,0; ...
    X1,Y0+irim2,Z0, -1,0,0, 0,0; ...
    X0,Y1-irim2,Z0, 1,0,0, 0,0; ...
    X1,Y1-irim2,Z0, -1,0,0, 0,0; ...
    X0+irim2,Y1,Z0, 0,-1,0, 0,0; ...
    X1-irim2,Y1,Z0, 0,-1,0, 0,0];
	
idx = [0,14,12; 0,12,13; 0,13,1; 1,13,15; 1,15,17; 1,17,3; 3,17,19; 3,19,18; 3,18,2; 2,18,16; 2,16,14; 2,14,0; ...
    4,0,5; 5,0,1; 1,3,6; 6,3,7; 3,2,8; 3,8,9; 10,11,0; 0,11,2; 1,6,5; 3,9,7; 2,11,8; 0,4,10; ...
    12,22,20; 12,14,22; 20,13,12; 20,21,13; 21,15,13; 21,23,15; 23,25,17; 23,17,15; 25,19,17; 25,27,19; 27,18,19; 27,26,18; 26,24,16; 26,16,18; 24,22,14; 24,14,16];

for i=1:size(vtx,1)
    vtx(i,7) = (vtx(i,1)-x0)/0.1;
    vtx(i,8) = (vtx(i,2)-y0)/0.1;
end

meshlist(1) = struct('vtx',vtx, 'idx',idx, 'grpname','PANELS1', 'name','');
prms = {};

% make children
sublist = make_rmpanel_labels(this.label);
meshlist = meshmerge(meshlist, sublist);

sublist = make_tempdisp(this.tempdisp);
meshlist = meshmerge(meshlist, sublist);

[sublist,subprms] = make_over_rmpanel_tempdial(this.tempdial);
meshlist = meshmerge(meshlist, sublist);
prms = [prms; subprms];

[sublist,subprms] = make_over_rmpanel_pumpdial(this.pumpdial);
meshlist = meshmerge(meshlist, sublist);
prms = [prms; subprms];

[sublist,subprms] = make_over_rmpanel_switch1(this.switch1);
meshlist = meshmerge(meshlist, sublist);
prms = [prms; subprms];

% map into parent frame
meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);
end

% -------------------------------------------------------------------------
% coolant panel labels
% -------------------------------------------------------------------------

function meshlist = make_rmpanel_labels(this)
global g;
x0 = -40.5*g.defscale;
x1 = x0 + this.w;
y1 = 41.5*g.defscale;
y0 = y1 - this.h;
z0 = g.label_ofs;
u0 = this.u0;
u1 = this.u1;
v0 = this.v0;
v1 = this.v1;
vtx = [x0,y0,z0, 0,0,-1, u1,v0; ...
    x1,y0,z0, 0,0,-1, u1,v1; ...
    x0,y1,z0, 0,0,-1, u0,v0; ...
    x1,y1,z0, 0,0,-1, u0,v1];
idx = [0,2,1; 1,2,3];
meshlist = struct('vtx',vtx, 'idx',idx, 'grpname','LIT_LABELS', 'name','');

% map into parent frame
meshlist = transform(this,meshlist);
end

% -------------------------------------------------------------------------
% coolant pump switch
% -------------------------------------------------------------------------

function [meshlist,prms] = make_over_rmpanel_switch1(this)
[meshlist,prms] = switch1(this.rad,this.len,'PUMP_SWITCH');
meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);
end

% -------------------------------------------------------------------------
% coolant pump dial
% -------------------------------------------------------------------------

function [meshlist,prms] = make_over_rmpanel_pumpdial(this)
[meshlist,prms] = dial1(this.rad, this.h);
meshlist(1).grpname = 'COOLING_PUMP_DIAL';
meshlist(1).name = '';
prms{1}.name = 'COOLING_PUMP_DIAL';

% map into parent frame
meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);
end

% -------------------------------------------------------------------------
% coolant reference temperature dial
% -------------------------------------------------------------------------

function [meshlist,prms] = make_over_rmpanel_tempdial(this)
[meshlist,prms] = dial1(this.rad, this.h);
meshlist(1).grpname = 'COOLING_REFTEMP_DIAL';
meshlist(1).name = '';
prms{1}.name = 'COOLING_REFTEMP_DIAL';

% map into parent frame
meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);
end

% -------------------------------------------------------------------------
% Light cover on overhead panel (left or right)
% -------------------------------------------------------------------------

function meshlist = make_over_lightcover(this,which)
x0 = this.x0;
x1 = x0 + this.w;
y0 = this.y0;
y1 = y0 + this.h;
z0 = 0;
z1 = z0 - this.depth;
u0 = this.u(1);
u1 = this.u(2);
v0 = this.v(1);
v1 = this.v(2);
vtx = [x0,y0,z1, 0,0,-1, u0,v0; ...
    x1,y0,z1, 0,0,-1, u0,v1; ...
    x0,y1,z1, 0,0,-1, u1,v0; ...
    x1,y1,z1, 0,0,-1, u1,v1; ...

    x0,y0,z1, -1,0,0, u0,v1; ...
    x0,y0,z0, -1,0,0, u0,v1; ...
    x0,y1,z1, -1,0,0, u1,v1; ...
    x0,y1,z0, -1,0,0, u1,v1; ...

    x0,y0,z1, 0,-1,0, u0,v0; ...
    x0,y0,z0, 0,-1,0, u0,v0; ...
    x1,y0,z1, 0,-1,0, u0,v1; ...
    x1,y0,z0, 0,-1,0, u0,v1; ...

    x1,y0,z1, 1,0,0, u0,v1; ...
    x1,y0,z0, 1,0,0, u0,v1; ...
    x1,y1,z1, 1,0,0, u1,v1; ...
    x1,y1,z0, 1,0,0, u1,v1];
idx = [0,2,1; 1,2,3; ...
    4,5,6; 5,7,6; 8,10,9; 9,10,11; 12,14,13; 13,14,15];
if strcmp(which, 'left')
    idx = [idx(:,1), idx(:,3), idx(:,2)];
    vtx = [-vtx(:,1), vtx(:,2:3), -vtx(:,4), vtx(:,5:end)];
end
meshlist(1) = struct('vtx',vtx, 'idx',idx, 'grpname','VC4_DEFAULT', 'name','');
end

% =========================================================================
% =========================================================================
% Level 3 components
% =========================================================================
% =========================================================================

% -------------------------------------------------------------------------
% HUD controls (mode buttons, brightness dial, colour button) on dash panel
% -------------------------------------------------------------------------

function [meshlist,prms] = make_hudctrl(this)

% make children
[meshlist,prms] = make_hudbuttons(this.modebtns);

[sublist,subprms] = make_hudbrtdial(this.brtdial);
meshlist = meshmerge(meshlist, sublist);
prms = [prms; subprms];

[sublist,subprms] = make_hudcolbutton(this.colbtn);
meshlist = meshmerge(meshlist, sublist);
prms = [prms; subprms];

sublist = make_hudlabel(this.label);
meshlist = meshmerge(meshlist, sublist);

% map into parent frame
meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);
end

% -------------------------------------------------------------------------
% RCS Prog buttons on dash panel
% -------------------------------------------------------------------------

function [meshlist,prms] = make_rcsprog(this)

global g;
meshlist = [];
p.mousearea = [-84.5,-2,0; 84.5,-2,0; -84.5,63,0; 84.5,63,0]*g.defscale;
p.name = 'NAV_BUTTONS';
prms{1} = p;

for i=1:6
    [sublist,subprms] = button3(this.btn(i), ['BTN_NAVMODE_' num2str(i)]);
    sublist = transform(this.btn(i),sublist);
    subprms = transform_prms(this.btn(i),subprms);
    meshlist = meshmerge(meshlist,sublist);
    prms = [prms;subprms];
end

% make children
%[meshlist,prms] = make_navbuttons(this.btn);

sublist = make_navlabel(this.label);
meshlist = meshmerge(meshlist, sublist);

% map into parent frame
meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);
end

% -------------------------------------------------------------------------
% AF selector dial
% -------------------------------------------------------------------------

function [meshlist,prms] = make_afdial(this)
[meshlist,prms] = dial2(this.rad,this.h);
meshlist(1).grpname = 'DIAL1';
meshlist(1).name = 'AF_DIAL';
prms{1}.name = 'AF_DIAL';

% map into parent frame
meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);
end

% -------------------------------------------------------------------------
% Label for AF selector dial
% -------------------------------------------------------------------------

function meshlist = make_aflabel(this)
global g;
x0 = this.xc - this.w/2;
x1 = x0 + this.w;
y0 = this.y0;
y1 = y0 + this.h;
z0 = g.label_ofs;
u0 = this.u0;
u1 = this.u1;
v0 = this.v0;
v1 = this.v1;
vtx = [x0,y0,z0, 0,0,-1, u0,v0; ...
    x1,y0,z0, 0,0,-1, u1,v0; ...
    x0,y1,z0, 0,0,-1, u0,v1; ...
    x1,y1,z0, 0,0,-1, u1,v1];
idx = [0,2,1; 1,2,3];
meshlist = struct('vtx',vtx, 'idx',idx, 'grpname','LIT_LABELS', 'name','');

% map into parent frame
meshlist = transform(this,meshlist);
end

% -------------------------------------------------------------------------
% RCS selector dial
% -------------------------------------------------------------------------

function [meshlist,prms] = make_rcsdial(this)
[meshlist,prms] = dial2(this.rad,this.h);
meshlist(1).grpname = 'DIAL1';
meshlist(1).name = 'RCS_DIAL';
prms{1}.name = 'RCS_DIAL';

% map into parent frame
meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);
end

% -------------------------------------------------------------------------
% Label for RCS selector dial
% -------------------------------------------------------------------------

function meshlist = make_rcslabel(this)
global g;
x0 = this.xc - this.w/2;
x1 = x0 + this.w;
y0 = this.y0;
y1 = y0 + this.h;
z0 = g.label_ofs;
u0 = this.u0;
u1 = this.u1;
v0 = this.v0;
v1 = this.v1;
vtx = [x0,y0,z0, 0,0,-1, u0,v0; ...
    x1,y0,z0, 0,0,-1, u1,v0; ...
    x0,y1,z0, 0,0,-1, u0,v1; ...
    x1,y1,z0, 0,0,-1, u1,v1];
idx = [0,2,1; 1,2,3];
meshlist = struct('vtx',vtx, 'idx',idx, 'grpname','LIT_LABELS', 'name','');

% map into parent frame
meshlist = transform(this,meshlist);
end

% -------------------------------------------------------------------------
% HUD buttons
% -------------------------------------------------------------------------

function [meshlist,prms] = make_hudbuttons(this)
global g;
meshlist = [];
p.mousearea = [-63,-17,0; 63,-17,0; -63,15,0; 63,15,0]*g.defscale;
p.name = 'HUD_BUTTONS';
prms{1} = p;

for i=1:3
    [sublist,subprms] = button3(this.btn(i), ['BTN_HUDMODE_' num2str(i)]);
    sublist = transform(this.btn(i),sublist);
    subprms = transform_prms(this.btn(i),subprms);
    meshlist = meshmerge(meshlist,sublist);
    prms = [prms;subprms];
end

% map into parent frame
meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);
end

% -------------------------------------------------------------------------
% HUD brightness dial
% -------------------------------------------------------------------------

function [meshlist,prms] = make_hudbrtdial(this)
[meshlist,prms] = dial1(this.rad,this.h);
meshlist(1).grpname = 'HUD_BRIGHTNESS';
meshlist(1).name = '';
prms{1}.name = 'HUD_BRIGHTNESS';

% map into parent frame
meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);
end

% -------------------------------------------------------------------------
% HUD colour selector button
% -------------------------------------------------------------------------

function [meshlist,prms] = make_hudcolbutton(this)
[meshlist,prms] = button2(this.w,this.h,this.depth,'HUD_COLBUTTON');

% map into parent frame
meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);
end

% -------------------------------------------------------------------------
% Label for HUD controls
% -------------------------------------------------------------------------

function meshlist = make_hudlabel(this)
global g;
x0 = -this.wa/2;
x1 = -x0;
y1 = 0;
y0 = y1 - this.ha;
z0 = g.label_ofs;
u0 = this.u0a;
u1 = this.u1a;
v0 = this.v0a;
v1 = this.v1a;
vtx = [x0,y0,z0, 0,0,-1, u0,v0; ...
    x1,y0,z0, 0,0,-1, u1,v0; ...
    x0,y1,z0, 0,0,-1, u0,v1; ...
    x1,y1,z0, 0,0,-1, u1,v1];
idx = [0,2,1; 1,2,3];

x0 = this.x0b;
x1 = x0+this.wb;
y1 = this.y1b;
y0 = y1-this.hb;
u0 = this.u0b;
u1 = this.u1b;
v0 = this.v0b;
v1 = this.v1b;
mvtx = [x0,y0,z0, 0,0,-1, u0,v0; ...
    x1,y0,z0, 0,0,-1, u1,v0; ...
    x0,y1,z0, 0,0,-1, u0,v1; ...
    x1,y1,z0, 0,0,-1, u1,v1];
midx = [0,2,1; 1,2,3];
[vtx,idx] = add_to_group (vtx,idx,mvtx,midx);

meshlist = struct('vtx',vtx, 'idx',idx, 'grpname','LIT_LABELS', 'name','');

% map into parent frame
meshlist = transform(this,meshlist);
end

% -------------------------------------------------------------------------
% Label for NAV Prog buttons
% -------------------------------------------------------------------------

function meshlist = make_navlabel(this)
global g;
x0 = -this.w/2;
x1 = -x0;
y1 = 0;
y0 = y1 - this.h;
z0 = g.label_ofs;
u0 = this.u0;
u1 = this.u1;
v0 = this.v0;
v1 = this.v1;
vtx = [x0,y0,z0, 0,0,-1, u0,v0; ...
    x1,y0,z0, 0,0,-1, u1,v0; ...
    x0,y1,z0, 0,0,-1, u0,v1; ...
    x1,y1,z0, 0,0,-1, u1,v1];
idx = [0,2,1; 1,2,3];

meshlist = struct('vtx',vtx, 'idx',idx, 'grpname','LIT_LABELS', 'name','');

% map into parent frame
meshlist = transform(this,meshlist);
end

% -------------------------------------------------------------------------
% NAV Prog buttons on dash panel
% -------------------------------------------------------------------------

function [meshlist,prms] = make_navbuttons(this)
x0 = [this.w*3+this.gapx*3, 0, this.w*2+this.gapx*2, ...
    this.w*2+this.gapx*2, this.w+this.gapx, this.w+this.gapx] - (this.w*4+this.gapx*3)/2;
y0 = [0, 0, this.h+this.gapy, 0, this.h+this.gapy, 0];
t0 = texcrd(368);
vtx = [];
idx = [];
for i=0:5
    u0 = texcrd((i+1)*41);
    [mvtx,midx] = make_dash_button(0,0,x0(i+1),y0(i+1),0,t0,u0,this);
    [vtx,idx] = add_to_group(vtx,idx,mvtx,midx);
end
meshlist(1) = struct('vtx',vtx, 'idx',idx, 'grpname','NAV_BUTTONS', 'name','');

xmin = min(vtx(:,1));
xmax = max(vtx(:,1));
ymin = min(vtx(:,2));
ymax = max(vtx(:,2));
p.mousearea = [xmin,ymin,0; ...
    xmax,ymin,0; ...
    xmin,ymax,0; ...
    xmax,ymax,0];
p.name = 'NAV_BUTTONS';
prms{1} = p;

% map into parent frame
meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);
end

% -------------------------------------------------------------------------
% Light controls on overhead panel
% -------------------------------------------------------------------------

function [meshlist,prms] = make_over_lfpanel_light(this)
global fhd;

% make children
meshlist = make_over_lfpanel_light_label(this.label);
prms = {};

% make children
[sublist,subprms] = make_over_lfpanel_light_switch1(this.switch1);
meshlist = meshmerge(meshlist, sublist);
prms = [prms; subprms];

[sublist,subprms] = make_over_lfpanel_light_switch2(this.switch2);
meshlist = meshmerge(meshlist, sublist);
prms = [prms; subprms];

[sublist,subprms] = make_over_lfpanel_light_dial1(this.dial1);
meshlist = meshmerge(meshlist, sublist);
prms = [prms; subprms];

[sublist,subprms] = make_over_lfpanel_light_dial2(this.dial2);
meshlist = meshmerge(meshlist, sublist);
prms = [prms; subprms];

% map into parent frame
meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);
end

function meshlist = make_over_lfpanel_light_label(this)
global g;
x0 = 0;
x1 = x0 + this.w;
y1 = 0;
y0 = y1 - this.h;
z0 = g.label_ofs;
u0 = this.u0;
u1 = this.u1;
v0 = this.v0;
v1 = this.v1;
vtx = [x0,y0,z0, 0,0,-1, u0,v0; ...
    x1,y0,z0, 0,0,-1, u1,v0; ...
    x0,y1,z0, 0,0,-1, u0,v1; ...
    x1,y1,z0, 0,0,-1, u1,v1];
idx = [0,2,1; 1,2,3];

meshlist = struct('vtx',vtx, 'idx',idx, 'grpname','LIT_LABELS', 'name','');

% map into parent frame
meshlist = transform(this,meshlist);
end

function [meshlist,prms] = make_over_lfpanel_light_switch1(this)
[meshlist,prms] = switch1(this.rad,this.len,'INSTRLIGHT_SWITCH');
meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);
end

function [meshlist,prms] = make_over_lfpanel_light_switch2(this)
[meshlist,prms] = switch1(this.rad,this.len,'FLOODLIGHT_SWITCH');
meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);
end

function [meshlist,prms] = make_over_lfpanel_light_dial1(this)
[meshlist,prms] = dial1(this.rad,this.h);
meshlist(1).grpname = 'INSTR_BRIGHTNESS';
meshlist(1).name = '';
prms{1}.name = 'INSTR_BRIGHTNESS';

% map into parent frame
meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);
end

function [meshlist,prms] = make_over_lfpanel_light_dial2(this)
[meshlist,prms] = dial1(this.rad,this.h);
meshlist(1).grpname = 'FLOOD_BRIGHTNESS';
meshlist(1).name = '';
prms{1}.name = 'FLOOD_BRIGHTNESS';

% map into parent frame
meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);
end


% -------------------------------------------------------------------------
% Controls and labels on front right overhead panel
% -------------------------------------------------------------------------

function [meshlist,prms] = make_over_rfpanel_light(this)
global fhd;

% make children
meshlist = make_over_rfpanel_light_label(this.label);
prms = {};

[sublist,subprms] = make_over_rfpanel_light_switch1(this.switch1);
meshlist = meshmerge(meshlist, sublist);
prms = [prms; subprms];

[sublist,subprms] = make_over_rfpanel_light_switch2(this.switch2);
meshlist = meshmerge(meshlist, sublist);
prms = [prms; subprms];

[sublist,subprms] = make_over_rfpanel_light_switch3(this.switch3);
meshlist = meshmerge(meshlist, sublist);
prms = [prms; subprms];

[sublist,subprms] = make_over_rfpanel_light_switch4(this.switch4);
meshlist = meshmerge(meshlist, sublist);
prms = [prms; subprms];

[sublist,subprms] = make_over_rfpanel_light_switch5(this.switch5);
meshlist = meshmerge(meshlist, sublist);
prms = [prms; subprms];

% map into parent frame
meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);
end

function meshlist = make_over_rfpanel_light_label(this)
global g;
x0 = 0;
x1 = x0 + this.w;
y1 = 0;
y0 = y1 - this.h;
z0 = g.label_ofs;
u0 = this.u0;
u1 = this.u1;
v0 = this.v0;
v1 = this.v1;
vtx = [x0,y0,z0, 0,0,-1, u0,v0; ...
    x1,y0,z0, 0,0,-1, u1,v0; ...
    x0,y1,z0, 0,0,-1, u0,v1; ...
    x1,y1,z0, 0,0,-1, u1,v1];
idx = [0,2,1; 1,2,3];

meshlist = struct('vtx',vtx, 'idx',idx, 'grpname','LIT_LABELS', 'name','');

% map into parent frame
meshlist = transform(this,meshlist);
end

function [meshlist,prms] = make_over_rfpanel_light_switch1(this)
[meshlist,prms] = switch1(this.rad,this.len,'HUDRETRACT_SWITCH');
meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);
end

function [meshlist,prms] = make_over_rfpanel_light_switch2(this)
[meshlist,prms] = switch1(this.rad,this.len,'LANDINGLIGHT_SWITCH');
meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);
end

function [meshlist,prms] = make_over_rfpanel_light_switch3(this)
[meshlist,prms] = switch1(this.rad,this.len,'STROBELIGHT_SWITCH');
meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);
end

function [meshlist,prms] = make_over_rfpanel_light_switch4(this)
[meshlist,prms] = switch1(this.rad,this.len,'NAVLIGHT_SWITCH');
meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);
end

function [meshlist,prms] = make_over_rfpanel_light_switch5(this)
[meshlist,prms] = switch1(this.rad,this.len,'RADIATOR_SWITCH');
meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);
end

% -------------------------------------------------------------------------
% Pressure display on left mid overhead panel
% -------------------------------------------------------------------------

function meshlist = make_pressuredisp(this)
x0 = 0;
x1 = x0 + this.w;
y1 = 0;
y0 = y1 - this.h;
z0 = this.depth;
u0 = 53.5/512;
u1 = 299.5/512;
v1 = 0.5/512;
v0 = 45.5/512;
vtx = [x0,y0,z0, 0,0,-1, u0,v0; ...
    x1,y0,z0, 0,0,-1, u1,v0; ...
    x0,y1,z0, 0,0,-1, u0,v1; ...
    x1,y1,z0, 0,0,-1, u1,v1];
idx = [0,2,1; 1,2,3];
meshlist = struct('vtx',vtx, 'idx',idx, 'grpname','BLITTGT1', 'name','');

% map into parent frame
meshlist = transform(this,meshlist);
end

function meshlist = make_pressurelabels(this)
global g;
% label above pressure display
label = this.valves;
x0 = -label.w/2;
x1 = x0 + label.w;
y0 = 0;
y1 = y0 + label.h;
z0 = g.label_ofs;
u0 = label.u0;
u1 = label.u1;
v0 = label.v0;
v1 = label.v1;
vtx = [x0,y0,z0, 0,0,-1, u0,v0; ...
    x1,y0,z0, 0,0,-1, u1,v0; ...
    x0,y1,z0, 0,0,-1, u0,v1; ...
    x1,y1,z0, 0,0,-1, u1,v1];
idx = [0,2,1; 1,2,3];
meshlist(1) = struct('vtx',vtx, 'idx',idx, 'grpname','LIT_LABELS', 'name','');

% map into parent frame
meshlist(1) = transform(label,meshlist(1));

% label for hatch and airlock switches
label = this.locks;
x0 = -label.w/2;
x1 = x0 + label.w;
y1a = 0;
y0a = y1a - label.ha;
y1b = y0a - 47*g.defscale;
y0b = y1b - label.hb;
z0 = g.label_ofs;
u0 = label.u0;
u1 = label.u1;
v0a = label.v0a;
v1a = label.v1a;
v0b = label.v0b;
v1b = label.v1b;
vtx = [x0,y0a,z0, 0,0,-1, u0,v0a; ...
    x1,y0a,z0, 0,0,-1, u1,v0a; ...
    x0,y1a,z0, 0,0,-1, u0,v1a; ...
    x1,y1a,z0, 0,0,-1, u1,v1a; ...
    x0,y0b,z0, 0,0,-1, u0,v0b; ...
    x1,y0b,z0, 0,0,-1, u1,v0b; ...
    x0,y1b,z0, 0,0,-1, u0,v1b; ...
    x1,y1b,z0, 0,0,-1, u1,v1b];
idx = [0,2,1; 1,2,3; 4,6,5; 5,6,7];
meshlist(2) = struct('vtx',vtx, 'idx',idx, 'grpname','LIT_LABELS', 'name','');

% map into parent frame
meshlist(2) = transform(label,meshlist(2));

% label for pressure display
label = this.press;
x0 = 0;
x1 = x0 + label.w;
y0 = -label.h/2;
y1 = y0 + label.h;
z0 = g.label_ofs;
u0 = label.u0;
u1 = label.u1;
v0 = label.v0;
v1 = label.v1;
vtx = [x0,y0,z0, 0,0,-1, u0,v0; ...
    x1,y0,z0, 0,0,-1, u1,v0; ...
    x0,y1,z0, 0,0,-1, u0,v1; ...
    x1,y1,z0, 0,0,-1, u1,v1];
idx = [0,2,1; 1,2,3];
meshlist(3) = struct('vtx',vtx, 'idx',idx, 'grpname','LIT_LABELS', 'name','');

% map into parent frame
meshlist(3) = transform(label,meshlist(3));
end

% -------------------------------------------------------------------------
% Temperature display on right mid overhead panel
% -------------------------------------------------------------------------

function meshlist = make_tempdisp(this)
x0 = 0;
x1 = x0 + this.w;
y1 = 0;
y0 = y1 - this.h;
z0 = this.depth;
u0 = 62/512;
u1 = 286/512;
v1 = 45.5/512;
v0 = 245.5/512;
vtx = [x0,y0,z0, 0,0,-1, u0,v0; ...
    x1,y0,z0, 0,0,-1, u1,v0; ...
    x0,y1,z0, 0,0,-1, u0,v1; ...
    x1,y1,z0, 0,0,-1, u1,v1];
idx = [0,2,1; 1,2,3];
meshlist = struct('vtx',vtx, 'idx',idx, 'grpname','BLITTGT1', 'name','');

% map into parent frame
meshlist = transform(this,meshlist);
end

% -------------------------------------------------------------------------
% Angular velocity/acceleration/moments display on right side panel
% -------------------------------------------------------------------------

function meshlist = make_rpanel_angveldisp(this)
x0 = -this.w/2;
x1 = x0 + this.w;
y0 = 0;
y1 = y0 + this.h;
z2 = 0;
z1 = z2 + this.depth;
z2 = z1 - 0.0001;

% The 9 indicator scales
w = [100,100,90]./290 * this.w; % width of individual scales
h = w(1);
titleh = (this.h - h*3)/3;
X0 = [x0,x0+w(1),x0+w(1)+w(2)];
X1 = X0 + w;
Y1 = [y1-titleh,y1-h-2*titleh,y1-2*h-3*titleh];
Y0 = Y1 - h;
u0 = 21.5/512;
u1 = 120.5/512;
v0 = 687.5/1024;
v1 = 588.5/1024;
v2 = v1 + (v0-v1)*90/100;

vtx = [];
idx = [];
for i=1:3
    vtx = [vtx; ...
        X0(1),Y0(i),z1, 0,0,-1, u0,v0; ... % yaw
        X1(1),Y0(i),z1, 0,0,-1, u1,v0; ...
        X0(1),Y1(i),z1, 0,0,-1, u0,v1; ...
        X1(1),Y1(i),z1, 0,0,-1, u1,v1; ...

        X0(2),Y0(i),z1, 0,0,-1, u0,v0; ... % roll
        X1(2),Y0(i),z1, 0,0,-1, u1,v0; ...
        X0(2),Y1(i),z1, 0,0,-1, u0,v1; ...
        X1(2),Y1(i),z1, 0,0,-1, u1,v1; ...

        X0(3),Y0(i),z1, 0,0,-1, u1,v2; ... % pitch
        X1(3),Y0(i),z1, 0,0,-1, u1,v1; ...
        X0(3),Y1(i),z1, 0,0,-1, u0,v2; ...
        X1(3),Y1(i),z1, 0,0,-1, u0,v1];

    idx = [idx; ...
        [0,2,1; 1,2,3; 4,6,5; 5,6,7; 8,10,9; 9,10,11] + (i-1)*12];
end

% The 3 title lines
Y1a = y1 - this.h/3*[0:2];
Y0a = Y1a - titleh;
u0 = 21.5/512;
u1 = 236.5/512;
v1 = (542.5+15*[0:2])/1024;
v0 = v1+14/1024;
mvtx = [];
midx = [];
for i=1:3
    mvtx = [mvtx; ...
        x0,Y0a(i),z1, 0,0,-1, u0,v0(i); ...
        x1,Y0a(i),z1, 0,0,-1, u1,v0(i); ...
        x0,Y1a(i),z1, 0,0,-1, u0,v1(i); ...
        x1,Y1a(i),z1, 0,0,-1, u1,v1(i)];
    midx = [midx; ...
        [0,2,1; 1,2,3] + (i-1)*4];
end
[vtx,idx] = add_to_group (vtx,idx,mvtx,midx);
meshlist(1) = struct('vtx',vtx, 'idx',idx, 'grpname','ANGVEL_DISP', 'name','');

% The semi-transparent shrouds over the scales
cntx = (X0+X1)/2;
cnty = (Y0+Y1)/2;
cntx(3) = X1(3)-(Y1(1)-cnty(1));
eps = h/50;
u = 22/512;
v = 692/1024;
vtx = [];
idx = [];
for i=1:3
    vtx = [vtx; ...
        cntx(1),Y1(i),  z2, 0,0,-1, u,v; ... % yaw scale overlay
        X0(1),Y1(i),    z2, 0,0,-1, u,v; ...
        X0(1),Y0(i)-eps,z2, 0,0,-1, u,v; ...
        cntx(1),Y1(i),  z2, 0,0,-1, u,v; ...
        X1(1),Y1(i),    z2, 0,0,-1, u,v; ...
        X1(1),Y0(i)-eps,z2, 0,0,-1, u,v; ...
        cntx(1),cnty(i),z2, 0,0,-1, u,v; ...

        cntx(2),Y1(i),  z2, 0,0,-1, u,v; ... % roll scale overlay
        X0(2),Y1(i),    z2, 0,0,-1, u,v; ...
        X0(2),Y0(i)-eps,z2, 0,0,-1, u,v; ...
        cntx(2),Y1(i),  z2, 0,0,-1, u,v; ...
        X1(2),Y1(i),    z2, 0,0,-1, u,v; ...
        X1(2),Y0(i)-eps,z2, 0,0,-1, u,v; ...
        cntx(2),cnty(i),z2, 0,0,-1, u,v; ...

        X1(3),cnty(i),  z2, 0,0,-1, u,v; ... % pitch scale overlay
        X1(3),Y1(i),    z2, 0,0,-1, u,v; ...
        X1(3)-h-eps,Y1(i),z2, 0,0,-1, u,v; ...
        X1(3),cnty(i),  z2, 0,0,-1, u,v; ...
        X1(3),Y0(i),    z2, 0,0,-1, u,v; ...
        X1(3)-h-eps,Y0(i),  z2, 0,0,-1, u,v; ...
        cntx(3),cnty(i),z2, 0,0,-1, u,v];

    idx = [idx; ...
        [6,1,0; 6,2,1; 6,3,4; 6,4,5; ...
        13,8,7; 13,9,8; 13,10,11; 13,11,12; ...
        20,15,14; 20,16,15; 20,17,18; 20,18,19] + (i-1)*21];
end
meshlist(2) = struct('vtx',vtx, 'idx',idx, 'grpname','ANGVEL_DISP_OVR', 'name','');

% Billboards for numerical value outputs
x0 = cntx-h*0.32;
x1 = cntx+h*0.32;
y0 = cnty-h*0.32*16/53;
y1 = cnty+h*0.32*16/53;
z3 = z2 - 0.0001;
u0 = 0.5/512;
u1 = 53.5/512;
v0 = (0.5 + 16*[1:9])/512;
v1 = (0.5 + 16*[0:8])/512;
vtx = [];
idx = [];
k = 1;
ofs = 0;
for i=1:3
    for j=1:3
        vtx = [vtx; ...
            x0(j),y0(i),z3, 0,0,-1, u0,v0(k); ...
            x1(j),y0(i),z3, 0,0,-1, u1,v0(k); ...
            x0(j),y1(i),z3, 0,0,-1, u0,v1(k); ...
            x1(j),y1(i),z3, 0,0,-1, u1,v1(k)];
        k = k+1;
        idx = [idx; ...
            [0,2,1; 1,2,3]+ofs];
        ofs = ofs+4;
    end
end
meshlist(3) = struct('vtx',vtx, 'idx',idx, 'grpname','BLITTGT1', 'name','');

% map into parent frame
meshlist = transform(this,meshlist);
end

% -------------------------------------------------------------------------
% Label for angular velocity/acceleration/moments display on right side panel
% -------------------------------------------------------------------------

function meshlist = make_rpanel_angveldisp_label(this)
global g;
x0 = -this.w/2;
x1 = x0 + this.w;
y0 = 0;
y1 = y0 + this.h;
z0 = g.label_ofs;
u0 = this.u0;
u1 = this.u1;
v0 = this.v0;
v1 = this.v1;
vtx = [x0,y0,z0, 0,0,-1, u0,v0; ...
    x1,y0,z0, 0,0,-1, u1,v0; ...
    x0,y1,z0, 0,0,-1, u0,v1; ...
    x1,y1,z0, 0,0,-1, u1,v1];
idx = [0,2,1; 1,2,3];
meshlist = struct('vtx',vtx, 'idx',idx, 'grpname','LIT_LABELS', 'name','');

% map into parent frame
meshlist = transform(this,meshlist);
end

% -------------------------------------------------------------------------
% HSI (heading) display on lower mid panel
% -------------------------------------------------------------------------

function [meshlist,prms] = make_hsi(this)
meshlist(1) = make_mfdframe (this);

[vtx,idx] = hsi_disp('VC',this.view);
meshlist(2) = struct('vtx',vtx, 'idx',idx, 'grpname','HSI', 'name','');

p.ref = [0,0,0];
p.axis = [0,0,-1];
p.name = 'HSI';
prms{1} = p;

% map into parent frame
meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);

end

% -------------------------------------------------------------------------
% AOA indicator tape
% -------------------------------------------------------------------------

function [meshlist,prms] = make_aoatape(this)
global g;

meshlist(1) = make_tapeframe(this);

% tape
s0w = 0.002;
x0 = -this.tapew/2+s0w;
x1 = -x0;
y0 = -this.tapeh/2+s0w;
y1 = -y0;
z0 = -this.depth+0.004;
u0 = 272.5/512;
u1 = 313.5/512;
v0 = 771/1024;
v1 = 259/1024;
vtx = [x0,y1,z0, 0,0,-1, u0,v1; ... % tape
       x1,y1,z0, 0,0,-1, u1,v1; ...
       x0,y1,z0, 0,0,-1, u0,v1; ...
       x1,y1,z0, 0,0,-1, u1,v1; ...
       x0,y0,z0, 0,0,-1, u0,v0; ...
       x1,y0,z0, 0,0,-1, u1,v0; ...
       x0,y0,z0, 0,0,-1, u0,v0; ...
       x1,y0,z0, 0,0,-1, u1,v0];
idx = [0,1,2; 3,2,1; 2,3,4; 5,4,3; 4,5,6; 7,6,5];
meshlist(2) = struct('vtx',vtx, 'idx',idx, 'grpname','VC_INSTR','name','AOA');

% index line
x0 = -this.tapew/2+s0w;
x1 = this.tapew/2+this.readoutw-s0w;
y1 = (x1-x0)/108*15;
y0 = -y1;
z0 = -this.depth+0.003;
u0 = 1/1024;
u1 = 109/1024;
v0 = 289/1024;
v1 = 259/1024;
vtx = [x0,y0,z0, 0,0,-1, u0,v0; ...
       x1,y0,z0, 0,0,-1, u1,v0; ...
       x0,y1,z0, 0,0,-1, u0,v1; ...
       x1,y1,z0, 0,0,-1, u1,v1];
idx = [0,2,1; 2,3,1];
meshlist(3) = struct('vtx',vtx, 'idx',idx, 'grpname','LIT_SURF', 'name','');

% readout
pix = (x1-x0)/108;
y0 = -pix*12;
y1 = -y0;
cw = (y1-y0)*0.5;
x0 = this.tapew/2+this.readoutw-s0w-4*cw-pix*3;
z0 = -this.depth+0.002;
vtx = [];
idx = [];
for i=0:3
    vtx = [vtx; ...
           x0,   y0,z0, 0,0,-1, 0,0; ...
           x0+cw,y0,z0, 0,0,-1, 0,0; ...
           x0,   y1,z0, 0,0,-1, 0,0; ...
           x0+cw,y1,z0, 0,0,-1, 0,0];
    idx = [idx; ...
           [0,2,1; 1,2,3]+i*4];
    x0 = x0+cw;
end
meshlist(4) = struct('vtx',vtx, 'idx',idx, 'grpname','VC_INSTR','name','AOA_READOUT');

% label
w = 0.006;
h = w*46/12;
x0 = (this.tapew/2+this.w/2)/2-w/2;
x1 = x0+w;
y0 = this.h/4-h/2;
y1 = y0+h;
z0 = -this.depth+g.shadow_ofs;
u0 = texcrd(349);
u1 = texcrd(361);
v0 = texcrd(397);
v1 = texcrd(351);
vtx = [x0,y0,z0, 0,0,-1, u0,v0; ...
       x1,y0,z0, 0,0,-1, u1,v0; ...
       x0,y1,z0, 0,0,-1, u0,v1; ...
       x1,y1,z0, 0,0,-1, u1,v1];
idx = [0,2,1; 1,2,3];
meshlist(5) = struct('vtx',vtx, 'idx',idx, 'grpname','SHADOW', 'name','');

% map into parent frame
meshlist = transform(this,meshlist);

p.ref = [0,0,0];
p.axis = [0,0,-1];
p.name = 'AOA';
prms{1} = p;

end

% -------------------------------------------------------------------------
% VS indicator tape
% -------------------------------------------------------------------------

function [meshlist,prms] = make_vstape(this)
global g;

meshlist(1) = make_tapeframe(this);

% tape
s0w = 0.002;
x0 = -this.tapew/2+s0w;
x1 = -x0;
y0 = -this.tapeh/2+s0w;
y1 = -y0;
z0 = -this.depth+0.004;
u0 = 314.5/512;
u1 = 355.5/512;
v1 = 260/1024;
v0 = 593/1024;
vtx = [x0,y1,z0, 0,0,-1, u0,v1; ...
       x1,y1,z0, 0,0,-1, u1,v1; ...
       x0,y0,z0, 0,0,-1, u0,v0; ...
       x1,y0,z0, 0,0,-1, u1,v0];
idx = [0,1,2; 3,2,1];
meshlist(2) = struct('vtx',vtx, 'idx',idx, 'grpname','VC_INSTR', 'name','VSTAPE');

% index line
x0 = -this.tapew/2+s0w;
x1 = this.tapew/2+this.readoutw-s0w;
y1 = (x1-x0)/108*15;
y0 = -y1;
z0 = -this.depth+0.003;
u0 = 1/1024;
u1 = 109/1024;
v0 = 321/1024;
v1 = 291/1024;
vtx = [x0,y0,z0, 0,0,-1, u0,v0; ...
       x1,y0,z0, 0,0,-1, u1,v0; ...
       x0,y1,z0, 0,0,-1, u0,v1; ...
       x1,y1,z0, 0,0,-1, u1,v1];
idx = [0,2,1; 2,3,1];
meshlist(3) = struct('vtx',vtx, 'idx',idx, 'grpname','LIT_SURF', 'name','');

% readout
pix = (x1-x0)/108;
y0 = -pix*12;
y1 = -y0;
cw = (y1-y0)*0.5;
x0 = this.tapew/2+this.readoutw-s0w-5*cw-pix*3;
z0 = -this.depth+0.002;
vtx = [];
idx = [];
for i=0:4
    vtx = [vtx; ...
           x0,   y0,z0, 0,0,-1, 0,0; ...
           x0+cw,y0,z0, 0,0,-1, 0,0; ...
           x0,   y1,z0, 0,0,-1, 0,0; ...
           x0+cw,y1,z0, 0,0,-1, 0,0];
    idx = [idx; ...
           [0,2,1; 1,2,3]+i*4];
    x0 = x0+cw;
end
meshlist(4) = struct('vtx',vtx, 'idx',idx, 'grpname','VC_INSTR','name','VS_READOUT');

% label
w = 0.006;
h = w*30/12;
x0 = (this.tapew/2+this.w/2)/2-w/2;
x1 = x0+w;
y0 = this.h/4-h/2;
y1 = y0+h;
z0 = -this.depth+g.shadow_ofs;
u0 = texcrd(333);
u1 = texcrd(345);
v0 = texcrd(388);
v1 = texcrd(359);
vtx = [x0,y0,z0, 0,0,-1, u0,v0; ...
       x1,y0,z0, 0,0,-1, u1,v0; ...
       x0,y1,z0, 0,0,-1, u0,v1; ...
       x1,y1,z0, 0,0,-1, u1,v1];
idx = [0,2,1; 1,2,3];
meshlist(5) = struct('vtx',vtx, 'idx',idx, 'grpname','SHADOW', 'name','');

% map into parent frame
meshlist = transform(this,meshlist);

p.ref = [0,0,0];
p.axis = [0,0,-1];
p.name = 'VS';
prms{1} = p;

end

% -------------------------------------------------------------------------
% Frame for AOA and VS tapes
% -------------------------------------------------------------------------

function meshlist = make_tapeframe(this)
x0 = -this.w/2;
x1 = x0+this.w;
X0 = -this.tapew/2;
X1 = X0+this.tapew;
X2 = X1+this.readoutw;
y0 = -this.h/2;
y1 = y0+this.h;
Y0 = -this.tapeh/2;
Y1 = Y0+this.tapeh;
Y2 = 0.006;
z0 = 0;
z1 = z0-this.depth;
Z1 = z0-0.003;
Z2 = z1+0.004;
sw = 0.008;
s0w = 0.002;
s1w = 0.001;
u0 = this.u0;
u1 = this.u1;
v0 = this.v0;
v1 = this.v1;
u0s = this.screw.u0;
u1s = this.screw.u1;
v0s = this.screw.v0;
v1s = this.screw.v1;
u2s = u0s + (sw-s1w)/sw*(u1s-u0s);
v2s = v0s + (sw-s1w)/sw*(v1s-v0s);
vtx = [x0+sw,y0,z1,        0,0,-1, 0,0; ... % outer frame of top plate
       x1-sw,y0,z1,        0,0,-1, 0,0; ...
       x0+sw,y0+sw-s0w,z1, 0,0,-1, 0,0; ...
       x1-sw,y0+sw-s0w,z1, 0,0,-1, 0,0; ...
       x0+sw-s0w,y0+sw,z1, 0,0,-1, 0,0; ...
       x1-sw+s0w,y0+sw,z1, 0,0,-1, 0,0; ...
       x0,y0+sw,z1,        0,0,-1, 0,0; ...
       x1,y0+sw,z1,        0,0,-1, 0,0; ...
       x0,y1-sw,z1,        0,0,-1, 0,0; ...
       x1,y1-sw,z1,        0,0,-1, 0,0; ...
       x0+sw-s0w,y1-sw,z1, 0,0,-1, 0,0; ...
       x1-sw+s0w,y1-sw,z1, 0,0,-1, 0,0; ...
       x0+sw,y1-sw+s0w,z1, 0,0,-1, 0,0; ...
       x1-sw,y1-sw+s0w,z1, 0,0,-1, 0,0; ...
       x0+sw,y1,z1,        0,0,-1, 0,0; ...
       x1-sw,y1,z1,        0,0,-1, 0,0; ...
       
       X0+s0w,Y0,z1, 0,0,-1, 0,0; ... % inner frame of top plate (tape)
       X1-s0w,Y0,z1, 0,0,-1, 0,0; ...
       X0,Y0+s0w,z1, 0,0,-1, 0,0; ...
       X1,Y0+s0w,z1, 0,0,-1, 0,0; ...
       X0,Y1-s0w,z1, 0,0,-1, 0,0; ...
       X1,Y1-s0w,z1, 0,0,-1, 0,0; ...
       X0+s0w,Y1,z1, 0,0,-1, 0,0; ...
       X1-s0w,Y1,z1, 0,0,-1, 0,0; ...
       
       X1,-Y2,    z1, 0,0,-1, 0,0; ... % inner frame of top plate (readout)
       X1, Y2,    z1, 0,0,-1, 0,0; ...
       X1-s0w,Y1-s0w,Z2,-1/sqrt(2),-1/sqrt(2),0, 0,0; ...
       X1-s0w,Y0+s0w,Z2,-1/sqrt(2),1/sqrt(2),0, 0,0; ...
       X2-s0w,-Y2,z1, 0,0,-1, 0,0; ...
       X2-s0w, Y2,z1, 0,0,-1, 0,0; ...
       X2,-Y2+s0w,z1, 0,0,-1, 0,0; ...
       X2, Y2-s0w,z1, 0,0,-1, 0,0; ...
       
       x0-s1w,y0+sw,z1+s1w, -1,0,0, 0,0; ... % outer rim
       x0-s1w,y1-sw,z1+s1w, -1,0,0, 0,0; ...
       x1+s1w,y0+sw,z1+s1w,  1,0,0, 0,0; ...
       x1+s1w,y1-sw,z1+s1w,  1,0,0, 0,0; ...
       x0+sw,y0-s1w,z1+s1w, 0,-1,0, 0,0; ...
       x1-sw,y0-s1w,z1+s1w, 0,-1,0, 0,0; ...
       x0+sw,y1+s1w,z1+s1w,  0,1,0, 0,0; ...
       x1-sw,y1+s1w,z1+s1w,  0,1,0, 0,0; ...
       x0,y0+sw-s1w,z1+s1w,        0,-1,0, 0,0; ...
       x0+sw-s0w,y0+sw-s1w,z1+s1w, 0,-1,0, 0,0; ...
       x0,y1-sw+s1w,z1+s1w,         0,1,0, 0,0; ...
       x0+sw-s0w,y1-sw+s1w,z1+s1w,  0,1,0, 0,0; ...
       x1-sw+s0w,y0+sw-s1w,z1+s1w, 0,-1,0, 0,0; ...
       x1,y0+sw-s1w,z1+s1w,        0,-1,0, 0,0; ...
       x1-sw+s0w,y1-sw+s1w,z1+s1w,  0,1,0, 0,0; ...
       x1,y1-sw+s1w,z1+s1w,         0,1,0, 0,0; ...
       x0+sw-s1w,y0,z1+s1w,        -1,0,0, 0,0; ...
       x0+sw-s1w,y0+sw-s0w,z1+s1w, -1,0,0, 0,0; ...
       x0+sw-s1w,y1,z1+s1w,        -1,0,0, 0,0; ...
       x0+sw-s1w,y1-sw+s0w,z1+s1w, -1,0,0, 0,0; ...
       x1-sw+s1w,y0,z1+s1w,         1,0,0, 0,0; ...
       x1-sw+s1w,y0+sw-s0w,z1+s1w,  1,0,0, 0,0; ...
       x1-sw+s1w,y1,z1+s1w,         1,0,0, 0,0; ...
       x1-sw+s1w,y1-sw+s0w,z1+s1w,  1,0,0, 0,0; ...
       
       x0-s1w,y0+sw,Z1, -1,0,0, 0,0; ... % outer side walls
       x0-s1w,y1-sw,Z1, -1,0,0, 0,0; ...
       x0-s1w,y0,Z1, -1,0,0, 0,0; ...
       x0-s1w,y1,Z1, -1,0,0, 0,0; ...
       x0-s1w,y0,z0, -1,0,0, 0,0; ...
       x0-s1w,y1,z0, -1,0,0, 0,0; ...
       
       x1+s1w,y0+sw,Z1, 1,0,0, 0,0; ... 
       x1+s1w,y1-sw,Z1, 1,0,0, 0,0; ...
       x1+s1w,y0,Z1, 1,0,0, 0,0; ...
       x1+s1w,y1,Z1, 1,0,0, 0,0; ...
       x1+s1w,y0,z0, 1,0,0, 0,0; ...
       x1+s1w,y1,z0, 1,0,0, 0,0; ...
       
       x0+sw,y1+s1w,Z1, 0,1,0, 0,0; ...
       x1-sw,y1+s1w,Z1, 0,1,0, 0,0; ...
       x0,y1+s1w,Z1, 0,1,0, 0,0; ...
       x1,y1+s1w,Z1, 0,1,0, 0,0; ...
       x0,y1+s1w,z0, 0,1,0, 0,0; ...
       x1,y1+s1w,z0, 0,1,0, 0,0; ...
       
       x0+sw,y0-s1w,Z1, 0,-1,0, 0,0; ...
       x1-sw,y0-s1w,Z1, 0,-1,0, 0,0; ...
       x0,y0-s1w,Z1, 0,-1,0, 0,0; ...
       x1,y0-s1w,Z1, 0,-1,0, 0,0; ...
       x0,y0-s1w,z0, 0,-1,0, 0,0; ...
       x1,y0-s1w,z0, 0,-1,0, 0,0; ...
       
       x0,y1-sw+s1w,Z1,        0,1,0, 0,0; ...
       x0+sw-s0w,y1-sw+s1w,Z1, 0,1,0, 0,0; ...
       x0+sw-s1w,y1,Z1,       -1,0,0, 0,0; ...
       x0+sw-s1w,y1-sw+s0w,Z1,-1,0,0, 0,0; ...
       
       x1-sw+s0w,y1-sw+s1w,Z1, 0,1,0, 0,0; ...
       x1,y1-sw+s1w,Z1,        0,1,0, 0,0; ...
       x1-sw+s1w,y1,Z1,        1,0,0, 0,0; ...
       x1-sw+s1w,y1-sw+s0w,Z1, 1,0,0, 0,0; ...
       
       x1-sw+s0w,y0+sw-s1w,Z1, 0,-1,0, 0,0; ...
       x1,y0+sw-s1w,Z1,        0,-1,0, 0,0; ...
       x1-sw+s1w,y0,Z1,        1,0,0, 0,0; ...
       x1-sw+s1w,y0+sw-s0w,Z1, 1,0,0, 0,0; ...
       
       x0,y0+sw-s1w,Z1,        0,-1,0, 0,0; ...
       x0+sw-s0w,y0+sw-s1w,Z1, 0,-1,0, 0,0; ...
       x0+sw-s1w,y0,Z1,        -1,0,0, 0,0; ...
       x0+sw-s1w,y0+sw-s0w,Z1, -1,0,0, 0,0; ...
       
       x1-sw,y1-sw,Z1,  0,0,-1, u0s,v0s; ...  % screw plates (top right)
       x1+s1w,y1-sw,Z1, 0,0,-1, u1s,v0s; ...
       x1-sw,y1+s1w,Z1, 0,0,-1, u0s,v1s; ...
       x1,y1+s1w,Z1,    0,0,-1, u2s,v1s; ...
       x1+s1w,y1,Z1,    0,0,-1, u1s,v2s; ...
       
       x0+sw,y1-sw,Z1,  0,0,-1, u0s,v0s; ...  % top left
       x0-s1w,y1-sw,Z1, 0,0,-1, u1s,v0s; ...
       x0+sw,y1+s1w,Z1, 0,0,-1, u0s,v1s; ...
       x0,y1+s1w,Z1,    0,0,-1, u2s,v1s; ...
       x0-s1w,y1,Z1,    0,0,-1, u1s,v2s; ...
       
       x1-sw,y0+sw,Z1,  0,0,-1, u0s,v0s; ...  % bottom right
       x1+s1w,y0+sw,Z1, 0,0,-1, u1s,v0s; ...
       x1-sw,y0-s1w,Z1, 0,0,-1, u0s,v1s; ...
       x1,y0-s1w,Z1,    0,0,-1, u2s,v1s; ...
       x1+s1w,y0,Z1,    0,0,-1, u1s,v2s; ...
       
       x0+sw,y0+sw,Z1,  0,0,-1, u0s,v0s; ...  % bottom left
       x0-s1w,y0+sw,Z1, 0,0,-1, u1s,v0s; ...
       x0+sw,y0-s1w,Z1, 0,0,-1, u0s,v1s; ...
       x0,y0-s1w,Z1,    0,0,-1, u2s,v1s; ...
       x0-s1w,y0,Z1,    0,0,-1, u1s,v2s; ...
       
       X0+s0w,Y1-s0w,Z2, 0,-1,0, 0,0; ...       % inner rim
       X1-s0w,Y1-s0w,Z2, 0,-1,0, 0,0; ...
       X0+s0w,Y0+s0w,Z2,  1,0,0, 0,0; ...
       X0+s0w,Y1-s0w,Z2,  1,0,0, 0,0; ...
       X0+s0w,Y1-s0w,Z2, 1/sqrt(2),-1/sqrt(2),0, 0,0; ...
       X0+s0w,Y0+s0w,Z2,  0,1,0, 0,0; ...  
       X1-s0w,Y0+s0w,Z2,  0,1,0, 0,0; ...
       X0+s0w,Y0+s0w,Z2, 1/sqrt(2),1/sqrt(2),0, 0,0; ...
       X1-s0w,Y1-s0w,Z2, -1,0,0, 0,0; ...
       X1-s0w,Y2-s0w,Z2, -1,0,0, 0,0; ...
       X1-s0w,Y0+s0w,Z2, -1,0,0, 0,0; ...
       X1-s0w,-Y2+s0w,Z2,-1,0,0, 0,0; ...
       X2-s0w,-Y2+s0w,Z2,-1,0,0, 0,0; ...
       X2-s0w,Y2-s0w,Z2, -1,0,0, 0,0; ...
       X1-s0w,Y2-s0w,Z2,  0,-1,0, 0,0; ...
       X2-s0w,Y2-s0w,Z2, 0,-1,0, 0,0; ...
       X2-s0w,Y2-s0w,Z2, -1/sqrt(2),-1/sqrt(2),0, 0,0; ...
       X1-s0w,-Y2+s0w,Z2, 0,1,0, 0,0; ...
       X2-s0w,-Y2+s0w,Z2, 0,1,0, 0,0; ...
       X2-s0w,-Y2+s0w,Z2, -1/sqrt(2),1/sqrt(2),0, 0,0];
   
idx = [0,18,16; 17,19,1; 0,16,1; 1,16,17; 0,2,18; 1,19,3; 14,22,20; 15,21,23; 14,15,22; 22,15,23; ... % top plate
       12,14,20; 21,15,13; 6,8,10; 6,10,4; 4,10,12; 4,12,2; 2,12,20; 2,20,18; 25,21,13; 3,19,24; ...
       13,11,25; 3,24,5; 11,29,25; 5,24,28; 11,9,29; 5,28,7; 9,31,29; 7,28,30; ...
       7,30,31; 7,31,9; ...
       32,33,8; 32,8,6; 7,9,34; 34,9,35; 0,1,37; 0,37,36; 14,39,15; 14,38,39; ... % outer rim
       6,4,41; 6,41,40; 8,42,10; 10,42,43; 44,5,45; 5,7,45; 11,46,9; 9,46,47; 32,6,40; 8,33,42; 7,34,45; 9,47,35; ...
       2,0,48; 2,48,49; 14,12,51; 14,51,50; 0,36,48; 14,50,38; 4,2,49; 4,49,41; 12,10,51,; 10,43,51; ...
       3,52,1; 3,53,52; 15,55,13; 15,54,55; 1,52,37; 15,39,54; 3,5,53; 53,5,44; 13,55,11; 55,46,11; ...
       57,61,59; 57,60,61; 57,56,60; 56,58,60; 57,32,56; 57,33,32; ... % outer side walls
       63,65,67; 63,67,66; 63,66,62; 62,66,64; 63,62,34; 63,34,35; ...
       68,70,72; 68,72,73; 68,73,69; 69,73,71; 68,69,39; 68,39,38; ...
       74,78,76; 74,79,78; 74,75,79; 75,77,79; 75,74,36; 75,36,37; ...
       70,59,72; 72,59,61; 71,67,65; 71,73,67; 64,79,77; 64,66,79; 76,60,58; 76,78,60; ...
       42,80,43; 80,81,43; 42,33,57; 42,57,80; 50,51,83; 50,83,82; 38,50,82; 38,82,68; 51,43,83; 43,81,83; ...
       55,54,87; 54,86,87; 46,84,47; 84,85,47; 39,86,54; 39,69,86; 47,85,35; 85,63,35; 55,87,46; 87,84,46; ...
       45,88,44; 45,89,88; 53,90,52; 53,91,90; 34,89,45; 34,62,89; 52,75,37; 52,90,75; 44,91,53; 44,88,91; ...
       40,41,93; 40,93,92; 49,48,94; 49,94,95; 32,40,92; 32,92,56; 48,36,74; 48,74,94; 41,49,95; 41,95,93 ; ...
       96,98,99; 96,99,100; 96,100,97; ... % screw plates (top right)
       101,104,103; 101,105,104; 101,102,105; ... % top left
       106,107,110; 106,110,109; 106,109,108; ... % bottom right
       111,113,114; 111,114,115; 111,115,112; ... % bottom left
       22,23,116; 116,23,117; 18,20,118; 118,20,119; 20,22,120; ... % inner rim
       16,122,17; 16,121,122; 16,18,123; 21,25,124; 124,25,125; 23,21,26; ...
       24,19,127; 127,19,126; 19,17,27; 31,30,129; 129,30,128; 25,29,130; 130,29,131; 29,31,132; ...
       24,133,28; 28,133,134; 30,28,135];

for i=1:size(vtx,1)
    if vtx(i,7)==0, vtx(i,7) = u0 + (vtx(i,1)-x0)/(x1-x0)*(u1-u0); end
    if vtx(i,8)==0, vtx(i,8) = v0 + (vtx(i,2)-y0)/(y1-y0)*(v1-v0); end
end
meshlist = struct('vtx',vtx, 'idx',idx, 'grpname','VC4_DEFAULT', 'name','');
end

% -------------------------------------------------------------------------
% Propellant status display on lower mid panel
% -------------------------------------------------------------------------

function meshlist = make_psd(this)
x0  = 0;
x1  = x0 + this.frame.w;
X0  = x0 + this.frame.border_tb;
X1  = x1 - this.frame.border_tb;
X0f = x0 + this.frame.border_lr;
X1f = x1 - this.frame.border_lr;
y1  = 0;
y0  = y1 - this.frame.h;
Y0  = y0 + this.frame.border_tb;
Y1  = y1 - this.frame.border_tb;
z1  = 0;
z0  = z1 - this.frame.depth;
u0  = texcrd(16);
u1  = texcrd(112);
U0 = u0 + (X0-x0)/(x1-x0)*(u1-u0);
U1 = u0 + (X1-x0)/(x1-x0)*(u1-u0);
U0f = u0 + (X0f-x0)/(x1-x0)*(u1-u0);
U1f = u0 + (X1f-x0)/(x1-x0)*(u1-u0);
v0 = texcrd(54);
v1 = texcrd(24);
V0 = v0 + (Y0-y0)/(y1-y0)*(v1-v0);
V1 = v0 + (Y1-y0)/(y1-y0)*(v1-v0);
dv = -texcrd(12);
ds = this.frame.border_lr - this.frame.border_tb;
phi = atan(ds/this.frame.depth);
sp = sin(phi);
cp = cos(phi);
vtx = [x0,y0,z0, 0,0,-1, u0,v0; ... % front plate
    x1,y0,z0, 0,0,-1, u1,v0; ...
    x0,y1,z0, 0,0,-1, u0,v1; ...
    x1,y1,z0, 0,0,-1, u1,v1; ...
    X0,Y0,z0, 0,0,-1, U0,V0; ...
    X1,Y0,z0, 0,0,-1, U1,V0; ...
    X0,Y1,z0, 0,0,-1, U0,V1; ...
    X1,Y1,z0, 0,0,-1, U1,V1; ...
    X0,Y0,z0, cp,0,-sp, U0,V0+dv; ... % inner edge
    X0f,Y0,z1, cp,0,-sp, U0f,V0+dv; ...
    X0,Y1,z0,  cp,0,-sp, U0,V1+dv; ...
    X0f,Y1,z1, cp,0,-sp, U0f,V1+dv; ...
    X1,Y0,z0, -cp,0,-sp, U1,V0+dv; ...
    X1f,Y0,z1, -cp,0,-sp, U1f,V0+dv; ...
    X1,Y1,z0,  -cp,0,-sp, U1,V1+dv; ...
    X1f,Y1,z1, -cp,0,-sp, U1f,V1+dv; ...
    X0,Y0,z0, 0,1,0, U0,V0+dv; ...
    X1,Y0,z0, 0,1,0, U1,V0+dv; ...
    X0f,Y0,z1, 0,1,0, U0f,V0+dv; ...
    X1f,Y0,z1, 0,1,0, U1f,V0+dv; ...
    X0,Y1,z0, 0,-1,0, U0,V1+dv; ...
    X1,Y1,z0, 0,-1,0, U1,V1+dv; ...
    X0f,Y1,z1, 0,-1,0, U0f,V1+dv; ...
    X1f,Y1,z1, 0,-1,0, U1f,V1+dv; ...
    x0,y1,z0, 0,1,0, u0,v1+dv; ... % outer edge
    x1,y1,z0, 0,1,0, u1,v1+dv; ...
    x0,y1,z1, 0,1,0, u0,v1+dv; ...
    x1,y1,z1, 0,1,0, u1,v1+dv; ...
    x0,y0,z0, 1,0,0, u1,v0+dv; ...
    x0,y1,z0, 1,0,0, u1,v1+dv; ...
    x0,y0,z1, 1,0,0, u1,v0+dv; ...
    x0,y1,z1, 1,0,0, u1,v1+dv];
idx = [0,4,1; 1,4,5; 1,5,3; 3,5,7; 3,7,2; 2,7,6; 2,6,0; 0,6,4; ...
    8,11,9; 8,10,11; 12,13,14; 13,15,14; 16,18,17; 17,18,19; 20,21,22; 21,23,22; ...
    25,24,26; 25,26,27; 28,30,29; 30,31,29];

meshlist(1) = struct('vtx',vtx, 'idx',idx, 'grpname','VC4_DEFAULT', 'name','');

[vtx,idx] = fuel_disp('VC',this);
meshlist(2) = struct('vtx',vtx, 'idx',idx, 'grpname','PROPELLANT_STATUS', 'name','');

% map into parent frame
meshlist = transform(this,meshlist);
end

% -------------------------------------------------------------------------
% Gimbal/hover control panel on lower mid panel
% -------------------------------------------------------------------------

function [meshlist,prms] = make_gimbalpanel(this)
gdisp = this.gdisp;
hdisp = this.hdisp;
adisp = this.adisp;
z0 = 0;
y1 = 0;
y0 = y1 - this.h;
x0 = 0;
x1 = x0 + this.w;
X0 = x0 + gdisp.ref(1);
X1 = X0 + gdisp.w;
X0h = x0 + hdisp.ref(1);
X1h = X0h + hdisp.w;
Y1 = y1 + gdisp.ref(2);
Y0 = Y1 - gdisp.h;
X0a = x0 + adisp.ref(1);
X1a = X0a + adisp.w;
Y1a = y1 + adisp.ref(2);
Y0a = Y1a - adisp.h;
Z0 = z0 + gdisp.ref(3);
rim = gdisp.rim;
rim2 = rim/2;
orim = 0.002;
vtx = [x0,y0,z0, 0,0,-1, 0,0; ... % panel background
    x1,y0,z0, 0,0,-1, 0,0; ...
    x0,y1,z0, 0,0,-1, 0,0; ...
    x1,y1,z0, 0,0,-1, 0,0; ...
    X0-rim,Y0,z0, 0,0,-1, 0,0; ... % (hole for gimbal display)
    X0,Y0-rim,z0, 0,0,-1, 0,0; ...
    X1,Y0-rim,z0, 0,0,-1, 0,0; ...
    X1+rim,Y0,z0, 0,0,-1, 0,0; ...
    X1+rim,Y1,z0, 0,0,-1, 0,0; ...
    X1,Y1+rim,z0, 0,0,-1, 0,0; ...
    X0,Y1+rim,z0, 0,0,-1, 0,0; ...
    X0-rim,Y1,z0, 0,0,-1, 0,0; ...
    X0h-rim,Y0,z0, 0,0,-1, 0,0; ... % (hole for hover display)
    X0h,Y0-rim,z0, 0,0,-1, 0,0; ...
    X1h,Y0-rim,z0, 0,0,-1, 0,0; ...
    X1h+rim,Y0,z0, 0,0,-1, 0,0; ...
    X1h+rim,Y1,z0, 0,0,-1, 0,0; ...
    X1h,Y1+rim,z0, 0,0,-1, 0,0; ...
    X0h,Y1+rim,z0, 0,0,-1, 0,0; ...
    X0h-rim,Y1,z0, 0,0,-1, 0,0; ...
    X0a-rim,Y0a,z0, 0,0,-1, 0,0; ... % (hole for holdalt display)
    X0a,Y0a-rim,z0, 0,0,-1, 0,0; ...
    X1a,Y0a-rim,z0, 0,0,-1, 0,0; ...
    X1a+rim,Y0a,z0, 0,0,-1, 0,0; ...
    X1a+rim,Y1a,z0, 0,0,-1, 0,0; ...
    X1a,Y1a+rim,z0, 0,0,-1, 0,0; ...
    X0a,Y1a+rim,z0, 0,0,-1, 0,0; ...
    X0a-rim,Y1a,z0, 0,0,-1, 0,0; ...

    X0+rim2,Y0,Z0, 0,1,0,  0,0; ...  % gimbal display rim
    X1-rim2,Y0,Z0, 0,1,0,  0,0; ...
    X1,Y0+rim2,Z0, -1,0,0, 0,0; ...
    X1,Y1-rim2,Z0, -1,0,0, 0,0; ...
    X1-rim2,Y1,Z0, 0,-1,0, 0,0; ...
    X0+rim2,Y1,Z0, 0,-1,0, 0,0; ...
    X0,Y1-rim2,Z0, 1,0,0,  0,0; ...
    X0,Y0+rim2,Z0, 1,0,0,  0,0; ...

    X0h+rim2,Y0,Z0, 0,1,0,  0,0; ...  % hover display rim
    X1h-rim2,Y0,Z0, 0,1,0,  0,0; ...
    X1h,Y0+rim2,Z0, -1,0,0, 0,0; ...
    X1h,Y1-rim2,Z0, -1,0,0, 0,0; ...
    X1h-rim2,Y1,Z0, 0,-1,0, 0,0; ...
    X0h+rim2,Y1,Z0, 0,-1,0, 0,0; ...
    X0h,Y1-rim2,Z0, 1,0,0,  0,0; ...
    X0h,Y0+rim2,Z0, 1,0,0,  0,0; ...

    X0a+rim2,Y0a,Z0, 0,1,0,  0,0; ...  % holdalt display rim
    X1a-rim2,Y0a,Z0, 0,1,0,  0,0; ...
    X1a,Y0a+rim2,Z0, -1,0,0, 0,0; ...
    X1a,Y1a-rim2,Z0, -1,0,0, 0,0; ...
    X1a-rim2,Y1a,Z0, 0,-1,0, 0,0; ...
    X0a+rim2,Y1a,Z0, 0,-1,0, 0,0; ...
    X0a,Y1a-rim2,Z0, 1,0,0,  0,0; ...
    X0a,Y0a+rim2,Z0, 1,0,0,  0,0; ...

    x1+orim,y0,z0+orim, 1,0,0, 0,0; ... % panel outer rim
    x1+orim,y1,z0+orim, 1,0,0, 0,0; ...
    x1,y1+orim,z0+orim, 0,1,0, 0,0; ...
    x0,y1+orim,z0+orim, 0,1,0, 0,0; ...
    x0-orim,y1,z0+orim, -1,0,0, 0,0; ...
    x0-orim,y0,z0+orim, -1,0,0, 0,0; ...
    x0,y0-orim,z0+orim, 0,-1,0, 0,0; ...
    x1,y0-orim,z0+orim, 0,-1,0, 0,0];

idx = [0,4,5; 0,5,1; 5,14,1; 14,15,1; 15,16,1; 6,7,13; 7,12,13; 7,8,12; 8,19,12; 8,9,19; 9,18,19; 0,11,4; 0,2,11; 11,2,10; 10,2,17; 17,2,20; 20,2,27; 27,2,26; 26,2,3; 26,3,25; 25,3,24; 24,3,23; 23,3,1; 22,23,1; 21,22,1; 16,21,1; 16,20,21; 16,17,20; ...
    5,28,6; 6,28,29; 7,30,8; 8,30,31; 6,29,7; 7,29,30; 9,32,10; 10,32,33; 8,31,9; 9,31,32; 11,34,4; 4,34,35; 10,33,11; 11,33,34; 4,35,5; 35,28,5; ...
    13,36,14; 14,36,37; 15,38,16; 16,38,39; 14,37,15; 15,37,38; 17,40,18; 18,40,41; 16,39,17; 17,39,40; 19,42,12; 12,42,43; 18,41,19; 19,41,42; 12,43,13; 43,36,13; ...
    21,44,22; 22,44,45; 23,46,24; 24,46,47; 22,45,23; 23,45,46; 25,48,26; 26,48,49; 24,47,25; 25,47,48; 27,50,20; 20,50,51; 26,49,27; 27,49,50; 20,51,21; 51,44,21; ...
    1,3,52; 52,3,53; 3,2,54; 54,2,55; 3,54,53; 2,0,56; 56,0,57; 2,56,55; 0,1,58; 58,1,59; 0,58,57; 1,52,59];

for i=1:size(vtx,1)
    vtx(i,7) = (vtx(i,1)-x0)/0.1;
    vtx(i,8) = (vtx(i,2)-y0)/0.1;
end

meshlist(1) = struct('vtx',vtx, 'idx',idx, 'grpname','PANELS1', 'name','');
prms = {};

% make children
sublist = make_gimbalpanel_label (this.label);
meshlist = meshmerge(meshlist, sublist);

[sublist,subprms] = make_gimbal_gdial(this.mdial);
meshlist = meshmerge(meshlist, sublist);
prms = [prms; subprms];

[sublist,subprms] = make_gimbal_hdial(this.hdial);
meshlist = meshmerge(meshlist, sublist);
prms = [prms; subprms];

[sublist,subprms] = make_gimbal_gswitches(this.switch);
meshlist = meshmerge(meshlist, sublist);
prms = [prms; subprms];

[sublist,subprms] = make_gimbal_hswitches(this.switch);
meshlist = meshmerge(meshlist, sublist);
prms = [prms; subprms];

[sublist,subprms] = make_gimbal_holdaltswitch(this.switch);
meshlist = meshmerge(meshlist, sublist);
prms = [prms; subprms];

[sublist,subprms] = make_gimbal_display(this.gdisp);
meshlist = meshmerge(meshlist, sublist);
prms = [prms; subprms];

[sublist,subprms] = make_hover_display(this.hdisp);
meshlist = meshmerge(meshlist, sublist);
prms = [prms; subprms];

[sublist,subprms] = make_hover_alt(this.halt);
meshlist = meshmerge(meshlist, sublist);
prms = [prms; subprms];

% map into parent frame
meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);
end

% -------------------------------------------------------------------------
% Gimbal/hover control panel labels
% -------------------------------------------------------------------------

function meshlist = make_gimbalpanel_label(this)
global g;
F = g.defscale;
z0 = g.label_ofs;
x0 = 0;
y0 = -this.h;
vtx = [F*  5, F*177, z0, 0,0,-1, texcrd(  0), texcrd( 12); ...  % "GIMBAL CTRL"
    F*202, F*177, z0, 0,0,-1, texcrd(197), texcrd( 12); ...
    F*  5, F*189, z0, 0,0,-1, texcrd(  0), texcrd(  0); ...
    F*202, F*189, z0, 0,0,-1, texcrd(197), texcrd(  0); ...
    F*225, F*177, z0, 0,0,-1, texcrd(  0), texcrd( 24); ... % "HOVER ATT CTRL"
    F*402, F*177, z0, 0,0,-1, texcrd(177), texcrd( 24); ...
    F*225, F*189, z0, 0,0,-1, texcrd(  0), texcrd( 12); ...
    F*402, F*189, z0, 0,0,-1, texcrd(177), texcrd( 12); ...
    F*426, F*177, z0, 0,0,-1, texcrd(  0), texcrd( 36); ... % "HOVER ALT"
    F*507, F*177, z0, 0,0,-1, texcrd( 81), texcrd( 36); ...
    F*426, F*189, z0, 0,0,-1, texcrd(  0), texcrd( 24); ...
    F*507, F*189, z0, 0,0,-1, texcrd( 81), texcrd( 24); ...
    F*211, F* 11, z0, 0,0,-1, texcrd(  0), texcrd(212); ... % vertical separator
    F*215, F* 11, z0, 0,0,-1, texcrd(  4), texcrd(212); ...
    F*211, F*187, z0, 0,0,-1, texcrd(  0), texcrd( 36); ...
    F*215, F*187, z0, 0,0,-1, texcrd(  4), texcrd( 36); ...
    F*412, F* 11, z0, 0,0,-1, texcrd(  0), texcrd(212); ... % vertical separator
    F*416, F* 11, z0, 0,0,-1, texcrd(  4), texcrd(212); ...
    F*412, F*187, z0, 0,0,-1, texcrd(  0), texcrd( 36); ...
    F*416, F*187, z0, 0,0,-1, texcrd(  4), texcrd( 36); ...
    F* 10, F*163, z0, 0,0,-1, texcrd(  4), texcrd( 36); ... % gimbal dial labels + "MAN" bracket
    F*131, F*163, z0, 0,0,-1, texcrd(125), texcrd( 36); ...
    F*131, F* 11, z0, 0,0,-1, texcrd(125), texcrd(188); ...
    F*122, F* 11, z0, 0,0,-1, texcrd(116), texcrd(188); ...
    F*122, F*121, z0, 0,0,-1, texcrd(116), texcrd( 78); ...
    F* 10, F*121, z0, 0,0,-1, texcrd(  4), texcrd( 78); ...
    F*224, F*163, z0, 0,0,-1, texcrd(  4), texcrd( 36); ... % hover dial labels + "MAN" bracket
    F*345, F*163, z0, 0,0,-1, texcrd(125), texcrd( 36); ...
    F*345, F* 11, z0, 0,0,-1, texcrd(125), texcrd(188); ...
    F*336, F* 11, z0, 0,0,-1, texcrd(116), texcrd(188); ...
    F*336, F*121, z0, 0,0,-1, texcrd(116), texcrd( 78); ...
    F*224, F*121, z0, 0,0,-1, texcrd(  4), texcrd( 78); ...
    F*133, F* 11, z0, 0,0,-1, texcrd(125), texcrd(187); ... % gimbal man control labels
    F*201, F* 11, z0, 0,0,-1, texcrd(193), texcrd(187); ...
    F*133, F*162, z0, 0,0,-1, texcrd(125), texcrd( 36); ...
    F*201, F*162, z0, 0,0,-1, texcrd(193), texcrd( 36); ...
    F*347, F* 37, z0, 0,0,-1, texcrd(  4), texcrd(206); ... % hover man control labels
    F*403, F* 37, z0, 0,0,-1, texcrd( 60), texcrd(206); ...
    F*347, F*162, z0, 0,0,-1, texcrd(  4), texcrd( 81); ...
    F*403, F*162, z0, 0,0,-1, texcrd( 60), texcrd( 81); ...
    F*424, F* 59, z0, 0,0,-1, texcrd(  1), texcrd(252); ... % hover alt labels
    F*506, F* 59, z0, 0,0,-1, texcrd( 83), texcrd(252); ...
    F*424, F* 93, z0, 0,0,-1, texcrd(  1), texcrd(218); ...
    F*506, F* 93, z0, 0,0,-1, texcrd( 83), texcrd(218)];
vtx(:,1) = vtx(:,1) + x0;
vtx(:,2) = vtx(:,2) + y0;
idx = [0,2,1; 1,2,3; ...
    4,6,5; 5,6,7; ...
    8,10,9; 9,10,11; ...
    12,14,13; 13,14,15; ...
    16,18,17; 17,18,19; ...
    20,24,25; 20,21,24; 21,22,24; 24,22,23; ...
    26,30,31; 26,27,30; 27,28,30; 30,28,29; ...
    32,34,33; 33,34,35; ...
    36,38,37; 37,38,39; ...
    40,42,41; 41,42,43];
meshlist = struct ('vtx',vtx, 'idx',idx, 'grpname','LIT_LABELS', 'name','');

% map into parent frame
meshlist = transform(this,meshlist);
end

% Gimbal control dial
function [meshlist,prms] = make_gimbal_gdial(this)
[meshlist,prms] = dial2(this.rad,this.h);
meshlist(1).grpname = 'DIAL1';
meshlist(1).name = 'GIMBAL_DIAL';
prms{1}.name = 'GIMBAL_DIAL';

% map into parent frame
meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);
end

% Hover control dial
function [meshlist,prms] = make_gimbal_hdial(this)
[meshlist,prms] = dial2(this.rad,this.h);
meshlist(1).grpname = 'DIAL1';
meshlist(1).name = 'HOVER_DIAL';
prms{1}.name = 'HOVER_DIAL';

% map into parent frame
meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);
end

% Gimbal manual switches
function [meshlist,prms] = make_gimbal_gswitches(this)
[meshlist,prms] = switch2_pair(this.len,this.wid,'v','GIMBAL_PSWITCH');
meshlist = transform(this.gp,meshlist);
prms = transform_prms(this.gp,prms);

[sublist,subprms] = switch2_pair(this.len,this.wid,'h','GIMBAL_YSWITCH');
sublist = transform(this.gy,sublist);
subprms = transform_prms(this.gy,subprms);
meshlist = meshmerge(meshlist,sublist);
prms = [prms; subprms];

meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);
end

% Hover manual switches
function [meshlist,prms] = make_gimbal_hswitches(this)
[meshlist,prms] = switch2(this.len,this.wid,'v','HOVER_PSWITCH');
meshlist = transform(this.hp,meshlist);
prms = transform_prms(this.hp,prms);

[sublist,subprms] = switch2(this.len,this.wid,'h','HOVER_RSWITCH');
sublist = transform(this.hr,sublist);
subprms = transform_prms(this.hr,subprms);
meshlist = meshmerge(meshlist,sublist);
prms = [prms; subprms];

meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);
end

% Hover hold alt preset selector switch
function [meshlist,prms] = make_gimbal_holdaltswitch(this)
[meshlist,prms] = switch2(this.len,this.wid,'h','HOVER_HOLDALT_SWITCH');
meshlist = transform(this.ha,meshlist);
prms = transform_prms(this.ha,prms);

meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);
end

% -------------------------------------------------------------------------
% Gimbal/hover indicator displays
% -------------------------------------------------------------------------

function [meshlist,prms] = make_gimbal_display(this)
global g;
x0 = 0;
x1 = x0 + this.w;
y1 = 0;
y0 = y1 - this.h;
z0 = 0;
u0 = this.u0;
u1 = this.u1;
v0 = this.v0;
v1 = this.v1;
vtx = [x0,y0,z0, 0,0,-1, u0,v0; ...  % gimbal display background
    x1,y0,z0, 0,0,-1, u1,v0; ...
    x0,y1,z0, 0,0,-1, u0,v1; ...
    x1,y1,z0, 0,0,-1, u1,v1];
idx = [0,2,1; 1,2,3];

meshlist(1) = struct('vtx',vtx, 'idx',idx, 'grpname','VC4_LIT', 'name','');

% the indicator needles
ind = this.ind;
x0L = ind.cnt0x - ind.dx;
x1L = x0L + 2*ind.dx;
x0R = ind.cnt1x - ind.dx;
x1R = x0R + 2*ind.dx;
y1  = ind.cnty + ind.dx;
y0  = y1 - 2*ind.dx;
z0  = -0.0002;
z1  = z0 - 0.0002;
u0  = ind.u0;
u1  = u0 + texcrd(9,g.texsize_instr.w);
v1t = ind.v0;
v0t = v1t + texcrd(9,g.texsize_instr.h);
v1c = ind.v1;
v0c = v1c + texcrd(9,g.texsize_instr.h);
vtx = [x0L,y0,z0, 0,0,-1, u0,v0c; ... % left indicator command value
    x1L,y0,z0, 0,0,-1, u1,v0c; ...
    x0L,y1,z0, 0,0,-1, u0,v1c; ...
    x1L,y1,z0, 0,0,-1, u1,v1c; ...
    x0L,y0,z1, 0,0,-1, u0,v0t; ... % left indicator true value
    x1L,y0,z1, 0,0,-1, u1,v0t; ...
    x0L,y1,z1, 0,0,-1, u0,v1t; ...
    x1L,y1,z1, 0,0,-1, u1,v1t];
idx = [0,2,1; 1,2,3; ...
    4,6,5; 5,6,7];
meshlist(2) = struct('vtx',vtx, 'idx',idx, 'grpname','VC_INSTR', 'name','GIMBAL_INDICATOR_LEFT');

vtx = [x0R,y0,z0, 0,0,-1, u0,v0c; ... % right indicator command value
    x1R,y0,z0, 0,0,-1, u1,v0c; ...
    x0R,y1,z0, 0,0,-1, u0,v1c; ...
    x1R,y1,z0, 0,0,-1, u1,v1c; ...
    x0R,y0,z1, 0,0,-1, u0,v0t; ... % right indicator true value
    x1R,y0,z1, 0,0,-1, u1,v0t; ...
    x0R,y1,z1, 0,0,-1, u0,v1t; ...
    x1R,y1,z1, 0,0,-1, u1,v1t];
idx = [0,2,1; 1,2,3; ...
    4,6,5; 5,6,7];
meshlist(3) = struct('vtx',vtx, 'idx',idx, 'grpname','VC_INSTR', 'name','GIMBAL_INDICATOR_RIGHT');

p.ref = [ind.cnt0x,ind.cnty,0; ind.cnt1x,ind.cnty,0];
p.name = 'GIMBAL_INDICATOR';
prms{1} = p;
%p.ref = [ind.cnt1x,ind.cnty,0];
%p.name = 'GIMBAL_INDICATOR_RIGHT';
%prms = [prms; p];

% map into parent frame
meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);
end

function [meshlist,prms] = make_hover_display(this)
global g;
x0 = 0;
x1 = x0 + this.w;
y1 = 0;
y0 = y1 - this.h;
z0 = 0;
u0 = this.u0;
u1 = this.u1;
v0 = this.v0;
v1 = this.v1;
vtx = [x0,y0,z0, 0,0,-1, u0,v0; ...  % hover display background
    x1,y0,z0, 0,0,-1, u1,v0; ...
    x0,y1,z0, 0,0,-1, u0,v1; ...
    x1,y1,z0, 0,0,-1, u1,v1];
idx = [0,2,1; 1,2,3];

meshlist(1) = struct('vtx',vtx, 'idx',idx, 'grpname','VC4_LIT', 'name','');

% the indicator needles
ind = this.ind;
x0  = ind.cntx - ind.dx;
x1  = x0 + 2*ind.dx;
y1  = ind.cnty + ind.dx;
y0  = y1 - 2*ind.dx;
z0  = -0.0002;
z1  = z0 - 0.0002;
u0  = ind.u0;
u1  = u0 + texcrd(9,g.texsize_instr.w);
v1t = ind.v0;
v0t = v1t + texcrd(9,g.texsize_instr.h);
v1c = ind.v1;
v0c = v1c + texcrd(9,g.texsize_instr.h);
vtx = [x0,y0,z0, 0,0,-1, u0,v0c; ... % indicator command value
    x1,y0,z0, 0,0,-1, u1,v0c; ...
    x0,y1,z0, 0,0,-1, u0,v1c; ...
    x1,y1,z0, 0,0,-1, u1,v1c; ...
    x0,y0,z1, 0,0,-1, u0,v0t; ... % indicator true value
    x1,y0,z1, 0,0,-1, u1,v0t; ...
    x0,y1,z1, 0,0,-1, u0,v1t; ...
    x1,y1,z1, 0,0,-1, u1,v1t];
idx = [0,2,1; 1,2,3; ...
    4,6,5; 5,6,7];

meshlist(2) = struct('vtx',vtx, 'idx',idx, 'grpname','VC_INSTR', 'name','HOVER_INDICATOR');

p.ref = [ind.cntx,ind.cnty,0];
p.name = 'HOVER_INDICATOR';
prms{1} = p;

% map into parent frame
meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);
end

function [meshlist,prms] = make_hover_alt(this)

% hover hold alt button
[meshlist,prms] = button3(this.btn,'BTN_HOVER_HOLDALT');
meshlist = transform(this.btn,meshlist);
prms = transform_prms(this.btn,prms);

% hover hold alt display
sublist = make_hoverholddisp(this.disp);
meshlist = meshmerge(meshlist, sublist);

% hover hold alt "current" button
[sublist,subprms] = make_hoverholdcurbtn(this.curbtn);
meshlist = meshmerge(meshlist, sublist);
prms = [prms; subprms];

% hover hold alt selector buttons (ALT/VSPD)
[sublist,subprms] = make_hoverholdmodebtns(this.modebtns);
meshlist = meshmerge(meshlist,sublist);
prms = [prms; subprms];

% map into parent frame
meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);
end

function meshlist = make_hoverholddisp(this)
x0 = 0;
x1 = x0 + this.w;
y1 = 0;
y0 = y1 - this.h;
z0 = this.depth;
u0 = 306.5/512;
u1 = 385.5/512;
v1 = 0.5/512;
v0 = 25.5/512;
vtx = [x0,y0,z0, 0,0,-1, u0,v0; ...
    x1,y0,z0, 0,0,-1, u1,v0; ...
    x0,y1,z0, 0,0,-1, u0,v1; ...
    x1,y1,z0, 0,0,-1, u1,v1];
idx = [0,2,1; 1,2,3];
meshlist = struct('vtx',vtx, 'idx',idx, 'grpname','BLITTGT1', 'name','');

% map into parent frame
meshlist = transform(this,meshlist);
end

function [meshlist,prms] = make_hoverholdcurbtn(this)
[meshlist,prms] = button2(this.w,this.h,this.depth,'BTN_HOVER_HOLDALT_CUR');

% map into parent frame
meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);
end

function [meshlist,prms] = make_hoverholdmodebtns(this)
global g;
meshlist = [];
p.mousearea = [-32,-17,0; 32,-17,0; -32,15,0; 32,15,0]*g.defscale;
p.name = 'HOVERMODE_BUTTONS';
prms{1} = p;

for i=1:2
    [sublist,subprms] = button3(this.btn(i), ['BTN_HOVERMODE_' num2str(i)]);
    sublist = transform(this.btn(i),sublist);
    subprms = transform_prms(this.btn(i),subprms);
    meshlist = meshmerge(meshlist,sublist);
    prms = [prms;subprms];
end

% map into parent frame
meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);
end

% =========================================================================
% Generate vertices and faces for left or right MFD
function meshlist = make_mfdframe(this)
% MFD geometry
z1  = 0;
z0  = z1 - this.outer_depth;
Z1  = z0 + this.inner_depth;
y1  = this.h/2;
y0  = y1 - this.h;
if this.pos == -1
    x1 = -this.cnt_dst;
    x0 = x1 - this.w;
else
    x0 = this.cnt_dst;
    x1 = x0 + this.w;
end
x0c = x0 + this.corner;
x1c = x1 - this.corner;
y0c = y0 + this.corner;
y1c = y1 - this.corner;
X0  = x0 + this.lr_frmw;
X1  = x1 - this.lr_frmw;
Y0  = y0 + this.b_frmh;
Y1  = y1 - this.t_frmh;
X0c = X0 + this.icorner;
X1c = X1 - this.icorner;
Y0c = Y0 + this.icorner;
Y1c = Y1 - this.icorner;
t0 = this.t0;
t1 = this.t1;
t0c = (x0c-x0)/(x1-x0)*(t1-t0) + t0;
t1c = (x1c-x0)/(x1-x0)*(t1-t0) + t0;
T0 = (X0-x0)/(x1-x0)*(t1-t0) + t0;
T1 = (X1-x0)/(x1-x0)*(t1-t0) + t0;
T0c = (X0c-x0)/(x1-x0)*(t1-t0) + t0;
T1c = (X1c-x0)/(x1-x0)*(t1-t0) + t0;
v0 = this.v0;
v1 = this.v1;
v0c = (y0c-y0)/(y1-y0)*(v1-v0) + v0;
v1c = (y1c-y0)/(y1-y0)*(v1-v0) + v0;
V0 = (Y0-y0)/(y1-y0)*(v1-v0) + v0;
V1 = (Y1-y0)/(y1-y0)*(v1-v0) + v0;
V0c = (Y0c-y0)/(y1-y0)*(v1-v0) + v0;
V1c = (Y1c-y0)/(y1-y0)*(v1-v0) + v0;
vtx = [];
idx = [];
% MFD - front surface
mvtx = [x0c,y0,z0,  0,0,-1, t0c,v0; ...
    x1c,y0,z0,  0,0,-1, t1c,v0; ...
    x0,y0c,z0,  0,0,-1, t0,v0c; ...
    x1,y0c,z0,  0,0,-1, t1,v0c; ...
    x0,y1c,z0,  0,0,-1, t0,v1c; ...
    x1,y1c,z0,  0,0,-1, t1,v1c; ...
    x0c,y1,z0,  0,0,-1, t0c,v1; ...
    x1c,y1,z0,  0,0,-1, t1c,v1; ...
    X0c,Y0,z0,  0,0,-1, T0c,V0; ...
    X1c,Y0,z0,  0,0,-1, T1c,V0; ...
    X0,Y0c,z0,  0,0,-1, T0,V0c; ...
    X1,Y0c,z0,  0,0,-1, T1,V0c; ...
    X0,Y1c,z0,  0,0,-1, T0,V1c; ...
    X1,Y1c,z0,  0,0,-1, T1,V1c; ...
    X0c,Y1,z0,  0,0,-1, T0c,V1; ...
    X1c,Y1,z0,  0,0,-1, T1c,V1];
midx = [0,8,1; 8,9,1; 1,9,3; 3,9,11; 3,11,5; 5,11,13; 5,13,7; 7,13,15; 7,15,6; 6,15,14; 6,14,4; 4,14,12; 4,12,2; 2,12,10; 2,10,0; 0,10,8];
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);
% MFD - inner edge
ds = this.infrm_w;
phi = atan(ds/this.inner_depth);
sp = sin(phi);
cp = cos(phi);
mvtx = [X0c,Y0,z0,  0,cp,-sp, 0,0; ...
    X1c,Y0,z0,  0,cp,-sp, 0,0; ...
    X0,Y0c,z0,  cp,0,-sp, 0,0; ...
    X1,Y0c,z0,  -cp,0,-sp, 0,0; ...
    X0,Y1c,z0,  cp,0,-sp, 0,0; ...
    X1,Y1c,z0,  -cp,0,-sp, 0,0; ...
    X0c,Y1,z0,  0,-cp,-sp, 0,0; ...
    X1c,Y1,z0,  0,-cp,-sp, 0,0; ...
    X0c+ds,Y0+ds,Z1,  0,cp,-sp, 0,0; ...
    X1c-ds,Y0+ds,Z1,  0,cp,-sp, 0,0; ...
    X0+ds,Y0c+ds,Z1,  cp,0,-sp, 0,0; ...
    X1-ds,Y0c+ds,Z1,  -cp,0,-sp, 0,0; ...
    X0+ds,Y1c-ds,Z1,  cp,0,-sp, 0,0; ...
    X1-ds,Y1c-ds,Z1,  -cp,0,-sp, 0,0; ...
    X0c+ds,Y1-ds,Z1,  0,-cp,-sp, 0,0; ...
    X1c-ds,Y1-ds,Z1,  0,-cp,-sp, 0,0];
midx = [0,8,1; 8,9,1; 1,9,3; 3,9,11; 3,11,5; 5,11,13; 5,13,7; 7,13,15; 7,15,6; 6,15,14; 6,14,4; 4,14,12; 4,12,2; 2,12,10; 2,10,0; 0,10,8];
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);
% MFD - outer edge (incomplete because of limited visibility)
if this.pos <= 0
    mvtx = [x1c,y1,z0,  0,1,0, 0,0; ...
        x1c,y1,z1,  0,1,0, 0,0; ...
        x1,y1c,z0,  1,0,0, 0,0; ...
        x1,y1c,z1,  1,0,0, 0,0; ...
        x1,y0c,z0,  1,0,0, 0,0; ...
        x1,y0c,z1,  1,0,0, 0,0];
    midx = [0,1,2; 1,3,2; 2,3,4; 3,5,4];
    [vtx,idx] = add_to_group(vtx,idx,mvtx,midx);
end
if this.pos >= 0
    mvtx = [x0c,y1,z1,  0,1,0, 0,0; ...
        x0c,y1,z0,  0,1,0, 0,0; ...
        x0,y1c,z1,  -1,0,0, 0,0; ...
        x0,y1c,z0,  -1,0,0, 0,0; ...
        x0,y0c,z1,  -1,0,0, 0,0; ...
        x0,y0c,z0,  -1,0,0, 0,0];
    midx = [0,1,2; 1,3,2; 2,3,4; 3,5,4];
    [vtx,idx] = add_to_group(vtx,idx,mvtx,midx);
end
if this.pos == 0
    mvtx = [];
    midx = [-5,-11,-12; -5,-6,-11];
    [vtx,idx] = add_to_group(vtx,idx,mvtx,midx);
end
meshlist = struct('vtx',vtx, 'idx',idx, 'grpname','VC4_DEFAULT', 'name','');
end

% =========================================================================
% OBSOLETE
% Generate vertices and faces for left or right MFD
function [vtx,idx] = make_mfd(zref,yref,prm,pos)
% MFD geometry
z1  = zref;
z0  = z1 - prm.outer_depth;
Z1  = z0 + prm.inner_depth;
y1  = yref - prm.top_dst;
y0  = y1 - prm.h;
if pos == -1
    x1 = -prm.cnt_dst;
    x0 = x1 - prm.w;
else
    x0 = prm.cnt_dst;
    x1 = x0 + prm.w;
end
x0c = x0 + prm.corner;
x1c = x1 - prm.corner;
y0c = y0 + prm.corner;
y1c = y1 - prm.corner;
X0  = x0 + prm.lr_frmw;
X1  = x1 - prm.lr_frmw;
Y0  = y0 + prm.b_frmh;
Y1  = y1 - prm.t_frmh;
X0c = X0 + prm.icorner;
X1c = X1 - prm.icorner;
Y0c = Y0 + prm.icorner;
Y1c = Y1 - prm.icorner;
t0 = prm.t0;
t1 = prm.t1;
t0c = (x0c-x0)/(x1-x0)*(t1-t0) + t0;
t1c = (x1c-x0)/(x1-x0)*(t1-t0) + t0;
T0 = (X0-x0)/(x1-x0)*(t1-t0) + t0;
T1 = (X1-x0)/(x1-x0)*(t1-t0) + t0;
T0c = (X0c-x0)/(x1-x0)*(t1-t0) + t0;
T1c = (X1c-x0)/(x1-x0)*(t1-t0) + t0;
v0 = prm.v0;
v1 = prm.v1;
v0c = (y0c-y0)/(y1-y0)*(v1-v0) + v0;
v1c = (y1c-y0)/(y1-y0)*(v1-v0) + v0;
V0 = (Y0-y0)/(y1-y0)*(v1-v0) + v0;
V1 = (Y1-y0)/(y1-y0)*(v1-v0) + v0;
V0c = (Y0c-y0)/(y1-y0)*(v1-v0) + v0;
V1c = (Y1c-y0)/(y1-y0)*(v1-v0) + v0;
vtx = [];
idx = [];
% MFD - front surface
mvtx = [x0c,y0,z0,  0,0,-1, t0c,v0; ...
    x1c,y0,z0,  0,0,-1, t1c,v0; ...
    x0,y0c,z0,  0,0,-1, t0,v0c; ...
    x1,y0c,z0,  0,0,-1, t1,v0c; ...
    x0,y1c,z0,  0,0,-1, t0,v1c; ...
    x1,y1c,z0,  0,0,-1, t1,v1c; ...
    x0c,y1,z0,  0,0,-1, t0c,v1; ...
    x1c,y1,z0,  0,0,-1, t1c,v1; ...
    X0c,Y0,z0,  0,0,-1, T0c,V0; ...
    X1c,Y0,z0,  0,0,-1, T1c,V0; ...
    X0,Y0c,z0,  0,0,-1, T0,V0c; ...
    X1,Y0c,z0,  0,0,-1, T1,V0c; ...
    X0,Y1c,z0,  0,0,-1, T0,V1c; ...
    X1,Y1c,z0,  0,0,-1, T1,V1c; ...
    X0c,Y1,z0,  0,0,-1, T0c,V1; ...
    X1c,Y1,z0,  0,0,-1, T1c,V1];
midx = [0,8,1; 8,9,1; 1,9,3; 3,9,11; 3,11,5; 5,11,13; 5,13,7; 7,13,15; 7,15,6; 6,15,14; 6,14,4; 4,14,12; 4,12,2; 2,12,10; 2,10,0; 0,10,8];
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);
% MFD - inner edge
ds = prm.infrm_w;
phi = atan(ds/prm.inner_depth);
sp = sin(phi);
cp = cos(phi);
mvtx = [X0c,Y0,z0,  0,cp,-sp, 0,0; ...
    X1c,Y0,z0,  0,cp,-sp, 0,0; ...
    X0,Y0c,z0,  cp,0,-sp, 0,0; ...
    X1,Y0c,z0,  -cp,0,-sp, 0,0; ...
    X0,Y1c,z0,  cp,0,-sp, 0,0; ...
    X1,Y1c,z0,  -cp,0,-sp, 0,0; ...
    X0c,Y1,z0,  0,-cp,-sp, 0,0; ...
    X1c,Y1,z0,  0,-cp,-sp, 0,0; ...
    X0c+ds,Y0+ds,Z1,  0,cp,-sp, 0,0; ...
    X1c-ds,Y0+ds,Z1,  0,cp,-sp, 0,0; ...
    X0+ds,Y0c+ds,Z1,  cp,0,-sp, 0,0; ...
    X1-ds,Y0c+ds,Z1,  -cp,0,-sp, 0,0; ...
    X0+ds,Y1c-ds,Z1,  cp,0,-sp, 0,0; ...
    X1-ds,Y1c-ds,Z1,  -cp,0,-sp, 0,0; ...
    X0c+ds,Y1-ds,Z1,  0,-cp,-sp, 0,0; ...
    X1c-ds,Y1-ds,Z1,  0,-cp,-sp, 0,0];
midx = [0,8,1; 8,9,1; 1,9,3; 3,9,11; 3,11,5; 5,11,13; 5,13,7; 7,13,15; 7,15,6; 6,15,14; 6,14,4; 4,14,12; 4,12,2; 2,12,10; 2,10,0; 0,10,8];
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);
% MFD - outer edge (incomplete because of limited visibility)
if pos <= 0
    mvtx = [x1c,y1,z0,  0,1,0, 0,0; ...
        x1c,y1,z1,  0,1,0, 0,0; ...
        x1,y1c,z0,  1,0,0, 0,0; ...
        x1,y1c,z1,  1,0,0, 0,0; ...
        x1,y0c,z0,  1,0,0, 0,0; ...
        x1,y0c,z1,  1,0,0, 0,0];
    midx = [0,1,2; 1,3,2; 2,3,4; 3,5,4];
    [vtx,idx] = add_to_group(vtx,idx,mvtx,midx);
end
if pos >= 0
    mvtx = [x0c,y1,z1,  0,1,0, 0,0; ...
        x0c,y1,z0,  0,1,0, 0,0; ...
        x0,y1c,z1,  -1,0,0, 0,0; ...
        x0,y1c,z0,  -1,0,0, 0,0; ...
        x0,y0c,z1,  -1,0,0, 0,0; ...
        x0,y0c,z0,  -1,0,0, 0,0];
    midx = [0,1,2; 1,3,2; 2,3,4; 3,5,4];
    [vtx,idx] = add_to_group(vtx,idx,mvtx,midx);
end
if pos == 0
    mvtx = [];
    midx = [-5,-11,-12; -5,-6,-11];
    [vtx,idx] = add_to_group(vtx,idx,mvtx,midx);
end
end

% =========================================================================
% Generate vertices and faces for left or right MFD display surface
function [vtx,idx] = make_mfd_disp(zref,yref,prm,isleft)
z = zref-prm.outer_depth + prm.inner_depth;
if isleft
    x1 = -prm.cnt_dst - prm.lr_frmw - prm.infrm_w;
    x0 = x1 - prm.disp;
else
    x0 = prm.cnt_dst + prm.lr_frmw + prm.infrm_w;
    x1 = x0 + prm.disp;
end
y1 = yref - prm.t_frmh - prm.infrm_w;
y0 = y1 - prm.disp;
vtx = [x0,y0,z, 0,0,-1,  0,1; ...
    x1,y0,z, 0,0,-1,  1,1; ...
    x0,y1,z, 0,0,-1,  0,0; ...
    x1,y1,z, 0,0,-1,  1,0];
idx = [0,2,1; 1,2,3];
end

% =========================================================================
% Generate vertices and faces for an MFD button
function [vtx,idx] = make_mfd_button(zref,yref,prm,isleftmfd,side,no)
btnw = prm.btnw;
btnh = prm.btnh;
if isleftmfd
    xofs = -prm.cnt_dst - prm.w + (prm.lr_frmw-btnw)/2;
else
    xofs = prm.cnt_dst + (prm.lr_frmw-btnw)/2;
end
switch side
    case 0 % left button column
        x0 = xofs;
    case 1 % right button column
        x0 = xofs + prm.w - prm.lr_frmw;
    case 2 % bottom button row
        x0 = xofs - (prm.lr_frmw-btnw)/2 + prm.w/2 - btnw - prm.btngap/2 + no*(btnw+prm.btngap);
end
x1 = x0 + btnw;
if side < 2
    ycnt = yref - prm.top_dst - prm.t_frmh - prm.infrm_w - prm.disp/2;
    ytop = ycnt + 2.5*prm.btngap + 3*btnh;
    y1 = ytop - no*(btnh+prm.btngap);
    y0 = y1 - btnh;
else
    y0 = yref - prm.top_dst - prm.h + (prm.b_frmh-btnh)/2;
    y1 = y0 + btnh;
end
z1  = zref - prm.outer_depth;
z0 = z1 - prm.btn_depth;
if side < 2
    t0 = texcrd(128+no*40);
    v1 = texcrd(side*41);
    if ~isleftmfd
        v1 = v1+texcrd(82);
    end
else
    t0 = texcrd(128+no*40);
    v1 = texcrd(41*4);
end
t1 = t0+texcrd(40);
v0 = v1 + texcrd(41);
vtx = [x0,y0,z0, 0,0,-1 ,t0,v0; ...
    x1,y0,z0, 0,0,-1, t1,v0; ...
    x0,y1,z0, 0,0,-1, t0,v1; ...
    x1,y1,z0, 0,0,-1, t1,v1; ...
    x0,y0,z0, -1,0,0, t0,v0; ...
    x0,y0,z1, -1,0,0, t0,v0; ...
    x0,y1,z0, -1,0,0, t0,v1; ...
    x0,y1,z1, -1,0,0, t0,v1; ...
    x1,y1,z0, 1,0,0,  t1,v1; ...
    x1,y1,z1, 1,0,0,  t1,v1; ...
    x1,y0,z0, 1,0,0,  t1,v0; ...
    x1,y0,z1, 1,0,0,  t1,v0];
idx = [0,3,1; 0,2,3; 4,5,6; 5,7,6; 6,7,8; 7,9,8; 8,9,10; 9,11,10];
end

% =========================================================================
% Generate vertices and faces for a dash button

function [vtx,idx] = make_dash_button(zref,yref,x0,y0,z0,u,v,prm)
btnw = prm.w;
btnh = prm.h;
x1 = x0+btnw;
y0 = y0+yref;
y1 = y0+btnh;
z1 = z0+zref;
z0 = z1-prm.depth;
if length(u)==1, u(2) = u(1)+texcrd(40); end
if length(v)==1, v(2) = v(1)-texcrd(41); end
u0 = u(1);  u1 = u(2);
v0 = v(1);  v1 = v(2);
vtx = [x0,y0,z0, 0,0,-1 ,u0,v0; ...
    x1,y0,z0, 0,0,-1, u1,v0; ...
    x0,y1,z0, 0,0,-1, u0,v1; ...
    x1,y1,z0, 0,0,-1, u1,v1; ...

    x0,y1,z0, 0,1,0,  u0,v1; ...
    x1,y1,z0, 0,1,0,  u1,v1; ...
    x0,y1,z1, 0,1,0,  u0,v1; ...
    x1,y1,z1, 0,1,0,  u1,v1; ...

    x0,y0,z0, -1,0,0, u0,v0; ...
    x0,y1,z0, -1,0,0, u0,v1; ...
    x0,y0,z1, -1,0,0, u0,v0; ...
    x0,y1,z1, -1,0,0, u0,v1; ...

    x1,y0,z0, 1,0,0, u0,v0; ...
    x1,y1,z0, 1,0,0, u0,v1; ...
    x1,y0,z1, 1,0,0, u0,v0; ...
    x1,y1,z1, 1,0,0, u0,v1];
idx = [0,3,1; 0,2,3; 4,7,5; 4,6,7; 8,11,9; 8,10,11; 12,13,15; 12,15,14];
end

% =========================================================================
% Generate vertices and faces for HUD projector lens
function [vtx,idx] = make_hud_lens(prm)
x0 = prm.proj.tuberad;
scale = x0*0.05;
x0 = x0 - 2*scale;
z1 = -prm.proj.tubelen*0.1;
for i=0:prm.proj.nseg-1
    phi = i/prm.proj.nseg*2*pi;
    vtx(i+1,:) = [x0*cos(phi),x0*sin(phi),z1, 0,0,1, 0,0];
end
for i=0:prm.proj.nseg-3
    idx(i+1,:) = [0, i+1, i+2];
end

vtx(:,3) = vtx(:,3) - prm.proj.dist;
vtx = tilt_x(vtx,0,0,prm.pane.tilt*2);

vtx(:,2) = vtx(:,2) + prm.pane.yref;
vtx(:,3) = vtx(:,3) + prm.pane.zref;
end

% =========================================================================
% Side wall for left side panels
function [vtx,idx] = make_lside_wall(slope_panel,side_panel)
x0 = side_panel.x0;
u = texcrd(1);
v = texcrd(142);
h = 0.06;
vtx = [x0,side_panel.yref,side_panel.zref, 1,0,0,  u,v; ...
    x0,slope_panel.ref(2),slope_panel.ref(3), 1,0,0, u,v; ...
    x0,side_panel.yref-h,slope_panel.ref(3)-0.04, 1,0,0, u,v; ...
    x0,side_panel.yref-h,side_panel.zref-side_panel.h, 1,0,0, u,v; ...
    x0,side_panel.yref,side_panel.zref-side_panel.h, 1,0,0, u,v; ...
    x0,side_panel.yref-h,side_panel.zref-side_panel.h, 0,0,-1, u,v; ...
    x0,side_panel.yref,side_panel.zref-side_panel.h, 0,0,-1, u,v; ...
    -0.421426,0.723895,6.922301, 0,0,-1, u,v; ...   % connection with old VC mesh
    x0-side_panel.w,side_panel.yref,side_panel.zref-side_panel.h, 0,0,-1, u,v];
idx = [0,1,2; 0,2,3; 0,3,4; 5,7,6; 6,7,8];
end

% =========================================================================
% Side wall for left side panels
function [vtx,idx] = make_rside_wall(slope_panel,side_panel)
x0 = side_panel.x0;
u = texcrd(1);
v = texcrd(142);
h = 0.06;
vtx = [x0,side_panel.yref,side_panel.zref, -1,0,0,  u,v; ...
    x0,slope_panel.yref,slope_panel.zref, -1,0,0, u,v; ...
    x0,side_panel.yref-h,slope_panel.zref-0.04, -1,0,0, u,v; ...
    x0,side_panel.yref-h,side_panel.zref-side_panel.h, -1,0,0, u,v; ...
    x0,side_panel.yref,side_panel.zref-side_panel.h, -1,0,0, u,v; ...
    x0,side_panel.yref-h,side_panel.zref-side_panel.h, 0,0,-1, u,v; ...
    x0,side_panel.yref,side_panel.zref-side_panel.h, 0,0,-1, u,v; ...
    0.421426,0.723895,6.922301, 0,0,-1, u,v; ...   % connection with old VC mesh
    x0-side_panel.w,side_panel.yref,side_panel.zref-side_panel.h, 0,0,-1, u,v];
idx = [0,2,1; 0,3,2; 0,4,3; 5,6,7; 6,8,7];
end

% =========================================================================
% Generate vertices and faces for lower left front panel
function [meshlist,prms] = make_lslopepanel(this)
abrake = this.abrake;
etrim  = this.etrim;
gear   = this.gear;
edge   = this.edge;
x0 = 0;
xb = -this.w_bottom;
xt = -this.w_top;
y1 = 0;
y0 = y1 - this.h;
z0 = 0;
z1 = z0 + 2*edge;
x0ab = x0 + abrake.ref(1) - abrake.slit.w/2;
x1ab = x0ab + abrake.slit.w;
y0ab = y1 + abrake.ref(2) - abrake.slit.h/2;
y1ab = y0ab + abrake.slit.h;
x0et = x0 + etrim.ref(1) - etrim.slit.w/2;
x1et = x0et + etrim.slit.w;
X1et = x1et + etrim.scale.w;
y0et = y1 + etrim.ref(2) - etrim.slit.h/2;
y1et = y0et + etrim.slit.h;
Y0et = y1 + etrim.ref(2) - etrim.scale.h/2;
Y1et = Y0et + etrim.scale.h;
x0gl = gear.sub.x0;
x1gl = gear.sub.x1;
y0gl = gear.sub.y0;
y1gl = gear.sub.y1;
grim = gear.sub.rim;
irim = grim/2;
orim = edge;
z2 = z0+grim;
grim = grim+irim;
vtx = [xb,y0,z0,        0,0,-1, 0,0; ...
    x0,y0,z0,           0,0,-1, 0,0; ...
    xt,y1,z0,           0,0,-1, 0,0; ...
    x0,y1,z0,           0,0,-1, 0,0; ...

    x0+orim,y0,z0+orim,  1,0,0, 0,0; ... % outer rim
    x0+orim,y1,z0+orim,  1,0,0, 0,0; ...
    x0,y1+orim,z0+orim,  0,1,0, 0,0; ...
    xt,y1+orim,z0+orim,  0,1,0, 0,0; ...
    x0,y0-orim,z0+orim, 0,-1,0, 0,0; ...
    xb,y0-orim,z0+orim, 0,-1,0, 0,0; ...
    xb-orim,y0,z0+orim, -1,0,0, 0,0; ...
    xt-orim,y1,z0+orim, -1,0,0, 0,0; ...
    x0+orim,y0,z1,       1,0,0, 0,0; ...
    x0+orim,y1,z1,       1,0,0, 0,0; ...
    x0,y1+orim,z1,       0,1,0, 0,0; ...
    xt,y1+orim,z1,       0,1,0, 0,0; ...

    x0ab,y0ab,z0,       0,0,-1, 0,0; ...  % slit for airbrake lever
    x1ab,y0ab,z0,       0,0,-1, 0,0; ...
    x0ab,y1ab,z0,       0,0,-1, 0,0; ...
    x1ab,y1ab,z0,       0,0,-1, 0,0; ...

    x0et,y0et,z0,       0,0,-1, 0,0; ...  % slit for elevator trim lever and scale
    x1et,y0et,z0,       0,0,-1, 0,0; ...
    x0et,y1et,z0,       0,0,-1, 0,0; ...
    x1et,y1et,z0,       0,0,-1, 0,0; ...
    x1et,Y0et,z0,       0,0,-1, 0,0; ...
    X1et,Y0et,z0,       0,0,-1, 0,0; ...
    x1et,Y1et,z0,       0,0,-1, 0,0; ...
    X1et,Y1et,z0,       0,0,-1, 0,0; ...

    % depression for gear lever
    x0gl,y0gl-grim,z0,  0,0,-1, 0,0; ...
    x1gl,y0gl-grim,z0,  0,0,-1, 0,0; ...
    x0gl-grim,y0gl,z0,  0,0,-1, 0,0; ...
    x1gl+grim,y0gl,z0,  0,0,-1, 0,0; ...
    x0gl-grim,y1gl,z0,  0,0,-1, 0,0; ...
    x1gl+grim,y1gl,z0,  0,0,-1, 0,0; ...
    x0gl,y1gl+grim,z0,  0,0,-1, 0,0; ...
    x1gl,y1gl+grim,z0,  0,0,-1, 0,0; ...

    x0gl,y0gl-irim,z2,  0,1,0, 0,0; ...
    x1gl,y0gl-irim,z2,  0,1,0, 0,0; ...
    x0gl-irim,y0gl,z2,  1,0,0, 0,0; ...
    x1gl+irim,y0gl,z2, -1,0,0, 0,0; ...
    x0gl-irim,y1gl,z2,  1,0,0, 0,0; ...
    x1gl+irim,y1gl,z2, -1,0,0, 0,0; ...
    x0gl,y1gl+irim,z2, 0,-1,0, 0,0; ...
    x1gl,y1gl+irim,z2, 0,-1,0, 0,0];

idx = [23,27,26; 21,24,25; 18,3,19; 19,3,17; 17,3,1; 27,17,1; 23,17,27; 23,16,17; 22,16,23; 25,27,1; 21,25,1; 20,21,1; 0,20,1; 0,29,20; 0,28,29; 0,30,28; 29,31,20; 20,31,22; 31,33,22; 22,33,16; 16,33,18; 33,35,18; 2,3,18; 35,2,18; 34,2,35; 32,2,34; 0,2,32; 0,32,30; ...
    1,3,4; 4,3,5; 3,2,6; 6,2,7; 3,6,5; 1,9,0; 1,8,9; 1,4,8; 10,11,0; 0,11,2; 0,9,10; 2,11,7; ...
    4,5,12; 12,5,13; 14,6,7; 14,7,15; 13,5,6; 13,6,14; ...
    28,36,29; 29,36,37; 31,29,37; 31,37,39; 33,31,39; 33,39,41; 35,33,41; 35,41,43; 35,43,42; 35,42,34; 34,42,40; 34,40,32; 40,38,30; 40,30,32; 38,36,28; 38,28,30];

% gear lever slit
slit = gear.slit;
sw2 = slit.w2;
sh2 = slit.h2;
phi = atan(sh2/gear.lever.depth); % gear lever tilt ange
oval_scale = 1/cos(phi);
sx = x0 + gear.ref(1);
sy = y1 + gear.ref(2);
sh0 = sh2 + sw2*oval_scale;
sw1 = sw2/sqrt(2);
sh1 = sh2 + sw1*oval_scale;
mvtx = [x0gl-irim,y0gl-irim,z2, 0,0,-1, 0,0; ...
    x1gl+irim,y0gl-irim,z2, 0,0,-1, 0,0; ...
    x0gl-irim,y1gl+irim,z2, 0,0,-1, 0,0; ...
    x1gl+irim,y1gl+irim,z2, 0,0,-1, 0,0; ...
    % gear lever slit
    sx-sw2,sy-sh2,z2, 0,0,-1,  0,0; ...
    sx+sw2,sy-sh2,z2, 0,0,-1,  0,0; ...
    sx-sw2,sy+sh2,z2, 0,0,-1,  0,0; ...
    sx+sw2,sy+sh2,z2, 0,0,-1,  0,0; ...
    sx,    sy-sh0,z2, 0,0,-1,  0,0; ...
    sx,    sy+sh0,z2, 0,0,-1,  0,0; ...
    sx-sw1,sy-sh1,z2, 0,0,-1,  0,0; ...
    sx+sw1,sy-sh1,z2, 0,0,-1,  0,0; ...
    sx-sw1,sy+sh1,z2, 0,0,-1,  0,0; ...
    sx+sw1,sy+sh1,z2, 0,0,-1,  0,0];
midx = [0,2,4; 2,6,4; 1,5,3; 3,5,7; 0,8,1; 3,9,2; ...
    0,4,10; 0,10,8; 1,8,11; 1,11,5; 2,9,12; 2,12,6; 3,7,13; 3,13,9];
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);

for i=1:size(vtx,1)
    vtx(i,7) = this.u0 + (this.ub-this.u0)/(xb-x0)*(vtx(i,1)-x0);
    vtx(i,8) = this.v0 + (this.v1-this.v0)/(y1-y0)*(vtx(i,2)-y0);
end

meshlist = struct('vtx',vtx, 'idx',idx, 'grpname','PANELS1', 'name','');
prms = {};

% make children
[sublist,subprms] = make_gear_lever(this.gear);
meshlist = meshmerge(meshlist,sublist);
prms = [prms; subprms];

[sublist,subprms] = make_airbrake_lever(this.abrake);
meshlist = meshmerge(meshlist,sublist);
prms = [prms; subprms];

[sublist,subprms] = make_elevtrim_wheel(this.etrim);
meshlist = meshmerge(meshlist,sublist);
prms = [prms; subprms];

% map into parent frame
meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);
end

% =========================================================================
% Generate vertices and faces for lower right front panel
function [meshlist,prms] = make_rslopepanel(this)
undock = this.undock;
ncone  = this.ncone;
edge   = this.edge;
x0 = 0;
xb = x0 + this.w_bottom;
xt = x0 + this.w_top;
y1 = 0;
y0 = y1 - this.h;
z0 = 0;
z1 = z0 + 2*edge;
x0ab = x0 + undock.ref(1) + undock.slit.w/2;
x1ab = x0ab - undock.slit.w;
y0ab = y1 + undock.ref(2) - undock.slit.h/2;
y1ab = y0ab + undock.slit.h;
x0gl = x0 + ncone.sub.x0;
x1gl = x0 + ncone.sub.x1;
y0gl = y1 + ncone.sub.y0;
y1gl = y1 + ncone.sub.y1;
grim = ncone.sub.rim;
irim = grim/2;
orim = edge;
z2 = z0+grim;
grim = grim+irim;
vtx = [xb,y0,z0,           0,0,-1, 0,0; ... % main panel surface
    x0,y0,z0,           0,0,-1, 0,0; ...
    xt,y1,z0,           0,0,-1, 0,0; ...
    x0,y1,z0,           0,0,-1, 0,0; ...

    x0-orim,y0,z0+orim, -1,0,0, 0,0; ... % outer rim
    x0-orim,y1,z0+orim, -1,0,0, 0,0; ...
    x0,y1+orim,z0+orim,  0,1,0, 0,0; ...
    xt,y1+orim,z0+orim,  0,1,0, 0,0; ...
    x0,y0-orim,z0+orim, 0,-1,0, 0,0; ...
    xb,y0-orim,z0+orim, 0,-1,0, 0,0; ...
    xb+orim,y0,z0+orim,  1,0,0, 0,0; ...
    xt+orim,y1,z0+orim,  1,0,0, 0,0; ...
    x0-orim,y0,z1,      -1,0,0, 0,0; ...
    x0-orim,y1,z1,      -1,0,0, 0,0; ...
    x0,y1+orim,z1,       0,1,0, 0,0; ...
    xt,y1+orim,z1,       0,1,0, 0,0; ...

    x0ab,y0ab,z0,       0,0,-1, 0,0; ...  % slit for undock lever
    x1ab,y0ab,z0,       0,0,-1, 0,0; ...
    x0ab,y1ab,z0,       0,0,-1, 0,0; ...
    x1ab,y1ab,z0,       0,0,-1, 0,0; ...

    % depression for ncone lever
    x0gl,y0gl-grim,z0,  0,0,-1, 0,0; ...
    x1gl,y0gl-grim,z0,  0,0,-1, 0,0; ...
    x0gl+grim,y0gl,z0,  0,0,-1, 0,0; ...
    x1gl-grim,y0gl,z0,  0,0,-1, 0,0; ...
    x0gl+grim,y1gl,z0,  0,0,-1, 0,0; ...
    x1gl-grim,y1gl,z0,  0,0,-1, 0,0; ...
    x0gl,y1gl+grim,z0,  0,0,-1, 0,0; ...
    x1gl,y1gl+grim,z0,  0,0,-1, 0,0; ...

    x0gl,y0gl-irim,z2,  0,1,0, 0,0; ...
    x1gl,y0gl-irim,z2,  0,1,0, 0,0; ...
    x0gl+irim,y0gl,z2, -1,0,0, 0,0; ...
    x1gl-irim,y0gl,z2,  1,0,0, 0,0; ...
    x0gl+irim,y1gl,z2, -1,0,0, 0,0; ...
    x1gl-irim,y1gl,z2,  1,0,0, 0,0; ...
    x0gl,y1gl+irim,z2, 0,-1,0, 0,0; ...
    x1gl,y1gl+irim,z2, 0,-1,0, 0,0];

idx = [2,24,26; 2,26,27; 2,22,24; 2,0,22; 22,0,20; 20,0,21; 21,16,23; 23,16,18; 23,18,25; 2,27,3; 3,27,25; 3,25,18; 3,18,19; 3,19,1; 1,19,17; 1,17,16; 1,16,0; 0,16,21; ...
    1,4,3; 4,5,3; 3,6,2; 6,7,2; 3,5,6; 1,0,9; 1,9,8; 1,8,4; 10,0,11; 0,2,11; 0,10,9; 2,7,11; ...
    4,12,5; 12,13,5; 14,7,6; 14,15,7; 13,6,5; 13,14,6; ...
    20,21,28; 21,29,28; 23,29,21; 23,31,29; 25,31,23; 25,33,31; 27,33,25; 27,35,33; 27,34,35; 27,26,34; 26,32,34; 26,24,32; 32,22,30; 32,24,22; 30,20,28; 30,22,20];

% ncone lever slit
slit = ncone.slit;
sw2 = slit.w2;
sh2 = slit.h2;
phi = atan(sh2/ncone.lever.depth); % ncone lever tilt ange
oval_scale = 1/cos(phi);
sx = x0 + ncone.ref(1);
sy = y1 + ncone.ref(2);
sh0 = sh2 + sw2*oval_scale;
sw1 = sw2/sqrt(2);
sh1 = sh2 + sw1*oval_scale;
mvtx = [x0gl+irim,y0gl-irim,z2, 0,0,-1, 0,0; ...
    x1gl-irim,y0gl-irim,z2, 0,0,-1, 0,0; ...
    x0gl+irim,y1gl+irim,z2, 0,0,-1, 0,0; ...
    x1gl-irim,y1gl+irim,z2, 0,0,-1, 0,0; ...
    % ncone lever slit
    sx+sw2,sy-sh2,z2, 0,0,-1,  0,0; ...
    sx-sw2,sy-sh2,z2, 0,0,-1,  0,0; ...
    sx+sw2,sy+sh2,z2, 0,0,-1,  0,0; ...
    sx-sw2,sy+sh2,z2, 0,0,-1,  0,0; ...
    sx,    sy-sh0,z2, 0,0,-1,  0,0; ...
    sx,    sy+sh0,z2, 0,0,-1,  0,0; ...
    sx+sw1,sy-sh1,z2, 0,0,-1,  0,0; ...
    sx-sw1,sy-sh1,z2, 0,0,-1,  0,0; ...
    sx+sw1,sy+sh1,z2, 0,0,-1,  0,0; ...
    sx-sw1,sy+sh1,z2, 0,0,-1,  0,0];
midx = [0,4,2; 2,4,6; 1,3,5; 3,7,5; 0,1,8; 3,2,9; ...
    0,10,4; 0,8,10; 1,11,8; 1,5,11; 2,12,9; 2,6,12; 3,13,7; 3,9,13];
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);

for i=1:size(vtx,1)
    vtx(i,7) = this.u0 + (this.ub-this.u0)/(xb-x0)*(vtx(i,1)-x0);
    vtx(i,8) = this.v0 + (this.v1-this.v0)/(y1-y0)*(vtx(i,2)-y0);
end

meshlist = struct('vtx',vtx, 'idx',idx, 'grpname','PANELS1', 'name','');
prms = {};

% make children
[sublist,subprms] = make_ncone_lever(this.ncone);
meshlist = meshmerge(meshlist,sublist);
prms = [prms; subprms];

[sublist,subprms] = make_undock_lever(this.undock);
meshlist = meshmerge(meshlist,sublist);
prms = [prms; subprms];

[sublist,subprms] = make_rcover_switch(this.rcover);
meshlist = meshmerge(meshlist,sublist);
prms = [prms; subprms];

[sublist,subprms] = make_eladder_switch(this.ladder);
meshlist = meshmerge(meshlist,sublist);
prms = [prms; subprms];

[sublist,subprms] = make_dockseal_indicator(this.seal);
meshlist = meshmerge(meshlist,sublist);
prms = [prms; subprms];

% map into parent frame
meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);
end

% =========================================================================
% Generate vertices and faces for right side panel
function [vtx,idx] = make_rside_panel(prm)
x0 = prm.x0;
x1 = x0 + prm.w;
y1 = prm.yref;
y0 = y1 - prm.h;
z0 = prm.zref;
X0 = x0 + prm.status.xref;
X1 = X0 + prm.status.w;
Y1 = y1 + prm.status.yref;
Y0 = Y1 - prm.status.h;
vtx = [x0,y1,z0, 0,0,-1, 0,0; ...
    x1,y1,z0, 0,0,-1, 0,0; ...
    x0,y0,z0, 0,0,-1, 0,0; ...
    x1,y0,z0, 0,0,-1, 0,0; ...

    X0,Y1,z0, 0,0,-1, 0,0; ...
    X1,Y1,z0, 0,0,-1, 0,0; ...
    X0,Y0,z0, 0,0,-1, 0,0; ...
    X1,Y0,z0, 0,0,-1, 0,0];
idx = [0,1,4; 1,5,4; 1,7,5; 1,3,7; 7,3,2; 6,7,2; 0,4,6; 0,6,2];

% status indicator well
v = texcrd(142);
u0 = texcrd(18);
u1 = texcrd(12);
u2 = texcrd(24);
z1 = z0+prm.status.zref+0.002;
mvtx = [X0,Y1,z0, 0,-1,0, u0,v; ...
    X1,Y1,z0, 0,-1,0, u0,v; ...
    X0,Y1,z1, 0,-1,0, u0,v; ...
    X1,Y1,z1, 0,-1,0, u0,v; ...

    X1,Y1,z0, -1,0,0, u1,v; ...
    X1,Y0,z0, -1,0,0, u1,v; ...
    X1,Y1,z1, -1,0,0, u1,v; ...
    X1,Y0,z1, -1,0,0, u1,v; ...

    X0,Y1,z1, 0,0,-1, u2,v; ...
    X1,Y1,z1, 0,0,-1, u2,v; ...
    X0,Y0,z1, 0,0,-1, u2,v; ...
    X1,Y0,z1, 0,0,-1, u2,v];
midx = [0,1,2; 1,3,2; 4,5,6; 5,7,6; 8,9,10; 9,11,10];
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);
end

% =========================================================================
% Generate vertices and faces for status indicator panel on right side
% panel
function [vtx,idx] = make_statusind(prm)
ind = prm.status;
x0 = prm.x0 + ind.xref;
y1 = prm.yref + ind.yref;
z0 = prm.zref + ind.zref;
u = 0.998;
v0 = 0.0;
dv = 0.0625;
nrow = 2;
ncol = 4;
indh = (ind.h - (nrow+1)*ind.gap)/nrow;
indw = (ind.w - (ncol+1)*ind.gap)/ncol;
vtx = [];
idx = [];
ofs = 0;
for i=1:nrow
    yt = y1 - ind.gap - (i-1)*(indh+ind.gap);
    yb = yt - indh;
    for j=1:ncol
        xl = x0 + ind.gap + (j-1)*(indw+ind.gap);
        xr = xl + indw;
        v = max(v0,0.001);
        vtx = [vtx; ...
            xl,yt,z0, 0,0,-1, u,v; ...
            xl,yb,z0, 0,0,-1, u,v0+dv; ...
            xr,yb,z0, 0,0,-1, u,v0+dv; ...
            xr,yt,z0, 0,0,-1, u,v];
        idx = [idx; ...
            ofs, ofs+2, ofs+1; ...
            ofs, ofs+3, ofs+2];
        ofs = ofs+4;
        v0 = v0+dv;
    end
end
end

% =========================================================================
% Generate vertices and faces for status indicator glass cover on right side
% panel
function [vtx,idx] = make_statusind_glass(prm)
ind = prm.status;
x0 = prm.x0 + ind.xref;
y1 = prm.yref + ind.yref;
z0 = prm.zref;
x1 = x0 + ind.w;
y0 = y1 - ind.h;
vtx = [x0,y1,z0, 0,0,-1, 0,0; ...
    x1,y1,z0, 0,0,-1, 0,0; ...
    x0,y0,z0, 0,0,-1, 0,0; ...
    x1,y0,z0, 0,0,-1, 0,0];
idx = [0,1,2; 1,3,2];
end

% =========================================================================
% Generate vertices and faces for scram temperature display
function [vtx,idx] = make_scramtemp(prm)
disp = prm.scrmtemp;
x0 = disp.x0;
x1 = x0 + disp.w;
z0 = prm.zref-disp.z0;
z1 = z0-disp.h;
y0 = prm.yref+disp.y0;
vtx = [x0,y0,z0, 0,-1,0, disp.u(1),disp.v(1); ...
    x1,y0,z0, 0,-1,0, disp.u(2),disp.v(1); ...
    x0,y0,z1, 0,-1,0, disp.u(1),disp.v(2); ...
    x1,y0,z1, 0,-1,0, disp.u(2),disp.v(2)];
idx = [0,2,1; 1,2,3];
end

% =========================================================================
% Generate vertices and faces for HUD overhead projector
function [vtx,idx] = make_hud_projector(prm)
x0 = prm.proj.tuberad;
z1 = -prm.proj.tubelen;
scale = x0*0.05;
profile.x = [x0,x0,x0-scale,x0-2*scale,x0-2*scale];
profile.z = [z1,-scale,0,0,z1*0.1];
profile.nx = [1,1,0,0,-1];
profile.nz = [0,0,1,1,0];
nprof = length(profile.x);
k = 1;
for i=0:prm.proj.nseg-1
    phi = i/prm.proj.nseg*2*pi;
    nz = profile.nz;
    nx = profile.nx*cos(phi);
    ny = profile.nx*sin(phi);
    z = profile.z;
    x = profile.x*cos(phi);
    y = profile.x*sin(phi);
    for j=1:nprof
        vtx(k,:) = [x(j),y(j),z(j), nx(j),ny(j),nz(j), 0,0];
        k = k+1;
    end
end
nvtx = size(vtx,1);
k = 1;
for i=0:prm.proj.nseg-1
    ofs0 = i*nprof;
    for j=0:nprof-2
        ofs1 = ofs0+j;
        idx(k,:) = [ofs1, mod(ofs1+nprof,nvtx), ofs1+1];
        k = k+1;
        idx(k,:) = [mod(ofs1+nprof,nvtx), mod(ofs1+nprof+1,nvtx), ofs1+1];
        k = k+1;
    end
end

vtx(:,3) = vtx(:,3) - prm.proj.dist;
vtx = tilt_x(vtx,0,0,prm.pane.tilt*2);

vtx(:,2) = vtx(:,2) + prm.pane.yref;
vtx(:,3) = vtx(:,3) + prm.pane.zref;
end

% =========================================================================
% Generate vertices and faces for HUD retraction rail (down to hinge)
function [vtx,idx] = make_hud_rail(prm)
x0 = -prm.joint.width/2;
x1 = -x0;
X0 = x0 + prm.frame.depth;
X1 = -X0;
z0 = prm.rail.rad;
z1 = z0 + prm.rail.depth;
Z0 = z0 + prm.frame.depth;
profile.x = [x0,x0,X0,X1,x1,x1];
profile.z = [z1,Z0,z0,z0,Z0,z1];
profile.nx = [-1,-1,0,0,1,1];
profile.nz = [0,0,-1,-1,0,0];
nprof = length(profile.x);
k = 1;
for i=0:prm.rail.nseg
    phi = i*prm.rail.dphi;
    nx = profile.nx;
    ny = profile.nz*sin(phi);
    nz = profile.nz*cos(phi);
    x = profile.x;
    y = profile.z*sin(phi);
    z = profile.z*cos(phi);
    for j=1:nprof
        vtx(k,:) = [x(j),y(j),z(j), nx(j),ny(j),nz(j), 0,0];
        k = k+1;
    end
end
k = 1;
for i=0:prm.rail.nseg-1
    ofs0 = i*nprof;
    for j=0:nprof-2
        ofs1 = ofs0+j;
        idx(k,:) = [ofs1, ofs1+nprof, ofs1+1];
        k = k+1;
        idx(k,:) = [ofs1+nprof, ofs1+nprof+1, ofs1+1];
        k = k+1;
    end
end

vtx = tilt_x(vtx,0,0,-prm.rail.phi0);
vtx(:,2) = vtx(:,2) + prm.rail.ycnt;
vtx(:,3) = vtx(:,3) + prm.rail.zcnt;

% hinge attachment
scale = prm.joint.rad*1.1;
z0 = -scale;
z1 = -z0;
y1 = z1*1.5;
y0 = z0;
Y0 = y0 + scale*0.5;
Z0 = z0 + scale*0.5;
Z1 = z1 - scale*0.5;
x0 = -prm.joint.width/2;
x1 = -x0;
X0 = x0 - prm.rail.hingew;
X1 = -X0;
X0i = x0 + scale;
X1i = x1 - scale;
Y1i = y1 + scale;
Y1  = Y1i + prm.rail.hingew;
s0 = sin(pi/6)*prm.rail.hingew;
c = 1/sqrt(2);
mvtx = [X0,y0,Z0, 0,-1,0, 0,0; ...
    x0,y0,Z0, 0,-1,0, 0,0; ...
    X0,Y0,z0, 0,0,-1, 0,0; ...
    x0,Y0,z0, 0,0,-1, 0,0; ...
    X0,y1+s0,z0, 0,0,-1, 0,0; ...
    x0,y1,z0,    0,0,-1, 0,0; ...
    X0i-s0,Y1,z0, 0,0,-1, 0,0; ...
    X0i,Y1i,z0,   0,0,-1, 0,0; ...
    X1i+s0,Y1,z0, 0,0,-1, 0,0; ...
    X1i,Y1i,z0,   0,0,-1, 0,0; ...
    X1,y1+s0,z0,  0,0,-1, 0,0; ...
    x1,y1,z0,     0,0,-1, 0,0; ...
    X1,Y0,z0,     0,0,-1, 0,0; ...
    x1,Y0,z0,     0,0,-1, 0,0; ...
    X1,y0,Z0,     0,-1,0, 0,0; ...
    x1,y0,Z0,     0,-1,0, 0,0; ...
    X0,y1+s0,z0,  -1,0,0, 0,0; ...
    X0,Y0,z0,     -1,0,0, 0,0; ...
    X0,y0,Z0,     -1,0,0, 0,0; ...
    X0,y0,Z1,     -1,0,0, 0,0; ...
    X0,Y0,z1,     -1,0,0, 0,0; ...
    X0,y1+s0,z1,  -1,0,0, 0,0; ...
    X0,y1+s0,z0,  -c,c,0, 0,0; ...
    X0,y1+s0,z1,  -c,c,0, 0,0; ...
    X0i-s0,Y1,z0, -c,c,0, 0,0; ...
    X0i-s0,Y1,z1, -c,c,0, 0,0; ...
    X0i-s0,Y1,z0, 0,1,0, 0,0; ...
    X0i-s0,Y1,z1, 0,1,0, 0,0; ...
    X1i+s0,Y1,z0, 0,1,0, 0,0; ...
    X1i+s0,Y1,z1, 0,1,0, 0,0; ...
    X1i+s0,Y1,z0, c,c,0, 0,0; ...
    X1i+s0,Y1,z1, c,c,0, 0,0; ...
    X1,y1+s0,z0,  c,c,0, 0,0; ...
    X1,y1+s0,z1,  c,c,0, 0,0; ...
    X1,y1+s0,z0,  1,0,0, 0,0; ...
    X1,Y0,z0,     1,0,0, 0,0; ...
    X1,y0,Z0,     1,0,0, 0,0; ...
    X1,y0,Z1,     1,0,0, 0,0; ...
    X1,Y0,z1,     1,0,0, 0,0; ...
    X1,y1+s0,z1,  1,0,0, 0,0; ...
    x0,y1,z0,     1,0,0, 0,0; ...
    x0,Y0,z0,     1,0,0, 0,0; ...
    x0,y0,Z0,     1,0,0, 0,0; ...
    x0,y0,Z1,     1,0,0, 0,0; ...
    x0,Y0,z1,     1,0,0, 0,0; ...
    x0,y1,z1,     1,0,0, 0,0; ...
    x1,y1,z0,     -1,0,0, 0,0; ...
    x1,Y0,z0,     -1,0,0, 0,0; ...
    x1,y0,Z0,     -1,0,0, 0,0; ...
    x1,y0,Z1,     -1,0,0, 0,0; ...
    x1,Y0,z1,     -1,0,0, 0,0; ...
    x1,y1,z1,     -1,0,0, 0,0; ...
    x0,y1,z0,     c,-c,0, 0,0; ...
    x0,y1,z1,     c,-c,0, 0,0; ...
    X0i,Y1i,z0,   c,-c,0, 0,0; ...
    X0i,Y1i,z1,   c,-c,0, 0,0; ...
    x1,y1,z0,     -c,-c,0, 0,0; ...
    x1,y1,z1,     -c,-c,0, 0,0; ...
    X1i,Y1i,z0,   -c,-c,0, 0,0; ...
    X1i,Y1i,z1,   -c,-c,0, 0,0];
midx = [0,2,1; 1,2,3; 2,4,3; 3,4,5; 4,6,5; 5,6,7; 6,8,7; 7,8,9; 8,10,9; 9,10,11; 10,12,11; 11,12,13; 12,14,13; 13,14,15; ...
    16,20,21; 16,17,20; 17,19,20; 17,18,19; 22,23,24; 23,25,24; 26,27,28; 27,29,28; 30,31,32; 31,33,32; ...
    34,39,38; 34,38,35; 35,38,37; 35,37,36; 40,45,44; 40,44,41; 41,44,43; 41,43,42; 46,50,51; 46,47,50; 47,49,50; 47,48,49; ...
    52,54,53; 53,54,55; 56,57,58; 57,59,58];
mvtx = tilt_x(mvtx,0,0,-prm.rail.phi0);
mvtx(:,2) = mvtx(:,2) + prm.joint.yref;
mvtx(:,3) = mvtx(:,3) + prm.joint.zref;
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);
end

% =========================================================================
% Generate vertices and faces for the HUD frame (up to hinge)
function [vtx,idx] = make_hud_frame(prm)
x0 = -prm.pane.w/2;
x1 = -x0;
X0 = x0 + prm.pane.corner;
X0f = x0 - prm.frame.width;
X1 = x1 - prm.pane.corner;
X1f = x1 + prm.frame.width;
X0r = x0 + prm.frame.depth;
X1r = x1 - prm.frame.depth;
X0u = X0f - prm.frame.depth;
X1u = X1f + prm.frame.depth;
y1 = prm.pane.yref + prm.pane.h/2;
y0 = y1 - prm.pane.corner;
Y0 = y0 - prm.frame.grip;
Y1f = y1 + prm.frame.width;
Y1r = y1 - prm.frame.depth;
Y1u = Y1f + prm.frame.depth;
z1 = prm.pane.zref;
z0 = z1 - prm.frame.depth;
z2 = z1 + prm.pane.depth;
z3 = z2 + prm.frame.depth;
s0 = sin(pi/6)*prm.frame.width;
s1 = sin(pi/6)*prm.frame.depth;
s2 = sin(pi/6)*(prm.frame.width+prm.frame.depth);
vtx = [x0,Y0,z0, 0,0,-1, 0,0; ...
    x0,y0,z0, 0,0,-1, 0,0; ...
    X0,y1,z0, 0,0,-1, 0,0; ...
    X1,y1,z0, 0,0,-1, 0,0; ...
    x1,y0,z0, 0,0,-1, 0,0; ...
    x1,Y0,z0, 0,0,-1, 0,0; ...
    X0f,Y0,z0, 0,0,-1, 0,0; ...
    X0f,y0+s0,z0, 0,0,-1, 0,0; ...
    X0-s0,Y1f,z0, 0,0,-1, 0,0; ...
    X1+s0,Y1f,z0, 0,0,-1, 0,0; ...
    X1f,y0+s0,z0, 0,0,-1, 0,0; ...
    X1f,Y0,z0, 0,0,-1, 0,0; ...
    X0r,Y0,z1, 1,0,0, 0,0; ...
    X0r,y0-s1,z1, 1,0,0, 0,0; ...
    X0+s1,Y1r,z1, 0,-1,0, 0,0; ...
    X1-s1,Y1r,z1, 0,-1,0, 0,0; ...
    X1r,y0-s1,z1, -1,0,0, 0,0; ...
    X1r,Y0,z1, -1,0,0, 0,0; ...
    X0u,Y0,z1, -1,0,0, 0,0; ...
    X0u,y0+s2,z1, -1,0,0, 0,0; ...
    X0-s2,Y1u,z1, 0,1,0, 0,0; ...
    X1+s2,Y1u,z1, 0,1,0, 0,0; ...
    X1u,y0+s2,z1, 1,0,0, 0,0; ...
    X1u,Y0,z1, 1,0,0, 0,0; ...
    X0u,Y0,z2, -1,0,0, 0,0; ...
    X0u,y0+s2,z2, -1,0,0, 0,0; ...
    X0-s2,Y1u,z2, 0,1,0, 0,0; ...
    X1+s2,Y1u,z2, 0,1,0, 0,0; ...
    X1u,y0+s2,z2, 1,0,0, 0,0; ...
    X1u,Y0,z2, 1,0,0, 0,0; ...
    x0,Y0,z0, 0,-1,0, 0,0; ...
    X0r,Y0,z1, 0,-1,0, 0,0; ...
    x0,Y0,z1, 0,-1,0, 0,0; ...
    x0,Y0,z2, 0,-1,0, 0,0; ...
    X0r,Y0,z2, 0,-1,0, 0,0; ...
    x0,Y0,z3, 0,-1,0, 0,0; ...
    X0f,Y0,z3, 0,-1,0, 0,0; ...
    X0u,Y0,z2, 0,-1,0, 0,0; ...
    X0u,Y0,z1, 0,-1,0, 0,0; ...
    X0f,Y0,z0, 0,-1,0, 0,0; ...
    x1,Y0,z0, 0,-1,0, 0,0; ...
    X1r,Y0,z1, 0,-1,0, 0,0; ...
    x1,Y0,z1, 0,-1,0, 0,0; ...
    x1,Y0,z2, 0,-1,0, 0,0; ...
    X1r,Y0,z2, 0,-1,0, 0,0; ...
    x1,Y0,z3, 0,-1,0, 0,0; ...
    X1f,Y0,z3, 0,-1,0, 0,0; ...
    X1u,Y0,z2, 0,-1,0, 0,0; ...
    X1u,Y0,z1, 0,-1,0, 0,0; ...
    X1f,Y0,z0, 0,-1,0, 0,0];
idx = [0,6,1; 6,7,1; 1,7,2; 7,8,2; 2,8,3; 8,9,3; 3,9,4; 9,10,4; 4,10,5; 10,11,5; ...
    12,0,13; 0,1,13; 13,1,14; 1,2,14; 14,2,15; 2,3,15; 15,3,16; 3,4,16; 16,4,17; 4,5,17; ...
    18,7,6; 18,19,7; 19,8,7; 19,20,8; 20,9,8; 20,21,9; 21,10,9; 21,22,10; 22,11,10; 22,23,11; ...
    19,18,24; 19,24,25; 20,19,25; 20,25,26; 21,20,26; 21,26,27; 22,21,27; 22,27,28; 23,22,28; 23,28,29; ...
    30,31,32; 33,34,35; 30,35,36; 30,36,39; 39,36,37; 39,37,38; ...
    40,42,41; 43,45,44; 40,46,45; 40,49,46; 49,47,46; 49,48,47];

% add connection to first joint
yj = Y1u + prm.joint.h;
zj = (z1+z2)/2;
x0 = -prm.joint.width/2;
x1 = -x0;
X0 = x0 + prm.frame.depth;
X1 = x1 - prm.frame.depth;
X0f = x0*2.5;
X1f = x1*2.5;
z0 = -prm.joint.depth/2;
z1 = -z0;
Z0 = z0 + prm.frame.depth;
Z1 = z1 - prm.frame.depth;
y1 = -prm.joint.rad*0.9;
y0 = -prm.joint.h;
Y0 = y0 + 0.005;
[jvtx,jidx] = make_cyl_x(0,0,0,prm.joint.width,prm.joint.rad,prm.joint.nseg);
mvtx = [X0,Y0,z0, 0,0,-1, 0,0; ...
    X0,y1,z0, 0,0,-1, 0,0; ...
    X1,Y0,z0, 0,0,-1, 0,0; ...
    X1,y1,z0, 0,0,-1, 0,0; ...
    x1,Y0,Z0, 1,0,0,  0,0; ...
    x1,y1,Z0, 1,0,0,  0,0; ...
    x1,Y0,Z1, 1,0,0,  0,0; ...
    x1,y1,Z1, 1,0,0,  0,0; ...
    X1,Y0,z1, 0,0,1,  0,0; ...
    X1,y1,z1, 0,0,1,  0,0; ...
    X0,Y0,z1, 0,0,1,  0,0; ...
    X0,y1,z1, 0,0,1,  0,0; ...
    x0,Y0,Z1, -1,0,0, 0,0; ...
    x0,y1,Z1, -1,0,0, 0,0; ...
    x0,Y0,Z0, -1,0,0, 0,0; ...
    x0,y1,Z0, -1,0,0, 0,0; ...
    X0f,y0,z0, 0,0,-1, 0,0; ...
    X0f,Y0,z0, 0,0,-1, 0,0; ...
    X1f,y0,z0, 0,0,-1, 0,0; ...
    X1f,Y0,z0, 0,0,-1, 0,0; ...
    X0f,Y0,z0, 0,1,0, 0,0; ...
    X0f,Y0,z1, 0,1,0, 0,0; ...
    X1f,Y0,z0, 0,1,0, 0,0; ...
    X1f,Y0,z1, 0,1,0, 0,0; ...
    X0f,y0,z0, -1,0,0, 0,0; ...
    X0f,y0,z1, -1,0,0, 0,0; ...
    X0f,Y0,z0, -1,0,0, 0,0; ...
    X0f,Y0,z1, -1,0,0, 0,0; ...
    X1f,y0,z0, 1,0,0, 0,0; ...
    X1f,y0,z1, 1,0,0, 0,0; ...
    X1f,Y0,z0, 1,0,0, 0,0; ...
    X1f,Y0,z1, 1,0,0, 0,0; ...
    ];
midx = [0,1,2; 1,3,2; 2,3,4; 3,5,4; 4,5,6; 5,7,6; 6,7,8; 7,9,8; 8,9,10; ...
    9,11,10; 10,11,12; 11,13,12; 12,13,14; 13,15,14; 14,15,0; 15,1,0; ...
    16,17,18; 17,19,18; 20,21,22; 21,23,22; 24,25,26; 26,25,27; ...
    28,30,29; 30,31,29];
[jvtx,jidx] = add_to_group(jvtx,jidx,mvtx,midx);
jvtx(:,2) = jvtx(:,2) + yj;
jvtx(:,3) = jvtx(:,3) + zj;
[vtx,idx] = add_to_group(vtx,idx,jvtx,jidx);

vtx = tilt_x(vtx,prm.pane.yref,prm.pane.zref,prm.pane.tilt);
end

% =========================================================================
% Generate vertices and faces for the HUD pane
function [vtx,idx] = make_hud_pane(prm)
x0 = -prm.pane.w/2;
x1 = -x0;
X0 = x0 + prm.pane.corner;
X1 = -X0;
y0 = prm.pane.yref - prm.pane.h/2;
y1 = prm.pane.yref + prm.pane.h/2;
Y0 = y0 + prm.pane.corner;
Y1 = y1 - prm.pane.corner;
z0 = prm.pane.zref;
z1 = z0 + prm.pane.depth;
c = 1/sqrt(2);
vtx = [X0,y0,z0, 0,0,-1, 0,0; ...
    X1,y0,z0, 0,0,-1, 0,0; ...
    x0,Y0,z0, 0,0,-1, 0,0; ...
    x1,Y0,z0, 0,0,-1, 0,0; ...
    x0,Y1,z0, 0,0,-1, 0,0; ...
    x1,Y1,z0, 0,0,-1, 0,0; ...
    X0,y1,z0, 0,0,-1, 0,0; ...
    X1,y1,z0, 0,0,-1, 0,0; ...
    x0,Y0,z0, -c,-c,0, 0,0; ...
    X0,y0,z0, -c,-c,0, 0,0; ...
    x0,Y0,z1, -c,-c,0, 0,0; ...
    X0,y0,z1, -c,-c,0, 0,0; ...
    X0,y0,z0, 0,-1,0, 0,0; ...
    X1,y0,z0, 0,-1,0, 0,0; ...
    X0,y0,z1, 0,-1,0, 0,0; ...
    X1,y0,z1, 0,-1,0, 0,0; ...
    X1,y0,z0, c,-c,0, 0,0; ...
    x1,Y0,z0, c,-c,0, 0,0; ...
    X1,y0,z1, c,-c,0, 0,0; ...
    x1,Y0,z1, c,-c,0, 0,0; ...
    x1,Y0,z0, 1,0,0, 0,0; ...
    x1,Y1,z0, 1,0,0, 0,0; ...
    x1,Y0,z1, 1,0,0, 0,0; ...
    x1,Y1,z1, 1,0,0, 0,0; ...
    x0,Y1,z0, -1,0,0, 0,0; ...
    x0,Y0,z0, -1,0,0, 0,0; ...
    x0,Y1,z1, -1,0,0, 0,0; ...
    x0,Y0,z1, -1,0,0, 0,0; ...
    x0,Y0,z0, c,c,0, 0,0; ...
    X0,y0,z0, c,c,0, 0,0; ...
    x0,Y0,z1, c,c,0, 0,0; ...
    X0,y0,z1, c,c,0, 0,0; ...
    X0,y0,z0, 0,1,0, 0,0; ...
    X1,y0,z0, 0,1,0, 0,0; ...
    X0,y0,z1, 0,1,0, 0,0; ...
    X1,y0,z1, 0,1,0, 0,0; ...
    X1,y0,z0, -c,c,0, 0,0; ...
    x1,Y0,z0, -c,c,0, 0,0; ...
    X1,y0,z1, -c,c,0, 0,0; ...
    x1,Y0,z1, -c,c,0, 0,0; ...
    x1,Y0,z0, -1,0,0, 0,0; ...
    x1,Y1,z0, -1,0,0, 0,0; ...
    x1,Y0,z1, -1,0,0, 0,0; ...
    x1,Y1,z1, -1,0,0, 0,0; ...
    x1,Y1,z0, -c,-c,0, 0,0; ...
    X1,y1,z0, -c,-c,0, 0,0; ...
    x1,Y1,z1, -c,-c,0, 0,0; ...
    X1,y1,z1, -c,-c,0, 0,0; ...
    X1,y1,z0, 0,-1,0, 0,0; ...
    X0,y1,z0, 0,-1,0, 0,0; ...
    X1,y1,z1, 0,-1,0, 0,0; ...
    X0,y1,z1, 0,-1,0, 0,0; ...
    X0,y1,z0, c,-c,0, 0,0; ...
    x0,Y1,z0, c,-c,0, 0,0; ...
    X0,y1,z1, c,-c,0, 0,0; ...
    x0,Y1,z1, c,-c,0, 0,0; ...
    x0,Y1,z0, 1,0,0, 0,0; ...
    x0,Y0,z0, 1,0,0, 0,0; ...
    x0,Y1,z1, 1,0,0, 0,0; ...
    x0,Y0,z1, 1,0,0, 0,0];
idx = [29,28,30; 29,30,31; ...
    33,32,34; 33,34,35; ...
    37,36,38; 37,38,39; ...
    41,40,42; 41,42,43; ...
    45,44,46; 45,46,47; ...
    49,48,50; 49,50,51; ...
    53,52,54; 53,54,55; ...
    57,56,58; 57,58,59; ...
    0,2,1; 1,2,3; 3,2,4; 3,4,5; 5,4,6; 5,6,7; ...
    8,9,11; 8,11,10; ...
    12,13,15; 12,15,14; ...
    16,17,19; 16,19,18; ...
    20,21,23; 20,23,22; ...
    24,25,27; 24,27,26];
vtx = tilt_x(vtx,prm.pane.yref,prm.pane.zref,prm.pane.tilt);
end

% =========================================================================
% Generate vertices and faces for the logical HUD surface
function [vtx,idx] = make_hudsurf(prm)
surf = prm.surf;
x0 = -surf.w/2;
x1 = -x0;
y0 = surf.y0;
y1 = surf.y1;
z  = surf.z;
u0 = surf.u0;
u1 = surf.u1;
v0 = surf.v0;
v1 = surf.v1;

vtx = [x0,y1,z, 0,0,-1, u0,v0; ...
    x1,y1,z, 0,0,-1, u1,v0; ...
    x1,y0,z, 0,0,-1, u1,v1; ...
    x0,y0,z, 0,0,-1, u0,v1];
idx = [2,3,0; 1,2,0];
end

% =========================================================================
function [vtx,idx] = make_cyl_x(xcnt,ycnt,zcnt,length,rad,nseg)
vtx = [];
idx = [];
x0 = -length/2;
x1 = -x0;
for i=0:nseg-1
    phi = i/nseg*2*pi;
    ny = sin(phi);
    nz = cos(phi);
    y = rad*ny;
    z = rad*nz;
    vtx(i*2+1,:) = [x0,y,z, 0,ny,nz, 0,0];
    vtx(i*2+2,:) = [x1,y,z, 0,ny,nz, 0,0];
    idx(i*2+1,:) = [i*2, i*2+1, mod(i*2+2,nseg*2)];
    idx(i*2+2,:) = [i*2+1, mod(i*2+3,nseg*2), mod(i*2+2,nseg*2)];
end
vtx(:,1) = vtx(:,1)+xcnt;
vtx(:,2) = vtx(:,2)+ycnt;
vtx(:,3) = vtx(:,3)+zcnt;
end

% =========================================================================
function [meshlist,prms] = dial1(rad,len)
nseg = 10;
z0 = -len;
z1 = z0+rad/4;
x0 = rad;
x1 = x0-rad/3;
u0 = texcrd(0.5);
u1 = texcrd(47.5);
profile.x = [x0,x0,x1];
profile.z = [0,z1,z0];
profile.nx = [1,1,0];
profile.nz = [0,0,-1];
profile.v  = texcrd([171.5,154.0,149.5]);
nprof = length(profile.x);
k = 2;
vtx(1,:) = [0,0,z0, 0,0,-1, (u0+u1)/2,texcrd(144.5)];
for i=0:nseg
    phi = i/nseg*2*pi;
    x = profile.x * sin(phi);
    y = profile.x * -cos(phi);
    z = profile.z;
    nx = profile.nx * sin(phi);
    ny = profile.nx * -cos(phi);
    nz = profile.nz;
    u = u0 + phi/(2*pi)*(u1-u0);
    v = profile.v;
    for j=1:nprof
        vtx(k,:) = [x(j),y(j),z(j), nx(j),ny(j),nz(j), u,v(j)];
        k = k+1;
    end
end
k = 1;
for i=0:nseg-1
    iofs = i*nprof+1;
    for j=0:nprof-2
        jofs = iofs+j;
        idx(k,:) = [jofs,jofs+1,jofs+nprof];
        k = k+1;
        idx(k,:) = [jofs+1,jofs+nprof+1,jofs+nprof];
        k = k+1;
    end
    idx(k,:) = [iofs+nprof-1,0,iofs+2*nprof-1];
    k = k+1;
end
vtx(:,3) = vtx(:,3) - 0.001;
meshlist(1) = struct('vtx',vtx, 'idx',idx, 'grpname','DIAL1', 'name','');

% the logical parameters to operate the dial
rad = rad*1.4;
p.mousearea = [-rad,-rad,0; ...
    rad,-rad,0; ...
    -rad, rad,0; ...
    rad, rad,0];
p.ref = [0,0,0];
p.axis = [0,0,1];
p.name = 'dial1';
prms{1} = p;
end

% =========================================================================
function [meshlist,prms] = button2(w,h,depth,name)
x0 = -w/2;
x1 = x0+w;
y0 = -h/2;
y1 = y0+h;
z0 = 0;
z1 = -depth;
u0 = texcrd(104.5);
u1 = texcrd(126.5);
v0 = texcrd(150.5);
v1 = texcrd(128.5);
vtx = [x0,y0,z1, 0,0,-1, u0,v0; ... % button top
    x1,y0,z1, 0,0,-1, u1,v0; ...
    x0,y1,z1, 0,0,-1, u0,v1; ...
    x1,y1,z1, 0,0,-1, u1,v1; ...
    x0,y0,z1, 0,-1,0, u0,v0; ... % bottom side
    x1,y0,z1, 0,-1,0, u1,v0; ...
    x0,y0,z0, 0,-1,0, u0,v0; ...
    x1,y0,z0, 0,-1,0, u1,v0; ...
    x1,y0,z1, 1,0,0,  u1,v0; ... % right side
    x1,y1,z1, 1,0,0,  u1,v1; ...
    x1,y0,z0, 1,0,0,  u1,v0; ...
    x1,y1,z0, 1,0,0,  u1,v1; ...
    x1,y1,z1, 0,1,0,  u1,v1; ... % top side
    x0,y1,z1, 0,1,0,  u0,v1; ...
    x1,y1,z0, 0,1,0,  u1,v1; ...
    x0,y1,z0, 0,1,0,  u0,v1; ...
    x0,y1,z1, -1,0,0, u0,v1; ... % left side
    x0,y0,z1, -1,0,0, u0,v0; ...
    x0,y1,z0, -1,0,0, u0,v1; ...
    x0,y0,z0, -1,0,0, u0,v0];
idx = [0,2,1; 1,2,3; 4,5,7; 4,7,6; 9,10,8; 9,11,10; 13,15,12; 12,15,14; 16,17,18; 18,17,19];
meshlist(1) = struct('vtx',vtx, 'idx',idx, 'grpname','BUTTON2', 'name','');
if nargin > 3
    meshlist(1).name = name;
end

p.ref = [0,0,0];
p.axis = [0,0,1];
p.mouserad = (w+h)/2;
if nargin > 3
    p.name = name;
else
    p.name = 'BUTTON2';
end
prms{1} = p;
end

% =========================================================================
function [meshlist,prms] = button3(this,name)
global g;
w = this.w;
h = this.h;
depth = this.depth;
x0 = -w/2;
x1 = x0+w;
y0 = -h/2;
y1 = y0+h;
z0 = 0;
z1 = -depth;
edge = 1*g.defscale;
X0 = x0+edge;
X1 = x1-edge;
Y0 = y0+edge;
Y1 = y1-edge;
Z1 = z1+edge;
u0 = texcrd( 4);
u1 = texcrd(38);
v0 = texcrd(340);
v1 = texcrd(316);
U0 = texcrd(0.5);
U1 = texcrd(41.5);
V0 = texcrd(343.5);
V1 = texcrd(312.5);

vtx = [X0,Y0,z1, 0,0,-1, u0,v0; ... % top
    X1,Y0,z1, 0,0,-1, u1,v0; ...
    X0,Y1,z1, 0,0,-1, u0,v1; ...
    X1,Y1,z1, 0,0,-1, u1,v1; ...

    X0,y0,Z1, 0,-1,0, u0,V0; ... % rim
    X1,y0,Z1, 0,-1,0, u1,V0; ...
    x1,Y0,Z1, 1,0,0,  U1,v0; ...
    x1,Y1,Z1, 1,0,0,  U1,v1; ...
    X1,y1,Z1, 0,1,0,  u1,V1; ...
    X0,y1,Z1, 0,1,0,  u0,V1; ...
    x0,Y1,Z1, -1,0,0, U0,v1; ...
    x0,Y0,Z1, -1,0,0, U0,v0; ...

    X0,y0,z0, 0,-1,0, u0,V0; ...
    X1,y0,z0, 0,-1,0, u1,V0; ...
    x1,Y0,z0, 1,0,0,  U1,v0; ...
    x1,Y1,z0, 1,0,0,  U1,v1; ...
    X1,y1,z0, 0,1,0,  u1,V1; ...
    X0,y1,z0, 0,1,0,  u0,V1; ...
    x0,Y1,z0, -1,0,0, U0,v1; ...
    x0,Y0,z0, -1,0,0, U0,v0];
idx = [0,2,1; 1,2,3; ...
    4,0,5; 5,0,1; 1,3,6; 6,3,7; 5,1,6; 3,2,9; 3,9,8; 7,3,8; 2,0,10; 10,0,11; 2,10,9; 0,4,11; ...
    12,4,13; 13,4,5; 6,7,14; 14,7,15; 13,5,14; 14,5,6; 8,9,17; 8,17,16; 7,8,16; 7,16,15; 10,11,18; 18,11,19; 9,10,18; 9,18,17; 11,4,12; 11,12,19];
meshlist(1) = struct('vtx',vtx, 'idx',idx, 'grpname','BUTTON3', 'name','');
if nargin > 1
    meshlist(1).name = name;
end

% label
u0 = this.label.u0;
u1 = this.label.u1;
v0 = this.label.v0;
v1 = this.label.v1;
Y1 = Y1-5*edge;
Y0 = Y1-7.2*g.defscale;%(X1-X0)*11/55;
X0 = -(Y1-Y0)/(v0-v1)*(u1-u0)/2;
X1 = -X0;
Z1 = z1 + g.label_ofs;
v0i = texcrd(10.5);
v1i = texcrd(10.5); % texcrd(1.5) if visible
Y1i = Y0-edge*4;
Y0i = Y1i-5.8909*g.defscale;%(X1-X0)*9/55;
vtx = [X0,Y0,Z1, 0,0,-1, u0,v0; ... % label
    X1,Y0,Z1, 0,0,-1, u1,v0; ...
    X0,Y1,Z1, 0,0,-1, u0,v1; ...
    X1,Y1,Z1, 0,0,-1, u1,v1; ...

    X0,Y0i,Z1, 0,0,-1, u0,v0i; ... % indicator
    X1,Y0i,Z1, 0,0,-1, u1,v0i; ...
    X0,Y1i,Z1, 0,0,-1, u0,v1i; ...
    X1,Y1i,Z1, 0,0,-1, u1,v1i];
idx = [0,2,1; 1,2,3; ...
    4,6,5; 5,6,7];
meshlist(2) = struct('vtx',vtx, 'idx',idx, 'grpname','LIT_SURF', 'name','');
if nargin > 1
    meshlist(2).name = [name '_LABEL'];
end

% shadow
pad = 12*w/(40*g.defscale);
X0 = x0 - pad*g.defscale;
X1 = x1 + pad*g.defscale;
Y0 = y0 - 12*g.defscale;
Y1 = y1 +  2*g.defscale;
Z1 = z0;
if isfield(this,'ofs')
    Z1 = Z1+this.ofs;
else
    Z1 = Z1+g.shadow_ofs;
end
u0 = texcrd(829);
u1 = texcrd(893);
v0 = texcrd(116);
v1 = texcrd( 72);
vtx = [X0,Y0,Z1, 0,0,-1, u0,v0; ...
    X1,Y0,Z1, 0,0,-1, u1,v0; ...
    X0,Y1,Z1, 0,0,-1, u0,v1; ...
    X1,Y1,Z1, 0,0,-1, u1,v1];
idx = [0,2,1; 1,2,3];
meshlist(3) = struct('vtx',vtx, 'idx',idx, 'grpname','SHADOW', 'name','');

p.ref = [0,0,0];
p.axis = [0,0,1];
p.mouserad = (w+h)/2;
if nargin > 1
    p.name = name;
else
    p.name = 'BUTTON3';
end
prms{1} = p;
end

% =========================================================================
function [meshlist,prms] = dial2(rad,len)
global g;
nseg = 16;
% make disc
z0 = -len/5;
z1 = z0/2;
x0 = rad;
x1 = x0-len/8;
u = texcrd([12,24,36]);
v = texcrd(142);
profile.x = [x0,x0,x1];
profile.z = [0,z1,z0];
profile.nx = [1,1,0];
profile.nz = [0,0,-1];
nprof = length(profile.x);
k = 2;
vtx(1,:) = [0,0,z0, 0,0,-1, u(3),v];
for i=0:nseg
    phi = i/nseg*2*pi;
    x = profile.x * sin(phi);
    y = profile.x * -cos(phi);
    z = profile.z;
    nx = profile.nx * sin(phi);
    ny = profile.nx * -cos(phi);
    nz = profile.nz;
    for j=1:nprof
        vtx(k,:) = [x(j),y(j),z(j), nx(j),ny(j),nz(j), u(j),v];
        k = k+1;
    end
end
k = 1;
for i=0:nseg-1
    iofs = i*nprof+1;
    for j=0:nprof-2
        jofs = iofs+j;
        idx(k,:) = [jofs,jofs+1,jofs+nprof];
        k = k+1;
        idx(k,:) = [jofs+1,jofs+nprof+1,jofs+nprof];
        k = k+1;
    end
    idx(k,:) = [iofs+nprof-1,0,iofs+2*nprof-1];
    k = k+1;
end

% make grip
edge = len*0.1;
z0 = -len;
Z0 = z0 + edge;
Z1 = -len/4;
x0 = -rad/3;
x1 = -x0;
X0 = x0 + edge;
X1 = x1 - edge;
Xt0 = -rad/8;
Xt1 = -Xt0;
y0 = -rad;
Y0 = y0 + edge;
y1 = rad*1.1;
Y1 = rad/3;
u0 = texcrd(48.5);
u1 = texcrd(95.5);
uc = (u0+u1)/2;
v0 = texcrd(171.5);
v1 = texcrd(144.5);
vc = (v0+v1)/2;
scl = (X1-X0)/(Xt1-Xt0);
c1s = sin(pi/6); c1c = cos(pi/6);
mvtx = [x1,Y0,0,  1,0,0, u1,v0; ...
    x1,Y1,0,  1,0,0, u1,vc; ...
    x1,Y0,Z0, 1,0,0, u1,v0; ...
    x1,Y1,Z0, 1,0,0, u1,vc; ...
    x0,Y0,0,  -1,0,0, u0,v0; ...
    x0,Y1,0,  -1,0,0, u0,vc; ...
    x0,Y0,Z0, -1,0,0, u0,v0; ...
    x0,Y1,Z0, -1,0,0, u0,vc; ...
    X1,y0,0,  0,-1,0, u1,v0; ...
    X1,y0,Z0, 0,-1,0, u1,v0; ...
    X0,y0,0,  0,-1,0, u0,v0; ...
    X0,y0,Z0, 0,-1,0, u0,v0; ...
    X1,Y0,z0, 0,0,-1, uc+texcrd(4*scl),v0; ...
    X1,Y1,z0, 0,0,-1, uc+texcrd(4*scl),vc+texcrd(4); ...
    X0,Y0,z0, 0,0,-1, uc-texcrd(4*scl),v0; ...
    X0,Y1,z0, 0,0,-1, uc-texcrd(4*scl),vc+texcrd(4); ...
    Xt1,y1,0, c1c,c1s,0, u1,v1; ...
    Xt1,y1,Z1, c1c,c1s,0, u1,v1; ...
    Xt0,y1,0, -c1c,c1s,0, u0,v1; ...
    Xt0,y1,Z1, -c1c,c1s,0, u0,v1; ...
    Xt1,y1,Z1, 0,c1s,-c1c, uc+texcrd(4),v1+texcrd(4); ...
    Xt0,y1,Z1, 0,c1s,-c1c, uc-texcrd(4),v1+texcrd(4); ...
    Xt1,y1,0, 0,1,0, uc+texcrd(4),v1+texcrd(1); ...
    Xt0,y1,0, 0,1,0, uc-texcrd(4),v1+texcrd(1)];
midx = [0,2,3; 0,3,1; 4,5,7; 4,7,6; 8,2,0; 8,9,2; 10,4,6; 10,6,11; 10,9,8; 10,11,9; ...
    2,9,12; 6,14,11; 9,11,12; 11,14,12; 2,12,3; 12,13,3; 12,14,13; 14,15,13; 6,15,14; 6,7,15; ...
    1,17,16; 1,3,17; 5,18,19; 5,19,7; 3,13,20; 13,15,20; 15,21,20; 7,21,15; 22,21,23; 22,20,21];
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);
vtx(:,3) = vtx(:,3) - 0.002;
meshlist(1) = struct('vtx',vtx, 'idx',idx, 'grpname','DIAL2', 'name','');

% create the shadow mesh
F = 0.0301; % convert pixels to dial radii
G = F*rad;  % convert pixels to mesh units
x0 = -39*G; x1 = 39*G;
y0 = -49*G; y1 = 16*G;
vtx = [x0, y0, g.shadow_ofs, 0,0,-1, texcrd( 945), texcrd(65); ...
    x1, y0, g.shadow_ofs, 0,0,-1, texcrd(1023), texcrd(65); ...
    x0, y1, g.shadow_ofs, 0,0,-1, texcrd( 945), texcrd( 1); ...
    x1, y1, g.shadow_ofs, 0,0,-1, texcrd(1023), texcrd( 1)];
idx = [0,2,1; 1,2,3];
meshlist(2) = struct('vtx',vtx, 'idx',idx, 'grpname','SHADOW', 'name','');

% the logical parameters to operate the dial
rad = 0.015;
p.mousearea = [-rad,-rad,0; ...
    rad,-rad,0; ...
    -rad, rad,0; ...
    rad, rad,0];
p.ref = [0,0,0];
p.axis = [0,0,1];
p.name = 'dial2';
prms{1} = p;
end

% =========================================================================
% flip switch (in central position), including base as panel stencil
function [meshlist,prms] = switch1(rad,len,name)
global g;
nseg = 8;
z0 = -len;
Z0 = z0 + rad/2;
z1 = z0/4;
Z1 = z1 - rad/4;
x0 = rad;
x1 = x0/2;
profile.x = [x1,x1,x0,x0];
profile.z = [0,z1,Z1,Z0];
profile.nx = [1,1,1,1];
profile.nz = [0,0,0,0];
profile.u = texcrd([64.5,64.5,65.5,90.5]);
v = texcrd(182);
nprof = length(profile.x);
vtx = [];
idx = [];
tip_idx = nseg*nprof;
for i=0:nseg-1
    phi = i/nseg*2*pi;
    x = profile.x * cos(phi);
    y = profile.x * sin(phi);
    z = profile.z;
    nx = profile.nx * cos(phi);
    ny = profile.nx * sin(phi);
    nz = profile.nz;
    u = profile.u;
    iofs = i*nprof;
    for j=1:nprof
        vtx = [vtx; x(j),y(j),z(j), nx(j),ny(j),nz(j), u(j),v];
    end
    for j = 0:nprof-2
        ofs = iofs+j;
        idx = [idx; ofs,ofs+1,mod(ofs+nprof,tip_idx); ofs+1,mod(ofs+nprof+1,tip_idx),mod(ofs+nprof,tip_idx)];
    end
    ofs = iofs+nprof-1;
    idx = [idx; ofs,tip_idx,mod(ofs+nprof,tip_idx)];
end
vtx = [vtx; 0,0,z0, 0,0,-1, texcrd(95.5),v];
meshlist(1) = struct('vtx',vtx, 'idx',idx, 'grpname','SWITCH1', 'name','');
if nargin > 2
    meshlist(1).name = name;
end

dx = rad*2;
u0 = texcrd(894);
u1 = texcrd(943);
v0 = texcrd(120);
v1 = texcrd( 71);
ofs = g.panelstencil_ofs;
basevtx = [-dx,-dx,ofs, 0,0,-1, u0,v0; ...
    dx,-dx,ofs, 0,0,-1, u1,v0; ...
    -dx, dx,ofs, 0,0,-1, u0,v1; ...
    dx, dx,ofs, 0,0,-1, u1,v1];
baseidx = [0,2,1; 1,2,3];
meshlist(2) = struct('vtx',basevtx, 'idx',baseidx, 'grpname','PANELSTENCIL', 'name','');

p.mousearea = [-0.01,-0.02,0; ...
    0.01,-0.02,0; ...
    -0.01, 0.02,0; ...
    0.01, 0.02,0];
p.ref = [0,0,0.003];
p.axis = [1,0,0];
if nargin > 2
    p.name = name;
else
    p.name = 'SWITCH1';
end
prms{1} = p;
end

% =========================================================================
% rocker switch (in central position), including shadow
function [meshlist,prms] = switch2(length,width,rot,name)
global g;
horz = (nargin >= 3 && rot == 'h'); % horizontal layout
% assume upright
x0 = -width/2;
x1 = x0+width;
y0 = -length/2;
y1 = y0+length;
z0 = length/5;
z1 = -length/5;
z2 = z1-length/7;
Y0 = y0 + length*0.4;
Y1 = y1 - length*0.4;
yh0 = y0+length/16;
yh1 = y1-length/16;
tu  = texcrd(35);
tu1 = texcrd(44);
tu2 = texcrd(24);
tv  = texcrd(142);
if horz
    tmp = tu1; tu1 = tu2; tu2 = tmp;
end
phi0 = 20*pi/180; sp0 = sin(phi0); cp0 = cos(phi0);
phi1 = phi0*2;    sp1 = sin(phi1); cp1 = cos(phi1);
vtx = [x0,yh0,z2,  0,sp1,-cp1,   tu,tv; ...
    x1,yh0,z2,  0,sp1,-cp1,   tu,tv; ...
    x0,Y0,z1,   0,sp0,-cp0,   tu,tv; ...
    x1,Y0,z1,   0,sp0,-cp0,   tu,tv; ...
    x0,Y1,z1,   0,-sp0,-cp0,  tu,tv; ...
    x1,Y1,z1,   0,-sp0,-cp0,  tu,tv; ...
    x0,yh1,z2,  0,-sp1,-cp1,  tu,tv; ...
    x1,yh1,z2,  0,-sp1,-cp1,  tu,tv; ...

    x0,yh0,z2,  0,-1,0,   tu1,tv; ...
    x1,yh0,z2,  0,-1,0,   tu1,tv; ...
    x0,y0,z0,   0,-1,0,   tu1,tv; ...
    x1,y0,z0,   0,-1,0,   tu1,tv; ...

    x0,yh1,z2,  0,1,0,    tu1,tv; ...
    x1,yh1,z2,  0,1,0,    tu1,tv; ...
    x0,y1,z0,   0,1,0,    tu1,tv; ...
    x1,y1,z0,   0,1,0,    tu1,tv; ...

    x0,y0,z0,  -1,0,0,    tu2,tv; ...
    x0,y1,z0,  -1,0,0,    tu2,tv; ...
    x0,yh0,z2, -1,0,0,    tu2,tv; ...
    x0,yh1,z2, -1,0,0,    tu2,tv; ...
    x0,Y0,z1,  -1,0,0,    tu2,tv; ...
    x0,Y1,z1,  -1,0,0,    tu2,tv; ...

    x1,y0,z0,  1,0,0,     tu2,tv; ...
    x1,y1,z0,  1,0,0,     tu2,tv; ...
    x1,yh0,z2, 1,0,0,     tu2,tv; ...
    x1,yh1,z2, 1,0,0,     tu2,tv; ...
    x1,Y0,z1,  1,0,0,     tu2,tv; ...
    x1,Y1,z1,  1,0,0,     tu2,tv];

idx = [0,2,1; 1,2,3; 2,4,3; 3,4,5; 4,6,5; 5,6,7; ...
    8,9,10; 10,9,11; ...
    12,14,13; 13,14,15; ...
    16,20,18; 16,21,20; 16,17,21; 17,19,21; ...
    22,24,26; 22,26,27; 22,27,23; 23,27,25];

if horz
    sinr = sin(pi/2);
    cosr = cos(pi/2);
    R = [cosr,sinr,0; -sinr,cosr,0; 0,0,1];
    for i=1:size(vtx,1)
        vtx(i,1:3) = (R*vtx(i,1:3)')';
        vtx(i,4:6) = (R*vtx(i,4:6)')';
    end
end

meshlist(1) = struct('vtx',vtx, 'idx',idx, 'grpname','SWITCH2', 'name','');
if nargin > 3
    meshlist(1).name = name;
end

% shadow
if ~horz
    sx0 = -21/18*width; sx1 = -sx0;
    sy0 = -41/18*width; sy1 = 27/18*width;
    vtx = [sx0,sy0,g.shadow_ofs, 0,0,-1, texcrd(831), texcrd(69); ...
        sx1,sy0,g.shadow_ofs, 0,0,-1, texcrd(873), texcrd(69); ...
        sx0,sy1,g.shadow_ofs, 0,0,-1, texcrd(831), texcrd( 1); ...
        sx1,sy1,g.shadow_ofs, 0,0,-1, texcrd(873), texcrd( 1)];
else
    sx0 = -39/18*width; sx1 = -sx0;
    sy0 = -22/18*width; sy1 = 11/18*width;
    vtx = [sx0,sy0,g.shadow_ofs, 0,0,-1, texcrd( 945), texcrd(159); ...
        sx1,sy0,g.shadow_ofs, 0,0,-1, texcrd(1023), texcrd(159); ...
        sx0,sy1,g.shadow_ofs, 0,0,-1, texcrd( 945), texcrd(126); ...
        sx1,sy1,g.shadow_ofs, 0,0,-1, texcrd(1023), texcrd(126)];
end
idx = [0,2,1; 1,2,3];
meshlist(2) = struct('vtx',vtx, 'idx',idx, 'grpname','SHADOW', 'name','');

% logical parameters
p.mousearea = [x0,y0,0; x1,y0,0; x0,y1,0; x1,y1,0];
if horz
    for i=1:4
        p.mousearea(i,:) = (R*p.mousearea(i,:)')';
    end
end
p.ref = [0,0,length/10];
p.axis = [1,0,0];
if horz
    p.axis = (R*p.axis')';
end
if nargin > 3
    p.name = name;
else
    p.name = 'SWITCH2';
end
prms{1} = p;
end

% =========================================================================
% a pair of switches with fixed distance (for the sake of using a
% combined shadow texture that doesn't have overlap issues)
% Here, origin is the centre point between the switches
function [meshlist,prms] = switch2_pair(length,width,rot,name)
global g;
horz = (nargin >= 3 && rot == 'h'); % horizontal layout
if (nargin < 4)
    name = 'SWITCH2';
end
gap = 8/18*width;      % gap between switches
shift = (width+gap)/2; % shift of centre
[meshlist,prms] = switch2(length,width,'v'); % make a single vertical switch
vtx = meshlist(1).vtx;
idx = meshlist(1).idx;
mvtx = vtx;
midx = idx;
vtx(:,1) = vtx(:,1)-shift;
mvtx(:,1) = mvtx(:,1)+shift;
[vtx,idx] = add_to_group (vtx,idx,mvtx,midx);
nvtx = size(vtx,1);
if horz
    sinr = sin(pi/2);
    cosr = cos(pi/2);
    R = [cosr,sinr,0; -sinr,cosr,0; 0,0,1];
    for i=1:nvtx
        vtx(i,1:3) = (R*vtx(i,1:3)')';
        vtx(i,4:6) = (R*vtx(i,4:6)')';
    end
end
meshlist(1) = struct('vtx',vtx, 'idx',idx, 'grpname','SWITCH2', 'name',name);

% shadow
if ~horz
    x0 = -34/18*width; x1 = -x0;
    y0 = -41/18*width; y1 = 27/18*width;
    vtx = [x0,y0,g.shadow_ofs, 0,0,-1, texcrd(875), texcrd(69); ...
        x1,y0,g.shadow_ofs, 0,0,-1, texcrd(943), texcrd(69); ...
        x0,y1,g.shadow_ofs, 0,0,-1, texcrd(875), texcrd( 1); ...
        x1,y1,g.shadow_ofs, 0,0,-1, texcrd(943), texcrd( 1)];
else
    x0 = -39/18*width; x1 = -x0;
    y0 = -35/18*width; y1 = 24/18*width;
    vtx = [x0,y0,g.shadow_ofs, 0,0,-1, texcrd( 945), texcrd(125); ...
        x1,y0,g.shadow_ofs, 0,0,-1, texcrd(1023), texcrd(125); ...
        x0,y1,g.shadow_ofs, 0,0,-1, texcrd( 945), texcrd( 66); ...
        x1,y1,g.shadow_ofs, 0,0,-1, texcrd(1023), texcrd( 66)];
end
idx = [0,2,1; 1,2,3];
meshlist(2) = struct('vtx',vtx, 'idx',idx, 'grpname','SHADOW', 'name','');

% logical parameters
x0 = -(width+gap/2);
x1 = -x0;
y0 = -length/2;
y1 = -y0;
p.mousearea = [x0,y0,0; x1,y0,0; x0,y1,0; x1,y1,0];
if horz
    for i=1:4
        p.mousearea(i,:) = (R*p.mousearea(i,:)')';
    end
end
p.ref = [0,0,length/10];
p.axis = [1,0,0];
if horz
    p.axis = (R*p.axis')';
end
if nargin > 3
    p.name = name;
else
    p.name = 'SWITCH2';
end
prms{1} = p;
end

% =========================================================================
function [vtx,idx] = make_switch4(xcnt,ycnt,zcnt,width,len)
x0 = xcnt-width/2;
x1 = x0+width;
y0 = ycnt-width/4;
y1 = y0+width/2;
Y0 = ycnt-width/3;
Y1 = ycnt+width/3;
Y0f = ycnt-width;
Y1f = ycnt+width;
z1 = zcnt-width/4;
z0 = zcnt-len;
Z1 = zcnt+width/4;
c = 1/sqrt(2);
vtx = [x0,y0,z0, 0,0,-1, 0,0; ...
    x1,y0,z0, 0,0,-1, 0,0; ...
    x0,y1,z0, 0,0,-1, 0,0; ...
    x1,y1,z0, 0,0,-1, 0,0; ...
    x0,Y0,z1, 0,-1,0, 0,0; ...
    x1,Y0,z1, 0,-1,0, 0,0; ...
    x0,y0,z0, 0,-1,0, 0,0; ...
    x1,y0,z0, 0,-1,0, 0,0; ...
    x0,Y1,z1, 0,1,0,  0,0; ...
    x1,Y1,z1, 0,1,0,  0,0; ...
    x0,y1,z0, 0,1,0,  0,0; ...
    x1,y1,z0, 0,1,0,  0,0; ...
    x0,y0,z0, -1,0,0, 0,0; ...
    x0,y1,z0, -1,0,0, 0,0; ...
    x0,Y0,z1, -1,0,0, 0,0; ...
    x0,Y1,z1, -1,0,0, 0,0; ...
    x0,Y0f,Z1, -1,0,0, 0,0; ...
    x0,Y1f,Z1, -1,0,0, 0,0; ...
    x1,y0,z0, 1,0,0, 0,0; ...
    x1,y1,z0, 1,0,0, 0,0; ...
    x1,Y0,z1, 1,0,0, 0,0; ...
    x1,Y1,z1, 1,0,0, 0,0; ...
    x1,Y0f,Z1, 1,0,0, 0,0; ...
    x1,Y1f,Z1, 1,0,0, 0,0; ...
    x0,Y0,z1, 0,0,-1, 0,0; ...
    x1,Y0,z1, 0,0,-1, 0,0; ...
    x0,Y0f,Z1,0,-1,0, 0,0; ...
    x1,Y0f,Z1,0,-1,0, 0,0; ...
    x0,Y1,z1, 0,0,-1, 0,0; ...
    x1,Y1,z1, 0,0,-1, 0,0; ...
    x0,Y1f,Z1, 0,1,0, 0,0; ...
    x1,Y1f,Z1, 0,1,0, 0,0];
idx = [0,3,1; 0,2,3; 6,7,5; 6,5,4; 8,9,10; 9,11,10; ...
    12,14,13; 13,14,15; 14,16,15; 15,16,17; ...
    18,19,20; 19,21,20; 20,21,22; 21,23,22;
    24,25,26; 25,27,26; 29,28,31; 28,30,31];
end

% =========================================================================
% Meshes and parameters for a trim wheel segment (rotating around x-axis)
% including scale and labels
function [meshlist,prms] = make_elevtrim_wheel(this)
global g;
wheel = this.wheel;
slit = this.slit;
scale = this.scale;
label = this.label;

x0 = -0.006;
X0 = -0.004;
X1 = -X0;
x1 = -x0;
y1 = wheel.rad;
Y1 = y1 - 0.002;
y0 = y1 - 0.014;
Y0 = y0 + 0.002;
u0 = texcrd(0.5);
u1 = texcrd(63.5);
vw = texcrd(174);
ud0 = texcrd(28);
ud1 = texcrd(64);
vd = texcrd(15);
profile.x =  [X0,X0,X0,x0,x0,X0,X1,x1,x1,X1,X1,X1];
profile.y =  [ 0,y0,y0,Y0,Y1,y1,y1,Y1,Y0,y0,y0, 0];
profile.nx = [-1,-1, 0,-1,-1, 0, 0, 1, 1, 0, 1, 1];
profile.ny = [ 0, 0,-1, 0, 0, 1, 1, 0, 0,-1, 0, 0];
np = length(profile.x);

vtx = [];
idx = [];
for i=0:wheel.nseg
    phi = -wheel.aperture/2 + wheel.aperture*i/wheel.nseg + wheel.rot;
    sinp = sin(phi);
    cosp = cos(phi);
    if mod(i,2), uw = u0; ud = ud0; else uw = u1; ud = ud1; end
    for j=1:np
        nx = profile.nx(j);
        ny = profile.ny(j)*cosp;
        nz = profile.ny(j)*sinp;
        x  = profile.x(j);
        y  = profile.y(j)*cosp;
        z  = profile.y(j)*sinp;
        if (j >= 3 && j <= 10)
            u = uw; v = vw;
        else
            u = ud; v = vd;
        end
        vtx = [vtx; x,y,z, nx,ny,nz, u,v];
    end
end
for i=1:size(vtx,1)
    vtx(i,1:3) = vtx(i,1:3)+wheel.rot_ref;
end
for i=1:wheel.nseg
    ofsi = (i-1)*np;
    for j=1:np-1
        ofs = ofsi+j-1;
        idx = [idx; ofs,ofs+np,ofs+1; ofs+1,ofs+np,ofs+np+1];
    end
end
meshlist = struct('vtx',vtx, 'idx',idx, 'grpname','ETRIM_WHEEL', 'name','');

% well
xm = 0;
ym = 0;
x0 = xm - slit.w/2;
x1 = x0 + slit.w;
X1 = x1 + scale.w;
y0 = ym - slit.h/2;
y1 = y0 + slit.h;
Y0 = ym - scale.h/2;
Y1 = Y0 + scale.h;
z0 = 0;
z1 = z0 + scale.z;
z2 = z0 + 0.02;
u = texcrd(12);
u1 = texcrd(0.5);
v = texcrd(142);

vtx = [x1,Y0,z0, 0,1,0, u,v; ... % rim around scale
    X1,Y0,z0, 0,1,0, u,v; ...
    x1,Y0,z1, 0,1,0, u,v; ...
    X1,Y0,z1, 0,1,0, u,v; ...
    X1,Y0,z0, -1,0,0, u,v; ...
    X1,Y1,z0, -1,0,0, u,v; ...
    X1,Y0,z1, -1,0,0, u,v; ...
    X1,Y1,z1, -1,0,0, u,v; ...
    X1,Y1,z0, 0,-1,0, u,v; ...
    x1,Y1,z0, 0,-1,0, u,v; ...
    X1,Y1,z1, 0,-1,0, u,v; ...
    x1,Y1,z1, 0,-1,0, u,v; ...

    x0,y0,z0, 0,1,0, u,v; ... % rim around wheel
    x1,y0,z0, 0,1,0, u,v; ...
    x0,y0,z2, 0,1,0, u1,v; ...
    x1,y0,z2, 0,1,0, u1,v; ...
    x0,y0,z0, 1,0,0, u,v; ...
    x0,y1,z0, 1,0,0, u,v; ...
    x0,y0,z2, 1,0,0, u1,v; ...
    x0,y1,z2, 1,0,0, u1,v; ...
    x0,y1,z0, 0,-1,0, u,v; ...
    x1,y1,z0, 0,-1,0, u,v; ...
    x0,y1,z2, 0,-1,0, u1,v; ...
    x1,y1,z2, 0,-1,0, u1,v];
idx = [0,2,1; 1,2,3; 4,6,5; 5,6,7; 8,10,9; 9,10,11; ...
    12,14,13; 13,14,15; 16,17,19; 16,19,18; 20,21,23; 20,23,22];
sublist = struct('vtx',vtx, 'idx',idx, 'grpname','VC4_DEFAULT', 'name','');
meshlist = meshmerge(meshlist,sublist);

p.axis = [1,0,0];
p.ref = this.wheel.rot_ref;
p.mousearea = [xm-0.015,y0-0.005,z0; ...
    xm+0.015,y0-0.005,z0; ...
    xm-0.015,y1+0.005,z0; ...
    xm+0.015,y1+0.005,z0];
p.name = 'ETRIMWHEEL';
prms{1} = p;

% indicator scale and needle
x0 = slit.w/2;
x1 = x0 + scale.w;
ym = 0;
y0 = ym - scale.h/2;
y1 = y0 + scale.h;
z0 = 0;
z1 = z0 + scale.z;
u0 = scale.u0;
u1 = scale.u1;
v0 = scale.v0;
v1 = scale.v1;
un = texcrd(34);
vn = texcrd(134);
vtx = [x1,ym-0.003,z0, 0,0,-1, un,vn; ... % indicator needle
    x1-0.007,ym,z0, 0,0,-1, un,vn; ...
    x1,ym+0.003,z0,0,0,-1, un,vn; ...
    x0,y0,z1, 0,0,-1, u0,v0; ... % scale
    x1,y0,z1, 0,0,-1, u1,v0; ...
    x0,y1,z1, 0,0,-1, u0,v1; ...
    x1,y1,z1, 0,0,-1, u1,v1];
idx = [0,1,2; ... % needle
       3,5,4; 4,5,6]; % scale
sublist = struct('vtx',vtx, 'idx',idx, 'grpname','VC4_LIT', 'name','ETRIMSCALE');
meshlist = meshmerge(meshlist,sublist);

ps.axis = [0,1,0];
ps.ref = vtx(1:3,1:3); % needle
ps.name = 'ETRIMSCALE';
subprms{1} = ps;
prms = [prms;subprms];

% label
x0 = label.x0;
x1 = x0 + label.w;
ym = 0;
y0 = ym - label.h/2;
y1 = y0 + label.h;
z0 = g.label_ofs;
u0 = label.u0;
u1 = label.u1;
v0 = label.v0;
v1 = label.v1;
vtx = [x0,y0,z0, 0,0,-1, u0,v0; ...
    x1,y0,z0, 0,0,-1, u1,v0; ...
    x0,y1,z0, 0,0,-1, u0,v1; ...
    x1,y1,z0, 0,0,-1, u1,v1];
idx = [0,2,1; 1,2,3];
sublist = struct('vtx',vtx, 'idx',idx, 'grpname','LIT_LABELS', 'name','');
meshlist = meshmerge(meshlist, sublist);

% transform into parent frame
meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);

end

% =========================================================================
% Meshes and parameters for the gear lever (rotating around x-axis),
% indicator and label
function [meshlist,prms] = make_gear_lever(this)
global g;
lever = this.lever;
ind   = this.indicator;
label = this.label;

% stick
nseg = 8;
z0 = -lever.stick.length;
z1 = -lever.axis.rad*0.9;
rad = lever.stick.rad;
vtx = [];
idx = [];
u0 = texcrd(40);
u1 = texcrd(10);
v = texcrd(142);
for i=0:nseg-1
    phi = 2*pi*i/nseg;
    nx = cos(phi);
    ny = sin(phi);
    x = rad*nx;
    y = rad*ny;
    vtx = [vtx; x,y,z0, nx,ny,0, u0,v; ...
        x,y,z1, nx,ny,0, u1,v];
    ofs = i*2;
    idx = [idx; ofs, mod(ofs+2,nseg*2), ofs+1; ...
        ofs+1, mod(ofs+2,nseg*2), mod(ofs+3,nseg*2)];
end

% knob
w = lever.knob.w/2;
h = lever.knob.rad;
corner = w/2;
profile.x = [w,w,w-corner,-w+corner,w,w];
profile.z = [0,h-corner,h,h,h-corner,0];
profile.nx = [1,1,0,0,-1,-1];
profile.nz = [0,0,1,1,0,0];
profile.u = texcrd([64.5,106,114,123,115,64.5]);
v = texcrd(186);
nprof = length(profile.x);
nseg = 12;
mvtx = [];
midx = [];
nv = nseg*nprof;
for i=0:nseg-1
    phi = 2*pi*i/nseg;
    nx = profile.nx;
    ny = profile.nz*sin(phi);
    nz = profile.nz*cos(phi);
    x = profile.x;
    y = profile.z*sin(phi);
    z = profile.z*cos(phi);
    u = profile.u;
    for j=1:nprof
        mvtx = [mvtx; x(j),y(j),z(j), nx(j),ny(j),nz(j), u(j),v];
    end
    iofs = i*nprof;
    for j=0:nprof-2
        ofs = iofs+j;
        midx = [midx; ofs, mod(ofs+nprof,nv), ofs+1; ...
            ofs+1, mod(ofs+nprof,nv), mod(ofs+nprof+1,nv)];
    end
end
mvtx(:,3) = mvtx(:,3)+z0-lever.knob.rad+0.01;
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);
vtx(:,3) = vtx(:,3) + lever.depth;

meshlist = struct('vtx',vtx, 'idx',idx, 'grpname','GEAR_LEVER', 'name','');

% well
phi = atan(this.slit.h2/lever.depth); % lever tilt ange
oval_scale = 1/cos(phi);
r = lever.depth;
d = this.slit.depth;
z0 = 0;
z1 = z0 + d;
sx = 0;
sy = 0;
sw2 = this.slit.w2;
sh2 = this.slit.h2;
sh0 = sh2 + sw2*oval_scale;
sw1 = sw2/sqrt(2);
sh1 = sh2 + sw1*oval_scale;
sv = texcrd(142);
su1 = texcrd(30);
su2 = texcrd(1);
nml1a = [1,1,-1]; nml1a = nml1a/norm(nml1a);
nml1b = [1,-1,-1]; nml1b = nml1b/norm(nml1b);
nml0a = [0,1,-1]; nml0a = nml0a/norm(nml0a);
nml0b = [0,-1,-1]; nml0b = nml0b/norm(nml0b);
nml2a = [-1,1,-1]; nml2a = nml2a/norm(nml2a);
nml2b = [-1,-1,-1]; nml2b = nml2b/norm(nml2b);
vtx = [ ...
    % gear lever well
    -sw2,-sh2,z0, 1,0,0,  su1,sv; ...
    -sw2, sh2,z0, 1,0,0,  su1,sv; ...
    -sw2,-sh2*(r-d)/r,z1, 1,0,0, su2,sv; ...
    -sw2, sh2*(r-d)/r,z1, 1,0,0, su2,sv; ...
    -sw1,-sh1,z0,         nml1a, su1,sv; ...
    -sw1,-sh1*(r-d)/r,z1, nml1a, su2,sv; ...
    -sw1, sh1,z0,         nml1b, su1,sv; ...
    -sw1, sh1*(r-d)/r,z1, nml1b, su2,sv; ...
    0,-sh0,z0,         nml0a, su1,sv; ...
    0,-sh0*(r-d)/r,z1,     nml0a, su2,sv; ...
    0, sh0,z0,             nml0b, su1,sv; ...
    0, sh0*(r-d)/r,z1,     nml0b, su2,sv; ...
    sw1,-sh1,z0,         nml2a, su1,sv; ...
    sw1,-sh1*(r-d)/r,z1, nml2a, su2,sv; ...
    sw1, sh1,z0,         nml2b, su1,sv; ...
    sw1, sh1*(r-d)/r,z1, nml2b, su2,sv; ...
    sw2,-sh2,z0,         -1,0,0,su1,sv; ...
    sw2,-sh2*(r-d)/r,z1, -1,0,0,su2,sv; ...
    sw2, sh2,z0,         -1,0,0,su1,sv; ...
    sw2, sh2*(r-d)/r,z1, -1,0,0,su2,sv];
idx = [0,1,2; 1,3,2; 0,2,5; 0,5,4; 1,6,3; 3,6,7; 4,5,9; 4,9,8; 6,10,7; 7,10,11; 8,9,13; 8,13,12; 10,14,11; 11,14,15; 12,13,17; 12,17,16; 14,18,15; 15,18,19; 16,17,19; 16,19,18];

% rotation drum
nseg = 5;
range = pi*0.48;
yofs = sy;
zofs = lever.depth;
u0 = lever.axis.u(1);
u1 = lever.axis.u(2);
v0 = lever.axis.v(1);
v1 = lever.axis.v(2);
mvtx = [];
for i=0:nseg
    theta = -range/2 + i/nseg*range;
    ny =  sin(theta);
    nz = -cos(theta);
    u = u0 + (u1-u0)*i/nseg;
    y =  lever.axis.rad*ny + yofs;
    z =  lever.axis.rad*nz + zofs;
    mvtx = [mvtx; -sw2,y,z, 0,ny,nz, u,v0; ...
        sw2,y,z, 0,ny,nz, u,v1];
end
midx = [];
for i=0:nseg-1
    ofs = i*2;
    midx = [midx; ofs,ofs+2,ofs+1; ofs+1,ofs+2,ofs+3];
end
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);

% indicator frame
x0 = ind.xcnt-ind.w/2;
x1 = x0+ind.w;
y0 = ind.ycnt-ind.h/2;
y1 = y0+ind.h;
z1 = 0;
z0 = z1 + ind.z0;
us = texcrd(59.5);
vs = texcrd(280);
mvtx = [x1,y0,z0, 1,0,0, us,vs; ...
     x1,y0,z1, 1,0,0, us,vs; ...
     x1,y1,z0, 1,0,0, us,vs; ...
     x1,y1,z1, 1,0,0, us,vs; ...
     x1,y1,z0, 0,1,0, us,vs; ...
     x1,y1,z1, 0,1,0, us,vs; ...
     x0,y1,z0, 0,1,0, us,vs; ...
     x0,y1,z1, 0,1,0, us,vs];
midx = [0,2,1; 1,2,3; 5,4,6; 5,6,7];
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);
sublist = struct('vtx',vtx, 'idx',idx, 'grpname','VC4_DEFAULT', 'name','');
meshlist = meshmerge(meshlist,sublist);

% indicator
u0 = ind.u(1);
u1 = ind.u(2);
v0 = ind.v(1);
v1 = ind.v(2);
vtx = [x0,y0,z0, 0,0,-1, u0,v0; ...
    x1,y0,z0, 0,0,-1, u1,v0; ...
    x0,y1,z0, 0,0,-1, u0,v1; ...
    x1,y1,z0, 0,0,-1, u1,v1];
idx = [0,2,1; 1,2,3];
sublist = struct('vtx',vtx, 'idx',idx, 'grpname','VC4_LIT', 'name','GEAR_INDICATOR');
meshlist = meshmerge(meshlist,sublist);

% label
x0 = this.slit.w2 + 0.001;
x1 = x0 + label.w;
ym = 0;
y0 = ym - label.h/2;
y1 = y0 + label.h;
z0 = g.label_ofs;
u0 = label.u0;
u1 = label.u1;
v0 = label.v0;
v1 = label.v1;
vtx = [x0,y0,z0, 0,0,-1, u0,v0; ...
    x1,y0,z0, 0,0,-1, u1,v0; ...
    x0,y1,z0, 0,0,-1, u0,v1; ...
    x1,y1,z0, 0,0,-1, u1,v1];
idx = [0,2,1; 1,2,3];

sublist = struct('vtx',vtx, 'idx',idx, 'grpname','LIT_LABELS', 'name','');
meshlist = meshmerge(meshlist, sublist);

p.ref = [0,0,lever.depth];
p.axis = [1,0,0];
p.mousearea = [-0.015, -sh0-0.01, 0; ...
                0.015, -sh0-0.01, 0; ...
               -0.015,  sh0+0.01, 0; ...
                0.015,  sh0+0.01, 0];
p.name = 'GEARLEVER';
prms{1} = p;

% transform into parent frame
meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);
end

% =========================================================================
% Meshes and parameters for the airbrake lever (rotating around x-axis)
% including slit and labels
function [meshlist,prms] = make_airbrake_lever(this)
global g;
lever = this.lever;
slit  = this.slit;

% lever (stick)
xm = 0;
x0 = xm - lever.stick.w/2;
x1 = xm + lever.stick.w/2;
y0 = lever.rot_ref(2) - lever.stick.h/2;
y1 = y0 + lever.stick.h;
Y0 = y0 - lever.stick.w/3;
Y1 = y1 + lever.stick.w/3;
z0 = lever.stick.z(1);
z1 = lever.stick.z(2);
u1 = texcrd(40);
u0 = texcrd(10);
v = texcrd(142);
vtx = [x1,y0,z0, 1,0,0, u0,v; ...
    x1,y0,z1, 1,0,0, u1,v; ...
    x1,y1,z0, 1,0,0, u0,v; ...
    x1,y1,z1, 1,0,0, u1,v; ...
    x0,y0,z0, -1,0,0,u0,v; ...
    x0,y0,z1, -1,0,0,u1,v; ...
    x0,y1,z0, -1,0,0,u0,v; ...
    x0,y1,z1, -1,0,0,u1,v; ...
    xm,Y0,z0, 0,-1,0,u0,v; ...
    xm,Y0,z1, 0,-1,0,u1,v; ...
    xm,Y1,z0, 0,1,0, u0,v; ...
    xm,Y1,z1, 0,1,0, u1,v];
idx = [0,1,2; 2,1,3; 4,6,5; 5,6,7; 8,9,0; 0,9,1; 4,5,9; 4,9,8; 2,3,10; 10,3,11; 6,10,7; 7,10,11];

% lever (handle)
X0 = xm - lever.handle.w/2 + 0.01;
X1 = xm + lever.handle.w/2 + 0.01;
x0 = X0 + lever.handle.corner;
x1 = X1 - lever.handle.corner;
y0 = Y0;
y1 = Y1;
Y0 = y0 - lever.handle.corner;
Y1 = y1 + lever.handle.corner;
z0 = z1;
z1 = z0 - lever.handle.depth;
Z0 = z0 - lever.handle.corner;
Z1 = z1 + lever.handle.corner;
u = lever.handle.u;
v = lever.handle.v;
mvtx = [x0,y0,z1, 0,0,-1, u,v; ...
    x1,y0,z1, 0,0,-1, u,v; ...
    x0,y1,z1, 0,0,-1, u,v; ...
    x1,y1,z1, 0,0,-1, u,v; ...
    X0,y0,Z1, -1,0,0, u,v; ...
    X0,y1,Z1, -1,0,0, u,v; ...
    X1,y0,Z1, 1,0,0,  u,v; ...
    X1,y1,Z1, 1,0,0,  u,v; ...
    x0,Y0,Z1, 0,-1,0, u,v; ...
    x1,Y0,Z1, 0,-1,0, u,v; ...
    x0,Y1,Z1, 0,1,0,  u,v; ...
    x1,Y1,Z1, 0,1,0,  u,v; ...
    X0,y0,Z0, -1,0,0, u,v; ...
    X0,y1,Z0, -1,0,0, u,v; ...
    X1,y0,Z0, 1,0,0,  u,v; ...
    X1,y1,Z0, 1,0,0,  u,v; ...
    x0,Y0,Z0, 0,-1,0, u,v; ...
    x1,Y0,Z0, 0,-1,0, u,v; ...
    x0,Y1,Z0, 0,1,0,  u,v; ...
    x1,Y1,Z0, 0,1,0,  u,v];
midx = [0,2,1; 1,2,3; 0,5,2; 0,4,5; 1,3,6; 6,3,7; 0,1,9; 0,9,8; 0,8,4; 1,6,9; 2,10,3; 3,10,11; 2,5,10; 3,11,7; ...
    4,13,5; 4,12,13; 14,6,7; 14,7,15; 8,9,17; 8,17,16; 10,18,11; 11,18,19; ...
    4,8,16; 4,16,12; 9,6,14; 9,14,17; 5,18,10; 5,13,18; 7,11,19; 7,19,15];
[vtx,idx] = add_to_group (vtx,idx, mvtx, midx);
meshlist = struct('vtx',vtx, 'idx',idx, 'grpname','AIRBRAKE_LEVER', 'name','');

% well
z0 = 0;
x0ab = -slit.w/2;
x1ab = x0ab + slit.w;
y0ab = -slit.h/2;
y1ab = y0ab + slit.h;
zab = z0 + 0.004;
u = texcrd(12);
v = texcrd(142);
vtx = [x0ab,y0ab,z0, 1,0,0, u,v; ... % slit rim
    x0ab,y1ab,z0, 1,0,0, u,v; ...
    x0ab,y0ab,zab, 1,0,0, u,v; ...
    x0ab,y1ab,zab, 1,0,0, u,v; ...
    x0ab,y0ab,z0, 0,1,0, u,v; ...
    x1ab,y0ab,z0, 0,1,0, u,v; ...
    x0ab,y0ab,zab, 0,1,0, u,v; ...
    x1ab,y0ab,zab, 0,1,0, u,v];
idx = [0,1,2; 2,1,3; 4,6,7; 4,7,5];

% brush
c0 = cos(20*pi/180); s0 = sin(20*pi/180);
u0 = texcrd(635.5); u1 = texcrd(834.5);
v0 = texcrd(180.5); v1 = texcrd(225.5);
xm = (x0ab+x1ab)/2;
zm = zab + 0.003;
mvtx = [x0ab,y0ab,zab, s0,0,-c0, u0,v0; ...
    x0ab,y1ab,zab, s0,0,-c0, u1,v0; ...
    xm,  y0ab,zm,  s0,0,-c0, u0,v1; ...
    xm,  y1ab,zm,  s0,0,-c0, u1,v1; ...

    xm,  y0ab,zm,  -s0,0,-c0, u0,v1; ...
    xm,  y1ab,zm,  -s0,0,-c0, u1,v1; ...
    x1ab,y0ab,zab, -s0,0,-c0, u0,v0; ...
    x1ab,y1ab,zab, -s0,0,-c0, u1,v0];
midx = [0,1,2; 1,3,2; 4,5,7; 4,7,6];
[vtx,idx] = add_to_group (vtx,idx,mvtx,midx);
sublist = struct('vtx',vtx, 'idx',idx, 'grpname','VC4_DEFAULT', 'name','');
meshlist = meshmerge(meshlist,sublist);

% label (left part)
F = g.defscale;
x0 = -slit.w/2 - F*15;
x1 = x0 + F*11;
y0 = -F*55;
y1 = y0 + F*110;
z0 = g.label_ofs;
u0 = texcrd(199);
u1 = texcrd(199+11);
v0 = texcrd(128);
v1 = texcrd(128-110);
vtx = [x0,y0,z0, 0,0,-1, u0,v0; ...
    x1,y0,z0, 0,0,-1, u1,v0; ...
    x0,y1,z0, 0,0,-1, u0,v1; ...
    x1,y1,z0, 0,0,-1, u1,v1];
idx = [0,2,1; 1,2,3];

% label (right part)
x0 = slit.w/2 + F*4;
x1 = x0 + F*26;
y0 = -F*72.5;
y1 = y0 + F*145;
u0 = texcrd(212);
u1 = texcrd(212+26);
v0 = texcrd(145.5);
v1 = texcrd(0.5);
mvtx = [x0,y0,z0, 0,0,-1, u0,v0; ...
    x1,y0,z0, 0,0,-1, u1,v0; ...
    x0,y1,z0, 0,0,-1, u0,v1; ...
    x1,y1,z0, 0,0,-1, u1,v1];
midx = [0,2,1; 1,2,3];
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);
sublist = struct('vtx',vtx, 'idx',idx, 'grpname','LIT_LABELS', 'name','');
meshlist = meshmerge(meshlist,sublist);



p.ref = lever.rot_ref;
p.axis = [1,0,0];
p.mousearea = [x0ab-0.015,y0ab,z0; ...
    x1ab+0.015,y0ab,z0; ...
    x0ab-0.015,y1ab,z0; ...
    x1ab+0.015,y1ab,z0];
p.name = 'AIRBRAKELEVER';
prms{1} = p;

% transform into parent frame
meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);

end

% =========================================================================
% Meshes and parameters for the nosecone lever (rotating around x-axis),
% indicator and label
function [meshlist,prms] = make_ncone_lever(this)
global g;
lever = this.lever;
ind   = this.indicator;
label = this.label;

% stick
nseg = 8;
z0 = -lever.stick.length;
z1 = -lever.axis.rad*0.9;
rad = lever.stick.rad;
vtx = [];
idx = [];
u0 = texcrd(40);
u1 = texcrd(10);
v = texcrd(142);
for i=0:nseg-1
    phi = 2*pi*i/nseg;
    nx = cos(phi);
    ny = sin(phi);
    x = rad*nx;
    y = rad*ny;
    vtx = [vtx; x,y,z0, nx,ny,0, u0,v; ...
        x,y,z1, nx,ny,0, u1,v];
    ofs = i*2;
    idx = [idx; ofs, mod(ofs+2,nseg*2), ofs+1; ...
        ofs+1, mod(ofs+2,nseg*2), mod(ofs+3,nseg*2)];
end

% handle
nseg = lever.handle.nseg;
z1 = z0-lever.handle.length;
Z0 = z0-lever.handle.corner;
Z1 = z1+lever.handle.corner;
profile.x = [lever.stick.rad, lever.handle.rad, lever.handle.rad, lever.handle.rad-lever.handle.corner, 0];
profile.z = [z0, Z0, Z1, z1, z1];
profile.nx = [0,1,1,0,0];
profile.nz = [1,0,0,-1,-1];
profile.v = texcrd([183.5, 180.5, 171.5, 165.5, 157.5]);
nprof = length(profile.x);
nv = nseg*nprof;
mvtx = [];
midx = [];
for i=0:nseg-1
    phi = 2*pi*i/nseg;
    nx = profile.nx*cos(phi);
    ny = profile.nx*sin(phi);
    nz = profile.nz;
    x  = profile.x*cos(phi);
    y  = profile.x*sin(phi);
    z  = profile.z;
    v  = profile.v;
    if mod(i,2)
        u = texcrd(113);
    else
        u = texcrd(97);
    end
    for j=1:nprof
        mvtx = [mvtx; x(j),y(j),z(j), nx(j),ny(j),nz(j), u,v(j)];
    end
    iofs = i*nprof;
    for j=0:nprof-2
        ofs = iofs+j;
        midx = [midx; ofs,ofs+1,mod(ofs+nprof,nv); ofs+1,mod(ofs+nprof+1,nv),mod(ofs+nprof,nv)];
    end
end
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);
vtx(:,3) = vtx(:,3) + lever.depth;

meshlist = struct('vtx',vtx, 'idx',idx, 'grpname','NOSECONE_LEVER', 'name','');

% well
phi = atan(this.slit.h2/lever.depth); % lever tilt ange
oval_scale = 1/cos(phi);
r = lever.depth;
d = this.slit.depth;
z0 = 0;
z1 = z0 + d;
sx = 0;
sy = 0;
sw2 = this.slit.w2;
sh2 = this.slit.h2;
sh0 = sh2 + sw2*oval_scale;
sw1 = sw2/sqrt(2);
sh1 = sh2 + sw1*oval_scale;
sv = texcrd(142);
su1 = texcrd(30);
su2 = texcrd(1);
nml1a = [1,1,-1]; nml1a = nml1a/norm(nml1a);
nml1b = [1,-1,-1]; nml1b = nml1b/norm(nml1b);
nml0a = [0,1,-1]; nml0a = nml0a/norm(nml0a);
nml0b = [0,-1,-1]; nml0b = nml0b/norm(nml0b);
nml2a = [-1,1,-1]; nml2a = nml2a/norm(nml2a);
nml2b = [-1,-1,-1]; nml2b = nml2b/norm(nml2b);
vtx = [ ...
    % nosecone lever well
    sx-sw2,sy-sh2,z0, 1,0,0,  su1,sv; ...
    sx-sw2,sy+sh2,z0, 1,0,0,  su1,sv; ...
    sx-sw2,sy-sh2*(r-d)/r,z1, 1,0,0, su2,sv; ...
    sx-sw2,sy+sh2*(r-d)/r,z1, 1,0,0, su2,sv; ...
    sx-sw1,sy-sh1,z0,         nml1a, su1,sv; ...
    sx-sw1,sy-sh1*(r-d)/r,z1, nml1a, su2,sv; ...
    sx-sw1,sy+sh1,z0,         nml1b, su1,sv; ...
    sx-sw1,sy+sh1*(r-d)/r,z1, nml1b, su2,sv; ...
    sx,sy-sh0,z0,             nml0a, su1,sv; ...
    sx,sy-sh0*(r-d)/r,z1,     nml0a, su2,sv; ...
    sx,sy+sh0,z0,             nml0b, su1,sv; ...
    sx,sy+sh0*(r-d)/r,z1,     nml0b, su2,sv; ...
    sx+sw1,sy-sh1,z0,         nml2a, su1,sv; ...
    sx+sw1,sy-sh1*(r-d)/r,z1, nml2a, su2,sv; ...
    sx+sw1,sy+sh1,z0,         nml2b, su1,sv; ...
    sx+sw1,sy+sh1*(r-d)/r,z1, nml2b, su2,sv; ...
    sx+sw2,sy-sh2,z0,         -1,0,0,su1,sv; ...
    sx+sw2,sy-sh2*(r-d)/r,z1, -1,0,0,su2,sv; ...
    sx+sw2,sy+sh2,z0,         -1,0,0,su1,sv; ...
    sx+sw2,sy+sh2*(r-d)/r,z1, -1,0,0,su2,sv];
idx = [0,1,2; 1,3,2; 0,2,5; 0,5,4; 1,6,3; 3,6,7; 4,5,9; 4,9,8; 6,10,7; 7,10,11; 8,9,13; 8,13,12; 10,14,11; 11,14,15; 12,13,17; 12,17,16; 14,18,15; 15,18,19; 16,17,19; 16,19,18];

% rotation drum
nseg = 5;
range = pi*0.48;
yofs = sy;
zofs = lever.depth;
u0 = lever.axis.u(1);
u1 = lever.axis.u(2);
v0 = lever.axis.v(1);
v1 = lever.axis.v(2);
mvtx = [];
for i=0:nseg
    theta = -range/2 + i/nseg*range;
    ny =  sin(theta);
    nz = -cos(theta);
    u = u0 + (u1-u0)*i/nseg;
    y =  lever.axis.rad*ny + yofs;
    z =  lever.axis.rad*nz + zofs;
    mvtx = [mvtx; sx-sw2,y,z, 0,ny,nz, u,v0; ...
        sx+sw2,y,z, 0,ny,nz, u,v1];
end
midx = [];
for i=0:nseg-1
    ofs = i*2;
    midx = [midx; ofs,ofs+2,ofs+1; ofs+1,ofs+2,ofs+3];
end
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);

% indicator frame
x0 = ind.xcnt-ind.w/2;
x1 = x0+ind.w;
y0 = ind.ycnt-ind.h/2;
y1 = y0+ind.h;
z1 = 0;
z0 = z1 + ind.z0;
us = texcrd(59.5);
vs = texcrd(280);
mvtx = [x0,y0,z0, 1,0,0, us,vs; ...
    x0,y0,z1, 1,0,0, us,vs; ...
    x0,y1,z0, 1,0,0, us,vs; ...
    x0,y1,z1, 1,0,0, us,vs; ...
    x1,y1,z0, 0,1,0, us,vs; ...
    x1,y1,z1, 0,1,0, us,vs; ...
    x0,y1,z0, 0,1,0, us,vs; ...
    x0,y1,z1, 0,1,0, us,vs];
midx = [0,1,2; 1,3,2; 5,4,6; 5,6,7];
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);
sublist = struct('vtx',vtx, 'idx',idx, 'grpname','VC4_DEFAULT', 'name','');
meshlist = meshmerge(meshlist,sublist);

% indicator front
u0 = ind.u(1);
u1 = ind.u(2);
v0 = ind.v(1);
v1 = ind.v(2);
vtx = [x0,y0,z0, 0,0,-1, u0,v0; ...
    x1,y0,z0, 0,0,-1, u1,v0; ...
    x0,y1,z0, 0,0,-1, u0,v1; ...
    x1,y1,z0, 0,0,-1, u1,v1];
idx = [0,2,1; 1,2,3];
sublist = struct('vtx',vtx, 'idx',idx, 'grpname','VC4_LIT', 'name','NCONE_INDICATOR');
meshlist = meshmerge(meshlist,sublist);

% label
x0 = -this.slit.w2;
x1 = x0 - label.w;
ym = 0;
y0 = ym - label.h/2;
y1 = y0 + label.h;
z0 = g.label_ofs;
u0 = label.u1;
u1 = label.u0;
v0 = label.v0;
v1 = label.v1;
vtx = [x0,y0,z0, 0,0,-1, u0,v0; ...
    x1,y0,z0, 0,0,-1, u1,v0; ...
    x0,y1,z0, 0,0,-1, u0,v1; ...
    x1,y1,z0, 0,0,-1, u1,v1];
idx = [0,1,2; 1,3,2];

sublist = struct('vtx',vtx, 'idx',idx, 'grpname','LIT_LABELS', 'name','');
meshlist = meshmerge(meshlist, sublist);

p.ref = [0,0,lever.depth];
p.axis = [1,0,0];
p.mousearea = [-0.015, -sh0-0.01, 0; ...
                0.015, -sh0-0.01, 0; ...
               -0.015,  sh0+0.01, 0; ...
                0.015,  sh0+0.01, 0];
p.name = 'NCONELEVER';
prms{1} = p;

% transform into parent frame
meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);

end

% =========================================================================
% Undock lever
function [meshlist,prms] = make_undock_lever(this)
global g;
lever = this.lever;
slit  = this.slit;
label = this.label;

% lever
x0 = lever.x0;
x1 = x0 + lever.w;
X0 = x0 + lever.corner;
X1 = x1 - lever.corner;
y0 = lever.y0;
y1 = y0 + lever.h;
Y0 = y0 + lever.corner;
Y1 = y1 - lever.corner;
Ym = (y0+y1)/2;
Yn = Ym-0.01;
z0 = 0;
z1 = z0 + lever.top_tickness;
z2 = z0 + lever.bottom_thickness;
Z0 = z0 + lever.corner;
Z1 = z1 - lever.corner;
Z2 = z2 - lever.corner;
u0 = lever.u(1);
u1 = lever.u(2);
v0 = lever.v(1);
v1 = lever.v(2);
vtx = [X0,Y0,z0, 0,0,-1, u1,v1; ...
    X1,Y0,z0, 0,0,-1, u1,v0; ...
    X0,Y1,z0, 0,0,-1, u0,v1; ...
    X1,Y1,z0, 0,0,-1, u0,v0; ...
    x0,Y0,Z0, -1,0,0, u1,v1; ...
    x0,Y1,Z0, -1,0,0, u0,v1; ...
    x1,Y0,Z0, 1,0,0,  u1,v0; ...
    x1,Y1,Z0, 1,0,0,  u0,v0; ...
    X0,y1,Z0, 0,1,0,  u0,v1; ...
    X1,y1,Z0, 0,1,0,  u0,v0; ...
    X0,y0,Z0, 0,-1,0, u1,v1; ...
    X1,y0,Z0, 0,-1,0, u1,v0; ...
    X0,y1,Z1, 0,1,0,  u0,v1; ...
    X1,y1,Z1, 0,1,0,  u0,v0; ...
    X0,Y1,z1, 0,0,1,  u0,v1; ...
    X1,Y1,z1, 0,0,1,  u0,v0; ...
    X0,y0,Z2, 0,-1,0, u1,v1; ...
    X1,y0,Z2, 0,-1,0, u1,v0; ...
    X0,Y0,z2, 0,0,1,  u1,v1; ...
    X1,Y0,z2, 0,0,1,  u1,v0; ...
    x0,Y1,Z1, -1,0,0, u0,v1; ...
    x1,Y1,Z1, 1,0,0,  u0,v0; ...
    X0,Ym,z1, 0,0,1,  u0,v1; ...
    X1,Ym,z1, 0,0,1,  u0,v0; ...
    X0,Yn,z2, 0,0,1,  u0,v1; ...
    X1,Yn,z2, 0,0,1,  u0,v0; ...
    x0,Ym,Z1, -1,0,0, u0,v1; ...
    x1,Ym,Z1, 1,0,0,  u0,v0; ...

    x0,Y0,Z2, -1,0,0, u1,v1; ...
    x1,Y0,Z2, 1,0,0,  u1,v0; ...
    x0,Yn,Z2, -1,0,0, u1,v1; ...
    x1,Yn,Z2, 1,0,0,  u1,v0; ...
    x0,Y0,Z0, -1,0,0, u0,v1];
idx = [0,2,1; 1,2,3; 0,4,5; 0,5,2; 6,1,3; 6,3,7; 3,2,8; 3,8,9; 2,5,8; 7,3,9; 11,10,0; 11,0,1; 10,4,0; 11,1,6; ...
    9,8,12; 9,12,13; 13,12,14; 13,14,15; 17,16,10; 17,10,11; 19,18,16; 19,16,17; ...
    8,5,20; 8,20,12; 7,9,13; 7,13,21; 12,20,14; 21,13,15; ...
    15,14,22; 15,22,23; 18,19,25; 18,25,24; 24,25,23; 24,23,22; ...
    14,20,26; 14,26,22; 15,23,21; 21,23,27; 18,24,28; 24,30,28; 19,31,25; 19,29,31;
    22,30,24; 22,26,30; 23,25,27; 27,25,31; ...
    5,26,20; 5,32,26; 26,32,30; 30,32,28];

meshlist = struct('vtx',vtx, 'idx',idx, 'grpname','UNDOCK_LEVER', 'name','');

%label
x0 = label.x0;
x1 = x0 - label.w;
ym = label.ycnt;
y0 = ym - label.h/2;
y1 = y0 + label.h;
z0 = g.label_ofs;
u0 = label.u1;
u1 = label.u0;
v0 = label.v0;
v1 = label.v1;
vtx = [x0,y0,z0, 0,0,-1, u0,v0; ...
    x1,y0,z0, 0,0,-1, u1,v0; ...
    x0,y1,z0, 0,0,-1, u0,v1; ...
    x1,y1,z0, 0,0,-1, u1,v1];
idx = [0,1,2; 1,3,2];

sublist = struct('vtx',vtx, 'idx',idx, 'grpname','LIT_LABELS', 'name','');
meshlist = meshmerge(meshlist, sublist);

% well
x1 = -slit.w/2;
x2 = x1 + slit.w;
y0 = -slit.h/2;
y1 = y0 + slit.h;
Y1 = y1 - slit.depth;
z0 = 0;
z1 = z0 + slit.depth;
ud = texcrd(0.5);
ut = texcrd(30);
vd = texcrd(142);
c = 1/sqrt(2);
vtx = [x1,y0,z1, 0,0,-1, ud,vd; ...
    x2,y0,z1, 0,0,-1, ud,vd; ...
    x1,Y1,z1, 0,0,-1, ud,vd; ...
    x2,Y1,z1, 0,0,-1, ud,vd; ...
    x1,y1,z0, 0,-c,-c, ut,vd; ...
    x2,y1,z0, 0,-c,-c, ut,vd; ...
    x1,y0,z1, 0,1,0, ud,vd; ...
    x2,y0,z1, 0,1,0, ud,vd; ...
    x1,y0,z0, 0,1,0, ud,vd; ...
    x2,y0,z0, 0,1,0, ud,vd; ...

    x2,y0,z1, -1,0,0, ud,vd; ...
    x2,y0,z0, -1,0,0, ud,vd; ...
    x2,Y1,z1, -1,0,0, ud,vd; ...
    x2,y1,z0, -1,0,0, ud,vd];
idx = [0,2,1; 1,2,3; 2,4,3; 3,4,5; 6,7,8; 7,9,8; 10,12,11; 11,12,13];
sublist = struct('vtx',vtx, 'idx',idx, 'grpname','VC4_DEFAULT', 'name','');
meshlist = meshmerge(meshlist,sublist);

p.ref = [0, lever.y0 + lever.bottom_thickness/2, lever.bottom_thickness/2];
p.axis = [1,0,0];
p.mousearea = [x1,y0,z0; x2,y0,z0; x1,y1,z0; x2,y1,z0];
p.name = 'UNDOCKLEVER';
prms{1} = p;

% transform into parent frame
meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);

end

% =========================================================================
% Retro engine cover switch

function [meshlist,prms] = make_rcover_switch(this)
global g;
sw    = this.switch;
ind   = this.ind;
label = this.label;

% switch
[meshlist,prms] = switch1(sw.rad,sw.len,'RCOVER_SWITCH');

% indicator
w = ind.w;
h = ind.h;
x0 = -w/2;
x1 = x0+w;
y0 = label.y0-0.007;
y1 = y0+h;
z0 = g.label_ofs;
u0 = ind.u0;
u1 = ind.u1;
v0 = ind.v0;
v1 = ind.v1;
vtx = [x0,y0,z0, 0,0,-1, u0,v0; ...
       x1,y0,z0, 0,0,-1, u1,v0; ...
       x0,y1,z0, 0,0,-1, u0,v1; ...
       x1,y1,z0, 0,0,-1, u1,v1];
idx = [0,2,1; 1,2,3];
sublist = struct('vtx',vtx, 'idx',idx, 'grpname','VC4_LIT', 'name','RCOVER_INDICATOR');
meshlist = meshmerge(meshlist,sublist);

% label
x0 = label.x0;
x1 = x0 + label.w;
y0 = label.y0;
y1 = y0 + label.h;
z0 = g.label_ofs;
u0 = label.u0;
u1 = label.u1;
v0 = label.v0;
v1 = label.v1;
vtx = [x0,y0,z0, 0,0,-1, u0,v0; ...
    x1,y0,z0, 0,0,-1, u1,v0; ...
    x0,y1,z0, 0,0,-1, u0,v1; ...
    x1,y1,z0, 0,0,-1, u1,v1];
idx = [0,2,1; 1,2,3];
sublist = struct('vtx',vtx, 'idx',idx, 'grpname','LIT_LABELS', 'name','');
meshlist = meshmerge(meshlist, sublist);

meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);

end

% =========================================================================
% Escape ladder switch

function [meshlist,prms] = make_eladder_switch(this)
global g;
sw    = this.switch;
ind   = this.ind;
label = this.label;

% switch
[meshlist,prms] = switch1(sw.rad,sw.len,'ELADDER_SWITCH');

% indicator
w = ind.w;
h = ind.h;
x0 = -w/2;
x1 = x0+w;
y0 = label.y0-0.007;
y1 = y0+h;
z0 = g.label_ofs;
u0 = ind.u0;
u1 = ind.u1;
v0 = ind.v0;
v1 = ind.v1;
vtx = [x0,y0,z0, 0,0,-1, u0,v0; ...
       x1,y0,z0, 0,0,-1, u1,v0; ...
       x0,y1,z0, 0,0,-1, u0,v1; ...
       x1,y1,z0, 0,0,-1, u1,v1];
idx = [0,2,1; 1,2,3];
sublist = struct('vtx',vtx, 'idx',idx, 'grpname','VC4_LIT', 'name','ELADDER_INDICATOR');
meshlist = meshmerge(meshlist,sublist);

% label
x0 = label.x0;
x1 = x0 + label.w;
y0 = label.y0;
y1 = y0 + label.h;
z0 = g.label_ofs;
u0 = label.u0;
u1 = label.u1;
v0 = label.v0;
v1 = label.v1;
vtx = [x0,y0,z0, 0,0,-1, u0,v0; ...
    x1,y0,z0, 0,0,-1, u1,v0; ...
    x0,y1,z0, 0,0,-1, u0,v1; ...
    x1,y1,z0, 0,0,-1, u1,v1];
idx = [0,2,1; 1,2,3];
sublist = struct('vtx',vtx, 'idx',idx, 'grpname','LIT_LABELS', 'name','');
meshlist = meshmerge(meshlist, sublist);

meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);

end

% =========================================================================
% Dock seal indicator

function [meshlist,prms] = make_dockseal_indicator(this)
global g;
ind   = this.ind;
label = this.label;

% indicator
x0 = -ind.w/2;
x1 = x0 + ind.w;
y0 = -ind.h/2;
y1 = y0 + ind.h;
z0 = g.label_ofs;
u0 = ind.u0;
u1 = ind.u1;
v0 = ind.v0;
v1 = ind.v1;
vtx = [x0,y0,z0, 0,0,-1, u0,v0; ...
       x1,y0,z0, 0,0,-1, u1,v0; ...
       x0,y1,z0, 0,0,-1, u0,v1; ...
       x1,y1,z0, 0,0,-1, u1,v1];
idx = [0,2,1; 1,2,3];
meshlist = struct('vtx',vtx, 'idx',idx, 'grpname','VC4_LIT', 'name','SEAL_INDICATOR');

% label
x0 = label.x0;
x1 = x0 + label.w;
y0 = label.y0;
y1 = y0 + label.h;
z0 = g.label_ofs;
u0 = label.u0;
u1 = label.u1;
v0 = label.v0;
v1 = label.v1;
vtx = [x0,y0,z0, 0,0,-1, u0,v0; ...
    x1,y0,z0, 0,0,-1, u1,v0; ...
    x0,y1,z0, 0,0,-1, u0,v1; ...
    x1,y1,z0, 0,0,-1, u1,v1];
idx = [0,2,1; 1,2,3];
sublist = struct('vtx',vtx, 'idx',idx, 'grpname','LIT_LABELS', 'name','');
meshlist = meshmerge(meshlist,sublist);

prms = {};

meshlist = transform(this,meshlist);
prms = transform_prms(this,prms);

end

% =========================================================================
% Tilt all vertices around the x-axis
function vtx = tilt_x(vtx,refy,refz,angle)
R = [1,0,0;  0,cos(angle),-sin(angle); 0,sin(angle),cos(angle)];
d = [0,refy,refz];
for i=1:size(vtx,1)
    vtx(i,1:3) = (R*(vtx(i,1:3)-d)')' + d;
    if size(vtx,2) > 3
        vtx(i,4:6) = (R*vtx(i,4:6)')';
    end
end
end

% =========================================================================
% Move vertices by distance 'dist' along the normals
function vtx = growmesh(vtx,dist)
for i=1:size(vtx,1)
    vtx(i,1:3) = vtx(i,1:3) + vtx(i,4:6)*dist;
end
end

% =========================================================================
% convert a pixel index into a texture coordinate
function t = texcrd(pix,tsize)
global g;
if nargin < 2
    tsize = g.texsize;
end
t = pix/tsize;
end

% =========================================================================
function write_materials(fid,mat)
nmat = length(mat);
fprintf (fid, 'MATERIALS %d\n', nmat);
for i=1:nmat
    fprintf (fid, [mat(i).name '\n']);
end
for i=1:nmat
    fprintf (fid, ['MATERIAL ' mat(i).name '\n']);
    fprintf (fid, '%0.4f %0.4f %0.4f %0.4f\n', mat(i).diffuse(1), mat(i).diffuse(2), mat(i).diffuse(3), mat(i).diffuse(4));
    fprintf (fid, '%0.4f %0.4f %0.4f %0.4f\n', mat(i).ambient(1), mat(i).ambient(2), mat(i).ambient(3), mat(i).ambient(4));
    if length(mat(i).specular) == 4
        fprintf (fid, '%0.4f %0.4f %0.4f %0.4f\n', mat(i).specular(1), mat(i).specular(2), mat(i).specular(3), mat(i).specular(4));
    else
        fprintf (fid, '%0.4f %0.4f %0.4f %0.4f %0.4f\n', mat(i).specular(1), mat(i).specular(2), mat(i).specular(3), mat(i).specular(4), mat(i).specular(5));
    end
    fprintf (fid, '%0.4f %0.4f %0.4f %0.4f\n', mat(i).emissive(1), mat(i).emissive(2), mat(i).emissive(3), mat(i).emissive(4));
end
end

% =========================================================================
function write_textures(fid,tex)
ntex = length(tex);
fprintf (fid, 'TEXTURES %d\n', ntex);
for i=1:ntex
    fprintf (fid, [tex(i).name '\n']);
end
end

% =========================================================================
% =========================================================================
% Functions for outputting data to dg_anim.h

% =========================================================================
function writeh_prms(fid,prms)
for i=1:length(prms)
    if ~isfield(prms{i},'name')
        error('parameter record requires field ''name''');
    end
    prefix = ['VC_' prms{i}.name];
    if isfield(prms{i},'mousearea')
        writeh_vector(fid, [prefix '_mousearea'], prms{i}.mousearea, [prms{i}.name ' (VC): mouse catch area']);
    end
    if isfield(prms{i},'mousepoint')
        writeh_vector(fid, [prefix '_mousepoint'], prms{i}.mousepoint, [prms{i}.name ' (VC): mouse catch point']);
    end
    if isfield(prms{i},'mouserad')
        writeh_scalar(fid, [prefix '_mouserad'], prms{i}.mouserad, [prms{i}.name ' (VC): mouse catch radius']);
    end
    if isfield(prms{i},'ref')
        writeh_vector(fid, [prefix '_ref'], prms{i}.ref, [prms{i}.name ' (VC): rotation reference']);
    end
    if isfield(prms{i},'axis')
        writeh_vector(fid, [prefix '_axis'], prms{i}.axis, [prms{i}.name ' (VC): rotation axis']);
    end
end
end

% =========================================================================
function writeh_string(fid,string)
fprintf (fid, [string, '\n']);
end

% =========================================================================
function writeh_vector(fid, varname, vec, comment)
if nargin >= 4
    fprintf (fid, ['// ' comment '\n']);
end
fprintf (fid, ['const VECTOR3 ' varname]);
if size(vec,1) > 1
    fprintf (fid, '[%d] = {', size(vec,1));
else
    fprintf (fid, ' = ');
end
for i=1:size(vec,1)
    fprintf (fid, '{%0.5f,%0.5f,%0.5f}', vec(i,1), vec(i,2), vec(i,3));
    if i < size(vec,1)
        fprintf (fid, ',');
    end
end
if size(vec,1) > 1
    fprintf (fid, '};\n\n');
else
    fprintf (fid, ';\n\n');
end
end

% =========================================================================
function writeh_scalar(fid, varname, val, comment)
val = reshape (val, [], 1);
if nargin >= 4
    fprintf (fid, ['// ' comment '\n']);
end
fprintf (fid, ['const double ' varname]);
if length(val) > 1
    fprintf (fid, '[%d] = {', length(val));
else
    fprintf (fid, ' = ');
end
for i=1:length(val)
    fprintf (fid, '%05f', val(i));
    if i < length(val)
        fprintf (fid, ',');
    end
end
if length(val) > 1
    fprintf (fid, '};\n\n');
else
    fprintf (fid, ';\n\n');
end
end

% =========================================================================
function writeh_int(fid, varname, val, comment)
val = reshape (val, [], 1);
if nargin >= 4
    fprintf (fid, ['// ' comment '\n']);
end
fprintf (fid, ['const int ' varname]);
if length(val) > 1
    fprintf (fid, '[%d] = {', length(val));
else
    fprintf (fid, ' = ');
end
for i=1:length(val)
    fprintf (fid, '%d', val(i));
    if i < length(val)
        fprintf (fid, ',');
    end
end
if length(val) > 1
    fprintf (fid, '};\n\n');
else
    fprintf (fid, ';\n\n');
end
end
