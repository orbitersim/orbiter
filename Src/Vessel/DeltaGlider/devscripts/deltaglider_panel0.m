% Create the mesh for the Deltaglider 2D main panel

function deltaglider_panel0

disp('**** Generating DG cockpit mesh for 2D main panel');

% global parameters
global g;
g.tex0.w = 2048; % texture 0 width (main panel texture)
g.tex0.h = 1024; % texture 0 height (main panel texture)
g.tex1.w =  512; % texture 1 width (instrument texture)
g.tex1.h = 1024; % texture 1 height (instrument texture)
g.nml = [0,0,-1];

%path = 'c:\source\orbiter\meshes\DG\';
path = 'C:\Source\Orbiter\Meshes\DG\';
fid = fopen([path 'deltaglider_p0_part.msh'],'wt');

switch1.w  =   26;
switch1.h  =   52;
switch1.tu0 =  53;
switch1.tv0 =  337;

% Main panel parameters
panel0.w = 1280;
panel0.h = 572;
panel0.x0 = 0;
panel0.y0 = 0;
panel0.tx0 = 0;
panel0.ty0 = g.tex0.h-panel0.h;

% Parameters for artificial horizon display
hor.cnt.x = 641;
hor.cnt.y = 150;

% Parameters for HSI indicator display
hsi.cnt.x = 641;
hsi.cnt.y = 473;

% Parameters for propellant status display
psd.x0 = 827.5;
psd.y0 = 395.5+167.0;

% Parameters for RCS mode dial
rcs.x0 = 100.5;
rcs.y0 = 69.5;
rcs.w  = 39.0;
rcs.h  = 43.0;
rcs.tx0 = 1160.5;
rcs.ty0 = g.tex0.h-615.5;

% Parameters for AF control dial
afc.x0 = 23.5;
afc.y0 = 69.5;
afc.w  = 39.0;
afc.h  = 43.0;
afc.tx0 = 1160.5;
afc.ty0 = g.tex0.h-615.5;

% Parameters for HUD mode buttons
hud.x0 = 46.5+[0:2]*29;
hud.y0 = 19.5;
hud.w  = 15;
hud.h  = 4;
hud.tx0 = 1143.5;
hud.ty0 = g.tex0.h-604.5;

% Parameters for nav mode buttons
nav.w  = 37.0;
nav.h  = 24.0;
nav.tdx = 37.0;
nav.tdy = 24.0;
nav.x0  = 1124 + [0,3*nav.w,nav.w,nav.w,2*nav.w,2*nav.w];
nav.y0  =   62 + [nav.h,nav.h,0,nav.h,0,nav.h];
nav.tx0 =  154 + [0,1,2,3,4,5]*nav.tdx;
nav.ty0 = g.tex0.h-597;

% Parameters for main engine throttles
thm.x0  = 4.5+[0,1]*26;
thm.y0  = 241.5;
thm.w   = 24.0;
thm.h   = 18.0;
thm.tx0 = 1135.0;
thm.ty0 = g.tex0.h-599.0;

% Parameters for hover engine throttles
thh.x0  = 4.5;
thh.y0  = 428.5;
thh.w   = 50;
thh.tx0 = 1135.0;
thh.ty0 = g.tex0.h-599.0;
thh.tdx = 24.0;
thh.tdy = 18.0;

% Parameters for scram engine throttles
ths.x0  = 4.5+[0,1]*26;
ths.y0  = 541.5;
ths.w   = 24.0;
ths.h   = 18.0;
ths.tx0 = 1135.0;
ths.ty0 = g.tex0.h-599.0;

% Parameters for elevator trim indicator
etrim.x0  =   87.5;
etrim.y0  =  324.5;
etrim.w   =   21.0;
etrim.h   =    7.0;
etrim.tx0 = 1138.0;
etrim.ty0 = g.tex0.h-580.0;

% Parameters for airbrake indicator
abrake.x0  =  138.5;
abrake.y0  =  325.5;
abrake.w   =   21.0;
abrake.h   =    7.0;
abrake.tx0 = 1138.0;
abrake.ty0 = g.tex0.h-580.0;

% Parameters for gear lever
glever.x0  =   91.5;
glever.y0  =  413.0;
glever.w   =   33.0;
glever.h   =  176.0;
glever.tx0 = 1104.0;
glever.ty0 = g.tex0.h-689.5;

% Parameters for gear indicator
gearind.x0 = glever.x0+11.5;
gearind.y0 = glever.y0-33;

% Parameters for undock button
undock.x0  = 1149.0;
undock.y0  =  368.0;
undock.w   =   46.0;
undock.h   =  104.0;
undock.tx0 = 1761.0;
undock.ty0 = g.tex0.h-356.0;

% Parameters for dock seal indicator
dockseal.ind.w = 24;
dockseal.ind.h = 6;
dockseal.ind.x0 = 1160.0;
dockseal.ind.y0 = 324.0;

% Parameters for wheel brake levers
wbrake.x0  = 1221.0;
wbrake.x1  = 1248.0;
wbrake.y0  =  493.0;
wbrake.w   =   25.0;
wbrake.h   =   77.0;
wbrake.tx0 =    0.0;
wbrake.tx1 =   25.0;
wbrake.ty0 = g.tex0.h-650.0;

% Parameters for button array
btnarray.nbutton = 8;
btnarray.x0 = 856.0;
btnarray.y0 = 415.0;
btnarray.w  =  32.0;
btnarray.tw =  25.0;
btnarray.th =  38.0;
btnarray.tx1 = 964.0;
btnarray.tx0 = btnarray.tx1 + btnarray.tw;
btnarray.ty0 = g.tex0.h-611.0;

% Parameters for main gimbal control dial
gimbaldial.x0 = 203.5;
gimbaldial.y0 = 426.5;
gimbaldial.w  = 39.0;
gimbaldial.h  = 43.0;
gimbaldial.tx0 = 1160.5;
gimbaldial.ty0 = g.tex0.h-615.5;

% Parameters for main gimbal display
gimbaldsp.x0  = 197.5;
gimbaldsp.x1  = gimbaldsp.x0+42;
gimbaldsp.y0  = 515.5;
gimbaldsp.w   = 10;
gimbaldsp.h   = 10;
gimbaldsp.tx0 = 1022.5;
gimbaldsp.ty0 = g.tex0.h-666.5;
gimbaldsp.ty1 = g.tex0.h-676.5;

% Parameters for gimbal pitch controls
pgimbal.x0 = 285.5;
pgimbal.x1 = pgimbal.x0+19;
pgimbal.y0 = 434.5;
pgimbal.w  = 15;
pgimbal.h  = 44;
pgimbal.tx0 = 1053.5;
pgimbal.ty0 = g.tex0.h-616.5;

% Parameters for gimbal yaw controls
ygimbal.x0 = 280.5;
ygimbal.y0 = 503.5;
ygimbal.y1 = ygimbal.y0+19;
ygimbal.w  = 15;
ygimbal.h  = 44;
ygimbal.tx0 = 1053.5;
ygimbal.ty0 = g.tex0.h-616.5;

% Parameters for hover control dial
hoverdial.x0 = 356.5;
hoverdial.y0 = 426.5;
hoverdial.w  = 39.0;
hoverdial.h  = 43.0;
hoverdial.tx0 = 1160.5;
hoverdial.ty0 = g.tex0.h-615.5;

% Parameters for hover attitude display
hoverdsp.x0  = 371.5;
hoverdsp.y0  = 515.5;
hoverdsp.w   = 10;
hoverdsp.h   = 10;
hoverdsp.tx0 = 1022.5;
hoverdsp.ty0 = g.tex0.h-666.5;
hoverdsp.ty1 = g.tex0.h-676.5;

% Parameters for hover pitch control
phover.x0 = 436.5;
phover.y0 = 434.5;
phover.w  = 15;
phover.h  = 44;
phover.tx0 = 1053.5;
phover.ty0 = g.tex0.h-616.5;

% Parameters for hover roll control
rhover.x0 = 423.5;
rhover.y0 = 513.5;
rhover.w  = 15;
rhover.h  = 44;
rhover.tx0 = 1053.5;
rhover.ty0 = g.tex0.h-616.5;

% Parameters for hover altitude/vspd set switch
hoverholdset.x0 = 484.5;
hoverholdset.y0 = 491.5;
hoverholdset.w = 15;
hoverholdset.h = 44;
hoverholdset.tx0 = 1053.5;
hoverholdset.ty0 = g.tex0.h-616.5;

% Parameters for hover hold button
hoverholdbtn.x0 = 491;
hoverholdbtn.y0 = 415;
hoverholdbtn.w = 42;
hoverholdbtn.h = 30;
hoverholdbtn.tx0 = 51;
hoverholdbtn.ty0 = g.tex0.h-633;

% Parameters for hover alt/vspd selector buttons
hoverholdsel.x0 = 482;
hoverholdsel.y0 = 520;
hoverholdsel.w = 59;
hoverholdsel.h = 30;
hoverholdsel.tx0 = 94;
hoverholdsel.ty0 = g.tex0.h-633;

% Parameters for retro door switch
retrodoor = switch1;
retrodoor.x0 = 1129;
retrodoor.y0 =  496;
retrodoor.ind.w = 14;
retrodoor.ind.h = 6;
retrodoor.ind.x0 = retrodoor.x0+retrodoor.w/2-retrodoor.ind.w/2;
retrodoor.ind.y0 = retrodoor.y0+retrodoor.h+6;

% Parameters for nosecone ladder switch
nconeladder = switch1;
nconeladder.x0 = 1171;
nconeladder.y0 =  496;
nconeladder.ind.w = 14;
nconeladder.ind.h = 6;
nconeladder.ind.x0 = nconeladder.x0+nconeladder.w/2-nconeladder.ind.w/2;
nconeladder.ind.y0 = nconeladder.y0+nconeladder.h+6;

% Parameters for MWS button
mws.x0  = 1071.0;
mws.y0  =    4.0;
mws.w   =   29.0;
mws.h   =   30.0;
mws.tx0 =  991.0;
mws.ty0 = g.tex0.h-643.0;

% Parameters for angular rate indicators
angveldisp.x0 = 1135;
angveldisp.y0 =  144;
angveldisp.xcnt = [22,67,107];
angveldisp.ycnt = [31,78,125];
angveldisp.rad = 20;
angveldisp.u = 1735/2048;
angveldisp.v =  839/1024;

% Parameters for AAP interface
aap.x0 = 90;
aap.y0 = 149;

% Generate instruments below main panel ----------------------------------

% Generate the artificial horizon display
[vtx,idx] = horizon('2D',hor);

% Generate the horizontal situation indicator display
[mvtx,midx] = hsi_disp('2D',hsi);
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);

% Add more instruments ...

% Write group of instruments rendered below main panel
write_group (fid, vtx,idx,0,2,'INSTRUMENTS_BELOW');

% main panel -------------------------------------------------------------

[vtx,idx] = make_mainpanel(panel0);
write_group (fid, vtx,idx,0,1,'MAINPANEL');

% Generate instruments above main panel ----------------------------------

% Generate propellant status display
[vtx,idx] = fuel_disp('2D',psd);
write_group (fid, vtx,idx,0,2,'FUEL_DISP');

% RCS mode selector
[vtx,idx] = make_rcsmode_switch(rcs);

% AF control selector
[mvtx,midx] = make_afcmode_switch(afc);
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);

% HUD mode buttons
[mvtx,midx] = make_hud_buttons(hud);
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);

% Nav mode buttons
[mvtx,midx] = make_nav_buttons(nav);
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);

% nose cone ladder switch
[mvtx,midx] = make_switch1(nconeladder);
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);

% Main engine throttles
[mvtx,midx] = make_main_throttles(thm);
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);

% Hover engine throttles
[mvtx,midx] = make_hover_throttles(thh);
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);

% Elevator trim indicator
[mvtx,midx] = make_etrim_indicator(etrim);
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);

% Airbrake indicator
[mvtx,midx] = make_abrake_indicator(abrake);
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);

% Gear lever
[mvtx,midx] = make_gearlever(glever);
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);

% Gear indicator
[mvtx,midx] = make_gearindicator(gearind);
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);

% Nose cone lever
[mvtx,midx] = make_noseconelever();
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);

% Nose cone indicator
[mvtx,midx] = make_noseconeindicator();
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);

% Undock button
[mvtx,midx] = make_undockbutton(undock);
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);

% Wheel brake levers
[mvtx,midx] = make_wbrakelever(wbrake);
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);

% main gimbal mode selector
[mvtx,midx] = make_gimbalmode_switch(gimbaldial);
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);

% main gimbal indicator
[mvtx,midx] = make_gimbal_indicator(gimbaldsp);
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);

% gimbal pitch controls
[mvtx,midx] = make_pgimbal_ctrl(pgimbal);
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);

% gimbal yaw controls
[mvtx,midx] = make_ygimbal_ctrl(ygimbal);
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);

% hover mode selector
[mvtx,midx] = make_hovermode_switch(hoverdial);
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);

% hover attitude indicator
[mvtx,midx] = make_hover_indicator(hoverdsp);
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);

% hover pitch control
[mvtx,midx] = make_phover_ctrl(phover);
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);

% hover roll control
[mvtx,midx] = make_rhover_ctrl(rhover);
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);

% hover hold alt/vspd set switch
[mvtx,midx] = make_hoverholdset_ctrl(hoverholdset);
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);

[mvtx,midx] = make_hoverholdbtn(hoverholdbtn);
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);

[mvtx,midx] = make_hoverholdsel(hoverholdsel);
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);

[mvtx,midx] = make_switch1(retrodoor);
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);

[mvtx,midx] = make_mws(mws);
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);

[mvtx,midx] = make_indicator(retrodoor.ind);
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);

[mvtx,midx] = make_indicator(nconeladder.ind);
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);

[mvtx,midx] = make_indicator(dockseal.ind);
[vtx,idx] = add_to_group(vtx,idx,mvtx,midx);

% Add more instruments ...

% Write group of instruments rendered above main panel
write_group (fid, vtx,idx,0,1,'INSTRUMENTS_ABOVE');

% Angular rate indicators
[vtx,idx] = make_angveldisp(angveldisp);
write_group (fid, vtx, idx, 0, 1, 'ANGVELDISP');

% Atmospheric autopilot interface
[vtx,idx] = make_aap(aap);
write_group (fid, vtx, idx, 0, 1, 'AAP');

% Scram engine throttles
[vtx,idx] = make_scram_throttles(ths);

% Write scram-only mesh elements

write_group (fid, vtx,idx,0,1,'SCRAM_INSTRUMENTS');

fclose(fid);

end

% =========================================================================
% main panel background
function [vtx,idx] = make_mainpanel(prm)
    disp('Computing main panel for 2D panel');
    global g;
    vtx = [prm.x0,prm.y0,0,             g.nml, prm.tx0/g.tex0.w, prm.ty0/g.tex0.h; ...
           prm.x0+prm.w,prm.y0,0,       g.nml, (prm.tx0+prm.w)/g.tex0.w, prm.ty0/g.tex0.h; ...
           prm.x0+prm.w,prm.y0+prm.h,0, g.nml, (prm.tx0+prm.w)/g.tex0.w, (prm.ty0+prm.h)/g.tex0.h; ...
           prm.x0,prm.y0+prm.h,0,       g.nml, prm.tx0/g.tex0.w, (prm.ty0+prm.h)/g.tex0.h];
    idx = [0,1,2; 2,3,0];
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

% =========================================================================
function [vtx,idx] = make_indicator(prm)
    global g;
    u = 1138/g.tex0.w;
    v = 412/g.tex0.h;
    vtx = [prm.x0,      prm.y0,      0, g.nml, u,v; ...
           prm.x0+prm.w,prm.y0,      0, g.nml, u,v; ...
           prm.x0,      prm.y0+prm.h,0, g.nml, u,v; ...
           prm.x0+prm.w,prm.y0+prm.h,0, g.nml, u,v];
    idx = [0,1,2; 1,3,2];       
end

% =========================================================================
% RCS mode selector
function [vtx,idx] = make_rcsmode_switch(prm)
    disp('Computing RCS mode selector for 2D panel');
    global g;
    vtx = [prm.x0,prm.y0,0,             g.nml, prm.tx0/g.tex0.w, prm.ty0/g.tex0.h; ...
           prm.x0+prm.w,prm.y0,0,       g.nml, (prm.tx0+prm.w)/g.tex0.w, prm.ty0/g.tex0.h; ...
           prm.x0,prm.y0+prm.h,0,       g.nml, prm.tx0/g.tex0.w, (prm.ty0+prm.h)/g.tex0.h; ...
           prm.x0+prm.w,prm.y0+prm.h,0, g.nml, (prm.tx0+prm.w)/g.tex0.w, (prm.ty0+prm.h)/g.tex0.h];
    idx = [0,1,2; 3,2,1];
end

% =========================================================================
% AF control selector
function [vtx,idx] = make_afcmode_switch(prm)
    disp('Computing AF control selector for 2D panel');
    global g;
    vtx = [prm.x0,prm.y0,0,             g.nml, prm.tx0/g.tex0.w, prm.ty0/g.tex0.h; ...
           prm.x0+prm.w,prm.y0,0,       g.nml, (prm.tx0+prm.w)/g.tex0.w, prm.ty0/g.tex0.h; ...
           prm.x0,prm.y0+prm.h,0,       g.nml, prm.tx0/g.tex0.w, (prm.ty0+prm.h)/g.tex0.h; ...
           prm.x0+prm.w,prm.y0+prm.h,0, g.nml, (prm.tx0+prm.w)/g.tex0.w, (prm.ty0+prm.h)/g.tex0.h];
    idx = [0,1,2; 3,2,1];
end

% =========================================================================
% main gimbal control selector
function [vtx,idx] = make_gimbalmode_switch(prm)
    disp('Computing main gimbal control selector for 2D panel');
    global g;
    vtx = [prm.x0,prm.y0,0,             g.nml, prm.tx0/g.tex0.w, prm.ty0/g.tex0.h; ...
           prm.x0+prm.w,prm.y0,0,       g.nml, (prm.tx0+prm.w)/g.tex0.w, prm.ty0/g.tex0.h; ...
           prm.x0,prm.y0+prm.h,0,       g.nml, prm.tx0/g.tex0.w, (prm.ty0+prm.h)/g.tex0.h; ...
           prm.x0+prm.w,prm.y0+prm.h,0, g.nml, (prm.tx0+prm.w)/g.tex0.w, (prm.ty0+prm.h)/g.tex0.h];
    idx = [0,1,2; 3,2,1];
end

% =========================================================================
% main gimbal indicator
function [vtx,idx] = make_gimbal_indicator(prm)
    disp('Computing main gimbal indicator for 2D panel');
    global g;
    vtx = [prm.x0,prm.y0,0,             g.nml, prm.tx0/g.tex0.w, prm.ty1/g.tex0.h; ...
           prm.x0+prm.w,prm.y0,0,       g.nml, (prm.tx0+prm.w)/g.tex0.w, prm.ty1/g.tex0.h; ...
           prm.x0,prm.y0+prm.h,0,       g.nml, prm.tx0/g.tex0.w, (prm.ty1+prm.h)/g.tex0.h; ...
           prm.x0+prm.w,prm.y0+prm.h,0, g.nml, (prm.tx0+prm.w)/g.tex0.w, (prm.ty1+prm.h)/g.tex0.h; ...
           prm.x1,prm.y0,0,             g.nml, prm.tx0/g.tex0.w, prm.ty1/g.tex0.h; ...
           prm.x1+prm.w,prm.y0,0,       g.nml, (prm.tx0+prm.w)/g.tex0.w, prm.ty1/g.tex0.h; ...
           prm.x1,prm.y0+prm.h,0,       g.nml, prm.tx0/g.tex0.w, (prm.ty1+prm.h)/g.tex0.h; ...
           prm.x1+prm.w,prm.y0+prm.h,0, g.nml, (prm.tx0+prm.w)/g.tex0.w, (prm.ty1+prm.h)/g.tex0.h; ...
       
           prm.x0,prm.y0,0,             g.nml, prm.tx0/g.tex0.w, prm.ty0/g.tex0.h; ...
           prm.x0+prm.w,prm.y0,0,       g.nml, (prm.tx0+prm.w)/g.tex0.w, prm.ty0/g.tex0.h; ...
           prm.x0,prm.y0+prm.h,0,       g.nml, prm.tx0/g.tex0.w, (prm.ty0+prm.h)/g.tex0.h; ...
           prm.x0+prm.w,prm.y0+prm.h,0, g.nml, (prm.tx0+prm.w)/g.tex0.w, (prm.ty0+prm.h)/g.tex0.h; ...
           prm.x1,prm.y0,0,             g.nml, prm.tx0/g.tex0.w, prm.ty0/g.tex0.h; ...
           prm.x1+prm.w,prm.y0,0,       g.nml, (prm.tx0+prm.w)/g.tex0.w, prm.ty0/g.tex0.h; ...
           prm.x1,prm.y0+prm.h,0,       g.nml, prm.tx0/g.tex0.w, (prm.ty0+prm.h)/g.tex0.h; ...
           prm.x1+prm.w,prm.y0+prm.h,0, g.nml, (prm.tx0+prm.w)/g.tex0.w, (prm.ty0+prm.h)/g.tex0.h];
    idx = [0,1,2; 3,2,1; 4,5,6; 7,6,5; ...
           8,9,10; 11,10,9; 12,13,14; 15,14,13];
end

% =========================================================================
% hover attitude indicator
function [vtx,idx] = make_hover_indicator(prm)
    disp('Computing hover indicator for 2D panel');
    global g;
    vtx = [prm.x0,prm.y0,0,             g.nml, prm.tx0/g.tex0.w, prm.ty1/g.tex0.h; ...
           prm.x0+prm.w,prm.y0,0,       g.nml, (prm.tx0+prm.w)/g.tex0.w, prm.ty1/g.tex0.h; ...
           prm.x0,prm.y0+prm.h,0,       g.nml, prm.tx0/g.tex0.w, (prm.ty1+prm.h)/g.tex0.h; ...
           prm.x0+prm.w,prm.y0+prm.h,0, g.nml, (prm.tx0+prm.w)/g.tex0.w, (prm.ty1+prm.h)/g.tex0.h; ...
           
           prm.x0,prm.y0,0,             g.nml, prm.tx0/g.tex0.w, prm.ty0/g.tex0.h; ...
           prm.x0+prm.w,prm.y0,0,       g.nml, (prm.tx0+prm.w)/g.tex0.w, prm.ty0/g.tex0.h; ...
           prm.x0,prm.y0+prm.h,0,       g.nml, prm.tx0/g.tex0.w, (prm.ty0+prm.h)/g.tex0.h; ...
           prm.x0+prm.w,prm.y0+prm.h,0, g.nml, (prm.tx0+prm.w)/g.tex0.w, (prm.ty0+prm.h)/g.tex0.h];
    idx = [0,1,2; 3,2,1; 4,5,6; 7,6,5];       
end

% =========================================================================
% gimbal pitch controls
function [vtx,idx] = make_pgimbal_ctrl(prm)
    disp('Computing gimbal pitch controls');
    global g;
    vtx = [prm.x0,      prm.y0,      0, g.nml, prm.tx0/g.tex0.w,        prm.ty0/g.tex0.h; ...
           prm.x0+prm.w,prm.y0,      0, g.nml, (prm.tx0+prm.w)/g.tex0.w,prm.ty0/g.tex0.h; ...
           prm.x0,      prm.y0+prm.h,0, g.nml, prm.tx0/g.tex0.w,        (prm.ty0+prm.h)/g.tex0.h; ...
           prm.x0+prm.w,prm.y0+prm.h,0, g.nml, (prm.tx0+prm.w)/g.tex0.w,(prm.ty0+prm.h)/g.tex0.h; ...
           prm.x1,      prm.y0,      0, g.nml, prm.tx0/g.tex0.w,        prm.ty0/g.tex0.h; ...
           prm.x1+prm.w,prm.y0,      0, g.nml, (prm.tx0+prm.w)/g.tex0.w,prm.ty0/g.tex0.h; ...
           prm.x1,      prm.y0+prm.h,0, g.nml, prm.tx0/g.tex0.w,        (prm.ty0+prm.h)/g.tex0.h; ...
           prm.x1+prm.w,prm.y0+prm.h,0, g.nml, (prm.tx0+prm.w)/g.tex0.w,(prm.ty0+prm.h)/g.tex0.h];
    idx = [0,1,2; 3,2,1; ...
           4,5,6; 7,6,5];
end

% =========================================================================
% gimbal yaw controls
function [vtx,idx] = make_ygimbal_ctrl(prm)
    disp('Computing gimbal yaw controls');
    global g;
    vtx = [prm.x0+prm.h,prm.y0,      0, g.nml, prm.tx0/g.tex0.w,        prm.ty0/g.tex0.h; ...
           prm.x0+prm.h,prm.y0+prm.w,0, g.nml, (prm.tx0+prm.w)/g.tex0.w,prm.ty0/g.tex0.h; ...
           prm.x0,      prm.y0,      0, g.nml, prm.tx0/g.tex0.w,        (prm.ty0+prm.h)/g.tex0.h; ...
           prm.x0,      prm.y0+prm.w,0, g.nml, (prm.tx0+prm.w)/g.tex0.w,(prm.ty0+prm.h)/g.tex0.h; ...
           prm.x0+prm.h,prm.y1,      0, g.nml, prm.tx0/g.tex0.w,        prm.ty0/g.tex0.h; ...
           prm.x0+prm.h,prm.y1+prm.w,0, g.nml, (prm.tx0+prm.w)/g.tex0.w,prm.ty0/g.tex0.h; ...
           prm.x0,      prm.y1,      0, g.nml, prm.tx0/g.tex0.w,        (prm.ty0+prm.h)/g.tex0.h; ...
           prm.x0,      prm.y1+prm.w,0, g.nml, (prm.tx0+prm.w)/g.tex0.w,(prm.ty0+prm.h)/g.tex0.h];
    idx = [0,1,2; 3,2,1; ...
           4,5,6; 7,6,5];
end

% =========================================================================
% hover control selector
function [vtx,idx] = make_hovermode_switch(prm)
    disp('Computing hover control selector for 2D panel');
    global g;
    vtx = [prm.x0,prm.y0,0,             g.nml, prm.tx0/g.tex0.w, prm.ty0/g.tex0.h; ...
           prm.x0+prm.w,prm.y0,0,       g.nml, (prm.tx0+prm.w)/g.tex0.w, prm.ty0/g.tex0.h; ...
           prm.x0,prm.y0+prm.h,0,       g.nml, prm.tx0/g.tex0.w, (prm.ty0+prm.h)/g.tex0.h; ...
           prm.x0+prm.w,prm.y0+prm.h,0, g.nml, (prm.tx0+prm.w)/g.tex0.w, (prm.ty0+prm.h)/g.tex0.h];
    idx = [0,1,2; 3,2,1];
end

% =========================================================================
% hover pitch control
function [vtx,idx] = make_phover_ctrl(prm)
    disp('Computing hover pitch control');
    global g;
    vtx = [prm.x0,      prm.y0,      0, g.nml, prm.tx0/g.tex0.w,        prm.ty0/g.tex0.h; ...
           prm.x0+prm.w,prm.y0,      0, g.nml, (prm.tx0+prm.w)/g.tex0.w,prm.ty0/g.tex0.h; ...
           prm.x0,      prm.y0+prm.h,0, g.nml, prm.tx0/g.tex0.w,        (prm.ty0+prm.h)/g.tex0.h; ...
           prm.x0+prm.w,prm.y0+prm.h,0, g.nml, (prm.tx0+prm.w)/g.tex0.w,(prm.ty0+prm.h)/g.tex0.h];
    idx = [0,1,2; 3,2,1];
end

% =========================================================================
% hover roll control
function [vtx,idx] = make_rhover_ctrl(prm)
    disp('Computing hover roll control');
    global g;
    vtx = [prm.x0+prm.h,prm.y0,      0, g.nml, prm.tx0/g.tex0.w,        prm.ty0/g.tex0.h; ...
           prm.x0+prm.h,prm.y0+prm.w,0, g.nml, (prm.tx0+prm.w)/g.tex0.w,prm.ty0/g.tex0.h; ...
           prm.x0,      prm.y0,      0, g.nml, prm.tx0/g.tex0.w,        (prm.ty0+prm.h)/g.tex0.h; ...
           prm.x0,      prm.y0+prm.w,0, g.nml, (prm.tx0+prm.w)/g.tex0.w,(prm.ty0+prm.h)/g.tex0.h];
    idx = [0,1,2; 3,2,1];
end

% =========================================================================
% hover hold alt/vspd set switch
function [vtx,idx] = make_hoverholdset_ctrl(prm)
    disp('Computing hover hold switch');
    global g;
    vtx = [prm.x0+prm.h,prm.y0,      0, g.nml, prm.tx0/g.tex0.w,        prm.ty0/g.tex0.h; ...
           prm.x0+prm.h,prm.y0+prm.w,0, g.nml, (prm.tx0+prm.w)/g.tex0.w,prm.ty0/g.tex0.h; ...
           prm.x0,      prm.y0,      0, g.nml, prm.tx0/g.tex0.w,        (prm.ty0+prm.h)/g.tex0.h; ...
           prm.x0,      prm.y0+prm.w,0, g.nml, (prm.tx0+prm.w)/g.tex0.w,(prm.ty0+prm.h)/g.tex0.h];
    idx = [0,1,2; 3,2,1];
end

% =========================================================================
% hover hold activate button
function [vtx,idx] = make_hoverholdbtn(prm)
    disp('Computing hover hold activation button');
    global g;
    vtx = [prm.x0,      prm.y0,      0, g.nml, prm.tx0/g.tex0.w,        prm.ty0/g.tex0.h; ...
           prm.x0+prm.w,prm.y0,      0, g.nml, (prm.tx0+prm.w)/g.tex0.w,prm.ty0/g.tex0.h; ...
           prm.x0,      prm.y0+prm.h,0, g.nml, prm.tx0/g.tex0.w,        (prm.ty0+prm.h)/g.tex0.h; ...
           prm.x0+prm.w,prm.y0+prm.h,0, g.nml, (prm.tx0+prm.w)/g.tex0.w,(prm.ty0+prm.h)/g.tex0.h];
    idx = [0,1,2; 3,2,1];
end

% =========================================================================
% hover hold mode selection buttons button
function [vtx,idx] = make_hoverholdsel(prm)
    disp('Computing hover hold selection buttons');
    global g;
    vtx = [prm.x0,      prm.y0,      0, g.nml, prm.tx0/g.tex0.w,        prm.ty0/g.tex0.h; ...
           prm.x0+prm.w,prm.y0,      0, g.nml, (prm.tx0+prm.w)/g.tex0.w,prm.ty0/g.tex0.h; ...
           prm.x0,      prm.y0+prm.h,0, g.nml, prm.tx0/g.tex0.w,        (prm.ty0+prm.h)/g.tex0.h; ...
           prm.x0+prm.w,prm.y0+prm.h,0, g.nml, (prm.tx0+prm.w)/g.tex0.w,(prm.ty0+prm.h)/g.tex0.h];
    idx = [0,1,2; 3,2,1];
end

% =========================================================================
% HUD mode buttons
function [vtx,idx] = make_hud_buttons(prm)
    disp('Computing HUD mode buttons for 2D panel');
    global g;
    y0 = prm.y0;
    vtx = [];
    idx = [];
    for i=1:3
        x0 = prm.x0(i);
        vtx = [vtx; ...
            x0,y0,0,             g.nml, prm.tx0/g.tex0.w, prm.ty0/g.tex0.h; ...
            x0+prm.w,y0,0,       g.nml, (prm.tx0+prm.w)/g.tex0.w, prm.ty0/g.tex0.h; ...
            x0,y0+prm.h,0,       g.nml, prm.tx0/g.tex0.w, (prm.ty0+prm.h)/g.tex0.h; ...
            x0+prm.w,y0+prm.h,0, g.nml, (prm.tx0+prm.w)/g.tex0.w, (prm.ty0+prm.h)/g.tex0.h];
        idx = [idx; ...
            [0,1,2; 3,2,1]+(i-1)*4];
    end
end

% =========================================================================
% Nav mode buttons
function [vtx,idx] = make_nav_buttons(prm)
    disp('Computing Nav mode buttons for 2D panel');
    global g;
    vtx = [];
    idx = [];
    for i=1:6
        x0 = prm.x0(i);
        y0 = prm.y0(i);
        tx0 = prm.tx0(i);
        ty0 = prm.ty0;
        vtx = [vtx; ...
            x0,y0,0,                 g.nml, tx0/g.tex0.w,ty0/g.tex0.h; ...
            x0+prm.tdx,y0,0,         g.nml, (tx0+prm.tdx)/g.tex0.w,ty0/g.tex0.h; ...
            x0,y0+prm.tdy,0,         g.nml, tx0/g.tex0.w,(ty0+prm.tdy)/g.tex0.h; ...
            x0+prm.tdx,y0+prm.tdy,0, g.nml, (tx0+prm.tdx)/g.tex0.w,(ty0+prm.tdy)/g.tex0.h];
        idx = [idx; ...
            [0,1,2; 3,2,1]+(i-1)*4];
    end
end

% =========================================================================
% Main engine throttles
function [vtx,idx] = make_main_throttles(prm)
    disp('Computing main engine throttles for 2D panel');
    global g;
    vtx = [];
    idx = [];
    for i=1:2
        x0 = prm.x0(i);
        vtx = [vtx; ...
            x0,prm.y0,0,             g.nml, prm.tx0/g.tex0.w,prm.ty0/g.tex0.h; ...
            x0+prm.w,prm.y0,0,       g.nml, (prm.tx0+prm.w)/g.tex0.w,prm.ty0/g.tex0.h; ...
            x0,prm.y0+prm.h,0,       g.nml, prm.tx0/g.tex0.w,(prm.ty0+prm.h)/g.tex0.h; ...
            x0+prm.w,prm.y0+prm.h,0, g.nml, (prm.tx0+prm.w)/g.tex0.w,(prm.ty0+prm.h)/g.tex0.h];
        idx = [idx; ...
            [0,1,2; 3,2,1]+(i-1)*4];
    end
end

% =========================================================================
% Hover engine throttles
function [vtx,idx] = make_hover_throttles(prm)
    disp('Computing hover engine throttles for 2D panel');
    global g;
    vtx = [prm.x0,prm.y0,0,               g.nml, prm.tx0/g.tex0.w,prm.ty0/g.tex0.h; ...
           prm.x0+prm.w,prm.y0,0,         g.nml, (prm.tx0+prm.tdx)/g.tex0.w,prm.ty0/g.tex0.h; ...
           prm.x0,prm.y0+prm.tdy,0,       g.nml, prm.tx0/g.tex0.w,(prm.ty0+prm.tdy)/g.tex0.h; ...
           prm.x0+prm.w,prm.y0+prm.tdy,0, g.nml, (prm.tx0+prm.tdx)/g.tex0.w,(prm.ty0+prm.tdy)/g.tex0.h];
    idx = [0,1,2; 3,2,1];
end

% =========================================================================
% Scram engine throttles
function [vtx,idx] = make_scram_throttles(prm)
    disp('Computing scram engine throttles for 2D panel');
    global g;
    vtx = [];
    idx = [];
    for i=1:2
        x0 = prm.x0(i);
        vtx = [vtx; ...
            x0,prm.y0,0,             g.nml, prm.tx0/g.tex0.w,prm.ty0/g.tex0.h; ...
            x0+prm.w,prm.y0,0,       g.nml, (prm.tx0+prm.w)/g.tex0.w,prm.ty0/g.tex0.h; ...
            x0,prm.y0+prm.h,0,       g.nml, prm.tx0/g.tex0.w,(prm.ty0+prm.h)/g.tex0.h; ...
            x0+prm.w,prm.y0+prm.h,0, g.nml, (prm.tx0+prm.w)/g.tex0.w,(prm.ty0+prm.h)/g.tex0.h];
        idx = [idx; ...
            [0,1,2; 3,2,1]+(i-1)*4];
    end
end

% =========================================================================
% Elevator trim indicator
function [vtx,idx] = make_etrim_indicator(prm)
    disp('Computing elevator trim indicator');
    global g;
    vtx = [prm.x0,prm.y0,0,             g.nml, prm.tx0/g.tex0.w,prm.ty0/g.tex0.h; ...
           prm.x0+prm.w,prm.y0,0,       g.nml, (prm.tx0+prm.w)/g.tex0.w,prm.ty0/g.tex0.h; ...
           prm.x0,prm.y0+prm.h,0,       g.nml, prm.tx0/g.tex0.w,(prm.ty0+prm.h)/g.tex0.h; ...
           prm.x0+prm.w,prm.y0+prm.h,0, g.nml, (prm.tx0+prm.w)/g.tex0.w,(prm.ty0+prm.h)/g.tex0.h];
    idx = [0,1,2; 1,3,2];
end

% =========================================================================
% Airbrake indicator
function [vtx,idx] = make_abrake_indicator(prm)
    disp('Computing airbrake indicator');
    global g;
    vtx = [prm.x0,prm.y0,0,             g.nml, prm.tx0/g.tex0.w,prm.ty0/g.tex0.h; ...
           prm.x0+prm.w,prm.y0,0,       g.nml, (prm.tx0+prm.w)/g.tex0.w,prm.ty0/g.tex0.h; ...
           prm.x0,prm.y0+prm.h,0,       g.nml, prm.tx0/g.tex0.w,(prm.ty0+prm.h)/g.tex0.h; ...
           prm.x0+prm.w,prm.y0+prm.h,0, g.nml, (prm.tx0+prm.w)/g.tex0.w,(prm.ty0+prm.h)/g.tex0.h];
    idx = [0,1,2; 1,3,2];
end

% =========================================================================
% Gear lever
function [vtx,idx] = make_gearlever(prm)
    disp('Computing gear lever');
    global g;
    vtx = [prm.x0,      prm.y0,      0, g.nml, prm.tx0/g.tex0.w,        (prm.ty0+prm.w)/g.tex0.h; ...
           prm.x0+prm.w,prm.y0,      0, g.nml, prm.tx0/g.tex0.w,        prm.ty0/g.tex0.h; ...
           prm.x0,      prm.y0+prm.h,0, g.nml, (prm.tx0+prm.h)/g.tex0.w,(prm.ty0+prm.w)/g.tex0.h; ...
           prm.x0+prm.w,prm.y0+prm.h,0, g.nml, (prm.tx0+prm.h)/g.tex0.w,prm.ty0/g.tex0.h];
    idx = [0,1,2; 3,2,1];
end

% =========================================================================
% Gear indicator
function [vtx,idx] = make_gearindicator(prm)
    disp('Computing gear indicator');
    global g;
    vtx = [prm.x0,   prm.y0,   0,  g.nml,  1018/g.tex0.w, (g.tex0.h-597)/g.tex0.h; ...
           prm.x0+10,prm.y0,   0,  g.nml,  1028/g.tex0.w, (g.tex0.h-597)/g.tex0.h; ...
           prm.x0,   prm.y0+10,0,  g.nml,  1018/g.tex0.w, (g.tex0.h-587)/g.tex0.h; ...
		   prm.x0+10,prm.y0+10,0,  g.nml,  1028/g.tex0.w, (g.tex0.h-587)/g.tex0.h; ...
		   prm.x0-8, prm.y0+16,0,  g.nml,  1018/g.tex0.w, (g.tex0.h-597)/g.tex0.h; ...
		   prm.x0+2, prm.y0+16,0,  g.nml,  1028/g.tex0.w, (g.tex0.h-597)/g.tex0.h; ...
		   prm.x0-8, prm.y0+26,0,  g.nml,  1018/g.tex0.w, (g.tex0.h-587)/g.tex0.h; ...
		   prm.x0+2, prm.y0+26,0,  g.nml,  1028/g.tex0.w, (g.tex0.h-587)/g.tex0.h; ...
		   prm.x0+8, prm.y0+16,0,  g.nml,  1018/g.tex0.w, (g.tex0.h-597)/g.tex0.h; ...
		   prm.x0+18,prm.y0+16,0,  g.nml,  1028/g.tex0.w, (g.tex0.h-597)/g.tex0.h; ...
		   prm.x0+8, prm.y0+26,0,  g.nml,  1018/g.tex0.w, (g.tex0.h-587)/g.tex0.h; ...
		   prm.x0+18,prm.y0+26,0,  g.nml,  1028/g.tex0.w, (g.tex0.h-587)/g.tex0.h];
    idx = [0,1,2; 3,2,1; ...
		   4,5,6; 7,6,5; ...
		   8,9,10; 11,10,9];
end

% =========================================================================
% Nose cone lever
function [vtx,idx] = make_noseconelever()
    disp('Computing nose cone lever');
    global g;
    vtx = [1220.5,358.5,0,  g.nml,  1047.5/g.tex0.w, (g.tex0.h-696.5)/g.tex0.h; ...
		   1260.5,358.5,0,  g.nml,  1087.5/g.tex0.w, (g.tex0.h-696.5)/g.tex0.h; ...
		   1220.5,377.5,0,  g.nml,  1047.5/g.tex0.w, (g.tex0.h-677.5)/g.tex0.h; ...
		   1260.5,377.5,0,  g.nml,  1087.5/g.tex0.w, (g.tex0.h-677.5)/g.tex0.h];
    idx = [0,1,2; 3,2,1];
end

% =========================================================================
% Nose cone indicator
function [vtx,idx] = make_noseconeindicator()
    disp('Computing nose cone indicator');
    global g;
    vtx = [1227,325,0,  g.nml,  1027/g.tex0.w, (g.tex0.h-611)/g.tex0.h; ... % top left
		   1239,325,0,  g.nml,  1039/g.tex0.w, (g.tex0.h-611)/g.tex0.h; ...
		   1227,337,0,  g.nml,  1027/g.tex0.w, (g.tex0.h-599)/g.tex0.h; ...
		   1253,325,0,  g.nml,  1027/g.tex0.w, (g.tex0.h-611)/g.tex0.h; ... % top right
		   1253,337,0,  g.nml,  1039/g.tex0.w, (g.tex0.h-611)/g.tex0.h; ...
		   1241,325,0,  g.nml,  1027/g.tex0.w, (g.tex0.h-599)/g.tex0.h; ...
		   1253,351,0,  g.nml,  1027/g.tex0.w, (g.tex0.h-611)/g.tex0.h; ... % bottom right
		   1241,351,0,  g.nml,  1039/g.tex0.w, (g.tex0.h-611)/g.tex0.h; ...
		   1253,339,0,  g.nml,  1027/g.tex0.w, (g.tex0.h-599)/g.tex0.h; ...
		   1227,351,0,  g.nml,  1027/g.tex0.w, (g.tex0.h-611)/g.tex0.h; ... % bottom left
		   1227,339,0,  g.nml,  1039/g.tex0.w, (g.tex0.h-611)/g.tex0.h; ...
		   1239,351,0,  g.nml,  1027/g.tex0.w, (g.tex0.h-599)/g.tex0.h];
    idx = [0,1,2; 3,4,5; 6,7,8; 9,10,11];
end

% =========================================================================
% Undock button
function [vtx,idx] = make_undockbutton(prm)
    disp('Computing undock button');
    global g;
    vtx = [prm.x0,      prm.y0,      0, g.nml, prm.tx0/g.tex0.w,        prm.ty0/g.tex0.h; ...
           prm.x0+prm.w,prm.y0,      0, g.nml, (prm.tx0+prm.w)/g.tex0.w,prm.ty0/g.tex0.h; ...
           prm.x0,      prm.y0+prm.h,0, g.nml, prm.tx0/g.tex0.w,        (prm.ty0+prm.h)/g.tex0.h; ...
           prm.x0+prm.w,prm.y0+prm.h,0, g.nml, (prm.tx0+prm.w)/g.tex0.w,(prm.ty0+prm.h)/g.tex0.h];
    idx = [0,1,2; 3,2,1];
end

% =========================================================================
% Wheel brake levers
function [vtx,idx] = make_wbrakelever(prm)
    disp('Computing wheel brake levers');
    global g;
    vtx = [prm.x0,      prm.y0,      0, g.nml, prm.tx0/g.tex0.w,        prm.ty0/g.tex0.h; ...
           prm.x0+prm.w,prm.y0,      0, g.nml, (prm.tx0+prm.w)/g.tex0.w,prm.ty0/g.tex0.h; ...
           prm.x0,      prm.y0+prm.h,0, g.nml, prm.tx0/g.tex0.w,        prm.ty0/g.tex0.h; ...
           prm.x0+prm.w,prm.y0+prm.h,0, g.nml, (prm.tx0+prm.w)/g.tex0.w,prm.ty0/g.tex0.h; ...
           prm.x1,      prm.y0,      0, g.nml, prm.tx1/g.tex0.w,        prm.ty0/g.tex0.h; ...
           prm.x1+prm.w,prm.y0,      0, g.nml, (prm.tx1+prm.w)/g.tex0.w,prm.ty0/g.tex0.h; ...
           prm.x1,      prm.y0+prm.h,0, g.nml, prm.tx1/g.tex0.w,        prm.ty0/g.tex0.h; ...
           prm.x1+prm.w,prm.y0+prm.h,0, g.nml, (prm.tx1+prm.w)/g.tex0.w,prm.ty0/g.tex0.h];
    idx = [0,1,2; 3,2,1; 4,5,6; 7,6,5];
end

% =========================================================================
% Master warning system button

function [vtx,idx] = make_mws(prm)
    global g;
    x0 = prm.x0;
    y0 = prm.y0;
    x1 = x0 + prm.w;
    y1 = y0 + prm.h;
    u0 = prm.tx0/g.tex0.w;
    u1 = (prm.tx0+prm.w)/g.tex0.w;
    v0 = prm.ty0/g.tex0.h;
    vtx = [x0,y0,0, g.nml, u0,v0; ...
           x1,y0,0, g.nml, u1,v0; ...
           x0,y1,0, g.nml, u0,v0; ...
           x1,y1,0, g.nml, u1,v0];
    idx = [0,1,2; 3,2,1];
end

% =========================================================================
% Angular rate displays
function [vtx,idx] = make_angveldisp(prm)
    global g;
    x0 = prm.x0;
    y0 = prm.y0;
    xcnt = prm.xcnt;
    ycnt = prm.ycnt;
    rad = prm.rad;
    eps = rad*2/50;
    u = prm.u;
    v = prm.v;
    
    % semi-transparent shrouds over the scales
    vtx = [];
    idx = [];
    for i=1:3
        vtx = [vtx; ...
            xcnt(1),    ycnt(i)-rad,    0, g.nml, u,v; ... % yaw scale overlay
            xcnt(1)-rad,ycnt(i)-rad,    0, g.nml, u,v; ...
            xcnt(1)-rad,ycnt(i)+rad+eps,0, g.nml, u,v; ...
            xcnt(1),    ycnt(i)-rad,    0, g.nml, u,v; ...
            xcnt(1)+rad,ycnt(i)-rad,    0, g.nml, u,v; ...
            xcnt(1)+rad,ycnt(i)+rad+eps,0, g.nml, u,v; ...
            xcnt(1),    ycnt(i),        0, g.nml, u,v; ...
            
            xcnt(2),    ycnt(i)-rad,    0, g.nml, u,v; ... % roll scale overlay
            xcnt(2)-rad,ycnt(i)-rad,    0, g.nml, u,v; ...
            xcnt(2)-rad,ycnt(i)+rad+eps,0, g.nml, u,v; ...
            xcnt(2),    ycnt(i)-rad,    0, g.nml, u,v; ...
            xcnt(2)+rad,ycnt(i)-rad,    0, g.nml, u,v; ...
            xcnt(2)+rad,ycnt(i)+rad+eps,0, g.nml, u,v; ...
            xcnt(2),    ycnt(i),        0, g.nml, u,v; ...
            
            xcnt(3)+rad,ycnt(i),        0, g.nml, u,v; ...
            xcnt(3)+rad,ycnt(i)-rad,    0, g.nml, u,v; ...
            xcnt(3)-rad-eps,ycnt(i)-rad,0, g.nml, u,v; ...
            xcnt(3)+rad,ycnt(i),        0, g.nml, u,v; ...
            xcnt(3)+rad,ycnt(i)+rad,    0, g.nml, u,v; ...
            xcnt(3)-rad-eps,ycnt(i)+rad,0, g.nml, u,v; ...
            xcnt(3),ycnt(i),            0, g.nml, u,v];
        
        idx = [idx; ...
            [6,1,0; 6,2,1; 6,3,4; 6,4,5; ...
            13,8,7; 13,9,8; 13,10,11; 13,11,12; ...
            20,15,14; 20,16,15; 20,17,18; 20,18,19] + (i-1)*21];
    end
    vtx(:,1) = vtx(:,1) + x0;
    vtx(:,2) = vtx(:,2) + y0;
end

% =========================================================================
% AAP
function [vtx,idx] = make_aap(prm)
    global g;
    x0 = prm.x0;
    y0 = prm.y0;
    vtx = [];
    idx = [];
    idx_bb = [0,1,2,3,2,1];
    % readouts
    for block=0:2
        ybofs = y0+block*46;
        for i=0:5
            for j=0:3
                vtx = [vtx; ...
                    x0+22+(i+mod(j,2))*7, ybofs+0+floor(j/2)*13,0, g.nml, 1222/g.tex0.w, (g.tex0.h-708+floor(j/2)*15)/g.tex0.h];
            end
        end
    end
    % activation buttons
    for block=0:2
        ybofs = y0+block*46;
        for j=0:3
            vtx = [vtx; ...
                x0+1+mod(j,2)*12, ybofs+1+floor(j/2)*12, 0, g.nml, (1034+mod(j,2)*12)/g.tex0.w, (g.tex0.h-683+floor(j/2)*12)/g.tex0.h];
        end
    end
    % scan switches
    for block=0:2
        ybofs = y0+block*46;
        for j=0:3
            vtx = [vtx; ...
                x0+22+mod(j,2)*42, ybofs+20+floor(j/2)*11, 0, g.nml, (1068-floor(j/2)*14)/g.tex0.w, (g.tex0.h-616+mod(j,2)*42)/g.tex0.h];
        end
    end
    for i=0:23
        for j=0:1
            idx = [idx; ...
                i*4 + idx_bb(j*3+1), i*4 + idx_bb(j*3+2), i*4 + idx_bb(j*3+3)];
        end
    end
end