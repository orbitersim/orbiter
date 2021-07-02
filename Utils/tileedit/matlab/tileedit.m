function varargout = tileedit(varargin)
% TILEEDIT MATLAB code for tileedit.fig
%      TILEEDIT, by itself, creates a new TILEEDIT or raises the existing
%      singleton*.
%
%      H = TILEEDIT returns the handle to a new TILEEDIT or the handle to
%      the existing singleton*.
%
%      TILEEDIT('CALLBACK',hObject,eventData,handles,...) calls the local
%      function named CALLBACK in TILEEDIT.M with the given input arguments.
%
%      TILEEDIT('Property','Value',...) creates a new TILEEDIT or raises the
%      existing singleton*.  Starting from the left, property value pairs are
%      applied to the GUI before tileedit_OpeningFcn gets called.  An
%      unrecognized property name or invalid value makes property application
%      stop.  All inputs are passed to tileedit_OpeningFcn via varargin.
%
%      *See GUI Options on GUIDE's Tools menu.  Choose "GUI allows only one
%      instance to run (singleton)".
%
% See also: GUIDE, GUIDATA, GUIHANDLES

% Edit the above text to modify the response to help tileedit

% Last Modified by GUIDE v2.5 13-Jan-2016 02:10:43

% Begin initialization code - DO NOT EDIT
gui_Singleton = 1;
gui_State = struct('gui_Name',       mfilename, ...
                   'gui_Singleton',  gui_Singleton, ...
                   'gui_OpeningFcn', @tileedit_OpeningFcn, ...
                   'gui_OutputFcn',  @tileedit_OutputFcn, ...
                   'gui_LayoutFcn',  [] , ...
                   'gui_Callback',   []);
if nargin && ischar(varargin{1})
    gui_State.gui_Callback = str2func(varargin{1});
end

if nargout
    [varargout{1:nargout}] = gui_mainfcn(gui_State, varargin{:});
else
    gui_mainfcn(gui_State, varargin{:});
end
% End initialization code - DO NOT EDIT

% --- Executes just before tileedit is made visible.
function tileedit_OpeningFcn(hObject, eventdata, handles, varargin)
% This function has no output args, see OutputFcn.
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
% varargin   command line arguments to tileedit (see VARARGIN)

disp('Loading tileedit ...');
disp('This may take a moment.');

% Choose default command line output for tileedit
handles.output = hObject;

% Update handles structure
guidata(hObject, handles);

axis(handles.axes1,'off');
axis(handles.axes2,'off');
axis(handles.axes3,'off');

setappdata(gcf,'handles',handles);
if exist('tileedit.mat','file')
    load tileedit.mat
    settilegui(handles,prm);
    set(handles.popupmenu1,'Value',prm.request(1));
    set(handles.popupmenu2,'Value',prm.request(2));
    set(handles.popupmenu3,'Value',prm.request(3));
    refresh_fromgui(handles);
end

global g_prm;
g_prm.btn_down = false;
% UIWAIT makes tileedit wait for user response (see UIRESUME)
% uiwait(handles.figure1);


% --- Outputs from this function are returned to the command line.
function varargout = tileedit_OutputFcn(hObject, eventdata, handles)
% varargout  cell array for returning output args (see VARARGOUT);
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Get default command line output from handles structure
varargout{1} = handles.output;


% --------------------------------------------------------------------
function FileMenu_Callback(hObject, eventdata, handles)
% hObject    handle to FileMenu (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)


% --------------------------------------------------------------------
function OpenMenuItem_Callback(hObject, eventdata, handles)
% hObject    handle to OpenMenuItem (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
prm = getappdata(handles.figure1,'prm');
if isempty(prm)
    prm.texpath = pwd;
end
dirname = uigetdir(prm.texpath);
if ~isequal(dirname, 0) && ~strcmp(dirname, prm.texpath)
    prm.texpath = dirname;
    prm.res = 1;
    prm.lockres = inf;
    prm.latidx = 0;
    prm.lngidx = 0;
    setappdata(handles.figure1,'prm',prm);
    settilegui(handles,prm);
    refresh_fromgui(handles);
end

% --------------------------------------------------------------------
function PrintMenuItem_Callback(hObject, eventdata, handles)
% hObject    handle to PrintMenuItem (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
printdlg(handles.figure1)

% --------------------------------------------------------------------
function CloseMenuItem_Callback(hObject, eventdata, handles)
% hObject    handle to CloseMenuItem (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
selection = questdlg(['Close ' get(handles.figure1,'Name') '?'],...
                     ['Close ' get(handles.figure1,'Name') '...'],...
                     'Yes','No','Yes');
if strcmp(selection,'No')
    return;
end

delete(handles.figure1)


% ========================================================================

% -----------------------------------------------------------------------
% Copy user GUI selections into prm structure
% -----------------------------------------------------------------------
function prm = tilefromgui(handles)
prm.texpath = get(handles.figure1,'Name');
prm.texpath = prm.texpath(19:end-1);
prm.res     = str2num(get(handles.edit2,'String'));
prm.latidx  = str2num(get(handles.edit3,'String'));
prm.lngidx  = str2num(get(handles.edit4,'String'));
[prm.latrange,prm.lngrange] = tilerange(prm);
if get(handles.checkbox1,'Value') == 0
    prm.lockres = inf;
else
    s = get(handles.checkbox1,'String');
    prm.lockres = str2double(s(10:end));
end
prm.request(1) = get(handles.popupmenu1,'Value');
prm.request(2) = get(handles.popupmenu2,'Value');
prm.request(3) = get(handles.popupmenu3,'Value');

function settilegui(handles,prm)
set(handles.figure1,'Name',['Orbiter tileedit [' prm.texpath ']']);
set(handles.edit2,'String',num2str(prm.res));
set(handles.edit3,'String',num2str(prm.latidx));
set(handles.edit4,'String',num2str(prm.lngidx));
if isinf(prm.lockres)
    set(handles.checkbox1,'Value',0);
    set(handles.checkbox1,'String','Lock');
else
    set(handles.checkbox1,'Value',1);
    set(handles.checkbox1,'String',['Locked at ' num2str(prm.lockres)]);
end

% -----------------------------------------------------------------------
% Compute latitude/longitude range of current tile
% -----------------------------------------------------------------------
function [latrange,lngrange] = tilerange(prm)
if prm.res < 4
    latrange(1) = -90;
    latrange(2) = 90;
    lngrange(1) = -180;
    lngrange(2) = 180;
else
    nlat = 2^(prm.res-4);
    nlng = nlat*2;
    d = 180/nlat;
    latrange(2) = 90 - prm.latidx*d;
    latrange(1) = latrange(2)-d;
    lngrange(1) = -180 + prm.lngidx*d;
    lngrange(2) = lngrange(1)+d;
end

% -----------------------------------------------------------------------
% Compute latitude/longitude from current mouse position
% -----------------------------------------------------------------------
function [lat,lng] = equ_pos(hAxes,x,y,handles)
prm = getappdata(handles.figure1,'prm');
data = getappdata(hAxes,'data');
imsz = size(data.im);
if bitand(imsz(1),1) % odd image size: pixels are vertex locations
    lng = prm.lngrange(1) + (prm.lngrange(2)-prm.lngrange(1))*(x-1)/(imsz(2)-1);
    lat = prm.latrange(2) - (prm.latrange(2)-prm.latrange(1))*(y-1)/(imsz(1)-1);
else % even image size: pixels are areas
    lng = prm.lngrange(1) + (prm.lngrange(2)-prm.lngrange(1))*(x-0.5)/imsz(2);
    lat = prm.latrange(2) - (prm.latrange(2)-prm.latrange(1))*(y-0.5)/imsz(1);
end

% -----------------------------------------------------------------------
% Compute mouse position from latitude/longitude
% -----------------------------------------------------------------------
function [x,y] = mouse_pos(hAxes,lat,lng,handles)
prm = getappdata(handles.figure1,'prm');
data = getappdata(hAxes,'data');
imsz = size(data.im);
if bitand(imsz(1),1) % odd image size: pixels are vertex locations
    x = (lng-prm.lngrange(1))*(imsz(2)-1)/(prm.lngrange(2)-prm.lngrange(1)) + 1;
    y = (prm.latrange(2)-lat)*(imsz(1)-1)/(prm.latrange(2)-prm.latrange(1)) + 1;
else
    x = (lng-prm.lngrange(1))*imsz(2)/(prm.lngrange(2)-prm.lngrange(1)) + 0.5;
    y = (prm.latrange(2)-lat)*imsz(1)/(prm.latrange(2)-prm.latrange(1)) + 0.5;
end

% =======================================================================
% DATA LOAD FUNCTIONS
% =======================================================================

% -----------------------------------------------------------------------
% Load the specified surface texture from dds file
% If texture file doesn't exist, load an ancestor file and display
% a sub-area
% -----------------------------------------------------------------------
function surf = loadsurf (prm)
fname = sprintf ('%s/Surf/%02d/%06d/%06d.dds', prm.texpath, prm.res, prm.latidx, prm.lngidx);
surf.prm = prm;
if prm.res <= prm.lockres
    surf.data = ddsread(fname);
else
    surf.data = [];
end
sz = 2^(min(prm.res+6,9));
surf.prm.lat_subrange = [0 sz];
surf.prm.lng_subrange = [0 sz];
if isempty(surf.data) % load subarea of lower-resolution tile
    surf = loaddds_subset(surf,'Surf');
end
surf.data = surf.data(:,:,1:3); % cut out alpha channel
surf.im = surf.data(surf.prm.lat_subrange(1)+1:surf.prm.lat_subrange(2), ...
                    surf.prm.lng_subrange(1)+1:surf.prm.lng_subrange(2),:);


% -----------------------------------------------------------------------
% Load the specified night-ligt/water-mask texture from dds file
% If texture file doesn't exist, load an ancestor file and display
% a sub-area
% -----------------------------------------------------------------------
function [mask,light] = loadmask (prm)
fname = sprintf ('%s/Mask/%02d/%06d/%06d.dds', prm.texpath, prm.res, prm.latidx, prm.lngidx);
mask.prm = prm;
if prm.res <= prm.lockres
    mask.data = ddsread(fname);
else
    mask.data = [];
end
sz = 2^(min(prm.res+6,9));
mask.prm.lat_subrange = [0 sz];
mask.prm.lng_subrange = [0 sz];
if isempty(mask.data) % load subarea of lower-resolution tile
    mask = loaddds_subset(mask,'Mask');
end
if ~isempty(mask.data)
    mask.im = mask.data(mask.prm.lat_subrange(1)+1:mask.prm.lat_subrange(2), ...
                        mask.prm.lng_subrange(1)+1:mask.prm.lng_subrange(2),:);
    light = mask;
    mask.im = mask.im(:,:,4);
    light.im = light.im(:,:,1:3);
else
    light = [];
    mask.im = [];
    light.im = [];
end


% -----------------------------------------------------------------------
% Recursive ancestor texture loader
% -----------------------------------------------------------------------
function tex = loaddds_subset(tex,which)
if tex.prm.res > 1
    tex.prm.lat_subrange = tex.prm.lat_subrange/2;
    if bitand(tex.prm.latidx,1)
        tex.prm.lat_subrange = tex.prm.lat_subrange+256;
    end
    tex.prm.lng_subrange = tex.prm.lng_subrange/2;
    if bitand(tex.prm.lngidx,1)
        tex.prm.lng_subrange = tex.prm.lng_subrange+256;
    end
    tex.prm.res = tex.prm.res-1;
    tex.prm.latidx = floor(tex.prm.latidx/2);
    tex.prm.lngidx = floor(tex.prm.lngidx/2);
    fname = sprintf ('%s/%s/%02d/%06d/%06d.dds', ...
        tex.prm.texpath, which, tex.prm.res, tex.prm.latidx, tex.prm.lngidx);
    if tex.prm.res <= tex.prm.lockres
        tex.data = ddsread(fname);
    else
        tex.data = [];
    end
    if isempty(tex.data)
        tex = loaddds_subset(tex,which);
    end
end


% -----------------------------------------------------------------------
% Load the specified elevation data from elv file
% If file doesn't exist, load an ancestor file and display
% a sub-area
% -----------------------------------------------------------------------
function elev = loadelev (prm)
fname = sprintf ('%s/Elev/%02d/%06d/%06d.elv', prm.texpath, prm.res, prm.latidx, prm.lngidx);
elev.prm.texpath = prm.texpath;
elev.prm.res = prm.res;
elev.prm.latidx = prm.latidx;
elev.prm.lngidx = prm.lngidx;
elev.prm.lat_subrange = [0 256];
elev.prm.lng_subrange = [0 256];
elev.data = double(elvread(fname));
if isempty(elev.data) % load subarea of lower-resolution tile
    elev = loadelev_subset(elev);
end

% Try to load the corresponding elevation mod function
elev.data0 = elev.data;
if isempty(elev.data)
    elev.im = [];
else
    fname = sprintf ('%s/Elev_mod/%02d/%06d/%06d.elv', ...
        elev.prm.texpath, elev.prm.res, elev.prm.latidx, elev.prm.lngidx);
    elev.data = double(elvmodread(fname,elev.data0));

    elev.im = elev.data(elev.prm.lat_subrange(1)+2:elev.prm.lat_subrange(2)+2, ...
                    elev.prm.lng_subrange(1)+2:elev.prm.lng_subrange(2)+2);
end
elev.modified = false;
elev.bndmod = zeros(3,3);


% -----------------------------------------------------------------------
% Recursive ancestor elevation loader
% -----------------------------------------------------------------------
function elev = loadelev_subset(elev)
if elev.prm.res > 1
    elev.prm.lat_subrange = elev.prm.lat_subrange/2;
    if bitand(elev.prm.latidx,1)
        elev.prm.lat_subrange = elev.prm.lat_subrange+128;
    end
    elev.prm.lng_subrange = elev.prm.lng_subrange/2;
    if bitand(elev.prm.lngidx,1)
        elev.prm.lng_subrange = elev.prm.lng_subrange+128;
    end
    elev.prm.res = elev.prm.res-1;
    elev.prm.latidx = floor(elev.prm.latidx/2);
    elev.prm.lngidx = floor(elev.prm.lngidx/2);
    fname = sprintf ('%s/Elev/%02d/%06d/%06d.elv', ...
        elev.prm.texpath, elev.prm.res, elev.prm.latidx, elev.prm.lngidx);
    elev.data = double(elvread(fname));
    if isempty(elev.data)
        elev = loadelev_subset(elev);
    end
end


% -----------------------------------------------------------------------
% Recursively save all elevation modifications
% -----------------------------------------------------------------------
function saveelev_all (elev)
if elev.modified
    saveelev_lvl (elev);
    saveelev_all (elevmod_downsample(elev));
end


% -----------------------------------------------------------------------
% Write elevation-mod data and any neighbours with overlapping mods
% -----------------------------------------------------------------------
function saveelev_lvl (elev)
saveelev(elev);
for y=1:3
    for x=1:3
        if elev.bndmod(y,x)
            saveneighbourelev(elev,x-2,y-2);
        end
    end
end


% -----------------------------------------------------------------------
% Write elevation-mod data to file
% -----------------------------------------------------------------------
function saveelev(elev)
dname = sprintf ('%s/Elev_mod/%02d/%06d', ...
    elev.prm.texpath, elev.prm.res, elev.prm.latidx);
fname = sprintf ('%06d.elv', elev.prm.lngidx);
elvmodwrite(dname,fname,elev.data,elev.data0);
% sync lower resolution tiles
%res = elev.prm.res;
%latidx = elev.prm.latidx;
%lngidx = elev.prm.lngidx;
%while elev_downsample(elev.prm.texpath,res,latidx,lngidx)
%    res = res-1;
%    latidx = floor(latidx/2);
%    lngidx = floor(lngidx/2);
%end    


% -----------------------------------------------------------------------
% Synchronise neighbour tile overlaps with tile changes
% -----------------------------------------------------------------------
function saveneighbourelev(elev,dlng,dlat)
nb_prm = elev.prm;
nb_prm.latidx = nb_prm.latidx + dlat;
nb_prm.lngidx = nb_prm.lngidx + dlng;
nlat = 2^(nb_prm.res-4);
nlng = nlat*2;
if nb_prm.latidx < 0 || nb_prm.latidx >= nlat
    return;
end
if nb_prm.lngidx < 0
    nb_prm.lngidx = nlng-1;
elseif nb_prm.lngidx >= nlng
    nb_prm.lngidx = 0;
end
nb_elev = loadelev(nb_prm);
switch dlng
    case -1
        switch dlat
            case -1
                nb_elev.data(end-2:end,end-2:end) = elev.data(1:3,1:3);
            case 0
                nb_elev.data(:,end-2:end) = elev.data(:,1:3);
            case 1
                nb_elev.data(1:3,end-2:end) = elev.data(end-2:end,1:3);
        end
    case 1
        switch dlat
            case -1
                nb_elev.data(end-2:end,1:3) = elev.data(1:3,end-2:end);
            case 0
                nb_elev.data(:,1:3) = elev.data(:,end-2:end);
            case 1
                nb_elev.data(1:3,1:3) = elev.data(end-2:end,end-2:end);
        end
    case 0
        switch dlat
            case -1
                nb_elev.data(end-2:end,:) = elev.data(1:3,:);
            case 0
                error('should not happen');
            case 1
                nb_elev.data(1:3,:) = elev.data(end-2:end,:);
        end
end
saveelev(nb_elev);


% -----------------------------------------------------------------------
% Map elevation mods down to the next lower level
% -----------------------------------------------------------------------
function pelev = elevmod_downsample(elev)
texpath = elev.prm.texpath;
lvl = elev.prm.res;
plvl = lvl-1;
pilat = floor(elev.prm.latidx/2);
pilng = floor(elev.prm.lngidx/2);
fname = sprintf('%s/Elev/%02d/%06d/%06d.elv', texpath, plvl, pilat, pilng);
pdata = elvread(fname);
pdata0 = pdata;
fname = sprintf('%s/Elev_mod/%02d/%06d/%06d.elv', texpath, plvl, pilat, pilng);
pdata = elvmodread(fname,pdata0);

ilat0 = pilat*2-1;
ilat1 = ilat0+3;
ilng0 = pilng*2-1;
ilng1 = ilng0+3;
data = NaN(1027,1027);
nlng = 2^(elev.prm.res-3);
nlat = nlng/2;
for i=ilat0:ilat1
    for j=ilng0:ilng1
        jofs = (j-ilng0)*256;
        iofs = (i-ilat0)*256;
        if j < 0, jj = nlng-1; elseif j==nlng, jj = 0; else jj=j; end
        if i < 0, ii = 0; elseif i==nlat, ii = nlat-1; else ii=i; end % not really
        fname = sprintf('%s/Elev/%02d/%06d/%06d.elv', texpath, lvl, ii, jj);
        sdata = elvread(fname);
        if ~isempty(sdata)
            fname = sprintf('%s/Elev_mod/%02d/%06d/%06d.elv', texpath, lvl, ii, jj);
            sdata = elvmodread(fname,sdata);
            data(iofs+1:iofs+259,jofs+1:jofs+259) = sdata;
        end
    end
end

mdata = zeros(size(pdata));
%stencil = [0.25,0.5,0.25; 0.5,1,0.5; 0.25,0.5,0.25]./4;
stencil = [0,0,0; 0,1,0; 0,0,0];
for i=1:259
    iofs = i*2+254;
    for j=1:259
        jofs = j*2+254;
        block = data(iofs-1:iofs+1,jofs-1:jofs+1);
        mask = ~isnan(block);
        idx = find(mask);
        s = sum(block(idx).*stencil(idx));
        w = sum(stencil(idx));
        mdata(i,j) = s + (1-w)*pdata(i,j);
    end
end

pelev.prm.texpath = texpath;
pelev.prm.res = plvl;
pelev.prm.latidx = pilat;
pelev.prm.lngidx = pilng;
pelev.data0 = pdata0;
pelev.data = round(mdata);

[y,x] = find(pelev.data-pdata);
pelev.modified = ~isempty(x);
pelev.bndmod = zeros(3,3);
if pelev.modified
    if min(x) <= 3
        pelev.bndmod(2,1) = true;
        if min(y) <= 3
            pelev.bndmod(1,1) = true;
        end
        if max(y) >= size(pdata,1)-2
            pelev.bndmod(3,1) = true;
        end
    end
    if max(x) >= size(pdata,2)-2
        pelev.bndmod(2,3) = true;
        if min(y) <= 3
            pelev.bndmod(1,3) = true;
        end
        if max(y) >= size(pdata,1)-2
            pelev.bndmod(3,3) = true;
        end
    end
    if min(y) <= 3
        pelev.bndmod(1,2) = true;
    end
    if max(y) >= size(pdata,1)-2
        pelev.bndmod(3,2) = true;
    end
end


% =======================================================================
% DATA DISPLAY FUNCTIONS
% =======================================================================

% -----------------------------------------------------------------------
% Refresh the display of one of the axes with the supplied data
% -----------------------------------------------------------------------
function refresh_axes(hAxes,data,handles)
if ~isempty(data.im)
    h = imagesc(data.im,'Parent',hAxes);
    set(h,'ButtonDownFcn',@ButtonDownFcn);
    axis(hAxes, 'off');
    setappdata(h,'handles',handles);
else
    cla(hAxes);
end
setappdata(hAxes,'data',data);
setappdata(hAxes,'marker',0);


% -----------------------------------------------------------------------
% Refresh the label below one of the axes
% -----------------------------------------------------------------------
function refresh_label(hStatic,data,handles)
if isempty(data.im)
    set(hStatic,'String','');
else
    set(hStatic,'String',sprintf('%02d / %06d / %06d', ...
        data.prm.res, data.prm.latidx, data.prm.lngidx));
end

% -----------------------------------------------------------------------
% Refresh the longitude/latitude range display
% -----------------------------------------------------------------------
function refresh_rangedisp(prm,handles)
set(handles.text12,'String', sprintf('%+0.16g\n%+0.16g', prm.latrange(1), prm.latrange(2)));
set(handles.text13,'String', sprintf('%+0.16g\n%+0.16g', prm.lngrange(1), prm.lngrange(2)));

% -----------------------------------------------------------------------
% Refresh all axes from user GUI selection
% -----------------------------------------------------------------------
function res = refresh_fromgui(handles)

prm = tilefromgui(handles);
setappdata(handles.figure1,'prm',prm);

if ~exist(prm.texpath,'dir')
    set(gcf,'WindowButtonMotionFcn',[]);
    for i=1:3
        cla(ax(i));
        set(label(i),'String','');
    end
    res = 0;
    return;
end    

set (handles.figure1,'pointer','watch');
%drawnow
ax = [handles.axes1, handles.axes2, handles.axes3];
label = [handles.text7, handles.text8, handles.text9];
refresh_rangedisp(prm,handles);

for i=1:3
    data = getappdata(ax(i),'data');
    if isfield(data,'modified') && data.modified == true
        saveelev_all(data);
        % for now, only elevation data get modified!
        data.modified = false;
        data.bndmod = zeros(3,3);
        setappdata(ax(i),'data',data);
    end
end

req = sort(unique(prm.request));
if ismember(1,req)
    surf = loadsurf(prm);
    for i=1:3
        if prm.request(i) == 1
            refresh_axes(ax(i),surf,handles);
            refresh_label(label(i),surf,handles);
        end
    end
end
if ismember(2,req) || ismember(3,req)
    [mask,light] = loadmask(prm);
    for i=1:3
        if prm.request(i) == 2
            refresh_axes(ax(i),mask,handles);
            refresh_label(label(i),mask,handles);
            set(ax(i),'CLim',[0 330]);
        end
        if prm.request(i) == 3
            refresh_axes(ax(i),light,handles);
            refresh_label(label(i),light,handles);
        end
    end
end
if ismember(4,req)
    elev = loadelev(prm);
    for i=1:3
        if prm.request(i) == 4
            refresh_axes(ax(i),elev,handles);
            refresh_label(label(i),elev,handles);
        end
    end
end
set(gcf,'WindowButtonMotionFcn',@MouseMoveFcn);
set(gcf,'WindowButtonUpFcn',@MouseBtnUpFcn);
MouseMoveFcn(gcf,[]);
set (handles.figure1,'pointer','arrow');
%drawnow
res = 1;


% =======================================================================
% DATA MANIPULATION FUNCTIONS
% =======================================================================

%------------------------------------------------------------------------
% set value of given pixel
%------------------------------------------------------------------------
function setval(hAxes,handles,x,y,val)
d = getappdata(hAxes,'data');
data = d.data;
x = x + d.prm.lng_subrange(1)+1;
y = y + d.prm.lat_subrange(1)+1;
data(y,x) = val;
d.data = data;
d.im = d.data(d.prm.lat_subrange(1)+2:d.prm.lat_subrange(2)+2, ...
              d.prm.lng_subrange(1)+2:d.prm.lng_subrange(2)+2);
d.modified = true;
if x <= 3
    d.bndmod(2,1) = true;
    if y <= 3
        d.bndmod(1,1) = true;
    elseif y >= size(data,1)-2
        d.bndmod(3,1) = true;
    end
elseif x >= size(data,2)-2
    d.bndmod(2,3) = true;
    if y <= 3
        d.bndmod(1,3) = true;
    elseif y >= size(data,1)-2
        d.bndmod(3,3) = true;
    end
end
if y <= 3
    d.bndmod(1,2) = true;
elseif y >= size(data,1)-2
    d.bndmod(3,2) = true;
end
refresh_axes(hAxes,d,handles);


%------------------------------------------------------------------------
% erase modification of given pixel
%------------------------------------------------------------------------
function erase(hAxes,handles,x,y)
d = getappdata(hAxes,'data');
if isfield(d,'data0')
    data0 = d.data0;
    data = d.data;
    x = x + d.prm.lng_subrange(1)+1;
    y = y + d.prm.lat_subrange(1)+1;
    data(y,x) = data0(y,x);
    d.data = data;
    d.im = d.data(d.prm.lat_subrange(1)+2:d.prm.lat_subrange(2)+2, ...
                  d.prm.lng_subrange(1)+2:d.prm.lng_subrange(2)+2);
    d.modified = true;
    if x <= 3
        d.bndmod(2,1) = true;
        if y <= 3
            d.bndmod(1,1) = true;
        elseif y >= size(data,1)-2
            d.bndmod(3,1) = true;
        end
    elseif x >= size(data,2)-2
        d.bndmod(2,3) = true;
        if y <= 3
            d.bndmod(1,3) = true;
        elseif y >= size(data,1)-2
            d.bndmod(3,3) = true;
        end
    end
    if y <= 3
        d.bndmod(1,2) = true;
    elseif y >= size(data,1)-2
        d.bndmod(3,2) = true;
    end
    refresh_axes(hAxes,d,handles);
end


%------------------------------------------------------------------------
% smooth image around a given pixel
%------------------------------------------------------------------------
function smooth(hAxes,handles,x,y)
d = getappdata(hAxes,'data');
data = d.data;
x = x + d.prm.lng_subrange(1)+1;
y = y + d.prm.lat_subrange(1)+1;
wght = [0.5,0.125,0.125,0.125,0.125];
v = data(y,x);
stencilx = x;
stencily = y;
if x > 1
    v = [v, data(y,x-1)];
    stencilx = [stencilx, x-1];
    stencily = [stencily, y];
end
if x < size(data,2)
    v = [v, data(y,x+1)];
    stencilx = [stencilx, x+1];
    stencily = [stencily, y];
end
if y > 1
    v = [v, data(y-1,x)];
    stencilx = [stencilx, x];
    stencily = [stencily, y-1];
end
if y < size(data,1)
    v = [v, data(y+1,x)];
    stencilx = [stencilx, x];
    stencily = [stencily, y+1];
end
n = length(v);
wght = wght(1:n);
mn = mean(v);
v2 = v+(mn-v).*wght;
v2 = v2 + (mn-mean(v2));
for i=1:n
    y = stencily(i);
    x = stencilx(i);
    data(y,x) = v2(i);
    if x <= 3
        d.bndmod(2,1) = true;
        if y <= 3
            d.bndmod(1,1) = true;
        elseif y >= size(data,1)-2
            d.bndmod(3,1) = true;
        end
    elseif x >= size(data,2)-2
        d.bndmod(2,3) = true;
        if y <= 3
            d.bndmod(1,3) = true;
        elseif y >= size(data,1)-2
            d.bndmod(3,3) = true;
        end
    end
    if y <= 3
        d.bndmod(1,2) = true;
    elseif y >= size(data,1)-2
        d.bndmod(3,2) = true;
    end
end
d.data = data;
d.im = d.data(d.prm.lat_subrange(1)+2:d.prm.lat_subrange(2)+2, ...
              d.prm.lng_subrange(1)+2:d.prm.lng_subrange(2)+2);
d.modified = true;
refresh_axes(hAxes,d,handles);


% =======================================================================
% MOUSE EVENT FUNCTIONS
% =======================================================================

% -----------------------------------------------------------------------
% Mouse button over image
% -----------------------------------------------------------------------
function ButtonDownFcn(hObject, eventdata)
% hObject    handle to axes1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
global g_prm;
g_prm.btn_down = true;
handles = getappdata(hObject,'handles');
ax = get(hObject,'Parent');
i = find([handles.axes1, handles.axes2, handles.axes3] == ax);
sel = [handles.popupmenu1, handles.popupmenu2, handles.popupmenu3];
datatype = get(sel(i),'Value');
action = get(handles.popupmenu4,'Value');
if action ~= 1 && datatype ~= 4
    return; % editing actions only allowed for elevation maps
end
mouse = get(ax,'currentpoint');
x = round(mouse(1,1));
y = round(mouse(1,2));
xlim = get(ax,'XLim');
ylim = get(ax,'YLim');
g_prm.pos = [x y];
if x < xlim(1) || x > xlim(2) || y < ylim(1) || y > ylim(2)
    return; % sanity check
end

prm = tilefromgui(handles);
update = false;
switch action
    case 1
        dx = xlim(2)-xlim(1);
        dy = ylim(2)-ylim(1);
        xcnt = (xlim(1)+xlim(2))/2;
        ycnt = (ylim(1)+ylim(2))/2;
        if abs(x-xcnt) < dx/10 && y < dy/10 && prm.latidx > 0 % pan up
            if prm.latidx > 0
                prm.latidx = prm.latidx-1;
                update = true;
            end
        elseif abs(x-xcnt) < dx/10 && y > 9*dy/10 && prm.latidx < 2^(prm.res-4)-1 % pan down
            if prm.latidx < 2^(prm.res-4)-1
                prm.latidx = prm.latidx+1;
                update = true;
            end
        elseif abs(y-ycnt) < dy/10 && x < dx/10 && prm.lngidx > 0 % pan left
            if prm.lngidx > 0
                prm.lngidx = prm.lngidx-1;
            else
                prm.lngidx = 2^(prm.res-3)-1;
            end
            update = true;
        elseif abs(y-ycnt) < dy/10 && x > dx*9/10 && prm.lngidx < 2^(prm.res-3)-1 % pan right
            if prm.lngidx < 2^(prm.res-3)-1
                prm.lngidx = prm.lngidx+1;
            else
                prm.lngidx = 0;
            end
            update = true;
        elseif abs(x-xcnt) < dx/10 && abs(y-ycnt) < dy/10 && prm.res > 1 % zoom out
            if prm.res > 1
                prm.res = prm.res-1;
                prm.latidx = floor(prm.latidx/2);
                prm.lngidx = floor(prm.lngidx/2);
                update = true;
            end
        else
            if prm.res < 3 % zoom in
                prm.res = prm.res+1;
                update = true;
            elseif prm.res == 3
                if x < xcnt % zoom w
                    prm.lngidx = 0;
                    prm.res = prm.res+1;
                    update = true;
                else % zoom e
                    prm.lngidx = 1;
                    prm.res = prm.res+1;
                    update = true;
                end
            else
                if x < xcnt
                    if y < ycnt % zoom nw
                        prm.lngidx = prm.lngidx*2;
                        prm.latidx = prm.latidx*2;
                        prm.res = prm.res+1;
                        update = true;
                    else % zoom sw
                        prm.lngidx = prm.lngidx*2;
                        prm.latidx = prm.latidx*2+1;
                        prm.res = prm.res+1;
                        update = true;
                    end
                else
                    if y < ycnt % zoom ne
                        prm.lngidx = prm.lngidx*2+1;
                        prm.latidx = prm.latidx*2;
                        prm.res = prm.res+1;
                        update = true;
                    else % zoom se
                        prm.lngidx = prm.lngidx*2+1;
                        prm.latidx = prm.latidx*2+1;
                        prm.res = prm.res+1;
                        update = true;
                    end
                end
            end
        end
  
    case 2 % 'setval'
        s = get(handles.edit5,'String');
        v = str2double(s);
        setval(ax,handles,x,y,v);

    case 3 % 'setrandom'
        s = get(handles.edit5,'String');
        v = str2double(s);
        s = get(handles.edit6,'String');
        std = str2double(s);
        v = round(normrnd(v,std));
        setval(ax,handles,x,y,v);

    case 4 % 'erase'
        erase(ax,handles,x,y);

    case 5 % 'smooth'
        smooth(ax,handles,x,y);
end
    
if update
    settilegui(handles,prm);
    refresh_fromgui(handles);
end


% -----------------------------------------------------------------------
% Mouse moved over image
% -----------------------------------------------------------------------
function MouseMoveFcn(hObject, eventdata)
global g_prm;
handles = getappdata(hObject,'handles');
prm = getappdata(handles.figure1,'prm');
ax = [handles.axes1, handles.axes2, handles.axes3];
sel = [handles.popupmenu1, handles.popupmenu2, handles.popupmenu3];
action = get(handles.popupmenu4,'Value');
mouseax = 0;
for i=1:3
    data = getappdata(ax(i),'data');
    datatype = get(sel(i),'Value');
    marker(i) = getappdata(ax(i),'marker');
    if marker(i)
        delete(marker(i));
        marker(i) = 0;
    end
    if isempty(data.im)
        continue;
    end
    xlim = get(ax(i),'XLim');
    ylim = get(ax(i),'YLim');
    mouse = get(ax(i),'currentpoint');
    x = round(mouse(1,1));
    y = round(mouse(1,2));
    if x >= xlim(1) && x <= xlim(2) && y >= ylim(1) && y <= ylim(2)
        set(handles.text5,'String',sprintf('Mouse: x=%0.0f, y=%0.0f', x, y));
        mouseax = i;
        mousex = x;
        mousey = y;
        dx = xlim(2)-xlim(1);
        dy = ylim(2)-ylim(1);
        xcnt = mean(xlim);
        ycnt = mean(ylim);
        [lat,lng] = equ_pos(ax(i),x,y,handles);
        set(handles.text15,'String',sprintf('Pos: lng=%+0.16g, lat=%+0.16g', lng, lat));
        switch action
            case 1 % Zoom and pan                                                                                                       
                if abs(x-xcnt) < dx/10 && y < dy/10 && prm.latidx > 0 % pan up
                    marker(i) = line([xcnt-dx/20,xcnt,xcnt+dx/20],[dy/20+0.5,0.5,dy/20+0.5],'Parent',ax(i),'LineWidth',3,'Color','red','HitTest','off','PickableParts','none');
                elseif abs(x-xcnt) < dx/10 && y > 9*dy/10 && prm.latidx < 2^(prm.res-4)-1 % pan down
                    marker(i) = line([xcnt-dx/20,xcnt,xcnt+dx/20],[dy*19/20+0.5,dy+0.5,dy*19/20+0.5],'Parent',ax(i),'LineWidth',3,'Color','red','HitTest','off','PickableParts','none');
                elseif abs(y-ycnt) < dy/10 && x < dx/10 && prm.lngidx > 0 % pan left
                    marker(i) = line([dx/20+0.5,0.5,dx/20+0.5],[ycnt-dy/20,ycnt,ycnt+dy/20],'Parent',ax(i),'LineWidth',3,'Color','red','HitTest','off','PickableParts','none');
                elseif abs(y-ycnt) < dy/10 && x > dx*9/10 && prm.lngidx < 2^(prm.res-3)-1 % pan right
                    marker(i) = line([dx*19/20+0.5,dx+0.5,dx*19/20+0.5],[ycnt-dy/20,ycnt,ycnt+dy/20],'Parent',ax(i),'LineWidth',3,'Color','red','HitTest','off','PickableParts','none');
                elseif abs(x-xcnt) < dx/10 && abs(y-ycnt) < dy/10 && prm.res > 1 % zoom out
                    marker(i) = line([xcnt-dx/20,xcnt+dx/20,nan,xcnt-dx/20,xcnt+dx/20],[ycnt-dy/20,ycnt+dy/20,nan,ycnt+dy/20,ycnt-dy/20],'Parent',ax(i),'LineWidth',3,'Color','red','HitTest','off','PickableParts','none');
                else
                    if prm.res < 3 % zoom in
                        marker(i) = rectangle('Position',[xlim(1),ylim(1),dx,dy],'Parent',ax(i),'LineWidth',3,'EdgeColor','red','HitTest','off','PickableParts','none');
                    elseif prm.res == 3
                        if x < xcnt % zoom w
                            marker(i) = rectangle('Position',[xlim(1),ylim(1),dx/2,dy],'Parent',ax(i),'LineWidth',3,'EdgeColor','red','HitTest','off','PickableParts','none');
                        else % zoom e
                            marker(i) = rectangle('Position',[xcnt,ylim(1),dx/2,dy],'Parent',ax(i),'LineWidth',3,'EdgeColor','red','HitTest','off','PickableParts','none');
                        end
                    else
                        if x < xcnt
                            if y < ycnt % zoom nw
                                marker(i) = rectangle('Position',[xlim(1),ylim(1),dx/2,dy/2],'Parent',ax(i),'LineWidth',3,'EdgeColor','red','HitTest','off','PickableParts','none');
                            else % zoom sw
                                marker(i) = rectangle('Position',[xlim(1),ycnt,dx/2,dy/2],'Parent',ax(i),'LineWidth',3,'EdgeColor','red','HitTest','off','PickableParts','none');
                            end
                        else
                            if y < ycnt % zoom ne
                                marker(i) = rectangle('Position',[xcnt,ylim(1),dx/2,dy/2],'Parent',ax(i),'LineWidth',3,'EdgeColor','red','HitTest','off','PickableParts','none');
                            else % zoom se
                                marker(i) = rectangle('Position',[xcnt,ycnt,dx/2,dy/2],'Parent',ax(i),'LineWidth',3,'EdgeColor','red','HitTest','off','PickableParts','none');
                            end
                        end
                    end
                end
                    
            case 2 % set value
                if datatype == 4 % edit only for elevation data
                    if g_prm.btn_down
                        if x ~= g_prm.pos(1) || y ~= g_prm.pos(2)
                            s = get(handles.edit5,'String');
                            v = str2double(s);
                            setval(ax(i),handles,x,y,v);
                            g_prm.pos = [x y];
                        end
                    end
                    marker(i) = line([x-5,x-1,nan,x+1,x+5,nan,x,x,nan,x,x],[y,y,nan,y,y,nan,y-5,y-1,nan,y+1,y+5],'Parent',ax(i),'LineWidth',3,'Color','green','HitTest','off','PickableParts','none');
                end
            case 3 % set randomized value
                if datatype == 4 % edit only for elevation data
                    if g_prm.btn_down
                        if x ~= g_prm.pos(1) || y ~= g_prm.pos(2)
                            s = get(handles.edit5,'String');
                            v = str2double(s);
                            s = get(handles.edit6,'String');
                            std = str2double(s);
                            v = round(normrnd(v,std));
                            setval(ax(i),handles,x,y,v);
                            g_prm.pos = [x y];
                        end
                    end
                    marker(i) = line([x-5,x-1,nan,x+1,x+5,nan,x,x,nan,x,x],[y,y,nan,y,y,nan,y-5,y-1,nan,y+1,y+5],'Parent',ax(i),'LineWidth',3,'Color','green','HitTest','off','PickableParts','none');
                end
            case 4 % erase
                if datatype == 4 % edit only for elevation data
                    marker(i) = line([x-5,x-1,nan,x+1,x+5,nan,x,x,nan,x,x],[y,y,nan,y,y,nan,y-5,y-1,nan,y+1,y+5],'Parent',ax(i),'LineWidth',3,'Color','green','HitTest','off','PickableParts','none');
                end
            case 5 % smoothing
                if datatype == 4 % edit only for elevation data
                    marker(i) = line([x-10,x-2,nan,x+2,x+10,nan,x,x,nan,x,x],[y,y,nan,y,y,nan,y-10,y-2,nan,y+2,y+10],'Parent',ax(i),'LineWidth',3,'Color','green','HitTest','off','PickableParts','none');
                end
        end
    end
end
if mouseax==0
    set(handles.text5,'String','');
    set(handles.text6,'String','');
    set(handles.text15,'String','');
else
    v = get(sel(mouseax),'Value');
    s = get(sel(mouseax),'String');
    set(handles.uipanel2,'Title',s{v});
    data = getappdata(ax(mouseax),'data');
    mousev = data.im(mousey,mousex,:);
    switch v
        case {1,3} % surface
            set(handles.text6,'String',sprintf('Colour: R%02d / G%02d / B%02d', mousev(1), mousev(2), mousev(3)));
        case 2 % watermask
            if mousev > 128
                set(handles.text6,'String','Land');
            else
                set(handles.text6,'String','Water');
            end
        case 4
            set(handles.text6,'String',sprintf('Height: %0.2f',mousev));
        otherwise
            set(handles.text6,'String',sprintf('Value: %f',mousev));
    end
    if action >= 2
        for i=1:3
            if i ~= mouseax
                [x,y] = mouse_pos(ax(i),lat,lng,handles);
                data = getappdata(ax(i),'data');
                scale = 0.1*size(data.im);
                marker(i) = line([x-scale(2),x+scale(2),nan,x,x], [y,y,nan,y-scale(1),y+scale(1)],'Parent',ax(i),'LineWidth',1,'Color','green','HitTest','off','PickableParts','none');
                setappdata(ax(i),'marker',marker);
            end
        end
    end
end
for i=1:3
    setappdata(ax(i),'marker',marker(i));
end

function MouseBtnUpFcn(hObject, eventdata)
global g_prm;
g_prm.btn_down = false;

% =======================================================================
% GUI CALLBACK FUNCTIONS
% =======================================================================

% -----------------------------------------------------------------------
% 'Update' button
% -----------------------------------------------------------------------
function pushbutton1_Callback(hObject, eventdata, handles)
% --- Executes on button press in pushbutton1.
% hObject    handle to pushbutton1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
refresh_fromgui(handles);


% -----------------------------------------------------------------------
% Left image selector list
% -----------------------------------------------------------------------
function popupmenu1_Callback(hObject, eventdata, handles)
% --- Executes on selection change in popupmenu1.
% hObject    handle to popupmenu1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
refresh_fromgui(handles);


function popupmenu1_CreateFcn(hObject, eventdata, handles)
% --- Executes during object creation, after setting all properties.
% hObject    handle to popupmenu1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
     set(hObject,'BackgroundColor','white');
end


% -----------------------------------------------------------------------
% Middle image selector list
% -----------------------------------------------------------------------
function popupmenu2_Callback(hObject, eventdata, handles)
% --- Executes on selection change in popupmenu2.
% hObject    handle to popupmenu2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
refresh_fromgui(handles);


function popupmenu2_CreateFcn(hObject, eventdata, handles)
% --- Executes during object creation, after setting all properties.
% hObject    handle to popupmenu2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% -----------------------------------------------------------------------
% Right image selector list
% -----------------------------------------------------------------------
function popupmenu3_Callback(hObject, eventdata, handles)
% --- Executes on selection change in popupmenu3.
% hObject    handle to popupmenu3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
refresh_fromgui(handles);


function popupmenu3_CreateFcn(hObject, eventdata, handles)
% --- Executes during object creation, after setting all properties.
% hObject    handle to popupmenu3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% -----------------------------------------------------------------------
% Mouse action selector list
% -----------------------------------------------------------------------
function popupmenu4_Callback(hObject, eventdata, handles)
% --- Executes on selection change in popupmenu4.
% hObject    handle to popupmenu4 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
widget_handle = [handles.edit5, handles.text14, handles.edit6, handles.text16];
vis2_handle = [handles.edit5, handles.text14];
vis3_handle = [handles.edit5, handles.text14, handles.edit6, handles.text16];

for i=1:length(widget_handle)
    set(widget_handle(i),'Visible','off');
end
switch get(hObject,'Value')
    case 2
        w = vis2_handle;
    case 3
        w = vis3_handle;
    otherwise
        w = [];
end
for i=1:length(w)
    set(w(i),'Visible','on');
end


function popupmenu4_CreateFcn(hObject, eventdata, handles)
% --- Executes during object creation, after setting all properties.
% hObject    handle to popupmenu4 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


function edit2_Callback(hObject, eventdata, handles)
% hObject    handle to edit2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of edit2 as text
%        str2double(get(hObject,'String')) returns contents of edit2 as a double


% --- Executes during object creation, after setting all properties.
function edit2_CreateFcn(hObject, eventdata, handles)
% hObject    handle to edit2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function edit3_Callback(hObject, eventdata, handles)
% hObject    handle to edit3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of edit3 as text
%        str2double(get(hObject,'String')) returns contents of edit3 as a double


% --- Executes during object creation, after setting all properties.
function edit3_CreateFcn(hObject, eventdata, handles)
% hObject    handle to edit3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function edit4_Callback(hObject, eventdata, handles)
% hObject    handle to edit4 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of edit4 as text
%        str2double(get(hObject,'String')) returns contents of edit4 as a double


% --- Executes during object creation, after setting all properties.
function edit4_CreateFcn(hObject, eventdata, handles)
% hObject    handle to edit4 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% -----------------------------------------------------------------------
% Value for edit function
% -----------------------------------------------------------------------
function edit5_Callback(hObject, eventdata, handles)
% hObject    handle to edit5 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

function edit5_CreateFcn(hObject, eventdata, handles)
% --- Executes during object creation, after setting all properties.
% hObject    handle to edit5 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes when user attempts to close figure1.
function figure1_CloseRequestFcn(hObject, eventdata, handles)
% hObject    handle to figure1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% write pending modifications
ax = [handles.axes1,handles.axes2,handles.axes3];
for i=1:3
    data = getappdata(ax(i),'data');
    if isfield(data,'modified') && data.modified == true
        saveelev_all(data);
        data.modified = false;
        data.bndmod = zeros(3,3);
        setappdata(ax(i),'data',data);
    end
end

% save current user selections
prm = getappdata(hObject,'prm');
save('tileedit.mat','prm');

% close main window
delete(hObject);


% --- Executes on button press in checkbox1.
function checkbox1_Callback(hObject, eventdata, handles)
% hObject    handle to checkbox1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
if get(hObject,'Value') == 1
    res = get(handles.edit2,'String');
    set(hObject,'String',['Locked at ' res]);
else
    set(hObject,'String','Lock');
    prm = getappdata(handles.figure1,'prm');
    if prm.res > prm.lockres
        %settilegui(handles,prm);
        refresh_fromgui(handles);
    end
end
    
% Hint: get(hObject,'Value') returns toggle state of checkbox1



function edit6_Callback(hObject, eventdata, handles)
% hObject    handle to edit6 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of edit6 as text
%        str2double(get(hObject,'String')) returns contents of edit6 as a double


% --- Executes during object creation, after setting all properties.
function edit6_CreateFcn(hObject, eventdata, handles)
% hObject    handle to edit6 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end
