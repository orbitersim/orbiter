clear all
close all

texpath = 'G:\Orbiter_copies\Orbiter_textures\Earth';
lvl = 15;
ilat = 698;
ilng = 1130;
while elev_downsample(texpath,lvl,ilat,ilng)
    lvl = lvl-1;
    ilat = floor(ilat/2);
    ilng = floor(ilng/2);
end
