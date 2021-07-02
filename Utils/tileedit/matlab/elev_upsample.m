function elev_upsample (texpath, lvl, ilat, ilng)
plvl = lvl-1;
pilat = floor(ilat/2);
pilng = floor(ilng/2);
fname = sprintf ('%s/Elev/%02d/%06d/%06d.elv', texpath, plvl, pilat, pilng);
pelev = double(elvread(fname));
if isempty(pelev)
    error('Parent elevation tile not found.');
end
fname = sprintf ('%s/Elev_mod/%02d/%06d/%06d.elv', texpath, plvl, pilat, pilng);
pelev = double(elvmodread(fname,pelev));
ix = [1.5:0.5:130.5];
iy = [1.5:0.5:130.5];
if bitand(ilng,1)
    ix = ix+128;
end
if bitand(ilat,1)
    iy = iy+128;
end
elev = interp2(pelev,ix,iy');
dname = sprintf ('%s/Elev/%02d/%06d', texpath, lvl, ilat);
fname = sprintf ('%06d.elv', ilng);
elvwrite (dname, fname, elev);

end