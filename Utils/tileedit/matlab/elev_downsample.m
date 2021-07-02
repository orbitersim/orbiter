function b = elev_downsample(texpath,lvl,ilat,ilng)
plvl = lvl-1;
pilat = floor(ilat/2);
pilng = floor(ilng/2);
fname = sprintf('%s/Elev/%02d/%06d/%06d.elv', texpath, plvl, pilat, pilng);
pdata = elvread(fname);
pdata0 = pdata;
fname = sprintf('%s/Elev_mod/%02d/%06d/%06d.elv', texpath, plvl, pilat, pilng);
pdata = elvmodread(fname,pdata0);

ilat0 = pilat*2-1;
ilat1 = ilat0+3;
ilng0 = pilng*2-1;
ilng1 = ilng0+3;
data = zeros(1027,1027);
data0 = zeros(1027,1027);
nlng = 2^(lvl-3);
nlat = nlng/2;
for i=ilat0:ilat1
    for j=ilng0:ilng1
        jofs = (j-ilng0)*256;
        iofs = (i-ilat0)*256;
        if j < 0, jj = nlng-1; elseif j==nlng, jj = 0; else jj=j; end
        if i < 0, ii = 0; elseif i==nlat, ii = nlat-1; else ii=i; end % not really
        fname = sprintf('%s/Elev/%02d/%06d/%06d.elv', texpath, lvl, ii, jj);
        sdata = elvread(fname);
        if isempty(sdata)
            sdata = loadelev_subset(texpath,lvl,ii,jj,[0,256],[0,256]);
            if isempty(sdata)
                b = false;
                return;
            end
        end
        data0(iofs+1:iofs+259,jofs+1:jofs+259) = sdata;
        fname = sprintf('%s/Elev_mod/%02d/%06d/%06d.elv', texpath, lvl, ii, jj);
        sdata = elvmodread(fname,sdata);
        data(iofs+1:iofs+259,jofs+1:jofs+259) = sdata;
    end
end
diffdata = zeros(1027,1027);
diffdata(find(data-data0))=1;

mdata = zeros(size(pdata));
ddata = zeros(size(pdata));
for i=1:259
    iofs = i*2+254;
    for j=1:259
        jofs = j*2+254;
        sum = data(iofs,jofs);
        sum = sum + 0.5*(data(iofs-1,jofs) + data(iofs+1,jofs) + data(iofs,jofs-1) + data(iofs,jofs+1));
        sum = sum + 0.25*(data(iofs-1,jofs-1) + data(iofs+1,jofs-1) + data(iofs-1,jofs+1) + data(iofs+1,jofs+1));
        mdata(i,j) = sum/4;
        sum = diffdata(iofs,jofs);
        sum = sum + 0.5*(diffdata(iofs-1,jofs) + diffdata(iofs+1,jofs) + diffdata(iofs,jofs-1) + diffdata(iofs,jofs+1));
        sum = sum + 0.25*(diffdata(iofs-1,jofs-1) + diffdata(iofs+1,jofs-1) + diffdata(iofs-1,jofs+1) + diffdata(iofs+1,jofs+1));
        ddata(i,j) = sum/4;
    end
end

moddata = pdata;
i = find(ddata);
moddata(i) = round(mdata(i));

%rng = [min(mdata(:)),max(mdata(:))];
%subplot(1,3,1); imagesc(pdata,rng); axis equal tight; colormap('jet'); colorbar
%subplot(1,3,2); imagesc(mdata,rng); axis equal tight; colormap('jet'); colorbar
%subplot(1,3,3); imagesc(moddata,rng); axis equal tight; colormap('jet'); colorbar

i = find(moddata-pdata,1);
b = ~isempty(i);
if b
    dname = sprintf('%s/Elev_mod/%02d/%06d', texpath, plvl, pilat);
    fname = sprintf('%06d.elv', pilng);
    elvmodwrite(dname,fname,moddata,pdata0);
end
end

function data = loadelev_subset(texpath,lvl,ilat,ilng,lat_subrange,lng_subrange)
if lvl > 1
    lat_subrange = lat_subrange/2;
    if bitand(ilat,1)
        lat_subrange = lat_subrange+128;
    end
    lng_subrange = lng_subrange/2;
    if bitand(ilng,1)
        lng_subrange = lng_subrange+128;
    end
    lvl = lvl-1;
    ilat = floor(ilat/2);
    ilng = floor(ilng/2);
    fname = sprintf ('%s/Elev/%02d/%06d/%06d.elv', ...
        texpath, lvl, ilat, ilng);
    data = double(elvread(fname));
    if isempty(data)
        data = loadelev_subset(texpath,lvl,ilat,ilng,lat_subrange,lng_subrange);
    else
        data = data(lat_subrange(1)+1:lat_subrange(2)+3,lng_subrange(1)+1:lng_subrange(2)+3);
        subsz = size(data,1)-3;
        dx = subsz/256;
        ix = [2-dx:dx:subsz+2+dx];
        data = interp2(data,ix,ix');
    end
else
    data = [];
end
end