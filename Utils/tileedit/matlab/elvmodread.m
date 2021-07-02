function im=elvmodread(fname,im0)

im = im0;

fid = fopen(fname,'rb');

if fid < 0
    return;
end

magic = char(fread(fid,4,'char')');
if ~strcmp(magic(1:3),'ELE') || magic(4) ~= 1
    error(['Invalid file format: ' fname]);
end

elvh = read_header(fid);

switch elvh.dtype
    case 0
        im = ones(elvh.grd,'int32')*elvh.offset;
    case 8
        mask = intmax('uint8');
        tmp = rot90(double(fread(fid,elvh.grd,'uint8')));
        i = find(tmp ~= mask);
        im(i) = tmp(i)+elvh.offset;
    case -16
        mask = intmax('int16');
        tmp = rot90(double(fread(fid,elvh.grd,'int16')));
        i = find(tmp ~= mask);
        im(i) = tmp(i)+elvh.offset;
end
fclose(fid);
end

function elvh = read_header(fid)
    elvh.size = fread(fid,1,'int32');
    elvh.dtype = fread(fid,1,'int32');
    elvh.grd = fread(fid,[1,2],'int32');
    elvh.pad = fread(fid,[1,2],'int32');
    elvh.scale = fread(fid,1,'double');
    elvh.offset = fread(fid,1,'double');
    elvh.latrange = fread(fid,[1,2],'double');
    elvh.lngrange = fread(fid,[1,2],'double');
    elvh.erange = fread(fid,[1,3],'double');
end