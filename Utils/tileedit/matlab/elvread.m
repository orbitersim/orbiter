function im=elvread(fname)

fid = fopen(fname,'rb');

if fid < 0
    im = [];
    return;
end

magic = char(fread(fid,4,'char')');
if ~strcmp(magic(1:3),'ELE') || magic(4) ~= 1
    error(['Invalid file format: ' fname]);
end

elvh = read_header(fid);

switch elvh.dtype
    case 0
        im = ones(elvh.grd)*elvh.offset;
    case 8
        im = double(fread(fid,elvh.grd,'uint8'))+elvh.offset;
    case -8
        im = double(fread(fid,elvh.grd,'int8'))+elvh.offset;
    case 16
        im = double(fread(fid,elvh.grd,'uint16'))+elvh.offset;
    case -16
        im = double(fread(fid,elvh.grd,'int16'))+elvh.offset;
end
im = rot90(im);
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