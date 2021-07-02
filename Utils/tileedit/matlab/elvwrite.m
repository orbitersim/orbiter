function elvwrite(dname,fname,im)

elvh.grd = size(im);
elvh.size = 100;
im = reshape(rot90(int32(im),3),[],1);

[res,msg,id] = mkdir(dname);
if res == 0
    return; % cannot create directory
end

fid = fopen([dname '/' fname], 'wb');
if fid < 0
    return; % cannot create file
end

v = im;
v0 = min(v);
v1 = max(v);
vrange = v1-v0;

if vrange < 255
    elvh.dtype = 8;
    elvh.offset = v0;
    im = uint8(im-v0);
else
    elvh.dtype = -16;
    elvh.offset = 0;
    im = int16(im);
end

elvh.pad = [1 1];
elvh.scale = 1;
elvh.latrange = [0 0]; % TODO
elvh.lngrange = [0 0]; % TODO
elvh.erange = [v0 v1, mean(v)];

write_header(fid, elvh);

if elvh.dtype == 8
    fwrite(fid,im,'uint8');
else
    fwrite(fid,im,'int16');
end
fclose(fid);

end


function write_header(fid,elvh)
    fwrite(fid,[69,76,69,1],'uint8');
    fwrite(fid,elvh.size,'int32');
    fwrite(fid,elvh.dtype,'int32');
    fwrite(fid,elvh.grd,'int32');
    fwrite(fid,elvh.pad,'int32');
    fwrite(fid,elvh.scale,'double');
    fwrite(fid,elvh.offset','double');
    fwrite(fid,elvh.latrange,'double');
    fwrite(fid,elvh.lngrange,'double');
    fwrite(fid,elvh.erange,'double');
end