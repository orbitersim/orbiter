function elvmodwrite(dname,fname,im,im0)

elvh.grd = size(im);
elvh.size = 100;

im = reshape(rot90(int32(im),3),[],1);
im0 = reshape(rot90(int32(im0),3),[],1);

i = find(im-im0);
if isempty(i)
    % no modifications - delete the mod file if it exists
    if exist([dname '/' fname],'file')
        delete([dname '/' fname]);
        % attempt to delete the directory as well (if empty)
        status = rmdir(dname);
    end
    return; % done
end

[res,msg,id] = mkdir(dname);
if res == 0
    return; % cannot create directory
end

fid = fopen([dname '/' fname], 'wb');
if fid < 0
    return; % cannot create file
end

v = im(i);
v0 = min(v);
v1 = max(v);
vrange = v1-v0;

if vrange < 255
    elvh.dtype = 8;
    elvh.offset = v0;
    im = im-v0;
    modim = ones(size(im),'uint8')*intmax('uint8');
    modim(i) = im(i);
else
    elvh.dtype = -16;
    elvh.offset = 0;
    modim = ones(size(im),'int16')*intmax('int16');
    modim(i) = im(i);
end

elvh.pad = [1 1];
elvh.scale = 1;
elvh.latrange = [0 0]; % TODO
elvh.lngrange = [0 0]; % TODO
elvh.erange = [v0 v1, mean(v)];

write_header(fid, elvh);

if elvh.dtype == 8
    fwrite(fid,modim,'uint8');
else
    fwrite(fid,modim,'int16');
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